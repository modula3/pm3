(*  SGML to xx                                                             *)
(*  Copyright (C) 1997 Michel Dagenais                                     *)
(*                                                                         *)
(*  This program is free software; you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation; either version 2 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program; if not, write to the Free Software            *)
(*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.              *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

MODULE Main;

IMPORT SGML, SGMLPrint, Stdio, Params, Text, Wr, Rd, Thread, TextIntTbl, 
    TextWr, TextRd, Fmt, Lex, OSError, FileRd, FileWr, TextSeq, IntSeq, 
    Pathname, Process, FS, File, RegularFile;

TYPE
  Translate = SGML.Application OBJECT
      wr, oldwr: Wr.T;
      tags: TextIntTbl.Default;
      tagStack: IntSeq.T;
      depth: INTEGER := 0;
      skip: BOOLEAN := FALSE;
      skipDepth := 0;
      fontSize: CARDINAL;
      preformatted: BOOLEAN;
      table: Table;
      currentRow, currentColumn: CARDINAL;
      firstKey, firstSee, inHead, firstAuthor, closeAuthor: BOOLEAN := FALSE;
      firstAppendix: BOOLEAN := TRUE;
      preface: BOOLEAN := FALSE;
      bibType: Special;
      math: BOOLEAN := FALSE;
    METHODS
      init(): Translate := Init;
      encodeData(t: TEXT): TEXT;
    OVERRIDES
      appInfo := AppInfo;
      startDtd := StartDtd;
      endDtd := EndDtd;
      endProlog := EndProlog;
      data := Data;
      sdata := SData;
      pi := Pi;
      externalDataEntityRef := ExternalDataEntityRef;
      subdocEntityRef := SubdocEntityRef;
      nonSgmlChar := NonSgmlChar;
      commentDecl := CommentDecl;
      markedSectionStart := MarkedSectionStart;
      markedSectionEnd := MarkedSectionEnd;
      ignoredChars := IgnoredChars;
      generalEntity := GeneralEntity;
      error := ErrorProc;
      openEntityChange:= OpenEntityChange;
    END;

  HTMLtoTeX = Translate OBJECT METHODS OVERRIDES
      startElement := StartElement;
      endElement := EndElement;
      encodeData := EncodeForTeX;
    END;

  Filter = SGMLPrint.T OBJECT
      tags: TextIntTbl.Default;
      tagStack: IntSeq.T;
      depth: INTEGER := 0;
      skip: BOOLEAN := FALSE;
      skipDepth := 0;
      inHead := FALSE;
    METHODS
      init(): Filter := InitF;
    OVERRIDES
      data := DataF;
      sdata := SDataF;
      pi := PiF;
      externalDataEntityRef := ExternalDataEntityRefF;
      subdocEntityRef := SubdocEntityRefF;
      nonSgmlChar := NonSgmlCharF;
      commentDecl := CommentDeclF;
      markedSectionStart := MarkedSectionStartF;
      markedSectionEnd := MarkedSectionEndF;
      ignoredChars := IgnoredCharsF;
      generalEntity := GeneralEntityF;
      error := ErrorProc;
      openEntityChange:= OpenEntityChangeF;
    END;

  HTMLtoHTML = Filter OBJECT METHODS OVERRIDES
      startElement := StartElementF;
      endElement := EndElementF;
    END;

  HTMLCheck = Filter OBJECT METHODS OVERRIDES
      init := InitCheck;
      startElement := StartElementCheck;
      endElement := EndElementCheck;
    END;

  Table = REF RECORD
      nbRow, nbColumn: CARDINAL;
      rows: REF ARRAY OF Row;
      caption: TEXT;
      border: CARDINAL;
    END;

  Row = REF ARRAY OF Cell;

  Cell = RECORD
      content: TEXT := NIL;
      rowSpan, columnSpan: CARDINAL := 1;
      hAlign: HAlign := HAlign.Middle;
      vAlign: VAlign := VAlign.Middle;
    END;

  HAlign = {Left, Middle, Right};

  VAlign = {Top, Middle, Bottom};

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

PROCEDURE AppInfo(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.AppinfoEvent) =
  VAR
  BEGIN
  END AppInfo;

PROCEDURE StartDtd(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.StartDtdEvent) =
  VAR
  BEGIN
  END StartDtd;

PROCEDURE EndDtd(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.EndDtdEvent) =
  VAR
  BEGIN
  END EndDtd;

PROCEDURE EndProlog(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.EndPrologEvent) =
  VAR
  BEGIN
  END EndProlog;

(* Encode the data for the target output format. *)

PROCEDURE Data(self: Translate; READONLY e: SGML.DataEvent) =
  BEGIN
    IF NOT self.skip THEN
      IF HTMLtoTeXtbl[self.tagStack.gethi()].pcdata THEN
        Wr.PutText(self.wr,self.encodeData(e.data)); 
      ELSE
        IgnoreWhite(self,e.data);
      END;
    END;
  END Data;

PROCEDURE SData(self: Translate; READONLY e: SGML.SdataEvent) =
  BEGIN
    IF NOT self.skip THEN Wr.PutText(self.wr,self.encodeData(e.text)); END;
  END SData;

PROCEDURE Pi(<*UNUSED*>self: Translate; <*UNUSED*>READONLY e: SGML.PiEvent) =
  BEGIN
  END Pi;

PROCEDURE ExternalDataEntityRef(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.ExternalDataEntityRefEvent) =
  BEGIN
  END ExternalDataEntityRef;

PROCEDURE SubdocEntityRef(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.SubdocEntityRefEvent) =
  BEGIN
  END SubdocEntityRef;

PROCEDURE NonSgmlChar(self: Translate; 
    READONLY e: SGML.NonSgmlCharEvent) =
  BEGIN
    IF NOT self.skip THEN
      Wr.PutText(self.wr,self.encodeData(Text.FromChar(e.c)));
    END;
  END NonSgmlChar;

PROCEDURE CommentDecl(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.CommentDeclEvent) =
  BEGIN
  END CommentDecl;

PROCEDURE MarkedSectionStart(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.MarkedSectionStartEvent) =
  BEGIN
  END MarkedSectionStart;

PROCEDURE MarkedSectionEnd(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.MarkedSectionEndEvent) =
  BEGIN
  END MarkedSectionEnd;

PROCEDURE IgnoredChars(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.IgnoredCharsEvent) =
  BEGIN
  END IgnoredChars;

PROCEDURE GeneralEntity(<*UNUSED*>self: Translate; 
    <*UNUSED*>READONLY e: SGML.GeneralEntityEvent) =
  BEGIN
  END GeneralEntity;

PROCEDURE ErrorProc(self: SGML.Application; READONLY e: SGML.ErrorEvent) =
  VAR
    position := self.getDetailedLocation(e.pos);
    msg: TEXT;
  BEGIN
    IF e.type <= SGML.ErrorType.Warning THEN
      msg := "WARNING";
    ELSE
      msg := "ERROR";
      INC(nbErrors);
    END;

    Wr.PutText(Stdio.stderr,msg & ": " & e.message & " in file " &
        position.filename & " line " & Fmt.Int(position.lineNumber) &
        " column " & Fmt.Int(position.columnNumber));
    IF position.entityName # NIL THEN
      Wr.PutText(Stdio.stderr," in entity " & position.entityName & 
          " offset " & Fmt.Int(position.entityOffset));
    END;
    Wr.PutText(Stdio.stderr,"\n");
  END ErrorProc;

PROCEDURE OpenEntityChange(<*UNUSED*>self: Translate) =
  BEGIN
  END OpenEntityChange;

(* Index in a table all the conversion entries in the HTML to TeX table. *)

PROCEDURE Init(self: Translate): Translate =
  BEGIN
    EVAL SGML.Application.init(self);
    self.tags := NEW(TextIntTbl.Default).init();
    self.tagStack := NEW(IntSeq.T).init();
    self.tagStack.addhi(0); (* In case we receive some data outside any tags *)
    self.fontSize := 3;
    self.preformatted := FALSE;
    FOR i := 0 TO LAST(HTMLtoTeXtbl) DO
      IF self.tags.put(HTMLtoTeXtbl[i].tag,i) THEN
        Wr.PutText(Stdio.stderr,"Duplicate tag " & HTMLtoTeXtbl[i].tag & 
            " in translation table\n");
      END;
    END;
    RETURN self;
  END Init;

(* For each start tag, find the associated text string to output,
   and the required special action, if any. *)

PROCEDURE StartElement(self: Translate; READONLY e: SGML.StartElementEvent) =
  VAR
    n: INTEGER;
    size, pos: INTEGER;
    t, rel, name: TEXT;
    valid, plus, found: BOOLEAN;
    action: Special;
  BEGIN
    IF verbose >= 2 THEN
      WITH position = self.getDetailedLocation(e.pos) DO
        Wr.PutText(Stdio.stdout,"In file " & position.filename & " line " & 
            Fmt.Int(position.lineNumber) & " column " & 
            Fmt.Int(position.columnNumber) & "\n");
      END;
    END;

    IF self.math AND (NOT self.skip) AND
        HTMLtoTeXtbl[self.tagStack.gethi()].action = Special.MathArgs THEN
      Wr.PutText(self.wr,"{");
    END;

    INC(self.depth);

    (* Unknown tag *)

    name := e.gi;
    found := self.tags.get(name,n);
    IF found THEN
      CASE HTMLtoTeXtbl[n].action OF
      | Special.Span, Special.Div =>
          IF GetAttributeValue(e.attributes,"CLASS",t) THEN
            found := FALSE;
            IF self.inHead THEN
              name := e.gi & ".HEAD." & t;
              found := self.tags.get(name,n);
            END;
            IF NOT found THEN
              name := e.gi & "." & t;
              found := self.tags.get(name,n);
            END;
          END;
      ELSE
      END;
    END;

    IF NOT found THEN n := 0; END;
    self.tagStack.addhi(n);

    IF NOT found THEN    
      Wr.PutText(Stdio.stderr,"Unexpected tag " & name & "\n");

    (* Output the associated text and execute the special action. *)
    ELSIF NOT self.skip THEN
      Wr.PutText(self.wr,HTMLtoTeXtbl[n].start);
      action := HTMLtoTeXtbl[n].action;

      IF action IN OkForLabel THEN PutLabel(e,self); END;

      CASE action OF
      | Special.Skip =>
          self.skip := TRUE;
          self.skipDepth := self.depth;

      (* Compute the desired font size. Font and BaseFont are treated alike.
         The font size may be absolute, or relative if preceeded by a sign. *)

      | Special.Font, Special.BaseFont =>
          IF GetAttributeIntValue(e.attributes,"SIZE",plus,valid,size) THEN
            IF valid THEN
              IF plus OR size < 0 THEN size := self.fontSize + size; END;
              SetFontSize(self,size);
            ELSE
              Wr.PutText(Stdio.stderr,"Improper font size\n");
            END;
          END;

      (* A bigger/smaller font is requested. *)

      | Special.Big => SetFontSize(self,self.fontSize + 1);
      | Special.Small => SetFontSize(self,self.fontSize - 1);

      (* Preformatted mode *)

      | Special.Preformatted =>
          IF self.preformatted THEN Wr.PutText(Stdio.stderr,"Nested <PRE>\n");
          END;
          self.preformatted := TRUE;

      (* A elements *) (* \cite, add label for ID or NAME... *)

      | Special.Link =>
          self.skip := TRUE;
          self.skipDepth := self.depth;
          PutLabel(e,self);
          IF GetAttributeValue(e.attributes,"HREF",t) THEN
            IF Text.Equal("#",Text.Sub(t,0,1)) THEN
              IF NOT GetAttributeValue(e.attributes,"REL",rel) THEN 
                rel := "";
              END;
              IF Text.Equal(rel,"BIB.ENTRY") THEN
                pos := Text.FindCharR(t,'#');
                Wr.PutText(self.wr,"\\cite{" & EncodeLabel(Text.Sub(t,pos + 1))
                & "}");
              ELSIF Text.Equal(rel,"BIB.NOREF") THEN
              ELSIF Text.Equal(rel,"REF.PAGE") THEN
                Wr.PutText(self.wr,"\\pageref{" & EncodeLabel(Text.Sub(t,1)) & 
                    "}");
              ELSE
                Wr.PutText(self.wr,"\\ref{" & EncodeLabel(Text.Sub(t,1)) & 
                    "}");
              END;
            ELSE
              Wr.PutText(self.wr," (" & EncodeForTeX(self,t) & ") ");
            END;
          END;

      (* Tables are more involved. *)

      | Special.Table =>
          (* Build a table structure to intercept everything *)
          PutLabel(e,self);
          BeginTable(self);
          IF GetAttributeIntValue(e.attributes,"BORDER",plus,valid,size) THEN
            IF valid THEN self.table.border := size;
            ELSE self.table.border := 1;
            END;
          END;

      (* Add a row in the table *)

      | Special.Row =>
          CheckTableSize(self.table, self.currentRow, self.currentColumn);

      (* Add a column header/data element in the table. Check for alignment,
         colSpan and rowSpan attributes. Store this in the corresponding
         table cell. Divert the output to a text writer which will
         accumulate the cell content. *)

      | Special.ColumnHeader, Special.ColumnData =>
          CheckTableSize(self.table, self.currentRow, self.currentColumn);
          WITH cell = self.table.rows[self.currentRow][self.currentColumn] DO
            IF GetAttributeValue(e.attributes,"ALIGN",t) THEN
              IF Text.Equal(t,"left") THEN cell.hAlign := HAlign.Left;
              ELSIF Text.Equal(t,"middle") THEN cell.hAlign := HAlign.Middle;
              ELSIF Text.Equal(t,"right") THEN cell.hAlign := HAlign.Right;
              END;
            END;
            IF GetAttributeValue(e.attributes,"VALIGN",t) THEN
              IF Text.Equal(t,"top") THEN cell.vAlign := VAlign.Top;
              ELSIF Text.Equal(t,"middle") THEN cell.vAlign := VAlign.Middle;
              ELSIF Text.Equal(t,"bottom") THEN cell.vAlign := VAlign.Bottom;
              END;
            END;
            IF GetAttributeIntValue(e.attributes,"ROWSPAN",plus,valid,
                size) THEN
              IF valid AND size > 0 THEN cell.rowSpan := size; END;
            END;
            IF GetAttributeIntValue(e.attributes,"COLSPAN",plus,valid,
                size) THEN
              IF valid AND size > 0 THEN cell.columnSpan := size; END;
            END;
            CheckTableSize(self.table,self.currentRow + cell.rowSpan - 1,
                self.currentColumn + cell.columnSpan - 1);
          END;

          IF self.oldwr # NIL THEN
            Wr.PutText(Stdio.stderr,"Unsupported nested table cells\n");
          END;
          self.oldwr := self.wr;
          self.wr := TextWr.New();

          IF action = Special.ColumnHeader THEN
            Wr.PutText(self.wr,"\\textbf");
          END;
          Wr.PutText(self.wr,"{");
      | Special.Html =>
          IF report THEN 
            Wr.PutText(self.wr,"\\documentclass{report}\n" & TeXPreamble);
          ELSE 
            Wr.PutText(self.wr,"\\documentclass{article}\n" & TeXPreamble);
          END;
      | Special.Header =>
          pos := ORD(Text.GetChar(e.gi,1)) - ORD('2');
          IF NOT report OR self.preface THEN INC(pos); END;
          Wr.PutText(self.wr,"\\" & SectionHeaders[pos]);
          IF self.preface THEN Wr.PutText(self.wr,"*"); END;
          Wr.PutText(self.wr,"{");
      | Special.Preface =>
          self.preface := TRUE;
      | Special.Appendix =>
          IF self.firstAppendix THEN
            Wr.PutText(self.wr,"\\appendix\n");
            self.firstAppendix := FALSE;
          END;
      | Special.Head =>
          self.inHead := TRUE;
          self.firstAuthor := TRUE;
          self.closeAuthor := FALSE;
      | Special.Article .. Special.Unpublished =>
          self.firstAuthor := TRUE;
          self.bibType := action;
          IF GetAttributeValue(e.attributes,"ID",t) THEN
            pos := Text.FindCharR(t,'#');
            Wr.PutText(self.wr,"\\bibitem{" & Text.Sub(t,pos + 1) & "}");
          ELSE
            Wr.PutText(Stdio.stderr,"Bibliographic entry without ID\n");
          END;

      | Special.Div => 
      | Special.IndexMark =>
          self.firstKey := TRUE;
          self.firstSee := TRUE;
      | Special.IndexKey =>
          IF NOT self.firstKey THEN Wr.PutText(self.wr,"!"); END;
          self.firstKey := FALSE;
      | Special.IndexSee =>
          IF self.firstSee THEN 
            Wr.PutText(self.wr,"|see{");
            self.firstSee := FALSE;
          ELSE 
            Wr.PutText(self.wr,", ");
          END;
      | Special.HeadAuthor =>
          IF self.firstAuthor THEN 
            Wr.PutText(self.wr,"\\author{");
            self.closeAuthor := TRUE;
            self.firstAuthor := FALSE;
          ELSE Wr.PutText(self.wr," \\and ");
          END;
      | Special.Author =>
          IF NOT self.firstAuthor THEN
            Wr.PutText(self.wr, ", ");
          ELSE
            self.firstAuthor := FALSE;
          END;
      | Special.Title =>
          IF self.bibType = Special.Report THEN
            Wr.PutText(self.wr,"{\\it ");
          ELSE
            Wr.PutText(self.wr,"``");
          END;
      | Special.Span =>
      | Special.Address =>
          IF self.inHead THEN Wr.PutText(self.wr,"\\\\");
          ELSE Wr.PutText(self.wr,"{\\sf ");
          END;
      | Special.Unimplemented, Special.Figure, Special.Rectangle,
        Special.Circle, Special.Ellipse, Special.Polyline, Special.Spline,
        Special.Picture, Special.Arc, Special.GText, Special.GGroup =>
          Wr.PutText(Stdio.stderr,"Unimplemented tag " & name & "\n");
      | Special.Math =>
          self.math := TRUE;
      | Special.MathArgs =>
          (* This action being on the stack will cause the childs to
             be put in {} as arguments to TeX commands should. *)
      | Special.None =>
      END;
    END;
  END StartElement;

PROCEDURE PutLabel(e: SGML.StartElementEvent; self: Translate) =
  VAR
    t: TEXT;
  BEGIN
    IF GetAttributeValue(e.attributes,"NAME",t) THEN
      Wr.PutText(self.wr,"\\label{" & EncodeLabel(t) & "}");
    END;

    IF GetAttributeValue(e.attributes,"ID",t) THEN
      Wr.PutText(self.wr,"\\label{" & EncodeLabel(t) & "}");
    END;
  END PutLabel;

(* Send the text associated with this ent tag and execute any associated
   special action. *)

PROCEDURE EndElement(self: Translate; 
    <*UNUSED*>READONLY e: SGML.EndElementEvent) =
  VAR
    n: INTEGER;
  BEGIN
    (* Unknown tag *)

    n := self.tagStack.remhi();

    IF NOT self.skip THEN
      CASE HTMLtoTeXtbl[n].action OF
      | Special.Font, Special.BaseFont, Special.Big, Special.Small,
        Special.Link, Special.Div, Special.Span,
        Special.Skip, Special.Header, Special.Html, Special.None =>
      | Special.Preformatted =>
          self.preformatted := FALSE;
      | Special.Table => EndTable(self);

      (* Changing rows, reset the column counter. *)

      | Special.Row =>
          INC(self.currentRow);
          self.currentColumn := 0;

      (* Check the column span value to know how many columns were covered
         by the last cell. Take the content from the text writer where it
         was accumulated. *)

      | Special.ColumnHeader, Special.ColumnData =>
          Wr.PutText(self.wr,"}");
          WITH cell = self.table.rows[self.currentRow][self.currentColumn] DO
            cell.content := TextWr.ToText(self.wr);
            INC(self.currentColumn,cell.columnSpan);
          END;
          self.wr := self.oldwr;
          self.oldwr := NIL;
      | Special.Appendix, Special.Article .. Special.Unpublished =>
      | Special.Preface =>
          self.preface := FALSE;
      | Special.Head =>
          self.inHead := FALSE;
      | Special.IndexMark =>
          IF NOT self.firstSee THEN Wr.PutText(self.wr,"}"); END;
      | Special.Title =>
          IF self.bibType = Special.Report THEN
            Wr.PutText(self.wr,"\\/}");
          ELSE
            Wr.PutText(self.wr,"''");
          END;
      | Special.IndexKey, Special.IndexSee, Special.HeadAuthor, 
        Special.Author =>
      | Special.Address =>
          IF NOT self.inHead THEN Wr.PutText(self.wr,"}"); END;
      | Special.Figure =>
      | Special.Rectangle =>
      | Special.Circle =>
      | Special.Ellipse =>
      | Special.Polyline =>
      | Special.Spline =>
      | Special.Picture =>
      | Special.Arc =>
      | Special.GText =>
      | Special.GGroup =>
      | Special.Unimplemented =>
      | Special.Math =>
          self.math := FALSE;
      | Special.MathArgs =>
      END;
      Wr.PutText(self.wr,HTMLtoTeXtbl[n].end);
    END;

    IF self.skip AND self.skipDepth = self.depth THEN self.skip := FALSE; END;

    IF self.math AND (NOT self.skip) AND
        HTMLtoTeXtbl[self.tagStack.gethi()].action = Special.MathArgs THEN
      Wr.PutText(self.wr,"}");
    END;

    DEC(self.depth);
  END EndElement;

(* Extract the integer value of an attribute, noting its sign if
   explicit. *)

PROCEDURE GetAttributeIntValue(attrs: REF ARRAY OF SGML.Attribute; 
    name: TEXT; VAR plus, valid: BOOLEAN; VAR val: INTEGER): BOOLEAN =
  VAR
    rd: Rd.T;
    t: TEXT;
    c: CHAR;
  BEGIN
    IF attrs = NIL THEN RETURN FALSE; END;
    FOR i := 0 TO LAST(attrs^) DO
      WITH a = attrs[i] DO
        IF Text.Equal(a.name,name) THEN
          IF a.tokens # NIL THEN t := a.tokens;
          ELSIF a.cdataChunks # NIL THEN
            t := "";
            FOR j := 0 TO LAST(a.cdataChunks^) DO 
              t := t & a.cdataChunks[i].data; 
            END;
          ELSE
            valid := FALSE;
            RETURN TRUE;
          END;
          TRY
            rd := TextRd.New(t);
            Lex.Skip(rd);
            c := Rd.GetChar(rd);
            Rd.UnGetChar(rd);
            val := Lex.Int(rd);
            plus := c = '+';
            valid := TRUE;
          EXCEPT ELSE
            valid := FALSE;
          END;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END GetAttributeIntValue;

(* Extract a textual attribute value. *)

PROCEDURE GetAttributeValue(attrs: REF ARRAY OF SGML.Attribute; 
    name: TEXT; VAR val: TEXT): BOOLEAN =
  VAR
    t: TEXT;
  BEGIN
    IF attrs = NIL THEN RETURN FALSE; END;
    FOR i := 0 TO LAST(attrs^) DO
      WITH a = attrs[i] DO
        IF Text.Equal(a.name,name) THEN
          IF a.tokens # NIL THEN t := a.tokens;
          ELSIF a.cdataChunks # NIL THEN
            t := "";
            FOR j := 0 TO LAST(a.cdataChunks^) DO 
              t := t & a.cdataChunks[j].data; 
            END;
          ELSE
            t := "";
          END;
          val := t;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END GetAttributeValue;

(* Change a textual attribute value. *)

PROCEDURE SetAttributeValue(attrs: REF ARRAY OF SGML.Attribute; 
    name, val: TEXT;) =
  BEGIN
    FOR i := 0 TO LAST(attrs^) DO
      WITH a = attrs[i] DO
        IF Text.Equal(a.name,name) THEN
          a.cdataChunks := NEW(REF ARRAY OF SGML.CdataChunk,1);
          a.cdataChunks[0].data := val;
        END;
      END;
    END;
  END SetAttributeValue;

(* Check that the font size is within limits and set it. *)

PROCEDURE SetFontSize(self: Translate; size: INTEGER) =
  BEGIN
    size := MAX(MIN(size,LAST(FontSizes)),FIRST(FontSizes));
    self.fontSize := size;
    Wr.PutText(self.wr,"{\\" & FontSizes[size] & " ");
  END SetFontSize;

(* The table size is deduced from the rows/cells encountered. Enlarge
   it as more rows/cells are found. *)

PROCEDURE CheckTableSize(table: Table; row, column: CARDINAL) =
  VAR
    rows: REF ARRAY OF Row;
    r: REF ARRAY OF Cell;
  BEGIN
    IF table.nbRow <= row THEN
      rows := table.rows;
      table.rows := NEW(REF ARRAY OF Row,row + 1);
      SUBARRAY(table.rows^,0,table.nbRow) := rows^;
      FOR i := table.nbRow TO row DO 
        table.rows[i] := NEW(REF ARRAY OF Cell,table.nbColumn);
      END;
      table.nbRow := row + 1;
    END;
    IF table.nbColumn <= column THEN
      FOR i := 0 TO table.nbRow - 1 DO
        r := table.rows[i];
        table.rows[i] := NEW(REF ARRAY OF Cell,column + 1);
        SUBARRAY(table.rows[i]^,0,table.nbColumn) := r^;
      END;
      table.nbColumn := column + 1;
    END;
  END CheckTableSize;

(* Initialize the structures to record an HTML table. *)

PROCEDURE BeginTable(self: Translate) =
  BEGIN
    IF self.table # NIL THEN
      Wr.PutText(Stdio.stderr,"Nested TABLE cannot be converted\n");
    ELSE
      self.table := NEW(Table, nbRow := 0, nbColumn := 0, caption := NIL,
          border := 1, rows := NEW(REF ARRAY OF Row,0));
      self.currentRow := 0;
      self.currentColumn := 0;
    END;
  END BeginTable;

(* Convert the accumulated table content into a TeX table. Values of
   rowSpan other than 1 are currently ignored. *)

PROCEDURE EndTable(self: Translate) =
  VAR
    row, column: CARDINAL;
    table := self.table;
  BEGIN
    Wr.PutText(self.wr,"\\begin{tabular}{");
    IF table.border > 0 THEN
      FOR i := 0 TO table.nbColumn DO Wr.PutText(self.wr,"|c"); END;
      Wr.PutText(self.wr,"|}\\hline\n");
    ELSE
      FOR i := 0 TO table.nbColumn DO Wr.PutText(self.wr,"c"); END;
      Wr.PutText(self.wr,"}");
    END;

    row := 0;
    column := 0;
    WHILE row < table.nbRow DO
      WHILE column < table.nbColumn DO
        WITH cell = table.rows[row][column] DO
          IF table.border > 0 THEN
            Wr.PutText(self.wr,"\\multicolumn{" & Fmt.Int(cell.columnSpan) &
                "}{|" & HAlignText[cell.hAlign] & "|}");
          ELSE
            Wr.PutText(self.wr,"\\multicolumn{" & Fmt.Int(cell.columnSpan) &
                "}{" & HAlignText[cell.hAlign] & "}");
          END;
          IF cell.rowSpan = 1 THEN 
            Wr.PutText(self.wr,"{" & cell.content & "}");
          ELSE
            Wr.PutText(self.wr,"{}");
          END;
          INC(column,cell.columnSpan);
          IF column < table.nbColumn - 1 THEN Wr.PutText(self.wr," & "); END;
        END;
      END;
      Wr.PutText(self.wr," \\\\");
      IF table.border > 0 THEN Wr.PutText(self.wr,"\\hline "); END;
      INC(row);
    END;
    IF table.caption # NIL THEN 
      Wr.PutText(self.wr,"\\caption{" & table.caption & "}");
    END;
    Wr.PutText(self.wr,"\\end{tabular}");
    self.table := NIL;
  END EndTable;

(* Escape all the special characters. *)

PROCEDURE EncodeForTeX(self: HTMLtoTeX; t: TEXT): TEXT =
  VAR
    car: [0..65535];
    c: CHAR;
    i := 0;
    end := Text.Length(t);
    startRef, endRef: INTEGER;
    wr := TextWr.New();
  BEGIN
    WHILE i < end DO
      c := Text.GetChar(t,i);
      IF c = '&' THEN
        startRef := i;
        endRef := Text.FindChar(t,';',startRef);
        IF endRef >= 0 THEN
          i := endRef;
          IF NOT SGML.CharRefToCode(Text.Sub(t,startRef,endRef - startRef + 1),
              car) THEN
            endRef := -1;
          END;
        END;
        IF endRef < 0 THEN
          car := ORD('?');
          self.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
            "Unexpected character data at position " & Fmt.Int(startRef) &
            " in " & t });
        END;
      ELSE
        car := ORD(c);
      END;
      INC(i);

      CASE car OF
      | ORD('\r') => Wr.PutText(wr,"\n");
      | ORD('"') =>
          IF self.preformatted THEN Wr.PutText(wr,"\"");
          ELSE Wr.PutText(wr,"''");
          END;
      | ORD('#') => Wr.PutText(wr,"\\#");
      | ORD('$') => Wr.PutText(wr,"\\$");
      | ORD('%') => Wr.PutText(wr,"\\%");
      | ORD('&') => Wr.PutText(wr,"\\&");
      | ORD('/') => Wr.PutText(wr,"\\slash ");
      | ORD('<') => Wr.PutText(wr,"\\(<\\)");
      | ORD('>') => Wr.PutText(wr,"\\(>\\)");
      | ORD('_') => Wr.PutText(wr,"\\_");
      | ORD('~') => Wr.PutText(wr,"{\\tt\\symbol{\"7e}}");
      | ORD('^') => Wr.PutText(wr,"{\\tt\\symbol{\"5e}}");
      | ORD('\\') => MathCar(self,wr,"\\backslash");
      | ORD('{') => Wr.PutText(wr,"\\{");
      | ORD('}') => Wr.PutText(wr,"\\}");
      | ORD('[') => Wr.PutText(wr,"{\\char91}");
      | ORD(']') => Wr.PutText(wr,"{\\char93}");
      | ORD('|') => MathCar(self,wr,"|");
      | ORD(' ') => 
          IF self.preformatted THEN Wr.PutText(wr,"~");
          ELSE Wr.PutText(wr," ");
          END;
      | 191..255 => 
          Wr.PutText(wr,IsoLatinTbl[car - 191]);

      | 161 => Wr.PutText(wr,"!`");
      | 162 => Wr.PutText(wr,"c\\llap/");
      | 163 => Wr.PutText(wr,"\\pounds{}");
      | 164 => Wr.PutText(wr,"o\\llap{x}");
      | 165 => Wr.PutText(wr,"Y\\llap=");
      | 166 => Wr.PutText(wr,"\\verb+|+");
      | 167 => Wr.PutText(wr,"\\S{}");
      | 168 => Wr.PutText(wr,"\\\"{ }");
      | 169 => Wr.PutText(wr,"\\copyright{}");
      | 170 => MathCar(self,wr,"$^a$");
      | 171 => MathCar(self,wr,"${\\footnotesize <}$");
      | 172 => MathCar(self,wr,"$\\neg$");
      | 173 => Wr.PutText(wr,"-");
      | 174 => Wr.PutText(wr,"\\reg ");
      | 175 => Wr.PutText(wr,"\\={ }");
      | 176 => Wr.PutText(wr,"\\degree{}");
      | 177 => MathCar(self,wr,"\\pm");
      | 178 => MathCar(self,wr,"$^2$");
      | 179 => MathCar(self,wr,"$^3$");
      | 180 => Wr.PutText(wr,"\\'{ }");
      | 181 => MathCar(self,wr,"$\\mu$");
      | 182 => Wr.PutText(wr,"\\P{}");
      | 183 => MathCar(self,wr,"$\\bullet$");
      | 184 => Wr.PutText(wr,"\\c{ }");
      | 185 => MathCar(self,wr,"$^1$");
      | 186 => MathCar(self,wr,"$^{\\tiny o}$");
      | 187 => MathCar(self,wr,"${\\footnotesize >}$");
      | 188 => MathCar(self,wr,"$\\frac14$");
      | 189 => MathCar(self,wr,"$\\frac12$");
      | 190 => MathCar(self,wr,"$\\frac34$");

      | 338 => (* latin capital ligature oe, u+0152 ISOlat2  *)
          Wr.PutText(wr,"\\OE");
      | 339 => (* latin small ligature oe, u+0153 ISOlat2  *)
          Wr.PutText(wr,"\\oe");
      | 352 => (* latin capital letter s with caron, u+0160 ISOlat2  *)
          Wr.PutText(wr,"\\Scaron");
      | 353 => (* latin small letter s with caron, u+0161 ISOlat2  *)
          Wr.PutText(wr,"\\scaron");
      | 376 => (* latin capital letter y with diaeresis, u+0178 ISOlat2  *)
          Wr.PutText(wr,"\\u{Y}");
      | 710 => (* modifier letter circumflex accent, u+02C6 ISOpub  *)
          Wr.PutText(wr,"\\^{}");
      | 732 => (* small tilde, u+02DC ISOdia  *)
          Wr.PutText(wr,"\\~{}");
      | 8194 => (* en space, u+2002 ISOpub  *)
          Wr.PutText(wr,"\\ ");
      | 8195 => (* em space, u+2003 ISOpub  *)
          Wr.PutText(wr,"\\ ");
      | 8201 => (* thin space, u+2009 ISOpub  *)
          Wr.PutText(wr,"\\ ");
      | 8204 => (* zero width non-joiner, u+200C NEW RFC 2070  *)
          Wr.PutText(wr,"\\zwnj");
      | 8205 => (* zero width joiner, u+200D NEW RFC 2070  *)
          Wr.PutText(wr,"\\zwj");
      | 8206 => (* left-to-right mark, u+200E NEW RFC 2070  *)
          MathCar(self,wr,"\\rightarrow");
      | 8207 => (* right-to-left mark, u+200F NEW RFC 2070  *)
          MathCar(self,wr,"\\leftarrow");
      | 8211 => (* en dash, u+2013 ISOpub  *)
          Wr.PutText(wr,"-");
      | 8212 => (* em dash, u+2014 ISOpub  *)
          Wr.PutText(wr,"--");
      | 8216 => (* left single quotation mark, u+2018 ISOnum  *)
          Wr.PutText(wr,"\\lsquo");
      | 8217 => (* right single quotation mark, u+2019 ISOnum  *)
          Wr.PutText(wr,"\\rsquo");
      | 8218 => (* single low-9 quotation mark, u+201A NEW  *)
          Wr.PutText(wr,"\\sbquo");
      | 8220 => (* left double quotation mark, u+201C ISOnum  *)
          Wr.PutText(wr,"\\ldquo");
      | 8221 => (* right double quotation mark, u+201D ISOnum  *)
          Wr.PutText(wr,"\\rdquo");
      | 8222 => (* double low-9 quotation mark, u+201E NEW  *)
          Wr.PutText(wr,"\\bdquo");
      | 8224 => (* dagger, u+2020 ISOpub  *)
          MathCar(self,wr,"\\dagger");
      | 8225 => (* double dagger, u+2021 ISOpub  *)
          MathCar(self,wr,"\\ddagger");
      | 8240 => (* per mille sign, u+2030 ISOtech  *)
          Wr.PutText(wr,"\\permil");
      | 8249 => (* single left-pointing angle quotation mark, u+2039 ISO proposed  *)
          Wr.PutText(wr,"\\lsaquo");
      | 8250 => (* single right-pointing angle quotation mark, u+203A ISO proposed  *)
          Wr.PutText(wr,"\\rsaquo");
      | 402 => (* latin small f with hook, =function, =florin, u+0192 ISOtech  *)
          Wr.PutText(wr,"\\fnof");
      | 913 => (* greek capital letter alpha,  u+0391  *)
          MathCar(self,wr,"\\Alpha");
      | 914 => (* greek capital letter beta,  u+0392  *)
          MathCar(self,wr,"\\Beta");
      | 915 => (* greek capital letter gamma,  u+0393 ISOgrk3  *)
          MathCar(self,wr,"\\Gamma");
      | 916 => (* greek capital letter delta,  u+0394 ISOgrk3  *)
          MathCar(self,wr,"\\Delta");
      | 917 => (* greek capital letter epsilon,  u+0395  *)
          MathCar(self,wr,"\\Epsilon");
      | 918 => (* greek capital letter zeta,  u+0396  *)
          MathCar(self,wr,"\\Zeta");
      | 919 => (* greek capital letter eta,  u+0397  *)
          MathCar(self,wr,"\\Eta");
      | 920 => (* greek capital letter theta,  u+0398 ISOgrk3  *)
          MathCar(self,wr,"\\Theta");
      | 921 => (* greek capital letter iota,  u+0399  *)
          MathCar(self,wr,"\\Iota");
      | 922 => (* greek capital letter kappa,  u+039A  *)
          MathCar(self,wr,"\\Kappa");
      | 923 => (* greek capital letter lambda,  u+039B ISOgrk3  *)
          MathCar(self,wr,"\\Lambda");
      | 924 => (* greek capital letter mu,  u+039C  *)
          MathCar(self,wr,"\\Mu");
      | 925 => (* greek capital letter nu,  u+039D  *)
          MathCar(self,wr,"\\Nu");
      | 926 => (* greek capital letter xi,  u+039E ISOgrk3  *)
          MathCar(self,wr,"\\Xi");
      | 927 => (* greek capital letter omicron,  u+039F  *)
          MathCar(self,wr,"O");
      | 928 => (* greek capital letter pi,  u+03A0 ISOgrk3  *)
          MathCar(self,wr,"\\Pi");
      | 929 => (* greek capital letter rho,  u+03A1  *)
          MathCar(self,wr,"\\Rho");
      | 931 => (* greek capital letter sigma,  u+03A3 ISOgrk3  *)
          MathCar(self,wr,"\\Sigma");
      | 932 => (* greek capital letter tau,  u+03A4  *)
          MathCar(self,wr,"\\Tau");
      | 933 => (* greek capital letter upsilon,  u+03A5 ISOgrk3  *)
          MathCar(self,wr,"\\Upsilon");
      | 934 => (* greek capital letter phi,  u+03A6 ISOgrk3  *)
          MathCar(self,wr,"\\Phi");
      | 935 => (* greek capital letter chi,  u+03A7  *)
          MathCar(self,wr,"\\Chi");
      | 936 => (* greek capital letter psi,  u+03A8 ISOgrk3  *)
          MathCar(self,wr,"\\Psi");
      | 937 => (* greek capital letter omega,  u+03A9 ISOgrk3  *)
          MathCar(self,wr,"\\Omega");
      | 945 => (* greek small letter alpha, u+03B1 ISOgrk3  *)
          MathCar(self,wr,"\\alpha");
      | 946 => (* greek small letter beta,  u+03B2 ISOgrk3  *)
          MathCar(self,wr,"\\beta");
      | 947 => (* greek small letter gamma,  u+03B3 ISOgrk3  *)
          MathCar(self,wr,"\\gamma");
      | 948 => (* greek small letter delta,  u+03B4 ISOgrk3  *)
          MathCar(self,wr,"\\delta");
      | 949 => (* greek small letter epsilon,  u+03B5 ISOgrk3  *)
          MathCar(self,wr,"\\epsilon");
      | 950 => (* greek small letter zeta,  u+03B6 ISOgrk3  *)
          MathCar(self,wr,"\\zeta");
      | 951 => (* greek small letter eta,  u+03B7 ISOgrk3  *)
          MathCar(self,wr,"\\eta");
      | 952 => (* greek small letter theta,  u+03B8 ISOgrk3  *)
          MathCar(self,wr,"\\theta");
      | 953 => (* greek small letter iota,  u+03B9 ISOgrk3  *)
          MathCar(self,wr,"\\iota");
      | 954 => (* greek small letter kappa,  u+03BA ISOgrk3  *)
          MathCar(self,wr,"\\kappa");
      | 955 => (* greek small letter lambda,  u+03BB ISOgrk3  *)
          MathCar(self,wr,"\\lambda");
      | 956 => (* greek small letter mu,  u+03BC ISOgrk3  *)
          MathCar(self,wr,"\\mu");
      | 957 => (* greek small letter nu,  u+03BD ISOgrk3  *)
          MathCar(self,wr,"\\nu");
      | 958 => (* greek small letter xi,  u+03BE ISOgrk3  *)
          MathCar(self,wr,"\\xi");
      | 959 => (* greek small letter omicron,  u+03BF NEW  *)
          Wr.PutText(wr,"o");
      | 960 => (* greek small letter pi,  u+03C0 ISOgrk3  *)
          MathCar(self,wr,"\\pi");
      | 961 => (* greek small letter rho,  u+03C1 ISOgrk3  *)
          MathCar(self,wr,"\\rho");
      | 962 => (* greek small letter final sigma,  u+03C2 ISOgrk3  *)
          MathCar(self,wr,"\\sigma");
      | 963 => (* greek small letter sigma,  u+03C3 ISOgrk3  *)
          MathCar(self,wr,"\\sigma");
      | 964 => (* greek small letter tau,  u+03C4 ISOgrk3  *)
          MathCar(self,wr,"\\tau");
      | 965 => (* greek small letter upsilon,  u+03C5 ISOgrk3  *)
          MathCar(self,wr,"\\upsilon");
      | 966 => (* greek small letter phi,  u+03C6 ISOgrk3  *)
          MathCar(self,wr,"\\phi");
      | 967 => (* greek small letter chi,  u+03C7 ISOgrk3  *)
          MathCar(self,wr,"\\chi");
      | 968 => (* greek small letter psi,  u+03C8 ISOgrk3  *)
          MathCar(self,wr,"\\psi");
      | 969 => (* greek small letter omega,  u+03C9 ISOgrk3  *)
          MathCar(self,wr,"\\omega");
      | 977 => (* greek small letter theta symbol,  u+03D1 NEW  *)
          MathCar(self,wr,"\\theta");
      | 978 => (* greek upsilon with hook symbol,  u+03D2 NEW  *)
          MathCar(self,wr,"\\upsi");
      | 982 => (* greek pi symbol,  u+03D6 ISOgrk3  *)
          MathCar(self,wr,"\\pi");
      | 8226 => (* bullet, =black small circle, u+2022 ISOpub   *)
          MathCar(self,wr,"\\bullet");
      | 8230 => (* horizontal ellipsis, =three dot leader, u+2026 ISOpub   *)
          Wr.PutText(wr,"\\ldots");
      | 8242 => (* prime, =minutes, =feet, u+2032 ISOtech  *)
          MathCar(self,wr,"\\prime");
      | 8243 => (* double prime, =seconds, =inches, u+2033 ISOtech  *)
          MathCar(self,wr,"\\Prime\\Prime");
      | 8254 => (* overline, =spacing overscore, u+203E NEW  *)
          MathCar(self,wr,"\\overline{\\ }");
      | 8260 => (* fraction slash, u+2044 NEW  *)
          Wr.PutText(wr,"/");
      | 8472 => (* script capital P,=power set,=Weierstrass p, u+2118 ISOamso*)
          MathCar(self,wr,"\\wp");
      | 8465 => (* blackletter capital I, =imaginary part, u+2111 ISOamso  *)
          MathCar(self,wr,"\\imath");
      | 8476 => (* blackletter capital R, =real part symbol, u+211C ISOamso  *)
          MathCar(self,wr,"\\Re");
      | 8482 => (* trade mark sign, u+2122 ISOnum  *)
          MathCar(self,wr,"^{TM}");
      | 8501 => (* alef symbol, =first transfinite cardinal, u+2135 NEW  *)
          MathCar(self,wr,"\\alef");
      | 8592 => (* leftwards arrow, u+2190 ISOnum  *)
          MathCar(self,wr,"\\leftarrow");
      | 8593 => (* upwards arrow, u+2191 ISOnum *)
          MathCar(self,wr,"\\uparrow");
      | 8594 => (* rightwards arrow, u+2192 ISOnum  *)
          MathCar(self,wr,"\\rightarrow");
      | 8595 => (* downwards arrow, u+2193 ISOnum  *)
          MathCar(self,wr,"\\downarrow");
      | 8596 => (* left right arrow, u+2194 ISOamsa  *)
          MathCar(self,wr,"\\leftrightarrow");
      | 8629 => (* downwards arrow with corner leftwards, =carriage return, u+21B5 NEW  *)
          MathCar(self,wr,"\\hookleftarrow");
      | 8656 => (* leftwards double arrow, u+21D0 ISOtech  *)
          MathCar(self,wr,"\\Leftarrow");
      | 8657 => (* upwards double arrow, u+21D1 ISOamsa  *)
          MathCar(self,wr,"\\Uparrow");
      | 8658 => (* rightwards double arrow, u+21D2 ISOtech  *)
          MathCar(self,wr,"\\Rightarrow");
      | 8659 => (* downwards double arrow, u+21D3 ISOamsa  *)
          MathCar(self,wr,"\\Downarrow");
      | 8660 => (* left right double arrow, u+21D4 ISOamsa  *)
          MathCar(self,wr,"\\Leftrightarrow");
      | 8704 => (* for all, u+2200 ISOtech  *)
          MathCar(self,wr,"\\forall");
      | 8706 => (* partial differential, u+2202 ISOtech   *)
          MathCar(self,wr,"\\partial");
      | 8707 => (* there exists, u+2203 ISOtech  *)
          MathCar(self,wr,"\\exist");
      | 8709 => (* empty set, =null set, =diameter, u+2205 ISOamso  *)
          MathCar(self,wr,"\\emptyset");
      | 8711 => (* nabla, =backward difference, u+2207 ISOtech  *)
          MathCar(self,wr,"\\nabla");
      | 8712 => (* element of, u+2208 ISOtech  *)
          MathCar(self,wr,"\\in");
      | 8713 => (* not an element of, u+2209 ISOtech  *)
          MathCar(self,wr,"\\notin");
      | 8715 => (* contains as member, u+220B ISOtech  *)
          MathCar(self,wr,"\\ni");
      | 8719 => (* n-ary product, =product sign, u+220F ISOamsb  *)
          MathCar(self,wr,"\\prod");
      | 8721 => (* n-ary sumation, u+2211 ISOamsb  *)
          MathCar(self,wr,"\\sum");
      | 8722 => (* minus sign, u+2212 ISOtech  *)
          Wr.PutText(wr,"-");
      | 8727 => (* asterisk operator, u+2217 ISOtech  *)
          MathCar(self,wr,"\\ast");
      | 8730 => (* square root, =radical sign, u+221A ISOtech  *)
          MathCar(self,wr,"\\surd");
      | 8733 => (* proportional to, u+221D ISOtech  *)
          MathCar(self,wr,"\\propto");
      | 8734 => (* infinity, u+221E ISOtech  *)
          MathCar(self,wr,"\\infty");
      | 8736 => (* angle, u+2220 ISOamso  *)
          MathCar(self,wr,"\\angle");
      | 8869 => (* logical and, =wedge, u+2227 ISOtech  *)
          MathCar(self,wr,"\\wedge");
      | 8870 => (* logical or, =vee, u+2228 ISOtech  *)
          MathCar(self,wr,"\\vee");
      | 8745 => (* intersection, =cap, u+2229 ISOtech  *)
          MathCar(self,wr,"\\cap");
      | 8746 => (* union, =cup, u+222A ISOtech  *)
          MathCar(self,wr,"\\cup");
      | 8747 => (* integral, u+222B ISOtech  *)
          MathCar(self,wr,"\\int");
      | 8756 => (* therefore, u+2234 ISOtech  *)
          Wr.PutText(wr,"\\there4");
      | 8764 => (* tilde operator, =varies with, =similar to, u+223C ISOtech *)
          Wr.PutText(wr,"\\~ ");
      | 8773 => (* approximately equal to, u+2245 ISOtech  *)
          MathCar(self,wr,"\\approx");
      | 8776 => (* almost equal to, =asymptotic to, u+2248 ISOamsr  *)
          MathCar(self,wr,"\\asymp");
      | 8800 => (* not equal to, u+2260 ISOtech  *)
          MathCar(self,wr,"\\neq");
      | 8801 => (* identical to, u+2261 ISOtech  *)
          MathCar(self,wr,"\\equiv");
      | 8804 => (* less-than or equal to, u+2264 ISOtech  *)
          MathCar(self,wr,"\\leq");
      | 8805 => (* greater-than or equal to, u+2265 ISOtech  *)
          MathCar(self,wr,"\\geq");
      | 8834 => (* subset of, u+2282 ISOtech  *)
          MathCar(self,wr,"\\subset");
      | 8835 => (* superset of, u+2283 ISOtech  *)
          MathCar(self,wr,"\\supset");
      | 8836 => (* not a subset of, u+2284 ISOamsn  *)
          MathCar(self,wr,"\\nsub");
      | 8838 => (* subset of or equal to, u+2286 ISOtech  *)
          MathCar(self,wr,"\\subseteq");
      | 8839 => (* superset of or equal to, u+2287 ISOtech  *)
          MathCar(self,wr,"\\supseteq");
      | 8853 => (* circled plus, =direct sum, u+2295 ISOamsb  *)
          MathCar(self,wr,"\\oplus");
      | 8855 => (* circled times, =vector product, u+2297 ISOamsb  *)
          MathCar(self,wr,"\\otimes");
      (* | 8869 =>  up tack, =orthogonal to, =perpendicular, u+22A5 ISOtech
          MathCar(self,wr,"\\perp"); *)
      | 8901 => (* dot operator, u+22C5 ISOamsb  *)
          MathCar(self,wr,"\\sdot");
      | 8968 => (* left ceiling, =apl upstile, u+2308, ISOamsc   *)
          MathCar(self,wr,"\\lceil");
      | 8969 => (* right ceiling, u+2309, ISOamsc   *)
          MathCar(self,wr,"\\rceil");
      | 8970 => (* left floor, =apl downstile, u+230A, ISOamsc   *)
          MathCar(self,wr,"\\lfloor");
      | 8971 => (* right floor, u+230B, ISOamsc   *)
          MathCar(self,wr,"\\rfloor");
      | 9001 => (* left-pointing angle bracket, =bra, u+2329 ISOtech  *)
          Wr.PutText(wr,"<");
      | 9002 => (* right-pointing angle bracket, =ket, u+232A ISOtech  *)
          Wr.PutText(wr,">");
      | 9674 => (* lozenge, u+25CA ISOpub  *)
          MathCar(self,wr,"\\loz");
      | 9824 => (* black spade suit, u+2660 ISOpub  *)
          MathCar(self,wr,"\\spadesuit");
      | 9827 => (* black club suit, =shamrock, u+2663 ISOpub  *)
          MathCar(self,wr,"\\clubsuit");
      | 9829 => (* black heart suit, =valentine, u+2665 ISOpub  *)
          MathCar(self,wr,"\\heartsuit");
      | 9830 => (* black diamond suit, u+2666 ISOpub  *)
          MathCar(self,wr,"\\diamondsuit");
      ELSE
        IF car <= 255 THEN
          c := VAL(car,CHAR);
          Wr.PutChar(wr,c);
        ELSE
          Wr.PutChar(wr,'?');
          self.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
            "Unexpected character data at position " & Fmt.Int(startRef) &
            " in " & t });
        END;
      END;
    END;
    RETURN TextWr.ToText(wr);
  END EncodeForTeX;

PROCEDURE MathCar(self: HTMLtoTeX; wr: Wr.T; t: TEXT) =
  BEGIN
    IF self.math THEN
      Wr.PutText(wr,t);
    ELSE
      Wr.PutText(wr,"$" & t & "$");
    END;
  END MathCar;

PROCEDURE EncodeLabel(t: TEXT): TEXT =
  VAR
    c: CHAR;
    wr := TextWr.New();
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      c := Text.GetChar(t,i);
      CASE c OF
      | '#' => Wr.PutText(wr,"/");
      ELSE
        Wr.PutChar(wr,c);
      END;
    END;
    RETURN TextWr.ToText(wr);
  END EncodeLabel;

PROCEDURE IgnoreWhite(self: HTMLtoTeX; t: TEXT) =
  VAR
    c: CHAR;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      c := Text.GetChar(t,i);
      CASE c OF
      | ' ', '\t', '\r', '\n' =>
      ELSE
        self.error(SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
            "Unexpected character data in " & 
            HTMLtoTeXtbl[self.tagStack.gethi()].tag});
      END;
    END;
  END IgnoreWhite;

TYPE
  Special = {Font, BaseFont, Big, Small, Address, Table, Row, ColumnHeader,
      ColumnData, Preformatted, Link, Div, Span, Skip, Appendix,
      Head, Article, Report, Booklet, InBook, InCollection, InProceedings,
      Manual, MSThesis, PHDThesis, MiscEntry, Proceedings, TechReport,
      Unpublished, IndexMark, IndexKey, IndexSee, HeadAuthor, Author,
      Title, Header, Html, Preface, Figure, 
      Rectangle, Circle, Ellipse, Polyline, Spline, 
      Picture, Arc, GText, GGroup, Math, MathArgs, None, Unimplemented};

  TagTranslate = RECORD 
      tag, start, end: TEXT; 
      action: Special;
      pcdata: BOOLEAN;
    END;

  TT = TagTranslate;

  TranslateTable = ARRAY OF TT;

CONST
  TeXPreamble =
      "\\usepackage{makeidx}\n\\makeindex\n" &
      "\\newcommand{\\mroot}[2]{\\sqrt[#2]{#1}}\n" &
      "\\newcommand{\\msub}[2]{{#1}_{#2}}\n" &
      "\\newcommand{\\msup}[2]{{#1}^{#2}}\n" &
      "\\newcommand{\\msubsup}[3]{{#1}_{#2}^{#3}}\n" &
      "\\newcommand{\\mmultiscripts}[3]{{#1}_{#2}^{#3}}\n" &
      "\\newcommand{\\munder}[2]{{#1}_{#2}}\n" &
      "\\newcommand{\\mover}[2]{{#1}^{#2}}\n" &
      "\\newcommand{\\munderover}[3]{{#1}_{#2}^{#3}}\n" &
      "\\newcommand{\\mint}[4]{{\\int}_{#2}^{#3}{#4}}\n" &
      "\\newcommand{\\mproduct}[4]{{\\prod}_{#2}^{#3}{#4}}\n";

  OkForLabel = SET OF Special{Special.Table, Special.Preformatted,
      Special.Div, Special.Appendix, Special.None};

  FontSizes = ARRAY OF TEXT{"scriptsize", "footnotesize", "small",
      "normalsize", "large", "Large", "LARGE", "huge", "Huge"};

  HAlignText = ARRAY HAlign OF TEXT{"l", "c", "r"};

  VAlignText = ARRAY VAlign OF TEXT{"t", "m", "b"}; <*NOWARN*>

  SectionHeaders = ARRAY OF TEXT{
      "chapter", "section", "subsection", "subsubsection", "paragraph",
      "subparagraph", "subparagraph", "subparagraph", "subparagraph",
      "subparagraph"};

  (* How to express the various special characters in TeX. *)

  IsoLatinTbl = ARRAY OF TEXT{
      "?`", "\\`A{}", "\\'A{}", "\\^A{}", "\\~A{}",             (* 191 *)
      "\\\"A", "\\AA{}", "\\AE{}", "\\c{C}", "\\`E",            (* 196 *)
      "\\'E", "\\^E", "\\\"E", "\\`I", "\\'I",                  (* 201 *)
      "\\^I", "\\\"I", "(Eth)", "\\~N", "\\`O",                 (* 206 *)
      "\\'O", "\\^O", "\\~O", "\\\"O", "$\\times$",             (* 211 *)
      "\\O{}", "\\`U", "\\'U", "\\^U", "\\\"U",                 (* 216 *)
      "\\'Y", "(Thorn)", "\\ss{}", "\\`a", "\\'a",              (* 221 *)
      "\\^a", "\\~a", "\\\"a", "\\aa{}", "\\ae{}",              (* 226 *)
      "\\c{c}", "\\`e", "\\'e", "\\^e", "\\\"e",                (* 231 *)
      "\\`\\i", "\\'\\i", "\\^\\i", "\\\"\\i", "(eth)",         (* 236 *)
      "\\~n", "\\`o", "\\'o", "\\^o", "\\~o",                   (* 241 *)
      "\\\"o", "$\\div$", "\\o{}", "\\`u", "\\'u",              (* 246 *)
      "\\^u", "\\\"u", "\\'y", "(thorn)", "\\\"y"};             (* 251 *)

  (* How to convert each HTML 3.2 tag into TeX. *)

  HTMLtoTeXtbl = TranslateTable{
    TT{"", "", "", Special.None, TRUE},
    TT{"HTML", "", "\\printindex\\end{document}\n", Special.Html, TRUE},
    TT{"TITLE", "\\title{", "}\n" , Special.None, TRUE},
    TT{"B", "{\\bf ", "}", Special.None, TRUE},
    TT{"I", "{\\it ", "\\/}", Special.None, TRUE},
    TT{"TT", "{\\tt ", "}", Special.None, TRUE},
    TT{"U", "\\underline{", "}", Special.None, TRUE},
    TT{"STRIKE", "{\\bf ", "}", Special.None, TRUE},
    TT{"BIG", "", "}", Special.Big, TRUE},
    TT{"SMALL", "", "}", Special.Small, TRUE},
    TT{"SUB", "$_{", "}$", Special.None, TRUE},
    TT{"SUP", "$^{", "}$", Special.None, TRUE},
    TT{"EM", "{\\bf ", "}", Special.None, TRUE},
    TT{"STRONG", "{\\bf ", "}", Special.None, TRUE},
    TT{"DFN", "{\\sf ", "}", Special.None, TRUE},
    TT{"CODE", "{\\tt ", "}", Special.None, TRUE},
    TT{"SAMP", "{\\tt ", "}", Special.None, TRUE},
    TT{"KBD", "{\\tt ", "}", Special.None, TRUE},
    TT{"VAR", "{\\tt ", "}", Special.None, TRUE},
    TT{"CITE", "{\\it ", "}", Special.None, TRUE},
    TT{"IMG", "", "", Special.None, TRUE}, (* *)
    TT{"HR", "\\par\n\\hline", "", Special.None, TRUE},
    TT{"PRE", "\\begin{flushleft}{\\obeylines\\obeyspaces\n", 
        "}\n\\end{flushleft}", Special.Preformatted, TRUE},
    TT{"P", "\n", "", Special.None, TRUE},
    TT{"A", "", "", Special.Link, TRUE}, 
    TT{"H1", "\\title{", "}\n\\begin{document}\n\\maketitle\\tableofcontents\n"
        , Special.None, TRUE},
    TT{"H2", "", "}", Special.Header, TRUE},
    TT{"H3", "", "}", Special.Header, TRUE},
    TT{"H4", "", "}", Special.Header, TRUE},
    TT{"H5", "", "}", Special.Header, TRUE},
    TT{"H6", "", "}", Special.Header, TRUE},
    TT{"H7", "", "}", Special.Header, TRUE},
    TT{"H8", "", "}", Special.Header, TRUE},
    TT{"H9", "", "}", Special.Header, TRUE},
    TT{"CENTER", "\\begin{center}\n", "\\end{center}\n", Special.None, TRUE},
    TT{"UL", "\\begin{itemize}", "\\end{itemize}", Special.None, TRUE},
    TT{"OL", "\\begin{enumerate}", "\\end{enumerate}", Special.None, TRUE},
    TT{"DIR", "\\begin{itemize}", "\\end{itemize}", Special.None, TRUE},
    TT{"MENU", "\\begin{itemize}", "\\end{itemize}", Special.None, TRUE},
    TT{"FONT", "", "}", Special.Font, TRUE}, (* *)
    TT{"BASEFONT", "", "}", Special.BaseFont, TRUE}, (* *)
    TT{"BR", "\\\\", "", Special.None, TRUE},
    TT{"DL", "\\begin{description}", "\\end{description}", Special.None, TRUE},
    TT{"DIV", "", "", Special.Div, TRUE},
    TT{"SPAN", "", "", Special.Span, TRUE},
    TT{"BLOCKQUOTE", "\\begin{quote}", "\\end{quote}", Special.None, TRUE},
    TT{"TABLE", "\\begin{tabular}", "\\end{tabular}", Special.None, TRUE},
    TT{"ADDRESS", "", "", Special.Address, TRUE},
    TT{"LINK", "", "", Special.None, TRUE},
    TT{"PLAINTEXT", "\\begin{verbatim}", "\\end{verbatim}", Special.None, 
        TRUE},
    TT{"DT", "\\item[", "]", Special.None, TRUE},
    TT{"DD", "", "", Special.None, TRUE},
    TT{"LI", "\\item ", "", Special.None, TRUE},
    TT{"TR", "", "", Special.None, TRUE}, (* *)
    TT{"TH", "", "", Special.None, TRUE}, (* *)
    TT{"TD", "", "", Special.None, TRUE}, (* *)
    TT{"CAPTION", "\\caption{", "}", Special.None, TRUE},
    TT{"FIGURE", "\\begin{figure}", "\\end{figure}", Special.Figure, TRUE},
    TT{"RECTANGLE", "", "", Special.Rectangle, TRUE},
    TT{"CIRCLE", "", "", Special.Circle, TRUE},
    TT{"ELLIPSE", "", "", Special.Ellipse, TRUE},
    TT{"POLYLINE", "", "", Special.Polyline, TRUE},
    TT{"SPLINE", "", "", Special.Spline, TRUE},
    TT{"PICTURE", "", "", Special.Picture, TRUE},
    TT{"ARC", "", "", Special.Arc, TRUE},
    TT{"GTEXT", "", "", Special.GText, TRUE},
    TT{"GGROUP", "", "", Special.GGroup, TRUE},
    TT{"HEAD", "", "", Special.None, TRUE},
    TT{"BODY", "", "", Special.None, TRUE},
    TT{"ISINDEX", "", "", Special.None, TRUE},
    TT{"BASE", "", "", Special.None, TRUE},
    TT{"META", "", "", Special.None, TRUE},
    TT{"STYLE", "", "", Special.None, TRUE},
    TT{"SCRIPT", "", "", Special.None, TRUE},
    TT{"DIV.APPENDIX", "", "", Special.Appendix, TRUE},
    TT{"DIV.PREFACE", "", "", Special.Preface, TRUE},
    TT{"DIV.ABSTRACT", "\\begin{abstract}", "\\end{abstract}", Special.None, 
        TRUE},
    TT{"DIV.HEAD", "", "", Special.Head, FALSE},
    TT{"DIV.REFERENCES", "\\begin{thebibliography}{10}", 
        "\\end{thebibliography}", Special.None, TRUE},
    TT{"DIV.BIB.ARTICLE", "", ".", Special.Article, FALSE},
    TT{"DIV.BIB.BOOK", "", ".", Special.Report, FALSE},
    TT{"DIV.BIB.BOOKLET", "", ".", Special.Booklet, FALSE},
    TT{"DIV.BIB.INBOOK", "", ".", Special.InBook, FALSE},
    TT{"DIV.BIB.INCOLLECTION", "", ".", Special.InCollection, FALSE},
    TT{"DIV.BIB.INPROCEEDINGS", "", ".", Special.InProceedings, FALSE},
    TT{"DIV.BIB.MANUAL", "", ".", Special.Manual, FALSE},
    TT{"DIV.BIB.MSTHESIS", "", ".", Special.MSThesis, FALSE},
    TT{"DIV.BIB.PHDTHESIS", "", ".", Special.PHDThesis, FALSE},
    TT{"DIV.BIB.MISCENTRY", "", ".", Special.MiscEntry, FALSE},
    TT{"DIV.BIB.PROCEEDINGS", "", ".", Special.Proceedings, FALSE},
    TT{"DIV.BIB.TECHREPORT", "", ".", Special.TechReport, FALSE},
    TT{"DIV.BIB.UNPUBLISHED", "", ".", Special.Unpublished, FALSE},
    TT{"DIV.m3tosgml.startProg", "", "", Special.None, TRUE},
    TT{"DIV.m3tosgml.comment2", "", "", Special.None, TRUE},
    TT{"SPAN.INDEX.MARK.BEGIN", "\\index{", "|(}", Special.IndexMark, FALSE},
    TT{"SPAN.INDEX.MARK.END", "\\index{", "|)}", Special.IndexMark, FALSE},
    TT{"SPAN.INDEX.MARK", "\\index{", "}", Special.IndexMark, FALSE},
    TT{"SPAN.INDEX.KEY", "", "", Special.IndexKey, TRUE},
    TT{"SPAN.INDEX.TEXT", "@", "", Special.None, TRUE},
    TT{"SPAN.INDEX.SEE", "", "", Special.IndexSee, TRUE},
    TT{"SPAN.HEAD.AUTHOR", "", "", Special.HeadAuthor, TRUE},
    TT{"SPAN.HEAD.DATE", "}\\date{", "}", Special.None, TRUE},
    TT{"SPAN.HEAD.COPYRIGHT", "\\\\\n\\copyright ", "", Special.None, TRUE},
    TT{"SPAN.HEAD.KEYWORD", "", "", Special.Skip, TRUE},
    TT{"SPAN.AUTHOR", "", "", Special.Author, TRUE},
    TT{"SPAN.DATE", ", ", "", Special.None, TRUE},
    TT{"SPAN.TITLE", ", ", "", Special.Title, TRUE},
    TT{"SPAN.JOURNAL", ", {\\it ", "\\/}", Special.None, TRUE},
    TT{"SPAN.YEAR", ", ", "", Special.None, TRUE},
    TT{"SPAN.VOLUME", ", ", "", Special.None, TRUE},
    TT{"SPAN.NUMBER", ", ", "", Special.None, TRUE},
    TT{"SPAN.PAGES", ", ", "", Special.None, TRUE},
    TT{"SPAN.MONTH", ", ", "", Special.None, TRUE},
    TT{"SPAN.URL", ", (", ")", Special.None, TRUE},
    TT{"SPAN.NOTE", "", "", Special.Skip, TRUE},
    TT{"SPAN.EDITOR", "", "", Special.Author, TRUE},
    TT{"SPAN.PUBLISHER", ", ", "", Special.None, TRUE},
    TT{"SPAN.SERIES", ", ", "", Special.None, TRUE},
    TT{"SPAN.ADDRESS", ", ", "", Special.None, TRUE},
    TT{"SPAN.EDITION", ", ", "", Special.None, TRUE},
    TT{"SPAN.HOWPUBLISHED", ", ", "", Special.None, TRUE},
    TT{"SPAN.CHAPTER", ", ", "", Special.None, TRUE},
    TT{"SPAN.TYPE", ", ", "", Special.None, TRUE},
    TT{"SPAN.BOOKTITLE", ", {\\it ", "\\/}", Special.None, TRUE},
    TT{"SPAN.ORGANIZATION", ", ", "", Special.None, TRUE},
    TT{"SPAN.SCHOOL", ", ", "", Special.None, TRUE},
    TT{"SPAN.INSTITUTION", ", ", "", Special.None, TRUE},
    TT{"MATH", "\\begin{math}", "\\end{math}", Special.Math, FALSE},
    TT{"MATHDISP", "\\begin{displaymath}", "\\end{displaymath}", 
        Special.Math, FALSE},
    TT{"MSTYLE", "{", "}", Special.Unimplemented, FALSE},
    TT{"MERROR", "{", "}", Special.Unimplemented, FALSE},
    TT{"MPHANTOM", "{", "}", Special.Unimplemented, FALSE},
    TT{"MROW", "{", "}", Special.None, FALSE},
    TT{"MFRACT", "\\fract", "", Special.MathArgs, FALSE},
    TT{"MSQRT", "\\sqrt", "", Special.MathArgs, FALSE},
    TT{"MROOT", "\\mroot", "", Special.MathArgs, FALSE},
    TT{"MSUB", "\\msub", "", Special.MathArgs, FALSE},
    TT{"MSUP", "\\msup", "", Special.MathArgs, FALSE},
    TT{"MSUPER", "{", "}", Special.Unimplemented, FALSE},
    TT{"MSUBSUP", "\\msubsup", "", Special.MathArgs, FALSE},
    TT{"MMULTISCRIPTS", "\\mmultiscripts", "", Special.MathArgs, FALSE},
    TT{"MPRESCRIPTS", "{", "}", Special.Unimplemented, FALSE},
    TT{"MUNDER", "\\munder", "", Special.MathArgs, FALSE},
    TT{"MOVER", "\\mover", "", Special.MathArgs, FALSE},
    TT{"MUNDEROVER", "\\munderover", "", Special.MathArgs, FALSE},
    TT{"MTABLE", "{", "}", Special.Unimplemented, FALSE},
    TT{"MTR", "{", "}", Special.Unimplemented, FALSE},
    TT{"MTD", "{", "}", Special.Unimplemented, FALSE},
    TT{"MACTION", "{", "}", Special.Unimplemented, FALSE},
    TT{"EXPR", "{", "}", Special.None, FALSE},
    TT{"FN", "{", "}", Special.None, TRUE},
    TT{"E", "{", "}", Special.None, FALSE},
    TT{"SEP", ", ", "", Special.None, TRUE},
    TT{"ST", " | ", "", Special.None, TRUE},
    TT{"MI", "{", "}", Special.None, TRUE},
    TT{"MN", "{", "}", Special.None, TRUE},
    TT{"MO", "", "", Special.None, TRUE},
    TT{"MF", "", "", Special.None, TRUE},
    TT{"MTEXT", "{", "}", Special.None, TRUE},
    TT{"MS", "{", "}", Special.None, TRUE},
    TT{"MSPACE", "\\ ", "", Special.None, TRUE},
    TT{"NONE", "", "", Special.None, TRUE},
    TT{"EQ", "=", "", Special.None, FALSE},
    TT{"NEQ", "\\noteq ", "", Special.None, FALSE},
    TT{"GT", ">", "", Special.None, FALSE},
    TT{"LT", "<", "", Special.None, FALSE},
    TT{"GEQ", "\\geq ", "", Special.None, FALSE},
    TT{"LEQ", "\\leq ", "", Special.None, FALSE},
    TT{"IN", "\\in ", "", Special.None, FALSE},
    TT{"NOTIN", "\\notin ", "", Special.None, FALSE},
    TT{"SUBSET", "\\subseteq ", "", Special.None, FALSE},
    TT{"NOTSUBSET", "\\notsubseteq ", "", Special.None, FALSE},
    TT{"PRSUBSET", "\\subset ", "", Special.None, FALSE},
    TT{"NOTPRSUBSET", "\\notsubset ", "", Special.None, FALSE},
    TT{"MINUS", "-", "", Special.None, FALSE},
    TT{"PLUS", "+", "", Special.None, FALSE},
    TT{"TIMES", "\\times ", "", Special.None, FALSE},
    TT{"OVER", "\\over ", "", Special.None, FALSE},
    TT{"EXP", "\\exp ", "", Special.None, FALSE},
    TT{"POWER", "{", "}", Special.Unimplemented, FALSE},
    TT{"UNION", "\\bigcup ", "", Special.None, FALSE},
    TT{"INTERSECT", "\\bigcap ", "", Special.None, FALSE},
    TT{"TENDSTO", "\\longrightarrow ", "", Special.None, FALSE},
    TT{"APPLY", "{", "}", Special.Unimplemented, FALSE},
    (* TT{"DIV", "\\div ", "", Special.None, FALSE}, Already defined *)
    TT{"REM", "{", "}", Special.Unimplemented, FALSE},
    TT{"INT", "\\mint", "", Special.MathArgs, FALSE},
    TT{"PARTDIFF", "\\partial {", "}", Special.None, FALSE},
    TT{"TOTALDIFF", "{", "}", Special.Unimplemented, FALSE},
    TT{"DIFF", "{", "}", Special.Unimplemented, FALSE},
    TT{"INVERSE", "{", "}", Special.Unimplemented, FALSE},
    TT{"MATRIXINVERSE", "{", "}", Special.Unimplemented, FALSE},
    TT{"DETERMINANT", "{", "}", Special.Unimplemented, FALSE},
    TT{"SUM", "{", "}", Special.Unimplemented, FALSE},
    TT{"PRODUCT", "{", "}", Special.Unimplemented, FALSE},
    TT{"LIMIT", "\\lim {", "}", Special.None, FALSE},
    TT{"SIN", "\\sin {", "}", Special.None, FALSE},
    TT{"COS", "\\cos {", "}", Special.None, FALSE},
    TT{"TAN", "\\tan {", "}", Special.None, FALSE},
    TT{"SEC", "\\sec {", "}", Special.None, FALSE},
    TT{"COSEC", "\\csc {", "}", Special.None, FALSE},
    TT{"COT", "\\cot {", "}", Special.None, FALSE},
    TT{"SINH", "\\sinh {", "}", Special.None, FALSE},
    TT{"COSH", "\\cosh {", "}", Special.None, FALSE},
    TT{"TANH", "\\tanh {", "}", Special.None, FALSE},
    TT{"SECH", "\\sech {", "}", Special.None, FALSE},
    TT{"COSECH", "\\cosech {", "}", Special.None, FALSE},
    TT{"COTANH", "\\coth {", "}", Special.None, FALSE},
    TT{"ARCSIN", "\\arcsin {", "}", Special.None, FALSE},
    TT{"ARCCOS", "\\arccos {", "}", Special.None, FALSE},
    TT{"ARCTAN", "\\arctan {", "}", Special.None, FALSE},
    TT{"MATRIX", "{", "}", Special.Unimplemented, FALSE},
    TT{"MATRIXROW", "{", "}", Special.Unimplemented, FALSE},
    TT{"VECTOR", "{", "}", Special.Unimplemented, FALSE},
    TT{"INTERVAL", "[", "]", Special.None, FALSE},
    TT{"SET", "\\{", "\\}", Special.None, FALSE},
    TT{"MOMENT", "{", "}", Special.Unimplemented, FALSE},
    TT{"MEDIAN", "{", "}", Special.Unimplemented, FALSE},
    TT{"MODE", "{", "}", Special.Unimplemented, FALSE},
    TT{"MEAN", "{", "}", Special.Unimplemented, FALSE},
    TT{"SDIV", "{", "}", Special.Unimplemented, FALSE},
    (* TT{"VAR", "{", "}", Special.None, FALSE}, Already defined *)
    TT{"LOG", "\\log {", "}", Special.None, FALSE},
    TT{"LN", "\\ln {", "}", Special.None, FALSE},
    TT{"MIN", "\\min {", "}", Special.None, FALSE},
    TT{"MAX", "\\max {", "}", Special.None, FALSE},
    TT{"FACTORIAL", "{", "}", Special.Unimplemented, FALSE},
    TT{"SEMANTICS", "{", "}", Special.Unimplemented, FALSE},
    TT{"ANNOTATION", "{", "}", Special.Unimplemented, FALSE},
    TT{"LOWLIMIT", "{", "}", Special.Unimplemented, FALSE},
    TT{"UPLIMIT", "{", "}", Special.Unimplemented, FALSE},
    TT{"BVAR", "{", "}", Special.Unimplemented, FALSE},
    TT{"DEGREE", "\\deg {", "}", Special.None, FALSE}
  };

PROCEDURE DataF(self: Filter; READONLY e: SGML.DataEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.data(self,e); END;
  END DataF;

PROCEDURE SDataF(self: Filter; READONLY e: SGML.SdataEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.sdata(self,e); END;
  END SDataF;

PROCEDURE PiF(self: Filter; READONLY e: SGML.PiEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.pi(self,e); END;
  END PiF;

PROCEDURE ExternalDataEntityRefF(self: Filter; 
    READONLY e: SGML.ExternalDataEntityRefEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.externalDataEntityRef(self,e); END;
  END ExternalDataEntityRefF;

PROCEDURE SubdocEntityRefF(self: Filter; 
    READONLY e: SGML.SubdocEntityRefEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.subdocEntityRef(self,e); END;
  END SubdocEntityRefF;

PROCEDURE NonSgmlCharF(self: Filter; READONLY e: SGML.NonSgmlCharEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.nonSgmlChar(self,e); END;
  END NonSgmlCharF;

PROCEDURE CommentDeclF(self: Filter; READONLY e: SGML.CommentDeclEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.commentDecl(self,e); END;
  END CommentDeclF;

PROCEDURE MarkedSectionStartF(self: Filter; 
    READONLY e: SGML.MarkedSectionStartEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.markedSectionStart(self,e); END;
  END MarkedSectionStartF;

PROCEDURE MarkedSectionEndF(self: Filter; 
    READONLY e: SGML.MarkedSectionEndEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.markedSectionEnd(self,e); END;
  END MarkedSectionEndF;

PROCEDURE IgnoredCharsF(self: Filter; READONLY e: SGML.IgnoredCharsEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.ignoredChars(self,e); END;
  END IgnoredCharsF;

PROCEDURE GeneralEntityF(self: Filter; READONLY e: SGML.GeneralEntityEvent) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.generalEntity(self,e); END;
  END GeneralEntityF;

PROCEDURE OpenEntityChangeF(self: Filter) =
  BEGIN
    IF NOT self.skip THEN SGMLPrint.T.openEntityChange(self); END;
  END OpenEntityChangeF;

PROCEDURE InitF(self: Filter): Filter =
  BEGIN
    EVAL SGMLPrint.T.init(self);
    self.tags := NEW(TextIntTbl.Default).init();
    self.tagStack := NEW(IntSeq.T).init();
    self.tagStack.addhi(0); (* In case we receive some data outside any tags *)
    FOR i := 0 TO LAST(HTMLtoHTMLtbl) DO
      IF self.tags.put(HTMLtoHTMLtbl[i].tag,i) THEN
        Wr.PutText(Stdio.stderr,"Duplicate tag " & HTMLtoHTMLtbl[i].tag & 
            " in translation table\n");
      END;
    END;
    RETURN self;
  END InitF;

PROCEDURE StartElementF(self: HTMLtoHTML; 
    READONLY e: SGML.StartElementEvent)=
  VAR
    name, t: TEXT;
    found: BOOLEAN;
    action: SpecialF;
    n: INTEGER;
  BEGIN
    IF verbose >= 2 THEN
      WITH position = self.getDetailedLocation(e.pos) DO
        Wr.PutText(Stdio.stdout,"In file " & position.filename & " line " & 
            Fmt.Int(position.lineNumber) & " column " & 
            Fmt.Int(position.columnNumber) & "\n");
      END;
    END;

    INC(self.depth);

    name := e.gi;
    found := self.tags.get(name,n);
    IF found THEN
      CASE HTMLtoHTMLtbl[n].action OF
      | SpecialF.Span, SpecialF.Div =>
          IF GetAttributeValue(e.attributes,"CLASS",t) THEN
            found := FALSE;
            IF self.inHead THEN
              name := e.gi & ".HEAD." & t;
              found := self.tags.get(name,n);
            END;
            IF NOT found THEN
              name := e.gi & "." & t;
              found := self.tags.get(name,n);
            END;
          END;
      ELSE
      END;
    END;

    IF NOT found THEN n := 0; END;
    self.tagStack.addhi(n);

    IF self.skip THEN RETURN; END;

    IF NOT found THEN    
      SGMLPrint.T.startElement(self,e);
    ELSE
      Wr.PutText(self.wr,HTMLtoHTMLtbl[n].start);
      action := HTMLtoHTMLtbl[n].action;

      CASE action OF
      | SpecialF.None =>
      | SpecialF.Default, SpecialF.Div, SpecialF.Span => 
          SGMLPrint.T.startElement(self,e);
      | SpecialF.Head =>
          self.inHead := TRUE;
          SGMLPrint.T.startElement(self,e);
      | SpecialF.Skip =>
          self.skip := TRUE;
          self.skipDepth := self.depth;
      | SpecialF.Link =>
          AdjustHref(self,e);
          SGMLPrint.T.startElement(self,e);
      END;
    END;
  END StartElementF;

PROCEDURE EndElementF(self: Filter; READONLY e: SGML.EndElementEvent) =
  VAR
    n: INTEGER;
  BEGIN
    n := self.tagStack.remhi();

    IF NOT self.skip THEN
      CASE HTMLtoHTMLtbl[n].action OF
      | SpecialF.None, SpecialF.Skip =>
      | SpecialF.Default, SpecialF.Div, SpecialF.Span, SpecialF.Link =>
          SGMLPrint.T.endElement(self,e);
      | SpecialF.Head =>
          self.inHead := FALSE;
          SGMLPrint.T.endElement(self,e);
      END;
      Wr.PutText(self.wr,HTMLtoHTMLtbl[n].end);
    END;

    IF self.skip AND self.skipDepth = self.depth THEN self.skip := FALSE; END;

    DEC(self.depth);
  END EndElementF;

(* Three adjustments are required for hypertext references: links to other
   packages must account for the hierarchy being flattened upon installation,
   some files are translated upon translation and their extension changes
   accordingly, and links to directories may need to point to the index.html
   file within. The updated value of the hypertext reference is changed
   in place within "e". *)

VAR error: TEXT; (* The optimizer does not see exceptions and tries to
                    be smart with local variables... *)

PROCEDURE AdjustHref(self: HTMLtoHTML; READONLY e: SGML.StartElementEvent) =
  VAR
    href, newref: TEXT;
    hrefLen, extLen: CARDINAL;
    pathseq, hrefseq, newseq: TextSeq.T;
    pkgNamePos, dashPos := -1;
    status: File.Status;
  BEGIN
    IF NOT GetAttributeValue(e.attributes,"HREF",href) THEN RETURN; END;

    IF Text.FindChar(href,'#') = 0 THEN RETURN; END; (* internal link *)

    (* Local links (without a ':') are checked to see if they are valid and 
       if they refer to a directory. *)

    IF Text.FindChar(href,':') < 0 THEN
      TRY
        (* Local links must be relative *)

        error := "incorrect HREF (non relative link) ";
        IF Pathname.Absolute(href) THEN RAISE OSError.E(NIL); END;

        (* Extract the file name (up to '#') and check if it exists relative
           to the prefix of the input file name (inPrefix). *)

        error := "incorrect HREF ";

        dashPos := Text.FindChar(href,'#');
        IF dashPos >= 0 THEN
          status :=FS.Status(Pathname.Join(inPrefix,Text.Sub(href,0,dashPos)));
        ELSE
          status := FS.Status(Pathname.Join(inPrefix,href));
        END;

        (* The file exists but is a directory. In that case, a tag address
           ('#') is not acceptable. Perhaps the directory contains an
           index.html file to which we should point. *)

        IF status.type = FS.DirectoryFileType THEN
          IF dashPos >= 0 THEN RAISE OSError.E(NIL); END;

          newref := Pathname.Join(href,"index","html");
          error := NIL;
          status := FS.Status(Pathname.Join(inPrefix,newref));

          (* If there is an index.html, check that it is a regular file *)

          error := "invalid HREF (not a regular file) ";
          IF status.type # RegularFile.FileType THEN RAISE OSError.E(NIL);END;
          href := newref;
        END;
      EXCEPT
      | OSError.E =>
          IF error # NIL THEN
            self.error(SGML.ErrorEvent{0,SGML.ErrorType.Warning,error
                & href});
          END;
      END;
    END;

    (* The package hierarchy gets flattened in the installation. Links
       must be redirected appropriately when the -path option is set and
       we have a relative link which points outside of the current package. *)

    IF path # NIL AND
        (Text.Length(href) >= Text.Length(Pathname.Parent)) AND
        Text.Equal(Text.Sub(href,0,Text.Length(Pathname.Parent)),
        Pathname.Parent) THEN
      TRY

        pathseq := Pathname.Decompose(path);
        hrefseq := Pathname.Decompose(href);
        newseq := NEW(TextSeq.T).init();

        (* The href starts by .. does it go beyond the current package
           (enough ../.. to get out of src arcs to go into something else)? *)

        IF hrefseq.size() <= pathseq.size() THEN newseq := NIL; END;

        (* Check that there are as many Pathname.Parent as the depth of
           the file in the current package (i.e. from "src/sub" have
           "../../../sgmlconv/src/format.html"). This means that the relative 
           link indeed points out of "src" to another package. Thus,
           "../.." is retained to get out of "src/sub". One more ".." is
           added to get out of the current package. *)

        IF newseq # NIL THEN
          newseq.addhi(hrefseq.get(0)); (* initial NIL arc *)
          newseq.addhi(Pathname.Parent); (* get out of the package dir *)
          FOR i := 1 TO pathseq.size() - 1 DO
            IF NOT Text.Equal(hrefseq.get(i),Pathname.Parent) THEN
              newseq := NIL;
              EXIT;
            END;
            newseq.addhi(Pathname.Parent);
          END;
        END;

        (* Find the position where we enter into the other package,
           i.e. one before the "src" directory. Add the path component
           required to get into the other package. We are thus removing
           everything between getting out of the current package and
           getting in the other package, which is the package hiererchy
           used in the source tree but not present in the install tree. *)

        IF newseq # NIL THEN
          FOR i := hrefseq.size() - 1 TO 1 BY -1 DO
            IF Text.Equal(hrefseq.get(i),"src") THEN
              pkgNamePos := i - 1;
              EXIT;
            END;
          END;

          (* Now that the path out of the current package is in newseq,
             add the path to get into the other package to which points
             href. *)

          IF pkgNamePos >= 0 THEN
            FOR i := pkgNamePos TO hrefseq.size() - 1 DO
              newseq.addhi(hrefseq.get(i));
            END;
          ELSE
            newseq := NIL;
          END;
        END;
        IF newseq # NIL THEN href := Pathname.Compose(newseq); END;
      EXCEPT ELSE END;
    END;

    (* Check for extensions corresponding to translated files. Replace
       the extension with the extension after translation, as specified
       in the command line replacement "patterns". *)

    hrefLen := Text.Length(href);
    FOR i := 0 TO LAST(patterns^) DO
      extLen := Text.Length(patterns[i].from);
      IF hrefLen >= extLen AND Text.Equal(Text.Sub(href,
          hrefLen - extLen,extLen),patterns[i].from) THEN
        href := Text.Sub(href, 0, hrefLen - extLen) & patterns[i].to;
        EXIT;
      END;
    END;

    SetAttributeValue(e.attributes,"HREF",href);
  END AdjustHref;

TYPE
  SpecialF = {None, Default, Div, Span, Head, Skip, Link};

  TagFilter = RECORD
      tag, start, end: TEXT;
      action: SpecialF;
    END;

  TF = TagFilter;

  FilterTable = ARRAY OF TF;

CONST
  HTMLtoHTMLtbl = FilterTable{
    TF{"", "", "", SpecialF.Default},
    TF{"A", "", "", SpecialF.Link}, 
    TF{"DIV", "", "", SpecialF.Div},
    TF{"SPAN", "", "", SpecialF.Span},
    TF{"DIV.HEAD", "", "", SpecialF.Head},
    TF{"DIV.ABSTRACT", "<DIV CLASS=ABSTRACT><H2>Abstract</H2>", 
        "</DIV>", SpecialF.None},
    TF{"DIV.REFERENCES", "<DIV CLASS=REFERENCES><H2>References</H2>", 
        "</DIV>", SpecialF.None},
    TF{"SPAN.INDEX.MARK.BEGIN", "", "", SpecialF.Skip},
    TF{"SPAN.INDEX.MARK.END", "", "", SpecialF.Skip},
    TF{"SPAN.INDEX.MARK", "", "", SpecialF.Skip},
    TF{"SPAN.INDEX.KEY", "", "", SpecialF.Skip},
    TF{"SPAN.INDEX.TEXT", "", "", SpecialF.Skip},
    TF{"SPAN.INDEX.SEE", "", "", SpecialF.Skip},
    TF{"SPAN.HEAD.AUTHOR", "author: ", "<BR>", SpecialF.Default},
    TF{"SPAN.HEAD.DATE", "date: ", "<BR>", SpecialF.Default},
    TF{"SPAN.HEAD.COPYRIGHT", "copyright: ", "<BR>", SpecialF.Default},
    TF{"SPAN.HEAD.KEYWORD", "keyword: ", "<BR>", SpecialF.Default},
    TF{"SPAN.AUTHOR", "author: ", "<BR>", SpecialF.Default},
    TF{"SPAN.DATE", "date: ", "<BR>", SpecialF.Default},
    TF{"SPAN.TITLE", "title: ", "<BR>", SpecialF.Default},
    TF{"SPAN.JOURNAL", "journal: ", "<BR>", SpecialF.Default},
    TF{"SPAN.YEAR", "year: ", "<BR>", SpecialF.Default},
    TF{"SPAN.VOLUME", "vol: ", "<BR>", SpecialF.Default},
    TF{"SPAN.NUMBER", "no: ", "<BR>", SpecialF.Default},
    TF{"SPAN.PAGES", "pp: ", "<BR>", SpecialF.Default},
    TF{"SPAN.MONTH", "month: ", "<BR>", SpecialF.Default},
    TF{"SPAN.URL", "url: ", "<BR>", SpecialF.Default},
    TF{"SPAN.NOTE", "note: ", "<BR>", SpecialF.Default},
    TF{"SPAN.EDITOR", "editor: ", "<BR>", SpecialF.Default},
    TF{"SPAN.PUBLISHER", "publisher: ", "<BR>", SpecialF.Default},
    TF{"SPAN.SERIES", "series: ", "<BR>", SpecialF.Default},
    TF{"SPAN.ADDRESS", "address: ", "<BR>", SpecialF.Default},
    TF{"SPAN.EDITION", "edition: ", "<BR>", SpecialF.Default},
    TF{"SPAN.HOWPUBLISHED", "howpublished: ", "<BR>", SpecialF.Default},
    TF{"SPAN.CHAPTER", "chapter: ", "<BR>", SpecialF.Default},
    TF{"SPAN.TYPE", "type: ", "<BR>", SpecialF.Default},
    TF{"SPAN.BOOKTITLE", "boottitle: ", "<DIV>", SpecialF.Default},
    TF{"SPAN.ORGANIZATION", "organization: ", "<DIV>", SpecialF.Default},
    TF{"SPAN.SCHOOL", "school: ", "<DIV>", SpecialF.Default},
    TF{"SPAN.INSTITUTION", "institution: ", "<DIV>", SpecialF.Default}
  };

(* With HTMLCheck, the document is parsed and each hypertext link is checked
   to see if valid. *)

PROCEDURE InitCheck(self: Filter): Filter =
  BEGIN
    EVAL SGMLPrint.T.init(self);
    self.tags := NEW(TextIntTbl.Default).init();
    self.tagStack := NEW(IntSeq.T).init();
    self.tagStack.addhi(0); (* In case we receive some data outside any tags *)
    self.skip := TRUE;
    RETURN self;
  END InitCheck;

PROCEDURE StartElementCheck(self: HTMLCheck; 
    READONLY e: SGML.StartElementEvent)=
  VAR
    t: TEXT;
    dashPos := -1;
    status: File.Status;
  BEGIN
    IF verbose >= 2 THEN
      WITH position = self.getDetailedLocation(e.pos) DO
        Wr.PutText(Stdio.stdout,"In file " & position.filename & " line " & 
            Fmt.Int(position.lineNumber) & " column " & 
            Fmt.Int(position.columnNumber) & "\n");
      END;
    END;

    INC(self.depth);

    (* Do we have an A element, with an HREF attribute, and refering not
       to an internal link (#...) or to a remote link (http:...) *)

    IF Text.Equal(e.gi,"A") AND GetAttributeValue(e.attributes,"HREF",t) AND
        Text.FindChar(t,'#') # 0 AND Text.FindChar(t,':') < 0 THEN
      TRY
        (* Local links must be relative *)

        error := "incorrect HREF (non relative link) ";
        IF Pathname.Absolute(t) THEN RAISE OSError.E(NIL); END;

        (* Extract the file name (up to '#') and check if it exists relative
           to the prefix of the input file name (inPrefix). *)

        error := "incorrect HREF ";

        dashPos := Text.FindChar(t,'#');
        IF dashPos >= 0 THEN
          status :=FS.Status(Pathname.Join(inPrefix,Text.Sub(t,0,dashPos)));
        ELSE
          status := FS.Status(Pathname.Join(inPrefix,t));
        END;

        (* The file exists but is a directory. In that case, a tag address
           ('#') is not acceptable. *)

        IF status.type = FS.DirectoryFileType AND dashPos >= 0 THEN 
          RAISE OSError.E(NIL);
        END;

      EXCEPT
      | OSError.E =>
          IF error # NIL THEN
            self.error(SGML.ErrorEvent{0,SGML.ErrorType.Warning,error
                & t});
          END;
      END;
    END;
  END StartElementCheck;

PROCEDURE EndElementCheck(self: Filter; READONLY e: SGML.EndElementEvent) =
  BEGIN
    DEC(self.depth);
  END EndElementCheck;


(* This starts the common part of the file with the command line options
   processing. *)

EXCEPTION UsageError(TEXT);

(* Arguments not starting with a - are treated as file names. *)

VAR
  currentParam := 1;

PROCEDURE ParseOptions(): BOOLEAN RAISES {UsageError} = 
  VAR
    arg: TEXT;
    file := 0;
  BEGIN
    in := Stdio.stdin;
    inName := "stdin";
    out := Stdio.stdout;
    outName := "stdout";
    path := NIL;

    IF currentParam >= Params.Count THEN RETURN FALSE; END;

    WHILE (currentParam < Params.Count AND file < 2) DO
      arg := Params.Get (currentParam); 
      INC (currentParam);
      IF Text.FindChar(arg,'-') # 0 THEN
        TRY
          IF file = 0 THEN 
            in := FileRd.Open(arg);
            inName := arg;
            inPrefix := Pathname.Prefix(inName);
          ELSIF file = 1 THEN 
            out := FileWr.Open(arg);
            outName := arg;
          ELSE RAISE UsageError("");
          END;
        EXCEPT
        | OSError.E =>
            RAISE UsageError("Unable to open file " & arg);
        END;
        INC(file);
      ELSIF Text.Equal (arg, "-htmltex") THEN 
        conversion := "htmltex";
        options.defaultDoctype := "HTML";
      ELSIF Text.Equal (arg, "-htmlhtml") THEN 
        conversion := "htmlhtml";
        options.defaultDoctype := "HTML";
      ELSIF Text.Equal (arg, "-htmlcheck") THEN 
        conversion := "htmlcheck";
        options.defaultDoctype := "HTML";
      ELSIF Text.Equal (arg, "-report") THEN 
        report := TRUE;
      ELSIF Text.Equal (arg, "-keep") THEN 
        discard := FALSE;
      ELSIF Text.Equal (arg, "-v") THEN 
        options.enableWarning := NEW(REF ARRAY OF TEXT,1);
        options.enableWarning[0] := "all";
        verbose := 1;
      ELSIF Text.Equal (arg, "-vv") THEN 
        options.enableWarning := NEW(REF ARRAY OF TEXT,1);
        options.enableWarning[0] := "all";
        verbose := 2;
      ELSIF Text.Equal (arg, "-r") THEN
        IF currentParam + 1 >= Params.Count THEN 
          RAISE UsageError("Missing argument for -r");
        END;
        arg := Params.Get (currentParam); INC (currentParam);
        replacements.addhi(arg);
        arg := Params.Get (currentParam); INC (currentParam);
        replacements.addhi(arg);
      ELSIF Text.Equal (arg, "-dtd") THEN
        IF currentParam >= Params.Count THEN 
          RAISE UsageError("Missing argument for -dtd");
        END;
        arg := Params.Get (currentParam); INC (currentParam);
        dtdDirs.addhi(arg);
      ELSIF Text.Equal (arg, "-path") THEN
        IF currentParam >= Params.Count THEN 
          RAISE UsageError("Missing argument for -path");
        END;
        path := Params.Get (currentParam); INC (currentParam);
      ELSE
        Wr.PutText(Stdio.stderr,"Unrecognized option " & arg & "\n");
        RAISE UsageError("");
      END;
    END;
    patterns := NEW(REF ARRAY OF Pattern,replacements.size() DIV 2);
    FOR i := 0 TO LAST(patterns^) DO
      patterns[i].from := replacements.get(2 * i);
      patterns[i].to := replacements.get((2 * i) + 1);
    END;

    IF dtdDirs.size() > 0 THEN
      options.addSearchDir := NEW(REF ARRAY OF TEXT,dtdDirs.size());
      FOR i := 0 TO LAST(options.addSearchDir^) DO
        options.addSearchDir[i] := dtdDirs.get(i);
      END;
    END;

    RETURN TRUE;
  END ParseOptions;

TYPE
  Pattern = RECORD
      from, to: TEXT;
    END;

VAR
  verbose := 0;
  replacements := NEW(TextSeq.T).init();
  dtdDirs := NEW(TextSeq.T).init();
  in: Rd.T;
  out: Wr.T;
  inName, outName, inPrefix: TEXT;
  patterns: REF ARRAY OF Pattern;
  options: SGML.ParserOptions;
  firstParser, parser: SGML.Parser := NIL;
  converter: SGML.Application;
  conversion: TEXT;
  files := NEW(REF ARRAY OF TEXT,1);
  rds := NEW(REF ARRAY OF Rd.T,1);
  path: TEXT;
  report := FALSE;
  nbErrors: CARDINAL;
  discard, noErrors := TRUE;

BEGIN
  TRY
    WHILE ParseOptions() DO
      files[0] := inName;
      rds[0] := in;
      IF verbose > 0 THEN 
        Wr.PutText(Stdio.stdout,"sgmlconv: " & inName & " -> " &outName& "\n");
      END;

      IF parser = NIL THEN 
        parser := NEW(SGML.Parser);
        EVAL parser.init(options,Params.Get(0),files,rds);
        firstParser := parser;
      ELSE
        parser := firstParser.newParser(files,rds);
      END;

      IF Text.Equal(conversion,"htmltex") THEN
        converter := NEW(HTMLtoTeX, wr := out).init();
      ELSIF Text.Equal(conversion,"htmlhtml") THEN
        converter := NEW(HTMLtoHTML, wr := out).init();
      ELSIF Text.Equal(conversion,"htmlcheck") THEN
        converter := NEW(HTMLCheck, wr := out).init();
      END;

      nbErrors := 0;
      EVAL parser.run(converter);
      Rd.Close(in);
      Wr.Close(out);

      IF nbErrors # 0 THEN
        noErrors := FALSE;
        IF discard AND out # Stdio.stdout THEN
          FS.DeleteFile(outName);
        END;
      END;
    END;
  EXCEPT
  | UsageError(t) =>
      Wr.PutText(Stdio.stderr,
          t & "\n? usage: sgmlconv [-htmltex|-htmlhtml|-htmlcheck] [-r old new] [-path p]... " & 
          "[-dtd dtdSearchPath] [-report] [-v] [-vv] [-keep] [infile]" &
          " [outfile]\n");
  | Rd.Failure =>
      Wr.PutText(Stdio.stderr,"? Unable to read file\n");
  | OSError.E =>
      Wr.PutText(Stdio.stderr,"Error while processing " & inName & " to " &
          outName & "\n");
  END;
  IF noErrors = FALSE THEN Process.Exit(1); END;
END Main.





