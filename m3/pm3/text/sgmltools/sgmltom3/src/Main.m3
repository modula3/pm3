(*  SGML to m3                                                             *)
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

IMPORT SGML, SGMLPrint, Stdio, Params, Text, Wr, Rd, SGMLtoM3, SGMLtoM3Seq,
    FileRd, FileWr;

IMPORT Thread, OSError;

TYPE
  T = SGMLPrint.T OBJECT
      cur: SGMLtoM3.T;
      stack: SGMLtoM3Seq.T;
      comment, firstComment: BOOLEAN;
    METHODS
      init(): T := Init;
    OVERRIDES
      appInfo := AppInfo;
      startDtd := StartDtd;
      endDtd := EndDtd;
      endProlog := EndProlog;
      startElement := StartElement;
      endElement := EndElement;
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

<*FATAL Thread.Alerted, Wr.Failure, Rd.Failure*>

PROCEDURE AppInfo(<*UNUSED*>self: T; <*UNUSED*>READONLY e: SGML.AppinfoEvent) =
  VAR
  BEGIN
  END AppInfo;

PROCEDURE StartDtd(<*UNUSED*>self: T; 
    <*UNUSED*>READONLY e: SGML.StartDtdEvent) =
  VAR
  BEGIN
  END StartDtd;

PROCEDURE EndDtd(<*UNUSED*>self: T; <*UNUSED*>READONLY e: SGML.EndDtdEvent) =
  VAR
  BEGIN
  END EndDtd;

PROCEDURE EndProlog(<*UNUSED*>self: T; 
    <*UNUSED*>READONLY e: SGML.EndPrologEvent) =
  VAR
  BEGIN
  END EndProlog;

(* XML and HTML may store the attribute value in different fields of the
   structure. *)

PROCEDURE AttValue(READONLY a: SGML.Attribute): TEXT =
  VAR
    result: TEXT;
  BEGIN
    IF a.tokens # NIL THEN RETURN a.tokens; END;
    result := "";
    FOR i := 0 TO LAST(a.cdataChunks^) DO
      result := result & a.cdataChunks[i].data;
    END;
    RETURN result;
  END AttValue;

(* Process the start tag *)

PROCEDURE StartElement(self: T; READONLY e: SGML.StartElementEvent) =
  VAR
    attValue: TEXT;
  BEGIN
    (* keep a stack of the tags we are currently in. *)

    self.stack.addhi(self.cur);
    self.cur.skiptag := FALSE;
    self.cur.endText := NIL;

    (* Check if special processing instructions are attached to this tag in
       a CLASS=m3tosgml.x attribute value. *)

    IF NOT self.cur.skip THEN
      FOR i := 0 TO LAST(e.attributes^) DO
        WITH a = e.attributes[i] DO
          IF Text.Equal(a.name,"CLASS") AND 
              (a.tokens # NIL OR a.cdataChunks # NIL) THEN
            attValue := AttValue(a);

            (* Skip this element and nested elements. *)
            IF Text.Equal(attValue,"m3tosgml.skip") THEN
              self.cur.skip := TRUE;

            (* Ignore this tag, and corresponding end tag. *)
            ELSIF Text.Equal(attValue,"m3tosgml.skiptag") OR
                Text.Equal(attValue,"m3tosgml.comment2") THEN
              self.cur.skiptag := TRUE;

            (* This section was in quote. *)
            ELSIF Text.Equal(attValue,"m3tosgml.quote") THEN
              self.cur.skiptag := TRUE;
              self.cur.endText := "\"";
              Wr.PutText(self.wr,"\"");

            (* This section was in backquote. *)
            ELSIF Text.Equal(attValue,"m3tosgml.bquote") THEN
              self.cur.skiptag := TRUE;
              self.cur.endText := "`";
              Wr.PutText(self.wr,"`");

            (* Program start, thus comment end. *)
            ELSIF Text.Equal(attValue,"m3tosgml.startProg") THEN
              self.cur.skiptag := TRUE;
              self.cur.program := TRUE;
              IF self.comment THEN Wr.PutText(self.wr,"*)"); END;
              self.comment := FALSE;

            (* This line and the following are in display mode. *)
            ELSIF Text.Equal(attValue,"m3tosgml.display") THEN
              self.cur.skiptag := TRUE;
              self.cur.display := TRUE;
              Wr.PutText(self.wr,"|");
            END;
          END;
        END;
      END;
      IF NOT (self.cur.skip OR self.cur.skiptag) THEN
        SGMLPrint.T.startElement(self,e);
      END;
    END;
  END StartElement;

(* Process the end tag. We remembered any special processing required 
   (e.g. skip). *)

PROCEDURE EndElement(self: T; READONLY e: SGML.EndElementEvent) =
  BEGIN
    IF self.cur.endText # NIL THEN
      Wr.PutText(self.wr,self.cur.endText);
    END;

    IF NOT (self.cur.skiptag OR self.cur.skip) THEN
      SGMLPrint.T.endElement(self,e); 
    END;
    self.cur := self.stack.remhi();
  END EndElement;

(* Process the element content. *)

PROCEDURE Data(self: T; READONLY e: SGML.DataEvent) =
  VAR
    c: CHAR;
  BEGIN
    IF NOT self.cur.skip THEN

      (* We left program mode but did not output the comment start yet. *)

      IF (NOT self.cur.program) AND (NOT self.comment) THEN
        Wr.PutText(self.wr,"(*");
        self.comment := TRUE;
      END;

      (* In display mode each line starts with "|". *)

      IF self.cur.display THEN
        FOR i := 0 TO Text.Length(e.data) - 1 DO 
          c := Text.GetChar(e.data,i);
          Wr.PutChar(self.wr,c);
          IF c = '\n' THEN Wr.PutChar(self.wr,'|'); END;
        END;
      ELSIF self.cur.program THEN
        Wr.PutText(self.wr,e.data);
      ELSE
        SGMLPrint.T.data(self,e);
      END;
    END;
  END Data;

(* Special characters within program sections and display were replaced by
   entity references. Bring back the original special character. *)

PROCEDURE SData(self: T; READONLY e: SGML.SdataEvent) =
  BEGIN
    IF NOT self.cur.skip THEN
      IF self.cur.program OR self.cur.display THEN Wr.PutText(self.wr,e.text);
      ELSE SGMLPrint.T.sdata(self,e); 
      END;
    END;
  END SData;

(* We dont expect any SGML processing instruction... or external entity
   references... *)

PROCEDURE Pi(self: T; READONLY e: SGML.PiEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.pi(self,e); END;
  END Pi;

PROCEDURE ExternalDataEntityRef(self: T; 
    READONLY e: SGML.ExternalDataEntityRefEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.externalDataEntityRef(self,e); END;
  END ExternalDataEntityRef;

PROCEDURE SubdocEntityRef(self: T; 
    READONLY e: SGML.SubdocEntityRefEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.subdocEntityRef(self,e); END;
  END SubdocEntityRef;

PROCEDURE NonSgmlChar(self: T; 
    READONLY e: SGML.NonSgmlCharEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.nonSgmlChar(self,e); END;  
  END NonSgmlChar;

(* The first comment is the copyright notice to be put back in its original
   form. *)

PROCEDURE CommentDecl(self: T; READONLY e: SGML.CommentDeclEvent) =
  BEGIN
    IF NOT self.cur.skip THEN 
      IF self.firstComment THEN
        FOR i := 0 TO LAST(e.comments^) DO
          Wr.PutText(self.wr,e.comments[i]);
          IF i < LAST(e.comments^) THEN Wr.PutText(self.wr,e.seps[i]); END;
        END;
        self.firstComment := FALSE;
      ELSE
        SGMLPrint.T.commentDecl(self,e);
      END;
    END;
  END CommentDecl;

(* We dont expect marked sections and ignored characters. *)

PROCEDURE MarkedSectionStart(self: T; 
    READONLY e: SGML.MarkedSectionStartEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.markedSectionStart(self,e); END;
  END MarkedSectionStart;

PROCEDURE MarkedSectionEnd(self: T; READONLY e: SGML.MarkedSectionEndEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.markedSectionEnd(self,e); END;
  END MarkedSectionEnd;

PROCEDURE IgnoredChars(self: T; 
    READONLY e: SGML.IgnoredCharsEvent) =
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.ignoredChars(self,e); END;
  END IgnoredChars;

PROCEDURE GeneralEntity(self: T; 
    READONLY e: SGML.GeneralEntityEvent) =
  VAR
  BEGIN
    IF NOT self.cur.skip THEN SGMLPrint.T.generalEntity(self,e); END;
  END GeneralEntity;

PROCEDURE ErrorProc(self: T; READONLY e: SGML.ErrorEvent) =
  VAR
    saveWr := self.wr;
  BEGIN
    self.wr := Stdio.stderr;
    SGMLPrint.T.error(self,e);
    self.wr := saveWr;
  END ErrorProc;

PROCEDURE OpenEntityChange(self: T) =
  VAR
  BEGIN
    SGMLPrint.T.openEntityChange(self);
  END OpenEntityChange;

PROCEDURE Init(self: T): T =
  VAR
  BEGIN
    self.cur.skip := FALSE;
    self.cur.skiptag := FALSE;
    self.cur.program := FALSE;
    self.cur.endText := NIL;
    self.stack := NEW(SGMLtoM3Seq.T).init();
    self.comment := FALSE;
    self.firstComment := TRUE;
    EVAL SGMLPrint.T.init(self);
    RETURN self;
  END Init;

EXCEPTION UsageError(TEXT);

(* Arguments not starting with a - are treated as file names. *)

PROCEDURE ParseOptions() RAISES {UsageError} = 
  VAR
    i := 1;
    arg: TEXT;
    file := 0;
  BEGIN
    WHILE (i < Params.Count) DO
      arg := Params.Get (i); 
      INC (i);
      IF Text.FindChar(arg,'-') # 0 THEN
        TRY
          IF file = 0 THEN 
            in := FileRd.Open(arg);
            inName := arg;
          ELSIF file = 1 THEN 
            out := FileWr.Open(arg);
            outName := arg;
          ELSE RAISE UsageError("");
          END;
        EXCEPT
        | OSError.E => RAISE UsageError("Unable to open file " & arg);
        END;
        INC(file);
      ELSE
        RAISE UsageError("Unrecognized option " & arg);
      END;
    END;
  END ParseOptions;

VAR
  in := Stdio.stdin;
  out := Stdio.stdout;
  inName := "stdin";
  outName := "stdout";
  options: SGML.ParserOptions;
  parser: SGML.Parser;
  conv: T;
  files := NEW(REF ARRAY OF TEXT,1);
  rds := NEW(REF ARRAY OF Rd.T,1);

(* Parse the command line options and run the SGML parser on the specified
   input file. The parser calls back the "conv" object with the extracted
   SGML tokens and pieces. *)

BEGIN
  TRY
    ParseOptions();
    files[0] := inName;
    rds[0] := in;
    parser := NEW(SGML.Parser);
    EVAL parser.init(options,Params.Get(0),files,rds);
    conv := NEW(T, wr := out).init();
    EVAL parser.run(conv);
    Rd.Close(in);
    Wr.Close(out);
  EXCEPT
  | UsageError(t) =>
      Wr.PutText(Stdio.stderr,
          t & "\n? usage: sgmltom3 [infile] [outfile]\n");
  END;
END Main.
