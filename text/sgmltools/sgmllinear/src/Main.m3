(*  HTML linearizer                                                        *)
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

IMPORT SGML, SGMLPrint, Stdio, Params, Text, Fmt, Wr, Process, TempFiles,
    TextRefTbl, Rd, FileWr, Thread, OSError, FS, ASCII, Pipe, FileRd, File,
    TextConv, TextSeq, TextTextTbl, TextWr, Pathname;

TYPE
  T = SGMLPrint.T OBJECT
      url: TEXT;
      oldWr: Wr.T := NIL;
      appendix := "";
      depthOffset: CARDINAL := 0;
      currentHeaderDepth: CARDINAL := 0;
      skipOther, skipLink, skipUntilBody: BOOLEAN := FALSE;
      skipUntilLabel: TEXT := NIL;
      depth, skipDepth, skipUntilDepth, redirectDepth: INTEGER := 0;
      currentURL, visitedURL: TextRefTbl.Default;
      references: TextTextTbl.Default;
      sortedReferences: TextSeq.T;
    METHODS
      init(file: TEXT; currentURL, visitedURL: TextRefTbl.T; 
          references: TextTextTbl.T; sortedReferences: TextSeq.T): T := Init;
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

  PostT = SGMLPrint.T OBJECT
      visitedURL: TextRefTbl.Default;
    OVERRIDES
      startElement := StartElementFixhref;
      endElement := EndElementPrintRef;
    END;

  LinkRel = {Include, Subdoc, BibRef, BibEntry, BibNoRef};

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>
<*FATAL OSError.E*>

(* For each parsing event received, check if we are currently within an
   element to be skipped before printing its content. *)

PROCEDURE AppInfo(self: T; READONLY e: SGML.AppinfoEvent) =
  VAR
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.appInfo(self,e); END;
  END AppInfo;

PROCEDURE StartDtd(self: T; 
    READONLY e: SGML.StartDtdEvent) =
  VAR
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.startDtd(self,e); END;
  END StartDtd;

PROCEDURE EndDtd(self: T; READONLY e: SGML.EndDtdEvent) =
  VAR
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.endDtd(self,e); END;
  END EndDtd;

PROCEDURE EndProlog(self: T; READONLY e: SGML.EndPrologEvent) =
  VAR
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.endProlog(self,e); END;
  END EndProlog;

PROCEDURE StartElement(self: T; READONLY olde: SGML.StartElementEvent) =
  VAR
    linkRel := LinkRel.BibRef;
    linkDest: TEXT := NIL;
    tmp: REFANY;
    e := olde;
    headerDepth: CARDINAL;
    val: TEXT;
  BEGIN
    (* Compute the current depth, used to know when to stop skipping *)

    INC(self.depth);

    (* Any element with the attribute CLASS=MD.SKIP causes skipping until we
       get back to the same depth level (matching end element). *)

    IF NOT self.skipOther THEN
      FOR i := 0 TO LAST(e.attributes^) DO
        WITH a = e.attributes[i] DO
          IF Text.Equal(a.name,"CLASS") THEN
            val := AttributeValue(a);

            (* Skip everything *)
            IF Text.Equal(val,"MD.SKIPALL") THEN
              self.skipOther := TRUE;
              self.skipLink := TRUE;
              self.skipDepth := self.depth;

            (* Skip but leave a url to it *)
            ELSIF Text.Equal(val,"MD.SKIPREF") THEN
              (* Print a reference to the skipped material *)
              Wr.PutText(self.wr,"<A HREF=\"" & self.url & "\"></A>");
              self.skipOther := TRUE;
              self.skipLink := TRUE;
              self.skipDepth := self.depth;

            (* Skip only this level *)
            ELSIF Text.Equal(val,"MD.SKIP") THEN
              self.skipOther := TRUE;
              self.skipDepth := self.depth;

            (* Accumulate the appendices at the top level (depth offset 0) *)
            ELSIF Text.Equal(val,"APPENDIX") THEN
              IF self.oldWr # NIL THEN
                Warning("Nested DIV CLASS=APPENDIX");
              ELSE
                self.redirectDepth := self.depth;
                self.oldWr := self.wr;
                self.wr := TextWr.New();
                self.depthOffset := 0;
              END;

            (* Skip the abstract in included documents *)
            ELSIF Text.Equal(val,"ABSTRACT") OR 
                Text.Equal(val,"HEAD") THEN
              IF self.skipUntilBody OR self.skipUntilLabel # NIL THEN
                self.skipOther := TRUE;
                self.skipDepth := self.depth;
              END;
            END;
          END;
        END;
      END;
    END;

    (* Adjust the Header level according to the number of nested 
       subdocuments. *)

    IF Text.Length(e.gi) = 2 AND Text.GetChar(e.gi,0) = 'H' THEN
      WITH car = Text.GetChar(e.gi,1) DO
        IF car IN ASCII.Digits THEN
          headerDepth := ORD(car) - ORD('0');
          self.currentHeaderDepth := headerDepth;
          e.gi := "H" & Fmt.Int(headerDepth + self.depthOffset);
        END;
      END;
    END;

    (* Check for A anchor elements *)

    IF (NOT self.skipLink) AND Text.Equal(e.gi,"A") THEN
      FOR i := 0 TO LAST(e.attributes^) DO
        WITH a = e.attributes[i] DO

          (* Element to include in the linearization? *)

          IF Text.Equal(a.name,"REL") THEN
            val := AttributeValue(a);

            IF Text.Equal(val,"SUBDOCUMENT") THEN
              linkRel := LinkRel.Subdoc;
            ELSIF Text.Equal(val,"INCLUDE") THEN
              linkRel := LinkRel.Include;
            ELSIF Text.Equal(val,"BIB.REF") THEN
              linkRel := LinkRel.BibRef;
            ELSIF Text.Equal(val,"BIB.ENTRY") THEN
              linkRel := LinkRel.BibEntry;
            ELSIF Text.Equal(val,"BIB.NOREF") THEN
              linkRel := LinkRel.BibNoRef;
            END;
          ELSIF Text.Equal(a.name,"HREF") THEN
            linkDest := AttributeValue(a);
            TRY
              linkDest := ComposeHref(self.url,linkDest);
            EXCEPT ELSE
              linkDest := AttributeValue(a);
              ErrorProc(self,SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
                  "Error processing reference " & linkDest});
            END;

            (* The HREF field is assumed to start with a "data" cdata chunk.
               If the data starts with # it is an internal link which must
               have the url prepended to preserve unicity once it is merged
               with the content of the other url. *)

            IF Text.GetChar(a.cdataChunks[0].data,0) = '#' THEN
              a.cdataChunks[0].data := "#" & self.url & a.cdataChunks[0].data;
            ELSE
              a.cdataChunks[0].data := linkDest;
            END;

          (* An internal name must have the url prepended to preserve
             unicity within the linearized text. *)

          ELSIF Text.Equal(a.name,"NAME") OR Text.Equal(a.name,"ID") THEN
            a.cdataChunks[0].data := self.url & "#" & a.cdataChunks[0].data;
          END;
        END;
      END;

      (* Include the referenced document *)

      IF linkRel IN SET OF LinkRel{LinkRel.Include, LinkRel.Subdoc, 
          LinkRel.BibEntry} AND linkDest # NIL THEN
        VAR
          subparser: SGML.Parser;
          files := NEW(REF ARRAY OF TEXT,1);
          rds := NEW(REF ARRAY OF Rd.T,1);
          subdoc: T;
          linkDestLen, extLen: CARDINAL;
          filter := FALSE;
          linkFile, linkLabel, reference: TEXT := NIL;
          referenceWr: TextWr.T;
          skipUntilBody := TRUE;
          pos: INTEGER;
        BEGIN
          IF self.currentURL.get(linkDest,tmp) THEN
            Error("Recursive inclusion of " & linkDest & " from " & self.url);
          ELSIF linkRel = LinkRel.BibEntry AND 
              self.references.get(linkDest,reference) THEN
          ELSE
            IF self.visitedURL.get(linkDest,tmp) THEN
              Warning("Repeated inclusion of " & linkDest & " from " & 
                  self.url);
            END;

            pos := Text.FindChar(linkDest,'#');
            IF pos > 0 THEN
              linkLabel := Text.Sub(linkDest,pos + 1);
              linkFile := Text.Sub(linkDest,0,pos);
              skipUntilBody := FALSE;
            ELSE
              linkFile := linkDest;
            END;

            IF Text.GetChar(linkFile,Text.Length(linkFile) - 1) = '/' THEN
              files[0] := linkFile & index;
            ELSE
              files[0] := linkFile;
            END;

            (* Check if a filter is required for this file type. *)

            TRY
              rds[0] := FileRd.Open(files[0]);
              linkDestLen := Text.Length(linkFile);
              FOR i := 0 TO LAST(replace^) DO
                extLen := Text.Length(replace[i].ext);
                IF linkDestLen >= extLen AND Text.Equal(Text.Sub(linkFile,
                    linkDestLen - extLen,extLen),replace[i].ext) THEN
                  rds[0] := DoFilter(replace[i].filter,rds[0]);
                  filter := TRUE;
                  EXIT;
                END;
              END;

              (* Create a parser for the document to include. Record the
                 URL to detect recursive or repeated inclusions. *)

              Wr.PutText(self.wr,"<!-- Opening " & linkDest & " (" & files[0] &
                  ") -->");
              subdoc := NEW(T, wr := self.wr, skipOther := TRUE, 
                  skipLink := TRUE, skipUntilBody := skipUntilBody,
                  skipUntilLabel := linkLabel, 
                  depthOffset := self.depthOffset + self.currentHeaderDepth).
                  init(linkFile,self.currentURL, self.visitedURL,
                      self.references,self.sortedReferences);

              (* A subdocument is assumed to be nested within the current
                 section. Thus, H1 in the subdocument, within a H2 section,
                 would become H2. An included document is assumed to be at
                 the same level as the current section, thus a H1 section
                 within a H2 section becomes a H2 section. *)

              IF linkRel = LinkRel.Include THEN DEC(subdoc.depthOffset); END;

              subparser := parser.newParser(files,rds);

              IF linkRel = LinkRel.BibEntry THEN
                referenceWr := TextWr.New();
                subdoc.wr := referenceWr;
                EVAL subparser.run(subdoc);
                reference := TextWr.ToText(referenceWr);
                EVAL self.references.put(linkDest,reference);
                self.sortedReferences.addhi(reference);
              ELSIF skipUntilBody THEN
                Wr.PutText(self.wr,"<DIV ID=\"" & linkDest & "\">");
                EVAL subparser.run(subdoc);
                Wr.PutText(self.wr,"</DIV>");
              ELSE
                EVAL subparser.run(subdoc);
              END;

              EVAL self.currentURL.delete(linkDest,tmp);
            EXCEPT ELSE
              ErrorProc(self,SGML.ErrorEvent{0,SGML.ErrorType.OtherError,
                  "Error including file " & files[0]});
            END;
          END;
        END;
      END;
    END;

    IF Text.Equal(e.gi,"DIV") THEN
      FOR i := 0 TO LAST(e.attributes^) DO
        WITH a = e.attributes[i] DO
          IF Text.Equal(a.name,"NAME") OR Text.Equal(a.name,"ID") THEN
            val := AttributeValue(a);
            a.cdataChunks[0].data := self.url & "#" & a.cdataChunks[0].data;
            IF self.skipUntilLabel # NIL AND
                Text.Equal(val,self.skipUntilLabel) THEN
              self.skipOther := FALSE;
              self.skipLink := FALSE;
              self.skipUntilDepth := self.depth;
            END;
          END;
        END;
      END;
    END;

    IF NOT self.skipOther THEN SGMLPrint.T.startElement(self,e); END;

    (* Everything until the BODY of an included document should be skipped.
       We insert an anchor at the beginning of each document such that
       links may refer to it when external links are converted to internal
       links. *)

    IF Text.Equal(e.gi,"BODY") THEN
      IF self.skipUntilBody THEN
        self.skipOther := FALSE;
        self.skipLink := FALSE;
      END;
    END;
  END StartElement;

(* Compute the absolute URL for the link from the current url and the 
   possibly relative url for the link. *)

PROCEDURE ComposeHref(base, link: TEXT): TEXT RAISES {Pathname.Invalid} =
  VAR
    linkseq, newseq: TextSeq.T;
    arc: TEXT;
    pos := Text.FindChar(link,':');
  BEGIN
    (* We have a full url like http://xxxxx. We assume that it is canonical
       and no processing is required. Should we use a more complex
       condition like the following... that would tolerate a ':' in a file
       name but would miss things like mailto:joe@m3.org

       (Text.Length(link) > pos + 2) AND 
       (Text.GetChar(link,pos + 1) = '/') AND
       (Text.GetChar(link,pos + 2) = '/') *)

    IF (pos > 0) THEN
      RETURN link;
    END;

    IF NOT Pathname.Absolute(link) THEN
      link := Pathname.Join(Pathname.Prefix(base),link);
    END;

    (* Make the link as canonical as possible *)

    linkseq := Pathname.Decompose(link);
    newseq := NEW(TextSeq.T).init();
    newseq.addhi(linkseq.remlo());
    FOR i := 0 TO linkseq.size() - 1 DO
      arc := linkseq.remlo();

      (* We have xx/yy/../zz, which is equivalent to xx/zz, remove the
         yy and .. parts. *)

      IF Text.Equal(arc,Pathname.Parent) AND
          newseq.size() > 1 AND 
          NOT Text.Equal(newseq.gethi(),Pathname.Parent) THEN
        EVAL newseq.remhi();

      (* We have x/./y, which is equivalent to x/y, remove the . *)

      ELSIF Text.Equal(arc,Pathname.Current) THEN

      (* Nothing special to do, keep as is. *)

      ELSE
        newseq.addhi(arc);
      END;
    END;
    RETURN Pathname.Compose(newseq);
  END ComposeHref;

(* Concatenate the pieces forming an attribute. *)

PROCEDURE AttributeValue(READONLY a: SGML.Attribute): TEXT =
  VAR
    t := "";
  BEGIN
    IF a.tokens # NIL THEN t := a.tokens;
    ELSIF a.cdataChunks # NIL THEN
      FOR i := 0 TO LAST(a.cdataChunks^) DO
        IF a.cdataChunks[i].entityName # NIL THEN
          t := t & "&" & a.cdataChunks[i].entityName & ";";
        ELSIF a.cdataChunks[i].data # NIL THEN
          t := t & a.cdataChunks[i].data;
        ELSE
          t := "&#" & Fmt.Int(ORD(a.cdataChunks[i].nonSgmlChar)) & ";";
        END;
      END;
    ELSE t := "";
    END;
    RETURN t;
  END AttributeValue;

PROCEDURE EndElement(self: T; READONLY olde: SGML.EndElementEvent) =
  VAR
    e := olde;
  BEGIN
    (* The BODY of the included document is finished, skip the rest *)

    IF Text.Equal(e.gi,"BODY") THEN
      IF self.skipUntilBody THEN
        self.skipOther := TRUE;
        self.skipLink := TRUE;
        self.skipDepth := 0;

      (* We are at the end of the BODY of the root document. *)
      ELSIF self.skipUntilLabel = NIL THEN 
        Wr.PutText(self.wr,self.appendix);
        IF self.sortedReferences.size() > 0 THEN
          Wr.PutText(self.wr,"\n<DIV CLASS=REFERENCES>\n");
          FOR i := 0 TO self.sortedReferences.size() - 1 DO
            Wr.PutText(self.wr,self.sortedReferences.get(i));
            Wr.PutText(self.wr,"\n");
          END;
          Wr.PutText(self.wr,"</DIV>\n");
        END;
      END;
    END;

    (* Adjust the section header level *)

    IF Text.Length(e.gi) = 2 AND Text.GetChar(e.gi,0) = 'H' THEN
      WITH car = Text.GetChar(e.gi,1) DO
        IF car IN ASCII.Digits THEN
          e.gi := "H" & Fmt.Int(ORD(car) - ORD('0') + 
              self.depthOffset);
        END;
      END;
    END;

    IF NOT self.skipOther THEN SGMLPrint.T.endElement(self,e); END;

    IF self.skipUntilLabel # NIL AND self.skipUntilDepth = self.depth THEN
      self.skipOther := TRUE;
      self.skipLink := TRUE;
    END;

    IF self.oldWr # NIL AND self.redirectDepth = self.depth THEN
      self.appendix := self.appendix & TextWr.ToText(NARROW(self.wr,TextWr.T));
      self.wr := self.oldWr;
      self.oldWr := NIL;
    END;

    (* The end of the section to skip is reached, stop skipping. *)

    IF self.skipOther AND self.skipDepth = self.depth THEN
      self.skipOther := FALSE;
      self.skipLink := FALSE;
    END;

    DEC(self.depth);
  END EndElement;

PROCEDURE Data(self: T; READONLY e: SGML.DataEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.data(self,e); END;
  END Data;

PROCEDURE SData(self: T; READONLY e: SGML.SdataEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.sdata(self,e); END;
  END SData;

PROCEDURE Pi(self: T; READONLY e: SGML.PiEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.pi(self,e); END;
  END Pi;

PROCEDURE ExternalDataEntityRef(self: T; 
    READONLY e: SGML.ExternalDataEntityRefEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.externalDataEntityRef(self,e); END;
  END ExternalDataEntityRef;

PROCEDURE SubdocEntityRef(self: T; 
    READONLY e: SGML.SubdocEntityRefEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.subdocEntityRef(self,e); END;
  END SubdocEntityRef;

PROCEDURE NonSgmlChar(self: T; 
    READONLY e: SGML.NonSgmlCharEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.nonSgmlChar(self,e); END;  
  END NonSgmlChar;

PROCEDURE CommentDecl(self: T; 
    READONLY e: SGML.CommentDeclEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.commentDecl(self,e); END;
  END CommentDecl;

PROCEDURE MarkedSectionStart(self: T; 
    READONLY e: SGML.MarkedSectionStartEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.markedSectionStart(self,e); END;
  END MarkedSectionStart;

PROCEDURE MarkedSectionEnd(self: T; READONLY e: SGML.MarkedSectionEndEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.markedSectionEnd(self,e); END;
  END MarkedSectionEnd;

PROCEDURE IgnoredChars(self: T; 
    READONLY e: SGML.IgnoredCharsEvent) =
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.ignoredChars(self,e); END;
  END IgnoredChars;

PROCEDURE GeneralEntity(self: T; 
    READONLY e: SGML.GeneralEntityEvent) =
  VAR
  BEGIN
    IF NOT self.skipOther THEN SGMLPrint.T.generalEntity(self,e); END;
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

(* We keep a list of currently visited URL to avoid cycles, and a list of all
   visited URL to determine which external references become internal. *)

PROCEDURE Init(self: T; file: TEXT; currentURL, visitedURL: TextRefTbl.T;
    references: TextTextTbl.T; sortedReferences: TextSeq.T): T =
  VAR
  BEGIN
    self.url := file;
    self.currentURL := currentURL;
    self.visitedURL := visitedURL;
    self.references := references;
    self.sortedReferences := sortedReferences;
    EVAL self.currentURL.put(file,NIL);
    EVAL self.visitedURL.put(file,NIL);
    EVAL SGMLPrint.T.init(self);
    RETURN self;
  END Init;

(* Run a filter asynchronously *)

TYPE
  FilterThread = Thread.Closure OBJECT 
      cmd: TEXT;
      args: REF ARRAY OF TEXT;
      in: Rd.T;
      out: File.T;
    OVERRIDES 
      apply := RunFilter; 
    END;

CONST
  Blank = SET OF CHAR{' '};

PROCEDURE RunFilter(self: FilterThread): REFANY =
  VAR
    hrChild, hwSelf: Pipe.T;
    p: Process.T;
    wr: Wr.T;
    buffer: ARRAY [0..1023] OF CHAR;
    nb: CARDINAL;
    rd := self.in;
  BEGIN
    Pipe.Open(hr := hrChild, hw := hwSelf);
    TRY
      p := Process.Create(self.cmd,self.args^,NIL,NIL,hrChild,self.out,stderr);
      hrChild.close();
      self.out.close();
      wr := NEW(FileWr.T).init(hwSelf);

      (* Feed the filter while the parser reads its output. *)
      LOOP
        nb := Rd.GetSub(rd,buffer);
        IF nb >= NUMBER(buffer) THEN
          Wr.PutString(wr,buffer);
        ELSE
          Wr.PutString(wr,SUBARRAY(buffer,0,nb));
          EXIT;
        END;
      END;
      Wr.Close(wr);
      Rd.Close(rd);
      EVAL Process.Wait(p);
    EXCEPT ELSE
      Wr.PutText(Stdio.stderr,"Error while running filter: " & self.cmd);
    END;
    RETURN NIL;
  END RunFilter;

PROCEDURE DoFilter(filter: TEXT; rd: Rd.T): Rd.T =
  VAR
    cmd: TEXT;
    args: REF ARRAY OF TEXT;
    pieces := NEW(REF ARRAY OF TEXT,TextConv.ExplodedSize(filter,Blank));
    hwChild, hrSelf: Pipe.T;
  BEGIN
    TextConv.Explode(filter,pieces^,Blank);
    cmd := pieces[0];
    args := NEW(REF ARRAY OF TEXT,NUMBER(pieces^) - 1);
    args^ := SUBARRAY(pieces^,1,NUMBER(pieces^) - 1);

    Pipe.Open(hr := hrSelf, hw := hwChild);
    EVAL Thread.Fork(NEW(FilterThread, cmd := cmd, args := args, in := rd,
        out := hwChild));
    RETURN NEW(FileRd.T).init(hrSelf);
  END DoFilter;

(* In a second pass, all external references to documents which were included
   in the linearization are converted to internal references. *)

PROCEDURE StartElementFixhref(self: PostT; READONLY e: SGML.StartElementEvent)=
  VAR
    tmp: REFANY;
    linkDest: TEXT;
    pos: INTEGER;
    suffix := "";
  BEGIN
    IF Text.Equal(e.gi,"A") THEN
      FOR i := 0 TO LAST(e.attributes^) DO
        WITH a = e.attributes[i] DO
          IF Text.Equal(a.name,"HREF") THEN
            linkDest := AttributeValue(a);
            pos := Text.FindChar(linkDest,'#');
            IF pos > 0 THEN 
              suffix := Text.Sub(linkDest,pos);
              linkDest := Text.Sub(linkDest,0,pos);
            END;

            IF pos # 0 AND self.visitedURL.get(linkDest,tmp) THEN
              a.cdataChunks[0].data := "#" & a.cdataChunks[0].data;
            ELSIF Text.Equal(Text.Sub(linkDest,0,oldPrefixLen),oldPrefix) THEN
              linkDest := newPrefix & Text.Sub(linkDest,oldPrefixLen);
              a.cdataChunks := NEW(REF ARRAY OF SGML.CdataChunk,1);
              a.cdataChunks[0].data := linkDest & suffix;
            END;
          END;
        END;
      END;
    END;
    SGMLPrint.T.startElement(self,e);
  END StartElementFixhref;

PROCEDURE EndElementPrintRef(self: PostT; READONLY e: SGML.EndElementEvent) =
  BEGIN
    SGMLPrint.T.endElement(self,e);
  END EndElementPrintRef;

PROCEDURE Error(t: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr,"Error: " & t & "\n");
    Process.Exit(1);
  END Error;

PROCEDURE Warning(t: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr,"Warning: " & t & "\n");
  END Warning;

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
        | OSError.E =>
            RAISE UsageError("Unable to open file " & arg);
        END;
        INC(file);
      ELSIF Text.Equal(arg, "-html") THEN
      ELSIF Text.Equal(arg, "-index") THEN
        IF i >= Params.Count THEN 
          RAISE UsageError("Missing argument for -index");
        END;
        index := Params.Get (i); INC (i);
      ELSIF Text.Equal (arg, "-prefix") THEN
        IF i + 1 >= Params.Count THEN 
          RAISE UsageError("Missing argument for -prefix");
        END;
        oldPrefix := Params.Get (i); INC (i);
        oldPrefixLen := Text.Length(oldPrefix);
        newPrefix := Params.Get (i); INC (i);
      ELSIF Text.Equal (arg, "-f") THEN
        IF i + 1 >= Params.Count THEN 
          RAISE UsageError("Missing argument for -f");
        END;
        arg := Params.Get (i); INC (i);
        replacements.addhi(arg);
        arg := Params.Get (i); INC (i);
        replacements.addhi(arg);
      ELSIF Text.Equal (arg, "-v") THEN
        verbose := NEW(REF ARRAY OF TEXT,1);
        verbose[0] := "all";
      ELSIF Text.Equal (arg, "-dtd") THEN
        IF i >= Params.Count THEN 
          RAISE UsageError("Missing argument for -dtd");
        END;
        arg := Params.Get(i); INC(i);
        dtdDirs.addhi(arg);
      ELSE
        RAISE UsageError("Unrecognized option " & arg);
      END;
    END;

    IF dtdDirs.size() > 0 THEN
      options.addSearchDir := NEW(REF ARRAY OF TEXT,dtdDirs.size());
      FOR i := 0 TO LAST(options.addSearchDir^) DO
        options.addSearchDir[i] := dtdDirs.get(i);
      END;
    END;

    replace := NEW(REF ARRAY OF Filter,replacements.size() DIV 2);
    FOR i := 0 TO LAST(replace^) DO
      replace[i].ext := replacements.remlo();
      replace[i].filter := replacements.remlo();
    END;
    replacements := NIL;
    options.enableWarning := verbose;
    options.defaultDoctype := "HTML";
  END ParseOptions;

TYPE
  Filter = RECORD
      ext, filter: TEXT;
    END;

VAR
  stdin, stdout, stderr: File.T;
  replacements := NEW(TextSeq.T).init();
  dtdDirs := NEW(TextSeq.T).init();
  replace: REF ARRAY OF Filter;
  in := Stdio.stdin;
  inName := "stdin";
  out := Stdio.stdout;
  outName := "stdout";
  options: SGML.ParserOptions;
  parser: SGML.Parser;
  files := NEW(REF ARRAY OF TEXT,1);
  rds := NEW(REF ARRAY OF Rd.T,1);
  tempFile := TempFiles.Get(part := "linear", ext := "sgml");
  wr := FileWr.Open(tempFile);
  linearize: T;
  fixhref: PostT;
  index := "index.html";
  verbose := NEW(REF ARRAY OF TEXT,0);
  oldPrefix := "//";
  newPrefix := "//";
  oldPrefixLen := 2;

BEGIN
  TRY
    Process.GetStandardFileHandles(stdin, stdout, stderr);
    ParseOptions();
    TempFiles.Note(tempFile);

    (* Parse the root file as well as all the referenced files when REL is
       INCLUDE or SUBDOCUMENT. The output is placed in a temporary file. *)

    files[0] := inName;
    rds[0] := in;
    parser := NEW(SGML.Parser);
    EVAL parser.init(options,Params.Get(0),files,rds);
    linearize := NEW(T, wr := wr).init(files[0],NEW(TextRefTbl.Default).init(),
        NEW(TextRefTbl.Default).init(),NEW(TextTextTbl.Default).init(),
        NEW(TextSeq.T).init());
    EVAL parser.run(linearize);
    Rd.Close(in);
    Wr.Close(wr);

    (* Once the complete list of visited files is known, convert external
       references to internal ones as appropriate in the second pass.
       print the output to standard output. *)

    files[0] := tempFile;
    parser := parser.newParser(files);
    fixhref := NEW(PostT, wr := out, 
        visitedURL := linearize.visitedURL).init();
    EVAL parser.run(fixhref);
    Wr.Close(out);
    TempFiles.Forget(tempFile);
    FS.DeleteFile(tempFile);
  EXCEPT
  | UsageError(t) =>
      Wr.PutText(Stdio.stderr,
          t & "\n? usage: sgmllinear [-v] [-html] [-index file] " &
          "[-prefix old new] [-f ext filter]... [infile] [outfile]\n");
  | Rd.Failure =>
      Wr.PutText(Stdio.stderr, "? can't read file\n");
  | Wr.Failure =>
      Wr.PutText(Stdio.stderr, "? can't write file\n");
  END;
END Main.
