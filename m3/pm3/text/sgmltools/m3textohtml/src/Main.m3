(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Modified from m3totex by Michel Dagenais 2 August 1995                    *)

MODULE Main;

IMPORT Stdio, Rd, Wr, Text, Lex, Thread, TextSeq;

IMPORT Params, TextWr, TextRd;

<*FATAL Wr.Failure, Rd.Failure, Rd.EndOfFile, Lex.Error, Thread.Alerted *>

PROCEDURE Trans(rd: Rd.T; wr: Wr.T) =
  VAR
    type: [2..3];
  BEGIN
    TRY
      (* No processing done on the first comment which usually is 
         the copyright and log *)
      AdvanceToBlankLine(rd, wr);

      (* Process the program pieces separated by comments *)
      LOOP 
        type := Prog(rd, wr); 
        Comment(type, rd, wr);
      END
    EXCEPT
      Rd.EndOfFile =>
    END 
  END Trans;

(* Skip while copying everything until the first blank line *)

PROCEDURE AdvanceToBlankLine(rd: Rd.T; wr: Wr.T) =
  VAR blank: BOOLEAN; c: CHAR;
  BEGIN
    TRY
      REPEAT
        blank := TRUE;
        LOOP
          c := Rd.GetChar(rd);
          Wr.PutChar(wr,c);
          IF c = '\n' THEN EXIT END;
          IF c # ' ' THEN blank := FALSE END
        END
      UNTIL blank
    EXCEPT
      Rd.EndOfFile => (*skip*)
    END
  END AdvanceToBlankLine;

(* Skip while copying a program section until a comment starting in first
   column *)

PROCEDURE Prog(rd: Rd.T; wr: Wr.T): [2..3] RAISES {Rd.EndOfFile} =
  VAR 
    c: CHAR;  
    vspace := 1; 
    hspace := 0; 
    startOfLine := TRUE;
  BEGIN
    TRY
      LOOP
        c := Rd.GetChar(rd);
        CASE c OF
        | '\n' => 
            INC(vspace); 
            hspace := 0;
            startOfLine := TRUE;
            Wr.PutChar(wr,c);
        | ' '  => 
            INC(hspace);
            Wr.PutChar(wr,c);

        (* Do not process the stuff between quotes not to get fooled by
           a ( and * within a string *)

        | '"'  => 
            vspace := 0;
            startOfLine := FALSE;
            Wr.PutChar(wr, c);
            ProcessString(rd,wr);

        (* If we do have a comment starting in first column, this ends
           the program section and starts a comment to process. *)
        | '(' =>
            WITH d = Rd.GetChar(rd) DO
              IF d = '*' AND startOfLine AND hspace = 0 THEN
                Wr.PutText(wr,"(*");
                EXIT;
              ELSE
                Rd.UnGetChar(rd);
                Wr.PutChar(wr, c);
                IF d = '*' THEN ProcessComment(rd,wr); END;
                vspace := 0;
                startOfLine := FALSE;
              END
            END 
        ELSE
          (* This is some other non blank character. We are thus not
             in the first column any more (startOfLine). *)
          vspace := 0;
          startOfLine := FALSE;
          Wr.PutChar(wr, c)
        END
      END
    FINALLY
      IF vspace < 2 THEN RETURN 2;
      ELSE RETURN 3;
      END;
    END
  END Prog;

CONST
  Alphabetic = SET OF CHAR{'a' .. 'z', 'A' .. 'Z'};
  AllChar = SET OF CHAR{FIRST(CHAR) .. LAST(CHAR)};
  ArgumentChar = AllChar - SET OF CHAR{'}'};
  ItemArgumentChar = AllChar - SET OF CHAR{']'};
  CommandChar = Alphabetic + SET OF CHAR{'\\'};

(* Process a comment while converting TeX commands to HTML commands.
   Consecutive commants are also merged *)

PROCEDURE Comment(<*UNUSED*>type: [2..3]; rd: Rd.T; wr: Wr.T) 
    RAISES {Rd.EndOfFile} =
  VAR 
    c, d: CHAR; 
    t: TEXT;
    startOfLine := TRUE; 
    inDisplay := FALSE;
    inList := FALSE;
    inItem := FALSE;
    searchItem := FALSE;
    inGroup := FALSE;
    groupCommand := NEW(TextSeq.T).init();
    texCommand, space := "";
    inQuote := FALSE;
    blankBetweenItem := "";
    taggedList, inTable := FALSE;
  BEGIN
    LOOP
      c := Rd.GetChar(rd);
      IF inDisplay AND startOfLine AND (c # '|') THEN
        inDisplay := FALSE;
      END;

      (* This is the end of the comment, return *)
      IF c = '*' THEN
        d := Rd.GetChar(rd);
        IF d = ')' THEN
          Wr.PutText(wr,"*)");
          RETURN;
        ELSE
          Rd.UnGetChar(rd);
          Wr.PutChar(wr, c);
        END;

      (* When in display, do not perform any processing *)
      ELSIF inDisplay THEN Wr.PutChar(wr,c);

      ELSIF c = '<' THEN
        Wr.PutText(wr,"&lt;");

      (* Apparently text in quote does not contain tex commands *)
      ELSIF c = '"' THEN
        Wr.PutChar(wr,c);
        inQuote := NOT inQuote;
      ELSIF inQuote THEN Wr.PutChar(wr,c);

      ELSIF c = '&' THEN
        IF inTable THEN Wr.PutText(wr,"<TD>");
        ELSE Wr.PutText(wr,"&amp;");
        END;

      (* It is a TeX command *)
      ELSIF c = '\\' THEN
        texCommand := Lex.Scan(rd,CommandChar);
        space := Lex.Scan(rd,SET OF CHAR{' '});
        IF Text.Equal(texCommand,"") AND Text.Length(space) = 0 THEN
          d := Rd.GetChar(rd);
          CASE d OF
          | '\\' =>
              IF inTable THEN Wr.PutText(wr,"<TR>");
              ELSE Wr.PutText(wr,"<BR>");
              END;
          | '$', '_', '%', '#' => Wr.PutChar(wr,d);
          | '&' => Wr.PutText(wr,"&amp;");
          | '/' =>
          ELSE
            Wr.PutText(Stdio.stderr,"Unknown character escape sequence \\"
              & Text.FromChar(d) & "\n");
          END;
        ELSIF Text.Equal(texCommand,"medskip\\bulletitem") OR
           Text.Equal(texCommand,"bulletitem") OR
           Text.Equal(texCommand,"medskip\\nobulletitem") OR
           Text.Equal(texCommand,"nobulletitem") THEN
          searchItem := FALSE;

          (* If we were not in a list, output a list header *)
          IF NOT inList THEN
            inList := TRUE;
            Wr.PutText(wr,"<UL>");
          END;

          Wr.PutText(wr,"<LI>");
          inItem := TRUE;
        ELSE

          (* It was not a list item and we were in a list. Time to output
             the list end tag *)
          IF searchItem THEN
            searchItem := FALSE;
            Wr.PutText(wr,"</UL>" & blankBetweenItem);
            inList := FALSE;
          END;

          IF Text.Equal(texCommand,"noindent") THEN (* do nothing *)
          ELSIF Text.Equal(texCommand,"epsilon") THEN
            Wr.PutText(wr,"&epsilon;");
          ELSIF Text.Equal(texCommand,"item") THEN
            IF taggedList THEN
              Wr.PutText(wr,"<DT>" & GetItemArgument(rd) & "<DD>");
            ELSE
              Wr.PutText(wr,"<LI>");
            END;
          ELSIF Text.Equal(texCommand,"index") THEN
            ProcessIndex(rd,wr);
          ELSIF Text.Equal(texCommand,"cite") THEN
            ProcessCite(rd,wr);
          ELSIF Text.Equal(texCommand,"ref") THEN
            ProcessRef(rd,wr,"REF.NUMBER");
          ELSIF Text.Equal(texCommand,"pageref") THEN
            ProcessRef(rd,wr,"REF.PAGE");
          ELSIF Text.Equal(texCommand,"medskip\\noindent") THEN (* nothing *)
          ELSIF Text.Equal(texCommand,"medskip") THEN (* nothing *)
          ELSIF Text.Equal(texCommand,"smallskip") THEN (* nothing *)
          ELSIF Text.Equal(texCommand,"tt") OR
                Text.Equal(texCommand,"protect\\tt") THEN
            Wr.PutText(wr,"<TT>");
            (* remember to go out of EM when the TeX group ends *)
            IF inGroup THEN groupCommand.addhi("</TT>"); END;
          ELSIF Text.Equal(texCommand,"em") THEN
            Wr.PutText(wr,"<EM>");
            (* remember to go out of EM when the TeX group ends *)
            IF inGroup THEN groupCommand.addhi("</EM>"); END;
          ELSIF Text.Equal(texCommand,"it") THEN
            Wr.PutText(wr,"<I>");
            (* remember to go out of I when the TeX group ends *)
            IF inGroup THEN groupCommand.addhi("</I>"); END;
          ELSIF Text.Equal(texCommand,"bf") THEN
            Wr.PutText(wr,"<B>");
            (* remember to go out of B when the TeX group ends *)
            IF inGroup THEN groupCommand.addhi("</B>"); END;
          ELSIF Text.Equal(texCommand,"medskip") THEN
            Wr.PutText(wr,"<P>");
          ELSIF Text.Equal(texCommand,"section") THEN
            Wr.PutText(wr,"<H1> " & GetArgument(rd) & " </H1>");
          ELSIF Text.Equal(texCommand,"subsection") THEN
            Wr.PutText(wr,"<H2> " & GetArgument(rd) & " </H2>");
          ELSIF Text.Equal(texCommand,"subsubsection") THEN
            Wr.PutText(wr,"<H3> " & GetArgument(rd) & " </H3>");
          ELSIF Text.Equal(texCommand,"paragraph") THEN
            Wr.PutText(wr,"<H4> " & GetArgument(rd) & " </H4>");
          ELSIF Text.Equal(texCommand,"char") THEN
            TRY
              Lex.Match(rd,"'");
              t := Lex.Scan(rd,SET OF CHAR{'0'..'9'});
              Wr.PutText(wr,"&#" & t & ";");
            EXCEPT
            ELSE
              Wr.PutText(Stdio.stderr,"Error processing \\char");
            END;
          ELSIF Text.Equal(texCommand,"begin") THEN
            t := GetArgument(rd);
            IF Text.Equal(t,"quote") THEN Wr.PutText(wr,"<BQ>");
            ELSIF Text.Equal(t,"itemize") THEN Wr.PutText(wr,"<UL>");
            ELSIF Text.Equal(t,"enumerate") THEN Wr.PutText(wr,"<OL>");
            ELSIF Text.Equal(t,"description") THEN 
              Wr.PutText(wr,"<DL>");
              taggedList := TRUE;
            ELSIF Text.Equal(t,"tabular") THEN
              t := GetArgument(rd);
              Wr.PutText(wr,"<TABLE><TR><TD>");
              inTable := TRUE;
            ELSE
              texCommand := "begin{" & t & "}";
              Wr.PutText(Stdio.stderr,"Unknown LaTeX command \\" & texCommand &
                  "\n");
              Wr.PutText(wr,"\\" & texCommand);
            END;
          ELSIF Text.Equal(texCommand,"end") THEN
            t := GetArgument(rd);
            IF Text.Equal(t,"quote") THEN Wr.PutText(wr,"</BQ>");
            ELSIF Text.Equal(t,"itemize") THEN Wr.PutText(wr,"</UL>");
            ELSIF Text.Equal(t,"enumerate") THEN Wr.PutText(wr,"</OL>");
            ELSIF Text.Equal(t,"description") THEN 
              Wr.PutText(wr,"</DL>");
              taggedList := FALSE;
            ELSIF Text.Equal(t,"tabular") THEN
              Wr.PutText(wr,"</TABLE>");
              inTable := FALSE;
            ELSE
              texCommand := "begin{" & t & "}";
              Wr.PutText(Stdio.stderr,"Unknown LaTeX command \\" & texCommand &
                  "\n");
              Wr.PutText(wr,"\\" & texCommand);
            END;
          ELSE
            Wr.PutText(Stdio.stderr,"Unknown LaTeX command \\" & texCommand &
                "\n");
            Wr.PutText(wr,"\\" & texCommand);
          END;
        END;
      ELSIF c = '{' THEN
        IF inGroup THEN
          Wr.PutText(Stdio.stderr,"Nested TeX groups not handled\n");
        ELSE
          inGroup := TRUE;
        END;

      (* The group ends, time to come out of modes. Indeed, in {\em toto }
         it is the } that is replaced by </EM> *)

      ELSIF c = '}' THEN
        IF NOT inGroup THEN
          Wr.PutText(Stdio.stderr,"Found } while not in a TeX group\n");
        ELSE
          inGroup := FALSE;
          FOR i := 0 TO groupCommand.size() - 1 DO
            Wr.PutText(wr,groupCommand.remhi());
          END;
        END;
      ELSIF c = '~' THEN
        Wr.PutText(wr,"&nbsp;");
      ELSIF c = '|' THEN
        IF startOfLine AND (NOT inDisplay) THEN
          inDisplay := TRUE;
        END;
        Wr.PutText(wr, "|");
      ELSE
        Wr.PutChar(wr, c);
      END;

      (* detect list ends *)
      IF c = '\n' AND inItem THEN
        blankBetweenItem := Lex.Scan(rd,Lex.Blanks);
        (* it is not a blank line, thus the item is not finished *)
        IF Text.FindChar(blankBetweenItem,'\n') = -1 THEN
          Wr.PutText(wr,blankBetweenItem);
        ELSE
          (* the item is finished, is it followed by another one ? *)
          inItem := FALSE;
          d := Rd.GetChar(rd);
          Rd.UnGetChar(rd);
          (* there may be another item after this one *)
          IF d = '\\' THEN 
            searchItem := TRUE 
          ELSE
            (* last item, output the list end *)
            Wr.PutText(wr,"</UL>" & blankBetweenItem);
            inList := FALSE;
          END;
        END;
      END;

      startOfLine := (c = '\n') OR (startOfLine AND c = ' ');
    END;
  END Comment;

(* Read until the closing double quote, skipping escaped quotes *)

PROCEDURE ProcessString(rd: Rd.T; wr: Wr.T) =
  VAR
    c: CHAR;
  BEGIN
    LOOP
      c := Rd.GetChar(rd);
      Wr.PutChar(wr,c);
      IF c = '"' THEN EXIT; END;

      IF c = '\\' THEN Wr.PutChar(wr,Rd.GetChar(rd)); END;
    END;
  END ProcessString;

(* Skip a short type 3 comment, not containing TeX commands *)

PROCEDURE ProcessComment(rd: Rd.T; wr: Wr.T) =
  VAR
    c: CHAR;
  BEGIN
    LOOP
      c := Rd.GetChar(rd);
      Wr.PutChar(wr,c);
      IF c = '*' THEN
        c := Rd.GetChar(rd);
        IF c = ')' THEN
          Wr.PutChar(wr,c);
          EXIT;
        ELSE
          Rd.UnGetChar(rd);
        END;
      END;
    END;
  END ProcessComment;

CONST
  CiteChar = SET OF CHAR{'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_', '+',
      '*', ':'};

PROCEDURE ProcessCite(rd: Rd.T; wr: Wr.T) =
  VAR
    t := "";
  BEGIN
    EVAL Lex.Scan(rd,Lex.Blanks);
    IF NOT Rd.GetChar(rd) = '{' THEN
      Rd.UnGetChar(rd);
      Wr.PutText(Stdio.stderr,"Missing argument for \\cite\n");
      RETURN;
    END;
    LOOP
      EVAL Lex.Scan(rd,Lex.Blanks);
      t := Lex.Scan(rd,CiteChar);
      IF Text.Length(t) > 0 THEN
        Wr.PutText(wr,"<A REL=BIB.ENTRY HREF=\"" & referenceFile & "#" & t &
            "\"> [" & t & "] </A>");
        Wr.PutText(Stdio.stderr,"Citation to " & t & "\n");
      END;
      EVAL Lex.Scan(rd,Lex.Blanks);
      IF NOT Text.Equal(Lex.Scan(rd,SET OF CHAR{','}),",") THEN EXIT; END;
    END;
    IF NOT Rd.GetChar(rd) = '}' THEN
      Rd.UnGetChar(rd);
      Wr.PutText(Stdio.stderr,"Missing argument for \\cite\n");
      RETURN;
    END;
  END ProcessCite;

PROCEDURE ProcessRef(rd: Rd.T; wr: Wr.T; rel: TEXT) =
  VAR
    t := "";
  BEGIN
    EVAL Lex.Scan(rd,Lex.Blanks);
    IF NOT Rd.GetChar(rd) = '{' THEN
      Rd.UnGetChar(rd);
      Wr.PutText(Stdio.stderr,"Missing argument for \\(page)ref\n");
      RETURN;
    END;

    EVAL Lex.Scan(rd,Lex.Blanks);
    t := Lex.Scan(rd,CiteChar);
    IF Text.Length(t) > 0 THEN
      Wr.PutText(wr,"<A REL=" & rel & " HREF=\"" & t &
          "\"> [" & t & "] </A>");
      Wr.PutText(Stdio.stderr,"Ref to " & t & "\n");
    END;
    EVAL Lex.Scan(rd,Lex.Blanks);

    IF NOT Rd.GetChar(rd) = '}' THEN
      Rd.UnGetChar(rd);
      Wr.PutText(Stdio.stderr,"Missing argument for \\(page)ref\n");
      RETURN;
    END;
  END ProcessRef;

(* Indexes, exactly as in books, is not too well supported yet by HTML.
   In LaTeX, the syntax is 

| \index{primaryKey}
| \index{primaryKey@text}
| \index{primaryKey!secondaryKey}
| \index{primaryKey!secondaryKey@text}

   Thus, the key is everything up to the @ or the end. The text is
   everything after the @ or the ! or the beginning. *)
   
PROCEDURE ProcessIndex(rd: Rd.T; wr: Wr.T) =
  VAR
    key := "";
    text := "";
    textWr: TextWr.T;
    textRd: TextRd.T;
    keyEnd := FALSE;
    groupLevel := 0;
    c: CHAR;
  BEGIN
    EVAL Lex.Scan(rd,Lex.Blanks);
    IF NOT Rd.GetChar(rd) = '{' THEN
      Rd.UnGetChar(rd);
      Wr.PutText(Stdio.stderr,"Missing argument for \\index\n");
      RETURN;
    END;
    Wr.PutText(wr,"<SPAN CLASS=INDEX.MARK>\n");
    LOOP
      c := Rd.GetChar(rd);

      IF (NOT keyEnd) AND 
          (c = '!' OR c = '@' OR (c = '}' AND groupLevel <= 0)) THEN
        Wr.PutText(wr,"<SPAN CLASS=INDEX.KEY>" & EncodeString(key) & 
            "</SPAN>\n");
        IF c = '@' THEN keyEnd := TRUE; END;
        key := "";
      ELSIF c = '}' AND groupLevel <= 0 THEN
        (* The text may contain some tex commands. This can be translated
           using the same procedure as the comments. A comment end is added
           for the processing and removed afterwards. *)

        textRd := TextRd.New(key & "*)");
        textWr := TextWr.New();
        Comment(3,textRd,textWr);
        text := TextWr.ToText(textWr);
        text := Text.Sub(text,0,Text.Length(text) - 2);
        Wr.PutText(wr,"<SPAN CLASS=INDEX.TEXT>" & EncodeString(text) & 
            "</SPAN>\n");
      ELSE
        key := key & Text.FromChar(c);
      END;

      IF c = '{' THEN 
        INC(groupLevel);
      ELSIF c = '}' THEN
        DEC(groupLevel);
        IF groupLevel < 0 THEN EXIT; END;
      END;
    END;
    Wr.PutText(wr,"</SPAN>\n");
  END ProcessIndex;

(* Change & for &ampersand; and <> for &lt; and &gt; *)

PROCEDURE EncodeString(t: TEXT): TEXT =
  VAR
    result := "";
    c: CHAR;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      c := Text.GetChar(t,i);
      IF c = '&' THEN result := result & "&amp;";
      ELSIF c = '<' THEN result := result & "&lt;";
      ELSIF c = '>' THEN result := result & "&gt;";
      ELSIF c = '_' THEN result := result & "&un;";
      ELSIF c = '"' THEN result := result & "&quot;";
      ELSE result := result & Text.FromChar(c);
      END;
    END;
    RETURN result;
  END EncodeString;

(* Undo the encoding. Because the & are encoded, only the & produced
   by EncodeString may be present. *)

(* PROCEDURE DecodeString(t: TEXT): TEXT =
  VAR
    result := "";
    sub: TEXT;
    c: CHAR;
    i := 0;
    end := Text.Length(t);
  BEGIN
    LOOP
      IF i >= end THEN EXIT; END;
      c := Text.GetChar(t,i);
      IF c = '&' THEN
        sub := Text.Sub(t,i,4);
        IF Text.Equal(sub,"&lt;") THEN
          result := result & Text.FromChar('<');
          INC(i,4);
        ELSIF Text.Equal(sub,"&gt;") THEN
          result := result & Text.FromChar('>');
          INC(i,4);
        ELSIF Text.Equal(sub,"&un;") THEN
          result := result & Text.FromChar('_');
          INC(i,4);
        ELSIF Text.Equal(sub,"&quo") THEN
          result := result & Text.FromChar('"');
          INC(i,6);
        ELSIF Text.Equal(sub,"&amp") THEN
          result := result & Text.FromChar('&');
          INC(i,5);
        ELSE <* ASSERT FALSE *>
        END;
      ELSE 
        result := result & Text.FromChar(c);
        INC(i);
      END;
    END;
    RETURN result;
  END DecodeString;
*)

PROCEDURE GetArgument(rd: Rd.T): TEXT =
  VAR
    texArgument := "";
    c: CHAR;
  BEGIN
    TRY
      EVAL Lex.Scan(rd,Lex.Blanks);
      c := Rd.GetChar(rd);
      IF c = '*' THEN
        EVAL Lex.Scan(rd,Lex.Blanks);
        c := Rd.GetChar(rd);
      END;
      IF c = '{' THEN    
        texArgument := Lex.Scan(rd,ArgumentChar);
        EVAL Rd.GetChar(rd);
      ELSE
        Rd.UnGetChar(rd);
        texArgument := Lex.Scan(rd,Alphabetic);
      END;
    EXCEPT
    | Rd.EndOfFile => 
        Wr.PutText(Stdio.stderr,"Missing }, EOF found\n");
    END;
    RETURN texArgument;
  END GetArgument;

PROCEDURE GetItemArgument(rd: Rd.T): TEXT =
  VAR
    texArgument := "";
  BEGIN
    TRY
      EVAL Lex.Scan(rd,Lex.Blanks);
      Lex.Match(rd,"[");
      texArgument := Lex.Scan(rd,ItemArgumentChar);
      EVAL Rd.GetChar(rd);
    EXCEPT
    | Rd.EndOfFile => 
        Wr.PutText(Stdio.stderr,"Missing }, EOF found\n");
    END;
    RETURN texArgument;
  END GetItemArgument;

VAR
  rd: Rd.T;
  wr: Wr.T;
  fileName := Params.Get(1);
  referenceFile := "html/references.html";
  pathDepth := -1;
BEGIN
  (* Check the file name to see how deep it is within a package and
     compute the relative path to the reference list file. *)
  FOR i := 0 TO Text.Length(fileName) - 1 DO
    IF Text.GetChar(fileName,i) = '/' THEN INC(pathDepth); END;
  END;
  FOR i := 0 TO pathDepth - 1 DO
    referenceFile := "../" & referenceFile;
  END;

  TRY
    rd := Stdio.stdin;
    wr := Stdio.stdout;
    Trans(rd, wr);
    Wr.Close(wr);
    Rd.Close(rd);
  EXCEPT
  | Rd.Failure => Wr.PutText(Stdio.stderr, "? can't read file\n")
  | Wr.Failure => Wr.PutText(Stdio.stderr, "? can't write file\n")
  END
END Main.
