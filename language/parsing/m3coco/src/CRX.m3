MODULE CRX ;

(* CRX   Parser Generation
   ===   =================

   Uses the top-down graph and the computed sets of terminal start symbols
   from CRT to generate recursive descent parsing procedures.

   Errors are reported by error numbers. The corresponding error messages
   are written to <grammar name>e.txt.

   ---------------------------------------------------------------------*)

IMPORT CRT, CRA ;
IMPORT Rd, Wr, TextWr, Text, Fmt, Stdio, Process ;
IMPORT Frame ;

<* FATAL ANY *>

CONST
  symSetSize = 100; (* max.number of symbol sets of the generated parser *)
  maxTerm    =   5; (* sets of size < maxTerm are enumerated *)
  maxAlter   =   5; (* more than maxAlter alternatives are handled with
                       a case statement *)
                    (* kinds of generated error messages *)

VAR
  symSet:   ARRAY [0 .. symSetSize] OF CRT.Set; (* symbol sets in the
                                                   generated parser *)
  maxSS:    INTEGER; (* number of symbol sets *)
  errorNr:  INTEGER; (* number of last generated error message*)
  curSy:    INTEGER; (* symbol whose production is currently generated *)
  NewLine:  BOOLEAN;
  IndDisp:  INTEGER;

PROCEDURE SymbolName(i : INTEGER) : TEXT =
VAR sn : CRT.SymbolNode ;
BEGIN
  CRT.GetSym(i, sn) ;
  IF (Text.Length(sn.constant) > 0) THEN
    RETURN sn.constant
  ELSE
    RETURN Fmt.Int(i)
  END
END SymbolName ;

(* PutSet               Enumerate bitset
----------------------------------------------------------------------*)
PROCEDURE PutSet(wr : Wr.T ; READONLY s : CRT.Set) =
CONST MaxLine = 76 ;
VAR first:  BOOLEAN;
    i:      CARDINAL;
    l, len: INTEGER;
    sn:     CRT.SymbolNode;
BEGIN
  i := 0 ;
  first := TRUE ;
  len := 20 ;
  WHILE (i <= ORD(CRT.maxT)) DO
    IF (i IN s) THEN
      IF (first) THEN
        first := FALSE
      ELSE
        Wr.PutText(wr, ", ") ;
        INC(len, 2)
      END ;
      CRT.GetSym(i, sn) ;
      l := Text.Length(sn.constant) ;
      IF (l > 0) THEN
        IF ((len + l) > MaxLine) THEN
          Wr.PutText(wr, "\n                    ") ;
          len := 20 ;
        END ;
        Wr.PutText(wr, sn.constant) ;
        INC(len, l)
      ELSE
        IF ((len + l) > MaxLine) THEN
          Wr.PutText(wr, "\n                    ") ;
          len := 20 ;
        END ;
        Wr.PutText(wr, Fmt.Int(i)) ;
        INC(len, (i DIV 10) + 1)
      END
    END ;
    INC(i)
  END
END PutSet ;

(* PutSet1              Enumerate long set
----------------------------------------------------------------------*)
PROCEDURE PutSet1(wr : Wr.T ; READONLY s : CRT.Set) =
VAR i     : INTEGER ;
    first : BOOLEAN ;
BEGIN
  i := 0 ;
  first := TRUE ;
  WHILE (i <= CRT.maxT) DO
    IF (i IN s) THEN
      IF (first) THEN
        first := FALSE
      ELSE
        Wr.PutText(wr, ", ")
      END ;
      Wr.PutText(wr, SymbolName(i))
    END ;
    INC(i)
  END
END PutSet1 ;

(* Alternatives         Count alternatives of gp
----------------------------------------------------------------------*)
PROCEDURE Alternatives(gp : INTEGER) : INTEGER =
VAR gn : CRT.GraphNode ;
    n  : INTEGER ;
BEGIN
  n := 0 ;
  WHILE (gp > 0) DO
    CRT.GetNode(gp, gn) ;
    gp := gn.p2 ;
    INC(n)
  END ;
  RETURN n ;
END Alternatives ;

(* CopySourcePart       Copy sequence <pos> from input file to file <syn>
----------------------------------------------------------------------*)
PROCEDURE CopySourcePart(wr : Wr.T ; pos : CRT.Position ;
                         indent : INTEGER ; padFirst : BOOLEAN) =
VAR lastCh,
    ch:     CHAR;
    extra,
    col, i: INTEGER;
    nChars: INTEGER;
    pad    : TEXT ;
BEGIN
  IF (pos.beg < 0) THEN
    RETURN
  END ;

  IF (indent = 0) THEN
    pad := ""
  ELSE
    pad := Fmt.Pad("", indent)
  END ;

  nChars := pos.len ;
  col := pos.col - 1 ;
  ch := ' ' ;
  extra := 0 ;

  Rd.Seek(CRT.src, pos.beg) ;

(* skip leading blanks *)
  WHILE ((nChars > 0) AND (ch = ' ')) DO
    ch := Rd.GetChar(CRT.src) ;
    DEC(nChars) ;
    INC(col)
  END ;

  IF (padFirst) THEN
    Wr.PutText(wr, pad) ;
  END ;

  LOOP
    WHILE (ch = '\n') DO
    (* write blank lines with the correct number of leading blanks *)
      Wr.PutChar(wr, '\n') ;
      lastCh := ch ;
      IF (nChars > 0) THEN
        ch := Rd.GetChar(CRT.src) ;
        DEC(nChars)
      ELSE
        RETURN
      END ;
      IF (ch # '\n') THEN
        (* we have something on this line *)
        Wr.PutText(wr, pad) ;
        i := col - 1 - extra ;
        WHILE ((ch = ' ') AND (i > 0)) DO
          (* skip at most "col-1" blanks at beginning of line *)
          IF (nChars > 0) THEN
            ch := Rd.GetChar(CRT.src) ;
            DEC(nChars)
          ELSE
            RETURN
          END ;
          DEC(i)
        END
      END
    END ;
    (* Handle extra blanks *)
    i := 0 ;
    WHILE (ch = ' ') DO
      IF (nChars > 0) THEN
        ch := Rd.GetChar(CRT.src) ;
        DEC(nChars)
      ELSE
        RETURN
      END ;
      INC(i)
    END ;
    IF (ch # '\n') THEN
      IF (i > 0) THEN
        Wr.PutText(wr, Fmt.Pad("", i))
      END ;
      Wr.PutChar(wr, ch) ;
      IF (nChars > 0) THEN
        ch := Rd.GetChar(CRT.src) ;
        DEC(nChars)
      ELSE
        RETURN
      END
    END
  END
END CopySourcePart ;

PROCEDURE GenErrName(errSym : INTEGER) : TEXT =
VAR sn    : CRT.SymbolNode ;
    chars : REF ARRAY OF CHAR ;
BEGIN
  CRT.GetSym(errSym, sn) ;
  chars := NEW(REF ARRAY OF CHAR, Text.Length(sn.name)) ;
  Text.SetChars(chars^, sn.name) ;
  FOR i := FIRST(chars^) TO LAST(chars^) DO
    IF (chars[i] = '"') THEN
      chars[i] := '\''
    END
  END ;
  RETURN Text.FromChars(chars^)
END GenErrName ;

(* NewCondSet    Generate a new condition set, if set not yet exists
----------------------------------------------------------------------*)
PROCEDURE NewCondSet (READONLY set: CRT.Set): INTEGER =
  VAR
    i: INTEGER;
  BEGIN
    i := 1; (*skip symSet[0]*)
    WHILE i <= maxSS DO
      IF (set = symSet[i]) THEN RETURN i END;
      INC(i)
    END;
    INC(maxSS);
    IF maxSS > symSetSize THEN CRT.Restriction(5, symSetSize) END;
    symSet[maxSS] := set;
    RETURN maxSS
  END NewCondSet;

(* GenCond              Generate code to check if sym is in set
----------------------------------------------------------------------*)
PROCEDURE GenCond(wr : Wr.T ; READONLY set : CRT.Set ; indent : INTEGER) =
VAR i, n : INTEGER ;

  PROCEDURE Small(READONLY s : CRT.Set) : BOOLEAN =
  BEGIN
    i := 16 ;
    WHILE (i <= CRT.maxT) DO
      IF (i IN s) THEN
        RETURN FALSE
      END ;
      INC(i)
    END ;
    RETURN TRUE
  END Small ;

BEGIN
  n := 0 ;
  FOR e := 0 TO CRT.maxTerminals DO
    IF (e IN set) THEN
      INC(n)
    END
  END ;
  IF (n = 0) THEN
    Wr.PutText(wr, " FALSE") (*this branch should never be taken*)
  ELSIF (n <= maxTerm) THEN
    i := 0 ;
    WHILE (i <= CRT.maxT) DO
      IF (i IN set) THEN
        Wr.PutText(wr, " (p.next.sym = " & SymbolName(i) & ")") ;
        DEC(n) ;
        IF (n > 0) THEN
          Wr.PutText(wr, " OR\n") ;
          Wr.PutText(wr, Fmt.Pad("", indent))
        END
      END ;
      INC(i)
    END
  ELSIF (Small(set)) THEN
    Wr.PutText(wr, " (p.next.sym IN SymbolSet{") ;
    PutSet(wr, set) ;
    Wr.PutText(wr, "}) ")
  ELSE
    Wr.PutText(wr, " (p.next.sym IN SymSet[") ;
    Wr.PutText(wr, Fmt.Int(NewCondSet(set))) ;
    Wr.PutText(wr, "])")
  END
END GenCond ;

(* GenCode              Generate code for graph gp in production curSy
----------------------------------------------------------------------*)
PROCEDURE GenCode(wr : Wr.T ; gp, indent : INTEGER ; VAR checked : CRT.Set) =
VAR gn, gn2:          CRT.GraphNode;
    sn:               CRT.SymbolNode;
    gp2:              INTEGER;
    s1, s2:           CRT.Set;
    alts,
    indent1,
    addInd,
    errSemNod:        INTEGER;
    FirstCase, equal,
    OldNewLine:       BOOLEAN;
    pad : TEXT ;
BEGIN
  IF (indent = 0) THEN
    pad := ""
  ELSE
    pad := Fmt.Pad("", indent)
  END ;
  WHILE (gp > 0) DO
    CRT.GetNode(gp, gn) ;
    CASE (gn.typ) OF

      CRT.nt =>

        CRT.GetSym(gn.p1, sn) ;
        Wr.PutText(wr, pad & "Parse" & sn.name & "(p") ;
        IF (gn.pos.beg >= 0) THEN
          Wr.PutText(wr, ", ") ;
          NewLine := FALSE ;
          indent1 := indent + Text.Length(sn.name) + 1 ;
          CopySourcePart(wr, gn.pos, indent1, FALSE) ;
        END ;
        Wr.PutText(wr, ") ;\n")

    | CRT.t =>

        CRT.GetSym(gn.p1, sn) ;
        IF (gn.p1 IN checked) THEN
          Wr.PutText(wr, pad & "Get(p) ;\n") ;
        ELSE
          Wr.PutText(wr, pad & "Expect(p, " & SymbolName(gn.p1) & ") ;\n")
        END

    | CRT.wt =>

        CRT.CompExpected(ABS(gn.next), curSy, s1) ;
        CRT.GetSet(0, s2) ;
        s1 := s1 + s2 ;
        CRT.GetSym(gn.p1, sn) ;
        Wr.PutText(wr, pad & "ExpectWeak(p, " & SymbolName(gn.p1) & ", ") ;
        Wr.PutText(wr, Fmt.Int(NewCondSet(s1)) & ") ;\n")

    | CRT.any =>

        Wr.PutText(wr, pad & "Get(p) ;\n")

    | CRT.eps => (* nothing *)

    | CRT.sem =>

        CopySourcePart(wr, gn.pos, indent, TRUE) ;
        Wr.PutText(wr, " ;\n") ;

    | CRT.sync =>

        CRT.GetSet(gn.p1, s1) ;
        Wr.PutText(wr, pad & "WHILE (NOT (") ;
        GenCond(wr, s1, indent + 9) ;
        Wr.PutText(wr, ")) DO\n") ;
        Wr.PutText(wr, pad & "  SynError(p, \"unexpected '\" & ") ;
        Wr.PutText(wr, "SymbolName[p.next.sym] & \"' in ") ;
        Wr.PutText(wr, GenErrName(curSy) & "\") ;\n") ;
        Wr.PutText(wr, pad & "  Get(p)\n") ;
        Wr.PutText(wr, pad & "END ;\n")

    | CRT.alt =>

        CRT.CompFirstSet(gp, s1) ;
        equal := (s1 = checked) ;
        alts := Alternatives(gp) ;
        OldNewLine := NewLine ;
        IF (alts > maxAlter) THEN
          Wr.PutText(wr, pad & "CASE p.next.sym OF\n")
        END ;
        gp2 := gp ;
        IF (alts > maxAlter) THEN
          addInd := 4
        ELSE
          addInd := 2
        END ;
        errSemNod := -1 ;
        FirstCase := TRUE ;
        WHILE (gp2 # 0) DO
          CRT.GetNode(gp2, gn2) ;
          CRT.CompExpected(gn2.p1, curSy, s1) ;
          Wr.PutText(wr, pad) ;
          IF (alts > maxAlter) THEN
            IF (FirstCase) THEN
              FirstCase := FALSE ;
              Wr.PutText(wr, "  ")
            ELSE
              Wr.PutText(wr, "| ")
            END ;
            PutSet1(wr, s1) ;
            Wr.PutText(wr, " =>\n")
          ELSIF (gp2 = gp) THEN
            Wr.PutText(wr, "IF") ;
            GenCond(wr, s1, indent + 2) ;
            Wr.PutText(wr, " THEN\n")
          ELSIF ((gn2.p2 = 0) AND equal) THEN
            Wr.PutText(wr, "ELSE\n")
          ELSE
            Wr.PutText(wr, "ELSIF") ;
            GenCond(wr, s1, indent + 5) ;
            Wr.PutText(wr, " THEN\n")
          END ;
          s1 := s1 + checked ;
          GenCode(wr, gn2.p1, indent + addInd, s1) ;
          NewLine := TRUE ;
          gp2 := gn2.p2 ;
        END ;
        IF (NOT equal) THEN
          Wr.PutText(wr, pad & "ELSE SynError(p, \"unexpected '\"") ;
          Wr.PutText(wr, " & SymbolName[p.next.sym] & \"' in ") ;
          Wr.PutText(wr, GenErrName(curSy) & "\") ;\n")
        END ;
        Wr.PutText(wr, pad & "END ;\n")

    | CRT.iter =>

        CRT.GetNode(gn.p1, gn2) ;
        Wr.PutText(wr, pad & "WHILE") ;
        IF (gn2.typ = CRT.wt) THEN
          CRT.CompExpected(ABS(gn2.next), curSy, s1) ;
          CRT.CompExpected(ABS(gn.next), curSy, s2) ;
          CRT.GetSym(gn2.p1, sn) ;
          Wr.PutText(wr, " WeakSeparator(p, " & SymbolName(gn2.p1) & ", ") ;
          Wr.PutText(wr, Fmt.Int(NewCondSet(s1)) & ", ") ;
          Wr.PutText(wr, Fmt.Int(NewCondSet(s2)) & ")") ;
          s1 := CRT.Set{} ; (*for inner structure*)
          IF (gn2.next > 0) THEN
            gp2 := gn2.next
          ELSE
            gp2 := 0
          END
        ELSE
          gp2 := gn.p1 ;
          CRT.CompFirstSet(gp2, s1) ;
          GenCond(wr, s1, indent + 5)
        END ;
        Wr.PutText(wr, " DO\n") ;
        GenCode(wr, gp2, indent + 2, s1) ;
        Wr.PutText(wr, pad & "END ;\n")

    | CRT.opt =>

        CRT.CompFirstSet(gn.p1, s1) ;
        IF (checked = s1) THEN
          GenCode(wr, gn.p1, indent, checked) ;
        ELSE
          Wr.PutText(wr, pad & "IF") ;
          GenCond(wr, s1, indent + 2) ;
          Wr.PutText(wr, " THEN\n") ;
          GenCode(wr, gn.p1, indent + 2, s1) ;
          Wr.PutText(wr, pad & "END ;\n")
        END

    ELSE
      Wr.PutText(Stdio.stderr, "Fatal error in CRX.GenCode()!\n") ;
      Process.Exit(1)
    END ;

    IF ((gn.typ # CRT.eps) AND (gn.typ # CRT.sem) AND (gn.typ # CRT.sync)) THEN
      checked := CRT.Set{}
    END ;
    gp := gn.next ;
  END
END GenCode ;

(* GenPragmaCode        Generate code for pragmas
----------------------------------------------------------------------*)
PROCEDURE GenPragmaCode(wr : Wr.T) =
VAR i:          INTEGER ;
    sn:         CRT.SymbolNode ;
    FirstCase : BOOLEAN ;
BEGIN
  i := CRT.maxT + 1 ;
  IF (i > CRT.maxP) THEN
    RETURN
  END ;
  FirstCase := TRUE ;
  Wr.PutText(wr, "CASE p.next.sym OF\n") ;
  LOOP
    CRT.GetSym(i, sn) ;
    IF (FirstCase) THEN
      FirstCase := FALSE ;
      Wr.PutText(wr, "  ")
    ELSE
      Wr.PutText(wr, "| ")
    END ;
    Wr.PutText(wr, SymbolName(i) & " =>\n") ;
    NewLine := FALSE ;
    CopySourcePart(wr, sn.semPos, 6, TRUE) ;
    IF (i = CRT.maxP) THEN
      EXIT
    END ;
    INC(i) ;
    Wr.PutChar(wr, '\n')
  END ;
  Wr.PutText(wr, "ELSE\nEND ;\n") ;
END GenPragmaCode ;

(* GenProductions       Generate code for all productions
----------------------------------------------------------------------*)
PROCEDURE GenProductions(wr : Wr.T) =
VAR sn      : CRT.SymbolNode ;
    checked : CRT.Set ;
  BEGIN
    curSy := CRT.firstNt ;
    NewLine := TRUE ;
    WHILE (curSy <= CRT.lastNt) DO (* for all nonterminals *)
      CRT.GetSym(curSy, sn) ;
      Wr.PutText(wr, "PROCEDURE Parse" & sn.name & "(p : Parser") ;
      IF (sn.attrPos.beg >= 0) THEN
        Wr.PutText(wr, " ; ") ;
        NewLine := FALSE ;
        CopySourcePart(wr, sn.attrPos, 12 + Text.Length(sn.name), FALSE)
      END ;
      Wr.PutText(wr, ") =\n") ;
      IF (sn.semPos.beg >= 0) THEN
        CopySourcePart(wr, sn.semPos, 2, TRUE) ;
        Wr.PutChar(wr, '\n')
      END ;
      Wr.PutText(wr, "BEGIN\n") ;
      checked := CRT.Set{} ;
      GenCode(wr, sn.struct, 2, checked) ;
      Wr.PutText(wr, "END Parse" & sn.name & " ;\n\n") ;
      INC(curSy) ;
  END ;
END GenProductions ;

(* GenSymbolSets          Initialise all sets
----------------------------------------------------------------------*)
PROCEDURE GenSymbolSets(wr : Wr.T) =
BEGIN
  CRT.GetSet(0, symSet[0]) ;
  FOR i := 0 TO maxSS DO
    IF (i # 0) THEN
      Wr.PutText(wr, ",\n")
    END ;
    Wr.PutText(wr, "  SymbolSet{") ;
    PutSet(wr, symSet[i]) ;
    Wr.PutChar(wr , '}')
  END
END GenSymbolSets ;

(* Generate - generate the parser code fragments *)

PROCEDURE Generate() =
VAR i, len, pos : INTEGER ;
    checked     : CRT.Set ;
    gn          : CRT.GraphNode ;
    sn          : CRT.SymbolNode ;
    wr          : Wr.T ;
    name        : TEXT ;
BEGIN

  CRT.GetNode(CRT.root, gn) ;
  CRT.GetSym(gn.p1, sn) ;

(* register the parser name *)

  Frame.Put("Parser", sn.name & "P") ;

(* set up the error stream and generate the token errors *)

(*
  err := TextWr.New() ;

  i := 0 ;
  WHILE (i <= CRT.maxT) DO
    GenErrorMsg(tErr, i, errNr) ;
    INC(i)
  END ;
*)

  Frame.Put("ErrCurPos", "S.Error(errNo, S.line, S.col, S.pos)") ;
  Frame.Put("ErrNextPos", "S.Error(errNo, S.nextLine. S.nextCol, S.nextPos)") ;

  wr := TextWr.New() ;

(* generate token list *)

  i := 0 ;
  pos := 0 ;
  REPEAT
    CRT.GetSym(i, sn) ;
    name := Text.Sub(sn.constant, 7) ;
    len := Text.Length(name) ;
    IF (len > 0) THEN
      IF ((pos + len + 6) > CRA.MaxSourceLineLength) THEN
        Wr.PutText(wr, "\n") ;
        pos := 0
      END ;
      Wr.PutText(wr, name) ;
      INC(pos, len) ;
      IF (i < CRT.maxP) THEN
        Wr.PutText(wr, ", ") ;
        INC(pos, 2)
      END
    END ;
    INC(i) ;
  UNTIL (i > CRT.maxP) ;

  Frame.Put("Symbols", TextWr.ToText(wr)) ;

  FOR i := CRT.maxT + 1 TO CRT.maxP DO
    CRT.GetSym(i, sn) ;
    Wr.PutText(wr, sn.constant) ;
    IF (i < CRT.maxP) THEN
      Wr.PutText(wr, ", ")
    END
  END ;

  Frame.Put("PragmaSet", TextWr.ToText(wr)) ;

(* generate the token names *)

  i := 0 ;
  pos := 0 ;
  REPEAT
    CRT.GetSym(i, sn) ;
    len := Text.Length(sn.name) ;
    IF ((pos + len + 6) > CRA.MaxSourceLineLength) THEN
      Wr.PutText(wr, "\n") ;
      pos := 0
    END ;
    IF (Text.GetChar(sn.name, 0) = '"') THEN
      Wr.PutText(wr, sn.name) ;
      INC(pos, len)
    ELSE
      Wr.PutText(wr, "\"" & sn.name & "\"") ;
      INC(pos, len + 2)
    END ;
    IF (i < CRT.maxP) THEN
      Wr.PutText(wr, ", ") ;
      INC(pos, 2)
    END ;
    INC(i) ;
  UNTIL (i > CRT.maxP) ;

  Frame.Put("SymbolNames", TextWr.ToText(wr)) ;

(* generate the declarations *)

  CopySourcePart(wr, CRT.semDeclPos, 0, TRUE) ;
  Frame.Put("Declarations", TextWr.ToText(wr)) ;

(* generate pragma code *)

  GenPragmaCode(wr) ;
  Frame.Put("Pragmas", TextWr.ToText(wr)) ;

(* generate productions *)

  GenProductions(wr) ;
  Frame.Put("Productions", TextWr.ToText(wr)) ;

(* generate the parse root code *)

  Wr.PutText(wr, "Get(p) ;\n") ;
  checked := CRT.Set{} ;
  GenCode(wr, CRT.root, 0, checked) ;

  Frame.Put("ParseRoot", TextWr.ToText(wr)) ;

(* generate set initializers *)

  GenSymbolSets(wr) ;
  Frame.Put("SymbolSets", TextWr.ToText(wr)) ;

(* generate the constants *)

  Frame.Put("MaxTerminals", Fmt.Int(CRT.maxT)) ;
  Frame.Put("MaxPragmas", Fmt.Int(CRT.maxP)) ;

  IF (maxSS < 0) THEN
    maxSS := 0
  END ;

  Frame.Put("MaxSets", Fmt.Int(maxSS)) ;

(* now we've gathered all errors, commit them *)

(*
  Frame.Put("Errors", TextWr.ToText(err)) ;
  Wr.Close(err)
*)

END Generate ;

(* WriteStatistics      Write statistics about compilation to list file
----------------------------------------------------------------------*)
PROCEDURE WriteStatistics(wr : Wr.T) =

  PROCEDURE WriteNumbers (used, available: INTEGER) =
  BEGIN
    Wr.PutText(wr, Fmt.F("%6s (limit %5s)\n", Fmt.Int(used + 1),
                                              Fmt.Int(available)))
  END WriteNumbers ;

BEGIN
  Wr.PutText(wr, "Statistics:\n\n") ;
  Wr.PutText(wr, "  nr of terminals:    ") ;
  WriteNumbers(CRT.maxT, CRT.maxTerminals) ;
  Wr.PutText(wr, "  nr of non-terminals:") ;
  WriteNumbers(CRT.lastNt-CRT.firstNt, CRT.maxSymbols-CRT.maxT-1) ;
  Wr.PutText(wr, "  nr of pragmas:      ") ;
  WriteNumbers(CRT.maxSymbols-CRT.lastNt-2, CRT.maxSymbols-CRT.maxT-1) ;
  Wr.PutText(wr, "  nr of symbolnodes:  ") ;
  WriteNumbers(CRT.maxSymbols-CRT.firstNt+CRT.maxT, CRT.maxSymbols) ;
  Wr.PutText(wr, "  nr of graphnodes:   ") ;
  WriteNumbers(CRT.nNodes, CRT.maxNodes) ;
  Wr.PutText(wr, "  nr of conditionsets:") ;
  WriteNumbers(maxSS, symSetSize) ;
  Wr.PutText(wr, "  nr of charactersets:") ;
  WriteNumbers(CRT.maxC, CRT.maxClasses) ;
  Wr.PutText(wr, "\n\n") ;
END WriteStatistics ;

BEGIN
  errorNr := -1 ;
  maxSS := 0 ; (*symSet[0] reserved for allSyncSyms*)
  NewLine := TRUE ;
  IndDisp := 0 ;
END CRX.
