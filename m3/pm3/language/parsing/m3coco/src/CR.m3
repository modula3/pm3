(* CR   Main Module of m3coco
   ==   =====================

   This is a compiler generator that produces a scanner and a parser
   from an attributed grammar, and optionally a complete small compiler.

   Original code in Oberon by Hanspeter Moessenboeck, ETH Zurich
   Ported at ETH to Apple Modula, and thence to JPI-2 Modula.

   JPI version of 27 January 1991 was then modified to make more
   portable by Pat Terry, January - October 1992

   Mocka-unix version (Pat Terry 1995)

   This is the Modula-3 port (Olly Stephens 1997)

Usage:
          m3coco [-options] GrammarName[.atg]

Input:
  attributed grammar     input grammar
  interface.frm          frame file for interface
  scanner.frm            frame file for scanner module
  parser.frm             frame file for parser module
  compiler.frm
  (or <GrammarName>.frm) frame file for main module (optional)

(default frame files are embedded in the program and will be used unless
 replacements exist in the same directory as the grammar)

Output:
  <GrammarName>.i3        generated interface
                          (defines scanner and parser objects)
  <GrammarName>S.m3       generated scanner
  <GrammarName>P.m3       generated parser
  <GrammarName>.lst       source listing with error messages and trace output
  <GrammarName>.m3        generated compiler main module (optional)

Implementation restrictions
  1  too many nodes in graph (>1500)                 CRT.NewNode
  2  too many symbols (>500)                         CRT.NewSym, MovePragmas
  3  too many sets (>256 ANY-syms or SYNC syms)      CRT.NewSet,
  4  too many character classes (>250)               CRT.NewClass
  5  too many conditions in generated code (>100)    CRX.NewCondSet
  6  too many token names in "NAMES" (>100)          CRT.NewName
  7  too many states in automata (>500)              CRA.NewState

Trace output
(To activate a trace switch, write "${letter}" in the input grammar, or
invoke Coco with a second command line parameter)

  A  Prints states of automaton

  B  Use built-in frames even if external ones exist in the current dir

  C  Generates complete compiler module

  D  Suppresses Def Mod generation

  F  Prints start symbols and followers of nonterminals.

  G  Prints the top-down graph.

  I  Trace of start symbol set computation.

  L  Forces a listing (otherwise a listing is only printed if errors are found).
  
  O  Trace of follow set computation (not yet implemented).

  P  Generates parser only

  S  Prints the symbol list.

  T  Suppresses generation of files (grammar tests only).

  X  Prints a cross reference list.

  Z  Scans input file only (no parsing)

==========================================================================*)

MODULE CR EXPORTS Main ;

IMPORT CR, CRT, CRA, CRX, Frame ;
IMPORT ASCII, Text, Rd, Wr, FileRd, FileWr,
       Pathname, Process, Stdio, Params, Fmt ;

<* FATAL ANY *>

CONST
  Version = "0.04" ;

TYPE
  Err = CR.ErrHandler BRANDED OBJECT
          num : CARDINAL := 0
        OVERRIDES
          error := GenError
        END ;

PROCEDURE GenError(e : Err ; line : CARDINAL ; col : CARDINAL ; msg : TEXT) =
BEGIN
  INC(e.num) ;
  TRY
    Wr.PutText(Stdio.stderr, Fmt.F("line %3s col %2s: %s\n", Fmt.Int(line),
                                                             Fmt.Int(col), msg))
  EXCEPT ELSE END
END GenError ;

PROCEDURE SetOption(s: TEXT) =
(* Set compiler options *)
VAR
  i  : CARDINAL ;
  ch : CHAR ;
BEGIN
  i := 1;
  WHILE (i < Text.Length(s)) DO
    ch := ASCII.Upper[Text.GetChar(s, i)] ;
    IF (ch IN ASCII.Uppers) THEN
      CRT.ddt[ch] := TRUE
    END ;
    INC(i) ;
  END ;
END SetOption ;

PROCEDURE Msg(S : TEXT) =
BEGIN
  Wr.PutText(Stdio.stdout, S & "\n")
END Msg ;

PROCEDURE Help() =
BEGIN
  Msg("Usage: m3coco [-options] Grammar[.atg]") ;
  Msg("Example: m3coco -cs Test") ;
  Msg("") ;
  Msg("Options are") ;
  Msg("a  - Trace automaton") ;
  Msg("c  - Generate compiler module") ;
  Msg("d  - Suppress generation of Definition Modules") ;
  Msg("f  - Give Start and Follower sets") ;
  Msg("g  - Print top-down graph") ;
  Msg("i  - Trace start set computations") ;
  Msg("l  - Force listing") ;
  Msg("p  - Generate parser only") ;
  Msg("s  - Print symbol table") ;
  Msg("t  - Grammar tests only - no code generated") ;
  Msg("x  - Print cross reference list") ;
  Msg("z  - Scan input file only")
END Help ;
 
VAR lst       : Wr.T ;
    e         : Err ;
    s         : CR.Scanner ;
    p         : CR.Parser ;
    ll1       : BOOLEAN ; (* TRUE if grammar is LL(1) *)
    ok        : BOOLEAN ; (* TRUE if grammar tests ok so far *)
    grammar   : TEXT ;
    param     := 1 ;
    directory : Pathname.T ;
    path      : Pathname.T ;

BEGIN
  Wr.PutText(Stdio.stdout, "m3coco (compiler compiler) v" & Version & "\n") ;
  Wr.Flush(Stdio.stdout) ;
  IF (Params.Count = 1) THEN
    Help() ;
    Process.Exit(0)
  END ;
  grammar := Params.Get(param) ;
  INC(param) ;

(* accept options before filename *)

  WHILE ((grammar # NIL) AND (Text.GetChar(grammar, 0) = '-')) DO
    SetOption(grammar) ;
    IF (param = Params.Count) THEN
      grammar := NIL
    ELSE
      grammar := Params.Get(param) ;
      INC(param)
    END
  END ;

  IF (grammar = NIL) THEN
    Wr.PutText(Stdio.stderr, "Missing grammar name\n") ;
    Process.Exit(1)
  END ;

  IF (Text.Equal(Pathname.LastExt(grammar), "")) THEN
    grammar := Pathname.ReplaceExt(grammar, "atg")
  END ;

  TRY
    CRT.src := FileRd.Open(grammar)
  EXCEPT
  ELSE
    Wr.PutText(Stdio.stderr, "cannot open " & grammar & " - aborting\n") ;
    Process.Exit(1)
  END ;

  directory := Pathname.Prefix(grammar) ;
  grammar := Pathname.LastBase(grammar) ;

  path := Pathname.Join(NIL, grammar, "lst") ;
  TRY
    lst := FileWr.Open(path)
  EXCEPT
  ELSE
    Wr.PutText(Stdio.stderr, "cannot open " & path & " - aborting\n") ;
    Process.Exit(1)
  END ;

  Wr.PutText(lst, "m3coco (compiler compiler) v" & Version & "\n\n") ;
  Wr.PutText(lst, "Source File: " & grammar & "\n\n\n") ;

  e := NEW(Err) ;
  s := NEW(CR.Scanner).init(CRT.src, e) ;

(* debug - scanner testing *)
  IF (CRT.ddt['Z']) THEN
    VAR ss : CR.ScanSymbol ;
    BEGIN
      s.get(ss) ;
      WHILE (ss.sym # CR.Symbol.Eof) DO
        Wr.PutText(Stdio.stdout,
                   Fmt.FN("off %4s line %3s col %2s len %2s : %s (%s)\n",
                      ARRAY OF TEXT{ Fmt.Int(ss.offset), Fmt.Int(ss.line),
                                     Fmt.Int(ss.column), Fmt.Int(ss.length),
                                     CR.SymbolName[ss.sym], ss.name})) ;
        Wr.Flush(Stdio.stdout) ;
        s.get(ss)
      END ;
      Process.Exit(0)
    END
  END ;

  p := NEW(CR.Parser).init(s, e) ;

  Msg("Parsing File...") ;

  p.parse() ;

  IF (e.num = 0) THEN
    Msg("Testing Grammar...") ;
    Wr.PutText(lst, "Grammar Tests:\n\n") ;
    CRT.CompSymbolSets(lst) ;
    CRT.TestCompleteness(lst, ok) ;
    IF (ok) THEN
      CRT.TestIfAllNtReached(lst, ok)
    END ;
    IF (ok) THEN
      CRT.FindCircularProductions(lst, ok)
    END ;
    IF (ok) THEN
      CRT.TestIfNtToTerm(lst, ok)
    END ;
    IF (ok) THEN
      CRT.LL1Test(lst, ll1)
    END ;
    Wr.PutText(lst, "\n") ;
    IF (CRT.genScanner AND CRT.ddt['A']) THEN
      CRA.PrintStates(lst)
    END ;
    IF ((NOT ok) OR (NOT ll1) OR CRT.ddt['L'] OR CRT.ddt['X']) THEN
      Msg("Listing...") ;
      IF (CRT.ddt['X']) THEN
        CRT.XRef(lst) ;
      END
    END ;
    Msg("Symbol Name Assignment...") ;
    CRT.AssignSymNames(CRT.ddt['N'], CRT.symNames) ;
    IF (ok AND ll1 AND (NOT CRT.ddt['T'])) THEN
      Msg("Generating Code Fragments...") ;
      Frame.Put("Grammar", grammar) ;
      CRX.Generate() ;
      IF (CRT.genScanner AND (NOT CRT.ddt['P'])) THEN
        CRA.Generate() ;
      END ;

      Msg("Generating Parser '" & grammar & "P.m3'...") ;
      Frame.Create(directory, "parser.frm", NIL, grammar & "P.m3") ;

      IF (CRT.genScanner AND (NOT CRT.ddt['P'])) THEN
        Msg("Generating Scanner '" & grammar & "S.m3'...") ;
        Frame.Create(directory, "scanner.frm", NIL, grammar & "S.m3") ;
      END ;

      Msg("Generating Interface '" & grammar & ".i3'...") ;
      Frame.Create(directory, "interface.frm", NIL, grammar & ".i3") ;

      IF (CRT.ddt['C']) THEN
        Msg("Generating Compiler '" & grammar & ".m3'...") ;
        Frame.Create(directory, "compiler.frm", grammar & ".frm",
                                                grammar & ".m3")
      END ;
      CRX.WriteStatistics(lst) ;
    END ;
    IF (NOT ok) THEN
      Msg("Compilation ended with errors in grammar tests.")
    ELSIF (NOT ll1) THEN
      Msg("Compilation ended with LL(1) errors.")
    ELSE
      Msg("Compilation completed. No errors detected.")
    END
  ELSE
    Msg("Listing...") ;
    IF (CRT.ddt['X']) THEN
      CRT.XRef(lst)
    END ;
    Msg("*** Errors Detected ***")
  END ;
  IF (CRT.ddt['G']) THEN
    CRT.PrintGraph(lst)
  END ;
  IF (CRT.ddt['S']) THEN
    CRT.PrintSymbolTable(lst)
  END ;
  Wr.Close(lst) ;
  Rd.Close(CRT.src) ;

END CR.
