MODULE -->Grammar EXPORTS Main ;

(* This is an example of a rudimentary main module for use with m3coco *)

IMPORT -->Grammar ;

IMPORT Text, Rd, Wr, Fmt, Params, Stdio, FileRd, Process, Thread, Pathname ;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

(* --------------------------- error handler ------------------------------- *)

TYPE
  Err = -->Grammar.ErrHandler BRANDED OBJECT
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

VAR rd         : Rd.T ;
    e          : Err ;
    s          : -->Grammar.Scanner ;
    p          : -->Grammar.Parser ;
    scanOnly   : BOOLEAN ;
    sourceFile : TEXT ;
    ss         : -->Grammar.ScanSymbol ;

BEGIN

(* check on correct parameter usage *)

  IF ((Params.Count = 3) AND Text.Equal(Params.Get(1), "-scan")) THEN
    scanOnly   := TRUE ;
    sourceFile := Params.Get(2)
  ELSIF (Params.Count = 2) THEN
    scanOnly   := FALSE ;
    sourceFile := Params.Get(1)
  ELSE
    Wr.PutText(Stdio.stderr, "Usage: " & Pathname.Base(Params.Get(0))
                                       & " [-scan] source-file\n") ;
    Process.Exit(1)
  END ;

  TRY
    rd := FileRd.Open(sourceFile)
  EXCEPT
  ELSE
    Wr.PutText(Stdio.stderr, "cannot open '" & sourceFile & "'\n") ;
    Process.Exit(1)
  END ;

  e := NEW(Err) ;
  s := NEW(-->Grammar.Scanner).init(rd, e) ;

  IF (scanOnly) THEN
    Wr.PutText(Stdio.stdout, "Scanning...\n") ;
    Wr.Flush(Stdio.stdout) ;

    s.get(ss) ;
    WHILE (ss.sym # -->Grammar.-->EofSym) DO
      Wr.PutText(Stdio.stdout, Fmt.F("line %3s col %2s = %s (%s)\n",
                                     Fmt.Int(ss.line), Fmt.Int(ss.column),
                                     -->Grammar.SymbolName[ss.sym],
                                     ss.string)) ;
      s.get(ss)
    END
  ELSE
    p := NEW(-->Grammar.Parser).init(s, e) ;

    Wr.PutText(Stdio.stdout, "Parsing...\n") ;
    Wr.Flush(Stdio.stdout) ;

    p.parse() ;

    IF (e.num > 0) THEN
      Wr.PutText(Stdio.stderr, "Failed to Parse\n") ;
      Process.Exit(1)
    END ;

    Wr.PutText(Stdio.stdout, "Parsed Correctly\n")
  END ;

  Rd.Close(rd)
END -->Grammar.
