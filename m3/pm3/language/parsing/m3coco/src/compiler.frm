MODULE -->Grammar EXPORTS Main ;

(* This is an example of a rudimentary main module for use with m3coco *)

IMPORT -->Grammar ;

IMPORT Text, Rd, Wr, Fmt, Params, Stdio, FileRd, Process, Thread (*, Pathname*);

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
    scanOnly   := FALSE ;
    sourceFile : TEXT ;
    ss         : -->Grammar.ScanSymbol ;
    useStdin   : BOOLEAN;
    paramPos   : CARDINAL := 1;

BEGIN

(* check on correct parameter usage *)

  IF paramPos < Params.Count AND Text.Equal(Params.Get(paramPos), "-scan") THEN
    scanOnly   := TRUE;
    INC(paramPos);
  END;

  IF paramPos < Params.Count THEN
    sourceFile := Params.Get(paramPos);
  ELSE
    useStdin := TRUE;
(*
    Wr.PutText(Stdio.stderr, "Usage: " & Pathname.Base(Params.Get(0))
                                       & " [-scan] source-file\n") ;
    Process.Exit(1)
*)
  END;

  IF useStdin THEN
    rd := Stdio.stdin;
  ELSE
    TRY
      rd := FileRd.Open(sourceFile)
    EXCEPT
    ELSE
      Wr.PutText(Stdio.stderr, "cannot open '" & sourceFile & "'\n") ;
      Process.Exit(1)
    END;
  END;

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

    Wr.PutText(Stdio.stderr, "Parsing...\n") ;
    Wr.Flush(Stdio.stderr) ;

    p.parse() ;

    IF (e.num > 0) THEN
      Wr.PutText(Stdio.stderr, "Failed to Parse\n") ;
      Process.Exit(1)
    END ;

    Wr.PutText(Stdio.stderr, "Parsed Correctly\n")
  END ;

  IF NOT useStdin THEN
    Rd.Close(rd)
  END;
END -->Grammar.
