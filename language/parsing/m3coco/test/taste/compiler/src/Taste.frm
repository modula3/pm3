MODULE -->Grammar EXPORTS Main ;

(* Simple Taste compiler/interpreter *)

IMPORT -->Grammar ;

IMPORT TasteCode ;
IMPORT Rd, Wr, Fmt, Params, Stdio, FileRd, Process, Thread, Pathname ;

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
    Wr.PutText(Stdio.stderr, Fmt.F("line %3s col %2s: %s\n",
                                   Fmt.Int(line), Fmt.Int(col), msg))
  EXCEPT
  ELSE
  END
END GenError ;

VAR rd         : Rd.T ;
    e          : Err ;
    s          : -->Grammar.Scanner ;
    p          : -->Grammar.Parser ;

BEGIN

  IF (Params.Count # 2) THEN
    Wr.PutText(Stdio.stderr, "Usage: " & Pathname.Base(Params.Get(0))
                                       & " source-file\n") ;
    Process.Exit(1)
  END ;

  TRY
    rd := FileRd.Open(Params.Get(1))
  EXCEPT
  ELSE
    Wr.PutText(Stdio.stderr, "cannot open '" & Params.Get(1) & "'\n") ;
    Process.Exit(1)
  END ;

  e := NEW(Err) ;
  s := NEW(-->Grammar.Scanner).init(rd, e) ;
  p := NEW(-->Grammar.Parser).init(s, e) ;

  p.parse() ;

  Rd.Close(rd) ;

  IF (e.num > 0) THEN
    Wr.PutText(Stdio.stderr, "Failed to Parse\n") ;
    Process.Exit(1)
  END ;

  TasteCode.Interpret()
END -->Grammar.
