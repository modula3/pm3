MODULE Perfmeter;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1996/10/08 10:37:37  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT OSError, Pathname, Process, Thread, Usignal;

CONST
  argname = ARRAY Quantity OF
              TEXT{"cpu", "pkts", "page", "swap", "intr", "disk", "cntxt",
                   "load", "colls", "errs"};

PROCEDURE Start (         what     : QuantitySet   := DefaultQuantities;
                          logToFile: BOOLEAN       := FALSE;
                          fileName : Pathname.T    := NIL;
                 READONLY params   : ARRAY OF TEXT := NullParams         ):
  Process.T RAISES {OSError.E} =
  VAR
    process: Process.T;
    al, i  : CARDINAL;
    args   : REF ARRAY OF TEXT;
  BEGIN
    (* Compute size of parameter araay *)
    al := 1;                     (* One for "-a" *)
    (* Logging information *)
    IF logToFile THEN
      INC(al);                   (* "-l" *)
      IF fileName # NIL THEN INC(al, 2) END; (* "-n fileName" *)
    END;
    FOR q := FIRST(Quantity) TO LAST(Quantity) DO
      (* Two for every quantity "-t qnty" *)
      IF q IN what THEN INC(al, 2) END;
    END;
    (* Parameters supplied by application. *)
    INC(al, NUMBER(params));

    (* Create and fill argument array *)
    args := NEW(REF ARRAY OF TEXT, al);
    args^[0] := "-a";
    i := 1;
    IF logToFile THEN
      args[i] := "-l";
      INC(i);
      IF fileName # NIL THEN
        args[i] := "-n";
        args[i + 1] := fileName;
        INC(i, 2);
      END;
    END;
    FOR q := FIRST(Quantity) TO LAST(Quantity) DO
      IF q IN what THEN
        args[i] := "-t";
        args[i + 1] := argname[q];
        INC(i, 2);
      END;
    END;
    SUBARRAY(args^, i, NUMBER(params)) := params;

    (* Start perfmeter and wait 5 seconds *)
    process := Process.Create("perfmeter", args^);
    Thread.Pause(5.0D0);
    RETURN process;
  END Start;

PROCEDURE Stop (p: Process.T) =
  BEGIN
    EVAL Usignal.kill(Process.GetID(p), Usignal.SIGQUIT);
    EVAL Process.Wait(p);
  END Stop;


BEGIN
END Perfmeter.
