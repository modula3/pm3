MODULE BenchmarkAux;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.2  1997/02/20 16:08:14  roland
    OO1 rewritten with graphical user interface.

    Revision 1.1  1996/10/08 10:37:29  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT BenchmarkLog, Process, OSError, Thread;

(*
 | --- auxiliary procedures for benchmark ---------------------------------
 *)
PROCEDURE ClearSystemCaches (l: BenchmarkLog.T := NIL) RAISES {Thread.Alerted}=
  VAR handle: Process.T;
  BEGIN
    TRY
      IF l # NIL THEN l.writeEvent("Clearing system caches"); END;
      handle := Process.Create("./coolscript", ARRAY [0 .. -1] OF TEXT{});
      IF l # NIL AND Process.Wait(handle) # 0 THEN
        l.write("Warning: Unable to clear system caches!\n");
      END;
    EXCEPT
    | OSError.E =>
        IF l # NIL THEN
          l.write("Error: Unable to clear system caches!\n");
        END;
    END;
  END ClearSystemCaches;


BEGIN
END BenchmarkAux.
