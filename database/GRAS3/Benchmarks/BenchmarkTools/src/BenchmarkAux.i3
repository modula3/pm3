INTERFACE BenchmarkAux;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.2  1997/02/20 16:08:12  roland
    OO1 rewritten with graphical user interface.

    Revision 1.1  1996/10/08 10:37:28  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT BenchmarkLog, Thread;

(*
 | --- auxiliary procedures for benchmark ---------------------------------
 *)
PROCEDURE ClearSystemCaches (l: BenchmarkLog.T := NIL) RAISES {Thread.Alerted};

END BenchmarkAux.
