INTERFACE LogView;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1997/02/20 16:08:41  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT BenchmarkLog;

PROCEDURE Open(log: BenchmarkLog.T);
PROCEDURE Close(log: BenchmarkLog.T);

END LogView.
