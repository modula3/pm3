INTERFACE PanelHandling;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1997/02/20 16:08:53  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT FormsVBT, BenchmarkLog;

(* Updates panel according to OO1Params and attaches event handling
   procedures for OO1 control panel.  Exits when FormsVBT complains about
   forms. *)
PROCEDURE CreatePanel (log: BenchmarkLog.T): FormsVBT.T RAISES {};

END PanelHandling.
