INTERFACE ChartView;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.2  1997/11/07 08:58:03  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

    Revision 1.1  1997/10/31 14:28:26  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT ChartVBT;

PROCEDURE Open(no: CARDINAL; notifyClose: PROCEDURE()): ChartVBT.T;

END ChartView.
