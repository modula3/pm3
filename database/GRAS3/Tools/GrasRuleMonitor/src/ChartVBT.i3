INTERFACE ChartVBT;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:01  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT RectsVBT;

TYPE
  T <: Public;
  Public =
    RectsVBT.T OBJECT
    METHODS
      init (noOfCharts: CARDINAL; initialheight: CARDINAL := 100): T;
    END;

PROCEDURE SetChart (vbt: T; no, height: CARDINAL);
PROCEDURE SetChartName (vbt: T; no: CARDINAL; name: TEXT);

END ChartVBT.
