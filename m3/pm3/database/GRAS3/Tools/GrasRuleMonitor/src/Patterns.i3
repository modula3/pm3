INTERFACE Patterns;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:12  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT EventPattern;

PROCEDURE Init ();
  
PROCEDURE Get(type: CARDINAL): EventPattern.T;
PROCEDURE Set(type: CARDINAL; pattern: EventPattern.T);

END Patterns.
