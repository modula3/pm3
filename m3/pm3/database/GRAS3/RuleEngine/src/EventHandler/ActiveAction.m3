MODULE ActiveAction;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:03:58  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

PROCEDURE PrioLess(p1, p2: PriorityType): BOOLEAN =
  BEGIN
    IF p1.prio < p2.prio THEN
      RETURN TRUE;
    ELSIF p2.prio < p1.prio THEN
      RETURN FALSE;
    ELSE
      RETURN p1.timeStamp > p2.timeStamp;
    END;
  END PrioLess;

PROCEDURE Priority(act: T): PriorityType =
  BEGIN
    RETURN act.priority;
  END Priority; 

BEGIN
END ActiveAction.
