MODULE GraphActiveAction;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:23:55  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

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
END GraphActiveAction. 
