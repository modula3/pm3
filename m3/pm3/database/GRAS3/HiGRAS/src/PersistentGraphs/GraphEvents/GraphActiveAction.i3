INTERFACE GraphActiveAction;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:23:53  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT Action, Event, ContextSet;

CONST Brand = "ActiveAction";

TYPE
  Struct =
    RECORD
      priority              : PriorityType;
      action                : Action.T;
      depsFirst, depsTarg   : BOOLEAN;
      event                 : Event.T;
      context               : ContextSet.T;
      userdata              : REFANY;
      level                 : CARDINAL;
      deleted               : BOOLEAN;
      nextClient, prevClient: T;
      nextLevel, prevLevel  : T;
    END;

  T = REF Struct;

  PriorityType = RECORD prio, timeStamp: CARDINAL END;

PROCEDURE PrioLess (p1, p2: PriorityType): BOOLEAN;
PROCEDURE Priority (act: T): PriorityType;

END GraphActiveAction.
