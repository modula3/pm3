INTERFACE RemoteActiveAction;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:07  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT Action, Event, ContextSet;

CONST Brand = "RemoteActiveAction";

TYPE
  Struct = RECORD
             priority: PriorityType;
             action  : Action.T;
             event   : Event.T;
             context : ContextSet.T;
             userdata: REFANY;
             next    : T;
           END;

  T = REF Struct;

  PriorityType = RECORD prio, timeStamp: CARDINAL END;

PROCEDURE PrioLess (p1, p2: PriorityType): BOOLEAN;
PROCEDURE Priority (act: T): PriorityType;

END RemoteActiveAction.
