(*!  DATA TYPE MODULE *)
INTERFACE TriggerStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:50  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.2  1998/08/12 11:05:02  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.1  1997/10/31 14:04:03  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

(* A trigger storage stores Triggers and computes activated actions for
   these triggers if an event is raised. *)

IMPORT Trigger, Event, ContextSet, Action;

TYPE

  T = <*TRANSIENT*> ROOT OBJECT
      METHODS
        storeTrigger (t: Trigger.T; userdata: <*TRANSIENT*> REFANY; id: CARDINAL);
                      (* store a trigger in trigger storage using id as a
                         (unique) identifier. *)
        removeTrigger (id: CARDINAL; VAR type: CARDINAL);
                       (* removes trigger with the given id *)

        notifyEvent (e: Event.T; context: ContextSet.T);
        getNextAction (VAR act     : Action.T;
                       VAR coupl   : Trigger.CouplingMode;
                       VAR priority: CARDINAL;
                       VAR userdata: <*TRANSIENT*> REFANY): BOOLEAN;
                       (* notifyEvent computes all activated actions for
                          event e.  These are held internal and can be
                          queried with getNextAction. *)

      END;

  Default <: T OBJECT METHODS init (): Default; END;


END TriggerStorage.
