(*!  DATA TYPE MODULE *)
INTERFACE EventHandler;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.2  1998/08/12 11:04:59  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.1  1997/10/31 14:03:59  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

(* An EventHandler.T is a storage for triggers with certain EventTypes.  It
   uses a TriggerStorage.T to store these triggers and an
   ActivatedActions.T to store the actions activated by an event as
   computed by the trigger storage. *)

IMPORT Trigger, TriggerStorage, Action, Event, ContextSet;

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (ts: TriggerStorage.T): T;
            (* Initialize the event handler to use ts as trigger storage. *)

      newTransactionUnit (tu: CARDINAL);
      delTransactionUnit (tu: CARDINAL);
      
      storeTrigger (t: Trigger.T; userdata: REFANY; id: CARDINAL);
                    (* store a trigger in trigger storage using id as a
                       (unique) identifier. *)
      removeTrigger (id: CARDINAL; VAR type: CARDINAL);
                     (* removes trigger with the given id *)

      handle (tu              : CARDINAL;
              event           : Event.T;
              context         : ContextSet.T;
              transactionLevel: CARDINAL      );
              (* Compute all actions for event e.  Actions are stored in
                 the activated action storage associated with tu. *)
      getNextAction (    tu              : CARDINAL;
                         coupling        : Trigger.CouplingMode;
                     VAR event           : Event.T;
                     VAR context         : ContextSet.T;
                     VAR transactionLevel: CARDINAL;
                     VAR action          : Action.T;
                     VAR userdata        : REFANY                ):
                     BOOLEAN;
                     (* Queries activated action storage of tu for activated
                        actions of the respective coupling mode. *)

      highestPriority (tu: CARDINAL; coupling: Trigger.CouplingMode):
                       CARDINAL;
                       (* Returns the highest priority of an activated
                          action.  level is the transaction level up which
                          deferred actions should be regarded. *)

      hasActivatedActions (tu: CARDINAL; coupling: Trigger.CouplingMode):
                           BOOLEAN;

      killClientActions (client: CARDINAL);
                         (* Remove all actions for client in activated
                            action storage *)

      killTransaction (tu: CARDINAL; level: CARDINAL);
                       (* Remove all actions which were triggered in a
                          transaction with 'level' or deeper. *)
    END;

CONST Brand = "EventHandler";

END EventHandler.
