(*!  DATA OBJECT MODULE *)
INTERFACE LocalRuleHandler;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.2  1998/08/12 11:04:42  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.1  1997/10/31 14:02:57  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling.

*)
(***************************************************************************)

(* The local rule handler is responsible for all locally monitored events.
   It is a collection of event handlers, each responsible for a set of
   event types.  A special event handler, the default handler, is
   responsible for events which no other handler can is responsible for.
   Event handlers may be registered but never unregistered.  They are
   considered part of the program architecture during runtime.  All
   accesses to the local rule handler are mutually exclusive, so that
   threads may concurrently use them without worry. *)

IMPORT CardSeq;
IMPORT Trigger, EventDetector, EventHandler, Event, Action, ContextSet;

(* Initialization and set up*)

PROCEDURE Login ();
  (* Initializes the local data structures *)

PROCEDURE RegisterEventHandler (handler: EventHandler.T; types: CardSeq.T);
  (* Register handler as event handler for the events types in types.  All
     occurring events of these types will be passed to handler after
     calling thsi procedure.  Note: It is not checked whether another
     handler for some of the types already is registered. *)

PROCEDURE RegisterEventDetector (detector: EventDetector.T;
                                 types   : CardSeq.T        );
  (* Register detector as a source for events of the types in types.  A
     detector will be notified if a trigger for any of the events in types
     is registered or unregisterd.  In this way, the detector only needs to
     signal events when triggers are present. *)

PROCEDURE RegisterTransactionUnit (): CARDINAL;

PROCEDURE UnregisterTransactionUnit (unit: CARDINAL);

(* Trigger handling *)

PROCEDURE NextTriggerId (): CARDINAL;
  (* Returns a unique number (during runtime).  This can be used for
     identifying triggers. *)

PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           userdata: REFANY;
                           id      : CARDINAL   );
  (* Registers trigger with the corresponding event handler.  The id
     servers as unique identifier for this trigger. *)

PROCEDURE UnregisterTrigger (id: CARDINAL);
  (* Unregisters trigger with id *)


(* Context handling *)

PROCEDURE ActivateContext (tu: CARDINAL; number: CARDINAL);
  (* Activates context identified by number as declared with ContextSet.
     Only triggers will be fired which inhibiting contexts are all inactive
     and permitting contexts are all active. *)

PROCEDURE DeactivateContext (tu: CARDINAL; number: CARDINAL);
  (* Deactivates context identified by number as declared with
     ContextSet. *)


(* Event handling *)

PROCEDURE Handle (tu: CARDINAL; event: Event.T; level: CARDINAL);
  (* Passes event to its responsible event handler.  The triggered actions
     must be queried with GetNextAction. *)

PROCEDURE GetNextAction (    tu              : CARDINAL;
                             coupling        : Trigger.CouplingMode;
                         VAR event           : Event.T;
                         VAR context         : ContextSet.T;
                         VAR transactionLevel: CARDINAL;
                         VAR action          : Action.T;
                         VAR userdata        : REFANY                ):
  BOOLEAN;
  (* Returns TRUE if actions are triggered.  action then contains the
     actions triggered in mode coupling with highest priority.  If coupling
     is deferred, all actions up to transaction level 'level' will be
     reported. *)

PROCEDURE KillClientActions (client: CARDINAL);
  (* Kill all activated actions in all event handlers that belong to
     client *)

PROCEDURE KillTransaction (tu: CARDINAL; level: CARDINAL);
  (* Kill all activated actions that were triggered in transaction at level
     'level' or deeper. *)

END LocalRuleHandler.
