(*!  DATA TYPE MODULE *)
INTERFACE Trigger;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:22  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

(* A Trigger.T is a rule of the rule engine.  It consists of an event
   pattern denoting when the trigger should fire and an action that is
   executed, when it fires.  Triggers are active only if all their
   permitting contexts are active and all their inhibiting contexts are
   inactive.  Not that a trigger can never be active if its inhibiting and
   permitting contexts are not disjunct!  The coupling mode of a trigger
   determines when the action of a fired trigger is executed.  If more than
   one trigger can fire, their order is determined by their pritorities.
   (Higher values fire first) *)

IMPORT EventPattern, Action, ContextSet;

TYPE
  CouplingMode = {Immediate, Deferred, Decoupled};
    (* Immediate means no delay between event detection and action
       execution.  Deferred actions are executed at the end of the current
       transaction.  Decoupled actions run in a separate transaction. *)


  T <: Public;

  Public = OBJECT
            METHODS
              pattern    (): EventPattern.T;
              action     (): Action.T;
              coupling   (): CouplingMode;
              priority   (): CARDINAL;
              inhibiting (): ContextSet.T;
              permitting (): ContextSet.T;
              active     (context: ContextSet.T): BOOLEAN;
            END;

PROCEDURE Create (pattern               : EventPattern.T;
                  action                : Action.T;
                  coupling              : CouplingMode;
                  priority              : CARDINAL;
                  inhibiting, permitting: ContextSet.T    ): T;


END Trigger.
