MODULE Trigger;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:23  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT EventPattern, Action, ContextSet;

REVEAL
  T = Public BRANDED OBJECT
        pat      : EventPattern.T;
        act      : Action.T;
        coup     : CouplingMode;
        prio     : CARDINAL;
        inh, perm: ContextSet.T;
      OVERRIDES
        pattern    := Pattern;
        action     := GetAction;
        coupling   := Coupling;
        priority   := Priority;
        inhibiting := Inhibiting;
        permitting := Permitting;
        active     := Active;
      END;

PROCEDURE Create (pattern               : EventPattern.T;
                  action                : Action.T;
                  coupling              : CouplingMode;
                  priority              : CARDINAL;
                  inhibiting, permitting: ContextSet.T    ): T =
  BEGIN
    RETURN NEW(T, pat := pattern, act := action, coup := coupling,
               prio := priority, inh := inhibiting, perm := permitting);
  END Create;

PROCEDURE Pattern (trigger: T): EventPattern.T =
  BEGIN
    RETURN trigger.pat;
  END Pattern;

PROCEDURE GetAction (trigger: T): Action.T =
  BEGIN
    RETURN trigger.act;
  END GetAction;

PROCEDURE Coupling (trigger: T): CouplingMode =
  BEGIN
    RETURN trigger.coup;
  END Coupling;

PROCEDURE Priority (trigger: T): CARDINAL =
  BEGIN
    RETURN trigger.prio;
  END Priority;

PROCEDURE Inhibiting (trigger: T): ContextSet.T =
  BEGIN
    RETURN trigger.inh;
  END Inhibiting;

PROCEDURE Permitting (trigger: T): ContextSet.T =
  BEGIN
    RETURN trigger.perm;
  END Permitting;

PROCEDURE Active (trigger: T; context: ContextSet.T): BOOLEAN =
  BEGIN
    RETURN NOT ContextSet.Inhibits(trigger.inh, context)
             AND ContextSet.Permits(trigger.perm, context);
  END Active;

BEGIN
END Trigger.
