(*! DATA TYPE MODULE *)
INTERFACE EventPattern;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:19  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

(* An EventPattern.T or event pattern is built like an event.  It has all
   attributes an event of the same type has.  Additionally these attributes
   can have a wildcard value.  An event e matches an event pattern p, iff

   e.type = p.type \wedge

   \forall_{a \in Attributes(e.type)}: p.isWildcard(a) \vee p.a = e.a .

*)

IMPORT Event, EventType;

TYPE
  T <: Public;

  Public =
    Event.T OBJECT
    METHODS
      init        (type: CARDINAL): T RAISES {EventType.Unknown};
      (* after init, all attributes are wildcards *)
      isWildcard  (index: CARDINAL): BOOLEAN RAISES {EventType.Unknown};
      setWildcard (index: CARDINAL) RAISES {EventType.Unknown};
      match       (e: Event.T): BOOLEAN;
    END;

PROCEDURE Less(ep1, ep2: T): BOOLEAN;
  (* Defines a total order on event patterns *)
  
END EventPattern.
