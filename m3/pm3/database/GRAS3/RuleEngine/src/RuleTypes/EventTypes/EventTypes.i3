(*! DATA OBJECT MODULE *)
INTERFACE EventTypes;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:37  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT EventType;

(* A storage for event types.  Each type gets a number as id. ids run
   from 1 to GetNumberOfTypes(). *)

PROCEDURE NewEventType (name: TEXT): EventType.T;
PROCEDURE GetNumberOfTypes(): CARDINAL;

PROCEDURE ExistsName (name: TEXT): BOOLEAN;
PROCEDURE GetNumber (name: TEXT): CARDINAL RAISES {Unknown};

PROCEDURE Exists(type: CARDINAL): BOOLEAN;
PROCEDURE Get (type: CARDINAL): EventType.T RAISES {Unknown};

EXCEPTION Unknown;

END EventTypes.
