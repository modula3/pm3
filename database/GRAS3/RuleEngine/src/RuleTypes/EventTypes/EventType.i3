(*!  DATA TYPE MODULE *)
INTERFACE EventType;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.2  1997/11/07 09:07:17  roland
    Methods added to provide readable information on event types.

    Revision 1.1  1997/10/31 14:06:34  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

(* An EventType.T has a name and defines the attribute names and types of
   the its instances.  Instances are of type Event.T.  Attributes might be
   reference by name or by index.  The index of an attribte depends on the
   initialization order of an EventType.  Each addXAttribute method returns
   the index of the new attribute. *)

(* Use interface EventTypes to create new event types *)

CONST Brand = "EventType";

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      (* addXAttribute declares a (new) attribute with name attrname and
         type X for the event type.  They return the index of the
         attribute.  Mismatch is raised, trying to declare an already known
         attribute with a different type. *)
      addBoolAttribute   (attrname: TEXT): CARDINAL RAISES {Mismatch};
      addIntAttribute    (attrname: TEXT): CARDINAL RAISES {Mismatch};
      addTextAttribute   (attrname: TEXT): CARDINAL RAISES {Mismatch};
      addRefAnyAttribute (attrname: TEXT): CARDINAL RAISES {Mismatch};

      (* addInfo adds informational text to the event type.  All info texts
         are concatenated and returned when calling getInfo. *)
      addInfo (info: TEXT);

      (* After creation of an event type, no events can be created
         conforming to this type (an attempt leads to a checked runtime
         error).  Only after a call to finishInitialization(), instances
         might be created.  On the other hand, calling addXAttribute after
         finishInitialization also yields a checked runtime error. *)
      finishInitialization ();

      getName               (): TEXT;
      getNumberOfAttributes (): CARDINAL;
      getInfo               (): TEXT;

      (* Queries on attribute names, types, and indices. *)
      getAttributeName (index: CARDINAL): TEXT RAISES {Unknown};

      isBoolAttribute   (index: CARDINAL): BOOLEAN RAISES {Unknown};
      isIntAttribute    (index: CARDINAL): BOOLEAN RAISES {Unknown};
      isTextAttribute   (index: CARDINAL): BOOLEAN RAISES {Unknown};
      isRefAnyAttribute (index: CARDINAL): BOOLEAN RAISES {Unknown};

      getAttributeIndex (attrname: TEXT): CARDINAL RAISES {Unknown};
    END;

EXCEPTION
  Mismatch;
  Unknown;

END EventType.
