(*!  DATA TYPE MODULE *)
INTERFACE InternEventType;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:38  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT EventType, IntSeq;

TYPE AttributeType = {Bool, Int, Text, RefAny};

REVEAL EventType.T <: Intern;

(* All external attribute indices start with 1.  Internally they start all
   with 0 to be able to use sequences. *)

TYPE
  Intern =
    EventType.Public OBJECT
      initializing: BOOLEAN := TRUE;

      attrIdx: CARDINAL;         (* next attribute index *)
      attrType: IntSeq.T;
      (* maps attribute indices to the type of the referenced attribute *)

      typedIdx := ARRAY AttributeType OF CARDINAL{0, ..}; (* next internal
                                                             attribute
                                                             index for
                                                             specific
                                                             type *)
      typedAttrIdx: IntSeq.T;
      (* maps attribute numbers to the indices in the typed sequences of an
         event instance. *)
    METHODS
      init (name: TEXT): EventType.T;
    END;

END InternEventType.
