(*!  DATA TYPE MODULE *)
INTERFACE Event;

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

    Revision 1.1  1997/10/31 14:06:17  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT EventType;

TYPE
  T <: Public;

  Public =
    <*TRANSIENT*> ROOT OBJECT
    METHODS
      init (type: CARDINAL): T RAISES {EventType.Unknown};
            (* The type number is resolved with EventTypes.Get *)

      type (): CARDINAL;

      setBoolAttribute (index: CARDINAL; val: BOOLEAN)
                        RAISES {EventType.Unknown, EventType.Mismatch};
      setIntAttribute (index: CARDINAL; val: INTEGER)
                       RAISES {EventType.Unknown, EventType.Mismatch};
      setTextAttribute (index: CARDINAL; val: TEXT)
                        RAISES {EventType.Unknown, EventType.Mismatch};
      setRefAnyAttribute (index: CARDINAL; val: REFANY)
                          RAISES {EventType.Unknown, EventType.Mismatch};

      getBoolAttribute (index: CARDINAL): BOOLEAN
                        RAISES {EventType.Unknown, EventType.Mismatch};
      getIntAttribute (index: CARDINAL): INTEGER
                       RAISES {EventType.Unknown, EventType.Mismatch};
      getTextAttribute (index: CARDINAL): TEXT
                        RAISES {EventType.Unknown, EventType.Mismatch};
      getRefAnyAttribute (index: CARDINAL): REFANY
                          RAISES {EventType.Unknown, EventType.Mismatch};
    END;

END Event.
