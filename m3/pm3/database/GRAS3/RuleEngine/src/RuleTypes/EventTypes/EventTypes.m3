MODULE EventTypes;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:51  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:06:37  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT EventType, EventTypeSeq, TextIntTransientTbl AS TextIntTbl,
       InternEventType;

VAR
  EventTypeNumbers           := NEW(TextIntTbl.Default).init();
  EventTypes                 := NEW(EventTypeSeq.T).init();
  LastId          : CARDINAL := 0;

PROCEDURE NewEventType (name: TEXT): EventType.T =
  VAR number: INTEGER;
  BEGIN
    IF EventTypeNumbers.get(name, number) THEN
      RETURN EventTypes.get(number);
    ELSE
      INC(LastId);
      EVAL EventTypeNumbers.put(name, LastId - 1);
      WITH type = NEW(EventType.T).init(name) DO
        EventTypes.addhi(type);
        RETURN type;
      END;
    END;
  END NewEventType;

PROCEDURE GetNumberOfTypes(): CARDINAL =
  BEGIN
    RETURN LastId;
  END GetNumberOfTypes;
  
PROCEDURE ExistsName (name: TEXT): BOOLEAN =
  VAR number: INTEGER;
  BEGIN
    RETURN EventTypeNumbers.get(name, number);
  END ExistsName;

PROCEDURE GetNumber (name: TEXT): CARDINAL RAISES {Unknown} =
  VAR number: INTEGER;
  BEGIN
    IF EventTypeNumbers.get(name, number) THEN
      RETURN number + 1;
    ELSE
      RAISE Unknown;
    END;
  END GetNumber;

PROCEDURE Exists (type: CARDINAL): BOOLEAN =
  BEGIN
    RETURN type > 0 AND type <= LastId;
  END Exists;

PROCEDURE Get (type: CARDINAL): EventType.T RAISES {Unknown} =

  BEGIN
    IF type > 0 AND type <= LastId THEN
      RETURN EventTypes.get(type - 1);
    ELSE
      RAISE Unknown;
    END;
  END Get;

BEGIN
END EventTypes.
