MODULE EventTranslation;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:03  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)


IMPORT IntIntTbl, IntTextTbl;
IMPORT EventPattern, Event, EventTypes, EventType;


PROCEDURE ComposeEvent (typename: TEXT;
                        bools   : IntIntTbl.T;
                        ints    : IntIntTbl.T;
                        texts   : IntTextTbl.T): Event.T
  RAISES {UnknownType, AttributeMismatch} =
  <* FATAL EventTypes.Unknown, EventType.Unknown, EventType.Mismatch *>
  VAR
    event: Event.T;
    type : EventType.T;
    vInt : INTEGER;
    vText: TEXT;
  BEGIN
    IF EventTypes.ExistsName(typename) THEN
      WITH no = EventTypes.GetNumber(typename) DO
        type := EventTypes.Get(no);
        event := NEW(Event.T).init(no);
      END;

      FOR a := 1 TO type.getNumberOfAttributes() DO
        IF type.isBoolAttribute(a) THEN
          IF NOT bools.get(a, vInt) THEN
            RAISE AttributeMismatch;
          ELSE
            event.setBoolAttribute(a, vInt # 0);
          END;
        ELSIF type.isIntAttribute(a) THEN
          IF NOT ints.get(a, vInt) THEN
            RAISE AttributeMismatch;
          ELSE
            event.setIntAttribute(a, vInt);
          END;
        ELSIF type.isTextAttribute(a) THEN
          IF NOT texts.get(a, vText) THEN
            RAISE AttributeMismatch;
          ELSE
            event.setTextAttribute(a, vText);
          END;
        ELSE
          event.setRefAnyAttribute(a, NIL);
        END;
      END;
    ELSE
      RAISE UnknownType;
    END;
    RETURN event;
  END ComposeEvent;

PROCEDURE DecomposeEvent (    event   : Event.T;
                          VAR typename: TEXT;
                          VAR bools   : IntIntTbl.T;
                          VAR ints    : IntIntTbl.T;
                          VAR texts   : IntTextTbl.T) =
  VAR type: EventType.T;
  <* FATAL EventTypes.Unknown, EventType.Unknown, EventType.Mismatch *>
  BEGIN
    WITH no = event.type() DO
      type := EventTypes.Get(no);
      typename := type.getName();
    END;

    bools := NEW(IntIntTbl.Default).init();
    ints := NEW(IntIntTbl.Default).init();
    texts := NEW(IntTextTbl.Default).init();

    FOR a := 1 TO type.getNumberOfAttributes() DO
      IF type.isBoolAttribute(a) THEN
        IF event.getBoolAttribute(a) THEN
          EVAL bools.put(a, 1);
        ELSE
          EVAL bools.put(a, 0);
        END;
      ELSIF type.isIntAttribute(a) THEN
        EVAL ints.put(a, event.getIntAttribute(a));
      ELSIF type.isTextAttribute(a) THEN
        EVAL texts.put(a, event.getTextAttribute(a));
      ELSE
        (* ignore refany attributes *)
      END;
    END;
  END DecomposeEvent;

PROCEDURE ComposePattern (typename : TEXT;
                          bools: IntIntTbl.T;
                          ints : IntIntTbl.T;
                          texts: IntTextTbl.T ): EventPattern.T
  RAISES {UnknownType} =
  <* FATAL EventTypes.Unknown, EventType.Unknown, EventType.Mismatch *>
  VAR
    pattern: EventPattern.T;
    type   : EventType.T;
    vInt   : INTEGER;
    vText  : TEXT;
  BEGIN
    IF EventTypes.ExistsName(typename) THEN
      WITH no = EventTypes.GetNumber(typename) DO
        type := EventTypes.Get(no);
        pattern := NEW(EventPattern.T).init(no);
      END;

      FOR a := 1 TO type.getNumberOfAttributes() DO
        IF type.isBoolAttribute(a) THEN
          IF bools.get(a, vInt) THEN
            pattern.setBoolAttribute(a, vInt # 0);
          (* ELSE suppose pattern attribute is wildcard *)
          END;
        ELSIF type.isIntAttribute(a) THEN
          IF ints.get(a, vInt) THEN
            pattern.setIntAttribute(a, vInt);
          (* ELSE suppose pattern attribute is wildcard *)
          END;
        ELSIF type.isTextAttribute(a) THEN
          IF texts.get(a, vText) THEN
            pattern.setTextAttribute(a, vText);
          (* ELSE suppose pattern attribute is wildcard *)
          END;
        END;
      END;
    ELSE
      RAISE UnknownType;
    END;
    RETURN pattern;
  END ComposePattern;

PROCEDURE DecomposePattern (    pattern : EventPattern.T;
                            VAR typename: TEXT;
                            VAR bools   : IntIntTbl.T;
                            VAR ints    : IntIntTbl.T;
                            VAR texts   : IntTextTbl.T    ) =
  VAR type: EventType.T;
  <* FATAL EventTypes.Unknown, EventType.Unknown, EventType.Mismatch *>
  BEGIN
    WITH no = pattern.type() DO
      type := EventTypes.Get(no);
      typename := type.getName();
    END;

    bools := NEW(IntIntTbl.Default).init();
    ints := NEW(IntIntTbl.Default).init();
    texts := NEW(IntTextTbl.Default).init();

    FOR a := 1 TO type.getNumberOfAttributes() DO
      IF NOT pattern.isWildcard(a) THEN
        IF type.isBoolAttribute(a) THEN
          IF pattern.getBoolAttribute(a) THEN
            EVAL bools.put(a, 1);
          ELSE
            EVAL bools.put(a, 0);
          END;
        ELSIF type.isIntAttribute(a) THEN
          EVAL ints.put(a, pattern.getIntAttribute(a));
        ELSIF type.isTextAttribute(a) THEN
          EVAL texts.put(a, pattern.getTextAttribute(a));
        END;
      END;
    END;
  END DecomposePattern;

BEGIN
END EventTranslation.
