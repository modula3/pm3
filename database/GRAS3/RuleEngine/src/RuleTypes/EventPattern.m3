MODULE EventPattern;

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

    Revision 1.1  1997/10/31 14:06:21  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT Text;
IMPORT EventType, Event, EventTypes;

TYPE BoolArray = <*TRANSIENT*> REF ARRAY OF BOOLEAN;

REVEAL
  T = Public BRANDED OBJECT
        wildcards: BoolArray;
      OVERRIDES
        isWildcard         := IsWildcard;
        setWildcard        := SetWildcard;
        init               := InitPattern;
        match              := Match;
        setBoolAttribute   := PatternSetBoolAttribute;
        setIntAttribute    := PatternSetIntAttribute;
        setTextAttribute   := PatternSetTextAttribute;
        setRefAnyAttribute := PatternSetRefAnyAttribute;
      END;

PROCEDURE InitPattern (pattern: T; type: CARDINAL): T
  RAISES {EventType.Unknown} =
  (* Already checked by Event.T.init: *)
  VAR etype: EventType.T;
  <* FATAL EventTypes.Unknown *>
  BEGIN
    EVAL Event.T.init(pattern, type);
    etype := EventTypes.Get(type);
    (* initialize wildcard storage *)
    WITH num = etype.getNumberOfAttributes() DO
      IF num > 0 THEN pattern.wildcards := NEW(BoolArray, num); END;
      FOR i := 0 TO LAST(pattern.wildcards^) DO
        pattern.wildcards^[i] := TRUE;
      END;
    END;
    RETURN pattern;
  END InitPattern;

PROCEDURE IsWildcard (pattern: T; index: CARDINAL): BOOLEAN
  RAISES {EventType.Unknown} =
  BEGIN
    IF index > 0 AND index <= NUMBER(pattern.wildcards^) THEN
      RETURN pattern.wildcards^[index - 1];
    ELSE
      RAISE EventType.Unknown;
    END;
  END IsWildcard;

PROCEDURE SetWildcard (pattern: T; index: CARDINAL)
  RAISES {EventType.Unknown} =
  BEGIN
    IF index > 0 AND index <= NUMBER(pattern.wildcards^) THEN
      pattern.wildcards^[index - 1] := TRUE;
    ELSE
      RAISE EventType.Unknown;
    END;
  END SetWildcard;

PROCEDURE PatternSetBoolAttribute (pattern: T;
                                   index  : CARDINAL;
                                   val    : BOOLEAN   )
  RAISES {EventType.Unknown, EventType.Mismatch} =
  BEGIN
    Event.T.setBoolAttribute(pattern, index, val);
    pattern.wildcards^[index - 1] := FALSE;
  END PatternSetBoolAttribute;

PROCEDURE PatternSetIntAttribute (pattern: T; index: CARDINAL; val: INTEGER)
  RAISES {EventType.Unknown, EventType.Mismatch} =
  BEGIN
    Event.T.setIntAttribute(pattern, index, val);
    pattern.wildcards^[index - 1] := FALSE;
  END PatternSetIntAttribute;

PROCEDURE PatternSetTextAttribute (pattern: T; index: CARDINAL; val: TEXT)
  RAISES {EventType.Unknown, EventType.Mismatch} =
  BEGIN
    Event.T.setTextAttribute(pattern, index, val);
    pattern.wildcards^[index - 1] := FALSE;
  END PatternSetTextAttribute;

PROCEDURE PatternSetRefAnyAttribute (pattern: T;
                                     index  : CARDINAL;
                                     val    : REFANY    )
  RAISES {EventType.Unknown, EventType.Mismatch} =
  BEGIN
    Event.T.setRefAnyAttribute(pattern, index, val);
    pattern.wildcards^[index - 1] := FALSE;
  END PatternSetRefAnyAttribute;

PROCEDURE Match (pattern: T; event: Event.T): BOOLEAN =
  VAR etype: EventType.T;
  <* FATAL EventTypes.Unknown, EventType.Mismatch, EventType.Unknown *>
  BEGIN
    IF pattern.type() = event.type() THEN
      etype := EventTypes.Get(pattern.type());
      FOR i := 1 TO etype.getNumberOfAttributes() DO
        IF NOT pattern.wildcards^[i - 1] THEN
          IF etype.isBoolAttribute(i) THEN
            IF pattern.getBoolAttribute(i) # event.getBoolAttribute(i) THEN
              RETURN FALSE;
            END;
          ELSIF etype.isIntAttribute(i) THEN
            IF pattern.getIntAttribute(i) # event.getIntAttribute(i) THEN
              RETURN FALSE;
            END;
          ELSIF etype.isTextAttribute(i) THEN
            IF NOT Text.Equal(pattern.getTextAttribute(i),
                              event.getTextAttribute(i)) THEN
              RETURN FALSE;
            END;
          ELSE
            IF pattern.getRefAnyAttribute(i) # event.getRefAnyAttribute(i) THEN
              RETURN FALSE;
            END;
          END;
        END;
      END;
      (* the loop ran through, no mismatch found *)
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Match;

PROCEDURE Less (ep1, ep2: T): BOOLEAN =
  (* ordered as tuple (type, a1, ..., an).  wildcards preceede every other
     value. *)
  VAR etype: EventType.T;
  <* FATAL EventTypes.Unknown, EventType.Mismatch, EventType.Unknown *>
  BEGIN
    IF ep1.type() < ep2.type() THEN
      RETURN TRUE;
    ELSIF ep1.type() > ep2.type() THEN
      RETURN FALSE;
    ELSE
      (* ep1.type() = ep2.type() *)
      etype := EventTypes.Get(ep1.type());
      FOR i := 1 TO etype.getNumberOfAttributes() DO
        IF ep1.wildcards^[i - 1] AND NOT ep2.wildcards^[i - 1] THEN
          (* ep1's wildcard preceeds any value in ep1 *)
          RETURN TRUE;
        ELSIF NOT ep1.wildcards^[i - 1] AND ep2.wildcards^[i - 1] THEN
          (* ep2's wildcard preceeds any value in ep1 *)
          RETURN FALSE
        ELSIF NOT ep1.wildcards^[i - 1] AND NOT ep2.wildcards^[i - 1] THEN
          (* both patterns have real values for this attribute *)
          IF etype.isBoolAttribute(i) THEN
            IF ep1.getBoolAttribute(i) < ep2.getBoolAttribute(i) THEN
              RETURN TRUE;
            ELSIF ep1.getBoolAttribute(i) > ep2.getBoolAttribute(i) THEN
              RETURN FALSE;
            END;
          ELSIF etype.isIntAttribute(i) THEN
            IF ep1.getIntAttribute(i) < ep2.getIntAttribute(i) THEN
              RETURN TRUE;
            ELSIF ep1.getIntAttribute(i) > ep2.getIntAttribute(i) THEN
              RETURN FALSE;
            END;
          ELSIF etype.isTextAttribute(i) THEN
            WITH comp = Text.Compare(
                          ep1.getTextAttribute(i), ep2.getTextAttribute(i)) DO
              IF comp < 0 THEN
                RETURN TRUE;
              ELSIF comp > 0 THEN
                RETURN FALSE;
              END;
            END;
          ELSE
            (* comparing ref anys makes no sense, because garbage
               collection may alter their addresses *)
            RETURN FALSE;
          END;
        END;
      END;
      (* the loop ran through, no difference found *)
      RETURN FALSE;
    END;
  END Less;

BEGIN
END EventPattern.
