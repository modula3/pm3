MODULE Event;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:17  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT EventType, InternEventType, EventTypes;

TYPE
  BoolArray = REF ARRAY OF BOOLEAN;
  IntArray = REF ARRAY OF INTEGER;
  TextArray = REF ARRAY OF TEXT;
  RefAnyArray = REF ARRAY OF REFANY;

REVEAL
  T = Public BRANDED OBJECT
        typ  : CARDINAL;
        bools: BoolArray   := NIL;
        ints : IntArray    := NIL;
        texts: TextArray   := NIL;
        refs : RefAnyArray := NIL;
      OVERRIDES
        init               := Init;
        type               := Type;
        setBoolAttribute   := SetBoolAttribute;
        setIntAttribute    := SetIntAttribute;
        setTextAttribute   := SetTextAttribute;
        setRefAnyAttribute := SetRefAnyAttribute;
        getBoolAttribute   := GetBoolAttribute;
        getIntAttribute    := GetIntAttribute;
        getTextAttribute   := GetTextAttribute;
        getRefAnyAttribute := GetRefAnyAttribute;
      END;

PROCEDURE Init (event: T; type: CARDINAL): T RAISES {EventType.Unknown} =
  VAR etype: EventType.T;
  BEGIN
    event.typ := type;
    TRY
      etype := EventTypes.Get(type);
      IF etype.initializing THEN
        <* ASSERT FALSE *>
      END;
    EXCEPT
      EventTypes.Unknown => RAISE EventType.Unknown;
    END;

    (* initialize attribute storages *)
    WITH num = etype.typedIdx[InternEventType.AttributeType.Bool] DO
      IF num > 0 THEN event.bools := NEW(BoolArray, num); END;
    END;
    WITH num = etype.typedIdx[InternEventType.AttributeType.Int] DO
      IF num > 0 THEN event.ints := NEW(IntArray, num); END;
    END;
    WITH num = etype.typedIdx[InternEventType.AttributeType.Text] DO
      IF num > 0 THEN event.texts := NEW(TextArray, num); END;
    END;
    WITH num = etype.typedIdx[InternEventType.AttributeType.RefAny] DO
      IF num > 0 THEN event.refs := NEW(RefAnyArray, num); END;
    END;
    RETURN event;
  END Init;

PROCEDURE Type (event: T; ): CARDINAL =
  BEGIN
    RETURN event.typ;
  END Type;

PROCEDURE SetBoolAttribute (event: T; index: CARDINAL; val: BOOLEAN)
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.Bool, typedIdx);
    event.bools^[typedIdx] := val;
  END SetBoolAttribute;

PROCEDURE SetIntAttribute (event: T; index: CARDINAL; val: INTEGER)
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.Int, typedIdx);
    event.ints^[typedIdx] := val;
  END SetIntAttribute;

PROCEDURE SetTextAttribute (event: T; index: CARDINAL; val: TEXT)
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.Text, typedIdx);
    event.texts^[typedIdx] := val;
  END SetTextAttribute;

PROCEDURE SetRefAnyAttribute (event: T; index: CARDINAL; val: REFANY)
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.RefAny, typedIdx);
    event.refs^[typedIdx] := val;
  END SetRefAnyAttribute;


PROCEDURE GetBoolAttribute (event: T; index: CARDINAL): BOOLEAN
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.Bool, typedIdx);
    RETURN event.bools^[typedIdx];
  END GetBoolAttribute;

PROCEDURE GetIntAttribute (event: T; index: CARDINAL): INTEGER
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.Int, typedIdx);
    RETURN event.ints^[typedIdx];
  END GetIntAttribute;

PROCEDURE GetTextAttribute (event: T; index: CARDINAL): TEXT
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.Text, typedIdx);
    RETURN event.texts^[typedIdx];
  END GetTextAttribute;

PROCEDURE GetRefAnyAttribute (event: T; index: CARDINAL): REFANY
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR typedIdx: CARDINAL;
  BEGIN
    CheckAttribute(
      event.typ, index, InternEventType.AttributeType.RefAny, typedIdx);
    RETURN event.refs^[typedIdx];
  END GetRefAnyAttribute;


PROCEDURE CheckAttribute (    type    : CARDINAL;
                              index   : CARDINAL;
                              atype   : InternEventType.AttributeType;
                          VAR typedIdx: CARDINAL                       )
  RAISES {EventType.Unknown, EventType.Mismatch} =
  VAR etype: EventType.T;
  BEGIN
    TRY
      etype := EventTypes.Get(type);
      IF index > 0 AND index <= etype.attrIdx THEN
        IF atype = VAL(etype.attrType.get(index - 1),
                       InternEventType.AttributeType) THEN
          typedIdx := etype.typedAttrIdx.get(index - 1);
        ELSE
          RAISE EventType.Mismatch;
        END;
      ELSE
        RAISE EventType.Unknown;
      END;
    EXCEPT
      EventTypes.Unknown => RAISE EventType.Unknown;
    END;
  END CheckAttribute;

BEGIN
END Event.
