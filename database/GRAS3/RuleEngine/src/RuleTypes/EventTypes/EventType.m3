MODULE EventType EXPORTS EventType, InternEventType;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.3  1997/12/15 16:33:00  roland
    Bugfix: initialize result to avoid out of range error.

    Revision 1.2  1997/11/07 09:07:18  roland
    Methods added to provide readable information on event types.

    Revision 1.1  1997/10/31 14:06:35  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT TextIntTbl, IntSeq, TextSeq;

REVEAL
  T = Intern BRANDED OBJECT
        name    : TEXT;
        info    : TEXT;
        attrIndx: TextIntTbl.T;
        attrName: TextSeq.T;
      OVERRIDES
        init                  := Init;
        addBoolAttribute      := AddBoolAttribute;
        addIntAttribute       := AddIntAttribute;
        addTextAttribute      := AddTextAttribute;
        addRefAnyAttribute    := AddRefAnyAttribute;
        addInfo               := AddInfo;
        finishInitialization  := FinishInitialization;
        getName               := GetName;
        getNumberOfAttributes := GetNumberOfAttributes;
        getInfo               := GetInfo;
        getAttributeName      := GetAttributeName;
        isBoolAttribute       := IsBoolAttribute;
        isIntAttribute        := IsIntAttribute;
        isTextAttribute       := IsTextAttribute;
        isRefAnyAttribute     := IsRefAnyAttribute;
        getAttributeIndex     := GetAttributeIndex;
      END;

PROCEDURE Init (type: T; name: TEXT): T =
  BEGIN
    type.name := name;
    type.attrIndx := NEW(TextIntTbl.Default).init();
    type.attrName := NEW(TextSeq.T).init();
    type.attrType := NEW(IntSeq.T).init();
    type.typedAttrIdx := NEW(IntSeq.T).init();
    RETURN type;
  END Init;

PROCEDURE AddBoolAttribute (type: T; attrname: TEXT): CARDINAL
  RAISES {Mismatch} =
  VAR
    exists     : BOOLEAN;
    indx, tindx: CARDINAL;
  BEGIN
    <* ASSERT type.initializing *>
    AttributeIndex(type, attrname, exists, indx);
    IF exists THEN
      IF GetAttributeType(type, indx, tindx) # AttributeType.Bool THEN
        RAISE Mismatch;
      END;
      RETURN indx;
    ELSE
      indx := NewAttribute(type, attrname, AttributeType.Bool);
      RETURN indx;
    END;
  END AddBoolAttribute;

PROCEDURE AddIntAttribute (type: T; attrname: TEXT): CARDINAL
  RAISES {Mismatch} =
  VAR
    exists     : BOOLEAN;
    indx, tindx: CARDINAL;
  BEGIN
    <* ASSERT type.initializing *>
    AttributeIndex(type, attrname, exists, indx);
    IF exists THEN
      IF GetAttributeType(type, indx, tindx) # AttributeType.Int THEN
        RAISE Mismatch;
      END;
      RETURN indx;
    ELSE
      indx := NewAttribute(type, attrname, AttributeType.Int);
      RETURN indx;
    END;
  END AddIntAttribute;

PROCEDURE AddTextAttribute (type: T; attrname: TEXT): CARDINAL
  RAISES {Mismatch} =
  VAR
    exists     : BOOLEAN;
    indx, tindx: CARDINAL;
  BEGIN
    <* ASSERT type.initializing *>
    AttributeIndex(type, attrname, exists, indx);
    IF exists THEN
      IF GetAttributeType(type, indx, tindx) # AttributeType.Text THEN
        RAISE Mismatch;
      END;
      RETURN indx;
    ELSE
      indx := NewAttribute(type, attrname, AttributeType.Text);
      RETURN indx;
    END;
  END AddTextAttribute;

PROCEDURE AddRefAnyAttribute (type: T; attrname: TEXT): CARDINAL
  RAISES {Mismatch} =
  VAR
    exists     : BOOLEAN;
    indx, tindx: CARDINAL;
  BEGIN
    <* ASSERT type.initializing *>
    AttributeIndex(type, attrname, exists, indx);
    IF exists THEN
      IF GetAttributeType(type, indx, tindx) # AttributeType.RefAny THEN
        RAISE Mismatch;
      END;
      RETURN indx;
    ELSE
      indx := NewAttribute(type, attrname, AttributeType.RefAny);
      RETURN indx;
    END;
  END AddRefAnyAttribute;

PROCEDURE AddInfo (type: T; info: TEXT) =
  BEGIN
    <* ASSERT type.initializing *>
    IF type.info = NIL THEN
      type.info := info;
    ELSIF info # NIL THEN
      type.info := type.info & info;
    END;
  END AddInfo; 

PROCEDURE FinishInitialization (type: T) =
  BEGIN
    type.initializing := FALSE;
  END FinishInitialization;

PROCEDURE GetName (type: T): TEXT =
  BEGIN
    RETURN type.name;
  END GetName;

PROCEDURE GetNumberOfAttributes (type: T): CARDINAL =
  BEGIN
    RETURN type.attrIdx;
  END GetNumberOfAttributes;

PROCEDURE GetInfo(type: T): TEXT =
  BEGIN
    RETURN type.info;
  END GetInfo;

PROCEDURE GetAttributeName (type: T; index: CARDINAL): TEXT
  RAISES {Unknown} =
  BEGIN
    IF index > 0 AND index <= type.attrIdx THEN
      RETURN type.attrName.get(index - 1);
    ELSE
      RAISE Unknown;
    END;
  END GetAttributeName;

PROCEDURE IsBoolAttribute (type: T; index: CARDINAL): BOOLEAN
  RAISES {Unknown} =
  VAR tidx: CARDINAL;
  BEGIN
    IF index > 0 AND index <= type.attrIdx THEN
      RETURN GetAttributeType(type, index, tidx) = AttributeType.Bool;
    ELSE
      RAISE Unknown;
    END;
  END IsBoolAttribute;

PROCEDURE IsIntAttribute (type: T; index: CARDINAL): BOOLEAN
  RAISES {Unknown} =
  VAR tidx: CARDINAL;
  BEGIN
    IF index > 0 AND index <= type.attrIdx THEN
      RETURN GetAttributeType(type, index, tidx) = AttributeType.Int;
    ELSE
      RAISE Unknown;
    END;
  END IsIntAttribute;

PROCEDURE IsTextAttribute (type: T; index: CARDINAL): BOOLEAN
  RAISES {Unknown} =
  VAR tidx: CARDINAL;
  BEGIN
    IF index > 0 AND index <= type.attrIdx THEN
      RETURN GetAttributeType(type, index, tidx) = AttributeType.Text;
    ELSE
      RAISE Unknown;
    END;
  END IsTextAttribute;

PROCEDURE IsRefAnyAttribute (type: T; index: CARDINAL): BOOLEAN
  RAISES {Unknown} =
  VAR tidx: CARDINAL;
  BEGIN
    IF index > 0 AND index <= type.attrIdx THEN
      RETURN GetAttributeType(type, index, tidx) = AttributeType.RefAny;
    ELSE
      RAISE Unknown;
    END;
  END IsRefAnyAttribute;

PROCEDURE GetAttributeIndex (type: T; attrname: TEXT): CARDINAL
  RAISES {Unknown} =
  VAR indx: INTEGER;
  BEGIN
    IF NOT type.attrIndx.get(attrname, indx) THEN RAISE Unknown; END;
    RETURN indx + 1;
  END GetAttributeIndex; 

(* internal procedures *)

PROCEDURE NewAttribute (type: T; name: TEXT; atype: AttributeType):
  CARDINAL =
  BEGIN
    (* reserve new index values *)
    INC(type.typedIdx[atype]);
    INC(type.attrIdx);

    (* store name, type, and index information *)
    type.attrName.addhi(name);
    EVAL type.attrIndx.put(name, type.attrIdx - 1);
    type.attrType.addhi(ORD(atype));
    type.typedAttrIdx.addhi(type.typedIdx[atype] - 1);

    RETURN type.attrIdx;
  END NewAttribute;

PROCEDURE AttributeIndex (    type  : T;
                              name  : TEXT;
                          VAR exists: BOOLEAN;
                          VAR indx  : CARDINAL ) =
  VAR i: INTEGER := 0;
  BEGIN
    exists := type.attrIndx.get(name, i);
    IF exists THEN
      indx := i + 1;
    END;
  END AttributeIndex;

PROCEDURE GetAttributeType (type: T; index: CARDINAL; VAR tindx: CARDINAL):
  AttributeType =
  BEGIN
    tindx := type.typedAttrIdx.get(index - 1);
    RETURN VAL(type.attrType.get(index - 1), AttributeType);
  END GetAttributeType;

BEGIN
END EventType.
