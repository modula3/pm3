MODULE IntegerAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:01  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:35:12  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT AttributeValue;

REVEAL

  T = Public BRANDED OBJECT
        val: INTEGER := 0;
      OVERRIDES
        get      := IntegerGet;
        set      := IntegerSet;
        toText   := IntegerToTextEnc;
        fromText := IntegerFromTextEnc;
        type     := IntegerType;
      END;

PROCEDURE IntegerGet (av: T): INTEGER =
  BEGIN
    RETURN av.val;
  END IntegerGet;

PROCEDURE IntegerSet (av: T; val: INTEGER) =
  BEGIN
    av.val := val;
    av.defined := TRUE;
  END IntegerSet;

PROCEDURE IntegerType (<* UNUSED *> av: T): CARDINAL =
  BEGIN
    RETURN AttributeValue.IntegerTypeCode;
  END IntegerType;

PROCEDURE IntegerToTextEnc (av: T; VAR len: CARDINAL): TEXT =
  BEGIN
    RETURN
      AttributeValue.StandardEncoding(
        AttributeValue.IntegerTypeCode, AttributeValue.IntToText(av.val), len);
  END IntegerToTextEnc;

PROCEDURE IntegerFromTextEnc (av: T; enc: TEXT)
  RAISES {AttributeValue.Invalid} =
  VAR
    type, len: CARDINAL;
    text     : TEXT;
  BEGIN
    AttributeValue.StandardDecoding(enc, type, len, text);
    IF type # AttributeValue.IntegerTypeCode OR len # 4 THEN
      RAISE AttributeValue.Invalid
    END;
    av.val := AttributeValue.TextToInt(text);
    av.defined := TRUE;
  END IntegerFromTextEnc;

BEGIN
END IntegerAttributeValue.
