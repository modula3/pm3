MODULE CardinalAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:56  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:35:05  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT AttributeValue;

REVEAL

  T = Public BRANDED OBJECT
        val: CARDINAL := 0;
      OVERRIDES
        get      := CardinalGet;
        set      := CardinalSet;
        toText   := CardinalToTextEnc;
        fromText := CardinalFromTextEnc;
        type     := CardinalType;
      END;

PROCEDURE CardinalGet (av: T): CARDINAL =
  BEGIN
    RETURN av.val;
  END CardinalGet;

PROCEDURE CardinalSet (av: T; val: CARDINAL) =
  BEGIN
    av.val := val;
    av.defined := TRUE;
  END CardinalSet;

PROCEDURE CardinalType (<* UNUSED *> av: T): CARDINAL =
  BEGIN
    RETURN AttributeValue.CardinalTypeCode;
  END CardinalType;

PROCEDURE CardinalToTextEnc (av: T; VAR len: CARDINAL): TEXT =
  BEGIN
    RETURN AttributeValue.StandardEncoding(
             AttributeValue.CardinalTypeCode,
             AttributeValue.CardToText(av.val), len);
  END CardinalToTextEnc;

PROCEDURE CardinalFromTextEnc (av: T; enc: TEXT)
  RAISES {AttributeValue.Invalid} =
  VAR
    type, len: CARDINAL;
    text     : TEXT;
  BEGIN
    AttributeValue.StandardDecoding(enc, type, len, text);
    IF type # AttributeValue.CardinalTypeCode OR len # 4 THEN
      RAISE AttributeValue.Invalid
    END;
    av.val := AttributeValue.TextToCard(text);
    av.defined := TRUE;
  END CardinalFromTextEnc;

BEGIN
END CardinalAttributeValue.
