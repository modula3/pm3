MODULE BooleanAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:53  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:35:00  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT AttributeValue, Text;

REVEAL

  T = Public BRANDED OBJECT
        val: BOOLEAN := FALSE;
      OVERRIDES
        get      := BooleanGet;
        set      := BooleanSet;
        toText   := BooleanToTextEnc;
        fromText := BooleanFromTextEnc;
        type     := BooleanType;
      END;

PROCEDURE BooleanGet (av: T): BOOLEAN =
  BEGIN
    RETURN av.val;
  END BooleanGet;

PROCEDURE BooleanSet (av: T; val: BOOLEAN) =
  BEGIN
    av.val := val;
    av.defined := TRUE;
  END BooleanSet;

PROCEDURE BooleanType (<* UNUSED *> av: T): CARDINAL =
  BEGIN
    RETURN AttributeValue.BooleanTypeCode;
  END BooleanType;

CONST BoolEncode = ARRAY [FALSE .. TRUE] OF TEXT{"F", "T"};

PROCEDURE BooleanToTextEnc (av: T; VAR len: CARDINAL): TEXT =
  BEGIN
    RETURN AttributeValue.StandardEncoding(
             AttributeValue.BooleanTypeCode, BoolEncode[av.val], len);
  END BooleanToTextEnc;

PROCEDURE BooleanFromTextEnc (av: T; enc: TEXT)
  RAISES {AttributeValue.Invalid} =
  VAR
    type, len: CARDINAL;
    text     : TEXT;
  BEGIN
    AttributeValue.StandardDecoding(enc, type, len, text);
    IF type # AttributeValue.BooleanTypeCode OR len # 1 THEN
      RAISE AttributeValue.Invalid
    END;
    IF Text.Equal(text, BoolEncode[TRUE]) THEN
      av.val := TRUE
    ELSE
      av.val := FALSE;
    END;
    av.defined := TRUE;
  END BooleanFromTextEnc;


BEGIN
END BooleanAttributeValue.
