MODULE TextAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:06  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:35:18  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT AttributeValue;

REVEAL

  T = Public BRANDED OBJECT
        val: TEXT := "";
      OVERRIDES
        get      := TextGet;
        set      := TextSet;
        toText   := TextToTextEnc;
        fromText := TextFromTextEnc;
        type     := TextType;
      END;

PROCEDURE TextGet (av: T): TEXT =
  BEGIN
    RETURN av.val;
  END TextGet;

PROCEDURE TextSet (av: T; val: TEXT) =
  BEGIN
    av.val := val;
    av.defined := TRUE;
  END TextSet;

PROCEDURE TextType (<* UNUSED *> av: T): CARDINAL =
  BEGIN
    RETURN AttributeValue.TextTypeCode;
  END TextType;

PROCEDURE TextToTextEnc (av: T; VAR len: CARDINAL): TEXT =
  BEGIN
    RETURN
      AttributeValue.StandardEncoding(AttributeValue.TextTypeCode, av.val, len);
  END TextToTextEnc;

PROCEDURE TextFromTextEnc (av: T; enc: TEXT)
  RAISES {AttributeValue.Invalid} =
  VAR type, len: CARDINAL;
  BEGIN
    AttributeValue.StandardDecoding(enc, type, len, av.val);
    IF type # AttributeValue.TextTypeCode THEN
      RAISE AttributeValue.Invalid
    END;
    av.defined := TRUE;
  END TextFromTextEnc;


BEGIN
END TextAttributeValue.
