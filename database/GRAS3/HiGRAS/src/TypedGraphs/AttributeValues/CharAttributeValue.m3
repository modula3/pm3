MODULE CharAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:59  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:35:08  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT AttributeValue, Text;

REVEAL

  T = Public BRANDED OBJECT
        val: CHAR := '\000';
      OVERRIDES
        get      := CharGet;
        set      := CharSet;
        toText   := CharToTextEnc;
        fromText := CharFromTextEnc;
        type     := CharType;
      END;

PROCEDURE CharGet (av: T): CHAR =
  BEGIN
    RETURN av.val;
  END CharGet;

PROCEDURE CharSet (av: T; val: CHAR) =
  BEGIN
    av.val := val;
    av.defined := TRUE;
  END CharSet;

PROCEDURE CharType (<* UNUSED *> av: T): CARDINAL =
  BEGIN
    RETURN AttributeValue.CharTypeCode;
  END CharType;

PROCEDURE CharToTextEnc (av: T; VAR len: CARDINAL): TEXT =
  BEGIN
    RETURN AttributeValue.StandardEncoding(
             AttributeValue.CharTypeCode, Text.FromChar(av.val), len);
  END CharToTextEnc;

PROCEDURE CharFromTextEnc (av: T; enc: TEXT)
  RAISES {AttributeValue.Invalid} =
  VAR
    type, len: CARDINAL;
    text     : TEXT;
  BEGIN
    AttributeValue.StandardDecoding(enc, type, len, text);
    IF type # AttributeValue.CharTypeCode OR len # 1 THEN
      RAISE AttributeValue.Invalid
    END;
    av.val := Text.GetChar(text, 0);
    av.defined := TRUE;
  END CharFromTextEnc;


BEGIN
END CharAttributeValue.
