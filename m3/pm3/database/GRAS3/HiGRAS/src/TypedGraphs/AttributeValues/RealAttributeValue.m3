UNSAFE MODULE RealAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:04  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:35:16  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)


IMPORT AttributeValue;

REVEAL

  T = Public BRANDED OBJECT
        val: REAL := 0.0;
      OVERRIDES
        get      := RealGet;
        set      := RealSet;
        toText   := RealToTextEnc;
        fromText := RealFromTextEnc;
        type     := RealType;
      END;

PROCEDURE RealGet (av: T): REAL =
  BEGIN
    RETURN av.val;
  END RealGet;

PROCEDURE RealSet (av: T; val: REAL) =
  BEGIN
    av.val := val;
    av.defined := TRUE;
  END RealSet;

PROCEDURE RealType (<* UNUSED *> av: T): CARDINAL =
  BEGIN
    RETURN AttributeValue.RealTypeCode;
  END RealType;

PROCEDURE RealToTextEnc (av: T; VAR len: CARDINAL): TEXT =
  BEGIN
    RETURN AttributeValue.StandardEncoding(
             AttributeValue.RealTypeCode, RealToText(av.val), len);
  END RealToTextEnc;

PROCEDURE RealFromTextEnc (av: T; enc: TEXT)
  RAISES {AttributeValue.Invalid} =
  VAR
    type, len: CARDINAL;
    text     : TEXT;
  BEGIN
    AttributeValue.StandardDecoding(enc, type, len, text);
    IF type # AttributeValue.RealTypeCode OR len # 4 THEN
      RAISE AttributeValue.Invalid
    END;
    av.val := TextToReal(text);
    av.defined := TRUE;
  END RealFromTextEnc;


PROCEDURE RealToText (x: REAL): TEXT =
  VAR buf: INTEGER;
  BEGIN
    buf := LOOPHOLE(x, INTEGER);
    RETURN AttributeValue.IntToText(buf);
  END RealToText;

PROCEDURE TextToReal (t: TEXT): REAL RAISES {} =
  VAR buf: INTEGER := 0;
  BEGIN
    IF t # NIL THEN
      buf := AttributeValue.TextToInt(t);
      RETURN LOOPHOLE(buf, REAL);
    ELSE
      RETURN 0.0;
    END;
  END TextToReal;

BEGIN
  <* ASSERT BYTESIZE(INTEGER) = BYTESIZE(REAL) *>
END RealAttributeValue.
