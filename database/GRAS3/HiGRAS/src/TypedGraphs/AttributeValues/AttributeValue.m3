MODULE AttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:51  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:34:57  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT Text, Word;
  
(* Useful procedures for conversion *)

PROCEDURE CardToText (val: CARDINAL): TEXT =
  VAR buf: ARRAY[0..3] OF CHAR;
  BEGIN
    buf[0] := VAL(Word.Extract(val, 0, 8), CHAR);
    buf[1] := VAL(Word.Extract(val, 8, 8), CHAR);
    buf[2] := VAL(Word.Extract(val, 16, 8), CHAR);
    buf[3] := VAL(Word.Extract(val, 24, 8), CHAR);
    RETURN Text.FromChars(buf);
  END CardToText;

PROCEDURE TextToCard (t: TEXT): CARDINAL =
  VAR res: CARDINAL := 0;
      buf := ARRAY[0..3] OF CHAR{'\000', '\000', '\000', '\000'};
  BEGIN
    IF t # NIL THEN
      Text.SetChars(buf, t);
      res := Word.Insert(res, ORD(buf[0]), 0, 8);
      res := Word.Insert(res, ORD(buf[1]), 8, 8);
      res := Word.Insert(res, ORD(buf[2]), 16, 8);
      res := Word.Insert(res, ORD(buf[3]) MOD 128, 24, 8);
    END;
    RETURN res;
  END TextToCard;

PROCEDURE IntToText (val: INTEGER): TEXT =
  VAR buf: ARRAY[0..3] OF CHAR;
  BEGIN
    buf[0] := VAL(Word.Extract(val, 0, 8), CHAR);
    buf[1] := VAL(Word.Extract(val, 8, 8), CHAR);
    buf[2] := VAL(Word.Extract(val, 16, 8), CHAR);
    buf[3] := VAL(Word.Extract(val, 24, 8), CHAR);
    RETURN Text.FromChars(buf);
  END IntToText; 

PROCEDURE TextToInt (t: TEXT): INTEGER =
  VAR res: INTEGER := 0;
      buf := ARRAY[0..3] OF CHAR{'\000', '\000', '\000', '\000'};
  BEGIN
    IF t # NIL THEN
      Text.SetChars(buf, t);
      res := Word.Insert(res, ORD(buf[0]), 0, 8);
      res := Word.Insert(res, ORD(buf[1]), 8, 8);
      res := Word.Insert(res, ORD(buf[2]), 16, 8);
      res := Word.Insert(res, ORD(buf[3]), 24, 8);
    END;
    RETURN res;
  END TextToInt; 

PROCEDURE StandardEncoding (type: CARDINAL; val: TEXT; VAR len: CARDINAL): TEXT =
  VAR tlen: CARDINAL;
  BEGIN
    IF val # NIL THEN
      tlen := Text.Length(val);
    ELSE
      tlen := 0; val := "";
    END;
    len := tlen + 8;
    RETURN CardToText(type) & CardToText(tlen) & val;
  END StandardEncoding;

PROCEDURE StandardDecoding (    code     : TEXT;
                            VAR type, len: CARDINAL;
                            VAR val      : TEXT      ) RAISES {Invalid} =
  VAR codelen: CARDINAL;
  BEGIN
    codelen := Text.Length(code);
    IF codelen < 8 THEN RAISE Invalid END;
    type := TextToCard(Text.Sub(code, 0, 4));
    len := TextToCard(Text.Sub(code, 4, 4));
    IF codelen - 8 < len THEN RAISE Invalid END;
    val := Text.Sub(code, 8, len);
  END StandardDecoding;


BEGIN
END AttributeValue.
