MODULE GRASConversions;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/09/17 12:50:33  roland
    Basics now contains everything CardinalCollections did. Comfortable
    use of generics with the procedures of basics.tmpl (CursorList,
    CursorSet, Stack, NTree).

    Corrections in NTree.

*)
(***************************************************************************)

IMPORT Word, Text;

VAR byte: ARRAY [0 .. 3] OF CHAR;

PROCEDURE IntToString4 (no: INTEGER): TEXT =
  (* converts a 4 byte cardinal to a TEXT-string of length 4 *)
  (* FOR-loop unrolled for efficiency *)
  BEGIN
    byte[0] := VAL(Word.Extract(no, 24, 8), CHAR);
    byte[1] := VAL(Word.Extract(no, 16, 8), CHAR);
    byte[2] := VAL(Word.Extract(no,  8, 8), CHAR);
    byte[3] := VAL(Word.Extract(no,  0, 8), CHAR);
    RETURN Text.FromChars(byte);
  END IntToString4;

PROCEDURE String4ToInt (string: TEXT): INTEGER RAISES {ConversionFailed} =
  (* converts a TEXT-string of length 4 to a 4-byte cardinal *)
  (* FOR-loop unrolled for efficiency *)
  VAR
    no  : Word.T := 0;
  BEGIN
    IF string # NIL AND Text.Length(string) >= 4 THEN
      no := Word.Insert(no, ORD(Text.GetChar(string, 0)), 24, 8);
      no := Word.Insert(no, ORD(Text.GetChar(string, 1)), 16, 8);
      no := Word.Insert(no, ORD(Text.GetChar(string, 2)),  8, 8);
      no := Word.Insert(no, ORD(Text.GetChar(string, 3)),  0, 8);
    ELSE
      RAISE ConversionFailed;
    END;
    RETURN no;
  END String4ToInt;

BEGIN
END GRASConversions.
