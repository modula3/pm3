MODULE Cardinal;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.6  1997/11/18 15:27:15  roland
    Bugfix in Compare.

    Revision 1.5  1997/10/31 14:08:22  roland
    New generic implementations for PriorityQueues. New templates and a new
    order function for Cardinal.

    Revision 1.4  1996/09/17 12:50:30  roland
    Basics now contains everything CardinalCollections did. Comfortable
    use of generics with the procedures of basics.tmpl (CursorList,
    CursorSet, Stack, NTree).

    Corrections in NTree.

    Revision 1.1  1996/01/12 13:39:14  grover
    added CardinalCollections. The generics from Basics are used

# Revision 1.2  1995/06/08  15:42:20  grover
# changes made for new implementations in RecoverableGraph
#
# Revision 1.1  1994/03/30  17:18:49  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Word, Type;


PROCEDURE Equal (a, b: T): BOOLEAN =
  BEGIN
    RETURN (a = b);
  END Equal;

PROCEDURE Hash (a: T): Word.T =
  BEGIN
    RETURN a;
  END Hash;

(**
PROCEDURE Compare (a, b: T): [-1 .. 1] =
  BEGIN
    IF (a < b) THEN
      RETURN -1;
    ELSIF (a = b) THEN
      RETURN 0;
    ELSE
      RETURN 1;
    END;
  END Compare;
*)

PROCEDURE Compare (a, b: T): [-1 .. 1] =
  VAR bit := 0;
      mask: Word.T := 1;
  BEGIN
    WHILE (bit < Word.Size)
             AND (Word.And(a, mask) = Word.And(b, mask)) DO
      mask := Word.LeftShift(mask, 1);
      INC(bit);
    END;
    IF (bit = Word.Size) THEN
      RETURN 0;
    ELSIF Word.And(a, mask) > 0 THEN
      RETURN 1;
    ELSE
      RETURN -1;
    END;
  END Compare;


PROCEDURE Eq (a, b: T): BOOLEAN =
  BEGIN
    RETURN a = b;
  END Eq;

PROCEDURE Lt (a, b: T): BOOLEAN =
  BEGIN
    RETURN a < b;
  END Lt;

PROCEDURE ToByteArray (x: CARDINAL; VAR ba: ARRAY OF Type.Byte) =
  BEGIN
    <* ASSERT NUMBER(ba)>=4 *>
    ba[0] := Word.Extract(x, 0, 8);
    ba[1] := Word.Extract(x, 8, 8);
    ba[2] := Word.Extract(x, 16, 8);
    ba[3] := Word.Extract(x, 24, 8);
  END ToByteArray;

PROCEDURE FromByteArray (READONLY ba: ARRAY OF Type.Byte; VAR x: CARDINAL) =
  BEGIN
    <* ASSERT NUMBER(ba)>=4 *>
    x := 0;
    x := Word.Insert(x, ba[0], 0, 8);
    x := Word.Insert(x, ba[1], 8, 8);
    x := Word.Insert(x, ba[2], 16, 8);
    x := Word.Insert(x, ba[3], 24, 7);
  END FromByteArray;

BEGIN
END Cardinal.
