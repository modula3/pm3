MODULE PersistentCheckpointInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:34:01  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/09/23 08:35:21  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:01  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:51  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT FilePos, Type, Word, DeltaInfo;

TYPE IntArray = ARRAY [0 .. BYTESIZE(INTEGER) - 1] OF Type.Byte;

PROCEDURE IntToByteArray (x: INTEGER; VAR ba: IntArray) =
  BEGIN
    ba[0] := Word.Extract(x, 0, 8);
    ba[1] := Word.Extract(x, 8, 8);
    ba[2] := Word.Extract(x, 16, 8);
    ba[3] := Word.Extract(x, 24, 8);
  END IntToByteArray;

PROCEDURE ByteArrayToInt (READONLY ba: IntArray; VAR x: INTEGER) =
  BEGIN
    x := Word.Insert(x, ba[0], 0, 8);
    x := Word.Insert(x, ba[1], 8, 8);
    x := Word.Insert(x, ba[2], 16, 8);
    x := Word.Insert(x, ba[3], 24, 8);
  END ByteArrayToInt;

CONST IntLength = BYTESIZE(INTEGER);

CONST
  PDILength   = 3 * BYTESIZE(INTEGER) + 4 * FilePos.ByteSize + DeltaInfo.ByteSize;
  LabelOff    = 0;
  NoOfSonsOff = LabelOff + IntLength;
  ActSonNoOff = NoOfSonsOff + IntLength;
  DeltaPosOff = ActSonNoOff + IntLength;
  LMCOff      = DeltaPosOff + DeltaInfo.ByteSize;
  RSOff       = LMCOff + FilePos.ByteSize;
  FatherOff   = RSOff + FilePos.ByteSize;
  ActSonOff   = FatherOff + FilePos.ByteSize;

(* Conversion routines for Stream instantiation *)
PROCEDURE ToByteArray (READONLY el : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray) =
  BEGIN
    ba := NEW(REF Type.ByteArray, PDILength);
    len := PDILength;
    IntToByteArray(el.label, SUBARRAY(ba^, LabelOff, IntLength));
    IntToByteArray(el.noOfSons, SUBARRAY(ba^, NoOfSonsOff, IntLength));
    IntToByteArray(el.actSonNo, SUBARRAY(ba^, ActSonNoOff, IntLength));
    DeltaInfo.ToByteArray(
      el.deltas, SUBARRAY(ba^, DeltaPosOff, DeltaInfo.ByteSize));
    FilePos.ToByteArray(
      el.leftMostChild, SUBARRAY(ba^, LMCOff, FilePos.ByteSize));
    FilePos.ToByteArray(
      el.rightSibling, SUBARRAY(ba^, RSOff, FilePos.ByteSize));
    FilePos.ToByteArray(
      el.father, SUBARRAY(ba^, FatherOff, FilePos.ByteSize));
    FilePos.ToByteArray(
      el.actSon, SUBARRAY(ba^, ActSonOff, FilePos.ByteSize));
  END ToByteArray;

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      el : T;
                         VAR      ok : BOOLEAN         ) =
  BEGIN
    IF len # PDILength THEN
      ok := FALSE
    ELSE
      ok := TRUE;
      ByteArrayToInt(SUBARRAY(ba, LabelOff, IntLength), el.label);
      ByteArrayToInt(SUBARRAY(ba, NoOfSonsOff, IntLength), el.noOfSons);
      ByteArrayToInt(SUBARRAY(ba, ActSonNoOff, IntLength), el.actSonNo);
      DeltaInfo.FromByteArray(
        SUBARRAY(ba, DeltaPosOff, DeltaInfo.ByteSize), el.deltas);
      FilePos.FromByteArray(
        SUBARRAY(ba, LMCOff, FilePos.ByteSize), el.leftMostChild);
      FilePos.FromByteArray(
        SUBARRAY(ba, RSOff, FilePos.ByteSize), el.rightSibling);
      FilePos.FromByteArray(
        SUBARRAY(ba, FatherOff, FilePos.ByteSize), el.father);
      FilePos.FromByteArray(
        SUBARRAY(ba, ActSonOff, FilePos.ByteSize), el.actSon);
    END;
  END FromByteArray;

BEGIN
END PersistentCheckpointInfo.
