MODULE DeltaInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:33:34  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/23 08:35:15  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT FilePos, Type, Cardinal;

CONST
  StateOff    = 0;
  FCostsOff   = StateOff + 1;
  FStartOff   = FCostsOff + BYTESIZE(CARDINAL);
  FEndOff     = FStartOff + FilePos.ByteSize;
  FCurrentOff = FEndOff + FilePos.ByteSize;
  BCostsOff   = FCurrentOff + FilePos.ByteSize;
  BStartOff   = BCostsOff + BYTESIZE(CARDINAL);
  BEndOff     = BStartOff + FilePos.ByteSize;
  BCurrentOff = BEndOff + FilePos.ByteSize;

  DILength = BCurrentOff + FilePos.ByteSize;

PROCEDURE ToByteArray (READONLY el: T; VAR ba: Type.ByteArray) =
  BEGIN
    <* ASSERT NUMBER(ba) >= DILength *>
    ba[StateOff] := ORD(el.state);
    Cardinal.ToByteArray(
      el.info[Forward].costs, SUBARRAY(ba, FCostsOff, BYTESIZE(CARDINAL)));
    FilePos.ToByteArray(
      el.info[Forward].start, SUBARRAY(ba, FStartOff, FilePos.ByteSize));
    FilePos.ToByteArray(
      el.info[Forward].end, SUBARRAY(ba, FEndOff, FilePos.ByteSize));
    FilePos.ToByteArray(el.info[Forward].current,
                        SUBARRAY(ba, FCurrentOff, FilePos.ByteSize));
    Cardinal.ToByteArray(el.info[Backward].costs,
                         SUBARRAY(ba, BCostsOff, BYTESIZE(CARDINAL)));
    FilePos.ToByteArray(
      el.info[Backward].start, SUBARRAY(ba, BStartOff, FilePos.ByteSize));
    FilePos.ToByteArray(
      el.info[Backward].end, SUBARRAY(ba, BEndOff, FilePos.ByteSize));
    FilePos.ToByteArray(el.info[Backward].current,
                        SUBARRAY(ba, BCurrentOff, FilePos.ByteSize));
  END ToByteArray;

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                         VAR      el : T              ) =
  BEGIN
    <* ASSERT NUMBER(ba) >= DILength *>
    el.state := VAL(ba[StateOff], State);
    Cardinal.FromByteArray(SUBARRAY(ba, FCostsOff, BYTESIZE(CARDINAL)),
                           el.info[Forward].costs);
    FilePos.FromByteArray(
        SUBARRAY(ba, FStartOff, FilePos.ByteSize), el.info[Forward].start);
    FilePos.FromByteArray(
        SUBARRAY(ba, FEndOff, FilePos.ByteSize), el.info[Forward].end);
    FilePos.FromByteArray(SUBARRAY(ba, FCurrentOff, FilePos.ByteSize),
                          el.info[Forward].current);
    Cardinal.FromByteArray(SUBARRAY(ba, BCostsOff, BYTESIZE(CARDINAL)),
                           el.info[Backward].costs);
    FilePos.FromByteArray(
        SUBARRAY(ba, BStartOff, FilePos.ByteSize), el.info[Backward].start);
    FilePos.FromByteArray(
        SUBARRAY(ba, BEndOff, FilePos.ByteSize), el.info[Backward].end);
    FilePos.FromByteArray(SUBARRAY(ba, BCurrentOff, FilePos.ByteSize),
                          el.info[Backward].current);
  END FromByteArray;

BEGIN
END DeltaInfo.
