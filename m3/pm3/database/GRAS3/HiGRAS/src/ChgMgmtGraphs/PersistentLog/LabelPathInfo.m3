MODULE LabelPathInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1998/01/21 12:31:11  roland
    Bugfixes in finding and reading path information.

    Revision 1.1  1997/04/23 13:33:47  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/17 12:57:47  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT CardSeq, Cardinal, Type, FilePos;

CONST
  IntLength   = BYTESIZE(INTEGER);
  LPMinLength = 2 * FilePos.ByteSize + IntLength;
  LabelOff    = 0;
  LeftOff     = LabelOff + IntLength;
  RightOff    = LeftOff + FilePos.ByteSize;
  PathOff     = RightOff + FilePos.ByteSize;

(* Conversion routines for Stream instantiation *)
PROCEDURE ToByteArray (READONLY el : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray) =
  VAR plen: CARDINAL := 0;
  BEGIN
    IF el.path # NIL THEN plen := el.path.size(); END;
    len := LPMinLength + plen*IntLength;
    ba := NEW(REF Type.ByteArray, len);
    Cardinal.ToByteArray(el.label, SUBARRAY(ba^, LabelOff, IntLength));
    FilePos.ToByteArray(el.left, SUBARRAY(ba^, LeftOff, FilePos.ByteSize));
    FilePos.ToByteArray(
      el.right, SUBARRAY(ba^, RightOff, FilePos.ByteSize));
    FOR i := 0 TO plen - 1 DO
      Cardinal.ToByteArray(
        el.path.get(i), SUBARRAY(ba^, PathOff + IntLength * i, IntLength));
    END;
  END ToByteArray;

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      el : T;
                         VAR      ok : BOOLEAN         ) =
  VAR plen, son: CARDINAL;
  BEGIN
    IF len < LPMinLength OR (len - LPMinLength) MOD IntLength # 0 THEN
      ok := FALSE
    ELSE
      ok := TRUE;
      Cardinal.FromByteArray(SUBARRAY(ba, LabelOff, IntLength), el.label);
      FilePos.FromByteArray(
        SUBARRAY(ba, LeftOff, FilePos.ByteSize), el.left);
      FilePos.FromByteArray(
        SUBARRAY(ba, RightOff, FilePos.ByteSize), el.right);
      plen := (len - LPMinLength) DIV IntLength;
      el.path := NEW(CardSeq.T).init(plen);
      FOR i := 0 TO plen - 1 DO
        Cardinal.FromByteArray(
          SUBARRAY(ba, PathOff + i * IntLength, IntLength), son);
        el.path.addhi(son);
      END;
    END;
  END FromByteArray;

BEGIN
END LabelPathInfo.
