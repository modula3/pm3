MODULE PersCard;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:33:53  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/23 08:35:18  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT Cardinal, Type;

PROCEDURE ToByteArray (READONLY el : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray) =
  BEGIN
    len := BYTESIZE(CARDINAL);
    ba := NEW(REF Type.ByteArray, BYTESIZE(CARDINAL));
    Cardinal.ToByteArray(el, ba^);
  END ToByteArray;

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      el : T;
                         VAR      ok : BOOLEAN         ) =
  BEGIN
    IF len # BYTESIZE(CARDINAL) THEN
      ok := FALSE;
    ELSE
      ok := TRUE;
      Cardinal.FromByteArray(ba, el);
    END
  END FromByteArray;

BEGIN
END PersCard.
