INTERFACE PersCard;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:33:50  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/23 08:35:17  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT Type;

TYPE
  T = CARDINAL;

(* Conversion routines for Stream instantiation *)
PROCEDURE ToByteArray (READONLY el : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray);

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      el : T;
                         VAR      ok : BOOLEAN         );


END PersCard.
