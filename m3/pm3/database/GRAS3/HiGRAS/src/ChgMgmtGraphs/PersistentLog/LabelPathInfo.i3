INTERFACE LabelPathInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:33:44  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/17 12:57:46  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A LabelPathInfo.T is a pair (l,p) where l is a cardinal (a checkpoint
   label) and p a stack of cardinals (the path from the root of the
   checkpoint tree to the checkpoint with label l).  These pairs are stored
   as a binary search tree in a stream.  Therefore, T also contains
   positions of successors and predecessor of the pair in the tree. *)

IMPORT CardSeq, FilePos, Type;

TYPE
  T = RECORD
        label: CARDINAL;
        path : CardSeq.T;
        left, right: FilePos.T;
      END;

(* Conversion routines for Stream instantiation *)
PROCEDURE ToByteArray (READONLY el : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray);

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      el : T;
                         VAR      ok : BOOLEAN         );

END LabelPathInfo.
