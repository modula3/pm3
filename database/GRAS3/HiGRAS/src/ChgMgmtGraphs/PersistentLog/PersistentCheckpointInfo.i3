INTERFACE PersistentCheckpointInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:33:57  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/09/23 08:35:20  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:00  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:49  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT FilePos, Type, DeltaInfo;

(** ModeBit and NeedsCheckpointBit are only used for the label of the root
    of a checkpoint tree. Let l be the label of the root then

    Word.And(l, ModeBit)            = 0 -> tree mode
    Word.And(l, ModeBit)            > 0 -> sequence mode
    Word.And(l, NeedsCheckpointBit) = 0 -> no checkpoint necessary for undo
    Word.And(l, NeedsCheckpointBit) > 0 -> set checkpoint before undo
*)

CONST
  ModeBit = 1;
  NeedsCheckpointBit = 2;
  
TYPE
  T = RECORD
        label, noOfSons, actSonNo: INTEGER;
        (* deltas give the positions in the commands streams and the
           costs of forward and backward deltas associated with the
           node. The deltas of the root node are the ones that are
           actually written to. All other deltas will not change. *)
        deltas: DeltaInfo.T;
        (* child and sibling are positions in a PCIStream *)
        leftMostChild, rightSibling, father, actSon: FilePos.T;
      END;

(* Conversion routines for Stream instantiation *)
PROCEDURE ToByteArray (READONLY el : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray);

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                  len: CARDINAL;
                         VAR      el : T;
                         VAR      ok : BOOLEAN         );

END PersistentCheckpointInfo.
