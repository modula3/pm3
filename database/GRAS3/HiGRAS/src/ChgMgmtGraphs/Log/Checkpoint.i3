INTERFACE Checkpoint;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 14:10:33  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.1  1996/09/17 12:57:02  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A Checkpoint.T contains the absNr (this is for addressing after
   closeTemp and (re-)open), the label and the deltas of the checkpoint. *)

IMPORT Delta;

CONST
  (* Constants for internal checkpoint labels. *)
  NoLabel = -1;

TYPE

  T = RECORD
        label      : INTEGER;
        forw, backw: Delta.T;
      END;


END Checkpoint.
