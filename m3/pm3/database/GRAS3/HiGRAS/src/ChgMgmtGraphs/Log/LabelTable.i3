INTERFACE LabelTable;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:32:52  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/11/20 12:20:42  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/09/17 12:57:17  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A LabelTable.T is a table that connects keys of type CARDINAL
   (checkpoint labels) to sequences of cardinals (the path from the root of
   the checkpoint tree to the checkpoint with the key as the label).

   This is the abstract base class for PersistentLabelTable and
   VolatileLabelTable. *)

IMPORT CardSeq;
IMPORT AtomList;

TYPE
  T =
    (* ABSTRACT *) OBJECT
    METHODS
      insert (key: CARDINAL; path: CardSeq.T) RAISES {InternalError} := NIL;
              (* Insert the pair (key, path) into the table.  If table
                 already contains a pair (key, x) it is replaced by the new
                 pair. *)

      remove (key: CARDINAL) RAISES {InternalError} := NIL;
              (* If a pair (key, x) in the table exists, it is removed. *)

      contains (key: CARDINAL): BOOLEAN RAISES {InternalError} := NIL;
                (* Checks whether a pair (key, x) exists in the table *)

      lookup (key: CARDINAL): CardSeq.T RAISES {NotFound, InternalError}
             := NIL;
              (* If a pair (key, x) exists, returns x.  Otherwise NotFound
                 is raised. *)
    END;

EXCEPTION
  NotFound;
  InternalError(AtomList.T);

END LabelTable.
