INTERFACE CheckpointTree;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:32:28  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:20:40  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/23 08:34:49  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.1  1996/09/17 12:57:06  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A CheckpointTree.T is a tree of checkpoints.  Each node of the tree
   represents a checkpoint and contains two deltas that represent the
   connection of the checkpoint to its father (so the deltas of the root
   are empty). *)

IMPORT Delta, CardSeq, Access;
IMPORT AtomList;

TYPE

  SonPosition = {Previous, Actual, Next};
  Mode = {Sequence, Tree};

  T =
    (* ABSTRACT *) OBJECT
    METHODS

      (** --- Commands on the tree --- *)

      getMode (): Mode RAISES {InternalError, Empty, Access.Locked} := NIL;
               (** Returns the mode of the tree. *)

      setMode (m: Mode) RAISES {InternalError, Empty, Access.Locked} := NIL;
               (** Set the mode of the log. *)


      (* --- Commands on the structure of the tree --- *)

      createRoot () RAISES {Access.Locked, InternalError} := NIL;
                  (* After initialization, the is empty.  createRoot
                     creates the root checkpoint.  The new root becomes the
                     actual node of the tree.  If the tree already has a
                     root, nothing happens, except that root becomes
                     actual. *)

      isEmpty (): BOOLEAN RAISES {InternalError, Access.Locked} := NIL;
               (* Check whether a root exists. *)

      changes() RAISES {Access.Locked, Empty, InternalError};
      (* notify that a checkpoint is needed before next undo *)

      needsCheckpoint(): BOOLEAN  RAISES {Access.Locked, Empty, InternalError};
      (* return TRUE if changes was called between last creatSon and now *)
      
      createSon (forward, backward: Delta.T)
                RAISES {Empty, InternalError, Access.Locked} := NIL;
                 (* A new node is created as a son of the actual node.  The
                    deltas backward and forward will be associated with the
                    new son. *)

      noSons (): CARDINAL RAISES {InternalError, Empty, Access.Locked} := NIL;
              (* Returns the number of sons of the actual node. *)

      actSon (): CARDINAL RAISES {InternalError, Empty, Access.Locked} := NIL;
              (* Returns the number of the actual son *)

      setLabel (l: INTEGER) RAISES {InternalError, Empty, Access.Locked} := NIL;
                (* Sets the label of the actual node. *)

      getLabel (VAR l: INTEGER; VAR ok: BOOLEAN)
               RAISES {InternalError, Empty, Access.Locked} := NIL;
                (* Get the label of the actual node. *)

      getDeltas (VAR forward, backward: Delta.T)
                RAISES {InternalError, Empty, Access.Locked} := NIL;
                 (* Return the deltas associated with the current node. *)

      getWriteDeltas (VAR forward, backward: Delta.T)
                RAISES {InternalError, Empty, Access.Locked} := NIL;
                 (* Return the deltas in that applied commands may be stored. *)

      getPath (): CardSeq.T RAISES {InternalError, Empty, Access.Locked};
      (* Returns the numbers of the actual sons from root (hi)
         to the actual node (lo) *)

      (* --- Commands for navigation --- *)

      gotoRoot () RAISES {Empty, InternalError, Access.Locked} := NIL;
                (* Root becomes actual. *)

      gotoFather ()
                 RAISES {NoSuchNode, Empty, InternalError, Access.Locked} := NIL;
                  (* Father becomes actual *)

      gotoSon (which: SonPosition)
              RAISES {NoSuchNode, Empty, InternalError, Access.Locked} := NIL;
               (* Actual, next or previous son becomes actual *)

      gotoNthSon (which: CARDINAL)
                 RAISES {NoSuchNode, Empty, InternalError, Access.Locked} := NIL;
                  (* The nth son becomes actual *)

    END;

EXCEPTION
  InternalError(AtomList.T);
  Empty;
  NoSuchNode;
  
END CheckpointTree.
