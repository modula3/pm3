INTERFACE Log;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.2  1997/05/30 07:54:02  roland
    Backward loop added to deltas to efficiently implement copying of
    backward deltas.

    Revision 1.1  1997/04/23 13:32:54  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:20:43  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/23 08:34:51  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.1  1996/09/17 12:57:19  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A log is a tree of checkpoints.  The edges of the tree are each labelled
   with two sequences of graph changing commands (deltas).  These deltas
   represent the changes to a graph made between the two adjacent
   checkpoints.  At each edge there are two deltas: One for the direction
   from father to son (forward delta) and one for the other direction
   (backward delta).  When the tree is traversed (with
   Undo/Redo/Backstep/Forstep), the appropriate deltas are returned and
   must be executed, so that the state of the graph always conforms to the
   actual checkpoint in the tree.  The path from the root of the tree to
   the actual checkpoint reflects the net changes applied to the graph
   since the creation of the log.  The sons of each checkpoint are ordered
   by their time of creation.  Each cehckpoint has a special son 
   called the actual son.  This is the son which was visited last.

   A log can be in one of two modes: Tree and Sequence.  If in mode Tree,
   the log really behaves as a tree.  If Sequence is the actual mode, only
   the actual path can be traversed and extended.  As a consequence, the
   operations redoIth, redoNext, redoPrev, backstep and forstep are
   forbidden in this mode and will raise an exception if called.

   This is the abstract base class for the classes PersistentLog and
   VolatileLog. *)

IMPORT Delta, DeltaList, Access;
IMPORT InternalLog AS Super;
IMPORT AtomList;

TYPE
  Mode = {Sequence, Tree};

  T <: Public;
  Public =
    Super.Internal OBJECT
    METHODS
      (* --- Commands on the log --- *)

      getMode (): Mode RAISES {InternalError, Access.Locked};
               (* Returns the mode of the log. *)

      setMode (m: Mode) RAISES {InternalError, Access.Locked};
               (* Set the mode of the log. *)


      (* --- Commands on the structure of the log -- *)

      setCheckpoint () RAISES {InternalError, Access.Locked};
                     (* Sets a checkpoint as a new son of the
                        actual checkpoint and as a leaf of the
                        tree.  This checkpoints becomes the
                        actual checkpoint. *)


      setCheckpointLabel (label: CARDINAL)
                          RAISES {InternalError, Access.Locked};
                          (* Set the label 'label' to the actual
                             checkpoint *)

      getCheckpointLabel (VAR l: CARDINAL; VAR ok: BOOLEAN)
                          RAISES {InternalError, Access.Locked};
                          (* If the actual checkpoint has a label, ok will
                             be TRUE and l will contain the label. *)

      removeCheckpointLabel ()
                             RAISES {InternalError, Access.Locked};
                            (* Removes the label from the current checkpoint. *)

      appliedCommands (forward, backward: Delta.T)
                       RAISES {InternalError, Access.Locked};
                       (* The actual checkpoint always keeps an implicit
                          pair of deltas representing the commands that
                          were applied to the graph since it became the
                          actual checkpoint.  These deltas will be the
                          labels of the edge to the next set checkpoint.
                          The parameters of appliedCommands will be added
                          to these deltas. *)

      getActualPath (VAR fwd, bwd: DeltaList.T)
                     RAISES {InternalError, Access.Locked};
                     (* Returns lists of Deltas containing all deltas of
                        the actual path.  The backward delta list contains
                        all backward deltas from the actual state (top) to
                        the root (bottom).  The forward delta list contains
                        all deltas from the root (top) to the actual state
                        (bottom). *)

      getActSon (): CARDINAL RAISES {InternalError, Access.Locked};
                 (* Return the number of the actual son of the actual
                    checkpoint. *)

      getNumberOfSons (): CARDINAL RAISES {InternalError, Access.Locked};
                       (* Returns the number of sons of the actual
                          checkpoint. *)


      (* --- Commands for traversing the log --- *)

      undo (): Delta.T
            RAISES {InternalError, NoSuchCheckpoint, Access.Locked};
            (* Go to the father checkpoint in the tree and return the delta
               from son to father.  This delta must be applied to the
               graph.  Otherwise, the state of the graph and the log will
               not be synchronous any more. *)

      redo (): Delta.T
            RAISES {InternalError, NoSuchCheckpoint, Access.Locked};
            (* Go to the actual son of the actual checkpoint in the tree
               and return the delta from father to son.  This delta must be
               applied to the graph.  Otherwise, the state of the graph and
               the log will not be synchronous any more. *)


      gotoCPLabel (l: CARDINAL): DeltaList.T
                   RAISES {NoSuchCheckpoint, InternalError, Access.Locked};
                   (* If a checkpoint with label l exists, it will become
                      the actual checkpoint.  The returned delta list
                      contains from top to bottom all deltas that have to be
                      executed to bring the graph into the corresponding
                      state.  If no checkpoint has label l or the
                      checkpoint with this label is not on the actual path
                      in Mode.Sequence, NoSuchCheckpoint will be raised. *)

      (* The following commands are forbidden in Mode.Sequence.  They will
         all raise the exception ModeError, when called in this mode. *)

      redoPrev (): Delta.T RAISES {ModeError, NoSuchCheckpoint,
                                   InternalError, Access.Locked};
                (* Returns the delta between the actual checkpoint and the
                   next older brother of the actual son.  If the actual son
                   is the oldest son, the exception NoSuchCheckpoint is
                   raised. *)

      redoNext (): Delta.T RAISES {ModeError, NoSuchCheckpoint,
                                   InternalError, Access.Locked};
                (* Returns the delta between the actual checkpoint and the
                   next younger brother of the actual son.  If the actual
                   son is the youngest son, the exception NoSuchCheckpoint
                   is raised. *)

      redoIth (READONLY i: CARDINAL): Delta.T
               RAISES {ModeError, NoSuchCheckpoint, InternalError,
                       Access.Locked};
               (* Returns the delta between the actual checkpoint and its
                  ith son.  If i is greater than the number of sons of the
                  actual checkpoint, the exception NoSuchCheckpoint is
                  raised. *)

      backstep (): Delta.T RAISES {ModeError, NoSuchCheckpoint,
                                   InternalError, Access.Locked};
                (* The graph changing commands togehter with the undo/redo
                   commands can be seen as sequence.  backstep goes
                   backward in this sequence. *)

      forstep (): Delta.T RAISES {ModeError, NoSuchCheckpoint,
                                  InternalError, Access.Locked};
               (* Like backstep, but forward. *)


    END;

EXCEPTION
  NoSuchCheckpoint;
  ModeError;
  InternalError(AtomList.T);

CONST Brand = "Log";

END Log.
