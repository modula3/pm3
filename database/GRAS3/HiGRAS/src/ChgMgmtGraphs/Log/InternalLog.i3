INTERFACE InternalLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:32:49  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/09/23 08:34:50  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:58:34  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:15  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT Delta, CheckpointTree, LabelTable, Access, MetaOpStack;

TYPE
  Internal <: Private;
  Private =
    OBJECT
    METHODS
      initialize (tree          : CheckpointTree.T;
                  labels        : LabelTable.T;
                  fstack, bstack: MetaOpStack.T     )
                  RAISES {Access.Locked, InitializeError};

      getComponents (VAR tree          : CheckpointTree.T;
                     VAR forw, backw   : Delta.T;
                     VAR labels        : LabelTable.T;
                     VAR fstack, bstack: MetaOpStack.T     );
    END;

EXCEPTION 
  InitializeError;
  
END InternalLog.
