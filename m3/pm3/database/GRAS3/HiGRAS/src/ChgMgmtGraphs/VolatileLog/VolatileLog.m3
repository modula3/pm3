MODULE VolatileLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:35:20  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.4  1996/11/14 14:17:25  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.3  1996/09/23 08:35:51  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:35  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:58:41  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT InternalLog AS Base;

IMPORT VolatileCheckpointTree, VolatileLabelTable,
       VolatileMetaOpStack;
IMPORT Access;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init          := Init;
      END;


PROCEDURE Init (log: T): T RAISES {} =
  (* The only possibility to get a Access.Locked from Log.T.initialize is
     that creation of the root node of the checkpointtree is aborted due to
     deadlocks.  This can't happen in a volatile checkpoint tree. A
     similar argument holds for Base.InitializeError. *)
  <* FATAL Access.Locked, Base.InitializeError *>
  BEGIN
    Base.Internal.initialize(
             log, tree := NEW(VolatileCheckpointTree.T).init(),
             labels := NEW(VolatileLabelTable.T).init(),
             fstack := NEW(VolatileMetaOpStack.T).init(),
             bstack := NEW(VolatileMetaOpStack.T).init());
    RETURN log;
  END Init;

BEGIN
END VolatileLog.
