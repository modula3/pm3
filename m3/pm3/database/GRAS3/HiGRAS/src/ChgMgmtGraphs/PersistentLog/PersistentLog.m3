MODULE PersistentLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.4  1998/05/19 10:17:54  roland
    Support for log-groups implemented.

    Revision 1.3  1998/03/17 14:14:02  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/04/24 14:30:29  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:22  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.6  1997/02/04 11:15:38  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.5  1996/11/20 12:21:15  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.4  1996/11/14 14:17:09  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.3  1996/09/23 08:35:28  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:10  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:58:06  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT InternalLog AS Base;
IMPORT Log AS Super;

IMPORT PersistentCheckpointTree, PersistentLabelTable, FilePos,
       PersistentMetaOpStack;
IMPORT GraphCommandStream, LabelTable, Delta, CheckpointTree, MetaOpStack;
IMPORT PageFile, Access, VirtualResource;
IMPORT Pathname;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        fstream, bstream: GraphCommandStream.T;
      OVERRIDES
        open  := Open;
        close := Close;
      END;

CONST
  ForwardStreamName  = "Forward";
  BackwardStreamName = "Backward";
  CheckpointTreeName = "Checkpoints";
  LabelTableName     = "Labels";
  FStackName         = "FStack";
  BStackName         = "BStack";

PROCEDURE Open ( log     : T;
                 resource: VirtualResource.T;
                 path    : Pathname.T;
                 access  : Access.Mode;
                 new     : BOOLEAN;
                 local   : BOOLEAN            ) : T
  RAISES {Access.Denied, Access.Locked, PageFile.NoAccess,
          Super.InternalError} =
  VAR
    fend, bstart: FilePos.T;
    tree        : PersistentCheckpointTree.T;
    lognew      : BOOLEAN;
  BEGIN
    TRY
      (* If new is FALSE, an old graph was opened and we have to deal with
         possibly not existing log-information (log.close(delete := TRUE)).
         If new is TRUE, log-files are created, too. *)
      lognew := new OR NOT VirtualResource.T.existsFile(
                             resource, path & ForwardStreamName, local);

      (* This will result in an error if new=TRUE and access is read-only,
         because PersistentLog will try to write information to the
         streams.  This configuration, however, should not arise, because
         VirtualResource will raise an exception if it is asked to create a
         resource in read-only mode, and this would be the only chance to
         get such parameters. *)

      IF NOT lognew OR access # Access.Mode.ReadOnlyShared THEN
        log.fstream :=
          NEW(GraphCommandStream.T).open(
            resource, path & ForwardStreamName, access, lognew, forward := TRUE);
        log.fstream.getLastPosition(fend);
        log.bstream :=
          NEW(GraphCommandStream.T).open(
            resource, path & BackwardStreamName, access, lognew, forward := FALSE);
        log.bstream.getFirstPosition(bstart);
        tree := NEW(PersistentCheckpointTree.T).open(
                  resource, path & CheckpointTreeName, access, lognew, log.fstream,
                  log.bstream);
        Base.Internal.initialize(
          log, tree, labels := NEW(PersistentLabelTable.T).open(
                                 resource, path & LabelTableName, access, lognew),
          fstack := NEW(PersistentMetaOpStack.T).open(
                      resource, path & FStackName, access, lognew),
          bstack := NEW(PersistentMetaOpStack.T).open(
                      resource, path & BStackName, access, lognew));
      END;
    EXCEPT
      GraphCommandStream.DirectionMismatch =>
        RAISE
          Super.InternalError(ErrorSupport.Create(
                                "PersistentLog.Open",
                                "GraphCommandStream.DirectionMismatch"));
    | LabelTable.InternalError =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLog.Open", "LabelTable.InternalError"));
    | CheckpointTree.InternalError =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLog.Open", "CheckpointTree.InternalError"));
    | MetaOpStack.InternalError =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLog.Open", "MetaOpStack.InternalError"));
    | Base.InitializeError =>
        RAISE
          Super.InternalError(ErrorSupport.Create("PersistentLog.Open",
                                                  "Log.InitializeError"));
    | VirtualResource.FatalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentLog.Open",
                                    "VirtualResource.FatalError", info));
    | GraphCommandStream.InternalError (info) =>
        RAISE
          Super.InternalError(ErrorSupport.Propagate(
                                "PersistentLog.Open",
                                "GraphCommandStream.InternalError", info));
    END;
    IF lognew AND access = Access.Mode.ReadOnlyShared THEN
      RETURN NIL
    ELSE
      RETURN log;
    END;
  END Open;

PROCEDURE Close ( log      : T;
                  resource : VirtualResource.T;
                  path     : Pathname.T;
                  delete   : BOOLEAN;
                  local    : BOOLEAN            )
  RAISES {Super.InternalError} =
  VAR
    labels        : LabelTable.T;
    forw, backw   : Delta.T;
    tree          : CheckpointTree.T;
    fstack, bstack: MetaOpStack.T;
  BEGIN
    Base.Internal.getComponents(
      log, tree, forw, backw, labels, fstack, bstack);
    TRY
      NARROW(tree, PersistentCheckpointTree.T).close();
      NARROW(labels, PersistentLabelTable.T).close();
      NARROW(fstack, PersistentMetaOpStack.T).close();
      NARROW(bstack, PersistentMetaOpStack.T).close();
      log.fstream.close();
      log.bstream.close();
    EXCEPT
    | CheckpointTree.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentLog.Close",
                                    "CheckpointTree.InternalError", info));
    | LabelTable.InternalError (info) =>
        RAISE
          Super.InternalError(
            ErrorSupport.Propagate(
              "PersistentLog.Close", "LabelTable.InternalError", info));
    | MetaOpStack.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentLog.Close",
                                    "MetaOpStack.InternalError", info));
    | GraphCommandStream.InternalError (info) =>
        RAISE
          Super.InternalError(ErrorSupport.Propagate(
                                "PersistentLog.Close",
                                "GraphCommandStream.InternalError", info));
    END;

    IF delete THEN
      TRY
        VirtualResource.T.deleteFile(resource, path & ForwardStreamName, local);
        VirtualResource.T.deleteFile(resource, path & BackwardStreamName, local);
        VirtualResource.T.deleteFile(resource, path & LabelTableName, local);
        VirtualResource.T.deleteFile(resource, path & CheckpointTreeName, local);
        VirtualResource.T.deleteFile(resource, path & BStackName, local);
        VirtualResource.T.deleteFile(resource, path & FStackName, local);
      EXCEPT
        PageFile.NoAccess =>
        (* ignore.  someone else works on the graph *)
      | VirtualResource.FatalError (info) =>
          RAISE Super.InternalError(ErrorSupport.Propagate(
                                      "PersistentLog.Close",
                                      "VirtualResource.FatalError", info));
      END;
    END;
  END Close;

PROCEDURE DeleteOldLogFiles ( res    : VirtualResource.T; 
                              path   : Pathname.T;
                              local  : BOOLEAN            )
  RAISES {Super.InternalError, PageFile.NoAccess} =
  BEGIN
    TRY
      IF res.existsFile(path & ForwardStreamName, local) THEN
        res.deleteFile(path & ForwardStreamName, local);
      END;
      IF res.existsFile(path & BackwardStreamName, local) THEN
        res.deleteFile(path & BackwardStreamName, local);
      END;
      IF res.existsFile(path & LabelTableName, local) THEN
        res.deleteFile(path & LabelTableName, local);
      END;
      IF res.existsFile(path & CheckpointTreeName, local) THEN
        res.deleteFile(path & CheckpointTreeName, local);
      END;
      IF res.existsFile(path & BStackName, local) THEN
        res.deleteFile(path & BStackName, local);
      END;
      IF res.existsFile(path & FStackName, local) THEN
        res.deleteFile(path & FStackName, local);
      END;
    EXCEPT
      VirtualResource.FatalError (info) =>
        RAISE
          Super.InternalError(
            ErrorSupport.Propagate("PersistentLog.DeleteOldLogFiles",
                                   "VirtualResource.FatalError", info));
    END;
  END DeleteOldLogFiles;

PROCEDURE LogFilesExist ( res   : VirtualResource.T; 
                          path  : Pathname.T;
                          local : BOOLEAN            ) : BOOLEAN
  RAISES {Super.InternalError} =
  BEGIN
    TRY
      RETURN res.existsFile(path & ForwardStreamName, local);
    EXCEPT
      VirtualResource.FatalError (info) =>
        RAISE
          Super.InternalError(
            ErrorSupport.Propagate("PersistentLog.DeleteOldLogFiles",
                                   "VirtualResource.FatalError", info));
    END;
  END LogFilesExist; 

PROCEDURE LogFilesInUse ( res   : VirtualResource.T; 
                          path  : Pathname.T;
                          local : BOOLEAN            ) : BOOLEAN
  RAISES {Super.InternalError} =
  BEGIN
    TRY
      RETURN res.fileInUse(path & ForwardStreamName, local);
    EXCEPT
      VirtualResource.FatalError (info) =>
        RAISE
          Super.InternalError(
            ErrorSupport.Propagate("PersistentLog.DeleteOldLogFiles",
                                   "VirtualResource.FatalError", info));
    END;
  END LogFilesInUse; 

BEGIN
END PersistentLog.
