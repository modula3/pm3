MODULE ScheduledClientRessource EXPORTS ScheduledClientRessource,
                                        InternalScheduledClientRessource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.11  1997/10/31 14:13:28  roland
    Adapted to new RuleEngine.

    Revision 1.10  1997/06/10 12:53:47  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.9  1997/04/24 12:12:37  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.8  1996/11/20 12:25:25  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.7  1996/11/18 17:51:52  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.6  1996/11/14 14:12:46  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.5  1996/10/29 15:01:49  rbnix
        New parameter for page age added.

    Revision 1.4  1996/10/02 15:06:15  rbnix
        Bug fixed (???) in ReleaseCallback: assertion to guarante
        handling of all release callbacks is given up. This is made
        because of possible unhandled callbacks at the same time
        when closing the file. (It is not possible to handle all
        closing related operations atomic, this violates a
        precondition for the server.)

    Revision 1.3  1996/09/09 11:39:37  rbnix
        Generation of resource events on begin/end of transaction
        added.

    Revision 1.2  1996/03/11 17:21:33  rbnix
        Method closeRemoteFile added to close file and delete entry
        out of collection.

    Revision 1.1  1996/02/09 16:47:02  rbnix
        First version of client scheduler added.

*)
(***************************************************************************)
(*
 | --- ScheduledClientRessource -------------------------------------------

 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledClientRessource AS Super;
IMPORT Pathname, PageFile, PageLock, Access, Txn, RemoteFile,
       CommunicationSeq, InternalBaseScheduledClientFile,
       ScheduledClientFile, InternalScheduledClientFile,
       ScheduledClientFileTbl, ErrorSupport, CallbackPort;

REVEAL
  T = Internal BRANDED OBJECT
        files: ScheduledClientFileTbl.T;
        remoteCommitNotifier: Notifier;

      OVERRIDES
        init  := Init;
        close := Close;

        openRemoteFile  := OpenRemoteFile;
        closeRemoteFile := CloseRemoteFile;

        startTransaction  := StartTransaction;
        commitTransaction := CommitTransaction;
        chainTransaction  := ChainTransaction;
        abortTransaction  := AbortTransaction;

        releaseCallback   := ReleaseCallback;
        propagateCallback := PropagateCallback;

      END;


PROCEDURE Init (self    : T;
                baseName: Pathname.T;
                access  : Access.Mode;
                new     : BOOLEAN;
                notifier: Notifier): Super.T
  RAISES {Access.Denied, PageFile.NoAccess, FatalError} =
  BEGIN
    self.files := NEW(ScheduledClientFileTbl.Default).init();
    self.remoteCommitNotifier := notifier;

    TRY
      RETURN Super.T.init(self, baseName, access, new);
    EXCEPT
      Super.FatalError (info) =>
        RAISE FatalError(
                ErrorSupport.Propagate("ScheduledClientRessource.Init",
                                       "Super.FatalError", info));
    END;
  END Init;


PROCEDURE Close (self: T) RAISES {FatalError} =
  BEGIN
    IF Txn.EnvelopeLevel < self.getTransactionLevel() THEN
      RAISE
        FatalError(ErrorSupport.Create("ScheduledClientRessource.Close",
                                       "Pending transaction(s)"));
    END;

    TRY
      Super.T.close(self);
    EXCEPT
      Super.FatalError (info) =>
        RAISE FatalError(
                ErrorSupport.Propagate("ScheduledClientRessource.Close",
                                       "Super.FatalError", info));
    END;
  END Close;


PROCEDURE OpenRemoteFile (self    : T;
                          baseName: Pathname.T;
                          mode    : Access.Mode;
                          kind    : Access.Kind;
                          new     : BOOLEAN      ): ScheduledClientFile.T
  RAISES {Access.Denied, PageFile.NoAccess, FatalError} =
  VAR file: ScheduledClientFile.T;
  BEGIN
    IF self.files.get(baseName, file) THEN
      RAISE FatalError(ErrorSupport.Create(
                         "ScheduledClientRessource.OpenRemoteFile",
                         "Already open."));
    END;

    TRY
      file :=
        NEW(ScheduledClientFile.T).open(self, baseName, mode, kind, new);
    EXCEPT
      ScheduledClientFile.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.OpenRemoteFile",
                           "ScheduledClientFile.FatalError", info));
    END;
    EVAL self.files.put(baseName, file);

    RETURN file;
  END OpenRemoteFile;


PROCEDURE CloseRemoteFile (self: T; file: ScheduledClientFile.T)
  RAISES {FatalError} =
  BEGIN
    EVAL self.files.delete(file.getBaseName(), file);

    TRY
      file.close();
    EXCEPT
      ScheduledClientFile.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.CloseRemoteFile",
                           "ScheduledClientFile.FatalError", info));
    END;
  END CloseRemoteFile;


PROCEDURE StartTransaction (self: T) RAISES {FatalError} =
  BEGIN
    (*
      empty,
      no initializing to clear lock tables neccessary
    *)
    TRY
      Super.T.startTransaction(self);
    EXCEPT
      Super.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.StartTransaction",
                           "Super.FatalError", info));
    END;

  END StartTransaction;


PROCEDURE CommitTransaction (self: T)
  RAISES {NotInTransaction, FatalError} =
  VAR
    baseName: Pathname.T;
    file    : ScheduledClientFile.T;
    i       : ScheduledClientFileTbl.Iterator;
  BEGIN
    TRY
      i := self.files.iterate();
      WHILE i.next(baseName, file) DO file.commitTransaction(); END;
      Super.T.commitTransaction(self);
    EXCEPT
      ScheduledClientFile.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.CommitTransaction",
                           "ScheduledClientFile.FatalError", info));
    | Super.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.CommitTransaction",
                           "Super.FatalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END CommitTransaction;


PROCEDURE ChainTransaction (self: T)
  RAISES {NotInTransaction, FatalError} =
  VAR
    baseName: Pathname.T;
    file    : ScheduledClientFile.T;
    i       : ScheduledClientFileTbl.Iterator;
  BEGIN
    TRY
      i := self.files.iterate();
      WHILE i.next(baseName, file) DO file.chainTransaction(); END;
      Super.T.chainTransaction(self);
    EXCEPT
      ScheduledClientFile.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                             "ScheduledClientRessource.ChainTransaction",
                             "ScheduledClientFile.FatalError", info));
    | Super.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                             "ScheduledClientRessource.ChainTransaction",
                             "Super.FatalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END ChainTransaction;


PROCEDURE AbortTransaction (self: T)
  RAISES {NotInTransaction, FatalError} =
  VAR
    baseName: Pathname.T;
    file    : ScheduledClientFile.T;
    i       : ScheduledClientFileTbl.Iterator;
  BEGIN
    TRY
      i := self.files.iterate();
      WHILE i.next(baseName, file) DO file.abortTransaction(); END;

      Super.T.abortTransaction(self);
    EXCEPT
      ScheduledClientFile.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.AbortTransaction",
                           "ScheduledClientFile.FatalError", info));
    | Super.FatalError (info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientRessource.CommitTransaction",
                           "Super.FatalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END AbortTransaction;


PROCEDURE ReleaseCallback (self   : T;
                           file   : RemoteFile.T;
                           pageNo : CARDINAL;
                           pageAge: CARDINAL;
                           lock   : PageLock.ServerMode)
  RAISES {Access.Locked, CallbackPort.FatalError} =
  VAR
    baseName     : Pathname.T;
    scheduledFile: ScheduledClientFile.T;
    i            : ScheduledClientFileTbl.Iterator;
  BEGIN
    i := self.files.iterate();
    WHILE i.next(baseName, scheduledFile) DO
      IF scheduledFile.getOriginalMedia().getFile() = file THEN
        scheduledFile.releaseCallback(pageNo, pageAge, lock);
        RETURN;
      END;
    END;

    (*
      maybe the client call to close the file and the release callback from
      the server cross each other
    *)
    RAISE Access.Locked;
  END ReleaseCallback;


PROCEDURE PropagateCallback (self   : T;
                             end    : Txn.End;
                             entries: CommunicationSeq.T)
  RAISES {CallbackPort.FatalError} =

  VAR
    baseName: Pathname.T;
    file    : ScheduledClientFile.T;
    i       : ScheduledClientFileTbl.Iterator;
  BEGIN
    IF 0 < entries.size() THEN
      i := self.files.iterate();
      WHILE i.next(baseName, file) DO file.propagateCallback(entries); END;
    END;

    CASE end OF
    | Txn.End.Abort, Txn.End.No =>
      (* nothing to do *)

    | Txn.End.Chain =>
      (* should we do anything? *)

    | Txn.End.Commit =>
        self.remoteCommitNotifier.notify();
    END;
  END PropagateCallback;

BEGIN
END ScheduledClientRessource.
