MODULE VirtualResource EXPORTS VirtualResource, InternalVirtualResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.22  1998/08/27 12:27:22  roland
    When copyinf files, make sure the path for the destination exists.

    Revision 1.21  1998/03/18 12:13:36  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.20  1998/03/17 14:14:41  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.19  1997/10/31 14:14:50  roland
    Adapted to new RuleEngine.

    Revision 1.18  1997/06/13 16:11:28  roland
    Bugfix: ClosedFileStack was corrupted in FindFile.

    Revision 1.17  1997/06/13 12:00:38  rbnix
        Adapted to simplified file handling with methods
        getPath and makeFileName.

    Revision 1.16  1997/06/10 12:54:23  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.15  1997/06/09 10:39:16  roland
    TextSet needs implementation. Here TextSetDef.

    Revision 1.14  1997/06/09 10:21:22  roland
    Files deleted in the same transaction they were closed will be deleted
    only after commit of the top-level-transaction.

    Revision 1.13  1997/06/06 16:10:42  roland
    Bugfix in administration of files closed within transactions. Tables
    are now used to keep track of the names of the files, because
    exception in ScheduleClientResource made it impossible to check for
    object identity.

    Revision 1.12  1997/05/16 08:48:53  roland
    Stack exceptions will never be raised, hence open methods need not raise
    FatalError.

    Revision 1.11  1997/05/09 16:27:03  renehuel
    The files have been changed to enable transaction semantic on closing
    of remote files. You may now close a graph within a transaction without
    an exception to be raised, and the final closing of the file
    Depends On the following action : a commit (of the top level
    transaction) closes the file, an abort aborts and leaves the resource
    still open.

    Revision 1.10  1997/04/24 12:13:04  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.9  1997/04/04 14:16:00  renehuel
    The type has been enhanced for handling of local files.
    The methods copyFile, deleteFile, renameFile, fileSize, existsFile and
    fileInUse now have an additional parameter remoteFile which determines
    whether the operation is to be used on a remote or local file.

    Revision 1.8  1997/03/25 12:06:53  rbnix
        Bug fixed: full pair of PageCache.BeginAccess and
        PageCache.EndAccess must be executed wether exceptions are
        raised or not.

    Revision 1.7  1996/11/21 07:54:48  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.6  1996/11/18 17:52:26  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.5  1996/11/14 14:13:08  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.4  1996/09/09 11:46:31  rbnix
        Handling of resource events from ClientScheduler inherited.

    Revision 1.3  1996/08/06 16:34:22  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/08/01 18:09:35  rbnix
        New file administration methods for remote files added:
        deleteFile, copyFile, renameFile, existsFile, fileInUse,
        fileSize and getFiles.

    Revision 1.2.2.1  1996/06/13 12:49:32  rbnix
        Method getID added to relate files to current client.

    Revision 1.2  1996/03/11 17:25:50  rbnix
        Method ScheduledClientFile.close is replaced by
        ScheduledClientResource.closeRemoteFile due to additional
        clean up. Therefore VirtualResource must redirect the
        operation.

    Revision 1.1  1996/02/29 17:44:33  rbnix
        First version of subsystem VirtualPages giving transparent
        access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualResource ----------------------------------------------------

 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, TextSeq, Text;
IMPORT PageFile, PageFileSystem, PageCache, Access, Transaction,
       VirtualPageEvent, ScheduledClientRessource, ScheduledClientFile,
       BaseScheduledClientRessource, ErrorSupport, ClientInfoSeq,
       ScheduledClientFileTblStack, ScheduledClientFileTbl,
       InternalBaseScheduledClientFile, TextSetDef, TextSet, RuleEngine;

REVEAL
  T =
    Internal BRANDED OBJECT
      scheduledRessource: ScheduledClientRessource.T;
      notifier          : CommitNotifier;
      localFileList     : TextSeq.T;
      closeFileStack: ScheduledClientFileTblStack.T;  (* holds all closed
                                                         files ordered by
                                                         transactuion
                                                         depth *)
      closedFiles, deletedFiles: TextSetDef.T;  (* hold all closed/deleted
                                                   files *)
      deletedFileStack: TextSetList;  (* hold all deleted Files ordered by
                                         transaction level *)

      ruleEngineId: CARDINAL;

    OVERRIDES
      open  := Open;
      close := Close;

      beginTransaction    := BeginTransaction;
      commitTransaction   := CommitTransaction;
      abortTransaction    := AbortTransaction;
      getTransactionLevel := GetTransactionLevel;

      getAccessMode   := GetAccessMode;
      getBaseName     := GetBaseName;
      getPath         := GetPath;
      makeFileName    := MakeFileName;
      getID           := GetID;
      getRuleEngineID := GetRuleEngineID;

      openRemoteFile      := OpenRemoteFile;
      closeRemoteFile     := CloseRemoteFile;
      registerLocalFile   := RegisterLocalFile;
      unregisterLocalFile := UnregisterLocalFile;

      deleteFile  := DeleteFile;
      copyFile    := CopyFile;
      renameFile  := RenameFile;
      existsFile  := ExistsFile;
      fileInUse   := FileInUse;
      getFileUser := GetFileUser;
      fileSize    := FileSize;
      getFiles    := GetFiles;
    END;

(* Stacks are never full and undefined only if resource is not open *)
<* FATAL ScheduledClientFileTblStack.Undefined,
         ScheduledClientFileTblStack.Full *>

PROCEDURE Open (self    : T;
                baseName: Pathname.T;
                access  : Access.Mode;
                new     : BOOLEAN      ): T
  RAISES {Access.Denied, PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess();
    TRY
      IF new AND access = Access.Mode.ReadOnlyShared THEN
        RAISE
          PageFile.NoAccess("Can't open new resource in read only mode!");
      END;

      TRY
        self.notifier := NEW(CommitNotifier).init(self);
        self.scheduledRessource := NEW(ScheduledClientRessource.T).init(
                                     baseName, access, new, self.notifier);
        self.localFileList := NEW(TextSeq.T).init();
        self.closeFileStack := NEW(ScheduledClientFileTblStack.T).init();
        self.deletedFileStack := NIL;
        self.deletedFiles := NEW(TextSetDef.T).init();
        self.closedFiles := NEW(TextSetDef.T).init();
        self.ruleEngineId := RuleEngine.RegisterTransactionUnit();
      EXCEPT
        ScheduledClientRessource.FatalError (info) =>
          RAISE PageFile.NoAccess(ErrorSupport.ToText(info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN self;
  END Open;


PROCEDURE Close (self: T) RAISES {FatalError} =
  BEGIN
    PageCache.BeginAccess();
    TRY
      TRY
        self.scheduledRessource.close();
      EXCEPT
        ScheduledClientRessource.FatalError (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "VirtualResource.Close",
                             "ScheduledClientRessource.FatalError", info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;
  END Close;



PROCEDURE BeginTransaction (self: T) RAISES {FatalError} =
  BEGIN
    VirtualPageEvent.SignalBegin(
      self.ruleEngineId, self.scheduledRessource.getBaseName(), self,
      isPreEvent := TRUE, level := GetTransactionLevel(self) + 1);
    PageCache.BeginAccess();
    TRY
      TRY
        self.scheduledRessource.startTransaction();
        (* Creation of a new empty file list on top of the
           closeFileStack. *)
        self.closeFileStack.push(
          NEW(ScheduledClientFileTbl.Default).init());
        TSLPush(self.deletedFileStack, NEW(TextSetDef.T).init());
      EXCEPT
      | ScheduledClientRessource.FatalError (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "VirtualResource.BeginTransaction",
                             "ScheduledClientRessource.FatalError", info));
      END
    FINALLY
      PageCache.EndAccess();
    END;
    RuleEngine.NotifyBeginTransaction(self.ruleEngineId);
    VirtualPageEvent.SignalBegin(
      self.ruleEngineId, self.scheduledRessource.getBaseName(), self,
      isPreEvent := FALSE, level := GetTransactionLevel(self));
  END BeginTransaction;


PROCEDURE CommitTransaction (self: T)
  RAISES {NotInTransaction, FatalError} =
  BEGIN
    VirtualPageEvent.SignalCommit(
      self.ruleEngineId, self.scheduledRessource.getBaseName(), self,
      isPreEvent := TRUE, level := GetTransactionLevel(self));
    RuleEngine.PreCommitTransaction(self.ruleEngineId);
    PageCache.BeginAccess();
    TRY
      TRY
        self.scheduledRessource.commitTransaction();
        (* When doing a commit, all files which have been entered into the
           list on top of the closeFileStack have to be merged with the
           list below.  If the entry was the last entry in the stack, all
           files in the list have to be closed. *)
        MergeOrClose(self);
      EXCEPT
      | ScheduledClientRessource.NotInTransaction =>
          RAISE NotInTransaction;
      | ScheduledClientRessource.FatalError (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "VirtualResource.CommitTransaction",
                             "ScheduledClientRessource.FatalError", info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;
    RuleEngine.PostCommitTransaction(self.ruleEngineId);
    VirtualPageEvent.SignalCommit(
      self.ruleEngineId, self.scheduledRessource.getBaseName(), self,
      isPreEvent := FALSE, level := GetTransactionLevel(self) + 1);
  END CommitTransaction;


PROCEDURE AbortTransaction (self: T)
  RAISES {NotInTransaction, FatalError} =
  VAR
    name: Pathname.T;
    file: ScheduledClientFile.T;
  BEGIN
    VirtualPageEvent.SignalAbort(
      self.ruleEngineId, self.scheduledRessource.getBaseName(), self,
      isPreEvent := TRUE, level := GetTransactionLevel(self));
    PageCache.BeginAccess();
    TRY
      TRY
        self.scheduledRessource.abortTransaction();
        (* On aborting the current transaction, all files which have been
           entered into the list may stay opened.  The top element of the
           closeFileStack can be abandoned. *)
        WITH closed = self.closeFileStack.pop(),
             it     = closed.iterate()           DO
          (* remove closed files of this transaction from the set of all
             closed files *)
          WHILE it.next(name, file) DO
            EVAL self.closedFiles.delete(name);
          END;
        END;
        WITH deleted = TSLPop(self.deletedFileStack) DO
          (* remove deleted files of this transaction from the set of all
             closed files *)
          EVAL self.deletedFiles.diffD(deleted);
        END;
      EXCEPT
        (* This exception cannot be raised here. *)
      | ScheduledClientFileTblStack.Empty =>
      | ScheduledClientFileTblStack.Undefined =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "VirtualResource.AbortTransaction",
                         "ScheduledClientFileTblStack.Undefined", NIL));
      | ScheduledClientRessource.NotInTransaction =>
          RAISE NotInTransaction;
      | ScheduledClientRessource.FatalError (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "VirtualResource.AbortTransaction",
                             "ScheduledClientRessource.FatalError", info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;
    RuleEngine.NotifyAbortTransaction(self.ruleEngineId);
    VirtualPageEvent.SignalAbort(
      self.ruleEngineId, self.scheduledRessource.getBaseName(), self,
      isPreEvent := FALSE, level := GetTransactionLevel(self) + 1);
  END AbortTransaction;


PROCEDURE GetTransactionLevel (self: T): Transaction.Level =
  VAR level: Transaction.Level;
  BEGIN
    PageCache.BeginAccess();
    level := self.scheduledRessource.getTransactionLevel();
    PageCache.EndAccess();

    RETURN level;
  END GetTransactionLevel;


PROCEDURE GetAccessMode (self: T): Access.Mode =
  VAR access: Access.Mode;
  BEGIN
    PageCache.BeginAccess();
    access := self.scheduledRessource.getAccessMode();
    PageCache.EndAccess();

    RETURN access;
  END GetAccessMode;


PROCEDURE GetBaseName (self: T): Pathname.T =
  VAR baseName: Pathname.T;
  BEGIN
    PageCache.BeginAccess();
    baseName := self.scheduledRessource.getBaseName();
    PageCache.EndAccess();

    RETURN baseName;
  END GetBaseName;


PROCEDURE GetPath (self: T; temporary: BOOLEAN): Pathname.Arcs =
  VAR path: Pathname.Arcs;
  BEGIN
    PageCache.BeginAccess();
    path := self.scheduledRessource.getPath(temporary);
    PageCache.EndAccess();

    RETURN path;
  END GetPath;

PROCEDURE MakeFileName (self: T; baseName: Pathname.T; temporary: BOOLEAN):
  Pathname.T RAISES {PageFile.NoAccess} =
  VAR name: Pathname.T;
  BEGIN
    TRY
      PageCache.BeginAccess();
      name := self.scheduledRessource.makeFileName(baseName, temporary);
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN name;
  END MakeFileName;


PROCEDURE GetID (self: T): TEXT =
  VAR id: TEXT;
  BEGIN
    PageCache.BeginAccess();
    id := self.scheduledRessource.getID();
    PageCache.EndAccess();

    RETURN id;
  END GetID;

PROCEDURE GetRuleEngineID (self: T): CARDINAL =
  BEGIN
    RETURN self.ruleEngineId;
  END GetRuleEngineID;

PROCEDURE OpenRemoteFile (self    : T;
                          baseName: Pathname.T;
                          mode    : Access.Mode;
                          kind    : Access.Kind;
                          new     : BOOLEAN      ): ScheduledClientFile.T
  RAISES {Access.Denied, PageFile.NoAccess} =
  VAR file: ScheduledClientFile.T := NIL;

  PROCEDURE FindFile (    self    : T;
                          baseName: Pathname.T;
                      VAR file    : ScheduledClientFile.T) =
    VAR
      hCloseStack := NEW(ScheduledClientFileTblStack.T).init();
      fileTbl: ScheduledClientFileTbl.T;
      found  : BOOLEAN                  := FALSE;
    BEGIN
      TRY
        (* search in all elements of the closeFileStack *)
        WHILE NOT found AND NOT self.closeFileStack.isEmpty() DO
          fileTbl := self.closeFileStack.pop();
          IF fileTbl.get(baseName, file) THEN
            found := TRUE;
            EVAL fileTbl.delete(baseName, file)
          END;
          hCloseStack.push(fileTbl);
        END;
        (* put everything back on the closeFileStack *)
        WHILE NOT hCloseStack.isEmpty() DO
          self.closeFileStack.push(hCloseStack.pop());
        END;
        IF NOT found THEN file := NIL END;
      EXCEPT
        (* This exceptions cannot be raised here. *)
      | ScheduledClientFileTblStack.Empty =>
      END;
    END FindFile;

  BEGIN
    PageCache.BeginAccess();
    TRY
      TRY
        (* An empty stack indicates that the opening operation has been
           done without a transaction, so it has not to be checked if the
           file has already been marked for closing. *)
        IF NOT self.closeFileStack.isEmpty()
             AND self.closedFiles.member(baseName) THEN
          (* If the file has been entered into the tables of the files to
             be closed, then it has to be removed from there.*)
          IF self.deletedFiles.member(baseName) THEN
            (* This file was closed and deleted within the running
               top-level transaction; we cannot open again, because server
               hasn't realized the changes yet. *)
            RAISE PageFile.NoAccess("File scheduled for deletion!\n");
          END;
          FindFile(self, baseName, file);
          EVAL self.closedFiles.delete(baseName);
        ELSE
          (* if no transaction is running or the file wasn't closed before,
             open it regularly *)
          file := self.scheduledRessource.openRemoteFile(
                    baseName, mode, kind, new);
        END
      EXCEPT
      | ScheduledClientRessource.FatalError (info) =>
          RAISE PageFile.NoAccess(ErrorSupport.ToText(info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN file;
  END OpenRemoteFile;


PROCEDURE CloseRemoteFile (self: T; file: ScheduledClientFile.T)
  RAISES {FatalError} =
  VAR fileTbl: ScheduledClientFileTbl.T;
  BEGIN
    PageCache.BeginAccess();
    TRY
      TRY
        (* When the closeFileStack is not empty, the close operation is
           done within a transaction.  The file cannot be closed
           immediately, but has to be entered into the top table of the
           closeFileStack, waiting for the commit of the top level
           transaction or an abortTransaction. *)
        IF NOT self.closeFileStack.isEmpty() THEN
          IF self.closedFiles.member(file.getBaseName()) THEN
            (* file was already closed *)
            RAISE FatalError(ErrorSupport.Create(
                               "VirtualResource.CloseRemoteFile",
                               "File '" & file.getBaseName()
                                 & "'already scheduled for closing!"));
          END;
          fileTbl := self.closeFileStack.top();
          EVAL fileTbl.put(file.getBaseName(), file);
          EVAL self.closedFiles.insert(file.getBaseName());
        ELSE
          (* An empty closeFileStack indicates that the close operation was
             done without an transaction, so the file can be closed
             immediately. *)
          self.scheduledRessource.closeRemoteFile(file);
        END;
      EXCEPT
      | ScheduledClientRessource.FatalError (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "VirtualResource.CloseRemoteFile",
                             "ScheduledClientRessource.FatalError", info));
        (* This exception cannot be raised. *)
      | ScheduledClientFileTblStack.Empty =>
      END;
    FINALLY
      PageCache.EndAccess();
    END;
  END CloseRemoteFile;


PROCEDURE RegisterLocalFile (self: T; baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    PageCache.BeginAccess();
    TRY
      WITH fileName = LocalFileName(self, baseName) DO
        TRY
          IF NOT LocalFileInUse(self, fileName) THEN
            self.localFileList.addhi(fileName);
            self.scheduledRessource.registerLocalFile(fileName);
          ELSE
            RAISE
              PageFile.NoAccess("Can't open file which is already open!");
          END
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE
              FatalError(ErrorSupport.Propagate(
                           "VirtualResource.RegisterLocalFile",
                           "ScheduledClientRessource.FatalError", info));
        END;
      END;
    FINALLY
      PageCache.EndAccess();
    END
  END RegisterLocalFile;


PROCEDURE UnregisterLocalFile (self: T; baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  VAR newLocalFileList: TextSeq.T;
  BEGIN
    PageCache.BeginAccess();
    TRY
      WITH fileName = LocalFileName(self, baseName) DO
        TRY
          IF LocalFileInUse(self, fileName) THEN
            newLocalFileList := NEW(TextSeq.T).init();
            FOR i := 1 TO self.localFileList.size() DO
              IF NOT Text.Equal(self.localFileList.get(i - 1), fileName) THEN
                newLocalFileList.addhi(self.localFileList.get(i - 1))
              END
            END;
            self.localFileList := newLocalFileList;
            self.scheduledRessource.unregisterLocalFile(fileName);
          ELSE
            RAISE PageFile.NoAccess("Can't close a file which isn't open!")
          END
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "VirtualResource.UnregisterLocalFile",
                      "BaseScheduledClientRessource.FatalError", info));
        END;
      END;
    FINALLY
      PageCache.EndAccess();
    END
  END UnregisterLocalFile;


PROCEDURE DeleteFile (self: T; baseName: Pathname.T; local: BOOLEAN)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      PageCache.BeginAccess();
      IF NOT local THEN
        (* code for remote files *)
        TRY
          (* delete only if file wasn't closed in the same top-level
             transaction *)
          IF NOT self.closedFiles.member(baseName) THEN
            self.scheduledRessource.deleteFile(baseName);
          ELSIF NOT self.deletedFiles.member(baseName) THEN
            (* keep file scheduled for deletion until end of top-level
               transaction *)
            EVAL self.deletedFiles.insert(baseName);
            EVAL TSLTop(self.deletedFileStack).insert(baseName);
          ELSE
            RAISE FatalError(ErrorSupport.Create(
                               "VirtualResource.DeleteFile",
                               "File '" & baseName
                                 & "' already scheduled for deletion!"));
          END;
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "VirtualResource.DeleteFile",
                      "BaseScheduledClientRessource.FatalError", info));
        END;
      ELSE
        (* code for local files *)
        WITH fileName = LocalFileName(self, baseName) DO
          IF NOT LocalFileInUse(self, fileName) THEN
            PageFileSystem.DeleteFile(fileName)
          ELSE
            RAISE PageFile.NoAccess(
                    "Can't delete a file which is still opened!");
          END;
        END;
      END;
    FINALLY
      PageCache.EndAccess();
    END
  END DeleteFile;


PROCEDURE CopyFile (self      : T;
                    sourceName: Pathname.T;
                    destName  : Pathname.T;
                    local     : BOOLEAN     )
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      PageCache.BeginAccess();
      IF NOT local THEN
        (* code for remote files *)
        TRY
          self.scheduledRessource.copyFile(sourceName, destName);
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "VirtualResource.CopyFile",
                      "BaseScheduledClientRessource.FatalError", info));
        END;
      ELSE
        (* code for local files *)
        WITH localSource = LocalFileName(self, sourceName),
             localDest   = LocalFileName(self, destName)    DO
          IF NOT (LocalFileInUse(self, localSource)
                    OR LocalFileInUse(self, localDest)) THEN
            (* make sure directory exists *)
            PageFileSystem.MakePath(Pathname.Prefix(localDest));
            
            PageFileSystem.CopyFile(localSource, localDest)
          ELSE
            RAISE
              PageFile.NoAccess("Can't copy a file which is still opened");
          END
        END;
      END;
    FINALLY
      PageCache.EndAccess();
    END
  END CopyFile;

PROCEDURE RenameFile (self   : T;
                      oldName: Pathname.T;
                      newName: Pathname.T;
                      local  : BOOLEAN     )
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      PageCache.BeginAccess();
      IF NOT local THEN
        (* code for remote files *)
        TRY
          self.scheduledRessource.renameFile(oldName, newName);
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "VirtualResource.RenameFile",
                      "BaseScheduledClientRessource.FatalError", info));
        END;
      ELSE
        (* code for local files *)
        WITH localOld = LocalFileName(self, oldName),
             localNew = LocalFileName(self, newName)  DO
          IF NOT (LocalFileInUse(self, localOld)
                    OR LocalFileInUse(self, localNew)) THEN
            PageFileSystem.RenameFile(localOld, localNew)
          ELSE
            RAISE PageFile.NoAccess(
                    "Can't rename a file which is still opened");
          END;
        END;
      END;
    FINALLY
      PageCache.EndAccess();
    END
  END RenameFile;


PROCEDURE ExistsFile (self: T; baseName: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {FatalError} =
  VAR result: BOOLEAN;
  BEGIN
    TRY
      PageCache.BeginAccess();
      IF NOT local THEN
        (* code for remote files *)
        TRY
          result := self.scheduledRessource.existsFile(baseName);
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "VirtualResource.ExistsFile",
                      "BaseScheduledClientRessource.FatalError", info));
        END;
      ELSE
        (* code for local files *)
        WITH fileName = LocalFileName(self, baseName) DO
          result := PageFileSystem.ExistsFile(fileName);
        END;
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END ExistsFile;


PROCEDURE FileInUse (self: T; baseName: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {FatalError} =
  VAR result: BOOLEAN;
  BEGIN
    TRY
      PageCache.BeginAccess();
      TRY
        IF NOT local THEN
          (* code for remote files *)
          result := self.scheduledRessource.fileInUse(baseName)
        ELSE
          (* code for local files *)
          result := LocalFileInUse(self, LocalFileName(self, baseName));
        END
      EXCEPT
        BaseScheduledClientRessource.FatalError (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "VirtualResource.FileInUse",
                         "BaseScheduledClientRessource.FatalError", info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END FileInUse;

PROCEDURE GetFileUser (self: T; baseName: Pathname.T): ClientInfoSeq.T
  RAISES {FatalError} =
  VAR result: ClientInfoSeq.T;
  BEGIN
    TRY
      PageCache.BeginAccess();
      TRY
        result := self.scheduledRessource.getFileUser(baseName);
      EXCEPT
        BaseScheduledClientRessource.FatalError (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "VirtualResource.GetFileUser",
                         "BaseScheduledClientRessource.FatalError", info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END GetFileUser;


PROCEDURE FileSize (self: T; baseName: Pathname.T; local: BOOLEAN := FALSE):
  CARDINAL RAISES {PageFile.NoAccess, FatalError} =
  VAR result: CARDINAL;
  BEGIN
    TRY
      PageCache.BeginAccess();
      IF NOT local THEN
        (* code for remote files *)
        TRY
          result := self.scheduledRessource.fileSize(baseName);
        EXCEPT
          BaseScheduledClientRessource.FatalError (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "VirtualResource.FileSize",
                      "BaseScheduledClientRessource.FatalError", info));
        END;
      ELSE
        (* code for local files *)
        result := PageFileSystem.FileSize(LocalFileName(self, baseName))
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END FileSize;


PROCEDURE GetFiles (self: T): TextSeq.T RAISES {FatalError} =
  VAR result: TextSeq.T;
  BEGIN
    TRY
      PageCache.BeginAccess();
      TRY
        result := self.scheduledRessource.getFiles();
      EXCEPT
        BaseScheduledClientRessource.FatalError (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "VirtualResource.GetFiles",
                         "BaseScheduledClientRessource.FatalError", info));
      END;
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END GetFiles;

(* internal *)

TYPE
  CommitNotifier = ScheduledClientRessource.Notifier OBJECT
                     resource: T;
                   METHODS
                     init (res: T): CommitNotifier := CommitNotifierInit;
                   OVERRIDES
                     notify := RemoteCommitNotify;
                   END;

PROCEDURE CommitNotifierInit (n: CommitNotifier; res: T): CommitNotifier =
  BEGIN
    n.resource := res;
    RETURN n;
  END CommitNotifierInit;

PROCEDURE RemoteCommitNotify (n: CommitNotifier) =
  BEGIN
    VirtualPageEvent.SignalRemoteCommit(
      n.resource.ruleEngineId, n.resource.scheduledRessource.getBaseName(),
      n.resource);
  END RemoteCommitNotify;

PROCEDURE LocalFileInUse (self: T; baseName: Pathname.T): BOOLEAN =
  (* This function is for internal use only.  Within the methods of T one
     cannot call the method fileInUse cause the call of
     PageCache.BeginAccess() locks a thread.  Calling the method fileInUse
     causes a deadlock cause this method also tries to lock the same thread
     with PageCache.BeginAccess. *)
  VAR result: BOOLEAN;
  BEGIN
    result := FALSE;
    FOR i := 1 TO self.localFileList.size() DO
      result :=
        result OR Text.Equal(self.localFileList.get(i - 1), baseName)
    END;
    RETURN result
  END LocalFileInUse;

PROCEDURE LocalFileName (res: T; name: Pathname.T): Pathname.T =
  (* Check whether path is absolut.  If so return path.  If not, prepend
     rootpath to path and return the extended path. *)
  <* FATAL Pathname.Invalid *>
  BEGIN
    IF Pathname.Absolute(name) THEN
      RETURN name;
    ELSE
      RETURN
        Pathname.Join(Pathname.Compose(
                        res.scheduledRessource.getPath(FALSE)), name, NIL);
    END;
  END LocalFileName;


PROCEDURE MergeOrClose (self: T) RAISES {FatalError} =
  (* This procedure is used to remove the top element of the
     closeFileStack, and to determine whether it was the last element, or
     it has to be merged with the next one, *)


  PROCEDURE CloseAndDeleteFiles (self: T; fileTbl: ScheduledClientFileTbl.T)
    RAISES {FatalError} =
    VAR
      file: ScheduledClientFile.T;
      name: Pathname.T;
    BEGIN
      TRY
        WITH it = fileTbl.iterate() DO
          WHILE it.next(name, file) DO
            (* remove from global set *)
            EVAL self.closedFiles.delete(name);
            (* close *)
            self.scheduledRessource.closeRemoteFile(file);
          END;
        END;
        WITH it = self.deletedFiles.iterate() DO
          WHILE it.next(name) DO
            self.scheduledRessource.deleteFile(name);
          END;
          self.deletedFiles := NEW(TextSetDef.T).init();
        END;
      EXCEPT
      | ScheduledClientRessource.FatalError (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "VirtualResource.CloseAndDeleteFiles",
                             "ScheduledClientRessource.FatalError", info));
      | BaseScheduledClientRessource.FatalError (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "VirtualResource.CloseAndDeleteFiles",
                         "BaseScheduledClientRessource.FatalError", info));
      | PageFile.NoAccess (msg) =>
          RAISE FatalError(ErrorSupport.Create(
                             "VirtualResource.CloseAndDeleteFiles",
                             "PageFile.NoAccess: " & msg));
      END
    END CloseAndDeleteFiles;


  PROCEDURE MergeLists (tab1, tab2: ScheduledClientFileTbl.T) =
    VAR
      file: ScheduledClientFile.T;
      name: Pathname.T;
    BEGIN
      WITH it = tab1.iterate() DO
        WHILE it.next(name, file) DO EVAL tab2.put(name, file) END
      END;
    END MergeLists;


  VAR
    topElement, secondElement: ScheduledClientFileTbl.T;
    deleted                  : TextSetDef.T;
  BEGIN                          (* MergeOrClose *)
    TRY
      (* The first element is popped from the stack, and the closeFileStack
         is checked for emtpiness. *)
      topElement := self.closeFileStack.pop();
      deleted := TSLPop(self.deletedFileStack);
      IF self.closeFileStack.isEmpty() THEN
        (* When the stack is empty the committed transaction was the top
           level transaction, and so the files in the topElement can
           finally be closed. *)
        CloseAndDeleteFiles(self, topElement)
      ELSE
        (* If it was not the top level transaction, the topElement list has
           to be merged with the current top of the stack. *)
        secondElement := self.closeFileStack.top();
        MergeLists(topElement, secondElement);
        EVAL TSLTop(self.deletedFileStack).unionD(deleted);
      END;
    EXCEPT
      (* This exception cannot be raised here. *)
    | ScheduledClientFileTblStack.Empty =>
    END
  END MergeOrClose;


TYPE
  TextSetList = REF RECORD
                      set : TextSet.T;
                      next: TextSetList;
                    END;

PROCEDURE TSLPush (VAR list: TextSetList; set: TextSet.T) =
  BEGIN
    list := NewTSL(set := set, next := list);
  END TSLPush;

PROCEDURE TSLPop (VAR list: TextSetList): TextSet.T =
  VAR
    set: TextSet.T   := NIL;
    h  : TextSetList;
  BEGIN
    IF list # NIL THEN
      h := list;
      list := list.next;
      set := h.set;
      DisposeTSL(h);
    END;
    RETURN set;
  END TSLPop;

PROCEDURE TSLTop (list: TextSetList): TextSet.T =
  BEGIN
    IF list # NIL THEN RETURN list.set; ELSE RETURN NIL END;
  END TSLTop;

VAR FreeTSL: TextSetList := NIL; (* free memeory list *)

PROCEDURE NewTSL (set: TextSet.T; next: TextSetList): TextSetList =
  VAR h: TextSetList;
  BEGIN
    IF FreeTSL # NIL THEN
      h := FreeTSL;
      FreeTSL := FreeTSL.next;
      h.set := set;
      h.next := next;
    ELSE
      h := NEW(TextSetList, set := set, next := next);
    END;
    RETURN h;
  END NewTSL;

PROCEDURE DisposeTSL (elem: TextSetList) =
  BEGIN
    elem.next := FreeTSL;
    elem.set := NIL;
    FreeTSL := elem;
  END DisposeTSL;

BEGIN
END VirtualResource.
