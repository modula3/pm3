UNSAFE MODULE BaseScheduledClientRessource
                EXPORTS BaseScheduledClientRessource,
                        InternalBaseScheduledClientRessource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.31  1998/08/06 08:55:15  roland
    Client now removes temporary directories also if they contain empty
    sub-directories.

    Revision 1.30  1998/05/27 16:55:18  roland
    Abort and commit filter the entries send to the server, so that only
    one entry for a page is transferred.

    Revision 1.29  1998/02/10 16:36:27  roland
    LocalResourceSystem offers procedures to manage the local part of
    resources.

    Revision 1.28  1998/01/21 14:11:06  roland
    Method baseName now in public interface.
    Files can now be opened as read-only in read-write-exclusive resources.

    Revision 1.27  1997/06/13 16:09:51  roland
    Clients extend their temporary resource names wit _c<pid> to
    distinguish temporary resources of different clients and the server.

    Revision 1.26  1997/06/13 11:55:14  rbnix
    	Adapted to unified path handling of
    	Config.GetRootPrefix. Method getTmpPath is removed using an
    	additional parameter for method getPath. Further file handling
    	simplified with new method makeFileName.

    Revision 1.25  1997/06/10 12:53:42  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.24  1997/04/24 12:12:29  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.23  1997/03/26 16:08:33  renehuel
    Added handling of new exception NameServer.InvalidServerIdentification.

    Revision 1.22  1997/03/25 17:05:47  roland
    To avoid deadlocks with propagate callbacks, Open-Method has to release
    PageCache when communicating with server.

    Revision 1.21  1997/03/24 08:39:31  rbnix
    	Adjusted to renaming Config.GetGrasServer ->
    	Config.GetGrasServerId.

    Revision 1.20  1997/03/20 16:54:49  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.19  1996/11/21 15:22:26  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.18  1996/11/21 07:54:25  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.17  1996/11/18 17:51:36  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.16  1996/11/14 14:12:44  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.15  1996/10/30 12:44:07  roland
    Error handling for NetObj.Error in GetData.

    Revision 1.14  1996/10/29 15:00:28  rbnix
        New method getTransactionNumber added.

        New parameter for page age added.

    Revision 1.13  1996/10/08 13:28:32  roland
    Improved exception handling.

    Revision 1.12  1996/10/02 15:31:28  rbnix
        Bug fixed in function Init: last path component is now
        included, it was lost.

    Revision 1.11  1996/09/26 14:24:43  rbnix
        Bug fixed in SignalAccess: Thread.Signal changed into
        Thread.Broadcast this works for multiple waiting threads.

    Revision 1.10  1996/09/23 08:37:30  roland
    GetHostname now uses M3toC.CopyStoT which is safer than M3toC.StoT.

    Revision 1.9  1996/08/22 14:07:49  rbnix
        In method init: support for complex resource names added.

    Revision 1.8  1996/08/06 16:24:49  roland
    Merge of PAGESERVER and main branch.

    Revision 1.7.2.6  1996/08/01 18:09:18  rbnix
        New file administration methods for remote files added:
        deleteFile, copyFile, renameFile, existsFile, fileInUse,
        fileSize and getFiles.

    Revision 1.7.2.5  1996/07/30 07:51:41  rbnix
        Module FS replaced by PageFileSystem.

    Revision 1.7.2.4  1996/07/24 12:51:13  rbnix
        In method init: new parameter clientInfo for entryPort is set
        to hostname and process id.

    Revision 1.7.2.3  1996/07/11 10:45:33  rbnix
        Registration of deadlocks (waitAccess) is now send to the
        server to perform global deadlock detection.

    Revision 1.7.2.2  1996/06/28 09:15:46  rbnix
        Bug fixed: concurrency control for all functions disabled
        while communication active.

    Revision 1.7.2.1  1996/06/27 11:03:59  rbnix
        Debug output for Variant.TestClientCommunication added.

    Revision 1.7  1996/03/15 14:25:28  rbnix
        Method getID added. The id is created by the server.

        Bug fixed: failure creating local data and temp directories is
        ignored now, they might exist before.

    Revision 1.6  1996/03/11 17:16:49  rbnix
        In closeRemoteFile defered data to put is now flushed to
        server. This is to give back cached locks on the file.

        In WaitAccess output to journal for variant
        TestClientScheduler added.

    Revision 1.5  1996/03/06 11:22:27  rbnix
        When reveiving an exception Access.Invalid the description
        will now added to journal before the program failes.

    Revision 1.4  1996/02/29 09:34:55  rbnix
        Release of resources added.

    Revision 1.3  1996/02/28 10:59:15  rbnix
        File and resource pathes are now related to a root path
        via Config.

    Revision 1.2  1996/02/26 17:55:31  rbnix
        Exception Access.Invalid added.

    Revision 1.1  1996/02/09 16:46:34  rbnix
        First version of client scheduler added.

*)
(***************************************************************************)
(*
 | --- BaseScheduledClientRessource ---------------------------------------
 Transfer of data back to the server is defered until transfer from server
 is neccessary or a transaction ends. The defered data is buffered so the
 function GetData returns immediate. Defering the data transfer reduces the
 amount of communication calls by collecting defered calls and grouping
 them into a single call.

 Currently deadlock handling is immediatly directed to the server. This may
 be enhanced by local waiting a while before sending the request to the
 server. See functions waitAccess, signalAccess.
 | ------------------------------------------------------------------------
 *)
IMPORT Unix, Cstdlib, M3toC, Process, Ctypes, (* causes unsafe status *)
       Uutmp, Upwd, Uugid;
IMPORT Pathname, Thread, NetObj, TextSeq, Fmt;
IMPORT Config, Variant, Journal, ErrorSupport, Page, PageCache, PageFile,
       PageFileSystem, Access, Transaction, PageLock, CommunicationEntry,
       CommunicationSeq, EntryPort, CommunicationPort,
       RemoteFile, CommunicationSeqSupport, CallbackClient, ClientInfoSeq,
       ClientInfo, NameServer;
IMPORT LocalResourceSystem;

REVEAL
  T = Internal BRANDED OBJECT
        dataChanged      : Thread.Condition;
        id               : TEXT;
        baseName         : Pathname.T;
        persistentPath   : Pathname.Arcs;
        temporaryPath    : Pathname.Arcs;
        transactionLevel : Transaction.Level;
        transactionNumber: CARDINAL;
        communicationPort: CommunicationPort.T;
        access           : Access.Mode;
        deferedData      : CommunicationSeq.T;

      OVERRIDES
        init  := Init;
        close := Close;

        registerLocalFile   := RegisterLocalFile;
        unregisterLocalFile := UnregisterLocalFile;
        deleteFile          := DeleteFile;
        copyFile            := CopyFile;
        renameFile          := RenameFile;
        existsFile          := ExistsFile;
        fileInUse           := FileInUse;
        getFileUser         := GetFileUser;
        fileSize            := FileSize;
        getFiles            := GetFiles;

        startTransaction     := StartTransaction;
        abortTransaction     := AbortTransaction;
        commitTransaction    := CommitTransaction;
        getTransactionLevel  := GetTransactionLevel;
        getTransactionNumber := GetTransactionNumber;

        openRemoteFile  := OpenRemoteFile;
        closeRemoteFile := CloseRemoteFile;
        getData         := GetData;
        putData         := PutData;
        waitAccess      := WaitAccess;
        signalAccess    := SignalAccess;

        getAccessMode := GetAccessMode;
        getBaseName   := GetBaseName;
        getPath       := GetPath;
        makeFileName  := MakeFileName;
        getID         := GetID;
      END;


(*
 | --- private stuff ------------------------------------------------------
 *)
PROCEDURE GetFileName (<* UNUSED *> file: RemoteFile.T): TEXT =
  BEGIN
    RETURN "???";
  END GetFileName;


PROCEDURE TerminateClient (why: TEXT) RAISES {FatalError} =
  BEGIN
    IF Variant.RegularClientJournal THEN
      Journal.Add("Client execution terminated. " & why);
    END;

    RAISE FatalError(ErrorSupport.Create(
                       "BaseScheduledClientRessource.TerminateClient",
                       "Termination requested: " & why))
  END TerminateClient;


(*
 | --- public stuff -------------------------------------------------------
 *)
PROCEDURE Init (self        : T;
                baseName    : Pathname.T;
                access      : Access.Mode;
                new         : BOOLEAN ): T
  RAISES {PageFile.NoAccess, Access.Denied, FatalError} =
  VAR
    entryPort         : EntryPort.T;
    callbackPort      : CallbackClient.T;
    NS                : NameServer.T;
    basePath	      : Pathname.Arcs;

  PROCEDURE GetInfo (): ClientInfo.T =
    CONST hostNameLength = 100;
    VAR
      info           : ClientInfo.T;
      hostName, cname: Ctypes.char_star;
      pwentry        : Upwd.struct_passwd_star;
    BEGIN
      hostName := Cstdlib.malloc(hostNameLength);
      IF Unix.gethostname(hostName, hostNameLength) = 0 THEN
        info.hostname := M3toC.CopyStoT(hostName);
      ELSE
        info.hostname := "<unknown host>";
      END;
      Cstdlib.free(hostName);

      info.pid := Process.GetMyID();
      info.userid := Uugid.getuid();

      cname := Uutmp.getlogin();
      IF cname # NIL THEN
        info.username := M3toC.CopyStoT(cname);
      ELSE
        pwentry := Upwd.getpwuid(Uugid.getuid());
        IF pwentry = NIL THEN
          info.username := "<unknown user>";
        ELSE
          info.username := M3toC.CopyStoT(pwentry^.pw_name);
        END;
      END;

      RETURN info;
    END GetInfo;

  (* Init *)
  BEGIN
    (* setup file paths *)
    TRY
      self.baseName := baseName;

      (* check baseName to be relative *)
      IF Pathname.Absolute (baseName) THEN
        RAISE PageFile.NoAccess (
                  "Resource with absolute pathname not allowed: " & baseName);
      END;

      (* get relative resource name *)
      basePath := Pathname.Decompose (baseName);
      EVAL basePath.remlo ();

      (* set up local persistent path *)
      self.persistentPath := TextSeq.Cat
        (Pathname.Decompose (Config.GetRootPrefix (temporary := FALSE)), basePath);

      (* get relative temporary resource name *)
      basePath := Pathname.Decompose (baseName & "_c" & Fmt.Int(Process.GetMyID()));
      EVAL basePath.remlo ();

      (* set up local temporary path *)
      self.temporaryPath := TextSeq.Cat
        (Pathname.Decompose (Config.GetRootPrefix (temporary := TRUE)), basePath);
    EXCEPT
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess(
                "Illegal pathname for resource '" & baseName & "'");

    | PageFile.NoAccess =>
      (* ignore this error, maybe the path still exists *)
    | Config.Unspecified =>
        RAISE PageFile.NoAccess("Not logged in! Unable to find root.");
    END;

    (* open communication ports *)
    TRY
      NS := NetObj.Import(NameServer.GrasNameServerID,
                          NetObj.Locate(Config.GetNameServer()));
      IF NS # NIL THEN
        entryPort := NS.getserver(Config.GetGrasServerId())
      ELSE
        RAISE NameServer.NoNameServer;
      END;

      (*entryPort := NameService.ImportEntry(Config.GetNameServer());*)
    EXCEPT
    | NameServer.InvalidServerIdentification =>
        RAISE FatalError(
                ErrorSupport.Create("BaseScheduledClientRessource",
                    "Invalid identification of page server"));
    | NameServer.NoNameServer =>
        RAISE FatalError(
                ErrorSupport.Create("BaseScheduledClientRessource.Init",
                                       "No gras-nameserver found!"));
    | NameServer.ServerNotInList =>
        RAISE FatalError(
                ErrorSupport.Create("BaseScheduledClientRessource.Init",
                                       "Specified server does not exist!"));
    | Thread.Alerted =>
        RAISE PageFile.NoAccess("Connect to server interrupted!");

    | NetObj.Error (code) =>
        RAISE PageFile.NoAccess(
                "Unable to connect to server: " & ErrorSupport.Fmt(code));

    | NetObj.Invalid =>
        RAISE PageFile.NoAccess("Identification of name server invalid"
                                  & " (" & Config.GetNameServer() & ")");
    END;

    PageCache.EndAccess();
    TRY
      TRY
	callbackPort := NEW(CallbackClient.T).init(self);
	IF Variant.TestClientCommunication THEN
	  Journal.Add(
	    "entryPort.open (basename = " & baseName & ", access = "
	      & Access.FmtMode(access) & ", new = " & Fmt.Bool(new) & ")");
	END;
	self.communicationPort :=
	  entryPort.open(
	    baseName, access, new, callbackPort, GetInfo(), self.id);
	IF Variant.TestClientCommunication THEN
	  Journal.Add("entryPort.open -> (id = " & self.id & ")");
	END;
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
	  RAISE FatalError(
		  ErrorSupport.Propagate("BaseScheduledClientRessource.Init",
					 "NetObj.Error", info));
      | Thread.Alerted =>
	  RAISE FatalError(
		  ErrorSupport.Create(
		    "BaseScheduledClientRessource.Init", "Thread.Alerted"));
      END;
    FINALLY
      PageCache.BeginAccess();
    END;

    (* create local directories *)
    IF new AND LocalResourceSystem.ExistsLocalResource(baseName) THEN
      LocalResourceSystem.DeleteLocalResource(baseName);
    END;
    (* Pathnames were checked before *)
    <* FATAL Pathname.Invalid *>
    BEGIN
      PageFileSystem.MakePath (Pathname.Compose (self.persistentPath));
      PageFileSystem.MakePath (Pathname.Compose (self.temporaryPath));
    END;
    (* provide an initial collection for defered data *)
    self.deferedData := NEW(CommunicationSeq.T).init();

    (* other setup *)
    self.dataChanged := NEW(Thread.Condition);
    self.transactionLevel := Transaction.EnvelopeLevel;
    self.transactionNumber := 0;
    self.access := access;

    RETURN self;
  END Init;


PROCEDURE Close (self: T) RAISES {FatalError} =
  <* FATAL Config.Unspecified *>
  VAR
    tmpPath, tmpPrefix: Pathname.Arcs;
    name: Pathname.T;
    deleted: BOOLEAN;

  PROCEDURE EmptySubTree(name: Pathname.T): BOOLEAN RAISES {PageFile.NoAccess} =
    (* checks the complete subtree under name for files *)
    VAR subs: TextSeq.T;
    BEGIN
      IF PageFileSystem.GetFileNames(name).size () > 0 THEN
        RETURN FALSE;
      ELSE
        subs := PageFileSystem.GetDirNames(name);
        FOR i := 0 TO subs.size() - 1 DO
          IF NOT EmptySubTree(Pathname.Join(name, subs.get(i),NIL)) THEN
            RETURN FALSE;
          END;
        END;
      END;
      RETURN TRUE;
    END EmptySubTree;
    
  PROCEDURE RecursiveRemoveDir(name: Pathname.T) RAISES {PageFile.NoAccess} =
    (* checks the complete subtree under name for files *)
    VAR subs: TextSeq.T;
    BEGIN
      subs := PageFileSystem.GetDirNames(name);
      FOR i := 0 TO subs.size() - 1 DO
        RecursiveRemoveDir(Pathname.Join(name, subs.get(i),NIL));
      END;
      PageFileSystem.RemoveDir(name);
    END RecursiveRemoveDir; 
    
  BEGIN
    PageCache.EndAccess();
    TRY
      (* closing communication *)
      TRY
        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.close (id = " & self.id & ")");
        END;
        self.communicationPort.close();
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.Close",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.Close",
                             "Thread.Alerted"));
      END;

      (* some clean up *)
      self.communicationPort := NIL;

      (* delete tmpPath *)
      TRY
	deleted := TRUE;
	tmpPrefix := Pathname.Decompose(Config.GetRootPrefix(temporary := TRUE));
	tmpPath := self.getPath(temporary := TRUE);
	WHILE tmpPath.size() > tmpPrefix.size() AND deleted DO
	  name := Pathname.Compose(tmpPath);
	  IF PageFileSystem.ExistsDir(name) THEN

	    IF EmptySubTree(name) THEN
	      RecursiveRemoveDir(name);
	    ELSE
	      (* leaving loop, can't delete more dirs *)
	      deleted := FALSE;
	    END;
	  ELSE
	    (* leaving loop, can't delete more dirs *)
	    deleted := FALSE;
	  END;


	  EVAL tmpPath.remhi();
	END;
      EXCEPT
        Pathname.Invalid => (* ignore *)
      | PageFile.NoAccess => (* ignore *)
      END;

    FINALLY
      PageCache.BeginAccess();
    END;
  END Close;


PROCEDURE RegisterLocalFile (self: T; baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    PageCache.EndAccess();
    TRY
      TRY
        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.registerLocalFile ("
                        & "baseName = " & baseName & ")");
        END;
        self.communicationPort.registerLocalFile(baseName);
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "BaseScheduledClientRessourceRegisterLocalFile",
                         "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE
            FatalError(ErrorSupport.Create(
                         "BaseScheduledClientRessource.RegisterLocalFile",
                         "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;
  END RegisterLocalFile;


PROCEDURE UnregisterLocalFile (self: T; baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    PageCache.EndAccess();
    TRY
      TRY
        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.unregisterLocalFile ("
                        & "baseName = " & baseName & ")");
        END;
        self.communicationPort.unregisterLocalFile(baseName);
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "BaseScheduledClientRessourceUnregisterLocalFile",
                         "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(
                  ErrorSupport.Create(
                    "BaseScheduledClientRessource.UnregisterLocalFile",
                    "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;
  END UnregisterLocalFile;


PROCEDURE DeleteFile (self: T; baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.deleteRemoteFile (" & "baseName = "
                      & baseName & ")");
      END;
      TRY
        self.communicationPort.deleteRemoteFile(baseName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.DeleteFile",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.DeleteFile",
                             "Thread.Alerted"));
      END;
    FINALLY
      PageCache.BeginAccess();
    END;
  END DeleteFile;


PROCEDURE CopyFile (self: T; sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.copyRemoteFile (" & "sourceName = "
                      & sourceName & "destName = " & destName & ")");
      END;
      TRY
        self.communicationPort.copyRemoteFile(sourceName, destName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessourceCopyFile",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.CopyFile",
                             "Thread.Alerted"));
      END;
    FINALLY
      PageCache.BeginAccess();
    END;
  END CopyFile;


PROCEDURE RenameFile (self: T; oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.renameRemoteFile (" & "oldName = "
                      & oldName & "newName = " & newName & ")");
      END;
      TRY
        self.communicationPort.renameRemoteFile(oldName, newName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessourceRenameFile",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.RenameFile",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;
  END RenameFile;


PROCEDURE ExistsFile (self: T; baseName: Pathname.T): BOOLEAN
  RAISES {FatalError} =
  VAR result: BOOLEAN;
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.existsRemoteFile (" & "baseName = "
                      & baseName & ")");
      END;
      TRY
        result := self.communicationPort.existsRemoteFile(baseName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessourceExistsFile",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.ExistsFile",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;

    RETURN result;
  END ExistsFile;


PROCEDURE FileInUse (self: T; baseName: Pathname.T): BOOLEAN
  RAISES {FatalError} =
  VAR result: BOOLEAN;
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.remoteFileInUse (" & "baseName = "
                      & baseName & ")");
      END;
      TRY
        result := self.communicationPort.remoteFileInUse(baseName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.FileInUse",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.FileInUse",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;

    RETURN result;
  END FileInUse;


PROCEDURE GetFileUser (self: T; baseName: Pathname.T): ClientInfoSeq.T
  RAISES {FatalError} =
  VAR result: ClientInfoSeq.T;
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.getRemoteFileUser (" & "baseName = "
                      & baseName & ")");
      END;
      TRY
        result := self.communicationPort.getRemoteFileUser(baseName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.GetFileUser",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.GetFileUser",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;

    RETURN result;
  END GetFileUser;


PROCEDURE FileSize (self: T; baseName: Pathname.T): CARDINAL
  RAISES {PageFile.NoAccess, FatalError} =
  VAR result: CARDINAL;
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.remoteFileSize (" & "baseName = "
                      & baseName & ")");
      END;
      TRY
        result := self.communicationPort.remoteFileSize(baseName);
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessourceGetFileUser",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.GetFileUser",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;

    RETURN result;
  END FileSize;


PROCEDURE GetFiles (self: T): TextSeq.T RAISES {FatalError} =
  VAR result: TextSeq.T;
  BEGIN
    TRY
      PageCache.EndAccess();
      IF Variant.TestClientCommunication THEN
        Journal.Add("communicationPort.getRemoteFiles ()");
      END;
      TRY
        result := self.communicationPort.getRemoteFiles();
      EXCEPT
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessourceGetFiles",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.GetFiles",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;

    RETURN result;
  END GetFiles;


PROCEDURE StartTransaction (self: T) RAISES {FatalError} =
  BEGIN
    IF self.transactionLevel = Transaction.EnvelopeLevel THEN
      PageCache.EndAccess();
      TRY
        TRY
          IF Variant.TestClientCommunication THEN
            Journal.Add("communicationPort.startTransaction ()");
          END;
          self.transactionNumber :=
            self.communicationPort.startTransaction();
          IF Variant.TestClientCommunication THEN
            Journal.Add("communicationPort.startTransaction () = "
                          & Fmt.Int(self.transactionNumber));
          END;
        EXCEPT
        | Access.Invalid (description) => TerminateClient(description);
        | NetObj.Error (info) =>
            RAISE
              FatalError(ErrorSupport.Propagate(
                           "BaseScheduledClientRessourceStartTransaction",
                           "NetObj.Error", info));
        | Thread.Alerted =>
            RAISE
              FatalError(ErrorSupport.Create(
                           "BaseScheduledClientRessource.StartTransaction",
                           "Thread.Alerted"));
        END;

      FINALLY
        PageCache.BeginAccess();
      END;
    END;

    INC(self.transactionLevel);
  END StartTransaction;


PROCEDURE CommitTransaction (self: T)
  RAISES {FatalError, NotInTransaction} =
  BEGIN
    IF NOT (Transaction.EnvelopeLevel < self.transactionLevel) THEN
      RAISE NotInTransaction;
    END;
    IF self.transactionLevel = Transaction.TopLevel THEN
      PageCache.EndAccess();
      TRY
        TRY
          (* HACK: Filter the entry sequence, so that for every page
             at most one entry exists. This must be the highest in the
             sequence. *)
          TransactionEndFilter(self.deferedData);
          IF Variant.TestClientCommunication THEN
            Journal.Add(
              "communicationPort.PutData (" & "client = " & self.id
                & ", end = Transaction.End.Commit" & ", entries = "
                & CommunicationSeqSupport.Fmt(
                    self.deferedData, GetFileName) & ")");
          END;
          self.communicationPort.putData(
            Transaction.End.Commit, self.deferedData);
        EXCEPT
        | Access.Invalid (description) => TerminateClient(description);
        | NetObj.Error (info) =>
            RAISE FatalError(
                    ErrorSupport.Propagate(
                      "BaseScheduledClientRessource.CommitTransaction",
                      "NetObj.Error", info));
        | Thread.Alerted =>
            RAISE FatalError(
                    ErrorSupport.Create(
                      "BaseScheduledClientRessource.CommitTransaction",
                      "Thread.Alerted"));
        END;

        self.deferedData := NEW(CommunicationSeq.T).init();

      FINALLY
        PageCache.BeginAccess();
      END;
    END;

    DEC(self.transactionLevel);
  END CommitTransaction;


PROCEDURE AbortTransaction (self: T)
  RAISES {NotInTransaction, FatalError} =
  BEGIN
    IF NOT (Transaction.EnvelopeLevel < self.transactionLevel) THEN
      RAISE NotInTransaction;
    END;

    IF self.transactionLevel = Transaction.TopLevel THEN
      PageCache.EndAccess();
      TRY
        TRY
          (* HACK: Filter the entry sequence, so that for every page
             at most one entry exists. This must be the highest in the
             sequence. *)
          TransactionEndFilter(self.deferedData);
          IF Variant.TestClientCommunication THEN
            Journal.Add(
              "communicationPort.PutData (" & "client = " & self.id
                & ", end = Transaction.End.Abort" & ", entries = "
                & CommunicationSeqSupport.Fmt(
                    self.deferedData, GetFileName) & ")");
          END;
          self.communicationPort.putData(
            Transaction.End.Abort, self.deferedData);
        EXCEPT
        | Access.Invalid (description) => TerminateClient(description);
        | NetObj.Error (info) =>
            RAISE
              FatalError(ErrorSupport.Propagate(
                           "BaseScheduledClientRessource.AbortTransaction",
                           "NetObj.Error", info));
        | Thread.Alerted =>
            RAISE
              FatalError(ErrorSupport.Create(
                           "BaseScheduledClientRessource.AbortTransaction",
                           "Thread.Alerted"));
        END;

        self.deferedData := NEW(CommunicationSeq.T).init();

      FINALLY
        PageCache.BeginAccess();
      END;
    END;

    DEC(self.transactionLevel);
  END AbortTransaction;


PROCEDURE GetTransactionLevel (self: T): Transaction.Level =
  BEGIN
    RETURN self.transactionLevel;
  END GetTransactionLevel;


PROCEDURE GetTransactionNumber (self: T): CARDINAL =
  BEGIN
    RETURN self.transactionNumber;
  END GetTransactionNumber;


PROCEDURE OpenRemoteFile (self    : T;
                          baseName: Pathname.T;
                          mode    : Access.Mode;
                          kind    : Access.Kind;
                          new     : BOOLEAN      ): RemoteFile.T
  RAISES {Access.Denied, PageFile.NoAccess, FatalError} =
  VAR file: RemoteFile.T;
  BEGIN
    PageCache.EndAccess();
    TRY
      TRY
        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.openRemoteFile (" & "baseName = "
                        & baseName & ", kind = " & Access.FmtKind(kind)
                        & ", NEW = " & Fmt.Bool(new) & ")");
        END;
        file := self.communicationPort.openRemoteFile(baseName, mode, kind, new);
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.OpenRemoteFile",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.OpenRemoteFile",
                             "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;

    RETURN file;
  END OpenRemoteFile;


PROCEDURE CloseRemoteFile (self: T; file: RemoteFile.T)
  RAISES {FatalError} =
  BEGIN
    PageCache.EndAccess();
    TRY
      TRY
        IF 0 < self.deferedData.size() THEN
          IF Variant.TestClientCommunication THEN
            Journal.Add(
              "communicationPort.PutData (" & "client = " & self.id
                & ", end = Transaction.End.No" & ", entries = "
                & CommunicationSeqSupport.Fmt(
                    self.deferedData, GetFileName) & ")");
          END;
          self.communicationPort.putData(
            Transaction.End.No, self.deferedData);
          self.deferedData := NEW(CommunicationSeq.T).init();
        END;

        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.closeRemoteFile (???");
        END;
        self.communicationPort.closeRemoteFile(file);
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE
            FatalError(ErrorSupport.Propagate(
                         "BaseScheduledClientRessource.CloseRemoteFile",
                         "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE
            FatalError(ErrorSupport.Create(
                         "BaseScheduledClientRessource.CloseRemoteFile",
                         "Thread.Alerted"));
      END;

    FINALLY
      PageCache.BeginAccess();
    END;
  END CloseRemoteFile;


PROCEDURE GetData (    self        : T;
                       file        : RemoteFile.T;
                       pageNo      : CARDINAL;
                   VAR pageAge     : CARDINAL;
                       lock        : PageLock.ServerMode;
                       transferData: BOOLEAN              ): Page.T
  RAISES {Access.Locked, FatalError} =
  VAR result: Page.T;
  BEGIN
    IF NOT (Transaction.EnvelopeLevel < self.transactionLevel) THEN
      RAISE FatalError(
              ErrorSupport.Create("BaseScheduledClientRessource.GetData",
                                  "Not in transaction"));
    END;

    TRY
      PageCache.EndAccess();
      TRY
        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.getData (" & "client = " & self.id
                        & ", file = ???" & ", pageNo = " & Fmt.Int(pageNo)
                        & ", pageAge = " & Fmt.Int(pageAge) & ", lock = "
                        & PageLock.FmtMode(lock) & ", transferData = "
                        & Fmt.Bool(transferData) & ", putEntries = "
                        & CommunicationSeqSupport.Fmt(
                            self.deferedData, GetFileName) & ")");
        END;
        result :=
          self.communicationPort.getData(
            file, pageNo, pageAge, lock, transferData, self.deferedData);
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.GetData",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.GetData",
                             "Thread.Alerted"));
      END;
    FINALLY
      PageCache.BeginAccess();

      (* reset defered data also if exceptions are raised *)
      self.deferedData := NEW(CommunicationSeq.T).init();
    END;

    RETURN result;
  END GetData;


PROCEDURE PutData (self   : T;
                   file   : RemoteFile.T;
                   pageNo : CARDINAL;
                   pageAge: CARDINAL;
                   lock   : PageLock.ServerMode;
                   page   : Page.T               ) =
  BEGIN
    self.deferedData.addhi(
      CommunicationEntry.T{file, pageNo, pageAge, lock, page});
  END PutData;


PROCEDURE WaitAccess (self  : T;
                      file  : RemoteFile.T;
                      pageNo: CARDINAL;
                      lock  : PageLock.ServerMode)
  RAISES {Access.Locked, FatalError} =
  BEGIN
    PageCache.EndAccess();
    TRY
      TRY
        IF Variant.TestClientCommunication THEN
          Journal.Add("communicationPort.WaitAccess (" & "client = "
                        & self.id & ", file = " & GetFileName(file)
                        & ", pageNo = " & Fmt.Int(pageNo) & ", lock = "
                        & PageLock.FmtMode(lock) & ")");
        END;
        self.communicationPort.waitAccess(file, pageNo, lock);
      EXCEPT
      | Access.Invalid (description) => TerminateClient(description);
      | NetObj.Error (info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientRessource.PutData",
                             "NetObj.Error", info));
      | Thread.Alerted =>
          RAISE FatalError(ErrorSupport.Create(
                             "BaseScheduledClientRessource.PutData",
                             "Thread.Alerted"));
      END;
    FINALLY
      PageCache.BeginAccess();
    END;

    (*
      *** disabled, currently *no local* waiting ***

    IF Variant.TestClientScheduler OR TRUE THEN
      Journal.Add ("BaseScheduledClientRessource WaitAccess START (" &
                   "pageNo = " & Fmt.Int (pageNo) &
                   ", lock = " & PageLock.FmtMode (lock) & ")");
    END;

    PageCache.WaitAccess (self.dataChanged);

    IF Variant.TestClientScheduler OR TRUE THEN
      Journal.Add ("BaseScheduledClientRessource WaitAccess END (" &
                   "pageNo = " & Fmt.Int (pageNo) &
                   ", lock = " & PageLock.FmtMode (lock) & ")");
    END;
    *)
  END WaitAccess;


PROCEDURE SignalAccess (<* UNUSED *> self: T) =
  BEGIN
    (*
      *** disabled, currently *no local* waiting ***

    Thread.Broadcast (self.dataChanged);
    *)
  END SignalAccess;


PROCEDURE GetAccessMode (self: T): Access.Mode =
  BEGIN
    RETURN self.access;
  END GetAccessMode;


PROCEDURE GetBaseName (self: T): Pathname.T =
  BEGIN
    RETURN self.baseName;
  END GetBaseName;


PROCEDURE GetPath (self: T; temporary:BOOLEAN): Pathname.Arcs =
  BEGIN
    IF temporary THEN
      RETURN TextSeq.Sub (self.temporaryPath, 0)
    ELSE
      RETURN TextSeq.Sub (self.persistentPath, 0);
    END
  END GetPath;

PROCEDURE MakeFileName (self :T; baseName :Pathname.T; temporary :BOOLEAN)
                       :Pathname.T
                       RAISES {PageFile.NoAccess} =
  VAR
    basePath		:Pathname.Arcs;
    fileName		:Pathname.T;
  BEGIN
    TRY
      (* check for relative file path *)
      IF Pathname.Absolute (baseName) THEN
        RAISE PageFile.NoAccess (
                  "File with absolute pathname is not allowed: " & baseName);
      END;

      (* remove leading NIL component indicating a relative path *)
      basePath := Pathname.Decompose (baseName);
      EVAL basePath.remlo ();

      (* compose full path *)
      fileName := Pathname.Compose (
                      TextSeq.Cat (self.getPath (temporary), basePath));
    EXCEPT
    | Pathname.Invalid =>
      RAISE PageFile.NoAccess (
                "Invalid file name '" & baseName & "'");
    END;

    RETURN fileName;
  END MakeFileName;


PROCEDURE GetID (self: T): TEXT =
  BEGIN
    RETURN self.id;
  END GetID;

(* LOCAL ADT INTERFACE CETable *)

(* TYPE CETable; *)

PROCEDURE CETableInit (VAR tab: CETable) =
  BEGIN
    tab := CETable{NIL, ..};
  END CETableInit;

PROCEDURE CETableExists (READONLY tab: CETable; ce: CommunicationEntry.T): BOOLEAN =
  VAR
    i    : CEIndex;
    entry: REF CETableEntry;
  BEGIN
    i := HashCEKey(ce);
    RETURN SearchCECollList(tab[i], ce, entry);
  END CETableExists;

PROCEDURE CETablePut (VAR tab: CETable;
                          ce : CommunicationEntry.T) =
  VAR
    i    : CEIndex;
    entry: REF CETableEntry;
  BEGIN
    i := HashCEKey(ce);
    IF SearchCECollList(tab[i], ce, entry) THEN
      entry^ :=
        CETableEntry{ce := ce, next := entry^.next};
    ELSE
      entry := NEW(REF CETableEntry, ce := ce, next := NIL);
      InsertInCECollList(tab[i], entry);
    END;
  END CETablePut;

(* END LOCAL CETable; *)

(* MODULE LOCAL ADT CETable *)

CONST CETableSize = 17;

TYPE
  CEKey = CommunicationEntry.T;
  CEIndex = [0 .. CETableSize - 1];

  CETable = ARRAY CEIndex OF REF CETableEntry;

  CETableEntry = RECORD
                   ce: CommunicationEntry.T;
                   next: REF CETableEntry;  (* Collision list *)
                 END;

PROCEDURE HashCEKey (key: CEKey): CEIndex =
  BEGIN
    RETURN (key.pageNo + key.pageAge) MOD CETableSize;
  END HashCEKey;

PROCEDURE EntryEqual(a,b: CommunicationEntry.T): BOOLEAN =
  BEGIN
    (* Entries are equal if they at most differ in their lock *)
    IF a.file # b.file THEN RETURN FALSE END;
    IF a.pageNo # b.pageNo THEN RETURN FALSE END;
    IF a.pageAge # b.pageAge THEN RETURN FALSE END;
    IF a.page # b.page THEN RETURN FALSE END;
    RETURN TRUE;
  END EntryEqual;
  
PROCEDURE SearchCECollList (    list : REF CETableEntry;
                                key  : CEKey;
                            VAR entry: REF CETableEntry  ): BOOLEAN =
  BEGIN
    entry := list;
    WHILE entry # NIL DO
      IF EntryEqual(entry^.ce, key) THEN RETURN TRUE END;
      entry := entry^.next;
    END;
    RETURN FALSE;
  END SearchCECollList;

PROCEDURE InsertInCECollList (VAR list : REF CETableEntry;
                                  entry: REF CETableEntry  ) =
  BEGIN
    entry^.next := list;
    list := entry;
  END InsertInCECollList;

(* END LOCAL ADT CETable; *)

PROCEDURE TransactionEndFilter(VAR seq: CommunicationSeq.T) =
  VAR tab: CETable;
      new:= NEW(CommunicationSeq.T).init();
  BEGIN    
    (* Insert entries in reverse order in the hash table. If an entry
       already is in the table (modulo lock), then remove the second
       from the sequence. *)
    CETableInit(tab);
    FOR i := seq.size() - 1 TO 0 BY - 1 DO
      WITH ce = seq.get(i) DO
        IF NOT CETableExists(tab, ce) THEN
          new.addlo(ce);
          CETablePut(tab, ce);
        END;
      END;
    END;
    seq := new;
  END TransactionEndFilter;
  
BEGIN
END BaseScheduledClientRessource.
