MODULE ScheduledServerResource
EXPORTS ScheduledServerResource, InternalScheduledServerResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:39  hosking
    Import of GRAS3 1.1

    Revision 1.24  1998/08/27 16:12:39  roland
    Bugfixes for IntraCopyGraph.

    Revision 1.23  1998/08/27 12:27:35  roland
    When copyinf files, make sure the path for the destination exists.

    Revision 1.22  1998/05/19 10:21:59  roland
    Typos and "'" removed from error messages.

    Revision 1.21  1998/01/21 14:13:09  roland
    Files can be opened as read-only in read-write-exclusive resources.

    Revision 1.20  1997/06/27 07:06:19  roland
    Files remove their shadow when they are closed. Therefore, shadow
    files have to initialized on every true open of a file with
    initShadow.

    Revision 1.19  1997/06/16 12:27:44  rbnix
    	Obsolete local variables in PutData removed.

    Revision 1.18  1997/06/16 12:21:47  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.17  1997/06/13 11:46:28  rbnix
    	Adapted to unified resource paths of module
    	BaseServerScheduler.

    Revision 1.16  1997/06/10 12:55:01  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.15  1997/04/24 12:13:52  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.14  1996/11/21 07:56:06  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.13  1996/11/20 12:11:39  roland
    Improved exception handling and startup.

    Revision 1.12  1996/11/14 14:13:47  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.11  1996/11/11 09:37:34  rbnix
    	Error messages for Netobj.Error evaluated.

    Revision 1.10  1996/10/29 14:41:00  rbnix
    	New parameter pageAge added.

    	StartTransaction returns the actual transaction number.

    Revision 1.9  1996/10/10 12:53:02  rbnix
    	Message handling for already killed clients improved.

    Revision 1.8  1996/10/08 10:46:44  roland
    Deleted and renamed files are now removed from scheduledFileTable
    instead of setting them to NIL.

    Revision 1.7  1996/10/02 14:58:51  rbnix
    	Bug fixed in function PutData: propagation of data is invoked
    	even if no data has changed, maybe there are lock changes to
    	send.

    Revision 1.6  1996/08/06 16:32:56  roland
    Merge of PAGESERVER and main branch.

    Revision 1.5.2.6  1996/08/01 18:02:20  rbnix
    	New administration methods deleteFile, copyFile, renameFile,
    	existsFile, fileInUse and getFiles added.

    	Method inUse added.

    	Bug fixed in method openFile: now opening a file with
    	new = TRUE deletes an existing file.

    Revision 1.5.2.5  1996/07/23 14:19:27  rbnix
    	Bug fixed: handling of access mode adjusted. The access mode
    	is now determined by the actual first active client rather
    	than the very first client.

    Revision 1.5.2.4  1996/07/11 11:08:36  rbnix
    	Method waitAccess added to provide global deadlock detection.

    Revision 1.5.2.3  1996/06/28 09:30:41  rbnix
    	Optimization in PutData: propagation of data over net and disk
    	is only done if really data changed.

    Revision 1.5.2.2  1996/06/27 11:05:05  rbnix
    	Bug fixed in PropagateData: data is send now to the other clients.

    Revision 1.5.2.1  1996/06/12 11:35:43  rbnix
    	Bug fixed in PropagateData: propagates will now only sended to
    	living clients.

    Revision 1.5  1996/03/15 14:20:13  rbnix
    	Output to journal improved.

    Revision 1.4  1996/03/06 08:15:07  rbnix
    	Descriptional argument for killClient added.

    	Output to journal for variant TestServerCommunication added.

    	Bug fixed: on checking access modes while open resources.

    Revision 1.3  1996/03/02 15:07:33  rbnix
    	Bug fixed: use of ScheduledClientSetDef instead abstract type
    	ScheduledClientSet. Objects of this type are now initialzed
    	well.

    	Bug fixed: check on found/new files/resources adjusted.

    Revision 1.2  1996/02/28 11:03:20  rbnix
    	File and resource pathes are now related to a root path
    	via Config. Errors related to creation of are submittet to
    	clients via exception PageFile.NoAccess.

    Revision 1.1  1996/02/26 17:59:52  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- ScheduledServerResource --------------------------------------------
 Files can be handled if they aren't in use by any user. They are assumed
 to be persistent if they are well closed.
 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledServerResource AS Super;
IMPORT ServedClientTransientSetDef AS ServedClientSet;
IMPORT
  Thread, NetObj, Fmt, TextTransientSeq AS TextSeq,
  Pathname,
  Variant, Journal,
  Page,
  PageFile, PageFileSystem, ErrorSupport, CallbackPort,
  Access, PageLock, Txn, Termination,
  CommunicationSeqSupport, CommunicationSeq, RemoteFile,
  ServedClientTable, ServedClient,
  BaseServerScheduler,
  InternalBaseScheduledServerResource,
  ScheduledServerFile, InternalScheduledServerFile,
  InternalBaseScheduledServerFile, ScheduledServerFileTbl,
  ServedClientTbl, CommunicationEntry, CommunicationTbl,
  ClientInfoSeq;

REVEAL
  T				= Internal BRANDED OBJECT
      scheduledFileTable	:ScheduledServerFileTbl.Default;
      registeredFileTable	:ServedClientTbl.Default;
      clientSet			:ServedClientSet.T;

    METHODS
      checkAccess		(         client	:ServedClient.T)
				RAISES {Access.Invalid}
				:= CheckAccess;

      propagateData		(         client	:ServedClient.T;
	                                  end		:Txn.End)
				:= PropagateData;

    OVERRIDES
      init			:= Init;
      open			:= Open;
      close			:= Close;

      killClient		:= KillClient;
      shutdown			:= Shutdown;
      inUse			:= InUse;
      user                      := User;

      registerLocalFile		:= RegisterLocalFile;
      unregisterLocalFile	:= UnregisterLocalFile;
      deleteFile		:= DeleteFile;
      copyFile			:= CopyFile;
      renameFile		:= RenameFile;
      existsFile		:= ExistsFile;
      fileInUse			:= FileInUse;
      getFileUser               := GetFileUser;
      fileSize			:= FileSize;
      getFiles			:= GetFiles;

      openFile			:= OpenFile;
      closeFile			:= CloseFile;
      putData			:= PutData;
      getData			:= GetData;
      startTransaction		:= StartTransaction;
      waitAccess		:= WaitAccess;
    END;


(*
 | --- private stuff ------------------------------------------------------
 *)
PROCEDURE GetFileName		(         file		:RemoteFile.T) :TEXT =
  BEGIN
    RETURN NARROW (file, ScheduledServerFile.T).getBaseName ()
  END GetFileName;

  
(*
 | --- public stuff -------------------------------------------------------
 *)
PROCEDURE Init			(         self		:T;
			                  baseName	:Pathname.T;
			                  new           :BOOLEAN)
				:Super.T 
				RAISES {PageFile.NoAccess} =
  BEGIN
    self.scheduledFileTable := NEW (ScheduledServerFileTbl.Default).init ();
    self.registeredFileTable := NEW (ServedClientTbl.Default).init ();
    self.clientSet := NEW (ServedClientSet.T).init ();

    RETURN Super.T.init (self, baseName, new);
  END Init;


PROCEDURE CheckAccess		(         self		:T;
                                          client	:ServedClient.T)
				RAISES {Access.Invalid} =
  BEGIN
    IF client.isKilled () THEN
      RAISE Access.Invalid ("Client is already killed: " &
                            client.whyKilled ());
    END;

    IF NOT self.clientSet.member (client) THEN
      self.killClient (client, 
                "Resource '" & self.getBaseName () & "' is not opened!");
      RAISE Access.Invalid (client.whyKilled ());
    END;
  END CheckAccess;

PROCEDURE Open			(         self		:T;
                                          client	:ServedClient.T;
	                                  access	:Access.Mode)
				:T
				RAISES {Access.Denied, Access.Invalid} =
  BEGIN
    IF client.isKilled () THEN
      RAISE Access.Invalid ("Client is already killed: " &
                            client.whyKilled ());
    END;
    IF self.clientSet.member (client) THEN
      self.killClient (client, 
                       "Cannot open resource '" & self.getBaseName () &
                       "'. It is already opened by client!");
      RAISE Access.Invalid (client.whyKilled ());
    END;

    IF self.clientSet.isEmpty () THEN
      self.setAccessMode (access);
      
    ELSE
      CASE self.getAccessMode () OF
      | Access.Mode.ReadOnlyShared,
        Access.Mode.ReadWriteShared =>
        IF access # self.getAccessMode () THEN
          RAISE Access.Denied (
                    "Cannot open resource '" & self.getBaseName () & "'. " &
                    "It is already opened with conflicting access mode (" &
                    Access.FmtMode (self.getAccessMode ()) & ").");
        END;

      | Access.Mode.ReadWriteExclusive =>
        RAISE Access.Denied (
                  "Cannot open resource '" & self.getBaseName () & "'. " &
                  "It is already opened in exclusive mode (" &
                  Access.FmtMode (self.getAccessMode ()) & ").");
      END;
    END;

    EVAL self.clientSet.insert (client);
    RETURN self;
  END Open;


PROCEDURE Close			(         self		:T;
                                          client	:ServedClient.T)
				RAISES {Access.Invalid} =

  PROCEDURE CheckScheduledFiles () RAISES {Access.Invalid} =
    VAR
      file			:ScheduledServerFile.T;
      baseName			:Pathname.T;
      i				:ScheduledServerFileTbl.Iterator;
    BEGIN
      i := self.scheduledFileTable.iterate ();
      WHILE i.next (baseName, file) DO
        IF file.inUse (client) THEN
          self.killClient (client, 
                           "Error on closing." &
                           "There were still opened files in Resource '" &
                           self.getBaseName () & "'");
          RAISE Access.Invalid (client.whyKilled ());
        END;
      END;
    END CheckScheduledFiles; 

  PROCEDURE CheckRegisteredFiles () RAISES {Access.Invalid} =
    VAR
      baseName			:Pathname.T;
      registeredClient		:ServedClient.T;
      i				:ServedClientTbl.Iterator;
    BEGIN
      i := self.registeredFileTable.iterate ();
      WHILE i.next (baseName, registeredClient) DO
        IF registeredClient = client THEN
          self.killClient (client,
                           "Error on closing." &
                           "There were still registered files in Resource '" &
                           self.getBaseName () & "'");
          RAISE Access.Invalid (client.whyKilled ());
        END;
      END;
    END CheckRegisteredFiles;

  (* Close *)
  BEGIN
    self.checkAccess (client);

    CheckScheduledFiles ();
    CheckRegisteredFiles ();

    EVAL self.clientSet.delete (client);
    IF self.clientSet.size() = 0 THEN
      self.cleanUp();
    END;
  END Close;


PROCEDURE Shutdown		(         self		:T;
                                          termination	:Termination.Mode)
				RAISES {Termination.StillInUse} =
  VAR
    file			:ScheduledServerFile.T;
    baseName			:Pathname.T;
    i				:ScheduledServerFileTbl.Iterator;
  BEGIN
    CASE termination OF
    | Termination.Mode.Try =>
      IF NOT (self.clientSet.isEmpty ()) THEN
        RAISE Termination.StillInUse;
      END;

    | Termination.Mode.Strikt =>
      (* empty *)
    END;

    i := self.scheduledFileTable.iterate ();
    WHILE i.next (baseName, file) DO
      file.shutdown ()
    END;
    self.cleanUp();
  END Shutdown;


PROCEDURE KillClient		(         self		:T;
                                          client	:ServedClient.T;
                                          why		:TEXT := NIL) =

  PROCEDURE KillFromScheduledFiles () =
    VAR
      file			:ScheduledServerFile.T;
      baseName                  :Pathname.T;
      i                         :ScheduledServerFileTbl.Iterator;
    BEGIN
      i := self.scheduledFileTable.iterate ();
      WHILE i.next (baseName, file) DO
        IF file.inUse (client) THEN
          file.killClient (client);
        END
      END;
    END KillFromScheduledFiles;

  PROCEDURE KillFromRegisteredFiles () =
    VAR
      baseName			:Pathname.T;
      registeredClient		:ServedClient.T;
      i				:ServedClientTbl.Iterator;
    BEGIN
      i := self.registeredFileTable.iterate ();
      WHILE i.next (baseName, registeredClient) DO
        IF registeredClient = client THEN
          EVAL self.registeredFileTable.delete (baseName, registeredClient);
        END;
      END;
    END KillFromRegisteredFiles;

  (* KillClient *)
  BEGIN
    WITH chain = client.getChainData() DO
      IF chain.size() > 0 THEN
        VAR
          it := chain.iterate();
          entry : CommunicationEntry.T;
          <*FATAL Access.Invalid*>
        BEGIN
          WHILE it.next(entry, entry) DO
            NARROW (entry.file, ScheduledServerFile.T).putData (client, Txn.End.Commit, entry);
          END
        END;
        self.propagateData (client, Txn.End.Commit);
        client.clearChainData();
      END
    END;

    KillFromScheduledFiles ();
    KillFromRegisteredFiles ();

    EVAL self.clientSet.delete (client);
    client.kill (why);
    self.propagateData (client, Txn.End.Abort);
  END KillClient;
          

PROCEDURE InUse			(         self		:T;
                                          client	:ServedClient.T)
				:BOOLEAN =
  BEGIN
    IF client = NIL THEN
      RETURN NOT self.clientSet.isEmpty ();
    ELSE
      RETURN self.clientSet.member (client);
    END;
  END InUse;


PROCEDURE User			(         self		:T)
				:ClientInfoSeq.T =
  VAR
    it: ServedClientSet.Iterator := self.clientSet.iterate();
    cl: ServedClient.T;
    res: ClientInfoSeq.T := NEW(ClientInfoSeq.T).init();
  BEGIN
    WHILE it.next(cl) DO
      res.addhi(cl.getInfo());
    END;
    RETURN res;
  END User; 


PROCEDURE RegisterLocalFile	(         self		:T;
                                          client	:ServedClient.T;
	                                  baseName	:Pathname.T)
				RAISES {PageFile.NoAccess, Access.Invalid} =

  VAR
    scheduledFile		:ScheduledServerFile.T;
    registeredClient		:ServedClient.T;
  BEGIN
    self.checkAccess (client);

    IF self.scheduledFileTable.get (baseName, scheduledFile) THEN
      RAISE PageFile.NoAccess (
                "Cannot register file '" & baseName & "' in resource '" &
                self.getBaseName () & "'. It is already opened.");
    END;
    IF self.registeredFileTable.get (baseName, registeredClient) THEN
      RAISE PageFile.NoAccess (
                "Cannot register file '" & baseName & "' in resource '" &
                self.getBaseName () & "'. It is already registered.");
    END;

    EVAL self.registeredFileTable.put (baseName, client);
  END RegisterLocalFile;


PROCEDURE UnregisterLocalFile	(         self		:T;
                                          client	:ServedClient.T;
	                                  baseName	:Pathname.T)
				RAISES {Access.Invalid} =

  VAR
    registeredClient		:ServedClient.T;
  BEGIN
    self.checkAccess (client);

    IF (NOT self.registeredFileTable.get (baseName, registeredClient)) OR
       (registeredClient # client) THEN
      self.killClient (client, 
                       "Cannot unregister file '" & baseName & "' in resource '" &
                       self.getBaseName () & "'. It is not registered.");
      RAISE Access.Invalid (client.whyKilled ());
    END;

    EVAL self.registeredFileTable.delete (baseName, client);
  END UnregisterLocalFile;


PROCEDURE DeleteFile		(         self		:T;
                                          baseName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  VAR
    scheduledFile		:ScheduledServerFile.T;
  BEGIN
    IF self.fileInUse (baseName) THEN
      RAISE PageFile.NoAccess (
                "Cannot delete file '" & baseName & "'. It is in use.");
    END;

    (* delete memory object *)
    IF self.scheduledFileTable.get (baseName, scheduledFile) THEN
      scheduledFile.shutdown ();
      EVAL self.scheduledFileTable.delete (baseName, scheduledFile);
    END;

    (* delete disk file *)
    PageFileSystem.DeleteFile (
        self.makeFileName (baseName, temporary := FALSE));
  END DeleteFile;


PROCEDURE CopyFile		(         self		:T;
                                          sourceName	:Pathname.T;
	                                  destName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  VAR destPath: Pathname.T;
  BEGIN
    IF self.fileInUse (sourceName) THEN
      RAISE PageFile.NoAccess (
                "Cannot copy file '" & sourceName & "' to '" & destName &
                "'. The source file is in use.");
    END;
    IF self.existsFile (destName) THEN
      RAISE PageFile.NoAccess (
                "Cannot copy file '" & sourceName & "' to '" & destName &
                "'. The destination file already exists.");
    END;

    (* Make sure the directory for the destination file exists *)
    destPath := self.makeFileName (destName, temporary := FALSE);
    PageFileSystem.MakePath(Pathname.Prefix(destPath));
    
    PageFileSystem.CopyFile (
        self.makeFileName (sourceName, temporary := FALSE), destPath);
  END CopyFile;


PROCEDURE RenameFile		(         self		:T;
                                          oldName	:Pathname.T;
	                                  newName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  VAR
    scheduledFile		:ScheduledServerFile.T;
  BEGIN
    IF self.fileInUse (oldName) THEN
      RAISE PageFile.NoAccess (
                "Cannot rename file '" & oldName & "' to '" & newName &
                "'. The file is in use.");
    END;
    IF self.existsFile (newName) THEN
      RAISE PageFile.NoAccess (
                "Cannot rename file '" & oldName & "' to '" & newName &
                "'. The new name is already  used.");
    END;

    (* delete memory object (it can't be renamed) *)
    IF self.scheduledFileTable.get (oldName, scheduledFile) THEN
      scheduledFile.shutdown ();
      EVAL self.scheduledFileTable.delete(oldName, scheduledFile);
    END;

    (* rename disk file *)
    PageFileSystem.RenameFile (
        self.makeFileName (oldName, temporary := FALSE),
        self.makeFileName (newName, temporary := FALSE));
  END RenameFile;


PROCEDURE ExistsFile		(         self		:T;
                                          baseName	:Pathname.T)
				:BOOLEAN =
  BEGIN
    TRY
      RETURN self.fileInUse (baseName) OR
             PageFileSystem.ExistsFile (
                 self.makeFileName (baseName, temporary := FALSE));
    EXCEPT
    | PageFile.NoAccess =>
      RETURN FALSE;
    END;
  END ExistsFile;


PROCEDURE FileInUse		(         self		:T;
                                          baseName	:Pathname.T)
				:BOOLEAN =
  VAR
    scheduledFile		:ScheduledServerFile.T;
    registeredClient		:ServedClient.T;
  BEGIN
    IF self.scheduledFileTable.get (baseName, scheduledFile) THEN
      RETURN scheduledFile.inUse (NIL);
    ELSE
      RETURN self.registeredFileTable.get (baseName, registeredClient);
    END;
  END FileInUse;


PROCEDURE GetFileUser		(         self		:T;
                                          baseName	:Pathname.T)
				:ClientInfoSeq.T =
  VAR
    scheduledFile		:ScheduledServerFile.T;
  BEGIN
    IF self.scheduledFileTable.get (baseName, scheduledFile) THEN
      RETURN scheduledFile.user ();
    ELSE
      RETURN NIL;
    END;
  END GetFileUser; 


PROCEDURE FileSize		(         self		:T;
                                          baseName	:Pathname.T)
				:CARDINAL
				RAISES {PageFile.NoAccess} =
  BEGIN
    IF self.fileInUse (baseName) THEN
      RAISE PageFile.NoAccess (
                "Cannot inspect file '" & baseName & "'. It is in use.");
    END;

    RETURN PageFileSystem.FileSize (
               self.makeFileName (baseName, temporary := FALSE));
  END FileSize;


PROCEDURE GetFiles		(         self		:T)
				:TextSeq.T =
  <* FATAL PageFile.NoAccess *>
  BEGIN
    RETURN PageFileSystem.GetFileNames (BaseServerScheduler.MakeResourceName (
                   self.getBaseName (), temporary := FALSE));
  END GetFiles;


PROCEDURE OpenFile		(         self		:T;
                                          client	:ServedClient.T;
	                                  baseName      :Pathname.T;
                                          mode          :Access.Mode;
	                                  kind		:Access.Kind;
	                                  new		:BOOLEAN)
				:RemoteFile.T
				RAISES {Access.Denied, PageFile.NoAccess,
                                        Access.Invalid} =
  VAR
    scheduledFile		:ScheduledServerFile.T;
  BEGIN
    self.checkAccess (client);
      
    CASE self.getAccessMode () OF
      Access.Mode.ReadWriteExclusive =>
        (* files can be read and written exclusively or only read *)
        IF mode = Access.Mode.ReadWriteShared THEN
          RAISE Access.Denied("Cannot open file '" & baseName & "'. " &
                  "Access mode " & Access.FmtMode (mode) &
                  " conflicts with resource access mode " &
                  Access.FmtMode (self.getAccessMode ()) & ".");
        END;
    | Access.Mode.ReadOnlyShared =>
        (* files can only be read *)
        IF mode # Access.Mode.ReadOnlyShared THEN
          RAISE Access.Denied("Cannot open file '" & baseName & "'. " &
                  "Access mode " & Access.FmtMode (mode) &
                  " conflicts with ressource access mode " &
                  Access.FmtMode (self.getAccessMode ()) & ".");
        END;
    | Access.Mode.ReadWriteShared =>
        (* ok *)
    END;
    
    IF new AND self.existsFile (baseName) THEN
      self.deleteFile (baseName);
    END;

    IF NOT self.scheduledFileTable.get (baseName, scheduledFile) THEN
      scheduledFile := NEW (ScheduledServerFile.T).init (
                                                 self, baseName, mode, kind, new);
      EVAL self.scheduledFileTable.put (baseName, scheduledFile);
    END;
    
    TRY
      scheduledFile.open (client, mode, kind);
    EXCEPT
    | Access.Invalid (description) =>
      self.killClient (client, description);
      RAISE Access.Invalid (description)
    END;

    RETURN scheduledFile;
  END OpenFile;    


PROCEDURE CloseFile		(         self		:T;
                                          client	:ServedClient.T;
                                          file		:RemoteFile.T)
				RAISES {Access.Invalid} =
  BEGIN
    self.checkAccess (client);

    TRY
      NARROW (file, ScheduledServerFile.T).close (client);
    EXCEPT
    | Access.Invalid (description) =>
      self.killClient (client, description);
      RAISE Access.Invalid (description)
    END
  END CloseFile;


PROCEDURE PropagateData		(         self		:T;
                                          client	:ServedClient.T;
                                          end		:Txn.End) =
  VAR
    otherClient			:ServedClient.T;
    i				:ServedClientTable.Iterator;
  BEGIN
    i := NEW (ServedClientTable.Iterator).init ();
    WHILE i.next (otherClient) DO
      IF (NOT otherClient.isKilled ()) AND
         (client # otherClient) AND
         ((end = Txn.End.Commit) OR
          (0 < otherClient.getPropagationData ().size ())) THEN
        TRY
          IF Variant.TestServerCommunication OR Variant.TestServerScheduler THEN
            Journal.Add ("ScheduledServerResource.PropagateData (" &
              "client = " & otherClient.getID () &
              ", end = " & Txn.FmtEnd (end) &
              ", entries = " & CommunicationSeqSupport.Fmt (
                           otherClient.getPropagationData (),GetFileName) & ")");
          END;
          
          otherClient.getCallbackPort ().propagateData (
                          end, otherClient.getPropagationData ());
        EXCEPT
        | Thread.Alerted =>
          self.killClient (otherClient,
                           "Propagation of data failed, thread alerted.");

        | NetObj.Error (code) =>
          self.killClient (otherClient,
                           "Propagation of data failed. " &
                           ErrorSupport.Fmt (code));
        | CallbackPort.FatalError(info) => 
          self.killClient (otherClient,
                    "Propagation of data failed. " &
                    ErrorSupport.ToText (info));
        END;
        otherClient.clearPropagationData ();
      END;
    END;
    BaseServerScheduler.SignalAccess ();
  END PropagateData;


PROCEDURE PutData		(         self		:T;
                                          client	:ServedClient.T;
	                                  end		:Txn.End;
	                                  entries       :CommunicationSeq.T)
				:CARDINAL
				RAISES {Access.Invalid} =
  BEGIN
    self.checkAccess (client);

    TRY
      (* validate returned data *)
      FOR i := 0 TO entries.size ()-1 DO
        WITH entry = entries.get (i) DO
          NARROW (entry.file, ScheduledServerFile.T).checkData (client, end, entry)
        END
      END;

      (* Propagate data / events to other clients.
         Propagate even when data unchanged:
         delayed data may be pending even for aborted transactions *)

      CASE end OF
      | Txn.End.No =>
        (* accept returned data *)
        FOR i := 0 TO entries.size ()-1 DO
          WITH entry = entries.get (i) DO
            NARROW (entry.file, ScheduledServerFile.T).putData (client, end, entry)
          END
        END;
        self.propagateData (client, end);

      | Txn.End.Chain =>
        (* accept chained data *)
        WITH chain = client.getChainData() DO
          FOR i := 0 TO entries.size ()-1 DO
            WITH entry = entries.get (i) DO
              EVAL chain.put(entry, entry);
            END
          END
        END;

        client.setTransaction (on := TRUE);

      | Txn.End.Abort =>
        (* propagate chained data *)
        WITH chain = client.getChainData() DO
          IF chain.size() > 0 THEN
            VAR
              it := chain.iterate();
              entry: CommunicationEntry.T;
            BEGIN
              WHILE it.next(entry, entry) DO
                NARROW (entry.file, ScheduledServerFile.T).putData (client, Txn.End.Commit, entry);
              END
            END;
            self.propagateData (client, Txn.End.Commit);
            client.clearChainData();
          END
        END;

        (* accept returned data *)
        FOR i := 0 TO entries.size ()-1 DO
          WITH entry = entries.get (i) DO
            NARROW (entry.file, ScheduledServerFile.T).putData (client, end, entry)
          END
        END;
        self.propagateData (client, end);

        (* handling transaction termination *)
        IF 0 # client.getXLockCount () THEN
          RAISE Access.Invalid (
                    "Illegal end of transaction." &
                    "Client has to return all X-Locks on end of transaction " &
                    "but there still rests " & Fmt.Int (client.getXLockCount ()));
        END;
        client.setTransaction (on := FALSE);

      | Txn.End.Commit =>
        VAR
          OEntries:= NEW(CommunicationTbl.Default).init();
          entry: CommunicationEntry.T;
          chain := client.getChainData();
        BEGIN
          (* accept returned data *)
          FOR i := 0 TO entries.size ()-1 DO
            entry := entries.get(i);
            IF entry.lock = PageLock.Mode.O THEN
              EVAL OEntries.put(entry, entry);
            ELSE
              EVAL chain.put(entry, entry);
            END
          END;

          (* propagate chained data *)
          IF chain.size() > 0 THEN
            VAR it := chain.iterate();
            BEGIN
              WHILE it.next(entry, entry) DO
                NARROW (entry.file, ScheduledServerFile.T).putData (client, end, entry);
              END
            END;
            client.clearChainData();
          END;

          VAR it := OEntries.iterate();            
          BEGIN
            WHILE it.next(entry, entry) DO
              NARROW (entry.file, ScheduledServerFile.T).putData (client, end, entry);
            END
          END
        END;

        self.propagateData (client, end);

        (* handling transaction termination *)
        IF 0 # client.getXLockCount () THEN
          RAISE Access.Invalid (
                    "Illegal end of transaction." &
                    "Client has to return all X-Locks on end of transaction " &
                    "but there still rests " & Fmt.Int (client.getXLockCount ()));
        END;
        client.setTransaction (on := FALSE);
      END;
      
    EXCEPT
    | Access.Invalid (description) =>
      self.killClient (client, description);
      RAISE Access.Invalid (description)
    END;
    RETURN client.getTransactionNumber ();
  END PutData;
  
    
PROCEDURE GetData		(         self		:T;
                                          client	:ServedClient.T;
	                                  file          :RemoteFile.T;
	                                  pageNo	:CARDINAL;
                                 VAR      pageAge	:CARDINAL;
	                                  lock		:PageLock.ServerMode;
	                                  transferData	:BOOLEAN;
	                                  putEntries	:CommunicationSeq.T)
				:Page.T
				RAISES {Access.Invalid, Access.Locked} =
  VAR
    page			:Page.T;
  BEGIN
    self.checkAccess (client);

    TRY
      IF 0 <  putEntries.size () THEN
        EVAL self.putData (client, Txn.End.No, putEntries);
      END;

      page :=  NARROW (file, ScheduledServerFile.T).getData
        (client, pageNo, pageAge, lock, transferData);
    EXCEPT
    | Access.Invalid (description) =>
      self.killClient (client, description);
      RAISE Access.Invalid (description)
    END;

    RETURN page;
  END GetData;


PROCEDURE StartTransaction	(         self		:T;
                                          client	:ServedClient.T)
				:CARDINAL
				RAISES {Access.Invalid} =
  BEGIN
    self.checkAccess (client);

    IF client.inTransaction () THEN
      self.killClient (client,
                       "Illegal begin of transaction. " &
                       "Client has already started a transaction.");
      RAISE Access.Invalid (client.whyKilled ());
    END;

    client.setTransaction (on := TRUE);

    RETURN client.getTransactionNumber ();
  END StartTransaction;

  
PROCEDURE WaitAccess		(         self		:T;
                                          client	:ServedClient.T;
	                                  file          :RemoteFile.T;
	                                  pageNo	:CARDINAL;
	                                  lock		:PageLock.ServerMode)
				RAISES {Access.Invalid, Access.Locked} =
  BEGIN
    TRY
      NARROW (file, ScheduledServerFile.T).waitAccess (client, pageNo, lock);
    EXCEPT
    | Access.Invalid (description) =>
      self.killClient (client, description);
      RAISE Access.Invalid (description);
    END
  END WaitAccess;
  

BEGIN
END ScheduledServerResource.
