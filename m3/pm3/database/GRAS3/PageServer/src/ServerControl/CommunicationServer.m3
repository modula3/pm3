MODULE CommunicationServer;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.14  1997/06/13 16:14:44  roland
    Slightly changed log message.

    Revision 1.13  1997/06/13 11:49:53  rbnix
    	Journal output for new connection extended.

    Revision 1.12  1997/04/24 12:13:24  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.11  1997/04/11 12:34:31  roland
    Slight modification in startup procedure of server.
    Man page adopted to gras name server and new name.

    Revision 1.10  1996/11/21 07:55:38  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.9  1996/11/14 14:13:27  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.8  1996/10/29 14:45:27  rbnix
    	New parameter pageAge added.

    	StartTransaction returns the actual transaction number.

    Revision 1.7  1996/08/06 16:32:22  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.4  1996/08/01 18:06:40  rbnix
    	New file administration methods deleteRemoteFile,
    	copyRemoteFile, renameRemoteFile, existsRemoteFile,
    	remoteFileInUse, remoteFileSize and getRemoteFiles added.

    Revision 1.6.2.3  1996/07/24 12:52:46  rbnix
    	New parameter clientInfo added.

    Revision 1.6.2.2  1996/07/11 10:49:42  rbnix
    	Method waitAccess added to perform global deadlock detection
    	at the server.

    Revision 1.6.2.1  1996/06/07 10:26:54  rbnix
    	Journal message improved.

    Revision 1.6  1996/03/15 14:22:25  rbnix
    	In method init/open attribute clientID added.

    Revision 1.5  1996/03/06 08:18:42  rbnix
    	Description for killClient in regular close operation added.

    Revision 1.4  1996/03/05 15:13:42  rbnix
    	Procedure Close enhanced: the client is now killed on end of
    	closing the port.

    Revision 1.3  1996/03/01 13:01:02  rbnix
    	Output to journal for variant TestServerCommunication added.

    Revision 1.2  1996/02/29 09:28:48  rbnix
    	Methods close and closeRemoteFile added.

    Revision 1.1  1996/02/26 17:58:17  rbnix
    	First version of subsystem ServerControl.

*)
(***************************************************************************)
IMPORT
  Pathname, Fmt, TextSeq,
  Variant, Journal,
  Page,
  PageFile,
  PageCache,
  Access, PageLock, Transaction, 
  RemoteFile, CallbackPort, ClientInfo, ClientInfoSeq,
  CommunicationSeq, CommunicationSeqSupport,
  ServedClientTable, ServedClient,
  ServerScheduler, ScheduledServerResource, ScheduledServerFile;

REVEAL
  T				= Public BRANDED OBJECT
      client			:ServedClient.T;
      resource			:ScheduledServerResource.T;

    OVERRIDES
      init			:= Init;
      close			:= Close;
      
      registerLocalFile		:= RegisterLocalFile;
      unregisterLocalFile	:= UnregisterLocalFile;
      deleteRemoteFile		:= DeleteRemoteFile;
      copyRemoteFile		:= CopyRemoteFile;
      renameRemoteFile		:= RenameRemoteFile;
      existsRemoteFile		:= ExistsRemoteFile;
      remoteFileInUse		:= RemoteFileInUse;
      remoteFileSize		:= RemoteFileSize;
      getRemoteFiles		:= GetRemoteFiles;
      getRemoteFileUser         := GetRemoteFileUser;
      
      openRemoteFile		:= OpenRemoteFile;
      closeRemoteFile		:= CloseRemoteFile;
      getData			:= GetData;
      putData			:= PutData;
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
	                                  access	:Access.Mode;
	                                  new		:BOOLEAN;
				          callback	:CallbackPort.T;
                                          clientInfo	:ClientInfo.T;
	                         VAR (* out *) clientID	:TEXT)
				:T
				RAISES {PageFile.NoAccess, Access.Denied,
                                        Access.Invalid} =
  BEGIN
    self.client := ServedClientTable.New (baseName, callback, clientInfo);

    IF Variant.RegularServerJournal THEN
      clientID := self.client.getID ();
      Journal.Add ("Connection "&
                   "for process " & Fmt.Int (clientInfo.pid) & " " &
                   "from user " & clientInfo.username & "@" &
                   clientInfo.hostname & " " &
                   "on resource '" & baseName &
                   "' established as client " & clientID);
    END;

    self.resource := ServerScheduler.OpenResource (
                         self.client, baseName, access, new);

    RETURN self;
  END Init;                                       


PROCEDURE Close			(         self		:T)
				RAISES {Access.Invalid} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.Close (" &
                   "client = " & self.client.getID () &
                   ", resource = " & self.resource.getBaseName () & ")");
    END;

    TRY
      self.resource.close (self.client);
      self.client.kill ("Regular close operation terminated sucessful.");
    FINALLY
      PageCache.EndAccess ();
    END
  END Close;
  

PROCEDURE RegisterLocalFile	(         self		:T;
                                          baseName	:Pathname.T)
				RAISES {PageFile.NoAccess, Access.Invalid} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.RegisterLocalFile (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      self.resource.registerLocalFile (self.client, baseName);
    FINALLY
      PageCache.EndAccess ();
    END
  END RegisterLocalFile;


PROCEDURE UnregisterLocalFile	(         self		:T;
                                          baseName	:Pathname.T)
				RAISES {PageFile.NoAccess, Access.Invalid} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.UnregisterLocalFile (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      self.resource.unregisterLocalFile (self.client, baseName);
    FINALLY
      PageCache.EndAccess ();
    END
  END UnregisterLocalFile;


PROCEDURE DeleteRemoteFile	(         self		:T;
                                          baseName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.DeleteRemoteFile (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      self.resource.deleteFile (baseName);
    FINALLY
      PageCache.EndAccess ();
    END;
  END DeleteRemoteFile;


PROCEDURE CopyRemoteFile	(         self		:T;
                                          sourceName	:Pathname.T;
	                                  destName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.CopyRemoteFile (" &
                   "client = " & self.client.getID () &
                   ", sourceName = " & sourceName &
                   ", destName = " & destName & ")");
    END;

    TRY
      self.resource.copyFile (sourceName, destName);
    FINALLY
      PageCache.EndAccess ();
    END;
  END CopyRemoteFile;


PROCEDURE RenameRemoteFile	(         self		:T;
                                          oldName	:Pathname.T;
	                                  newName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.RenameRemoteFile (" &
                   "client = " & self.client.getID () &
                   ", oldName = " & oldName &
                   ", newName = " & newName & ")");
    END;

    TRY
      self.resource.renameFile (oldName, newName);
    FINALLY
      PageCache.EndAccess ();
    END;
  END RenameRemoteFile;


PROCEDURE ExistsRemoteFile	(         self		:T;
                                          baseName	:Pathname.T)
				:BOOLEAN =
  VAR
    result			:BOOLEAN;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.ExistsRemoteFile (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      result := self.resource.existsFile (baseName);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN result;
  END ExistsRemoteFile;
    

PROCEDURE RemoteFileInUse		(         self		:T;
                                          baseName	:Pathname.T)
				:BOOLEAN =
  VAR
    result			:BOOLEAN;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.RemoteFileInUse (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      result := self.resource.fileInUse (baseName);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN result;
  END RemoteFileInUse;


PROCEDURE GetRemoteFileUser	(         self		:T;
                                          baseName	:Pathname.T)
				:ClientInfoSeq.T =
  VAR
    result			:ClientInfoSeq.T;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.GetRemoteFileUser (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      result := self.resource.getFileUser (baseName);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN result;
  END GetRemoteFileUser; 


PROCEDURE RemoteFileSize	(         self		:T;
                                          baseName	:Pathname.T)
				:CARDINAL
				RAISES {PageFile.NoAccess} =
  VAR
    result			:CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.RemoteFileSize (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName & ")");
    END;

    TRY
      result := self.resource.fileSize (baseName);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN result;
  END RemoteFileSize;


PROCEDURE GetRemoteFiles	(         self		:T)
				:TextSeq.T =
  VAR
    result			:TextSeq.T;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.GetRemoteFiles ()");
    END;

    TRY
      result := self.resource.getFiles ();
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN result;
  END GetRemoteFiles;


PROCEDURE OpenRemoteFile	(         self		:T;
                                          baseName      :Pathname.T;
                                          mode          :Access.Mode;
	                                  kind		:Access.Kind;
	                                  new		:BOOLEAN) :RemoteFile.T
				RAISES {Access.Denied, PageFile.NoAccess,
                                        Access.Invalid} =
  VAR
    file			:RemoteFile.T;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.OpenRemoteFile (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & baseName &
                   ", kind = " & Access.FmtKind (kind) &
                   ", new = " & Fmt.Bool (new) & ")");
    END;

    TRY
      file := self.resource.openFile (self.client, baseName, mode, kind, new);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN file;
  END OpenRemoteFile;


PROCEDURE CloseRemoteFile	(         self		:T;
                                          file		:RemoteFile.T)
				RAISES {Access.Invalid} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.CloseRemoteFile (" &
                   "client = " & self.client.getID () &
                   ", baseName = " & GetFileName (file) & ")");
    END;

    TRY
      self.resource.closeFile (self.client, file);
    FINALLY
      PageCache.EndAccess ();
    END
  END CloseRemoteFile;


PROCEDURE GetData		(         self		:T;
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
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.GetData (" &
                   "client = " & self.client.getID () &
                   ", file = " & GetFileName (file) &
                   ", pageNo = " & Fmt.Int (pageNo) &
                   ", pageAge = " & Fmt.Int (pageAge) &
                   ", lock = " & PageLock.FmtMode (lock) &
                   ", transferData = " & Fmt.Bool (transferData) &
                   ", putEntries = " &
                   CommunicationSeqSupport.Fmt (putEntries, GetFileName) & ")");
    END;

    TRY
      page := self.resource.getData (self.client, file, pageNo, pageAge,
                                     lock, transferData, putEntries);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN page;
  END GetData;


PROCEDURE PutData		(         self		:T;
                                          end		:Transaction.End;
	                                  entries       :CommunicationSeq.T)
				RAISES {Access.Invalid} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.PutData (" &
                   "client = " & self.client.getID () &
                   ", end = " & Transaction.FmtEnd (end) &
                   ", entries = " &
                   CommunicationSeqSupport.Fmt (entries, GetFileName) & ")");
    END;

    TRY
      self.resource.putData (self.client, end, entries);
    FINALLY
      PageCache.EndAccess ();
    END;
  END PutData;


PROCEDURE StartTransaction	(         self		:T)
				:CARDINAL
				RAISES {Access.Invalid} =
  VAR
    transactionNumber		:CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.StartTransaction (" &
                   "client = " & self.client.getID () & ")");
    END;

    TRY
      transactionNumber := self.resource.startTransaction (self.client);
    FINALLY
      PageCache.EndAccess ();
    END;

    RETURN transactionNumber;
  END StartTransaction;


PROCEDURE WaitAccess		(         self		:T;
                                          file		:RemoteFile.T;
	                                  pageNo	:CARDINAL;
	                                  lock		:PageLock.ServerMode;)
				RAISES {Access.Invalid, Access.Locked} =
  BEGIN
    PageCache.BeginAccess ();
    IF Variant.TestServerCommunication THEN
      Journal.Add ("CommunicationServer.WaitAccess (" &
                   ", client = " & self.client.getID () &
                   ", file = " & GetFileName (file) &
                   ", pageNo = " & Fmt.Int (pageNo) &
                   ", lock = " & PageLock.FmtMode (lock) & ")");
    END;

    TRY
      self.resource.waitAccess (self.client, file, pageNo, lock);
    FINALLY
      PageCache.EndAccess ();
    END;
  END WaitAccess;
  

BEGIN
END CommunicationServer.
