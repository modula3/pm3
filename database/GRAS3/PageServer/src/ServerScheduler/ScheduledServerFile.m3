MODULE ScheduledServerFile
EXPORTS ScheduledServerFile, InternalScheduledServerFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.18  1998/05/19 10:21:56  roland
    Typos and "'" removed from error messages.

    Revision 1.17  1997/06/27 07:06:17  roland
    Files remove their shadow when they are closed. Therefore, shadow
    files have to initialized on every true open of a file with
    initShadow.

    Revision 1.16  1997/06/16 12:21:42  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.15  1997/05/15 17:06:22  roland
    Bugfix: ScheduledClientFile has to set mode and kind in open method when
    it is not used by another client. Therfore: methods setAccessMode and setKind
    added to internal interface of BaseScheduledServerFile.

    Revision 1.14  1997/05/05 10:53:46  roland
    Bugfix in open method for files. Access mode is only checked when
    client set is not empty.

    Revision 1.13  1997/04/24 12:13:49  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.12  1996/11/21 07:56:03  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.11  1996/11/20 12:11:36  roland
    Improved exception handling and startup.

    Revision 1.10  1996/10/29 14:40:50  rbnix
    	New parameter pageAge added.

    	StartTransaction returns the actual transaction number.

    Revision 1.9  1996/08/06 16:32:50  roland
    Merge of PAGESERVER and main branch.

    Revision 1.8.2.2  1996/08/01 18:00:19  rbnix
    	Method shutdown changed: now all resources are prepared to be freed.

    	Semantic of method inUse extended to allow check over all clients.

    Revision 1.8.2.1  1996/07/11 11:08:33  rbnix
    	Method waitAccess added to provide global deadlock detection.

    Revision 1.8  1996/03/11 17:23:19  rbnix
    	Method close for pages is removed. All cached data and locks
    	must be returned before a file can be closed.

    Revision 1.7  1996/03/08 11:39:44  rbnix
    	Close procedure for pages added. This procedure checks closing
    	the file is ok due to pages can be given up (unused C-locks)
    	or must be hold (used C or X-locks).

    Revision 1.6  1996/03/06 16:41:12  rbnix
    	Bug fixed: in Shutdown is now checked if file is still open.

    Revision 1.5  1996/03/06 11:24:23  rbnix
    	Bug fixed: in procedure init now the supertypes method is
    	called.

    Revision 1.4  1996/03/06 08:47:22  rbnix
    	Error descriptions improved.

    Revision 1.3  1996/03/02 15:07:32  rbnix
    	Bug fixed: use of ScheduledClientSetDef instead abstract type
    	ScheduledClientSet. Objects of this type are now initialzed
    	well.

    	Bug fixed: check on found/new files/resources adjusted.

    Revision 1.2  1996/02/28 11:03:19  rbnix
    	File and resource pathes are now related to a root path
    	via Config. Errors related to creation of are submittet to
    	clients via exception PageFile.NoAccess.

    Revision 1.1  1996/02/26 17:59:45  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- ScheduledServerFile ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledServerFile AS Super;
IMPORT
  Pathname,
  Page,
  PageFile, PageFileSystem,
  PageCache,
  Access, PageLock, Transaction,
  CommunicationEntry, ClientInfoSeq,
  ServedClient,
  BaseScheduledServerResource,
  InternalBaseScheduledServerFile,
  ScheduledServerPage, ScheduledServerPageTbl;
IMPORT ServedClientSetDef AS ServedClientSet;


REVEAL
  T			= Internal BRANDED OBJECT
      pageTable		:ScheduledServerPageTbl.Default;
      clientSet		:ServedClientSet.T;

    METHODS
      checkAccess	(         client	:ServedClient.T)
			RAISES {Access.Invalid}
			:= CheckAccess;

      internalClose	(         force		:BOOLEAN)
			:= InternalClose;

    OVERRIDES
      init		:= Init;
      open		:= Open;
      close		:= Close;
      shutdown		:= Shutdown;
      killClient	:= KillClient;

      inUse		:= InUse;
      user              := User;

      checkData		:= CheckData;
      putData		:= PutData;
      getData		:= GetData;
      waitAccess	:= WaitAccess;
    END;
  
    
PROCEDURE Init		(         self		:T;
                                  resource	:BaseScheduledServerResource.T;
                                  baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:Super.T 
			RAISES {PageFile.NoAccess}=
  BEGIN
    self.pageTable := NEW (ScheduledServerPageTbl.Default).init ();
    self.clientSet := NEW (ServedClientSet.T).init ();

    RETURN Super.T.init (self, resource, baseName, mode, kind, new);
  END Init;

  
PROCEDURE CheckAccess		(         self		:T;
                                          client	:ServedClient.T)
				RAISES {Access.Invalid} =
  BEGIN
    IF NOT self.clientSet.member (client) THEN
      RAISE Access.Invalid (
                "Invalid access of file '" & self.getBaseName () & "'. " &
                "It is not opened.");
    END;
  END CheckAccess;


PROCEDURE Open		(         self		:T;
                                  client	:ServedClient.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind)
			RAISES {Access.Denied, PageFile.NoAccess, Access.Invalid} =
  BEGIN
    IF self.clientSet.member (client) THEN
      RAISE Access.Invalid (
                "Cannot open file '" & self.getBaseName () &
                "'. It is already opened by the client.");
    END;

    IF self.clientSet.isEmpty () THEN
      (* set up mode and kind determined as first user *)
      self.setAccessMode (mode);
      self.setKind(kind);

      (* open files *)
      self.getPersistentMedia ().getFile ().open ();
      (* initialize new shadow file *)
      self.initShadow();
      self.getTemporaryMedia ().getFile ().open ();
      
    ELSE
      (* check mode and kind against already set values *)
      CASE self.getAccessMode () OF
      | Access.Mode.ReadOnlyShared,
        Access.Mode.ReadWriteShared =>
        IF mode # self.getAccessMode () THEN
          RAISE Access.Denied (
                    "Cannot open file '" & self.getBaseName () & "'. " &
                    "It is already opened with conflicting access mode (" &
                    Access.FmtMode (self.getAccessMode ()) & ").");
        END;
      
      | Access.Mode.ReadWriteExclusive =>
        RAISE Access.Denied (
                  "Cannot open file '" & self.getBaseName () & "'. " &
                  "It is already opened in exclusive mode (" &
                  Access.FmtMode (self.getAccessMode ()) & ").");
      END;

      IF self.getKind () # kind THEN
        RAISE PageFile.NoAccess (
                  "Cannot open file '" & self.getBaseName () &
                  "'. It is already openend on another specified kind.");
      END;
    END;

    EVAL self.clientSet.insert (client);
  END Open;

  
(*
 | --- InternalClose ------------------------------------------------------
 Save changed data located in cache or shadow file to persistent
 file and close files. Normally this will be done if no client still works
 on the file but also if the force flag is set.
 | ------------------------------------------------------------------------
 *) 
PROCEDURE InternalClose	(         self		:T;
                                  force		:BOOLEAN) =
  VAR
    persistentFile	:PageFile.T;
    temporaryFile	:PageFile.T;
    page		:ScheduledServerPage.T;
    pageNo		:CARDINAL;
    i			:ScheduledServerPageTbl.Iterator;
  BEGIN
    IF force OR self.clientSet.isEmpty () THEN
      persistentFile := self.getPersistentMedia ().getFile ();
      temporaryFile := self.getTemporaryMedia ().getFile ();

      IF persistentFile.isOpen () THEN
        (* relocating pages to persistent media *)
        i := self.pageTable.iterate ();
        WHILE i.next (pageNo, page) DO
          page.relocate ();
        END;

        (* save data in persistent file *)
        PageCache.FlushPages (self.getPersistentMedia ());
        persistentFile.close ();

        (* close and delete temporary file *)
        TRY
          temporaryFile.close ();
          PageFileSystem.DeleteFile (temporaryFile.getFileName ());
        EXCEPT
        | PageFile.NoAccess =>
          (* can be ignored *)
        END;

      END;
    END;
  END InternalClose;
    

PROCEDURE Shutdown	(         self		:T) =
  VAR
    page		:ScheduledServerPage.T;
    pageNo		:CARDINAL;
    i			:ScheduledServerPageTbl.Iterator;
  BEGIN
    self.internalClose (force := TRUE);

    i := self.pageTable.iterate ();
    WHILE i.next (pageNo, page) DO
      page.shutdown ();
    END;
  END Shutdown;

  
PROCEDURE Close		(         self		:T;
                                  client	:ServedClient.T)
			RAISES {Access.Invalid} =
  VAR
    page		:ScheduledServerPage.T;
    pageNo		:CARDINAL;
    i			:ScheduledServerPageTbl.Iterator;
  BEGIN
    self.checkAccess (client);

    i := self.pageTable.iterate ();
    WHILE i.next (pageNo, page) DO
      IF page.inUse (client) THEN
        RAISE Access.Invalid (
                  "Error on closing file '" & self.getBaseName () &
                  "'. There are still locks.");
      END
    END;

    EVAL self.clientSet.delete (client);
    self.internalClose (force := FALSE);
  END Close;

  
PROCEDURE KillClient	(         self		:T;
                                  client	:ServedClient.T) =
  VAR
    page		:ScheduledServerPage.T;
    pageNo		:CARDINAL;
    i			:ScheduledServerPageTbl.Iterator;
  BEGIN
    i := self.pageTable.iterate ();
    WHILE i.next (pageNo, page) DO
      IF page.inUse (client) THEN
        page.killClient (client);
      END
    END;

    EVAL self.clientSet.delete (client);
    self.internalClose (force := FALSE);
  END KillClient;

  
PROCEDURE InUse		(         self		:T;
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
  
PROCEDURE GetPage	(         self		:T;
                                  pageNo	:CARDINAL)
			:ScheduledServerPage.T =
  VAR
    page		:ScheduledServerPage.T;
  BEGIN
    IF NOT self.pageTable.get (pageNo, page) THEN
      page := NEW (ScheduledServerPage.T).init (self, pageNo);
      EVAL self.pageTable.put (pageNo, page);
    END;

    RETURN page;
  END GetPage;
  

PROCEDURE CheckData	(         self		:T;
                                  client	:ServedClient.T;
                                  end		:Transaction.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid} =
  BEGIN
    self.checkAccess (client);

    GetPage (self, entry.pageNo).checkData (client, end, entry)    
  END CheckData;

  
PROCEDURE PutData	(         self		:T;
                                  client	:ServedClient.T;
                                  end		:Transaction.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid}=
  BEGIN
    self.checkAccess (client);

    GetPage (self, entry.pageNo).putData (client, end, entry)    
  END PutData;

  
PROCEDURE GetData	(         self		:T;
                                  client	:ServedClient.T;
                                  pageNo	:CARDINAL;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN)
			:Page.T
			RAISES {Access.Invalid, Access.Locked} =
  BEGIN
    self.checkAccess (client);

    RETURN GetPage (self, pageNo).getData
      (client, pageAge, lock, transferData)
  END GetData;

  
PROCEDURE WaitAccess	(         self		:T;
                                  client	:ServedClient.T;
                                  pageNo	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Invalid, Access.Locked} =
  BEGIN
    self.checkAccess (client);
    
    GetPage (self, pageNo).waitAccess (client, lock)
  END WaitAccess;


BEGIN
END ScheduledServerFile.
