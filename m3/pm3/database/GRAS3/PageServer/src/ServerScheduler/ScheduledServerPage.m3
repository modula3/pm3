MODULE ScheduledServerPage;

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

    Revision 1.19  1998/05/19 10:21:57  roland
    Typos and "'" removed from error messages.

    Revision 1.18  1998/02/04 11:49:06  roland
    Last change introduced a bug at transaction abort. Fixed that.

    Revision 1.17  1998/01/30 09:06:07  roland
    Avoid a server crash when downgrading from an X-lock to an O-lock
    without page data.

    Revision 1.16  1997/06/16 12:21:45  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.15  1996/11/20 12:11:37  roland
    Improved exception handling and startup.

    Revision 1.14  1996/11/11 09:37:32  rbnix
    	Error messages for Netobj.Error evaluated.

    Revision 1.13  1996/10/29 14:42:48  rbnix
    	New parameter pageAge added. This used to support some more
    	error checks.

    	Debug output improved.

    Revision 1.12  1996/10/10 12:56:05  rbnix
    	In GetData: more information for error handling used.
    	Handling of killed clients causes more secure waiting till
    	resources of killed client are freed.

    Revision 1.11  1996/10/02 14:56:33  rbnix
    	Bug fixed in function GetData: if a release request is rejected
    	by one client the server now propagates P->C lock changes
    	immediate to other clients. The server lock table is now
    	also adjusted.

    Revision 1.10  1996/08/06 16:32:53  roland
    Merge of PAGESERVER and main branch.

    Revision 1.9.2.4  1996/08/01 18:00:50  rbnix
    	Method shutdown added to free page resources.

    Revision 1.9.2.3  1996/07/11 12:11:36  rbnix
    	Method waitAccess added to provide global deadlock detection.

    	Output to journal improved when calling waitAccess.

    	Bug fixed in GetData: on requested X-locks the P-locks are now
    	freed only if there is no other client with granted
    	X-lock. The exception is now re-raised as expected.

    Revision 1.9.2.2  1996/06/26 13:58:06  rbnix
       	Bug fixed in GetData: release requests will now only sended to
       	living clients.

    Revision 1.9.2.1  1996/06/07 14:04:13  rbnix
    	Test in CheckData disabled. Due to two transfers (X->C, C->O)
    	aren't realized as releated to each other.

    Revision 1.9  1996/03/15 14:20:11  rbnix
    	Output to journal improved.

    Revision 1.8  1996/03/11 17:23:22  rbnix
    	Method close for pages is removed. All cached data and locks
    	must be returned before a file can be closed.

    Revision 1.7  1996/03/08 11:39:46  rbnix
    	Close procedure for pages added. This procedure checks closing
    	the file is ok due to pages can be given up (unused C-locks)
    	or must be hold (used C or X-locks).

    Revision 1.6  1996/03/06 16:42:03  rbnix
    	Bug fixed: in fmt.

    Revision 1.5  1996/03/06 16:19:32  rbnix
    	Method fmt added.

    	Output on journal added for variant TestServerScheduler.

    Revision 1.4  1996/03/06 08:47:24  rbnix
    	Error descriptions improved.

    Revision 1.3  1996/03/06 08:14:15  rbnix
    	Output to journal for variant TestServerCommunication added.

    Revision 1.2  1996/02/29 17:42:20  rbnix
    	Adopted to new function PageCache.GetPage.

    Revision 1.1  1996/02/26 17:59:48  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- ScheduledServerPage ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT Fmt AS StdFmt;
IMPORT
  Thread, NetObj,
  Variant, Journal,
  ErrorSupport,
  Page, PageData,
  PageHandle, PageCache,
  PageLock, Access, Txn,
  CommunicationEntry, CommunicationSeqSupport,
  ServedClient,
  BaseScheduledServerFile, InternalBaseScheduledServerFile, ServerLockTable,
  CallbackPort;

IMPORT RemoteFile;

REVEAL
  T			= Public BRANDED OBJECT
      logPageNo		:CARDINAL;
      lockTable		:ServerLockTable.T;
      lockState		:PageLock.ServerMode;
      pageAge		:CARDINAL;
      handle		:PageHandle.T;
      file		:BaseScheduledServerFile.T;

    METHODS
      fmt		() :TEXT
			:= Fmt;

    OVERRIDES
      init		:= Init;

      checkData		:= CheckData;
      getData		:= GetData;
      putData		:= PutData;
      relocate		:= Relocate;
      waitAccess	:= WaitAccess;

      killClient	:= KillClient;
      shutdown		:= Shutdown;
      inUse		:= InUse;
    END;
  

PROCEDURE Init		(         self		:T;
	                          file		:BaseScheduledServerFile.T;
                                  pageNo	:CARDINAL)
			:T =
  BEGIN
    self.logPageNo := pageNo;
    self.lockTable := NEW (ServerLockTable.T).init ();
    self.lockState := PageLock.Mode.C;
    self.pageAge := 0;
    self.handle := PageCache.GetPage (pageNo, file.getPersistentMedia ());
    self.file := file;

    RETURN self;
  END Init;

  
PROCEDURE GetData	(         self		:T;
                                  client	:ServedClient.T;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN)
			:Page.T
			RAISES {Access.Invalid, Access.Locked} =
  VAR
    wait		:BOOLEAN;
    otherClient		:ServedClient.T;
    otherLock		:PageLock.ServerMode;
    i			:ServerLockTable.Iterator;
    page		:Page.T;
  BEGIN
    IF Variant.TestServerScheduler THEN
      Journal.Add ("ScheduledServerPage.GetData (" &
                   " client = " & client.getID () &
                   ", transaction = " & StdFmt.Int(client.getTransactionNumber ()) &
                   ")\n" &
		   "page  = (" & self.fmt () & ")");
    END;

    IF NOT (client.inTransaction ()) THEN
      RAISE Access.Invalid ("Cannot get data. There is no started transaction.");
    END;

    CASE lock OF
    | PageLock.Mode.O, PageLock.Mode.P =>
      RAISE Access.Invalid ("Unable to get O- or P-locks.")

    | PageLock.Mode.C =>
      CASE self.lockTable.get (client) OF
      | PageLock.Mode.X =>
        RAISE Access.Invalid ("Unable to downgrade a lock while getting data.");

      | PageLock.Mode.C =>
        IF self.pageAge # pageAge THEN
          RAISE Access.Invalid (
                    "Page age at client and server must be consistent (#1).");
        END;

      | PageLock.Mode.O =>
        (* no page at client => no age to check *)

      | PageLock.Mode.P =>
        RAISE Access.Invalid ("Illegal request by client while having a P-lock.");
      END;

      WHILE self.lockState # PageLock.Mode.C DO
        IF Variant.TestServerScheduler THEN
          Journal.Add ("ScheduledServerPage.waitAccess #1");
        END;
        self.file.waitAccess (client, self.lockTable,
                              self.logPageNo, PageLock.Mode.C);
      END;
      self.lockTable.put (client, PageLock.Mode.C);

    | PageLock.Mode.X =>
      CASE self.file.getAccessMode () OF
      | Access.Mode.ReadOnlyShared =>
        RAISE Access.Invalid ("Wrong access mode to change data");

      | Access.Mode.ReadWriteShared,
        Access.Mode.ReadWriteExclusive =>
        (* ok *)
      END;
      IF self.lockTable.get (client) = PageLock.Mode.C THEN
        IF self.pageAge # pageAge THEN
          RAISE Access.Invalid (
                    "Page age at client and server must be consistent (#2).");
        END;
      END;

      REPEAT
        (* starting callback of C-locks *)
        IF self.lockState = PageLock.Mode.C THEN
          self.lockState := PageLock.Mode.P;
        END;

        IF self.lockState = PageLock.Mode.P THEN
          wait := FALSE;
          i := self.lockTable.iterate ();
          WHILE (NOT wait) AND (i.next (otherClient, otherLock)) DO
            IF otherClient.isKilled () THEN
              wait := TRUE;
            ELSIF(otherClient # client) AND (otherLock = PageLock.Mode.C) THEN
              TRY
                IF Variant.TestServerCommunication OR
                   Variant.TestServerScheduler THEN
                  Journal.Add ("ScheduledServerPage.ReleaseData (" &
                    "client = " & otherClient.getID () &
                    ", file = " & self.file.getBaseName () &
                    ", pageNo = " & StdFmt.Int (self.logPageNo) &
                    ", pageAge = " & StdFmt.Int (self.pageAge) &
                    ", lock = " & PageLock.FmtMode (PageLock.Mode.P) & ")");
                END;

                otherClient.getCallbackPort ().releaseData (
                  self.file, self.logPageNo, self.pageAge, PageLock.Mode.P);
                self.lockTable.put (otherClient, PageLock.Mode.P);

                IF Variant.TestServerCommunication OR
                   Variant.TestServerScheduler THEN
                  Journal.Add ("ScheduledServerPage.ReleaseData () = done");
                END;
                
              EXCEPT
              | Thread.Alerted =>
                (*
                  'Simple' solution: the client is marked as killed but
                  resources are freed in recent future.
                  (Freeing is still a job controlled on higher levels.)
                *)
                otherClient.kill ("Execution of release callback failed, " &
                                  "thread alerted");
                wait := TRUE;

              | NetObj.Error (code) =>
                (*
                  'Simple' solution: the client is marked as killed but
                  resources are freed in recent future.
                  (Freeing is still a job controlled on higher levels.)
                *)
                otherClient.kill ("Execution of release callback failed. " &
				  ErrorSupport.Fmt (code));
                wait := TRUE;

              | Access.Locked =>
                (* client revoked the release request *)
                wait := TRUE;

                IF Variant.TestServerCommunication OR
                   Variant.TestServerScheduler THEN
                  Journal.Add ("ScheduledServerPage.ReleaseData () = revoked");
                END;
              | CallbackPort.FatalError(info) =>
                (*
                  'Simple' solution: the client is marked as killed but
                  resources are freed in recent future.
                  (Freeing is still a job controlled on higher levels.)
                *)
                otherClient.kill ("Execution of release callback failed. " &
				  ErrorSupport.ToText (info));
                wait := TRUE;

              END;
            END;
          END;

        ELSE
          <* ASSERT (self.lockState = PageLock.Mode.X) *>
          wait := TRUE
        END;

        IF wait THEN
          TRY
            IF Variant.TestServerScheduler THEN
              Journal.Add ("ScheduledServerPage.waitAccess #2");
            END;
            self.file.waitAccess (client, self.lockTable,
                                  self.logPageNo, PageLock.Mode.X);
          EXCEPT
          | Access.Locked =>
            IF self.lockState = PageLock.Mode.P THEN
              i := self.lockTable.iterate ();
              WHILE i.next (otherClient, otherLock) DO
                IF otherLock = PageLock.Mode.P THEN
                  TRY
                    otherClient.getPropagationData ().addhi (
                                    CommunicationEntry.T {self.file,
                                                          self.logPageNo,
                                                          self.pageAge,
                                                          PageLock.Mode.C,
                                                          page := NIL});

                    IF Variant.TestServerCommunication OR
                       Variant.TestServerScheduler THEN
                      Journal.Add ("ScheduledServerPage.PropagateData (" &
                        "client = " & otherClient.getID () &
                        ", end = Txn.End.No" &
                        ", entries = " & CommunicationSeqSupport.Fmt (
                            otherClient.getPropagationData (),GetFileName) & ")");
                    END;          
                      
                    otherClient.getCallbackPort ().propagateData (
                                    Txn.End.No,
                                    otherClient.getPropagationData ());
                    otherClient.clearPropagationData ();
                    self.lockTable.put (otherClient, PageLock.Mode.C);

                  EXCEPT
                  | Thread.Alerted =>
                    (*
                      'Simple' solution: the client is marked as killed but
                      resources are freed in recent future.
                      (Freeing is still a job controlled on higher levels.)
                    *)
                    otherClient.kill ("Propagation of data failed. " &
                                      "thread alerted.");

                  | NetObj.Error (code) =>
                    (*
                      'Simple' solution: the client is marked as killed but
                      resources are freed in recent future.
                      (Freeing is still a job controlled on higher levels.)
                    *)
                    otherClient.kill ("Propagation of data failed. " &
                                      ErrorSupport.Fmt (code));
                  | CallbackPort.FatalError(info) => 
                    (*
                      'Simple' solution: the client is marked as killed but
                      resources are freed in recent future.
                      (Freeing is still a job controlled on higher levels.)
                    *)
                    otherClient.kill ("Propagation of data failed. " &
                                      ErrorSupport.ToText (info));
                  END;
                END;
              END;
              self.lockState := PageLock.Mode.C;
            END;
            RAISE Access.Locked;
          END;
        END;
      UNTIL NOT (wait);

      self.lockState := PageLock.Mode.X;
      self.lockTable.put (client, PageLock.Mode.X);
      client.incXLockCount ();
    END;

    IF transferData THEN
      page := NEW (Page.T);
      PROCEDURE GetAll(READONLY data: PageData.T) =
        BEGIN
          page.data := data;
        END GetAll;
      BEGIN
        self.handle.getAll (GetAll);
      END;
      pageAge := self.pageAge;
    ELSE
      page := NIL;
    END;

    IF Variant.TestServerScheduler THEN
      Journal.Add ("page' = (" & self.fmt () & ")");
    END;

    RETURN page;
  END GetData;

(* Just for testing: *)
PROCEDURE GetFileName(f: RemoteFile.T): TEXT =
BEGIN
  RETURN NARROW(f, BaseScheduledServerFile.T).getBaseName();
END GetFileName;

PROCEDURE CheckData	(         self		:T;
                                  client	:ServedClient.T;
                                  end		:Txn.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid} =
  BEGIN
    (*
    IF Variant.TestServerScheduler THEN
      Journal.Add ("ScheduledServerPage.CheckData (" &
                   " client = " & client.getID () &
                   ", transaction = " & StdFmt.Int(client.getTransactionNumber ()) &
                   ")\n" &
		   "page  = (" & self.fmt () & ")");
    END;
    *)

    CASE self.lockTable.get (client) OF
    | PageLock.Mode.O,
      PageLock.Mode.P,
      PageLock.Mode.C =>
      IF entry.lock # PageLock.Mode.O THEN
        RAISE Access.Invalid ("Putting data supports only downgrade locks")
      END;
      IF entry.page # NIL THEN
        RAISE Access.Invalid ("Downgrades to O-locks cannot transfer data")
      END;

    | PageLock.Mode.X =>
      <* ASSERT (self.lockState = PageLock.Mode.X) *>
      CASE entry.lock OF
      | PageLock.Mode.X, PageLock.Mode.P =>
        RAISE Access.Invalid ("Putting data supports only downgrade locks");

      | PageLock.Mode.C =>
        IF entry.page # NIL THEN
          IF end # Txn.End.Commit AND end # Txn.End.Chain THEN
            RAISE Access.Invalid ("Putting pages only at commit/chain time")
          END;
          IF entry.pageAge # client.getTransactionNumber () THEN
            RAISE Access.Invalid ("Age of page and transaction differs");
          END
        ELSE
          IF (end # Txn.End.No) THEN
            RAISE Access.Invalid ("Downgrading X- to C-lock only in transactions")
          END
        END;

      | PageLock.Mode.O =>
        IF entry.page # NIL THEN
          RAISE Access.Invalid ("Downgrades to O-locks cannot transfer data")
        END;
        IF end = Txn.End.Commit THEN
          (* maybe there is a transfer X -> C with page in this sequence *)
          (*
          RAISE Access.Invalid (
              "Downgrading X-locks with commit requires page transfer \n(entry =" &
                CommunicationEntry.Fmt (entry, GetFileName) & ")");
          *)
        ELSIF end = Txn.End.Chain THEN
          RAISE Access.Invalid (
                    "Downgrading X-locks with chain requires page transfer \n(entry =" &
                    CommunicationEntry.Fmt (entry, GetFileName) & ")");
        END;
      END
    END;

    (*
    IF Variant.TestServerScheduler THEN
      Journal.Add ("page' = (" & self.fmt () & ")");
    END;
    *)
  END CheckData; 


PROCEDURE PutData	(         self		:T;
                                  client	:ServedClient.T;
                                  end		:Txn.End;
                         READONLY entry		:CommunicationEntry.T) =
  VAR
    otherClient		:ServedClient.T;
    otherLock		:PageLock.ServerMode;
    i			:ServerLockTable.Iterator;
  BEGIN
    IF Variant.TestServerScheduler THEN
      Journal.Add ("ScheduledServerPage.PutData (" &
                   " client = " & client.getID () &
                   ", transaction = " & StdFmt.Int(client.getTransactionNumber ()) &
                   ")\n" &
		   "page  = (" & self.fmt () & ")");
    END;

    CASE self.lockTable.get (client) OF
    | PageLock.Mode.O,
      PageLock.Mode.P,
      PageLock.Mode.C =>
      <* ASSERT (entry.lock = PageLock.Mode.O) *>
      <* ASSERT (entry.page = NIL) *>
      self.lockTable.put (client, entry.lock);

    | PageLock.Mode.X =>
      IF entry.page # NIL THEN
        <* ASSERT (end = Txn.End.Commit) *>
        <* ASSERT (entry.lock = PageLock.Mode.C) *>
        self.pageAge := entry.pageAge;

        (* relocate page to temporary media and put data *)
        WITH temporaryMedia = self.file.getTemporaryMedia () DO
          IF self.handle.getMedia () # temporaryMedia THEN
            self.handle.setPageNo (temporaryMedia.obtainPageNo ());
            self.handle.setMedia (temporaryMedia);
          END;
        END;
        self.handle.putData (entry.page.data);
      ELSE
        (* This case is not handled by checkData anymore. So we should ignore
           it here, too. Server will complain about remaining X-locks later.  *)
        IF end = Txn.End.Commit THEN
          RETURN;
        END;
      END;
      self.lockState := PageLock.Mode.C;
      self.lockTable.put (client, entry.lock);
      client.decXLockCount ();
      
      i := self.lockTable.iterate ();
      WHILE i.next (otherClient, otherLock) DO
        IF otherLock = PageLock.Mode.P THEN
          self.lockTable.put (otherClient, PageLock.Mode.C);
          otherClient.getPropagationData ().addhi (
                          CommunicationEntry.T {self.file,
                                                self.logPageNo,
                                                self.pageAge,
                                                PageLock.Mode.C,
                                                entry.page});
        END
      END
    END;

    IF Variant.TestServerScheduler THEN
      Journal.Add ("page' = (" & self.fmt () & ")");
    END;
    END PutData;


PROCEDURE Relocate	(         self		:T) =
  VAR
    pageData		:PageData.T;
  BEGIN
    WITH
      persistentMedia = self.file.getPersistentMedia (),
      temporaryMedia = self.file.getTemporaryMedia () DO

      (* get page data *)
      PROCEDURE GetAll(READONLY data: PageData.T) =
        BEGIN
          pageData := data;
        END GetAll;
      BEGIN
        self.handle.getAll (GetAll);
      END;

      (* release temporary stuff *)
      temporaryMedia.freePageNo (self.handle.getPageNo ());

      (* set up persistent stuff *)
      self.handle.setPageNo (self.logPageNo);
      self.handle.setMedia (persistentMedia);
      self.handle.putData (pageData);
    END;
  END Relocate; 
  

PROCEDURE WaitAccess	(         self		:T;
                                  client	:ServedClient.T;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Invalid, Access.Locked} =
  BEGIN
    IF Variant.TestServerScheduler THEN
      Journal.Add ("ScheduledServerPage.WaitAccess (" &
                   " client = " & client.getID () &
                   ", transaction = " & StdFmt.Int(client.getTransactionNumber ()) &
                   ")\n" &
		   "page  = (" & self.fmt () & ")");
    END;

    IF NOT (client.inTransaction ()) THEN
      RAISE Access.Invalid("Cannot wait for data. There is no started transaction.");
    END;

    (* repeaded local waiting until page is accessable *)
    WHILE self.lockState # PageLock.Mode.C DO
      IF Variant.TestServerScheduler THEN
        Journal.Add ("ScheduledServerPage.waitAccess #3");
      END;
      self.file.waitAccess (client, self.lockTable, self.logPageNo, lock);
    END;

    IF Variant.TestServerScheduler THEN
      Journal.Add ("ScheduledServerPage.WaitAccess\n" &
		   "page  = (" & self.fmt () & ")");
    END;
  END WaitAccess;


PROCEDURE KillClient	(         self		:T;
                                  client	:ServedClient.T) =
  VAR
    otherClient		:ServedClient.T;
    otherLock		:PageLock.ServerMode;
    i			:ServerLockTable.Iterator;
  BEGIN
    IF Variant.TestServerScheduler THEN
      Journal.Add ("ScheduledServerPage.KillClient (" &
                   " client = " & client.getID () &
                   ", transaction = " & StdFmt.Int(client.getTransactionNumber ()) &
                   ")\n" &
		   "page  = (" & self.fmt () & ")");
    END;

    CASE self.lockTable.get (client) OF
    | PageLock.Mode.O =>
      (* empty *)

    | PageLock.Mode.C, PageLock.Mode.P =>
      self.lockTable.put (client, PageLock.Mode.O);

    | PageLock.Mode.X =>
      <* ASSERT (self.lockState = PageLock.Mode.X) *>
      self.lockState := PageLock.Mode.C;
      self.lockTable.put (client, PageLock.Mode.O);
      client.decXLockCount ();

      i := self.lockTable.iterate ();
      WHILE i.next (otherClient, otherLock) DO
        IF otherLock = PageLock.Mode.P THEN
          self.lockTable.put (otherClient, PageLock.Mode.C);
          otherClient.getPropagationData ().addhi (
                          CommunicationEntry.T {self.file,
                                                self.logPageNo,
                                                self.pageAge,
                                                PageLock.Mode.C,
                                                page := NIL});
        END
      END
    END;

    IF Variant.TestServerScheduler THEN
      Journal.Add ("page' = (" & self.fmt () & ")");
    END;
  END KillClient;

  
PROCEDURE Shutdown	(         self		:T) =
  BEGIN
    PageCache.RemovePage (self.handle);
  END Shutdown;


PROCEDURE InUse		(         self		:T;
                                  client	:ServedClient.T)
			:BOOLEAN =
  BEGIN
    RETURN (self.lockTable.get (client) # PageLock.Mode.O);
  END InUse;


PROCEDURE Fmt		(         self		:T)
			:TEXT =
  BEGIN
    RETURN "lockState = " & PageLock.FmtMode (self.lockState) &
           ", pageAge = " & StdFmt.Int (self.pageAge) &
           ", handle = (" & self.handle.fmt () & ")" &
           ", lockTable = " & self.lockTable.fmt ();
  END Fmt;


BEGIN
END ScheduledServerPage.
