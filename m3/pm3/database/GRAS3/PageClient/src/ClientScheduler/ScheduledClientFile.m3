MODULE ScheduledClientFile
EXPORTS ScheduledClientFile, InternalScheduledClientFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.6  1997/04/24 12:12:34  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.5  1996/11/18 17:51:46  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.4  1996/10/29 15:01:47  rbnix
    	New parameter for page age added.

    Revision 1.3  1996/03/11 17:20:46  rbnix
    	Method close added to close down all pages.

    Revision 1.2  1996/02/23 15:00:33  rbnix
    	Bug fixed in index range of sequence.

    Revision 1.1  1996/02/09 16:46:55  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)
(*
 | --- ScheduledClientFile ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledClientFile AS Super;
IMPORT
  Pathname,
  PageHandle,
  PageFile,
  PageLock, Access,
  CommunicationEntry, CommunicationSeq, RemoteFile,
  ScheduledClientPage, InternalScheduledClientPage, ScheduledClientPageTbl,
  InternalBaseScheduledClientFile,
  BaseScheduledClientRessource,
  ErrorSupport, CallbackPort;

REVEAL
  T			= Internal BRANDED OBJECT
      pages		:ScheduledClientPageTbl.T;

    OVERRIDES
      open		:= Open;
      close		:= Close;

      getPage		:= GetPage;

      commitTransaction	:= CommitTransaction;
      abortTransaction	:= AbortTransaction;

      releaseCallback	:= ReleaseCallback;
      propagateCallback	:= PropagateCallback;

      dropData		:= DropData;
    END;


PROCEDURE Open			(   self		:T;
                                    ressource	:BaseScheduledClientRessource.T;
                                    baseName	:Pathname.T;
                                    mode        :Access.Mode;
                                    kind	:Access.Kind;
                                    new		:BOOLEAN)
				:Super.T
				RAISES {Access.Denied, PageFile.NoAccess,
                                        FatalError} =
  BEGIN
    self.pages := NEW (ScheduledClientPageTbl.Default).init ();

    TRY
      RETURN Super.T.open (self, ressource, baseName, mode, kind, new);
    EXCEPT
      Super.FatalError(info) =>
      RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientFile.Open",
                           "BaseScheduledClientFile.FatalError", info));
    END;
  END Open;


PROCEDURE Close			(         self		:T) RAISES {FatalError} =
  VAR
    pageNo			:CARDINAL;
    page			:ScheduledClientPage.T;
    i				:ScheduledClientPageTbl.Iterator;
  BEGIN
    TRY
      i := self.pages.iterate ();
      WHILE i.next (pageNo, page) DO
        page.close ()
      END;

      Super.T.close (self);
    EXCEPT
      Super.FatalError(info) =>
      RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientFile.Close",
                           "BaseScheduledClientFile.FatalError", info));
    | ScheduledClientPage.FatalError(info) => 
      RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientFile.Close",
                           "ScheduledClientPage.FatalError", info));
    END;    
  END Close;


PROCEDURE GetPage		(         self		:T;
                                          pageNo	:CARDINAL)
				:ScheduledClientPage.T =
  VAR
    page			:ScheduledClientPage.T;
  BEGIN
    IF NOT (self.pages.get (pageNo, page)) THEN
      page := NEW (ScheduledClientPage.T). init (self, pageNo);
      EVAL self.pages.put (pageNo, page);
    END;
    
    RETURN page;
  END GetPage;


PROCEDURE CommitTransaction	(         self		:T) RAISES {FatalError} =
  VAR
    pageNo			:CARDINAL;
    page			:ScheduledClientPage.T;
    i				:ScheduledClientPageTbl.Iterator;
  BEGIN
    TRY
      i := self.pages.iterate ();
      WHILE i.next (pageNo, page) DO
        page.commitTransaction ()
      END
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientFile.CommitTransaction",
                           "ScheduledClientPage.FatalError", info));
    END;
  END CommitTransaction;


PROCEDURE AbortTransaction	(         self		:T) RAISES {FatalError}=
  VAR
    pageNo			:CARDINAL;
    page			:ScheduledClientPage.T;
    i				:ScheduledClientPageTbl.Iterator;
  BEGIN
    TRY
      i := self.pages.iterate ();
      WHILE i.next (pageNo, page) DO
        page.abortTransaction ()
      END
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientFile.AbortTransaction",
                           "ScheduledClientPage.FatalError", info));
    END;
  END AbortTransaction;


PROCEDURE ReleaseCallback	(         self		:T;
                                          pageNo	:CARDINAL;
                                          pageAge	:CARDINAL;
	                                  lock		:PageLock.CallbackMode) 
				RAISES {Access.Locked, CallbackPort.FatalError} =
  VAR
    page			:ScheduledClientPage.T;
  BEGIN
    IF NOT (self.pages.get (pageNo, page)) THEN
      RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                        "ScheduledClientFile.ReleaseCallback",
                                        "Page not found"));
    END;
    page.releaseCallback (pageAge, lock);
  END ReleaseCallback;


PROCEDURE PropagateCallback	(         self		:T;
                                          entries	:CommunicationSeq.T)
  RAISES {CallbackPort.FatalError} =
  VAR
    entry			:CommunicationEntry.T;
    page			:ScheduledClientPage.T;
    remoteFile			:RemoteFile.T;
  BEGIN
    remoteFile := self.getOriginalMedia ().getFile ();
    
    FOR i := 0 TO entries.size ()-1 DO
      entry := entries.get (i);
      IF entry.file = remoteFile THEN
        IF NOT (self.pages.get (entry.pageNo, page)) THEN
          RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                            "ScheduledClientFile.PropagateCallback",
                                            "Page not found"));
        END;
        page.propagateCallback (entry.pageAge, entry.lock, entry.page);
      END;
    END;
  END PropagateCallback;


PROCEDURE DropData		(         self		:T;
                                          handle	:PageHandle.T)
  RAISES {FatalError} =
  VAR
    page			:ScheduledClientPage.T;
  BEGIN
    IF NOT (self.pages.get (handle.getPageNo (), page)) THEN
      RAISE FatalError(ErrorSupport.Create(
                           "ScheduledClientFile.DropData",
                           "Unknown page."));
    END;
    TRY
      page.dropData (handle);
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE FatalError(ErrorSupport.Propagate(
                           "ScheduledClientFile.DropData",
                           "BaseScheduledClientRessource.FatalError", info));
    END;
  END DropData;
  

BEGIN
END ScheduledClientFile.
