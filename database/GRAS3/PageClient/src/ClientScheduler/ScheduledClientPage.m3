MODULE ScheduledClientPage
EXPORTS ScheduledClientPage, InternalScheduledClientPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.14  1998/02/04 11:51:02  roland
    Cache locks on envelope level might not have data loaded. This is
    handled as a special case similar to S-locks now. (Maybe WriteAccess
    needs the same treatment.)

    Revision 1.13  1997/01/20 08:54:23  roland
    ClientLockTable uses an array now directly to gain more performance
    (getLastEntry is called much more often than any other procedure of
    the module, so this must be fast).
    Minor, non-critical changes in ScheduledClientPage.

    Revision 1.12  1996/11/18 17:51:48  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.11  1996/10/29 15:02:41  rbnix
    	New variable for page age added. This is used to provide some
    	more error checks.

    	Debug output reduced.

    	Bug fixed in ReleaseCallback: due to still pending getData
    	operations (PageLock.Mode.S, transferData = TRUE) releases on
    	non hold pages are revoked. The messages may cross each other
    	in a way that the server has granted the lock but the response
    	isn't still arrived at the client, meanwhile this release
    	callback is worked. At least the page is used at the client
    	and the server assumes the page as marked pending.

    Revision 1.10  1996/09/20 14:37:33  rbnix
    	Bug fixed in WriteAccess: cache locks can't be hold when
    	relocating a page to write on.

    Revision 1.9  1996/08/06 16:24:51  roland
    Merge of PAGESERVER and main branch.

    Revision 1.8.2.2  1996/06/27 11:04:33  rbnix
    	Bug fixed in CommitTopLevelTransaction,	AbortTopLevelTransaction:
    	Termination of transaction for dropped pages with S-locks
    	causes downgrading the lock and now also clearing the
    	envelope from resting C-locks.

    Revision 1.8.2.1  1996/04/29 13:34:48  roland
    Bugfix in CommitToplevelTransaction: When downgrading from X to C,
    pages should be marked as unchanged.

    Revision 1.8  1996/03/22 17:37:18  rbnix
    	Bug fixed in CommitNestedTransaction: wrong freeing of shadow
    	page number of S locked page removed.

    Revision 1.7  1996/03/22 16:58:07  rbnix
    	Bug fixed in WriteAccess: lock (maybe also page) will be
    	obtained from server only if still not hold on lower level. It
    	will be now inserted only if read from server otherwise it
    	will be copied.

    Revision 1.6  1996/03/22 16:27:52  rbnix
    	Bug fixed in WriteAccess: page is now relocated when read
    	accessed at current level only.

    Revision 1.5  1996/03/11 17:19:37  rbnix
    	Method close added to return cached locks.

    	Bug fixed: in WriteAccess on reusing a cached page to write on
    	the cache lock is now removed. (Solved phantom lock).

    Revision 1.4  1996/03/08 11:07:02  rbnix
    	In CommitTopLevelTransaction use oflocal copy of page data is
    	replaced by function getAll.

    	Bug fixed: in CommitTopLevelTransaction page is now placed in
    	envelope only if still load otherwise the lock is downgraded
    	and the handle is removed from cache.

    	Bug fixed: in AbortNestedTransaction and
    	AbortTopLevelTransaction obsolete page handle when downgrading
    	from S to O lock is removed from cache.

    	Bug fixed: in DropData wrong assumption of dropping only
    	handles on envelope levels adjusted. Now the handle is
    	searched from current to envelope level. When giving up the
    	page the handle is now removed from cache.

    Revision 1.3  1996/03/06 16:11:33  rbnix
    	Journal output improved.

    Revision 1.2  1996/03/06 14:06:33  rbnix
    	New method fmt added to get a formatted representation of the
    	lock table's value.

    	Output to journal for variant TestClientScheduler added.

    Revision 1.1  1996/02/09 16:46:58  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)
(*
 | --- ScheduledClientPage ------------------------------------------------
 This module implements the scheduler part responsible for lock management
 of one page. The provided functions realize the C4-Protocol CB-R/2Q and
 are implemented as specified in the Functional Access Model.

 Special care is taken in regard to following points:
  To cooperate well with the PageCache it is neccessary to remove not
  longer used handles explicit out of the cache.

  Using insert or copy operations of the PageCache may cause indirect
  recursive calls of DropData. Pay attention that still used locks are not
  released by this way. 

  The operations waitAccess and getData of the module scheduledFile
  (scheduledRessource) breaks atomic execution of the current thread
  (supervised via cache control). This behavior is required to avoid
  deadlocks between a client operation and server callbacks. It is
  therefore neccessary to recheck allready made assumptions or protect
  required data.

  Read access on dropped pages located at original media has to be catched
  explicit. Via original media the cache isn't able to swap them in but it
  is subject of scheduler to handle required data and lock transfer in one
  call.

  As allways, it is required for operations potentially beeing aborted by
  raised exceptions to leave a valid state.


 Reference:
  CB-R/2Q and Funktionales Zugriffsmodell in "Verteilung eines DBMS: eine
  Workstation/Server-Architektur am Beispiel des post-relationalen DBMS
  GRAS", Reiner B. Nix, diploma thesis, RWTH-Aachen, germany, '96.
 | ------------------------------------------------------------------------
 *)
IMPORT Fmt AS StdFmt;
IMPORT
  Variant, Journal,
  Page,
  PageHandle, PageCache,
  PageLock, Access, Transaction,
  ShadowMedia,
  ClientLockEntry, ClientLockTable,
  BaseScheduledClientFile, InternalBaseScheduledClientFile,
  ErrorSupport, CallbackPort;


REVEAL
  T			= Internal BRANDED OBJECT
      scheduledFile	:BaseScheduledClientFile.T;
      locks		:ClientLockTable.T;
      logPageNo		:CARDINAL;
      pageAge		:CARDINAL;

    METHODS
      fmt		() :TEXT
			:= Fmt;

    OVERRIDES
      init		:= Init;
      close		:= Close;

      readAccess	:= ReadAccess;
      writeAccess	:= WriteAccess;

      commitTransaction	:= CommitTransaction;
      abortTransaction	:= AbortTransaction;

      releaseCallback	:= ReleaseCallback;
      propagateCallback	:= PropagateCallback;

      dropData		:= DropData;
    END;


PROCEDURE Init		(         self		:T;
                                  scheduledFile	:BaseScheduledClientFile.T;
                                  pageNo	:CARDINAL) :T =
  BEGIN
    self.scheduledFile := scheduledFile;
    self.logPageNo := pageNo;
    self.locks := NEW (ClientLockTable.T).init ();
    self.pageAge := 0;

    RETURN self;
  END Init;


PROCEDURE Close		(         self		:T) RAISES {FatalError} =
  VAR
    currentLevel	:Transaction.Level;
    lastLevel		:Transaction.Level;
    lastEntry		:ClientLockEntry.T;
  BEGIN
    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.Close\n" &
                   "page  = (" & self.fmt () & ")");
    END;

    currentLevel := self.scheduledFile.getTransactionLevel ();
    lastEntry := self.locks.getLastEntry (currentLevel, lastLevel);
    CASE lastEntry.lock OF
    | PageLock.Mode.O =>
      (* nothing to do *)

    | PageLock.Mode.P, PageLock.Mode.C =>
      IF lastLevel # Transaction.EnvelopeLevel THEN
        RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.Close",
                                             "Lock mode error."));
      END;

      self.scheduledFile.putData (
               self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
      PageCache.RemovePage (lastEntry.handle);
      self.locks.putEntry (lastLevel, ClientLockEntry.T {PageLock.Mode.O, NIL});

    | PageLock.Mode.S, PageLock.Mode.X =>
      IF lastLevel # Transaction.EnvelopeLevel THEN
        RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.Close",
                                             "Lock mode error."));
      END;
    END
  END Close;


PROCEDURE ReadAccess	(         self		:T) :PageHandle.T
			RAISES {Access.Locked, FatalError} =
  VAR
    currentLevel	:Transaction.Level;
    lastLevel		:Transaction.Level;
    lastEntry		:ClientLockEntry.T;
    page		:Page.T;
    changes		:BOOLEAN := FALSE;
  BEGIN
    currentLevel := self.scheduledFile.getTransactionLevel ();
    IF NOT (Transaction.EnvelopeLevel < currentLevel) THEN
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.ReadAccess",
                                           "Not in transaction."));
    END;

    (* wait for access unless pending lock is released *)
    lastEntry := self.locks.getLastEntry (currentLevel, lastLevel);

    IF Variant.TestClientScheduler AND
       ((currentLevel # lastLevel) OR 
        ((lastEntry.lock#PageLock.Mode.S) AND (lastEntry.lock#PageLock.Mode.X)))THEN
      Journal.Add ("ScheduledClientPage.ReadAccess\n" &
		   "page  = (" & self.fmt () & ")");
    END;


    WHILE lastEntry.lock = PageLock.Mode.P DO		
      IF NOT (lastLevel = Transaction.EnvelopeLevel) THEN
        RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.ReadAccess",
                                           "Protocol error."));
      END;
      TRY
        self.scheduledFile.waitAccess (self.logPageNo, PageLock.Mode.C);
      EXCEPT
        BaseScheduledClientFile.FatalError(info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                               "ScheduledClientPage.ReadAccess",
                               "BaseScheduledClientFile.FatalError", info));
      END;
      lastEntry := self.locks.getLastEntry (currentLevel, lastLevel);
    END;

    CASE lastEntry.lock OF
    | PageLock.Mode.P =>
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.ReadAccess",
                                           "Lock mode error."));
      

    | PageLock.Mode.X =>
      IF currentLevel = lastLevel THEN
        (*
          nothing to do,
          read locks are compatible with write locks
        *)

      ELSE
        (*
          because of existance of compatible write lock on lower level
          the lock isn't demanded from server and data is adopted from
          lower level
        *)
        lastEntry.lock := PageLock.Mode.S;
        self.locks.putEntry (currentLevel, lastEntry);
        changes := TRUE;
      END;

    | PageLock.Mode.C =>
      (*
        local and autonomous upgrading of cached lock
      *)
      IF NOT (lastLevel = Transaction.EnvelopeLevel) THEN
        RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.ReadAccess",
                                             "Lock mode error."));
      END;
      
      IF (NOT lastEntry.handle.isLoad ()) AND
         (lastEntry.handle.getMedia () =
          self.scheduledFile.getOriginalMedia ()) THEN
        TRY
          page := self.scheduledFile.getData (
                           self.logPageNo, self.pageAge,
                           PageLock.Mode.C, transferData := TRUE);
        EXCEPT
          BaseScheduledClientFile.FatalError(info) =>
            RAISE FatalError(ErrorSupport.Propagate(
                                 "ScheduledClientPage.ReadAccess",
                                 "BaseScheduledClientFile.FatalError", info));
        END;

        PageCache.ReInsertPage (lastEntry.handle, page.getAll ());
        changes := TRUE;
      END;
      lastEntry.lock := PageLock.Mode.S;
      self.locks.putEntry (currentLevel, lastEntry);
      changes := TRUE;
      
    | PageLock.Mode.S =>
      IF (NOT lastEntry.handle.isLoad ()) AND
         (lastEntry.handle.getMedia () =
          self.scheduledFile.getOriginalMedia ()) THEN
        TRY
          page := self.scheduledFile.getData (
                           self.logPageNo, self.pageAge,
                           PageLock.Mode.C, transferData := TRUE);
        EXCEPT
          BaseScheduledClientFile.FatalError(info) =>
            RAISE FatalError(ErrorSupport.Propagate(
                                 "ScheduledClientPage.ReadAccess",
                                 "BaseScheduledClientFile.FatalError", info));
        END;

        PageCache.ReInsertPage (lastEntry.handle, page.getAll ());
        changes := TRUE;
      END;

      IF lastLevel < currentLevel THEN
        self.locks.putEntry (currentLevel, lastEntry);
        changes := TRUE;
      END;

    | PageLock.Mode.O =>
      TRY
        page := self.scheduledFile.getData (
                         self.logPageNo, self.pageAge,
                         PageLock.Mode.C, transferData := TRUE);
      EXCEPT
        BaseScheduledClientFile.FatalError(info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                             "ScheduledClientPage.ReadAccess",
                             "BaseScheduledClientFile.FatalError", info));
      END;
      lastEntry.handle := PageCache.InsertPage (
                              self.logPageNo,
                              self.scheduledFile.getOriginalMedia (),
                              page.getAll ());
      lastEntry.lock := PageLock.Mode.S;
      self.locks.putEntry (currentLevel, lastEntry);
      changes := TRUE;
    END;

    IF Variant.TestClientScheduler AND changes THEN
      Journal.Add ("ScheduledClientPage.ReadAccess  " &
		   "page' = (" & self.fmt () & ")");
    END;

    RETURN lastEntry.handle;
  END ReadAccess;

  
PROCEDURE WriteAccess	(         self		:T) :PageHandle.T
			RAISES {Access.Locked, FatalError} =
  VAR
    currentLevel	:Transaction.Level;
    lastLevel		:Transaction.Level;
    lastEntry		:ClientLockEntry.T;
    prevLevel		:Transaction.Level;
    prevEntry		:ClientLockEntry.T;
    page		:Page.T;
    shadowMedia		:ShadowMedia.T;
    changes		:BOOLEAN := FALSE;
  BEGIN
    currentLevel := self.scheduledFile.getTransactionLevel ();
    IF currentLevel <= Transaction.EnvelopeLevel THEN
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.WriteAccess",
                                           "Not in transaction."));
    END;
    IF NOT (self.scheduledFile.getAccessMode () IN Access.WriteModes) THEN
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.WriteAccess",
                              "Access rights violation: write access denied."));
    END;
    shadowMedia := self.scheduledFile.getShadowMedia ();

    lastEntry := self.locks.getLastEntry (currentLevel, lastLevel);

    IF Variant.TestClientScheduler AND
       ((currentLevel # lastLevel) OR (lastEntry.lock # PageLock.Mode.X)) THEN
      Journal.Add ("ScheduledClientPage.WriteAccess\n" &
		   "page  = (" & self.fmt () & ")");
    END;

    WHILE lastEntry.lock = PageLock.Mode.P DO
      IF lastLevel # Transaction.EnvelopeLevel THEN
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.WriteAccess",
                                           "Protocol error."));
      END;        
      TRY
        self.scheduledFile.waitAccess (self.logPageNo, PageLock.Mode.X);
      EXCEPT
        BaseScheduledClientFile.FatalError(info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                               "ScheduledClientPage.WriteAccess",
                               "BaseScheduledClientFile.FatalError", info));
      END;
      lastEntry := self.locks.getLastEntry (currentLevel, lastLevel);
    END;

    CASE lastEntry.lock OF
    | PageLock.Mode.P =>
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.WriteAccess",
                                           "Lock mode error."));

    | PageLock.Mode.X =>
      IF lastLevel < currentLevel THEN
        lastEntry.handle := PageCache.CopyPage (
                                lastEntry.handle,
                                shadowMedia.obtainPageNo (),
                                shadowMedia);
        self.locks.putEntry (currentLevel, lastEntry);
        changes := TRUE;
      END;

    | PageLock.Mode.C =>
      IF NOT (lastLevel = Transaction.EnvelopeLevel) THEN
        RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.WriteAccess",
                                             "Lock mode error."));
      END;        
      TRY
        lastEntry.lock := PageLock.Mode.S;
        self.locks.putEntry (currentLevel, lastEntry);
        EVAL self.scheduledFile.getData (
                      self.logPageNo, self.pageAge,
                      PageLock.Mode.X, transferData := FALSE);
      EXCEPT
      | Access.Locked =>
        self.locks.putEntry (currentLevel,
                             ClientLockEntry.T {PageLock.Mode.O, NIL});
        RAISE Access.Locked;
      | BaseScheduledClientFile.FatalError(info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                             "ScheduledClientPage.WriteAccess",
                             "BaseScheduledClientFile.FatalError", info));
      END;
      lastEntry.handle.setMedia (shadowMedia);
      lastEntry.handle.setPageNo (shadowMedia.obtainPageNo ());
      lastEntry.lock := PageLock.Mode.X;
      self.locks.putEntry (currentLevel, lastEntry);
      self.locks.putEntry (lastLevel, ClientLockEntry.T {PageLock.Mode.O, NIL});
      changes := TRUE;

    | PageLock.Mode.O =>
      TRY
        page := self.scheduledFile.getData (
                         self.logPageNo, self.pageAge,
                         PageLock.Mode.X, transferData := TRUE);
      EXCEPT
      | BaseScheduledClientFile.FatalError(info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                             "ScheduledClientPage.WriteAccess",
                             "BaseScheduledClientFile.FatalError", info));
      END;
      lastEntry.lock := PageLock.Mode.X;
      lastEntry.handle := PageCache.InsertPage (
                              shadowMedia.obtainPageNo (),
                              shadowMedia,
                              page.getAll ());
      self.locks.putEntry (currentLevel, lastEntry);
      changes := TRUE;

    | PageLock.Mode.S =>
      IF NOT (self.locks.exists (currentLevel-1, PageLock.Mode.X)) THEN
        (* no X-lock hold, maybe page must be loaded too *)
        TRY
          page := self.scheduledFile.getData (
                           self.logPageNo, self.pageAge,
                           PageLock.Mode.X,
                           transferData := NOT (lastEntry.handle.isLoad ()));
        EXCEPT
        | BaseScheduledClientFile.FatalError(info) =>
          RAISE FatalError(ErrorSupport.Propagate(
                               "ScheduledClientPage.WriteAccess",
                               "BaseScheduledClientFile.FatalError", info));
        END;
      ELSE
        (* X-lock still hold *)
        page := NIL;
      END;
      lastEntry.lock := PageLock.Mode.X;
      prevEntry := self.locks.getLastEntry (lastLevel-1, prevLevel);
      IF ((lastLevel = currentLevel) AND (prevEntry.lock = PageLock.Mode.O)) OR
         (prevEntry.lock = PageLock.Mode.C) THEN
        (* page is used only on current level and will be relocated *)
        lastEntry.handle.setMedia (shadowMedia);
        lastEntry.handle.setPageNo (shadowMedia.obtainPageNo ());
        IF NOT (lastEntry.handle.isLoad ()) THEN
          PageCache.ReInsertPage (lastEntry.handle, page.getAll ());
        END;
        IF prevEntry.lock = PageLock.Mode.C THEN
          self.locks.putEntry (Transaction.EnvelopeLevel,
                               ClientLockEntry.T {PageLock.Mode.O, NIL});
        END;
      ELSIF page # NIL THEN
        (* insert first copy this will be modified *)
        lastEntry.handle := PageCache.InsertPage (
                                shadowMedia.obtainPageNo (),
                                shadowMedia,
                                page.getAll ());
      ELSE
        (* create a new independent page copy for this level *)
        lastEntry.handle := PageCache.CopyPage (
                                lastEntry.handle,
                                shadowMedia.obtainPageNo (),
                                shadowMedia);
      END;
      self.locks.putEntry (currentLevel, lastEntry);
      changes := TRUE;
    END;
    
    IF Variant.TestClientScheduler AND changes THEN
      Journal.Add ("ScheduledClientPage.WriteAccess  " &
		   "page' = (" & self.fmt () & ")");
    END;

    RETURN lastEntry.handle;
  END WriteAccess;

  
PROCEDURE CommitTransaction (     self		:T) RAISES {FatalError}=
  VAR
    currentLevel	:Transaction.Level;
    currentEntry	:ClientLockEntry.T;
    shadowMedia		:ShadowMedia.T;
    changes		:BOOLEAN := FALSE;

  PROCEDURE CommitNestedTransaction () RAISES {FatalError} =
    VAR
      prevEntry         :ClientLockEntry.T;
      prev2Level        :Transaction.Level;
      prev2Entry        :ClientLockEntry.T;
    BEGIN
      CASE currentEntry.lock OF
      | PageLock.Mode.P, PageLock.Mode.C =>
        RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.CommitNestedTransaction",
                             "Protocol error."));

      | PageLock.Mode.O =>
        (* nothing to do, page unused *)

      | PageLock.Mode.S =>
        prevEntry := self.locks.getEntry (currentLevel-1);
        CASE prevEntry.lock OF
        | PageLock.Mode.P, PageLock.Mode.C =>
          RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.CommitNestedTransaction",
                             "Protocol error."));

        | PageLock.Mode.O =>
          self.locks.putEntry (currentLevel-1, currentEntry);

        | PageLock.Mode.S, PageLock.Mode.X =>
          (* nothing to do, allready locked *)
        END;
        self.locks.putEntry (currentLevel, ClientLockEntry.T{PageLock.Mode.O, NIL});
        changes := TRUE;

      | PageLock.Mode.X =>
        prevEntry := self.locks.getEntry (currentLevel-1);
        CASE prevEntry.lock OF
        | PageLock.Mode.P, PageLock.Mode.C =>
          RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.CommitNestedTransaction",
                             "Protocol error."));

        | PageLock.Mode.O =>
          self.locks.putEntry (currentLevel-1, currentEntry);

        | PageLock.Mode.X =>
          shadowMedia := prevEntry.handle.getMedia ();
          shadowMedia.freePageNo (prevEntry.handle.getPageNo ());
          PageCache.RemovePage (prevEntry.handle);
          
          self.locks.putEntry (currentLevel-1, currentEntry);

        | PageLock.Mode.S =>
          prev2Entry := self.locks.getLastEntry (currentLevel-2, prev2Level);
          IF prev2Entry.lock = PageLock.Mode.O THEN
            PageCache.RemovePage (prevEntry.handle);
          END;

          self.locks.putEntry (currentLevel-1, currentEntry);
        END;
        self.locks.putEntry (currentLevel, ClientLockEntry.T{PageLock.Mode.O, NIL});
        changes := TRUE;
      END;
    END CommitNestedTransaction;

  PROCEDURE CommitTopLevelTransaction () RAISES {FatalError}=
    VAR
      page              :Page.T;
    BEGIN
      CASE currentEntry.lock OF
      | PageLock.Mode.P, PageLock.Mode.C =>
        RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.CommitTopLevelTransaction",
                             "Protocol error."));

      | PageLock.Mode.O =>
        (* nothing to do, page unused *)

      | PageLock.Mode.S =>
        IF currentEntry.handle.isLoad () THEN
          (* place page as cached in envelope *)
          currentEntry.lock := PageLock.Mode.C;
          self.locks.putEntry (Transaction.EnvelopeLevel, currentEntry);
        ELSE
          (* downgrade lock and clear envelope *)
          self.scheduledFile.putData (
                   self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
          PageCache.RemovePage (currentEntry.handle);
          self.locks.putEntry (Transaction.EnvelopeLevel,
                               ClientLockEntry.T {PageLock.Mode.O, NIL});
        END;
        changes := TRUE;

      | PageLock.Mode.X =>
        (* set new page age *)
        self.pageAge := self.scheduledFile.getTransactionNumber ();
        
        (* transfer changes back to server *)
        page := NEW (Page.T);
        page.putData (currentEntry.handle.getAll ());
        self.scheduledFile.putData (self.logPageNo,
                                    self.pageAge,
                                    PageLock.Mode.C,
                                    page);

        (* relocate page to original media *)
        shadowMedia := currentEntry.handle.getMedia ();
        shadowMedia.freePageNo (currentEntry.handle.getPageNo ());
        currentEntry.handle.setPageNo (self.logPageNo);
        currentEntry.handle.setMedia (self.scheduledFile.getOriginalMedia ());
        currentEntry.handle.unmarkChanges ();

        (* place page as cached in envelope *)
        currentEntry.lock := PageLock.Mode.C;
        self.locks.putEntry (Transaction.EnvelopeLevel, currentEntry);
        changes := TRUE;
      END;
      self.locks.putEntry (currentLevel, ClientLockEntry.T {PageLock.Mode.O, NIL});
    END CommitTopLevelTransaction;

  (* CommitTransaction *)
  BEGIN
    (*
    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.CommitTransaction\n" &
		   "page  = (" & self.fmt () & ")");
    END;
    *)

    currentLevel := self.scheduledFile.getTransactionLevel ();
    currentEntry := self.locks.getEntry (currentLevel);

    IF Transaction.TopLevel < currentLevel THEN
      CommitNestedTransaction ();
    ELSE
      IF Transaction.TopLevel # currentLevel THEN
        RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.CommitTransaction",
                             "Protocol error."));
      END;        
      CommitTopLevelTransaction ();
    END;

    IF Variant.TestClientScheduler AND changes THEN
      Journal.Add ("ScheduledClientPage.CommitTransaction  " &
		   "page' = (" & self.fmt () & ")");
    END;
  END CommitTransaction;

  
PROCEDURE AbortTransaction (      self		:T) RAISES {FatalError} =
  VAR
    currentLevel	:Transaction.Level;
    currentEntry	:ClientLockEntry.T;
    shadowMedia		:ShadowMedia.T;
    changes		:BOOLEAN := FALSE;

  PROCEDURE AbortNestedTransaction () RAISES {FatalError} =
    VAR
      prevLevel		:Transaction.Level;
      prevEntry		:ClientLockEntry.T;
    BEGIN
      CASE currentEntry.lock OF
      | PageLock.Mode.P, PageLock.Mode.C =>
        RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.AbortNestedTransaction",
                             "Protocol error."));

      | PageLock.Mode.O =>
        (* nothing to do *)

      | PageLock.Mode.S =>
        (* place page in envelope if not used elsewhere *)
        prevEntry := self.locks.getLastEntry (currentLevel-1, prevLevel);
        IF prevEntry.lock = PageLock.Mode.O THEN
          IF currentEntry.handle.isLoad () THEN
            currentEntry.lock := PageLock.Mode.C;
            self.locks.putEntry (Transaction.EnvelopeLevel, currentEntry);
          ELSE
            self.scheduledFile.putData (
                     self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
            PageCache.RemovePage (currentEntry.handle);
          END;
        END;
        self.locks.putEntry (currentLevel, ClientLockEntry.T{PageLock.Mode.O, NIL});
        changes := TRUE;

      | PageLock.Mode.X =>
        shadowMedia := currentEntry.handle.getMedia ();
        shadowMedia.freePageNo (currentEntry.handle.getPageNo ());
        PageCache.RemovePage (currentEntry.handle);
        IF NOT self.locks.exists (currentLevel-1, PageLock.Mode.X) THEN
          prevEntry := self.locks.getLastEntry (currentLevel-1, prevLevel);
          CASE prevEntry.lock OF
          | PageLock.Mode.P, PageLock.Mode.X =>
            RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.AbortNestedTransaction",
                             "Protocol error."));

          | PageLock.Mode.C, PageLock.Mode.S =>
            self.scheduledFile.putData (
                     self.logPageNo, self.pageAge, PageLock.Mode.C, page := NIL);

          | PageLock.Mode.O =>
            self.scheduledFile.putData (
                     self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
          END;
        END;
        self.locks.putEntry (currentLevel, ClientLockEntry.T{PageLock.Mode.O, NIL});
        changes := TRUE;
      END
    END AbortNestedTransaction;

  PROCEDURE AbortTopLevelTransaction () RAISES {FatalError} =
    BEGIN
      CASE currentEntry.lock OF
      | PageLock.Mode.P, PageLock.Mode.C =>
        RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.AbortTopLevelTransaction",
                             "Protocol error."));

      | PageLock.Mode.O =>
        (* nothing to do *)

      | PageLock.Mode.S =>
        IF currentEntry.handle.isLoad () THEN
          (* place page as cached in envelope *)
          currentEntry.lock := PageLock.Mode.C;
          self.locks.putEntry (Transaction.EnvelopeLevel, currentEntry);
        ELSE
          (* downgrade lock and clear envelope *)
          self.scheduledFile.putData (
                   self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
          PageCache.RemovePage (currentEntry.handle);
          self.locks.putEntry (Transaction.EnvelopeLevel,
                               ClientLockEntry.T {PageLock.Mode.O, NIL});
        END;
        self.locks.putEntry (currentLevel, ClientLockEntry.T{PageLock.Mode.O, NIL});
        changes := TRUE;

      | PageLock.Mode.X =>
        shadowMedia := currentEntry.handle.getMedia ();
        shadowMedia.freePageNo (currentEntry.handle.getPageNo ());
        PageCache.RemovePage (currentEntry.handle);
        self.scheduledFile.putData (
                 self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
        self.locks.putEntry (currentLevel, ClientLockEntry.T{PageLock.Mode.O, NIL});
        changes := TRUE;
      END
    END AbortTopLevelTransaction;
    
  (* AbortTransaction *)
  BEGIN
    (*
    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.AbortTransaction\n" &
		   "page  = (" & self.fmt () & ")");
    END;
    *)

    currentLevel := self.scheduledFile.getTransactionLevel ();
    currentEntry := self.locks.getEntry (currentLevel);

    IF Transaction.TopLevel < currentLevel THEN
      AbortNestedTransaction ();
    ELSE
      IF NOT (Transaction.TopLevel = currentLevel) THEN
        RAISE FatalError(ErrorSupport.Create(
                             "ScheduledClientPage.AbortTransaction",
                             "Protocol error."));
      END;
      AbortTopLevelTransaction ();
    END;

    IF Variant.TestClientScheduler AND changes THEN
      Journal.Add ("ScheduledClientPage.AbortTransaction  " &
		   "page' = (" & self.fmt () & ")");
    END;
  END AbortTransaction;

  
PROCEDURE ReleaseCallback (       self		:T;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.CallbackMode)
			  RAISES {Access.Locked, CallbackPort.FatalError} =
  VAR
    currentLevel	:Transaction.Level;
    lastLevel		:Transaction.Level;
    lastEntry		:ClientLockEntry.T;
  BEGIN
    (*
    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.ReleaseCallback\n" &
		   "page  = (" & self.fmt () & ")");
    END;
    *)

    currentLevel := self.scheduledFile.getTransactionLevel ();

    IF lock # PageLock.Mode.P THEN
      RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                        "ScheduledClientPage.ReleaseCallback",
                                        "Protocol error: Wrong lock mode."));
    END;
    lastEntry := self.locks.getLastEntry (currentLevel, lastLevel);
    CASE lastEntry.lock OF
    | PageLock.Mode.P, PageLock.Mode.X =>
      RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                        "ScheduledClientPage.ReleaseCallback",
                                        "Protocol error: Wrong lock mode."));

    | PageLock.Mode.O =>
      (* cache locks must reside in the envelope *)
      IF lastLevel # Transaction.EnvelopeLevel THEN
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                          "ScheduledClientPage.ReleaseCallback",
                                          "Protocol error: Transaction level."));
      END;
      
      IF Variant.TestClientScheduler THEN
        Journal.Add ("ScheduledClientPage.ReleaseCallback REVOKED ");
      END;

      (*
        nothing to release but request will be revoked
        maybe there is a pending response of a getData
      *)
      RAISE Access.Locked;

    | PageLock.Mode.C =>
      (* cache locks must reside in the envelope *)
      IF lastLevel # Transaction.EnvelopeLevel THEN
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                          "ScheduledClientPage.ReleaseCallback",
                                          "Protocol error: Transaction level."));
      END;

      (* equal page ages at client and server expected *)
      IF pageAge # self.pageAge THEN
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                          "ScheduledClientPage.ReleaseCallback",
                                          "Protocol error: Ages differ."));
      END;
      
      lastEntry.lock := PageLock.Mode.P;
      self.locks.putEntry (lastLevel, lastEntry);

    | PageLock.Mode.S =>
      (* equal page ages at client and server expected *)
      IF pageAge # self.pageAge THEN
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                          "ScheduledClientPage.ReleaseCallback",
                                          "Protocol error: Ages differ."));
      END;        
      
      IF Variant.TestClientScheduler THEN
        Journal.Add ("ScheduledClientPage.ReleaseCallback REVOKED ");
      END;

      (*
        release not possible
        because lock is locally granted
      *)
      RAISE Access.Locked;
    END;

    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.ReleaseCallback  " &
		   "page' = (" & self.fmt () & ")");
    END;
  END ReleaseCallback;

  
PROCEDURE PropagateCallback (     self		:T;
                                  pageAge	:CARDINAL;
                                  lock          :PageLock.CallbackMode;
                                  page          :Page.T)
  RAISES {CallbackPort.FatalError} =
  VAR
    lastLevel		:Transaction.Level;
    envelopeEntry	:ClientLockEntry.T;
    changes		:BOOLEAN := FALSE;
  BEGIN
    (*
    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.PropagateCallback\n" &
		   "page  = (" & self.fmt () & ")");
    END;
    *)

    envelopeEntry := self.locks.getLastEntry (
                              self.scheduledFile.getTransactionLevel(), lastLevel);
    (* page data must'nt be used at all *)
    IF NOT (lastLevel = Transaction.EnvelopeLevel) THEN
      RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                        "ScheduledClientPage.PropagateCallback",
                                        "Protocol error: Page in use."));
    END;

    CASE envelopeEntry.lock OF
    | PageLock.Mode.S, PageLock.Mode.C, PageLock.Mode.X =>
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                 "ScheduledClientPage.PropagateCallback",
                                 "Protocol error: Wrong lock mode (S|C|X -> ?)."));

    | PageLock.Mode.O =>
      CASE lock OF
      | PageLock.Mode.P =>
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                    "ScheduledClientPage.PropagateCallback",
                                    "Protocol error: Wrong lock mode (O -> P)."));

      | PageLock.Mode.C =>
        (*
          Assumed to be correct because of delayed page unlock.
          For security reason the page is unlocked again (compatible operation).
        *)
        self.scheduledFile.putData (
                 self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
        changes := TRUE;

      | PageLock.Mode.O => 
        (*
          Assumed to be correct because of delayed page unlock
          but there is nothing to do.
        *)
      END;

    | PageLock.Mode.P =>
      CASE lock OF
      | PageLock.Mode.P =>
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                "ScheduledClientPage.PropagateCallback",
                                "Protocol error: Wrong lock mode (P->P)."));



      | PageLock.Mode.C =>
        IF NOT (envelopeEntry.handle.isLoad ()) THEN
        RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                        "ScheduledClientPage.PropagateCallback",
                                        "Protocol error: Not in cache."));

        END;

        (* reset lock *)
        envelopeEntry.lock := PageLock.Mode.C;
        self.locks.putEntry (Transaction.EnvelopeLevel, envelopeEntry);

        IF page # NIL THEN
          (* store propagated data *)
          envelopeEntry.handle.putData (page.getAll ());
          envelopeEntry.handle.unmarkChanges ();
          self.pageAge := pageAge;
        ELSE
          (* equal page ages at client and server expected *)
          IF NOT (pageAge = self.pageAge) THEN
            RAISE CallbackPort.FatalError(ErrorSupport.Create(
                                        "ScheduledClientPage.PropagateCallback",
                                        "Protocol error: Ages differ."));

          END;
        END;
        changes := TRUE;

      | PageLock.Mode.O =>
        PageCache.RemovePage (envelopeEntry.handle);

        envelopeEntry.lock := PageLock.Mode.O;
        envelopeEntry.handle := NIL;
        self.locks.putEntry (Transaction.EnvelopeLevel, envelopeEntry);
        changes := TRUE;
      END
    END;

    IF Variant.TestClientScheduler AND changes THEN
      Journal.Add ("ScheduledClientPage.PropagateCallback  " &
		   "page' = (" & self.fmt () & ")");
    END;
  END PropagateCallback;

  
PROCEDURE DropData	(         self		:T;
                                  handle	:PageHandle.T)
  RAISES {FatalError} =
  VAR
    currentLevel	:Transaction.Level;
    thisLevel		:Transaction.Level;
    thisEntry		:ClientLockEntry.T;
    changes		:BOOLEAN := FALSE;
  BEGIN
    (*
    IF Variant.TestClientScheduler THEN
      Journal.Add ("ScheduledClientPage.DropData\n" &
		   "page  = (" & self.fmt () & ")");
    END;
    *)

    currentLevel := self.scheduledFile.getTransactionLevel ();

    (* search entry to given handle *)
    thisLevel := currentLevel;
    thisEntry := self.locks.getEntry (thisLevel);
    WHILE thisEntry.handle # handle DO
      DEC (thisLevel);
      thisEntry := self.locks.getEntry (thisLevel);
    END;

    (* check if lock can be dropped *)
    CASE thisEntry.lock OF
    | PageLock.Mode.X, PageLock.Mode.O =>
      RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.DropData",
                                           "Protocol error."));

    | PageLock.Mode.S =>
      (* nothing to do, lock can't be dropped *)

    | PageLock.Mode.C, PageLock.Mode.P =>
      IF NOT (thisLevel = Transaction.EnvelopeLevel) THEN
        RAISE FatalError(ErrorSupport.Create("ScheduledClientPage.DropData",
                                             "Protocol error."));
      END;
      self.scheduledFile.putData (
               self.logPageNo, self.pageAge, PageLock.Mode.O, page := NIL);
      self.locks.putEntry (Transaction.EnvelopeLevel,
                           ClientLockEntry.T {PageLock.Mode.O, NIL});
      PageCache.RemovePage (thisEntry.handle);
      changes := TRUE;
    END;

    IF Variant.TestClientScheduler AND changes THEN
      Journal.Add ("ScheduledClientPage.DropData  " &
		   "page' = (" & self.fmt () & ")");
    END;
  END DropData;

  
PROCEDURE Fmt		(         self		:T) :TEXT =
  BEGIN
    RETURN "pageNo = " & StdFmt.Int (self.logPageNo) &
           ", pageAge = " & StdFmt.Int (self.pageAge) &
           ", locks = " & self.locks.fmt ();
  END Fmt;


BEGIN
END ScheduledClientPage.
