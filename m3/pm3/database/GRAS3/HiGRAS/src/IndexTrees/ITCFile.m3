MODULE ITCFile EXPORTS ITCFile, InternalITCFile;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.10  1998/03/17 14:14:06  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.9  1998/01/21 12:34:53  roland
    New method baseName to determine filename.

    Revision 1.8  1997/11/13 14:14:07  roland
    New parameter composeName for VirtualLocalFile.Open determines whether
    fileName should be treated as absolute path or as relative to its
    resource path.

    Revision 1.7  1997/10/31 14:19:30  roland
    Adapted to new RuleEngine.

    Revision 1.6  1997/06/27 07:02:36  roland
    Create path for bufferFile before opening it.

    Revision 1.5  1997/06/13 13:57:38  roland
    Bugfix: changes of GrasGraph/ITCFile.m3 were not propagated completely

    Revision 1.4  1997/06/13 12:00:17  rbnix
        Adapted to simplified file handling with methods
        getPath and makeFileName.

    Revision 1.3  1997/06/10 12:53:11  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.2  1997/04/24 14:31:29  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:24:59  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.11  1997/03/25 17:01:12  roland
    ITCFile.Open: MUTEX must be allocated before using it!

    Revision 1.10  1997/03/25 13:28:01  rbnix
        Trigger handling for preserving cache consistency
        rewritten. Now caching data is enhanced to inter and intra
        transaction caching.

        Bug fixed: when using triggers accessing internal data this
        data must be protected for multi user access. This is done via
        a mutex guarding the access functions itself.

    Revision 1.9  1996/11/20 12:22:03  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.8  1996/09/09 11:48:30  rbnix
        Coherence between index tree cache and page data reconstructed
        using resource events.

    Revision 1.7  1996/08/06 16:25:30  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.7  1996/07/30 07:55:46  rbnix
        Module FS replaced by PageFileSystem.

    Revision 1.6.2.6  1996/07/25 08:53:31  rbnix
        Methods insertCacheEntry, removeCacheEntry, findCacheEntry,
        changeCacheEntry moved from ITCFile into new interface
        InternalITCFile.

        BEWARE: usage of this methods is *** CURRENTLY NOT *** correct
        in multiuser mode because of ignorance of changes in database
        from other clients.

    Revision 1.6.2.5  1996/07/25 08:00:30  rbnix
        Obsolete functions EndTransactions, Delete and CheckOut
        removed.

    Revision 1.6.2.4  1996/07/24 09:18:42  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.6.2.3  1996/06/13 13:26:46  rbnix
        Bug fixed: file for splitbuffer is now deleted when closing
        file.

        File name for splitbuffer normalized to share the same
        directory for several clients.

    Revision 1.6.2.2  1996/06/12 10:36:59  roland
    Path of splitbuffer now within client resource.

    Revision 1.6.2.1  1996/04/29 13:38:05  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.6  1994/03/30  17:28:15  pk
# Adaptions for new Files subsystem.
#
# Revision 1.5  1993/11/03  19:25:09  pk
# Some identifiers renamed.
#
# Revision 1.4  1993/11/03  18:25:29  pk
# New naming convention Base and Super introduced.
#
# Revision 1.3  1993/10/26  19:48:18  pk
# Naming of methods and procedures updated.
#
# Revision 1.2  1993/09/08  12:09:23  pk
# Several DIV/MOD operations on keys replaced by Word operations.
#
# Revision 1.1  1993/08/17  12:51:08  pk
# Abstract data type for index tree cache files.
#
*)
(***************************************************************************)
(*
 | --- ITCFile ------------------------------------------------------------
 To preserve coherence between index cache and relevant pages the index
 cache is served only as intra-transaction cache. It will be cleared at
 begin of every transaction (regardless the transaction level).

 The cache in this module caches in two cases
  a) Only intra transaction caching: at the beginning of the transaction no
     entries are hold in the cache. All new entries can be add to the cache
     and hold without regarding some events because the data is protected
     by transaction semantics.

  b) On inter transaction caching, the case has some entries at the
     beginning of a transaction. These entries can be used longer iff no
     event arises showing possible changes is global data. If an event is
     raised the whole cache must be cleared!

 The cache is protected by its own mutex. This should be hold only for
 short time. (Note: the resource event can be raised in another thread even
 if a command in the main thread will be executed. So the mutex can't be
 hold in the main thread over a long time.)
 | ------------------------------------------------------------------------
 *)
IMPORT VirtualRemoteFile, VirtualLocalFile, VirtualFile;
IMPORT Word, Pathname, PageFile, PageFileSystem, Access, VirtualPage,
       VirtualResource, Transaction, DataPage, SystemPage, ErrorSupport;
IMPORT Trigger, Action, ContextSet, VirtualPageEventPattern,
       VirtualPageEvent, RuleEngine, EventType;

CONST
  Size    = 32;                  (* Size = 2^Ld2Size must hold *)
  Ld2Size = 5;

  splitBuffer1No = 0;
  splitBuffer2No = 1;

TYPE
  Index = [0 .. Size - 1];
  IndexSet = SET OF Index;

  Entry = RECORD
            relevantKey1: CARDINAL;  (* relevant part of the keys *)
            relevantKey2: CARDINAL;  (* (the first depthN bits) *)
            location    : CARDINAL;  (* no.  of the page *)
            depth1      : CARDINAL;  (* local depths in trees *)
            depth2      : CARDINAL;
          END;
  EntryArray = ARRAY Index OF Entry;

  IndexTreeCache = RECORD
                     bitOffset := 0;
                     (* the hash key is taken from the Ld2Size bits of key1
                        starting at bitOffset *)
                     definedEntries               := IndexSet{};
                     entries       : EntryArray;
                   END;
  CacheArray = ARRAY [1 .. SystemPage.MaxNoOfIndexTrees] OF IndexTreeCache;


REVEAL
  T =
    Internal BRANDED OBJECT
      vfile : VirtualFile.T;
      caches: CacheArray;
      beginTrigger, commitTrigger, remoteTrigger, abortTrigger: CARDINAL;
      cacheIsEmpty                                            : BOOLEAN;
      dirtyCache                                              : BOOLEAN;
      cacheMutex                                              : MUTEX;

      bufferFile       : VirtualLocalFile.T;
      bufferFileName   : Pathname.T;
      splitMergeBuffer1: VirtualPage.T;
      splitMergeBuffer2: VirtualPage.T
      (* buffer pages for preserving source pages in Split/MergePage *)
    OVERRIDES
      open  := Open;
      close := Close;

      baseName  := BaseName;
      getPage   := GetPage;
      splitPage := SplitPage;
      copyPage  := CopyPage;
      mergePage := MergePage;

      insertCacheEntry := InsertCacheEntry;
      removeCacheEntry := RemoveCacheEntry;
      findCacheEntry   := FindCacheEntry;
      changeCacheEntry := ChangeCacheEntry;
    END;


(*
 | --- private stuff ------------------------------------------------------
 *)

PROCEDURE CommitAction (<* UNUSED *> event  : VirtualPageEvent.T;
                        <* UNUSED *> context: ContextSet.T;
                        <* UNUSED *> local  : BOOLEAN;
                                     data   : REFANY              ) =
  BEGIN
    WITH file = NARROW(data, T) DO
      LOCK file.cacheMutex DO
        (* do not clear cache on commit of top level.  clear only when a
           remote commit arrives. *)
        file.dirtyCache := TRUE;
      END;
    END;
  END CommitAction;

PROCEDURE AbortAction (<* UNUSED *> event  : VirtualPageEvent.T;
                       <* UNUSED *> context: ContextSet.T;
                       <* UNUSED *> local  : BOOLEAN;
                                    data   : REFANY              ) =
  BEGIN
    WITH file = NARROW(data, T) DO
      LOCK file.cacheMutex DO ClearCache(file) END;
    END;
  END AbortAction;

PROCEDURE BeginAction (<* UNUSED*>  event  : VirtualPageEvent.T;
                       <* UNUSED *> context: ContextSet.T;
                       <* UNUSED *> local  : BOOLEAN;
                                    data   : REFANY              ) =
  BEGIN
    WITH file = NARROW(data, T) DO
      LOCK file.cacheMutex DO
        file.dirtyCache := NOT file.cacheIsEmpty;
      END;
    END;
  END BeginAction;

PROCEDURE RemoteCommitAction (<* UNUSED *> event  : VirtualPageEvent.T;
                              <* UNUSED *> context: ContextSet.T;
                              <* UNUSED *> local  : BOOLEAN;
                                           data   : REFANY              ) =
  BEGIN
    WITH file = NARROW(data, T) DO
      LOCK file.cacheMutex DO
        IF file.dirtyCache THEN
          ClearCache(file);
          file.dirtyCache := FALSE;
        END;
      END;
    END;
  END RemoteCommitAction;


(*
 | --- public stuff -------------------------------------------------------
 *)
PROCEDURE Open (file    : T;
                resource: VirtualResource.T;
                fileName: Pathname.T;
                mode    : Access.Mode;
                kind    : Access.Kind;
                new     : BOOLEAN;
                local   : BOOLEAN            ): T
  RAISES {Access.Denied, PageFile.NoAccess} =
  BEGIN
    (* open regular file *)
    IF local THEN
      file.vfile := NEW(VirtualLocalFile.T).open(resource, fileName, new,
                                                 composeName := TRUE);
      (* open local file for splitbuffer *)
      file.bufferFileName :=
        resource.makeFileName(
          fileName & ".splitbuffer.local." & resource.getID(), temporary := TRUE);      
    ELSE
      file.vfile :=
        NEW(VirtualRemoteFile.T).open(resource, fileName, mode, kind, new);
      (* open local file for splitbuffer *)
      file.bufferFileName :=
        resource.makeFileName(
          fileName & ".splitbuffer." & resource.getID(), temporary := TRUE);      
    END;

    TRY
      WITH path = Pathname.Decompose(file.bufferFileName) DO
        (* make sure directoryies exist *)
        EVAL path.remhi();
        PageFileSystem.MakePath(Pathname.Compose(path));
      END;
    EXCEPT
      Pathname.Invalid =>        (* will throw up again later *)
    END;
    file.bufferFile := NEW(VirtualLocalFile.T).open(
                         resource, file.bufferFileName, new := TRUE,
                         composeName := FALSE);
    file.splitMergeBuffer1 := file.bufferFile.getPage(splitBuffer1No);
    file.splitMergeBuffer2 := file.bufferFile.getPage(splitBuffer2No);

    (* cache coherence handling *)
    file.cacheMutex := NEW(MUTEX);
    LOCK file.cacheMutex DO
      ClearCache(file);
      VAR
        pattern: VirtualPageEventPattern.T;
        trigger: Trigger.T;
        action : Action.Local;
        coupling  := Trigger.CouplingMode.Immediate;
        inh, perm := ContextSet.Empty();
      <* FATAL EventType.Mismatch, EventType.Unknown *>
      BEGIN
        (* commit: pre event, top level *)
        pattern := VirtualPageEventPattern.Create(
                     VirtualPageEvent.Operation.Commit);
        action := NEW(Action.Local).init(CommitAction);
        VirtualPageEventPattern.SetResource(pattern, resource);
        VirtualPageEventPattern.SetPreEvent(pattern, TRUE);
        VirtualPageEventPattern.SetLevel(pattern, Transaction.TopLevel);
        trigger := Trigger.Create(pattern, action, coupling, 0, inh, perm);
        file.commitTrigger := RuleEngine.RegisterTrigger(
                                trigger, RuleEngine.Interest.Self, file);
        (* begin: post event, top level *)
        pattern :=
          VirtualPageEventPattern.Create(VirtualPageEvent.Operation.Begin);
        action := NEW(Action.Local).init(BeginAction);
        VirtualPageEventPattern.SetResource(pattern, resource);
        VirtualPageEventPattern.SetPreEvent(pattern, FALSE);
        VirtualPageEventPattern.SetLevel(pattern, Transaction.TopLevel);
        trigger := Trigger.Create(pattern, action, coupling, 0, inh, perm);
        file.beginTrigger := RuleEngine.RegisterTrigger(
                               trigger, RuleEngine.Interest.Self, file);
        (* abort: pre event, all levels *)
        pattern :=
          VirtualPageEventPattern.Create(VirtualPageEvent.Operation.Abort);
        action := NEW(Action.Local).init(AbortAction);
        VirtualPageEventPattern.SetResource(pattern, resource);
        VirtualPageEventPattern.SetPreEvent(pattern, TRUE);
        trigger := Trigger.Create(pattern, action, coupling, 0, inh, perm);
        file.abortTrigger := RuleEngine.RegisterTrigger(
                               trigger, RuleEngine.Interest.Self, file);
        (* remote commit *)
        pattern := VirtualPageEventPattern.Create(
                     VirtualPageEvent.Operation.RemoteCommit);
        action := NEW(Action.Local).init(RemoteCommitAction);
        VirtualPageEventPattern.SetResource(pattern, resource);
        trigger := Trigger.Create(pattern, action, coupling, 0, inh, perm);
        file.remoteTrigger := RuleEngine.RegisterTrigger(
                                trigger, RuleEngine.Interest.Self, file);
      END;
    END;

    RETURN file;
  END Open;

PROCEDURE Close (file: T) RAISES {InternalError} =
  (* The local split buffer is private to this client.  No other clients
     will use it.  Hence, if we close it, it can be deleted. *)
  <* FATAL PageFile.NoAccess *>
  BEGIN
    TRY
      RuleEngine.UnregisterTrigger(file.commitTrigger);
      RuleEngine.UnregisterTrigger(file.abortTrigger);
      RuleEngine.UnregisterTrigger(file.beginTrigger);
      RuleEngine.UnregisterTrigger(file.remoteTrigger);

      file.bufferFile.close();
      PageFileSystem.DeleteFile(file.bufferFileName);

      file.vfile.close();
    EXCEPT
      VirtualResource.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITCFile.Close", "VirtualResource.FatalError", info));
    END;
  END Close;

PROCEDURE BaseName (file: T): Pathname.T =
  BEGIN
    RETURN file.vfile.getBaseName();
  END BaseName;
  
PROCEDURE GetPage (file: T; pageNo: CARDINAL): VirtualPage.T =
  BEGIN
    RETURN file.vfile.getPage(pageNo);
  END GetPage;

PROCEDURE SplitPage (    file              : T;
                         oldpno            : CARDINAL;
                         newpno1, newpno2  : CARDINAL;
                     VAR oldPage           : DataPage.T;
                     VAR newPage1, newPage2: DataPage.T  )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    (* get pointer to source page *)
    oldPage := file.vfile.getPage(oldpno);
    (* if it will be overwritten by one of the new pages, remove old page
       from pool *)
    IF ((oldpno = newpno1) OR (oldpno = newpno2)) THEN
      (**      oldPage.resetModified();   (* don't write back *)
            PagePool.ReleasePage(file, oldpno);
      *)
      (* copy old page data; it might be discarded by the calling
         procedures *)
      TRY
        file.splitMergeBuffer1.putAll(oldPage.getAll());
      EXCEPT
        VirtualPage.FatalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate(
                    "ITCFile.SplitPage", "VirtualPage.FatalError", info));
      END;
      oldPage := file.splitMergeBuffer1;
    END;

    (* get new pages *)
    newPage1 := file.vfile.getPage(newpno1);
    newPage2 := file.vfile.getPage(newpno2);
  END SplitPage;

PROCEDURE MergePage (    file                  : T;
                         newPageNo             : CARDINAL;
                         oldPage1No, oldPage2No: CARDINAL;
                     VAR newPage               : DataPage.T;
                     VAR oldPage1, oldPage2    : DataPage.T  )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    (* get source pages *)
    oldPage1 := file.vfile.getPage(oldPage1No);
    oldPage2 := file.vfile.getPage(oldPage2No);

    (**
    (* if the new page overwrites one of the old pages, don't write it
       back *)
    IF (oldPage1No = newPageNo) THEN
      oldPage1.resetModified();
    ELSIF (oldPage2No = newPageNo) THEN
      oldPage2.resetModified();
    END;
    (* remove source pages *)
    PagePool.ReleasePage(file, oldPage1No);
    PagePool.ReleasePage(file, oldPage2No);
    *)
    (* copy old page data; it might be discarded by the calling
       procedures *)
    TRY
      file.splitMergeBuffer1.putAll(oldPage1.getAll());
      oldPage1 := file.splitMergeBuffer1;
      file.splitMergeBuffer2.putAll(oldPage2.getAll());
      oldPage2 := file.splitMergeBuffer2;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITCFile.MergePage", "VirtualPage.FatalError", info));
    END;

    (* create new page *)
    newPage := file.vfile.getPage(newPageNo);
  END MergePage;


PROCEDURE CopyPage (file: T; oldPageNo: CARDINAL; newPageNo: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR op, np: VirtualPage.T;
  BEGIN
    op := file.vfile.getPage(oldPageNo);
    np := file.vfile.getPage(newPageNo);
    TRY
      np.putAll(op.getAll());
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITCFile.CopyPage", "VirtualPage.FatalError", info));
    END;
  END CopyPage;


PROCEDURE InsertCacheEntry (file          : T;
                            indexTree     : CARDINAL;
                            key1, key2    : CARDINAL;
                            location      : CARDINAL;
                            depth1, depth2: CARDINAL  ) =
  VAR
    hashKey     : CARDINAL;
    newHashKey  : CARDINAL;
    newBitOffset: CARDINAL;

  BEGIN
    LOCK file.cacheMutex DO
      WITH cache = file.caches[indexTree] DO
        (* compute help values *)
        hashKey := Word.Extract(key1, cache.bitOffset, Ld2Size);

        (* store new entry *)
        cache.entries[hashKey] :=
          Entry{relevantKey1 := Word.Extract(key1, 0, depth1),
                relevantKey2 := Word.Extract(key2, 0, depth2), location :=
                location, depth1 := depth1, depth2 := depth2};
        cache.definedEntries := cache.definedEntries + IndexSet{hashKey};

        (* check if bit offset must be adapted *)
        IF (depth1 > Ld2Size) THEN
          newBitOffset := depth1 - Ld2Size;
        ELSE
          newBitOffset := 0;
        END;

        (* rehash entry if necessary *)
        IF (newBitOffset # cache.bitOffset) THEN
          cache.bitOffset := newBitOffset;
          newHashKey := Word.Extract(key1, newBitOffset, Ld2Size);
          cache.entries[newHashKey] := cache.entries[hashKey];
          cache.definedEntries :=
            cache.definedEntries + IndexSet{newHashKey};
        END;
      END;

      file.cacheIsEmpty := FALSE;
    END;
  END InsertCacheEntry;


PROCEDURE RemoveCacheEntry (file: T; indexTree: CARDINAL; pageNo: CARDINAL) =
  BEGIN
    LOCK file.cacheMutex DO
      WITH cache = file.caches[indexTree] DO
        FOR i := 0 TO Size - 1 DO
          IF ((i IN cache.definedEntries)
                AND (cache.entries[i].location = pageNo)) THEN
            cache.definedEntries := cache.definedEntries - IndexSet{i};
          END;
        END;
      END;
    END;
  END RemoveCacheEntry;


PROCEDURE FindCacheEntry (    file          : T;
                              indexTree     : CARDINAL;
                              key1, key2    : CARDINAL;
                          VAR location      : CARDINAL;
                          VAR depth1, depth2: CARDINAL  ): BOOLEAN =
  VAR
    hashKey: CARDINAL;
    res    : BOOLEAN  := FALSE;
  BEGIN
    LOCK file.cacheMutex DO
      WITH cache = file.caches[indexTree] DO
        hashKey := Word.Extract(key1, cache.bitOffset, Ld2Size);
        IF (hashKey IN cache.definedEntries) THEN
          WITH entry = cache.entries[hashKey] DO
            IF ((entry.relevantKey1 = Word.Extract(key1, 0, entry.depth1))
                  AND (entry.relevantKey2
                         = Word.Extract(key2, 0, entry.depth2))) THEN
              location := entry.location;
              depth1 := entry.depth1;
              depth2 := entry.depth2;
              res := TRUE;
            END;
          END;
        END;
      END;
    END;
    RETURN res;
  END FindCacheEntry;


PROCEDURE ChangeCacheEntry (file                : T;
                            indexTree           : CARDINAL;
                            oldPageNo, newPageNo: CARDINAL  ) =
  BEGIN
    LOCK file.cacheMutex DO
      WITH cache = file.caches[indexTree] DO
        FOR i := 0 TO Size - 1 DO
          IF ((i IN cache.definedEntries)
                AND (cache.entries[i].location = oldPageNo)) THEN
            cache.entries[i].location := newPageNo;
          END;
        END;
      END;
    END;
  END ChangeCacheEntry;


PROCEDURE ClearCache (file: T) =
  BEGIN
    (* cacheMutex is already locked! *)

    FOR i := 1 TO SystemPage.MaxNoOfIndexTrees DO
      file.caches[i].bitOffset := 0;
      file.caches[i].definedEntries := IndexSet{};
    END;

    file.cacheIsEmpty := TRUE;
  END ClearCache;

BEGIN
END ITCFile.
