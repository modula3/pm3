MODULE IndexTreeOrganization;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:25:15  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.11  1997/02/20 16:16:02  roland
    Minor fixes.
    IndexTreeOrganization can now log merge and split of pages.

    Revision 1.10  1996/11/20 12:22:11  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.9  1996/08/06 16:25:40  roland
    Merge of PAGESERVER and main branch.

    Revision 1.8.2.3  1996/07/25 08:53:36  rbnix
        Methods insertCacheEntry, removeCacheEntry, findCacheEntry,
        changeCacheEntry moved from ITCFile into new interface
        InternalITCFile.

        BEWARE: usage of this methods is *** CURRENTLY NOT *** correct
        in multiuser mode because of ignorance of changes in database
        from other clients.

    Revision 1.8.2.2  1996/07/24 09:18:52  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.8.2.1  1996/04/29 13:38:15  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.8  1993/11/03  21:01:51  pk
# Parameter type for splitKey changed from [0 .. 1] to [1 .. 2].
#
# Revision 1.7  1993/11/03  20:56:44  pk
# Parameter type for splitKey changed from CARDINAL to [0 .. 1].
#
# Revision 1.6  1993/10/26  19:48:25  pk
# Naming of methods and procedures updated.
#
# Revision 1.5  1993/09/08  12:09:30  pk
# Several DIV/MOD operations on keys replaced by Word operations.
#
# Revision 1.4  1993/08/30  19:28:13  pk
# Error corrected: In ReleaseMemoryPage, the page no. must be adapted
# according to swapMap.
#
# Revision 1.3  1993/08/23  16:42:02  pk
# Error corrected: New pages are not created, since SplitPage does this.
#
# Revision 1.2  1993/08/20  20:08:30  pk
# Error corrected: must use ITCFile.T methods.
#
# Revision 1.1  1993/08/17  12:52:55  pk
# Functional support module for organizing the system index trees.
#
*)
(***************************************************************************)

IMPORT Word, Access, ITCFile, InternalITCFile, DataPage, IndexPage,
       SystemPage, ErrorSupport;

IMPORT Fmt, Variant, Journal;

CONST
  MaxDepth = 30;                 (* The highest value, for which
                                    Word.Insert(x, 1, MaxDepth, 1) does not
                                    result in a run-time error for x being
                                    a CARDINAL. *)

TYPE
  IndexTreeEntryDescription = RECORD
                                indexPageNo   : CARDINAL;
                                nodePosition  : IndexPage.NodePosition;
                                rightNode     : BOOLEAN;
                                depth1, depth2: CARDINAL;
                                entry         : IndexPage.Entry;
                              END;

  SwapMap = ARRAY [1 .. SystemPage.PageBufferSize] OF
              RECORD oldNo, newNo: CARDINAL;  END;


PROCEDURE SplitDataPage (    file                  : ITCFile.T;
                             dataPageNo            : CARDINAL;
                             indexTree             : CARDINAL;
                             splitKey              : [1 .. 2];
                         VAR newPage1No, newPage2No: CARDINAL;
                         VAR oldPage               : DataPage.T;
                         VAR newPage1, newPage2    : DataPage.T  )
  RAISES {Access.Locked, WrongPageReference, DepthExhausted, InternalError} =
  VAR
    pageLocation              : IndexTreeEntryDescription;
    newLocation1, newLocation2: IndexTreeEntryDescription;
    dataPage                  : DataPage.T;
    indexPage                 : IndexPage.T;
    oldIndexTree              : CARDINAL;
    key1, key2                : CARDINAL;
    depth1, depth2            : CARDINAL;

  BEGIN
    TRY
      (* get location of index tree reference to the data page *)
      dataPage := file.getPage(dataPageNo);
      DataPage.GetAll(dataPage, oldIndexTree, depth1, depth2, key1, key2);
      (* ITOWrongPageReference: reference to a wrong depth, key, or type of
         page? *)
      IF NOT (indexTree = oldIndexTree) THEN RAISE WrongPageReference END;

      IF Variant.LogSplitAndMerge THEN
        Journal.Add("SplitDataPage along key " & Fmt.Int(splitKey));
        Journal.Add("  oldPage " & FmtPageInfo(dataPageNo, oldIndexTree,
                                               depth1, depth2, key1, key2));
      END;

      pageLocation :=
        GetLocation(file, indexTree, key1, key2, depth1 + depth2);
      (* ITOWrongPageReference: reference to a wrong depth, key, or type of
         page? *)
      IF NOT ((ISTYPE(pageLocation.entry, IndexPage.PageRefEntry))
                AND (pageLocation.depth1 = depth1)
                AND (pageLocation.depth2 = depth2)
                AND (NARROW(
                       pageLocation.entry, IndexPage.PageRefEntry).pageNo
                       = dataPageNo)) THEN
        RAISE WrongPageReference;
      END;

      (* remove cache entry for data page *)
      ITCFile.T.removeCacheEntry(file, indexTree, dataPageNo);

      (* set up location of index tree entry to left new page reference *)

      (* the old page is reused as left page *)
      newLocation1 := pageLocation;

      (* a new node is required to hold pointers to the new pages *)
      CreateNode(file, newLocation1.indexPageNo, newLocation1.nodePosition,
                 dataPageNo);
      newLocation1.rightNode := FALSE;
      IF (splitKey = 1) THEN
        INC(newLocation1.depth1);
        IF newLocation1.depth1 > MaxDepth THEN RAISE DepthExhausted END;
      ELSE
        INC(newLocation1.depth2);
        IF newLocation1.depth2 > MaxDepth THEN RAISE DepthExhausted END;
      END;
      newPage1No := dataPageNo;

      (* set up location of index tree entry to right new page reference *)

      (* create a new data page as right page *)
      newPage2No := CreateDataPage(file, indexTree);
      newLocation2 := newLocation1;
      newLocation2.entry :=
        NEW(IndexPage.PageRefEntry, pageNo := newPage2No);
      newLocation2.rightNode := TRUE;

      (* modify old index tree node to point to the new node *)
      pageLocation.entry :=
        NEW(IndexPage.NodeRefEntry, splitKey := splitKey,
            indexPageNo := newLocation1.indexPageNo,
            indexPagePos := newLocation1.nodePosition);

      TRY
        indexPage := file.getPage(pageLocation.indexPageNo);
        IndexPage.PutEntry(indexPage, pageLocation.nodePosition,
                           pageLocation.rightNode, pageLocation.entry);

        IF (newLocation1.indexPageNo # pageLocation.indexPageNo) THEN
          indexPage := file.getPage(newLocation1.indexPageNo);
        END;
        IndexPage.PutEntry(indexPage, newLocation1.nodePosition,
                           newLocation1.rightNode, newLocation1.entry);

        IF (newLocation2.indexPageNo # newLocation1.indexPageNo) THEN
          indexPage := file.getPage(newLocation2.indexPageNo);
        END;
        IndexPage.PutEntry(indexPage, newLocation2.nodePosition,
                           newLocation2.rightNode, newLocation2.entry);
      EXCEPT
        IndexPage.StatusViolation =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.SplitDataPage",
                                "IndexPage.StatusViolation"));
      | IndexPage.WrongEntry =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.SplitDataPage",
                                "IndexPage.WrongEntry"));
      END;

      (* get all pages into memory *)
      file.splitPage(
        dataPageNo, newPage1No, newPage2No, oldPage, newPage1, newPage2);

      (* write data page information to the new pages *)
      DataPage.PutAll(newPage1, indexTree, newLocation1.depth1,
                      newLocation1.depth2, key1, key2);
      IF Variant.LogSplitAndMerge THEN
        Journal.Add(
          " newPage1 "
            & FmtPageInfo(newPage1No, indexTree, newLocation1.depth1,
                          newLocation1.depth2, key1, key2));
      END;
      IF (splitKey = 1) THEN
        WITH newKey1 = Word.Insert(key1, 1, pageLocation.depth1, 1) DO
          DataPage.PutAll(newPage2, indexTree, newLocation2.depth1,
                          newLocation2.depth2, newKey1, key2);
          IF Variant.LogSplitAndMerge THEN
            Journal.Add(
              " newPage2 "
                & FmtPageInfo(newPage2No, indexTree, newLocation2.depth1,
                              newLocation2.depth2, newKey1, key2));
          END;
        END;
      ELSE
        WITH newKey2 = Word.Insert(key2, 1, pageLocation.depth2, 1) DO
          DataPage.PutAll(newPage2, indexTree, newLocation2.depth1,
                          newLocation2.depth2, key1, newKey2);
          IF Variant.LogSplitAndMerge THEN
            Journal.Add(
              "  oldPage "
                & FmtPageInfo(newPage2No, indexTree, newLocation2.depth1,
                              newLocation2.depth2, key1, newKey2));
          END;
        END;
      END;
    EXCEPT
      DataPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.SplitDataPage",
                              "DataPage.InternalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.SplitDataPage",
                              "IndexPage.InternalError", info));
    | ITCFile.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.SplitDataPage",
                              "ITCFile.InternalError", info));
    END;
  END SplitDataPage;


PROCEDURE MergeDataPage (    file                  : ITCFile.T;
                             oldPage1No, oldPage2No: CARDINAL;
                             indexTree             : CARDINAL;
                             splitKey              : [1 .. 2];
                         VAR newPageNo             : CARDINAL;
                         VAR oldPage1, oldPage2    : DataPage.T;
                         VAR newPage               : DataPage.T  )
  RAISES {Access.Locked, WrongPageReference, PageBufferFault, InternalError} =
  VAR
    dataPage                      : DataPage.T;
    indexPage                     : IndexPage.T;
    oldIndexTree                  : CARDINAL;
    key1, key2                    : CARDINAL;
    depth1, depth2                : CARDINAL;
    fatherLocation                : IndexTreeEntryDescription;
    child1Location, child2Location: IndexTreeEntryDescription;
    swapMap                       : SwapMap;
    swapCount                     : CARDINAL;

  BEGIN
    TRY
      (* get locations for father and one of the child pages *)
      dataPage := file.getPage(oldPage1No);
      DataPage.GetAll(dataPage, oldIndexTree, depth1, depth2, key1, key2);
      (* ITOWrongPageReference: reference to a wrong depth, key, or type of
         page? *)
      IF NOT (indexTree = oldIndexTree) THEN RAISE WrongPageReference END;

      IF Variant.LogSplitAndMerge THEN
        Journal.Add("MergeDataPage along key " & Fmt.Int(splitKey));
        Journal.Add(" oldPage1 " & FmtPageInfo(oldPage1No, oldIndexTree,
                                               depth1, depth2, key1, key2));
      END;

      TRY
        GetFatherAndBrother(file, indexTree, key1, key2, depth1 + depth2,
                            fatherLocation, child2Location);
      EXCEPT
        DepthExhausted =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.MergeDataPage",
                                "IndexTreeOrganization.DepthExhausted"));
      END;
      (* ITOWrongPageReference: reference to a wrong depth, key, or type of
         page? *)
      IF NOT ((ISTYPE(child2Location.entry, IndexPage.PageRefEntry))
                AND (child2Location.depth1 = depth1)
                AND (child2Location.depth2 = depth2)
                AND (NARROW(
                       child2Location.entry, IndexPage.PageRefEntry).pageNo
                       = oldPage2No)
                AND (ISTYPE(fatherLocation.entry, IndexPage.NodeRefEntry))
                AND (NARROW(
                       fatherLocation.entry, IndexPage.NodeRefEntry).splitKey
                       = splitKey)) THEN
        RAISE WrongPageReference;
      END;

      IF Variant.LogSplitAndMerge THEN
        Journal.Add(
          " oldPage2 "
            & FmtPageInfo(oldPage2No, indexTree, child2Location.depth1,
                          child2Location.depth2, key1, key2));
      END;


      TRY
        (* set up brother *)
        child1Location := child2Location;
        child1Location.rightNode := NOT child1Location.rightNode;
        indexPage := file.getPage(child1Location.indexPageNo);
        child1Location.entry :=
          IndexPage.GetEntry(indexPage, child1Location.nodePosition,
                             child1Location.rightNode);
        (* ITOWrongPageReference: reference to a wrong depth, key, or type
           of page? *)
        IF NOT ((ISTYPE(child1Location.entry, IndexPage.PageRefEntry))
                  AND (child2Location.depth1 = depth1)
                  AND (child2Location.depth2 = depth2)
                  AND (NARROW(
                         child1Location.entry, IndexPage.PageRefEntry).pageNo
                         = oldPage1No)) THEN
          RAISE WrongPageReference;
        END;
      EXCEPT
        IndexPage.StatusViolation =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.MergeDataPage",
                                "IndexPage.StatusViolation"));
      END;

      (* remove old pages from cache *)
      ITCFile.T.removeCacheEntry(file, indexTree, oldPage1No);
      ITCFile.T.removeCacheEntry(file, indexTree, oldPage2No);

      (* delete second page, adjust page numbers if necessary *)
      ReleaseDataPage(file, oldPage2No, indexTree, swapMap, swapCount);
      FOR i := 1 TO swapCount DO
        IF (swapMap[i].oldNo = oldPage1No) THEN
          oldPage1No := swapMap[i].newNo;
        ELSIF (swapMap[i].oldNo = oldPage2No) THEN
          oldPage2No := swapMap[i].newNo;
        END;
      END;

      (* sync page number variables *)
      NARROW(child1Location.entry, IndexPage.PageRefEntry).pageNo :=
        oldPage1No;

      (* remove index node for second entry *)
      ReleaseNode(
        file, child2Location.indexPageNo, child2Location.nodePosition);


      TRY
        (* set up new father entry & store it *)
        fatherLocation.entry := child1Location.entry;
        newPageNo := oldPage1No;
        indexPage := file.getPage(fatherLocation.indexPageNo);
        IndexPage.PutEntry(indexPage, fatherLocation.nodePosition,
                           fatherLocation.rightNode, fatherLocation.entry);
      EXCEPT
        IndexPage.StatusViolation =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.MergeDataPage",
                                "IndexPage.StatusViolation"));
      | IndexPage.WrongEntry =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.MergeDataPage",
                                "IndexPage.WrongEntry"));
      END;

      (* swap in all pages *)
      file.mergePage(
        newPageNo, oldPage1No, oldPage2No, newPage, oldPage1, oldPage2);

      (* store new data page info on merged page *)
      WITH fatherKey1 = Word.Extract(key1, 0, fatherLocation.depth1),
           fatherKey2 = Word.Extract(key2, 0, fatherLocation.depth2)  DO
        DataPage.PutAll(newPage, indexTree, fatherLocation.depth1,
                        fatherLocation.depth2, fatherKey1, fatherKey2);

        IF Variant.LogSplitAndMerge THEN
          Journal.Add(
            "  newPage "
              & FmtPageInfo(newPageNo, indexTree, fatherLocation.depth1,
                            fatherLocation.depth2, fatherKey1, fatherKey2));
        END;
      END;

    EXCEPT
      DataPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.MergeDataPage",
                              "DataPage.InternalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.MergeDataPage",
                              "IndexPage.InternalError", info));
    | ITCFile.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.MergeDataPage",
                              "ITCFile.InternalError", info));
    END;
  END MergeDataPage;


PROCEDURE GetFatherAndBrotherEntry (    file      : ITCFile.T;
                                        indexTree : CARDINAL;
                                        key1, key2: CARDINAL;
                                        depth     : CARDINAL;
                                    VAR father    : IndexPage.Entry;
                                    VAR brother   : IndexPage.Entry  )
  RAISES {Access.Locked, DepthExhausted, InternalError} =
  VAR fatherLocation, brotherLocation: IndexTreeEntryDescription;

  BEGIN
    GetFatherAndBrother(
      file, indexTree, key1, key2, depth, fatherLocation, brotherLocation);
    father := fatherLocation.entry;
    brother := brotherLocation.entry;
  END GetFatherAndBrotherEntry;


PROCEDURE GetFatherAndBrother (    file      : ITCFile.T;
                                   indexTree : CARDINAL;
                                   key1, key2: CARDINAL;
                                   depth     : CARDINAL;
                               VAR father    : IndexTreeEntryDescription;
                               VAR brother   : IndexTreeEntryDescription  )
  RAISES {Access.Locked, DepthExhausted, InternalError} =
  VAR indexPage: IndexPage.T;

  BEGIN
    father := GetLocation(file, indexTree, key1, key2, depth - 1);
    (* ITODepthExhausted: index tree is not deep enough? *)
    IF NOT (ISTYPE(father.entry, IndexPage.NodeRefEntry)) THEN
      RAISE DepthExhausted
    END;

    WITH entry = NARROW(father.entry, IndexPage.NodeRefEntry) DO
      brother.indexPageNo := entry.indexPageNo;
      brother.nodePosition := entry.indexPagePos;
      brother.depth1 := father.depth1;
      brother.depth2 := father.depth2;

      (* branch according to split key and inverted key bit *)
      IF (entry.splitKey = 1) THEN
        brother.rightNode := (Word.Extract(key1, brother.depth1, 1) = 0);
        INC(brother.depth1);
      ELSE
        brother.rightNode := (Word.Extract(key2, brother.depth2, 1) = 0);
        INC(brother.depth2);
      END;
    END;

    indexPage := file.getPage(brother.indexPageNo);
    TRY
      brother.entry := IndexPage.GetEntry(indexPage, brother.nodePosition,
                                          brother.rightNode);
    EXCEPT
      IndexPage.StatusViolation =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.GetFatherAndBrother",
                              "IndexPage.StatusViolation"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.GetFatherAndBrother",
                              "IndexPage.InternalError", info));
    END;
  END GetFatherAndBrother;


PROCEDURE CreateNode (    file        : ITCFile.T;
                      VAR pageNo      : CARDINAL;
                      VAR nodePosition: IndexPage.NodePosition;
                      VAR swapPage    : CARDINAL                )
  RAISES {Access.Locked, InternalError} =
  VAR
    systemPage: SystemPage.T;
    indexPage : IndexPage.T;
    exists    : BOOLEAN;

  BEGIN
    TRY
      systemPage := file.getPage(SystemPageNo);
      pageNo := SystemPage.FindNonfullIndexPage(systemPage, exists);
      IF (NOT exists) THEN pageNo := CreateIndexPage(file, swapPage); END;

      indexPage := file.getPage(pageNo);
      nodePosition := IndexPage.Reserve(indexPage);

      systemPage := file.getPage(SystemPageNo);
      SystemPage.NotifyNodeReservation(systemPage, pageNo);
    EXCEPT
      SystemPage.PageOverflow =>
        RAISE InternalError(
                ErrorSupport.Create("IndexTreeOrganization.CreateNode",
                                    "SystemPage.PageOverflow"));
    | IndexPage.Overflow =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateNode",
                              "IndexPage.Overflow"));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexTreeOrganization.CreateNode",
                                       "SystemPage.InternalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexTreeOrganization.CreateNode",
                                       "IndexPage.InternalError", info));
    END;
  END CreateNode;


PROCEDURE ReleaseNode (file        : ITCFile.T;
                       pageNo      : CARDINAL;
                       nodePosition: IndexPage.NodePosition)
  RAISES {Access.Locked, InternalError} =
  VAR
    systemPage: SystemPage.T;
    indexPage : IndexPage.T;

  BEGIN
    TRY
      indexPage := file.getPage(pageNo);
      IndexPage.Release(indexPage, nodePosition);

      systemPage := file.getPage(SystemPageNo);
      SystemPage.NotifyNodeRelease(systemPage, pageNo);
    EXCEPT
      SystemPage.PageUnderflow =>
        RAISE InternalError(
                ErrorSupport.Create("IndexTreeOrganization.ReleaseNode",
                                    "SystemPage.PageUnderflow"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexTreeOrganization.ReleaseNode",
                                       "IndexPage.InternalError", info));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexTreeOrganization.ReleaseNode",
                                       "SystemPage.InternalError", info));
    END;
  END ReleaseNode;


PROCEDURE CreateIndexPage (file: ITCFile.T; swapPage: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    newPageNo     : CARDINAL;
    newPage       : IndexPage.T;
    systemPage    : SystemPage.T;
    noOfIndexPages: CARDINAL;

  BEGIN
    newPageNo := CreateMemoryPage(file);
    systemPage := file.getPage(SystemPageNo);

    TRY
      (* if the new page is behind a data page, move the data page back and
         put the index page there *)
      noOfIndexPages := SystemPage.NoOfIndexPages(systemPage);
      IF (newPageNo # noOfIndexPages + 1) THEN
        TRY
          CopyDataPage(file, noOfIndexPages + 1, newPageNo);
        EXCEPT
          WrongPageReference =>
            RAISE
              InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateIndexPage",
                              "IndexTreeOrganization.WrongPageReference"));
        END;
        IF (swapPage = noOfIndexPages + 1) THEN swapPage := newPageNo; END;
        newPageNo := noOfIndexPages + 1;
      END;

      newPage := file.getPage(newPageNo);
      IndexPage.Init(newPage);

      systemPage := file.getPage(SystemPageNo);
      SystemPage.NotifyIndexPageCreation(systemPage, newPageNo);
    EXCEPT
      SystemPage.AlreadyUsed =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateIndexPage",
                              "SystemPage.AlreadyUsed"));
    | SystemPage.IndexTreeOverflow =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateIndexPage",
                              "SystemPage.IndexTreeOverflow"));
    | SystemPage.WrongIndexPageNumber =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateIndexPage",
                              "SystemPage.WrongIndexPageNumber"));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CreateIndexPage",
                              "SystemPage.InternalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CreateIndexPage",
                              "IndexPage.InternalError", info));
    END;
    RETURN newPageNo;
  END CreateIndexPage;


PROCEDURE CreateDataPage (file: ITCFile.T; indexTree: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    newPageNo : CARDINAL;
    systemPage: SystemPage.T;

  BEGIN
    newPageNo := CreateMemoryPage(file);
    systemPage := file.getPage(SystemPageNo);
    TRY
      SystemPage.NotifyDataPageCreation(systemPage, indexTree);
    EXCEPT
    | SystemPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CreateDataPage",
                              "SystemPage.InternalError", info));
    END;
    RETURN newPageNo;
  END CreateDataPage;


PROCEDURE ReleaseDataPage (    file     : ITCFile.T;
                               pageNo   : CARDINAL;
                               indexTree: CARDINAL;
                           VAR swapMap  : SwapMap;
                           VAR swapCount: CARDINAL   )
  RAISES {Access.Locked, PageBufferFault, InternalError} =
  VAR systemPage: SystemPage.T;

  BEGIN
    ReleaseMemoryPage(file, pageNo, swapMap, swapCount);
    systemPage := file.getPage(SystemPageNo);
    TRY
      SystemPage.NotifyDataPageDeletion(systemPage, indexTree);
    EXCEPT
      SystemPage.SystemPageCorrupt =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.ReleaseDataPage",
                              "SystemPage.SystemPageCorrupt"));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.ReleaseDataPage",
                              "SystemPage.InternalError", info));
    END;
  END ReleaseDataPage;


PROCEDURE CopyDataPage (file: ITCFile.T; oldPageNo, newPageNo: CARDINAL)
  RAISES {Access.Locked, WrongPageReference, InternalError} =
  VAR
    dataPage      : DataPage.T;
    indexPage     : IndexPage.T;
    indexTree     : CARDINAL;
    reference     : IndexTreeEntryDescription;
    depth1, depth2: CARDINAL;
    key1, key2    : CARDINAL;

  BEGIN
    TRY
      file.copyPage(oldPageNo, newPageNo);
      dataPage := file.getPage(newPageNo);
      DataPage.GetAll(dataPage, indexTree, depth1, depth2, key1, key2);

      (* change the cache entry *)
      ITCFile.T.changeCacheEntry(file, indexTree, oldPageNo, newPageNo);

      (* change index tree references to the old page *)
      TRY
        reference :=
          GetLocation(file, indexTree, key1, key2, depth1 + depth2);
      EXCEPT
        DepthExhausted =>
          RAISE InternalError(ErrorSupport.Create(
                                "IndexTreeOrganization.CopyDataPage",
                                "IndexTreeOrganization.DepthExhausted"));
      END;
      (* ITOWrongPageReference: reference to a wrong depth, key, or type of
         page? *)
      IF NOT ((ISTYPE(reference.entry, IndexPage.PageRefEntry)
                 AND (NARROW(
                        reference.entry, IndexPage.PageRefEntry).pageNo
                        = oldPageNo))) THEN
        RAISE WrongPageReference;
      END;
      NARROW(reference.entry, IndexPage.PageRefEntry).pageNo := newPageNo;
      indexPage := file.getPage(reference.indexPageNo);
      IndexPage.PutEntry(indexPage, reference.nodePosition,
                         reference.rightNode, reference.entry);
    EXCEPT
      IndexPage.StatusViolation =>
        RAISE InternalError(
                ErrorSupport.Create("IndexTreeOrganization.CopyDataPage",
                                    "IndexPage.StatusViolation"));
    | IndexPage.WrongEntry =>
        RAISE InternalError(
                ErrorSupport.Create("IndexTreeOrganization.CopyDataPage",
                                    "IndexPage.WrongEntry"));
    | ITCFile.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CopyDataPage",
                              "ITCFile.InternalError", info));
    | DataPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CopyDataPage",
                              "DataPage.InternalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CopyDataPage",
                              "IndexPage.InternalError", info));
    END;
  END CopyDataPage;


PROCEDURE GetLocation (file      : ITCFile.T;
                       indexTree : CARDINAL;
                       key1, key2: CARDINAL;
                       depth     : CARDINAL   ): IndexTreeEntryDescription
  RAISES {Access.Locked, DepthExhausted, InternalError} =
  VAR
    current := IndexTreeEntryDescription{indexPageNo := SystemPageNo,
                                         nodePosition := 1, rightNode :=
                                         FALSE, depth1 := 0, depth2 := 0,
                                         entry := NIL};
    oldPageNo: CARDINAL;
    page     : IndexPage.T;

  BEGIN
    (* initialize current description *)
    page := file.getPage(current.indexPageNo);
    TRY
      current.nodePosition :=
        SystemPage.GetIndexTreeRootNode(page, indexTree);
      current.entry :=
        IndexPage.GetEntry(page, current.nodePosition, current.rightNode);
      oldPageNo := current.indexPageNo;

      (* loop until depth is reached *)
      WHILE (depth > 0) DO
        (* ITODepthExhausted: index tree is not deep enough? *)
        IF NOT (ISTYPE(current.entry, IndexPage.NodeRefEntry)) THEN
          RAISE DepthExhausted
        END;

        DEC(depth);
        WITH entry = NARROW(current.entry, IndexPage.NodeRefEntry) DO
          (* branch according to split key and key bit *)
          IF (entry.splitKey = 1) THEN
            current.rightNode := (Word.Extract(key1, 0, 1) = 1);
            key1 := Word.RightShift(key1, 1);
            INC(current.depth1);
          ELSE
            current.rightNode := (Word.Extract(key2, 0, 1) = 1);
            key2 := Word.RightShift(key2, 1);
            INC(current.depth2);
          END;

          current.indexPageNo := entry.indexPageNo;
          current.nodePosition := entry.indexPagePos;

          (* swap in new page if necessary *)
          IF (current.indexPageNo # oldPageNo) THEN
            page := file.getPage(current.indexPageNo);
            oldPageNo := current.indexPageNo;
          END;

          current.entry :=
            IndexPage.GetEntry(
              page, current.nodePosition, current.rightNode);
        END;
      END;
    EXCEPT
      IndexPage.StatusViolation =>
        RAISE InternalError(
                ErrorSupport.Create("IndexTreeOrganization.GetLocation",
                                    "IndexPage.StatusViolation"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexTreeOrganization.GetLocation",
                                       "IndexPage.InternalError", info));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexTreeOrganization.GetLocation",
                                       "SystemPage.InternalError", info));
    END;
    RETURN current;
  END GetLocation;


PROCEDURE CreateMemoryPage (file: ITCFile.T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    systemPage: SystemPage.T;
    unused    : CARDINAL;
  BEGIN
    systemPage := file.getPage(SystemPageNo);

    TRY
      (* extend file if necessary *)
      IF (SystemPage.NoOfUnusedPages(systemPage) = 0) THEN
        SystemPage.NotifyExtension(systemPage);
      END;

      unused := SystemPage.GetUnusedPage(systemPage);
    EXCEPT
      SystemPage.NoUnusedPages =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateMemoryPage",
                              "SystemPage.NoUnusedPages"));
    | SystemPage.BufferNotEmpty =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.CreateMemoryPage",
                              "SystemPage.BufferNotEmpty"));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.CreateMemoryPage",
                              "SystemPage.InternalError", info));
    END;
    RETURN unused;
  END CreateMemoryPage;


PROCEDURE ReleaseMemoryPage (    file     : ITCFile.T;
                                 pageNo   : CARDINAL;
                             VAR swapMap  : SwapMap;
                             VAR swapCount: CARDINAL   )
  RAISES {Access.Locked, PageBufferFault, InternalError} =
  VAR systemPage: SystemPage.T;

  BEGIN
    systemPage := file.getPage(SystemPageNo);

    TRY
      (* truncate file if necessary *)
      IF (SystemPage.NoOfUnusedPages(systemPage)
            = SystemPage.PageBufferSize) THEN
        SwapUnusedToBack(file, swapMap, swapCount);
        systemPage := file.getPage(SystemPageNo);
        SystemPage.NotifyTruncation(systemPage);

        (* check if the page was swapped *)
        FOR i := 1 TO swapCount DO
          IF (swapMap[i].oldNo = pageNo) THEN
            pageNo := swapMap[i].newNo;
          END;
        END;
      END;

      SystemPage.PutUnusedPage(systemPage, pageNo);
    EXCEPT
      SystemPage.BufferNotFull =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.ReleaseMemoryPage",
                              "SystemPage.BufferNotFull"));
    | SystemPage.BufferOverflow =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.ReleaseMemoryPage",
                              "SystemPage.BufferOverflow"));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.ReleaseMemoryPage",
                              "SystemPage.InternalError", info));
    END;
  END ReleaseMemoryPage;


PROCEDURE SwapUnusedToBack (    file     : ITCFile.T;
                            VAR swapMap  : SwapMap;
                            VAR swapCount: CARDINAL   )
  RAISES {Access.Locked, PageBufferFault, InternalError} =
  TYPE PageBuffer = ARRAY [1 .. SystemPage.PageBufferSize] OF CARDINAL;

  VAR
    noOfUnused          : CARDINAL;
    noOfExistent        : CARDINAL;
    margin              : CARDINAL;
    j                   : CARDINAL;
    noOfKeepUnused                     := 0;
    noOfReusable                       := 0;
    noOfUsedBehindMargin               := 0;
    unusedPages         : PageBuffer;
    keepUnused          : PageBuffer;
    reuse               : PageBuffer;
    usedBehindMargin    : PageBuffer;
    isUnused            : BOOLEAN;
    systemPage          : SystemPage.T;

  BEGIN
    (* get counter values from system page *)
    systemPage := file.getPage(SystemPageNo);
    TRY
      noOfUnused := SystemPage.NoOfUnusedPages(systemPage);
      noOfExistent := SystemPage.NoOfPhysExistentPages(systemPage);
      margin := noOfExistent - SystemPage.PageBufferSize;

      (* categorize unused pages *)
      FOR i := 1 TO noOfUnused DO
        unusedPages[i] := SystemPage.GetUnusedPage(systemPage);
        IF (unusedPages[i] > margin) THEN
          INC(noOfKeepUnused);
          keepUnused[noOfKeepUnused] := unusedPages[i];
        ELSE
          INC(noOfReusable);
          reuse[noOfReusable] := unusedPages[i];
        END;
      END;

      (* push back unused pages already behind margin *)
      FOR i := 1 TO noOfKeepUnused DO
        SystemPage.PutUnusedPage(systemPage, keepUnused[i]);
      END;
    EXCEPT
      SystemPage.NoUnusedPages =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.SwapUnusedToBack",
                              "SystemPage.NoUnusedPages"));
    | SystemPage.BufferOverflow =>
        RAISE InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.SwapUnusedToBack",
                              "SystemPage.BufferOverflow"));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "IndexTreeOrganization.SwapUnusedToBack",
                              "SystemPage.InternalError", info));
    END;

    (* check the pages behind margin *)
    FOR i := margin + 1 TO noOfExistent DO
      (* find out if the page is in use *)
      isUnused := FALSE;
      j := 1;
      WHILE (j <= noOfUnused) DO
        IF (unusedPages[j] = i) THEN isUnused := TRUE; END;
        INC(j);
      END;
      IF (NOT isUnused) THEN
        INC(noOfUsedBehindMargin);
        usedBehindMargin[noOfUsedBehindMargin] := i;
      END;
    END;

    (* find new locations for used pages behind margin *)
    swapCount := 0;
    WHILE (noOfUsedBehindMargin > 0) DO
      IF (noOfReusable > 0) THEN
        (* move the page to an unused page before margin *)
        INC(swapCount);
        swapMap[swapCount].oldNo := usedBehindMargin[noOfUsedBehindMargin];
        swapMap[swapCount].newNo := reuse[noOfReusable];
        TRY
          CopyDataPage(
            file, swapMap[swapCount].oldNo, swapMap[swapCount].newNo);
          systemPage := file.getPage(SystemPageNo);
          SystemPage.PutUnusedPage(systemPage, swapMap[swapCount].oldNo);
        EXCEPT
          WrongPageReference =>
            RAISE
              InternalError(ErrorSupport.Create(
                              "IndexTreeOrganization.SwapUnusedToBack",
                              "IndexTreeOrganization.WrongPageReference"));
        | SystemPage.BufferOverflow =>
            RAISE InternalError(ErrorSupport.Create(
                                  "IndexTreeOrganization.SwapUnusedToBack",
                                  "SystemPage.BufferOverflow"));
        | SystemPage.InternalError (info) =>
            RAISE InternalError(ErrorSupport.Propagate(
                                  "IndexTreeOrganization.SwapUnusedToBack",
                                  "SystemPage.InternalError", info));
        END;
        DEC(noOfReusable);
        DEC(noOfUsedBehindMargin);
      ELSE
        (* ITOPageBufferFault: error in handling the page buffer? *)
        RAISE PageBufferFault;
      END;
    END;
  END SwapUnusedToBack;

PROCEDURE FmtPageInfo (pageNo, indexTree, depth1, depth2, key1, key2: CARDINAL):
  TEXT =
  BEGIN
    RETURN
      "pageNo = " & Fmt.Int(pageNo) & " tree = " & Fmt.Int(indexTree)
        & " depth1 = " & Fmt.Int(depth1) & " depth2 = " & Fmt.Int(depth2)
        & " key1 = " & Fmt.Int(key1) & " key2 = " & Fmt.Int(key2);
  END FmtPageInfo;

BEGIN
END IndexTreeOrganization.
