MODULE ITPFile;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.4  1997/11/19 17:59:12  roland
    Removed grouping of page accesses.

    Revision 1.3  1997/09/24 13:22:33  roland
    A method for preorder traversal of index tree added.

    Revision 1.2  1997/04/24 14:31:34  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:25:03  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.18  1997/02/20 16:15:59  roland
    Minor fixes.
    IndexTreeOrganization can now log merge and split of pages.

    Revision 1.17  1996/11/20 12:22:06  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.16  1996/09/17 13:03:11  roland
    Explicitly call VirtualResource.T.beginTransaction/commitTransaction
    to avoid conflicts with higher layers that override these methods.

    Revision 1.15  1996/08/06 16:25:33  roland
    Merge of PAGESERVER and main branch.

    Revision 1.14.2.4  1996/07/25 09:17:40  rbnix
        Obsolete function checkIn removed.

    Revision 1.14.2.3  1996/07/25 08:53:34  rbnix
        Methods insertCacheEntry, removeCacheEntry, findCacheEntry,
        changeCacheEntry moved from ITCFile into new interface
        InternalITCFile.

        BEWARE: usage of this methods is *** CURRENTLY NOT *** correct
        in multiuser mode because of ignorance of changes in database
        from other clients.

    Revision 1.14.2.2  1996/07/24 09:18:45  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.14.2.1  1996/04/29 13:38:09  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.14  1995/06/23  08:17:29  rbnix
# 	Bug fixed in FindPage: the page number of the currently loaded
# 	page was used but not updated when changing to another page.
#
# Revision 1.13  1994/03/30  18:26:35  pk
# ReleasePage removed.
#
# Revision 1.12  1994/03/30  17:28:18  pk
# Adaptions for new Files subsystem.
#
# Revision 1.11  1994/01/18  19:06:50  pk
# IllegalOperation is now a local and FATAL exception.
#
# Revision 1.10  1993/11/03  21:13:57  pk
# Parameter type for splitKey changed from CARDINAL to [1 .. 2].
#
# Revision 1.9  1993/11/03  20:53:27  pk
# New methods for splitPage and mergePage hide the old ones and provide
# the functionality of the key specific methods.
#
# Revision 1.8  1993/11/03  20:39:37  pk
# New checkIn and close methods hide the old ones, where truncate and
# maxPage values had to be supplied.
#
# Revision 1.7  1993/11/03  19:25:12  pk
# Some identifiers renamed.
#
# Revision 1.6  1993/11/03  18:25:34  pk
# New naming convention Base and Super introduced.
#
# Revision 1.5  1993/10/26  19:48:21  pk
# Naming of methods and procedures updated.
#
# Revision 1.4  1993/09/08  12:09:27  pk
# Several DIV/MOD operations on keys replaced by Word operations.
#
# Revision 1.3  1993/08/19  19:30:01  pk
# Error corrected: ITCFile method used for Create
#
# Revision 1.2  1993/08/19  18:38:14  pk
# dataStart parameter eliminated: useless
#
# Revision 1.1  1993/08/17  12:51:44  pk
# Abstract data type for index tree page files.
#
*)
(***************************************************************************)
IMPORT ITCFile AS Super;
IMPORT Word, Pathname, PageFile, Access, VirtualPage, VirtualResource,
       InternalITCFile, IndexTreeOrganization, SystemPage, IndexPage,
       DataPage, ErrorSupport, Journal, Fmt;


REVEAL
  T = Public BRANDED OBJECT
        noOfTrees: CARDINAL;
      OVERRIDES
        open  := Open;
        close := Close;

        noOfPagesInTree := NoOfPagesInTree;
        findPage        := FindPage;
        findBrother     := FindBrother;
        getPage         := GetPage;
        splitPage       := SplitPage;
        mergePage       := MergePage;

        preorderRun := PreorderRun;
      END;


PROCEDURE Open (file     : T;
                resource : VirtualResource.T;
                fileName : Pathname.T;
                mode     : Access.Mode;
                kind     : Access.Kind;
                new      : BOOLEAN;
                local    : BOOLEAN;
                noOfTrees: CARDINAL           ): T
  RAISES {Access.Denied, PageFile.NoAccess, TreeUnknown} =
  VAR
    systemPage: SystemPage.T;
    rootPage  : DataPage.T;
    abort     : BOOLEAN      := FALSE;

  (* When Access.Locked is raised on the page in a new file (which no one
     else can possibly access by now) this is definetely a reason to abort,
     Abort and commit are under local control. *)
  <* FATAL Access.Locked, VirtualResource.NotInTransaction *>
  BEGIN
    (* init file as Super *)
    file := Super.T.open(file, resource, fileName, mode, kind, new, local);

    TRY
      TRY
        VirtualResource.T.beginTransaction(resource);
        IF new THEN
          (* create system page and root pages for every index tree *)
          systemPage :=
            Super.T.getPage(file, IndexTreeOrganization.SystemPageNo);

          IndexPage.Init(systemPage);
          SystemPage.Init(systemPage, noOfTrees);

          FOR i := 1 TO noOfTrees DO
            rootPage := Super.T.getPage(
                          file, i + IndexTreeOrganization.SystemPageNo);
            DataPage.PutAll(
              rootPage, i, depth1 := 0, depth2 := 0, key1 := 0, key2 := 0);
            SystemPage.NotifyDataPageCreation(systemPage, i);
          END;
          file.noOfTrees := noOfTrees;
        ELSE
          (* read no of trees from file *)
          systemPage :=
            Super.T.getPage(file, IndexTreeOrganization.SystemPageNo);
          file.noOfTrees := SystemPage.GetNoOfIndexTrees(systemPage);
          IF file.noOfTrees # noOfTrees THEN RAISE TreeUnknown END;
        END;
      EXCEPT
        SystemPage.InternalError (info) =>
          abort := TRUE;
          RAISE PageFile.NoAccess(
                  "SystemPage.InternalError " & ErrorSupport.ToText(info));
      | VirtualResource.FatalError (info) =>
          abort := TRUE;
          RAISE PageFile.NoAccess("VirtualResource.FatalError "
                                    & ErrorSupport.ToText(info));
      | IndexPage.InternalError (info) =>
          abort := TRUE;
          RAISE PageFile.NoAccess(
                  "IndexPage.InternalError" & ErrorSupport.ToText(info));
      | DataPage.InternalError (info) =>
          abort := TRUE;
          RAISE PageFile.NoAccess(
                  "DataPage.InternalError" & ErrorSupport.ToText(info));
      END;
    FINALLY
      TRY
        IF abort THEN
          VirtualResource.T.abortTransaction(resource);
        ELSE
          VirtualResource.T.commitTransaction(resource);
        END;
      EXCEPT
        VirtualResource.FatalError => (* ignore and propagate the real
                                         fault*)
      END;
    END;
    RETURN file;
  END Open;


PROCEDURE Close (file: T) RAISES {InternalError} =
  VAR
  (* systemPage: SystemPage.T; *)
  (* noOfPages : CARDINAL; *)

  BEGIN
    TRY
      (* systemPage := Super.T.getPage(file,
         IndexTreeOrganization.SystemPageNo); *)
      (* noOfPages := SystemPage.NoOfPhysExistentPages(systemPage); *)
      Super.T.close(file (*, TRUE, noOfPages*));
    EXCEPT
      Super.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "ITPFile.Close", "Super.InternalError", info));
    END;
  END Close;


PROCEDURE NoOfPagesInTree (file: T; indexTree: CARDINAL): CARDINAL
  RAISES {Access.Locked, TreeUnknown, InternalError} =
  VAR systemPage: SystemPage.T;

  BEGIN
    TRY
      IF indexTree > file.noOfTrees THEN RAISE TreeUnknown END;
      systemPage :=
        Super.T.getPage(file, IndexTreeOrganization.SystemPageNo);
      RETURN SystemPage.NoOfDataPagesInTree(systemPage, indexTree);
    EXCEPT
    | SystemPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ITPFile.NoOfPagesInTree",
                                       "SystemPage.InternalError", info));
    END;
  END NoOfPagesInTree;


PROCEDURE FindPage (    file          : T;
                        indexTree     : CARDINAL;
                        key1, key2    : CARDINAL;
                    VAR depth1, depth2: CARDINAL  ): CARDINAL
  RAISES {Access.Locked, TreeUnknown, InternalError} =
  VAR
    currentKey1                                 := key1;
    currentKey2                                 := key2;
    found              : BOOLEAN;
    pageNo             : CARDINAL;
    oldPageNo          : CARDINAL;
    currentPage        : IndexPage.T;
    currentNodePosition: IndexPage.NodePosition;
    currentEntry       : IndexPage.Entry;
    rightNode          : BOOLEAN;

  BEGIN
    IF indexTree > file.noOfTrees THEN RAISE TreeUnknown END;
    (* try cache *)
    found := Super.T.findCacheEntry(
               file, indexTree, key1, key2, pageNo, depth1, depth2);
    IF found THEN RETURN pageNo; END;

    (* no cache entry: traverse index tree *)
    depth1 := 0;
    depth2 := 0;
    TRY
      currentPage :=
        Super.T.getPage(file, IndexTreeOrganization.SystemPageNo);
      currentNodePosition :=
        SystemPage.GetIndexTreeRootNode(currentPage, indexTree);
      currentEntry :=
        IndexPage.GetEntry(currentPage, currentNodePosition, FALSE);
    EXCEPT
      IndexPage.StatusViolation =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ITPFile.FindPage", "IndexPage.StatusViolation"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITPFile.FindPage", "IndexPage.InternalError", info));
    | SystemPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITPFile.FindPage", "SystemPage.InternalError", info));
    END;
    oldPageNo := IndexTreeOrganization.SystemPageNo;

    TRY
      WHILE (ISTYPE(currentEntry, IndexPage.NodeRefEntry)) DO
        WITH entry = NARROW(currentEntry, IndexPage.NodeRefEntry) DO
          IF (entry.splitKey = 1) THEN
            rightNode := (Word.Extract(currentKey1, 0, 1) = 1);
            currentKey1 := Word.RightShift(currentKey1, 1);
            INC(depth1);
          ELSE
            rightNode := (Word.Extract(currentKey2, 0, 1) = 1);
            currentKey2 := Word.RightShift(currentKey2, 1);
            INC(depth2);
          END;

          IF (entry.indexPageNo # oldPageNo) THEN
            currentPage := Super.T.getPage(file, entry.indexPageNo);
            oldPageNo := entry.indexPageNo;
          END;

          currentEntry :=
            IndexPage.GetEntry(currentPage, entry.indexPagePos, rightNode);
        END;
      END;
    EXCEPT
      IndexPage.StatusViolation =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ITPFile.FindPage", "IndexPage.StatusViolation"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITPFile.FindPage", "IndexPage.InternalError", info));
    END;

    (* insert to cache *)
    pageNo := NARROW(currentEntry, IndexPage.PageRefEntry).pageNo;
    Super.T.insertCacheEntry(
      file, indexTree, key1, key2, pageNo, depth1, depth2);
    RETURN pageNo;
  END FindPage;

(* JournalMonitor serves as default monitor for preorderRun *)
PROCEDURE JournalMonitor (tree            : CARDINAL;
                          depth1, depth2  : CARDINAL;
                          relKey1, relKey2: CARDINAL;
                          node            : BOOLEAN;
                          keyOrPage       : CARDINAL  ) =
  BEGIN
    Journal.Add("tree = " & Fmt.Int(tree) & ", d1 = " & Fmt.Int(depth1)
                  & ", d2 = " & Fmt.Int(depth2) & ", rk1 = "
                  & Fmt.Pad(Fmt.Int(relKey1, 2), depth1, '0') & ", rk2 = "
                  & Fmt.Pad(Fmt.Int(relKey2, 2), depth2, '0') & ":");
    IF node THEN
      Journal.Add("  splits along key " & Fmt.Int(keyOrPage));
    ELSE
      Journal.Add("  points to data page no " & Fmt.Int(keyOrPage));
    END;
  END JournalMonitor;

PROCEDURE PreorderRun (file   : T;
                       tree   : CARDINAL;
                       monitor: TraversalMonitor := NIL)
  RAISES {Access.Locked, TreeUnknown, InternalError} =

  PROCEDURE RecPreorder (root          : IndexPage.Entry;
                         depth1, depth2: CARDINAL;
                         rk1, rk2      : CARDINAL         )
    RAISES {Access.Locked, InternalError} =
    BEGIN
      TRY
        IF (ISTYPE(root, IndexPage.NodeRefEntry)) THEN
          WITH entry = NARROW(root, IndexPage.NodeRefEntry) DO
            monitor(tree, depth1, depth2, rk1, rk2, TRUE, entry.splitKey);

            IF (entry.indexPageNo # oldPageNo) THEN
              currentPage := Super.T.getPage(file, entry.indexPageNo);
              oldPageNo := entry.indexPageNo;
            END;

            IF (entry.splitKey = 1) THEN
              (* traverse left subtree *)
              RecPreorder(
                IndexPage.GetEntry(currentPage, entry.indexPagePos, FALSE),
                depth1 + 1, depth2, rk1, rk2);
              rk1 := Word.Insert(rk1, 1, depth1, 1);
              (* traverse left subtree *)
              RecPreorder(
                IndexPage.GetEntry(currentPage, entry.indexPagePos, TRUE),
                depth1 + 1, depth2, rk1, rk2);
            ELSE
              (* traverse left subtree *)
              RecPreorder(
                IndexPage.GetEntry(currentPage, entry.indexPagePos, FALSE),
                depth1, depth2 + 1, rk1, rk2);
              rk2 := Word.Insert(rk2, 1, depth2, 1);
              (* traverse left subtree *)
              RecPreorder(
                IndexPage.GetEntry(currentPage, entry.indexPagePos, TRUE),
                depth1, depth2 + 1, rk1, rk2);
            END;
          END;
        ELSE
          (* page reference *)
          WITH entry = NARROW(root, IndexPage.PageRefEntry) DO
            monitor(tree, depth1, depth2, rk1, rk2, FALSE, entry.pageNo);
          END;
        END

      EXCEPT
        IndexPage.StatusViolation =>
          RAISE InternalError(
                  ErrorSupport.Create(
                    "ITPFile.FindPage", "IndexPage.StatusViolation"));
      | IndexPage.InternalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate(
                    "ITPFile.FindPage", "IndexPage.InternalError", info));
      END;
    END RecPreorder;

  VAR
    oldPageNo          : CARDINAL;
    currentPage        : IndexPage.T;
    currentNodePosition: IndexPage.NodePosition;
    currentEntry       : IndexPage.Entry;
  BEGIN
    IF tree > file.noOfTrees THEN RAISE TreeUnknown END;

    IF monitor = NIL THEN monitor := JournalMonitor; END;

    TRY
      currentPage :=
        Super.T.getPage(file, IndexTreeOrganization.SystemPageNo);
      currentNodePosition :=
        SystemPage.GetIndexTreeRootNode(currentPage, tree);
      currentEntry :=
        IndexPage.GetEntry(currentPage, currentNodePosition, FALSE);
    EXCEPT
      IndexPage.StatusViolation =>
        RAISE
          InternalError(ErrorSupport.Create("ITPFile.PreorderRun",
                                            "IndexPage.StatusViolation"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITPFile.PreorderRun", "IndexPage.InternalError", info));
    | SystemPage.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ITPFile.PreorderRun", "SystemPage.InternalError", info));
    END;
    oldPageNo := IndexTreeOrganization.SystemPageNo;
    RecPreorder(currentEntry, 0, 0, 0, 0);
  END PreorderRun;

PROCEDURE FindBrother (    file    : T;
                           pageNo  : CARDINAL;
                       VAR exists  : BOOLEAN;
                       VAR splitKey: [1 .. 2]  ): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    page           : DataPage.T;
    depth1, depth2 : CARDINAL;
    key1, key2     : CARDINAL;
    indexTree      : CARDINAL;
    father, brother: IndexPage.Entry;

  BEGIN
    TRY
      page := Super.T.getPage(file, pageNo);
      DataPage.GetAll(page, indexTree, depth1, depth2, key1, key2);
      exists := ((depth1 > 0) OR (depth2 > 0));

      IF (exists) THEN
        IndexTreeOrganization.GetFatherAndBrotherEntry(
          file, indexTree, key1, key2, depth1 + depth2, father, brother);
        exists := ISTYPE(brother, IndexPage.PageRefEntry);
        IF (exists) THEN
          splitKey := NARROW(father, IndexPage.NodeRefEntry).splitKey;
          RETURN NARROW(brother, IndexPage.PageRefEntry).pageNo;
        END;
      END;
    EXCEPT
      IndexTreeOrganization.DepthExhausted =>
        RAISE InternalError(ErrorSupport.Create("ITPFile.FindBrother",
                                                "ITPFile.DepthExhausted"));
    | IndexTreeOrganization.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "ITPFile.FindBrother",
                          "IndexTreeOrganization.InternalError", info));
    | DataPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITPFile.FindBrother", "DataPage.InternalError", info));
    END;

    RETURN 0;
  END FindBrother;


PROCEDURE GetPage (file: T; pageNo: CARDINAL; VAR depth1, depth2: CARDINAL; ):
  VirtualPage.T RAISES {Access.Locked, InternalError} =
  VAR page: DataPage.T;

  BEGIN
    page := Super.T.getPage(file, pageNo);
    TRY
      DataPage.GetDepths(page, depth1, depth2);
    EXCEPT
    | DataPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ITPFile.GetPage", "DataPage.InternalError", info));
    END;
    RETURN page;
  END GetPage;


PROCEDURE SplitPage (    file                  : T;
                         dataPageNo            : CARDINAL;
                         indexTree             : CARDINAL;
                         splitKey              : [1 .. 2];
                     VAR newPage1No, newPage2No: CARDINAL;
                     VAR oldPage               : VirtualPage.T;
                     VAR newPage1, newPage2    : VirtualPage.T  )
  RAISES {Access.Locked, TreeUnknown, WrongPageReference, DepthExhausted,
          InternalError} =
  BEGIN
    TRY
      IF indexTree > file.noOfTrees THEN RAISE TreeUnknown END;
      IndexTreeOrganization.SplitDataPage(
        file, dataPageNo, indexTree, splitKey, newPage1No, newPage2No,
        oldPage, newPage1, newPage2);
    EXCEPT
      IndexTreeOrganization.WrongPageReference => RAISE WrongPageReference;
    | IndexTreeOrganization.DepthExhausted => RAISE DepthExhausted;
    | IndexTreeOrganization.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "ITPFile.SplitPage",
                          "IndexTreeOrganization.InternalError", info));
    END;
  END SplitPage;


PROCEDURE MergePage (    file                  : T;
                         oldPage1No, oldPage2No: CARDINAL;
                         indexTree             : CARDINAL;
                         splitKey              : [1 .. 2];
                     VAR newPageNo             : CARDINAL;
                     VAR oldPage1, oldPage2    : VirtualPage.T;
                     VAR newPage               : VirtualPage.T  )
  RAISES {Access.Locked, TreeUnknown, WrongPageReference, InternalError} =
  BEGIN
    TRY
      IF indexTree > file.noOfTrees THEN RAISE TreeUnknown END;
      IndexTreeOrganization.MergeDataPage(
        file, oldPage1No, oldPage2No, indexTree, splitKey, newPageNo,
        oldPage1, oldPage2, newPage);
    EXCEPT
      IndexTreeOrganization.WrongPageReference => RAISE WrongPageReference;
    | IndexTreeOrganization.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "ITPFile.MergePage",
                          "IndexTreeOrganization.InternalError", info));
    | IndexTreeOrganization.PageBufferFault =>
        RAISE InternalError(ErrorSupport.Create(
                              "ITPFile.MergePage",
                              "IndexTreeOrganization.PageBufferFault"));
    END;
  END MergePage;

BEGIN
END ITPFile.
