GENERIC MODULE RecordStorage(StoragePage, RecordParameter, TreeParameter);

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.2  1997/11/21 09:38:52  roland
    Bugfix in BrotherPageKey. Higher bits must be set to 0.

    Revision 1.1  1997/03/26 11:27:52  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.40  1997/02/20 16:21:59  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.39  1996/11/20 12:23:42  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.38  1996/08/06 16:29:07  roland
    Merge of PAGESERVER and main branch.

    Revision 1.37.2.2  1996/07/24 09:17:12  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.37.2.1  1996/04/29 13:56:11  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.37  1994/01/19  11:22:27  pk
# Treshold for splitting after merging set to 100% page usage.
#
# Revision 1.36  1993/12/31  17:14:04  pk
# Error in MergePages corrected: the original location.pageNo might get
# out of date when the current page is swapped.
#
# Revision 1.35  1993/12/15  11:29:20  pk
# Error in GetNextInRange corrected: maxNoOfRecords variable has to be
# updated after switching to brother page.
#
# Revision 1.34  1993/12/13  12:42:07  pk
# Error in GetNextInRange corrected: page variable has to be updated
# after switching to brother page.
#
# Revision 1.33  1993/12/13  12:39:17  pk
# Error in BrotherPageKey corrected.
#
# Revision 1.32  1993/11/24  19:12:23  pk
# Error in SelectPage corrected: new key segments may only be determined
# if a new page was selected.
#
# Revision 1.31  1993/11/19  16:00:26  pk
# Again, recursive split calls corrected.
#
# Revision 1.30  1993/11/19  11:34:30  pk
# Error in recursive split calls corrected.
#
# Revision 1.29  1993/11/15  12:06:45  pk
# Adaptions to new StoragePage with InitAsMerge/SplitTarget.
#
# Revision 1.28  1993/11/09  21:32:08  pk
# Error corrected: correct pageNos set in locations.
#
# Revision 1.27  1993/11/09  21:04:30  pk
# Handling of location updates changed.
#
# Revision 1.26  1993/11/09  19:49:52  pk
# Error corrected: after merging, it is legal for the location record to
# be not on the target page (it might be a dummy).
#
# Revision 1.25  1993/11/09  19:47:14  pk
# Error corrected in computing the no. of records on a merge target
# page.
#
# Revision 1.24  1993/11/09  17:06:29  pk
# Error corrected: MergePages has to make sure that all source records
# fit onto the page.
#
# Revision 1.23  1993/11/09  16:58:47  pk
# Error corrected: MoveRecord forgot to compute pageKeys.
#
# Revision 1.22  1993/11/09  14:35:32  pk
# Error corrected: wrong parameter profile for MoveRecords.
#
# Revision 1.21  1993/11/03  21:37:20  pk
# PutAttribute checks for attribute # NIL.
#
# Revision 1.20  1993/11/03  21:20:58  pk
# Adaptions to new merge/split methods in ITPFiles.
#
# Revision 1.19  1993/11/03  19:38:09  pk
# MIN-expression for calculating range size replaced by IF-statement to
# avoid overflow.
#
# Revision 1.18  1993/10/31  17:29:12  pk
# Another error corrected in calculating range sizes.
#
# Revision 1.17  1993/10/29  17:45:42  pk
# New error check in Split when updating the location record.
#
# Revision 1.16  1993/10/28  16:17:24  pk
# Error corrected: Split took the wrong bit to decide where the
# location record went after DistributeRecords.
#
# Revision 1.15  1993/10/26  20:13:18  pk
# Naming of methods and procedures updated.
#
# Revision 1.14  1993/10/21  14:18:24  pk
# Typo in DeleteRecord messed up collision list updating.
#
# Revision 1.13  1993/10/19  18:31:24  pk
# Another stupid error in DistributeRecords corrected.
#
# Revision 1.12  1993/10/19  16:31:22  pk
# Stupid syntax error corrected.
#
# Revision 1.11  1993/10/19  16:26:30  pk
# GetFirstSignificantBit optimized.
#
# Revision 1.10  1993/10/19  14:44:35  pk
# Error corrected: available bitsize for CARDINAL is 31, not
# BITSIZE(CARDINAL).
#
# Revision 1.9  1993/10/09  21:20:19  pk
# Yet another MIN/MAX error corrected.
#
# Revision 1.8  1993/10/09  00:55:36  pk
# Error corrected: BrotherPageKey handles case where depth is 0.
#
# Revision 1.7  1993/10/09  00:16:16  pk
# Last error really corrected.
#
# Revision 1.6  1993/10/09  00:01:10  pk
# Error in intializing range sizes corrected (at least one record has to
# be considered).
#
# Revision 1.5  1993/10/08  23:56:06  pk
# Error in initializing pageNo of ranges corrected.
#
# Revision 1.4  1993/10/08  15:26:33  pk
# Error in computing BitMask corrected.
#
# Revision 1.3  1993/10/04  22:12:13  pk
# MaxElements passed through to RecordStorage.i3
#
# Revision 1.2  1993/10/02  18:54:15  pk
# Parameter to LocateNeighbour changed from location to file.
#
# Revision 1.1  1993/10/02  15:59:07  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Text, Word;
IMPORT ErrorSupport, Type, Access, ITPFile;


CONST
  SplitUsage = 85;               (* split page if usage exceeds 85% *)
  MergeUsage = 70;               (* merge pages if usage is below 70% *)


PROCEDURE Init (file: ITPFile.T; tree: CARDINAL)
  RAISES {Access.Locked, LevelError, InternalError} =
  VAR
    rootPage      : StoragePage.T;
    rootPageNo    : CARDINAL;
    depth1, depth2: CARDINAL;

  BEGIN
    TRY
      rootPageNo := file.findPage(tree, key1 := 0, key2 := 0,
                                  depth1 := depth1, depth2 := depth2);
      rootPage := file.getPage(rootPageNo, depth1, depth2);
      (* RSLevelError: wrong initialization of index tree *)
      IF NOT ((depth1 = 0) AND (depth2 = 0)) THEN RAISE LevelError END;

      StoragePage.Init(rootPage, StoragePage.defaultNoOfRecords);
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RecordStorage.Init", "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "RecordStorage.Init", "StoragePage.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "RecordStorage.Init", "ITPFile.TreeUnknown"));
    END;
  END Init;


PROCEDURE PutRecord (VAR      location: RecordLocation;
                     READONLY key1    : RecordParameter.Key1;
                     READONLY key2    : RecordParameter.Key2  )
  RAISES {Access.Locked, NotFree, InternalError} =
  VAR
    page          : StoragePage.T;
    depth1, depth2: CARDINAL;
    recordKey1    : RecordParameter.Key1;
    recordKey2    : RecordParameter.Key2;
    emptyKey      : BOOLEAN;

  BEGIN
    TRY
      page := location.file.getPage(location.pageNo, depth1, depth2);
      StoragePage.GetKeys(
        page, location.recordNo, recordKey1, recordKey2, emptyKey);

      (* RSNotFree: try to overwrite used memory? *)
      IF NOT emptyKey THEN RAISE NotFree END;

      StoragePage.PutKeys(page, location.recordNo, key1, key2);
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.PutRecord",
                                       "StoragePage.InternalError", info));
    | ITPFile.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "RecordStorage.PutRecord",
                              "ITPFile.InternalError", info));
    END;
    SplitPage(location, mandatory := FALSE);
  END PutRecord;


PROCEDURE DeleteRecord (READONLY location: RecordLocation)
  RAISES {Access.Locked, InternalError} =
  VAR
    page                    : StoragePage.T;
    depth1, depth2          : CARDINAL;
    maxNoOfRecords          : CARDINAL;
    fromRecordNo, toRecordNo: CARDINAL;
    key1                    : RecordParameter.Key1;
    key2                    : RecordParameter.Key2;
    pageKey1, pageKey2      : CARDINAL;
    hashedRecordNo          : CARDINAL;
    move                    : BOOLEAN;
    emptyKey                : BOOLEAN;

  BEGIN
    TRY
      page := location.file.getPage(location.pageNo, depth1, depth2);
      StoragePage.DeleteEntry(page, location.recordNo);
      maxNoOfRecords := StoragePage.MaxNoOfRecords(page);

      (* update linear collision list *)
      fromRecordNo := location.recordNo;
      toRecordNo := location.recordNo;
      LOOP
        (* consider next record *)
        fromRecordNo := (fromRecordNo MOD maxNoOfRecords) + 1;
        StoragePage.GetKeys(page, fromRecordNo, key1, key2, emptyKey);
        IF (emptyKey) THEN EXIT; END;

        (* determine where the record should have been *)
        TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
        pageKey1 := Word.RightShift(pageKey1, depth1);
        pageKey2 := Word.RightShift(pageKey2, depth2);
        hashedRecordNo := Hash(page, pageKey1, pageKey2);

        (* the record has to be moved if it's original location is below
           it's actual location: linear probing moved it past the current
           location *)
        move := (hashedRecordNo <= toRecordNo);
        (* check cases where probing or from pointer cycled *)
        IF (toRecordNo < fromRecordNo) THEN
          move := move OR (hashedRecordNo > fromRecordNo);
        ELSE
          move := move AND (hashedRecordNo > fromRecordNo);
        END;

        IF (move) THEN
          StoragePage.MoveEntry(page, fromRecordNo, toRecordNo);
          toRecordNo := fromRecordNo;
        END;
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.DeleteRecord",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.DeleteRecord",
                                       "StoragePage.InternalError", info));
    END;
    MergePages(location);
  END DeleteRecord;


PROCEDURE FindRecord (         file : ITPFile.T;
                               tree : CARDINAL;
                      READONLY key1 : RecordParameter.Key1;
                      READONLY key2 : RecordParameter.Key2;
                      VAR      found: BOOLEAN               ):
  RecordLocation RAISES {Access.Locked, InternalError} =
  VAR
    location          : RecordLocation;
    pageKey1, pageKey2: CARDINAL;
    depth1, depth2    : CARDINAL;
    page              : StoragePage.T;
  BEGIN
    location.file := file;
    location.tree := tree;
    TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
    TRY
      location.pageNo :=
        file.findPage(location.tree, pageKey1, pageKey2, depth1, depth2);
      page := file.getPage(location.pageNo, depth1, depth2);
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.FindRecord",
                                       "ITPFile.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE InternalError(ErrorSupport.Create("RecordStorage.FindRecord",
                                                "ITPFile.TreeUnknown"));
    END;
    location.recordNo :=
      SearchRecordOnPage(
        page, key1, key2, pageKey1, pageKey2, depth1, depth2, found);
    RETURN location;
  END FindRecord;


PROCEDURE LocateNeighbourPosition (    file: ITPFile.T;
                                       tree: CARDINAL;
                                   VAR key1: RecordParameter.Key1;
                                   VAR key2: RecordParameter.Key2  ):
  RecordLocation RAISES {Access.Locked, InternalError} =
  VAR
    page                   : StoragePage.T;
    pageKey1, pageKey2     : CARDINAL;
    depth1, depth2         : CARDINAL;
    newLocation            : RecordLocation;
    highKeyPart, lowKeyPart: CARDINAL;
    found                  : BOOLEAN;
    proposedRecordNo       : CARDINAL;
    maxNoOfRecords         : CARDINAL;
    addressableRecords     : CARDINAL;
    maxRecords             : CARDINAL;

  BEGIN
    TRY
      TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
      pageKey2 := 0;
      newLocation.file := file;
      newLocation.tree := tree;

      LOOP
        IF (pageKey1 = 0) THEN
          (* 0 is not allowed, take something very close to 0 *)
          pageKey1 := Word.LeftShift(1, BITSIZE(CARDINAL) - 2);
        END;

        newLocation.pageNo := newLocation.file.findPage(
                                tree, pageKey1, pageKey2, depth1, depth2);

        page :=
          newLocation.file.getPage(newLocation.pageNo, depth1, depth2);

        (* split key: the bits in lowKeyPart are equal for all records on
           this page *)
        highKeyPart := Word.RightShift(pageKey1, depth1);
        lowKeyPart := Word.Extract(pageKey1, 0, depth1);

        (* try the key proposed by pageKey *)
        TreeParameter.ComputeRecordKeys(key1, key2, pageKey1, pageKey2);
        newLocation.recordNo :=
          SearchRecordOnPage(
            page, key1, key2, pageKey1, pageKey2, depth1, depth2, found);
        (* ok, free position found -> we're done *)
        IF (NOT found) THEN EXIT; END;

        proposedRecordNo := newLocation.recordNo;
        maxNoOfRecords := StoragePage.MaxNoOfRecords(page);
        IF (StoragePage.Usage(page) > MergeUsage) THEN
          SelectPage(
            newLocation.file, newLocation.tree, newLocation.pageNo,
            highKeyPart, lowKeyPart, depth1, depth2);
          page :=
            newLocation.file.getPage(newLocation.pageNo, depth1, depth2);
          maxNoOfRecords := StoragePage.MaxNoOfRecords(page);
          proposedRecordNo := 1;
        END;

        addressableRecords :=
          Word.LeftShift(1, BITSIZE(CARDINAL) - 1 - MAX(1, depth1));
        maxRecords := MIN(maxNoOfRecords, addressableRecords);

        InitSearchPosition(maxRecords, proposedRecordNo);
        newLocation.recordNo :=
          GetSearchPosition(
            page, maxRecords, maxNoOfRecords, addressableRecords);

        (* assemble the key *)
        IF ((newLocation.recordNo = 1) AND (lowKeyPart = 0)) THEN
          highKeyPart := maxNoOfRecords;
        ELSE
          highKeyPart := newLocation.recordNo - 1;
        END;
        IF (highKeyPart >= addressableRecords) THEN
          pageKey1 := lowKeyPart;
          BrotherPageKey(pageKey1, depth1);
        ELSE
          pageKey1 := Word.LeftShift(highKeyPart, depth1) + lowKeyPart;
          TreeParameter.ComputeRecordKeys(key1, key2, pageKey1, pageKey2);
          EXIT;
        END;
      END;
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "RecordStorage.LocateNeighbourPosition",
                              "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "RecordStorage.LocateNeighbourPosition",
                              "StoragePage.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE InternalError(ErrorSupport.Create(
                              "RecordStorage.LocateNeighbourPosition",
                              "ITPFile.TreeUnknown"));
    END;
    RETURN newLocation;
  END LocateNeighbourPosition;


PROCEDURE GetKeys (READONLY location: RecordLocation;
                   VAR      key1    : RecordParameter.Key1;
                   VAR      key2    : RecordParameter.Key2  )
  RAISES {Access.Locked, InternalError} =
  VAR
    depth1, depth2: CARDINAL;
    emptyKey      : BOOLEAN;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        StoragePage.GetKeys(page, location.recordNo, key1, key2, emptyKey);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RecordStorage.GetKeys", "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetKeys",
                                       "StoragePage.InternalError", info));
    END;
  END GetKeys;


PROCEDURE PutData (READONLY location: RecordLocation;
                   READONLY data    : RecordParameter.Data)
  RAISES {Access.Locked, InternalError} =
  VAR depth1, depth2: CARDINAL;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        StoragePage.PutData(page, location.recordNo, data);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RecordStorage.PutData", "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.PutData",
                                       "StoragePage.InternalError", info));
    END;
  END PutData;

PROCEDURE GetData (READONLY location: RecordLocation): RecordParameter.Data
  RAISES {Access.Locked, InternalError} =
  VAR depth1, depth2: CARDINAL;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        RETURN StoragePage.GetData(page, location.recordNo);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RecordStorage.GetData", "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetData",
                                       "StoragePage.InternalError", info));
    END;
  END GetData;


PROCEDURE PutAttribute (VAR location: RecordLocation; attribute: TEXT)
  RAISES {Access.Locked, InternalError} =
  VAR
    page          : StoragePage.T;
    overflow      : BOOLEAN;
    depth1, depth2: CARDINAL;

  BEGIN
    TRY
      LOOP
        page := location.file.getPage(location.pageNo, depth1, depth2);
        StoragePage.PutAttribute(
          page, location.recordNo, attribute, overflow);
        IF (NOT overflow) THEN EXIT; END;
        SplitPage(
          location, keepFree := Text.Length(attribute), mandatory := TRUE);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.PutAttribute",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.PutAttribute",
                                       "StoragePage.InternalError", info));
    END;
    SplitPage(location, mandatory := FALSE);
  END PutAttribute;


PROCEDURE GetAttribute (READONLY location: RecordLocation): TEXT
  RAISES {Access.Locked, InternalError} =
  VAR depth1, depth2: CARDINAL;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        RETURN StoragePage.GetAttribute(page, location.recordNo);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetAttribute",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetAttribute",
                                       "StoragePage.InternalError", info));
    END;
  END GetAttribute;


PROCEDURE PutSetElement (VAR location : RecordLocation;
                         VAR element  : CARDINAL;
                         VAR displaced: BOOLEAN         )
  RAISES {Access.Locked, NotFree, InternalError} =
  VAR
    page          : StoragePage.T;
    depth1, depth2: CARDINAL;
    existent      : BOOLEAN;
    overflow      : BOOLEAN;

  BEGIN
    TRY
      LOOP
        page := location.file.getPage(location.pageNo, depth1, depth2);
        StoragePage.PutSetElement(
          page, location.recordNo, element, existent, overflow, displaced);
        (* RSNotFree: try to overwrite used memory? *)
        IF existent THEN RAISE NotFree END;
        IF (NOT overflow) THEN EXIT; END;
        WITH newCard = StoragePage.SetCardinality(page, location.recordNo)
                         + 1 DO
          SplitPage(location, keepFree := newCard * BYTESIZE(CARDINAL),
                    mandatory := TRUE);
        END;
      END;
      SplitPage(location, mandatory := FALSE);
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.PutSetElement",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.PutSetElement",
                                       "StoragePage.InternalError", info));
    END;
  END PutSetElement;


PROCEDURE GetSetElement (READONLY location : RecordLocation;
                                  elementNo: CARDINAL        ): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR depth1, depth2: CARDINAL;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        RETURN
          StoragePage.GetSetElement(page, location.recordNo, elementNo);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetSetElement",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetSetElement",
                                       "StoragePage.InternalError", info));
    END;
  END GetSetElement;


PROCEDURE DeleteSetElement (VAR location: RecordLocation; element: CARDINAL)
  RAISES {Access.Locked, NotFound, InternalError} =
  VAR
    page          : StoragePage.T;
    depth1, depth2: CARDINAL;
    key1          : RecordParameter.Key1;
    key2          : RecordParameter.Key2;
    found         : BOOLEAN;
    emptyKey      : BOOLEAN;
  BEGIN
    TRY
      page := location.file.getPage(location.pageNo, depth1, depth2);
      StoragePage.GetKeys(page, location.recordNo, key1, key2, emptyKey);
      StoragePage.DeleteSetElement(page, location.recordNo, element);
      MergePages(location);

      (* relocate record *)
      IF (NOT emptyKey) THEN
        location :=
          FindRecord(location.file, location.tree, key1, key2, found);
        (* RSNotFound *)
        IF NOT found THEN RAISE NotFound END;
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.DeleteSetElement",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.DeleteSetElement",
                                       "StoragePage.InternalError", info));
    END;
  END DeleteSetElement;


PROCEDURE SetCardinality (READONLY location: RecordLocation): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR depth1, depth2: CARDINAL;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        RETURN StoragePage.SetCardinality(page, location.recordNo);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SetCardinality",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SetCardinality",
                                       "StoragePage.InternalError", info));
    END;
  END SetCardinality;


PROCEDURE IsElementInSet (READONLY location: RecordLocation;
                                   element : CARDINAL        ): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  VAR depth1, depth2: CARDINAL;

  BEGIN
    TRY
      WITH page = location.file.getPage(location.pageNo, depth1, depth2) DO
        RETURN
          StoragePage.IsElementInSet(page, location.recordNo, element);
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.IsElementInSet",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.IsElementInSet",
                                       "StoragePage.InternalError", info));
    END;
  END IsElementInSet;


PROCEDURE InitRange (         file: ITPFile.T;
                              tree: CARDINAL;
                     READONLY key1: RecordParameter.Key1): Range
  RAISES {Access.Locked, InternalError} =
  VAR
    range   : Range;
    pageKey2: CARDINAL;
    key2    : RecordParameter.Key2;

  BEGIN
    range.file := file;
    range.tree := tree;
    range.key1 := key1;
    TreeParameter.ComputePageKeys(key1, key2, range.pageKey1, pageKey2);
    range.pageKey2 := 0;
    SetRangeRecordNo(range);
    RETURN range;
  END InitRange;


PROCEDURE ResetRange (VAR range: Range)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    range.pageKey2 := 0;
    SetRangeRecordNo(range);
  END ResetRange;


PROCEDURE SearchInRange (VAR      range: Range;
                         READONLY key2 : RecordParameter.Key2;
                         VAR      found: BOOLEAN               ):
  RecordLocation RAISES {Access.Locked, InternalError} =
  VAR
    location          : RecordLocation;
    page              : StoragePage.T;
    pageKey1, pageKey2: CARDINAL;
    depth1, depth2    : CARDINAL;

  BEGIN
    location.file := range.file;
    location.tree := range.tree;
    TreeParameter.ComputePageKeys(range.key1, key2, pageKey1, pageKey2);
    TRY
      location.pageNo := location.file.findPage(
                           range.tree, pageKey1, pageKey2, depth1, depth2);
      page := location.file.getPage(location.pageNo, depth1, depth2);
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SearchInRange",
                                       "ITPFile.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE
          InternalError(ErrorSupport.Create("RecordStorage.SearchInRange",
                                            "ITPFile.TreeUnknown"));
    END;
    location.recordNo :=
      SearchRecordOnPage(
        page, range.key1, key2, pageKey1, pageKey2, depth1, depth2, found);
    RETURN location;
  END SearchInRange;


PROCEDURE GetNextInRange (VAR range: Range; VAR found: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  VAR
    page          : StoragePage.T;
    maxNoOfRecords: CARDINAL;
    lastRecordNo  : CARDINAL;
    key1          : RecordParameter.Key1;
    key2          : RecordParameter.Key2;
    depth1, depth2: CARDINAL;
    emptyKey      : BOOLEAN;

  BEGIN
    TRY
      found := FALSE;
      page := range.file.getPage(range.pageNo, depth1, depth2);
      maxNoOfRecords := StoragePage.MaxNoOfRecords(page);

      LOOP
        lastRecordNo := range.recordNo - 1;
        IF (lastRecordNo = 0) THEN lastRecordNo := maxNoOfRecords; END;
        WHILE ((range.size > 0) AND (range.recordNo # lastRecordNo)) DO
          DEC(range.size);
          range.recordNo := (range.recordNo MOD maxNoOfRecords) + 1;
          StoragePage.GetKeys(page, range.recordNo, key1, key2, emptyKey);
          IF (NOT emptyKey) THEN
            IF (range.size = 0) THEN range.size := 1; END;
            IF (key1 = range.key1) THEN found := TRUE; RETURN; END;
          END;
        END;

        (* continue searching on brother page regarding key2 *)
        BrotherPageKey(range.pageKey2, depth2);
        IF (range.pageKey2 = 0) THEN
          range.pageKey2 := LAST(CARDINAL);
          EXIT;
        ELSE
          SetRangeRecordNo(range);
          page := range.file.getPage(range.pageNo, depth1, depth2);
          maxNoOfRecords := StoragePage.MaxNoOfRecords(page);
        END;
      END;
    EXCEPT
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetNextInRange",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetNextInRange",
                                       "StoragePage.InternalError", info));
    END;
  END GetNextInRange;


PROCEDURE GetCurrentFromRange (READONLY range: Range): RecordLocation =
  VAR location: RecordLocation;

  BEGIN
    location.file := range.file;
    location.tree := range.tree;
    location.pageNo := range.pageNo;
    location.recordNo := range.recordNo;
    RETURN location;
  END GetCurrentFromRange;


(* Merge the page where the location record is on with it's brother. *)
PROCEDURE MergePages (READONLY location: RecordLocation)
  RAISES {Access.Locked, InternalError} =
  VAR
    page              : StoragePage.T;
    brotherPage       : StoragePage.T;
    newPage           : StoragePage.T;
    oldPage1, oldPage2: StoragePage.T;
    usage             : CARDINAL;
    brotherUsage      : CARDINAL;
    key1              : RecordParameter.Key1;
    key2              : RecordParameter.Key2;
    depth1, depth2    : CARDINAL;
    brotherPageNo     : CARDINAL;
    newPageNo         : CARDINAL;
    exists            : BOOLEAN;
    splitKey          : [1 .. 2];
    dummyLocation                            := location;
    currentPageNo                            := location.pageNo;

  BEGIN
    TRY
      page := location.file.getPage(currentPageNo, depth1, depth2);
      usage := StoragePage.Usage(page);

      (* repeat merging until usage is tolerable (50% is the expected
         overall usage) *)
      WHILE (usage <= (MergeUsage - 50)) DO
        brotherPageNo :=
          location.file.findBrother(currentPageNo, exists, splitKey);
        IF (NOT exists) THEN EXIT; END;

        brotherPage :=
          location.file.getPage(brotherPageNo, depth1, depth2);
        brotherUsage := StoragePage.Usage(brotherPage);
        IF (usage + brotherUsage > MergeUsage) THEN EXIT; END;

        (* merge pages in index tree and collect records *)
        IF (splitKey = 1) THEN DEC(depth1); ELSE DEC(depth2); END;
        location.file.mergePage(
          currentPageNo, brotherPageNo, location.tree, splitKey, newPageNo,
          oldPage1, oldPage2, newPage);
        StoragePage.InitAsMergeTarget(oldPage1, oldPage2, newPage);
        MoveRecords(oldPage1, newPage, depth1, depth2);
        MoveRecords(oldPage2, newPage, depth1, depth2);

        currentPageNo := newPageNo;
        usage := StoragePage.Usage(newPage);
      END;

      (* split if necessary to avoid completely full pages *)
      IF (usage = 100) THEN
        dummyLocation.pageNo := currentPageNo;
        Split(newPage, dummyLocation, depth1 := depth1, depth2 := depth2,
              key1 := key1, key2 := key2, updateLocation := FALSE);
      END;
    EXCEPT
      KeyLengthExceeded =>
        RAISE InternalError(
                ErrorSupport.Create("RecordStorage.MergePages",
                                    "RecordStorage.KeyLengthExceeded"));
    | ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.MergePages",
                                       "ITPFile.InternalError", info));
    | ITPFile.WrongPageReference =>
        RAISE
          InternalError(ErrorSupport.Create("RecordStorage.MergePages",
                                            "ITPFile.WrongPageReference"));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.MergePages",
                                       "StoragePage.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE InternalError(ErrorSupport.Create("RecordStorage.MergePages",
                                                "ITPFile.TreeUnknown"));
    END;
  END MergePages;


(* Move all records on a page to another page. *)
PROCEDURE MoveRecords (oldPage, newPage: StoragePage.T;
                       depth1, depth2  : CARDINAL       )
  RAISES {Access.Locked, InternalError} =
  VAR
    key1              : RecordParameter.Key1;
    key2              : RecordParameter.Key2;
    emptyKey          : BOOLEAN;
    newRecordNo       : CARDINAL;
    pageKey1, pageKey2: CARDINAL;
    found             : BOOLEAN;

  BEGIN
    TRY
      FOR recordNo := 1 TO StoragePage.MaxNoOfRecords(oldPage) DO
        StoragePage.GetKeys(oldPage, recordNo, key1, key2, emptyKey);
        IF (NOT emptyKey) THEN
          TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
          newRecordNo :=
            SearchRecordOnPage(newPage, key1, key2, pageKey1, pageKey2,
                               depth1, depth2, found);
          (* RSNotFree: try to overwrite used memory? *)
          IF found THEN
            RAISE InternalError(ErrorSupport.Create(
                                  "RecordStorage.MoveRecords", "NotFree"))
          END;
          MoveRecord(oldPage, newPage, recordNo, newRecordNo);
        END;
      END;
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.MoveRecords",
                                       "StoragePage.InternalError", info));
    END;
  END MoveRecords;


(* Split the page on which the record in location lives.  location is
   updated, i.e.  it will point to the new record location after splitting.
   keepFree denotes a no.  of bytes which should be free on the new pages.
   If mandatory is TRUE, the page is split anyway; otherwise, the page is
   only split if the usage is to high. *)
PROCEDURE SplitPage (VAR location : RecordLocation;
                         keepFree                    := 0;
                         mandatory: BOOLEAN                )
  RAISES {Access.Locked, InternalError} =
  VAR
    page          : StoragePage.T;
    depth1, depth2: CARDINAL;
    key1          : RecordParameter.Key1;
    key2          : RecordParameter.Key2;
    emptyKey      : BOOLEAN;

  BEGIN
    TRY
      page := location.file.getPage(location.pageNo, depth1, depth2);
      IF ((NOT mandatory) AND (StoragePage.Usage(page) <= SplitUsage)) THEN
        RETURN;
      END;

      StoragePage.GetKeys(page, location.recordNo, key1, key2, emptyKey);
      Split(page, location, keepFree, depth1, depth2, key1, key2,
            updateLocation := TRUE);
    EXCEPT
      KeyLengthExceeded =>
        RAISE InternalError(
                ErrorSupport.Create("RecordStorage.SplitPage",
                                    "RecordStorage.KeyLengthExceeded"));
    | ITPFile.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "RecordStorage.SplitPage",
                              "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SplitPage",
                                       "StoragePage.InternalError", info));
    END;
  END SplitPage;


(* Split a page with the given depths.  location has to refer to a record
   with the given keys on this page.  If updateLocation is TRUE, the
   location is updated, i.e.  it will point to the new record location
   after splitting. *)
PROCEDURE Split (         page          : StoragePage.T;
                 VAR      location      : RecordLocation;
                          keepFree                               := 0;
                          depth1, depth2: CARDINAL;
                 READONLY key1          : RecordParameter.Key1;
                 READONLY key2          : RecordParameter.Key2;
                          updateLocation: BOOLEAN                      )
  RAISES {Access.Locked, KeyLengthExceeded, InternalError} =
  VAR
    splitKey          : [1 .. 2];
    page1No, page2No  : CARDINAL;
    page1, page2      : StoragePage.T;
    oldPage           : StoragePage.T;
    pageKey1, pageKey2: CARDINAL;
    recordIsOnPage1   : BOOLEAN;
    brotherLocation   : RecordLocation;
    found             : BOOLEAN;

  BEGIN
    TRY
      (* split the page in the index tree and move the records *)
      splitKey := CalculateSplitKey(page, depth1, depth2);
      location.file.splitPage(location.pageNo, location.tree, splitKey,
                              page1No, page2No, oldPage, page1, page2);
      DistributeRecords(
        oldPage, page1, page2, depth1, depth2, splitKey, keepFree);

      (* update location *)
      brotherLocation := location;
      IF (updateLocation) THEN
        TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
        IF (splitKey = 1) THEN
          recordIsOnPage1 := (Word.Extract(pageKey1, depth1, 1) = 0);
          INC(depth1);
        ELSE
          recordIsOnPage1 := (Word.Extract(pageKey2, depth2, 1) = 0);
          INC(depth2);
        END;

        IF (recordIsOnPage1) THEN
          location.recordNo := SearchRecordOnPage(
                                 page1, key1, key2, pageKey1, pageKey2,
                                 depth1, depth2, found);
          (* RSNotFound: object could not be found? *)
          IF NOT found THEN
            RAISE InternalError(
                    ErrorSupport.Create("RecordStorage.Split", "NotFound"))
          END;
          location.pageNo := page1No;
          brotherLocation.pageNo := page2No;
        ELSE
          location.recordNo := SearchRecordOnPage(
                                 page2, key1, key2, pageKey1, pageKey2,
                                 depth1, depth2, found);
          (* RSNotFound: object could not be found? *)
          IF NOT found THEN
            RAISE InternalError(
                    ErrorSupport.Create("RecordStorage.Split", "NotFound"))
          END;
          location.pageNo := page2No;
          brotherLocation.pageNo := page1No;
        END;
      ELSE
        (* don't update location: use arbitrary values for new locations *)
        location.pageNo := page1No;
        brotherLocation.pageNo := page2No;
        recordIsOnPage1 := TRUE;

        (* update depths *)
        IF (splitKey = 1) THEN INC(depth1); ELSE INC(depth2); END;
      END;

      (* recursive call if usage is still to high *)
      WITH usagePage1 = StoragePage.Usage(page1),
           usagePage2 = StoragePage.Usage(page2)  DO
        IF (recordIsOnPage1) THEN
          IF (usagePage1 >= SplitUsage) THEN
            Split(page1, location, keepFree, depth1, depth2, key1, key2,
                  updateLocation);
          END;
          IF (usagePage2 >= SplitUsage) THEN
            Split(page2, brotherLocation, keepFree, depth1, depth2, key1,
                  key2, updateLocation := FALSE);
          END;
        ELSE
          IF (usagePage1 >= SplitUsage) THEN
            Split(page1, brotherLocation, keepFree, depth1, depth2, key1,
                  key2, updateLocation := FALSE);
          END;
          IF (usagePage2 >= SplitUsage) THEN
            Split(page2, location, keepFree, depth1, depth2, key1, key2,
                  updateLocation);
          END;
        END;
      END;
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "RecordStorage.Split", "ITPFile.InternalError", info));
    | ITPFile.WrongPageReference =>
        RAISE
          InternalError(ErrorSupport.Create("RecordStorage.Split",
                                            "ITPFile.WrongPageReference"));
    | ITPFile.DepthExhausted =>
        RAISE InternalError(ErrorSupport.Create("RecordStorage.Split",
                                                "ITPFile.DepthExhausted"));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "RecordStorage.Split",
                              "StoragePage.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "RecordStorage.Split", "ITPFile.TreeUnknown"));
    END;
  END Split;

EXCEPTION KeyLengthExceeded;

(* Computes according to which key the page should be splitted. *)
PROCEDURE CalculateSplitKey (page: StoragePage.T; depth1, depth2: CARDINAL):
  CARDINAL RAISES {Access.Locked, KeyLengthExceeded, InternalError} =

  (* Determine the first bit at which keyA and keyB differ. *)
  PROCEDURE GetFirstSignificantBit (keyA, keyB: CARDINAL): CARDINAL
    RAISES {} =
    VAR bit := 0;

    BEGIN
      IF (keyA = keyB) THEN RETURN BITSIZE(CARDINAL) - 1; END;
      WHILE ((bit <= BITSIZE(CARDINAL) - 1) AND (Word.Extract(keyA, bit, 1)
                                                   = Word.Extract(
                                                       keyB, bit, 1))) DO
        INC(bit);
      END;
      RETURN bit;
    END GetFirstSignificantBit;

  CONST
    PreferKey1 = 6;              (* key1 is preferred until more than
                                    PreferKey1 bits are equal *)

  VAR
    significantBitsKey1 := BITSIZE(CARDINAL);
    significantBitsKey2 := BITSIZE(CARDINAL);
    newSignificantBitsKey1, newSignificantBitsKey2: CARDINAL;
    key1                                          : RecordParameter.Key1;
    key2                                          : RecordParameter.Key2;
    emptyKey                                      : BOOLEAN;
    firstPageKey1, firstPageKey2                  : CARDINAL;
    pageKey1, pageKey2                            : CARDINAL;

  BEGIN
    (* check if we have no choice because one of the keys has no more bits
       to distinguish *)
    WITH depth1Exceeded = ((RecordParameter.Key1Size = 0)
                             OR (depth1 >= BITSIZE(RecordParameter.Key1))
                             OR (depth1 >= BITSIZE(CARDINAL))),
         depth2Exceeded = ((RecordParameter.Key2Size = 0)
                             OR (depth2 >= BITSIZE(RecordParameter.Key2))
                             OR (depth2 >= BITSIZE(CARDINAL))) DO
      IF (depth1Exceeded) THEN
        (* RSKeyLengthExceeded: index tree to deep to be addressable *)
        IF depth2Exceeded THEN RAISE KeyLengthExceeded END;
        RETURN 2;
      ELSIF (depth2Exceeded) THEN
        RETURN 1;
      END;
    END;

    TRY
      (* determine minimal bit position which distinguishes at least one
         record *)
      StoragePage.GetKeys(page, recordNo := 1, key1 := key1, key2 := key2,
                          emptyKey := emptyKey);
      TreeParameter.ComputePageKeys(
        key1, key2, firstPageKey1, firstPageKey2);

      FOR recordNo := 2 TO StoragePage.MaxNoOfRecords(page) DO
        StoragePage.GetKeys(page, recordNo, key1, key2, emptyKey);
        IF (NOT emptyKey) THEN
          TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
          newSignificantBitsKey1 :=
            GetFirstSignificantBit(firstPageKey1, pageKey1);
          IF (newSignificantBitsKey1 <= PreferKey1) THEN RETURN 1; END;
          newSignificantBitsKey2 :=
            GetFirstSignificantBit(firstPageKey2, pageKey2);
          significantBitsKey1 :=
            MIN(significantBitsKey1, newSignificantBitsKey1);
          significantBitsKey2 :=
            MIN(significantBitsKey2, newSignificantBitsKey2);
        END;
      END;
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.CalculateSplitKey",
                                       "StoragePage.InternalError", info));
    END;
    (* the key is returned which provides the fastest access to a
       distinguishing bit (key1 is preferred up to some bits) *)
    IF (significantBitsKey1 = BITSIZE(CARDINAL)) THEN RETURN 2; END;

    IF (significantBitsKey1 <= PreferKey1 + significantBitsKey2) THEN
      RETURN 1;
    ELSE
      RETURN 2;
    END;
  END CalculateSplitKey;


(* Distribute the records on old page to the new pages according to split
   key.  keepFree denotes a no.  of extra bytes which should remain unused
   on the new pages. *)
PROCEDURE DistributeRecords (oldPage, page1, page2: StoragePage.T;
                             depth1, depth2       : CARDINAL;
                             splitKey             : [1 .. 2];
                             keepFree             : CARDINAL       )
  RAISES {Access.Locked, InternalError} =
  VAR
    targetPage          : StoragePage.T;
    key1                : RecordParameter.Key1;
    key2                : RecordParameter.Key2;
    emptyKey            : BOOLEAN;
    newDepth1, newDepth2: CARDINAL;
    pageKey1, pageKey2  : CARDINAL;
    recordOnPage1       : BOOLEAN;
    newRecordNo         : CARDINAL;
    found               : BOOLEAN;

  BEGIN
    TRY
      (* set up new pages *)
      StoragePage.InitAsSplitTarget(oldPage, page1, keepFree);
      StoragePage.InitAsSplitTarget(oldPage, page2, keepFree);

      (* loop through all records on the source page *)
      FOR recordNo := 1 TO StoragePage.MaxNoOfRecords(oldPage) DO
        StoragePage.GetKeys(oldPage, recordNo, key1, key2, emptyKey);
        IF (NOT emptyKey) THEN
          TreeParameter.ComputePageKeys(key1, key2, pageKey1, pageKey2);
          newDepth1 := depth1;
          newDepth2 := depth2;

          (* compute to which target page the record goes *)
          IF (splitKey = 1) THEN
            recordOnPage1 := (Word.Extract(pageKey1, depth1, 1) = 0);
            newDepth1 := depth1 + 1;
            newDepth2 := depth2;
          ELSE
            recordOnPage1 := (Word.Extract(pageKey2, depth2, 1) = 0);
            newDepth1 := depth1;
            newDepth2 := depth2 + 1;
          END;

          IF (recordOnPage1) THEN
            targetPage := page1;
          ELSE
            targetPage := page2;
          END;

          (* calculate target record no.  and move record there *)
          newRecordNo :=
            SearchRecordOnPage(targetPage, key1, key2, pageKey1, pageKey2,
                               newDepth1, newDepth2, found);

          (* RSNotFree: try to overwrite used memory? *)
          IF found THEN
            RAISE InternalError(
                    ErrorSupport.Create(
                      "RecordStorage.DistributeRecords", "NotFree"))
          END;

          MoveRecord(oldPage, targetPage, recordNo, newRecordNo);
        END;
      END;
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.DistributeRecords",
                                       "StoragePage.InternalError", info));
    END;
  END DistributeRecords;


(* Move (i.e.  copy and delete) a record including set or attribute data
   from one page to another. *)
PROCEDURE MoveRecord (sourcePage, targetPage    : StoragePage.T;
                      sourceRecord, targetRecord: CARDINAL       )
  RAISES {Access.Locked, InternalError} =
  VAR
    key1    : RecordParameter.Key1;
    key2    : RecordParameter.Key2;
    data    : RecordParameter.Data;
    emptyKey: BOOLEAN;
    overflow: BOOLEAN;

  BEGIN
    TRY
      (* copy keys *)
      StoragePage.GetKeys(sourcePage, sourceRecord, key1, key2, emptyKey);
      StoragePage.PutKeys(targetPage, targetRecord, key1, key2);

      (* copy data *)
      IF (RecordParameter.DataSize > 0) THEN
        data := StoragePage.GetData(sourcePage, sourceRecord);
        StoragePage.PutData(targetPage, targetRecord, data);
      END;

      IF (RecordParameter.HasAttribute) THEN

        (* copy attribute *)
        IF (StoragePage.AttributeLength(sourcePage, sourceRecord) > 0) THEN
          VAR attribute: TEXT;
          (**        <* FATAL StoragePage.NoAttribute *> *)
          BEGIN
            attribute :=
              StoragePage.GetAttribute(sourcePage, sourceRecord);
            StoragePage.PutAttribute(
              targetPage, targetRecord, attribute, overflow);
          END;
        END;
        (* RSOverflow: page overflow? *)
        IF overflow THEN
          RAISE InternalError(ErrorSupport.Create(
                                "RecordStorage.MoveRecord", "NotFound"))
        END;

      ELSIF (RecordParameter.HasSet) THEN

        (* copy set *)
        IF (StoragePage.SetCardinality(sourcePage, sourceRecord) > 0) THEN
          VAR set: REF Type.ByteArray;
          BEGIN
            set := StoragePage.GetSet(sourcePage, sourceRecord);
            StoragePage.PutSet(targetPage, targetRecord, set, overflow);
          END;

          (* RSOverflow: page overflow? *)
          IF overflow THEN
            RAISE InternalError(ErrorSupport.Create(
                                  "RecordStorage.MoveRecord", "NotFound"))
          END;
        END;
      END;

      (* delete source record *)
      StoragePage.DeleteEntry(sourcePage, sourceRecord);
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.MoveRecord",
                                       "StoragePage.InternalError", info));
    END;
  END MoveRecord;

(* Try to locate a record with given keys on the page.  If a record with
   these keys is on the page, found is set to TRUE and the record no.  is
   returned.  Otherwise, found is set to FALSE and a free position where to
   store a record with these keys is returned. *)
PROCEDURE SearchRecordOnPage (         page: StoragePage.T;
                              READONLY key1: RecordParameter.Key1;
                              READONLY key2: RecordParameter.Key2;
                                  pageKey1, pageKey2: CARDINAL;
                                  depth1, depth2    : CARDINAL;
                              VAR found             : BOOLEAN   ): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    recordNo        : CARDINAL;
    firstRecord     : CARDINAL;
    maxRecordsOnPage: CARDINAL;
    recordKey1      : RecordParameter.Key1;
    recordKey2      : RecordParameter.Key2;
    emptyKey        : BOOLEAN;

  BEGIN
    found := FALSE;
    pageKey1 := Word.RightShift(pageKey1, depth1);
    pageKey2 := Word.RightShift(pageKey2, depth2);

    TRY
      (* compute a start value for the search using the hash function *)
      recordNo := Hash(page, pageKey1, pageKey2);
      firstRecord := recordNo;
      maxRecordsOnPage := StoragePage.MaxNoOfRecords(page);

      (* linear sonding until the record or a free position is found *)
      REPEAT
        StoragePage.GetKeys(
          page, recordNo, recordKey1, recordKey2, emptyKey);
        IF (emptyKey) THEN
          RETURN recordNo;
        ELSIF ((recordKey1 = key1) AND (recordKey2 = key2)) THEN
          found := TRUE;
          RETURN recordNo;
        END;

        recordNo := (recordNo MOD maxRecordsOnPage) + 1;
      UNTIL (recordNo = firstRecord);
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SearchRecordOnPage",
                                       "StoragePage.InternalError", info));
    END;

    (* RSNotFound: object could not be found! *)
    RAISE InternalError(ErrorSupport.Create(
                          "RecordStorage.SearchRecordOnPage", "NotFound"));
  END SearchRecordOnPage;


VAR
  proposedLowKey: CARDINAL := 0; (* where to start the SelectPage search *)

(* Try to find a page which is not too deep in the index tree. *)
PROCEDURE SelectPage (    file                   : ITPFile.T;
                          tree                   : CARDINAL;
                      VAR pageNo                 : CARDINAL;
                      VAR highKeyPart, lowKeyPart: CARDINAL;
                      VAR depth1, depth2         : CARDINAL   )
  RAISES {Access.Locked, InternalError} =
  VAR
    maxPagesWithNextLevel: CARDINAL;
    levelCount                      := 1;
    maxPages             : CARDINAL;
    originalPageNo                  := pageNo;

  BEGIN
    TRY
      maxPagesWithNextLevel := 2 * file.noOfPagesInTree(tree) + 1;
      maxPages := Word.LeftShift(1, depth1) - 1;
      WHILE (maxPages >= maxPagesWithNextLevel) DO
        INC(levelCount);

        (* RSInternalError: some algorithm didn't work *)
        IF NOT (levelCount < maxPagesWithNextLevel) THEN
          RAISE InternalError(ErrorSupport.Create(
                                "RecordStorage.SelectPage", "NotFound"))
        END;

        (* check at which depth a page with proposedLowKey is *)
        pageNo := file.findPage(tree, proposedLowKey, 0, depth1, depth2);

        maxPages := Word.LeftShift(1, depth1) - 1;
        IF (maxPages >= maxPagesWithNextLevel) THEN
          (* too deep: try a brother page *)
          BrotherPageKey(proposedLowKey, depth1);
        END;
      END;

      (* adapt key segments *)
      IF (originalPageNo # pageNo) THEN
        highKeyPart := Word.RightShift(proposedLowKey, depth1);
        lowKeyPart := Word.Extract(proposedLowKey, 0, depth1);
      END;
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SelectPage",
                                       "ITPFile.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE InternalError(ErrorSupport.Create("RecordStorage.SelectPage",
                                                "ITPFile.TreeUnknown"));
    END;
  END SelectPage;


VAR
  searchPosition := 1;           (* where to start search in
                                    LocateNeighbourPosition *)
  lastProposedRecordNo := 0;
  searchDelta          := 64;
  seqSearchPosition    := 1;

(* Initialize search position for LocateNeighbourPosition. *)
PROCEDURE InitSearchPosition (maxRecords      : CARDINAL;
                              proposedRecordNo: CARDINAL  ) =
  BEGIN
    IF (searchPosition > maxRecords) THEN searchPosition := 1; END;
    IF (lastProposedRecordNo # proposedRecordNo) THEN
      lastProposedRecordNo := proposedRecordNo;
      IF (((searchPosition MOD searchDelta)
             # (proposedRecordNo MOD searchDelta)) OR (searchDelta <= 1)) THEN
        (* last search position was calculated for another search,
           discard *)
        searchPosition := proposedRecordNo;
        searchDelta := 64;
      END;
    ELSIF (searchDelta >= 2) THEN
      (* the same root no.  as last time, start somewhere else *)
      searchDelta := Word.RightShift(searchDelta, 1);
    ELSIF ((seqSearchPosition + 5 >= maxRecords)
             OR (seqSearchPosition * 2 <= maxRecords)) THEN
      searchPosition := Word.RightShift(maxRecords, 1);
    ELSE
      searchPosition := seqSearchPosition + 5;
    END;

    IF (searchPosition * 2 > maxRecords) THEN
      WHILE ((searchDelta > 1) AND (searchPosition <= searchDelta)) DO
        searchDelta := Word.RightShift(searchDelta, 1);
      END;
      DEC(searchPosition, searchDelta);
    ELSE
      WHILE ((searchDelta > 1)
               AND (searchPosition + searchDelta > maxRecords)) DO
        searchDelta := Word.RightShift(searchDelta, 1);
      END;
      INC(searchPosition, searchDelta);
    END;
  END InitSearchPosition;


(* Return a recordNo for LocateNeighbourPosition. *)
PROCEDURE GetSearchPosition (page              : StoragePage.T;
                             maxRecords        : CARDINAL;
                             maxNoOfRecords    : CARDINAL;
                             addressableRecords: CARDINAL       ): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    found   : BOOLEAN;
    recordNo: CARDINAL;
    emptyKey: BOOLEAN;
    key1    : RecordParameter.Key1;
    key2    : RecordParameter.Key2;

  BEGIN
    found := FALSE;
    recordNo := searchPosition;
    TRY
      LOOP
        StoragePage.GetKeys(page, recordNo, key1, key2, emptyKey);
        IF (emptyKey) THEN RETURN recordNo; END;
        IF (searchDelta > 1) THEN
          REPEAT
            searchDelta := Word.RightShift(searchDelta, 1);
          UNTIL
            ((recordNo + searchDelta <= maxRecords) OR (searchDelta = 1));
          IF (searchDelta > 1) THEN
            INC(recordNo, searchDelta);
          ELSE
            recordNo := seqSearchPosition + 4;
            IF (recordNo > maxRecords) THEN recordNo := 1; END;
          END;
          searchPosition := recordNo;
        ELSE
          INC(recordNo);
          IF (recordNo > maxRecords) THEN recordNo := 1; END;
          seqSearchPosition := recordNo;
          IF (recordNo = searchPosition) THEN
            (* RSInternalError: some algorithm didn't work *)
            IF NOT (addressableRecords <= maxNoOfRecords) THEN
              RAISE InternalError(
                      ErrorSupport.Create(
                        "RecordStorage.GetSearchPosition", "NotFound"))
            END;

            RETURN maxNoOfRecords;
          END;
        END;
      END;
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.GetSearchPosition",
                                       "StoragePage.InternalError", info));
    END;
  END GetSearchPosition;


(* Compute the key for a record on a brother page in the index tree. *)
PROCEDURE BrotherPageKey (VAR key: CARDINAL; depth: CARDINAL) =
  VAR exp: CARDINAL;
  BEGIN
    IF (depth = 0) THEN
      (* surely no brother page *)
      key := 0;
      RETURN;
    END;

    (* eliminate bits which don't contribute to this depth *)
    exp := Word.LeftShift(1, depth);
    key := key MOD exp;
    exp := exp DIV 2;

    (* get the last zero bit before depth and set all more significant '1' to 0 *)
    WHILE (depth > 0) AND (Word.And(key, exp) > 0) DO
      DEC(depth);
      DEC(key, exp);
      exp := exp DIV 2;
    END;

    IF (depth = 0) THEN
      (* all bits were set *)
      key := 0;
    ELSE
      (* set the bit *)
      INC(key, exp);
    END;
  END BrotherPageKey;


(* Initialize the record no.  in a range to the hashed value - 1. *)
PROCEDURE SetRangeRecordNo (VAR range: Range)
  RAISES {Access.Locked, InternalError} =
  VAR
    page          : StoragePage.T;
    depth1, depth2: CARDINAL;
    pageKey1      : CARDINAL;
    maxNoOfRecords: CARDINAL;

  BEGIN
    TRY
      range.pageNo := range.file.findPage(range.tree, range.pageKey1,
                                          range.pageKey2, depth1, depth2);
      page := range.file.getPage(range.pageNo, depth1, depth2);
      maxNoOfRecords := StoragePage.MaxNoOfRecords(page);
      pageKey1 := Word.RightShift(range.pageKey1, depth1);
      range.recordNo := Hash(page, pageKey1, 0) - 1;
      IF (range.recordNo = 0) THEN range.recordNo := maxNoOfRecords END;
      (* determine how many record positions have to be considered in the
         range: since key2 is truncated by the hash mask, there can be only
         a limited number of record positions with the same key1 and
         different key2 values *)
      IF (TreeParameter.Key2HashMask >= maxNoOfRecords) THEN
        range.size := maxNoOfRecords;
      ELSIF TreeParameter.Key2HashMask = 0 THEN
        range.size := maxNoOfRecords;
      ELSE
        range.size := TreeParameter.Key2HashMask + 1;
        (* range fault cannot occur since bound by maxNoOfRecords *)
      END;
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SetRangeRecordNo",
                                       "ITPFile.InternalError", info));
    | StoragePage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("RecordStorage.SetRangeRecordNo",
                                       "StoragePage.InternalError", info));
    | ITPFile.TreeUnknown =>
        RAISE
          InternalError(
            ErrorSupport.Create(
              "RecordStorage.SetRangeRecordNo", "ITPFile.TreeUnknown"));
    END;
  END SetRangeRecordNo;

(* Compute a hash value where to store a record with given page keys. *)
PROCEDURE Hash (page: StoragePage.T; pageKey1, pageKey2: CARDINAL):
  CARDINAL RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH noOfRecords = StoragePage.MaxNoOfRecords(page) DO
        IF TreeParameter.Key2HashMask = 0 THEN
          (* Use all of key 2 *)
          RETURN ((pageKey1 * 2 + pageKey2) MOD noOfRecords) + 1;
        ELSIF TreeParameter.Key2HashMask < LAST(CARDINAL) THEN
          (* Use only key2 mod TreeParameter.Key2HashMask *)
          RETURN ((pageKey1 * (TreeParameter.Key2HashMask + 1)
                     + (pageKey2 MOD TreeParameter.Key2HashMask))
                    MOD noOfRecords) + 1;
        ELSE
          (* ignore key 2 *)
          RETURN (pageKey1 MOD noOfRecords) + 1;
        END;
      END;
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "RecordStorage.Hash", "StoragePage.InternalError", info));
    END;
  END Hash;

(**
(* Compute a hash value where to store a record with given page keys. *)
PROCEDURE Hash (page: StoragePage.T; pageKey1, pageKey2: CARDINAL):
  CARDINAL RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH noOfRecords = StoragePage.MaxNoOfRecords(page),
           mask        = MIN(TreeParameter.Key2HashMask, noOfRecords) + 1 DO
        RETURN ((pageKey1 + (pageKey2 MOD mask)) MOD noOfRecords) + 1;
      END;
    EXCEPT
    | StoragePage.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "RecordStorage.Hash", "StoragePage.InternalError", info));
    END;
  END Hash;
*)
BEGIN
  MaxElements := StoragePage.MaxElements;
END RecordStorage.
