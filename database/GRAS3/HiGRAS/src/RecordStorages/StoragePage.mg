GENERIC MODULE StoragePage(PageFrame);

(***************************************************************************)
(** A storage page is a data page with typed access to storage of data
    records. A page is organized as follows (the page indices are generic):

  Byte                                     Entry

                            |==============header==============|
  1                         | reserved for data page           |
                            | information                      |
                            |----------------------------------|
  staticSize ..             | size of static page part         |
  staticSize+3              |                                  |
                            |----------------------------------|
  staticEntries ..          | no. of entries in static page    |
  staticEntries+3           | part                             |
                            |----------------------------------|
  dynamicOverhead ..        | no. of allocated but unused      |
  dynamicOverhead+3         | bytes in dynamic page part       |
                            |----------------------------------|
  unusedStart ..            | pointer to first free byte in    |
  unusedStart+3             | dynamic page part                |
                            |=========static page part=========|
  staticStart               | entry 1                          |
                            |----------------------------------|
  .                         |                .                 |
  .                         |                .                 |
  .                         |                .                 |
                            |----------------------------------|
  .. + (staticEntries)      | entry n                          |
                            |----------------------------------|
                            | free                             |
                            |========dynamic page part=========|
  (staticSize)*recordSize   | attribute/set 1                  |
  .. + staticStart          |                                  |
                            |----------------------------------|
  .                         |                .                 |
  .                         |                .                 |
  .                         |                .                 |
                            |----------------------------------|
                            | attribute/set m                  |
                            |----------------------------------|
  (unusedStart) ..          | free                             |
  Size                      |                                  |
                            |==================================|


  Each entry is stored as follows (the pointers are generic):

                            |==================================|
  Key1Start ..              | key 1                            |
  .. + Key1Size-1           |                                  |
                            |----------------------------------|
  Key2Start ..              | key 2                            |
  .. + Key2Size-1           |                                  |
                            |==================================|
  DataStart ..              | entry data                       |
  .. + DataSize-1           |                                  |
                            |==================================|
  if HasAttribute:
                            |==================================|
  AttributePointer          | pointer to attribute data        |
                            | (= 0 if undefined or length = 0) |
                            |----------------------------------|
  AttributeLength           | length of attribute              |
                            |==================================|
  if HasSet:
                            |==================================|
  SetPointer                | pointer to set data or the       |
                            | element itself if card is 1      |
                            |----------------------------------|
  SetCard                   | no. of elements in set           |
                            |==================================|


  Attributes and sets are stored as follows (constants are generic):

                            |==============header==============|
  RecordNumber ..           | no. of the record the attribute/ |
  .. + 2                    | set belongs to                   |
                            |----------------------------------|
  AllocatedLength ..        | no. of allocated elements   .    |
  .. + 1                    |                                  |
                            |===============data===============|
  AttSetData ..             | actual attribute/set data        |
  .. + (AttributeLength)-1  |                                  |
                            |----------------------------------|
  .. + (AllocatedLength)-1  | free                             |
                            |==================================|
  *)

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.3  1997/11/19 17:59:31  roland
    Removed grouping of page accesses.

    Revision 1.2  1997/09/18 08:23:24  roland
    Grouping of access to the same page.

    Revision 1.1  1997/03/26 11:27:56  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.37  1997/02/20 16:22:02  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.36  1996/11/20 12:23:46  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.35  1996/08/06 16:29:22  roland
    Merge of PAGESERVER and main branch.

    Revision 1.34.2.2  1996/07/24 09:17:17  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.34.2.1  1996/04/29 13:56:21  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.34  1995/07/04  15:46:05  roland
# Error in AllocateSetAtt removed: Upper bound for allocation is
# MaxElements.
#
# Revision 1.33  1994/01/03  15:30:53  pk
# Changed OptNoOfRecords to keep at least 10% of the overall page size
# free for the dynamic part.
#
# Revision 1.32  1993/12/02  16:25:03  pk
# InitAsSplit/MergeTarget now consider minNoOfRecords into
# optimalNoOfRecords to avoid unnecessary Overflow exceptions.
#
# Revision 1.31  1993/12/02  11:59:43  pk
# Error corrected in DeleteSetElement: before moving the elements behind
# the deleted one, a check is made if the last element was deleted.
#
# Revision 1.30  1993/11/19  11:31:41  pk
# Error corrected when releasing a set/attribute from the dynamic page
# part: Only the current no. of elements are new overhead, since the
# allocated but unused elements are already counted as overhead.
#
# Revision 1.29  1993/11/18  10:23:40  pk
# Some minor syntactical changes.
#
# Revision 1.28  1993/11/15  19:34:55  pk
# Error corrected in PutSetElement: the set contents must be saved
# before reallocating.
# Computation in InitAsMergeTarget changed.
#
# Revision 1.27  1993/11/15  18:33:40  pk
# Error in computing new overhead after PutSetElement corrected.
#
# Revision 1.26  1993/11/15  13:17:08  pk
# Typing error in error condition in InitAsSplitTarget corrected.
#
# Revision 1.25  1993/11/15  12:20:26  pk
# AllocateSetAtt doesn't try to allocate AvgSize elements to avoid
# compressions when merging/splitting.
#
# Revision 1.24  1993/11/15  12:05:22  pk
# Optimal/CurrentNoOfRecords replaced by InitAsMerge/SplitTarget.
# CompressSets/Attributes now reduces the overhead really to 0.
#
# Revision 1.23  1993/11/14  17:10:10  pk
# Another error in AllocateSetAtt corrected: lower bound for the no. of
# elements we want to allocate is the no. of elements we have to
# allocate.
#
# Revision 1.22  1993/11/14  16:59:22  pk
# Error in AllocateSetAtt corrected: wrong value for maximum number of
# free elements on the page was computed.
# Also, on first allocation, PageFrame.AvgSize wasn't respected.
#
# Revision 1.21  1993/11/12  14:04:13  pk
# InsertElement now checks if there are elements to copy to avoid range
# errors.
#
# Revision 1.20  1993/11/12  13:55:53  pk
# Another error in OptimalNoOfRecords corrected: now it can handle the
# case where no records are on the page.
#
# Revision 1.19  1993/11/10  14:27:55  pk
# Error corrected: OptimalNoOfRecords didn't dereference staticSize pointer.
#
# Revision 1.18  1993/11/09  19:45:24  pk
# New procedure CurrentNoOfRecords.
#
# Revision 1.17  1993/11/09  18:17:31  pk
# Time and again: MIN/MAX error corrected.
#
# Revision 1.16  1993/11/09  14:30:54  pk
# Error corrected: DeleteEntry didn't add AttSetHeaderLength to the new
# overhead.
#
# Revision 1.15  1993/11/09  14:25:56  pk
# Init procedure optimized.
#
# Revision 1.14  1993/11/08  19:55:21  pk
# Error corrected: DeleteSetElement didn't reconvert to compressed
# representation when deleting the last but one element.
#
# Revision 1.13  1993/11/05  17:34:14  pk
# Silly syntactic error corrected.
#
# Revision 1.12  1993/11/05  17:32:26  pk
# Error corrected in DeleteEntry: the procedure checks on the existance
# of an attribute/set in the dynamic page part when updating the
# overhead.
# Some comments added.
#
# Revision 1.11  1993/11/05  17:04:31  pk
# PutKeys initializes Set/Attribute Pointer/Length to 0.
#
# Revision 1.10  1993/11/03  21:37:23  pk
# PutAttribute checks for attribute # NIL.
#
# Revision 1.9  1993/10/29  17:48:14  pk
# Error corrected when re-/storing sets from byte arrays.
#
# Revision 1.8  1993/10/27  19:36:13  pk
# Error corrected: on PutSetElement, allocatedCard instead setCard was
# stored.
#
# Revision 1.7  1993/10/26  20:13:24  pk
# Naming of methods and procedures updated.
#
# Revision 1.6  1993/10/19  18:33:48  pk
# Some checks concerning the recordNo removed; they couldn't be
# processed ordinary since recordNo's are not continuous.
#
# Revision 1.5  1993/10/19  16:27:32  pk
# Error corrected: forgotten to dereference PageFrame.staticSize.
#
# Revision 1.4  1993/10/08  23:57:10  pk
# Error in computing empty keys corrected.
#
# Revision 1.3  1993/10/08  15:27:32  pk
# Error in initializing emptyRecord corrected.
#
# Revision 1.2  1993/10/04  22:12:17  pk
# MaxElements passed through to RecordStorage.i3
#
# Revision 1.1  1993/10/02  15:59:34  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Text, Type, PageData, Access, BitCompareWord;
IMPORT VirtualPage, ErrorSupport, RecordBase;

CONST SetElementSize = BYTESIZE(CARDINAL);


VAR emptyRecord := NEW(REF Type.ByteArray, PageFrame.recordLength);


PROCEDURE Init (page: T; noOfRecords: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  CONST EmptyPage = ARRAY PageData.Index OF Type.Byte{0, ..};

  BEGIN
    TRY
      page.putInt(PageFrame.staticSize, noOfRecords);
      page.putInt(PageFrame.staticEntries, 0);
      page.putInt(PageFrame.dynamicOverhead, 0);
      WITH staticByteSize = PageFrame.recordLength * noOfRecords DO
        page.putInt(
          PageFrame.unusedStart, PageFrame.staticStart + staticByteSize);
        page.putArray(
          PageFrame.staticStart, SUBARRAY(EmptyPage, 0, staticByteSize));
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "StoragePage.Init", "VirtualPage.FatalError", info));
    END;
  END Init;


PROCEDURE InitAsMergeTarget (sourcePage1, sourcePage2: T; targetPage: T)
  RAISES {Access.Locked, InternalError} =
  VAR noOfRecords: CARDINAL;

  BEGIN
    TRY
      WITH
        (* compute how many bytes must fit into the dynamic section *)
        dynamicBytesOnPage1 = DynamicByteUsage(sourcePage1),
        dynamicBytesOnPage2 = DynamicByteUsage(sourcePage2),
        dynamicNoOfBytes    = dynamicBytesOnPage1 + dynamicBytesOnPage2,

        (* how many records can be allocated now *)
        totalSize           = PageFrame.Size - PageFrame.staticStart + 1,
        staticAvailable     = totalSize - dynamicNoOfBytes,
        possibleNoOfRecords = staticAvailable DIV PageFrame.recordLength,

        (* all source records must fit onto the target page *)
        noOfRecordsPage1 = sourcePage1.getInt(PageFrame.staticEntries),
        noOfRecordsPage2 = sourcePage2.getInt(PageFrame.staticEntries),
        minNoOfRecords   = noOfRecordsPage1 + noOfRecordsPage2,

        (* compute optimal page layout *)
        optimalNoOfRecordsPage1 = OptimalNoOfRecords(sourcePage1),
        optimalNoOfRecordsPage2 = OptimalNoOfRecords(sourcePage2),
        avgOptimalNoOfRecords = (optimalNoOfRecordsPage1
                                   + optimalNoOfRecordsPage2) DIV 2,
        optimalNoOfRecords  = MAX(avgOptimalNoOfRecords, minNoOfRecords),
        optimalStaticUsage  = optimalNoOfRecords * PageFrame.recordLength,
        optimalDynamicUsage = totalSize - optimalStaticUsage               DO

        (* check if optimal page layout is possible *)
        noOfRecords := optimalNoOfRecords;
        IF (optimalDynamicUsage < dynamicNoOfBytes) THEN
          noOfRecords := possibleNoOfRecords;
          (* SPOverflow: parameter is too large? *)
          <* ASSERT (minNoOfRecords <= noOfRecords) *>
        END;

        Init(targetPage, noOfRecords);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.InitAsMergeTarget",
                                       "VirtualPage.FatalError", info));
    END;
  END InitAsMergeTarget;


PROCEDURE InitAsSplitTarget (sourcePage: T;
                             targetPage: T;
                             keepFree  : CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR noOfRecords: CARDINAL;

  BEGIN
    TRY
      IF (keepFree > 0) THEN
        (* the new attribute/set will need a header *)
        INC(keepFree, PageFrame.AttSetHeaderLength);
      END;

      WITH
        (* compute how many bytes must fit into the dynamic section *)
        dynamicNoOfBytes = DynamicByteUsage(sourcePage) + keepFree,

        (* how many records can be allocated now *)
        totalSize           = PageFrame.Size - PageFrame.staticStart + 1,
        staticAvailable     = totalSize - dynamicNoOfBytes,
        possibleNoOfRecords = staticAvailable DIV PageFrame.recordLength,

        (* all source records must fit onto the target page *)
        minNoOfRecords = sourcePage.getInt(PageFrame.staticEntries),

        (* compute optimal page layout *)
        newOptimalNoOfRecords = OptimalNoOfRecords(sourcePage),
        optimalNoOfRecords    = MAX(newOptimalNoOfRecords, minNoOfRecords),
        optimalStaticUsage  = optimalNoOfRecords * PageFrame.recordLength,
        optimalDynamicUsage = totalSize - optimalStaticUsage               DO

        (* check if optimal page layout is possible *)
        noOfRecords := optimalNoOfRecords;
        IF (optimalDynamicUsage < dynamicNoOfBytes) THEN
          noOfRecords := possibleNoOfRecords;
          (* SPOverflow: parameter is too large? *)
          <* ASSERT (minNoOfRecords <= noOfRecords) *>
        END;

        Init(targetPage, noOfRecords);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.InitAsSplitTarget",
                                       "VirtualPage.FatalError", info));
    END;
  END InitAsSplitTarget;


PROCEDURE MaxNoOfRecords (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN page.getInt(PageFrame.staticSize);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.MaxNoOfRecords",
                                       "VirtualPage.FatalError", info));
    END;
  END MaxNoOfRecords;


PROCEDURE Usage (page: T): [0 .. 100]
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH staticSize  = page.getInt(PageFrame.staticSize),
           staticUsed  = page.getInt(PageFrame.staticEntries),
           staticUsage = staticUsed * 100 DIV staticSize,

           dynamicStart = PageFrame.staticStart
                            + PageFrame.recordLength * staticSize,
           dynamicSize = PageFrame.Size - dynamicStart + 1,
           dynamicUsed = page.getInt(PageFrame.unusedStart) - dynamicStart
                           - page.getInt(PageFrame.dynamicOverhead),
           dynamicUsage = dynamicUsed * 100 DIV dynamicSize DO
        RETURN MAX(staticUsage, dynamicUsage);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "StoragePage.Usage", "VirtualPage.FatalError", info));
    END;
  END Usage;


PROCEDURE PutKeys (         page    : T;
                            recordNo: CARDINAL;
                   READONLY key1    : PageFrame.Key1;
                   READONLY key2    : PageFrame.Key2  )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH used = page.getInt(PageFrame.staticEntries) DO
        (* SPOverflow: parameter is too large? *)
        <* ASSERT (used < page.getInt(PageFrame.staticSize)) *>
        page.putInt(PageFrame.staticEntries, used + 1);
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength DO
        PageFrame.PutKeys(page, offset, key1, key2);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "StoragePage.PutKeys", "VirtualPage.FatalError", info));
    | RecordBase.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "StoragePage.PutKeys", "RecordBase.InternalError", info));
    END;
  END PutKeys;


PROCEDURE GetKeys (    page    : T;
                       recordNo: CARDINAL;
                   VAR key1    : PageFrame.Key1;
                   VAR key2    : PageFrame.Key2;
                   VAR emptyKey: BOOLEAN         )
  RAISES {Access.Locked, InternalError} =
  VAR emptyKey1, emptyKey2: BOOLEAN := TRUE;

  BEGIN
    TRY
      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength DO
        PageFrame.GetKeys(page, offset, key1, emptyKey1, key2, emptyKey2);
      END;
      emptyKey := (emptyKey1 AND emptyKey2);
    EXCEPT
    | RecordBase.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "StoragePage.GetKeys", "RecordBase.InternalError", info));
    END;
  END GetKeys;


PROCEDURE PutData (         page    : T;
                            recordNo: CARDINAL;
                   READONLY data    : PageFrame.Data)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF PageFrame.DataSize = 0 THEN
        <* ASSERT (FALSE) *>
      END;
      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength DO
        PageFrame.PutData(page, PageFrame.DataStart + offset, data);
      END;
    EXCEPT
    | RecordBase.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "StoragePage.PutData", "RecordBase.InternalError", info));
    END;
  END PutData;


PROCEDURE GetData (page: T; recordNo: CARDINAL): PageFrame.Data
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF PageFrame.DataSize = 0 THEN
        <* ASSERT (FALSE) *>
      END;
      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength DO
        RETURN PageFrame.GetData(page, PageFrame.DataStart + offset);
      END;
    EXCEPT
    | RecordBase.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "StoragePage.GetData", "RecordBase.InternalError", info));
    END;
  END GetData;


PROCEDURE DeleteEntry (page: T; recordNo: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH noOfStaticEntries = page.getInt(PageFrame.staticEntries) DO
        WITH offset = PageFrame.staticStart
                        + (recordNo - 1) * PageFrame.recordLength DO

          (* mark attribute/set space occupied by the entry as overhead *)
          IF (PageFrame.HasAttribute) THEN
            WITH                 (* attributePointerIndex =
                                    PageFrame.AttributePointer + offset,*)
              attributeLengthIndex = PageFrame.AttributeLength + offset,
              attributeLength      = page.getByte(attributeLengthIndex)  DO
              IF (attributeLength > 0) THEN
                WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
                  page.putInt(PageFrame.dynamicOverhead,
                              overhead + attributeLength
                                + PageFrame.AttSetHeaderLength);
                END;
              END;
            END;
          ELSIF (PageFrame.HasSet) THEN
            WITH                 (* setPointerIndex = PageFrame.SetPointer
                                    + offset,*)
              setCardIndex = PageFrame.SetCard + offset,
              setCard      = page.getByte(setCardIndex)  DO
              IF (setCard > 1) THEN
                WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
                  page.putInt(PageFrame.dynamicOverhead,
                              overhead + setCard * SetElementSize
                                + PageFrame.AttSetHeaderLength);
                END;
              END;
            END;
          END;

          (* delete entry's data *)
          page.putArray(offset, emptyRecord^);
        END;

        (* adapt global counter *)
        page.putInt(PageFrame.staticEntries, noOfStaticEntries - 1);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.DeleteEntry",
                                       "VirtualPage.FatalError", info));
    END;
  END DeleteEntry;


PROCEDURE MoveEntry (page: T; from, to: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH fromOffset = PageFrame.staticStart
                          + (from - 1) * PageFrame.recordLength,
           toOffset = PageFrame.staticStart
                        + (to - 1) * PageFrame.recordLength DO
        (* copy static part *)
        page.copyArray(fromOffset, toOffset, PageFrame.recordLength);

        (* remove old entry *)
        page.putArray(fromOffset, emptyRecord^);

        (* adapt set/attribute record references *)
        IF (PageFrame.HasAttribute) THEN
          WITH attributePointerIndex = PageFrame.AttributePointer
                                         + toOffset,
               attributeLengthIndex = PageFrame.AttributeLength + toOffset,
               attributeLength      = page.getByte(attributeLengthIndex)    DO
            IF (attributeLength > 0) THEN
              WITH attributePointer = page.getShort(attributePointerIndex) DO
                page.putShort(
                  attributePointer + PageFrame.RecordNumber, to);
              END;
            END;
          END;
        ELSIF (PageFrame.HasSet) THEN
          WITH setPointerIndex = PageFrame.SetPointer + toOffset,
               setCardIndex    = PageFrame.SetCard + toOffset,
               setCard         = page.getByte(setCardIndex)       DO
            IF (setCard > 1) THEN
              WITH setPointer = page.getInt(setPointerIndex) DO
                page.putShort(setPointer + PageFrame.RecordNumber, to);
              END;
            END;
          END;
        END;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "StoragePage.MoveEntry", "VirtualPage.FatalError", info));
    END;
  END MoveEntry;


PROCEDURE PutAttribute (    page     : T;
                            recordNo : CARDINAL;
                            attribute: TEXT;
                        VAR overflow : BOOLEAN   )
  RAISES {Access.Locked, InternalError} =
  VAR
    attributeLength: CARDINAL;
    attributeStart : CARDINAL;
    currentLength  : CARDINAL;
    allocatedLength: CARDINAL;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasAttribute THEN
        <* ASSERT (FALSE) *>
      END;

      attributeLength := Text.Length(attribute);
      (* SPOverflow: parameter is too large? *)
      <* ASSERT (attributeLength <= MaxElements) *>
      (*SPNoAttribute: attribute is NIL? *)
      <* ASSERT (attribute # NIL) *>

      overflow := FALSE;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           attributePointerIndex = PageFrame.AttributePointer + offset,
           attributeLengthIndex  = PageFrame.AttributeLength + offset   DO

        currentLength := page.getByte(attributeLengthIndex);

        (* if there is already an attribute, determine its current and
           allocated length *)
        IF (currentLength = 0) THEN
          attributeStart := 0;
          allocatedLength := 0;
        ELSE
          attributeStart := page.getShort(attributePointerIndex);
          allocatedLength :=
            page.getByte(PageFrame.AllocatedLength + attributeStart);
        END;

        (* an empty attribute is written *)
        IF (attributeLength = 0) THEN
          page.putByte(attributeLengthIndex, 0);
          page.putShort(attributePointerIndex, 0);

          (* old attribute space is now overhead *)
          IF (allocatedLength > 0) THEN
            WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
              page.putInt(
                PageFrame.dynamicOverhead,
                overhead + allocatedLength + PageFrame.AttSetHeaderLength);
            END;
          END;
          RETURN;
        END;

        (* determine where to store new attribute *)
        IF (attributeLength <= allocatedLength) THEN
          (* easy case: new attribute fits into allocated space *)
          WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
            page.putInt(PageFrame.dynamicOverhead,
                        overhead + (currentLength - attributeLength));
          END;
        ELSE
          (* no room for the new attribute at the old place: try to
             allocate new room in unused area *)
          attributeStart :=
            AllocateSetAtt(
              page, attributeLength, elementSize := 1,
              recordNo := recordNo, allocated := allocatedLength);
          page.putShort(attributePointerIndex, attributeStart);
          IF (allocatedLength = 0) THEN
            overflow := TRUE;
            RETURN;
          END;
        END;

        (* set up pointers for new attribute *)
        page.putByte(attributeLengthIndex, attributeLength);

        (* write attribute *)
        page.putText(attributeStart + PageFrame.AttSetData, attribute);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.PutAttribute",
                                       "VirtualPage.FatalError", info));
    END;
  END PutAttribute;


PROCEDURE GetAttribute (page: T; recordNo: CARDINAL): TEXT
  RAISES {Access.Locked, InternalError} =
  VAR t: TEXT;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasAttribute THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           attributePointerIndex = PageFrame.AttributePointer + offset,
           attributeLengthIndex  = PageFrame.AttributeLength + offset,
           length                = page.getByte(attributeLengthIndex),
           pointer               = page.getShort(attributePointerIndex) DO
        IF (length > 0) THEN
          t := page.getText(PageFrame.AttSetData + pointer, length);
        ELSE
          t := "";
        END;
      END;
      RETURN t;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.GetAttribute",
                                       "VirtualPage.FatalError", info));
    END;
  END GetAttribute;


PROCEDURE AttributeLength (page: T; recordNo: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasAttribute THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           attributeLengthIndex = PageFrame.AttributeLength + offset DO
        RETURN page.getByte(attributeLengthIndex);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.AttributeLength",
                                       "VirtualPage.FatalError", info));
    END;
  END AttributeLength;


PROCEDURE PutSet (    page    : T;
                      recordNo: CARDINAL;
                      set     : REF Type.ByteArray;
                  VAR overflow: BOOLEAN             )
  RAISES {Access.Locked, InternalError} =
  VAR
    setCard                 := NUMBER(set^) DIV SetElementSize;
    setStart     : CARDINAL;
    currentCard  : CARDINAL;
    allocatedCard: CARDINAL;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;
      (* SPAccessError: operation not allowed on this instance? *)
      <* ASSERT ((NUMBER(set^) MOD SetElementSize) = 0) *>
      (* SPOverflow: parameter is too large? *)
      <* ASSERT (setCard <= MaxElements) *>

      overflow := FALSE;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setPointerIndex = PageFrame.SetPointer + offset,
           setCardIndex    = PageFrame.SetCard + offset     DO

        currentCard := page.getByte(setCardIndex);

        (* if there is already a set, determine its current and allocated
           length *)
        IF (currentCard <= 1) THEN
          allocatedCard := 0;
          setStart := 0;
        ELSE
          allocatedCard :=
            page.getByte(PageFrame.AllocatedLength + setStart);
          setStart := page.getInt(setPointerIndex);
        END;

        (* the set is empty or stored within the static part *)
        IF (setCard <= 1) THEN
          page.putByte(setCardIndex, setCard);
          IF (setCard = 0) THEN
            page.putInt(setPointerIndex, 0);
          ELSE
            page.putArray(setPointerIndex, SUBARRAY(set^, 0, 4));
          END;

          (* old set space is now overhead *)
          IF (allocatedCard > 0) THEN
            WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
              page.putInt(PageFrame.dynamicOverhead,
                          overhead + allocatedCard * SetElementSize
                            + PageFrame.AttSetHeaderLength);
            END;
          END;
          RETURN;
        END;

        (* determine where to store new set *)
        IF (setCard <= allocatedCard) THEN
          (* easy case: new set fits into allocated space *)
          WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
            page.putInt(
              PageFrame.dynamicOverhead,
              overhead + (currentCard - setCard) * SetElementSize);
          END;
        ELSE
          (* no room for the new set at the old place: try to allocate new
             room in unused area *)
          setStart := AllocateSetAtt(
                        page, setCard, elementSize := SetElementSize,
                        recordNo := recordNo, allocated := allocatedCard);
          page.putInt(setPointerIndex, setStart);
          IF (allocatedCard = 0) THEN
            overflow := TRUE;
            RETURN;
          END;
        END;

        (* set up pointers for new set *)
        page.putByte(setCardIndex, setCard);

        (* write set *)
        page.putArray(setStart + PageFrame.AttSetData, set^);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "StoragePage.PutSet", "VirtualPage.FatalError", info));
    END;
  END PutSet;


PROCEDURE GetSet (page: T; recordNo: CARDINAL): REF Type.ByteArray
  RAISES {Access.Locked, InternalError} =
  VAR set: REF Type.ByteArray;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setPointerIndex = PageFrame.SetPointer + offset,
           setCardIndex    = PageFrame.SetCard + offset,
           setCard         = page.getByte(setCardIndex),
           setPointer      = page.getInt(setPointerIndex)   DO
        set := NEW(REF Type.ByteArray, setCard * SetElementSize);
        IF (setCard > 0) THEN
          IF (setCard = 1) THEN
            page.getArray(setPointerIndex, SUBARRAY(set^, 0, 4));
          ELSE
            page.getArray(PageFrame.AttSetData + setPointer, set^);
          END;
        END;
      END;
      RETURN set;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "StoragePage.GetSet", "VirtualPage.FatalError", info));
    END;
  END GetSet;


PROCEDURE SetCardinality (page: T; recordNo: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setCardIndex = PageFrame.SetCard + offset DO
        RETURN page.getByte(setCardIndex);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.SetCardinality",
                                       "VirtualPage.FatalError", info));
    END;
  END SetCardinality;


PROCEDURE PutSetElement (    page     : T;
                             recordNo : CARDINAL;
                         VAR element  : CARDINAL;
                         VAR existent : BOOLEAN;
                         VAR overflow : BOOLEAN;
                         VAR displaced: BOOLEAN   )
  RAISES {Access.Locked, InternalError} =
  VAR
    allocated  : CARDINAL;
    setStart   : CARDINAL;
    setContents: REF Type.ByteArray;
    elementNo  : CARDINAL;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      existent := FALSE;
      overflow := FALSE;
      displaced := FALSE;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setPointerIndex = PageFrame.SetPointer + offset,
           setCardIndex    = PageFrame.SetCard + offset,
           card            = page.getByte(setCardIndex)     DO
        IF (card = 0) THEN
          (* first element in set: store instead of pointer *)
          page.putByte(setCardIndex, 1);
          page.putInt(setPointerIndex, element);
          RETURN;
        END;

        IF (card = 1) THEN
          (* first element so far is at pointer index *)
          WITH firstElement = page.getInt(setPointerIndex) DO
            IF (firstElement = element) THEN
              existent := TRUE;
              RETURN;
            END;

            (* try to build real entry in dynamic page part for two
               elements *)
            allocated := 0;
            setStart :=
              AllocateSetAtt(
                page, noOfElements := 2, elementSize := SetElementSize,
                recordNo := recordNo, allocated := allocated);
            IF (allocated = 0) THEN
              overflow := TRUE;
              RETURN;
            END;

            (* set up pointers for new set *)
            page.putInt(setPointerIndex, setStart);
            page.putByte(setCardIndex, 2);

            (* write set *)
            IF (BitCompareWord.F(firstElement, element)) THEN
              page.putInt(PageFrame.AttSetData + setStart, element);
              page.putInt(PageFrame.AttSetData + setStart + SetElementSize,
                          firstElement);
            ELSE
              page.putInt(PageFrame.AttSetData + setStart, firstElement);
              page.putInt(
                PageFrame.AttSetData + setStart + SetElementSize, element);
            END;
            RETURN;
          END;
        END;

        (* here we have already a real set entry in the dynamic part *)
        setStart := page.getInt(setPointerIndex);

        (* try to locate element in set *)
        elementNo :=
          GetSetElementPosition(
            page, startPointer := PageFrame.AttSetData + setStart,
            firstElement := 1, lastElement := card, element := element,
            found := existent);
        IF (existent) THEN RETURN; END;

        (* element must be inserted *)
        allocated := page.getByte(PageFrame.AllocatedLength + setStart);

        IF ((card = allocated) AND (card < MaxElements)) THEN
          (* reallocation necessary: save old set (may be destroyed by
             compression) *)
          setContents := NEW(REF Type.ByteArray, card * SetElementSize);
          page.getArray(setStart + PageFrame.AttSetData, setContents^);

          (* now try to reallocate *)
          setStart :=
            AllocateSetAtt(page, noOfElements := card + 1,
                           elementSize := SetElementSize,
                           recordNo := recordNo, allocated := allocated);
          IF (allocated = 0) THEN
            overflow := TRUE;
            RETURN;
          END;

          (* set up pointers for new set *)
          page.putInt(setPointerIndex, setStart);

          (* copy old set to new position *)
          page.putArray(setStart + PageFrame.AttSetData, setContents^);

        ELSIF (card = MaxElements) THEN
          (* displace largest element in set *)
          displaced := TRUE;
          WITH displacedElement = page.getInt(
                                    PageFrame.AttSetData + setStart
                                      + (card - 1) * SetElementSize) DO
            IF (BitCompareWord.F(element, displacedElement)) THEN
              (* largest element is the new one *)
              RETURN;
            END;
            InsertElement(page, setStart, elementNo, card - 1, element);
            element := displacedElement;
            RETURN;
          END;

        ELSE
          (* element fits into allocated space: reduce overhead *)
          WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
            page.putInt(
              PageFrame.dynamicOverhead, overhead - SetElementSize);
          END;
        END;

        InsertElement(page, setStart, elementNo, card, element);
        page.putByte(setCardIndex, card + 1);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.PutSetElement",
                                       "VirtualPage.FatalError", info));
    END;
  END PutSetElement;


PROCEDURE GetSetElement (page: T; recordNo: CARDINAL; elementNo: CARDINAL):
  CARDINAL RAISES {Access.Locked, InternalError} =
  VAR res: INTEGER;
  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setPointerIndex = PageFrame.SetPointer + offset,
           setCardIndex    = PageFrame.SetCard + offset,
           card            = page.getByte(setCardIndex)     DO
        (* SPAccessError: operation not allowed on this instance? *)
        <* ASSERT (elementNo <= card) *>

        IF (card = 1) THEN
          res := page.getInt(setPointerIndex);
        ELSE
          WITH setOffset = PageFrame.AttSetData
                             + page.getInt(setPointerIndex) DO
            res :=
              page.getInt(setOffset + SetElementSize * (elementNo - 1));
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.GetSetElement",
                                       "VirtualPage.FatalError", info));
    END;
  END GetSetElement;


PROCEDURE DeleteSetElement (page: T; recordNo: CARDINAL; element: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    elementNo: CARDINAL;
    found    : BOOLEAN;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setPointerIndex = PageFrame.SetPointer + offset,
           setCardIndex    = PageFrame.SetCard + offset     DO
        WITH card = page.getByte(setCardIndex) DO
          IF (card = 1) THEN
            (* SPNotFound *)
            <* ASSERT (page.getInt(setPointerIndex) = element) *>

            page.putInt(setPointerIndex, 0);
            page.putByte(setCardIndex, 0);
            RETURN;
          END;

          elementNo :=
            GetSetElementPosition(
              page, page.getInt(setPointerIndex) + PageFrame.AttSetData,
              firstElement := 1, lastElement := card, element := element,
              found := found);
          (* SPNotFound: no such object? *)
          <* ASSERT (found) *>

          IF (card = 2) THEN
            WITH otherElementNo = 3 - elementNo,
                 otherElementIndex = page.getInt(setPointerIndex)
                                       + PageFrame.AttSetData
                                       + (otherElementNo - 1)
                                           * SetElementSize,
                 otherElement = page.getInt(otherElementIndex),
                 overhead     = page.getInt(PageFrame.dynamicOverhead) DO
              page.putInt(PageFrame.dynamicOverhead,
                          overhead + 2 * SetElementSize
                            + PageFrame.AttSetHeaderLength);
              page.putInt(setPointerIndex, otherElement);
            END;
            page.putByte(setCardIndex, 1);
            RETURN;
          END;

          (* move elements behind the deleted element one place back *)
          IF (elementNo < card) THEN
            WITH elementIndex = page.getInt(setPointerIndex)
                                  + PageFrame.AttSetData
                                  + (elementNo - 1) * SetElementSize DO
              page.copyArray(elementIndex + SetElementSize, elementIndex,
                             (card - elementNo) * SetElementSize);
            END;
          END;

          WITH overhead = page.getInt(PageFrame.dynamicOverhead) DO
            page.putInt(
              PageFrame.dynamicOverhead, overhead + SetElementSize);
          END;
          page.putByte(setCardIndex, card - 1);
        END;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.DeleteSetElement",
                                       "VirtualPage.FatalError", info));
    END;
  END DeleteSetElement;


PROCEDURE GetCard (page: T; recordNo: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setCardIndex = PageFrame.SetCard + offset DO
        RETURN page.getByte(setCardIndex);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "StoragePage.GetCard", "VirtualPage.FatalError", info));
    END;
  END GetCard;


PROCEDURE IsElementInSet (page: T; recordNo: CARDINAL; element: CARDINAL):
  BOOLEAN RAISES {Access.Locked, InternalError} =
  VAR found: BOOLEAN;

  BEGIN
    TRY
      (* SPAccessError: operation not allowed on this instance? *)
      IF NOT PageFrame.HasSet THEN
        <* ASSERT (FALSE) *>
      END;

      WITH offset = PageFrame.staticStart
                      + (recordNo - 1) * PageFrame.recordLength,
           setPointerIndex = PageFrame.SetPointer + offset,
           setCardIndex    = PageFrame.SetCard + offset     DO
        WITH card = page.getByte(setCardIndex) DO
          IF (card = 1) THEN
            RETURN (element = page.getInt(setPointerIndex));
          ELSE
            EVAL GetSetElementPosition(
                   page, startPointer := PageFrame.AttSetData
                                           + page.getInt(setPointerIndex),
                   firstElement := 1, lastElement := card,
                   element := element, found := found);
            RETURN found;
          END;
        END;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.IsElementInSet",
                                       "VirtualPage.FatalError", info));
    END;
  END IsElementInSet;


(* Allocate space for a set or an attribute in the dynamic page part.  The
   overhead counter is updated and the dynamic header is initialized.  The
   function returns the memory page index of the base address of the set or
   attribute.  When calling AllocateSetAtt 'allocated' must be the number
   of elements allocated so far.  After the call to AllocateSetAtt
   'allocated' gives the number of elements that are now allocated (input
   value of allocated + number of added Elements) *)
PROCEDURE AllocateSetAtt (    page        : T;
                              noOfElements: CARDINAL;
                              elementSize : CARDINAL;
                              recordNo    : CARDINAL;
                          VAR allocated   : CARDINAL  ): PageData.Index
  RAISES {Access.Locked, InternalError} =
  VAR
    minAllocateBytes := noOfElements * elementSize
                          + PageFrame.AttSetHeaderLength;
    allocatedBytes: CARDINAL;
    overhead      : CARDINAL;
    firstFree     : CARDINAL;
    wantToAllocate: CARDINAL;

  BEGIN
    TRY
      overhead := page.getInt(PageFrame.dynamicOverhead);

      (* SPOverflow: parameter is too large *)
      <* ASSERT (noOfElements <= MaxElements) *>

      (* check if there are enough bytes free in the dynamic part *)
      firstFree := page.getInt(PageFrame.unusedStart);
      IF (firstFree + minAllocateBytes - 1 - overhead > PageFrame.Size) THEN
        (* even compression won't help *)
        allocated := 0;
        RETURN 1;
      END;

      (* check if new attribute fits into free section *)
      IF (firstFree + minAllocateBytes - 1 > PageFrame.Size) THEN
        (* try compression of allocated but unused space *)
        IF (PageFrame.HasAttribute) THEN
          CompressAttributes(page, firstFree);
        ELSE
          CompressSets(page, firstFree);
        END;
        IF (firstFree + minAllocateBytes - 1 > PageFrame.Size) THEN
          (* compression didn't succeed: overflow *)
          allocated := 0;
          RETURN 1;
        ELSE
          (* compression succeeded: overhead eliminated *)
          overhead := 0;
        END;
      END;

      (* compute how many elements we want to allocate *)
      wantToAllocate := noOfElements;
      IF (allocated > 0) THEN
        (* previously allocated space is now overhead *)
        INC(
          overhead, allocated * elementSize + PageFrame.AttSetHeaderLength);
        wantToAllocate := MIN(wantToAllocate
                                + (noOfElements * PageFrame.Extension)
                                    DIV 100, MaxElements);
      END;

      (* now check how many elements we may allocate *)
      WITH totalFreeElements = (PageFrame.Size - firstFree + 1
                                  - PageFrame.AttSetHeaderLength)
                                 DIV elementSize DO
        allocated := MIN(totalFreeElements, wantToAllocate);
      END;
      (* SPOverflow: parameter is too large *)
      <* ASSERT (noOfElements <= allocated) *>

      allocatedBytes :=
        allocated * elementSize + PageFrame.AttSetHeaderLength;

      (* overhead introduced by new allocation *)
      INC(overhead, allocatedBytes - minAllocateBytes);
      page.putInt(PageFrame.dynamicOverhead, overhead);

      (* set up header *)
      WITH allocatedLengthIndex = PageFrame.AllocatedLength + firstFree,
           recordNumberIndex    = PageFrame.RecordNumber + firstFree     DO
        page.putShort(recordNumberIndex, recordNo);
        page.putByte(allocatedLengthIndex, allocated);
      END;

      (* advance free pointer *)
      page.putInt(PageFrame.unusedStart, firstFree + allocatedBytes);

      RETURN firstFree
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.AllocateSetAtt",
                                       "VirtualPage.FatalError", info));
    END;
  END AllocateSetAtt;


(* Compresses the dynamic part of a page with attributes. *)
PROCEDURE CompressAttributes (page: T; VAR firstFree: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    pointer         : CARDINAL;
    newPointer      : CARDINAL;
    top             : CARDINAL;
    recordNo        : CARDINAL;
    allocatedLength : CARDINAL;
    attributePointer: CARDINAL;
    attributeLength : CARDINAL;

  BEGIN
    TRY

      pointer :=
        PageFrame.staticStart
          + PageFrame.recordLength * page.getInt(PageFrame.staticSize);
      newPointer := pointer;
      top := page.getInt(PageFrame.unusedStart);

      WHILE (pointer < top) DO
        recordNo := page.getShort(PageFrame.RecordNumber + pointer);
        allocatedLength :=
          page.getByte(PageFrame.AllocatedLength + pointer);

        WITH offset = PageFrame.staticStart
                        + (recordNo - 1) * PageFrame.recordLength DO
          attributePointer :=
            page.getShort(PageFrame.AttributePointer + offset);
          attributeLength :=
            page.getByte(PageFrame.AttributeLength + offset);

          (* copy the attribute's value if it is not deleted *)
          IF ((attributePointer = pointer) AND (attributeLength > 0)) THEN
            (* attribute is valid, set up new record data *)
            page.putShort(PageFrame.RecordNumber + newPointer, recordNo);
            page.putByte(
              PageFrame.AllocatedLength + newPointer, attributeLength);
            page.copyArray(
              PageFrame.AttSetData + attributePointer,
              PageFrame.AttSetData + newPointer, attributeLength);

            (* set new pointer in static entry *)
            page.putShort(PageFrame.AttributePointer + offset, newPointer);

            INC(newPointer, PageFrame.AttSetHeaderLength + attributeLength);
          END;
        END;

        INC(pointer, PageFrame.AttSetHeaderLength + allocatedLength);
      END;

      (* store new page global data *)
      page.putInt(PageFrame.dynamicOverhead, 0);
      firstFree := newPointer;
      page.putInt(PageFrame.unusedStart, firstFree);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.CompressAttributes",
                                       "VirtualPage.FatalError", info));
    END;
  END CompressAttributes;


(* Compresses the dynamic part of a page with sets. *)
PROCEDURE CompressSets (page: T; VAR firstFree: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    pointer      : CARDINAL;
    newPointer   : CARDINAL;
    top          : CARDINAL;
    recordNo     : CARDINAL;
    allocatedCard: CARDINAL;
    setPointer   : CARDINAL;
    setCard      : CARDINAL;

  BEGIN
    TRY

      pointer :=
        PageFrame.staticStart
          + PageFrame.recordLength * page.getInt(PageFrame.staticSize);
      newPointer := pointer;
      top := page.getInt(PageFrame.unusedStart);

      WHILE (pointer < top) DO
        recordNo := page.getShort(PageFrame.RecordNumber + pointer);
        allocatedCard := page.getByte(PageFrame.AllocatedLength + pointer);

        WITH offset = PageFrame.staticStart
                        + (recordNo - 1) * PageFrame.recordLength DO
          setPointer := page.getInt(PageFrame.SetPointer + offset);
          setCard := page.getByte(PageFrame.SetCard + offset);
          (* copy the set's value if it is not deleted *)
          IF ((setPointer = pointer) AND (setCard > 1)) THEN
            (* set is valid, set up new record data *)
            page.putShort(PageFrame.RecordNumber + newPointer, recordNo);
            page.putByte(PageFrame.AllocatedLength + newPointer, setCard);
            page.copyArray(
              PageFrame.AttSetData + setPointer,
              PageFrame.AttSetData + newPointer, setCard * SetElementSize);

            (* set new pointer in static entry *)
            page.putInt(PageFrame.SetPointer + offset, newPointer);

            INC(newPointer,
                PageFrame.AttSetHeaderLength + setCard * SetElementSize);
          END;
        END;

        INC(pointer,
            PageFrame.AttSetHeaderLength + allocatedCard * SetElementSize);
      END;

      (* store new page global data *)
      page.putInt(PageFrame.dynamicOverhead, 0);
      firstFree := newPointer;
      page.putInt(PageFrame.unusedStart, firstFree);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.CompressSets",
                                       "VirtualPage.FatalError", info));
    END;
  END CompressSets;


(* Inserts an element into a set in the dynamic page part. *)
PROCEDURE InsertElement (page     : T;
                         setStart : PageData.Index;
                         elementNo: CARDINAL;
                         card     : CARDINAL;
                         element  : CARDINAL        )
  RAISES {Access.Locked, InternalError} =
  VAR
    insertionIndex := PageFrame.AttSetData + setStart
                        + (elementNo - 1) * SetElementSize;
    bytesToCopy            := (card - elementNo + 1) * SetElementSize;
    elementFound: CARDINAL;

  BEGIN
    TRY

      elementFound := page.getInt(insertionIndex);
      (* element has to be inserted behind the one found *)
      IF (BitCompareWord.F(element, elementFound)) THEN
        INC(insertionIndex, SetElementSize);
        DEC(bytesToCopy, SetElementSize);
      END;
      IF (bytesToCopy > 0) THEN
        page.copyArray(
          insertionIndex, insertionIndex + SetElementSize, bytesToCopy);
      END;

      (* insert element *)
      page.putInt(insertionIndex, element);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.InsertElement",
                                       "VirtualPage.FatalError", info));
    END;
  END InsertElement;


(* Evaluates the relative position of an element in a set in the dynamic
   page part. *)
PROCEDURE GetSetElementPosition (page        : T;
                                 startPointer: PageData.Index;
                                     firstElement, lastElement: CARDINAL;
                                     element                  : CARDINAL;
                                 VAR found                    : BOOLEAN   ):
  CARDINAL RAISES {Access.Locked, InternalError} =
  VAR
    currentIndex: CARDINAL;
    currentValue: CARDINAL;

  BEGIN
    TRY
      (* binary search in the set *)
      found := FALSE;
      WHILE ((firstElement <= lastElement) AND (NOT found)) DO
        currentIndex := (firstElement + lastElement) DIV 2;
        currentValue :=
          page.getInt(startPointer + (currentIndex - 1) * SetElementSize);
        IF (currentValue = element) THEN
          found := TRUE;
        ELSIF (BitCompareWord.F(currentValue, element)) THEN
          lastElement := currentIndex - 1;
        ELSE
          firstElement := currentIndex + 1;
        END;
      END;
      RETURN currentIndex;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.GetSetElementPosition",
                                       "VirtualPage.FatalError", info));
    END;
  END GetSetElementPosition;


(* Computes how many bytes are used in the dynamic page part. *)
PROCEDURE DynamicByteUsage (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH staticSize = page.getInt(PageFrame.staticSize),
           dynamicStart = PageFrame.staticStart
                            + PageFrame.recordLength * staticSize DO
        RETURN page.getInt(PageFrame.unusedStart) - dynamicStart
                 - page.getInt(PageFrame.dynamicOverhead);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.DynamicByteUsage",
                                       "VirtualPage.FatalError", info));
    END;
  END DynamicByteUsage;


(* Determines how many records should be allocated for the page.  This is
   computed as the page size divided by the average no.  of bytes used by
   one record.  A minimum of 10% of the overall page size is kept free for
   the dynamic page part to avoid drastical changes in dynamic usage due to
   very few bytes of actual change. *)
PROCEDURE OptimalNoOfRecords (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR avgDynamicBytesPerRecord: CARDINAL;

  BEGIN
    TRY
      WITH dynamicNoOfBytes = DynamicByteUsage(page),
           totalSize        = PageFrame.Size - PageFrame.staticStart + 1,
           dynamicKeepFree  = totalSize DIV 10,
           dynamicTotal     = MAX(dynamicNoOfBytes, dynamicKeepFree),
           noOfRecords      = page.getInt(PageFrame.staticEntries)        DO

        (* compute average space occupied by one record *)
        IF (noOfRecords = 0) THEN
          avgDynamicBytesPerRecord := PageFrame.AvgSize;
        ELSE
          avgDynamicBytesPerRecord := dynamicTotal DIV noOfRecords;
        END;
        WITH avgBytesPerRecord = avgDynamicBytesPerRecord
                                   + PageFrame.recordLength DO

          (* compute no.  of records on optimal page layout *)
          RETURN totalSize DIV avgBytesPerRecord;
        END;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("StoragePage.OptimalNoOfRecords",
                                       "VirtualPage.FatalError", info));
    END;
  END OptimalNoOfRecords;

BEGIN
  FOR i := FIRST(emptyRecord^) TO LAST(emptyRecord^) DO
    emptyRecord^[i] := 0;
  END;
  defaultNoOfRecords := PageFrame.defaultNoOfRecords;
END StoragePage.
