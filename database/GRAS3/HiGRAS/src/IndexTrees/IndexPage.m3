UNSAFE MODULE IndexPage;

(***************************************************************************)
(** Index pages are organized as follows:

  Byte                                  Entry

                         |===========node status============|
  1                      | status of node 1                 |
  .                      |                .                 |
  .                      |                .                 |
  .                      |                .                 |
  Size                   | status of node Size              |
                         |===========node storage===========|
  Size+1                 | left entry  byte 1               |
  Size+2                 | left entry  byte 2               |
  Size+3                 | left entry  byte 3    index node |
  Size+4                 | right entry byte 1        1      |
  Size+5                 | right entry byte 2               |
  Size+6                 | right entry byte 3               |
                         |----------------------------------|
  .                      |                .                 |
  .                      |                .                 |
  .                      |                .                 |
                         |----------------------------------|
  Size*7-5               | left entry  byte 1               |
  Size*7-4               | left entry  byte 2               |
  Size*7-3               | left entry  byte 3    index node |
  Size*7-2               | right entry byte 1       Size    |
  Size*7-1               | right entry byte 2               |
  Size*7                 | right entry byte 3               |
                         |----------------------------------|
                         | empty                            |
                         |=============pointers=============|
  DeprivedStart ..       | pointer to deprived section      |
  DeprivedStart+3        | (=0 if none)                     |
                         |----------------------------------|
  LastUsed ..            | pointer to last used node        |
  LastUsed+3             |                                  |
                         |==================================|


  There are Size nodes on every page, each of them represented by 6 byte
  of information: 3 for the left and 3 for the right successor. Both
  entries have the following format:
  Byte1: depending on byte 2
  Byte2: >= 128: page reference to page number (Byte2-128)*256 + Byte1
         < 128:  node reference,
                  >= 64: split key 2, position (Byte2-64)*256 + Byte1
                  < 64:  split key 1, position (Byte2*256 + Byte1
  Byte3: only if node reference: index page number

  Additionally, there is a status list on top of the page with a one byte
  entry for every node. Here, the current status of the node is stored.
*)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.3  1997/11/19 17:59:16  roland
    Removed grouping of page accesses.

    Revision 1.2  1997/09/18 08:23:15  roland
    Grouping of access to the same page.

    Revision 1.1  1997/03/26 11:25:11  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.10  1996/11/20 12:22:08  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.9  1996/08/06 16:25:37  roland
    Merge of PAGESERVER and main branch.

    Revision 1.8.2.2  1996/07/24 09:18:48  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.8.2.1  1996/04/29 13:38:12  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.8  1993/11/03  20:38:26  pk
# Minor syntactical changes.
#
# Revision 1.7  1993/10/26  19:48:23  pk
# Naming of methods and procedures updated.
#
# Revision 1.6  1993/09/30  15:15:26  pk
# Basic types moved from Physical/MemoryPage to Basics/Type.i3
#
# Revision 1.5  1993/08/25  17:03:34  pk
# Some LOOPHOLEs replaced by VALs/ORDs.
#
# Revision 1.4  1993/08/19  19:59:08  pk
# Error corrected: status list pointers subrange type extended (crashed
# on initialization with last = 0)
#
# Revision 1.3  1993/08/19  19:31:18  pk
# Error corrected in DeprivedStart pointer and searching deprived sections.
#
# Revision 1.2  1993/08/17  15:45:50  pk
# Some exception handling added.
#
# Revision 1.1  1993/08/17  12:52:11  pk
# Abstract data type for index pages.
#
*)
(***************************************************************************)

IMPORT Type, PageData, Access;
IMPORT ErrorSupport, VirtualPage;

TYPE
  NodeStatus = {Free,            (* the node is unused *)
                Used,            (* the node is in use *)
                Locked,          (* the node may not be accessed *)
                WriteLocked,     (* the node may not be overwritten *)
                Deprived};       (* the node is not used as a node at
                                    all *)

  StatusArray = ARRAY [1 .. Size] OF NodeStatus;

  ByteStatusArray = ARRAY [1 .. Size] OF Type.Byte;


CONST
  (* predefined status lists *)
  FreeArray     = StatusArray{NodeStatus.Free, ..};
  DeprivedArray = StatusArray{NodeStatus.Deprived, ..};

  (* pointer positions on page *)
  LastUsed      = PageData.Size - 3;
  DeprivedStart = LastUsed - 4;


PROCEDURE Init (page: T) RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      (* erase bit list *)
      page.putArray(1, LOOPHOLE(FreeArray, ByteStatusArray));

      (* initialize pointers *)
      page.putInt(DeprivedStart, 0);
      page.putInt(LastUsed, 0);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.Init", "VirtualPage.FatalError", info));
    END;

  END Init;


PROCEDURE Reserve (page: T): NodePosition
  RAISES {Access.Locked, Overflow, InternalError} =
  VAR
    last             : CARDINAL;
    statusListPointer: CARDINAL;
    value            : NodeStatus;

  BEGIN
    TRY
      last := page.getInt(LastUsed);
      IF (last = Size) THEN
        statusListPointer := 1;
      ELSE
        statusListPointer := last + 1;
      END;

      (* searching for free node in status list *)
      REPEAT
        value := VAL(page.getByte(statusListPointer), NodeStatus);
        IF (value # NodeStatus.Free) THEN
          INC(statusListPointer);
          IF (statusListPointer > Size) THEN statusListPointer := 1; END;

          (* Overflow, full index page? *)
          IF NOT statusListPointer # last THEN RAISE Overflow END;
        END;
      UNTIL (value = NodeStatus.Free);

      (* correct the values in the page *)

      page.putByte(statusListPointer, ORD(NodeStatus.Used));
      page.putInt(LastUsed, statusListPointer);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.Reserve", "VirtualPage.FatalError", info));
    END;

    RETURN statusListPointer;
  END Reserve;


PROCEDURE Release (page: T; nodePosition: NodePosition)
  RAISES {Access.Locked, InternalError} =
  BEGIN

    TRY
      page.putByte(nodePosition, ORD(NodeStatus.Free));
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.Release", "VirtualPage.FatalError", info));
    END;
  END Release;


PROCEDURE Deprive (page: T; bytes: CARDINAL): PageData.Index
  RAISES {Access.Locked, AlreadyDeprived, Overflow, InternalError} =
  VAR
    nodesToUse   : CARDINAL;
    statusList   : StatusArray;
    tryStart     : NodePosition;
    compLength                    := 0;
    deprivedStart: PageData.Index;

  BEGIN
    TRY
      (* is there already a deprived section on this page? *)
      IF NOT (page.getInt(DeprivedStart) = 0) THEN
        RAISE AlreadyDeprived
      END;

      nodesToUse := (bytes + 5) DIV 6;
      page.getArray(1, LOOPHOLE(statusList, ByteStatusArray));

      (* search for nodesToUse contiguous free elements in statusList *)
      tryStart := 1;
      WHILE ((tryStart <= Size - nodesToUse + 1)
               AND (compLength < nodesToUse)) DO
        compLength := 0;
        WHILE ((compLength < nodesToUse)
                 AND (statusList[tryStart + compLength] = NodeStatus.Free)) DO
          INC(compLength);
        END;
        IF (compLength < nodesToUse) THEN INC(tryStart, compLength); END;
      END;

      (* Overflow: full index page? *)
      IF NOT (nodesToUse <= compLength) THEN RAISE Overflow END;
      deprivedStart := tryStart * 6 + Size - 5;

      (* mark nodes in statusList *)
      SUBARRAY(statusList, tryStart - 1, nodesToUse) :=
        SUBARRAY(DeprivedArray, tryStart - 1, nodesToUse);

      (* modify page *)

      page.putArray(1, LOOPHOLE(statusList, ByteStatusArray));
      page.putInt(DeprivedStart, deprivedStart);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.Deprive", "VirtualPage.FatalError", info));
    END;

    RETURN deprivedStart;
  END Deprive;


PROCEDURE Return (page: T)
  RAISES {Access.Locked, NotDeprived, InternalError} =
  VAR
    statusList       : StatusArray;
    statusListPointer: NodePosition;
    deprivedStart    : PageData.Index;

  BEGIN
    TRY
      deprivedStart := page.getInt(DeprivedStart);

      (* there must be a deprived section on this page *)
      IF NOT (deprivedStart # 0) THEN RAISE NotDeprived END;

      statusListPointer := (deprivedStart - Size + 5) DIV 6;

      page.getArray(1, LOOPHOLE(statusList, ByteStatusArray));

      WHILE (statusList[statusListPointer] = NodeStatus.Deprived) DO
        statusList[statusListPointer] := NodeStatus.Free;
        INC(statusListPointer);
      END;


      page.putInt(DeprivedStart, 0);
      page.putArray(1, LOOPHOLE(statusList, ByteStatusArray));
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.Return", "VirtualPage.FatalError", info));
    END;
  END Return;


PROCEDURE DeprivedSectionStart (page: T): PageData.Index
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN page.getInt(DeprivedStart);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexPage.DeprivedSectionStart",
                                       "VirtualPage.FatalError", info));
    END;
  END DeprivedSectionStart;


PROCEDURE NoOfFreeEntries (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    statusList: StatusArray;
    count     : CARDINAL;

  BEGIN
    TRY
      page.getArray(1, LOOPHOLE(statusList, ByteStatusArray));
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("IndexPage.NoOfFreeEntries",
                                       "VirtualPage.FatalError", info));
    END;
    count := 0;
    FOR i := 1 TO Size DO
      IF (statusList[i] = NodeStatus.Free) THEN INC(count); END;
    END;
    RETURN count;
  END NoOfFreeEntries;


PROCEDURE GetEntry (page: T; nodePosition: NodePosition; rightNode: BOOLEAN):
  Entry RAISES {Access.Locked, StatusViolation, InternalError} =
  VAR
    pagePos : PageData.Index;
    pageData: ARRAY [1 .. 3] OF Type.Byte;
    entry   : Entry;
    status  : NodeStatus;

  BEGIN
    TRY
      status := VAL(page.getByte(nodePosition), NodeStatus);
      (* Check for StatusViolation: is operation allowed on this node? *)
      IF NOT ((status = NodeStatus.Used)
                OR (status = NodeStatus.WriteLocked)) THEN
        RAISE StatusViolation
      END;

      (* calculate the entry's position on the page *)
      pagePos := nodePosition * 6 + Size - 5;
      IF (rightNode) THEN INC(pagePos, 3); END;
      page.getArray(pagePos, pageData);
      
      (* decode data *)
      IF (pageData[2] >= 128) THEN
        (* it's a page reference *)
        entry := NEW(PageRefEntry,
                     pageNo := (pageData[2] - 128) * 256 + pageData[1]);
      ELSE
        (* it's a node reference *)
        entry := NEW(NodeRefEntry);
        WITH entry = NARROW(entry, NodeRefEntry) DO
          IF (pageData[2] >= 64) THEN
            entry.splitKey := 2;
            DEC(pageData[2], 64);
          ELSE
            entry.splitKey := 1;
          END;
          entry.indexPageNo := pageData[3];
          entry.indexPagePos := pageData[2] * 256 + pageData[1];
        END;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.GetEntry", "VirtualPage.FatalError", info));
    END;

    RETURN entry;
  END GetEntry;


PROCEDURE PutEntry (page        : T;
                    nodePosition: NodePosition;
                    rightNode   : BOOLEAN;
                    pageEntry   : Entry         )
  RAISES {Access.Locked, StatusViolation, WrongEntry, InternalError} =
  VAR
    pagePos : PageData.Index;
    pageData: ARRAY [1 .. 3] OF Type.Byte;
    status  : NodeStatus;

  BEGIN
    TRY
      status := VAL(page.getByte(nodePosition), NodeStatus);

      (* Check for StatusViolation: is operation allowed on this node? *)
      IF NOT ((status = NodeStatus.Used)
                OR (status = NodeStatus.WriteLocked)) THEN
        RAISE StatusViolation
      END;

      (* calculate the entry's position on the page *)
      pagePos := nodePosition * 6 + Size - 5;
      IF (rightNode) THEN INC(pagePos, 3); END;

      (* encode data *)
      TYPECASE pageEntry OF
      | PageRefEntry (entry) =>
          (* it's a page reference *)
          pageData[1] := entry.pageNo MOD 256;
          pageData[2] := entry.pageNo DIV 256 + 128;
      | NodeRefEntry (entry) =>
          (* it's a node reference *)
          pageData[1] := entry.indexPagePos MOD 256;
          pageData[2] :=
            entry.indexPagePos DIV 256 + entry.splitKey * 64 - 64;
          pageData[3] := entry.indexPageNo;
      ELSE
        (* IPWrongEntry: wrong kind of entry type *)
        RAISE WrongEntry;
      END;

      (* modify page *)
      page.putArray(pagePos, pageData);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "IndexPage.PutEntry", "VirtualPage.FatalError", info));
    END;
  END PutEntry;

BEGIN
END IndexPage.
