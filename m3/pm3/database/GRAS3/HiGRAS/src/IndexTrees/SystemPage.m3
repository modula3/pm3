MODULE SystemPage;

(***************************************************************************)
(** A system page is actually an index page with some additional
  information in a deprived section. This section is organized as follows:

  Byte                                      Entry

                             |==================================|
  PhysExistent ..            | no. of physically existent pages |
  PhysExistent+3             |                                  |
                             |==================================|
  NoOfUnused ..              | no. of existing but unused pages |
  NoOfUnused+3               |                                  |
                             |==================================|
  NoOfIndex ..               | no. of existing index pages      |
  NoOfIndex+3                |                                  |
                             |==================================|
  LastNonfull ..             | no. of most recently accessed    |
  LastNonfull+3              | nonfull index page               |
                             |==================================|
  NoOfIndexTrees ..          | no. of index trees administered  |
  NoOfIndexTress+3           | by this system page              |
                             |==================================|
  IndexRoots ..              | node no. of root of index tree   |
  IndexRoots+3               | no. 1                            |
                             |----------------------------------|
  .                          |                .                 |
  .                          |                .                 |
  .                          |                .                 |
                             |----------------------------------|
  ..+(MaxNoOfIndexTrees-1)*4 | node no. of root of index tree   |
 ..+(MaxNoOfIndexTrees-1)*4+3| no. MaxNoOfIndexTrees            |
                             |==================================|
  IndexTreePages ..          | no. of data pages in index tree  |
  IndexTreePages+3           | no. 1                            |
                             |----------------------------------|
  .                          |                .                 |
  .                          |                .                 |
  .                          |                .                 |
                             |----------------------------------|
  ..+(MaxNoOfIndexTrees-1)*4 | no. of data pages in index tree  |
 ..+(MaxNoOfIndexTrees-1)*4+3| no. MaxNoOfIndexTrees            |
                             |==================================|
  FreeNodes ..               | no. of free nodes on index page  |
  FreeNodes+3                | no. 1                            |
                             |----------------------------------|
  .                          |                .                 |
  .                          |                .                 |
  .                          |                .                 |
                             |----------------------------------|
  ..+(MaxIndexPages-1)*4 ..  | no. of free nodes on index page  |
  ..+(MaxIndexPages-1)*4+3   | no. MaxIndexPages                |
                             |==================================|
  FreePages ..               | existing but unused page         |
  FreePages+3                | no. 1                            |
                             |----------------------------------|
  .                          |                .                 |
  .                          |                .                 |
  .                          |                .                 |
                             |----------------------------------|
  ..+(PageBufferSize-1)*4 .. | existing but unused page         |
  ..+(PageBufferSize-1)*4+3  | no. PageBufferSize               |
                             |==================================|

*)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.5  1997/11/19 17:59:18  roland
    Removed grouping of page accesses.

    Revision 1.4  1997/09/22 20:23:22  roland
    On file creation, data pages were counted twice. Now system page will
    only be notified by ITPFile of new pages.

    Revision 1.3  1997/09/18 08:23:16  roland
    Grouping of access to the same page.

    Revision 1.2  1997/05/06 13:17:09  roland
    Bugfix: first (index) page number is 0 not 1.

    Revision 1.1  1997/03/26 11:25:23  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.11  1997/02/20 16:16:05  roland
    Minor fixes.
    IndexTreeOrganization can now log merge and split of pages.

    Revision 1.10  1996/11/20 12:22:14  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.9  1996/08/06 16:25:46  roland
    Merge of PAGESERVER and main branch.

    Revision 1.8.2.2  1996/07/24 09:18:56  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.8.2.1  1996/04/29 13:38:18  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.8  1993/10/26  19:48:27  pk
# Naming of methods and procedures updated.
#
# Revision 1.7  1993/08/30  19:29:45  pk
# Error corrected: no. of free index nodes on system page after
# initialization didn't count root nodes for index trees.
#
# Revision 1.6  1993/08/25  12:55:59  pk
# Reverse storage of new page numbers now really works.
#
# Revision 1.5  1993/08/25  09:21:53  pk
# Another index range changed.
#
# Revision 1.4  1993/08/23  16:42:54  pk
# Error corrected: index range gave range error.
#
# Revision 1.3  1993/08/20  20:10:19  pk
# New pages numbers after extension are stored backwards so they come
# out in the right order.
#
# Revision 1.2  1993/08/17  15:45:54  pk
# Some exception handling added.
#
# Revision 1.1  1993/08/17  12:54:00  pk
# Abstract data type for system pages.
#
*)
(***************************************************************************)
IMPORT PageData, Access, IndexPage, IndexTreeOrganization;
IMPORT ErrorSupport, VirtualPage;

CONST
  (* maximum number of index pages *)
  MaxIndexPage = 256;

  (* locations on system page *)
  PhysExistent   = 0;
  NoOfUnused     = PhysExistent + 4;
  NoOfIndex      = NoOfUnused + 4;
  LastNonfull    = NoOfIndex + 4;
  NoOfIndexTrees = LastNonfull + 4;
  IndexRoots     = NoOfIndexTrees + 4;
  IndexTreePages = IndexRoots + MaxNoOfIndexTrees * 4;
  FreeNodes      = IndexTreePages + MaxNoOfIndexTrees * 4;
  FreePages      = FreeNodes + MaxIndexPage * 4;

  TotalUse = FreePages + PageBufferSize * 4;


PROCEDURE Init (page: T; trees: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    offset          : PageData.Index;
    indexRoots      : PageData.Index;
    indexTreePages  : PageData.Index;
    freeNodes       : PageData.Index;
    nodePosition    : IndexPage.NodePosition;
    node            : IndexPage.Entry;
    freeOnSystemPage: CARDINAL;

  BEGIN
    <* ASSERT trees <= MaxNoOfIndexTrees *>
    TRY
      (* set up deprived section *)
      offset := IndexPage.Deprive(page, TotalUse);

      (* calculate absolute pointer positions *)
      indexRoots := IndexRoots + offset;
      indexTreePages := IndexTreePages + offset;
      freeNodes := FreeNodes + offset;

      (* the system page and one root page for every index tree must
         exist *)
      page.putInt(PhysExistent + offset, trees + 1);
      page.putInt(NoOfUnused + offset, 0);
      page.putInt(NoOfIndex + offset, 1);
      page.putInt(LastNonfull + offset, IndexTreeOrganization.SystemPageNo);
      page.putInt(NoOfIndexTrees + offset, trees);
      freeOnSystemPage :=
        IndexPage.NoOfFreeEntries(page) - MaxNoOfIndexTrees;
      page.putInt(freeNodes, freeOnSystemPage);

      (* set up a page reference *)
      node := NEW(IndexPage.PageRefEntry);

      FOR i := 1 TO trees DO
        (* store page references for the roots of every index tree *)
        nodePosition := IndexPage.Reserve(page);
        page.putInt(indexRoots + (i - 1) * 4, nodePosition);
        NARROW(node, IndexPage.PageRefEntry).pageNo :=
          i + IndexTreeOrganization.SystemPageNo;
        IndexPage.PutEntry(page, nodePosition, FALSE, node);

        (* currently no page in index tree *)
        page.putInt(indexTreePages + (i - 1) * 4, 0);
      END;

      (* no free nodes in other pages *)
      FOR i := 1 TO MaxIndexPage-1 DO
        page.putInt(freeNodes + i * 4, 0);
      END;
    EXCEPT
      IndexPage.AlreadyDeprived =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "SystemPage.Init", "IndexPage.AlreadyDeprived"));
    | IndexPage.Overflow =>
        RAISE InternalError(ErrorSupport.Create(
                              "SystemPage.Init", "IndexPage.Overflow"));
    | IndexPage.StatusViolation =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "SystemPage.Init", "IndexPage.StatusViolation"));
    | IndexPage.WrongEntry =>
        RAISE InternalError(ErrorSupport.Create(
                              "SystemPage.Init", "IndexPage.WrongEntry"));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "SystemPage.Init", "IndexPage.InternalError", info));
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "SystemPage.Init", "VirtualPage.FatalError", info));
    END;

  END Init;

PROCEDURE GetNoOfIndexTrees (page: T): CARDINAL
  RAISES {InternalError, Access.Locked} =
  VAR offset: CARDINAL;
  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      WITH res = page.getInt(NoOfIndexTrees + offset) DO
        RETURN res;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.GetNoOfIndexTrees",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.GetNoOfIndexTrees",
                                       "IndexPage.InternalError", info));
    END;
  END GetNoOfIndexTrees;

PROCEDURE NotifyIndexPageCreation (page: T; pageNo: CARDINAL)
  RAISES {Access.Locked, AlreadyUsed, IndexTreeOverflow,
          WrongIndexPageNumber, InternalError} =
  VAR
    offset        : CARDINAL;
    noOfIndexPages: PageData.Index;
    freeNodes     : PageData.Index;
    noOfFreeNodes : CARDINAL;
    indexPageCount: CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      freeNodes := FreeNodes + pageNo * 4 + offset;
      noOfIndexPages := NoOfIndex + offset;

      (* check counter values *)
      noOfFreeNodes := page.getInt(freeNodes);
      indexPageCount := page.getInt(noOfIndexPages);
      (* SPAlreadyUsed: is the page already in use? *)
      IF noOfFreeNodes # 0 THEN RAISE AlreadyUsed END;
      (* SPIndexTreeOverflow: maximum number of index pages exceeded? *)
      IF indexPageCount >= MaxIndexPage THEN RAISE IndexTreeOverflow END;
      (* SPWrongIndexPageNumber: there may be no index page here *)
      IF pageNo # indexPageCount + 1 THEN RAISE WrongIndexPageNumber END;

      (* write back new values *)
      page.putInt(freeNodes, IndexPage.Size);
      INC(indexPageCount);
      page.putInt(noOfIndexPages, indexPageCount);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "SystemPage.NotifyIndexPageCreation",
                              "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "SystemPage.NotifyIndexPageCreation",
                              "IndexPage.InternalError", info));
    END;
  END NotifyIndexPageCreation;


PROCEDURE NotifyIndexPageDeletion (page: T; pageNo: CARDINAL)
  RAISES {Access.Locked, NotEmpty, IndexTreeUnderflow, InternalError} =
  VAR
    offset        : CARDINAL;
    noOfIndexPages: PageData.Index;
    freeNodes     : PageData.Index;
    noOfFreeNodes : CARDINAL;
    indexPageCount: CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      freeNodes := FreeNodes + pageNo * 4 + offset;
      noOfIndexPages := NoOfIndex + offset;

      (* check counter values *)
      noOfFreeNodes := page.getInt(freeNodes);
      indexPageCount := page.getInt(noOfIndexPages);
      (* SPNotEmpty: contains the page data? *)
      IF noOfFreeNodes # 0 THEN RAISE NotEmpty END;
      (* SPIndexTreeUnderflow: no index pages left? *)
      IF indexPageCount < 1 THEN RAISE IndexTreeUnderflow END;

      (* write back new values *)
      page.putInt(freeNodes, 0);
      DEC(indexPageCount);
      page.putInt(noOfIndexPages, indexPageCount);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "SystemPage.NotifyIndexPageDeletion",
                              "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "SystemPage.NotifyIndexPageDeletion",
                              "IndexPage.InternalError", info));
    END;
  END NotifyIndexPageDeletion;


PROCEDURE NotifyDataPageCreation (page: T; indexTree: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    noOfPagesInTree: CARDINAL;
    indexTreePages : PageData.Index;

  BEGIN
    TRY
      (* calculate absolute page positions *)
      indexTreePages := IndexTreePages + (indexTree - 1) * 4
                          + IndexPage.DeprivedSectionStart(page);

      (* modify data page counter for index tree *)
      noOfPagesInTree := page.getInt(indexTreePages);
      INC(noOfPagesInTree);
      page.putInt(indexTreePages, noOfPagesInTree);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyDataPageCreation",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyDataPageCreation",
                                       "IndexPage.InternalError", info));
    END;
  END NotifyDataPageCreation;


PROCEDURE NotifyDataPageDeletion (page: T; indexTree: CARDINAL)
  RAISES {Access.Locked, SystemPageCorrupt, InternalError} =
  VAR
    noOfPagesInTree: CARDINAL;
    indexTreePages : PageData.Index;

  BEGIN
    TRY
      (* calculate absolute page positions *)
      indexTreePages := IndexTreePages + (indexTree - 1) * 4
                          + IndexPage.DeprivedSectionStart(page);

      (* modify data page counter for index tree *)
      noOfPagesInTree := page.getInt(indexTreePages);
      (* SPSystemPageCorrupt: the data on the system page makes no
         sense? *)
      IF noOfPagesInTree = 0 THEN RAISE SystemPageCorrupt END;

      DEC(noOfPagesInTree);
      page.putInt(indexTreePages, noOfPagesInTree);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyDataPageDeletion",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyDataPageDeletion",
                                       "IndexPage.InternalError", info));
    END;
  END NotifyDataPageDeletion;


PROCEDURE FindNonfullIndexPage (page: T; VAR exists: BOOLEAN): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    offset         : CARDINAL;
    lastNonfullPage: CARDINAL;
    noOfFreeNodes  : CARDINAL;
    freeNodes      : PageData.Index;
    lastNonfull    : PageData.Index;
    indexPage                       := IndexTreeOrganization.SystemPageNo;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      freeNodes := FreeNodes + offset;
      lastNonfull := LastNonfull + offset;

      lastNonfullPage := page.getInt(lastNonfull);

      (* first try cached index page *)
      noOfFreeNodes := page.getInt(freeNodes + lastNonfullPage * 4);
      IF (noOfFreeNodes > 0) THEN
        (* cache hit *)
        exists := TRUE;
        RETURN lastNonfullPage;
      ELSE
        (* failed: search all index pages *)
        WHILE indexPage < MaxIndexPage DO
          noOfFreeNodes := page.getInt(freeNodes + indexPage * 4);
          IF (noOfFreeNodes > 0) THEN
            exists := TRUE;
            lastNonfullPage := indexPage;
            page.putInt(lastNonfull, lastNonfullPage);
            RETURN lastNonfullPage;
          ELSE
            INC(indexPage);
          END;
        END;
        
        (* nothing was found *)
        exists := FALSE;
        RETURN 0;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.FindNonfullIndexPage",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.FindNonfullIndexPage",
                                       "IndexPage.InternalError", info));
    END;
  END FindNonfullIndexPage;


PROCEDURE NotifyNodeReservation (page: T; indexPageNo: CARDINAL)
  RAISES {Access.Locked, PageOverflow, InternalError} =
  VAR
    offset         : CARDINAL;
    noOfFreeNodes  : CARDINAL;
    freeNodesOnPage: PageData.Index;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      freeNodesOnPage := FreeNodes + offset + indexPageNo * 4;

      (* determine number of free nodes *)
      noOfFreeNodes := page.getInt(freeNodesOnPage);
      (* SPPageOverflow: there are no free nodes on this page? *)
      IF noOfFreeNodes = 0 THEN RAISE PageOverflow END;

      (* write back new value *)
      DEC(noOfFreeNodes);
      page.putInt(freeNodesOnPage, noOfFreeNodes);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyNodeReservation",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyNodeReservation",
                                       "IndexPage.InternalError", info));
    END;
  END NotifyNodeReservation;


PROCEDURE NotifyNodeRelease (page: T; indexPageNo: CARDINAL)
  RAISES {Access.Locked, PageUnderflow, InternalError} =
  VAR
    offset         : CARDINAL;
    noOfFreeNodes  : CARDINAL;
    freeNodesOnPage: PageData.Index;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      freeNodesOnPage := FreeNodes + offset + indexPageNo * 4;

      (* determine number of free nodes *)
      noOfFreeNodes := page.getInt(freeNodesOnPage);
      (* SPPageUnderflow: too many free nodes on this page *)
      IF noOfFreeNodes >= PageData.Size THEN RAISE PageUnderflow END;

      (* write back new value *)
      INC(noOfFreeNodes);
      page.putInt(freeNodesOnPage, noOfFreeNodes);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyNodeRelease",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyNodeRelease",
                                       "IndexPage.InternalError", info));
    END;
  END NotifyNodeRelease;


PROCEDURE GetUnusedPage (page: T): CARDINAL
  RAISES {Access.Locked, NoUnusedPages, InternalError} =
  VAR
    offset          : CARDINAL;
    noOfUnused      : PageData.Index;
    unusedPageOffset: CARDINAL;
    noOfUnusedPages : CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      noOfUnused := NoOfUnused + offset;

      (* modify counter for unused pages *)
      noOfUnusedPages := page.getInt(noOfUnused);
      (* SPNoUnusedPage: there is no unused page left? *)
      IF noOfUnusedPages = 0 THEN RAISE NoUnusedPages END;

      unusedPageOffset := (noOfUnusedPages - 1) * 4;
      DEC(noOfUnusedPages);
      page.putInt(noOfUnused, noOfUnusedPages);

      (* get the page number *)
      WITH res = page.getInt(FreePages + offset + unusedPageOffset) DO
        RETURN res;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.GetUnusedPage",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.GetUnusedPage",
                                       "IndexPage.InternalError", info));
    END;
  END GetUnusedPage;


PROCEDURE PutUnusedPage (page: T; pageNo: CARDINAL)
  RAISES {Access.Locked, BufferOverflow, InternalError} =
  VAR
    offset          : CARDINAL;
    noOfUnused      : PageData.Index;
    unusedPageOffset: CARDINAL;
    noOfUnusedPages : CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      noOfUnused := NoOfUnused + offset;

      (* modify counter for unused pages *)
      noOfUnusedPages := page.getInt(noOfUnused);
      (*SPBufferOverflow: there are no free nodes on this page? *)
      IF noOfUnusedPages >= PageBufferSize THEN RAISE BufferOverflow END;

      INC(noOfUnusedPages);
      unusedPageOffset := (noOfUnusedPages - 1) * 4;
      page.putInt(noOfUnused, noOfUnusedPages);

      (* store the page number *)
      page.putInt(FreePages + offset + unusedPageOffset, pageNo);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.PutUnusedPage",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.PutUnusedPage",
                                       "IndexPage.InternalError", info));
    END;
  END PutUnusedPage;


PROCEDURE NotifyExtension (page: T)
  RAISES {Access.Locked, BufferNotEmpty, InternalError} =
  VAR
    offset          : CARDINAL;
    physExistent    : PageData.Index;
    noOfUnused      : PageData.Index;
    freePages       : PageData.Index;
    noOfPhysExistent: CARDINAL;
    noOfUnusedPages : CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      physExistent := PhysExistent + offset;
      noOfUnused := NoOfUnused + offset;
      freePages := FreePages + offset;

      (* get counter values *)
      noOfPhysExistent := page.getInt(physExistent);
      noOfUnusedPages := page.getInt(noOfUnused);
      (* SPBufferNotEmpty: the page buffer is not empty? *)
      IF noOfUnusedPages # 0 THEN RAISE BufferNotEmpty END;

      (* store new page numbers in buffer *)
      FOR i := 1 TO PageBufferSize DO
        page.putInt(freePages + (i - 1) * 4,
                    noOfPhysExistent + PageBufferSize - i + 1);
      END;

      (* modify counters *)
      INC(noOfPhysExistent, PageBufferSize);
      noOfUnusedPages := PageBufferSize;
      page.putInt(physExistent, noOfPhysExistent);
      page.putInt(noOfUnused, noOfUnusedPages);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyExtension",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyExtension",
                                       "IndexPage.InternalError", info));
    END;
  END NotifyExtension;


PROCEDURE NotifyTruncation (page: T)
  RAISES {Access.Locked, BufferNotFull, InternalError} =
  VAR
    offset          : CARDINAL;
    physExistent    : PageData.Index;
    noOfUnused      : PageData.Index;
    freePages       : PageData.Index;
    noOfPhysExistent: CARDINAL;
    noOfUnusedPages : CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      (* calculate absolute page positions *)
      physExistent := PhysExistent + offset;
      noOfUnused := NoOfUnused + offset;
      freePages := FreePages + offset;

      (* get counter values *)
      noOfPhysExistent := page.getInt(physExistent);
      noOfUnusedPages := page.getInt(noOfUnused);
      (* SPBufferNotFull: the page buffer is not full? *)
      IF noOfUnusedPages # PageBufferSize THEN RAISE BufferNotFull END;

      (* modify counters *)
      DEC(noOfPhysExistent, PageBufferSize);
      noOfUnusedPages := 0;
      page.putInt(physExistent, noOfPhysExistent);
      page.putInt(noOfUnused, noOfUnusedPages);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyTruncation",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NotifyTruncation",
                                       "IndexPage.InternalError", info));
    END;
  END NotifyTruncation;


PROCEDURE GetIndexTreeRootNode (page: T; indexTree: CARDINAL):
  IndexPage.NodePosition RAISES {Access.Locked, InternalError} =
  VAR offset: CARDINAL;
      root: IndexPage.NodePosition;
  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      root := page.getInt(IndexRoots + offset + (indexTree - 1) * 4);
      RETURN root;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.GetIndexTreeRootNode",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.GetIndexTreeRootNode",
                                       "IndexPage.InternalError", info));
    END;
  END GetIndexTreeRootNode;


PROCEDURE NoOfPhysExistentPages (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR offset: CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      WITH res = page.getInt(PhysExistent + offset) DO
        RETURN res;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfPhysExistentPages",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfPhysExistentPages",
                                       "IndexPage.InternalError", info));
    END;
  END NoOfPhysExistentPages;


PROCEDURE NoOfUnusedPages (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR offset: CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      WITH res = page.getInt(NoOfUnused + offset) DO
        RETURN res;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfUnusedPages",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfUnusedPages",
                                       "IndexPage.InternalError", info));
    END;
  END NoOfUnusedPages;


PROCEDURE NoOfIndexPages (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR offset: CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      WITH res = page.getInt(NoOfIndex + offset) DO
        RETURN res;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfIndexPages",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfIndexPages",
                                       "IndexPage.InternalError", info));
    END;
  END NoOfIndexPages;


PROCEDURE NoOfDataPagesInTree (page: T; indexTree: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR offset: CARDINAL;

  BEGIN
    TRY
      offset := IndexPage.DeprivedSectionStart(page);
      WITH res = page.getInt(IndexTreePages + offset + (indexTree - 1) * 4) DO
        RETURN res;
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfDataPagesInTree",
                                       "VirtualPage.FatalError", info));
    | IndexPage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("SystemPage.NoOfDataPagesInTree",
                                       "IndexPage.InternalError", info));
    END;
  END NoOfDataPagesInTree;

BEGIN
END SystemPage.
