INTERFACE SystemPage;

(***************************************************************************)
(* A SystemPage is a special IndexPage used for organizing the index trees
   for one database.  Conceptually SystemPage.T should be a subtype of
   IndexPage.T.  But since memory for VirtualPages (and hence IndexPages)
   may only be allocated by the scheduler, SystemPages are merely a
   refernce to IndexPages. *)
(***************************************************************************)
(** Created by: Peter Klein *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:25:20  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.4  1996/11/20 12:22:13  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/08/06 16:25:45  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/24 09:18:54  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.2.2.1  1996/04/29 13:38:17  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.2  1993/08/17  15:45:52  pk
# Some exception handling added.
#
# Revision 1.1  1993/08/17  12:53:57  pk
# Abstract data type for system pages.
#
*)
(***************************************************************************)

IMPORT IndexPage;
IMPORT Access;
IMPORT AtomList;


CONST
  MaxNoOfIndexTrees = 4;
  (* max.  no.  of index trees (for different storage classes) the system
     page should support *)

  PageBufferSize = 8;
  (* size of buffer for existent but unused pages *)


TYPE T = IndexPage.T;

PROCEDURE Init (page: T; trees: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Initialize a system page for 'trees' index trees.  This requires page
     to be an initialized index page. *)

PROCEDURE GetNoOfIndexTrees (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Return the number of index trees administered by page. *)

PROCEDURE NotifyIndexPageCreation (page: T; pageNo: CARDINAL)
  RAISES {Access.Locked, AlreadyUsed, IndexTreeOverflow,
          WrongIndexPageNumber, InternalError};
  (* Notify the system page that the index page pageNo was created. *)


PROCEDURE NotifyIndexPageDeletion (page: T; pageNo: CARDINAL)
  RAISES {Access.Locked, NotEmpty, IndexTreeUnderflow, InternalError};
  (* Notify the system page that the index page pageNo was deleted. *)


PROCEDURE NotifyDataPageCreation (page: T; indexTree: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Notify the system page that a data page in index tree indexTree was
     created. *)


PROCEDURE NotifyDataPageDeletion (page: T; indexTree: CARDINAL)
  RAISES {Access.Locked, SystemPageCorrupt, InternalError};
  (* Notify the system page that a data page in index tree indexTree was
     deleted. *)


PROCEDURE FindNonfullIndexPage (page: T; VAR exists: BOOLEAN): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Find an index page with a free node.  If such a page exists, its
     number is returned and exists is TRUE.  If there is no free index
     page, 0 will be returned and exists will be FALSE. *)


PROCEDURE NotifyNodeReservation (page: T; indexPageNo: CARDINAL)
  RAISES {Access.Locked, PageOverflow, InternalError};
  (* Notify the system page that there has been a node reservation on index
     page indexPageNo. *)


PROCEDURE NotifyNodeRelease (page: T; indexPageNo: CARDINAL)
  RAISES {Access.Locked, PageUnderflow, InternalError};
  (* Notify the system page that there has been a node release on index
     page indexPageNo. *)


PROCEDURE GetUnusedPage (page: T): CARDINAL
  RAISES {Access.Locked, NoUnusedPages, InternalError};
  (* Returns an unused page from the buffer. *)


PROCEDURE PutUnusedPage (page: T; pageNo: CARDINAL)
  RAISES {Access.Locked, BufferOverflow, InternalError};
  (* Enter page pageNo to the buffer of unused pages. *)


PROCEDURE NotifyExtension (page: T)
  RAISES {Access.Locked, BufferNotEmpty, InternalError};
  (* Notify the system page that new pages have been actually allocated.
     This operation may be called only if the buffer for unused pages is
     empty.  The buffer is filled with the new pages. *)


PROCEDURE NotifyTruncation (page: T)
  RAISES {Access.Locked, BufferNotFull, InternalError};
  (* Notify the system page that the unused pages have been deallocated.
     This operation may be called only if the buffer for unused pages is
     full.  The buffer will be emptied. *)


PROCEDURE GetIndexTreeRootNode (page: T; indexTree: CARDINAL):
  IndexPage.NodePosition RAISES {Access.Locked, InternalError};
  (* Get the node position of the root for index tree indexTree. *)


PROCEDURE NoOfPhysExistentPages (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the number of actually existent pages. *)


PROCEDURE NoOfUnusedPages (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the number of existing but unused pages. *)


PROCEDURE NoOfIndexPages (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the number of all index pages in use. *)


PROCEDURE NoOfDataPagesInTree (page: T; indexTree: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the number of data pages in a certain index tree. *)

EXCEPTION
  BufferNotFull;
  BufferNotEmpty;
  BufferOverflow;
  NoUnusedPages;
  PageOverflow;
  PageUnderflow;
  SystemPageCorrupt;
  IndexTreeUnderflow;
  IndexTreeOverflow;
  NotEmpty;
  AlreadyUsed;
  WrongIndexPageNumber;
  InternalError(AtomList.T);

END SystemPage.
