INTERFACE IndexPage;

(***************************************************************************)
(* IndexPages are special MemoryPages used for storage of index nodes.
   Conceptually IndexPage should be a subtype of VirtualPage.  But since
   memory for VirtualPages may only be allocated by the scheduler,
   IndexPages are merely a refernce to VirtualPages. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:25:08  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.5  1996/11/20 12:22:07  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.4  1996/08/06 16:25:36  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.2  1996/07/24 09:18:47  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.3.2.1  1996/04/29 13:38:11  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.3  1993/11/03  21:13:59  pk
# Parameter type for splitKey changed from CARDINAL to [1 .. 2].
#
# Revision 1.2  1993/08/17  15:45:48  pk
# Some exception handling added.
#
# Revision 1.1  1993/08/17  12:52:09  pk
# Abstract data type for index pages.
#
*)
(***************************************************************************)

IMPORT VirtualPage;
IMPORT PageData, Access;
IMPORT AtomList;

CONST
  NodeSize = 3 + 3 + 1;          (* 3 byte for each child plus one for
                                    status *)
  PointerSize = 4;
  Size = (PageData.Size - 2 * PointerSize) DIV NodeSize;
  (* Maximum number of nodes on an index page.  Two pointers are on each
     page: LastUsed and DeprivedStart. *)


TYPE
  NodePosition = [1 .. Size];

  Entry = OBJECT END;
  PageRefEntry = Entry OBJECT pageNo: CARDINAL;  END;
  NodeRefEntry = Entry OBJECT
                   splitKey    : [1 .. 2];
                   indexPageNo : CARDINAL;
                   indexPagePos: NodePosition;
                 END;
    (* entry types for index page nodes *)

  T = VirtualPage.T;

PROCEDURE Init (page: T) RAISES {Access.Locked, InternalError};
  (* Initialize page for usage as an index page. *)


PROCEDURE Reserve (page: T): NodePosition
  RAISES {Access.Locked, Overflow, InternalError};
  (* Reserve a node.  The position of the node is returned. *)


PROCEDURE Release (page: T; nodePosition: NodePosition)
  RAISES {Access.Locked, InternalError};
  (* Mark the node at nodePosition as unused. *)


PROCEDURE Deprive (page: T; bytes: CARDINAL): PageData.Index
  RAISES {Access.Locked, AlreadyDeprived, Overflow, InternalError};
  (* On the page, bytes no.  of bytes are blocked for use as index nodes.
     They may be modified as in a MemoryPage.  The start index of the
     reserved section is returned.  There may be only one deprived section
     on each page. *)


PROCEDURE Return (page: T)
  RAISES {Access.Locked, NotDeprived, InternalError};
  (* Return previously deprived bytes on the page. *)


PROCEDURE DeprivedSectionStart (page: T): PageData.Index
  RAISES {Access.Locked, InternalError};
  (* Returns the start index of the deprived section. *)


PROCEDURE NoOfFreeEntries (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Return the number of free entries on the page. *)


PROCEDURE GetEntry (page: T; nodePosition: NodePosition; rightNode: BOOLEAN):
  Entry RAISES {Access.Locked, StatusViolation, InternalError};
  (* Get an index page entry at nodePosition.  If rightNode is TRUE, the
     right successor is returned, otherwise the left successor. *)


PROCEDURE PutEntry (page        : T;
                    nodePosition: NodePosition;
                    rightNode   : BOOLEAN;
                    pageEntry   : Entry         )
  RAISES {Access.Locked, StatusViolation, WrongEntry, InternalError};
  (* Store an index page entry at nodePosition.  If rightNode is TRUE, the
     right successor is modified, otherwise the left successor. *)

EXCEPTION
  WrongEntry;
  StatusViolation;
  Overflow;
  AlreadyDeprived;
  NotDeprived;
  InternalError(AtomList.T);

END IndexPage.
