INTERFACE DataPage;

(***************************************************************************)
(* DataPages are special MemoryPages used for storage of data.  They form
   the leaves of the index tree.  Conceptually DataPage should be a subtype
   of VirtualPage.  But since memory for VirtualPages may only be allocated
   by the scheduler, DataPages are merely a refernce to VirtualPages. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:24:54  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.5  1997/02/20 16:14:53  roland
    Performance improvement: depth1 and depth2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.

    Revision 1.4  1996/11/20 12:21:59  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/08/06 16:25:26  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/04/29 13:38:00  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.2  1993/08/19  18:36:05  pk
# page parameter for GetDataSection eliminated: must be constant anyway
# for StoragePage.
#
# Revision 1.1  1993/08/17  12:50:21  pk
# Abstract data type for data pages.
#
*)
(***************************************************************************)

IMPORT VirtualPage;
IMPORT PageData, Access;
IMPORT AtomList;

TYPE T = VirtualPage.T;

PROCEDURE PutIndexTreeNumber (page: T; indexTree: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Store the information to which index tree the page belongs. *)


PROCEDURE GetIndexTreeNumber (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Return the index tree to which the page belongs. *)


PROCEDURE PutDepths (page: T; depth1, depth2: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Store the depth of the page regarding key 1 and key 2. *)


PROCEDURE GetDepths (page: T; VAR depth1, depth2: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Return the depth of the page regarding key 1 and key 2. *)


PROCEDURE PutRelevantKey1 (page: T; key: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Store the relevant part of the first key. *)


PROCEDURE GetRelevantKey1 (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Return the relevant part of the first key. *)


PROCEDURE PutRelevantKey2 (page: T; key: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Store the relevant part of the second key. *)


PROCEDURE GetRelevantKey2 (page: T): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Return the relevant part of the second key. *)


PROCEDURE PutAll (page          : T;
                  indexTree     : CARDINAL;
                  depth1, depth2: CARDINAL;
                  key1, key2    : CARDINAL  )
  RAISES {Access.Locked, InternalError};
  (* Store the complete page information. *)


PROCEDURE GetAll (    page          : T;
                  VAR indexTree     : CARDINAL;
                  VAR depth1, depth2: CARDINAL;
                  VAR key1, key2    : CARDINAL  )
  RAISES {Access.Locked, InternalError};
  (* Retrieve the complete page information. *)


PROCEDURE GetDataSection (): PageData.Index;
  (* Return the index of the first free byte on the page. *)

EXCEPTION
  InternalError(AtomList.T);

END DataPage.
