GENERIC INTERFACE StoragePage(RecordParameter);

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:27:54  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.8  1996/11/20 12:23:44  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.7  1996/08/06 16:29:20  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.2  1996/07/24 09:17:14  rbnix
    	Error handling adjusted: internal errors are now guarded by
    	assertions rather than exceptions. This should simplify
    	locating errors.

    Revision 1.6.2.1  1996/04/29 13:56:20  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.6  1993/11/15  12:05:20  pk
# Optimal/CurrentNoOfRecords replaced by InitAsMerge/SplitTarget.
# CompressSets/Attributes now reduces the overhead really to 0.
#
# Revision 1.5  1993/11/09  19:45:21  pk
# New procedure CurrentNoOfRecords.
#
# Revision 1.4  1993/11/03  21:37:21  pk
# PutAttribute checks for attribute # NIL.
#
# Revision 1.3  1993/10/19  18:33:46  pk
# Some checks concerning the recordNo removed; they couldn't be
# processed ordinary since recordNo's are not continuous.
#
# Revision 1.2  1993/10/04  22:12:15  pk
# MaxElements passed through to RecordStorage.i3
#
# Revision 1.1  1993/10/02  15:59:32  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT
  Type,
  Access,
  DataPage;
IMPORT
  AtomList;

TYPE T = DataPage.T;


CONST
  MaxElements = Type.MaxByte;    (* maximum number of elements in a set /
                                    characters in an attribute *)


VAR defaultNoOfRecords: CARDINAL;


PROCEDURE Init (page: T; noOfRecords: CARDINAL) RAISES {Access.Locked, InternalError};
  (* Initialize page as storage page for noOfRecords records. *)


PROCEDURE InitAsMergeTarget (sourcePage1, sourcePage2: T; targetPage: T)
  RAISES {Access.Locked, InternalError};
  (* Initialize targetPage with a suitable layout for merging the
     sourcePages to targetPage. *)


PROCEDURE InitAsSplitTarget (sourcePage: T;
                             targetPage: T;
                             keepFree  : CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Initialize targetPage with a suitable layout for splitting sourcePage.
     Additional keepFree bytes are kept free for an attribute/set on
     targetPage. *)


PROCEDURE MaxNoOfRecords (page: T): CARDINAL RAISES {Access.Locked, InternalError};
  (* Returns the no.  of records which can be stored on the page. *)


PROCEDURE Usage (page: T): [0 .. 100] RAISES {Access.Locked, InternalError};
  (* Returns the current usage of the page in percent. *)


PROCEDURE PutKeys (         page    : T;
                            recordNo: CARDINAL;
                   READONLY key1    : RecordParameter.Key1;
                   READONLY key2    : RecordParameter.Key2  )
  RAISES {Access.Locked, InternalError};
  (* Store the keys for a new entry.  The record is implicitely created. *)


PROCEDURE GetKeys (    page    : T;
                       recordNo: CARDINAL;
                   VAR key1    : RecordParameter.Key1;
                   VAR key2    : RecordParameter.Key2;
                   VAR emptyKey: BOOLEAN               )
  RAISES {Access.Locked, InternalError};
  (* Retrieve the keys of entry recordNo.  If the keys denote a nonexisting
     record, emptyKey returns TRUE. *)


PROCEDURE PutData (         page    : T;
                            recordNo: CARDINAL;
                   READONLY data    : RecordParameter.Data)
  RAISES {Access.Locked, InternalError};
  (* Store the data for entry recordNo. *)


PROCEDURE GetData (page: T; recordNo: CARDINAL): RecordParameter.Data
  RAISES {Access.Locked, InternalError};
  (* Retrieve the data of entry recordNo. *)


PROCEDURE DeleteEntry (page: T; recordNo: CARDINAL) RAISES {Access.Locked, InternalError};
  (* Delete the entry recordNo. *)


PROCEDURE MoveEntry (page: T; from, to: CARDINAL) RAISES {Access.Locked, InternalError};
  (* Move the entry at from to to.  The old entry is deleted. *)


PROCEDURE PutAttribute (    page     : T;
                            recordNo : CARDINAL;
                            attribute: TEXT;
                        VAR overflow : BOOLEAN   )
  RAISES {Access.Locked, InternalError};
  (* Store attribute for entry recordNo.  If the new attribute does not fit
     on the page, the page remains unchanged and overflow is set to
     TRUE. *)


PROCEDURE GetAttribute (page: T; recordNo: CARDINAL): TEXT
  RAISES {Access.Locked, InternalError};
  (* Retrieve the attribute at entry recordNo. *)


PROCEDURE AttributeLength (page: T; recordNo: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the length of the attribute at entry recordNo. *)


PROCEDURE PutSet (    page    : T;
                      recordNo: CARDINAL;
                      set     : REF Type.ByteArray;
                  VAR overflow: BOOLEAN             )
  RAISES {Access.Locked, InternalError};
  (* Store set for entry recordNo.  If the new set does not fit on the
     page, the page remains unchanged and overflow is set to TRUE. *)


PROCEDURE GetSet (page: T; recordNo: CARDINAL): REF Type.ByteArray
  RAISES {Access.Locked, InternalError};
  (* Retrieve the set at entry recordNo. *)


PROCEDURE SetCardinality (page: T; recordNo: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the cardinality of the set at entry recordNo. *)


PROCEDURE PutSetElement (    page     : T;
                             recordNo : CARDINAL;
                         VAR element  : CARDINAL;
                         VAR existent : BOOLEAN;
                         VAR overflow : BOOLEAN;
                         VAR displaced: BOOLEAN   )
  RAISES {Access.Locked, InternalError};
  (* Store an element in the set for entry recordNo.  If the element
     already exists, the page remains unchanged and existent is set to
     TRUE.  If the page cannot hold the new set, the page remains unchanged
     and overflow is set to TRUE.  If the set has reached its maximum size,
     the new element is inserted and the greatest element of the set is
     displaced.  In the latter case, displaced is set to TRUE and element
     returns the displaced element. *)


PROCEDURE GetSetElement (page: T; recordNo: CARDINAL; elementNo: CARDINAL):
  CARDINAL RAISES {Access.Locked, InternalError};
  (* Get the elementNo'th element of the set for entry recordNo. *)


PROCEDURE DeleteSetElement (page: T; recordNo: CARDINAL; element: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Remove element from the set for entry recordNo. *)


PROCEDURE GetCard (page: T; recordNo: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the cardinality of the set for entry recordNo. *)


PROCEDURE IsElementInSet (page: T; recordNo: CARDINAL; element: CARDINAL):
  BOOLEAN RAISES {Access.Locked, InternalError};
  (* Returns TRUE if element is in the set for entry recordNo. *)

EXCEPTION
  InternalError(AtomList.T);
  
END StoragePage.
