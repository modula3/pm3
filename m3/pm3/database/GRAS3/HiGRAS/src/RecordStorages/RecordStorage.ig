GENERIC INTERFACE RecordStorage(RecordParameter);

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:27:51  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.7  1997/02/20 16:21:56  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.6  1996/11/20 12:23:40  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.5  1996/08/06 16:29:05  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.2  1996/07/24 09:17:10  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.4.2.1  1996/04/29 13:56:09  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.4  1993/11/09  18:00:01  pk
# Comment added.
#
# Revision 1.3  1993/10/04  22:12:11  pk
# MaxElements passed through to RecordStorage.i3
#
# Revision 1.2  1993/10/02  18:54:12  pk
# Parameter to LocateNeighbour changed from location to file.
#
# Revision 1.1  1993/10/02  15:59:05  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT AtomList;
IMPORT Access, ITPFile;


TYPE
  RecordLocation = RECORD        (* Describes a record location *)
                     file: ITPFile.T;  (* in the file.  This is opaque. *)
                     tree: CARDINAL;
                     pageNo  : CARDINAL;
                     recordNo: CARDINAL;
                   END;

  Range = RECORD                 (* Describes a search range *)
            file    : ITPFile.T;  (* in the record storage. *)
            tree    : CARDINAL;   (* This is opaque. *)
            pageNo  : CARDINAL;
            recordNo: CARDINAL;
            key1              : RecordParameter.Key1;
            pageKey1, pageKey2: CARDINAL;
            size              : CARDINAL;
          END;


VAR
  (* CONST *)
  MaxElements: CARDINAL;         (* maximum number of elements in a set /
                                    characters in an attribute *)


PROCEDURE Init (file: ITPFile.T; tree: CARDINAL)
  RAISES {Access.Locked, LevelError, InternalError};
  (* Initialize the record storage. *)


PROCEDURE PutRecord (VAR      location: RecordLocation;
                     READONLY key1    : RecordParameter.Key1;
                     READONLY key2    : RecordParameter.Key2  )
  RAISES {Access.Locked, NotFree, InternalError};
  (* Store a record with the given keys at location. *)


PROCEDURE DeleteRecord (READONLY location: RecordLocation)
  RAISES {Access.Locked, InternalError};
  (* Delete the record at location. *)


PROCEDURE FindRecord (         file : ITPFile.T;
                               tree : CARDINAL;
                      READONLY key1 : RecordParameter.Key1;
                      READONLY key2 : RecordParameter.Key2;
                      VAR      found: BOOLEAN               ):
  RecordLocation RAISES {Access.Locked, InternalError};
  (* Returns the location of the record with the given keys in file. *)


PROCEDURE LocateNeighbourPosition (    file: ITPFile.T;
                                       tree: CARDINAL;
                                   VAR key1: RecordParameter.Key1;
                                   VAR key2: RecordParameter.Key2  ):
  RecordLocation RAISES {Access.Locked, InternalError};
  (* Returns the location and the keys of a record somewhere near to the
     record described by key1 and key2. *)


PROCEDURE GetKeys (READONLY location: RecordLocation;
                   VAR      key1    : RecordParameter.Key1;
                   VAR      key2    : RecordParameter.Key2  )
  RAISES {Access.Locked, InternalError};
  (* Retrieve the keys of the record at location. *)


PROCEDURE PutData (READONLY location: RecordLocation;
                   READONLY data    : RecordParameter.Data)
  RAISES {Access.Locked, InternalError};
  (* Store data for the record at location. *)


PROCEDURE GetData (READONLY location: RecordLocation): RecordParameter.Data
  RAISES {Access.Locked, InternalError};
  (* Returns the data at the record at location. *)


PROCEDURE PutAttribute (VAR location: RecordLocation; attribute: TEXT)
  RAISES {Access.Locked, InternalError};
  (* Stores the attribute for the record at location.  Note that the
     attribute may move location during the operation. *)


PROCEDURE GetAttribute (READONLY location: RecordLocation): TEXT
  RAISES {Access.Locked, InternalError};
  (* Returns the attribute at the record at location. *)


PROCEDURE PutSetElement (VAR location : RecordLocation;
                         VAR element  : CARDINAL;
                         VAR displaced: BOOLEAN         )
  RAISES {Access.Locked, NotFree, InternalError};
  (* Stores an element in the set at the record at location.  If the set is
     full, the new element is stored, the greatest element of the set is
     removed and returned in element, and displaced will is set to
     TRUE.Note that the attribute may move location during the
     operation. *)


PROCEDURE GetSetElement (READONLY location : RecordLocation;
                                  elementNo: CARDINAL        ): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the elementNo'th element of the set at the record at
     location. *)


PROCEDURE DeleteSetElement (VAR location: RecordLocation; element: CARDINAL)
  RAISES {Access.Locked, NotFound, InternalError};
  (* Removes element from the set at the record at location. *)


PROCEDURE SetCardinality (READONLY location: RecordLocation): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Returns the cardinality of the set at the record at location. *)


PROCEDURE IsElementInSet (READONLY location: RecordLocation;
                                   element : CARDINAL        ): BOOLEAN
  RAISES {Access.Locked, InternalError};
  (* Returns TRUE if element is in the set at the record at location. *)


PROCEDURE InitRange (         file: ITPFile.T;
                              tree: CARDINAL;
                     READONLY key1: RecordParameter.Key1): Range
  RAISES {Access.Locked, InternalError};
  (* Returns a range for a loop through all records with key1.  Note that
     the range may become invalid after inserting or deleting a record. *)


PROCEDURE ResetRange (VAR range: Range)
  RAISES {Access.Locked, InternalError};
  (* Reset range to start from the beginning. *)


PROCEDURE SearchInRange (VAR      range: Range;
                         READONLY key2 : RecordParameter.Key2;
                         VAR      found: BOOLEAN               ):
  RecordLocation RAISES {Access.Locked, InternalError};
  (* Returns a record with key2 in the given range. *)


PROCEDURE GetNextInRange (VAR range: Range; VAR found: BOOLEAN)
  RAISES {Access.Locked, InternalError};
  (* Let the current range pointer advance to the next record. *)


PROCEDURE GetCurrentFromRange (READONLY range: Range): RecordLocation;
  (* Returns the location of the current record in range. *)

EXCEPTION
  LevelError;
  NotFree;
  NotFound;
  InternalError(AtomList.T);

END RecordStorage.
