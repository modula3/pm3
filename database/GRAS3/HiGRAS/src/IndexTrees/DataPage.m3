UNSAFE MODULE DataPage;

(***************************************************************************)
(** Data pages are organized as follows:

  Byte                                  Entry

                         |==================================|
  IndexTree ..           | no. of index tree to which the   |
  IndexTree+3            | page belongs                     |
                         |==================================|
  Depth1 ..              | depth of the page regarding key1 |
  Depth1+3               |                                  |
                         |----------------------------------|
  Depth2 ..              | depth of the page regarding key2 |
  Depth2+3               |                                  |
                         |==================================|
  RelevantKey1 ..        | relevant part of first key       |
  RelevantKey1+3         |                                  |
                         |----------------------------------|
  RelevantKey2 ..        | relevant part of second key      |
  RelevantKey2+3         |                                  |
                         |==================================|
  DataSection            | the data part of the page        |
  .                      |                .                 |
  .                      |                .                 |
  .                      |                .                 |
  MemoryPage.Size        |                                  |
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

    Revision 1.2  1997/05/06 13:17:05  roland
    Bugfix: first (index) page number is 0 not 1.

    Revision 1.1  1997/03/26 11:24:55  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.7  1997/02/20 16:14:55  roland
    Performance improvement: depth1 and depth2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.

    Revision 1.6  1996/11/20 12:22:00  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.5  1996/08/06 16:25:27  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.1  1996/04/29 13:38:02  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.4  1993/10/26  19:48:15  pk
# Naming of methods and procedures updated.
#
# Revision 1.3  1993/09/30  15:15:24  pk
# Basic types moved from Physical/MemoryPage to Basics/Type.i3
#
# Revision 1.2  1993/08/19  18:36:06  pk
# page parameter for GetDataSection eliminated: must be constant anyway
# for StoragePage.
#
# Revision 1.1  1993/08/17  12:50:24  pk
# Abstract data type for data pages.
#
*)
(***************************************************************************)

IMPORT Type, PageData, Access;
IMPORT ErrorSupport, VirtualPage;

CONST
  (* positions on page *)
  IndexTree    = 1;
  Depth1       = IndexTree + 4;
  Depth2       = Depth1 + 4;
  RelevantKey1 = Depth2 + 4;
  RelevantKey2 = RelevantKey1 + 4;
  DataSection  = RelevantKey2 + 4;


TYPE
  DataArray = ARRAY [1 .. 5] OF CARDINAL;
  DataByteArray = ARRAY [1 .. 20] OF Type.Byte;
  KeyArray = ARRAY [1..2] OF CARDINAL;
  KeyByteArray = ARRAY [1..8] OF Type.Byte;

PROCEDURE PutIndexTreeNumber (page: T; indexTree: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      page.putInt(IndexTree, indexTree);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("DataPage.PutIndexTreeNumber",
                                       "VirtualPage.FatalError", info));
    END;
  END PutIndexTreeNumber;


PROCEDURE GetIndexTreeNumber (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN page.getInt(IndexTree);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("DataPage.GetIndexTreeNumber",
                                       "VirtualPage.FatalError", info));
    END;
  END GetIndexTreeNumber;


PROCEDURE PutDepths (page: T; depth1, depth2: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR ka: KeyArray;
  BEGIN
    TRY
      ka[1] := depth1; ka[2] := depth2;
      page.putArray(Depth1, LOOPHOLE(ka, KeyByteArray));
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "DataPage.PutDepths", "VirtualPage.FatalError", info));
    END;
  END PutDepths; 


PROCEDURE GetDepths (page: T; VAR depth1, depth2: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR ka: KeyArray;
  BEGIN
    TRY
      page.getArray(Depth1, LOOPHOLE(ka, KeyByteArray));
      depth1 := ka[1]; depth2 := ka[2];
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "DataPage.GetDepths", "VirtualPage.FatalError", info));
    END;
  END GetDepths;


PROCEDURE PutRelevantKey1 (page: T; key: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      page.putInt(RelevantKey1, key);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("DataPage.PutRelevantKey1",
                                       "VirtualPage.FatalError", info));
    END;
  END PutRelevantKey1;


PROCEDURE GetRelevantKey1 (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN page.getInt(RelevantKey1);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("DataPage.GetRelevantKey1",
                                       "VirtualPage.FatalError", info));
    END;
  END GetRelevantKey1;


PROCEDURE PutRelevantKey2 (page: T; key: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      page.putInt(RelevantKey2, key);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("DataPage.PutRelevantKey2",
                                       "VirtualPage.FatalError", info));
    END;
  END PutRelevantKey2;


PROCEDURE GetRelevantKey2 (page: T): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN page.getInt(RelevantKey2);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("DataPage.GetRelevantKey2",
                                       "VirtualPage.FatalError", info));
    END;
  END GetRelevantKey2;


PROCEDURE PutAll (page          : T;
                  indexTree     : CARDINAL;
                  depth1, depth2: CARDINAL;
                  key1, key2    : CARDINAL  )
  RAISES {Access.Locked, InternalError} =
  VAR data: DataArray;

  BEGIN
    data := DataArray{indexTree, depth1, depth2, key1, key2};
    TRY
      page.putArray(IndexTree, LOOPHOLE(data, DataByteArray));
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "DataPage.PutAll", "VirtualPage.FatalError", info));
    END;
  END PutAll;


PROCEDURE GetAll (    page          : T;
                  VAR indexTree     : CARDINAL;
                  VAR depth1, depth2: CARDINAL;
                  VAR key1, key2    : CARDINAL  )
  RAISES {Access.Locked, InternalError} =
  VAR data: DataArray;

  BEGIN
    TRY
      page.getArray(IndexTree, LOOPHOLE(data, DataByteArray));
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "DataPage.GetAll", "VirtualPage.FatalError", info));
    END;
    indexTree := data[1];
    depth1 := data[2];
    depth2 := data[3];
    key1 := data[4];
    key2 := data[5];
  END GetAll;


PROCEDURE GetDataSection (): PageData.Index =
  BEGIN
    RETURN DataSection;
  END GetDataSection;

BEGIN
END DataPage.
