MODULE EntityRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:28:43  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:22:43  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:24:27  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:29:52  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:58:09  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.2  1993/10/26  20:13:15  pk
# Naming of methods and procedures updated.
#
# Revision 1.1  1993/10/04  21:44:08  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Access;
IMPORT RecordBase, VirtualPage, ErrorSupport;

PROCEDURE PutKeys (                      page    : DataPage.T;
                                         position: PageData.Index;
                                READONLY key1    : Key1;
                   <* UNUSED *> READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    TRY
      page.putInt(position, key1);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("EntityRecordParameter.PutKey1",
                                       "VirtualPage.FatalError", info));
    END;
  END PutKeys;


PROCEDURE GetKeys (                 page     : DataPage.T;
                                    position : PageData.Index;
                                VAR key1     : Key1;
                                VAR emptyKey1: BOOLEAN;
                   <* UNUSED *> VAR key2     : Key2;
                                VAR emptyKey2: BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    TRY
      key1 := page.getInt(position);
      emptyKey1 := (key1 = 0);
      emptyKey2 := emptyKey1;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("EntityRecordParameter.GetKey1",
                                       "VirtualPage.FatalError", info));
    END;
  END GetKeys;


PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    TRY
      page.putInt(position, data);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("EntityRecordParameter.PutData",
                                       "VirtualPage.FatalError", info));
    END;
  END PutData;


PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    TRY
      RETURN page.getInt(position);
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("EntityRecordParameter.GetData",
                                       "VirtualPage.FatalError", info));
    END;
  END GetData;

BEGIN
END EntityRecordParameter.
