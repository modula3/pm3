MODULE NameRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:29:06  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:23:02  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:24:46  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:30:05  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:58:58  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.4  1993/10/26  20:13:16  pk
# Naming of methods and procedures updated.
#
# Revision 1.3  1993/10/04  21:47:25  pk
# Key and Data types + comments corrected.
#
# Revision 1.2  1993/10/02  19:11:19  pk
# Types for Data corrected.
#
# Revision 1.1  1993/10/02  15:58:49  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Access, Type, Word;
IMPORT RecordBase, ErrorSupport, VirtualPage;

TYPE
  KeyByteArray = ARRAY [0..Key1Size + Key2Size -1] OF Type.Byte;

PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1    : Key1;
                   READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  VAR buf: KeyByteArray;
  BEGIN
    TRY
      buf[0] := Word.Extract(key1, 24, 8);
      buf[1] := Word.Extract(key1, 16, 8);
      buf[2] := Word.Extract(key1,  8, 8);
      buf[3] := Word.Extract(key1,  0, 8);
      buf[4] := Word.Extract(key2, 24, 8);
      buf[5] := Word.Extract(key2, 16, 8);
      buf[6] := Word.Extract(key2,  8, 8);
      buf[7] := Word.Extract(key2,  0, 8);
      page.putArray(position, buf);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("NameRecordParameter.PutKey1",
                                       "VirtualPage.FatalError", info));
    END
  END PutKeys;


PROCEDURE GetKeys (    page     : DataPage.T;
                       position : PageData.Index;
                   VAR key1     : Key1;
                   VAR emptyKey1: BOOLEAN;
                   VAR key2     : Key2;
                   VAR emptyKey2: BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError} =
  VAR buf: KeyByteArray;
  BEGIN
    TRY
      page.getArray(position, buf);
      key1 := buf[0] * 256 + buf[1];
      key1 := key1 * 256 + buf[2];
      key1 := key1 * 256 + buf[3];
      key2 := buf[4] * 256 + buf[5];
      key2 := key2 * 256 + buf[6];
      key2 := key2 * 256 + buf[7];
      emptyKey1 := (key1 = 0);
      emptyKey2 := (key2 = 0);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("NameRecordParameter.GetKey1",
                                       "VirtualPage.FatalError", info));
    END
  END GetKeys;

PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    TRY
      page.putInt(position, data.entity);
      page.putInt(position + BYTESIZE(CARDINAL), data.tag);

    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("NameRecordParameter.PutData",
                                       "VirtualPage.FatalError", info));
    END
  END PutData;


PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError} =
  VAR data: Data;

  BEGIN
    TRY
      data.entity := page.getInt(position);
      data.tag := page.getInt(position + BYTESIZE(CARDINAL));
      RETURN data;

    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("NameRecordParameter.GetData",
                                       "VirtualPage.FatalError", info));
    END
  END GetData;

BEGIN
END NameRecordParameter.
