MODULE AttributeRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:28:21  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.3  1997/02/20 16:22:24  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.2  1996/11/20 12:24:09  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/04/29 13:57:31  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.2  1993/10/26  20:13:13  pk
# Naming of methods and procedures updated.
#
# Revision 1.1  1993/10/04  21:43:48  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, Access, PageData, Type, Word;
IMPORT RecordBase, ErrorSupport, VirtualPage;


PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1    : Key1;
                   READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  VAR buf: ARRAY [0 .. Key2Size + Key1Size - 1] OF Type.Byte;
  BEGIN
    buf[0] := Word.Extract(key1, 24, 8);
    buf[1] := Word.Extract(key1, 16, 8);
    buf[2] := Word.Extract(key1, 8, 8);
    buf[3] := Word.Extract(key1, 0, 8);
    buf[4] := Word.Extract(key2.attributeNo, 24, 8);
    buf[5] := Word.Extract(key2.attributeNo, 16, 8);
    buf[6] := Word.Extract(key2.attributeNo, 8, 8);
    buf[7] := Word.Extract(key2.attributeNo, 0, 8);
    buf[8] := Word.Extract(key2.recordNo, 16, 8);
    buf[9] := Word.Extract(key2.recordNo, 8, 8);
    buf[10] := Word.Extract(key2.recordNo, 0, 8);
    TRY
      page.putArray(position, buf);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("AttributeRecordParameter.PutKey1",
                                       "VirtualPage.FatalError", info));
    END;
  END PutKeys;


PROCEDURE GetKeys (    page     : DataPage.T;
                       position : PageData.Index;
                   VAR key1     : Key1;
                   VAR emptyKey1: BOOLEAN;
                   VAR key2     : Key2;
                   VAR emptyKey2: BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError} =
  VAR buf: ARRAY [0 .. Key2Size + Key1Size - 1] OF Type.Byte;
  BEGIN
    TRY
      page.getArray(position, buf);
      key1 := buf[0] * 256 + buf[1];
      key1 := key1 * 256 + buf[2];
      key1 := key1 * 256 + buf[3];
      emptyKey1 := (key1 = 0);
      key2.attributeNo := buf[4] * 256 + buf[5];
      key2.attributeNo := key2.attributeNo * 256 + buf[6];
      key2.attributeNo := key2.attributeNo * 256 + buf[7];
      key2.recordNo := buf[8] * 256 + buf[9];
      key2.recordNo := key2.recordNo * 256 + buf[10];
      emptyKey2 := (key2.attributeNo = 0);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("AttributeRecordParameter.GetKey1",
                                       "VirtualPage.FatalError", info));
    END;
  END GetKeys;


PROCEDURE PutData (<* UNUSED *>          page    : DataPage.T;
                   <* UNUSED *>          position: PageData.Index;
                   <* UNUSED *> READONLY data    : Data            )
  RAISES <* NOWARN *> {Access.Locked} = (* exception never raised *)
  BEGIN
  END PutData;


PROCEDURE GetData (<* UNUSED *> page    : DataPage.T;
                   <* UNUSED *> position: PageData.Index): Data
  RAISES <* NOWARN *> {Access.Locked} = (* exception never raised *)
  BEGIN                          <* NOWARN *>(* I know this doesn't return
                                                anything *)
  END GetData;

BEGIN
END AttributeRecordParameter.
