MODULE RelationRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:34  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:29:29  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:23:21  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:25:06  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:30:17  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:59:36  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.4  1993/10/26  20:13:20  pk
# Naming of methods and procedures updated.
#
# Revision 1.3  1993/10/09  21:14:19  pk
# Key2 access routines corrected.
#
# Revision 1.2  1993/10/04  21:47:32  pk
# Key and Data types + comments corrected.
#
# Revision 1.1  1993/10/02  15:59:18  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Type, Access, Word;
IMPORT RecordBase, ErrorSupport, VirtualPage;

PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1     : Key1;
                   READONLY key2     : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  VAR buf: ARRAY [0 .. Key1Size + Key2Size - 1] OF Type.Byte;
  BEGIN
    TRY
      buf[0] := key1.order;
      buf[1] := Word.Extract(key1.entity1, 24, 8);
      buf[2] := Word.Extract(key1.entity1, 16, 8);
      buf[3] := Word.Extract(key1.entity1, 8, 8);
      buf[4] := Word.Extract(key1.entity1, 0, 8);
      buf[5] := Word.Extract(key1.overflowCounter, 16, 8);
      buf[6] := Word.Extract(key1.overflowCounter, 8, 8);
      buf[7] := Word.Extract(key1.overflowCounter, 0, 8);
      buf[8] := Word.Extract(key2, 24, 8);
      buf[9] := Word.Extract(key2, 16, 8);
      buf[10] := Word.Extract(key2, 8, 8);
      buf[11] := Word.Extract(key2, 0, 8);
      page.putArray(position, buf);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("RelationRecordParameter.PutKey1",
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
  VAR
    buf: ARRAY [0 .. Key1Size + Key2Size- 1] OF Type.Byte;
  BEGIN
    TRY
      page.getArray(position, buf);
      key1.order := buf[0];
      key1.entity1 := buf[1] * 256 + buf[2];
      key1.entity1 := key1.entity1 * 256 + buf[3];
      key1.entity1 := key1.entity1 * 256 + buf[4];
      key1.overflowCounter := (buf[5] * 256 + buf[6]) * 256 + buf[7];
      emptyKey1 := (key1.entity1 = 0);
      key2 := buf[8] * 256 + buf[9];
      key2 := key2 * 256 + buf[10];
      key2 := key2 * 256 + buf[11];
      emptyKey2 := (key2 = 0);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("RelationRecordParameter.GetKey1",
                                       "VirtualPage.FatalError", info));
    END
  END GetKeys;

PROCEDURE PutData (<* UNUSED *>          page    : DataPage.T;
                   <* UNUSED *>          position: PageData.Index;
                   <* UNUSED *> READONLY data    : Data            )
  RAISES {} =
  BEGIN
  END PutData;


PROCEDURE GetData (<* UNUSED *> page    : DataPage.T;
                   <* UNUSED *> position: PageData.Index): Data RAISES {} =
  BEGIN                          <* NOWARN *>(* I know this doesn't return
                                                anything *)
  END GetData;

BEGIN
END RelationRecordParameter.
