GENERIC MODULE StoragePageFrame(RecordParameter);

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.3  1997/11/19 17:59:33  roland
    Removed grouping of page accesses.

    Revision 1.2  1997/09/18 08:23:25  roland
    Grouping of access to the same page.

    Revision 1.1  1997/03/26 11:27:58  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:22:05  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:23:49  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:29:25  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:56:25  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.1  1993/10/02  15:59:38  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Access;
IMPORT RecordBase, VirtualPage, ErrorSupport;

VAR bytesPerRecord: CARDINAL;


PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1    : Key1;
                   READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    TRY
      RecordParameter.PutKeys(page, position + Key1Start, key1, key2);
      IF HasAttribute THEN
        page.putTriple(AttributePointer + position, 0);
      ELSIF HasSet THEN
        page.putTriple(SetPointer + position, 0);
      END;
    EXCEPT
      VirtualPage.FatalError (info) =>
        RAISE RecordBase.InternalError(
                ErrorSupport.Propagate("StoragePageFrame.PutKeys",
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
  BEGIN
    RecordParameter.GetKeys(
      page, position + Key1Start, key1, emptyKey1, key2, emptyKey2);
  END GetKeys;

PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    RecordParameter.PutData(page, position, data);
  END PutData;


PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError} =
  BEGIN
    RETURN RecordParameter.GetData(page, position);
  END GetData;


BEGIN
  <* ASSERT NOT(HasAttribute AND HasSet) *>

  staticSize := DataPage.GetDataSection();
  staticEntries := staticSize + 4;
  dynamicOverhead := staticEntries + 4;
  unusedStart := dynamicOverhead + 4;
  staticStart := unusedStart + 4;

  IF (HasAttribute) THEN
    recordLength := AttributeLength + 1;
  ELSIF (HasSet) THEN
    recordLength := SetCard + 1;
  ELSE
    recordLength := AttributePointer;
  END;

  bytesPerRecord := recordLength;
  IF (HasAttribute) THEN
    INC(bytesPerRecord, AvgSize + AttSetHeaderLength);
  ELSIF (HasSet) THEN
    INC(bytesPerRecord, AvgSize * BYTESIZE(CARDINAL) + AttSetHeaderLength);
  END;

  WITH freeBytes = Size - staticStart + 1 DO
    defaultNoOfRecords := freeBytes DIV bytesPerRecord;
  END;
END StoragePageFrame.
