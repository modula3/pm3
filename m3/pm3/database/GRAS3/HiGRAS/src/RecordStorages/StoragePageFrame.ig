GENERIC INTERFACE StoragePageFrame(RecordParameter);

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:27:57  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:22:04  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:23:48  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:29:24  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:56:23  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.1  1993/10/02  15:59:36  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Access;
IMPORT RecordBase;

CONST
  DataSize     = RecordParameter.DataSize;
  HasSet       = RecordParameter.HasSet;
  HasAttribute = RecordParameter.HasAttribute;
  AvgSize      = RecordParameter.AvgSize;
  Extension    = RecordParameter.Extension;

  Key1Size         = RecordParameter.Key1Size;
  Key2Size         = RecordParameter.Key2Size;
  Key1Start        = 0;
  Key2Start        = Key1Start + Key1Size;
  DataStart        = Key2Start + Key2Size;
  AttributePointer = DataStart + DataSize;
  AttributeLength  = AttributePointer + 2;
  SetPointer       = DataStart + DataSize;
  SetCard          = SetPointer + 4;

  RecordNumber       = 0;
  AllocatedLength    = RecordNumber + 2;
  AttSetData         = AllocatedLength + 1;
  AttSetHeaderLength = AttSetData;

  Size = PageData.Size;


TYPE
  Key1 = RecordParameter.Key1;
  Key2 = RecordParameter.Key2;

  Data = RecordParameter.Data;


VAR
  staticSize     : PageData.Index;
  staticEntries  : PageData.Index;
  dynamicOverhead: PageData.Index;
  unusedStart    : PageData.Index;
  staticStart    : PageData.Index;

  recordLength      : CARDINAL;
  defaultNoOfRecords: CARDINAL;


PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1    : Key1;
                   READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError};
  (* Write key1, key2 to position + Key1Start.  If HasAttribute, attribute
     pointer and length are initialized to 0, if HasSet, set pointer and
     size are set to 0 *)


PROCEDURE GetKeys (    page     : DataPage.T;
                       position : PageData.Index;
                   VAR key1     : Key1;
                   VAR emptyKey1: BOOLEAN;
                   VAR key2     : Key2;
                   VAR emptyKey2: BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError};


PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError};


PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError};

END StoragePageFrame.
