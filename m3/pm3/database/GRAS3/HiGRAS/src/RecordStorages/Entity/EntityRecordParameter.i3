INTERFACE EntityRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:28:42  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:22:41  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:24:26  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:29:50  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:58:05  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.1  1993/10/04  21:44:06  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Access;
IMPORT RecordBase;

CONST
  DataSize     = BYTESIZE(Data);
  HasSet       = FALSE;
  HasAttribute = TRUE;           (* entity attribute *)
  AvgSize      = 5;
  Extension    = 0;
  Key1Size     = BYTESIZE(Key1);
  Key2Size     = 0;


TYPE
  Key1 = CARDINAL;               (* entity number *)
  Key2 = [0 .. 0];

  Data = CARDINAL;               (* entity label *)


PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1    : Key1;
                   READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE GetKeys (    page     : DataPage.T;
                       position : PageData.Index;
                   VAR key1     : Key1;
                   VAR emptyKey1: BOOLEAN;
                   VAR key2     : Key2;
                   VAR emptyKey : BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError};

END EntityRecordParameter.
