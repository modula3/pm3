INTERFACE AttributeRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:15:03  roland
    Modified code to remove compiler warnings.

    Revision 1.1  1997/03/26 11:28:20  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:22:23  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/12/03 09:48:48  roland
    Replaced Type.Triple with CARDINAL. Special handling in put/getTriple
    of VirtualPage.

    Revision 1.2  1996/11/20 12:24:08  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/04/29 13:57:30  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.1  1993/10/04  21:43:47  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, Access, PageData;
IMPORT RecordBase;

CONST
  DataSize     = 0;
  HasSet       = FALSE;
  HasAttribute = TRUE;           (* the attribute *)
  AvgSize      = 20;
  Extension    = 0;
  Key1Size     = BYTESIZE(Key1);
  Key2Size     = 7;


TYPE
  Key1 = CARDINAL;               (* the entity which has the attribute *)
  Key2 = RECORD
           attributeNo: CARDINAL;
           recordNo   : CARDINAL (* Type.Triple *);
         END;

  Data = [0 .. 0];


PROCEDURE PutKeys (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY key1    : Key1;
                   READONLY key2    : Key2            )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE GetKeys (    page     : DataPage.T;
                       position : PageData.Index;
                   VAR key1     : Key1;
                   VAR emptyKey1: BOOLEAN;
                   VAR ke2      : Key2;
                   VAR emptyKey2: BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError};


PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError};

END AttributeRecordParameter.
