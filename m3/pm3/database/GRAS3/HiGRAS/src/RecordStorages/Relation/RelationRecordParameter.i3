INTERFACE RelationRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:29:28  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.5  1997/02/20 16:23:20  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.4  1996/12/03 09:49:08  roland
    Replaced Type.Triple with CARDINAL. Special handling in put/getTriple
    of VirtualPage.

    Revision 1.3  1996/11/20 12:25:05  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:30:16  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:59:34  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.4  1993/11/14  16:56:40  pk
# Value of set extension constant changed.
#
# Revision 1.3  1993/10/04  21:47:30  pk
# Key and Data types + comments corrected.
#
# Revision 1.2  1993/10/02  19:11:21  pk
# Types for Data corrected.
#
# Revision 1.1  1993/10/02  15:59:16  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Type, Access;
IMPORT RecordBase;

CONST
  DataSize     = 0;
  HasSet       = TRUE;           (* third entities *)
  HasAttribute = FALSE;
  AvgSize      = 4;
  Extension    = 50;
  Key1Size     = 8;
  Key2Size     = BYTESIZE(Key2);


TYPE
  Key1 = RECORD
           order          : Type.Byte;
           entity1        : CARDINAL;
           overflowCounter: CARDINAL (* Type.Triple *);
         END;
  Key2 = CARDINAL;               (* entity2 *)

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
                   VAR key2     : Key2;
                   VAR emptyKey2: BOOLEAN         )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE PutData (         page    : DataPage.T;
                            position: PageData.Index;
                   READONLY data    : Data            )
  RAISES {Access.Locked, RecordBase.InternalError};

PROCEDURE GetData (page: DataPage.T; position: PageData.Index): Data
  RAISES {Access.Locked, RecordBase.InternalError};

END RelationRecordParameter.
