INTERFACE NameRecordParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:29:05  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:23:01  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1996/11/20 12:24:45  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:30:04  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:58:56  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.3  1993/10/04  21:47:23  pk
# Key and Data types + comments corrected.
#
# Revision 1.2  1993/10/02  19:11:17  pk
# Types for Data corrected.
#
# Revision 1.1  1993/10/02  15:58:48  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT DataPage, PageData, Access;
IMPORT RecordBase;


CONST
  DataSize     = 8;
  HasSet       = FALSE;
  HasAttribute = TRUE;           (* the name *)
  AvgSize      = 20;
  Extension    = 0;
  Key1Size     = BYTESIZE(Key1);
  Key2Size     = BYTESIZE(Key2);


TYPE
  Key1 = CARDINAL;               (* hash value computed over the name *)
  Key2 = CARDINAL;               (* collision counter for hashed values *)

  Data = RECORD
           entity: CARDINAL;     (* the entity which has the name *)
           tag   : CARDINAL;     (* the tag of the name *)
         END;


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

END NameRecordParameter.
