INTERFACE RecordBase;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:27:49  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.3  1996/11/20 12:23:39  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:29:04  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/30 13:56:09  roland
    Exceptions for subsystem RecordStorage.

*)
(***************************************************************************)

IMPORT
  AtomList;

EXCEPTION
  InternalError(AtomList.T);

END RecordBase.
