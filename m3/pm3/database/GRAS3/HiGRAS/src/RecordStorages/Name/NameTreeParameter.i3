INTERFACE NameTreeParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:29:07  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.2  1996/08/06 16:30:06  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:59:11  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.2  1993/10/04  21:47:27  pk
# Key and Data types + comments corrected.
#
# Revision 1.1  1993/10/02  15:59:01  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT NameRecordParameter;


CONST
  Key2HashMask = 0;              (* use full key2 for hashing *)


PROCEDURE ComputePageKeys (READONLY key1: NameRecordParameter.Key1;
                           READONLY key2: NameRecordParameter.Key2;
                           VAR pageKey1, pageKey2: CARDINAL);


PROCEDURE ComputeRecordKeys (VAR key1: NameRecordParameter.Key1;
                             VAR key2: NameRecordParameter.Key2;
                             pageKey1, pageKey2: CARDINAL);

END NameTreeParameter.
