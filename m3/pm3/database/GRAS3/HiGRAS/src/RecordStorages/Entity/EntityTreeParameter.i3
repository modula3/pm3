INTERFACE EntityTreeParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:28:45  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.2  1996/08/06 16:29:53  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:58:31  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.1  1993/10/04  21:44:17  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT EntityRecordParameter;


CONST
  Key2HashMask = LAST(CARDINAL); (* don't use key2 for hashing (there is no
                                    key2) *)


PROCEDURE ComputePageKeys (READONLY key1: EntityRecordParameter.Key1;
                           READONLY key2: EntityRecordParameter.Key2;
                           VAR pageKey1, pageKey2: CARDINAL);


PROCEDURE ComputeRecordKeys (VAR key1: EntityRecordParameter.Key1;
                             VAR key2: EntityRecordParameter.Key2;
                             pageKey1, pageKey2: CARDINAL);

END EntityTreeParameter.
