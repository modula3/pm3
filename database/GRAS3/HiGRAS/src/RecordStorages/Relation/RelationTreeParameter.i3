INTERFACE RelationTreeParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:34  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:29:30  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.2  1996/08/06 16:30:19  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:59:49  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.2  1993/10/04  21:47:34  pk
# Key and Data types + comments corrected.
#
# Revision 1.1  1993/10/02  15:59:28  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT RelationRecordParameter;


CONST
  Key2HashMask = 7;              (* use lower three bits for hashing
                                    (approx.  7 entity2 values for one
                                    entity2 *)


PROCEDURE ComputePageKeys (READONLY key1: RelationRecordParameter.Key1;
                           READONLY key2: RelationRecordParameter.Key2;
                           VAR pageKey1, pageKey2: CARDINAL);


PROCEDURE ComputeRecordKeys (VAR key1: RelationRecordParameter.Key1;
                             VAR key2: RelationRecordParameter.Key2;
                             pageKey1, pageKey2: CARDINAL);

END RelationTreeParameter.
