INTERFACE AttributeTreeParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:28:22  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.1  1996/04/29 13:57:40  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.1  1993/10/04  21:43:58  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT AttributeRecordParameter;


CONST
  Key2HashMask = 7;              (* use lower three bits for hashing
                                    (approx.  7 attribute records for one
                                    entity *)


PROCEDURE ComputePageKeys (READONLY key1: AttributeRecordParameter.Key1;
                           READONLY key2: AttributeRecordParameter.Key2;
                           VAR pageKey1, pageKey2: CARDINAL);


PROCEDURE ComputeRecordKeys (VAR key1: AttributeRecordParameter.Key1;
                             VAR key2: AttributeRecordParameter.Key2;
                             pageKey1, pageKey2: CARDINAL);

END AttributeTreeParameter.
