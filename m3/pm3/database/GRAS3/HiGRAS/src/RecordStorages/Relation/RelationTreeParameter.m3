MODULE RelationTreeParameter;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:34  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:15:12  roland
    Modified code to remove compiler warnings.

    Revision 1.1  1997/03/26 11:29:32  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.4  1997/02/20 16:23:23  roland
    Performance improvement: key1 and key2 were always read and
    written together. Accordingly there is now only one operation for both
    resulting in fewer  page accesses.
    RecordStorage: new hash function for on-page hashing.

    Revision 1.3  1997/01/31 10:29:06  roland
    Maybe a little performance improvement.

    Revision 1.2  1996/08/06 16:30:20  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/04/29 13:59:52  roland
    Adapted for page-server and restructured. Exceptionhandling improved.

# Revision 1.2  1993/10/04  21:47:35  pk
# Key and Data types + comments corrected.
#
# Revision 1.1  1993/10/02  15:59:30  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT RelationRecordParameter, Word;


PROCEDURE ComputePageKeys (READONLY key1: RelationRecordParameter.Key1;
                           READONLY key2: RelationRecordParameter.Key2;
                           VAR pageKey1, pageKey2: CARDINAL) =
  BEGIN
    pageKey1 := Word.Plus(Word.LeftShift(key1.order, 13), key1.entity1);
    pageKey1 := Word.Plus(key1.overflowCounter, pageKey1);
    pageKey2 := key2;
  END ComputePageKeys;


PROCEDURE ComputeRecordKeys (VAR key1: RelationRecordParameter.Key1;
                             VAR key2: RelationRecordParameter.Key2;
                             <* UNUSED *> pageKey1: CARDINAL;
                                          pageKey2: CARDINAL  ) =
  BEGIN
    key1 :=
      RelationRecordParameter.Key1{0, 0, 0}; (* no conversion possible *)
    key2 := pageKey2;
  END ComputeRecordKeys;

BEGIN
END RelationTreeParameter.
