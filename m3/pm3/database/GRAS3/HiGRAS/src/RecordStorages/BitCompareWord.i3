INTERFACE BitCompareWord;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:27:47  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.2  1996/08/06 16:28:19  roland
    Merge of PAGESERVER and main branch.

# Revision 1.1  1993/10/02  15:57:58  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Word;


PROCEDURE F (i, j: Word.T): BOOLEAN;
  (* Returns TRUE if i is greater than j if bitwise compared from least to
     most significant bit. *)

END BitCompareWord.
