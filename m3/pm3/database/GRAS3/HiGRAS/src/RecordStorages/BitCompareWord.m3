MODULE BitCompareWord;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:27:48  roland
    Subsystem RecordStorages adapted to handle index files with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges. The specialized RecordStorages do not
    define their index tree number statically, but receive it from the
    application at run time.

    Revision 1.2  1996/08/06 16:28:20  roland
    Merge of PAGESERVER and main branch.

# Revision 1.1  1993/10/02  15:58:00  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Word;


PROCEDURE F (i, j: Word.T): BOOLEAN =
  VAR bit := 0;

  BEGIN
    WHILE ((bit < Word.Size)
             AND (Word.Extract(i, bit, 1) = Word.Extract(j, bit, 1))) DO
      INC(bit);
    END;
    IF (bit = Word.Size) THEN
      RETURN FALSE;
    ELSE
      RETURN (Word.Extract(i, bit, 1) = 1);
    END;
  END F;

BEGIN
END BitCompareWord.
