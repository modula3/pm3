MODULE CardText;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:34  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:37:01  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT Word, Text;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN a.num = b.num AND Text.Equal(a.text, b.text);
  END Equal;

PROCEDURE Hash(a: T): Word.T =
  BEGIN
    RETURN Word.Xor(a.num, Text.Hash(a.text));
  END Hash;


BEGIN
END CardText.
