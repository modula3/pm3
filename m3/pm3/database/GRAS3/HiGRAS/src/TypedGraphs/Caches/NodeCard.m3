MODULE NodeCard;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:44  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:36:58  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT Word;

PROCEDURE Equal(READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

PROCEDURE Hash(READONLY a: T): Word.T =
  BEGIN
    RETURN Word.Xor(a.node.entity, a.num);
  END Hash;

BEGIN
END NodeCard.
