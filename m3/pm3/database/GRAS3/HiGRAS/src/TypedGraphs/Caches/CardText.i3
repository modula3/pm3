INTERFACE CardText;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:33  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:36:59  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT Word;

CONST
  Brand = "CardTextKey";

TYPE T = RECORD num: CARDINAL; text: TEXT; END;    

PROCEDURE Equal(a, b: T): BOOLEAN;

PROCEDURE Hash(a: T): Word.T;

END CardText.
