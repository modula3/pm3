GENERIC INTERFACE RelationElement(Elem1, Elem2);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:29:37  roland
    New implementation of sets using red-black trees.

    Revision 1.1  1997/03/21 16:42:55  roland
    New generic module Relation realizes binary relations between objects
    of two (distinct) abstract data types.

*)
(***************************************************************************)

CONST Brand = "( " & Elem1.Brand &" " & Elem2.Brand & " RelationElement )";

TYPE T = RECORD m: Elem1.T; n: Elem2.T;  END;


PROCEDURE Equal (READONLY a, b: T): BOOLEAN;
  (* Return "a = b". *)


PROCEDURE Compare (READONLY a, b: T): [-1 .. 1];
  (* Return "-1" if "a < b", "0" if "a = b", or "+1" if "a > b". *)

END RelationElement.
