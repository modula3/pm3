GENERIC MODULE RelationElement(Elem1, Elem2);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1997/03/21 16:42:59  roland
    New generic module Relation realizes binary relations between objects
    of two (distinct) abstract data types.

*)
(***************************************************************************)


PROCEDURE Equal (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN Elem1.Equal(a.m, b.m) AND Elem2.Equal(a.n, b.n);
  END Equal;


PROCEDURE Compare (READONLY a, b: T): [-1 .. 1] =  
  BEGIN
    WITH res = Elem1.Compare(a.m, b.m) DO
      IF res # 0 THEN
        RETURN res;
      ELSE
        RETURN Elem2.Compare(a.n, b.n);
      END;
    END;
  END Compare;

BEGIN
END RelationElement.
