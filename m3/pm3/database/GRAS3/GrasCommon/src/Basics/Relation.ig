GENERIC INTERFACE Relation(Elem1, Elem2, Elem1Set, Elem2Set, TupleSet);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.4  1997/09/18 14:33:58  roland
    Cartesian product does not return a relation but inserts the cartesian
    product of to sets in a relation.

    Revision 1.3  1997/09/18 08:19:29  roland
    Comments corrected. CardRelation now instantiated in GrasCommon.

    Revision 1.2  1997/07/21 10:29:33  roland
    New implementation of sets using red-black trees.

    Revision 1.1  1997/03/21 16:42:48  roland
    New generic module Relation realizes binary relations between objects
    of two (distinct) abstract data types.

*)
(***************************************************************************)

CONST Brand = "( Relation_" & Elem1.Brand & " " & Elem2.Brand & " )";

TYPE
  T <: Public;

  Public =
    TupleSet.T OBJECT
    METHODS
      fromElem1Set (source: Elem1Set.T; comp2: Elem2.T := Elem2.Null): T
                    RAISES {};
                    (* Returns {(n, comp2) | n \in source} *)

      fromElem2Set (source: Elem2Set.T; comp1: Elem1.T := Elem1.Null): T
                    RAISES {};
                    (* Returns {(comp1, n) | n \in source} *)

      cartesian (comp1: Elem1Set.T; comp2: Elem2Set.T) RAISES {};
                 (* Inserts {(m, n) | m \in comp1 \wedge n \in comp2}. *)

      projection1 (): Elem1Set.T RAISES {};
                   (* Returns a projection of the relation on the first
                      component. *)

      projection2 (): Elem2Set.T RAISES {};
                   (* Returns a projection of the relation on the second
                      component. *)

      queryProjection1 (comp2: Elem2.T): Elem1Set.T RAISES {};
                        (* Returns a projection of the relation on the
                           second component.  Only elements where the first
                           component is equal to comp1 are copied. *)

      queryProjection2 (comp1: Elem1.T): Elem2Set.T RAISES {};
                        (* Returns a projection of the relation on the
                           first component.  Only elements where the second
                           component is equal to comp2 are copied. *)

      singleQueryProjection1 (comp2: Elem2.T; VAR found: BOOLEAN): Elem1.T
                              RAISES {};
                              (* Returns the first component for some
                                 element with comp2 as second component.
                                 If no such element exists, found will
                                 return FALSE. *)

      singleQueryProjection2 (comp1: Elem1.T; VAR found: BOOLEAN): Elem2.T
                              RAISES {};
                              (* Returns the second component for some
                                 element with comp1 as first component.  If
                                 no such element exists, found will return
                                 FALSE. *)

      (* Some element-oriented methods are redefined here to give easy
         access to the components of the tuple. *)

      insert (comp1: Elem1.T; comp2: Elem2.T) RAISES {};


      get (VAR comp1: Elem1.T; VAR comp2: Elem2.T; VAR found: BOOLEAN)
           RAISES {};


      extractAnyElement (VAR comp1: Elem1.T;
                         VAR comp2: Elem2.T;
                         VAR found: BOOLEAN  ) RAISES {};


      deleteElement (comp1: Elem1.T; comp2: Elem2.T) RAISES {};


      gotoElement (comp1: Elem1.T; comp2: Elem2.T; VAR found: BOOLEAN)
                   RAISES {};


      in (comp1: Elem1.T; comp2: Elem2.T): BOOLEAN RAISES {};
    END;

PROCEDURE New(): T;
  (* Is semantically equivalent to NEW(Relation.T).init(), but uses an internal
     free memory list *)
  
  
END Relation.
