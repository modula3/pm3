GENERIC INTERFACE CursorSet(Element);

(***************************************************************************)
(* This module implements a set of elements of type Element.T.  It requires
   Element to export comparision functions Compare and Equal as well as a
   text constant Brand used to construct a brand for the set type. *)
(***************************************************************************)
(** Created by:  Peter Klein                                               *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.4  1997/07/21 10:29:28  roland
    New implementation of sets using red-black trees.

*)
(***************************************************************************)

CONST Brand = "(" & Element.Brand & " CursorSet )";

TYPE
  CompareResult = {Less, Equal, Greater};
    (* Result type for element comparision procedures. *)

  CompareProcedure = PROCEDURE (READONLY a, b: Element.T): CompareResult;
    (* Type for element comparision procedures. *)

  T <: Public;

  Public =
    OBJECT
    METHODS
      (**********************************************************************)
      (**                                                                   *)
      (**                     Organization                                  *)
      (**                                                                   *)
      (**********************************************************************)

      init (): T;
            (* Initialize a set created by a NEW(T) command.  After
               initialization, the set will be empty. *)



      copy (): T RAISES {};
            (* Returns a copy of the set. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                     Queries                                       *)
      (**                                                                   *)
      (**********************************************************************)

      card (): CARDINAL RAISES {};
            (* Determine the number of elements in the set. *)


      isEmpty (): BOOLEAN RAISES {};
               (* Yields TRUE if the set has no elements. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                     Delete                                        *)
      (**                                                                   *)
      (**********************************************************************)

      deleteElement (READONLY data: Element.T; VAR found: BOOLEAN)
                     RAISES {};
                     (* Remove the element which is equal to data from the
                        set (where equality is defined through Element.Eq).
                        found returns FALSE if no such element exists. *)


      extractAnyElement (VAR found: BOOLEAN): Element.T RAISES {};
                         (* Get an arbitrary element from the set and
                            delete it.  If the set is empty, found returns
                            FALSE and and arbitrary element of type
                            Element.T is returned. *)


      rangeExtractAnyElement (         compareProcedure: CompareProcedure;
                              READONLY data            : Element.T;
                              VAR      found           : BOOLEAN           ):
                              Element.T RAISES {IllegalCompare};
                              (* Get an arbitrary element from the set
                                 which is equal to data with respect to
                                 compareProcedure.  For a description of
                                 comparision procedures see rangeLoop.  The
                                 element will be removed from the set.  If
                                 no appropriate element could be found,
                                 found will return FALSE and and arbitrary
                                 element of type Element.T is returned. *)

      dispose (insertInFreeList: BOOLEAN := TRUE) RAISES {};
               (* Removes all elements from the set.  Consider calling this
                  method when a set is no longer needed, because internal
                  nodes are kept in a free-list. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                     Insert and Replace                            *)
      (**                                                                   *)
      (**********************************************************************)

      insert (READONLY data: Element.T) RAISES {};
              (* Insert an element into the set.  If there already exists
                 an element which is equal to data by means of Element.Eq,
                 nothing happens. *)


      replaceValue (READONLY data: Element.T; VAR found: BOOLEAN)
                    RAISES {};
                    (* Replace the element which is equal to data by means
                       of Element.Eq with data.  found returns FALSE if no
                       such elements exists. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                     Element Queries                               *)
      (**                                                                   *)
      (**********************************************************************)

      getValue (READONLY data: Element.T; VAR Found: BOOLEAN): Element.T
                RAISES {};
                (* Get the element which is equal to data by means of
                   Element.Eq.  If no appropriate element could be found,
                   found will return FALSE and and arbitrary element of
                   type Element.T is returned. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                     Algebraic Operations                          *)
      (**                                                                   *)
      (**********************************************************************)

      in (READONLY data: Element.T): BOOLEAN RAISES {};
          (* Yields TRUE if an element equal to data (where equality is
             defined through Element.Eq) is in the set. *)


      compare (    secondSet: T;
               VAR equal    : BOOLEAN;
               VAR less     : BOOLEAN;
               VAR greater  : BOOLEAN  ) RAISES {};
               (* Compare with secondSet.  equal, less, and greater are
                  defined in terms of the subset relation. *)


      isSubset (secondSet: T): BOOLEAN RAISES {};
                (* Yields TRUE if the set is a subset of secondSet. *)


      isStrictSubset (secondSet: T): BOOLEAN RAISES {};
                      (* Yields TRUE if the set is a strict subset of
                         secondSet. *)


      isDisjunct (secondSet: T): BOOLEAN RAISES {};
                  (* Yields TRUE if the set and secondSet are disjunct. *)


      union (secondSet: T) RAISES {};
             (* Add all elements of secondSet to the set.  secondSet
                remains unchanged. *)


      intersection (secondSet: T) RAISES {};
                    (* Remove all elements from the set which are not
                       elements of secondSet.  secondSet remains
                       unchanged. *)


      difference (secondSet: T) RAISES {};
                  (* Remove all elements from the set which are elements of
                     secondSet.  secondSet remains unchanged. *)


      symmetricDifference (secondset: T) RAISES {};
                           (* Remove all elements from the set which are
                              elements of secondSet.  Add all elements from
                              secondSet which are not elements of the set.
                              secondSet remains unchanged. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                     Miscellaneous                                 *)
      (**                                                                   *)
      (**********************************************************************)

      gotoElement (READONLY data: Element.T; VAR found: BOOLEAN) RAISES {};
                   (* Set the internal cursor to the element which is equal
                      to data by means of Element.Eq.  The next call of get
                      or extractAnyElement will return this element.  If no
                      such element exists, found will return FALSE and the
                      cursor remains unchanged. *)


      gotoMinimum () RAISES {};
                   (* Set the internal cursor to the smallest element by
                      means of Element.Lt.  The next call of get or
                      extractAnyElement will return this element. *)


      gotoMaximum () RAISES {};
                   (* Set the internal cursor to the greatest element by
                      means of Element.Lt.  The next call of get or
                      extractAnyElement will return this element. *)


      loop () RAISES {};
            (* Prepare the internal cursor for a loop over the set.  This
               is actually the same as gotoMinimum. *)


      rangeLoop (         compareProcedure: CompareProcedure;
                 READONLY data            : Element.T         )
                 RAISES {IllegalCompare};
                 (* Prepare the internal cursor for a loop over a subrange
                    of the set.  The loop will only affect elements which
                    are equal to data be means of compareProcedure.
                    compareProcedure must respect the order of Element.Lt
                    and Element.Eq and fulfill the requirements of an order
                    relation.  extractAnyElement may not be used in a
                    rangeLoop/get loop. *)


      get (VAR found: BOOLEAN): Element.T RAISES {};
           (* Get the current element, i.e.  the one at the cursor
              position, from the set.  The cursor will be advanced to the
              next element in the order defined by Element.Lt.  If no
              appropriate element could be found, found will return FALSE
              and and arbitrary element of type Element.T is returned. *)


    END;

PROCEDURE New(): T;
  (* Is semantically equivalent to NEW(CursorSet.T).init(), but uses an internal
     free memory list *)
  
EXCEPTION IllegalCompare;

END CursorSet.
