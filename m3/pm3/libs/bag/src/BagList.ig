(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Mon Jul 25 11:53:25 PDT 1994 by detlefs                  *)
(*      modified on Tue Feb 11 20:48:45 PST 1992 by muller                   *)

(* "BagList" is a generic interface defining sets of "Elem.T"'s,
   implemented as linked lists.  This implementations is appropriate
   only for small sets.
*)

GENERIC INTERFACE BagList(ElemBag);
(* WHERE "Elem.T" is a REF type and contains 

| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;

   "ElemBag = Bag(Elem)".

   "Equal" must be an equivalence relation.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".
*)

TYPE 
  Public = ElemBag.T OBJECT METHODS
    init(): T;
  END;
  T <: Public;
  Iterator <: ElemBag.Iterator;

(* The expression

| NEW(BagList.T).init()

   creates a new, empty bag.

*)

END BagList.
