(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Sat Sep  4 21:50:20 PDT 1993 by detlefs                  *)
(*      modified on Tue Feb 11 20:48:45 PST 1992 by muller                   *)

(* "BagDef" is a generic interface defining bags of "Elem.T"'s, implemented
   via hash tables.  *)

GENERIC INTERFACE BagDef(ElemBag);
(* WHERE "ElemBag = Bag(Elem)", and "Elem.T" is a type that is not an open
   array type and "Elem" contains

| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;
| PROCEDURE Hash(k: Key.T): Word.T;

   "Equal" must be an equivalence relation and "Hash" must respect that
   equivalence relation, i.e., if "Equal(k1, k2)", then "Hash(k1)=Hash(k2)".

   "Hash" and "Equal" may be declared with a parameter mode of either "VALUE"
   or "READONLY", but not "VAR".  *)

TYPE
  Public = ElemBag.T OBJECT METHODS
    init(sizeHint: CARDINAL := 0): T
  END;
  T <: Public;
  Iterator <: ElemBag.Iterator;

(* The expression

| NEW(BagDef.T).init(sz)

   creates a new, empty bag.  The argument "sz" is a hint; the bag may be more
   efficient if "sz" accurately predicts the maximum number of elements it
   will ever contain.

*)

END BagDef.
