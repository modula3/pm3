(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Sun May 14 19:55:17 PDT 1995 by detlefs                  *)
(*      modified on Tue Feb 11 20:48:45 PST 1992 by muller                   *)

(* "Bag" is a generic interface defining bags of "Elem.T"'s. *)

GENERIC INTERFACE Bag(Super, Elem);
(* Where "Elem.T" is a type that is not an open array type and "Elem" contains

| CONST Brand = <text-constant>;
| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;

   "Brand" must be a text constant. It will be used to construct a brand for
   any generic types instantiated with the "Bag" interface. For a non-generic
   interface, we recommend choosing the name of the interface.

   "Equal" must be an equivalence relation.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".  *)

CONST Brand = "(Bag " & Elem.Brand & ")";

TYPE 
  Public = Super.T OBJECT METHODS
    fromArray(READONLY a: ARRAY OF Elem.T): T;
    copy(): T;
    member(e: Elem.T): BOOLEAN;
    count(e: Elem.T): INTEGER;
    insert(e: Elem.T);
    delete(e: Elem.T): BOOLEAN;
    size(): CARDINAL;
    isEmpty(): BOOLEAN;
    subset(s2: T): BOOLEAN;
    equal(s2: T): BOOLEAN;
    intersect(s2: T): BOOLEAN;
    union(s2: T): T;
    add(s2: T): T;
    intersection(s2: T): T;
    diff(s2: T): T;
    unionD(s2: T): T;
    addD(s2: T): T;
    intersectionD(s2: T): T;
    diffD(s2: T): T;
    iterate(): Iterator;
  END;
  T <: Public;
  Iterator = OBJECT METHODS
    next(VAR e: Elem.T): BOOLEAN
  END;

(* A "Bag(Elem)" is a multiset of "Elem.T"'s.  "Elem.T"'s that are equivalent
   under "Elem.Equal" are treated as equivalent by "Bag"; for example, if you
   are creating a bag with an "Elem.T" of "TEXT", you are likely to want
   "Text.Equal" as the equivalence relation.  The equivalence relation must be
   time-invariant.  For example, it can't depend on the values of particular
   references since some garbage collectors may move "REF" values.

   Formally, a bag "s" has the components

| set(s) `a set of equivalence classes of "Elem.T"'s`
| freq(s, e) `a frequency distribution for each equivalence class.`

   We will use "equiv(e)" to denote the equivalence class containing an
   "Elem.T" "e".

   At all times "freq(s, equiv(e)) = 0" iff "equiv(e)" is not in "set(s)".

   For efficiency, a bag is not monitored: it is up to the clients to avoid
   illegal concurrent accesses on the methods of a bag.  A bag's "insert" and
   "delete" methods have side-effects on the bag, so can't be performed
   concurrently with any other method of that bag or of an iterator on that
   bag.  An iterator's "next" method has side-effects on the iterator, and is
   also considered to be a side-effect free operation on the parent bag.

   The methods of an object "s" of type "Bag.T" have the following
   specifications:

   The call "s.fromArray(a)" causes "set(s)" to contain exactly the
   equivalence classes containing all the elements of the array "a";
   "freq(s, e)" records the frequency counts for each equivalence class.

   The call "s.copy()" returns a bag "s2" whose abstract state "set(s2)" is
   the same as "set(s)", and "freq(s, e) = freq(s2, e)" for all "e" in
   "set(s)".

   The call "s.member(e)" returns "TRUE" iff "e" is an equivalence class in
   "set(s)".

   The call "s.count(e)" returns "freq(s, equiv(e))".

   The call "s.insert(e)" returns "TRUE" and does not modify "s" if "equiv(e)"
   is in "set(s)"; otherwise it adds "equiv(e)" to "set(s)" and returns
   "FALSE".  In either case, it increments "freq(s, equiv(e))".

   The call "s.delete(e)" decrements "freq(s, equiv(e))", ensuring that
   "set(s)" does not contain "equiv(e)" iff "freq(s, equiv(e))" is now zero,
   and returning "TRUE" iff "set(s)" contained "equiv(e)" before the call.

   The call "s.isEmpty()" returns "TRUE" iff "set(s)" is the empty set.

   The call "s.size()" returns the sum of "freq(s, e)" for all "e" in
   "set(s)".

   The call "s.subset(s2)" returns "TRUE" iff "freq(s, e) <= freq(s2, e)" for
   all "e" in "set(s)".

   The call "s.equal(s2)" returns "TRUE" iff "set(s)" and "set(s2)" are the
   same set, and "freq(s2, e) = freq(s, e)" for all "e".

   The call "s.union(s2)" returns a new bag "s3" such that "set(s3)" is the
   union of "set(s)" and "set(s2)", and "freq(s3, e)" is the maximum of
   "freq(s, e)" and "freq(s2, e)" for all "e" in "set(s3)".

   The call "s.add(s2)" returns a new bag "s3" such that "set(s3)" is the
   union of "set(s)" and "set(s2)", and "freq(s3, e)" is the sum of "freq(s,
   e)" and "freq(s2, e)" for all "e" in "set(s3)".

   The call "s.intersection(s2)" returns a new set "s3" such that "set(s3)" is
   the intersection of "set(s)" and "set(s2)", and "freq(s3, e)" is the
   minimum of "freq(s, e)" and "freq(s2, e)" for all "e" in "set(s3)".

   The call "s.diff(s2)" returns a set "s3" such that "freq(s3, e)" is
   "max(freq(s, e) - freq(s2, e), 0)" for all "e" in "set(s)", ensuring that
   "set(s3)" does not contain "e" iff "freq(s3, e)" is zero.

   The call "s.unionD(s2)" modifies "s" so that "set(s)" contains the union of
   "set(s`)" and "set(s2)", and "freq(s, e)" is the maximum of "freq(s`, e)"
   and "freq(s2, e)" for all "e" in "set(s)", where "s`" is the state of "s"
   immediately before the call; it returns the modified bag.

   The call "s.addD(s2)" modifies "s" so that "set(s)" contains the union of
   "set(s`)" and "set(s2)", and "freq(s, e)" is the sum of "freq(s`, e)" and
   "freq(s2, e)" for all "e" in "set(s)", where "s`" is the state of "s"
   immediately before the call; returns the modified bag.

   The call "s.intersectionD(s2)" modifies "s" so that "set(s)" contains the
   intersection of "set(s`)" and "set(s2)", and "freq(s, e)" is the minimum of
   "freq(s`, e)" and "freq(s2, e)" for all "e" in "set(s)", where "s`" is the
   state of "s" immediately before the call; it returns the modified bag.

   The call "s.diffD(s2)" modifies "s" so that "freq(s, e)" is "max(freq(s`,
   e) - freq(s2, e), 0)" for all "e" in "set(s`)", ensuring that "set(s)" does
   not contain "e" iff "freq(s, e)" is zero; it returns the modified bag.

   The call "s.iterate()" returns an iterator, which is an object that can be
   used to iterate over the elements in "s".  See below for the specification
   of the "Iterator" type.

   If "it" is the result of the call "s.iterate()", then the call "it.next(e)"
   selects an occurrence of an element from "s" that has not already been
   returned by "it", sets "e" to that element, and returns "TRUE".  If no
   entries remain, the call returns "FALSE" without setting "e".  It is a
   checked runtime error to call "next" after it has returned "FALSE".  *)

PROCEDURE Equal(s1, s2: T): BOOLEAN;
(* Equivalent to "s1.equal(s2)".  Exported so that "Bag"'s may be used as
   arguments to generic interfaces. *)

END Bag.
