(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Nov  8 17:00:45 PST 1993 by mcjones                  *)
(*      modified on Fri Feb  5 15:45:03 PST 1993 by kalsow                   *)

(* <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>sorting</SPAN>
<SPAN CLASS=INDEX.KEY>arrays</SPAN>
</SPAN>
 *)

GENERIC INTERFACE ArraySort(Elem);
(* Where "Elem.T" is a type that is not an open array type and "Elem"
   contains

| PROCEDURE Compare(a, b: Elem.T): [-1 .. 1];

   "Compare" must define a total order.  Any parameter mode may be
   used. *)

PROCEDURE Sort(VAR a: ARRAY OF Elem.T; cmp := Elem.Compare);
(* Sort the elements of "a" using the order defined by "cmp". *)

END ArraySort.

(* "Sort(a, cmp)" permutes the elements of "a" such that:

| FIRST(a) <= i < j <= LAST(a)

   implies

| cmp(a[i], a[j]) <= 0.
   
   The algorithm used is QuickSort:
   <UL>
   <LI>It is not stable.
   <LI>On average, it requires "O(N ln N)" comparison and assignment
         operations.  In the worst case it may require "O(N*N)" operations.
   </UL>

   For an expanded description of QuickSort, see <A REL=BIB.ENTRY HREF="../../../html/references.html#Sedgewick:Alg"> [Sedgewick:Alg] </A>.
*)

