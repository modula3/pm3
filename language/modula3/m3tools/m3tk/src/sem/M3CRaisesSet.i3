INTERFACE M3CRaisesSet;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3AST_AS;


TYPE
  Comparison = {ProperSubSet, Equal, ProperSuperSet, Disjoint};
  ComparisonSet = SET OF Comparison;

CONST
  SubSet = ComparisonSet{Comparison.ProperSubSet, Comparison.Equal};
  SuperSet = ComparisonSet{Comparison.Equal, Comparison.ProperSuperSet};

PROCEDURE Compare(r1, r2: M3AST_AS.RAISEES_NULL): Comparison RAISES {};

END M3CRaisesSet.
