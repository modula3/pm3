(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Nov 19 09:31:20 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3CG_Clean;

IMPORT M3CG;

PROCEDURE New (child: M3CG.T;  jumps, stores: BOOLEAN): M3CG.T;
(* returns a fresh, initialized code generator that passes its calls
   to 'child' and inserts enough extra loads and stores to ensure that
   the stack is empty at every jump and/or store. *)

END M3CG_Clean.
