(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Aug  5 11:21:09 PDT 1993 by hanson     *)
(*      modified on Thu Dec 17 16:41:46 PST 1992 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3CG_Burs;

IMPORT M3CG, Wr;

PROCEDURE New (wr: Wr.T; debugging := TRUE): M3CG.T;
(* returns a fresh, initialized code generator that writes
   its assembler output on `wr' and includes debugging information
   if `debugging' is TRUE. *)

END M3CG_Burs.
