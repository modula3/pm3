(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* A "RefanyTransient.T" is a "<*TRANSIENT*> REFANY".  This interface is
   intended to be used to instantiate generic interfaces and modules such as
   "Table" and "List". *)

INTERFACE RefanyTransient;

IMPORT Word;

TYPE T = <*TRANSIENT*> REFANY;

CONST Brand = "RefanyTransient";

PROCEDURE Equal(r1, r2: T): BOOLEAN;
(* Return "r1 = r2". *)

PROCEDURE Hash(r: T): Word.T;
(* Cause a checked runtime error. *)

PROCEDURE Compare(r1, r2: T): [-1..1];
(* Cause a checked runtime error. *)


END RefanyTransient.
