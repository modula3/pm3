(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* A "RootTransient.T" is a "<*TRANSIENT*> ROOT".  This interface is intended
   to be used to instantiate generic interfaces and modules such as "Table" and
   "List". *)

INTERFACE RootTransient;

IMPORT Word;

TYPE T = <*TRANSIENT*> ROOT;

CONST Brand = "RootTransient";

PROCEDURE Equal(r1, r2: T): BOOLEAN;
(* Return "r1 = r2". *)

PROCEDURE Hash(r: T): Word.T;
(* Cause a checked runtime error. *)

PROCEDURE Compare(r1, r2: T): [-1..1];
(* Cause a checked runtime error. *)

END RootTransient. 
