(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jan 26 07:54:12 PST 1994 by kalsow     *)

INTERFACE RTObject;

PROCEDURE PatchMethods (r: ROOT);
(* bind any undefined methods to a procedure that causes
   a clean checked runtime error. *)

END RTObject.
