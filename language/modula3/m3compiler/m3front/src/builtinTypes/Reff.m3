(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Reff.m3                                               *)
(* Last Modified On Fri Jun 24 09:44:39 PDT 1994 By kalsow     *)
(*      Modified On Thu Nov  2 21:29:23 1989 By muller         *)

MODULE Reff;

IMPORT RefType, Tipe, M3String, TextExpr;

PROCEDURE Initialize () =
  BEGIN
    T := RefType.New (NIL, TRUE, TextExpr.New (M3String.Add ("$refany$")));
    Tipe.Define ("REFANY", T, TRUE);
  END Initialize;

BEGIN
END Reff.
