(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Addr.m3                                               *)
(* Last Modified On Fri Jun 24 09:42:33 PDT 1994 By kalsow     *)
(*      Modified On Fri Feb  9 06:19:03 1990 By muller         *)

MODULE Addr;

IMPORT M3String, RefType, Tipe, TextExpr;

PROCEDURE Initialize () =
  BEGIN
    T := RefType.New (NIL, FALSE, TextExpr.New (M3String.Add ("$address$")));
    Tipe.Define ("ADDRESS", T, TRUE);
  END Initialize;

BEGIN
END Addr.
