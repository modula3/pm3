(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ReffTransient.m3                                      *)

MODULE ReffTransient;

IMPORT RefType, Tipe, Brand;

PROCEDURE Initialize () =
  BEGIN
    T := RefType.New (NIL, TRUE, TRUE, Brand.New ("$refanytransient$"));
    Tipe.Define ("_TRANSIENT_REFANY", T, FALSE);
  END Initialize;

BEGIN
END ReffTransient.
