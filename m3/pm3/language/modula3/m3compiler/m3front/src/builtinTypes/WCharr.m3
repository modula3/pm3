(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE WCharr;

IMPORT M3, M3ID, EnumType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR elts: Scope.T;  cs := M3.OuterCheckState;
  BEGIN
    elts := Scope.PushNew (FALSE, M3ID.Add ("WIDECHAR"));
    T := EnumType.New (16_10000, elts);
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("WIDECHAR", T, TRUE);
  END Initialize;

BEGIN
END WCharr.
