(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ObjectTransient.m3                                    *)

MODULE ObjectTransient;

IMPORT M3ID, ObjectType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR s: Scope.T;
  BEGIN
    s := Scope.PushNew (FALSE, M3ID.NoID);
    Scope.PopNew ();
    T := NIL;   (* the value is used by ObjectType.New ! *)
    T := ObjectType.New (NIL, TRUE, TRUE, NIL, s, s);
    Tipe.Define ("_TRANSIENT_ROOT", T, FALSE);
  END Initialize;

BEGIN
END ObjectTransient.
