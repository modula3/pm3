(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Method.i3                                             *)
(* Last Modified On Fri Jun 24 09:31:38 PDT 1994 By kalsow     *)
(*      Modified On Wed Mar 20 00:26:12 1991 By muller         *)


INTERFACE Method;

IMPORT M3ID, Type, Value, Expr;

TYPE
  Info = RECORD
    name      : M3ID.T;
    offset    : INTEGER;
    parent    : Type.T;
    signature : Type.T;
    dfault    : Expr.T;
    override  : BOOLEAN;
  END;

PROCEDURE New (READONLY info: Info): Value.T;

PROCEDURE Split (method: Value.T;  VAR info: Info): BOOLEAN;

PROCEDURE SplitX (method: Value.T;  VAR info: Info);

PROCEDURE NoteOverride (new, old: Value.T);

PROCEDURE IsEqual (va, vb: Value.T;  x: Type.Assumption): BOOLEAN;

END Method.
