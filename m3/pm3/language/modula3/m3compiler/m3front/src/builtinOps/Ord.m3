(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ord.m3                                                *)
(* Last Modified On Tue May  3 16:32:32 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:19 1990 By muller         *)

MODULE Ord;

IMPORT CallExpr, Expr, ExprRep, Type, Procedure, Int, Error;
IMPORT IntegerExpr, EnumExpr, Target;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t: Type.T;
  BEGIN
    t := Expr.TypeOf (ce.args[0]);
    IF NOT Type.IsOrdinal (t) THEN
      Error.Msg ("ORD: argument must be an ordinal");
    END;
    ce.type := Int.T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e: Expr.T;  i: Target.Int;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) THEN
      RETURN NIL;
    ELSIF EnumExpr.Split (e, i, t) THEN
      RETURN IntegerExpr.New (i);
    ELSIF IntegerExpr.Split (e, i) THEN
      RETURN IntegerExpr.New (i);
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (ce.args[0], min, max);
  END GetBounds;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, Int.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("ORD", Z, TRUE);
  END Initialize;

BEGIN
END Ord.
