(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Val.m3                                                *)
(* Last Modified On Tue May  3 16:33:31 PDT 1994 By kalsow     *)
(*      Modified On Fri Dec 21 01:18:57 1990 By muller         *)

MODULE Val;

IMPORT CallExpr, Expr, ExprRep, Type, Procedure, Error, TypeExpr, Int;
IMPORT IntegerExpr, EnumExpr, EnumType, CheckExpr, Target, TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t)
      THEN RETURN t;
      ELSE RETURN Int.T;
    END;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, u: Type.T;  mint, maxt, minu, maxu: Target.Int;
  BEGIN
    u := Expr.TypeOf (ce.args[0]);
    t := Int.T;
    IF  NOT Type.IsSubtype (u, Int.T) THEN
      Error.Msg ("VAL: first argument must be an INTEGER");
    ELSIF  NOT TypeExpr.Split (ce.args[1], t) THEN
      Error.Msg ("VAL: second argument must be a type");
    ELSIF  NOT Type.IsOrdinal (t) THEN
      Error.Msg ("VAL: second argument must be an ordinal type");
    ELSE (* looks ok *)
      EVAL Type.GetBounds (t, mint, maxt);
      EVAL Type.GetBounds (u, minu, maxu);
      IF TInt.LT (minu, mint) THEN
        (* we need a lower bound check *)
        IF TInt.LT (maxt, maxu) THEN
          (* we also need an upper bound check *)
          ce.args[0] := CheckExpr.New (ce.args[0], mint, maxt);
          Expr.TypeCheck (ce.args[0], cs);
        ELSE
          ce.args[0] := CheckExpr.NewLower (ce.args[0], mint);
          Expr.TypeCheck (ce.args[0], cs);
        END;
      ELSIF TInt.LT (maxt, maxu) THEN
        (* we need an upper bound check *)
        ce.args[0] := CheckExpr.NewUpper (ce.args[0], maxt);
        Expr.TypeCheck (ce.args[0], cs);
      END;
    END;
    ce.type := t;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t) THEN Type.Compile (t) END;
    Expr.Compile (ce.args[0]);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR t: Type.T;  e: Expr.T;  x, min, max: Target.Int;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) OR (NOT IntegerExpr.Split (e, x))
      OR (NOT TypeExpr.Split (ce.args[1], t)) THEN
      RETURN NIL;
    END;
    EVAL Type.GetBounds (t, min, max);
    IF TInt.LT (x, min) OR TInt.LT (max, x) THEN
      Error.Msg ("VAL: value out of range");
      RETURN NIL;
    END;
    t := Type.Base (t);
    IF EnumType.Is (t)
      THEN RETURN EnumExpr.New (t, x);
      ELSE RETURN IntegerExpr.New (x);
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 Fold,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("VAL", Z, TRUE);
  END Initialize;

BEGIN
END Val.
