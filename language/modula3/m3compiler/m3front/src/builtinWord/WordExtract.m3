(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordExtract.m3                                        *)
(* Last Modified On Mon Dec  5 15:30:52 PST 1994 By kalsow     *)
(*      Modified On Thu Mar  7 20:19:39 1991 By muller         *)

MODULE WordExtract;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure;
IMPORT Int, IntegerExpr, ProcType, Host, Card;
IMPORT Target, TInt, TWord, Value, Formal, CheckExpr, Error;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Int.T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    t1, t2: CG.Val;
    max: Target.Int;
    b, x1, x2: BOOLEAN;
    i1, i2: INTEGER;
  BEGIN
    x1 := GetBitIndex (ce.args[1], i1);
    x2 := GetBitIndex (ce.args[2], i2);

    IF x1 AND x2 THEN
      (* we can use the extract_mn operator *)
      IF (i1 + i2 > Target.Integer.size) THEN
        Error.Warn (2, "value out of range");
        CG.Load_integer (TInt.One);
        CG.Check_hi (TInt.Zero);
      ELSE
        Expr.Compile (ce.args[0]);
        CG.Extract_mn (FALSE, i1, i2);
      END;

    ELSIF x2 THEN
      (* we can use the extract_n operator *)
      b := TInt.FromInt (Target.Integer.size - i2, max); <*ASSERT b*>
      Expr.Compile (ce.args[0]);
      CheckExpr.Emit (ce.args[1], TInt.Zero, max);
      CG.Extract_n (FALSE, i2);

    ELSIF x1 THEN
      (* we need the general purpose extract operator, but can simplify
         the range checking code *)
      b := TInt.FromInt (Target.Integer.size - i1, max); <*ASSERT b*>
      Expr.Compile (ce.args[0]);
      CG.Force ();
      CG.Load_intt (i1);
      CheckExpr.Emit (ce.args[2], TInt.Zero, max);
      CG.Extract (sign := FALSE);

    ELSE
      (* we need the general purpose extract operator *)
      CheckExpr.Emit (ce.args[1], TInt.Zero, Target.Integer.max);
      t1 := CG.Pop ();
      CheckExpr.Emit (ce.args[2], TInt.Zero, Target.Integer.max);
      t2 := CG.Pop ();
      IF Host.doRangeChk THEN
        b := TInt.FromInt (Target.Integer.size, max); <*ASSERT b*>
        CG.Push (t1);
        CG.Push (t2);
        CG.Add (CG.Type.Int);
        CG.Check_hi (max);
        CG.Discard (CG.Type.Int);
      END;
      Expr.Compile (ce.args[0]);
      CG.Force ();
      CG.Push (t1);
      CG.Push (t2);
      CG.Extract (sign := FALSE);
      CG.Free (t1);
      CG.Free (t2);
    END;
  END Compile;

PROCEDURE GetBitIndex (e: Expr.T;  VAR i: INTEGER): BOOLEAN =
  VAR x: Target.Int;
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    RETURN IntegerExpr.Split (e, x)
       AND TInt.ToInt (x, i)
       AND (0 <= i) AND (i <= Target.Integer.size);
  END GetBitIndex;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e0, e1, e2: Expr.T;  w0, i1, i2, result: Target.Int;
  BEGIN
    e0 := Expr.ConstValue (ce.args[0]);
    e1 := Expr.ConstValue (ce.args[1]);
    e2 := Expr.ConstValue (ce.args[2]);
    IF (e0 = NIL) OR (NOT IntegerExpr.Split (e0, w0)) OR 
       (e1 = NIL) OR (NOT IntegerExpr.Split (e1, i1)) OR 
       (e2 = NIL) OR (NOT IntegerExpr.Split (e2, i2)) OR
       NOT TWord.Extract (w0, i1, i2, result) THEN
      RETURN NIL;
    END;
    RETURN IntegerExpr.New (result);
  END Fold;

PROCEDURE Initialize () =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, Int.T);
    f1 := Formal.NewBuiltin ("i", 1, Card.T);
    f2 := Formal.NewBuiltin ("n", 2, Card.T);
    t  := ProcType.New (Int.T, f0, f1, f2);
  BEGIN
    Z := CallExpr.NewMethodList (3, 3, TRUE, TRUE, TRUE, Int.T,
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
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("Extract", Z, FALSE, t);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END WordExtract.
