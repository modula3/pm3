(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordLE.m3                                             *)
(* Last Modified On Mon Dec  5 15:30:50 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:13:09 1990 By muller         *)

MODULE WordLE;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TWord;
IMPORT Int, Bool, WordPlus, Value, Formal, ProcType;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Bool.T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Le (CG.Type.Word);
  END Compile;

PROCEDURE PrepBR (ce: CallExpr.T;  true, false: CG.Label;  freq: CG.Frequency)=
  BEGIN
    Expr.Prep (ce.args[0]);
    Expr.Prep (ce.args[1]);
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    IF (true # CG.No_label)
      THEN CG.If_le (true, CG.Type.Word, freq);
      ELSE CG.If_gt (false, CG.Type.Word, freq);
    END;
  END PrepBR;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1: Target.Int;
  BEGIN
    IF WordPlus.GetArgs (ce.args, w0, w1)
      THEN RETURN Bool.Map [TWord.LE (w0, w1)];
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  VAR
    x1 := Formal.NewBuiltin ("x", 0, Int.T);
    y1 := Formal.NewBuiltin ("y", 1, Int.T);
    t1 := ProcType.New (Bool.T, x1, y1);
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Bool.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 PrepBR,
                                 CallExpr.NoBranch,
                                 Fold,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("LE", Z, FALSE, t1);
    formals := ProcType.Formals (t1);
  END Initialize;

BEGIN
END WordLE.
