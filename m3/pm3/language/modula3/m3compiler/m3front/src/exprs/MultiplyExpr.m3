(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MultiplyExpr.m3                                       *)
(* Last modified on Wed Feb 22 09:07:35 PST 1995 by kalsow     *)
(*      modified on Wed Nov 21 21:55:34 1990 by muller         *)

MODULE MultiplyExpr;

IMPORT CG, Expr, ExprRep, Type, Int, Reel, EReel, Target;
IMPORT SetExpr, IntegerExpr, ReelExpr, LReel, SetType, ErrType;

CONST
  cINT   = 0;
  cREAL  = 1;
  cLONG  = 2;
  cEXTND = 3;
  cSET   = 4;

CONST
  CGType = ARRAY [0..3] OF CG.Type {
             CG.Type.Int, CG.Type.Reel, CG.Type.LReel, CG.Type.XReel };

TYPE
  P = ExprRep.Tabc BRANDED "MultiplyExpr.T" OBJECT
        tmp : CG.Var;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckAB;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a := a;
    p.b := b;
    p.tmp := NIL;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.TypeOf (p.a);
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb, range: Type.T;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    IF    (tb = Int.T)   AND (ta = Int.T)   THEN
      p.class := cINT;  INC (cs.int_ops);
    ELSIF (tb = Reel.T)  AND (ta = Reel.T)  THEN
      p.class := cREAL;  INC (cs.fp_ops);
    ELSIF (tb = LReel.T) AND (ta = LReel.T) THEN
      p.class := cLONG;  INC (cs.fp_ops);
    ELSIF (tb = EReel.T) AND (ta = EReel.T) THEN
      p.class := cEXTND;  INC (cs.fp_ops);
    ELSIF (ta = ErrType.T) OR (tb = ErrType.T) THEN
      p.class := cINT; (* there's already an error *)
      ta := ErrType.T;
    ELSIF SetType.Split (ta, range) AND Type.IsEqual (ta, tb, NIL) THEN
      p.class := cSET;
    ELSE
      ta := Expr.BadOperands ("\'*\'", ta, tb);
      p.class := cINT;
    END;
    p.type := ta;
  END Check;

PROCEDURE Prep (p: P) =
  VAR size: INTEGER;  info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.class = cSET) THEN
      EVAL Type.CheckInfo (p.type, info);
      size := info.size;
      IF (size > Target.Integer.size) THEN
        p.tmp := CG.Declare_temp (size, Target.Integer.align,
                                  CG.Type.Struct, in_memory := TRUE);
        CG.Load_addr_of (p.tmp, 0, Target.Integer.align);
        CG.Force ();
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Set_intersection (size);
      END;
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR size: INTEGER;  info: Type.Info;
  BEGIN
    IF (p.class # cSET) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      CG.Multiply (CGType [p.class]);
    ELSE
      EVAL Type.CheckInfo (p.type, info);
      size := info.size;
      IF (size > Target.Integer.size) THEN
        CG.Load_addr_of_temp (p.tmp, 0, Target.Integer.align);
        p.tmp := NIL;
      ELSE
        Expr.Compile (p.a);
        Expr.Compile (p.b);
        CG.Set_intersection (size);
      END;
    END;
  END Compile;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF    (e1 = NIL) OR (e2 = NIL)          THEN
    ELSIF IntegerExpr.Multiply (e1, e2, e3) THEN
    ELSIF ReelExpr.Multiply    (e1, e2, e3) THEN
    ELSIF SetExpr.Intersection (e1, e2, e3) THEN
    END;
    RETURN e3;
  END Fold;

BEGIN
END MultiplyExpr.
