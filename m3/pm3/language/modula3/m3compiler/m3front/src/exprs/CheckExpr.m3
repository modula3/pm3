(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CheckExpr.m3                                          *)
(* Last Modified On Fri Feb 24 16:41:16 PST 1995 By kalsow     *)
(*      Modified On Fri Feb 15 04:03:38 1991 By muller         *)

MODULE CheckExpr;

IMPORT M3, CG, Expr, ExprRep, Type, IntegerExpr, EnumExpr, Host;
IMPORT Target, TInt, Error;

TYPE
  Class = { cLOWER, cUPPER, cBOTH };

TYPE
  P = Expr.T OBJECT
        expr  : Expr.T;
        min   : Target.Int;
        max   : Target.Int;
        class : Class;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (a: Expr.T;  READONLY min, max: Target.Int): Expr.T =
  BEGIN
    RETURN Create (a, min, max, Class.cBOTH);
  END New;

PROCEDURE NewLower (a: Expr.T;  READONLY min: Target.Int): Expr.T =
  BEGIN
    RETURN Create (a, min, TInt.Zero, Class.cLOWER);
  END NewLower;

PROCEDURE NewUpper (a: Expr.T;  READONLY max: Target.Int): Expr.T =
  BEGIN
    RETURN Create (a, TInt.Zero, max, Class.cUPPER);
  END NewUpper;

PROCEDURE Create (a: Expr.T; READONLY min, max: Target.Int; c: Class): Expr.T =
  VAR p: P;
  BEGIN
    IF (NOT Host.doRangeChk) THEN RETURN a END;
    p := NEW (P);
    ExprRep.Init (p);
    p.expr   := a;
    p.min    := min;
    p.max    := max;
    p.class  := c;
    p.origin := a.origin;
    RETURN p;
  END Create;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.TypeOf (p.expr);
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Expr.TypeCheck (p.expr, cs);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.class = b.class)
                 AND (a.min = b.min)
                 AND (a.max = b.max)
                 AND Expr.IsEqual (a.expr, b.expr, x);
    ELSE RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.expr);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    Expr.Compile (p.expr);
    CASE p.class OF
    | Class.cLOWER => CG.Check_lo (p.min);
    | Class.cUPPER => CG.Check_hi (p.max);
    | Class.cBOTH  => CG.Check_range (p.min, p.max);
    END;
  END Compile;

PROCEDURE Emit (e: Expr.T;  READONLY min, max: Target.Int) =
  VAR minE, maxE: Target.Int;  x: Expr.T;
  BEGIN
    x := Expr.ConstValue (e);
    IF (x # NIL) THEN e := x;  END;
    Expr.Compile (e);
    IF Host.doRangeChk THEN
      Expr.GetBounds (e, minE, maxE);
      IF TInt.LT (minE, min) AND TInt.LT (max, maxE) THEN
        CG.Check_range (min, max);
      ELSIF TInt.LT (minE, min) THEN
        IF TInt.LT (maxE, min) THEN
          Error.Warn (2, "value out of range");
        END;
        CG.Check_lo (min);
      ELSIF TInt.LT (max, maxE) THEN
        IF TInt.LT (max, minE) THEN
          Error.Warn (2, "value out of range");
        END;
        CG.Check_hi (max);
      END;
    END;
  END Emit;

PROCEDURE Fold (p: P): Expr.T =
  VAR e: Expr.T;  i: Target.Int;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (p.expr);
    IF (e = NIL) THEN RETURN NIL END;
    IF (NOT IntegerExpr.Split (e, i))
      AND (NOT EnumExpr.Split (e, i, t)) THEN
      RETURN NIL;
    END;
    CASE p.class OF
    | Class.cLOWER => IF TInt.LT (i, p.min) THEN RETURN NIL END;
    | Class.cUPPER => IF TInt.LT (p.max, i) THEN RETURN NIL END;
    | Class.cBOTH  => IF TInt.LT (i, p.min)
                      OR TInt.LT (p.max, i) THEN RETURN NIL END;
    END;
    RETURN e;
  END Fold;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (p.expr, min, max);
    CASE p.class OF
    | Class.cLOWER => IF TInt.LT (min, p.min) THEN min := p.min END;
    | Class.cUPPER => IF TInt.LT (p.max, max) THEN max := p.max END;
    | Class.cBOTH  => IF TInt.LT (min, p.min) THEN min := p.min END;
                      IF TInt.LT (p.max, max) THEN max := p.max END;
    END;
  END Bounder;

BEGIN
END CheckExpr.
