(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CompareExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:41:37 PST 1995 by kalsow     *)
(*      modified on Thu Nov 29 03:32:44 1990 by muller         *)

MODULE CompareExpr;

IMPORT M3, CG, Expr, ExprRep, Type, Int, Reel, LReel, EReel;
IMPORT EnumType, SetType, Bool, Module, Addr, Target, TInt;
IMPORT IntegerExpr, EnumExpr, ReelExpr, AddressExpr;
IMPORT SetExpr, Error;

CONST
  cINT   = 0;
  cREAL  = 1;
  cLONG  = 2;
  cEXTND = 3;
  cADDR  = 4;
  cENUM  = 5;
  cSET   = 6;

CONST
  NotOp = ARRAY Op OF Op { Op.NE, Op.EQ, Op.GE, Op.GT, Op.LE, Op.LT };

CONST
  CGType = ARRAY [0..5] OF CG.Type {
             CG.Type.Int,   CG.Type.Reel, CG.Type.LReel,
             CG.Type.XReel, CG.Type.Addr, CG.Type.Int    };

TYPE
  CompareOp = [Op.LT .. Op.GE];

TYPE
  P = ExprRep.Tabc BRANDED "CompareExpr.P" OBJECT
        op      : CompareOp;
        bad_set : BOOLEAN;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := PrepBR;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := ExprRep.NotWritable;
      END;

TYPE
  OpDesc = RECORD signA, signB : INTEGER;  name: TEXT END;

CONST
  Ops = ARRAY CompareOp OF OpDesc {
    (*LT*) OpDesc { -1, -1, "\'<\'" },
    (*LE*) OpDesc { -1,  0, "\'<=\'" },
    (*GT*) OpDesc {  1,  1, "\'>\'" },
    (*GE*) OpDesc {  1,  0, "\'>=\'" }
  };

PROCEDURE NewLT (a, b: Expr.T): Expr.T =
  BEGIN RETURN Create (a, b, Op.LT) END NewLT;

PROCEDURE NewLE (a, b: Expr.T): Expr.T =
  BEGIN RETURN Create (a, b, Op.LE) END NewLE;

PROCEDURE NewGT (a, b: Expr.T): Expr.T =
  BEGIN RETURN Create (a, b, Op.GT) END NewGT;

PROCEDURE NewGE (a, b: Expr.T): Expr.T =
  BEGIN RETURN Create (a, b, Op.GE) END NewGE;

PROCEDURE Create (a, b: Expr.T;  op: CompareOp): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a    := a;
    p.b    := b;
    p.op   := op;
    p.type := Bool.T;
    p.bad_set := FALSE;
    RETURN p;
  END Create;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR ta, tb, range: Type.T;  info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Type.Base (Expr.TypeOf (p.b));
    p.class := cINT;
    IF    (ta = Int.T)   AND (tb = Int.T)    THEN  p.class := cINT;
    ELSIF (ta = Reel.T)  AND (tb = Reel.T)   THEN  p.class := cREAL;
    ELSIF (ta = LReel.T) AND (tb = LReel.T)  THEN  p.class := cLONG;
    ELSIF (ta = EReel.T) AND (tb = EReel.T)  THEN  p.class := cEXTND;
    ELSIF (Type.IsSubtype (ta, Addr.T)) AND (Type.IsSubtype (tb, Addr.T)) THEN
      IF Module.IsSafe () THEN Error.Msg ("unsafe operation") END;
      p.class := cADDR;
    ELSIF  NOT Type.IsEqual (ta, tb, NIL)    THEN  Err (p, ta, tb);
    ELSIF EnumType.Is (ta)                   THEN  p.class := cENUM;
    ELSIF SetType.Split (ta, range)          THEN
      p.class := cSET;
      ta := Type.CheckInfo (ta, info);
    ELSE Err (p, ta, tb);
    END;

    IF (p.class = cSET)
      AND ((p.op = Op.LT) OR (p.op = Op.GT))
      AND (info.size <= Target.Integer.size) THEN
      p.bad_set := TRUE;
    END;
  END Check;

PROCEDURE Err (p: P;  a, b: Type.T) =
  BEGIN
    p.type := Expr.BadOperands (Ops[p.op].name, a, b);
  END Err;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.class = b.class)
                 AND (a.op = b.op)
		 AND Expr.IsEqual (a.a, b.a, x)
                 AND Expr.IsEqual (a.b, b.b, x);
    ELSE RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
  END Prep;

PROCEDURE Compile (p: P) =
  VAR sz: INTEGER;  type: CG.Type;  ta, tb, tmp: CG.Val;  info: Type.Info;
  BEGIN
    IF (p.class # cSET) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      type := CGType [p.class];
      CASE p.op OF
      | Op.LT =>  CG.Lt (type);
      | Op.LE =>  CG.Le (type);
      | Op.GT =>  CG.Gt (type);
      | Op.GE =>  CG.Ge (type);
      END;

    ELSIF (p.bad_set) THEN
      Expr.Compile (p.a);  ta := CG.Pop ();
      Expr.Compile (p.b);  tb := CG.Pop ();
      IF (p.op = Op.GT) THEN tmp := ta;  ta := tb;  tb := tmp END;
      CG.Push (ta);
      CG.Push (tb);
      CG.Ne (CG.Type.Word);
      CG.Push (ta);
      CG.Push (tb);
      CG.Or ();
      CG.Push (tb);
      CG.Eq (CG.Type.Word);
      CG.And ();
      CG.Free (ta);
      CG.Free (tb);

    ELSE (* simple set ops *)
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      EVAL Type.CheckInfo (Expr.TypeOf (p.a), info);
      sz := info.size;
      CASE p.op OF
      | Op.LT =>  CG.Set_lt (sz);
      | Op.LE =>  CG.Set_le (sz);
      | Op.GT =>  CG.Set_gt (sz);
      | Op.GE =>  CG.Set_ge (sz);
      END;

    END;
  END Compile;

PROCEDURE PrepBR (p: P;  true, false: CG.Label;  freq: CG.Frequency) =
  VAR sz: INTEGER;  type: CG.Type;  op: CompareOp;  ta, tb, tmp: CG.Val;
      info: Type.Info;
  BEGIN
    Expr.Prep (p.a);
    Expr.Prep (p.b);
    IF (p.class # cSET) THEN
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      type := CGType [p.class];
      op := p.op;
      IF (true = CG.No_label) THEN
        true := false;
        op := NotOp [op];
        freq := CG.Always - freq;
      END;
      CASE op OF
      | Op.LT =>  CG.If_lt (true, type, freq);
      | Op.LE =>  CG.If_le (true, type, freq);
      | Op.GT =>  CG.If_gt (true, type, freq);
      | Op.GE =>  CG.If_ge (true, type, freq);
      END;

    ELSIF (p.bad_set) THEN
      Expr.Compile (p.a);  ta := CG.Pop ();
      Expr.Compile (p.b);  tb := CG.Pop ();
      IF (p.op = Op.GT) THEN tmp := ta;  ta := tb;  tb := tmp END;
      CG.Push (ta);
      CG.Push (tb);
      CG.Ne (CG.Type.Word);
      CG.Push (ta);
      CG.Push (tb);
      CG.Not ();
      CG.And ();
      CG.Load_integer (TInt.Zero);
      CG.Eq (CG.Type.Word);
      CG.And ();
      IF (true # CG.No_label)
        THEN CG.If_true  (true,  freq);
        ELSE CG.If_false (false, freq);
      END;
      CG.Free (ta);
      CG.Free (tb);

    ELSE (* simple set ops *)
      Expr.Compile (p.a);
      Expr.Compile (p.b);
      EVAL Type.CheckInfo (Expr.TypeOf (p.a), info);
      sz := info.size;
      CASE p.op OF
      | Op.LT =>  CG.Set_lt (sz);
      | Op.LE =>  CG.Set_le (sz);
      | Op.GT =>  CG.Set_gt (sz);
      | Op.GE =>  CG.Set_ge (sz);
      END;
      IF (true # CG.No_label)
        THEN CG.If_true  (true, freq);
        ELSE CG.If_false (false, freq);
      END;
    END;
  END PrepBR;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2: Expr.T;  s: INTEGER;  op: CompareOp;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    IF (e1 = NIL) THEN RETURN NIL END;
    e2 := Expr.ConstValue (p.b);
    IF (e2 = NIL) THEN RETURN NIL END;
    op := p.op;
    IF   IntegerExpr.Compare (e1, e2, s)
      OR EnumExpr.Compare (e1, e2, s)
      OR ReelExpr.Compare (e1, e2, s)
      OR AddressExpr.Compare (e1, e2, s)
      OR SetExpr.Compare (e1, e2, s)
      THEN
      RETURN Bool.Map[(s = Ops[op].signA) OR (s = Ops[op].signB)];
    END;
    RETURN NIL;
  END Fold;

BEGIN
END CompareExpr.
