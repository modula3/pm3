(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubscriptExpr.m3                                      *)
(* Last modified on Thu Nov 10 12:06:15 PST 1994 by kalsow     *)
(*      modified on Thu Mar  7 01:44:07 1991 by muller         *)

MODULE SubscriptExpr;

IMPORT CG, Expr, ExprRep, ArrayType, Error, Type, Int;
IMPORT ArrayExpr, OpenArrayType, Host, EnumExpr;
IMPORT CheckExpr, SubtractExpr, IntegerExpr, ErrType;
IMPORT RefType, DerefExpr, Target, TInt, M3RT;

TYPE
  P = ExprRep.Tab BRANDED "SubscriptExpr.P" OBJECT
        biased_b : Expr.T;
        depth    : INTEGER;  (* open array depth before subscripting *)
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := Prep;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := ExprRep.EqCheckAB;
        getBounds    := ExprRep.NoBounds;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
      END;

PROCEDURE New (a, b: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.a        := a;
    p.b        := b;
    p.biased_b := NIL;
    p.depth    := 0;
    RETURN p;
  END New;

PROCEDURE TypeOf (p: P): Type.T =
  VAR ta, ti, te: Type.T;
  BEGIN
    ta := Type.Base (Expr.TypeOf (p.a));

    IF RefType.Is (ta) THEN
      (* auto-magic dereference *)
      p.a := DerefExpr.New (p.a);
      p.a.origin := p.origin;
      ta := Type.Base (Expr.TypeOf (p.a));
    END;

    IF ArrayType.Split (ta, ti, te)
      THEN RETURN te;
      ELSE RETURN ta;
    END;
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    ta, tb, ti, te: Type.T;
    mini, maxi, minb, maxb, z: Target.Int;
    b: BOOLEAN;
  BEGIN
    Expr.TypeCheck (p.a, cs);
    Expr.TypeCheck (p.b, cs);
    ta := Type.Base (Expr.TypeOf (p.a));
    tb := Expr.TypeOf (p.b);

    IF RefType.Is (ta) THEN
      (* auto-magic dereference *)
      p.a := DerefExpr.New (p.a);
      p.a.origin := p.origin;
      Expr.TypeCheck (p.a, cs);
      ta := Type.Base (Expr.TypeOf (p.a));
    END;

    ta := Type.Check (ta);
    IF NOT ArrayType.Split (ta, ti, te) THEN
      Error.Msg ("subscripted expression is not an array");
      p.type := ErrType.T;
      RETURN;
    END;
    p.type := te;
    Expr.NeedsAddress (p.a);

    EVAL Type.GetBounds (ti, mini, maxi);
    Expr.GetBounds (p.b, minb, maxb);

    p.biased_b := p.b;
    IF (ti = NIL) THEN
      (* a is an open array *)
      p.depth := OpenArrayType.OpenDepth (ta);
      IF NOT Type.IsSubtype (tb, Int.T) THEN
        Error.Msg ("open arrays must be indexed by integer expressions");
      END;

    ELSIF Type.IsSubtype (tb, Type.Base (ti)) THEN
      (* the index value's type has a common base type with the index type *)
      IF NOT TInt.EQ (mini, TInt.Zero) THEN
        p.biased_b := SubtractExpr.New (p.b, IntegerExpr.New (mini), TRUE);
        p.biased_b.origin := p.origin;
        Expr.TypeCheck (p.biased_b, cs);
      END;
      IF TInt.LT (minb, mini) AND TInt.LT (maxi, maxb) THEN
        b := TInt.Subtract (maxi, mini, z);  <*ASSERT b *>
        p.biased_b := CheckExpr.New (p.biased_b, TInt.Zero, z);
        p.biased_b.origin := p.origin;
        Expr.TypeCheck (p.biased_b, cs);
      ELSIF TInt.LT (minb, mini) THEN
        IF TInt.LT (maxb, mini) THEN
          Error.Warn (2, "subscript is out of range");
        END;
        p.biased_b := CheckExpr.NewLower (p.biased_b, TInt.Zero);
        p.biased_b.origin := p.origin;
        Expr.TypeCheck (p.biased_b, cs);
      ELSIF TInt.LT (maxi, maxb) THEN
        IF TInt.LT (maxi, minb) THEN
          Error.Warn (2, "subscript is out of range");
        END;
        b := TInt.Subtract (maxi, mini, z);  <*ASSERT b *>
        p.biased_b := CheckExpr.NewUpper (p.biased_b, z);
        p.biased_b.origin := p.origin;
        Expr.TypeCheck (p.biased_b, cs);
      END;

    ELSE
      Error.Msg ("incompatible array index");
    END;
  END Check;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Expr.NeedsAddress (p.a);
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR e := Expr.ConstValue (p.biased_b);
  BEGIN
    IF (e # NIL) THEN p.biased_b := e; END;
    IF Expr.IsDesignator (p.a)
      THEN Expr.PrepLValue (p.a);
      ELSE Expr.Prep (p.a);
    END;
    Expr.Prep (p.biased_b);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    CompileLV (p);
    Type.LoadScalar (p.type);
  END Compile;

PROCEDURE CompileLV (p: P) =
  VAR
    ti, te    : Type.T;
    subscript : INTEGER;
    subs      : Target.Int;
    e         : Expr.T;
    t         : Type.T;
    fixed     := FALSE;
    ta        := Type.Base (Expr.TypeOf (p.a));
    b         := ArrayType.Split (ta, ti, te);
    elt_pack  := ArrayType.EltPack (ta);
    t1, t2    : CG.Val;
    t3        : CG.Var;
  BEGIN
    <* ASSERT b *>

    IF Expr.IsDesignator (p.a)
      THEN Expr.CompileLValue (p.a);
      ELSE Expr.Compile (p.a);
    END;

    IF (p.depth = 0) THEN
      (* a is a fixed array *)
      e := Expr.ConstValue (p.biased_b);
      IF (e # NIL) THEN
        fixed := (IntegerExpr.Split (e, subs))
                  OR (EnumExpr.Split (e, subs, t));
        fixed := fixed AND TInt.ToInt (subs, subscript);
      END;

      IF (fixed) THEN
        CG.Add_offset (subscript * elt_pack);
      ELSE
        Expr.Compile (p.biased_b);
        ArrayType.GenIndex (ta);
      END;

    ELSIF (p.depth = 1) THEN
      (* a is a single dimension open array *)
      t1 := CG.Pop ();
      CG.Push (t1);
      CG.Open_elt_ptr (ArrayType.EltAlign (ta));
      Expr.Compile (p.biased_b);
      IF Host.doRangeChk THEN
        (* range check the subscript *)
        CG.Push (t1);
        CG.Open_size (0);
        CG.Check_index ();
      END;
      CG.Index_bytes (elt_pack);
      CG.Boost_alignment (ArrayType.EltAlign (ta));
      CG.Free (t1);

    ELSE
      (* a is a multi-dimensional open array *)

      (* evaluate the subexpressions & allocate space for the result *)
      t1 := CG.Pop ();

      Expr.Compile (p.biased_b);
      IF Host.doRangeChk THEN
        (* range check the subscript *)
        CG.Push (t1);
        CG.Open_size (0);
        CG.Check_index ();
      END;
      t2 := CG.Pop ();

      (* allocate a new dope vector *)
      t3 := OpenArrayType.DeclareTemp (ta);

      (* copy the rest of the dope vector *)
      FOR i := 1 TO p.depth-1 DO
        CG.Push (t1);
        CG.Open_size (i);
        CG.Store_int (t3, M3RT.OA_sizes + (i-1) * Target.Integer.pack);
      END;

      (* build the new data pointer *)
      CG.Push (t1);
      CG.Open_elt_ptr (ArrayType.EltAlign (ta));
      CG.Push (t2);
      FOR i := 0 TO p.depth-2 DO
        CG.Load_int (t3, M3RT.OA_sizes + i * Target.Integer.pack);
        CG.Multiply (CG.Type.Word);
      END;
      CG.Index_bytes (elt_pack);
      CG.Store_addr (t3, M3RT.OA_elt_ptr);
      CG.Load_addr_of_temp (t3, 0, Target.Address.align);
      CG.Free (t1);
      CG.Free (t2);
    END;
  END CompileLV;

PROCEDURE IsDesignator (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (p.a);
  END IsDesignator;

PROCEDURE IsWritable (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (p.a);
  END IsWritable;

PROCEDURE Fold (p: P): Expr.T =
  VAR e1, e2, e3: Expr.T;
  BEGIN
    e1 := Expr.ConstValue (p.a);
    e2 := Expr.ConstValue (p.b);
    e3 := NIL;
    IF e1 = NIL OR e2 = NIL                THEN RETURN NIL
    ELSIF ArrayExpr.Subscript (e1, e2, e3) THEN RETURN e3
    ELSE                                        RETURN NIL; END;
  END Fold;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.a);
  END NoteWrites;

BEGIN
END SubscriptExpr.
