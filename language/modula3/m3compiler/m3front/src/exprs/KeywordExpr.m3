(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: KeywordExpr.m3                                        *)
(* Last modified on Fri Feb 24 16:43:45 PST 1995 by kalsow     *)
(*      modified on Sat Dec  8 00:21:11 1990 by muller         *)

MODULE KeywordExpr;

IMPORT M3, M3ID, Expr, ExprRep, Type, Target;

TYPE
  P = Expr.T OBJECT
        expr : Expr.T;
	key  : M3ID.T;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := Bounder;
        isWritable   := IsWritable;
        isDesignator := IsDesignator;
	isZeroes     := IsZeroes;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := PrepLiteral;
	genLiteral   := GenLiteral;
        note_write   := NoteWrites;
      END;

PROCEDURE New (key: M3ID.T;  arg: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.key  := key;
    p.expr := arg;
    RETURN p;
  END New;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Is;

PROCEDURE Split (e: Expr.T;  VAR key: M3ID.T;  VAR value: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => key := p.key;  value := p.expr;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Expr.TypeOf (p.expr);
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    p.type := Expr.TypeOf (p.expr);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Expr.IsEqual (a.expr, b.expr, x);
    ELSE      RETURN Expr.IsEqual (a.expr, e, x);
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Expr.NeedsAddress (p.expr);
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  BEGIN
    Expr.Prep (p.expr);
  END Prep;

PROCEDURE Compile (p: P) =
  BEGIN
    Expr.Compile (p.expr);
  END Compile;

PROCEDURE PrepLV (p: P) =
  BEGIN
    Expr.PrepLValue (p.expr);
  END PrepLV;

PROCEDURE CompileLV (p: P) =
  BEGIN
    Expr.CompileLValue (p.expr);
  END CompileLV;

PROCEDURE Bounder (p: P;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (p.expr, min, max);
  END Bounder;

PROCEDURE IsDesignator (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (p.expr);
  END IsDesignator;

PROCEDURE IsWritable (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (p.expr);
  END IsWritable;

PROCEDURE Fold (p: P): Expr.T =
  BEGIN
    WITH e = Expr.ConstValue (p.expr) DO
      IF e = NIL THEN
        RETURN NIL
      ELSE
        p.expr := e; 
        RETURN p; 
      END;
    END;
  END Fold;

PROCEDURE IsZeroes (p: P): BOOLEAN =
  BEGIN
    RETURN Expr.IsZeroes (p.expr);
  END IsZeroes;

PROCEDURE PrepLiteral (p: P;  type: Type.T) =
  BEGIN
    Expr.PrepLiteral (p.expr, type);
  END PrepLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  type: Type.T) =
  BEGIN
    Expr.GenLiteral (p.expr, offset, type);
  END GenLiteral;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Expr.NoteWrite (p.expr);
  END NoteWrites;

BEGIN
END KeywordExpr.
