(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Field.m3                                              *)
(* Last modified on Wed Mar  1 08:43:31 PST 1995 by kalsow     *)
(*      modified on Fri Apr 20 06:47:07 1990 by muller         *)

MODULE Field;

IMPORT M3, CG, Value, ValueRep, Type, Expr, Error;
IMPORT AssignStmt, M3Buf;

TYPE
  T = Value.T BRANDED OBJECT
        index   : INTEGER;
        offset  : INTEGER;
        tipe    : Type.T;
        dfault  : Expr.T;
        transient : BOOLEAN;
      OVERRIDES
        typeCheck   := TypeCheck;
        set_globals := SetGlobals;
        load        := ValueRep.NoLoader;
        declare     := ValueRep.Never;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := Compile;
        user_init   := ValueRep.NoInit;
	toExpr      := ValueRep.NoExpr;
	toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

PROCEDURE New (READONLY info: Info): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, info.name, Value.Class.Field);
    t.index  := info.index;
    t.offset := info.offset;
    t.tipe   := info.type;
    t.dfault := info.dfault;
    t.transient := info.transient;
    RETURN t;
  END New;

PROCEDURE Is (v: Value.T): BOOLEAN =
  BEGIN
    TYPECASE v OF
    | NULL =>  RETURN FALSE;
    | T    =>  RETURN TRUE;
    ELSE       RETURN FALSE;
    END;
  END Is;

PROCEDURE Split (field: Value.T;  VAR info: Info) =
  VAR t: T := field;
  BEGIN
    info.name   := t.name;
    info.index  := t.index;
    info.offset := t.offset;
    info.type   := t.tipe;
    info.dfault := t.dfault;
    info.transient := t.transient;
  END Split;

PROCEDURE SetOffset (field: Value.T;  newOffset: INTEGER) =
  VAR t: T := field;
  BEGIN
    t.offset := newOffset;
  END SetOffset;

PROCEDURE EmitDeclaration (field: Value.T) =
  VAR
    t: T := field;
    info : Type.Info;
  BEGIN
    EVAL Type.CheckInfo (t.tipe, info);
    Type.Compile (t.tipe);
    CG.Declare_field (t.name, t.offset, info.size, Type.GlobalUID (t.tipe));
  END EmitDeclaration;

PROCEDURE IsEqualList (a, b: Value.T;  x: Type.Assumption;
                       types: BOOLEAN): BOOLEAN =
  BEGIN
    WHILE (a # NIL) AND (b # NIL) DO
      IF NOT IsEqual (a, b, x, types) THEN RETURN FALSE END;
      a := a.next;  b := b.next;
    END;
    RETURN (a = NIL) AND (b = NIL);
  END IsEqualList;

PROCEDURE IsEqual (va, vb: Value.T;  x: Type.Assumption;
                   types: BOOLEAN): BOOLEAN =
  VAR a: T := va;  b: T := vb;
  BEGIN
    IF (a = NIL) OR (b = NIL) OR (a.name # b.name) OR (a.index # b.index) THEN
      RETURN FALSE;
    END;
    IF NOT types THEN RETURN TRUE; END;

    (* now, we'll do the harder type-based checks... *)
    RETURN Type.IsEqual (TypeOf (a), TypeOf (b), x)
       AND Expr.IsEqual (Expr.ConstValue (a.dfault),
                         Expr.ConstValue (b.dfault), x);
  END IsEqual;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.dfault) END;
    RETURN t.tipe;
  END TypeOf;

PROCEDURE TypeCheck (t: T;  VAR cs: Value.CheckState) =
  VAR info: Type.Info;
  BEGIN
    t.tipe := Type.CheckInfo (TypeOf (t), info);
    IF (info.isEmpty) THEN
      Error.ID (t.name, "empty field type");
    END;
    IF (info.class = Type.Class.OpenArray) THEN
      Error.ID (t.name, "fields may not be open arrays");
    END;
    t.checked := TRUE;

    IF t.transient AND info.isTransient THEN
      Error.Warn (1, "field type already transient, <*TRANSIENT*> ignored");
    END;

    IF (t.dfault # NIL) THEN
      (* check for assignability!! *)
      AssignStmt.Check (t.tipe, t.dfault, cs);
      Expr.TypeCheck (t.dfault, cs);
      IF (Expr.ConstValue (t.dfault) = NIL) THEN
        Error.ID (t.name, "default is not a constant");
      END;
      (* NOTE: we don't save the constant-folded version of the default,
         otherwise we'd loose references to large named constants. *)
    END;
  END TypeCheck;

PROCEDURE Compile (t: T) =
  BEGIN
    Type.Compile (t.tipe);
  END Compile;

PROCEDURE SetGlobals (<*UNUSED*> t: T) =
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* IF (t.dfault # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.dfault)) END; *)
  END SetGlobals;

PROCEDURE AddFPTag  (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "FIELD ", 0, global := FALSE);
    IF (t.dfault # NIL) THEN
      M3Buf.PutText (x.buf, " := ");
      Expr.GenFPLiteral (t.dfault, x.buf);
    END;
    RETURN 1;
  END AddFPTag;

BEGIN
END Field.
