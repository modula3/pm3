(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Narrow.m3                                             *)
(* Last Modified On Wed Jun 29 17:29:43 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:20 1990 By muller         *)

MODULE Narrow;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Error, TypeExpr;
IMPORT Procedure, ObjectType, Reff, Null, M3RT, RefType;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t)
      THEN RETURN t;
      ELSE RETURN Expr.TypeOf (ce.args[0]);
    END;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR dest: Type.T;  src := Expr.TypeOf (ce.args[0]);
  BEGIN
    IF NOT TypeExpr.Split (ce.args[1], dest) THEN
      Error.Msg ("NARROW: second argument must be a type");
      dest := src;
    END;

    IF NOT Type.IsAssignable (dest, src) THEN
      Error.Msg ("NARROW: types must be assignable");
    ELSIF ObjectType.Is (dest) OR Type.IsSubtype (dest, Reff.T) THEN
      (* ok *)
    ELSE (* untraced ref type *)
      Error.Msg ("NARROW: must be a traced reference or object type");
    END;

    ce.type := dest;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  VAR t: Type.T;
  BEGIN
    EVAL TypeExpr.Split (ce.args[1], t);
    Type.Compile (t);
    Expr.Prep (ce.args[0]);
    Expr.Compile (ce.args[0]);
    ce.tmp := EmitCore (t, Expr.TypeOf (ce.args[0]));
    IF (ce.tmp = NIL) THEN
      (* capture the ref value *)
      ce.tmp := CG.Pop_temp ();
    END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the work was done by Prep *)
    CG.Push (ce.tmp);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END Compile;

PROCEDURE Emit (tlhs, trhs: Type.T) =
  VAR tmp := EmitCore (tlhs, trhs);
  BEGIN
    IF (tmp # NIL) THEN
      (* reload the ref value on the stack *)
      CG.Push (tmp);
      CG.Free (tmp);
    END;
  END Emit;

PROCEDURE EmitCore (tlhs, trhs: Type.T): CG.Val =
  VAR
    ok: CG.Label;
    ref, tc: CG.Val;
    is_object := ObjectType.Is (tlhs);
    target: Type.T;
    align: INTEGER;
    lhs_info, info: Type.Info;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, lhs_info);
    IF is_object THEN
      align := ObjectType.FieldAlignment (tlhs);
      CG.Boost_alignment (align);
    ELSIF RefType.Split (tlhs, target) THEN
      target := Type.CheckInfo (target, info);
      align := info.alignment;
      CG.Boost_alignment (align);
    END;

    (* test for the no-check cases... *)
    IF Type.IsSubtype (trhs, tlhs) THEN RETURN NIL; END;
    IF (NOT is_object) AND (NOT lhs_info.isTraced) THEN RETURN NIL; END;

    (* capture the right-hand side and get a couple labels *)
    ref := CG.Pop ();
    CG.Push (ref);
    ok := CG.Next_label (2);

    (* check for rhs = NIL *)
    CG.Load_nil ();
    CG.If_eq (ok, CG.Type.Addr, CG.Maybe);

    IF (Type.IsEqual (tlhs, Null.T, NIL)) THEN
      (* no more checking is needed *)
    ELSIF NOT is_object THEN
      CG.Push (ref);
      CG.Ref_to_typecode ();
      Type.LoadInfo (tlhs, M3RT.TC_typecode);
      CG.If_eq (ok, CG.Type.Int, CG.Always);
    ELSE (* object *)
      CG.Push (ref);
      CG.Ref_to_typecode ();
      tc := CG.Pop ();
      CG.Push (tc);
      Type.LoadInfo (tlhs, M3RT.TC_typecode);
      CG.If_lt (ok+1, CG.Type.Int, CG.Never);
      CG.Push (tc);
      Type.LoadInfo (tlhs, M3RT.TC_lastSubTypeTC);
      CG.If_le (ok, CG.Type.Int, CG.Always);
      CG.Free (tc);
      CG.Set_label (ok+1);
    END;

    (* generate the fault and the by-pass label *)
    CG.Narrow_fault ();
    CG.Set_label (ok);

    RETURN ref;
  END EmitCore;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  BEGIN
    RETURN Expr.ConstValue (ce.args[0]);
  END Fold;

PROCEDURE NoteWrites (ce: CallExpr.T) =
  BEGIN
    Expr.NoteWrite (ce.args[0]);
  END NoteWrites;

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
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 NoteWrites);
    Procedure.Define ("NARROW", Z, TRUE);
  END Initialize;

BEGIN
END Narrow.

