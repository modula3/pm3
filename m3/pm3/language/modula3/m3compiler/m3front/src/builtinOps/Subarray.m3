(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Subarray.m3                                           *)
(* Last Modified On Sat Mar 23 13:48:43 PST 1996 By heydon     *)
(*      Modified On Wed Jun 29 17:03:23 PDT 1994 By kalsow     *)
(*      Modified On Thu Mar  7 20:18:53 1991 By muller         *)

MODULE Subarray;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error, ArrayType, Card;
IMPORT OpenArrayType, CheckExpr, Host, Target, TInt, M3RT, IntegerExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  BEGIN
    RETURN ArrayType.OpenCousin (Type.Base (Expr.TypeOf (ce.args[0])));
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, u, v, index, elt: Type.T;  info: Type.Info;
  BEGIN
    t := Type.Base (Expr.TypeOf (ce.args[0]));
    u := Expr.TypeOf (ce.args[1]);
    v := Expr.TypeOf (ce.args[2]);
    IF (NOT ArrayType.Split (t, index, elt)) THEN
      Error.Msg ("SUBARRAY: first argument must be an array");
    ELSIF (NOT Type.IsAssignable (Card.T, u)) THEN
      Error.Msg ("SUBARRAY: second argrment must be assignable to CARDINAL");
    ELSIF (NOT Type.IsAssignable (Card.T, v)) THEN
      Error.Msg ("SUBARRAY: third argument must be assignable to CARDINAL");
    ELSE
      elt := Type.CheckInfo (elt, info);
      IF (info.class # Type.Class.OpenArray)
        AND (info.size MOD Target.Byte) # 0 THEN
        Error.Msg ("SUBARRAY: array elements are not byte-aligned.");
      END;
      ce.args[1] := CheckPositive (ce.args[1], cs);
      ce.args[2] := CheckPositive (ce.args[2], cs);
    END;
    Expr.NeedsAddress (ce.args[0]);
    t := ArrayType.OpenCousin (t);
    ce.type := Type.Check (t);
    INC (cs.int_ops);
  END Check;

PROCEDURE CheckPositive (e: Expr.T;  VAR cs: Expr.CheckState): Expr.T =
  VAR min, max: Target.Int; BEGIN
    IF e = NIL THEN RETURN NIL END;
    Expr.GetBounds (e, min, max);
    IF TInt.LT (min, TInt.Zero) OR TInt.LT (max, min) THEN
      e := CheckExpr.NewLower (e, TInt.Zero);
      Expr.TypeCheck (e, cs);
    END;
    RETURN e;
  END CheckPositive;

PROCEDURE NeedsAddress (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    (* yes, all subarray's get memory addresses *)
  END NeedsAddress;

PROCEDURE Prep (ce: CallExpr.T) =
  VAR
    base      := ce.args[0];
    start     := ce.args[1];
    len       := ce.args[2];
    array     := Type.Base (Expr.TypeOf (base));
    open      := ArrayType.OpenCousin (array);
    src_depth := OpenArrayType.OpenDepth (array);
    elt_pack  := ArrayType.EltPack (array);

    index, element: Type.T;
    t_result: CG.Var;
    t_base, t_start: CG.Val;
    i_start, i_len : INTEGER;
    case: [0..7];
    n_elts, x_len, x_start, max: Target.Int;
  BEGIN
    Type.Compile (array);
    Type.Compile (open);
    EVAL ArrayType.Split (array, index, element);

    Expr.PrepLValue (base);

    (* determine which case to use *)
    case := 0;
    IF (src_depth # 0)
      THEN case := 4;
      ELSE n_elts := Type.Number (index);
    END;
    IF GetCard (start, i_start, x_start)
      THEN INC (case, 2);
      ELSE Expr.Prep (start);
    END;
    IF GetCard (len,   i_len,   x_len)
      THEN INC (case, 1);
      ELSE Expr.Prep (len);
    END;

    (* declare space for the result *)
    t_result := OpenArrayType.DeclareTemp (open);

    CASE case OF
    | 0 =>  (* fixed array, var start, var len ------------------------------*)

          (* initialize the new count *)
          Expr.Compile (len);
          CG.Store_int (t_result, M3RT.OA_size_0);

          IF NOT Host.doRangeChk THEN
            Expr.CompileAddress (base);
            Expr.Compile (start);
          ELSE
            Expr.Compile (start);         t_start := CG.Pop ();
            CG.Push (t_start);
            CG.Load_int (t_result, M3RT.OA_size_0);
            CG.Add (CG.Type.Int);
            CG.Check_hi (n_elts);
            CG.Discard (CG.Type.Int);
            Expr.CompileAddress (base);
            CG.Push (t_start);
            CG.Free (t_start);
          END;

          (* initialize the new data pointer *)
          CG.Index_bytes (elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 1 =>  (* fixed array, var start, const len ----------------------------*)

          (* initialize the new count *)
          CG.Load_integer (x_len);
          CG.Store_int (t_result, M3RT.OA_size_0);

          IF NOT Host.doRangeChk THEN
            Expr.CompileAddress (base);
            Expr.Compile (start);
          ELSIF NOT TInt.Subtract (n_elts, x_len, max) THEN
            (* cannot compute n-len at compile time *)
           Expr.Compile (start);         t_start := CG.Pop ();
            CG.Push (t_start);
            CG.Load_integer (x_len);
            CG.Add (CG.Type.Int);
            CG.Check_hi (n_elts);
           CG.Discard (CG.Type.Int);
            Expr.CompileAddress (base);
            CG.Push (t_start);
            CG.Free (t_start);
          ELSIF TInt.LT (max, TInt.Zero) THEN
            Error.Warn (2, "SUBARRAY length out of range");
            Expr.CompileAddress (base);
            Expr.Compile (start);
            CG.Check_hi (max);
          ELSE
            Expr.CompileAddress (base);
            Expr.Compile (start);
            CG.Check_hi (max);
          END;

          (* initialize the new data pointer *)
          CG.Index_bytes (elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 2 =>  (* fixed array, const start, var len ----------------------------*)

          IF NOT Host.doRangeChk THEN
            (* no check => initialize the new count *)
            Expr.Compile (len);
            CG.Store_int (t_result, M3RT.OA_size_0);
          ELSIF NOT TInt.Subtract (n_elts, x_start, max) THEN
            (* initialize the new count *)
            Expr.Compile (len);
            CG.Store_int (t_result, M3RT.OA_size_0);
            (* cannot compute n-start at compile time *)
            CG.Load_int (t_result, M3RT.OA_size_0);
            CG.Load_integer (x_start);
            CG.Add (CG.Type.Int);
            CG.Check_hi (n_elts);
            CG.Discard (CG.Type.Int);
          ELSIF TInt.LT (max, TInt.Zero) THEN
            (* initialize and check the new count *)
            Error.Warn (2, "SUBARRAY initial index out of range");
            Expr.Compile (len);
            CG.Check_hi (max);
            CG.Store_int (t_result, M3RT.OA_size_0);
          ELSE
            (* initialize and check the new count *)
            Expr.Compile (len);
            CG.Check_hi (max);
            CG.Store_int (t_result, M3RT.OA_size_0);
          END;

          (* initialize the new data pointer *)
          Expr.CompileAddress (base);
          CG.Add_offset (i_start * elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 3 =>  (* fixed array, const start, const len --------------------------*)

          (* initialize the new count *)
          CG.Load_integer (x_len);
          CG.Store_int (t_result, M3RT.OA_size_0);

          IF NOT Host.doRangeChk THEN
            (* no check *)
          ELSIF NOT TInt.Add (x_start, x_len, max) THEN
            (* cannot compute start+len at compile time *)
            CG.Load_integer (x_start);
            CG.Load_integer (x_len);
            CG.Add (CG.Type.Int);
            CG.Check_hi (n_elts);
            CG.Discard (CG.Type.Int);
          ELSIF TInt.LT (n_elts, max) THEN
            (* oops, they're too big *)
            Error.Warn (2, "SUBARRAY start+length out of range");
            CG.Load_integer (max);
            CG.Check_hi (n_elts);
            CG.Discard (CG.Type.Int);
          ELSE
            (* ok *)
          END;

          (* initialize the new data pointer *)
          Expr.CompileAddress (base);
          CG.Add_offset (i_start * elt_pack);
          CG.Store_addr (t_result, M3RT.OA_elt_ptr);

    | 4 =>  (* open array, var start, var len -------------------------------*)

          Expr.CompileAddress (base);   t_base  := CG.Pop ();
          Expr.Compile (start);         t_start := CG.Pop ();

          (* initialize the new counts *)
          Expr.Compile (len);
          CG.Store_int (t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF Host.doRangeChk THEN
            CG.Push (t_start);
            CG.Load_int (t_result, M3RT.OA_size_0);
            CG.Add (CG.Type.Int);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (CG.Type.Int);
            CG.Check_hi (TInt.Zero);
            CG.Discard (CG.Type.Int);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          CG.Push (t_start);
          ComputeOffset (t_result, src_depth, elt_pack);

          CG.Free (t_base);
          CG.Free (t_start);

    | 5 =>  (* open array, var start, const len -----------------------------*)

          Expr.CompileAddress (base);   t_base  := CG.Pop ();
          Expr.Compile (start);         t_start := CG.Pop ();

          (* initialize the new counts *)
          CG.Load_integer (x_len);
          CG.Store_int (t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF Host.doRangeChk THEN
            CG.Push (t_start);
            CG.Load_integer (x_len);
            CG.Add (CG.Type.Int);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (CG.Type.Int);
            CG.Check_hi (TInt.Zero);
            CG.Discard (CG.Type.Int);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          CG.Push (t_start);
          ComputeOffset (t_result, src_depth, elt_pack);

          CG.Free (t_base);
          CG.Free (t_start);

    | 6 =>  (* open array, const start, var len -----------------------------*)

          Expr.CompileAddress (base);   t_base  := CG.Pop ();

          (* initialize the new counts *)
          Expr.Compile (len);
          CG.Store_int (t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF Host.doRangeChk THEN
            CG.Load_int (t_result, M3RT.OA_size_0);
            CG.Load_integer (x_start);
            CG.Add (CG.Type.Int);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (CG.Type.Int);
            CG.Check_hi (TInt.Zero);
            CG.Discard (CG.Type.Int);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          IF (src_depth <= 1) THEN
            CG.Add_offset (i_start * elt_pack);
            CG.Store_addr (t_result, M3RT.OA_elt_ptr);
          ELSE
            CG.Load_integer (x_start);
            ComputeOffset (t_result, src_depth, elt_pack);
          END;

          CG.Free (t_base);

    | 7 =>  (* open array, const start, const len ---------------------------*)

          Expr.CompileAddress (base);   t_base  := CG.Pop ();

          (* initialize the new counts *)
          CG.Load_integer (x_len);
          CG.Store_int (t_result, M3RT.OA_size_0);
          CopyDopeVector (t_base, t_result, src_depth);

          IF NOT Host.doRangeChk THEN
            (* no check *)
          ELSIF TInt.Add (x_start, x_len, max) THEN
            CG.Load_integer (max);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (CG.Type.Int);
            CG.Check_hi (TInt.Zero);
            CG.Discard (CG.Type.Int);
          ELSE
            CG.Load_integer (x_start);
            CG.Load_integer (x_len);
            CG.Add (CG.Type.Int);
            CG.Push (t_base);
            CG.Open_size (0);
            CG.Subtract (CG.Type.Int);
            CG.Check_hi (TInt.Zero);
            CG.Discard (CG.Type.Int);
          END;

          (* initialize the new data pointer *)
          CG.Push (t_base);
          CG.Open_elt_ptr (ArrayType.EltAlign (array));
          IF (src_depth <= 1) THEN
            CG.Add_offset (i_start * elt_pack);
            CG.Store_addr (t_result, M3RT.OA_elt_ptr);
          ELSE
            CG.Load_integer (x_start);
            ComputeOffset (t_result, src_depth, elt_pack);
          END;

          CG.Free (t_base);

    END; (* CASE *)

    (* leave the new subarray as the result. *)
    CG.Load_addr_of_temp (t_result, 0, Target.Address.align);
    ce.tmp := CG.Pop ();

  END Prep;

PROCEDURE GetCard (e: Expr.T;  VAR i: INTEGER;  VAR x: Target.Int): BOOLEAN =
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    RETURN IntegerExpr.Split (e, x)
       AND TInt.ToInt (x, i)
       AND (0 <= i);
  END GetCard;

PROCEDURE CopyDopeVector (src: CG.Val;  dest: CG.Var;  depth: INTEGER) =
  BEGIN
    FOR i := 1 TO depth - 1 DO
      CG.Push (src);
      CG.Open_size (i);
      CG.Store_int (dest, M3RT.OA_sizes + i * Target.Integer.pack);
    END;
  END CopyDopeVector;

PROCEDURE ComputeOffset (array: CG.Var;  depth, elt_pack: INTEGER) =
  BEGIN
    FOR i := 1 TO depth - 1 DO
      CG.Load_int (array, M3RT.OA_sizes + i * Target.Integer.pack);
      CG.Multiply (CG.Type.Int);
    END;
    CG.Index_bytes (elt_pack);
    CG.Store_addr (array, M3RT.OA_elt_ptr);
  END ComputeOffset;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the real work was done by Prep *)
    CG.Push (ce.tmp);
    CG.Boost_alignment (Target.Address.align);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END Compile;

PROCEDURE IsWritable (ce: CallExpr.T): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (ce.args[0]);
  END IsWritable;

PROCEDURE IsDesignator (ce: CallExpr.T): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (ce.args[0]);
  END IsDesignator;

PROCEDURE NoteWrites (ce: CallExpr.T) =
  BEGIN
    Expr.NoteWrite (ce.args[0]);
  END NoteWrites;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (3, 3, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 NeedsAddress,
                                 Check,
                                 Prep,
                                 Compile,
                                 Prep,
                                 Compile,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue, (* fold *)
                                 IsWritable,
                                 IsDesignator,
                                 NoteWrites);
    Procedure.Define ("SUBARRAY", Z, TRUE);
  END Initialize;

BEGIN
END Subarray.
