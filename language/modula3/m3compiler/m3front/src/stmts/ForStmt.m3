(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: ForStmt.m3                                            *)
(* Last modified on Fri Feb 24 09:28:39 PST 1995
 by kalsow     *)
(*      modified on Tue Nov 27 23:52:39 1990 by muller         *)

MODULE ForStmt;

IMPORT M3ID, CG, Error, Scope, Expr, Stmt, StmtRep;
IMPORT EnumType, Type, Int, Variable, Target, TInt, ErrType;
IMPORT IntegerExpr, EnumExpr, Token, Marker, Tracer;
FROM Scanner IMPORT Match, MatchID, GetToken, cur;

TYPE
  P = Stmt.T OBJECT
        scope   : Scope.T;
        var     : Variable.T;
        from    : Expr.T;
        limit   : Expr.T;
        step    : Expr.T;
        body    : Stmt.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR id: M3ID.T;  p: P;  trace: Tracer.T;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    Match (TK.tFOR);
    id := MatchID ();
    trace := Variable.ParseTrace ();
    Match (TK.tASSIGN);
    p.from := Expr.Parse ();
    Match (TK.tTO);
    p.limit := Expr.Parse ();
    p.step := NIL;
    IF (cur.token = TK.tBY) THEN
      GetToken (); (* BY *)
      p.step := Expr.Parse ();
    ELSE
      p.step := IntegerExpr.New (TInt.One);
    END;
    p.var := Variable.New (id, TRUE);
    p.scope := Scope.New1 (p.var);
    Variable.BindTrace (p.var, trace);
    Match (TK.tDO);
    p.body := Stmt.Parse ();
    Match (TK.tEND);
    Scope.PopNew ();
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR
    tFrom, tTo, tStep  : Type.T;
    minFrom,  maxFrom  : Target.Int;
    minLimit, maxLimit : Target.Int;
    minStep,  maxStep  : Target.Int;
    newMin,   newMax   : Target.Int;
    zz                 : Scope.T;
    errored            : BOOLEAN := FALSE;
  BEGIN
    Expr.TypeCheck (p.from, cs);
    Expr.TypeCheck (p.limit, cs);
    Expr.TypeCheck (p.step, cs);
    tFrom := Type.Base (Expr.TypeOf (p.from));
    tTo   := Type.Base (Expr.TypeOf (p.limit));
    tStep := Expr.TypeOf (p.step);

    IF (tFrom = ErrType.T) OR (tTo = ErrType.T) THEN
      (* already an error... *)
      tFrom := ErrType.T;
      tTo := ErrType.T;
      errored := TRUE;
    ELSIF EnumType.Is (tFrom) THEN
      IF NOT Type.IsEqual (tFrom, tTo, NIL) THEN
        Error.Msg ("\'from\' and \'to\' expressions are incompatible");
        errored := TRUE;
      END;
    ELSIF (tFrom # Int.T) OR (tTo # Int.T) THEN
      Error.Msg("\'from\' and \'to\' expressions must be compatible ordinals");
      errored := TRUE;
    END;
    IF  NOT Type.IsSubtype (tStep, Int.T) THEN
      Error.Msg ("\'by\' expression must be an integer");
      errored := TRUE;
    END;

    (* set the type of the control variable *)
    Variable.BindType (p.var, tFrom, indirect := FALSE, readonly := TRUE,
                       needs_init := FALSE,  open_array_ok := FALSE);

    (* determine the ranges of the control values *)
    IF Reduce (p.step, minStep)
      THEN maxStep := minStep;
      ELSE Expr.GetBounds (p.step, minStep, maxStep);
    END;
    IF Reduce (p.from, minFrom)
      THEN maxFrom := minFrom;
      ELSE Expr.GetBounds (p.from, minFrom, maxFrom);
    END;
    IF Reduce (p.limit, minLimit)
      THEN maxLimit := minLimit;
      ELSE Expr.GetBounds (p.limit, minLimit, maxLimit);
    END;

    IF TInt.EQ (minStep, TInt.Zero) AND TInt.EQ (maxStep, TInt.Zero) THEN
      (* warning suggested by Ernst A. Heinz <heinze@ira.uka.de>
         to catch typos. (March 19, 1995) *)
      Error.Warn (1, "zero \'by\' value in FOR loop");
      errored := TRUE;
    END;

    (* try to tighten up the range of the new index variable *)
    IF TInt.LE (TInt.Zero, minStep) THEN
      (* we're counting up! *)
      newMin := minFrom;
      newMax := maxLimit;
    ELSIF TInt.LT (maxStep, TInt.Zero) THEN
      (* we're counting down *)
      newMin := minLimit;
      newMax := maxFrom;
    ELSE
      (* we might be counting in either direction... *)
      IF TInt.LT (minFrom, minLimit)
        THEN newMin := minFrom;
        ELSE newMin := minLimit;
      END;
      IF TInt.LT (maxFrom, maxLimit)
        THEN newMax := maxLimit;
        ELSE newMax := maxFrom;
      END;
    END;
    Variable.SetBounds (p.var, newMin, newMax);

    IF NOT errored AND TInt.LT (newMax, newMin) THEN
      Error.Warn (1, "FOR loop body is unreachable (empty range)");
    END;

    zz := Scope.Push (p.scope);
      Scope.TypeCheck (p.scope, cs);
      Marker.PushExit (CG.No_label);
      Stmt.TypeCheck (p.body, cs);
      Marker.Pop ();
    Scope.Pop (zz);
  END Check;

PROCEDURE Reduce (VAR expr: Expr.T;  VAR i: Target.Int): BOOLEAN =
  VAR e: Expr.T;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (expr);
    IF (e = NIL) THEN RETURN FALSE END;
    expr := e;
    RETURN IntegerExpr.Split (e, i) OR EnumExpr.Split (e, i, t);
  END Reduce;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    step, limit, from: Expr.T;
    step_val, limit_val, from_val: Target.Int;
    step_min, step_max: Target.Int;
    t: Type.T;
    oc: Stmt.Outcomes;
    zz: Scope.T;
    index, to, by: CG.Var;
    t_index, t_to, t_by: CG.Val;
    l_top, l_test, l_less, l_exit: CG.Label;
    type: Type.T;
    indirect, readonly, index_copy: BOOLEAN;
    info: Type.Info;
    offset: INTEGER;
  BEGIN
    Variable.Split (p.var, type, indirect, readonly);

    from := Expr.ConstValue (p.from);
    IF (from = NIL) THEN
      Expr.Prep (p.from);
      Expr.Compile (p.from);
      t_index := CG.Pop_temp ();
    ELSE
      (* lower bound is a constant *)
      from_val := TInt.Zero;
      EVAL IntegerExpr.Split (from, from_val)
        OR EnumExpr.Split (from, from_val, t);
    END;

    limit := Expr.ConstValue (p.limit);
    IF (limit = NIL) THEN
      Expr.Prep (p.limit);
      Expr.Compile (p.limit);
      t_to := CG.Pop_temp ();
    ELSE (* upper bound is a constant *)
      limit_val := TInt.Zero;
      EVAL IntegerExpr.Split (limit, limit_val)
        OR EnumExpr.Split (limit, limit_val, t);
    END;

    step := Expr.ConstValue (p.step);
    IF (step = NIL) THEN
      (* non-constant step value *)
      Expr.Prep (p.step);
      Expr.Compile (p.step);
      t_by := CG.Pop_temp ();
      Expr.GetBounds (p.step, step_min, step_max);
    ELSE (* step is a constant *)
      step_val := TInt.Zero;
      EVAL IntegerExpr.Split (step, step_val)
        OR EnumExpr.Split (step, step_val, t);
    END;

    l_top  := CG.Next_label (3);
    l_test := l_top + 1;
    l_exit := l_top + 2;

    zz := Scope.Push (p.scope);
      Scope.Enter (p.scope);
      Scope.InitValues (p.scope);

      IF Type.IsEqual (type, Int.T, NIL) THEN
        (* use the user's variable *)
        index_copy := FALSE;
        Variable.LocalCGName (p.var, index, offset);
        <*ASSERT offset = 0*>
      ELSE
        (* declare a fresh local variable for the index *)
        (* 'cause small variables may overflow at the end of their ranges *)
        index_copy := TRUE;
        index := CG.Declare_local (M3ID.NoID, Target.Integer.size,
                                 Target.Integer.align, Target.Integer.cg_type,
                                 Type.GlobalUID (Int.T), in_memory := FALSE,
                                 up_level := FALSE, f := CG.Always);
      END;

      IF (from = NIL) THEN
        CG.Push (t_index);
        CG.Store_int (index);
        CG.Free (t_index);
      ELSE
        CG.Load_integer (from_val);
        CG.Store_int (index);
      END;

      IF (limit = NIL) THEN
        (* declare the local variable *)
        to := CG.Declare_local (M3ID.NoID, Target.Integer.size,
                                Target.Integer.align, Target.Integer.cg_type,
                                Type.GlobalUID (Int.T), in_memory := FALSE,
                                up_level := FALSE, f := CG.Maybe);
        CG.Push (t_to);
        CG.Store_int (to);
        CG.Free (t_to);
      END;

      IF (step = NIL) THEN
        (* declare the local variable *)
        by := CG.Declare_local (M3ID.NoID, Target.Integer.size,
                                Target.Integer.align, Target.Integer.cg_type,
                                Type.GlobalUID (Int.T), in_memory := FALSE,
                                up_level := FALSE, f := CG.Maybe);
        CG.Push (t_by);
        CG.Store_int (by);
        CG.Free (t_by);
      END;

      IF (from = NIL) OR (limit = NIL) OR (step = NIL) THEN
        (* we don't know all three values... *)
        CG.Jump (l_test);
      ELSIF TInt.LE (TInt.Zero, step_val)
        AND TInt.LE (from_val, limit_val) THEN
        (* we know we'll execute the loop at least once. *)
      ELSIF TInt.LE (step_val, TInt.Zero)
        AND TInt.LE (limit_val, from_val) THEN
        (* we know we'll execute the loop at least once. *)
      ELSE
        (* we won't execute the loop... *)
        CG.Jump (l_test);
      END;
      CG.Set_label (l_top);

      Marker.PushExit (l_exit);

      IF (index_copy) THEN
        (* make the user's variable equal to the counter *)
        EVAL Type.CheckInfo (type, info);
        Variable.LoadLValue (p.var);
        CG.Load_int (index);
        CG.Store_indirect (info.stk_type, 0, info.size);
      END;
      Variable.ScheduleTrace (p.var);

      oc := Stmt.Compile (p.body);

      (* increment the counter *)
      CG.Gen_location (p.origin);
      CG.Load_int (index);
      IF (step # NIL)
        THEN CG.Load_integer (step_val);
        ELSE CG.Load_int (by);
      END;
      CG.Add (Target.Integer.cg_type);
      CG.Store_int (index);

      (* generate the loop test *)
      CG.Gen_location (p.origin);
      CG.Set_label (l_test);
      IF (step # NIL) THEN (* constant step value *)
        CG.Load_int (index);
        IF (limit # NIL)
          THEN CG.Load_integer (limit_val);
          ELSE CG.Load_int (to);
        END;
        IF TInt.LE (TInt.Zero, step_val)
          THEN CG.If_compare (Target.Integer.cg_type, CG.Cmp.LE, l_top, CG.Likely);
          ELSE CG.If_compare (Target.Integer.cg_type, CG.Cmp.GE, l_top, CG.Likely);
        END;
      ELSIF TInt.LE (TInt.Zero, step_min) THEN
        (* positive, variable step value *)
        CG.Load_int (index);
        IF (limit # NIL)
          THEN CG.Load_integer (limit_val);
          ELSE CG.Load_int (to);
        END;
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.LE, l_top, CG.Likely);
      ELSIF TInt.LT (step_max, TInt.Zero) THEN
        (* negative, variable step value *)
        CG.Load_int (index);
        IF (limit # NIL)
          THEN CG.Load_integer (limit_val);
          ELSE CG.Load_int (to);
        END;
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.GE, l_top, CG.Likely);
      ELSE (* variable step value *)
        l_less := CG.Next_label (2);
        CG.Load_int (by);
        CG.Load_integer (TInt.Zero);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.LT, l_less, CG.Likely);
        CG.Load_int (index);
        IF (limit # NIL)
          THEN CG.Load_integer (limit_val);
          ELSE CG.Load_int (to);
        END;
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.LE, l_top, CG.Likely);
        CG.Jump (l_less+1);
        CG.Set_label (l_less);
        CG.Load_int (index);
        IF (limit # NIL)
          THEN CG.Load_integer (limit_val);
          ELSE CG.Load_int (to);
        END;
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.GE, l_top, CG.Likely);
        CG.Set_label (l_less+1);
      END;

      Marker.Pop ();
      CG.Set_label (l_exit);

      Scope.Exit (p.scope);
    Scope.Pop (zz);

    (* a FOR can fall through if its index range may be empty *)
    (* or if its body can fall through or exit.  -- Ernst Heinz *)
    IF (Stmt.Outcome.Exits IN oc)
      OR (from = NIL) OR (limit = NIL) OR (step = NIL) OR
       (NOT TInt.LT (step_val, TInt.Zero) AND TInt.LT (limit_val, from_val)) OR
       (    TInt.LT (step_val, TInt.Zero) AND TInt.LT (from_val, limit_val))
      THEN oc := oc + Stmt.Outcomes {Stmt.Outcome.FallThrough};
    END;

    RETURN oc - Stmt.Outcomes {Stmt.Outcome.Exits};
  END Compile;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.GetOutcome (p.body)
            - Stmt.Outcomes {Stmt.Outcome.Exits}
            + Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

BEGIN
END ForStmt.

