(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WithStmt.m3                                           *)
(* Last modified on Tue Jun 20 09:28:07 PDT 1995 by kalsow     *)
(*      modified on Fri Jun 16 12:48:09 PDT 1995 by ericv      *)
(*      modified on Tue Jun 26 08:01:23 1990 by muller         *)

MODULE WithStmt;

IMPORT M3ID, CG, Expr, Scope, Value, Variable, OpenArrayType;
IMPORT Type, Stmt, StmtRep, Token, M3RT, Target, Tracer, AssignStmt;
FROM Scanner IMPORT Match, MatchID, GetToken, cur;

TYPE
  Kind = {designator, openarray, structure, other};

  P = Stmt.T OBJECT
        var     : Variable.T;
        expr    : Expr.T;
        scope   : Scope.T;
        body    : Stmt.T;
	kind    : Kind;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  BEGIN
    Match (TK.tWITH);
    RETURN ParseTail ();
  END Parse;

PROCEDURE ParseTail (): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;  id: M3ID.T;  trace: Tracer.T;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    id := MatchID ();
    trace := Variable.ParseTrace ();
    p.var := Variable.New (id, FALSE);
    Match (TK.tEQUAL);
    p.expr := Expr.Parse ();
    p.scope := Scope.New1 (p.var);
    Variable.BindTrace (p.var, trace);
    IF (cur.token = TK.tCOMMA) THEN
      GetToken (); (* , *)
      p.body := ParseTail ();
    ELSE
      Match (TK.tDO);
      p.body := Stmt.Parse ();
      Match (TK.tEND);
    END;
    Scope.PopNew ();
    RETURN p;
  END ParseTail;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;  zz: Scope.T;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    t := Expr.TypeOf (p.expr);

    IF OpenArrayType.Is (t) THEN
      p.kind := Kind.openarray;
      Variable.NeedsAddress (p.var);
    ELSIF Expr.IsDesignator (p.expr) THEN
      p.kind := Kind.designator;
      Expr.NeedsAddress (p.expr);
    ELSIF Type.IsStructured (t) THEN
      p.kind := Kind.structure;
      Variable.NeedsAddress (p.var);
      AssignStmt.Check (t, p.expr, cs);
    ELSE
      p.kind := Kind.other;
    END;
      
    Variable.BindType (p.var, t, indirect := (p.kind = Kind.designator),
                       readonly := NOT Expr.IsWritable (p.expr),
                       open_array_ok := TRUE,  needs_init := FALSE);

    Scope.TypeCheck (p.scope, cs);
    zz := Scope.Push (p.scope);
      Stmt.TypeCheck (p.body, cs);
    Scope.Pop (zz);
    Scope.WarnUnused (p.scope);
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    oc: Stmt.Outcomes;
    t: Type.T;
    zz: Scope.T;
    info: Type.Info;
    val: CG.Val;
    dope_size: INTEGER;
  BEGIN
    t := Type.CheckInfo (Value.TypeOf (p.var), info);

    (* evaluate the expr outside the new scope and capture its value *)
    CASE p.kind OF
    | Kind.designator =>
        Expr.PrepLValue (p.expr);
        Expr.CompileAddress (p.expr);
        val := CG.Pop ();
    | Kind.structure =>
        AssignStmt.PrepForEmit (Value.TypeOf (p.var), p.expr,
                                initializing := TRUE);
    | Kind.openarray, Kind.other =>
        Expr.Prep (p.expr);
        Expr.Compile (p.expr);
        val := CG.Pop ();
    END;

    (* open the new scope *)
    zz := Scope.Push (p.scope);
      Scope.Enter (p.scope);

      (* initialize the variable *)
      CASE p.kind OF
      | Kind.designator =>
          CG.Push (val);
          Variable.SetLValue (p.var);
          CG.Free (val);
      | Kind.openarray =>
          dope_size := OpenArrayType.OpenDepth(t) * Target.Integer.pack;
          INC (dope_size, M3RT.OA_sizes);
          Variable.LoadLValue (p.var);
          CG.Push (val);
          CG.Copy (dope_size, overlap := FALSE);
          CG.Free (val);
      | Kind.structure =>
          Variable.LoadLValue (p.var);
          AssignStmt.DoEmit (Value.TypeOf (p.var), p.expr);
      | Kind.other =>
          Variable.LoadLValue (p.var);
          CG.Push (val);
          CG.Store_indirect (info.stk_type, 0, info.size);
          CG.Free (val);
      END;
      Variable.ScheduleTrace (p.var);

      oc := Stmt.Compile (p.body);
      Scope.Exit (p.scope);
    Scope.Pop (zz);
    RETURN oc;
  END Compile;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.GetOutcome (p.body);
  END GetOutcome;

BEGIN
END WithStmt.
