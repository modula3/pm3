(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeCaseStmt.m3                                       *)
(* Last modified on Wed Jun 29 17:17:47 PDT 1994 by kalsow     *)
(*      modified on Thu Feb 21 23:57:16 1991 by muller         *)

MODULE TypeCaseStmt;

IMPORT M3ID, CG, Expr, Stmt, StmtRep, Type, Variable, Scope;
IMPORT Error, Token, Null, ObjectAdr, RefType, Scanner;
IMPORT Host, Reff, Target, M3RT, Tracer;
FROM Scanner IMPORT Match, MatchID, GetToken, Fail, cur;

TYPE
  P = Stmt.T OBJECT
        expr     : Expr.T;
        cases    : Case;
        complete : BOOLEAN;
        hasElse  : BOOLEAN;
        elseBody : Stmt.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

TYPE
  Case = REF RECORD
           origin : INTEGER;
           next   : Case;
           nTags  : INTEGER;
           tags   : TypeList;
           var    : Variable.T;
           scope  : Scope.T;
           stmt   : Stmt.T;
         END;

TYPE TypeList = REF ARRAY OF Type.T;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;  bar: BOOLEAN;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    p.cases    := NIL;
    p.complete := FALSE;
    p.hasElse  := FALSE;
    p.elseBody := NIL;

    Match (TK.tTYPECASE);
    p.expr := Expr.Parse ();
    Match (TK.tOF);
    bar := (cur.token = TK.tBAR);
    IF (bar) THEN GetToken ()(* | *)  END;
    LOOP
      IF (cur.token = TK.tELSE) THEN EXIT END;
      IF (cur.token = TK.tEND) THEN EXIT END;
      bar := FALSE;
      ParseCase (p);
      IF (cur.token # TK.tBAR) THEN EXIT END;
      bar := TRUE; GetToken (); (* | *)
    END;

    ReverseCases (p);
    IF (bar) THEN Fail ("missing case"); END;

    IF (cur.token = TK.tELSE) THEN
      GetToken (); (* ELSE *)
      p.hasElse := TRUE;
      p.elseBody := Stmt.Parse ();
    END;

    Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE ParseCase (p: P) =
  TYPE TK = Token.T;
  VAR c: Case;  id: M3ID.T;  trace: Tracer.T;
  BEGIN
    c := NEW (Case);
    c.origin := Scanner.offset;
    c.next   := p.cases;  p.cases := c;
    c.var    := NIL;
    c.scope  := NIL;
    c.stmt   := NIL;
    c.nTags  := 0;
    c.tags   := NEW (TypeList, 2);

    LOOP
      IF (c.nTags > LAST (c.tags^)) THEN ExpandTags (c) END;
      c.tags[c.nTags] := Type.Parse ();
      INC (c.nTags);
      IF (cur.token # TK.tCOMMA) THEN EXIT END;
      GetToken (); (* , *)
    END;

    IF (cur.token = TK.tLPAREN) THEN
      GetToken (); (* ( *)
      id := MatchID ();
      trace := Variable.ParseTrace ();
      c.var := Variable.New (id, FALSE);
      c.scope := Scope.New1 (c.var);
      Variable.BindTrace (c.var, trace);
      Variable.BindType (c.var, c.tags[0], indirect := FALSE,
                         readonly := FALSE, needs_init := FALSE,
                         open_array_ok := FALSE);
      Match (TK.tRPAREN);
      Match (TK.tIMPLIES);
      c.stmt := Stmt.Parse ();
      Scope.PopNew ();
    ELSE
      Match (TK.tIMPLIES);
      c.stmt := Stmt.Parse ();
    END;
  END ParseCase;

PROCEDURE ExpandTags (c: Case) =
  VAR new, old: TypeList;
  BEGIN
    old := c.tags;
    new := NEW (TypeList, 2 * NUMBER (old^));
    FOR i := 0 TO LAST (old^) DO new[i] := old[i] END;
    c.tags := new;
  END ExpandTags;

PROCEDURE ReverseCases (p: P) =
  VAR c1, c2, c3: Case;
  BEGIN
    c1 := p.cases;
    c3 := NIL;
    WHILE (c1 # NIL) DO
      c2 := c1.next;
      c1.next := c3;
      c3 := c1;
      c1 := c2;
    END;
    p.cases := c3;
  END ReverseCases;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;  c: Case;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    t := Type.Base (Expr.TypeOf (p.expr));

    IF (NOT Type.IsSubtype (t, Reff.T))
      AND (NOT Type.IsSubtype (t, ObjectAdr.T)) THEN
      Error.Msg ("typecase selector must be a REF or OBJECT type");
    END;

    (* check each of the cases *)
    p.complete := p.hasElse;
    c := p.cases;
    WHILE (c # NIL) DO
      IF CheckCase (c, t, cs) THEN  p.complete := TRUE  END;
      c := c.next;
    END;

    Stmt.TypeCheck (p.elseBody, cs);

    IF (NOT p.complete) THEN
      Scanner.offset := p.origin;
      Error.Warn (1, "TYPECASE statement may not handle all cases");
    END;
  END Check;

PROCEDURE CheckCase (c: Case;  exprType: Type.T;
                                           VAR cs: Stmt.CheckState): BOOLEAN =
  VAR t, u: Type.T;  complete: BOOLEAN;  zz: Scope.T;
  BEGIN
    (* check the labels *)
    complete := FALSE;
    u := c.tags[0];
    FOR i := 0 TO c.nTags - 1 DO
      t := Type.Check (c.tags[i]);
      c.tags[i] := t;
      IF (c.scope # NIL) AND (NOT Type.IsEqual (t, u, NIL)) THEN
        Scanner.offset := c.origin;
        Error.Msg ("type labels are incompatible");
      END;
      IF NOT Type.IsSubtype (t, exprType) THEN
        (***** AND (NOT Type.IsSubtype (exprType, t)) THEN******)
        Scanner.offset := c.origin;
        Error.Msg ("type label incompatible with case expression");
      END;
      complete := complete OR Type.IsSubtype (exprType, t);
    END;

    (* check the body *)
    IF (c.scope # NIL) THEN
      zz := Scope.Push (c.scope);
        Scope.TypeCheck (c.scope, cs);
        Stmt.TypeCheck (c.stmt, cs);
        Scope.WarnUnused (c.scope);
      Scope.Pop (zz);
    ELSE
      Stmt.TypeCheck (c.stmt, cs);
    END;
    RETURN complete;
  END CheckCase;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    c: Case;
    ref: CG.Var;
    tc: CG.Val;
    i: INTEGER;
    oc: Stmt.Outcomes; 
    foundForSure := FALSE;
    l_null := CG.Next_label ();
    l_base := CG.Next_label (CntCases (p.cases));
    l_else := CG.Next_label ();
    l_exit := CG.Next_label ();
    ref_type := Type.Base (Expr.TypeOf (p.expr));
  BEGIN
    (* capture the ref *)
    Type.Compile (ref_type);
    Expr.Prep (p.expr);
    Expr.Compile (p.expr);
    ref := CG.Declare_local (M3ID.NoID, Target.Address.size,
                             Target.Address.align, CG.Type.Addr,
                             Type.GlobalUID (ref_type),
                             in_memory := FALSE, up_level := FALSE,
                             f := CG.Never);
    CG.Store_addr (ref);

    (* check for NIL *)
    CG.Load_addr (ref);
    CG.Load_nil ();
    CG.If_eq (l_null, CG.Type.Addr, CG.Maybe);

    (* capture the ref's typecode *)
    CG.Load_addr (ref);
    CG.Ref_to_typecode ();
    tc := CG.Pop ();

    (* compile the tests *)
    c := p.cases;  i := 0;
    WHILE (c # NIL) DO
      foundForSure := CompileCaseTest (p, c, tc, l_base + i);
      IF foundForSure THEN
        IF (c.next # NIL) THEN UnreachableCases (c.next) END;
        c := NIL;
      ELSE
        c := c.next;
      END;
      INC (i);
    END;
    IF NOT foundForSure THEN  CG.Jump (l_else)  END;
    CG.Free (tc);

    (* compile the case bodies *)
    oc := Stmt.Outcomes {};
    CG.Set_label (l_null);
    c := p.cases;
    i := 0;
    WHILE (c # NIL) DO
      oc := oc + CompileCaseBody (c, ref, l_base + i, l_exit);
      c := c.next;
      INC (i);
    END;

    (* generate the else clause *)
    IF foundForSure THEN
      IF (p.elseBody # NIL) THEN
        Scanner.offset := p.elseBody.origin;
        Error.Warn (1, "unreachable ELSE in TYPECASE");
      END;
    ELSE
      CG.Set_label (l_else);
      IF (p.hasElse) THEN
        oc := oc + Stmt.Compile (p.elseBody);
      ELSIF (NOT p.complete) AND (Host.doTCaseChk) THEN
        CG.Typecase_fault ();
      END;
    END;

    CG.Set_label (l_exit);
    RETURN oc;
  END Compile;

PROCEDURE CntCases (c: Case): INTEGER =
  VAR n := 0;
  BEGIN
    WHILE (c # NIL) DO INC (n);  c := c.next END;
    RETURN n;
  END CntCases;

PROCEDURE CompileCaseTest (p: P;  c: Case;  tc: CG.Val;
                           label: CG.Label): BOOLEAN =
  VAR t, u: Type.T;  skip: CG.Label;
  BEGIN
    CG.Gen_location (c.origin);
    u := Expr.TypeOf (p.expr);
    FOR i := 0 TO c.nTags - 1 DO
      t := c.tags[i];
      IF Type.IsEqual (t, Null.T, NIL) THEN
        (* nothing to do; we have already generated a goto tc0 
           if the expr is NIL *)
      ELSIF Type.IsSubtype (u, t) THEN
        (* the test succedes statically! *)
        CG.Jump (label);
        RETURN TRUE;
      ELSIF RefType.Is (t) THEN
        CG.Push (tc);
        Type.LoadInfo (t, M3RT.TC_typecode);
        CG.If_eq (label, CG.Type.Int, CG.Maybe);
      ELSE
        skip := CG.Next_label ();
        CG.Push (tc);
        Type.LoadInfo (t, M3RT.TC_typecode);
        CG.If_lt (skip, CG.Type.Int, CG.Maybe);
        CG.Push (tc);
        Type.LoadInfo (t, M3RT.TC_lastSubTypeTC);
        CG.If_le (label, CG.Type.Int, CG.Maybe);
        CG.Set_label (skip);
      END;
    END;
    RETURN FALSE;
  END CompileCaseTest;

PROCEDURE CompileCaseBody (c: Case;  ref: CG.Var;
                           label, exit: CG.Label): Stmt.Outcomes =
  VAR oc: Stmt.Outcomes;  zz: Scope.T;
  BEGIN
    CG.Gen_location (c.origin);
    CG.Set_label (label);
    IF (c.scope # NIL) THEN
      zz := Scope.Push (c.scope);
        Scope.Enter (c.scope);
        Scope.InitValues (c.scope);
        Variable.LoadLValue (c.var);
        CG.Load_addr (ref);
        CG.Store_indirect (CG.Type.Addr, 0, Target.Address.size);
        Variable.ScheduleTrace (c.var);
        oc := Stmt.Compile (c.stmt);
        IF (Stmt.Outcome.FallThrough IN oc) THEN CG.Jump (exit); END;
        (* for the debugger's sake, this Jump should be inside the scope *)
        Scope.Exit (c.scope);
      Scope.Pop (zz);
    ELSE
      oc := Stmt.Compile (c.stmt);
      IF (Stmt.Outcome.FallThrough IN oc) THEN CG.Jump (exit); END;
    END;
    RETURN oc;
  END CompileCaseBody;

PROCEDURE UnreachableCases (c: Case) =
  VAR save: INTEGER;
  BEGIN
    save := Scanner.offset;
    WHILE (c # NIL) DO
      Scanner.offset := c.origin;
      Error.Warn (1, "unreachable case");
      c := c.next;
    END;
    Scanner.offset := save;
  END UnreachableCases;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR c: Case;  oc := Stmt.Outcomes {};
  BEGIN
    c := p.cases;
    WHILE (c # NIL) DO
      oc := oc + Stmt.GetOutcome (c.stmt);
      c := c.next;
    END;
    IF (p.hasElse) THEN  oc := oc + Stmt.GetOutcome (p.elseBody)  END;
    RETURN oc;
  END GetOutcome;

BEGIN
END TypeCaseStmt.
