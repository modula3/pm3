(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CaseStmt.m3                                           *)
(* Last modified on Fri Oct 21 14:37:48 PDT 1994 by kalsow     *)
(*      modified on Fri Feb 15 04:03:38 1991 by muller         *)

MODULE CaseStmt;

IMPORT CG, Expr, Stmt, StmtRep, Type, Error, Target, TInt, Host;
IMPORT EnumExpr, Token, IntegerExpr, Scanner, Word, ErrType;
FROM Scanner IMPORT Match, GetToken, Fail, cur;

TYPE
  P = Stmt.T BRANDED "CaseStmt.P" OBJECT
        expr     : Expr.T   := NIL;
        tree     : Tree     := NIL;
        bodies   : StmtList := NIL;
	complete : BOOLEAN  := FALSE;
        hasElse  : BOOLEAN  := FALSE;
        badLabels: BOOLEAN  := FALSE;
        elseBody : Stmt.T   := NIL;
        nCases   : INTEGER  := 0;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

TYPE
  Tree = REF RECORD
           less    : Tree;
           greater : Tree;
           emin    : Expr.T;
           emax    : Expr.T;
           min     : Target.Int;
           max     : Target.Int;
           body    : INTEGER;
           origin  : INTEGER;
           bad     : BOOLEAN;
         END;

TYPE
  StmtList = REF ARRAY OF Stmt.T;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;  bar: BOOLEAN;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    p.bodies := NEW (StmtList, 8);

    Match (TK.tCASE);
    p.expr := Expr.Parse ();
    Match (TK.tOF);
    bar := (cur.token = TK.tBAR);
    IF (bar) THEN GetToken () (* | *)  END;
    LOOP
      IF (cur.token = TK.tELSE) THEN EXIT END;
      IF (cur.token = TK.tEND) THEN EXIT END;
      bar := FALSE;
      ParseCase (p);
      IF (cur.token # TK.tBAR) THEN EXIT END;
      bar := TRUE; GetToken (); (* | *)
    END;
    IF (bar) THEN
      Fail ("missing case");
    END;
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
  VAR t: Tree;
  BEGIN
    LOOP
      t := NEW (Tree);
      t.origin  := Scanner.offset;
      t.less    := p.tree;   p.tree := t;
      t.greater := NIL;
      t.emin    := Expr.Parse ();
      t.emax    := NIL;
      t.body    := p.nCases;
      t.bad     := FALSE;
      IF (cur.token = TK.tDOTDOT) THEN
        GetToken (); (* .. *)
        t.emax := Expr.Parse ();
      END;
      IF (cur.token # TK.tCOMMA) THEN EXIT END;
      GetToken (); (* , *)
    END;
    Match (TK.tIMPLIES);
    IF (p.nCases > LAST (p.bodies^)) THEN ExpandBodies (p) END;
    p.bodies[p.nCases] := Stmt.Parse ();
    INC (p.nCases);
  END ParseCase;

PROCEDURE ExpandBodies (p: P) =
  VAR old, new: StmtList;
  BEGIN
    old := p.bodies;
    new := NEW (StmtList, NUMBER (old^) * 2);
    FOR i := 0 TO LAST (old^) DO new[i] := old[i] END;
    p.bodies := new;
  END ExpandBodies;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t, u, v: Tree;  type: Type.T;  min, max, minE, maxE: Target.Int;
  BEGIN
    (* check out the selector *)
    Expr.TypeCheck (p.expr, cs);
    type := Expr.TypeOf (p.expr);
    IF NOT Type.IsOrdinal (type) THEN
      Error.Msg ("invalid expression type for case selector");
      p.badLabels := TRUE;
      type := ErrType.T;
    END;

    (* reverse the tree nodes so they're in source order *)
    t := p.tree;
    u := NIL;
    WHILE (t # NIL) DO
      v := t.less;
      t.less := u;
      u := t;
      t := v;
    END;
    p.tree := u;

    (* type check the cases & build a tree *)
    EVAL Type.GetBounds (type, min, max);
    t := p.tree;
    p.tree := NIL;
    WHILE (t # NIL) DO
      Scanner.offset := t.origin;
      u := t.less;
      p.tree := AddNode (p, p.tree, t, type, min, max, cs);
      t := u;
    END;

    (* type check the bodies *)
    FOR i := 0 TO p.nCases - 1 DO Stmt.TypeCheck (p.bodies[i], cs) END;

    (* check the else clause *)
    IF (p.hasElse) THEN Stmt.TypeCheck (p.elseBody, cs) END;

    (* check for a complete tree *)
    Expr.GetBounds (p.expr, minE, maxE);
    p.complete := p.hasElse OR CompleteTree (p.tree, minE, maxE);
    IF (NOT p.complete) AND (NOT p.badLabels) THEN
      Scanner.offset := p.origin;
      Error.Warn (1, "CASE statement does not handle all possible values");
    END;
  END Check;

PROCEDURE AddNode (p: P;  old, new: Tree;  type: Type.T;
                   READONLY min, max: Target.Int;
                   VAR cs: Stmt.CheckState): Tree =
  BEGIN
    new.min := CheckLabel (p, new, new.emin, type, cs);
    IF (new.emax # NIL)
      THEN new.max := CheckLabel (p, new, new.emax, type, cs);
      ELSE new.max := new.min;
    END;

    IF (new.bad) OR (p.badLabels) THEN
      (* don't generate another error message *)
    ELSIF TInt.LT (new.min, min) OR TInt.LT (max, new.max) THEN
      Error.Msg ("case labels out of range");
      new.bad := TRUE;
    END;

    RETURN AddToTree (p, old, new);
  END AddNode;

PROCEDURE CheckLabel (p: P;  tree: Tree;  e: Expr.T;  type: Type.T;
                                         VAR cs: Stmt.CheckState): Target.Int =
  VAR t: Type.T;  i: Target.Int;  n_errs, n_xxx, n_warns: INTEGER;
  BEGIN
    Error.Count (n_errs, n_warns);
      Expr.TypeCheck (e, cs);
    Error.Count (n_xxx, n_warns);
    IF (n_xxx > n_errs) THEN tree.bad := TRUE;  RETURN TInt.Zero; END;

    t := Expr.TypeOf (e);
    IF (NOT p.badLabels) AND NOT Type.IsAssignable (type, t) THEN
      Error.Msg ("case label not compatible with selector");
      tree.bad := TRUE;
    END;

    e := Expr.ConstValue (e);
    IF (e = NIL) AND (NOT tree.bad) THEN
      Error.Msg ("case label must be constant");
      tree.bad := TRUE;
    END;
    i := TInt.Zero;
    IF IntegerExpr.Split (e, i) OR EnumExpr.Split (e, i, t) THEN END;
    RETURN i;
  END CheckLabel;

PROCEDURE AddToTree (p: P;  old, new: Tree): Tree =
  VAR z: Tree;  new_min, new_max: BOOLEAN;
  BEGIN
    new.less    := NIL;
    new.greater := NIL;
    IF (new.bad) OR (p.badLabels) THEN
      (* ignore this node *)
    ELSIF (old = NIL) THEN
      old := new;
    ELSIF TInt.LT (new.max, old.min) THEN
      old.less := AddToTree (p, old.less, new);
    ELSIF TInt.LT (old.max, new.min) THEN
      old.greater := AddToTree (p, old.greater, new);
    ELSIF (new.body # old.body) THEN
      Error.Msg ("duplicate labels in case statement");
      new.bad := TRUE;
    ELSE
      (* new and old overlap, but are in the same case arm *)
      Error.Warn (2, "repeated labels in case arm");
      new_min := TInt.LT (new.min, old.min);
      new_max := TInt.LT (old.max, new.max);
      IF new_min AND new_max THEN
        z := NEW (Tree);  z^ := new^;
        EVAL TInt.Subtract (old.min, TInt.One, z.max);
        old.less := AddToTree (p, old.less, z);
        EVAL TInt.Add (old.max, TInt.One, new.min);
        old.greater := AddToTree (p, old.greater, new);
      ELSIF new_min THEN
        EVAL TInt.Subtract (old.min, TInt.One, new.max);
        old.less := AddToTree (p, old.less, new);
      ELSIF new_max THEN
        EVAL TInt.Add (old.max, TInt.One, new.min);
        old.greater := AddToTree (p, old.greater, new);
      END;
    END;
    RETURN old;
  END AddToTree;

PROCEDURE CompleteTree (t: Tree;  min, max: Target.Int): BOOLEAN =
  VAR x, y: Target.Int;
  BEGIN
    WHILE (t # NIL) DO
      IF TInt.LT (t.max, min) OR TInt.LT (max, t.min) THEN
        RETURN TInt.LT (max, min);
      END;
      IF    TInt.Subtract (t.min, min, x)
        AND TInt.Subtract (max, t.max, y)
        AND TInt.LT (y, x) THEN
        IF TInt.Add (t.max, TInt.One, x) THEN
          IF NOT CompleteTree (t.greater, x, max) THEN RETURN FALSE END;
        END;
        IF NOT TInt.Subtract (t.min, TInt.One, max) THEN
          RETURN TRUE;
        END;
	t := t.less;
      ELSE
        IF TInt.Subtract (t.min, TInt.One, x) THEN
          IF NOT CompleteTree (t.less, min, x) THEN RETURN FALSE END;
        END;
        IF NOT TInt.Add (t.max, TInt.One, min) THEN
          RETURN TRUE;
        END;
	t := t.greater;  
      END;
    END;
    RETURN TInt.LT (max, min);
  END CompleteTree;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR minL, maxL: Target.Int;  t: Tree;  oc: Stmt.Outcomes;
      min_L, max_L: INTEGER;
  BEGIN
    (* find the smallest label *)
    minL := Target.Integer.max;
    t := p.tree;
    WHILE (t # NIL) DO
      minL := t.min;
      t := t.less;
    END;

    (* find the largest label *)
    maxL := Target.Integer.min;
    t := p.tree;
    WHILE (t # NIL) DO
      maxL := t.max;
      t := t.greater;
    END;

    (* collapse adjacent tree nodes *)
    p.tree := FlattenTree (p.tree, NIL);

    IF TInt.ToInt (minL, min_L) AND TInt.ToInt (maxL, max_L)
      AND ShouldBeIndexed (p, max_L, min_L) THEN
      (* generate an indexed table branch *)
      oc := GenIndexedBranch (p, min_L, max_L, minL, maxL);
    ELSE
      (* generate an IF-ELSE structure *)
      oc := GenIfTable (p);
 (* ELSE generate a binary search table... *)
    END;
    RETURN oc;
  END Compile;

PROCEDURE FlattenTree (t, tail: Tree): Tree =
  BEGIN
    IF (t = NIL) THEN RETURN tail END;
    t.greater := FlattenTree (t.greater, tail);
    RETURN FlattenTree (t.less, t);
  END FlattenTree;

PROCEDURE ShouldBeIndexed (p: P;  maxL, minL: INTEGER): BOOLEAN =
  VAR
    t: Tree;
    last, zz: Target.Int;
    n_tests: INTEGER;
    n_slots := Word.Minus (maxL, minL);
  BEGIN
    (* don't bother with huge tables *)
    IF (n_slots > 4096) OR (n_slots < 0) THEN RETURN FALSE END;

    (* don't bother with tiny tables *)
    (* => count the number of IF tests that would be needed *)
    n_tests := 0;
    last := Target.Integer.min;
    t := p.tree;
    WHILE (t # NIL) DO
      IF TInt.Subtract (t.min, TInt.One, zz)
        AND TInt.LT (last, zz) THEN INC (n_tests) END;
      INC (n_tests);
      last := t.max;
      t := t.greater;
    END;
    IF (n_tests < 8) THEN RETURN FALSE END;

    (* always build small tables *)
    IF (maxL - minL) <= 256 THEN RETURN TRUE END;

    (* otherwise, use a table if the density is at least 0.05 *)
    RETURN (p.nCases * 20) > (maxL - minL);
  END ShouldBeIndexed;

PROCEDURE GenIndexedBranch (p: P;  l_min, l_max: INTEGER;
                            READONLY L_min, L_max: Target.Int): Stmt.Outcomes =
  VAR
    t: Tree;
    x: CG.Val;
    b: BOOLEAN;
    oc, xc: Stmt.Outcomes;
    e_min, e_max: Target.Int;
    l_else, l_end, l_case, l_bodies: CG.Label;
    labels: REF ARRAY OF CG.Label;
    min, max, j: INTEGER;
  BEGIN
    Expr.GetBounds (p.expr, e_min, e_max);

    (* allocate the label's we need *)
    l_bodies := CG.Next_label (p.nCases);
    l_else := CG.Next_label ();
    labels := NEW (REF ARRAY OF CG.Label, l_max - l_min + 1);
    FOR i := 0 TO LAST (labels^) DO labels[i] := l_else END;
    l_end := CG.Next_label ();

    (* initialize the label table *)
    t := p.tree;
    WHILE (t # NIL) DO
      l_case := l_bodies + t.body;
      b := TInt.ToInt (t.min, min); <*ASSERT b*>
      b := TInt.ToInt (t.max, max); <*ASSERT b*>
      j := min - l_min;
      WHILE (j <= max - l_min) DO
        labels [j] := l_case;
        IF (j = max - l_min) THEN EXIT END;
        INC (j);
      END;
      t := t.greater;
    END;

    (* compute the index and translate it to a zero base *)
    Expr.Prep (p.expr);
    Expr.Compile (p.expr);
    IF (l_min # 0) THEN
      (* translate  [l_min .. l_max] => [0 .. l_max - l_min] *)
      CG.Load_intt (l_min);
      CG.Subtract (CG.Type.Int);
    END;

    (* range check the index expression *)
    IF TInt.LE (L_min, e_min) AND TInt.LE (e_max, L_max) THEN
      (* no range checking is required *)
    ELSIF TInt.LE (L_min, e_min) THEN
      (* lower bound is OK *)
      x := CG.Pop ();
      CG.Push (x);
      CG.Load_intt (l_max - l_min);
      CG.If_gt (l_else, CG.Type.Int, CG.Never);
      CG.Push (x);
      CG.Free (x);
    ELSIF TInt.LE (e_max, L_max) THEN
      (* upper bound is OK *)
      x := CG.Pop ();
      CG.Push (x);
      CG.Load_integer (TInt.Zero);
      CG.If_lt (l_else, CG.Type.Int, CG.Never);
      CG.Push (x);
      CG.Free (x);
    ELSE
      (* need to check both bounds *)
      x := CG.Pop ();
      CG.Push (x);
      CG.Load_integer (TInt.Zero);
      CG.If_lt (l_else, CG.Type.Int, CG.Never);
      CG.Push (x);
      CG.Load_intt (l_max - l_min);
      CG.If_gt (l_else, CG.Type.Int, CG.Never);
      CG.Push (x);
      CG.Free (x);
    END;

    (* generate the branch *)
    CG.Case_jump (labels^);

    (* generate the table entries *)
    oc := Stmt.Outcomes {};
    FOR i := 0 TO p.nCases - 1 DO
      CG.Set_label (l_bodies + i);
      xc := Stmt.Compile (p.bodies[i]);
      oc := oc + xc;
      IF (Stmt.Outcome.FallThrough IN xc) THEN  CG.Jump (l_end)  END;
    END;

    (* generate the else clause *)
    CG.Set_label (l_else);
    IF (p.hasElse) THEN
      oc := oc + Stmt.Compile (p.elseBody);
    ELSIF (NOT p.complete) AND (Host.doCaseChk) THEN
      CG.Case_fault ();
    END;

    CG.Set_label (l_end);
    RETURN oc;
  END GenIndexedBranch;

PROCEDURE GenIfTable (p: P): Stmt.Outcomes =
  VAR
    t: Tree;
    x: CG.Val;
    e_min, e_max: Target.Int;
    next: Target.Int;
    oc, xc: Stmt.Outcomes;
    l_bodies, l_else, l_end: INTEGER;
  BEGIN
    p.tree := CollapseTree (p.tree);
    l_bodies := CG.Next_label (p.nCases);
    l_else   := CG.Next_label ();
    l_end    := CG.Next_label ();
    oc := Stmt.Outcomes {};

    (* compile the tests & branches *)
    Expr.Prep (p.expr);
    Expr.Compile (p.expr);
    x := CG.Pop ();

    (* walk the list of labels generating the goto's *)
    Expr.GetBounds (p.expr, e_min, e_max);
    next := e_min;
    t := p.tree;
    WHILE (t # NIL) DO
      CG.Gen_location (t.origin);
      IF TInt.LT (next, t.min) THEN
        CG.Push (x);
        CG.Load_integer (t.min);
        CG.If_lt (l_else, CG.Type.Int, CG.Never);
      END;
      CG.Push (x);
      CG.Load_integer (t.max);
      CG.If_le (l_bodies + t.body, CG.Type.Int, CG.Maybe);
      IF NOT TInt.Add (t.max, TInt.One, next) THEN
        IF (t.greater # NIL) THEN Error.Msg ("case label too large") END;
        next := t.max;
      END;
      t := t.greater;
    END;
    IF TInt.LE (next, e_max) THEN  CG.Jump (l_else)  END;
    CG.Free (x);

    (* generate the bodies *)
    FOR i := 0 TO p.nCases - 1 DO
      CG.Set_label (l_bodies + i);
      xc := Stmt.Compile (p.bodies[i]);
      oc := oc + xc;
      IF (Stmt.Outcome.FallThrough IN xc) THEN  CG.Jump (l_end)  END;
    END;

    (* generate the else clause *)
    CG.Set_label (l_else);
    IF (p.hasElse) THEN
      oc := oc + Stmt.Compile (p.elseBody);
    ELSIF (NOT p.complete) AND (Host.doCaseChk) THEN
      CG.Case_fault ();
    END;

    CG.Set_label (l_end);
    RETURN oc;
  END GenIfTable;

PROCEDURE CollapseTree (t: Tree): Tree =
  VAR t1, t2: Tree;  c: INTEGER;  x, xx: Target.Int;
  BEGIN
    t1 := t;
    WHILE (t1 # NIL) DO
      c := t1.body;
      x := t1.max;
      t2 := t1.greater;
      WHILE (t2 # NIL) AND (t2.body = c)
        AND TInt.Add (x, TInt.One, xx)
        AND TInt.EQ (xx, t2.min) DO
        x := t2.max;
        t2 := t2.greater;
      END;
      t1.greater := t2;
      t1.max := x;
      t1 := t2;
    END;
    RETURN t;
  END CollapseTree;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR oc := Stmt.Outcomes {};
  BEGIN
    FOR i := 0 TO p.nCases - 1 DO
      oc := oc + Stmt.GetOutcome (p.bodies[i]);
    END;
    IF (p.hasElse) THEN
      oc := oc + Stmt.GetOutcome (p.elseBody);
    END;
    RETURN oc;
  END GetOutcome;

BEGIN
END CaseStmt.
