(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssertStmt.m3                                         *)
(* Last modified on Sat Nov 19 10:15:44 PST 1994 by kalsow     *)
(*      modified on Sat Mar 16 02:01:05 1991 by muller         *)

MODULE AssertStmt;

IMPORT CG, Expr, Token, Scanner, Stmt, StmtRep, Error;
IMPORT Host, EnumExpr, Type, Bool, Target, TInt, ErrType;

TYPE
  P = Stmt.T OBJECT
        e: Expr.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    Scanner.Match (Token.T.tASSERT);
    p.e := Expr.Parse ();
    IF (Scanner.cur.token # Token.T.tENDPRAGMA) THEN
      Scanner.Fail ("missing \'*>\'");
    END;
    Scanner.cur.token := Token.T.tSEMI;  (* for the statement parser *)
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;
  BEGIN
    Expr.TypeCheck (p.e, cs);
    t := Type.Base (Expr.TypeOf(p.e));
    IF (t # Bool.T) AND (t # ErrType.T) THEN
      Error.Msg ("ASSERT condition must be a BOOLEAN");
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR v: Expr.T;  i: Target.Int;  u: Type.T;   skip: CG.Label;
  BEGIN
    IF Host.doAsserts THEN
      i := TInt.MOne;
      v := Expr.ConstValue (p.e);
      IF (v = NIL) THEN
        skip := CG.Next_label ();
        Expr.PrepBranch (p.e, skip, CG.No_label, CG.Always);
        Expr.CompileBranch (p.e, skip, CG.No_label, CG.Always);
        CG.Assert_fault ();
        CG.Set_label (skip);
      ELSIF EnumExpr.Split (v, i, u) AND TInt.EQ (i, TInt.Zero) THEN
        (* ASSERT (FALSE) *)
        CG.Assert_fault ();
        RETURN Stmt.Outcomes {};
      ELSE <* ASSERT TInt.EQ (i, TInt.One) *>
        (* ASSERT (TRUE) *)
      END;
    END;
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

BEGIN
END AssertStmt.
