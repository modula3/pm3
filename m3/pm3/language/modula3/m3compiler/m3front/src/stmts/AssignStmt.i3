(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssignStmt.i3                                         *)
(* Last Modified On Tue Jun 20 15:23:26 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 11:26:34 PDT 1995 By ericv      *)
(*      Modified On Tue Mar 20 01:30:09 1990 By muller         *)

INTERFACE AssignStmt;

IMPORT Expr, Stmt, Type;

PROCEDURE Parse (): Stmt.T;

PROCEDURE Check (tlhs: Type.T;  rhs: Expr.T;  VAR cs: Stmt.CheckState);
(* check that rhs is assignable to a variable of type tlhs. *)

PROCEDURE PrepForEmit (tlhs: Type.T;  rhs: Expr.T;  initializing: BOOLEAN);
(* An alternative to calling Expr.Prep(rhs) before calling Emit() below,
   that tries to avoid unnecessary structure copying on assignments
   by passing the final destination to operations that can assign
   the result directly.  "initializing" is TRUE if the lhs is
   uninitialized storage (i.e. contains no user data). *)

PROCEDURE Emit (tlhs: Type.T;  rhs: Expr.T);
(* emit code to assign  (s0.A).tlhs := rhs.
   Note that Emit assumes that TypeOf(rhs) is assignable to tlhs
   and that Expr.Prep(rhs) or preferably PrepRHS(rhs) has been called. *)

PROCEDURE EmitCheck (tlhs: Type.T;  rhs: Expr.T);
(* emit code to evaluate "rhs" and generate whatever
   runtime checks would be needed if it were assigned to
   a value of type 'tlhs'.  The new value is left on the stack.
   Note that Emit assumes that TypeOf(rhs) is assignable to tlhs
   and that Expr.Prep(rhs) has been called.  'tlhs' may not be
   an open array type.  *)

END AssignStmt.
