(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallExpr.i3                                           *)
(* Last Modified On Tue Jun 20 15:17:26 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 10:59:14 PDT 1995 By ericv      *)
(*      Modified On Fri Aug  3 02:44:12 1990 By muller         *)

INTERFACE CallExpr;

IMPORT CG, Expr, Type;

PROCEDURE New (proc: Expr.T;  args: Expr.List): Expr.T;

(*******************************************************************)

TYPE
  T <: T_;
  T_ = Expr.T OBJECT
         proc   : Expr.T;
         args   : Expr.List;
         tmp    : CG.Val;         (* for use by the Prep methods *)
         align  : CG.Alignment;   (* for use by the Prep methods *)
       END;

TYPE
  MethodList <: REFANY;

TYPE
  Typer       = PROCEDURE (t: T): Type.T;
  Visitor     = PROCEDURE (t: T);
  TypeChecker = PROCEDURE (t: T;  VAR cs: Expr.CheckState);
  Evaluator   = PROCEDURE (t: T): Expr.T;
  Predicate   = PROCEDURE (t: T): BOOLEAN;
  Compiler    = PROCEDURE (t: T);
  CompilerLV  = PROCEDURE (t: T);
  CompilerBR  = PROCEDURE (t: T;  true, false: CG.Label;  freq: CG.Frequency);
  NoteWriter  = PROCEDURE (t: T);

PROCEDURE NewMethodList
  (minArgs      : INTEGER;
   maxArgs      : INTEGER;
   functional   : BOOLEAN;
   keywords     : BOOLEAN;
   strict       : BOOLEAN;
   fixedType    : Type.T;
   typeOf       : Typer;
   need_addr    : Visitor;
   checker      : TypeChecker;
   prep         : Compiler;
   compiler     : Compiler;
   prepLV       : CompilerLV;
   compilerLV   : CompilerLV;
   prepBR       : CompilerBR;
   compilerBR   : CompilerBR;
   evaluator    : Evaluator;
   isWritable   : Predicate;
   isDesignator : Predicate;
   noteWriter   : NoteWriter): MethodList;

PROCEDURE Is (e: Expr.T): BOOLEAN;
PROCEDURE IsUserProc (e: Expr.T): BOOLEAN;

PROCEDURE IsNever        (t: T): BOOLEAN;
PROCEDURE IsAlways       (t: T): BOOLEAN;
PROCEDURE NoValue        (t: T): Expr.T;
PROCEDURE NotAddressable (t: T);
PROCEDURE PrepArgs       (t: T);
PROCEDURE NoLValue       (t: T);
PROCEDURE NotBoolean     (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE PrepNoBranch   (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE NoBranch       (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE NotWritable    (t: T);

END CallExpr.
