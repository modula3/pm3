(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Procedure.i3                                          *)
(* Last Modified On Tue Dec 20 14:55:57 PST 1994 By kalsow     *)
(*      Modified On Tue Oct  9 23:46:24 1990 By muller         *)

INTERFACE Procedure;

IMPORT CG, Value, Type, CallExpr, Decl;

TYPE T <: Value.T;

PROCEDURE ParseDecl (READONLY att: Decl.Attributes;
                     headerOnly: BOOLEAN := FALSE);

PROCEDURE Signature   (t: T): Type.T;
PROCEDURE HasBody     (t: T): BOOLEAN;
PROCEDURE IsNested    (t: T): BOOLEAN;
PROCEDURE StaticLevel (t: T): INTEGER;

PROCEDURE Define (name      : TEXT;
                  methods   : CallExpr.MethodList;
                  reserved  : BOOLEAN;
                  signature : Type.T := NIL
                  );

PROCEDURE CheckBody (t: T;  VAR cs: Value.CheckState);

PROCEDURE IsEqual    (a, b: Value.T): BOOLEAN;
PROCEDURE NoteExport (impl, intf: Value.T);

PROCEDURE CGName (t: T;  VAR proc: CG.Proc;
                         VAR unit: CG.Var;  VAR offset: INTEGER);
(* return the back-end address of the procedure.  If known, 'proc'
   is returned.  Otherwise, 'proc' is NIL and 'unit' and 'offset'
   are set. *)

PROCEDURE LoadStaticLink (t: T);
(* generate code to load the static link needed to call t *)

PROCEDURE CaptureResult (result: CG.Type): CG.Val;
(* generate code to capture a procedure's return result of type "result"
   in a temporary.  Return the temporary.  If the procedure doesn't
   have a result, return NIL *)

PROCEDURE StartCall (t: T);
PROCEDURE EmitCall (t: T): CG.Val;
(* generate code to start and finish a procedure call.  "EmitCall"
   returns the temporary that holds "t"s return result;  "NIL" if
   "t" doesn't have a result. *)
   
PROCEDURE Reset ();

END Procedure.
