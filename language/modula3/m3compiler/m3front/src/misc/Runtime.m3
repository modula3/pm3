(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Runtime.m3                                            *)
(* Last Modified On Tue Jun 20 09:16:29 PDT 1995 By kalsow     *)
(*      Modified On Wed Jun 14 08:25:39 PDT 1995 By ericv      *)

MODULE Runtime;

IMPORT M3, M3ID, Value, ValueRep, Scope, Module, Error, Procedure, CG;

TYPE
  ActualHook = [0..14];
CONST
  HookNames = ARRAY ActualHook OF TEXT {
    "Raise",
    "ResumeRaise",
    "PushEFrame",
    "PopEFrame",
    "LockMutex",
    "UnlockMutex",
    "Concat",
    "Allocate",
    "AllocateOpenArray",
    "AllocateUntracedObj",
    "AllocateUntracedRef",
    "AllocateUntracedOpenArray",
    "DisposeUntracedRef",
    "DisposeUntracedObj",
    "ReportFault"
  };

CONST
  HookMap = ARRAY Hook OF ActualHook { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 14, 14, 14, 14, 14,
                                       14, 14, 14 };
                           
VAR
  hooks       : Module.T := NIL;
  hooks_name  : M3ID.T   := M3ID.NoID;
  hooks_alias : M3ID.T   := M3ID.NoID;
  hook_procs  : ARRAY ActualHook OF Procedure.T;

(*---------------------------------------------------------------------------*)

PROCEDURE Reset () =
  BEGIN
    hooks := NIL;
  END Reset;

PROCEDURE Import () =
  BEGIN
    IF (hooks # NIL) THEN RETURN END;
    hooks_name  := M3ID.Add ("RTHooks");
    hooks_alias := M3ID.Add ("__RTHooks__");
    hooks := Module.LookUp (hooks_name, internal := TRUE);
    FOR h := FIRST (hook_procs) TO LAST (hook_procs) DO
      hook_procs[h] := NIL;
    END;
  END Import;

PROCEDURE Bind (dest: Module.T;  VAR runtime: Module.T;  VAR id: M3ID.T) =
  BEGIN
    IF (dest.name = hooks_name) AND Module.IsInterface ()
      THEN runtime := NIL;    id := M3ID.NoID;
      ELSE runtime := hooks;  id := hooks_alias;
    END;
  END Bind;

PROCEDURE LookUpProc (h: Hook): Procedure.T =
  VAR hh := HookMap[h];  p := hook_procs [hh];
  BEGIN
    <*ASSERT hooks # NIL*>
    IF (p = NIL) THEN
      p := LookUpNewProc (M3ID.Add (HookNames [hh]));
      hook_procs [hh] := p;
    END;
    RETURN p;
  END LookUpProc;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE LookUpNewProc (name: M3ID.T): Procedure.T =
  VAR
    v := LookUp (name);
    c := Value.ClassOf (v);
    proc   : CG.Proc;
    unit   : CG.Var;
    offset : INTEGER;
  BEGIN
    IF (c # Value.Class.Procedure) THEN RETURN NIL END;
    Value.Declare (v);  (* force a version stamp *)
    v := Value.Base (v);
    Procedure.CGName (v, proc, unit, offset);
    IF (proc # NIL) THEN
      CG.Set_runtime_proc (v.name, proc);
    ELSIF (unit # NIL) THEN
      CG.Set_runtime_hook (v.name, unit, offset);
    END;
    RETURN v;
  END LookUpNewProc;

PROCEDURE LookUp (name: M3ID.T): Value.T =
  VAR syms: Scope.T;  v, v2: Value.T;
  BEGIN
    IF (hooks = NIL) THEN RETURN NIL END;
    syms := Module.ExportScope (hooks);
    IF (syms # NIL)
      THEN v := Scope.LookUp (syms, name, TRUE);
      ELSE v := NIL; (* probably a circular import! *)
    END;
    IF (v = NIL) THEN
      Error.QID (M3.QID {module := hooks_name, item := name},
                  "undefined runtime symbol !!")
    END;

    (* If possible, use the local explicit declaration... *)
    syms := Scope.Top ();
    v2 := Scope.LookUp (syms, name, strict := FALSE);
    IF (v2 # NIL) AND Procedure.IsEqual (v2, v) THEN
      v := v2;
    END;

    RETURN v;
  END LookUp;

BEGIN
END Runtime.

(*******************************************
PROCEDURE LookUpType (name: TEXT): Type.T =
  VAR
    n := M3ID.Add (name);
    v := LookUp (n);
    c := Value.ClassOf (v);
    t : Type.T;
    info: Type.Info;
  BEGIN
    IF (c # Value.Class.Type) THEN RETURN NIL END;
    t := Value.ToType (v);
    t := Type.Check (t, info);
    RETURN t;
  END LookUpType;
**********************************************)

(*******************************************
PROCEDURE GenHooks () =
  VAR
    syms: Scope.T;
    v: Value.T;
    proc: CG.Proc;
    unit: CG.Var;
    offset: INTEGER;
    c: Value.Class;
  BEGIN
    CG.Comment (-1, "runtime routines");
    IF (hooks = NIL) THEN RETURN END;
    syms := Module.ExportScope (hooks);
    IF (syms = NIL) THEN RETURN END; (* probably a circular import! *)
    v := Scope.ToList (syms);
    WHILE (v # NIL) DO
      c := Value.ClassOf (v);
      IF (c = Value.Class.Procedure) THEN
        Procedure.CGName (v, proc, unit, offset);
        IF (proc # NIL) THEN
          CG.Set_runtime_proc (v.name, proc);
        ELSIF (unit # NIL) THEN
          CG.Set_runtime_hook (v.name, unit, offset);
        END;
      ELSIF (c = Value.Class.Var) THEN
        Variable.CGName (v, unit, offset);
        IF (unit # NIL) THEN
          CG.Set_runtime_hook (v.name, unit, offset);
        END;
      END;
      v := v.next;
    END;
  END GenHooks;
**********************************************)
