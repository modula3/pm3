(* From File: MxGen.m3 *)
(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE MxGenCG;

IMPORT Stdio;
IMPORT Mx, MxRep, MxMap, M3ID, Target;
IMPORT M3CG, M3CG_Ops, TInt, TargetMap;

FROM MxGenRep IMPORT BinderSuffix, UnitInfo, Common, Err;

REVEAL
  T = Common BRANDED "MxGenCG.T" OBJECT
    (* M3CG output information *)
    cg            : M3CG.T       := NIL;
  OVERRIDES
    generateMain := GenerateMain;
  END;

TYPE
  State = T;

(*------------------------------------------------------------------------*)

PROCEDURE New(base: Mx.LinkSet;  cg: M3CG.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN): T=
  VAR
    t := NEW(T, 
             base    := base,
             cg      := cg,
             verbose := verbose,
             errors  := Stdio.stdout,
             gui     := windowsGUI);
  BEGIN
    RETURN t;
  END New;

PROCEDURE GenerateMain (s: State)  =
  BEGIN
    GenTypeDecls (s);
    ImportMain (s);
    GenerateEntry (s);
  END GenerateMain;

(*------------------------------------------------------------------------*)

PROCEDURE GenTypeDecls (VAR s: State) =
  VAR
    interface    : M3CG.Var;
    struct_align := MAX (Target.Structure_size_boundary, Target.Address.align)
                       DIV Target.Byte;   (* == min structure alignment (bytes)*)
  BEGIN
    s.cg.begin_unit ();
    s.cg.set_source_file ("_m3main.mc");
    s.cg.set_source_line (1);

    (* we only need to declare a global segment so the gcc-based backend
       can pick up the unit name, "_m3main".   But then, the native x86
       backend requires that it be explicitly initialized... *)
    interface := s.cg.declare_segment (M3ID.Add ("MM__m3main"), 0, FALSE);
    s.cg.bind_segment (interface, Target.Address.bytes, struct_align,
                       Target.CGType.Struct, FALSE, TRUE);
    s.cg.begin_init (interface);
    s.cg.end_init (interface);
  END GenTypeDecls;

(*------------------------------------------------------------------------*)

PROCEDURE ImportMain (VAR s: State) =
  VAR
    main  := M3ID.Add ("Main");
    units := MxMap.GetData (s.base.modules);
    u     : Mx.Unit;
  BEGIN
    s.main_units := NIL;

    (* find the modules exporting "Main" *)
    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      IF (u # NIL) THEN
        FOR i := u.exported_units.start
              TO u.exported_units.start + u.exported_units.cnt - 1 DO
          IF (u.info[i] = main) THEN
            s.main_units := NEW (UnitInfo, unit := u, next := s.main_units);
            ImportUnit (s, s.main_units);
            EXIT;
          END;
        END;
      END;
    END;

    IF s.main_units = NIL THEN
      Err (s, "No module implements \"Main\".", Target.EOL);
    END;
  END ImportMain;

PROCEDURE ImportUnit (VAR s: State;  ui: UnitInfo) =
  VAR u := ui.unit;
  BEGIN
    ui.binder := M3ID.ToText (u.name) & BinderSuffix [u.interface];
    ui.cg_proc := s.cg.import_procedure (M3ID.Add (ui.binder), 1,
                                         Target.CGType.Addr,
                                         Target.DefaultCall);
    EVAL DeclareParam (s, "mode",  Target.Integer.cg_type);
  END ImportUnit;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateEntry (VAR s: State) =
  VAR
    main      : M3CG.Proc;
    run_proc  : M3CG.Proc;
    link_proc : M3CG.Proc;
    exit_proc : M3CG.Proc;
    getenv    : M3CG.Proc;
    winapi    : Target.CallingConvention;
    argv      : M3CG.Var;
    argc      : M3CG.Var;
    envp      : M3CG.Var;
    self      : M3CG.Var;
    prev      : M3CG.Var;
    mode      : M3CG.Var;
    src_line  : INTEGER := 2;
    ui        : UnitInfo;
    int_t     := Target.Integer.cg_type;
    addr_t    := Target.CGType.Addr;
  BEGIN
    run_proc := s.cg.import_procedure (M3ID.Add ("RTLinker__InitRuntime"), 4,
                                       Target.CGType.Void, Target.DefaultCall);
    EVAL DeclareParam (s, "argc", int_t);
    EVAL DeclareParam (s, "argv", addr_t);
    EVAL DeclareParam (s, "envp", addr_t);
    EVAL DeclareParam (s, "instance", addr_t);

    link_proc := s.cg.import_procedure (M3ID.Add ("RTLinker__AddUnit"), 1,
                                       Target.CGType.Void, Target.DefaultCall);
    EVAL DeclareParam (s, "m", addr_t);

    exit_proc := s.cg.import_procedure (M3ID.Add ("RTProcess__Exit"), 1,
                                       Target.CGType.Void, Target.DefaultCall);
    EVAL DeclareParam (s, "n", int_t);

    IF (s.gui) THEN
      (*
         #include <windows.h>
         extern LPSTR WINAPI GetEnvironmentStringsA ();
      *)
      winapi := Target.FindConvention ("WINAPI");
      getenv := s.cg.import_procedure (M3ID.Add ("GetEnvironmentStringsA"), 0, 
                                       addr_t, winapi);

      (* int WINAPI WinMain(HINSTANCE self, HINSTANCE prev, LPSTR args, int mode) *)
      main := s.cg.declare_procedure (M3ID.Add ("WinMain"), (*n_params*) 4,
                                 int_t, (*lev*) 0, winapi, TRUE, NIL);
      self := DeclareParam (s, "self", addr_t);
      prev := DeclareParam (s, "prev", addr_t);
      argv := DeclareParam (s, "args", addr_t);
      mode := DeclareParam (s, "mode", int_t);
      s.cg.begin_procedure(main);

      (* int argc;   void *envp;  *)
      argc := DeclareLocal (s, "argc", int_t);
      envp := DeclareLocal (s, "envp", addr_t);

      (* argc = -1; *)
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.load_integer (int_t, TInt.MOne);
      s.cg.store (argc, 0, int_t, int_t);

      (* envp = (_ADDRESS)GetEnvironmentStringsA(); *)
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.start_call_direct (getenv, 0, addr_t);
      s.cg.call_direct (getenv, addr_t);
      s.cg.store (envp, 0, addr_t, addr_t);

    ELSE (* not GUI *)
      (*
        int main (int argc, char **argv, char **envp)
      *)
      main := s.cg.declare_procedure (M3ID.Add("main"), (*n_params*) 3,
                                      int_t, (*lev*) 0,
                                      Target.DefaultCall, TRUE, NIL);
      argc := DeclareParam (s, "argc", int_t);
      argv := DeclareParam (s, "argv", addr_t);
      envp := DeclareParam (s, "envp", addr_t);
      s.cg.begin_procedure(main);

      (* void * self; *)
      self := DeclareLocal (s, "self", addr_t);
      
      (* self = 0; *)
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.load_integer (int_t, TInt.Zero);
      s.cg.store (self, 0, int_t, int_t);
    END; (* if GUI *)

    (* RTLinker__InitRuntime (argc, argv, envp, self); *)
    s.cg.set_source_line (src_line);  INC (src_line);
    s.cg.start_call_direct (run_proc, 0, Target.CGType.Void);
    IF Target.DefaultCall.args_left_to_right THEN
      s.cg.load (argc, 0, int_t, int_t);        (* argc *)
      s.cg.pop_param (int_t);
      s.cg.load (argv, 0, addr_t, addr_t);      (* argv *)
      s.cg.pop_param (addr_t);
      s.cg.load (envp, 0, addr_t, addr_t);      (* envp *)
      s.cg.pop_param (addr_t);
      s.cg.load (self, 0, addr_t, addr_t);      (* self *)
      s.cg.pop_param (addr_t);
    ELSE
      s.cg.load (self, 0, addr_t, addr_t);      (* self *)
      s.cg.pop_param (addr_t);
      s.cg.load (envp, 0, addr_t, addr_t);      (* envp *)
      s.cg.pop_param (addr_t);
      s.cg.load (argv, 0, addr_t, addr_t);      (* argv *)
      s.cg.pop_param (addr_t);
      s.cg.load (argc, 0, int_t,  int_t);       (* argc *)
      s.cg.pop_param (int_t);
    END;
    s.cg.call_direct (run_proc, Target.CGType.Void);

    ui := s.main_units;
    WHILE (ui # NIL) DO
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.start_call_direct (link_proc, 0, Target.CGType.Void);
      s.cg.load_procedure (ui.cg_proc);
      s.cg.pop_param (addr_t);
      s.cg.call_direct (link_proc, Target.CGType.Void);
      ui := ui.next;
    END;

    (*  RTProcess.Exit (0); *)
    s.cg.set_source_line (src_line);  INC (src_line);
    s.cg.start_call_direct (exit_proc, 0, Target.CGType.Void);
    s.cg.load_integer (int_t, TInt.Zero);
    s.cg.pop_param (int_t);
    s.cg.call_direct (exit_proc, Target.CGType.Void);

    (*  return 0; *)
    s.cg.set_source_line (src_line);  INC (src_line);
    s.cg.load_integer (int_t, TInt.Zero);
    s.cg.exit_proc (int_t);
    s.cg.end_procedure (main);
    s.cg.end_unit ();
  END GenerateEntry;

PROCEDURE DeclareParam (VAR s: State;  nm: TEXT;  tipe: Target.CGType): M3CG.Var =
  BEGIN
    RETURN s.cg.declare_param (M3ID.Add (nm), TargetMap.CG_Bytes[tipe],
                               TargetMap.CG_Align_bytes [tipe],
                               tipe, (*typeUID*) 0, (*in_memory*) FALSE,
                               (*up_level*) FALSE, (*frequency*) M3CG.Always);
  END DeclareParam;

PROCEDURE DeclareLocal (VAR s: State;  nm: TEXT;  tipe: Target.CGType): M3CG.Var =
  BEGIN
    RETURN s.cg.declare_local (M3ID.Add (nm), TargetMap.CG_Bytes[tipe],
                               TargetMap.CG_Align_bytes [tipe],
                               tipe, (*typeUID*) 0, (*in_memory*) FALSE,
                               (*up_level*) FALSE, (*frequency*) M3CG.Always);
  END DeclareLocal;

BEGIN
END MxGenCG.

