(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxGenCG.m3                                            *)
(* Last Modified On Thu Jan 18 15:09:58 EST 1996 By ldd        *)
(*                                                             *)
(* From File: MxGen.m3                                         *)
(* Last Modified On Thu Feb 23 08:53:23 PST 1995 By kalsow     *)
(*      Modified On Fri Jul  2 19:33:09 PDT 1993 By muller     *)

MODULE MxGenCG;

IMPORT Stdio;
IMPORT Mx, MxRep, MxMap, M3ID, M3RT, Target;
IMPORT M3CG, M3CG_Ops, Text, TInt, MxGenRep;

FROM MxGenRep IMPORT IR_Prefix, UnitInfo, UnitInfoList, CheckOpaques, 
                     FindBuiltins, FindUnit, SortUnits;

REVEAL
  T = MxGenRep.Common BRANDED "MxGenCG.T" OBJECT
    (* cg information *)
    cg            : M3CG.T       := NIL;
    module_type   : TypeInfo;
    link_info_type: TypeInfo;
    linker_type   : TypeInfo;
    modules_var   : VarInfo      := VarInfo{0, NIL};
    m3_link_info_var: VarInfo    := VarInfo{0, NIL};
    next_unit     : INTEGER      := 0;
    unit_vars     : REF ARRAY OF VarInfo    := NIL;
    main_proc     : ProcInfo     := ProcInfo{0, NIL};
    MI_RTLinker_var  : VarInfo   := VarInfo{0, NIL};
    MM_RTLinker_var  : VarInfo   := VarInfo{0, NIL};
    argv_var      : VarInfo      := VarInfo{0, NIL};
    argc_var      : VarInfo      := VarInfo{0, NIL};
    envp_var      : VarInfo      := VarInfo{0, NIL};
    linker_var    : VarInfo      := VarInfo{0, NIL};
    (* for windows *)
    self_var      : VarInfo      := VarInfo{0, NIL};
    prev_var      : VarInfo      := VarInfo{0, NIL};
    mode_var      : VarInfo      := VarInfo{0, NIL};
    tc_words      : INTEGER      := 0;
  OVERRIDES
    generateMain := GenerateMain;
  END;

TYPE
  State = T;
  TypeInfo = RECORD
    size: INTEGER;
  END;
  VarInfo = RECORD
    id: M3ID.T;
    var: M3CG.Var;
  END;
  ProcInfo = RECORD
    id: M3ID.T;
    proc: M3CG.Proc;
  END;

(*------------------------------------------------------------------------*)

PROCEDURE New(base: Mx.LinkSet;  cg: M3CG.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN): T=
  VAR
    t := NEW(T, 
             cg := cg,
             base    := base,
             verbose := verbose,
             errors  := Stdio.stdout,
             gui     := windowsGUI,
             eol     := Target.EOL);
  BEGIN
    RETURN t;
  END New;

PROCEDURE GenerateMain (s: State)  =
  BEGIN
    GenTypeDecls (s);
    CheckOpaques (s);
    FindBuiltins (s);
    GenerateUnitList (s);
    (* GenerateEntry (s); *)
    (* EmitTypecodeTypedef (s); -- no used by the debugger *)
  END GenerateMain;

(*------------------------------------------------------------------------*)

PROCEDURE GenTypeDecls (VAR s: State) =
  BEGIN
    s.cg.begin_unit();
    EVAL s.cg.declare_segment(M3ID.NoID, -1);
    s.cg.set_source_file("m3main.mc");
    s.cg.set_source_line(1);
    (*
      typedef long  _INTEGER;
      typedef char* _ADDRESS;
      typedef void ( *_PROC)();
      
      typedef struct module {
        _ADDRESS  file;
        _ADDRESS  type_cells;
        _ADDRESS  type_cell_ptrs;
        _ADDRESS  full_revelations;
        _ADDRESS  partial_revelations;
        _ADDRESS  proc_info;
        _ADDRESS  try_scopes;
        _ADDRESS  var_map;
        _ADDRESS  gc_map;
        _PROC     link;
        _PROC     main;
      } _MODULE;
    *)
    s.module_type.size := Target.Address.bytes * 11;

    (*
      typedef struct link_info {
        _INTEGER n_modules;
        _ADDRESS modules;
        _INTEGER argc;
        _ADDRESS argv;
        _ADDRESS envp;
        _ADDRESS instance;
        _ADDRESS bottom_of_stack;
        _ADDRESS top_of_stack;
      } _LINK_INFO;
    *)
    s.link_info_type.size := Target.Address.bytes * 6 
    + Target.Integer.bytes * 2;

    (*
      typedef struct {
        _MODULE     module;
        _ADDRESS    info_typecell[ @sc.tc_words@ ];
        _LINK_INFO *info;
      _LINKER;
    *)
    s.tc_words := M3RT.TC_SIZE DIV Target.Address.size;
    s.linker_type.size := Target.Address.bytes + Target.Address.bytes * 
      s.tc_words + s.module_type.size;
  END GenTypeDecls;


(*------------------------------------------------------------------------*)

PROCEDURE GenerateUnitList (VAR s: State) =
  VAR
    u       : Mx.Unit;
    main    : UnitInfo;
    builtin : UnitInfo;
    nm      : Mx.Name;
    ui, vi  : UnitInfo;
    all_ui  : UnitInfo := NIL;
    intfs   : REF ARRAY OF UnitInfo;
    units   : MxMap.Contents;
    n_units : INTEGER := 0;
  BEGIN
    (* allocate the UnitInfo nodes for the interfaces *)
    units := MxMap.GetData (s.base.interfaces);
    intfs := NEW (REF ARRAY OF UnitInfo, NUMBER (units^));
    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      ui := NIL;
      IF (u # NIL) THEN
        ui := NEW (UnitInfo, unit := u, next := all_ui);
        all_ui := ui;
        INC (n_units);
      END;
      intfs [i] := ui;
    END;

    (* allocate the UnitInfo nodes for the modules *)
    units := MxMap.GetData (s.base.modules);
    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      IF (u # NIL) THEN
        all_ui := NEW (UnitInfo, unit := u, next := all_ui);
        INC (n_units);
      END;
    END;

    (* sort the units by name to help canonicalize _m3main.c *)
    all_ui := SortUnits (all_ui, n_units);

    (* connect the UnitInfo graph *)
    ui := all_ui;
    WHILE (ui # NIL) DO
      u := ui.unit;

      FOR i := u.imported_units.start
            TO u.imported_units.start + u.imported_units.cnt - 1 DO
        nm := u.info [i];
        vi := FindUnit (s, intfs, nm);
        IF (ui # vi) THEN
          ui.imports := NEW (UnitInfoList, next := ui.imports, ui := vi);
        END;
      END;

      FOR i := u.exported_units.start
            TO u.exported_units.start + u.exported_units.cnt - 1 DO
        nm := u.info [i];
        vi := FindUnit (s, intfs, nm);
        IF (ui # vi) THEN
          ui.imports := NEW (UnitInfoList, next := ui.imports, ui := vi);
          vi.exporters := NEW (UnitInfoList, next := vi.exporters, ui := ui);
        END;
      END;

      ui := ui.next;
    END;

    (* generate the debugger's import/export map *)
    (* GenExporters (s, all_ui); *)

    (* import the units *)
    ui := all_ui;
    s.unit_vars := NEW(REF ARRAY OF VarInfo, n_units);
    WHILE (ui # NIL) DO
      ImportUnit (s, ui.unit);
      ui := ui.next;
    END;

    (* locate "Main" *)
    main := FindUnit (s, intfs, M3ID.Add ("Main"));
    builtin := FindUnit (s, intfs, s.builtin_name);

    (* static _MODULE *_modules[ @n_units+1@ ] = { *)
    s.modules_var.id := M3ID.Add("modules");
    s.modules_var.var := 
        s.cg.declare_global(s.modules_var.id, 
                            Target.Address.bytes * n_units , 4, 
                            Target.CGType.Struct, 0, FALSE, FALSE);

    GenerateEntryTop(s);
    s.dfs_count := 1;
    s.init_stack := NIL;
    IF (builtin # NIL) THEN InitUnit (s, builtin, NIL) END;
    InitUnit (s, main, all_ui);
    GenerateEntryBottom(s);

    (* 0};*)

    <*ASSERT n_units = s.n_modules*>
  END GenerateUnitList;


PROCEDURE ImportUnit (VAR s: State;  u: Mx.Unit) =
  BEGIN
    (* extern _MODULE @IR_Prefix [u.interface] & M3ID.ToText(u.name)@; *)
    s.unit_vars[s.next_unit].id := 
        M3ID.Add(IR_Prefix [u.interface] & M3ID.ToText(u.name));
    s.unit_vars[s.next_unit].var :=
        s.cg.import_global(s.unit_vars[s.next_unit].id,
                           s.module_type.size, 4, Target.CGType.Struct, 0);

    IF Text.Equal(M3ID.ToText(u.name), "RTLinker") THEN
      IF u.interface THEN
        s.MI_RTLinker_var.id := s.unit_vars[s.next_unit].id;
        s.MI_RTLinker_var.var := s.unit_vars[s.next_unit].var;
      ELSE
        s.MM_RTLinker_var.id := s.unit_vars[s.next_unit].id;
        s.MM_RTLinker_var.var := s.unit_vars[s.next_unit].var;
      END;
    END;
    INC(s.next_unit);
  END ImportUnit;

PROCEDURE InitUnit (VAR s: State;  ui: UnitInfo;  others: UnitInfo) =
  (* This procedure is adapted from the algorithm, SEARHC, given in
     "The Design and Analysis of Computer Algorithms" by Aho, Hopcroft,
     and Ullman for finding strongly connected components. *)
  VAR x: UnitInfoList;  z, next_z: UnitInfo;  n_mods: INTEGER;
  BEGIN
    IF (ui # NIL) THEN
      ui.init_started := TRUE;
      ui.dfs_id := s.dfs_count;  INC (s.dfs_count);
      ui.low_link := ui.dfs_id;

      <*ASSERT NOT ui.stacked *>
      ui.stacked := TRUE;
      ui.prev_stack := s.init_stack;
      s.init_stack := ui;

      (* visit my imports *)
      x := ui.imports;
      WHILE (x # NIL) DO  InitProbe (s, ui, x.ui); x := x.next;  END;
      (* visit my exporters *)
      x := ui.exporters;
      WHILE (x # NIL) DO  InitProbe (s, ui, x.ui); x := x.next;  END;
    END;

    (* visit everbody else *)
    z := others;
    WHILE (z # NIL) DO  InitProbe (s, ui, z); z := z.next;  END;

    IF (ui # NIL) AND (ui.low_link = ui.dfs_id) THEN
      (* ui is the root of a strongly connected component *)
      (* => "pop" the component off the stack *)
      n_mods := s.n_modules;

      (* first, init the interfaces in the component *)
      z := s.init_stack;
      LOOP
        IF (z.unit.interface) THEN EmitInit (s, z.unit) END;
        IF (z = ui) THEN EXIT END;
        z := z.prev_stack;
      END;

      (* then, init the modules in the component *)
      z := s.init_stack;
      LOOP
        IF (NOT z.unit.interface) THEN EmitInit (s, z.unit) END;
        IF (z = ui) THEN EXIT END;
        z := z.prev_stack;
      END;

      (* finally, pop the stack *)
      z := s.init_stack;
      LOOP
        next_z := z.prev_stack;
        z.stacked := FALSE;
        z.prev_stack := NIL;
        IF (z = ui) THEN EXIT END;
        z := next_z;
      END;
      s.init_stack := next_z;
    END;
  END InitUnit;

PROCEDURE InitProbe (VAR s: State;  v, w: UnitInfo) =
  BEGIN
    IF (NOT w.init_started) THEN
      InitUnit (s, w, NIL);
      IF (v # NIL) THEN v.low_link := MIN (w.low_link, v.low_link) END;
    ELSIF (v # NIL) AND (w.dfs_id < v.dfs_id) AND (w.stacked) THEN
      v.low_link := MIN (w.dfs_id, v.low_link);
    END;
  END InitProbe;

PROCEDURE EmitInit (VAR s: State;  u: Mx.Unit) =
  VAR
    I  := FIRST(s.unit_vars^);
    id := M3ID.Add(IR_Prefix [u.interface] & M3ID.ToText (u.name));
  BEGIN
    (*  &@IR_Prefix [u.interface]& M3ID.ToText (u.name)@, *)
    WHILE (I <= LAST(s.unit_vars^)) DO
      IF (id = s.unit_vars[I].id) THEN EXIT END;
      INC(I);
    END;
    <* ASSERT I <= LAST(s.unit_vars^) *>
    s.cg.load_address(s.unit_vars[I].var);
    s.cg.store(s.modules_var.var, s.n_modules * Target.Address.bytes, 
               Target.CGType.Addr);
    INC (s.n_modules);
  END EmitInit;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateEntryTop (VAR s: State) =
  VAR
    int: Target.Int;
    proc: M3CG.Proc;
    align := Target.Address.align DIV Target.Address.bytes;
  BEGIN
    (*  
        static _LINK_INFO _m3_link_info = {
          /* n_modules  */ @s.n_modules@,
          /* modules    */ (_ADDRESS)_modules,
          /* argc       */ 0,
          /* argv       */ 0,
          /* envp       */ 0,
          /* instance   */ 0,
          /* stack_bot  */ 0,
          /* stack_top  */ (_ADDRESS)0x400000
        };
    *)

    s.m3_link_info_var.id := M3ID.Add("m3_link_info");
    s.m3_link_info_var.var := s.cg.declare_global(s.m3_link_info_var.id, 
                                             s.link_info_type.size, 4, 
                                             Target.CGType.Struct,
                                             0, FALSE, FALSE);
    IF (s.gui) THEN
      (* 
         #include <windows.h>
         int WINAPI
         WinMain (HINSTANCE self, HINSTANCE prev,
                             LPSTR args, int mode)
         {
           { /* initialize RTLinker's global data */
             _LINKER* linker = (_LINKER* )&@IR_Prefix[TRUE]@RTLinker;
      *)
      s.main_proc.id := M3ID.Add("WinMain@16");
      s.main_proc.proc := 
          s.cg.declare_procedure(s.main_proc.id, 4, Target.CGType.Int,
                                 0, Target.DefaultCall, TRUE, NIL);
      s.self_var.id := M3ID.Add("self");
      s.self_var.var := 
          s.cg.declare_param(s.self_var.id, Target.Address.bytes, 
                             4, Target.CGType.Addr, 0, FALSE, FALSE, 1);
      s.prev_var.id := M3ID.Add("pref");
      s.prev_var.var := 
          s.cg.declare_param(s.prev_var.id, Target.Address.bytes,
                             4, Target.CGType.Addr, 0, FALSE, FALSE, 1);
      s.argv_var.id := M3ID.Add("args");
      s.argv_var.var := 
          s.cg.declare_param(s.argv_var.id, Target.Address.bytes,
                             4, Target.CGType.Addr, 0, FALSE, FALSE, 1);
      s.mode_var.id := M3ID.Add("mode");
      s.mode_var.var := 
          s.cg.declare_param(s.mode_var.id, Target.Integer.bytes,
                             4, Target.CGType.Int, 0, FALSE, FALSE, 1);
      s.cg.begin_procedure(s.main_proc.proc);
      s.linker_var.var := 
          s.cg.declare_temp(Target.Address.bytes, align, Target.CGType.Addr, 
	 		    TRUE);
      
      (* linker->info = &_m3_link_info; *)

      <* ASSERT s.MI_RTLinker_var.var # NIL *>
      s.cg.load_address(s.m3_link_info_var.var);
      s.cg.store(s.MI_RTLinker_var.var, s.module_type.size + 
        Target.Address.bytes * s.tc_words, Target.CGType.Addr);
      
      (*
        _m3_link_info.argc = -1;
        _m3_link_info.argv = (_ADDRESS)(args);
        _m3_link_info.envp = (_ADDRESS)GetEnvironmentStrings();
        _m3_link_info.instance = (_ADDRESS)(self);
      *) 

      <* ASSERT TInt.FromInt(-1, int) *>
      s.cg.load_integer(int);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes + Target.Integer.bytes, 
                 Target.CGType.Int);
      s.cg.load(s.argv_var.var, 0, Target.CGType.Addr);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes + Target.Integer.bytes * 2, 
                 Target.CGType.Addr);
      
      proc := s.cg.import_procedure(M3ID.Add("GetEnvironmentStrings@0"), 0, 
                            Target.CGType.Addr, Target.DefaultCall);
      s.cg.start_call_direct(proc, 0, Target.CGType.Addr);
      s.cg.call_direct(proc, Target.CGType.Addr);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes * 2 + Target.Integer.bytes * 2, 
                 Target.CGType.Addr);
      s.cg.load(s.self_var.var, 0, Target.CGType.Addr);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes * 3 + Target.Integer.bytes * 2, 
                 Target.CGType.Int);

    ELSE
      (*
        int main (argc, argv, envp)
        int argc;
        char **argv;
        char **envp;
        {
          { /* initialize RTLinker's global data */
           _LINKER* linker = (_LINKER* )&@IR_Prefix[TRUE]@RTLinker;
      *)
      s.main_proc.id := M3ID.Add("main");
      s.main_proc.proc := 
          s.cg.declare_procedure(s.main_proc.id, 3, Target.CGType.Int,
                                 0, Target.DefaultCall, TRUE, NIL);
      s.argc_var.id  := M3ID.Add("argc");
      s.argc_var.var :=
          s.cg.declare_param(s.argc_var.id, Target.Integer.bytes, 4, 
                             Target.CGType.Int, 0,
                             FALSE, FALSE, 1);
      s.argv_var.id  := M3ID.Add("argv");
      s.argv_var.var :=
          s.cg.declare_param(s.argv_var.id, Target.Address.bytes, 4, 
                             Target.CGType.Addr, 0,
                             FALSE, FALSE, 1);
      s.envp_var.id := M3ID.Add("envp");
      s.envp_var.var :=  
          s.cg.declare_param(s.envp_var.id, Target.Integer.bytes,
                             4, Target.CGType.Int, 0,
                             FALSE, FALSE, 1);
      s.cg.begin_procedure(s.main_proc.proc);
      s.linker_var.var := 
          s.cg.declare_temp(Target.Address.bytes, 4, 
                            Target.CGType.Addr, TRUE);

      (* linker->info = &_m3_link_info; *)

      <* ASSERT s.MI_RTLinker_var.var # NIL *>
      s.cg.load_address(s.m3_link_info_var.var);
      s.cg.store(s.MI_RTLinker_var.var, s.module_type.size + 
        Target.Address.bytes * s.tc_words, Target.CGType.Addr);

      (*
        _m3_link_info.argc = argc;
        _m3_link_info.argv = (_ADDRESS)(argv);
        _m3_link_info.envp = (_ADDRESS)(envp);
        _m3_link_info.instance = (_ADDRESS)(0);
      *)


      s.cg.load(s.argc_var.var, 0, Target.CGType.Int);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes + Target.Integer.bytes, 
                 Target.CGType.Int);
      s.cg.load(s.argv_var.var, 0, Target.CGType.Addr);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes + Target.Integer.bytes * 2, 
                 Target.CGType.Addr);
      s.cg.load(s.envp_var.var, 0, Target.CGType.Addr);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes * 2 + Target.Integer.bytes * 2, 
                 Target.CGType.Addr);
      <* ASSERT TInt.FromInt(0, int) *>
      s.cg.load_integer(int);
      s.cg.store(s.m3_link_info_var.var, 
                 Target.Address.bytes * 3 + Target.Integer.bytes * 2, 
                 Target.CGType.Int);
    END;
  END GenerateEntryTop;

PROCEDURE GenerateEntryBottom (VAR s: State) =
  VAR
    int: Target.Int;
  BEGIN
    (* This is the initialization that would have been done with
       the declaration of m3_link_info *)
    <* ASSERT TInt.FromInt(s.n_modules, int) *>
    s.cg.load_integer(int);
    s.cg.store(s.m3_link_info_var.var,
               0, Target.CGType.Int);
    s.cg.load_address(s.modules_var.var);
    s.cg.store(s.m3_link_info_var.var, Target.Integer.bytes,
               Target.CGType.Addr);
    <* ASSERT TInt.FromInt(16_400000, int) *>
    s.cg.load_integer(int);
    s.cg.store(s.m3_link_info_var.var,
               Target.Address.bytes * 5 + Target.Integer.bytes * 2,
               Target.CGType.Addr);
 
    (*  
          _m3_link_info.bottom_of_stack = (_ADDRESS)(&linker);
        };
    *)
    s.cg.load_address(s.linker_var.var);
    s.cg.store(s.m3_link_info_var.var,
                 Target.Address.bytes * 4 + Target.Integer.bytes * 2, 
                 Target.CGType.Addr);

    (*
      /* finally, start the Modula-3 program */
      @IR_Prefix[FALSE]@RTLinker.main ();
    *)
    <* ASSERT s.MM_RTLinker_var.var # NIL *>
    s.cg.start_call_indirect(Target.CGType.Void, Target.DefaultCall);
    s.cg.load(s.MM_RTLinker_var.var, Target.Address.bytes * 10,
              Target.CGType.Addr);
    s.cg.call_indirect(Target.CGType.Void, Target.DefaultCall);
    (* 
         return 0;
       }
    *)
    s.cg.exit_proc(Target.CGType.Void);
    s.cg.free_temp(s.linker_var.var);
    s.cg.end_procedure(s.main_proc.proc);
    s.cg.end_unit();
  END GenerateEntryBottom;

BEGIN
END MxGenCG.

