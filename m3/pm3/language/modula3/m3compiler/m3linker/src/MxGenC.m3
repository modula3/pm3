(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxGenC.m3                                             *)
(* Last Modified On Thu Jan 18 15:09:58 EST 1996 By ldd        *)
(*                                                             *)
(* From File: MxGen.m3                                         *)
(* Last Modified On Thu Feb 23 08:53:23 PST 1995 By kalsow     *)
(*      Modified On Fri Jul  2 19:33:09 PDT 1993 By muller     *)

MODULE MxGenC;

IMPORT Wr, Fmt, Stdio;
IMPORT Mx, MxRep, MxMap, M3ID, M3RT, Target, MxGenRep;

FROM MxGenRep IMPORT IR_Prefix, UnitInfo, UnitInfoList, CheckOpaques, 
                     FindBuiltins, FindUnit, Out, SortUnits;

REVEAL
  T = MxGenRep.Common BRANDED "MxGenC.T" OBJECT
  OVERRIDES
    generateMain := GenerateMain;
  END;

TYPE
  State = T;

(*------------------------------------------------------------------------*)

PROCEDURE New(base: Mx.LinkSet;  output: Wr.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN): T=
  VAR
    t := NEW(T, 
             output  := output,
             base    := base,
             verbose := verbose,
             errors  := Stdio.stdout,
             gui     := windowsGUI,
             eol     := Target.EOL);
  BEGIN
    RETURN t;
  END New;

PROCEDURE GenerateMain (s: State) =
  BEGIN
    GenTypeDecls (s);
    CheckOpaques (s);
    FindBuiltins (s);
    GenerateUnitList (s);
    GenerateEntry (s);
    (* EmitTypecodeTypedef (s); -- no used by the debugger *)
  END GenerateMain;

(*------------------------------------------------------------------------*)

PROCEDURE GenTypeDecls (VAR s: State) =
  VAR  tc_words := Fmt.Int (M3RT.TC_SIZE DIV Target.Address.size);
  BEGIN
    Out (s, "typedef long  _INTEGER;", s.eol);
    Out (s, "typedef char* _ADDRESS;", s.eol);
    Out (s, "typedef void (*_PROC)();", s.eol);
    Out (s, s.eol);

    Out (s, "typedef struct module {", s.eol);
    Out (s, "  _ADDRESS  file;", s.eol);
    Out (s, "  _ADDRESS  type_cells;", s.eol);
    Out (s, "  _ADDRESS  type_cell_ptrs;", s.eol);
    Out (s, "  _ADDRESS  full_revelations;", s.eol);
    Out (s, "  _ADDRESS  partial_revelations;", s.eol);
    Out (s, "  _ADDRESS  proc_info;", s.eol);
    Out (s, "  _ADDRESS  try_scopes;", s.eol);
    Out (s, "  _ADDRESS  var_map;", s.eol);
    Out (s, "  _ADDRESS  gc_map;", s.eol);
    Out (s, "  _PROC     link;", s.eol);
    Out (s, "  _PROC     main;", s.eol);
    Out (s, "} _MODULE;", s.eol, s.eol);

    Out (s, "typedef struct link_info {", s.eol);
    Out (s, "  _INTEGER n_modules;", s.eol);
    Out (s, "  _ADDRESS modules;", s.eol);
    Out (s, "  _INTEGER argc;", s.eol);
    Out (s, "  _ADDRESS argv;", s.eol);
    Out (s, "  _ADDRESS envp;", s.eol);
    Out (s, "  _ADDRESS instance;", s.eol);
    Out (s, "  _ADDRESS bottom_of_stack;", s.eol);
    Out (s, "  _ADDRESS top_of_stack;", s.eol);
    Out (s, "} _LINK_INFO;", s.eol, s.eol);

    Out (s, "typedef struct {", s.eol);
    Out (s, "  _MODULE     module;", s.eol);
    Out (s, "  _ADDRESS    info_typecell[", tc_words, "];", s.eol);
    Out (s, "  _LINK_INFO *info;", s.eol);
    Out (s, "} _LINKER;", s.eol, s.eol);

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
    GenExporters (s, all_ui);

    (* import the units *)
    ui := all_ui;
    WHILE (ui # NIL) DO
      ImportUnit (s, ui.unit);
      ui := ui.next;
    END;

    (* locate "Main" *)
    main := FindUnit (s, intfs, M3ID.Add ("Main"));
    builtin := FindUnit (s, intfs, s.builtin_name);

    Out (s, s.eol, "static _MODULE *_modules[");
    Out (s, Fmt.Int (n_units+1), "] = {", s.eol);

    s.dfs_count := 1;
    s.init_stack := NIL;
    IF (builtin # NIL) THEN InitUnit (s, builtin, NIL) END;
    InitUnit (s, main, all_ui);

    Out (s, "  0", s.eol, "};");
    Out (s, s.eol, s.eol);

    <*ASSERT n_units = s.n_modules*>
  END GenerateUnitList;

PROCEDURE GenExporters (VAR s: State;  all_ui: UnitInfo) =
  CONST ShortForm = TRUE;
  VAR ui := all_ui;  u: Mx.Unit;  z: UnitInfoList;
  BEGIN
    Out (s, s.eol, "struct {", s.eol);
    WHILE (ui # NIL) DO
      u := ui.unit;
      IF (u.interface) THEN
        z := ui.exporters;
        IF (z # NIL) THEN
          IF (ShortForm) AND (z.next = NIL) AND (z.ui.unit.name = u.name) THEN
            (* this is a simple "A exports A" case => don't emit it *)
          ELSE
            Out (s, "  struct {");
            WHILE (z # NIL) DO
              Out (s, " int ", M3ID.ToText (z.ui.unit.name), ";");
              z := z.next;
            END;
            Out (s, " } ", M3ID.ToText (u.name), ";", s.eol);
          END;
        END;
      END;
      ui := ui.next;
    END;
    Out (s, "} * _m3_exporters;", s.eol, s.eol);
  END GenExporters;


PROCEDURE ImportUnit (VAR s: State;  u: Mx.Unit) =
  BEGIN
    Out (s, "extern _MODULE ", IR_Prefix [u.interface]);
    Out (s, M3ID.ToText (u.name), ";", s.eol);
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

      IF (n_mods # s.n_modules) THEN Out (s, s.eol) END; (*break list*)

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
  BEGIN
    Out (s, "  &", IR_Prefix [u.interface], M3ID.ToText (u.name));
    Out (s, ",", s.eol);
    INC (s.n_modules);
  END EmitInit;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateEntry (VAR s: State) =
  BEGIN
    Out (s, "static _LINK_INFO _m3_link_info = {", s.eol);
    Out (s, "  /* n_modules  */ ", Fmt.Int (s.n_modules), ",", s.eol);
    Out (s, "  /* modules    */ (_ADDRESS)_modules,", s.eol);
    Out (s, "  /* argc       */ 0,", s.eol);
    Out (s, "  /* argv       */ 0,", s.eol);
    Out (s, "  /* envp       */ 0,", s.eol);
    Out (s, "  /* instance   */ 0,", s.eol);
    Out (s, "  /* stack_bot  */ 0,", s.eol);
    Out (s, "  /* stack_top  */ (_ADDRESS)0x400000", s.eol);
    Out (s, "};", s.eol, s.eol);

    IF (s.gui) THEN
      Out (s, "#include <windows.h>", s.eol);
      Out (s, "int WINAPI ");
      Out (s, "WinMain (HINSTANCE self, HINSTANCE prev,", s.eol);
      Out (s, "                    LPSTR args, int mode)", s.eol);
      Out (s, "{", s.eol);
      Out (s, "  { /* initialize RTLinker's global data */", s.eol);
      Out (s, "    _LINKER* linker = (_LINKER*)&", IR_Prefix[TRUE],
                  "RTLinker;", s.eol);
      Out (s, "    linker->info = &_m3_link_info;", s.eol);
      Out (s, "    _m3_link_info.argc = -1;", s.eol);
      Out (s, "    _m3_link_info.argv = (_ADDRESS)(args);", s.eol);
      Out (s, "    _m3_link_info.envp = (_ADDRESS)GetEnvironmentStrings();", s.eol);
      Out (s, "    _m3_link_info.instance = (_ADDRESS)(self);", s.eol);
    ELSE
      Out (s, "int main (argc, argv, envp)", s.eol);
      Out (s, "int argc;", s.eol);
      Out (s, "char **argv;", s.eol);
      Out (s, "char **envp;", s.eol);
      Out (s, "{", s.eol);
      Out (s, "  { /* initialize RTLinker's global data */", s.eol);
      Out (s, "    _LINKER* linker = (_LINKER*)&", IR_Prefix[TRUE],
                  "RTLinker;", s.eol);
      Out (s, "    linker->info = &_m3_link_info;", s.eol);
      Out (s, "    _m3_link_info.argc = argc;", s.eol);
      Out (s, "    _m3_link_info.argv = (_ADDRESS)(argv);", s.eol);
      Out (s, "    _m3_link_info.envp = (_ADDRESS)(envp);", s.eol);
      Out (s, "    _m3_link_info.instance = (_ADDRESS)(0);", s.eol);
    END;
    Out (s, "    _m3_link_info.bottom_of_stack = (_ADDRESS)(&linker);", s.eol);
    Out (s, "  };", s.eol, s.eol);

    Out (s, "  /* finally, start the Modula-3 program */", s.eol);
    Out (s, "  ", IR_Prefix[FALSE], "RTLinker.main ();", s.eol);
    Out (s, "  return 0;", s.eol);
    Out (s, "}", s.eol, s.eol);
  END GenerateEntry;


(**************************
PROCEDURE OutI (VAR s: State;  i: INTEGER) =
  <*FATAL Convert.Failed*>
  VAR
    buf: ARRAY [0..BITSIZE(INTEGER) + 3] OF CHAR;
    len := Convert.FromInt (buf, i);
  BEGIN
    Wr.PutString (s.output, SUBARRAY (buf, 0, len));
  END OutI;

PROCEDURE OutU (VAR s: State;  i: INTEGER) =
  <*FATAL Convert.Failed*>
  VAR
    buf: ARRAY [0..BITSIZE(INTEGER) + 3] OF CHAR;
    len := Convert.FromUnsigned (buf, i, 16);
  BEGIN
    Wr.PutString (s.output, SUBARRAY (buf, 0, len));
  END OutU;

PROCEDURE OutC (VAR s: State;  c: CHAR) =
  BEGIN
    Wr.PutChar (s.output, c);
  END OutC;
****************************)

BEGIN
END MxGenC.

