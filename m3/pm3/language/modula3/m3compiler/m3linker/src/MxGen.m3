(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxGen.m3                                              *)
(* Last Modified On Thu Feb 23 08:53:23 PST 1995 By kalsow     *)
(*      Modified On Fri Jul  2 19:33:09 PDT 1993 By muller     *)

MODULE MxGen;

IMPORT Wr, Fmt, Thread, IntRefTbl, Stdio, IntArraySort;
IMPORT Mx, MxRep, MxMap, M3ID, M3RT, Target;
<*FATAL Wr.Failure, Thread.Alerted*>

CONST (* name-mangling done by the compiler & back-end *)
  IR_Prefix = ARRAY BOOLEAN OF TEXT { "MM_", "MI_" };

TYPE
  State = RECORD
    base          : Mx.LinkSet   := NIL;
    errors        : Wr.T         := NIL;
    output        : Wr.T         := NIL;
    failed        : BOOLEAN      := FALSE;
    verbose       : BOOLEAN      := FALSE;
    gui           : BOOLEAN      := FALSE;
    n_modules     : INTEGER      := 0;
    dfs_count     : INTEGER      := 0;
    init_stack    : UnitInfo     := NIL;
    n_opaques     : INTEGER      := 0;
    opaques       : IntRefTbl.T  := NIL; (* type name -> OpaqueInfo *)
    all_opaques   : OpaqueInfo   := NIL;
    builtin_name  : Mx.Name      := 0;
    builtin_unit  : Mx.Unit      := NIL;
    eol           : TEXT         := NIL;
  END;

TYPE
  UnitInfo = REF RECORD
    unit         : Mx.Unit;
    next         : UnitInfo;
    imports      : UnitInfoList := NIL;
    exporters    : UnitInfoList := NIL;
    dfs_id       : INTEGER  := 0;
    low_link     : INTEGER  := 0;
    prev_stack   : UnitInfo := NIL;
    init_started : BOOLEAN  := FALSE;
    stacked      : BOOLEAN  := FALSE;
  END;

TYPE
  UnitInfoList = REF RECORD
    ui   : UnitInfo;
    next : UnitInfoList;
  END;

TYPE
  UnitProc = PROCEDURE (VAR s: State;  u: Mx.Unit);

TYPE
  OpaqueInfo = REF RECORD
    next   : OpaqueInfo     := NIL;
    type   : Mx.OpaqueType  := NIL;
    t_unit : Mx.Unit        := NIL;
    reveal : Mx.Revelation  := NIL;
    r_unit : Mx.Unit        := NIL;
  END;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateMain (base: Mx.LinkSet;  output: Wr.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN) =
  VAR s: State;
  BEGIN
    s.base    := base;
    s.output  := output;
    s.verbose := verbose;
    s.errors  := Stdio.stdout;
    s.gui     := windowsGUI;
    s.eol     := Target.EOL;
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

PROCEDURE CheckOpaques (VAR s: State) =
  VAR o: OpaqueInfo;
  BEGIN
    s.opaques := NEW (IntRefTbl.Default).init ();
    ForEachUnit (s, NoteOpaques);
    ForEachUnit (s, IdentifyOpaques);
    o := s.all_opaques;
    WHILE (o # NIL) DO
      IF (o.reveal = NIL) THEN
        Err (s, "opaque type never revealed: ",TName(o.type.type), Wr.EOL);
        Err (s, "  defined in ", MxRep.UnitName (o.t_unit), Wr.EOL);
      END;
      o := o.next;
    END;
  END CheckOpaques;

PROCEDURE NoteOpaques (VAR s: State;  u: Mx.Unit) =
  VAR o: Mx.OpaqueType;  z: OpaqueInfo;  ref: REFANY;
  BEGIN
    o := u.opaques;
    WHILE (o # NIL) DO
      IF s.opaques.get (o.type, ref) THEN
        z := ref;
        Err (s, "opaque type defined twice: ", TName (z.type.type), Wr.EOL);
        Err (s, "  defined in  ", MxRep.UnitName (z.t_unit), Wr.EOL);
        Err (s, "  and also    ", MxRep.UnitName (u), Wr.EOL);
      ELSE
        z := NEW (OpaqueInfo, type := o, t_unit := u, next:= s.all_opaques);
        s.all_opaques := z;
        EVAL s.opaques.put (o.type, z);
        INC (s.n_opaques);
      END;
      o := o.next;
    END;
  END NoteOpaques;

PROCEDURE IdentifyOpaques (VAR s: State;  u: Mx.Unit) =
  VAR z: OpaqueInfo;  ref: REFANY;  r := u.revelations;
  BEGIN
    WHILE (r # NIL) DO
      IF (r.partial) OR (NOT r.export) THEN
        (* ignore for now *)
      ELSIF s.opaques.get (r.lhs, ref) THEN
        z := ref;
        IF (z.reveal # NIL) THEN
          Err (s, "multiple revelations for opaque type:  ",
                   TName(z.type.type), Wr.EOL );
          Err (s, "  defined in  ", MxRep.UnitName (z.t_unit), Wr.EOL);
          Err (s, "  revealed in ", MxRep.UnitName (z.r_unit), Wr.EOL);
          Err (s, "  and also in ", MxRep.UnitName (u), Wr.EOL);
        ELSE
          z.reveal := r;
          z.r_unit := u;
        END;
      ELSE
        Err (s, "revelation without matching opaque type declaration:  ",
                 TName (r.lhs), Wr.EOL);
        Err (s, "  revealed in ", MxRep.UnitName (u), Wr.EOL);
      END;
      r := r.next;
    END;
  END IdentifyOpaques;

(*------------------------------------------------------------------------*)

PROCEDURE FindBuiltins (VAR s: State) =
  VAR u: Mx.Unit;
  BEGIN
    s.builtin_unit := NIL;
    s.builtin_name := M3ID.Add (Mx.BuiltinUnitName);
    u := MxMap.Get (s.base.interfaces, s.builtin_name);
    IF (u = NIL) THEN
      Err (s, "builtins are missing: ", Mx.BuiltinUnitName, Wr.EOL);
    END;
    s.builtin_unit := u;
  END FindBuiltins;

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

PROCEDURE SortUnits (all_ui: UnitInfo;  n_units: INTEGER): UnitInfo =
  (* generate a sorted list of the imports *)
  VAR
    ui,vi : UnitInfo;
    cnt   := 0;
    units := NEW (REF ARRAY OF UnitInfo, n_units);
    map   := NEW (REF ARRAY OF INTEGER, n_units);

  PROCEDURE CmpUnit (a, b: INTEGER): [-1..1] =
    VAR ax := units[a].unit.name;  bx := units[b].unit.name;
    BEGIN
      IF    (ax = bx)          THEN RETURN 0;
      ELSIF M3ID.IsLT (ax, bx) THEN RETURN -1;
      ELSE                          RETURN +1;
      END;
    END CmpUnit;

  BEGIN
    ui := all_ui;
    WHILE (ui # NIL) DO
      units [cnt] := ui;
      map   [cnt] := cnt;
      INC (cnt);
      ui := ui.next;
    END;
    <*ASSERT cnt = n_units*>
      
    IntArraySort.Sort (map^, CmpUnit);

    (* rebuild the linked list *)
    ui := NIL;
    FOR i := n_units-1 TO 0 BY -1 DO
      vi := units[map[i]];
      vi.next := ui;
      ui := vi;
    END;

    RETURN ui;
  END SortUnits;

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

PROCEDURE FindUnit (VAR s: State;  intfs: REF ARRAY OF UnitInfo;
                     READONLY name: Mx.Name): UnitInfo =
  VAR x: INTEGER;
  BEGIN
    x := MxMap.GetIndex (s.base.interfaces, name);
    IF (0 <= x) AND (x <= LAST (intfs^))
      THEN RETURN intfs[x];
      ELSE RETURN NIL;
    END;
  END FindUnit;

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

(*------------------------------------------------------------------------*)

PROCEDURE ForEachUnit (VAR s: State;  p: UnitProc) =
  VAR x: MxMap.Contents;  u: Mx.Unit;
  BEGIN
    x := MxMap.GetData (s.base.interfaces);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
    x := MxMap.GetData (s.base.modules);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
  END ForEachUnit;

PROCEDURE TName (t: Mx.TypeName): TEXT =
  BEGIN
    RETURN "_t" & Fmt.Unsigned (t, 16);
  END TName;

PROCEDURE Err (VAR s: State;  a, b, c, d: TEXT := NIL) =
  BEGIN
    s.failed := TRUE;
    IF (s.errors = NIL) THEN RETURN END;
    IF (a # NIL) THEN Wr.PutText (s.errors, a); END;
    IF (b # NIL) THEN Wr.PutText (s.errors, b); END;
    IF (c # NIL) THEN Wr.PutText (s.errors, c); END;
    IF (d # NIL) THEN Wr.PutText (s.errors, d); END;
  END Err;

PROCEDURE Out (VAR s: State;  a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (s.output, a) END;
    IF (b # NIL) THEN Wr.PutText (s.output, b) END;
    IF (c # NIL) THEN Wr.PutText (s.output, c) END;
    IF (d # NIL) THEN Wr.PutText (s.output, d) END;
  END Out;

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
END MxGen.

