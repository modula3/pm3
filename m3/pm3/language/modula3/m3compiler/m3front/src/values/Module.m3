(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Module.m3                                             *)
(* Last modified on Wed Apr 12 08:36:02 PDT 1995 by kalsow     *)
(*      modified on Tue May 25 10:53:07 PDT 1993 by muller     *)

UNSAFE MODULE Module;

IMPORT M3, M3ID, CG, Value, ValueRep, Scope, Stmt, Error, ESet,  External;
IMPORT Variable, Type, Procedure, Ident, M3Buf, BlockStmt;
IMPORT Host, Token, Revelation, Coverage, Decl, Scanner, WebInfo;
IMPORT ProcBody, Target, M3RT, Marker, M3FP, File, Tracer;
IMPORT M3Compiler;

FROM Scanner IMPORT GetToken, Fail, Match, MatchID, cur;

REVEAL
  T = Value.T BRANDED "Module.T" OBJECT
        safe        : BOOLEAN;
        interface   : BOOLEAN;
        external    : BOOLEAN;
        has_errors  : BOOLEAN;
        genericBase : M3ID.T;
        genericFile : TEXT;
        externals   : External.Set;
        importScope : Scope.T;
        localScope  : Scope.T;
	revelations : Revelation.Set;
        block       : Stmt.T;
        body        : InitBody;
        counter     : ARRAY [0..4] OF CHAR;
        fails       : ESet.T;
        body_origin : INTEGER;
        visit_age   : INTEGER;
        compile_age : INTEGER;
        global_size : INTEGER;
        global_data : CG.Var;
        prefix      : TEXT;
        trace       : Tracer.T;
        type_info   : Type.ModuleInfo;
        value_info  : Value.T;
      OVERRIDES
        typeCheck   := TypeCheckMethod;
        set_globals := ValueRep.NoInit;
        load        := ValueRep.NoLoader;
        declare     := ValueRep.Never;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := ValueRep.NoInit;
        user_init   := ValueRep.NoInit;
	toExpr      := ValueRep.NoExpr;
	toType      := ValueRep.NoType;
        typeOf      := ValueRep.TypeVoid;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := FPType;
      END;

TYPE
  InitBody = ProcBody.T OBJECT
    self: T;
  OVERRIDES
    gen_decl := EmitDecl;
    gen_body := EmitBody;
  END;

TYPE
  TK = Token.T;

VAR (* CONST *)
  builtin0_name : M3ID.T  := M3ID.NoID;
  builtin0      : T       := NIL;
  builtin1_name : M3ID.T  := M3ID.NoID;
  builtin1      : T       := NIL;
  init_proc_uid : INTEGER := 0;

VAR
  curModule   : T := NIL;
  parseStack  : ARRAY [0..200] OF M3ID.T;
  parseDepth  := 0;
  error_buf   : M3Buf.T  := NIL; (* used to compose error messages *)
  visit_age   : INTEGER  := 0;
  visit_proc  : Visitor  := NIL;
  compile_age := 0;

CONST
  ModuleTypeUID = -1;  (* special CG.TypeUID used for all interface records *)

PROCEDURE Reset () =
  BEGIN
    curModule := NIL;
    parseDepth := 0;
    INC (compile_age);
  END Reset;

PROCEDURE Create (name: M3ID.T): T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    ValueRep.Init (t, name, Value.Class.Module);
    t.readonly    := TRUE;
    t.safe        := TRUE;
    t.interface   := TRUE;
    t.external    := FALSE;
    t.has_errors  := FALSE;
    t.genericBase := M3ID.NoID;
    t.genericFile := NIL;
    t.externals   := External.NewSet ();
    t.importScope := NIL;
    t.localScope  := NIL;
    t.block       := NIL;
    t.body        := NIL;
    t.revelations := Revelation.NewSet (t);
    t.fails       := NIL;
    t.prefix      := NIL;
    t.body_origin := Scanner.offset;
    t.visit_age   := 0;
    t.compile_age := compile_age;
    t.global_size := 0;
    t.global_data := NIL;
    t.trace       := NIL;
    t.type_info   := NIL;
    t.value_info  := NIL;
    t.counter[0]  := '_';
    FOR i := 1 TO LAST (t.counter) DO t.counter[i] := '0' END;
    RETURN t;
  END Create;

PROCEDURE Switch (new: T): T =
  VAR
    old        : T               := curModule;
    old_types  : Type.ModuleInfo := NIL;
    old_values : Value.T         := NIL;
    new_types  : Type.ModuleInfo := NIL;
    new_values : Value.T         := NIL;
  BEGIN
    IF (new # NIL) THEN
      new_types  := new.type_info;
      new_values := new.value_info;
    END;
    old_types  := Type.SetModule  (new_types);
    old_values := Value.SetModule (new_values);
    IF (old # NIL) THEN
      old.type_info  := old_types;
      old.value_info := old_values;
    END;
    curModule := new;
    RETURN old;
  END Switch;

PROCEDURE NewDefn (name: TEXT;  safe: BOOLEAN;  syms: Scope.T): T =
  VAR save, t: T;  zz: Scope.T;  yy: Revelation.Set;
  BEGIN
    t := Create (M3ID.Add (name));
    IF (builtin0 = NIL)
      THEN builtin0_name := t.name;  builtin0 := t;
      ELSE builtin1_name := t.name;  builtin1 := t;
    END;
    t.safe := safe;
    save := Switch (t);
    yy := Revelation.Push (t.revelations);
    zz := Scope.Push (Scope.Initial);
      t.importScope := Scope.PushNew (TRUE, M3ID.NoID, module := TRUE);
        IF (syms # NIL) THEN
          t.localScope := syms;
        ELSE
          t.localScope := Scope.PushNew (TRUE, t.name, module := TRUE);
          Scope.PopNew ();
        END;
      Scope.PopNew ();
    Scope.Pop (zz);
    Revelation.Pop (yy);
    RecordInterface (t);
    EVAL Switch (save);
    RETURN t;
  END NewDefn;

PROCEDURE Parse (interfaceOnly : BOOLEAN := FALSE): T =
  VAR
    t, save: T;
    id: M3ID.T;
    n: INTEGER;
    genericReader: File.T;
    yy: Revelation.Set;
    topLevel := NOT interfaceOnly;
    n_errs, n_warns, n_initial_errs: INTEGER;
    cc: CG.CallingConvention;
  BEGIN
    (* ETimer.Push (M3Timers.parse); *)
    Error.Count (n_initial_errs, n_warns);

    t := Create (M3ID.NoID);
    yy := Revelation.Push (t.revelations);
    save := Switch (t);

    IF (cur.token = TK.tEXTERNAL) THEN
      Decl.ParseExternalPragma (id, cc);
      IF (id # M3ID.NoID) THEN
        Error.ID (id, "external module name ignored");
      END;
      t.external := TRUE;
    END;

    IF (cur.token = TK.tUNSAFE) THEN
      t.safe := FALSE;
      GetToken ();
    END;

    t.interface := (cur.token = TK.tINTERFACE);
    IF interfaceOnly THEN
      IF (cur.token = TK.tINTERFACE)
        THEN GetToken ();
        ELSE Fail ("missing INTERFACE keyword");
      END;
      t.interface := TRUE;
    ELSIF (cur.token = TK.tINTERFACE) OR (cur.token = TK.tMODULE) THEN
      GetToken ();
    ELSE Fail ("missing INTERFACE or MODULE keyword");
    END;

    IF t.external AND NOT t.interface THEN
      Error.Msg ("Only interfaces can be <*EXTERNAL*>");
    END;

    id := MatchID ();
    t.name := id;

    IF (t.interface) THEN
      RecordInterface (t);
      IF (topLevel) THEN EVAL PushInterface (id); INC (parseDepth) END;
    END;

    IF (cur.token = TK.tEXPORTS) THEN
      IF (t.interface) THEN
        Error.Msg ("EXPORTS clause not allowed in an interface");
        t.interface := FALSE;
      END;
      GetToken ();
      n := Ident.ParseList ();
      FOR i := 0 TO n - 1 DO
        External.NoteExport (t.externals, Ident.stack[Ident.top - n + i]);
      END;
      DEC (Ident.top, n);
    ELSIF (NOT t.interface) THEN
      External.NoteExport (t.externals, t.name);
    END;

    IF (cur.token = TK.tSEMI) THEN
      (* this is a simple module/interface, just fall through *)
      GetToken (); (* ; *)
    ELSIF (cur.token = TK.tEQUAL) THEN
      (* this is an instantiation of a generic module/interface *)
      GetToken (); (* = *)
      t.genericBase := PushGeneric (t, genericReader);
    ELSE Fail ("missing \';\' or \'=\', assuming \';\'");
    END;

    (* parse the imports *)
    External.ParseImports (t.externals, t);

    (* build my scopes and fill them! *)
    t.importScope := Scope.PushNew (TRUE, M3ID.NoID, module := TRUE);
      (* this scope must be created after the imports & exports are
         parsed so that their module scopes aren't nested inside this one. *)

      (* copy the imports and exports into my scope *)
      External.LoadImports (t.externals, t);

      (* open my private, local scope *)
      t.localScope := Scope.PushNew (TRUE, id, module := TRUE);

        WHILE (cur.token IN Token.DeclStart) DO
          Decl.Parse (t.interface, TRUE, t.fails);
        END;

        t.body_origin := Scanner.offset;
        IF (topLevel) THEN
          t.body := NEW (InitBody, self := t, name :="_INIT" & Prefix (t));
          ProcBody.Push (t.body);
        END;
        IF (NOT t.interface) THEN
          Match (TK.tBEGIN);
          t.trace := BlockStmt.ParseTrace ();
          t.block := Stmt.Parse ();
        END;
        IF (topLevel) THEN
          ProcBody.Pop ();
        END;

        IF (t.genericBase # M3ID.NoID) THEN
          ParseFinalEndID (t.genericBase);
          Scanner.Pop ();
        END;
        Host.CloseFile (genericReader);
        ParseFinalEndID (t.name);

      Scope.PopNew (); (* localScope *)

    Scope.PopNew (); (* importScope *)
    Revelation.Pop (yy);
    IF (t.interface) AND (topLevel) THEN DEC (parseDepth) END;
    EVAL Switch (save);

    Error.Count (n_errs, n_warns);
    IF (n_errs > n_initial_errs) THEN t.has_errors := TRUE; END;

    (* ETimer.Pop (); *)
    RETURN t;
  END Parse;

PROCEDURE PushGeneric (t: T;  VAR rd: File.T): M3ID.T =
(* instantiate a call on a generic interface or module *)
  VAR
    genericName, id : M3ID.T;
    nActuals, aBase : INTEGER;
    nFormals, fBase : INTEGER;
    formal, actual  : M3ID.T;
    filename: TEXT;
    im: T;
    old_file := Scanner.offset;
    save: INTEGER;
    old_filename: TEXT;
  BEGIN
    Scanner.Here (old_filename, save);

    genericName := MatchID ();
    IF (genericName = M3ID.NoID) THEN RETURN genericName END;

    (* parse the list of actuals *)
    nActuals := ParseGenericArgs ();
 
    (* open the external file *)
    rd := Host.OpenUnit (genericName, t.interface, TRUE, filename);
    IF (rd = NIL) THEN
      Error.ID (genericName, "unable to find generic");
      RETURN M3ID.NoID;
    END;

    (* build a synthetic file name & start reading *)
    filename := old_filename & " => " & filename;
    Scanner.Push (filename, rd, is_main := Scanner.in_main);
    t.genericFile := filename;

    (* make sure we got what we wanted *)
    Match (TK.tGENERIC);
    IF (t.interface)
      THEN Match (TK.tINTERFACE);
      ELSE Match (TK.tMODULE);
    END;

    (* get the generic's name *)
    id := MatchID ();
    IF (id # M3ID.NoID) THEN
      IF (id # genericName) THEN
        Error.ID (id, "imported module has wrong name");
        genericName := id;
      END;
    END;

    (* parse the list of formals *)
    nFormals := ParseGenericArgs ();
    Match (TK.tSEMI);

    (* finally, generate the rewriting *)
    IF (nActuals # nFormals) THEN
      save := Scanner.offset;
      Scanner.offset := old_file;
      Error.Msg ("number of actuals doesn\'t match number of generic formals");
      Scanner.offset := save;
    END;
    fBase := Ident.top - nFormals;
    aBase := fBase - nActuals;
    FOR i := 0 TO MAX (nActuals, nFormals)-1 DO
      IF (i < nFormals)
       THEN formal := Ident.stack[fBase + i];
       ELSE formal := Ident.stack[aBase + i]; (* use the actual instead *)
      END;
      IF (i < nActuals)
       THEN actual := Ident.stack[aBase + i];
       ELSE actual := formal; (* use the actual instead *)
      END;
      im := LookUp (actual, internal := FALSE);
      External.NoteImport (t.externals, im, formal);
    END;
    DEC (Ident.top, nActuals + nFormals);

    RETURN genericName;
  END PushGeneric;

PROCEDURE ParseGenericArgs (): INTEGER =
  VAR n := 0;
  BEGIN
    Match (TK.tLPAREN);
    IF (cur.token = TK.tIDENT) THEN
      n := Ident.ParseList ();
    END;
    Match (TK.tRPAREN);
    RETURN n;
  END ParseGenericArgs;

PROCEDURE ParseFinalEndID (goal: M3ID.T) =
  VAR id: M3ID.T;
  BEGIN
    Match (TK.tEND);
    id := MatchID ();
    IF (goal # id) THEN
      Error.ID (id, "Initial module name doesn\'t match final name");
    END;
    Match (TK.tDOT);
    IF (cur.token # TK.tEOF) THEN
      Fail ("extra tokens ignored");
    END;
  END ParseFinalEndID;

PROCEDURE PushInterface (name: M3ID.T): BOOLEAN =
  VAR i: INTEGER;
  BEGIN
    (* check for a cycle in the active imports *)
    parseStack [parseDepth] := name;
    i := 0;  WHILE (parseStack[i] # name) DO INC (i) END;
    IF (i = parseDepth) THEN RETURN TRUE END;

    IF (error_buf = NIL) THEN error_buf := M3Buf.New (); END;
    M3ID.Put (error_buf, name);
    FOR j := i+1 TO parseDepth DO
      M3Buf.PutText (error_buf, " -> ");
      M3ID.Put      (error_buf, parseStack [j]);
    END;
    Error.Txt (M3Buf.ToText (error_buf), "circular imports");
    RETURN FALSE;
  END PushInterface;

PROCEDURE LookUp (name: M3ID.T;  internal: BOOLEAN): T =
  (* find and return the named interface module *)
  VAR
    t: T;
    save: INTEGER;
    filename: TEXT;
    cs := M3.OuterCheckState;
    rd: File.T;
  BEGIN
    IF NOT internal THEN
      IF NOT PushInterface (name) THEN RETURN NIL END;
    END;

    t := Host.env.find_ast (name);
    IF (t # NIL) THEN
      IF (t.has_errors) THEN
        Error.ID (name, "imported interface contains errors");
      END;
      MakeCurrent (t);
    ELSE
      (* open the external file & parse the interface*)
      rd := Host.OpenUnit (name, TRUE, FALSE, filename);
      IF (rd = NIL) THEN
        Error.ID (name, "unable to find interface");
        RETURN NIL;
      END;

      Scanner.Push (filename, rd, is_main := FALSE);
      INC (parseDepth);
      t := Parse (TRUE);
      DEC (parseDepth);
      Scanner.Pop ();
      Host.CloseFile (rd);
      rd := NIL;

      (* make sure we got what we wanted *)
      IF (t = NIL) THEN
        Error.ID (name, "imported object is not an interface");
        RETURN NIL;
      END;
      IF (t.name # name) THEN
        save := Scanner.offset;
        Scanner.offset := t.origin;
        Error.ID (name, "imported interface has wrong name");
        Scanner.offset := save;
        RETURN NIL;
      END;
      IF (NOT t.interface) THEN
        save := Scanner.offset;
        Scanner.offset := t.origin;
        Error.ID (name, "imported unit is not an interface");
        Scanner.offset := save;
        RETURN NIL;
      END;
      RecordInterface (t);
      Value.TypeCheck (t, cs);
    END;
    IF (curModule # NIL) AND (curModule.safe)
      AND (NOT t.safe) AND (NOT internal) THEN
      Error.ID (name, "cannot import an unsafe interface in a safe module");
    END;
    RETURN t;
  END LookUp;

PROCEDURE MakeCurrent (t: T) =
  BEGIN
    IF (t # NIL) AND (t.compile_age < compile_age) THEN
      t.compile_age := compile_age;
      t.global_data := NIL;
      t.used        := FALSE;
      t.imported    := TRUE;
      t.exported    := FALSE;
      Value.Reuse (t.value_info);
      Revelation.Reuse (t.revelations);
      External.Visit (t.externals, MakeCurrent);
    END;
  END MakeCurrent;

PROCEDURE RecordInterface (t: T) =
  (* we must be careful not to overwrite the cached values of
     the builtin interfaces (e.g. Word), because the versions generated
     from source don't have the special procedure methods needed
     for code generation and constant evaluation. *)
  BEGIN
    IF (t = NIL) OR (t.name = M3ID.NoID) THEN RETURN END;
    IF (t.name = builtin0_name) AND (t # builtin0) THEN RETURN END;
    IF (t.name = builtin1_name) AND (t # builtin1) THEN RETURN END;
    Host.env.note_ast (t.name, t);
  END RecordInterface;

PROCEDURE ImportRevelations (t: T;  source: Value.T) =
  BEGIN
    Revelation.Inherit (t.revelations, source);
  END ImportRevelations;

PROCEDURE TypeCheckMethod (t: T;  VAR cs: Value.CheckState) =
  BEGIN
    TypeCheck (t, FALSE, cs);
  END TypeCheckMethod;

PROCEDURE TypeCheck (t: T;  main: BOOLEAN;  VAR cs: Value.CheckState) =
  VAR
    save: T;
    yy: Revelation.Set;
    z1, z2: Scope.T;
    save_main: BOOLEAN;
    n_errs, n_warns, n_initial_errs: INTEGER;
  BEGIN
    IF (t.checked) THEN RETURN END;
    (* ETimer.Push (M3Timers.check); *)
    Error.Count (n_initial_errs, n_warns);

    save := Switch (t);
    save_main := Scanner.in_main;
    Scanner.in_main := main;
    yy := Revelation.Push (t.revelations);
    SoftPush (t.importScope, z1);
      Scope.TypeCheck (t.importScope, cs);
      SoftPush (t.localScope, z2);
        ESet.TypeCheck (t.fails);
        ESet.Push (cs, NIL, t.fails, stop := TRUE);

          Revelation.TypeCheck (t.revelations);
          Scope.TypeCheck (t.localScope, cs);
          IF (NOT t.interface) THEN
            BlockStmt.CheckTrace (t.trace, cs);
            Stmt.TypeCheck (t.block, cs);
          END;

        ESet.Pop (cs, NIL, t.fails, stop := TRUE);
      SoftPop (t.localScope, z2);
    SoftPop (t.importScope, z1);
    Revelation.Pop (yy);
    CheckDuplicates (t);
    IF (main) THEN
      NoteVisibility (t);
      Scope.WarnUnused (t.importScope);
      Scope.WarnUnused (t.localScope);
    END;

    Error.Count (n_errs, n_warns);
    IF (n_errs > n_initial_errs) THEN t.has_errors := TRUE; END;

    SetGlobals (t);
    Scanner.in_main := save_main;
    EVAL Switch (save);

    Error.Count (n_errs, n_warns);
    IF (n_errs > n_initial_errs) THEN t.has_errors := TRUE; END;
    (* ETimer.Pop (); *)

    (* This is a horrible hack!  Since we want to call Module.TypeCheck
       with "main:=TRUE" directly from M3Compiler.Compile, we bypass
       the normal flag setting done by Value.TypeCheck. *)
    t.checkDepth := 0;
    t.checked := TRUE;
  END TypeCheck;

PROCEDURE SoftPush (s: Scope.T;  VAR tmp: Scope.T) =
  (* the scopes may be NIL when there's illegal cycles in the import graph *)
  BEGIN
    IF (s # NIL) THEN  tmp := Scope.Push (s)  END;
  END SoftPush;

PROCEDURE SoftPop (s: Scope.T;  tmp: Scope.T) =
  BEGIN
    IF (s # NIL) THEN Scope.Pop (tmp)  END;
  END SoftPop;

PROCEDURE SetGlobals (t: T) =
  (* Interface record offsets are allocated here.  We don't
     allocate them during typechecking, since the order is
     unpredictable.  Here, the offsets are allocated to
     objects in the order that they appear in the source. *)
  VAR v := Scope.ToList (t.localScope);
  BEGIN
    IF (t.has_errors) THEN (*don't bother *) RETURN END;
    IF (Host.verbose) OR (Host.load_map AND Scanner.in_main) THEN
      Out (Target.EOL, Target.EOL, " allocation for ");
      Out (Prefix (t), Target.EOL);
    END;
    IF (t.global_size = 0) THEN
      EVAL Allocate (M3RT.MI_SIZE, Target.Address.align, "*module info*");
    END;
    Type.BeginSetGlobals ();
    WHILE (v # NIL) DO
      Type.SetGlobals (v.origin);
      Value.SetGlobals (v);  v := v.next;
    END;
    Type.SetGlobals (LAST (INTEGER));
  END SetGlobals;

PROCEDURE Allocate (size, align: INTEGER;
                    tag: TEXT := NIL;  id: M3ID.T := M3ID.NoID): INTEGER =
  VAR offset: INTEGER;
  BEGIN
    align  := MAX (align, Target.Byte);
    align  := (align + Target.Byte - 1) DIV Target.Byte * Target.Byte;
    size   := (size  + Target.Byte - 1) DIV Target.Byte * Target.Byte;
    offset := (curModule.global_size + align - 1) DIV align * align;
    curModule.global_size := offset + size;

    IF (Host.verbose) OR (Host.load_map AND Scanner.in_main) THEN
      OutI (offset DIV Target.Byte, 6);
      OutI (size   DIV Target.Byte, 6);
      OutI (align  DIV Target.Byte, 3);
      Out  ("  ", tag);
      IF (id # M3ID.NoID) THEN M3ID.Put (load_map, id); END;
      Out  (Target.EOL);
    END;

    RETURN offset;
  END Allocate;

VAR load_map: M3Buf.T := NIL;
CONST Pads = ARRAY [0..5] OF TEXT { "", " ", "  ", "   ", "    ", "     " };

PROCEDURE OutI (n, width: INTEGER) =
  VAR x := 10;  pad := width - 1;
  BEGIN
    IF (load_map = NIL) THEN load_map := M3Buf.New () END;
    WHILE (pad > 0) AND (n >= x) DO DEC (pad); x := 10*x; END;
    IF (pad > 0) THEN M3Buf.PutText (load_map, Pads[pad]) END;
    M3Buf.PutInt (load_map, n);
  END OutI;

PROCEDURE Out (a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (load_map = NIL) THEN load_map := M3Buf.New () END;
    IF (a # NIL) THEN M3Buf.PutText (load_map, a) END;
    IF (b # NIL) THEN M3Buf.PutText (load_map, b) END;
    IF (c # NIL) THEN M3Buf.PutText (load_map, c) END;
    IF (d # NIL) THEN M3Buf.PutText (load_map, d) END;
  END Out;

PROCEDURE CheckDuplicates (t: T) =
  VAR
    v, v2 : Value.T;
    save  := Scanner.offset;
  BEGIN
    M3ID.AdvanceMarks ();

    (* mark all the imports *)
    v := Scope.ToList (t.importScope);
    WHILE (v # NIL) DO
      EVAL M3ID.SetMark (v.name);
      v := v.next;
    END;

    (* check for anything already marked in the local scope *)
    v := Scope.ToList (t.localScope);
    WHILE (v # NIL) DO
      IF M3ID.SetMark (v.name) THEN
        v2 := Scope.LookUp (t.importScope, v.name, strict := TRUE);
        IF (v2 # NIL) THEN
          (* possible duplicate *)
          IF (NOT External.IsExportable (v2))
            OR (Value.ClassOf (v)  # Value.Class.Procedure)
            OR (Value.ClassOf (v2) # Value.Class.Procedure) THEN
            Scanner.offset := v.origin;
            Error.ID (v.name, "symbol redefined");
          ELSE
            Procedure.NoteExport (v, v2);
            External.Redirect (v2, v);
          END;
        END;
      END;
      v := v.next;
    END;

    Scanner.offset := save;
  END CheckDuplicates;

PROCEDURE NoteVisibility (t: T) =
  VAR v := Scope.ToList (t.localScope);
  BEGIN
    WHILE (v # NIL) DO
      CASE Value.ClassOf (v) OF
      | Value.Class.Module,
        Value.Class.Error =>
          (* no change of import/export status *)
      | Value.Class.Expr,
        Value.Class.Var,
        Value.Class.Type,
        Value.Class.Exception =>
          IF (t.interface) THEN
            <*ASSERT NOT v.imported*>
            v.exported := TRUE;
            v.exportable := TRUE;
          (* ELSE no change of import/export status *)
          END;
      | Value.Class.Procedure =>
          <*ASSERT NOT v.imported*>
          IF (t.interface) THEN
            v.used     := TRUE; (* force a version stamp *)
            v.exported := TRUE;
            v.imported := FALSE;
            (*****
            v.exported := v.external;
            v.imported := NOT v.exported;
            ****)
            v.exportable := TRUE;
          END;
      | Value.Class.Field,
        Value.Class.Method,
	Value.Class.Formal =>
          <* ASSERT FALSE *>
      END;
      v := v.next;
    END;
  END NoteVisibility;

PROCEDURE IsSafe (): BOOLEAN =
  BEGIN
    RETURN (curModule = NIL) OR (curModule.safe);
  END IsSafe;

PROCEDURE IsInterface (): BOOLEAN =
  BEGIN
    RETURN (curModule = NIL) OR (curModule.interface);
  END IsInterface;

PROCEDURE IsExternal (): BOOLEAN =
  BEGIN
    RETURN (curModule # NIL) AND (curModule.external);
  END IsExternal;

PROCEDURE GetImports (t: T): M3Compiler.IDList =
  BEGIN
    RETURN External.GetImports (t.externals);
  END GetImports;

PROCEDURE ExportScope (t: T): Scope.T =
  BEGIN
    IF (t = NIL)
      THEN RETURN NIL;
      ELSE RETURN t.localScope;
    END;
  END ExportScope;

PROCEDURE Compile (t: T) =
  VAR save: T;  zz: Scope.T;  yy: Revelation.Set;
  BEGIN
    (* ETimer.Push (M3Timers.emit); *)
    save := Switch (t);
    Scanner.offset := t.origin;
    yy := Revelation.Push (t.revelations);
    zz := Scope.Push (t.localScope);
      WebInfo.Reset ();
      CG.Begin_unit ();
      CG.Gen_location (t.origin);
      Host.env.note_unit (t.name, t.interface);
      DeclareGlobalData (t);
      IF (t.body # NIL) THEN EmitDecl (t.body); END;
      Type.CompileAll ();
      IF (t.interface)
        THEN CompileInterface (t);
        ELSE CompileModule (t);
      END;
      IF (load_map # NIL) THEN
        CG.Comment (-1, "load map", Target.EOL, M3Buf.ToText (load_map));
        load_map := NIL;
      END;
      CG.End_unit ();
      Host.env.note_webinfo (WebInfo.Finish ());
    Scope.Pop (zz);
    Revelation.Pop (yy);
    EVAL Switch (save);
    (* ETimer.Pop (); *)
  END Compile;

PROCEDURE CompileInterface (t: T) =
  VAR proc_info, type_map, rev_full, rev_part: INTEGER;  link_proc: CG.Proc;
  BEGIN
    (* declare the modules that I import & export *)
    (** EVAL GlobalData (t); **)
    CG.Export_unit (t.name);
    Host.env.note_interface_use (t.name, imported := FALSE);
    IF (t.genericBase # M3ID.NoID) THEN
      Host.env.note_generic_use (t.genericBase);
    END;
    External.GenLinkInfo (t.externals);

    (* declare my imports, exports and local variables *)
    External.GenImports (t.externals);
    Scope.Enter (t.importScope);
    Scope.Enter (t.localScope);

    (* declare any visible revelations *)
    Revelation.Declare (t.revelations, rev_full, rev_part);

    (* generate any internal procedures *)
    ProcBody.EmitAll (proc_info, link_proc);

    type_map := Variable.GenGlobalMap (t.localScope);

    GenLinkerInfo (t, proc_info, type_map, rev_full, rev_part, link_proc);

  END CompileInterface;

PROCEDURE CompileModule (t: T) =
  VAR proc_info, type_map, rev_full, rev_part: INTEGER;  link_proc: CG.Proc;
  BEGIN
    (* declare the modules that I import & export *)
    IF (t.genericBase # M3ID.NoID) THEN
      Host.env.note_generic_use (t.genericBase);
    END;
    External.GenLinkInfo (t.externals);

    (* declare my imports, exports and local variables *)
    (**** moved below **** External.GenImports (t.externals); *)
    Scope.Enter (t.importScope);
    Scope.Enter (t.localScope);

    (* declare any visible revelations *)
    Revelation.Declare (t.revelations, rev_full, rev_part);

    (* generate the tables for coverage *)
    Coverage.GenerateTables ();

    (* generate any internal procedures *)
    ProcBody.EmitAll (proc_info, link_proc);

    type_map := Variable.GenGlobalMap (t.localScope);

    (* declare my imports *)
    External.GenImports (t.externals);
    (* we deferred the import declarations until all the code
       has been generated to pick up imports that are used
       via "Value.Load", but not "Scope.LookUp". *)

    GenLinkerInfo (t, proc_info, type_map, rev_full, rev_part, link_proc);

  END CompileModule;

PROCEDURE DeclareGlobalData (t: T) =
  BEGIN
    CG.Comment (-1, "module global data");
    t.global_data := CG.Declare_segment (M3ID.Add (Prefix (t)), ModuleTypeUID);
  END DeclareGlobalData;

PROCEDURE GlobalData (t: T): CG.Var =
  BEGIN
    IF (t = NIL) THEN t := curModule END;
    <*ASSERT t.compile_age >= compile_age*>
    IF (t.global_data = NIL) THEN
      (* this is the first reference to the imported interface 't' *)
      t.global_data := CG.Import_global (M3ID.Add (Prefix (t)), t.global_size,
                                         Target.Address.align,
                                         CG.Type.Struct, ModuleTypeUID);
    END;
    RETURN t.global_data;
  END GlobalData;

PROCEDURE NeedMain (t: T): BOOLEAN =
  VAR v: Value.T;
  BEGIN
    (* were there any user written statements? *)
    IF (t.block # NIL) THEN RETURN TRUE END;

    (* do any of the imported symbols require init code? *)
    v := Scope.ToList (t.importScope);
    WHILE (v # NIL) DO
      IF Value.NeedsInit (v) THEN RETURN TRUE END;
      v := v.next;
    END;

    (* do any of the global symbols require init code? *)
    v := Scope.ToList (t.localScope);
    WHILE (v # NIL) DO
      IF Value.NeedsInit (v) THEN RETURN TRUE END;
      v := v.next;
    END;

    IF External.NeedGlobalInit (t.externals) THEN RETURN TRUE END;

    RETURN FALSE;
  END NeedMain;

PROCEDURE EmitDecl (x: InitBody) =
  VAR t := x.self;
  BEGIN
    IF (x.cg_proc # NIL) THEN RETURN END;
    IF NOT NeedMain (t) THEN RETURN END;
    Scanner.offset := t.body_origin;
    CG.Gen_location (t.body_origin);
    x.cg_proc := CG.Declare_procedure (M3ID.Add (x.name), 0, CG.Type.Void,
                                 lev := 0, cc := Target.DefaultCall,
                                 exported := FALSE, parent := NIL);
  END EmitDecl;

PROCEDURE EmitBody (x: InitBody) =
  VAR t := x.self;  zz: Scope.T;
  BEGIN
    IF (x.cg_proc = NIL) THEN RETURN END;

    (* restore my environment *)
    zz := Scope.Push (t.localScope);

    (* generate my initialization procedure *)
    CG.Comment (-1, "module main body ", x.name);
    Scanner.offset := t.body_origin;
    CG.Gen_location (t.body_origin);
    CG.Begin_procedure (x.cg_proc);
    Scope.InitValues (t.importScope);
    Scope.InitValues (t.localScope);

    (* initialize my exported variables *)
    External.InitGlobals (t.externals);

    (* perform the main body *)
    Tracer.Push (t.trace);
    EVAL Stmt.Compile (t.block);
    Tracer.Pop (t.trace);

    CG.Exit_proc (CG.Type.Void);
    CG.End_procedure (x.cg_proc);

    Scope.Pop (zz);
  END EmitBody;

PROCEDURE GenLinkerInfo (t: T;
                         proc_info, type_map, rev_full, rev_part: INTEGER;
                         link_proc: CG.Proc) =
  CONST LinkName = ARRAY BOOLEAN OF TEXT {"M3_LINK", "I3_LINK"};
  CONST MainName = ARRAY BOOLEAN OF TEXT {"M3_MAIN", "I3_MAIN"};
  VAR
    v := t.global_data;
    file: TEXT;
    line, offs: INTEGER;
    type_cells, type_cell_ptrs: INTEGER;
    exception_scopes := Marker.EmitScopeTable ();
  BEGIN
    Scanner.offset := t.origin;
    IF (t.genericFile # NIL) THEN
      offs := CG.EmitText (t.genericFile);
    ELSE
      Scanner.Here (file, line);
      offs := CG.EmitText (file);
    END;
    CG.Init_var (M3RT.MI_file, v, offs);
    CG.Comment (offs, "file name");

    type_cells := Type.GenCells ();
    type_cell_ptrs := Type.GenCellPtrs ();
    (* note: the type info cannot be generated until *all* types have
       have been declared *)

    IF (type_cells # 0) THEN
      CG.Init_var (M3RT.MI_type_cells, v, type_cells);
    END;
    IF (type_cell_ptrs # 0) THEN
      CG.Init_var (M3RT.MI_type_cell_ptrs, v, type_cell_ptrs);
    END;
    IF (rev_full # 0) THEN
      CG.Init_var (M3RT.MI_full_rev, v, rev_full);
    END;
    IF (rev_part # 0) THEN
      CG.Init_var (M3RT.MI_part_rev, v, rev_part);
    END;
    IF (proc_info # 0) THEN
      CG.Init_var (M3RT.MI_proc_info, v, proc_info);
    END;
    IF (exception_scopes # 0) THEN
      CG.Init_var (M3RT.MI_try_scopes, v, exception_scopes);
    END;
    IF (type_map > 0) THEN
      CG.Init_var (M3RT.MI_var_map, v, type_map);
      CG.Init_var (M3RT.MI_gc_map, v, type_map);
    END;
    IF (link_proc # NIL) THEN
      CG.Declare_global_field (M3ID.Add (LinkName [t.interface]), M3RT.MI_link,
                               Target.Address.size, InitProcType ());
      CG.Init_proc (M3RT.MI_link, link_proc);
    END;
    IF (t.body # NIL) AND (t.body.cg_proc # NIL) THEN
      CG.Declare_global_field (M3ID.Add (MainName [t.interface]), M3RT.MI_main,
                               Target.Address.size, InitProcType ());
      CG.Init_proc (M3RT.MI_main, t.body.cg_proc);
    END;

    (* finish up the global data segment allocations *)
    EVAL Allocate (0, Target.Address.align, "*TOTAL*");

    (* generate a debugging type descriptor for the global data *)
    CG.Comment (-1, "global data type descriptor");
    CG.Emit_global_record (t.global_size);

    (* finish the global data initializations *)
    CG.Comment (-1, "module global data");
    CG.Bind_segment (t.global_data, t.global_size, CG.Max_alignment,
                     CG.Type.Struct, exported := TRUE, init := TRUE);
  END GenLinkerInfo;

PROCEDURE InitProcType(): INTEGER =
  (* we need the UID of a procedure type with no parameters... *)
  BEGIN
    IF (init_proc_uid = 0) THEN
      init_proc_uid := M3FP.ToInt (M3FP.FromText ("PROC()"));
    END;
    RETURN init_proc_uid;
  END InitProcType;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  CONST Tags = ARRAY BOOLEAN OF TEXT { "MODULE ", "INTERFACE " };
  BEGIN
    ValueRep.FPStart (t, x, Tags[t.interface], 0, global := FALSE);
    RETURN 0;
  END AddFPTag;

PROCEDURE FPType (<*UNUSED*> t: T): Type.T =
  BEGIN
    RETURN NIL;
  END FPType;

PROCEDURE Current (): T =
  BEGIN
    RETURN curModule;
  END Current;

PROCEDURE Name (t: T): M3ID.T =
  BEGIN
    IF (t = NIL) THEN t := curModule;   END;
    IF (t = NIL) THEN RETURN M3ID.NoID; END;
    RETURN t.name;
  END Name;

PROCEDURE Prefix (t: T): TEXT =
  CONST Prefix = ARRAY (*t.interface*)BOOLEAN OF TEXT {"M_", "I_"};
  BEGIN
    IF (t = NIL) THEN t := curModule; END;
    IF (t = NIL) THEN RETURN "";      END;
    IF (t.prefix = NIL) THEN
      t.prefix := Prefix [t.interface] & M3ID.ToText (t.name);
    END;
    RETURN t.prefix;
  END Prefix;

PROCEDURE CurrentCounter (): ARRAY [0..4] OF CHAR =
  BEGIN
    <* ASSERT curModule # NIL *>
    RETURN curModule.counter;
  END CurrentCounter;

PROCEDURE SetCurrentCounter (c: ARRAY [0..4] OF CHAR) =
  BEGIN
    <* ASSERT curModule # NIL *>
    curModule.counter := c;
  END SetCurrentCounter;

PROCEDURE GetTypeInfo (t: T): Type.ModuleInfo =
  BEGIN
    RETURN t.type_info;
  END GetTypeInfo;

PROCEDURE VisitImports (v: Visitor) =
  BEGIN
    <*ASSERT visit_proc = NIL *>
    visit_proc := v;
    INC (visit_age);
    External.Visit (curModule.externals, InnerVisit);
    visit_proc := NIL;
  END VisitImports;

PROCEDURE InnerVisit (t: T) =
  BEGIN
    IF (t # NIL) AND (t.visit_age < visit_age) THEN
      t.visit_age := visit_age;
      External.Visit (t.externals, InnerVisit);
      visit_proc (t);
    END;
  END InnerVisit;

BEGIN
END Module.
