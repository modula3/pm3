(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Main.m3                                               *)
(* Last modified on Fri Jan 27 15:42:42 PST 1995 by kalsow     *)
(*      modified on Wed Jun  2 11:50:24 PDT 1993 by muller     *)

MODULE M3Driver;

(* TO DO:
    - do not wait for the result of the compilation of modules; use a crew of
      processes
*)

IMPORT Text, Rd, Wr, TextIntTbl AS TextSet, IntIntTbl AS IntSet;
IMPORT FileRd, OSError, Fmt, IntRefTbl, TextRefTbl, Stdio;
IMPORT FS, Process, File, FileWr, Time, Fingerprint;
IMPORT RTCollector, (*RTCollectorSRC,*) RTutils, Thread, ETimer;
IMPORT M3ID, M3CG, M3Timers, M3Compiler, Target, WebFile;
IMPORT Mx, MxMerge, MxCheck, MxGen, MxIn, MxOut, MxVS;
IMPORT Msg, Arg, Utils, M3Path, Unit, M3Backend;
FROM Msg IMPORT Error;

TYPE
  NK = M3Path.Kind;

VAR
  void: INTEGER;    (* the value for tables used as sets *)

TYPE
  FileInfo = REF RECORD
    next         : FileInfo := NIL;
    library      : TEXT     := NIL;
    source       : TEXT     := NIL;
    object       : TEXT     := NIL;
    link_info    : Mx.Unit  := NIL;
    link_info_ts : INTEGER  := LAST(INTEGER);
    exporters    : FileList := NIL;
    name         : M3Path.T;
    compiling    : BOOLEAN  := FALSE;
    stale_src    : BOOLEAN  := FALSE;
    missing_info : BOOLEAN  := FALSE;
    shared       : BOOLEAN  := FALSE;
  END;

TYPE
  FileList = REF RECORD
    file : FileInfo;
    next : FileList;
  END;

TYPE
  PassInfo = RECORD
    cmd   : TEXT      := NIL;
    args  : Arg.List  := NIL;
    noise : TEXT      := NIL;
  END;

TYPE
  ASTCacheEntry = REF RECORD
    fullname   : TEXT    := NIL;
    name       : M3ID.T;
    time_stamp : INTEGER := 0;
    imports    : ASTList := NIL;
    n_imports  : INTEGER := -1;   (* -1 = means list of imports not yet cheched*)
    interface  : REFANY  := NIL;  (* Module.T *)
  END;

TYPE
  ASTList = REF RECORD
    import : ASTCacheEntry := NIL;
    next   : ASTList       := NIL;
  END;

VAR
  n_targets     : INTEGER   := 0;
  lib_name      : TEXT      := NIL;
  pgm_name      : TEXT      := NIL;
  info_name     : TEXT      := NIL;
  compile_failed: BOOLEAN   := FALSE;
  keep_cache    : BOOLEAN   := FALSE;

VAR (* CONST *)
  TimeNow : INTEGER; (* for AST cache *)

VAR
  warning_level : INTEGER   := 3;
  warning_arg   : TEXT      := "-w3";
  make_mode     : BOOLEAN   := FALSE;
  skip_std_lib  : BOOLEAN   := FALSE;
  keep_files    : BOOLEAN   := FALSE;
  dump_config   : BOOLEAN   := FALSE;
  do_coverage   : BOOLEAN   := FALSE;
  keep_resolved : BOOLEAN   := FALSE;
  need_Rpath    : BOOLEAN   := FALSE;
  supply_shared : BOOLEAN   := FALSE; (* pass -B switches to linker ? *)
  shared_libs   : BOOLEAN   := FALSE; (* a mode, flipped by parsing -B. *)
  new_linkInfo  : BOOLEAN   := FALSE;
  bootstrap_mode: BOOLEAN   := FALSE;
  search_path   : BOOLEAN   := FALSE;
  compile_once  : BOOLEAN   := FALSE;
  gui           : BOOLEAN   := FALSE;  (* for NT *)
  heap_stats    : BOOLEAN   := FALSE;
  skip_link     : BOOLEAN   := FALSE;
  has_loader    : BOOLEAN   := FALSE;
  target_os     : M3Path.OSKind := M3Path.OSKind.Unix;
  host_os       : M3Path.OSKind := M3Path.OSKind.Unix;
  target_machine: TEXT      := NIL;

VAR
  pass_0        : PassInfo  := NewPass ("*UNDEFINED*");
  pass_1        : PassInfo  := NewPass ("*UNDEFINED*");
  pass_2        : PassInfo  := NewPass ("*UNDEFINED*");
  pass_3        : PassInfo  := NewPass ("*UNDEFINED*");
  pass_4        : PassInfo  := NewPass (NIL);
  pass_6        : PassInfo  := NewPass (NIL);
  pass_7        : PassInfo  := NewPass (NIL);
  pass_8        : PassInfo  := NewPass (NIL);
  do_optimize   : BOOLEAN   := FALSE;
  cc_optimize   : Arg.List  := Arg.NewList ();
  cg_optimize   : Arg.List  := Arg.NewList ();
  as_optimize   : Arg.List  := Arg.NewList ();
  do_debug      : BOOLEAN   := FALSE;
  cc_debug      : Arg.List  := Arg.NewList ();
  cg_debug      : Arg.List  := Arg.NewList ();
  as_debug      : Arg.List  := Arg.NewList ();
  link_files    : TEXT      := "-z2@@";
  include_dir   : TEXT      := NIL;
  link_coverage : TEXT      := NIL;

VAR
  n_tfiles      : INTEGER    := 0;
  include_path  : Arg.List   := Arg.NewList ();
  lib_path      : Arg.List   := Arg.NewList ();

VAR
  sources       : FileInfo := NIL;
  libraries     : FileInfo := NIL;
  pending_impls : FileInfo := NIL;

VAR
  interfaces := NEW (IntRefTbl.Default).init ();
  modules    := NEW (IntRefTbl.Default).init ();
  intf_dirs  := NEW (TextSet.Default).init ();
  h_dirs     := NEW (TextSet.Default).init ();
  link_base  : Mx.LinkSet  := NIL;
  generic_intfs := NEW (IntRefTbl.Default).init ();
  generic_impls := NEW (IntRefTbl.Default).init ();

VAR
  magic     := NEW (IntRefTbl.Default).init ();
  ast_cache := NEW (TextRefTbl.Default).init ();

VAR
  long_names: IntRefTbl.T := NIL;  (* for the procedure ShortenName at the bottom *)

(*----------------------------------------------- interface to the server ---*)

PROCEDURE ResetCompiler (wr: Wr.T) =
  BEGIN
    Msg.SetWriter(wr);
    MxVS.Init();
    Utils.Init();
    Unit.ResetPath();
    Init();
  END ResetCompiler;

PROCEDURE Compile(options: TEXT)
  RAISES {M3Error} =
  BEGIN
    TRY
      EvaluateCacheSize();
      DoIt(options);
      EvaluateCacheSize();
    EXCEPT
      | Error  =>  RAISE M3Error;
    END;
  END Compile;

PROCEDURE ResetASTCache () =
  BEGIN
    ast_cache := NEW (TextRefTbl.Default).init (100); (* intf name -> AST *)
  END ResetASTCache;

PROCEDURE EvaluateCacheSize() =
  VAR n : INTEGER:= 1;
      it := ast_cache.iterate();
      name : TEXT;
      ref  : REFANY;
      tmp  : ASTCacheEntry;
  BEGIN
    IF NOT keep_cache THEN  RETURN;  END;
Wr.PutText(Stdio.stdout, Wr.EOL & "======= INTERFACES IN THE CACHE: =======" & Wr.EOL);

    WHILE it.next(name, ref) DO
      tmp:= ref;
Wr.PutText(Stdio.stdout,Wr.EOL & Fmt.Int(n) & " : " & tmp.fullname );
      INC(n);
    END;
Wr.PutText(Stdio.stdout,Wr.EOL & Wr.EOL);
Wr.PutText(Stdio.stdout,"There are " & Fmt.Int(n-1) & " interfaces in the cache");
Wr.PutText(Stdio.stdout,Wr.EOL & Wr.EOL);
Wr.Flush(Stdio.stdout);
  END EvaluateCacheSize;

PROCEDURE Init() =
  BEGIN
    n_targets     := 0;
    lib_name      := NIL;
    pgm_name      := NIL;
    info_name     := NIL;
    compile_failed:= FALSE;
    TimeNow       := Utils.M3Time( Time.Now() );

    warning_level := 3;
    warning_arg   := "-w3";
    make_mode     := FALSE;
    skip_std_lib  := FALSE;
    keep_files    := FALSE;
    dump_config   := FALSE;
    do_coverage   := FALSE;
    keep_resolved := FALSE;
    need_Rpath    := FALSE;
    supply_shared := FALSE; (* pass -B switches to linker ? *)
    shared_libs   := FALSE; (* a mode, flipped by parsing -B. *)
    new_linkInfo  := FALSE;
    bootstrap_mode:= FALSE;
    search_path   := FALSE;
    compile_once  := FALSE;
    gui           := FALSE;  (* for NT *)
    heap_stats    := FALSE;
    skip_link     := FALSE;
    has_loader    := FALSE;
    target_os     := M3Path.OSKind.Unix;
    host_os       := M3Path.OSKind.Unix;
    target_machine:= NIL;

    pass_0        := NewPass ("*UNDEFINED*");
    pass_1        := NewPass ("*UNDEFINED*");
    pass_2        := NewPass ("*UNDEFINED*");
    pass_3        := NewPass ("*UNDEFINED*");
    pass_4        := NewPass (NIL);
    pass_6        := NewPass (NIL);
    pass_7        := NewPass (NIL);
    pass_8        := NewPass (NIL);
    do_optimize   := FALSE;
    cc_optimize   := Arg.NewList ();
    cg_optimize   := Arg.NewList ();
    as_optimize   := Arg.NewList ();
    do_debug      := FALSE;
    cc_debug      := Arg.NewList ();
    cg_debug      := Arg.NewList ();
    as_debug      := Arg.NewList ();
    link_files    := "-z2@@";
    include_dir   := NIL;
    link_coverage := NIL;

    n_tfiles      := 0;
    include_path  := Arg.NewList ();
    lib_path      := Arg.NewList ();

    sources       := NIL;
    libraries     := NIL;
    pending_impls := NIL;

    interfaces := NEW (IntRefTbl.Default).init (100); (* intf name -> FileInfo*)
    modules    := NEW (IntRefTbl.Default).init (100); (* impl name -> FileInfo*)
    intf_dirs  := NEW (TextSet.Default).init (100);
    h_dirs     := NEW (TextSet.Default).init (100);
    link_base  := NIL;
    generic_intfs := NEW (IntRefTbl.Default).init (20); (* name -> full path *)
    generic_impls := NEW (IntRefTbl.Default).init (20); (* name -> full path *)

    magic     := NEW (IntRefTbl.Default).init (100); (* type name -> info *)

    long_names:= NIL;
  END Init;

(*------------------------------------------------------------------ main ---*)

PROCEDURE DoIt (options: TEXT)
  RAISES {Error} =
  BEGIN

    TRY
(**
RTCollectorSRC.gcRatio := 1.0;
RTCollectorSRC.incremental := TRUE;
**)

      ParseCommandLine (options);

      BuildSearchPaths ();

      ETimer.Push (M3Timers.localobj);
      Utils.NoteLocalFileTimes ();
      ETimer.Pop ();

(**     RTCollector.Disable (); **)
      InhaleLinkInfo ();
      BuildLibraryPool ();
(**     RTCollector.Enable (); **)

      CompileEverything ();

      DumpLinkInfo ();
      WebFile.Dump ();

      IF (pgm_name # NIL) THEN
        IF (bootstrap_mode) THEN
          BuildBootProgram ();
        ELSE
          BuildProgram ();
        END;
      ELSIF (lib_name # NIL) THEN
        IF (bootstrap_mode)
          THEN BuildBootLibrary ();
          ELSE BuildLibrary ();
        END;
      END;

      IF (compile_failed) THEN  RAISE Error;  END;

    FINALLY
      CleanUp();
    END;
  END DoIt;

(*------------------------------------------------------ finishing things ---*)

PROCEDURE CleanUp ()
  RAISES {Error} =
  BEGIN
    DumpLinkInfo ();
    WebFile.Dump ();
    IF (M3Timers.pass_0 # NIL) THEN
      IF (pass_6.cmd = NIL) AND (pass_7.cmd = NIL) THEN
        ETimer.Relabel (M3Timers.pass_0, "compiling Modula-3 -> object");
      ELSIF (pass_6.cmd = NIL) THEN
        ETimer.Relabel (M3Timers.pass_0, "compiling Modula-3 -> assembly");
      ELSIF (pass_7.cmd = NIL) THEN
        ETimer.Relabel (M3Timers.pass_6, "compiling IL -> object");
      END;
    END;
    M3Timers.Stop (Msg.writer);
    Msg.Out (); (* to flush the writer *)
    Utils.RemoveTempFiles ();
    IF (heap_stats) THEN
      RTutils.Heap (suppressZeros := TRUE,
                    presentation := RTutils.HeapPresentation.ByNumber);
      RTCollector.Collect ();
      RTutils.Heap (suppressZeros := TRUE,
                    presentation := RTutils.HeapPresentation.ByNumber);
    END;
  END CleanUp;

(*------------------------------------------------- command line parsing ---*)

PROCEDURE ParseCommandLine (options: TEXT)
  RAISES {Error} =
  VAR args := Arg.NewList ();
      i, j : INTEGER:= 0;
  BEGIN
    (* build the initial argument list *)
    options := options & " ";
    WHILE i <= Text.Length(options) - 1 DO
      IF Text.GetChar(options, i) = ' ' THEN
        IF i # j  THEN
          Arg.Append (args, Text.Sub(options, j, i-j));
        END;
        j:= i + 1;
      END;
      INC(i);
    END;

(*  FOR i := 1 TO Params.Count - 1 DO
      Arg.Append (args, Params.Get (i));
    END;*)

    (* parse the argument list *)
    ParseArgList (args);

    IF (NOT skip_std_lib) THEN
      (* add the standard libraries as arguments *)
      ParseArgList (GetChunks (link_files));
    END;

    IF (dump_config) THEN
      DumpConfiguration ();
      M3Timers.Stop (Msg.writer);
      IF keep_cache THEN
        Msg.Out("\nCan keep a cache of the interfaces with this option...\n");
      END;
      CleanUp();
      Process.Exit (0);

    ELSIF (n_targets = 0) THEN
      pgm_name := M3Path.DefaultProgram (host := FALSE);
    ELSIF (n_targets > 1) THEN
      Msg.UsageError ("Only one of -c, -o, -a, -C, -S can be specified");
    END;

    IF (target_machine = NIL) THEN
      Msg.FatalError (NIL, "target machine not specified");
    END;

    ReverseSources ();
  END ParseCommandLine;

PROCEDURE ParseFileArgs (file: TEXT)
  RAISES {Error} =
  VAR rd: Rd.T;  args := Arg.NewList ();
  BEGIN
    TRY
      rd := FileRd.Open (file);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to open argument file: ", file);
    END;

    TRY
      WHILE NOT Rd.EOF (rd) DO
        Arg.Append (args, Trim (Rd.GetLine (rd)));
      END;
      Rd.Close (rd);
    EXCEPT
    | Rd.Failure (args) =>
        Msg.FatalError (args, "unable to read file: ", file);
    | Rd.EndOfFile, Thread.Alerted =>
        Msg.FatalError (NIL, "unable to read file: ", file);
    END;

    ParseArgList (args);
  END ParseFileArgs;

PROCEDURE ParseArgList (list: Arg.List)
  RAISES {Error} =
  VAR len: INTEGER;  arg: TEXT;
  BEGIN
    WHILE (list.cnt > 0) DO
      arg := Arg.Pop (list);
      len := Text.Length (arg);
      IF (len < 1) THEN
        (* empty argument ignore *)
      ELSIF (Text.GetChar (arg, 0) # '-') OR (len < 2) THEN
        AddSourceFile (NIL, arg, cmd_line := TRUE);
      ELSE (* it's an option *)
        ParseOption (arg, len, list);
      END;
    END;
  END ParseArgList;

PROCEDURE ParseOption (arg: TEXT;  arg_len: INTEGER;  rest: Arg.List)
  RAISES {Error} =
  VAR ok := FALSE;
  BEGIN
    CASE Text.GetChar (arg, 1) OF

    | '?' => IF (arg_len = 2) THEN
               dump_config := TRUE;  ok := TRUE;
             END;

    | 'a' => IF (arg_len = 2) THEN
               lib_name := GetArg (arg, rest);  INC (n_targets);  ok := TRUE;
             END;

    | 'A' => IF (arg_len = 2) THEN
               Arg.Append (pass_0.args, "-NoAsserts");  ok := TRUE;
             END;

    | 'b' => IF Text.Equal (arg, "-boot") THEN
               bootstrap_mode := TRUE;  (*skip_std_lib := TRUE;*)  ok := TRUE;
             END;

    | 'B' => IF Text.Equal (arg, "-Bdynamic") THEN
               shared_libs := TRUE; ok := TRUE;
             ELSIF Text.Equal (arg, "-Bstatic") THEN
               shared_libs := FALSE; ok := TRUE;
             ELSE
               ok := FALSE;
             END;

    | 'c' => IF (arg_len = 2) THEN
               INC (n_targets);  pgm_name := NIL;  lib_name := NIL; ok := TRUE;
             ELSIF Text.Equal (arg, "-commands") THEN
               Msg.SetLevel (Msg.Level.Commands);  ok := TRUE;
             ELSIF Text.Equal (arg, "-config") THEN
               dump_config := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-console") THEN
               gui := FALSE;  ok := TRUE;
             END;

    | 'd' => IF Text.Equal (arg, "-debug") THEN
               Msg.SetLevel (Msg.Level.Debug);  M3Timers.Start ();  ok := TRUE;
             END;

    | 'D' => IF (arg_len = 2)
               THEN Unit.ResetPath ();  n_tfiles := 0;
               ELSE Unit.PushDir (Text.Sub (arg, 2, arg_len));
             END;
             ok := TRUE;

    | 'T' => INC (n_tfiles);
             IF (arg_len = 2)
               THEN Unit.ResetPath ();  n_tfiles := 0;
               ELSE Unit.PushTable (Text.Sub (arg, 2, arg_len));
             END;
             ok := TRUE;

    | 'F' => IF (arg_len > 2) THEN
               ParseFileArgs (Text.Sub (arg, 2, arg_len));
               ok := TRUE;
             END;

    | 'g' => IF (arg_len = 2) THEN
               do_debug := TRUE;
             ELSIF Text.Equal(arg, "-gui") THEN
               gui := TRUE;
             ELSE
               do_debug := TRUE;
               cc_debug := Arg.NewList ();
               cg_debug := Arg.NewList ();
               as_debug := Arg.NewList ();
               Arg.Append (cc_debug, arg);
               Arg.Append (cg_debug, arg);
               Arg.Append (as_debug, arg);
             END;
             ok := TRUE;

    | 'h' => IF Text.Equal (arg, "-heap_stats") THEN
               heap_stats := TRUE;  ok := TRUE;
             END;

    | 'k' => IF Text.Equal (arg, "-keep_cache") THEN
               keep_cache := TRUE;  ok := TRUE;
             ELSIF (arg_len = 2) OR Text.Equal (arg, "-keep") THEN
               keep_files := TRUE;  ok := TRUE;
             END;

    | 'L' => IF (arg_len = 2)
               THEN lib_path := Arg.NewList ();
               ELSE PushPath (lib_path, Text.Sub (arg, 2, arg_len));
             END;
             ok := TRUE;

    | 'l' => IF (arg_len > 2) THEN
               VAR name: M3Path.T; BEGIN
                 name.dir  := NIL;
                 name.base := Text.Sub (arg, 2, arg_len);
                 name.kind := NK.A;
                 AddLibrary (NIL, name);
                 ok := TRUE;
               END;
             END;

    | 'm' => IF Text.Equal (arg, "-make") THEN
               (** Msg.SetLevel (Msg.Level.Explain); **)
               make_mode := TRUE;  ok := TRUE;
             END;

    | 'n' => IF Text.Equal (arg, "-nostd") THEN
               skip_std_lib := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-noflatten") THEN
               search_path := TRUE;  ok := TRUE;
             END;

    | 'o' => IF (arg_len = 2) THEN
               pgm_name := GetArg (arg, rest);  INC (n_targets);  ok := TRUE;
             ELSIF Text.Equal (arg, "-once") THEN
               compile_once := TRUE;  ok := TRUE;
             END;

    | 'O' => IF (arg_len = 2) THEN
               do_optimize := TRUE;
             ELSE
               do_optimize := TRUE;
               cc_optimize := Arg.NewList ();
               cg_optimize := Arg.NewList ();
               as_optimize := Arg.NewList ();
               Arg.Append (cc_optimize, arg);
               Arg.Append (cg_optimize, arg);
               Arg.Append (as_optimize, arg);
             END;
             ok := TRUE;

    | 's' => IF Text.Equal (arg, "-silent") THEN
               Msg.SetLevel (Msg.Level.Silent);  ok := TRUE;
             ELSIF Text.Equal (arg, "-skiplink") THEN
               skip_link := TRUE;  ok := TRUE;
             END;

    | 't' => IF Text.Equal (arg, "-times") THEN
               M3Timers.Start ();  ok := TRUE;
             END;

    | 'v' => IF Text.Equal (arg, "-verbose") THEN
               Msg.SetLevel (Msg.Level.Verbose);
               M3Timers.Start ();
               ok := TRUE;
             ELSIF (arg_len = 2) THEN
               Msg.SetLevel (Msg.Level.Verbose);
               M3Timers.Start ();
               warning_level := 0;  warning_arg := "-w0";
               ok := TRUE;
             END;

    | 'w' => IF Text.Equal (arg, "-why") THEN
               Msg.SetLevel (Msg.Level.Explain);  ok := TRUE;
             ELSIF Text.Equal (arg, "-windows") THEN
               gui := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-w0") THEN
               SetWarning (0, arg);  ok := TRUE;
             ELSIF Text.Equal (arg, "-w1") THEN
               SetWarning (1, arg);  ok := TRUE;
             ELSIF Text.Equal (arg, "-w2") THEN
               SetWarning (2, arg);  ok := TRUE;
             ELSIF Text.Equal (arg, "-w3") THEN
               SetWarning (3, arg);  ok := TRUE;
             END;

    | 'X' => IF (arg_len > 3) THEN
               ok := TRUE;
               CASE Text.GetChar (arg, 2) OF
               | '0' => GetArgs (pass_0.args, arg);
               | '1' => GetArgs (pass_1.args, arg);
               | '2' => GetArgs (pass_2.args, arg);
               | '3' => GetArgs (pass_3.args, arg);
               | '4' => GetArgs (pass_4.args, arg);
               | '6' => GetArgs (pass_6.args, arg);
               | '7' => GetArgs (pass_7.args, arg);
               | '8' => GetArgs (pass_8.args, arg);
               ELSE (*error*) ok := FALSE;
               END;
             END;

    | 'Y' => IF (arg_len > 3) THEN
               ok := TRUE;
               CASE Text.GetChar (arg, 2) OF
               | '0' => GetPass (pass_0, arg);
               | '1' => GetPass (pass_1, arg);
               | '2' => GetPass (pass_2, arg);
               | '3' => GetPass (pass_3, arg);
               | '4' => GetPass (pass_4, arg);
               | '5' => (* silently ignore the option *)
               | '6' => GetPass (pass_6, arg);
               | '7' => GetPass (pass_7, arg);
               | '8' => GetPass (pass_8, arg);
               ELSE (*error*) ok := FALSE;
               END;
             END;

    | 'Z' => IF (arg_len = 2) THEN
               Arg.Append (pass_0.args, arg);  ok := TRUE;
               do_coverage := TRUE;
             END;

    | 'z' => IF (arg_len > 3) THEN
               ok := TRUE;
               CASE Text.GetChar (arg, 2) OF
               | '0' => (* silently ignore the option *)
               | '1' => (* silently ignore the option *)
               | '2' => link_files    := arg;
               | '3' => link_coverage := Text.Sub (arg, 3, arg_len);
               | '4' => include_dir   := Text.Sub (arg, 3, arg_len);
               | '5' => SetHostOS (Text.GetChar (arg, 3));
               | '6' => keep_resolved := (Text.GetChar (arg, 3) = '1');
                        need_Rpath    := (Text.GetChar (arg, 3) = '2');
               | '7' => (* silently ignore the option *)
               | '8' => (* silently ignore the option *)
               | '9' => (* silently ignore the option *)
               | 'A' => (* silently ignore the option *)
               | 'B' => cc_optimize   := GetChunks (arg);
               | 'C' => cg_optimize   := GetChunks (arg);
               | 'D' => as_optimize   := GetChunks (arg);
               | 'E' => cc_debug      := GetChunks (arg);
               | 'F' => cg_debug      := GetChunks (arg);
               | 'G' => as_debug      := GetChunks (arg);
               | 'H' => SetTargetOS (Text.GetChar (arg, 3));
               | 'I' => SetTarget (Text.Sub (arg, 3, arg_len));
               | 'J' => SetNoise (ORD(Text.GetChar (arg, 3)) - ORD('0'),
                                  (Text.GetChar (arg, 4) # '0'));
               | 'K' => supply_shared := (Text.GetChar (arg, 3) # '0');
               | 'L' => has_loader    := (Text.GetChar (arg, 3) # '0');
               ELSE (*error*) ok := FALSE;
               END;
             END;

    ELSE (* error *)
    END;

    IF (NOT ok) THEN Msg.UsageError ("unrecognized option \'", arg, "\'") END;
  END ParseOption;

PROCEDURE GetArg (arg: TEXT;  rest: Arg.List): TEXT
  RAISES {Error} =
  BEGIN
    IF (rest.cnt <= 0) THEN
      Msg.UsageError ("missing argument to \'", arg, "\' option");
    END;
    RETURN Arg.Pop (rest);
  END GetArg;

PROCEDURE SetNoise (pass: INTEGER;  noisy: BOOLEAN)
  RAISES {Error} =
  BEGIN
    CASE pass OF
    | 0 => SetPassNoise (pass_0, noisy, ".out0");
    | 1 => SetPassNoise (pass_1, noisy, ".out1");
    | 2 => SetPassNoise (pass_2, noisy, ".out2");
    | 3 => SetPassNoise (pass_3, noisy, ".out3");
    | 4 => SetPassNoise (pass_4, noisy, ".out4");
    | 6 => SetPassNoise (pass_6, noisy, ".out6");
    | 7 => SetPassNoise (pass_7, noisy, ".out7");
    | 8 => SetPassNoise (pass_8, noisy, ".out8");
    ELSE Msg.FatalError (NIL, "bad -zJ configuration");
    END;
  END SetNoise;

PROCEDURE SetPassNoise (VAR p: PassInfo;  noisy: BOOLEAN;  ext: TEXT) =
  BEGIN
    IF noisy
      THEN p.noise := ext;
      ELSE p.noise := NIL;
    END;
  END SetPassNoise;

PROCEDURE SetTarget (tar: TEXT)
  RAISES {Error} =
  BEGIN
    IF NOT Target.Init (tar) THEN
      Msg.FatalError (NIL, "unrecognized target machine: ", tar);
    END;
    target_machine := tar;
  END SetTarget;

PROCEDURE SetHostOS (ch: CHAR)
  RAISES {Error} =
  VAR os := GetOSType (ch);
  BEGIN
    M3Path.SetOS (os, host := TRUE);
    host_os   := os;
  END SetHostOS;

PROCEDURE SetTargetOS (ch: CHAR)
  RAISES {Error} =
  VAR os := GetOSType (ch);
  BEGIN
    M3Path.SetOS (os, host := FALSE);
    target_os := os;
  END SetTargetOS;

PROCEDURE GetOSType (ch: CHAR): M3Path.OSKind
  RAISES {Error} =
  BEGIN
    IF    (ch = '0')   THEN RETURN M3Path.OSKind.Unix;
    ELSIF (ch = '1')   THEN RETURN M3Path.OSKind.GrumpyUnix;
    ELSIF (ch = '2')   THEN RETURN M3Path.OSKind.Win32;
    END;
    Msg.FatalError (NIL, "unrecognized os type: ", Text.FromChar (ch));
    RETURN M3Path.OSKind.Unix;
  END GetOSType;

PROCEDURE SetWarning (level: INTEGER;  arg: TEXT) =
  BEGIN
    IF (level < warning_level) THEN
      warning_level := level;
      warning_arg   := arg;
    END;
  END SetWarning;

PROCEDURE GetPass (VAR(*OUT*) pass: PassInfo;  value: TEXT)
  RAISES {Error} =
  VAR list := GetChunks (value);  n: Arg.T;
  BEGIN
    (* reset the pass *)
    pass.cmd       := NIL;
    pass.args.head := NIL;
    pass.args.tail := NIL;
    pass.args.cnt  := 0;

    IF (list.cnt > 0) THEN
      pass.cmd := list.head.arg;
      n := list.head.next;
      WHILE (n # NIL) DO Arg.Append (pass.args, n.arg);  n := n.next END;
    END;
   END GetPass;

PROCEDURE GetArgs (args: Arg.List;  value: TEXT)
  RAISES {Error} =
  BEGIN
    Arg.AppendL (args, GetChunks (value));
  END GetArgs;

PROCEDURE GetChunks (value: TEXT): Arg.List
  RAISES {Error} =
  (* extract the Ai in '-Xn/A1/A2/.../An/' *)
  VAR
    i, j: INTEGER;
    len := Text.Length (value);
    dot: CHAR;
    result := Arg.NewList ();
  BEGIN
    IF (len < 5) THEN
      Msg.FatalError (NIL, "improperly formatted argument: \"", value, "\"");
    END;
    dot := Text.GetChar (value, 3);
    IF Text.GetChar (value, len-1) # dot THEN
      Msg.FatalError (NIL, "improperly formatted argument: \"", value, "\"");
    END;
    j := 4;
    WHILE (j < len) DO
      i := j;
      WHILE (j < len) AND Text.GetChar (value, j) # dot DO INC (j) END;
      IF (i < j) THEN Arg.Append (result, Text.Sub (value, i, (j-i))); END;
      INC (j);
    END;
    RETURN result;
  END GetChunks;

PROCEDURE PushPath (path: Arg.List;  new: TEXT) =
  CONST Sep = ARRAY M3Path.OSKind OF CHAR { ':', ':', '+' };
  VAR x := Text.Length (new)-1;  y: INTEGER;  sep := Sep [host_os];
  BEGIN
    WHILE (x >= 0) DO
      y := x;
      WHILE (x >= 0) AND (Text.GetChar (new, x) # sep) DO DEC (x) END;
      IF (x < y) THEN Arg.Prepend (path, Text.Sub (new, x+1, y-x)) END;
      DEC (x);
    END;
  END PushPath;

PROCEDURE Trim (t: TEXT): TEXT =
  VAR start := 0;  len := Text.Length (t);
  BEGIN
    WHILE (len > 0) AND (Text.GetChar (t, start) = ' ') DO
      INC (start);
      DEC (len);
    END;
    WHILE (len > 0) AND (Text.GetChar (t, start+len-1) = ' ') DO
      DEC (len);
    END;
    RETURN Text.Sub (t, start, len);
  END Trim;

(*--------------------------------------------------------- help/config ---*)

PROCEDURE DumpConfiguration ()
  RAISES {Error} =
  CONST Bool = ARRAY BOOLEAN OF TEXT { "FALSE", "TRUE" };
  BEGIN
    Msg.Out  ("target       := ", target_machine, Wr.EOL);
    Msg.OutL ("pass 0       := ", "m3c", pass_0.args);
    Msg.OutL ("pass 1       := ", pass_1.cmd, pass_1.args);
    Msg.OutL ("pass 2       := ", pass_2.cmd, pass_2.args);
    Msg.OutL ("pass 3       := ", pass_3.cmd, pass_3.args);
    IF (pass_4.cmd # NIL) THEN
      Msg.OutL ("pass 4       := ", pass_4.cmd, pass_4.args);
    END;
    IF (pass_6.cmd # NIL) THEN
      Msg.OutL ("pass 6       := ", pass_6.cmd, pass_6.args);
    END;
    IF (pass_7.cmd # NIL) THEN
      Msg.OutL ("pass 7       := ", pass_7.cmd, pass_7.args);
    END;
    IF (pass_8.cmd # NIL) THEN
      Msg.OutL ("pass 8       := ", pass_8.cmd, pass_8.args);
    END;
    Msg.OutL ("lib path     := ", NIL, lib_path);
    Msg.Out  ("include      := ", include_dir, Wr.EOL);
    Msg.Out  ("make mode    := ", Bool [make_mode], Wr.EOL);
    Msg.Out  ("bootstrap    := ", Bool [bootstrap_mode], Wr.EOL); 
    Msg.Out  ("std libs     := ", Bool [NOT skip_std_lib]);
    Msg.OutL (" ",                NIL, GetChunks (link_files));
    Msg.Out  ("keep files   := ", Bool [keep_files], Wr.EOL);
    Msg.Out  ("coverage     := ", Bool [do_coverage]," ",link_coverage,Wr.EOL);
    Msg.Out  ("resolve libs := ", Bool [keep_resolved], Wr.EOL);
    Msg.Out  ("need -R path := ", Bool [need_Rpath], Wr.EOL);
    Msg.OutL ("-O [cc]      => ", NIL, cc_optimize);
    Msg.OutL ("-O [cg]      => ", NIL, cg_optimize);
    Msg.OutL ("-O [as]      => ", NIL, as_optimize);
    Msg.OutL ("-g [cc]      => ", NIL, cc_debug);
    Msg.OutL ("-g [cg]      => ", NIL, cg_debug);
    Msg.OutL ("-g [as]      => ", NIL, as_debug);
  END DumpConfiguration;

(*----------------------------------------------------------- libraries ---*)

PROCEDURE AddLibrary (file: TEXT;  READONLY name: M3Path.T) =
  VAR f := NewSource (file, name);
  BEGIN
    IF (file # NIL)
      THEN f.library := file;
      ELSE f.library := ResolveLib (f.name);
    END;
    f.shared := shared_libs;
    f.next := libraries;
    libraries := f;
  END AddLibrary;

PROCEDURE ResolveLib (VAR name: M3Path.T): TEXT =
  VAR resolution: TEXT;  a := lib_path.head;
  BEGIN
    WHILE (a # NIL) DO
      resolution := M3Path.Join (a.arg, name.base, name.kind, host := TRUE);
      IF (Utils.ModificationTime (resolution) # Utils.NO_TIME) THEN
        Msg.Verbose ("resolve: ", name.base, " -> ", resolution);
        name.dir := a.arg;
        RETURN resolution;
      END;
      resolution := M3Path.Join (a.arg, name.base, NK.AX, host := TRUE);
      IF (Utils.ModificationTime (resolution) # Utils.NO_TIME) THEN
        Msg.Verbose ("resolve: ", name.base, " -> ", resolution);
        name.dir := a.arg;
        RETURN M3Path.Join (a.arg, name.base, NK.A, host := TRUE);
      END;
      a := a.next;
    END;
    name.dir := NIL;
    resolution := "-l" & name.base;
    Msg.Verbose ("resolve: ", name.base, " -> ", resolution);
    RETURN resolution;
  END ResolveLib;

(*-------------------------------------------------------- source files ---*)

PROCEDURE AddSourceFile (dir, name: TEXT;  cmd_line := FALSE)
  RAISES {Error} =
  VAR
    file := M3Path.Join (dir, name, NK.Unknown, host := TRUE);
    info := M3Path.Parse (file, host := TRUE);
  BEGIN
    CASE info.kind OF
    | NK.I3, NK.IC, NK.IS, NK.IO => AddInterfaceSource (file, info);
    | NK.M3, NK.MC, NK.MS, NK.MO => AddModuleSource (file, info);
    | NK.IG, NK.MG               => AddGeneric (file, info);
    | NK.C, NK.H, NK.S, NK.O     => AddSource (file, info);
    | NK.A                       => AddLibrary (file, info);
    | NK.AX, NK.PX               => AddLinkInfo (file, info);
    ELSE                            VisitSourceDir (file, cmd_line);
    END;
  END AddSourceFile;

PROCEDURE NewSource (file: TEXT;  READONLY name: M3Path.T): FileInfo =
  BEGIN
    IF (file # NIL) THEN Msg.Verbose ("using ", file); END;
    RETURN NEW (FileInfo, source := file, name := name);
  END NewSource;

PROCEDURE AddLinkInfo (file: TEXT;  READONLY name: M3Path.T) =
  VAR f := NewSource (file, name);
  BEGIN
    f.next := sources;  sources := f;
  END AddLinkInfo;

PROCEDURE AddInterfaceSource (file: TEXT;  READONLY name: M3Path.T)
  RAISES {Error} =
  VAR f := NewSource (file, name);
  BEGIN
    f.next := sources;  sources := f;
    AddInterface (f);
  END AddInterfaceSource;

PROCEDURE AddInterface (f: FileInfo)
  RAISES {Error} =
  VAR ref: REFANY;  old: FileInfo;  id := M3ID.Add (f.name.base);
  BEGIN
    IF NOT interfaces.get (id, ref) THEN
      EVAL interfaces.put (id, f);
      IF (f.source # NIL) THEN
        IF (f.name.dir # NIL)
          THEN EVAL intf_dirs.put (f.name.dir, void);
          ELSE EVAL intf_dirs.put (".", void);
        END;
      END;
    ELSE
      old := ref;
      IF (old.source # NIL) THEN
        IF (f.source = NIL) OR NOT Text.Equal (f.source, old.source) THEN
           Duplicate (old, f, "interface");
        END;
      ELSIF (old.link_info # NIL) THEN
        Duplicate (old, f, "interface");
      ELSE
        old.library      := f.library;
        old.source       := f.source;
        old.link_info    := f.link_info;
        old.link_info_ts := f.link_info_ts;
      END;
    END;
  END AddInterface;

PROCEDURE AddModuleSource (file: TEXT;  READONLY name: M3Path.T)
  RAISES {Error} =
  VAR f := NewSource (file, name);
  BEGIN
    f.next := sources;  sources := f;
    AddModule (f);
  END AddModuleSource;

PROCEDURE AddModule (f: FileInfo)
  RAISES {Error} =
  VAR ref: REFANY;  old: FileInfo;  id := M3ID.Add (f.name.base);
  BEGIN
    IF NOT modules.get (id, ref) THEN
      EVAL modules.put (id, f);
    ELSE
      old := ref;
      IF (old.source # NIL) THEN
        IF (f.source = NIL) OR NOT Text.Equal (f.source, old.source) THEN
          Duplicate (old, f, "module");
        END;
      ELSIF (old.link_info # NIL) THEN
        Duplicate (old, f, "module");
      ELSE
        old.library      := f.library;
        old.source       := f.source;
        old.link_info    := f.link_info;
        old.link_info_ts := f.link_info_ts;
      END;
    END;
  END AddModule;

PROCEDURE AddGeneric (file: TEXT;  READONLY name: M3Path.T) =
  VAR id := M3ID.Add (name.base);
  BEGIN
    Msg.Verbose ("using ", file);
    IF (name.kind = NK.IG)
      THEN EVAL generic_intfs.put (id, file);
      ELSE EVAL generic_impls.put (id, file);
    END;
    IF (name.dir # NIL)
      THEN EVAL intf_dirs.put (name.dir, void);
      ELSE EVAL intf_dirs.put (".", void);
    END;
  END AddGeneric;

PROCEDURE AddSource (file: TEXT;  READONLY name: M3Path.T) =
  VAR f := NewSource (file, name);
  BEGIN
    f.next := sources;  sources := f;
    IF (name.kind = NK.H) THEN
      IF (name.dir # NIL)
        THEN EVAL h_dirs.put (name.dir, void);
        ELSE EVAL h_dirs.put (".", void);
      END;
    END;
  END AddSource;

PROCEDURE VisitSourceDir (dir: TEXT;  cmd_line: BOOLEAN)
  RAISES {Error} =
  BEGIN
    TRY
      VAR
        iter := FS.Iterate (dir);
        name: TEXT;
      BEGIN
        Msg.Verbose ("Looking in ", dir);
        TRY
          WHILE iter.next(name) DO
            AddSourceFile (dir, name, cmd_line := FALSE);
          END
        FINALLY
          iter.close();
        END;
      END;
    EXCEPT
    | OSError.E (args) =>
        IF (cmd_line) THEN
          Msg.FatalError (args, "unsupported file type \"", dir, "\"");
        END;
        Msg.Verbose ("ignoring ", dir, Msg.OSErr (args));
    END;
  END VisitSourceDir;

PROCEDURE ReverseSources () =
  VAR a, b, c: FileInfo;
  BEGIN
    (* reverse the linked list of source files *)
    a := sources;  b := NIL;
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    sources := b;

    (* reverse the linked list of libraries *)
    a := libraries;  b := NIL;
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    libraries := b;
  END ReverseSources;

(*----------------------------------------------------------- search path ---*)

PROCEDURE BuildSearchPaths () =
  VAR reverse_include_path := Arg.NewList ();  n: Arg.T;
  BEGIN
    (* build the Modula-3 search path *)
    IF (n_tfiles = 0) THEN
      VAR iter := intf_dirs.iterate (); dir: TEXT;
      BEGIN
        WHILE iter.next (dir, void) DO
          IF NOT Text.Empty (dir) THEN Unit.PushDir (dir); END;
        END;
      END;
    END;
    (* build the include path *)
    VAR iter := h_dirs.iterate(); dir: TEXT;
    BEGIN
      WHILE iter.next(dir, void) DO
        IF NOT Text.Empty (dir) THEN Arg.Prepend (include_path, dir) END;
      END;
    END;
    IF (include_dir # NIL) AND NOT Text.Empty (include_dir) THEN 
      Arg.Prepend (include_path, include_dir);
    END;

    n := include_path.head;
    WHILE (n # NIL) DO
      Arg.Prepend (reverse_include_path, "-I" & n.arg);
      n := n.next; 
    END;
    Arg.AppendL (pass_1.args, reverse_include_path);
  END BuildSearchPaths;

(*------------------------------------------------------- local link info ---*)

PROCEDURE InhaleLinkInfo ()
  RAISES {Error} =
  VAR file: TEXT;  name: M3Path.T;  ux: Mx.UnitList;  kind: NK;
      info_time := LAST(INTEGER);
  BEGIN
    ETimer.Push (M3Timers.inhale);

    (* figure out what file to read... *)
    IF    (pgm_name # NIL) THEN file := pgm_name;  kind := NK.PX;
    ELSIF (lib_name # NIL) THEN file := lib_name;  kind := NK.AX;
    ELSE  Msg.FatalError (NIL, "cannot locate version stamps: ",
                         "no program or library specified");
    END;
    name := M3Path.Parse (file, host := TRUE);
    info_name := M3Path.Join (NIL, name.base, kind, host := TRUE);

    Msg.Commands ("inhale ", info_name);
    ux := GetLinkUnits (info_name, NIL, imported := FALSE);
    IF (ux = NIL) THEN
      Msg.Debug ("no local link info", Wr.EOL);
    ELSE
      IF keep_cache THEN
        info_time := Utils.NoteModification(info_name);
      END;

      Msg.Debug ("adding units: ");
      WHILE (ux # NIL) DO
        MatchLocalUnit (ux.unit, info_time);
        ux := ux.next;
      END;
      Msg.Debug (Wr.EOL);
    END;

    new_linkInfo := FALSE;
    ETimer.Pop ();
  END InhaleLinkInfo;

PROCEDURE MatchLocalUnit (u: Mx.Unit; info_time: INTEGER)
  RAISES {Error} =
  VAR map: IntRefTbl.T;  suffix: TEXT;  f: FileInfo;  ref: REFANY;
  BEGIN
    IF (u = NIL) THEN RETURN END;
    IF (u.interface)
      THEN map := interfaces;  suffix := ".i3";
      ELSE map := modules;     suffix := ".m3";
    END;
    IF map.get (u.name, ref) THEN
      f := ref;
      Msg.Debug (" ", f.name.base, suffix);
      IF (f.link_info # NIL) THEN
        Msg.FatalError (NIL, "duplicate link info for ", FName (f));
      END;
      f.link_info := u;
      IF (u.file # NIL) AND (u.file.name = NIL) THEN
        u.file.name := f.source;
      END;
      f.link_info_ts := info_time;
    ELSE (* no source to match this unit *)
      Msg.Debug (" <", M3ID.ToText (u.name), suffix, ">");
    END;
  END MatchLocalUnit;

PROCEDURE DumpLinkInfo ()
  RAISES {Error} =
  VAR f := sources;  units: Mx.UnitList := NIL;  wr: Wr.T;
  BEGIN
    IF NOT new_linkInfo THEN RETURN END;
    new_linkInfo := FALSE;  (* in case we die writing the info *)
    ETimer.Push (M3Timers.exhale);

    (* build a list of the local units *)
    WHILE (f # NIL) DO
      IF (f.link_info # NIL) THEN
        units := NEW (Mx.UnitList, unit := f.link_info, next := units);
      END;
      f := f.next;
    END;

    (* and write them *)
    Msg.Commands ("exhale ", info_name);
    wr := Utils.OpenWriter (info_name, fatal := TRUE);
    MxOut.WriteUnits (units, wr);
    Utils.CloseWriter (wr, info_name);

    ETimer.Pop ();
  END DumpLinkInfo;

(*---------------------------------------------------------- library pool ---*)

PROCEDURE BuildLibraryPool ()
  RAISES {Error} =
  VAR a := libraries;  ux: Mx.UnitList;  info: TEXT;
      info_time := LAST(INTEGER);
  BEGIN
    WHILE (a # NIL) DO
      ETimer.Push (M3Timers.inhale);
      Msg.Commands ("inhale ", a.library);
      info := M3Path.Join (a.name.dir, a.name.base, NK.AX, host := TRUE);
      ux := GetLinkUnits (info, a.library, imported := TRUE);
      IF (ux = NIL) THEN
        Msg.Debug ("no link info for ", a.library, Wr.EOL);
      ELSE
        IF keep_cache THEN
          info_time := Utils.NoteModification(info);
        END;

        Msg.Debug ("adding units: ");
        WHILE (ux # NIL) DO
          AddLibraryUnit (ux.unit, a.library, info_time);
          ux := ux.next;
        END;
        Msg.Debug (Wr.EOL);
      END;
      ETimer.Pop ();
      a := a.next;
    END;
  END BuildLibraryPool;

PROCEDURE AddLibraryUnit (u: Mx.Unit;  lib: TEXT;  info_time: INTEGER)
  RAISES {Error} =
  CONST suffix = ARRAY BOOLEAN OF TEXT {".m3", ".i3"};
  VAR
    nm := M3ID.ToText (u.name);
    f  := NEW (FileInfo, library := lib, link_info := u);
  BEGIN
    Msg.Debug (" ", nm, suffix[u.interface]);
    f.link_info_ts := info_time;
    f.source       := NIL;
    f.name.dir     := NIL;
    f.name.base    := nm;
    IF u.interface THEN
      f.name.kind := NK.I3;
      AddInterface (f);
    ELSE
      f.name.kind := NK.M3;
      AddModule (f);
      FOR i := u.exported_units.start
            TO u.exported_units.start + u.exported_units.cnt - 1 DO
        AddExportHook (u.info [i], f);
      END;
    END;
  END AddLibraryUnit;

PROCEDURE AddExportHook (name: M3ID.T;  f: FileInfo) =
  VAR ref: REFANY;  intf: FileInfo;  z := NEW (FileList, file := f);
  BEGIN
    IF interfaces.get (name, ref) THEN
      intf := ref;
    ELSE
      intf := NEW (FileInfo);
      intf.name.dir  := NIL;
      intf.name.base := M3ID.ToText (name);
      intf.name.kind := NK.I3;
      EVAL interfaces.put (name, intf);
    END;
    z.next := intf.exporters;
    intf.exporters := z;
  END AddExportHook;

(*------------------------------------------------------------ compilation --*)

PROCEDURE CompileEverything ()
  RAISES {Error} =
  VAR f: FileInfo;  ref: REFANY;
  BEGIN
    link_base := Mx.NewSet ();
    IF interfaces.get (M3ID.Add (Mx.BuiltinUnitName), ref) THEN
      CompileOne (ref);
    END;

    (* compile all the non-module sources once *)
    f := sources;
    WHILE (f # NIL) DO
      IF (f.name.kind # NK.M3) THEN CompileOne (f) END;
      f := f.next;
    END;

    (* compile all the module sources once *)
    f := sources;
    WHILE (f # NIL) DO
      IF (f.name.kind = NK.M3) THEN CompileOne (f) END;
      f := f.next;
    END;
    FlushPending ();

    IF NOT compile_once THEN
      (* recompile those that could use the new opaque object information *)
      f := sources;
      WHILE (f # NIL) DO
        IF CouldBeImproved (f) THEN RecompileM3 (f) END;
        f := f.next;
      END;
    END;
    FlushPending ();
  END CompileEverything;

PROCEDURE CompileOne (f: FileInfo)
  RAISES {Error} =
  VAR x: FileList;
  BEGIN
    IF (f.compiling) THEN RETURN; END;
    f.compiling := TRUE;
    VerboseF ("checking ", f);

    IF (f.name.kind = NK.AX) THEN
      FlushPending ();
      CompileAX (f);
    ELSIF (f.source # NIL) THEN
      FlushPending ();
      f.object := ObjectName (f);
      CASE f.name.kind OF
      | NK.I3, NK.M3       => CompileM3 (f);
      | NK.IC, NK.MC, NK.C => CompileC (f);
      | NK.IS, NK.MS, NK.S => CompileS (f);
      | NK.IO, NK.MO, NK.O => CompileO (f);
      | NK.H               => CompileH (f);
      ELSE BadFile ("unexpected source unit", f);
      END;
    ELSIF (f.link_info # NIL) THEN
      IF (f.library = NIL) THEN
        BadFile ("non-library unit without source", f);
      END;
      IF (f.link_info.interface) THEN
        Merge (f);
      ELSE (* defer this guy as long as possible *)
        <*ASSERT f.next = NIL *>
        f.next := pending_impls;
        pending_impls := f;
      END;
    ELSE
      BadFile ("missing source file", f);
    END;

    (* unmark any pending exporters & compile the top-level ones *)
    x := f.exporters;
    WHILE (x # NIL) DO
      CompileOne (x.file);
      x := x.next;
    END;
  END CompileOne;

PROCEDURE FlushPending ()
  RAISES {Error} =
  VAR f: FileInfo;
  BEGIN
    WHILE (pending_impls # NIL) DO
      f := pending_impls;
      pending_impls := f.next;
      f.next := NIL;
      Merge (f);
    END;
  END FlushPending;

PROCEDURE CompileAX (f: FileInfo)
  RAISES {Error} =
  VAR units: Mx.UnitList;
  BEGIN
    IF (f.link_info = NIL) THEN
      DebugF ("reading link info from ", f);
      units := GetLinkUnits (f.source, f.source, imported := FALSE);
      IF (units = NIL) THEN BadFile ("missing link info", f); END;
      f.link_info := units.unit;
      <*ASSERT units.next = NIL*>
    END;
    Merge (f);
  END CompileAX;

PROCEDURE CompileO (f: FileInfo)
  RAISES {Error} =
  BEGIN
    IF (f.name.kind # NK.O) THEN Merge (f) END;

    IF bootstrap_mode THEN
      Msg.Explain ("new object -> copying ", f.object);
      PullForBootstrap (f, text_file := FALSE);
    END;
    EVAL Utils.NoteModification (f.object);
  END CompileO;

PROCEDURE CompileS (f: FileInfo)
  RAISES {Error} =
  BEGIN
    IF (f.name.kind # NK.S) THEN Merge (f) END;

    IF (f.object = NIL) OR Text.Equal (f.object, f.source) THEN
      (* already done *)
      EVAL Utils.NoteModification (f.object);
    ELSIF NOT ObjectIsStale (f) THEN
      (* already done *)
    ELSIF bootstrap_mode THEN
      PullForBootstrap (f, text_file := TRUE);
      EVAL Utils.NoteModification (f.object);
    ELSIF (f.name.kind = NK.S) THEN
      Pass1 (f.source, f.object, f.name.base);
      Utils.NoteNewFile (f.object);
    ELSE (* NK.IS or NK.MS *)
      IF  Pass7 (f.source, f.object, f.name.base)
      AND Pass8 (f.source, f.object, f.name.base) THEN
      END;
      Utils.NoteNewFile (f.object);
    END;
  END CompileS;

PROCEDURE CompileC (f: FileInfo)
  RAISES {Error} =
  VAR tmpS: TEXT;
  BEGIN
    IF (f.name.kind # NK.C) THEN Merge (f) END;

    IF (f.object = NIL) OR Text.Equal (f.object, f.source) THEN
      (* already done *)
      EVAL Utils.NoteModification (f.object);
    ELSIF NOT ObjectIsStale (f) THEN
      (* already done *)
    ELSIF (f.name.kind = NK.C) THEN
      IF (bootstrap_mode)
        THEN PullForBootstrap (f, text_file := TRUE);
        ELSE Pass1 (f.source, f.object, f.name.base);
      END;
      Utils.NoteNewFile (f.object);
    ELSIF (pass_6.cmd = NIL) THEN
      Msg.FatalError (NIL, "this compiler cannot compile .ic or .mc files");
    ELSIF bootstrap_mode THEN
      EVAL Pass6 (f.source, f.object, f.name.base);
      Utils.NoteNewFile (f.object);
    ELSE (* NK.IC or NK.MC *)
      tmpS := TempSName (f);
      IF (NOT keep_files) THEN Utils.NoteTempFile (tmpS) END;
      IF  Pass6 (f.source, tmpS, f.name.base)
      AND Pass7 (tmpS, f.object, f.name.base)
      AND Pass8 (tmpS, f.object, f.name.base) THEN
      END;
      IF (NOT keep_files) THEN Utils.Remove (tmpS) END;
      Utils.NoteNewFile (f.object);
    END;
  END CompileC;

PROCEDURE CompileH (f: FileInfo)
  RAISES {Error} =
  BEGIN
    IF NOT bootstrap_mode THEN
      (* already done *)
    ELSIF (f.object = NIL) OR Text.Equal (f.object, f.source) THEN
      (* already done *)
      EVAL Utils.NoteModification (f.object);
    ELSIF NOT ObjectIsStale (f) THEN
      (* already done *)
    ELSE
      PullForBootstrap (f, text_file := TRUE);
      EVAL Utils.NoteModification (f.object);
    END;
  END CompileH;

PROCEDURE CompileM3 (f: FileInfo)
  RAISES {Error} =
  BEGIN
    IF (f.library # NIL) THEN
      <*ASSERT f.link_info # NIL*>
      DebugF ("compile ", f, " -> from library");
      Merge (f);
    ELSIF (f.object = NIL) OR Text.Equal (f.object, f.source) THEN
      (* already done *)
      EVAL Utils.NoteModification (f.object);
      DebugF ("compile ", f, " -> object = source");
      RETURN;
    ELSIF NOT M3isStale (f) THEN
      (* already done *)
      DebugF ("compile ", f, " -> not stale");
      RETURN;
    ELSIF PushOneM3 (f) THEN
      Merge (f);
    END;
  END CompileM3;

PROCEDURE PushOneM3 (f: FileInfo): BOOLEAN
  RAISES {Error} =
  VAR tmpC, tmpS: TEXT;  need_merge := FALSE;  plan: [0..7] := 0;
  BEGIN
    f.link_info := NIL;

    IF (pass_7.cmd # NIL) THEN INC (plan, 1) END;
    IF (pass_6.cmd # NIL) THEN INC (plan, 2) END;
    IF (bootstrap_mode)   THEN INC (plan, 4) END;

    CASE plan OF
    | 0,    (* -bootstrap, -pass 6, -pass 7 *)
      4,    (* +bootstrap, -pass 6, -pass 7 *)
      5 =>  (* +bootstrap, -pass 6, +pass 7 *)
        IF Pass0 (f, f.object) THEN
          need_merge := TRUE;
        ELSE
          IF (NOT keep_files) THEN Utils.Remove (f.object) END;
        END;

    | 1 =>  (* -bootstrap, -pass 6, +pass 7 *)
        tmpS := TempSName (f);
        IF (NOT keep_files) THEN Utils.NoteTempFile (tmpS) END;
        IF Pass0 (f, tmpS) THEN
          IF  Pass7 (tmpS, f.object, f.name.base)
          AND Pass8 (tmpS, f.object, f.name.base) THEN
          END;
          need_merge := TRUE;
        END;
        IF (NOT keep_files) THEN Utils.Remove (tmpS) END;

    | 2 =>  (* -bootstrap, +pass 6, -pass 7 *)
        tmpC := TempCName (f);
        IF (NOT keep_files) THEN Utils.NoteTempFile (tmpC) END;
        IF Pass0 (f, tmpC) THEN
          EVAL Pass6 (tmpC, f.object, f.name.base);
          need_merge := TRUE;
        END;
        IF (NOT keep_files) THEN Utils.Remove (tmpC) END;


    | 3 =>  (* -bootstrap, +pass 6, +pass 7 *)
        tmpC := TempCName (f);
        tmpS := TempSName (f);
        IF (NOT keep_files) THEN Utils.NoteTempFile (tmpC) END;
        IF (NOT keep_files) THEN Utils.NoteTempFile (tmpS) END;
        IF Pass0 (f, tmpC) THEN
          IF  Pass6 (tmpC, tmpS, f.name.base)
          AND Pass7 (tmpS, f.object, f.name.base)
          AND Pass8 (tmpS, f.object, f.name.base) THEN
          END;
          need_merge := TRUE;
        END;
        IF (NOT keep_files) THEN Utils.Remove (tmpC) END;
        IF (NOT keep_files) THEN Utils.Remove (tmpS) END;

    | 6,    (* +bootstrap, +pass 6, -pass 7 *)
      7 =>  (* +bootstrap, +pass 6, +pass 7 *)
        tmpC := TempCName (f);
        IF (NOT keep_files) THEN Utils.NoteTempFile (tmpC) END;
        IF Pass0 (f, tmpC) THEN
          EVAL Pass6 (tmpC, f.object, f.name.base);
          need_merge := TRUE;
        END;
        IF (NOT keep_files) THEN Utils.Remove (tmpC) END;
    END; (* CASE plan *)
    Utils.NoteNewFile (f.object);

    RETURN need_merge;
  END PushOneM3;

PROCEDURE RecompileM3 (f: FileInfo)
  RAISES {Error} =
  BEGIN
    IF PushOneM3 (f) THEN Remerge (f) END;
  END RecompileM3;

PROCEDURE CouldBeImproved (f: FileInfo): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    IF (f.library # NIL) OR (f.link_info = NIL) THEN
      (* can't improve the code we didn't compile... *)
      RETURN FALSE;
    ELSIF (f.name.kind # NK.M3) THEN
      (* can only improve executable Modula-3... *)
      RETURN FALSE;
    ELSIF (f.object = NIL) OR Text.Equal (f.object, f.source) THEN
      (* can't improve the code we didn't compile... *)
      RETURN FALSE;
    ELSE
      (* check for a wish that could be fulfilled. *)
      WITH z = f.link_info.wishes DO
        FOR i := z.start TO z.start + z.cnt - 1 DO
          IF magic.get (f.link_info.info[i], ref) THEN
            ExplainF ("new opaque info -> recompiling ", f);
            RETURN TRUE;
          END;
        END;
      END;
      RETURN FALSE;
    END;
  END CouldBeImproved;

PROCEDURE ObjectIsStale (f: FileInfo): BOOLEAN =
  VAR objTime: INTEGER;
  BEGIN
    IF (NOT make_mode) THEN
      IF (f.name.kind = NK.I3) OR (f.name.kind = NK.M3)
        THEN f.stale_src := TRUE; (* defer the message for a moment *)
        ELSE Msg.Explain (" -> compiling ", Unit.GetRelativePath (f.source));
      END;
      RETURN TRUE
    END;

    ETimer.Push (M3Timers.staleobj);

    (* check if the source is newer than the object *)
    objTime := Utils.LocalModTime (f.object);

    (*********************************************************
      ---- too many people thought that "missing object" was
           an error, so we just won't distinguish a missing
           object from an old one.  I guess "new source" is
           a cheery, more positive message...  -----
    *********************************************************)

    IF (objTime = Utils.NO_TIME)
      OR (objTime < Utils.ModificationTime (f.source)) THEN
      IF (f.name.kind = NK.I3) OR (f.name.kind = NK.M3)
        THEN f.stale_src := TRUE; (* defer the message for a moment *)
        ELSE Msg.Explain ("new source -> compiling ", 
					 Unit.GetRelativePath (f.source));
      END;
      ETimer.Pop ();
      RETURN TRUE;
    END;

    (* object exists and is newer than the source... *)
    ETimer.Pop ();
    RETURN FALSE;
  END ObjectIsStale;

PROCEDURE M3isStale (f: FileInfo): BOOLEAN
  RAISES {Error} =
  BEGIN
    IF ObjectIsStale (f) THEN RETURN TRUE END;

    ETimer.Push (M3Timers.stalem3);

    IF (f.link_info = NIL) THEN
      f.missing_info := TRUE; (* defer the message for a moment *)
      ETimer.Pop ();
      RETURN TRUE;
    END;

    (* check my imports first *)
    CheckImports (f.link_info);

    (* check for new generics *)
    IF NewGenerics (f) THEN
      ExplainF ("new generic source -> compiling ", f);
      ETimer.Pop ();
      RETURN TRUE;
    END;

    (* finally, add my self to the set *)
    DebugF ("merging initial link info for ", f);
    IF NOT MergeUnit (f.link_info, optional := TRUE) THEN
      ExplainF ("stale imports -> compiling ", f);
      ETimer.Pop ();
      RETURN TRUE;
    END;

    DebugF ("ok ", f);
    ETimer.Pop ();
    RETURN FALSE;
  END M3isStale;

PROCEDURE Merge (f: FileInfo)
  RAISES {Error} =
  BEGIN
    ETimer.Push (M3Timers.merge);

    IF (f.link_info = NIL) THEN BadFile ("missing link info", f); END;

    CheckImports (f.link_info);

    DebugF ("merging final link info for ", f);
    EVAL MergeUnit (f.link_info, optional := FALSE);

    ETimer.Pop ();
  END Merge;

PROCEDURE Remerge (f: FileInfo)
  RAISES {Error} =
  BEGIN
    ETimer.Push (M3Timers.merge);

    IF (f.link_info = NIL) THEN BadFile ("missing link info", f); END;

    DebugF ("adding new magic for ", f);
    AddMagic (f.link_info);

    ETimer.Pop ();
  END Remerge;

PROCEDURE CheckImports (u: Mx.Unit)
  RAISES {Error} =
  BEGIN
    CheckImp (u, u.imported_units,  interfaces);
    CheckImp (u, u.exported_units,  interfaces);
    CheckImp (u, u.used_interfaces, interfaces);
    CheckImp (u, u.used_modules,    modules);
  END CheckImports;

PROCEDURE CheckImp (u: Mx.Unit;  READONLY z: Mx.InfoList;  map: IntRefTbl.T)
  RAISES {Error} =
  VAR ref: REFANY;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      IF map.get (u.info[i], ref) THEN CompileOne (ref) END;
    END;
  END CheckImp;

PROCEDURE NewGenerics (f: FileInfo): BOOLEAN
  RAISES {Error} =
  VAR
    u := f.link_info;
    obj_time: INTEGER;
    generic_time: INTEGER;
    nm: TEXT;
  BEGIN
    IF (u.imported_generics.cnt <= 0) THEN RETURN FALSE END;
    obj_time := Utils.LocalModTime (f.object);

    WITH z = u.imported_generics DO
      FOR i := z.start TO z.start + z.cnt - 1 DO
        nm := M3ID.ToText (u.info[i]);
        generic_time := FindGeneric (nm, u.interface);
        IF (obj_time < generic_time) THEN RETURN TRUE END;
      END;
    END;
    RETURN FALSE;
  END NewGenerics;

PROCEDURE FindGeneric (name: TEXT;  interface: BOOLEAN): INTEGER
  RAISES {Error} =
  VAR
    path := Unit.Find (name, interface, generic := TRUE);
    ext  := NK.IG;
  BEGIN
    IF (path = NIL) THEN
      IF (NOT interface) THEN ext := NK.MG; END;
      Msg.FatalError (NIL, "cannot find generic source: ",
                      M3Path.Join (NIL, name, ext, host := TRUE));
      RETURN Utils.NO_TIME;
    END;
    RETURN Utils.ModificationTime (path);
  END FindGeneric;

(*------------------------------------------------------------ first pass ---*)

TYPE
  InfoList = RECORD
    cnt : INTEGER    := 0;
    info: Mx.InfoVec := NIL;
  END;

TYPE
  Env = M3Compiler.Environment OBJECT
    source            : TEXT;
    object            : TEXT;
    output            : Wr.T;
    cg                : M3CG.T;
    unit              : Mx.Unit;
    imports           : IntSet.T;
    exports           : IntSet.T;
    used_intfs        : IntSet.T;
    used_impls        : IntSet.T;
    wish_map          : IntSet.T;
    used_magic        : IntSet.T;
    exported_units    : InfoList; (* of M3ID.Ts *)
    imported_units    : InfoList; (* of M3ID.Ts *)
    imported_generics : InfoList; (* of M3ID.Ts *)
    used_interfaces   : InfoList; (* of M3ID.Ts *)
    used_modules      : InfoList; (* of M3ID.Ts *)
    import_def_syms   : InfoList; (* of MxVS.Ts *)
    import_use_syms   : InfoList; (* of MxVS.Ts *)
    export_def_syms   : InfoList; (* of MxVS.Ts *)
    export_use_syms   : InfoList; (* of MxVS.Ts *)
    imported_types    : InfoList; (* of TypeNames *)
    exported_types    : InfoList; (* of TypeNames *)
    wishes            : InfoList; (* of TypeNames *)
  OVERRIDES
    report_error       := Pass0_Error;
    find_source        := Pass0_Open;
    note_unit          := Pass0_NoteUnit;
    note_comment       := Pass0_Comment;
    note_interface_use := Pass0_NoteInterface;
    note_generic_use   := Pass0_NoteGeneric;
    note_version_stamp := Pass0_NoteVS;
    note_opaque        := Pass0_NoteOpaque;
    note_revelation    := Pass0_NoteRevelation;
    note_opaque_magic  := Pass0_AddMagic;
    find_opaque_magic  := Pass0_FindMagic;
    note_ast           := Pass0_NoteAST;
    find_ast           := Pass0_FindAST;
    note_type          := Pass0_NoteType;
    init_code_generator:= Pass0_InitCodeGenerator;
    note_webinfo       := Pass0_NoteWebInfo;
  END;

VAR (* CONST *)
  env: Env := NEW (Env);

PROCEDURE ResetEnv (source, object: TEXT) =
  BEGIN
    env.source                := source;
    env.object                := object;
    env.output                := NIL;
    env.cg                    := NIL;
    env.unit                  := NIL;
    env.imports               := NIL;
    env.exports               := NIL;
    env.used_intfs            := NIL;
    env.used_impls            := NIL;
    env.wish_map              := NIL;
    env.used_magic            := NIL;
    env.exported_units.cnt    := 0;
    env.imported_units.cnt    := 0;
    env.imported_generics.cnt := 0;
    env.used_interfaces.cnt   := 0;
    env.used_modules.cnt      := 0;
    env.import_def_syms.cnt   := 0;
    env.import_use_syms.cnt   := 0;
    env.export_def_syms.cnt   := 0;
    env.export_use_syms.cnt   := 0;
    env.imported_types.cnt    := 0;
    env.exported_types.cnt    := 0;
    env.wishes.cnt            := 0;
  END ResetEnv;

PROCEDURE Pass0 (f: FileInfo;  object: TEXT): BOOLEAN
  RAISES {Error} =
  VAR
    ok      : BOOLEAN;
    source  : M3Compiler.SourceFile;
    options := Arg.Flatten (pass_0.args, warning_arg);
    input   : File.T      := NIL;
  BEGIN
    ETimer.Push (M3Timers.pass_0);

    (* open the input file *)
    input  := Utils.OpenReader (f.source, fatal := FALSE);
    ok := (input # NIL);
    source.name := f.source;
    source.contents := input;

    IF (ok) AND ((f.stale_src) OR (f.missing_info)) THEN
      Pass0_CheckImports (source);
      FlushPending ();
      (* finally, generate the deferred message *)
      IF (f.missing_info) THEN
        f.missing_info := FALSE;
        ExplainF ("missing version stamps -> compiling ", f);
      ELSIF (NOT make_mode) THEN
        f.stale_src := FALSE;
        Msg.Explain (" -> compiling ", Unit.GetRelativePath (f.source));
      ELSE
        f.stale_src := FALSE;
        Msg.Explain ("new source -> compiling ", 
				    Unit.GetRelativePath (f.source));
      END;
    END;

    (* do the compilation *)
    IF (ok) THEN
      ResetEnv (f.source, object);
      Pass0_Trace (f.source, options);

      TRY
        ok := M3Compiler.Compile (source, env, options^);
      EXCEPT
        | M3Compiler.FrontError =>  RAISE Error;
      END;
    END;
    IF (ok) AND (env.unit # NIL) THEN
      new_linkInfo := TRUE;
      f.link_info := FinishUnitInfo (env);
    ELSE
      IF (f.link_info # NIL) THEN new_linkInfo := TRUE; END;
      f.link_info := NIL;
    END;

    (* dump the generated code *)
    IF (env.cg # NIL) THEN M3Backend.Close (env.cg); END;

    (* flush and close the files *)
    Utils.CloseReader (input, f.source);
    Utils.CloseWriter (env.output, env.object);
    ResetEnv (NIL, NIL);

    IF NOT ok THEN
      compile_failed := TRUE;
      IF (NOT keep_files) THEN Utils.Remove (object); END;
    END;

    ETimer.Pop ();
    RETURN ok;
  END Pass0;

PROCEDURE Pass0_InitCodeGenerator (env: Env): M3CG.T
  RAISES {M3Compiler.FrontError} =
  BEGIN
    TRY
      env.cg     := NIL;
      env.output := Utils.OpenWriter (env.object, fatal := FALSE);
      IF (env.output # NIL) THEN
        env.cg := M3Backend.Open (env.output, env.object);
      END;
    EXCEPT
      | Error =>  RAISE M3Compiler.FrontError;
    END;

    RETURN env.cg;
  END Pass0_InitCodeGenerator;

PROCEDURE Pass0_CheckImports (VAR source: M3Compiler.SourceFile)
  RAISES {Error} =
  VAR ids: M3ID.IDList;  ref: REFANY;
  BEGIN
    ResetEnv (source.name, NIL);
    ids := M3Compiler.ParseImports (source, env);
    WHILE (ids # NIL) DO
      IF interfaces.get (ids.id, ref) THEN CompileOne (ref) END;
      ids := ids.next;
    END;
    Utils.RewindReader (source.contents, source.name);
  END Pass0_CheckImports;

PROCEDURE Pass0_Trace (source: TEXT;  options: REF ARRAY OF TEXT) =
  BEGIN
    IF (Msg.level < Msg.Level.Commands) THEN RETURN END;
    Msg.Out ("m3c ", source);
    FOR i := 0 TO LAST (options^) DO
      Msg.Out (" ", options[i]);
    END;
    Msg.Out (Wr.EOL);
  END Pass0_Trace;

PROCEDURE Pass0_Error (<*UNUSED*>env: Env;  file: TEXT;  line: INTEGER;
                       msg: TEXT) =
  BEGIN
    IF (file # NIL)
      THEN Msg.Out ("\"", Unit.GetRelativePath(file), "\", line ",
		    Fmt.Int (line), ": ", msg,Wr.EOL);
      ELSE Msg.Out (msg, Wr.EOL);
    END;
  END Pass0_Error;

PROCEDURE Pass0_Open (<*UNUSED*> env: Env;  name: M3ID.T;
                      interface, generic: BOOLEAN): M3Compiler.SourceFile =
  BEGIN
    RETURN Unit.Open (M3ID.ToText (name), interface, generic);
  END Pass0_Open;

PROCEDURE Pass0_NoteUnit (env: Env;  name: M3ID.T;  interface: BOOLEAN) =
  BEGIN
    env.unit := NEW (Mx.Unit, name := name, interface := interface,
                     file := NEW (Mx.File, name := env.source));
    env.imports    := NEW (IntSet.Default).init ();
    env.exports    := NEW (IntSet.Default).init ();
    env.used_intfs := NEW (IntSet.Default).init ();
    env.used_impls := NEW (IntSet.Default).init ();
    env.wish_map   := NEW (IntSet.Default).init ();
    env.used_magic := NEW (IntSet.Default).init ();
  END Pass0_NoteUnit;

PROCEDURE Pass0_NoteInterface (env: Env;  name: M3ID.T;  imported: BOOLEAN) =
  BEGIN
    EVAL env.used_intfs.put (name, 0);
    IF imported THEN
      IF NOT env.imports.put (name, 0) THEN
        AddInfo (env.imported_units, name);
      END;
    ELSE
      IF NOT env.exports.put (name, 0) THEN
        AddInfo (env.exported_units, name);
      END;
    END;
  END Pass0_NoteInterface;

PROCEDURE Pass0_NoteGeneric (env: Env;  name: M3ID.T) =
  BEGIN
    AddInfo (env.imported_generics, name);
  END Pass0_NoteGeneric;

PROCEDURE Pass0_NoteVS (env: Env;  intf, name: M3ID.T;
                        READONLY fp: Fingerprint.T;
                        imported, implemented: BOOLEAN) =
  VAR info: MxVS.Info;   vs: MxVS.T;
  BEGIN
    info.source := intf;
    info.symbol := name;
    info.stamp  := fp;
    vs := MxVS.Put (info);
    Pass0_NoteInterface (env, intf, imported);
    IF (imported) THEN
      IF (implemented)
        THEN AddInfo (env.import_def_syms, vs);
        ELSE AddInfo (env.import_use_syms, vs);
      END;
    ELSE (*exported*)
      IF (implemented)
        THEN AddInfo (env.export_def_syms, vs);
        ELSE AddInfo (env.export_use_syms, vs);
      END;
    END;
  END Pass0_NoteVS;

PROCEDURE Pass0_NoteRevelation (env: Env;  source: M3ID.T;  interface: BOOLEAN;
                          lhs, rhs: INTEGER;  full, imported: BOOLEAN) =
  VAR r := NEW (Mx.Revelation, source := source, lhs := lhs, rhs := rhs,
                partial := NOT full, export := NOT imported);
  BEGIN
    Pass0_AddUnit (env, source, interface);
    r.next := env.unit.revelations;
    env.unit.revelations := r;
  END Pass0_NoteRevelation;

PROCEDURE Pass0_Comment (<*UNUSED*> env: Env;  t: TEXT) =
  BEGIN
    Msg.Verbose (t);
  END Pass0_Comment;

PROCEDURE Pass0_NoteOpaque (env: Env;  type, super_type: INTEGER) =
  BEGIN
    env.unit.opaques := NEW (Mx.OpaqueType, type := type,
                             super_type := super_type,
                             next := env.unit.opaques);
  END Pass0_NoteOpaque;

PROCEDURE Pass0_AddUnit (env: Env;  nm: M3ID.T;  interface: BOOLEAN) =
  BEGIN
    IF (interface) THEN
      IF NOT env.used_intfs.put (nm, 0) THEN
        AddInfo (env.used_interfaces, nm);
      END;
    ELSE (*module*)
      IF NOT env.used_impls.put (nm, 0) THEN
        AddInfo (env.used_modules, nm);
      END;
    END;
  END Pass0_AddUnit;

PROCEDURE Pass0_AddMagic (env         : Env;
                          type        : INTEGER;
                          super_type  : INTEGER;
                          data_size   : INTEGER;
                          data_align  : INTEGER;
                          method_size : INTEGER) =
  VAR obj := Pass0_NoteObject (env, env.unit.name, env.unit.interface,
                               FALSE, type, super_type, data_size,
                               data_align, method_size);
  BEGIN
    EVAL env.used_magic.put (type, 0);
    EVAL magic.put (type, obj);
  END Pass0_AddMagic;

PROCEDURE Pass0_FindMagic (env         : Env;
                           type        : INTEGER;
                VAR(*OUT*) super_type  : INTEGER;
                VAR(*OUT*) data_size   : INTEGER;
                VAR(*OUT*) data_align  : INTEGER;
                VAR(*OUT*) method_size : INTEGER): BOOLEAN =
  VAR ref: REFANY;  obj: Mx.ObjectType;
  BEGIN
    IF NOT magic.get (type, ref) THEN
      IF NOT env.wish_map.put (type, 0) THEN
        AddInfo (env.wishes, type);
      END;
      RETURN FALSE;
    END;
    obj := ref;

    IF NOT env.used_magic.put (type, 0) THEN
      EVAL Pass0_NoteObject (env, obj.source, NOT obj.from_module, TRUE,
                             obj.type, obj.super_type, obj.data_size,
                             obj.data_align, obj.method_size);
    END;

    super_type  := obj.super_type;
    data_size   := obj.data_size;
    data_align  := obj.data_align;
    method_size := obj.method_size;
    RETURN TRUE;
  END Pass0_FindMagic;

PROCEDURE Pass0_NoteObject (env: Env;  source: M3ID.T;
                      interface, imported: BOOLEAN;
                      type, super_type: INTEGER;
                      data_size, data_align, method_size: INTEGER
                      ): Mx.ObjectType =
  VAR obj := NEW (Mx.ObjectType, source := source, type := type,
                  super_type := super_type, data_size := data_size,
                  data_align := data_align, method_size := method_size,
                  export := NOT imported, from_module := NOT interface);
  BEGIN
    IF (NOT imported) THEN
      obj.next := env.unit.exported_objects;
      env.unit.exported_objects := obj;
    ELSE
      Pass0_AddUnit (env, source, interface);
      obj.next := env.unit.imported_objects;
      env.unit.imported_objects := obj;
    END;
    RETURN obj;
  END Pass0_NoteObject;

PROCEDURE ExpandInfo (VAR x: InfoList) =
  VAR n := NUMBER (x.info^);  new := NEW (Mx.InfoVec, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := x.info^;
    x.info := new;
  END ExpandInfo;

PROCEDURE FullName(id: M3ID.T): TEXT =
  VAR name,fullname : TEXT;
  BEGIN
    name:= M3ID.ToText(id);
    fullname:= Unit.GetFullPath(name);
    IF fullname = NIL THEN  fullname:= name  END;
    RETURN fullname;
  END FullName;

PROCEDURE Pass0_NoteAST (<*UNUSED*> env: Env;  intf: M3ID.T;  ast: REFANY) =
  VAR fullname := FullName(intf);
      entry    :  ASTCacheEntry;
  BEGIN
    entry := NEW (ASTCacheEntry, fullname:= fullname,  name:= intf,
				 time_stamp:= TimeNow, interface:= ast);
    EVAL ast_cache.put (fullname, entry);
  END Pass0_NoteAST;

PROCEDURE AddImports (list: ASTList; import: ASTCacheEntry): ASTList =
  BEGIN
    RETURN NEW(ASTList, import := import, next := list);
  END AddImports;

PROCEDURE GetImports (interface: ASTCacheEntry) =
  VAR ref    : REFANY;
      import : TEXT;
      idlist : M3ID.IDList;
  BEGIN
    IF interface.n_imports = -1 THEN
      interface.n_imports := 0;
      idlist := M3Compiler.GetImports(interface.interface);

      WHILE idlist # NIL DO
        import:= FullName(idlist.id);
        EVAL ast_cache.get(import, ref);
        interface.imports := AddImports(interface.imports, ref);
        INC(interface.n_imports);
        idlist := idlist.next;
      END;

    END;
  END GetImports;

PROCEDURE MakeValid (entry: ASTCacheEntry): BOOLEAN = 
  VAR ref       : REFANY := NIL;
      import    : ASTList;
      valid     := TRUE;
      filetime  : INTEGER;
      file      : FileInfo;
  BEGIN
    GetImports(entry);

    import := entry.imports;
    WHILE import # NIL DO
      (* we need to make sure that all imported interfaces are valid first *)
      valid:= MakeValid(import.import) AND valid;
      import:= import.next;
    END;

    (* if the imported interfaces are valid, we need to validate the current one *)
    IF valid THEN

      (* if this one has not been already validated during this build, we check *)
      IF entry.time_stamp < TimeNow THEN

         EVAL interfaces.get(entry.name, ref);
         file := ref;

         (* if the package where this interface comes from has been recompiled
            since this interface was put in the cache, we check *)
         IF file = NIL OR file.link_info_ts > entry.time_stamp THEN
           (* we check the time stamps of the interface in the cache and the
              corresponding file to see if the entry is out of date ... *)
	   filetime := Utils.NoteModification(entry.fullname);
           valid := (filetime < entry.time_stamp);
        END;
      END;
    END;

    IF valid THEN
      entry.time_stamp := TimeNow;
    ELSE
      entry := NIL;  (* This interface is staled, put it out of the cache *)
    END;

    RETURN valid;
  END MakeValid;

PROCEDURE Pass0_FindAST (<*UNUSED*> env: Env;  intf: M3ID.T): REFANY =
  VAR ref   : REFANY;
      entry : ASTCacheEntry;
      fullname := FullName(intf);
  BEGIN
    IF ast_cache.get (fullname, ref) THEN
      entry := ref;

      (* if we have a persistent cache across multiple builds,
         we must validate the entries *)
      IF keep_cache THEN
        IF MakeValid(entry) THEN
          RETURN entry.interface;
        ELSE
          RETURN NIL;
        END;
      ELSE
        RETURN entry.interface;
      END;

    ELSE RETURN NIL;
    END;
  END Pass0_FindAST;

PROCEDURE Pass0_NoteType (env: Env;  type: INTEGER;  imported: BOOLEAN) =
  BEGIN
    IF (imported)
      THEN AddInfo (env.imported_types, type);
      ELSE AddInfo (env.exported_types, type);
    END;
  END Pass0_NoteType;

PROCEDURE AddInfo (VAR x: InfoList;  i: INTEGER) =
  BEGIN
    IF (x.info = NIL) THEN
      x.info := NEW (Mx.InfoVec, 40);
    ELSIF (x.cnt >= NUMBER (x.info^)) THEN
      ExpandInfo (x);
    END;
    x.info [x.cnt] := i;
    INC (x.cnt);
  END AddInfo;

PROCEDURE FinishUnitInfo (env: Env): Mx.Unit =
  VAR n: INTEGER;  info: Mx.InfoVec;   u := env.unit;
  BEGIN
    n := env.exported_units.cnt + env.imported_units.cnt
       + env.imported_generics.cnt + env.used_interfaces.cnt
       + env.used_modules.cnt + env.import_def_syms.cnt
       + env.import_use_syms.cnt + env.export_def_syms.cnt
       + env.export_use_syms.cnt + env.imported_types.cnt
       + env.exported_types.cnt + env.wishes.cnt;

    info := NEW (Mx.InfoVec, n);
    n := FinishInfo (info, 0, env.exported_units,    u.exported_units);
    n := FinishInfo (info, n, env.imported_units,    u.imported_units);
    n := FinishInfo (info, n, env.imported_generics, u.imported_generics);
    n := FinishInfo (info, n, env.used_interfaces,   u.used_interfaces);
    n := FinishInfo (info, n, env.used_modules,      u.used_modules);
    n := FinishInfo (info, n, env.import_def_syms,   u.import_def_syms);
    n := FinishInfo (info, n, env.import_use_syms,   u.import_use_syms);
    n := FinishInfo (info, n, env.export_def_syms,   u.export_def_syms);
    n := FinishInfo (info, n, env.export_use_syms,   u.export_use_syms);
    n := FinishInfo (info, n, env.imported_types,    u.imported_types);
    n := FinishInfo (info, n, env.exported_types,    u.exported_types);
    n := FinishInfo (info, n, env.wishes,            u.wishes);

    u.info := info;
    RETURN u;
  END FinishUnitInfo;

PROCEDURE FinishInfo (info: Mx.InfoVec;  n: INTEGER;
                      READONLY x: InfoList;  VAR z: Mx.InfoList): INTEGER=
  BEGIN
    z.start := n;
    z.cnt := x.cnt;
    FOR i := 0 TO x.cnt - 1 DO
      info [n] := x.info[i];
      INC (n);
    END;
    RETURN n;
  END FinishInfo;

PROCEDURE Pass0_NoteWebInfo (env: Env;  info: TEXT) =
  BEGIN
    WebFile.Update (env.source, info);
  END Pass0_NoteWebInfo;

(*------------------------------------------------ compilations and links ---*)

PROCEDURE Pass1 (source, object, base: TEXT)
  RAISES {Error} =
  VAR args := Arg.NewList ();  stdout: TEXT;
  BEGIN
    ETimer.Push (M3Timers.pass_1);
    Arg.AppendL (args, pass_1.args);
    IF (do_debug)    THEN Arg.AppendL (args, cc_debug);    END;
    IF (do_optimize) THEN Arg.AppendL (args, cc_optimize); END;
    Arg.Append (args, "-c");
    Arg.Append (args, source);

    stdout := StdoutName (pass_1, base);
    IF Utils.Execute (pass_1.cmd, args, stdout, fatal := FALSE) # 0 THEN
      compile_failed := TRUE;
      Utils.Remove (object);
    END;
    IF (NOT keep_files) THEN Utils.Remove (stdout); END;
    ETimer.Pop ();
  END Pass1;

PROCEDURE Pass6 (source, object, base: TEXT): BOOLEAN
  RAISES {Error} =
  VAR args: Arg.List;  status: INTEGER;  stdout: TEXT;
  BEGIN
    IF (pass_6.cmd = NIL) THEN RETURN TRUE END;

    ETimer.Push (M3Timers.pass_6);
    args := Arg.NewList ();
    Arg.AppendL (args, pass_6.args);
    IF (do_debug)    THEN Arg.AppendL (args, cg_debug);    END;
    IF (do_optimize) THEN Arg.AppendL (args, cg_optimize); END;
    (**
    IF (Msg.level >= Msg.Level.Verbose) THEN Arg.Append (args, "-v"); END;
    **)
    Arg.Append (args, "-o");
    Arg.Append (args, object);
    Arg.Append (args, source);

    stdout := StdoutName (pass_6, base);
    status := Utils.Execute (pass_6.cmd, args, stdout, fatal := TRUE);
    IF (status # 0) THEN
      compile_failed := TRUE;
      Utils.Remove (object);
    END;
    IF (NOT keep_files) THEN Utils.Remove (stdout); END;
    ETimer.Pop ();
    RETURN (status = 0);
  END Pass6;

PROCEDURE Pass7 (source, object, base: TEXT): BOOLEAN
  RAISES {Error} =
  VAR args: Arg.List;  status: INTEGER;  stdout: TEXT;
  BEGIN
    IF (pass_7.cmd = NIL) THEN RETURN TRUE END;

    ETimer.Push (M3Timers.pass_7);
    args := Arg.NewList ();
    Arg.AppendL (args, pass_7.args);
    IF (do_debug)    THEN Arg.AppendL (args, as_debug);    END;
    IF (do_optimize) THEN Arg.AppendL (args, as_optimize); END;
    (**
    IF (Msg.level >= Msg.Level.Verbose) THEN Arg.Append (args, "-v");    END;
    **)
    IF (target_os = M3Path.OSKind.Win32) THEN
      Arg.Append (args, source);
      Arg.Append (args, object);
      Arg.Append (args, ";");
    ELSE
      Arg.Append (args, "-o");
      Arg.Append (args, object);
      Arg.Append (args, source);
    END;

    stdout := StdoutName (pass_7, base);
    status := Utils.Execute (pass_7.cmd, args, stdout, fatal := TRUE);
    IF (status # 0) THEN
      compile_failed := TRUE;
      Utils.Remove (object);
    END;
    IF (NOT keep_files) THEN Utils.Remove (stdout); END;
    ETimer.Pop ();
    RETURN (status = 0);
  END Pass7;

PROCEDURE Pass8 (source, object, base: TEXT): BOOLEAN
  RAISES {Error} =
  VAR args: Arg.List;  status: INTEGER;  stdout: TEXT;
  BEGIN
    IF (pass_8.cmd = NIL) THEN RETURN TRUE END;

    ETimer.Push (M3Timers.pass_8);
    args := Arg.NewList ();
    Arg.AppendL (args, pass_8.args);
    Arg.Append (args, "-o");
    Arg.Append (args, object);
    Arg.Append (args, source);

    stdout := StdoutName (pass_8, base);
    status := Utils.Execute (pass_8.cmd, args, stdout, fatal := TRUE);
    IF (status # 0) THEN
      compile_failed := TRUE;
      Utils.Remove (object);
    END;
    IF (NOT keep_files) THEN Utils.Remove (stdout); END;
    ETimer.Pop ();
    RETURN (status = 0);
  END Pass8;

(*------------------------------------------------ compilations and links ---*)

PROCEDURE BuildProgram ()
  RAISES {Error} =
  CONST Desc_file = ".M3LINK";
  CONST Shared_switch = ARRAY BOOLEAN OF TEXT { "-Bstatic", "-Bdynamic" };
  VAR
    Main_C   := M3Path.Join (NIL, "_m3main", NK.C, host := FALSE);
    Main_O   := M3Path.Join (NIL, "_m3main", NK.O, host := FALSE);
    Main_XX  := "_m3main.new";
    args     := Arg.NewList ();
    pgmTime  : INTEGER;
    pgmValid : BOOLEAN;
    f        : FileInfo;
    pgm_obj  := Arg.NewList ();
    pgm_file : TEXT := pgm_name;
    wr       : Wr.T;
    init_code: TEXT := NIL;
    time_O   : INTEGER;
    time_C   : INTEGER;
    stdout   : TEXT;
  BEGIN
    <*ASSERT NOT bootstrap_mode *>

    IF (compile_failed) THEN
      Msg.Explain ("compilation failed => not building program \"",pgm_name,"\"");
      IF has_loader THEN Utils.Remove (Desc_file); END;
      RETURN;
    END;

    IF (target_os = M3Path.OSKind.Win32) THEN
      pgm_file := pgm_name & ".exe";
    END;
    pgmTime := Utils.LocalModTime (pgm_file);

    pgmValid := (make_mode) AND (pgmTime # Utils.NO_TIME);
    IF NOT pgmValid AND NOT skip_link THEN
      Msg.Explain (" -> linking ", pgm_file);
    END;

    f := sources;
    WHILE (f # NIL) DO
      IF (f.object # NIL) THEN
        IF pgmValid AND (Utils.LocalModTime (f.object) > pgmTime) THEN
          IF NOT skip_link THEN
            Msg.Explain ("new \"",f.object,"\" -> linking ",pgm_file);
          END;
          pgmValid := FALSE;
        END;
        Arg.Append (pgm_obj, f.object);
      END;
      f := f.next;
    END;

    IF (do_coverage) THEN
      Arg.Append (pgm_obj, link_coverage);
    END;

    f := libraries;
    WHILE (f # NIL) DO
      IF pgmValid AND (Utils.ModificationTime (f.library) > pgmTime) THEN
        IF NOT skip_link THEN
          Msg.Explain ("new \"",f.library,"\" -> linking ",pgm_file);
        END;
        pgmValid := FALSE;
      END;
      IF (supply_shared) THEN
        Arg.Append(pgm_obj, Shared_switch[f.shared]);
      END;
      IF (keep_resolved) OR (f.source # NIL) THEN
        Arg.Append (pgm_obj, f.library);
      ELSE
        IF (f.name.dir # NIL) THEN
          Arg.Append (pgm_obj, "-L" & f.name.dir);
          IF (need_Rpath) AND (f.shared) THEN
            (* For shared libs, augment the run-time library search path. *)
            Arg.Append (pgm_obj, "-R" & f.name.dir)
          END;
        END;
        Arg.Append (pgm_obj, "-l" & f.name.base);
      END;
      f := f.next;
    END;

    IF pgmValid THEN RETURN END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsProgram (link_base, Msg.writer) THEN
      IF has_loader THEN Utils.Remove (Desc_file); END;
      Msg.FatalError (NIL, "incomplete program");
    END;
    ETimer.Pop ();

    (* check for an up-to-date Main_O *)
    time_O := Utils.LocalModTime (Main_O);
    time_C := Utils.LocalModTime (Main_C);
    IF (time_O < time_C) OR (time_C = Utils.NO_TIME) THEN
      (* we must compile the linker generated code *)
      init_code := Main_C;
    ELSE
      init_code := Main_XX;
      Utils.NoteTempFile (Main_XX);
    END;

    (* produce the module init list *)
    ETimer.Push (M3Timers.genMain);
    Msg.Commands ("generate ", init_code);
    wr := Utils.OpenWriter (init_code, fatal := TRUE);
    MxGen.GenerateMain (link_base, wr, Msg.level >= Msg.Level.Debug,
                        gui AND (target_os = M3Path.OSKind.Win32));
    Utils.CloseWriter (wr, init_code);
    ETimer.Pop ();

    IF (init_code = Main_XX) AND Utils.IsEqual (Main_XX, Main_C) THEN
      (* we don't need to compile! *)
      Utils.Remove (Main_XX);
    ELSE
      IF (init_code = Main_XX) THEN
        Utils.Copy (Main_XX, Main_C);
        Utils.Remove (Main_XX);
      END;
      Msg.Debug ("compiling ", Main_C, " ...", Wr.EOL);
      Pass1 (Main_C, Main_O, "_m3main");
      IF (compile_failed) THEN
        Msg.FatalError (NIL, "cc ", Main_C, " failed!!");
      END;
      Utils.NoteNewFile (Main_O);
      Utils.NoteNewFile (Main_C);
    END;

    IF has_loader THEN WriteProgramDesc (Desc_file, Main_O); END;

    IF skip_link THEN RETURN END;

    ETimer.Push (M3Timers.pass_2);
    Arg.AppendL (args, pass_2.args);
    IF (target_os = M3Path.OSKind.Win32) THEN
      IF (gui) THEN
        Arg.Append (args, "-subsystem:windows");
        Arg.Append (args, "-entry:WinMainCRTStartup");
      ELSE
        Arg.Append (args, "-subsystem:console");
        Arg.Append (args, "-entry:mainCRTStartup");
      END;
      Arg.Append (args, "-out:" & pgm_file);
      Arg.Append (args, "-map:" & pgm_name & ".map"); (* until debugger.. *)
      pgm_obj := GenObjectList (pgm_name, pgm_obj);
    ELSE
      Arg.Append (args, "-o");
      Arg.Append (args, pgm_name);
    END;
    Arg.Append  (args, Main_O);
    Arg.AppendL (args, pgm_obj);
    stdout := StdoutName (pass_2, pgm_name);
    EVAL Utils.Execute (pass_2.cmd, args, stdout, fatal := TRUE);
    IF (NOT keep_files) THEN Utils.Remove (stdout); END;
    ETimer.Pop ();

    (******
    IF (NOT keep_files) THEN
      Utils.Remove (Main_C);
      Utils.Remove (Main_O);
    END;
    ********)
  END BuildProgram;

PROCEDURE WriteProgramDesc (desc_file, main_o: TEXT)
  RAISES {Error} =
  VAR f: FileInfo;  wr: Wr.T;
  BEGIN
    ETimer.Push (M3Timers.genLink);
    ReverseSources ();

    (* write the description *)
    wr := Utils.OpenWriter (desc_file, fatal := TRUE);
    TRY
      IF (target_os = M3Path.OSKind.Win32) THEN
        Wr.PutText (wr, "-out:");
        Wr.PutText (wr, pgm_name);
        Wr.PutText (wr, ".exe");
        Wr.PutText (wr, Target.EOL);
        IF (gui)
          THEN Wr.PutText (wr, "-subsystem:windows");
          ELSE Wr.PutText (wr, "-subsystem:console");
        END;
        Wr.PutText (wr, Target.EOL);
      ELSE
        Wr.PutText (wr, "-o ");
        Wr.PutText (wr, pgm_name);
        Wr.PutText (wr, Target.EOL);
      END;

      (* write the library timestamps *)
      f := libraries;
      WHILE (f # NIL) DO
        Wr.PutText (wr, f.library);
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (Utils.ModificationTime (f.library)));
        Wr.PutText (wr, Target.EOL);
        f := f.next;
      END;

      IF (do_coverage) THEN
        Wr.PutText (wr, link_coverage);
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (Utils.ModificationTime (link_coverage)));
        Wr.PutText (wr, Target.EOL);
      END;

      (* write the object timestamps *)
      f := sources;
      WHILE (f # NIL) DO
        IF (f.object # NIL) THEN
          Wr.PutText (wr, f.object);
          Wr.PutChar (wr, ' ');
          Wr.PutText (wr, Fmt.Int (Utils.LocalModTime (f.object)));
          Wr.PutText (wr, Target.EOL);
        END;
        f := f.next;
      END;

      (* add the linker generated main body *)
      Wr.PutText (wr, main_o);
      Wr.PutChar (wr, ' ');
      Wr.PutText (wr, Fmt.Int (Utils.LocalModTime (main_o)));
      Wr.PutText (wr, Target.EOL);

      Utils.CloseWriter (wr, desc_file);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to write file: ", desc_file);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to write file: ", desc_file);
    END;

    ReverseSources ();
    ETimer.Pop ();
  END WriteProgramDesc;

PROCEDURE BuildBootProgram ()
  RAISES {Error} =
  CONST Makefile = "make.boot";
  VAR f: FileInfo;  wr: Wr.T;  Main_C: TEXT;
  BEGIN
    <*ASSERT bootstrap_mode *>

    IF (compile_failed) THEN
      Msg.Explain ("compilation failed => not building program \"",pgm_name,"\"");
      Utils.Remove (Makefile);
      RETURN;
    END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsProgram (link_base, Msg.writer) THEN
      Msg.FatalError (NIL, "incomplete program");
    END;
    ETimer.Pop ();

    (* produce the module init list *)
    ETimer.Push (M3Timers.genMain);
    Main_C := M3Path.Join (NIL, "_m3main", NK.C, host := FALSE);
    Msg.Commands ("generate ", Main_C);
    wr := Utils.OpenWriter (Main_C, fatal := TRUE);
    MxGen.GenerateMain (link_base, wr, Msg.level >=Msg.Level.Debug,
                        gui AND (target_os = M3Path.OSKind.Win32));
    Utils.CloseWriter (wr, Main_C);
    ETimer.Pop ();

    Msg.Explain ("building makefile -> ", Makefile);
    wr := Utils.OpenWriter (Makefile, fatal := TRUE);
    TRY
      Wr.PutText (wr, "% bootstrap Modula-3 makefile");
      Wr.PutText (wr, Target.EOL);
      Wr.PutText (wr, Target.EOL);

      Wr.PutText (wr, "boot_c (\"_m3main\")");
      Wr.PutText (wr, Target.EOL);
      f := sources;
      WHILE (f # NIL) DO
        IF (f.object # NIL) THEN  GenBootLine (wr, f);  END;
        f := f.next;
      END;
      Wr.PutText (wr, Target.EOL);

      f := libraries;
      WHILE (f # NIL) DO
        Wr.PutText (wr, "boot_import (\"");
        IF (f.name.dir = NIL) THEN
          Wr.PutText (wr, "-l" & f.name.base);
        ELSE
          Wr.PutText (wr, M3Path.Escape (
                            M3Path.Convert (
                              M3Path.Join (f.name.dir, f.name.base,
                                          f.name.kind, host := FALSE),
                              host := FALSE)));
        END;
        Wr.PutText (wr, "\")");
        Wr.PutText (wr, Target.EOL);
        f := f.next;
      END;
      Wr.PutText (wr, Target.EOL);

      Wr.PutText (wr, Target.EOL);
      Wr.PutText (wr, "boot_prog (\"");
      Wr.PutText (wr, M3Path.Escape (pgm_name));
      Wr.PutText (wr, "\")");
      Wr.PutText (wr, Target.EOL);

      Wr.Close (wr);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to write file: ", Makefile);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to write file: ", Makefile);
    END;
  END BuildBootProgram;

PROCEDURE GenBootLine (wr: Wr.T;  f: FileInfo)
  RAISES {Wr.Failure, Thread.Alerted} =
  TYPE F = NK;
  VAR func: TEXT;  base: TEXT;  shorten := FALSE;
  BEGIN
    CASE f.name.kind OF 
    | F.I3, F.IC, F.IS =>
        IF (pass_7.cmd # NIL)
          THEN func := "boot_is";  shorten := TRUE;
          ELSE func := "boot_io";
        END;
    | F.M3, F.MC, F.MS =>
        IF (pass_7.cmd # NIL)
          THEN func := "boot_ms";  shorten := TRUE;
          ELSE func := "boot_mo";
        END;
    | F.C              =>  func := "boot_c";
    | F.S              =>  func := "boot_s";
    | F.IO, F.MO, F.O  =>  func := "boot_obj";
    ELSE RETURN; 
    END;
    base := f.name.base;
    IF (shorten) AND (target_os = M3Path.OSKind.Win32) THEN
      base := ShortenName (f.name.base);
    END;
    Wr.PutText (wr, func);
    Wr.PutText (wr, " (\"");
    Wr.PutText (wr, base);
    Wr.PutText (wr, "\")");
    Wr.PutText (wr, Target.EOL);
  END GenBootLine;

PROCEDURE BuildLibrary ()
  RAISES {Error} =
  VAR
    lib      := lib_name;
    args     := Arg.NewList ();
    f        : FileInfo;
    lib_time : INTEGER;
    libValid := TRUE;
    lib_obj  := Arg.NewList ();
    name     := M3Path.Parse (lib_name, host := FALSE);
    stdout   : TEXT;
  BEGIN
    <*ASSERT NOT bootstrap_mode *>

    IF (name.kind # NK.A) THEN
      Msg.FatalError (NIL, "bad library name: ", lib_name);
    END;

    IF (compile_failed) THEN
      Msg.Explain ("compilation failed => not building library \"",
                   lib_name, "\"");
      RETURN;
    END;

    lib_time := Utils.LocalModTime (lib);

    IF (NOT make_mode) OR (lib_time = Utils.NO_TIME) THEN
      Msg.Explain (" -> archiving ", lib);
      libValid := FALSE;
    END;

    f := sources;
    WHILE (f # NIL) DO
      IF (f.object # NIL) THEN
        IF libValid AND (Utils.LocalModTime (f.object) > lib_time) THEN
          Msg.Explain ("new \"", f.object, "\" -> archiving ", lib);
          libValid := FALSE;
        END;
        Arg.Append (lib_obj, f.object);
      END;
      f := f.next;
    END;

    IF libValid THEN RETURN END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsLibrary (link_base, Msg.writer) THEN
      Msg.FatalError (NIL, "incomplete library");
    END;
    ETimer.Pop ();

    Msg.Debug ("building the library...", Wr.EOL);
    Utils.Remove (lib);

    IF (target_os = M3Path.OSKind.Win32) THEN
      GenLibDef (name.base);
      lib_obj := GenObjectList (name.base, lib_obj);
    END;

    IF (Msg.level >= Msg.Level.Debug) THEN
      (* add the Msg.Verbose option to 'ar' *)
      WITH x = pass_3.args.tail DO
        IF (x # NIL) THEN
          IF Text.Equal (x.arg, "cru")  THEN x.arg := "cruv"; END;
          IF Text.Equal (x.arg, "lcru") THEN x.arg := "lcruv"; END;
        END;
      END;
    END;

    ETimer.Push (M3Timers.pass_3);
    Arg.AppendL (args, pass_3.args);
    IF (target_os = M3Path.OSKind.Win32)
      THEN Arg.Append  (args, "-out:" & lib);
      ELSE Arg.Append  (args, lib);
    END;
    Arg.AppendL (args, lib_obj);
    stdout := StdoutName (pass_3, name.base);
    EVAL Utils.Execute (pass_3.cmd, args, stdout, fatal := TRUE);
    IF (NOT keep_files) THEN Utils.Remove (stdout); END;
    ETimer.Pop ();

    IF (pass_4.cmd # NIL) THEN
      ETimer.Push (M3Timers.pass_4);
      args := Arg.NewList ();
      Arg.AppendL (args, pass_4.args);
      Arg.Append  (args, lib);
      stdout := StdoutName (pass_4, name.base);
      EVAL Utils.Execute (pass_4.cmd, args, stdout, fatal := TRUE);
      IF (NOT keep_files) THEN Utils.Remove (stdout); END;
      ETimer.Pop ();
    END;
  END BuildLibrary;

PROCEDURE BuildBootLibrary ()
  RAISES {Error} =
  CONST Makefile = "make.boot";
  VAR
    f     : FileInfo;
    wr    : Wr.T;
    name  := M3Path.Parse (lib_name, host := TRUE);
  BEGIN
    <*ASSERT bootstrap_mode *>

    IF (compile_failed) THEN
      Msg.Explain ("compilation failed => not building library \"",
                   lib_name,"\"");
      Utils.Remove (Makefile);
      RETURN;
    END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsLibrary (link_base, Msg.writer) THEN
      Msg.FatalError (NIL, "incomplete library");
    END;
    ETimer.Pop ();

    Msg.Explain ("building makefile -> ", Makefile);
    wr := Utils.OpenWriter (Makefile, fatal := TRUE);
    TRY
      Wr.PutText (wr, "% bootstrap Modula-3 makefile");
      Wr.PutText (wr, Target.EOL);
      Wr.PutText (wr, Target.EOL);

      f := sources;
      WHILE (f # NIL) DO
        IF (f.object # NIL) THEN  GenBootLine (wr, f); END;
        f := f.next;
      END;
      Wr.PutText (wr, Target.EOL);

      Wr.PutText (wr, Target.EOL);
      Wr.PutText (wr, "boot_lib (\"");
      Wr.PutText (wr, M3Path.Escape (name.base));
      Wr.PutText (wr, "\")");
      Wr.PutText (wr, Target.EOL);

      Wr.Close (wr);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to write file: ", Makefile);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to write file: ", Makefile);
    END;
  END BuildBootLibrary;

PROCEDURE GenLibDef (libname: TEXT)
  RAISES {Error} =
  VAR lib_def := libname & ".def";  wr: Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(lib_def);
      Wr.PutText (wr, "LIBRARY ");
      Wr.PutText (wr, libname);
      Wr.PutText (wr, Target.EOL);
      Wr.Close(wr);
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        Msg.FatalError (NIL, "failed to write: ", lib_def);
    | OSError.E(args) =>
        Msg.FatalError (args, "failed to write: ", lib_def);
    END
  END GenLibDef;

PROCEDURE GenObjectList (base: TEXT;  obj: Arg.List): Arg.List
  RAISES {Error} =
  VAR tmp_file: TEXT;  wr: Wr.T;  arg: Arg.T;
  BEGIN
    IF (obj.cnt <= 10) THEN RETURN obj; END;
    TRY
      tmp_file := base & ".lrf";
      (* Utils.NoteTempFile (tmp_file); *)
      wr := FileWr.Open (tmp_file);
      arg := obj.head;
      WHILE (arg # NIL) DO
        Wr.PutText (wr, arg.arg);
        Wr.PutText (wr, Target.EOL);
        arg := arg.next;
      END;
      Wr.Close(wr);
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
        Msg.FatalError (NIL, "failed to write: ", tmp_file);
    | OSError.E(args) =>
        Msg.FatalError (args, "failed to write: ", tmp_file);
    END;
    obj := Arg.NewList ();
    Arg.Append (obj, "@" & tmp_file);
    RETURN obj;
  END GenObjectList;

(*--------------------------------------------------------- version stamps --*)

PROCEDURE GetLinkUnits (info, file: TEXT;  imported: BOOLEAN): Mx.UnitList
  RAISES {Error} =
  VAR
    rd: File.T;
    wr: Wr.T;
    units: Mx.UnitList;
    start, stop: INTEGER;
  BEGIN
    IF (Msg.level >= Msg.Level.Verbose) THEN start := ROUND (Time.Now ()) END;

    (* try to open file's link info file *)
    TRY
      rd := FS.OpenFileReadonly (info);
    EXCEPT OSError.E (args) =>
      Msg.Debug ("unable to open link info file: ",
                 info,Msg.OSErr (args), Wr.EOL);
      RETURN NIL;
    END;

    IF (Msg.level < Msg.Level.Verbose)
      THEN  wr := NIL;
      ELSE  wr := Msg.writer;
    END;

    (* try to read the file *)
    TRY
      units := MxIn.ReadUnits (rd, file, imported, wr);
    FINALLY
      Utils.CloseReader (rd, info);
    END;
    IF (units = NIL) THEN
      IF (imported)
        THEN Msg.FatalError (NIL, "bad link info file: ", info);
        ELSE Msg.Debug ("bad link info file: ", info);
      END;
      RETURN NIL;
    END;

    IF (Msg.level >= Msg.Level.Verbose) THEN
      stop := ROUND (Time.Now ());
      Msg.Verbose ("reading \"", info, "\": ", Fmt.Int(stop-start), " seconds");
    END;
    RETURN units;
  END GetLinkUnits;

PROCEDURE MergeUnit (u: Mx.Unit;  optional := TRUE): BOOLEAN
  RAISES {Error} =
  VAR
    wr := Msg.writer;
    bad, ux: Mx.UnitList;
    x: Mx.Unit;
    ok := TRUE;
    ext: NK;
    ref: REFANY;
    map: IntRefTbl.T;
  BEGIN
    IF (u = NIL) THEN RETURN TRUE END;
    IF (optional) AND (Msg.level < Msg.Level.Debug) THEN wr := NIL END;

    bad := MxMerge.MergeUnit (u, link_base, wr);

    (* add u's magic info if it was ok *)
    ux := bad;
    LOOP
      IF (ux = NIL) THEN  AddMagic (u);  EXIT  END;
      IF (ux.unit = u) THEN EXIT END;
      ux := ux.next;
    END;

    IF (bad = NIL) THEN RETURN TRUE END;

    (* try to fix as many units as possible *)
    WHILE (bad # NIL) DO
      x := bad.unit;
      IF (x.interface)
        THEN map := interfaces;  ext := NK.I3;
        ELSE map := modules;     ext := NK.M3;
      END;
      IF (x # u) AND map.get (x.name, ref) THEN
        CompileOne (ref);
      ELSE
        IF (NOT optional) THEN
          Msg.FatalError (NIL, "bad version stamps: ",
                          M3Path.Join (NIL, M3ID.ToText (x.name),
                                       ext, host := FALSE));
        END;
        ok := FALSE
      END;
      bad := bad.next;
    END;

    RETURN ok;
  END MergeUnit;

PROCEDURE AddMagic (u: Mx.Unit) =
  VAR o := u.exported_objects;
  BEGIN
    WHILE (o # NIL) DO
      EVAL magic.put (o.type, o);
      o := o.next;
    END;
  END AddMagic;

(*----------------------------------------------------------- file names ---*)

PROCEDURE StdoutName (READONLY p: PassInfo;  base: TEXT): TEXT =
  BEGIN
    IF (Msg.level >= Msg.Level.Verbose) THEN RETURN NIL END;
    IF p.noise = NIL THEN RETURN NIL END;
    RETURN base & p.noise;
  END StdoutName;

PROCEDURE TempCName (f: FileInfo): TEXT =
  VAR ext := f.name.kind;  base: TEXT;  shorten := FALSE;
  BEGIN
    CASE ext OF
    | NK.I3, NK.IC => ext := NK.IC;
    | NK.IS        => ext := NK.IS;  shorten := TRUE;
    | NK.M3, NK.MC => ext := NK.MC;
    | NK.MS        => ext := NK.MS;  shorten := TRUE;
    ELSE <* ASSERT FALSE *>
    END;
    base := f.name.base;
    IF (shorten) AND (target_os = M3Path.OSKind.Win32) THEN
      base := ShortenName (f.name.base);
    END;
    RETURN M3Path.Join (NIL, base, ext, host := FALSE);
  END TempCName;

PROCEDURE TempSName (f: FileInfo): TEXT =
  VAR ext := f.name.kind;  base: TEXT;
  BEGIN
    CASE ext OF
    | NK.I3, NK.IC => ext := NK.IS;
    | NK.M3, NK.MC => ext := NK.MS;
    ELSE <* ASSERT FALSE *>
    END;
    base := f.name.base;
    IF (target_os = M3Path.OSKind.Win32) THEN
      base := ShortenName (f.name.base);
    END;
    RETURN M3Path.Join (NIL, base, ext, host := TRUE);
  END TempSName;

PROCEDURE ObjectName (f: FileInfo): TEXT =
  VAR ext := f.name.kind;  base: TEXT;  shorten: BOOLEAN := FALSE;
  BEGIN
    IF NOT bootstrap_mode THEN
      (* produce object modules *)
      CASE ext OF
      | NK.I3, NK.IC, NK.IS =>  ext :=  NK.IO;
      | NK.M3, NK.MC, NK.MS =>  ext :=  NK.MO;
      | NK.C, NK.S          =>  ext :=  NK.O;
      | NK.IO, NK.MO, NK.O  =>  RETURN f.source;
      ELSE RETURN NIL;
      END; 

    ELSIF (pass_7.cmd # NIL) THEN
      (* bootstrap with an assembler *)
      CASE ext OF 
      | NK.I3, NK.IC, NK.IS =>  ext :=  NK.IS;  shorten := TRUE;
      | NK.M3, NK.MC, NK.MS =>  ext :=  NK.MS;  shorten := TRUE;
      | NK.C, NK.S, NK.H    =>  (* skip *)
      | NK.IO, NK.MO, NK.O  =>  (* skip *)
      ELSE RETURN NIL; 
      END;

    ELSE
      (* bootstrap without an assembler *)
      CASE ext OF
      | NK.I3, NK.IC, NK.IS =>  ext :=  NK.IO;
      | NK.M3, NK.MC, NK.MS =>  ext :=  NK.MO;
      | NK.C, NK.S, NK.H    =>  (* skip *)
      | NK.IO, NK.MO, NK.O  =>  (* skip *)
      ELSE RETURN NIL;
      END; 

    END;

    base := f.name.base;
    IF (target_os = M3Path.OSKind.Win32) AND (shorten) THEN
      base := ShortenName (f.name.base);
    END;
    RETURN M3Path.Join (NIL, base, ext, host := FALSE);
  END ObjectName;

(*---------------------------------------------------------------------------*)
(* HACK: Masm 5.1 on NT doesn't generate case sensitive labels
         if the file name is longer than 13+3 !! *)
TYPE
  TruncatedName = REF RECORD
    full, short : TEXT;
    next : TruncatedName;
  END;

PROCEDURE ShortenName (n: TEXT): TEXT =
  CONST MaxName = 8;
  VAR
    buf: ARRAY [0..MaxName-1] OF CHAR;
    short_id: M3ID.T;
    ref: REFANY;
    cnt: INTEGER;
    name_list, t: TruncatedName;
  BEGIN
    IF Text.Length (n) < MaxName THEN RETURN n; END;

    IF (long_names = NIL) THEN
      long_names := NEW (IntRefTbl.Default).init ();
    END;

    (* fetch the list of truncations *)
    Text.SetChars (buf, n);
    short_id := M3ID.FromStr (buf);
    IF long_names.get (short_id, ref)
      THEN name_list := ref;
      ELSE name_list := NIL;
    END;

    (* search for a match *)
    t := name_list;  cnt := 0;
    WHILE (t # NIL) DO
      IF Text.Equal (n, t.full) THEN RETURN t.short END;
      t := t.next;  INC (cnt);
    END;

    (* need to create a new name *)
    IF (cnt > 0) THEN
      VAR
        new := Fmt.Int (cnt);
        len := Text.Length (new);
        num : ARRAY [0..LAST(buf)] OF CHAR;
      BEGIN
        Text.SetChars (num, new);
        FOR i := 0 TO len - 1 DO
          buf[NUMBER(buf) - len + i] := num[i];
        END;
      END;
    END;
    t := NEW (TruncatedName, full := n, short := Text.FromChars (buf),
                next := name_list);
    EVAL long_names.put (short_id, t);
    Msg.Verbose ("long name: ", n, " -> ", t.short);
    RETURN t.short;
  END ShortenName;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE PullForBootstrap (f: FileInfo;  text_file: BOOLEAN)
  RAISES {Error} =
  BEGIN
    IF NOT Text.Equal (f.source, f.object) THEN
      Utils.Remove (f.object);
      IF text_file AND NOT Text.Equal (Wr.EOL, Target.EOL)
        THEN Utils.CopyText (f.source, f.object);
        ELSE Utils.Copy (f.source, f.object);
      END;
    END;
  END PullForBootstrap;

PROCEDURE NewPass (cmd: TEXT): PassInfo =
  VAR p: PassInfo;
  BEGIN
    p.cmd   := cmd;
    p.args  := Arg.NewList ();
    RETURN p;
  END NewPass;

(*---------------------------------------------------------------- errors ---*)


PROCEDURE Duplicate (a, b: FileInfo;  tag: TEXT)
  RAISES {Error} =
  BEGIN
    Msg.FatalError (NIL, "duplicate ", tag, Fmt.F (": %s%s  %s%s  %s",
                                a.name.base, Wr.EOL, FName (a),
                                Wr.EOL, FName (b)));
    <*ASSERT FALSE*>
  END Duplicate;

PROCEDURE BadFile (msg: TEXT;  f: FileInfo)
  RAISES {Error} =
  BEGIN
    Msg.FatalError (NIL, msg, ": ", FName (f));
  END BadFile;

PROCEDURE DebugF (msg0: TEXT;  f: FileInfo;  msg1: TEXT := NIL) =
  BEGIN
    IF (Msg.level >= Msg.Level.Debug) THEN
      Msg.Debug (msg0, FName (f), msg1, Wr.EOL);
    END;
  END DebugF;

PROCEDURE ExplainF (msg: TEXT;  f: FileInfo) =
  BEGIN
    IF (Msg.level >= Msg.Level.Explain) THEN
      Msg.Explain (msg, FName (f));
    END;
  END ExplainF;

PROCEDURE VerboseF (msg: TEXT;  f: FileInfo) =
  BEGIN
    IF (Msg.level >= Msg.Level.Verbose) THEN
      Msg.Verbose (msg, FName (f));
    END;
  END VerboseF;

PROCEDURE FName (f: FileInfo): TEXT =
  BEGIN
    IF (f.source # NIL) AND (f.library # NIL) THEN
      RETURN Unit.GetRelativePath (f.source) & " in library " & f.library;
    ELSIF (f.source # NIL) THEN
      RETURN Unit.GetRelativePath (f.source);
    ELSIF (f.library # NIL) THEN
      RETURN M3Path.Join (f.name.dir, f.name.base, f.name.kind, host := TRUE) 
             & " in library " & f.library;
    ELSIF (f.name.base # NIL) THEN
      RETURN M3Path.Join (f.name.dir, f.name.base, f.name.kind, host := TRUE) 
    ELSE
      RETURN "???";
    END;
  END FName;

BEGIN
(***  DoIt ();***)
END M3Driver.



