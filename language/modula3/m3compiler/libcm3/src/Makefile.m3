(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Makefile;

IMPORT FS, M3File, M3Timers, OSError, Params, Process, Text, Thread, Wr;
IMPORT Arg, M3Options, M3Path, Msg, Utils, TextTextTbl;
IMPORT MxConfig AS M3Config;

TYPE
  NK = M3Path.Kind;
  MM = M3Options.Mode;

CONST
  ModeName = ARRAY MM OF TEXT { "-build", "-clean", "-ship", "-find", 
                                "-depend" };
  ModeFlag = ARRAY MM OF TEXT { "_all",   "_clean", "_ship", "_find", 
                                "_depend" };

TYPE
  State = RECORD
    args          : Arg.List := NIL;
    wr            : Wr.T     := NIL;
    keep_files    : BOOLEAN  := FALSE;
    use_overrides : BOOLEAN  := FALSE;
    mode_set      : BOOLEAN  := FALSE;
    found_work    : BOOLEAN  := FALSE;
  END;

PROCEDURE Build (args: Arg.List;  src_dir: TEXT): TEXT =
  CONST Makefile = "m3make.args";
  VAR
    s: State;
    src_makefile := M3Path.New (src_dir, "m3makefile");
    src_overrides := M3Path.New (src_dir, "m3overrides");

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      s.wr := wr;
      Out (wr, "set_config_options ()");
      Out (wr, "readonly ", ModeFlag [M3Options.major_mode],
           " = TRUE % cm3 ", ModeName [M3Options.major_mode]);

      CASE M3Options.major_mode OF
      | MM.Find => Out (wr, "M3_FIND_UNITS = []")
      | MM.Build, MM.Clean, MM.Ship, MM.Depend =>  (* skip *)
      END;

      ConvertArgList (s);

      CASE M3Options.major_mode OF
      | MM.Build, MM.Clean, MM.Find, MM.Depend =>
          IncludeOverrides (s, src_overrides);
          IncludeMakefile (s, src_makefile, src_dir);

      | MM.Ship =>
          IF M3File.IsReadable (".M3OVERRIDES") THEN
            Msg.Out ("package was built with overrides, not shipping.", Wr.EOL);
          ELSIF NOT M3File.IsReadable (".M3SHIP") THEN
            Msg.Out ("missing \".M3SHIP\" file, build the package first.", Wr.EOL);
          ELSE
            Out (wr, "include (\".M3SHIP\")");
            s.found_work := TRUE;
          END;
      END;
    END Emit;

  BEGIN
    s.args := args;

    Utils.WriteFile (Makefile, Emit, append := FALSE);

    IF s.found_work THEN
      IF NOT s.keep_files THEN Utils.NoteTempFile (Makefile); END;
      RETURN Makefile;
    ELSE
      Msg.Out ("cm3: found nothing to build.", Wr.EOL);
      IF NOT s.keep_files THEN Utils.Remove (Makefile); END;
      RETURN NIL;
    END;
  END Build;

(*---------------------------------------------------------- internal ---*)

PROCEDURE ConvertArgList (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len: INTEGER;  arg: TEXT;
  BEGIN
    WHILE (s.args.cnt > 0) DO
      arg := Arg.Pop (s.args);
      len := Text.Length (arg);
      IF (len < 1) THEN
        (* empty argument ignore *)
      ELSIF (Text.GetChar (arg, 0) # '-') OR (len < 2) THEN
        NoteSourceFile (s, NIL, arg, cmd_line := TRUE);
      ELSE (* it's an option *)
        ConvertOption (s, arg, len);
      END;
    END;
  END ConvertArgList;

PROCEDURE ConvertOption (VAR s: State;  arg: TEXT;  arg_len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ok := FALSE;  wr := s.wr;
  BEGIN
    CASE Text.GetChar (arg, 1) OF

    | '?' => IF (arg_len = 2) THEN
               ok := TRUE;  (* printed during the pre-scan *)
             END;

    | 'a' => IF (arg_len = 2) THEN
               Out (wr, "library (\"", GetArg (arg, s.args), "\")");  ok := TRUE;
               s.found_work := TRUE;
             END;

    | 'A' => IF (arg_len = 2) THEN
               Out (wr, "M3_OPTIONS += \"-NoAsserts\"");  ok := TRUE;
             END;

    | 'b' => IF Text.Equal (arg, "-boot") THEN
               Out (wr, "M3_BOOTSTRAP = TRUE");  ok := TRUE;
             ELSIF Text.Equal(arg, "-build") THEN
               ok := TRUE; (* mode set during the pre-scan *)
             END;

    | 'c' => IF (arg_len = 2) THEN
               Out (wr, "m3_compile_only ()");  ok := TRUE;
               s.found_work := TRUE;
             ELSIF Text.Equal (arg, "-commands") THEN
               Msg.SetLevel (Msg.Level.Commands);  ok := TRUE;
               Out (wr, "m3_option (\"-commands\")");
             ELSIF Text.Equal (arg, "-config") THEN
               ok := TRUE; (* printed during the pre-scan *)
             ELSIF Text.Equal (arg, "-console") THEN
               Out (wr, "M3_WINDOWS_GUI = FALSE");  ok := TRUE;
             ELSIF Text.Equal(arg, "-clean") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 'd' => IF Text.Equal(arg, "-debug") THEN
               Msg.SetLevel (Msg.Level.Debug);  ok := TRUE;
               Out (wr, "m3_option (\"-debug\")");
             ELSIF Text.Equal (arg, "-depend") THEN
               ok := TRUE;
             END;

    | 'D' => ProcessDefine (arg, wr);  ok := TRUE;

    | 'f' => IF Text.Equal(arg, "-find") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 'g' => IF (arg_len = 2) THEN
               Out (wr, "m3_debug (TRUE)");  ok := TRUE;
             ELSIF Text.Equal(arg, "-gui") THEN
               Out (wr, "M3_WINDOWS_GUI = TRUE");  ok := TRUE;
             END;

    | 'h' => IF Text.Equal (arg, "-heap_stats") THEN
               M3Options.heap_stats := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-help") THEN
               ok := TRUE;  (* printed during the pre-scan *)
             END;

    | 'k' => IF (arg_len = 2) OR Text.Equal (arg, "-keep") THEN
               Out (wr, "M3_KEEP_FILES = TRUE");
               s.keep_files := TRUE;  ok := TRUE;
             END;

    | 'o' => IF (arg_len = 2) THEN
               Out (wr, "program (\"", GetArg (arg, s.args), "\")");  ok := TRUE;
               s.found_work := TRUE;
             ELSIF Text.Equal (arg, "-override") THEN
               s.use_overrides := TRUE;  ok := TRUE;
             ELSIF Text.Equal (arg, "-once") THEN
               Out (wr, "M3_COMPILE_ONCE = TRUE");  ok := TRUE;
             END;

    | 'p' => IF Text.Equal(arg, "-pretend") OR 
                Text.Equal(arg, "-profile") THEN
               ok := TRUE;
             END;

    | 'O' => IF (arg_len = 2) THEN
               Out (wr, "m3_optimize (TRUE)");  ok := TRUE;
             END;

    | 's' => IF Text.Equal (arg, "-silent") THEN
               Msg.SetLevel (Msg.Level.Silent);  ok := TRUE;
               Out (wr, "m3_option (\"-silent\")");
             ELSIF Text.Equal (arg, "-skiplink") THEN
               Out (wr, "M3_SKIP_LINK = TRUE");  ok := TRUE;
             ELSIF Text.Equal (arg, "-ship") THEN
               ok := TRUE;  (* mode set during the pre-scan *)
               s.found_work := TRUE;
             END;

    | 't' => IF Text.Equal (arg, "-times") THEN
               M3Timers.Start ();  ok := TRUE;
             END;

    | 'v' => IF Text.Equal (arg, "-verbose") THEN
               Msg.SetLevel (Msg.Level.Verbose);
               Out (wr, "m3_option (\"-verbose\")");
               M3Timers.Start ();
               ok := TRUE;
             ELSIF Text.Equal (arg, "-version") THEN
               ok := TRUE;  (* printed during the pre-scan *)
             END;

    | 'w' => IF Text.Equal (arg, "-why") THEN
               Msg.SetLevel (Msg.Level.Explain);  ok := TRUE;
               Out (wr, "m3_option (\"-why\")");
             ELSIF Text.Equal (arg, "-windows") THEN
               Out (wr, "M3_WINDOWS_GUI = TRUE");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w0") THEN
               Out (wr, "M3_OPTIONS += \"-w0\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w1") THEN
               Out (wr, "M3_OPTIONS += \"-w1\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w2") THEN
               Out (wr, "M3_OPTIONS += \"-w2\"");  ok := TRUE;
             ELSIF Text.Equal (arg, "-w3") THEN
               Out (wr, "M3_OPTIONS += \"-w3\"");  ok := TRUE;
             END;

    | 'x' => IF (arg_len = 2) THEN
               s.use_overrides := TRUE;  ok := TRUE;
             END;

    | 'Z' => IF (arg_len = 2) THEN
               Out (wr, "M3_COVERAGE = TRUE");  ok := TRUE;
             END;

    ELSE (* error *)
    END;

    IF (NOT ok) THEN Msg.UsageError ("unrecognized option \"", arg, "\"") END;
  END ConvertOption;

PROCEDURE GetArg (arg: TEXT;  rest: Arg.List): TEXT =
  BEGIN
    IF (rest.cnt <= 0) THEN
      Msg.UsageError ("missing argument to \"", arg, "\" option");
    END;
    RETURN Arg.Pop (rest);
  END GetArg;

PROCEDURE ProcessDefine (arg: TEXT;  wr: Wr.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len := Text.Length (arg);  eq: INTEGER;  sym, val: TEXT;
  BEGIN
    IF (len <= 2) THEN
      Msg.UsageError ("missing argument to \"-D\" option");
      RETURN;
    END;

    eq := Text.FindChar (arg, '=');
    IF (eq < 0) THEN
      (* -Dsymbol ==>  symbol = TRUE *)
      Out (wr, Text.Sub (arg, 2), " = TRUE");
      RETURN;
    END;

    sym := Text.Sub (arg, 2, eq-2);
    val := Text.Sub (arg, eq+1);
    len := Text.Length (val);

    IF (len = 0) THEN
      (* -Dsymbol=   ==> symbol = "" *)
      Out (wr, sym, " = \"\"");

    ELSIF Text.GetChar (arg, 0) = '"'
      AND Text.GetChar (arg, len-1) = '"' THEN
      (* -Dsymbol="foo" ==> symbol = "foo" *)
      Out (wr, sym, " = ", val);

    ELSIF Text.Equal (val, "TRUE") OR Text.Equal (val, "FALSE") THEN
      Out (wr, sym, " = ", val);

    ELSE
      (* -Dsymbol=val  ==> symbol = "val" *)
      Out (wr, sym, " = \"", val, "\"");

    END;
  END ProcessDefine;

CONST
  SourceTag = ARRAY NK OF TEXT {
    NIL,                                              (* unknown *)
    "interface", NIL, NIL, "import_obj",              (* i3, ic, is, io *)
    "implementation", NIL, NIL, "import_obj",         (* m3, mc, ms, mo *)
    "generic_interface", "generic_implementation",    (* ig, mg *)
    "c_source", "h_source", "s_source", "import_obj", (* c, h, s, o *)
    "import_lib", "import_lib", NIL,                  (* m3lib, lib, m3x *)
    NIL, NIL, NIL                                     (* pgm, mx, tmpl *)
  };

PROCEDURE NoteSourceFile (VAR s: State;  dir, name: TEXT;  cmd_line: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    file := M3Path.New (dir, name);
    info := M3Path.Parse (file, host := TRUE);
    tag  : TEXT;
  BEGIN
    Msg.Debug ("  file ", file, Wr.EOL);

    IF (M3Options.major_mode = MM.Find) AND (cmd_line) THEN
      Out (s.wr, "M3_FIND_UNITS += \"", M3Path.Join (NIL, info.base, info.kind,
                                             host := TRUE), "\"");
      s.found_work := TRUE;
      RETURN;
    END;

    tag := SourceTag [info.kind];
    IF (tag # NIL) THEN
      file := M3Path.Escape (M3Path.New (info.dir, info.base));
      Out (s.wr, tag, " (\"", file, "\")");
      s.found_work := TRUE;
    ELSE
      VisitSourceDir (s, file, cmd_line);
    END;
  END NoteSourceFile;

PROCEDURE VisitSourceDir (VAR s: State;  dir: TEXT;  cmd_line: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR iter: FS.Iterator;  name: TEXT;
  BEGIN
    Msg.Debug ("--- dir ", dir, " ---", Wr.EOL);
    IF NOT M3File.IsDirectory (dir) THEN
      IF (cmd_line) THEN
        Msg.FatalError (NIL, "unsupported file type \"", dir, "\"");
      END;
      Msg.Verbose ("ignoring ", dir, " (not a directory)");
      RETURN;
    END;
    TRY
      Msg.Verbose ("Looking in ", dir);
      iter := FS.Iterate (dir);
      TRY
        WHILE iter.next (name) DO
          NoteSourceFile (s, dir, name, cmd_line := FALSE);
        END;
      FINALLY
        iter.close();
      END;
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to scan directory \"", dir, "\"");
    END;
  END VisitSourceDir;

PROCEDURE IncludeOverrides (VAR s: State;  overrides: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF M3File.IsReadable (overrides) THEN
      IF (s.use_overrides) THEN
        Out (s.wr, "include (\"", M3Path.Escape (overrides), "\")");
        s.found_work := TRUE;
      ELSE
        IF (M3Options.major_mode = MM.Depend) THEN
          Msg.Verbose ("ignoring ", overrides, Wr.EOL);
        ELSE
          Msg.Out ("ignoring ", overrides, Wr.EOL);
        END;
      END;
    ELSE
      IF (s.use_overrides) THEN
        IF (M3Options.major_mode = MM.Depend) THEN
          Msg.Verbose ("unable to read ", overrides,
                       ", options \"-override\" and \"-x\" ignored.", Wr.EOL);
        ELSE
          Msg.Out ("unable to read ", overrides,
                   ", options \"-override\" and \"-x\" ignored.", Wr.EOL);
        END;
      END;
    END;
  END IncludeOverrides;

PROCEDURE IncludeMakefile (VAR s: State;  makefile, dir: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF M3File.IsReadable (makefile) THEN
      Out (s.wr, "include_dir (\"", M3Path.Escape (dir), "\")");
      s.found_work := TRUE;
    ELSE
      Out (s.wr, "import (\"libm3\")");
      VisitSourceDir (s, dir, cmd_line := FALSE);
      Out (s.wr, "program (\"prog\")");
    END;
  END IncludeMakefile;

(*----------------------------------------- pre-scan command line ---*)

PROCEDURE ScanCommandLine () : TextTextTbl.T =
  VAR 
    cnt := 0;  arg: TEXT;
  BEGIN
    FOR i := 1 TO Params.Count-1 DO
      arg := Params.Get (i);
      IF    Text.Equal (arg, "-build")   THEN  SetMode (cnt, MM.Build);
      ELSIF Text.Equal (arg, "-clean")   THEN  SetMode (cnt, MM.Clean);
      ELSIF Text.Equal (arg, "-find")    THEN  SetMode (cnt, MM.Find);
      ELSIF Text.Equal (arg, "-ship")    THEN  SetMode (cnt, MM.Ship);
      ELSIF Text.Equal (arg, "-depend")  THEN  SetMode (cnt, MM.Depend);
      ELSIF Text.Equal (arg, "-?")       THEN  PrintHelp ();
      ELSIF Text.Equal (arg, "-help")    THEN  PrintHelp ();
      ELSIF Text.Equal (arg, "-config")  THEN  PrintVersion (TRUE);
      ELSIF Text.Equal (arg, "-version") THEN  PrintVersion (TRUE);
      ELSIF Text.Equal (arg, "-profile") THEN
        EVAL defs.put("M3_PROFILING", "TRUE");
      ELSIF Text.Equal (arg, "-pretend") THEN
        IF i < Params.Count - 1 THEN
          EVAL defs.put("CM3_VERSION", Params.Get(i+1));
        ELSE
          Msg.Error(NIL, "missing argument for -pretend");
        END;
      END;
    END;
    IF (cnt <= 0) THEN SetMode (cnt, MM.Build); END;
    RETURN defs;
  END ScanCommandLine;

PROCEDURE SetMode (VAR cnt: INTEGER;  mode: MM) =
  BEGIN
    INC (cnt);
    IF (cnt > 1) THEN
      Msg.Error (NIL, "mode \"", ModeName [M3Options.major_mode],
                 "\" already set, \"", ModeName [mode] & "\" ignored.");
    ELSE
      M3Options.major_mode := mode;
    END;
  END SetMode;

PROCEDURE PrintVersion (exit: BOOLEAN) =
  BEGIN
    Msg.Out ("Critical Mass Modula-3 version ", Val("CM3_RELEASE"), Wr.EOL);
    Msg.Out ("  last updated: ", Val("CM3_CREATED"), Wr.EOL);
    Msg.Out ("  configuration: ", M3Config.FindFile(), Wr.EOL);
    Msg.Out (Wr.EOL);
    IF exit THEN Process.Exit (0); END;
  END PrintVersion;

PROCEDURE PrintHelp () =
  BEGIN
    PrintVersion (FALSE);
    FOR i := FIRST (HelpMsg) TO LAST (HelpMsg) DO
      Msg.Out (HelpMsg[i], Wr.EOL);
    END;
    Process.Exit (0);
  END PrintHelp;

CONST
  HelpMsg = ARRAY OF TEXT {
    "command line options:",
    "",
    "modes:  (default: -build)",
    "  -build         compile and link",
    "  -ship          install package",
    "  -clean         delete derived files",
    "  -find          locate source files",
    "  -depend        output package dependencies",
    "",
    "compile options:  (default: -g -w1)",
    "  -g             produce symbol table information for debugger",
    "  -O             optimize code",
    "  -A             disable code generation for assertions",
    "  -once          don't recompile to improve opaque object code",
    "  -w0 .. -w3     limit compiler warning messages",
    "  -Z             generate coverage analysis code",
    "  -profile       generate profiling code",
    "",
    "program and library options:  (default: -o prog)",
    "  -c             compile only, produce no program or library",
    "  -a <foo>       build library <foo>",
    "  -o <foo>       build program <foo>",
    "  -skiplink      skip the final link step",
    "",
    "messages:  (default: -why)",
    "  -silent        produce no diagnostic output",
    "  -why           explain why code is being recompiled",
    "  -commands      list system commands as they are performed",
    "  -verbose       list internal steps as they are performed",
    "  -debug         dump internal debugging information",
    "",
    "information and help:",
    "  -help          print this help message",
    "  -?             print this help message",
    "  -version       print the version number header",
    "  -config        print the version number header",
    "",
    "misc:",
    "  -keep          preserve intermediate and temporary files",
    "  -times         produce a dump of elapsed times",
    "  -override      include the \".m3overrides\" file",
    "  -x             include the \".m3overrides\" file",
    "  -D<symbol>     define <symbol> with the value TRUE",
    "  -D<sym>=<val>  define <sym> with the value <val>",
    "  -console       produce a Windows CONSOLE subsystem program",
    "  -gui           produce a Windows GUI subsystem program",
    "  -windows       produce a Windows GUI subsystem program",
    "  -pretend <val> pretend to run as CM3_Version <val>",
    ""
  };

(*---------------------------------------------------------- misc ---*)

PROCEDURE Out (wr: Wr.T;  a, b, c, d, e: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    IF (e # NIL) THEN Wr.PutText (wr, e); END;
    Wr.PutText (wr, Wr.EOL);
  END Out;

PROCEDURE Val(name: TEXT) : TEXT =
  VAR res: TEXT := "undefined";
  BEGIN
    EVAL defs.get(name, res);
    RETURN res;
  END Val;

VAR
  defs := NEW(TextTextTbl.Default).init();
BEGIN
  EVAL defs.put("CM3_RELEASE", "5.1.8");       (* readable release version *)
  EVAL defs.put("CM3_VERSION", "050108");      (* version as number *)
  EVAL defs.put("CM3_CREATED", "2001-12-19");  (* date of last change *)
  EVAL defs.put("M3_PROFILING", "");           (* no profiling by default *)
  EVAL defs.put("EOL", Wr.EOL);
END Makefile.
