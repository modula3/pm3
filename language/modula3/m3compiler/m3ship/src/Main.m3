(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Feb 22 13:07:25 PST 1995 by kalsow   *)
(*      modified on Thu Mar 31 17:38:23 PST 1994 by harrison *)

MODULE Main;

IMPORT Text, TextList, M3Config, Stdio, Wr, FS, OSError;
IMPORT Process, ASCII, Thread, Params, Pathname;
IMPORT Quake, M3File;

CONST
  SL         = M3Config.PATH_SEP;
  M3_VERSION = "SRC Modula-3 version XX.X";

TYPE
  DefineList = REF RECORD
    next   : DefineList;
    symbol : TEXT;
    value  : TEXT;
  END;

VAR
  default_template_dir := M3Config.PKG_USE &SL& "m3build" &SL& "templates";
  other_template_dir_1 := ".." & SL & "lib" & SL & "m3" & SL & "pkg" & SL &
      "m3build" & SL & "templates";
  other_template_dir_2 := ".." & SL & other_template_dir_1;

  template_dir    : TEXT := Env.Get("M3TEMPLATES");
  template        : TEXT := M3Config.BUILD_DIR;
  build_dir       : TEXT := M3Config.BUILD_DIR;
  start_dir       : TEXT := ".";
  base            : TEXT := NIL;
  parent          : TEXT := NIL;
  package_dir     : TEXT := NIL;
  package         : TEXT := NIL;
  shipfile        : TEXT := NIL;
  quake_preloads  : TextList.T := NIL;
  quake_defines   : DefineList := NIL;

VAR
  src_ok        : BOOLEAN := FALSE;
  verbose       : BOOLEAN := FALSE;
  quiet         : BOOLEAN := FALSE;
  ship_partial  : BOOLEAN := FALSE;
  

PROCEDURE Out (a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
  END Out;

PROCEDURE Err (a, b: TEXT := NIL) =
  BEGIN
    Out ("\nm3ship: ", a, b, "\n");
    Process.Exit (1);
  END Err;

PROCEDURE PrintUsage () =
  BEGIN
    Out ("usage: m3ship [options]\n");
    Out ("  -b <dir>   build with template <dir> in directory <dir>\n");
    Out ("               (default='", build_dir, "')\n");
    Out ("  -d <dir>   start in directory <dir> (default='", start_dir,"')\n");
    Out ("  -F <file>  prepend the quake code in <file>\n");
    Out ("  -T <dir>   use templates in directory <dir>\n");
    Out ("               (default='", default_template_dir, "')\n");
    Out ("  -S         build in 'src' directory\n");
    Out ("  -q         quiet\n");
    Out ("  -v         verbose\n");
    Out ("  -help      print this messsage\n");
    Out ("  -version   print version\n");
    Out ("  -<arg>     pass -<arg> to quake\n");
    Out ("  -k         ship partial\n");
    Out ("\n");
  END PrintUsage;

PROCEDURE ParseCommandLine () =
  VAR i := 1;  n := Params.Count;  arg: TEXT;
  BEGIN
    WHILE (i < n) DO
      arg := Params.Get (i);
      IF Text.Equal (arg, "-b") THEN
        INC (i);  IF (i >= n) THEN EXIT END;  arg := Params.Get (i);
        build_dir := arg;
        template  := arg;
      ELSIF Text.Equal (arg, "-T") THEN
        INC (i);  IF (i >= n) THEN EXIT END;  arg := Params.Get (i);
        template_dir := arg;
      ELSIF Text.Equal (arg, "-F") THEN
        INC (i);  IF (i >= n) THEN EXIT END;  arg := Params.Get (i);
        quake_preloads := TextList.Cons (arg, quake_preloads);
      ELSIF Text.Equal (arg, "-S") THEN
        src_ok := TRUE;
      ELSIF Text.Equal (arg, "-k") THEN
        ship_partial := TRUE;
      ELSIF Text.Equal (arg, "-d") THEN
        INC (i);  IF (i >= n) THEN EXIT END;  arg := Params.Get (i);
        start_dir := arg;
      ELSIF Text.Equal (arg, "-q") THEN
        quiet := TRUE;   verbose := FALSE;
      ELSIF Text.Equal (arg, "-v") THEN
        quiet := FALSE;  verbose := TRUE;
      ELSIF Text.Equal (arg, "-version") THEN
        Out ("m3ship: ", M3_VERSION, "\n");
        Process.Exit (0);
      ELSIF Text.Equal (arg, "-h") THEN
        PrintUsage ();
        Process.Exit (0);
      ELSIF Text.Equal (arg, "-help") THEN
        PrintUsage ();
        Process.Exit (0);
      ELSIF Text.Equal (arg, "-?") THEN
        PrintUsage ();
        Process.Exit (0);
      ELSIF Text.Equal (Text.Sub (arg, 0, 2), "-D") THEN
        AddDefine (Text.Sub (arg, 2));
      ELSIF Text.GetChar (arg, 0) = '-' THEN
        PrintUsage ();
        Err ("unrecognized option: ", arg);
      ELSE
        PrintUsage ();
        Err ("unrecognized option: ", arg);
      END;
      INC (i);
    END;
    quake_preloads := TextList.ReverseD (quake_preloads);
  END ParseCommandLine;

PROCEDURE AddDefine (defn: TEXT) =
  VAR sym, val: TEXT;  eq := Text.FindChar (defn, '=');
  BEGIN
    IF (eq < 0) THEN
      sym := defn;
      val := defn;
    ELSIF (eq = 0) THEN
      (* no name => ignore it *)
      RETURN;
    ELSE
      sym := Text.Sub (defn, 0, eq);
      val := Text.Sub (defn, eq+1);
    END;
    quake_defines := NEW (DefineList, next := quake_defines,
                          symbol := sym, value := val);
  END AddDefine;

PROCEDURE ChDir (dir: TEXT) =
  BEGIN
    TRY
      Process.SetWorkingDirectory (dir);
    EXCEPT OSError.E =>
      Err ("unable to move to directory: ", dir);
    END;
  END ChDir;

PROCEDURE MkDir (dir: TEXT) =
  BEGIN
    TRY
      FS.CreateDirectory (dir);
    EXCEPT OSError.E =>
      Err ("unable to create directory: ", dir);
    END;
  END MkDir;

PROCEDURE CheckTemplateDir() =
  VAR
    path, trial1, trial2: TEXT;
    trials: TextSeq.T;
    pos: INTEGER;
  BEGIN
    IF template_dir = NIL AND M3File.IsDirectory(default_template_dir) THEN
      template_dir := default_template_dir;
    END;

    IF template_dir = NIL THEN
      WITH m3buildPaths = NEW(TextSeq.T).init(),
           programDir = Pathname.Prefix(Params.Get(0)) DO
        trials := NEW(TextSeq.T).init();
        trials.addhi(default_template_dir);

        IF Pathname.Absolute(programDir) THEN
          m3buildPaths.addhi(programDir);
        ELSE
          m3buildPaths.addhi(Pathname.Join(Process.GetWorkingDirectory(),
              programDir));
          path := Env.Get("PATH");
          WHILE path # NIL DO
            IF Text.Length(path) = 0 THEN path := NIL;
            ELSE
              pos := Text.FindChar(path,':');
              IF pos < 0 THEN
                m3buildPaths.addhi(path);
                path := NIL;
              ELSIF pos = 0 THEN path := Text.Sub(path,1);
              ELSE
                m3buildPaths.addhi(Text.Sub(path,0,pos));
                path := Text.Sub(path,pos + 1);
              END;
           END;
         END;

         FOR i := 0 TO m3buildPaths.size() - 1 DO
           trial1 := Pathname.Join(m3buildPaths.get(i),other_template_dir_1);
           trial2 := Pathname.Join(m3buildPaths.get(i),other_template_dir_2);
           trials.addhi(trial1); trials.addhi(trial2);
           IF M3File.IsDirectory(trial1) THEN
             template_dir := trial1; RETURN;
           ELSIF M3File.IsDirectory(trial2) THEN
             template_dir := trial2; RETURN;
           END;
        END;
      END;
    END;

    IF template_dir = NIL THEN
      path := "";
      FOR i := TO trials.size() - 1 DO
        path := path & "\n" & trials.get(i);
      END;
      Err ("unable to find template directory", path);
    END;
  END CheckTemplateDir;

PROCEDURE GotoInitialDirectory () =
  BEGIN
    ChDir (start_dir);
    TRY
      start_dir := Process.GetWorkingDirectory ();
    EXCEPT OSError.E =>
      Err ("unable to get full directory path: ", start_dir);
    END;
    base   := Pathname.Last (start_dir);
    parent := Pathname.Prefix (start_dir);
  END GotoInitialDirectory;

PROCEDURE GotoBuildDir (dir: TEXT) =
  BEGIN
    IF M3File.IsDirectory (dir) THEN
      IF NOT quiet THEN Out ("--- shipping from ", dir, " ---\n"); END;
      ChDir (dir);
    ELSIF M3File.IsReadable (template_dir & SL & template) THEN
      IF NOT quiet THEN Out ("mkdir ", dir, "\n"); END;
      MkDir (dir);
      IF NOT quiet THEN Out ("--- shipping from ", dir, " ---\n"); END;
      ChDir (dir);
    ELSE
      Err ("cannot locate build directory ", template);
    END;
  END GotoBuildDir;

PROCEDURE PathEQ (a, b: TEXT): BOOLEAN =
  VAR
    len := Text.Length (a);
    ac, bc: CHAR;
  BEGIN
    IF (len # Text.Length (b)) THEN RETURN FALSE; END;
    FOR i := 0 TO len - 1 DO
      ac := ASCII.Upper [Text.GetChar (a, i)];
      bc := ASCII.Upper [Text.GetChar (b, i)];
      IF ac # bc THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END PathEQ;

PROCEDURE GotoDerivedDirectory () =
  BEGIN
    IF src_ok THEN
      (* assume that we're in an immediate subdirectory of the package *)
      package_dir := parent;
      build_dir   := base;
    ELSIF PathEQ (base, "src") THEN
      package_dir := parent;
      GotoBuildDir (".." & SL & build_dir);
    ELSIF PathEQ (base, build_dir) THEN
      package_dir := parent;
    ELSE
      package_dir := start_dir;
      GotoBuildDir (build_dir);
    END;
    package := Pathname.Last (package_dir);
  END GotoDerivedDirectory;

PROCEDURE CheckForShipfile () =
  BEGIN
    IF M3File.IsReadable (".M3SHIP") THEN
      shipfile        := package_dir &SL& build_dir &SL& ".M3SHIP";
    ELSE
      Err ("cannot locate a .M3SHIP file, run m3build first");
    END;
  END CheckForShipfile;

PROCEDURE CheckForOverridesFile () =
  BEGIN
    IF M3File.IsReadable (".M3OVERRIDES") AND NOT ship_partial THEN
      Err ("package built with overrides - not shipping");
    END;
  END CheckForOverridesFile;

PROCEDURE QDefine (m: Quake.Machine;  sym, val: TEXT) RAISES {Quake.Error} =
  BEGIN
    IF (verbose) THEN Out (sym, " = ", val, "\n") END;
    Quake.Define (m, sym, val);
  END QDefine;

PROCEDURE QRun (m: Quake.Machine;  file: TEXT) RAISES {Quake.Error} =
  BEGIN
    IF (verbose) THEN Out ("EVAL (", file, ")\n") END;
    Quake.Run (m, file);
  END QRun;

PROCEDURE RunQuake (build_target: TEXT) =
  VAR
    mach := Quake.NewMachine ();
    defn : DefineList;
    load : TextList.T;
  BEGIN
    TRY
      TRY
        (* preload the environment *)
        QDefine (mach, "_" & build_target, "TRUE");
        IF (quiet) THEN QDefine (mach, "_quiet", "TRUE"); END;
        QDefine (mach, "PACKAGE_DIR", package_dir);
        QDefine (mach, "PACKAGE", package);
        QDefine (mach, "BUILD_DIR", build_dir);
        QRun (mach, template_dir &SL& template);

        (* add the user defined symbols *)
        defn := quake_defines;
        WHILE (defn # NIL) DO
          QDefine (mach, defn.symbol, defn.value);
          defn := defn.next;
        END;

        (* add the user defined preloads *)
        load := quake_preloads;
        WHILE (load # NIL) DO
          QRun (mach, load.head);
          load := load.tail;
        END;

        (* run the user's shipfile *)
        QRun (mach, shipfile);

        (* run the cleanup template *)
        QRun (mach, template_dir &SL& "CLEANUP");
  
      FINALLY
        (* free any temp files & garbage *)
        Quake.Done (mach);
      END;

    EXCEPT Quake.Error(msg) =>
      Err ("quake error: ", msg);
    END;
  END RunQuake;

BEGIN
  ParseCommandLine ();
  CheckTemplateDir();
  GotoInitialDirectory ();
  GotoDerivedDirectory ();
  CheckForShipfile ();
  CheckForOverridesFile ();
  RunQuake("ship");
END Main.
