(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Feb 22 13:07:25 PST 1995 by kalsow   *)
(*      modified on Thu Mar 31 17:38:23 PST 1994 by harrison *)

MODULE Main;

IMPORT Text, TextList, M3Config, Stdio, Wr, OSError;
IMPORT Process, ASCII, Thread, Params, Pathname;
IMPORT Quake, M3File, TempFiles, Env, FileWr, BldQuake;

CONST
  SL         = M3Config.PATH_SEP;
  M3_VERSION = "PM3 Version " & M3Config.M3_VERSION &
               " (" & M3Config.M3_VERSION_DATE & ")";

TYPE
  DefineList = REF RECORD
    next   : DefineList;
    symbol : TEXT;
    value  : TEXT;
  END;

VAR
  default_template_dir := M3Config.PKG_USE &SL& "m3config" &SL& "src";
  template_dir    : TEXT := NIL;
  template        : TEXT := NIL;
  build_dir       : TEXT := NIL;
  start_dir       : TEXT := ".";
  base            : TEXT := NIL;
  parent          : TEXT := NIL;
  package_dir     : TEXT := NIL;
  package         : TEXT := NIL;
  show_hidden     : TEXT := "";
  query           : TEXT := NIL;
  quake_preloads  : TextList.T := NIL;
  quake_defines   : DefineList := NIL;
  units           : TextList.T;
  verbose         : BOOLEAN := FALSE;
  quiet           : BOOLEAN := FALSE;
  

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
    Out ("\nm3where: ", a, b, "\n");
    Process.Exit (1);
  END Err;

PROCEDURE PrintUsage () =
  BEGIN
    Out ("usage: m3where [options] [units]\n");
    Out ("  -b <dir>   build with template <dir> in directory <dir>\n");
    Out ("               (default='", build_dir, "')\n");
    Out ("  -d <dir>   start in directory <dir> (default='", start_dir,"')\n");
    Out ("  -F <file>  prepend the quake code in <file>\n");
    Out ("  -T <dir>   use templates in directory <dir>\n");
    Out ("               (default='", default_template_dir, "')\n");
    Out ("  -q         quiet\n");
    Out ("  -v         verbose\n");
    Out ("  -help      print this messsage\n");
    Out ("  -version   print version\n");
    Out ("  -<arg>     pass -<arg> to quake\n");
    Out ("  -h         show hidden\n");
    Out ("  <unit>     call quake with each unit\n");
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
      ELSIF Text.Equal (arg, "-d") THEN
        INC (i);  IF (i >= n) THEN EXIT END;  arg := Params.Get (i);
        start_dir := arg;
      ELSIF Text.Equal (arg, "-q") THEN
        quiet := TRUE;   verbose := FALSE;
      ELSIF Text.Equal (arg, "-v") THEN
        quiet := FALSE;  verbose := TRUE;
      ELSIF Text.Equal (arg, "-version") THEN
        Out ("m3where: ", M3_VERSION, "\n");
        Process.Exit (0);
      ELSIF Text.Equal (arg, "-h") THEN
        show_hidden := "show_hidden";
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
        units := TextList.Cons (arg, units);
      END;
      INC (i);
    END;
    quake_preloads := TextList.ReverseD (quake_preloads);
    units := TextList.ReverseD (units);
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
      IF NOT quiet THEN Out ("--- searching in ", dir, " ---\n"); END;
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
    IF PathEQ (base, "src") THEN
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

PROCEDURE CheckForExportsfile () =
  <*FATAL OSError.E*>
  BEGIN
    IF NOT M3File.IsReadable (".M3EXPORTS") THEN
      Err ("missing .M3EXPORTS file in " & Process.GetWorkingDirectory());
    END;
  END CheckForExportsfile;

PROCEDURE BuildQuery () =
  VAR
    wr: Wr.T;
  BEGIN
    query := TempFiles.Get(Env.Get("TEMP"),"m3","qk");
    TempFiles.Note(query);
    TRY
      wr := FileWr.Open(query);

      Wr.PutText(wr,"%% ");
      FOR i := 0 TO Params.Count - 1 DO
        Wr.PutText(wr,Params.Get(i) & " ");
      END;
      Wr.PutText(wr,"\n");

      IF units = NIL THEN
        Wr.PutText(wr,"enum_units (\"" & show_hidden & "\")\n");
      ELSE
        WHILE units # NIL DO
          Wr.PutText(wr,"find_unit (\"" & units.head & "\", \"" & show_hidden &
              "\")\n");
          units := units.tail;
        END;
      END;
      Wr.Close(wr);
    EXCEPT ELSE
      Err("writing query in " & query);
    END;
  END BuildQuery;

PROCEDURE QDefine (m: Quake.Machine;  sym, val: TEXT) RAISES {Quake.Error} =
  BEGIN
    IF (verbose) THEN Out (sym, " = ", val, "\n") END;
    Quake.Define (m, sym, val);
  END QDefine;

PROCEDURE QRun (m: Quake.Machine;  file: TEXT) RAISES {Quake.Error} =
  BEGIN
    IF (verbose) THEN Out ("EVAL (", file, ")\n") END;
    Quake.RunSourceFile (m, file);
  END QRun;

PROCEDURE RunQuake () =
  VAR
    mach : BldQuake.T;
    defn : DefineList;
    load : TextList.T;
  BEGIN
    TRY
      mach := NEW(BldQuake.T).init(Stdio.stdout, package, package_dir, 
                                   build_dir);
      TRY
        (* preload the environment *)
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

        (* setup *)
        mach.setup();

        (* run the user's shipfile *)
        QRun (mach, ".M3EXPORTS");
        QRun (mach, query);
      FINALLY
        (* free any temp files & garbage *)
        Quake.Done (mach);
      END;

    EXCEPT Quake.Error(msg) =>
      Err ("quake error: ", msg);
    END;
  END RunQuake;

PROCEDURE FindTemplateDir()=
  VAR 
    m3_template := Env.Get("M3_TEMPLATE_DIR");
    path        : TEXT;
    subpath     : TEXT;
    nextsep     : INTEGER := 0;
    prevsep     : INTEGER := 0;
    sep         : CHAR;
  BEGIN
    IF m3_template # NIL THEN
      template_dir := m3_template;
    ELSE
      path := Env.Get("PATH");
      IF Text.Equal(M3Config.OS_TYPE, "POSIX") THEN
        sep := ':';
      ELSIF Text.Equal(M3Config.OS_TYPE, "WIN32") THEN
        sep := ';';
      ELSE
        Err("FindTemplate does not know how to handle OS_TYPE: ", 
            M3Config.OS_TYPE);
      END;
      REPEAT
        nextsep := Text.FindChar(path, sep, prevsep + 1);
        IF nextsep # -1 THEN
          subpath := Text.Sub(path, prevsep, nextsep - prevsep - 1);
        ELSE
          subpath := Text.Sub(path, prevsep);
        END;
        IF M3File.IsReadable(subpath & template) THEN
          template_dir := subpath;
          RETURN;
        END;
        prevsep := nextsep;
      UNTIL prevsep = -1;
      IF template_dir = NIL THEN
        template_dir := default_template_dir;
      END;
    END;
  END FindTemplateDir;

PROCEDURE FindTemplate()=
  VAR 
    m3_template := Env.Get("M3_TEMPLATE");
  BEGIN
    IF m3_template # NIL THEN
      template     := Pathname.Last(m3_template);
    ELSE
      template     := M3Config.BUILD_DIR;
    END;
    build_dir    := template;
  END FindTemplate;

BEGIN
  ParseCommandLine ();
  IF template = NIL THEN
    FindTemplate();
  END;
  IF template_dir = NIL THEN
    FindTemplateDir();
  END;
  GotoInitialDirectory ();
  GotoDerivedDirectory ();

  IF NOT Pathname.Absolute(template_dir) THEN
    template_dir := package_dir & SL & build_dir & SL & template_dir;
  END;

  CheckForExportsfile ();
  BuildQuery ();
  RunQuake();
END Main.
