(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Feb 22 13:07:25 PST 1995 by kalsow   *)
(*      modified on Thu Mar 31 17:38:23 PST 1994 by harrison *)

MODULE Main;

IMPORT Text, TextList, Wr, OSError, Env, RTutils, RTCollector;
IMPORT Process, Thread, Params, Pathname, RTCollectorSRC, MxConfig;
IMPORT Quake, M3Timers, Utils, RTParams, Makefile;
IMPORT Builder, M3Build, Dirs, M3Options, Msg, WebFile, Arg, TextTextTbl;

VAR
  config          : TEXT          := NIL;
  makefile        : TEXT          := NIL;
  build_dir       : TEXT          := NIL;
  mach            : Quake.Machine := NIL;
  template_dir    : TEXT          := NIL;
  template        : TEXT          := NIL;
  start_dir       : TEXT          := ".";
  quake_preloads  : TextList.T    := NIL;
  args            : Arg.List      := Arg.NewList ();
  defs            : TextTextTbl.T := NIL;
  
PROCEDURE DoIt () =
  BEGIN
    RTCollectorSRC.DisableVM ();
    IF RTParams.IsPresent ("verbose") THEN
      Msg.SetLevel (Msg.Level.Verbose);
      M3Timers.Start ();
    END;
    IF RTParams.IsPresent ("debug") THEN
      Msg.SetLevel (Msg.Level.Debug);
      M3Timers.Start();
    END;
    Process.RegisterExitor (CleanUp);

    config := MxConfig.FindFile();
    IF (config = NIL) THEN
      Msg.FatalError (NIL, "unable to locate configuration file, \"",
                      MxConfig.Filename, "\"");
    END;

    mach := M3Build.NewMachine ();
    TRY
      TRY
        (* figure out what we're trying to do *)
        VAR
          name, val: TEXT;
          iter := defs.iterate();
        BEGIN
          WHILE iter.next(name, val) DO
            Quake.Define(mach, name, val);
          END;
        END;

        (* define the site configuration *)
        Msg.Verbose ("EVAL (\"", config, "\")");
        Quake.Run (mach, config);

        (* -- disabled 
        CheckExpire (Quake.LookUp (mach, "INSTALL_KEY"));
        *)

        (* figure out where we are and get where we want to be *)
        build_dir := Quake.LookUp (mach, "BUILD_DIR");
        IF (build_dir = NIL) THEN
          Msg.FatalError (NIL, "configuration file didn't specify BUILD_DIR");
        END;
        Dirs.SetUp (build_dir);

        (* define the "builtin" quake functions *)
        M3Build.SetUp (mach, Dirs.package, Dirs.to_package,
                       Pathname.Last (Dirs.derived));

        (* add the user defined preloads *)
        VAR load := quake_preloads;
        BEGIN
          WHILE (load # NIL) DO
            Msg.Verbose ("EVAL (\"", load.head, "\")");
            Quake.Run (mach, load.head);
            load := load.tail;
          END;
        END;

        (* what does the user want us to do? *)
        makefile := Makefile.Build (args, Dirs.to_source);

        (* and finally, do it *)
        IF (makefile # NIL) THEN
          Msg.Verbose ("EVAL (\"", makefile, "\")");
          M3Build.Run (mach, Pathname.Join (Dirs.derived, makefile, NIL));
        END;

      FINALLY
        (* free any temp files & garbage *)
        Quake.Done (mach);
        mach := NIL;
      END;

    EXCEPT
    | Quake.Error(msg) =>
      IF NOT M3Build.done THEN
        Msg.Error (NIL, msg);
        M3Options.exit_code := 2;
      END;
    | Thread.Alerted =>
      Msg.FatalError (NIL, "interrupted");
    END;

    IF M3Options.exit_code # 0 THEN
      Msg.Out("Fatal Error: package build failed", Wr.EOL);
    END;
    Process.Exit (M3Options.exit_code);
  END DoIt;

PROCEDURE ParseCommandLine () =
  VAR i := 1;  n := Params.Count;  arg: TEXT;
  BEGIN
    defs := Makefile.ScanCommandLine();
    Arg.Append (args, "-find");
    M3Options.major_mode := M3Options.Mode.Find;
    WHILE (i < n) DO
      arg := Params.Get (i);
      IF Text.Equal (arg, "-b") THEN
        INC (i);  IF (i >= n) THEN EXIT END;  arg := Params.Get (i);
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
      ELSE
        Arg.Append (args, arg);
      END;
      INC (i);
    END;
    quake_preloads := TextList.ReverseD (quake_preloads);
  END ParseCommandLine;

PROCEDURE ChDir (dir: TEXT) =
  BEGIN
    TRY
      Process.SetWorkingDirectory (dir);
    EXCEPT OSError.E =>
      Msg.FatalError (NIL, "m3build: unable to move to directory: ", dir);
    END;
  END ChDir;

(*------------------------------------------------- process shutdown ---*)

PROCEDURE CleanUp () =
  BEGIN
    IF (mach # NIL) THEN
      TRY
        Quake.Done (mach);
        mach := NIL;
      EXCEPT Quake.Error (msg) =>
        Msg.Error (NIL, msg);
      END;
    END;
    
    WebFile.Dump ();
    Builder.CleanUp ();
    M3Timers.Stop ();
    Utils.RemoveTempFiles ();
    Dirs.CleanUp ();

    IF (M3Options.heap_stats) THEN
      RTutils.Heap (suppressZeros := TRUE,
                    presentation := RTutils.HeapPresentation.ByNumber);
      RTCollector.Collect ();
      RTutils.Heap (suppressZeros := TRUE,
                    presentation := RTutils.HeapPresentation.ByNumber);
    END;
  END CleanUp;

BEGIN
  ParseCommandLine ();

  IF template = NIL THEN
    template := Env.Get("M3_TEMPLATE");
  END;
  IF template # NIL THEN
    MxConfig.Filename := template;
  END;

  IF template_dir = NIL THEN
    template_dir := Env.Get("M3_TEMPLATE_DIR");
  END;
  IF template_dir # NIL THEN
    MxConfig.DefaultPath := template_dir;
  END;

  ChDir (start_dir);

  DoIt();
END Main.
