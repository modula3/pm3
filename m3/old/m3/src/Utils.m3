(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Feb 22 08:50:13 PST 1995 by kalsow     *)

MODULE Utils;

IMPORT File, FileWr, Wr, Thread, Fmt, Process, TextIntTbl;
IMPORT Stdio, OSError, ETimer, FS, RegularFile, Time;
IMPORT Msg, Arg, M3Timers, Target, CoffTime, M3File;

(*--------------------------------------------------------------- writers ---*)

PROCEDURE OpenWriter (name: TEXT;  fatal: BOOLEAN): Wr.T =
  VAR wr: Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open (name);
    EXCEPT OSError.E (args) =>
      IF (fatal)
        THEN Msg.FatalError (args, "unable to open file for writing: ", name);
        ELSE Msg.Error      (args, "unable to open file for writing: ", name);
      END;
      wr := NIL;
    END;
    RETURN wr;
  END OpenWriter;

PROCEDURE FlushWriter (wr: Wr.T;  name: TEXT) =
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      Wr.Flush (wr);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to flush output file: ", name);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to flush output file: ", name);
    END;
  END FlushWriter;

PROCEDURE CloseWriter (wr: Wr.T;  name: TEXT) =
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      Wr.Close (wr);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to close output file: ", name);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to close output file: ", name);
    END;
  END CloseWriter;

(*--------------------------------------------------------------- readers ---*)

PROCEDURE OpenReader (name: TEXT;  fatal: BOOLEAN): File.T =
  VAR rd: File.T;
  BEGIN
    TRY
      rd := FS.OpenFileReadonly (name);
    EXCEPT OSError.E (args) =>
      IF (fatal)
        THEN Msg.FatalError (args, "unable to open file for reading: ", name);
        ELSE Msg.Error      (args, "unable to open file for reading: ", name);
      END;
      rd := NIL;
    END;
    RETURN rd;
  END OpenReader;

PROCEDURE CloseReader (rd: File.T;  name: TEXT) =
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    TRY
      rd.close ();
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to close input file: ", name);
    END;
  END CloseReader;

PROCEDURE RewindReader (rd: File.T;  name: TEXT) =
  VAR f: RegularFile.T := rd;
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    TRY
      EVAL f.seek (RegularFile.Origin.Beginning, 0);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to rewind input file: ", name);
    END;
  END RewindReader;

(*------------------------------------------------------- temporary files ---*)

VAR
  tmp_files := NEW (TextIntTbl.Default).init (100);

PROCEDURE OpenTempFile (root: TEXT;  VAR file: TEXT): Wr.T =
  VAR seq := 0;  wr: Wr.T;
  BEGIN
    file := root;
    WHILE (ModificationTime (file) # NO_TIME) DO
      INC (seq);
      file := root & "_" & Fmt.Int (seq);
    END;
    wr := OpenWriter (file, fatal := TRUE);
    EVAL tmp_files.put (file, 0);
    RETURN wr;
  END OpenTempFile;

PROCEDURE NoteTempFile (name: TEXT) =
  BEGIN
    EVAL tmp_files.put (name, 0);
  END NoteTempFile;

PROCEDURE RemoveTempFiles () =
  VAR
    iter := tmp_files.iterate ();
    name : TEXT;
    void : INTEGER;
  BEGIN
    WHILE iter.next (name, void) DO  Remove (name);  END;
  END RemoveTempFiles;

(*------------------------------------------------------- file operations ---*)

PROCEDURE Remove (file: TEXT) =
  VAR void: INTEGER;
  BEGIN
    IF (file = NIL) THEN RETURN END;
    ETimer.Push (M3Timers.remove);
    Msg.Commands ("rm ", file);
    TRY
      FS.DeleteFile (file);
    EXCEPT OSError.E =>
      (* ignore the failure *)
    END;
    EVAL tmp_files.delete (file, void);
    ETimer.Pop ();
  END Remove;

PROCEDURE Copy (old, new: TEXT) =
  BEGIN
    Msg.Commands ("copy ", old, " -> ", new);
    TRY
      M3File.Copy (old, new);
    EXCEPT OSError.E (ec) =>
      Msg.FatalError (ec, "unable to copy: ", old, " -> ", new);
    END;
  END Copy;

PROCEDURE CopyText (old, new: TEXT) =
  BEGIN
    Msg.Commands ("copy ", old, " -> ", new);
    TRY
      M3File.CopyText (old, new, Target.EOL);
    EXCEPT OSError.E (ec) =>
      Msg.FatalError (ec, "unable to copy: ", old, " -> ", new);
    END;
  END CopyText;

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN =
  BEGIN
    Msg.Commands ("compare ", a, " ", b);
    TRY
      RETURN M3File.IsEqual (a, b);
    EXCEPT OSError.E (ec) =>
      Msg.FatalError (ec, "unable to compare: ", a, " == ", b);
    END;
    RETURN FALSE;
  END IsEqual;

(*------------------------------------------------------------ file times ---*)

VAR
  file_times := NEW (TextIntTbl.Default).init ();

PROCEDURE NoteLocalFileTimes () =
  VAR
    iter : FS.Iterator;
    file : TEXT;
    stat : File.Status;
    time : INTEGER;
  BEGIN
    TRY
      iter := FS.Iterate (".");
      WHILE iter.nextWithStatus (file, stat) DO
        time := M3Time (stat.modificationTime);
        EVAL file_times.put (file, time);
      END;
    EXCEPT OSError.E (ec) =>
      Msg.FatalError (ec, "unable to get file times in current directory");
    END;
  END NoteLocalFileTimes;

PROCEDURE LocalModTime (file: TEXT): INTEGER =
  VAR t := NO_TIME;
  BEGIN
    IF (file # NIL) THEN EVAL file_times.get (file, t) END;
    RETURN t;
  END LocalModTime;

PROCEDURE ModificationTime (file: TEXT): INTEGER =
  VAR t: INTEGER;
  BEGIN
    IF (file = NIL) OR NOT file_times.get (file, t) THEN
      t := NoteModification (file);
    END;
    RETURN t;
  END ModificationTime;

PROCEDURE NoteModification (file: TEXT): INTEGER =
  VAR s: File.Status;  t: INTEGER;
  BEGIN
    IF (file = NIL) THEN RETURN NO_TIME; END;
    TRY
      s := FS.Status (file);
      t := M3Time (s.modificationTime);
    EXCEPT OSError.E =>
      t := NO_TIME;
    END;
    EVAL file_times.put (file, t);
    RETURN t;
  END NoteModification;

PROCEDURE NoteNewFile (file: TEXT) =
  BEGIN
    EVAL file_times.put (file, M3Time (Time.Now ()));
  END NoteNewFile;

PROCEDURE M3Time (t: Time.T): INTEGER =
  CONST Year  = 365.25d0 * 24.0d0 * 3600.0d0;
  CONST Epoch = CoffTime.EpochAdjust(*1970*) + 24.0d0 * Year;
  BEGIN
    RETURN ROUND (t - Epoch);
  END M3Time;

(*---------------------------------- process creation / command execution ---*)

PROCEDURE PrepArgs (program: TEXT;  args: Arg.List): REF ARRAY OF TEXT =
  VAR argv := NEW (REF ARRAY OF TEXT, args.cnt);  a := args.head;
  BEGIN
    IF (Msg.level >= Msg.Level.Commands) THEN Msg.Out (program); END;

    FOR i := 0 TO args.cnt-1 DO 
      argv[i] := a.arg; 
      IF (Msg.level >= Msg.Level.Commands) THEN Msg.Out (" ", argv [i]); END;
      a := a.next;
    END;

    IF (Msg.level >= Msg.Level.Commands) THEN Msg.Out (Wr.EOL); END;

    RETURN argv;
  END PrepArgs;

PROCEDURE Execute (program: TEXT;  args: Arg.List;
                   stdout: TEXT;  fatal: BOOLEAN): INTEGER =
  VAR
    p         : Process.T;
    my_stdin  : File.T;
    my_stdout : File.T;
    my_stderr : File.T;
    ec        : Process.ExitCode;
  BEGIN
    FlushWriter (Stdio.stdout, "<stdout>");
    FlushWriter (Stdio.stderr, "<stderr>");

    TRY
      Process.GetStandardFileHandles (my_stdin, my_stdout, my_stderr);
      IF (stdout # NIL) THEN my_stdout := OpenStdout (stdout); END;
      p := Process.Create (cmd := program, 
                           params := PrepArgs (program, args)^,
                           stdin := my_stdin, stdout := my_stdout,
                           stderr := my_stderr);
      IF (stdout # NIL) THEN CloseStdout (my_stdout, stdout); END;
      ec := Process.Wait (p);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "Process.Create(\""& program &"\") failed");
    END;

    IF (fatal) AND (ec # 0) THEN
      Msg.FatalError (NIL, "program \"", program, "\" failed, exit status = ",
                        Fmt.Int (ec));
    END;
    RETURN ec;
  END Execute;

PROCEDURE OpenStdout (nm: TEXT): File.T =
  VAR wr: File.T;
  BEGIN
    TRY
      wr := FS.OpenFile (nm);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to open file for listing: ", nm);
      wr := NIL;
    END;
    RETURN wr;
  END OpenStdout;

PROCEDURE CloseStdout (wr: File.T;  name: TEXT) =
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      wr.close ();
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to close listing file: ", name);
    END;
  END CloseStdout;

BEGIN
END Utils.
