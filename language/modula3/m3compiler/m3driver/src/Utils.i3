(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Jan 27 15:42:58 PST 1995 by kalsow     *)

INTERFACE Utils;

IMPORT File, Wr, Arg, Time;
FROM M3Driver IMPORT Error;

PROCEDURE OpenWriter   (name: TEXT;  fatal: BOOLEAN): Wr.T RAISES {Error};
PROCEDURE FlushWriter  (wr: Wr.T;  name: TEXT) RAISES {Error};
PROCEDURE CloseWriter  (wr: Wr.T;  name: TEXT) RAISES {Error};

PROCEDURE OpenReader   (name: TEXT;  fatal: BOOLEAN): File.T RAISES {Error};
PROCEDURE CloseReader  (rd: File.T;  name: TEXT) RAISES {Error};
PROCEDURE RewindReader (rd: File.T;  name: TEXT) RAISES {Error};

PROCEDURE OpenTempFile (root: TEXT;  VAR(*OUT*) file: TEXT): Wr.T RAISES {Error};
PROCEDURE NoteTempFile (name: TEXT);
PROCEDURE RemoveTempFiles ();

PROCEDURE Remove (file: TEXT);
PROCEDURE Copy (old_file, new_file: TEXT) RAISES {Error};
PROCEDURE CopyText (old_file, new_file: TEXT) RAISES {Error};
PROCEDURE IsEqual (a, b: TEXT): BOOLEAN RAISES {Error};

PROCEDURE NoteLocalFileTimes () RAISES {Error};
PROCEDURE LocalModTime     (file: TEXT): INTEGER;
PROCEDURE ModificationTime (file: TEXT): INTEGER;
PROCEDURE NoteModification (file: TEXT): INTEGER;
PROCEDURE NoteNewFile      (file: TEXT);
PROCEDURE M3Time (t: Time.T): INTEGER;

CONST NO_TIME = 0;

PROCEDURE PrepArgs (program: TEXT;  args: Arg.List): REF ARRAY OF TEXT;
PROCEDURE Execute  (program: TEXT;  args: Arg.List;
                     stdout: TEXT;  fatal: BOOLEAN): INTEGER RAISES {Error};

END Utils.
