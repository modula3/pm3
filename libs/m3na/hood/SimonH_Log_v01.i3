
(*****************************************************************************)
(*****************************************************************************)

(*  'log_streams' provides some simple "TEXT" logging mechanisms;  these might
    be used to provide diagnostics on what a program is doing, or recording
    data for diagnostic purposes during program development;  
    continually flushed:  this means that the streams are safe --- data 
    logged will be preserved if a program dies;

    streams can of course be files or 'stdio':  setting stream name to
    "Text.Empty" allows `opening` a stream to 'stdio';  if name parameter is 
    omitted the user is interactively asked for a 'name';  of course,
    output to 'stdio' does not need to be `opened`, so it's easy;

    NOTES :

    [1]  after each and every "Put_" the stream is flushed so that all output
         *is* written to disk and a program crash will not result in 
         apparently logged data being lost;

    [2]  continual flushing is somewhat inefficient, so this concept does not 
         really replace output of (large amounts of) data to buffered 
         disk-files;


    HISTORY :

    v0, 15 March 96 -

        --- moved from returning flags and stuff (yeuk!) to RAISEing 
            EXCEPTIONS

        --- complete implementation (v-1 did was never completed);

    *)


INTERFACE SimonH_Log_v01;

IMPORT Wr, OSError;

TYPE 
  Writer = Wr.T;

EXCEPTION 
  Name_Is_Nil;
  Cannot_Create;
  Cannot_Close;
  Cannot_Put;

  (* ... and those defined in 'libm3' :

     Alerted (from "Thread")  --- indicates ???
     Failure (from "Wr")      --- indicates e.g., disk-full, network gone 
                                  down ...;  
     OSError.E                --- indicates ???
   *)


PROCEDURE Open_Append(name : TEXT) : Writer RAISES {OSError.E};

  (* opens a log-stream called "name" for writing;  if the stream already
     exists then append;  if "name" is "" output goes to "Stdio.stdout";  
     if "name" is NIL then fails; *)


PROCEDURE Create_Stream(name : TEXT;) : Writer 
    RAISES {Name_Is_Nil, Cannot_Create};

  (* opens a log-stream called "name" for writing;  if "name" is "" output 
     goes to "Stdio.stdout";  if "name" is NIL fails;  if a stream exists 
     with same name then it gets overwritten;  *)


PROCEDURE Close_Stream(stream_handle : Writer) 
    RAISES {Cannot_Close};

  (* closed the text-stream;  assigns "NIL" to "wr" --- unless "wr" is
     "Stdio.stdout" in which case the stream is simply flushed; *)


PROCEDURE Put_Text(text : TEXT;
                   wr   : Writer := NIL) RAISES {Cannot_Put};

  (* puts a "TEXT" into the named 'Writer', "Wr";  if 'wr' is ommitted
     or "NIL" then output goes to "Stdio.stdout"; *)


PROCEDURE Put_Event(text : TEXT) RAISES {Cannot_Put};

  (* opens a new window and puts a "TEXT" into it; *)

				(* This PROCEDURE should be viewed as a      *)
				(* precurser to a full 'log-window-stream'   *)
				(* facility set in which a stream can be a   *)
				(* (scrollable) window.                      *)

PROCEDURE Put_Ln(wr : Writer := NIL) RAISES {Cannot_Put};

  (* puts a "\n" into the named 'Writer', "Wr";  if 'wr' is ommitted
     or "NIL" then output goes to "Stdio.stdout"; *)


PROCEDURE Put_Int(int    : INTEGER;
                  wr     : Writer := NIL) RAISES {Cannot_Put};

  (* puts an "INTEGER" into the named 'Writer', "Wr";  if 'wr' is ommitted
     or "NIL" then output goes to "Stdio.stdout"; *)


PROCEDURE Put_LReal(lreal  : LONGREAL;
                    wr     : Writer := NIL;
                    sigfig : CARDINAL := 5) RAISES {Cannot_Put};

  (* puts a "LONGREAL" into the named 'Writer', "Wr";  if 'wr' is ommitted
     or "NIL" then output goes to "Stdio.stdout"; *)


END SimonH_Log_v01.

(*****************************************************************************)
(*****************************************************************************)

