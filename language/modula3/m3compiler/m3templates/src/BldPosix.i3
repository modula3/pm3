INTERFACE BldPosix;

IMPORT BldQuake, Quake;

CONST
  SL = "/";       (* the path separator *)
  SLship = "/";   (* the path separator in M3SHIP file *)
  CR = "\n";      (* line break character *)
  CRship = "\n";  (* line break character in M3SHIP file *)

(* POSIX specific routines *)
PROCEDURE DelFile(t: BldQuake.T; x: TEXT);
PROCEDURE LinkFile(t: BldQuake.T; from, to: TEXT);
PROCEDURE MakeExec(t: BldQuake.T; script: TEXT);
PROCEDURE MakeDir(t: BldQuake.T; dir: TEXT) RAISES {Quake.Error};

END BldPosix.
