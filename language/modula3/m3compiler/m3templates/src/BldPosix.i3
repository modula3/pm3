INTERFACE BldPosix;

IMPORT BldQuake;

(* POSIX specific routines *)
PROCEDURE DelFile(t: BldQuake.T; x: TEXT);
PROCEDURE LinkFile(t: BldQuake.T; from, to: TEXT);
PROCEDURE MakeExec(t: BldQuake.T; script: TEXT);

END BldPosix.
