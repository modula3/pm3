INTERFACE BldWin32;

IMPORT BldQuake;

(* Win32 specific routines *)
PROCEDURE DelFile(t: BldQuake.T; x: TEXT);
PROCEDURE LinkFile(t: BldQuake.T; from, to: TEXT);
PROCEDURE MakeExec(t: BldQuake.T; script: TEXT);

END BldWin32.
