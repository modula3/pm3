
MODULE Fatal_Error_v0;
IMPORT SimonH_Log_v01 AS Log;
IMPORT Process;

PROCEDURE F_Error(text : TEXT) =
BEGIN
  TRY
    Log.Put_Text("\n\n" & text & "\n\n");
  EXCEPT
    | Log.Cannot_Put => Process.Exit(666);
  END;
  Process.Exit(666);
END F_Error;


BEGIN
END Fatal_Error_v0.

