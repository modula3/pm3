
(*****************************************************************************)
(*****************************************************************************)

MODULE SimonH_Log_v01;

IMPORT Wr, Text, Stdio, FileWr, Fmt, OSError, Thread;

(*****************************************************************************)
(* EXPORTED/IMPLEMENTED ROUTINES :                                           *)
(*****************************************************************************)

PROCEDURE Open_Append(name : TEXT) : Writer RAISES {OSError.E} =
BEGIN
							      (* TO COMPLETE *)
							      (* TO COMPLETE *)
END Open_Append;

(*****************************************************************************)

PROCEDURE Create_Stream(name : TEXT) : Writer 
RAISES 
  {Name_Is_Nil, Cannot_Create} = 
VAR
  wr : Writer;
BEGIN
  IF name = NIL THEN RAISE Name_Is_Nil;
  ELSIF Text.Equal(name,"") THEN 
      wr := Stdio.stdout;  
  ELSE
      TRY  
          wr := FileWr.Open(name);  
      EXCEPT  OSError.E => RAISE Cannot_Create;
      END;
  END;
  RETURN wr;
END Create_Stream;

(*****************************************************************************)

PROCEDURE Close_Stream(wr : Writer) RAISES {Cannot_Close} = 
BEGIN
  IF (wr # NIL) THEN
      TRY 
          IF (wr = Stdio.stdout) THEN Wr.Flush(Stdio.stdout);
          ELSE Wr.Close(wr);  wr:= NIL;
               (* : this could RAISE "Failure" or "Alerted". *)
          END;
      EXCEPT 
          Wr.Failure, Thread.Alerted => RAISE Cannot_Close;
      END;
  END;
END Close_Stream;

(*****************************************************************************)

PROCEDURE Put_Text(text : TEXT;
                   wr   : Writer) RAISES {Cannot_Put} =
BEGIN
  IF (wr = NIL) THEN  wr := Stdio.stdout;  END;
  TRY
      Wr.PutText(wr, text);
      Wr.Flush(wr);
      (* : each could raise "Failure" or "Alerted". *)
  EXCEPT 
      Wr.Failure, Thread.Alerted => RAISE Cannot_Put;
  END;
END Put_Text;

(*****************************************************************************)

PROCEDURE Put_Event(text : TEXT) RAISES {Cannot_Put} =
BEGIN
END Put_Event;

(*****************************************************************************)

PROCEDURE Put_Ln(wr : Writer := NIL) RAISES {Cannot_Put} =
BEGIN
  IF (wr = NIL) THEN  wr := Stdio.stdout;  END;
  TRY
      Wr.PutText(wr, "\n");
      Wr.Flush(wr);
      (* : each could raise "Failure" or "Alerted". *)
  EXCEPT 
      Wr.Failure, Thread.Alerted => RAISE Cannot_Put;
  END;
END Put_Ln;

(*****************************************************************************)

PROCEDURE Put_Int(int    : INTEGER;
                  wr     : Writer := NIL) RAISES {Cannot_Put} =
BEGIN
  IF (wr = NIL) THEN  wr := Stdio.stdout;  END;
  TRY
      Wr.PutText(wr, Fmt.Int(int));
      Wr.Flush(wr);
      (* : each could raise "Failure" or "Alerted". *)
  EXCEPT 
      Wr.Failure, Thread.Alerted => RAISE Cannot_Put;
  END;
END Put_Int;

(*****************************************************************************)

PROCEDURE Put_LReal(lreal  : LONGREAL;
                    wr     : Writer := NIL;
                    sigfig : CARDINAL := 5) RAISES {Cannot_Put} =
BEGIN
  IF (wr = NIL) THEN  wr := Stdio.stdout;  END;
  TRY
      Wr.PutText(wr, Fmt.LongReal(lreal, 
                                  prec := sigfig - 1, 
                                  style := Fmt.Style.Sci));
      Wr.Flush(wr);
      (* : each could raise "Failure" or "Alerted". *)
  EXCEPT 
      Wr.Failure, Thread.Alerted => RAISE Cannot_Put;
  END;
END Put_LReal;

(*****************************************************************************)
(*****************************************************************************)

BEGIN

END SimonH_Log_v01.

(*****************************************************************************)
(*****************************************************************************)

