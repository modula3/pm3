(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jan 21 15:46:56 PST 1994 by kalsow     *)
(*      modified on Thu Aug  5 11:21:09 PDT 1993 by hanson     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

MODULE Main;
IMPORT FileRd, FileWr, IO, M3CG_Rd, M3CG_Burs, OSError, Params,
       Rd, Stdio, Text, Wr;
VAR cg: ROOT; gFlag := FALSE; input, output: TEXT; i := 0;
    in: FileRd.T; out: FileWr.T;
<*FATAL ANY*>
BEGIN
  WHILE i < Params.Count DO
    WITH opt = Params.Get (i) DO
      IF    Text.Equal (Text.Sub (opt, 0, 2), "-g") THEN
         gFlag := TRUE
      ELSIF Text.Equal (opt, "-o") AND i < Params.Count - 1 THEN
        INC (i); output := Params.Get (i)
      ELSIF Text.GetChar (opt, 0) # '-' THEN
        input := opt
      END
    END;
    INC (i)
  END;
  TRY
    in  := FileRd.Open (input);
    out := FileWr.Open (output);
    cg := M3CG_Burs.New (out, gFlag);
    (* RTObject.PatchMethods (cg);*)
    M3CG_Rd.Inhale (in, cg);
    Wr.Close (out); Rd.Close (in)
  EXCEPT
    OSError.E => IO.Put ("m3cg: IO error\n", Stdio.stderr)
  END;
END Main.
