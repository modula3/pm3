(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul 11 10:00:00 PDT 1994 by collin     *)
(*      modified on Wed Oct 12 16:12:57 PDT 1994 by kalsow     *)

MODULE M3BackLinux EXPORTS M3Backend;

IMPORT Wr, Thread;
IMPORT M3CG, Msg, Utils, ELFObjFile, M3x86, M3ObjFile, M3CG_Wr, Arg;
FROM M3Driver IMPORT Interface;

VAR
  obj_file : M3ObjFile.T := NIL;
  obj_wr   : Wr.T        := NIL;
  obj_name : TEXT        := NIL;
  log      : Wr.T        := NIL;
  log_name : TEXT        := NIL;

PROCEDURE Open (target: Wr.T;  target_name: TEXT; 
                optimize, debug, shared: BOOLEAN;
                interface: Interface): M3CG.T =
  VAR
    options := Arg.NewList();
  BEGIN
    IF interface.ext_pass_6 THEN
      RETURN M3CG_Wr.New (target);
    ELSE
      <*ASSERT obj_file = NIL *>
      (* IF optimize THEN Arg.Append(options, "-O") END; *)
      IF debug THEN Arg.Append(options, "-g") END;
      (* IF shared THEN *) Arg.Append(options, "-fPIC"); (* END; *)
      obj_file := ELFObjFile.New ();
      obj_wr   := target;
      obj_name := target_name;
      IF (Msg.level >= Msg.Level.Verbose) THEN
        log_name := target_name & "log";
        log := Utils.OpenWriter (log_name, fatal := TRUE);
      END;
      RETURN M3x86.New (log, obj_file, Arg.Flatten(options, NIL)^);
    END;
  END Open;

PROCEDURE Close (<*UNUSED*> cg: M3CG.T) =
  BEGIN
    IF obj_file # NIL THEN
      TRY
        ELFObjFile.Dump (obj_file, obj_wr);
      EXCEPT Wr.Failure, Thread.Alerted =>
        Msg.FatalError (NIL, "problem writing object file: ", obj_name);
      END;
      Utils.CloseWriter (log, log_name);
      obj_file := NIL;
      obj_wr   := NIL;
      obj_name := NIL;
      log      := NIL;
      log_name := NIL;
    END;
  END Close;

BEGIN
END M3BackLinux.
