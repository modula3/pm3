(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Feb 22 13:08:06 PST 1995 by kalsow     *)

MODULE Quake;

IMPORT QMachine, QValue, QCompiler, M3ID;

PROCEDURE NewMachine (): Machine =
  BEGIN
    RETURN NEW (QMachine.T).init ();
  END NewMachine;

PROCEDURE Run (m: Machine;  source_file: TEXT) RAISES {Error} =
  BEGIN
    m.evaluate (QCompiler.CompileFile (source_file));
  END Run;

PROCEDURE Define (m: Machine;  symbol, value: TEXT) RAISES {Error} =
  VAR v: QValue.T;
  BEGIN
    v.kind := QValue.Kind.String;
    v.int  := M3ID.Add (value);
    v.ref  := NIL;
    m.put (M3ID.Add (symbol), v);
  END Define;

PROCEDURE Done (m: Machine) RAISES {Error} =
  BEGIN
    m.cleanup ();
  END Done;

BEGIN
END Quake.
