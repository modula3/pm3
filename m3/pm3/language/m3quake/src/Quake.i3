(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Feb 22 13:07:43 PST 1995 by kalsow     *)

INTERFACE Quake;

EXCEPTION
  Error (TEXT);

TYPE
  CodeStream <: REFANY;
  Machine    <: REFANY;

PROCEDURE NewMachine (): Machine;

PROCEDURE Run (m: Machine;  source_file: TEXT) RAISES {Error};

PROCEDURE Define (m: Machine;  symbol, value: TEXT) RAISES {Error};

PROCEDURE Done (m: Machine) RAISES {Error};

END Quake.
