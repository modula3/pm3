(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxGenCG.i3                                            *)
(* Last Modified On Thu Jan 18 15:09:58 EST 1996 By ldd        *)

INTERFACE MxGenCG;

IMPORT MxGenRep, M3CG, Mx;

TYPE 
  T <: MxGenRep.Common;

PROCEDURE New(base: Mx.LinkSet;  cg: M3CG.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN): T;

END MxGenCG.
