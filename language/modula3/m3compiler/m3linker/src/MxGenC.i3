(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxGenC.i3                                             *)
(* Last Modified On Thu Jan 18 15:09:58 EST 1996 By ldd        *)


INTERFACE MxGenC;

IMPORT Mx, Wr, MxGenRep;

TYPE
  T <: MxGenRep.Common;

PROCEDURE New(base: Mx.LinkSet;  output: Wr.T;
              verbose: BOOLEAN;  windowsGUI: BOOLEAN): T;


END MxGenC.
