(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxGen.i3                                              *)
(* Last Modified On Fri Nov 11 13:00:26 PST 1994 By kalsow     *)

INTERFACE MxGen;

IMPORT Mx, Wr;


PROCEDURE GenerateMain (base       : Mx.LinkSet;
                        output     : Wr.T;
                        verbose    : BOOLEAN;
                        windowsGUI : BOOLEAN);
(* write the list of compilation units in 'base' on 'output' in a
   correct Modula-3 initialization order.  It is an error to pass a 'base'
   that hasn't successfully passed through 'MxCheck.IsProgram'. *)

END MxGen.
