(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* File: MxGen.i3                                              *)
(* Last Modified On Thu Jan 18 15:09:57 EST 1996 By ldd        *)
(* Last Modified On Fri Nov 11 13:00:26 PST 1994 By kalsow     *)

INTERFACE MxGen;

TYPE
  T = BRANDED "MxGen.T" OBJECT
  METHODS
    generateMain ();
    (* Generate the m3main source file.  The actual method depends on the 
       actual kind of MxGen object created. *)
  END;

END MxGen.
