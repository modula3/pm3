(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Sep 27 11:02:16 PDT 1994 by kalsow     *)

INTERFACE M3Backend;

IMPORT Wr, M3CG;
FROM M3Driver IMPORT Error, Interface;

PROCEDURE Open (target: Wr.T;  target_name: TEXT;
                optimize, debug, shared: BOOLEAN;
                interface: Interface): M3CG.T  RAISES {Error};
PROCEDURE Close (cg: M3CG.T)  RAISES {Error};

END M3Backend.
