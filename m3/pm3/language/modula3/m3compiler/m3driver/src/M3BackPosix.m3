(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Oct 12 16:12:41 PDT 1994 by kalsow     *)

MODULE M3BackPosix EXPORTS M3Backend;

IMPORT Wr, M3CG, M3CG_Wr;
FROM M3Driver IMPORT Error, Interface;

PROCEDURE Open (target: Wr.T;  <*UNUSED*> target_name: TEXT;
                <* UNUSED *> optimize, debug, shared: BOOLEAN;
                <* UNUSED *> interface: Interface): M3CG.T
  RAISES {Error} = <* NOWARN *>
  BEGIN
    RETURN M3CG_Wr.New (target);
  END Open;

PROCEDURE Close (<*UNUSED*> cg: M3CG.T)
  RAISES {Error} = <* NOWARN *>
  BEGIN
  END Close;

BEGIN
END M3BackPosix.
