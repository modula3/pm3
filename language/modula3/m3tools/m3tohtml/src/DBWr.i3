(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr  7 13:32:41 PDT 1994 by kalsow                   *)

INTERFACE DBWr;

TYPE
  T <: T_; T_ = OBJECT
  METHODS
    init (path: TEXT): T;
    put_int (i: INTEGER);
    put_line (t: TEXT);
    close ();
  END;

END DBWr.


