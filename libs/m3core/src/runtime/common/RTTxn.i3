(* Copyright (C) 1998, Purdue Research Foundation            *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE RTTxn;

TYPE
  T = MUTEX OBJECT
    parent: T;
  END;

END RTTxn.
