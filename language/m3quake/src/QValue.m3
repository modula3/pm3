(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Feb 17 09:11:09 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QValue;

IMPORT M3ID;

BEGIN
  BoolID [FALSE] := M3ID.Add ("");
  BoolID [TRUE]  := M3ID.Add ("TRUE");
END QValue.
