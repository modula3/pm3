(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last Modified On Fri Jun 11 14:52:25 PDT 1993 by kalsow                   *)
(*      Modified On Sat Feb  6 11:41:23 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 17:37:42 PDT 1992 by muller                   *)

UNSAFE MODULE RTPerfTool;

IMPORT RTParams, Text;

PROCEDURE Start (param: TEXT;  VAR w: Handle): BOOLEAN =
  VAR value: TEXT;  r: Handle;
  BEGIN
    value := RTParams.Value (param);
    IF value = NIL THEN  RETURN FALSE; END;
    IF Text.Length (value) = 0 THEN  value := param;  END;
    IF NOT StartTool (value, r, w) THEN  RETURN FALSE; END;
    <*ASSERT FALSE*>
  END Start;

PROCEDURE Close (w: Handle) =
  BEGIN
    EVAL w;
    <*ASSERT FALSE*>
  END Close;

PROCEDURE Send (w: Handle;  at: ADDRESS;  len: CARDINAL): BOOLEAN =
  BEGIN
    EVAL w;
    EVAL at;
    EVAL len;
    <*ASSERT FALSE*>
  END Send;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE StartTool (name: TEXT; VAR r, w: Handle): BOOLEAN =
  BEGIN
    EVAL name;
    EVAL r;
    EVAL w;
    RETURN FALSE;
  END StartTool;

BEGIN
END RTPerfTool.

