(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Mar 22 14:07:21 PST 1994 by kalsow     *)

UNSAFE MODULE Fmt2;

IMPORT Fmt;

PROCEDURE Ref (r: REFANY): TEXT =
  BEGIN
    RETURN Fmt.Unsigned (LOOPHOLE (r, INTEGER));
  END Ref;

BEGIN
END Fmt2.
