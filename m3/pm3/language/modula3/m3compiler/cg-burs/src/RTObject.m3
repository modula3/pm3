(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jan 26 07:54:21 PST 1994 by kalsow     *)

UNSAFE MODULE RTObject;

IMPORT RT0, RT0u, RTMisc;

PROCEDURE PatchMethods (r: ROOT) =
  VAR
    x := LOOPHOLE (r, UNTRACED REF RT0.MethodSuite);
    m := x^;
    t := RT0u.types [TYPECODE (r)];
    n := t.methodSize DIV BYTESIZE (ADDRESS) - 1;
  BEGIN
    FOR i := 0 TO n-1 DO
      IF (m.methods[i] = NIL) THEN
        m.methods[i] := LOOPHOLE (Crash, ADDRESS);
      END;
    END;
  END PatchMethods;

PROCEDURE Crash () =
  BEGIN
    RTMisc.FatalError (NIL, 0, "attempted invocation of undefined method");
  END Crash;

BEGIN
END RTObject.
