(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OS.m3                                                 *)
(* Last modified on Wed Jun 21 09:36:57 PDT 1995 by kalsow     *)
(*      modified on Tue Mar 24 16:04:38 PST 1992 by muller     *)

UNSAFE MODULE OS;

IMPORT M3toC, Time, Text;
IMPORT IP, Unix, Ustat;

PROCEDURE CreateTime (file: TEXT): Time.T =
  VAR s: Ustat.struct_stat;
  BEGIN
    IF Ustat.stat (M3toC.TtoS (file), ADR (s)) = 0
      THEN RETURN FLOAT (s.st_mtime, LONGREAL);
      ELSE RETURN NO_TIME;
    END;
  END CreateTime;

PROCEDURE GetHostName (): TEXT =
  VAR buf: ARRAY [0..255] OF CHAR;  host: TEXT;
  BEGIN
    IF Unix.gethostname (ADR (buf[0]), BYTESIZE (buf)) # 0 THEN RETURN NIL END;
    host := M3toC.CopyStoT (ADR (buf[0]));

    TRY
      RETURN IP.GetCanonicalByName(host);
    EXCEPT IP.Error =>
      (* drat...  *)
    END;

    (* -- apparently, on many systems getdomainname returns the local YP
          domain name, not what we wanted, but better than nothing *)

    IF Text.FindChar (host, '.') # -1 THEN RETURN host; END;
    IF Unix.getdomainname (ADR (buf), BYTESIZE (buf)) = 0 THEN
      host := host & "." & M3toC.StoT (ADR (buf[0]));
    END;
    RETURN host;
  END GetHostName;

BEGIN
END OS.

