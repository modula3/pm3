(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(*                                                          *)
(* Last modified on Tue Dec 20 08:39:03 PST 1994 by kalsow  *)
(*      modified on Fri May  7 10:40:38 PDT 1993 by mcjones *)
(*      modified on Thu May  6 13:34:41 PDT 1993 by mjordan *)

UNSAFE MODULE OSErrorWin32 EXPORTS OSError, OSErrorWin32;

IMPORT OSError;
IMPORT Atom, AtomList, Fmt;
IMPORT WinBase;

VAR cache := ARRAY [0..2000] OF Atom.T { NIL, .. };
(* The table is initialized lazily. *)

PROCEDURE NewAtom (n: CARDINAL): Atom.T =
  BEGIN
    RETURN Atom.FromText("ErrorCode=" & Fmt.Int(n));
  END NewAtom;

PROCEDURE ErrnoAtom(n: CARDINAL): Atom.T =
  BEGIN
    IF (n < NUMBER (cache)) THEN
      IF cache[n] = NIL THEN cache[n] := NewAtom(n) END;
      RETURN cache[n];
    ELSE
      RETURN NewAtom (n);
    END;
  END ErrnoAtom;

PROCEDURE Raise0(errno: INTEGER) RAISES {OSError.E} =
  BEGIN
    RAISE OSError.E(
      NEW(AtomList.T, head := ErrnoAtom(errno), tail := NIL))
  END Raise0;

PROCEDURE Raise() RAISES {OSError.E} =
  BEGIN
    Raise0(WinBase.GetLastError());
  END Raise;

BEGIN
END OSErrorWin32.
