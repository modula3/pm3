UNSAFE MODULE StringCopy;

IMPORT Ctypes, Cstring, TextF;

PROCEDURE CopyAttributeStoT (s: Ctypes.char_star; len: CARDINAL): TEXT =
  VAR t := NEW(TEXT, len + 1);
  BEGIN
    EVAL Cstring.memcpy(ADR(t[0]), s, len);
    t[len] := '\000';
    RETURN t;
  END CopyAttributeStoT;

PROCEDURE CopyStoT (s: Ctypes.char_star): TEXT =
  VAR len: CARDINAL;
      t: TEXT := NIL;
  BEGIN
    IF s # NIL THEN
      len := Cstring.strlen(s) + 1;
      t   := NEW(TEXT, len);
      EVAL Cstring.memcpy(ADR(t[0]), s, len);
    END;
    RETURN t;
  END CopyStoT;

PROCEDURE CopyTtoS (t: TEXT; s: Ctypes.char_star) =
  VAR len: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN; END;
    len := NUMBER(t^);
    IF (len > 1) THEN EVAL Cstring.memcpy(s, ADR(t[0]), len - 1); END;
  END CopyTtoS;

BEGIN
END StringCopy.
