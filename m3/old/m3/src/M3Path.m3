(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 09:07:50 PDT 1994 by kalsow     *)
(*      modified on Mon Oct 25 10:31:06 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE M3Path;

IMPORT Text, TextF;

CONST
  Null      = '\000';
  Colon     = ':';
  Slash     = '/';
  BackSlash = '\\';

CONST
  DirSep = ARRAY OSKind OF CHAR { Slash,  Slash,  BackSlash };
  VolSep = ARRAY OSKind OF CHAR { Null,   Null,   Colon  };

CONST
  DirSepText = ARRAY OSKind OF TEXT { "/",  "/",  "\\" };

TYPE
  SMap = ARRAY Kind OF TEXT;

CONST
  Suffix = ARRAY OSKind OF SMap {
  (* Unix *)       SMap { ".i3", ".ic", ".is", ".io",
                          ".m3", ".mc", ".ms", ".mo",
                          ".ig", ".mg", ".c", ".h", ".s",
                          ".o", ".a", ".m3x", ".m3x", "" },
  (* GrumpyUnix *) SMap { ".i3", ".ic", ".is", "_i.o",
                          ".m3", ".mc", ".ms", "_m.o",
                          ".ig", ".mg", ".c", ".h", ".s",
                          ".o", ".a", ".m3x", ".m3x", "" },
  (* Win32 *)      SMap { ".i3", ".ic", ".is", ".io",
                          ".m3", ".mc", ".ms", ".mo",
                          ".ig", ".mg", ".c", ".h", ".s",
                          ".obj",".lib",".m3x", ".m3x", "" }
  };

CONST
  Default_pgm = ARRAY OSKind OF TEXT { "a.out", "a.out", "NONAME.EXE" };

VAR
  os_map := ARRAY BOOLEAN OF OSKind { OSKind.Unix, OSKind.Unix };
  lcase  : ARRAY CHAR OF CHAR;

PROCEDURE SetOS (kind: OSKind;  host: BOOLEAN) =
  BEGIN
    os_map [host] := kind;
  END SetOS;

PROCEDURE Join (dir, base: TEXT;  k: Kind;  host: BOOLEAN): TEXT=
  VAR
    len    := 0;
    os     := os_map [host];
    ext    := Suffix [os][k];
    d_sep  := DirSep [os];
    v_sep  := VolSep [os];
    result : TEXT;
  BEGIN
    (* find out how much space we need *)
    IF (dir # NIL) THEN
      len := LAST (dir^);
      IF (dir[len-1] # d_sep) AND (dir[len-1] # v_sep) THEN INC (len); END;
    END;
    IF (os # OSKind.Win32) AND ((k = Kind.A) OR (k = Kind.AX)) THEN
      INC (len, 3);
    END;
    INC (len, LAST (base^));
    INC (len, LAST (ext^));

    (* allocate it and fill it in *)
    result := TextF.New (len);
    len := 0;
    IF (dir # NIL) THEN
      len := Append (result, dir, 0);
      IF (dir[len-1] # d_sep) AND (dir[len-1] # v_sep) THEN
        result [len] := d_sep; INC (len);
      END;
    END;
    IF (os # OSKind.Win32) AND ((k = Kind.A) OR (k = Kind.AX)) THEN
      len := Append (result, "lib", len);
    END;
    len := Append (result, base, len);
    len := Append (result, ext, len);
    RETURN result;
  END Join;

PROCEDURE Append (a, b: TEXT;  start: INTEGER): INTEGER =
  BEGIN
    SUBARRAY (a^, start, NUMBER (b^)) := b^;
    RETURN start + LAST (b^);
  END Append;

PROCEDURE Parse (nm: TEXT;  host: BOOLEAN): T =
  VAR
    t       : T;
    len     := LAST (nm^);
    base_len:= 0;
    d_index := -1;
    v_index := -1;
    start   := 0;
    os      := os_map [host];
    d_sep   := DirSep [os];
    v_sep   := VolSep [os];
    ext     : TEXT;
    ext_len : INTEGER;
  BEGIN
    (* find the last instance of each separator *)
    FOR i := 0 TO len-1 DO IF (nm[i] = v_sep) THEN v_index := i; END; END;
    FOR i := 0 TO len-1 DO IF (nm[i] = d_sep) THEN d_index := i; END; END;

    (* extract the prefix *)
    IF (v_index = -1) AND (d_index = -1) THEN
      (* no separators *)
      t.dir := NIL;
      start := 0;
    ELSIF (d_index = -1) THEN
      (* no directory separator, only a volumne separator *)
      t.dir := Text.FromChars (SUBARRAY (nm^, 0, v_index+1));
      start := v_index + 1;
    ELSIF (d_index = 0) THEN
      t.dir := DirSepText [os];
      start := 1;
    ELSE
      t.dir := Text.FromChars (SUBARRAY (nm^, 0, d_index));
      start := d_index+1;
    END;
    base_len := len - start;

    (* search for a matching suffix *)
    t.kind := Kind.Unknown;
    FOR k := FIRST (Kind) TO LAST (Kind) DO
      ext := Suffix [os][k];
      ext_len := Text.Length (ext);
      IF ExtMatch (nm, ext, os) THEN
        t.kind := k;
        EXIT;
      END;
    END;

    (* extract the base component *)
    t.base := Text.FromChars (SUBARRAY (nm^, start, base_len - ext_len));

    IF (os # OSKind.Win32) AND ((t.kind = Kind.A) OR (t.kind = Kind.AX)) THEN
      IF (LAST (t.base^) >= 3) AND (t.base[0] = 'l')
        AND (t.base[1] = 'i') AND (t.base[2] = 'b') THEN
        t.base := Text.Sub (t.base, 3);
      END;
    END;

    RETURN t;
  END Parse;

PROCEDURE ExtMatch (nm, ext: TEXT;  os: OSKind): BOOLEAN =
  VAR
    nm_len  := LAST (nm^);
    ext_len := LAST (ext^);
    j := 0;
  BEGIN
    IF (nm_len < ext_len) THEN RETURN FALSE END;
    IF (os = OSKind.Win32) THEN
      (* ignore case *)
      FOR i := nm_len-ext_len TO nm_len-1 DO
        IF lcase [nm[i]] # ext[j] THEN RETURN FALSE; END;
        INC (j);
      END;
    ELSE
      FOR i := nm_len-ext_len TO nm_len-1 DO
        IF nm[i] # ext[j] THEN RETURN FALSE; END;
        INC (j);
      END;
    END;
    RETURN TRUE;
  END ExtMatch;

PROCEDURE DefaultProgram (host: BOOLEAN): TEXT =
  BEGIN
    RETURN Default_pgm [os_map [host]];
  END DefaultProgram;

PROCEDURE Convert (nm: TEXT;  host: BOOLEAN): TEXT =
  VAR
    len  := LAST (nm^);
    res  := TextF.New (len);
    good := DirSep [os_map [host]];
    bad  := DirSep [os_map [NOT host]];
  BEGIN
    FOR i := 0 TO len-1 DO
      IF (nm[i] = bad)
        THEN res[i] := good;
        ELSE res[i] := nm[i];
      END;
    END;
    RETURN res;
  END Convert;

PROCEDURE Escape (nm: TEXT): TEXT =
  VAR n_escapes := 0;  len := LAST (nm^);
  BEGIN
    FOR i := 0 TO len-1 DO
      IF (nm[i] = BackSlash) THEN INC (n_escapes); END;
    END;
    IF (n_escapes = 0) THEN RETURN nm; END;
    VAR res := TextF.New (len + n_escapes);  j := 0;  BEGIN
      FOR i := 0 TO len-1 DO
        res[j] := nm[i];  INC (j);
        IF (nm[i] = BackSlash) THEN res[j] := BackSlash;  INC (j); END;
      END;
      RETURN res;
    END;
  END Escape;

BEGIN
  FOR i := FIRST (lcase) TO LAST (lcase) DO lcase[i] := i; END;
  FOR i := 'A' TO 'Z' DO
    lcase[i] := VAL (ORD (i) - ORD ('A') + ORD ('a'), CHAR);
  END;
END M3Path.
