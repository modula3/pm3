(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr  7 16:11:03 PDT 1994 by kalsow                   *)

UNSAFE MODULE DBRd;

IMPORT FS, File, OSError, Text;

CONST BIG = 16_1000000; (* 2^24 => 16M *)
TYPE BufPtr = UNTRACED REF ARRAY [0..BIG-1] OF File.Byte;

REVEAL
  T = T_ BRANDED OBJECT
    buf : REF ARRAY OF CHAR;
    cur : CARDINAL;
    len : INTEGER;
  OVERRIDES
    init     := Init;
    get_int  := GetInt;
    get_line := GetLine;
    close    := Close;
  END;

PROCEDURE Init (t: T;  path: TEXT): T =
  VAR f: File.T;  sz: INTEGER;  ptr: BufPtr;
  BEGIN
    t.len := 0;
    t.cur := 0;
    TRY
      f     := FS.OpenFileReadonly (path);
      sz    := f.status().size;
      t.buf := NEW (REF ARRAY OF CHAR, MAX (1, sz));
      ptr   := ADR (t.buf[0]);
      t.len := f.read (SUBARRAY (ptr^, 0, NUMBER (t.buf^)), mayBlock := TRUE);
      f.close ();
    EXCEPT OSError.E => (* skip *)
    END;
    RETURN t;
  END Init;

PROCEDURE GetInt (t: T): INTEGER =
  VAR cur := t.cur;  len := t.len;  val := 0;  ch: CHAR;
  BEGIN
    (* skip white space *)
    WHILE (cur < len) DO
      ch := t.buf[cur];
      IF (ch # ' ') AND (ch # '\n') THEN EXIT; END;
      INC (cur);
    END;

    (* eat digits *)
    WHILE (cur < len) DO
      ch := t.buf[cur];  INC (cur);
      IF (ch < '0') OR ('9' < ch) THEN EXIT END;
      val := val * 10 + ORD (ch) - ORD ('0');
    END;

    t.cur := cur;
    RETURN val;
  END GetInt;

PROCEDURE GetLine (t: T): TEXT =
  VAR cur := t.cur;  len := t.len;  txt: TEXT;
  BEGIN
    (* scan to the new line *)
    WHILE (cur < len) AND (t.buf[cur] # '\n') DO INC (cur); END;
    txt := Text.FromChars (SUBARRAY (t.buf^, t.cur, cur - t.cur));
    t.cur := cur+1;
    RETURN txt;
  END GetLine;

PROCEDURE Close (t: T) =
  BEGIN
    t.buf := NIL;
  END Close;

BEGIN
END DBRd.
