(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Feb 20 11:59:05 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QVal;

IMPORT Fmt, Convert, Text, TextF;
IMPORT M3ID, M3Buf, QValue, QVTbl, QVSeq, QMachine, QCode;
FROM Quake IMPORT Error, Machine;

TYPE
  QK = QValue.Kind;

PROCEDURE ToTag (m: Machine;  READONLY t: T): TEXT
  RAISES {Error} =
  VAR txt: TEXT;
  BEGIN
    CASE t.kind OF
    | QK.Var      => txt := "<variable " & M3ID.ToText (t.int) & ">";
    | QK.Integer  => txt := Fmt.Int (t.int);
    | QK.String   => txt := M3ID.ToText (t.int);
    | QK.Table    => txt := TableText (m, t.ref);
    | QK.Array    => txt := ArrayText (m, t.ref);
    | QK.Proc     => txt := ProcText (t.ref);
    END; (*CASE*)
    RETURN txt;
  END ToTag;

PROCEDURE ToBool (m: Machine;  READONLY t: T): BOOLEAN
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.String THEN
      m.error ("attempting to use non-string value as a boolean")
    END;
    RETURN (t.int # QValue.BoolID [FALSE]);
  END ToBool;

PROCEDURE ToInt (m: Machine;  READONLY t: T): INTEGER
  RAISES {Error} =
  VAR txt: TEXT;  used, val: INTEGER;
  BEGIN
    IF t.kind = QK.Integer THEN
      RETURN t.int;
    END;
    IF t.kind # QK.String THEN
      m.error ("cannot convert value to an integer")
    END;
    txt := M3ID.ToText (t.int);
    val := Convert.ToInt (txt^, used);
    IF (used # Text.Length (txt)) THEN
      m.error ("cannot convert value to an integer")
    END;
    RETURN val;
  END ToInt;

PROCEDURE ToText (m: Machine;  READONLY t: T): TEXT
  RAISES {Error} =
  BEGIN
    CASE t.kind OF
    | QK.Integer => RETURN Fmt.Int (t.int);
    | QK.String  => RETURN M3ID.ToText (t.int);
    | QK.Array   => RETURN ArrayText (m, t.ref);
    | QK.Table   => RETURN TableText (m, t.ref);
    ELSE m.error ("cannot convert value to string");  RETURN NIL;
    END;
  END ToText;

PROCEDURE ToID (m: Machine;  READONLY t: T): M3ID.T
  RAISES {Error} =
  BEGIN
    IF (t.kind = QK.String)
      THEN RETURN t.int;
      ELSE RETURN M3ID.Add (ToText (m, t));
    END;
  END ToID;

PROCEDURE ToTable (m: Machine;  READONLY t: T): QVTbl.T
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.Table THEN
      m.error ("cannot convert value to table");
    END;
    RETURN t.ref;
  END ToTable;

PROCEDURE ToArray (m: Machine;  READONLY t: T): QVSeq.T
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.Array THEN
      m.error ("cannot convert value to array");
    END;
    RETURN t.ref;
  END ToArray;

PROCEDURE ToProc (m: Machine;  READONLY t: T): QValue.Proc
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.Proc THEN
      m.error ("attempting to call a non-procedure value");
    END;
    RETURN t.ref;
  END ToProc;

PROCEDURE ToBuf (m: Machine;  READONLY t: T;  buf: M3Buf.T)
  RAISES {Error} =
  BEGIN
    CASE t.kind OF
    | QK.Integer => M3Buf.PutInt (buf, t.int);
    | QK.String  => M3ID.Put (buf, t.int);
    | QK.Array   => ArrayToBuf (m, t.ref, buf);
    | QK.Table   => TableToBuf (m, t.ref, buf);
    ELSE m.error ("cannot convert value to string");
    END;
  END ToBuf;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE TableText (m: Machine;  tbl: QVTbl.T): TEXT
  RAISES {Error} =
  VAR buf := M3Buf.New ();
  BEGIN
    TableToBuf (m, tbl, buf);
    RETURN M3Buf.ToText (buf);
  END TableText;

PROCEDURE TableToBuf (m: Machine;  tbl: QVTbl.T;  buf: M3Buf.T)
  RAISES {Error} =
  VAR
    iter  := tbl.iterate();
    key   : INTEGER;
    val   : T;
    first := TRUE;
  BEGIN
    WHILE iter.next(key, val) DO
      IF NOT first THEN M3Buf.PutChar (buf, ' '); END;
      ToBuf (m, val, buf);
      first := FALSE;
    END;
  END TableToBuf;

PROCEDURE ArrayText (m: Machine;  arr: QVSeq.T): TEXT
  RAISES {Error} =
  VAR buf := M3Buf.New ();
  BEGIN
    ArrayToBuf (m, arr, buf);
    RETURN M3Buf.ToText (buf);
  END ArrayText;

PROCEDURE ArrayToBuf (m: Machine;  arr: QVSeq.T;  buf: M3Buf.T)
  RAISES {Error} =
  BEGIN
    FOR i := 0 TO arr.size() - 1 DO
      IF i > 0 THEN M3Buf.PutChar (buf, ' '); END;
      ToBuf (m, arr.get(i), buf);
    END;
  END ArrayToBuf;

PROCEDURE ProcText (proc: QValue.Proc): TEXT =
  BEGIN
    RETURN "<procedure "
             & M3ID.ToText (proc.info.name)
             & " from "
             & M3ID.ToText (proc.info.code.source_file)
             & ">";
  END ProcText;

BEGIN
END QVal.
