(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Field.i3                                              *)
(* Last Modified On Fri Jun 24 09:30:30 PDT 1994 By kalsow     *)

INTERFACE Field;

IMPORT M3ID, Type, Value, Expr;

TYPE
  Info = RECORD
    name   : M3ID.T;
    index  : INTEGER;  (* 0..nFields-1 *)
    offset : INTEGER;  (* bit offset from beginning of record *)
    type   : Type.T;
    dfault : Expr.T;
  END;

PROCEDURE New (READONLY info: Info): Value.T;

PROCEDURE SetOffset (field: Value.T;  newOffset: INTEGER);

PROCEDURE Is (v: Value.T): BOOLEAN;
(* Returns TRUE iff 'v' is a field *)

PROCEDURE Split (field: Value.T;  VAR info: Info);

PROCEDURE EmitDeclaration (field: Value.T);
(* emit the C struct member or bit-field for 'field' *)

PROCEDURE IsEqual (va, vb: Value.T;  x: Type.Assumption): BOOLEAN;

END Field.
