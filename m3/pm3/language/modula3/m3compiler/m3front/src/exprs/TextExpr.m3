(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TextExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:47:35 PST 1995 by kalsow     *)
(*      modified on Sun Feb 24 04:07:17 1991 by muller         *)

MODULE TextExpr;

IMPORT M3, CG, Expr, ExprRep, M3String, Textt, Type, M3Buf;
IMPORT Target, Module, M3RT;

TYPE
  P = Expr.T OBJECT
        value : M3String.T;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := ExprRep.NoCheck;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
	isZeroes     := IsZeroes;
	genFPLiteral := GenFPLiteral;
	prepLiteral  := PrepLiteral;
	genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

TYPE
  LiteralTable = REF ARRAY OF INTEGER;

VAR nextID      : INTEGER := 0;
VAR literals    : LiteralTable := NIL;
VAR global_data : CG.Var := NIL;

PROCEDURE Reset () =
  BEGIN
    nextID := 0;
    global_data := NIL;
    (* literals := NIL; *)
    IF (literals # NIL) THEN
      FOR i := FIRST (literals^) TO LAST (literals^) DO literals[i] := 0; END;
    END;
  END Reset;

PROCEDURE New (value: M3String.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.value   := value;
    p.type    := Textt.T;
    p.checked := TRUE;
    RETURN p;
  END New;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.value = b.value);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE SetUID (p: P): INTEGER =
  VAR
    Header_offset  := 0;
    Pointer_offset := Header_offset  + Target.Address.pack;
    Length_offset  := Pointer_offset + Target.Address.pack;
    Chars_offset   := Length_offset  + Target.Integer.pack;
  VAR
    uid  := M3String.GetUID (p.value);
    len  : INTEGER;
    x    : INTEGER;
  BEGIN
    (* assign this value a unique ID *)
    IF (uid < 0) THEN
      uid := nextID;  INC (nextID);
      M3String.SetUID (p.value, uid);
    END;

    (* make sure there's room in the table *)
    IF (literals = NIL) OR (LAST (literals^) < uid) THEN ExpandLiterals () END;

    x := literals [uid];
    IF (x # 0) THEN RETURN uid END;

    IF (global_data = NIL) THEN
      global_data := Module.GlobalData (NIL);
    END;

    len := M3String.Length (p.value) + 1;

    (* allocate the variable *)
    x := Module.Allocate (Chars_offset + len * Target.Char.size,
                           Target.Address.align, "*TEXT literal*");
    literals[uid] := x;
 
    (* initialize the variable *)
    CG.Init_intt (x+Header_offset + M3RT.RH_typecode_offset,
                  M3RT.RH_typecode_size, M3RT.TEXT_typecode);
    CG.Init_var  (x+Pointer_offset, global_data, x+Chars_offset);
    CG.Init_intt (x+Length_offset, Target.Integer.size, len);
    M3String.Init_chars (x+Chars_offset, p.value);

    RETURN uid;
  END SetUID;

PROCEDURE ExpandLiterals () =
  VAR new: LiteralTable;
  BEGIN
    IF (literals = NIL) THEN
      new := NEW (LiteralTable, 200);
    ELSE
      new := NEW (LiteralTable, 2 * NUMBER (literals^));
      SUBARRAY (new^, 0, NUMBER (literals^)) := literals^;
    END;
    literals := new;
  END ExpandLiterals;

PROCEDURE Compile (p: P) =
  VAR uid := SetUID (p);
  BEGIN
    CG.Load_addr_of (global_data, literals[uid] + Target.Address.pack,
                     Target.Address.align);
  END Compile;

PROCEDURE Split (e: Expr.T;  VAR value: M3String.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => value := p.value;  RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE Cat (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR sa, sb: M3String.T;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => sa := p.value;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => sb := p.value;
    ELSE      RETURN FALSE;
    END;
    c := New (M3String.Concat (sa, sb));
    RETURN TRUE;
  END Cat;

PROCEDURE IsZeroes (<*UNUSED*>p: P): BOOLEAN = 
  BEGIN
    RETURN FALSE;
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "TEXT<");
    M3Buf.PutInt  (buf, M3String.Length (p.value));
    M3Buf.PutChar (buf, ',');
    M3String.Put  (buf, p.value);
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE PrepLiteral (p: P;  <*UNUSED*>type: Type.T) =
  BEGIN
    EVAL SetUID (p);
  END PrepLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*>type: Type.T) =
  VAR uid := SetUID (p);
  BEGIN
    CG.Init_var (offset, global_data, literals[uid] + Target.Address.pack);
  END GenLiteral;

BEGIN
END TextExpr.
