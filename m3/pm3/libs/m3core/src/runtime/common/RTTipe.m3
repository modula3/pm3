(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jul 23 13:34:38 PDT 1996 by heydon     *)
(*      modified on Wed Jun 22 14:00:41 PDT 1994 by kalsow     *)

UNSAFE MODULE RTTipe;

IMPORT RT0, RTType, RTTypeSRC, RTPacking, Word;

TYPE (* This list must be kept in sync with the compiler *)
  Op = {
  (* opcode         -- op  --- operands ------------------- *)
     Address,       (* 00                                   *)
     Array,         (* 01, #elements: INT, element: TYPE    *)
     Boolean,       (* 02                                   *)
     Cardinal,      (* 03                                   *)
     Char,          (* 04                                   *)
     Enum,          (* 05, #elements: INT                   *)
     Extended,      (* 06                                   *)
     Integer,       (* 07                                   *)
     Longreal,      (* 08                                   *)
     Null,          (* 09                                   *)
     Object,        (* 0a, #fields: INT, {fields: TYPE}     *)
     OpenArray,     (* 0b, #dimensions: INT, element: TYPE  *)
     Packed,        (* 0c, bit size: INT, base type: TYPE   *)
     Proc,          (* 0d                                   *)
     Real,          (* 0e                                   *)
     Record,        (* 0f, #fields: INT, {fields: TYPE}     *)
     Ref,           (* 10, self id: UID                     *)
     Refany,        (* 11                                   *)
     Set,           (* 12, #elements: INT                   *)
     Subrange,      (* 13, min, max: INT                    *)
     UntracedRef,   (* 14, self id: UID                     *)
     OldN,          (* 15, node #: INT                      *)
     Old0           (* 16                                   *)
  };(* Old1, Old2, ... Old(255-ORD(Old0)) *)

TYPE Byte  = BITS 8 FOR [0..255];
TYPE Ptr   = UNTRACED REF Byte;
TYPE TList = REF ARRAY OF T;

TYPE
  State = RECORD
    self    : RT0.TypeDefn;
    map     : ADDRESS;
    ptr     : Ptr;
    n_types : INTEGER;
    types   : TList;
    next    : INTEGER;
  END;

TYPE
  Packing = RECORD
    max_align    : INTEGER;
    struct_align : INTEGER;
    word_size    : INTEGER;
    word_align   : INTEGER;
  END;

PROCEDURE Get (typecode: INTEGER;  READONLY packing: RTPacking.T): T =
  VAR t: T := NIL;  s: State;  p: Packing;
  BEGIN
    IF (typecode < 0) OR (RTType.MaxTypecode() < typecode) THEN RETURN NIL; END;
    s.self := RTType.Get (typecode);

    (* read the description *)
    s.map  := s.self.type_desc;
    IF (s.map = NIL) THEN RETURN NIL; END;
    s.ptr     := LOOPHOLE (s.map, Ptr);
    s.n_types := GetInt (s.ptr);
    s.types   := NEW (TList, s.n_types);
    s.next    := 0;
    t         := ReadOp (s);
    <*ASSERT s.next = s.n_types*>

    (* add the sizes, alignments, and field offsets *)
    p.max_align    := packing.max_align;
    p.struct_align := packing.struct_align;
    p.word_size    := packing.word_size;
    p.word_align   := MIN (p.word_size, p.max_align);
    FixSizes (t, p);

    RETURN t;
  END Get;

(*---------------------------------------------------------------- reader ---*)

PROCEDURE ReadOp (VAR s: State): T =
  VAR stuffed := FALSE;  x: Byte := s.ptr^;  t: T;
  BEGIN
    INC (s.ptr, ADRSIZE (s.ptr^));
    CASE x OF
    | ORD (Op.Address)  => t := NEW (Builtin, kind := Kind.Address);
    | ORD (Op.Boolean)  => t := NEW (Builtin, kind := Kind.Boolean);
    | ORD (Op.Cardinal) => t := NEW (Builtin, kind := Kind.Cardinal);
    | ORD (Op.Char)     => t := NEW (Builtin, kind := Kind.Char);
    | ORD (Op.Extended) => t := NEW (Builtin, kind := Kind.Extended);
    | ORD (Op.Integer)  => t := NEW (Builtin, kind := Kind.Integer);
    | ORD (Op.Longreal) => t := NEW (Builtin, kind := Kind.Longreal);
    | ORD (Op.Null)     => t := NEW (Builtin, kind := Kind.Null);
    | ORD (Op.Proc)     => t := NEW (Builtin, kind := Kind.Proc);
    | ORD (Op.Real)     => t := NEW (Builtin, kind := Kind.Real);
    | ORD (Op.Refany)   => t := NEW (Builtin, kind := Kind.Refany);

    | ORD (Op.Array) =>
        VAR arr := NEW (Array, kind := Kind.Array, n_elts := GetInt (s.ptr));
        BEGIN
          t := arr;  s.types [s.next] := t;  INC (s.next);  stuffed := TRUE;
          arr.element := ReadOp (s);
        END;

    | ORD (Op.Enum) =>
        t := NEW (Enum, kind := Kind.Enum, n_elts := GetInt (s.ptr));

    | ORD (Op.Object) =>
        VAR obj := NEW (Object, kind := Kind.Object,
                        super := LOOPHOLE (s.self, RT0.ObjectTypeDefn).parent,
                        fields := NIL);
        BEGIN
          t := obj;  s.types [s.next] := t;  INC (s.next);  stuffed := TRUE;
          obj.fields := GetFields (s);
        END;

    | ORD (Op.OpenArray) =>
        VAR oarr := NEW (OpenArray, kind := Kind.OpenArray,
                         n_dimensions := GetInt (s.ptr));
        BEGIN
          t := oarr;  s.types [s.next] := t;  INC (s.next);  stuffed := TRUE;
          oarr.element := ReadOp (s);
        END;

    | ORD (Op.Packed) =>
        VAR pack := NEW (Packed, kind := Kind.Packed,
                         n_bits := GetInt (s.ptr));
        BEGIN
          t := pack;  s.types [s.next] := t;  INC (s.next);  stuffed := TRUE;
          pack.base := ReadOp (s);
        END;

    | ORD (Op.Record) =>
        VAR rec := NEW (Record, kind := Kind.Record, fields := NIL); BEGIN
          t := rec;  s.types [s.next] := t;  INC (s.next);  stuffed := TRUE;
          rec.fields := GetFields (s);
        END;

    | ORD (Op.Ref) =>
        VAR ref := NEW (Ref, kind := Kind.Ref, traced := TRUE,
                        uid := GetUID (s.ptr));
        BEGIN
          t := ref;  ref.self := RTTypeSRC.FindType (ref.uid);
        END;

    | ORD (Op.Set) =>
        t := NEW (Set, kind := Kind.Set, n_elts := GetInt (s.ptr));

    | ORD (Op.Subrange) =>
        VAR subr := NEW (Subrange, kind := Kind.Subrange);  BEGIN
          t := subr;
          subr.min := GetInt (s.ptr);
          subr.max := GetInt (s.ptr);
        END;

    | ORD (Op.UntracedRef) =>
        VAR ref := NEW (Ref, kind := Kind.UntracedRef, traced := FALSE,
                        uid := GetUID (s.ptr));
        BEGIN
          t := ref;  ref.self := RTTypeSRC.FindType (ref.uid);
        END;

    | ORD (Op.OldN) =>
        t := s.types [GetInt (s.ptr)];

    ELSE (* Old0 .. 255 *)
        t := s.types [ORD (x) - ORD (Op.Old0)];

    END; (*CASE*)

    IF NOT stuffed THEN  s.types [s.next] := t;  INC (s.next);  END;
    RETURN t;
  END ReadOp;

PROCEDURE GetFields (VAR s: State): Field =
  VAR n := GetInt (s.ptr);  f, g, h: Field := NIL;
  BEGIN
    (* read the fields onto a stack *)
    WHILE (n > 0) DO
      f := NEW (Field, next := f, type := ReadOp (s));
      DEC (n);
    END;

    (* reverse the list *)
    g := NIL;  h := NIL;
    WHILE (f # NIL) DO
      h := f.next;
      f.next := g;
      g := f;
      f := h;
    END;

    RETURN g;
  END GetFields;

(*------------------------------------------------- low-level map readers ---*)

(*
   Integer values are encoded in a variable-length byte stream. The
   first byte contains two flag bits: the sign bit (s=16_80) and
   the special bit (x=16_40).  The rest of the first byte (v=16_3f)
   represents the values 0..16_3f.  So, given a stream x0, x1, x2, ...
   of 0..255 values where x0 is split into the fields (s/x/v), the
   represented integer value is:

          (x=0)             value = sign(s) * v
          (x=1) (v=1..8)    value = sign(s) * (SUM(i=1 to v) xi<<8*i)
    (s=0) (x=1) (v=16_3e)   value = +16_7fffffff  = +2^31 - 1
    (s=1) (x=1) (v=16_3e)   value = -16_80000000  = -2^31
    (s=0) (x=1) (v=16_3f)   value = +16_7fffffffffffffff  = +2^63 - 1
    (s=1) (x=1) (v=16_3f)   value = -16_8000000000000000  = -2^63

   where sign(0)=+1, sign(1)=-1.

*)
PROCEDURE GetInt (VAR ptr: Ptr): INTEGER =
  CONST SignBit = 16_80;  SpecialBit = 16_40;  ValueBits = 16_3f;
  CONST Sign = ARRAY BOOLEAN OF INTEGER { -1, +1 };
  VAR
    x       : Byte    := ptr^;
    sign    : INTEGER := Sign [Word.And (x, SignBit) = 0];
    special : BOOLEAN := Word.And (x, SpecialBit) # 0;
    val     : INTEGER := Word.And (x, ValueBits);
  BEGIN
    INC (ptr, ADRSIZE (ptr^));
    IF NOT special THEN
      (* small value:  -63..+63 *)
      RETURN sign * val;
    ELSIF (val = 16_3e) THEN
      (* 32-bit FIRST/LAST of integer *)
      IF (sign < 0)
        THEN RETURN -16_7fffffff-1;
        ELSE RETURN 16_7fffffff;
      END;
    ELSIF (val = 16_3f) THEN
      (* 64-bit FIRST/LAST of integer *)
      IF BITSIZE (INTEGER) # 64 THEN <*ASSERT FALSE*>
      ELSIF (sign < 0)          THEN RETURN FIRST (INTEGER);
      ELSE                           RETURN LAST (INTEGER);
      END;
    ELSE
      (* arbitrary value *)
      <*ASSERT BYTESIZE (INTEGER) >= val *>
      VAR res := 0;  shift := 0;  BEGIN
        FOR i := 1 TO val DO
          res := Word.Or (res, Word.LeftShift (ptr^, shift));
          INC (shift, 8);
          INC (ptr, ADRSIZE (ptr^));
        END;
        RETURN sign * res;
      END;
    END;
  END GetInt;

PROCEDURE GetUID (VAR ptr: Ptr): INTEGER =
  VAR a, b, c, d: INTEGER;
  BEGIN
    a := ptr^;  INC (ptr, ADRSIZE (ptr^));
    b := ptr^;  INC (ptr, ADRSIZE (ptr^));
    c := ptr^;  INC (ptr, ADRSIZE (ptr^));
    d := ptr^;  INC (ptr, ADRSIZE (ptr^));
    RETURN Word.Or (
             Word.Or (Word.LeftShift (a,  0), Word.LeftShift (b, 8)),
             Word.Or (Word.LeftShift (c, 16), Word.LeftShift (d, 24)));
  END GetUID;

(*----------------------------------------------------------------- sizes ---*)

PROCEDURE FixSizes (t: T;  READONLY p: Packing) =
  BEGIN
    IF (t = NIL) OR (t.align # 0) THEN RETURN; END;
    CASE t.kind OF
    | Kind.Address,
      Kind.Cardinal,
      Kind.Integer,
      Kind.Null,
      Kind.Proc,
      Kind.Ref,
      Kind.Refany,
      Kind.UntracedRef =>
        t.size  := p.word_size;
        t.align := p.word_align;

    | Kind.Boolean,
      Kind.Char =>
        t.size  := 8;
        t.align := 8;

    | Kind.Real =>
        t.size  := 32;
        t.align := MIN (t.size, p.max_align);

    | Kind.Longreal,
      Kind.Extended =>
        t.size  := 64;
        t.align := p.max_align;

    | Kind.Enum =>
        VAR enum: Enum := t; BEGIN
          IF    (enum.n_elts <= 256)         THEN t.size := 8;
          ELSIF (enum.n_elts <= 65535)       THEN t.size := 16;
          ELSIF (enum.n_elts <= 16_7fffffff) THEN t.size := 32;
          ELSE                                    t.size := 64;
          END;
          t.align := MIN (t.size, p.max_align);
        END;

    | Kind.Subrange =>
        VAR sub: Subrange := t; BEGIN
          IF    (0 <= sub.min) AND (sub.max <= 255)         THEN t.size := 8;
          ELSIF (-128 <= sub.min) AND (sub.max <= 127)      THEN t.size := 8;
          ELSIF (0 <= sub.min) AND (sub.max <= 65535)       THEN t.size := 16;
          ELSIF (-32768 <= sub.min) AND (sub.max <= 32767)  THEN t.size := 16;
          ELSIF (p.word_size <= 32)                         THEN t.size := 32;
          ELSIF (0 <= sub.min) AND (sub.max <= 16_ffffffff) THEN t.size := 32;
          ELSIF (-16_7fffffff-1 <= sub.min)
                            AND (sub.max <= 16_7fffffff)    THEN t.size := 32;
          ELSE                                                   t.size := 64;
          END;
          t.align := MIN (t.size, p.max_align);
        END;

    | Kind.Set =>
        VAR set: Set := t; BEGIN
          t.size  := RoundUp (set.n_elts, p.word_size);
          t.align := MAX (p.word_align, p.struct_align);
        END;

    | Kind.Packed =>
        VAR pack: Packed := t; BEGIN
          FixSizes (pack.base, p);
          t.size  := pack.n_bits;
          t.align := pack.base.align;
        END;

    | Kind.Object =>
        VAR obj: Object := t; BEGIN
          t.size  := p.word_size;
          t.align := p.word_align;
          FixFields (obj.fields, p, obj.field_size, obj.field_align);
        END;

    | Kind.Record =>
        VAR rec: Record := t; BEGIN
          FixFields (rec.fields, p, t.size, t.align);
        END;

    | Kind.Array =>
        VAR arr: Array := t;  elt := arr.element;  BEGIN
          FixSizes (elt, p);
          IF (elt.kind = Kind.Packed) THEN
            arr.elt_pack := elt.size;
            t.align := FindArrayAlign (arr, elt, p);
          ELSE
            arr.elt_pack := RoundUp (elt.size, elt.align);
            t.align := elt.align;
          END;
          t.size := RoundUp (arr.n_elts * arr.elt_pack, t.align);
        END;

    | Kind.OpenArray =>
        VAR oa: OpenArray := t;  elt := oa.element;  BEGIN
          FixSizes (elt, p);
          IF (elt.kind = Kind.Packed)
            THEN oa.elt_pack := elt.size;
            ELSE oa.elt_pack := RoundUp (elt.size, elt.align);
          END;
          t.size  := -1;
          t.align := MAX (MAX (p.word_align, p.struct_align), elt.align);
        END;
    END; (*CASE*)

    t.align := MIN (t.align, p.max_align);
  END FixSizes;

PROCEDURE FixFields (fields: Field;  READONLY p: Packing;
                     VAR(*OUT*) recSize, recAlign: INTEGER) =
  VAR
    f      := fields;
    size   := 0;
    align  := p.struct_align;
    packed := FALSE;
  BEGIN
    WHILE (f # NIL) DO
      FixSizes (f.type, p);
      IF (f.type.kind = Kind.Packed) THEN
        packed := TRUE;
      ELSE
        size  := RoundUp (size, f.type.align);
        align := MAX (align, f.type.align);
      END;
      f.offset := size;
      INC (size, f.type.size);
      f := f.next;
    END;

    IF (packed) THEN
      (* find an alignment that avoids scalar word-boundary crossings *)
      align := FindRecordAlign (fields, align, p);
    END;

    size := RoundUp (size, align);
    (* make sure the record can be copied easily *)

    IF (size > 0) THEN
      (* find the largest alignment that doesn't change the record size *)
      VAR x := p.word_align; BEGIN
        WHILE (x > align) DO
          IF (size MOD x = 0) THEN align := x; EXIT; END;
          x := x DIV 2;
        END;
      END;
    END;

    recAlign := align;
    recSize  := size;
  END FixFields;

PROCEDURE FindRecordAlign (f: Field;  align: INTEGER;
                           READONLY p: Packing): INTEGER =
  BEGIN
    WHILE (align < p.max_align) DO
      IF FieldsAlignedOK (f, align, p) THEN RETURN align; END;
      align := 2 * align;
    END;
    RETURN p.max_align;
  END FindRecordAlign;

PROCEDURE FieldsAlignedOK (f: Field;  align: INTEGER;
                           READONLY p: Packing): BOOLEAN =
  VAR ff: Field;  rec_offs := 0;
  BEGIN
    REPEAT
      ff := f;
      WHILE (ff # NIL) DO
        IF NOT IsAlignedOK (ff.type, rec_offs + ff.offset, p) THEN
          RETURN FALSE;
        END;
        ff := ff.next;
      END;
      rec_offs := (rec_offs + align) MOD p.word_size;
    UNTIL (rec_offs = 0);
    RETURN TRUE;
  END FieldsAlignedOK;

PROCEDURE FindArrayAlign (arr: Array;  elt: T;  READONLY p: Packing): INTEGER =
  VAR align := elt.align;
  BEGIN
    WHILE (align < p.max_align) DO
      IF IsAlignedOK (arr, align, p) THEN RETURN align; END;
      align := 2 * align;
    END;
    RETURN p.max_align;
  END FindArrayAlign;

PROCEDURE IsAlignedOK (t: T;  offset: INTEGER;  READONLY p: Packing): BOOLEAN =
  BEGIN
    CASE t.kind OF
    | Kind.Address,
      Kind.Cardinal,
      Kind.Integer,
      Kind.Null,
      Kind.Proc,
      Kind.Ref,
      Kind.Refany,
      Kind.UntracedRef,
      Kind.Real,
      Kind.Longreal,
      Kind.Extended,
      Kind.Set =>
        (* types that must be aligned *)
        RETURN (offset MOD t.align = 0);


    | Kind.Enum,
      Kind.Subrange,
      Kind.Boolean,
      Kind.Char =>
        (* ok as long as they can be loaded from a single word *)
        VAR z0 := offset DIV p.word_align * p.word_align;  BEGIN
          RETURN (offset + t.size) <= (z0 + p.word_size);
        END;

    | Kind.Packed =>
        VAR
          pack: Packed := t;
          z0 := offset DIV p.word_align * p.word_align;
        BEGIN
          IF IsAlignedOK (pack.base, offset, p) THEN RETURN TRUE END;
          RETURN (offset + pack.size) <= (z0 + p.word_size);
        END;

    | Kind.Object =>
        VAR obj: Object := t;  BEGIN
          RETURN FieldsAlignedOK (obj.fields, offset, p);
        END;

    | Kind.Record =>
        VAR rec: Record := t;  BEGIN
          RETURN FieldsAlignedOK (rec.fields, offset, p);
        END;

    | Kind.Array =>
        VAR
          arr : Array := t;
          elt := arr.element;
          x0  := offset MOD p.word_size;
          x   := x0;
        BEGIN
          FOR i := 0 TO arr.n_elts-1 DO
            IF NOT IsAlignedOK (elt, x, p) THEN RETURN FALSE END;
            x := (x + arr.elt_pack) MOD p.word_size;
            IF (x = x0) THEN EXIT END;
          END;
          RETURN TRUE;
        END;

    | Kind.OpenArray =>
        VAR
          oa  : OpenArray := t;
          elt := oa.element;
          x0  := offset MOD p.word_size;
          x   := x0;
        BEGIN
          WHILE (elt.kind = Kind.OpenArray) DO
            oa := elt;
            elt := oa.element;
          END;
          REPEAT
            IF NOT IsAlignedOK (elt, x, p) THEN RETURN FALSE END;
            x := (x + oa.elt_pack) MOD p.word_size;
          UNTIL (x = x0);
          RETURN TRUE;
        END;

    END; (*CASE*)
  END IsAlignedOK;

PROCEDURE RoundUp (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN (a + b - 1) DIV b * b;
  END RoundUp;

BEGIN
END RTTipe.
