(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:03:47 PDT 1995 by kalsow     *)
(*      modified on Tue Jun  1 14:56:30 PDT 1993 by muller     *)

MODULE M3CG_Check;

IMPORT Wr, Thread, Fmt, Text, Stdio, IntIntTbl;
IMPORT M3ID, M3CG, M3CG_Ops, Target, TargetMap;

FROM M3CG IMPORT Name, ByteOffset, CallingConvention;
FROM M3CG IMPORT ByteSize, Alignment, Frequency;
FROM M3CG IMPORT Var, Proc, Label, Sign;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;

TYPE (* stack data types *)
  ST = { Addr, Word, Int, Reel, LReel, XReel, Void,
         IType, RType, AType,
         Any, Missing, DontCare, Match };

CONST
  T_to_ST = ARRAY Type OF ST {
    ST.Addr, ST.Int (*ST.Word*), ST.Int,
    ST.Reel, ST.LReel, ST.XReel,
    ST.Int, ST.Int, ST.Int, ST.Int,  (* Int_? *)
    ST.Int, ST.Int, ST.Int, ST.Int,  (* Word_? *)
    ST.Addr, (*Struct*)
    ST.Void
  };

CONST
  ST_name = ARRAY ST OF TEXT {
    "Addr ", "Word ", "Int ", "Real ", "LReal ", "ExReal ", "Void ",
    "W,I ", "R,L,E ", "W,I,R,L,E ",
    "any ", "", "", "<=match "
  };

TYPE
  U = M3CG.T OBJECT
        clean_stores := FALSE;
        clean_jumps  := FALSE;
        nested_calls := TRUE;
        nested_procs := FALSE;
        cur_line     : INTEGER := 0;
        next_var     := 1;
        next_proc    := 1;
        next_scope   := 1;
        n_errors     := 0;
        proc_count   := 0;
        block_count  := 0;
        call_count   := 0;
        top_of_stack := 0;
        in_init      := 0;
        init_cursor  := 0;
        note_error   : M3CG_Ops.ErrorHandler := NIL;
        runtime      : IntIntTbl.T := NIL;  (* Name -> BOOL *)
(*        temps        : IntIntTbl.T  := NIL; (* Var -> line number *) *)
        stack        : ARRAY [0..50] OF Type;
      METHODS
        s_pop (s0, s1, s2, s3 := ST.DontCare) := Stack_Pop;
        s_push (t: Type) := Stack_Push;
        s_repush () := Stack_Repush;
        s_empty () := Stack_Empty;
      OVERRIDES
        set_error_handler := set_error_handler;
        begin_unit := begin_unit;
        end_unit   := end_unit;
        set_source_line := set_source_line;
        set_runtime_proc := set_runtime_proc;
        set_runtime_hook := set_runtime_hook;
        get_runtime_hook := get_runtime_hook;
        bind_segment := bind_segment;
        declare_temp   := declare_temp;
        free_temp := free_temp;
        begin_init := begin_init;
        end_init := end_init;
        init_int := init_int;
        init_proc := init_proc;
        init_label := init_label;
        init_var := init_var;
        init_offset := init_offset;
        init_chars := init_chars;
        init_float := init_float;
        begin_procedure := begin_procedure;
        end_procedure := end_procedure;
        begin_block := begin_block;
        end_block := end_block;
        note_procedure_origin := note_procedure_origin;
        set_label := set_label;
        jump := jump;
        if_true  := if_true;
        if_false := if_false;
        if_eq := if_eq;
        if_ne := if_ne;
        if_gt := if_gt;
        if_ge := if_ge;
        if_lt := if_lt;
        if_le := if_le;
        case_jump := case_jump;
        exit_proc := exit_proc;
        load  := load;
        store := store;
        store_ref := store_ref;
        load_address := load_address;
        load_indirect := load_indirect;
        store_indirect := store_indirect;
        store_ref_indirect := store_ref_indirect;
        load_nil      := load_nil;
        load_integer  := load_integer;
        load_float    := load_float;
        eq       := eq;
        ne       := ne;
        gt       := gt;
        ge       := ge;
        lt       := lt;
        le       := le;
        add      := add;
        subtract := subtract;
        multiply := multiply;
        divide   := divide;
        div      := div;
        mod      := mod;
        negate   := negate;
        abs      := abs;
        max      := max;
        min      := min;
        round    := round;
        trunc    := trunc;
        floor    := floor;
        ceiling  := ceiling;
        cvt_float := cvt_float;
        set_union          := set_union;
        set_difference     := set_difference;
        set_intersection   := set_intersection;
        set_sym_difference := set_sym_difference;
        set_member         := set_member;
        set_eq       := set_eq;
        set_ne       := set_ne;
        set_gt       := set_gt;
        set_ge       := set_ge;
        set_lt       := set_lt;
        set_le       := set_le;
        set_range    := set_range;
        set_singleton := set_singleton;
        not := not;
        and := and;
        or  := or;
        xor := xor;
        shift        := shift;
        shift_left   := shift_left;
        shift_right  := shift_right;
        rotate       := rotate;
        rotate_left  := rotate_left;
        rotate_right := rotate_right;
        extract := extract;
        extract_n := extract_n;
        extract_mn := extract_mn;
        insert  := insert;
        insert_n  := insert_n;
        insert_mn  := insert_mn;
        swap := swap;
        pop  := pop;
        copy := copy;
        copy_n := copy_n;
        zero := zero;
        zero_n := zero_n;
        loophole := loophole;
        assert_fault := assert_fault;
        narrow_fault := narrow_fault;
        return_fault := return_fault;
        case_fault := case_fault;
        typecase_fault := typecase_fault;
        check_nil := check_nil;
        check_lo := check_lo;
        check_hi := check_hi;
        check_range := check_range;
        check_index := check_index;
        check_eq := check_eq;
        add_offset := add_offset;
        index_address := index_address;
        start_call_direct := start_call_direct;
        call_direct := call_direct;
        start_call_indirect := start_call_indirect;
        call_indirect := call_indirect;
        pop_param := pop_param;
        pop_struct := pop_struct;
        pop_static_link := pop_static_link;
        load_procedure := load_procedure;
        load_static_link := load_static_link;
      END;
        

(*----------------------------------------------- binary/ASCII conversion ---*)


VAR Ints := ARRAY [0..1023] OF TEXT { NIL, .. };

PROCEDURE Int (i: INTEGER): TEXT =
  BEGIN
    IF (FIRST (Ints) <= i) AND (i <= LAST (Ints)) THEN
      IF (Ints[i] = NIL) THEN Ints [i] := " " & Fmt.Int (i) END;
      RETURN Ints [i];
    ELSE
      RETURN " " & Fmt.Int (i);
    END;
  END Int;

(*--------------------------------------------------------- low level I/O ---*)

PROCEDURE PutErr (u: U;  a, b, c: TEXT := NIL) =
  BEGIN
    u.child.comment ("********* M3CG_Check ERROR *********** ", a, b, c);
    INC (u.n_errors);
  END PutErr;

(*-------------------------------------------------------- stack checking ---*)

PROCEDURE Stack_Get (self: U;  depth: INTEGER): ST =
  VAR x := self.top_of_stack - depth - 1;
  BEGIN
    IF (FIRST (self.stack) <= x) AND (x <= LAST (self.stack))
      THEN RETURN T_to_ST [self.stack [x]];
      ELSE RETURN ST.Missing;
    END;
  END Stack_Get;

PROCEDURE IsOK (need, got, prev: ST): BOOLEAN =
  CONST
    min_IType = T_to_ST [FIRST (IType)];
    max_IType = T_to_ST [LAST  (IType)];
    min_RType = T_to_ST [FIRST (RType)];
    max_RType = T_to_ST [LAST  (RType)];
    min_AType = T_to_ST [FIRST (AType)];
    max_AType = T_to_ST [LAST  (AType)];
    min_Type  = T_to_ST [FIRST (Type)];
    max_Type  = T_to_ST [LAST  (Type)];
  BEGIN
    CASE need OF
    | ST.Void     => RETURN (got = ST.Missing);
    | ST.IType    => RETURN (min_IType <= got) AND (got <= max_IType);
    | ST.RType    => RETURN (min_RType <= got) AND (got <= max_RType);
    | ST.AType    => RETURN (min_AType <= got) AND (got <= max_AType);
    | ST.Any      => RETURN (min_Type  <= got) AND (got <= max_Type);
    | ST.DontCare => RETURN TRUE;
    | ST.Match    => RETURN got = prev;
    ELSE             RETURN (got = need);
    END;
  END IsOK;

PROCEDURE ST_Name (a, prev: ST): TEXT =
  BEGIN
    IF (a = ST.Match) THEN a := prev END;
    RETURN ST_name [a];
  END ST_Name;

PROCEDURE Stack_Pop (self: U;  a, b, c, d: ST) =
  VAR
    s0 := Stack_Get (self, 0);
    s1 := Stack_Get (self, 1);
    s2 := Stack_Get (self, 2);
    s3 := Stack_Get (self, 3);
  BEGIN
    IF IsOK (a, s0, a) AND IsOK (b, s1, a)
      AND IsOK (c, s2, b) AND IsOK (d, s3, c) THEN
      (* no error *)
    ELSE
      PutErr (self, "bad stack:  expected [ ",
        ST_Name (a, a) & ST_Name (b, a)
          & ST_Name (c, b) & ST_Name (d, c),
        "] got [ " &
        ST_Name (s0, s0) & ST_Name (s1, s0)
          & ST_Name (s2, s1) & ST_Name (s3, s2) & "]");
    END;
    IF    (d # ST.DontCare) THEN DEC (self.top_of_stack, 4)
    ELSIF (c # ST.DontCare) THEN DEC (self.top_of_stack, 3)
    ELSIF (b # ST.DontCare) THEN DEC (self.top_of_stack, 2)
    ELSE (*a # ST.DontCare*)     DEC (self.top_of_stack)
    END;
    IF (self.top_of_stack < 0) THEN self.top_of_stack := 0 END;
  END Stack_Pop;

PROCEDURE Stack_Push (self: U;  t: Type) =
  BEGIN
    IF (self.top_of_stack <= LAST (self.stack))
      THEN self.stack [self.top_of_stack] := t;
      ELSE PutErr (self, "stack overflow");
    END;
    INC (self.top_of_stack);
  END Stack_Push;

PROCEDURE Stack_Repush (self: U) =
  BEGIN
    INC (self.top_of_stack);
  END Stack_Repush;

PROCEDURE Stack_Empty (self: U) =
  BEGIN
    IF (self.top_of_stack > 0) THEN
      PutErr (self, "non-empty stack: ", Stack_Dump (self));
      self.top_of_stack := 0;
    END;
  END Stack_Empty;

(*************** DEBUGGING ********
PROCEDURE SDump (self: U) =
  BEGIN
    self.child.comment ("**** ", Stack_Dump (self));
  END SDump;
****************************************)

PROCEDURE Stack_Dump (self: U): TEXT =
  VAR s := "[ ";
  BEGIN
    FOR i := 0 TO MIN (self.top_of_stack - 1, 4) DO
      s := s & ST_name [Stack_Get (self, i)];
    END;
    IF (self.top_of_stack > 5) THEN
      s := s & "... ";
    END;
    s := s & "]";
    RETURN s;
  END Stack_Dump;

(*---------------------------------------------------------------------------*)

PROCEDURE New (child: M3CG.T;
               clean_jumps, clean_stores: BOOLEAN;
               nested_calls, nested_procs: BOOLEAN): M3CG.T =
  BEGIN
    child.set_error_handler (CrashAndBurn);
    RETURN NEW (U,
                child        := child,
                note_error   := CrashAndBurn,
                runtime      := NEW (IntIntTbl.Default).init (20),
                clean_jumps  := clean_jumps,
                clean_stores := clean_stores,
                nested_calls := nested_calls,
                nested_procs := nested_procs
               );
  END New;

PROCEDURE CrashAndBurn (msg: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    Wr.PutText (Stdio.stdout, "Unhandled M3CG_Check error: " & msg);
    Wr.Flush (Stdio.stdout);
    Wr.Flush (Stdio.stderr);
    <*ASSERT FALSE*>
  END CrashAndBurn;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (self: U;  p: M3CG_Ops.ErrorHandler) =
  BEGIN
    self.note_error := p;
    self.child.set_error_handler (p);
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (self: U;  optimize : INTEGER) =
  (* called before any other method to initialize the compilation unit *)
  BEGIN
    self.s_empty ();
    self.child.begin_unit (optimize);
  END begin_unit;

PROCEDURE end_unit   (self: U) =
  (* called after all other methods to finalize the unit and write the
     resulting object *)
  BEGIN
    self.s_empty ();
    self.child.end_unit ();
    IF (self.n_errors <= 0) THEN
      (* ok *)
    ELSIF (self.n_errors = 1) THEN
      self.note_error ("1 code generation error");
    ELSE (*self.n_errors > 1 *)
      self.note_error (Int (self.n_errors) & " code generation errors");
    END;
  END end_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_line (self: U; line: INTEGER) =
  BEGIN
    self.cur_line := line;
    self.child.set_source_line (line);
  END set_source_line;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (self: U;  n: Name;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    IF self.runtime.put (n, 0) THEN
      PutErr (self, "redefined runtime proc: ", M3ID.ToText (n));
    END;
    self.child.set_runtime_proc (n, p);
  END set_runtime_proc;

PROCEDURE set_runtime_hook (self: U;  n: Name;  v: Var;  o: ByteOffset) =
  BEGIN
    CheckVar (self, v);
    IF self.runtime.put (n, 0) THEN
      PutErr (self, "redefined runtime hook: ", M3ID.ToText (n));
    END;
    self.child.set_runtime_hook (n, v, o);
  END set_runtime_hook;

PROCEDURE get_runtime_hook (self: U;  n: Name; VAR p: Proc;  VAR v: Var; VAR o: ByteOffset) =
  VAR i: INTEGER;
  BEGIN
    IF NOT self.runtime.get (n, i) THEN
      PutErr (self, "undefined runtime hook: ", M3ID.ToText (n));
    END;
    self.child.get_runtime_hook (n, p, v, o);
  END get_runtime_hook;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE CheckVar (self: U;  v: Var) =
  BEGIN
    IF (v = NIL) THEN
      PutErr (self, "NIL variable");
    END;
  END CheckVar;

PROCEDURE bind_segment (self: U;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  BEGIN
    CheckVar (self, seg);
    self.child.bind_segment (seg, s, a, t, exported, inited);
  END bind_segment;

PROCEDURE declare_temp   (self: U;  s: ByteSize;  a: Alignment;  t: Type;
                          in_memory:BOOLEAN): Var =
  VAR v: Var;
  BEGIN
(*
    IF (self.temps = NIL) THEN 
      self.temps := NEW (IntIntTbl.Default).init (); END;
*)
    v := self.child.declare_temp (s, a, t, in_memory);
(*
    IF self.temps.put (v, self.cur_line) THEN
      PutErr (self, "temporary reused while live!");
    END;
*)
    RETURN v;
  END declare_temp;

PROCEDURE free_temp (self: U;  v: Var) =
  (* VAR line: INTEGER; *)
  BEGIN
    CheckVar (self, v);
(*
    IF (self.temps = NIL) THEN 
      self.temps := NEW (IntIntTbl.Default).init (); END;
    IF NOT self.temps.delete (v, line) THEN
      PutErr (self, "temp freed twice");
    END;
*)
    self.child.free_temp (v);
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (self: U;  v: Var) =
  BEGIN
    CheckVar (self, v);
    IF (self.in_init > 0) THEN
      PutErr (self, "nested static initialization");
    END;
    INC (self.in_init);
    self.init_cursor := 0;
    self.child.begin_init (v);
  END begin_init;

PROCEDURE end_init (self: U;  v: Var) =
  BEGIN
    CheckVar (self, v);
    IF (self.in_init > 0)
      THEN DEC (self.in_init);  self.init_cursor := 0;
      ELSE PutErr (self, "missing begin_init");
    END;
    self.child.end_init (v);
  END end_init;

PROCEDURE DoInit (self: U;  o: ByteOffset;  s: ByteSize) =
  BEGIN
    IF (self.in_init <= 0) THEN PutErr (self, "missing begin_init") END;
    IF (o >= self.init_cursor)
      THEN self.init_cursor := o + s;
      ELSE PutErr (self, "decreasing offsets");
    END;
  END DoInit;

PROCEDURE init_int (self: U;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
    DoInit (self, o, TargetMap.CG_Bytes[t]);
    self.child.init_int (o, value, t);
  END init_int;

PROCEDURE init_proc (self: U;  o: ByteOffset;  value: Proc) =
  BEGIN
    DoInit (self, o, Target.Address.bytes);
    self.child.init_proc (o, value);
  END init_proc;

PROCEDURE init_label (self: U;  o: ByteOffset;  value: Label) =
  BEGIN
    DoInit (self, o, Target.Address.bytes);
    self.child.init_label (o, value);
  END init_label;

PROCEDURE init_var (self: U;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
    DoInit (self, o, Target.Address.bytes);
    self.child.init_var (o, value, bias);
  END init_var;

PROCEDURE init_offset (self: U;  o: ByteOffset;  value: Var) =
  BEGIN
    DoInit (self, o, Target.Integer.bytes);
    self.child.init_offset (o, value);
  END init_offset;

PROCEDURE init_chars (self: U;  o: ByteOffset;  value: TEXT) =
  BEGIN
    DoInit (self, o, Text.Length (value) * Target.Char.bytes);
    self.child.init_chars (o, value);
  END init_chars;

PROCEDURE init_float (self: U;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    DoInit (self, o, TargetMap.Float_types[f.pre].bytes);
    self.child.init_float (o, f);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE CheckProc (self: U;  p: Proc) =
  BEGIN
    IF (p = NIL) THEN
      PutErr (self, "NIL procedure");
    END;
  END CheckProc;

PROCEDURE begin_procedure (self: U;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    IF (self.proc_count > 0) AND (NOT self.nested_procs) THEN
      PutErr (self, "nested procedure declaration");
    END;
    INC (self.proc_count);
    self.child.begin_procedure (p);
  END begin_procedure;

PROCEDURE end_procedure (self: U;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    IF (self.proc_count > 0)
      THEN DEC (self.proc_count);
      ELSE PutErr (self, "missing begin_procedure");
    END;
    IF (self.block_count > 0) AND (NOT self.nested_procs) THEN
      PutErr (self, "missing end_blocks: ", Int (self.block_count));
      self.block_count := 0;
    END;
    self.s_empty ();
(*
    IF (self.temps # NIL) THEN
      VAR it := self.temps.iterate (); k: REFANY; line: INTEGER; BEGIN
        WHILE it.next (tag, line) DO
          PutErr (self, "temp not freed, created on line ", Int (line));
        END;
      END;
    END;
*)
    self.child.end_procedure (p);
  END end_procedure;

PROCEDURE begin_block (self: U) =
  (* marks the beginning of a nested anonymous block *)
  BEGIN
    IF (self.proc_count <= 0) THEN
      PutErr (self, "begin_block not in procedure");
    END;
    self.s_empty ();
    INC (self.block_count);
    self.child.begin_block ();
  END begin_block;

PROCEDURE end_block (self: U) =
  (* marks the ending of a nested anonymous block *)
  BEGIN
    IF (self.block_count > 0)
      THEN DEC (self.block_count);
      ELSE PutErr (self, "missing begin_block");
    END;
    self.s_empty ();
    self.child.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (self: U;  p: Proc) =
  BEGIN
    CheckProc (self, p);
    self.s_empty ();
    self.child.note_procedure_origin (p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE CheckLabel (self: U;  l: Label) =
  BEGIN
    IF (l < 0) (*OR (self.next_label <= l)*) THEN
      PutErr (self, "undefined label: ", Int (l));
    END;
  END CheckLabel;

PROCEDURE set_label (self: U;  l: Label;  barrier: BOOLEAN) =
  (* define 'l' to be at the current pc *)
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.set_label (l, barrier);
  END set_label;

PROCEDURE jump (self: U; l: Label) =
  (* GOTO l *)
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.jump (l);
  END jump;

PROCEDURE if_true  (self: U; l: Label;  f: Frequency) =
  (* IF (s0.I # 0) GOTO l ; pop *)
  BEGIN
    self.s_pop (ST.Int);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_true (l, f);
  END if_true;

PROCEDURE if_false (self: U; l: Label;  f: Frequency) =
  (* IF (s0.I = 0) GOTO l ; pop *)
  BEGIN
    self.s_pop (ST.Int);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_false (l, f);
  END if_false;

PROCEDURE if_eq (self: U;  l: Label;  t: ZType;  f: Frequency) =
  (* IF (s1.t = s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_eq (l, t, f);
  END if_eq;

PROCEDURE if_ne (self: U;  l: Label;  t: ZType;  f: Frequency) =
  (* IF (s1.t # s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_ne (l, t, f);
  END if_ne;

PROCEDURE if_gt (self: U;  l: Label;  t: ZType;  f: Frequency) =
  (* IF (s1.t > s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_gt (l, t, f);
  END if_gt;

PROCEDURE if_ge (self: U;  l: Label;  t: ZType;  f: Frequency) =
  (* IF (s1.t >= s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_ge (l, t, f);
  END if_ge;

PROCEDURE if_lt (self: U;  l: Label;  t: ZType;  f: Frequency) =
  (* IF (s1.t < s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_lt (l, t, f);
  END if_lt;

PROCEDURE if_le (self: U;  l: Label;  t: ZType;  f: Frequency) =
  (* IF (s1.t <= s0.t) GOTO l ; pop(2) *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    IF (self.clean_jumps) THEN self.s_empty () END;
    CheckLabel (self, l);
    self.child.if_le (l, t, f);
  END if_le;

PROCEDURE case_jump (self: U; READONLY labels: ARRAY OF Label) =
  (* "GOTO labels[s0.I] ; pop" with no range checking on s0.I *)
  BEGIN
    self.s_pop (ST.Int);
    IF (self.clean_jumps) THEN self.s_empty () END;
    FOR i := FIRST (labels) TO LAST (labels) DO
      CheckLabel (self, labels [i]);
    END;
    self.child.case_jump (labels);
  END case_jump;

PROCEDURE exit_proc (self: U; t: Type) =
  (* Returns s0.t if the stack is non-empty, otherwise returns no value. *)
  BEGIN
    IF (t # Type.Void) THEN self.s_pop (T_to_ST [t]) END;
    self.s_empty ();
    self.child.exit_proc (t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (self: U;  v: Var;  o: ByteOffset;  t: MType) =
  BEGIN
    CheckVar (self, v);
    self.s_push (t);
    self.child.load (v, o, t);
  END load;

PROCEDURE store  (self: U;  v: Var;  o: ByteOffset;  t: MType) =
  BEGIN
    CheckVar (self, v);
    self.s_pop (T_to_ST [t]);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store (v, o, t);
  END store;

PROCEDURE store_ref (self: U;  v: Var;  o: ByteOffset) =
  BEGIN
    CheckVar (self, v);
    self.s_pop (ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store_ref (v, o);
  END store_ref;

PROCEDURE load_address (self: U;  v: Var;  o: ByteOffset) =
  BEGIN
    CheckVar (self, v);
    self.s_push (Type.Addr);
    self.child.load_address (v, o);
  END load_address;

PROCEDURE load_indirect (self: U;  o: ByteOffset;  t: MType) =
  BEGIN
    self.s_pop (ST.Addr);
    self.s_push (t);
    self.child.load_indirect (o, t);
  END load_indirect;

PROCEDURE store_indirect (self: U;  o: ByteOffset;  t: MType) =
  BEGIN
    self.s_pop (T_to_ST [t], ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store_indirect (o, t);
  END store_indirect;


PROCEDURE store_ref_indirect (self: U;  o: ByteOffset;  var: BOOLEAN) =
  BEGIN
    self.s_pop (ST.Addr, ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.store_ref_indirect (o, var);
  END store_ref_indirect;


(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (self: U) =
  (* push ; s0.A := a *)
  BEGIN
    self.s_push (Type.Addr);
    self.child.load_nil ();
  END load_nil;

PROCEDURE load_integer  (self: U;  READONLY i: Target.Int) =
  (* push ; s0.I := i *)
  BEGIN
    self.s_push (Type.Int);
    self.child.load_integer (i);
  END load_integer;

PROCEDURE load_float    (self: U;  READONLY f: Target.Float) =
  (* push ; s0.t := f *)
  CONST FType = ARRAY Target.Precision OF Type
                { Type.Reel, Type.LReel, Type.XReel };
  VAR t := FType [f.pre];
  BEGIN
    self.s_push (t);
    self.child.load_float (f);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE Binary (self: U;  lhs, rhs: Type) =
  (* s1.lhs := s1.rhs 'op' s0.rhs ; pop *)
  BEGIN
    self.s_pop (T_to_ST [rhs], ST.Match);
    self.s_push (lhs);
  END Binary;

PROCEDURE Unary (self: U;  lhs, rhs: Type) =
  (* s1.lhs := 'op' (s1.rhs) *)
  BEGIN
    self.s_pop (T_to_ST [rhs]);
    self.s_push (lhs);
  END Unary;

PROCEDURE eq (self: U;  t: ZType) =
  (* s1.I := (s1.t = s0.t)  ; pop *)
  BEGIN
    Binary (self, Type.Int, t);
    self.child.eq (t);
  END eq;

PROCEDURE ne (self: U;  t: ZType) =
  (* s1.I := (s1.t # s0.t)  ; pop *)
  BEGIN
    Binary (self, Type.Int, t);
    self.child.ne (t);
  END ne;

PROCEDURE gt (self: U;  t: ZType) =
  (* s1.I := (s1.t > s0.t)  ; pop *)
  BEGIN
    Binary (self, Type.Int, t);
    self.child.gt (t);
  END gt;

PROCEDURE ge (self: U;  t: ZType) =
  (* s1.I := (s1.t >= s0.t) ; pop *)
  BEGIN
    Binary (self, Type.Int, t);
    self.child.ge (t);
  END ge;

PROCEDURE lt (self: U;  t: ZType) =
  (* s1.I := (s1.t < s0.t)  ; pop *)
  BEGIN
    Binary (self, Type.Int, t);
    self.child.lt (t);
  END lt;

PROCEDURE le (self: U;  t: ZType) =
  (* s1.I := (s1.t <= s0.t) ; pop *)
  BEGIN
    Binary (self, Type.Int, t);
    self.child.le (t);
  END le;

PROCEDURE add (self: U;  t: AType) =
  (* s1.t := s1.t + s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.add (t);
  END add;

PROCEDURE subtract (self: U;  t: AType) =
  (* s1.t := s1.t - s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.subtract (t);
  END subtract;

PROCEDURE multiply (self: U;  t: AType) =
  (* s1.t := s1.t * s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.multiply (t);
  END multiply;

PROCEDURE divide (self: U;  t: RType) =
  (* s1.t := s1.t / s0.t ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.divide (t);
  END divide;

PROCEDURE div (self: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t DIV s0.t ; pop *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    self.s_push (t);
    self.child.div (t, a, b);
  END div;

PROCEDURE mod (self: U;  t: IType;  a, b: Sign) =
  (* s1.t := s1.t MOD s0.t ; pop *)
  BEGIN
    self.s_pop (T_to_ST [t], ST.Match);
    self.s_push (t);
    self.child.mod (t, a, b);
  END mod;

PROCEDURE negate (self: U;  t: AType) =
  (* s0.t := - s0.t *)
  BEGIN
    Unary (self, t, t);
    self.child.negate (t);
  END negate;

PROCEDURE abs      (self: U;  t: AType) =
  (* s0.t := ABS (s0.t) (noop on Words) *)
  BEGIN
    Unary (self, t, t);
    self.child.abs (t);
  END abs;

PROCEDURE max      (self: U;  t: ZType) =
  (* s1.t := MAX (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.max (t);
  END max;

PROCEDURE min      (self: U;  t: ZType) =
  (* s1.t := MIN (s1.t, s0.t) ; pop *)
  BEGIN
    Binary (self, t, t);
    self.child.min (t);
  END min;

PROCEDURE round    (self: U;  t: RType) =
  (* s0.I := ROUND (s0.t) *)
  BEGIN
    Unary (self, Type.Int, t);
    self.child.round (t);
  END round;

PROCEDURE trunc    (self: U;  t: RType) =
  (* s0.I := TRUNC (s0.t) *)
  BEGIN
    Unary (self, Type.Int, t);
    self.child.trunc (t);
  END trunc;

PROCEDURE floor    (self: U;  t: RType) =
  (* s0.I := FLOOR (s0.t) *)
  BEGIN
    Unary (self, Type.Int, t);
    self.child.floor (t);
  END floor;

PROCEDURE ceiling  (self: U;  t: RType) =
  (* s0.I := CEILING (s0.t) *)
  BEGIN
    Unary (self, Type.Int, t);
    self.child.ceiling (t);
  END ceiling;

PROCEDURE cvt_float    (self: U;  t: AType;  u: RType) =
  (* s0.u := FLOAT (s0.t, u) *)
  BEGIN
    self.s_pop (T_to_ST [t]);
    self.s_push (u);
    self.child.cvt_float (t, u);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (self: U;  s: ByteSize) =
  (* s2.B := s1.B + s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_union (s);
  END set_union;

PROCEDURE set_difference (self: U;  s: ByteSize) =
  (* s2.B := s1.B - s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_difference (s);
  END set_difference;

PROCEDURE set_intersection (self: U;  s: ByteSize) =
  (* s2.B := s1.B * s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_intersection (s);
  END set_intersection;

PROCEDURE set_sym_difference (self: U;  s: ByteSize) =
  (* s2.B := s1.B / s0.B ; pop(3) *)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr, ST.Addr);
    self.child.set_sym_difference (s);
  END set_sym_difference;

PROCEDURE set_member       (self: U;  s: ByteSize) =
  (* s1.I := (s0.I IN s1.B) ; pop *)
  BEGIN
    self.s_pop (ST.Int, ST.Addr);
    self.s_push (Type.Int);
    self.child.set_member (s);
  END set_member;

PROCEDURE set_eq       (self: U;  s: ByteSize) =
  (* s1.I := (s1.B = s0.B)  ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Addr);
    self.child.set_eq (s);
  END set_eq;

PROCEDURE set_ne (self: U;  s: ByteSize) =
  (* s1.I := (s1.B # s0.B)  ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Addr);
    self.child.set_ne (s);
  END set_ne;

PROCEDURE set_gt (self: U;  s: ByteSize) =
  (* s1.I := (s1.B > s0.B)  ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Addr);
    self.child.set_gt (s);
  END set_gt;

PROCEDURE set_ge (self: U;  s: ByteSize) =
  (* s1.I := (s1.B >= s0.B) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Addr);
    self.child.set_ge (s);
  END set_ge;

PROCEDURE set_lt (self: U;  s: ByteSize) =
  (* s1.I := (s1.B < s0.B)  ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Addr);
    self.child.set_lt (s);
  END set_lt;

PROCEDURE set_le (self: U;  s: ByteSize) =
  (* s1.I := (s1.B <= s0.B) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Addr);
    self.child.set_le (s);
  END set_le;

PROCEDURE set_range (self: U;  s: ByteSize) =
  (* s2.A [s1.I .. s0.I] := 1's; pop(3)*)
  BEGIN
    self.s_pop (ST.Int, ST.Int, ST.Addr);
    self.child.set_range (s);
  END set_range;

PROCEDURE set_singleton (self: U;  s: ByteSize) =
  (* s1.A [s0.I] := 1; pop(2) *)
  BEGIN
    self.s_pop (ST.Int, ST.Addr);
    self.child.set_singleton (s);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (self: U) =
  (* s0.I := Word.Not (s0.I) *)
  BEGIN
    Unary (self, Type.Int, Type.Int);
    self.child.not ();
  END not;

PROCEDURE and (self: U) =
  (* s1.I := Word.And (s1.I, s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.and ();
  END and;

PROCEDURE or  (self: U) =
  (* s1.I := Word.Or  (s1.I, s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.or ();
  END or;

PROCEDURE xor (self: U) =
  (* s1.I := Word.Xor (s1.I, s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.xor ();
  END xor;

PROCEDURE shift        (self: U) =
  (* s1.I := Word.Shift  (s1.I, s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.shift ();
  END shift;

PROCEDURE shift_left   (self: U) =
  (* s1.I := Word.Shift  (s1.I, s0.I) ; pop *)  
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.shift_left ();
  END shift_left;

PROCEDURE shift_right  (self: U) =
  (* s1.I := Word.Shift  (s1.I, -s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.shift_right ();
  END shift_right;

PROCEDURE rotate       (self: U) =
  (* s1.I := Word.Rotate (s1.I, s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.rotate ();
  END rotate;

PROCEDURE rotate_left  (self: U) =
  (* s1.I := Word.Rotate (s1.I, s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.rotate_left ();
  END rotate_left;

PROCEDURE rotate_right (self: U) =
  (* s1.I := Word.Rotate (s1.I, -s0.I) ; pop *)
  BEGIN
    Binary (self, Type.Int, Type.Int);
    self.child.rotate_right ();
  END rotate_right;

PROCEDURE extract (self: U;  sign: BOOLEAN) =
  (* s2.I := Word.Extract(s2.I, s1.I, s0.I);
     IF sign THEN SignExtend s2 END; pop(2) *)
  BEGIN
    self.s_pop (ST.Int, ST.Int, ST.Int);
    self.s_push (Type.Int);
    self.child.extract (sign);
  END extract;

PROCEDURE extract_n (self: U;  sign: BOOLEAN;  n: INTEGER) =
  (* s1.I := Word.Extract(s1.I, s0.I, n);
     IF sign THEN SignExtend s1 END; pop(1) *)
  BEGIN
    self.s_pop (ST.Int, ST.Int);
    self.s_push (Type.Int);
    self.child.extract_n (sign, n);
  END extract_n;

PROCEDURE extract_mn (self: U;  sign: BOOLEAN;  m, n: INTEGER) =
  (* s0.I := Word.Extract(s0.I, m, n);
     IF sign THEN SignExtend s0 END; *)
  BEGIN
    self.s_pop (ST.Int);
    self.s_push (Type.Int);
    self.child.extract_mn (sign, m, n);
  END extract_mn;

PROCEDURE insert  (self: U) =
  (* s3.I := Word.Insert (s3.I, s2.I, s1.I, s0.I) ; pop(3) *)
  BEGIN
    self.s_pop (ST.Int, ST.Int, ST.Int, ST.Int);
    self.s_push (Type.Int);
    self.child.insert ();
  END insert;

PROCEDURE insert_n  (self: U;  n: INTEGER) =
  (* s2.I := Word.Insert (s2.I, s1.I, s0.I, n) ; pop(2) *)
  BEGIN
    self.s_pop (ST.Int, ST.Int, ST.Int);
    self.s_push (Type.Int);
    self.child.insert_n (n);
  END insert_n;

PROCEDURE insert_mn  (self: U;  m, n: INTEGER) =
  (* s1.I := Word.Insert (s1.I, s0.I, m, n) ; pop(2) *)
  BEGIN
    self.s_pop (ST.Int, ST.Int);
    self.s_push (Type.Int);
    self.child.insert_mn (m, n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (self: U;  a, b: Type) =
  (* tmp := s1 ; s1 := s0 ; s0 := tmp *)
  BEGIN
    self.s_pop (T_to_ST [b], T_to_ST [a]);
    self.s_push (b);
    self.s_push (a);
    self.child.swap (a, b);
  END swap;

PROCEDURE pop  (self: U;  t: Type) =
  (* pop(1) (i.e. discard s0) *)
  BEGIN
    self.s_pop (T_to_ST [t]);
    self.child.pop (t);
  END pop;

PROCEDURE copy_n (self: U;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:s0.I] := Mem[s1.A:s0.I]; pop(3)*)
  BEGIN
    self.s_pop (ST.Int, ST.Addr, ST.Addr);
    self.child.copy_n (t, overlap);
  END copy_n;

PROCEDURE copy (self: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  (* Mem[s2.A:sz] := Mem[s1.A:sz]; pop(2)*)
  BEGIN
    self.s_pop (ST.Addr, ST.Addr);
    self.child.copy (n, t, overlap);
  END copy;

PROCEDURE zero_n (self: U;  t: MType) =
  (* Mem[s1.A:s0.I] := 0; pop(2) *)
  BEGIN
    self.s_pop (ST.Int, ST.Addr);
    self.child.zero_n (t);
  END zero_n;

PROCEDURE zero (self: U;  n: INTEGER;  t: MType) =
  (* Mem[s1.A:sz] := 0; pop(1) *)
  BEGIN
    self.s_pop (ST.Addr);
    self.child.zero (n, t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (self: U;  from, two: ZType) =
  (* s0.to := LOOPHOLE(s0.from, to) *)
  BEGIN
    self.s_pop (T_to_ST [from]);
    self.s_push (two);
    self.child.loophole (from, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE assert_fault (self: U) =
  BEGIN
    self.s_empty ();
    self.child.assert_fault ();
  END assert_fault;

PROCEDURE narrow_fault (self: U) =
  BEGIN
    self.child.narrow_fault ();
  END narrow_fault;

PROCEDURE return_fault (self: U) =
  BEGIN
    self.s_empty ();
    self.child.return_fault ();
  END return_fault;

PROCEDURE case_fault (self: U) =
  BEGIN
    self.s_empty ();
    self.child.case_fault ();
  END case_fault;

PROCEDURE typecase_fault (self: U) =
  (* Abort *)
  BEGIN
    self.s_empty ();
    self.child.typecase_fault ();
  END typecase_fault;

PROCEDURE check_nil (self: U) =
  (* IF (s0.A = NIL) THEN Abort *)
  BEGIN
    self.s_pop (ST.Addr);
    self.s_push (Type.Addr);
    self.child.check_nil ();
  END check_nil;

PROCEDURE check_lo (self: U;  READONLY i: Target.Int) =
  (* IF (s0.I < i) THEN Abort *)
  BEGIN
    self.s_pop (ST.Int);
    self.s_push (Type.Int);
    self.child.check_lo (i);
  END check_lo;

PROCEDURE check_hi (self: U;  READONLY i: Target.Int) =
  (* IF (i < s0.I) THEN Abort *)
  BEGIN
    self.s_pop (ST.Int);
    self.s_push (Type.Int);
    self.child.check_hi (i);
  END check_hi;

PROCEDURE check_range (self: U;  READONLY a, b: Target.Int) =
  (* IF (s0.I < a) OR (b < s0.I) THEN Abort *)
  BEGIN
    self.s_pop (ST.Int);
    self.s_push (Type.Int);
    self.child.check_range (a, b);
  END check_range;

PROCEDURE check_index (self: U) =
  BEGIN
    self.s_pop (ST.Int, ST.Int);
    self.s_push (Type.Int);
    self.child.check_index ();
  END check_index;

PROCEDURE check_eq (self: U) =
  (* IF (s0.I # s1.I) THEN Abort;  Pop (2) *)
  BEGIN
    self.s_pop (ST.Int, ST.Int);
    self.child.check_eq ();
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (self: U; i: INTEGER) =
  (* s0.A := s0.A + i *)
  BEGIN
    self.s_pop (ST.Addr);
    self.s_push (Type.Addr);
    self.child.add_offset (i);
  END add_offset;

PROCEDURE index_address (self: U;  size: INTEGER) =
  (* s1.A := s1.A + s0.I * size ; pop *)
  BEGIN
    self.s_pop (ST.Int, ST.Addr);
    self.s_push (Type.Addr);
    self.child.index_address (size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (self: U;  p: Proc;  lev: INTEGER;  t: Type) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    CheckProc (self, p);
    IF (self.clean_jumps) THEN self.s_empty () END;
    IF (self.call_count > 0) AND (NOT self.nested_calls) THEN
      PutErr (self, "nested procedure call");
    END;
    INC (self.call_count);
    self.child.start_call_direct (p, lev, t);
  END start_call_direct;

PROCEDURE start_call_indirect (self: U;  t: Type;  cc: CallingConvention) =
  (* begin a procedure call to a procedure at static level 'lev'. *)
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    IF (self.call_count > 0) AND (NOT self.nested_calls) THEN
      PutErr (self, "nested procedure call");
    END;
    INC (self.call_count);
    self.child.start_call_indirect (t, cc);
  END start_call_indirect;

PROCEDURE pop_param (self: U;  t: MType) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF (self.call_count <= 0) THEN PutErr (self, "missing start_call") END;
    self.s_pop (T_to_ST [t]);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.pop_param (t);
  END pop_param;

PROCEDURE pop_struct (self: U;  s: ByteSize;  a: Alignment) =
  (* pop s0 and make it the "next" parameter in the current call *)
  BEGIN
    IF (self.call_count <= 0) THEN PutErr (self, "missing start_call") END;
    self.s_pop (ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.pop_struct (s, a);
  END pop_struct;

PROCEDURE pop_static_link (self: U) =
  BEGIN
    IF (self.call_count <= 0) THEN PutErr (self, "missing start_call") END;
    self.s_pop (ST.Addr);
    IF (self.clean_stores) THEN self.s_empty () END;
    self.child.pop_static_link ();
  END pop_static_link;

PROCEDURE DoCall (self: U) =
  BEGIN
    IF (self.clean_jumps) THEN self.s_empty () END;
    IF (self.call_count > 0)
      THEN DEC (self.call_count);
      ELSE PutErr (self, "missing start_call");
    END;
  END DoCall;

PROCEDURE call_direct (self: U; p: Proc;  t: Type) =
  (* call the procedure identified by block b.  The procedure
     returns a value of type t. *)
  BEGIN
    CheckProc (self, p);
    DoCall (self);
    IF (t # Type.Void) THEN self.s_push (t) END;
    self.child.call_direct (p, t);
  END call_direct;

PROCEDURE call_indirect (self: U; t: Type;  cc: CallingConvention) =
  (* call the procedure whose address is in s0.A and pop s0.  The
     procedure returns a value of type t. *)
  BEGIN
    self.s_pop (ST.Addr);
    DoCall (self);
    IF (t # Type.Void) THEN self.s_push (t) END;
    self.child.call_indirect (t, cc);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (self: U;  p: Proc) =
  (* push; s0.A := ADDR (p's body) *)
  BEGIN
    CheckProc (self, p);
    self.s_push (Type.Addr);
    self.child.load_procedure (p);
  END load_procedure;

PROCEDURE load_static_link (self: U;  p: Proc) =
  (* push; s0.A := (static link needed to call p, NIL for top-level procs) *)
  BEGIN
    CheckProc (self, p);
    self.s_push (Type.Addr);
    self.child.load_static_link (p);
  END load_static_link;

BEGIN
END M3CG_Check.
