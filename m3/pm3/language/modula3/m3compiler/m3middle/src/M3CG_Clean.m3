(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May 23 15:09:22 PDT 1995 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

MODULE M3CG_Clean;

IMPORT Target, TInt, TFloat, TargetMap, M3CG, M3CG_Ops;

FROM M3CG IMPORT ByteOffset, ByteSize, Frequency, CallingConvention;
FROM M3CG IMPORT Var, Proc, Label, Sign, Alignment;
FROM M3CG IMPORT Type, ZType, AType, RType, IType, MType;

TYPE
  Op = {
    load_clean_tmp,
    set_source_file,
    set_source_line,
    free_temp,
    set_label,
    jump,
    if_true,
    if_false,
    if_eq,
    if_ne,
    if_gt,
    if_ge,
    if_lt,
    if_le,
    load,
    store,
    store_ref,
    load_address,
    load_indirect,
    store_indirect,
    store_ref_indirect,
    load_nil,
    load_integer,
    load_float,
    eq,
    ne,
    gt,
    ge,
    lt,
    le,
    add,
    subtract,
    multiply,
    divide,
    div,
    mod,
    negate,
    abs,
    max,
    min,
    round,
    trunc,
    floor,
    ceiling,
    cvt_float,
    set_union,
    set_difference,
    set_intersection,
    set_sym_difference,
    set_member,
    set_eq,
    set_ne,
    set_gt,
    set_ge,
    set_lt,
    set_le,
    set_range,
    set_singleton,
    not,
    and,
    or,
    xor,
    shift,
    shift_left,
    shift_right,
    rotate,
    rotate_left,
    rotate_right,
    extract,
    extract_n,
    extract_mn,
    insert,
    insert_n,
    insert_mn,
    swap,
    pop,
    copy_n,
    copy,
    zero_n,
    zero,
    loophole,
    check_nil,
    check_lo,
    check_hi,
    check_range,
    check_index,
    check_eq,
    add_offset,
    index_address,
    start_call_direct,
    start_call_indirect,
    pop_param,
    pop_struct,
    pop_static_link,
    pcall_direct,
    fcall_direct,
    pcall_indirect,
    fcall_indirect,
    load_procedure,
    load_static_link,
    comment
  };

CONST
  OpType = ARRAY Op OF Type {
    Type.Void, (* load_clean_tmp *)
    Type.Void, (* set_source_file *)
    Type.Void, (* set_source_line *)
    Type.Void, (* free_temp *)
    Type.Void, (* set_label *)
    Type.Void, (* jump *)
    Type.Void, (* if_true *)
    Type.Void, (* if_false *)
    Type.Void, (* if_eq *)
    Type.Void, (* if_ne *)
    Type.Void, (* if_gt *)
    Type.Void, (* if_ge *)
    Type.Void, (* if_lt *)
    Type.Void, (* if_le *)
    Type.Void, (* load *)
    Type.Void, (* store *)
    Type.Void, (* store_ref *)
    Type.Addr, (* load_address *)
    Type.Void, (* load_indirect *)
    Type.Void, (* store_indirect *)
    Type.Void, (* store_ref_indirect *)
    Type.Addr, (* load_nil *)
    Type.Int,  (* load_integer *)
    Type.Void, (* load_float *)
    Type.Int,  (* eq *)
    Type.Int,  (* ne *)
    Type.Int,  (* gt *)
    Type.Int,  (* ge *)
    Type.Int,  (* lt *)
    Type.Int,  (* le *)
    Type.Void, (* add *)
    Type.Void, (* subtract *)
    Type.Void, (* multiply *)
    Type.Void, (* divide *)
    Type.Int,  (* div *)
    Type.Int,  (* mod *)
    Type.Void, (* negate *)
    Type.Void, (* abs *)
    Type.Void, (* max *)
    Type.Void, (* min *)
    Type.Int,  (* round *)
    Type.Int,  (* trunc *)
    Type.Int,  (* floor *)
    Type.Int,  (* ceiling *)
    Type.Void, (* cvt_float *)
    Type.Void, (* set_union *)
    Type.Void, (* set_difference *)
    Type.Void, (* set_intersection *)
    Type.Void, (* set_sym_difference *)
    Type.Int,  (* set_member *)
    Type.Int,  (* set_eq *)
    Type.Int,  (* set_ne *)
    Type.Int,  (* set_gt *)
    Type.Int,  (* set_ge *)
    Type.Int,  (* set_lt *)
    Type.Int,  (* set_le *)
    Type.Void, (* set_range *)
    Type.Void, (* set_singleton *)
    Type.Int,  (* not *)
    Type.Int,  (* and *)
    Type.Int,  (* or *)
    Type.Int,  (* xor *)
    Type.Int,  (* shift *)
    Type.Int,  (* shift_left *)
    Type.Int,  (* shift_right *)
    Type.Int,  (* rotate *)
    Type.Int,  (* rotate_left *)
    Type.Int,  (* rotate_right *)
    Type.Int,  (* extract *)
    Type.Int,  (* extract_n *)
    Type.Int,  (* extract_mn *)
    Type.Int,  (* insert *)
    Type.Int,  (* insert_n *)
    Type.Int,  (* insert_mn *)
    Type.Void, (* swap *)
    Type.Void, (* pop *)
    Type.Void, (* copy_n *)
    Type.Void, (* copy *)
    Type.Void, (* zero_n *)
    Type.Void, (* zero *)
    Type.Void, (* loophole *)
    Type.Addr, (* check_nil *)
    Type.Int,  (* check_lo *)
    Type.Int,  (* check_hi *)
    Type.Int,  (* check_range *)
    Type.Int,  (* check_index *)
    Type.Void, (* check_eq *)
    Type.Addr, (* add_offset *)
    Type.Addr, (* index_address *)
    Type.Void, (* start_call_direct *)
    Type.Void, (* start_call_indirect *)
    Type.Void, (* pop_param *)
    Type.Void, (* pop_struct *)
    Type.Void, (* pop_static_link *)
    Type.Void, (* pcall_direct *)
    Type.Void, (* fcall_direct *)
    Type.Void, (* pcall_indirect *)
    Type.Void, (* fcall_indirect *)
    Type.Addr, (* load_procedure *)
    Type.Addr, (* load_static_link *)
    Type.Void  (* comment *)
  };

CONST
  Pushes = ARRAY Op OF INTEGER {
    1, (* load_clean_tmp *)
    0, (* set_source_file *)
    0, (* set_source_line *)
    0, (* free_temp *)
    0, (* set_label *)
    0, (* jump *)
   -1, (* if_true *)
   -1, (* if_false *)
   -2, (* if_eq *)
   -2, (* if_ne *)
   -2, (* if_gt *)
   -2, (* if_ge *)
   -2, (* if_lt *)
   -2, (* if_le *)
    1, (* load *)
   -1, (* store *)
   -1, (* store_ref *)
    1, (* load_address *)
    0, (* load_indirect *)
   -2, (* store_indirect *)
   -2, (* store_ref_indirect *)
    1, (* load_nil *)
    1, (* load_integer *)
    1, (* load_float *)
   -1, (* eq *)
   -1, (* ne *)
   -1, (* gt *)
   -1, (* ge *)
   -1, (* lt *)
   -1, (* le *)
   -1, (* add *)
   -1, (* subtract *)
   -1, (* multiply *)
   -1, (* divide *)
   -1, (* div *)
   -1, (* mod *)
    0, (* negate *)
    0, (* abs *)
   -1, (* max *)
   -1, (* min *)
    0, (* round *)
    0, (* trunc *)
    0, (* floor *)
    0, (* ceiling *)
    0, (* cvt_float *)
   -3, (* set_union *)
   -3, (* set_difference *)
   -3, (* set_intersection *)
   -3, (* set_sym_difference *)
   -1, (* set_member *)
   -1, (* set_eq *)
   -1, (* set_ne *)
   -1, (* set_gt *)
   -1, (* set_ge *)
   -1, (* set_lt *)
   -1, (* set_le *)
   -3, (* set_range *)
   -2, (* set_singleton *)
    0, (* not *)
   -1, (* and *)
   -1, (* or *)
   -1, (* xor *)
   -1, (* shift *)
   -1, (* shift_left *)
   -1, (* shift_right *)
   -1, (* rotate *)
   -1, (* rotate_left *)
   -1, (* rotate_right *)
   -2, (* extract *)
   -1, (* extract_n *)
    0, (* extract_mn *)
   -3, (* insert *)
   -2, (* insert_n *)
   -1, (* insert_mn *)
    0, (* swap *)
   -1, (* pop *)
   -3, (* copy_n *)
   -2, (* copy *)
   -2, (* zero_n *)
   -1, (* zero *)
    0, (* loophole *)
    0, (* check_nil *)
    0, (* check_lo *)
    0, (* check_hi *)
    0, (* check_range *)
   -1, (* check_index *)
   -2, (* check_eq *)
    0, (* add_offset *)
   -1, (* index_address *)
    0, (* start_call_direct *)
    0, (* start_call_indirect *)
   -1, (* pop_param *)
   -1, (* pop_struct *)
   -1, (* pop_static_link *)
    0, (* pcall_direct *)
    1, (* fcall_direct *)
    0, (* pcall_indirect *)
    1, (* fcall_indirect *)
    1, (* load_procedure *)
    1, (* load_static_link *)
    0  (* comment *)
  };

TYPE
  OpInfo = RECORD
    op    : Op;
    depth : INTEGER;
    result: Type;
    txt   : TEXT;
    int   : INTEGER;
    int2  : INTEGER;
    var   : Var;
    bool  : BOOLEAN;
    lab   : Label;
    type  : Type;
    type2 : Type;
    tint  : Target.Int;
    tint2 : Target.Int;
    flt   : Target.Float;
    sign1 : Sign;
    sign2 : Sign;
    proc  : Proc;
    cconv : Target.CallingConvention;
  END;

TYPE OpBuffer = REF ARRAY OF OpInfo;

TYPE
  U = M3CG.T OBJECT
        clean_jumps  : BOOLEAN  := FALSE;
        clean_stores : BOOLEAN  := FALSE;
        buffer       : OpBuffer := NIL;
        next_buf     : INTEGER  := 0;
        stack_depth  : INTEGER := 0;
      METHODS
        make_clean (depth: INTEGER) := Make_clean;
        flush_buffer () := Flush_buffer;

        stuff (op: Op;
               int : INTEGER := 0;
               type: Type    := Type.Void) := Stuff;
 
        stuffX (op: Op;
               txt : TEXT    := NIL;
               int : INTEGER := 0;
               int2: INTEGER := 0;
               var : Var     := NIL;
               bool: BOOLEAN := FALSE;
               lab : Label   := 0;
               type: Type    := Type.Void;
               type2: Type   := Type.Void;
               READONLY tint: Target.Int := TInt.Zero;
               READONLY tint2: Target.Int := TInt.Zero;
               READONLY flt: Target.Float := TFloat.ZeroR;
               sign1: Sign := Sign.Positive;
               sign2: Sign := Sign.Positive;
               proc: Proc := NIL;
               cconv: CallingConvention := NIL
              ) := StuffX;

      OVERRIDES
        end_unit   := end_unit;
        set_source_file := set_source_file;
        set_source_line := set_source_line;
        free_temp := free_temp;
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
        comment := comment;
      END;

(*--------------------------------------------------- buffer manipulation ---*)

PROCEDURE Make_clean (self: U;  depth: INTEGER) =
  VAR s: INTEGER;
  BEGIN
    s := DoClean (self, self.next_buf-1, self.stack_depth - depth);
    DoFlush (self, s, self.next_buf);
    self.next_buf := MAX (0, self.stack_depth - depth);
    self.stack_depth := self.next_buf;
  END Make_clean;

PROCEDURE DoClean (self: U;  end, hgt: INTEGER): INTEGER =
  VAR start: INTEGER;  t: Type;  tmp: Var;
  BEGIN
    (* find the tail segment [start..end] that's at least 'hgt' deep *)
    LOOP
      IF (end < 0) THEN EXIT END;
      WITH x = self.buffer[end] DO
        IF (x.depth <= hgt) AND (x.result # Type.Void) THEN EXIT END;
      END;
      DEC (end);
    END;

    IF (end < 0) THEN
      RETURN 0;

    ELSIF (hgt <= 0) THEN
      DoFlush (self, 0, end+1);

    ELSE
      start := DoClean (self, end-1, hgt-1);
      IF (start # end) OR (start # hgt-1)
        OR (self.buffer[end].op # Op.load_clean_tmp) THEN
        DoFlush (self, start, end+1);
        t := self.buffer[end].result;
        tmp := self.child.declare_temp (TargetMap.CG_Bytes[t],
                                        TargetMap.CG_Align[t],
                                        TargetMap.CG_Base[t],
                                        in_memory := FALSE);
        self.child.store (tmp, 0, t);
        WITH x = self.buffer[hgt-1] DO
          x.op     := Op.load_clean_tmp;
          x.var    := tmp;
          x.depth  := hgt;
          x.result := t;
        END;
      END;
    END;

    RETURN end + 1;
  END DoClean;

PROCEDURE Flush_buffer (self: U) =
  BEGIN
    DoFlush (self, 0, self.next_buf);
    self.next_buf := 0;
    self.stack_depth := 0;
  END Flush_buffer;

PROCEDURE DoFlush (self: U;  a, b: INTEGER) =
  VAR ch := self.child;
  BEGIN
    FOR i := a TO b-1 DO
      WITH x = self.buffer [i] DO
        CASE x.op OF
        | Op.load_clean_tmp =>
              ch.load (x.var, 0, x.type);
              ch.free_temp (x.var);
        | Op.set_source_file => ch.set_source_file (x.txt);
        | Op.set_source_line => ch.set_source_line (x.int);
        | Op.free_temp => ch.free_temp (x.var);
        | Op.set_label => ch.set_label (x.lab, x.bool);
        | Op.jump => ch.jump (x.lab);
        | Op.if_true => ch.if_true (x.lab, x.int);
        | Op.if_false => ch.if_false (x.lab, x.int);
        | Op.if_eq => ch.if_eq (x.lab, x.type, x.int);
        | Op.if_ne => ch.if_ne (x.lab, x.type, x.int);
        | Op.if_gt => ch.if_gt (x.lab, x.type, x.int);
        | Op.if_ge => ch.if_ge (x.lab, x.type, x.int);
        | Op.if_lt => ch.if_lt (x.lab, x.type, x.int);
        | Op.if_le => ch.if_le (x.lab, x.type, x.int);
        | Op.load => ch.load (x.var, x.int, x.type);
        | Op.store => ch.store (x.var, x.int, x.type);
        | Op.store_ref => ch.store_ref (x.var, x.int);
        | Op.load_address => ch.load_address (x.var, x.int);
        | Op.load_indirect => ch.load_indirect (x.int, x.type);
        | Op.store_indirect => ch.store_indirect (x.int, x.type);
        | Op.store_ref_indirect => ch.store_ref_indirect (x.int, x.bool);
        | Op.load_nil => ch.load_nil ();
        | Op.load_integer => ch.load_integer (x.tint);
        | Op.load_float => ch.load_float (x.flt);
        | Op.eq => ch.eq (x.type);
        | Op.ne => ch.ne (x.type);
        | Op.gt => ch.gt (x.type);
        | Op.ge => ch.ge (x.type);
        | Op.lt => ch.lt (x.type);
        | Op.le => ch.le (x.type);
        | Op.add => ch.add (x.type);
        | Op.subtract => ch.subtract (x.type);
        | Op.multiply => ch.multiply (x.type);
        | Op.divide => ch.divide (x.type);
        | Op.div => ch.div (x.type, x.sign1, x.sign2);
        | Op.mod => ch.mod (x.type, x.sign1, x.sign2);
        | Op.negate => ch.negate (x.type);
        | Op.abs => ch.abs (x.type);
        | Op.max => ch.max (x.type);
        | Op.min => ch.min (x.type);
        | Op.round => ch.round (x.type);
        | Op.trunc => ch.trunc (x.type);
        | Op.floor => ch.floor (x.type);
        | Op.ceiling => ch.ceiling (x.type);
        | Op.cvt_float => ch.cvt_float (x.type2, x.type);
        | Op.set_union => ch.set_union (x.int);
        | Op.set_difference => ch.set_difference (x.int);
        | Op.set_intersection => ch.set_intersection (x.int);
        | Op.set_sym_difference => ch.set_sym_difference (x.int);
        | Op.set_member => ch.set_member (x.int);
        | Op.set_eq => ch.set_eq (x.int);
        | Op.set_ne => ch.set_ne (x.int);
        | Op.set_gt => ch.set_gt (x.int);
        | Op.set_ge => ch.set_ge (x.int);
        | Op.set_lt => ch.set_lt (x.int);
        | Op.set_le => ch.set_le (x.int);
        | Op.set_range => ch.set_range (x.int);
        | Op.set_singleton => ch.set_singleton (x.int);
        | Op.not => ch.not ();
        | Op.and => ch.and ();
        | Op.or => ch.or ();
        | Op.xor => ch.xor ();
        | Op.shift => ch.shift ();
        | Op.shift_left => ch.shift_left ();
        | Op.shift_right => ch.shift_right ();
        | Op.rotate => ch.rotate ();
        | Op.rotate_left => ch.rotate_left ();
        | Op.rotate_right => ch.rotate_right ();
        | Op.extract => ch.extract (x.bool);
        | Op.extract_n => ch.extract_n (x.bool, x.int);
        | Op.extract_mn => ch.extract_mn (x.bool, x.int, x.int2);
        | Op.insert => ch.insert ();
        | Op.insert_n => ch.insert_n (x.int);
        | Op.insert_mn => ch.insert_mn (x.int, x.int2);
        | Op.swap => ch.swap (x.type, x.type2);
        | Op.pop => ch.pop (x.type);
        | Op.copy_n => ch.copy_n (x.type, x.bool);
        | Op.copy => ch.copy (x.int, x.type, x.bool);
        | Op.zero_n => ch.zero_n (x.type);
        | Op.zero => ch.zero (x.int, x.type);
        | Op.loophole => ch.loophole (x.type2, x.type);
        | Op.check_nil => ch.check_nil ();
        | Op.check_lo => ch.check_lo (x.tint);
        | Op.check_hi => ch.check_hi (x.tint);
        | Op.check_range => ch.check_range (x.tint, x.tint2);
        | Op.check_index => ch.check_index ();
        | Op.check_eq => ch.check_eq ();
        | Op.add_offset => ch.add_offset (x.int);
        | Op.index_address => ch.index_address (x.int);
        | Op.start_call_direct => ch.start_call_direct (x.proc, x.int, x.type);
        | Op.start_call_indirect => ch.start_call_indirect (x.type, x.cconv);
        | Op.pop_param => ch.pop_param (x.type);
        | Op.pop_struct => ch.pop_struct (x.int, x.int2);
        | Op.pop_static_link => ch.pop_static_link ();
        | Op.pcall_direct => ch.call_direct (x.proc, Type.Void);
        | Op.fcall_direct => ch.call_direct (x.proc, x.type);
        | Op.pcall_indirect => ch.call_indirect (Type.Void, x.cconv);
        | Op.fcall_indirect => ch.call_indirect (x.type, x.cconv);
        | Op.load_procedure => ch.load_procedure (x.proc);
        | Op.load_static_link => ch.load_static_link (x.proc);
        | Op.comment => ch.comment (x.txt);
        END;
      END;
    END;
  END DoFlush;

PROCEDURE Stuff (self: U;  op: Op;  int: INTEGER;  type: Type) =
  BEGIN
    IF (self.next_buf >= NUMBER (self.buffer^)) THEN ExpandBuffer (self) END;
    WITH x = self.buffer[self.next_buf] DO
      INC (self.stack_depth, Pushes[op]);
      x.depth  := self.stack_depth;
      x.op     := op;
      x.int    := int;
      x.type   := type;
      IF (OpType[op] = Type.Void)
        THEN x.result := type;
        ELSE x.result := OpType[op];
      END;
    END;
    IF (self.stack_depth = 0) THEN Flush_buffer (self) END;
  END Stuff;

PROCEDURE StuffX (self: U;
               op: Op;
               txt : TEXT    := NIL;
               int : INTEGER := 0;
               int2: INTEGER := 0;
               var : Var     := NIL;
               bool: BOOLEAN := FALSE;
               lab : Label   := 0;
               type: Type    := Type.Void;
               type2: Type   := Type.Void;
               READONLY tint: Target.Int := TInt.Zero;
               READONLY tint2: Target.Int := TInt.Zero;
               READONLY flt: Target.Float := TFloat.ZeroR;
               sign1: Sign := Sign.Positive;
               sign2: Sign := Sign.Positive;
               proc: Proc := NIL;
               cconv: CallingConvention := NIL
              ) =
  BEGIN
    IF (self.next_buf >= NUMBER (self.buffer^)) THEN ExpandBuffer (self) END;
    WITH x = self.buffer[self.next_buf] DO
      INC (self.stack_depth, Pushes[op]);
      x.depth  := self.stack_depth;
      x.op     := op;
      x.txt    := txt;
      x.int    := int;
      x.int2   := int2;
      x.var    := var;
      x.bool   := bool;
      x.lab    := lab;
      x.type   := type;
      x.type2  := type2;
      x.tint   := tint;
      x.tint2  := tint2;
      x.flt    := flt;
      x.sign1  := sign1;
      x.sign2  := sign2;
      x.proc   := proc;
      x.cconv  := cconv;
      IF (OpType[op] = Type.Void)
        THEN x.result := type;
        ELSE x.result := OpType[op];
      END;
    END;
    IF (self.stack_depth = 0) THEN Flush_buffer (self) END;
  END StuffX;

PROCEDURE ExpandBuffer (self: U) =
  VAR n := NUMBER (self.buffer^);  new := NEW (OpBuffer, 2 * n);
  BEGIN
    SUBARRAY (new^, 0, n) := self.buffer^;
    self.buffer := new;
  END ExpandBuffer;

(*---------------------------------------------------------------------------*)

PROCEDURE New (child: M3CG.T;  jumps, stores: BOOLEAN): M3CG.T =
  BEGIN
    RETURN NEW (U,
                child  := child,
                buffer := NEW (OpBuffer, 100),
                clean_jumps  := jumps,
                clean_stores := stores
               );
  END New;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE end_unit (self: U) =
  BEGIN
    self.flush_buffer ();
    self.child.end_unit ();
  END end_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (self: U; file: TEXT) =
  BEGIN
    self.stuffX (Op.set_source_file, txt := file);
  END set_source_file;

PROCEDURE set_source_line (self: U; line: INTEGER) =
  BEGIN
    self.stuff (Op.set_source_line, int := line);
  END set_source_line;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE free_temp (self: U;  v: Var) =
  BEGIN
    self.stuffX (Op.free_temp, var := v);
  END free_temp;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE end_procedure (self: U;  p: Proc) =
  BEGIN
    self.flush_buffer ();
    self.child.end_procedure (p);
  END end_procedure;

PROCEDURE begin_block (self: U) =
  BEGIN
    self.make_clean (0);
    self.child.begin_block ();
  END begin_block;

PROCEDURE end_block (self: U) =
  BEGIN
    self.make_clean (0);
    self.child.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (self: U;  p: Proc) =
  BEGIN
    self.make_clean (0);
    self.child.note_procedure_origin (p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (self: U;  l: Label;  barrier: BOOLEAN) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (0);
      self.child.set_label (l, barrier);
    ELSE
      self.stuffX (Op.set_label, lab := l, bool := barrier);
    END;
  END set_label;

PROCEDURE jump (self: U; l: Label) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (0);
      self.child.jump (l);
    ELSE
      self.stuffX (Op.jump, lab := l);
    END;
  END jump;

PROCEDURE if_true  (self: U; l: Label;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (1);
      self.child.if_true (l, f);
    ELSE
      self.stuffX (Op.if_true, lab := l, int := f);
    END;
  END if_true;

PROCEDURE if_false (self: U; l: Label;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (1);
      self.child.if_false (l, f);
    ELSE
      self.stuffX (Op.if_false, lab := l, int := f);
    END;
  END if_false;

PROCEDURE if_eq (self: U;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (2);
      self.child.if_eq (l, t, f);
    ELSE
      self.stuffX (Op.if_eq, lab := l, type := t, int := f);
    END;
  END if_eq;

PROCEDURE if_ne (self: U;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (2);
      self.child.if_ne (l, t, f);
    ELSE
      self.stuffX (Op.if_ne, lab := l, type := t, int := f);
    END;
  END if_ne;

PROCEDURE if_gt (self: U;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (2);
      self.child.if_gt (l, t, f);
    ELSE
      self.stuffX (Op.if_gt, lab := l, type := t, int := f);
    END;
  END if_gt;

PROCEDURE if_ge (self: U;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (2);
      self.child.if_ge (l, t, f);
    ELSE
      self.stuffX (Op.if_ge, lab := l, type := t, int := f);
    END;
  END if_ge;

PROCEDURE if_lt (self: U;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (2);
      self.child.if_lt (l, t, f);
    ELSE
      self.stuffX (Op.if_lt, lab := l, type := t, int := f);
    END;
  END if_lt;

PROCEDURE if_le (self: U;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (2);
      self.child.if_le (l, t, f);
    ELSE
      self.stuffX (Op.if_le, lab := l, type := t, int := f);
    END;
  END if_le;

PROCEDURE case_jump (self: U; READONLY labels: ARRAY OF Label) =
  BEGIN
    self.flush_buffer ();
    self.child.case_jump (labels);
  END case_jump;

PROCEDURE exit_proc (self: U; t: Type) =
  BEGIN
    self.flush_buffer ();
    self.child.exit_proc (t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load  (self: U;  v: Var;  o: ByteOffset;  t: MType) =
  BEGIN
    self.stuffX (Op.load, var := v, int := o, type := t);
  END load;

PROCEDURE store  (self: U;  v: Var;  o: ByteOffset;  t: MType) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (1);
      self.child.store (v, o, t);
    ELSE
      self.stuffX (Op.store, var := v, int := o, type := t);
    END;
  END store;

PROCEDURE store_ref (self: U;  v: Var;  o: ByteOffset) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (1);
      self.child.store_ref (v, o);
    ELSE
      self.stuffX (Op.store_ref, var := v, int := o);
    END;
  END store_ref;

PROCEDURE load_address (self: U;  v: Var;  o: ByteOffset) =
  BEGIN
    self.stuffX (Op.load_address, var := v, int := o);
  END load_address;

PROCEDURE load_indirect (self: U;  o: ByteOffset;  t: MType) =
  BEGIN
    self.stuff (Op.load_indirect, int := o, type := t);
  END load_indirect;

PROCEDURE store_indirect (self: U;  o: ByteOffset;  t: MType) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (2);
      self.child.store_indirect (o, t);
    ELSE
      self.stuff (Op.store_indirect, int := o, type := t);
    END;
  END store_indirect;

PROCEDURE store_ref_indirect (self: U;  o: ByteOffset;  var: BOOLEAN) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (2);
      self.child.store_ref_indirect (o, var);
    ELSE
      self.stuffX (Op.store_ref_indirect, int := o, bool := var);
    END;
  END store_ref_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (self: U) =
  BEGIN
    self.stuff (Op.load_nil);
  END load_nil;

PROCEDURE load_integer (self: U;  READONLY i: Target.Int) =
  BEGIN
    self.stuffX (Op.load_integer, tint := i);
  END load_integer;

PROCEDURE load_float (self: U;  READONLY f: Target.Float) =
  CONST FType = ARRAY Target.Precision OF Type
                { Type.Reel, Type.LReel, Type.XReel };
  BEGIN
    self.stuffX (Op.load_float, flt := f, type := FType [f.pre]);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE eq (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.eq, type := t);
  END eq;

PROCEDURE ne (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.ne, type := t);
  END ne;

PROCEDURE gt (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.gt, type := t);
  END gt;

PROCEDURE ge (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.ge, type := t);
  END ge;

PROCEDURE lt (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.lt, type := t);
  END lt;

PROCEDURE le (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.le, type := t);
  END le;

PROCEDURE add (self: U;  t: AType) =
  BEGIN
    self.stuff (Op.add, type := t);
  END add;

PROCEDURE subtract (self: U;  t: AType) =
  BEGIN
    self.stuff (Op.subtract, type := t);
  END subtract;

PROCEDURE multiply (self: U;  t: AType) =
  BEGIN
    self.stuff (Op.multiply, type := t);
  END multiply;

PROCEDURE divide (self: U;  t: RType) =
  BEGIN
    self.stuff (Op.divide, type := t);
  END divide;

PROCEDURE div (self: U;  t: IType;  a, b: Sign) =
  BEGIN
    self.stuffX (Op.div, type := t, sign1 := a, sign2 := b);
  END div;

PROCEDURE mod (self: U;  t: IType;  a, b: Sign) =
  BEGIN
    self.stuffX (Op.mod, type := t, sign1 := a, sign2 := b);
  END mod;

PROCEDURE negate (self: U;  t: AType) =
  BEGIN
    self.stuff (Op.negate, type := t);
  END negate;

PROCEDURE abs (self: U;  t: AType) =
  BEGIN
    self.stuff (Op.abs, type := t);
  END abs;

PROCEDURE max (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.max, type := t);
  END max;

PROCEDURE min (self: U;  t: ZType) =
  BEGIN
    self.stuff (Op.min, type := t);
  END min;

PROCEDURE round (self: U;  t: RType) =
  BEGIN
    self.stuff (Op.round, type := t);
  END round;

PROCEDURE trunc (self: U;  t: RType) =
  BEGIN
    self.stuff (Op.trunc, type := t);
  END trunc;

PROCEDURE floor (self: U;  t: RType) =
  BEGIN
    self.stuff (Op.floor, type := t);
  END floor;

PROCEDURE ceiling  (self: U;  t: RType) =
  BEGIN
    self.stuff (Op.ceiling, type := t);
  END ceiling;

PROCEDURE cvt_float (self: U;  t: AType;  u: RType) =
  BEGIN
    self.stuffX (Op.cvt_float, type := u,  type2 := t);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_union, int := s);
  END set_union;

PROCEDURE set_difference (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_difference, int := s);
  END set_difference;

PROCEDURE set_intersection (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_intersection, int := s);
  END set_intersection;

PROCEDURE set_sym_difference (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_sym_difference, int := s);
  END set_sym_difference;

PROCEDURE set_member (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_member, int := s);
  END set_member;

PROCEDURE set_eq (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_eq, int := s);
  END set_eq;

PROCEDURE set_ne (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_ne, int := s);
  END set_ne;

PROCEDURE set_gt (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_gt, int := s);
  END set_gt;

PROCEDURE set_ge (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_ge, int := s);
  END set_ge;

PROCEDURE set_lt (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_lt, int := s);
  END set_lt;

PROCEDURE set_le (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_le, int := s);
  END set_le;

PROCEDURE set_range (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_range, int := s);
  END set_range;

PROCEDURE set_singleton (self: U;  s: ByteSize) =
  BEGIN
    self.stuff (Op.set_singleton, int := s);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (self: U) =
  BEGIN
    self.stuff (Op.not);
  END not;

PROCEDURE and (self: U) =
  BEGIN
    self.stuff (Op.and);
  END and;

PROCEDURE or (self: U) =
  BEGIN
    self.stuff (Op.or);
  END or;

PROCEDURE xor (self: U) =
  BEGIN
    self.stuff (Op.xor);
  END xor;

PROCEDURE shift (self: U) =
  BEGIN
    self.stuff (Op.shift);
  END shift;

PROCEDURE shift_left (self: U) =
  BEGIN
    self.stuff (Op.shift_left);
  END shift_left;

PROCEDURE shift_right  (self: U) =
  BEGIN
    self.stuff (Op.shift_right);
  END shift_right;

PROCEDURE rotate (self: U) =
  BEGIN
    self.stuff (Op.rotate);
  END rotate;

PROCEDURE rotate_left  (self: U) =
  BEGIN
    self.stuff (Op.rotate_left);
  END rotate_left;

PROCEDURE rotate_right (self: U) =
  BEGIN
    self.stuff (Op.rotate_right);
  END rotate_right;

PROCEDURE extract (self: U;  sign: BOOLEAN) =
  BEGIN
    self.stuffX (Op.extract, bool := sign);
  END extract;

PROCEDURE extract_n (self: U;  sign: BOOLEAN;  n: INTEGER) =
  BEGIN
    self.stuffX (Op.extract_n, bool := sign,  int := n);
  END extract_n;

PROCEDURE extract_mn (self: U;  sign: BOOLEAN;  m, n: INTEGER) =
  BEGIN
    self.stuffX (Op.extract_mn, bool := sign, int := m, int2 := n);
  END extract_mn;

PROCEDURE insert  (self: U) =
  BEGIN
    self.stuff (Op.insert);
  END insert;

PROCEDURE insert_n  (self: U;  n: INTEGER) =
  BEGIN
    self.stuff (Op.insert_n, int := n);
  END insert_n;

PROCEDURE insert_mn  (self: U;  m, n: INTEGER) =
  BEGIN
    self.stuffX (Op.insert_mn, int := m, int2 := n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (self: U;  a, b: Type) =
  BEGIN
    self.stuffX (Op.swap, type := a, type2 := b);
  END swap;

PROCEDURE pop  (self: U;  t: Type) =
  BEGIN
    self.stuff (Op.pop, type := t);
  END pop;

PROCEDURE copy_n (self: U;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    self.stuffX (Op.copy_n, type := t, bool := overlap);
  END copy_n;

PROCEDURE copy (self: U;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    self.stuffX (Op.copy, int := n, type := t, bool := overlap);
  END copy;

PROCEDURE zero_n (self: U;  t: MType) =
  BEGIN
    self.stuff (Op.zero_n, type := t);
  END zero_n;

PROCEDURE zero (self: U;  n: INTEGER;  t: MType) =
  BEGIN
    self.stuff (Op.zero, int := n, type := t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (self: U;  from, two: ZType) =
  BEGIN
    self.stuffX (Op.loophole, type := two, type2 := from);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE assert_fault (self: U) =
  BEGIN
    self.flush_buffer ();
    self.child.assert_fault ();
  END assert_fault;

PROCEDURE narrow_fault (self: U) =
  BEGIN
    self.flush_buffer ();
    self.child.narrow_fault ();
  END narrow_fault;

PROCEDURE return_fault (self: U) =
  BEGIN
    self.flush_buffer ();
    self.child.return_fault ();
  END return_fault;

PROCEDURE case_fault (self: U) =
  BEGIN
    self.flush_buffer ();
    self.child.case_fault ();
  END case_fault;

PROCEDURE typecase_fault (self: U) =
  (* Abort *)
  BEGIN
    self.flush_buffer ();
    self.child.typecase_fault ();
  END typecase_fault;

PROCEDURE check_nil (self: U) =
  BEGIN
    self.stuff (Op.check_nil);
  END check_nil;

PROCEDURE check_lo (self: U;  READONLY i: Target.Int) =
  BEGIN
    self.stuffX (Op.check_lo, tint := i);
  END check_lo;

PROCEDURE check_hi (self: U;  READONLY i: Target.Int) =
  BEGIN
    self.stuffX (Op.check_hi, tint := i);
  END check_hi;

PROCEDURE check_range (self: U;  READONLY a, b: Target.Int) =
  BEGIN
    self.stuffX (Op.check_range, tint := a, tint2 := b);
  END check_range;

PROCEDURE check_index (self: U) =
  BEGIN
    self.stuff (Op.check_index);
  END check_index;

PROCEDURE check_eq (self: U) =
  BEGIN
    self.stuff (Op.check_eq);
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (self: U; i: INTEGER) =
  BEGIN
    self.stuff (Op.add_offset, int := i);
  END add_offset;

PROCEDURE index_address (self: U;  size: INTEGER) =
  BEGIN
    self.stuff (Op.index_address, int := size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (self: U;  p: Proc;  lev: INTEGER;  t: Type) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (0);
      self.child.start_call_direct (p, lev, t);
    ELSE
      self.stuffX (Op.start_call_direct, proc := p, int := lev, type := t);
    END;
  END start_call_direct;

PROCEDURE start_call_indirect (self: U;  t: Type;  cc: CallingConvention) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (0);
      self.child.start_call_indirect (t, cc);
    ELSE
      self.stuffX (Op.start_call_indirect, type := t, cconv := cc);
    END;
  END start_call_indirect;

PROCEDURE pop_param (self: U;  t: MType) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (1);
      self.child.pop_param (t);
    ELSE
      self.stuff (Op.pop_param, type := t);
    END;
  END pop_param;

PROCEDURE pop_struct (self: U;  s: ByteSize;  a: Alignment) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (1);
      self.child.pop_struct (s, a);
    ELSE
      self.stuffX (Op.pop_struct, int := s, int2 := a);
    END;
  END pop_struct;

PROCEDURE pop_static_link (self: U) =
  BEGIN
    IF (self.clean_stores) THEN
      self.make_clean (1);
      self.child.pop_static_link ();
    ELSE
      self.stuff (Op.pop_static_link);
    END;
  END pop_static_link;

PROCEDURE call_direct (self: U; p: Proc;  t: Type) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (0);
      self.child.call_direct (p, t);
    ELSIF (t = Type.Void) THEN
      self.stuffX (Op.pcall_direct, proc := p);
    ELSE
      self.stuffX (Op.fcall_direct, proc := p, type := t);
    END;
  END call_direct;

PROCEDURE call_indirect (self: U; t: Type;  cc: CallingConvention) =
  BEGIN
    IF (self.clean_jumps) THEN
      self.make_clean (0);
      self.child.call_indirect (t, cc);
    ELSIF (t = Type.Void) THEN
      self.stuffX (Op.pcall_indirect, cconv := cc);
    ELSE
      self.stuffX (Op.fcall_indirect, type := t, cconv := cc);
    END;
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (self: U;  p: Proc) =
  BEGIN
    self.stuffX (Op.load_procedure,  proc := p);
  END load_procedure;

PROCEDURE load_static_link (self: U;  p: Proc) =
  BEGIN
    self.stuffX (Op.load_static_link,  proc := p);
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (self: U;  a, b, c, d: TEXT := NIL) =
  VAR x: TEXT := "";
  BEGIN
    IF (a # NIL) THEN x := x & a END;
    IF (b # NIL) THEN x := x & b END;
    IF (c # NIL) THEN x := x & c END;
    IF (d # NIL) THEN x := x & d END;
    self.stuffX (Op.comment, txt := x);
  END comment;

BEGIN
END M3CG_Clean.
