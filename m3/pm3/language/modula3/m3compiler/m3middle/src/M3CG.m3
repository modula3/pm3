(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 20 16:02:46 PDT 1995 by kalsow     *)
(*      modified on Tue May 25 14:13:23 PDT 1993 by muller     *)

MODULE M3CG EXPORTS M3CG, M3CG_Ops;

IMPORT Target;

REVEAL
  T = Public BRANDED "M3CG.T" OBJECT OVERRIDES
    next_label := next_label;
    set_error_handler := set_error_handler;
    begin_unit := begin_unit;
    end_unit := end_unit;
    import_unit := import_unit;
    export_unit := export_unit;
    set_source_file := set_source_file;
    set_source_line := set_source_line;
    declare_typename := declare_typename;
    declare_array := declare_array;
    declare_open_array := declare_open_array;
    declare_enum := declare_enum;
    declare_enum_elt := declare_enum_elt;
    declare_packed := declare_packed;
    declare_record := declare_record;
    declare_field := declare_field;
    declare_set := declare_set;
    declare_subrange := declare_subrange;
    declare_pointer := declare_pointer;
    declare_indirect := declare_indirect;
    declare_proctype := declare_proctype;
    declare_formal := declare_formal;
    declare_raises := declare_raises;
    declare_object := declare_object;
    declare_method := declare_method;
    declare_opaque := declare_opaque;
    reveal_opaque := reveal_opaque;
    declare_exception := declare_exception;
    set_runtime_proc := set_runtime_proc;
    set_runtime_hook := set_runtime_hook;
    get_runtime_hook := get_runtime_hook;
    import_global := import_global;
    declare_segment := declare_segment;
    bind_segment := bind_segment;
    declare_global := declare_global;
    declare_constant := declare_constant;
    declare_local := declare_local;
    declare_param := declare_param;
    declare_temp :=declare_temp;
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
    import_procedure := import_procedure;
    declare_procedure := declare_procedure;
    begin_procedure := begin_procedure;
    end_procedure := end_procedure;
    begin_block := begin_block;
    end_block := end_block;
    note_procedure_origin := note_procedure_origin;
    set_label := set_label;
    jump := jump;
    if_true := if_true;
    if_false := if_false;
    if_eq := if_eq; 
    if_ne := if_ne; 
    if_gt := if_gt; 
    if_ge := if_ge; 
    if_lt := if_lt; 
    if_le := if_le; 
    case_jump := case_jump;
    exit_proc := exit_proc;
    load := load;
    load_address := load_address;
    load_indirect := load_indirect;
    store := store;
    store_indirect := store_indirect;
    store_ref := store_ref;
    store_ref_indirect := store_ref_indirect;
    load_nil := load_nil;                           
    load_integer := load_integer; 
    load_float := load_float; 
    eq := eq;        
    ne := ne;        
    gt := gt;        
    ge := ge;        
    lt := lt;        
    le := le;        
    add := add;       
    subtract := subtract;  
    multiply := multiply;  
    divide := divide;    
    negate := negate;    
    abs := abs;       
    max := max;       
    min := min;       
    round := round;     
    trunc := trunc;     
    floor := floor;     
    ceiling := ceiling;   
    cvt_float := cvt_float; 
    div := div;     
    mod := mod;     
    set_union := set_union;
    set_difference := set_difference;
    set_intersection := set_intersection;
    set_sym_difference := set_sym_difference;
    set_member := set_member;
    set_eq := set_eq;  
    set_ne := set_ne;  
    set_lt := set_lt;  
    set_le := set_le;  
    set_gt := set_gt;  
    set_ge := set_ge;  
    set_range := set_range;
    set_singleton := set_singleton;
    not := not;  
    and := and;  
    or := or;   
    xor := xor;  
    shift := shift;         
    shift_left := shift_left;    
    shift_right := shift_right;   
    rotate := rotate;        
    rotate_left := rotate_left;   
    rotate_right := rotate_right;  
    extract := extract;
    extract_n := extract_n;
    extract_mn := extract_mn;
    insert := insert;
    insert_n := insert_n;
    insert_mn := insert_mn;
    swap := swap;
    pop := pop;
    copy_n := copy_n;
    copy := copy;
    zero_n := zero_n;
    zero := zero;
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

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE next_label (xx: T;  n: INTEGER := 1): Label =
  BEGIN
    RETURN xx.child.next_label (n);
  END next_label;

(*------------------------------------------------ READONLY configuration ---*)

PROCEDURE set_error_handler (xx: T;  p: PROCEDURE (msg: TEXT)) =
  BEGIN
    xx.child.set_error_handler (p);
  END set_error_handler;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE begin_unit (xx: T;  optimize : INTEGER) =
  BEGIN
    xx.child.begin_unit (optimize);
  END begin_unit;

PROCEDURE end_unit (xx: T) =
  BEGIN
    xx.child.end_unit ();
  END end_unit;

PROCEDURE import_unit (xx: T;  n: Name) =
  BEGIN
    xx.child.import_unit (n);
  END import_unit;

PROCEDURE export_unit (xx: T;  n: Name) =
  BEGIN
    xx.child.export_unit (n);
  END export_unit;

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE set_source_file (xx: T;  file: TEXT) =
  BEGIN
    xx.child.set_source_file (file);
  END set_source_file;

PROCEDURE set_source_line (xx: T; line: INTEGER) =
  BEGIN
    xx.child.set_source_line (line);
  END set_source_line;

(*------------------------------------------- debugging type declarations ---*)

PROCEDURE declare_typename (xx: T;  t: TypeUID;  n: Name) =
  BEGIN
    xx.child.declare_typename (t, n);
  END declare_typename;

PROCEDURE declare_array (xx: T;  t, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    xx.child.declare_array (t, index, elt, s);
  END declare_array;

PROCEDURE declare_open_array (xx: T;  t, elt: TypeUID;  s: BitSize) =
  BEGIN
    xx.child.declare_open_array (t, elt, s);
  END declare_open_array;

PROCEDURE declare_enum (xx: T;  t: TypeUID;  n_elts: INTEGER;  s: BitSize) =
  BEGIN
    xx.child.declare_enum (t, n_elts, s);
  END declare_enum;

PROCEDURE declare_enum_elt (xx: T;  n: Name) =
  BEGIN
    xx.child.declare_enum_elt (n);
  END declare_enum_elt;

PROCEDURE declare_packed (xx: T;  t: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    xx.child.declare_packed (t, s, base);
  END declare_packed;

PROCEDURE declare_record (xx: T; t: TypeUID;  s: BitSize;
                          n_fields: INTEGER)=
  BEGIN
    xx.child.declare_record (t, s, n_fields);
  END declare_record;

PROCEDURE declare_field (xx: T;  n: Name;  o: BitOffset;  s: BitSize;
                         t: TypeUID)=
  BEGIN
    xx.child.declare_field (n, o, s, t);
  END declare_field;

PROCEDURE declare_set (xx: T;  t, domain: TypeUID;  s: BitSize) =
  BEGIN
    xx.child.declare_set (t, domain, s);
  END declare_set;

PROCEDURE declare_subrange (xx: T; t, domain: TypeUID;
                            READONLY min, max: Target.Int;
                            s: BitSize) =
  BEGIN
    xx.child.declare_subrange (t, domain, min, max, s);
  END declare_subrange;


PROCEDURE declare_pointer (xx: T;  t, target: TypeUID;  brand: TEXT;
                           traced: BOOLEAN) =
  BEGIN
    xx.child.declare_pointer (t, target, brand, traced);
  END declare_pointer;


PROCEDURE declare_indirect (xx: T;  t, target: TypeUID) =
  BEGIN
    xx.child.declare_indirect (t, target);
  END declare_indirect;


PROCEDURE declare_proctype (xx: T; t: TypeUID; n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention) =
  BEGIN
    xx.child.declare_proctype (t, n_formals, result, n_raises, cc);
  END declare_proctype;

PROCEDURE declare_formal (xx: T;  n: Name;  t: TypeUID) =
  BEGIN
    xx.child.declare_formal (n, t);
  END declare_formal;

PROCEDURE declare_raises (xx: T;  n: Name) =
  BEGIN
    xx.child.declare_raises (n);
  END declare_raises;


PROCEDURE declare_object (xx: T; t, super: TypeUID;
                          brand: TEXT;  traced: BOOLEAN;
                          n_fields, n_methods: INTEGER;
                          field_size: BitSize) =
  BEGIN
    xx.child.declare_object (t, super, brand, traced,
                             n_fields, n_methods, field_size);
  END declare_object;

PROCEDURE declare_method (xx: T;  n: Name;  signature: TypeUID) =
  BEGIN
    xx.child.declare_method (n, signature);
  END declare_method;

PROCEDURE declare_opaque (xx: T;  t, super: TypeUID) =
  BEGIN
    xx.child.declare_opaque (t, super);
  END declare_opaque;

PROCEDURE reveal_opaque (xx: T;  lhs, rhs: TypeUID) =
  BEGIN
    xx.child.reveal_opaque (lhs, rhs);
  END reveal_opaque;

PROCEDURE declare_exception (xx: T;  n: Name;  arg_type: TypeUID;
                           raise_proc: BOOLEAN;  base: Var;  offset: INTEGER) =
  BEGIN
    xx.child.declare_exception (n, arg_type, raise_proc, base, offset);
  END declare_exception;

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE set_runtime_proc (xx: T;  n: Name;  p: Proc) =
  BEGIN
    xx.child.set_runtime_proc (n, p);
  END set_runtime_proc;

PROCEDURE set_runtime_hook (xx: T;  n: Name;  v: Var;  o: ByteOffset) =
  BEGIN
    xx.child.set_runtime_hook (n, v, o);
  END set_runtime_hook;

PROCEDURE get_runtime_hook (xx: T;  n: Name; VAR p: Proc;  VAR v: Var; VAR o: ByteOffset) =
  BEGIN
    xx.child.get_runtime_hook (n, p, v, o);
  END get_runtime_hook;

(*------------------------------------------------- variable declarations ---*)

PROCEDURE import_global (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID): Var =
  BEGIN
    RETURN xx.child.import_global (n, s, a, t, m3t);
  END import_global;

PROCEDURE declare_segment (xx: T;  n: Name;  m3t: TypeUID): Var =
  BEGIN
    RETURN xx.child.declare_segment (n, m3t);
  END declare_segment;

PROCEDURE bind_segment (xx: T;  seg: Var;  s: ByteSize;  a: Alignment;
                        t: Type;  exported, inited: BOOLEAN) =
  BEGIN
    xx.child.bind_segment (seg, s, a, t, exported, inited);
  END bind_segment;

PROCEDURE declare_global (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN xx.child.declare_global (n, s, a, t, m3t, exported, inited);
  END declare_global;

PROCEDURE declare_constant (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                     t: Type;  m3t: TypeUID;  exported, inited: BOOLEAN): Var =
  BEGIN
    RETURN xx.child.declare_constant (n, s, a, t, m3t, exported, inited);
  END declare_constant;

PROCEDURE declare_local (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN xx.child.declare_local (n, s, a, t, m3t, in_memory, up_level, f);
  END declare_local;

PROCEDURE declare_param (xx: T;  n: Name;  s: ByteSize;  a: Alignment;
                         t: Type;  m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var =
  BEGIN
    RETURN xx.child.declare_param (n, s, a, t, m3t, in_memory, up_level, f);
  END declare_param;

PROCEDURE declare_temp (xx: T;  s: ByteSize;  a: Alignment;  t: Type;
                        in_memory:BOOLEAN): Var =
  BEGIN
    RETURN xx.child.declare_temp (s, a, t, in_memory);
  END declare_temp;

PROCEDURE free_temp (xx: T;  v: Var) =
  BEGIN
    xx.child.free_temp (v);
  END free_temp;

(*---------------------------------------- static variable initialization ---*)

PROCEDURE begin_init (xx: T;  v: Var) =
  BEGIN
    xx.child.begin_init (v);
  END begin_init;

PROCEDURE end_init (xx: T;  v: Var) =
  BEGIN
    xx.child.end_init (v);
  END end_init;

PROCEDURE init_int (xx: T;  o: ByteOffset;  READONLY value: Target.Int;
                    t: Type) =
  BEGIN
    xx.child.init_int (o, value, t);
  END init_int;

PROCEDURE init_proc (xx: T;  o: ByteOffset;  value: Proc) =
  BEGIN
    xx.child.init_proc (o, value);
  END init_proc;

PROCEDURE init_label (xx: T;  o: ByteOffset;  value: Label) =
  BEGIN
    xx.child.init_label (o, value);
  END init_label;

PROCEDURE init_var (xx: T;  o: ByteOffset;  value: Var;  bias: ByteOffset) =
  BEGIN
    xx.child.init_var (o, value, bias);
  END init_var;

PROCEDURE init_offset (xx: T;  o: ByteOffset;  value: Var) =
  BEGIN
    xx.child.init_offset (o, value);
  END init_offset;

PROCEDURE init_chars (xx: T;  o: ByteOffset;  value: TEXT) =
  BEGIN
    xx.child.init_chars (o, value);
  END init_chars;

PROCEDURE init_float (xx: T;  o: ByteOffset;  READONLY f: Target.Float) =
  BEGIN
    xx.child.init_float (o, f);
  END init_float;

(*------------------------------------------------------------ procedures ---*)

PROCEDURE import_procedure (xx: T;  n: Name;  n_params: INTEGER;
                          ret_type: Type;  cc: CallingConvention): Proc =
  BEGIN
    RETURN xx.child.import_procedure (n, n_params, ret_type, cc);
  END import_procedure;

PROCEDURE declare_procedure (xx: T;  n: Name;  n_params: INTEGER;
                             return_type: Type;  lev: INTEGER;
                             cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc =
  BEGIN
    RETURN xx.child.declare_procedure (n, n_params, return_type,
                                       lev, cc, exported, parent);
  END declare_procedure;

PROCEDURE begin_procedure (xx: T;  p: Proc) =
  BEGIN
    xx.child.begin_procedure (p);
  END begin_procedure;

PROCEDURE end_procedure (xx: T;  p: Proc) =
  BEGIN
    xx.child.end_procedure (p);
  END end_procedure;

PROCEDURE begin_block (xx: T) =
  BEGIN
    xx.child.begin_block ();
  END begin_block;

PROCEDURE end_block (xx: T) =
  BEGIN
    xx.child.end_block ();
  END end_block;

PROCEDURE note_procedure_origin (xx: T;  p: Proc) =
  BEGIN
    xx.child.note_procedure_origin (p);
  END note_procedure_origin;

(*------------------------------------------------------------ statements ---*)

PROCEDURE set_label (xx: T;  l: Label;  barrier: BOOLEAN) =
  BEGIN
    xx.child.set_label (l, barrier);
  END set_label;

PROCEDURE jump (xx: T; l: Label) =
  BEGIN
    xx.child.jump (l);
  END jump;

PROCEDURE if_true (xx: T; l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_true (l, f);
  END if_true;

PROCEDURE if_false (xx: T; l: Label;  f: Frequency) =
  BEGIN
    xx.child.if_false (l, f);
  END if_false;

PROCEDURE if_eq (xx: T;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    xx.child.if_eq (l, t, f);
  END if_eq;

PROCEDURE if_ne (xx: T;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    xx.child.if_ne (l, t, f);
  END if_ne;

PROCEDURE if_gt (xx: T;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    xx.child.if_gt (l, t, f);
  END if_gt;

PROCEDURE if_ge (xx: T;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    xx.child.if_ge (l, t, f);
  END if_ge;

PROCEDURE if_lt (xx: T;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    xx.child.if_lt (l, t, f);
  END if_lt;

PROCEDURE if_le (xx: T;  l: Label;  t: ZType;  f: Frequency) =
  BEGIN
    xx.child.if_le (l, t, f);
  END if_le;

PROCEDURE case_jump (xx: T; READONLY labels: ARRAY OF Label) =
  BEGIN
    xx.child.case_jump (labels);
  END case_jump;

PROCEDURE exit_proc (xx: T;  t: Type) =
  BEGIN
    xx.child.exit_proc (t);
  END exit_proc;

(*------------------------------------------------------------ load/store ---*)

PROCEDURE load (xx: T;  v: Var;  o: ByteOffset;  t: MType) =
  BEGIN
    xx.child.load (v, o, t);
  END load;

PROCEDURE store (xx: T;  v: Var;  o: ByteOffset;  t: MType) =
  BEGIN
    xx.child.store (v, o, t);
  END store;

PROCEDURE store_ref (xx: T;  v: Var;  o: ByteOffset) =
  BEGIN
    xx.child.store_ref (v, o);
  END store_ref;

PROCEDURE load_address (xx: T;  v: Var;  o: ByteOffset) =
  BEGIN
    xx.child.load_address (v, o);
  END load_address;

PROCEDURE load_indirect (xx: T;  o: ByteOffset;  t: MType) =
  BEGIN
    xx.child.load_indirect (o, t);
  END load_indirect;

PROCEDURE store_indirect (xx: T;  o: ByteOffset;  t: MType) =
  BEGIN
    xx.child.store_indirect (o, t);
  END store_indirect;

PROCEDURE store_ref_indirect (xx: T;  o: ByteOffset;  var: BOOLEAN) =
  BEGIN
    xx.child.store_ref_indirect (o, var);
  END store_ref_indirect;

(*-------------------------------------------------------------- literals ---*)

PROCEDURE load_nil (xx: T) =
  BEGIN
    xx.child.load_nil ();
  END load_nil;

PROCEDURE load_integer (xx: T;  READONLY i: Target.Int) =
  BEGIN
    xx.child.load_integer (i);
  END load_integer;

PROCEDURE load_float (xx: T;  READONLY f: Target.Float) =
  BEGIN
    xx.child.load_float (f);
  END load_float;

(*------------------------------------------------------------ arithmetic ---*)

PROCEDURE eq (xx: T;  t: ZType) =
  BEGIN
    xx.child.eq (t);
  END eq;

PROCEDURE ne (xx: T;  t: ZType) =
  BEGIN
    xx.child.ne (t);
  END ne;

PROCEDURE gt (xx: T;  t: ZType) =
  BEGIN
    xx.child.gt (t);
  END gt;

PROCEDURE ge (xx: T;  t: ZType) =
  BEGIN
    xx.child.ge (t);
  END ge;

PROCEDURE lt (xx: T;  t: ZType) =
  BEGIN
    xx.child.lt (t);
  END lt;

PROCEDURE le (xx: T;  t: ZType) =
  BEGIN
    xx.child.le (t);
  END le;

PROCEDURE add (xx: T;  t: AType) =
  BEGIN
    xx.child.add (t);
  END add;

PROCEDURE subtract (xx: T;  t: AType) =
  BEGIN
    xx.child.subtract (t);
  END subtract;

PROCEDURE multiply (xx: T;  t: AType) =
  BEGIN
    xx.child.multiply (t);
  END multiply;

PROCEDURE divide (xx: T;  t: RType) =
  BEGIN
    xx.child.divide (t);
  END divide;

PROCEDURE div (xx: T;  t: IType;  a, b: Sign) =
  BEGIN
    xx.child.div (t, a, b);
  END div;

PROCEDURE mod (xx: T;  t: IType;  a, b: Sign) =
  BEGIN
    xx.child.mod (t, a, b);
  END mod;

PROCEDURE negate (xx: T;  t: AType) =
  BEGIN
    xx.child.negate (t);
  END negate;

PROCEDURE abs (xx: T;  t: AType) =
  BEGIN
    xx.child.abs (t);
  END abs;

PROCEDURE max (xx: T;  t: ZType) =
  BEGIN
    xx.child.max (t);
  END max;

PROCEDURE min (xx: T;  t: ZType) =
  BEGIN
    xx.child.min (t);
  END min;

PROCEDURE round (xx: T;  t: RType) =
  BEGIN
    xx.child.round (t);
  END round;

PROCEDURE trunc (xx: T;  t: RType) =
  BEGIN
    xx.child.trunc (t);
  END trunc;

PROCEDURE floor (xx: T;  t: RType) =
  BEGIN
    xx.child.floor (t);
  END floor;

PROCEDURE ceiling (xx: T;  t: RType) =
  BEGIN
    xx.child.ceiling (t);
  END ceiling;

PROCEDURE cvt_float (xx: T;  t: AType;  u: RType) =
  BEGIN
    xx.child.cvt_float (t, u);
  END cvt_float;

(*------------------------------------------------------------------ sets ---*)

PROCEDURE set_union (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_union (s);
  END set_union;

PROCEDURE set_difference (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_difference (s);
  END set_difference;

PROCEDURE set_intersection (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_intersection (s);
  END set_intersection;

PROCEDURE set_sym_difference (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_sym_difference (s);
  END set_sym_difference;

PROCEDURE set_member (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_member (s);
  END set_member;

PROCEDURE set_eq (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_eq (s);
  END set_eq;

PROCEDURE set_ne (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_ne (s);
  END set_ne;

PROCEDURE set_gt (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_gt (s);
  END set_gt;

PROCEDURE set_ge (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_ge (s);
  END set_ge;

PROCEDURE set_lt (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_lt (s);
  END set_lt;

PROCEDURE set_le (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_le (s);
  END set_le;

PROCEDURE set_range (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_range (s);
  END set_range;

PROCEDURE set_singleton (xx: T;  s: ByteSize) =
  BEGIN
    xx.child.set_singleton (s);
  END set_singleton;

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE not (xx: T) =
  BEGIN
    xx.child.not ();
  END not;

PROCEDURE and (xx: T) =
  BEGIN
    xx.child.and ();
  END and;

PROCEDURE or (xx: T) =
  BEGIN
    xx.child.or ();
  END or;

PROCEDURE xor (xx: T) =
  BEGIN
    xx.child.xor ();
  END xor;

PROCEDURE shift (xx: T) =
  BEGIN
    xx.child.shift ();
  END shift;

PROCEDURE shift_left (xx: T) =
  BEGIN
    xx.child.shift_left ();
  END shift_left;

PROCEDURE shift_right (xx: T) =
  BEGIN
    xx.child.shift_right ();
  END shift_right;

PROCEDURE rotate (xx: T) =
  BEGIN
    xx.child.rotate ();
  END rotate;

PROCEDURE rotate_left (xx: T) =
  BEGIN
    xx.child.rotate_left ();
  END rotate_left;

PROCEDURE rotate_right (xx: T) =
  BEGIN
    xx.child.rotate_right ();
  END rotate_right;

PROCEDURE extract (xx: T;  sign: BOOLEAN) =
  BEGIN
    xx.child.extract (sign);
  END extract;

PROCEDURE extract_n (xx: T;  sign: BOOLEAN;  n: INTEGER) =
  BEGIN
    xx.child.extract_n (sign, n);
  END extract_n;

PROCEDURE extract_mn (xx: T;  sign: BOOLEAN;  m, n: INTEGER) =
  BEGIN
    xx.child.extract_mn (sign, m, n);
  END extract_mn;

PROCEDURE insert (xx: T) =
  BEGIN
    xx.child.insert ();
  END insert;

PROCEDURE insert_n (xx: T;  n: INTEGER) =
  BEGIN
    xx.child.insert_n (n);
  END insert_n;

PROCEDURE insert_mn (xx: T;  m, n: INTEGER) =
  BEGIN
    xx.child.insert_mn (m, n);
  END insert_mn;

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE swap (xx: T;  a, b: Type) =
  BEGIN
    xx.child.swap (a, b);
  END swap;

PROCEDURE pop (xx: T;  t: Type) =
  BEGIN
    xx.child.pop (t);
  END pop;

PROCEDURE copy_n (xx: T;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    xx.child.copy_n (t, overlap);
  END copy_n;

PROCEDURE copy (xx: T;  n: INTEGER;  t: MType;  overlap: BOOLEAN) =
  BEGIN
    xx.child.copy (n, t, overlap);
  END copy;

PROCEDURE zero_n (xx: T;  t: MType) =
  BEGIN
    xx.child.zero_n (t);
  END zero_n;

PROCEDURE zero (xx: T;  n: INTEGER;  t: MType) =
  BEGIN
    xx.child.zero (n, t);
  END zero;

(*----------------------------------------------------------- conversions ---*)

PROCEDURE loophole (xx: T;  from, two: ZType) =
  BEGIN
    xx.child.loophole (from, two);
  END loophole;

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE assert_fault (xx: T) =
  BEGIN
    xx.child.assert_fault ();
  END assert_fault;

PROCEDURE narrow_fault (xx: T) =
  BEGIN
    xx.child.narrow_fault ();
  END narrow_fault;

PROCEDURE return_fault (xx: T) =
  BEGIN
    xx.child.return_fault ();
  END return_fault;

PROCEDURE case_fault (xx: T) =
  BEGIN
    xx.child.case_fault ();
  END case_fault;

PROCEDURE typecase_fault (xx: T) =
  BEGIN
    xx.child.typecase_fault ();
  END typecase_fault;

PROCEDURE check_nil (xx: T) =
  BEGIN
    xx.child.check_nil ();
  END check_nil;

PROCEDURE check_lo (xx: T;  READONLY i: Target.Int) =
  BEGIN
    xx.child.check_lo (i);
  END check_lo;

PROCEDURE check_hi (xx: T;  READONLY i: Target.Int) =
  BEGIN
    xx.child.check_hi (i);
  END check_hi;

PROCEDURE check_range (xx: T;  READONLY a, b: Target.Int) =
  BEGIN
    xx.child.check_range (a, b);
  END check_range;

PROCEDURE check_index (xx: T) =
  BEGIN
    xx.child.check_index ();
  END check_index;

PROCEDURE check_eq (xx: T) =
  BEGIN
    xx.child.check_eq ();
  END check_eq;

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE add_offset (xx: T; i: INTEGER) =
  BEGIN
    xx.child.add_offset (i);
  END add_offset;

PROCEDURE index_address (xx: T;  size: INTEGER) =
  BEGIN
    xx.child.index_address (size);
  END index_address;

(*------------------------------------------------------- procedure calls ---*)

PROCEDURE start_call_direct (xx: T;  p: Proc;  lev: INTEGER;  t: Type) =
  BEGIN
    xx.child.start_call_direct (p, lev, t);
  END start_call_direct;

PROCEDURE start_call_indirect (xx: T;  t: Type;  cc: CallingConvention) =
  BEGIN
    xx.child.start_call_indirect (t, cc);
  END start_call_indirect;

PROCEDURE pop_param (xx: T;  t: MType) =
  BEGIN
    xx.child.pop_param (t);
  END pop_param;

PROCEDURE pop_struct (xx: T;  s: ByteSize;  a: Alignment) =
  BEGIN
    xx.child.pop_struct (s, a);
  END pop_struct;

PROCEDURE pop_static_link (xx: T) =
  BEGIN
    xx.child.pop_static_link ();
  END pop_static_link;

PROCEDURE call_direct (xx: T; p: Proc;  t: Type) =
  BEGIN
    xx.child.call_direct (p, t);
  END call_direct;

PROCEDURE call_indirect (xx: T;  t: Type;  cc: CallingConvention) =
  BEGIN
    xx.child.call_indirect (t, cc);
  END call_indirect;

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE load_procedure (xx: T;  p: Proc) =
  BEGIN
    xx.child.load_procedure (p);
  END load_procedure;

PROCEDURE load_static_link (xx: T;  p: Proc) =
  BEGIN
    xx.child.load_static_link (p);
  END load_static_link;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE comment (xx: T;  a, b, c, d: TEXT := NIL) =
  BEGIN
    xx.child.comment (a, b, c, d);
  END comment;

BEGIN
END M3CG.
