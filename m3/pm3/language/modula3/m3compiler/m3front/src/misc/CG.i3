(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CG.i3                                                 *)
(* Last modified on Tue Jun 20 15:58:36 PDT 1995 by kalsow     *)
(*      modified on Tue May 25 14:29:03 PDT 1993 by muller     *)

INTERFACE CG;

IMPORT Target, M3CG, M3;

(*
This interface provides a single front-end specific veneer over
M3CG, M3CG_Ops and M3RT.
*)

TYPE (* see M3CG for the interpretation of these types *)
  Type        = M3CG.Type;
  MType       = M3CG.MType;
  IType       = M3CG.IType;
  RType       = M3CG.RType;
  AType       = M3CG.AType;
  ZType       = M3CG.ZType;
  Sign        = M3CG.Sign;
  Name        = M3CG.Name;
  Var         = M3CG.Var;
  Proc        = M3CG.Proc;
  Offset      = M3CG.BitOffset;
  Size        = M3CG.BitSize;
  Alignment   = M3CG.Alignment;
  TypeUID     = M3CG.TypeUID;
  Label       = M3CG.Label;
  Frequency   = M3CG.Frequency;
  CallingConvention = M3CG.CallingConvention;

CONST (* see M3CG for the interpretation of these values *)
  No_label  = M3CG.No_label;

CONST (* see M3CG for the interpretation of these values *)
  Never  : Frequency = M3CG.Never;
  Maybe  : Frequency = M3CG.Maybe;
  Likely : Frequency = M3CG.Likely;
  Always : Frequency = M3CG.Always;

VAR (* maximum possible machine alignment *)
  Max_alignment: CARDINAL;

PROCEDURE Init ();
(* creates a fresh, initialized code generator *)

(*----------------------------------------------------------- ID counters ---*)

PROCEDURE Next_label (n_labels := 1): Label;
(* allocates and returns the next 'n_labels' labels *)

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Begin_unit (optimize: INTEGER := 0);
(* called before any other procedures to initialize the compilation unit. *)

PROCEDURE End_unit ();
(* called after all other procedures to finalize the unit and write the
   resulting object.  *)

PROCEDURE Import_unit (n: Name);
PROCEDURE Export_unit (n: Name);
(* note that the current compilation unit imports/exports the interface 'n' *)

(*------------------------------------------------ debugging line numbers ---*)

PROCEDURE Gen_location (here: INTEGER);
(* generate the source file and line number info for 'here' *)

(*------------------------------------------- debugging type declarations ---*)

(* The debugging information for a type is identified by small a integer
   within a compilation unit.  The information is identified by a global
   uid (an INTEGER) across compilation units. The following procedures generate
   the symbol table entries needed to describe Modula-3 types to the
   debugger.  Note that Modula-3's builtin types have the fixed IDs
   listed above.  The 'hint' passed to 'import_type' is the name of
   the source file that generated the type declaration.  *)

PROCEDURE Declare_typename (t: TypeUID;  n: Name);

PROCEDURE Declare_array (t: TypeUID;  index, elt: TypeUID;  s: Size);
PROCEDURE Declare_open_array (t: TypeUID;  elt: TypeUID;  s: Size);

PROCEDURE Declare_enum (t: TypeUID;  n_elts: INTEGER;  s: Size);
PROCEDURE Declare_enum_elt (n: Name);

PROCEDURE Declare_packed  (t: TypeUID;  s: Size;  base: TypeUID);

PROCEDURE Declare_record (t: TypeUID;  s: Size;  n_fields: INTEGER);
PROCEDURE Declare_field (n: Name;  o: Offset;  s: Size;  t: TypeUID);

PROCEDURE Declare_global_field (n: Name;  o: Offset;  s: Size;  t: TypeUID);
PROCEDURE Emit_global_record (s: Size);

PROCEDURE Declare_set (t, domain: TypeUID;  s: Size);

PROCEDURE Declare_subrange (t, domain: TypeUID;  READONLY min, max: Target.Int;
                   s: Size);

PROCEDURE Declare_pointer (t, target: TypeUID;  brand: TEXT;  traced: BOOLEAN);

PROCEDURE Declare_indirect (target: TypeUID): TypeUID;
(* an automatically dereferenced pointer! (WITH variables, VAR formals, ...) *)

PROCEDURE Declare_proctype (t: TypeUID; n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER;
                            cc: CallingConvention);
PROCEDURE Declare_formal (n: Name;  t: TypeUID);
PROCEDURE Declare_raises (n: Name);

PROCEDURE Declare_object (t, super: TypeUID;  brand: TEXT;  traced: BOOLEAN;
                 n_fields, n_methods, n_overrides: INTEGER;  field_size: Size);
PROCEDURE Declare_method (n: Name;  signature: TypeUID;  dfault: M3.Expr);
PROCEDURE Declare_override (n: Name;  dfault: M3.Expr);
PROCEDURE Declare_opaque (t, super: TypeUID);
PROCEDURE Reveal_opaque (lhs, rhs: TypeUID);

PROCEDURE Declare_exception (n: Name;  arg_type: TypeUID;  raise_proc: BOOLEAN;
                             base: Var;  offset: INTEGER);
(* declares an exception named 'n' identified with the address 'base+offset'
   that carries an argument of type 'arg_type'.  If 'raise_proc', then
   'base+offset+BYTESIZE(ADDRESS)' is a pointer to the procedure that
   packages the argument and calls the runtime to raise the exception. *)

(*--------------------------------------------------------- runtime hooks ---*)

PROCEDURE Set_runtime_proc (n: Name;  p: Proc);
(* declares 'n' as a runtime procedure 'p'.  *)

PROCEDURE Set_runtime_hook (n: Name;  v: Var;  o: Offset);
(* declares 'n' as a runtime procedure 'p' available at location 'ADR(v)+o' *)

PROCEDURE Get_runtime_hook (n: Name;  VAR p: Proc; VAR v: Var; VAR o: Offset);
(* returns the location of the runtime symbol 'n' *)

(*------------------------------------------------- variable declarations ---*)

(* Clients must declare a variable before generating any statements or
   expressions that refer to it;  declarations of global variables and
   temps can be intermixed with generation of statements and expressions.

   In the declarations that follow:

|    n: Name            is the name of the variable.  If it's NIL, the
|                         the back-end is free to choose its own unique name.
|    s: Size            is the size in bits of the declared variable
|    a: Alignment       is the minimum required alignment of the variable
|    t: Type            is the machine reprentation type of the variable
|    m3t: TypeUID       is the UID of the Modula-3 type of the variable
|    in_memory: BOOLEAN specifies whether the variable must have an address
|    exported: BOOLEAN  specifies whether the variable must be visible in
|                         other compilation units
|    init: BOOLEAN      indicates whether an explicit static initialization
|                         immediately follows this declaration.
|    up_level: BOOLEAN  specifies whether the variable is accessed from
|                         nested procedures.
|    f: Frequency       is the front-end estimate of how frequently the
|                         variable is accessed.

*)

PROCEDURE Import_global (n: Name;  s: Size;  a: Alignment;
                         t: Type;  m3t: TypeUID): Var;
(* imports the specified global variable. *)

PROCEDURE Declare_segment (n: Name;  m3t: TypeUID): Var;
PROCEDURE Bind_segment (seg: Var;  s: Size;  a: Alignment;  t: Type;
                        exported, init: BOOLEAN);
(* Together Declare_segment and Bind_segment accomplish what
   Declare_global does, but Declare_segment gives the front-end a
   handle on the variable before its size, type, or initial values
   are known.  Every declared segment must be bound exactly once. *)

PROCEDURE Declare_global (n: Name;  s: Size;  a: Alignment;  t: Type;
                          m3t: TypeUID;  exported, init: BOOLEAN): Var;
(* declares a global variable. *)

PROCEDURE Declare_constant (n: Name;  s: Size;  a: Alignment;  t: Type;
                            m3t: TypeUID;  exported, init: BOOLEAN): Var;
(* declares a read-only global variable *)
 
PROCEDURE Declare_local (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var;
(* declares a local variable.  Local variables must be declared in the
   procedure that contains them.  The lifetime of a local variable extends
   from the beginning to end of the closest enclosing begin_block/end_block. *)

PROCEDURE Declare_param (n: Name;  s: Size;  a: Alignment;  t: Type;
                         m3t: TypeUID;  in_memory, up_level: BOOLEAN;
                         f: Frequency): Var;
(* declares a formal parameter.  Formals are declared in their lexical
   order immediately following the 'declare_procedure' or
   'import_procedure' that contains them.  *)

PROCEDURE Declare_temp (s: Size;  a: Alignment;  t: Type;
                        in_memory: BOOLEAN): Var;
(* declares an anonymous local variable.  Temps are declared
   and freed between their containing procedure's begin_procedure and
   end_procedure calls.  Temps are never referenced by nested procedures. *)

PROCEDURE Free_temp (v: Var);
(* releases the space occupied by temp 'v' so that it may be reused by
   other new temporaries. *)

PROCEDURE Free_temps ();
(* free any temps that are marked busy. *)

(*--------------------------------------------- direct stack manipulation ---*)

TYPE Val <: REFANY; (* a stack value: R-value or L-value *)

PROCEDURE Pop (): Val;
(* return s0;  pop -- if necessary, create a temp and store s0 in it.  *)

PROCEDURE Pop_temp (): Val;
(* return s0;  pop -- create a temp and store s0 in it.  *)

PROCEDURE Push (v: Val);
(* push;  s0 := v *)

PROCEDURE Free (v: Val);
(* free any temporaries that "v" created *)

PROCEDURE Store_temp (v: Val);
(* v := s0;  pop  -- v must have been created by "Pop_temp" *)

PROCEDURE Force ();
(* force s0 to be materialized on the M3CG stack.  If s0 is an L-value,
   a byte-aligned address is generated.  *)

(*---------------------------------------- static variable initialization ---*)

(* Global variables may be initialized only once.  All of their init_*
   calls must be bracketed by begin_init and end_init.  Within a begin/end
   pair, init_* calls must be made in ascending offset order.  Begin/end
   pairs may not be nested.  Any space in a global variable that's not
   explicitly initialized is zeroed.  *)

PROCEDURE Begin_init (v: Var);
PROCEDURE End_init (v: Var);
(* must precede and follow any init calls *)

PROCEDURE Init_int  (o: Offset;  s: Size;  READONLY value: Target.Int);
PROCEDURE Init_intt (o: Offset;  s: Size;  value: INTEGER);
(* initializes the integer static variable at 'ADR(v)+offset' with
   the 's' low order bits of 'value' *)

PROCEDURE Init_proc (o: Offset;  value: Proc);
(* initializes the static variable at 'ADR(v)+o' with the address
   of procedure 'value'. *)

PROCEDURE Init_label (o: Offset;  value: Label);
(* initializes the static variable at 'ADR(v)+o' with the address
   of the label 'value'.  *)

PROCEDURE Init_var (o: Offset;  value: Var;  bias: Offset);
(* initializes the static variable at 'ADR(v)+o' with the address
   of 'value+bias'.  *)

PROCEDURE Init_offset (o: Offset;  var: Var);
(* initializes the static variable at 'ADR(v)+o' with the integer
   frame offset of the local variable 'var'. *)

PROCEDURE Init_chars (o: Offset;  value: TEXT);
(* initializes the static variable at 'ADR(v)+offset' with the characters
   of 'value' *)

PROCEDURE Init_float (o: Offset;  READONLY f: Target.Float);
(* initializes the static variable at 'ADR(v)+offset' with the
   floating point value 'f' *)

PROCEDURE EmitText (t: TEXT): INTEGER;
(* Emits the zero terminated string and returns its global offset. *)

(*------------------------------------------------------------ procedures ---*)

(* Clients compile a procedure by doing:

      proc := Declare_procedure (...)
        ...declare formals...
        ...declare locals...
        ...generate nested procedures (IF nested_procs_first)...
      Begin_procedure (proc)
        ...generate statements of procedure...
      End_procedure (...)
        ...generate nested procedures (IF NOT nested_procs_first)...

  begin_/end_procedure should never be nested;  depending on the
  value of 'nested_procs_first', clients compile nested procedures
  either immediately before or after their enclosing procedure.
*)

PROCEDURE Import_procedure (n: Name;  n_params: INTEGER;  ret_type: Type;
                            cc: CallingConvention;
                            VAR(*OUT*) new: BOOLEAN): Proc;
(* declare and import the external procedure with name 'n' and 'n_params'
   formal parameters.  It must be a top-level (=0) procedure that returns
   values of type 'ret_type'.  'cc' is the convention specified
   in the procedure's <*EXTERNAL*> declaration.  If 'new' is 'TRUE', the formal
   parameters must be specified by the subsequent 'declare_param' calls. *)

PROCEDURE Declare_procedure (n: Name;  n_params: INTEGER;  ret_type: Type;
                             lev: INTEGER;  cc: CallingConvention;
                             exported: BOOLEAN;  parent: Proc): Proc;
(* declare a procedure named 'n' with 'n_params' formal parameters
   at static level 'lev'.  Sets "current procedure" to this procedure.
   If the name 'n' is NIL, a new unique name will be supplied by the back-end.
   The type of the procedure's result is specifed in 'ret_type'.  If the new
   procedure is a nested procedure (level > 1) then 'parent' is
   the immediately enclosing procedure, otherwise 'parent' is NIL.
   The formal parameters are specified by the subsequent 'declare_param'
   calls. *)

PROCEDURE Begin_procedure (p: Proc);
(* begin generating code for the procedure 'p'.  Sets "current procedure"
   to 'p'. *)

PROCEDURE End_procedure (p: Proc);
(* marks the end of the code for procedure 'p'.  Sets "current procedure"
   to NIL. *)

PROCEDURE Begin_block ();
PROCEDURE End_block ();
(* marks the beginning and ending of nested anonymous blocks *)

PROCEDURE Note_procedure_origin (p: Proc);
(* note that nested procedure 'p's body occured at the current location
   in the source.  In particular, nested in whatever procedures,
   anonymous blocks, or exception scopes surround this point. *)

(*------------------------------------------------------------ statements ---*)

PROCEDURE Set_label (l: Label;  barrier: BOOLEAN := FALSE);
(* define 'l' to be at the current pc, if 'barrier', 'l' bounds an exception
   scope and no code is allowed to migrate past it. *)

PROCEDURE Jump (l: Label);
(* GOTO l *)

PROCEDURE If_true  (l: Label;  f: Frequency);
(* tmp := s0.I; pop; IF (tmp # 0) GOTO l *)

PROCEDURE If_false (l: Label;  f: Frequency);
(* tmp := s0.I; pop; IF (tmp = 0) GOTO l *)

PROCEDURE If_eq (l: Label;  t: ZType;  f: Frequency); (*== eq(t); if_true(l) *)
PROCEDURE If_ne (l: Label;  t: ZType;  f: Frequency); (*== ne(t); if_true(l) *)
PROCEDURE If_gt (l: Label;  t: ZType;  f: Frequency); (*== gt(t); if_true(l) *)
PROCEDURE If_ge (l: Label;  t: ZType;  f: Frequency); (*== ge(t); if_true(l) *)
PROCEDURE If_lt (l: Label;  t: ZType;  f: Frequency); (*== lt(t); if_true(l) *)
PROCEDURE If_le (l: Label;  t: ZType;  f: Frequency); (*== le(t); if_true(l) *)

PROCEDURE Case_jump (READONLY labels: ARRAY OF Label);
(* tmp := s0.I; pop; GOTO labels[tmp]  (NOTE: no range checking on s0.I) *)

PROCEDURE Exit_proc (t: Type);
(* Returns s0.t if the stack is non-empty, otherwise returns no value. *)

(*----------------------------------------------------------- expressions ---*)

(*  The code to evaluate expressions is generated by calling the
    procedures listed below.  Each procedure corresponds to an
    instruction for a simple stack machine.  Values in the stack
    have a type and a size.  Operations on the stack values are
    also typed.  Type mismatches may cause bad code to be generated.
    Explicit type conversions must be used.

    Integer values on the stack, regardless of how they are loaded,
    are sign-extended to full-width values.  Similarly, word values
    on the stack are always zero-extened to full-width values.

    The expression stack must be empty at each label, jump, call,
    or store operation.  The stack must contain exactly one value
    prior to a conditional or indexed jump.

    All addresses are bit addresses.  There is no boolean type;  boolean
    operators yield [0..1].

    Operations on word values are performed MOD the word size and are
    not checked for overflow.  Operations on integer values may or may not
    cause checked runtime errors depending on the particular code generator.

    The operators are declared below with a definition in terms of
    what they do to the execution stack.  For example,  ceiling(Reel)
    returns the ceiling, an integer, of the top value on the stack,
    a real:  s0.I := CEILING (s0.R).

    Unless otherwise indicated, operators have the same meaning as in
    the Modula-3 report.
*)

(*------------------------------------------------------------ load/store ---*)

PROCEDURE Load (v: Var;  o: Offset;  s: Size;  a: Alignment;  t: Type);
(* push ; s0.t := Mem [ ADR(v) + o : s ] *)

PROCEDURE Load_addr_of (v: Var;  o: Offset;  a: Alignment);
(* push ; s0.A := ADR(v) + o *)

PROCEDURE Load_addr_of_temp (v: Var;  o: Offset;  a: Alignment);
(* == Load_addr_of (v, o, a) ; free v when this L-value is consumed *)

PROCEDURE Load_indirect (t: Type;  o: Offset;  s: Size);
(* s0.t := Mem [s0.A + o : s] *)

PROCEDURE Load_int (v: Var;  o: Offset := 0);
(* == Load (v, o, Target.Integer.size, Target.Integer.align, Type.Int) *)

PROCEDURE Load_int_temp (v: Var;  o: Offset := 0);
(* == Load_int (v, o); free v when this R-value is consumed *)

PROCEDURE Load_addr (v: Var;  o: Offset := 0);
(* == Load (v, o, Target.Address.size, Target.Address.align, Type.Addr) *)

PROCEDURE Store (v: Var;  o: Offset;  s: Size;  a: Alignment;  t: Type);
(* Mem [ ADR(v) + o : s ] := s0.t ; pop *)

PROCEDURE Store_ref (v: Var;  o: Offset := 0);
(* == store (v, o, Target.Address.size, Target.Address.align, Type.Addr),
          but also does reference counting *)

PROCEDURE Store_indirect (t: Type;  o: Offset;  s: Size);
(* Mem [s1.A + o : s] := s0.t ; pop (2) *)

PROCEDURE Store_ref_indirect (o: Offset;  var: BOOLEAN);
  (* == store_indirect(Type.Addr, o, Target.Address.size);
     but also does reference counting.  If "var" is true, then reference
     counting depends on whether the effective address is in the heap or
     stack. *)

PROCEDURE Store_int (v: Var;  o: Offset := 0);
(* == Store (v, o, Target.Integer.size, Target.Integer.align, Type.Int) *)

PROCEDURE Store_addr (v: Var;  o: Offset := 0);
(* == Store (v, o, Target.Address.size, Target.Address.align, Type.Addr) *)

(*-------------------------------------------------------------- literals ---*)


PROCEDURE Load_nil     ();                         (*push ; s0.A := NIL*)
PROCEDURE Load_byte_address (x: INTEGER);          (*push ; s0.A := x *)
PROCEDURE Load_intt    (i: INTEGER);               (*push;  s0.I := i *)
PROCEDURE Load_integer (READONLY i: Target.Int);   (*push ; s0.I := i *)
PROCEDURE Load_float   (READONLY f: Target.Float); (*push ; s0.t := f *)

(*------------------------------------------------------------ arithmetic ---*)

(* when any of these operators is passed t=Type.Word, the operator
   does the unsigned comparison or arithmetic, but the operators
   and the result are of type Integer *)
   
PROCEDURE Eq        (t: ZType);  (* s1.I := (s1.t = s0.t)  ; pop *)
PROCEDURE Ne        (t: ZType);  (* s1.I := (s1.t # s0.t)  ; pop *)
PROCEDURE Gt        (t: ZType);  (* s1.I := (s1.t > s0.t)  ; pop *)
PROCEDURE Ge        (t: ZType);  (* s1.I := (s1.t >= s0.t) ; pop *)
PROCEDURE Lt        (t: ZType);  (* s1.I := (s1.t < s0.t)  ; pop *)
PROCEDURE Le        (t: ZType);  (* s1.I := (s1.t <= s0.t) ; pop *)
PROCEDURE Add       (t: AType);  (* s1.t := s1.t + s0.t ; pop *)
PROCEDURE Subtract  (t: AType);  (* s1.t := s1.t - s0.t ; pop *)
PROCEDURE Multiply  (t: AType);  (* s1.t := s1.t * s0.t ; pop *)
PROCEDURE Divide    (t: RType);  (* s1.t := s1.t / s0.t ; pop *)
PROCEDURE Negate    (t: AType);  (* s0.t := - s0.t *)
PROCEDURE Abs       (t: AType);  (* s0.t := ABS (s0.t) (noop on Words) *)
PROCEDURE Max       (t: ZType);  (* s1.t := MAX (s1.t, s0.t) ; pop *)
PROCEDURE Min       (t: ZType);  (* s1.t := MIN (s1.t, s0.t) ; pop *)
PROCEDURE Round     (t: RType);  (* s0.I := ROUND (s0.t) *)
PROCEDURE Trunc     (t: RType);  (* s0.I := TRUNC (s0.t) *)
PROCEDURE Floor     (t: RType);  (* s0.I := FLOOR (s0.t) *)
PROCEDURE Ceiling   (t: RType);  (* s0.I := CEILING (s0.t) *)
PROCEDURE Cvt_float (t: AType;  u: RType);   (* s0.u := FLOAT (s0.t, u) *)
PROCEDURE Div       (t: IType;  a, b: Sign); (* s1.t := s1.t DIV s0.t;pop*)
PROCEDURE Mod       (t: IType;  a, b: Sign); (* s1.t := s1.t MOD s0.t;pop*)

(*------------------------------------------------------------------ sets ---*)

(* Set sizes are in bits.  Sets not larger than an integer are
   represented on the stack as integers.  Other "large" sets are
   represented by their addresses.  The strict inequality operators
   (lt, gt) are *not* supported for small sets, the front-end
   must synthesize them from union, difference, eq, etc.  *)

PROCEDURE Set_union          (s: Size);  (* s2.B := s1.B + s0.B ; pop(3) *)
PROCEDURE Set_difference     (s: Size);  (* s2.B := s1.B - s0.B ; pop(3) *)
PROCEDURE Set_intersection   (s: Size);  (* s2.B := s1.B * s0.B ; pop(3) *)
PROCEDURE Set_sym_difference (s: Size);  (* s2.B := s1.B / s0.B ; pop(3) *)
PROCEDURE Set_member         (s: Size);  (* s1.I := (s0.I IN s1.B); pop *)
PROCEDURE Set_eq             (s: Size);  (* s1.I := (s1.B = s0.B); pop *)
PROCEDURE Set_ne             (s: Size);  (* s1.I := (s1.B # s0.B); pop *)
PROCEDURE Set_lt             (s: Size);  (* s1.I := (s1.B < s0.B); pop *)
PROCEDURE Set_le             (s: Size);  (* s1.I := (s1.B <= s0.B); pop *)
PROCEDURE Set_gt             (s: Size);  (* s1.I := (s1.B > s0.B); pop *)
PROCEDURE Set_ge             (s: Size);  (* s1.I := (s1.B >= s0.B); pop *)
PROCEDURE Set_singleton      (s: Size);  (* s1.A [s0.I] := 1; pop(2) *)
PROCEDURE Set_range          (s: Size);  (* s2.A[s1.I..s0.I] := 1; pop(3)
                                             --- S2.A must be forced *)

(*------------------------------------------------- Word.T bit operations ---*)

PROCEDURE Not ();  (* s0.I := Word.Not (s0.I) *)
PROCEDURE And ();  (* s1.I := Word.And (s1.I, s0.I) ; pop *)
PROCEDURE Or  ();  (* s1.I := Word.Or  (s1.I, s0.I) ; pop *)
PROCEDURE Xor ();  (* s1.I := Word.Xor (s1.I, s0.I) ; pop *)

PROCEDURE Shift        ();  (* s1.I := Word.Shift  (s1.I, s0.I) ; pop *)
PROCEDURE Shift_left   ();  (* s1.I := Word.Shift  (s1.I, s0.I) ; pop *)  
PROCEDURE Shift_right  ();  (* s1.I := Word.Shift  (s1.I, -s0.I) ; pop *)
PROCEDURE Rotate       ();  (* s1.I := Word.Rotate (s1.I, s0.I) ; pop *)
PROCEDURE Rotate_left  ();  (* s1.I := Word.Rotate (s1.I, s0.I) ; pop *)
PROCEDURE Rotate_right ();  (* s1.I := Word.Rotate (s1.I, -s0.I) ; pop *)

PROCEDURE Extract (sign: BOOLEAN);
  (* s2.I := Word.Extract(s2.I, s1.I, s0.I);
     IF sign THEN SignExtend s2 ; pop(2) *)

PROCEDURE Extract_n (sign: BOOLEAN;  n: INTEGER);
(* s1.I := Word.Extract(s1.I, s0.I, n);
   IF sign THEN SignExtend s1; pop(1) *)

PROCEDURE Extract_mn (sign: BOOLEAN;  m, n: INTEGER);
(* s0.I := Word.Extract(s0.I, m, n);
   IF sign THEN SignExtend s0 *)

PROCEDURE Insert  ();
  (* s3.I := Word.Insert (s3.I, s2.I, s1.I, s0.I) ; pop(3) *)

PROCEDURE Insert_n (n: INTEGER);
(* s2.I := Word.Insert (s2.I, s1.I, s0.I, n); pop(2) *)

PROCEDURE Insert_mn (m, n: INTEGER);
(* s1.I := Word.Insert (s1.I, s0.I, m, n); pop(1) *)

(*------------------------------------------------ misc. stack/memory ops ---*)

PROCEDURE Swap ();           (* tmp := s1; s1 := s0; s0 := tmp *)
PROCEDURE Discard (t: Type); (* pop(1) discard s0, not its side effects *)

PROCEDURE Copy_n (s: Size;  overlap: BOOLEAN);
(* Mem[s2.A:s0.I*s] := Mem[s1.A:s0.I*s]; pop(3) -- s2.A &s1.A must be forced.
   'overlap' is true if the source and destination may partially overlap
   (ie. you need memmove, not just memcpy). *)

PROCEDURE Copy (s: Size;  overlap: BOOLEAN);
(* Mem[s1.A:s] := Mem[s0.A:s]; pop(2).
   'overlap' is true if the source and destination may partially overlap
   (ie. you need memmove, not just memcpy). *)

PROCEDURE Zero (s: Size);
(* Mem[s0.A:s] := 0; pop(1) *)

(*----------------------------------------------------------- conversions ---*)

PROCEDURE Loophole (from, two: Type);
(* s0.to := LOOPHOLE(s0.from, to) *)

(*------------------------------------------------ traps & runtime checks ---*)

PROCEDURE Assert_fault   ();
PROCEDURE Narrow_fault   ();
PROCEDURE Return_fault   ();
PROCEDURE Case_fault     ();
PROCEDURE Typecase_fault ();
(* Abort *)

PROCEDURE Check_nil ();
(* IF (s0.A = NIL) THEN Abort *)

PROCEDURE Check_lo (READONLY i: Target.Int);
(* IF (s0.I < i) THEN Abort *)

PROCEDURE Check_hi (READONLY i: Target.Int);
(* IF (i < s0.I) THEN Abort *)

PROCEDURE Check_range (READONLY a, b: Target.Int);
(* IF (s0.I < a) OR (b < s0.I) THEN Abort *)

PROCEDURE Check_index ();
(* IF NOT (0 <= s1.I < s0.I) THEN Abort END; pop *)

PROCEDURE Check_eq ();
(* IF (s0.I # s1.I) THEN Abort;  Pop (2) *)

PROCEDURE Check_byte_aligned ();
(* IF (s0.A is not byte-aligned) THEN Abort *)

(*---------------------------------------------------- address arithmetic ---*)

PROCEDURE Add_offset (i: INTEGER);
(* s0.A := s0.A + i *)

PROCEDURE Index_bytes (size: INTEGER);
(* s1.A := s1.A + s0.I * size ; pop -- size must be a multiple of
   Target.Byte. *)

PROCEDURE Index_bits ();
(* s1.A := s1.A + s0.I ; pop -- note that s0.I must be less than
  or equal to the alignment of s1.A, otherwise bad code will be generated. *)

PROCEDURE Boost_alignment (a: Alignment);
(* note that s0.A has an alignment of at least 'a'. *)

PROCEDURE GCD (a, b: INTEGER): INTEGER;
(* return the greatest x that divides both a and b. *)

(*------------------------------------------------------- procedure calls ---*)

(* To generate a direct procedure call:

      Start_call_direct (proc, level, t);
    
      for each actual parameter i
          <generate value for parameter i>
          Pop_param ();  -or-  Pop_struct();
        
      Call_direct (proc, t);

   or to generate an indirect call:

      Start_call_indirect (t, cc);

      If the target is a nested procedure,
          <evaluate the static link to be used>
          Pop_static_link ();
    
      for each actual parameter i
          <generate value for parameter i>
          Pop_param ();  -or-  Pop_struct();

      <evaluate the address of the procedure to call>
      Call_indirect (t, cc);
*)

PROCEDURE Start_call_direct (p: Proc;  lev: INTEGER;  t: Type);
(* begin a procedure call to procedure 'p' at static level 'lev'
   that will return a value of type 't'. *)

PROCEDURE Call_direct (p: Proc;  t: Type);
(* call the procedure 'p'.  It returns a value of type t. *)

PROCEDURE Start_call_indirect (t: Type;  cc: CallingConvention);
(* begin an indirect procedure call that will return a value of type 't'. *)

PROCEDURE Call_indirect (t: Type;  cc: CallingConvention);
(* call the procedure whose address is in s0.A and pop s0.  The
   procedure returns a value of type t. *)

PROCEDURE Pop_param (t: Type);
(* pop s0.t and make it the "next" parameter in the current call *)

PROCEDURE Pop_struct (s: Size;  a: Alignment);
(* pop s0.A, it's a pointer to a structure occupying 's' bits that's
  'a' bit aligned; pass it by value as the "next" parameter in the current
  call. *)

PROCEDURE Pop_static_link ();
(* pop s0.A and make it the static link for the current indirect procedure call *)

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE Load_procedure (p: Proc);
(* push; s0.A := ADDR (p's body) *)

PROCEDURE Load_static_link (p: Proc);
(* push; s0.A := (static link need to call p, NIL for top-level procs) *)

(*------------------------------------------------ builtin type operations --*)

PROCEDURE Ref_to_typecode ();
(* s0.I := TYPECODE (s0.A)  for non-NIL s0.A
   == Load_indirect (Type.Int, -Target.Address.pack,
                        Target.Address.align, Target.Address.size);
      Load_integer (1);
      Shift_right ();
*)

(*------------------------------------------------------------ open arrays --*)

PROCEDURE Open_elt_ptr (a: Alignment);
(* == Load_indirect (Type.Addr, M3RT.OA_elt_ptr, Target.Address.align,
                        Target.Address.size);  Boost_alignment (a) *)

PROCEDURE Open_size (n: INTEGER);
(* == Load_indirect (Type.Int, M3RT.OA_sizes + n * Target.Integer.pack,
                        Target.Integer.align, Target.Integer.size) *)

(*------------------------------------------- procedure and closure types ---*)

PROCEDURE If_closure (proc: Val;  true, false: Label;  freq: Frequency);
(* x := (proc # NIL) AND ORD ((proc)^.CL_marker = CL_marker_value);
   IF (x) GOTO true ELSE goto FALSE;
 Note: either true or false must be No_label *)

PROCEDURE Closure_proc ();
(* s0.A := (s0.A)^.CL_proc *)

PROCEDURE Closure_frame ();
(* s0.A := (s0.A)^.CL_frame *)

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE Comment (offset: INTEGER;  a, b, c, d: TEXT := NIL);
(* annotate the output with a&b&c&d as a comment *)

END CG.
