(* Copyright (C) 1994, Digital Equipment Corporation                       *)
(* All rights reserved.                                                    *)
(* See the file COPYRIGHT for a full description.                          *)
(*                                                                         *)
(* Last modified on Fri Jan 10 13:00:00 PDT 1997 by collin@vlsi.polymtl.ca *)
(*      modified on Fri Oct 14 10:14:32 PDT 1994 by kalsow                 *)
(*							                   *)
(* An "M3ObjFile.T" represents a conventional "object file"                *)
(* with text and data segments, symbol tables, and 	                   *)
(* relocation information. 				                   *)

INTERFACE M3ObjFile;

IMPORT M3ID, Target;

FROM M3CG IMPORT Name, BitOffset, BitSize, ByteOffset, ByteSize, TypeUID, Type;

TYPE
  T = OBJECT METHODS
    set_debugging();

    cursor (s: Seg): INTEGER;
    append (s: Seg;  value, length: INTEGER);
    patch  (s: Seg;  offset, value, length: INTEGER);

    relocate        (src_sym, src_offset, target_sym, kind: INTEGER);

    import_symbol     (id: M3ID.T): INTEGER;
    define_symbol     (id: M3ID.T;  s: Seg;  offset: INTEGER): INTEGER;
    define_bss_symbol (id: M3ID.T;  size, align: INTEGER): INTEGER;
    define_sect_sym   (s: Seg): INTEGER;

    add_symbol_size (id: M3ID.T;  size: INTEGER);
    move_symbol   (sym: INTEGER;  new_offset: INTEGER);
    export_symbol (sym: INTEGER);

    set_source_file (filename: TEXT);
    set_source_line (line: INTEGER);

    begin_unit();
    end_unit();
    declare_segment(n : Name);
    export_unit(n: Name);

    declare_typename (t: TypeUID;  n: Name);
    declare_array (t, index, elt: TypeUID;  s: BitSize);
    declare_open_array (t, elt: TypeUID;  s: BitSize);
    declare_enum (t: TypeUID; n_elts: INTEGER;  s: BitSize);
    declare_enum_elt (n: Name);
    declare_packed  (t: TypeUID;  s: BitSize;  base: TypeUID);
    declare_record (t: TypeUID;  s: BitSize;  n_fields: INTEGER);
    declare_field (n: Name;  o: BitOffset;  s: BitSize;  t: TypeUID);
    declare_set (t, domain: TypeUID;  s: BitSize);
    declare_subrange (t, domain: TypeUID; READONLY min,max: Target.Int;
                      s: BitSize);
    declare_pointer (t, target: TypeUID;  brand: TEXT;  traced: BOOLEAN);
    declare_indirect (t, target: TypeUID);
    declare_proctype (t: TypeUID;  n_formals: INTEGER;
                      result: TypeUID;  n_raises: INTEGER);
    declare_formal (n: Name;  t: TypeUID);
    declare_raises (n: Name);
    declare_object (t, super: TypeUID;  brand: TEXT;
                    traced: BOOLEAN;  n_fields, n_methods: INTEGER;
                    field_size: BitSize);
    declare_method (n: Name;  signature: TypeUID);
    declare_opaque (t, super: TypeUID);
    reveal_opaque (lhs, rhs: TypeUID);

    declare_exception (name: Name;  arg_type: TypeUID;  raise_proc: BOOLEAN);
    declare_global    (n: Name;  s: ByteSize;  bss: BOOLEAN);
    declare_constant  (sym: INTEGER;  s: ByteSize;  m3t: TypeUID);

    declare_local (n: Name;  s: ByteSize;  frame: ByteOffset;  m3t: TypeUID;
		   t: Type);
    declare_param (n: Name;  s: ByteSize;  frame: ByteOffset;  m3t: TypeUID;
		   t: Type);

    declare_procedure (n, parent_n: Name;  return_type: Type;
		       exported: BOOLEAN; sym: INTEGER);

    declare_case_jump (n_labels, indexreg : INTEGER; pic: BOOLEAN);

    begin_procedure (sym, o: INTEGER);
    end_procedure (sym: INTEGER);

    begin_block ();
    end_block (o:= 0);

    note_procedure_origin (sym: INTEGER);
  END;

TYPE
  Seg = {Text, Data, Bss, Rodata};

CONST     (* ELF Relocation types *)
  R_386_32     =  1;
  R_386_PC32   =  2;
  R_386_GOT32  =  3;
  R_386_PLT32  =  4;
  R_386_GOTOFF =  9;
  R_386_GOTPC  = 10;

END M3ObjFile.
