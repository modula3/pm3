(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 25 09:11:22 PDT 1994 by kalsow     *)

MODULE M3RT;

IMPORT Target;

PROCEDURE Init () =
  VAR
    IP := Target.Integer.pack;
    AP := Target.Address.pack;
    CP := Target.Char.pack;
  BEGIN
    (* closure offsets *)
    CL_marker := 0;                (* : INTEGER *)
    CL_proc   := CL_marker + IP;   (* : PROCEDURE() *)
    CL_frame  := CL_proc   + AP;   (* : ADDRESS *)
    CL_SIZE   := CL_frame  + AP;

    (* exception scope descriptors *)
    EX_class       := 0;                   (* : CHAR(HandlerClass)*)
    EX_outermost   := EX_class + CP;       (* : CHAR(BOOLEAN)     *)
    EX_end_of_list := EX_outermost + CP;   (* : CHAR(BOOLEAN)     *)
    EX_pad         := EX_end_of_list + CP; (* : CHAR(BOOLEAN)     *)
    EX_start       := MAX (EX_pad + CP, EX_class + AP); (* : ADDRESS *)
    EX_stop        := EX_start + AP;       (* : ADDRESS           *)
    EX_excepts     := EX_stop  + AP;       (* : ADDRESS           *)
    EX_offset      := EX_excepts + AP;     (* : INTEGER           *)
    EX_SIZE        := EX_offset + IP;

    (* exception info record *)
    EI_exception   := 0;                   (* : ADDRESS *)
    EI_arg         := EI_exception + AP;   (* : ADDRESS *)
    EI_SIZE        := EI_arg + AP;

    (* all exception frames  (== all of a RaisesNone frame) *)
    EF_next        := 0;                   (* : ADDRESS *)
    EF_class       := EF_next + AP;        (* : INTEGER(HandlerClass) *)
    EF_SIZE        := EF_class + IP;

    (* Except, ExceptElse, and Finally  frames *)
    EF1_handles    := EF_SIZE;             (* : ADDRESS *)
    EF1_exception  := EF1_handles + AP;    (* : ADDRESS *)
    EF1_arg        := EF1_exception + AP;  (* : ADDRESS *)
    EF1_jmpbuf     := RoundUp (EF1_arg + AP, Target.Jumpbuf_align);
                                           (* : jmp_buf *)
    EF1_SIZE       := EF1_jmpbuf + Target.Jumpbuf_size;
    EF1_ALIGN      := MAX (Target.Address.align, Target.Jumpbuf_align);

    (* FinallyProc frames *)
    EF2_handler    := EF_SIZE;            (* : ADDRESS (PROC) *)
    EF2_frame      := EF2_handler + AP;   (* : ADDRESS *)
    EF2_SIZE       := EF2_frame + AP;

    (* Raises frames *)
    EF3_raises     := EF_SIZE;            (* : ADDRESS *)
    EF3_SIZE       := EF3_raises + AP;

    (* Lock frames *)
    EF4_mutex      := EF_SIZE;            (* : MUTEX *)
    EF4_SIZE       := EF4_mutex + AP;

    (* typecell offsets *)
    TC_typecode       := 0;                      (* : INTEGER *)
    TC_lastSubTypeTC  := TC_typecode       + IP; (* : INTEGER *)
    TC_selfID         := TC_lastSubTypeTC  + IP; (* : INTEGER *)
    TC_fp             := TC_selfID         + IP; (* : 64-bit fingerprint *)
    TC_traced         := TC_fp             + 64; (* : BOOLEAN *)
    TC_dataOffset     := TC_traced         + IP; (* : INTEGER *)
    TC_dataSize       := TC_dataOffset     + IP; (* : INTEGER *)
    TC_dataAlignment  := TC_dataSize       + IP; (* : INTEGER *)
    TC_methodOffset   := TC_dataAlignment  + IP; (* : INTEGER *)
    TC_methodSize     := TC_methodOffset   + IP; (* : INTEGER *)
    TC_nDimensions    := TC_methodSize     + IP; (* : INTEGER *)
    TC_elementSize    := TC_nDimensions    + IP; (* : INTEGER *)
    TC_defaultMethods := TC_elementSize    + IP; (* : ADDRESS *)
    TC_type_map       := TC_defaultMethods + AP; (* : ADDRESS *)
    TC_gc_map         := TC_type_map       + AP; (* : ADDRESS *)
    TC_type_desc      := TC_gc_map         + AP; (* : ADDRESS *)
    TC_initProc       := TC_type_desc      + AP; (* : PROC()  *)
    TC_linkProc       := TC_initProc       + AP; (* : PROC()  *)
    TC_parentID       := TC_linkProc       + AP; (* : INTEGER *)
    TC_parent         := TC_parentID       + IP; (* : ADDRESS *)
    TC_children       := TC_parent         + AP; (* : ADDRESS *)
    TC_sibling        := TC_children       + AP; (* : ADDRESS *)
    TC_brand          := TC_sibling        + AP; (* : ADDRESS *)
    TC_name           := TC_brand          + AP; (* : ADDRESS *)
    TC_next           := TC_name           + AP; (* : ADDRESS *)
    TC_SIZE           := TC_next           + AP;
    TC_ALIGN          := MAX (Target.Address.align, Target.Integer.align);

    RV_lhs_id  := 0;                  (* : INTEGER *)
    RV_rhs_id  := RV_lhs_id + IP;     (* : INTEGER *)
    RV_SIZE    := RV_rhs_id + IP;

    (* dope vector offsets *)
    OA_elt_ptr := 0;                                (*: ADDRESS *)
    OA_sizes   := OA_elt_ptr + AP; (*: ARRAY [0..depth] OF INT*)
    OA_size_0  := OA_sizes;                         (*: INTEGER *)
    OA_size_1  := OA_size_0 + IP;  (*: INTEGER *)

    (* offsets and size of an RT0.ModuleInfo record *)
    MI_file           := 0;                      (* : ADDRESS *)
    MI_type_cells     := MI_file + AP;           (* : ADDRESS *)
    MI_type_cell_ptrs := MI_type_cells + AP;     (* : ADDRESS *)
    MI_full_rev       := MI_type_cell_ptrs + AP; (* : ADDRESS *)
    MI_part_rev       := MI_full_rev + AP;       (* : ADDRESS *)
    MI_proc_info      := MI_part_rev + AP;       (* : ADDRESS *)
    MI_try_scopes     := MI_proc_info + AP;      (* : ADDRESS *)
    MI_var_map        := MI_try_scopes + AP;     (* : ADDRESS *)
    MI_gc_map         := MI_var_map + AP;        (* : ADDRESS *)
    MI_link           := MI_gc_map + AP;         (* : ADDRESS *)
    MI_main           := MI_link + AP;           (* : ADDRESS *)
    MI_SIZE           := MI_main + AP;

    (* offsets and size of an RT0.ProcInfo record *)
    PI_proc      := 0;                (*: ADDRESS *)
    PI_name      := PI_proc + AP;     (*: ADDRESS *)
    PI_export    := PI_name + AP;     (*: ADDRESS *)
    PI_SIZE      := PI_export + AP;
  END Init;

PROCEDURE RoundUp (a, b: INTEGER): INTEGER =
  BEGIN
    RETURN (a + b - 1) DIV b * b;
  END RoundUp;

BEGIN
END M3RT.
