(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 25 09:10:40 PDT 1994 by kalsow     *)

(*  Modula-3 runtime interface

    This interface defines the data structures needed by the compiler
    to interface to the Modula-3 runtime.

    Note that all runtime structures have their fields naturally aligned.
*)

INTERFACE M3RT;

(*------------------------------------------- procedure and closure types ---*)

(* closure offsets *)
VAR
  CL_marker : CARDINAL;   (* : INTEGER *)
  CL_proc   : CARDINAL;   (* : PROCEDURE() *)
  CL_frame  : CARDINAL;   (* : ADDRESS *)
  CL_SIZE   : CARDINAL;

CONST
  CL_marker_value = -1;

(*------------------------------------------------------ exception support --*)
(* Exception scope descriptors for systems with stack walkers. *)

TYPE
  HandlerClass = { Except, ExceptElse,
                   Finally, FinallyProc,
                   Raises, RaisesNone,
                   Lock };

VAR
  EX_class       : CARDINAL; (* : CHAR(HandlerClass)*)
  EX_outermost   : CARDINAL; (* : CHAR(BOOLEAN)     *)
  EX_end_of_list : CARDINAL; (* : CHAR(BOOLEAN)     *)
  EX_pad         : CARDINAL; (* : CHAR(BOOLEAN)     *)
  EX_start       : CARDINAL; (* : ADDRESS           *)
  EX_stop        : CARDINAL; (* : ADDRESS           *)
  EX_excepts     : CARDINAL; (* : ADDRESS           *)
  EX_offset      : CARDINAL; (* : INTEGER           *)
  EX_SIZE        : CARDINAL;

VAR
  EI_exception   : CARDINAL; (* : ADDRESS *)
  EI_arg         : CARDINAL; (* : ADDRESS *)
  EI_SIZE        : CARDINAL;

(*------------------------------------------------------- exception frames --*)
(* Explicit exception frames for systems without stack walkers.  *)

VAR (* all frames  (== all of a RaisesNone frame) *)
  EF_next        : CARDINAL;  (* : ADDRESS *)
  EF_class       : CARDINAL;  (* : INTEGER(HandlerClass) *)
  EF_SIZE        : CARDINAL;

VAR (* Except, ExceptElse, and Finally  frames *)
  EF1_handles    : CARDINAL;  (* : ADDRESS *)
  EF1_exception  : CARDINAL;  (* : ADDRESS *)
  EF1_arg        : CARDINAL;  (* : ADDRESS *)
  EF1_jmpbuf     : CARDINAL;  (* : jmp_buf *)
  EF1_SIZE       : CARDINAL;
  EF1_ALIGN      : CARDINAL;  (* because of the jmp_buf alignment... *)

VAR (* FinallyProc frames *)
  EF2_handler    : CARDINAL;  (* : ADDRESS (PROC) *)
  EF2_frame      : CARDINAL;  (* : ADDRESS *)
  EF2_SIZE       : CARDINAL;

VAR (* Raises frames *)
  EF3_raises     : CARDINAL;   (* : ADDRESS *)
  EF3_SIZE       : CARDINAL;

VAR (* Lock frames *)
  EF4_mutex      : CARDINAL;   (* : MUTEX *)
  EF4_SIZE       : CARDINAL;

(*---------------------------------------------------- runtime type system --*)
(* Reference types are represented by global variables call "typecells". *)

VAR (* typecell offsets *)
  TC_typecode       : CARDINAL; (* : INTEGER *)
  TC_lastSubTypeTC  : CARDINAL; (* : INTEGER *)
  TC_selfID         : CARDINAL; (* : INTEGER *)
  TC_fp             : CARDINAL; (* : 64-bit fingerprint *)
  TC_traced         : CARDINAL; (* : BOOLEAN *)
  TC_dataOffset     : CARDINAL; (* : INTEGER *)
  TC_dataSize       : CARDINAL; (* : INTEGER *)
  TC_dataAlignment  : CARDINAL; (* : INTEGER *)
  TC_methodOffset   : CARDINAL; (* : INTEGER *)
  TC_methodSize     : CARDINAL; (* : INTEGER *)
  TC_nDimensions    : CARDINAL; (* : INTEGER *)
  TC_elementSize    : CARDINAL; (* : INTEGER *)
  TC_defaultMethods : CARDINAL; (* : ADDRESS *)
  TC_type_map       : CARDINAL; (* : ADDRESS *)
  TC_gc_map         : CARDINAL; (* : ADDRESS *)
  TC_type_desc      : CARDINAL; (* : ADDRESS *)
  TC_initProc       : CARDINAL; (* : PROC()  *)
  TC_linkProc       : CARDINAL; (* : PROC()  *)
  TC_parentID       : CARDINAL; (* : INTEGER *)
  TC_parent         : CARDINAL; (* : ADDRESS *)
  TC_children       : CARDINAL; (* : ADDRESS *)
  TC_sibling        : CARDINAL; (* : ADDRESS *)
  TC_brand          : CARDINAL; (* : ADDRESS *)
  TC_name           : CARDINAL; (* : ADDRESS *)
  TC_next           : CARDINAL; (* : ADDRESS *)
  TC_SIZE           : CARDINAL;
  TC_ALIGN          : CARDINAL;

VAR (* opaque revelations *)
  RV_lhs_id         : CARDINAL; (* : INTEGER *)
  RV_rhs_id         : CARDINAL; (* : INTEGER *)
  RV_SIZE           : CARDINAL;

(*------------------------------------------------------------ open arrays --*)

VAR (* dope vector offsets *)
  OA_elt_ptr : CARDINAL; (*: ADDRESS *)
  OA_sizes   : CARDINAL; (*: ARRAY [0..depth] OF INT*)
  OA_size_0  : CARDINAL; (*: INTEGER *)
  OA_size_1  : CARDINAL; (*: INTEGER *)

(*----------------------------------------------------------- module info ---*)

VAR (* offsets and size of an RT0.ModuleInfo record *)
  MI_file           : CARDINAL; (* : ADDRESS *)
  MI_type_cells     : CARDINAL; (* : ADDRESS *)
  MI_type_cell_ptrs : CARDINAL; (* : ADDRESS *)
  MI_full_rev       : CARDINAL; (* : ADDRESS *)
  MI_part_rev       : CARDINAL; (* : ADDRESS *)
  MI_proc_info      : CARDINAL; (* : ADDRESS *)
  MI_try_scopes     : CARDINAL; (* : ADDRESS *)
  MI_var_map        : CARDINAL; (* : ADDRESS *)
  MI_gc_map         : CARDINAL; (* : ADDRESS *)
  MI_link           : CARDINAL; (* : ADDRESS *)
  MI_main           : CARDINAL; (* : ADDRESS *)
  MI_SIZE           : CARDINAL;

VAR (* offsets and size of an RT0.ProcInfo record *)
  PI_proc       : CARDINAL; (*: ADDRESS *)
  PI_name       : CARDINAL; (*: ADDRESS *)
  PI_export     : CARDINAL; (*: ADDRESS *)
  PI_SIZE       : CARDINAL;

(*----------------------------------------------------------- ref headers ---*)

CONST (* bit offsets and sizes of the fields within a REF's header word *)
  RH_typecode_offset  = 1;
  RH_typecode_size    = 20;

CONST (* builtin, constant typecodes *)
  NULL_typecode = 0;
  TEXT_typecode = 1;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init ();
(* initializes the values above to reflect the values exported by Target *)

END M3RT.

