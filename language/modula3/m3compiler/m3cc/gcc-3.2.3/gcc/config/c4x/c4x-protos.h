/* Definitions of target machine for GNU compiler.  TMS320C[34]x
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999
   Free Software Foundation, Inc.

   Contributed by Michael Hayes (m.hayes@elec.canterbury.ac.nz)
              and Herman Ten Brugge (Haj.Ten.Brugge@net.HCC.nl).

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef GCC_C4X_PROTOS_H
#define GCC_C4X_PROTOS_H

extern void c4x_override_options PARAMS ((void));

extern void c4x_optimization_options PARAMS ((int, int));

extern void c4x_output_ascii PARAMS ((FILE *, const char *, int));

extern int c4x_interrupt_function_p PARAMS ((void));

extern void c4x_expand_prologue PARAMS ((void));

extern void c4x_expand_epilogue PARAMS ((void));

extern int c4x_null_epilogue_p PARAMS ((void));

extern void c4x_global_label (const char *);

extern void c4x_external_ref (const char *);

extern void c4x_file_end (FILE *);

#ifdef TREE_CODE
extern void c4x_function_arg_advance PARAMS ((CUMULATIVE_ARGS *, 
					      enum machine_mode, tree, int));

extern struct rtx_def *c4x_function_arg PARAMS ((CUMULATIVE_ARGS *,
						 enum machine_mode, tree,
						 int));

extern void c4x_encode_section_info PARAMS ((tree));

#endif /* TREE_CODE */


#if defined(RTX_CODE) && defined(TREE_CODE)
extern void c4x_init_cumulative_args PARAMS ((CUMULATIVE_ARGS *c, tree, rtx));

extern void c4x_va_start PARAMS ((int, tree, rtx));

extern struct rtx_def *c4x_va_arg PARAMS ((tree, tree));

extern rtx c4x_expand_builtin PARAMS ((tree, rtx, rtx,
				       enum machine_mode, int));

extern void c4x_init_builtins PARAMS ((void));

#endif /* TREE_CODE and RTX_CODE*/


#ifdef RTX_CODE
extern struct rtx_def *c4x_gen_compare_reg PARAMS ((enum rtx_code, rtx, rtx));

extern int c4x_check_legit_addr PARAMS ((enum machine_mode, rtx, int));

extern int c4x_hard_regno_mode_ok PARAMS ((unsigned int, enum machine_mode));

extern int c4x_hard_regno_rename_ok PARAMS ((unsigned int, unsigned int));

extern struct rtx_def *c4x_legitimize_address PARAMS ((rtx,
						       enum machine_mode));

extern int c4x_address_cost PARAMS ((rtx));

extern void c4x_print_operand PARAMS ((FILE *, rtx, int));

extern void c4x_print_operand_address PARAMS ((FILE *, rtx));

extern enum reg_class c4x_preferred_reload_class PARAMS ((rtx,
							  enum reg_class));

extern struct rtx_def *c4x_operand_subword PARAMS ((rtx, int, int,
						   enum machine_mode));

extern char *c4x_output_cbranch PARAMS ((const char *, rtx));

extern int c4x_label_conflict PARAMS ((rtx, rtx, rtx));

extern int c4x_address_conflict PARAMS ((rtx, rtx, int, int));

extern void c4x_process_after_reload PARAMS ((rtx));

extern void c4x_rptb_insert PARAMS ((rtx insn));

extern int c4x_rptb_nop_p PARAMS ((rtx));

extern int c4x_rptb_rpts_p PARAMS ((rtx, rtx));

extern int c4x_check_laj_p PARAMS ((rtx));

extern int c4x_autoinc_operand PARAMS ((rtx, enum machine_mode));

extern int any_operand PARAMS ((rtx, enum machine_mode));

extern int fp_zero_operand PARAMS ((rtx, enum machine_mode));

extern int const_operand PARAMS ((rtx, enum machine_mode));

extern int stik_const_operand PARAMS ((rtx, enum machine_mode));

extern int not_const_operand PARAMS ((rtx, enum machine_mode));

extern int parallel_operand PARAMS ((rtx, enum machine_mode));

extern int reg_or_const_operand PARAMS ((rtx, enum machine_mode));

extern int reg_operand PARAMS ((rtx, enum machine_mode));

extern int mixed_subreg_operand PARAMS ((rtx, enum machine_mode));

extern int reg_imm_operand PARAMS ((rtx, enum machine_mode));

extern int r0r1_reg_operand PARAMS ((rtx, enum machine_mode));

extern int r2r3_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ext_low_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ext_reg_operand PARAMS ((rtx, enum machine_mode));

extern int std_reg_operand PARAMS ((rtx, enum machine_mode));

extern int std_or_reg_operand PARAMS ((rtx, enum machine_mode));

extern int dst_operand PARAMS ((rtx, enum machine_mode));

extern int src_operand PARAMS ((rtx, enum machine_mode));

extern int src_hi_operand PARAMS ((rtx, enum machine_mode));

extern int lsrc_operand PARAMS ((rtx, enum machine_mode));

extern int tsrc_operand PARAMS ((rtx, enum machine_mode));

extern int addr_reg_operand PARAMS ((rtx, enum machine_mode));

extern int index_reg_operand PARAMS ((rtx, enum machine_mode));

extern int dp_reg_operand PARAMS ((rtx, enum machine_mode));

extern int sp_reg_operand PARAMS ((rtx, enum machine_mode));

extern int rc_reg_operand PARAMS ((rtx, enum machine_mode));

extern int st_reg_operand PARAMS ((rtx, enum machine_mode));

extern int symbolic_address_operand PARAMS ((rtx, enum machine_mode));

extern int ar0_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar0_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar1_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar1_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar2_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar2_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar3_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar3_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar4_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar4_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar5_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar5_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar6_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar6_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ar7_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ar7_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ir0_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ir0_mem_operand PARAMS ((rtx, enum machine_mode));

extern int ir1_reg_operand PARAMS ((rtx, enum machine_mode));

extern int ir1_mem_operand PARAMS ((rtx, enum machine_mode));

extern int group1_reg_operand PARAMS ((rtx, enum machine_mode));

extern int group1_mem_operand PARAMS ((rtx, enum machine_mode));

extern int arx_reg_operand PARAMS ((rtx, enum machine_mode));

extern int call_address_operand PARAMS ((rtx, enum machine_mode));

extern int par_ind_operand PARAMS ((rtx, enum machine_mode));

extern int not_rc_reg PARAMS ((rtx, enum machine_mode));

extern int not_modify_reg PARAMS ((rtx, enum machine_mode));

extern int c4x_shiftable_constant PARAMS ((rtx));

extern int c4x_H_constant PARAMS ((rtx));

extern int c4x_I_constant PARAMS ((rtx));

extern int c4x_J_constant PARAMS ((rtx));

extern int c4x_L_constant PARAMS ((rtx));

extern int c4x_Q_constraint PARAMS ((rtx));

extern int c4x_R_constraint PARAMS ((rtx));

extern int c4x_S_constraint PARAMS ((rtx));

extern int c4x_T_constraint PARAMS ((rtx));

extern int c4x_U_constraint PARAMS ((rtx));

extern void c4x_emit_libcall PARAMS ((rtx, enum rtx_code,
				      enum machine_mode,
				      enum machine_mode, int, rtx *));

extern void c4x_emit_libcall3 PARAMS ((rtx, enum rtx_code, 
				       enum machine_mode, rtx *));

extern void c4x_emit_libcall_mulhi PARAMS ((rtx, enum rtx_code,
					    enum machine_mode, rtx *));

extern int c4x_emit_move_sequence PARAMS ((rtx *, enum machine_mode));

extern int legitimize_operands PARAMS ((enum rtx_code, rtx *, 
					enum machine_mode));

extern int valid_operands PARAMS ((enum rtx_code, rtx *, enum machine_mode));

extern int valid_parallel_load_store PARAMS ((rtx *, enum machine_mode));

extern int valid_parallel_operands_4 PARAMS ((rtx *, enum machine_mode));

extern int valid_parallel_operands_5 PARAMS ((rtx *, enum machine_mode));

extern int valid_parallel_operands_6 PARAMS ((rtx *, enum machine_mode));

extern rtx smulhi3_libfunc;
extern rtx umulhi3_libfunc;
extern rtx fix_truncqfhi2_libfunc;
extern rtx fixuns_truncqfhi2_libfunc;
extern rtx fix_trunchfhi2_libfunc;
extern rtx fixuns_trunchfhi2_libfunc;
extern rtx floathiqf2_libfunc;
extern rtx floatunshiqf2_libfunc;
extern rtx floathihf2_libfunc;
extern rtx floatunshihf2_libfunc;

extern struct rtx_def *c4x_compare_op0;	/* Operand 0 for comparisons.  */
extern struct rtx_def *c4x_compare_op1;	/* Operand 1 for comparisons.  */

#endif /* RTX_CODE */

/* Smallest class containing REGNO.  */
extern enum reg_class c4x_regclass_map[FIRST_PSEUDO_REGISTER];
extern enum machine_mode c4x_caller_save_map[FIRST_PSEUDO_REGISTER];

extern int c4x_rpts_cycles;	        /* Max cycles for RPTS.  */
extern int c4x_cpu_version;		/* Cpu version C30/31/32/40/44.  */

#ifdef GCC_C_PRAGMA_H
extern void c4x_pr_CODE_SECTION		PARAMS ((cpp_reader *));
extern void c4x_pr_DATA_SECTION		PARAMS ((cpp_reader *));
extern void c4x_pr_FUNC_IS_PURE		PARAMS ((cpp_reader *));
extern void c4x_pr_FUNC_NEVER_RETURNS	PARAMS ((cpp_reader *));
extern void c4x_pr_INTERRUPT		PARAMS ((cpp_reader *));
extern void c4x_pr_ignored		PARAMS ((cpp_reader *));
extern void c4x_init_pragma		PARAMS ((int (*) (tree *)));
extern tree code_tree, data_tree, pure_tree, noreturn_tree, interrupt_tree;
#endif

#endif /* ! GCC_C4X_PROTOS_H */
