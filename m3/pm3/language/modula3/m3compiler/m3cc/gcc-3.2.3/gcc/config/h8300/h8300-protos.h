/* Definitions of target machine for GNU compiler. 
   Hitachi H8/300 version generating coff 
   Copyright (C) 2000 Free SoftwareFoundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

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

#ifndef GCC_H8300_PROTOS_H
#define GCC_H8300_PROTOS_H

/* Declarations for functions used in insn-output.c.  */
#ifdef RTX_CODE
extern const char *output_a_shift PARAMS ((rtx *));
extern const char *emit_a_rotate PARAMS ((enum rtx_code, rtx *));
extern const char *output_simode_bld PARAMS ((int, rtx[]));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern int const_costs PARAMS ((rtx, enum rtx_code, enum rtx_code));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void final_prescan_insn PARAMS ((rtx, rtx *, int));
extern int do_movsi PARAMS ((rtx[]));
extern void notice_update_cc PARAMS ((rtx, rtx));
extern const char *output_logical_op PARAMS ((enum machine_mode, int, rtx *));
extern int expand_a_shift PARAMS ((enum machine_mode, int, rtx[]));
extern int expand_a_rotate PARAMS ((enum rtx_code, rtx[]));
extern int fix_bit_operand PARAMS ((rtx *, int, enum rtx_code));
extern int h8300_adjust_insn_length PARAMS ((rtx, int));
extern void split_adds_subs PARAMS ((enum machine_mode, rtx[]));

extern int general_operand_src PARAMS ((rtx, enum machine_mode));
extern int general_operand_dst PARAMS ((rtx, enum machine_mode));
extern int o_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int two_insn_adds_subs_operand PARAMS ((rtx, enum machine_mode));
extern int small_call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int jump_address_operand PARAMS ((rtx, enum machine_mode));
extern int bit_operand PARAMS ((rtx, enum machine_mode));
extern int bit_memory_operand PARAMS ((rtx, enum machine_mode));
extern int bit_operator PARAMS ((rtx, enum machine_mode));
extern int nshift_operator PARAMS ((rtx, enum machine_mode));

/* Used in builtins.c */
extern rtx h8300_return_addr_rtx PARAMS ((int, rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern struct rtx_def *function_arg PARAMS ((CUMULATIVE_ARGS *,
					     enum machine_mode, tree, int));
extern int h8300_funcvec_function_p PARAMS ((tree));
extern int h8300_eightbit_data_p PARAMS ((tree));
extern int h8300_tiny_data_p PARAMS ((tree));
extern void h8300_encode_label PARAMS ((tree));
#endif /* TREE_CODE */

extern void h8300_init_once PARAMS ((void));
extern void asm_file_start PARAMS ((FILE *));
extern void asm_file_end PARAMS ((FILE *));
extern int ok_for_bclr PARAMS ((HOST_WIDE_INT));
extern int small_power_of_two PARAMS ((HOST_WIDE_INT));
extern int initial_offset PARAMS ((int, int));

#ifdef GCC_C_PRAGMA_H
extern void h8300_pr_interrupt PARAMS ((cpp_reader *));
extern void h8300_pr_saveall PARAMS ((cpp_reader *));
#endif

#endif /* ! GCC_H8300_PROTOS_H */
