/* Decode header for m32rxf.

THIS FILE IS MACHINE GENERATED WITH CGEN.

Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of the GNU Simulators.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#ifndef M32RXF_DECODE_H
#define M32RXF_DECODE_H

extern const IDESC *m32rxf_decode (SIM_CPU *, IADDR,
                                  CGEN_INSN_INT, CGEN_INSN_INT,
                                  ARGBUF *);
extern void m32rxf_init_idesc_table (SIM_CPU *);
extern void m32rxf_sem_init_idesc_table (SIM_CPU *);
extern void m32rxf_semf_init_idesc_table (SIM_CPU *);

/* Enum declaration for instructions in cpu family m32rxf.  */
typedef enum m32rxf_insn_type {
  M32RXF_INSN_X_INVALID, M32RXF_INSN_X_AFTER, M32RXF_INSN_X_BEFORE, M32RXF_INSN_X_CTI_CHAIN
 , M32RXF_INSN_X_CHAIN, M32RXF_INSN_X_BEGIN, M32RXF_INSN_ADD, M32RXF_INSN_ADD3
 , M32RXF_INSN_AND, M32RXF_INSN_AND3, M32RXF_INSN_OR, M32RXF_INSN_OR3
 , M32RXF_INSN_XOR, M32RXF_INSN_XOR3, M32RXF_INSN_ADDI, M32RXF_INSN_ADDV
 , M32RXF_INSN_ADDV3, M32RXF_INSN_ADDX, M32RXF_INSN_BC8, M32RXF_INSN_BC24
 , M32RXF_INSN_BEQ, M32RXF_INSN_BEQZ, M32RXF_INSN_BGEZ, M32RXF_INSN_BGTZ
 , M32RXF_INSN_BLEZ, M32RXF_INSN_BLTZ, M32RXF_INSN_BNEZ, M32RXF_INSN_BL8
 , M32RXF_INSN_BL24, M32RXF_INSN_BCL8, M32RXF_INSN_BCL24, M32RXF_INSN_BNC8
 , M32RXF_INSN_BNC24, M32RXF_INSN_BNE, M32RXF_INSN_BRA8, M32RXF_INSN_BRA24
 , M32RXF_INSN_BNCL8, M32RXF_INSN_BNCL24, M32RXF_INSN_CMP, M32RXF_INSN_CMPI
 , M32RXF_INSN_CMPU, M32RXF_INSN_CMPUI, M32RXF_INSN_CMPEQ, M32RXF_INSN_CMPZ
 , M32RXF_INSN_DIV, M32RXF_INSN_DIVU, M32RXF_INSN_REM, M32RXF_INSN_REMU
 , M32RXF_INSN_DIVH, M32RXF_INSN_JC, M32RXF_INSN_JNC, M32RXF_INSN_JL
 , M32RXF_INSN_JMP, M32RXF_INSN_LD, M32RXF_INSN_LD_D, M32RXF_INSN_LDB
 , M32RXF_INSN_LDB_D, M32RXF_INSN_LDH, M32RXF_INSN_LDH_D, M32RXF_INSN_LDUB
 , M32RXF_INSN_LDUB_D, M32RXF_INSN_LDUH, M32RXF_INSN_LDUH_D, M32RXF_INSN_LD_PLUS
 , M32RXF_INSN_LD24, M32RXF_INSN_LDI8, M32RXF_INSN_LDI16, M32RXF_INSN_LOCK
 , M32RXF_INSN_MACHI_A, M32RXF_INSN_MACLO_A, M32RXF_INSN_MACWHI_A, M32RXF_INSN_MACWLO_A
 , M32RXF_INSN_MUL, M32RXF_INSN_MULHI_A, M32RXF_INSN_MULLO_A, M32RXF_INSN_MULWHI_A
 , M32RXF_INSN_MULWLO_A, M32RXF_INSN_MV, M32RXF_INSN_MVFACHI_A, M32RXF_INSN_MVFACLO_A
 , M32RXF_INSN_MVFACMI_A, M32RXF_INSN_MVFC, M32RXF_INSN_MVTACHI_A, M32RXF_INSN_MVTACLO_A
 , M32RXF_INSN_MVTC, M32RXF_INSN_NEG, M32RXF_INSN_NOP, M32RXF_INSN_NOT
 , M32RXF_INSN_RAC_DSI, M32RXF_INSN_RACH_DSI, M32RXF_INSN_RTE, M32RXF_INSN_SETH
 , M32RXF_INSN_SLL, M32RXF_INSN_SLL3, M32RXF_INSN_SLLI, M32RXF_INSN_SRA
 , M32RXF_INSN_SRA3, M32RXF_INSN_SRAI, M32RXF_INSN_SRL, M32RXF_INSN_SRL3
 , M32RXF_INSN_SRLI, M32RXF_INSN_ST, M32RXF_INSN_ST_D, M32RXF_INSN_STB
 , M32RXF_INSN_STB_D, M32RXF_INSN_STH, M32RXF_INSN_STH_D, M32RXF_INSN_ST_PLUS
 , M32RXF_INSN_ST_MINUS, M32RXF_INSN_SUB, M32RXF_INSN_SUBV, M32RXF_INSN_SUBX
 , M32RXF_INSN_TRAP, M32RXF_INSN_UNLOCK, M32RXF_INSN_SATB, M32RXF_INSN_SATH
 , M32RXF_INSN_SAT, M32RXF_INSN_PCMPBZ, M32RXF_INSN_SADD, M32RXF_INSN_MACWU1
 , M32RXF_INSN_MSBLO, M32RXF_INSN_MULWU1, M32RXF_INSN_MACLH1, M32RXF_INSN_SC
 , M32RXF_INSN_SNC, M32RXF_INSN_PAR_ADD, M32RXF_INSN_WRITE_ADD, M32RXF_INSN_PAR_AND
 , M32RXF_INSN_WRITE_AND, M32RXF_INSN_PAR_OR, M32RXF_INSN_WRITE_OR, M32RXF_INSN_PAR_XOR
 , M32RXF_INSN_WRITE_XOR, M32RXF_INSN_PAR_ADDI, M32RXF_INSN_WRITE_ADDI, M32RXF_INSN_PAR_ADDV
 , M32RXF_INSN_WRITE_ADDV, M32RXF_INSN_PAR_ADDX, M32RXF_INSN_WRITE_ADDX, M32RXF_INSN_PAR_BC8
 , M32RXF_INSN_WRITE_BC8, M32RXF_INSN_PAR_BL8, M32RXF_INSN_WRITE_BL8, M32RXF_INSN_PAR_BCL8
 , M32RXF_INSN_WRITE_BCL8, M32RXF_INSN_PAR_BNC8, M32RXF_INSN_WRITE_BNC8, M32RXF_INSN_PAR_BRA8
 , M32RXF_INSN_WRITE_BRA8, M32RXF_INSN_PAR_BNCL8, M32RXF_INSN_WRITE_BNCL8, M32RXF_INSN_PAR_CMP
 , M32RXF_INSN_WRITE_CMP, M32RXF_INSN_PAR_CMPU, M32RXF_INSN_WRITE_CMPU, M32RXF_INSN_PAR_CMPEQ
 , M32RXF_INSN_WRITE_CMPEQ, M32RXF_INSN_PAR_CMPZ, M32RXF_INSN_WRITE_CMPZ, M32RXF_INSN_PAR_JC
 , M32RXF_INSN_WRITE_JC, M32RXF_INSN_PAR_JNC, M32RXF_INSN_WRITE_JNC, M32RXF_INSN_PAR_JL
 , M32RXF_INSN_WRITE_JL, M32RXF_INSN_PAR_JMP, M32RXF_INSN_WRITE_JMP, M32RXF_INSN_PAR_LD
 , M32RXF_INSN_WRITE_LD, M32RXF_INSN_PAR_LDB, M32RXF_INSN_WRITE_LDB, M32RXF_INSN_PAR_LDH
 , M32RXF_INSN_WRITE_LDH, M32RXF_INSN_PAR_LDUB, M32RXF_INSN_WRITE_LDUB, M32RXF_INSN_PAR_LDUH
 , M32RXF_INSN_WRITE_LDUH, M32RXF_INSN_PAR_LD_PLUS, M32RXF_INSN_WRITE_LD_PLUS, M32RXF_INSN_PAR_LDI8
 , M32RXF_INSN_WRITE_LDI8, M32RXF_INSN_PAR_LOCK, M32RXF_INSN_WRITE_LOCK, M32RXF_INSN_PAR_MACHI_A
 , M32RXF_INSN_WRITE_MACHI_A, M32RXF_INSN_PAR_MACLO_A, M32RXF_INSN_WRITE_MACLO_A, M32RXF_INSN_PAR_MACWHI_A
 , M32RXF_INSN_WRITE_MACWHI_A, M32RXF_INSN_PAR_MACWLO_A, M32RXF_INSN_WRITE_MACWLO_A, M32RXF_INSN_PAR_MUL
 , M32RXF_INSN_WRITE_MUL, M32RXF_INSN_PAR_MULHI_A, M32RXF_INSN_WRITE_MULHI_A, M32RXF_INSN_PAR_MULLO_A
 , M32RXF_INSN_WRITE_MULLO_A, M32RXF_INSN_PAR_MULWHI_A, M32RXF_INSN_WRITE_MULWHI_A, M32RXF_INSN_PAR_MULWLO_A
 , M32RXF_INSN_WRITE_MULWLO_A, M32RXF_INSN_PAR_MV, M32RXF_INSN_WRITE_MV, M32RXF_INSN_PAR_MVFACHI_A
 , M32RXF_INSN_WRITE_MVFACHI_A, M32RXF_INSN_PAR_MVFACLO_A, M32RXF_INSN_WRITE_MVFACLO_A, M32RXF_INSN_PAR_MVFACMI_A
 , M32RXF_INSN_WRITE_MVFACMI_A, M32RXF_INSN_PAR_MVFC, M32RXF_INSN_WRITE_MVFC, M32RXF_INSN_PAR_MVTACHI_A
 , M32RXF_INSN_WRITE_MVTACHI_A, M32RXF_INSN_PAR_MVTACLO_A, M32RXF_INSN_WRITE_MVTACLO_A, M32RXF_INSN_PAR_MVTC
 , M32RXF_INSN_WRITE_MVTC, M32RXF_INSN_PAR_NEG, M32RXF_INSN_WRITE_NEG, M32RXF_INSN_PAR_NOP
 , M32RXF_INSN_WRITE_NOP, M32RXF_INSN_PAR_NOT, M32RXF_INSN_WRITE_NOT, M32RXF_INSN_PAR_RAC_DSI
 , M32RXF_INSN_WRITE_RAC_DSI, M32RXF_INSN_PAR_RACH_DSI, M32RXF_INSN_WRITE_RACH_DSI, M32RXF_INSN_PAR_RTE
 , M32RXF_INSN_WRITE_RTE, M32RXF_INSN_PAR_SLL, M32RXF_INSN_WRITE_SLL, M32RXF_INSN_PAR_SLLI
 , M32RXF_INSN_WRITE_SLLI, M32RXF_INSN_PAR_SRA, M32RXF_INSN_WRITE_SRA, M32RXF_INSN_PAR_SRAI
 , M32RXF_INSN_WRITE_SRAI, M32RXF_INSN_PAR_SRL, M32RXF_INSN_WRITE_SRL, M32RXF_INSN_PAR_SRLI
 , M32RXF_INSN_WRITE_SRLI, M32RXF_INSN_PAR_ST, M32RXF_INSN_WRITE_ST, M32RXF_INSN_PAR_STB
 , M32RXF_INSN_WRITE_STB, M32RXF_INSN_PAR_STH, M32RXF_INSN_WRITE_STH, M32RXF_INSN_PAR_ST_PLUS
 , M32RXF_INSN_WRITE_ST_PLUS, M32RXF_INSN_PAR_ST_MINUS, M32RXF_INSN_WRITE_ST_MINUS, M32RXF_INSN_PAR_SUB
 , M32RXF_INSN_WRITE_SUB, M32RXF_INSN_PAR_SUBV, M32RXF_INSN_WRITE_SUBV, M32RXF_INSN_PAR_SUBX
 , M32RXF_INSN_WRITE_SUBX, M32RXF_INSN_PAR_TRAP, M32RXF_INSN_WRITE_TRAP, M32RXF_INSN_PAR_UNLOCK
 , M32RXF_INSN_WRITE_UNLOCK, M32RXF_INSN_PAR_PCMPBZ, M32RXF_INSN_WRITE_PCMPBZ, M32RXF_INSN_PAR_SADD
 , M32RXF_INSN_WRITE_SADD, M32RXF_INSN_PAR_MACWU1, M32RXF_INSN_WRITE_MACWU1, M32RXF_INSN_PAR_MSBLO
 , M32RXF_INSN_WRITE_MSBLO, M32RXF_INSN_PAR_MULWU1, M32RXF_INSN_WRITE_MULWU1, M32RXF_INSN_PAR_MACLH1
 , M32RXF_INSN_WRITE_MACLH1, M32RXF_INSN_PAR_SC, M32RXF_INSN_WRITE_SC, M32RXF_INSN_PAR_SNC
 , M32RXF_INSN_WRITE_SNC, M32RXF_INSN_MAX
} M32RXF_INSN_TYPE;

/* Enum declaration for semantic formats in cpu family m32rxf.  */
typedef enum m32rxf_sfmt_type {
  M32RXF_SFMT_EMPTY, M32RXF_SFMT_ADD, M32RXF_SFMT_ADD3, M32RXF_SFMT_AND3
 , M32RXF_SFMT_OR3, M32RXF_SFMT_ADDI, M32RXF_SFMT_ADDV, M32RXF_SFMT_ADDV3
 , M32RXF_SFMT_ADDX, M32RXF_SFMT_BC8, M32RXF_SFMT_BC24, M32RXF_SFMT_BEQ
 , M32RXF_SFMT_BEQZ, M32RXF_SFMT_BL8, M32RXF_SFMT_BL24, M32RXF_SFMT_BCL8
 , M32RXF_SFMT_BCL24, M32RXF_SFMT_BRA8, M32RXF_SFMT_BRA24, M32RXF_SFMT_CMP
 , M32RXF_SFMT_CMPI, M32RXF_SFMT_CMPZ, M32RXF_SFMT_DIV, M32RXF_SFMT_JC
 , M32RXF_SFMT_JL, M32RXF_SFMT_JMP, M32RXF_SFMT_LD, M32RXF_SFMT_LD_D
 , M32RXF_SFMT_LD_PLUS, M32RXF_SFMT_LD24, M32RXF_SFMT_LDI8, M32RXF_SFMT_LDI16
 , M32RXF_SFMT_LOCK, M32RXF_SFMT_MACHI_A, M32RXF_SFMT_MULHI_A, M32RXF_SFMT_MV
 , M32RXF_SFMT_MVFACHI_A, M32RXF_SFMT_MVFC, M32RXF_SFMT_MVTACHI_A, M32RXF_SFMT_MVTC
 , M32RXF_SFMT_NOP, M32RXF_SFMT_RAC_DSI, M32RXF_SFMT_RTE, M32RXF_SFMT_SETH
 , M32RXF_SFMT_SLL3, M32RXF_SFMT_SLLI, M32RXF_SFMT_ST, M32RXF_SFMT_ST_D
 , M32RXF_SFMT_STB, M32RXF_SFMT_STB_D, M32RXF_SFMT_STH, M32RXF_SFMT_STH_D
 , M32RXF_SFMT_ST_PLUS, M32RXF_SFMT_TRAP, M32RXF_SFMT_UNLOCK, M32RXF_SFMT_SATB
 , M32RXF_SFMT_SAT, M32RXF_SFMT_SADD, M32RXF_SFMT_MACWU1, M32RXF_SFMT_MSBLO
 , M32RXF_SFMT_MULWU1, M32RXF_SFMT_SC
} M32RXF_SFMT_TYPE;

/* Function unit handlers (user written).  */

extern int m32rxf_model_m32rx_u_store (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*src1*/, INT /*src2*/);
extern int m32rxf_model_m32rx_u_load (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*sr*/, INT /*dr*/);
extern int m32rxf_model_m32rx_u_cti (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*sr*/);
extern int m32rxf_model_m32rx_u_mac (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*src1*/, INT /*src2*/);
extern int m32rxf_model_m32rx_u_cmp (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*src1*/, INT /*src2*/);
extern int m32rxf_model_m32rx_u_exec (SIM_CPU *, const IDESC *, int /*unit_num*/, int /*referenced*/, INT /*sr*/, INT /*dr*/, INT /*dr*/);

/* Profiling before/after handlers (user written) */

extern void m32rxf_model_insn_before (SIM_CPU *, int /*first_p*/);
extern void m32rxf_model_insn_after (SIM_CPU *, int /*last_p*/, int /*cycles*/);

#endif /* M32RXF_DECODE_H */
