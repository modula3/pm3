/* Simulator instruction semantics for m32r.

Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

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

#ifdef DEFINE_LABELS
#undef DEFINE_LABELS


  /* The labels have the case they have because the enum of insn types
     is all uppercase and in the non-stdc case the insn symbol is built
     into the enum name.

     The order here must match the order in m32r_decode_vars in decode.c.  */

  static void *labels[] = {
    && case_sem_INSN_ILLEGAL,
    && case_sem_INSN_ADD,
    && case_sem_INSN_ADD3,
    && case_sem_INSN_AND,
    && case_sem_INSN_AND3,
    && case_sem_INSN_OR,
    && case_sem_INSN_OR3,
    && case_sem_INSN_XOR,
    && case_sem_INSN_XOR3,
    && case_sem_INSN_ADDI,
    && case_sem_INSN_ADDV,
    && case_sem_INSN_ADDV3,
    && case_sem_INSN_ADDX,
    && case_sem_INSN_BC8,
    && case_sem_INSN_BC24,
    && case_sem_INSN_BEQ,
    && case_sem_INSN_BEQZ,
    && case_sem_INSN_BGEZ,
    && case_sem_INSN_BGTZ,
    && case_sem_INSN_BLEZ,
    && case_sem_INSN_BLTZ,
    && case_sem_INSN_BNEZ,
    && case_sem_INSN_BL8,
    && case_sem_INSN_BL24,
    && case_sem_INSN_BNC8,
    && case_sem_INSN_BNC24,
    && case_sem_INSN_BNE,
    && case_sem_INSN_BRA8,
    && case_sem_INSN_BRA24,
    && case_sem_INSN_CMP,
    && case_sem_INSN_CMPI,
    && case_sem_INSN_CMPU,
    && case_sem_INSN_CMPUI,
    && case_sem_INSN_DIV,
    && case_sem_INSN_DIVU,
    && case_sem_INSN_REM,
    && case_sem_INSN_REMU,
    && case_sem_INSN_JL,
    && case_sem_INSN_JMP,
    && case_sem_INSN_LD,
    && case_sem_INSN_LD_D,
    && case_sem_INSN_LDB,
    && case_sem_INSN_LDB_D,
    && case_sem_INSN_LDH,
    && case_sem_INSN_LDH_D,
    && case_sem_INSN_LDUB,
    && case_sem_INSN_LDUB_D,
    && case_sem_INSN_LDUH,
    && case_sem_INSN_LDUH_D,
    && case_sem_INSN_LD_PLUS,
    && case_sem_INSN_LD24,
    && case_sem_INSN_LDI8,
    && case_sem_INSN_LDI16,
    && case_sem_INSN_LOCK,
    && case_sem_INSN_MACHI,
    && case_sem_INSN_MACLO,
    && case_sem_INSN_MACWHI,
    && case_sem_INSN_MACWLO,
    && case_sem_INSN_MUL,
    && case_sem_INSN_MULHI,
    && case_sem_INSN_MULLO,
    && case_sem_INSN_MULWHI,
    && case_sem_INSN_MULWLO,
    && case_sem_INSN_MV,
    && case_sem_INSN_MVFACHI,
    && case_sem_INSN_MVFACLO,
    && case_sem_INSN_MVFACMI,
    && case_sem_INSN_MVFC,
    && case_sem_INSN_MVTACHI,
    && case_sem_INSN_MVTACLO,
    && case_sem_INSN_MVTC,
    && case_sem_INSN_NEG,
    && case_sem_INSN_NOP,
    && case_sem_INSN_NOT,
    && case_sem_INSN_RAC,
    && case_sem_INSN_RACH,
    && case_sem_INSN_RTE,
    && case_sem_INSN_SETH,
    && case_sem_INSN_SLL,
    && case_sem_INSN_SLL3,
    && case_sem_INSN_SLLI,
    && case_sem_INSN_SRA,
    && case_sem_INSN_SRA3,
    && case_sem_INSN_SRAI,
    && case_sem_INSN_SRL,
    && case_sem_INSN_SRL3,
    && case_sem_INSN_SRLI,
    && case_sem_INSN_ST,
    && case_sem_INSN_ST_D,
    && case_sem_INSN_STB,
    && case_sem_INSN_STB_D,
    && case_sem_INSN_STH,
    && case_sem_INSN_STH_D,
    && case_sem_INSN_ST_PLUS,
    && case_sem_INSN_ST_MINUS,
    && case_sem_INSN_SUB,
    && case_sem_INSN_SUBV,
    && case_sem_INSN_SUBX,
    && case_sem_INSN_TRAP,
    && case_sem_INSN_UNLOCK,
    0
  };
  extern DECODE *m32r_decode_vars[];
  int i;

  for (i = 0; m32r_decode_vars[i] != 0; ++i)
    m32r_decode_vars[i]->semantic_lab = labels[i];

#endif /* DEFINE_LABELS */

#ifdef DEFINE_SWITCH
#undef DEFINE_SWITCH

/* If hyper-fast [well not unnecessarily slow] execution is selected, turn
   off frills like tracing and profiling.  */
/* FIXME: A better way would be to have TRACE_RESULT check for something
   that can cause it to be optimized out.  */

#if FAST_P
#undef TRACE_RESULT
#define TRACE_RESULT(cpu, name, type, val)
#endif

#undef GET_ATTR
#define GET_ATTR(cpu, num, attr) CGEN_INSN_ATTR (abuf->opcode, CGEN_INSN_##attr)

{
  SEM_ARG sem_arg = sc;
  ARGBUF *abuf = SEM_ARGBUF (sem_arg);
  CIA new_pc = SEM_NEXT_PC (sem_arg);

  SWITCH (sem, sem_arg->semantic.sem_case)
    {

  CASE (sem, INSN_ILLEGAL) :
{
  sim_engine_halt (CPU_STATE (current_cpu), current_cpu, NULL, NULL_CIA/*FIXME*/,
		   sim_stopped, SIM_SIGILL);
  BREAK (sem);
}

  CASE (sem, INSN_ADD) : /* add $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = ADDSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ADD3) : /* add3 $dr,$sr,#$slo16 */
{
#define FLD(f) abuf->fields.fmt_1_add3.f
* FLD (f_r1) = ADDSI (* FLD (f_r2), FLD (f_simm16));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_AND) : /* and $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = ANDSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_AND3) : /* and3 $dr,$sr,#$uimm16 */
{
#define FLD(f) abuf->fields.fmt_2_and3.f
* FLD (f_r1) = ANDSI (* FLD (f_r2), FLD (f_uimm16));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_OR) : /* or $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = ORSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_OR3) : /* or3 $dr,$sr,#$ulo16 */
{
#define FLD(f) abuf->fields.fmt_3_or3.f
* FLD (f_r1) = ORSI (* FLD (f_r2), FLD (f_uimm16));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_XOR) : /* xor $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = XORSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_XOR3) : /* xor3 $dr,$sr,#$uimm16 */
{
#define FLD(f) abuf->fields.fmt_2_and3.f
* FLD (f_r1) = XORSI (* FLD (f_r2), FLD (f_uimm16));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ADDI) : /* addi $dr,#$simm8 */
{
#define FLD(f) abuf->fields.fmt_4_addi.f
* FLD (f_r1) = ADDSI (* FLD (f_r1), FLD (f_simm8));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ADDV) : /* addv $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
do {
  BI temp1;SI temp0;
  temp0 = ADDSI (* FLD (f_r1), * FLD (f_r2));
  temp1 = ADDOFSI (* FLD (f_r1), * FLD (f_r2), 0);
* FLD (f_r1) = temp0;
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
  CPU (h_cond) = temp1;
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ADDV3) : /* addv3 $dr,$sr,#$simm16 */
{
#define FLD(f) abuf->fields.fmt_5_addv3.f
do {
  BI temp1;SI temp0;
  temp0 = ADDSI (* FLD (f_r2), FLD (f_simm16));
  temp1 = ADDOFSI (* FLD (f_r2), FLD (f_simm16), 0);
* FLD (f_r1) = temp0;
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
  CPU (h_cond) = temp1;
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ADDX) : /* addx $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_6_addx.f
do {
  BI temp1;SI temp0;
  temp0 = ADDCSI (* FLD (f_r1), * FLD (f_r2), CPU (h_cond));
  temp1 = ADDCFSI (* FLD (f_r1), * FLD (f_r2), CPU (h_cond));
* FLD (f_r1) = temp0;
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
  CPU (h_cond) = temp1;
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BC8) : /* bc $disp8 */
{
#define FLD(f) abuf->fields.fmt_7_bc8.f
if (CPU (h_cond)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp8)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BC24) : /* bc $disp24 */
{
#define FLD(f) abuf->fields.fmt_8_bc24.f
if (CPU (h_cond)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp24)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BEQ) : /* beq $src1,$src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_9_beq.f
if (EQSI (* FLD (f_r1), * FLD (f_r2))) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BEQZ) : /* beqz $src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_10_beqz.f
if (EQSI (* FLD (f_r2), 0)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BGEZ) : /* bgez $src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_10_beqz.f
if (GESI (* FLD (f_r2), 0)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BGTZ) : /* bgtz $src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_10_beqz.f
if (GTSI (* FLD (f_r2), 0)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BLEZ) : /* blez $src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_10_beqz.f
if (LESI (* FLD (f_r2), 0)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BLTZ) : /* bltz $src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_10_beqz.f
if (LTSI (* FLD (f_r2), 0)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BNEZ) : /* bnez $src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_10_beqz.f
if (NESI (* FLD (f_r2), 0)) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BL8) : /* bl $disp8 */
{
#define FLD(f) abuf->fields.fmt_11_bl8.f
do {
  CPU (h_gr[14]) = ADDSI (ANDSI (CPU (h_pc), -4), 4);
  TRACE_RESULT (current_cpu, "h-gr", 'x', CPU (h_gr[14]));
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp8)));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BL24) : /* bl $disp24 */
{
#define FLD(f) abuf->fields.fmt_12_bl24.f
do {
  CPU (h_gr[14]) = ADDSI (CPU (h_pc), 4);
  TRACE_RESULT (current_cpu, "h-gr", 'x', CPU (h_gr[14]));
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp24)));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BNC8) : /* bnc $disp8 */
{
#define FLD(f) abuf->fields.fmt_7_bc8.f
if (NOTBI (CPU (h_cond))) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp8)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BNC24) : /* bnc $disp24 */
{
#define FLD(f) abuf->fields.fmt_8_bc24.f
if (NOTBI (CPU (h_cond))) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp24)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BNE) : /* bne $src1,$src2,$disp16 */
{
#define FLD(f) abuf->fields.fmt_9_beq.f
if (NESI (* FLD (f_r1), * FLD (f_r2))) {
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp16)));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BRA8) : /* bra $disp8 */
{
#define FLD(f) abuf->fields.fmt_13_bra8.f
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp8)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_BRA24) : /* bra $disp24 */
{
#define FLD(f) abuf->fields.fmt_14_bra24.f
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_CACHE (sem_arg, FLD (f_disp24)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_CMP) : /* cmp $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
  CPU (h_cond) = LTSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_CMPI) : /* cmpi $src2,#$simm16 */
{
#define FLD(f) abuf->fields.fmt_16_cmpi.f
  CPU (h_cond) = LTSI (* FLD (f_r2), FLD (f_simm16));
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_CMPU) : /* cmpu $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
  CPU (h_cond) = LTUSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_CMPUI) : /* cmpui $src2,#$uimm16 */
{
#define FLD(f) abuf->fields.fmt_17_cmpui.f
  CPU (h_cond) = LTUSI (* FLD (f_r2), FLD (f_uimm16));
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_DIV) : /* div $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_18_div.f
if (NESI (* FLD (f_r2), 0)) {
* FLD (f_r1) = DIVSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_DIVU) : /* divu $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_18_div.f
if (NESI (* FLD (f_r2), 0)) {
* FLD (f_r1) = UDIVSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_REM) : /* rem $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_18_div.f
if (NESI (* FLD (f_r2), 0)) {
* FLD (f_r1) = MODSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_REMU) : /* remu $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_18_div.f
if (NESI (* FLD (f_r2), 0)) {
* FLD (f_r1) = UMODSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
}
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_JL) : /* jl $sr */
{
#define FLD(f) abuf->fields.fmt_19_jl.f
do {
  USI temp1;SI temp0;
  temp0 = ADDSI (ANDSI (CPU (h_pc), -4), 4);
  temp1 = * FLD (f_r2);
  CPU (h_gr[14]) = temp0;
  TRACE_RESULT (current_cpu, "h-gr", 'x', CPU (h_gr[14]));
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_ADDR (sem_arg, temp1));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_JMP) : /* jmp $sr */
{
#define FLD(f) abuf->fields.fmt_20_jmp.f
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_ADDR (sem_arg, * FLD (f_r2)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LD) : /* ld $dr,@$sr */
{
#define FLD(f) abuf->fields.fmt_21_ld.f
* FLD (f_r1) = GETMEMSI (current_cpu, * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LD_D) : /* ld $dr,@($slo16,$sr) */
{
#define FLD(f) abuf->fields.fmt_22_ld_d.f
* FLD (f_r1) = GETMEMSI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16)));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDB) : /* ldb $dr,@$sr */
{
#define FLD(f) abuf->fields.fmt_23_ldb.f
* FLD (f_r1) = EXTQISI (GETMEMQI (current_cpu, * FLD (f_r2)));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDB_D) : /* ldb $dr,@($slo16,$sr) */
{
#define FLD(f) abuf->fields.fmt_24_ldb_d.f
* FLD (f_r1) = EXTQISI (GETMEMQI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDH) : /* ldh $dr,@$sr */
{
#define FLD(f) abuf->fields.fmt_25_ldh.f
* FLD (f_r1) = EXTHISI (GETMEMHI (current_cpu, * FLD (f_r2)));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDH_D) : /* ldh $dr,@($slo16,$sr) */
{
#define FLD(f) abuf->fields.fmt_26_ldh_d.f
* FLD (f_r1) = EXTHISI (GETMEMHI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDUB) : /* ldub $dr,@$sr */
{
#define FLD(f) abuf->fields.fmt_23_ldb.f
* FLD (f_r1) = ZEXTQISI (GETMEMQI (current_cpu, * FLD (f_r2)));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDUB_D) : /* ldub $dr,@($slo16,$sr) */
{
#define FLD(f) abuf->fields.fmt_24_ldb_d.f
* FLD (f_r1) = ZEXTQISI (GETMEMQI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDUH) : /* lduh $dr,@$sr */
{
#define FLD(f) abuf->fields.fmt_25_ldh.f
* FLD (f_r1) = ZEXTHISI (GETMEMHI (current_cpu, * FLD (f_r2)));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDUH_D) : /* lduh $dr,@($slo16,$sr) */
{
#define FLD(f) abuf->fields.fmt_26_ldh_d.f
* FLD (f_r1) = ZEXTHISI (GETMEMHI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LD_PLUS) : /* ld $dr,@$sr+ */
{
#define FLD(f) abuf->fields.fmt_21_ld.f
do {
  SI temp1;SI temp0;
  temp0 = GETMEMSI (current_cpu, * FLD (f_r2));
  temp1 = ADDSI (* FLD (f_r2), 4);
* FLD (f_r1) = temp0;
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
* FLD (f_r2) = temp1;
  TRACE_RESULT (current_cpu, "sr", 'x', * FLD (f_r2));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LD24) : /* ld24 $dr,#$uimm24 */
{
#define FLD(f) abuf->fields.fmt_27_ld24.f
* FLD (f_r1) = FLD (f_uimm24);
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDI8) : /* ldi $dr,#$simm8 */
{
#define FLD(f) abuf->fields.fmt_28_ldi8.f
* FLD (f_r1) = FLD (f_simm8);
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LDI16) : /* ldi $dr,$slo16 */
{
#define FLD(f) abuf->fields.fmt_29_ldi16.f
* FLD (f_r1) = FLD (f_simm16);
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_LOCK) : /* lock $dr,@$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
do_lock (current_cpu, * FLD (f_r1), * FLD (f_r2));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MACHI) : /* machi $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_30_machi.f
  CPU (h_accum) = SRADI (SLLDI (ADDDI (CPU (h_accum), MULDI (EXTSIDI (ANDSI (* FLD (f_r1), 0xffff0000)), EXTHIDI (TRUNCSIHI (SRASI (* FLD (f_r2), 16))))), 8), 8);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MACLO) : /* maclo $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_30_machi.f
  CPU (h_accum) = SRADI (SLLDI (ADDDI (CPU (h_accum), MULDI (EXTSIDI (SLLSI (* FLD (f_r1), 16)), EXTHIDI (TRUNCSIHI (* FLD (f_r2))))), 8), 8);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MACWHI) : /* macwhi $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_30_machi.f
  CPU (h_accum) = SRADI (SLLDI (ADDDI (CPU (h_accum), MULDI (EXTSIDI (* FLD (f_r1)), EXTHIDI (TRUNCSIHI (SRASI (* FLD (f_r2), 16))))), 8), 8);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MACWLO) : /* macwlo $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_30_machi.f
  CPU (h_accum) = SRADI (SLLDI (ADDDI (CPU (h_accum), MULDI (EXTSIDI (* FLD (f_r1)), EXTHIDI (TRUNCSIHI (* FLD (f_r2))))), 8), 8);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MUL) : /* mul $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = MULSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MULHI) : /* mulhi $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
  CPU (h_accum) = SRADI (SLLDI (MULDI (EXTSIDI (ANDSI (* FLD (f_r1), 0xffff0000)), EXTHIDI (TRUNCSIHI (SRASI (* FLD (f_r2), 16)))), 16), 16);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MULLO) : /* mullo $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
  CPU (h_accum) = SRADI (SLLDI (MULDI (EXTSIDI (SLLSI (* FLD (f_r1), 16)), EXTHIDI (TRUNCSIHI (* FLD (f_r2)))), 16), 16);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MULWHI) : /* mulwhi $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
  CPU (h_accum) = SRADI (SLLDI (MULDI (EXTSIDI (* FLD (f_r1)), EXTHIDI (TRUNCSIHI (SRASI (* FLD (f_r2), 16)))), 8), 8);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MULWLO) : /* mulwlo $src1,$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
  CPU (h_accum) = SRADI (SLLDI (MULDI (EXTSIDI (* FLD (f_r1)), EXTHIDI (TRUNCSIHI (* FLD (f_r2)))), 8), 8);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MV) : /* mv $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_31_mv.f
* FLD (f_r1) = * FLD (f_r2);
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVFACHI) : /* mvfachi $dr */
{
#define FLD(f) abuf->fields.fmt_32_mvfachi.f
* FLD (f_r1) = TRUNCDISI (SRADI (CPU (h_accum), 32));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVFACLO) : /* mvfaclo $dr */
{
#define FLD(f) abuf->fields.fmt_32_mvfachi.f
* FLD (f_r1) = TRUNCDISI (CPU (h_accum));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVFACMI) : /* mvfacmi $dr */
{
#define FLD(f) abuf->fields.fmt_32_mvfachi.f
* FLD (f_r1) = TRUNCDISI (SRADI (CPU (h_accum), 16));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVFC) : /* mvfc $dr,$scr */
{
#define FLD(f) abuf->fields.fmt_33_mvfc.f
* FLD (f_r1) = m32r_h_cr_get (current_cpu, FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVTACHI) : /* mvtachi $src1 */
{
#define FLD(f) abuf->fields.fmt_34_mvtachi.f
  CPU (h_accum) = ORDI (ANDDI (CPU (h_accum), MAKEDI (0, 0xffffffff)), SLLDI (EXTSIDI (* FLD (f_r1)), 32));
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVTACLO) : /* mvtaclo $src1 */
{
#define FLD(f) abuf->fields.fmt_34_mvtachi.f
  CPU (h_accum) = ORDI (ANDDI (CPU (h_accum), MAKEDI (0xffffffff, 0)), EXTSIDI (* FLD (f_r1)));
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_MVTC) : /* mvtc $sr,$dcr */
{
#define FLD(f) abuf->fields.fmt_35_mvtc.f
m32r_h_cr_set (current_cpu, FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dcr", 'x', m32r_h_cr_get (current_cpu, FLD (f_r1)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_NEG) : /* neg $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_31_mv.f
* FLD (f_r1) = NEGSI (* FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_NOP) : /* nop */
{
#define FLD(f) abuf->fields.fmt_36_nop.f
PROFILE_COUNT_FILLNOPS (current_cpu, abuf->addr);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_NOT) : /* not $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_31_mv.f
* FLD (f_r1) = INVSI (* FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_RAC) : /* rac */
{
#define FLD(f) abuf->fields.fmt_37_rac.f
do {
  DI tmp_tmp1;
  tmp_tmp1 = ANDDI (CPU (h_accum), MAKEDI (16777215, 0xffffffff));
if (ANDIFSI (GEDI (tmp_tmp1, MAKEDI (16383, 0xffff8000)), LEDI (tmp_tmp1, MAKEDI (8388607, 0xffffffff)))) {
  tmp_tmp1 = MAKEDI (16383, 0xffff8000);
} else {
if (ANDIFSI (GEDI (tmp_tmp1, MAKEDI (8388608, 0)), LEDI (tmp_tmp1, MAKEDI (16760832, 0)))) {
  tmp_tmp1 = MAKEDI (16760832, 0);
} else {
  tmp_tmp1 = ANDDI (ADDDI (CPU (h_accum), MAKEDI (0, 16384)), MAKEDI (16777215, 0xffff8000));
}
}
  tmp_tmp1 = SLLDI (tmp_tmp1, 1);
  CPU (h_accum) = SRADI (SLLDI (tmp_tmp1, 7), 7);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_RACH) : /* rach */
{
#define FLD(f) abuf->fields.fmt_37_rac.f
do {
  DI tmp_tmp1;
  tmp_tmp1 = ANDDI (CPU (h_accum), MAKEDI (16777215, 0xffffffff));
if (ANDIFSI (GEDI (tmp_tmp1, MAKEDI (16383, 0x80000000)), LEDI (tmp_tmp1, MAKEDI (8388607, 0xffffffff)))) {
  tmp_tmp1 = MAKEDI (16383, 0x80000000);
} else {
if (ANDIFSI (GEDI (tmp_tmp1, MAKEDI (8388608, 0)), LEDI (tmp_tmp1, MAKEDI (16760832, 0)))) {
  tmp_tmp1 = MAKEDI (16760832, 0);
} else {
  tmp_tmp1 = ANDDI (ADDDI (CPU (h_accum), MAKEDI (0, 1073741824)), MAKEDI (0xffffffff, 0x80000000));
}
}
  tmp_tmp1 = SLLDI (tmp_tmp1, 1);
  CPU (h_accum) = SRADI (SLLDI (tmp_tmp1, 7), 7);
  TRACE_RESULT (current_cpu, "accum", 'D', CPU (h_accum));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_RTE) : /* rte */
{
#define FLD(f) abuf->fields.fmt_36_nop.f
do {
  CPU (h_sm) = CPU (h_bsm);
  TRACE_RESULT (current_cpu, "h-sm", 'x', CPU (h_sm));
  CPU (h_ie) = CPU (h_bie);
  TRACE_RESULT (current_cpu, "h-ie", 'x', CPU (h_ie));
  CPU (h_cond) = CPU (h_bcond);
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
  BRANCH_NEW_PC (current_cpu, new_pc, SEM_BRANCH_VIA_ADDR (sem_arg, CPU (h_bpc)));
  TRACE_RESULT (current_cpu, "pc", 'x', CPU (h_pc));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SETH) : /* seth $dr,$hi16 */
{
#define FLD(f) abuf->fields.fmt_38_seth.f
* FLD (f_r1) = SLLSI (FLD (f_hi16), 16);
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SLL) : /* sll $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = SLLSI (* FLD (f_r1), ANDSI (* FLD (f_r2), 31));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SLL3) : /* sll3 $dr,$sr,#$simm16 */
{
#define FLD(f) abuf->fields.fmt_5_addv3.f
* FLD (f_r1) = SLLSI (* FLD (f_r2), ANDSI (FLD (f_simm16), 31));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SLLI) : /* slli $dr,#$uimm5 */
{
#define FLD(f) abuf->fields.fmt_39_slli.f
* FLD (f_r1) = SLLSI (* FLD (f_r1), FLD (f_uimm5));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SRA) : /* sra $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = SRASI (* FLD (f_r1), ANDSI (* FLD (f_r2), 31));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SRA3) : /* sra3 $dr,$sr,#$simm16 */
{
#define FLD(f) abuf->fields.fmt_5_addv3.f
* FLD (f_r1) = SRASI (* FLD (f_r2), ANDSI (FLD (f_simm16), 31));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SRAI) : /* srai $dr,#$uimm5 */
{
#define FLD(f) abuf->fields.fmt_39_slli.f
* FLD (f_r1) = SRASI (* FLD (f_r1), FLD (f_uimm5));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SRL) : /* srl $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = SRLSI (* FLD (f_r1), ANDSI (* FLD (f_r2), 31));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SRL3) : /* srl3 $dr,$sr,#$simm16 */
{
#define FLD(f) abuf->fields.fmt_5_addv3.f
* FLD (f_r1) = SRLSI (* FLD (f_r2), ANDSI (FLD (f_simm16), 31));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SRLI) : /* srli $dr,#$uimm5 */
{
#define FLD(f) abuf->fields.fmt_39_slli.f
* FLD (f_r1) = SRLSI (* FLD (f_r1), FLD (f_uimm5));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ST) : /* st $src1,@$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
SETMEMSI (current_cpu, * FLD (f_r2), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMSI (current_cpu, * FLD (f_r2)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ST_D) : /* st $src1,@($slo16,$src2) */
{
#define FLD(f) abuf->fields.fmt_40_st_d.f
SETMEMSI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16)), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMSI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_STB) : /* stb $src1,@$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
SETMEMQI (current_cpu, * FLD (f_r2), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMQI (current_cpu, * FLD (f_r2)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_STB_D) : /* stb $src1,@($slo16,$src2) */
{
#define FLD(f) abuf->fields.fmt_40_st_d.f
SETMEMQI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16)), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMQI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_STH) : /* sth $src1,@$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
SETMEMHI (current_cpu, * FLD (f_r2), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMHI (current_cpu, * FLD (f_r2)));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_STH_D) : /* sth $src1,@($slo16,$src2) */
{
#define FLD(f) abuf->fields.fmt_40_st_d.f
SETMEMHI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16)), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMHI (current_cpu, ADDSI (* FLD (f_r2), FLD (f_simm16))));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ST_PLUS) : /* st $src1,@+$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
do {
* FLD (f_r2) = ADDSI (* FLD (f_r2), 4);
  TRACE_RESULT (current_cpu, "src2", 'x', * FLD (f_r2));
SETMEMSI (current_cpu, * FLD (f_r2), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMSI (current_cpu, * FLD (f_r2)));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_ST_MINUS) : /* st $src1,@-$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
do {
* FLD (f_r2) = SUBSI (* FLD (f_r2), 4);
  TRACE_RESULT (current_cpu, "src2", 'x', * FLD (f_r2));
SETMEMSI (current_cpu, * FLD (f_r2), * FLD (f_r1));
  TRACE_RESULT (current_cpu, "h-memory", 'x', GETMEMSI (current_cpu, * FLD (f_r2)));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SUB) : /* sub $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
* FLD (f_r1) = SUBSI (* FLD (f_r1), * FLD (f_r2));
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SUBV) : /* subv $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_0_add.f
do {
  BI temp1;SI temp0;
  temp0 = SUBSI (* FLD (f_r1), * FLD (f_r2));
  temp1 = SUBOFSI (* FLD (f_r1), * FLD (f_r2), 0);
* FLD (f_r1) = temp0;
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
  CPU (h_cond) = temp1;
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_SUBX) : /* subx $dr,$sr */
{
#define FLD(f) abuf->fields.fmt_6_addx.f
do {
  BI temp1;SI temp0;
  temp0 = SUBCSI (* FLD (f_r1), * FLD (f_r2), CPU (h_cond));
  temp1 = SUBCFSI (* FLD (f_r1), * FLD (f_r2), CPU (h_cond));
* FLD (f_r1) = temp0;
  TRACE_RESULT (current_cpu, "dr", 'x', * FLD (f_r1));
  CPU (h_cond) = temp1;
  TRACE_RESULT (current_cpu, "condbit", 'x', CPU (h_cond));
} while (0);
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_TRAP) : /* trap #$uimm4 */
{
#define FLD(f) abuf->fields.fmt_41_trap.f
do_trap (current_cpu, FLD (f_uimm4));
#undef FLD
}
  BREAK (sem);

  CASE (sem, INSN_UNLOCK) : /* unlock $src1,@$src2 */
{
#define FLD(f) abuf->fields.fmt_15_cmp.f
do_unlock (current_cpu, * FLD (f_r1), * FLD (f_r2));
#undef FLD
}
  BREAK (sem);


    }
  ENDSWITCH (sem) /* End of semantic switch.  */

  PC = new_pc;
}

#endif /* DEFINE_SWITCH */
