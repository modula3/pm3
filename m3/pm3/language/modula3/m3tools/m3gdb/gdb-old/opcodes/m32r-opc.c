/* CGEN opcode support for m32r.

This file is machine generated with CGEN.

Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

This file is part of the GNU Binutils and/or GDB, the GNU debugger.

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


#include "sysdep.h"
#include <stdio.h>
#include "ansidecl.h"
#include "libiberty.h"
#include "bfd.h"
#include "m32r-opc.h"

/* Attributes.  */

static const CGEN_ATTR_ENTRY MACH_attr[] =
{
  { "m32r", MACH_M32R },
  { "max", MACH_MAX },
  { 0, 0 }
};

const CGEN_ATTR_TABLE m32r_cgen_operand_attr_table[] =
{
  { "ABS-ADDR", NULL },
  { "FAKE", NULL },
  { "NEGATIVE", NULL },
  { "PC", NULL },
  { "PCREL-ADDR", NULL },
  { "RELAX", NULL },
  { "RELOC", NULL },
  { "SIGN-OPT", NULL },
  { "UNSIGNED", NULL },
  { 0, 0 }
};

const CGEN_ATTR_TABLE m32r_cgen_insn_attr_table[] =
{
  { "MACH", & MACH_attr[0] },
  { "ALIAS", NULL },
  { "COND-CTI", NULL },
  { "FILL-SLOT", NULL },
  { "PARALLEL", NULL },
  { "RELAX", NULL },
  { "RELAXABLE", NULL },
  { "UNCOND-CTI", NULL },
  { 0, 0 }
};

CGEN_KEYWORD_ENTRY m32r_cgen_opval_h_gr_entries[] = 
{
  { "fp", 13 },
  { "lr", 14 },
  { "sp", 15 },
  { "r0", 0 },
  { "r1", 1 },
  { "r2", 2 },
  { "r3", 3 },
  { "r4", 4 },
  { "r5", 5 },
  { "r6", 6 },
  { "r7", 7 },
  { "r8", 8 },
  { "r9", 9 },
  { "r10", 10 },
  { "r11", 11 },
  { "r12", 12 },
  { "r13", 13 },
  { "r14", 14 },
  { "r15", 15 }
};

CGEN_KEYWORD m32r_cgen_opval_h_gr = 
{
  & m32r_cgen_opval_h_gr_entries[0],
  19
};

CGEN_KEYWORD_ENTRY m32r_cgen_opval_h_cr_entries[] = 
{
  { "psw", 0 },
  { "cbr", 1 },
  { "spi", 2 },
  { "spu", 3 },
  { "bpc", 6 },
  { "cr0", 0 },
  { "cr1", 1 },
  { "cr2", 2 },
  { "cr3", 3 },
  { "cr4", 4 },
  { "cr5", 5 },
  { "cr6", 6 }
};

CGEN_KEYWORD m32r_cgen_opval_h_cr = 
{
  & m32r_cgen_opval_h_cr_entries[0],
  12
};


static CGEN_HW_ENTRY m32r_cgen_hw_entries[] =
{
#define x m32r_cgen_hw_entries
  { &x[1], "h-pc", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[2], "h-memory", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[3], "h-sint", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[4], "h-uint", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[5], "h-addr", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[6], "h-iaddr", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[7], "h-hi16", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[8], "h-slo16", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[9], "h-ulo16", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[10], "h-gr", CGEN_ASM_KEYWORD, (PTR) & m32r_cgen_opval_h_gr },
  { &x[11], "h-cr", CGEN_ASM_KEYWORD, (PTR) & m32r_cgen_opval_h_cr },
  { &x[12], "h-accum", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[13], "h-cond", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[14], "h-sm", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[15], "h-bsm", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[16], "h-ie", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[17], "h-bie", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[18], "h-bcond", CGEN_ASM_KEYWORD, (PTR) 0 },
  { &x[19], "h-bpc", CGEN_ASM_KEYWORD, (PTR) 0 },
  { 0 }
#undef x
};

const CGEN_OPERAND m32r_cgen_operand_table[MAX_OPERANDS] =
{
/* pc: program counter */
  { "pc", 0, 0, { 0, 0|(1<<CGEN_OPERAND_FAKE)|(1<<CGEN_OPERAND_PC), { 0 } }  },
/* sr: source register */
  { "sr", 12, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* dr: destination register */
  { "dr", 4, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* src1: source register 1 */
  { "src1", 4, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* src2: source register 2 */
  { "src2", 12, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* scr: source control register */
  { "scr", 12, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* dcr: destination control register */
  { "dcr", 4, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* simm8: 8 bit signed immediate */
  { "simm8", 8, 8, { 0, 0, { 0 } }  },
/* simm16: 16 bit signed immediate */
  { "simm16", 16, 16, { 0, 0, { 0 } }  },
/* uimm4: 4 bit trap number */
  { "uimm4", 12, 4, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* uimm5: 5 bit shift count */
  { "uimm5", 11, 5, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* uimm16: 16 bit unsigned immediate */
  { "uimm16", 16, 16, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* hi16: high 16 bit immediate, sign optional */
  { "hi16", 16, 16, { 0, 0|(1<<CGEN_OPERAND_SIGN_OPT)|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* slo16: 16 bit signed immediate, for low() */
  { "slo16", 16, 16, { 0, 0, { 0 } }  },
/* ulo16: 16 bit unsigned immediate, for low() */
  { "ulo16", 16, 16, { 0, 0|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* uimm24: 24 bit address */
  { "uimm24", 8, 24, { 0, 0|(1<<CGEN_OPERAND_RELOC)|(1<<CGEN_OPERAND_ABS_ADDR)|(1<<CGEN_OPERAND_UNSIGNED), { 0 } }  },
/* disp8: 8 bit displacement */
  { "disp8", 8, 8, { 0, 0|(1<<CGEN_OPERAND_RELAX)|(1<<CGEN_OPERAND_RELOC)|(1<<CGEN_OPERAND_PCREL_ADDR), { 0 } }  },
/* disp16: 16 bit displacement */
  { "disp16", 16, 16, { 0, 0|(1<<CGEN_OPERAND_RELOC)|(1<<CGEN_OPERAND_PCREL_ADDR), { 0 } }  },
/* disp24: 24 bit displacement */
  { "disp24", 8, 24, { 0, 0|(1<<CGEN_OPERAND_RELAX)|(1<<CGEN_OPERAND_RELOC)|(1<<CGEN_OPERAND_PCREL_ADDR), { 0 } }  },
/* condbit: condition bit */
  { "condbit", 0, 0, { 0, 0|(1<<CGEN_OPERAND_FAKE), { 0 } }  },
/* accum: accumulator */
  { "accum", 0, 0, { 0, 0|(1<<CGEN_OPERAND_FAKE), { 0 } }  },
};

#define MNEM 1 /* syntax value for mnemonic */
#define OP(x) (128 + CAT (M32R_OPERAND_,x))

static const CGEN_SYNTAX syntax_table[] =
{
/* <op> $dr,$sr */
/*   0 */  { MNEM, ' ', OP(DR), ',', OP(SR), 0 },
/* <op> $dr,$sr,#$slo16 */
/*   1 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', '#', OP(SLO16), 0 },
/* <op> $dr,$sr,$slo16 */
/*   2 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', OP(SLO16), 0 },
/* <op> $dr,$sr,#$uimm16 */
/*   3 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', '#', OP(UIMM16), 0 },
/* <op> $dr,$sr,$uimm16 */
/*   4 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', OP(UIMM16), 0 },
/* <op> $dr,$sr,#$ulo16 */
/*   5 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', '#', OP(ULO16), 0 },
/* <op> $dr,$sr,$ulo16 */
/*   6 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', OP(ULO16), 0 },
/* <op> $dr,#$simm8 */
/*   7 */  { MNEM, ' ', OP(DR), ',', '#', OP(SIMM8), 0 },
/* <op> $dr,$simm8 */
/*   8 */  { MNEM, ' ', OP(DR), ',', OP(SIMM8), 0 },
/* <op> $dr,$sr,#$simm16 */
/*   9 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', '#', OP(SIMM16), 0 },
/* <op> $dr,$sr,$simm16 */
/*  10 */  { MNEM, ' ', OP(DR), ',', OP(SR), ',', OP(SIMM16), 0 },
/* <op> $disp8 */
/*  11 */  { MNEM, ' ', OP(DISP8), 0 },
/* <op> $disp24 */
/*  12 */  { MNEM, ' ', OP(DISP24), 0 },
/* <op> $src1,$src2,$disp16 */
/*  13 */  { MNEM, ' ', OP(SRC1), ',', OP(SRC2), ',', OP(DISP16), 0 },
/* <op> $src2,$disp16 */
/*  14 */  { MNEM, ' ', OP(SRC2), ',', OP(DISP16), 0 },
/* <op> $src1,$src2 */
/*  15 */  { MNEM, ' ', OP(SRC1), ',', OP(SRC2), 0 },
/* <op> $src2,#$simm16 */
/*  16 */  { MNEM, ' ', OP(SRC2), ',', '#', OP(SIMM16), 0 },
/* <op> $src2,$simm16 */
/*  17 */  { MNEM, ' ', OP(SRC2), ',', OP(SIMM16), 0 },
/* <op> $src2,#$uimm16 */
/*  18 */  { MNEM, ' ', OP(SRC2), ',', '#', OP(UIMM16), 0 },
/* <op> $src2,$uimm16 */
/*  19 */  { MNEM, ' ', OP(SRC2), ',', OP(UIMM16), 0 },
/* <op> $src2 */
/*  20 */  { MNEM, ' ', OP(SRC2), 0 },
/* <op> $sr */
/*  21 */  { MNEM, ' ', OP(SR), 0 },
/* <op> $dr,@$sr */
/*  22 */  { MNEM, ' ', OP(DR), ',', '@', OP(SR), 0 },
/* <op> $dr,@($sr) */
/*  23 */  { MNEM, ' ', OP(DR), ',', '@', '(', OP(SR), ')', 0 },
/* <op> $dr,@($slo16,$sr) */
/*  24 */  { MNEM, ' ', OP(DR), ',', '@', '(', OP(SLO16), ',', OP(SR), ')', 0 },
/* <op> $dr,@($sr,$slo16) */
/*  25 */  { MNEM, ' ', OP(DR), ',', '@', '(', OP(SR), ',', OP(SLO16), ')', 0 },
/* <op> $dr,@$sr+ */
/*  26 */  { MNEM, ' ', OP(DR), ',', '@', OP(SR), '+', 0 },
/* <op> $dr,#$uimm24 */
/*  27 */  { MNEM, ' ', OP(DR), ',', '#', OP(UIMM24), 0 },
/* <op> $dr,$uimm24 */
/*  28 */  { MNEM, ' ', OP(DR), ',', OP(UIMM24), 0 },
/* <op> $dr,$slo16 */
/*  29 */  { MNEM, ' ', OP(DR), ',', OP(SLO16), 0 },
/* <op> $src1,$src2,$x */
/*  30 */  { MNEM, ' ', OP(SRC1), ',', OP(SRC2), ',', 141, 0 },
/* <op> $dr */
/*  31 */  { MNEM, ' ', OP(DR), 0 },
/* <op> $dr,$xs */
/*  32 */  { MNEM, ' ', OP(DR), ',', 140, 0 },
/* <op> $dr,$scr */
/*  33 */  { MNEM, ' ', OP(DR), ',', OP(SCR), 0 },
/* <op> $src1 */
/*  34 */  { MNEM, ' ', OP(SRC1), 0 },
/* <op> $src1,$xs */
/*  35 */  { MNEM, ' ', OP(SRC1), ',', 140, 0 },
/* <op> $sr,$dcr */
/*  36 */  { MNEM, ' ', OP(SR), ',', OP(DCR), 0 },
/* <op> */
/*  37 */  { MNEM, 0 },
/* <op> $xs */
/*  38 */  { MNEM, ' ', 140, 0 },
/* <op> $dr,#$hi16 */
/*  39 */  { MNEM, ' ', OP(DR), ',', '#', OP(HI16), 0 },
/* <op> $dr,$hi16 */
/*  40 */  { MNEM, ' ', OP(DR), ',', OP(HI16), 0 },
/* <op> $dr,#$uimm5 */
/*  41 */  { MNEM, ' ', OP(DR), ',', '#', OP(UIMM5), 0 },
/* <op> $dr,$uimm5 */
/*  42 */  { MNEM, ' ', OP(DR), ',', OP(UIMM5), 0 },
/* <op> $src1,@$src2 */
/*  43 */  { MNEM, ' ', OP(SRC1), ',', '@', OP(SRC2), 0 },
/* <op> $src1,@($src2) */
/*  44 */  { MNEM, ' ', OP(SRC1), ',', '@', '(', OP(SRC2), ')', 0 },
/* <op> $src1,@($slo16,$src2) */
/*  45 */  { MNEM, ' ', OP(SRC1), ',', '@', '(', OP(SLO16), ',', OP(SRC2), ')', 0 },
/* <op> $src1,@($src2,$slo16) */
/*  46 */  { MNEM, ' ', OP(SRC1), ',', '@', '(', OP(SRC2), ',', OP(SLO16), ')', 0 },
/* <op> $src1,@+$src2 */
/*  47 */  { MNEM, ' ', OP(SRC1), ',', '@', '+', OP(SRC2), 0 },
/* <op> $src1,@-$src2 */
/*  48 */  { MNEM, ' ', OP(SRC1), ',', '@', '-', OP(SRC2), 0 },
/* <op> #$uimm4 */
/*  49 */  { MNEM, ' ', '#', OP(UIMM4), 0 },
/* <op> $uimm4 */
/*  50 */  { MNEM, ' ', OP(UIMM4), 0 },
/* <op> $dr,$src2 */
/*  51 */  { MNEM, ' ', OP(DR), ',', OP(SRC2), 0 },
};

#undef MNEM
#undef OP

static const CGEN_FORMAT format_table[] = 
{
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.sr. */
/*   0 */  { 16, 16, 0xf0f0 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.sr.f-simm16.slo16. */
/*   1 */  { 32, 32, 0xf0f00000 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.sr.f-uimm16.uimm16. */
/*   2 */  { 32, 32, 0xf0f00000 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.sr.f-uimm16.ulo16. */
/*   3 */  { 32, 32, 0xf0f00000 },
/* f-op1.number.f-r1.dr.f-simm8.simm8. */
/*   4 */  { 16, 16, 0xf000 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.sr.f-simm16.simm16. */
/*   5 */  { 32, 32, 0xf0f00000 },
/* f-op1.number.f-r1.number.f-disp8.disp8. */
/*   6 */  { 16, 16, 0xff00 },
/* f-op1.number.f-r1.number.f-disp24.disp24. */
/*   7 */  { 32, 32, 0xff000000 },
/* f-op1.number.f-r1.src1.f-op2.number.f-r2.src2.f-disp16.disp16. */
/*   8 */  { 32, 32, 0xf0f00000 },
/* f-op1.number.f-r1.number.f-op2.number.f-r2.src2.f-disp16.disp16. */
/*   9 */  { 32, 32, 0xfff00000 },
/* f-op1.number.f-r1.src1.f-op2.number.f-r2.src2. */
/*  10 */  { 16, 16, 0xf0f0 },
/* f-op1.number.f-r1.number.f-op2.number.f-r2.src2.f-simm16.simm16. */
/*  11 */  { 32, 32, 0xfff00000 },
/* f-op1.number.f-r1.number.f-op2.number.f-r2.src2.f-uimm16.uimm16. */
/*  12 */  { 32, 32, 0xfff00000 },
/* f-op1.number.f-r1.number.f-op2.number.f-r2.src2. */
/*  13 */  { 16, 16, 0xfff0 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.sr.f-simm16.number. */
/*  14 */  { 32, 32, 0xf0f0ffff },
/* f-op1.number.f-r1.number.f-op2.number.f-r2.sr. */
/*  15 */  { 16, 16, 0xfff0 },
/* f-op1.number.f-r1.dr.f-uimm24.uimm24. */
/*  16 */  { 32, 32, 0xf0000000 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.number.f-simm16.slo16. */
/*  17 */  { 32, 32, 0xf0ff0000 },
/* f-op1.number.f-r1.src1.f-acc.acc.f-op23.number.f-r2.src2. */
/*  18 */  { 16, 16, 0xf070 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.number. */
/*  19 */  { 16, 16, 0xf0ff },
/* f-op1.number.f-r1.dr.f-op2.number.f-accs.accs.f-op3.number. */
/*  20 */  { 16, 16, 0xf0f3 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.scr. */
/*  21 */  { 16, 16, 0xf0f0 },
/* f-op1.number.f-r1.src1.f-op2.number.f-r2.number. */
/*  22 */  { 16, 16, 0xf0ff },
/* f-op1.number.f-r1.src1.f-op2.number.f-accs.accs.f-op3.number. */
/*  23 */  { 16, 16, 0xf0f3 },
/* f-op1.number.f-r1.dcr.f-op2.number.f-r2.sr. */
/*  24 */  { 16, 16, 0xf0f0 },
/* f-op1.number.f-r1.number.f-op2.number.f-r2.number. */
/*  25 */  { 16, 16, 0xffff },
/* f-op1.number.f-r1.number.f-op2.number.f-accs.accs.f-op3.number. */
/*  26 */  { 16, 16, 0xfff3 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.number.f-hi16.hi16. */
/*  27 */  { 32, 32, 0xf0ff0000 },
/* f-op1.number.f-r1.dr.f-shift-op2.number.f-uimm5.uimm5. */
/*  28 */  { 16, 16, 0xf0e0 },
/* f-op1.number.f-r1.src1.f-op2.number.f-r2.src2.f-simm16.slo16. */
/*  29 */  { 32, 32, 0xf0f00000 },
/* f-op1.number.f-r1.number.f-op2.number.f-uimm4.uimm4. */
/*  30 */  { 16, 16, 0xfff0 },
/* f-op1.number.f-r1.dr.f-op2.number.f-r2.src2.f-uimm16.number. */
/*  31 */  { 32, 32, 0xf0f0ffff },
};

#define A(a) (1 << CGEN_CAT3 (CGEN_INSN,_,a))
#define SYN(n) (& syntax_table[n])
#define FMT(n) (& format_table[n])

const CGEN_INSN m32r_cgen_insn_table_entries[MAX_INSNS] =
{
  /* null first entry, end of all hash chains */
  { { 0 }, 0 },
/* add $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "add", "add", SYN (0), FMT (0), 0xa0,
    { 2, 0|A(PARALLEL), { (1<<MACH_M32R) } }
  },
/* add3 $dr,$sr,#$slo16 */
  {
    { 1, 1, 1, 1 },
    "add3", "add3", SYN (1), FMT (1), 0x80a00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* add3 $dr,$sr,$slo16 */
  {
    { 1, 1, 1, 1 },
    "add3.a", "add3", SYN (2), FMT (1), 0x80a00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* and $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "and", "and", SYN (0), FMT (0), 0xc0,
    { 2, 0|A(PARALLEL), { (1<<MACH_M32R) } }
  },
/* and3 $dr,$sr,#$uimm16 */
  {
    { 1, 1, 1, 1 },
    "and3", "and3", SYN (3), FMT (2), 0x80c00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* and3 $dr,$sr,$uimm16 */
  {
    { 1, 1, 1, 1 },
    "and3.a", "and3", SYN (4), FMT (2), 0x80c00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* or $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "or", "or", SYN (0), FMT (0), 0xe0,
    { 2, 0|A(PARALLEL), { (1<<MACH_M32R) } }
  },
/* or3 $dr,$sr,#$ulo16 */
  {
    { 1, 1, 1, 1 },
    "or3", "or3", SYN (5), FMT (3), 0x80e00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* or3 $dr,$sr,$ulo16 */
  {
    { 1, 1, 1, 1 },
    "or3.a", "or3", SYN (6), FMT (3), 0x80e00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* xor $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "xor", "xor", SYN (0), FMT (0), 0xd0,
    { 2, 0|A(PARALLEL), { (1<<MACH_M32R) } }
  },
/* xor3 $dr,$sr,#$uimm16 */
  {
    { 1, 1, 1, 1 },
    "xor3", "xor3", SYN (3), FMT (2), 0x80d00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* xor3 $dr,$sr,$uimm16 */
  {
    { 1, 1, 1, 1 },
    "xor3.a", "xor3", SYN (4), FMT (2), 0x80d00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* addi $dr,#$simm8 */
  {
    { 1, 1, 1, 1 },
    "addi", "addi", SYN (7), FMT (4), 0x4000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* addi $dr,$simm8 */
  {
    { 1, 1, 1, 1 },
    "addi.a", "addi", SYN (8), FMT (4), 0x4000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* addv $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "addv", "addv", SYN (0), FMT (0), 0x80,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* addv3 $dr,$sr,#$simm16 */
  {
    { 1, 1, 1, 1 },
    "addv3", "addv3", SYN (9), FMT (5), 0x80800000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* addv3 $dr,$sr,$simm16 */
  {
    { 1, 1, 1, 1 },
    "addv3.a", "addv3", SYN (10), FMT (5), 0x80800000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* addx $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "addx", "addx", SYN (0), FMT (0), 0x90,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* bc $disp8 */
  {
    { 1, 1, 1, 1 },
    "bc8", "bc", SYN (11), FMT (6), 0x7c00,
    { 2, 0|A(RELAXABLE)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bc.s $disp8 */
  {
    { 1, 1, 1, 1 },
    "bc8.s", "bc.s", SYN (11), FMT (6), 0x7c00,
    { 2, 0|A(ALIAS)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bc $disp24 */
  {
    { 1, 1, 1, 1 },
    "bc24", "bc", SYN (12), FMT (7), 0xfc000000,
    { 2, 0|A(RELAX)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bc.l $disp24 */
  {
    { 1, 1, 1, 1 },
    "bc24.l", "bc.l", SYN (12), FMT (7), 0xfc000000,
    { 2, 0|A(ALIAS)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* beq $src1,$src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "beq", "beq", SYN (13), FMT (8), 0xb0000000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* beqz $src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "beqz", "beqz", SYN (14), FMT (9), 0xb0800000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bgez $src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "bgez", "bgez", SYN (14), FMT (9), 0xb0b00000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bgtz $src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "bgtz", "bgtz", SYN (14), FMT (9), 0xb0d00000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* blez $src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "blez", "blez", SYN (14), FMT (9), 0xb0c00000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bltz $src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "bltz", "bltz", SYN (14), FMT (9), 0xb0a00000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bnez $src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "bnez", "bnez", SYN (14), FMT (9), 0xb0900000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bl $disp8 */
  {
    { 1, 1, 1, 1 },
    "bl8", "bl", SYN (11), FMT (6), 0x7e00,
    { 2, 0|A(FILL_SLOT)|A(RELAXABLE)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bl.s $disp8 */
  {
    { 1, 1, 1, 1 },
    "bl8.s", "bl.s", SYN (11), FMT (6), 0x7e00,
    { 2, 0|A(FILL_SLOT)|A(ALIAS)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bl $disp24 */
  {
    { 1, 1, 1, 1 },
    "bl24", "bl", SYN (12), FMT (7), 0xfe000000,
    { 2, 0|A(RELAX)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bl.l $disp24 */
  {
    { 1, 1, 1, 1 },
    "bl24.l", "bl.l", SYN (12), FMT (7), 0xfe000000,
    { 2, 0|A(ALIAS)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bnc $disp8 */
  {
    { 1, 1, 1, 1 },
    "bnc8", "bnc", SYN (11), FMT (6), 0x7d00,
    { 2, 0|A(RELAXABLE)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bnc.s $disp8 */
  {
    { 1, 1, 1, 1 },
    "bnc8.s", "bnc.s", SYN (11), FMT (6), 0x7d00,
    { 2, 0|A(ALIAS)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bnc $disp24 */
  {
    { 1, 1, 1, 1 },
    "bnc24", "bnc", SYN (12), FMT (7), 0xfd000000,
    { 2, 0|A(RELAX)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bnc.l $disp24 */
  {
    { 1, 1, 1, 1 },
    "bnc24.l", "bnc.l", SYN (12), FMT (7), 0xfd000000,
    { 2, 0|A(ALIAS)|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bne $src1,$src2,$disp16 */
  {
    { 1, 1, 1, 1 },
    "bne", "bne", SYN (13), FMT (8), 0xb0100000,
    { 2, 0|A(COND_CTI), { (1<<MACH_M32R) } }
  },
/* bra $disp8 */
  {
    { 1, 1, 1, 1 },
    "bra8", "bra", SYN (11), FMT (6), 0x7f00,
    { 2, 0|A(FILL_SLOT)|A(RELAXABLE)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bra.s $disp8 */
  {
    { 1, 1, 1, 1 },
    "bra8.s", "bra.s", SYN (11), FMT (6), 0x7f00,
    { 2, 0|A(ALIAS)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bra $disp24 */
  {
    { 1, 1, 1, 1 },
    "bra24", "bra", SYN (12), FMT (7), 0xff000000,
    { 2, 0|A(RELAX)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* bra.l $disp24 */
  {
    { 1, 1, 1, 1 },
    "bra24.l", "bra.l", SYN (12), FMT (7), 0xff000000,
    { 2, 0|A(ALIAS)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* cmp $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "cmp", "cmp", SYN (15), FMT (10), 0x40,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* cmpi $src2,#$simm16 */
  {
    { 1, 1, 1, 1 },
    "cmpi", "cmpi", SYN (16), FMT (11), 0x80400000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* cmpi $src2,$simm16 */
  {
    { 1, 1, 1, 1 },
    "cmpi.a", "cmpi", SYN (17), FMT (11), 0x80400000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* cmpu $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "cmpu", "cmpu", SYN (15), FMT (10), 0x50,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* cmpui $src2,#$uimm16 */
  {
    { 1, 1, 1, 1 },
    "cmpui", "cmpui", SYN (18), FMT (12), 0x80500000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* cmpui $src2,$uimm16 */
  {
    { 1, 1, 1, 1 },
    "cmpui.a", "cmpui", SYN (19), FMT (12), 0x80500000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* div $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "div", "div", SYN (0), FMT (14), 0x90000000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* divu $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "divu", "divu", SYN (0), FMT (14), 0x90100000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* rem $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "rem", "rem", SYN (0), FMT (14), 0x90200000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* remu $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "remu", "remu", SYN (0), FMT (14), 0x90300000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* jl $sr */
  {
    { 1, 1, 1, 1 },
    "jl", "jl", SYN (21), FMT (15), 0x1ec0,
    { 2, 0|A(FILL_SLOT)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* jmp $sr */
  {
    { 1, 1, 1, 1 },
    "jmp", "jmp", SYN (21), FMT (15), 0x1fc0,
    { 2, 0|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* ld $dr,@$sr */
  {
    { 1, 1, 1, 1 },
    "ld", "ld", SYN (22), FMT (0), 0x20c0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ld $dr,@($sr) */
  {
    { 1, 1, 1, 1 },
    "ld-2", "ld", SYN (23), FMT (0), 0x20c0,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ld $dr,@($slo16,$sr) */
  {
    { 1, 1, 1, 1 },
    "ld-d", "ld", SYN (24), FMT (1), 0xa0c00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ld $dr,@($sr,$slo16) */
  {
    { 1, 1, 1, 1 },
    "ld-d2", "ld", SYN (25), FMT (1), 0xa0c00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldb $dr,@$sr */
  {
    { 1, 1, 1, 1 },
    "ldb", "ldb", SYN (22), FMT (0), 0x2080,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldb $dr,@($sr) */
  {
    { 1, 1, 1, 1 },
    "ldb-2", "ldb", SYN (23), FMT (0), 0x2080,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldb $dr,@($slo16,$sr) */
  {
    { 1, 1, 1, 1 },
    "ldb-d", "ldb", SYN (24), FMT (1), 0xa0800000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldb $dr,@($sr,$slo16) */
  {
    { 1, 1, 1, 1 },
    "ldb-d2", "ldb", SYN (25), FMT (1), 0xa0800000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldh $dr,@$sr */
  {
    { 1, 1, 1, 1 },
    "ldh", "ldh", SYN (22), FMT (0), 0x20a0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldh $dr,@($sr) */
  {
    { 1, 1, 1, 1 },
    "ldh-2", "ldh", SYN (23), FMT (0), 0x20a0,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldh $dr,@($slo16,$sr) */
  {
    { 1, 1, 1, 1 },
    "ldh-d", "ldh", SYN (24), FMT (1), 0xa0a00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldh $dr,@($sr,$slo16) */
  {
    { 1, 1, 1, 1 },
    "ldh-d2", "ldh", SYN (25), FMT (1), 0xa0a00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldub $dr,@$sr */
  {
    { 1, 1, 1, 1 },
    "ldub", "ldub", SYN (22), FMT (0), 0x2090,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldub $dr,@($sr) */
  {
    { 1, 1, 1, 1 },
    "ldub-2", "ldub", SYN (23), FMT (0), 0x2090,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldub $dr,@($slo16,$sr) */
  {
    { 1, 1, 1, 1 },
    "ldub-d", "ldub", SYN (24), FMT (1), 0xa0900000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldub $dr,@($sr,$slo16) */
  {
    { 1, 1, 1, 1 },
    "ldub-d2", "ldub", SYN (25), FMT (1), 0xa0900000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* lduh $dr,@$sr */
  {
    { 1, 1, 1, 1 },
    "lduh", "lduh", SYN (22), FMT (0), 0x20b0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* lduh $dr,@($sr) */
  {
    { 1, 1, 1, 1 },
    "lduh-2", "lduh", SYN (23), FMT (0), 0x20b0,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* lduh $dr,@($slo16,$sr) */
  {
    { 1, 1, 1, 1 },
    "lduh-d", "lduh", SYN (24), FMT (1), 0xa0b00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* lduh $dr,@($sr,$slo16) */
  {
    { 1, 1, 1, 1 },
    "lduh-d2", "lduh", SYN (25), FMT (1), 0xa0b00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ld $dr,@$sr+ */
  {
    { 1, 1, 1, 1 },
    "ld-plus", "ld", SYN (26), FMT (0), 0x20e0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ld24 $dr,#$uimm24 */
  {
    { 1, 1, 1, 1 },
    "ld24", "ld24", SYN (27), FMT (16), 0xe0000000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ld24 $dr,$uimm24 */
  {
    { 1, 1, 1, 1 },
    "ld24.a", "ld24", SYN (28), FMT (16), 0xe0000000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldi $dr,#$simm8 */
  {
    { 1, 1, 1, 1 },
    "ldi8", "ldi", SYN (7), FMT (4), 0x6000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldi $dr,$simm8 */
  {
    { 1, 1, 1, 1 },
    "ldi8.a", "ldi", SYN (8), FMT (4), 0x6000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldi8 $dr,#$simm8 */
  {
    { 1, 1, 1, 1 },
    "ldi8a", "ldi8", SYN (7), FMT (4), 0x6000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldi8 $dr,$simm8 */
  {
    { 1, 1, 1, 1 },
    "ldi8a.a", "ldi8", SYN (8), FMT (4), 0x6000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* ldi $dr,$slo16 */
  {
    { 1, 1, 1, 1 },
    "ldi16", "ldi", SYN (29), FMT (17), 0x90f00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* ldi16 $dr,$slo16 */
  {
    { 1, 1, 1, 1 },
    "ldi16a", "ldi16", SYN (29), FMT (17), 0x90f00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* lock $dr,@$sr */
  {
    { 1, 1, 1, 1 },
    "lock", "lock", SYN (22), FMT (0), 0x20d0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* machi $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "machi", "machi", SYN (15), FMT (10), 0x3040,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* maclo $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "maclo", "maclo", SYN (15), FMT (10), 0x3050,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* macwhi $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "macwhi", "macwhi", SYN (15), FMT (10), 0x3060,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* macwlo $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "macwlo", "macwlo", SYN (15), FMT (10), 0x3070,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mul $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "mul", "mul", SYN (0), FMT (0), 0x1060,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mulhi $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "mulhi", "mulhi", SYN (15), FMT (10), 0x3000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mullo $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "mullo", "mullo", SYN (15), FMT (10), 0x3010,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mulwhi $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "mulwhi", "mulwhi", SYN (15), FMT (10), 0x3020,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mulwlo $src1,$src2 */
  {
    { 1, 1, 1, 1 },
    "mulwlo", "mulwlo", SYN (15), FMT (10), 0x3030,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mv $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "mv", "mv", SYN (0), FMT (0), 0x1080,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvfachi $dr */
  {
    { 1, 1, 1, 1 },
    "mvfachi", "mvfachi", SYN (31), FMT (19), 0x50f0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvfaclo $dr */
  {
    { 1, 1, 1, 1 },
    "mvfaclo", "mvfaclo", SYN (31), FMT (19), 0x50f1,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvfacmi $dr */
  {
    { 1, 1, 1, 1 },
    "mvfacmi", "mvfacmi", SYN (31), FMT (19), 0x50f2,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvfc $dr,$scr */
  {
    { 1, 1, 1, 1 },
    "mvfc", "mvfc", SYN (33), FMT (21), 0x1090,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvtachi $src1 */
  {
    { 1, 1, 1, 1 },
    "mvtachi", "mvtachi", SYN (34), FMT (22), 0x5070,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvtaclo $src1 */
  {
    { 1, 1, 1, 1 },
    "mvtaclo", "mvtaclo", SYN (34), FMT (22), 0x5071,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* mvtc $sr,$dcr */
  {
    { 1, 1, 1, 1 },
    "mvtc", "mvtc", SYN (36), FMT (24), 0x10a0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* neg $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "neg", "neg", SYN (0), FMT (0), 0x30,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* nop */
  {
    { 1, 1, 1, 1 },
    "nop", "nop", SYN (37), FMT (25), 0x7000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* not $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "not", "not", SYN (0), FMT (0), 0xb0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* rac */
  {
    { 1, 1, 1, 1 },
    "rac", "rac", SYN (37), FMT (25), 0x5090,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* rach */
  {
    { 1, 1, 1, 1 },
    "rach", "rach", SYN (37), FMT (25), 0x5080,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* rte */
  {
    { 1, 1, 1, 1 },
    "rte", "rte", SYN (37), FMT (25), 0x10d6,
    { 2, 0|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* seth $dr,#$hi16 */
  {
    { 1, 1, 1, 1 },
    "seth", "seth", SYN (39), FMT (27), 0xd0c00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* seth $dr,$hi16 */
  {
    { 1, 1, 1, 1 },
    "seth.a", "seth", SYN (40), FMT (27), 0xd0c00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* sll $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "sll", "sll", SYN (0), FMT (0), 0x1040,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sll3 $dr,$sr,#$simm16 */
  {
    { 1, 1, 1, 1 },
    "sll3", "sll3", SYN (9), FMT (5), 0x90c00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sll3 $dr,$sr,$simm16 */
  {
    { 1, 1, 1, 1 },
    "sll3.a", "sll3", SYN (10), FMT (5), 0x90c00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* slli $dr,#$uimm5 */
  {
    { 1, 1, 1, 1 },
    "slli", "slli", SYN (41), FMT (28), 0x5040,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* slli $dr,$uimm5 */
  {
    { 1, 1, 1, 1 },
    "slli.a", "slli", SYN (42), FMT (28), 0x5040,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* sra $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "sra", "sra", SYN (0), FMT (0), 0x1020,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sra3 $dr,$sr,#$simm16 */
  {
    { 1, 1, 1, 1 },
    "sra3", "sra3", SYN (9), FMT (5), 0x90a00000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sra3 $dr,$sr,$simm16 */
  {
    { 1, 1, 1, 1 },
    "sra3.a", "sra3", SYN (10), FMT (5), 0x90a00000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* srai $dr,#$uimm5 */
  {
    { 1, 1, 1, 1 },
    "srai", "srai", SYN (41), FMT (28), 0x5020,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* srai $dr,$uimm5 */
  {
    { 1, 1, 1, 1 },
    "srai.a", "srai", SYN (42), FMT (28), 0x5020,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* srl $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "srl", "srl", SYN (0), FMT (0), 0x1000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* srl3 $dr,$sr,#$simm16 */
  {
    { 1, 1, 1, 1 },
    "srl3", "srl3", SYN (9), FMT (5), 0x90800000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* srl3 $dr,$sr,$simm16 */
  {
    { 1, 1, 1, 1 },
    "srl3.a", "srl3", SYN (10), FMT (5), 0x90800000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* srli $dr,#$uimm5 */
  {
    { 1, 1, 1, 1 },
    "srli", "srli", SYN (41), FMT (28), 0x5000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* srli $dr,$uimm5 */
  {
    { 1, 1, 1, 1 },
    "srli.a", "srli", SYN (42), FMT (28), 0x5000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* st $src1,@$src2 */
  {
    { 1, 1, 1, 1 },
    "st", "st", SYN (43), FMT (10), 0x2040,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* st $src1,@($src2) */
  {
    { 1, 1, 1, 1 },
    "st-2", "st", SYN (44), FMT (10), 0x2040,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* st $src1,@($slo16,$src2) */
  {
    { 1, 1, 1, 1 },
    "st-d", "st", SYN (45), FMT (29), 0xa0400000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* st $src1,@($src2,$slo16) */
  {
    { 1, 1, 1, 1 },
    "st-d2", "st", SYN (46), FMT (29), 0xa0400000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* stb $src1,@$src2 */
  {
    { 1, 1, 1, 1 },
    "stb", "stb", SYN (43), FMT (10), 0x2000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* stb $src1,@($src2) */
  {
    { 1, 1, 1, 1 },
    "stb-2", "stb", SYN (44), FMT (10), 0x2000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* stb $src1,@($slo16,$src2) */
  {
    { 1, 1, 1, 1 },
    "stb-d", "stb", SYN (45), FMT (29), 0xa0000000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* stb $src1,@($src2,$slo16) */
  {
    { 1, 1, 1, 1 },
    "stb-d2", "stb", SYN (46), FMT (29), 0xa0000000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* sth $src1,@$src2 */
  {
    { 1, 1, 1, 1 },
    "sth", "sth", SYN (43), FMT (10), 0x2020,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sth $src1,@($src2) */
  {
    { 1, 1, 1, 1 },
    "sth-2", "sth", SYN (44), FMT (10), 0x2020,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* sth $src1,@($slo16,$src2) */
  {
    { 1, 1, 1, 1 },
    "sth-d", "sth", SYN (45), FMT (29), 0xa0200000,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sth $src1,@($src2,$slo16) */
  {
    { 1, 1, 1, 1 },
    "sth-d2", "sth", SYN (46), FMT (29), 0xa0200000,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* st $src1,@+$src2 */
  {
    { 1, 1, 1, 1 },
    "st-plus", "st", SYN (47), FMT (10), 0x2060,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* st $src1,@-$src2 */
  {
    { 1, 1, 1, 1 },
    "st-minus", "st", SYN (48), FMT (10), 0x2070,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* sub $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "sub", "sub", SYN (0), FMT (0), 0x20,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* subv $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "subv", "subv", SYN (0), FMT (0), 0x0,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* subx $dr,$sr */
  {
    { 1, 1, 1, 1 },
    "subx", "subx", SYN (0), FMT (0), 0x10,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* trap #$uimm4 */
  {
    { 1, 1, 1, 1 },
    "trap", "trap", SYN (49), FMT (30), 0x10f0,
    { 2, 0|A(FILL_SLOT)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* trap $uimm4 */
  {
    { 1, 1, 1, 1 },
    "trap.a", "trap", SYN (50), FMT (30), 0x10f0,
    { 2, 0|A(ALIAS)|A(FILL_SLOT)|A(UNCOND_CTI), { (1<<MACH_M32R) } }
  },
/* unlock $src1,@$src2 */
  {
    { 1, 1, 1, 1 },
    "unlock", "unlock", SYN (43), FMT (10), 0x2050,
    { 2, 0, { (1<<MACH_M32R) } }
  },
/* push $src1 */
  {
    { 1, 1, 1, 1 },
    "push", "push", SYN (34), FMT (22), 0x207f,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
/* pop $dr */
  {
    { 1, 1, 1, 1 },
    "pop", "pop", SYN (31), FMT (19), 0x20ef,
    { 2, 0|A(ALIAS), { (1<<MACH_M32R) } }
  },
};

#undef A
#undef SYN
#undef FMT

CGEN_INSN_TABLE m32r_cgen_insn_table =
{
  & m32r_cgen_insn_table_entries[0],
  sizeof (CGEN_INSN),
  MAX_INSNS,
  NULL,
  m32r_cgen_asm_hash_insn, CGEN_ASM_HASH_SIZE,
  m32r_cgen_dis_hash_insn, CGEN_DIS_HASH_SIZE
};

/* The hash functions are recorded here to help keep assembler code out of
   the disassembler and vice versa.  */

unsigned int
m32r_cgen_asm_hash_insn (insn)
     const char * insn;
{
  return CGEN_ASM_HASH (insn);
}

unsigned int
m32r_cgen_dis_hash_insn (buf, value)
     const char * buf;
     unsigned long value;
{
  return CGEN_DIS_HASH (buf, value);
}

CGEN_OPCODE_DATA m32r_cgen_opcode_data = 
{
  & m32r_cgen_hw_entries[0],
  & m32r_cgen_insn_table,
};

void
m32r_cgen_init_tables (mach)
    int mach;
{
}

/* Main entry point for stuffing values in cgen_fields.  */

CGEN_INLINE void
m32r_cgen_set_operand (opindex, valuep, fields)
     int opindex;
     const long * valuep;
     CGEN_FIELDS * fields;
{
  switch (opindex)
    {
    case M32R_OPERAND_SR :
      fields->f_r2 = * valuep;
      break;
    case M32R_OPERAND_DR :
      fields->f_r1 = * valuep;
      break;
    case M32R_OPERAND_SRC1 :
      fields->f_r1 = * valuep;
      break;
    case M32R_OPERAND_SRC2 :
      fields->f_r2 = * valuep;
      break;
    case M32R_OPERAND_SCR :
      fields->f_r2 = * valuep;
      break;
    case M32R_OPERAND_DCR :
      fields->f_r1 = * valuep;
      break;
    case M32R_OPERAND_SIMM8 :
      fields->f_simm8 = * valuep;
      break;
    case M32R_OPERAND_SIMM16 :
      fields->f_simm16 = * valuep;
      break;
    case M32R_OPERAND_UIMM4 :
      fields->f_uimm4 = * valuep;
      break;
    case M32R_OPERAND_UIMM5 :
      fields->f_uimm5 = * valuep;
      break;
    case M32R_OPERAND_UIMM16 :
      fields->f_uimm16 = * valuep;
      break;
    case M32R_OPERAND_HI16 :
      fields->f_hi16 = * valuep;
      break;
    case M32R_OPERAND_SLO16 :
      fields->f_simm16 = * valuep;
      break;
    case M32R_OPERAND_ULO16 :
      fields->f_uimm16 = * valuep;
      break;
    case M32R_OPERAND_UIMM24 :
      fields->f_uimm24 = * valuep;
      break;
    case M32R_OPERAND_DISP8 :
      fields->f_disp8 = * valuep;
      break;
    case M32R_OPERAND_DISP16 :
      fields->f_disp16 = * valuep;
      break;
    case M32R_OPERAND_DISP24 :
      fields->f_disp24 = * valuep;
      break;

    default :
      fprintf (stderr, "Unrecognized field %d while setting operand.\n",
		       opindex);
      abort ();
  }
}

/* Main entry point for getting values from cgen_fields.  */

CGEN_INLINE long
m32r_cgen_get_operand (opindex, fields)
     int opindex;
     const CGEN_FIELDS * fields;
{
  long value;

  switch (opindex)
    {
    case M32R_OPERAND_SR :
      value = fields->f_r2;
      break;
    case M32R_OPERAND_DR :
      value = fields->f_r1;
      break;
    case M32R_OPERAND_SRC1 :
      value = fields->f_r1;
      break;
    case M32R_OPERAND_SRC2 :
      value = fields->f_r2;
      break;
    case M32R_OPERAND_SCR :
      value = fields->f_r2;
      break;
    case M32R_OPERAND_DCR :
      value = fields->f_r1;
      break;
    case M32R_OPERAND_SIMM8 :
      value = fields->f_simm8;
      break;
    case M32R_OPERAND_SIMM16 :
      value = fields->f_simm16;
      break;
    case M32R_OPERAND_UIMM4 :
      value = fields->f_uimm4;
      break;
    case M32R_OPERAND_UIMM5 :
      value = fields->f_uimm5;
      break;
    case M32R_OPERAND_UIMM16 :
      value = fields->f_uimm16;
      break;
    case M32R_OPERAND_HI16 :
      value = fields->f_hi16;
      break;
    case M32R_OPERAND_SLO16 :
      value = fields->f_simm16;
      break;
    case M32R_OPERAND_ULO16 :
      value = fields->f_uimm16;
      break;
    case M32R_OPERAND_UIMM24 :
      value = fields->f_uimm24;
      break;
    case M32R_OPERAND_DISP8 :
      value = fields->f_disp8;
      break;
    case M32R_OPERAND_DISP16 :
      value = fields->f_disp16;
      break;
    case M32R_OPERAND_DISP24 :
      value = fields->f_disp24;
      break;

    default :
      fprintf (stderr, "Unrecognized field %d while getting operand.\n",
		       opindex);
      abort ();
  }

  return value;
}

