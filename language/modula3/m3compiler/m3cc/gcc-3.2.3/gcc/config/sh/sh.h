/* Definitions of target machine for GNU compiler for Hitachi / SuperH SH.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

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

#ifndef GCC_SH_H
#define GCC_SH_H

#define TARGET_VERSION \
  fputs (" (Hitachi SH)", stderr);

/* Unfortunately, insn-attrtab.c doesn't include insn-codes.h.  We can't
   include it here, because hconfig.h is also included by gencodes.c .  */
/* ??? No longer true.  */
extern int code_for_indirect_jump_scratch;

/* Generate SDB debugging information.  */

#define SDB_DEBUGGING_INFO

/* Output DBX (stabs) debugging information if doing -gstabs.  */

#include "dbxcoff.h"

#define SDB_DELIM ";"

#define CPP_SPEC " \
%{m5-64media|m5-64media-nofpu|m5-32media|m5-32media-nofpu:-D__SHMEDIA__=1} \
%{m5-compact|m5-compact-nofpu:-D__SHMEDIA__=0} \
%{m5-64media|m5-64media-nofpu:-D__SH5__=64 -D__LONG_MAX__=9223372036854775807L} \
%{m5-32media|m5-32media-nofpu|m5-compact|m5-compact-nofpu:-D__SH5__=32} \
%{m5-64media-nofpu|m5-32media-nofpu|m5-compact-nofpu:-D__SH4_NOFPU__} \
%{m1:-D__sh1__} \
%{m2:-D__sh2__} \
%{m3:-D__sh3__} \
%{m3e:-D__SH3E__} \
%{m4-single-only:-D__SH4_SINGLE_ONLY__} \
%{m4-single:-D__SH4_SINGLE__} \
%{m4-nofpu:-D__sh3__ -D__SH4_NOFPU__} \
%{m4:-D__SH4__} \
%{!m1:%{!m2:%{!m3*:%{!m4*:%{!m5*:%(cpp_default_cpu_spec)}}}}} \
%{mhitachi:-D__HITACHI__} \
%(subtarget_cpp_spec) \
%(subtarget_cpp_ptr_spec) \
%(subtarget_cpp_endian_spec) "

#ifndef SUBTARGET_CPP_ENDIAN_SPEC
#define SUBTARGET_CPP_ENDIAN_SPEC "%{ml:-D__LITTLE_ENDIAN__}"
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#ifndef CPP_DEFAULT_CPU_SPEC
#define CPP_DEFAULT_CPU_SPEC "-D__sh1__"
#endif

#ifndef SUBTARGET_CPP_PTR_SPEC
#define SUBTARGET_CPP_PTR_SPEC "\
%{m5-64media|m5-64media-nofpu|m5-32media|m5-32media-nofpu|m5-compact|m5-compact-nofpu:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
%{!m5-64media:%{!m5-64media-nofpu:%{!m5-32media:%{!m5-32media-nofpu:%{!m5-compact:%{!m5-compact-nofpu:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}}}}}} \
"
#endif

#define EXTRA_SPECS						\
  { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC },			\
  { "subtarget_cpp_endian_spec", SUBTARGET_CPP_ENDIAN_SPEC },	\
  { "subtarget_cpp_ptr_spec", SUBTARGET_CPP_PTR_SPEC },		\
  { "cpp_default_cpu_spec", CPP_DEFAULT_CPU_SPEC },

#define CPP_PREDEFINES "-D__sh__ -Acpu=sh -Amachine=sh"

#define ASM_SPEC  "%{ml:-little} %{mrelax:-relax}"

#define LINK_SPEC "%{ml:-m shl} %{mrelax:-relax}"

/* We can not debug without a frame pointer.  */
/* #define CAN_DEBUG_WITHOUT_FP */

#define CONDITIONAL_REGISTER_USAGE do					\
{									\
  int regno;								\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno ++)		\
    if (! VALID_REGISTER_P (regno))					\
      fixed_regs[regno] = call_used_regs[regno] = 1;			\
  /* R8 and R9 are call-clobbered on SH5, but not on earlier SH ABIs.  */ \
  if (TARGET_SH5)							\
    call_used_regs[FIRST_GENERAL_REG + 8]				\
      = call_used_regs[FIRST_GENERAL_REG + 9] = 1;			\
  if (TARGET_SHMEDIA)							\
    {									\
      regno_reg_class[FIRST_GENERAL_REG] = GENERAL_REGS;		\
      CLEAR_HARD_REG_SET (reg_class_contents[FP0_REGS]);		\
      regno_reg_class[FIRST_FP_REG] = FP_REGS;				\
    }									\
  if (flag_pic)								\
    fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;				\
  /* Hitachi saves and restores mac registers on call.  */		\
  if (TARGET_HITACHI && ! TARGET_NOMACSAVE)				\
    {									\
      call_used_regs[MACH_REG] = 0;					\
      call_used_regs[MACL_REG] = 0;					\
    }									\
  if (TARGET_SHMEDIA)							\
    {									\
      for (regno = FIRST_TARGET_REG; regno <= LAST_TARGET_REG; regno ++)\
	if (! fixed_regs[regno] && call_used_regs[regno])		\
	  SET_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], regno);	\
    }									\
  else									\
    for (regno = FIRST_GENERAL_REG; regno <= LAST_GENERAL_REG; regno++)	\
      if (! fixed_regs[regno] && call_used_regs[regno])			\
	SET_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], regno);	\
} while (0)

/* ??? Need to write documentation for all SH options and add it to the
   invoke.texi file.  */

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;
#define ISIZE_BIT      	(1<<1)
#define DALIGN_BIT     	(1<<6)
#define SH1_BIT	       	(1<<8)
#define SH2_BIT	       	(1<<9)
#define SH3_BIT	       	(1<<10)
#define SH3E_BIT	(1<<11)
#define HARD_SH4_BIT	(1<<5)
#define FPU_SINGLE_BIT	(1<<7)
#define SH4_BIT	       	(1<<12)
#define FMOVD_BIT	(1<<4)
#define SH5_BIT		(1<<0)
#define SPACE_BIT 	(1<<13)
#define BIGTABLE_BIT  	(1<<14)
#define RELAX_BIT	(1<<15)
#define USERMODE_BIT	(1<<16)
#define HITACHI_BIT     (1<<22)
#define NOMACSAVE_BIT   (1<<23)
#define PREFERGOT_BIT	(1<<24)
#define PADSTRUCT_BIT  (1<<28)
#define LITTLE_ENDIAN_BIT (1<<29)
#define IEEE_BIT (1<<30)

/* Nonzero if we should dump out instruction size info.  */
#define TARGET_DUMPISIZE  (target_flags & ISIZE_BIT)

/* Nonzero to align doubles on 64 bit boundaries.  */
#define TARGET_ALIGN_DOUBLE (target_flags & DALIGN_BIT)

/* Nonzero if we should generate code using type 1 insns.  */
#define TARGET_SH1 (target_flags & SH1_BIT)

/* Nonzero if we should generate code using type 2 insns.  */
#define TARGET_SH2 (target_flags & SH2_BIT)

/* Nonzero if we should generate code using type 3 insns.  */
#define TARGET_SH3 (target_flags & SH3_BIT)

/* Nonzero if we should generate code using type 3E insns.  */
#define TARGET_SH3E ((target_flags & SH3E_BIT) && (target_flags & SH1_BIT))

/* Nonzero if the cache line size is 32.  */
#define TARGET_CACHE32 (target_flags & HARD_SH4_BIT || TARGET_SH5)

/* Nonzero if we schedule for a superscalar implementation.  */
#define TARGET_SUPERSCALAR (target_flags & HARD_SH4_BIT)

/* Nonzero if the target has separate instruction and data caches.  */
#define TARGET_HARVARD (target_flags & HARD_SH4_BIT)

/* Nonzero if compiling for SH4 hardware (to be used for insn costs etc.)  */
#define TARGET_HARD_SH4 (target_flags & HARD_SH4_BIT)

/* Nonzero if the default precision of th FPU is single */
#define TARGET_FPU_SINGLE (target_flags & FPU_SINGLE_BIT)

/* Nonzero if a double-precision FPU is available.  */
#define TARGET_FPU_DOUBLE (target_flags & SH4_BIT)

/* Nonzero if an FPU is available.  */
#define TARGET_FPU_ANY (TARGET_SH3E || TARGET_FPU_DOUBLE)

/* Nonzero if we should generate code using type 4 insns.  */
#define TARGET_SH4 ((target_flags & SH4_BIT) && (target_flags & SH1_BIT))

/* Nonzero if we should generate code for a SH5 CPU (either ISA).  */
#define TARGET_SH5 (target_flags & SH5_BIT)

/* Nonzero if we should generate code using the SHcompact instruction
   set and 32-bit ABI.  */
#define TARGET_SHCOMPACT (TARGET_SH5 && TARGET_SH1)

/* Nonzero if we should generate code using the SHmedia instruction
   set and ABI.  */
#define TARGET_SHMEDIA (TARGET_SH5 && ! TARGET_SH1)

/* Nonzero if we should generate code using the SHmedia ISA and 32-bit
   ABI.  */
#define TARGET_SHMEDIA32 (TARGET_SH5 && ! TARGET_SH1 \
			  && (target_flags & SH3E_BIT))

/* Nonzero if we should generate code using the SHmedia ISA and 64-bit
   ABI.  */
#define TARGET_SHMEDIA64 (TARGET_SH5 && ! TARGET_SH1 \
 			  && ! (target_flags & SH3E_BIT))

/* Nonzero if we should generate code using SHmedia FPU instructions.  */
#define TARGET_SHMEDIA_FPU (TARGET_SHMEDIA && TARGET_FPU_DOUBLE)
/* Nonzero if we should generate fmovd.  */
#define TARGET_FMOVD (target_flags & FMOVD_BIT)

/* Nonzero if we respect NANs.  */
#define TARGET_IEEE (target_flags & IEEE_BIT)

/* Nonzero if we should generate smaller code rather than faster code.  */
#define TARGET_SMALLCODE   (target_flags & SPACE_BIT)

/* Nonzero to use long jump tables.  */
#define TARGET_BIGTABLE     (target_flags & BIGTABLE_BIT)

/* Nonzero to generate pseudo-ops needed by the assembler and linker
   to do function call relaxing.  */
#define TARGET_RELAX (target_flags & RELAX_BIT)

/* Nonzero if using Hitachi's calling convention.  */
#define TARGET_HITACHI 		(target_flags & HITACHI_BIT)

/* Nonzero if not saving macl/mach when using -mhitachi */
#define TARGET_NOMACSAVE	(target_flags & NOMACSAVE_BIT)

/* Nonzero if padding structures to a multiple of 4 bytes.  This is
   incompatible with Hitachi's compiler, and gives unusual structure layouts
   which confuse programmers.
   ??? This option is not useful, but is retained in case there are people
   who are still relying on it.  It may be deleted in the future.  */
#define TARGET_PADSTRUCT       (target_flags & PADSTRUCT_BIT)

/* Nonzero if generating code for a little endian SH.  */
#define TARGET_LITTLE_ENDIAN     (target_flags & LITTLE_ENDIAN_BIT)

/* Nonzero if we should do everything in userland.  */
#define TARGET_USERMODE		(target_flags & USERMODE_BIT)

/* Nonzero if we should prefer @GOT calls when generating PIC.  */
#define TARGET_PREFERGOT	(target_flags & PREFERGOT_BIT)

/* Reset all target-selection flags.  */
#define TARGET_NONE -(SH1_BIT | SH2_BIT | SH3_BIT | SH3E_BIT | SH4_BIT \
		      | HARD_SH4_BIT | FPU_SINGLE_BIT | SH5_BIT)

#define TARGET_SWITCHES  			\
{ {"1",	        TARGET_NONE, "" },		\
  {"1",	        SH1_BIT, "" },			\
  {"2",	        TARGET_NONE, "" },		\
  {"2",	        SH2_BIT|SH1_BIT, "" },		\
  {"3",	        TARGET_NONE, "" },		\
  {"3",	        SH3_BIT|SH2_BIT|SH1_BIT, "" },	\
  {"3e",	TARGET_NONE, "" },		\
  {"3e",	SH3E_BIT|SH3_BIT|SH2_BIT|SH1_BIT|FPU_SINGLE_BIT, "" },	\
  {"4-single-only",	TARGET_NONE, "" },	\
  {"4-single-only",	SH3E_BIT|SH3_BIT|SH2_BIT|SH1_BIT|HARD_SH4_BIT|FPU_SINGLE_BIT, "" },	\
  {"4-single",	TARGET_NONE, "" },		\
  {"4-single",	SH4_BIT|SH3E_BIT|SH3_BIT|SH2_BIT|SH1_BIT|HARD_SH4_BIT|FPU_SINGLE_BIT, "" },\
  {"4-nofpu",	TARGET_NONE, "" },		\
  {"4-nofpu",	SH3_BIT|SH2_BIT|SH1_BIT|HARD_SH4_BIT, "" },\
  {"4",	        TARGET_NONE, "" },		\
  {"4",	        SH4_BIT|SH3E_BIT|SH3_BIT|SH2_BIT|SH1_BIT|HARD_SH4_BIT, "" }, \
  {"5-64media",	TARGET_NONE, "" },		\
  {"5-64media", SH5_BIT|SH4_BIT, "Generate 64-bit SHmedia code" }, \
  {"5-64media-nofpu", TARGET_NONE, "" },	\
  {"5-64media-nofpu", SH5_BIT, "Generate 64-bit FPU-less SHmedia code" }, \
  {"5-32media",	TARGET_NONE, "" },		\
  {"5-32media", SH5_BIT|SH4_BIT|SH3E_BIT, "Generate 32-bit SHmedia code" }, \
  {"5-32media-nofpu", TARGET_NONE, "" },	\
  {"5-32media-nofpu", SH5_BIT|SH3E_BIT, "Generate 32-bit FPU-less SHmedia code" }, \
  {"5-compact",	TARGET_NONE, "" },		\
  {"5-compact",	SH5_BIT|SH4_BIT|SH3E_BIT|SH3_BIT|SH2_BIT|SH1_BIT|FPU_SINGLE_BIT, "Generate SHcompact code" }, \
  {"5-compact-nofpu", TARGET_NONE, "" },	\
  {"5-compact-nofpu", SH5_BIT|SH3_BIT|SH2_BIT|SH1_BIT, "Generate FPU-less SHcompact code" }, \
  {"b",		-LITTLE_ENDIAN_BIT, "" },  	\
  {"bigtable", 	BIGTABLE_BIT, "" },		\
  {"dalign",  	DALIGN_BIT, "" },		\
  {"fmovd",  	FMOVD_BIT, "" },		\
  {"hitachi",	HITACHI_BIT, "" },		\
  {"nomacsave", NOMACSAVE_BIT, "" },		\
  {"ieee",  	IEEE_BIT, "" },			\
  {"isize", 	ISIZE_BIT, "" },		\
  {"l",		LITTLE_ENDIAN_BIT, "" },  	\
  {"no-ieee",  	-IEEE_BIT, "" },		\
  {"padstruct", PADSTRUCT_BIT, "" },    	\
  {"prefergot",	PREFERGOT_BIT, "" },		\
  {"relax",	RELAX_BIT, "" },		\
  {"space", 	SPACE_BIT, "" },		\
  {"usermode",	USERMODE_BIT, "" },		\
  SUBTARGET_SWITCHES                            \
  {"",   	TARGET_DEFAULT, "" } 		\
}

/* This are meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES

#define TARGET_DEFAULT  (SH1_BIT)

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)				\
do {									\
  if (LEVEL)								\
    flag_omit_frame_pointer = -1;					\
  if (SIZE)								\
    target_flags |= SPACE_BIT;						\
} while (0)

#define ASSEMBLER_DIALECT assembler_dialect

extern int assembler_dialect;

#define OVERRIDE_OPTIONS 						\
do {									\
  int regno;								\
									\
  sh_cpu = CPU_SH1;							\
  assembler_dialect = 0;						\
  if (TARGET_SH2)							\
    sh_cpu = CPU_SH2;							\
  if (TARGET_SH3)							\
    sh_cpu = CPU_SH3;							\
  if (TARGET_SH3E)							\
    sh_cpu = CPU_SH3E;							\
  if (TARGET_SH4)							\
    {									\
      assembler_dialect = 1;						\
      sh_cpu = CPU_SH4;							\
    }									\
  if (TARGET_SH5)							\
    {									\
      sh_cpu = CPU_SH5;							\
      target_flags |= DALIGN_BIT;					\
      if (TARGET_FPU_ANY)						\
	target_flags |= FMOVD_BIT;					\
      if (TARGET_SHMEDIA)						\
	{								\
	  /* There are no delay slots on SHmedia.  */			\
	  flag_delayed_branch = 0;					\
	  /* Relaxation isn't yet supported for SHmedia */		\
	  target_flags &= ~RELAX_BIT;					\
	}								\
      if (profile_flag || profile_arc_flag)				\
	{								\
	  warning ("Profiling is not supported on this target.");	\
	  profile_flag = profile_arc_flag = 0;				\
	}								\
    }									\
  else									\
    {									\
       /* Only the sh64-elf assembler fully supports .quad properly.  */\
       targetm.asm_out.aligned_op.di = NULL;				\
       targetm.asm_out.unaligned_op.di = NULL;				\
    }									\
									\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)		\
    if (! VALID_REGISTER_P (regno))					\
      sh_register_names[regno][0] = '\0';				\
									\
  for (regno = 0; regno < ADDREGNAMES_SIZE; regno++)			\
    if (! VALID_REGISTER_P (ADDREGNAMES_REGNO (regno)))			\
      sh_additional_register_names[regno][0] = '\0';			\
									\
  if (flag_omit_frame_pointer < 0)					\
   {									\
     /* The debugging information is sufficient,			\
        but gdb doesn't implement this yet */				\
     if (0)								\
      flag_omit_frame_pointer						\
        = (PREFERRED_DEBUGGING_TYPE == DWARF_DEBUG			\
	   || PREFERRED_DEBUGGING_TYPE == DWARF2_DEBUG);		\
     else								\
      flag_omit_frame_pointer = 0;					\
   }									\
									\
  if (flag_pic && ! TARGET_PREFERGOT)					\
    flag_no_function_cse = 1;						\
									\
  /* Never run scheduling before reload, since that can			\
     break global alloc, and generates slower code anyway due		\
     to the pressure on R0.  */						\
  flag_schedule_insns = 0;						\
} while (0)

/* Target machine storage layout.  */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */

#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#if defined(__LITTLE_ENDIAN__)
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#endif

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT  8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD  (TARGET_SHMEDIA ? 64 : 32)
#define MAX_BITS_PER_WORD 64

#define MAX_LONG_TYPE_SIZE MAX_BITS_PER_WORD

/* Width in bits of an `int'.  We want just 32-bits, even if words are
   longer. */
#define INT_TYPE_SIZE 32

/* Width in bits of a `long'.  */
#define LONG_TYPE_SIZE (TARGET_SHMEDIA64 ? 64 : 32)

/* Width in bits of a `long long'.  */
#define LONG_LONG_TYPE_SIZE 64

/* Width in bits of a `long double'.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD	(TARGET_SHMEDIA ? 8 : 4)
#define MIN_UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE  (TARGET_SHMEDIA64 ? 64 : 32)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY  	(TARGET_SH5 ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  BIGGEST_ALIGNMENT

/* The log (base 2) of the cache line size, in bytes.  Processors prior to
   SH2 have no actual cache, but they fetch code in chunks of 4 bytes.
   The SH2/3 have 16 byte cache lines, and the SH4 has a 32 byte cache line */
#define CACHE_LOG (TARGET_CACHE32 ? 5 : TARGET_SH2 ? 4 : 2)

/* Allocation boundary (in *bits*) for the code of a function.
   32 bit alignment is faster, because instructions are always fetched as a
   pair from a longword boundary.  */
#define FUNCTION_BOUNDARY  \
  (TARGET_SMALLCODE ? 16 << TARGET_SHMEDIA : (1 << CACHE_LOG) * 8)

/* On SH5, the lowest bit is used to indicate SHmedia functions, so
   the vbit must go into the delta field of
   pointers-to-member-functions.  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION \
  (TARGET_SH5 ? ptrmemfunc_vbit_in_delta : ptrmemfunc_vbit_in_pfn)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  (TARGET_ALIGN_DOUBLE ? 64 : 32)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT (TARGET_SH5 ? 64 : 32)

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)	\
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
    ? FASTEST_ALIGNMENT : (ALIGN))

#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 128
#endif

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Number of bits which any structure or union's size must be a
   multiple of.  Each structure or union's size is rounded up to a
   multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY (TARGET_PADSTRUCT ? 32 : 8)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* If LABEL_AFTER_BARRIER demands an alignment, return its base 2 logarithm.  */
#define LABEL_ALIGN_AFTER_BARRIER(LABEL_AFTER_BARRIER) \
  barrier_align (LABEL_AFTER_BARRIER)

#define LOOP_ALIGN(A_LABEL) \
  ((! optimize || TARGET_HARVARD || TARGET_SMALLCODE) \
   ? 0 : sh_loop_align (A_LABEL))

#define LABEL_ALIGN(A_LABEL) \
(									\
  (PREV_INSN (A_LABEL)							\
   && GET_CODE (PREV_INSN (A_LABEL)) == INSN				\
   && GET_CODE (PATTERN (PREV_INSN (A_LABEL))) == UNSPEC_VOLATILE	\
   && XINT (PATTERN (PREV_INSN (A_LABEL)), 1) == UNSPECV_ALIGN)		\
   /* explicit alignment insn in constant tables.  */			\
  ? INTVAL (XVECEXP (PATTERN (PREV_INSN (A_LABEL)), 0, 0))		\
  : 0)

/* Jump tables must be 32 bit aligned, no matter the size of the element.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 2

/* The base two logarithm of the known minimum alignment of an insn length.  */
#define INSN_LENGTH_ALIGNMENT(A_INSN)					\
  (GET_CODE (A_INSN) == INSN						\
   ? 1 << TARGET_SHMEDIA						\
   : GET_CODE (A_INSN) == JUMP_INSN || GET_CODE (A_INSN) == CALL_INSN	\
   ? 1 << TARGET_SHMEDIA						\
   : CACHE_LOG)

/* Standard register usage.  */

/* Register allocation for the Hitachi calling convention:

        r0		arg return
	r1..r3          scratch
	r4..r7		args in
	r8..r13		call saved
	r14		frame pointer/call saved
	r15		stack pointer
	ap		arg pointer (doesn't really exist, always eliminated)
	pr		subroutine return address
	t               t bit
	mach		multiply/accumulate result, high part
	macl		multiply/accumulate result, low part.
	fpul		fp/int communication register
	rap		return address pointer register
	fr0		fp arg return
	fr1..fr3	scratch floating point registers
	fr4..fr11	fp args in
	fr12..fr15	call saved floating point registers  */

#define MAX_REGISTER_NAME_LENGTH 5
extern char sh_register_names[][MAX_REGISTER_NAME_LENGTH + 1];

#define SH_REGISTER_NAMES_INITIALIZER					\
{				                   			\
  "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7", 	\
  "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "r15",	\
  "r16",  "r17",  "r18",  "r19",  "r20",  "r21",  "r22",  "r23",	\
  "r24",  "r25",  "r26",  "r27",  "r28",  "r29",  "r30",  "r31",	\
  "r32",  "r33",  "r34",  "r35",  "r36",  "r37",  "r38",  "r39", 	\
  "r40",  "r41",  "r42",  "r43",  "r44",  "r45",  "r46",  "r47",	\
  "r48",  "r49",  "r50",  "r51",  "r52",  "r53",  "r54",  "r55",	\
  "r56",  "r57",  "r58",  "r59",  "r60",  "r61",  "r62",  "r63",	\
  "fr0",  "fr1",  "fr2",  "fr3",  "fr4",  "fr5",  "fr6",  "fr7", 	\
  "fr8",  "fr9",  "fr10", "fr11", "fr12", "fr13", "fr14", "fr15",	\
  "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23",	\
  "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31",	\
  "fr32", "fr33", "fr34", "fr35", "fr36", "fr37", "fr38", "fr39", 	\
  "fr40", "fr41", "fr42", "fr43", "fr44", "fr45", "fr46", "fr47",	\
  "fr48", "fr49", "fr50", "fr51", "fr52", "fr53", "fr54", "fr55",	\
  "fr56", "fr57", "fr58", "fr59", "fr60", "fr61", "fr62", "fr63",	\
  "tr0",  "tr1",  "tr2",  "tr3",  "tr4",  "tr5",  "tr6",  "tr7", 	\
  "xd0",  "xd2",  "xd4",  "xd6",  "xd8",  "xd10", "xd12", "xd14",	\
  "gbr",  "ap",	  "pr",   "t",    "mach", "macl", "fpul", "fpscr",	\
  "rap"									\
}

#define DEBUG_REGISTER_NAMES SH_REGISTER_NAMES_INITIALIZER

#define REGNAMES_ARR_INDEX_1(index) \
  (sh_register_names[index])
#define REGNAMES_ARR_INDEX_2(index) \
  REGNAMES_ARR_INDEX_1 ((index)), REGNAMES_ARR_INDEX_1 ((index)+1)
#define REGNAMES_ARR_INDEX_4(index) \
  REGNAMES_ARR_INDEX_2 ((index)), REGNAMES_ARR_INDEX_2 ((index)+2)
#define REGNAMES_ARR_INDEX_8(index) \
  REGNAMES_ARR_INDEX_4 ((index)), REGNAMES_ARR_INDEX_4 ((index)+4)
#define REGNAMES_ARR_INDEX_16(index) \
  REGNAMES_ARR_INDEX_8 ((index)), REGNAMES_ARR_INDEX_8 ((index)+8)
#define REGNAMES_ARR_INDEX_32(index) \
  REGNAMES_ARR_INDEX_16 ((index)), REGNAMES_ARR_INDEX_16 ((index)+16)
#define REGNAMES_ARR_INDEX_64(index) \
  REGNAMES_ARR_INDEX_32 ((index)), REGNAMES_ARR_INDEX_32 ((index)+32)

#define REGISTER_NAMES \
{ \
  REGNAMES_ARR_INDEX_64 (0), \
  REGNAMES_ARR_INDEX_64 (64), \
  REGNAMES_ARR_INDEX_8 (128), \
  REGNAMES_ARR_INDEX_8 (136), \
  REGNAMES_ARR_INDEX_8 (144), \
  REGNAMES_ARR_INDEX_1 (152) \
}

#define ADDREGNAMES_SIZE 32
#define MAX_ADDITIONAL_REGISTER_NAME_LENGTH 4
extern char sh_additional_register_names[ADDREGNAMES_SIZE] \
  [MAX_ADDITIONAL_REGISTER_NAME_LENGTH + 1];

#define SH_ADDITIONAL_REGISTER_NAMES_INITIALIZER			\
{									\
  "dr0",  "dr2",  "dr4",  "dr6",  "dr8",  "dr10", "dr12", "dr14",	\
  "dr16", "dr18", "dr20", "dr22", "dr24", "dr26", "dr28", "dr30",	\
  "dr32", "dr34", "dr36", "dr38", "dr40", "dr42", "dr44", "dr46",	\
  "dr48", "dr50", "dr52", "dr54", "dr56", "dr58", "dr60", "dr62"	\
}

#define ADDREGNAMES_REGNO(index) \
  ((index < 32) ? (FIRST_FP_REG + (index) * 2) \
   : (-1))

#define ADDREGNAMES_ARR_INDEX_1(index) \
  { (sh_additional_register_names[index]), ADDREGNAMES_REGNO (index) }
#define ADDREGNAMES_ARR_INDEX_2(index) \
  ADDREGNAMES_ARR_INDEX_1 ((index)), ADDREGNAMES_ARR_INDEX_1 ((index)+1)
#define ADDREGNAMES_ARR_INDEX_4(index) \
  ADDREGNAMES_ARR_INDEX_2 ((index)), ADDREGNAMES_ARR_INDEX_2 ((index)+2)
#define ADDREGNAMES_ARR_INDEX_8(index) \
  ADDREGNAMES_ARR_INDEX_4 ((index)), ADDREGNAMES_ARR_INDEX_4 ((index)+4)
#define ADDREGNAMES_ARR_INDEX_16(index) \
  ADDREGNAMES_ARR_INDEX_8 ((index)), ADDREGNAMES_ARR_INDEX_8 ((index)+8)
#define ADDREGNAMES_ARR_INDEX_32(index) \
  ADDREGNAMES_ARR_INDEX_16 ((index)), ADDREGNAMES_ARR_INDEX_16 ((index)+16)

#define ADDITIONAL_REGISTER_NAMES \
{					\
  ADDREGNAMES_ARR_INDEX_32 (0)		\
}

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

/* There are many other relevant definitions in sh.md's md_constants.  */

#define FIRST_GENERAL_REG R0_REG
#define LAST_GENERAL_REG (FIRST_GENERAL_REG + (TARGET_SHMEDIA ? 63 : 15))
#define FIRST_FP_REG DR0_REG
#define LAST_FP_REG  (FIRST_FP_REG + \
		      (TARGET_SHMEDIA_FPU ? 63 : TARGET_SH3E ? 15 : -1))
#define FIRST_XD_REG XD0_REG
#define LAST_XD_REG  (FIRST_XD_REG + ((TARGET_SH4 && TARGET_FMOVD) ? 7 : -1))
#define FIRST_TARGET_REG TR0_REG
#define LAST_TARGET_REG  (FIRST_TARGET_REG + (TARGET_SHMEDIA ? 7 : -1))

#define GENERAL_REGISTER_P(REGNO) \
  IN_RANGE ((REGNO), FIRST_GENERAL_REG, LAST_GENERAL_REG)

#define GENERAL_OR_AP_REGISTER_P(REGNO) \
  (GENERAL_REGISTER_P (REGNO) || ((REGNO) == AP_REG))

#define FP_REGISTER_P(REGNO) \
  ((REGNO) >= FIRST_FP_REG && (REGNO) <= LAST_FP_REG)

#define XD_REGISTER_P(REGNO) \
  ((REGNO) >= FIRST_XD_REG && (REGNO) <= LAST_XD_REG)

#define FP_OR_XD_REGISTER_P(REGNO) \
  (FP_REGISTER_P (REGNO) || XD_REGISTER_P (REGNO))

#define FP_ANY_REGISTER_P(REGNO) \
  (FP_REGISTER_P (REGNO) || XD_REGISTER_P (REGNO) || (REGNO) == FPUL_REG)

#define SPECIAL_REGISTER_P(REGNO) \
  ((REGNO) == GBR_REG || (REGNO) == T_REG \
   || (REGNO) == MACH_REG || (REGNO) == MACL_REG)

#define TARGET_REGISTER_P(REGNO) \
  ((REGNO) >= FIRST_TARGET_REG && (REGNO) <= LAST_TARGET_REG)

#define SHMEDIA_REGISTER_P(REGNO) \
  (GENERAL_REGISTER_P (REGNO) || FP_REGISTER_P (REGNO) \
   || TARGET_REGISTER_P (REGNO))

/* This is to be used in CONDITIONAL_REGISTER_USAGE, to mark registers
   that should be fixed.  */
#define VALID_REGISTER_P(REGNO) \
  (SHMEDIA_REGISTER_P (REGNO) || XD_REGISTER_P (REGNO) \
   || (REGNO) == AP_REG || (REGNO) == RAP_REG \
   || (TARGET_SH1 && (SPECIAL_REGISTER_P (REGNO) || (REGNO) == PR_REG)) \
   || (TARGET_SH3E && (REGNO) == FPUL_REG))

/* The mode that should be generally used to store a register by
   itself in the stack, or to load it back.  */
#define REGISTER_NATURAL_MODE(REGNO) \
  (FP_REGISTER_P (REGNO) ? SFmode \
   : XD_REGISTER_P (REGNO) ? DFmode \
   : TARGET_SHMEDIA && ! HARD_REGNO_CALL_PART_CLOBBERED ((REGNO), DImode) \
   ? DImode \
   : SImode)

#define FIRST_PSEUDO_REGISTER 153

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   Mach register is fixed 'cause it's only 10 bits wide for SH1.
   It is 32 bits wide for SH2.  */

#define FIXED_REGISTERS  						\
{				                   			\
/* Regular registers.  */						\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      1,		\
  /* r16 is reserved, r18 is the former pr.  */				\
  1,      0,      0,      0,      0,      0,      0,      0,		\
  /* r24 is reserved for the OS; r25, for the assembler or linker.  */	\
  /* r26 is a global variable data pointer; r27 is for constants.  */	\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      1,		\
/* FP registers.  */							\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/* Branch target registers.  */						\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/* XD registers.  */							\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/*"gbr",  "ap",	  "pr",   "t",    "mach", "macl", "fpul", "fpscr", */	\
  1,      1,      1,      1,      1,      1,      0,      1,		\
/*"rap" */								\
  1,									\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS  						\
{				                   			\
/* Regular registers.  */						\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  /* R8 and R9 are call-clobbered on SH5, but not on earlier SH ABIs.	\
     Only the lower 32bits of R10-R14 are guaranteed to be preserved	\
     across SH5 function calls.  */					\
  0,      0,      0,      0,      0,      0,      0,      1,		\
  1,      1,      0,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      1,      1,      1,      1,		\
/* FP registers.  */							\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      1,      1,      1,      1,		\
  1,      1,      1,      1,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
  0,      0,      0,      0,      0,      0,      0,      0,		\
/* Branch target registers.  */						\
  1,      1,      1,      1,      1,      0,      0,      0,		\
/* XD registers.  */							\
  1,      1,      1,      1,      1,      1,      0,      0,		\
/*"gbr",  "ap",	  "pr",   "t",    "mach", "macl", "fpul", "fpscr", */	\
  1,      1,      0,      1,      1,      1,      1,      1,		\
/*"rap" */								\
  1,									\
}

/* Only the lower 32-bits of R10-R14 are guaranteed to be preserved
   across SHcompact function calls.  We can't tell whether a called
   function is SHmedia or SHcompact, so we assume it may be when
   compiling SHmedia code with the 32-bit ABI, since that's the only
   ABI that can be linked with SHcompact code.  */
#define HARD_REGNO_CALL_PART_CLOBBERED(REGNO,MODE) \
  (TARGET_SHMEDIA32 \
   && GET_MODE_SIZE (MODE) > 4 \
   && (((REGNO) >= FIRST_GENERAL_REG + 10 \
        && (REGNO) <= FIRST_GENERAL_REG + 14) \
       || (REGNO) == PR_MEDIA_REG))

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the SH all but the XD regs are UNITS_PER_WORD bits wide.  */

#define HARD_REGNO_NREGS(REGNO, MODE) \
   (XD_REGISTER_P (REGNO) \
    ? (GET_MODE_SIZE (MODE) / (2 * UNITS_PER_WORD)) \
    : (TARGET_SHMEDIA && FP_REGISTER_P (REGNO)) \
    ? ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD/2 - 1) / (UNITS_PER_WORD/2)) \
    : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)) \

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   We can allow any mode in any general register.  The special registers
   only allow SImode.  Don't allow any mode in the PR.  */

/* We cannot hold DCmode values in the XD registers because alter_reg
   handles subregs of them incorrectly.  We could work around this by
   spacing the XD registers like the DR registers, but this would require
   additional memory in every compilation to hold larger register vectors.
   We could hold SFmode / SCmode values in XD registers, but that
   would require a tertiary reload when reloading from / to memory,
   and a secondary reload to reload from / to general regs; that
   seems to be a loosing proposition.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)		\
  (SPECIAL_REGISTER_P (REGNO) ? (MODE) == SImode \
   : (REGNO) == FPUL_REG ? (MODE) == SImode || (MODE) == SFmode	\
   : FP_REGISTER_P (REGNO) && (MODE) == SFmode \
   ? 1 \
   : (MODE) == V2SFmode \
   ? (FP_REGISTER_P (REGNO) && ((REGNO) - FIRST_FP_REG) % 2 == 0) \
   : (MODE) == V4SFmode \
   ? (FP_REGISTER_P (REGNO) && ((REGNO) - FIRST_FP_REG) % 4 == 0) \
   : (MODE) == V16SFmode \
   ? (TARGET_SHMEDIA \
      ? (FP_REGISTER_P (REGNO) && ((REGNO) - FIRST_FP_REG) % 16 == 0) \
      : (REGNO) == FIRST_XD_REG) \
   : FP_REGISTER_P (REGNO) \
   ? ((MODE) == SFmode \
      || (TARGET_SHMEDIA && (MODE) == SImode) \
      || ((TARGET_SH3E || TARGET_SHMEDIA) && (MODE) == SCmode) \
      || (((TARGET_SH4 && (MODE) == DFmode) || (MODE) == DCmode \
	   || (TARGET_SHMEDIA && ((MODE) == DFmode || (MODE) == DImode \
				  || (MODE) == V2SFmode))) \
	  && (((REGNO) - FIRST_FP_REG) & 1) == 0)) \
   : XD_REGISTER_P (REGNO) \
   ? (MODE) == DFmode \
   : TARGET_REGISTER_P (REGNO) \
   ? ((MODE) == DImode || (MODE) == SImode) \
   : (REGNO) == PR_REG ? 0			\
   : (REGNO) == FPSCR_REG ? (MODE) == PSImode \
   : 1)

/* Value is 1 if MODE is a supported vector mode.  */
#define VECTOR_MODE_SUPPORTED_P(MODE) \
  (TARGET_FPU_ANY \
   && ((MODE) == V2SFmode || (MODE) == V4SFmode || (MODE) == V16SFmode))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.
   That's the case for xd registers: we don't hold SFmode values in
   them, so we can't tie an SFmode pseudos with one in another
   floating-point mode.  */

#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) == (MODE2) \
   || (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2) \
       && (TARGET_SHMEDIA ? ((GET_MODE_SIZE (MODE1) <= 4) \
			      && (GET_MODE_SIZE (MODE2) <= 4)) \
			  : ((MODE1) != SFmode && (MODE2) != SFmode))))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Define this if the program counter is overloaded on a register.  */
/* #define PC_REGNUM		15*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM	SP_REG

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM	FP_REG

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM RAP_REG

/* Register to hold the addressing base for position independent
   code access to data items.  */
#define PIC_OFFSET_TABLE_REGNUM	PIC_REG

#define GOT_SYMBOL_NAME "*_GLOBAL_OFFSET_TABLE_"

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.  */

#define FRAME_POINTER_REQUIRED	0

/* Definitions for register eliminations.

   We have three registers that can be eliminated on the SH.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.
   Third, there is the return address pointer, which can also be replaced
   with either the stack or the frame pointer.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

/* If you add any registers here that are not actually hard registers,
   and that have any alternative of elimination that doesn't always
   apply, you need to amend calc_live_regs to exclude it, because
   reload spills all eliminable registers where it sees an
   can_eliminate == 0 entry, thus making them 'live' .
   If you add any hard registers that can be eliminated in different
   ways, you have to patch reload to spill them only when all alternatives
   of elimination fail.  */

#define ELIMINABLE_REGS						\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { RETURN_ADDRESS_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},}

/* Given FROM and TO register numbers, say whether this elimination
   is allowed.  */
#define CAN_ELIMINATE(FROM, TO) \
  (!((FROM) == FRAME_POINTER_REGNUM && FRAME_POINTER_REQUIRED))

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = initial_elimination_offset ((FROM), (TO))

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	AP_REG

/* Register in which the static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM	(TARGET_SH5 ? 1 : 3)

/* The register in which a struct value address is passed.  */

#define STRUCT_VALUE_REGNUM 2

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */

/* The Hitachi calling convention doesn't quite fit into this scheme since
   the address is passed like an invisible argument, but one that is always
   passed in memory.  */
#define STRUCT_VALUE \
  (TARGET_HITACHI ? 0 : gen_rtx_REG (Pmode, STRUCT_VALUE_REGNUM))

#define RETURN_IN_MEMORY(TYPE) \
  (TARGET_SH5 \
   ? ((TYPE_MODE (TYPE) == BLKmode \
       ? int_size_in_bytes (TYPE) \
       : GET_MODE_SIZE (TYPE_MODE (TYPE))) > 8) \
   : (TYPE_MODE (TYPE) == BLKmode \
      || TARGET_HITACHI && TREE_CODE (TYPE) == RECORD_TYPE))

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */

#define DEFAULT_PCC_STRUCT_RETURN 0

#define SHMEDIA_REGS_STACK_ADJUST() \
  (TARGET_SHCOMPACT && current_function_has_nonlocal_label \
   ? (8 * (/* r28-r35 */ 8 + /* r44-r59 */ 16 + /* tr5-tr7 */ 3) \
      + (TARGET_FPU_ANY ? 4 * (/* fr36 - fr63 */ 28) : 0)) \
   : 0)


/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

/* The SH has two sorts of general registers, R0 and the rest.  R0 can
   be used as the destination of some of the arithmetic ops. There are
   also some special purpose registers; the T bit register, the
   Procedure Return Register and the Multiply Accumulate Registers.  */
/* Place GENERAL_REGS after FPUL_REGS so that it will be preferred by
   reg_class_subunion.  We don't want to have an actual union class
   of these, because it would only be used when both classes are calculated
   to give the same cost, but there is only one FPUL register.
   Besides, regclass fails to notice the different REGISTER_MOVE_COSTS
   applying to the actual instruction alternative considered.  E.g., the
   y/r alternative of movsi_ie is considered to have no more cost that
   the r/r alternative, which is patently untrue.  */

enum reg_class
{
  NO_REGS,
  R0_REGS,
  PR_REGS,
  T_REGS,
  MAC_REGS,
  FPUL_REGS,
  SIBCALL_REGS,
  GENERAL_REGS,
  FP0_REGS,
  FP_REGS,
  DF_REGS,
  FPSCR_REGS,
  GENERAL_FP_REGS,
  TARGET_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES	\
{			\
  "NO_REGS",		\
  "R0_REGS",		\
  "PR_REGS",		\
  "T_REGS",		\
  "MAC_REGS",		\
  "FPUL_REGS",		\
  "SIBCALL_REGS",	\
  "GENERAL_REGS",	\
  "FP0_REGS",		\
  "FP_REGS",		\
  "DF_REGS",		\
  "FPSCR_REGS",		\
  "GENERAL_FP_REGS",	\
  "TARGET_REGS",	\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS						\
{									\
/* NO_REGS:  */								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
/* R0_REGS:  */								\
  { 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
/* PR_REGS:  */								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00040000 },	\
/* T_REGS:  */								\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00080000 },	\
/* MAC_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00300000 },	\
/* FPUL_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00400000 },	\
/* SIBCALL_REGS: Initialized in CONDITIONAL_REGISTER_USAGE.  */	\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 },	\
/* GENERAL_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0x00000000, 0x00000000, 0x01020000 },	\
/* FP0_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000 },	\
/* FP_REGS:  */								\
  { 0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0x00000000 },	\
/* DF_REGS:  */								\
  { 0x00000000, 0x00000000, 0xffffffff, 0xffffffff, 0x0000ff00 },	\
/* FPSCR_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00800000 },	\
/* GENERAL_FP_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x0102ff00 },	\
/* TARGET_REGS:  */							\
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x000000ff },	\
/* ALL_REGS:  */							\
  { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x01ffffff },	\
}									 

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern int regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[(REGNO)]

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers.  */

#define SMALL_REGISTER_CLASSES (! TARGET_SHMEDIA)

/* The order in which register should be allocated.  */
/* Sometimes FP0_REGS becomes the preferred class of a floating point pseudo,
   and GENERAL_FP_REGS the alternate class.  Since FP0 is likely to be
   spilled or used otherwise, we better have the FP_REGS allocated first.  */
#define REG_ALLOC_ORDER \
  { 65, 66, 67, 68, 69, 70, 71, 64, \
    72, 73, 74, 75, 76, 77, 78, 79, \
   136,137,138,139,140,141,142,143, \
    80, 81, 82, 83, 84, 85, 86, 87, \
    88, 89, 90, 91, 92, 93, 94, 95, \
    96, 97, 98, 99,100,101,102,103, \
   104,105,106,107,108,109,110,111, \
   112,113,114,115,116,117,118,119, \
   120,121,122,123,124,125,126,127, \
   151,  1,  2,  3,  7,  6,  5,  4, \
     0,  8,  9, 10, 11, 12, 13, 14, \
    16, 17, 18, 19, 20, 21, 22, 23, \
    24, 25, 26, 27, 28, 29, 30, 31, \
    32, 33, 34, 35, 36, 37, 38, 39, \
    40, 41, 42, 43, 44, 45, 46, 47, \
    48, 49, 50, 51, 52, 53, 54, 55, \
    56, 57, 58, 59, 60, 61, 62, 63, \
   150, 15,145,146,147,144,148,149, \
   128,129,130,131,132,133,134,135, \
   152 }

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  (TARGET_SHMEDIA ? GENERAL_REGS : R0_REGS)
#define BASE_REG_CLASS	 GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine
   description.  */
extern const enum reg_class reg_class_from_letter[];

#define REG_CLASS_FROM_LETTER(C) \
   ( ISLOWER (C) ? reg_class_from_letter[(C)-'a'] : NO_REGS )

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.
	I: arithmetic operand -127..128, as used in add, sub, etc
	J: arithmetic operand -32768..32767, as used in SHmedia movi and shori
	K: shift operand 1,2,8 or 16
	L: logical operand 0..255, as used in and, or, etc.
	M: constant 1
	N: constant 0
	O: arithmetic operand -32..31, as used in SHmedia beqi, bnei and xori
	P: arithmetic operand -512..511, as used in SHmedia andi, ori
*/

#define CONST_OK_FOR_I(VALUE) (((HOST_WIDE_INT)(VALUE))>= -128 \
			       && ((HOST_WIDE_INT)(VALUE)) <= 127)
#define CONST_OK_FOR_J(VALUE) (((HOST_WIDE_INT)(VALUE)) >= -32768 \
			       && ((HOST_WIDE_INT)(VALUE)) <= 32767)
#define CONST_OK_FOR_K(VALUE) ((VALUE)==1||(VALUE)==2||(VALUE)==8||(VALUE)==16)
#define CONST_OK_FOR_L(VALUE) (((HOST_WIDE_INT)(VALUE))>= 0 \
			       && ((HOST_WIDE_INT)(VALUE)) <= 255)
#define CONST_OK_FOR_M(VALUE) ((VALUE)==1)
#define CONST_OK_FOR_N(VALUE) ((VALUE)==0)
#define CONST_OK_FOR_O(VALUE) (((HOST_WIDE_INT)(VALUE)) >= -32 \
			       && ((HOST_WIDE_INT)(VALUE)) <= 31)
#define CONST_OK_FOR_P(VALUE) (((HOST_WIDE_INT)(VALUE)) >= -512 \
			       && ((HOST_WIDE_INT)(VALUE)) <= 511)
#define CONST_OK_FOR_LETTER_P(VALUE, C)		\
     ((C) == 'I' ? CONST_OK_FOR_I (VALUE)	\
    : (C) == 'J' ? CONST_OK_FOR_J (VALUE)	\
    : (C) == 'K' ? CONST_OK_FOR_K (VALUE)	\
    : (C) == 'L' ? CONST_OK_FOR_L (VALUE)	\
    : (C) == 'M' ? CONST_OK_FOR_M (VALUE)	\
    : (C) == 'N' ? CONST_OK_FOR_N (VALUE)	\
    : (C) == 'O' ? CONST_OK_FOR_O (VALUE)	\
    : (C) == 'P' ? CONST_OK_FOR_P (VALUE)	\
    : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)		\
((C) == 'G' ? (fp_zero_operand (VALUE) && fldi_ok ())	\
 : (C) == 'H' ? (fp_one_operand (VALUE) && fldi_ok ())	\
 : (C) == 'F')

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  ((CLASS) == NO_REGS && TARGET_SHMEDIA \
   && (GET_CODE (X) == CONST_DOUBLE \
       || GET_CODE (X) == SYMBOL_REF) \
   ? GENERAL_REGS \
   : (CLASS)) \

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS,MODE,X) \
  ((((((CLASS) == FP_REGS || (CLASS) == FP0_REGS			\
	|| (CLASS) == DF_REGS)						\
      && (GET_CODE (X) == REG && GENERAL_OR_AP_REGISTER_P (REGNO (X))))	\
     || (((CLASS) == GENERAL_REGS || (CLASS) == R0_REGS)		\
	 && GET_CODE (X) == REG						\
	 && FP_REGISTER_P (REGNO (X))))					\
    && ! TARGET_SHMEDIA							\
    && MODE == SFmode)							\
   ? FPUL_REGS								\
   : ((CLASS) == FPUL_REGS						\
      && (GET_CODE (X) == MEM						\
	  || (GET_CODE (X) == REG					\
	      && (REGNO (X) >= FIRST_PSEUDO_REGISTER			\
		  || REGNO (X) == T_REG					\
		  || system_reg_operand (X, VOIDmode)))))		\
   ? GENERAL_REGS							\
   : (((CLASS) == FP_REGS || (CLASS) == DF_REGS) && TARGET_SHMEDIA	\
      && immediate_operand ((X), (MODE)))				\
   ? GENERAL_REGS							\
   : ((CLASS) == TARGET_REGS						\
      || (TARGET_SHMEDIA && (CLASS) == SIBCALL_REGS))			\
   ? ((target_operand ((X), (MODE))					\
       && ! target_reg_operand ((X), (MODE)))				\
      ? NO_REGS : GENERAL_REGS)						\
   : (((CLASS) == MAC_REGS || (CLASS) == PR_REGS)			\
      && GET_CODE (X) == REG && ! GENERAL_REGISTER_P (REGNO (X))	\
      && (CLASS) != REGNO_REG_CLASS (REGNO (X)))			\
   ? GENERAL_REGS : NO_REGS)

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS,MODE,X)  \
  ((((CLASS) == FP_REGS || (CLASS) == FP0_REGS || (CLASS) == DF_REGS)	\
    && ! TARGET_SHMEDIA							\
    && immediate_operand ((X), (MODE))					\
    && ! ((fp_zero_operand (X) || fp_one_operand (X))			\
	  && (MODE) == SFmode && fldi_ok ()))				\
   ? R0_REGS								\
   : (CLASS == FPUL_REGS						\
      && ((GET_CODE (X) == REG						\
          && (REGNO (X) == MACL_REG || REGNO (X) == MACH_REG		\
	      || REGNO (X) == T_REG))))					\
   ? GENERAL_REGS							\
   : CLASS == FPUL_REGS && immediate_operand ((X), (MODE))		\
   ? (GET_CODE (X) == CONST_INT && CONST_OK_FOR_I (INTVAL (X))		\
      ? GENERAL_REGS							\
      : R0_REGS)							\
   : (CLASS == FPSCR_REGS						\
      && ((GET_CODE (X) == REG && REGNO (X) >= FIRST_PSEUDO_REGISTER)	\
	  || (GET_CODE (X) == MEM && GET_CODE (XEXP ((X), 0)) == PLUS)))\
   ? GENERAL_REGS							\
   : SECONDARY_OUTPUT_RELOAD_CLASS((CLASS),(MODE),(X)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On SH this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
     ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the mode of the object illegally.  */

#define CLASS_CANNOT_CHANGE_MODE        DF_REGS

/* Defines illegal mode changes for CLASS_CANNOT_CHANGE_MODE.  */

#define CLASS_CANNOT_CHANGE_MODE_P(FROM,TO) \
  (GET_MODE_SIZE (FROM) != GET_MODE_SIZE (TO))

/* Stack layout; function entry, exit and calling.  */

/* Define the number of registers that can hold parameters.
   These macros are used only in other macro definitions below.  */

#define NPARM_REGS(MODE) \
  (TARGET_FPU_ANY && (MODE) == SFmode \
   ? (TARGET_SH5 ? 12 : 8) \
   : TARGET_SH4 && (GET_MODE_CLASS (MODE) == MODE_FLOAT \
		    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
   ? (TARGET_SH5 ? 12 : 8) \
   : (TARGET_SH5 ? 8 : 4))

#define FIRST_PARM_REG (FIRST_GENERAL_REG + (TARGET_SH5 ? 2 : 4))
#define FIRST_RET_REG  (FIRST_GENERAL_REG + (TARGET_SH5 ? 2 : 0))

#define FIRST_FP_PARM_REG (FIRST_FP_REG + (TARGET_SH5 ? 0 : 4))
#define FIRST_FP_RET_REG FIRST_FP_REG

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/*  Define this macro if the addresses of local variable slots are at
    negative offsets from the frame pointer.

    The SH only has positive indexes, so grow the frame up.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */
#define STARTING_FRAME_OFFSET  0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
/* Don't define PUSH_ROUNDING, since the hardware doesn't do this.
   When PUSH_ROUNDING is not defined, PARM_BOUNDARY will cause gcc to
   do correct alignment.  */
#if 0
#define PUSH_ROUNDING(NPUSHED)  (((NPUSHED) + 3) & ~3)
#endif

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  0

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the SH, the caller does not pop any of its arguments that were passed
   on the stack.  */
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)  0

/* Value is the number of bytes of arguments automatically popped when
   calling a subroutine.
   CUM is the accumulated argument list.

   On SHcompact, the call trampoline pops arguments off the stack.  */
#define CALL_POPS_ARGS(CUM) (TARGET_SHCOMPACT ? (CUM).stack_regs * 8 : 0)

/* Nonzero if we do not know how to pass TYPE solely in registers.
   Values that come in registers with inconvenient padding are stored
   to memory at the function start.  */

#define MUST_PASS_IN_STACK(MODE,TYPE)			\
  ((TYPE) != 0						\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
       || TREE_ADDRESSABLE (TYPE)))
/* Some subroutine macros specific to this machine.  */

#define BASE_RETURN_VALUE_REG(MODE) \
  ((TARGET_FPU_ANY && ((MODE) == SFmode))			\
   ? FIRST_FP_RET_REG					\
   : TARGET_FPU_ANY && (MODE) == SCmode		\
   ? FIRST_FP_RET_REG					\
   : (TARGET_FPU_DOUBLE					\
      && ((MODE) == DFmode || (MODE) == SFmode		\
	  || (MODE) == DCmode || (MODE) == SCmode ))	\
   ? FIRST_FP_RET_REG					\
   : FIRST_RET_REG)

#define BASE_ARG_REG(MODE) \
  ((TARGET_SH3E && ((MODE) == SFmode))			\
   ? FIRST_FP_PARM_REG					\
   : TARGET_SH4 && (GET_MODE_CLASS (MODE) == MODE_FLOAT	\
		    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)\
   ? FIRST_FP_PARM_REG					\
   : FIRST_PARM_REG)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.
   For the SH, this is like LIBCALL_VALUE, except that we must change the
   mode like PROMOTE_MODE does.
   ??? PROMOTE_MODE is ignored for non-scalar types.  The set of types
   tested here has to be kept in sync with the one in explow.c:promote_mode.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  gen_rtx (REG,								\
	   ((GET_MODE_CLASS (TYPE_MODE (VALTYPE)) == MODE_INT		\
	     && GET_MODE_SIZE (TYPE_MODE (VALTYPE)) < UNITS_PER_WORD	\
	     && (TREE_CODE (VALTYPE) == INTEGER_TYPE			\
		 || TREE_CODE (VALTYPE) == ENUMERAL_TYPE		\
		 || TREE_CODE (VALTYPE) == BOOLEAN_TYPE			\
		 || TREE_CODE (VALTYPE) == CHAR_TYPE			\
		 || TREE_CODE (VALTYPE) == REAL_TYPE			\
		 || TREE_CODE (VALTYPE) == OFFSET_TYPE))		\
	    ? (TARGET_SHMEDIA ? DImode : SImode) : TYPE_MODE (VALTYPE)), \
	   BASE_RETURN_VALUE_REG (TYPE_MODE (VALTYPE)))
     
/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) \
  gen_rtx_REG ((MODE), BASE_RETURN_VALUE_REG (MODE));

/* 1 if N is a possible register number for a function value.  */
#define FUNCTION_VALUE_REGNO_P(REGNO) \
  ((REGNO) == FIRST_RET_REG || (TARGET_SH3E && (REGNO) == FIRST_FP_RET_REG) \
   || (TARGET_SHMEDIA_FPU && (REGNO) == FIRST_FP_RET_REG))

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  (((REGNO) >= FIRST_PARM_REG && (REGNO) < (FIRST_PARM_REG		\
					    + NPARM_REGS (SImode)))	\
   || (TARGET_FPU_ANY                                                   \
       && (REGNO) >= FIRST_FP_PARM_REG && (REGNO) < (FIRST_FP_PARM_REG	\
						     + NPARM_REGS (SFmode))))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SH, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus NARGREGS or more means all following args should go on the stack.  */

enum sh_arg_class { SH_ARG_INT = 0, SH_ARG_FLOAT = 1 };
struct sh_args {
    int arg_count[2];
    int force_mem;
  /* Non-zero if a prototype is available for the function.  */
    int prototype_p;
  /* The number of an odd floating-point register, that should be used
     for the next argument of type float.  */
    int free_single_fp_reg;
  /* Whether we're processing an outgoing function call.  */
    int outgoing;
  /* The number of general-purpose registers that should have been
     used to pass partial arguments, that are passed totally on the
     stack.  On SHcompact, a call trampoline will pop them off the
     stack before calling the actual function, and, if the called
     function is implemented in SHcompact mode, the incoming arguments
     decoder will push such arguments back onto the stack.  For
     incoming arguments, STACK_REGS also takes into account other
     arguments passed by reference, that the decoder will also push
     onto the stack.  */
    int stack_regs;
  /* The number of general-purpose registers that should have been
     used to pass arguments, if the arguments didn't have to be passed
     by reference.  */
    int byref_regs;
  /* Set by SHCOMPACT_BYREF if the current argument is to be passed by
     reference.  */
    int byref;

  /* call_cookie is a bitmask used by call expanders, as well as
     function prologue and epilogues, to allow SHcompact to comply
     with the SH5 32-bit ABI, that requires 64-bit registers to be
     used even though only the lower 32-bit half is visible in
     SHcompact mode.  The strategy is to call SHmedia trampolines.

     The alternatives for each of the argument-passing registers are
     (a) leave it unchanged; (b) pop it off the stack; (c) load its
     contents from the address in it; (d) add 8 to it, storing the
     result in the next register, then (c); (e) copy it from some
     floating-point register,

     Regarding copies from floating-point registers, r2 may only be
     copied from dr0.  r3 may be copied from dr0 or dr2.  r4 maybe
     copied from dr0, dr2 or dr4.  r5 maybe copied from dr0, dr2,
     dr4 or dr6.  r6 may be copied from dr0, dr2, dr4, dr6 or dr8.
     r7 through to r9 may be copied from dr0, dr2, dr4, dr8, dr8 or
     dr10.

     The bit mask is structured as follows:

     - 1 bit to tell whether to set up a return trampoline.

     - 3 bits to count the number consecutive registers to pop off the
       stack.

     - 4 bits for each of r9, r8, r7 and r6.

     - 3 bits for each of r5, r4, r3 and r2.

     - 3 bits set to 0 (the most significant ones)

        3           2            1           0
       1098 7654 3210 9876 5432 1098 7654 3210
       FLPF LPFL PFLP FFLP FFLP FFLP FFLP SSST
       2223 3344 4555 6666 7777 8888 9999 SSS-

     - If F is set, the register must be copied from an FP register,
       whose number is encoded in the remaining bits.

     - Else, if L is set, the register must be loaded from the address
       contained in it.  If the P bit is *not* set, the address of the
       following dword should be computed first, and stored in the
       following register.

     - Else, if P is set, the register alone should be popped off the
       stack.

     - After all this processing, the number of registers represented
       in SSS will be popped off the stack.  This is an optimization
       for pushing/popping consecutive registers, typically used for
       varargs and large arguments partially passed in registers.

     - If T is set, a return trampoline will be set up for 64-bit
     return values to be split into 2 32-bit registers.  */
#define CALL_COOKIE_RET_TRAMP_SHIFT 0
#define CALL_COOKIE_RET_TRAMP(VAL) ((VAL) << CALL_COOKIE_RET_TRAMP_SHIFT)
#define CALL_COOKIE_STACKSEQ_SHIFT 1
#define CALL_COOKIE_STACKSEQ(VAL) ((VAL) << CALL_COOKIE_STACKSEQ_SHIFT)
#define CALL_COOKIE_STACKSEQ_GET(COOKIE) \
  (((COOKIE) >> CALL_COOKIE_STACKSEQ_SHIFT) & 7)
#define CALL_COOKIE_INT_REG_SHIFT(REG) \
  (4 * (7 - (REG)) + (((REG) <= 2) ? ((REG) - 2) : 1) + 3)
#define CALL_COOKIE_INT_REG(REG, VAL) \
  ((VAL) << CALL_COOKIE_INT_REG_SHIFT (REG))
#define CALL_COOKIE_INT_REG_GET(COOKIE, REG) \
  (((COOKIE) >> CALL_COOKIE_INT_REG_SHIFT (REG)) & ((REG) < 4 ? 7 : 15))
    long call_cookie;
};

#define CUMULATIVE_ARGS  struct sh_args

#define GET_SH_ARG_CLASS(MODE) \
  ((TARGET_FPU_ANY && (MODE) == SFmode) \
   ? SH_ARG_FLOAT \
   /* There's no mention of complex float types in the SH5 ABI, so we
      should presumably handle them as aggregate types.  */ \
   : TARGET_SH5 && GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT \
   ? SH_ARG_INT \
   : TARGET_FPU_DOUBLE && (GET_MODE_CLASS (MODE) == MODE_FLOAT \
			   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
   ? SH_ARG_FLOAT : SH_ARG_INT)

#define ROUND_ADVANCE(SIZE) \
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round a register number up to a proper boundary for an arg of mode
   MODE.

   The SH doesn't care about double alignment, so we only
   round doubles to even regs when asked to explicitly.  */

#define ROUND_REG(CUM, MODE) \
   (((TARGET_ALIGN_DOUBLE					\
      || (TARGET_SH4 && ((MODE) == DFmode || (MODE) == DCmode)	\
	  && (CUM).arg_count[(int) SH_ARG_FLOAT] < NPARM_REGS (MODE)))\
     && GET_MODE_UNIT_SIZE ((MODE)) > UNITS_PER_WORD)		\
    ? ((CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)]		\
       + ((CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)] & 1))	\
    : (CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)])

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On SH, the offset always starts at 0: the first parm reg is always
   the same reg for a given argument class.

   For TARGET_HITACHI, the structure value pointer is passed in memory.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  do {								\
    (CUM).arg_count[(int) SH_ARG_INT] = 0;			\
    (CUM).arg_count[(int) SH_ARG_FLOAT] = 0;			\
    (CUM).force_mem						\
      = (TARGET_HITACHI && FNTYPE				\
	 && aggregate_value_p (TREE_TYPE (FNTYPE)));		\
    (CUM).prototype_p = (FNTYPE) && TYPE_ARG_TYPES (FNTYPE);	\
    (CUM).arg_count[(int) SH_ARG_INT]				\
      = (TARGET_SH5 && (FNTYPE)					\
	 && aggregate_value_p (TREE_TYPE (FNTYPE)));		\
    (CUM).free_single_fp_reg = 0;				\
    (CUM).outgoing = 1;						\
    (CUM).stack_regs = 0;					\
    (CUM).byref_regs = 0;					\
    (CUM).byref = 0;						\
    (CUM).call_cookie						\
      = (CALL_COOKIE_RET_TRAMP					\
	 (TARGET_SHCOMPACT && (FNTYPE)				\
	  && (CUM).arg_count[(int) SH_ARG_INT] == 0		\
	  && (TYPE_MODE (TREE_TYPE (FNTYPE)) == BLKmode		\
	      ? int_size_in_bytes (TREE_TYPE (FNTYPE))		\
	      : GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (FNTYPE)))) > 4 \
	  && (BASE_RETURN_VALUE_REG (TYPE_MODE (TREE_TYPE	\
 						(FNTYPE)))	\
	      == FIRST_RET_REG)));				\
  } while (0)

#define INIT_CUMULATIVE_LIBCALL_ARGS(CUM, MODE, LIBNAME) \
  do {								\
    INIT_CUMULATIVE_ARGS ((CUM), NULL_TREE, (LIBNAME), 0);	\
    (CUM).call_cookie						\
      = (CALL_COOKIE_RET_TRAMP					\
	 (TARGET_SHCOMPACT && GET_MODE_SIZE (MODE) > 4		\
	  && BASE_RETURN_VALUE_REG (MODE) == FIRST_RET_REG));	\
  } while (0)

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
  do {								\
    INIT_CUMULATIVE_ARGS ((CUM), (FNTYPE), (LIBNAME), 0);	\
    (CUM).outgoing = 0;						\
  } while (0)
 
/* FIXME: This is overly conservative.  A SHcompact function that
   receives arguments ``by reference'' will have them stored in its
   own stack frame, so it must not pass pointers or references to
   these arguments to other functions by means of sibling calls.  */
#define FUNCTION_OK_FOR_SIBCALL(DECL) \
  (! TARGET_SHCOMPACT || current_function_args_info.stack_regs == 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be
   available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 if ((CUM).force_mem)					\
   (CUM).force_mem = 0;					\
 else if (TARGET_SH5)					\
   {							\
     tree TYPE_ = ((CUM).byref && (TYPE)		\
		   ? TREE_TYPE (TYPE)			\
 		   : (TYPE));				\
     enum machine_mode MODE_ = ((CUM).byref && (TYPE)	\
				? TYPE_MODE (TYPE_)	\
				: (MODE));		\
     int dwords = (((CUM).byref				\
		    ? (CUM).byref			\
		    : (MODE_) == BLKmode		\
		    ? int_size_in_bytes (TYPE_)		\
		    : GET_MODE_SIZE (MODE_)) + 7) / 8;	\
     int numregs = MIN (dwords, NPARM_REGS (SImode)	\
			- (CUM).arg_count[(int) SH_ARG_INT]); \
							\
     if (numregs)					\
       {						\
	 (CUM).arg_count[(int) SH_ARG_INT] += numregs;	\
	 if (TARGET_SHCOMPACT				\
	     && SHCOMPACT_FORCE_ON_STACK (MODE_, TYPE_)) \
	   (CUM).call_cookie 				\
	     |= CALL_COOKIE_INT_REG (((CUM).arg_count[(int) SH_ARG_INT] \
				      - numregs), 1);	\
	 else if ((CUM).byref)				\
	   {						\
	     if (! (CUM).outgoing)			\
	       (CUM).stack_regs += numregs;		\
	     (CUM).byref_regs += numregs;		\
	     (CUM).byref = 0;				\
	     do						\
	       (CUM).call_cookie			\
		 |= CALL_COOKIE_INT_REG (((CUM).arg_count[(int) SH_ARG_INT] \
					  - numregs), 2); \
	     while (--numregs);				\
	     (CUM).call_cookie				\
	       |= CALL_COOKIE_INT_REG (((CUM).arg_count[(int) SH_ARG_INT] \
				        - 1), 1); \
	   }						\
	 else if (dwords > numregs)			\
	   {						\
	     int pushregs = numregs;			\
							\
	     if (TARGET_SHCOMPACT)			\
	       (CUM).stack_regs += numregs;		\
	     while (pushregs < NPARM_REGS (SImode) - 1	\
		    && (CALL_COOKIE_INT_REG_GET		\
			((CUM).call_cookie,		\
			NPARM_REGS (SImode) - pushregs) \
			== 1))				\
	       {					\
		 (CUM).call_cookie			\
		   &= ~ CALL_COOKIE_INT_REG (NPARM_REGS (SImode) \
					     - pushregs, 1); \
		 pushregs++;				\
	       }					\
	     if (numregs == NPARM_REGS (SImode))	\
	       (CUM).call_cookie 			\
		 |= CALL_COOKIE_INT_REG (0, 1)		\
		    | CALL_COOKIE_STACKSEQ (numregs - 1); \
	     else					\
	       (CUM).call_cookie			\
		 |= CALL_COOKIE_STACKSEQ (numregs);	\
	   }						\
       }						\
     if (GET_SH_ARG_CLASS (MODE_) == SH_ARG_FLOAT	\
	 && ((NAMED) || ! (CUM).prototype_p))		\
       {						\
	 if ((MODE_) == SFmode && (CUM).free_single_fp_reg) \
	   (CUM).free_single_fp_reg = 0;		\
	 else if ((CUM).arg_count[(int) SH_ARG_FLOAT]	\
 		  < NPARM_REGS (SFmode))		\
	   {					        \
	     int numfpregs		 		\
	       = MIN ((GET_MODE_SIZE (MODE_) + 7) / 8 * 2, \
		      NPARM_REGS (SFmode)		\
		      - (CUM).arg_count[(int) SH_ARG_FLOAT]); \
		 					\
	     (CUM).arg_count[(int) SH_ARG_FLOAT] += numfpregs; \
							\
	     if (TARGET_SHCOMPACT && ! (CUM).prototype_p) \
	       {					\
		 if ((CUM).outgoing && numregs > 0)	\
		   do					\
		     {					\
		       (CUM).call_cookie		\
			 |= (CALL_COOKIE_INT_REG	\
			     ((CUM).arg_count[(int) SH_ARG_INT] \
			      - numregs + ((numfpregs - 2) / 2), \
			      4 + ((CUM).arg_count[(int) SH_ARG_FLOAT] \
				   - numfpregs) / 2));	\
		     }					\
		   while (numfpregs -= 2);		\
	       }					\
	     else if ((MODE_) == SFmode && (NAMED)	\
		      && ((CUM).arg_count[(int) SH_ARG_FLOAT] \
			  < NPARM_REGS (SFmode)))	\
	       (CUM).free_single_fp_reg			\
		 = FIRST_FP_PARM_REG - numfpregs	\
		 + (CUM).arg_count[(int) SH_ARG_FLOAT] + 1; \
	   }						\
       }						\
   }							\
 else if (! TARGET_SH4 || PASS_IN_REG_P ((CUM), (MODE), (TYPE))) \
   ((CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)]	\
    = (ROUND_REG ((CUM), (MODE))			\
       + ((MODE) == BLKmode				\
	  ? ROUND_ADVANCE (int_size_in_bytes (TYPE))	\
	  : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))))

/* Return boolean indicating arg of mode MODE will be passed in a reg.
   This macro is only used in this file.  */

#define PASS_IN_REG_P(CUM, MODE, TYPE) \
  (((TYPE) == 0 \
    || (! TREE_ADDRESSABLE ((tree)(TYPE)) \
	&& (! TARGET_HITACHI || ! AGGREGATE_TYPE_P (TYPE)))) \
   && ! (CUM).force_mem \
   && (TARGET_SH3E \
       ? ((MODE) == BLKmode \
	  ? (((CUM).arg_count[(int) SH_ARG_INT] * UNITS_PER_WORD \
	      + int_size_in_bytes (TYPE)) \
	     <= NPARM_REGS (SImode) * UNITS_PER_WORD) \
	  : ((ROUND_REG((CUM), (MODE)) \
	      + HARD_REGNO_NREGS (BASE_ARG_REG (MODE), (MODE))) \
	     <= NPARM_REGS (MODE))) \
       : ROUND_REG ((CUM), (MODE)) < NPARM_REGS (MODE)))

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On SH the first args are normally in registers
   and the rest are pushed.  Any arg that starts within the first
   NPARM_REGS words is at least partially passed in a register unless
   its data type forbids.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  ((! TARGET_SH5 \
    && PASS_IN_REG_P ((CUM), (MODE), (TYPE))				\
    && ((NAMED)								\
	|| (! TARGET_HITACHI && (TARGET_SH3E || ! current_function_varargs)))) \
   ? gen_rtx_REG ((MODE),						\
		  ((BASE_ARG_REG (MODE) + ROUND_REG ((CUM), (MODE))) 	\
		   ^ ((MODE) == SFmode && TARGET_SH4			\
		      && TARGET_LITTLE_ENDIAN != 0)))			\
   : TARGET_SH5								\
   ? ((MODE) == VOIDmode && TARGET_SHCOMPACT				\
      ? GEN_INT ((CUM).call_cookie)					\
      /* The following test assumes unnamed arguments are promoted to	\
	 DFmode.  */							\
      : (MODE) == SFmode && (CUM).free_single_fp_reg			\
      ? SH5_PROTOTYPED_FLOAT_ARG ((CUM), (MODE), (CUM).free_single_fp_reg) \
      : (GET_SH_ARG_CLASS (MODE) == SH_ARG_FLOAT			\
         && ((NAMED) || ! (CUM).prototype_p)				\
         && (CUM).arg_count[(int) SH_ARG_FLOAT] < NPARM_REGS (SFmode))	\
      ? ((! (CUM).prototype_p && TARGET_SHMEDIA)			\
	 ? SH5_PROTOTYPELESS_FLOAT_ARG ((CUM), (MODE))			\
	 : SH5_PROTOTYPED_FLOAT_ARG ((CUM), (MODE),			\
				     FIRST_FP_PARM_REG			\
				     + (CUM).arg_count[(int) SH_ARG_FLOAT])) \
      : ((CUM).arg_count[(int) SH_ARG_INT] < NPARM_REGS (SImode)	\
	 && (! TARGET_SHCOMPACT						\
	     || (! SHCOMPACT_FORCE_ON_STACK ((MODE), (TYPE))		\
	         && ! SH5_WOULD_BE_PARTIAL_NREGS ((CUM), (MODE),	\
						  (TYPE), (NAMED)))))	\
      ? gen_rtx_REG ((MODE), (FIRST_PARM_REG				\
 			      + (CUM).arg_count[(int) SH_ARG_INT]))	\
      : 0)								\
   : 0)

/* Whether an argument must be passed by reference.  On SHcompact, we
   pretend arguments wider than 32-bits that would have been passed in
   registers are passed by reference, so that an SHmedia trampoline
   loads them into the full 64-bits registers.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM,MODE,TYPE,NAMED) \
  (MUST_PASS_IN_STACK ((MODE), (TYPE)) \
   || SHCOMPACT_BYREF ((CUM), (MODE), (TYPE), (NAMED)))

#define SHCOMPACT_BYREF(CUM, MODE, TYPE, NAMED) \
  ((CUM).byref								\
   = (TARGET_SHCOMPACT							\
      && (CUM).arg_count[(int) SH_ARG_INT] < NPARM_REGS (SImode)	\
      && (! (NAMED) || GET_SH_ARG_CLASS (MODE) == SH_ARG_INT		\
	  || (GET_SH_ARG_CLASS (MODE) == SH_ARG_FLOAT			\
	      && ((CUM).arg_count[(int) SH_ARG_FLOAT]			\
 		  >= NPARM_REGS (SFmode))))				\
      && ((MODE) == BLKmode ? int_size_in_bytes (TYPE)			\
	  : GET_MODE_SIZE (MODE)) > 4				       	\
      && ! SHCOMPACT_FORCE_ON_STACK ((MODE), (TYPE))			\
      && ! SH5_WOULD_BE_PARTIAL_NREGS ((CUM), (MODE),			\
				       (TYPE), (NAMED)))		\
      ? ((MODE) == BLKmode ? int_size_in_bytes (TYPE)			\
	 : GET_MODE_SIZE (MODE))					\
      : 0)

/* If an argument of size 5, 6 or 7 bytes is to be passed in a 64-bit
   register in SHcompact mode, it must be padded in the most
   significant end.  This means that passing it by reference wouldn't
   pad properly on a big-endian machine.  In this particular case, we
   pass this argument on the stack, in a way that the call trampoline
   will load its value into the appropriate register.  */
#define SHCOMPACT_FORCE_ON_STACK(MODE,TYPE) \
  ((MODE) == BLKmode \
   && TARGET_SHCOMPACT \
   && ! TARGET_LITTLE_ENDIAN \
   && int_size_in_bytes (TYPE) > 4 \
   && int_size_in_bytes (TYPE) < 8)

/* Minimum alignment for an argument to be passed by callee-copy
   reference.  We need such arguments to be aligned to 8 byte
   boundaries, because they'll be loaded using quad loads.  */
#define SH_MIN_ALIGN_FOR_CALLEE_COPY (8 * BITS_PER_UNIT)

#define FUNCTION_ARG_CALLEE_COPIES(CUM,MODE,TYPE,NAMED) \
  ((CUM).outgoing							\
   && (((MODE) == BLKmode ? TYPE_ALIGN (TYPE)				\
	: GET_MODE_ALIGNMENT (MODE))					\
       % SH_MIN_ALIGN_FOR_CALLEE_COPY == 0))

/* The SH5 ABI requires floating-point arguments to be passed to
   functions without a prototype in both an FP register and a regular
   register or the stack.  When passing the argument in both FP and
   general-purpose registers, list the FP register first.  */
#define SH5_PROTOTYPELESS_FLOAT_ARG(CUM,MODE) \
  (gen_rtx_PARALLEL							\
   ((MODE),								\
    gen_rtvec (2,							\
	       gen_rtx_EXPR_LIST					\
	       (VOIDmode,						\
		((CUM).arg_count[(int) SH_ARG_INT] < NPARM_REGS (SImode) \
		 ? gen_rtx_REG ((MODE), FIRST_FP_PARM_REG		\
				+ (CUM).arg_count[(int) SH_ARG_FLOAT])	\
		 : NULL_RTX),						\
		const0_rtx),						\
	       gen_rtx_EXPR_LIST					\
	       (VOIDmode,						\
		((CUM).arg_count[(int) SH_ARG_INT] < NPARM_REGS (SImode) \
		 ? gen_rtx_REG ((MODE), FIRST_PARM_REG			\
				+ (CUM).arg_count[(int) SH_ARG_INT])	\
		 : gen_rtx_REG ((MODE), FIRST_FP_PARM_REG		\
				+ (CUM).arg_count[(int) SH_ARG_FLOAT])), \
		const0_rtx))))

/* The SH5 ABI requires regular registers or stack slots to be
   reserved for floating-point arguments.  Registers are taken care of
   in FUNCTION_ARG_ADVANCE, but stack slots must be reserved here.
   Unfortunately, there's no way to just reserve a stack slot, so
   we'll end up needlessly storing a copy of the argument in the
   stack.  For incoming arguments, however, the PARALLEL will be
   optimized to the register-only form, and the value in the stack
   slot won't be used at all.  */
#define SH5_PROTOTYPED_FLOAT_ARG(CUM,MODE,REG) \
  ((CUM).arg_count[(int) SH_ARG_INT] < NPARM_REGS (SImode)		\
   ? gen_rtx_REG ((MODE), (REG))					\
   : gen_rtx_PARALLEL ((MODE),						\
		       gen_rtvec (2,					\
				  gen_rtx_EXPR_LIST			\
				  (VOIDmode, NULL_RTX,			\
				   const0_rtx),				\
				  gen_rtx_EXPR_LIST			\
				  (VOIDmode, gen_rtx_REG ((MODE),	\
							  (REG)),	\
				   const0_rtx))))

#define STRICT_ARGUMENT_NAMING TARGET_SH5

#define PRETEND_OUTGOING_VARARGS_NAMED (! TARGET_HITACHI && ! TARGET_SH5)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.

   We sometimes split args.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  ((! TARGET_SH5 \
    && PASS_IN_REG_P ((CUM), (MODE), (TYPE))			\
    && ! TARGET_SH4						\
    && (ROUND_REG ((CUM), (MODE))				\
	+ ((MODE) != BLKmode					\
	   ? ROUND_ADVANCE (GET_MODE_SIZE (MODE))		\
	   : ROUND_ADVANCE (int_size_in_bytes (TYPE)))		\
	> NPARM_REGS (MODE)))					\
   ? NPARM_REGS (MODE) - ROUND_REG ((CUM), (MODE))		\
   : (SH5_WOULD_BE_PARTIAL_NREGS ((CUM), (MODE), (TYPE), (NAMED)) \
      && ! TARGET_SHCOMPACT)					\
   ? NPARM_REGS (SImode) - (CUM).arg_count[(int) SH_ARG_INT]	\
   : 0)

#define SH5_WOULD_BE_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  (TARGET_SH5 && (MODE) == BLKmode				\
   && ((CUM).arg_count[(int) SH_ARG_INT]			\
       + (int_size_in_bytes (TYPE) + 7) / 8) > NPARM_REGS (SImode))

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.  */

/* We actually emit the code in sh_expand_prologue.  We used to use
   a static variable to flag that we need to emit this code, but that
   doesn't when inlining, when functions are deferred and then emitted
   later.  Fortunately, we already have two flags that are part of struct
   function that tell if a function uses varargs or stdarg.  */
#define SETUP_INCOMING_VARARGS(ASF, MODE, TYPE, PAS, ST)  do \
  if (! current_function_varargs && ! current_function_stdarg) \
    abort (); \
while (0)

/* Define the `__builtin_va_list' type for the ABI.  */
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = sh_build_va_list ()

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  sh_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  sh_va_arg (valist, type)

/* Call the function profiler with a given profile label.
   We use two .aligns, so as to make sure that both the .long is aligned
   on a 4 byte boundary, and that the .long is a fixed distance (2 bytes)
   from the trapa instruction.  */

#define FUNCTION_PROFILER(STREAM,LABELNO)			\
{								\
	fprintf((STREAM), "\t.align\t2\n");			\
	fprintf((STREAM), "\ttrapa\t#33\n");			\
 	fprintf((STREAM), "\t.align\t2\n");			\
	asm_fprintf((STREAM), "\t.long\t%LLP%d\n", (LABELNO));	\
}

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */

#define PROFILE_BEFORE_PROLOGUE

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* 
   On the SH, the trampoline looks like
   2 0002 D202     	   	mov.l	l2,r2
   1 0000 D301     		mov.l	l1,r3
   3 0004 422B     		jmp	@r2
   4 0006 0009     		nop
   5 0008 00000000 	l1:  	.long   area
   6 000c 00000000 	l2:	.long   function  */

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  (TARGET_SHMEDIA64 ? 40 : TARGET_SH5 ? 32 : 16)

/* Alignment required for a trampoline in bits .  */
#define TRAMPOLINE_ALIGNMENT \
  ((CACHE_LOG < 3 || (TARGET_SMALLCODE && ! TARGET_HARVARD)) ? 32 : 64)

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) do			\
{									\
  if (TARGET_SH5)							\
    {									\
      rtx tramp_templ = gen_rtx_SYMBOL_REF (Pmode,			\
					    "__GCC_nested_trampoline");	\
      int fixed_len = TRAMPOLINE_SIZE - 2 * GET_MODE_SIZE (Pmode);	\
									\
      tramp_templ = gen_datalabel_ref (tramp_templ);			\
      emit_block_move (gen_rtx_MEM (BLKmode, (TRAMP)),			\
		       gen_rtx_MEM (BLKmode, tramp_templ),		\
		       GEN_INT (fixed_len));				\
      emit_move_insn (gen_rtx_MEM (Pmode, plus_constant ((TRAMP),	\
							 fixed_len)),	\
		      (FNADDR));					\
      emit_move_insn (gen_rtx_MEM (Pmode,				\
				   plus_constant ((TRAMP),		\
						  fixed_len		\
						  + GET_MODE_SIZE (Pmode))), \
		      (CXT));						\
      emit_insn (gen_ic_invalidate_line (TRAMP));			\
      break;								\
    }									\
  emit_move_insn (gen_rtx_MEM (SImode, (TRAMP)),			\
                  GEN_INT (trunc_int_for_mode                  		\
                         (TARGET_LITTLE_ENDIAN ? 0xd301d202 : 0xd202d301,\
                          SImode))); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant ((TRAMP), 4)),	\
		  GEN_INT (TARGET_LITTLE_ENDIAN ? 0x0009422b : 0x422b0009));\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant ((TRAMP), 8)),	\
		  (CXT));						\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant ((TRAMP), 12)),	\
		  (FNADDR));						\
  if (TARGET_HARVARD)							\
    {									\
      if (TARGET_USERMODE)						\
	emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__ic_invalidate"),\
			   0, VOIDmode, 1, (TRAMP), SImode);		\
      else								\
	emit_insn (gen_ic_invalidate_line (TRAMP));			\
    }									\
} while (0)

/* On SH5, trampolines are SHmedia code, so add 1 to the address.  */

#define TRAMPOLINE_ADJUST_ADDRESS(TRAMP) do				\
{									\
  if (TARGET_SH5)							\
    (TRAMP) = expand_simple_binop (Pmode, PLUS, (TRAMP), GEN_INT (1),	\
				   gen_reg_rtx (Pmode), 0,		\
				   OPTAB_LIB_WIDEN);			\
} while (0)

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is already the frame pointer of the COUNT frame, so we
   can ignore COUNT.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)	\
  (((COUNT) == 0)				\
   ? get_hard_reg_initial_val (Pmode, PR_REG) \
   : (rtx) 0)

/* Generate necessary RTL for __builtin_saveregs().  */
#define EXPAND_BUILTIN_SAVEREGS() sh_builtin_saveregs ()

/* Addressing modes, and classification of registers for them.  */
#define HAVE_POST_INCREMENT  TARGET_SH1
/*#define HAVE_PRE_INCREMENT   1*/
/*#define HAVE_POST_DECREMENT  1*/
#define HAVE_PRE_DECREMENT   TARGET_SH1

#define USE_LOAD_POST_INCREMENT(mode)    ((mode == SImode || mode == DImode) \
                                           ? 0 : TARGET_SH1)
#define USE_LOAD_PRE_DECREMENT(mode)     0
#define USE_STORE_POST_INCREMENT(mode)   0
#define USE_STORE_PRE_DECREMENT(mode)    ((mode == SImode || mode == DImode) \
                                           ? 0 : TARGET_SH1)

#define MOVE_BY_PIECES_P(SIZE, ALIGN)  (move_by_pieces_ninsns (SIZE, ALIGN) \
                                        < (TARGET_SMALLCODE ? 2 :           \
                                           ((ALIGN >= 32) ? 16 : 2)))

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_BASE_P(REGNO) \
  (GENERAL_OR_AP_REGISTER_P (REGNO) \
   || GENERAL_OR_AP_REGISTER_P (reg_renumber[(REGNO)]))
#define REGNO_OK_FOR_INDEX_P(REGNO) \
  (TARGET_SHMEDIA \
   ? (GENERAL_REGISTER_P (REGNO) \
      || GENERAL_REGISTER_P ((unsigned) reg_renumber[(REGNO)])) \
   : (REGNO) == R0_REG || (unsigned) reg_renumber[(REGNO)] == R0_REG)

/* Maximum number of registers that can appear in a valid memory
   address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)	(GET_CODE (X) == LABEL_REF)

/* Nonzero if the constant value X is a legitimate general operand.  */

#define LEGITIMATE_CONSTANT_P(X) \
  (TARGET_SHMEDIA							\
   ? (GET_MODE (X) != DFmode						\
      || (X) == CONST0_RTX (GET_MODE (X))				\
      || ! TARGET_SHMEDIA_FPU						\
      || TARGET_SHMEDIA64)						\
   : (GET_CODE (X) != CONST_DOUBLE					\
      || GET_MODE (X) == DFmode || GET_MODE (X) == SFmode		\
      || (TARGET_SH3E && (fp_zero_operand (X) || fp_one_operand (X)))))

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (GENERAL_OR_AP_REGISTER_P (REGNO (X)) || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  ((TARGET_SHMEDIA ? GENERAL_REGISTER_P (REGNO (X)) \
    : REGNO (X) == R0_REG) || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X/OFFSET is a hard reg that can be used as an index
   or if X is a pseudo reg.  */
#define SUBREG_OK_FOR_INDEX_P(X, OFFSET) \
  ((TARGET_SHMEDIA ? GENERAL_REGISTER_P (REGNO (X)) \
    : REGNO (X) == R0_REG && OFFSET == 0) || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#else

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) \
  REGNO_OK_FOR_BASE_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) \
  REGNO_OK_FOR_INDEX_P (REGNO (X))

/* Nonzero if X/OFFSET is a hard reg that can be used as an index.  */
#define SUBREG_OK_FOR_INDEX_P(X, OFFSET) \
  (REGNO_OK_FOR_INDEX_P (REGNO (X)) && (OFFSET) == 0)

#endif

/* The 'Q' constraint is a pc relative load operand.  */
#define EXTRA_CONSTRAINT_Q(OP)                          		\
  (GET_CODE (OP) == MEM 						\
   && ((GET_CODE (XEXP ((OP), 0)) == LABEL_REF)				\
       || (GET_CODE (XEXP ((OP), 0)) == CONST                		\
	   && GET_CODE (XEXP (XEXP ((OP), 0), 0)) == PLUS		\
	   && GET_CODE (XEXP (XEXP (XEXP ((OP), 0), 0), 0)) == LABEL_REF \
	   && GET_CODE (XEXP (XEXP (XEXP ((OP), 0), 0), 1)) == CONST_INT)))

/* The `S' constraint is a 16-bit constant, literal or symbolic.  */
#define EXTRA_CONSTRAINT_S(OP) \
  (GET_CODE (OP) == CONST \
   && GET_CODE (XEXP ((OP), 0)) == SIGN_EXTEND \
   && GET_MODE (XEXP ((OP), 0)) == DImode \
   && GET_CODE (XEXP (XEXP ((OP), 0), 0)) == TRUNCATE \
   && GET_MODE (XEXP (XEXP ((OP), 0), 0)) == HImode \
   && (MOVI_SHORI_BASE_OPERAND_P (XEXP (XEXP (XEXP ((OP), 0), 0), 0)) \
       || (GET_CODE (XEXP (XEXP (XEXP ((OP), 0), 0), 0)) == ASHIFTRT \
	   && (MOVI_SHORI_BASE_OPERAND_P \
	       (XEXP (XEXP (XEXP (XEXP ((OP), 0), 0), 0), 0))) \
	   && GET_CODE (XEXP (XEXP (XEXP (XEXP ((OP), 0), 0), 0), \
			      1)) == CONST_INT)))

/* Check whether OP is a datalabel unspec.  */
#define DATALABEL_REF_NO_CONST_P(OP) \
  (GET_CODE (OP) == UNSPEC \
   && XINT ((OP), 1) == UNSPEC_DATALABEL \
   && XVECLEN ((OP), 0) == 1 \
   && (GET_CODE (XVECEXP ((OP), 0, 0)) == SYMBOL_REF \
       || GET_CODE (XVECEXP ((OP), 0, 0)) == LABEL_REF))

/* Check whether OP is a datalabel unspec, possibly enclosed within a
   CONST.  */
#define DATALABEL_REF_P(OP) \
  ((GET_CODE (OP) == CONST && DATALABEL_REF_NO_CONST_P (XEXP ((OP), 0))) \
   || DATALABEL_REF_NO_CONST_P (OP))

#define GOT_ENTRY_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_GOT)

#define GOTPLT_ENTRY_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_GOTPLT)

#define GOTOFF_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_GOTOFF)

#define PIC_ADDR_P(OP) \
  (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == UNSPEC \
   && XINT (XEXP ((OP), 0), 1) == UNSPEC_PIC)

#define NON_PIC_REFERENCE_P(OP) \
  (GET_CODE (OP) == LABEL_REF || GET_CODE (OP) == SYMBOL_REF \
   || DATALABEL_REF_P (OP) \
   || (GET_CODE (OP) == CONST && GET_CODE (XEXP ((OP), 0)) == PLUS \
       && (GET_CODE (XEXP (XEXP ((OP), 0), 0)) == SYMBOL_REF \
	   || DATALABEL_REF_P (XEXP (XEXP ((OP), 0), 0))) \
       && GET_CODE (XEXP (XEXP ((OP), 0), 1)) == CONST_INT))

#define PIC_REFERENCE_P(OP) \
  (GOT_ENTRY_P (OP) || GOTPLT_ENTRY_P (OP) \
   || GOTOFF_P (OP) || PIC_ADDR_P (OP))

#define MOVI_SHORI_BASE_OPERAND_P(OP) \
  (flag_pic ? PIC_REFERENCE_P (OP) : NON_PIC_REFERENCE_P (OP))

/* The `T' constraint is a label or a symbol.  */
#define EXTRA_CONSTRAINT_T(OP) \
  (NON_PIC_REFERENCE_P (OP))

#define EXTRA_CONSTRAINT(OP, C)		\
  ((C) == 'Q' ? EXTRA_CONSTRAINT_Q (OP)	\
   : (C) == 'S' ? EXTRA_CONSTRAINT_S (OP) \
   : (C) == 'T' ? EXTRA_CONSTRAINT_T (OP) \
   : 0)

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  */

#define MODE_DISP_OK_4(X,MODE) \
(GET_MODE_SIZE (MODE) == 4 && (unsigned) INTVAL (X) < 64	\
 && ! (INTVAL (X) & 3) && ! (TARGET_SH3E && (MODE) == SFmode))

#define MODE_DISP_OK_8(X,MODE) \
((GET_MODE_SIZE(MODE)==8) && ((unsigned)INTVAL(X)<60)	\
 && ! (INTVAL(X) & 3) && ! (TARGET_SH4 && (MODE) == DFmode))

#define BASE_REGISTER_RTX_P(X)				\
  ((GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))	\
   || (GET_CODE (X) == SUBREG				\
       && GET_CODE (SUBREG_REG (X)) == REG		\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

/* Since this must be r0, which is a single register class, we must check
   SUBREGs more carefully, to be sure that we don't accept one that extends
   outside the class.  */
#define INDEX_REGISTER_RTX_P(X)				\
  ((GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))	\
   || (GET_CODE (X) == SUBREG				\
       && GET_CODE (SUBREG_REG (X)) == REG		\
       && SUBREG_OK_FOR_INDEX_P (SUBREG_REG (X), SUBREG_BYTE (X))))

/* Jump to LABEL if X is a valid address RTX.  This must also take
   REG_OK_STRICT into account when deciding about valid registers, but it uses
   the above macros so we are in luck.

   Allow  REG
	  REG+disp
	  REG+r0
	  REG++
	  --REG  */

/* ??? The SH3e does not have the REG+disp addressing mode when loading values
   into the FRx registers.  We implement this by setting the maximum offset
   to zero when the value is SFmode.  This also restricts loading of SFmode
   values into the integer registers, but that can't be helped.  */

/* The SH allows a displacement in a QI or HI amode, but only when the
   other operand is R0. GCC doesn't handle this very well, so we forgo
   all of that.

   A legitimate index for a QI or HI is 0, SI can be any number 0..63,
   DI can be any number 0..60.  */

#define GO_IF_LEGITIMATE_INDEX(MODE, OP, LABEL)  			\
  do {									\
    if (GET_CODE (OP) == CONST_INT) 					\
      {									\
	if (TARGET_SHMEDIA)						\
	  {								\
	    int MODE_SIZE = GET_MODE_SIZE (MODE);			\
	    if (! (INTVAL (OP) & (MODE_SIZE - 1))			\
		&& INTVAL (OP) >= -512 * MODE_SIZE			\
		&& INTVAL (OP) < 512 * MODE_SIZE)			\
	      goto LABEL;						\
	    else							\
	      break;							\
	  }								\
	if (MODE_DISP_OK_4 ((OP), (MODE)))  goto LABEL;		      	\
	if (MODE_DISP_OK_8 ((OP), (MODE)))  goto LABEL;		      	\
      }									\
  } while(0)

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
{									\
  if (BASE_REGISTER_RTX_P (X))						\
    goto LABEL;								\
  else if ((GET_CODE (X) == POST_INC || GET_CODE (X) == PRE_DEC)	\
	   && ! TARGET_SHMEDIA						\
	   && BASE_REGISTER_RTX_P (XEXP ((X), 0)))			\
    goto LABEL;								\
  else if (GET_CODE (X) == PLUS						\
	   && ((MODE) != PSImode || reload_completed))			\
    {									\
      rtx xop0 = XEXP ((X), 0);						\
      rtx xop1 = XEXP ((X), 1);						\
      if (GET_MODE_SIZE (MODE) <= 8 && BASE_REGISTER_RTX_P (xop0))	\
	GO_IF_LEGITIMATE_INDEX ((MODE), xop1, LABEL);			\
      if (GET_MODE_SIZE (MODE) <= 4					\
	  || (TARGET_SHMEDIA && GET_MODE_SIZE (MODE) <= 8)		\
	  || (TARGET_SH4 && TARGET_FMOVD && MODE == DFmode))		\
	{								\
	  if (BASE_REGISTER_RTX_P (xop1) && INDEX_REGISTER_RTX_P (xop0))\
	    goto LABEL;							\
	  if (INDEX_REGISTER_RTX_P (xop1) && BASE_REGISTER_RTX_P (xop0))\
	    goto LABEL;							\
	}								\
    }									\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the SH, if X is almost suitable for indexing, but the offset is
   out of range, convert it into a normal form so that cse has a chance
   of reducing the number of address registers used.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{								\
  if (flag_pic)							\
    (X) = legitimize_pic_address (OLDX, MODE, NULL_RTX);	\
  if (GET_CODE (X) == PLUS					\
      && (GET_MODE_SIZE (MODE) == 4				\
	  || GET_MODE_SIZE (MODE) == 8)				\
      && GET_CODE (XEXP ((X), 1)) == CONST_INT			\
      && BASE_REGISTER_RTX_P (XEXP ((X), 0))			\
      && ! TARGET_SHMEDIA					\
      && ! (TARGET_SH4 && (MODE) == DFmode)			\
      && ! (TARGET_SH3E && (MODE) == SFmode))			\
    {								\
      rtx index_rtx = XEXP ((X), 1);				\
      HOST_WIDE_INT offset = INTVAL (index_rtx), offset_base;	\
      rtx sum;							\
								\
      GO_IF_LEGITIMATE_INDEX ((MODE), index_rtx, WIN);		\
      /* On rare occasions, we might get an unaligned pointer	\
	 that is indexed in a way to give an aligned address.	\
	 Therefore, keep the lower two bits in offset_base.  */ \
      /* Instead of offset_base 128..131 use 124..127, so that	\
	 simple add suffices.  */				\
      if (offset > 127)						\
	{							\
	  offset_base = ((offset + 4) & ~60) - 4;		\
	}							\
      else							\
	offset_base = offset & ~60;				\
      /* Sometimes the normal form does not suit DImode.  We	\
	 could avoid that by using smaller ranges, but that	\
	 would give less optimized code when SImode is		\
	 prevalent.  */						\
      if (GET_MODE_SIZE (MODE) + offset - offset_base <= 64)	\
	{							\
	  sum = expand_binop (Pmode, add_optab, XEXP ((X), 0),	\
			      GEN_INT (offset_base), NULL_RTX, 0, \
			      OPTAB_LIB_WIDEN);			\
                                                                \
	  (X) = gen_rtx_PLUS (Pmode, sum, GEN_INT (offset - offset_base)); \
	  goto WIN;						\
	}							\
    }								\
}

/* A C compound statement that attempts to replace X, which is an address
   that needs reloading, with a valid memory address for an operand of
   mode MODE.  WIN is a C statement label elsewhere in the code.

   Like for LEGITIMIZE_ADDRESS, for the SH we try to get a normal form
   of the address.  That will allow inheritance of the address reloads.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	\
{									\
  if (GET_CODE (X) == PLUS						\
      && (GET_MODE_SIZE (MODE) == 4 || GET_MODE_SIZE (MODE) == 8)	\
      && GET_CODE (XEXP (X, 1)) == CONST_INT				\
      && BASE_REGISTER_RTX_P (XEXP (X, 0))				\
      && ! TARGET_SHMEDIA						\
      && ! (TARGET_SH4 && (MODE) == DFmode)				\
      && ! ((MODE) == PSImode && (TYPE) == RELOAD_FOR_INPUT_ADDRESS))	\
    {									\
      rtx index_rtx = XEXP (X, 1);					\
      HOST_WIDE_INT offset = INTVAL (index_rtx), offset_base;		\
      rtx sum;								\
									\
      if (TARGET_SH3E && MODE == SFmode)				\
	{								\
	  X = copy_rtx (X);						\
	  push_reload (index_rtx, NULL_RTX, &XEXP (X, 1), NULL,		\
		       INDEX_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM),	\
		       (TYPE));						\
	  goto WIN;							\
	}								\
      /* Instead of offset_base 128..131 use 124..127, so that		\
	 simple add suffices.  */					\
      if (offset > 127)							\
	{								\
	  offset_base = ((offset + 4) & ~60) - 4;			\
	}								\
      else								\
	offset_base = offset & ~60;					\
      /* Sometimes the normal form does not suit DImode.  We		\
	 could avoid that by using smaller ranges, but that		\
	 would give less optimized code when SImode is			\
	 prevalent.  */							\
      if (GET_MODE_SIZE (MODE) + offset - offset_base <= 64)		\
	{								\
	  sum = gen_rtx (PLUS, Pmode, XEXP (X, 0),			\
			 GEN_INT (offset_base));			\
	  X = gen_rtx (PLUS, Pmode, sum, GEN_INT (offset - offset_base));\
	  push_reload (sum, NULL_RTX, &XEXP (X, 0), NULL,		\
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM),	\
		       (TYPE));						\
	  goto WIN;							\
	}								\
    }									\
  /* We must re-recognize what we created before.  */			\
  else if (GET_CODE (X) == PLUS						\
	   && (GET_MODE_SIZE (MODE) == 4 || GET_MODE_SIZE (MODE) == 8)	\
	   && GET_CODE (XEXP (X, 0)) == PLUS				\
	   && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT		\
	   && BASE_REGISTER_RTX_P (XEXP (XEXP (X, 0), 0))		\
	   && GET_CODE (XEXP (X, 1)) == CONST_INT			\
	   && ! TARGET_SHMEDIA						\
	   && ! (TARGET_SH3E && MODE == SFmode))			\
    {									\
      /* Because this address is so complex, we know it must have	\
	 been created by LEGITIMIZE_RELOAD_ADDRESS before; thus,	\
	 it is already unshared, and needs no further unsharing.  */	\
      push_reload (XEXP ((X), 0), NULL_RTX, &XEXP ((X), 0), NULL,	\
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM), (TYPE));\
      goto WIN;								\
    }									\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   ??? Strictly speaking, we should also include all indexed addressing,
   because the index scale factor is the length of the operand.
   However, the impact of GO_IF_MODE_DEPENDENT_ADDRESS would be to
   high if we did that.  So we rely on reload to fix things up.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)			\
{									\
  if (GET_CODE(ADDR) == PRE_DEC || GET_CODE(ADDR) == POST_INC)		\
    goto LABEL;								\
}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_BIGTABLE ? SImode : HImode)

#define CASE_VECTOR_SHORTEN_MODE(MIN_OFFSET, MAX_OFFSET, BODY) \
((MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 127 \
 ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, QImode) \
 : (MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 255 \
 ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 1, QImode) \
 : (MIN_OFFSET) >= -32768 && (MAX_OFFSET) <= 32767 ? HImode \
 : SImode)

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE 1

/* Define it here, so that it doesn't get bumped to 64-bits on SHmedia.  */
#define FLOAT_TYPE_SIZE 32

/* Since the SH3e has only `float' support, it is desirable to make all
   floating point types equivalent to `float'.  */
#define DOUBLE_TYPE_SIZE ((TARGET_SH3E && ! TARGET_SH4) ? 32 : 64)

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  1

/* We -Define SIZE_TYPE in CPP_SPEC.  */
#define NO_BUILTIN_SIZE_TYPE 1

/* The type of size_t unsigned int.  */
#define SIZE_TYPE (TARGET_SH5 ? "long unsigned int" : "unsigned int")

#define NO_BUILTIN_PTRDIFF_TYPE 1

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_SH5 ? "long int" : "int")

#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Don't cse the address of the function being compiled.  */
/*#define NO_RECURSIVE_FUNCTION_CSE 1*/

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX (TARGET_SHMEDIA ? 8 : 4)

/* Maximum value possibly taken by MOVE_MAX.  Must be defined whenever
   MOVE_MAX is not a compile-time constant.  */
#define MAX_MOVE_MAX 8

/* Max number of bytes we want move_by_pieces to be able to copy
   efficiently.  */
#define MOVE_MAX_PIECES (TARGET_SH4 || TARGET_SHMEDIA ? 8 : 4)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Nonzero if access to memory by bytes is no faster than for words.  */
#define SLOW_BYTE_ACCESS 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Immediate shift counts are truncated by the output routines (or was it
   the assembler?).  Shift counts in a register are truncated by SH.  Note
   that the native compiler puts too large (> 32) immediate shift counts
   into a register and shifts by the register, letting the SH decide what
   to do instead of doing that itself.  */
/* ??? The library routines in lib1funcs.asm truncate the shift count.
   However, the SH3 has hardware shifts that do not truncate exactly as gcc
   expects - the sign bit is significant - so it appears that we need to
   leave this zero for correct SH3 code.  */
#define SHIFT_COUNT_TRUNCATED (! TARGET_SH3)

/* All integers have the same format so truncation is easy.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
/*#define NO_FUNCTION_CSE 1*/

/* Chars and shorts should be passed as ints.  */
#define PROMOTE_PROTOTYPES 1

/* The machine modes of pointers and functions.  */
#define Pmode  (TARGET_SHMEDIA64 ? DImode : SImode)
#define FUNCTION_MODE  Pmode

/* The relative costs of various types of constants.  */

#define CONST_COSTS(RTX, CODE, OUTER_CODE)	\
  case CONST_INT:				\
    if (TARGET_SHMEDIA)				\
      {						\
	if (CONST_OK_FOR_J (INTVAL (RTX)))	\
          return COSTS_N_INSNS (1);		\
	else if (CONST_OK_FOR_J (INTVAL (RTX) >> 16)) \
	  return COSTS_N_INSNS (2);		\
	else if (CONST_OK_FOR_J ((INTVAL (RTX) >> 16) >> 16)) \
	  return COSTS_N_INSNS (3);		\
        else					\
	  return COSTS_N_INSNS (4);		\
      }						\
    if (CONST_OK_FOR_I (INTVAL (RTX)))		\
      return 0;					\
    else if (((OUTER_CODE) == AND || (OUTER_CODE) == IOR || (OUTER_CODE) == XOR) \
	     && CONST_OK_FOR_L (INTVAL (RTX)))	\
      return 1;					\
    else					\
      return 8;					\
  case CONST: 					\
  case LABEL_REF:				\
  case SYMBOL_REF:				\
    if (TARGET_SHMEDIA64)			\
      return COSTS_N_INSNS (4);			\
    if (TARGET_SHMEDIA32)			\
      return COSTS_N_INSNS (2);			\
    return 5;					\
  case CONST_DOUBLE:				\
    if (TARGET_SHMEDIA)				\
      return COSTS_N_INSNS (4);			\
    else					\
      return 10;

#define RTX_COSTS(X, CODE, OUTER_CODE)			\
  case PLUS:						\
    return COSTS_N_INSNS (addsubcosts (X));		\
  case AND:						\
    return COSTS_N_INSNS (andcosts (X));		\
  case MULT:						\
    return COSTS_N_INSNS (multcosts (X));		\
  case ASHIFT:						\
  case ASHIFTRT:					\
  case LSHIFTRT:					\
    return COSTS_N_INSNS (shiftcosts (X));		\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (20);				\
  case FLOAT:						\
  case FIX:						\
    return 100;

/* The multiply insn on the SH1 and the divide insns on the SH1 and SH2
   are actually function calls with some special constraints on arguments
   and register usage.

   These macros tell reorg that the references to arguments and
   register clobbers for insns of type sfunc do not appear to happen
   until after the millicode call.  This allows reorg to put insns
   which set the argument registers into the delay slot of the millicode
   call -- thus they act more like traditional CALL_INSNs.

   get_attr_is_sfunc will try to recognize the given insn, so make sure to
   filter out things it will not accept -- SEQUENCE, USE and CLOBBER insns
   in particular.  */

#define INSN_SETS_ARE_DELAYED(X) 		\
  ((GET_CODE (X) == INSN			\
    && GET_CODE (PATTERN (X)) != SEQUENCE	\
    && GET_CODE (PATTERN (X)) != USE		\
    && GET_CODE (PATTERN (X)) != CLOBBER	\
    && get_attr_is_sfunc (X)))

#define INSN_REFERENCES_ARE_DELAYED(X) 		\
  ((GET_CODE (X) == INSN			\
    && GET_CODE (PATTERN (X)) != SEQUENCE	\
    && GET_CODE (PATTERN (X)) != USE		\
    && GET_CODE (PATTERN (X)) != CLOBBER	\
    && get_attr_is_sfunc (X)))


/* Position Independent Code.  */
/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or function
   named by the symbol (such as what section it is in).

   On SH, if using PIC, mark a SYMBOL_REF for a non-global symbol
   so that we may access it using GOTOFF instead of GOT.  */

#define ENCODE_SECTION_INFO(DECL) \
do									\
  {									\
    if (flag_pic)							\
      {									\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		   ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));		\
									\
	SYMBOL_REF_FLAG (XEXP (rtl, 0)) =				\
	  (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'			\
	   || ! TREE_PUBLIC (DECL));					\
      }									\
    if (TARGET_SH5)							\
      {									\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		   ? TREE_CST_RTL (DECL)				\
		   : TREE_CODE (DECL) != VAR_DECL			\
		   ? NULL_RTX						\
 		   : DECL_RTL (DECL));					\
									\
        if (rtl && GET_CODE (rtl) == MEM				\
 	    && GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF)			\
	  XEXP (rtl, 0) = gen_datalabel_ref (XEXP (rtl, 0));		\
      }									\
  }									\
while (0)

/* The prefix used to mark SYMBOL_REFs that refer to data symbols.  */
#define SH_DATALABEL_ENCODING "#"

/* Return true if SYM_NAME starts with SH_DATALABEL_ENCODING.  */
#define DATALABEL_SYMNAME_P(SYM_NAME) \
  (SH_DATALABEL_ENCODING[1] ? (abort (), 0) : \
   (SYM_NAME)[0] == SH_DATALABEL_ENCODING[0])

/* Skip an optional SH_DATALABEL_ENCODING in the beginning of
   SYM_NAME.  Then, remove a leading *, like the default definition in
   output.h.  */
#define STRIP_DATALABEL_ENCODING(VAR, SYM_NAME) \
  (VAR) = (SYM_NAME) + (DATALABEL_SYMNAME_P (SYM_NAME) \
			? strlen (SH_DATALABEL_ENCODING) : 0)
#define STRIP_NAME_ENCODING(VAR, SYM_NAME) \
  STRIP_DATALABEL_ENCODING((VAR), (SYM_NAME)), \
  (VAR) += (*(VAR) == '*')

/* We can't directly access anything that contains a symbol,
   nor can we indirect via the constant pool.  */
#define LEGITIMATE_PIC_OPERAND_P(X)				\
	(! nonpic_symbol_mentioned_p (X)			\
	 && (! CONSTANT_POOL_ADDRESS_P (X)			\
	     || ! nonpic_symbol_mentioned_p (get_pool_constant (X))))

#define SYMBOLIC_CONST_P(X)	\
((GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == LABEL_REF)	\
  && nonpic_symbol_mentioned_p (X))

/* Compute the cost of an address.  For the SH, all valid addresses are
   the same cost.  Use a slightly higher cost for reg + reg addressing,
   since it increases pressure on r0.  */

#define ADDRESS_COST(X) (GET_CODE (X) == PLUS && ! CONSTANT_P (XEXP (X, 1)) \
			 && ! TARGET_SHMEDIA \
			 ? 1 : 0)

/* Compute extra cost of moving data between one register class
   and another.  */

/* Regclass always uses 2 for moves in the same register class;
   If SECONDARY*_RELOAD_CLASS says something about the src/dst pair,
   it uses this information.  Hence, the general register <-> floating point
   register information here is not used for SFmode.  */
#define REGISTER_MOVE_COST(MODE, SRCCLASS, DSTCLASS) \
 (((((DSTCLASS) == T_REGS) || ((DSTCLASS) == PR_REGS)) ? 10		\
   : ((((DSTCLASS) == FP0_REGS || (DSTCLASS) == FP_REGS || (DSTCLASS) == DF_REGS) \
       && ((SRCCLASS) == GENERAL_REGS || (SRCCLASS) == R0_REGS))	\
      || (((DSTCLASS) == GENERAL_REGS || (DSTCLASS) == R0_REGS)		\
	  && ((SRCCLASS) == FP0_REGS || (SRCCLASS) == FP_REGS		\
	      || (SRCCLASS) == DF_REGS)))				\
   ? (TARGET_SHMEDIA ? 2						\
      : TARGET_FMOVD ? 8 : 12)						\
   : (((DSTCLASS) == FPUL_REGS						\
       && ((SRCCLASS) == GENERAL_REGS || (SRCCLASS) == R0_REGS))	\
      || (SRCCLASS == FPUL_REGS						\
	  && ((DSTCLASS) == GENERAL_REGS || (DSTCLASS) == R0_REGS)))	\
   ? 5									\
   : (((DSTCLASS) == FPUL_REGS						\
       && ((SRCCLASS) == PR_REGS || (SRCCLASS) == MAC_REGS		\
	    || (SRCCLASS) == T_REGS))					\
      || ((SRCCLASS) == FPUL_REGS					\
	  && ((DSTCLASS) == PR_REGS || (DSTCLASS) == MAC_REGS)))	\
   ? 7									\
   : (((SRCCLASS) == TARGET_REGS && (DSTCLASS) != GENERAL_REGS)		\
      || ((DSTCLASS) == TARGET_REGS && (SRCCLASS) != GENERAL_REGS))	\
   ? 20									\
   : (((SRCCLASS) == FPSCR_REGS && (DSTCLASS) != GENERAL_REGS)		\
      || ((DSTCLASS) == FPSCR_REGS && (SRCCLASS) != GENERAL_REGS))	\
   ? 4									\
   : 2) * ((MODE) == V16SFmode ? 8 : (MODE) == V4SFmode ? 2 : 1))

/* ??? Perhaps make MEMORY_MOVE_COST depend on compiler option?  This
   would be so that people with slow memory systems could generate
   different code that does fewer memory accesses.  */

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.
   The SH1 does not have delay slots, hence we get a pipeline stall
   at every branch.  The SH4 is superscalar, so the single delay slot
   is not sufficient to keep both pipelines filled.  */
#define BRANCH_COST (TARGET_SH5 ? 1 : ! TARGET_SH2 || TARGET_HARD_SH4 ? 2 : 1)

/* Assembler output control.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START "!"

/* The text to go at the start of the assembler file.  */
#define ASM_FILE_START(STREAM) \
  output_file_start (STREAM)

#define ASM_FILE_END(STREAM)

#define ASM_APP_ON  		""
#define ASM_APP_OFF  		""
#define FILE_ASM_OP 		"\t.file\n"
#define IDENT_ASM_OP 		"\t.ident\t"
#define SET_ASM_OP		"\t.set\t"

/* How to change between sections.  */

#define TEXT_SECTION_ASM_OP  		(TARGET_SHMEDIA32 ? "\t.section\t.text..SHmedia32,\"ax\"" : "\t.text")
#define DATA_SECTION_ASM_OP  		"\t.data"

#if defined CRT_BEGIN || defined CRT_END
/* Arrange for TEXT_SECTION_ASM_OP to be a compile-time constant.  */
# undef TEXT_SECTION_ASM_OP
# if __SHMEDIA__ == 1 && __SH5__ == 32
#  define TEXT_SECTION_ASM_OP "\t.section\t.text..SHmedia32,\"ax\""
# else
#  define TEXT_SECTION_ASM_OP "\t.text"
# endif
#endif


/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#ifndef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"
#endif

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used
   in place of `ASM_OUTPUT_BSS', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.

   Try to use function `asm_output_aligned_bss' defined in file
   `varasm.c' when defining this macro.  */
#ifndef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)
#endif

/* Define this so that jump tables go in same section as the current function,
   which could be text or it could be a user defined section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  sh_asm_named_section

#undef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY			\
{						\
  typedef (*pfunc)();				\
  extern pfunc __ctors[];			\
  extern pfunc __ctors_end[];			\
  pfunc *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}

#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY			\
{						\
  typedef (*pfunc)();				\
  extern pfunc __dtors[];			\
  extern pfunc __dtors_end[];			\
  pfunc *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}

#define ASM_OUTPUT_REG_PUSH(file, v) \
  fprintf ((file), "\tmov.l\tr%d,@-r15\n", (v));

#define ASM_OUTPUT_REG_POP(file, v) \
  fprintf ((file), "\tmov.l\t@r15+,r%d\n", (v));

/* DBX register number for a given compiler register number.  */
/* GDB has FPUL at 23 and FP0 at 25, so we must add one to all FP registers
   to match gdb.  */
/* If you change this macro, make sure you update it in elf.h too.  */
#define DBX_REGISTER_NUMBER(REGNO) \
  (GENERAL_REGISTER_P (REGNO) \
   ? ((REGNO) - FIRST_GENERAL_REG) \
   : FP_REGISTER_P (REGNO) \
   ? ((REGNO) - FIRST_FP_REG + (TARGET_SH5 ? (TARGET_SHCOMPACT ? 245 \
					      : 77) : 25)) \
   : XD_REGISTER_P (REGNO) \
   ? ((REGNO) - FIRST_XD_REG + (TARGET_SH5 ? 289 : 87)) \
   : TARGET_REGISTER_P (REGNO) \
   ? ((REGNO) - FIRST_TARGET_REG + 68) \
   : (REGNO) == PR_REG \
   ? (TARGET_SH5 ? 241 : 17) \
   : (REGNO) == T_REG \
   ? (TARGET_SH5 ? 242 : 18) \
   : (REGNO) == GBR_REG \
   ? (TARGET_SH5 ? 238 : 19) \
   : (REGNO) == MACH_REG \
   ? (TARGET_SH5 ? 239 : 20) \
   : (REGNO) == MACL_REG \
   ? (TARGET_SH5 ? 240 : 21) \
   : (REGNO) == FPUL_REG \
   ? (TARGET_SH5 ? 244 : 23) \
   : (abort(), -1))

/* This is how to output a reference to a user-level label named NAME.  */
#define ASM_OUTPUT_LABELREF(FILE, NAME)			\
  do							\
    {							\
      char * lname;					\
							\
      STRIP_DATALABEL_ENCODING (lname, (NAME));		\
      if (lname[0] == '*')				\
	fputs (lname + 1, (FILE));			\
      else						\
	asm_fprintf ((FILE), "%U%s", lname);		\
    }							\
  while (0)

/* This is how to output a reference to a symbol_ref.  On SH5,
   references to non-code symbols must be preceded by `datalabel'.  */
#define ASM_OUTPUT_SYMBOL_REF(FILE,SYM)			\
  do 							\
    {							\
      if (TARGET_SH5					\
	  && (DATALABEL_SYMNAME_P (XSTR ((SYM), 0))	\
	      || CONSTANT_POOL_ADDRESS_P (SYM)))	\
	fputs ("datalabel ", (FILE));			\
      assemble_name ((FILE), XSTR ((SYM), 0));		\
    }							\
  while (0)

/* Output a label definition.  */
#define ASM_OUTPUT_LABEL(FILE,NAME) \
  do { assemble_name ((FILE), (NAME)); fputs (":\n", (FILE)); } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf ((FILE), "\t.align %d\n", (LOG))

/* Output a function label definition.  */
#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL) \
    ASM_OUTPUT_LABEL((STREAM), (NAME))

/* Output a globalising directive for a label.  */
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)	\
  (fprintf ((STREAM), "\t.global\t"),		\
   assemble_name ((STREAM), (NAME)),		\
   fputc ('\n', (STREAM)))

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* The prefix to add to an internally generated label.  */

#define LOCAL_LABEL_PREFIX ""

/* Make an internal label into a string.  */
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf ((STRING), "*%s%s%ld", LOCAL_LABEL_PREFIX, (PREFIX), (long)(NUM))

/* Output an internal label definition.  */
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM) \
  asm_fprintf ((FILE), "%L%s%d:\n", (PREFIX), (NUM))

/* #define ASM_OUTPUT_CASE_END(STREAM,NUM,TABLE)	    */

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)	\
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),	\
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Output a relative address table.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)  		\
  switch (GET_MODE (BODY))						\
    {									\
    case SImode:							\
      if (TARGET_SH5)							\
	{								\
	  asm_fprintf ((STREAM), "\t.long\t%LL%d-datalabel %LL%d\n",	\
		       (VALUE), (REL));					\
	  break;							\
	}								\
      asm_fprintf ((STREAM), "\t.long\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case HImode:							\
      if (TARGET_SH5)							\
	{								\
	  asm_fprintf ((STREAM), "\t.word\t%LL%d-datalabel %LL%d\n",	\
		       (VALUE), (REL));					\
	  break;							\
	}								\
      asm_fprintf ((STREAM), "\t.word\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case QImode:							\
      if (TARGET_SH5)							\
	{								\
	  asm_fprintf ((STREAM), "\t.byte\t%LL%d-datalabel %LL%d\n",	\
		       (VALUE), (REL));					\
	  break;							\
	}								\
      asm_fprintf ((STREAM), "\t.byte\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    default:								\
      break;								\
    }

/* Output an absolute table element.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  				\
  if (TARGET_BIGTABLE) 							\
    asm_fprintf ((STREAM), "\t.long\t%LL%d\n", (VALUE)); 			\
  else									\
    asm_fprintf ((STREAM), "\t.word\t%LL%d\n", (VALUE)); 			\

/* Output various types of constants.  */

/* Loop alignment is now done in machine_dependent_reorg, so that
   branch shortening can know about it.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE) \
  fprintf ((FILE), "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* A C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.
   The contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler output
   by changing the contents of the vector.  */

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  final_prescan_insn ((INSN), (OPVEC), (NOPERANDS))

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(STREAM, X, CODE)  print_operand ((STREAM), (X), (CODE))

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(STREAM,X)  print_operand_address ((STREAM), (X))

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR) == '.' || (CHAR) == '#' || (CHAR) == '@' || (CHAR) == ','	\
   || (CHAR) == '$')

/* Recognize machine-specific patterns that may appear within
   constants.  Used for PIC-specific UNSPECs.  */
#define OUTPUT_ADDR_CONST_EXTRA(STREAM, X, FAIL) \
  do									\
    if (GET_CODE (X) == UNSPEC && XVECLEN ((X), 0) == 1)	\
      {									\
	switch (XINT ((X), 1))						\
	  {								\
	  case UNSPEC_DATALABEL:					\
	    fputs ("datalabel ", (STREAM));				\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    break;							\
	  case UNSPEC_PIC:						\
	    /* GLOBAL_OFFSET_TABLE or local symbols, no suffix.  */	\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    break;							\
	  case UNSPEC_GOT:						\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    fputs ("@GOT", (STREAM));					\
	    break;							\
	  case UNSPEC_GOTOFF:						\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    fputs ("@GOTOFF", (STREAM));				\
	    break;							\
	  case UNSPEC_PLT:						\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    fputs ("@PLT", (STREAM));					\
	    break;							\
	  case UNSPEC_GOTPLT:						\
	    output_addr_const ((STREAM), XVECEXP ((X), 0, 0));		\
	    fputs ("@GOTPLT", (STREAM));				\
	    break;							\
	  case UNSPEC_CALLER:						\
	    {								\
	      char name[32];						\
	      /* LPCS stands for Label for PIC Call Site.  */		\
	      ASM_GENERATE_INTERNAL_LABEL				\
		(name, "LPCS", INTVAL (XVECEXP ((X), 0, 0)));		\
	      assemble_name ((STREAM), name);				\
	    }								\
	    break;							\
	  default:							\
	    goto FAIL;							\
	  }								\
	break;								\
      }									\
    else								\
      goto FAIL;							\
  while (0)


extern struct rtx_def *sh_compare_op0;
extern struct rtx_def *sh_compare_op1;

/* Which processor to schedule for.  The elements of the enumeration must
   match exactly the cpu attribute in the sh.md file.  */

enum processor_type {
  PROCESSOR_SH1,
  PROCESSOR_SH2,
  PROCESSOR_SH3,
  PROCESSOR_SH3E,
  PROCESSOR_SH4,
  PROCESSOR_SH5
};

#define sh_cpu_attr ((enum attr_cpu)sh_cpu)
extern enum processor_type sh_cpu;

extern int optimize; /* needed for gen_casesi.  */

enum mdep_reorg_phase_e
{
  SH_BEFORE_MDEP_REORG,
  SH_INSERT_USES_LABELS,
  SH_SHORTEN_BRANCHES0,
  SH_FIXUP_PCLOAD,
  SH_SHORTEN_BRANCHES1,
  SH_AFTER_MDEP_REORG
};

extern enum mdep_reorg_phase_e mdep_reorg_phase;

#define MACHINE_DEPENDENT_REORG(X) machine_dependent_reorg(X)

/* Generate calls to memcpy, memcmp and memset.  */

#define TARGET_MEM_FUNCTIONS

/* Handle Hitachi compiler's pragmas.  */
#define REGISTER_TARGET_PRAGMAS(PFILE) do {				    \
  cpp_register_pragma (PFILE, 0, "interrupt", sh_pr_interrupt);		    \
  cpp_register_pragma (PFILE, 0, "trapa", sh_pr_trapa);			    \
  cpp_register_pragma (PFILE, 0, "nosave_low_regs", sh_pr_nosave_low_regs); \
} while (0)

/* Set when processing a function with pragma interrupt turned on.  */

extern int pragma_interrupt;

/* Set when processing a function with interrupt attribute.  */

extern int current_function_interrupt;

/* Set to an RTX containing the address of the stack to switch to
   for interrupt functions.  */
extern struct rtx_def *sp_switch;

extern int rtx_equal_function_value_matters;
extern struct rtx_def *fpscr_rtx;


/* Instructions with unfilled delay slots take up an
   extra two bytes for the nop in the delay slot.
   sh-dsp parallel processing insns are four bytes long.  */

#define ADJUST_INSN_LENGTH(X, LENGTH)				\
  (LENGTH) += sh_insn_length_adjustment (X);

/* Define the codes that are matched by predicates in sh.c.  */
#define PREDICATE_CODES \
  {"arith_operand", {SUBREG, REG, CONST_INT}},				\
  {"arith_reg_operand", {SUBREG, REG}},					\
  {"arith_reg_or_0_operand", {SUBREG, REG, CONST_INT}},			\
  {"binary_float_operator", {PLUS, MULT}},				\
  {"commutative_float_operator", {PLUS, MULT}},				\
  {"fp_arith_reg_operand", {SUBREG, REG}},				\
  {"fpscr_operand", {REG}},						\
  {"fpul_operand", {REG}},						\
  {"general_movsrc_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE, MEM}}, \
  {"general_movdst_operand", {SUBREG, REG, MEM}},			\
  {"logical_operand", {SUBREG, REG, CONST_INT}},			\
  {"noncommutative_float_operator", {MINUS, DIV}},			\
  {"shmedia_6bit_operand", {SUBREG, REG, CONST_INT}},			\
  {"target_reg_operand", {SUBREG, REG}},				\
  {"target_operand", {SUBREG, REG, LABEL_REF, SYMBOL_REF}},		\
  {"register_operand", {SUBREG, REG}},					\
  {"symbol_ref_operand", {SYMBOL_REF}},

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Leaving the unsignedp unchanged gives better code than always setting it
   to 0.  This is despite the fact that we have only signed char and short
   load instructions.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
  if (GET_MODE_CLASS (MODE) == MODE_INT			\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)		\
    (UNSIGNEDP) = ((MODE) == SImode ? 0 : (UNSIGNEDP)),	\
    (MODE) = (TARGET_SH1 ? SImode : DImode);

/* Defining PROMOTE_FUNCTION_ARGS eliminates some unnecessary zero/sign
   extensions applied to char/short functions arguments.  Defining
   PROMOTE_FUNCTION_RETURN does the same for function returns.  */

#define PROMOTE_FUNCTION_ARGS
#define PROMOTE_FUNCTION_RETURN

/* ??? Define ACCUMULATE_OUTGOING_ARGS?  This is more efficient than pushing
   and poping arguments.  However, we do have push/pop instructions, and
   rather limited offsets (4 bits) in load/store instructions, so it isn't
   clear if this would give better code.  If implemented, should check for
   compatibility problems.  */

#define SH_DYNAMIC_SHIFT_COST \
  (TARGET_HARD_SH4 ? 1 : TARGET_SH3 ? (TARGET_SMALLCODE ? 1 : 2) : 20)


#define NUM_MODES_FOR_MODE_SWITCHING { FP_MODE_NONE }

#define OPTIMIZE_MODE_SWITCHING(ENTITY) TARGET_SH4

#define NORMAL_MODE(ENTITY) \
   (TARGET_FPU_SINGLE ? FP_MODE_SINGLE : FP_MODE_DOUBLE) 

#define EPILOGUE_USES(REGNO)       ((TARGET_SH3E || TARGET_SH4)		\
				    && (REGNO) == FPSCR_REG)

#define MODE_NEEDED(ENTITY, INSN)					\
  (recog_memoized (INSN) >= 0						\
   ? get_attr_fp_mode (INSN)						\
   : FP_MODE_NONE)

#define MODE_PRIORITY_TO_MODE(ENTITY, N) \
  ((TARGET_FPU_SINGLE != 0) ^ (N) ? FP_MODE_SINGLE : FP_MODE_DOUBLE)

#define EMIT_MODE_SET(ENTITY, MODE, HARD_REGS_LIVE) \
  fpscr_set_from_mem ((MODE), (HARD_REGS_LIVE))

#define MD_CAN_REDIRECT_BRANCH(INSN, SEQ) \
  sh_can_redirect_branch ((INSN), (SEQ))

#if (defined CRT_BEGIN || defined CRT_END) && ! __SHMEDIA__
/* SH constant pool breaks the devices in crtstuff.c to control section
   in where code resides.  We have to write it as asm code.  */
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC) \
   asm (SECTION_OP "\n\
	mov.l	1f,r1\n\
	mova	2f,r0\n\
	braf	r1\n\
	lds	r0,pr\n\
0:	.p2align 2\n\
1:	.long	" USER_LABEL_PREFIX #FUNC " - 0b\n\
2:\n" TEXT_SECTION_ASM_OP);
#endif /* (defined CRT_BEGIN || defined CRT_END) && ! __SHMEDIA__ */

#define ALLOCATE_INITIAL_VALUE(hard_reg) \
  (REGNO (hard_reg) == PR_REG \
   ? (current_function_is_leaf && ! sh_pr_n_sets () \
      ? (hard_reg) \
      : gen_rtx_MEM (Pmode, arg_pointer_rtx)) \
   : NULL_RTX)

#endif /* ! GCC_SH_H */
