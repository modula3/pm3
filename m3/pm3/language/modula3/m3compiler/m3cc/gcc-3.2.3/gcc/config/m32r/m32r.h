/* Definitions of target machine for GNU compiler, Mitsubishi M32R cpu.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

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

/* Things to do:
- longlong.h?
*/

#undef SWITCH_TAKES_ARG
#undef WORD_SWITCH_TAKES_ARG
#undef HANDLE_SYSV_PRAGMA
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef ASM_FILE_START
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#undef TARGET_VERSION
#undef CPP_SPEC
#undef ASM_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC
#undef SUBTARGET_SWITCHES


/* M32R/X overrides.  */
/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (m32r/x)");

/* Additional flags for the preprocessor.  */
#define CPP_CPU_SPEC "%{m32rx:-D__M32RX__} %{m32r:-U__M32RX__}"

/* Assembler switches.  */
#define ASM_CPU_SPEC \
"%{m32r} %{m32rx} %{!O0: %{O*: -O}} --no-warn-explicit-parallel-conflicts"

/* Use m32rx specific crt0/crtinit/crtfini files.  */
#define STARTFILE_CPU_SPEC "%{!shared:crt0.o%s} %{m32rx:m32rx/crtinit.o%s} %{!m32rx:crtinit.o%s}"
#define ENDFILE_CPU_SPEC "-lgloss %{m32rx:m32rx/crtfini.o%s} %{!m32rx:crtfini.o%s}"

/* Extra machine dependent switches.  */
#define SUBTARGET_SWITCHES							\
    { "32rx",			TARGET_M32RX_MASK, "Compile for the m32rx" },	\
    { "32r",			-TARGET_M32RX_MASK, "" },

/* Define this macro as a C expression for the initializer of an array of
   strings to tell the driver program which options are defaults for this
   target and thus do not need to be handled specially when using
   `MULTILIB_OPTIONS'.  */
#define SUBTARGET_MULTILIB_DEFAULTS , "m32r"

/* Number of additional registers the subtarget defines.  */
#define SUBTARGET_NUM_REGISTERS 1

/* 1 for registers that cannot be allocated.  */
#define SUBTARGET_FIXED_REGISTERS , 1

/* 1 for registers that are not available across function calls.  */
#define SUBTARGET_CALL_USED_REGISTERS , 1

/* Order to allocate model specific registers.  */
#define SUBTARGET_REG_ALLOC_ORDER , 19

/* Registers which are accumulators.  */
#define SUBTARGET_REG_CLASS_ACCUM 0x80000

/* All registers added.  */
#define SUBTARGET_REG_CLASS_ALL SUBTARGET_REG_CLASS_ACCUM

/* Additional accumulator registers.  */
#define SUBTARGET_ACCUM_P(REGNO) ((REGNO) == 19)

/* Define additional register names.  */
#define SUBTARGET_REGISTER_NAMES , "a1"
/* end M32R/X overrides.  */

/* Print subsidiary information on the compiler version in use.  */
#ifndef	TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (m32r)")
#endif

/* Switch  Recognition by gcc.c.  Add -G xx support */

#undef  SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
(DEFAULT_SWITCH_TAKES_ARG (CHAR) || (CHAR) == 'G')

/* Names to predefine in the preprocessor for this target machine.  */
/* __M32R__ is defined by the existing compiler so we use that.  */
#define CPP_PREDEFINES "-Acpu=m32r -Amachine=m32r -D__M32R__"

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#ifndef	ASM_CPU_SPEC
#define	ASM_CPU_SPEC ""
#endif

#ifndef	CPP_CPU_SPEC
#define	CPP_CPU_SPEC ""
#endif

#ifndef	CC1_CPU_SPEC
#define	CC1_CPU_SPEC ""
#endif

#ifndef	LINK_CPU_SPEC
#define	LINK_CPU_SPEC ""
#endif

#ifndef STARTFILE_CPU_SPEC
#define STARTFILE_CPU_SPEC "%{!shared:crt0.o%s} crtinit.o%s"
#endif

#ifndef ENDFILE_CPU_SPEC
#define ENDFILE_CPU_SPEC "-lgloss crtfini.o%s"
#endif

#ifndef RELAX_SPEC
#if 0 /* not supported yet */
#define RELAX_SPEC "%{mrelax:-relax}"
#else
#define RELAX_SPEC ""
#endif
#endif

#define EXTRA_SPECS							\
  { "asm_cpu",			ASM_CPU_SPEC },				\
  { "cpp_cpu",			CPP_CPU_SPEC },				\
  { "cc1_cpu",			CC1_CPU_SPEC },				\
  { "link_cpu",			LINK_CPU_SPEC },			\
  { "startfile_cpu",		STARTFILE_CPU_SPEC },			\
  { "endfile_cpu",		ENDFILE_CPU_SPEC },			\
  { "relax",			RELAX_SPEC },				\
  SUBTARGET_EXTRA_SPECS

#define CC1_SPEC "%{G*} %(cc1_cpu)"

/* Options to pass on to the assembler.  */
#undef  ASM_SPEC
#define ASM_SPEC "%{v} %(asm_cpu) %(relax)"

#undef  ASM_FINAL_SPEC

#define LINK_SPEC "%{v} %(link_cpu) %(relax)"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%(startfile_cpu)"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "%(endfile_cpu)"

#undef LIB_SPEC

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* If non-zero, tell the linker to do relaxing.
   We don't do anything with the option, other than recognize it.
   LINK_SPEC handles passing -relax to the linker.
   This can cause incorrect debugging information as line numbers may
   turn out wrong.  This shouldn't be specified unless accompanied with -O2
   [where the user expects debugging information to be less accurate].  */
#define TARGET_RELAX_MASK 	(1 << 0)

/* For miscellaneous debugging purposes.  */
#define TARGET_DEBUG_MASK 	(1 << 1)
#define TARGET_DEBUG 		(target_flags & TARGET_DEBUG_MASK)

/* Align loops to 32 byte boundaries (cache line size).  */
/* ??? This option is experimental and is not documented.  */
#define TARGET_ALIGN_LOOPS_MASK (1 << 2)
#define TARGET_ALIGN_LOOPS 	(target_flags & TARGET_ALIGN_LOOPS_MASK)

/* Change issue rate.  */
#define TARGET_LOW_ISSUE_RATE_MASK	(1 << 3)
#define TARGET_LOW_ISSUE_RATE	(target_flags & TARGET_LOW_ISSUE_RATE_MASK)

/* Change branch cost */
#define TARGET_BRANCH_COST_MASK	(1 << 4)
#define TARGET_BRANCH_COST	(target_flags & TARGET_BRANCH_COST_MASK)

/* Target machine to compile for.  */
#define TARGET_M32R 		1

/* Support extended instruction set.  */
#define TARGET_M32RX_MASK       (1 << 5)
#define TARGET_M32RX            (target_flags & TARGET_M32RX_MASK)
#undef  TARGET_M32R
#define TARGET_M32R             (! TARGET_M32RX)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#ifndef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES
#endif

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#define TARGET_SWITCHES							\
{									\
/*  { "relax",			TARGET_RELAX_MASK, "" },		\
    { "no-relax",		-TARGET_RELAX_MASK, "" },*/		\
    { "debug",			TARGET_DEBUG_MASK, 			\
	N_("Display compile time statistics") },			\
    { "align-loops",		TARGET_ALIGN_LOOPS_MASK, 		\
	N_("Align all loops to 32 byte boundary") },			\
    { "no-align-loops",		-TARGET_ALIGN_LOOPS_MASK, "" },		\
    { "issue-rate=1",		TARGET_LOW_ISSUE_RATE_MASK, 		\
	N_("Only issue one instruction per cycle") },			\
    { "issue-rate=2",		-TARGET_LOW_ISSUE_RATE_MASK, "" },	\
    { "branch-cost=1",		TARGET_BRANCH_COST_MASK, 		\
	N_("Prefer branches over conditional execution") },		\
    { "branch-cost=2",		-TARGET_BRANCH_COST_MASK, "" },		\
    SUBTARGET_SWITCHES							\
    { "", TARGET_DEFAULT, "" }						\
}

extern const char * m32r_model_string;
extern const char * m32r_sdata_string;

#ifndef SUBTARGET_OPTIONS
#define SUBTARGET_OPTIONS
#endif

#define TARGET_OPTIONS							\
{									\
  { "model=", & m32r_model_string,					\
    N_("Code size: small, medium or large") },				\
  { "sdata=", & m32r_sdata_string,					\
    N_("Small data area: none, sdata, use") }				\
  SUBTARGET_OPTIONS							\
}

/* Code Models

   Code models are used to select between two choices of two separate
   possibilities (address space size, call insn to use):

   small: addresses use 24 bits, use bl to make calls
   medium: addresses use 32 bits, use bl to make calls (*1)
   large: addresses use 32 bits, use seth/add3/jl to make calls (*2)

   The fourth is "addresses use 24 bits, use seth/add3/jl to make calls" but
   using this one doesn't make much sense.

   (*1) The linker may eventually be able to relax seth/add3 -> ld24.
   (*2) The linker may eventually be able to relax seth/add3/jl -> bl.

   Internally these are recorded as TARGET_ADDR{24,32} and
   TARGET_CALL{26,32}.

   The __model__ attribute can be used to select the code model to use when
   accessing particular objects.  */

enum m32r_model { M32R_MODEL_SMALL, M32R_MODEL_MEDIUM, M32R_MODEL_LARGE };

extern enum m32r_model m32r_model;
#define TARGET_MODEL_SMALL (m32r_model == M32R_MODEL_SMALL)
#define TARGET_MODEL_MEDIUM (m32r_model == M32R_MODEL_MEDIUM)
#define TARGET_MODEL_LARGE (m32r_model == M32R_MODEL_LARGE)
#define TARGET_ADDR24 (m32r_model == M32R_MODEL_SMALL)
#define TARGET_ADDR32 (! TARGET_ADDR24)
#define TARGET_CALL26 (! TARGET_CALL32)
#define TARGET_CALL32 (m32r_model == M32R_MODEL_LARGE)

/* The default is the small model.  */
#ifndef M32R_MODEL_DEFAULT
#define M32R_MODEL_DEFAULT "small"
#endif

/* Small Data Area

   The SDA consists of sections .sdata, .sbss, and .scommon.
   .scommon isn't a real section, symbols in it have their section index
   set to SHN_M32R_SCOMMON, though support for it exists in the linker script.

   Two switches control the SDA:

   -G NNN        - specifies the maximum size of variable to go in the SDA

   -msdata=foo   - specifies how such variables are handled

        -msdata=none  - small data area is disabled

        -msdata=sdata - small data goes in the SDA, special code isn't
                        generated to use it, and special relocs aren't
                        generated

        -msdata=use   - small data goes in the SDA, special code is generated
                        to use the SDA and special relocs are generated

   The SDA is not multilib'd, it isn't necessary.
   MULTILIB_EXTRA_OPTS is set in tmake_file to -msdata=sdata so multilib'd
   libraries have small data in .sdata/SHN_M32R_SCOMMON so programs that use
   -msdata=use will successfully link with them (references in header files
   will cause the compiler to emit code that refers to library objects in
   .data).  ??? There can be a problem if the user passes a -G value greater
   than the default and a library object in a header file is that size.
   The default is 8 so this should be rare - if it occurs the user
   is required to rebuild the libraries or use a smaller value for -G.
*/

/* Maximum size of variables that go in .sdata/.sbss.
   The -msdata=foo switch also controls how small variables are handled.  */
#ifndef SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE 8
#endif

extern int g_switch_value;		/* value of the -G xx switch */
extern int g_switch_set;		/* whether -G xx was passed.  */

enum m32r_sdata { M32R_SDATA_NONE, M32R_SDATA_SDATA, M32R_SDATA_USE };

extern enum m32r_sdata m32r_sdata;
#define TARGET_SDATA_NONE (m32r_sdata == M32R_SDATA_NONE)
#define TARGET_SDATA_SDATA (m32r_sdata == M32R_SDATA_SDATA)
#define TARGET_SDATA_USE (m32r_sdata == M32R_SDATA_USE)

/* Default is to disable the SDA
   [for upward compatibility with previous toolchains].  */
#ifndef M32R_SDATA_DEFAULT
#define M32R_SDATA_DEFAULT "none"
#endif

/* Define this macro as a C expression for the initializer of an array of
   strings to tell the driver program which options are defaults for this
   target and thus do not need to be handled specially when using
   `MULTILIB_OPTIONS'.  */
#ifndef SUBTARGET_MULTILIB_DEFAULTS
#define SUBTARGET_MULTILIB_DEFAULTS
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mmodel=small" SUBTARGET_MULTILIB_DEFAULTS }
#endif

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#ifndef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS
#endif

#define OVERRIDE_OPTIONS			\
  do						\
    {						\
      /* These need to be done at start up.	\
	 It's convenient to do them here.  */	\
      m32r_init ();				\
      SUBTARGET_OVERRIDE_OPTIONS		\
    }						\
  while (0)

#ifndef SUBTARGET_OPTIMIZATION_OPTIONS
#define SUBTARGET_OPTIMIZATION_OPTIONS
#endif

#define OPTIMIZATION_OPTIONS(LEVEL, SIZE)	\
  do						\
    {						\
      if (LEVEL == 1)				\
	flag_regmove = TRUE;			\
      						\
      if (SIZE)					\
	{					\
	  flag_omit_frame_pointer = TRUE;	\
	  flag_strength_reduce = FALSE;		\
	}					\
      						\
      SUBTARGET_OPTIMIZATION_OPTIONS		\
    }						\
  while (0)

/* Define this macro if debugging can be performed even without a
   frame pointer.  If this macro is defined, GNU CC will turn on the
   `-fomit-frame-pointer' option whenever `-O' is specified.  */
#define CAN_DEBUG_WITHOUT_FP

/* Target machine storage layout.  */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

/* Define this macro if WORDS_BIG_ENDIAN is not constant.  This must
   be a constant value with the same meaning as WORDS_BIG_ENDIAN,
   which will be used only when compiling libgcc2.c.  Typically the
   value will be set based on preprocessor defines.  */
/*#define LIBGCC2_WORDS_BIG_ENDIAN 1*/

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
    {						\
      (MODE) = SImode;				\
    }

/* Define this macro if the promotion described by `PROMOTE_MODE'
   should also be done for outgoing function arguments.  */
/*#define PROMOTE_FUNCTION_ARGS*/

/* Likewise, if the function return value is promoted.
   If defined, FUNCTION_VALUE must perform the same promotions done by
   PROMOTE_MODE.  */
/*#define PROMOTE_FUNCTION_RETURN*/

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* ALIGN FRAMES on word boundaries */
#define M32R_STACK_ALIGN(LOC) (((LOC)+3) & ~3)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)	\
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  (TREE_CODE (TYPE) == ARRAY_TYPE					\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode				\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Layout of source language data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long int"
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

#define M32R_NUM_REGISTERS 	19

#ifndef SUBTARGET_NUM_REGISTERS
#define SUBTARGET_NUM_REGISTERS 0
#endif

#define FIRST_PSEUDO_REGISTER (M32R_NUM_REGISTERS + SUBTARGET_NUM_REGISTERS)
	
/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   0-3   - arguments/results
   4-5   - call used [4 is used as a tmp during prologue/epilogue generation]
   6     - call used, gptmp
   7     - call used, static chain pointer
   8-11  - call saved
   12    - call saved [reserved for global pointer]
   13    - frame pointer
   14    - subroutine link register
   15    - stack pointer
   16    - arg pointer
   17    - carry flag
   18	 - accumulator
   19    - accumulator 1 in the m32r/x
   By default, the extension registers are not available.  */

#ifndef SUBTARGET_FIXED_REGISTERS
#define SUBTARGET_FIXED_REGISTERS
#endif

#define FIXED_REGISTERS		\
{				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 1,	\
  1, 1, 1			\
  SUBTARGET_FIXED_REGISTERS	\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#ifndef SUBTARGET_CALL_USED_REGISTERS
#define SUBTARGET_CALL_USED_REGISTERS
#endif

#define CALL_USED_REGISTERS	\
{				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1			\
  SUBTARGET_CALL_USED_REGISTERS	\
}

/* Zero or more C statements that may conditionally modify two variables
   `fixed_regs' and `call_used_regs' (both of type `char []') after they
   have been initialized from the two preceding macros.

   This is necessary in case the fixed or call-clobbered registers depend
   on target flags.

   You need not define this macro if it has no work to do.  */

#ifdef SUBTARGET_CONDITIONAL_REGISTER_USAGE
#define CONDITIONAL_REGISTER_USAGE SUBTARGET_CONDITIONAL_REGISTER_USAGE
#endif

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GNU CC should
   prefer to use them (from most preferred to least).  */

#ifndef SUBTARGET_REG_ALLOC_ORDER
#define SUBTARGET_REG_ALLOC_ORDER
#endif

#if 1 /* better for int code */
#define REG_ALLOC_ORDER				\
{						\
  4,  5,  6,  7,  2,  3,  8,  9, 10,		\
  11, 12, 13, 14,  0,  1, 15, 16, 17, 18	\
  SUBTARGET_REG_ALLOC_ORDER			\
}

#else /* better for fp code at expense of int code */
#define REG_ALLOC_ORDER				\
{						\
   0,  1,  2,  3,  4,  5,  6,  7,  8,		\
   9, 10, 11, 12, 13, 14, 15, 16, 17, 18	\
  SUBTARGET_REG_ALLOC_ORDER			\
}
#endif

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE) \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */
extern unsigned int m32r_hard_regno_mode_ok[FIRST_PSEUDO_REGISTER];
extern unsigned int m32r_mode_class[];
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
((m32r_hard_regno_mode_ok[REGNO] & m32r_mode_class[MODE]) != 0)

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

/* Tie QI/HI/SI modes together.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
(GET_MODE_CLASS (MODE1) == MODE_INT		\
 && GET_MODE_CLASS (MODE2) == MODE_INT		\
 && GET_MODE_SIZE (MODE1) <= UNITS_PER_WORD	\
 && GET_MODE_SIZE (MODE2) <= UNITS_PER_WORD)

/* Register classes and constants.  */

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
   class that represents their union.

   It is important that any condition codes have class NO_REGS.
   See `register_operand'.  */

enum reg_class
{
  NO_REGS, CARRY_REG, ACCUM_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES \
  { "NO_REGS", "CARRY_REG", "ACCUM_REGS", "GENERAL_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#ifndef SUBTARGET_REG_CLASS_CARRY
#define SUBTARGET_REG_CLASS_CARRY 0
#endif

#ifndef SUBTARGET_REG_CLASS_ACCUM
#define SUBTARGET_REG_CLASS_ACCUM 0
#endif

#ifndef SUBTARGET_REG_CLASS_GENERAL
#define SUBTARGET_REG_CLASS_GENERAL 0
#endif

#ifndef SUBTARGET_REG_CLASS_ALL
#define SUBTARGET_REG_CLASS_ALL 0
#endif

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000 },								\
  { 0x20000 | SUBTARGET_REG_CLASS_CARRY },				\
  { 0x40000 | SUBTARGET_REG_CLASS_ACCUM },				\
  { 0x1ffff | SUBTARGET_REG_CLASS_GENERAL },				\
  { 0x7ffff | SUBTARGET_REG_CLASS_ALL },				\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
extern enum reg_class m32r_regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) (m32r_regno_reg_class[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

#define REG_CLASS_FROM_LETTER(C)					\
((C) == 'c'	? CARRY_REG						\
 : (C) == 'a'	? ACCUM_REGS						\
 :		  NO_REGS)

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < FIRST_PSEUDO_REGISTER			\
 ? GPR_P (REGNO) || (REGNO) == ARG_POINTER_REGNUM	\
 : GPR_P (reg_renumber[REGNO]))
#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) \
(CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */
/* 'I' is used for 8 bit signed immediates.
   'J' is used for 16 bit signed immediates.
   'K' is used for 16 bit unsigned immediates.
   'L' is used for 16 bit immediates left shifted by 16 (sign ???).
   'M' is used for 24 bit unsigned immediates.
   'N' is used for any 32 bit non-symbolic value.
   'O' is used for 5 bit unsigned immediates (shift count).
   'P' is used for 16 bit signed immediates for compares
       (values in the range -32767 to +32768).  */

/* Return true if a value is inside a range.  */
#define IN_RANGE_P(VALUE, LOW, HIGH)					\
  (((unsigned HOST_WIDE_INT)((VALUE) - (LOW)))				\
   <= ((unsigned HOST_WIDE_INT)((HIGH) - (LOW))))

/* Local to this file.  */
#define INT8_P(X) ((X) >= -0x80 && (X) <= 0x7f)
#define INT16_P(X) ((X) >= -0x8000 && (X) <= 0x7fff)
#define CMP_INT16_P(X) ((X) >= -0x7fff && (X) <= 0x8000)
#define UPPER16_P(X) (((X) & 0xffff) == 0				\
		      && ((X) >> 16) >= -0x8000				\
		      && ((X) >> 16) <= 0x7fff)
#define UINT16_P(X) (((unsigned HOST_WIDE_INT) (X)) <= 0x0000ffff)
#define UINT24_P(X) (((unsigned HOST_WIDE_INT) (X)) <= 0x00ffffff)
#define UINT32_P(X) (((unsigned HOST_WIDE_INT) (X)) <= 0xffffffff)
#define UINT5_P(X)  ((X) >= 0 && (X) < 32)
#define INVERTED_SIGNED_8BIT(VAL) ((VAL) >= -127 && (VAL) <= 128)

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
((C) == 'I' ? INT8_P (VALUE)						\
 : (C) == 'J' ? INT16_P (VALUE)						\
 : (C) == 'K' ? UINT16_P (VALUE)					\
 : (C) == 'L' ? UPPER16_P (VALUE)					\
 : (C) == 'M' ? UINT24_P (VALUE)					\
 : (C) == 'N' ? INVERTED_SIGNED_8BIT (VALUE)				\
 : (C) == 'O' ? UINT5_P (VALUE)						\
 : (C) == 'P' ? CMP_INT16_P (VALUE)					\
 : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.
   For the m32r, handle a few constants inline.
   ??? We needn't treat DI and DF modes differently, but for now we do.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
((C) == 'G' ? easy_di_const (VALUE)					\
 : (C) == 'H' ? easy_df_const (VALUE)					\
 : 0)

/* A C expression that defines the optional machine-dependent constraint
   letters that can be used to segregate specific types of operands,
   usually memory references, for the target machine.  It should return 1 if
   VALUE corresponds to the operand type represented by the constraint letter
   C.  If C is not defined as an extra constraint, the value returned should
   be 0 regardless of VALUE.  */
/* Q is for symbolic addresses loadable with ld24.
   R is for symbolic addresses when ld24 can't be used.
   S is for stores with pre {inc,dec}rement
   T is for indirect of a pointer.
   U is for loads with post increment.  */

#define EXTRA_CONSTRAINT(VALUE, C)					\
(  (C) == 'Q' ? ((TARGET_ADDR24 && GET_CODE (VALUE) == LABEL_REF)	\
		 || addr24_operand (VALUE, VOIDmode))			\
 : (C) == 'R' ? ((TARGET_ADDR32 && GET_CODE (VALUE) == LABEL_REF)	\
		 || addr32_operand (VALUE, VOIDmode))			\
 : (C) == 'S' ? (GET_CODE (VALUE) == MEM				\
		 && STORE_PREINC_PREDEC_P (GET_MODE (VALUE),		\
					   XEXP (VALUE, 0)))		\
 : (C) == 'T' ? (GET_CODE (VALUE) == MEM				\
		 && memreg_operand (VALUE, GET_MODE (VALUE)))		\
 : (C) == 'U' ? (GET_CODE (VALUE) == MEM				\
		 && LOAD_POSTINC_P (GET_MODE (VALUE),			\
				    XEXP (VALUE, 0)))			\
 : 0)

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset from the frame pointer.  */
/*#define FRAME_GROWS_DOWNWARD*/

/* Offset from frame pointer to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
/* The frame pointer points at the same place as the stack pointer, except if
   alloca has been called.  */
#define STARTING_FRAME_OFFSET \
M32R_STACK_ALIGN (current_function_outgoing_args_size)

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET 0

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* A C expression whose value is RTL representing the address in a
   stack frame where the pointer to the caller's frame is stored.
   Assume that FRAMEADDR is an RTL expression for the address of the
   stack frame itself.

   If you don't define this macro, the default is to return the value
   of FRAMEADDR--that is, the stack frame address is also the address
   of the stack word that points to the previous frame.  */
/*define DYNAMIC_CHAIN_ADDRESS (FRAMEADDR)*/

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current frame.
   FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME'
   is defined.  */
/* The current return address is in r14.  */
#if 0 /* The default value should work.  */
#define RETURN_ADDR_RTX(COUNT, FRAME) \
(((COUNT) == -1)							\
 ? gen_rtx_REG (Pmode, 14)						\
 : copy_to_reg (gen_rtx_MEM (Pmode,					\
			     memory_address (Pmode,			\
					     plus_constant ((FRAME),	\
							    UNITS_PER_WORD)))))
#endif

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 13

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 16

/* The register number of the return address pointer register, which
   is used to access the current function's return address from the
   stack.  On some machines, the return address is not at a fixed
   offset from the frame pointer or stack pointer or argument
   pointer.  This register can be defined to point to the return
   address on the stack, and then be converted by `ELIMINABLE_REGS'
   into either the frame pointer or stack pointer.

   Do not define this macro unless there is no other way to get the
   return address from the stack.  */
/* ??? revisit */
/* #define RETURN_ADDRESS_POINTER_REGNUM */

/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM 7

/* These aren't official macros.  */
#define PROLOGUE_TMP_REGNUM 4
#define RETURN_ADDR_REGNUM 14
/* #define GP_REGNUM 12 */
#define CARRY_REGNUM 17
#define ACCUM_REGNUM 18
#define M32R_MAX_INT_REGS 16

#ifndef SUBTARGET_GPR_P
#define SUBTARGET_GPR_P(REGNO) 0
#endif

#ifndef SUBTARGET_ACCUM_P
#define SUBTARGET_ACCUM_P(REGNO) 0
#endif

#ifndef SUBTARGET_CARRY_P
#define SUBTARGET_CARRY_P(REGNO) 0
#endif

#define GPR_P(REGNO)   (IN_RANGE_P ((REGNO), 0, 15) || SUBTARGET_GPR_P (REGNO))
#define ACCUM_P(REGNO) ((REGNO) == ACCUM_REGNUM || SUBTARGET_ACCUM_P (REGNO))
#define CARRY_P(REGNO) ((REGNO) == CARRY_REGNUM || SUBTARGET_CARRY_P (REGNO))

/* Eliminating the frame and arg pointers.  */

/* A C expression which is nonzero if a function must have and use a
   frame pointer.  This expression is evaluated in the reload pass.
   If its value is nonzero the function will have a frame pointer.  */
#define FRAME_POINTER_REQUIRED current_function_calls_alloca

#if 0
/* C statement to store the difference between the frame pointer
   and the stack pointer values immediately after the function prologue.
   If `ELIMINABLE_REGS' is defined, this macro will be not be used and
   need not be defined.  */
#define INITIAL_FRAME_POINTER_OFFSET(VAR) \
((VAR) = m32r_compute_frame_size (get_frame_size ()))
#endif

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS					\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM }}

/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if `ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
  ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM		\
   ? ! frame_pointer_needed						\
   : 1)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
{									\
  int size = m32r_compute_frame_size (get_frame_size ());		\
									\
 if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
   (OFFSET) = 0;							\
 else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)	\
   (OFFSET) = size - current_function_pretend_args_size;		\
 else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
   (OFFSET) = size - current_function_pretend_args_size;		\
  else									\
    abort ();								\
}

/* Function argument passing.  */

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES 1

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define this macro if functions should assume that stack space has
   been allocated for arguments even when their values are passed in
   registers.

   The value of this macro is the size, in bytes, of the area
   reserved for arguments passed in registers for the function
   represented by FNDECL.

   This space can be allocated by the caller, or be a part of the
   machine-dependent stack frame: `OUTGOING_REG_PARM_STACK_SPACE' says
   which.  */
#if 0
#define REG_PARM_STACK_SPACE(FNDECL) \
  (M32R_MAX_PARM_REGS * UNITS_PER_WORD)
#endif

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */
#define RETURN_POPS_ARGS(DECL, FUNTYPE, SIZE) 0

/* Nonzero if we do not know how to pass TYPE solely in registers. */
#define MUST_PASS_IN_STACK(MODE, TYPE)			\
  ((TYPE) != 0						\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
       || TREE_ADDRESSABLE (TYPE)))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  ((CUM) = 0)

/* The number of registers used for parameter passing.  Local to this file.  */
#define M32R_MAX_PARM_REGS 4

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) \
  ((unsigned) (N) < M32R_MAX_PARM_REGS)

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE) \
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE) \
  ((MODE) == BLKmode				\
   ? ROUND_ADVANCE ((unsigned int) int_size_in_bytes (TYPE))	\
   : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

/* Round CUM up to the necessary point for argument MODE/TYPE.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) (CUM)

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   This macro is only used in this file.  */
#define PASS_IN_REG_P(CUM, MODE, TYPE, NAMED) \
  (ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)) < M32R_MAX_PARM_REGS)

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */
/* On the M32R the first M32R_MAX_PARM_REGS args are normally in registers
   and the rest are pushed.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  (PASS_IN_REG_P ((CUM), (MODE), (TYPE), (NAMED))			\
   ? gen_rtx_REG ((MODE), ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)))	\
   : 0)

/* ??? Quick hack to try to get varargs working the normal way.  */
#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  (((! current_function_varargs || (NAMED))				\
    && PASS_IN_REG_P ((CUM), (MODE), (TYPE), (NAMED)))			\
   ? gen_rtx_REG ((MODE), ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)))	\
   : 0)

/* A C expression for the number of words, at the beginning of an
   argument, must be put in registers.  The value must be zero for
   arguments that are passed entirely in registers or that are entirely
   pushed on the stack.

   On some machines, certain arguments must be passed partially in
   registers and partially in memory.  On these machines, typically the
   first @var{n} words of arguments are passed in registers, and the rest
   on the stack.  If a multi-word argument (a @code{double} or a
   structure) crosses that boundary, its first few words must be passed
   in registers and the rest must be pushed.  This macro tells the
   compiler when this occurs, and how many of the words should go in
   registers.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  function_arg_partial_nregs (&CUM, (int)MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */
/* All arguments greater than 8 bytes are passed this way.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  ((TYPE) && int_size_in_bytes (TYPE) > 8)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
  ((CUM) = (ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)) \
	  + ROUND_ADVANCE_ARG ((MODE), (TYPE))))

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined, 
   PARM_BOUNDARY is used for all arguments.  */
#if 0
/* We assume PARM_BOUNDARY == UNITS_PER_WORD here.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
(((TYPE) ? TYPE_ALIGN (TYPE) : GET_MODE_BITSIZE (MODE)) <= PARM_BOUNDARY \
 ? PARM_BOUNDARY \
 : 2 * PARM_BOUNDARY)
#endif

/* This macro offers an alternative
   to using `__builtin_saveregs' and defining the macro
   `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have
   been passed consecutively on the stack.  Once this is done, you
   can use the standard implementation of varargs that works for
   machines that pass all their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure,
   containing the values that obtain after processing of the named
   arguments.  The arguments MODE and TYPE describe the last named
   argument--its machine mode and its data type as a tree node.

   The macro implementation should do two things: first, push onto the
   stack all the argument registers *not* used for the named
   arguments, and second, store the size of the data thus pushed into
   the `int'-valued variable whose name is supplied as the argument
   PRETEND_SIZE.  The value that you store here will serve as
   additional offset for setting up the stack frame.

   If the argument NO_RTL is nonzero, it means that the
   arguments of the function are being analyzed for the second time.
   This happens for an inline function, which is not actually
   compiled until the end of the source file.  The macro
   `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */

#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_SIZE, NO_RTL) \
 m32r_setup_incoming_varargs (&ARGS_SO_FAR, MODE, TYPE, &PRETEND_SIZE, NO_RTL)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  m32r_va_arg (valist, type)

/* Function results.  */

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) gen_rtx_REG (TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, 0)

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */
/* ??? What about r1 in DI/DF values.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value says
   to return the function value in memory, just as large structures are
   always returned.  Here TYPE will be a C expression of type `tree',
   representing the data type of the value.  */
#define RETURN_IN_MEMORY(TYPE) \
(int_size_in_bytes (TYPE) > 8)

/* Tell GCC to use RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Register in which address to store a structure value
   is passed to a function, or 0 to use `invisible' first argument.  */
#define STRUCT_VALUE 0

/* Function entry and exit.  */

/* Initialize data used by insn expanders.  This is called from
   init_emit, once for each function, before code is generated.  */
#define INIT_EXPANDERS m32r_init_expanders ()

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 1

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
#define FUNCTION_PROFILER(FILE, LABELNO) abort ()

/* Trampolines.  */

/* On the M32R, the trampoline is

	ld24 r7,STATIC
	ld24 r6,FUNCTION
	jmp r6
	nop

   ??? Need addr32 support.
*/

/* Length in bytes of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE 12

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
do { \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 0)), \
		  plus_constant ((CXT), 0xe7000000)); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 4)), \
		  plus_constant ((FNADDR), 0xe6000000)); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 8)), \
		  GEN_INT (0x1fc67000)); \
  emit_insn (gen_flush_icache (validize_mem (gen_rtx_MEM (SImode, TRAMP)))); \
} while (0)

/* Library calls.  */

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* We have post-inc load and pre-dec,pre-inc store,
   but only for 4 byte vals.  */
#define HAVE_PRE_DECREMENT 1
#define HAVE_PRE_INCREMENT 1
#define HAVE_POST_INCREMENT 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) \
(GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
 || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST)

/* Nonzero if the constant value X is a legitimate general operand.
   We don't allow (plus symbol large-constant) as the relocations can't
   describe it.  INTVAL > 32767 handles both 16 bit and 24 bit relocations.
   We allow all CONST_DOUBLE's as the md file patterns will force the
   constant to memory if they can't handle them.  */

#define LEGITIMATE_CONSTANT_P(X)					\
(! (GET_CODE (X) == CONST						\
    && GET_CODE (XEXP (X, 0)) == PLUS					\
    && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF			\
    && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT			\
    && (unsigned HOST_WIDE_INT) INTVAL (XEXP (XEXP (X, 0), 1)) > 32767))

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

#ifdef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) GPR_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

#else

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)		\
(GPR_P (REGNO (X))			\
 || (REGNO (X)) == ARG_POINTER_REGNUM	\
 || REGNO (X) >= FIRST_PSEUDO_REGISTER)
/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  */

/* Local to this file.  */
#define RTX_OK_FOR_BASE_P(X) (REG_P (X) && REG_OK_FOR_BASE_P (X))

/* Local to this file.  */
#define RTX_OK_FOR_OFFSET_P(X) \
(GET_CODE (X) == CONST_INT && INT16_P (INTVAL (X)))

/* Local to this file.  */
#define LEGITIMATE_OFFSET_ADDRESS_P(MODE, X)				\
(GET_CODE (X) == PLUS							\
 && RTX_OK_FOR_BASE_P (XEXP (X, 0))					\
 && RTX_OK_FOR_OFFSET_P (XEXP (X, 1)))

/* Local to this file.  */
/* For LO_SUM addresses, do not allow them if the MODE is > 1 word,
   since more than one instruction will be required.  */
#define LEGITIMATE_LO_SUM_ADDRESS_P(MODE, X)				\
(GET_CODE (X) == LO_SUM							\
 && (MODE != BLKmode && GET_MODE_SIZE (MODE) <= UNITS_PER_WORD)		\
 && RTX_OK_FOR_BASE_P (XEXP (X, 0))					\
 && CONSTANT_P (XEXP (X, 1)))

/* Local to this file.  */
/* Is this a load and increment operation.  */
#define LOAD_POSTINC_P(MODE, X)						\
(((MODE) == SImode || (MODE) == SFmode)					\
 && GET_CODE (X) == POST_INC						\
 && GET_CODE (XEXP (X, 0)) == REG					\
 && RTX_OK_FOR_BASE_P (XEXP (X, 0)))

/* Local to this file.  */
/* Is this an increment/decrement and store operation.  */
#define STORE_PREINC_PREDEC_P(MODE, X)					\
(((MODE) == SImode || (MODE) == SFmode)					\
 && (GET_CODE (X) == PRE_INC || GET_CODE (X) == PRE_DEC)		\
 && GET_CODE (XEXP (X, 0)) == REG					\
 && RTX_OK_FOR_BASE_P (XEXP (X, 0)))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{ if (RTX_OK_FOR_BASE_P (X))						\
    goto ADDR;								\
  if (LEGITIMATE_OFFSET_ADDRESS_P ((MODE), (X)))			\
    goto ADDR;								\
  if (LEGITIMATE_LO_SUM_ADDRESS_P ((MODE), (X)))			\
    goto ADDR;								\
  if (LOAD_POSTINC_P ((MODE), (X)))					\
    goto ADDR;								\
  if (STORE_PREINC_PREDEC_P ((MODE), (X)))				\
    goto ADDR;								\
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

   ??? Is there anything useful we can do here for the M32R?  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)			\
do {									\
  if (GET_CODE (ADDR) == PRE_DEC					\
      || GET_CODE (ADDR) == PRE_INC					\
      || GET_CODE (ADDR) == POST_INC					\
      || GET_CODE (ADDR) == LO_SUM)					\
    goto LABEL;								\
} while (0)

/* Condition code usage.  */

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
#define SELECT_CC_MODE(OP, X, Y) \
((enum machine_mode)m32r_select_cc_mode ((int)OP, X, Y))

/* Return non-zero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */
#define REVERSIBLE_CC_MODE(MODE) 1 /*???*/

/* Costs.  */

/* ??? I'm quite sure I don't understand enough of the subtleties involved
   in choosing the right numbers to use here, but there doesn't seem to be
   enough documentation on this.  What I've done is define an insn to cost
   4 "units" and work from there.  COSTS_N_INSNS (N) is defined as (N) * 4 - 2
   so that seems reasonable.  Some values are supposed to be defined relative
   to each other and thus aren't necessarily related to COSTS_N_INSNS.  */

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */
/* Small integers are as cheap as registers.  4 byte values can be fetched
   as immediate constants - let's give that the cost of an extra insn.  */
#define CONST_COSTS(X, CODE, OUTER_CODE)			\
  case CONST_INT :						\
    if (INT16_P (INTVAL (X)))					\
      return 0;							\
    /* fall through */						\
  case CONST :							\
  case LABEL_REF :						\
  case SYMBOL_REF :						\
    return 4;							\
  case CONST_DOUBLE :						\
    {								\
      rtx high, low;						\
      split_double (X, &high, &low);				\
      return 4 * (!INT16_P (INTVAL (high))			\
		  + !INT16_P (INTVAL (low)));			\
    }

/* Compute the cost of an address.  */
#define ADDRESS_COST(ADDR) m32r_address_cost (ADDR)

/* Compute extra cost of moving data between one register class
   and another.  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2) 2

/* Compute the cost of moving data between registers and memory.  */
/* Memory is 3 times as expensive as registers.
   ??? Is that the right way to look at it?  */
#define MEMORY_MOVE_COST(MODE,CLASS,IN_P) \
(GET_MODE_SIZE (MODE) <= UNITS_PER_WORD ? 6 : 12)

/* The cost of a branch insn.  */
/* A value of 2 here causes GCC to avoid using branches in comparisons like
   while (a < N && a).  Branches aren't that expensive on the M32R so
   we define this as 1.  Defining it as 2 had a heavy hit in fp-bit.c.  */
#define BRANCH_COST ((TARGET_BRANCH_COST) ? 2 : 1)

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  The purpose for the cost of MULT is to encourage
   `synth_mult' to find a synthetic multiply when reasonable.

   If we need more than 12 insns to do a multiply, then go out-of-line,
   since the call overhead will be < 10% of the cost of the multiply.  */
#define RTX_COSTS(X, CODE, OUTER_CODE)	\
  case MULT :				\
    return COSTS_N_INSNS (3);		\
  case DIV :				\
  case UDIV :				\
  case MOD :				\
  case UMOD :				\
    return COSTS_N_INSNS (10);

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE

/* Define this macro if it is as good or better for a function to call
   itself with an explicit address than to call an address kept in a
   register.  */
#define NO_RECURSIVE_FUNCTION_CSE

/* When the `length' insn attribute is used, this macro specifies the
   value to be assigned to the address of the first insn in a
   function.  If not specified, 0 is used.  */
#define FIRST_INSN_ADDRESS m32r_first_insn_address ()


/* Section selection.  */

#define TEXT_SECTION_ASM_OP	"\t.section .text"
#define DATA_SECTION_ASM_OP	"\t.section .data"
#define RODATA_SECTION_ASM_OP	"\t.section .rodata"
#define BSS_SECTION_ASM_OP	"\t.section .bss"
#define SDATA_SECTION_ASM_OP	"\t.section .sdata"
#define SBSS_SECTION_ASM_OP	"\t.section .sbss"
/* This one is for svr4.h.  */
#undef  CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP	"\t.section .rodata"

/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro
   on a system with no other sections (that GCC needs to use).  */
#undef  EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_sbss, in_const

/* One or more functions to be defined in "varasm.c".  These
   functions should do jobs analogous to those of `text_section' and
   `data_section', for your additional sections.  Do not define this
   macro if you do not define `EXTRA_SECTIONS'.  */
#undef  EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS	\
  CONST_SECTION_FUNCTION	\
  SDATA_SECTION_FUNCTION	\
  SBSS_SECTION_FUNCTION

#define SDATA_SECTION_FUNCTION						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\

#define SBSS_SECTION_FUNCTION						\
void									\
sbss_section ()								\
{									\
  if (in_section != in_sbss)						\
    {									\
      fprintf (asm_out_file, "%s\n", SBSS_SECTION_ASM_OP);		\
      in_section = in_sbss;						\
    }									\
}									\

/* A C statement or statements to switch to the appropriate section for
   output of EXP.  You can assume that EXP is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether the initial value
   of EXP requires link-time relocations.  */
#undef  SELECT_SECTION
#define SELECT_SECTION(EXP, RELOC, ALIGN) \
  m32r_select_section ((EXP), (RELOC))

/* A C statement or statements to switch to the appropriate section for
   output of RTX in mode MODE.  You can assume that RTX
   is some kind of constant in RTL.  The argument MODE is redundant
   except in the case of a `const_int' rtx.  Select the section by
   calling `text_section' or one of the alternatives for other
   sections.

   Do not define this macro if you put all constants in the read-only
   data section.  */

#undef SELECT_RTX_SECTION

/* Define this macro if jump tables (for tablejump insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.
   This macro is irrelevant if there is no separate readonly data section.  */
/*#define JUMP_TABLES_IN_TEXT_SECTION*/

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to store a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).  */

#define SDATA_FLAG_CHAR '@'
/* Small objects are recorded with no prefix for space efficiency since
   they'll be the most common.  This isn't the case if the user passes
   -mmodel={medium|large} and one could choose to not mark symbols that
   are the default, but that complicates things.  */
/*#define SMALL_FLAG_CHAR '#'*/
#define MEDIUM_FLAG_CHAR '%'
#define LARGE_FLAG_CHAR '&'

#define SDATA_NAME_P(NAME) (*(NAME) == SDATA_FLAG_CHAR)
/*#define SMALL_NAME_P(NAME) (*(NAME) == SMALL_FLAG_CHAR)*/
#define SMALL_NAME_P(NAME) (! ENCODED_NAME_P (NAME) && ! LIT_NAME_P (NAME))
#define MEDIUM_NAME_P(NAME) (*(NAME) == MEDIUM_FLAG_CHAR)
#define LARGE_NAME_P(NAME) (*(NAME) == LARGE_FLAG_CHAR)
/* For string literals, etc.  */
#define LIT_NAME_P(NAME) ((NAME)[0] == '*' && (NAME)[1] == '.')

#define ENCODED_NAME_P(SYMBOL_NAME) \
(SDATA_NAME_P (SYMBOL_NAME) \
 /*|| SMALL_NAME_P (SYMBOL_NAME)*/ \
 || MEDIUM_NAME_P (SYMBOL_NAME) \
 || LARGE_NAME_P (SYMBOL_NAME))

#define ENCODE_SECTION_INFO(DECL) m32r_encode_section_info (DECL)

/* Decode SYM_NAME and store the real name part in VAR, sans
   the characters that encode section info.  Define this macro if
   ENCODE_SECTION_INFO alters the symbol's name string.  */
/* Note that we have to handle symbols like "%*start".  */
#define STRIP_NAME_ENCODING(VAR, SYMBOL_NAME) \
do {							\
  (VAR) = (SYMBOL_NAME) + ENCODED_NAME_P (SYMBOL_NAME);	\
  (VAR) += *(VAR) == '*';				\
} while (0)

/* PIC */

/* The register number of the register used to address a table of static
   data addresses in memory.  In some cases this register is defined by a
   processor's ``application binary interface'' (ABI).  When this macro
   is defined, RTL is generated for this register once, as with the stack
   pointer and frame pointer registers.  If this macro is not defined, it
   is up to the machine-dependent files to allocate such a register (if
   necessary).  */
/*#define PIC_OFFSET_TABLE_REGNUM 12*/

/* Define this macro if the register defined by PIC_OFFSET_TABLE_REGNUM is
   clobbered by calls.  Do not define this macro if PIC_OFFSET_TABLE_REGNUM
   is not defined.  */
/* This register is call-saved on the M32R.  */
/*#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED*/

/* By generating position-independent code, when two different programs (A
   and B) share a common library (libC.a), the text of the library can be
   shared whether or not the library is linked at the same address for both
   programs.  In some of these environments, position-independent code
   requires not only the use of different addressing modes, but also
   special code to enable the use of these addressing modes.

   The FINALIZE_PIC macro serves as a hook to emit these special
   codes once the function is being compiled into assembly code, but not
   before.  (It is not done before, because in the case of compiling an
   inline function, it would lead to multiple PIC prologues being
   included in functions which used inline functions and were compiled to
   assembly language.)  */

/*#define FINALIZE_PIC m32r_finalize_pic ()*/

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent code.
   You can assume that X satisfies CONSTANT_P, so you need not
   check this.  You can also assume `flag_pic' is true, so you need not
   check it either.  You need not define this macro if all constants
   (including SYMBOL_REF) can be immediate operands when generating
   position independent code.  */
/*#define LEGITIMATE_PIC_OPERAND_P(X)*/

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */
#define ASM_FILE_START(FILE) m32r_asm_file_start (FILE)

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */
#define ASM_COMMENT_START ";"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF ""

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
/* On the M32R we need to ensure the next instruction starts on a 32 bit
   boundary [the previous insn must either be 2 16 bit insns or 1 32 bit].  */
#define ASM_OUTPUT_LABEL(FILE, NAME)	\
  do					\
    {					\
      assemble_name (FILE, NAME);	\
      fputs (":\n", FILE);		\
    }					\
  while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */
#define ASM_GLOBALIZE_LABEL(FILE, NAME)	\
  do					\
    {					\
      fputs ("\t.global\t", FILE);	\
      assemble_name (FILE, NAME);	\
      fputs ("\n", FILE);		\
    }					\
  while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME) 	\
  do						\
    {						\
      const char * real_name;			\
      STRIP_NAME_ENCODING (real_name, (NAME));	\
      asm_fprintf (FILE, "%U%s", real_name);	\
    }						\
  while (0)           

/* If -Os, don't force line number labels to begin at the beginning of
   the word; we still want the assembler to try to put things in parallel,
   should that be possible.
   For m32r/d, instructions are never in parallel (other than with a nop)
   and the simulator and stub both handle a breakpoint in the middle of
   a word so don't ever force line number labels to begin at the beginning
   of a word.  */

#undef	ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)				\
  do									\
    {									\
      static int sym_lineno = 1;					\
      fprintf (file, ".stabn 68,0,%d,.LM%d-",				\
	       line, sym_lineno);					\
      assemble_name							\
	(file, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));	\
      fprintf (file, (optimize_size || TARGET_M32R)			\
	       ? "\n\t.debugsym .LM%d\n"				\
	       : "\n.LM%d:\n",						\
	       sym_lineno);						\
      sym_lineno += 1;							\
    }									\
  while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
  do							\
    {							\
      (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10);\
      sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO));	\
    }							\
  while (0)

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */
#ifndef SUBTARGET_REGISTER_NAMES
#define SUBTARGET_REGISTER_NAMES
#endif

#define REGISTER_NAMES					\
{							\
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",	\
  "r8", "r9", "r10", "r11", "r12", "fp", "lr", "sp",	\
  "ap", "cbit", "a0"					\
  SUBTARGET_REGISTER_NAMES				\
}

/* If defined, a C initializer for an array of structures containing
   a name and a register number.  This macro defines additional names
   for hard registers, thus allowing the `asm' option in declarations
   to refer to registers using alternate names.  */
#ifndef SUBTARGET_ADDITIONAL_REGISTER_NAMES
#define SUBTARGET_ADDITIONAL_REGISTER_NAMES
#endif

#define ADDITIONAL_REGISTER_NAMES	\
{					\
  /*{ "gp", GP_REGNUM },*/		\
  { "r13", FRAME_POINTER_REGNUM },	\
  { "r14", RETURN_ADDR_REGNUM },	\
  { "r15", STACK_POINTER_REGNUM },	\
  SUBTARGET_ADDITIONAL_REGISTER_NAMES	\
}

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  */
extern char m32r_punct_chars[256];
#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  m32r_punct_chars[(unsigned char) (CHAR)]

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
#define PRINT_OPERAND(FILE, X, CODE) \
  m32r_print_operand (FILE, X, CODE)

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
  m32r_print_operand_address (FILE, ADDR)

/* If defined, C string expressions to be used for the `%R', `%L',
   `%U', and `%I' options of `asm_fprintf' (see `final.c').  These
   are useful when a single `md' file must support multiple assembler
   formats.  In that case, the various `tm.h' files can define these
   macros differently.  */
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX ".L"
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX "#"

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)		\
   do							\
     {							\
       char label[30];					\
       ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
       fprintf (FILE, "\t.word\t");			\
       assemble_name (FILE, label);			\
       fprintf (FILE, "\n");				\
     }							\
  while (0)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)\
  do							\
    {							\
      char label[30];					\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
      fprintf (FILE, "\t.word\t");			\
      assemble_name (FILE, label);			\
      fprintf (FILE, "-");				\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", REL);	\
      assemble_name (FILE, label);			\
      fprintf (FILE, ")\n");				\
    }							\
  while (0)

/* The desired alignment for the location counter at the beginning
   of a loop.  */
/* On the M32R, align loops to 32 byte boundaries (cache line size)
   if -malign-loops.  */
#define LOOP_ALIGN(LABEL) (TARGET_ALIGN_LOOPS ? 5 : 0)

/* Define this to be the maximum number of insns to move around when moving
   a loop test from the top of a loop to the bottom
   and seeing whether to duplicate it.  The default is thirty.

   Loop unrolling currently doesn't like this optimization, so
   disable doing if we are unrolling loops and saving space.  */
#define LOOP_TEST_THRESHOLD (optimize_size				\
			     && !flag_unroll_loops			\
			     && !flag_unroll_all_loops ? 2 : 30)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
/* .balign is used to avoid confusion.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)			\
  do							\
    {							\
      if ((LOG) != 0)					\
	fprintf (FILE, "\t.balign %d\n", 1 << (LOG));	\
    }							\
  while (0)

/* Like `ASM_OUTPUT_COMMON' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used in
   place of `ASM_OUTPUT_COMMON', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.  */

#define SCOMMON_ASM_OP "\t.scomm\t"

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      if (! TARGET_SDATA_NONE						\
	  && (SIZE) > 0 && (SIZE) <= g_switch_value)			\
	fprintf ((FILE), "%s", SCOMMON_ASM_OP);				\
      else								\
	fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
    }									\
  while (0)

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used in
   place of `ASM_OUTPUT_BSS', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.

   For the M32R we need sbss support.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
  do								\
    {								\
      ASM_GLOBALIZE_LABEL (FILE, NAME);				\
      ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);	\
    }								\
  while (0)

/* Debugging information.  */

/* Generate DBX and DWARF debugging information.  */
#undef	DBX_DEBUGGING_INFO
#undef	DWARF_DEBUGGING_INFO
#undef	DWARF2_DEBUGGING_INFO

#define DBX_DEBUGGING_INFO
#define DWARF_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO

/* Prefer STABS (for now).  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Turn off splitting of long stabs.  */
#define DBX_CONTIN_LENGTH 0

/* Miscellaneous.  */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* It's not clear what PIC will look like or whether we want to use -fpic
   for the embedded form currently being talked about.  For now require -fpic
   to get pc relative switch tables.  */
/*#define CASE_VECTOR_PC_RELATIVE 1 */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
/* ??? The M32R doesn't have full 32 bit pointers, but making this PSImode has
   it's own problems (you have to add extendpsisi2 and truncsipsi2).
   Try to avoid it.  */
#define Pmode SImode

/* A function address in a call instruction.  */
#define FUNCTION_MODE SImode

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
extern struct rtx_def * m32r_compare_op0;
extern struct rtx_def * m32r_compare_op1;

/* M32R function types.   */
enum m32r_function_type
{
  M32R_FUNCTION_UNKNOWN, M32R_FUNCTION_NORMAL, M32R_FUNCTION_INTERRUPT
};

#define M32R_INTERRUPT_P(TYPE) ((TYPE) == M32R_FUNCTION_INTERRUPT)

/* Define this if you have defined special-purpose predicates in the
   file `MACHINE.c'.  This macro is called within an initializer of an
   array of structures.  The first field in the structure is the name
   of a predicate and the second field is an array of rtl codes.  For
   each predicate, list all rtl codes that can be in expressions
   matched by the predicate.  The list should have a trailing comma.  */

#define PREDICATE_CODES							\
{ "reg_or_zero_operand",        { REG, SUBREG, CONST_INT }},            \
{ "conditional_move_operand",	{ REG, SUBREG, CONST_INT }},		\
{ "carry_compare_operand",	{ EQ, NE }},				\
{ "eqne_comparison_operator",	{ EQ, NE }},				\
{ "signed_comparison_operator", { EQ, NE, LT, LE, GT, GE }},		\
{ "move_dest_operand",		{ REG, SUBREG, MEM }},			\
{ "move_src_operand",		{ REG, SUBREG, MEM, CONST_INT,		\
				  CONST_DOUBLE, LABEL_REF, CONST,	\
				  SYMBOL_REF }},			\
{ "move_double_src_operand",	{ REG, SUBREG, MEM, CONST_INT,		\
				  CONST_DOUBLE }},			\
{ "two_insn_const_operand",	{ CONST_INT }},				\
{ "symbolic_operand",		{ SYMBOL_REF, LABEL_REF, CONST }},	\
{ "seth_add3_operand",		{ SYMBOL_REF, LABEL_REF, CONST }},	\
{ "int8_operand",		{ CONST_INT }},				\
{ "uint16_operand",		{ CONST_INT }},				\
{ "reg_or_int16_operand",	{ REG, SUBREG, CONST_INT }},		\
{ "reg_or_uint16_operand",	{ REG, SUBREG, CONST_INT }},		\
{ "reg_or_cmp_int16_operand",	{ REG, SUBREG, CONST_INT }},		\
{ "reg_or_eq_int16_operand",	{ REG, SUBREG, CONST_INT }},		\
{ "cmp_int16_operand",		{ CONST_INT }},				\
{ "call_address_operand",	{ SYMBOL_REF, LABEL_REF, CONST }},	\
{ "extend_operand",		{ REG, SUBREG, MEM }},			\
{ "small_insn_p",		{ INSN, CALL_INSN, JUMP_INSN }},	\
{ "m32r_block_immediate_operand",{ CONST_INT }},			\
{ "large_insn_p",		{ INSN, CALL_INSN, JUMP_INSN }},	\
{ "seth_add3_operand",		{ SYMBOL_REF, LABEL_REF, CONST }},

