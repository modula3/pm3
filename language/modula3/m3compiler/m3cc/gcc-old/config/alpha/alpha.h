/* Definitions of target machine for GNU compiler, for DEC Alpha.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "\
-Dunix -D__osf__ -D__alpha -D__alpha__ -D_LONGLONG -DSYSTYPE_BSD  \
-D_SYSTYPE_BSD -Asystem(unix) -Asystem(xpg4) -Acpu(alpha) -Amachine(alpha)"

/* Write out the correct language type definition for the header files.  
   Unless we have assembler language, write out the symbols for C.  */
#define CPP_SPEC "\
%{!.S:	-D__LANGUAGE_C__ -D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}  \
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C__ -D__LANGUAGE_OBJECTIVE_C} \
%{mieee:-D_IEEE_FP} \
%{mieee-with-inexact:-D_IEEE_FP -D_IEEE_FP_INEXACT}"

/* Set the spec to use for signed char.  The default tests the above macro
   but DEC's compiler can't handle the conditional in a "constant"
   operand.  */

#define SIGNED_CHAR_SPEC "%{funsigned-char:-D__CHAR_UNSIGNED__}"

/* Under OSF/1, -p and -pg require -lprof1.  */

#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} %{a:-lprof2} -lc"

/* Pass "-G 8" to ld because Alpha's CC does.  Pass -O3 if we are
   optimizing, -O1 if we are not.  Pass -shared, -non_shared or
   -call_shared as appropriate.  Also pass -pg.  */
#define LINK_SPEC  \
  "-G 8 %{O*:-O3} %{!O*:-O1} %{static:-non_shared} \
   %{!static:%{shared:-shared} %{!shared:-call_shared}} %{pg} %{taso} \
   %{rpath*}"

#define WORD_SWITCH_TAKES_ARG(STR)		\
 (!strcmp (STR, "rpath") || !strcmp (STR, "include")	\
  || !strcmp (STR, "imacros") || !strcmp (STR, "aux-info") \
  || !strcmp (STR, "idirafter") || !strcmp (STR, "iprefix") \
  || !strcmp (STR, "iwithprefix") || !strcmp (STR, "iwithprefixbefore") \
  || !strcmp (STR, "isystem"))

#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION

/* Define the location for the startup file on OSF/1 for Alpha.  */

#define MD_STARTFILE_PREFIX "/usr/lib/cmplrs/cc/"

/* Run-time compilation parameters selecting different hardware subsets.  */

/* Which processor to schedule for. The cpu attribute defines a list that
   mirrors this list, so changes to alpha.md must be made at the same time.  */

enum processor_type
 {PROCESSOR_EV4,			/* 2106[46]{a,} */
  PROCESSOR_EV5};			/* 21164{a,} */

extern enum processor_type alpha_cpu;

enum alpha_trap_precision
{
  ALPHA_TP_PROG,	/* No precision (default).  */
  ALPHA_TP_FUNC,      	/* Trap contained within originating function.  */
  ALPHA_TP_INSN		/* Instruction accuracy and code is resumption safe. */
};

enum alpha_fp_rounding_mode
{
  ALPHA_FPRM_NORM,	/* Normal rounding mode.  */
  ALPHA_FPRM_MINF,	/* Round towards minus-infinity.  */
  ALPHA_FPRM_CHOP,	/* Chopped rounding mode (towards 0). */
  ALPHA_FPRM_DYN	/* Dynamic rounding mode.  */
};

enum alpha_fp_trap_mode
{
  ALPHA_FPTM_N,		/* Normal trap mode. */
  ALPHA_FPTM_U,		/* Underflow traps enabled.  */
  ALPHA_FPTM_SU,	/* Software completion, w/underflow traps */
  ALPHA_FPTM_SUI	/* Software completion, w/underflow & inexact traps */
};

extern int target_flags;

extern enum alpha_trap_precision alpha_tp;
extern enum alpha_fp_rounding_mode alpha_fprm;
extern enum alpha_fp_trap_mode alpha_fptm;

/* This means that floating-point support exists in the target implementation
   of the Alpha architecture.  This is usually the default.  */

#define MASK_FP		1
#define TARGET_FP	(target_flags & MASK_FP)

/* This means that floating-point registers are allowed to be used.  Note
   that Alpha implementations without FP operations are required to
   provide the FP registers.  */

#define MASK_FPREGS	2
#define TARGET_FPREGS	(target_flags & MASK_FPREGS)

/* This means that gas is used to process the assembler file.  */

#define MASK_GAS 4
#define TARGET_GAS	(target_flags & MASK_GAS)

/* This means that we should mark procedures as IEEE conformant. */

#define MASK_IEEE_CONFORMANT 8
#define TARGET_IEEE_CONFORMANT	(target_flags & MASK_IEEE_CONFORMANT)

/* This means we should be IEEE-compliant except for inexact.  */

#define MASK_IEEE	16
#define TARGET_IEEE	(target_flags & MASK_IEEE)

/* This means we should be fully IEEE-compliant.  */

#define MASK_IEEE_WITH_INEXACT 32
#define TARGET_IEEE_WITH_INEXACT (target_flags & MASK_IEEE_WITH_INEXACT)

/* This means we are compiling for Windows NT.  */

#define MASK_WINDOWS_NT	64
#define TARGET_WINDOWS_NT (target_flags & MASK_WINDOWS_NT)

/* This means we must construct all constants rather than emitting
   them as literal data.  */

#define MASK_BUILD_CONSTANTS 128
#define TARGET_BUILD_CONSTANTS (target_flags & MASK_BUILD_CONSTANTS)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES				\
  { {"no-soft-float", MASK_FP},			\
    {"soft-float", - MASK_FP},			\
    {"fp-regs", MASK_FPREGS},			\
    {"no-fp-regs", - (MASK_FP|MASK_FPREGS)},	\
    {"alpha-as", -MASK_GAS},			\
    {"gas", MASK_GAS},				\
    {"ieee-conformant", MASK_IEEE_CONFORMANT},	\
    {"ieee", MASK_IEEE|MASK_IEEE_CONFORMANT},	\
    {"ieee-with-inexact", MASK_IEEE_WITH_INEXACT|MASK_IEEE_CONFORMANT}, \
    {"build-constants", MASK_BUILD_CONSTANTS},  \
    {"", TARGET_DEFAULT | TARGET_CPU_DEFAULT} }

#define TARGET_DEFAULT MASK_FP|MASK_FPREGS

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an initializer
   with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the fixed
   part of the option name, and the address of a variable.  The
   variable, type `char *', is set to the variable part of the given
   option if the fixed part matches.  The actual option name is made
   by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

	extern char *m88k_short_data;
	#define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }  */

extern char *alpha_cpu_string;  /* For -mcpu=ev[4|5] */
extern char *alpha_fprm_string;	/* For -mfp-rounding-mode=[n|m|c|d] */
extern char *alpha_fptm_string;	/* For -mfp-trap-mode=[n|u|su|sui]  */
extern char *alpha_tp_string;	/* For -mtrap-precision=[p|f|i] */

#define TARGET_OPTIONS				\
{						\
  {"cpu=",		&alpha_cpu_string},	\
  {"fp-rounding-mode=",	&alpha_fprm_string},	\
  {"fp-trap-mode=",	&alpha_fptm_string},	\
  {"trap-precision=",	&alpha_tp_string},	\
}

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   On the Alpha, it is used to translate target-option strings into
   numeric values.  */

extern void override_options ();
#define OVERRIDE_OPTIONS override_options ()


/* Define this macro to change register usage conditional on target flags.

   On the Alpha, we use this to disable the floating-point registers when
   they don't exist.  */

#define CONDITIONAL_REGISTER_USAGE	\
  if (! TARGET_FPREGS)			\
    for (i = 32; i < 63; i++)		\
      fixed_regs[i] = call_used_regs[i] = 1;

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* target machine storage layout */

/* Define to enable software floating point emulation. */
#define REAL_ARITHMETIC

/* Define the size of `int'.  The default is the same as the word size.  */
#define INT_TYPE_SIZE 32

/* Define the size of `long long'.  The default is the twice the word size.  */
#define LONG_LONG_TYPE_SIZE 64

/* The two floating-point formats we support are S-floating, which is
   4 bytes, and T-floating, which is 8 bytes.  `float' is S and `double'
   and `long double' are T.  */

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define	WCHAR_TYPE "unsigned int"
#define	WCHAR_TYPE_SIZE 32

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   For Alpha, we always store objects in a full register.  32-bit objects
   are always sign-extended, but smaller objects retain their signedness.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
    {						\
      if ((MODE) == SImode)			\
	(UNSIGNEDP) = 0;			\
      (MODE) = DImode;				\
    }

/* Define this if function arguments should also be promoted using the above
   procedure.  */

#define PROMOTE_FUNCTION_ARGS

/* Likewise, if the function return value is promoted.  */

#define PROMOTE_FUNCTION_RETURN

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.

   There are no such instructions on the Alpha, but the documentation
   is little endian.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.
   This is false on the Alpha.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.

   For Alpha we can decide arbitrarily since there are no machine instructions
   for them.  Might as well be consistent with bytes. */
#define WORDS_BIG_ENDIAN 0

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 8

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 64

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 64

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 64

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Align loop starts for optimal branching.  

   ??? Kludge this and the next macro for the moment by not doing anything if
   we don't optimize and also if we are writing ECOFF symbols to work around
   a bug in DEC's assembler. */

#define ASM_OUTPUT_LOOP_ALIGN(FILE) \
  if (optimize > 0 && write_symbols != SDB_DEBUG)  \
    ASM_OUTPUT_ALIGN (FILE, 5)

/* This is how to align an instruction for optimal branching.
   On Alpha we'll get better performance by aligning on a quadword
   boundary.  */

#define ASM_OUTPUT_ALIGN_CODE(FILE)	\
  if (optimize > 0 && write_symbols != SDB_DEBUG) \
    ASM_OUTPUT_ALIGN ((FILE), 4)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Align all constants and variables to at least a word boundary so
   we can pick up pieces of them faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN) MAX ((ALIGN), BITS_PER_WORD)
#define DATA_ALIGNMENT(EXP, ALIGN) MAX ((ALIGN), BITS_PER_WORD)

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.

   Since we get an error message when we do one, call them invalid.  */

#define STRICT_ALIGNMENT 1

/* Set this non-zero if unaligned move instructions are extremely slow.

   On the Alpha, they trap.  */

#define SLOW_UNALIGNED_ACCESS 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   We define all 32 integer registers, even though $31 is always zero,
   and all 32 floating-point registers, even though $f31 is also
   always zero.  We do not bother defining the FP status register and
   there are no other registers. 

   Since $31 is always zero, we will use register number 31 as the
   argument pointer.  It will never appear in the generated code
   because we will always be eliminating it in favor of the stack
   pointer or hardware frame pointer.

   Likewise, we use $f31 for the frame pointer, which will always
   be eliminated in favor of the hardware frame pointer or the
   stack pointer.  */

#define FIRST_PSEUDO_REGISTER 64

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, \
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
   $f1			(nonsaved floating-point register)
   $f10-$f15		(likewise)
   $f22-$f30		(likewise)
   $f21-$f16		(likewise, but input args)
   $f0			(nonsaved, but return value)
   $f2-$f9		(saved floating-point registers)
   $1-$8		(nonsaved integer registers)
   $22-$25		(likewise)
   $28			(likewise)
   $0			(likewise, but return value)
   $21-$16		(likewise, but input args)
   $27			(procedure value in OSF, nonsaved in NT)
   $9-$14		(saved integer registers)
   $26			(return PC)
   $15			(frame pointer)
   $29			(global pointer)
   $30, $31, $f31	(stack pointer and always zero/ap & fp)  */

#define REG_ALLOC_ORDER		\
  {33,					\
   42, 43, 44, 45, 46, 47,		\
   54, 55, 56, 57, 58, 59, 60, 61, 62,	\
   53, 52, 51, 50, 49, 48,		\
   32,					\
   34, 35, 36, 37, 38, 39, 40, 41,	\
   1, 2, 3, 4, 5, 6, 7, 8,		\
   22, 23, 24, 25,			\
   28,					\
   0,					\
   21, 20, 19, 18, 17, 16,		\
   27,					\
   9, 10, 11, 12, 13, 14,		\
   26,					\
   15,					\
   29,					\
   30, 31, 63 }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On Alpha, the integer registers can hold any mode.  The floating-point
   registers can hold 32-bit and 64-bit integers as well, but not 16-bit
   or 8-bit values.  If we only allowed the larger integers into FP registers,
   we'd have to say that QImode and SImode aren't tiable, which is a
   pain.  So say all registers can hold everything and see how that works.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Alpha pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 30

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM 15

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 31

/* Base register for access to local variables of function.  */
#define FRAME_POINTER_REGNUM 63

/* Register in which static-chain is passed to a function. 

   For the Alpha, this is based on an example; the calling sequence
   doesn't seem to specify this.  */
#define STATIC_CHAIN_REGNUM 1

/* Register in which address to store a structure value
   arrives in the function.  On the Alpha, the address is passed
   as a hidden argument.  */
#define STRUCT_VALUE 0

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
   
enum reg_class { NO_REGS, GENERAL_REGS, FLOAT_REGS, ALL_REGS,
		 LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES				\
 {"NO_REGS", "GENERAL_REGS", "FLOAT_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS	\
  { {0, 0}, {~0, 0x80000000}, {0, 0x7fffffff}, {~0, ~0} }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
 ((REGNO) >= 32 && (REGNO) <= 62 ? FLOAT_REGS : GENERAL_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)	\
 ((C) == 'f' ? FLOAT_REGS : NO_REGS)

/* Define this macro to change register usage conditional on target flags.  */
/* #define CONDITIONAL_REGISTER_USAGE  */

/* The letters I, J, K, L, M, N, O, and P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For Alpha:
   `I' is used for the range of constants most insns can contain.
   `J' is the constant zero.
   `K' is used for the constant in an LDA insn.
   `L' is used for the constant in a LDAH insn.
   `M' is used for the constants that can be AND'ed with using a ZAP insn.
   `N' is used for complemented 8-bit constants.
   `O' is used for negated 8-bit constants.
   `P' is used for the constants 1, 2 and 3.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'I' ? (unsigned HOST_WIDE_INT) (VALUE) < 0x100	\
   : (C) == 'J' ? (VALUE) == 0					\
   : (C) == 'K' ? (unsigned HOST_WIDE_INT) ((VALUE) + 0x8000) < 0x10000	\
   : (C) == 'L' ? (((VALUE) & 0xffff) == 0			\
		   && (((VALUE)) >> 31 == -1 || (VALUE) >> 31 == 0) \
		   && ((HOST_BITS_PER_WIDE_INT == 64		\
			|| (unsigned) (VALUE) != 0x80000000U)))	\
   : (C) == 'M' ? zap_mask (VALUE)				\
   : (C) == 'N' ? (unsigned HOST_WIDE_INT) (~ (VALUE)) < 0x100	\
   : (C) == 'O' ? (unsigned HOST_WIDE_INT) (- (VALUE)) < 0x100	\
   : (C) == 'P' ? (VALUE) == 1 || (VALUE) == 2 || (VALUE) == 3	\
   : 0)

/* Similar, but for floating or large integer constants, and defining letters
   G and H.   Here VALUE is the CONST_DOUBLE rtx itself.

   For Alpha, `G' is the floating-point constant zero.  `H' is a CONST_DOUBLE
   that is the operand of a ZAP insn.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  			\
  ((C) == 'G' ? (GET_MODE_CLASS (GET_MODE (VALUE)) == MODE_FLOAT	\
		 && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))		\
   : (C) == 'H' ? (GET_MODE (VALUE) == VOIDmode				\
		   && zap_mask (CONST_DOUBLE_LOW (VALUE))		\
		   && zap_mask (CONST_DOUBLE_HIGH (VALUE)))		\
   : 0)

/* Optional extra constraints for this machine.

   For the Alpha, `Q' means that this is a memory operand but not a
   reference to an unaligned location.
   `R' is a SYMBOL_REF that has SYMBOL_REF_FLAG set or is the current
   function.  */

#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q' ? GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) != AND \
   : (C) == 'R' ? current_file_function_operand (OP, Pmode)	\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   On the Alpha, all constants except zero go into a floating-point
   register via memory.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS)		\
  (CONSTANT_P (X) && (X) != const0_rtx && (X) != CONST0_RTX (GET_MODE (X)) \
   ? ((CLASS) == FLOAT_REGS ? NO_REGS : GENERAL_REGS)			\
   : (CLASS))

/* Loading and storing HImode or QImode values to and from memory
   usually requires a scratch register.  The exceptions are loading
   QImode and HImode from an aligned address to a general register. 
   We also cannot load an unaligned address or a paradoxical SUBREG into an
   FP register.   */

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS,MODE,IN)			\
(((GET_CODE (IN) == MEM 						\
   || (GET_CODE (IN) == REG && REGNO (IN) >= FIRST_PSEUDO_REGISTER)	\
   || (GET_CODE (IN) == SUBREG						\
       && (GET_CODE (SUBREG_REG (IN)) == MEM				\
	   || (GET_CODE (SUBREG_REG (IN)) == REG			\
	       && REGNO (SUBREG_REG (IN)) >= FIRST_PSEUDO_REGISTER))))	\
  && (((CLASS) == FLOAT_REGS						\
       && ((MODE) == SImode || (MODE) == HImode || (MODE) == QImode))	\
      || (((MODE) == QImode || (MODE) == HImode)			\
	  && unaligned_memory_operand (IN, MODE))))			\
 ? GENERAL_REGS								\
 : ((CLASS) == FLOAT_REGS && GET_CODE (IN) == MEM			\
    && GET_CODE (XEXP (IN, 0)) == AND) ? GENERAL_REGS			\
 : ((CLASS) == FLOAT_REGS && GET_CODE (IN) == SUBREG			\
    && (GET_MODE_SIZE (GET_MODE (IN))					\
	> GET_MODE_SIZE (GET_MODE (SUBREG_REG (IN))))) ? GENERAL_REGS	\
 : NO_REGS)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS,MODE,OUT)			\
(((GET_CODE (OUT) == MEM 						\
   || (GET_CODE (OUT) == REG && REGNO (OUT) >= FIRST_PSEUDO_REGISTER)	\
   || (GET_CODE (OUT) == SUBREG						\
       && (GET_CODE (SUBREG_REG (OUT)) == MEM				\
	   || (GET_CODE (SUBREG_REG (OUT)) == REG			\
	       && REGNO (SUBREG_REG (OUT)) >= FIRST_PSEUDO_REGISTER)))) \
  && (((MODE) == HImode || (MODE) == QImode				\
       || ((MODE) == SImode && (CLASS) == FLOAT_REGS))))		\
 ? GENERAL_REGS								\
 : ((CLASS) == FLOAT_REGS && GET_CODE (OUT) == MEM			\
    && GET_CODE (XEXP (OUT, 0)) == AND) ? GENERAL_REGS			\
 : ((CLASS) == FLOAT_REGS && GET_CODE (OUT) == SUBREG			\
    && (GET_MODE_SIZE (GET_MODE (OUT))					\
	> GET_MODE_SIZE (GET_MODE (SUBREG_REG (OUT))))) ? GENERAL_REGS	\
 : NO_REGS)

/* If we are copying between general and FP registers, we need a memory
   location.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1,CLASS2,MODE) ((CLASS1) != (CLASS2))

/* Specify the mode to be used for memory when a secondary memory
   location is needed.  If MODE is floating-point, use it.  Otherwise,
   widen to a word like the default.  This is needed because we always
   store integers in FP registers in quadword format.  This whole
   area is very tricky! */
#define SECONDARY_MEMORY_NEEDED_MODE(MODE)		\
  (GET_MODE_CLASS (MODE) == MODE_FLOAT ? (MODE)		\
   : GET_MODE_SIZE (MODE) >= 4 ? (MODE)			\
   : mode_for_size (BITS_PER_WORD, GET_MODE_CLASS (MODE), 0))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)				\
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the size of the object.  */

#define CLASS_CANNOT_CHANGE_SIZE	FLOAT_REGS

/* Define the cost of moving between registers of various classes.  Moving
   between FLOAT_REGS and anything else except float regs is expensive. 
   In fact, we make it quite expensive because we really don't want to
   do these moves unless it is clearly worth it.  Optimizations may
   reduce the impact of not being able to allocate a pseudo to a
   hard register.  */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)	\
  (((CLASS1) == FLOAT_REGS) == ((CLASS2) == FLOAT_REGS) ? 2 : 20)

/* A C expressions returning the cost of moving data of MODE from a register to
   or from memory.

   On the Alpha, bump this up a bit.  */

#define MEMORY_MOVE_COST(MODE)  6

/* Provide the cost of a branch.  Exact meaning under development.  */
#define BRANCH_COST 5

/* Adjust the cost of dependencies.  */

#define ADJUST_COST(INSN,LINK,DEP,COST) \
  (COST) = alpha_adjust_cost (INSN, LINK, DEP, COST)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */

#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On Alpha, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS

/* Offset of first parameter from the argument pointer register value.  */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the Alpha.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer. */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

#define ELIMINABLE_REGS				     \
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	     \
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},   \
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	     \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   All eliminations are valid since the cases where FP can't be
   eliminated are already handled.  */

#define CAN_ELIMINATE(FROM, TO) 1

/* Round up to a multiple of 16 bytes.  */
#define ALPHA_ROUND(X) (((X) + 15) & ~ 15)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{ if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = (ALPHA_ROUND (current_function_outgoing_args_size)	\
		+ alpha_sa_size ());					\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
    (OFFSET) = (ALPHA_ROUND (current_function_outgoing_args_size)	\
		+ alpha_sa_size ()					\
		+ (ALPHA_ROUND (get_frame_size ()			\
			       + current_function_pretend_args_size)	\
		   - current_function_pretend_args_size));		\
}

/* Define this if stack space is still allocated for a parameter passed
   in a register.  */
/* #define REG_PARM_STACK_SPACE */

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.

   On Alpha the value is found in $0 for integer functions and
   $f0 for floating-point functions.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)	\
  gen_rtx (REG,						\
	   (INTEGRAL_MODE_P (TYPE_MODE (VALTYPE))	\
	    && TYPE_PRECISION (VALTYPE) < BITS_PER_WORD) \
	   ? word_mode : TYPE_MODE (VALTYPE),		\
	   TARGET_FPREGS && TREE_CODE (VALTYPE) == REAL_TYPE ? 32 : 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)	\
   gen_rtx (REG, MODE,		\
	    TARGET_FPREGS && GET_MODE_CLASS (MODE) == MODE_FLOAT ? 32 : 0)

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.

   For the Alpha, any structure or union type is returned in memory, as
   are integers whose size is larger than 64 bits.  */

#define RETURN_IN_MEMORY(TYPE) \
  (TYPE_MODE (TYPE) == BLKmode \
   || (TREE_CODE (TYPE) == INTEGER_TYPE && TYPE_PRECISION (TYPE) > 64))

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N) == 32)

/* 1 if N is a possible register number for function argument passing.
   On Alpha, these are $16-$21 and $f16-$f21.  */

#define FUNCTION_ARG_REGNO_P(N) \
  (((N) >= 16 && (N) <= 21) || ((N) >= 16 + 32 && (N) <= 21 + 32))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On Alpha, this is a single integer, which is a number of words
   of arguments scanned so far.
   Thus 6 or more means all following args should go on the stack.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)  (CUM) = 0

/* Define intermediate macro to compute the size (in registers) of an argument
   for the Alpha.  */

#define ALPHA_ARG_SIZE(MODE, TYPE, NAMED)				\
((MODE) != BLKmode							\
 ? (GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD 	\
 : (int_size_in_bytes (TYPE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  if (MUST_PASS_IN_STACK (MODE, TYPE))					\
    (CUM) = 6;								\
  else									\
    (CUM) += ALPHA_ARG_SIZE (MODE, TYPE, NAMED)

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
    (otherwise it is an extra parameter matching an ellipsis).

   On Alpha the first 6 words of args are normally in registers
   and the rest are pushed.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)	\
((CUM) < 6 && ! MUST_PASS_IN_STACK (MODE, TYPE)	\
 ? gen_rtx(REG, (MODE),				\
	   (CUM) + 16 + ((TARGET_FPREGS		\
			  && (GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT  \
			      || GET_MODE_CLASS (MODE) == MODE_FLOAT)) \
			 * 32))			\
 : 0)

/* Specify the padding direction of arguments.

   On the Alpha, we must pad upwards in order to be able to pass args in
   registers.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE)	upward

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)	\
((CUM) < 6 && 6 < (CUM) + ALPHA_ARG_SIZE (MODE, TYPE, NAMED)	\
 ? 6 - (CUM) : 0)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed. 

   On the Alpha, we allocate space for all 12 arg registers, but only
   push those that are remaining.

   However, if NO registers need to be saved, don't allocate any space.
   This is not only because we won't need the space, but because AP includes
   the current_pretend_args_size and we don't want to mess up any
   ap-relative addresses already made.

   If we are not to use the floating-point registers, save the integer
   registers where we would put the floating-point registers.  This is
   not the most efficient way to implement varargs with just one register
   class, but it isn't worth doing anything more efficient in this rare
   case.  */
   

#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if ((CUM) < 6)							\
    {									\
      if (! (NO_RTL))							\
	{								\
	  move_block_from_reg						\
	    (16 + CUM,							\
	     gen_rtx (MEM, BLKmode,					\
		      plus_constant (virtual_incoming_args_rtx,		\
				     ((CUM) + 6)* UNITS_PER_WORD)),	\
	     6 - (CUM), (6 - (CUM)) * UNITS_PER_WORD);			\
	  move_block_from_reg						\
	    (16 + (TARGET_FPREGS ? 32 : 0) + CUM,			\
	     gen_rtx (MEM, BLKmode,					\
		      plus_constant (virtual_incoming_args_rtx,		\
				     (CUM) * UNITS_PER_WORD)),		\
	     6 - (CUM), (6 - (CUM)) * UNITS_PER_WORD);			\
	 }								\
      PRETEND_SIZE = 12 * UNITS_PER_WORD;				\
    }									\
}

/* Try to output insns to set TARGET equal to the constant C if it can be
   done in less than N insns.  Do all computations in MODE.  Returns the place
   where the output has been placed if it can be done and the insns have been
   emitted.  If it would take more than N insns, zero is returned and no
   insns and emitted.  */
extern struct rtx_def *alpha_emit_set_const ();
extern struct rtx_def *alpha_emit_set_long_const ();
extern struct rtx_def *alpha_emit_conditional_move ();

/* Generate necessary RTL for __builtin_saveregs().
   ARGLIST is the argument list; see expr.c.  */
extern struct rtx_def *alpha_builtin_saveregs ();
#define EXPAND_BUILTIN_SAVEREGS(ARGLIST) alpha_builtin_saveregs (ARGLIST)

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *alpha_compare_op0, *alpha_compare_op1;
extern int alpha_compare_fp_p;

/* This macro produces the initial definition of a function name.  On the
   Alpha, we need to save the function name for the prologue and epilogue.  */

extern char *alpha_function_name;

#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL)	\
{							\
   alpha_function_name = NAME;				\
}
   
/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)  output_prolog (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  Under OSF/1, profiling is enabled
   by simply passing -pg to the assembler and linker.  */

#define FUNCTION_PROFILER(FILE, LABELNO)

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.
   This assumes that __bb_init_func doesn't garble a1-a5. */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)			\
    do {							\
	ASM_OUTPUT_REG_PUSH (FILE, 16);				\
	fputs ("\tlda $16,$PBX32\n", (FILE));			\
	fputs ("\tldq $26,0($16)\n", (FILE));			\
	fputs ("\tbne $26,1f\n", (FILE));			\
	fputs ("\tlda $27,__bb_init_func\n", (FILE));		\
	fputs ("\tjsr $26,($27),__bb_init_func\n", (FILE));	\
	fputs ("\tldgp $29,0($26)\n", (FILE));			\
	fputs ("1:\n", (FILE));					\
	ASM_OUTPUT_REG_POP (FILE, 16);				\
    } while (0);

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)				\
    do {							\
	int blockn = (BLOCKNO);					\
	fputs ("\tsubq $30,16,$30\n", (FILE));			\
	fputs ("\tstq $26,0($30)\n", (FILE));			\
	fputs ("\tstq $27,8($30)\n", (FILE));			\
	fputs ("\tlda $26,$PBX34\n", (FILE));			\
	fprintf ((FILE), "\tldq $27,%d($26)\n", 8*blockn);	\
	fputs ("\taddq $27,1,$27\n", (FILE));			\
	fprintf ((FILE), "\tstq $27,%d($26)\n", 8*blockn);	\
	fputs ("\tldq $26,0($30)\n", (FILE));			\
	fputs ("\tldq $27,8($30)\n", (FILE));			\
	fputs ("\taddq $30,16,$30\n", (FILE));			\
    } while (0)


/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE)	output_epilog (FILE, SIZE)


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  
   Note that $27 has been set to the address of the trampoline, so we can
   use it for addressability of the two data items.  Trampolines are always
   aligned to FUNCTION_BOUNDARY, which is 64 bits.  */

#define TRAMPOLINE_TEMPLATE(FILE)		\
{						\
  fprintf (FILE, "\tldq $1,24($27)\n");		\
  fprintf (FILE, "\tldq $27,16($27)\n");	\
  fprintf (FILE, "\tjmp $31,($27),0\n");	\
  fprintf (FILE, "\tnop\n");			\
  fprintf (FILE, "\t.quad 0,0\n");		\
}

/* Section in which to place the trampoline.  On Alpha, instructions
   may only be placed in a text segment.  */

#define TRAMPOLINE_SECTION text_section

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE    32

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  We assume
   here that a function will be called many more times than its address
   is taken (e.g., it might be passed to qsort), so we take the trouble 
   to initialize the "hint" field in the JMP insn.  Note that the hint
   field is PC (new) + 4 * bits 13:0.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  rtx _temp, _temp1, _addr;						\
									\
  _addr = memory_address (Pmode, plus_constant ((TRAMP), 16));		\
  emit_move_insn (gen_rtx (MEM, Pmode, _addr), (FNADDR));		\
  _addr = memory_address (Pmode, plus_constant ((TRAMP), 24));		\
  emit_move_insn (gen_rtx (MEM, Pmode, _addr), (CXT));			\
									\
  _temp = force_operand (plus_constant ((TRAMP), 12), NULL_RTX);	\
  _temp = expand_binop (DImode, sub_optab, (FNADDR), _temp, _temp, 1,	\
			OPTAB_WIDEN);					\
  _temp = expand_shift (RSHIFT_EXPR, Pmode, _temp,			\
			build_int_2 (2, 0), NULL_RTX, 1);		\
  _temp = expand_and (gen_lowpart (SImode, _temp),			\
		      GEN_INT (0x3fff), 0); 				\
									\
  _addr = memory_address (SImode, plus_constant ((TRAMP), 8));		\
  _temp1 = force_reg (SImode, gen_rtx (MEM, SImode, _addr));		\
  _temp1 = expand_and (_temp1, GEN_INT (0xffffc000), NULL_RTX);		\
  _temp1 = expand_binop (SImode, ior_optab, _temp1, _temp, _temp1, 1,	\
			 OPTAB_WIDEN);					\
									\
  emit_move_insn (gen_rtx (MEM, SImode, _addr), _temp1);		\
									\
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode,			\
			      "__enable_execute_stack"),		\
		     0, VOIDmode, 1,_addr, Pmode);			\
									\
  emit_insn (gen_rtx (UNSPEC_VOLATILE, VOIDmode,			\
		      gen_rtvec (1, const0_rtx), 0));			\
}

/* Attempt to turn on access permissions for the stack.  */

#define TRANSFER_FROM_TRAMPOLINE					\
									\
void									\
__enable_execute_stack (addr)						\
     void *addr;							\
{									\
  long size = getpagesize ();						\
  long mask = ~(size-1);						\
  char *page = (char *) (((long) addr) & mask);				\
  char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size); \
									\
  /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */				\
  if (mprotect (page, end - page, 7) < 0)				\
    perror ("mprotect of trampoline code");				\
}

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is the frame pointer of the COUNT frame, or the frame pointer of
   the COUNT-1 frame if RETURN_ADDR_IN_PREVIOUS_FRAME} is defined.

   This definition for Alpha is broken, but is put in at the request of
   Mike Stump.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)					\
((COUNT == 0 && alpha_sa_size () == 0 && 0 /* not right. */)		\
 ? gen_rtx (REG, Pmode, 26)						\
 : gen_rtx (MEM, Pmode,							\
	    memory_address (Pmode, FRAME)))

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32  \
 || (REGNO) == 63 || reg_renumber[REGNO] == 63)

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  For the Alpha,
   there are only constants none since we want to use LDA to load any
   symbolic addresses into registers.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == CONST_INT	\
   && (unsigned HOST_WIDE_INT) (INTVAL (X) + 0x8000) < 0x10000)

/* Include all constant integers and constant doubles, but not
   floating-point, except for floating-point zero.  */

#define LEGITIMATE_CONSTANT_P(X)  		\
  (GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT	\
   || (X) == CONST0_RTX (GET_MODE (X)))

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

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) 0
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)  \
  (REGNO (X) < 32 || REGNO (X) == 63 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address. 

   For Alpha, we have either a constant address or the sum of a register
   and a constant address, or just a register.  For DImode, any of those
   forms can be surrounded with an AND that clear the low-order three bits;
   this is an "unaligned" access.

   First define the basic valid address.  */

#define GO_IF_LEGITIMATE_SIMPLE_ADDRESS(MODE, X, ADDR) \
{ if (REG_P (X) && REG_OK_FOR_BASE_P (X))	\
    goto ADDR;					\
  if (CONSTANT_ADDRESS_P (X))			\
    goto ADDR;					\
  if (GET_CODE (X) == PLUS			\
      && REG_P (XEXP (X, 0))			\
      && REG_OK_FOR_BASE_P (XEXP (X, 0))	\
      && CONSTANT_ADDRESS_P (XEXP (X, 1)))	\
    goto ADDR;					\
}

/* Now accept the simple address, or, for DImode only, an AND of a simple
   address that turns off the low three bits.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) \
{ GO_IF_LEGITIMATE_SIMPLE_ADDRESS (MODE, X, ADDR); \
  if ((MODE) == DImode				\
      && GET_CODE (X) == AND			\
      && GET_CODE (XEXP (X, 1)) == CONST_INT	\
      && INTVAL (XEXP (X, 1)) == -8)		\
    GO_IF_LEGITIMATE_SIMPLE_ADDRESS (MODE, XEXP (X, 0), ADDR); \
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

   For the Alpha, there are three cases we handle:

   (1) If the address is (plus reg const_int) and the CONST_INT is not a
       valid offset, compute the high part of the constant and add it to the
       register.  Then our address is (plus temp low-part-const).
   (2) If the address is (const (plus FOO const_int)), find the low-order
       part of the CONST_INT.  Then load FOO plus any high-order part of the
       CONST_INT into a register.  Our address is (plus reg low-part-const).
       This is done to reduce the number of GOT entries.
   (3) If we have a (plus reg const), emit the load as in (2), then add
       the two registers, and finally generate (plus reg low-part-const) as
       our address.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{ if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == REG	\
      && GET_CODE (XEXP (X, 1)) == CONST_INT			\
      && ! CONSTANT_ADDRESS_P (XEXP (X, 1)))			\
    {								\
      HOST_WIDE_INT val = INTVAL (XEXP (X, 1));			\
      HOST_WIDE_INT lowpart = (val & 0xffff) - 2 * (val & 0x8000); \
      HOST_WIDE_INT highpart = val - lowpart;			\
      rtx high = GEN_INT (highpart);				\
      rtx temp = expand_binop (Pmode, add_optab, XEXP (x, 0),	\
			       high, NULL_RTX, 1, OPTAB_LIB_WIDEN); \
								\
      (X) = plus_constant (temp, lowpart);			\
      goto WIN;							\
    }								\
  else if (GET_CODE (X) == CONST				\
	   && GET_CODE (XEXP (X, 0)) == PLUS			\
	   && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT)	\
    {								\
      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (X, 0), 1));	\
      HOST_WIDE_INT lowpart = (val & 0xffff) - 2 * (val & 0x8000); \
      HOST_WIDE_INT highpart = val - lowpart;			\
      rtx high = XEXP (XEXP (X, 0), 0);				\
								\
      if (highpart)						\
	high = plus_constant (high, highpart);			\
								\
      (X) = plus_constant (force_reg (Pmode, high), lowpart);	\
      goto WIN;							\
    }								\
  else if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == REG \
	   && GET_CODE (XEXP (X, 1)) == CONST			\
	   && GET_CODE (XEXP (XEXP (X, 1), 0)) == PLUS		\
	   && GET_CODE (XEXP (XEXP (XEXP (X, 1), 0), 1)) == CONST_INT) \
    {								\
      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (XEXP (X, 1), 0), 1)); \
      HOST_WIDE_INT lowpart = (val & 0xffff) - 2 * (val & 0x8000); \
      HOST_WIDE_INT highpart = val - lowpart;			\
      rtx high = XEXP (XEXP (XEXP (X, 1), 0), 0);		\
								\
      if (highpart)						\
	high = plus_constant (high, highpart);			\
								\
      high = expand_binop (Pmode, add_optab, XEXP (X, 0),	\
			   force_reg (Pmode, high),		\
			   high, 1, OPTAB_LIB_WIDEN);		\
      (X) = plus_constant (high, lowpart);			\
      goto WIN;							\
    }								\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the Alpha this is true only for the unaligned modes.   We can
   simplify this test since we know that the address must be valid.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)  \
{ if (GET_CODE (ADDR) == AND) goto LABEL; }

/* Compute the cost of an address.  For the Alpha, all valid addresses are
   the same cost.  */

#define ADDRESS_COST(X)  0

/* Define this if some processing needs to be done immediately before
   emitting code for an insn.  */

extern void final_prescan_insn ();
#define FINAL_PRESCAN_INSN(INSN,OPERANDS,NOPERANDS) \
  final_prescan_insn ((INSN), (OPERANDS), (NOPERANDS))

/* Define this if FINAL_PRESCAN_INSN should be called for a CODE_LABEL.  */
#define FINAL_PRESCAN_LABEL

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.
   On the Alpha, the table is really GP-relative, not relative to the PC
   of the table, but we pretend that it is PC-relative; this should be OK,
   but we should try to find some better way sometime.  */
#define CASE_VECTOR_PC_RELATIVE

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.

   We actually lie a bit here as overflow conditions are different.  But
   they aren't being checked anyway.  */

#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move to or from memory
   in one reasonably fast instruction.  */

#define MOVE_MAX 8

/* Largest number of bytes of an object that can be placed in a register.
   On the Alpha we have plenty of registers, so use TImode.  */
#define MAX_FIXED_MODE_SIZE	GET_MODE_BITSIZE (TImode)

/* Nonzero if access to memory by bytes is no faster than for words.
   Also non-zero if doing byte operations (specifically shifts) in registers
   is undesirable. 

   On the Alpha, we want to not use the byte operation and instead use
   masking operations to access fields; these will save instructions.  */

#define SLOW_BYTE_ACCESS	1

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

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Define the value returned by a floating-point comparison instruction.  */

#define FLOAT_STORE_FLAG_VALUE 0.5

/* Canonicalize a comparison from one we don't have to one we do have.  */

#define CANONICALIZE_COMPARISON(CODE,OP0,OP1) \
  do {									\
    if (((CODE) == GE || (CODE) == GT || (CODE) == GEU || (CODE) == GTU) \
	&& (GET_CODE (OP1) == REG || (OP1) == const0_rtx))		\
      {									\
	rtx tem = (OP0);						\
	(OP0) = (OP1);							\
	(OP1) = tem;							\
	(CODE) = swap_condition (CODE);					\
      }									\
    if (((CODE) == LT || (CODE) == LTU)					\
	&& GET_CODE (OP1) == CONST_INT && INTVAL (OP1) == 256)		\
      {									\
	(CODE) = (CODE) == LT ? LE : LEU;				\
	(OP1) = GEN_INT (255);						\
      }									\
  } while (0)

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode DImode

/* Mode of a function address in a call instruction (for indexing purposes). */

#define FUNCTION_MODE Pmode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.

   We define this on the Alpha so that gen_call and gen_call_value
   get to see the SYMBOL_REF (for the hint field of the jsr).  It will
   then copy it into a register, thus actually letting the address be
   cse'ed.  */

#define NO_FUNCTION_CSE

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.

   If this is an 8-bit constant, return zero since it can be used
   nearly anywhere with no cost.  If it is a valid operand for an
   ADD or AND, likewise return 0 if we know it will be used in that
   context.  Otherwise, return 2 since it might be used there later.
   All other constants take at least two insns.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) >= 0 && INTVAL (RTX) < 256)		\
      return 0;							\
  case CONST_DOUBLE:						\
    if (((OUTER_CODE) == PLUS && add_operand (RTX, VOIDmode))	\
	|| ((OUTER_CODE) == AND && and_operand (RTX, VOIDmode))) \
      return 0;							\
    else if (add_operand (RTX, VOIDmode) || and_operand (RTX, VOIDmode)) \
      return 2;							\
    else							\
      return COSTS_N_INSNS (2);					\
  case CONST:							\
  case SYMBOL_REF:						\
  case LABEL_REF:						\
  switch (alpha_cpu)						\
    {								\
    case PROCESSOR_EV4:						\
      return COSTS_N_INSNS (3);					\
    case PROCESSOR_EV5:						\
      return COSTS_N_INSNS (2);					\
    }
    
/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */
   
#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case PLUS:  case MINUS:				\
    if (FLOAT_MODE_P (GET_MODE (X)))			\
      switch (alpha_cpu)				\
        {						\
        case PROCESSOR_EV4:				\
          return COSTS_N_INSNS (6);			\
        case PROCESSOR_EV5:				\
          return COSTS_N_INSNS (4); 			\
	}						\
    else if (GET_CODE (XEXP (X, 0)) == MULT		\
	     && const48_operand (XEXP (XEXP (X, 0), 1), VOIDmode)) \
      return (2 + rtx_cost (XEXP (XEXP (X, 0), 0), OUTER_CODE)	\
	      + rtx_cost (XEXP (X, 1), OUTER_CODE));	\
    break;						\
  case MULT:						\
    switch (alpha_cpu)					\
      {							\
      case PROCESSOR_EV4:				\
        if (FLOAT_MODE_P (GET_MODE (X)))		\
          return COSTS_N_INSNS (6);			\
        return COSTS_N_INSNS (23);			\
      case PROCESSOR_EV5:				\
        if (FLOAT_MODE_P (GET_MODE (X)))		\
          return COSTS_N_INSNS (4);			\
        else if (GET_MODE (X) == DImode)		\
          return COSTS_N_INSNS (12);			\
        else						\
          return COSTS_N_INSNS (8);			\
      }							\
  case ASHIFT:						\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& INTVAL (XEXP (X, 1)) <= 3)			\
      break;						\
    /* ... fall through ... */				\
  case ASHIFTRT:  case LSHIFTRT:  case IF_THEN_ELSE:	\
    switch (alpha_cpu)					\
      {							\
      case PROCESSOR_EV4:				\
        return COSTS_N_INSNS (2);			\
      case PROCESSOR_EV5:				\
        return COSTS_N_INSNS (1); 			\
      }							\
  case DIV:  case UDIV:  case MOD:  case UMOD:		\
    switch (alpha_cpu)					\
      {							\
      case PROCESSOR_EV4:				\
        if (GET_MODE (X) == SFmode)			\
          return COSTS_N_INSNS (34);			\
        else if (GET_MODE (X) == DFmode)		\
          return COSTS_N_INSNS (63);			\
        else						\
          return COSTS_N_INSNS (70);			\
      case PROCESSOR_EV5:				\
        if (GET_MODE (X) == SFmode)			\
          return COSTS_N_INSNS (15);			\
        else if (GET_MODE (X) == DFmode)		\
          return COSTS_N_INSNS (22);			\
        else						\
          return COSTS_N_INSNS (70);	/* EV5 ??? */	\
      }							\
  case MEM:						\
    switch (alpha_cpu)					\
      {							\
      case PROCESSOR_EV4:				\
        return COSTS_N_INSNS (3);			\
      case PROCESSOR_EV5:				\
        return COSTS_N_INSNS (2); 			\
      }							\
  case NEG:  case ABS:					\
    if (! FLOAT_MODE_P (GET_MODE (X)))			\
      break;						\
    /* ... fall through ... */				\
  case FLOAT:  case UNSIGNED_FLOAT:  case FIX:  case UNSIGNED_FIX: \
  case FLOAT_EXTEND:  case FLOAT_TRUNCATE:		\
    switch (alpha_cpu)					\
      {							\
      case PROCESSOR_EV4:				\
        return COSTS_N_INSNS (6);			\
      case PROCESSOR_EV5:				\
        return COSTS_N_INSNS (4); 			\
      }

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");                                \
  fprintf (FILE, "\t.set noat\n");				\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

#define TEXT_SECTION_ASM_OP ".text"

/* Output before read-only data.  */

#define READONLY_DATA_SECTION_ASM_OP ".rdata"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Define an extra section for read-only data, a routine to enter it, and
   indicate that it is for read-only data.

   The first time we enter the readonly data section for a file, we write
   eight bytes of zero.  This works around a bug in DEC's assembler in
   some versions of OSF/1 V3.x.  */

#define EXTRA_SECTIONS	readonly_data

#define EXTRA_SECTION_FUNCTIONS					\
void								\
literal_section ()						\
{								\
  if (in_section != readonly_data)				\
    {								\
      static int firsttime = 1;				        \
								\
      fprintf (asm_out_file, "%s\n", READONLY_DATA_SECTION_ASM_OP); \
      if (firsttime)						\
	{							\
	  firsttime = 0;				        \
	  ASM_OUTPUT_DOUBLE_INT (asm_out_file, const0_rtx);	\
	}							\
								\
      in_section = readonly_data;				\
    }								\
}								\

#define READONLY_DATA_SECTION	literal_section

/* If we are referencing a function that is static, make the SYMBOL_REF
   special.  We use this to see indicate we can branch to this function
   without setting PV or restoring GP.  */

#define ENCODE_SECTION_INFO(DECL)  \
  if (TREE_CODE (DECL) == FUNCTION_DECL && ! TREE_PUBLIC (DECL)) \
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES						\
{"$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8",		\
 "$9", "$10", "$11", "$12", "$13", "$14", "$15",		\
 "$16", "$17", "$18", "$19", "$20", "$21", "$22", "$23",	\
 "$24", "$25", "$26", "$27", "$28", "$29", "$30", "AP",		\
 "$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7", "$f8",	\
 "$f9", "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",		\
 "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",\
 "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "FP"}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  if ((PREFIX)[0] == 'L')				\
    fprintf (FILE, "$%s%d:\n", & (PREFIX)[1], NUM + 32); \
  else							\
    fprintf (FILE, "%s%d:\n", PREFIX, NUM);

/* This is how to output a label for a jump table.  Arguments are the same as
   for ASM_OUTPUT_INTERNAL_LABEL, except the insn for the jump table is
   passed. */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 2); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  if ((PREFIX)[0] == 'L')				\
    sprintf (LABEL, "*$%s%d", & (PREFIX)[1], NUM + 32);	\
  else							\
    sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* Check a floating-point value for validity for a particular machine mode.  */

#define CHECK_FLOAT_VALUE(MODE, D, OVERFLOW) \
  ((OVERFLOW) = check_float_value (MODE, &D, OVERFLOW))

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t[2];							\
	REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);			\
	fprintf (FILE, "\t.quad 0x%lx%08lx\n",				\
		t[1] & 0xffffffff, t[0] & 0xffffffff);			\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", str);			\
	fprintf (FILE, "\t.t_floating %s\n", str);			\
      }									\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)				\
  do {								\
    long t;							\
    REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);			\
    fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffff);		\
} while (0)
  
/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  		\
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining a `long' constant.  */

#define ASM_OUTPUT_DOUBLE_INT(FILE,VALUE)	\
( fprintf (FILE, "\t.quad "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
  fprintf (FILE, "\t.word %d\n",		\
    (GET_CODE (VALUE) == CONST_INT		\
     ? INTVAL (VALUE) & 0xffff : (abort (), 0)))

#define ASM_OUTPUT_CHAR(FILE,VALUE)		\
  fprintf (FILE, "\t.byte %d\n",		\
    (GET_CODE (VALUE) == CONST_INT		\
     ? INTVAL (VALUE) & 0xff : (abort (), 0)))

/* We use the default ASCII-output routine, except that we don't write more
   than 50 characters since the assembler doesn't support very long lines.  */

#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    unsigned char *_hide_p = (unsigned char *) (MYSTRING);		      \
    int _hide_thissize = (MYLENGTH);					      \
    int _size_so_far = 0;						      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      unsigned char *p = _hide_p;					      \
      int thissize = _hide_thissize;					      \
      int i;								      \
      fprintf (asm_out_file, "\t.ascii \"");				      \
									      \
      for (i = 0; i < thissize; i++)					      \
	{								      \
	  register int c = p[i];					      \
									      \
	  if (_size_so_far ++ > 50 && i < thissize - 4)			      \
	    _size_so_far = 0, fprintf (asm_out_file, "\"\n\t.ascii \"");      \
									      \
	  if (c == '\"' || c == '\\')					      \
	    putc ('\\', asm_out_file);					      \
	  if (c >= ' ' && c < 0177)					      \
	    putc (c, asm_out_file);					      \
	  else								      \
	    {								      \
	      fprintf (asm_out_file, "\\%o", c);			      \
	      /* After an octal-escape, if a digit follows,		      \
		 terminate one string constant and start another.	      \
		 The Vax assembler fails to stop reading the escape	      \
		 after three digits, so this is the only way we		      \
		 can get it to parse the data properly.  */		      \
	      if (i < thissize - 1					      \
		  && p[i + 1] >= '0' && p[i + 1] <= '9')		      \
		_size_so_far = 0, fprintf (asm_out_file, "\"\n\t.ascii \"");  \
	  }								      \
	}								      \
      fprintf (asm_out_file, "\"\n");					      \
    }									      \
  }									      \
  while (0)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)					\
 fprintf (FILE, "\tsubq $30,8,$30\n\tst%s $%s%d,0($30)\n",		\
	  (REGNO) > 32 ? "t" : "q", (REGNO) > 32 ? "f" : "",		\
	  (REGNO) & 31);

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)					\
  fprintf (FILE, "\tld%s $%s%d,0($30)\n\taddq $30,8,$30\n",		\
	  (REGNO) > 32 ? "t" : "q", (REGNO) > 32 ? "f" : "",		\
	  (REGNO) & 31);

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE) & 0xff)

/* This is how to output an element of a case-vector that is absolute.
   (Alpha does not use such vectors, but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) abort ()

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t.%s $%d\n", TARGET_WINDOWS_NT ? "long" : "gprel32", \
	   (VALUE) + 32)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", LOG);

/* This is how to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE,ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

/* Determine which codes are valid without a following integer.  These must
   not be alphabetic (the characters are chosen so that
   PRINT_OPERAND_PUNCT_VALID_P translates into a simple range change when
   using ASCII).

   &	Generates fp-rounding mode suffix: nothing for normal, 'c' for
   	chopped, 'm' for minus-infinity, and 'd' for dynamic rounding
	mode.  alpha_fprm controls which suffix is generated.

   '	Generates trap-mode suffix for instructions that accept the
        su suffix only (cmpt et al).

   )    Generates trap-mode suffix for instructions that accept the
	u, su, and sui suffix.  This is the bulk of the IEEE floating
	point instructions (addt et al).

   +    Generates trap-mode suffix for instructions that accept the
	sui suffix (cvtqt and cvtqs).
   */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '&' || (CODE) == '\'' || (CODE) == ')' || (CODE) == '+')

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)		\
{ rtx addr = (ADDR);					\
  int basereg = 31;					\
  HOST_WIDE_INT offset = 0;				\
							\
  if (GET_CODE (addr) == AND)				\
    addr = XEXP (addr, 0);				\
							\
  if (GET_CODE (addr) == REG)				\
    basereg = REGNO (addr);				\
  else if (GET_CODE (addr) == CONST_INT)		\
    offset = INTVAL (addr);				\
  else if (GET_CODE (addr) == PLUS			\
	   && GET_CODE (XEXP (addr, 0)) == REG		\
	   && GET_CODE (XEXP (addr, 1)) == CONST_INT)	\
    basereg = REGNO (XEXP (addr, 0)), offset = INTVAL (XEXP (addr, 1)); \
  else							\
    abort ();						\
							\
  fprintf (FILE, "%d($%d)", offset, basereg);		\
}
/* Define the codes that are matched by predicates in alpha.c.  */

#define PREDICATE_CODES \
  {"reg_or_0_operand", {SUBREG, REG, CONST_INT}},	\
  {"reg_or_6bit_operand", {SUBREG, REG, CONST_INT}},	\
  {"reg_or_8bit_operand", {SUBREG, REG, CONST_INT}},	\
  {"cint8_operand", {CONST_INT}},                       \
  {"reg_or_cint_operand", {SUBREG, REG, CONST_INT}},	\
  {"add_operand", {SUBREG, REG, CONST_INT}},		\
  {"sext_add_operand", {SUBREG, REG, CONST_INT}},	\
  {"const48_operand", {CONST_INT}},			\
  {"and_operand", {SUBREG, REG, CONST_INT}},		\
  {"or_operand", {SUBREG, REG, CONST_INT}},		\
  {"mode_mask_operand", {CONST_INT}},			\
  {"mul8_operand", {CONST_INT}},			\
  {"mode_width_operand", {CONST_INT}},			\
  {"reg_or_fp0_operand", {SUBREG, REG, CONST_DOUBLE}},	\
  {"alpha_comparison_operator", {EQ, LE, LT, LEU, LTU}}, \
  {"signed_comparison_operator", {EQ, NE, LE, LT, GE, GT}}, \
  {"divmod_operator", {DIV, MOD, UDIV, UMOD}},		\
  {"fp0_operand", {CONST_DOUBLE}},			\
  {"current_file_function_operand", {SYMBOL_REF}},	\
  {"call_operand", {REG, SYMBOL_REF}},			\
  {"input_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE,	\
		     SYMBOL_REF, CONST, LABEL_REF}},	\
  {"some_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE, \
		    SYMBOL_REF, CONST, LABEL_REF}},	\
  {"aligned_memory_operand", {MEM}},			\
  {"unaligned_memory_operand", {MEM}},			\
  {"reg_or_unaligned_mem_operand", {SUBREG, REG, MEM}},	\
  {"any_memory_operand", {MEM}},

/* Tell collect that the object format is ECOFF.  */
#define OBJECT_FORMAT_COFF
#define EXTENDED_COFF

/* If we use NM, pass -g to it so it only lists globals.  */
#define NM_FLAGS "-pg"

/* Definitions for debugging.  */

#define SDB_DEBUGGING_INFO		/* generate info for mips-tfile */
#define DBX_DEBUGGING_INFO		/* generate embedded stabs */
#define MIPS_DEBUGGING_INFO		/* MIPS specific debugging info */

#ifndef PREFERRED_DEBUGGING_TYPE	/* assume SDB_DEBUGGING_INFO */
#define PREFERRED_DEBUGGING_TYPE  \
 ((len > 1 && !strncmp (str, "ggdb", len)) ? DBX_DEBUG : SDB_DEBUG)
#endif


/* Correct the offset of automatic variables and arguments.  Note that
   the Alpha debug format wants all automatic variables and arguments
   to be in terms of two different offsets from the virtual frame pointer,
   which is the stack pointer before any adjustment in the function.
   The offset for the argument pointer is fixed for the native compiler,
   it is either zero (for the no arguments case) or large enough to hold
   all argument registers.
   The offset for the auto pointer is the fourth argument to the .frame
   directive (local_offset).
   To stay compatible with the native tools we use the same offsets
   from the virtual frame pointer and adjust the debugger arg/auto offsets
   accordingly. These debugger offsets are set up in output_prolog.  */

extern long alpha_arg_offset;
extern long alpha_auto_offset;
#define DEBUGGER_AUTO_OFFSET(X) \
  ((GET_CODE (X) == PLUS ? INTVAL (XEXP (X, 1)) : 0) + alpha_auto_offset)
#define DEBUGGER_ARG_OFFSET(OFFSET, X) (OFFSET + alpha_arg_offset)


#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE)				\
  alpha_output_lineno (STREAM, LINE)
extern void alpha_output_lineno ();

#define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME)			\
  alpha_output_filename (STREAM, NAME)
extern void alpha_output_filename ();


/* mips-tfile.c limits us to strings of one page.  */
#define DBX_CONTIN_LENGTH 4000

/* By default, turn on GDB extensions.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Stabs-in-ECOFF can't handle dbxout_function_end().  */
#define NO_DBX_FUNCTION_END 1

/* If we are smuggling stabs through the ALPHA ECOFF object
   format, put a comment in front of the .stab<x> operation so
   that the ALPHA assembler does not choke.  The mips-tfile program
   will correctly put the stab into the object file.  */

#define ASM_STABS_OP	((TARGET_GAS) ? ".stabs" : " #.stabs")
#define ASM_STABN_OP	((TARGET_GAS) ? ".stabn" : " #.stabn")
#define ASM_STABD_OP	((TARGET_GAS) ? ".stabd" : " #.stabd")

/* Forward references to tags are allowed.  */
#define SDB_ALLOW_FORWARD_REFERENCES

/* Unknown tags are also allowed.  */
#define SDB_ALLOW_UNKNOWN_REFERENCES

#define PUT_SDB_DEF(a)					\
do {							\
  fprintf (asm_out_file, "\t%s.def\t",			\
	   (TARGET_GAS) ? "" : "#");			\
  ASM_OUTPUT_LABELREF (asm_out_file, a); 		\
  fputc (';', asm_out_file);				\
} while (0)

#define PUT_SDB_PLAIN_DEF(a)				\
do {							\
  fprintf (asm_out_file, "\t%s.def\t.%s;",		\
	   (TARGET_GAS) ? "" : "#", (a));		\
} while (0)

#define PUT_SDB_TYPE(a)					\
do {							\
  fprintf (asm_out_file, "\t.type\t0x%x;", (a));	\
} while (0)

/* For block start and end, we create labels, so that
   later we can figure out where the correct offset is.
   The normal .ent/.end serve well enough for functions,
   so those are just commented out.  */

extern int sdb_label_count;		/* block start/end next label # */

#define PUT_SDB_BLOCK_START(LINE)			\
do {							\
  fprintf (asm_out_file,				\
	   "$Lb%d:\n\t%s.begin\t$Lb%d\t%d\n",		\
	   sdb_label_count,				\
	   (TARGET_GAS) ? "" : "#",			\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_BLOCK_END(LINE)				\
do {							\
  fprintf (asm_out_file,				\
	   "$Le%d:\n\t%s.bend\t$Le%d\t%d\n",		\
	   sdb_label_count,				\
	   (TARGET_GAS) ? "" : "#",			\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_FUNCTION_START(LINE)

#define PUT_SDB_FUNCTION_END(LINE)

#define PUT_SDB_EPILOGUE_END(NAME)

/* No point in running CPP on our assembler output.  */
#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_GAS) != 0
/* Don't pass -g to GNU as, because some versions don't accept this option.  */
#define ASM_SPEC "%{malpha-as:-g} -nocpp %{pg}"
#else
/* In OSF/1 v3.2c, the assembler by default does not output file names which
   causes mips-tfile to fail.  Passing -g to the assembler fixes this problem.
   ??? Stricly speaking, we only need -g if the user specifies -g.  Passing
   it always means that we get slightly larger than necessary object files
   if the user does not specify -g.  If we don't pass -g, then mips-tfile
   will need to be fixed to work in this case.  */
#define ASM_SPEC "%{!mgas:-g} -nocpp %{pg}"
#endif

/* Specify to run a post-processor, mips-tfile after the assembler
   has run to stuff the ecoff debug information into the object file.
   This is needed because the Alpha assembler provides no way
   of specifying such information in the assembly file.  */

#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_GAS) != 0

#define ASM_FINAL_SPEC "\
%{malpha-as: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#else
#define ASM_FINAL_SPEC "\
%{!mgas: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#endif

/* Macros for mips-tfile.c to encapsulate stabs in ECOFF, and for
   mips-tdump.c to print them out.

   These must match the corresponding definitions in gdb/mipsread.c.
   Unfortunately, gcc and gdb do not currently share any directories. */

#define CODE_MASK 0x8F300
#define MIPS_IS_STAB(sym) (((sym)->index & 0xFFF00) == CODE_MASK)
#define MIPS_MARK_STAB(code) ((code)+CODE_MASK)
#define MIPS_UNMARK_STAB(code) ((code)-CODE_MASK)

/* Override some mips-tfile definitions.  */

#define SHASH_SIZE 511
#define THASH_SIZE 55

/* Align ecoff symbol tables to avoid OSF1/1.3 nm complaints.  */

#define ALIGN_SYMTABLE_OFFSET(OFFSET) (((OFFSET) + 7) & ~7)

/* The system headers under OSF/1 are C++-aware.  */
#define NO_IMPLICIT_EXTERN_C

/* The linker will stick __main into the .init section.  */
#define HAS_INIT_SECTION
#define LD_INIT_SWITCH "-init"
#define LD_FINI_SWITCH "-fini"

/* Define gethostid in unistd.h as returning an int, not a long.  */
#define SYS_PROTO_OVERRIDES \
  "extern int gethostid (void);",
