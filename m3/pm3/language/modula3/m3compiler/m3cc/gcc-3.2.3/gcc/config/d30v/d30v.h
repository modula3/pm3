/* Definitions of target machine for Mitsubishi D30V.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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

#ifndef GCC_D30V_H

/* D30V specific macros */

/* Align an address */
#define D30V_ALIGN(addr,align) (((addr) + (align) - 1) & ~((align) - 1))


/* Driver configuration */

/* Defined in svr4.h.  */
/* #define SWITCH_TAKES_ARG(CHAR) */

/* Defined in svr4.h.  */
/* #define WORD_SWITCH_TAKES_ARG(NAME) */

/* Defined in svr4.h.  */
#undef	ASM_SPEC
#define ASM_SPEC "\
%{!mno-asm-optimize: %{O*: %{!O0: -O} %{O0: %{masm-optimize: -O}}}} \
%{v} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*}"

/* Defined in svr4.h.  */
/* #define ASM_FINAL_SPEC "" */

/* Defined in svr4.h.  */
#undef	LINK_SPEC
#define LINK_SPEC "\
%{h*} %{v:-V} \
%{b} %{Wl,*:%*} \
%{static:-dn -Bstatic} \
%{shared:-G -dy -z text} \
%{symbolic:-Bsymbolic -G -dy -z text} \
%{G:-G} \
%{YP,*} \
%{Qy:} %{!Qn:-Qy} \
%{mextmem: -m d30v_e} %{mextmemory: -m d30v_e} %{monchip: -m d30v_o}"

/* Defined in svr4.h.  */
#undef	LIB_SPEC
#define LIB_SPEC "--start-group -lsim -lc --end-group"

/* Defined in svr4.h.  */
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crtbegin%O%s"

/* Defined in svr4.h.  */
#undef	ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s"

/* Defined in svr4.h for host compilers.  */
/* #define MD_EXEC_PREFIX "" */

/* Defined in svr4.h for host compilers.  */
/* #define MD_STARTFILE_PREFIX "" */


/* Run-time target specifications */

#define CPP_PREDEFINES "-D__D30V__ -Amachine=d30v"

/* This declaration should be present.  */
extern int target_flags;

#define MASK_NO_COND_MOVE	0x00000001	/* disable conditional moves */

#define MASK_DEBUG_ARG		0x10000000	/* debug argument handling */
#define MASK_DEBUG_STACK	0x20000000	/* debug stack allocations */
#define MASK_DEBUG_ADDR		0x40000000	/* debug GO_IF_LEGITIMATE_ADDRESS */

#define TARGET_NO_COND_MOVE	(target_flags & MASK_NO_COND_MOVE)
#define TARGET_DEBUG_ARG	(target_flags & MASK_DEBUG_ARG)
#define TARGET_DEBUG_STACK	(target_flags & MASK_DEBUG_STACK)
#define TARGET_DEBUG_ADDR	(target_flags & MASK_DEBUG_ADDR)

#define TARGET_COND_MOVE	(! TARGET_NO_COND_MOVE)

/* Default switches used.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#define TARGET_SWITCHES							\
{									\
  { "cond-move",	-MASK_NO_COND_MOVE,				\
      N_("Enable use of conditional move instructions") },		\
									\
  { "no-cond-move",	MASK_NO_COND_MOVE,				\
      N_("Disable use of conditional move instructions") },		\
									\
  { "debug-arg",	 MASK_DEBUG_ARG,				\
      N_("Debug argument support in compiler") },			\
									\
  { "debug-stack",	 MASK_DEBUG_STACK,				\
      N_("Debug stack support in compiler") },				\
									\
  { "debug-addr",	 MASK_DEBUG_ADDR,				\
      N_("Debug memory address support in compiler") },			\
									\
  { "asm-optimize",	 0,						\
      N_("Make adjacent short instructions parallel if possible") },	\
									\
  { "no-asm-optimize",	 0,						\
      N_("Do not make adjacent short instructions parallel") },	\
									\
  { "extmem",		 0,						\
      N_("Link programs/data to be in external memory by default") },	\
									\
  { "extmemory",	 0,						\
      N_("Link programs/data to be in external memory by default") },	\
									\
  { "onchip",		 0,						\
      N_("Link programs/data to be in onchip memory by default") },	\
									\
  { "",			 TARGET_DEFAULT, "" },				\
}

#define TARGET_OPTIONS							\
{									\
  {"branch-cost=",  &d30v_branch_cost_string,				\
     N_("Change the branch costs within the compiler") },		\
									\
  {"cond-exec=",    &d30v_cond_exec_string,				\
     N_("Change the threshold for conversion to conditional execution") }, \
}

#define TARGET_VERSION fprintf (stderr, " d30v")

#define OVERRIDE_OPTIONS override_options ()

#define CAN_DEBUG_WITHOUT_FP


/* Storage Layout */

#define BITS_BIG_ENDIAN 1

#define BYTES_BIG_ENDIAN 1

#define WORDS_BIG_ENDIAN 1

#define BITS_PER_UNIT 8

#define BITS_PER_WORD 32

#define UNITS_PER_WORD 4

#define POINTER_SIZE 32

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)				\
do {									\
  if (GET_MODE_CLASS (MODE) == MODE_INT					\
      && GET_MODE_SIZE (MODE) < 4)					\
    (MODE) = SImode;							\
} while (0)

#define PARM_BOUNDARY 32

#define STACK_BOUNDARY 64

#define FUNCTION_BOUNDARY 64

#define BIGGEST_ALIGNMENT 64

/* Defined in svr4.h.  */
/* #define MAX_OFILE_ALIGNMENT */

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define STRICT_ALIGNMENT 1

/* Defined in svr4.h.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT


/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32

#define SHORT_TYPE_SIZE 16

#define LONG_TYPE_SIZE 32

#define LONG_LONG_TYPE_SIZE 64

#define CHAR_TYPE_SIZE 8

#define FLOAT_TYPE_SIZE 32

#define DOUBLE_TYPE_SIZE 64

#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

/* Defined in svr4.h.  */
/* #define SIZE_TYPE */

/* Defined in svr4.h.  */
/* #define PTRDIFF_TYPE */

/* Defined in svr4.h.  */
/* #define WCHAR_TYPE */

/* Defined in svr4.h.  */
/* #define WCHAR_TYPE_SIZE */


/* D30V register layout.  */

/* Return true if a value is inside a range */
#define IN_RANGE_P(VALUE, LOW, HIGH) \
  (((unsigned)((VALUE) - (LOW))) <= ((unsigned)((HIGH) - (LOW))))

/* General purpose registers.  */
#define GPR_FIRST	0			/* First gpr */
#define GPR_LAST	(GPR_FIRST + 63)	/* Last gpr */
#define GPR_R0		GPR_FIRST		/* R0, constant 0 */
#define GPR_ARG_FIRST	(GPR_FIRST + 2)		/* R2, first argument reg */
#define GPR_ARG_LAST	(GPR_FIRST + 17)	/* R17, last argument reg */
#define GPR_RET_VALUE	GPR_ARG_FIRST		/* R2, function return reg */
#define GPR_ATMP_FIRST	(GPR_FIRST + 20)	/* R20, tmp to save accs */
#define GPR_ATMP_LAST	(GPR_FIRST + 21)	/* R21, tmp to save accs */
#define GPR_STACK_TMP	(GPR_FIRST + 22)	/* R22, tmp for saving stack */
#define GPR_RES_FIRST	(GPR_FIRST + 32)	/* R32, first reserved reg */
#define GPR_RES_LAST	(GPR_FIRST + 35)	/* R35, last reserved reg */
#define GPR_FP		(GPR_FIRST + 61)	/* Frame pointer */
#define GPR_LINK	(GPR_FIRST + 62)	/* Return address register */
#define GPR_SP		(GPR_FIRST + 63)	/* Stack pointer */

/* Argument register that is eliminated in favor of the frame and/or stack
   pointer.  Also add register to point to where the return address is
   stored.  */
#define SPECIAL_REG_FIRST		(GPR_LAST + 1)
#define SPECIAL_REG_LAST		(SPECIAL_REG_FIRST)
#define ARG_POINTER_REGNUM		(SPECIAL_REG_FIRST + 0)
#define SPECIAL_REG_P(R)		((R) == SPECIAL_REG_FIRST)

#define GPR_OR_SPECIAL_REG_P(R)		IN_RANGE_P (R, GPR_FIRST, SPECIAL_REG_LAST)
#define GPR_P(R)			IN_RANGE_P (R, GPR_FIRST, GPR_LAST)
#define GPR_OR_PSEUDO_P(R)		(GPR_OR_SPECIAL_REG_P (R)	\
					 || (R) >= FIRST_PSEUDO_REGISTER)

/* Flag bits.  */
#define FLAG_FIRST	(SPECIAL_REG_LAST + 1)	/* First flag */
#define FLAG_LAST	(FLAG_FIRST + 7)	/* Last flag */
#define FLAG_F0		(FLAG_FIRST)		/* F0, used in prediction */
#define FLAG_F1		(FLAG_FIRST + 1)	/* F1, used in prediction */
#define FLAG_F2		(FLAG_FIRST + 2)	/* F2, general flag */
#define FLAG_F3		(FLAG_FIRST + 3)	/* F3, general flag */
#define FLAG_SAT	(FLAG_FIRST + 4)	/* F4, saturation flag */
#define FLAG_OVERFLOW	(FLAG_FIRST + 5)	/* F5, overflow flag */
#define FLAG_ACC_OVER	(FLAG_FIRST + 6)	/* F6, accumulated overflow */
#define FLAG_CARRY	(FLAG_FIRST + 7)	/* F7, carry/borrow flag */
#define FLAG_BORROW	FLAG_CARRY

#define FLAG_P(R)		IN_RANGE_P (R, FLAG_FIRST, FLAG_LAST)
#define FLAG_OR_PSEUDO_P(R)	(FLAG_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

#define BR_FLAG_P(R)		IN_RANGE_P (R, FLAG_F0, FLAG_F1)
#define BR_FLAG_OR_PSEUDO_P(R)	(BR_FLAG_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

/* Accumulators */
#define ACCUM_FIRST	(FLAG_LAST + 1)		/* First accumulator */
#define ACCUM_A0	ACCUM_FIRST		/* Register A0 */
#define ACCUM_A1	(ACCUM_FIRST + 1)	/* Register A1 */
#define ACCUM_LAST	(ACCUM_FIRST + 1)	/* Last accumulator */

#define ACCUM_P(R)		IN_RANGE_P (R, ACCUM_FIRST, ACCUM_LAST)
#define ACCUM_OR_PSEUDO_P(R)	(ACCUM_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

/* Special registers.  Note, we only define the registers that can actually
   be used.  */
#define CR_FIRST	(ACCUM_LAST + 1)	/* First CR */
#define CR_LAST		(CR_FIRST + 14)		/* Last CR */
#define CR_PSW		(CR_FIRST + 0)		/* CR0, Program status word */
#define CR_BPSW		(CR_FIRST + 1)		/* CR1, Backup PSW */
#define CR_PC		(CR_FIRST + 2)		/* CR2, Program counter */
#define CR_BPC		(CR_FIRST + 3)		/* CR3, Backup PC */
#define CR_DPSW		(CR_FIRST + 4)		/* CR4, Debug PSW */
#define CR_DPC		(CR_FIRST + 5)		/* CR5, Debug PC */
#define CR_RPT_C	(CR_FIRST + 6)		/* CR7, loop count register */
#define CR_RPT_S	(CR_FIRST + 7)		/* CR8, loop start address */
#define CR_RPT_E	(CR_FIRST + 8)		/* CR9, loop end address */
#define CR_MOD_S	(CR_FIRST + 9)		/* CR10, modulo address start*/
#define CR_MOD_E	(CR_FIRST + 10)		/* CR11, modulo address */
#define CR_IBA		(CR_FIRST + 11)		/* CR14, Interrupt break addr */
#define CR_EIT_VB	(CR_FIRST + 12)		/* CR15, EIT vector address */
#define CR_INT_S	(CR_FIRST + 13)		/* CR16, Interrupt status */
#define CR_INT_M	(CR_FIRST + 14)		/* CR17, Interrupt mask */

#define CR_P(R)			IN_RANGE_P (R, CR_FIRST, CR_LAST)
#define CR_OR_PSEUDO_P(R)	(CR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)


/* Register Basics */

/* Number of hardware registers known to the compiler.  They receive numbers 0
   through `FIRST_PSEUDO_REGISTER-1'; thus, the first pseudo register's number
   really is assigned the number `FIRST_PSEUDO_REGISTER'.  */
#define FIRST_PSEUDO_REGISTER (CR_LAST + 1)

/* An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation.  These would include the stack pointer, the frame pointer
   (except on machines where that can be used as a general register when no
   frame pointer is needed), the program counter on machines where that is
   considered one of the addressable registers, and any other numbered register
   with a standard use.

   This information is expressed as a sequence of numbers, separated by commas
   and surrounded by braces.  The Nth number is 1 if register N is fixed, 0
   otherwise.

   The table initialized from this macro, and the table initialized by the
   following one, may be overridden at run time either automatically, by the
   actions of the macro `CONDITIONAL_REGISTER_USAGE', or by the user with the
   command options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.  */
#define FIXED_REGISTERS							\
{									\
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* R0  - R15 */	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,  /* R16 - R31 */	\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* R32 - R47 */	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  /* R48 - R63 */	\
  1,						   /* ARG ptr */	\
  0, 0, 0, 0, 1, 1, 1, 1,			   /* F0 - F7 */	\
  0, 0,						   /* A0 - A1 */	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	   /* CRs */		\
}

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */
#define CALL_USED_REGISTERS		        			\
{					        			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* R0  - R15 */	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* R16 - R31 */	\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* R32 - R47 */	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  /* R48 - R63 */	\
  1,						   /* ARG ptr */	\
  1, 1, 1, 1, 1, 1, 1, 1,			   /* F0 - F7 */	\
  1, 0,						   /* A0 - A1 */	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	   /* CRs */		\
}

/* Zero or more C statements that may conditionally modify two variables
   `fixed_regs' and `call_used_regs' (both of type `char []') after they have
   been initialized from the two preceding macros.

   This is necessary in case the fixed or call-clobbered registers depend on
   target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target flags,
   you may indicate this to GCC by using this macro to modify `fixed_regs' and
   `call_used_regs' to 1 for each of the registers in the classes which should
   not be used by GCC.  Also define the macro `REG_CLASS_FROM_LETTER' to return
   `NO_REGS' if it is called with a letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all of the
   insn patterns whose constraints permit this class are controlled by target
   switches, then GCC will automatically avoid using these registers when the
   target switches are opposed to them.)  */
/* #define CONDITIONAL_REGISTER_USAGE */

/* If this macro is defined and has a nonzero value, it means that `setjmp' and
   related functions fail to save the registers, or that `longjmp' fails to
   restore them.  To compensate, the compiler avoids putting variables in
   registers in functions that use `setjmp'.  */
/* #define NON_SAVING_SETJMP */

/* Define this macro if the target machine has register windows.  This C
   expression returns the register number as seen by the called function
   corresponding to the register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */
/* #define INCOMING_REGNO(OUT) */

/* Define this macro if the target machine has register windows.  This C
   expression returns the register number as seen by the calling function
   corresponding to the register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */
/* #define OUTGOING_REGNO(IN) */


/* Order of allocation of registers */

/* If defined, an initializer for a vector of integers, containing the numbers
   of hard registers in the order in which GNU CC should prefer to use them
   (from most preferred to least).

   If this macro is not defined, registers are used lowest numbered first (all
   else being equal).

   One use of this macro is on machines where the highest numbered registers
   must always be saved and the save-multiple-registers instruction supports
   only sequences of consecutive registers.  On such machines, define
   `REG_ALLOC_ORDER' to be an initializer that lists the highest numbered
   allocatable register first.  */

#define REG_ALLOC_ORDER							\
{									\
  /* volatile registers */						\
  GPR_FIRST + 2,    GPR_FIRST + 3,    GPR_FIRST + 4,    GPR_FIRST + 5,	\
  GPR_FIRST + 6,    GPR_FIRST + 7,    GPR_FIRST + 8,    GPR_FIRST + 9,	\
  GPR_FIRST + 10,   GPR_FIRST + 11,   GPR_FIRST + 12,   GPR_FIRST + 13,	\
  GPR_FIRST + 14,   GPR_FIRST + 15,   GPR_FIRST + 16,   GPR_FIRST + 17,	\
  GPR_FIRST + 18,   GPR_FIRST + 19,   GPR_FIRST + 20,   GPR_FIRST + 21,	\
  GPR_FIRST + 22,   GPR_FIRST + 23,   GPR_FIRST + 24,   GPR_FIRST + 25, \
  GPR_FIRST + 1,							\
									\
  /* saved registers */							\
  GPR_FIRST + 34,   GPR_FIRST + 35,   GPR_FIRST + 36,   GPR_FIRST + 37,	\
  GPR_FIRST + 38,   GPR_FIRST + 39,   GPR_FIRST + 40,   GPR_FIRST + 41,	\
  GPR_FIRST + 42,   GPR_FIRST + 43,   GPR_FIRST + 44,   GPR_FIRST + 45,	\
  GPR_FIRST + 46,   GPR_FIRST + 47,   GPR_FIRST + 48,   GPR_FIRST + 49,	\
  GPR_FIRST + 50,   GPR_FIRST + 51,   GPR_FIRST + 52,   GPR_FIRST + 53,	\
  GPR_FIRST + 54,   GPR_FIRST + 55,   GPR_FIRST + 56,   GPR_FIRST + 57,	\
  GPR_FIRST + 58,   GPR_FIRST + 59,   GPR_FIRST + 60,   GPR_FIRST + 61,	\
  GPR_FIRST + 62,							\
									\
  /* flags */								\
  FLAG_F2,          FLAG_F3,          FLAG_F0,          FLAG_F1,	\
  FLAG_SAT,         FLAG_OVERFLOW,    FLAG_ACC_OVER,    FLAG_CARRY,	\
									\
  /* accumultors */							\
  ACCUM_FIRST + 0,  ACCUM_FIRST + 1,					\
									\
  /* fixed registers */							\
  GPR_FIRST + 0,    GPR_FIRST + 26,   GPR_FIRST + 27,   GPR_FIRST + 28,	\
  GPR_FIRST + 29,   GPR_FIRST + 30,   GPR_FIRST + 31,   GPR_FIRST + 32,	\
  GPR_FIRST + 33,   GPR_FIRST + 63,					\
  CR_PSW,	    CR_BPSW,	      CR_PC,		CR_BPC,		\
  CR_DPSW,	    CR_DPC,	      CR_RPT_C,		CR_RPT_S,	\
  CR_RPT_E,	    CR_MOD_S,	      CR_MOD_E,		CR_IBA,		\
  CR_EIT_VB,	    CR_INT_S,	      CR_INT_M,				\
  ARG_POINTER_REGNUM,							\
}

/* A C statement (sans semicolon) to choose the order in which to allocate hard
   registers for pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.  Element 0
   should be the register to allocate first; element 1, the next register; and
   so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */
/* #define ORDER_REGS_FOR_LOCAL_ALLOC */


/* How Values Fit in Registers */

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.

   On a machine where all registers are exactly one word, a suitable definition
   of this macro is

        #define HARD_REGNO_NREGS(REGNO, MODE)            \
           ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
            / UNITS_PER_WORD))  */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
(ACCUM_P (REGNO) ? ((GET_MODE_SIZE (MODE) + 2*UNITS_PER_WORD - 1)	\
		    / (2*UNITS_PER_WORD))				\
		 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)		\
		    / UNITS_PER_WORD))

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  For a machine where all registers are equivalent, a suitable
   definition is

        #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

   It is not necessary for this macro to check for the numbers of fixed
   registers, because the allocation mechanism considers them to be always
   occupied.

   On some machines, double-precision values must be kept in even/odd register
   pairs.  The way to implement that is to define this macro to reject odd
   register numbers for such modes.

   The minimum requirement for a mode to be OK in a register is that the
   `movMODE' instruction pattern support moves between the register and any
   other hard register for which the mode is OK; and that moving a value into
   the register and back out not alter it.

   Since the same instruction used to move `SImode' will work for all narrower
   integer modes, it is not necessary on any machine for `HARD_REGNO_MODE_OK'
   to distinguish between these modes, provided you define patterns `movhi',
   etc., to take advantage of this.  This is useful because of the interaction
   between `HARD_REGNO_MODE_OK' and `MODES_TIEABLE_P'; it is very desirable for
   all integer modes to be tieable.

   Many machines have special registers for floating point arithmetic.  Often
   people assume that floating point machine modes are allowed only in floating
   point registers.  This is not true.  Any registers that can hold integers
   can safely *hold* a floating point machine mode, whether or not floating
   arithmetic can be done on it in those registers.  Integer move instructions
   can be used to move the values.

   On some machines, though, the converse is true: fixed-point machine modes
   may not go in floating registers.  This is true if the floating registers
   normalize any value stored in them, because storing a non-floating value
   there would garble it.  In this case, `HARD_REGNO_MODE_OK' should reject
   fixed-point machine modes in floating registers.  But if the floating
   registers do not automatically normalize, if you can store any bit pattern
   in one and retrieve it unchanged without a trap, then any machine mode may
   go in a floating register, so you can define this macro to say so.

   The primary significance of special floating registers is rather that they
   are the registers acceptable in floating point arithmetic instructions.
   However, this is of no concern to `HARD_REGNO_MODE_OK'.  You handle it by
   writing the proper constraints for those instructions.

   On some machines, the floating registers are especially slow to access, so
   that it is better to store a value in a stack frame than in such a register
   if floating point arithmetic is not being done.  As long as the floating
   registers are not in class `GENERAL_REGS', they will not be used unless some
   pattern's constraint asks for one.  */

extern unsigned char hard_regno_mode_ok[][FIRST_PSEUDO_REGISTER];
#define HARD_REGNO_MODE_OK(REGNO, MODE) hard_regno_mode_ok[ (int)MODE ][ REGNO ]

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */

extern unsigned char modes_tieable_p[];
#define MODES_TIEABLE_P(MODE1, MODE2) \
  modes_tieable_p[ (((int)(MODE1)) * (NUM_MACHINE_MODES)) + (int)(MODE2) ]

/* Define this macro if the compiler should avoid copies to/from CCmode
   registers.  You should only define this macro if support fo copying to/from
   CCmode is incomplete.  */
   
/* On the D30V, copying to/from CCmode is complete, but since there are only
   two CC registers usable for conditional tests, this helps gcse not compound
   the reload problem.  */
#define AVOID_CCMODE_COPIES


/* Handling Leaf Functions */

/* A C initializer for a vector, indexed by hard register number, which
   contains 1 for a register that is allowable in a candidate for leaf function
   treatment.

   If leaf function treatment involves renumbering the registers, then the
   registers marked here should be the ones before renumbering--those that GNU
   CC would ordinarily allocate.  The registers which will actually be used in
   the assembler code, after renumbering, should not be marked with 1 in this
   vector.

   Define this macro only if the target machine offers a way to optimize the
   treatment of leaf functions.  */
/* #define LEAF_REGISTERS */

/* A C expression whose value is the register number to which REGNO should be
   renumbered, when a function is treated as a leaf function.

   If REGNO is a register number which should not appear in a leaf function
   before renumbering, then the expression should yield -1, which will cause
   the compiler to abort.

   Define this macro only if the target machine offers a way to optimize the
   treatment of leaf functions, and registers need to be renumbered to do this.  */
/* #define LEAF_REG_REMAP(REGNO) */


/* Registers That Form a Stack.  */

/* Define this if the machine has any stack-like registers.  */
/* #define STACK_REGS */

/* The number of the first stack-like register.  This one is the top
   of the stack.  */
/* #define FIRST_STACK_REG */

/* The number of the last stack-like register.  This one is the
   bottom of the stack.  */
/* #define LAST_STACK_REG */


/* Register Classes */

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there are.

   Each register class has a number, which is the value of casting the class
   name to type `int'.  The number serves as an index in many of the tables
   described below.  */
enum reg_class
{
  NO_REGS,
  REPEAT_REGS,
  CR_REGS,
  ACCUM_REGS,
  OTHER_FLAG_REGS,
  F0_REGS,
  F1_REGS,
  BR_FLAG_REGS,
  FLAG_REGS,
  EVEN_REGS,
  GPR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS GPR_REGS

/* The number of distinct register classes, defined as follows:

        #define N_REG_CLASSES (int) LIM_REG_CLASSES  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "REPEAT_REGS",							\
  "CR_REGS",								\
  "ACCUM_REGS",								\
  "OTHER_FLAG_REGS",							\
  "F0_REGS",								\
  "F1_REGS",								\
  "BR_FLAG_REGS",							\
  "FLAG_REGS",								\
  "EVEN_REGS",								\
  "GPR_REGS",								\
  "ALL_REGS",								\
}

/* Create mask bits for 3rd word of REG_CLASS_CONTENTS */
#define MASK_WORD3(REG) ((long)1 << ((REG) - 64))

#define NO_MASK		0
#define REPEAT_MASK	MASK_WORD3 (CR_RPT_C)
#define CR_MASK		(MASK_WORD3 (CR_PSW)	 | MASK_WORD3 (CR_BPSW)	  \
			 | MASK_WORD3 (CR_PC)	 | MASK_WORD3 (CR_BPC)	  \
			 | MASK_WORD3 (CR_DPSW)	 | MASK_WORD3 (CR_DPC)	  \
			 | MASK_WORD3 (CR_RPT_C) | MASK_WORD3 (CR_RPT_S)  \
			 | MASK_WORD3 (CR_RPT_E) | MASK_WORD3 (CR_MOD_S)  \
			 | MASK_WORD3 (CR_MOD_E) | MASK_WORD3 (CR_IBA)	  \
			 | MASK_WORD3 (CR_EIT_VB) | MASK_WORD3 (CR_INT_S) \
			 | MASK_WORD3 (CR_INT_M))

#define ACCUM_MASK	(MASK_WORD3 (ACCUM_A0)	 | MASK_WORD3 (ACCUM_A1))
#define OTHER_FLAG_MASK	(MASK_WORD3 (FLAG_F2)	 | MASK_WORD3 (FLAG_F3)	\
			 | MASK_WORD3 (FLAG_SAT) | MASK_WORD3 (FLAG_OVERFLOW) \
			 | MASK_WORD3 (FLAG_ACC_OVER) | MASK_WORD3 (FLAG_CARRY))

#define F0_MASK		MASK_WORD3 (FLAG_F0)
#define F1_MASK		MASK_WORD3 (FLAG_F1)
#define BR_FLAG_MASK	(F0_MASK | F1_MASK)
#define FLAG_MASK	(BR_FLAG_MASK | OTHER_FLAG_MASK)
#define SPECIAL_MASK	MASK_WORD3 (ARG_POINTER_REGNUM)

#define ALL_MASK	(CR_MASK | ACCUM_MASK | FLAG_MASK | SPECIAL_MASK)

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not suffice.
   Then the integers are replaced by sub-initializers, braced groupings
   containing several integers.  Each sub-initializer must be suitable as an
   initializer for the type `HARD_REG_SET' which is defined in
   `hard-reg-set.h'.  */
#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, NO_MASK },		/* NO_REGS */		\
  { 0x00000000, 0x00000000, REPEAT_MASK },	/* REPEAT_REGS */	\
  { 0x00000000, 0x00000000, CR_MASK },		/* CR_REGS */		\
  { 0x00000000, 0x00000000, ACCUM_MASK },	/* ACCUM_REGS */	\
  { 0x00000000, 0x00000000, OTHER_FLAG_MASK },	/* OTHER_FLAG_REGS */	\
  { 0x00000000, 0x00000000, F0_MASK },		/* F0_REGS */		\
  { 0x00000000, 0x00000000, F1_MASK },		/* F1_REGS */		\
  { 0x00000000, 0x00000000, BR_FLAG_MASK },	/* BR_FLAG_REGS */	\
  { 0x00000000, 0x00000000, FLAG_MASK },	/* FLAG_REGS */		\
  { 0xfffffffc, 0x3fffffff, NO_MASK },		/* EVEN_REGS */		\
  { 0xffffffff, 0xffffffff, SPECIAL_MASK },	/* GPR_REGS */		\
  { 0xffffffff, 0xffffffff, ALL_MASK },		/* ALL_REGS */		\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */

extern enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[ (REGNO) ]

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GPR_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS GPR_REGS

/* A C expression which defines the machine-dependent operand constraint
   letters for register classes.  If CHAR is such a letter, the value should be
   the register class corresponding to it.  Otherwise, the value should be
   `NO_REGS'.  The register letter `r', corresponding to class `GENERAL_REGS',
   will not be passed to this macro; you do not need to handle it.

   The following letters are unavailable, due to being used as
   constraints:
	'0'..'9'
	'<', '>'
	'E', 'F', 'G', 'H'
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'
	'Q', 'R', 'S', 'T', 'U'
	'V', 'X'
	'g', 'i', 'm', 'n', 'o', 'p', 'r', 's' */

extern enum reg_class reg_class_from_letter[256];
#define REG_CLASS_FROM_LETTER(CHAR) reg_class_from_letter[(unsigned char)(CHAR)]

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.  */

#define REGNO_OK_FOR_BASE_P(NUM) 					\
((NUM) < FIRST_PSEUDO_REGISTER						\
 ? GPR_P (NUM)								\
 : (reg_renumber[NUM] >= 0 && GPR_P (reg_renumber[NUM])))


/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */

#define REGNO_OK_FOR_INDEX_P(NUM)					\
((NUM) < FIRST_PSEUDO_REGISTER						\
 ? GPR_P (NUM)								\
 : (reg_renumber[NUM] >= 0 && GPR_P (reg_renumber[NUM])))

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to copy value X into a register in class CLASS.
   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.  On many machines, the following definition is safe:

        #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

   Sometimes returning a more restrictive class makes better code.  For
   example, on the 68000, when X is an integer constant that is in range for a
   `moveq' instruction, the value of this macro is always `DATA_REGS' as long
   as CLASS includes the data registers.  Requiring a data register guarantees
   that a `moveq' will be used.

   If X is a `const_double', by returning `NO_REGS' you can force X into a
   memory constant.  This is useful on certain machines where immediate
   floating values cannot be loaded into certain kinds of registers.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

/* Like `PREFERRED_RELOAD_CLASS', but for output reloads instead of input
   reloads.  If you don't define this macro, the default is to use CLASS,
   unchanged.  */
/* #define PREFERRED_OUTPUT_RELOAD_CLASS(X, CLASS) */

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to be able to hold a value of mode MODE in a reload
   register for which class CLASS would ordinarily be used.

   Unlike `PREFERRED_RELOAD_CLASS', this macro should be used when there are
   certain modes that simply can't go in certain reload classes.

   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.

   Don't define this macro unless the target machine has limitations which
   require the macro to do something nontrivial.  */
/* #define LIMIT_RELOAD_CLASS(MODE, CLASS) */

/* Many machines have some registers that cannot be copied directly to or from
   memory or even from other types of registers.  An example is the `MQ'
   register, which on most machines, can only be copied to or from general
   registers, but not memory.  Some machines allow copying all registers to and
   from memory, but require a scratch register for stores to some memory
   locations (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the Sparc when compiling PIC).  In some cases,
   both an intermediate and a scratch register are required.

   You should define these macros to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   CLASS in MODE requires an intermediate register, you should define
   `SECONDARY_INPUT_RELOAD_CLASS' to return the largest register class all of
   whose registers can be used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate or scratch
   register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be defined to return the
   largest register class required.  If the requirements for input and output
   reloads are the same, the macro `SECONDARY_RELOAD_CLASS' should be used
   instead of defining both macros identically.

   The values returned by these macros are often `GENERAL_REGS'.  Return
   `NO_REGS' if no spare register is needed; i.e., if X can be directly copied
   to or from a register of CLASS in MODE without requiring a scratch register.
   Do not define this macro if it would always return `NO_REGS'.

   If a scratch register is required (either with or without an intermediate
   register), you should define patterns for `reload_inM' or `reload_outM', as
   required (*note Standard Names::..  These patterns, which will normally be
   implemented with a `define_expand', should be similar to the `movM'
   patterns, except that operand 2 is the scratch register.

   Define constraints for the reload register and scratch register that contain
   a single register class.  If the original reload register (whose class is
   CLASS) can meet the constraint given in the pattern, the value returned by
   these macros is used for the class of the scratch register.  Otherwise, two
   additional reload registers are required.  Their classes are obtained from
   the constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register, which could
   either be in a hard register or in memory.  Use `true_regnum' to find out;
   it will return -1 if the pseudo is in memory and the hard register number if
   it is in a register.

   These macros should not be used in the case where a particular class of
   registers can only be copied to memory and not to another class of
   registers.  In that case, secondary reload registers are not needed and
   would not be helpful.  Instead, a stack location must be used to perform the
   copy and the `movM' pattern should use memory as an intermediate storage.
   This case often occurs between floating-point and general registers.  */

#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X)				\
((CLASS) == GPR_REGS		? NO_REGS				\
 : (CLASS) == EVEN_REGS		? NO_REGS				\
 : (CLASS) == ACCUM_REGS	? EVEN_REGS				\
 :				  GPR_REGS)

/* #define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) */
/* #define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) */

/* Certain machines have the property that some registers cannot be copied to
   some other registers without using memory.  Define this macro on those
   machines to be a C expression that is non-zero if objects of mode M in
   registers of CLASS1 can only be copied to registers of class CLASS2 by
   storing a register of CLASS1 into memory and loading that memory location
   into a register of CLASS2.

   Do not define this macro if its value would always be zero.  */
/* #define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, M) */

/* Normally when `SECONDARY_MEMORY_NEEDED' is defined, the compiler allocates a
   stack slot for a memory location needed for register copies.  If this macro
   is defined, the compiler instead uses the memory location defined by this
   macro.

   Do not define this macro if you do not define
   `SECONDARY_MEMORY_NEEDED'.  */
/* #define SECONDARY_MEMORY_NEEDED_RTX(MODE) */

/* When the compiler needs a secondary memory location to copy between two
   registers of mode MODE, it normally allocates sufficient memory to hold a
   quantity of `BITS_PER_WORD' bits and performs the store and load operations
   in a mode that many bits wide and whose class is the same as that of MODE.

   This is right thing to do on most machines because it ensures that all bits
   of the register are copied and prevents accesses to the registers in a
   narrower mode, which some machines prohibit for floating-point registers.

   However, this default behavior is not correct on some machines, such as the
   DEC Alpha, that store short integers in floating-point registers differently
   than in integer registers.  On those machines, the default widening will not
   work correctly and you must define this macro to suppress that widening in
   some cases.  See the file `alpha.h' for details.

   Do not define this macro if you do not define `SECONDARY_MEMORY_NEEDED' or
   if widening MODE to a mode that is `BITS_PER_WORD' bits wide is correct for
   your machine.  */
/* #define SECONDARY_MEMORY_NEEDED_MODE(MODE) */

/* Normally the compiler avoids choosing registers that have been explicitly
   mentioned in the rtl as spill registers (these registers are normally those
   used to pass parameters and return values).  However, some machines have so
   few registers of certain classes that there would not be enough registers to
   use as spill registers if this were done.

   Define `SMALL_REGISTER_CLASSES' to be an expression with a non-zero value on
   these machines.  When this macro has a non-zero value, the compiler allows
   registers explicitly used in the rtl to be used as spill registers but
   avoids extending the lifetime of these registers.

   It is always safe to define this macro with a non-zero value, but if you
   unnecessarily define it, you will reduce the amount of optimizations that
   can be performed in some cases.  If you do not define this macro with a
   non-zero value when it is required, the compiler will run out of spill
   registers and print a fatal error message.  For most machines, you should
   not define this macro at all.  */
/* #define SMALL_REGISTER_CLASSES */

/* A C expression whose value is nonzero if pseudos that have been assigned to
   registers of class CLASS would likely be spilled because registers of CLASS
   are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one register
   and zero otherwise.  On most machines, this default should be used.  Only
   define this macro to some other expression if pseudo allocated by
   `local-alloc.c' end up in memory because their hard registers were needed
   for spill registers.  If this macro returns nonzero for those classes, those
   pseudos will only be allocated by `global.c', which knows how to reallocate
   the pseudo to another register.  If there would not be another register
   available for reallocation, you should not change the definition of this
   macro since the only effect of such a definition would be to slow down
   register allocation.  */
#define CLASS_LIKELY_SPILLED_P(CLASS) \
  ((CLASS) != GPR_REGS && (CLASS) != EVEN_REGS)

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.  */

#define CLASS_MAX_NREGS(CLASS, MODE)					\
(((CLASS) == ACCUM_REGS)						\
 ? ((GET_MODE_SIZE (MODE) + 8 - 1) / 8)					\
 : ((GET_MODE_SIZE (MODE) + 4 - 1) / 4))

/* A C expression that defines the machine-dependent operand constraint letters
   (`I', `J', `K', .. 'P') that specify particular ranges of integer values.
   If C is one of those letters, the expression should check that VALUE, an
   integer, is in the appropriate range and return 1 if so, 0 otherwise.  If C
   is not one of those letters, the value should be 0 regardless of VALUE.  */
#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
((C) == 'I'	? IN_RANGE_P (VALUE, -32, 31)				\
 : (C) == 'J'	? IN_RANGE_P (VALUE, 0, 31)				\
 : (C) == 'K'	? IN_RANGE_P (exact_log2 (VALUE), 0, 31)		\
 : (C) == 'L'	? IN_RANGE_P (exact_log2 (~ (VALUE)), 0, 31)		\
 : (C) == 'M'	? ((VALUE) == 32)					\
 : (C) == 'N'	? ((VALUE) == 1)					\
 : (C) == 'O'	? ((VALUE) == 0)					\
 : (C) == 'P'	? IN_RANGE_P (VALUE, 32, 63)				\
 :		  FALSE)

/* A C expression that defines the machine-dependent operand constraint letters
   (`G', `H') that specify particular ranges of `const_double' values.

   If C is one of those letters, the expression should check that VALUE, an RTX
   of code `const_double', is in the appropriate range and return 1 if so, 0
   otherwise.  If C is not one of those letters, the value should be 0
   regardless of VALUE.

   `const_double' is used for all floating-point constants and for `DImode'
   fixed-point constants.  A given letter can accept either or both kinds of
   values.  It can use `GET_MODE' to distinguish between these kinds.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
((C) == 'G'	? (CONST_DOUBLE_LOW (VALUE) == 0			\
		   && CONST_DOUBLE_HIGH (VALUE) == 0)			\
 : (C) == 'H'	? FALSE							\
 :		  FALSE)

/* A C expression that defines the optional machine-dependent constraint
   letters (`Q', `R', `S', `T', `U') that can be used to segregate specific
   types of operands, usually memory references, for the target machine.
   Normally this macro will not be defined.  If it is required for a particular
   target machine, it should return 1 if VALUE corresponds to the operand type
   represented by the constraint letter C.  If C is not defined as an extra
   constraint, the value returned should be 0 regardless of VALUE.

   For example, on the ROMP, load instructions cannot have their output in r0
   if the memory reference contains a symbolic address.  Constraint letter `Q'
   is defined as representing a memory address that does *not* contain a
   symbolic address.  An alternative is specified with a `Q' constraint on the
   input and `r' on the output.  The next alternative specifies `m' on the
   input and a register class that does not include r0 on the output.  */

#define EXTRA_CONSTRAINT(VALUE, C)					\
(((C) == 'Q')	? short_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'R')	? single_reg_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'S')	? const_addr_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'T')	? long_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'U')	? FALSE							\
 :		  FALSE)


/* Basic Stack Layout */

/* Stack layout */

/* Structure used to define the d30v stack */
typedef struct d30v_stack {
  int varargs_p;		/* whether this is a varargs function */
  int varargs_size;		/* size to hold varargs args passed in regs */
  int vars_size;		/* variable save area size */
  int parm_size;		/* outgoing parameter size */
  int gpr_size;			/* size of saved GPR registers */
  int accum_size;		/* size of saved ACCUM registers */
  int total_size;		/* total bytes allocated for stack */
				/* which registers are to be saved */
  int save_offset;		/* offset from new sp to start saving vars at */
  int link_offset;		/* offset r62 is saved at */
  int memrefs_varargs;		/* # of 2 word memory references for varargs */
  int memrefs_2words;		/* # of 2 word memory references */
  int memrefs_1word;		/* # of 1 word memory references */
				/* 1 for ldw/stw ops; 2 for ld2w/st2w ops */
  unsigned char save_p[FIRST_PSEUDO_REGISTER];
} d30v_stack_t;

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.

   When we say, "define this macro if ...," it means that the compiler checks
   this macro only with `#ifdef' so the precise definition used does not
   matter.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Define this macro if successive arguments to a function occupy decreasing
   addresses on the stack.  */
/* #define ARGS_GROW_DOWNWARD */

/* Offset from the frame pointer to the first local variable slot to be
   allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by subtracting the
   first slot's length from `STARTING_FRAME_OFFSET'.  Otherwise, it is found by
   adding the length of the first slot to the value `STARTING_FRAME_OFFSET'.  */

#define STARTING_FRAME_OFFSET						\
  (D30V_ALIGN (current_function_outgoing_args_size,			\
	       (STACK_BOUNDARY / BITS_PER_UNIT)))

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  If not specified, the default value of zero
   is used.  This is the proper value for most machines.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   location at which outgoing arguments are placed.  */
/* #define STACK_POINTER_OFFSET */

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   argument's address.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* Offset from the stack pointer register to an item dynamically allocated on
   the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the length
   of the outgoing arguments.  The default is correct for most machines.  See
   `function.c' for details.  */
/* #define STACK_DYNAMIC_OFFSET(FUNDECL) */

/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */
/* #define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) */

/* If defined, a C expression that produces the machine-specific code to setup
   the stack so that arbitrary frames can be accessed.  For example, on the
   Sparc, we must flush all of the register windows to the stack before we can
   access arbitrary stack frames.  This macro will seldom need to be defined.  */
/* #define SETUP_FRAME_ADDRESSES() */

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */

/* ??? This definition fails for leaf functions.  There is currently no
   general solution for this problem.  */

/* ??? There appears to be no way to get the return address of any previous
   frame except by disassembling instructions in the prologue/epilogue.
   So currently we support only the current frame.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)					\
  ((COUNT) == 0 ? d30v_return_addr() : const0_rtx)

/* Define this if the return address of a particular stack frame is
   accessed from the frame pointer of the previous stack frame.  */
/* #define RETURN_ADDR_IN_PREVIOUS_FRAME */

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */

/* Before the prologue, RA lives in r62.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx (REG, Pmode, GPR_LINK)

/* A C expression whose value is an integer giving the offset, in bytes, from
   the value of the stack pointer register to the top of the stack frame at the
   beginning of any function, before the prologue.  The top of the frame is
   defined to be the value of the stack pointer in the previous frame, just
   before the call instruction.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_FRAME_SP_OFFSET 0

/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */

#define INIT_EXPANDERS  d30v_init_expanders ()


/* Stack Checking.  */

/* A nonzero value if stack checking is done by the configuration files in a
   machine-dependent manner.  You should define this macro if stack checking is
   require by the ABI of your machine or if you would like to have to stack
   checking in some more efficient way than GNU CC's portable approach.  The
   default value of this macro is zero.  */
/* #define STACK_CHECK_BUILTIN */

/* An integer representing the interval at which GNU CC must generate stack
   probe instructions.  You will normally define this macro to be no larger
   than the size of the "guard pages" at the end of a stack area.  The default
   value of 4096 is suitable for most systems.  */
/* #define STACK_CHECK_PROBE_INTERVAL */

/* An integer which is nonzero if GNU CC should perform the stack probe as a
   load instruction and zero if GNU CC should use a store instruction.  The
   default is zero, which is the most efficient choice on most systems.  */
/* #define STACK_CHECK_PROBE_LOAD */

/* The number of bytes of stack needed to recover from a stack overflow, for
   languages where such a recovery is supported.  The default value of 75 words
   should be adequate for most machines.  */
/* #define STACK_CHECK_PROTECT */

/* The maximum size of a stack frame, in bytes.  GNU CC will generate probe
   instructions in non-leaf functions to ensure at least this many bytes of
   stack are available.  If a stack frame is larger than this size, stack
   checking will not be reliable and GNU CC will issue a warning.  The default
   is chosen so that GNU CC only generates one instruction on most systems.
   You should normally not change the default value of this macro.  */
/* #define STACK_CHECK_MAX_FRAME_SIZE */

/* GNU CC uses this value to generate the above warning message.  It represents
   the amount of fixed frame used by a function, not including space for any
   callee-saved registers, temporaries and user variables.  You need only
   specify an upper bound for this amount and will normally use the default of
   four words.  */
/* #define STACK_CHECK_FIXED_FRAME_SIZE */

/* The maximum size, in bytes, of an object that GNU CC will place in the fixed
   area of the stack frame when the user specifies `-fstack-check'.  GNU CC
   computed the default from the values of the above macros and you will
   normally not need to override that default.  */
/* #define STACK_CHECK_MAX_VAR_SIZE */


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */
#define STACK_POINTER_REGNUM GPR_SP

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */
#define FRAME_POINTER_REGNUM GPR_FP

/* On some machines the offset between the frame pointer and starting offset of
   the automatic variables is not known until after register allocation has
   been done (for example, because the saved registers are between these two
   locations).  On those machines, define `FRAME_POINTER_REGNUM' the number of
   a special, fixed register to be used internally until the offset is known,
   and define `HARD_FRAME_POINTER_REGNUM' to be actual the hard register number
   used for the frame pointer.

   You should define this macro only in the very rare circumstances when it is
   not possible to calculate the offset between the frame pointer and the
   automatic variables until after register allocation has been completed.
   When this macro is defined, you must also indicate in your definition of
   `ELIMINABLE_REGS' how to eliminate `FRAME_POINTER_REGNUM' into either
   `HARD_FRAME_POINTER_REGNUM' or `STACK_POINTER_REGNUM'.

   Do not define this macro if it would be the same as `FRAME_POINTER_REGNUM'.  */
/* #define HARD_FRAME_POINTER_REGNUM */

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  On some machines, this is the same as the frame
   pointer register.  On some machines, the hardware determines which register
   this is.  On other machines, you can choose any register you wish for this
   purpose.  If this is not the same register as the frame pointer register,
   then you must mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it (*note Elimination::.).  */
/* #define ARG_POINTER_REGNUM */

/* The register number of the return address pointer register, which is used to
   access the current function's return address from the stack.  On some
   machines, the return address is not at a fixed offset from the frame pointer
   or stack pointer or argument pointer.  This register can be defined to point
   to the return address on the stack, and then be converted by
   `ELIMINABLE_REGS' into either the frame pointer or stack pointer.

   Do not define this macro unless there is no other way to get the return
   address from the stack.  */
/* #define RETURN_ADDRESS_POINTER_REGNUM */

/* Register numbers used for passing a function's static chain pointer.  If
   register windows are used, the register number as seen by the called
   function is `STATIC_CHAIN_INCOMING_REGNUM', while the register number as
   seen by the calling function is `STATIC_CHAIN_REGNUM'.  If these registers
   are the same, `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be defined;
   instead, the next two macros should be defined.  */

#define STATIC_CHAIN_REGNUM (GPR_FIRST + 18)
/* #define STATIC_CHAIN_INCOMING_REGNUM */

/* If the static chain is passed in memory, these macros provide rtx giving
   `mem' expressions that denote where they are stored.  `STATIC_CHAIN' and
   `STATIC_CHAIN_INCOMING' give the locations as seen by the calling and called
   functions, respectively.  Often the former will be at an offset from the
   stack pointer and the latter at an offset from the frame pointer.

   The variables `stack_pointer_rtx', `frame_pointer_rtx', and
   `arg_pointer_rtx' will have been initialized prior to the use of these
   macros and should be used to refer to those items.

   If the static chain is passed in a register, the two previous
   macros should be defined instead.  */
/* #define STATIC_CHAIN */
/* #define STATIC_CHAIN_INCOMING */


/* Eliminating the Frame Pointer and the Arg Pointer */

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and decide
   according to the facts, but on most machines the constant 0 or the constant
   1 suffices.  Use 0 when the machine allows code to be generated with no
   frame pointer, and doing so saves some time or space.  Use 1 when there is
   no possible advantage to avoiding a frame pointer.

   In certain cases, the compiler does not know how to produce valid code
   without a frame pointer.  The compiler recognizes those cases and
   automatically gives the function a frame pointer regardless of what
   `FRAME_POINTER_REQUIRED' says.  You don't need to worry about them.

   In a function that does not require a frame pointer, the frame pointer
   register can be allocated for ordinary usage, unless you mark it as a fixed
   register.  See `FIXED_REGISTERS' for more information.  */
#define FRAME_POINTER_REQUIRED 0

/* A C statement to store in the variable DEPTH-VAR the difference between the
   frame pointer and the stack pointer values immediately after the function
   prologue.  The value would be computed from information such as the result
   of `get_frame_size ()' and the tables of registers `regs_ever_live' and
   `call_used_regs'.

   If `ELIMINABLE_REGS' is defined, this macro will be not be used and need not
   be defined.  Otherwise, it must be defined even if `FRAME_POINTER_REQUIRED'
   is defined to always be true; in that case, you may set DEPTH-VAR to
   anything.  */
/* #define INITIAL_FRAME_POINTER_OFFSET(DEPTH_VAR) */

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.

   On some machines, the position of the argument pointer is not known until
   the compilation is completed.  In such a case, a separate hard register must
   be used for the argument pointer.  This register can be eliminated by
   replacing it with either the frame pointer or the argument pointer,
   depending on whether or not the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack pointer is
   specified first since that is the preferred elimination.  */
#define ELIMINABLE_REGS							\
{									\
  { ARG_POINTER_REGNUM,		STACK_POINTER_REGNUM },			\
  { ARG_POINTER_REGNUM,		FRAME_POINTER_REGNUM },			\
  { FRAME_POINTER_REGNUM,	STACK_POINTER_REGNUM }			\
}

/* A C expression that returns non-zero if the compiler is allowed to try to
   replace register number FROM-REG with register number TO-REG.  This macro
   need only be defined if `ELIMINABLE_REGS' is defined, and will usually be
   the constant 1, since most of the cases preventing register elimination are
   things that the compiler already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM		\
  ? ! frame_pointer_needed						\
  : 1)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  d30v_stack_t *info = d30v_stack_info ();				\
									\
  if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = 0;							\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
    (OFFSET) = info->total_size - current_function_pretend_args_size;	\
  else									\
    abort ();								\
}


/* Passing Function Arguments on the Stack */

/* Define this macro if an argument declared in a prototype as an integral type
   smaller than `int' should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for better code
   on certain machines.  */
/* #define PROMOTE_PROTOTYPES */

/* A C expression that is the number of bytes actually pushed onto the stack
   when an instruction attempts to push NPUSHED bytes.

   If the target machine does not have a push instruction, do not define this
   macro.  That directs GNU CC to use an alternate strategy: to allocate the
   entire argument block and then store the arguments into it.

   On some machines, the definition

        #define PUSH_ROUNDING(BYTES) (BYTES)

   will suffice.  But on other machines, instructions that appear to push one
   byte actually push two bytes in an attempt to maintain alignment.  Then the
   definition should be

        #define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)  */
/* #define PUSH_ROUNDING(NPUSHED) */

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the
   stack frame size by this amount.

   Defining both `PUSH_ROUNDING' and `ACCUMULATE_OUTGOING_ARGS' is not
   proper.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define this macro if functions should assume that stack space has been
   allocated for arguments even when their values are passed in registers.

   The value of this macro is the size, in bytes, of the area reserved for
   arguments passed in registers for the function represented by FNDECL.

   This space can be allocated by the caller, or be a part of the
   machine-dependent stack frame: `OUTGOING_REG_PARM_STACK_SPACE' says
   which.  */
/* #define REG_PARM_STACK_SPACE(FNDECL) */

/* Define these macros in addition to the one above if functions might allocate
   stack space for arguments even when their values are passed in registers.
   These should be used when the stack space allocated for arguments in
   registers is not a simple constant independent of the function declaration.

   The value of the first macro is the size, in bytes, of the area that we
   should initially assume would be reserved for arguments passed in registers.

   The value of the second macro is the actual size, in bytes, of the area that
   will be reserved for arguments passed in registers.  This takes two
   arguments: an integer representing the number of bytes of fixed sized
   arguments on the stack, and a tree representing the number of bytes of
   variable sized arguments on the stack.

   When these macros are defined, `REG_PARM_STACK_SPACE' will only be called
   for libcall functions, the current function, or for a function being called
   when it is known that such stack space must be allocated.  In each case this
   value can be easily computed.

   When deciding whether a called function needs such stack space, and how much
   space to reserve, GNU CC uses these two macros instead of
   `REG_PARM_STACK_SPACE'.  */
/* #define MAYBE_REG_PARM_STACK_SPACE */
/* #define FINAL_REG_PARM_STACK_SPACE(CONST_SIZE, VAR_SIZE) */

/* Define this if it is the responsibility of the caller to allocate the area
   reserved for arguments passed in registers.

   If `ACCUMULATE_OUTGOING_ARGS' is defined, this macro controls whether the
   space for these arguments counts in the value of
   `current_function_outgoing_args_size'.  */
/* #define OUTGOING_REG_PARM_STACK_SPACE */

/* Define this macro if `REG_PARM_STACK_SPACE' is defined, but the stack
   parameters don't skip the area specified by it.

   Normally, when a parameter is not passed in registers, it is placed on the
   stack beyond the `REG_PARM_STACK_SPACE' area.  Defining this macro
   suppresses this behavior and causes the parameter to be passed on the stack
   in its natural location.  */
/* #define STACK_PARMS_IN_REG_PARM_AREA */

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.

   FUNDECL is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_DECL' that
   describes the declaration of the function.  From this it is possible to
   obtain the DECL_ATTRIBUTES of the function.

   FUNTYPE is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_TYPE' that
   describes the data type of the function.  From this it is possible to obtain
   the data types of the value and arguments (if known).

   When a call to a library function is being considered, FUNTYPE will contain
   an identifier node for the library function.  Thus, if you need to
   distinguish among various library functions, you can do so by their names.
   Note that "library function" in this context means a function used to
   perform arithmetic, whose name is known specially in the compiler and was
   not mentioned in the C code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the stack.  If a
   variable number of bytes is passed, it is zero, and argument popping will
   always be the responsibility of the calling function.

   On the VAX, all functions always pop their arguments, so the definition of
   this macro is STACK-SIZE.  On the 68000, using the standard calling
   convention, no functions pop their arguments, so the value of the macro is
   always 0 in this case.  But an alternative calling convention is available
   in which functions that take a fixed number of arguments pop them but other
   functions (such as `printf') pop nothing (the caller pops all).  When this
   convention is in use, FUNTYPE is examined to determine whether a function
   takes a fixed number of arguments.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0


/* Function Arguments in Registers */

/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The arguments are CUM, which summarizes all the previous arguments; MODE,
   the machine mode of the argument; TYPE, the data type of the argument as a
   tree node or 0 if that is not known (which happens for C support library
   functions); and NAMED, which is 1 for an ordinary argument and 0 for
   nameless arguments that correspond to `...' in the called function's
   prototype.

   The value of the expression should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the argument on the
   stack.

   For machines like the VAX and 68000, where normally all arguments are
   pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine where
   some arguments are usually passed in registers, is to cause nameless
   arguments to be passed on the stack instead.  This is done by making
   `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the definition of
   this macro to determine if this argument is of a type that must be passed in
   the stack.  If `REG_PARM_STACK_SPACE' is not defined and `FUNCTION_ARG'
   returns non-zero for such an argument, the compiler will abort.  If
   `REG_PARM_STACK_SPACE' is defined, the argument will be computed in the
   stack and then loaded into a register.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg (&CUM, (int)MODE, TYPE, NAMED, FALSE)

/* Define this macro if the target machine has "register windows", so that the
   register in which a function sees an arguments is not necessarily the same
   as the one in which the caller passed the argument.

   For such machines, `FUNCTION_ARG' computes the register in which the caller
   passes the value, and `FUNCTION_INCOMING_ARG' should be defined in a similar
   fashion to tell the function being called where the arguments will arrive.

   If `FUNCTION_INCOMING_ARG' is not defined, `FUNCTION_ARG' serves both
   purposes.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg (&CUM, (int)MODE, TYPE, NAMED, TRUE)

/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory.  On these machines, typically the first N words of
   arguments are passed in registers, and the rest on the stack.  If a
   multi-word argument (a `double' or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be pushed.
   This macro tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register to be
   used by the caller for this argument; likewise `FUNCTION_INCOMING_ARG', for
   the called function.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg_partial_nregs (&CUM, (int)MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be
        #define FUNCTION_ARG_PASS_BY_REFERENCE\
        (CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) 0

/* If defined, a C expression that indicates when it is the called function's
   responsibility to make a copy of arguments passed by invisible reference.
   Normally, the caller makes a copy and passes the address of the copy to the
   routine being called.  When FUNCTION_ARG_CALLEE_COPIES is defined and is
   nonzero, the caller does not make a copy.  Instead, it passes a pointer to
   the "live" value.  The called function must not modify this value.  If it
   can be determined that the value won't be modified, it need not make a copy;
   otherwise a copy must be made.  */
/* #define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) */

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  */
typedef int CUMULATIVE_ARGS;

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  d30v_init_cumulative_args (&CUM, FNTYPE, LIBNAME, INDIRECT, FALSE)

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.

   The value passed for LIBNAME is always 0, since library routines with
   special calling conventions are never compiled with GNU CC.  The argument
   LIBNAME exists for symmetry with `INIT_CUMULATIVE_ARGS'.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
  d30v_init_cumulative_args (&CUM, FNTYPE, LIBNAME, FALSE, TRUE)

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg_advance (&CUM, (int) MODE, TYPE, NAMED)

/* If defined, a C expression which determines whether, and in which direction,
   to pad out an argument with extra space.  The value should be of type `enum
   direction': either `upward' to pad above the argument, `downward' to pad
   below, or `none' to inhibit padding.

   The *amount* of padding is always just enough to reach the next multiple of
   `FUNCTION_ARG_BOUNDARY'; this macro does not control it.

   This macro has a default definition which is right for most systems.  For
   little-endian machines, the default is to pad upward.  For big-endian
   machines, the default is to pad downward for an argument of constant size
   shorter than an `int', and upward otherwise.  */
/* #define FUNCTION_ARG_PADDING(MODE, TYPE) */

/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  d30v_function_arg_boundary ((int) MODE, TYPE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */

#define FUNCTION_ARG_REGNO_P(REGNO) \
  IN_RANGE_P (REGNO, GPR_ARG_FIRST, GPR_ARG_LAST)


/* How Scalar Function Values are Returned */

/* Define this macro if `-traditional' should not cause functions declared to
   return `float' to convert the value to `double'.  */ /* #define
   TRADITIONAL_RETURN_FLOAT */

/* A C expression to create an RTX representing the place where a function
   returns a value of data type VALTYPE.  VALTYPE is a tree node representing a
   data type.  Write `TYPE_MODE (VALTYPE)' to get the machine mode used to
   represent that type.  On many machines, only the mode is relevant.
   (Actually, on most machines, scalar values are returned in the same place
   regardless of mode).

   If `PROMOTE_FUNCTION_RETURN' is defined, you must apply the same promotion
   rules specified in `PROMOTE_MODE' if VALTYPE is a scalar type.

   If the precise function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This makes it
   possible to use a different value-returning convention for specific
   functions when all their calls are known.

   `FUNCTION_VALUE' is not used for return vales with aggregate data types,
   because these are returned in another way.  See `STRUCT_VALUE_REGNUM' and
   related macros, below.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx (REG, TYPE_MODE (VALTYPE), GPR_RET_VALUE)

/* Define this macro if the target machine has "register windows" so that the
   register in which a function returns its value is not the same as the one in
   which the caller sees the value.

   For such machines, `FUNCTION_VALUE' computes the register in which the
   caller will see the value.  `FUNCTION_OUTGOING_VALUE' should be defined in a
   similar fashion to tell the function where to put the value.

   If `FUNCTION_OUTGOING_VALUE' is not defined, `FUNCTION_VALUE' serves both
   purposes.

   `FUNCTION_OUTGOING_VALUE' is not used for return vales with aggregate data
   types, because these are returned in another way.  See `STRUCT_VALUE_REGNUM'
   and related macros, below.  */
/* #define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC) */

/* A C expression to create an RTX representing the place where a library
   function returns a value of mode MODE.  If the precise function being called
   is known, FUNC is a tree node (`FUNCTION_DECL') for it; otherwise, FUNC is a
   null pointer.  This makes it possible to use a different value-returning
   convention for specific functions when all their calls are known.

   Note that "library function" in this context means a compiler support
   routine, used to perform arithmetic, whose name is known specially by the
   compiler and was not mentioned in the C code being compiled.

   The definition of `LIBRARY_VALUE' need not be concerned aggregate data
   types, because none of the library functions returns such types.  */

#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, GPR_RET_VALUE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back.

   A register whose use for returning values is limited to serving as the
   second of a pair (for a value of type `double', say) need not be recognized
   by this macro.  So for most machines, this definition suffices:

        #define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

   If the machine has register windows, so that the caller and the called
   function use different registers for the return value, this macro should
   recognize only the caller's register numbers.  */

#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == GPR_RET_VALUE)

/* Define this macro if `untyped_call' and `untyped_return' need more space
   than is implied by `FUNCTION_VALUE_REGNO_P' for saving and restoring an
   arbitrary return value.  */
/* #define APPLY_RESULT_SIZE */


/* How Large Values are Returned */

/* A C expression which can inhibit the returning of certain function values in
   registers, based on the type of value.  A nonzero value says to return the
   function value in memory, just as large structures are always returned.
   Here TYPE will be a C expression of type `tree', representing the data type
   of the value.

   Note that values of mode `BLKmode' must be explicitly handled by this macro.
   Also, the option `-fpcc-struct-return' takes effect regardless of this
   macro.  On most systems, it is possible to leave the macro undefined; this
   causes a default definition to be used, whose value is the constant 1 for
   `BLKmode' values, and 0 otherwise.

   Do not use this macro to indicate that structures and unions should always
   be returned in memory.  You should instead use `DEFAULT_PCC_STRUCT_RETURN'
   to indicate this.  */
/* #define RETURN_IN_MEMORY(TYPE) */

/* Define this macro to be 1 if all structure and union return values must be
   in memory.  Since this results in slower code, this should be defined only
   if needed for compatibility with other compilers or with an ABI.  If you
   define this macro to be 0, then the conventions used for structure and union
   return values are decided by the `RETURN_IN_MEMORY' macro.

   If not defined, this defaults to the value 1.  */
/* #define DEFAULT_PCC_STRUCT_RETURN */

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */

#define STRUCT_VALUE_REGNUM GPR_ARG_FIRST

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place where the
   address is passed.  If it returns 0, the address is passed as an "invisible"
   first argument.  */

#define STRUCT_VALUE 0

/* On some architectures the place where the structure value address is found
   by the called function is not the same place that the caller put it.  This
   can be due to register windows, or it could be because the function prologue
   moves it to a different place.

   If the incoming location of the structure value address is in a register,
   define this macro as the register number.  */
/* #define STRUCT_VALUE_INCOMING_REGNUM */

/* If the incoming location is not a register, then you should define
   `STRUCT_VALUE_INCOMING' as an expression for an RTX for where the called
   function should find the value.  If it should find the value on the stack,
   define this to create a `mem' which refers to the frame pointer.  A
   definition of 0 means that the address is passed as an "invisible" first
   argument.  */
/* #define STRUCT_VALUE_INCOMING */

/* Define this macro if the usual system convention on the target machine for
   returning structures and unions is for the called function to return the
   address of a static variable containing the value.

   Do not define this if the usual system convention is for the caller to pass
   an address to the subroutine.

   This macro has effect in `-fpcc-struct-return' mode, but it does nothing
   when you use `-freg-struct-return' mode.  */
/* #define PCC_STATIC_STRUCT_RETURN */


/* Caller-Saves Register Allocation */

/* Define this macro if function calls on the target machine do not preserve
   any registers; in other words, if `CALL_USED_REGISTERS' has 1 for all
   registers.  This macro enables `-fcaller-saves' by default.  Eventually that
   option will be enabled by default on all machines and both the option and
   this macro will be eliminated.  */
/* #define DEFAULT_CALLER_SAVES */

/* A C expression to determine whether it is worthwhile to consider placing a
   pseudo-register in a call-clobbered hard register and saving and restoring
   it around each function call.  The expression should be 1 when this is worth
   doing, and 0 otherwise.

   If you don't define this macro, a default is used which is good on most
   machines: `4 * CALLS < REFS'.  */
/* #define CALLER_SAVE_PROFITABLE(REFS, CALLS) */


/* #define EXIT_IGNORE_STACK */

/* Define this macro as a C expression that is nonzero for registers
   are used by the epilogue or the `return' pattern.  The stack and
   frame pointer registers are already be assumed to be used as
   needed.  */
#define EPILOGUE_USES(REGNO)  ((REGNO) == GPR_LINK)

/* Define this macro if the function epilogue contains delay slots to which
   instructions from the rest of the function can be "moved".  The definition
   should be a C expression whose value is an integer representing the number
   of delay slots there.  */
/* #define DELAY_SLOTS_FOR_EPILOGUE */

/* A C expression that returns 1 if INSN can be placed in delay slot number N
   of the epilogue.

   The argument N is an integer which identifies the delay slot now being
   considered (since different slots may have different rules of eligibility).
   It is never negative and is always less than the number of epilogue delay
   slots (what `DELAY_SLOTS_FOR_EPILOGUE' returns).  If you reject a particular
   insn for a given delay slot, in principle, it may be reconsidered for a
   subsequent delay slot.  Also, other insns may (at least in principle) be
   considered for the so far unfilled delay slot.

   The insns accepted to fill the epilogue delay slots are put in an
   RTL list made with `insn_list' objects, stored in the variable
   `current_function_epilogue_delay_list'.  The insn for the first
   delay slot comes first in the list.  Your definition of the function
   output_function_epilogue() should fill the delay slots by outputting the
   insns in this list, usually by calling `final_scan_insn'.

   You need not define this macro if you did not define
   `DELAY_SLOTS_FOR_EPILOGUE'.  */
/* #define ELIGIBLE_FOR_EPILOGUE_DELAY(INSN, N) */

/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  The
   thunk acts as a wrapper around a virtual function, adjusting the implicit
   object parameter before handing control off to the real function.

   First, emit code to add the integer DELTA to the location that contains the
   incoming first argument.  Assume that this argument contains a pointer, and
   is the one used to pass the `this' pointer in C++.  This is the incoming
   argument *before* the function prologue, e.g. `%o0' on a sparc.  The
   addition must preserve the values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
   the return address.  Hence returning from FUNCTION will return to whoever
   called the current `thunk'.

   The effect must be as if FUNCTION had been called directly with the
   adjusted first argument.  This macro is responsible for emitting
   all of the code for a thunk function; output_function_prologue()
   and output_function_epilogue() are not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
   extracted from it.)  It might possibly be useful on some targets, but
   probably not.

   If you do not define this macro, the target-independent code in the C++
   frontend will generate a less efficient heavyweight thunk that calls
   FUNCTION instead of jumping to it.  The generic approach does not support
   varargs.  */
/* #define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION) */

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct machine_function
{
  /* Additionsl stack adjustment in __builtin_eh_throw.  */
  struct rtx_def * eh_epilogue_sp_ofs;
} machine_function;


/* Generating Code for Profiling.  */

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.  */

#define FUNCTION_PROFILER(FILE, LABELNO) d30v_function_profiler (FILE, LABELNO)

/* Define this macro if the code for function profiling should come before the
   function prologue.  Normally, the profiling code comes after.  */
/* #define PROFILE_BEFORE_PROLOGUE */


/* Implementing the Varargs Macros.  */

/* If defined, is a C expression that produces the machine-specific code for a
   call to `__builtin_saveregs'.  This code will be moved to the very beginning
   of the function, before any parameter access are made.  The return value of
   this function should be an RTX that contains the value to use as the return
   of `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary call to
   the library function `__builtin_saveregs'.  */

#define EXPAND_BUILTIN_SAVEREGS() d30v_expand_builtin_saveregs ()

/* This macro offers an alternative to using `__builtin_saveregs' and defining
   the macro `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have been
   passed consecutively on the stack.  Once this is done, you can use the
   standard implementation of varargs that works for machines that pass all
   their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure, containing
   the values that obtain after processing of the named arguments.  The
   arguments MODE and TYPE describe the last named argument--its machine mode
   and its data type as a tree node.

   The macro implementation should do two things: first, push onto the stack
   all the argument registers *not* used for the named arguments, and second,
   store the size of the data thus pushed into the `int'-valued variable whose
   name is supplied as the argument PRETEND_ARGS_SIZE.  The value that you
   store here will serve as additional offset for setting up the stack frame.

   Because you must generate code to push the anonymous arguments at compile
   time without knowing their data types, `SETUP_INCOMING_VARARGS' is only
   useful on machines that have just a single category of argument register and
   use it uniformly for all data types.

   If the argument SECOND_TIME is nonzero, it means that the arguments of the
   function are being analyzed for the second time.  This happens for an inline
   function, which is not actually compiled until the end of the source file.
   The macro `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */

#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_ARGS_SIZE, SECOND_TIME) \
  d30v_setup_incoming_varargs (&ARGS_SO_FAR, (int) MODE, TYPE,		\
			       &PRETEND_ARGS_SIZE, SECOND_TIME)

/* Define this macro if the location where a function argument is passed
   depends on whether or not it is a named argument.

   This macro controls how the NAMED argument to `FUNCTION_ARG' is set for
   varargs and stdarg functions.  With this macro defined, the NAMED argument
   is always true for named arguments, and false for unnamed arguments.  If
   this is not defined, but `SETUP_INCOMING_VARARGS' is defined, then all
   arguments are treated as named.  Otherwise, all named arguments except the
   last are treated as named.  */
/* #define STRICT_ARGUMENT_NAMING */

/* Build up the stdarg/varargs va_list type tree, assinging it to NODE.  If not
   defined, it is assumed that va_list is a void * pointer.  */

#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = d30v_build_va_list ()


/* Implement the stdarg/varargs va_start macro.  STDARG_P is non-zero if this
   is stdarg.h instead of varargs.h.  VALIST is the tree of the va_list
   variable to initialize.  NEXTARG is the machine independent notion of the
   'next' argument after the variable arguments.  If not defined, a standard
   implementation will be defined that works for arguments passed on the stack.  */

#define EXPAND_BUILTIN_VA_START(STDARG_P, VALIST, NEXTARG)		\
(d30v_expand_builtin_va_start(STDARG_P, VALIST, NEXTARG))

/* Implement the stdarg/varargs va_arg macro.  VALIST is the variable of type
   va_list as a tree, TYPE is the type passed to va_arg.  */

#define EXPAND_BUILTIN_VA_ARG(VALIST, TYPE)				\
(d30v_expand_builtin_va_arg (VALIST, TYPE))

/* Implement the stdarg/varargs va_end macro.
   VALIST is the variable of type va_list as a tree.  */

/* #define EXPAND_BUILTIN_VA_END(VALIST) */



/* Trampolines for Nested Functions.  */

/* A C statement to output, on the stream FILE, assembler code for a block of
   data that contains the constant parts of a trampoline.  This code should not
   include a label--the label is taken care of automatically.  */
/* #define TRAMPOLINE_TEMPLATE(FILE) d30v_trampoline_template (FILE) */

/* The name of a subroutine to switch to the section in which the trampoline
   template is to be placed (*note Sections::.).  The default is a value of
   `readonly_data_section', which places the trampoline in the section
   containing read-only data.  */
/* #define TRAMPOLINE_SECTION */

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE (d30v_trampoline_size ())

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT' is used for
   aligning trampolines.  */
#define TRAMPOLINE_ALIGNMENT 64

/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
  d30v_initialize_trampoline (ADDR, FNADDR, STATIC_CHAIN)

/* A C expression to allocate run-time space for a trampoline.  The expression
   value should be an RTX representing a memory reference to the space for the
   trampoline.

   If this macro is not defined, by default the trampoline is allocated as a
   stack slot.  This default is right for most machines.  The exceptions are
   machines where it is impossible to execute instructions in the stack area.
   On such machines, you may have to implement a separate stack, using this
   macro in conjunction with output_function_prologue () and
   output_function_epilogue ().

   FP points to a data structure, a `struct function', which describes the
   compilation status of the immediate containing function of the function
   which the trampoline is for.  Normally (when `ALLOCATE_TRAMPOLINE' is not
   defined), the stack slot for the trampoline is in the stack frame of this
   containing function.  Other allocation strategies probably must do something
   analogous with this information.  */
/* #define ALLOCATE_TRAMPOLINE(FP) */

/* Implementing trampolines is difficult on many machines because they have
   separate instruction and data caches.  Writing into a stack location fails
   to clear the memory in the instruction cache, so when the program jumps to
   that location, it executes the old contents.

   Here are two possible solutions.  One is to clear the relevant parts of the
   instruction cache whenever a trampoline is set up.  The other is to make all
   trampolines identical, by having them jump to a standard subroutine.  The
   former technique makes trampoline execution faster; the latter makes
   initialization faster.

   To clear the instruction cache when a trampoline is initialized, define the
   following macros which describe the shape of the cache.  */

/* The total size in bytes of the cache.  */
/* #define INSN_CACHE_SIZE */

/* The length in bytes of each cache line.  The cache is divided into cache
   lines which are disjoint slots, each holding a contiguous chunk of data
   fetched from memory.  Each time data is brought into the cache, an entire
   line is read at once.  The data loaded into a cache line is always aligned
   on a boundary equal to the line size.  */
/* #define INSN_CACHE_LINE_WIDTH */

/* The number of alternative cache lines that can hold any particular memory
   location.  */
/* #define INSN_CACHE_DEPTH */

/* Alternatively, if the machine has system calls or instructions to clear the
   instruction cache directly, you can define the following macro.  */

/* If defined, expands to a C expression clearing the *instruction cache* in
   the specified interval.  If it is not defined, and the macro INSN_CACHE_SIZE
   is defined, some generic code is generated to clear the cache.  The
   definition of this macro would typically be a series of `asm' statements.
   Both BEG and END are both pointer expressions.  */
/* #define CLEAR_INSN_CACHE (BEG, END) */

/* To use a standard subroutine, define the following macro.  In addition, you
   must make sure that the instructions in a trampoline fill an entire cache
   line with identical instructions, or else ensure that the beginning of the
   trampoline code is always aligned at the same point in its cache line.  Look
   in `m68k.h' as a guide.  */

/* Define this macro if trampolines need a special subroutine to do their work.
   The macro should expand to a series of `asm' statements which will be
   compiled with GNU CC.  They go in a library function named
   `__transfer_from_trampoline'.

   If you need to avoid executing the ordinary prologue code of a compiled C
   function when you jump to the subroutine, you can do so by placing a special
   label of your own in the assembler code.  Use one `asm' statement to
   generate an assembler label, and another to make the label global.  Then
   trampolines can use that label to jump directly to your special assembler
   code.  */
/* #define TRANSFER_FROM_TRAMPOLINE */


/* Implicit Calls to Library Routines */

/* A C string constant giving the name of the function to call for
   multiplication of one signed full-word by another.  If you do not define
   this macro, the default name is used, which is `__mulsi3', a function
   defined in `libgcc.a'.  */
/* #define MULSI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one signed full-word by another.  If you do not define this macro, the
   default name is used, which is `__divsi3', a function defined in `libgcc.a'.  */
/* #define DIVSI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one unsigned full-word by another.  If you do not define this macro, the
   default name is used, which is `__udivsi3', a function defined in
   `libgcc.a'.  */
/* #define UDIVSI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one signed full-word by another.  If you do not
   define this macro, the default name is used, which is `__modsi3', a function
   defined in `libgcc.a'.  */
/* #define MODSI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one unsigned full-word by another.  If you do not
   define this macro, the default name is used, which is `__umodsi3', a
   function defined in `libgcc.a'.  */
/* #define UMODSI3_LIBCALL */

/* A C string constant giving the name of the function to call for
   multiplication of one signed double-word by another.  If you do not define
   this macro, the default name is used, which is `__muldi3', a function
   defined in `libgcc.a'.  */
/* #define MULDI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one signed double-word by another.  If you do not define this macro, the
   default name is used, which is `__divdi3', a function defined in `libgcc.a'.  */
/* #define DIVDI3_LIBCALL */

/* A C string constant giving the name of the function to call for division of
   one unsigned full-word by another.  If you do not define this macro, the
   default name is used, which is `__udivdi3', a function defined in
   `libgcc.a'.  */
/* #define UDIVDI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one signed double-word by another.  If you do not
   define this macro, the default name is used, which is `__moddi3', a function
   defined in `libgcc.a'.  */
/* #define MODDI3_LIBCALL */

/* A C string constant giving the name of the function to call for the
   remainder in division of one unsigned full-word by another.  If you do not
   define this macro, the default name is used, which is `__umoddi3', a
   function defined in `libgcc.a'.  */
/* #define UMODDI3_LIBCALL */

/* Define this macro as a C statement that declares additional library routines
   renames existing ones. `init_optabs' calls this macro after initializing all
   the normal library routines.  */
/* #define INIT_TARGET_OPTABS */

/* The value of `EDOM' on the target machine, as a C integer constant
   expression.  If you don't define this macro, GNU CC does not attempt to
   deposit the value of `EDOM' into `errno' directly.  Look in
   `/usr/include/errno.h' to find the value of `EDOM' on your system.

   If you do not define `TARGET_EDOM', then compiled code reports domain errors
   by calling the library function and letting it report the error.  If
   mathematical functions on your system use `matherr' when there is an error,
   then you should leave `TARGET_EDOM' undefined so that `matherr' is used
   normally.  */
/* #define TARGET_EDOM */

/* Define this macro as a C expression to create an rtl expression that refers
   to the global "variable" `errno'.  (On certain systems, `errno' may not
   actually be a variable.)  If you don't define this macro, a reasonable
   default is used.  */
/* #define GEN_ERRNO_RTX */

/* Define this macro if GNU CC should generate calls to the System V (and ANSI
   C) library functions `memcpy' and `memset' rather than the BSD functions
   `bcopy' and `bzero'.

   Defined in svr4.h.  */
/* #define TARGET_MEM_FUNCTIONS */

/* Define this macro to generate code for Objective C message sending using the
   calling convention of the NeXT system.  This calling convention involves
   passing the object, the selector and the method arguments all at once to the
   method-lookup library function.

   The default calling convention passes just the object and the selector to
   the lookup function, which returns a pointer to the method.  */
/* #define NEXT_OBJC_RUNTIME */


/* Addressing Modes */

/* Define this macro if the machine supports post-increment addressing.  */
#define HAVE_POST_INCREMENT 1

/* Similar for other kinds of addressing.  */
/* #define HAVE_PRE_INCREMENT 0 */
#define HAVE_POST_DECREMENT 1
/* #define HAVE_PRE_DECREMENT 0 */

/* A C expression that is 1 if the RTX X is a constant which is a valid
   address.  On most machines, this can be defined as `CONSTANT_P (X)', but a
   few machines are more restrictive in which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are not
   explicitly known, such as `symbol_ref', `label_ref', and `high' expressions
   and `const' arithmetic expressions, in addition to `const_int' and
   `const_double' expressions.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

/* A number, the maximum number of registers that can appear in a valid memory
   address.  Note that it is up to you to specify a value equal to the maximum
   number that `GO_IF_LEGITIMATE_ADDRESS' would ever accept.  */
#define MAX_REGS_PER_ADDRESS 2

/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.

   It usually pays to define several simpler macros to serve as subroutines for
   this one.  Otherwise it may be too complicated to understand.

   This macro must exist in two variants: a strict variant and a non-strict
   one.  The strict variant is used in the reload pass.  It must be defined so
   that any pseudo-register that has not been allocated a hard register is
   considered a memory reference.  In contexts where some kind of register is
   required, a pseudo-register with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be defined to
   accept all pseudo-registers in every context where some kind of register is
   required.

   Compiler source files that want to use the strict variant of this macro
   define the macro `REG_OK_STRICT'.  You should use an `#ifdef REG_OK_STRICT'
   conditional to define the strict variant in that case and the non-strict
   variant otherwise.

   Subroutines to check for acceptable registers for various purposes (one for
   base registers, one for index registers, and so on) are typically among the
   subroutines used to define `GO_IF_LEGITIMATE_ADDRESS'.  Then only these
   subroutine macros need have two variants; the higher levels of macros may be
   the same whether strict or not.

   Normally, constant addresses which are the sum of a `symbol_ref' and an
   integer are stored inside a `const' RTX to mark them as constant.
   Therefore, there is no need to recognize such sums specifically as
   legitimate addresses.  Normally you would simply recognize any `const' as
   legitimate.

   Usually `PRINT_OPERAND_ADDRESS' is not prepared to handle constant sums that
   are not marked with `const'.  It assumes that a naked `plus' indicates
   indexing.  If so, then you *must* reject such naked constant sums as
   illegitimate addresses, so that none of them will be given to
   `PRINT_OPERAND_ADDRESS'.

   On some machines, whether a symbolic address is legitimate depends on the
   section that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.  When you see a `const', you will have to look
   inside it to find the `symbol_ref' in order to determine the section.  *Note
   Assembler Format::.

   The best way to modify the name string is by adding text to the beginning,
   with suitable punctuation to prevent any ambiguity.  Allocate the new name
   in `saveable_obstack'.  You will have to modify `ASM_OUTPUT_LABELREF' to
   remove and decode the added text and output the name accordingly, and define
   `STRIP_NAME_ENCODING' to access the original name string.

   You can check the information stored here into the `symbol_ref' in the
   definitions of the macros `GO_IF_LEGITIMATE_ADDRESS' and
   `PRINT_OPERAND_ADDRESS'.  */

#ifdef	REG_OK_STRICT
#define REG_OK_STRICT_P 1
#else
#define REG_OK_STRICT_P 0
#endif

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
do {									\
    if (d30v_legitimate_address_p ((int)MODE, X, REG_OK_STRICT_P))	\
      goto ADDR;							\
} while (0)

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  For hard registers, it should always accept those
   which the hardware permits and reject the others.  Whether the macro accepts
   or rejects pseudo registers must be controlled by `REG_OK_STRICT' as
   described above.  This usually requires two variant definitions, of which
   `REG_OK_STRICT' controls the one actually used.  */

#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) (GPR_P (REGNO (X)))
#else
#define REG_OK_FOR_BASE_P(X) (GPR_OR_PSEUDO_P (REGNO (X)))
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.  WIN will be a C statement label
   elsewhere in the code; the macro definition may use

        GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs', and OLDX
   will be the operand that was given to that function to produce X.

   The code generated by this macro should not alter the substructure of X.  If
   it transforms X into a more legitimate form, it should assign X (which will
   always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate address.
   The compiler has standard ways of doing so in all cases.  In fact, it is
   safe for this macro to do nothing.  But often a machine-dependent strategy
   can generate better code.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
do {									\
  rtx y = d30v_legitimize_address (X, OLDX, (int)MODE, REG_OK_STRICT_P); \
  if (y)								\
    {									\
      X = y;								\
      GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);				\
    }									\
} while (0)

/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.

   Autoincrement and autodecrement addresses typically have mode-dependent
   effects because the amount of the increment or decrement is the size of the
   operand being addressed.  Some machines have other mode-dependent addresses.
   Many RISC machines have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)			\
do {									\
  if (d30v_mode_dependent_address_p (ADDR))				\
    goto LABEL;								\
} while (0)								\

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  You can assume that X satisfies
   `CONSTANT_P', so you need not check this.  In fact, `1' is a suitable
   definition for this macro on machines where anything `CONSTANT_P' is valid.  */
#define LEGITIMATE_CONSTANT_P(X) 1


/* Condition Code Status */

/* C code for a data type which is used for declaring the `mdep' component of
   `cc_status'.  It defaults to `int'.

   This macro is not used on machines that do not use `cc0'.  */
/* #define CC_STATUS_MDEP */

/* A C expression to initialize the `mdep' field to "empty".  The default
   definition does nothing, since most machines don't use the field anyway.  If
   you want to use the field, you should probably define this macro to
   initialize it.

   This macro is not used on machines that do not use `cc0'.  */
/* #define CC_STATUS_MDEP_INIT */

/* A C compound statement to set the components of `cc_status' appropriately
   for an insn INSN whose body is EXP.  It is this macro's responsibility to
   recognize insns that set the condition code as a byproduct of other activity
   as well as those that explicitly set `(cc0)'.

   This macro is not used on machines that do not use `cc0'.

   If there are insns that do not set the condition code but do alter other
   machine registers, this macro must check to see whether they invalidate the
   expressions that the condition code is recorded as reflecting.  For example,
   on the 68000, insns that store in address registers do not set the condition
   code, which means that usually `NOTICE_UPDATE_CC' can leave `cc_status'
   unaltered for such insns.  But suppose that the previous insn set the
   condition code based on location `a4@(102)' and the current insn stores a
   new value in `a4'.  Although the condition code is not changed by this, it
   will no longer be true that it reflects the contents of `a4@(102)'.
   Therefore, `NOTICE_UPDATE_CC' must alter `cc_status' in this case to say
   that nothing is known about the condition code value.

   The definition of `NOTICE_UPDATE_CC' must be prepared to deal with the
   results of peephole optimization: insns whose patterns are `parallel' RTXs
   containing various `reg', `mem' or constants which are just the operands.
   The RTL structure of these insns is not sufficient to indicate what the
   insns actually do.  What `NOTICE_UPDATE_CC' should do when it sees one is
   just to run `CC_STATUS_INIT'.

   A possible definition of `NOTICE_UPDATE_CC' is to call a function that looks
   at an attribute (*note Insn Attributes::.) named, for example, `cc'.  This
   avoids having detailed information about patterns in two places, the `md'
   file and in `NOTICE_UPDATE_CC'.  */
/* #define NOTICE_UPDATE_CC(EXP, INSN) */

/* A list of names to be used for additional modes for condition code values in
   registers (*note Jump Patterns::.).  These names are added to `enum
   machine_mode' and all have class `MODE_CC'.  By convention, they should
   start with `CC' and end with `mode'.

   You should only define this macro if your machine does not use `cc0' and
   only if additional modes are required.  */
/* #define EXTRA_CC_MODES */

/* Returns a mode from class `MODE_CC' to be used when comparison operation
   code OP is applied to rtx X and Y.  For example, on the Sparc,
   `SELECT_CC_MODE' is defined as (see *note Jump Patterns::.  for a
   description of the reason for this definition)

        #define SELECT_CC_MODE(OP,X,Y) \
          (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT          \
           ? ((OP == EQ || OP == NE) ? CCFPmode : CCFPEmode)    \
           : ((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS    \
               || GET_CODE (X) == NEG) \
              ? CC_NOOVmode : CCmode))

   You need not define this macro if `EXTRA_CC_MODES' is not defined.  */
/* #define SELECT_CC_MODE(OP, X, Y) */

/* One some machines not all possible comparisons are defined, but you can
   convert an invalid comparison into a valid one.  For example, the Alpha does
   not have a `GT' comparison, but you can use an `LT' comparison instead and
   swap the order of the operands.

   On such machines, define this macro to be a C statement to do any required
   conversions.  CODE is the initial comparison code and OP0 and OP1 are the
   left and right operands of the comparison, respectively.  You should modify
   CODE, OP0, and OP1 as required.

   GNU CC will not assume that the comparison resulting from this macro is
   valid but will see if the resulting insn matches a pattern in the `md' file.

   You need not define this macro if it would never change the comparison code
   or operands.  */
/* #define CANONICALIZE_COMPARISON(CODE, OP0, OP1) */

/* A C expression whose value is one if it is always safe to reverse a
   comparison whose mode is MODE.  If `SELECT_CC_MODE' can ever return MODE for
   a floating-point inequality comparison, then `REVERSIBLE_CC_MODE (MODE)'
   must be zero.

   You need not define this macro if it would always returns zero or if the
   floating-point format is anything other than `IEEE_FLOAT_FORMAT'.  For
   example, here is the definition used on the Sparc, where floating-point
   inequality comparisons are always given `CCFPEmode':

        #define REVERSIBLE_CC_MODE(MODE)  ((MODE) != CCFPEmode)  */
/* #define REVERSIBLE_CC_MODE(MODE) */


/* Describing Relative Costs of Operations */

/* A part of a C `switch' statement that describes the relative costs of
   constant RTL expressions.  It must contain `case' labels for expression
   codes `const_int', `const', `symbol_ref', `label_ref' and `const_double'.
   Each case must ultimately reach a `return' statement to return the relative
   cost of the use of that kind of constant value in an expression.  The cost
   may depend on the precise value of the constant, which is available for
   examination in X, and the rtx code of the expression in which it is
   contained, found in OUTER_CODE.

   CODE is the expression code--redundant, since it can be obtained with
   `GET_CODE (X)'.  */

/* On the d30v, consider operatnds that fit in a short instruction very
   cheap.  However, at this time, it causes cse to generate incorrect
   code, so disable it for now.  */
#if 0
#define CONST_COSTS(X, CODE, OUTER_CODE)				\
  case CONST_INT:							\
    if (IN_RANGE_P (INTVAL (X), 0, 31))					\
      return 0;								\
    else if ((OUTER_CODE) == LEU && (OUTER_CODE) == LTU			\
	     && (OUTER_CODE) == GEU && (OUTER_CODE) == GTU)		\
      return IN_RANGE_P (INTVAL (X), 32, 63) ? 0 : COSTS_N_INSNS (2);	\
    else								\
      return IN_RANGE_P (INTVAL (X), -31, -1) ? 0 : COSTS_N_INSNS (2);	\
  case SYMBOL_REF:							\
  case LABEL_REF:							\
  case CONST:								\
    return COSTS_N_INSNS (2);						\
  case CONST_DOUBLE:							\
    return COSTS_N_INSNS ((GET_MODE (X) == SFmode) ? 2 : 4);
#else
#define CONST_COSTS(X, CODE, OUTER_CODE)
#endif

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.  This can be
   used, for example, to indicate how costly a multiply instruction is.  In
   writing this macro, you can use the construct `COSTS_N_INSNS (N)' to specify
   a cost equal to N fast instructions.  OUTER_CODE is the code of the
   expression in which X is contained.

   This macro is optional; do not define it if the default cost assumptions are
   adequate for the target machine.  */
#define RTX_COSTS(X, CODE, OUTER_CODE)					\
  case MULT:								\
    return COSTS_N_INSNS ((GET_CODE (XEXP (x, 1)) == CONST_INT		\
			   && exact_log2 (INTVAL (XEXP (x, 1))) >= 0)	\
			  ? 1 : 2);

/* An expression giving the cost of an addressing mode that contains ADDRESS.
   If not defined, the cost is computed from the ADDRESS expression and the
   `CONST_COSTS' values.

   For most CISC machines, the default cost is a good approximation of the true
   cost of the addressing mode.  However, on RISC machines, all instructions
   normally have the same length and execution time.  Hence all addresses will
   have equal costs.

   In cases where more than one form of an address is known, the form with the
   lowest cost will be used.  If multiple forms have the same, lowest, cost,
   the one that is the most complex will be used.

   For example, suppose an address that is equal to the sum of a register and a
   constant is used twice in the same basic block.  When this macro is not
   defined, the address will be computed in a register and memory references
   will be indirect through that register.  On machines where the cost of the
   addressing mode containing the sum is no higher than that of a simple
   indirect reference, this will produce an additional instruction and possibly
   require an additional register.  Proper specification of this macro
   eliminates this overhead for such machines.

   Similar use of this macro is made in strength reduction of loops.

   ADDRESS need not be valid as an address.  In such a case, the cost is not
   relevant and can be any value; invalid addresses need not be assigned a
   different cost.

   On machines where an address involving more than one register is as cheap as
   an address computation involving only one register, defining `ADDRESS_COST'
   to reflect this can cause two registers to be live over a region of code
   where only one would have been if `ADDRESS_COST' were not defined in that
   manner.  This effect should be considered in the definition of this macro.
   Equivalent costs should probably only be given to addresses with different
   numbers of registers on machines with lots of registers.

   This macro will normally either not be defined or be defined as a constant.  */
#define ADDRESS_COST(ADDRESS) 0

/* A C expression for the cost of moving data from a register in class FROM to
   one in class TO.  The classes are expressed using the enumeration values
   such as `GENERAL_REGS'.  A value of 4 is the default; other values are
   interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.

   If reload sees an insn consisting of a single `set' between two hard
   registers, and if `REGISTER_MOVE_COST' applied to their classes returns a
   value of 2, reload does not check to ensure that the constraints of the insn
   are met.  Setting a cost of other than 2 will allow reload to verify that
   the constraints are met.  You should do this if the `movM' pattern's
   constraints do not allow such copying.  */

#define REGISTER_MOVE_COST(MODE, FROM, TO)				\
  (((FROM) != GPR_REGS && (FROM) != EVEN_REGS				\
   && (TO) != GPR_REGS && (TO) != EVEN_REGS) ? 4 : 2)

/* A C expression for the cost of moving data of mode M between a register and
   memory.  A value of 2 is the default; this cost is relative to those in
   `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than between two
   registers, you should define this macro to express the relative cost.  */
#define MEMORY_MOVE_COST(M,C,I) 4

/* A C expression for the cost of a branch instruction.  A value of 1 is the
   default; other values are interpreted relative to that.  */

#define BRANCH_COST d30v_branch_cost

#define D30V_DEFAULT_BRANCH_COST 2

/* Values of the -mbranch-cost=n string.  */
extern int d30v_branch_cost;
extern const char *d30v_branch_cost_string;

/* Here are additional macros which do not specify precise relative costs, but
   only that certain actions are more expensive than GNU CC would ordinarily
   expect.  */

/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory, i.e., if such access require more than one instruction or if
   there is no difference in cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by finding
   the smallest containing object; when it is defined, a fullword load will be
   used if alignment permits.  Unless bytes accesses are faster than word
   accesses, using word accesses is preferable since it may eliminate
   subsequent memory access if subsequent accesses occur to other fields in the
   same word of the structure, but to different bytes.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro to be the value 1 if unaligned accesses have a cost many
   times greater than aligned accesses, for example if they are emulated in a
   trap handler.

   When this macro is non-zero, the compiler will act as if `STRICT_ALIGNMENT'
   were non-zero when generating code for block moves.  This can cause
   significantly more instructions to be produced.  Therefore, do not set this
   macro non-zero if unaligned accesses only add a cycle or two to the time for
   a memory access.

   If the value of this macro is always zero, it need not be defined.  */
/* #define SLOW_UNALIGNED_ACCESS */

/* Define this macro to inhibit strength reduction of memory addresses.  (On
   some machines, such strength reduction seems to do harm rather than good.)  */
/* #define DONT_REDUCE_ADDR */

/* The number of scalar move insns which should be generated instead of a
   string move insn or a library call.  Increasing the value will always make
   code faster, but eventually incurs high cost in increased code size.

   If you don't define this, a reasonable default is used.  */
/* #define MOVE_RATIO */

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE

/* Define this macro if it is as good or better for a function to call itself
   with an explicit address than to call an address kept in a register.  */
/* #define NO_RECURSIVE_FUNCTION_CSE */


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  Normally `".text"' is
   right.  */
#define TEXT_SECTION_ASM_OP "\t.text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  Normally
   `".data"' is right.  */
#define DATA_SECTION_ASM_OP "\t.data"

/* if defined, a C expression whose value is a string containing the assembler
   operation to identify the following data as shared data.  If not defined,
   `DATA_SECTION_ASM_OP' will be used.  */
/* #define SHARED_SECTION_ASM_OP */

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP "\t.bss"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global shared data.  If not defined, and
   `BSS_SECTION_ASM_OP' is, the latter will be used.  */
/* #define SHARED_BSS_SECTION_ASM_OP */

/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro on a system with no
   other sections (that GCC needs to use).

   Defined in svr4.h.  */
/* #define EXTRA_SECTIONS */

/* One or more functions to be defined in `varasm.c'.  These functions should
   do jobs analogous to those of `text_section' and `data_section', for your
   additional sections.  Do not define this macro if you do not define
   `EXTRA_SECTIONS'.

   Defined in svr4.h.  */
/* #define EXTRA_SECTION_FUNCTIONS */

/* On most machines, read-only variables, constants, and jump tables are placed
   in the text section.  If this is not the case on your machine, this macro
   should be defined to be the name of a function (either `data_section' or a
   function defined in `EXTRA_SECTIONS') that switches to the section to be
   used for read-only items.

   If these items should be placed in the text section, this macro should not
   be defined.  */
/* #define READONLY_DATA_SECTION */

/* A C statement or statements to switch to the appropriate section for output
   of EXP.  You can assume that EXP is either a `VAR_DECL' node or a constant
   of some sort.  RELOC indicates whether the initial value of EXP requires
   link-time relocations.  Select the section by calling `text_section' or one
   of the alternatives for other sections.

   Do not define this macro if you put all read-only variables and constants in
   the read-only data section (usually the text section).

   Defined in svr4.h.  */
/* #define SELECT_SECTION(EXP, RELOC, ALIGN) */

/* A C statement or statements to switch to the appropriate section for output
   of RTX in mode MODE.  You can assume that RTX is some kind of constant in
   RTL.  The argument MODE is redundant except in the case of a `const_int'
   rtx.  Select the section by calling `text_section' or one of the
   alternatives for other sections.

   Do not define this macro if you put all constants in the read-only data
   section.

   Defined in svr4.h.  */
/* #define SELECT_RTX_SECTION(MODE, RTX, ALIGN) */

/* Define this macro if jump tables (for `tablejump' insns) should be output in
   the text section, along with the assembler instructions.  Otherwise, the
   readonly data section is used.

   This macro is irrelevant if there is no separate readonly data section.  */
/* #define JUMP_TABLES_IN_TEXT_SECTION */

/* Define this macro if references to a symbol must be treated differently
   depending on something about the variable or function named by the symbol
   (such as what section it is in).

   The macro definition, if any, is executed immediately after the rtl for DECL
   has been created and stored in `DECL_RTL (DECL)'.  The value of the rtl will
   be a `mem' whose address is a `symbol_ref'.

   The usual thing for this macro to do is to record a flag in the `symbol_ref'
   (such as `SYMBOL_REF_FLAG') or to store a modified name string in the
   `symbol_ref' (if one bit is not enough information).  */
/* #define ENCODE_SECTION_INFO(DECL) */

/* Decode SYM_NAME and store the real name part in VAR, sans the characters
   that encode section info.  Define this macro if `ENCODE_SECTION_INFO' alters
   the symbol's name string.  */
/* #define STRIP_NAME_ENCODING(VAR, SYM_NAME) */

/* A C statement to build up a unique section name, expressed as a
   STRING_CST node, and assign it to `DECL_SECTION_NAME (DECL)'.
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  If you do not define this macro, GNU CC
   will use the symbol name prefixed by `.' as the section name.

   Defined in svr4.h.  */
/* #define UNIQUE_SECTION(DECL, RELOC) */


/* Position Independent Code.  */

/* The register number of the register used to address a table of static data
   addresses in memory.  In some cases this register is defined by a
   processor's "application binary interface" (ABI).  When this macro is
   defined, RTL is generated for this register once, as with the stack pointer
   and frame pointer registers.  If this macro is not defined, it is up to the
   machine-dependent files to allocate such a register (if necessary).  */
/* #define PIC_OFFSET_TABLE_REGNUM */

/* Define this macro if the register defined by `PIC_OFFSET_TABLE_REGNUM' is
   clobbered by calls.  Do not define this macro if `PIC_OFFSET_TABLE_REGNUM'
   is not defined.  */
/* #define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED */

/* By generating position-independent code, when two different programs (A and
   B) share a common library (libC.a), the text of the library can be shared
   whether or not the library is linked at the same address for both programs.
   In some of these environments, position-independent code requires not only
   the use of different addressing modes, but also special code to enable the
   use of these addressing modes.

   The `FINALIZE_PIC' macro serves as a hook to emit these special codes once
   the function is being compiled into assembly code, but not before.  (It is
   not done before, because in the case of compiling an inline function, it
   would lead to multiple PIC prologues being included in functions which used
   inline functions and were compiled to assembly language.)  */
/* #define FINALIZE_PIC */

/* A C expression that is nonzero if X is a legitimate immediate operand on the
   target machine when generating position independent code.  You can assume
   that X satisfies `CONSTANT_P', so you need not check this.  You can also
   assume FLAG_PIC is true, so you need not check it either.  You need not
   define this macro if all constants (including `SYMBOL_REF') can be immediate
   operands when generating position independent code.  */
/* #define LEGITIMATE_PIC_OPERAND_P(X) */


/* The Overall Framework of an Assembler File.  */

/* A C expression which outputs to the stdio stream STREAM some appropriate
   text to go at the start of an assembler file.

   Normally this macro is defined to output a line containing `#NO_APP', which
   is a comment that has no effect on most assemblers but tells the GNU
   assembler that it can save time by not checking for certain assembler
   constructs.

   On systems that use SDB, it is necessary to output certain commands; see
   `attasm.h'.

   Defined in svr4.h.  */

/* #define ASM_FILE_START(STREAM) \
  output_file_directive ((STREAM), main_input_filename) */

/* A C expression which outputs to the stdio stream STREAM some appropriate
   text to go at the end of an assembler file.

   If this macro is not defined, the default is to output nothing special at
   the end of the file.  Most systems don't require any definition.

   On systems that use SDB, it is necessary to output certain commands; see
   `attasm.h'.

   Defined in svr4.h.  */
/* #define ASM_FILE_END(STREAM) */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at the
   end of the line.  */
#define ASM_COMMENT_START ";"

/* A C string constant for text to be output before each `asm' statement or
   group of consecutive ones.  Normally this is `"#APP"', which is a comment
   that has no effect on most assemblers but tells the GNU assembler that it
   must check the lines that follow for all valid assembler constructs.  */
#define ASM_APP_ON "#APP\n"

/* A C string constant for text to be output after each `asm' statement or
   group of consecutive ones.  Normally this is `"#NO_APP"', which tells the
   GNU assembler to resume making the time-saving assumptions that are valid
   for ordinary compiler output.  */
#define ASM_APP_OFF "#NO_APP\n"

/* A C statement to output COFF information or DWARF debugging information
   which indicates that filename NAME is the current source file to the stdio
   stream STREAM.

   This macro need not be defined if the standard form of output for the file
   format in use is appropriate.  */
/* #define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME) */

/* A C statement to output DBX or SDB debugging information before code for
   line number LINE of the current source file to the stdio stream STREAM.

   This macro need not be defined if the standard form of debugging information
   for the debugger in use is appropriate.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE) */

/* A C statement to output something to the assembler file to handle a `#ident'
   directive containing the text STRING.  If this macro is not defined, nothing
   is output for a `#ident' directive.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_IDENT(STREAM, STRING) */

/* A C statement to output any assembler statements which are required to
   precede any Objective C object definitions or message sending.  The
   statement is executed only when compiling an Objective C program.  */
/* #define OBJC_PROLOGUE */


/* Output of Data.  */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble a string constant containing the LEN bytes at PTR.  PTR will be
   a C expression of type `char *' and LEN a C expression of type `int'.

   If the assembler has a `.ascii' pseudo-op as found in the Berkeley Unix
   assembler, do not define the macro `ASM_OUTPUT_ASCII'.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_ASCII(STREAM, PTR, LEN) */

/* You may define this macro as a C expression.  You should define the
   expression to have a non-zero value if GNU CC should output the
   constant pool for a function before the code for the function, or
   a zero value if GNU CC should output the constant pool after the
   function.  If you do not define this macro, the usual case, GNU CC
   will output the constant pool before the function.  */
/* #define CONSTANT_POOL_BEFORE_FUNCTION */

/* A C statement to output assembler commands to define the start of the
   constant pool for a function.  FUNNAME is a string giving the name of the
   function.  Should the return type of the function be required, it can be
   obtained via FUNDECL.  SIZE is the size, in bytes, of the constant pool that
   will be written immediately after this call.

   If no constant-pool prefix is required, the usual case, this macro need not
   be defined.  */
/* #define ASM_OUTPUT_POOL_PROLOGUE(FILE FUNNAME FUNDECL SIZE) */

/* A C statement (with or without semicolon) to output a constant in the
   constant pool, if it needs special treatment.  (This macro need not do
   anything for RTL expressions that can be output normally.)

   The argument FILE is the standard I/O stream to output the assembler code
   on.  X is the RTL expression for the constant to output, and MODE is the
   machine mode (in case X is a `const_int').  ALIGN is the required alignment
   for the value X; you should output an assembler directive to force this much
   alignment.

   The argument LABELNO is a number to use in an internal label for the address
   of this pool entry.  The definition of this macro is responsible for
   outputting the label definition at the proper place.  Here is how to do
   this:

        ASM_OUTPUT_INTERNAL_LABEL (FILE, "LC", LABELNO);

   When you output a pool entry specially, you should end with a `goto' to the
   label JUMPTO.  This will prevent the same pool entry from being output a
   second time in the usual manner.

   You need not define this macro if it would do nothing.  */
/* #define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, X, MODE, ALIGN, LABELNO, JUMPTO) */

/* Define this macro as a C expression which is nonzero if the constant EXP, of
   type `tree', should be output after the code for a function.  The compiler
   will normally output all constants before the function; you need not define
   this macro if this is OK.  */
/* #define CONSTANT_AFTER_FUNCTION_P(EXP) */

/* A C statement to output assembler commands to at the end of the constant
   pool for a function.  FUNNAME is a string giving the name of the function.
   Should the return type of the function be required, you can obtain it via
   FUNDECL.  SIZE is the size, in bytes, of the constant pool that GNU CC wrote
   immediately before this call.

   If no constant-pool epilogue is required, the usual case, you need not
   define this macro.  */
/* #define ASM_OUTPUT_POOL_EPILOGUE (FILE FUNNAME FUNDECL SIZE) */

/* Define this macro as a C expression which is nonzero if C is used as a
   logical line separator by the assembler.

   If you do not define this macro, the default is that only the character `;'
   is treated as a logical line separator.  */
/* #define IS_ASM_LOGICAL_LINE_SEPARATOR(C) */

/* These macros are provided by `real.h' for writing the definitions of
   `ASM_OUTPUT_DOUBLE' and the like: */

/* These translate X, of type `REAL_VALUE_TYPE', to the target's floating point
   representation, and store its bit pattern in the array of `long int' whose
   address is L.  The number of elements in the output array is determined by
   the size of the desired target floating point data type: 32 bits of it go in
   each `long int' array element.  Each array element holds 32 bits of the
   result, even if `long int' is wider than 32 bits on the host machine.

   The array element values are designed so that you can print them out using
   `fprintf' in the order they should appear in the target machine's memory.  */
/* #define REAL_VALUE_TO_TARGET_SINGLE(X, L) */
/* #define REAL_VALUE_TO_TARGET_DOUBLE(X, L) */
/* #define REAL_VALUE_TO_TARGET_LONG_DOUBLE(X, L) */

/* This macro converts X, of type `REAL_VALUE_TYPE', to a decimal number and
   stores it as a string into STRING.  You must pass, as STRING, the address of
   a long enough block of space to hold the result.

   The argument FORMAT is a `printf'-specification that serves as a suggestion
   for how to format the output string.  */
/* #define REAL_VALUE_TO_DECIMAL(X, FORMAT, STRING) */


/* Output of Uninitialized Variables.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a common-label named NAME whose size is SIZE bytes.
   The variable ROUNDED is the size rounded up to whatever alignment the caller
   wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the name itself;
   before and after that, output the additional assembler syntax for defining
   the name, and a newline.

   This macro controls how the assembler definitions of uninitialized global
   variables are output.  */
/* #define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED) */

/* Like `ASM_OUTPUT_COMMON' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_COMMON', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGNMENT) */

/* Like ASM_OUTPUT_ALIGNED_COMMON except that it takes an additional argument -
   the DECL of the variable to be output, if there is one.  This macro can be
   called with DECL == NULL_TREE.  If you define this macro, it is used in
   place of both ASM_OUTPUT_COMMON and ASM_OUTPUT_ALIGNED_COMMON, and gives you
   more flexibility in handling the destination of the variable.  */
/* #define ASM_OUTPUT_DECL_COMMON (STREAM, DECL, NAME, SIZE, ALIGNMENT) */

/* If defined, it is similar to `ASM_OUTPUT_COMMON', except that it is used
   when NAME is shared.  If not defined, `ASM_OUTPUT_COMMON' will be used.  */
/* #define ASM_OUTPUT_SHARED_COMMON(STREAM, NAME, SIZE, ROUNDED) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of uninitialized global DECL named NAME whose size is
   SIZE bytes.  The variable ROUNDED is the size rounded up to whatever
   alignment the caller wants.

   Try to use function `asm_output_bss' defined in `varasm.c' when defining
   this macro.  If unable, use the expression `assemble_name (STREAM, NAME)' to
   output the name itself; before and after that, output the additional
   assembler syntax for defining the name, and a newline.

   This macro controls how the assembler definitions of uninitialized global
   variables are output.  This macro exists to properly support languages like
   `c++' which do not have `common' data.  However, this macro currently is not
   defined for all targets.  If this macro and `ASM_OUTPUT_ALIGNED_BSS' are not
   defined then `ASM_OUTPUT_COMMON' or `ASM_OUTPUT_ALIGNED_COMMON' or
   `ASM_OUTPUT_DECL_COMMON' is used.  */
/* #define ASM_OUTPUT_BSS(STREAM, DECL, NAME, SIZE, ROUNDED) */

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_BSS', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Try to use function `asm_output_aligned_bss' defined in file `varasm.c' when
   defining this macro.  */
/* #define ASM_OUTPUT_ALIGNED_BSS(STREAM, DECL, NAME, SIZE, ALIGNMENT) */

/* If defined, it is similar to `ASM_OUTPUT_BSS', except that it is used when
   NAME is shared.  If not defined, `ASM_OUTPUT_BSS' will be used.  */
/* #define ASM_OUTPUT_SHARED_BSS(STREAM, DECL, NAME, SIZE, ROUNDED) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a local-common-label named NAME whose size is SIZE
   bytes.  The variable ROUNDED is the size rounded up to whatever alignment
   the caller wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the name itself;
   before and after that, output the additional assembler syntax for defining
   the name, and a newline.

   This macro controls how the assembler definitions of uninitialized static
   variables are output.  */
/* #define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED) */

/* Like `ASM_OUTPUT_LOCAL' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_LOCAL', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGNMENT) */

/* Like `ASM_OUTPUT_ALIGNED_LOCAL' except that it takes an additional
   parameter - the DECL of variable to be output, if there is one.
   This macro can be called with DECL == NULL_TREE.  If you define
   this macro, it is used in place of `ASM_OUTPUT_LOCAL' and
   `ASM_OUTPUT_ALIGNED_LOCAL', and gives you more flexibility in
   handling the destination of the variable.  */
/* #define ASM_OUTPUT_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) */

/* If defined, it is similar to `ASM_OUTPUT_LOCAL', except that it is used when
   NAME is shared.  If not defined, `ASM_OUTPUT_LOCAL' will be used.  */
/* #define ASM_OUTPUT_SHARED_LOCAL (STREAM, NAME, SIZE, ROUNDED) */


/* Output and Generation of Labels.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a label named NAME.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before and after
   that, output the additional assembler syntax for defining the name, and a
   newline.  */

#define ASM_OUTPUT_LABEL(STREAM, NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name NAME of a function which is being defined.
   This macro is responsible for outputting the label definition (perhaps using
   `ASM_OUTPUT_LABEL').  The argument DECL is the `FUNCTION_DECL' tree node
   representing the function.

   If this macro is not defined, then the function name is defined in the usual
   manner as a label (by means of `ASM_OUTPUT_LABEL').

   Defined in svr4.h.  */
/* #define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the size of a function which is being defined.  The
   argument NAME is the name of the function.  The argument DECL is the
   `FUNCTION_DECL' tree node representing the function.

   If this macro is not defined, then the function size is not defined.

   Defined in svr4.h.  */
/* #define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name NAME of an initialized variable which is
   being defined.  This macro must output the label definition (perhaps using
   `ASM_OUTPUT_LABEL').  The argument DECL is the `VAR_DECL' tree node
   representing the variable.

   If this macro is not defined, then the variable name is defined in the usual
   manner as a label (by means of `ASM_OUTPUT_LABEL').

   Defined in svr4.h.  */
/* #define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) */

/* A C statement (sans semicolon) to finish up declaring a variable name once
   the compiler has processed its initializer fully and thus has had a chance
   to determine the size of an array when controlled by an initializer.  This
   is used on systems where it's necessary to declare something about the size
   of the object.

   If you don't define this macro, that is equivalent to defining it to do
   nothing.

   Defined in svr4.h.  */
/* #define ASM_FINISH_DECLARE_OBJECT(STREAM, DECL, TOPLEVEL, ATEND) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM some
   commands that will make the label NAME global; that is, available for
   reference from other files.  Use the expression `assemble_name (STREAM,
   NAME)' to output the name itself; before and after that, output the
   additional assembler syntax for making that name global, and a newline.  */

#define ASM_GLOBALIZE_LABEL(STREAM,NAME)				\
do {									\
  fputs ("\t.globl ", STREAM);						\
  assemble_name (STREAM, NAME);						\
  fputs ("\n", STREAM);							\
} while (0)

/* A C statement (sans semicolon) to output to the stdio stream STREAM some
   commands that will make the label NAME weak; that is, available for
   reference from other files but only used if no other definition is
   available.  Use the expression `assemble_name (STREAM, NAME)' to output the
   name itself; before and after that, output the additional assembler syntax
   for making that name weak, and a newline.

   If you don't define this macro, GNU CC will not support weak symbols and you
   should not define the `SUPPORTS_WEAK' macro.

   Defined in svr4.h.  */
/* #define ASM_WEAKEN_LABEL */

/* A C expression which evaluates to true if the target supports weak symbols.

   If you don't define this macro, `defaults.h' provides a default definition.
   If `ASM_WEAKEN_LABEL' is defined, the default definition is `1'; otherwise,
   it is `0'.  Define this macro if you want to control weak symbol support
   with a compiler flag such as `-melf'.  */
/* #define SUPPORTS_WEAK */

/* A C statement (sans semicolon) to mark DECL to be emitted as a
   public symbol such that extra copies in multiple translation units
   will be discarded by the linker.  Define this macro if your object
   file format provides support for this concept, such as the `COMDAT'
   section flags in the Microsoft Windows PE/COFF format, and this
   support requires changes to DECL, such as putting it in a separate
   section.

   Defined in svr4.h.  */
/* #define MAKE_DECL_ONE_ONLY */

/* A C expression which evaluates to true if the target supports one-only
   semantics.

   If you don't define this macro, `varasm.c' provides a default definition.
   If `MAKE_DECL_ONE_ONLY' is defined, the default definition is `1';
   otherwise, it is `0'.  Define this macro if you want to control one-only
   symbol support with a compiler flag, or if setting the `DECL_ONE_ONLY' flag
   is enough to mark a declaration to be emitted as one-only.  */
/* #define SUPPORTS_ONE_ONLY */

/* A C statement (sans semicolon) to output to the stdio stream STREAM any text
   necessary for declaring the name of an external symbol named NAME which is
   referenced in this compilation but not defined.  The value of DECL is the
   tree node for the declaration.

   This macro need not be defined if it does not need to output anything.  The
   GNU assembler and most Unix assemblers don't require anything.  */
/* #define ASM_OUTPUT_EXTERNAL(STREAM, DECL, NAME) */

/* A C statement (sans semicolon) to output on STREAM an assembler pseudo-op to
   declare a library function name external.  The name of the library function
   is given by SYMREF, which has type `rtx' and is a `symbol_ref'.

   This macro need not be defined if it does not need to output anything.  The
   GNU assembler and most Unix assemblers don't require anything.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_EXTERNAL_LIBCALL(STREAM, SYMREF) */

/* A C statement (sans semicolon) to output to the stdio stream STREAM a
   reference in assembler syntax to a label named NAME.  This should add `_' to
   the front of the name, if that is customary on your operating system, as it
   is in most Berkeley Unix systems.  This macro is used in `assemble_name'.  */
/* #define ASM_OUTPUT_LABELREF(STREAM, NAME) */

/* A C statement to output to the stdio stream STREAM a label whose name is
   made from the string PREFIX and the number NUM.

   It is absolutely essential that these labels be distinct from the labels
   used for user-level functions and variables.  Otherwise, certain programs
   will have name conflicts with internal labels.

   It is desirable to exclude internal labels from the symbol table of the
   object file.  Most assemblers have a naming convention for labels that
   should be excluded; on many systems, the letter `L' at the beginning of a
   label has this effect.  You should find out what convention your system
   uses, and follow it.

   The usual definition of this macro is as follows:

        fprintf (STREAM, "L%s%d:\n", PREFIX, NUM)

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM) */

/* A C statement to store into the string STRING a label whose name is made
   from the string PREFIX and the number NUM.

   This string, when output subsequently by `assemble_name', should produce the
   output that `ASM_OUTPUT_INTERNAL_LABEL' would produce with the same PREFIX
   and NUM.

   If the string begins with `*', then `assemble_name' will output the rest of
   the string unchanged.  It is often convenient for
   `ASM_GENERATE_INTERNAL_LABEL' to use `*' in this way.  If the string doesn't
   start with `*', then `ASM_OUTPUT_LABELREF' gets to output the string, and
   may change it.  (Of course, `ASM_OUTPUT_LABELREF' is also part of your
   machine description, so you should know what it does on your machine.)

   Defined in svr4.h.  */

/*
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  sprintf (LABEL, "*.%s%d", PREFIX, NUM);				\
} while (0)
*/

/* A C expression to assign to OUTVAR (which is a variable of type `char *') a
   newly allocated string made from the string NAME and the number NUMBER, with
   some suitable punctuation added.  Use `alloca' to get space for the string.

   The string will be used as an argument to `ASM_OUTPUT_LABELREF' to produce
   an assembler label for an internal static variable whose name is NAME.
   Therefore, the string must be such as to result in valid assembler code.
   The argument NUMBER is different each time this macro is executed; it
   prevents conflicts between similarly-named internal static variables in
   different scopes.

   Ideally this string should not be a valid C identifier, to prevent any
   conflict with the user's own symbols.  Most assemblers allow periods or
   percent signs in assembler symbols; putting at least one of these between
   the name and the number will suffice.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)			\
do {									\
  (OUTVAR) = (char *) alloca (strlen ((NAME)) + 12);			\
  sprintf ((OUTVAR), "%s.%ld", (NAME), (long)(NUMBER));			\
} while (0)

/* A C statement to output to the stdio stream STREAM assembler code which
   defines (equates) the symbol NAME to have the value VALUE.

   If SET_ASM_OP is defined, a default definition is provided which is correct
   for most systems.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_DEF(STREAM, NAME, VALUE) */

/* A C statement to output to the stdio stream STREAM assembler code which
   defines (equates) the weak symbol NAME to have the value VALUE.

   Define this macro if the target only supports weak aliases; define
   ASM_OUTPUT_DEF instead if possible.  */
/* #define ASM_OUTPUT_WEAK_ALIAS (STREAM, NAME, VALUE) */

/* Define this macro to override the default assembler names used for Objective
   C methods.

   The default name is a unique method number followed by the name of the class
   (e.g. `_1_Foo').  For methods in categories, the name of the category is
   also included in the assembler name (e.g.  `_1_Foo_Bar').

   These names are safe on most systems, but make debugging difficult since the
   method's selector is not present in the name.  Therefore, particular systems
   define other ways of computing names.

   BUF is an expression of type `char *' which gives you a buffer in which to
   store the name; its length is as long as CLASS_NAME, CAT_NAME and SEL_NAME
   put together, plus 50 characters extra.

   The argument IS_INST specifies whether the method is an instance method or a
   class method; CLASS_NAME is the name of the class; CAT_NAME is the name of
   the category (or NULL if the method is not in a category); and SEL_NAME is
   the name of the selector.

   On systems where the assembler can handle quoted names, you can use this
   macro to provide more human-readable names.  */
/* #define OBJC_GEN_METHOD_LABEL(BUF, IS_INST, CLASS_NAME, CAT_NAME, SEL_NAME) */


/* Macros Controlling Initialization Routines.  */

/* If defined, a C string constant for the assembler operation to identify the
   following data as initialization code.  If not defined, GNU CC will assume
   such a section does not exist.  When you are using special sections for
   initialization and termination functions, this macro also controls how
   `crtstuff.c' and `libgcc2.c' arrange to run the initialization functions.

   Defined in svr4.h.  */
/* #define INIT_SECTION_ASM_OP */

/* If defined, `main' will not call `__main' as described above.  This macro
   should be defined for systems that control the contents of the init section
   on a symbol-by-symbol basis, such as OSF/1, and should not be defined
   explicitly for systems that support `INIT_SECTION_ASM_OP'.  */
/* #define HAS_INIT_SECTION */

/* If defined, a C string constant for a switch that tells the linker that the
   following symbol is an initialization routine.  */
/* #define LD_INIT_SWITCH */

/* If defined, a C string constant for a switch that tells the linker that the
   following symbol is a finalization routine.  */
/* #define LD_FINI_SWITCH */

/* If defined, `main' will call `__main' despite the presence of
   `INIT_SECTION_ASM_OP'.  This macro should be defined for systems where the
   init section is not actually run automatically, but is still useful for
   collecting the lists of constructors and destructors.  */
#define INVOKE__main

/* If your system uses `collect2' as the means of processing constructors, then
   that program normally uses `nm' to scan an object file for constructor
   functions to be called.  On certain kinds of systems, you can define these
   macros to make `collect2' work faster (and, in some cases, make it work at
   all): */

/* Define this macro if the system uses COFF (Common Object File Format) object
   files, so that `collect2' can assume this format and scan object files
   directly for dynamic constructor/destructor functions.  */
/* #define OBJECT_FORMAT_COFF */

/* Define this macro if the system uses ROSE format object files, so that
   `collect2' can assume this format and scan object files directly for dynamic
   constructor/destructor functions.

   These macros are effective only in a native compiler; `collect2' as
   part of a cross compiler always uses `nm' for the target machine.  */
/* #define OBJECT_FORMAT_ROSE */

/* Define this macro if the system uses ELF format object files.

   Defined in svr4.h.  */
/* #define OBJECT_FORMAT_ELF */

/* Define this macro as a C string constant containing the file name to use to
   execute `nm'.  The default is to search the path normally for `nm'.

   If your system supports shared libraries and has a program to list the
   dynamic dependencies of a given library or executable, you can define these
   macros to enable support for running initialization and termination
   functions in shared libraries: */
/* #define REAL_NM_FILE_NAME */

/* Define this macro to a C string constant containing the name of the program
   which lists dynamic dependencies, like `"ldd"' under SunOS 4.  */
/* #define LDD_SUFFIX */

/* Define this macro to be C code that extracts filenames from the output of
   the program denoted by `LDD_SUFFIX'.  PTR is a variable of type `char *'
   that points to the beginning of a line of output from `LDD_SUFFIX'.  If the
   line lists a dynamic dependency, the code must advance PTR to the beginning
   of the filename on that line.  Otherwise, it must set PTR to `NULL'.  */
/* #define PARSE_LDD_OUTPUT (PTR) */


/* Output of Assembler Instructions.  */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  This is what translates register numbers
   in the compiler into assembler language.  */
#define REGISTER_NAMES							\
{									\
  "r0",		"r1",		"r2",		"r3",			\
  "r4",		"r5",		"r6",		"r7",			\
  "r8",		"r9",		"r10",		"r11",			\
  "r12",	"r13",		"r14",		"r15",			\
  "r16",	"r17",		"r18",		"r19",			\
  "r20",	"r21",		"r22",		"r23",			\
  "r24",	"r25",		"r26",		"r27",			\
  "r28",	"r29",		"r30",		"r31",			\
  "r32",	"r33",		"r34",		"r35",			\
  "r36",	"r37",		"r38",		"r39",			\
  "r40",	"r41",		"r42",		"r43",			\
  "r44",	"r45",		"r46",		"r47",			\
  "r48",	"r49",		"r50",		"r51",			\
  "r52",	"r53",		"r54",		"r55",			\
  "r56",	"r57",		"r58",		"r59",			\
  "r60",	"r61",		"link",		"sp",			\
  "ap",									\
  "f0",		"f1",		"f2",		"f3",			\
  "s",		"v",		"va",		"c",			\
  "a0",		"a1",							\
  "psw",	"bpsw",		"pc",		"bpc",			\
  "dpsw",	"dpc",		"rpt_c",	"rpt_s",		\
  "rpt_e",	"mod_s",	"mod_e",	"iba",			\
  "eit_vb",	"int_s",	"int_m",				\
}

/* If defined, a C initializer for an array of structures containing a name and
   a register number.  This macro defines additional names for hard registers,
   thus allowing the `asm' option in declarations to refer to registers using
   alternate names.  */
#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {"r62",	GPR_LINK},			\
  {"r63",	GPR_SP},			\
  {"f4",	FLAG_SAT},			\
  {"f5",	FLAG_OVERFLOW},			\
  {"f6",	FLAG_ACC_OVER},			\
  {"f7",	FLAG_CARRY},			\
  {"carry",	FLAG_CARRY},			\
  {"borrow",	FLAG_BORROW},			\
  {"b",		FLAG_BORROW},			\
  {"cr0",	CR_PSW},			\
  {"cr1",	CR_BPSW},			\
  {"cr2",	CR_PC},				\
  {"cr3",	CR_BPC},			\
  {"cr4",	CR_DPSW},			\
  {"cr5",	CR_DPC},			\
  {"cr7",	CR_RPT_C},			\
  {"cr8",	CR_RPT_S},			\
  {"cr9",	CR_RPT_E},			\
  {"cr10",	CR_MOD_S},			\
  {"cr11",	CR_MOD_E},			\
  {"cr14",	CR_IBA},			\
  {"cr15",	CR_EIT_VB},			\
  {"cr16",	CR_INT_S},			\
  {"cr17",	CR_INT_M}			\
}

/* Define this macro if you are using an unusual assembler that requires
   different names for the machine instructions.

   The definition is a C statement or statements which output an assembler
   instruction opcode to the stdio stream STREAM.  The macro-operand PTR is a
   variable of type `char *' which points to the opcode name in its "internal"
   form--the form that is written in the machine description.  The definition
   should output the opcode name to STREAM, performing any translation you
   desire, and increment the variable PTR to point at the end of the opcode so
   that it will not be output twice.

   In fact, your macro definition may process less than the entire opcode name,
   or more than the opcode name; but if you want to process text that includes
   `%'-sequences to substitute operands, you must take care of the substitution
   yourself.  Just be sure to increment PTR over whatever text should not be
   output normally.

   If you need to look at the operand values, they can be found as the elements
   of `recog_data.operand'.

   If the macro definition does nothing, the instruction is output in the usual
   way.  */
/* #define ASM_OUTPUT_OPCODE(STREAM, PTR) */

/* If defined, a C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so they will be
   output differently.

   Here the argument OPVEC is the vector containing the operands extracted from
   INSN, and NOPERANDS is the number of elements of the vector which contain
   meaningful data for this insn.  The contents of this vector are what will be
   used to convert the insn template into assembler code, so you can change the
   assembler output by changing the contents of the vector.

   This macro is useful when various assembler syntaxes share a single file of
   instruction patterns; by defining this macro differently, you can cause a
   large class of instructions to be output differently (such as with
   rearranged operands).  Naturally, variations in assembler syntax affecting
   individual insn patterns ought to be handled by writing conditional output
   routines in those patterns.

   If this macro is not defined, it is equivalent to a null statement.  */
/* #define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) */

/* If defined, `FINAL_PRESCAN_INSN' will be called on each
   `CODE_LABEL'.  In that case, OPVEC will be a null pointer and
   NOPERANDS will be zero.  */
/* #define FINAL_PRESCAN_LABEL */

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand X.  X is an RTL expression.

   CODE is a value that can be used to specify one of several ways of printing
   the operand.  It is used when identical operands must be printed differently
   depending on the context.  CODE comes from the `%' specification that was
   used to request printing of the operand.  If the specification was just
   `%DIGIT' then CODE is 0; if the specification was `%LTR DIGIT' then CODE is
   the ASCII code for LTR.

   If X is a register, this macro should print the register's name.  The names
   can be found in an array `reg_names' whose type is `char *[]'.  `reg_names'
   is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%' followed by
   a punctuation character), this macro is called with a null pointer for X and
   the punctuation character for CODE.

   Standard operand flags that are handled elsewhere:
	`='  Output a number unique to each instruction in the compilation.
	`a'  Substitute an operand as if it were a memory reference.
	`c'  Omit the syntax that indicates an immediate operand.
	`l'  Substitute a LABEL_REF into a jump instruction.
	`n'  Like %cDIGIT, except negate the value before printing.

   The d30v specific operand flags are:
	`.'  Print r0.
	`f'  Print a SF constant as an int.
	`s'  Subtract 32 and negate.
	`A'  Print accumulator number without an `a' in front of it.
	`B'  Print bit offset for BSET, etc. instructions.
	`E'  Print u if this is zero extend, nothing if this is sign extend.
	`F'  Emit /{f,t,x}{f,t,x} for executing a false condition.
	`L'  Print the lower half of a 64 bit item.
	`M'  Print a memory reference for ld/st instructions.
	`R'  Return appropriate cmp instruction for relational test.
	`S'  Subtract 32.
	`T'  Emit /{f,t,x}{f,t,x} for executing a true condition.
	`U'  Print the upper half of a 64 bit item.  */

#define PRINT_OPERAND(STREAM, X, CODE) d30v_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid punctuation
   character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no punctuation
   characters (except for the standard one, `%') are used in this way.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '.' || (CODE) == ':')

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.

   On some machines, the syntax for a symbolic address depends on the section
   that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.  *Note Assembler Format::.  */

#define PRINT_OPERAND_ADDRESS(STREAM, X) d30v_print_operand_address (STREAM, X)

/* A C statement, to be executed after all slot-filler instructions have been
   output.  If necessary, call `dbr_sequence_length' to determine the number of
   slots filled in a sequence (zero if not currently outputting a sequence), to
   decide how many no-ops to output, or whatever.

   Don't define this macro if it has nothing to do, but it is helpful in
   reading assembly output if the extent of the delay sequence is made explicit
   (e.g. with white space).

   Note that output routines for instructions with delay slots must be prepared
   to deal with not being output as part of a sequence (i.e.  when the
   scheduling pass is not run, or when no slot fillers could be found.)  The
   variable `final_sequence' is null when not processing a sequence, otherwise
   it contains the `sequence' rtx being output.  */
/* #define DBR_OUTPUT_SEQEND(FILE) */

/* If defined, C string expressions to be used for the `%R', `%L', `%U', and
   `%I' options of `asm_fprintf' (see `final.c').  These are useful when a
   single `md' file must support multiple assembler formats.  In that case, the
   various `tm.h' files can define these macros differently.

   USER_LABEL_PREFIX is defined in svr4.h.  */

#define REGISTER_PREFIX "%"
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX ""

/* If your target supports multiple dialects of assembler language (such as
   different opcodes), define this macro as a C expression that gives the
   numeric index of the assembler language dialect to use, with zero as the
   first variant.

   If this macro is defined, you may use `{option0|option1|option2...}'
   constructs in the output templates of patterns (*note Output Template::.) or
   in the first argument of `asm_fprintf'.  This construct outputs `option0',
   `option1' or `option2', etc., if the value of `ASSEMBLER_DIALECT' is zero,
   one or two, etc.  Any special characters within these strings retain their
   usual meaning.

   If you do not define this macro, the characters `{', `|' and `}' do not have
   any special meaning when used in templates or operands to `asm_fprintf'.

   Define the macros `REGISTER_PREFIX', `LOCAL_LABEL_PREFIX',
   `USER_LABEL_PREFIX' and `IMMEDIATE_PREFIX' if you can express the variations
   in assemble language syntax with that mechanism.  Define `ASSEMBLER_DIALECT'
   and use the `{option0|option1}' syntax if the syntax variant are larger and
   involve such things as different opcodes or operand order.  */
/* #define ASSEMBLER_DIALECT */

/* A C expression to output to STREAM some assembler code which will push hard
   register number REGNO onto the stack.  The code need not be optimal, since
   this macro is used only when profiling.  */
/* #define ASM_OUTPUT_REG_PUSH (STREAM, REGNO) */

/* A C expression to output to STREAM some assembler code which will pop hard
   register number REGNO off of the stack.  The code need not be optimal, since
   this macro is used only when profiling.  */
/* #define ASM_OUTPUT_REG_POP (STREAM, REGNO) */


/* Output of dispatch tables.  */

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a difference between two labels.
   VALUE and REL are the numbers of two internal labels.  The definitions of
   these labels are output using `ASM_OUTPUT_INTERNAL_LABEL', and they must be
   printed in the same way here.  For example,

        fprintf (STREAM, "\t.word L%d-L%d\n", VALUE, REL)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
fprintf (STREAM, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This macro should be provided on machines where the addresses in a dispatch
   table are absolute.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a reference to a label.  VALUE
   is the number of an internal label whose definition is output using
   `ASM_OUTPUT_INTERNAL_LABEL'.  For example,

        fprintf (STREAM, "\t.word L%d\n", VALUE)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
fprintf (STREAM, "\t.word .L%d\n", VALUE)

/* Define this if the label before a jump-table needs to be output specially.
   The first three arguments are the same as for `ASM_OUTPUT_INTERNAL_LABEL';
   the fourth argument is the jump-table which follows (a `jump_insn'
   containing an `addr_vec' or `addr_diff_vec').

   This feature is used on system V to output a `swbeg' statement for the
   table.

   If this macro is not defined, these labels are output with
   `ASM_OUTPUT_INTERNAL_LABEL'.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, TABLE) */

/* Define this if something special must be output at the end of a jump-table.
   The definition should be a C statement to be executed after the assembler
   code for the table is written.  It should write the appropriate code to
   stdio stream STREAM.  The argument TABLE is the jump-table insn, and NUM is
   the label-number of the preceding label.

   If this macro is not defined, nothing special is output at the end of the
   jump-table.  */
/* #define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE) */


/* Assembler Commands for Exception Regions.  */

/* An rtx used to mask the return address found via RETURN_ADDR_RTX, so that it
   does not contain any extraneous set bits in it.  */
/* #define MASK_RETURN_ADDR */

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling.  Otherwise,
   if your target supports this information (if it defines
   `INCOMING_RETURN_ADDR_RTX'), GCC will provide a default definition of 1.

   If this macro is defined to 1, the DWARF 2 unwinder will be the default
   exception handling mechanism; otherwise, setjmp/longjmp will be used by
   default.

   If this macro is defined to anything, the DWARF 2 unwinder will be used
   instead of inline unwinders and __unwind_function in the non-setjmp case.  */
/* #define DWARF2_UNWIND_INFO */


/* Assembler Commands for Alignment.  */

/* The alignment (log base 2) to put in front of LABEL, which follows
   a BARRIER.

   This macro need not be defined if you don't want any special alignment to be
   done at such a time.  Most machine descriptions do not currently define the
   macro.  */
/* #define LABEL_ALIGN_AFTER_BARRIER(LABEL) */

/* The desired alignment for the location counter at the beginning
   of a loop.

   This macro need not be defined if you don't want any special alignment to be
   done at such a time.  Most machine descriptions do not currently define the
   macro.  */
/* #define LOOP_ALIGN(LABEL) */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to advance the location counter by NBYTES bytes.  Those bytes should be zero
   when loaded.  NBYTES will be a C expression of type `int'.

   Defined in svr4.h.  */
/* #define ASM_OUTPUT_SKIP(STREAM, NBYTES) \
  fprintf (STREAM, "\t.zero\t%u\n", (NBYTES)) */

/* Define this macro if `ASM_OUTPUT_SKIP' should not be used in the text
   section because it fails put zeros in the bytes that are skipped.  This is
   true on many Unix systems, where the pseudo-op to skip bytes produces no-op
   instructions rather than zeros when used in the text section.  */
/* #define ASM_NO_SKIP_IN_TEXT */

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  POWER
   will be a C expression of type `int'.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))


/* Macros Affecting all Debug Formats.  */

/* A C expression that returns the DBX register number for the compiler
   register number REGNO.  In simple cases, the value of this expression may be
   REGNO itself.  But sometimes there are some registers that the compiler
   knows about and DBX does not, or vice versa.  In such cases, some register
   may need to have one number in the compiler and another for DBX.

   If two registers have consecutive numbers inside GNU CC, and they can be
   used as a pair to hold a multiword value, then they *must* have consecutive
   numbers after renumbering with `DBX_REGISTER_NUMBER'.  Otherwise, debuggers
   will be unable to access such a pair, because they expect register pairs to
   be consecutive in their own numbering scheme.

   If you find yourself defining `DBX_REGISTER_NUMBER' in way that does not
   preserve register pairs, then what you must do instead is redefine the
   actual register numbering scheme.  */
#define DBX_REGISTER_NUMBER(REGNO)					\
(GPR_P (REGNO)			 ? ((REGNO) - GPR_FIRST)		\
 : ACCUM_P (REGNO)		 ? ((REGNO) - ACCUM_FIRST + 84)		\
 : FLAG_P (REGNO)		 ? 66 /* return psw for all flags */	\
 : (REGNO) == ARG_POINTER_REGNUM ? (GPR_SP - GPR_FIRST)			\
 : (REGNO) == CR_PSW		 ? (66 + 0)				\
 : (REGNO) == CR_BPSW		 ? (66 + 1)				\
 : (REGNO) == CR_PC		 ? (66 + 2)				\
 : (REGNO) == CR_BPC		 ? (66 + 3)				\
 : (REGNO) == CR_DPSW		 ? (66 + 4)				\
 : (REGNO) == CR_DPC		 ? (66 + 5)				\
 : (REGNO) == CR_RPT_C		 ? (66 + 7)				\
 : (REGNO) == CR_RPT_S		 ? (66 + 8)				\
 : (REGNO) == CR_RPT_E		 ? (66 + 9)				\
 : (REGNO) == CR_MOD_S		 ? (66 + 10)				\
 : (REGNO) == CR_MOD_E		 ? (66 + 11)				\
 : (REGNO) == CR_IBA		 ? (66 + 14)				\
 : (REGNO) == CR_EIT_VB		 ? (66 + 15)				\
 : (REGNO) == CR_INT_S		 ? (66 + 16)				\
 : (REGNO) == CR_INT_M		 ? (66 + 17)				\
 :				   -1)

/* A C expression that returns the integer offset value for an automatic
   variable having address X (an RTL expression).  The default computation
   assumes that X is based on the frame-pointer and gives the offset from the
   frame-pointer.  This is required for targets that produce debugging output
   for DBX or COFF-style debugging output for SDB and allow the frame-pointer
   to be eliminated when the `-g' options is used.  */
/* #define DEBUGGER_AUTO_OFFSET(X) */

/* A C expression that returns the integer offset value for an argument having
   address X (an RTL expression).  The nominal offset is OFFSET.  */
/* #define DEBUGGER_ARG_OFFSET(OFFSET, X) */

/* A C expression that returns the type of debugging output GNU CC produces
   when the user specifies `-g' or `-ggdb'.  Define this if you have arranged
   for GNU CC to support more than one format of debugging output.  Currently,
   the allowable values are `DBX_DEBUG', `SDB_DEBUG', `DWARF_DEBUG',
   `DWARF2_DEBUG', and `XCOFF_DEBUG'.

   The value of this macro only affects the default debugging output; the user
   can always get a specific type of output by using `-gstabs', `-gcoff',
   `-gdwarf-1', `-gdwarf-2', or `-gxcoff'.

   Defined in svr4.h.  */

#undef	PREFERRED_DEBUGGING_TYPE
#define	PREFERRED_DEBUGGING_TYPE DBX_DEBUG


/* Specific Options for DBX Output.  */

/* Define this macro if GNU CC should produce debugging output for DBX in
   response to the `-g' option.

   Defined in svr4.h.  */
/* #define DBX_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce XCOFF format debugging output in
   response to the `-g' option.  This is a variant of DBX format.  */
/* #define XCOFF_DEBUGGING_INFO */

/* Define this macro to control whether GNU CC should by default generate GDB's
   extended version of DBX debugging information (assuming DBX-format debugging
   information is enabled at all).  If you don't define the macro, the default
   is 1: always generate the extended information if there is any occasion to.  */
/* #define DEFAULT_GDB_EXTENSIONS */

/* Define this macro if all `.stabs' commands should be output while in the
   text section.  */
/* #define DEBUG_SYMS_TEXT */

/* A C string constant naming the assembler pseudo op to use instead of
   `.stabs' to define an ordinary debugging symbol.  If you don't define this
   macro, `.stabs' is used.  This macro applies only to DBX debugging
   information format.  */
/* #define ASM_STABS_OP */

/* A C string constant naming the assembler pseudo op to use instead of
   `.stabd' to define a debugging symbol whose value is the current location.
   If you don't define this macro, `.stabd' is used.  This macro applies only
   to DBX debugging information format.  */
/* #define ASM_STABD_OP */

/* A C string constant naming the assembler pseudo op to use instead of
   `.stabn' to define a debugging symbol with no name.  If you don't define
   this macro, `.stabn' is used.  This macro applies only to DBX debugging
   information format.  */
/* #define ASM_STABN_OP */

/* Define this macro if DBX on your system does not support the construct
   `xsTAGNAME'.  On some systems, this construct is used to describe a forward
   reference to a structure named TAGNAME.  On other systems, this construct is
   not supported at all.  */
/* #define DBX_NO_XREFS */

/* A symbol name in DBX-format debugging information is normally continued
   (split into two separate `.stabs' directives) when it exceeds a certain
   length (by default, 80 characters).  On some operating systems, DBX requires
   this splitting; on others, splitting must not be done.  You can inhibit
   splitting by defining this macro with the value zero.  You can override the
   default splitting-length by defining this macro as an expression for the
   length you desire.  */
/* #define DBX_CONTIN_LENGTH */

/* Normally continuation is indicated by adding a `\' character to the end of a
   `.stabs' string when a continuation follows.  To use a different character
   instead, define this macro as a character constant for the character you
   want to use.  Do not define this macro if backslash is correct for your
   system.  */
/* #define DBX_CONTIN_CHAR */

/* Define this macro if it is necessary to go to the data section before
   outputting the `.stabs' pseudo-op for a non-global static variable.  */
/* #define DBX_STATIC_STAB_DATA_SECTION */

/* The value to use in the "code" field of the `.stabs' directive for a
   typedef.  The default is `N_LSYM'.  */
/* #define DBX_TYPE_DECL_STABS_CODE */

/* The value to use in the "code" field of the `.stabs' directive for a static
   variable located in the text section.  DBX format does not provide any
   "right" way to do this.  The default is `N_FUN'.  */
/* #define DBX_STATIC_CONST_VAR_CODE */

/* The value to use in the "code" field of the `.stabs' directive for a
   parameter passed in registers.  DBX format does not provide any "right" way
   to do this.  The default is `N_RSYM'.  */
/* #define DBX_REGPARM_STABS_CODE */

/* The letter to use in DBX symbol data to identify a symbol as a parameter
   passed in registers.  DBX format does not customarily provide any way to do
   this.  The default is `'P''.  */
/* #define DBX_REGPARM_STABS_LETTER */

/* The letter to use in DBX symbol data to identify a symbol as a stack
   parameter.  The default is `'p''.  */
/* #define DBX_MEMPARM_STABS_LETTER */

/* Define this macro if the DBX information for a function and its arguments
   should precede the assembler code for the function.  Normally, in DBX
   format, the debugging information entirely follows the assembler code.

   Defined in svr4.h.  */
/* #define DBX_FUNCTION_FIRST */

/* Define this macro if the `N_LBRAC' symbol for a block should precede the
   debugging information for variables and functions defined in that block.
   Normally, in DBX format, the `N_LBRAC' symbol comes first.  */
/* #define DBX_LBRAC_FIRST */

/* Define this macro if the value of a symbol describing the scope of a block
   (`N_LBRAC' or `N_RBRAC') should be relative to the start of the enclosing
   function.  Normally, GNU C uses an absolute address.

   Defined in svr4.h.  */
/* #define DBX_BLOCKS_FUNCTION_RELATIVE */

/* Define this macro if GNU C should generate `N_BINCL' and `N_EINCL'
   stabs for included header files, as on Sun systems.  This macro
   also directs GNU C to output a type number as a pair of a file
   number and a type number within the file.  Normally, GNU C does not
   generate `N_BINCL' or `N_EINCL' stabs, and it outputs a single
   number for a type number.  */
/* #define DBX_USE_BINCL */


/* Open ended Hooks for DBX Output.  */

/* Define this macro to say how to output to STREAM the debugging information
   for the start of a scope level for variable names.  The argument NAME is the
   name of an assembler symbol (for use with `assemble_name') whose value is
   the address where the scope begins.  */
/* #define DBX_OUTPUT_LBRAC(STREAM, NAME) */

/* Like `DBX_OUTPUT_LBRAC', but for the end of a scope level.  */
/* #define DBX_OUTPUT_RBRAC(STREAM, NAME) */

/* Define this macro if the target machine requires special handling to output
   an enumeration type.  The definition should be a C statement (sans
   semicolon) to output the appropriate information to STREAM for the type
   TYPE.  */
/* #define DBX_OUTPUT_ENUM(STREAM, TYPE) */

/* Define this macro if the target machine requires special output at the end
   of the debugging information for a function.  The definition should be a C
   statement (sans semicolon) to output the appropriate information to STREAM.
   FUNCTION is the `FUNCTION_DECL' node for the function.  */
/* #define DBX_OUTPUT_FUNCTION_END(STREAM, FUNCTION) */

/* Define this macro if you need to control the order of output of the standard
   data types at the beginning of compilation.  The argument SYMS is a `tree'
   which is a chain of all the predefined global symbols, including names of
   data types.

   Normally, DBX output starts with definitions of the types for integers and
   characters, followed by all the other predefined types of the particular
   language in no particular order.

   On some machines, it is necessary to output different particular types
   first.  To do this, define `DBX_OUTPUT_STANDARD_TYPES' to output those
   symbols in the necessary order.  Any predefined types that you don't
   explicitly output will be output afterward in no particular order.

   Be careful not to define this macro so that it works only for C.  There are
   no global variables to access most of the built-in types, because another
   language may have another set of types.  The way to output a particular type
   is to look through SYMS to see if you can find it.  Here is an example:

        {
          tree decl;
          for (decl = syms; decl; decl = TREE_CHAIN (decl))
            if (!strcmp (IDENTIFIER_POINTER (DECL_NAME (decl)),
                         "long int"))
              dbxout_symbol (decl);
          ...
        }

   This does nothing if the expected type does not exist.

   See the function `init_decl_processing' in `c-decl.c' to find the names to
   use for all the built-in C types. */
/* #define DBX_OUTPUT_STANDARD_TYPES(SYMS) */

/* Some stabs encapsulation formats (in particular ECOFF), cannot
   handle the `.stabs "",N_FUN,,0,0,Lscope-function-1' gdb dbx
   extension construct.  On those machines, define this macro to turn
   this feature off without disturbing the rest of the gdb extensions.  */
/* #define NO_DBX_FUNCTION_END */


/* File names in DBX format.  */

/* Define this if DBX wants to have the current directory recorded in each
   object file.

   Note that the working directory is always recorded if GDB extensions are
   enabled.  */
/* #define DBX_WORKING_DIRECTORY */

/* A C statement to output DBX debugging information to the stdio stream STREAM
   which indicates that file NAME is the main source file--the file specified
   as the input file for compilation.  This macro is called only once, at the
   beginning of compilation.

   This macro need not be defined if the standard form of output for DBX
   debugging information is appropriate.

   Defined in svr4.h.  */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILENAME(STREAM, NAME) */

/* A C statement to output DBX debugging information to the stdio stream STREAM
   which indicates that the current directory during compilation is named NAME.

   This macro need not be defined if the standard form of output for DBX
   debugging information is appropriate.  */
/* #define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(STREAM, NAME) */

/* A C statement to output DBX debugging information at the end of compilation
   of the main source file NAME.

   If you don't define this macro, nothing special is output at the end of
   compilation, which is correct for most machines.  */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILE_END(STREAM, NAME) */

/* A C statement to output DBX debugging information to the stdio stream STREAM
   which indicates that file NAME is the current source file.  This output is
   generated each time input shifts to a different source file as a result of
   `#include', the end of an included file, or a `#line' command.

   This macro need not be defined if the standard form of output for DBX
   debugging information is appropriate.  */
/* #define DBX_OUTPUT_SOURCE_FILENAME(STREAM, NAME) */


/* Macros for SDB and Dwarf Output.  */

/* Define this macro if GNU CC should produce COFF-style debugging output for
   SDB in response to the `-g' option.  */
/* #define SDB_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce dwarf format debugging output in
   response to the `-g' option.

   Defined in svr4.h.  */
/* #define DWARF_DEBUGGING_INFO */

/* Define this macro if GNU CC should produce dwarf version 2 format debugging
   output in response to the `-g' option.

   To support optional call frame debugging information, you must also define
   `INCOMING_RETURN_ADDR_RTX' and either set `RTX_FRAME_RELATED_P' on the
   prologue insns if you use RTL for the prologue, or call `dwarf2out_def_cfa'
   and `dwarf2out_reg_save' as appropriate from output_function_prologue() if
   you don't.

   Defined in svr4.h.  */
/* #define DWARF2_DEBUGGING_INFO */

/* Define these macros to override the assembler syntax for the special SDB
   assembler directives.  See `sdbout.c' for a list of these macros and their
   arguments.  If the standard syntax is used, you need not define them
   yourself.  */
/* #define PUT_SDB_... */

/* Some assemblers do not support a semicolon as a delimiter, even between SDB
   assembler directives.  In that case, define this macro to be the delimiter
   to use (usually `\n').  It is not necessary to define a new set of
   `PUT_SDB_OP' macros if this is the only change required.  */
/* #define SDB_DELIM */

/* Define this macro to override the usual method of constructing a dummy name
   for anonymous structure and union types.  See `sdbout.c' for more
   information.  */
/* #define SDB_GENERATE_FAKE */

/* Define this macro to allow references to unknown structure, union, or
   enumeration tags to be emitted.  Standard COFF does not allow handling of
   unknown references, MIPS ECOFF has support for it.  */
/* #define SDB_ALLOW_UNKNOWN_REFERENCES */

/* Define this macro to allow references to structure, union, or enumeration
   tags that have not yet been seen to be handled.  Some assemblers choke if
   forward tags are used, while some require it.  */
/* #define SDB_ALLOW_FORWARD_REFERENCES */


/* Cross Compilation and Floating Point.  */

/* While all modern machines use 2's complement representation for integers,
   there are a variety of representations for floating point numbers.  This
   means that in a cross-compiler the representation of floating point numbers
   in the compiled program may be different from that used in the machine doing
   the compilation.

   Because different representation systems may offer different amounts of
   range and precision, the cross compiler cannot safely use the host machine's
   floating point arithmetic.  Therefore, floating point constants must be
   represented in the target machine's format.  This means that the cross
   compiler cannot use `atof' to parse a floating point constant; it must have
   its own special routine to use instead.  Also, constant folding must emulate
   the target machine's arithmetic (or must not be done at all).

   The macros in the following table should be defined only if you are cross
   compiling between different floating point formats.

   Otherwise, don't define them.  Then default definitions will be set up which
   use `double' as the data type, `==' to test for equality, etc.

   You don't need to worry about how many times you use an operand of any of
   these macros.  The compiler never uses operands which have side effects.  */

/* A macro for the C data type to be used to hold a floating point value in the
   target machine's format.  Typically this would be a `struct' containing an
   array of `int'.  */
/* #define REAL_VALUE_TYPE */

/* A macro for a C expression which compares for equality the two values, X and
   Y, both of type `REAL_VALUE_TYPE'.  */
/* #define REAL_VALUES_EQUAL(X, Y) */

/* A macro for a C expression which tests whether X is less than Y, both values
   being of type `REAL_VALUE_TYPE' and interpreted as floating point numbers in
   the target machine's representation.  */
/* #define REAL_VALUES_LESS(X, Y) */

/* A macro for a C expression which performs the standard library function
   `ldexp', but using the target machine's floating point representation.  Both
   X and the value of the expression have type `REAL_VALUE_TYPE'.  The second
   argument, SCALE, is an integer.  */
/* #define REAL_VALUE_LDEXP(X, SCALE) */

/* A macro whose definition is a C expression to convert the target-machine
   floating point value X to a signed integer.  X has type `REAL_VALUE_TYPE'.  */
/* #define REAL_VALUE_FIX(X) */

/* A macro whose definition is a C expression to convert the target-machine
   floating point value X to an unsigned integer.  X has type
   `REAL_VALUE_TYPE'.  */
/* #define REAL_VALUE_UNSIGNED_FIX(X) */

/* A macro whose definition is a C expression to round the target-machine
   floating point value X towards zero to an integer value (but still as a
   floating point number).  X has type `REAL_VALUE_TYPE', and so does the
   value.  */
/* #define REAL_VALUE_RNDZINT(X) */

/* A macro whose definition is a C expression to round the target-machine
   floating point value X towards zero to an unsigned integer value (but still
   represented as a floating point number).  X has type `REAL_VALUE_TYPE', and
   so does the value.  */
/* #define REAL_VALUE_UNSIGNED_RNDZINT(X) */

/* A macro for a C expression which converts STRING, an expression of type
   `char *', into a floating point number in the target machine's
   representation for mode MODE.  The value has type `REAL_VALUE_TYPE'.  */
/* #define REAL_VALUE_ATOF(STRING, MODE) */

/* Define this macro if infinity is a possible floating point value, and
   therefore division by 0 is legitimate.  */
/* #define REAL_INFINITY */

/* A macro for a C expression which determines whether X, a floating point
   value, is infinity.  The value has type `int'.  By default, this is defined
   to call `isinf'.  */
/* #define REAL_VALUE_ISINF(X) */

/* A macro for a C expression which determines whether X, a floating point
   value, is a "nan" (not-a-number).  The value has type `int'.  By default,
   this is defined to call `isnan'.  */
/* #define REAL_VALUE_ISNAN(X) */

/* Define the following additional macros if you want to make floating point
   constant folding work while cross compiling.  If you don't define them,
   cross compilation is still possible, but constant folding will not happen
   for floating point values.  */

/* A macro for a C statement which calculates an arithmetic operation of the
   two floating point values X and Y, both of type `REAL_VALUE_TYPE' in the
   target machine's representation, to produce a result of the same type and
   representation which is stored in OUTPUT (which will be a variable).

   The operation to be performed is specified by CODE, a tree code which will
   always be one of the following: `PLUS_EXPR', `MINUS_EXPR', `MULT_EXPR',
   `RDIV_EXPR', `MAX_EXPR', `MIN_EXPR'.

   The expansion of this macro is responsible for checking for overflow.  If
   overflow happens, the macro expansion should execute the statement `return
   0;', which indicates the inability to perform the arithmetic operation
   requested.  */
/* #define REAL_ARITHMETIC(OUTPUT, CODE, X, Y) */

/* The real.h file actually defines REAL_ARITHMETIC appropriately if it was
   defined at all before entering into the code, by using #undef first.  */
#define REAL_ARITHMETIC

/* A macro for a C expression which returns the negative of the floating point
   value X.  Both X and the value of the expression have type `REAL_VALUE_TYPE'
   and are in the target machine's floating point representation.

   There is no way for this macro to report overflow, since overflow can't
   happen in the negation operation.  */
/* #define REAL_VALUE_NEGATE(X) */

/* A macro for a C expression which converts the floating point value X to mode
   MODE.

   Both X and the value of the expression are in the target machine's floating
   point representation and have type `REAL_VALUE_TYPE'.  However, the value
   should have an appropriate bit pattern to be output properly as a floating
   constant whose precision accords with mode MODE.

   There is no way for this macro to report overflow.  */
/* #define REAL_VALUE_TRUNCATE(MODE, X) */

/* A macro for a C expression which converts a floating point value X into a
   double-precision integer which is then stored into LOW and HIGH, two
   variables of type INT.  */
/* #define REAL_VALUE_TO_INT(LOW, HIGH, X) */

/* A macro for a C expression which converts a double-precision integer found
   in LOW and HIGH, two variables of type INT, into a floating point value
   which is then stored into X.  */
/* #define REAL_VALUE_FROM_INT(X, LOW, HIGH) */


/* Miscellaneous Parameters.  */

/* Define this if you have defined special-purpose predicates in the file
   `MACHINE.c'.  This macro is called within an initializer of an array of
   structures.  The first field in the structure is the name of a predicate and
   the second field is an array of rtl codes.  For each predicate, list all rtl
   codes that can be in expressions matched by the predicate.  The list should
   have a trailing comma.  Here is an example of two entries in the list for a
   typical RISC machine:

        #define PREDICATE_CODES \
          {"gen_reg_rtx_operand", {SUBREG, REG}},  \
          {"reg_or_short_cint_operand", {SUBREG, REG, CONST_INT}},

   Defining this macro does not affect the generated code (however, incorrect
   definitions that omit an rtl code that may be matched by the predicate can
   cause the compiler to malfunction).  Instead, it allows the table built by
   `genrecog' to be more compact and efficient, thus speeding up the compiler.
   The most important predicates to include in the list specified by this macro
   are thoses used in the most insn patterns.  */

#define PREDICATE_CODES							\
  { "short_memory_operand",		{ MEM }},			\
  { "long_memory_operand",		{ MEM }},			\
  { "d30v_memory_operand",		{ MEM }},			\
  { "single_reg_memory_operand",	{ MEM }},			\
  { "const_addr_memory_operand",	{ MEM }},			\
  { "call_operand",			{ MEM }},			\
  { "gpr_operand",			{ REG, SUBREG }},		\
  { "accum_operand",			{ REG, SUBREG }},		\
  { "gpr_or_accum_operand",		{ REG, SUBREG }},		\
  { "cr_operand",			{ REG, SUBREG }},		\
  { "repeat_operand",			{ REG, SUBREG }},		\
  { "flag_operand",			{ REG, SUBREG }},		\
  { "br_flag_operand",			{ REG, SUBREG }},		\
  { "br_flag_or_constant_operand",	{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_br_flag_operand",		{ REG, SUBREG }},		\
  { "f0_operand",			{ REG, SUBREG }},		\
  { "f1_operand",			{ REG, SUBREG }},		\
  { "carry_operand",			{ REG, SUBREG }},		\
  { "reg_or_0_operand",			{ REG, SUBREG, CONST_INT,	\
					  CONST_DOUBLE }},		\
  { "gpr_or_signed6_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_unsigned5_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_unsigned6_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_constant_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF }},			\
  { "gpr_or_dbl_const_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, CONST_DOUBLE }},	\
  { "gpr_or_memory_operand",		{ REG, SUBREG, MEM }},		\
  { "move_input_operand",		{ REG, SUBREG, MEM, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, CONST_DOUBLE }},	\
  { "move_output_operand",		{ REG, SUBREG, MEM }},		\
  { "signed6_operand",			{ CONST_INT }},			\
  { "unsigned5_operand",		{ CONST_INT }},			\
  { "unsigned6_operand",		{ CONST_INT }},			\
  { "bitset_operand",			{ CONST_INT }},			\
  { "condexec_test_operator",		{ EQ, NE }},			\
  { "condexec_branch_operator",		{ EQ, NE }},			\
  { "condexec_unary_operator",		{ ABS, NEG, NOT, ZERO_EXTEND }}, \
  { "condexec_addsub_operator",		{ PLUS, MINUS }},		\
  { "condexec_binary_operator",		{ MULT, AND, IOR, XOR,		\
					  ASHIFT, ASHIFTRT, LSHIFTRT,	\
					  ROTATE, ROTATERT }},		\
  { "condexec_shiftl_operator",		{ ASHIFT, ROTATE }},		\
  { "condexec_extend_operator",		{ SIGN_EXTEND, ZERO_EXTEND }},	\
  { "branch_zero_operator",		{ EQ, NE }},			\
  { "cond_move_dest_operand",		{ REG, SUBREG, MEM }},		\
  { "cond_move_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, MEM }},		\
  { "cond_exec_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, MEM }},		\
  { "srelational_si_operator",		{ EQ, NE, LT, LE, GT, GE }},	\
  { "urelational_si_operator",		{ LTU, LEU, GTU, GEU }},	\
  { "relational_di_operator",		{ EQ, NE, LT, LE, GT, GE,	\
					  LTU, LEU, GTU, GEU }},

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this if control falls through a `case' insn when the index value is
   out of range.  This means the specified default-label is actually ignored by
   the `case' insn proper.  */
/* #define CASE_DROPS_THROUGH */

/* Define this to be the smallest number of different values for which it is
   best to use a jump-table instead of a tree of conditional branches.  The
   default is four for machines with a `casesi' instruction and five otherwise.
   This is best for most machines.  */
/* #define CASE_VALUES_THRESHOLD */

/* Define this macro if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  Most RISC machines
   have this property and most CISC machines do not.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define this macro to be a C expression indicating when insns that read
   memory in MODE, an integral mode narrower than a word, set the bits outside
   of MODE to be either the sign-extension or the zero-extension of the data
   read.  Return `SIGN_EXTEND' for values of MODE for which the insn
   sign-extends, `ZERO_EXTEND' for which it zero-extends, and `NIL' for other
   modes.

   This macro is not called with MODE non-integral or with a width greater than
   or equal to `BITS_PER_WORD', so you may return any value in this case.  Do
   not define this macro if it would always return `NIL'.  On machines where
   this macro is defined, you will normally define it as the constant
   `SIGN_EXTEND' or `ZERO_EXTEND'.  */

#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Define this macro if the same instructions that convert a floating point
   number to a signed fixed point number also convert validly to an unsigned
   one.  */
/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 8

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  If this is undefined, the default is `MOVE_MAX'.
   Otherwise, it is the constant value that is the largest value that
   `MOVE_MAX' can have at run-time.  */
/* #define MAX_MOVE_MAX */

/* A C expression that is nonzero if on this machine the number of bits
   actually used for the count of a shift operation is equal to the number of
   bits needed to represent the size of the object being shifted.  When this
   macro is non-zero, the compiler will assume that it is safe to omit a
   sign-extend, zero-extend, and certain bitwise `and' instructions that
   truncates the count of a shift operation.  On machines that have
   instructions that act on bitfields at variable positions, which may include
   `bit test' instructions, a nonzero `SHIFT_COUNT_TRUNCATED' also enables
   deletion of truncations of the values that serve as arguments to bitfield
   instructions.

   If both types of instructions truncate the count (for shifts) and position
   (for bitfield operations), or if no variable-position bitfield instructions
   exist, you should define this macro.

   However, on some machines, such as the 80386 and the 680x0, truncation only
   applies to shift operations and not the (real or pretended) bitfield
   operations.  Define `SHIFT_COUNT_TRUNCATED' to be zero on such machines.
   Instead, add patterns to the `md' file that include the implied truncation
   of the shift instructions.

   You need not define this macro if it would always have the value of zero.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* A C expression describing the value returned by a comparison operator with
   an integral mode and stored by a store-flag instruction (`sCOND') when the
   condition is true.  This description must apply to *all* the `sCOND'
   patterns and all the comparison operators whose results have a `MODE_INT'
   mode.

   A value of 1 or -1 means that the instruction implementing the comparison
   operator returns exactly 1 or -1 when the comparison is true and 0 when the
   comparison is false.  Otherwise, the value indicates which bits of the
   result are guaranteed to be 1 when the comparison is true.  This value is
   interpreted in the mode of the comparison operation, which is given by the
   mode of the first operand in the `sCOND' pattern.  Either the low bit or the
   sign bit of `STORE_FLAG_VALUE' be on.  Presently, only those bits are used
   by the compiler.

   If `STORE_FLAG_VALUE' is neither 1 or -1, the compiler will generate code
   that depends only on the specified bits.  It can also replace comparison
   operators with equivalent operations if they cause the required bits to be
   set, even if the remaining bits are undefined.  For example, on a machine
   whose comparison operators return an `SImode' value and where
   `STORE_FLAG_VALUE' is defined as `0x80000000', saying that just the sign bit
   is relevant, the expression

        (ne:SI (and:SI X (const_int POWER-OF-2)) (const_int 0))

   can be converted to

        (ashift:SI X (const_int N))

   where N is the appropriate shift count to move the bit being tested into the
   sign bit.

   There is no way to describe a machine that always sets the low-order bit for
   a true value, but does not guarantee the value of any other bits, but we do
   not know of any machine that has such an instruction.  If you are trying to
   port GNU CC to such a machine, include an instruction to perform a
   logical-and of the result with 1 in the pattern for the comparison operators
   and let us know (*note How to Report Bugs: Bug Reporting.).

   Often, a machine will have multiple instructions that obtain a value from a
   comparison (or the condition codes).  Here are rules to guide the choice of
   value for `STORE_FLAG_VALUE', and hence the instructions to be used:

      * Use the shortest sequence that yields a valid definition for
        `STORE_FLAG_VALUE'.  It is more efficient for the compiler to
        "normalize" the value (convert it to, e.g., 1 or 0) than for
        the comparison operators to do so because there may be
        opportunities to combine the normalization with other
        operations.

      * For equal-length sequences, use a value of 1 or -1, with -1
        being slightly preferred on machines with expensive jumps and
        1 preferred on other machines.

      * As a second choice, choose a value of `0x80000001' if
        instructions exist that set both the sign and low-order bits
        but do not define the others.

      * Otherwise, use a value of `0x80000000'.

   Many machines can produce both the value chosen for `STORE_FLAG_VALUE' and
   its negation in the same number of instructions.  On those machines, you
   should also define a pattern for those cases, e.g., one matching

        (set A (neg:M (ne:M B C)))

   Some machines can also perform `and' or `plus' operations on condition code
   values with less instructions than the corresponding `sCOND' insn followed
   by `and' or `plus'.  On those machines, define the appropriate patterns.
   Use the names `incscc' and `decscc', respectively, for the the patterns
   which perform `plus' or `minus' operations on condition code values.  See
   `rs6000.md' for some examples.  The GNU Superoptizer can be used to find
   such instruction sequences on other machines.

   You need not define `STORE_FLAG_VALUE' if the machine has no store-flag
   instructions.  */
/* #define STORE_FLAG_VALUE */

/* A C expression that gives a non-zero floating point value that is returned
   when comparison operators with floating-point results are true.  Define this
   macro on machine that have comparison operations that return floating-point
   values.  If there are no such operations, do not define this macro.  */
/* #define FLOAT_STORE_FLAG_VALUE */

/* An alias for the machine mode for pointers.  On most machines, define this
   to be the integer mode corresponding to the width of a hardware pointer;
   `SImode' on 32-bit machine or `DImode' on 64-bit machines.  On some machines
   you must define this to be one of the partial integer modes, such as
   `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to `Pmode'.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  On most machines this should be
   `QImode'.  */
#define FUNCTION_MODE QImode

/* A C expression for the maximum number of instructions above which the
   function DECL should not be inlined.  DECL is a `FUNCTION_DECL' node.

   The default definition of this macro is 64 plus 8 times the number of
   arguments that the function accepts.  Some people think a larger threshold
   should be used on RISC machines.  */
/* #define INTEGRATE_THRESHOLD(DECL) */

/* Define this if the preprocessor should ignore `#sccs' directives and print
   no error message.

   Defined in svr4.h.  */
/* #define SCCS_DIRECTIVE */

/* Define this macro if the system header files support C++ as well as C.  This
   macro inhibits the usual method of using system header files in C++, which
   is to pretend that the file's contents are enclosed in `extern "C" {...}'.  */
/* #define NO_IMPLICIT_EXTERN_C */

/* Define this macro to handle System V style pragmas (particularly #pack).

   Defined in svr4.h.  */
#define HANDLE_SYSV_PRAGMA

/* Define this macro if you want to handle #pragma weak (HANDLE_SYSV_PRAGMA
   must also be defined).  */
/* #define HANDLE_WEAK_PRAGMA */

/* Define this macro to control use of the character `$' in identifier names.
   The value should be 0, 1, or 2.  0 means `$' is not allowed by default; 1
   means it is allowed by default if `-traditional' is used; 2 means it is
   allowed by default provided `-ansi' is not used.  1 is the default; there is
   no need to define this macro in that case.  */
/* #define DOLLARS_IN_IDENTIFIERS */

/* Define this macro if the assembler does not accept the character `$' in
   label names.  By default constructors and destructors in G++ have `$' in the
   identifiers.  If this macro is defined, `.' is used instead.

   Defined in svr4.h.  */
/* #define NO_DOLLAR_IN_LABEL */

/* Define this macro if the assembler does not accept the character `.' in
   label names.  By default constructors and destructors in G++ have names that
   use `.'.  If this macro is defined, these names are rewritten to avoid `.'.  */
/* #define NO_DOT_IN_LABEL */

/* Define this macro if the target system expects every program's `main'
   function to return a standard "success" value by default (if no other value
   is explicitly returned).

   The definition should be a C statement (sans semicolon) to generate the
   appropriate rtl instructions.  It is used only when compiling the end of
   `main'.  */
/* #define DEFAULT_MAIN_RETURN */

/* Define this if your `exit' function needs to do something besides calling an
   external function `_cleanup' before terminating with `_exit'.  The
   `EXIT_BODY' macro is only needed if `NEED_ATEXIT' is defined and
   `ON_EXIT' is not defined.  */
/* #define EXIT_BODY */

/* Define this macro as a C expression that is nonzero if it is safe for the
   delay slot scheduler to place instructions in the delay slot of INSN, even
   if they appear to use a resource set or clobbered in INSN.  INSN is always a
   `jump_insn' or an `insn'; GNU CC knows that every `call_insn' has this
   behavior.  On machines where some `insn' or `jump_insn' is really a function
   call and hence has this behavior, you should define this macro.

   You need not define this macro if it would always return zero.  */
/* #define INSN_SETS_ARE_DELAYED(INSN) */

/* Define this macro as a C expression that is nonzero if it is safe for the
   delay slot scheduler to place instructions in the delay slot of INSN, even
   if they appear to set or clobber a resource referenced in INSN.  INSN is
   always a `jump_insn' or an `insn'.  On machines where some `insn' or
   `jump_insn' is really a function call and its operands are registers whose
   use is actually in the subroutine it calls, you should define this macro.
   Doing so allows the delay slot scheduler to move instructions which copy
   arguments into the argument registers into the delay slot of INSN.

   You need not define this macro if it would always return zero.  */
/* #define INSN_REFERENCES_ARE_DELAYED(INSN) */

/* In rare cases, correct code generation requires extra machine dependent
   processing between the second jump optimization pass and delayed branch
   scheduling.  On those machines, define this macro as a C statement to act on
   the code starting at INSN.  */
#define MACHINE_DEPENDENT_REORG(INSN) d30v_machine_dependent_reorg (INSN)

/* Define this macro if in some cases global symbols from one translation unit
   may not be bound to undefined symbols in another translation unit without
   user intervention.  For instance, under Microsoft Windows symbols must be
   explicitly imported from shared libraries (DLLs).  */
/* #define MULTIPLE_SYMBOL_SPACES */

/* A C expression for the maximum number of instructions to execute via
   conditional execution instructions instead of a branch.  A value of
   BRANCH_COST+1 is the default if the machine does not use cc0, and 1 if it
   does use cc0. */
#define MAX_CONDITIONAL_EXECUTE d30v_cond_exec

#define D30V_DEFAULT_MAX_CONDITIONAL_EXECUTE 4

/* Values of the -mcond-exec=n string.  */
extern int d30v_cond_exec;
extern const char *d30v_cond_exec_string;

#endif /* GCC_D30V_H */
