/* Definitions of target machine for GNU compiler, for MMIX.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Hans-Peter Nilsson (hp@bitrange.com)

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

#ifndef GCC_MMIX_H
#define GCC_MMIX_H

/* First, some local helper macros.  Note that the "default" value of
   FIXED_REGISTERS, CALL_USED_REGISTERS, REG_ALLOC_ORDER and
   REG_CLASS_CONTENTS depend on these values.  */
#define MMIX_RESERVED_GNU_ARG_0_REGNUM 231
#define MMIX_FIRST_ARG_REGNUM \
  (TARGET_ABI_GNU ? MMIX_RESERVED_GNU_ARG_0_REGNUM : 16)
#define MMIX_FIRST_INCOMING_ARG_REGNUM \
  (TARGET_ABI_GNU ? MMIX_RESERVED_GNU_ARG_0_REGNUM : 0)
#define MMIX_MAX_ARGS_IN_REGS 16

/* FIXME: This one isn't fully implemented yet.  Return values larger than
   one register are passed by reference in MMIX_STRUCT_VALUE_REGNUM by the
   caller, except for return values of type "complex".  */
#define MMIX_MAX_REGS_FOR_VALUE 16
#define MMIX_RETURN_VALUE_REGNUM \
  (TARGET_ABI_GNU ? MMIX_RESERVED_GNU_ARG_0_REGNUM : 15)
#define MMIX_OUTGOING_RETURN_VALUE_REGNUM \
  (TARGET_ABI_GNU ? MMIX_RESERVED_GNU_ARG_0_REGNUM : 0)
#define MMIX_STRUCT_VALUE_REGNUM 251
#define MMIX_STATIC_CHAIN_REGNUM 252
#define MMIX_FRAME_POINTER_REGNUM 253
#define MMIX_STACK_POINTER_REGNUM 254
#define MMIX_LAST_GENERAL_REGISTER 255
#define MMIX_INCOMING_RETURN_ADDRESS_REGNUM MMIX_rJ_REGNUM
#define MMIX_HIMULT_REGNUM 258
#define MMIX_REMAINDER_REGNUM 260
#define MMIX_ARG_POINTER_REGNUM 261
#define MMIX_LAST_REGISTER_FILE_REGNUM 31

/* Four registers; "ideally, these registers should be call-clobbered", so
   just grab a bunch of the common clobbered registers.  FIXME: Last
   registers of return-value should be used, with an error if there's a
   return-value (that collides in size).  */
#define MMIX_EH_RETURN_DATA_REGNO_START (MMIX_STRUCT_VALUE_REGNUM - 4)

/* Try to keep the definitions from running away on their own.  */
#if (MMIX_EH_RETURN_DATA_REGNO_START \
     != MMIX_RESERVED_GNU_ARG_0_REGNUM + MMIX_MAX_ARGS_IN_REGS)
 #error MMIX register definition inconsistency
#endif

#if (MMIX_MAX_REGS_FOR_VALUE + MMIX_MAX_ARGS_IN_REGS > 32)
 #error MMIX parameters and return values bad, more than 32 registers
#endif

/* This chosen as "a call-clobbered hard register that is otherwise
   untouched by the epilogue".  */
#define MMIX_EH_RETURN_STACKADJ_REGNUM MMIX_STATIC_CHAIN_REGNUM

#ifdef REG_OK_STRICT
# define MMIX_REG_OK_STRICT 1
#else
# define MMIX_REG_OK_STRICT 0
#endif

#define MMIX_FUNCTION_ARG_SIZE(MODE, TYPE) \
 ((MODE) != BLKmode ? GET_MODE_SIZE (MODE) : int_size_in_bytes (TYPE))

/* Declarations for helper variables that are not tied to a particular
   target macro.  */
extern struct rtx_def *mmix_compare_op0;
extern struct rtx_def *mmix_compare_op1;

/* Per-function machine data.  This is normally an opaque type just
   defined and used in the tm.c file, but we need to see the definition in
   mmix.md too.  */
struct machine_function
 {
   int has_landing_pad;
 };

/* For these target macros, there is no generic documentation here.  You
   should read `Using and Porting GCC' for that.  Only comments specific
   to the MMIX target are here.

   There are however references to the specific texinfo node (comments
   with "Node:"), so there should be little or nothing amiss.  Probably
   the opposite, since we don't have to care about old littering and
   soon outdated generic comments.  */

/* Node: Driver */

/* When both ABI:s work, this is how we tell them apart in code.  The
   GNU abi is implied the default.  Also implied in TARGET_DEFAULT.  */
#define CPP_SPEC \
 "%{mabi=gnu:-D__MMIX_ABI_GNU__\
    %{mabi=mmixware:\
      %eoptions -mabi=mmixware and -mabi=gnu are mutually exclusive}}\
  %{!mabi=gnu:-D__MMIX_ABI_MMIXWARE__}"

/* User symbols are in the same name-space as built-in symbols, but we
   don't need the built-in symbols, so remove those and instead apply
   stricter operand checking.  Don't warn when expanding insns.  */
#define ASM_SPEC "-no-predefined-syms -x"

/* Pass on -mset-program-start=N and -mset-data-start=M to the linker.
   Provide default program start 0x100 unless -mno-set-program-start.
   Don't do this if linking relocatably, with -r.  For a final link,
   produce mmo, unless ELF is requested or when linking relocatably.  */
#define LINK_SPEC \
 "%{mset-program-start=*:--defsym __.MMIX.start..text=%*}\
  %{mset-data-start=*:--defsym __.MMIX.start..data=%*}\
  %{!mset-program-start=*:\
    %{!mno-set-program-start:\
     %{!r:--defsym __.MMIX.start..text=0x100}}}\
  %{!melf:%{!r:-m mmo}}%{melf|r:-m elf64mmix}"

/* Put unused option values here.  */
extern const char *mmix_cc1_ignored_option;

#define TARGET_OPTIONS					\
   {{"set-program-start=", &mmix_cc1_ignored_option,	\
  N_("Set start-address of the program") },		\
    {"set-data-start=", &mmix_cc1_ignored_option,	\
  N_("Set start-address of data")}}

/* FIXME: There's no provision for profiling here.  */
#define STARTFILE_SPEC  \
  "crti%O%s crtbegin%O%s"

#define ENDFILE_SPEC "crtend%O%s crtn%O%s"

/* Node: Run-time Target */

/* Define __LONG_MAX__, since we're advised not to change glimits.h.  */
#define CPP_PREDEFINES "-D__mmix__ -D__MMIX__ -D__LONG_MAX__=9223372036854775807L"

extern int target_flags;

#define TARGET_MASK_LIBFUNCS 1
#define TARGET_MASK_ABI_GNU 2
#define TARGET_MASK_FCMP_EPSILON 4
#define TARGET_MASK_ZERO_EXTEND 8
#define TARGET_MASK_KNUTH_DIVISION 16
#define TARGET_MASK_TOPLEVEL_SYMBOLS 32
#define TARGET_MASK_BRANCH_PREDICT 64

/* We use the term "base address" since that's what Knuth uses.  The base
   address goes in a global register.  When addressing, it's more like
   "base address plus offset", with the offset being 0..255 from the base,
   which itself can be a symbol plus an offset.  The effect is like having
   a constant pool in global registers, code offseting from those
   registers (automatically causing a request for a suitable constant base
   address register) without having to know the specific register or the
   specific offset.  */
#define TARGET_MASK_BASE_ADDRESSES 128

/* FIXME: Get rid of this one.  */
#define TARGET_LIBFUNC (target_flags & TARGET_MASK_LIBFUNCS)
#define TARGET_ABI_GNU (target_flags & TARGET_MASK_ABI_GNU)
#define TARGET_FCMP_EPSILON (target_flags & TARGET_MASK_FCMP_EPSILON)
#define TARGET_ZERO_EXTEND (target_flags & TARGET_MASK_ZERO_EXTEND)
#define TARGET_KNUTH_DIVISION (target_flags & TARGET_MASK_KNUTH_DIVISION)
#define TARGET_TOPLEVEL_SYMBOLS (target_flags & TARGET_MASK_TOPLEVEL_SYMBOLS)
#define TARGET_BRANCH_PREDICT (target_flags & TARGET_MASK_BRANCH_PREDICT)
#define TARGET_BASE_ADDRESSES (target_flags & TARGET_MASK_BASE_ADDRESSES)

#define TARGET_DEFAULT \
 (TARGET_MASK_BRANCH_PREDICT | TARGET_MASK_BASE_ADDRESSES)

/* FIXME: Provide a way to *load* the epsilon register.  */
#define TARGET_SWITCHES							\
 {{"libfuncs",		TARGET_MASK_LIBFUNCS,				\
   N_("For intrinsics library: pass all parameters in registers")},	\
  {"no-libfuncs",	-TARGET_MASK_LIBFUNCS, ""},			\
  {"abi=mmixware",	-TARGET_MASK_ABI_GNU,				\
   N_("Use register stack for parameters and return value")},		\
  {"abi=gnu",		TARGET_MASK_ABI_GNU,				\
   N_("Use call-clobbered registers for parameters and return value")},	\
  {"epsilon",		TARGET_MASK_FCMP_EPSILON,			\
   N_("Use epsilon-respecting floating point compare instructions")},	\
  {"no-epsilon",	-TARGET_MASK_FCMP_EPSILON, ""},			\
  {"zero-extend",	TARGET_MASK_ZERO_EXTEND,			\
   N_("Use zero-extending memory loads, not sign-extending ones")},	\
  {"no-zero-extend",	-TARGET_MASK_ZERO_EXTEND,  ""},			\
  {"knuthdiv",		TARGET_MASK_KNUTH_DIVISION,			\
   N_("Generate divide results with reminder having the same sign as the\
 divisor (not the dividend)")},						\
  {"no-knuthdiv",	-TARGET_MASK_KNUTH_DIVISION, ""},		\
  {"toplevel-symbols",	TARGET_MASK_TOPLEVEL_SYMBOLS,			\
   N_("Prepend global symbols with \":\" (for use with PREFIX)")},	\
  {"no-toplevel-symbols", -TARGET_MASK_TOPLEVEL_SYMBOLS,		\
   N_("Do not provide a default start-address 0x100 of the program")},	\
  {"elf", 0,								\
   N_("Link to emit program in ELF format (rather than mmo)")},		\
  {"branch-predict",	TARGET_MASK_BRANCH_PREDICT,			\
   N_("Use P-mnemonics for branches statically predicted as taken")},	\
  {"no-branch-predict",	-TARGET_MASK_BRANCH_PREDICT,			\
   N_("Don't use P-mnemonics for branches")},				\
  {"base-addresses",	TARGET_MASK_BASE_ADDRESSES,			\
   N_("Use addresses that allocate global registers")},			\
  {"no-base-addresses",	-TARGET_MASK_BASE_ADDRESSES,			\
   N_("Do not use addresses that allocate global registers")},		\
  {"",			TARGET_DEFAULT, ""}}

/* Unfortunately, this must not reference anything in "mmix.c".  */
#define TARGET_VERSION \
  fprintf (stderr, " (MMIX)")

#define OVERRIDE_OPTIONS mmix_override_options ()

#define OPTIMIZATION_OPTIONS(LEVEL, SIZE)	\
  do						\
    {						\
      if (LEVEL >= 1)				\
	flag_regmove = TRUE;			\
      						\
      if (SIZE || LEVEL > 1)			\
	{					\
	  flag_omit_frame_pointer = TRUE;	\
	  flag_strength_reduce = FALSE;		\
	}					\
    }						\
  while (0)

/* This one will have to wait a little bit; right now we can't debug
   neither with or without a frame-pointer.  */
/* #define CAN_DEBUG_WITHOUT_FP */


/* Node: Per-Function Data */
#define INIT_EXPANDERS mmix_init_expanders ()


/* Node: Storage Layout */
/* I see no bitfield instructions.  Anyway, the common order is from low
   to high, as the power of two, hence little-endian.  */
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1
#define FLOAT_WORDS_BIG_ENDIAN 1
#define BITS_PER_UNIT 8
#define BITS_PER_WORD 64
#define UNITS_PER_WORD 8
#define POINTER_SIZE 64

/* FIXME: This macro is correlated to MAX_FIXED_MODE_SIZE in that
   e.g. this macro must not be 8 (default, UNITS_PER_WORD) when
   MAX_FIXED_MODE_SIZE is 64 (default, DImode), or really: this must be
   set manually if MAX_FIXED_MODE_SIZE is not at least twice the register
   size.  By setting it to 4, we don't have to worry about TImode things
   yet.  Revisit, perhaps get TImode going or get some solution that does
   not mandate TImode or lie in other ways.  */
#define MIN_UNITS_PER_WORD 4

/* FIXME: Promotion of modes currently generates slow code, extending
   before every operation.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
 do {						\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 8)		\
   {						\
     (MODE) = DImode;				\
     /* Do the following some time later,	\
	scrutinizing differences.  */		\
     if (0) (UNSIGNEDP) = 0;			\
   }						\
 } while (0)

#define PROMOTE_FUNCTION_ARGS

#if 0
/* Apparently not doing TRT if int < register-size.  FIXME: Perhaps
   FUNCTION_VALUE and LIBCALL_VALUE needs tweaking as some ports say.  */
#define PROMOTE_FUNCTION_RETURN
#endif

/* I'm a little bit undecided about this one.  It might be beneficial to
   promote all operations.  */
#define PROMOTE_FOR_CALL_ONLY

/* We need to align everything to 64 bits that can affect the alignment
   of other types.  Since address N is interpreted in MMIX as (N modulo
   access_size), we must align.  */
#define PARM_BOUNDARY 64
#define STACK_BOUNDARY 64
#define FUNCTION_BOUNDARY 32
#define BIGGEST_ALIGNMENT 64

/* This one is only used in the ADA front end.  */
#define MINIMUM_ATOMIC_ALIGNMENT 8

/* Copied from elfos.h.  */
#define MAX_OFILE_ALIGNMENT (32768 * 8)

#define DATA_ALIGNMENT(TYPE, BASIC_ALIGN) \
 mmix_data_alignment (TYPE, BASIC_ALIGN)

#define CONSTANT_ALIGNMENT(CONSTANT, BASIC_ALIGN) \
 mmix_constant_alignment (CONSTANT, BASIC_ALIGN)

#define LOCAL_ALIGNMENT(TYPE, BASIC_ALIGN) \
 mmix_local_alignment (TYPE, BASIC_ALIGN)

/* Following other ports, this seems to most commonly be the word-size,
   so let's do that here too.  */
#define EMPTY_FIELD_BOUNDARY 64

/* We chose to have this low solely for similarity with the alpha.  It has
   nothing to do with passing the tests dg/c99-scope-2 and
   execute/align-1.c.  Nothing.  Though the tests seem wrong.  Padding of
   the structure is automatically added to get alignment when needed if we
   set this to just byte-boundary.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* The lower bits are ignored.  */
#define STRICT_ALIGNMENT 1


/* Node: Type Layout */

/* It might seem more natural to have 64-bit ints on a 64-bit machine,
   but then an occasional MMIX programmer needs to know how to put a lot
   of __attribute__ stuff to get to the 8, 16 and 32-bit modes rather
   than the "intuitive" char, short and int types.  */
#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1


/* Node: Register Basics */
/* We tell GCC about all 256 general registers, and we also include
   rD, rE, rH, rJ and rR (in that order) so we can describe what insns
   clobber them.  We use a faked register for the argument pointer.  It is
   always eliminated towards the frame-pointer or the stack-pointer, never
   output in assembly.  Any fixed register would do for this, like $255,
   but future debugging is easier when using a separate register.  It
   counts as a global register for pseudorandom reasons.  */
#define FIRST_PSEUDO_REGISTER 262

/* We treat general registers with no assigned purpose as fixed.  The
   stack pointer, $254, is also fixed.  Register $255 is referred to as a
   temporary register in the MMIX papers, and used as such in mmixal, so
   it should not be used as a stack pointer.  We set it to fixed, and use
   it "manually" at times of despair.  */
#define FIXED_REGISTERS \
 { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, \
   1, 1, 0, 0, 0, 1 \
 }

/* General registers are fixed and therefore "historically" marked
   call-used.  (FIXME: This has changed).  Registers $15..$31 are
   call-clobbered; we'll put arguments in $16 and up, and we need $15 for
   the MMIX register-stack "hole".  */
#define CALL_USED_REGISTERS \
 { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, \
   1, 1, 1, 1, 1, 1 \
 }

#define CONDITIONAL_REGISTER_USAGE mmix_conditional_register_usage ()

/* No LOCAL_REGNO, INCOMING_REGNO or OUTGOING_REGNO, since those macros
   are not usable for MMIX: it doesn't have a fixed register window size.
   FIXME: Perhaps we should say something about $0..$15 may sometimes be
   the incoming $16..$31.  Those macros need better documentation; it
   looks like they're just bogus and that FUNCTION_INCOMING_ARG_REGNO_P
   and FUNCTION_OUTGOING_VALUE should be used where they're used.  For the
   moment, do nothing; things seem to work anyway.  */


/* Node: Allocation Order */

/* We should allocate registers from 0 to 31 by increasing number, because
   I think that's what people expect.  Beyond that, just use
   call-clobbered global registers first, then call-clobbered special
   registers.  Last, the fixed registers.  */
#define MMIX_MMIXWARE_ABI_REG_ALLOC_ORDER	\
 { 0, 1, 2, 3, 4, 5, 6, 7,			\
   8, 9, 10, 11, 12, 13, 14, 15,		\
   16, 17, 18, 19, 20, 21, 22, 23,		\
   24, 25, 26, 27, 28, 29, 30, 31,    		\
						\
   252, 251, 250, 249, 248, 247, 		\
						\
   253,						\
						\
   258, 260, 259,				\
						\
   32, 33, 34, 35, 36, 37, 38, 39,		\
   40, 41, 42, 43, 44, 45, 46, 47,		\
   48, 49, 50, 51, 52, 53, 54, 55,		\
   56, 57, 58, 59, 60, 61, 62, 63,		\
   64, 65, 66, 67, 68, 69, 70, 71,		\
   72, 73, 74, 75, 76, 77, 78, 79,		\
   80, 81, 82, 83, 84, 85, 86, 87,		\
   88, 89, 90, 91, 92, 93, 94, 95,		\
   96, 97, 98, 99, 100, 101, 102, 103,		\
   104, 105, 106, 107, 108, 109, 110, 111,	\
   112, 113, 114, 115, 116, 117, 118, 119,	\
   120, 121, 122, 123, 124, 125, 126, 127,	\
   128, 129, 130, 131, 132, 133, 134, 135,	\
   136, 137, 138, 139, 140, 141, 142, 143,	\
   144, 145, 146, 147, 148, 149, 150, 151,	\
   152, 153, 154, 155, 156, 157, 158, 159,	\
   160, 161, 162, 163, 164, 165, 166, 167,	\
   168, 169, 170, 171, 172, 173, 174, 175,	\
   176, 177, 178, 179, 180, 181, 182, 183,	\
   184, 185, 186, 187, 188, 189, 190, 191,	\
   192, 193, 194, 195, 196, 197, 198, 199,	\
   200, 201, 202, 203, 204, 205, 206, 207,	\
   208, 209, 210, 211, 212, 213, 214, 215,	\
   216, 217, 218, 219, 220, 221, 222, 223,	\
   224, 225, 226, 227, 228, 229, 230, 231,	\
   232, 233, 234, 235, 236, 237, 238, 239,	\
   240, 241, 242, 243, 244, 245, 246,		\
						\
   254, 255, 256, 257, 261 			\
 }

/* As a convenience, we put this nearby, for ease of comparison.
   First, call-clobbered registers in reverse order of assignment as
   parameters (also the top ones; not because they're parameters, but
   for continuity).

   Second, saved registers that go on the register-stack.

   Third, special registers rH, rR and rJ.  They should not normally be
   allocated, but since they're call-clobbered, it is cheaper to use one
   of them than using a call-saved register for a call-clobbered use,
   assuming it is referenced a very limited number of times.  Other global
   and fixed registers come next; they are never allocated.  */
#define MMIX_GNU_ABI_REG_ALLOC_ORDER		\
 { 252, 251, 250, 249, 248, 247, 246,		\
   245, 244, 243, 242, 241, 240, 239, 238,	\
   237, 236, 235, 234, 233, 232, 231,		\
						\
   0, 1, 2, 3, 4, 5, 6, 7,			\
   8, 9, 10, 11, 12, 13, 14, 15,		\
   16, 17, 18, 19, 20, 21, 22, 23,		\
   24, 25, 26, 27, 28, 29, 30, 31,		\
						\
   253,						\
						\
   258, 260, 259,				\
						\
   32, 33, 34, 35, 36, 37, 38, 39,		\
   40, 41, 42, 43, 44, 45, 46, 47,		\
   48, 49, 50, 51, 52, 53, 54, 55,		\
   56, 57, 58, 59, 60, 61, 62, 63,		\
   64, 65, 66, 67, 68, 69, 70, 71,		\
   72, 73, 74, 75, 76, 77, 78, 79,		\
   80, 81, 82, 83, 84, 85, 86, 87,		\
   88, 89, 90, 91, 92, 93, 94, 95,		\
   96, 97, 98, 99, 100, 101, 102, 103,		\
   104, 105, 106, 107, 108, 109, 110, 111,	\
   112, 113, 114, 115, 116, 117, 118, 119,	\
   120, 121, 122, 123, 124, 125, 126, 127,	\
   128, 129, 130, 131, 132, 133, 134, 135,	\
   136, 137, 138, 139, 140, 141, 142, 143,	\
   144, 145, 146, 147, 148, 149, 150, 151,	\
   152, 153, 154, 155, 156, 157, 158, 159,	\
   160, 161, 162, 163, 164, 165, 166, 167,	\
   168, 169, 170, 171, 172, 173, 174, 175,	\
   176, 177, 178, 179, 180, 181, 182, 183,	\
   184, 185, 186, 187, 188, 189, 190, 191,	\
   192, 193, 194, 195, 196, 197, 198, 199,	\
   200, 201, 202, 203, 204, 205, 206, 207,	\
   208, 209, 210, 211, 212, 213, 214, 215,	\
   216, 217, 218, 219, 220, 221, 222, 223,	\
   224, 225, 226, 227, 228, 229, 230,		\
						\
   254, 255, 256, 257, 261 			\
 }

/* The default one.  */
#define REG_ALLOC_ORDER MMIX_MMIXWARE_ABI_REG_ALLOC_ORDER

/* Node: Values in Registers */

#define HARD_REGNO_NREGS(REGNO, MODE)            	\
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  	\
    / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Note that no register can really be accessed in single-float mode, so
   we *can* say 1 here.  FIXME:  Will TRT happen for single-float, or do
   we have to punt to libgcc1.asm?  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1


/* Node: Leaf Functions */
/* (empty) */


/* Node: Register Classes */

enum reg_class
 {
   NO_REGS, GENERAL_REGS, REMAINDER_REG, HIMULT_REG,
   SYSTEM_REGS, ALL_REGS, LIM_REG_CLASSES
 };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES						\
 {"NO_REGS", "GENERAL_REGS", "REMAINDER_REG", "HIMULT_REG",	\
  "SYSTEM_REGS", "ALL_REGS"}

/* Note that the contents of each item is always 32 bits.  */
#define REG_CLASS_CONTENTS			\
 {{0, 0, 0, 0, 0, 0, 0, 0, 0},			\
  {~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, 0x20},	\
  {0, 0, 0, 0, 0, 0, 0, 0, 0x10},		\
  {0, 0, 0, 0, 0, 0, 0, 0, 4},			\
  {0, 0, 0, 0, 0, 0, 0, 0, 0x3f},		\
  {~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, 0x3f}}

#define REGNO_REG_CLASS(REGNO)					\
 ((REGNO) <= MMIX_LAST_GENERAL_REGISTER				\
  || (REGNO) == MMIX_ARG_POINTER_REGNUM				\
  ? GENERAL_REGS						\
  : (REGNO) == MMIX_REMAINDER_REGNUM ? REMAINDER_REG		\
  : (REGNO) == MMIX_HIMULT_REGNUM ? HIMULT_REG : SYSTEM_REGS)

#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS GENERAL_REGS

#define REG_CLASS_FROM_LETTER(CHAR)		\
 ((CHAR) == 'x' ? SYSTEM_REGS			\
  : (CHAR) == 'y' ? REMAINDER_REG		\
  : (CHAR) == 'z' ? HIMULT_REG : NO_REGS)

#define REGNO_OK_FOR_BASE_P(REGNO)				\
 ((REGNO) <= MMIX_LAST_GENERAL_REGISTER				\
  || (REGNO) == MMIX_ARG_POINTER_REGNUM				\
  || (reg_renumber[REGNO] > 0					\
      && reg_renumber[REGNO] <= MMIX_LAST_GENERAL_REGISTER))

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P (REGNO)

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
 mmix_preferred_reload_class (X, CLASS)

#define PREFERRED_OUTPUT_RELOAD_CLASS(X, CLASS) \
 mmix_preferred_output_reload_class (X, CLASS)

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) \
 mmix_secondary_reload_class (CLASS, MODE, X, 1)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) \
 mmix_secondary_reload_class (CLASS, MODE, X, 0)

#define CLASS_MAX_NREGS(CLASS, MODE) HARD_REGNO_NREGS (CLASS, MODE)

#define CONST_OK_FOR_LETTER_P(VALUE, C)	\
 mmix_const_ok_for_letter_p (VALUE, C)

#define EXTRA_CONSTRAINT(VALUE, C)	\
 mmix_extra_constraint (VALUE, C, MMIX_REG_OK_STRICT)

/* Do we need anything serious here?  Yes, any FLOT constant.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)			\
 mmix_const_double_ok_for_letter_p (VALUE, C)


/* Node: Frame Layout */

#define STACK_GROWS_DOWNWARD
#define FRAME_GROWS_DOWNWARD

#define STARTING_FRAME_OFFSET \
  mmix_starting_frame_offset ()

#define FIRST_PARM_OFFSET(FUNDECL) 0

#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) \
 mmix_dynamic_chain_address (FRAMEADDR)

/* FIXME: It seems RETURN_ADDR_OFFSET is undocumented.  */

#define SETUP_FRAME_ADDRESSES() \
 mmix_setup_frame_addresses ()

#define RETURN_ADDR_RTX(COUNT, FRAME)		\
 mmix_return_addr_rtx (COUNT, FRAME)

/* It's in rJ before we store it somewhere.  */
#define INCOMING_RETURN_ADDR_RTX \
 gen_rtx_REG (Pmode, MMIX_INCOMING_RETURN_ADDRESS_REGNUM)

/* FIXME: This does not seem properly documented or cross-indexed.
   Nowhere except in the code does it say it *has* to be in the range
   0..255, or else it will be truncated.  That goes for the default too.  */
#define DWARF_FRAME_RETURN_COLUMN \
 DWARF_FRAME_REGNUM (MMIX_INCOMING_RETURN_ADDRESS_REGNUM)

/* No return address is stored there.  */
#define INCOMING_FRAME_SP_OFFSET 0

/* Node: Stack Checking */
/* (empty) */


/* Node: Exception Handling */

#define EH_RETURN_DATA_REGNO(N) \
 mmix_eh_return_data_regno (N)

#define EH_RETURN_STACKADJ_RTX \
 mmix_eh_return_stackadj_rtx ()

#define EH_RETURN_HANDLER_RTX \
 mmix_eh_return_handler_rtx ()

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
 mmix_asm_preferred_eh_data_format (CODE, GLOBAL)

/* Node: Frame Registers */
#define STACK_POINTER_REGNUM MMIX_STACK_POINTER_REGNUM

/* Perhaps we can use HARD_FRAME_POINTER_REGNUM and decide later on
   what register we want to use.  */
#define FRAME_POINTER_REGNUM MMIX_FRAME_POINTER_REGNUM
#define ARG_POINTER_REGNUM MMIX_ARG_POINTER_REGNUM

#define STATIC_CHAIN_REGNUM MMIX_STATIC_CHAIN_REGNUM


/* Node: Elimination */
/* FIXME: Is this requirement built-in?  Anyway, we should try to get rid
   of it; we can deduce the value.  */
#define FRAME_POINTER_REQUIRED (nonlocal_goto_stack_level != NULL_RTX)

/* The frame-pointer is stored in a location that either counts to the
   offset of incoming parameters, or that counts to the offset of the
   frame, so we can't use a single offset.  We therefore eliminate those
   two separately.  */
#define ELIMINABLE_REGS				\
 {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* We need not worry about when the frame-pointer is required for other
   reasons; GCC takes care of those cases.  */
#define CAN_ELIMINATE(FROM, TO) 1

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
 (OFFSET) = mmix_initial_elimination_offset (FROM, TO)


/* Node: Stack Arguments */

#define ACCUMULATE_OUTGOING_ARGS 1

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACKSIZE) 0


/* Node: Register Arguments */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)	\
 mmix_function_arg (&(CUM), MODE, TYPE, NAMED, 0)

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)	\
 mmix_function_arg (&(CUM), MODE, TYPE, NAMED, 1)

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)	\
 mmix_function_arg_pass_by_reference (&(CUM), MODE, TYPE, NAMED)

/* This *sounds* good, but does not seem to be implemented correctly to
   be a win; at least it wasn't in 2.7.2.  FIXME: Check and perhaps
   replace with a big comment.  */
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) 1

typedef struct { int regs; int lib; int now_varargs; } CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT)	\
 ((CUM).regs = 0, (CUM).lib = ((LIBNAME) != 0), (CUM).now_varargs = 0)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
 ((CUM).regs							\
  = ((MUST_PASS_IN_STACK (MODE, TYPE))				\
     || (MMIX_FUNCTION_ARG_SIZE (MODE, TYPE) > 8		\
	 && !TARGET_LIBFUNC && !(CUM).lib))			\
  ? (MMIX_MAX_ARGS_IN_REGS) + 1					\
  : (CUM).regs + (7 + (MMIX_FUNCTION_ARG_SIZE (MODE, TYPE))) / 8)

#define FUNCTION_ARG_REGNO_P(REGNO)		\
 mmix_function_arg_regno_p (REGNO, 0)

#define FUNCTION_INCOMING_ARG_REGNO_P(REGNO)		\
 mmix_function_arg_regno_p (REGNO, 1)


/* Node: Register Arguments */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
 gen_rtx_REG (TYPE_MODE (VALTYPE), MMIX_RETURN_VALUE_REGNUM)

/* This needs to take care of the register hole for complex return values.  */
#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)  \
 mmix_function_outgoing_value (VALTYPE, FUNC)

#define LIBCALL_VALUE(MODE) \
 gen_rtx_REG (MODE, MMIX_OUTGOING_RETURN_VALUE_REGNUM)

#define FUNCTION_VALUE_REGNO_P(REGNO) \
 ((REGNO) == MMIX_OUTGOING_RETURN_VALUE_REGNUM)


/* Node: Aggregate Return */

#define STRUCT_VALUE_REGNUM MMIX_STRUCT_VALUE_REGNUM


/* Node: Caller Saves */
/* (empty) */


/* Node: Function Entry */

/* See mmix.c for TARGET_ASM_FUNCTION_PROLOGUE and
   TARGET_ASM_FUNCTION_EPILOGUE.  */

/* We need to say that the epilogue uses the return address, so the
   initial-value machinery restores it.  FIXME: Some targets
   conditionalize on "reload_completed &&".  Investigate difference.
   FIXME: Not needed if nonlocal_goto_stack_level.  */
#define EPILOGUE_USES(REGNO) \
 ((REGNO) == MMIX_INCOMING_RETURN_ADDRESS_REGNUM)

#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	\
 mmix_asm_output_mi_thunk (FILE, THUNK_FNDECL, DELTA, FUNCTION)


/* Node: Profiling */
#define FUNCTION_PROFILER(FILE, LABELNO)	\
 mmix_function_profiler (FILE, LABELNO)

/* Node: Varargs */

/* For the moment, let's stick to pushing argument registers on the stack.
   Later, we can parse all arguments in registers, to improve
   performance.  */
#define SETUP_INCOMING_VARARGS(A, M, T, P, S)	\
 mmix_setup_incoming_varargs(&(A), M, T, &(P), S)

/* FIXME: This and other EXPAND_BUILTIN_VA_... target macros are not
   documented, although used by several targets.  */
#define EXPAND_BUILTIN_VA_ARG(VALIST, TYPE) \
 mmix_expand_builtin_va_arg (VALIST, TYPE)

/* Node: Trampolines */

#define TRAMPOLINE_TEMPLATE(FILE) \
 mmix_trampoline_template (FILE)

#define TRAMPOLINE_SIZE mmix_trampoline_size
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
 mmix_initialize_trampoline (ADDR, FNADDR, STATIC_CHAIN)


/* Node: Library Calls */

#define TARGET_MEM_FUNCTIONS


/* Node: Addressing Modes */

#define CONSTANT_ADDRESS_P(X) \
 mmix_constant_address_p (X)

#define MAX_REGS_PER_ADDRESS 2

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)		\
 if (mmix_legitimate_address (MODE, X, MMIX_REG_OK_STRICT))	\
   goto LABEL

#ifndef REG_OK_STRICT
# define REG_OK_FOR_BASE_P(X)			\
  (REGNO (X) <= MMIX_LAST_GENERAL_REGISTER	\
   || REGNO (X) == MMIX_ARG_POINTER_REGNUM	\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)
#else
# define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#endif /* REG_OK_STRICT */

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

#define LEGITIMATE_CONSTANT_P(X) \
 mmix_legitimate_constant_p (X)


/* Node: Condition Code */

#define EXTRA_CC_MODES				\
 CC(CC_UNSmode, "CC_UNS")			\
 CC(CC_FPmode, "CC_FP")				\
 CC(CC_FPEQmode, "CC_FPEQ")			\
 CC(CC_FUNmode, "CC_FUN")

#define SELECT_CC_MODE(OP, X, Y)		\
 mmix_select_cc_mode (OP, X, Y)

#define CANONICALIZE_COMPARISON(CODE, OP0, OP1)		\
 mmix_canonicalize_comparison (&(CODE), &(OP0), &(OP1));

#define REVERSIBLE_CC_MODE(MODE)		\
 mmix_reversible_cc_mode (MODE)


/* Node: Costs */

/* This one takes on both the RTX_COSTS and CONST_COSTS tasks.  */
#define DEFAULT_RTX_COSTS(X, CODE, OUTER_CODE)			\
 {								\
   int mmix_rtx_cost;						\
   if (mmix_rtx_cost_recalculated (X, CODE, OUTER_CODE, 	\
				   &mmix_rtx_cost))		\
     return mmix_rtx_cost;					\
 }

#define ADDRESS_COST(ADDRESS) mmix_address_cost (ADDRESS)

/* The special registers can only move to and from general regs, and we
   need to check that their constraints match, so say 3 for them.  */
/* WARNING: gcc-2.7.2.2 i686-pc-linux-gnulibc1 (as shipped with RH 4.2)
   miscompiles reload1.c:reload_cse_simplify_set; a call to
   reload_cse_regno_equal_p is missing when checking if a substitution of
   a register setting is valid if this is defined to just the expression
   in mmix_register_move_cost.

   Symptom: a (all?) register setting is optimized away for e.g.
   "char *p1(char *p) { return p+1; }" and the value of register zero ($0)
   is returned.

   We can workaround by making this a function call - unknown if this
   causes dire speed effects.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO) \
 mmix_register_move_cost (MODE, FROM, TO)

#define SLOW_BYTE_ACCESS 0


/* Node: Sections */

/* This must be a constant string, since it's used in crtstuff.c.  */
#define TEXT_SECTION_ASM_OP \
 "\t.text ! mmixal:= 9H LOC 8B"

/* FIXME: Not documented.  */
#define DATA_SECTION_ASM_OP \
 mmix_data_section_asm_op ()

/* Stuff copied from elfos.h.  */
#define EXTRA_SECTIONS in_const

#define EXTRA_SECTION_FUNCTIONS		\
  CONST_SECTION_FUNCTION

#define READONLY_DATA_SECTION() const_section ()

#define CONST_SECTION_ASM_OP	"\t.section\t.rodata"

#define CONST_SECTION_FUNCTION					\
void								\
const_section ()						\
{								\
  if (in_section != in_const)					\
    {								\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);	\
      in_section = in_const;					\
    }								\
}

#undef  SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, RTX, ALIGN) const_section ()

#define SELECT_SECTION(DECL, RELOC, ALIGN) \
 mmix_select_section (DECL, RELOC, ALIGN)

#define ENCODE_SECTION_INFO(DECL) \
 mmix_encode_section_info (DECL)

#define STRIP_NAME_ENCODING(VAR, SYM_NAME) \
 (VAR) = mmix_strip_name_encoding (SYM_NAME)

#define UNIQUE_SECTION(DECL, RELOC) \
  mmix_unique_section (decl, reloc)

/* Node: PIC */
/* (empty) */


/* Node: File Framework */

#define ASM_FILE_START(STREAM) \
 mmix_asm_file_start (STREAM)

#define ASM_FILE_END(STREAM) \
 mmix_asm_file_end (STREAM)

/* While any other punctuation character but ";" would do, we prefer "%"
   or "!"; "!" is an unary operator and so will not be mistakenly included
   in correctly formed expressions.  The hash character adds mass; catches
   the eye.  We can't have it as a comment char by itself, since it's a
   hex-number prefix.  */
#define ASM_COMMENT_START "!#"

/* These aren't currently functional.  We just keep them as markers.  */
#define ASM_APP_ON "%APP\n"
#define ASM_APP_OFF "%NO_APP\n"

#define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME) \
 mmix_asm_output_source_filename (STREAM, NAME)

#define OUTPUT_QUOTED_STRING(STREAM, STRING) \
 mmix_output_quoted_string (STREAM, STRING, strlen (STRING))

#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE) \
 mmix_asm_output_source_line  (STREAM, LINE)

#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section


/* Node: Data Output */

#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN) \
 mmix_asm_output_ascii (STREAM, PTR, LEN)

/* Node: Uninitialized Data */

#define ASM_OUTPUT_ALIGNED_COMMON(ST, N, S, A) \
 mmix_asm_output_aligned_common (ST, N, S, A)

#define ASM_OUTPUT_ALIGNED_LOCAL(ST, N, S, A) \
 mmix_asm_output_aligned_local (ST, N, S, A)


/* Node: Label Output */

#define ASM_OUTPUT_LABEL(STREAM, NAME) \
 mmix_asm_output_label (STREAM, NAME)

#define ASM_DECLARE_REGISTER_GLOBAL(STREAM, DECL, REGNO, NAME) \
 mmix_asm_declare_register_global (STREAM, DECL, REGNO, NAME)

#define ASM_GLOBALIZE_LABEL(STREAM, NAME) \
 mmix_asm_globalize_label (STREAM, NAME)

#define ASM_WEAKEN_LABEL(STREAM, NAME) \
 mmix_asm_weaken_label (STREAM, NAME)

#define MAKE_DECL_ONE_ONLY(DECL) \
 mmix_make_decl_one_only (DECL)

#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
 mmix_asm_output_labelref (STREAM, NAME)

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM) \
 mmix_asm_output_internal_label (STREAM, PREFIX, NUM)

/* We insert a ":" to disambiguate against user symbols like L5.  */
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
 sprintf (LABEL, "*%s:%ld", PREFIX, (long)(NUM))

/* Insert "::"; these are rarer than internal labels.  FIXME: Make sure no
   ":" is seen in the object file; we don't really want that mmixal
   feature visible there.  We don't want the default, which uses a dot;
   that'd be incompatible with mmixal.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)		\
 ((OUTPUT) = (char *) alloca (strlen ((NAME)) + 2 + 10),	\
  sprintf ((OUTPUT), "%s::%d", (NAME), (LABELNO)))

#define ASM_OUTPUT_DEF(STREAM, NAME, VALUE) \
 mmix_asm_output_def (STREAM, NAME, VALUE)

#define ASM_OUTPUT_DEFINE_LABEL_DIFFERENCE_SYMBOL(STREAM, SY, HI, LO) \
 mmix_asm_output_define_label_difference_symbol (STREAM, SY, HI, LO)


/* Node: Macros for Initialization */
/* We're compiling to ELF and linking to MMO; all ELF features that GCC
   care for are there.  FIXME: Are they?  */

/* These must be constant strings, since they're used in crtstuff.c.  */
#define INIT_SECTION_ASM_OP "\t.section .init,\"ax\" ! mmixal-incompatible"

#define FINI_SECTION_ASM_OP "\t.section .fini,\"ax\" ! mmixal-incompatible"

#define OBJECT_FORMAT_ELF


/* Node: Instruction Output */

/* The non-$ register names must be prefixed with ":", since they're
   affected by PREFIX.  We provide the non-colon names as additional
   names.  */
#define REGISTER_NAMES							\
 {"$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7",			\
  "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15",			\
  "$16", "$17", "$18", "$19", "$20", "$21", "$22", "$23",		\
  "$24", "$25", "$26", "$27", "$28", "$29", "$30", "$31",		\
  "$32", "$33", "$34", "$35", "$36", "$37", "$38", "$39",		\
  "$40", "$41", "$42", "$43", "$44", "$45", "$46", "$47",		\
  "$48", "$49", "$50", "$51", "$52", "$53", "$54", "$55",		\
  "$56", "$57", "$58", "$59", "$60", "$61", "$62", "$63",		\
  "$64", "$65", "$66", "$67", "$68", "$69", "$70", "$71",		\
  "$72", "$73", "$74", "$75", "$76", "$77", "$78", "$79",		\
  "$80", "$81", "$82", "$83", "$84", "$85", "$86", "$87",		\
  "$88", "$89", "$90", "$91", "$92", "$93", "$94", "$95",		\
  "$96", "$97", "$98", "$99", "$100", "$101", "$102", "$103",		\
  "$104", "$105", "$106", "$107", "$108", "$109", "$110", "$111",	\
  "$112", "$113", "$114", "$115", "$116", "$117", "$118", "$119",	\
  "$120", "$121", "$122", "$123", "$124", "$125", "$126", "$127",	\
  "$128", "$129", "$130", "$131", "$132", "$133", "$134", "$135",	\
  "$136", "$137", "$138", "$139", "$140", "$141", "$142", "$143",	\
  "$144", "$145", "$146", "$147", "$148", "$149", "$150", "$151",	\
  "$152", "$153", "$154", "$155", "$156", "$157", "$158", "$159",	\
  "$160", "$161", "$162", "$163", "$164", "$165", "$166", "$167",	\
  "$168", "$169", "$170", "$171", "$172", "$173", "$174", "$175",	\
  "$176", "$177", "$178", "$179", "$180", "$181", "$182", "$183",	\
  "$184", "$185", "$186", "$187", "$188", "$189", "$190", "$191",	\
  "$192", "$193", "$194", "$195", "$196", "$197", "$198", "$199",	\
  "$200", "$201", "$202", "$203", "$204", "$205", "$206", "$207",	\
  "$208", "$209", "$210", "$211", "$212", "$213", "$214", "$215",	\
  "$216", "$217", "$218", "$219", "$220", "$221", "$222", "$223",	\
  "$224", "$225", "$226", "$227", "$228", "$229", "$230", "$231",	\
  "$232", "$233", "$234", "$235", "$236", "$237", "$238", "$239",	\
  "$240", "$241", "$242", "$243", "$244", "$245", "$246", "$247",	\
  "$248", "$249", "$250", "$251", "$252", "$253", "$254", "$255",	\
  ":rD",  ":rE",  ":rH",  ":rJ",  ":rR",  "ap_!BAD!"}

#define ADDITIONAL_REGISTER_NAMES			\
 {{"sp", 254}, {":sp", 254}, {"rD", 256}, {"rE", 257},	\
  {"rH", 258}, {"rJ", MMIX_rJ_REGNUM}}

#define PRINT_OPERAND(STREAM, X, CODE) \
 mmix_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) \
 mmix_print_operand_punct_valid_p (CODE)

#define PRINT_OPERAND_ADDRESS(STREAM, X) \
 mmix_print_operand_address (STREAM, X)

#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO) \
 mmix_asm_output_reg_push (STREAM, REGNO)

#define ASM_OUTPUT_REG_POP(STREAM, REGNO) \
 mmix_asm_output_reg_pop (STREAM, REGNO)


/* Node: Dispatch Tables */

/* We define both types, since SImode is the better, but DImode the only
   possible for mmixal so that's the one actually used.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
 mmix_asm_output_addr_diff_elt (STREAM, BODY, VALUE, REL)

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
 mmix_asm_output_addr_vec_elt (STREAM, VALUE)


/* Node: Exception Region Output */
/* (empty) */

/* Node: Alignment Output */

#define ASM_OUTPUT_SKIP(STREAM, NBYTES) \
 mmix_asm_output_skip (STREAM, NBYTES)

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
 mmix_asm_output_align (STREAM, POWER)


/* Node: All Debuggers */

#define DBX_REGISTER_NUMBER(REGNO) \
 mmix_dbx_register_number (REGNO)


/* Node: DBX Options */
/* (empty) */
/* Node: DBX Hooks */
/* (empty) */
/* Node: File Names and DBX */
/* (empty) */


/* Node: SDB and DWARF */
#define DWARF2_DEBUGGING_INFO
#define DWARF2_ASM_LINE_DEBUG_INFO 1

/* Node: Cross-compilation */

/* FIXME: I don't know whether it is best to tweak emit-rtl.c to handle
   the case where sizeof (float) == word_size / 2 on the target, or to fix
   real.h to define REAL_ARITHMETIC in that case.  Anyway, it should be
   documented that a target can define this to force emulation.  Note that
   we don't check #ifdef CROSS_COMPILE here; not even if mmix gets
   self-hosted must we do that.  Case gcc.c-torture/compile/930611-1.c.  */
#define REAL_ARITHMETIC


/* Node: Misc */

#define PREDICATE_CODES				\
 {"mmix_reg_cc_operand", {SUBREG, REG}},	\
 {"mmix_foldable_comparison_operator",		\
  {NE, EQ, GE, GT, LE, LT}},			\
 /* All '<', actually.  */			\
 {"mmix_comparison_operator",			\
  {NE, EQ, GE, GT, LE, LT, GEU, GTU, LEU,	\
   LTU, UNORDERED, ORDERED, UNEQ, UNGE, UNLE,	\
   UNLT, LTGT}},				\
 {"mmix_symbolic_or_address_operand",		\
  {SYMBOL_REF, LABEL_REF, CONST,		\
   SUBREG, REG, PLUS}},				\
 {"mmix_reg_or_constant_operand",		\
  {CONST_INT, CONST_DOUBLE, SUBREG, REG}},	\
 {"mmix_reg_or_8bit_or_256_operand",		\
  {CONST_INT, CONST_DOUBLE, SUBREG, REG}},	\
 {"mmix_reg_or_8bit_operand",			\
  {CONST_INT, CONST_DOUBLE, SUBREG, REG}},	\
 {"mmix_reg_or_0_operand",			\
  {CONST_INT, CONST_DOUBLE, SUBREG, REG}},

#define SPECIAL_MODE_PREDICATES "mmix_symbolic_or_address_operand",

/* There's no way to get a PC-relative offset into tables for SImode, so
   for the moment we have absolute entries in DImode.
   When we're going ELF, these should be SImode and 1.  */
#define CASE_VECTOR_MODE DImode
#define CASE_VECTOR_PC_RELATIVE 0

#define WORD_REGISTER_OPERATIONS

/* We have a choice, which makes this yet another parameter to tweak.  The
   gut feeling is currently that SIGN_EXTEND wins; "int" is more frequent
   than "unsigned int", and we have signed characters.  FIXME: measure.  */
#define LOAD_EXTEND_OP(MODE) (TARGET_ZERO_EXTEND ? ZERO_EXTEND : SIGN_EXTEND)

#define MOVE_MAX 8

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We have a choice here too.  */
#if 0
/* FIXME:  Revisit, we don't have scc expanders yet.  */
#define STORE_FLAG_VALUE 1
#endif

#define Pmode DImode

#define FUNCTION_MODE QImode

/* When in due time we *will* have some specific headers.  */
#define NO_IMPLICIT_EXTERN_C

#define HANDLE_SYSV_PRAGMA

/* These are checked.  */
#define DOLLARS_IN_IDENTIFIERS 0
#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

#endif /* GCC_MMIX_H */
/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
