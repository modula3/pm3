/* Definitions of target machine for GNU compiler, for Sun SPARC.
   Copyright (C) 1987, 1988, 1989, 1992, 1994, 1995, 1996, 1997, 1998, 1999
   2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).
   64 bit SPARC V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

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

/* Note that some other tm.h files include this one and then override
   whatever definitions are necessary.  */

/* Specify this in a cover file to provide bi-architecture (32/64) support.  */
/* #define SPARC_BI_ARCH */

/* Macro used later in this file to determine default architecture.  */
#define DEFAULT_ARCH32_P ((TARGET_DEFAULT & MASK_64BIT) == 0)

/* TARGET_ARCH{32,64} are the main macros to decide which of the two
   architectures to compile for.  We allow targets to choose compile time or
   runtime selection.  */
#ifdef IN_LIBGCC2
#if defined(__sparcv9) || defined(__arch64__)
#define TARGET_ARCH32 0
#else
#define TARGET_ARCH32 1
#endif /* sparc64 */
#else
#ifdef SPARC_BI_ARCH
#define TARGET_ARCH32 (! TARGET_64BIT)
#else
#define TARGET_ARCH32 (DEFAULT_ARCH32_P)
#endif /* SPARC_BI_ARCH */
#endif /* IN_LIBGCC2 */
#define TARGET_ARCH64 (! TARGET_ARCH32)

/* Code model selection.
   -mcmodel is used to select the v9 code model.
   Different code models aren't supported for v7/8 code.

   TARGET_CM_32:     32 bit address space, top 32 bits = 0,
		     pointers are 32 bits.  Note that this isn't intended
                     to imply a v7/8 abi.

   TARGET_CM_MEDLOW: 32 bit address space, top 32 bits = 0,
                     avoid generating %uhi and %ulo terms,
		     pointers are 64 bits.

   TARGET_CM_MEDMID: 64 bit address space.
                     The executable must be in the low 16 TB of memory.
                     This corresponds to the low 44 bits, and the %[hml]44
                     relocs are used.  The text segment has a maximum size
                     of 31 bits.

   TARGET_CM_MEDANY: 64 bit address space.
                     The text and data segments have a maximum size of 31
                     bits and may be located anywhere.  The maximum offset
                     from any instruction to the label _GLOBAL_OFFSET_TABLE_
                     is 31 bits.

   TARGET_CM_EMBMEDANY: 64 bit address space.
                     The text and data segments have a maximum size of 31 bits
                     and may be located anywhere.  Register %g4 contains
                     the start address of the data segment.
*/

enum cmodel {
  CM_32,
  CM_MEDLOW,
  CM_MEDMID,
  CM_MEDANY,
  CM_EMBMEDANY
};

/* Value of -mcmodel specified by user.  */
extern const char *sparc_cmodel_string;
/* One of CM_FOO.  */
extern enum cmodel sparc_cmodel;

/* V9 code model selection.  */
#define TARGET_CM_MEDLOW    (sparc_cmodel == CM_MEDLOW)
#define TARGET_CM_MEDMID    (sparc_cmodel == CM_MEDMID)
#define TARGET_CM_MEDANY    (sparc_cmodel == CM_MEDANY)
#define TARGET_CM_EMBMEDANY (sparc_cmodel == CM_EMBMEDANY)

#define SPARC_DEFAULT_CMODEL CM_32

/* This is call-clobbered in the normal ABI, but is reserved in the
   home grown (aka upward compatible) embedded ABI.  */
#define EMBMEDANY_BASE_REG "%g4"

/* Values of TARGET_CPU_DEFAULT, set via -D in the Makefile,
   and specified by the user via --with-cpu=foo.
   This specifies the cpu implementation, not the architecture size.  */
/* Note that TARGET_CPU_v9 is assumed to start the list of 64-bit 
   capable cpu's.  */
#define TARGET_CPU_sparc	0
#define TARGET_CPU_v7		0	/* alias for previous */
#define TARGET_CPU_sparclet	1
#define TARGET_CPU_sparclite	2
#define TARGET_CPU_v8		3	/* generic v8 implementation */
#define TARGET_CPU_supersparc	4
#define TARGET_CPU_hypersparc   5
#define TARGET_CPU_sparc86x	6
#define TARGET_CPU_sparclite86x	6
#define TARGET_CPU_v9		7	/* generic v9 implementation */
#define TARGET_CPU_sparcv9	7	/* alias */
#define TARGET_CPU_sparc64	7	/* alias */
#define TARGET_CPU_ultrasparc	8

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9 \
 || TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc

#define CPP_CPU32_DEFAULT_SPEC ""
#define ASM_CPU32_DEFAULT_SPEC ""

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
/* ??? What does Sun's CC pass?  */
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
/* ??? It's not clear how other assemblers will handle this, so by default
   use GAS.  Sun's Solaris assembler recognizes -xarch=v8plus, but this case
   is handled in sol2.h.  */
#define ASM_CPU64_DEFAULT_SPEC "-Av9"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#define CPP_CPU64_DEFAULT_SPEC "-D__sparc_v9__"
#define ASM_CPU64_DEFAULT_SPEC "-Av9a"
#endif

#else

#define CPP_CPU64_DEFAULT_SPEC ""
#define ASM_CPU64_DEFAULT_SPEC ""

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparc \
 || TARGET_CPU_DEFAULT == TARGET_CPU_v8
#define CPP_CPU32_DEFAULT_SPEC ""
#define ASM_CPU32_DEFAULT_SPEC ""
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparclet
#define CPP_CPU32_DEFAULT_SPEC "-D__sparclet__"
#define ASM_CPU32_DEFAULT_SPEC "-Asparclet"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparclite
#define CPP_CPU32_DEFAULT_SPEC "-D__sparclite__"
#define ASM_CPU32_DEFAULT_SPEC "-Asparclite"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_supersparc
#define CPP_CPU32_DEFAULT_SPEC "-D__supersparc__ -D__sparc_v8__"
#define ASM_CPU32_DEFAULT_SPEC ""
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_hypersparc
#define CPP_CPU32_DEFAULT_SPEC "-D__hypersparc__ -D__sparc_v8__"
#define ASM_CPU32_DEFAULT_SPEC ""
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_sparclite86x
#define CPP_CPU32_DEFAULT_SPEC "-D__sparclite86x__"
#define ASM_CPU32_DEFAULT_SPEC "-Asparclite"
#endif

#endif

#if !defined(CPP_CPU32_DEFAULT_SPEC) || !defined(CPP_CPU64_DEFAULT_SPEC)
Unrecognized value in TARGET_CPU_DEFAULT.
#endif

#ifdef SPARC_BI_ARCH

#define CPP_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" CPP_CPU64_DEFAULT_SPEC "} \
%{!m64:" CPP_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" CPP_CPU32_DEFAULT_SPEC "} \
%{!m32:" CPP_CPU64_DEFAULT_SPEC "} \
")
#define ASM_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" ASM_CPU64_DEFAULT_SPEC "} \
%{!m64:" ASM_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" ASM_CPU32_DEFAULT_SPEC "} \
%{!m32:" ASM_CPU64_DEFAULT_SPEC "} \
")

#else /* !SPARC_BI_ARCH */

#define CPP_CPU_DEFAULT_SPEC (DEFAULT_ARCH32_P ? CPP_CPU32_DEFAULT_SPEC : CPP_CPU64_DEFAULT_SPEC)
#define ASM_CPU_DEFAULT_SPEC (DEFAULT_ARCH32_P ? ASM_CPU32_DEFAULT_SPEC : ASM_CPU64_DEFAULT_SPEC)

#endif /* !SPARC_BI_ARCH */

/* Define macros to distinguish architectures.  */

/* Common CPP definitions used by CPP_SPEC amongst the various targets
   for handling -mcpu=xxx switches.  */
#define CPP_CPU_SPEC "\
%{msoft-float:-D_SOFT_FLOAT} \
%{mcypress:} \
%{msparclite:-D__sparclite__} \
%{mf930:-D__sparclite__} %{mf934:-D__sparclite__} \
%{mv8:-D__sparc_v8__} \
%{msupersparc:-D__supersparc__ -D__sparc_v8__} \
%{mcpu=sparclet:-D__sparclet__} %{mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite:-D__sparclite__} \
%{mcpu=f930:-D__sparclite__} %{mcpu=f934:-D__sparclite__} \
%{mcpu=v8:-D__sparc_v8__} \
%{mcpu=supersparc:-D__supersparc__ -D__sparc_v8__} \
%{mcpu=hypersparc:-D__hypersparc__ -D__sparc_v8__} \
%{mcpu=sparclite86x:-D__sparclite86x__} \
%{mcpu=v9:-D__sparc_v9__} \
%{mcpu=ultrasparc:-D__sparc_v9__} \
%{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:%(cpp_cpu_default)}}}}}}} \
"

/* ??? The GCC_NEW_VARARGS macro is now obsolete, because gcc always uses
   the right varags.h file when bootstrapping.  */
/* ??? It's not clear what value we want to use for -Acpu/machine for
   sparc64 in 32 bit environments, so for now we only use `sparc64' in
   64 bit environments.  */

#ifdef SPARC_BI_ARCH

#define CPP_ARCH32_SPEC "-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int \
-D__GCC_NEW_VARARGS__ -Acpu=sparc -Amachine=sparc"
#define CPP_ARCH64_SPEC "-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int \
-D__arch64__ -Acpu=sparc64 -Amachine=sparc64"

#else

#define CPP_ARCH32_SPEC "-D__GCC_NEW_VARARGS__ -Acpu=sparc -Amachine=sparc"
#define CPP_ARCH64_SPEC "-D__arch64__ -Acpu=sparc64 -Amachine=sparc64"

#endif

#define CPP_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? CPP_ARCH32_SPEC : CPP_ARCH64_SPEC)

#define CPP_ARCH_SPEC "\
%{m32:%(cpp_arch32)} \
%{m64:%(cpp_arch64)} \
%{!m32:%{!m64:%(cpp_arch_default)}} \
"

/* Macros to distinguish endianness.  */
#define CPP_ENDIAN_SPEC "\
%{mlittle-endian:-D__LITTLE_ENDIAN__} \
%{mlittle-endian-data:-D__LITTLE_ENDIAN_DATA__}"

/* Macros to distinguish the particular subtarget.  */
#define CPP_SUBTARGET_SPEC ""

#define CPP_SPEC "%(cpp_cpu) %(cpp_arch) %(cpp_endian) %(cpp_subtarget)"

/* Prevent error on `-sun4' and `-target sun4' options.  */
/* This used to translate -dalign to -malign, but that is no good
   because it can't turn off the usual meaning of making debugging dumps.  */
/* Translate old style -m<cpu> into new style -mcpu=<cpu>.
   ??? Delete support for -m<cpu> for 2.9.  */

#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
"

/* Override in target specific files.  */
#define ASM_CPU_SPEC "\
%{mcpu=sparclet:-Asparclet} %{mcpu=tsc701:-Asparclet} \
%{msparclite:-Asparclite} \
%{mf930:-Asparclite} %{mf934:-Asparclite} \
%{mcpu=sparclite:-Asparclite} \
%{mcpu=sparclite86x:-Asparclite} \
%{mcpu=f930:-Asparclite} %{mcpu=f934:-Asparclite} \
%{mv8plus:-Av8plus} \
%{mcpu=v9:-Av9} \
%{mcpu=ultrasparc:%{!mv8plus:-Av9a}} \
%{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:%(asm_cpu_default)}}}}}}} \
"

/* Word size selection, among other things.
   This is what GAS uses.  Add %(asm_arch) to ASM_SPEC to enable.  */

#define ASM_ARCH32_SPEC "-32"
#ifdef HAVE_AS_REGISTER_PSEUDO_OP
#define ASM_ARCH64_SPEC "-64 -no-undeclared-regs"
#else
#define ASM_ARCH64_SPEC "-64"
#endif
#define ASM_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? ASM_ARCH32_SPEC : ASM_ARCH64_SPEC)

#define ASM_ARCH_SPEC "\
%{m32:%(asm_arch32)} \
%{m64:%(asm_arch64)} \
%{!m32:%{!m64:%(asm_arch_default)}} \
"

#ifdef HAVE_AS_RELAX_OPTION
#define ASM_RELAX_SPEC "%{!mno-relax:-relax}"
#else
#define ASM_RELAX_SPEC ""
#endif

/* Special flags to the Sun-4 assembler when using pipe for input.  */

#define ASM_SPEC "\
%| %{R} %{!pg:%{!p:%{fpic:-k} %{fPIC:-k}}} %{keep-local-as-symbols:-L} \
%(asm_cpu) %(asm_relax)"

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define EXTRA_SPECS \
  { "cpp_cpu",		CPP_CPU_SPEC },		\
  { "cpp_cpu_default",	CPP_CPU_DEFAULT_SPEC },	\
  { "cpp_arch32",	CPP_ARCH32_SPEC },	\
  { "cpp_arch64",	CPP_ARCH64_SPEC },	\
  { "cpp_arch_default",	CPP_ARCH_DEFAULT_SPEC },\
  { "cpp_arch",		CPP_ARCH_SPEC },	\
  { "cpp_endian",	CPP_ENDIAN_SPEC },	\
  { "cpp_subtarget",	CPP_SUBTARGET_SPEC },	\
  { "asm_cpu",		ASM_CPU_SPEC },		\
  { "asm_cpu_default",	ASM_CPU_DEFAULT_SPEC },	\
  { "asm_arch32",	ASM_ARCH32_SPEC },	\
  { "asm_arch64",	ASM_ARCH64_SPEC },	\
  { "asm_relax",	ASM_RELAX_SPEC },	\
  { "asm_arch_default",	ASM_ARCH_DEFAULT_SPEC },\
  { "asm_arch",		ASM_ARCH_SPEC },	\
  SUBTARGET_EXTRA_SPECS

#define SUBTARGET_EXTRA_SPECS

/* Because libgcc can generate references back to libc (via .umul etc.) we have
   to list libc again after the second libgcc.  */
#define LINK_GCC_C_SEQUENCE_SPEC "%G %L %G %L"


#ifdef SPARC_BI_ARCH
#define NO_BUILTIN_PTRDIFF_TYPE
#define NO_BUILTIN_SIZE_TYPE
#endif
#define PTRDIFF_TYPE (TARGET_ARCH64 ? "long int" : "int")
#define SIZE_TYPE (TARGET_ARCH64 ? "long unsigned int" : "unsigned int")

/* ??? This should be 32 bits for v9 but what can we do?  */
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

#define OVERRIDE_OPTIONS  sparc_override_options ()

/* Generate DBX debugging information.  */

#define DBX_DEBUGGING_INFO

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Nonzero if we should generate code to use the fpu.  */
#define MASK_FPU 1
#define TARGET_FPU (target_flags & MASK_FPU)

/* Nonzero if we should assume that double pointers might be unaligned.
   This can happen when linking gcc compiled code with other compilers,
   because the ABI only guarantees 4 byte alignment.  */
#define MASK_UNALIGNED_DOUBLES 4
#define TARGET_UNALIGNED_DOUBLES (target_flags & MASK_UNALIGNED_DOUBLES)

/* Nonzero means that we should generate code for a v8 sparc.  */
#define MASK_V8 0x8
#define TARGET_V8 (target_flags & MASK_V8)

/* Nonzero means that we should generate code for a sparclite.
   This enables the sparclite specific instructions, but does not affect
   whether FPU instructions are emitted.  */
#define MASK_SPARCLITE 0x10
#define TARGET_SPARCLITE (target_flags & MASK_SPARCLITE)

/* Nonzero if we're compiling for the sparclet.  */
#define MASK_SPARCLET 0x20
#define TARGET_SPARCLET (target_flags & MASK_SPARCLET)

/* Nonzero if we're compiling for v9 sparc.
   Note that v9's can run in 32 bit mode so this doesn't necessarily mean
   the word size is 64.  */
#define MASK_V9 0x40
#define TARGET_V9 (target_flags & MASK_V9)

/* Non-zero to generate code that uses the instructions deprecated in
   the v9 architecture.  This option only applies to v9 systems.  */
/* ??? This isn't user selectable yet.  It's used to enable such insns
   on 32 bit v9 systems and for the moment they're permanently disabled
   on 64 bit v9 systems.  */
#define MASK_DEPRECATED_V8_INSNS 0x80
#define TARGET_DEPRECATED_V8_INSNS (target_flags & MASK_DEPRECATED_V8_INSNS)

/* Mask of all CPU selection flags.  */
#define MASK_ISA \
(MASK_V8 + MASK_SPARCLITE + MASK_SPARCLET + MASK_V9 + MASK_DEPRECATED_V8_INSNS)

/* Non-zero means don't pass `-assert pure-text' to the linker.  */
#define MASK_IMPURE_TEXT 0x100
#define TARGET_IMPURE_TEXT (target_flags & MASK_IMPURE_TEXT)

/* Nonzero means that we should generate code using a flat register window
   model, i.e. no save/restore instructions are generated, which is
   compatible with normal sparc code.
   The frame pointer is %i7 instead of %fp.  */
#define MASK_FLAT 0x200
#define TARGET_FLAT (target_flags & MASK_FLAT)

/* Nonzero means use the registers that the Sparc ABI reserves for
   application software.  This must be the default to coincide with the
   setting in FIXED_REGISTERS.  */
#define MASK_APP_REGS 0x400
#define TARGET_APP_REGS (target_flags & MASK_APP_REGS)

/* Option to select how quad word floating point is implemented.
   When TARGET_HARD_QUAD is true, we use the hardware quad instructions.
   Otherwise, we use the SPARC ABI quad library functions.  */
#define MASK_HARD_QUAD 0x800
#define TARGET_HARD_QUAD (target_flags & MASK_HARD_QUAD)

/* Non-zero on little-endian machines.  */
/* ??? Little endian support currently only exists for sparclet-aout and
   sparc64-elf configurations.  May eventually want to expand the support
   to all targets, but for now it's kept local to only those two.  */
#define MASK_LITTLE_ENDIAN 0x1000
#define TARGET_LITTLE_ENDIAN (target_flags & MASK_LITTLE_ENDIAN)

/* 0x2000, 0x4000 are unused */

/* Nonzero if pointers are 64 bits.  */
#define MASK_PTR64 0x8000
#define TARGET_PTR64 (target_flags & MASK_PTR64)

/* Nonzero if generating code to run in a 64 bit environment.
   This is intended to only be used by TARGET_ARCH{32,64} as they are the
   mechanism used to control compile time or run time selection.  */
#define MASK_64BIT 0x10000
#define TARGET_64BIT (target_flags & MASK_64BIT)

/* 0x20000,0x40000 unused */

/* Non-zero means use a stack bias of 2047.  Stack offsets are obtained by
   adding 2047 to %sp.  This option is for v9 only and is the default.  */
#define MASK_STACK_BIAS 0x80000
#define TARGET_STACK_BIAS (target_flags & MASK_STACK_BIAS)

/* 0x100000,0x200000 unused */

/* Non-zero means -m{,no-}fpu was passed on the command line.  */
#define MASK_FPU_SET 0x400000
#define TARGET_FPU_SET (target_flags & MASK_FPU_SET)

/* Use the UltraSPARC Visual Instruction Set extensions.  */
#define MASK_VIS 0x1000000          
#define TARGET_VIS (target_flags & MASK_VIS)

/* Compile for Solaris V8+.  32 bit Solaris preserves the high bits of
   the current out and global registers and Linux 2.2+ as well.  */
#define MASK_V8PLUS 0x2000000
#define TARGET_V8PLUS (target_flags & MASK_V8PLUS)                            

/* Force a the fastest alignment on structures to take advantage of
   faster copies.  */
#define MASK_FASTER_STRUCTS 0x4000000
#define TARGET_FASTER_STRUCTS (target_flags & MASK_FASTER_STRUCTS)

/* Use IEEE quad long double.  */
#define MASK_LONG_DOUBLE_128 0x8000000
#define TARGET_LONG_DOUBLE_128 (target_flags & MASK_LONG_DOUBLE_128)

/* TARGET_HARD_MUL: Use hardware multiply instructions but not %y.
   TARGET_HARD_MUL32: Use hardware multiply instructions with rd %y
   to get high 32 bits.  False in V8+ or V9 because multiply stores
   a 64 bit result in a register.  */

#define TARGET_HARD_MUL32				\
  ((TARGET_V8 || TARGET_SPARCLITE			\
    || TARGET_SPARCLET || TARGET_DEPRECATED_V8_INSNS)	\
   && ! TARGET_V8PLUS && TARGET_ARCH32)

#define TARGET_HARD_MUL					\
  (TARGET_V8 || TARGET_SPARCLITE || TARGET_SPARCLET	\
   || TARGET_DEPRECATED_V8_INSNS || TARGET_V8PLUS)                        


/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"fpu", MASK_FPU | MASK_FPU_SET,					\
     N_("Use hardware fp") },						\
    {"no-fpu", -MASK_FPU,						\
     N_("Do not use hardware fp") },					\
    {"no-fpu", MASK_FPU_SET,				NULL, },	\
    {"hard-float", MASK_FPU | MASK_FPU_SET,				\
     N_("Use hardware fp") },						\
    {"soft-float", -MASK_FPU,						\
     N_("Do not use hardware fp") },					\
    {"soft-float", MASK_FPU_SET,			NULL },		\
    {"unaligned-doubles", MASK_UNALIGNED_DOUBLES,			\
     N_("Assume possible double misalignment") },			\
    {"no-unaligned-doubles", -MASK_UNALIGNED_DOUBLES,			\
     N_("Assume all doubles are aligned") },				\
    {"impure-text", MASK_IMPURE_TEXT,					\
     N_("Pass -assert pure-text to linker") }, 				\
    {"no-impure-text", -MASK_IMPURE_TEXT,				\
     N_("Do not pass -assert pure-text to linker") }, 			\
    {"flat", MASK_FLAT,							\
     N_("Use flat register window model") }, 				\
    {"no-flat", -MASK_FLAT,						\
     N_("Do not use flat register window model") }, 			\
    {"app-regs", MASK_APP_REGS,						\
     N_("Use ABI reserved registers") },				\
    {"no-app-regs", -MASK_APP_REGS,					\
     N_("Do not use ABI reserved registers") }, 			\
    {"hard-quad-float", MASK_HARD_QUAD,					\
     N_("Use hardware quad fp instructions") }, 			\
    {"soft-quad-float", -MASK_HARD_QUAD,				\
     N_("Do not use hardware quad fp instructions") }, 			\
    {"v8plus", MASK_V8PLUS,						\
     N_("Compile for v8plus ABI") },					\
    {"no-v8plus", -MASK_V8PLUS,						\
     N_("Do not compile for v8plus ABI") }, 				\
    {"vis", MASK_VIS,							\
     N_("Utilize Visual Instruction Set") }, 				\
    {"no-vis", -MASK_VIS,						\
     N_("Do not utilize Visual Instruction Set") }, 			\
    /* ??? These are deprecated, coerced to -mcpu=.  Delete in 2.9.  */ \
    {"cypress", 0,							\
     N_("Optimize for Cypress processors") }, 				\
    {"sparclite", 0,							\
     N_("Optimize for SparcLite processors") }, 			\
    {"f930", 0,								\
     N_("Optimize for F930 processors") }, 				\
    {"f934", 0,								\
     N_("Optimize for F934 processors") }, 				\
    {"v8", 0,								\
     N_("Use V8 Sparc ISA") }, 						\
    {"supersparc", 0,							\
     N_("Optimize for SuperSparc processors") }, 			\
    /* End of deprecated options.  */					\
    {"ptr64", MASK_PTR64,						\
     N_("Pointers are 64-bit") }, 					\
    {"ptr32", -MASK_PTR64,						\
     N_("Pointers are 32-bit") }, 					\
    {"32", -MASK_64BIT,							\
     N_("Use 32-bit ABI") }, 						\
    {"64", MASK_64BIT,							\
     N_("Use 64-bit ABI") }, 						\
    {"stack-bias", MASK_STACK_BIAS,					\
     N_("Use stack bias") }, 						\
    {"no-stack-bias", -MASK_STACK_BIAS,					\
     N_("Do not use stack bias") }, 					\
    {"faster-structs", MASK_FASTER_STRUCTS,				\
     N_("Use structs on stronger alignment for double-word copies") }, 	\
    {"no-faster-structs", -MASK_FASTER_STRUCTS,				\
     N_("Do not use structs on stronger alignment for double-word copies") }, \
    {"relax", 0,							\
     N_("Optimize tail call instructions in assembler and linker") },	\
    {"no-relax", 0,							\
     N_("Do not optimize tail call instructions in assembler or linker") }, \
    SUBTARGET_SWITCHES			\
    { "", TARGET_DEFAULT, ""}}

/* MASK_APP_REGS must always be the default because that's what
   FIXED_REGISTERS is set to and -ffixed- is processed before
   CONDITIONAL_REGISTER_USAGE is called (where we process -mno-app-regs).  */
#define TARGET_DEFAULT (MASK_APP_REGS + MASK_FPU)

/* This is meant to be redefined in target specific files.  */
#define SUBTARGET_SWITCHES

/* Processor type.
   These must match the values for the cpu attribute in sparc.md.  */
enum processor_type {
  PROCESSOR_V7,
  PROCESSOR_CYPRESS,
  PROCESSOR_V8,
  PROCESSOR_SUPERSPARC,
  PROCESSOR_SPARCLITE,
  PROCESSOR_F930,
  PROCESSOR_F934,
  PROCESSOR_HYPERSPARC,
  PROCESSOR_SPARCLITE86X,
  PROCESSOR_SPARCLET,
  PROCESSOR_TSC701,
  PROCESSOR_V9,
  PROCESSOR_ULTRASPARC
};

/* This is set from -m{cpu,tune}=xxx.  */
extern enum processor_type sparc_cpu;

/* Recast the cpu class to be the cpu attribute.
   Every file includes us, but not every file includes insn-attr.h.  */
#define sparc_cpu_attr ((enum attr_cpu) sparc_cpu)

#define TARGET_OPTIONS \
{								\
  { "cpu=",  &sparc_select[1].string,				\
    N_("Use features of and schedule code for given CPU") },	\
  { "tune=", &sparc_select[2].string,				\
    N_("Schedule code for given CPU") },			\
  { "cmodel=", &sparc_cmodel_string,				\
    N_("Use given Sparc code model") },				\
  SUBTARGET_OPTIONS 						\
}

/* This is meant to be redefined in target specific files.  */
#define SUBTARGET_OPTIONS

/* sparc_select[0] is reserved for the default cpu.  */
struct sparc_cpu_select
{
  const char *string;
  const char *const name;
  const int set_tune_p;
  const int set_arch_p;
};

extern struct sparc_cpu_select sparc_select[];

/* target machine storage layout */

/* Define for cross-compilation to a sparc target with no TFmode from a host
   with a different float format (e.g. VAX).  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#if defined (__LITTLE_ENDIAN__) || defined(__LITTLE_ENDIAN_DATA__)
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#endif

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD		(TARGET_ARCH64 ? 64 : 32)
#define MAX_BITS_PER_WORD	64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		(TARGET_ARCH64 ? 8 : 4)
#ifdef IN_LIBGCC2
#define MIN_UNITS_PER_WORD	UNITS_PER_WORD
#else
#define MIN_UNITS_PER_WORD	4
#endif

/* Now define the sizes of the C data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		(TARGET_ARCH64 ? 64 : 32)
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64

#ifdef SPARC_BI_ARCH
#define MAX_LONG_TYPE_SIZE	64
#endif

#if 0
/* ??? This does not work in SunOS 4.x, so it is not enabled here.
   Instead, it is enabled in sol2.h, because it does work under Solaris.  */
/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE 128
#endif

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE (TARGET_PTR64 ? 64 : 32)

/* If we have to extend pointers (only when TARGET_ARCH64 and not
   TARGET_PTR64), we want to do it unsigned.   This macro does nothing
   if ptr_mode and Pmode are the same.  */
#define POINTERS_EXTEND_UNSIGNED 1

/* A macro to update MODE and UNSIGNEDP when an object whose type
   is TYPE and which has the specified mode and signedness is to be
   stored in a register.  This macro is only called when TYPE is a
   scalar type.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
if (TARGET_ARCH64				\
    && GET_MODE_CLASS (MODE) == MODE_INT	\
    && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
  (MODE) = DImode;

/* Define this macro if the promotion described by PROMOTE_MODE
   should also be done for outgoing function arguments.  */
/* This is only needed for TARGET_ARCH64, but since PROMOTE_MODE is a no-op
   for TARGET_ARCH32 this is ok.  Otherwise we'd need to add a runtime test
   for this value.  */
#define PROMOTE_FUNCTION_ARGS

/* Define this macro if the promotion described by PROMOTE_MODE
   should also be done for the return value of functions.
   If this macro is defined, FUNCTION_VALUE must perform the same
   promotions done by PROMOTE_MODE.  */
/* This is only needed for TARGET_ARCH64, but since PROMOTE_MODE is a no-op
   for TARGET_ARCH32 this is ok.  Otherwise we'd need to add a runtime test
   for this value.  */
#define PROMOTE_FUNCTION_RETURN

/* Define this macro if the promotion described by PROMOTE_MODE
   should _only_ be performed for outgoing function arguments or
   function return values, as specified by PROMOTE_FUNCTION_ARGS
   and PROMOTE_FUNCTION_RETURN, respectively.  */
/* This is only needed for TARGET_ARCH64, but since PROMOTE_MODE is a no-op
   for TARGET_ARCH32 this is ok.  Otherwise we'd need to add a runtime test
   for this value.  For TARGET_ARCH64 we need it, as we don't have instructions
   for arithmetic operations which do zero/sign extension at the same time,
   so without this we end up with a srl/sra after every assignment to an
   user variable,  which means very very bad code.  */
#define PROMOTE_FOR_CALL_ONLY

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_ARCH64 ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY (TARGET_ARCH64 ? 128 : 64)

/* ALIGN FRAMES on double word boundaries */

#define SPARC_STACK_ALIGN(LOC) \
  (TARGET_ARCH64 ? (((LOC)+15) & ~15) : (((LOC)+7) & ~7))

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY (TARGET_ARCH64 ? 64 : 32)

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT (TARGET_ARCH64 ? 128 : 64)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 64

/* Define this macro as an expression for the alignment of a structure
   (given by STRUCT as a tree node) if the alignment computed in the
   usual way is COMPUTED and the alignment explicitly specified was
   SPECIFIED.

   The default is to use SPECIFIED if it is larger; otherwise, use
   the smaller of COMPUTED and `BIGGEST_ALIGNMENT' */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)	\
 (TARGET_FASTER_STRUCTS ?				\
  ((TREE_CODE (STRUCT) == RECORD_TYPE			\
    || TREE_CODE (STRUCT) == UNION_TYPE                 \
    || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)           \
   && TYPE_FIELDS (STRUCT) != 0                         \
     ? MAX (MAX ((COMPUTED), (SPECIFIED)), BIGGEST_ALIGNMENT) \
     : MAX ((COMPUTED), (SPECIFIED)))			\
   :  MAX ((COMPUTED), (SPECIFIED)))

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Things that must be doubleword aligned cannot go in the text section,
   because the linker fails to align the text section enough!
   Put them in the data section.  This macro is only used in this file.  */
#define MAX_TEXT_ALIGN 32

/* This forces all variables and constants to the data section when PIC.
   This is because the SunOS 4 shared library scheme thinks everything in
   text is a function, and patches the address to point to a loader stub.  */
/* This is defined to zero for every system which doesn't use the a.out object
   file format.  */
#ifndef SUNOS4_SHARED_LIBRARIES
#define SUNOS4_SHARED_LIBRARIES 0
#endif


/* Use text section for a constant
   unless we need more alignment than that offers.  */
/* This is defined differently for v9 in a cover file.  */
#define SELECT_RTX_SECTION(MODE, X, ALIGN)	\
{						\
  if (GET_MODE_BITSIZE (MODE) <= MAX_TEXT_ALIGN \
      && ! (flag_pic && (symbolic_operand ((X), (MODE)) || SUNOS4_SHARED_LIBRARIES)))  \
    text_section ();				\
  else						\
    data_section ();				\
}

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   SPARC has 32 integer registers and 32 floating point registers.
   64 bit SPARC has 32 additional fp regs, but the odd numbered ones are not
   accessible.  We still account for them to simplify register computations
   (eg: in CLASS_MAX_NREGS).  There are also 4 fp condition code registers, so
   32+32+32+4 == 100.
   Register 100 is used as the integer condition code register.
   Register 101 is used as the soft frame pointer register.  */

#define FIRST_PSEUDO_REGISTER 102

#define SPARC_FIRST_FP_REG     32
/* Additional V9 fp regs.  */
#define SPARC_FIRST_V9_FP_REG  64
#define SPARC_LAST_V9_FP_REG   95
/* V9 %fcc[0123].  V8 uses (figuratively) %fcc0.  */
#define SPARC_FIRST_V9_FCC_REG 96
#define SPARC_LAST_V9_FCC_REG  99
/* V8 fcc reg.  */
#define SPARC_FCC_REG 96
/* Integer CC reg.  We don't distinguish %icc from %xcc.  */
#define SPARC_ICC_REG 100

/* Nonzero if REGNO is an fp reg.  */
#define SPARC_FP_REG_P(REGNO) \
((REGNO) >= SPARC_FIRST_FP_REG && (REGNO) <= SPARC_LAST_V9_FP_REG)

/* Argument passing regs.  */
#define SPARC_OUTGOING_INT_ARG_FIRST 8
#define SPARC_INCOMING_INT_ARG_FIRST (TARGET_FLAT ? 8 : 24)
#define SPARC_FP_ARG_FIRST           32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On non-v9 systems:
   g1 is free to use as temporary.
   g2-g4 are reserved for applications.  Gcc normally uses them as
   temporaries, but this can be disabled via the -mno-app-regs option.
   g5 through g7 are reserved for the operating system.

   On v9 systems:
   g1,g5 are free to use as temporaries, and are free to use between calls
   if the call is to an external function via the PLT.
   g4 is free to use as a temporary in the non-embedded case.
   g4 is reserved in the embedded case.
   g2-g3 are reserved for applications.  Gcc normally uses them as
   temporaries, but this can be disabled via the -mno-app-regs option.
   g6-g7 are reserved for the operating system (or application in
   embedded case).
   ??? Register 1 is used as a temporary by the 64 bit sethi pattern, so must
   currently be a fixed register until this pattern is rewritten.
   Register 1 is also used when restoring call-preserved registers in large
   stack frames.

   Registers fixed in arch32 and not arch64 (or vice-versa) are marked in
   CONDITIONAL_REGISTER_USAGE in order to properly handle -ffixed-.
*/

#define FIXED_REGISTERS  \
 {1, 0, 2, 2, 2, 2, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
				\
  0, 0, 0, 0, 0, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1}

/* If !TARGET_FPU, then make the fp registers and fp cc regs fixed so that
   they won't be allocated.  */

#define CONDITIONAL_REGISTER_USAGE				\
do								\
  {								\
    if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)		\
      {								\
	fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
	call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
      }								\
    /* If the user has passed -f{fixed,call-{used,saved}}-g5 */	\
    /* then honour it.  */					\
    if (TARGET_ARCH32 && fixed_regs[5])				\
      fixed_regs[5] = 1;					\
    else if (TARGET_ARCH64 && fixed_regs[5] == 2)		\
      fixed_regs[5] = 0;					\
    if (! TARGET_V9)						\
      {								\
	int regno;						\
	for (regno = SPARC_FIRST_V9_FP_REG;			\
	     regno <= SPARC_LAST_V9_FP_REG;			\
	     regno++)						\
	  fixed_regs[regno] = 1;				\
	/* %fcc0 is used by v8 and v9.  */			\
	for (regno = SPARC_FIRST_V9_FCC_REG + 1;		\
	     regno <= SPARC_LAST_V9_FCC_REG;			\
	     regno++)						\
	  fixed_regs[regno] = 1;				\
      }								\
    if (! TARGET_FPU)						\
      {								\
	int regno;						\
	for (regno = 32; regno < SPARC_LAST_V9_FCC_REG; regno++) \
	  fixed_regs[regno] = 1;				\
      }								\
    /* If the user has passed -f{fixed,call-{used,saved}}-g2 */	\
    /* then honour it.  Likewise with g3 and g4.  */		\
    if (fixed_regs[2] == 2)					\
      fixed_regs[2] = ! TARGET_APP_REGS;			\
    if (fixed_regs[3] == 2)					\
      fixed_regs[3] = ! TARGET_APP_REGS;			\
    if (TARGET_ARCH32 && fixed_regs[4] == 2)			\
      fixed_regs[4] = ! TARGET_APP_REGS;			\
    else if (TARGET_CM_EMBMEDANY)				\
      fixed_regs[4] = 1;					\
    else if (fixed_regs[4] == 2)				\
      fixed_regs[4] = 0;					\
    if (TARGET_FLAT)						\
      {								\
	int regno;						\
	/* Let the compiler believe the frame pointer is still	\
	   %fp, but output it as %i7.  */			\
	fixed_regs[31] = 1;					\
	reg_names[HARD_FRAME_POINTER_REGNUM] = "%i7";		\
	/* Disable leaf functions */				\
	memset (sparc_leaf_regs, 0, FIRST_PSEUDO_REGISTER);	\
	/* Make LEAF_REG_REMAP a noop.  */			\
	for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)	\
	  leaf_reg_remap [regno] = regno;			\
      }								\
  }								\
while (0)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On SPARC, ordinary registers hold 32 bits worth;
   this means both integer and floating point registers.
   On v9, integer regs hold 64 bits worth; floating point regs hold
   32 bits worth (this includes the new fp regs as even the odd ones are
   included in the hard register count).  */

#define HARD_REGNO_NREGS(REGNO, MODE) \
  (TARGET_ARCH64							\
   ? ((REGNO) < 32 || (REGNO) == FRAME_POINTER_REGNUM			\
      ? (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD	\
      : (GET_MODE_SIZE (MODE) + 3) / 4)					\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Due to the ARCH64 descrepancy above we must override this next
   macro too.  */
#define REGMODE_NATURAL_SIZE(MODE) \
  ((TARGET_ARCH64 && FLOAT_MODE_P (MODE)) ? 4 : UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   See sparc.c for how we initialize this.  */
extern const int *hard_regno_mode_classes;
extern int sparc_mode_class[];

/* ??? Because of the funny way we pass parameters we should allow certain
   ??? types of float/complex values to be in integer registers during
   ??? RTL generation.  This only matters on arch32.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((hard_regno_mode_classes[REGNO] & sparc_mode_class[MODE]) != 0)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.

   For V9: SFmode can't be combined with other float modes, because they can't
   be allocated to the %d registers.  Also, DFmode won't fit in odd %f
   registers, but SFmode will.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) == (MODE2)						\
   || (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2)		\
       && (! TARGET_V9						\
	   || (GET_MODE_CLASS (MODE1) != MODE_FLOAT		\
	       || (MODE1 != SFmode && MODE2 != SFmode)))))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* SPARC pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* The stack bias (amount by which the hardware register is offset by).  */
#define SPARC_STACK_BIAS ((TARGET_ARCH64 && TARGET_STACK_BIAS) ? 2047 : 0)

/* Actual top-of-stack address is 92/176 greater than the contents of the
   stack pointer register for !v9/v9.  That is:
   - !v9: 64 bytes for the in and local registers, 4 bytes for structure return
     address, and 6*4 bytes for the 6 register parameters.
   - v9: 128 bytes for the in and local registers + 6*8 bytes for the integer
     parameter regs.  */
#define STACK_POINTER_OFFSET (FIRST_PARM_OFFSET(0) + SPARC_STACK_BIAS)

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM 30

/* The soft frame pointer does not have the stack bias applied.  */
#define FRAME_POINTER_REGNUM 101

/* Given the stack bias, the stack pointer isn't actually aligned.  */
#define INIT_EXPANDERS							 \
  do {									 \
    if (cfun && cfun->emit->regno_pointer_align && SPARC_STACK_BIAS)	 \
      {									 \
	REGNO_POINTER_ALIGN (STACK_POINTER_REGNUM) = BITS_PER_UNIT;	 \
	REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = BITS_PER_UNIT; \
      }									 \
  } while (0)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.
   Used in flow.c, global.c, and reload1.c.

   Being a non-leaf function does not mean a frame pointer is needed in the
   flat window model.  However, the debugger won't be able to backtrace through
   us with out it.  */
#define FRAME_POINTER_REQUIRED				\
  (TARGET_FLAT						\
   ? (current_function_calls_alloca			\
      || current_function_varargs			\
      || !leaf_function_p ())				\
   : ! (leaf_function_p () && only_leaf_regs_used ()))

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM (TARGET_ARCH64 ? 5 : 2)

/* Register which holds offset table for position-independent
   data references.  */

#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? 23 : INVALID_REGNUM)

/* Pick a default value we can notice from override_options:
   !v9: Default is on.
   v9: Default is off.  */

#define DEFAULT_PCC_STRUCT_RETURN -1

/* Sparc ABI says that quad-precision floats and all structures are returned
   in memory.
   For v9: unions <= 32 bytes in size are returned in int regs,
   structures up to 32 bytes are returned in int and fp regs.  */

#define RETURN_IN_MEMORY(TYPE)				\
(TARGET_ARCH32						\
 ? (TYPE_MODE (TYPE) == BLKmode				\
    || TYPE_MODE (TYPE) == TFmode			\
    || TYPE_MODE (TYPE) == TCmode)			\
 : (TYPE_MODE (TYPE) == BLKmode				\
    && (unsigned HOST_WIDE_INT) int_size_in_bytes (TYPE) > 32))

/* Functions which return large structures get the address
   to place the wanted value at offset 64 from the frame.
   Must reserve 64 bytes for the in and local registers.
   v9: Functions which return large structures get the address to place the
   wanted value from an invisible first argument.  */
/* Used only in other #defines in this file.  */
#define STRUCT_VALUE_OFFSET 64

#define STRUCT_VALUE \
  (TARGET_ARCH64					\
   ? 0							\
   : gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, \
					STRUCT_VALUE_OFFSET)))

#define STRUCT_VALUE_INCOMING \
  (TARGET_ARCH64						\
   ? 0								\
   : gen_rtx_MEM (Pmode, plus_constant (frame_pointer_rtx,	\
					STRUCT_VALUE_OFFSET)))

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

/* The SPARC has various kinds of registers: general, floating point,
   and condition codes [well, it has others as well, but none that we
   care directly about].

   For v9 we must distinguish between the upper and lower floating point
   registers because the upper ones can't hold SFmode values.
   HARD_REGNO_MODE_OK won't help here because reload assumes that register(s)
   satisfying a group need for a class will also satisfy a single need for
   that class.  EXTRA_FP_REGS is a bit of a misnomer as it covers all 64 fp
   regs.

   It is important that one class contains all the general and all the standard
   fp regs.  Otherwise find_reg() won't properly allocate int regs for moves,
   because reg_class_record() will bias the selection in favor of fp regs,
   because reg_class_subunion[GENERAL_REGS][FP_REGS] will yield FP_REGS,
   because FP_REGS > GENERAL_REGS.

   It is also important that one class contain all the general and all the
   fp regs.  Otherwise when spilling a DFmode reg, it may be from EXTRA_FP_REGS
   but find_reloads() may use class GENERAL_OR_FP_REGS. This will cause
   allocate_reload_reg() to bypass it causing an abort because the compiler
   thinks it doesn't have a spill reg when in fact it does.

   v9 also has 4 floating point condition code registers.  Since we don't
   have a class that is the union of FPCC_REGS with either of the others,
   it is important that it appear first.  Otherwise the compiler will die
   trying to compile _fixunsdfsi because fix_truncdfsi2 won't match its
   constraints.

   It is important that SPARC_ICC_REG have class NO_REGS.  Otherwise combine
   may try to use it to hold an SImode value.  See register_operand.
   ??? Should %fcc[0123] be handled similarly?
*/

enum reg_class { NO_REGS, FPCC_REGS, I64_REGS, GENERAL_REGS, FP_REGS,
		 EXTRA_FP_REGS, GENERAL_OR_FP_REGS, GENERAL_OR_EXTRA_FP_REGS,
		 ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES \
  { "NO_REGS", "FPCC_REGS", "I64_REGS", "GENERAL_REGS", "FP_REGS",	\
     "EXTRA_FP_REGS", "GENERAL_OR_FP_REGS", "GENERAL_OR_EXTRA_FP_REGS",	\
     "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS				\
  {{0, 0, 0, 0},	/* NO_REGS */			\
   {0, 0, 0, 0xf},	/* FPCC_REGS */			\
   {0xffff, 0, 0, 0},	/* I64_REGS */			\
   {-1, 0, 0, 0x20},	/* GENERAL_REGS */		\
   {0, -1, 0, 0},	/* FP_REGS */			\
   {0, -1, -1, 0},	/* EXTRA_FP_REGS */		\
   {-1, -1, 0, 0x20},	/* GENERAL_OR_FP_REGS */	\
   {-1, -1, -1, 0x20},	/* GENERAL_OR_EXTRA_FP_REGS */	\
   {-1, -1, -1, 0x3f}}	/* ALL_REGS */

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class sparc_regno_reg_class[FIRST_PSEUDO_REGISTER];

#define REGNO_REG_CLASS(REGNO) sparc_regno_reg_class[(REGNO)]

/* This is the order in which to allocate registers normally.  
   
   We put %f0/%f1 last among the float registers, so as to make it more
   likely that a pseudo-register which dies in the float return register
   will get allocated to the float return register, thus saving a move
   instruction at the end of the function.  */

#define REG_ALLOC_ORDER \
{ 8, 9, 10, 11, 12, 13, 2, 3,		\
  15, 16, 17, 18, 19, 20, 21, 22,	\
  23, 24, 25, 26, 27, 28, 29, 31,	\
  34, 35, 36, 37, 38, 39,		/* %f2-%f7 */   \
  40, 41, 42, 43, 44, 45, 46, 47,	/* %f8-%f15 */  \
  48, 49, 50, 51, 52, 53, 54, 55,	/* %f16-%f23 */ \
  56, 57, 58, 59, 60, 61, 62, 63,	/* %f24-%f31 */ \
  64, 65, 66, 67, 68, 69, 70, 71,	/* %f32-%f39 */ \
  72, 73, 74, 75, 76, 77, 78, 79,	/* %f40-%f47 */ \
  80, 81, 82, 83, 84, 85, 86, 87,	/* %f48-%f55 */ \
  88, 89, 90, 91, 92, 93, 94, 95,	/* %f56-%f63 */ \
  32, 33,				/* %f0,%f1 */   \
  96, 97, 98, 99, 100,			/* %fcc0-3, %icc */ \
  1, 4, 5, 6, 7, 0, 14, 30, 101}

/* This is the order in which to allocate registers for
   leaf functions.  If all registers can fit in the "gi" registers,
   then we have the possibility of having a leaf function.  */

#define REG_LEAF_ALLOC_ORDER \
{ 2, 3, 24, 25, 26, 27, 28, 29,		\
  4, 5, 6, 7, 1,			\
  15, 8, 9, 10, 11, 12, 13,		\
  16, 17, 18, 19, 20, 21, 22, 23,	\
  34, 35, 36, 37, 38, 39,		\
  40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55,	\
  56, 57, 58, 59, 60, 61, 62, 63,	\
  64, 65, 66, 67, 68, 69, 70, 71,	\
  72, 73, 74, 75, 76, 77, 78, 79,	\
  80, 81, 82, 83, 84, 85, 86, 87,	\
  88, 89, 90, 91, 92, 93, 94, 95,	\
  32, 33,				\
  96, 97, 98, 99, 100,			\
  0, 14, 30, 31, 101}
  
#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()

extern char sparc_leaf_regs[];
#define LEAF_REGISTERS sparc_leaf_regs

extern char leaf_reg_remap[];
#define LEAF_REG_REMAP(REGNO) (leaf_reg_remap[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Local macro to handle the two v9 classes of FP regs.  */
#define FP_REG_CLASS_P(CLASS) ((CLASS) == FP_REGS || (CLASS) == EXTRA_FP_REGS)

/* Get reg_class from a letter such as appears in the machine description.
   In the not-v9 case, coerce v9's 'e' class to 'f', so we can use 'e' in the
   .md file for v8 and v9.
   'd' and 'b' are used for single and double precision VIS operations,
   if TARGET_VIS.
   'h' is used for V8+ 64 bit global and out registers.  */

#define REG_CLASS_FROM_LETTER(C)		\
(TARGET_V9					\
 ? ((C) == 'f' ? FP_REGS			\
    : (C) == 'e' ? EXTRA_FP_REGS 		\
    : (C) == 'c' ? FPCC_REGS			\
    : ((C) == 'd' && TARGET_VIS) ? FP_REGS\
    : ((C) == 'b' && TARGET_VIS) ? EXTRA_FP_REGS\
    : ((C) == 'h' && TARGET_V8PLUS) ? I64_REGS\
    : NO_REGS)					\
 : ((C) == 'f' ? FP_REGS			\
    : (C) == 'e' ? FP_REGS			\
    : (C) == 'c' ? FPCC_REGS			\
    : NO_REGS))

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is used for the range of constants an insn can actually contain.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for constants which can be loaded with a single sethi insn.
   `L' is used for the range of constants supported by the movcc insns.
   `M' is used for the range of constants supported by the movrcc insns.
   `N' is like K, but for constants wider than 32 bits.  */

#define SPARC_SIMM10_P(X) ((unsigned HOST_WIDE_INT) (X) + 0x200 < 0x400)
#define SPARC_SIMM11_P(X) ((unsigned HOST_WIDE_INT) (X) + 0x400 < 0x800)
#define SPARC_SIMM13_P(X) ((unsigned HOST_WIDE_INT) (X) + 0x1000 < 0x2000)
/* 10 and 11 bit immediates are only used for a few specific insns.
   SMALL_INT is used throughout the port so we continue to use it.  */
#define SMALL_INT(X) (SPARC_SIMM13_P (INTVAL (X)))
/* 13 bit immediate, considering only the low 32 bits */
#define SMALL_INT32(X) (SPARC_SIMM13_P (trunc_int_for_mode \
					(INTVAL (X), SImode)))
#define SPARC_SETHI_P(X) \
  (((unsigned HOST_WIDE_INT) (X) \
    & ((unsigned HOST_WIDE_INT) 0x3ff - GET_MODE_MASK (SImode) - 1)) == 0)
#define SPARC_SETHI32_P(X) \
  (SPARC_SETHI_P ((unsigned HOST_WIDE_INT) (X) & GET_MODE_MASK (SImode)))

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? SPARC_SIMM13_P (VALUE)			\
   : (C) == 'J' ? (VALUE) == 0				\
   : (C) == 'K' ? SPARC_SETHI32_P (VALUE)		\
   : (C) == 'L' ? SPARC_SIMM11_P (VALUE)		\
   : (C) == 'M' ? SPARC_SIMM10_P (VALUE)		\
   : (C) == 'N' ? SPARC_SETHI_P (VALUE)			\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)	\
  ((C) == 'G' ? fp_zero_operand (VALUE, GET_MODE (VALUE))	\
   : (C) == 'H' ? arith_double_operand (VALUE, DImode)		\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
/* - We can't load constants into FP registers.
   - We can't load FP constants into integer registers when soft-float,
     because there is no soft-float pattern with a r/F constraint.
   - We can't load FP constants into integer registers for TFmode unless
     it is 0.0L, because there is no movtf pattern with a r/F constraint.
   - Try and reload integer constants (symbolic or otherwise) back into
     registers directly, rather than having them dumped to memory.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  (CONSTANT_P (X)					\
   ? ((FP_REG_CLASS_P (CLASS)				\
       || (CLASS) == GENERAL_OR_FP_REGS			\
       || (CLASS) == GENERAL_OR_EXTRA_FP_REGS		\
       || (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT	\
	   && ! TARGET_FPU)				\
       || (GET_MODE (X) == TFmode			\
	   && ! fp_zero_operand (X, TFmode)))		\
      ? NO_REGS						\
      : (!FP_REG_CLASS_P (CLASS)			\
         && GET_MODE_CLASS (GET_MODE (X)) == MODE_INT)	\
      ? GENERAL_REGS					\
      : (CLASS))					\
   : (CLASS))

/* Return the register class of a scratch register needed to load IN into
   a register of class CLASS in MODE.

   We need a temporary when loading/storing a HImode/QImode value
   between memory and the FPU registers.  This can happen when combine puts
   a paradoxical subreg in a float/fix conversion insn.

   We need a temporary when loading/storing a DFmode value between
   unaligned memory and the upper FPU registers.  */

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, IN)		\
  ((FP_REG_CLASS_P (CLASS)					\
    && ((MODE) == HImode || (MODE) == QImode)			\
    && (GET_CODE (IN) == MEM					\
        || ((GET_CODE (IN) == REG || GET_CODE (IN) == SUBREG)	\
            && true_regnum (IN) == -1)))			\
   ? GENERAL_REGS						\
   : ((CLASS) == EXTRA_FP_REGS && (MODE) == DFmode		\
      && GET_CODE (IN) == MEM && TARGET_ARCH32			\
      && ! mem_min_alignment ((IN), 8))				\
     ? FP_REGS							\
     : (((TARGET_CM_MEDANY					\
	  && symbolic_operand ((IN), (MODE)))			\
	 || (TARGET_CM_EMBMEDANY				\
	     && text_segment_operand ((IN), (MODE))))		\
	&& !flag_pic)						\
       ? GENERAL_REGS						\
       : NO_REGS)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, IN)		\
  ((FP_REG_CLASS_P (CLASS)					\
     && ((MODE) == HImode || (MODE) == QImode)			\
     && (GET_CODE (IN) == MEM					\
         || ((GET_CODE (IN) == REG || GET_CODE (IN) == SUBREG)	\
             && true_regnum (IN) == -1)))			\
   ? GENERAL_REGS						\
   : ((CLASS) == EXTRA_FP_REGS && (MODE) == DFmode		\
      && GET_CODE (IN) == MEM && TARGET_ARCH32			\
      && ! mem_min_alignment ((IN), 8))				\
     ? FP_REGS							\
     : (((TARGET_CM_MEDANY					\
	  && symbolic_operand ((IN), (MODE)))			\
	 || (TARGET_CM_EMBMEDANY				\
	     && text_segment_operand ((IN), (MODE))))		\
	&& !flag_pic)						\
       ? GENERAL_REGS						\
       : NO_REGS)

/* On SPARC it is not possible to directly move data between 
   GENERAL_REGS and FP_REGS.  */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) \
  (FP_REG_CLASS_P (CLASS1) != FP_REG_CLASS_P (CLASS2))

/* Return the stack location to use for secondary memory needed reloads.
   We want to use the reserved location just below the frame pointer.
   However, we must ensure that there is a frame, so use assign_stack_local
   if the frame size is zero.  */
#define SECONDARY_MEMORY_NEEDED_RTX(MODE) \
  (get_frame_size () == 0						\
   ? assign_stack_local (MODE, GET_MODE_SIZE (MODE), 0)			\
   : gen_rtx_MEM (MODE, plus_constant (frame_pointer_rtx,		\
				       STARTING_FRAME_OFFSET)))

/* Get_secondary_mem widens its argument to BITS_PER_WORD which loses on v9
   because the movsi and movsf patterns don't handle r/f moves.
   For v8 we copy the default definition.  */
#define SECONDARY_MEMORY_NEEDED_MODE(MODE) \
  (TARGET_ARCH64						\
   ? (GET_MODE_BITSIZE (MODE) < 32				\
      ? mode_for_size (32, GET_MODE_CLASS (MODE), 0)		\
      : MODE)							\
   : (GET_MODE_BITSIZE (MODE) < BITS_PER_WORD			\
      ? mode_for_size (BITS_PER_WORD, GET_MODE_CLASS (MODE), 0)	\
      : MODE))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On SPARC, this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  (FP_REG_CLASS_P (CLASS) ? (GET_MODE_SIZE (MODE) + 3) / 4 \
   : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define the number of register that can hold parameters.
   This macro is only used in other macro definitions below and in sparc.c.
   MODE is the mode of the argument.
   !v9: All args are passed in %o0-%o5.
   v9: %o0-%o5 and %f0-%f31 are cumulatively used to pass values.
   See the description in sparc.c.  */
#define NPARM_REGS(MODE) \
(TARGET_ARCH64 \
 ? (GET_MODE_CLASS (MODE) == MODE_FLOAT ? 32 : 6) \
 : 6)

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
/* This allows space for one TFmode floating point value.  */
#define STARTING_FRAME_OFFSET \
  (TARGET_ARCH64 ? -16 \
   : (-SPARC_STACK_ALIGN (LONG_DOUBLE_TYPE_SIZE / BITS_PER_UNIT)))

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On SPARC, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   !v9: This is 64 for the ins and locals, plus 4 for the struct-return reg
   even if this function isn't going to use it.
   v9: This is 128 for the ins and locals.  */
#define FIRST_PARM_OFFSET(FNDECL) \
  (TARGET_ARCH64 ? 16 * UNITS_PER_WORD : STRUCT_VALUE_OFFSET + UNITS_PER_WORD)

/* Offset from the argument pointer register value to the CFA.
   This is different from FIRST_PARM_OFFSET because the register window
   comes between the CFA and the arguments.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL)  0

/* When a parameter is passed in a register, stack space is still
   allocated for it.
   !v9: All 6 possible integer registers have backing store allocated.
   v9: Only space for the arguments passed is allocated.  */
/* ??? Ideally, we'd use zero here (as the minimum), but zero has special
   meaning to the backend.  Further, we need to be able to detect if a
   varargs/unprototyped function is called, as they may want to spill more
   registers than we've provided space.  Ugly, ugly.  So for now we retain
   all 6 slots even for v9.  */
#define REG_PARM_STACK_SPACE(DECL) (6 * UNITS_PER_WORD)

/* Definitions for register elimination.  */
/* ??? In TARGET_FLAT mode we needn't have a hard frame pointer.  */
   
#define ELIMINABLE_REGS \
  {{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
   { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM} }

/* The way this is structured, we can't eliminate SFP in favor of SP
   if the frame pointer is required: we want to use the SFP->HFP elimination
   in that case.  But the test in update_eliminables doesn't know we are
   assuming below that we only do the former elimination.  */
#define CAN_ELIMINATE(FROM, TO) \
  ((TO) == HARD_FRAME_POINTER_REGNUM || !FRAME_POINTER_REQUIRED)

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  do {								\
    (OFFSET) = 0;						\
    if ((TO) == STACK_POINTER_REGNUM)				\
      {								\
	/* Note, we always pretend that this is a leaf function	\
	   because if it's not, there's no point in trying to	\
	   eliminate the frame pointer.  If it is a leaf	\
	   function, we guessed right!  */			\
	if (TARGET_FLAT)					\
	  (OFFSET) =						\
	    sparc_flat_compute_frame_size (get_frame_size ());	\
	else							\
	  (OFFSET) = compute_frame_size (get_frame_size (), 1);	\
      }								\
    (OFFSET) += SPARC_STACK_BIAS;				\
  } while (0)

/* Keep the stack pointer constant throughout the function.
   This is both an optimization and a necessity: longjmp
   doesn't behave itself when the stack pointer moves within
   the function!  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Some subroutine macros specific to this machine.
   When !TARGET_FPU, put float return values in the general registers,
   since we don't have any fp registers.  */
#define BASE_RETURN_VALUE_REG(MODE)					\
  (TARGET_ARCH64							\
   ? (TARGET_FPU && FLOAT_MODE_P (MODE) ? 32 : 8)			\
   : (((MODE) == SFmode || (MODE) == DFmode) && TARGET_FPU ? 32 : 8))

#define BASE_OUTGOING_VALUE_REG(MODE)				\
  (TARGET_ARCH64						\
   ? (TARGET_FPU && FLOAT_MODE_P (MODE) ? 32			\
      : TARGET_FLAT ? 8 : 24)					\
   : (((MODE) == SFmode || (MODE) == DFmode) && TARGET_FPU ? 32	\
      : (TARGET_FLAT ? 8 : 24)))

#define BASE_PASSING_ARG_REG(MODE)				\
  (TARGET_ARCH64						\
   ? (TARGET_FPU && FLOAT_MODE_P (MODE) ? 32 : 8)		\
   : 8)

/* ??? FIXME -- seems wrong for v9 structure passing...  */
#define BASE_INCOMING_ARG_REG(MODE)				\
  (TARGET_ARCH64						\
   ? (TARGET_FPU && FLOAT_MODE_P (MODE) ? 32			\
      : TARGET_FLAT ? 8 : 24)					\
   : (TARGET_FLAT ? 8 : 24))

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT) \
 ((TARGET_FLAT || (OUT) < 8 || (OUT) > 15) ? (OUT) : (OUT) + 16)

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN) \
 ((TARGET_FLAT || (IN) < 24 || (IN) > 31) ? (IN) : (IN) - 16)

/* Define this macro if the target machine has register windows.  This
   C expression returns true if the register is call-saved but is in the
   register window.  */

#define LOCAL_REGNO(REGNO) \
  (TARGET_FLAT ? 0 : (REGNO) >= 16 && (REGNO) <= 31)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On SPARC the value is found in the first "output" register.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  function_value ((VALTYPE), TYPE_MODE (VALTYPE), 1)

/* But the called function leaves it in the first "input" register.  */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC) \
  function_value ((VALTYPE), TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  function_value (NULL_TREE, (MODE), 1)

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On SPARC, the first "output" reg is used for integer values,
   and the first floating point register is used for floating point values.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 8 || (N) == 32)

/* Define the size of space to allocate for the return value of an
   untyped_call.  */

#define APPLY_RESULT_SIZE 16

/* 1 if N is a possible register number for function argument passing.
   On SPARC, these are the "output" registers.  v9 also uses %f0-%f31.  */

#define FUNCTION_ARG_REGNO_P(N) \
(TARGET_ARCH64 \
 ? (((N) >= 8 && (N) <= 13) || ((N) >= 32 && (N) <= 63)) \
 : ((N) >= 8 && (N) <= 13))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SPARC (!v9), this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 7 or more means all following args should go on the stack.

   For v9, we also need to know whether a prototype is present.  */

struct sparc_args {
  int words;       /* number of words passed so far */
  int prototype_p; /* non-zero if a prototype is present */
  int libcall_p;   /* non-zero if a library call */
};
#define CUMULATIVE_ARGS struct sparc_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
init_cumulative_args (& (CUM), (FNTYPE), (LIBNAME), (INDIRECT));

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   TYPE is null for libcalls where that information may not be available.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
function_arg_advance (& (CUM), (MODE), (TYPE), (NAMED))

/* Nonzero if we do not know how to pass TYPE solely in registers.  */

#define MUST_PASS_IN_STACK(MODE,TYPE)			\
  ((TYPE) != 0						\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
       || TREE_ADDRESSABLE (TYPE)))

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

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
function_arg (& (CUM), (MODE), (TYPE), (NAMED), 0)

/* Define where a function finds its arguments.
   This is different from FUNCTION_ARG because of register windows.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
function_arg (& (CUM), (MODE), (TYPE), (NAMED), 1)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
function_arg_partial_nregs (& (CUM), (MODE), (TYPE), (NAMED))

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.
   The pointer is passed in whatever way is appropriate for passing a pointer
   to that type.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
function_arg_pass_by_reference (& (CUM), (MODE), (TYPE), (NAMED))

/* If defined, a C expression which determines whether, and in which direction,
   to pad out an argument with extra space.  The value should be of type
   `enum direction': either `upward' to pad above the argument,
   `downward' to pad below, or `none' to inhibit padding.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE) \
function_arg_padding ((MODE), (TYPE))

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined,
   PARM_BOUNDARY is used for all arguments.
   For sparc64, objects requiring 16 byte alignment are passed that way.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
((TARGET_ARCH64					\
  && (GET_MODE_ALIGNMENT (MODE) == 128		\
      || ((TYPE) && TYPE_ALIGN (TYPE) == 128)))	\
 ? 128 : PARM_BOUNDARY)

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *sparc_compare_op0, *sparc_compare_op1;


/* Generate the special assembly code needed to tell the assembler whatever
   it might need to know about the return value of a function.

   For Sparc assemblers, we need to output a .proc pseudo-op which conveys
   information to the assembler relating to peephole optimization (done in
   the assembler).  */

#define ASM_DECLARE_RESULT(FILE, RESULT) \
  fprintf ((FILE), "\t.proc\t0%lo\n", sparc_type_code (TREE_TYPE (RESULT)))

/* Output the special assembly code needed to tell the assembler some
   register is used as global register variable.  

   SPARC 64bit psABI declares registers %g2 and %g3 as application
   registers and %g6 and %g7 as OS registers.  Any object using them
   should declare (for %g2/%g3 has to, for %g6/%g7 can) that it uses them
   and how they are used (scratch or some global variable).
   Linker will then refuse to link together objects which use those
   registers incompatibly.

   Unless the registers are used for scratch, two different global
   registers cannot be declared to the same name, so in the unlikely
   case of a global register variable occupying more than one register
   we prefix the second and following registers with .gnu.part1. etc.  */

extern char sparc_hard_reg_printed[8];

#ifdef HAVE_AS_REGISTER_PSEUDO_OP
#define ASM_DECLARE_REGISTER_GLOBAL(FILE, DECL, REGNO, NAME)		\
do {									\
  if (TARGET_ARCH64)							\
    {									\
      int end = HARD_REGNO_NREGS ((REGNO), DECL_MODE (decl)) + (REGNO); \
      int reg;								\
      for (reg = (REGNO); reg < 8 && reg < end; reg++)			\
	if ((reg & ~1) == 2 || (reg & ~1) == 6)				\
	  {								\
	    if (reg == (REGNO))						\
	      fprintf ((FILE), "\t.register\t%%g%d, %s\n", reg, (NAME)); \
	    else							\
	      fprintf ((FILE), "\t.register\t%%g%d, .gnu.part%d.%s\n",	\
		       reg, reg - (REGNO), (NAME));			\
	    sparc_hard_reg_printed[reg] = 1;				\
	  }								\
    }									\
} while (0)
#endif


/* Emit rtl for profiling.  */
#define PROFILE_HOOK(LABEL)   sparc_profile_hook (LABEL)

/* All the work done in PROFILE_HOOK, but still required.  */
#define FUNCTION_PROFILER(FILE, LABELNO) do { } while (0)

/* Set the name of the mcount function for the system.  */
#define MCOUNT_FUNCTION "*mcount"

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK	\
 (get_frame_size () != 0	\
  || current_function_calls_alloca || current_function_outgoing_args_size)

#define DELAY_SLOTS_FOR_EPILOGUE \
  (TARGET_FLAT ? sparc_flat_epilogue_delay_slots () : 1)
#define ELIGIBLE_FOR_EPILOGUE_DELAY(trial, slots_filled) \
  (TARGET_FLAT ? sparc_flat_eligible_for_epilogue_delay (trial, slots_filled) \
   : eligible_for_epilogue_delay (trial, slots_filled))

/* Define registers used by the epilogue and return instruction.  */
#define EPILOGUE_USES(REGNO) \
  (!TARGET_FLAT && REGNO == 31)

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE (TARGET_ARCH64 ? 32 : 16)

#define TRAMPOLINE_ALIGNMENT 128 /* 16 bytes */

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
    if (TARGET_ARCH64)						\
      sparc64_initialize_trampoline (TRAMP, FNADDR, CXT);	\
    else							\
      sparc_initialize_trampoline (TRAMP, FNADDR, CXT)

/* Generate necessary RTL for __builtin_saveregs().  */

#define EXPAND_BUILTIN_SAVEREGS() sparc_builtin_saveregs ()

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  sparc_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  sparc_va_arg (valist, type)

/* Define this macro if the location where a function argument is passed
   depends on whether or not it is a named argument.

   This macro controls how the NAMED argument to FUNCTION_ARG
   is set for varargs and stdarg functions.  With this macro defined,
   the NAMED argument is always true for named arguments, and false for
   unnamed arguments.  If this is not defined, but SETUP_INCOMING_VARARGS
   is defined, then all arguments are treated as named.  Otherwise, all named
   arguments except the last are treated as named.
   For the v9 we want NAMED to mean what it says it means.  */

#define STRICT_ARGUMENT_NAMING TARGET_V9

/* We do not allow sibling calls if -mflat, nor
   we do not allow indirect calls to be optimized into sibling calls.

   Also, on sparc 32-bit we cannot emit a sibling call when the
   current function returns a structure.  This is because the "unimp
   after call" convention would cause the callee to return to the
   wrong place.  The generic code already disallows cases where the
   function being called returns a structure.

   It may seem strange how this last case could occur.  Usually there
   is code after the call which jumps to epilogue code which dumps the
   return value into the struct return area.  That ought to invalidate
   the sibling call right?  Well, in the c++ case we can end up passing
   the pointer to the struct return area to a constructor (which returns
   void) and then nothing else happens.  Such a sibling call would look
   valid without the added check here.  */
#define FUNCTION_OK_FOR_SIBCALL(DECL) \
	(DECL \
	 && ! TARGET_FLAT \
	 && (TARGET_ARCH64 || ! current_function_returns_struct))

/* Generate RTL to flush the register windows so as to make arbitrary frames
   available.  */
#define SETUP_FRAME_ADDRESSES()		\
  emit_insn (gen_flush_register_windows ())

/* Given an rtx for the address of a frame,
   return an rtx for the address of the word in the frame
   that holds the dynamic chain--the previous frame's address.
   ??? -mflat support? */
#define DYNAMIC_CHAIN_ADDRESS(frame)	\
  plus_constant (frame, 14 * UNITS_PER_WORD + SPARC_STACK_BIAS)

/* The return address isn't on the stack, it is in a register, so we can't
   access it from the current frame pointer.  We can access it from the
   previous frame pointer though by reading a value from the register window
   save area.  */
#define RETURN_ADDR_IN_PREVIOUS_FRAME

/* This is the offset of the return address to the true next instruction to be
   executed for the current function.  */
#define RETURN_ADDR_OFFSET \
  (8 + 4 * (! TARGET_ARCH64 && current_function_returns_struct))

/* The current return address is in %i7.  The return address of anything
   farther back is in the register window save area at [%fp+60].  */
/* ??? This ignores the fact that the actual return address is +8 for normal
   returns, and +12 for structure returns.  */
#define RETURN_ADDR_RTX(count, frame)		\
  ((count == -1)				\
   ? gen_rtx_REG (Pmode, 31)			\
   : gen_rtx_MEM (Pmode,			\
		  memory_address (Pmode, plus_constant (frame, \
							15 * UNITS_PER_WORD \
							+ SPARC_STACK_BIAS))))

/* Before the prologue, the return address is %o7 + 8.  OK, sometimes it's
   +12, but always using +8 is close enough for frame unwind purposes.
   Actually, just using %o7 is close enough for unwinding, but %o7+8
   is something you can return to.  */
#define INCOMING_RETURN_ADDR_RTX \
  plus_constant (gen_rtx_REG (word_mode, 15), 8)
#define DWARF_FRAME_RETURN_COLUMN	DWARF_FRAME_REGNUM (15)

/* The offset from the incoming value of %sp to the top of the stack frame
   for the current function.  On sparc64, we have to account for the stack
   bias if present.  */
#define INCOMING_FRAME_SP_OFFSET SPARC_STACK_BIAS

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 24 : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, 1)	/* %g1 */
#define EH_RETURN_HANDLER_RTX	gen_rtx_REG (Pmode, 31)	/* %i7 */

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   If assembler and linker properly support .uaword %r_disp32(foo),
   then use PC relative 32-bit relocations instead of absolute relocs
   for shared libraries.  On sparc64, use pc relative 32-bit relocs even
   for binaries, to save memory.

   binutils 2.12 would emit a R_SPARC_DISP32 dynamic relocation if the
   symbol %r_disp32() is against was not local, but .hidden.  In that
   case, we have to use DW_EH_PE_absptr for pic personality.  */
#ifdef HAVE_AS_SPARC_UA_PCREL
#ifdef HAVE_AS_SPARC_UA_PCREL_HIDDEN
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic								\
   ? (GLOBAL ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4\
   : ((TARGET_ARCH64 && ! GLOBAL)					\
      ? (DW_EH_PE_pcrel | DW_EH_PE_sdata4)				\
      : DW_EH_PE_absptr))
#else
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic								\
   ? (GLOBAL ? DW_EH_PE_absptr : (DW_EH_PE_pcrel | DW_EH_PE_sdata4))	\
   : ((TARGET_ARCH64 && ! GLOBAL)					\
      ? (DW_EH_PE_pcrel | DW_EH_PE_sdata4)				\
      : DW_EH_PE_absptr))
#endif

/* Emit a PC-relative relocation.  */
#define ASM_OUTPUT_DWARF_PCREL(FILE, SIZE, LABEL)	\
  do {							\
    fputs (integer_asm_op (SIZE, FALSE), FILE);		\
    fprintf (FILE, "%%r_disp%d(", SIZE * 8);		\
    assemble_name (FILE, LABEL);			\
    fputc (')', FILE);					\
  } while (0)
#endif

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT 0 */
/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_DECREMENT 0 */
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < (unsigned)32	\
 || (REGNO) == FRAME_POINTER_REGNUM				\
 || reg_renumber[REGNO] == FRAME_POINTER_REGNUM)

#define REGNO_OK_FOR_BASE_P(REGNO)  REGNO_OK_FOR_INDEX_P (REGNO)

#define REGNO_OK_FOR_FP_P(REGNO) \
  (((unsigned) (REGNO) - 32 < (TARGET_V9 ? (unsigned)64 : (unsigned)32)) \
   || ((unsigned) reg_renumber[REGNO] - 32 < (TARGET_V9 ? (unsigned)64 : (unsigned)32)))
#define REGNO_OK_FOR_CCFP_P(REGNO) \
 (TARGET_V9 \
  && (((unsigned) (REGNO) - 96 < (unsigned)4) \
      || ((unsigned) reg_renumber[REGNO] - 96 < (unsigned)4)))

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the SPARC, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Is X, a REG, an in or global register?  i.e. is regno 0..7 or 24..31 */
#define IN_OR_GLOBAL_P(X) (REGNO (X) < 8 || (REGNO (X) >= 24 && REGNO (X) <= 31))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.
   When PIC, we do not accept an address that would require a scratch reg
   to load into a register.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == HIGH			\
   || (GET_CODE (X) == CONST						\
       && ! (flag_pic && pic_address_needs_scratch (X))))

/* Define this, so that when PIC, reload won't try to reload invalid
   addresses which require two reload registers.  */

#define LEGITIMATE_PIC_OPERAND_P(X)  (! pic_address_needs_scratch (X))

/* Nonzero if the constant value X is a legitimate general operand.
   Anything can be made to work except floating point constants.
   If TARGET_VIS, 0.0 can be made to work as well.  */

#define LEGITIMATE_CONSTANT_P(X) 					\
  (GET_CODE (X) != CONST_DOUBLE || GET_MODE (X) == VOIDmode || 		\
   (TARGET_VIS &&							\
    (GET_MODE (X) == SFmode || GET_MODE (X) == DFmode ||		\
     GET_MODE (X) == TFmode) &&						\
    fp_zero_operand (X, GET_MODE (X))))

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

/* Optional extra constraints for this machine.

   'Q' handles floating point constants which can be moved into
       an integer register with a single sethi instruction.

   'R' handles floating point constants which can be moved into
       an integer register with a single mov instruction.

   'S' handles floating point constants which can be moved into
       an integer register using a high/lo_sum sequence.

   'T' handles memory addresses where the alignment is known to
       be at least 8 bytes.

   `U' handles all pseudo registers or a hard even numbered
       integer register, needed for ldd/std instructions.

   'W' handles the memory operand when moving operands in/out
       of 'e' constraint floating point registers.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) < 32				\
   || REGNO (X) == FRAME_POINTER_REGNUM		\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)  REG_OK_FOR_INDEX_P (X)

/* 'T', 'U' are for aligned memory loads which aren't needed for arch64.
   'W' is like 'T' but is assumed true on arch64.

   Remember to accept pseudo-registers for memory constraints if reload is
   in progress.  */

#define EXTRA_CONSTRAINT(OP, C) \
	sparc_extra_constraint_check(OP, C, 0)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#define EXTRA_CONSTRAINT(OP, C) \
	sparc_extra_constraint_check(OP, C, 1)

#endif

/* Should gcc use [%reg+%lo(xx)+offset] addresses?  */

#ifdef HAVE_AS_OFFSETABLE_LO10
#define USE_AS_OFFSETABLE_LO10 1
#else
#define USE_AS_OFFSETABLE_LO10 0
#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On SPARC, the actual legitimate addresses must be REG+REG or REG+SMALLINT
   ordinarily.  This changes a bit when generating PIC.

   If you change this, execute "rm explow.o recog.o reload.o".  */

#define SYMBOLIC_CONST(X) symbolic_operand (X, VOIDmode)

#define RTX_OK_FOR_BASE_P(X)						\
  ((GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
  || (GET_CODE (X) == SUBREG						\
      && GET_CODE (SUBREG_REG (X)) == REG				\
      && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define RTX_OK_FOR_INDEX_P(X)						\
  ((GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))			\
  || (GET_CODE (X) == SUBREG						\
      && GET_CODE (SUBREG_REG (X)) == REG				\
      && REG_OK_FOR_INDEX_P (SUBREG_REG (X))))

#define RTX_OK_FOR_OFFSET_P(X)						\
  (GET_CODE (X) == CONST_INT && INTVAL (X) >= -0x1000 && INTVAL (X) < 0x1000 - 8)
  
#define RTX_OK_FOR_OLO10_P(X)						\
  (GET_CODE (X) == CONST_INT && INTVAL (X) >= -0x1000 && INTVAL (X) < 0xc00 - 8)

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (RTX_OK_FOR_BASE_P (X))				\
    goto ADDR;						\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      register rtx op0 = XEXP (X, 0);			\
      register rtx op1 = XEXP (X, 1);			\
      if (flag_pic && op0 == pic_offset_table_rtx)	\
	{						\
	  if (RTX_OK_FOR_BASE_P (op1))			\
	    goto ADDR;					\
	  else if (flag_pic == 1			\
		   && GET_CODE (op1) != REG		\
		   && GET_CODE (op1) != LO_SUM		\
		   && GET_CODE (op1) != MEM		\
		   && (! SYMBOLIC_CONST (op1)		\
		       || MODE == Pmode)		\
		   && (GET_CODE (op1) != CONST_INT	\
		       || SMALL_INT (op1)))		\
	    goto ADDR;					\
	}						\
      else if (RTX_OK_FOR_BASE_P (op0))			\
	{						\
	  if ((RTX_OK_FOR_INDEX_P (op1)			\
 	      /* We prohibit REG + REG for TFmode when	\
		 there are no instructions which accept	\
		 REG+REG instructions.  We do this	\
		 because REG+REG is not an offsetable	\
		 address.  If we get the situation	\
		 in reload where source and destination	\
		 of a movtf pattern are both MEMs with	\
		 REG+REG address, then only one of them	\
		 gets converted to an offsetable	\
		 address.  */				\
 	       && (MODE != TFmode			\
		   || (TARGET_FPU && TARGET_ARCH64	\
		       && TARGET_V9			\
		       && TARGET_HARD_QUAD))		\
	      /* We prohibit REG + REG on ARCH32 if	\
		 not optimizing for DFmode/DImode	\
		 because then mem_min_alignment is	\
		 likely to be zero after reload and the \
		 forced split would lack a matching	\
		 splitter pattern.  */			\
	       && (TARGET_ARCH64 || optimize		\
		   || (MODE != DFmode			\
		       && MODE != DImode)))		\
	      || RTX_OK_FOR_OFFSET_P (op1))		\
	    goto ADDR;					\
	}						\
      else if (RTX_OK_FOR_BASE_P (op1))			\
	{						\
	  if ((RTX_OK_FOR_INDEX_P (op0)			\
 	      /* See the previous comment.  */		\
 	       && (MODE != TFmode			\
		  || (TARGET_FPU && TARGET_ARCH64	\
		      && TARGET_V9			\
		      && TARGET_HARD_QUAD))		\
	       && (TARGET_ARCH64 || optimize		\
		   || (MODE != DFmode			\
		       && MODE != DImode)))		\
	      || RTX_OK_FOR_OFFSET_P (op0))		\
	    goto ADDR;					\
	}						\
      else if (USE_AS_OFFSETABLE_LO10			\
	       && GET_CODE (op0) == LO_SUM		\
	       && TARGET_ARCH64				\
	       && ! TARGET_CM_MEDMID			\
	       && RTX_OK_FOR_OLO10_P (op1))		\
	{						\
	  register rtx op00 = XEXP (op0, 0);		\
	  register rtx op01 = XEXP (op0, 1);		\
	  if (RTX_OK_FOR_BASE_P (op00)			\
	      && CONSTANT_P (op01))			\
	    goto ADDR;					\
	}						\
      else if (USE_AS_OFFSETABLE_LO10			\
	       && GET_CODE (op1) == LO_SUM		\
	       && TARGET_ARCH64				\
	       && ! TARGET_CM_MEDMID			\
	       && RTX_OK_FOR_OLO10_P (op0))		\
	{						\
	  register rtx op10 = XEXP (op1, 0);		\
	  register rtx op11 = XEXP (op1, 1);		\
	  if (RTX_OK_FOR_BASE_P (op10)			\
	      && CONSTANT_P (op11))			\
	    goto ADDR;					\
	}						\
    }							\
  else if (GET_CODE (X) == LO_SUM)			\
    {							\
      register rtx op0 = XEXP (X, 0);			\
      register rtx op1 = XEXP (X, 1);			\
      if (RTX_OK_FOR_BASE_P (op0)			\
	  && CONSTANT_P (op1)				\
	  /* We can't allow TFmode, because an offset	\
	     greater than or equal to the alignment (8)	\
	     may cause the LO_SUM to overflow if !v9.  */\
	  && (MODE != TFmode || TARGET_V9))		\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == CONST_INT && SMALL_INT (X))	\
    goto ADDR;						\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   In PIC mode,

      (mem:HI [%l7+a])

   is not equivalent to
   
      (mem:QI [%l7+a]) (mem:QI [%l7+a+1])

   because [%l7+a+1] is interpreted as the address of (a+1).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)	\
{							\
  if (flag_pic == 1)					\
    {							\
      if (GET_CODE (ADDR) == PLUS)			\
	{						\
	  rtx op0 = XEXP (ADDR, 0);			\
	  rtx op1 = XEXP (ADDR, 1);			\
	  if (op0 == pic_offset_table_rtx		\
	      && SYMBOLIC_CONST (op1))			\
	    goto LABEL;					\
	}						\
    }							\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

/* On SPARC, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */
#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	\
{ rtx sparc_x = (X);						\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx_PLUS (Pmode, XEXP (X, 1),			\
			force_operand (XEXP (X, 0), NULL_RTX));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx_PLUS (Pmode, XEXP (X, 0),			\
			force_operand (XEXP (X, 1), NULL_RTX));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == PLUS)	\
    (X) = gen_rtx_PLUS (Pmode, force_operand (XEXP (X, 0), NULL_RTX),\
			XEXP (X, 1));				\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == PLUS)	\
    (X) = gen_rtx_PLUS (Pmode, XEXP (X, 0),			\
			force_operand (XEXP (X, 1), NULL_RTX));	\
  if (sparc_x != (X) && memory_address_p (MODE, X))		\
    goto WIN;							\
  if (flag_pic) (X) = legitimize_pic_address (X, MODE, 0);	\
  else if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))	\
    (X) = gen_rtx_PLUS (Pmode, XEXP (X, 0),			\
			copy_to_mode_reg (Pmode, XEXP (X, 1)));	\
  else if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))	\
    (X) = gen_rtx_PLUS (Pmode, XEXP (X, 1),			\
			copy_to_mode_reg (Pmode, XEXP (X, 0)));	\
  else if (GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == CONST	\
	   || GET_CODE (X) == LABEL_REF)			\
    (X) = copy_to_suggested_reg (X, NULL_RTX, Pmode); 		\
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.

   For Sparc 32, we wish to handle addresses by splitting them into
   HIGH+LO_SUM pairs, retaining the LO_SUM in the memory reference. 
   This cuts the number of extra insns by one.

   Do nothing when generating PIC code and the address is a
   symbolic operand or requires a scratch register.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)     \
do {                                                                    \
  /* Decompose SImode constants into hi+lo_sum.  We do have to 		\
     rerecognize what we produce, so be careful.  */			\
  if (CONSTANT_P (X)							\
      && (MODE != TFmode || TARGET_ARCH64)				\
      && GET_MODE (X) == SImode						\
      && GET_CODE (X) != LO_SUM && GET_CODE (X) != HIGH			\
      && ! (flag_pic							\
	    && (symbolic_operand (X, Pmode)				\
		|| pic_address_needs_scratch (X)))			\
      && sparc_cmodel <= CM_MEDLOW)					\
    {									\
      X = gen_rtx_LO_SUM (GET_MODE (X),					\
			  gen_rtx_HIGH (GET_MODE (X), X), X);		\
      push_reload (XEXP (X, 0), NULL_RTX, &XEXP (X, 0), NULL,		\
                   BASE_REG_CLASS, GET_MODE (X), VOIDmode, 0, 0,	\
                   OPNUM, TYPE);					\
      goto WIN;								\
    }									\
  /* ??? 64-bit reloads.  */						\
} while (0)

/* If we are referencing a function make the SYMBOL_REF special.
   In the Embedded Medium/Anywhere code model, %g4 points to the data segment
   so we must not add it to function addresses.  */

#define ENCODE_SECTION_INFO(DECL) \
  do {							\
    if (TARGET_CM_EMBMEDANY && TREE_CODE (DECL) == FUNCTION_DECL) \
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;	\
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
/* If we ever implement any of the full models (such as CM_FULLANY),
   this has to be DImode in that case */
#ifdef HAVE_GAS_SUBSECTION_ORDERING
#define CASE_VECTOR_MODE \
(! TARGET_PTR64 ? SImode : flag_pic ? SImode : TARGET_CM_MEDLOW ? SImode : DImode)
#else
/* If assembler does not have working .subsection -1, we use DImode for pic, as otherwise
   we have to sign extend which slows things down.  */
#define CASE_VECTOR_MODE \
(! TARGET_PTR64 ? SImode : flag_pic ? DImode : TARGET_CM_MEDLOW ? SImode : DImode)
#endif

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

#if 0 /* Sun 4 has matherr, so this is no good.  */
/* This is the value of the error code EDOM for this machine,
   used by the sqrt instruction.  */
#define TARGET_EDOM 33

/* This is how to refer to the variable errno.  */
#define GEN_ERRNO_RTX \
  gen_rtx_MEM (SImode, gen_rtx_SYMBOL_REF (Pmode, "errno"))
#endif /* 0 */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES (TARGET_ARCH32)

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode (TARGET_ARCH64 ? DImode : SImode)

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Add any extra modes needed to represent the condition code.

   On the Sparc, we have a "no-overflow" mode which is used when an add or
   subtract insn is used to set the condition code.  Different branches are
   used in this case for some operations.

   We also have two modes to indicate that the relevant condition code is
   in the floating-point condition code register.  One for comparisons which
   will generate an exception if the result is unordered (CCFPEmode) and
   one for comparisons which will never trap (CCFPmode).

   CCXmode and CCX_NOOVmode are only used by v9.  */

#define EXTRA_CC_MODES			\
    CC(CCXmode,	     "CCX")		\
    CC(CC_NOOVmode,  "CC_NOOV")		\
    CC(CCX_NOOVmode, "CCX_NOOV")	\
    CC(CCFPmode,     "CCFP")		\
    CC(CCFPEmode,    "CCFPE")

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point,
   CCFP[E]mode is used.  CC_NOOVmode should be used when the first operand
   is a PLUS, MINUS, NEG, or ASHIFT.  CCmode should be used when no special
   processing is needed.  */
#define SELECT_CC_MODE(OP,X,Y)  select_cc_mode ((OP), (X), (Y))

/* Return non-zero if MODE implies a floating point inequality can be
   reversed.  For Sparc this is always true because we have a full
   compliment of ordered and unordered comparisons, but until generic
   code knows how to reverse it correctly we keep the old definition.  */
#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPEmode && (MODE) != CCFPmode)

/* A function address in a call instruction for indexing purposes.  */
#define FUNCTION_MODE Pmode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* alloca should avoid clobbering the old register save area.  */
#define SETJMP_VIA_SAVE_AREA

/* Define subroutines to call to handle multiply and divide.
   Use the subroutines that Sun's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */

#define DIVSI3_LIBCALL "*.div"
#define UDIVSI3_LIBCALL "*.udiv"
#define MODSI3_LIBCALL "*.rem"
#define UMODSI3_LIBCALL "*.urem"
/* .umul is a little faster than .mul.  */
#define MULSI3_LIBCALL "*.umul"

/* Define library calls for quad FP operations.  These are all part of the
   SPARC 32bit ABI.  */
#define ADDTF3_LIBCALL "_Q_add"
#define SUBTF3_LIBCALL "_Q_sub"
#define NEGTF2_LIBCALL "_Q_neg"
#define MULTF3_LIBCALL "_Q_mul"
#define DIVTF3_LIBCALL "_Q_div"
#define FLOATSITF2_LIBCALL "_Q_itoq"
#define FIX_TRUNCTFSI2_LIBCALL "_Q_qtoi"
#define FIXUNS_TRUNCTFSI2_LIBCALL "_Q_qtou"
#define EXTENDSFTF2_LIBCALL "_Q_stoq"
#define TRUNCTFSF2_LIBCALL "_Q_qtos"
#define EXTENDDFTF2_LIBCALL "_Q_dtoq"
#define TRUNCTFDF2_LIBCALL "_Q_qtod"
#define EQTF2_LIBCALL "_Q_feq"
#define NETF2_LIBCALL "_Q_fne"
#define GTTF2_LIBCALL "_Q_fgt"
#define GETF2_LIBCALL "_Q_fge"
#define LTTF2_LIBCALL "_Q_flt"
#define LETF2_LIBCALL "_Q_fle"

/* Assume by default that the _Qp_* 64-bit libcalls are implemented such
   that the inputs are fully consumed before the output memory is clobbered.  */

#define TARGET_BUGGY_QP_LIB	0

/* We can define the TFmode sqrt optab only if TARGET_FPU.  This is because
   with soft-float, the SFmode and DFmode sqrt instructions will be absent,
   and the compiler will notice and try to use the TFmode sqrt instruction
   for calls to the builtin function sqrt, but this fails.  */
#define INIT_TARGET_OPTABS						\
  do {									\
    if (TARGET_ARCH32)							\
      {									\
	add_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (ADDTF3_LIBCALL);				\
	sub_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (SUBTF3_LIBCALL);				\
	neg_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (NEGTF2_LIBCALL);				\
	smul_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (MULTF3_LIBCALL);				\
	sdiv_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (DIVTF3_LIBCALL);				\
	eqtf2_libfunc = init_one_libfunc (EQTF2_LIBCALL);		\
	netf2_libfunc = init_one_libfunc (NETF2_LIBCALL);		\
	gttf2_libfunc = init_one_libfunc (GTTF2_LIBCALL);		\
	getf2_libfunc = init_one_libfunc (GETF2_LIBCALL);		\
	lttf2_libfunc = init_one_libfunc (LTTF2_LIBCALL);		\
	letf2_libfunc = init_one_libfunc (LETF2_LIBCALL);		\
	trunctfsf2_libfunc = init_one_libfunc (TRUNCTFSF2_LIBCALL);	\
	trunctfdf2_libfunc = init_one_libfunc (TRUNCTFDF2_LIBCALL);	\
	extendsftf2_libfunc = init_one_libfunc (EXTENDSFTF2_LIBCALL);	\
	extenddftf2_libfunc = init_one_libfunc (EXTENDDFTF2_LIBCALL);	\
	floatsitf_libfunc = init_one_libfunc (FLOATSITF2_LIBCALL);	\
	fixtfsi_libfunc = init_one_libfunc (FIX_TRUNCTFSI2_LIBCALL);	\
	fixunstfsi_libfunc						\
	  = init_one_libfunc (FIXUNS_TRUNCTFSI2_LIBCALL);		\
	if (TARGET_FPU)							\
	  sqrt_optab->handlers[(int) TFmode].libfunc			\
	    = init_one_libfunc ("_Q_sqrt");				\
      }									\
    if (TARGET_ARCH64)							\
      {									\
        /* In the SPARC 64bit ABI, these libfuncs do not exist in the	\
           library.  Make sure the compiler does not emit calls to them	\
	   by accident.  */						\
	sdiv_optab->handlers[(int) SImode].libfunc = NULL;		\
	udiv_optab->handlers[(int) SImode].libfunc = NULL;		\
	smod_optab->handlers[(int) SImode].libfunc = NULL;		\
	umod_optab->handlers[(int) SImode].libfunc = NULL;		\
        smul_optab->handlers[(int) SImode].libfunc = NULL;		\
      }									\
    INIT_SUBTARGET_OPTABS;						\
  } while (0)

/* This is meant to be redefined in the host dependent files */
#define INIT_SUBTARGET_OPTABS

/* Nonzero if a floating point comparison library call for
   mode MODE that will return a boolean value.  Zero if one
   of the libgcc2 functions is used.  */
#define FLOAT_LIB_COMPARE_RETURNS_BOOL(MODE, COMPARISON) ((MODE) == TFmode)

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) < 0x1000 && INTVAL (RTX) >= -0x1000)	\
      return 0;							\
  case HIGH:							\
    return 2;							\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 4;							\
  case CONST_DOUBLE:						\
    if (GET_MODE (RTX) == DImode)				\
      if ((XINT (RTX, 3) == 0					\
	   && (unsigned) XINT (RTX, 2) < 0x1000)		\
	  || (XINT (RTX, 3) == -1				\
	      && XINT (RTX, 2) < 0				\
	      && XINT (RTX, 2) >= -0x1000))			\
	return 0;						\
    return 8;

#define ADDRESS_COST(RTX)  1

/* Compute extra cost of moving data between one register class
   and another.  */
#define GENERAL_OR_I64(C) ((C) == GENERAL_REGS || (C) == I64_REGS)
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)		\
  (((FP_REG_CLASS_P (CLASS1) && GENERAL_OR_I64 (CLASS2)) \
    || (GENERAL_OR_I64 (CLASS1) && FP_REG_CLASS_P (CLASS2)) \
    || (CLASS1) == FPCC_REGS || (CLASS2) == FPCC_REGS)		\
   ? (sparc_cpu == PROCESSOR_ULTRASPARC ? 12 : 6) : 2)

/* Provide the cost of a branch.  For pre-v9 processors we use
   a value of 3 to take into account the potential annulling of
   the delay slot (which ends up being a bubble in the pipeline slot)
   plus a cycle to take into consideration the instruction cache
   effects.

   On v9 and later, which have branch prediction facilities, we set
   it to the depth of the pipeline as that is the cost of a
   mispredicted branch.

   ??? Set to 9 when PROCESSOR_ULTRASPARC3 is added  */

#define BRANCH_COST \
	((sparc_cpu == PROCESSOR_V9 \
	  || sparc_cpu == PROCESSOR_ULTRASPARC) \
	 ? 7 : 3)

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  The purpose for the cost of MULT is to encourage
   `synth_mult' to find a synthetic multiply when reasonable.

   If we need more than 12 insns to do a multiply, then go out-of-line,
   since the call overhead will be < 10% of the cost of the multiply.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case MULT:						\
    if (sparc_cpu == PROCESSOR_ULTRASPARC)		\
      return (GET_MODE (X) == DImode ?			\
              COSTS_N_INSNS (34) : COSTS_N_INSNS (19));	\
    return TARGET_HARD_MUL ? COSTS_N_INSNS (5) : COSTS_N_INSNS (25); \
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    if (sparc_cpu == PROCESSOR_ULTRASPARC)		\
      return (GET_MODE (X) == DImode ?			\
              COSTS_N_INSNS (68) : COSTS_N_INSNS (37));	\
    return COSTS_N_INSNS (25);				\
  /* Make FLOAT and FIX more expensive than CONST_DOUBLE,\
     so that cse will favor the latter.  */		\
  case FLOAT:						\
  case FIX:						\
    return 19;

#define PREFETCH_BLOCK \
	((sparc_cpu == PROCESSOR_ULTRASPARC) ? 64 : 32)

/* ??? UltraSPARC-III note: Can set this to 8 for ultra3.  */
#define SIMULTANEOUS_PREFETCHES \
	((sparc_cpu == PROCESSOR_ULTRASPARC) ? 2 : 3)

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(file)

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */

#define ASM_COMMENT_START "!"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* ??? Try to make the style consistent here (_OP?).  */

#define ASM_FLOAT	".single"
#define ASM_DOUBLE	".double"
#define ASM_LONGDOUBLE	".xxx"		/* ??? Not known (or used yet).  */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"%g0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",		\
 "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",		\
 "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",		\
 "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7",		\
 "%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",		\
 "%f8", "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",		\
 "%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",	\
 "%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31",	\
 "%f32", "%f33", "%f34", "%f35", "%f36", "%f37", "%f38", "%f39",	\
 "%f40", "%f41", "%f42", "%f43", "%f44", "%f45", "%f46", "%f47",	\
 "%f48", "%f49", "%f50", "%f51", "%f52", "%f53", "%f54", "%f55",	\
 "%f56", "%f57", "%f58", "%f59", "%f60", "%f61", "%f62", "%f63",	\
 "%fcc0", "%fcc1", "%fcc2", "%fcc3", "%icc", "%sfp" }

/* Define additional names for use in asm clobbers and asm declarations.  */

#define ADDITIONAL_REGISTER_NAMES \
{{"ccr", SPARC_ICC_REG}, {"cc", SPARC_ICC_REG}}

/* On Sun 4, this limit is 2048.  We use 1000 to be safe, since the length
   can run past this up to a continuation point.  Once we used 1500, but
   a single entry in C++ can run more than 500 bytes, due to the length of
   mangled symbol names.  dbxout.c should really be fixed to do
   continuations when they are actually needed instead of trying to
   guess...  */
#define DBX_CONTIN_LENGTH 1000

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.global ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf ((LABEL), "*%s%ld", (PREFIX), (long)(NUM))

/* This is how we hook in and defer the case-vector until the end of
   the function.  */
#define ASM_OUTPUT_ADDR_VEC(LAB,VEC) \
  sparc_defer_case_vector ((LAB),(VEC), 0)

#define ASM_OUTPUT_ADDR_DIFF_VEC(LAB,VEC) \
  sparc_defer_case_vector ((LAB),(VEC), 1)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do {									\
  char label[30];							\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);			\
  if (CASE_VECTOR_MODE == SImode)					\
    fprintf (FILE, "\t.word\t");					\
  else									\
    fprintf (FILE, "\t.xword\t");					\
  assemble_name (FILE, label);						\
  fputc ('\n', FILE);							\
} while (0)

/* This is how to output an element of a case-vector that is relative.
   (SPARC uses such vectors only when generating PIC.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)		\
do {									\
  char label[30];							\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));			\
  if (CASE_VECTOR_MODE == SImode)					\
    fprintf (FILE, "\t.word\t");					\
  else									\
    fprintf (FILE, "\t.xword\t");					\
  assemble_name (FILE, label);						\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", (REL));			\
  fputc ('-', FILE);							\
  assemble_name (FILE, label);						\
  fputc ('\n', FILE);							\
} while (0)

/* This is what to output before and after case-vector (both
   relative and absolute).  If .subsection -1 works, we put case-vectors
   at the beginning of the current section.  */

#ifdef HAVE_GAS_SUBSECTION_ORDERING

#define ASM_OUTPUT_ADDR_VEC_START(FILE)					\
  fprintf(FILE, "\t.subsection\t-1\n")

#define ASM_OUTPUT_ADDR_VEC_END(FILE)					\
  fprintf(FILE, "\t.previous\n")

#endif

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (1<<(LOG)))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.common ", (FILE)),		\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u,\"bss\"\n", (SIZE)))

/* This says how to output an assembler line to define a local common
   symbol.  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
( fputs ("\t.reserve ", (FILE)),					\
  assemble_name ((FILE), (NAME)),					\
  fprintf ((FILE), ",%u,\"bss\",%u\n",					\
	   (SIZE), ((ALIGNED) / BITS_PER_UNIT)))

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
  do {								\
    fputs (".globl ", (FILE));					\
    assemble_name ((FILE), (NAME));				\
    fputs ("\n", (FILE));					\
    ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);		\
  } while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

#define IDENT_ASM_OP "\t.ident\t"

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "%s\"%s\"\n", IDENT_ASM_OP, NAME);

/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION) \
  sparc_output_mi_thunk (FILE, THUNK_FNDECL, DELTA, FUNCTION)

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR) == '#' || (CHAR) == '*' || (CHAR) == '^' || (CHAR) == '(' || (CHAR) == '_')

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;					\
  int offset = 0;						\
  register rtx addr = ADDR;					\
  if (GET_CODE (addr) == REG)					\
    fputs (reg_names[REGNO (addr)], FILE);			\
  else if (GET_CODE (addr) == PLUS)				\
    {								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);\
      else							\
	base = XEXP (addr, 0), index = XEXP (addr, 1);		\
      if (GET_CODE (base) == LO_SUM)				\
	{							\
	  if (! USE_AS_OFFSETABLE_LO10				\
	      || TARGET_ARCH32					\
	      || TARGET_CM_MEDMID)				\
	    abort ();						\
	  output_operand (XEXP (base, 0), 0);			\
	  fputs ("+%lo(", FILE);				\
	  output_address (XEXP (base, 1));			\
	  fprintf (FILE, ")+%d", offset);			\
	}							\
      else							\
	{							\
	  fputs (reg_names[REGNO (base)], FILE);		\
	  if (index == 0)					\
	    fprintf (FILE, "%+d", offset);			\
	  else if (GET_CODE (index) == REG)			\
	    fprintf (FILE, "+%s", reg_names[REGNO (index)]);	\
	  else if (GET_CODE (index) == SYMBOL_REF		\
		   || GET_CODE (index) == CONST)		\
	    fputc ('+', FILE), output_addr_const (FILE, index);	\
	  else abort ();					\
	}							\
    }								\
  else if (GET_CODE (addr) == MINUS				\
	   && GET_CODE (XEXP (addr, 1)) == LABEL_REF)		\
    {								\
      output_addr_const (FILE, XEXP (addr, 0));			\
      fputs ("-(", FILE);					\
      output_addr_const (FILE, XEXP (addr, 1));			\
      fputs ("-.)", FILE);					\
    }								\
  else if (GET_CODE (addr) == LO_SUM)				\
    {								\
      output_operand (XEXP (addr, 0), 0);			\
      if (TARGET_CM_MEDMID)					\
        fputs ("+%l44(", FILE);					\
      else							\
        fputs ("+%lo(", FILE);					\
      output_address (XEXP (addr, 1));				\
      fputc (')', FILE);					\
    }								\
  else if (flag_pic && GET_CODE (addr) == CONST			\
	   && GET_CODE (XEXP (addr, 0)) == MINUS		\
	   && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST	\
	   && GET_CODE (XEXP (XEXP (XEXP (addr, 0), 1), 0)) == MINUS	\
	   && XEXP (XEXP (XEXP (XEXP (addr, 0), 1), 0), 1) == pc_rtx)	\
    {								\
      addr = XEXP (addr, 0);					\
      output_addr_const (FILE, XEXP (addr, 0));			\
      /* Group the args of the second CONST in parenthesis.  */	\
      fputs ("-(", FILE);					\
      /* Skip past the second CONST--it does nothing for us.  */\
      output_addr_const (FILE, XEXP (XEXP (addr, 1), 0));	\
      /* Close the parenthesis.  */				\
      fputc (')', FILE);					\
    }								\
  else								\
    {								\
      output_addr_const (FILE, addr);				\
    }								\
}

/* Define the codes that are matched by predicates in sparc.c.  */

#define PREDICATE_CODES							\
{"reg_or_0_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}},		\
{"fp_zero_operand", {CONST_DOUBLE}},					\
{"fp_register_operand", {SUBREG, REG}},					\
{"intreg_operand", {SUBREG, REG}},					\
{"fcc_reg_operand", {REG}},						\
{"fcc0_reg_operand", {REG}},						\
{"icc_or_fcc_reg_operand", {REG}},					\
{"restore_operand", {REG}},						\
{"call_operand", {MEM}},						\
{"call_operand_address", {SYMBOL_REF, LABEL_REF, CONST, CONST_DOUBLE,	\
	ADDRESSOF, SUBREG, REG, PLUS, LO_SUM, CONST_INT}},		\
{"symbolic_operand", {SYMBOL_REF, LABEL_REF, CONST}},			\
{"symbolic_memory_operand", {SUBREG, MEM}},				\
{"label_ref_operand", {LABEL_REF}},					\
{"sp64_medium_pic_operand", {CONST}},					\
{"data_segment_operand", {SYMBOL_REF, PLUS, CONST}},			\
{"text_segment_operand", {LABEL_REF, SYMBOL_REF, PLUS, CONST}},		\
{"reg_or_nonsymb_mem_operand", {SUBREG, REG, MEM}},			\
{"splittable_symbolic_memory_operand", {MEM}},				\
{"splittable_immediate_memory_operand", {MEM}},				\
{"eq_or_neq", {EQ, NE}},						\
{"normal_comp_operator", {GE, GT, LE, LT, GTU, LEU}},			\
{"noov_compare_op", {NE, EQ, GE, GT, LE, LT, GEU, GTU, LEU, LTU}},	\
{"noov_compare64_op", {NE, EQ, GE, GT, LE, LT, GEU, GTU, LEU, LTU}},	\
{"v9_regcmp_op", {EQ, NE, GE, LT, LE, GT}},				\
{"extend_op", {SIGN_EXTEND, ZERO_EXTEND}},				\
{"cc_arithop", {AND, IOR, XOR}},					\
{"cc_arithopn", {AND, IOR}},						\
{"arith_operand", {SUBREG, REG, CONST_INT}},				\
{"arith_add_operand", {SUBREG, REG, CONST_INT}},			\
{"arith11_operand", {SUBREG, REG, CONST_INT}},				\
{"arith10_operand", {SUBREG, REG, CONST_INT}},				\
{"arith_double_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}},	\
{"arith_double_add_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}},	\
{"arith11_double_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}},	\
{"arith10_double_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE}},	\
{"small_int", {CONST_INT}},						\
{"small_int_or_double", {CONST_INT, CONST_DOUBLE}},			\
{"uns_small_int", {CONST_INT}},						\
{"uns_arith_operand", {SUBREG, REG, CONST_INT}},			\
{"clobbered_register", {REG}},						\
{"input_operand", {SUBREG, REG, CONST_INT, MEM, CONST}},		\
{"const64_operand", {CONST_INT, CONST_DOUBLE}},				\
{"const64_high_operand", {CONST_INT, CONST_DOUBLE}},

/* The number of Pmode words for the setjmp buffer.  */
#define JMP_BUF_SIZE 12

#define DONT_ACCESS_GBLS_AFTER_EPILOGUE (flag_pic)

