/* Definitions of target machine for GNU compiler, for the HP Spectrum.
   Copyright (C) 1992, 93-98, 1999 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) of Cygnus Support
   and Tim Moore (moore@defmacro.cs.utah.edu) of the Center for
   Software Science at the University of Utah.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

enum cmp_type				/* comparison type */
{
  CMP_SI,				/* compare integers */
  CMP_SF,				/* compare single precision floats */
  CMP_DF,				/* compare double precision floats */
  CMP_MAX				/* max comparison type */
};

/* For long call handling.  */
extern unsigned int total_code_bytes;

/* Which processor to schedule for.  */

enum processor_type
{
  PROCESSOR_700,
  PROCESSOR_7100,
  PROCESSOR_7100LC,
  PROCESSOR_7200,
  PROCESSOR_8000
};

/* For -mschedule= option.  */
extern char *pa_cpu_string;
extern enum processor_type pa_cpu;

#define pa_cpu_attr ((enum attr_cpu)pa_cpu)

/* The 700 can only issue a single insn at a time.
   The 7XXX processors can issue two insns at a time.
   The 8000 can issue 4 insns at a time.  */
#define ISSUE_RATE \
  (pa_cpu == PROCESSOR_700 ? 1 \
   : pa_cpu == PROCESSOR_7100 ? 2 \
   : pa_cpu == PROCESSOR_7100LC ? 2 \
   : pa_cpu == PROCESSOR_7200 ? 2 \
   : pa_cpu == PROCESSOR_8000 ? 4 \
   : 2)

/* Which architecture to generate code for.  */

enum architecture_type
{
  ARCHITECTURE_10,
  ARCHITECTURE_11,
  ARCHITECTURE_20
};

/* For -march= option.  */
extern char *pa_arch_string;
extern enum architecture_type pa_arch;

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fputs (" (hppa)", stderr);

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* compile code for HP-PA 1.1 ("Snake") */

#define MASK_PA_11 1
#define TARGET_PA_11 (target_flags & MASK_PA_11)

/* Disable all FP registers (they all become fixed).  This may be necessary
   for compiling kernels which perform lazy context switching of FP regs.
   Note if you use this option and try to perform floating point operations
   the compiler will abort!  */

#define MASK_DISABLE_FPREGS 2
#define TARGET_DISABLE_FPREGS (target_flags & MASK_DISABLE_FPREGS)

/* Generate code which assumes that calls through function pointers will
   never cross a space boundary.  Such assumptions are generally safe for
   building kernels and statically linked executables.  Code compiled with
   this option will fail miserably if the executable is dynamically linked
   or uses nested functions!

   This is also used to trigger aggressive unscaled index addressing.  */
#define MASK_NO_SPACE_REGS 4
#define TARGET_NO_SPACE_REGS (target_flags & MASK_NO_SPACE_REGS)

/* Allow unconditional jumps in the delay slots of call instructions.  */
#define MASK_JUMP_IN_DELAY 8
#define TARGET_JUMP_IN_DELAY (target_flags & MASK_JUMP_IN_DELAY)

/* Optimize for space.  Currently this only turns on out of line
   prologues and epilogues.  */
#define MASK_SPACE 16
#define TARGET_SPACE (target_flags & MASK_SPACE)

/* Disable indexed addressing modes.  */
#define MASK_DISABLE_INDEXING 32
#define TARGET_DISABLE_INDEXING (target_flags & MASK_DISABLE_INDEXING)

/* Emit code which follows the new portable runtime calling conventions
   HP wants everyone to use for ELF objects.  If at all possible you want
   to avoid this since it's a performance loss for non-prototyped code.

   Note TARGET_PORTABLE_RUNTIME also forces all calls to use inline
   long-call stubs which is quite expensive.  */
#define MASK_PORTABLE_RUNTIME 64
#define TARGET_PORTABLE_RUNTIME (target_flags & MASK_PORTABLE_RUNTIME)

/* Emit directives only understood by GAS.  This allows parameter
   relocations to work for static functions.  There is no way
   to make them work the HP assembler at this time.  */
#define MASK_GAS 128
#define TARGET_GAS (target_flags & MASK_GAS)

/* Emit code for processors which do not have an FPU.  */
#define MASK_SOFT_FLOAT 256
#define TARGET_SOFT_FLOAT (target_flags & MASK_SOFT_FLOAT)

/* Use 3-insn load/store sequences for access to large data segments
   in shared libraries on hpux10.  */
#define MASK_LONG_LOAD_STORE 512
#define TARGET_LONG_LOAD_STORE (target_flags & MASK_LONG_LOAD_STORE)

/* Use a faster sequence for indirect calls.  */
#define MASK_FAST_INDIRECT_CALLS 1024
#define TARGET_FAST_INDIRECT_CALLS (target_flags & MASK_FAST_INDIRECT_CALLS)

/* Generate code with big switch statements to avoid out of range branches
   occurring within the switch table.  */
#define MASK_BIG_SWITCH 2048
#define TARGET_BIG_SWITCH (target_flags & MASK_BIG_SWITCH)


/* Generate code for the HPPA 2.0 architecture.  TARGET_PA_11 should also be
   true when this is true.  */
#define MASK_PA_20 4096
#define TARGET_PA_20 (target_flags & MASK_PA_20)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES \
  {{"snake", MASK_PA_11, "Generate PA1.1 code"},			\
   {"nosnake", -(MASK_PA_11 | MASK_PA_20), "Generate PA1.0 code"},		\
   {"pa-risc-1-0", -(MASK_PA_11 | MASK_PA_20), "Generate PA1.0 code"},		\
   {"pa-risc-1-1", MASK_PA_11, "Generate PA1.1 code"},			\
   {"pa-risc-2-0", MASK_PA_20, "Generate PA2.0 code.  This option requires gas snapshot 19990413 or later"},			\
   {"disable-fpregs", MASK_DISABLE_FPREGS, "Disable FP regs"},		\
   {"no-disable-fpregs", -MASK_DISABLE_FPREGS, "Do not disable FP regs"},\
   {"no-space-regs", MASK_NO_SPACE_REGS, "Disable space regs"},		\
   {"space-regs", -MASK_NO_SPACE_REGS, "Do not disable space regs"},	\
   {"jump-in-delay", MASK_JUMP_IN_DELAY, "Put jumps in call delay slots"},\
   {"no-jump-in-delay", -MASK_JUMP_IN_DELAY, "Do not put jumps in call delay slots"},	\
   {"space", MASK_SPACE, "Optimize for code space"},			\
   {"no-space", -MASK_SPACE, "Do not optimize for code space"},		\
   {"disable-indexing", MASK_DISABLE_INDEXING, "Disable indexed addressing"},\
   {"no-disable-indexing", -MASK_DISABLE_INDEXING, "Do not disable indexed addressing"},\
   {"portable-runtime", MASK_PORTABLE_RUNTIME, "Use portable calling conventions"},	\
   {"no-portable-runtime", -MASK_PORTABLE_RUNTIME, "Do not use portable calling conventions"},\
   {"gas", MASK_GAS, "Assume code will be assembled by GAS"},		\
   {"no-gas", -MASK_GAS, "Do not assume code will be assembled by GAS"},		\
   {"soft-float", MASK_SOFT_FLOAT, "Use software floating point"},		\
   {"no-soft-float", -MASK_SOFT_FLOAT, "Do not use software floating point"},	\
   {"long-load-store", MASK_LONG_LOAD_STORE, "Emit long load/store sequences"},	\
   {"no-long-load-store", -MASK_LONG_LOAD_STORE, "Do not emit long load/store sequences"},\
   {"fast-indirect-calls", MASK_FAST_INDIRECT_CALLS, "Generate fast indirect calls"},\
   {"no-fast-indirect-calls", -MASK_FAST_INDIRECT_CALLS, "Do not generate fast indirect calls"},\
   {"big-switch", MASK_BIG_SWITCH, "Generate code for huge switch statements"},	\
   {"no-big-switch", -MASK_BIG_SWITCH, "Do not generate code for huge switch statements"},	\
   {"linker-opt", 0, "Enable linker optimizations"},		\
   { "", TARGET_DEFAULT | TARGET_CPU_DEFAULT, NULL}}

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_GAS | MASK_JUMP_IN_DELAY)
#endif

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

#define TARGET_OPTIONS			\
{					\
  { "schedule=",	&pa_cpu_string, "Specify CPU for scheduling purposes" },\
  { "arch=",		&pa_arch_string, "Specify architecture for code generation.  Values are 1.0, 1.1, and 2.0.  2.0 requires gas snapshot 19990413 or later." }\
}

#define OVERRIDE_OPTIONS override_options ()

#define DBX_DEBUGGING_INFO
#define DEFAULT_GDB_EXTENSIONS 1

/* This is the way other stabs-in-XXX tools do things.  We will be
   compatible.  */
#define DBX_BLOCKS_FUNCTION_RELATIVE 1

/* Likewise for linenos.

   We make the first line stab special to avoid adding several
   gross hacks to GAS.  */
#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    static tree last_function_decl = NULL;		\
    if (current_function_decl == last_function_decl)	\
      fprintf (file, "\t.stabn 68,0,%d,L$M%d-%s\nL$M%d:\n",	\
	       line, sym_lineno,			\
	       XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0) + 1, \
	       sym_lineno);				\
    else						\
      fprintf (file, "\t.stabn 68,0,%d,0\n", line);	\
    last_function_decl = current_function_decl;		\
    sym_lineno += 1; }

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */
#define DBX_FUNCTION_FIRST

/* Only labels should ever begin in column zero.  */
#define ASM_STABS_OP "\t.stabs"
#define ASM_STABN_OP "\t.stabn"

/* GDB always assumes the current function's frame begins at the value
   of the stack pointer upon entry to the current function.  Accessing
   local variables and parameters passed on the stack is done using the
   base of the frame + an offset provided by GCC.

   For functions which have frame pointers this method works fine;
   the (frame pointer) == (stack pointer at function entry) and GCC provides
   an offset relative to the frame pointer.

   This loses for functions without a frame pointer; GCC provides an offset
   which is relative to the stack pointer after adjusting for the function's
   frame size.  GDB would prefer the offset to be relative to the value of
   the stack pointer at the function's entry.  Yuk!  */
#define DEBUGGER_AUTO_OFFSET(X) \
  ((GET_CODE (X) == PLUS ? INTVAL (XEXP (X, 1)) : 0) \
    + (frame_pointer_needed ? 0 : compute_frame_size (get_frame_size (), 0)))

#define DEBUGGER_ARG_OFFSET(OFFSET, X) \
  ((GET_CODE (X) == PLUS ? OFFSET : 0) \
    + (frame_pointer_needed ? 0 : compute_frame_size (get_frame_size (), 0)))

/* gdb needs a null N_SO at the end of each file for scattered loading. */

#undef	DBX_OUTPUT_MAIN_SOURCE_FILE_END
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME) \
  text_section (); \
  if (!TARGET_PORTABLE_RUNTIME) \
    fputs ("\t.SPACE $TEXT$\n\t.NSUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n", FILE); \
  else \
    fprintf (FILE, "%s\n", TEXT_SECTION_ASM_OP); \
  fprintf (FILE,							\
	   "\t.stabs \"\",%d,0,0,L$text_end0000\nL$text_end0000:\n", N_SO)

#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_PA_11) == 0
#define CPP_SPEC "%{msnake:-D__hp9000s700 -D_PA_RISC1_1}\
 %{mpa-risc-1-1:-D__hp9000s700 -D_PA_RISC1_1}\
 %{!ansi: -D_HPUX_SOURCE -D_HIUX_SOURCE -D__STDC_EXT__}\
 %{threads:-D_REENTRANT -D_DCE_THREADS}"
#else
#define CPP_SPEC "%{!mpa-risc-1-0:%{!mnosnake:%{!msoft-float:-D__hp9000s700 -D_PA_RISC1_1}}} \
 %{!ansi: -D_HPUX_SOURCE -D_HIUX_SOURCE -D__STDC_EXT__}\
 %{threads:-D_REENTRANT -D_DCE_THREADS}"
#endif

/* Defines for a K&R CC */

#define CC1_SPEC "%{pg:} %{p:}"

#define LINK_SPEC "%{mlinker-opt:-O} %{!shared:-u main} %{shared:-b}"

/* We don't want -lg.  */
#ifndef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"
#endif

/* Make gcc agree with <machine/ansi.h> */

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE 32

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* Machine dependent reorg pass.  */
#define MACHINE_DEPENDENT_REORG(X) pa_reorg(X)

/* Prototype function used in MACHINE_DEPENDENT_REORG macro. */
void pa_reorg ();

/* Prototype function used in various macros. */
int symbolic_operand ();

/* Used in insn-*.c. */
int following_call ();
int function_label_operand ();
int lhs_lshift_cint_operand ();

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dhppa -Dhp9000s800 -D__hp9000s800 -Dhp9k8 -Dunix -Dhp9000 -Dhp800 -Dspectrum -DREVARGV -Asystem(unix) -Asystem(bsd) -Acpu(hppa) -Amachine(hppa)"

/* HPUX has a program 'chatr' to list the dependencies of dynamically
   linked executables and shared libraries.  */
#define LDD_SUFFIX "chatr"
/* Look for lines like "dynamic   /usr/lib/X11R5/libX11.sl"
   or "static    /usr/lib/X11R5/libX11.sl". 

   HPUX 10.20 also has lines like "static branch prediction ..."
   so we filter that out explicitly.

   We also try to bound our search for libraries with marker
   lines.  What a pain.  */
#define PARSE_LDD_OUTPUT(PTR)					\
do {								\
  static int in_shlib_list = 0;					\
  while (*PTR == ' ') PTR++;					\
  if (strncmp (PTR, "shared library list:",			\
	       sizeof ("shared library list:") - 1) == 0)	\
    {								\
      PTR = 0;							\
      in_shlib_list = 1;					\
    }								\
  else if (strncmp (PTR, "shared library binding:",		\
		    sizeof ("shared library binding:") - 1) == 0)\
    {								\
      PTR = 0;							\
      in_shlib_list = 0;					\
    }								\
  else if (strncmp (PTR, "static branch prediction disabled",	\
		    sizeof ("static branch prediction disabled") - 1) == 0)\
    {								\
      PTR = 0;							\
      in_shlib_list = 0;					\
    }								\
  else if (in_shlib_list					\
	   &&  strncmp (PTR, "dynamic", sizeof ("dynamic") - 1) == 0) \
    {								\
      PTR += sizeof ("dynamic") - 1;				\
      while (*p == ' ') PTR++;					\
    }								\
  else if (in_shlib_list					\
	   && strncmp (PTR, "static", sizeof ("static") - 1) == 0) \
    {								\
      PTR += sizeof ("static") - 1;				\
      while (*p == ' ') PTR++;					\
    }								\
  else								\
    PTR = 0;							\
} while (0)

/* target machine storage layout */

/* Define for cross-compilation from a host with a different float format
   or endianness (e.g. VAX, x86).  */
#define REAL_ARITHMETIC

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT	\
      && GET_MODE_SIZE (MODE) < 4)  	\
    (MODE) = SImode;

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the HP-PA.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Largest alignment required for any stack parameter, in bits.
   Don't define this if it is equal to PARM_BOUNDARY */
#define MAX_PARM_BOUNDARY 64

/* Boundary (in *bits*) on which stack pointer is always aligned;
   certain optimizations in combine depend on this.

   GCC for the PA always rounds its stacks to a 512bit boundary,
   but that happens late in the compilation process.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* The .align directive in the HP assembler allows up to a 32 alignment.  */
#define MAX_OFILE_ALIGNMENT 32768

/* Get around hp-ux assembler bug, and make strcpy of constants fast. */
#define CONSTANT_ALIGNMENT(CODE, TYPEALIGN) \
  ((TYPEALIGN) < 32 ? 32 : (TYPEALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))


/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   HP-PA 1.0 has 32 fullword registers and 16 floating point
   registers. The floating point registers hold either word or double
   word values.

   16 additional registers are reserved.

   HP-PA 1.1 has 32 fullword registers and 32 floating point
   registers. However, the floating point registers behave
   differently: the left and right halves of registers are addressable
   as 32 bit registers. So, we will set things up like the 68k which
   has different fp units: define separate register sets for the 1.0
   and 1.1 fp units. */

#define FIRST_PSEUDO_REGISTER 89  /* 32 general regs + 56 fp regs +
				     + 1 shift reg */

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the HP-PA, these are:
   Reg 0	= 0 (hardware). However, 0 is used for condition code,
                  so is not fixed.
   Reg 1	= ADDIL target/Temporary (hardware).
   Reg 2	= Return Pointer
   Reg 3	= Frame Pointer
   Reg 4	= Frame Pointer (>8k varying frame with HP compilers only)
   Reg 4-18	= Preserved Registers
   Reg 19	= Linkage Table Register in HPUX 8.0 shared library scheme.
   Reg 20-22	= Temporary Registers
   Reg 23-26	= Temporary/Parameter Registers
   Reg 27	= Global Data Pointer (hp)
   Reg 28	= Temporary/???/Return Value register
   Reg 29	= Temporary/Static Chain/Return Value register #2
   Reg 30	= stack pointer
   Reg 31	= Temporary/Millicode Return Pointer (hp)

   Freg 0-3	= Status Registers	 -- Not known to the compiler.
   Freg 4-7	= Arguments/Return Value
   Freg 8-11	= Temporary Registers
   Freg 12-15	= Preserved Registers

   Freg 16-31	= Reserved

   On the Snake, fp regs are

   Freg 0-3	= Status Registers	-- Not known to the compiler.
   Freg 4L-7R	= Arguments/Return Value
   Freg 8L-11R	= Temporary Registers
   Freg 12L-21R	= Preserved Registers
   Freg 22L-31R = Temporary Registers

*/

#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 1, 0, 0, 1, 0, \
  /* fp registers */	  \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, \
  /* fp registers */	  \
  1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, \
  1}

#define CONDITIONAL_REGISTER_USAGE \
{						\
  if (!TARGET_PA_11)				\
    {						\
      for (i = 56; i < 88; i++) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
      for (i = 33; i < 88; i += 2) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    }						\
  if (TARGET_DISABLE_FPREGS || TARGET_SOFT_FLOAT)\
    {						\
      for (i = 32; i < 88; i++) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    }						\
  if (flag_pic)					\
    {						\
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
      fixed_regs[PIC_OFFSET_TABLE_REGNUM_SAVED] = 1;\
    }						\
}

/* Allocate the call used registers first.  This should minimize
   the number of registers that need to be saved (as call used
   registers will generally not be allocated across a call).

   Experimentation has shown slightly better results by allocating
   FP registers first.  

   FP registers are ordered so that all L registers are selected before
   R registers.  This works around a false dependency interlock on the
   PA8000 when accessing the high and low parts of an FP register
   independently.  */

#define REG_ALLOC_ORDER \
 {					\
  /* caller-saved fp regs.  */		\
  68, 70, 72, 74, 76, 78, 80, 82,	\
  84, 86, 40, 42, 44, 46, 32, 34,	\
  36, 38,				\
  69, 71, 73, 75, 77, 79, 81, 83,	\
  85, 87, 41, 43, 45, 47, 33, 35,	\
  37, 39,				\
  /* caller-saved general regs.  */	\
  19, 20, 21, 22, 23, 24, 25, 26,	\
  27, 28, 29, 31,  2,			\
  /* callee-saved fp regs.  */		\
  48, 50, 52, 54, 56, 58, 60, 62,	\
  64, 66,				\
  49, 51, 53, 55, 57, 59, 61, 63,	\
  65, 67,				\
  /* callee-saved general regs.  */	\
   3,  4,  5,  6,  7,  8,  9, 10, 	\
  11, 12, 13, 14, 15, 16, 17, 18,	\
  /* special registers.  */		\
   1, 30,  0, 88}


/* True if register is floating-point.  */
#define FP_REGNO_P(N) ((N) >= 32 && (N) <= 87)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the HP-PA, ordinary registers hold 32 bits worth;
   The floating point registers are 64 bits wide. Snake fp regs are 32
   bits wide */
#define HARD_REGNO_NREGS(REGNO, MODE)					\
  (!TARGET_PA_11 && FP_REGNO_P (REGNO) ? 1				\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the HP-PA, the cpu registers can hold any mode.  We
   force this to be an even register is it cannot hold the full mode.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((REGNO) == 0 ? (MODE) == CCmode || (MODE) == CCFPmode		\
   /* On 1.0 machines, don't allow wide non-fp modes in fp regs. */	\
   : !TARGET_PA_11 && FP_REGNO_P (REGNO)				\
     ? GET_MODE_SIZE (MODE) <= 4 || GET_MODE_CLASS (MODE) == MODE_FLOAT	\
   /* Make wide modes be in aligned registers. */			\
   : GET_MODE_SIZE (MODE) <= 4 || ((REGNO) & 1) == 0)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* The HP-PA pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 30

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 3

/* Value should be nonzero if functions must have frame pointers.  */
#define FRAME_POINTER_REQUIRED \
  (current_function_calls_alloca)

/* C statement to store the difference between the frame pointer
   and the stack pointer values immediately after the function prologue.

   Note, we always pretend that this is a leaf function because if
   it's not, there's no point in trying to eliminate the
   frame pointer.  If it is a leaf function, we guessed right!  */
#define INITIAL_FRAME_POINTER_OFFSET(VAR) \
  do {(VAR) = - compute_frame_size (get_frame_size (), 0);} while (0)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 3

/* Register in which static-chain is passed to a function.  */
/* ??? */
#define STATIC_CHAIN_REGNUM 29

/* Register which holds offset table for position-independent
   data references.  */

#define PIC_OFFSET_TABLE_REGNUM 19
#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED 1

/* Register into which we save the PIC_OFFEST_TABLE_REGNUM so that it
   can be restore across function calls.  */
#define PIC_OFFSET_TABLE_REGNUM_SAVED 4

/* SOM ABI says that objects larger than 64 bits are returned in memory.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
#define RETURN_IN_MEMORY(TYPE)	\
  (int_size_in_bytes (TYPE) > 8)

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 28

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

  /* The HP-PA has four kinds of registers: general regs, 1.0 fp regs,
     1.1 fp regs, and the high 1.1 fp regs, to which the operands of
     fmpyadd and fmpysub are restricted.  */

enum reg_class { NO_REGS, R1_REGS, GENERAL_REGS, FPUPPER_REGS, FP_REGS, GENERAL_OR_FP_REGS,
  SHIFT_REGS, ALL_REGS, LIM_REG_CLASSES};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
  {"NO_REGS", "R1_REGS", "GENERAL_REGS", "FPUPPER_REGS", "FP_REGS", \
   "GENERAL_OR_FP_REGS", "SHIFT_REGS", "ALL_REGS"}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES. Register 0, the "condition code" register,
   is in no class. */

#define REG_CLASS_CONTENTS	\
 {{0x00000000, 0x00000000, 0x00000000},	/* NO_REGS */			\
  {0x00000002, 0x00000000, 0x00000000},	/* R1_REGS */			\
  {0xfffffffe, 0x00000000, 0x00000000},	/* GENERAL_REGS */		\
  {0x00000000, 0xff000000, 0x00ffffff},	/* FPUPPER_REGS */			\
  {0x00000000, 0xffffffff, 0x00ffffff},	/* FP_REGS */			\
  {0xfffffffe, 0xffffffff, 0x00ffffff},	/* GENERAL_OR_FP_REGS */	\
  {0x00000000, 0x00000000, 0x01000000},	/* SHIFT_REGS */		\
  {0xfffffffe, 0xffffffff, 0x01ffffff}}	/* ALL_REGS */

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)						\
  ((REGNO) == 0 ? NO_REGS 						\
   : (REGNO) == 1 ? R1_REGS						\
   : (REGNO) < 32 ? GENERAL_REGS					\
   : (REGNO) < 56 ? FP_REGS						\
   : (REGNO) < 88 ? FPUPPER_REGS						\
   : SHIFT_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

#define FP_REG_CLASS_P(CLASS) \
  ((CLASS) == FP_REGS || (CLASS) == FPUPPER_REGS)

/* Get reg_class from a letter such as appears in the machine description.  */
/* Keep 'x' for backward compatibility with user asm.   */
#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FP_REGS :					\
   (C) == 'y' ? FPUPPER_REGS :					\
   (C) == 'x' ? FP_REGS :					\
   (C) == 'q' ? SHIFT_REGS :					\
   (C) == 'a' ? R1_REGS :					\
   (C) == 'Z' ? ALL_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is used for the 11 bit constants.
   `J' is used for the 14 bit constants.
   `K' is used for values that can be moved with a zdepi insn.
   `L' is used for the 5 bit constants.
   `M' is used for 0.
   `N' is used for values with the least significant 11 bits equal to zero.
   `O' is used for numbers n such that n+1 is a power of 2.
   */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? VAL_11_BITS_P (VALUE)				\
   : (C) == 'J' ? VAL_14_BITS_P (VALUE)				\
   : (C) == 'K' ? zdepi_cint_p (VALUE)				\
   : (C) == 'L' ? VAL_5_BITS_P (VALUE)				\
   : (C) == 'M' ? (VALUE) == 0					\
   : (C) == 'N' ? ((VALUE) & 0x7ff) == 0			\
   : (C) == 'O' ? (((VALUE) & ((VALUE) + 1)) == 0)		\
   : (C) == 'P' ? and_mask_p (VALUE)				\
   : 0)

/* Prototype function used in macro CONST_OK_FOR_LETTER_P. */
int zdepi_cint_p ();

/* Similar, but for floating or large integer constants, and defining letters
   G and H.   Here VALUE is the CONST_DOUBLE rtx itself.

   For PA, `G' is the floating-point constant zero.  `H' is undefined.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  			\
  ((C) == 'G' ? (GET_MODE_CLASS (GET_MODE (VALUE)) == MODE_FLOAT	\
		 && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))		\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly
   NO_REGS is returned. 

  Avoid doing any work for the common case calls.  */

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  ((CLASS == BASE_REG_CLASS && GET_CODE (IN) == REG		\
    && REGNO (IN) < FIRST_PSEUDO_REGISTER)			\
   ? NO_REGS : secondary_reload_class (CLASS, MODE, IN))

/* On the PA it is not possible to directly move data between
   GENERAL_REGS and FP_REGS.  */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE)  \
  (FP_REG_CLASS_P (CLASS1) != FP_REG_CLASS_P (CLASS2))

/* Return the stack location to use for secondary memory needed reloads.  */
#define SECONDARY_MEMORY_NEEDED_RTX(MODE) \
  gen_rtx_MEM (MODE, gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (-16)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE)					\
  (!TARGET_PA_11 && ((CLASS) == FP_REGS || (CLASS) == FPUPPER_REGS) ? 1 :				\
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

/* Believe it or not.  */
#define ARGS_GROW_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 8

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the HP-PA, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   This value will be negated because the arguments grow down.
   Also note that on STACK_GROWS_UPWARD machines (such as this one)
   this is the distance from the frame pointer to the end of the first
   argument, not it's beginning.  To get the real offset of the first
   argument, the size of the argument must be added.

   ??? Have to check on this.*/

#define FIRST_PARM_OFFSET(FNDECL) -32

/* Absolute value of offset from top-of-stack address to location to store the
   function parameter if it can't go in a register.
   Addresses for following parameters are computed relative to this one.  */
#define FIRST_PARM_CALLER_OFFSET(FNDECL) -32


/* When a parameter is passed in a register, stack space is still
   allocated for it.  */
#define REG_PARM_STACK_SPACE(DECL) 16

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE

/* Keep the stack pointer constant throughout the function.
   This is both an optimization and a necessity: longjmp
   doesn't behave itself when the stack pointer moves within
   the function!  */
#define ACCUMULATE_OUTGOING_ARGS

/* The weird HPPA calling conventions require a minimum of 48 bytes on
   the stack: 16 bytes for register saves, and 32 bytes for magic.
   This is the difference between the logical top of stack and the
   actual sp. */
#define STACK_POINTER_OFFSET -32

#define STACK_DYNAMIC_OFFSET(FNDECL)	\
  ((STACK_POINTER_OFFSET) - current_function_outgoing_args_size)

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the HP-PA the value is found in register(s) 28(-29), unless
   the mode is SF or DF. Then the value is returned in fr4 (32, ) */


#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), ((! TARGET_SOFT_FLOAT		     \
				      && (TYPE_MODE (VALTYPE) == SFmode ||  \
					  TYPE_MODE (VALTYPE) == DFmode)) ? \
				     32 : 28))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)	\
  gen_rtx_REG (MODE,							\
	       (! TARGET_SOFT_FLOAT					\
	        && ((MODE) == SFmode || (MODE) == DFmode) ? 32 : 28))

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 28 || (! TARGET_SOFT_FLOAT && (N) == 32))

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) \
  (((N) >= 23 && (N) <= 26) || (! TARGET_SOFT_FLOAT && (N) >= 32 && (N) <= 39))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the HP-PA, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 4 or more means all following args should go on the stack.  */

struct hppa_args {int words, nargs_prototype, indirect; };

#define CUMULATIVE_ARGS struct hppa_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT) \
  (CUM).words = 0, 							\
  (CUM).indirect = INDIRECT,						\
  (CUM).nargs_prototype = (FNTYPE && TYPE_ARG_TYPES (FNTYPE)		\
			   ? (list_length (TYPE_ARG_TYPES (FNTYPE)) - 1	\
			      + (TYPE_MODE (TREE_TYPE (FNTYPE)) == BLKmode \
				 || RETURN_IN_MEMORY (TREE_TYPE (FNTYPE)))) \
			   : 0)



/* Similar, but when scanning the definition of a procedure.  We always
   set NARGS_PROTOTYPE large so we never return a PARALLEL.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM,FNTYPE,IGNORE) \
  (CUM).words = 0,				\
  (CUM).indirect = 0,				\
  (CUM).nargs_prototype = 1000

/* Figure out the size in words of the function argument. */

#define FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((((MODE) != BLKmode ? GET_MODE_SIZE (MODE) : int_size_in_bytes (TYPE))+3)/4)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
{ (CUM).nargs_prototype--;						\
  ((((CUM).words & 01) && (TYPE) != 0					\
    && FUNCTION_ARG_SIZE(MODE, TYPE) > 1)				\
   && (CUM).words++),							\
     (CUM).words += FUNCTION_ARG_SIZE(MODE, TYPE);			\
}

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

   On the HP-PA the first four words of args are normally in registers
   and the rest are pushed.  But any arg that won't entirely fit in regs
   is pushed.

   Arguments passed in registers are either 1 or 2 words long.

   The caller must make a distinction between calls to explicitly named
   functions and calls through pointers to functions -- the conventions
   are different!  Calls through pointers to functions only use general
   registers for the first four argument words.

   Of course all this is different for the portable runtime model
   HP wants everyone to use for ELF.  Ugh.  Here's a quick description
   of how it's supposed to work.

   1) callee side remains unchanged.  It expects integer args to be
   in the integer registers, float args in the float registers and
   unnamed args in integer registers.

   2) caller side now depends on if the function being called has
   a prototype in scope (rather than if it's being called indirectly).

      2a) If there is a prototype in scope, then arguments are passed
      according to their type (ints in integer registers, floats in float
      registers, unnamed args in integer registers.

      2b) If there is no prototype in scope, then floating point arguments
      are passed in both integer and float registers.  egad.

  FYI: The portable parameter passing conventions are almost exactly like
  the standard parameter passing conventions on the RS6000.  That's why
  you'll see lots of similar code in rs6000.h.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE) function_arg_padding ((MODE), (TYPE))

/* Do not expect to understand this without reading it several times.  I'm
   tempted to try and simply it, but I worry about breaking something.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)		      		\
  (4 >= ((CUM).words + FUNCTION_ARG_SIZE ((MODE), (TYPE)))		\
   ? (!TARGET_PORTABLE_RUNTIME || (TYPE) == 0				\
      || !FLOAT_MODE_P (MODE) || TARGET_SOFT_FLOAT			\
      || (CUM).nargs_prototype > 0)					\
      ? gen_rtx_REG ((MODE),						\
		 (FUNCTION_ARG_SIZE ((MODE), (TYPE)) > 1		\
		  ? (((!(CUM).indirect 					\
		       || TARGET_PORTABLE_RUNTIME)			\
		      && (MODE) == DFmode				\
		      && ! TARGET_SOFT_FLOAT)				\
		     ? ((CUM).words ? 38 : 34)				\
		     : ((CUM).words ? 23 : 25))				\
		  : (((!(CUM).indirect					\
		       || TARGET_PORTABLE_RUNTIME)			\
		      && (MODE) == SFmode				\
		      && ! TARGET_SOFT_FLOAT)				\
		     ? (32 + 2 * (CUM).words)				\
		     : (27 - (CUM).words - FUNCTION_ARG_SIZE ((MODE),	\
							      (TYPE))))))\
   /* We are calling a non-prototyped function with floating point	\
      arguments using the portable conventions.  */			\
   : gen_rtx_PARALLEL ((MODE),						\
	      gen_rtvec							\
	      (2,							\
	       gen_rtx_EXPR_LIST (VOIDmode,				\
			gen_rtx_REG ((MODE),				\
				 (FUNCTION_ARG_SIZE ((MODE), (TYPE)) > 1 \
				  ? ((CUM).words ? 38 : 34)		\
				  : (32 + 2 * (CUM).words))),		\
			const0_rtx),					\
	       gen_rtx_EXPR_LIST (VOIDmode,				\
			gen_rtx_REG ((MODE),				\
				 (FUNCTION_ARG_SIZE ((MODE), (TYPE)) > 1 \
				  ? ((CUM).words ? 23 : 25)		\
				  : (27 - (CUM).words -			\
				     FUNCTION_ARG_SIZE ((MODE),		\
							(TYPE))))),	\
			const0_rtx)))					\
  /* Pass this parameter in the stack.  */				\
  : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* If defined, a C expression that gives the alignment boundary, in
   bits, of an argument with the specified mode and type.  If it is
   not defined,  `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)				\
  (((TYPE) != 0)							\
	? (((int_size_in_bytes (TYPE)) + 3) / 4) * BITS_PER_WORD	\
	: ((GET_MODE_ALIGNMENT(MODE) <= PARM_BOUNDARY)			\
		? PARM_BOUNDARY						\
		: GET_MODE_ALIGNMENT(MODE)))

/* Arguments larger than eight bytes are passed by invisible reference */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  ((TYPE) && int_size_in_bytes (TYPE) > 8)
 
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) \
  ((TYPE) && int_size_in_bytes (TYPE) > 8)


extern struct rtx_def *hppa_compare_op0, *hppa_compare_op1;
extern enum cmp_type hppa_branch_type;

/* Output the label for a function definition.  */
#ifndef HP_FP_ARG_DESCRIPTOR_REVERSED
#define ASM_DOUBLE_ARG_DESCRIPTORS(FILE, ARG0, ARG1)	\
  do { fprintf (FILE, ",ARGW%d=FR", (ARG0));		\
       fprintf (FILE, ",ARGW%d=FU", (ARG1));} while (0)
#define DFMODE_RETURN_STRING ",RTNVAL=FU"
#define SFMODE_RETURN_STRING ",RTNVAL=FR"
#else
#define ASM_DOUBLE_ARG_DESCRIPTORS(FILE, ARG0, ARG1)	\
  do { fprintf (FILE, ",ARGW%d=FU", (ARG0));		\
       fprintf (FILE, ",ARGW%d=FR", (ARG1));} while (0)
#define DFMODE_RETURN_STRING ",RTNVAL=FR"
#define SFMODE_RETURN_STRING ",RTNVAL=FU"
#endif

#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION) \
{ char *target_name = XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0); \
  STRIP_NAME_ENCODING (target_name, target_name); \
  output_function_prologue (FILE, 0); \
  if (VAL_14_BITS_P (DELTA)) \
    fprintf (FILE, "\tb %s\n\tldo %d(%%r26),%%r26\n", target_name, DELTA); \
  else \
    fprintf (FILE, "\taddil L%%%d,%%r26\n\tb %s\n\tldo R%%%d(%%r1),%%r26\n", \
	     DELTA, target_name, DELTA); \
  fprintf (FILE, "\n\t.EXIT\n\t.PROCEND\n"); \
}

/* NAME refers to the function's name.  If we are placing each function into
   its own section, we need to switch to the section for this function.  Note
   that the section name will have a "." prefix.  */
#define ASM_OUTPUT_FUNCTION_PREFIX(FILE, NAME) \
  {									\
    char *name;								\
    STRIP_NAME_ENCODING (name, NAME);					\
    if (!TARGET_PORTABLE_RUNTIME && TARGET_GAS && in_section == in_text) \
      fputs ("\t.NSUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n", FILE); \
    else if (! TARGET_PORTABLE_RUNTIME && TARGET_GAS)			\
      fprintf (FILE,							\
	       "\t.SUBSPA .%s\n", name);				\
  }
    
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
    do { tree fntype = TREE_TYPE (TREE_TYPE (DECL));			\
	 tree tree_type = TREE_TYPE (DECL);				\
	 tree parm;							\
	 int i;								\
	 if (TREE_PUBLIC (DECL) || TARGET_GAS)				\
	   { extern int current_function_varargs;			\
	     if (TREE_PUBLIC (DECL))					\
	       {							\
		 fputs ("\t.EXPORT ", FILE);				\
		 assemble_name (FILE, NAME);				\
		 fputs (",ENTRY,PRIV_LEV=3", FILE);			\
	       }							\
	     else							\
	       {							\
		 fputs ("\t.PARAM ", FILE);				\
		 assemble_name (FILE, NAME);				\
	       }							\
	     if (TARGET_PORTABLE_RUNTIME)				\
	       {							\
		 fputs (",ARGW0=NO,ARGW1=NO,ARGW2=NO,ARGW3=NO,", FILE);	\
		 fputs ("RTNVAL=NO\n", FILE);				\
		 break;							\
	       }							\
	     for (parm = DECL_ARGUMENTS (DECL), i = 0; parm && i < 4;	\
		  parm = TREE_CHAIN (parm))				\
	       {							\
		 if (TYPE_MODE (DECL_ARG_TYPE (parm)) == SFmode		\
		     && ! TARGET_SOFT_FLOAT)				\
		   fprintf (FILE, ",ARGW%d=FR", i++);			\
		 else if (TYPE_MODE (DECL_ARG_TYPE (parm)) == DFmode	\
			  && ! TARGET_SOFT_FLOAT)			\
		   {							\
		     if (i <= 2)					\
		       {						\
			 if (i == 1) i++;				\
			 ASM_DOUBLE_ARG_DESCRIPTORS (FILE, i++, i++);	\
		       }						\
		     else						\
		       break;						\
		   }							\
		 else							\
		   {							\
		     int arg_size =					\
		       FUNCTION_ARG_SIZE (TYPE_MODE (DECL_ARG_TYPE (parm)),\
					  DECL_ARG_TYPE (parm));	\
		     /* Passing structs by invisible reference uses	\
			one general register.  */			\
		     if (arg_size > 2					\
			 || TREE_ADDRESSABLE (DECL_ARG_TYPE (parm)))	\
		       arg_size = 1;					\
		     if (arg_size == 2 && i <= 2)			\
		       {						\
			 if (i == 1) i++;				\
			 fprintf (FILE, ",ARGW%d=GR", i++);		\
			 fprintf (FILE, ",ARGW%d=GR", i++);		\
		       }						\
		     else if (arg_size == 1)				\
		       fprintf (FILE, ",ARGW%d=GR", i++);		\
		     else						\
		       i += arg_size;					\
		   }							\
	       }							\
	     /* anonymous args */					\
	     if ((TYPE_ARG_TYPES (tree_type) != 0			\
		  && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (tree_type)))\
		      != void_type_node))				\
		 || current_function_varargs)				\
	       {							\
		 for (; i < 4; i++)					\
		   fprintf (FILE, ",ARGW%d=GR", i);			\
	       }							\
	     if (TYPE_MODE (fntype) == DFmode && ! TARGET_SOFT_FLOAT)	\
	       fputs (DFMODE_RETURN_STRING, FILE);			\
	     else if (TYPE_MODE (fntype) == SFmode && ! TARGET_SOFT_FLOAT) \
	       fputs (SFMODE_RETURN_STRING, FILE);			\
	     else if (fntype != void_type_node)				\
	       fputs (",RTNVAL=GR", FILE);				\
	     fputs ("\n", FILE);					\
	   }} while (0)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

/* On HP-PA, move-double insns between fpu and cpu need an 8-byte block
   of memory.  If any fpu reg is used in the function, we allocate
   such a block here, at the bottom of the frame, just in case it's needed.

   If this function is a leaf procedure, then we may choose not
   to do a "save" insn.  The decision about whether or not
   to do this is made in regclass.c.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) \
  output_function_prologue (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.

   Because HPUX _mcount is so different, we actually emit the
   profiling code in function_prologue. This just stores LABELNO for
   that. */

#define PROFILE_BEFORE_PROLOGUE
#define FUNCTION_PROFILER(FILE, LABELNO) \
{ extern int hp_profile_labelno; hp_profile_labelno = (LABELNO);}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int may_call_alloca;
extern int current_function_pretend_args_size;

#define EXIT_IGNORE_STACK	\
 (get_frame_size () != 0	\
  || current_function_calls_alloca || current_function_outgoing_args_size)


/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

/* This declaration is needed due to traditional/ANSI
   incompatibilities which cannot be #ifdefed away
   because they occur inside of macros.  Sigh.  */
extern union tree_node *current_function_decl;

#define FUNCTION_EPILOGUE(FILE, SIZE)			\
  output_function_epilogue (FILE, SIZE)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.\

   The trampoline sets the static chain pointer to STATIC_CHAIN_REGNUM
   and then branches to the specified routine.

   This code template is copied from text segment to stack location
   and then patched with INITIALIZE_TRAMPOLINE to contain
   valid values, and then entered as a subroutine.

   It is best to keep this as small as possible to avoid having to
   flush multiple lines in the cache.  */

#define TRAMPOLINE_TEMPLATE(FILE) \
  {							\
    fputs ("\tldw	36(%r22),%r21\n", FILE);	\
    fputs ("\tbb,>=,n	%r21,30,.+16\n", FILE);	\
    fputs ("\tdepi	0,31,2,%r21\n", FILE);		\
    fputs ("\tldw	4(%r21),%r19\n", FILE);	\
    fputs ("\tldw	0(%r21),%r21\n", FILE);	\
    fputs ("\tldsid	(%r21),%r1\n", FILE);	\
    fputs ("\tmtsp	%r1,%sr0\n", FILE);		\
    fputs ("\tbe	0(%sr0,%r21)\n", FILE);	\
    fputs ("\tldw	40(%r22),%r29\n", FILE);	\
    fputs ("\t.word	0\n", FILE);			\
    fputs ("\t.word	0\n", FILE);			\
  }

/* Length in units of the trampoline for entering a nested function.

   Flush the cache entries corresponding to the first and last addresses
   of the trampoline.  This is necessary as the trampoline may cross two
   cache lines.

   If the code part of the trampoline ever grows to > 32 bytes, then it
   will become necessary to hack on the cacheflush pattern in pa.md.  */

#define TRAMPOLINE_SIZE (11 * 4)

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   Move the function address to the trampoline template at offset 12.
   Move the static chain value to trampoline template at offset 16.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
{									\
  rtx start_addr, end_addr;						\
									\
  start_addr = memory_address (Pmode, plus_constant ((TRAMP), 36));	\
  emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (FNADDR));		\
  start_addr = memory_address (Pmode, plus_constant ((TRAMP), 40));	\
  emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (CXT));		\
  /* fdc and fic only use registers for the address to flush,		\
     they do not accept integer displacements.  */ 			\
  start_addr = force_reg (SImode, (TRAMP));				\
  end_addr = force_reg (SImode, plus_constant ((TRAMP), 32));		\
  emit_insn (gen_dcacheflush (start_addr, end_addr));			\
  end_addr = force_reg (SImode, plus_constant (start_addr, 32));	\
  emit_insn (gen_icacheflush (start_addr, end_addr, start_addr,		\
			      gen_reg_rtx (SImode), gen_reg_rtx (SImode)));\
}

/* Emit code for a call to builtin_saveregs.  We must emit USE insns which
   reference the 4 integer arg registers and 4 fp arg registers.
   Ordinarily they are not call used registers, but they are for
   _builtin_saveregs, so we must make this explicit.  */

extern struct rtx_def *hppa_builtin_saveregs ();
#define EXPAND_BUILTIN_SAVEREGS(ARGLIST) hppa_builtin_saveregs (ARGLIST)


/* Addressing modes, and classification of registers for them. 

   Using autoincrement addressing modes on PA8000 class machines is
   not profitable.  */

#define HAVE_POST_INCREMENT (pa_cpu < PROCESSOR_8000)
#define HAVE_POST_DECREMENT (pa_cpu < PROCESSOR_8000)

#define HAVE_PRE_DECREMENT (pa_cpu < PROCESSOR_8000)
#define HAVE_PRE_INCREMENT (pa_cpu < PROCESSOR_8000)

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) && ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32))
#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) && ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32))
#define REGNO_OK_FOR_FP_P(REGNO) \
  (FP_REGNO_P (REGNO) || FP_REGNO_P (reg_renumber[REGNO]))

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the HP-PA, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address except
   for symbolic addresses.  We get better CSE by rejecting them
   here and allowing hppa_legitimize_address to break them up.  We
   use most of the constants accepted by CONSTANT_P, except CONST_DOUBLE.  */

#define CONSTANT_ADDRESS_P(X) \
  ((GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH) 						\
   && (reload_in_progress || reload_completed || ! symbolic_expression_p (X)))

/* Include all constant integers and constant doubles, but not
   floating-point, except for floating-point zero.

   Reject LABEL_REFs if we're not using gas or the new HP assembler.  */
#ifdef NEW_HP_ASSEMBLER
#define LEGITIMATE_CONSTANT_P(X)  		\
  ((GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT	\
    || (X) == CONST0_RTX (GET_MODE (X)))	\
   && !function_label_operand (X, VOIDmode))
#else
#define LEGITIMATE_CONSTANT_P(X)  		\
  ((GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT	\
    || (X) == CONST0_RTX (GET_MODE (X)))	\
   && (GET_CODE (X) != LABEL_REF || TARGET_GAS)\
   && !function_label_operand (X, VOIDmode))
#endif

/* Subroutine for EXTRA_CONSTRAINT.

   Return 1 iff OP is a pseudo which did not get a hard register and
   we are running the reload pass.  */

#define IS_RELOADING_PSEUDO_P(OP) \
  ((reload_in_progress					\
    && GET_CODE (OP) == REG				\
    && REGNO (OP) >= FIRST_PSEUDO_REGISTER		\
    && reg_renumber [REGNO (OP)] < 0))

/* Optional extra constraints for this machine. Borrowed from sparc.h.

   For the HPPA, `Q' means that this is a memory operand but not a
   symbolic memory operand.  Note that an unassigned pseudo register
   is such a memory operand.  Needed because reload will generate
   these things in insns and then not re-recognize the insns, causing
   constrain_operands to fail.

   `R' is used for scaled indexed addresses.

   `S' is unused.

   `T' is for fp loads and stores.  */
#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q' ?						\
   (IS_RELOADING_PSEUDO_P (OP)				\
    || (GET_CODE (OP) == MEM				\
	&& (memory_address_p (GET_MODE (OP), XEXP (OP, 0))\
	    || reload_in_progress)			\
	&& ! symbolic_memory_operand (OP, VOIDmode)	\
        && !(GET_CODE (XEXP (OP, 0)) == PLUS		\
	     && (GET_CODE (XEXP (XEXP (OP, 0), 0)) == MULT\
		 || GET_CODE (XEXP (XEXP (OP, 0), 1)) == MULT))))\
   : ((C) == 'R' ?					\
     (GET_CODE (OP) == MEM				\
      && GET_CODE (XEXP (OP, 0)) == PLUS		\
      && (GET_CODE (XEXP (XEXP (OP, 0), 0)) == MULT	\
	  || GET_CODE (XEXP (XEXP (OP, 0), 1)) == MULT)	\
      && (move_operand (OP, GET_MODE (OP))		\
	  || memory_address_p (GET_MODE (OP), XEXP (OP, 0))\
	  || reload_in_progress))			\
   : ((C) == 'T' ? 					\
      (GET_CODE (OP) == MEM				\
       /* Using DFmode forces only short displacements	\
	  to be recognized as valid in reg+d addresses.  */\
       && memory_address_p (DFmode, XEXP (OP, 0))	\
       && !(GET_CODE (XEXP (OP, 0)) == PLUS		\
	    && (GET_CODE (XEXP (XEXP (OP, 0), 0)) == MULT\
		|| GET_CODE (XEXP (XEXP (OP, 0), 1)) == MULT))) : 0)))

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
#define REG_OK_FOR_INDEX_P(X) \
(REGNO (X) && (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER))
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
(REGNO (X) && (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER))

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

   On the HP-PA, the actual legitimate addresses must be
   REG+REG, REG+(REG*SCALE) or REG+SMALLINT.
   But we can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT. 

   Note we only allow 5 bit immediates for access to a constant address;
   doing so avoids losing for loading/storing a FP register at an address
   which will not fit in 5 bits.  */

#define VAL_5_BITS_P(X) ((unsigned)(X) + 0x10 < 0x20)
#define INT_5_BITS(X) VAL_5_BITS_P (INTVAL (X))

#define VAL_U5_BITS_P(X) ((unsigned)(X) < 0x20)
#define INT_U5_BITS(X) VAL_U5_BITS_P (INTVAL (X))

#define VAL_11_BITS_P(X) ((unsigned)(X) + 0x400 < 0x800)
#define INT_11_BITS(X) VAL_11_BITS_P (INTVAL (X))

#define VAL_14_BITS_P(X) ((unsigned)(X) + 0x2000 < 0x4000)
#define INT_14_BITS(X) VAL_14_BITS_P (INTVAL (X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{							\
  if ((REG_P (X) && REG_OK_FOR_BASE_P (X))		\
      || ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_DEC		\
	   || GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_INC)	\
	  && REG_P (XEXP (X, 0))			\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0))))		\
    goto ADDR;						\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      rtx base = 0, index = 0;				\
      if (flag_pic && XEXP (X, 0) == pic_offset_table_rtx)\
	{						\
	  if (GET_CODE (XEXP (X, 1)) == REG		\
	      && REG_OK_FOR_BASE_P (XEXP (X, 1)))	\
	    goto ADDR;					\
	  else if (flag_pic == 1			\
		   && GET_CODE (XEXP (X, 1)) == SYMBOL_REF)\
	    goto ADDR;					\
	}						\
      else if (REG_P (XEXP (X, 0))			\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
	base = XEXP (X, 0), index = XEXP (X, 1);	\
      else if (REG_P (XEXP (X, 1))			\
	       && REG_OK_FOR_BASE_P (XEXP (X, 1)))	\
	base = XEXP (X, 1), index = XEXP (X, 0);	\
      if (base != 0)					\
	if (GET_CODE (index) == CONST_INT		\
	    && ((INT_14_BITS (index)			\
		 && (TARGET_SOFT_FLOAT			\
		     || ((MODE) != SFmode && (MODE) != DFmode))) \
		|| INT_5_BITS (index)))			\
	  goto ADDR;					\
      if (! TARGET_SOFT_FLOAT				\
	  && ! TARGET_DISABLE_INDEXING			\
	  && base					\
	  && (mode == SFmode || mode == DFmode)		\
	  && GET_CODE (index) == MULT			\
	  && GET_CODE (XEXP (index, 0)) == REG		\
	  && REG_OK_FOR_BASE_P (XEXP (index, 0))	\
	  && GET_CODE (XEXP (index, 1)) == CONST_INT	\
	  && INTVAL (XEXP (index, 1)) == (mode == SFmode ? 4 : 8))\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == LO_SUM			\
	   && GET_CODE (XEXP (X, 0)) == REG		\
	   && REG_OK_FOR_BASE_P (XEXP (X, 0))		\
	   && CONSTANT_P (XEXP (X, 1))			\
	   && (TARGET_SOFT_FLOAT			\
	       || ((MODE) != SFmode			\
		   && (MODE) != DFmode)))		\
    goto ADDR;						\
  else if (GET_CODE (X) == LO_SUM			\
	   && GET_CODE (XEXP (X, 0)) == SUBREG		\
	   && GET_CODE (SUBREG_REG (XEXP (X, 0))) == REG\
	   && REG_OK_FOR_BASE_P (SUBREG_REG (XEXP (X, 0)))\
	   && CONSTANT_P (XEXP (X, 1))			\
	   && (TARGET_SOFT_FLOAT			\
	       || ((MODE) != SFmode			\
		   && (MODE) != DFmode)))		\
    goto ADDR;						\
  else if (GET_CODE (X) == LABEL_REF			\
	   || (GET_CODE (X) == CONST_INT		\
	       && INT_5_BITS (X)))			\
    goto ADDR;						\
  /* Needed for -fPIC */				\
  else if (GET_CODE (X) == LO_SUM			\
	   && GET_CODE (XEXP (X, 0)) == REG             \
	   && REG_OK_FOR_BASE_P (XEXP (X, 0))		\
	   && GET_CODE (XEXP (X, 1)) == UNSPEC)		\
    goto ADDR;						\
}

/* Look for machine dependent ways to make the invalid address AD a
   valid address.

   For the PA, transform:

        memory(X + <large int>)

   into:

        if (<large int> & mask) >= 16
          Y = (<large int> & ~mask) + mask + 1  Round up.
        else
          Y = (<large int> & ~mask)             Round down.
        Z = X + Y
        memory (Z + (<large int> - Y));

   This makes reload inheritance and reload_cse work better since Z
   can be reused.

   There may be more opportunities to improve code with this hook.  */
#define LEGITIMIZE_RELOAD_ADDRESS(AD, MODE, OPNUM, TYPE, IND, WIN) 	\
do { 									\
  int offset, newoffset, mask;						\
  rtx new, temp = NULL_RTX;						\
  mask = GET_MODE_CLASS (MODE) == MODE_FLOAT ? 0x1f : 0x3fff;		\
									\
  if (optimize								\
      && GET_CODE (AD) == PLUS)						\
    temp = simplify_binary_operation (PLUS, Pmode,			\
				      XEXP (AD, 0), XEXP (AD, 1));	\
									\
  new = temp ? temp : AD;						\
									\
  if (optimize								\
      && GET_CODE (new) == PLUS						\
      && GET_CODE (XEXP (new, 0)) == REG				\
      && GET_CODE (XEXP (new, 1)) == CONST_INT)				\
    {									\
      offset = INTVAL (XEXP ((new), 1));				\
									\
      /* Choose rounding direction.  Round up if we are >= halfway.  */	\
      if ((offset & mask) >= ((mask + 1) / 2))				\
	newoffset = (offset & ~mask) + mask + 1;			\
      else								\
	newoffset = offset & ~mask;					\
									\
      if (newoffset != 0						\
	  && VAL_14_BITS_P (newoffset))					\
	{								\
									\
	  temp = gen_rtx_PLUS (Pmode, XEXP (new, 0),			\
			       GEN_INT (newoffset));			\
	  AD = gen_rtx_PLUS (Pmode, temp, GEN_INT (offset - newoffset));\
	  push_reload (XEXP (AD, 0), 0, &XEXP (AD, 0), 0,		\
			     BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,	\
			     (OPNUM), (TYPE));				\
	  goto WIN;							\
	}								\
    }									\
} while (0)




/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

extern struct rtx_def *hppa_legitimize_address ();
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)	\
{ rtx orig_x = (X);				\
  (X) = hppa_legitimize_address (X, OLDX, MODE);	\
  if ((X) != orig_x && memory_address_p (MODE, X)) \
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
  if (GET_CODE (ADDR) == PRE_DEC	\
      || GET_CODE (ADDR) == POST_DEC	\
      || GET_CODE (ADDR) == PRE_INC	\
      || GET_CODE (ADDR) == POST_INC)	\
    goto LABEL

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   On the HP-PA we use this to indicate if a symbol is in text or
   data space.  Also, function labels need special treatment. */

#define TEXT_SPACE_P(DECL)\
  (TREE_CODE (DECL) == FUNCTION_DECL					\
   || (TREE_CODE (DECL) == VAR_DECL					\
       && TREE_READONLY (DECL) && ! TREE_SIDE_EFFECTS (DECL)		\
       && (! DECL_INITIAL (DECL) || ! reloc_needed (DECL_INITIAL (DECL))) \
       && !flag_pic)							\
   || (TREE_CODE_CLASS (TREE_CODE (DECL)) == 'c'			\
       && !(TREE_CODE (DECL) == STRING_CST && flag_writable_strings)))

#define FUNCTION_NAME_P(NAME) \
(*(NAME) == '@' || (*(NAME) == '*' && *((NAME) + 1) == '@'))

#define ENCODE_SECTION_INFO(DECL)\
do							\
  { if (TEXT_SPACE_P (DECL))				\
      {	rtx _rtl;					\
	if (TREE_CODE (DECL) == FUNCTION_DECL		\
	    || TREE_CODE (DECL) == VAR_DECL)		\
	  _rtl = DECL_RTL (DECL);			\
	else						\
	  _rtl = TREE_CST_RTL (DECL);			\
	SYMBOL_REF_FLAG (XEXP (_rtl, 0)) = 1;		\
	if (TREE_CODE (DECL) == FUNCTION_DECL)		\
	  hppa_encode_label (XEXP (DECL_RTL (DECL), 0), 0);\
      }							\
  }							\
while (0)

/* Store the user-specified part of SYMBOL_NAME in VAR.
   This is sort of inverse to ENCODE_SECTION_INFO.  */

#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME)	\
  (VAR) = ((SYMBOL_NAME)  + ((SYMBOL_NAME)[0] == '*' ?	\
			     1 + (SYMBOL_NAME)[1] == '@'\
			     : (SYMBOL_NAME)[0] == '@'))

/* On hpux10, the linker will give an error if we have a reference
   in the read-only data section to a symbol defined in a shared
   library.  Therefore, expressions that might require a reloc can
   not be placed in the read-only data section.  */
#define SELECT_SECTION(EXP,RELOC) \
  if (TREE_CODE (EXP) == VAR_DECL \
      && TREE_READONLY (EXP) \
      && !TREE_THIS_VOLATILE (EXP) \
      && DECL_INITIAL (EXP) \
      && (DECL_INITIAL (EXP) == error_mark_node \
          || TREE_CONSTANT (DECL_INITIAL (EXP))) \
      && !RELOC) \
    readonly_data_section (); \
  else if (TREE_CODE_CLASS (TREE_CODE (EXP)) == 'c' \
	   && !(TREE_CODE (EXP) == STRING_CST && flag_writable_strings) \
	   && !RELOC) \
    readonly_data_section (); \
  else \
    data_section ();
   
/* Arghh.  The hpux10 linker chokes if we have a reference to symbols
   in a readonly data section when the symbol is defined in a shared
   library.  Since we can't know at compile time if a symbol will be
   satisfied by a shared library or main program we put any symbolic
   constant into the normal data section.  */
#define SELECT_RTX_SECTION(MODE,RTX)	\
  if (symbolic_operand (RTX, MODE))	\
    data_section ();			\
  else					\
    readonly_data_section ();

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_BIG_SWITCH ? TImode : DImode)

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* Higher than the default as we prefer to use simple move insns
   (better scheduling and delay slot filling) and because our
   built-in block move is really a 2X unrolled loop.  */
#define MOVE_RATIO 4

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Do not break .stabs pseudos into continuations.

   This used to be zero (no max length), but big enums and such can
   cause huge strings which killed gas.

   We also have to avoid lossage in dbxout.c -- it does not compute the
   string size accurately, so we are real conservative here.  */
#define DBX_CONTIN_LENGTH 3000

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* Add any extra modes needed to represent the condition code.

   HPPA floating comparisons produce condition codes. */
#define EXTRA_CC_MODES CCFPmode

/* Define the names for the modes specified above.  */
#define EXTRA_CC_NAMES "CCFP"

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  CC_NOOVmode should be used when the first operand is a
   PLUS, MINUS, or NEG.  CCmode should be used when no special processing is
   needed.  */
#define SELECT_CC_MODE(OP,X,Y) \
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT ? CCFPmode : CCmode)    \

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
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
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:							\
    if (INTVAL (RTX) == 0) return 0;					\
    if (INT_14_BITS (RTX)) return 1;					\
  case HIGH:								\
    return 2;								\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 4;								\
  case CONST_DOUBLE:							\
    if ((RTX == CONST0_RTX (DFmode) || RTX == CONST0_RTX (SFmode))	\
	&& OUTER_CODE != SET)						\
      return 0;								\
    else								\
      return 8;

#define ADDRESS_COST(RTX) \
  (GET_CODE (RTX) == REG ? 1 : hppa_address_cost (RTX))

/* Compute extra cost of moving data between one register class
   and another.

   Make moves from SAR so expensive they should never happen.  We used to
   have 0xffff here, but that generates overflow in rare cases.

   Copies involving a FP register and a non-FP register are relatively
   expensive because they must go through memory.

   Other copies are reasonably cheap.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2) \
 (CLASS1 == SHIFT_REGS ? 0x100					\
  : FP_REG_CLASS_P (CLASS1) && ! FP_REG_CLASS_P (CLASS2) ? 16	\
  : FP_REG_CLASS_P (CLASS2) && ! FP_REG_CLASS_P (CLASS1) ? 16	\
  : 2)


/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  The purpose for the cost of MULT is to encourage
   `synth_mult' to find a synthetic multiply when reasonable.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)					\
  case MULT:								\
    if (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
      return COSTS_N_INSNS (3);						\
    return (TARGET_PA_11 && ! TARGET_DISABLE_FPREGS && ! TARGET_SOFT_FLOAT) \
	    ? COSTS_N_INSNS (8) : COSTS_N_INSNS (20);	\
  case DIV:								\
    if (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
      return COSTS_N_INSNS (14);					\
  case UDIV:								\
  case MOD:								\
  case UMOD:								\
    return COSTS_N_INSNS (60);						\
  case PLUS: /* this includes shNadd insns */				\
  case MINUS:								\
    if (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
      return COSTS_N_INSNS (3);						\
    return COSTS_N_INSNS (1);						\
  case ASHIFT:								\
  case ASHIFTRT:							\
  case LSHIFTRT:							\
    return COSTS_N_INSNS (1);

/* Adjust the cost of dependencies.  */

#define ADJUST_COST(INSN,LINK,DEP,COST) \
  (COST) = pa_adjust_cost (INSN, LINK, DEP, COST)

/* Adjust scheduling priorities.  We use this to try and keep addil
   and the next use of %r1 close together.  */
#define ADJUST_PRIORITY(PREV) \
  {								\
    rtx set = single_set (PREV);				\
    rtx src, dest;						\
    if (set)							\
      {								\
        src = SET_SRC (set);					\
	dest = SET_DEST (set);					\
	if (GET_CODE (src) == LO_SUM				\
	    && symbolic_operand (XEXP (src, 1), VOIDmode)	\
	    && ! read_only_operand (XEXP (src, 1), VOIDmode))   \
	  INSN_PRIORITY (PREV) >>= 3;				\
        else if (GET_CODE (src) == MEM				\
		 && GET_CODE (XEXP (src, 0)) == LO_SUM		\
		 && symbolic_operand (XEXP (XEXP (src, 0), 1), VOIDmode)\
		 && ! read_only_operand (XEXP (XEXP (src, 0), 1), VOIDmode))\
	  INSN_PRIORITY (PREV) >>= 1;				\
	else if (GET_CODE (dest) == MEM				\
		 && GET_CODE (XEXP (dest, 0)) == LO_SUM		\
		 && symbolic_operand (XEXP (XEXP (dest, 0), 1), VOIDmode)\
		 && ! read_only_operand (XEXP (XEXP (dest, 0), 1), VOIDmode))\
	  INSN_PRIORITY (PREV) >>= 3;				\
      }								\
  }

/* Handling the special cases is going to get too complicated for a macro,
   just call `pa_adjust_insn_length' to do the real work.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH)	\
  LENGTH += pa_adjust_insn_length (INSN, LENGTH);

/* Millicode insns are actually function calls with some special
   constraints on arguments and register usage.

   Millicode calls always expect their arguments in the integer argument
   registers, and always return their result in %r29 (ret1).  They
   are expected to clobber their arguments, %r1, %r29, and %r31 and
   nothing else.

   These macros tell reorg that the references to arguments and
   register clobbers for millicode calls do not appear to happen
   until after the millicode call.  This allows reorg to put insns
   which set the argument registers into the delay slot of the millicode
   call -- thus they act more like traditional CALL_INSNs.

   get_attr_type will try to recognize the given insn, so make sure to
   filter out things it will not accept -- SEQUENCE, USE and CLOBBER insns
   in particular.  */
#define INSN_SETS_ARE_DELAYED(X) (insn_sets_and_refs_are_delayed (X))
#define INSN_REFERENCES_ARE_DELAYED(X) (insn_sets_and_refs_are_delayed (X))


/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE) \
do { fputs ("\t.SPACE $PRIVATE$\n\
\t.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31\n\
\t.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82\n\
\t.SPACE $TEXT$\n\
\t.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44\n\
\t.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n\
\t.IMPORT $global$,DATA\n\
\t.IMPORT $$dyncall,MILLICODE\n", FILE);\
     if (profile_flag)\
       fprintf (FILE, "\t.IMPORT _mcount, CODE\n");\
     if (write_symbols != NO_DEBUG) \
       output_file_directive ((FILE), main_input_filename); \
   } while (0)

#define ASM_FILE_END(FILE) output_deferred_plabels (FILE)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* We don't yet know how to identify GCC to HP-PA machines.  */
#define ASM_IDENTIFY_GCC(FILE) fputs ("; gcc_compiled.:\n", FILE)

/* Output before code.  */

/* Supposedly the assembler rejects the command if there is no tab!  */
#define TEXT_SECTION_ASM_OP "\t.SPACE $TEXT$\n\t.SUBSPA $CODE$\n"

/* Output before read-only data.  */

/* Supposedly the assembler rejects the command if there is no tab!  */
#define READONLY_DATA_ASM_OP "\t.SPACE $TEXT$\n\t.SUBSPA $LIT$\n"

#define READONLY_DATA_SECTION readonly_data

/* Output before writable data.  */

/* Supposedly the assembler rejects the command if there is no tab!  */
#define DATA_SECTION_ASM_OP "\t.SPACE $PRIVATE$\n\t.SUBSPA $DATA$\n"

/* Output before uninitialized data.  */

#define BSS_SECTION_ASM_OP "\t.SPACE $PRIVATE$\n\t.SUBSPA $BSS$\n"

/* Define the .bss section for ASM_OUTPUT_LOCAL to use. */

#ifndef CTORS_SECTION_FUNCTION
#define EXTRA_SECTIONS in_readonly_data
#define CTORS_SECTION_FUNCTION
#define DTORS_SECTION_FUNCTION
#else
#define EXTRA_SECTIONS in_readonly_data, in_ctors, in_dtors
#endif

/* Switch into a generic section.
   This is currently only used to support section attributes.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.  */
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC) \
  if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)		\
    {								\
      fputs ("\t.SPACE $TEXT$\n", FILE);			\
      fprintf (FILE,						\
	       "\t.SUBSPA %s%s%s,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY,SORT=24\n",\
	       TARGET_GAS ? "" : "$", NAME, TARGET_GAS ? "" : "$"); \
    }								\
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))		\
    {								\
      fputs ("\t.SPACE $TEXT$\n", FILE);			\
      fprintf (FILE,						\
	       "\t.SUBSPA %s%s%s,QUAD=0,ALIGN=8,ACCESS=44,SORT=16\n", \
	       TARGET_GAS ? "" : "$", NAME, TARGET_GAS ? "" : "$"); \
    }								\
  else								\
    {								\
      fputs ("\t.SPACE $PRIVATE$\n", FILE);			\
      fprintf (FILE,						\
	       "\t.SUBSPA %s%s%s,QUAD=1,ALIGN=8,ACCESS=31,SORT=16\n", \
	       TARGET_GAS ? "" : "$", NAME, TARGET_GAS ? "" : "$"); \
    }

/* FIXME: HPUX ld generates incorrect GOT entries for "T" fixups
   which reference data within the $TEXT$ space (for example constant
   strings in the $LIT$ subspace).

   The assemblers (GAS and HP as) both have problems with handling
   the difference of two symbols which is the other correct way to
   reference constant data during PIC code generation.

   So, there's no way to reference constant data which is in the
   $TEXT$ space during PIC generation.  Instead place all constant
   data into the $PRIVATE$ subspace (this reduces sharing, but it
   works correctly).  */

#define EXTRA_SECTION_FUNCTIONS						\
void									\
readonly_data ()							\
{									\
  if (in_section != in_readonly_data)					\
    {									\
      if (flag_pic)							\
	fprintf (asm_out_file, "%s\n", DATA_SECTION_ASM_OP);		\
      else								\
	fprintf (asm_out_file, "%s\n", READONLY_DATA_ASM_OP);		\
      in_section = in_readonly_data;					\
    }									\
}									\
CTORS_SECTION_FUNCTION							\
DTORS_SECTION_FUNCTION


/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"%r0",   "%r1",    "%r2",   "%r3",    "%r4",   "%r5",    "%r6",   "%r7",    \
 "%r8",   "%r9",    "%r10",  "%r11",   "%r12",  "%r13",   "%r14",  "%r15",   \
 "%r16",  "%r17",   "%r18",  "%r19",   "%r20",  "%r21",   "%r22",  "%r23",   \
 "%r24",  "%r25",   "%r26",  "%r27",   "%r28",  "%r29",   "%r30",  "%r31",   \
 "%fr4",  "%fr4R",  "%fr5",  "%fr5R",  "%fr6",  "%fr6R",  "%fr7",  "%fr7R",  \
 "%fr8",  "%fr8R",  "%fr9",  "%fr9R",  "%fr10", "%fr10R", "%fr11", "%fr11R", \
 "%fr12", "%fr12R", "%fr13", "%fr13R", "%fr14", "%fr14R", "%fr15", "%fr15R", \
 "%fr16", "%fr16R", "%fr17", "%fr17R", "%fr18", "%fr18R", "%fr19", "%fr19R", \
 "%fr20", "%fr20R", "%fr21", "%fr21R", "%fr22", "%fr22R", "%fr23", "%fr23R", \
 "%fr24", "%fr24R", "%fr25", "%fr25R", "%fr26", "%fr26R", "%fr27", "%fr27R", \
 "%fr28", "%fr28R", "%fr29", "%fr29R", "%fr30", "%fr30R", "%fr31", "%fr31R", \
 "SAR"}

#define ADDITIONAL_REGISTER_NAMES \
{{"%fr4L",32}, {"%fr5L",34}, {"%fr6L",36}, {"%fr7L",38},		\
 {"%fr8L",40}, {"%fr9L",42}, {"%fr10L",44}, {"%fr11L",46},		\
 {"%fr12L",48}, {"%fr13L",50}, {"%fr14L",52}, {"%fr15L",54},		\
 {"%fr16L",56}, {"%fr17L",58}, {"%fr18L",60}, {"%fr19L",62},		\
 {"%fr20L",64}, {"%fr21L",66}, {"%fr22L",68}, {"%fr23L",70},		\
 {"%fr24L",72}, {"%fr25L",74}, {"%fr26L",76}, {"%fr27L",78},		\
 {"%fr28L",80}, {"%fr29L",82}, {"%fr30L",84}, {"%fr31R",86},		\
 {"%cr11",88}}

/* How to renumber registers for dbx and gdb.

   Registers 0  - 31 remain unchanged.

   Registers 32 - 87 are mapped to 72 - 127

   Register 88 is mapped to 32.  */

#define DBX_REGISTER_NUMBER(REGNO) \
  ((REGNO) <= 31 ? (REGNO) :						\
   ((REGNO) > 31 && (REGNO) <= 87 ? (REGNO) + 40 : 32))

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE, NAME)	\
  do { assemble_name (FILE, NAME); 	\
       fputc ('\n', FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.

   We call assemble_name, which in turn sets TREE_SYMBOL_REFERENCED.  This
   macro will restore the original value of TREE_SYMBOL_REFERENCED to avoid
   placing useless function definitions in the output file.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
  do { int save_referenced;					\
       save_referenced = TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL)); \
       fputs ("\t.IMPORT ", FILE);					\
	 assemble_name (FILE, NAME);				\
       if (FUNCTION_NAME_P (NAME))     				\
	 fputs (",CODE\n", FILE);				\
       else							\
	 fputs (",DATA\n", FILE);				\
       TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL)) = save_referenced; \
     } while (0)

/* The bogus HP assembler requires ALL external references to be
   "imported", even library calls. They look a bit different, so
   here's this macro.

   Also note not all libcall names are passed to ENCODE_SECTION_INFO
   (__main for example).  To make sure all libcall names have section
   info recorded in them, we do it here.  */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, RTL) \
  do { fputs ("\t.IMPORT ", FILE);					\
       if (!function_label_operand (RTL, VOIDmode))			\
	 hppa_encode_label (RTL, 1);					\
       assemble_name (FILE, XSTR ((RTL), 0));		       		\
       fputs (",CODE\n", FILE);						\
     } while (0)

#define ASM_GLOBALIZE_LABEL(FILE, NAME)					\
  do {									\
    /* We only handle DATA objects here, functions are globalized in	\
       ASM_DECLARE_FUNCTION_NAME.  */					\
    if (! FUNCTION_NAME_P (NAME))					\
      {									\
	fputs ("\t.EXPORT ", FILE);					\
	assemble_name (FILE, NAME);					\
	fputs (",DATA\n", FILE);					\
      }									\
  } while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf ((FILE), "%s", (NAME) + (FUNCTION_NAME_P (NAME) ? 1 : 0))

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  {fprintf (FILE, "%c$%s%04d\n", (PREFIX)[0], (PREFIX) + 1, NUM);}

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%c$%s%04d", (PREFIX)[0], (PREFIX) + 1, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  do { long l[2];							\
       REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);				\
       fprintf (FILE, "\t.word 0x%lx\n\t.word 0x%lx\n", l[0], l[1]);	\
     } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  do { long l;								\
       REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);				\
       fprintf (FILE, "\t.word 0x%lx\n", l);				\
     } while (0)

/* This is how to output an assembler line defining an `int' constant. 

   This is made more complicated by the fact that functions must be
   prefixed by a P% as well as code label references for the exception
   table -- otherwise the linker chokes.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
{ fputs ("\t.word ", FILE);			\
  if (function_label_operand (VALUE, VOIDmode)	\
      && !TARGET_PORTABLE_RUNTIME)		\
    fputs ("P%", FILE);				\
  output_addr_const (FILE, (VALUE));		\
  fputs ("\n", FILE);}

/* Likewise for `short' and `char' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fputs ("\t.half ", FILE),			\
  output_addr_const (FILE, (VALUE)),		\
  fputs ("\n", FILE))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fputs ("\t.byte ", FILE),			\
  output_addr_const (FILE, (VALUE)),		\
  fputs ("\n", FILE))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)  \
  output_ascii ((FILE), (P), (SIZE))

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)
#define ASM_OUTPUT_REG_POP(FILE,REGNO)
/* This is how to output an element of a case-vector that is absolute.
   Note that this method makes filling these branch delay slots
   impossible.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  if (TARGET_BIG_SWITCH)					\
    fprintf (FILE, "\tstw %%r1,-16(%%r30)\n\tldil LR'L$%04d,%%r1\n\tbe RR'L$%04d(%%sr4,%%r1)\n\tldw -16(%%r30),%%r1\n", VALUE, VALUE);		\
  else								\
    fprintf (FILE, "\tb L$%04d\n\tnop\n", VALUE)

/* Jump tables are executable code and live in the TEXT section on the PA.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* This is how to output an element of a case-vector that is relative.
   This must be defined correctly as it is used when generating PIC code.

   I believe it safe to use the same definition as ASM_OUTPUT_ADDR_VEC_ELT
   on the PA since ASM_OUTPUT_ADDR_VEC_ELT uses pc-relative jump instructions
   rather than a table of absolute addresses.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  if (TARGET_BIG_SWITCH)					\
    fprintf (FILE, "\tstw %%r1,-16(%%r30)\n\tldw T'L$%04d(%%r19),%%r1\n\tbv %%r0(%%r1)\n\tldw -16(%%r30),%%r1\n", VALUE);				\
  else								\
    fprintf (FILE, "\tb L$%04d\n\tnop\n", VALUE)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    fprintf (FILE, "\t.align %d\n", (1<<(LOG)))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.blockz %d\n", (SIZE))

/* This says how to output an assembler line to define a global common symbol
   with size SIZE (in bytes) and alignment ALIGN (in bits).  */

#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNED)  		\
{ bss_section ();							\
  assemble_name ((FILE), (NAME));					\
  fputs ("\t.comm ", (FILE));						\
  fprintf ((FILE), "%d\n", MAX ((SIZE), ((ALIGNED) / BITS_PER_UNIT)));}

/* This says how to output an assembler line to define a local common symbol
   with size SIZE (in bytes) and alignment ALIGN (in bits).  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
{ bss_section ();							\
  fprintf ((FILE), "\t.align %d\n", ((ALIGNED) / BITS_PER_UNIT));	\
  assemble_name ((FILE), (NAME));				\
  fprintf ((FILE), "\n\t.block %d\n", (SIZE));}
  
/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* All HP assemblers use "!" to separate logical lines.  */
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) ((C) == '!')

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR) == '@' || (CHAR) == '#' || (CHAR) == '*' || (CHAR) == '^')

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the HP-PA, the CODE can be `r', meaning this is a register-only operand
   and an immediate zero should be represented as `r0'.

   Several % codes are defined:
   O an operation
   C compare conditions
   N extract conditions
   M modifier to handle preincrement addressing for memory refs.
   F modifier to handle preincrement addressing for fp memory refs */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)


/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx addr = ADDR;						\
  register rtx base;							\
  int offset;								\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "0(%s)", reg_names [REGNO (addr)]);		\
      break;								\
    case PLUS:								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)			\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);	\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)			\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);	\
      else								\
	abort ();							\
      fprintf (FILE, "%d(%s)", offset, reg_names [REGNO (base)]);	\
      break;								\
    case LO_SUM:							\
      if (!symbolic_operand (XEXP (addr, 1)))				\
	fputs ("R'", FILE);						\
      else if (flag_pic == 0)						\
	fputs ("RR'", FILE);						\
      else if (flag_pic == 1)						\
	abort ();							\
      else if (flag_pic == 2)						\
	fputs ("RT'", FILE);						\
      output_global_address (FILE, XEXP (addr, 1), 0);			\
      fputs ("(", FILE);						\
      output_operand (XEXP (addr, 0), 0);				\
      fputs (")", FILE);						\
      break;								\
    case CONST_INT:							\
      fprintf (FILE, "%d(%%r0)", INTVAL (addr));			\
      break;								\
    default:								\
      output_addr_const (FILE, addr);					\
    }}


/* Define functions in pa.c and used in insn-output.c.  */

extern char *output_and ();
extern char *output_ior ();
extern char *output_move_double ();
extern char *output_fp_move_double ();
extern char *output_block_move ();
extern char *output_cbranch ();
extern char *output_bb ();
extern char *output_bvb ();
extern char *output_dbra ();
extern char *output_movb ();
extern char *output_parallel_movb ();
extern char *output_parallel_addb ();
extern char *output_return ();
extern char *output_call ();
extern char *output_millicode_call ();
extern char *output_mul_insn ();
extern char *output_div_insn ();
extern char *output_mod_insn ();
extern char *singlemove_string ();
extern void output_arg_descriptor ();
extern void output_deferred_plabels ();
extern void override_options ();
extern void output_ascii ();
extern void output_function_prologue ();
extern void output_function_epilogue ();
extern void output_global_address ();
extern void print_operand ();
extern struct rtx_def *legitimize_pic_address ();
extern struct rtx_def *gen_cmp_fp ();
extern void hppa_encode_label ();
extern int arith11_operand ();
extern int symbolic_expression_p ();
extern int reloc_needed ();
extern int compute_frame_size ();
extern int hppa_address_cost ();
extern int and_mask_p ();
extern int symbolic_memory_operand ();
extern int pa_adjust_cost ();
extern int pa_adjust_insn_length ();
extern int int11_operand ();
extern int reg_or_cint_move_operand ();
extern int arith5_operand ();
extern int uint5_operand ();
extern int pic_label_operand ();
extern int plus_xor_ior_operator ();
extern int basereg_operand ();
extern int shadd_operand ();
extern int arith_operand ();
extern int read_only_operand ();
extern int move_operand ();
extern int and_operand ();
extern int ior_operand ();
extern int arith32_operand ();
extern int uint32_operand ();
extern int reg_or_nonsymb_mem_operand ();
extern int reg_or_0_operand ();
extern int reg_or_0_or_nonsymb_mem_operand ();
extern int pre_cint_operand ();
extern int post_cint_operand ();
extern int div_operand ();
extern int int5_operand ();
extern int movb_comparison_operator ();
extern int ireg_or_int5_operand ();
extern int fmpyaddoperands ();
extern int fmpysuboperands ();
extern int call_operand_address ();
extern int cint_ok_for_move ();
extern int ior_operand ();
extern void emit_bcond_fp ();
extern int emit_move_sequence ();
extern int emit_hpdiv_const ();
extern void hppa_expand_prologue ();
extern void hppa_expand_epilogue ();
extern int hppa_can_use_return_insn_p ();
extern int is_function_label_plus_const ();
extern int jump_in_call_delay ();
extern enum reg_class secondary_reload_class ();
extern int insn_sets_and_refs_are_delayed ();

/* Declare functions defined in pa.c and used in templates.  */

extern struct rtx_def *return_addr_rtx ();

/* We want __gcc_plt_call to appear in every program built by
   gcc, so we make a reference to it out of __main.
   We use the asm statement to fool the optimizer into not
   removing the dead (but important) initialization of
   REFERENCE.  */

#define DO_GLOBAL_DTORS_BODY			\
do {						\
  extern void __gcc_plt_call ();		\
  void (*reference)() = &__gcc_plt_call;	\
  func_ptr *p;					\
  __asm__ ("" : : "r" (reference));		\
  for (p = __DTOR_LIST__ + 1; *p; )		\
    (*p++) ();					\
} while (0)

/* Find the return address associated with the frame given by
   FRAMEADDR.  */
#define RETURN_ADDR_RTX(COUNT, FRAMEADDR)				 \
  (return_addr_rtx (COUNT, FRAMEADDR))

/* Used to mask out junk bits from the return address, such as
   processor state, interrupt status, condition codes and the like.  */
#define MASK_RETURN_ADDR						\
  /* The privilege level is in the two low order bits, mask em out	\
     of the return address.  */						\
  (GEN_INT (0xfffffffc))

/* The number of Pmode words for the setjmp buffer.  */
#define JMP_BUF_SIZE 50
