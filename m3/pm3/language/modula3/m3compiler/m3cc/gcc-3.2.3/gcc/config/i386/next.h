/* Target definitions for GNU compiler for Intel x86 CPU running NeXTSTEP
   Copyright (C) 1993, 1995, 1996, 1999 Free Software Foundation, Inc.

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

#include "i386/gas.h"
#include "nextstep.h"

/* By default, target has a 80387, with IEEE FP.  */

#undef	TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT  (MASK_80387 | MASK_IEEE_FP)

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Machines that use the AT&T assembler syntax
   also return floating point values in an FP register.
   Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#undef	VALUE_REGNO
#define VALUE_REGNO(MODE) \
  ((MODE) == SFmode || (MODE) == DFmode || (MODE) == XFmode	\
   ? FIRST_FLOAT_REG : 0)

/* A C statement or statements which output an assembler instruction
   opcode to the stdio stream STREAM.  The macro-operand PTR is a
   variable of type `char *' which points to the opcode name in its
   "internal" form--the form that is written in the machine description.

   GAS version 1.38.1 doesn't understand the `repz' opcode mnemonic.
   So use `repe' instead.  */

#undef	ASM_OUTPUT_OPCODE
#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
{							\
  if ((PTR)[0] == 'r'					\
      && (PTR)[1] == 'e'				\
      && (PTR)[2] == 'p')				\
    {							\
      if ((PTR)[3] == 'z')				\
	{						\
	  fprintf (STREAM, "repe");			\
	  (PTR) += 4;					\
	}						\
      else if ((PTR)[3] == 'n' && (PTR)[4] == 'z')	\
	{						\
	  fprintf (STREAM, "repne");			\
	  (PTR) += 5;					\
	}						\
    }							\
}

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.

   GAS requires the %cl argument, so override unx386.h.  */

#undef	SHIFT_DOUBLE_OMITS_COUNT
#define SHIFT_DOUBLE_OMITS_COUNT 0

/* Print opcodes the way that GAS expects them.  */
#define GAS_MNEMONICS 1

/* Names to predefine in the preprocessor for this target machine.  */

#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-DNeXT -Dunix -D__MACH__ -D__LITTLE_ENDIAN__ \
  -D__ARCHITECTURE__=\"i386\" -Asystem=unix -Asystem=mach"

/* This accounts for the return pc and saved fp on the i386.  */

#define OBJC_FORWARDING_STACK_OFFSET 8
#define OBJC_FORWARDING_MIN_OFFSET 8

/* We do not want a dot in internal labels.  */

#undef LPREFIX
#define LPREFIX "L"

#undef	ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
    sprintf ((BUF), "*%s%ld", (PREFIX), (long)(NUMBER))

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#undef	ASM_APP_ON
#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#undef	ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef	ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tpushl %se%s\n", "%", reg_names[REGNO])

#undef	ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tpopl %se%s\n", "%", reg_names[REGNO])

/* This is being overridden because the default i386 configuration
   generates calls to "_mcount".  NeXT system libraries all use
   "mcount".  */

#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%edx\n",		\
               LPREFIX, (LABELNO));					\
      fprintf (FILE, "\tcall *mcount@GOT(%%ebx)\n");			\
    }									\
  else									\
    {								  	\
      fprintf (FILE, "\tmovl $%sP%d,%%edx\n", LPREFIX, (LABELNO));	\
      fprintf (FILE, "\tcall mcount\n");				\
    }									\
}

/* BEGIN Calling Convention CHANGES */

/* These changes violate the Intel/Unix ABI.  Specifically, they
   change the way that space for a block return value is passed to a
   function.  The ABI says that the pointer is passed on the stack.
   We change to pass the pointer in %ebx.  This makes the NeXT
   Objective-C forwarding mechanism possible to implement on an i386.  */

/* Do NOT pass address of structure values on the stack.  */

#undef	STRUCT_VALUE_INCOMING
#undef	STRUCT_VALUE

/* Pass them in %ebx.  */

#undef	STRUCT_VALUE_REGNUM
#define STRUCT_VALUE_REGNUM 3

/* Because we are passing the pointer in a register, we don't need to
   rely on the callee to pop it.  */

#undef	RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 				\
  ((FUNDECL) && TREE_CODE (FUNDECL) == IDENTIFIER_NODE		\
   ? 0								\
   : (TARGET_RTD						\
      && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
          || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
              == void_type_node))) ? (SIZE) : 0)

/* END Calling Convention CHANGES */

/* NeXT still uses old binutils that don't insert nops by default
   when the .align directive demands to insert extra space in the text
   segment.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d,0x90\n", (LOG))
