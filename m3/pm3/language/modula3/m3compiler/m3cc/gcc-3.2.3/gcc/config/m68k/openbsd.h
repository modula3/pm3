/* Configuration file for an m68k OpenBSD target.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* m68k is an old configuration that does not yet use the TARGET_CPU_DEFAULT
   framework.  */
#define TARGET_DEFAULT (MASK_BITFIELD | MASK_68881 | MASK_68020)

#include <m68k/m68k.h>

/* Get generic OpenBSD definitions.  */
#define OBSD_OLD_GAS
#include <openbsd.h>

/* Define __HAVE_68881__ in preprocessor, unless -msoft-float is specified.
   This will control the use of inline 68881 insns in certain macros.  */
#undef CPP_SPEC
#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__ -D__HAVE_FPU__} %{posix:-D_POSIX_SOURCE} %{pthread:-D_POSIX_THREADS}"

/* Run-time target specifications.  */
#define CPP_PREDEFINES "-D__unix__ -D__m68k__ -D__mc68000__ -D__mc68020__ -D__OpenBSD__ -Asystem=unix -Asystem=OpenBSD -Acpu=m68k -Amachine=m68k"

/* m68k as needs to know about the processor subtype.  */
#undef ASM_SPEC
#define ASM_SPEC "%| %{m68030} %{m68040} %{m68060} %{fpic:-k} %{fPIC:-k -K}"

/* Layout of source language data types.  */

/* This must agree with <machine/ansi.h> */
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Storage layout.  */

/* Every structure or union's size must be a multiple of 2 bytes.  */
#define STRUCTURE_SIZE_BOUNDARY 16

/* Specific options for DBX Output.  */

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */
#define DBX_CONTIN_CHAR '?'

/* Stack & calling: aggregate returns.  */

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Assembler format: exception region output.  */

/* All configurations that don't use elf must be explicit about not using
   dwarf unwind information. egcs doesn't try too hard to check internal
   configuration files...  */
#define DWARF2_UNWIND_INFO 0


/* TODO: ASM_OUTPUT_MI_THUNK is busted. I need to figure out 
   what bra func@PLTPC means under linux, and find the corresponding 
   construction for our gas/pic setup.  */
#if 0
/* Taken from linux.h. Processor dependent optimized code to handle C++
   multiple inheritance vtable lookup.  */

/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	\
do {									\
  if (DELTA > 0 && DELTA <= 8)						\
    asm_fprintf (FILE, "\taddq.l %I%d,4(%Rsp)\n", DELTA);		\
  else if (DELTA < 0 && DELTA >= -8)					\
    asm_fprintf (FILE, "\tsubq.l %I%d,4(%Rsp)\n", -DELTA);		\
  else									\
    asm_fprintf (FILE, "\tadd.l %I%d,4(%Rsp)\n", DELTA);		\
									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tbra.l ");					\
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	\
      fprintf (FILE, "@PLTPC\n");					\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tjmp ");						\
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	\
      fprintf (FILE, "\n");						\
    }									\
} while (0)
#endif

