/* Definitions of target machine for GNU compiler, for SPARC64, ELF.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
   Contributed by Doug Evans, dje@cygnus.com.

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

/* This is a v9 only compiler.  -mv8 is not expected to work.  If you want
   a v8/v9 compiler, this isn't the place to do it.  */

#define SPARC_V9 1	/* See sparc.h.  */
#define SPARC_ARCH64 1

/* ??? We're taking the scheme of including another file and then overriding
   the values we don't like a bit too far here.  The alternative is to more or
   less duplicate all of svr4.h, sparc/sysv4.h, and sparc/sol2.h here
   (suitably cleaned up).  */

#include "sparc/sol2.h"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc64-elf)")

/* A v9 compiler with stack-bias, 32 bit integers and 64 bit pointers,
   in a Medium/Anywhere code model environment.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_ARCH64 + MASK_PTR64 + MASK_HARD_QUAD \
   + MASK_STACK_BIAS + MASK_MEDANY + MASK_APP_REGS + MASK_EPILOGUE + MASK_FPU)

/* __svr4__ is used by the C library */
/* ??? __arch64__ is subject to change.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__sparc__ -D__sparc_v9__ -D__arch64__ -D__svr4__ \
-Acpu(sparc64) -Amachine(sparc64) \
"

#undef CPP_SPEC
#define CPP_SPEC "\
%{mint64:-D__INT_MAX__=9223372036854775807LL -D__LONG_MAX__=9223372036854775807LL} \
%{mlong64:-D__LONG_MAX__=9223372036854775807LL} \
"

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef ASM_SPEC
#define ASM_SPEC "\
%{v:-V} -s %{fpic:-K PIC} %{fPIC:-K PIC} \
"

/* This is taken from sol2.h.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{v:-V} \
"

/* We need something a little simpler for the embedded environment.
   Profiling doesn't really work yet so we just copy the default.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{!shared:%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}} \
crtbegin.o%s \
"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"

/* Use the default (for now).  */
#undef LIB_SPEC

/* Unfortunately, svr4.h redefines these so we have to restore them to
   their original values in sparc.h.  */
/* ??? It might be possible to eventually get svr4.h to do the right thing.  */

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long long int"

#undef SIZE_TYPE
#define SIZE_TYPE "long long unsigned int"

/* ??? This should be 32 bits for v9 but what can we do?  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* The medium/anywhere code model practically requires us to put jump tables
   in the text section as gcc is unable to distinguish LABEL_REF's of jump
   tables from other label refs (when we need to).  */
#undef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION

/* System V Release 4 uses DWARF debugging info.
   GDB doesn't support 64 bit stabs yet and the desired debug format is DWARF
   anyway so it is the default.  */

#define DWARF_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG

/* Stabs doesn't use this, and it confuses a simulator.  */
/* ??? Need to see what DWARF needs, if anything.  */
#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE)

/* Define the names of various pseudo-ops used by the Sparc/svr4 assembler.
   ??? If ints are 64 bits then UNALIGNED_INT_ASM_OP (defined elsewhere) is
   misnamed.  These should all refer to explicit sizes (half/word/xword?),
   anything other than short/int/long/etc.  */

#define UNALIGNED_LONGLONG_ASM_OP	".uaxword"

/* DWARF stuff.  */

#define ASM_OUTPUT_DWARF_ADDR(FILE, LABEL) \
do {								\
  fprintf ((FILE), "\t%s\t", UNALIGNED_LONGLONG_ASM_OP);	\
  assemble_name ((FILE), (LABEL));				\
  fprintf ((FILE), "\n");					\
} while (0)

#define ASM_OUTPUT_DWARF_ADDR_CONST(FILE, RTX) \
do {								\
  fprintf ((FILE), "\t%s\t", UNALIGNED_LONGLONG_ASM_OP);	\
  output_addr_const ((FILE), (RTX));				\
  fputc ('\n', (FILE));						\
} while (0)

/* ??? Not sure if this should be 4 or 8 bytes.  4 works for now.  */
#define ASM_OUTPUT_DWARF_REF(FILE, LABEL) \
do {								\
  fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);		\
  assemble_name ((FILE), (LABEL));				\
  fprintf ((FILE), "\n");					\
} while (0)
