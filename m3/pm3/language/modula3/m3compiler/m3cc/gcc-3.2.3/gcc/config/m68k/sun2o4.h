/* Definitions of target machine for GNU compiler.  Sun 2 running SunOS 4.
   Copyright (C) 1987, 1988, 1993, 1996, 1997 Free Software Foundation, Inc.

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

#include "m68k/sun2.h"


/* Define __HAVE_SKY__ in preprocessor, according to the -m flags.
   Also inform the program which CPU this is for.  */

#undef CPP_SPEC

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
#undef SIZE_TYPE
#define SIZE_TYPE "int"
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#if TARGET_DEFAULT & MASK_SKY

/* -msky is the default */
#define CPP_SPEC \
"%{!msoft-float:-D__HAVE_SKY__}\
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{msky:-D__HAVE_SKY__ }\
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

#endif

/* STARTFILE_SPEC to include sun floating point initialization
   This is necessary (tr: Sun does it) for the sky routines.
   I'm not sure what would happen below if people gave contradictory
   arguments (eg. -msoft-float -mfpa) */

#undef STARTFILE_SPEC

#if TARGET_DEFAULT & MASK_SKY
/* -msky is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{msoft-float:Fcrt1.o%s}				\
   %{!msoft-float:Scrt1.o%s}"
#else
/* -msoft-float is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{msky:Scrt1.o%s}					\
   %{!msky:Fcrt1.o%s}"
#endif

/* Specify library to handle `-a' basic block profiling.
   Control choice of libm.a (if user says -lm)
   based on fp arith default and options.  */

#undef LIB_SPEC

#if TARGET_DEFAULT & MASK_SKY
/* -msky is the default */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o -lc} %{g:-lg} \
%{msoft-float:-L/usr/lib/fsoft} \
%{!msoft_float:-L/usr/lib/fsky}"
#else
/* -msoft-float is the default */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o -lc} %{g:-lg} \
%{!msky:-L/usr/lib/fsoft} \
%{msky:-L/usr/lib/ffpa}"
#endif

#undef LINK_SPEC
#define LINK_SPEC \
  "%{!nostdlib:%{!r*:%{!e*:-e start}}} -dc -dp %{static:-Bstatic}"

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)		\
 do { 								\
      if (CODE != 'f')						\
        {							\
          long l;						\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
          if (sizeof (int) == sizeof (long))			\
            asm_fprintf ((FILE), "%I0x%x", (int) l);		\
          else							\
            asm_fprintf ((FILE), "%I0x%lx", l);			\
        }							\
      else if (REAL_VALUE_ISINF (VALUE))			\
        {							\
          if (REAL_VALUE_NEGATIVE (VALUE))			\
            fprintf (FILE, "#0r-99e999");			\
          else							\
            fprintf (FILE, "#0r99e999");			\
        }							\
      else if (REAL_VALUE_MINUS_ZERO (VALUE))			\
        {							\
          fprintf (FILE, "#0r-0.0");				\
        }							\
      else							\
        { char dstr[30];					\
          REAL_VALUE_TO_DECIMAL ((VALUE), "%.9g", dstr);	\
          fprintf (FILE, "#0r%s", dstr);			\
        }							\
    } while (0)

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { if (REAL_VALUE_ISINF (VALUE))					\
        {								\
          if (REAL_VALUE_NEGATIVE (VALUE))				\
            fprintf (FILE, "#0r-99e999");				\
          else								\
            fprintf (FILE, "#0r99e999");				\
        }								\
      else if (REAL_VALUE_MINUS_ZERO (VALUE))				\
        {								\
          fprintf (FILE, "#0r-0.0");					\
        }								\
      else								\
        { char dstr[30];						\
          REAL_VALUE_TO_DECIMAL ((VALUE), "%.20g", dstr);		\
          fprintf (FILE, "#0r%s", dstr);				\
        }								\
    } while (0)
