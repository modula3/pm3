/* CYGNUS LOCAL ericsson */
/* Definitions of target machine for GNU compiler.  Vxworks 68010 version.
   Copyright 1987, 1988, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "m68k/m68k.h"
#include "aoutos.h"

/* See m68k.h.  0 means 68000 with no 68881.  */

#define TARGET_DEFAULT 0

/* Define __HAVE_68881 in preprocessor only if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#define CPP_SPEC "%{m68881:-D__HAVE_68881__} \
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

/* -m68020 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68020:-mc68020}%{mc68020:-mc68020}%{!mc68020:%{!m68020:-mc68000}}"
  
/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000"

/* Prevent error on `-sun2' and `-target sun2' options.  */

#define CC1_SPEC "%{sun2:} %{target:}"

#define PTRDIFF_TYPE "int"
#define SIZE_TYPE "unsigned int"
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Specify what to link with.  */

/* VxWorks does all the library stuff itself.  */

#define LIB_SPEC ""

/* Provide required defaults for linker -e. */
 
#define LINK_SPEC "%{!nostdlib:%{!r*:%{!e*:-e start}}}"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#define STARTFILE_SPEC ""

/* Alignment of field after `int : 0' in a structure.  */

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 16

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO
