/* Definitions of target machine for GNU compiler.  ARM on semi-hosted platform
   AOF Syntax assembler.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (richard.earnshaw@armltd.co.uk)

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

#define CPP_PREDEFINES \
    "-Darm -Dsemi"

#define ASM_SPEC "%{g -g} -arch 4 \
-apcs 3%{mapcs-32:/32bit}%{mapcs-26:/26bit}%{!mapcs-26:%{!macps-32:/32bit}}"

#define LIB_SPEC "%{Eb: armlib_h.32b%s}%{!Eb: armlib_h.32l%s}"

#define TARGET_VERSION fputs (" (ARM/semi-hosted)", stderr);

#define TARGET_DEFAULT ARM_FLAG_APCS_32

/* The Norcroft C library defines size_t as "unsigned int" */
#define SIZE_TYPE "unsigned int"

#undef CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC "-D__APCS_32__"
