/* Definitions of target machine for GNU compiler.  "naked" 68020,
   a.out object files and debugging, version.
   Copyright (C) 1994, 1996 Free Software Foundation, Inc.

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

/* This comment is here to see if it will keep Sun's cpp from dying.  */

#include "m68k/m68k-none.h"
#include "m68k/m68kemb.h"
#include "aoutos.h"

#define DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as uninitialized global
   data.  */
#define BSS_SECTION_ASM_OP "\t.bss"

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes.  The variable ROUNDED
   is the size rounded up to whatever alignment the caller wants.
   Try to use asm_output_bss to implement this macro.  */
/* a.out files typically can't handle arbitrary variable alignments so
   define ASM_OUTPUT_BSS instead of ASM_OUTPUT_ALIGNED_BSS.  */
#define ASM_OUTPUT_BSS(FILE, DECL, NAME, SIZE, ROUNDED) \
  asm_output_bss ((FILE), (DECL), (NAME), (SIZE), (ROUNDED))
