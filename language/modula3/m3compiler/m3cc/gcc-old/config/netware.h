/* This file is CYGNUS LOCAL.  */

/* netware.h -- operating system specific defines to be used when 
   targeting GCC for some generic NetWare 4 system.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

   Written by David V. Henkel-Wallace (gumby@cygnus.com)

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

/* We don't actually need any of these; the MD_ vars are ignored
   anyway for cross-compilers, and the other specs won't get picked up
   'coz the user is supposed to do ld -r (hmm, perhaps that should be
   the default).  In any case, setting them thus will catch some
   common user errors. */

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef	LIB_SPEC
#define LIB_SPEC ""

/* Kinda useless, but what the hell */
#undef	LINK_SPEC
#define LINK_SPEC "%{h*} %{V} %{v:%{!V:-V}} \
		   %{b} %{Wl,*:%*} \
		   %{Qy:} %{!Qn:-Qy}"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC ""

#undef	RELATIVE_PREFIX_NOT_LINKDIR
#undef	LIBGCC_SPEC

/* set debugging info */
#define	DBX_DEBUGGING_INFO
#undef	SDB_DEBUGGING_INFO
#undef	DWARF_DEBUGGING_INFO
#undef	XCOFF_DEBUGGING_INFO
#undef	PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

#define	HAVE_ATEXIT

#undef	HAS_INIT_SECTION
#undef	INIT_SECTION_ASM_OP

#undef	READONLY_DATA_SECTION
#define	READONLY_DATA_SECTION	const_section 

#undef	CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP    ".section\t.rodata"
#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"x\""
#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"x\""

/* A list of other sections which the compiler might be "in" at any
   given time.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors

/* A list of extra section function definitions.  */

#undef	EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#undef	CONST_SECTION_FUNCTION
#define CONST_SECTION_FUNCTION                                          \
void                                                                    \
const_section ()                                                        \
{                                                                       \
  if (in_section != in_const)                                      	\
    {                                                                   \
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);             \
      in_section = in_const;                                            \
    }                                                                   \
}

#undef	CTORS_SECTION_FUNCTION
#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#undef	DTORS_SECTION_FUNCTION
#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#define INT_ASM_OP ".long"

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)
