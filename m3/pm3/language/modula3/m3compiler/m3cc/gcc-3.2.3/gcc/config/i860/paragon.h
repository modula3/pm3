/* Target definitions for GNU compiler for Intel 80860 running OSF/1AD
   Copyright (C) 1991, 1996, 1999, 2000 Free Software Foundation, Inc.
   Based upon original work of Ron Guilmette (rfg@monkeys.com).
   Contributed by Andy Pfiffer (andyp@ssd.intel.com).
   Partially inspired by
	Pete Beckman of Indiana University (beckman@cs.indiana.edu)
	Harry Dolan of Intel Corporation (dolan@ssd.intel.com)

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

#undef TARGET_SWITCHES
#define TARGET_SWITCHES  \
  { {"xp", 1, N_("Generate code which uses the FPU")},              \
    {"noxp", -1, N_("Do not generate code which uses the FPU")},    \
    {"xr", -1, N_("Do not generate code which uses the FPU")},      \
    {"noieee", -1, N_("Do not generate code which uses the FPU")},	\
    {"nx", 2, NULL},                  \
    { "", TARGET_DEFAULT, NULL}}
 
#undef TARGET_DEFAULT
#define TARGET_DEFAULT 1

/* The Intel as860 assembler does not understand .stabs, must use COFF */
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860 OSF/1AD)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES	"-Di860 -D__i860 -D__i860__ -D__PARAGON__ -D__OSF1__ -D_COFF -Dunix -DMACH -DCMU"

#define CPP_SPEC "%{mnx:-D__NODE}"

/* autoinit.o autolaunches NX applications */
#define STARTFILE_SPEC "crt0.o%s %{mnx:-yoptions/autoinit.o%s}"

/* libic.a is the PGI intrinsic library */
/* libpm.o and guard.o are for the performance monitoring modules (ignored) */
/* /usr/lib/noieee contains non-IEEE compliant (but faster) math routines */
#if	HAVE_DASH_G
#define LIB_SPEC \
"%{mnoieee:-L/usr/lib/noieee} %{mnx:-lnx} %{g*:-lg} -lc -lmach -lc -lic"
#else	/* HAVE_DASH_G */
/* can't use -g for -lg; libg.a doesn't have a symbol table and ld complains */
#define LIB_SPEC "%{mnoieee:-L/usr/lib/noieee} %{mnx:-lnx} -lc -lmach -lc -lic"
#endif	/* HAVE_DASH_G */

/* Get rid of definition from svr3.h.  */
#undef SIZE_TYPE

#undef	I860_REG_PREFIX

#undef	ASM_COMMENT_START
#define ASM_COMMENT_START "//"

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT      "\"%s\""

#undef ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP	"\t.byte\t"

/*
 *	the assembler we're using doesn't grok .ident...
 */
#undef	ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "//\t.ident \"%s\"\n", NAME);

#undef	ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)			\
  do								\
    {								\
      register const unsigned char *str = (const unsigned char *) (STR); \
      register const unsigned char *limit = str + (LENGTH);	\
      register unsigned bytes_in_chunk = 0;			\
      for (; str < limit; str++)				\
        {							\
          register unsigned ch = *str;				\
          if (ch < 32 || ch == '\\' || ch == '"' || ch >= 127)	\
	    {							\
	      if (bytes_in_chunk > 0)				\
	        {						\
	          fprintf ((FILE), "\"\n");			\
	          bytes_in_chunk = 0;				\
	        }						\
	      assemble_aligned_integer (1, GEN_INT (ch));	\
	    }							\
          else							\
	    {							\
	      if (bytes_in_chunk >= 60)				\
	        {						\
	          fprintf ((FILE), "\"\n");			\
	          bytes_in_chunk = 0;				\
	        }						\
	      if (bytes_in_chunk == 0)				\
	        fprintf ((FILE), "%s\"", ASCII_DATA_ASM_OP);	\
	      putc (ch, (FILE));				\
	      bytes_in_chunk++;					\
	    }							\
        }							\
      if (bytes_in_chunk > 0)					\
        fprintf ((FILE), "\"\n");				\
    }								\
  while (0)


/* This says how to output an assembler line
   to define a local common symbol.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/*
 *	not defining ASM_STABS_OP yields .stabs in the .s file
 *	when using g++ -- so, I'll define it.
 */
#define	ASM_STABS_OP	"//.stabs"

/* Define this macro if an instruction to load a value narrower
   than a word from memory into a register also zero-extends the
   value to the whole  register.  */
/*#define BYTE_LOADS_ZERO_EXTEND*/

/* Define this macro as a C expression which is nonzero if
   accessing less than a word of memory (i.e. a `char' or a
   `short') is no faster than accessing a word of memory, i.e., if
   such access require more than one instruction or if there is no
   difference in cost between byte and (aligned) word loads.

   On RISC machines, it tends to generate better code to define
   this as 1, since it avoids making a QI or HI mode register.  */
/*
#undef SLOW_BYTE_ACCESS
#define SLOW_BYTE_ACCESS 1
*/

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count. */
#define SHIFT_COUNT_TRUNCATED 1


#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/*
 *	disable a few things picked up from svr3.h
 */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#undef CONST_SECTION_ASM_OP
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#undef DO_GLOBAL_CTORS_BODY
#undef ASM_OUTPUT_DESTRUCTOR
#undef SELECT_SECTION
#undef SELECT_RTX_SECTION
#undef READONLY_DATA_SECTION

#define	BSS_SECTION_ASM_OP	"\t.bss"	/* XXX */
#undef EXTRA_SECTIONS
#undef EXTRA_SECTION_FUNCTIONS
