/* Definitions of target machine for GNU compiler.  MIPS R4000 version with
   GOFAST floating point library.
   Copyright (C) 1994, 1995, 1996, 1997, 1999, 2000 Free Software Foundation, Inc.

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

#define OBJECT_FORMAT_ELF

/* Default to -mips3.  */
#define TARGET_DEFAULT MASK_FLOAT64|MASK_64BIT
#define MIPS_ISA_DEFAULT 3

/* This should change to n32 when it is supported in gas.  */
#define MIPS_ABI_DEFAULT ABI_O64

/* Until we figure out what MIPS ELF targets normally use, just do
   stabs in ELF.  */
#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

/* US Software GOFAST library support.  */
#include "gofast.h"
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS

#include "mips/mips.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -DMIPSEB -DR4000 -D_mips -D_MIPSEB -D_R4000"

/* I would rather put this in CPP_PREDEFINES, but the gcc driver
   doesn't handle -U options in CPP_PREDEFINES.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
%{!mips1:%{!mips2:-U__mips -D__mips=3 -D__mips64}}"

/* Use memcpy, et. al., rather than bcopy.  */
#define TARGET_MEM_FUNCTIONS

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */

#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (32768*8)

/* We need to use .esize and .etype instead of .size and .type to
   avoid conflicting with ELF directives.  */
#undef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.esize\t");		\
  fprintf (asm_out_text_file, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) (a)); \
  fprintf (asm_out_text_file, ";");		       	\
} while (0)

#undef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.etype\t0x%x;", (a));	\
} while (0)

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

/* Given that Irix has it's own headers, not having TARGET_GAS here
   seems a mistake.  If we actually need to be prepared for file
   switching, then we need a custom TARGET_ASM_NAMED_SECTION too.  */

#undef TEXT_SECTION
#define TEXT_SECTION()				\
do {						\
  if (TARGET_FILE_SWITCHING)			\
    abort ();					\
  fputs (TEXT_SECTION_ASM_OP, asm_out_file);	\
  fputc ('\n', asm_out_file);			\
} while (0)

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define TYPE_OPERAND_FMT        "@%s"

/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#undef TYPE_ASM_OP
#undef SIZE_ASM_OP
#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare an object properly.  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "%s", TYPE_ASM_OP);					\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "object");				\
    putc ('\n', FILE);							\
    size_directive_output = 0;						\
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
      {									\
	size_directive_output = 1;					\
	fprintf (FILE, "%s", SIZE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	fprintf (FILE, ",");						\
	fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,				\
	  int_size_in_bytes (TREE_TYPE (DECL)));			\
	fprintf (FILE, "\n");						\
      }									\
    mips_declare_object (FILE, NAME, "", ":\n", 0);			\
  } while (0)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 fprintf (FILE, "%s", SIZE_ASM_OP);				 \
	 assemble_name (FILE, name);					 \
	 fprintf (FILE, ",");						 \
	 fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,			 \
		  int_size_in_bytes (TREE_TYPE (DECL)));		 \
	 fprintf (FILE, "\n");						 \
       }								 \
   } while (0)

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)                            \
 do { fputc ( '\t', FILE);                                            \
      assemble_name (FILE, LABEL1);                                   \
      fputs ( " = ", FILE);                                           \
      assemble_name (FILE, LABEL2);                                   \
      fputc ( '\n', FILE);                                            \
 } while (0)

/* Note about .weak vs. .weakext
   The mips native assemblers support .weakext, but not .weak.
   mips-elf gas supports .weak, but not .weakext.
   mips-elf gas has been changed to support both .weak and .weakext,
   but until that support is generally available, the 'if' below
   should serve.  */

#undef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME) ASM_OUTPUT_WEAK_ALIAS(FILE,NAME,0)
#define ASM_OUTPUT_WEAK_ALIAS(FILE,NAME,VALUE)	\
 do {						\
  if (TARGET_GAS)                               \
      fputs ("\t.weak\t", FILE);		\
  else                                          \
      fputs ("\t.weakext\t", FILE);		\
  assemble_name (FILE, NAME);			\
  if (VALUE)					\
    {						\
      fputc (' ', FILE);			\
      assemble_name (FILE, VALUE);		\
    }						\
  fputc ('\n', FILE);				\
 } while (0)

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
#undef UNIQUE_SECTION
#define UNIQUE_SECTION(DECL,RELOC)					   \
do {									   \
  int len, size, sec;							   \
  const char *name, *prefix;						   \
  char *string;								   \
  static const char *const prefixes[4][2] = {				   \
    { ".text.", ".gnu.linkonce.t." },					   \
    { ".rodata.", ".gnu.linkonce.r." },					   \
    { ".data.", ".gnu.linkonce.d." },					   \
    { ".sdata.", ".gnu.linkonce.s." }					   \
  };									   \
									   \
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));		   \
  size = int_size_in_bytes (TREE_TYPE (decl));				   \
									   \
  /* Determine the base section we are interested in:			   \
     0=text, 1=rodata, 2=data, 3=sdata, [4=bss].  */			   \
  if (TREE_CODE (DECL) == FUNCTION_DECL)				   \
    sec = 0;								   \
  else if (DECL_INITIAL (DECL) == 0					   \
           || DECL_INITIAL (DECL) == error_mark_node)			   \
    sec = 2;								   \
  else if ((TARGET_EMBEDDED_PIC || TARGET_MIPS16)			   \
      && TREE_CODE (decl) == STRING_CST					   \
      && !flag_writable_strings)					   \
    {									   \
      /* For embedded position independent code, put constant strings	   \
	 in the text section, because the data section is limited to	   \
	 64K in size.  For mips16 code, put strings in the text		   \
	 section so that a PC relative load instruction can be used to	   \
	 get their address.  */						   \
      sec = 0;								   \
    }									   \
  else if (TARGET_EMBEDDED_DATA)					   \
    {									   \
      /* For embedded applications, always put an object in read-only data \
	 if possible, in order to reduce RAM usage.  */			   \
									   \
      if (DECL_READONLY_SECTION (DECL, RELOC))				   \
	sec = 1;							   \
      else if (size > 0 && size <= mips_section_threshold)		   \
	sec = 3;							   \
      else								   \
	sec = 2;							   \
    }									   \
  else									   \
    {									   \
      /* For hosted applications, always put an object in small data if	   \
	 possible, as this gives the best performance.  */		   \
									   \
      if (size > 0 && size <= mips_section_threshold)			   \
	sec = 3;							   \
      else if (DECL_READONLY_SECTION (DECL, RELOC))			   \
	sec = 1;							   \
      else								   \
	sec = 2;							   \
    }									   \
									   \
  prefix = prefixes[sec][DECL_ONE_ONLY (DECL)];				   \
  len = strlen (name) + strlen (prefix);				   \
  string = alloca (len + 1);						   \
  sprintf (string, "%s%s", prefix, name);				   \
									   \
  DECL_SECTION_NAME (DECL) = build_string (len, string);		   \
} while (0)

/* A list of other sections which the compiler might be "in" at any
   given time.  */
#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_rdata

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS                                         \
  SECTION_FUNCTION_TEMPLATE(sdata_section, in_sdata, SDATA_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(rdata_section, in_rdata, RDATA_SECTION_ASM_OP)

#define SECTION_FUNCTION_TEMPLATE(FN, ENUM, OP)                               \
void FN ()                                                            \
{                                                                     \
  if (in_section != ENUM)                                             \
    {                                                                 \
      fprintf (asm_out_file, "%s\n", OP);                             \
      in_section = ENUM;                                              \
    }                                                                 \
}

/* On elf, we *do* have support for the .init and .fini sections, and we
   can put stuff in there to be executed before and after `main'.  We let
   crtstuff.c and other files know this by defining the following symbols.
   The definitions say how to change sections to the .init and .fini
   sections.  This is the same for all known elf assemblers.  */

#undef  INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP     "\t.section\t.init"
#undef  FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP     "\t.section\t.fini"

/* Don't set the target flags, this is done by the linker script */
#undef LIB_SPEC
#define LIB_SPEC ""

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crti%O%s crtbegin%O%s %{!mno-crt0:crt0%O%s}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s crtn%O%s"
