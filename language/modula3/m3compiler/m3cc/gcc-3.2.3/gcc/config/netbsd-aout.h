/* Common configuration file for NetBSD a.out targets.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Wasabi Systems, Inc.

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

/* This defines which switch letters take arguments. */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)		\
  (DEFAULT_SWITCH_TAKES_ARG(CHAR)	\
   || (CHAR) == 'R')


/* Provide an ASM_SPEC appropriate for NetBSD.  Currently we only deal
   with the options for generating PIC code.  */

#undef ASM_SPEC
#define ASM_SPEC " %| %{fpic:-k} %{fPIC:-k -K}"


/* Provide a STARTFILE_SPEC appropriate for NetBSD a.out.  Here we
   provide support for the special GCC option -static.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC			\
  "%{!shared:				\
     %{pg:gcrt0%O%s}			\
     %{!pg:				\
       %{p:mcrt0%O%s}			\
       %{!p:				\
	 %{!static:crt0%O%s}		\
	 %{static:scrt0%O%s}}}}"

/* Provide a LINK_SPEC appropriate for NetBSD a.out.  Here we provide
   support for the special GCC options -static, -assert, and -nostdlib.  */

#undef LINK_SPEC
#define LINK_SPEC			\
  "%{nostdlib:-nostdlib}		\
   %{!shared:				\
     %{!nostdlib:			\
       %{!r*:				\
	 %{!e*:-e start}}}		\
     -dc -dp				\
     %{static:-Bstatic}}		\
   %{shared:-Bshareable}		\
   %{R*}				\
   %{assert*}"


/* Some imports from svr4.h in support of shared libraries.  */

/* Define the strings used for the .type, .size, and .set directives.
   These strings generally do not vary from one system running NetBSD
   to another, but if a given system needs to use different pseudo-op
   names for these, they may be overridden in the file included after
   this one.  */

#undef TYPE_ASM_OP
#undef SIZE_ASM_OP
#undef SET_ASM_OP                
#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"
#define SET_ASM_OP	"\t.set\t"


/* This is how we tell the assembler that a symbol is weak.  */

#undef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME)					\
  do 									\
    {									\
      fputs ("\t.globl\t", FILE); assemble_name (FILE, NAME);		\
      fputc ('\n', FILE);						\
      fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME);		\
      fputc ('\n', FILE);						\
    }									\
  while (0)


/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms of this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending on the particulars of your assembler).  */

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"@%s"


/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif


/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4 (and a.out on NetBSD).
   These macros also output the starting labels for the relevant
   functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do									\
    {									\
      fprintf (FILE, "%s", TYPE_ASM_OP);				\
      assemble_name (FILE, NAME);					\
      putc (',', FILE);							\
      fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
      putc ('\n', FILE);						\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
      ASM_OUTPUT_LABEL(FILE, NAME);					\
    }									\
  while (0)


/* Write the extra assembler code needed to declare an object properly.  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do									\
    {									\
      fprintf (FILE, "%s", TYPE_ASM_OP);				\
      assemble_name (FILE, NAME);					\
      putc (',', FILE);							\
      fprintf (FILE, TYPE_OPERAND_FMT, "object");			\
      putc ('\n', FILE);						\
      size_directive_output = 0;					\
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
	{								\
	  size_directive_output = 1;					\
	  fprintf (FILE, "%s", SIZE_ASM_OP);				\
	  assemble_name (FILE, NAME);					\
	  fprintf (FILE, ",%d\n",					\
	           int_size_in_bytes (TREE_TYPE (DECL)));		\
	}								\
      ASM_OUTPUT_LABEL(FILE, NAME);					\
    }									\
  while (0)


/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	\
  do									\
    {									\
      const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		\
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		\
	  && ! AT_END && TOP_LEVEL					\
	  && DECL_INITIAL (DECL) == error_mark_node			\
	  && !size_directive_output)					\
	{								\
	  size_directive_output = 1;					\
	  fprintf (FILE, "%s", SIZE_ASM_OP);				\
	  assemble_name (FILE, name);					\
	  fprintf (FILE, ",%d\n",					\
		   int_size_in_bytes (TREE_TYPE (DECL)));		\
	}								\
    }									\
  while (0)


/* This is how to declare the size of a function.  */

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do									\
    {									\
      if (!flag_inhibit_size_directive)					\
	{								\
	  char label[256];						\
	  static int labelno;						\
	  labelno++;							\
	  ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
	  ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	  fprintf (FILE, "%s", SIZE_ASM_OP);				\
	  assemble_name (FILE, (FNAME));				\
	  fprintf (FILE, ",");						\
	  assemble_name (FILE, label);					\
	  fprintf (FILE, "-");						\
	  assemble_name (FILE, (FNAME));				\
	  putc ('\n', FILE);						\
	}								\
    }									\
  while (0)
