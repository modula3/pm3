/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
   Contributed by Axis Communications.  Written by Hans-Peter Nilsson.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* After the first "Node:" comment comes all preprocessor directives and
   attached declarations described in the info files, the "Using and
   Porting GCC" manual (uapgcc), in the same order as found in the "Target
   macros" section in the gcc-2.9x CVS edition of 2000-03-17.  FIXME: Not
   really, but needs an update anyway.

   There is no generic copy-of-uapgcc comment, you'll have to see uapgcc
   for that.  If applicable, there is a CRIS-specific comment.  The order
   of macro definitions follow the order in the manual.  Every section in
   the manual (node in the info pages) has an introductory `Node:
   <subchapter>' comment.  If no macros are defined for a section, only
   the section-comment is present.  */

/* This file defines the macros for a.out that are not covered by cris.h.
   Many macros are copied from elfos.h and should be in some generic
   config/gas-aout.h.  */

/* Node: Driver */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
 "%{melinux:crt0.o%s}\
  %{!melinux:\
   %{sim2:s2crt0.o%s}\
   %{!sim2:\
    %{sim:scrt0.o%s}\
    %{!sim:%{pg:gcrt0.o%s}\
     %{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}}}"

/* Which library to get.  The only difference from the default is to get
   libsc.a if -sim is given to the driver.  Repeat -lc -lsysX
   {X=sim,linux}, because libsysX needs (at least) errno from libc, and
   then we want to resolve new unknowns in libc against libsysX, not
   libnosys.  Assume everything is in libc for -mlinux.  */
#undef LIB_SPEC
#define LIB_SPEC \
 "%{melinux:-lc -lsyslinux -lc -lsyslinux -lic}\
  %{!melinux:\
   %{sim*:-lc -lsyssim -lc -lsyssim}\
   %{!sim*:%{g*:-lg}\
     %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} -lbsp}\
   -lnosys}"

#undef CRIS_CPP_SUBTARGET_SPEC
#define CRIS_CPP_SUBTARGET_SPEC \
 "-D__AOUT__\
  %{melinux:-D__gnu_linux__ -D__linux__ -D__unix__ -D__elinux__ -D__uclinux__\
    %{!nostdinc:\
      %{!mbest-lib-options:%{isystem*}}\
      -isystem elinux/include%s\
      %{mbest-lib-options:%{isystem*}}}\
    %{!ansi:%{!std=*:%{!undef:-Dlinux -Dunix -Delinux -Duclinux}}}}\
  %{mbest-lib-options:\
   %{!moverride-best-lib-options:\
    %{!march=*:%{!metrax*:%{!mcpu=*:-D__tune_v8 -D__CRIS_arch_tune=8}}}}}"

#undef CRIS_CC1_SUBTARGET_SPEC
#define CRIS_CC1_SUBTARGET_SPEC \
 "%{mbest-lib-options:\
   %{!moverride-best-lib-options:\
    %{!march=*:%{!mcpu=*:-mtune=v8}}}}"

#undef CRIS_ASM_SUBTARGET_SPEC
#define CRIS_ASM_SUBTARGET_SPEC "--em=crisaout"

#undef CRIS_LINK_SUBTARGET_SPEC
#define CRIS_LINK_SUBTARGET_SPEC \
 "-mcrisaout\
  %{sim2:%{!T*:-Tdata 0x4000000 -Tbss 0x8000000}}\
  %{melinux:-Ur -d\
   %{!shlib:%{!symbolic:-Bstatic}}\
   %{shlib:-Bdynamic}\
   %{symbolic:-Bdynamic}\
   %{static:-Bstatic}}\
  %{melinux-stacksize=*:-defsym __Stacksize=%*}"

#undef CRIS_SUBTARGET_SWITCHES
#define CRIS_SUBTARGET_SWITCHES						\
  {"elinux", (TARGET_MASK_SVINTO					\
	      + TARGET_MASK_STACK_ALIGN					\
	      + TARGET_MASK_CONST_ALIGN					\
	      + TARGET_MASK_DATA_ALIGN					\
	      + TARGET_MASK_ETRAX4_ADD					\
	      + TARGET_MASK_ALIGN_BY_32),				\
   N_("Compile for the MMU-less Etrax 100-based elinux system")},	\
  /* Legacy option.  */							\
  {"aout",   0,	""},

#undef CRIS_SUBTARGET_LONG_OPTIONS
#define CRIS_SUBTARGET_LONG_OPTIONS \
  {"elinux-stacksize=", &cris_elinux_stacksize_str,			\
   N_("For elinux, request a specified stack-size for this program")},	\

#undef CRIS_SUBTARGET_VERSION
#define CRIS_SUBTARGET_VERSION " - a.out"

#undef CRIS_SUBTARGET_DEFAULT
#define CRIS_SUBTARGET_DEFAULT 0

/* Node: Storage Layout */

/* We can align to 16 bits (only) with CRIS a.out.  */
#define MAX_OFILE_ALIGNMENT 16


/* Node: Library Calls */

#define TARGET_MEM_FUNCTIONS


/* Node: Data Output */

#define ESCAPES \
"\1\1\1\1\1\1\1\1btn\1fr\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\0\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"

/* Some svr4 assemblers have a limit on the number of characters which
   can appear in the operand of a .string directive.  If your assembler
   has such a limitation, you should define STRING_LIMIT to reflect that
   limit.  Note that at least some svr4 assemblers have a limit on the
   actual number of bytes in the double-quoted string, and that they
   count each character in an escape sequence as one byte.  Thus, an
   escape sequence like \377 would count as four bytes.

   If your target assembler doesn't support the .string directive, you
   should define this to zero.  */

#define STRING_LIMIT	((unsigned) 256)

#define STRING_ASM_OP	"\t.string\t"
#define ASCII_DATA_ASM_OP	"\t.ascii\t"
#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"
#define TYPE_OPERAND_FMT	"@%s"

/* The routine used to output NUL terminated strings.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable, especially for targets like the i386
   (where the only alternative is to output character sequences as
   comma separated lists of numbers).  */

#define ASM_OUTPUT_LIMITED_STRING(FILE, STR)		\
  do							\
    {							\
      register const unsigned char *_limited_str =	\
	(const unsigned char *) (STR);			\
      register unsigned ch;				\
      							\
      fprintf ((FILE), "%s\"", STRING_ASM_OP);		\
      							\
      for (; (ch = *_limited_str); _limited_str++)	\
        {						\
	  register int escape;				\
	  						\
	  switch (escape = ESCAPES[ch])			\
	    {						\
	    case 0:					\
	      putc (ch, (FILE));			\
	      break;					\
	    case 1:					\
	      fprintf ((FILE), "\\%03o", ch);		\
	      break;					\
	    default:					\
	      putc ('\\', (FILE));			\
	      putc (escape, (FILE));			\
	      break;					\
	    }						\
        }						\
      							\
      fprintf ((FILE), "\"\n");				\
    }							\
  while (0)

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

#undef  ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)				\
  do									\
    {									\
      register const unsigned char *_ascii_bytes =			\
	(const unsigned char *) (STR);					\
      register const unsigned char *limit = _ascii_bytes + (LENGTH);	\
      register unsigned bytes_in_chunk = 0;				\
									\
      for (; _ascii_bytes < limit; _ascii_bytes++)			\
        {								\
	  register const unsigned char *p;				\
      									\
	  if (bytes_in_chunk >= 60)					\
	    {								\
	      fprintf ((FILE), "\"\n");					\
	      bytes_in_chunk = 0;					\
	    }								\
      									\
	  for (p = _ascii_bytes; p < limit && *p != '\0'; p++)		\
	    continue;							\
      									\
	  if (p < limit && (p - _ascii_bytes) <= (long)STRING_LIMIT)	\
	    {								\
	      if (bytes_in_chunk > 0)					\
		{							\
		  fprintf ((FILE), "\"\n");				\
		  bytes_in_chunk = 0;					\
		}							\
      									\
	      ASM_OUTPUT_LIMITED_STRING ((FILE), _ascii_bytes);		\
	      _ascii_bytes = p;						\
	    }								\
	  else								\
	    {								\
	      register int escape;					\
	      register unsigned ch;					\
      									\
	      if (bytes_in_chunk == 0)					\
		fprintf ((FILE), "%s\"", ASCII_DATA_ASM_OP);		\
      									\
	      switch (escape = ESCAPES[ch = *_ascii_bytes])		\
		{							\
		case 0:							\
		  putc (ch, (FILE));					\
		  bytes_in_chunk++;					\
		  break;						\
		case 1:							\
		  fprintf ((FILE), "\\%03o", ch);			\
		  bytes_in_chunk += 4;					\
		  break;						\
		default:						\
		  putc ('\\', (FILE));					\
		  putc (escape, (FILE));				\
		  bytes_in_chunk += 2;					\
		  break;						\
		}							\
	    }								\
	}								\
      									\
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\"\n");					\
    }									\
  while (0)


/* Node: Label Output */

#define SET_ASM_OP	"\t.set\t"

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)	\
  ASM_GLOBALIZE_LABEL (FILE, XSTR (FUN, 0))

#define ASM_WEAKEN_LABEL(FILE, NAME) 	\
  do					\
    {					\
      fputs ("\t.weak\t", (FILE));	\
      assemble_name ((FILE), (NAME)); 	\
      fputc ('\n', (FILE));		\
    }					\
  while (0)

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
  do							\
    {							\
      fprintf (FILE, "%s", TYPE_ASM_OP);		\
      assemble_name (FILE, NAME);			\
      putc (',', FILE);					\
      fprintf (FILE, TYPE_OPERAND_FMT, "function");	\
      putc ('\n', FILE);				\
      							\
      ASM_OUTPUT_LABEL(FILE, NAME);			\
    }							\
  while (0)

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      fprintf (FILE, "%s", TYPE_ASM_OP);			\
      assemble_name (FILE, NAME);				\
      putc (',', FILE);						\
      fprintf (FILE, TYPE_OPERAND_FMT, "object");		\
      putc ('\n', FILE);					\
      								\
      size_directive_output = 0;				\
      								\
      if (!flag_inhibit_size_directive				\
	  && (DECL) && DECL_SIZE (DECL))			\
	{							\
	  size_directive_output = 1;				\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, NAME);				\
	  putc (',', FILE);					\
	  fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,		\
		   int_size_in_bytes (TREE_TYPE (DECL)));	\
	  fputc ('\n', FILE);					\
	}							\
      								\
      ASM_OUTPUT_LABEL (FILE, NAME);				\
    }								\
  while (0)

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)\
  do								\
    {								\
      const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);	\
      								\
      if (!flag_inhibit_size_directive				\
	  && DECL_SIZE (DECL)					\
	  && ! AT_END && TOP_LEVEL				\
	  && DECL_INITIAL (DECL) == error_mark_node		\
	  && !size_directive_output)				\
	{							\
	  size_directive_output = 1;				\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, name);				\
	  putc (',', FILE);					\
	  fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,		\
		   int_size_in_bytes (TREE_TYPE (DECL))); 	\
	  fputc ('\n', FILE);					\
	}							\
    }								\
  while (0)

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      if (!flag_inhibit_size_directive)				\
	{							\
	  char label[256];					\
	  static int labelno;					\
	  							\
	  labelno++;						\
	  							\
	  ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);	\
	  ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);	\
	  							\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, (FNAME));			\
	  fprintf (FILE, ",");					\
	  assemble_name (FILE, label);				\
	  fprintf (FILE, "-");					\
	  assemble_name (FILE, (FNAME));			\
	  putc ('\n', FILE);					\
	}							\
    }								\
  while (0)


/* Node: Alignment Output */

#define SKIP_ASM_OP	"\t.zero\t"

#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE, SIZE) \
  fprintf (FILE, "%s%u\n", SKIP_ASM_OP, (SIZE))

/* Node: All Debuggers */

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG


/* Node: Misc */

#define HANDLE_SYSV_PRAGMA

/* In theory, this one isn't necessary, but over time, external tools have
   been primed on names with "." rather than "$".  */
#define NO_DOLLAR_IN_LABEL

/* These are undocumented, but to keep a single
   CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON, we set this to an asm that will
   emit an error if ever output.  It will not be emitted for a.out modulo
   careless hacking.  */
#define COMMON_ASM_OP	"\t.err\t"
#define LOCAL_ASM_OP	"\t.err\t"

#if defined(__CRIS__) && defined (__AOUT__) && defined (IN_GCC)

#define CRIS_ABI_VERSION_SYMBOL_STRING ".$CRIS_ABI_V2"

/* Make all a.out library functions have undefined references to the
   .$CRIS_ABI_V2 symbol, so it will be picked up.  Used by GDB.  GDB has
   a bug with reading a.out symbols; it does not see the GNU weak
   extensions, so we can't have .$CRIS_ABI_V2 weak.  Weak.  */
__asm__ (".set .$abi_referer," CRIS_ABI_VERSION_SYMBOL_STRING);
#endif

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
