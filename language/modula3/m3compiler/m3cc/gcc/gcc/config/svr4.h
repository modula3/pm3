/* Operating system specific defines to be used when targeting GCC for some
   generic System V Release 4 system.
   Copyright (C) 1991, 94-98, 1999 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com).

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
Boston, MA 02111-1307, USA.

   To use this file, make up a file with a name like:

	?????svr4.h

   where ????? is replaced by the name of the basic hardware that you
   are targeting for.  Then, in the file ?????svr4.h, put something
   like:

	#include "?????.h"
	#include "svr4.h"

   followed by any really system-specific defines (or overrides of
   defines) which you find that you need.  For example, CPP_PREDEFINES
   is defined here with only the defined -Dunix and -DSVR4.  You should
   probably override that in your target-specific ?????svr4.h file
   with a set of defines that includes these, but also contains an
   appropriate define for the type of hardware that you are targeting.
*/

/* Define a symbol indicating that we are using svr4.h.  */
#define USING_SVR4_H

/* For the sake of libgcc2.c, indicate target supports atexit.  */
#define HAVE_ATEXIT

/* Cpp, assembler, linker, library, and startfile spec's.  */

/* This defines which switch letters take arguments.  On svr4, most of
   the normal cases (defined in gcc.c) apply, and we also have -h* and
   -z* options (for the linker).  Note however that there is no such
   thing as a -T option for svr4.  */

#define SWITCH_TAKES_ARG(CHAR) \
  (DEFAULT_SWITCH_TAKES_ARG (CHAR) \
   || (CHAR) == 'h' \
   || (CHAR) == 'x' \
   || (CHAR) == 'z')

/* This defines which multi-letter switches take arguments.  On svr4,
   there are no such switches except those implemented by GCC itself.  */

#define WORD_SWITCH_TAKES_ARG(STR)			\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)			\
  && strcmp (STR, "Tdata") && strcmp (STR, "Ttext")	\
  && strcmp (STR, "Tbss"))

/* You should redefine CPP_PREDEFINES in any file which includes this one.
   The definition should be appropriate for the type of target system
   involved, and it should include any -A (assertion) options which are
   appropriate for the given target system.  */
#undef CPP_PREDEFINES

/* Provide an ASM_SPEC appropriate for svr4.  Here we try to support as
   many of the specialized svr4 assembler options as seems reasonable,
   given that there are certain options which we can't (or shouldn't)
   support directly due to the fact that they conflict with other options
   for other svr4 tools (e.g. ld) or with other options for GCC itself.
   For example, we don't support the -o (output file) or -R (remove
   input file) options because GCC already handles these things.  We
   also don't support the -m (run m4) option for the assembler because
   that conflicts with the -m (produce load map) option of the svr4
   linker.  We do however allow passing arbitrary options to the svr4
   assembler via the -Wa, option.

   Note that gcc doesn't allow a space to follow -Y in a -Ym,* or -Yd,*
   option.
*/

#undef ASM_SPEC
#define ASM_SPEC \
  "%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*}"

/* svr4 assemblers need the `-' (indicating input from stdin) to come after
   the -o option (and its argument) for some reason.  If we try to put it
   before the -o option, the assembler will try to read the file named as
   the output file in the -o option as an input file (after it has already
   written some stuff to it) and the binary stuff contained therein will
   cause totally confuse the assembler, resulting in many spurious error
   messages.  */

#undef ASM_FINAL_SPEC
#define ASM_FINAL_SPEC "%|"

/* Under svr4, the normal location of the `ld' and `as' programs is the
   /usr/ccs/bin directory.  */

#ifndef CROSS_COMPILE
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/ccs/bin/"
#endif

/* Under svr4, the normal location of the various *crt*.o files is the
   /usr/ccs/lib directory.  */

#ifndef CROSS_COMPILE
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/ccs/lib/"
#endif

/* Provide a LIB_SPEC appropriate for svr4.  Here we tack on the default
   standard C library (unless we are building a shared library).  */

#undef	LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

/* Provide an ENDFILE_SPEC appropriate for svr4.  Here we tack on our own
   magical crtend.o file (see crtstuff.c) which provides part of the
   support for getting C++ file-scope static object constructed before
   entering `main', followed by the normal svr3/svr4 "finalizer" file,
   which is either `gcrtn.o' or `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %{pg:gcrtn.o%s}%{!pg:crtn.o%s}"

/* Provide a LINK_SPEC appropriate for svr4.  Here we provide support
   for the special GCC options -static, -shared, and -symbolic which
   allow us to link things in one of these three modes by applying the
   appropriate combinations of options at link-time.  We also provide
   support here for as many of the other svr4 linker options as seems
   reasonable, given that some of them conflict with options for other
   svr4 tools (e.g. the assembler).  In particular, we do support the
   -z*, -V, -b, -t, -Qy, -Qn, and -YP* options here, and the -e*,
   -l*, -o*, -r, -s, -u*, and -L* options are directly supported
   by gcc.c itself.  We don't directly support the -m (generate load
   map) option because that conflicts with the -m (run m4) option of
   the svr4 assembler.  We also don't directly support the svr4 linker's
   -I* or -M* options because these conflict with existing GCC options.
   We do however allow passing arbitrary options to the svr4 linker
   via the -Wl, option.  We don't support the svr4 linker's -a option
   at all because it is totally useless and because it conflicts with
   GCC's own -a option.

   Note that gcc doesn't allow a space to follow -Y in a -YP,* option.

   When the -G link option is used (-shared and -symbolic) a final link is
   not being done.  */

#undef	LINK_SPEC
#ifdef CROSS_COMPILE
#define LINK_SPEC "%{h*} %{v:-V} \
		   %{b} %{Wl,*:%*} \
		   %{static:-dn -Bstatic} \
		   %{shared:-G -dy -z text} \
		   %{symbolic:-Bsymbolic -G -dy -z text} \
		   %{G:-G} \
		   %{YP,*} \
		   %{Qy:} %{!Qn:-Qy}"
#else
#define LINK_SPEC "%{h*} %{v:-V} \
		   %{b} %{Wl,*:%*} \
		   %{static:-dn -Bstatic} \
		   %{shared:-G -dy -z text} \
		   %{symbolic:-Bsymbolic -G -dy -z text} \
		   %{G:-G} \
		   %{YP,*} \
		   %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
		    %{!p:-Y P,/usr/ccs/lib:/usr/lib}} \
		   %{Qy:} %{!Qn:-Qy}"
#endif

/* Gcc automatically adds in one of the files /usr/ccs/lib/values-Xc.o,
   /usr/ccs/lib/values-Xa.o, or /usr/ccs/lib/values-Xt.o for each final
   link step (depending upon the other gcc options selected, such as
   -traditional and -ansi).  These files each contain one (initialized)
   copy of a special variable called `_lib_version'.  Each one of these
   files has `_lib_version' initialized to a different (enum) value.
   The SVR4 library routines query the value of `_lib_version' at run
   to decide how they should behave.  Specifically, they decide (based
   upon the value of `_lib_version') if they will act in a strictly ANSI
   conforming manner or not.
*/

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}}}\
			%{pg:gcrti.o%s}%{!pg:crti.o%s} \
			%{ansi:values-Xc.o%s} \
			%{!ansi: \
			 %{traditional:values-Xt.o%s} \
			 %{!traditional:values-Xa.o%s}} \
 			crtbegin.o%s"

/* Attach a special .ident directive to the end of the file to identify
   the version of GCC which compiled this code.  The format of the
   .ident string is patterned after the ones produced by native svr4
   C compilers.  */

#define IDENT_ASM_OP ".ident"

#define ASM_FILE_END(FILE)					\
do {				 				\
     if (!flag_no_ident)					\
	fprintf ((FILE), "\t%s\t\"GCC: (GNU) %s\"\n",		\
		 IDENT_ASM_OP, version_string);			\
   } while (0)

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "\t%s\t\"%s\"\n", IDENT_ASM_OP, NAME);

/* Use periods rather than dollar signs in special g++ assembler names.  */

#define NO_DOLLAR_IN_LABEL

/* Writing `int' for a bitfield forces int alignment for the structure.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Handle #pragma weak and #pragma pack.  */

#define HANDLE_SYSV_PRAGMA

/* System V Release 4 uses DWARF debugging info.  */

#define DWARF_DEBUGGING_INFO

/* All ELF targets can support DWARF-2.  */
#ifndef DWARF2_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO
#endif

/* The numbers used to denote specific machine registers in the System V
   Release 4 DWARF debugging information are quite likely to be totally
   different from the numbers used in BSD stabs debugging information
   for the same kind of target machine.  Thus, we undefine the macro
   DBX_REGISTER_NUMBER here as an extra inducement to get people to
   provide proper machine-specific definitions of DBX_REGISTER_NUMBER
   (which is also used to provide DWARF registers numbers in dwarfout.c)
   in their tm.h files which include this file.  */

#undef DBX_REGISTER_NUMBER

/* Use DWARF debugging info by default.  */

#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
#endif

/* But allow STABS to be supoorted as well.  */
#include "dbxelf.h"

/* Define the actual types of some ANSI-mandated types.  (These
   definitions should work for most SVR4 systems).  */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* This causes trouble, because it requires the host machine
   to support ANSI C.  */
/* #define MULTIBYTE_CHARS */

#undef ASM_BYTE_OP
#define ASM_BYTE_OP	".byte"

#undef SET_ASM_OP
#define SET_ASM_OP	".set"

/* This is how to begin an assembly language file.  Most svr4 assemblers want
   at least a .file directive to come first, and some want to see a .version
   directive come right after that.  Here we just establish a default
   which generates only the .file directive.  If you need a .version
   directive for any specific target, you should override this definition
   in the target-specific file which includes this one.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)                                    \
  output_file_directive ((FILE), main_input_filename)

/* This is how to allocate empty space in some section.  The .zero
   pseudo-op is used for this on most svr4 assemblers.  */

#define SKIP_ASM_OP	".zero"

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE) \
  fprintf (FILE, "\t%s\t%u\n", SKIP_ASM_OP, (SIZE))

/* The prefix to add to user-visible assembler symbols.

   For System V Release 4 the convention is *not* to prepend a leading
   underscore onto user-level symbol names.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.

   For most svr4 systems, the convention is that any symbol which begins
   with a period is not put into the linker symbol table by the assembler.  */

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)			\
do {									\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM);				\
} while (0)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.

   For most svr4 systems, the convention is that any symbol which begins
   with a period is not put into the linker symbol table by the assembler.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  sprintf (LABEL, "*.%s%d", PREFIX, (unsigned) (NUM));			\
} while (0)

/* Output the label which precedes a jumptable.  Note that for all svr4
   systems where we actually generate jumptables (which is to say every
   svr4 target except i386, where we use casesi instead) we put the jump-
   tables into the .rodata section and since other stuff could have been
   put into the .rodata section prior to any given jumptable, we have to
   make sure that the location counter for the .rodata section gets pro-
   perly re-aligned prior to the actual beginning of the jump table.  */

#define ALIGN_ASM_OP ".align"

#ifndef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE) \
  ASM_OUTPUT_ALIGN ((FILE), 2);
#endif

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,JUMPTABLE)		\
  do {									\
    ASM_OUTPUT_BEFORE_CASE_LABEL (FILE, PREFIX, NUM, JUMPTABLE)		\
    ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM);			\
  } while (0)

/* The standard SVR4 assembler seems to require that certain builtin
   library routines (e.g. .udiv) be explicitly declared as .globl
   in each assembly file where they are referenced.  */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)				\
  ASM_GLOBALIZE_LABEL (FILE, XSTR (FUN, 0))

/* This says how to output assembler code to declare an
   uninitialized external linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#define COMMON_ASM_OP	".comm"

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "\t%s\t", COMMON_ASM_OP);				\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
} while (0)

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#define LOCAL_ASM_OP	".local"

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "\t%s\t", LOCAL_ASM_OP);				\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), "\n");						\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */

#define MAX_OFILE_ALIGNMENT (32768*8)

/* This is the pseudo-op used to generate a 32-bit word of data with a
   specific value in some section.  This is the same for all known svr4
   assemblers.  */

#define INT_ASM_OP		".long"

/* This is the pseudo-op used to generate a contiguous sequence of byte
   values from a double-quoted string WITHOUT HAVING A TERMINATING NUL
   AUTOMATICALLY APPENDED.  This is the same for most svr4 assemblers.  */

#undef ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP	".ascii"

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

#define USE_CONST_SECTION	1

#define CONST_SECTION_ASM_OP	".section\t.rodata"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.

   Note that we want to give these sections the SHF_WRITE attribute
   because these sections will actually contain data (i.e. tables of
   addresses of functions in the current root executable or shared library
   file) and, in the case of a shared library, the relocatable addresses
   will have to be properly resolved/relocated (and then written into) by
   the dynamic linker when it actually attaches the given shared library
   to the executing process.  (Note that on SVR4, you may wish to use the
   `-z text' option to the ELF linker, when building a shared library, as
   an additional check that you are doing everything right.  But if you do
   use the `-z text' option when building a shared library, you will get
   errors unless the .ctors and .dtors sections are marked as writable
   via the SHF_WRITE attribute.)  */

#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"aw\""
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"aw\""

/* On svr4, we *do* have support for the .init and .fini sections, and we
   can put stuff in there to be executed before and after `main'.  We let
   crtstuff.c and other files know this by defining the following symbols.
   The definitions say how to change sections to the .init and .fini
   sections.  This is the same for all known svr4 assemblers.  */

#define INIT_SECTION_ASM_OP	".section\t.init"
#define FINI_SECTION_ASM_OP	".section\t.fini"

/* A default list of other sections which we might be "in" at any given
   time.  For targets that use additional sections (e.g. .tdesc) you
   should override this definition in the target-specific file which
   includes this file.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors

/* A default list of extra section function definitions.  For targets
   that use additional sections (e.g. .tdesc) you should override this
   definition in the target-specific file which includes this file.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#define READONLY_DATA_SECTION() const_section ()

extern void text_section ();

#define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  if (!USE_CONST_SECTION)						\
    text_section();							\
  else if (in_section != in_const)					\
    {									\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);		\
      in_section = in_const;						\
    }									\
}

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

/* Switch into a generic section.
 
   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.
 
   If the section has already been defined, we must not
   emit the attributes here. The SVR4 assembler does not
   recognize section redefinitions.
   If DECL is NULL, no attributes are emitted.  */

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC)		\
do {									\
  static struct section_info						\
    {									\
      struct section_info *next;				        \
      char *name;						        \
      enum sect_enum {SECT_RW, SECT_RO, SECT_EXEC} type;		\
    } *sections;							\
  struct section_info *s;						\
  char *mode;								\
  enum sect_enum type;							\
									\
  for (s = sections; s; s = s->next)					\
    if (!strcmp (NAME, s->name))					\
      break;								\
									\
  if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)			\
    type = SECT_EXEC, mode = "ax";					\
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))			\
    type = SECT_RO, mode = "a";						\
  else									\
    type = SECT_RW, mode = "aw";					\
									\
  if (s == 0)								\
    {									\
      s = (struct section_info *) xmalloc (sizeof (struct section_info));  \
      s->name = xmalloc ((strlen (NAME) + 1) * sizeof (*NAME));		\
      strcpy (s->name, NAME);						\
      s->type = type;							\
      s->next = sections;						\
      sections = s;							\
      fprintf (FILE, ".section\t%s,\"%s\",@progbits\n", NAME, mode);	\
    }									\
  else									\
    {									\
      if (DECL && s->type != type)					\
	error_with_decl (DECL, "%s causes a section type conflict");	\
									\
      fprintf (FILE, ".section\t%s\n", NAME);				\
    }									\
} while (0)

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
#define UNIQUE_SECTION_P(DECL) (DECL_ONE_ONLY (DECL))
#define UNIQUE_SECTION(DECL,RELOC)				\
do {								\
  int len;							\
  char *name, *string, *prefix;					\
								\
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
								\
  if (! DECL_ONE_ONLY (DECL))					\
    {								\
      prefix = ".";                                             \
      if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	prefix = ".text.";					\
      else if (DECL_READONLY_SECTION (DECL, RELOC))		\
	prefix = ".rodata.";					\
      else							\
	prefix = ".data.";					\
    }								\
  else if (TREE_CODE (DECL) == FUNCTION_DECL)			\
    prefix = ".gnu.linkonce.t.";				\
  else if (DECL_READONLY_SECTION (DECL, RELOC))			\
    prefix = ".gnu.linkonce.r.";				\
  else								\
    prefix = ".gnu.linkonce.d.";				\
								\
  len = strlen (name) + strlen (prefix);			\
  string = alloca (len + 1);					\
  sprintf (string, "%s%s", prefix, name);			\
								\
  DECL_SECTION_NAME (DECL) = build_string (len, string);	\
} while (0)
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

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

#define SELECT_SECTION(DECL,RELOC)					\
{									\
  if (flag_pic && RELOC)						\
    data_section ();							\
  else if (TREE_CODE (DECL) == STRING_CST)				\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if (! DECL_READONLY_SECTION (DECL, RELOC))			\
	data_section ();						\
      else								\
	const_section ();						\
    }									\
  else									\
    const_section ();							\
}

/* A C statement or statements to switch to the appropriate
   section for output of RTX in mode MODE.  RTX is some kind
   of constant in RTL.  The argument MODE is redundant except
   in the case of a `const_int' rtx.  Currently, these always
   go into the const section.  */

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX) const_section()

/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#define TYPE_ASM_OP	".type"
#define SIZE_ASM_OP	".size"

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define TYPE_OPERAND_FMT	"@%s"

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', FILE);							\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* Write the extra assembler code needed to declare an object properly.  */

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "object");				\
    putc ('\n', FILE);							\
    size_directive_output = 0;						\
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
      {									\
	size_directive_output = 1;					\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	putc (',', FILE);						\
	fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,				\
		 int_size_in_bytes (TREE_TYPE (DECL)));			\
	fputc ('\n', FILE);						\
      }									\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);			 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);			 \
	 assemble_name (FILE, name);					 \
	 putc (',', FILE);						 \
	 fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,			 \
		  int_size_in_bytes (TREE_TYPE (DECL))); 		 \
	fputc ('\n', FILE);						 \
       }								 \
   } while (0)

/* This is how to declare the size of a function.  */

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
        char label[256];						\
	static int labelno;						\
	labelno++;							\
	ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",");						\
	assemble_name (FILE, label);					\
        fprintf (FILE, "-");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }									\
  } while (0)

/* A table of bytes codes used by the ASM_OUTPUT_ASCII and
   ASM_OUTPUT_LIMITED_STRING macros.  Each byte in the table
   corresponds to a particular byte value [0..255].  For any
   given byte value, if the value in the corresponding table
   position is zero, the given character can be output directly.
   If the table value is 1, the byte must be output as a \ooo
   octal escape.  If the tables value is anything else, then the
   byte value should be output as a \ followed by the value
   in the table.  Note that we can use standard UN*X escape
   sequences for many control characters, but we don't use
   \a to represent BEL because some svr4 assemblers (e.g. on
   the i386) don't know about that.  Also, we don't use \v
   since some versions of gas, such as 2.2 did not accept it.  */

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
   should define this to zero.
*/

#define STRING_LIMIT	((unsigned) 256)

#define STRING_ASM_OP	".string"

/* The routine used to output NUL terminated strings.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable, especially for targets like the i386
   (where the only alternative is to output character sequences as
   comma separated lists of numbers).   */

#define ASM_OUTPUT_LIMITED_STRING(FILE, STR)				\
  do									\
    {									\
      register unsigned char *_limited_str = (unsigned char *) (STR);	\
      register unsigned ch;						\
      fprintf ((FILE), "\t%s\t\"", STRING_ASM_OP);			\
      for (; (ch = *_limited_str); _limited_str++)			\
        {								\
	  register int escape;						\
	  switch (escape = ESCAPES[ch])					\
	    {								\
	    case 0:							\
	      putc (ch, (FILE));					\
	      break;							\
	    case 1:							\
	      fprintf ((FILE), "\\%03o", ch);				\
	      break;							\
	    default:							\
	      putc ('\\', (FILE));					\
	      putc (escape, (FILE));					\
	      break;							\
	    }								\
        }								\
      fprintf ((FILE), "\"\n");						\
    }									\
  while (0)

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)				\
  do									\
    {									\
      register unsigned char *_ascii_bytes = (unsigned char *) (STR);	\
      register unsigned char *limit = _ascii_bytes + (LENGTH);		\
      register unsigned bytes_in_chunk = 0;				\
      for (; _ascii_bytes < limit; _ascii_bytes++)			\
        {								\
	  register unsigned char *p;					\
	  if (bytes_in_chunk >= 60)					\
	    {								\
	      fprintf ((FILE), "\"\n");					\
	      bytes_in_chunk = 0;					\
	    }								\
	  for (p = _ascii_bytes; p < limit && *p != '\0'; p++)		\
	    continue;							\
	  if (p < limit && (p - _ascii_bytes) <= (long)STRING_LIMIT)	\
	    {								\
	      if (bytes_in_chunk > 0)					\
		{							\
		  fprintf ((FILE), "\"\n");				\
		  bytes_in_chunk = 0;					\
		}							\
	      ASM_OUTPUT_LIMITED_STRING ((FILE), _ascii_bytes);		\
	      _ascii_bytes = p;						\
	    }								\
	  else								\
	    {								\
	      register int escape;					\
	      register unsigned ch;					\
	      if (bytes_in_chunk == 0)					\
		fprintf ((FILE), "\t%s\t\"", ASCII_DATA_ASM_OP);	\
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
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\"\n");					\
    }									\
  while (0)

/* All SVR4 targets use the ELF object file format.  */
#define OBJECT_FORMAT_ELF
