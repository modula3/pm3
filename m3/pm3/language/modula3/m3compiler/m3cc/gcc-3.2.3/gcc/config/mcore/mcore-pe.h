/* Definitions of target machine for GNU compiler, for MCore using COFF/PE.
   Copyright (C) 1994, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).

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

/* Run-time Target Specification.  */
#define TARGET_VERSION fputs (" (MCORE/pe)", stderr)

#define SUBTARGET_CPP_PREDEFINES " -D__pe__"

/* The MCore ABI says that bitfields are unsigned by default.  */
/* The EPOC C++ environment does not support exceptions.  */
#define CC1_SPEC "-funsigned-bitfields %{!DIN_GCC:-fno-rtti} %{!DIN_GCC:-fno-exceptions}"

#include "svr3.h"
#include "mcore/mcore.h"
#include "dbxcoff.h"

#undef  SDB_DEBUGGING_INFO
#undef  DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO 1

/* Computed in toplev.c.  */
#undef  PREFERRED_DEBUGGING_TYPE

/* Lay out additional 'sections' where we place things like code
   and readonly data. This gets them out of default places.  */

#define SUBTARGET_SWITCH_SECTIONS 		\
  case in_drectve: drectve_section (); break;	\
  case in_rdata:   rdata_section (); break;

#define DRECTVE_SECTION_ASM_OP	"\t.section .drectve"
#define RDATA_SECTION_ASM_OP	"\t.section .rdata"

#define SUBTARGET_EXTRA_SECTIONS in_drectve, in_rdata

#define SUBTARGET_EXTRA_SECTION_FUNCTIONS \
  DRECTVE_SECTION_FUNCTION		  \
  RDATA_SECTION_FUNCTION

#define DRECTVE_SECTION_FUNCTION 				\
void								\
drectve_section ()						\
{								\
  if (in_section != in_drectve)					\
    {								\
      fprintf (asm_out_file, "%s\n", DRECTVE_SECTION_ASM_OP);	\
      in_section = in_drectve;					\
    }								\
}

#define RDATA_SECTION_FUNCTION 					\
void								\
rdata_section ()						\
{								\
  if (in_section != in_rdata)					\
    {								\
      fprintf (asm_out_file, "%s\n", RDATA_SECTION_ASM_OP);	\
      in_section = in_rdata;					\
    }								\
}

#undef  READONLY_DATA_SECTION
#define READONLY_DATA_SECTION() rdata_section ()

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */
#undef  SELECT_SECTION
#define SELECT_SECTION(DECL, RELOC, ALIGN)				\
{									\
  if (TREE_CODE (DECL) == STRING_CST)					\
    {									\
      if (! flag_writable_strings)					\
	rdata_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if ((0 && RELOC)	/* should be (flag_pic && RELOC) */		\
	  || !TREE_READONLY (DECL) || TREE_SIDE_EFFECTS (DECL)		\
	  || !DECL_INITIAL (DECL)					\
	  || (DECL_INITIAL (DECL) != error_mark_node			\
	      && !TREE_CONSTANT (DECL_INITIAL (DECL))))			\
	data_section ();						\
      else								\
	rdata_section ();						\
    }									\
  else									\
    rdata_section ();							\
}

/* A C statement or statements to switch to the appropriate
   section for output of RTX in mode MODE.  RTX is some kind
   of constant in RTL.  The argument MODE is redundant except
   in the case of a `const_int' rtx.  Currently, these always
   go into the const section.  */
#undef  SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, RTX, ALIGN) rdata_section ()

#define MCORE_EXPORT_NAME(STREAM, NAME)			\
  do							\
    {							\
      drectve_section ();				\
      fprintf (STREAM, "\t.ascii \" -export:%s\"\n",	\
	       MCORE_STRIP_NAME_ENCODING (NAME));	\
    }							\
  while (0);

/* Output the label for an initialized variable.  */
#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)		\
  do								\
    {								\
      if (mcore_dllexport_name_p (NAME))			\
        {							\
          enum in_section save_section = in_section;		\
	  MCORE_EXPORT_NAME (STREAM, NAME);			\
          switch_to_section (save_section, (DECL));		\
        }							\
      ASM_OUTPUT_LABEL ((STREAM), (NAME));			\
    }								\
  while (0)

/* Output a function label definition.  */
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)		\
  do								\
    {								\
      if (mcore_dllexport_name_p (NAME))			\
	{							\
          MCORE_EXPORT_NAME (STREAM, NAME);			\
	  function_section (DECL);				\
	}							\
      ASM_OUTPUT_LABEL ((STREAM), (NAME));			\
    }								\
  while (0);

#undef  ASM_FILE_START
#define ASM_FILE_START(STREAM)					\
  do								\
    {								\
      fprintf (STREAM, "%s Generated by gcc %s for MCore/pe\n",	\
	   ASM_COMMENT_START, version_string);			\
      output_file_directive ((STREAM), main_input_filename);	\
    }								\
  while (0)

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)				  \
  {									  \
    if (write_symbols == DBX_DEBUG)					  \
      {									  \
        static int sym_lineno = 1;					  \
        char buffer[256];						  \
									  \
        ASM_GENERATE_INTERNAL_LABEL (buffer, "LM", sym_lineno);		  \
        fprintf (FILE, ".stabn 68,0,%d,", LINE);			  \
        assemble_name (FILE, buffer);					  \
        putc ('-', FILE);						  \
        assemble_name (FILE,						  \
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
        putc ('\n', FILE);						  \
        ASM_OUTPUT_INTERNAL_LABEL (FILE, "LM", sym_lineno);		  \
        sym_lineno ++;							  \
      }									  \
  }

#define STARTFILE_SPEC "crt0.o%s"
#define ENDFILE_SPEC  "%{!mno-lsim:-lsim}"

/* __CTOR_LIST__ and __DTOR_LIST__ must be defined by the linker script.  */
#define CTOR_LISTS_DEFINED_EXTERNALLY

#undef DO_GLOBAL_CTORS_BODY
#undef DO_GLOBAL_DTORS_BODY
#undef INIT_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#define SUPPORTS_ONE_ONLY 1

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  default_pe_asm_named_section
