/* CYGNUS LOCAL (entire file) m68k-elf */
/* m68kelf support, derived from m68kv4.h */

/* Target definitions for GNU compiler for mc680x0 running System V.4
   Copyright (C) 1991, 1993 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@netcom.com) and Fred Fish (fnf@cygnus.com).

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

/* #notinclude "m68k/sgs.h"	/* The m68k/SVR4 assembler is SGS based */

#ifndef SWBEG_ASM_OP
#define SWBEG_ASM_OP ".swbeg"
#endif

/* Here are four prefixes that are used by asm_fprintf to
   facilitate customization for alternate assembler syntaxes.
   Machines with no likelihood of an alternate syntax need not
   define these and need not use asm_fprintf.  */

/* The prefix for register names.  Note that REGISTER_NAMES
   is supposed to include this prefix. Also note that this is NOT an
   fprintf format string, it is a literal string */

#undef REGISTER_PREFIX
#define REGISTER_PREFIX "%"

/* The prefix for local (compiler generated) labels.
   These labels will not appear in the symbol table. */

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* The prefix to add to user-visible assembler symbols. */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* The prefix for immediate operands.  */

#undef IMMEDIATE_PREFIX
#define IMMEDIATE_PREFIX "&"

/* In the machine description we can't use %R, because it will not be seen
   by ASM_FPRINTF.  (Isn't that a design bug?).  */

#undef REGISTER_PREFIX_MD
#define REGISTER_PREFIX_MD "%%"

/* config/m68k.md has an explicit reference to the program counter,
   prefix this by the register prefix.  */

#define ASM_RETURN_CASE_JUMP    return "jmp %%pc@(2,%0:w)"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number.
   Motorola format uses different register names than defined in m68k.h.
   We also take this chance to convert 'a6' to 'fp' */

#undef REGISTER_NAMES

#define REGISTER_NAMES \
{"%d0",   "%d1",   "%d2",   "%d3",   "%d4",   "%d5",   "%d6",   "%d7",	     \
 "%a0",   "%a1",   "%a2",   "%a3",   "%a4",   "%a5",   "%fp",   "%sp",	     \
 "%fp0",  "%fp1",  "%fp2",  "%fp3",  "%fp4",  "%fp5",  "%fp6",  "%fp7" }

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)				\
  if ((LOG) > 0)						\
    fprintf ((FILE), "\t%s \t%u\n", ALIGN_ASM_OP, 1 << (LOG));	\
  else if ((LOG) > 31)						\
    abort ();

/* Use proper assembler syntax for these macros.  */
#undef ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  asm_fprintf (FILE, "\t%Omove.l %s,-(%Rsp)\n", reg_names[REGNO])

#undef ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  asm_fprintf (FILE, "\t%Omove.l (%Rsp)+,%s\n", reg_names[REGNO])

/*  Override the definition of NO_DOLLAR_IN_LABEL in svr4.h, for special
    g++ assembler names.  When this is defined, g++ uses embedded '.'
    characters and some m68k assemblers have problems with this.  The
    chances are much greater that any particular assembler will permit
    embedded '$' characters. */

#undef NO_DOLLAR_IN_LABEL

/* Define PCC_STATIC_STRUCT_RETURN if the convention on the target machine
   is to use the nonreentrant technique for returning structure and union
   values, as commonly implemented by the AT&T Portable C Compiler (PCC).
   When defined, the gcc option -fpcc-struct-return can be used to cause
   this form to be generated.  When undefined, the option does nothing.
   For m68k SVR4, the convention is to use a reentrant technique compatible
   with the gcc default, so override the definition of this macro in m68k.h */

#undef PCC_STATIC_STRUCT_RETURN

/* Local common symbols are declared to the assembler with ".lcomm" rather
   than ".bss", so override the definition in svr4.h */

#undef BSS_ASM_OP
#define BSS_ASM_OP	".lcomm"

/* Register in which address to store a structure value is passed to a
   function.  The default in m68k.h is a1.  For m68k/SVR4 it is a0. */

#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE_REGNUM 8

#define ASM_COMMENT_START "#"

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT      "@%s"

/* Define how the m68k registers should be numbered for Dwarf output.
   The numbering provided here should be compatible with the native
   SVR4 SDB debugger in the m68k/SVR4 reference port, where d0-d7
   are 0-7, a0-a8 are 8-15, and fp0-fp7 are 16-23. */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* The ASM_OUTPUT_SKIP macro is first defined in m68k.h, using ".skip".
   It is then overridden by m68k/sgs.h to use ".space", and again by svr4.h
   to use ".zero".  The m68k/SVR4 assembler uses ".space", so repeat the
   definition from m68k/sgs.h here.  Note that ASM_NO_SKIP_IN_TEXT is
   defined in m68k/sgs.h, so we don't have to repeat it here. */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t%s %u\n", SPACE_ASM_OP, (SIZE))

/* 1 if N is a possible register number for a function value.
   For m68k/SVR4 allow d0, a0, or fp0 as return registers, for integral,
   pointer, or floating types, respectively. Reject fp0 if not using a
   68881 coprocessor. */

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 0 || (N) == 8 || (TARGET_68881 && (N) == 16))

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1

/* Define how to generate (in the callee) the output value of a function
   and how to find (in the caller) the value returned by a function.  VALTYPE
   is the data type of the value (as a tree).  If the precise function being
   called is known, FUNC is its FUNCTION_DECL; otherwise, FUNC is 0.
   For m68k/SVR4 generate the result in d0, a0, or fp0 as appropriate. */
   
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_68881			\
   ? gen_rtx (REG, TYPE_MODE (VALTYPE), 16)				\
   : (TREE_CODE (VALTYPE) == POINTER_TYPE				\
      ? gen_rtx (REG, TYPE_MODE (VALTYPE), 8)				\
      : gen_rtx (REG, TYPE_MODE (VALTYPE), 0)))

/* For compatibility with the large body of existing code which does not
   always properly declare external functions returning pointer types, the
   m68k/SVR4 convention is to copy the value returned for pointer functions
   from a0 to d0 in the function epilogue, so that callers that have
   neglected to properly declare the callee can still find the correct return
   value. */

extern int current_function_returns_pointer;
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)				\
do {									\
  if ((current_function_returns_pointer) && 				\
      ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))	\
    asm_fprintf (FILE, "\tmov.l %Ra0,%Rd0\n");				\
} while (0);

/* Define how to find the value returned by a library function assuming the
   value has mode MODE.
   For m68k/SVR4 look for integer values in d0, pointer values in d0
   (returned in both d0 and a0), and floating values in fp0. */

#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)						\
  (((MODE) == SFmode || (MODE) == DFmode) && TARGET_68881		\
   ? gen_rtx (REG, (MODE), 16)						\
   : gen_rtx (REG, (MODE), 0))

/* Boundary (in *bits*) on which stack pointer should be aligned.
   The m68k/SVR4 convention is to keep the stack pointer longword aligned. */
 
#undef STACK_BOUNDARY
#define STACK_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.
   For m68k/SVR4, this is the next longword boundary. */

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 32

/* No data type wants to be aligned rounder than this.
   For m68k/SVR4, some types (doubles for example) are aligned on 8 byte
   boundaries */
	
#undef BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 64

#if 0
/* SVR4 m68k assembler is bitching on the `comm i,1,1' which askes for 
   1 byte alignment. Don't generate alignment for COMMON seems to be
   safer until we the assembler is fixed. */
#undef ASM_OUTPUT_ALIGNED_COMMON
/* Same problem with this one.  */
#undef ASM_OUTPUT_ALIGNED_LOCAL
#endif

/* The `string' directive on m68k svr4 does not handle string with
   escape char (ie., `\') right. Use normal way to output ASCII bytes
   seems to be safer. */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)				\
do {								\
  register int sp = 0, lp = 0, ch;				\
  fprintf ((FILE), "\t%s ", BYTE_ASM_OP);			\
  do {								\
    ch = (PTR)[sp];						\
    if (ch > ' ' && ! (ch & 0x80) && ch != '\\')		\
      {								\
	fprintf ((FILE), "'%c", ch);				\
      }								\
    else							\
      {								\
	fprintf ((FILE), "0x%x", ch);				\
      }								\
    if (++sp < (LEN))						\
      {								\
	if ((sp % 10) == 0)					\
	  {							\
	    fprintf ((FILE), "\n\t%s ", BYTE_ASM_OP);		\
	  }							\
	else							\
	  {							\
	    putc (',', (FILE));					\
	  }							\
      }								\
  } while (sp < (LEN));						\
  putc ('\n', (FILE));						\
} while (0)

/* SVR4 m68k assembler is bitching on the syntax `2.b'.
   So use the "LLDnnn-LLnnn" format.  Define LLDnnn after the table.  */

#undef ASM_OUTPUT_CASE_END
#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE)				\
do {									\
  if (switch_table_difference_label_flag)				\
    asm_fprintf ((FILE), "\t%s %LLD%d,%LL%d\n", SET_ASM_OP, (NUM), (NUM));\
  switch_table_difference_label_flag = 0;				\
} while (0)

extern int switch_table_difference_label_flag;

#undef ASM_OUTPUT_COMMON
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))

/* Currently, JUMP_TABLES_IN_TEXT_SECTION must be defined in order to
   keep switch tables in the text section. */
   
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Override the definition in svr4.h. In m68k svr4, using swbeg is the 
   standard way to do switch table. */
#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE)		\
  fprintf ((FILE), "\t%s &%d\n", SWBEG_ASM_OP, XVECLEN (PATTERN (TABLE), 1));

/* In m68k svr4, a symbol_ref rtx can be a valid PIC operand if it is an
   operand of a function call. */
#undef LEGITIMATE_PIC_OPERAND_P
#define LEGITIMATE_PIC_OPERAND_P(X) \
  (! symbolic_operand (X, VOIDmode) \
   || ((GET_CODE(X) == SYMBOL_REF) && SYMBOL_REF_FLAG(X)))

/* Turn off function cse if we are doing PIC. We always want function call
   to be done as `bsr foo@PLTPC', so it will force the assembler to create 
   the PLT entry for `foo'. Doing function cse will cause the address of `foo'
   to be loaded into a register, which is exactly what we want to avoid when
   we are doing PIC on svr4 m68k. */
#undef OVERRIDE_OPTIONS
#define OVERRIDE_OPTIONS		\
{					\
  if (flag_pic) flag_no_function_cse = 1; \
  if (! TARGET_68020 && flag_pic == 2)	\
    error("-fPIC is not currently supported on the 68000 or 68010\n");	\
}
/* end of stuff from m68kv4.h */

#undef SGS_CMP_ORDER

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "crtbegin.o%s"

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME) \
do {									\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)			\
    fprintf (FILE, ".section\t%s,\"ax\"\n", (NAME));			\
  else if ((DECL) && TREE_READONLY (DECL))				\
    fprintf (FILE, ".section\t%s,\"a\"\n", (NAME));			\
  else									\
    fprintf (FILE, ".section\t%s,\"aw\"\n", (NAME));			\
} while (0)
