/* This is tested by i386gas.h.  */
#define NO_UNDERSCORES

#include <i386/gstabs.h>

/* Get perform_* macros to build libgcc.a.  */
#include <i386/perform.h>

/* Get generic NetBSD definitions.  */
#include <netbsd.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -D__NetBSD__ -Asystem(unix) -Asystem(NetBSD) -Acpu(i386) -Amachine(i386)"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* There are conflicting reports about whether this system uses
   a different assembler syntax.  wilson@cygnus.com says # is right.  */
#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"
#undef COMMENT_BEGIN
#define COMMENT_BEGIN "#"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1 << (LOG))

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*.%s%d", PREFIX, NUM)

/* The following macros are stolen from i386v4.h */
/* These have to be defined to get PIC code correct */

/* This is how to output an element of a case-vector that is relative.
   This is only used for PIC code.  See comments by the `casesi' insn in
   i386.md for an explanation of the expression this outputs. */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", LPREFIX, VALUE)

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */

#define JUMP_TABLES_IN_TEXT_SECTION

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Profiling routines, partially copied from i386/osfrose.h.  */

/* Redefine this to use %eax instead of %edx.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tcall mcount@PLT\n");				\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tcall mcount\n");				\
    }									\
}
