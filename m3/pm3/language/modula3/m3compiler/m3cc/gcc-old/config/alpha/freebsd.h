/*
 * Minimal alpha/freebsd.h file for compiling m3cgc1.
 *
 * This file is in the public domain.
 */

#include "alpha/alpha.h"
#include "alpha/elf.h"

#undef	WCHAR_TYPE
#define	WCHAR_TYPE "int"

#undef	WCHAR_TYPE_SIZE
#define	WCHAR_TYPE_SIZE 32

#undef ASM_SPEC
#define ASM_SPEC " %|"

#undef ASM_FINAL_SPEC

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)			\
	fputs ("\tjsr $28,_mcount\n", (FILE)); /* at */

#define TARGET_PROFILING_NEEDS_GP

#define bsd4_4
#undef HAS_INIT_SECTION

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_FP|MASK_FPREGS|MASK_IEEE|MASK_IEEE_CONFORMANT

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (FreeBSD/alpha ELF)");

#undef SDB_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO
#undef DBS_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE  \
 ((len > 1 && !strncmp (str, "gsdb", len)) ? SDB_DEBUG : DBX_DEBUG)

#define HANDLE_SYSV_PRAGMA

#undef SET_ASM_OP
#define SET_ASM_OP	".set"
