/* Definitions for SPARC running Linux-based GNU systems with a.out.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Eddie C. Dost (ecd@skynet.be)

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

#include <aoutos.h>
#include <sparc/sparc.h>

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

/* GNU/Linux uses ctype from glibc.a. I am not sure how complete it is.
   For now, we play safe. It may change later. */

#if 0
#undef MULTIBYTE_CHARS
#define MULTIBYTE_CHARS 1
#endif

/* We need that too. */
#define HANDLE_SYSV_PRAGMA

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)                                            \
  do {                                                                  \
        output_file_directive (FILE, main_input_filename);              \
        fprintf (FILE, "\t.version\t\"01.01\"\n");                      \
  } while (0)

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "%{pg:gcrt0.o%s} %{!pg:%{p:gcrt0.o%s} %{!p:crt0.o%s}} %{static:-static}"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc GNU/Linux with a.out)");

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dsparc -Dlinux -Asystem(unix) -Asystem(posix)"

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC \
"%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE}"

/* Don't default to pcc-struct-return, because gcc is the only compiler,
   and we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

#undef LIB_SPEC

#if 1
/* We no longer link with libc_p.a or libg.a by default. If you
   want to profile or debug the GNU/Linux C library, please add
   -lc_p or -ggdb to LDFLAGS at the link time, respectively.  */
#define LIB_SPEC \
"%{mieee-fp:-lieee} %{p:-lgmon} %{pg:-lgmon} %{!ggdb:-lc} %{ggdb:-lg}"
#else    
#define LIB_SPEC \
"%{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
 %{!p:%{!pg:%{!g*:-lc} %{g*:-lg -static}}}" 
#endif

#undef LINK_SPEC
#define LINK_SPEC	"-m sparclinux"

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s %{fpic:-K PIC} %{fPIC:-K PIC}"

#if 0
/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words. GNU/Linux does not support
   long double yet.  */
#define LONG_DOUBLE_TYPE_SIZE 128
#endif

/* Override MACHINE_STATE_{SAVE,RESTORE} because we have special
   traps available which can get and set the condition codes
   reliably.  */
#undef MACHINE_STATE_SAVE
#define MACHINE_STATE_SAVE(ID)				\
  unsigned long int ms_flags, ms_saveret;		\
  asm volatile("ta	0x20\n\t"			\
	       "mov	%%g1, %0\n\t"			\
	       "mov	%%g2, %1\n\t"			\
	       : "=r" (ms_flags), "=r" (ms_saveret));

#undef MACHINE_STATE_RESTORE
#define MACHINE_STATE_RESTORE(ID)			\
  asm volatile("mov	%0, %%g1\n\t"			\
	       "mov	%1, %%g2\n\t"			\
	       "ta	0x21\n\t"			\
	       : /* no outputs */			\
	       : "r" (ms_flags), "r" (ms_saveret));
