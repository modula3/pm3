/* Definitions of target machine for GNU compiler, for DEC Alpha on OSF/1.
   Copyright (C) 1992, 93, 94, 95, 96, 97, 1998 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

/* As of OSF 4.0, as can subtract adjacent labels.  */

#undef TARGET_AS_CAN_SUBTRACT_LABELS
#define TARGET_AS_CAN_SUBTRACT_LABELS 1

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "\
-Dunix -D__osf__ -D_LONGLONG -DSYSTYPE_BSD \
-D_SYSTYPE_BSD -Asystem(unix) -Asystem(xpg4)"

/* Accept DEC C flags for multithreaded programs.  We use _PTHREAD_USE_D4
   instead of PTHREAD_USE_D4 since both have the same effect and the former
   doesn't invade the users' namespace.  */

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC \
"%{pthread|threads:-D_REENTRANT} %{threads:-D_PTHREAD_USE_D4}"

/* Under OSF4, -p and -pg require -lprof1, and -lprof1 requires -lpdf.  */

#define LIB_SPEC \
"%{p|pg:-lprof1%{pthread|threads:_r} -lpdf} %{a:-lprof2} \
 %{threads: -lpthreads} %{pthread|threads: -lpthread -lmach -lexc} -lc"

/* Pass "-G 8" to ld because Alpha's CC does.  Pass -O3 if we are
   optimizing, -O1 if we are not.  Pass -shared, -non_shared or
   -call_shared as appropriate.  Also pass -pg.  */
#define LINK_SPEC  \
  "-G 8 %{O*:-O3} %{!O*:-O1} %{static:-non_shared} \
   %{!static:%{shared:-shared} %{!shared:-call_shared}} %{pg} %{taso} \
   %{rpath*}"

#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}"

#define MD_STARTFILE_PREFIX "/usr/lib/cmplrs/cc/"

#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");                          \
  fprintf (FILE, "\t.set noat\n");				\
  if (TARGET_SUPPORT_ARCH)					\
    fprintf (FILE, "\t.arch %s\n",				\
             alpha_cpu == PROCESSOR_EV6 ? "ev6"			\
	     : (alpha_cpu == PROCESSOR_EV5			\
		? (TARGET_MAX ? "pca56" : TARGET_BWX ? "ev56" : "ev5") \
		: "ev4"));					\
								\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

/* No point in running CPP on our assembler output.  */
#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_GAS) != 0
/* Don't pass -g to GNU as, because some versions don't accept this option.  */
#define ASM_SPEC "%{malpha-as:-g} -nocpp %{pg}"
#else
/* In OSF/1 v3.2c, the assembler by default does not output file names which
   causes mips-tfile to fail.  Passing -g to the assembler fixes this problem.
   ??? Strictly speaking, we need -g only if the user specifies -g.  Passing
   it always means that we get slightly larger than necessary object files
   if the user does not specify -g.  If we don't pass -g, then mips-tfile
   will need to be fixed to work in this case.  Pass -O0 since some
   optimization are broken and don't help us anyway.  */
#define ASM_SPEC "%{!mgas:-g} -nocpp %{pg} -O0"
#endif

/* Specify to run a post-processor, mips-tfile after the assembler
   has run to stuff the ecoff debug information into the object file.
   This is needed because the Alpha assembler provides no way
   of specifying such information in the assembly file.  */

#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_GAS) != 0

#define ASM_FINAL_SPEC "\
%{malpha-as: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#else
#define ASM_FINAL_SPEC "\
%{!mgas: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#endif

/* Indicate that we have a stamp.h to use.  */
#ifndef CROSS_COMPILE
#define HAVE_STAMP_H 1
#endif

/* Attempt to turn on access permissions for the stack.  */

#define TRANSFER_FROM_TRAMPOLINE					\
void									\
__enable_execute_stack (addr)						\
     void *addr;							\
{									\
  long size = getpagesize ();						\
  long mask = ~(size-1);						\
  char *page = (char *) (((long) addr) & mask);				\
  char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size); \
									\
  /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */				\
  if (mprotect (page, end - page, 7) < 0)				\
    perror ("mprotect of trampoline code");				\
}
