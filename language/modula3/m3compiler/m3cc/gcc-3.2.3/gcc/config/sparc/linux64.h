/* Definitions for 64-bit SPARC running Linux-based GNU systems with ELF.
   Copyright 1996, 1997, 1998, 2000, 2002 Free Software Foundation, Inc.
   Contributed by David S. Miller (davem@caip.rutgers.edu)

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

#define LINUX_DEFAULT_ELF

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9 || TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
/* A 64 bit v9 compiler with stack-bias,
   in a Medium/Low code model environment.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_PTR64 + MASK_64BIT /* + MASK_HARD_QUAD */ \
   + MASK_STACK_BIAS + MASK_APP_REGS + MASK_FPU + MASK_LONG_DOUBLE_128)
#endif

#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-Av9a"

#ifdef SPARC_BI_ARCH

#undef CPP_ARCH32_SPEC
#define CPP_ARCH32_SPEC "%{mlong-double-128:-D__LONG_DOUBLE_128__} \
-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int \
-D__GCC_NEW_VARARGS__ -Acpu=sparc -Amachine=sparc"

#endif

/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'.  */
   
#undef  STARTFILE_SPEC

#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{static:crtbeginT.o%s}\
   %{!static:%{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}}"

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC

#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s\
   %{ffast-math|funsafe-math-optimizations:crtfastmath.o%s}"

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc64 GNU/Linux with ELF)");

/* The default code model.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDLOW

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						    \
{"long-double-64", -MASK_LONG_DOUBLE_128, N_("Use 64 bit long doubles") },  \
{"long-double-128", MASK_LONG_DOUBLE_128, N_("Use 128 bit long doubles") },

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_128 ? 128 : 64)

/* Constant which presents upper bound of the above value.  */
#undef MAX_LONG_DOUBLE_TYPE_SIZE
#define MAX_LONG_DOUBLE_TYPE_SIZE 128

/* Define this to set long double type size to use in libgcc2.c, which can
   not depend on target_flags.  */
#if defined(__arch64__) || defined(__LONG_DOUBLE_128__)
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 128
#else
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64
#endif

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Dunix -D_LONGLONG -D__sparc__ -D__gnu_linux__ -Dlinux -Asystem=unix -Asystem=posix"

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{fPIC:-D__PIC__ -D__pic__} \
%{fpic:-D__PIC__ -D__pic__} \
%{posix:-D_POSIX_SOURCE} \
%{pthread:-D_REENTRANT} \
"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!shared: %{mieee-fp:-lieee} %{pthread:-lpthread} \
     %{profile:-lc_p} %{!profile: -lc}}"

/* Provide a LINK_SPEC appropriate for GNU/Linux.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time. We like to support here for
   as many of the other GNU linker options as possible. But I don't
   have the time to search for those flags. I am sure how to add
   support for -soname shared_object_name. H.J.

   I took out %{v:%{!V:-V}}. It is too much :-(. They can use
   -Wl,-V.

   When the -shared link option is used a final link is not being
   done.  */

/* If ELF is the default format, we should not use /lib/elf.  */

#ifdef SPARC_BI_ARCH

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "link_arch32",       LINK_ARCH32_SPEC },              \
  { "link_arch64",       LINK_ARCH64_SPEC },              \
  { "link_arch_default", LINK_ARCH_DEFAULT_SPEC },	  \
  { "link_arch",	 LINK_ARCH_SPEC },
    
#define LINK_ARCH32_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
        %{static:-static}}} \
"

#define LINK_ARCH64_SPEC "-m elf64_sparc -Y P,/usr/lib64 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib64/ld-linux.so.2}} \
        %{static:-static}}} \
"

#define LINK_ARCH_SPEC "\
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef  LINK_SPEC
#define LINK_SPEC "\
%(link_arch) \
%{mlittle-endian:-EL} \
%{!mno-relax:%{!r:-relax}} \
"

#undef	CC1_SPEC
#if DEFAULT_ARCH32_P
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m64:-mptr64 -mstack-bias -mlong-double-128 \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:-mcpu=ultrasparc}}}}}}} \
  %{!mno-vis:%{!mcpu=v9:-mvis}}} \
"
#else
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m32:-mptr32 -mno-stack-bias %{!mlong-double-128:-mlong-double-64} \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:-mcpu=cypress}}}}}}}} \
%{!m32:%{!mcpu*:-mcpu=ultrasparc}} \
%{!mno-vis:%{!m32:%{!mcpu=v9:-mvis}}} \
"
#endif

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

#else /* !SPARC_BI_ARCH */

#undef LINK_SPEC
#define LINK_SPEC "-m elf64_sparc -Y P,/usr/lib64 %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib64/ld-linux.so.2}} \
        %{static:-static}}} \
%{mlittle-endian:-EL} \
%{!mno-relax:%{!r:-relax}} \
"

#endif /* !SPARC_BI_ARCH */

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used.  */
#undef ASM_SPEC
#define ASM_SPEC "\
%{V} \
%{v:%{!V:-V}} \
%{!Qn:-Qy} \
%{n} \
%{T} \
%{Ym,*} \
%{Wa,*:%*} \
-s %{fpic:-K PIC} %{fPIC:-K PIC} \
%{mlittle-endian:-EL} \
%(asm_cpu) %(asm_arch) %(asm_relax)"

/* Same as sparc.h */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* System V Release 4 uses DWARF debugging info.  Buf DWARF1 doesn't do
   64-bit anything, so we use DWARF2.  */

#undef DWARF2_DEBUGGING_INFO
#undef DWARF_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common\t"

#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABELREF
#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.L%s%ld", PREFIX, (long)(NUM))

/* DWARF bits.  */

/* Follow Irix 6 and not the Dwarf2 draft in using 64-bit offsets. 
   Obviously the Dwarf2 folks haven't tried to actually build systems
   with their spec.  On a 64-bit system, only 64-bit relocs become
   RELATIVE relocations.  */

/* #define DWARF_OFFSET_SIZE PTR_SIZE */

#if defined(HAVE_LD_EH_FRAME_HDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif

/* Don't be different from other Linux platforms in this regard.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP

/* We use GNU ld so undefine this so that attribute((init_priority)) works.  */
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

/* Handle multilib correctly.  */
#if defined(__arch64__)
/* 64-bit Sparc version */
#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned int *pc_ = (CONTEXT)->ra;					\
    long new_cfa_, i_;							\
    long regs_off_, fpu_save_off_;					\
    long this_cfa_, fpu_save_;						\
									\
    if (pc_[0] != 0x82102065		/* mov NR_rt_sigreturn, %g1 */	\
        || pc_[1] != 0x91d0206d)		/* ta 0x6d */		\
      break;								\
    regs_off_ = 192 + 128;						\
    fpu_save_off_ = regs_off_ + (16 * 8) + (3 * 8) + (2 * 4);		\
    this_cfa_ = (long) (CONTEXT)->cfa;					\
    new_cfa_ = *(long *)(((CONTEXT)->cfa) + (regs_off_ + (14 * 8)));	\
    new_cfa_ += 2047; /* Stack bias */					\
    fpu_save_ = *(long *)((this_cfa_) + (fpu_save_off_));		\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = 14;							\
    (FS)->cfa_offset = new_cfa_ - (long) (CONTEXT)->cfa;		\
    for (i_ = 1; i_ < 16; ++i_)						\
      {									\
	(FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_].loc.offset =					\
	  this_cfa_ + (regs_off_ + (i_ * 8)) - new_cfa_;		\
      }									\
    for (i_ = 0; i_ < 16; ++i_)						\
      {									\
	(FS)->regs.reg[i_ + 16].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_ + 16].loc.offset =				\
	  this_cfa_ + (i_ * 8) - new_cfa_;				\
      }									\
    if (fpu_save_)							\
      {									\
	for (i_ = 0; i_ < 64; ++i_)					\
	  {								\
            if (i_ > 32 && (i_ & 0x1))					\
              continue;							\
	    (FS)->regs.reg[i_ + 32].how = REG_SAVED_OFFSET;		\
	    (FS)->regs.reg[i_ + 32].loc.offset =			\
	      (fpu_save_ + (i_ * 4)) - new_cfa_;			\
	  }								\
      }									\
    /* Stick return address into %g0, same trick Alpha uses.  */	\
    (FS)->regs.reg[0].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[0].loc.offset =					\
      this_cfa_ + (regs_off_ + (16 * 8) + 8) - new_cfa_;		\
    (FS)->retaddr_column = 0;						\
    goto SUCCESS;							\
  } while (0)
#else
/* 32-bit Sparc version */
#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned int *pc_ = (CONTEXT)->ra;					\
    int new_cfa_, i_, oldstyle_;					\
    int regs_off_, fpu_save_off_;					\
    int fpu_save_, this_cfa_;						\
									\
    if (pc_[1] != 0x91d02010)		/* ta 0x10 */			\
      break;								\
    if (pc_[0] == 0x821020d8)		/* mov NR_sigreturn, %g1 */	\
      oldstyle_ = 1;							\
    else if (pc_[0] == 0x82102065)	/* mov NR_rt_sigreturn, %g1 */	\
      oldstyle_ = 0;							\
    else								\
      break;								\
    if (oldstyle_)							\
      {									\
        regs_off_ = 96;							\
        fpu_save_off_ = regs_off_ + (4 * 4) + (16 * 4);			\
      }									\
    else								\
      {									\
        regs_off_ = 96 + 128;						\
        fpu_save_off_ = regs_off_ + (4 * 4) + (16 * 4) + (2 * 4);	\
      }									\
    this_cfa_ = (int) (CONTEXT)->cfa;					\
    new_cfa_ = *(int *)(((CONTEXT)->cfa) + (regs_off_+(4*4)+(14 * 4)));	\
    fpu_save_ = *(int *)((this_cfa_) + (fpu_save_off_));		\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = 14;							\
    (FS)->cfa_offset = new_cfa_ - (int) (CONTEXT)->cfa;			\
    for (i_ = 1; i_ < 16; ++i_)						\
      {									\
        if (i_ == 14)							\
          continue;							\
	(FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_].loc.offset =					\
	   this_cfa_ + (regs_off_+(4 * 4)+(i_ * 4)) - new_cfa_;		\
      }									\
    for (i_ = 0; i_ < 16; ++i_)						\
      {									\
	(FS)->regs.reg[i_ + 16].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_ + 16].loc.offset =				\
	  this_cfa_ + (i_ * 4) - new_cfa_;				\
      }									\
    if (fpu_save_)							\
      {									\
	for (i_ = 0; i_ < 32; ++i_)					\
	  {								\
	    (FS)->regs.reg[i_ + 32].how = REG_SAVED_OFFSET;		\
	    (FS)->regs.reg[i_ + 32].loc.offset =			\
	      (fpu_save_ + (i_ * 4)) - new_cfa_;			\
	  }								\
      }									\
    /* Stick return address into %g0, same trick Alpha uses.  */	\
    (FS)->regs.reg[0].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[0].loc.offset = this_cfa_+(regs_off_+4)-new_cfa_;	\
    (FS)->retaddr_column = 0;						\
    goto SUCCESS;							\
  } while (0)
#endif
