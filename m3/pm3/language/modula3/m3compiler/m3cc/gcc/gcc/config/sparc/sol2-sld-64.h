/* Definitions of target machine for GNU compiler, for 64-bit SPARC
   running Solaris 2 using the system linker.  */

#define SPARC_BI_ARCH

#include "sparc/sol2.h"

/* At least up through Solaris 2.6,
   the system linker does not work with DWARF or DWARF2,
   since it does not have working support for relocations
   to unaligned data.  */

#define LINKER_DOES_NOT_WORK_WITH_DWARF2

/* A 64 bit v9 compiler with stack-bias */

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9 || TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_PTR64 + MASK_64BIT /* + MASK_HARD_QUAD */ + \
   MASK_STACK_BIAS + MASK_APP_REGS + MASK_EPILOGUE + MASK_FPU)
#endif

/* The default code model.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDANY

#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 128

#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC	""
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC	"-xarch=v9"

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plus"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusa"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC "-xarch=v9a"
#endif

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC "\
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
%{fpic:-K PIC} %{fPIC:-K PIC} \
%(asm_cpu)\
"

#if DEFAULT_ARCH32_P
#define DEF_ARCH32_SPEC(__str) "%{!m64:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{m64:" __str "}"
#else
#define DEF_ARCH32_SPEC(__str) "%{m32:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{!m32:" __str "}"
#endif

#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
%{mcypress:} \
%{msparclite:-D__sparclite__} \
%{mf930:-D__sparclite__} %{mf934:-D__sparclite__} \
%{mv8:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{msupersparc:-D__supersparc__ " DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=sparclet:-D__sparclet__} %{mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite:-D__sparclite__} \
%{mcpu=f930:-D__sparclite__} %{mcpu=f934:-D__sparclite__} \
%{mcpu=v8:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=supersparc:-D__supersparc__ " DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=v9:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=ultrasparc:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:%(cpp_cpu_default)}}}}}}} \
"

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "\
%{mcpu=ultrasparc:" DEF_ARCH32_SPEC("-xarch=v8plusa") DEF_ARCH64_SPEC("-xarch=v9a") "} \
%{mcpu=v9:" DEF_ARCH32_SPEC("-xarch=v8plus") DEF_ARCH64_SPEC("-xarch=v9") "} \
%{!mcpu=ultrasparc:%{!mcpu=v9:%{mcpu*:" DEF_ARCH32_SPEC("-xarch=v8") DEF_ARCH64_SPEC("-xarch=v9") "}}} \
%{!mcpu*:%(asm_cpu_default)} \
"

#define STARTFILE_SPEC32 "\
%{ansi:values-Xc.o%s} \
%{!ansi: \
 %{traditional:values-Xt.o%s} \
 %{!traditional:values-Xa.o%s}}"

#define STARTFILE_SPEC64 "\
%{ansi:/usr/lib/sparcv9/values-Xc.o%s} \
%{!ansi: \
 %{traditional:/usr/lib/sparcv9/values-Xt.o%s} \
 %{!traditional:/usr/lib/sparcv9/values-Xa.o%s}}"
 
#ifdef SPARC_BI_ARCH

#if DEFAULT_ARCH32_P
#define STARTFILE_ARCH_SPEC "\
%{m32:" STARTFILE_SPEC32 "} \
%{m64:" STARTFILE_SPEC64 "} \
%{!m32:%{!m64:" STARTFILE_SPEC32 "}}"
#else
#define STARTFILE_ARCH_SPEC "\
%{m32:" STARTFILE_SPEC32 "} \
%{m64:" STARTFILE_SPEC64 "} \
%{!m32:%{!m64:" STARTFILE_SPEC64 "}}"
#endif

#else /* !SPARC_BI_ARCH */

/* In this case we define MD_STARTFILE_PREFIX to /usr/lib/sparcv9/ */
#define STARTFILE_ARCH_SPEC STARTFILE_SPEC32

#endif /* !SPARC_BI_ARCH */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{p:mcrt1.o%s} \
                          %{!p: \
	                    %{pg:gcrt1.o%s gmon.o%s} \
                            %{!pg:crt1.o%s}}}} \
			crti.o%s" STARTFILE_ARCH_SPEC " \
			crtbegin.o%s"

#ifdef SPARC_BI_ARCH

#undef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" CPP_CPU64_DEFAULT_SPEC "} \
%{!m64:" CPP_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" CPP_CPU32_DEFAULT_SPEC "} \
%{!m32:" CPP_CPU64_DEFAULT_SPEC "} \
")

#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" ASM_CPU64_DEFAULT_SPEC "} \
%{!m64:" ASM_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" ASM_CPU32_DEFAULT_SPEC "} \
%{!m32:" ASM_CPU64_DEFAULT_SPEC "} \
")

#undef CPP_ARCH32_SPEC
#define CPP_ARCH32_SPEC "-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int \
-D__GCC_NEW_VARARGS__ -Acpu(sparc) -Amachine(sparc)"
#undef CPP_ARCH64_SPEC
#define CPP_ARCH64_SPEC "-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int \
-D__arch64__ -Acpu(sparc64) -Amachine(sparcv9) -D__sparcv9"

#undef CPP_ARCH_SPEC
#define CPP_ARCH_SPEC "\
%{m32:%(cpp_arch32)} \
%{m64:%(cpp_arch64)} \
%{!m32:%{!m64:%(cpp_arch_default)}} \
"

#undef ASM_ARCH_SPEC
#define ASM_ARCH_SPEC ""

#undef ASM_ARCH32_SPEC
#define ASM_ARCH32_SPEC ""

#undef ASM_ARCH64_SPEC
#define ASM_ARCH64_SPEC ""

#undef ASM_ARCH_DEFAULT_SPEC
#define ASM_ARCH_DEFAULT_SPEC ""

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "link_arch32",       LINK_ARCH32_SPEC },              \
  { "link_arch64",       LINK_ARCH64_SPEC },              \
  { "link_arch_default", LINK_ARCH_DEFAULT_SPEC },	  \
  { "link_arch",	 LINK_ARCH_SPEC },
    
/* This should be the same as in svr4.h, except with -R added.  */
#define LINK_ARCH32_SPEC \
  "%{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{pg:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:%{!pg:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{pg:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:%{!pg:-Y P,/usr/ccs/lib:/usr/lib}}}}"

#define LINK_ARCH64_SPEC \
  "%{mcmodel=medlow:-M /usr/lib/ld/sparcv9/map.below4G} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/sparcv9}}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{pg:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/lib/sparcv9}}}}"

#define LINK_ARCH_SPEC "\
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef  LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %(link_arch) \
   %{Qy:} %{!Qn:-Qy}"

#undef	CC1_SPEC
#if DEFAULT_ARCH32_P
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m64:-mptr64 -mcpu=v9 -mstack-bias -mno-v8plus} \
"
#else
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m32:-mptr32 -mcpu=cypress -mno-stack-bias} \
%{mv8plus:-m32 -mptr32 -mcpu=cypress -mno-stack-bias} \
"
#endif

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

#else /* !SPARC_BI_ARCH */

/*
 * This should be the same as in sol2-sld.h, except with "/sparcv9"
 * appended to the paths and /usr/ccs/lib is no longer necessary
 */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %{mcmodel=medlow:-M /usr/lib/ld/sparcv9/map.below4G} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/sparcv9}}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{pg:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/lib/sparcv9}}}} \
   %{Qy:} %{!Qn:-Qy}"
   
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/lib/sparcv9/"
 
#endif /* ! SPARC_BI_ARCH */

/*
 * Attempt to turn on access permissions for the stack.
 *
 * This code must be defined when compiling gcc but not when compiling
 * libgcc2.a, unless we're generating code for 64 bits SPARC
 *
 * _SC_STACK_PROT is only defined for post 2.6, but we want this code
 * to run always.  2.6 can change the stack protection but has no way to
 * query it.
 *
 */

#define TRANSFER_FROM_TRAMPOLINE					\
static int need_enable_exec_stack;					\
									\
static void check_enabling(void) __attribute__ ((constructor));		\
static void check_enabling(void)					\
{									\
  extern long sysconf(int);						\
									\
  int prot = (int) sysconf(515 /*_SC_STACK_PROT */);			\
  if (prot != 7)							\
    need_enable_exec_stack = 1;						\
}									\
									\
void									\
__enable_execute_stack (addr)						\
     void *addr;							\
{									\
  if (!need_enable_exec_stack)						\
    return;								\
  else {								\
    long size = getpagesize ();						\
    long mask = ~(size-1);						\
    char *page = (char *) (((long) addr) & mask); 			\
    char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size); \
									\
    /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */ 			\
    if (mprotect (page, end - page, 7) < 0)				\
      perror ("mprotect of trampoline code");				\
  }									\
}

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", TARGET_ARCH64 ? ASM_LONGLONG : INT_ASM_OP); \
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", TARGET_ARCH64 ? ASM_LONGLONG : INT_ASM_OP); \
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

