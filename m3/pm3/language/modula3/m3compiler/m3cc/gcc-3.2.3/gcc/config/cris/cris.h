/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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

/* Note that other header files (e.g. config/elfos.h, config/linux.h,
   config/cris/linux.h and config/cris/aout.h) are responsible for lots of
   settings not repeated below.  This file contains general CRIS
   definitions and definitions for the cris-*-elf subtarget.  */

/* Replacement for REG_P since it does not match SUBREGs.  Happens for
   testcase Axis-20000320 with gcc-2.9x.  */
#define REG_S_P(x) \
 (REG_P (x) || (GET_CODE (x) == SUBREG && REG_P (XEXP (x, 0))))

/* Last register in main register bank r0..r15.  */
#define CRIS_LAST_GENERAL_REGISTER 15

/* Descriptions of registers used for arguments.  */
#define CRIS_FIRST_ARG_REG 10
#define CRIS_MAX_ARGS_IN_REGS 4

/* Other convenience definitions.  */
#define CRIS_PC_REGNUM 15
#define CRIS_SRP_REGNUM 16

/* Most of the time, we need the index into the register-names array.
   When passing debug-info, we need the real register number.  */
#define CRIS_CANONICAL_SRP_REGNUM (16 + 11)
#define CRIS_CANONICAL_MOF_REGNUM (16 + 7)

/* When generating PIC, these suffixes are added to the names of non-local
   functions when being output.  Contrary to other ports, we have offsets
   relative to the GOT, not the PC.  We might implement PC-relative PLT
   semantics later for the general case; they are used in some cases right
   now, such as MI thunks.  */
#define CRIS_GOTPLT_SUFFIX ":GOTPLT"
#define CRIS_PLT_GOTOFFSET_SUFFIX ":PLTG"
#define CRIS_PLT_PCOFFSET_SUFFIX ":PLT"

/* If you tweak this, don't forget to check cris_expand_builtin_va_arg.  */
#define CRIS_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

/* Check for max allowed stackframe. A "const char *" to be parsed.  */
extern const char *cris_max_stackframe_str;

/* Which CPU version this is.  A "const char *" to be parsed.  */
extern const char *cris_cpu_str;

/* Which CPU version this is.  The parsed and adjusted cris_cpu_str.  */
extern int cris_cpu_version;

/* Which CPU version to tune for.  A "const char *" to be parsed.  */
extern const char *cris_tune_str;

/* The argument to "-melinux-stacksize=".  We don't parse it currently;
   it's just passed on to the linker.  We might want to do something
   here someday.  */
extern const char *cris_elinux_stacksize_str;

/* Changing the order used to be necessary to put the fourth __make_dp
   argument (a DImode parameter) in registers, to fit with the libfunc
   parameter passing scheme used for intrinsic functions.  FIXME: Check
   performance and maybe remove definition from TARGET_LIBGCC2_CFLAGS now
   that it isn't strictly necessary.  We used to do this through
   TARGET_LIBGCC2_CFLAGS, but that became increasingly difficult as the
   parenthesis (that needed quoting) travels through several layers of
   make and shell invocations.  */
#ifdef IN_LIBGCC2
#define __make_dp(a,b,c,d) __cris_make_dp(d,a,b,c)
#endif


/* Node: Driver */

/* When using make with defaults.mak for Sun this will handily remove
   any "-target sun*" switches.  */
/* We need to override any previous definitions (linux.h) */
#undef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR)		\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)		\
  || !strcmp (STR, "target"))

/* Also provide canonical vN definitions when user specifies an alias.
   Note that -melf overrides -maout.  */

/* The `-$' is here mostly due to the integrated preprocessor not
   handling the builtin expansion of "#define __REGISTER_PREFIX__ $"
   gracefully.  This is slightly redundant although not incorrect.
   We're quite alone defining REGISTER_PREFIX as "$" so it's unlikely
   someone will fight for us.  This year in the mountains.
   Note that for -melinux and -mlinux, command-line -isystem options are
   emitted both before and after the synthesized one.  We can't remove all
   of them: a %{<isystem} will only remove the first one and %{<isystem*}
   will not do TRT.  Those extra occurrences are harmless anyway.  */
#define CPP_SPEC \
 "-$ -D__CRIS_ABI_version=2\
  %{mtune=*:-D__tune_%* %{mtune=v*:-D__CRIS_arch_tune=%*}}\
   %{mtune=etrax4:-D__tune_v3 -D__CRIS_arch_tune=3}\
   %{mtune=etrax100:-D__tune_v8 -D__CRIS_arch_tune=8}\
   %{mtune=svinto:-D__tune_v8 -D__CRIS_arch_tune=8}\
   %{mtune=etrax100lx:-D__tune_v10 -D__CRIS_arch_tune=10}\
   %{mtune=ng:-D__tune_v10 -D__CRIS_arch_tune=10}\
  %{mcpu=*:-D__arch_%* %{mcpu=v*:-D__CRIS_arch_version=%*}}\
   %{mcpu=etrax4:-D__arch_v3 -D__CRIS_arch_version=3}\
   %{mcpu=etrax100:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{mcpu=svinto:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{mcpu=etrax100lx:-D__arch_v10 -D__CRIS_arch_version=10}\
   %{mcpu=ng:-D__arch_v10 -D__CRIS_arch_version=10}\
  %{march=*:-D__arch_%* %{march=v*:-D__CRIS_arch_version=%*}}\
   %{march=etrax4:-D__arch_v3 -D__CRIS_arch_version=3}\
   %{march=etrax100:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{march=svinto:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{march=etrax100lx:-D__arch_v10 -D__CRIS_arch_version=10}\
   %{march=ng:-D__arch_v10 -D__CRIS_arch_version=10}\
  %{metrax100:-D__arch__v8 -D__CRIS_arch_version=8}\
  %{metrax4:-D__arch__v3 -D__CRIS_arch_version=3}\
  %(cpp_subtarget)"

/* For the cris-*-elf subtarget.  */
#define CRIS_CPP_SUBTARGET_SPEC \
 "-D__ELF__\
  %{mbest-lib-options:\
   %{!moverride-best-lib-options:\
    %{!march=*:%{!metrax*:%{!mcpu=*:-D__tune_v10 -D__CRIS_arch_tune=10}}}}}"

/* Remove those Sun-make "target" switches.  */
/* Override previous definitions (linux.h).  */
#undef CC1_SPEC
#define CC1_SPEC \
 "%{target*:}\
  %{metrax4:-march=v3}\
  %{metrax100:-march=v8}\
  %(cc1_subtarget)"

/* For the cris-*-elf subtarget.  */
#define CRIS_CC1_SUBTARGET_SPEC \
 "-melf\
  %{mbest-lib-options:\
   %{!moverride-best-lib-options:\
    %{!march=*:%{!mcpu=*:-mtune=v10 -D__CRIS_arch_tune=10}}\
    %{!finhibit-size-directive:\
      %{!fno-function-sections: -ffunction-sections}\
      %{!fno-data-sections: -fdata-sections}}}}"

/* This adds to CC1_SPEC.  When bugs are removed from -fvtable-gc
   (-fforce-addr causes invalid .vtable_entry asm in tinfo.cc and
   nothing at all works in GCC 3.0-pre), add this line:
   "%{mbest-lib-options:%{!moverride-best-lib-options:\
   %{!melinux:%{!maout|melf:%{!fno-vtable-gc:-fvtable-gc}}}}}".  */
#define CC1PLUS_SPEC ""

/* Override previous definitions (linux.h).  */
#undef ASM_SPEC
#define ASM_SPEC \
 "%{v:-v}\
  %(asm_subtarget)"

/* For the cris-*-elf subtarget.  */
#define CRIS_ASM_SUBTARGET_SPEC "--em=criself"

/* FIXME: We should propagate the -melf option to make the criself
   "emulation" unless a linker script is provided (-T*), but I don't know
   how to do that if either of -Ttext, -Tdata or -Tbss is given but no
   linker script, as is usually the case.  Leave it to the user for the
   time being.

   Note that -melf overrides -maout except that a.out-compiled libraries
   are linked in (multilibbing).  The somewhat cryptic -rpath-link pair is
   to avoid *only* picking up the linux multilib subdir from the "-B./"
   option during build, while still giving it preference.  We'd need some
   %s-variant that checked for existence of some specific file.  */
/* Override previous definitions (svr4.h).  */
#undef LINK_SPEC
#define LINK_SPEC \
 "%{v:--verbose}\
  %(link_subtarget)"

/* For the cris-*-elf subtarget.  */
#define CRIS_LINK_SUBTARGET_SPEC \
 "-mcriself\
  %{sim2:%{!T*:-Tdata 0x4000000 -Tbss 0x8000000}}\
  %{O2|O3: --gc-sections}"

/* Which library to get.  The only difference from the default is to get
   libsc.a if -sim is given to the driver.  Repeat -lc -lsysX
   {X=sim,linux}, because libsysX needs (at least) errno from libc, and
   then we want to resolve new unknowns in libc against libsysX, not
   libnosys.  */
/* Override previous definitions (linux.h).  */
#undef LIB_SPEC
#define LIB_SPEC \
 "%{sim*:-lc -lsyssim -lc -lsyssim}\
  %{!sim*:%{g*:-lg}\
    %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} -lbsp}\
  -lnosys"

/* Linker startfile options; crt0 flavors.

   At the moment there are no gcrt0.o or mcrt0.o, but keep them here and
   link them to crt0.o to be prepared.  Use scrt0.c if running the
   simulator, linear style, or s2crt0.c if fixed style.  */
/* We need to remove any previous definition (elfos.h).  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
 "%{sim2:s2crt0.o%s}\
  %{!sim2:%{sim:scrt0.o%s}\
   %{!sim:%{pg:gcrt0.o%s}\
    %{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}}\
  crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"

#define EXTRA_SPECS				\
  {"cpp_subtarget", CRIS_CPP_SUBTARGET_SPEC},	\
  {"cc1_subtarget", CRIS_CC1_SUBTARGET_SPEC},	\
  {"asm_subtarget", CRIS_ASM_SUBTARGET_SPEC},	\
  {"link_subtarget", CRIS_LINK_SUBTARGET_SPEC},	\
  CRIS_SUBTARGET_EXTRA_SPECS

#define CRIS_SUBTARGET_EXTRA_SPECS


/* Node: Run-time Target */

/* Only keep the non-varying ones here.  */
#define CPP_PREDEFINES	"-Dcris -DCRIS -DGNU_CRIS"

/* This needs to be at least 32 bits.  */
extern int target_flags;

/* Currently this just affects aligment.  FIXME:  Redundant with
   TARGET_ALIGN_BY_32, or put machine stuff here?  */
#define TARGET_MASK_SVINTO 1
#define TARGET_SVINTO (target_flags & TARGET_MASK_SVINTO)

/* If to use condition-codes generated by insns other than the
   immediately preceding compare/test insn.
    Used to check for errors in notice_update_cc.  */
#define TARGET_MASK_CCINIT 2
#define TARGET_CCINIT (target_flags & TARGET_MASK_CCINIT)

/* Debug option.  */
#define TARGET_MASK_PDEBUG 4
#define TARGET_PDEBUG (target_flags & TARGET_MASK_PDEBUG)

/* If to use side-effect patterns.  Used to debug the [rx=ry+i] type
   patterns.  */
#define TARGET_MASK_SIDE_EFFECT_PREFIXES 8
#define TARGET_SIDE_EFFECT_PREFIXES \
 (target_flags & TARGET_MASK_SIDE_EFFECT_PREFIXES)

/* If to expand mul into mstep.  Only used when making libc.a.  */
#define TARGET_MASK_EXPAND_MUL 16
#define TARGET_EXPAND_MUL (target_flags & TARGET_MASK_EXPAND_MUL)

/* If to *keep* (not force) alignment of stack at 16 bits.  */
#define TARGET_MASK_STACK_ALIGN 32
#define TARGET_STACK_ALIGN (target_flags & TARGET_MASK_STACK_ALIGN)

/* If to do alignment on individual non-modifiable objects.  */
#define TARGET_MASK_CONST_ALIGN 64
#define TARGET_CONST_ALIGN (target_flags & TARGET_MASK_CONST_ALIGN)

/* If to do alignment on individual modifiable objects.  */
#define TARGET_MASK_DATA_ALIGN 128
#define TARGET_DATA_ALIGN (target_flags & TARGET_MASK_DATA_ALIGN)

/* If not to omit function prologue and epilogue.  */
#define TARGET_MASK_PROLOGUE_EPILOGUE 256
#define TARGET_PROLOGUE_EPILOGUE (target_flags & TARGET_MASK_PROLOGUE_EPILOGUE)

/* Instructions additions from Etrax 4 and up.
   (Just "lz", which we don't really generate from GCC -- yet).  */
#define TARGET_MASK_ETRAX4_ADD 512
#define TARGET_ETRAX4_ADD (target_flags & TARGET_MASK_ETRAX4_ADD)

/* Say that all alignment specifications say to prefer 32 rather
   than 16 bits.  */
#define TARGET_MASK_ALIGN_BY_32 1024
#define TARGET_ALIGN_BY_32 (target_flags & TARGET_MASK_ALIGN_BY_32)

/* This condition is of limited use, as gcc is riddled with #ifdef:s
   controlling this, rather than if (...):s.  */
#define TARGET_MASK_ELF 2048
#define TARGET_ELF (target_flags & TARGET_MASK_ELF)

/* Currently just used to error-check other options.  Note that this is
   *not* set for -melinux.  */
#define TARGET_MASK_LINUX 4096
#define TARGET_LINUX (target_flags & TARGET_MASK_LINUX)

/* There's a small setup cost with using GOTPLT references, but should
   in total be a win both in code-size and execution-time.  */
#define TARGET_MASK_AVOID_GOTPLT 8192
#define TARGET_AVOID_GOTPLT (target_flags & TARGET_MASK_AVOID_GOTPLT)

#define TARGET_SWITCHES							\
 {									\
  /* No "no-etrax" as it does not really imply any model.		\
     On the other hand, "etrax" implies the common (and large)		\
     subset matching all models.  */					\
  {"etrax4",				 TARGET_MASK_ETRAX4_ADD,	\
   N_("Compile for ETRAX 4 (CRIS v3)")},				\
  {"no-etrax4",				-TARGET_MASK_ETRAX4_ADD, ""},	\
  {"etrax100",			     (TARGET_MASK_SVINTO		\
				      + TARGET_MASK_ETRAX4_ADD		\
				      + TARGET_MASK_ALIGN_BY_32),	\
   N_("Compile for ETRAX 100 (CRIS v8)")},				\
  {"no-etrax100",		    -(TARGET_MASK_SVINTO		\
				      + TARGET_MASK_ETRAX4_ADD), ""},	\
  {"pdebug",				     TARGET_MASK_PDEBUG,	\
   N_("Emit verbose debug information in assembly code")},		\
  {"no-pdebug",				    -TARGET_MASK_PDEBUG, ""},	\
  {"cc-init",				     TARGET_MASK_CCINIT,	\
   N_("Do not use condition codes from normal instructions")},		\
  {"no-cc-init",			    -TARGET_MASK_CCINIT, ""},	\
  {"side-effects",	       TARGET_MASK_SIDE_EFFECT_PREFIXES, ""},	\
  {"no-side-effects",	      -TARGET_MASK_SIDE_EFFECT_PREFIXES,	\
   N_("Do not emit addressing modes with side-effect assignment")},	\
  {"stack-align",			TARGET_MASK_STACK_ALIGN, ""},	\
  {"no-stack-align",		       -TARGET_MASK_STACK_ALIGN,	\
   N_("Do not tune stack alignment")},					\
  {"data-align",			 TARGET_MASK_DATA_ALIGN, ""},	\
  {"no-data-align",			-TARGET_MASK_DATA_ALIGN,	\
   N_("Do not tune writable data alignment")},				\
  {"const-align",			TARGET_MASK_CONST_ALIGN, ""},	\
  {"no-const-align",		       -TARGET_MASK_CONST_ALIGN,	\
   N_("Do not tune code and read-only data alignment")},		\
  {"32-bit",			    (TARGET_MASK_STACK_ALIGN		\
				     + TARGET_MASK_CONST_ALIGN		\
				     + TARGET_MASK_DATA_ALIGN		\
				     + TARGET_MASK_ALIGN_BY_32), ""},	\
  {"32bit",			    (TARGET_MASK_STACK_ALIGN		\
				     + TARGET_MASK_CONST_ALIGN		\
				     + TARGET_MASK_DATA_ALIGN		\
				     + TARGET_MASK_ALIGN_BY_32),	\
   N_("Align code and data to 32 bits")},				\
  {"16-bit",			     (TARGET_MASK_STACK_ALIGN		\
				      + TARGET_MASK_CONST_ALIGN		\
				      + TARGET_MASK_DATA_ALIGN), ""},	\
  {"16bit",			     (TARGET_MASK_STACK_ALIGN		\
				      + TARGET_MASK_CONST_ALIGN		\
				      + TARGET_MASK_DATA_ALIGN), ""},	\
  {"8-bit",			    -(TARGET_MASK_STACK_ALIGN		\
				      + TARGET_MASK_CONST_ALIGN		\
				      + TARGET_MASK_DATA_ALIGN), ""},	\
  {"8bit",			    -(TARGET_MASK_STACK_ALIGN		\
				      + TARGET_MASK_CONST_ALIGN		\
				      + TARGET_MASK_DATA_ALIGN),	\
   N_("Don't align items in code or data")},				\
  {"prologue-epilogue",		  TARGET_MASK_PROLOGUE_EPILOGUE, ""},	\
  {"no-prologue-epilogue",	 -TARGET_MASK_PROLOGUE_EPILOGUE,	\
   N_("Do not emit function prologue or epilogue")},			\
  /* We have to handle this m-option here since we can't wash it off in \
     both CC1_SPEC and CC1PLUS_SPEC.  */				\
  {"best-lib-options",					      0,	\
 N_("Use the most feature-enabling options allowed by other options")},	\
									\
  /* We must call it "override-" since calling it "no-" will cause	\
     gcc.c to forget it, if there's a "later" -mbest-lib-options.	\
     Kludgy, but needed for some multilibbed files.  */			\
  {"override-best-lib-options",				      0,	\
   N_("Override -mbest-lib-options")},					\
  CRIS_SUBTARGET_SWITCHES						\
  {"",			TARGET_DEFAULT | CRIS_SUBTARGET_DEFAULT, ""}}	\

/* For the cris-*-elf subtarget.  */
#define CRIS_SUBTARGET_SWITCHES \
 {"elf", 0, ""},

/* Default target_flags if no switches specified.  */
#ifndef TARGET_DEFAULT
# define TARGET_DEFAULT \
 (TARGET_MASK_SIDE_EFFECT_PREFIXES + TARGET_MASK_STACK_ALIGN \
  + TARGET_MASK_CONST_ALIGN + TARGET_MASK_DATA_ALIGN \
  + TARGET_MASK_PROLOGUE_EPILOGUE)
#endif

/* For the cris-*-elf subtarget.  */
#define CRIS_SUBTARGET_DEFAULT TARGET_MASK_ELF

#define CRIS_CPU_BASE 0
#define CRIS_CPU_ETRAX4 3	/* Just lz added.  */
#define CRIS_CPU_SVINTO 8	/* Added swap, jsrc & Co., 32-bit accesses.  */
#define CRIS_CPU_NG 10		/* Added mul[su].  */

/* Local, providing a default for cris_cpu_version.  */
#define CRIS_DEFAULT_CPU_VERSION CRIS_CPU_BASE

#define TARGET_HAS_MUL_INSNS (cris_cpu_version >= CRIS_CPU_NG)

#define TARGET_OPTIONS							\
 {{"cpu=", &cris_cpu_str, ""},						\
  {"arch=", &cris_cpu_str,						\
   N_("Generate code for the specified chip or CPU version")},		\
  {"tune=", &cris_tune_str,						\
   N_("Tune alignment for the specified chip or CPU version")},		\
  {"max-stackframe=", &cris_max_stackframe_str,				\
   N_("Warn when a stackframe is larger than the specified size")},	\
  CRIS_SUBTARGET_LONG_OPTIONS						\
  {"ax-stackframe=", &cris_max_stackframe_str, ""}}

#define CRIS_SUBTARGET_LONG_OPTIONS

/* Print subsidiary information on the compiler version in use.
   Do not use VD.D syntax (D=digit), since this will cause confusion
   with the base gcc version among users, when we ask which version of
   gcc-cris they are using.  Please use some flavor of "R<number>" for
   the version (no need for major.minor versions, I believe).  */
#define TARGET_VERSION \
 fprintf (stderr, " [Axis CRIS%s]", CRIS_SUBTARGET_VERSION)

/* For the cris-*-elf subtarget.  */
#define CRIS_SUBTARGET_VERSION " - generic ELF"

#define OVERRIDE_OPTIONS cris_override_options ()

/* The following gives optimal code for gcc-2.7.2, but *may* be subject
   to change.  Omitting flag_force_addr gives .1-.7% faster code for gcc
   *only*, but 1.3% larger code.  On ipps it gives 5.3-10.6% slower
   code(!) and 0.3% larger code.  For products, images gets .1-1.8%
   larger.  Do not set strict aliasing from optimization options.  */
#define OPTIMIZATION_OPTIONS(OPTIMIZE, SIZE)	\
  do						\
    {						\
      if ((OPTIMIZE) >= 2 || (SIZE))		\
	{					\
	  flag_force_addr = 1;			\
	  flag_omit_frame_pointer = 1;		\
	}					\
    }						\
  while (0)


/* Node: Storage Layout */

#define BITS_BIG_ENDIAN 0

#define BYTES_BIG_ENDIAN 0

/* WORDS_BIG_ENDIAN is not defined in the hardware, but for consistency,
   we use little-endianness, and we may also be able to use
   post-increment on DImode indirect.  */
#define WORDS_BIG_ENDIAN 0

#define BITS_PER_UNIT 8

#define BITS_PER_WORD 32

#define UNITS_PER_WORD 4

#define POINTER_SIZE 32

/* A combination of defining PROMOTE_MODE, PROMOTE_FUNCTION_ARGS,
   PROMOTE_FOR_CALL_ONLY and *not* defining PROMOTE_PROTOTYPES gives the
   best code size and speed for gcc, ipps and products in gcc-2.7.2.  */
#define CRIS_PROMOTED_MODE(MODE, UNSIGNEDP, TYPE) \
 (GET_MODE_CLASS (MODE) == MODE_INT && GET_MODE_SIZE (MODE) < 4) \
  ? SImode : MODE

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)  \
  (MODE) = CRIS_PROMOTED_MODE (MODE, UNSIGNEDP, TYPE)

#define PROMOTE_FUNCTION_ARGS

/* Defining PROMOTE_FUNCTION_RETURN in gcc-2.7.2 uncovers bug 981110 (even
   if defining FUNCTION_VALUE with MODE as PROMOTED_MODE ;-)

   FIXME: Report this when cris.h is part of GCC, so others can easily
   see the problem.  Maybe check other systems that define
   PROMOTE_FUNCTION_RETURN.  */
#define PROMOTE_FOR_CALL_ONLY

/* We will be using prototype promotion, so they will be 32 bit.  */
#define PARM_BOUNDARY 32

/* Stack boundary is guided by -mstack-align, -mno-stack-align,
   -malign.
   Old comment: (2.1: still valid in 2.7.2?)
    Note that to make this macro affect the alignment of stack
   locals, a fix was required, and special precautions when handling
   the stack pointer in various other macros (TARGET_ASM_FUNCTION_PROLOGUE
   et al) were required.  See file "function.c".  If you would just define
   this macro, it would only affect the builtin alloca and variable
   local data (non-ANSI, non-K&R, Gnu C extension).  */
#define STACK_BOUNDARY \
 (TARGET_STACK_ALIGN ? (TARGET_ALIGN_BY_32 ? 32 : 16) : 8)

#define FUNCTION_BOUNDARY 16

/* Do not change BIGGEST_ALIGNMENT (when optimizing), as it will affect
   strange places, at least in 2.1.  */
#define BIGGEST_ALIGNMENT 8

/* If -m16bit,	-m16-bit, -malign or -mdata-align,
   align everything to 16 bit.  */
#define DATA_ALIGNMENT(TYPE, BASIC_ALIGN)			\
 (TARGET_DATA_ALIGN						\
  ? (TARGET_ALIGN_BY_32						\
     ? (BASIC_ALIGN < 32 ? 32 : BASIC_ALIGN)			\
     : (BASIC_ALIGN < 16 ? 16 : BASIC_ALIGN)) : BASIC_ALIGN)

/* Note that CONSTANT_ALIGNMENT has the effect of making gcc believe that
   ALL references to constant stuff (in code segment, like strings) has
   this alignment.  That is a rather rushed assumption.  Luckily we do not
   care about the "alignment" operand to builtin memcpy (only place where
   it counts), so it doesn't affect any bad spots.  */
#define CONSTANT_ALIGNMENT(CONSTANT, BASIC_ALIGN)		\
 (TARGET_CONST_ALIGN						\
  ? (TARGET_ALIGN_BY_32						\
     ? (BASIC_ALIGN < 32 ? 32 : BASIC_ALIGN)			\
     : (BASIC_ALIGN < 16 ? 16 : BASIC_ALIGN)) : BASIC_ALIGN)

/* FIXME: Define LOCAL_ALIGNMENT for word and dword or arrays and
   structures (if -mstack-align=), and check that it is good.  */

#define EMPTY_FIELD_BOUNDARY 8

#define STRUCTURE_SIZE_BOUNDARY 8

#define STRICT_ALIGNMENT 0

/* Remove any previous definition (elfos.h).
   ??? If it wasn't for all the other stuff that affects layout of
   structures and bit-fields, this could presumably cause incompatibility
   with other GNU/Linux ports (i.e. elfos.h users).  */
#undef PCC_BITFIELD_TYPE_MATTERS

/* This is only used for non-scalars.  Strange stuff happens to structs
   (FIXME: What?) if we use anything larger than largest actually used
   datum size, so lets make it 32.  The type "long long" will still work
   as usual.  We can still have DImode insns, but they will only be used
   for scalar data (i.e. long long).  */
#define MAX_FIXED_MODE_SIZE 32


/* Node: Type Layout */

/* Note that DOUBLE_TYPE_SIZE is not defined anymore, since the default
   value gives a 64-bit double, which is what we now use.  */

/* For compatibility and historical reasons, a char should be signed.  */
#define DEFAULT_SIGNED_CHAR 1

/* No DEFAULT_SHORT_ENUMS, please.  */

/* Note that WCHAR_TYPE_SIZE is used in cexp.y,
   where TARGET_SHORT is not available.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* Node: Register Basics */

/*  We count all 16 non-special registers, SRP and a faked argument
    pointer register.  */
#define FIRST_PSEUDO_REGISTER (16 + 1 + 1)

/* For CRIS, these are r15 (pc) and r14 (sp). Register r8 is used as a
   frame-pointer, but is not fixed.  SRP is not included in general
   registers and will not be used automatically.  All other special
   registers are fixed at the moment.  The faked argument pointer register
   is fixed too.  */
#define FIXED_REGISTERS \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1}

/* Register r9 is used for structure-address, r10-r13 for parameters,
   r10- for return values.  */
#define CALL_USED_REGISTERS \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1}

#define CONDITIONAL_REGISTER_USAGE cris_conditional_register_usage ()


/* Node: Allocation Order */

/* We need this on CRIS, because call-used regs should be used first,
   (so we don't need to push).  Else start using registers from r0 and up.
    This preference is mainly because if we put call-used-regs from r0
   and up, then we can't use movem to push the rest, (which have to be
   saved if we use them, and movem has to start with r0).
   Change here if you change which registers to use as call registers.

   The actual need to explicitly prefer call-used registers improved the
   situation a lot for 2.1, but might not actually be needed anymore.
   Still, this order reflects what GCC should find out by itself, so it
   probably does not hurt.

   Order of preference: Call-used-regs first, then r0 and up, last fp &
   sp & pc as fillers.
   Call-used regs in opposite order, so they will cause less conflict if
   a function has few args (<= 3) and it wants a scratch reg.
    Use struct-return address first, since very few functions use
   structure return values so it is likely to be available.  */
#define REG_ALLOC_ORDER \
 {9, 13, 12, 11, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 14, 15, 16, 17}


/* Node: Values in Registers */

/* The VOIDmode test is so we can omit mode on anonymous insns.  FIXME:
   Still needed in 2.9x, at least for Axis-20000319.  */
#define HARD_REGNO_NREGS(REGNO, MODE)	\
 (MODE == VOIDmode \
  ? 1 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* CRIS permits all registers to hold all modes.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

#define MODES_TIEABLE_P(MODE1, MODE2)  1


/* Node: Leaf Functions */
/* (no definitions) */

/* Node: Stack Registers */
/* (no definitions) */


/* Node: Register Classes */

/* CRIS has only one kind of registers, so NO_REGS and ALL_REGS
   are the only classes.  FIXME: It actually makes sense to have another
   class for special registers, and yet another class for the
   multiply-overflow register in v10; then a class for the return
   register also makes sense.  */
enum reg_class {NO_REGS, ALL_REGS, LIM_REG_CLASSES};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES {"NO_REGS", "ALL_REGS"}

#define GENERAL_REGS ALL_REGS

/* Count in the faked argument register in GENERAL_REGS.  Keep out SRP.  */
#define REG_CLASS_CONTENTS {{0}, {0x2ffff}}

#define REGNO_REG_CLASS(REGNO) GENERAL_REGS

#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine
   description.  No letters are used, since 'r' is used for any
   register.  */
#define REG_CLASS_FROM_LETTER(C) NO_REGS

/* Since it uses reg_renumber, it is safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO)					\
 ((REGNO) <= CRIS_LAST_GENERAL_REGISTER					\
  || (REGNO) == ARG_POINTER_REGNUM					\
  || (unsigned) reg_renumber[REGNO] <= CRIS_LAST_GENERAL_REGISTER	\
  || (unsigned) reg_renumber[REGNO] == ARG_POINTER_REGNUM)

/* See REGNO_OK_FOR_BASE_P.  */
#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* It seems like gcc (2.7.2 and 2.9x of 2000-03-22) may send "NO_REGS" as
   the class for a constant (testcase: __Mul in arit.c).  To avoid forcing
   out a constant into the constant pool, we will trap this case and
   return something a bit more sane.  FIXME: Check if this is a bug.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) \
 ((CLASS) == NO_REGS ? GENERAL_REGS : (CLASS))

/* For CRIS, this is always the size of MODE in words,
   since all registers are the same size.  To use omitted modes in
   patterns with reload constraints, you must say the widest size
   which is allowed for VOIDmode.
   FIXME:  Does that still apply for gcc-2.9x?  Keep poisoned until such
   patterns are added back.  News: 2001-03-16: Happens as early as the
   underscore-test.  */
#define CLASS_MAX_NREGS(CLASS, MODE)					\
 ((MODE) == VOIDmode							\
  ? 1 /* + cris_fatal ("CLASS_MAX_NREGS with VOIDmode")	*/		\
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* We are now out of letters; we could use ten more.  This forces us to
   use C-code in the 'md' file.  FIXME: Use some EXTRA_CONSTRAINTS.  */
#define CONST_OK_FOR_LETTER_P(VALUE, C)			\
 (							\
  /* MOVEQ, CMPQ, ANDQ, ORQ.  */			\
  (C) == 'I' ? (VALUE) >= -32 && (VALUE) <= 31 :	\
  /* ADDQ, SUBQ.  */					\
  (C) == 'J' ? (VALUE) >= 0 && (VALUE) <= 63 :		\
  /* ASRQ, BTSTQ, LSRQ, LSLQ.  */			\
  (C) == 'K' ? (VALUE) >= 0 && (VALUE) <= 31 :		\
  /* A 16-bit signed number.  */			\
  (C) == 'L' ? (VALUE) >= -32768 && (VALUE) <= 32767 :	\
  /* The constant 0 for CLEAR.  */			\
  (C) == 'M' ? (VALUE) == 0 :				\
  /* A negative ADDQ or SUBQ.  */			\
  (C) == 'N' ? (VALUE) >= -63 && (VALUE) < 0 :		\
  /* Quickened ints, QI and HI.  */			\
  (C) == 'O' ? (VALUE) >= 0 && (VALUE) <= 65535		\
		&& ((VALUE) >= (65535-31)		\
		    || ((VALUE) >= (255-31)		\
			&& (VALUE) <= 255 )) :		\
  /* A 16-bit number signed *or* unsigned.  */		\
  (C) == 'P' ? (VALUE) >= -32768 && (VALUE) <= 65535 :	\
  0)

/* It is really simple to make up a 0.0; it is the same as int-0 in
   IEEE754.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)			\
 ((C) == 'G' && ((VALUE) == CONST0_RTX (DFmode)			\
		 || (VALUE) == CONST0_RTX (SFmode)))

/* We need this on cris to distinguish delay-slottable addressing modes.  */
#define EXTRA_CONSTRAINT(X, C)			\
 (						\
  /* Slottable address mode?  */		\
  (C) == 'Q' ? EXTRA_CONSTRAINT_Q (X) :		\
  /* Operand to BDAP or BIAP?  */		\
  (C) == 'R' ? EXTRA_CONSTRAINT_R (X) :		\
  /* A local PIC symbol?  */			\
  (C) == 'S' ? EXTRA_CONSTRAINT_S (X) :		\
  /* A three-address addressing-mode?  */	\
  (C) == 'T' ? EXTRA_CONSTRAINT_T (X) :		\
  /* A global PIC symbol?  */			\
  (C) == 'U' ? EXTRA_CONSTRAINT_U (X) :		\
  0)

#define EXTRA_CONSTRAINT_Q(X)				\
 (							\
  /* Slottable addressing modes:			\
     A register?  FIXME: Unnecessary.  */		\
  (BASE_P (X) && REGNO (X) != CRIS_PC_REGNUM)		\
  /* Indirect register: [reg]?  */			\
  || (GET_CODE (X) == MEM && BASE_P (XEXP (X, 0))	\
      && REGNO (XEXP (X, 0)) != CRIS_PC_REGNUM)		\
 )

#define EXTRA_CONSTRAINT_R(X)					\
 (								\
  /* An operand to BDAP or BIAP:				\
     A BIAP; r.S? */						\
  BIAP_INDEX_P (X)						\
  /* A [reg] or (int) [reg], maybe with post-increment.  */	\
  || BDAP_INDEX_P (X)						\
  || CONSTANT_INDEX_P (X)					\
 )

#define EXTRA_CONSTRAINT_T(X)						\
 (									\
  /* Memory three-address operand.  All are indirect-memory:  */	\
  GET_CODE (X) == MEM							\
  && ((GET_CODE (XEXP (X, 0)) == MEM					\
       /* Double indirect: [[reg]] or [[reg+]]?  */			\
       && (BASE_OR_AUTOINCR_P (XEXP (XEXP (X, 0), 0))))			\
      /* Just an explicit indirect reference: [const]?  */		\
      || CONSTANT_P (XEXP (X, 0))					\
      /* Something that is indexed; [...+...]?  */			\
      || (GET_CODE (XEXP (X, 0)) == PLUS				\
	  /* A BDAP constant: [reg+(8|16|32)bit offset]?  */		\
	  && ((BASE_P (XEXP (XEXP (X, 0), 0))				\
	       && CONSTANT_INDEX_P (XEXP (XEXP (X, 0), 1)))		\
	      /* Swap arguments to the above.  FIXME: gcc-2.9x? */	\
	      || (BASE_P (XEXP (XEXP (X, 0), 1))			\
		  && CONSTANT_INDEX_P (XEXP (XEXP (X, 0), 0)))		\
	      /* A BDAP register: [reg+[reg(+)].S]?  */			\
	      || (BASE_P (XEXP (XEXP (X, 0), 0))			\
		  && BDAP_INDEX_P(XEXP(XEXP(X, 0), 1)))			\
	      /* Same, but with swapped arguments.  */			\
	      || (BASE_P (XEXP (XEXP (X, 0), 1))			\
		  && BDAP_INDEX_P (XEXP (XEXP (X, 0), 0)))		\
	      /* A BIAP: [reg+reg.S].  */				\
	      || (BASE_P (XEXP (XEXP (X, 0), 0))			\
		  && BIAP_INDEX_P (XEXP (XEXP (X, 0), 1)))		\
	      /* Same, but with swapped arguments.  */			\
	      || (BASE_P (XEXP (XEXP (X, 0), 1))			\
		  && BIAP_INDEX_P (XEXP (XEXP (X, 0), 0))))))		\
 )

#define EXTRA_CONSTRAINT_S(X) \
 (flag_pic && CONSTANT_P (X) && cris_gotless_symbol (X))

#define EXTRA_CONSTRAINT_U(X) \
 (flag_pic && CONSTANT_P (X) && cris_got_symbol (X))


/* Node: Frame Layout */

#define STACK_GROWS_DOWNWARD
#define FRAME_GROWS_DOWNWARD

/* It seems to be indicated in the code (at least 2.1) that this is
   better a constant, and best 0.  */
#define STARTING_FRAME_OFFSET 0

#define FIRST_PARM_OFFSET(FNDECL) 0

#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) \
 cris_return_addr_rtx (COUNT, FRAMEADDR)

#define INCOMING_RETURN_ADDR_RTX gen_rtx (REG, Pmode, CRIS_SRP_REGNUM)

/* FIXME: Any __builtin_eh_return callers must not return anything and
   there must not be collisions with incoming parameters.  Luckily the
   number of __builtin_eh_return callers is limited.  For now return
   parameter registers in reverse order and hope for the best.  */
#define EH_RETURN_DATA_REGNO(N) \
  (IN_RANGE ((N), 0, 3) ? (CRIS_FIRST_ARG_REG + 3 - (N)) : INVALID_REGNUM)

/* Store the stack adjustment in the structure-return-address register.  */
#define CRIS_STACKADJ_REG STRUCT_VALUE_REGNUM
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (SImode, CRIS_STACKADJ_REG)

#define EH_RETURN_HANDLER_RTX \
  cris_return_addr_rtx (0, NULL)

#define INIT_EXPANDERS cris_init_expanders ()

/* FIXME: Move this to right node (it's not documented properly yet).  */
#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (CRIS_SRP_REGNUM)

/* FIXME: Move this to right node (it's not documented properly yet).
   FIXME: Check what alignment we can assume regarding
   TARGET_STACK_ALIGN and TARGET_ALIGN_BY_32.  */
#define DWARF_CIE_DATA_ALIGNMENT -1

/* If we would ever need an exact mapping between canonical register
   number and dwarf frame register, we would either need to include all
   registers in the gcc decription (with some marked fixed of course), or
   an inverse mapping from dwarf register to gcc register.  There is one
   need in dwarf2out.c:expand_builtin_init_dwarf_reg_sizes.  Right now, I
   don't see that we need exact correspondence between DWARF *frame*
   registers and DBX_REGISTER_NUMBER, so map them onto GCC registers.  */
#define DWARF_FRAME_REGNUM(REG) (REG)

/* Node: Stack Checking */
/* (no definitions) FIXME: Check.  */

/* Node: Frame Registers */

#define STACK_POINTER_REGNUM 14

/* Register used for frame pointer.  This is also the last of the saved
   registers, when a frame pointer is not used.  */
#define FRAME_POINTER_REGNUM 8

/* Faked register, is always eliminated.  We need it to eliminate
   allocating stack slots for the return address and the frame pointer.  */
#define ARG_POINTER_REGNUM 17

#define STATIC_CHAIN_REGNUM 7


/* Node: Elimination */

/* Really only needed if the stack frame has variable length (alloca
   or variable sized local arguments (GNU C extension).  */
#define FRAME_POINTER_REQUIRED 0

#define ELIMINABLE_REGS				\
 {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* We need not worry about when the frame-pointer is required for other
   reasons.  */
#define CAN_ELIMINATE(FROM, TO) 1

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
 (OFFSET) = cris_initial_elimination_offset (FROM, TO)


/* Node: Stack Arguments */

/* Since many parameters take up one register each in any case,
   PROMOTE_PROTOTYPES would seem like a good idea, but measurements
   indicate that a combination using PROMOTE_MODE is better.  */

#define ACCUMULATE_OUTGOING_ARGS 1

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACKSIZE) 0


/* Node: Register Arguments */

/* The void_type_node is sent as a "closing" call.  We have to stop it
   since it's invalid to FUNCTION_ARG_PASS_BY_REFERENCE (or was invalid at
   some time).  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)			\
 ((CUM).regs < CRIS_MAX_ARGS_IN_REGS				\
  && (TYPE) != void_type_node					\
  && ! FUNCTION_ARG_PASS_BY_REFERENCE (CUM, MODE, TYPE, NAMED)	\
  ? gen_rtx (REG, MODE, (CRIS_FIRST_ARG_REG) + (CUM).regs)	\
  : NULL_RTX)

/* The differences between this and the previous, is that this one checks
   that an argument is named, since incoming stdarg/varargs arguments are
   pushed onto the stack, and we don't have to check against the "closing"
   void_type_node TYPE parameter.  */
#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)			\
 (((NAMED) && (CUM).regs < CRIS_MAX_ARGS_IN_REGS			\
   && ! FUNCTION_ARG_PASS_BY_REFERENCE (CUM, MODE, TYPE, NAMED))	\
  ? gen_rtx (REG, MODE, CRIS_FIRST_ARG_REG + (CUM).regs)		\
  : NULL_RTX)

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)	\
 (((CUM).regs == (CRIS_MAX_ARGS_IN_REGS - 1)			\
   && !MUST_PASS_IN_STACK (MODE, TYPE)				\
   && CRIS_FUNCTION_ARG_SIZE (MODE, TYPE) > 4			\
   && CRIS_FUNCTION_ARG_SIZE (MODE, TYPE) <= 8)			\
  ? 1 : 0)

/* Structs may be passed by value, but they must not be more than 8
   bytes long.  If you tweak this, don't forget to adjust
   cris_expand_builtin_va_arg.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
 (MUST_PASS_IN_STACK (MODE, TYPE)					\
  || CRIS_FUNCTION_ARG_SIZE (MODE, TYPE) > 8)				\

/* Contrary to what you'd believe, defining FUNCTION_ARG_CALLEE_COPIES
   seems like a (small total) loss, at least for gcc-2.7.2 compiling and
   running gcc-2.1 (small win in size, small loss running -- 100.1%),
   and similarly for size for products (.1 .. .3% bloat, sometimes win).
   Due to the empirical likeliness of making slower code, it is not
   defined.  */

/* This no longer *needs* to be a structure; but keeping it as such should
   not hurt (and hacking the ABI is simpler).  */
#define CUMULATIVE_ARGS struct cum_args
struct cum_args {int regs;};

/* The regs member is an integer, the number of arguments got into
   registers so far, and lib is nonzero if init_cumulative_args was
   found to generate a call to a library function.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT)	  \
 ((CUM).regs = 0)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
 ((CUM).regs							\
  = (FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)	\
     ? (CRIS_MAX_ARGS_IN_REGS) + 1				\
     : ((CUM).regs						\
	+ (3 + (CRIS_FUNCTION_ARG_SIZE (MODE, TYPE))) / 4)))

#define FUNCTION_ARG_REGNO_P(REGNO)			\
 ((REGNO) >= CRIS_FIRST_ARG_REG				\
  && (REGNO) < CRIS_FIRST_ARG_REG + (CRIS_MAX_ARGS_IN_REGS))


/* Node: Scalar Return */

/* Let's assume all functions return in r[CRIS_FIRST_ARG_REG] for the
   time being.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  \
 gen_rtx (REG, TYPE_MODE (VALTYPE), CRIS_FIRST_ARG_REG)

#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, CRIS_FIRST_ARG_REG)

#define FUNCTION_VALUE_REGNO_P(N) ((N) == CRIS_FIRST_ARG_REG)


/* Node: Aggregate Return */

#if 0
/* FIXME: Let's try this some time, so we return structures in registers.
   We would cast the result of int_size_in_bytes to unsigned, so we will
   get a huge number for "structures" of variable size (-1).  */
#define RETURN_IN_MEMORY(TYPE) \
 ((unsigned) int_size_in_bytes (TYPE) > CRIS_MAX_ARGS_IN_REGS * UNITS_PER_WORD)
#endif

#define STRUCT_VALUE_REGNUM ((CRIS_FIRST_ARG_REG) - 1)


/* Node: Caller Saves */
/* (no definitions) */

/* Node: Function entry */

/* See cris.c for TARGET_ASM_FUNCTION_PROLOGUE and
   TARGET_ASM_FUNCTION_EPILOGUE.  */

/* If the epilogue uses the "ret" insn, we need to fill the
   delay slot.  */
#define DELAY_SLOTS_FOR_EPILOGUE cris_delay_slots_for_epilogue ()

#define ELIGIBLE_FOR_EPILOGUE_DELAY(INSN, N) \
  cris_eligible_for_epilogue_delay (INSN)

#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION) \
 cris_asm_output_mi_thunk(FILE, THUNK_FNDECL, DELTA, FUNCTION)


/* Node: Profiling */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
 error ("no FUNCTION_PROFILER for CRIS")

/* FIXME: Some of the undefined macros might be mandatory.  If so, fix
   documentation.  */


/* Node: Varargs */

/* We save the register number of the first anonymous argument in
   first_vararg_reg, and take care of this in the function prologue.
   This behaviour is used by at least one more port (the ARM?), but
   may be unsafe when compiling nested functions.  (With varargs? Hairy.)
   Note that nested-functions is a GNU C extension.

   FIXME: We can actually put the size in PRETEND and deduce the number
   of registers from it in the prologue and epilogue.  */
#define SETUP_INCOMING_VARARGS(ARGSSF, MODE, TYPE, PRETEND, SECOND)	\
  do									\
    {									\
      if ((ARGSSF).regs < (CRIS_MAX_ARGS_IN_REGS))			\
	(PRETEND) = ((CRIS_MAX_ARGS_IN_REGS) - (ARGSSF).regs) * 4;	\
      if (TARGET_PDEBUG)						\
	{								\
	  fprintf (asm_out_file,					\
		   "\n; VA:: %s: %d args before, anon @ #%d, %dtime\n",	\
		   current_function_varargs ? "OLD" : "ANSI",		\
		   (ARGSSF).regs, PRETEND, SECOND);			\
	}								\
    }									\
  while (0)

/* FIXME: This and other EXPAND_BUILTIN_VA_... target macros are not
   documented, although used by several targets.  */
#define EXPAND_BUILTIN_VA_ARG(VALIST, TYPE) \
 cris_expand_builtin_va_arg (VALIST, TYPE)


/* Node: Trampolines */

/* This looks too complicated, and it is.  I assigned r7 to be the
   static chain register, but it is call-saved, so we have to save it,
   and come back to restore it after the call, so we have to save srp...
   Anyway, trampolines are rare enough that we can cope with this
   somewhat lack of elegance.
    (Do not be tempted to "straighten up" whitespace in the asms; the
   assembler #NO_APP state mandates strict spacing).  */
#define TRAMPOLINE_TEMPLATE(FILE)		\
  do						\
    {						\
      fprintf (FILE, "\tmove.d $%s,[$pc+20]\n",	\
	       reg_names[STATIC_CHAIN_REGNUM]);	\
      fprintf (FILE, "\tmove $srp,[$pc+22]\n");	\
      fprintf (FILE, "\tmove.d 0,$%s\n",	\
	       reg_names[STATIC_CHAIN_REGNUM]);	\
      fprintf (FILE, "\tjsr 0\n");		\
      fprintf (FILE, "\tmove.d 0,$%s\n",	\
	       reg_names[STATIC_CHAIN_REGNUM]);	\
      fprintf (FILE, "\tjump 0\n");		\
    }						\
  while (0)

#define TRAMPOLINE_SIZE 32

/* CRIS wants instructions on word-boundary.
   Note that due to a bug (reported) in 2.7.2 and earlier, this is
   actually treated as alignment in _bytes_, not _bits_.  (Obviously
   this is not fatal, only a slight waste of stack space).  */
#define TRAMPOLINE_ALIGNMENT 16

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)		\
  do								\
    {								\
      emit_move_insn (gen_rtx (MEM, SImode,			\
			       plus_constant (TRAMP, 10)),	\
		      CXT);					\
      emit_move_insn (gen_rtx (MEM, SImode,			\
			       plus_constant (TRAMP, 16)),	\
		      FNADDR);					\
    }								\
  while (0)

/* Note that there is no need to do anything with the cache for sake of
   a trampoline.  */


/* Node: Library Calls */

#define MULSI3_LIBCALL "__Mul"
#define DIVSI3_LIBCALL "__Div"
#define UDIVSI3_LIBCALL "__Udiv"
#define MODSI3_LIBCALL "__Mod"
#define UMODSI3_LIBCALL "__Umod"

/* If you change this, you have to check whatever libraries and systems
   that use it.  */
#define TARGET_EDOM 33


/* Node: Addressing Modes */

#define HAVE_POST_INCREMENT 1

#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

#define MAX_REGS_PER_ADDRESS 2

/* There are helper macros defined here which are used only in
   GO_IF_LEGITIMATE_ADDRESS.

   Note that you *have to* reject invalid addressing modes for mode
   MODE, even if it is legal for normal addressing modes.  You cannot
   rely on the constraints to do this work.  They can only be used to
   doublecheck your intentions.  One example is that you HAVE TO reject
   (mem:DI (plus:SI (reg:SI x) (reg:SI y))) because for some reason
   this cannot be reloaded.  (Which of course you can argue that gcc
   should have done.)  FIXME:  Strange.  Check.  */

/* No symbol can be used as an index (or more correct, as a base) together
   with a register with PIC; the PIC register must be there.  */
#define CONSTANT_INDEX_P(X) \
 (CONSTANT_P (X) && !(flag_pic && cris_symbol (X)))

/* True if X is a valid base register.  */
#define BASE_P(X) \
 (REG_P (X) && REG_OK_FOR_BASE_P (X))

/* True if X is a valid base register with or without autoincrement.  */
#define BASE_OR_AUTOINCR_P(X) \
 (BASE_P (X) || (GET_CODE (X) == POST_INC && BASE_P (XEXP (X, 0))))

/* True if X is a valid (register) index for BDAP, i.e. [Rs].S or [Rs+].S.  */
#define BDAP_INDEX_P(X)					\
 ((GET_CODE (X) == MEM && GET_MODE (X) == SImode	\
   && BASE_OR_AUTOINCR_P (XEXP (X, 0)))			\
  || (GET_CODE (X) == SIGN_EXTEND			\
      && GET_CODE (XEXP (X, 0)) == MEM			\
      && (GET_MODE (XEXP (X, 0)) == HImode		\
	  || GET_MODE (XEXP (X, 0)) == QImode)		\
      && BASE_OR_AUTOINCR_P (XEXP (XEXP (X, 0), 0))))

/* True if X is a valid (register) index for BIAP, i.e. Rd.m.  */
#define BIAP_INDEX_P(X)				\
 ((BASE_P (X) && REG_OK_FOR_INDEX_P (X))	\
  || (GET_CODE (X) == MULT			\
      && BASE_P (XEXP (X, 0))			\
      && REG_OK_FOR_INDEX_P (XEXP (X, 0))	\
      && GET_CODE (XEXP (X, 1)) == CONST_INT	\
      && (INTVAL (XEXP (X, 1)) == 2		\
	  || INTVAL (XEXP (X, 1)) == 4)))

/* True if X is an address that doesn't need a prefix i.e. [Rs] or [Rs+].  */
#define SIMPLE_ADDRESS_P(X) \
 (BASE_P (X)						\
  || (GET_CODE (X) == POST_INC				\
      && BASE_P (XEXP (X, 0))))

/* A PIC operand looks like a normal symbol here.  At output we dress it
   in "[rPIC+symbol:GOT]" (global symbol) or "rPIC+symbol:GOTOFF" (local
   symbol) so we exclude all addressing modes where we can't replace a
   plain "symbol" with that.  A global PIC symbol does not fit anywhere
   here (but is thankfully a general_operand in itself).  A local PIC
   symbol is valid for the plain "symbol + offset" case.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)			\
 {								\
   rtx x1, x2;							\
   if (SIMPLE_ADDRESS_P (X))					\
     goto ADDR;							\
   if (CONSTANT_P (X)						\
       && (! flag_pic						\
	   || cris_gotless_symbol (X)				\
	   || ! cris_symbol (X)))				\
     goto ADDR;							\
   /* Indexed?  */						\
   if (GET_CODE (X) == PLUS)					\
     {								\
       x1 = XEXP (X, 0);					\
       x2 = XEXP (X, 1);					\
       /* BDAP o, Rd.  */					\
       if ((BASE_P (x1) && CONSTANT_INDEX_P (x2))		\
	   || (BASE_P (x2) && CONSTANT_INDEX_P (x1))		\
	    /* BDAP Rs[+], Rd.  */				\
	   || (GET_MODE_SIZE (MODE) <= UNITS_PER_WORD		\
	       && ((BASE_P (x1) && BDAP_INDEX_P (x2))		\
		   || (BASE_P (x2) && BDAP_INDEX_P (x1))	\
		   /* BIAP.m Rs, Rd */				\
		   || (BASE_P (x1) && BIAP_INDEX_P (x2))	\
		   || (BASE_P (x2) && BIAP_INDEX_P (x1)))))	\
	 goto ADDR;						\
     }								\
   else if (GET_CODE (X) == MEM)				\
     {								\
       /* DIP (Rs).  Reject [[reg+]] and [[reg]] for		\
	  DImode (long long).  */				\
       if (GET_MODE_SIZE (MODE) <= UNITS_PER_WORD		\
	   && (BASE_P (XEXP (X, 0))				\
	       || BASE_OR_AUTOINCR_P (XEXP (X, 0))))		\
	 goto ADDR;						\
     }								\
 }

#ifndef REG_OK_STRICT
 /* Nonzero if X is a hard reg that can be used as a base reg
    or if it is a pseudo reg.  */
# define REG_OK_FOR_BASE_P(X)			\
 (REGNO (X) <= CRIS_LAST_GENERAL_REGISTER	\
  || REGNO (X) == ARG_POINTER_REGNUM		\
  || REGNO (X) >= FIRST_PSEUDO_REGISTER)
#else
 /* Nonzero if X is a hard reg that can be used as a base reg.  */
# define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#endif

#ifndef REG_OK_STRICT
 /* Nonzero if X is a hard reg that can be used as an index
    or if it is a pseudo reg.  */
# define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)
#else
 /* Nonzero if X is a hard reg that can be used as an index.  */
# define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#endif

/* For now, don't do anything.  GCC does a good job most often.

    Maybe we could do something about gcc:s misbehaviour when it
   recalculates frame offsets for local variables, from fp+offs to
   sp+offs.  The resulting address expression gets screwed up
   sometimes, but I'm not sure that it may be fixed here, since it is
   already split up in several instructions (Is this still true?).
   FIXME: Check and adjust for gcc-2.9x.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN) {}

/* Functionality import from EGCS.
   Kludge to solve Axis-990219: Work around imperfection in
   reload_load_address1:
    (plus (sign_extend (mem:qi (reg))) (reg))
   should be reloaded as (plus (reg) (reg)), not
    (plus (sign_extend (reg)) (reg)).
   There are no checks that reload_load_address_1 "reloads"
   addresses correctly, so invalidness is not caught or
   corrected.
    When the right thing happens, the "something_reloaded" kludge can
   be removed.  The right thing does not appear to happen for
   EGCS CVS as of this date (above).  */

#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND_LEVELS, WIN) \
  do									\
    {									\
      if (GET_CODE (X) == PLUS						\
	  && REG_P (XEXP (X, 1))					\
	  && GET_CODE (XEXP (X, 0)) == SIGN_EXTEND			\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == MEM			\
	  && (GET_MODE (XEXP (XEXP (X, 0), 0)) == HImode		\
	      || GET_MODE (XEXP (XEXP (X, 0), 0)) == QImode)		\
	  && (REG_P (XEXP (XEXP (XEXP (X, 0), 0), 0))			\
	      || (GET_CODE (XEXP (XEXP (XEXP (X, 0), 0), 0))		\
		  == POST_INC						\
		  && REG_P (XEXP (XEXP (XEXP (XEXP (X, 0), 0), 0),	\
				  0)))))				\
	{								\
	  int something_reloaded = 0;					\
									\
	  if (REGNO (XEXP (X, 1)) >= FIRST_PSEUDO_REGISTER)		\
	    {								\
	      /* Second reg is pseudo, reload it.  */			\
	      push_reload (XEXP (X, 1), NULL_RTX, &XEXP (X, 1), 	\
			   NULL,					\
			   GENERAL_REGS, GET_MODE (X), VOIDmode, 0, 0,	\
			   OPNUM, TYPE);				\
	      something_reloaded = 1;					\
	    }								\
									\
	  if (REG_P (XEXP (XEXP (XEXP (X, 0), 0), 0))			\
	      && (REGNO (XEXP (XEXP (XEXP (X, 0), 0), 0))		\
		  >= FIRST_PSEUDO_REGISTER))				\
	    {								\
	      /* First one is a pseudo - reload that.  */		\
	      push_reload (XEXP (XEXP (XEXP (X, 0), 0), 0), NULL_RTX,	\
			   &XEXP (XEXP (XEXP (X, 0), 0), 0), NULL, 	\
			   GENERAL_REGS,				\
			   GET_MODE (X), VOIDmode, 0, 0, OPNUM, TYPE);	\
	      something_reloaded = 1;					\
	    }								\
									\
	  if (! something_reloaded					\
	      || (GET_CODE (XEXP (XEXP (X, 0), 0)) == POST_INC		\
		  && (REGNO (XEXP (XEXP (XEXP (X, 0), 0), 0))		\
		      >= FIRST_PSEUDO_REGISTER)))			\
	    /* Reload the sign_extend.	Happens if neither reg is a	\
	       pseudo, or the first one was inside post_increment.  */	\
	    push_reload (XEXP (X, 0), NULL_RTX, &XEXP (X, 0), NULL,	\
			 GENERAL_REGS, GET_MODE (X), VOIDmode, 0, 0,	\
			 OPNUM, TYPE);					\
	  goto WIN;							\
	}								\
    }									\
  while (0)

/* In CRIS, only the postincrement address mode depends thus,
   since the increment depends on the size of the operand.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)	\
  do							\
    {							\
      if (GET_CODE (ADDR) == POST_INC)			\
	goto LABEL;					\
    }							\
  while (0)

#define LEGITIMATE_CONSTANT_P(X) 1


/* Node: Condition Code */

#define NOTICE_UPDATE_CC(EXP, INSN) cris_notice_update_cc (EXP, INSN)

/* FIXME: Maybe define CANONICALIZE_COMPARISON later, when playing with
   optimizations.  It is needed; currently we do this with instruction
   patterns and NOTICE_UPDATE_CC.  */


/* Node: Costs */

#define CONST_COSTS(RTX, CODE, OUTER_CODE)				\
 case CONST_INT:							\
   if (INTVAL (RTX) == 0)						\
     return 0;								\
   if (INTVAL (RTX) < 32 && INTVAL (RTX) >= -32)			\
     return 1;								\
   /* Eight or 16 bits are a word and cycle more expensive.  */		\
   if (INTVAL (RTX) <= 32767 && INTVAL (RTX) >= -32768)			\
     return 2;								\
   /* A 32 bit constant (or very seldom, unsigned 16 bits) costs	\
      another word.  FIXME: This isn't linear to 16 bits.  */		\
   return 4;								\
 case LABEL_REF:							\
   return 6;								\
 case CONST:								\
 case SYMBOL_REF:							\
   /* For PIC, we need a prefix (if it isn't already there),		\
      and the PIC register.  For a global PIC symbol, we also need a	\
      read of the GOT.  */						\
   return								\
     flag_pic ? (cris_got_symbol (RTX) ? (2 + 4 + 6) : (2 + 6)) : 6;	\
 case CONST_DOUBLE:							\
   if (RTX != CONST0_RTX (GET_MODE (RTX) == VOIDmode ? DImode		\
			  : GET_MODE (RTX)))				\
     return 12;								\
   /* Make 0.0 cheap, else test-insns will not be used.  */		\
   return 0;

#define RTX_COSTS(X, CODE, OUTER_CODE)					\
 case MULT:								\
   /* Identify values that are no powers of two.  Powers of 2 are	\
      taken care of already and those values should not be		\
      changed.  */							\
   if (GET_CODE (XEXP (X, 1)) != CONST_INT				\
       || exact_log2 (INTVAL (XEXP (X, 1)) < 0))			\
     {									\
	/* If we have a multiply insn, then the cost is between		\
	   1 and 2 "fast" instructions.  */				\
	if (TARGET_HAS_MUL_INSNS)					\
	  return COSTS_N_INSNS (1) + COSTS_N_INSNS (1) /2;		\
									\
	/* Estimate as 4 + 4 * #ofbits.  */				\
	return COSTS_N_INSNS (132);					\
     }									\
     break;								\
 case UDIV:								\
 case MOD:								\
 case UMOD:								\
 case DIV:								\
   if (GET_CODE (XEXP (X, 1)) != CONST_INT				\
       || exact_log2 (INTVAL (XEXP (X, 1)) < 0))			\
     /* Estimate this as 4 + 8 * #of bits.  */				\
     return COSTS_N_INSNS (260);					\
									\
 case AND:								\
   if (GET_CODE (XEXP (X, 1)) == CONST_INT				\
       /* Two constants may actually happen before optimization.  */	\
       && GET_CODE (XEXP (X, 0)) != CONST_INT				\
       && !CONST_OK_FOR_LETTER_P (INTVAL (XEXP (X, 1)), 'I'))		\
     return								\
       rtx_cost (XEXP (X, 0), OUTER_CODE) + 2				\
       + 2 * GET_MODE_NUNITS (GET_MODE (XEXP (X, 0)));			\
									\
 case ZERO_EXTEND: case SIGN_EXTEND:					\
   /* Same as move. If embedded in other insn, cost is 0.  */		\
   return rtx_cost (XEXP (X, 0), OUTER_CODE);

#define ADDRESS_COST(X) cris_address_cost (X)

/* FIXME: Need to define REGISTER_MOVE_COST when more register classes are
   introduced.  */

/* This isn't strictly correct for v0..3 in buswidth-8bit mode, but
   should suffice.  */
#define MEMORY_MOVE_COST(M, CLASS, IN) \
 (((M) == QImode) ? 4 : ((M) == HImode) ? 4 : 6)

/* Regardless of the presence of delay slots, the default value of 1 for
   BRANCH_COST is the best in the range (1, 2, 3), tested with gcc-2.7.2
   with testcases ipps and gcc, giving smallest and fastest code.  */

#define SLOW_BYTE_ACCESS 0

/* This is the threshold *below* which inline move sequences of
   word-length sizes will be emitted.  The "9" will translate to
   (9 - 1) * 4 = 32 bytes maximum moved, but using 16 instructions
   (8 instruction sequences) or less.  */
#define MOVE_RATIO 9


/* Node: Sections */

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define FORCE_EH_FRAME_INFO_IN_DATA_SECTION (! TARGET_ELF)

/* The jump table is immediately connected to the preceding insn.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* We need to code in PIC-specific flags into SYMBOL_REF_FLAG.  */

#define ENCODE_SECTION_INFO(EXP) cris_encode_section_info (EXP)

/* We pull a little trick to register the _fini function with atexit,
   after (presumably) registering the eh frame info, since we don't handle
   _fini (a.k.a. ___fini_start) in crt0 or have a crti for "pure" ELF.  If
   you change this, don't forget that you can't have library function
   references (e.g. to atexit) in crtend.o, since those won't be resolved
   to libraries; those are linked in *before* crtend.o.  */
#ifdef CRT_BEGIN
# define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)		\
static void __attribute__((__used__))				\
call_ ## FUNC (void)						\
{								\
  asm (SECTION_OP);						\
  FUNC ();							\
  if (__builtin_strcmp (#FUNC, "frame_dummy") == 0)		\
   {								\
     extern void __fini__start (void);				\
     atexit (__fini__start);					\
   }								\
  asm (TEXT_SECTION_ASM_OP);					\
}
#endif

/* Node: PIC */

#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? 0 : INVALID_REGNUM)

#define LEGITIMATE_PIC_OPERAND_P(X) cris_legitimate_pic_operand (X)


/* Node: File Framework */

/* NO_APP *only at file start* means faster assembly.
   It also means comments are not allowed.
   In some cases comments will be output for debugging purposes.
   Make sure they are allowed then.  */
/* Override previous definitions (elfos.h).  */
#undef ASM_FILE_START
#define ASM_FILE_START(STREAM)					\
  do								\
    {								\
      if (TARGET_PDEBUG || flag_print_asm_name)			\
	fprintf ((STREAM), "#APP\n");				\
      else							\
	fprintf ((STREAM), "#NO_APP\n");			\
      if (TARGET_ELF)						\
	output_file_directive ((STREAM), main_input_filename);	\
    }								\
  while (0)

/* Override previous definitions (elfos.h).  */
#undef ASM_FILE_END
#define ASM_FILE_END(STREAM)

/* We don't want an .ident for gcc.  To avoid that but still support
   #ident, we override ASM_OUTPUT_IDENT and, since the gcc .ident is its
   only use besides ASM_OUTPUT_IDENT, undef IDENT_ASM_OP from elfos.h.  */
#undef IDENT_ASM_OP
#undef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "%s\"%s\"\n", "\t.ident\t", NAME);

#define ASM_APP_ON "#APP\n"

#define ASM_APP_OFF "#NO_APP\n"


/* Node: Data Output */

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) (C) == '@'

/* Node: Uninitialized Data */

/* Remember to round off odd values if we want data alignment,
   since we cannot do that with an .align directive.

   Using .comm causes the space not to be reserved in .bss, but by
   tricks with the symbol type.  Not good if other tools than binutils
   are used on the object files.  Since ".global ... .lcomm ..." works, we
   use that.  Use .._ALIGNED_COMMON, since gcc whines when we only have
   ..._COMMON, and we prefer to whine ourselves; BIGGEST_ALIGNMENT is not
   the one to check.  This done for a.out only.  */
/* FIXME: I suspect a bug in gcc with alignment.  Do not warn until
   investigated; it mucks up the testsuite results.  */
#define CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN, LOCAL) \
  do									\
    {									\
      int align_ = (ALIGN) / BITS_PER_UNIT;				\
      if (TARGET_DATA_ALIGN && TARGET_ALIGN_BY_32 && align_ < 4)	\
	align_ = 4;							\
      else if (TARGET_DATA_ALIGN && align_ < 2)				\
	align_ = 2;							\
      /* FIXME: Do we need this?  */					\
      else if (align_ < 1)						\
	align_ = 1;							\
									\
      if (TARGET_ELF)							\
	{								\
	  if (LOCAL)							\
	    {								\
	      fprintf ((FILE), "%s", LOCAL_ASM_OP);			\
	      assemble_name ((FILE), (NAME));				\
	      fprintf ((FILE), "\n");					\
	    }								\
	  fprintf ((FILE), "%s", COMMON_ASM_OP);			\
	  assemble_name ((FILE), (NAME));				\
	  fprintf ((FILE), ",%u,%u\n", (SIZE), align_);			\
	}								\
      else								\
	{								\
	  /* We can't tell a one-only or weak COMM from a "global	\
	     COMM" so just make all non-locals weak.  */		\
	  if (! (LOCAL))						\
	    ASM_WEAKEN_LABEL (FILE, NAME);				\
	  fputs ("\t.lcomm ", (FILE));					\
	  assemble_name ((FILE), (NAME));				\
	  fprintf ((FILE), ",%u\n",					\
		   ((SIZE) + (align_ - 1)) & ~(align_ - 1));		\
	}								\
    }									\
  while (0)

#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN) \
 CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN, 0)

#undef ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN) \
 CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN, 1)

/* FIXME: define ASM_OUTPUT_SHARED_COMMON and emit an error when it is
   used with -melinux and a.out.  */

/* Node: Label Output */

#define ASM_OUTPUT_LABEL(FILE, NAME)		\
  do						\
    {						\
      assemble_name (FILE, NAME);		\
      fputs (":\n", FILE);			\
    }						\
  while (0)

#define ASM_GLOBALIZE_LABEL(FILE, NAME)		\
  do						\
    {						\
      fputs ("\t.global ", FILE);		\
      assemble_name (FILE, NAME);		\
      fputs ("\n", FILE);			\
    }						\
  while (0)

#define SUPPORTS_WEAK 1

/* FIXME: This macro isn't documented, but this would probably be an
   appropriate location.  It's only used in crtstuff.c, else we'd have to
   handle (to #undef or ignore it) in a.out.  */
#define HAVE_GAS_HIDDEN 1

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)	\
  do							\
    {							\
      asm_fprintf (FILE, "%L%s%d:\n", PREFIX, NUM);	\
    }							\
  while (0)

/* Remove any previous definition (elfos.h).  */
#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*%s%s%ld", LOCAL_LABEL_PREFIX, PREFIX, (long) NUM)

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)		\
  do								\
    {								\
      (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10);	\
      sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO));		\
    }								\
  while (0)


/* Node: Initialization */
/* (no definitions) */

/* Node: Macros for Initialization */
/* (no definitions) */

/* Node: Instruction Output */

#define REGISTER_NAMES					\
 {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8",	\
  "r9", "r10", "r11", "r12", "r13", "sp", "pc", "srp", "faked_ap"}

#define ADDITIONAL_REGISTER_NAMES \
 {{"r14", 14}, {"r15", 15}}

#define PRINT_OPERAND(FILE, X, CODE)		\
 cris_print_operand (FILE, X, CODE)

/* For delay-slot handling.  */
#define PRINT_OPERAND_PUNCT_VALID_P(CODE) (CODE == '#')

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)	\
   cris_print_operand_address (FILE, ADDR)

/* Output an empty line to illustrate the presence of the delay slot.  */
#define DBR_OUTPUT_SEQEND(FILE) \
  fprintf (FILE, "\n")

#define LOCAL_LABEL_PREFIX (TARGET_ELF ? "." : "")

/* cppinit.c initializes a const array from this, so it must be constant,
   can't have it different based on options.  Luckily, the prefix is
   always allowed, so let's have it on all GCC-generated code.  Note that
   we have this verbatim everywhere in the back-end, not using %R or %s or
   such.  */
#define REGISTER_PREFIX "$"

/* Remove any previous definition (elfos.h).  */
/* We use -fno-leading-underscore to remove it, when necessary.  */
#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO) \
  fprintf (FILE, "\tpush $%s\n", reg_names[REGNO])

#define ASM_OUTPUT_REG_POP(FILE, REGNO) \
  fprintf (FILE, "\tpop $%s\n", reg_names[REGNO])


/* Node: Dispatch Tables */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)	\
  asm_fprintf (FILE, "\t.word %LL%d-%LL%d\n", VALUE, REL)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  asm_fprintf (FILE, "\t.dword %LL%d\n", VALUE)

/* Defined to also emit an .align in elfos.h.  We don't want that.  */
#undef ASM_OUTPUT_CASE_LABEL

/* Since the "bound" insn loads the comparison value if the compared<
   value (register) is out of bounds (0..comparison value-1), we need
   to output another case to catch it.
   The way to find it is to look for the label_ref at the else-arm inside
   the expanded casesi core-insn.
   FIXME: Check this construct when changing to new version of gcc.  */
#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE)				\
  do									\
    {									\
      asm_fprintf (STREAM, "\t.word %LL%d-%LL%d%s\n",			\
		   CODE_LABEL_NUMBER					\
		    (XEXP (XEXP (XEXP					\
				  (XVECEXP				\
				    (PATTERN (PREV_INSN (PREV_INSN	\
							  (TABLE))),	\
				     0, 0), 1), 2), 0)),		\
		   NUM,							\
		   (TARGET_PDEBUG ? "; default" : ""));			\
    }									\
  while (0)


/* Node: Exception Region Output */
/* (no definitions) */
/* FIXME: Fill in with our own optimized layout.  */

/* Node: Alignment Output */

#define ASM_OUTPUT_ALIGN(FILE, LOG)  \
 fprintf (FILE, "\t.align %d\n", (LOG))


/* Node: All Debuggers */

#define DBX_REGISTER_NUMBER(REGNO) \
 ((REGNO) == CRIS_SRP_REGNUM ? CRIS_CANONICAL_SRP_REGNUM : (REGNO))

/* FIXME: Investigate DEBUGGER_AUTO_OFFSET, DEBUGGER_ARG_OFFSET.  */


/* Node: DBX Options */

/* Is this correct? Check later.  */
#define DBX_NO_XREFS

#define DBX_CONTIN_LENGTH 0

/* FIXME: Is this needed when we have 0 DBX_CONTIN_LENGTH?  */
#define DBX_CONTIN_CHAR '?'


/* Node: DBX Hooks */
/* (no definitions) */

/* Node: File names and DBX */
/* (no definitions) */


/* Node: SDB and DWARF */
/* (no definitions) */

/* Node: Cross-compilation */
#define REAL_ARITHMETIC


/* Node: Misc */

/* FIXME: Check this one more time.  */
#define PREDICATE_CODES					\
 {"cris_orthogonal_operator",				\
  {PLUS, MINUS, IOR, AND, UMIN}},			\
 {"cris_commutative_orth_op",				\
  {PLUS, IOR, AND, UMIN}},				\
 {"cris_operand_extend_operator",			\
  {PLUS, MINUS, UMIN}},					\
 {"cris_extend_operator",				\
  {ZERO_EXTEND, SIGN_EXTEND}},				\
 {"cris_plus_or_bound_operator",			\
  {PLUS, UMIN}},					\
 {"cris_bdap_operand",					\
  {SUBREG, REG, LABEL_REF, SYMBOL_REF, MEM, CONST_INT,	\
   CONST_DOUBLE, CONST, SIGN_EXTEND}},			\
 {"cris_bdap_biap_operand",				\
  {SUBREG, REG, LABEL_REF, SYMBOL_REF, MEM, CONST_INT,	\
   CONST_DOUBLE, CONST, SIGN_EXTEND, MULT}},		\
 {"cris_general_operand_or_gotless_symbol",		\
  {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,		\
   LABEL_REF, SUBREG, REG, MEM}},			\
 {"cris_general_operand_or_symbol",			\
  {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,		\
   LABEL_REF, SUBREG, REG, MEM}},			\
 {"cris_general_operand_or_plt_symbol",			\
  {CONST_INT, CONST_DOUBLE, CONST, SYMBOL_REF,		\
   LABEL_REF, SUBREG, REG, MEM}},			\
 {"cris_mem_call_operand",				\
  {MEM}},

/* A combination of the bound (umin) insn together with a
   sign-extended add via the table to PC seems optimal.
   If the table overflows, the assembler will take care of it.
   Theoretically, in extreme cases (uncertain if they occur), an error
   will be emitted, so FIXME: Check how large case-tables are emitted,
   possible add an option to emit SImode case-tables.  */
#define CASE_VECTOR_MODE HImode

#define CASE_VECTOR_PC_RELATIVE 1

/* FIXME: Investigate CASE_VECTOR_SHORTEN_MODE to make sure HImode is not
   used when broken-.word could possibly fail (plus test-case).  */

#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* This is the number of bytes that can be moved in one
   reasonably fast instruction sequence.  For CRIS, this is two
   instructions: mem => reg, reg => mem.  */
#define MOVE_MAX 4

/* Maybe SHIFT_COUNT_TRUNCATED is safe to define?  FIXME: Check later.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define STORE_FLAG_VALUE 1

#define Pmode SImode

#define FUNCTION_MODE QImode

#define NO_IMPLICIT_EXTERN_C

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
