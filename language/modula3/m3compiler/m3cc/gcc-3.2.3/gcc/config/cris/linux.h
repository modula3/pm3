/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
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

/* This file defines the macros for cris-axis-linux-gnu that are not
   covered by cris.h, elfos.h and (config/)linux.h.  */


/* Node: Instruction Output */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Node: Driver */
/* These macros are CRIS-specific, but used in target driver macros.  */

#undef CRIS_CPP_SUBTARGET_SPEC
#define CRIS_CPP_SUBTARGET_SPEC \
  "-D__gnu_linux__ -D__linux__ -D__unix__  -D__ELF__\
   %{pthread:-D_REENTRANT}\
   %{fPIC|fpic: -D__PIC__ -D__pic__}\
   %{!fleading-underscore:-fno-leading-underscore -D__NO_UNDERSCORES__}\
   %{!march=*:%{!cpu=*:-D__arch_v10 -D__CRIS_arch_version=10}}\
   %{!ansi:%{!std=*:%{!undef:-Dlinux -Dunix}\
     -Asystem(unix) -Asystem(posix) -Acpu(cris) -Amachine(cris)}}"

#undef CRIS_CC1_SUBTARGET_SPEC
#define CRIS_CC1_SUBTARGET_SPEC \
 "%{!march=*:%{!cpu=*:-march=v10}}"

#undef CRIS_ASM_SUBTARGET_SPEC
#define CRIS_ASM_SUBTARGET_SPEC \
 "--em=criself\
  %{!fleading-underscore:--no-underscore}\
  %{fPIC|fpic: --pic}"

/* Provide a legacy -mlinux option.  */
#undef CRIS_SUBTARGET_SWITCHES
#define CRIS_SUBTARGET_SWITCHES						\
 {"linux",				 0, ""},			\
 {"gotplt",	 -TARGET_MASK_AVOID_GOTPLT, ""},			\
 {"no-gotplt",	  TARGET_MASK_AVOID_GOTPLT,				\
  N_("Together with -fpic and -fPIC, do not use GOTPLT references")},

#undef CRIS_SUBTARGET_DEFAULT
#define CRIS_SUBTARGET_DEFAULT			\
  (TARGET_MASK_SVINTO				\
   + TARGET_MASK_ETRAX4_ADD			\
   + TARGET_MASK_ALIGN_BY_32			\
   + TARGET_MASK_ELF				\
   + TARGET_MASK_LINUX)

#undef CRIS_DEFAULT_CPU_VERSION
#define CRIS_DEFAULT_CPU_VERSION CRIS_CPU_NG

#undef CRIS_SUBTARGET_VERSION
#define CRIS_SUBTARGET_VERSION " - cris-axis-linux-gnu"

/* We need an -rpath-link to ld.so.1, and presumably to each directory
   specified with -B.  */
#undef CRIS_LINK_SUBTARGET_SPEC
#define CRIS_LINK_SUBTARGET_SPEC \
 "-mcrislinux\
  -rpath-link include/asm/../..%s\
  %{shared} %{static}\
  %{symbolic:-Bdynamic} %{shlib:-Bdynamic} %{static:-Bstatic}\
  %{!shared:%{!static:%{rdynamic:-export-dynamic}}}\
  %{!r:%{O2|O3: --gc-sections}}"


/* Node: Sections */

/* GNU/Linux has crti and crtn and does not need the
   CRT_CALL_STATIC_FUNCTION trick in cris.h.  */
#undef CRT_CALL_STATIC_FUNCTION

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
