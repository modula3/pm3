/* Target definitions for GNU compiler for Intel 80386 running Solaris 2
   Copyright (C) 1993, 1995, 1996 Free Software Foundation, Inc.
   Contributed by Fred Fish (fnf@cygnus.com).

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

#include "i386/sysv4.h"

/* The Solaris 2.0 x86 linker botches alignment of code sections.
   It tries to align to a 16 byte boundary by padding with 0x00000090
   ints, rather than 0x90 bytes (nop).  This generates trash in the
   ".init" section since the contribution from crtbegin.o is only 7
   bytes.  The linker pads it to 16 bytes with a single 0x90 byte, and
   two 0x00000090 ints, which generates a segmentation violation when
   executed.  This macro forces the assembler to do the padding, since
   it knows what it is doing. */

#define FORCE_INIT_SECTION_ALIGN do { asm (ALIGN_ASM_OP ## " 16"); } while (0)
#define FORCE_FINI_SECTION_ALIGN FORCE_INIT_SECTION_ALIGN

/* Add "sun" to the list of symbols defined for SVR4.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-Di386 -Dunix -D__svr4__ -D__SVR4 -Dsun \
   -Asystem(unix) -Asystem(svr4) -Acpu(i386) -Amachine(i386)"

#undef CPP_SPEC
#define CPP_SPEC "\
   %{compat-bsd:-iwithprefixbefore ucbinclude -I/usr/ucbinclude}"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{compat-bsd:-lucb -lsocket -lnsl -lelf -laio} %{!shared:%{!symbolic:-lc}}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %{pg:crtn.o%s}%{!pg:crtn.o%s}"

/* This should be the same as in svr4.h, except with -R added.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy -z text} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:-Y P,/usr/ccs/lib:/usr/lib}}} \
   %{Qy:} %{!Qn:-Qy}"

/* This defines which switch letters take arguments.
   It is as in svr4.h but with -R added.  */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
  (DEFAULT_SWITCH_TAKES_ARG(CHAR) \
   || (CHAR) == 'R' \
   || (CHAR) == 'h' \
   || (CHAR) == 'z')

