/* Definitions for Intel 386 running MOSS
   Copyright (C) 1996 Free Software Foundation, Inc.
   Contributed by Bryan Ford <baford@cs.utah.edu>

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

/* I believe in reuse... */
#include "i386/linux.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Di386 -Dmoss -Asystem(posix) -Acpu(i386) -Amachine(i386)"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "crt0.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC  "crtn.o%s"

#undef	LINK_SPEC

