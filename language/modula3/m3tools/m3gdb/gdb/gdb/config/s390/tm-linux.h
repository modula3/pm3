/* Target definitions for GDB for a s390 running Linux.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by D.J. Barrow (djbarrow@de.ibm.com,barrow_dj@yahoo.com)
   for IBM Deutschland Entwicklung GmbH, IBM Corporation.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#ifndef TM_LINUX_H
#define TM_LINUX_H
#ifdef GDBSERVER
#define S390_GNULINUX_TARGET
#endif /* GDBSERVER */
#undef  TARGET_ELF64
#define TARGET_ELF64 (gdbarch_tdep (current_gdbarch)->intreg_size==8)

#include "config/tm-linux.h"

/* Zap several macros defined in the above header so that multi-arch
   can safely re-define them.  The ``correct fix'' involves
   eliminating either the above include or even this file.  */
#undef SKIP_TRAMPOLINE_CODE

#include "s390/tm-s390.h"



#endif /* TM_LINUX_H */
