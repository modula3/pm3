/* Native-dependent definitions for Linux/MIPS.
   Copyright 1996, 2001 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef NM_MIPSLINUX_H
#define NM_MIPSLINUX_H

#define MIPS_GNULINUX_TARGET

#include "nm-linux.h"

/* Return sizeof user struct to callers in less machine dependent
   routines.  Hard coded for cross-compilation friendliness. */

#define KERNEL_U_SIZE 504

/* ptrace register ``addresses'' are absolute.  */

#define U_REGS_OFFSET 0

/* ptrace transfers longs, and expects addresses as longs.  */

#define PTRACE_ARG3_TYPE long
#define PTRACE_XFER_TYPE long

#define REGISTER_U_ADDR(addr, blockend, regno) \
  (addr) = mips_register_addr ((regno),(blockend))

int mips_linux_cannot_fetch_register (int regno);
int mips_linux_cannot_store_register (int regno);
#define CANNOT_FETCH_REGISTER(regno) mips_linux_cannot_fetch_register (regno)
#define CANNOT_STORE_REGISTER(regno) mips_linux_cannot_store_register (regno)

/* FIXME: This should be moved to ../nm-linux.h once we have converted all
   Linux targets to use the new threads stuff (without the #undef of
   course).  */

extern int lin_lwp_prepare_to_proceed (void);
#undef PREPARE_TO_PROCEED
#define PREPARE_TO_PROCEED(select_it) lin_lwp_prepare_to_proceed ()

extern void lin_lwp_attach_lwp (ptid_t ptid, int verbose);
#define ATTACH_LWP(ptid, verbose) lin_lwp_attach_lwp ((ptid), (verbose))

#include <signal.h>

extern void lin_thread_get_thread_signals (sigset_t *mask);
#define GET_THREAD_SIGNALS(mask) lin_thread_get_thread_signals (mask)

#endif /* NM_MIPSLINUX_H */
