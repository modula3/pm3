/* Native definitions for alpha running Linux.
   Copyright 1993, 1994, 1996, 1998, 2000, 2001
   Free Software Foundation, Inc.

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

#ifndef NM_LINUX_H
#define NM_LINUX_H

#include "nm-linux.h"

/* Figure out where the longjmp will land.  We expect that we have just entered
   longjmp and haven't yet setup the stack frame, so the args are still in the
   argument regs.  A0_REGNUM points at the jmp_buf structure from which we
   extract the pc (JB_PC) that we will land at.  The pc is copied into ADDR.
   This routine returns true on success */

#define GET_LONGJMP_TARGET(ADDR) get_longjmp_target(ADDR)
extern int get_longjmp_target (CORE_ADDR *);

/* ptrace register ``addresses'' are absolute.  */

#define U_REGS_OFFSET 0

/* FIXME: This is probably true, or should be, on all Linux ports.
   IA64?  Sparc64?  */
#define PTRACE_ARG3_TYPE long

/* ptrace transfers longs, the ptrace man page is lying.  */

#define PTRACE_XFER_TYPE long

/* The alpha does not step over a breakpoint, the manpage is lying again.  */

#define CANNOT_STEP_BREAKPOINT

/* Linux has shared libraries.  */

#define GDB_TARGET_HAS_SHARED_LIBS

/* Given a pointer to either a gregset_t or fpregset_t, return a
   pointer to the first register.  */
#define ALPHA_REGSET_BASE(regsetp)  ((long *) (regsetp))

/* FIXME: kettenis/2000-09-03: This should be moved to ../nm-linux.h
   once we have converted all Linux targets to use the new threads
   stuff (without the #undef of course).  */

extern int lin_lwp_prepare_to_proceed (void);
#undef PREPARE_TO_PROCEED
#define PREPARE_TO_PROCEED(select_it) lin_lwp_prepare_to_proceed ()

extern void lin_lwp_attach_lwp (ptid_t pid, int verbose);
#define ATTACH_LWP(ptid, verbose) lin_lwp_attach_lwp ((ptid), (verbose))

#include <signal.h>

extern void lin_thread_get_thread_signals (sigset_t *mask);
#define GET_THREAD_SIGNALS(mask) lin_thread_get_thread_signals (mask)

#endif /* NM_LINUX_H */
