/* collection of junk waiting time to sort out
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

This file is part of the GNU Simulators.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef M32R_SIM_H
#define M32R_SIM_H

#define PC_REGNUM	21
#define ACCL_REGNUM	22
#define ACCH_REGNUM	23

/* This is invoked by the nop pattern in the .cpu file.  */
#if WITH_PROFILE_INSN_P
#define PROFILE_COUNT_FILLNOPS(cpu, addr) \
do { \
  if (CPU_PROFILE_FLAGS (cpu) [PROFILE_INSN_IDX] \
      && (addr & 3) != 0) \
    ++ CPU_M32R_MISC_PROFILE (cpu).fillnop_count; \
} while (0)
#else
#define PROFILE_COUNT_FILLNOPS(cpu, addr)
#endif

#define GETTWI GETTSI
#define SETTWI SETTSI

#define BRANCH_NEW_PC(current_cpu, var, addr) \
do { var = (addr); } while (0)

/* Support for the MSPR register.
   This is needed in order for overlays to work correctly with the scache.  */

/* Address of the MSPR register (Cache Purge Control Register).  */
#define MSPR_ADDR 0xfffffff7
/* sim_core_attach device argument.  */
extern device m32r_mspr_device;

/* FIXME: Temporary, until device support ready.  */
struct _device { int foo; };

#endif /* M32R_SIM_H */
