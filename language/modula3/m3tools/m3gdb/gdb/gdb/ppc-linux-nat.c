/* PPC linux native support.
   Copyright 1988, 1989, 1991, 1992, 1994, 1996, 2000, 2001
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
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "frame.h"
#include "inferior.h"
#include "gdbcore.h"
#include "regcache.h"

#include <sys/types.h>
#include <sys/param.h>
#include <signal.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/procfs.h>

/* Prototypes for supply_gregset etc. */
#include "gregset.h"

int
kernel_u_size (void)
{
  return (sizeof (struct user));
}

static int regmap[] =
{PT_R0, PT_R1, PT_R2, PT_R3, PT_R4, PT_R5, PT_R6, PT_R7,
 PT_R8, PT_R9, PT_R10, PT_R11, PT_R12, PT_R13, PT_R14, PT_R15,
 PT_R16, PT_R17, PT_R18, PT_R19, PT_R20, PT_R21, PT_R22, PT_R23,
 PT_R24, PT_R25, PT_R26, PT_R27, PT_R28, PT_R29, PT_R30, PT_R31,
 PT_FPR0, PT_FPR0 + 2, PT_FPR0 + 4, PT_FPR0 + 6, PT_FPR0 + 8, PT_FPR0 + 10, PT_FPR0 + 12, PT_FPR0 + 14,
 PT_FPR0 + 16, PT_FPR0 + 18, PT_FPR0 + 20, PT_FPR0 + 22, PT_FPR0 + 24, PT_FPR0 + 26, PT_FPR0 + 28, PT_FPR0 + 30,
 PT_FPR0 + 32, PT_FPR0 + 34, PT_FPR0 + 36, PT_FPR0 + 38, PT_FPR0 + 40, PT_FPR0 + 42, PT_FPR0 + 44, PT_FPR0 + 46,
 PT_FPR0 + 48, PT_FPR0 + 50, PT_FPR0 + 52, PT_FPR0 + 54, PT_FPR0 + 56, PT_FPR0 + 58, PT_FPR0 + 60, PT_FPR0 + 62,
 PT_NIP, PT_MSR, PT_CCR, PT_LNK, PT_CTR, PT_XER, PT_MQ};

int 
ppc_register_u_addr (int ustart, int regnum)
{
  return (ustart + 4 * regmap[regnum]);
}

void
supply_gregset (gdb_gregset_t *gregsetp)
{
  int regi;
  register elf_greg_t *regp = (elf_greg_t *) gregsetp;

  for (regi = 0; regi < 32; regi++)
    supply_register (regi, (char *) (regp + regi));

  for (regi = FIRST_UISA_SP_REGNUM; regi <= LAST_UISA_SP_REGNUM; regi++)
    supply_register (regi, (char *) (regp + regmap[regi]));
}

void
fill_gregset (gdb_gregset_t *gregsetp, int regno)
{
  int regi;
  elf_greg_t *regp = (elf_greg_t *) gregsetp;

#define COPY_REG(_idx_,_regi_) \
  if ((regno == -1) || regno == _regi_) \
    memcpy (regp + _idx_, &registers[REGISTER_BYTE (_regi_)], \
	    REGISTER_RAW_SIZE (_regi_))

  for (regi = 0; regi < 32; regi++)
    {
      COPY_REG (regmap[regi], regi);
    }

  for (regi = FIRST_UISA_SP_REGNUM; regi <= LAST_UISA_SP_REGNUM; regi++)
    {
      COPY_REG (regmap[regi], regi);
    }
}

void
supply_fpregset (gdb_fpregset_t * fpregsetp)
{
  int regi;
  for (regi = 0; regi < 32; regi++)
    {
      supply_register (FP0_REGNUM + regi, (char *) (*fpregsetp + regi));
    }
}

/*  Given a pointer to a floating point register set in /proc format
   (fpregset_t *), update the register specified by REGNO from gdb's idea
   of the current floating point register set.  If REGNO is -1, update
   them all. */

void
fill_fpregset (gdb_fpregset_t *fpregsetp, int regno)
{
  int regi;
  char *to;
  char *from;
  
  for (regi = 0; regi < 32; regi++)
    {
      if ((regno == -1) || (regno == FP0_REGNUM + regi))
        {
	  from = (char *) &registers[REGISTER_BYTE (FP0_REGNUM + regi)];
	  to = (char *) (*fpregsetp + regi);
	  memcpy (to, from, REGISTER_RAW_SIZE (FP0_REGNUM + regi));
        }
    }
}
