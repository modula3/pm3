/* Simulate breakpoints by patching locations in the target system, for GDB.
   Copyright 1990, 1991, 1995 Free Software Foundation, Inc.
   Contributed by Cygnus Support.  Written by John Gilmore.

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

/* Either BREAKPOINT should be defined, or both of LITTLE_BREAKPOINT,
   BIG_BREAKPOINT should be defined.  */

#if defined (BREAKPOINT) || (defined (LITTLE_BREAKPOINT) && defined (BIG_BREAKPOINT))

/* This file is only useful if BREAKPOINT is set.  If not, we punt.  */

#include "symtab.h"
#include "breakpoint.h"
#include "inferior.h"
#include "target.h"

/* If the target isn't bi-endian, just pretend it is.  */
#if defined(BREAKPOINT) && !defined (LITTLE_BREAKPOINT) && !defined (BIG_BREAKPOINT)
#define LITTLE_BREAKPOINT BREAKPOINT
#define BIG_BREAKPOINT BREAKPOINT
#endif

/* This is the sequence of bytes we insert for a breakpoint.  On some
   machines, breakpoints are handled by the target environment and we
   don't have to worry about them here.  */

static unsigned char big_break_insn[] = BIG_BREAKPOINT;
static unsigned char little_break_insn[] = LITTLE_BREAKPOINT;

/* FIXME: We assume big and little breakpoints are the same size.  */
#define BREAKPOINT_LEN (sizeof (big_break_insn))

/* Insert a breakpoint on targets that don't have any better breakpoint
   support.  We read the contents of the target location and stash it,
   then overwrite it with a breakpoint instruction.  ADDR is the target
   location in the target machine.  CONTENTS_CACHE is a pointer to 
   memory allocated for saving the target contents.  It is guaranteed
   by the caller to be long enough to save BREAKPOINT_LEN bytes (this
   is accomplished via BREAKPOINT_MAX).  */

int
memory_insert_breakpoint (addr, contents_cache)
     CORE_ADDR addr;
     char *contents_cache;
{
  int val;

  val = target_read_memory (addr, contents_cache, BREAKPOINT_LEN);

  if (val == 0)
    {
      if (TARGET_BYTE_ORDER == BIG_ENDIAN)
	val = target_write_memory (addr, (char *)big_break_insn,
				   BREAKPOINT_LEN);
      else
	val = target_write_memory (addr, (char *)little_break_insn,
				   BREAKPOINT_LEN);
    }

  return val;
}


int
memory_remove_breakpoint (addr, contents_cache)
     CORE_ADDR addr;
     char *contents_cache;
{
  return target_write_memory (addr, contents_cache, BREAKPOINT_LEN);
}


/* FIXME: This is a hack and should depend on the debugging target.
   See comment in breakpoint.c where this is used.  */

int memory_breakpoint_size = BREAKPOINT_LEN;


#else  /* BREAKPOINT */

char nogo[] = "Breakpoints not implemented for this target.";

int
memory_insert_breakpoint (addr, contents_cache)
     CORE_ADDR addr;
     char *contents_cache;
{
  error (nogo);
  return 0;	/* lint */
}

int
memory_remove_breakpoint (addr, contents_cache)
     CORE_ADDR addr;
     char *contents_cache;
{
  error (nogo);
  return 0;	/* lint */
}

int memory_breakpoint_size = -1;

#endif /* BREAKPOINT */
