/* Macro definitions for i386 running under NetBSD.
   Copyright 1994, 1996, 2000 Free Software Foundation, Inc.

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

#ifndef TM_NBSD_H
#define TM_NBSD_H

#define HAVE_I387_REGS

#include "i386/tm-i386.h"
#include "config/tm-nbsd.h"

extern use_struct_convention_fn i386nbsd_use_struct_convention;
#define USE_STRUCT_CONVENTION(gcc_p, type) \
	i386nbsd_use_struct_convention(gcc_p, type)


#define JB_ELEMENT_SIZE sizeof(int)	/* jmp_buf[_JBLEN] is array of ints */
#define JB_PC	0		/* Setjmp()'s return PC saved here */

/* Figure out where the longjmp will land.  Slurp the args out of the stack.
   We expect the first arg to be a pointer to the jmp_buf structure from which
   we extract the pc (JB_PC) that we will land at.  The pc is copied into ADDR.
   This routine returns true on success */

extern int get_longjmp_target (CORE_ADDR *);

#define GET_LONGJMP_TARGET(ADDR) get_longjmp_target(ADDR)


/* Support for signal handlers.  */

/* The sigtramp is above the user stack and immediately below the 
   user area.  Using constants here allows for cross debugging. */

#define SIGTRAMP_START(pc)	0xbfbfdf20
#define SIGTRAMP_END(pc)	0xbfbfdff0

/* Offset to saved PC in sigcontext, from <sys/signal.h>.  */
#define SIGCONTEXT_PC_OFFSET 20

#endif /* TM_NBSD_H */
