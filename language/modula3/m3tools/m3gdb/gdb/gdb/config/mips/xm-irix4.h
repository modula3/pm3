/* Definitions for irix4 hosting support.

   Copyright 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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

/* This is for the iris. */

#include "mips/xm-irix3.h"

#define BROKEN_SIGINFO_H	/* <sys/siginfo.h> si_pid & si_uid are bogus */

/* Irix 4.0.1 and later have termios.  Not sure about earlier versions.  */
#undef HAVE_TERMIO
#define HAVE_TERMIOS

/* This enables reliable signals (and the associated setjmp/longjmp), and gives
   bsdish prototypes for getpgrp/setpgrg/setgroups and initgroups.  */
#define _BSD_COMPAT
