/* Configuration for GNU C-compiler for Motorola 68000 family.
   SysV68 Motorola 3300 Delta Series
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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


#define USG 1

#include "m68k/xm-m68k.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#define rindex strrchr
#define index strchr

#define NO_SYS_SIGLIST

/* do not use alloca from -lPW with cc, because function epilogues use %sp */
#ifndef __GNUC__
#define USE_C_ALLOCA
extern char *alloca ();
#endif

/* Override part of the obstack macros.  */

#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))
