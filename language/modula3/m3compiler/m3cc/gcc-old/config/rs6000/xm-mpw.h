/* CYGNUS LOCAL mpw (entire file) */
/* PowerPC-based Macintosh MPW host definitions for GNU C Compiler.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

#ifndef MPW
#define MPW
#endif

/* Remove when we're confident struct fields bugs are fixed.  */
#ifndef __GNUC__
#define ONLY_INT_FIELDS
#endif

/* #defines that need visibility everywhere.  */
#define	FALSE	0
#define	TRUE	1

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR	 8
#define HOST_BITS_PER_SHORT	16
#define HOST_BITS_PER_INT	32
#define HOST_BITS_PER_LONG	32
#define HOST_BITS_PER_LONGLONG	64

#define HOST_WORDS_BIG_ENDIAN

/* Target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.  */

#include "tm.h"

/* Definitions common to all MPW hosts.
   Use angle brackets so that parent dir's xm-mpw.h is found.  */

#include <xm-mpw.h>
