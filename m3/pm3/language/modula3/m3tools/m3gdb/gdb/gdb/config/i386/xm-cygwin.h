/* Definitions for hosting on WIN32, for GDB.
   Copyright 1995, 1996, 1997, 1998, 2001 Free Software Foundation, Inc.

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

#define HOST_BYTE_ORDER LITTLE_ENDIAN

#include "fopen-bin.h"

#define GDBINIT_FILENAME "gdb.ini"

/* Define this if source files use \r\n rather than just \n.  */
#define CRLF_SOURCE_FILES

#define HAVE_SIGSETMASK 0

/* If under Cygwin, provide backwards compatibility with older
   Cygwin compilers that don't define the current cpp define. */
#ifdef __CYGWIN32__
#ifndef __CYGWIN__
#define __CYGWIN__
#endif
#endif 
