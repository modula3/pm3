/* Native-dependent definitions for Sparc running SVR4.
   Copyright 1994 Free Software Foundation, Inc.

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

/* Include the generic SVR4 definitions.  */

#include <nm-sysv4.h>

/* Before storing, we need to read all the registers.  */

#define CHILD_PREPARE_TO_STORE() read_register_bytes (0, NULL, REGISTER_BYTES)

/* Solaris PSRVADDR support does not seem to include a place for nPC.  */

#define PRSVADDR_BROKEN

#ifdef HAVE_THREAD_DB_LIB

#ifdef __STDC__
struct objfile;
#endif

#define target_new_objfile(OBJFILE) sol_thread_new_objfile (OBJFILE)

void sol_thread_new_objfile PARAMS ((struct objfile *objfile));

#endif
