/* Definitions of target machine for GNU compiler, for HP-UX using GNU as.
   Copyright (C) 1996 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 0x88		/* TARGET_GAS + TARGET_JUMP_IN_DELAY */

#define DBX_DEBUGGING_INFO
#undef DEFAULT_GDB_EXTENSIONS
#define DEFAULT_GDB_EXTENSIONS 1
