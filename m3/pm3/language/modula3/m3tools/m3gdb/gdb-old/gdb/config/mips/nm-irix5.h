/* Definitions for native support of irix5.

Copyright (C) 1993 Free Software Foundation, Inc.

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

#include "nm-sysv4.h"

#define TARGET_HAS_HARDWARE_WATCHPOINTS

/* Temporary new watchpoint stuff */
#define TARGET_CAN_USE_HARDWARE_WATCHPOINT(type, cnt, ot) \
	((type) == bp_hardware_watchpoint)

/* When a hardware watchpoint fires off the PC will be left at the
   instruction which caused the watchpoint.  It will be necessary for
   GDB to step over the watchpoint. */

#define STOPPED_BY_WATCHPOINT(W) \
  procfs_stopped_by_watchpoint(inferior_pid)
extern int procfs_stopped_by_watchpoint PARAMS ((int));

#define HAVE_NONSTEPPABLE_WATCHPOINT

/* Use these macros for watchpoint insertion/deletion.  */
/* type can be 0: write watch, 1: read watch, 2: access watch (read/write) */
#define target_insert_watchpoint(addr, len, type) procfs_set_watchpoint (inferior_pid, addr, len, 2)
#define target_remove_watchpoint(addr, len, type) procfs_set_watchpoint (inferior_pid, addr, 0, 0)
extern int procfs_set_watchpoint PARAMS ((int, CORE_ADDR, int, int));
