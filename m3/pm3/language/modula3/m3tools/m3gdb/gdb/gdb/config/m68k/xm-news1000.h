/* OBSOLETE /* Parameters for a Sony/NEWS series 1000 with News-OS version 3, */
/* OBSOLETE    for GDB, the GNU debugger. */
/* OBSOLETE    Copyright (C) 1990 Free Software Foundation, Inc. */
/* OBSOLETE  */
/* OBSOLETE    This file is part of GDB. */
/* OBSOLETE  */
/* OBSOLETE    This program is free software; you can redistribute it and/or modify */
/* OBSOLETE    it under the terms of the GNU General Public License as published by */
/* OBSOLETE    the Free Software Foundation; either version 2 of the License, or */
/* OBSOLETE    (at your option) any later version. */
/* OBSOLETE  */
/* OBSOLETE    This program is distributed in the hope that it will be useful, */
/* OBSOLETE    but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* OBSOLETE    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/* OBSOLETE    GNU General Public License for more details. */
/* OBSOLETE  */
/* OBSOLETE    You should have received a copy of the GNU General Public License */
/* OBSOLETE    along with this program; if not, write to the Free Software */
/* OBSOLETE    Foundation, Inc., 59 Temple Place - Suite 330, */
/* OBSOLETE    Boston, MA 02111-1307, USA.  */ */
/* OBSOLETE  */
/* OBSOLETE /* This is required by Sony include files like <sys/user.h> so we */
/* OBSOLETE    get the right offset into the u area.  Relying on the compiler */
/* OBSOLETE    to define this only works for cc, not gcc.  */ */
/* OBSOLETE #undef mc68030 */
/* OBSOLETE #define mc68030 */
/* OBSOLETE #include "m68k/xm-news.h" */
