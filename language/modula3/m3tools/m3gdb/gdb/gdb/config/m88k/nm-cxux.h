/* OBSOLETE /* Native definitions for Motorola 88K running Harris CX/UX */
/* OBSOLETE    Copyright 1993, 1994 Free Software Foundation, Inc. */
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
/* OBSOLETE /* Override the standard fetch/store definitions.  */ */
/* OBSOLETE  */
/* OBSOLETE #define FETCH_INFERIOR_REGISTERS */
/* OBSOLETE  */
/* OBSOLETE #define REGISTER_U_ADDR(addr, blockend, regno) \ */
/* OBSOLETE         (addr) = m88k_register_u_addr ((blockend),(regno)); */
/* OBSOLETE  */
/* OBSOLETE #define ATTACH_DETACH */
/* OBSOLETE  */
/* OBSOLETE #define PTRACE_ATTACH 128 */
/* OBSOLETE #define PTRACE_DETACH 129 */
