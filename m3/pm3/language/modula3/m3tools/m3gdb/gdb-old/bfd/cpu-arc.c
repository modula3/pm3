/* BFD support for the ARC processor
   Copyright 1994, 1995, 1997 Free Software Foundation, Inc.
   Contributed by Doug Evans (dje@cygnus.com).

This file is part of BFD, the Binary File Descriptor library.

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

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

#define ARC(mach, print_name, default_p, next) \
{					\
    32,	/* 32 bits in a word */		\
    32,	/* 32 bits in an address */	\
    8,	/* 8 bits in a byte */		\
    bfd_arch_arc,			\
    mach,				\
    "arc",				\
    print_name,				\
    4, /* section alignment power */	\
    default_p,				\
    bfd_default_compatible, 		\
    bfd_default_scan,			\
    next,				\
  }

#if 0 /* ??? Not currently needed, but keep in for future reference.  */
static const bfd_arch_info_type arch_info_struct[] =
{
  ARC (bfd_mach_arc_foo, "arc-foo", false, &arch_info_struct[1]),
  ARC (bfd_mach_arc_bar, "arc-bar", false, 0),
};
#endif

const bfd_arch_info_type bfd_arc_arch =
  ARC (bfd_mach_arc_base, "arc-base", true, 0 /*&arch_info_struct[0]*/);

/* Utility routines.  */

/* Given cpu type NAME, return its bfd_mach_arc_xxx value.
   Returns -1 if not found.  */

int
arc_get_mach (name)
     char *name;
{
  const bfd_arch_info_type *p;

  for (p = &bfd_arc_arch; p != NULL; p = p->next)
    {
      /* +4: skip over "arc-" */
      if (strcmp (name, p->printable_name + 4) == 0)
	return p->mach;
    }
  return -1;
}
