/* BFD support for the s390 processor.
   Copyright 2000, 2001 Free Software Foundation, Inc.
   Contributed by Carl B. Pedersen and Martin Schwidefsky.

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
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

const bfd_arch_info_type bfd_s390_esame_arch =
{
    64,        /* bits in a word */
    64,        /* bits in an address */
    8, /* bits in a byte */
    bfd_arch_s390,
    bfd_mach_s390_esame,
    "s390",
    "s390:esame",
    3, /* section alignment power */
    true, /* the default */
    bfd_default_compatible,
    bfd_default_scan,
    NULL
};

const bfd_arch_info_type bfd_s390_arch =
{
    32,	/* bits in a word */
    32,	/* bits in an address */
    8,	/* bits in a byte */
    bfd_arch_s390,
    bfd_mach_s390_esa,
    "s390",
    "s390:390",
    3, /* section alignment power */
    true, /* the default */
    bfd_default_compatible,
    bfd_default_scan,
    &bfd_s390_esame_arch
};
