/* Definitions of target machine for GNU compiler, for HPs using the
   64bit runtime model.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

/* The default sizes for basic datatypes provided by GCC are not
   correct for the PA64 runtime architecture.

   In PA64, basic types have the following sizes

     char	1 byte
     short	2 bytes
     int	4 bytes
     long	8 bytes
     long long	8 bytes
     pointer	8 bytes
     float	4 bytes
     double	8 bytes
     long double 16 bytes
     size_t	8 bytes
     ptrdiff_t	8 bytes
     wchar	4 bytes
     
  Make GCC agree with types.h.  */
#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* If it is not listed here, then the default selected by GCC is OK.  */
#undef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE 16
#undef INT_TYPE_SIZE
#define INT_TYPE_SIZE 32
#undef MAX_LONG_TYPE_SIZE
#define MAX_LONG_TYPE_SIZE 64
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 64
#undef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE 64
#undef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE 32
#undef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE 64
/* This should be 128, but until we work out the ABI for the 128bit
   FP codes supplied by HP we'll keep it at 64 bits.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#define MAX_WCHAR_TYPE_SIZE 32

/* Temporary until we figure out what to do with those *(&@$ 32bit
   relocs which appear in stabs.  */
#undef DBX_DEBUGGING_INFO

/* We want the compiler to select a suitable secondary memory location.
   ?!? This may not work reliably.  Keep an eye out for problems.  */
#undef SECONDARY_MEMORY_NEEDED_RTX


/* ?!? This needs to be made compile-time selectable.

   The PA64 runtime model has arguments that grow to higher addresses
   (like most other targets).  The older runtime model has arguments
   that grow to lower addresses.  What fun.  */
#undef ARGS_GROW_DOWNWARD
#undef ARG_POINTER_REGNUM
#define ARG_POINTER_REGNUM 29
#undef STATIC_CHAIN_REGNUM
#define STATIC_CHAIN_REGNUM 31

/* Nonzero if we do not know how to pass TYPE solely in registers.  */
#define MUST_PASS_IN_STACK(MODE,TYPE) \
  ((TYPE) != 0							\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST		\
       || TREE_ADDRESSABLE (TYPE)))
