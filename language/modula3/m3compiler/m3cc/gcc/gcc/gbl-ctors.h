/* Definitions relating to the special __do_global_init function used
   for getting g++ file-scope static objects constructed.  This file
   will get included either by libgcc2.c (for systems that don't support
   a .init section) or by crtstuff.c (for those that do).
   Copyright (C) 1991, 1995, 1996, 1998 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@segfault.us.com)

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

/*	This file contains definitions and declarations of things
	relating to the normal start-up-time invocation of C++
	file-scope static object constructors.  These declarations
	and definitions are used by *both* libgcc2.c and by crtstuff.c.

	Note that this file should only be compiled with GCC.
*/

#ifdef NEED_ATEXIT
#ifndef HAVE_ATEXIT
#define HAVE_ATEXIT	1	/* Take it from libgcc2.c */
#endif
#endif

#ifdef HAVE_ATEXIT
#if defined (WINNT) || defined (NEED_ATEXIT)
extern int atexit (void (*) (void));
#endif
#define ON_EXIT(FUNC,ARG) atexit ((FUNC))
#else
#ifdef sun
extern int on_exit (void *, void *);	/* The man page says it returns int. */
#define ON_EXIT(FUNC,ARG) on_exit ((FUNC), (ARG))
#endif
#endif

/*  Declare a pointer to void function type.  */

typedef void (*func_ptr) (void);

/* Declare the set of symbols use as begin and end markers for the lists
   of global object constructors and global object destructors.  */

extern func_ptr __CTOR_LIST__[];
extern func_ptr __DTOR_LIST__[];

/* Declare the routine which need to get invoked at program exit time.  */

extern void __do_global_dtors (void);

/* Define a macro with the code which needs to be executed at program
   start-up time.  This macro is used in two places in crtstuff.c (for
   systems which support a .init section) and in one place in libgcc2.c
   (for those system which do *not* support a .init section).  For all
   three places where this code might appear, it must be identical, so
   we define it once here as a macro to avoid various instances getting
   out-of-sync with one another.  */

/* Some systems place the number of pointers
   in the first word of the table.
   On other systems, that word is -1.
   In all cases, the table is null-terminated.
   If the length is not recorded, count up to the null.  */

/* Some systems use a different strategy for finding the ctors.
   For example, svr3.  */
#ifndef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY						\
do {									\
  unsigned long nptrs = (unsigned long) __CTOR_LIST__[0];		\
  unsigned i;								\
  if (nptrs == (unsigned long)-1)				        \
    for (nptrs = 0; __CTOR_LIST__[nptrs + 1] != 0; nptrs++);		\
  for (i = nptrs; i >= 1; i--)						\
    __CTOR_LIST__[i] ();						\
} while (0) 
#endif

