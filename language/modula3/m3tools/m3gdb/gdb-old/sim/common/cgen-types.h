/* Types for Cpu tools GENerated simulators.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

This file is part of GDB, the GNU debugger.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef CGEN_TYPES_H
#define CGEN_TYPES_H

#ifdef __GNUC__
#define HAVE_LONGLONG
#undef DI_FN_SUPPORT
#define SIM_INLINE extern inline
#else
#undef HAVE_LONGLONG
#define DI_FN_SUPPORT
#define SIM_INLINE
#endif

extern const char *mode_names[];
#define MODE_NAME(m) (mode_names[m])

#ifdef __STDC__
typedef /*FIXME*/ signed char BI;
typedef /*FIXME*/ signed char QI;
#else
typedef /*FIXME*/ char BI;
typedef /*FIXME*/ char QI;
#endif
typedef short HI;
typedef int SI;
typedef unsigned char UBI;
typedef unsigned char UQI;
typedef unsigned short UHI;
typedef unsigned int USI;

#ifdef HAVE_LONGLONG
typedef long long DI;
typedef unsigned long long UDI;
#define GETLODI(di) ((SI) (di))
#define GETHIDI(di) ((SI) ((di) >> 32))
#define SETLODI(di, val) ((di) = (((di) & 0xffffffff00000000LL) | (val)))
#define SETHIDI(di, val) ((di) = (((di) & 0xffffffffLL) | (((DI) (val)) << 32)))
#define SETDI(di, hi, lo) ((di) = MAKEDI (hi, lo))
#define MAKEDI(hi, lo) ((((DI) (hi)) << 32) | ((DI) (lo)))
#else
/* DI mode support if "long long" doesn't exist.
   At one point CGEN supported K&R C compilers, and ANSI C compilers without
   "long long".  One can argue the various merits of keeping this in or
   throwing it out.  I went to the trouble of adding it so for the time being
   I'm leaving it in.  */
typedef struct { SI hi,lo; } DI;
typedef DI UDI;
#define GETLODI(di) ((di).lo)
#define GETHIDI(di) ((di).hi)
#define SETLODI(di, val) ((di).lo = (val))
#define SETHIDI(di, val) ((di).hi = (val))
#define SETDI(di, hi, lo) ((di) = MAKEDI (hi, lo))
extern DI make_struct_di (SI, SI);
#define MAKEDI(hi, lo) (make_struct_di ((hi), (lo)))
#endif

/* FIXME: Need to provide libraries if these aren't appropriate for target,
   or user's needs.  */
typedef float SF;
typedef double DF;
typedef double XF; /* FIXME: configure, provide library */
typedef double TF; /* FIXME: configure, provide library */

/* This is used to record extracted raw data from an instruction, among other
   things.  It must be a host data type, and not a target one so USI is
   inappropriate.  */
typedef unsigned int UINT;

typedef unsigned long PCADDR;
typedef unsigned long ADDR;
typedef /*FIXME*/ unsigned long insn_t;

struct cgen_insn;

#endif /* CGEN_TYPES_H */
