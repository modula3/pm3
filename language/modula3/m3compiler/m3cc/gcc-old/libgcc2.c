/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.

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

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "machmode.h"
#include "defaults.h" 
#ifndef L_trampoline
#include <stddef.h>
#endif

/* Don't use `fancy_abort' here even if config.h says to use it.  */
#ifdef abort
#undef abort
#endif

#if (SUPPORTS_WEAK == 1) && defined (ASM_OUTPUT_DEF)
#define WEAK_ALIAS
#endif

/* CYGNUS LOCAL mpw */
/* MPW's stdio.h defines size_t, potentially conflicting with stddef.h,
   if we don't suppress it by defining __size_t__.  A fixed include
   might help here, but fixincludes doesn't yet work for MPW.  */
#ifdef MPW
#define __size_t__
#endif
/* END CYGNUS LOCAL */
/* Permit the tm.h file to select the endianness to use just for this
   file.  This is used when the endianness is determined when the
   compiler is run.  */

#ifndef LIBGCC2_WORDS_BIG_ENDIAN
#define LIBGCC2_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN
#endif

/* In the first part of this file, we are interfacing to calls generated
   by the compiler itself.  These calls pass values into these routines
   which have very specific modes (rather than very specific types), and
   these compiler-generated calls also expect any return values to have
   very specific modes (rather than very specific types).  Thus, we need
   to avoid using regular C language type names in this part of the file
   because the sizes for those types can be configured to be anything.
   Instead we use the following special type names.  */

typedef unsigned int UQItype	__attribute__ ((mode (QI)));
typedef 	 int SItype	__attribute__ ((mode (SI)));
typedef unsigned int USItype	__attribute__ ((mode (SI)));
typedef		 int DItype	__attribute__ ((mode (DI)));
typedef unsigned int UDItype	__attribute__ ((mode (DI)));

typedef 	float SFtype	__attribute__ ((mode (SF)));
typedef		float DFtype	__attribute__ ((mode (DF)));

#if LONG_DOUBLE_TYPE_SIZE == 96
typedef		float XFtype	__attribute__ ((mode (XF)));
#endif
#if LONG_DOUBLE_TYPE_SIZE == 128
typedef		float TFtype	__attribute__ ((mode (TF)));
#endif

typedef int word_type __attribute__ ((mode (__word__)));

/* Make sure that we don't accidentally use any normal C language built-in
   type names in the first part of this file.  Instead we want to use *only*
   the type names defined above.  The following macro definitions insure
   that if we *do* accidentally use some normal C language built-in type name,
   we will get a syntax error.  */

#define char bogus_type
#define short bogus_type
#define int bogus_type
#define long bogus_type
#define unsigned bogus_type
#define float bogus_type
#define double bogus_type

#define SI_TYPE_SIZE (sizeof (SItype) * BITS_PER_UNIT)

/* DIstructs are pairs of SItype values in the order determined by
   LIBGCC2_WORDS_BIG_ENDIAN.  */

#if LIBGCC2_WORDS_BIG_ENDIAN
  struct DIstruct {SItype high, low;};
#else
  struct DIstruct {SItype low, high;};
#endif

/* We need this union to unpack/pack DImode values, since we don't have
   any arithmetic yet.  Incoming DImode parameters are stored into the
   `ll' field, and the unpacked result is read from the struct `s'.  */

typedef union
{
  struct DIstruct s;
  DItype ll;
} DIunion;

#if (defined (L_udivmoddi4) || defined (L_muldi3) || defined (L_udiv_w_sdiv)\
     || defined (L_divdi3) || defined (L_udivdi3) \
     || defined (L_moddi3) || defined (L_umoddi3))

#include "longlong.h"

#endif /* udiv or mul */

extern DItype __fixunssfdi (SFtype a);
extern DItype __fixunsdfdi (DFtype a);
#if LONG_DOUBLE_TYPE_SIZE == 96
extern DItype __fixunsxfdi (XFtype a);
#endif
#if LONG_DOUBLE_TYPE_SIZE == 128
extern DItype __fixunstfdi (TFtype a);
#endif

#if defined (L_negdi2) || defined (L_divdi3) || defined (L_moddi3)
#if defined (L_divdi3) || defined (L_moddi3)
static inline
#endif
DItype
__negdi2 (u)
     DItype u;
{
  DIunion w;
  DIunion uu;

  uu.ll = u;

  w.s.low = -uu.s.low;
  w.s.high = -uu.s.high - ((USItype) w.s.low > 0);

  return w.ll;
}
#endif

#ifdef L_lshrdi3
DItype
__lshrdi3 (u, b)
     DItype u;
     word_type b;
{
  DIunion w;
  word_type bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (USItype)uu.s.high >> -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.high << bm;
      w.s.high = (USItype)uu.s.high >> b;
      w.s.low = ((USItype)uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashldi3
DItype
__ashldi3 (u, b)
     DItype u;
     word_type b;
{
  DIunion w;
  word_type bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (USItype)uu.s.low << -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.low >> bm;
      w.s.low = (USItype)uu.s.low << b;
      w.s.high = ((USItype)uu.s.high << b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashrdi3
DItype
__ashrdi3 (u, b)
     DItype u;
     word_type b;
{
  DIunion w;
  word_type bm;
  DIunion uu;

  if (b == 0)
    return u;

  uu.ll = u;

  bm = (sizeof (SItype) * BITS_PER_UNIT) - b;
  if (bm <= 0)
    {
      /* w.s.high = 1..1 or 0..0 */
      w.s.high = uu.s.high >> (sizeof (SItype) * BITS_PER_UNIT - 1);
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      USItype carries = (USItype)uu.s.high << bm;
      w.s.high = uu.s.high >> b;
      w.s.low = ((USItype)uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ffsdi2
DItype
__ffsdi2 (u)
     DItype u;
{
  DIunion uu, w;
  uu.ll = u;
  w.s.high = 0;
  w.s.low = ffs (uu.s.low);
  if (w.s.low != 0)
    return w.ll;
  w.s.low = ffs (uu.s.high);
  if (w.s.low != 0)
    {
      w.s.low += BITS_PER_UNIT * sizeof (SItype);
      return w.ll;
    }
  return w.ll;
}
#endif

#ifdef L_muldi3
DItype
__muldi3 (u, v)
     DItype u, v;
{
  DIunion w;
  DIunion uu, vv;

  uu.ll = u,
  vv.ll = v;

  w.ll = __umulsidi3 (uu.s.low, vv.s.low);
  w.s.high += ((USItype) uu.s.low * (USItype) vv.s.high
	       + (USItype) uu.s.high * (USItype) vv.s.low);

  return w.ll;
}
#endif

#ifdef L_udiv_w_sdiv
#if defined (sdiv_qrnnd)
USItype
__udiv_w_sdiv (rp, a1, a0, d)
     USItype *rp, a1, a0, d;
{
  USItype q, r;
  USItype c0, c1, b1;

  if ((SItype) d >= 0)
    {
      if (a1 < d - a1 - (a0 >> (SI_TYPE_SIZE - 1)))
	{
	  /* dividend, divisor, and quotient are nonnegative */
	  sdiv_qrnnd (q, r, a1, a0, d);
	}
      else
	{
	  /* Compute c1*2^32 + c0 = a1*2^32 + a0 - 2^31*d */
	  sub_ddmmss (c1, c0, a1, a0, d >> 1, d << (SI_TYPE_SIZE - 1));
	  /* Divide (c1*2^32 + c0) by d */
	  sdiv_qrnnd (q, r, c1, c0, d);
	  /* Add 2^31 to quotient */
	  q += (USItype) 1 << (SI_TYPE_SIZE - 1);
	}
    }
  else
    {
      b1 = d >> 1;			/* d/2, between 2^30 and 2^31 - 1 */
      c1 = a1 >> 1;			/* A/2 */
      c0 = (a1 << (SI_TYPE_SIZE - 1)) + (a0 >> 1);

      if (a1 < b1)			/* A < 2^32*b1, so A/2 < 2^31*b1 */
	{
	  sdiv_qrnnd (q, r, c1, c0, b1); /* (A/2) / (d/2) */

	  r = 2*r + (a0 & 1);		/* Remainder from A/(2*b1) */
	  if ((d & 1) != 0)
	    {
	      if (r >= q)
		r = r - q;
	      else if (q - r <= d)
		{
		  r = r - q + d;
		  q--;
		}
	      else
		{
		  r = r - q + 2*d;
		  q -= 2;
		}
	    }
	}
      else if (c1 < b1)			/* So 2^31 <= (A/2)/b1 < 2^32 */
	{
	  c1 = (b1 - 1) - c1;
	  c0 = ~c0;			/* logical NOT */

	  sdiv_qrnnd (q, r, c1, c0, b1); /* (A/2) / (d/2) */

	  q = ~q;			/* (A/2)/b1 */
	  r = (b1 - 1) - r;

	  r = 2*r + (a0 & 1);		/* A/(2*b1) */

	  if ((d & 1) != 0)
	    {
	      if (r >= q)
		r = r - q;
	      else if (q - r <= d)
		{
		  r = r - q + d;
		  q--;
		}
	      else
		{
		  r = r - q + 2*d;
		  q -= 2;
		}
	    }
	}
      else				/* Implies c1 = b1 */
	{				/* Hence a1 = d - 1 = 2*b1 - 1 */
	  if (a0 >= -d)
	    {
	      q = -1;
	      r = a0 + d;
	    }
	  else
	    {
	      q = -2;
	      r = a0 + 2*d;
	    }
	}
    }

  *rp = r;
  return q;
}
#else
/* If sdiv_qrnnd doesn't exist, define dummy __udiv_w_sdiv.  */
USItype
__udiv_w_sdiv (rp, a1, a0, d)
     USItype *rp, a1, a0, d;
{}
#endif
#endif

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
#define L_udivmoddi4
#endif

#ifdef L_udivmoddi4
static const UQItype __clz_tab[] =
{
  0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
};

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
static inline
#endif
UDItype
__udivmoddi4 (n, d, rp)
     UDItype n, d;
     UDItype *rp;
{
  DIunion ww;
  DIunion nn, dd;
  DIunion rr;
  USItype d0, d1, n0, n1, n2;
  USItype q0, q1;
  USItype b, bm;

  nn.ll = n;
  dd.ll = d;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;

#if !UDIV_NEEDS_NORMALIZATION
  if (d1 == 0)
    {
      if (d0 > n1)
	{
	  /* 0q = nn / 0D */

	  udiv_qrnnd (q0, n0, n1, n0, d0);
	  q1 = 0;

	  /* Remainder in n0.  */
	}
      else
	{
	  /* qq = NN / 0d */

	  if (d0 == 0)
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */

	  udiv_qrnnd (q1, n1, 0, n1, d0);
	  udiv_qrnnd (q0, n0, n1, n0, d0);

	  /* Remainder in n0.  */
	}

      if (rp != 0)
	{
	  rr.s.low = n0;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }

#else /* UDIV_NEEDS_NORMALIZATION */

  if (d1 == 0)
    {
      if (d0 > n1)
	{
	  /* 0q = nn / 0D */

	  count_leading_zeros (bm, d0);

	  if (bm != 0)
	    {
	      /* Normalize, i.e. make the most significant bit of the
		 denominator set.  */

	      d0 = d0 << bm;
	      n1 = (n1 << bm) | (n0 >> (SI_TYPE_SIZE - bm));
	      n0 = n0 << bm;
	    }

	  udiv_qrnnd (q0, n0, n1, n0, d0);
	  q1 = 0;

	  /* Remainder in n0 >> bm.  */
	}
      else
	{
	  /* qq = NN / 0d */

	  if (d0 == 0)
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */

	  count_leading_zeros (bm, d0);

	  if (bm == 0)
	    {
	      /* From (n1 >= d0) /\ (the most significant bit of d0 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 leading quotient digit q1 = 1).

		 This special case is necessary, not an optimization.
		 (Shifts counts of SI_TYPE_SIZE are undefined.)  */

	      n1 -= d0;
	      q1 = 1;
	    }
	  else
	    {
	      /* Normalize.  */

	      b = SI_TYPE_SIZE - bm;

	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q1, n1, n2, n1, d0);
	    }

	  /* n1 != d0...  */

	  udiv_qrnnd (q0, n0, n1, n0, d0);

	  /* Remainder in n0 >> bm.  */
	}

      if (rp != 0)
	{
	  rr.s.low = n0 >> bm;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }
#endif /* UDIV_NEEDS_NORMALIZATION */

  else
    {
      if (d1 > n1)
	{
	  /* 00 = nn / DD */

	  q0 = 0;
	  q1 = 0;

	  /* Remainder in n1n0.  */
	  if (rp != 0)
	    {
	      rr.s.low = n0;
	      rr.s.high = n1;
	      *rp = rr.ll;
	    }
	}
      else
	{
	  /* 0q = NN / dd */

	  count_leading_zeros (bm, d1);
	  if (bm == 0)
	    {
	      /* From (n1 >= d1) /\ (the most significant bit of d1 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 quotient digit q0 = 0 or 1).

		 This special case is necessary, not an optimization.  */

	      /* The condition on the next line takes advantage of that
		 n1 >= d1 (true due to program flow).  */
	      if (n1 > d1 || n0 >= d0)
		{
		  q0 = 1;
		  sub_ddmmss (n1, n0, n1, n0, d1, d0);
		}
	      else
		q0 = 0;

	      q1 = 0;

	      if (rp != 0)
		{
		  rr.s.low = n0;
		  rr.s.high = n1;
		  *rp = rr.ll;
		}
	    }
	  else
	    {
	      USItype m1, m0;
	      /* Normalize.  */

	      b = SI_TYPE_SIZE - bm;

	      d1 = (d1 << bm) | (d0 >> b);
	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q0, n1, n2, n1, d1);
	      umul_ppmm (m1, m0, q0, d0);

	      if (m1 > n1 || (m1 == n1 && m0 > n0))
		{
		  q0--;
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
		}

	      q1 = 0;

	      /* Remainder in (n1n0 - m1m0) >> bm.  */
	      if (rp != 0)
		{
		  sub_ddmmss (n1, n0, n1, n0, m1, m0);
		  rr.s.low = (n1 << b) | (n0 >> bm);
		  rr.s.high = n1 >> bm;
		  *rp = rr.ll;
		}
	    }
	}
    }

  ww.s.low = q0;
  ww.s.high = q1;
  return ww.ll;
}
#endif

#ifdef L_divdi3
UDItype __udivmoddi4 ();

DItype
__divdi3 (u, v)
     DItype u, v;
{
  word_type c = 0;
  DIunion uu, vv;
  DItype w;

  uu.ll = u;
  vv.ll = v;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = __negdi2 (uu.ll);
  if (vv.s.high < 0)
    c = ~c,
    vv.ll = __negdi2 (vv.ll);

  w = __udivmoddi4 (uu.ll, vv.ll, (UDItype *) 0);
  if (c)
    w = __negdi2 (w);

  return w;
}
#endif

#ifdef L_moddi3
UDItype __udivmoddi4 ();
DItype
__moddi3 (u, v)
     DItype u, v;
{
  word_type c = 0;
  DIunion uu, vv;
  DItype w;

  uu.ll = u;
  vv.ll = v;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = __negdi2 (uu.ll);
  if (vv.s.high < 0)
    vv.ll = __negdi2 (vv.ll);

  (void) __udivmoddi4 (uu.ll, vv.ll, &w);
  if (c)
    w = __negdi2 (w);

  return w;
}
#endif

#ifdef L_umoddi3
UDItype __udivmoddi4 ();
UDItype
__umoddi3 (u, v)
     UDItype u, v;
{
  UDItype w;

  (void) __udivmoddi4 (u, v, &w);

  return w;
}
#endif

#ifdef L_udivdi3
UDItype __udivmoddi4 ();
UDItype
__udivdi3 (n, d)
     UDItype n, d;
{
  return __udivmoddi4 (n, d, (UDItype *) 0);
}
#endif

#ifdef L_cmpdi2
word_type
__cmpdi2 (a, b)
     DItype a, b;
{
  DIunion au, bu;

  au.ll = a, bu.ll = b;

  if (au.s.high < bu.s.high)
    return 0;
  else if (au.s.high > bu.s.high)
    return 2;
  if ((USItype) au.s.low < (USItype) bu.s.low)
    return 0;
  else if ((USItype) au.s.low > (USItype) bu.s.low)
    return 2;
  return 1;
}
#endif

#ifdef L_ucmpdi2
word_type
__ucmpdi2 (a, b)
     DItype a, b;
{
  DIunion au, bu;

  au.ll = a, bu.ll = b;

  if ((USItype) au.s.high < (USItype) bu.s.high)
    return 0;
  else if ((USItype) au.s.high > (USItype) bu.s.high)
    return 2;
  if ((USItype) au.s.low < (USItype) bu.s.low)
    return 0;
  else if ((USItype) au.s.low > (USItype) bu.s.low)
    return 2;
  return 1;
}
#endif

#if defined(L_fixunstfdi) && (LONG_DOUBLE_TYPE_SIZE == 128)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunstfdi (a)
     TFtype a;
{
  TFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the TFtype, leaving the low part as flonum.  */
  a -= (TFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#if defined(L_fixtfdi) && (LONG_DOUBLE_TYPE_SIZE == 128)
DItype
__fixtfdi (a)
     TFtype a;
{
  if (a < 0)
    return - __fixunstfdi (-a);
  return __fixunstfdi (a);
}
#endif

#if defined(L_fixunsxfdi) && (LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunsxfdi (a)
     XFtype a;
{
  XFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the XFtype, leaving the low part as flonum.  */
  a -= (XFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#if defined(L_fixxfdi) && (LONG_DOUBLE_TYPE_SIZE == 96)
DItype
__fixxfdi (a)
     XFtype a;
{
  if (a < 0)
    return - __fixunsxfdi (-a);
  return __fixunsxfdi (a);
}
#endif

#ifdef L_fixunsdfdi
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunsdfdi (a)
     DFtype a;
{
  DFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the DFtype, leaving the low part as flonum.  */
  a -= (DFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#ifdef L_fixdfdi
DItype
__fixdfdi (a)
     DFtype a;
{
  if (a < 0)
    return - __fixunsdfdi (-a);
  return __fixunsdfdi (a);
}
#endif

#ifdef L_fixunssfdi
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DItype
__fixunssfdi (SFtype original_a)
{
  /* Convert the SFtype to a DFtype, because that is surely not going
     to lose any bits.  Some day someone else can write a faster version
     that avoids converting to DFtype, and verify it really works right.  */
  DFtype a = original_a;
  DFtype b;
  UDItype v;

  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  b = (a / HIGH_WORD_COEFF);
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  v = (USItype) b;
  v <<= WORD_SIZE;
  /* Remove high part from the DFtype, leaving the low part as flonum.  */
  a -= (DFtype)v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (- a);
  else
    v += (USItype) a;
  return v;
}
#endif

#ifdef L_fixsfdi
DItype
__fixsfdi (SFtype a)
{
  if (a < 0)
    return - __fixunssfdi (-a);
  return __fixunssfdi (a);
}
#endif

#if defined(L_floatdixf) && (LONG_DOUBLE_TYPE_SIZE == 96)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

XFtype
__floatdixf (u)
     DItype u;
{
  XFtype d;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  d = (USItype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (negate ? -d : d);
}
#endif

#if defined(L_floatditf) && (LONG_DOUBLE_TYPE_SIZE == 128)
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

TFtype
__floatditf (u)
     DItype u;
{
  TFtype d;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  d = (USItype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (negate ? -d : d);
}
#endif

#ifdef L_floatdidf
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)

DFtype
__floatdidf (u)
     DItype u;
{
  DFtype d;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  d = (USItype) (u >> WORD_SIZE);
  d *= HIGH_HALFWORD_COEFF;
  d *= HIGH_HALFWORD_COEFF;
  d += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (negate ? -d : d);
}
#endif

#ifdef L_floatdisf
#define WORD_SIZE (sizeof (SItype) * BITS_PER_UNIT)
#define HIGH_HALFWORD_COEFF (((UDItype) 1) << (WORD_SIZE / 2))
#define HIGH_WORD_COEFF (((UDItype) 1) << WORD_SIZE)
#define DI_SIZE (sizeof (DItype) * BITS_PER_UNIT)

/* Define codes for all the float formats that we know of.  Note
   that this is copied from real.h.  */
   
#define UNKNOWN_FLOAT_FORMAT 0
#define IEEE_FLOAT_FORMAT 1
#define VAX_FLOAT_FORMAT 2
#define IBM_FLOAT_FORMAT 3

/* Default to IEEE float if not specified.  Nearly all machines use it.  */
#ifndef HOST_FLOAT_FORMAT
#define	HOST_FLOAT_FORMAT	IEEE_FLOAT_FORMAT
#endif

#if HOST_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
#define DF_SIZE 53
#define SF_SIZE 24
#endif

#if HOST_FLOAT_FORMAT == IBM_FLOAT_FORMAT
#define DF_SIZE 56
#define SF_SIZE 24
#endif

#if HOST_FLOAT_FORMAT == VAX_FLOAT_FORMAT
#define DF_SIZE 56
#define SF_SIZE 24
#endif

SFtype
__floatdisf (u)
     DItype u;
{
  /* Do the calculation in DFmode
     so that we don't lose any of the precision of the high word
     while multiplying it.  */
  DFtype f;
  SItype negate = 0;

  if (u < 0)
    u = -u, negate = 1;

  /* Protect against double-rounding error.
     Represent any low-order bits, that might be truncated in DFmode,
     by a bit that won't be lost.  The bit can go in anywhere below the
     rounding position of the SFmode.  A fixed mask and bit position
     handles all usual configurations.  It doesn't handle the case
     of 128-bit DImode, however.  */
  if (DF_SIZE < DI_SIZE
      && DF_SIZE > (DI_SIZE - DF_SIZE + SF_SIZE))
    {
#define REP_BIT ((USItype) 1 << (DI_SIZE - DF_SIZE))
      if (u >= ((UDItype) 1 << DF_SIZE))
	{
	  if ((USItype) u & (REP_BIT - 1))
	    u |= REP_BIT;
	}
    }
  f = (USItype) (u >> WORD_SIZE);
  f *= HIGH_HALFWORD_COEFF;
  f *= HIGH_HALFWORD_COEFF;
  f += (USItype) (u & (HIGH_WORD_COEFF - 1));

  return (SFtype) (negate ? -f : f);
}
#endif

#if defined(L_fixunsxfsi) && LONG_DOUBLE_TYPE_SIZE == 96
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>

USItype
__fixunsxfsi (a)
     XFtype a;
{
  if (a >= - (DFtype) LONG_MIN)
    return (SItype) (a + LONG_MIN) - LONG_MIN;
  return (SItype) a;
}
#endif

#ifdef L_fixunsdfsi
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>

USItype
__fixunsdfsi (a)
     DFtype a;
{
  if (a >= - (DFtype) LONG_MIN)
    return (SItype) (a + LONG_MIN) - LONG_MIN;
  return (SItype) a;
}
#endif

#ifdef L_fixunssfsi
/* Reenable the normal types, in case limits.h needs them.  */
#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double
#undef MIN
#undef MAX
#include <limits.h>

USItype
__fixunssfsi (SFtype a)
{
  if (a >= - (SFtype) LONG_MIN)
    return (SItype) (a + LONG_MIN) - LONG_MIN;
  return (SItype) a;
}
#endif

/* From here on down, the routines use normal data types.  */

#define SItype bogus_type
#define USItype bogus_type
#define DItype bogus_type
#define UDItype bogus_type
#define SFtype bogus_type
#define DFtype bogus_type

#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double

#ifdef L__gcc_bcmp

/* Like bcmp except the sign is meaningful.
   Result is negative if S1 is less than S2,
   positive if S1 is greater, 0 if S1 and S2 are equal.  */

int
__gcc_bcmp (s1, s2, size)
     unsigned char *s1, *s2;
     size_t size;
{
  while (size > 0)
    {
      unsigned char c1 = *s1++, c2 = *s2++;
      if (c1 != c2)
	return c1 - c2;
      size--;
    }
  return 0;
}

#endif

#ifdef L__dummy
void
__dummy () {}
#endif

#ifdef L_varargs
#ifdef __i860__
#if defined(__svr4__) || defined(__alliant__)
	asm ("	.text");
	asm ("	.align	4");

/* The Alliant needs the added underscore.  */
	asm (".globl	__builtin_saveregs");
asm ("__builtin_saveregs:");
	asm (".globl	___builtin_saveregs");
asm ("___builtin_saveregs:");

        asm ("	andnot	0x0f,%sp,%sp");	/* round down to 16-byte boundary */
	asm ("	adds	-96,%sp,%sp");  /* allocate stack space for reg save
					   area and also for a new va_list
					   structure */
	/* Save all argument registers in the arg reg save area.  The
	   arg reg save area must have the following layout (according
	   to the svr4 ABI):

		struct {
		  union  {
		    float freg[8];
		    double dreg[4];
		  } float_regs;
		  long	ireg[12];
		};
	*/

	asm ("	fst.q	%f8,  0(%sp)"); /* save floating regs (f8-f15)  */
	asm ("	fst.q	%f12,16(%sp)"); 

	asm ("	st.l	%r16,32(%sp)"); /* save integer regs (r16-r27) */
	asm ("	st.l	%r17,36(%sp)"); 
	asm ("	st.l	%r18,40(%sp)");
	asm ("	st.l	%r19,44(%sp)");
	asm ("	st.l	%r20,48(%sp)");
	asm ("	st.l	%r21,52(%sp)");
	asm ("	st.l	%r22,56(%sp)");
	asm ("	st.l	%r23,60(%sp)");
	asm ("	st.l	%r24,64(%sp)");
	asm ("	st.l	%r25,68(%sp)");
	asm ("	st.l	%r26,72(%sp)");
	asm ("	st.l	%r27,76(%sp)");

	asm ("	adds	80,%sp,%r16");  /* compute the address of the new
					   va_list structure.  Put in into
					   r16 so that it will be returned
					   to the caller.  */

	/* Initialize all fields of the new va_list structure.  This
	   structure looks like:

		typedef struct {
		    unsigned long	ireg_used;
		    unsigned long	freg_used;
		    long		*reg_base;
		    long		*mem_ptr;
		} va_list;
	*/

	asm ("	st.l	%r0, 0(%r16)"); /* nfixed */
	asm ("	st.l	%r0, 4(%r16)"); /* nfloating */
	asm ("  st.l    %sp, 8(%r16)"); /* __va_ctl points to __va_struct.  */
	asm ("	bri	%r1");		/* delayed return */
	asm ("	st.l	%r28,12(%r16)"); /* pointer to overflow args */

#else /* not __svr4__ */
#if defined(__PARAGON__)
	/*
	 *	we'll use SVR4-ish varargs but need SVR3.2 assembler syntax,
	 *	and we stand a better chance of hooking into libraries
	 *	compiled by PGI.  [andyp@ssd.intel.com]
	 */
	asm ("	.text");
	asm ("	.align	4");
	asm (".globl	__builtin_saveregs");
asm ("__builtin_saveregs:");
	asm (".globl	___builtin_saveregs");
asm ("___builtin_saveregs:");

        asm ("	andnot	0x0f,sp,sp");	/* round down to 16-byte boundary */
	asm ("	adds	-96,sp,sp");	/* allocate stack space for reg save
					   area and also for a new va_list
					   structure */
	/* Save all argument registers in the arg reg save area.  The
	   arg reg save area must have the following layout (according
	   to the svr4 ABI):

		struct {
		  union  {
		    float freg[8];
		    double dreg[4];
		  } float_regs;
		  long	ireg[12];
		};
	*/

	asm ("	fst.q	f8,  0(sp)");
	asm ("	fst.q	f12,16(sp)"); 
	asm ("	st.l	r16,32(sp)");
	asm ("	st.l	r17,36(sp)"); 
	asm ("	st.l	r18,40(sp)");
	asm ("	st.l	r19,44(sp)");
	asm ("	st.l	r20,48(sp)");
	asm ("	st.l	r21,52(sp)");
	asm ("	st.l	r22,56(sp)");
	asm ("	st.l	r23,60(sp)");
	asm ("	st.l	r24,64(sp)");
	asm ("	st.l	r25,68(sp)");
	asm ("	st.l	r26,72(sp)");
	asm ("	st.l	r27,76(sp)");

	asm ("	adds	80,sp,r16");  /* compute the address of the new
					   va_list structure.  Put in into
					   r16 so that it will be returned
					   to the caller.  */

	/* Initialize all fields of the new va_list structure.  This
	   structure looks like:

		typedef struct {
		    unsigned long	ireg_used;
		    unsigned long	freg_used;
		    long		*reg_base;
		    long		*mem_ptr;
		} va_list;
	*/

	asm ("	st.l	r0, 0(r16)"); /* nfixed */
	asm ("	st.l	r0, 4(r16)"); /* nfloating */
	asm ("  st.l    sp, 8(r16)"); /* __va_ctl points to __va_struct.  */
	asm ("	bri	r1");		/* delayed return */
	asm ("	 st.l	r28,12(r16)"); /* pointer to overflow args */
#else /* not __PARAGON__ */
	asm ("	.text");
	asm ("	.align	4");

	asm (".globl	___builtin_saveregs");
	asm ("___builtin_saveregs:");
	asm ("	mov	sp,r30");
	asm ("	andnot	0x0f,sp,sp");
	asm ("	adds	-96,sp,sp");  /* allocate sufficient space on the stack */

/* Fill in the __va_struct.  */
	asm ("	st.l	r16, 0(sp)"); /* save integer regs (r16-r27) */
	asm ("	st.l	r17, 4(sp)"); /* int	fixed[12] */
	asm ("	st.l	r18, 8(sp)");
	asm ("	st.l	r19,12(sp)");
	asm ("	st.l	r20,16(sp)");
	asm ("	st.l	r21,20(sp)");
	asm ("	st.l	r22,24(sp)");
	asm ("	st.l	r23,28(sp)");
	asm ("	st.l	r24,32(sp)");
	asm ("	st.l	r25,36(sp)");
	asm ("	st.l	r26,40(sp)");
	asm ("	st.l	r27,44(sp)");

	asm ("	fst.q	f8, 48(sp)"); /* save floating regs (f8-f15) */
	asm ("	fst.q	f12,64(sp)"); /* int floating[8] */

/* Fill in the __va_ctl.  */
	asm ("  st.l    sp, 80(sp)"); /* __va_ctl points to __va_struct.  */
	asm ("	st.l	r28,84(sp)"); /* pointer to more args */
	asm ("	st.l	r0, 88(sp)"); /* nfixed */
	asm ("	st.l	r0, 92(sp)"); /* nfloating */

	asm ("	adds	80,sp,r16");  /* return address of the __va_ctl.  */
	asm ("	bri	r1");
	asm ("	mov	r30,sp");
				/* recover stack and pass address to start 
				   of data.  */
#endif /* not __PARAGON__ */
#endif /* not __svr4__ */
#else /* not __i860__ */
#ifdef __sparc__
	asm (".global __builtin_saveregs");
	asm ("__builtin_saveregs:");
	asm (".global ___builtin_saveregs");
	asm ("___builtin_saveregs:");
#ifdef NEED_PROC_COMMAND
	asm (".proc 020");
#endif
	asm ("st %i0,[%fp+68]");
	asm ("st %i1,[%fp+72]");
	asm ("st %i2,[%fp+76]");
	asm ("st %i3,[%fp+80]");
	asm ("st %i4,[%fp+84]");
	asm ("retl");
	asm ("st %i5,[%fp+88]");
#ifdef NEED_TYPE_COMMAND
	asm (".type __builtin_saveregs,#function");
	asm (".size __builtin_saveregs,.-__builtin_saveregs");
#endif
#else /* not __sparc__ */
#if defined(__MIPSEL__) | defined(__R3000__) | defined(__R2000__) | defined(__mips__)

  asm ("	.text");
  asm ("	.ent __builtin_saveregs");
  asm ("	.globl __builtin_saveregs");
  asm ("__builtin_saveregs:");
  asm ("	sw	$4,0($30)");
  asm ("	sw	$5,4($30)");
  asm ("	sw	$6,8($30)");
  asm ("	sw	$7,12($30)");
  asm ("	j	$31");
  asm ("	.end __builtin_saveregs");
#else /* not __mips__, etc.  */

void *
__builtin_saveregs ()
{
  abort ();
}

#endif /* not __mips__ */
#endif /* not __sparc__ */
#endif /* not __i860__ */
#endif

#ifdef L_eprintf
#ifndef inhibit_libc

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
/* This is used by the `assert' macro.  */
void
__eprintf (string, expression, line, filename)
     const char *string;
     const char *expression;
     int line;
     const char *filename;
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}

#endif
#endif

#ifdef L_bb

/* Structure emitted by -a  */
struct bb
{
  long zero_word;
  const char *filename;
  long *counts;
  long ncounts;
  struct bb *next;
  const unsigned long *addresses;

  /* Older GCC's did not emit these fields.  */
  long nwords;
  const char **functions;
  const long *line_nums;
  const char **filenames;
  char *flags;
};

#ifdef BLOCK_PROFILER_CODE
BLOCK_PROFILER_CODE
#else
#ifndef inhibit_libc

/* Simple minded basic block profiling output dumper for
   systems that don't provide tcov support.  At present,
   it requires atexit and stdio.  */

/* CYGNUS LOCAL mpw */
/* ifdef MPW, stdio.h should already be included in include/mpw/mpw.h and 
   __STDIO__ and NULL are defined in stdio.h; therefore no stdio.h will be 
   further included and there is need to undef NULL. */
#ifndef MPW
#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#endif
/* END CYGNUS LOCAL */

#include <stdio.h>
char *ctime ();

#include "gbl-ctors.h"

static struct bb *bb_head;

/* Return the number of digits needed to print a value */
/* __inline__ */ static int num_digits (long value, int base)
{
  int minus = (value < 0 && base != 16);
  unsigned long v = (minus) ? -value : value;
  int ret = minus;

  do
    {
      v /= base;
      ret++;
    }
  while (v);

  return ret;
}

void
__bb_exit_func (void)
{
  /* CYGNUS LOCAL: gcov */
  FILE *file;
  long time_value;
  int i;

  if (bb_head == 0)
    return;

  i = strlen (bb_head->filename) - 3;

  if (!strcmp (bb_head->filename+i, ".da"))
    {
      /* Must be -fprofile-arcs not -a.
	 Dump data in a form that gcov expects.  */

      struct bb *ptr;

      for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
	{
	  /* If the file exists, and the number of counts in it is the same,
	     then merge them in.  */
	     
	  if ((file = fopen (ptr->filename, "r")) != NULL)
	    {
	      long n_counts = 0;
	      unsigned char tmp;
	      int i;
	      int ret = 0;

	      /* Reading a byte at a time avoids any questions about the
		 the byte-endianness of the target.  */
	      for (i = sizeof (long) - 1; i >= 0; i--)
		{
		  ret += fread (&tmp, sizeof (char), 1, file);
		  n_counts |= (tmp << (8 * i));
		}
	      if (ret != 4)
		{
		  fprintf (stderr, "arc profiling: Can't read output file %s.\n",
			   ptr->filename);
		  continue;
		}

	      if (n_counts == ptr->ncounts)
		{
		  int i;

		  for (i = 0; i < n_counts; i++)
		    {
		      long v = 0;
		      unsigned char tmp;
		      int j;
		      int ret = 0;

		      for (j = sizeof (long) - 1; j >= 0; j--)
			{
			  ret += fread (&tmp, sizeof (char), 1, file);
			  v |= (tmp << (8 * j));
			}
		      if (ret != 4)
			{
			  fprintf (stderr, "arc profiling: Can't read output file %s.\n",
				   ptr->filename);
			  break;
			}
		      ptr->counts[i] += v;
		    }
		}

	      if (fclose (file) == EOF)
		fprintf (stderr, "arc profiling: Error closing output file %s.\n",
			 ptr->filename);
	    }
	  if ((file = fopen (ptr->filename, "w")) == NULL)
	    {
	      fprintf (stderr, "arc profiling: Can't open output file %s.\n",
		       ptr->filename);
	      continue;
	    }

	  /* ??? Should first write a header to the file.  Perferably, a 4 byte
	     magic number, 4 bytes containing the time the program was
	     compiled, 4 bytes containing the last modification time of the
	     source file, and 4 bytes indicating the compiler options used.

	     That way we can easily verify that the proper source/executable/
	     data file combination is being used from gcov.  */

	  /* We always write one byte at a time, so as to avoid any problems
	     with the endianness of the target.  */

	  {
	    unsigned char tmp;
	    int i;
	    for (i = sizeof (long) - 1; i >= 0; i--)
	      {
		tmp = ((ptr->ncounts >> (8 * i)) & 0xff);
		fwrite (&tmp, sizeof (char), 1, file);
	      }
	  }

	  {
	    int j;
	    long *address = ptr->counts;
	    int ret = 0;
	    for (j = ptr->ncounts; j > 0; j--)
	      {
		unsigned char tmp;
		int i;
		for (i = sizeof (long) - 1; i >= 0; i--)
		  {
		    tmp = ((*address >> (8 * i)) & 0xff);
		    ret += fwrite (&tmp, sizeof (char), 1, file);
		  }
		address++;
	      }
	    if (ret != ptr->ncounts * sizeof (long))
	      fprintf (stderr, "arc profiling: Error writing output file %s.\n",
		       ptr->filename);
	  }

	  if (fclose (file) == EOF)
	    fprintf (stderr, "arc profiling: Error closing output file %s.\n",
		     ptr->filename);
	}

      return;
    }

  /* Must be basic block profiling.  Emit a human readable output file.  */

  file = fopen ("bb.out", "a");

  if (!file)
    perror ("bb.out");

  else
    {
      struct bb *ptr;

      /* This is somewhat type incorrect, but it avoids worrying about
	 exactly where time.h is included from.  It should be ok unless
	 a void * differs from other pointer formats, or if sizeof (long)
	 is < sizeof (time_t).  It would be nice if we could assume the
	 use of rationale standards here.  */

      time ((void *) &time_value);
      fprintf (file, "Basic block profiling finished on %s\n", ctime ((void *) &time_value));

      /* We check the length field explicitly in order to allow compatibility
	 with older GCC's which did not provide it.  */

      for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
	{
	  int i;
	  /* CYGNUS LOCAL: gcov */
	  int func_p	= (ptr->nwords >= sizeof (struct bb)
			   && ptr->nwords <= 1000
			   && ptr->functions);
	  int line_p	= (func_p && ptr->line_nums);
	  int file_p	= (func_p && ptr->filenames);
	  int addr_p	= (ptr->addresses != 0);
	  long ncounts	= ptr->ncounts;
	  long cnt_max  = 0;
	  long line_max = 0;
	  long addr_max = 0;
	  int file_len	= 0;
	  int func_len	= 0;
	  int blk_len	= num_digits (ncounts, 10);
	  int cnt_len;
	  int line_len;
	  int addr_len;

	  fprintf (file, "File %s, %ld basic blocks \n\n",
		   ptr->filename, ncounts);

	  /* Get max values for each field.  */
	  for (i = 0; i < ncounts; i++)
	    {
	      const char *p;
	      int len;

	      if (cnt_max < ptr->counts[i])
		cnt_max = ptr->counts[i];

	      /* CYGNUS LOCAL: gcov */
	      if (addr_p && addr_max < ptr->addresses[i])
		addr_max = ptr->addresses[i];

	      if (line_p && line_max < ptr->line_nums[i])
		line_max = ptr->line_nums[i];

	      if (func_p)
		{
		  p = (ptr->functions[i]) ? (ptr->functions[i]) : "<none>";
		  len = strlen (p);
		  if (func_len < len)
		    func_len = len;
		}

	      if (file_p)
		{
		  p = (ptr->filenames[i]) ? (ptr->filenames[i]) : "<none>";
		  len = strlen (p);
		  if (file_len < len)
		    file_len = len;
		}
	    }

	  addr_len = num_digits (addr_max, 16);
	  cnt_len  = num_digits (cnt_max, 10);
	  line_len = num_digits (line_max, 10);

	  /* Now print out the basic block information.  */
	  for (i = 0; i < ncounts; i++)
	    {
	      fprintf (file,
		       /* CYGNUS LOCAL: gcov */
		       "    Block #%*d: executed %*ld time(s)",
		       blk_len, i+1,
		       cnt_len, ptr->counts[i]);

	      if (addr_p)
		fprintf (file, " address= 0x%.*lx", addr_len,
			 ptr->addresses[i]);

	      if (func_p)
		fprintf (file, " function= %-*s", func_len,
			 (ptr->functions[i]) ? ptr->functions[i] : "<none>");

	      if (line_p)
		fprintf (file, " line= %*ld", line_len, ptr->line_nums[i]);

	      if (file_p)
		fprintf (file, " file= %s",
			 (ptr->filenames[i]) ? ptr->filenames[i] : "<none>");

	      fprintf (file, "\n");
	    }

	  fprintf (file, "\n");
	  fflush (file);
	}

      fprintf (file, "\n\n");
      fclose (file);
    }
}

void
__bb_init_func (struct bb *blocks)
{
  /* User is supposed to check whether the first word is non-0,
     but just in case....  */

  if (blocks->zero_word)
    return;

#ifdef ON_EXIT
  /* Initialize destructor.  */
  if (!bb_head)
    ON_EXIT (__bb_exit_func, 0);
#endif

  /* Set up linked list.  */
  blocks->zero_word = 1;
  blocks->next = bb_head;
  bb_head = blocks;
}

#ifndef MACHINE_STATE_SAVE
#define MACHINE_STATE_SAVE(ID)
#endif
#ifndef MACHINE_STATE_RESTORE
#define MACHINE_STATE_RESTORE(ID)
#endif

#include <string.h>

/* Number of buckets in hashtable of basic block addresses.  */

#define BB_BUCKETS 311

/* Maximum length of string in file bb.in.  */

#define BBINBUFSIZE 500

/* BBINBUFSIZE-1 with double quotes. We could use #BBINBUFSIZE or
   "BBINBUFSIZE" but want to avoid trouble with preprocessors.  */

#define BBINBUFSIZESTR "499"

struct bb_edge
{
  struct bb_edge *next;
  unsigned long src_addr;
  unsigned long dst_addr;
  unsigned long count;
};

enum bb_func_mode
{
  TRACE_KEEP = 0, TRACE_ON = 1, TRACE_OFF = 2
};

struct bb_func
{
  struct bb_func *next;
  char *funcname;
  char *filename;
  enum bb_func_mode mode;
};

/* This is the connection to the outside world.
   The BLOCK_PROFILER macro must set __bb.blocks
   and __bb.blockno.  */

struct {
  unsigned long blockno;
  struct bb *blocks;
} __bb;

/* Vars to store addrs of source and destination basic blocks 
   of a jump.  */

static unsigned long bb_src = 0;
static unsigned long bb_dst = 0;

static FILE *bb_tracefile = (FILE *) 0;
static struct bb_edge **bb_hashbuckets = (struct bb_edge **) 0;
static struct bb_func *bb_func_head = (struct bb_func *) 0;
static unsigned long bb_callcount = 0;
static int bb_mode = 0;

static unsigned long *bb_stack = (unsigned long *) 0;
static size_t bb_stacksize = 0;

static int reported = 0;

/* Trace modes:
Always             :   Print execution frequencies of basic blocks
                       to file bb.out.
bb_mode & 1 != 0   :   Dump trace of basic blocks to file bbtrace[.gz]
bb_mode & 2 != 0   :   Print jump frequencies to file bb.out.
bb_mode & 4 != 0   :   Cut call instructions from basic block flow.
bb_mode & 8 != 0   :   Insert return instructions in basic block flow.
*/

#ifdef HAVE_POPEN

/*#include <sys/types.h>*/
#include <sys/stat.h>
/*#include <malloc.h>*/

/* Commands executed by gopen.  */

#define GOPENDECOMPRESS "gzip -cd "
#define GOPENCOMPRESS "gzip -c >"

/* Like fopen but pipes through gzip.  mode may only be "r" or "w".
   If it does not compile, simply replace gopen by fopen and delete
   '.gz' from any first parameter to gopen.  */

static FILE *
gopen (fn, mode)
     char *fn;
     char *mode;
{
  int use_gzip;
  char *p;

  if (mode[1])
    return (FILE *) 0;

  if (mode[0] != 'r' && mode[0] != 'w') 
    return (FILE *) 0;

  p = fn + strlen (fn)-1;
  use_gzip = ((p[-1] == '.' && (p[0] == 'Z' || p[0] == 'z')) ||
              (p[-2] == '.' && p[-1] == 'g' && p[0] == 'z'));

  if (use_gzip)
    {
      if (mode[0]=='r')
        {
          FILE *f;
          char *s = (char *) malloc (sizeof (char) * strlen (fn)
				     + sizeof (GOPENDECOMPRESS));
          strcpy (s, GOPENDECOMPRESS);
          strcpy (s + (sizeof (GOPENDECOMPRESS)-1), fn);
          f = popen (s, mode);
          free (s);
          return f;
        }

      else
        {
          FILE *f;
          char *s = (char *) malloc (sizeof (char) * strlen (fn)
				     + sizeof (GOPENCOMPRESS));
          strcpy (s, GOPENCOMPRESS);
          strcpy (s + (sizeof (GOPENCOMPRESS)-1), fn);
          if (!(f = popen (s, mode)))
            f = fopen (s, mode);
          free (s);
          return f;
        }
    }

  else
    return fopen (fn, mode);
}

static int
gclose (f)
     FILE *f;
{
  struct stat buf;

  if (f != NULL)
    {
      if (!fstat (fileno (f), &buf) && S_ISFIFO (buf.st_mode))
        return pclose (f);

      return fclose (f);
    }
  return 0;
}

#endif /* HAVE_POPEN */

/* Called once per program.  */

static void
__bb_exit_trace_func ()
{
  FILE *file = fopen ("bb.out", "a");
  struct bb_func *f;
  struct bb_edge *e;
  struct bb *b;
        
  if (!file)
    perror ("bb.out");

  if (bb_mode & 1)
    {
      if (!bb_tracefile)
        perror ("bbtrace");
      else
#ifdef HAVE_POPEN
        gclose (bb_tracefile);
#else
        fclose (bb_tracefile);
#endif /* HAVE_POPEN */
    }

  /* Check functions in `bb.in'.  */

  if (file)
    {
      long time_value;
      const struct bb_func *p;
      int printed_something = 0;
      struct bb *ptr;
      long blk;

      /* This is somewhat type incorrect.  */
      time ((void *) &time_value);

      for (p = bb_func_head; p != (struct bb_func *) 0; p = p->next)
        {
          for (ptr = bb_head; ptr != (struct bb *) 0; ptr = ptr->next)
            {
              if (!ptr->filename || p->filename != (char *) 0 && strcmp (p->filename, ptr->filename))
                continue;
              for (blk = 0; blk < ptr->ncounts; blk++)
                {
                  if (!strcmp (p->funcname, ptr->functions[blk]))
                    goto found;
                }
            }
  
          if (!printed_something)
            {
              fprintf (file, "Functions in `bb.in' not executed during basic block profiling on %s\n", ctime ((void *) &time_value));
              printed_something = 1;
            }

          fprintf (file, "\tFunction %s", p->funcname);
          if (p->filename)
              fprintf (file, " of file %s", p->filename);
          fprintf (file, "\n" );
  
found:        ;
        }

      if (printed_something)
       fprintf (file, "\n");

    }

  if (bb_mode & 2)
    {
      if (!bb_hashbuckets)
        {
          if (!reported)
            {
              fprintf (stderr, "Profiler: out of memory\n");
              reported = 1;
            }
          return;
        }
    
      else if (file)
        {
          long time_value;
          int i;
          unsigned long addr_max = 0;
          unsigned long cnt_max  = 0;
          int cnt_len;
          int addr_len;
    
          /* This is somewhat type incorrect, but it avoids worrying about
             exactly where time.h is included from.  It should be ok unless
             a void * differs from other pointer formats, or if sizeof (long)
             is < sizeof (time_t).  It would be nice if we could assume the
             use of rationale standards here.  */
    
          time ((void *) &time_value);
          fprintf (file, "Basic block jump tracing");

          switch (bb_mode & 12)
            {
              case 0:
                fprintf (file, " (with call)");
              break;

              case 4:
		/* Print nothing.  */
              break;

              case 8:
                fprintf (file, " (with call & ret)");
              break;

              case 12:
                fprintf (file, " (with ret)");
              break;
            }

          fprintf (file, " finished on %s\n", ctime ((void *) &time_value));
    
          for (i = 0; i < BB_BUCKETS; i++)
            {
               struct bb_edge *bucket = bb_hashbuckets[i];
               for ( ; bucket; bucket = bucket->next )
                 {
                   if (addr_max < bucket->src_addr) 
                     addr_max = bucket->src_addr;
                   if (addr_max < bucket->dst_addr) 
                     addr_max = bucket->dst_addr;
                   if (cnt_max < bucket->count) 
                     cnt_max = bucket->count;
                 }
            }
          addr_len = num_digits (addr_max, 16);
          cnt_len  = num_digits (cnt_max, 10);
    
          for ( i = 0; i < BB_BUCKETS; i++)
            {
               struct bb_edge *bucket = bb_hashbuckets[i];
               for ( ; bucket; bucket = bucket->next )
                 {
                   fprintf (file, "Jump from block 0x%.*lx to "
                                  "block 0x%.*lx executed %*d time(s)\n", 
                            addr_len, bucket->src_addr, 
                            addr_len, bucket->dst_addr, 
                            cnt_len, bucket->count);
                 }
            }
  
          fprintf (file, "\n");

        }
    }

   if (file)
     fclose (file);

   /* Free allocated memory.  */

   f = bb_func_head;
   while (f)
     {
       struct bb_func *old = f;

       f = f->next;
       if (old->funcname) free (old->funcname);
       if (old->filename) free (old->filename);
       free (old);
     }

   if (bb_stack)
     free (bb_stack);

   if (bb_hashbuckets)
     {
       int i;

       for (i = 0; i < BB_BUCKETS; i++)
         {
           struct bb_edge *old, *bucket = bb_hashbuckets[i];

           while (bucket)
             {
               old = bucket;
               bucket = bucket->next;
               free (old);
             }
         }
       free (bb_hashbuckets);
     }

   for (b = bb_head; b; b = b->next)
     if (b->flags) free (b->flags);
}

/* Called once per program.  */

static void
__bb_init_prg ()
{

  FILE *file;
  char buf[BBINBUFSIZE];
  const char *p;
  const char *pos;
  enum bb_func_mode m;

#ifdef ON_EXIT
  /* Initialize destructor.  */
  ON_EXIT (__bb_exit_func, 0);
#endif

  if (!(file = fopen ("bb.in", "r")))
    return;

  while(fscanf (file, " %" BBINBUFSIZESTR "s ", buf) != EOF)
    {
      p = buf;
      if (*p == '-') 
        { 
          m = TRACE_OFF; 
          p++; 
        }
      else 
        { 
          m = TRACE_ON; 
        }
      if (!strcmp (p, "__bb_trace__"))
        bb_mode |= 1;
      else if (!strcmp (p, "__bb_jumps__"))
        bb_mode |= 2;
      else if (!strcmp (p, "__bb_hidecall__"))
        bb_mode |= 4;
      else if (!strcmp (p, "__bb_showret__"))
        bb_mode |= 8;
      else 
        {
          struct bb_func *f = (struct bb_func *) malloc (sizeof (struct bb_func));
          if (f)
            {
              unsigned long l;
              f->next = bb_func_head;
              if (pos = strchr (p, ':'))
                {
                  if (!(f->funcname = (char *) malloc (strlen (pos+1)+1)))
                    continue;
                  strcpy (f->funcname, pos+1);
                  l = pos-p;
                  if ((f->filename = (char *) malloc (l+1)))
                    {
                      strncpy (f->filename, p, l);
                      f->filename[l] = '\0';
                    }
                  else
                    f->filename = (char *) 0;
                }
              else
                {
                  if (!(f->funcname = (char *) malloc (strlen (p)+1)))
                    continue;
                  strcpy (f->funcname, p);
                  f->filename = (char *) 0;
                }
              f->mode = m;
              bb_func_head = f;
	    }
         }
    }
  fclose (file);

#ifdef HAVE_POPEN 

  if (bb_mode & 1)
      bb_tracefile = gopen ("bbtrace.gz", "w");

#else

  if (bb_mode & 1)
      bb_tracefile = fopen ("bbtrace", "w");

#endif /* HAVE_POPEN */

  if (bb_mode & 2)
    {
      bb_hashbuckets = (struct bb_edge **) 
                   malloc (BB_BUCKETS * sizeof (struct bb_edge *));
      if (bb_hashbuckets)
        bzero ((char *) bb_hashbuckets, BB_BUCKETS);
    }

  if (bb_mode & 12)
    {
      bb_stacksize = 10;
      bb_stack = (unsigned long *) malloc (bb_stacksize * sizeof (*bb_stack));
    }

#ifdef ON_EXIT
      /* Initialize destructor.  */
      ON_EXIT (__bb_exit_trace_func, 0);
#endif

}

/* Called upon entering a basic block.  */

void
__bb_trace_func ()
{
  struct bb_edge *bucket;

  MACHINE_STATE_SAVE("1")

  if (!bb_callcount || (__bb.blocks->flags && (__bb.blocks->flags[__bb.blockno] & TRACE_OFF)))
    goto skip;

  bb_dst = __bb.blocks->addresses[__bb.blockno];
  __bb.blocks->counts[__bb.blockno]++;

  if (bb_tracefile)
    {
      fwrite (&bb_dst, sizeof (unsigned long), 1, bb_tracefile);
    }

  if (bb_hashbuckets)
    {
      struct bb_edge **startbucket, **oldnext;

      oldnext = startbucket =
          & bb_hashbuckets[ (((int) bb_src*8) ^ (int) bb_dst) % BB_BUCKETS ];
      bucket = *startbucket;

      for (bucket = *startbucket; bucket; 
           oldnext = &(bucket->next), bucket = *oldnext)
        {
          if ( bucket->src_addr == bb_src &&
               bucket->dst_addr == bb_dst )
            {
              bucket->count++;
              *oldnext = bucket->next;
              bucket->next = *startbucket;
              *startbucket = bucket;
              goto ret;
            }
        }

      bucket = (struct bb_edge *) malloc (sizeof (struct bb_edge));

      if (!bucket)
        {
          if (!reported)
            {
              fprintf (stderr, "Profiler: out of memory\n");
              reported = 1;
            }
        }

      else
        {
          bucket->src_addr = bb_src;
          bucket->dst_addr = bb_dst;
          bucket->next = *startbucket;
          *startbucket = bucket;
          bucket->count = 1;
        }
    }

ret:
  bb_src = bb_dst;

skip:
  ;

  MACHINE_STATE_RESTORE("1")

}

/* Called when returning from a function and `__bb_showret__' is set.  */

static void
__bb_trace_func_ret ()
{
  struct bb_edge *bucket;

  if (!bb_callcount || (__bb.blocks->flags && (__bb.blocks->flags[__bb.blockno] & TRACE_OFF)))
    goto skip;

  if (bb_hashbuckets)
    {
      struct bb_edge **startbucket, **oldnext;

      oldnext = startbucket =
          & bb_hashbuckets[ (((int) bb_dst * 8) ^ (int) bb_src) % BB_BUCKETS ];
      bucket = *startbucket;

      for (bucket = *startbucket; bucket; 
           oldnext = &(bucket->next), bucket = *oldnext)
        {
          if ( bucket->src_addr == bb_dst &&
               bucket->dst_addr == bb_src )
            {
              bucket->count++;
              *oldnext = bucket->next;
              bucket->next = *startbucket;
              *startbucket = bucket;
              goto ret;
            }
        }

      bucket = (struct bb_edge *) malloc (sizeof (struct bb_edge));

      if (!bucket)
        {
          if (!reported)
            {
              fprintf (stderr, "Profiler: out of memory\n");
              reported = 1;
            }
        }

      else
        {
          bucket->src_addr = bb_dst;
          bucket->dst_addr = bb_src;
          bucket->next = *startbucket;
          *startbucket = bucket;
          bucket->count = 1;
        }
    }

ret:
  bb_dst = bb_src;

skip:
  ;

}

/* Called upon entering the first function of a file.  */

static void
__bb_init_file (blocks)
     struct bb *blocks;
{

  const struct bb_func *p;
  long blk, ncounts = blocks->ncounts;
  const char **functions = blocks->functions;

  /* Set up linked list.  */
  blocks->zero_word = 1;
  blocks->next = bb_head;
  bb_head = blocks;

  blocks->flags = 0;
  if (!bb_func_head ||
      !(blocks->flags = (char *) malloc (sizeof (char) * blocks->ncounts)))
    return;

  for (blk = 0; blk < ncounts; blk++)
    blocks->flags[blk] = 0;

  for (blk = 0; blk < ncounts; blk++)
    {
      for (p = bb_func_head; p; p = p->next)
        {
          if (!strcmp (p->funcname, functions[blk]) &&
              (!p->filename || !strcmp (p->filename, blocks->filename)))
            {
              blocks->flags[blk] |= p->mode;
            }
        }
    }

}

/* Called when exiting from a function.  */

void
__bb_trace_ret ()
{

  MACHINE_STATE_SAVE("2")

  if (bb_callcount)
    {
      if ((bb_mode & 12) && bb_stacksize > bb_callcount)
        {
          bb_src = bb_stack[bb_callcount];
          if (bb_mode & 8)
            __bb_trace_func_ret ();
        }

      bb_callcount -= 1;
    }

  MACHINE_STATE_RESTORE("2")

}

/* Called when entering a function.  */

void
__bb_init_trace_func (blocks, blockno)
     struct bb *blocks;
     unsigned long blockno;
{
  static int trace_init = 0;

  MACHINE_STATE_SAVE("3")

  if (!blocks->zero_word)
    { 
      if (!trace_init)
        { 
          trace_init = 1;
          __bb_init_prg ();
        }
      __bb_init_file (blocks);
    }

  if (bb_callcount)
    {

      bb_callcount += 1;

      if (bb_mode & 12)
        {
          if (bb_callcount >= bb_stacksize)
            {
              size_t newsize = bb_callcount + 100;

              bb_stack = (unsigned long *) realloc (bb_stack, newsize);
              if (! bb_stack)
                {
                  if (!reported)
                    {
                      fprintf (stderr, "Profiler: out of memory\n");
                      reported = 1;
                    }
                  bb_stacksize = 0;
                  goto stack_overflow;
                }
	      bb_stacksize = newsize;
            }
          bb_stack[bb_callcount] = bb_src;

          if (bb_mode & 4)
            bb_src = 0;

        }

stack_overflow:;

    }

  else if (blocks->flags && (blocks->flags[blockno] & TRACE_ON))
    {
      bb_callcount = 1;
      bb_src = 0;

      if (bb_stack)
          bb_stack[bb_callcount] = bb_src;
    }

  MACHINE_STATE_RESTORE("3")
}

#endif /* not inhibit_libc */
#endif /* not BLOCK_PROFILER_CODE */
#endif /* L_bb */

/* Default free-store management functions for C++, per sections 12.5 and
   17.3.3 of the Working Paper.  */

#ifdef L_op_new
/* operator new (size_t), described in 17.3.3.5.  This function is used by
   C++ programs to allocate a block of memory to hold a single object.  */

typedef void (*vfp)(void);
extern vfp __new_handler;
extern void __default_new_handler (void);

#ifdef WEAK_ALIAS
void * __builtin_new (size_t sz)
     __attribute__ ((weak, alias ("___builtin_new")));
void *
___builtin_new (size_t sz)
#else
void *
__builtin_new (size_t sz)
#endif
{
  void *p;
  vfp handler = (__new_handler) ? __new_handler : __default_new_handler;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = (void *) malloc (sz);
  while (p == 0)
    {
      (*handler) ();
      p = (void *) malloc (sz);
    }
  
  return p;
}
#endif /* L_op_new */

#ifdef L_op_vnew
/* void * operator new [] (size_t), described in 17.3.3.6.  This function
   is used by C++ programs to allocate a block of memory for an array.  */

extern void * __builtin_new (size_t);

#ifdef WEAK_ALIAS
void * __builtin_vec_new (size_t sz)
     __attribute__ ((weak, alias ("___builtin_vec_new")));
void *
___builtin_vec_new (size_t sz)
#else
void *
__builtin_vec_new (size_t sz)
#endif
{
  return __builtin_new (sz);
}
#endif /* L_op_vnew */

#ifdef L_new_handler
/* set_new_handler (fvoid_t *) and the default new handler, described in
   17.3.3.2 and 17.3.3.5.  These functions define the result of a failure
   to allocate the amount of memory requested from operator new or new [].  */

#ifndef inhibit_libc
/* This gets us __GNU_LIBRARY__.  */
#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

#ifdef __GNU_LIBRARY__
  /* Avoid forcing the library's meaning of `write' on the user program
     by using the "internal" name (for use within the library)  */
#define write(fd, buf, n)	__write((fd), (buf), (n))
#endif
#endif /* inhibit_libc */

typedef void (*vfp)(void);
void __default_new_handler (void);

vfp __new_handler = (vfp) 0;

vfp
set_new_handler (vfp handler)
{
  vfp prev_handler;

  prev_handler = __new_handler;
  if (handler == 0) handler = __default_new_handler;
  __new_handler = handler;
  return prev_handler;
}

#define MESSAGE "Virtual memory exceeded in `new'\n"

void
__default_new_handler ()
{
#ifndef inhibit_libc
  /* don't use fprintf (stderr, ...) because it may need to call malloc.  */
  /* This should really print the name of the program, but that is hard to
     do.  We need a standard, clean way to get at the name.  */
  write (2, MESSAGE, sizeof (MESSAGE));
#endif
  /* don't call exit () because that may call global destructors which
     may cause a loop.  */
  _exit (-1);
}
#endif

#ifdef L_op_delete
/* operator delete (void *), described in 17.3.3.3.  This function is used
   by C++ programs to return to the free store a block of memory allocated
   as a single object.  */

#ifdef WEAK_ALIAS
void __builtin_delete (void *ptr)
     __attribute__ ((weak, alias ("___builtin_delete")));
void
___builtin_delete (void *ptr)
#else
void
__builtin_delete (void *ptr)
#endif
{
  if (ptr)
    free (ptr);
}
#endif

#ifdef L_op_vdel
/* operator delete [] (void *), described in 17.3.3.4.  This function is
   used by C++ programs to return to the free store a block of memory
   allocated as an array.  */

extern void __builtin_delete (void *);

#ifdef WEAK_ALIAS
void __builtin_vec_delete (void *ptr)
     __attribute__ ((weak, alias ("___builtin_vec_delete")));
void
___builtin_vec_delete (void *ptr)
#else
void
__builtin_vec_delete (void *ptr)
#endif
{
  __builtin_delete (ptr);
}
#endif

/* End of C++ free-store management functions */

#ifdef L_shtab
unsigned int __shtab[] = {
    0x00000001, 0x00000002, 0x00000004, 0x00000008,
    0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800,
    0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000,
    0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000,
    0x10000000, 0x20000000, 0x40000000, 0x80000000
  };
#endif

#ifdef L_clear_cache
/* Clear part of an instruction cache.  */

#define INSN_CACHE_PLANE_SIZE (INSN_CACHE_SIZE / INSN_CACHE_DEPTH)

void
__clear_cache (beg, end)
     char *beg, *end;
{
#ifdef CLEAR_INSN_CACHE 
  CLEAR_INSN_CACHE (beg, end);
#else
#ifdef INSN_CACHE_SIZE
  static char array[INSN_CACHE_SIZE + INSN_CACHE_PLANE_SIZE + INSN_CACHE_LINE_WIDTH];
  static int initialized;
  int offset;
  void *start_addr
  void *end_addr;
  typedef (*function_ptr) ();

#if (INSN_CACHE_SIZE / INSN_CACHE_LINE_WIDTH) < 16
  /* It's cheaper to clear the whole cache.
     Put in a series of jump instructions so that calling the beginning
     of the cache will clear the whole thing.  */

  if (! initialized)
    {
      int ptr = (((int) array + INSN_CACHE_LINE_WIDTH - 1)
		 & -INSN_CACHE_LINE_WIDTH);
      int end_ptr = ptr + INSN_CACHE_SIZE;

      while (ptr < end_ptr)
	{
	  *(INSTRUCTION_TYPE *)ptr
	    = JUMP_AHEAD_INSTRUCTION + INSN_CACHE_LINE_WIDTH;
	  ptr += INSN_CACHE_LINE_WIDTH;
	}
      *(INSTRUCTION_TYPE *) (ptr - INSN_CACHE_LINE_WIDTH) = RETURN_INSTRUCTION;

      initialized = 1;
    }

  /* Call the beginning of the sequence.  */
  (((function_ptr) (((int) array + INSN_CACHE_LINE_WIDTH - 1)
		    & -INSN_CACHE_LINE_WIDTH))
   ());

#else /* Cache is large.  */

  if (! initialized)
    {
      int ptr = (((int) array + INSN_CACHE_LINE_WIDTH - 1)
		 & -INSN_CACHE_LINE_WIDTH);

      while (ptr < (int) array + sizeof array)
	{
	  *(INSTRUCTION_TYPE *)ptr = RETURN_INSTRUCTION;
	  ptr += INSN_CACHE_LINE_WIDTH;
	}

      initialized = 1;
    }

  /* Find the location in array that occupies the same cache line as BEG.  */

  offset = ((int) beg & -INSN_CACHE_LINE_WIDTH) & (INSN_CACHE_PLANE_SIZE - 1);
  start_addr = (((int) (array + INSN_CACHE_PLANE_SIZE - 1)
		 & -INSN_CACHE_PLANE_SIZE)
		+ offset);

  /* Compute the cache alignment of the place to stop clearing.  */
#if 0  /* This is not needed for gcc's purposes.  */
  /* If the block to clear is bigger than a cache plane,
     we clear the entire cache, and OFFSET is already correct.  */ 
  if (end < beg + INSN_CACHE_PLANE_SIZE)
#endif
    offset = (((int) (end + INSN_CACHE_LINE_WIDTH - 1)
	       & -INSN_CACHE_LINE_WIDTH)
	      & (INSN_CACHE_PLANE_SIZE - 1));

#if INSN_CACHE_DEPTH > 1
  end_addr = (start_addr & -INSN_CACHE_PLANE_SIZE) + offset;
  if (end_addr <= start_addr)
    end_addr += INSN_CACHE_PLANE_SIZE;

  for (plane = 0; plane < INSN_CACHE_DEPTH; plane++)
    {
      int addr = start_addr + plane * INSN_CACHE_PLANE_SIZE;
      int stop = end_addr + plane * INSN_CACHE_PLANE_SIZE;

      while (addr != stop)
	{
	  /* Call the return instruction at ADDR.  */
	  ((function_ptr) addr) ();

	  addr += INSN_CACHE_LINE_WIDTH;
	}
    }
#else /* just one plane */
  do
    {
      /* Call the return instruction at START_ADDR.  */
      ((function_ptr) start_addr) ();

      start_addr += INSN_CACHE_LINE_WIDTH;
    }
  while ((start_addr % INSN_CACHE_SIZE) != offset);
#endif /* just one plane */
#endif /* Cache is large */
#endif /* Cache exists */
#endif /* CLEAR_INSN_CACHE */
}

#endif /* L_clear_cache */

#ifdef L_trampoline

/* Jump to a trampoline, loading the static chain address.  */

#ifdef WINNT

long getpagesize()
{
#ifdef _ALPHA_
  return 8192;
#else
  return 4096;
#endif
}

int mprotect(addr, len, prot)
  char *addr;
  int len, prot;
{
  int np, op;

  if (prot == 7) np = 0x40;
  else if (prot == 5) np = 0x20;
  else if (prot == 4) np = 0x10;
  else if (prot == 3) np = 0x04;
  else if (prot == 1) np = 0x02;
  else if (prot == 0) np = 0x01;

  if (VirtualProtect (addr, len, np, &op))
    return 0;
  else
    return -1;
    
}

#endif

#ifdef TRANSFER_FROM_TRAMPOLINE 
TRANSFER_FROM_TRAMPOLINE 
#endif

#if defined (NeXT) && defined (__MACH__)

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in next.h.  */
#ifdef NeXTStep21
 #include <mach.h>
#else
 #include <mach/mach.h>
#endif

void
__enable_execute_stack (addr)
     char *addr;
{
  kern_return_t r;
  char *eaddr = addr + TRAMPOLINE_SIZE;
  vm_address_t a = (vm_address_t) addr;

  /* turn on execute access on stack */
  r = vm_protect (task_self (), a, TRAMPOLINE_SIZE, FALSE, VM_PROT_ALL);
  if (r != KERN_SUCCESS)
    {
      mach_error("vm_protect VM_PROT_ALL", r);
      exit(1);
    }

  /* We inline the i-cache invalidation for speed */

#ifdef CLEAR_INSN_CACHE
  CLEAR_INSN_CACHE (addr, eaddr);
#else
  __clear_cache ((int) addr, (int) eaddr);
#endif
} 

#endif /* defined (NeXT) && defined (__MACH__) */

#ifdef __convex__

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in convex.h.  */

#include <sys/mman.h>
#include <sys/vmparam.h>
#include <machine/machparam.h>

void
__enable_execute_stack ()
{
  int fp;
  static unsigned lowest = USRSTACK;
  unsigned current = (unsigned) &fp & -NBPG;

  if (lowest > current)
    {
      unsigned len = lowest - current;
      mremap (current, &len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE);
      lowest = current;
    }

  /* Clear instruction cache in case an old trampoline is in it.  */
  asm ("pich");
}
#endif /* __convex__ */

#ifdef __DOLPHIN__

/* Modified from the convex -code above.  */

#include <sys/param.h>
#include <errno.h>
#include <sys/m88kbcs.h>

void
__enable_execute_stack ()
{
  int save_errno;
  static unsigned long lowest = USRSTACK;
  unsigned long current = (unsigned long) &save_errno & -NBPC;
  
  /* Ignore errno being set. memctl sets errno to EINVAL whenever the
     address is seen as 'negative'. That is the case with the stack.   */

  save_errno=errno;
  if (lowest > current)
    {
      unsigned len=lowest-current;
      memctl(current,len,MCT_TEXT);
      lowest = current;
    }
  else
    memctl(current,NBPC,MCT_TEXT);
  errno=save_errno;
}

#endif /* __DOLPHIN__ */

#ifdef __pyr__

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/vmmac.h>

/* Modified from the convex -code above.
   mremap promises to clear the i-cache.  */

void
__enable_execute_stack ()
{
  int fp;
  if (mprotect (((unsigned int)&fp/PAGSIZ)*PAGSIZ, PAGSIZ,
		PROT_READ|PROT_WRITE|PROT_EXEC))
    {
      perror ("mprotect in __enable_execute_stack");
      fflush (stderr);
      abort ();
    }
}
#endif /* __pyr__ */
#endif /* L_trampoline */

#ifdef L__main

#include "gbl-ctors.h"
/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#define SYMBOL__MAIN __main
#endif

#if !defined (INIT_SECTION_ASM_OP) || !defined (OBJECT_FORMAT_ELF)
/* Run all the global destructors on exit from the program.  */

void
__do_global_dtors ()
{
#ifdef DO_GLOBAL_DTORS_BODY
  DO_GLOBAL_DTORS_BODY;
#else
  func_ptr *p;
  for (p = __DTOR_LIST__ + 1; *p; )
    (*p++) ();
#endif
}
#endif

#ifndef INIT_SECTION_ASM_OP
/* Run all the global constructors on entry to the program.  */

#ifndef ON_EXIT
#define ON_EXIT(a, b)
#else
/* Make sure the exit routine is pulled in to define the globals as
   bss symbols, just in case the linker does not automatically pull
   bss definitions from the library.  */

extern int _exit_dummy_decl;
int *_exit_dummy_ref = &_exit_dummy_decl;
#endif /* ON_EXIT */

void
__do_global_ctors ()
{
  DO_GLOBAL_CTORS_BODY;
  ON_EXIT (__do_global_dtors, 0);
}
#endif /* no INIT_SECTION_ASM_OP */

#if !defined (INIT_SECTION_ASM_OP) || defined (INVOKE__main)
/* Subroutine called automatically by `main'.
   Compiling a global function named `main'
   produces an automatic call to this function at the beginning.

   For many systems, this routine calls __do_global_ctors.
   For systems which support a .init section we use the .init section
   to run __do_global_ctors, so we need not do anything here.  */

void
SYMBOL__MAIN ()
{
  /* Support recursive calls to `main': run initializers just once.  */
  static int initialized;
  if (! initialized)
    {
      initialized = 1;
      __do_global_ctors ();
    }
}
#endif /* no INIT_SECTION_ASM_OP or INVOKE__main */

#endif /* L__main */

#ifdef L_ctors

#include "gbl-ctors.h"

/* Provide default definitions for the lists of constructors and
   destructors, so that we don't get linker errors.  These symbols are
   intentionally bss symbols, so that gld and/or collect will provide
   the right values.  */

/* We declare the lists here with two elements each,
   so that they are valid empty lists if no other definition is loaded.  */
#if !defined(INIT_SECTION_ASM_OP) && !defined(CTOR_LISTS_DEFINED_EXTERNALLY)
#if defined(__NeXT__) || defined(_AIX)
/* After 2.3, try this definition on all systems.  */
func_ptr __CTOR_LIST__[2] = {0, 0};
func_ptr __DTOR_LIST__[2] = {0, 0};
#else
func_ptr __CTOR_LIST__[2];
func_ptr __DTOR_LIST__[2];
#endif
#endif /* no INIT_SECTION_ASM_OP and not CTOR_LISTS_DEFINED_EXTERNALLY */
#endif /* L_ctors */

#ifdef L_exit

#include "gbl-ctors.h"

#ifdef NEED_ATEXIT
# ifdef ON_EXIT
#  undef ON_EXIT
# endif
int _exit_dummy_decl = 0;	/* prevent compiler & linker warnings */
#endif

#ifndef ON_EXIT

#ifdef NEED_ATEXIT
# include <errno.h>

extern void *malloc ();
extern void *realloc ();

static func_ptr *atexit_chain = NULL;
static long atexit_chain_length = 0;
static volatile long last_atexit_chain_slot = -1;

int atexit (func_ptr func)
{
  if (++last_atexit_chain_slot == atexit_chain_length)
    {
      atexit_chain_length += 32;
      if (atexit_chain)
	atexit_chain = realloc (atexit_chain,
				atexit_chain_length * sizeof (func_ptr));
      else
	atexit_chain = malloc (atexit_chain_length * sizeof (func_ptr));
      if (! atexit_chain)
	{
	  atexit_chain_length = 0;
	  last_atexit_chain_slot = -1;
	  errno = ENOMEM;
	  return (-1);
	}
    }
  atexit_chain[last_atexit_chain_slot] = func;
  return (0);
}
#endif /* NEED_ATEXIT */

/* If we have no known way of registering our own __do_global_dtors
   routine so that it will be invoked at program exit time, then we
   have to define our own exit routine which will get this to happen.  */

extern void __do_global_dtors ();
/* CYGNUS LOCAL: gcov */
extern void __bb_exit_func ();
/* END CYGNUS LOCAL */
extern void _cleanup ();
extern void _exit () __attribute__ ((noreturn));

void 
exit (status)
     int status;
{
#if !defined (INIT_SECTION_ASM_OP) || !defined (OBJECT_FORMAT_ELF)
#ifdef NEED_ATEXIT
  if (atexit_chain)
    {
      for ( ; last_atexit_chain_slot-- >= 0; )
	{
	  (*atexit_chain[last_atexit_chain_slot + 1]) ();
	  atexit_chain[last_atexit_chain_slot + 1] = NULL;
	}
      free (atexit_chain);
      atexit_chain = NULL;
    }
#else /* No NEED_ATEXIT */
  __do_global_dtors ();
#endif /* No NEED_ATEXIT */
#endif
/* CYGNUS LOCAL: gcov */
#ifndef inhibit_libc
  __bb_exit_func ();
#endif
/* END CYGNUS LOCAL */
#ifdef EXIT_BODY
  EXIT_BODY;
#else
  _cleanup ();
#endif
  _exit (status);
}

#else
int _exit_dummy_decl = 0;	/* prevent compiler & linker warnings */
#endif

#endif /* L_exit */

#ifdef L_eh

#ifdef EH_TABLE_LOOKUP

EH_TABLE_LOOKUP

#else

typedef struct {
  void *start;
  void *end;
  void *exception_handler;
} exception_table;

struct exception_table_node {
  exception_table *table;
  void *start;
  void *end;
  struct exception_table_node *next;
};

static int except_table_pos;
static void *except_pc;
static struct exception_table_node *exception_table_list;

static exception_table *
find_exception_table (pc)
     void *pc;
{
  register struct exception_table_node *table = exception_table_list;
  for ( ; table != 0; table = table->next)
    {
      if (table->start <= pc && table->end > pc)
	return table->table;
    }
  return 0;
}

/* this routine takes a pc, and the address of the exception handler associated
   with the closest exception table handler entry associated with that PC,
   or 0 if there are no table entries the PC fits in.  The algorithm works
   something like this:

    while(current_entry exists) {
        if(current_entry.start < pc )
            current_entry = next_entry;
        else {
            if(prev_entry.start <= pc && prev_entry.end > pc) {
                save pointer to prev_entry;
                return prev_entry.exception_handler;
             }
            else return 0;
         }
     }
    return 0;

   Assuming a correctly sorted table (ascending order) this routine should
   return the tightest match...

   In the advent of a tie, we have to give the last entry, as it represents
   an inner block.  */

void *
__find_first_exception_table_match(pc)
void *pc;
{
  exception_table *table = find_exception_table (pc);
  int pos = 0;
  int best = 0;
  if (table == 0)
    return (void *) 0;
#if 0
  printf ("find_first_exception_table_match(): pc = %x!\n", pc);
#endif

  except_pc = pc;

#if 0
  /* We can't do this yet, as we don't know that the table is sorted.  */
  do {
    ++pos;
    if (table[pos].start > except_pc)
      /* found the first table[pos].start > except_pc, so the previous
	 entry better be the one we want! */
      break;
  } while (table[pos].exception_handler != (void *) -1);

  --pos;
  if (table[pos].start <= except_pc && table[pos].end > except_pc)
    {
      except_table_pos = pos;
#if 0
      printf ("find_first_eh_table_match(): found match: %x\n", table[pos].exception_handler);
#endif
      return table[pos].exception_handler;
    }
#else
  while (table[++pos].exception_handler != (void *) -1) {
    if (table[pos].start <= except_pc && table[pos].end > except_pc)
      {
	/* This can apply.  Make sure it is better or as good as the previous
	   best.  */
	/* The best one ends first.  */
	if (best == 0 || (table[pos].end <= table[best].end
			  /* The best one starts last.  */
			  && table[pos].start >= table[best].start))
	  best = pos;
      }
  }
  if (best != 0)
    return table[best].exception_handler;
#endif

#if 0
  printf ("find_first_eh_table_match(): else: returning NULL!\n");
#endif
  return (void *) 0;
}

void
__register_exceptions (exception_table *table)
{
  struct exception_table_node *node;
  exception_table *range = table + 1;

  if (range->start == (void *) -1)
    return;

  node = (struct exception_table_node *)
    malloc (sizeof (struct exception_table_node));
  node->table = table;

  /* This look can be optimized away either if the table
     is sorted, or if we pass in extra parameters.  */
  node->start = range->start;
  node->end = range->end;
  for (range++ ; range->start != (void *) (-1); range++)
    {
      if (range->start < node->start)
	node->start = range->start;
      if (range->end > node->end)
	node->end = range->end;
    }

  node->next = exception_table_list;
  exception_table_list = node;
}
#endif

void *
__throw_type_match (void *catch_type, void *throw_type, void *obj)
{
#if 0
 printf ("__throw_type_match (): catch_type = %s, throw_type = %s\n",
	 catch_type, throw_type);
#endif
 if (strcmp ((const char *)catch_type, (const char *)throw_type) == 0)
   return obj;
 return 0;
}

/* Throw stub routine.

   This is work in progress, but not completed yet.  */

void
__throw ()
{
  abort ();
}

/* This value identifies the place from which an exception is being
   thrown.  */

void *__eh_pc;

void
__empty ()
{
}

#if #machine(i386)
void
__unwind_function(void *ptr)
{
  asm("movl 8(%esp),%ecx");
  /* Undo current frame */
  asm("movl %ebp,%esp");
  asm("popl %ebp");
  /* like ret, but stay here */
  asm("addl $4,%esp");
  
  /* Now, undo previous frame.  */
  /* This is a test routine, as we have to dynamically probe to find out
     what to pop for certain, this is just a guess.  */
  asm("leal -16(%ebp),%esp");
  asm("pop %ebx");
  asm("pop %esi");
  asm("pop %edi");
  asm("movl %ebp,%esp");
  asm("popl %ebp");

  asm("movl %ecx,0(%esp)");
  asm("ret");
}
#elif #machine(rs6000) && !defined _ARCH_PPC
__unwind_function(void *ptr)
{
  asm("mr 31,1");
  asm("l 1,0(1)");
  asm("l 31,-4(1)");
  asm("# br");

  asm("mr 31,1");
  asm("l 1,0(1)");
  /* use 31 as a scratch register to restore the link register.  */
  asm("l 31, 8(1);mtlr 31 # l lr,8(1)");
  asm("l 31,-4(1)");
  asm("# br");
  asm("mtctr 3;bctr # b 3");
}
#elif (#machine(rs6000) || #machine(powerpc)) && defined _ARCH_PPC
__unwind_function(void *ptr)
{
  asm("mr 31,1");
  asm("lwz 1,0(1)");
  asm("lwz 31,-4(1)");
  asm("# br");

  asm("mr 31,1");
  asm("lwz 1,0(1)");
  /* use 31 as a scratch register to restore the link register.  */
  asm("lwz 31, 8(1);mtlr 31 # l lr,8(1)");
  asm("lwz 31,-4(1)");
  asm("# br");
  asm("mtctr 3;bctr # b 3");
}
#elif #machine(vax)
__unwind_function(void *ptr)
{
  __label__ return_again;

  /* Replace our frame's return address with the label below.
     During execution, we will first return here instead of to
     caller, then second return takes caller's frame off the stack.
     Two returns matches two actual calls, so is less likely to
     confuse debuggers.  `16' corresponds to RETURN_ADDRESS_OFFSET.  */
  __asm ("movl %0,16(fp)" : : "p" (&& return_again));
  return;

 return_again:
  return;
}
#else
__unwind_function(void *ptr)
{
  abort ();
}
#endif /* powerpc */
#endif /* L_eh */

#ifdef L_pure
#ifndef inhibit_libc
/* This gets us __GNU_LIBRARY__.  */
#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

#ifdef __GNU_LIBRARY__
  /* Avoid forcing the library's meaning of `write' on the user program
     by using the "internal" name (for use within the library)  */
#define write(fd, buf, n)	__write((fd), (buf), (n))
#endif
#endif /* inhibit_libc */

#define MESSAGE "pure virtual method called\n"

void
__pure_virtual ()
{
#ifndef inhibit_libc
  write (2, MESSAGE, sizeof (MESSAGE) - 1);
#endif
  _exit (-1);
}
#endif
