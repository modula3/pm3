/* This is a stripped down version of floatlib.c.  It supplies only those
   functions which exist in libgcc, but for which there is not assembly
   language versions in m68k/lb1sf68.asm.

   It also includes simplistic support for extended floats (by working in
   double precision).  You must compile this file again with -DEXTFLOAT
   to get this support.  */

/*
** gnulib support for software floating point.
** Copyright (C) 1991 by Pipeline Associates, Inc.  All rights reserved.
** Permission is granted to do *anything* you want with this file,
** commercial or otherwise, provided this message remains intact.  So there!
** I would appreciate receiving any updates/patches/changes that anyone
** makes, and am willing to be the repository for said changes (am I
** making a big mistake?).
**
** Pat Wood
** Pipeline Associates, Inc.
** pipeline!phw@motown.com or
** sun!pipeline!phw or
** uunet!motown!pipeline!phw
**
** 05/01/91 -- V1.0 -- first release to gcc mailing lists
** 05/04/91 -- V1.1 -- added float and double prototypes and return values
**                  -- fixed problems with adding and subtracting zero
**                  -- fixed rounding in truncdfsf2
**                  -- fixed SWAP define and tested on 386
*/

/*
** The following are routines that replace the gnulib soft floating point
** routines that are called automatically when -msoft-float is selected.
** The support single and double precision IEEE format, with provisions
** for byte-swapped machines (tested on 386).  Some of the double-precision
** routines work at full precision, but most of the hard ones simply punt
** and call the single precision routines, producing a loss of accuracy.
** long long support is not assumed or included.
** Overall accuracy is close to IEEE (actually 68882) for single-precision
** arithmetic.  I think there may still be a 1 in 1000 chance of a bit
** being rounded the wrong way during a multiply.  I'm not fussy enough to
** bother with it, but if anyone is, knock yourself out.
**
** Efficiency has only been addressed where it was obvious that something
** would make a big difference.  Anyone who wants to do this right for
** best speed should go in and rewrite in assembler.
**
** I have tested this only on a 68030 workstation and 386/ix integrated
** in with -msoft-float.
*/

/* the following deal with IEEE single-precision numbers */
#define EXCESS		126L
#define SIGNBIT		0x80000000L
#define HIDDEN		(1L << 23L)
#define SIGN(fp)	((fp) & SIGNBIT)
#define EXP(fp)		(((fp) >> 23L) & 0xFF)
#define MANT(fp)	(((fp) & 0x7FFFFFL) | HIDDEN)
#define PACK(s,e,m)	((s) | ((e) << 23L) | (m))

/* the following deal with IEEE double-precision numbers */
#define EXCESSD		1022
#define HIDDEND		(1L << 20L)
#define EXPDBITS	11
#define EXPDMASK	0x7FF
#define EXPD(fp)	(((fp.l.upper) >> 20L) & 0x7FFL)
#define SIGND(fp)	((fp.l.upper) & SIGNBIT)
#define MANTD(fp)	(((((fp.l.upper) & 0xFFFFF) | HIDDEND) << 10) | \
				(fp.l.lower >> 22))
#define MANTDMASK	0xFFFFF /* mask of upper part */

/* the following deal with IEEE extended-precision numbers */
#define EXCESSX		16382
#define HIDDENX		(1L << 31L)
#define EXPXBITS	15
#define EXPXMASK	0x7FFF
#define EXPX(fp)	(((fp.l.upper) >> 16) & EXPXMASK)
#define SIGNX(fp)	((fp.l.upper) & SIGNBIT)
#define MANTXMASK	0x7FFFFFFF /* mask of upper part */

union double_long 
{
  double d;
  struct {
      long upper;
      unsigned long lower;
    } l;
};

union float_long {
  float f;
  long l;
};

union long_double_long
{
  long double ld;
  struct
    {
      long upper;
      unsigned long middle;
      unsigned long lower;
    } l;
};

#ifndef EXTFLOAT

/* convert int to double */
double
__floatsidf (int a1)
{
  long sign = 0, exp = 31 + EXCESSD;
  union double_long dl;

  if (!a1)
    {
      dl.l.upper = dl.l.lower = 0;
      return dl.d;
    }

  if (a1 < 0)
    {
      sign = SIGNBIT;
      a1 = -a1;
      if (a1 < 0)
	{
	  dl.l.upper = SIGNBIT | ((32 + EXCESSD) << 20L);
	  dl.l.lower = 0;
	  return dl.d;
        }
    }

  while (a1 < 0x1000000)
    {
      a1 <<= 4;
      exp -= 4;
    }

  while (a1 < 0x40000000)
    {
      a1 <<= 1;
      exp--;
    }

  /* pack up and go home */
  dl.l.upper = sign;
  dl.l.upper |= exp << 20L;
  dl.l.upper |= (a1 >> 10L) & ~HIDDEND;
  dl.l.lower = a1 << 22L;

  return dl.d;
}

/* convert int to float */
float
__floatsisf (int l)
{
  double foo = __floatsidf (l);
  return foo;
}

/* convert float to double */
double
__extendsfdf2 (float a1)
{
  register union float_long fl1;
  register union double_long dl;
  register long exp;

  fl1.f = a1;

  if (!fl1.l)
    {
      dl.l.upper = dl.l.lower = 0;
      return dl.d;
    }

  dl.l.upper = SIGN (fl1.l);
  exp = EXP (fl1.l) - EXCESS + EXCESSD;
  dl.l.upper |= exp << 20;
  dl.l.upper |= (MANT (fl1.l) & ~HIDDEN) >> 3;
  dl.l.lower = MANT (fl1.l) << 29;
	
  return dl.d;
}

/* convert double to float */
float
__truncdfsf2 (double a1)
{
  register long exp;
  register long mant;
  register union float_long fl;
  register union double_long dl1;

  dl1.d = a1;

  if (!dl1.l.upper && !dl1.l.lower)
    return 0;

  exp = EXPD (dl1) - EXCESSD + EXCESS;

  /* shift double mantissa 6 bits so we can round */
  mant = MANTD (dl1) >> 6;

  /* now round and shift down */
  mant += 1;
  mant >>= 1;

  /* did the round overflow? */
  if (mant & 0xFF000000)
    {
      mant >>= 1;
      exp++;
    }

  mant &= ~HIDDEN;

  /* pack up and go home */
  fl.l = PACK (SIGND (dl1), exp, mant);
  return (fl.f);
}

/* convert double to int */
int
__fixdfsi (double a1)
{
  register union double_long dl1;
  register long exp;
  register long l;

  dl1.d = a1;

  if (!dl1.l.upper && !dl1.l.lower) 
    return 0;

  exp = EXPD (dl1) - EXCESSD - 31;
  l = MANTD (dl1);

  if (exp > 0) 
    {
      /* Return largest integer.  */
      return SIGND (dl1) ? 0x80000000 : 0x7fffffff;
    }

  if (exp <= -32)
    return 0;

  /* shift down until exp = 0 */
  if (exp < 0)
    l >>= -exp;

  return (SIGND (dl1) ? -l : l);
}

/* convert float to int */
int
__fixsfsi (float a1)
{
  double foo = a1;
  return __fixdfsi (foo);
}

#else /* EXTFLOAT */

/* Primitive extended precision floating point support.

   We assume all numbers are normalized, don't do any rounding, etc.  */

/* Prototypes for the above in case we use them.  */
double __floatsidf (int);
float __floatsisf (int);
double __extendsfdf2 (float);
float __truncdfsf2 (double);
int __fixdfsi (double);
int __fixsfsi (float);

/* convert double to long double */
long double
__extenddfxf2 (double d)
{
  register union double_long dl;
  register union long_double_long ldl;
  register long exp;

  dl.d = d;
  /*printf ("dfxf in: %g\n", d);*/

  if (!dl.l.upper && !dl.l.lower)
    return 0;

  ldl.l.upper = SIGND (dl);
  exp = EXPD (dl) - EXCESSD + EXCESSX;
  ldl.l.upper |= exp << 16;
  ldl.l.middle = HIDDENX;
  /* 31-20: # mantissa bits in ldl.l.middle - # mantissa bits in dl.l.upper */
  ldl.l.middle |= (dl.l.upper & MANTDMASK) << (31 - 20);
  /* 1+20: explicit-integer-bit + # mantissa bits in dl.l.upper */
  ldl.l.middle |= dl.l.lower >> (1 + 20);
  /* 32 - 21: # bits of dl.l.lower in ldl.l.middle */
  ldl.l.lower = dl.l.lower << (32 - 21);

  /*printf ("dfxf out: %s\n", dumpxf (ldl.ld));*/
  return ldl.ld;
}

/* convert long double to double */
double
__truncxfdf2 (long double ld)
{
  register long exp;
  register union double_long dl;
  register union long_double_long ldl;

  ldl.ld = ld;
  /*printf ("xfdf in: %s\n", dumpxf (ld));*/

  if (!ldl.l.upper && !ldl.l.middle && !ldl.l.lower)
    return 0;

  exp = EXPX (ldl) - EXCESSX + EXCESSD;
  /* ??? quick and dirty: keep `exp' sane */
  if (exp >= EXPDMASK)
    exp = EXPDMASK - 1;
  dl.l.upper = SIGNX (ldl);
  dl.l.upper |= exp << (32 - (EXPDBITS + 1));
  /* +1-1: add one for sign bit, but take one off for explicit-integer-bit */
  dl.l.upper |= (ldl.l.middle & MANTXMASK) >> (EXPDBITS + 1 - 1);
  dl.l.lower = (ldl.l.middle & MANTXMASK) << (32 - (EXPDBITS + 1 - 1));
  dl.l.lower |= ldl.l.lower >> (EXPDBITS + 1 - 1);

  /*printf ("xfdf out: %g\n", dl.d);*/
  return dl.d;
}

/* convert a float to a long double */
long double
__extendsfxf2 (float f)
{
  long double foo = __extenddfxf2 (__extendsfdf2 (f));
  return foo;
}

/* convert a long double to a float */
float
__truncxfsf2 (long double ld)
{
  float foo = __truncdfsf2 (__truncxfdf2 (ld));
  return foo;
}

/* convert an int to a long double */
long double
__floatsixf (int l)
{
  double foo = __floatsidf (l);
  return foo;
}

/* convert a long double to an int */
int
__fixxfsi (long double ld)
{
  int foo = __fixdfsi ((double) ld);
  return foo;
}

/* The remaining provide crude math support by working in double precision.  */

long double
__addxf3 (long double x1, long double x2)
{
  return (double) x1 + (double) x2;
}

long double
__subxf3 (long double x1, long double x2)
{
  return (double) x1 - (double) x2;
}

long double
__mulxf3 (long double x1, long double x2)
{
  return (double) x1 * (double) x2;
}

long double
__divxf3 (long double x1, long double x2)
{
  return (double) x1 / (double) x2;
}

long double
__negxf2 (long double x1)
{
  return - (double) x1;
}

long
__cmpxf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

long
__eqxf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

long
__nexf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

long
__ltxf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

long
__lexf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

long
__gtxf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

long
__gexf2 (long double x1, long double x2)
{
  return __cmpdf2 ((double) x1, (double) x2);
}

#endif /* EXTFLOAT */
