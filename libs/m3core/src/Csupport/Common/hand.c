/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

long m3_div (b, a)
long a, b;
{
  register long c;
  if ((a == 0L) && (b != 0L))  {  c = 0L;
  } else if (a > 0L)  {  c = (b >= 0L) ? (a) / (b) : -1L - (a-1L) / (-b);
  } else /* a < 0L */ {  c = (b >= 0L) ? -1L - (-1L-a) / (b) : (-a) / (-b);
  }
  return c;
}

long m3_mod (b, a)
long a, b;
{
  register long c;
  if ((a == 0L) && (b != 0L)) {  c = 0L;
  } else if (a > 0L)  {  c = (b >= 0L) ? a % b : b + 1L + (a-1L) % (-b);
  } else /* a < 0L */ {  c = (b >= 0L) ? b - 1L - (-1L-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

#define SET_GRAIN (sizeof (long) * 8)

long set_member (elt, set)
long elt;
long* set;
{
  register long word = elt / SET_GRAIN;
  register long bit  = elt % SET_GRAIN;
  return (set[word] & (1L << bit)) != 0L;
}

void set_union (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] | c[i];
  }
}

void set_intersection (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] & c[i];
  }
}

void set_difference (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] & (~ c[i]);
  }
}

void set_sym_difference (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] ^ c[i];
  }
}

long set_eq (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if (a[i] != b[i]) return 0L;
  }
  return 1L;
}

long set_ne (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if (a[i] != b[i]) return 1L;
  }
  return 0L;
}

long set_ge (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0L;
  }
  return 1L;
}

long set_gt (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  register long eq = 0L;
  for (i = 0L; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0L;
    eq |=  (a[i] ^ b[i]);
  }
  return (eq != 0L);
}

long set_le (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0L;
  }
  return 1L;
}

long set_lt (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  register long eq = 0L;
  for (i = 0L; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0L;
    eq |= (a[i] ^ b[i]);
  }
  return (eq != 0L);
}

long tables_built = 0L;
unsigned long LoBits[SET_GRAIN];  /* LoBits [i] = SET { 0..i } */
unsigned long HiBits[SET_GRAIN];  /* HiBits [i] = SET { i..GRAIN-1 } */

void BuildTables ()
{
  long i, j;

  tables_built = 1L;

  /* LoBits [i] = SET { 0..i } */
  j = 0L;  /* == SET { } */
  for (i = 0L; i < SET_GRAIN; i++) {
    j = (j << 1L) + 1L;
    LoBits[i] = j;
  }

  /* HiBits [i] = SET { i..GRAIN-1 } */
  j = ~0L; /* == SET { 0..GRAIN-1 } */
  for (i = 0L; i < SET_GRAIN; i++) {
    HiBits[i] = j;
    j = (j << 1L);
  }
}

void set_range (b, a, s)
long b, a;
long *s;
{
  long a_word = a / SET_GRAIN;
  long a_bit  = a % SET_GRAIN;
  long b_word = b / SET_GRAIN;
  long b_bit  = b % SET_GRAIN;
  long i;

  if (!tables_built) BuildTables ();

  if (b < a) {
      /* no bits to set */
  } else if (a_word == b_word) {
      s [a_word] |= (HiBits [a_bit] & LoBits [b_bit]);
  } else {
      s [a_word] |= HiBits [a_bit];
      for (i = a_word+1L; i < b_word; i++)  s[i] = HiBits [0];
      s [b_word] |= LoBits [b_bit];
  }
}

void set_singleton (a, s)
long a;
long *s;
{
  long a_word = a / SET_GRAIN;
  long a_bit  = a % SET_GRAIN;
  s [a_word] |= 1L << a_bit;
}

/* _lowbits[i] = bits{(i-1)..0} for 32-bit integer masks */
int _lowbits [33] = {
  0x0,
  0x1, 0x3, 0x7, 0xf,
  0x1f, 0x3f, 0x7f, 0xff,
  0x1ff, 0x3ff, 0x7ff, 0xfff,
  0x1fff, 0x3fff, 0x7fff, 0xffff,
  0x1ffff, 0x3ffff, 0x7ffff, 0xfffff,
  0x1fffff, 0x3fffff, 0x7fffff, 0xffffff,
  0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff,
  0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff };

/* _highbits[i] = bits{31..i} for 32-bit integer masks */
int _highbits [33] = {
  0xffffffff, 0xfffffffe, 0xfffffffc, 0xfffffff8,
  0xfffffff0, 0xffffffe0, 0xffffffc0, 0xffffff80,
  0xffffff00, 0xfffffe00, 0xfffffc00, 0xfffff800,
  0xfffff000, 0xffffe000, 0xffffc000, 0xffff8000,
  0xffff0000, 0xfffe0000, 0xfffc0000, 0xfff80000,
  0xfff00000, 0xffe00000, 0xffc00000, 0xff800000,
  0xff000000, 0xfe000000, 0xfc000000, 0xf8000000,
  0xf0000000, 0xe0000000, 0xc0000000, 0x80000000,
  0x0 };

/************************************************************************

#include <stdio.h>

static _crash (msg)
char *msg;
{
  fprintf (stderr, "\n**** UNIMPLEMENTED: %s ***\n", msg);
  fflush (stderr);

  *((long*)0L) = 1L;    /  * bad memory reference => crash! *  /
  while (1L);           /  * if not, loop forever           *  /
}

_xx0 () { _crash ("_xx0 (runtime fault)"); }

**************************************************************************/


