/* Simulator Floating-point support.
   Copyright (C) 1997 Free Software Foundation, Inc.
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



#ifndef SIM_FPU_H
#define SIM_FPU_H



/* The FPU intermediate type - this object, passed by reference,
   should be treated as opaque.

   Pragmatics - pass struct by ref:

   The alternatives for this object/interface that were considered
   were: a packed 64 bit value; an unpacked structure passed by value;
   and an unpacked structure passed by reference.

   The packed 64 bit value was rejected because: it limited the
   precision of intermediate values; reasonable performance would only
   be achieved when the sim_fpu package was in-lined allowing repeated
   unpacking operations to be eliminated.

   For unpacked structures (passed by value and reference), the code
   quality of GCC-2.7 (on x86) for each alternative was compared.
   Needless to say the results, while better then for a packed 64 bit
   object, were still poor (GCC had only limited support for the
   optimization of references to structure members).  Regardless, the
   struct-by-ref alternative achieved better results when compiled
   with (better speed) and without (better code density) in-lining.
   Here's looking forward to an improved GCC optimizer.

   Pragmatics - avoid host FP hardware:

   FP operations can be implemented by either: the host's floating
   point hardware; or by emulating the FP operations using integer
   only routines.  This is direct tradeoff between speed, portability
   and correctness.

   The two principal reasons for selecting portability and correctness
   over speed are:

   1 - Correctness.  The assumption that FP correctness wasn't an
   issue for code being run on simulators was wrong.  Instead of
   running FP tolerant (?) code, simulator users instead typically run
   very aggressive FP code sequences.  The sole purpose of those
   sequences being to test the target ISA's FP implementation.

   2 - Portability.  The host FP implementation is not predictable.  A
   simulator modeling aggressive FP code sequences using the hosts FPU
   relies heavily on the correctness of the hosts FP implementation.
   It turns out that such trust can be misplaced.  The behavior of
   host FP implementations when handling edge conditions such as SNaNs
   and exceptions varied widely.

   */


typedef enum 
{
  sim_fpu_class_zero,
  sim_fpu_class_snan,
  sim_fpu_class_qnan,
  sim_fpu_class_number,
  sim_fpu_class_infinity,
} sim_fpu_class;

typedef struct _sim_fpu {
  sim_fpu_class class;
  int normal_exp;
  int result;
  int sign;
  unsigned64 fraction;
} sim_fpu;



/* Rounding options.

   The value zero (sim_fpu_round_default) for ALU operations indicates
   that, when possible, rounding should be avoided. */

typedef enum
{
  sim_fpu_round_default = 0,
  sim_fpu_round_near = 1,
  sim_fpu_round_zero = 2,
  sim_fpu_round_up = 3,
  sim_fpu_round_down = 4,
} sim_fpu_round;


/* Options when handling denormalized numbers.  */

typedef enum
{
  sim_fpu_denorm_underflow_inexact = 1,
  sim_fpu_denorm_zero =2,
} sim_fpu_denorm;



/* Status values returned by FPU operators.

   When checking the result of an FP sequence (ex 32to, add, single,
   to32) the caller may either: check the return value of each FP
   operator; or form the union (OR) of the returned values and examine
   them once at the end.

   FIXME: This facility is still being developed.  The choice of
   status values returned and their exact meaning may changed in the
   future.  */

typedef enum
{
  sim_fpu_status_invalid_snan = 1,
  sim_fpu_status_invalid_qnan = 2,
  sim_fpu_status_invalid_isi = 4, /* (inf - inf) */
  sim_fpu_status_invalid_idi = 8, /* (inf / inf) */
  sim_fpu_status_invalid_zdz = 16, /* (0 / 0) */
  sim_fpu_status_invalid_imz = 32, /* (inf * 0) */
  sim_fpu_status_invalid_cvi = 64, /* convert to integer */
  sim_fpu_status_invalid_div0 = 128, /* (X / 0) */
  sim_fpu_status_invalid_cmp = 256, /* compare */
  sim_fpu_status_invalid_sqrt = 512,
  sim_fpu_status_rounded = 1024,
  sim_fpu_status_inexact = 2048,
  sim_fpu_status_overflow = 4096,
  sim_fpu_status_underflow = 8192,
  sim_fpu_status_denorm = 16384,
} sim_fpu_status;




/* Directly map between a 32/64 bit register and the sim_fpu internal
   type.

   When converting from the 32/64 bit packed format to the sim_fpu
   internal type, the operation is exact.

   When converting from the sim_fpu internal type to 32/64 bit packed
   format, the operation may result in a loss of precision. The
   configuration macro WITH_FPU_CONVERSION controls this.  By default,
   silent round to nearest is performed.  Alternativly, round up,
   round down and round to zero can be performed.  In a simulator
   emulating exact FPU behavour, sim_fpu_round_{32,64} should be
   called before packing the sim_fpu value.  */

INLINE_SIM_FPU (void) sim_fpu_32to (sim_fpu *f, unsigned32 s);
INLINE_SIM_FPU (void) sim_fpu_232to (sim_fpu *f, unsigned32 h, unsigned32 l);
INLINE_SIM_FPU (void) sim_fpu_64to (sim_fpu *f, unsigned64 d);

INLINE_SIM_FPU (void) sim_fpu_to32 (unsigned32 *s, const sim_fpu *f);
INLINE_SIM_FPU (void) sim_fpu_to232 (unsigned32 *h, unsigned32 *l, const sim_fpu *f);
INLINE_SIM_FPU (void) sim_fpu_to64 (unsigned64 *d, const sim_fpu *f);



/* Rounding operators.

   Force an intermediate result to an exact 32/64 bit
   representation. */

INLINE_SIM_FPU (int) sim_fpu_round_32 (sim_fpu *f,
				       sim_fpu_round round,
				       sim_fpu_denorm denorm);
INLINE_SIM_FPU (int) sim_fpu_round_64 (sim_fpu *f,
				       sim_fpu_round round,
				       sim_fpu_denorm denorm);



/* Arrithmetic operators.

   FIXME: In the future, additional arguments ROUNDING and BITSIZE may
   be added. */

INLINE_SIM_FPU (int) sim_fpu_add (sim_fpu *f,
				  const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_sub (sim_fpu *f,
				  const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_mul (sim_fpu *f,
				  const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_div (sim_fpu *f,
				  const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_neg (sim_fpu *f,
				  const sim_fpu *a);
INLINE_SIM_FPU (int) sim_fpu_abs (sim_fpu *f,
				  const sim_fpu *a);
INLINE_SIM_FPU (int) sim_fpu_inv (sim_fpu *f,
				  const sim_fpu *a);
INLINE_SIM_FPU (int) sim_fpu_sqrt (sim_fpu *f,
				   const sim_fpu *sqr);



/* Conversion of integer <-> floating point. */

INLINE_SIM_FPU (int) sim_fpu_i32to (sim_fpu *f, signed32 i,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_u32to (sim_fpu *f, unsigned32 u,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_i64to (sim_fpu *f, signed64 i,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_u64to (sim_fpu *f, unsigned64 u,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_i232to (sim_fpu *f, signed32 h, signed32 l,
				     sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_u232to (sim_fpu *f, unsigned32 h, unsigned32 l,
				     sim_fpu_round round);

INLINE_SIM_FPU (int) sim_fpu_to32i (signed32 *i, const sim_fpu *f,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_to32u (unsigned32 *u, const sim_fpu *f,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_to64i (signed64 *i, const sim_fpu *f,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_to64u (unsigned64 *u, const sim_fpu *f,
				    sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_to232i (signed64 *h, signed64 *l, const sim_fpu *f,
				     sim_fpu_round round);
INLINE_SIM_FPU (int) sim_fpu_to232u (unsigned64 *h, unsigned64 *l, const sim_fpu *f,
				     sim_fpu_round round);



/* Conversion of internal sim_fpu type to host double format.

   For debuging/tracing only.  A SNaN is never returned. */

/* INLINE_SIM_FPU (float) sim_fpu_2f (const sim_fpu *f); */
INLINE_SIM_FPU (double) sim_fpu_2d (const sim_fpu *d);

/* INLINE_SIM_FPU (void) sim_fpu_f2 (sim_fpu *f, float s); */
INLINE_SIM_FPU (void) sim_fpu_d2 (sim_fpu *f, double d);



/* Specific number classes */

INLINE_SIM_FPU (int) sim_fpu_is_nan (const sim_fpu *s); /* 1 => SNaN or QNaN */
INLINE_SIM_FPU (int) sim_fpu_is_snan (const sim_fpu *s); /* 1 => SNaN */
INLINE_SIM_FPU (int) sim_fpu_is_qnan (const sim_fpu *s); /* 1 => QNaN */

INLINE_SIM_FPU (int) sim_fpu_is_zero (const sim_fpu *s);
INLINE_SIM_FPU (int) sim_fpu_is_infinity (const sim_fpu *s);
INLINE_SIM_FPU (int) sim_fpu_is_number (const sim_fpu *s); /* but not zero */



/* Specific comparison operators

   The comparison operators set *IS to zero and return a nonzero
   result for NaNs et.al. */

INLINE_SIM_FPU (int) sim_fpu_lt (int *is, const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_le (int *is, const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_eq (int *is, const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_ne (int *is, const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_ge (int *is, const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_gt (int *is, const sim_fpu *l, const sim_fpu *r);

INLINE_SIM_FPU (int) sim_fpu_is_lt (const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_is_le (const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_is_eq (const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_is_ne (const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_is_ge (const sim_fpu *l, const sim_fpu *r);
INLINE_SIM_FPU (int) sim_fpu_is_gt (const sim_fpu *l, const sim_fpu *r);



/* General number class and comparison operators.

   The result of the comparison is indicated by returning one of the
   values below.  Efficient emulation of a target FP compare
   instruction can be achieved by redefining the values below to match
   corresponding target FP status bits.

   For instance.  SIM_FPU_QNAN may be redefined to be the bit
   `INVALID' while SIM_FPU_NINF might be redefined as the bits
   `NEGATIVE | INFINITY | VALID'. */

#ifndef SIM_FPU_IS_SNAN
#define SIM_FPU_IS_SNAN    1 /* Noisy not-a-number */
#define SIM_FPU_IS_QNAN    2 /* Quite not-a-number */
#define SIM_FPU_IS_NINF    3 /* -infinity */
#define SIM_FPU_IS_PINF    4 /* +infinity */
#define SIM_FPU_IS_NNUM    5 /* -number - [ -MAX .. -MIN ] */
#define SIM_FPU_IS_PNUM    6 /* +number - [ +MIN .. +MAX ] */
#define SIM_FPU_IS_NDENORM 7 /* -denorm - ( MIN .. 0 ) */
#define SIM_FPU_IS_PDENORM 8 /* +denorm - ( 0 .. MIN ) */
#define SIM_FPU_IS_NZERO   9 /* -0 */
#define SIM_FPU_IS_PZERO  10 /* +0 */
#endif

INLINE_SIM_FPU (int) sim_fpu_is (const sim_fpu *l);
INLINE_SIM_FPU (int) sim_fpu_cmp (const sim_fpu *l, const sim_fpu *r);



/* A constant of useful numbers */

extern const sim_fpu sim_fpu_zero;
extern const sim_fpu sim_fpu_one;
extern const sim_fpu sim_fpu_two;
extern const sim_fpu sim_fpu_qnan;


/* For debugging */

typedef void sim_fpu_print_func (void *, char *, ...);

INLINE_SIM_FPU (void) sim_fpu_print_fpu (const sim_fpu *f,
					 sim_fpu_print_func *print,
					 void *arg);

INLINE_SIM_FPU (void) sim_fpu_print_status (int status,
					    sim_fpu_print_func *print,
					    void *arg);

#endif
