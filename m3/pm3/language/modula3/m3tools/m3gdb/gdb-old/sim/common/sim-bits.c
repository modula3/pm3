/*  This file is part of the program psim.

    Copyright (C) 1994-1997, Andrew Cagney <cagney@highland.com.au>

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 
    */


#ifndef _SIM_BITS_C_
#define _SIM_BITS_C_

#include "sim-basics.h"
#include "sim-assert.h"
#include "sim-io.h"


INLINE_SIM_BITS\
(unsigned_word)
LSMASKED (unsigned_word val,
	  int start,
	  int stop)
{
  /* NOTE - start, stop can wrap */
  val &= LSMASK (start, stop);
  return val;
}


INLINE_SIM_BITS\
(unsigned_word)
MSMASKED (unsigned_word val,
	  int start,
	  int stop)
{
  /* NOTE - start, stop can wrap */
  val &= MSMASK (start, stop);
  return val;
}


INLINE_SIM_BITS\
(unsigned_word)
LSEXTRACTED (unsigned_word val,
	     int start,
	     int stop)
{
  ASSERT (start >= stop);
#if (WITH_TARGET_WORD_BITSIZE == 64)
  return LSEXTRACTED64 (val, start, stop);
#endif
#if (WITH_TARGET_WORD_BITSIZE == 32)
  if (stop >= 32)
    return 0;
  else
    {
      if (start < 32)
	val &= LSMASK (start, 0);
      val >>= stop;
      return val;
    }
#endif
}


INLINE_SIM_BITS\
(unsigned_word)
MSEXTRACTED (unsigned_word val,
	     int start,
	     int stop)
{
  ASSERT (start <= stop);
#if (WITH_TARGET_WORD_BITSIZE == 64)
  return MSEXTRACTED64 (val, start, stop);
#endif
#if (WITH_TARGET_WORD_BITSIZE == 32)
  if (stop < 32)
    return 0;
  else
    {
      if (start >= 32)
	val &= MSMASK (start, 64 - 1);
      val >>= (64 - stop - 1);
      return val;
    }
#endif
}


INLINE_SIM_BITS\
(unsigned_word)
LSINSERTED (unsigned_word val,
	    int start,
	    int stop)
{
  ASSERT (start >= stop);
#if (WITH_TARGET_WORD_BITSIZE == 64)
  return LSINSERTED64 (val, start, stop);
#endif
#if (WITH_TARGET_WORD_BITSIZE == 32)
  /* Bit numbers are 63..0, even for 32 bit targets.
     On 32 bit targets we ignore 63..32  */
  if (stop >= 32)
    return 0;
  else
    {
      val <<= stop;
      val &= LSMASK (start, stop);
      return val;
    }
#endif
}

INLINE_SIM_BITS\
(unsigned_word)
MSINSERTED (unsigned_word val,
	    int start,
	    int stop)
{
  ASSERT (start <= stop);
#if (WITH_TARGET_WORD_BITSIZE == 64)
  return MSINSERTED64 (val, start, stop);
#endif
#if (WITH_TARGET_WORD_BITSIZE == 32)
  /* Bit numbers are 0..63, even for 32 bit targets.
     On 32 bit targets we ignore 0..31.  */
  if (stop < 32)
    return 0;
  else
    {
      val <<= ((64 - 1) - stop);
      val &= MSMASK (start, stop);
      return val;
    }
#endif
}



INLINE_SIM_BITS\
(unsigned_word)
LSSEXT (signed_word val,
	int sign_bit)
{
  ASSERT (sign_bit < 64);
#if (WITH_TARGET_WORD_BITSIZE == 64)
  return LSSEXT64 (val, sign_bit);
#endif
#if (WITH_TARGET_WORD_BITSIZE == 32)
  if (sign_bit >= 32)
    return val;
  else {
    val = LSSEXT32 (val, sign_bit);
    return val;
  }
#endif
}

INLINE_SIM_BITS\
(unsigned_word)
MSSEXT (signed_word val,
	int sign_bit)
{
  ASSERT (sign_bit < 64);
#if (WITH_TARGET_WORD_BITSIZE == 64)
  return MSSEXT64 (val, sign_bit);
#endif
#if (WITH_TARGET_WORD_BITSIZE == 32)
  if (sign_bit < 32)
    return val;
  else {
    val = MSSEXT32 (val, sign_bit - 32);
    return val;
  }
#endif
}



#define N 8
#include "sim-n-bits.h"
#undef N

#define N 16
#include "sim-n-bits.h"
#undef N

#define N 32
#include "sim-n-bits.h"
#undef N

#define N 64
#include "sim-n-bits.h"
#undef N

#endif /* _SIM_BITS_C_ */
