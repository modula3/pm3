/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Fri Aug 13 09:02:20 PDT 1993 by kalsow                   */
/*      modified on Tue Feb 11 14:23:53 PST 1992 by muller                   */

#ifndef IEEE_8087
#define IEEE_8087
#endif

#define Int_32 int


#include "dtoa.h"

/* Apparently libc defines both "__dtoa" and "dtoa".  ???  */

char * __dtoa       
#ifdef KR_headers
        (d, mode, ndigits, decpt, sign, rve)
        double d; int mode, ndigits, *decpt, *sign; char **rve;
#else 
        (double d, int mode, int ndigits, int *decpt, int *sign, char **rve)
#endif
{
  return dtoa(d, mode, ndigits, decpt, sign, rve);
}
