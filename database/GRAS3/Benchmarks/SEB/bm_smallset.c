/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 This module implements a simple set

 Creation date: 25.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#include "bm_global.h"
#include "bm_smallset.h"


void
empty_set(smallset * set)
{
  *set = 0;
}



void
add_to_set(smallset * set, membertype value)
{
  if (0 <= value && value <= 31)	       /* within set range? */
    *set |= (1 << value);
}


int
is_member(smallset set, membertype value)
{
  if (value < 0 || value > 31)
    return (FALSE);			       /* error ... */
  else if (set & (1 << value))
    return (TRUE);			       /* member */
  else
    return (FALSE);			       /* not member */
}



int
is_empty(smallset set)
{
  if (set == 0)
    return (TRUE);
  else
    return (FALSE);
}
