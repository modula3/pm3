/*  This file is part of the program psim.

    Copyright (C) 1994-1998, Andrew Cagney <cagney@highland.com.au>

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


#include "hw-main.h"
#include "hw-base.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Address methods */

const hw_unit *
hw_unit_address (struct hw *me)
{
  return &me->unit_address_of_hw;
}


/* IOCTL: */

int
hw_ioctl (struct hw *me,
	  hw_ioctl_request request,
	  ...)
{
  int status;
  va_list ap;
  va_start(ap, request);
  status = me->to_ioctl (me, request, ap);
  va_end(ap);
  return status;
}
      
char *
hw_strdup (struct hw *me, const char *str)
{
  if (str != NULL)
    {
      char *dup = hw_zalloc (me, strlen (str) + 1);
      strcpy (dup, str);
      return dup;
    }
  else
    {
      return NULL;
    }
}
