/* parens.c -- Implemenation of matching parenthesis feature. */

/* Copyright (C) 1987, 1989, 1992 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library, a library for
   reading lines of text with interactive input and history editing.

   The GNU Readline Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 1, or
   (at your option) any later version.

   The GNU Readline Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <sys/types.h>

#if defined(__GO32__) || defined(_WIN32)
#undef FD_SET
#endif

#if defined (FD_SET)
#include <sys/time.h>
#endif
#include "readline.h"

/* Non-zero means try to blink the matching open parenthesis when the
   close parenthesis is inserted. */
#if defined (FD_SET)
int rl_blink_matching_paren = 1;
#else /* !FD_SET */
int rl_blink_matching_paren = 0;
#endif /* !FD_SET */

static int find_matching_open ();

rl_insert_close (count, invoking_key)
     int count, invoking_key;
{
  extern int rl_explicit_arg;

  if (rl_explicit_arg || !rl_blink_matching_paren)
    rl_insert (count, invoking_key);
  else
    {
#if defined (FD_SET) && !defined(__GO32__)
      int orig_point, match_point, ready;
      struct timeval timer;
      fd_set readfds;

      rl_insert (1, invoking_key);
      rl_redisplay ();
      match_point =
	find_matching_open (rl_line_buffer, rl_point - 2, invoking_key);

      /* Emacs might message or ring the bell here, but I don't. */
      if (match_point < 0)
	return;

      FD_ZERO (&readfds);
      FD_SET (fileno (rl_instream), &readfds);
      timer.tv_sec = 1;
      timer.tv_usec = 500;

      orig_point = rl_point;
      rl_point = match_point;
      rl_redisplay ();
      ready = select (1, &readfds, (fd_set *)NULL, (fd_set *)NULL, &timer);
      rl_point = orig_point;
#else /* !FD_SET */
      rl_insert (count, invoking_key);
#endif /* !FD_SET */
    }
}

static int
find_matching_open (string, from, closer)
     char *string;
     int from, closer;
{
  register int i;
  int opener, level, delimiter;

  switch (closer)
    {
    case ']': opener = '['; break;
    case '}': opener = '{'; break;
    case ')': opener = '('; break;
    default:
      return (-1);
    }

  level = 1;			/* The closer passed in counts as 1. */
  delimiter = 0;		/* Delimited state unknown. */

  for (i = from; i > -1; i--)
    {
      if (delimiter && (string[i] == delimiter))
	delimiter = 0;
      else if ((string[i] == '\'') || (string[i] == '"'))
	delimiter = rl_line_buffer[i];
      else if (!delimiter && (string[i] == closer))
	level++;
      else if (!delimiter && (string[i] == opener))
	level--;

      if (!level)
	break;
    }
  return (i);
}


      
