/* display.c -- readline redisplay facility. */

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

#include "sysdep.h"
#include <stdio.h>
#include <sys/types.h>

/* System-specific feature definitions and include files. */
#include "rldefs.h"

/* Some standard library routines. */
#include "readline.h"
#include "history.h"

#if defined (__GO32__)
#  include <sys/pc.h>
#endif /* __GO32__ */

#if !defined (strchr) && !defined (__STDC__)
extern char *strchr (), *strrchr ();
#endif /* !strchr && !__STDC__ */

/* Global and pseudo-global variables and functions
   imported from readline.c. */
extern char *rl_prompt;
extern int readline_echoing_p;
extern char *term_clreol, *term_im, *term_ic,  *term_ei, *term_DC;
/* Termcap variables. */
extern char *term_up, *term_dc, *term_cr, *term_IC;
extern int screenheight, screenwidth, terminal_can_insert, term_xn;

extern void _rl_output_some_chars ();
extern int _rl_output_character_function ();

extern int _rl_convert_meta_chars_to_ascii;
extern int _rl_horizontal_scroll_mode;
extern int _rl_mark_modified_lines;
extern int _rl_prefer_visible_bell;

/* Pseudo-global functions (local to the readline library) exported
   by this file. */
void _rl_move_cursor_relative (), _rl_output_some_chars ();
void _rl_move_vert ();

static void update_line (), clear_to_eol ();
static void delete_chars (), insert_some_chars ();

extern char *xmalloc (), *xrealloc ();

/* **************************************************************** */
/*								    */
/*			Display stuff				    */
/*								    */
/* **************************************************************** */

/* This is the stuff that is hard for me.  I never seem to write good
   display routines in C.  Let's see how I do this time. */

/* (PWP) Well... Good for a simple line updater, but totally ignores
   the problems of input lines longer than the screen width.

   update_line and the code that calls it makes a multiple line,
   automatically wrapping line update.  Carefull attention needs
   to be paid to the vertical position variables. */

/* Keep two buffers; one which reflects the current contents of the
   screen, and the other to draw what we think the new contents should
   be.  Then compare the buffers, and make whatever changes to the
   screen itself that we should.  Finally, make the buffer that we
   just drew into be the one which reflects the current contents of the
   screen, and place the cursor where it belongs.

   Commands that want to can fix the display themselves, and then let
   this function know that the display has been fixed by setting the
   RL_DISPLAY_FIXED variable.  This is good for efficiency. */

/* Global variables declared here. */
/* What YOU turn on when you have handled all redisplay yourself. */
int rl_display_fixed = 0;

/* The stuff that gets printed out before the actual text of the line.
   This is usually pointing to rl_prompt. */
char *rl_display_prompt = (char *)NULL;

/* Pseudo-global variables declared here. */
/* The visible cursor position.  If you print some text, adjust this. */
int _rl_last_c_pos = 0;
int _rl_last_v_pos = 0;

/* Number of lines currently on screen minus 1. */
int _rl_vis_botlin = 0;

/* Variables used only in this file. */
/* The last left edge of text that was displayed.  This is used when
   doing horizontal scrolling.  It shifts in thirds of a screenwidth. */
static int last_lmargin = 0;

/* The line display buffers.  One is the line currently displayed on
   the screen.  The other is the line about to be displayed. */
static char *visible_line = (char *)NULL;
static char *invisible_line = (char *)NULL;

/* A buffer for `modeline' messages. */
static char msg_buf[128];

/* Non-zero forces the redisplay even if we thought it was unnecessary. */
static int forced_display = 0;

/* Default and initial buffer size.  Can grow. */
static int line_size = 1024;

/* Basic redisplay algorithm. */
void
rl_redisplay ()
{
  register int in, out, c, linenum;
  register char *line = invisible_line;
  char *prompt_this_line;
  int c_pos = 0;
  int inv_botlin = 0;		/* Number of lines in newly drawn buffer. */

  if (!readline_echoing_p)
    return;

  if (!rl_display_prompt)
    rl_display_prompt = "";

  if (!invisible_line)
    {
      visible_line = (char *)xmalloc (line_size);
      invisible_line = (char *)xmalloc (line_size);
      line = invisible_line;
      for (in = 0; in < line_size; in++)
	{
	  visible_line[in] = 0;
	  invisible_line[in] = 1;
	}
      rl_on_new_line ();
    }

  /* Draw the line into the buffer. */
  c_pos = -1;

  /* Mark the line as modified or not.  We only do this for history
     lines. */
  out = 0;
  if (_rl_mark_modified_lines && current_history () && rl_undo_list)
    {
      line[out++] = '*';
      line[out] = '\0';
    }

  /* If someone thought that the redisplay was handled, but the currently
     visible line has a different modification state than the one about
     to become visible, then correct the caller's misconception. */
  if (visible_line[0] != invisible_line[0])
    rl_display_fixed = 0;

  prompt_this_line = strrchr (rl_display_prompt, '\n');
  if (!prompt_this_line)
    prompt_this_line = rl_display_prompt;
  else
    {
      prompt_this_line++;
      if (forced_display)
	_rl_output_some_chars
	  (rl_display_prompt, prompt_this_line - rl_display_prompt);
    }

  strncpy (line + out,  prompt_this_line, strlen (prompt_this_line));
  out += strlen (prompt_this_line);
  line[out] = '\0';

  for (in = 0; in < rl_end; in++)
    {
      c = (unsigned char)rl_line_buffer[in];

      if (out + 8 >= line_size)		/* XXX - 8 for \t */
	{
	  line_size *= 2;
	  visible_line = (char *)xrealloc (visible_line, line_size);
	  invisible_line = (char *)xrealloc (invisible_line, line_size);
	  line = invisible_line;
	}

      if (in == rl_point)
	c_pos = out;

      if (META_CHAR (c))
	{
	  if (_rl_convert_meta_chars_to_ascii)
	    {
	      sprintf (line + out, "\\%o", c);
	      out += 4;
	    }
	  else
	    line[out++] = c;	  
	}
#define DISPLAY_TABS
#if defined (DISPLAY_TABS)
      else if (c == '\t')
	{
	  register int newout = (out | (int)7) + 1;
	  while (out < newout)
	    line[out++] = ' ';
	}
#endif
      else if (c < ' ')
	{
	  line[out++] = '^';
	  line[out++] = UNCTRL (c);	/* XXX was c ^ 0x40 */
	}
      else if (c == 127)
	{
	  line[out++] = '^';
	  line[out++] = '?';
	}
      else
	line[out++] = c;
    }
  line[out] = '\0';
  if (c_pos < 0)
    c_pos = out;

  /* PWP: now is when things get a bit hairy.  The visible and invisible
     line buffers are really multiple lines, which would wrap every
     screenwidth characters.  Go through each in turn, finding
     the changed region and updating it.  The line order is top to bottom. */

  /* If we can move the cursor up and down, then use multiple lines,
     otherwise, let long lines display in a single terminal line, and
     horizontally scroll it. */

  if (!_rl_horizontal_scroll_mode && term_up && *term_up)
    {
      int total_screen_chars = (screenwidth * screenheight);

      if (!rl_display_fixed || forced_display)
	{
	  forced_display = 0;

	  /* If we have more than a screenful of material to display, then
	     only display a screenful.  We should display the last screen,
	     not the first.  I'll fix this in a minute. */
	  if (out >= total_screen_chars)
	    out = total_screen_chars - 1;

	  /* Number of screen lines to display. */
	  inv_botlin = out / screenwidth;

	  /* For each line in the buffer, do the updating display. */
	  for (linenum = 0; linenum <= inv_botlin; linenum++)
	    update_line (linenum > _rl_vis_botlin ? ""
			 : &visible_line[linenum * screenwidth],
			 &invisible_line[linenum * screenwidth],
			 linenum);

	  /* We may have deleted some lines.  If so, clear the left over
	     blank ones at the bottom out. */
	  if (_rl_vis_botlin > inv_botlin)
	    {
	      char *tt;
	      for (; linenum <= _rl_vis_botlin; linenum++)
		{
		  tt = &visible_line[linenum * screenwidth];
		  _rl_move_vert (linenum);
		  _rl_move_cursor_relative (0, tt);
		  clear_to_eol
		    ((linenum == _rl_vis_botlin) ? strlen (tt) : screenwidth);
		}
	    }
	  _rl_vis_botlin = inv_botlin;

	  /* Move the cursor where it should be. */
	  _rl_move_vert (c_pos / screenwidth);
	  _rl_move_cursor_relative (c_pos % screenwidth,
				&invisible_line[(c_pos / screenwidth) * screenwidth]);
	}
    }
  else				/* Do horizontal scrolling. */
    {
      int lmargin;

      /* Always at top line. */
      _rl_last_v_pos = 0;

      /* If the display position of the cursor would be off the edge
	 of the screen, start the display of this line at an offset that
	 leaves the cursor on the screen. */
      if (c_pos - last_lmargin > screenwidth - 2)
	lmargin = (c_pos / (screenwidth / 3) - 2) * (screenwidth / 3);
      else if (c_pos - last_lmargin < 1)
	lmargin = ((c_pos - 1) / (screenwidth / 3)) * (screenwidth / 3);
      else
	lmargin = last_lmargin;

      /* If the first character on the screen isn't the first character
	 in the display line, indicate this with a special character. */
      if (lmargin > 0)
	line[lmargin] = '<';

      if (lmargin + screenwidth < out)
	line[lmargin + screenwidth - 1] = '>';

      if (!rl_display_fixed || forced_display || lmargin != last_lmargin)
	{
	  forced_display = 0;
	  update_line (&visible_line[last_lmargin],
		       &invisible_line[lmargin], 0);

	  _rl_move_cursor_relative (c_pos - lmargin, &invisible_line[lmargin]);
	  last_lmargin = lmargin;
	}
    }
  fflush (rl_outstream);

  /* Swap visible and non-visible lines. */
  {
    char *temp = visible_line;
    visible_line = invisible_line;
    invisible_line = temp;
    rl_display_fixed = 0;
  }
}

/* PWP: update_line() is based on finding the middle difference of each
   line on the screen; vis:

			     /old first difference
	/beginning of line   |              /old last same       /old EOL
	v		     v              v                    v
old:	eddie> Oh, my little gruntle-buggy is to me, as lurgid as
new:	eddie> Oh, my little buggy says to me, as lurgid as
	^		     ^        ^			   ^
	\beginning of line   |        \new last same	   \new end of line
			     \new first difference

   All are character pointers for the sake of speed.  Special cases for
   no differences, as well as for end of line additions must be handeled.

   Could be made even smarter, but this works well enough */
static void
update_line (old, new, current_line)
     register char *old, *new;
     int current_line;
{
  register char *ofd, *ols, *oe, *nfd, *nls, *ne;
  int lendiff, wsatend;

  if (_rl_last_c_pos == screenwidth && term_xn && new[0])
    {
      putc (new[0], rl_outstream);
      _rl_last_c_pos = 1;
      _rl_last_v_pos++;
      if (old[0])
	old[0] = new[0];
    }

  /* Find first difference. */
  for (ofd = old, nfd = new;
       (ofd - old < screenwidth) && *ofd && (*ofd == *nfd);
       ofd++, nfd++)
    ;

  /* Move to the end of the screen line. */
  for (oe = ofd; ((oe - old) < screenwidth) && *oe; oe++);
  for (ne = nfd; ((ne - new) < screenwidth) && *ne; ne++);

  /* If no difference, continue to next line. */
  if (ofd == oe && nfd == ne)
    return;

  wsatend = 1;			/* flag for trailing whitespace */
  ols = oe - 1;			/* find last same */
  nls = ne - 1;
  while ((ols > ofd) && (nls > nfd) && (*ols == *nls))
    {
      if (*ols != ' ')
	wsatend = 0;
      ols--;
      nls--;
    }

  if (wsatend)
    {
      ols = oe;
      nls = ne;
    }
  else if (*ols != *nls)
    {
      if (*ols)			/* don't step past the NUL */
	ols++;
      if (*nls)
	nls++;
    }

  _rl_move_vert (current_line);
  _rl_move_cursor_relative (ofd - old, old);

  /* if (len (new) > len (old)) */
  lendiff = (nls - nfd) - (ols - ofd);

  /* Insert (diff (len (old), len (new)) ch. */
  if (lendiff > 0)
    {
      if (terminal_can_insert)
	{
	  /* Sometimes it is cheaper to print the characters rather than
	     use the terminal's capabilities. */
	  if ((2 * (ne - nfd)) < lendiff && !term_IC)
	    {
	      _rl_output_some_chars (nfd, (ne - nfd));
	      _rl_last_c_pos += (ne - nfd);
	    }
	  else
	    {
	      if (*ols)
		{
		  insert_some_chars (nfd, lendiff);
		  _rl_last_c_pos += lendiff;
		}
	      else
		{
		  /* At the end of a line the characters do not have to
		     be "inserted".  They can just be placed on the screen. */
		  _rl_output_some_chars (nfd, lendiff);
		  _rl_last_c_pos += lendiff;
		}
	      /* Copy (new) chars to screen from first diff to last match. */
	      if (((nls - nfd) - lendiff) > 0)
		{
		  _rl_output_some_chars (&nfd[lendiff], ((nls - nfd) - lendiff));
		  _rl_last_c_pos += ((nls - nfd) - lendiff);
		}
	    }
	}
      else
	{		/* cannot insert chars, write to EOL */
	  _rl_output_some_chars (nfd, (ne - nfd));
	  _rl_last_c_pos += (ne - nfd);
	}
    }
  else				/* Delete characters from line. */
    {
      /* If possible and inexpensive to use terminal deletion, then do so. */
      if (term_dc && (2 * (ne - nfd)) >= (-lendiff))
	{
	  if (lendiff)
	    delete_chars (-lendiff); /* delete (diff) characters */

	  /* Copy (new) chars to screen from first diff to last match */
	  if ((nls - nfd) > 0)
	    {
	      _rl_output_some_chars (nfd, (nls - nfd));
	      _rl_last_c_pos += (nls - nfd);
	    }
	}
      /* Otherwise, print over the existing material. */
      else
	{
	  _rl_output_some_chars (nfd, (ne - nfd));
	  _rl_last_c_pos += (ne - nfd);
	  clear_to_eol ((oe - old) - (ne - new));
	}
    }
}

/* Tell the update routines that we have moved onto a new (empty) line. */
rl_on_new_line ()
{
  if (visible_line)
    visible_line[0] = '\0';

  _rl_last_c_pos = _rl_last_v_pos = 0;
  _rl_vis_botlin = last_lmargin = 0;
}

/* Actually update the display, period. */
rl_forced_update_display ()
{
  if (visible_line)
    {
      register char *temp = visible_line;

      while (*temp) *temp++ = '\0';
    }
  rl_on_new_line ();
  forced_display++;
  rl_redisplay ();
}

/* Move the cursor from _rl_last_c_pos to NEW, which are buffer indices.
   DATA is the contents of the screen line of interest; i.e., where
   the movement is being done. */
void
_rl_move_cursor_relative (new, data)
     int new;
     char *data;
{
  register int i;

  /* It may be faster to output a CR, and then move forwards instead
     of moving backwards. */
  if (new + 1 < _rl_last_c_pos - new)
    {
#if defined(__MSDOS__) || defined(_WIN32)
      putc('\r', rl_outstream);
#else
      tputs (term_cr, 1, _rl_output_character_function);
#endif
      _rl_last_c_pos = 0;
    }

  if (_rl_last_c_pos == new) return;

  if (_rl_last_c_pos < new)
    {
      /* Move the cursor forward.  We do it by printing the command
	 to move the cursor forward if there is one, else print that
	 portion of the output buffer again.  Which is cheaper? */

      /* The above comment is left here for posterity.  It is faster
	 to print one character (non-control) than to print a control
	 sequence telling the terminal to move forward one character.
	 That kind of control is for people who don't know what the
	 data is underneath the cursor. */
#if defined (HACK_TERMCAP_MOTION)
      extern char *term_forward_char;

      if (term_forward_char)
	for (i = _rl_last_c_pos; i < new; i++)
	  tputs (term_forward_char, 1, _rl_output_character_function);
      else
	for (i = _rl_last_c_pos; i < new; i++)
	  putc (data[i], rl_outstream);
#else
      for (i = _rl_last_c_pos; i < new; i++)
	putc (data[i], rl_outstream);
#endif				/* HACK_TERMCAP_MOTION */
    }
  else
    backspace (_rl_last_c_pos - new);
  _rl_last_c_pos = new;
}

/* PWP: move the cursor up or down. */
void
_rl_move_vert (to)
     int to;
{
  register int delta, i;

  if (_rl_last_v_pos == to || to > screenheight)
    return;

#if defined (MINIMAL)
  {
    int row, col;

    ScreenGetCursor (&row, &col);
    ScreenSetCursor ((row + to - _rl_last_v_pos), col);
  }
#else /* !MINIMAL */

  if ((delta = to - _rl_last_v_pos) > 0)
    {
      for (i = 0; i < delta; i++)
	putc ('\n', rl_outstream);
      tputs (term_cr, 1, _rl_output_character_function);
      _rl_last_c_pos = 0;
    }
  else
    {			/* delta < 0 */
      if (term_up && *term_up)
	for (i = 0; i < -delta; i++)
	  tputs (term_up, 1, _rl_output_character_function);
    }
#endif /* !MINIMAL */
  _rl_last_v_pos = to;		/* Now TO is here */
}

/* Physically print C on rl_outstream.  This is for functions which know
   how to optimize the display. */
rl_show_char (c)
     int c;
{
  if (META_CHAR (c) && _rl_convert_meta_chars_to_ascii)
    {
      fprintf (rl_outstream, "M-");
      c = UNMETA (c);
    }

#if defined (DISPLAY_TABS)
  if (c < 32 && c != '\t')
#else
  if (c < 32)
#endif /* !DISPLAY_TABS */
    {
      c += 64;
    }

  putc (c, rl_outstream);
  fflush (rl_outstream);
}

int
rl_character_len (c, pos)
     register int c, pos;
{
  if (META_CHAR (c))
    return (_rl_convert_meta_chars_to_ascii ? 4 : 1);

  if (c == '\t')
    {
#if defined (DISPLAY_TABS)
      return (((pos | (int)7) + 1) - pos);
#else
      return (2);
#endif /* !DISPLAY_TABS */
    }

  if (isprint (c))
    return (1);
  else
    return (2);
}

/* How to print things in the "echo-area".  The prompt is treated as a
   mini-modeline. */

#if defined (HAVE_VARARGS_H)
rl_message (va_alist)
     va_dcl
{
  char *format;
  va_list args;

  va_start (args);
  format = va_arg (args, char *);
  vsprintf (msg_buf, format, args);
  va_end (args);

  rl_display_prompt = msg_buf;
  rl_redisplay ();
}
#else /* !HAVE_VARARGS_H */
rl_message (format, arg1, arg2)
     char *format;
{
  sprintf (msg_buf, format, arg1, arg2);
  rl_display_prompt = msg_buf;
  rl_redisplay ();
}
#endif /* !HAVE_VARARGS_H */

/* How to clear things from the "echo-area". */
rl_clear_message ()
{
  rl_display_prompt = rl_prompt;
  rl_redisplay ();
}

rl_reset_line_state ()
{
  rl_on_new_line ();

  rl_display_prompt = rl_prompt ? rl_prompt : "";
  forced_display = 1;
}

/* Quick redisplay hack when erasing characters at the end of the line. */
void
_rl_erase_at_end_of_line (l)
     int l;
{
  register int i;

  backspace (l);
  for (i = 0; i < l; i++)
    putc (' ', rl_outstream);
  backspace (l);
  for (i = 0; i < l; i++)
    visible_line[--_rl_last_c_pos] = '\0';
  rl_display_fixed++;
}

/* Clear to the end of the line.  COUNT is the minimum
   number of character spaces to clear, */
static void
clear_to_eol (count)
     int count;
{
#if !defined (MINIMAL)
  if (term_clreol)
    {
      tputs (term_clreol, 1, _rl_output_character_function);
    }
  else
#endif /* !__GO32__ */
    {
      register int i;

      /* Do one more character space. */
      count++;

      for (i = 0; i < count; i++)
	putc (' ', rl_outstream);

      backspace (count);
    }
}

/* Insert COUNT characters from STRING to the output stream. */
static void
insert_some_chars (string, count)
     char *string;
     int count;
{
#if defined(_WIN32)
#else
#if defined (__GO32__)
  int row, col, width;
  short *row_start;
  ScreenGetCursor (&row, &col);
  width = ScreenCols ();
  row_start = ScreenPrimary + (row * width);

  memcpy (row_start + col + count, row_start + col, width - col - count);

  /* Place the text on the screen. */
  _rl_output_some_chars (string, count);
#else /* !__GO32__ */

  /* If IC is defined, then we do not have to "enter" insert mode. */
  if (term_IC)
    {
      char *tgoto (), *buffer;
      buffer = tgoto (term_IC, 0, count);
      tputs (buffer, 1, _rl_output_character_function);
      _rl_output_some_chars (string, count);
    }
  else
    {
      register int i;

      /* If we have to turn on insert-mode, then do so. */
      if (term_im && *term_im)
	tputs (term_im, 1, _rl_output_character_function);

      /* If there is a special command for inserting characters, then
	 use that first to open up the space. */
      if (term_ic && *term_ic)
	{
	  for (i = count; i--; )
	    tputs (term_ic, 1, _rl_output_character_function);
	}

      /* Print the text. */
      _rl_output_some_chars (string, count);

      /* If there is a string to turn off insert mode, we had best use
	 it now. */
      if (term_ei && *term_ei)
	tputs (term_ei, 1, _rl_output_character_function);
    }
#endif /* !__GO32__ */
#endif
}

/* Delete COUNT characters from the display line. */
static void
delete_chars (count)
     int count;
{
#if defined(_WIN32)

#else
#if defined (__GO32__)
  int row, col, width;
  short *row_start;

  ScreenGetCursor (&row, &col);
  width = ScreenCols ();
  row_start = ScreenPrimary + (row * width);

  memcpy (row_start + col, row_start + col + count, width - col - count);
  memset (row_start + width - count, 0, count * 2);
#else /* !__GO32__ */

  if (count > screenwidth)
    return;

  if (term_DC && *term_DC)
    {
      char *tgoto (), *buffer;
      buffer = tgoto (term_DC, count, count);
      tputs (buffer, count, _rl_output_character_function);
    }
  else
    {
      if (term_dc && *term_dc)
	while (count--)
	  tputs (term_dc, 1, _rl_output_character_function);
    }
#endif /* !__GO32__ */
#endif
}
