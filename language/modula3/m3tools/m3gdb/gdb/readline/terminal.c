/* terminal.c -- controlling the terminal with termcap. */

/* Copyright (C) 1996 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library, a library for
   reading lines of text with interactive input and history editing.

   The GNU Readline Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   The GNU Readline Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   59 Temple Place, Suite 330, Boston, MA 02111 USA. */
#define READLINE_LIBRARY

#if defined (HAVE_CONFIG_H)
#  include <config.h>
#endif

#include <sys/types.h>
#include "posixstat.h"
#include <fcntl.h>
#if defined (HAVE_SYS_FILE_H)
#  include <sys/file.h>
#endif /* HAVE_SYS_FILE_H */

#if defined (HAVE_UNISTD_H)
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#if defined (HAVE_STDLIB_H)
#  include <stdlib.h>
#else
#  include "ansi_stdlib.h"
#endif /* HAVE_STDLIB_H */

#if defined (HAVE_LOCALE_H)
#  include <locale.h>
#endif

#include <stdio.h>

/* System-specific feature definitions and include files. */
#include "rldefs.h"

#if defined (GWINSZ_IN_SYS_IOCTL) && !defined (TIOCGWINSZ)
#  include <sys/ioctl.h>
#endif /* GWINSZ_IN_SYS_IOCTL && !TIOCGWINSZ */

#ifdef __MSDOS__
# include <pc.h>
#endif

#include "rltty.h"
#include "tcap.h"

/* Some standard library routines. */
#include "readline.h"
#include "history.h"

#include "rlprivate.h"
#include "rlshell.h"

/* **************************************************************** */
/*								    */
/*			Terminal and Termcap			    */
/*								    */
/* **************************************************************** */

#ifndef __MSDOS__
static char *term_buffer = (char *)NULL;
static char *term_string_buffer = (char *)NULL;

/* Non-zero means this terminal can't really do anything. */
static int dumb_term;
#endif /* !__MSDOS__ */

static int tcap_initialized;

#if !defined (__linux__)
#  if defined (__EMX__) || defined (NEED_EXTERN_PC)
extern 
#  endif /* __EMX__ || NEED_EXTERN_PC */
char PC, *BC, *UP;
#endif /* __linux__ */

/* Some strings to control terminal actions.  These are output by tputs (). */
char *term_goto, *term_clreol, *term_cr, *term_clrpag, *term_backspace;
char *term_pc;

/* Non-zero if we determine that the terminal can do character insertion. */
int terminal_can_insert = 0;

/* How to insert characters. */
char *term_im, *term_ei, *term_ic, *term_ip, *term_IC;

/* How to delete characters. */
char *term_dc, *term_DC;

#if defined (HACK_TERMCAP_MOTION)
char *term_forward_char;
#endif  /* HACK_TERMCAP_MOTION */

/* How to go up a line. */
char *term_up;

/* A visible bell, if the terminal can be made to flash the screen. */
static char *visible_bell;

/* Non-zero means the terminal can auto-wrap lines. */
int _rl_term_autowrap;

/* Non-zero means that this terminal has a meta key. */
static int term_has_meta;

/* The sequences to write to turn on and off the meta key, if this
   terminal    has one. */
static char *term_mm, *term_mo;

/* The key sequences output by the arrow keys, if this terminal has any. */
static char *term_ku, *term_kd, *term_kr, *term_kl;

/* How to initialize and reset the arrow keys, if this terminal has any. */
static char *term_ks, *term_ke;

/* The key sequences sent by the Home and End keys, if any. */
static char *term_kh, *term_kH;

/* Variables that hold the screen dimensions, used by the display code. */
int screenwidth, screenheight, screenchars;

/* Non-zero means the user wants to enable the keypad. */
int _rl_enable_keypad;

/* Non-zero means the user wants to enable a meta key. */
int _rl_enable_meta = 1;

#if defined (__EMX__)
static void
_emx_get_screensize (swp, shp)
     int *swp, *shp;
{
  int sz[2];

  _scrsize (sz);

  if (swp)
    *swp = sz[0];
  if (shp)
    *shp = sz[1];
}
#endif

/* Get readline's idea of the screen size.  TTY is a file descriptor open
   to the terminal.  If IGNORE_ENV is true, we do not pay attention to the
   values of $LINES and $COLUMNS.  The tests for TERM_STRING_BUFFER being
   non-null serve to check whether or not we have initialized termcap. */
void
_rl_get_screen_size (tty, ignore_env)
     int tty, ignore_env;
{
  char *ss;
#if defined (TIOCGWINSZ)
  struct winsize window_size;
#endif /* TIOCGWINSZ */

#if defined (TIOCGWINSZ)
  if (ioctl (tty, TIOCGWINSZ, &window_size) == 0)
    {
      screenwidth = (int) window_size.ws_col;
      screenheight = (int) window_size.ws_row;
    }
#endif /* TIOCGWINSZ */

#if defined (__EMX__)
  _emx_get_screensize (&screenwidth, &screenheight);
#endif

  /* Environment variable COLUMNS overrides setting of "co" if IGNORE_ENV
     is unset. */
  if (screenwidth <= 0)
    {
      if (ignore_env == 0 && (ss = get_env_value ("COLUMNS")))
	screenwidth = atoi (ss);

#if defined (__DJGPP__)
      if (screenwidth <= 0)
	screenwidth = ScreenCols ();
#else
      if (screenwidth <= 0 && term_string_buffer)
	screenwidth = tgetnum ("co");
#endif
    }

  /* Environment variable LINES overrides setting of "li" if IGNORE_ENV
     is unset. */
  if (screenheight <= 0)
    {
      if (ignore_env == 0 && (ss = get_env_value ("LINES")))
	screenheight = atoi (ss);

#if defined (__DJGPP__)
      if (screenheight <= 0)
	screenheight = ScreenRows ();
#else
      if (screenheight <= 0 && term_string_buffer)
	screenheight = tgetnum ("li");
#endif
    }

  /* If all else fails, default to 80x24 terminal. */
  if (screenwidth <= 1)
    screenwidth = 80;

  if (screenheight <= 0)
    screenheight = 24;

  /* If we're being compiled as part of bash, set the environment
     variables $LINES and $COLUMNS to new values.  Otherwise, just
     do a pair of putenv () or setenv () calls. */
  set_lines_and_columns (screenheight, screenwidth);

  if (_rl_term_autowrap == 0)
    screenwidth--;

  screenchars = screenwidth * screenheight;
}

void
_rl_set_screen_size (rows, cols)
     int rows, cols;
{
  screenheight = rows;
  screenwidth = cols;

  if (_rl_term_autowrap == 0)
    screenwidth--;

  screenchars = screenwidth * screenheight;
}

void
rl_resize_terminal ()
{
  if (readline_echoing_p)
    {
      _rl_get_screen_size (fileno (rl_instream), 1);
      _rl_redisplay_after_sigwinch ();
    }
}

struct _tc_string {
     char *tc_var;
     char **tc_value;
};

/* This should be kept sorted, just in case we decide to change the
   search algorithm to something smarter. */
static struct _tc_string tc_strings[] =
{
  { "DC", &term_DC },
  { "IC", &term_IC },
  { "ce", &term_clreol },
  { "cl", &term_clrpag },
  { "cr", &term_cr },
  { "dc", &term_dc },
  { "ei", &term_ei },
  { "ic", &term_ic },
  { "im", &term_im },
  { "kd", &term_kd },
  { "kh", &term_kh },	/* home */
  { "kH", &term_kH },	/* end */
  { "kl", &term_kl },
  { "kr", &term_kr },
  { "ku", &term_ku },
  { "ks", &term_ks },
  { "ke", &term_ke },
  { "le", &term_backspace },
  { "mm", &term_mm },
  { "mo", &term_mo },
#if defined (HACK_TERMCAP_MOTION)
  { "nd", &term_forward_char },
#endif
  { "pc", &term_pc },
  { "up", &term_up },
  { "vb", &visible_bell },
};

#define NUM_TC_STRINGS (sizeof (tc_strings) / sizeof (struct _tc_string))

/* Read the desired terminal capability strings into BP.  The capabilities
   are described in the TC_STRINGS table. */
static void
get_term_capabilities (bp)
     char **bp;
{
#if !defined (__DJGPP__)	/* XXX - doesn't DJGPP have a termcap library? */
  register int i;

  for (i = 0; i < NUM_TC_STRINGS; i++)
    *(tc_strings[i].tc_value) = tgetstr (tc_strings[i].tc_var, bp);
#endif
  tcap_initialized = 1;
}

#define CUSTOM_REDISPLAY_FUNC() (rl_redisplay_function != rl_redisplay)
#define CUSTOM_INPUT_FUNC() (rl_getc_function != rl_getc)

int
_rl_init_terminal_io (terminal_name)
     char *terminal_name;
{
  char *term, *buffer;
  int tty, tgetent_ret;
  Keymap xkeymap;

  term = terminal_name ? terminal_name : get_env_value ("TERM");
  term_clrpag = term_cr = term_clreol = (char *)NULL;
  tty = rl_instream ? fileno (rl_instream) : 0;
  screenwidth = screenheight = 0;

  if (term == 0)
    term = "dumb";

#ifdef __MSDOS__
  term_im = term_ei = term_ic = term_IC = (char *)NULL;
  term_up = term_dc = term_DC = visible_bell = (char *)NULL;
  term_ku = term_kd = term_kl = term_kr = (char *)NULL;
  term_mm = term_mo = (char *)NULL;
  terminal_can_insert = term_has_meta = _rl_term_autowrap = 0;
  term_cr = "\r";

  _rl_get_screen_size (tty, 0);
#else  /* !__MSDOS__ */
  /* I've separated this out for later work on not calling tgetent at all
     if the calling application has supplied a custom redisplay function,
     (and possibly if the application has supplied a custom input function). */
  if (CUSTOM_REDISPLAY_FUNC())
    {
      tgetent_ret = -1;
    }
  else
    {
      if (term_string_buffer == 0)
	term_string_buffer = xmalloc(2032);

      if (term_buffer == 0)
	term_buffer = xmalloc(4080);

      buffer = term_string_buffer;

      tgetent_ret = tgetent (term_buffer, term);
    }

  if (tgetent_ret <= 0)
    {
      FREE (term_string_buffer);
      FREE (term_buffer);
      buffer = term_buffer = term_string_buffer = (char *)NULL;

      dumb_term = 1;
      _rl_term_autowrap = 0;	/* used by _rl_get_screen_size */

#if defined (__EMX__)
      _emx_get_screensize (&screenwidth, &screenheight);
      screenwidth--;
#else /* !__EMX__ */
      _rl_get_screen_size (tty, 0);
#endif /* !__EMX__ */

      /* Defaults. */
      if (screenwidth <= 0 || screenheight <= 0)
        {
	  screenwidth = 79;
	  screenheight = 24;
        }

      /* Everything below here is used by the redisplay code (tputs). */
      screenchars = screenwidth * screenheight;
      term_cr = "\r";
      term_im = term_ei = term_ic = term_IC = (char *)NULL;
      term_up = term_dc = term_DC = visible_bell = (char *)NULL;
      term_ku = term_kd = term_kl = term_kr = (char *)NULL;
      term_mm = term_mo = (char *)NULL;
#if defined (HACK_TERMCAP_MOTION)
      term_forward_char = (char *)NULL;
#endif
      terminal_can_insert = term_has_meta = 0;

      /* Reasonable defaults for tgoto().  Readline currently only uses
         tgoto if term_IC or term_DC is defined, but just in case we
         change that later... */
      PC = '\0';
      BC = term_backspace = "\b";
      UP = term_up;

      return 0;
    }

  get_term_capabilities (&buffer);

  /* Set up the variables that the termcap library expects the application
     to provide. */
  PC = term_pc ? *term_pc : 0;
  BC = term_backspace;
  UP = term_up;

  if (!term_cr)
    term_cr = "\r";

  _rl_term_autowrap = tgetflag ("am") && tgetflag ("xn");

  _rl_get_screen_size (tty, 0);

  /* "An application program can assume that the terminal can do
      character insertion if *any one of* the capabilities `IC',
      `im', `ic' or `ip' is provided."  But we can't do anything if
      only `ip' is provided, so... */
  terminal_can_insert = (term_IC || term_im || term_ic);

  /* Check to see if this terminal has a meta key and clear the capability
     variables if there is none. */
  term_has_meta = (tgetflag ("km") || tgetflag ("MT"));
  if (!term_has_meta)
    term_mm = term_mo = (char *)NULL;

#endif /* !__MSDOS__ */

  /* Attempt to find and bind the arrow keys.  Do not override already
     bound keys in an overzealous attempt, however. */
  xkeymap = _rl_keymap;

  _rl_keymap = emacs_standard_keymap;
  _rl_bind_if_unbound (term_ku, rl_get_previous_history);
  _rl_bind_if_unbound (term_kd, rl_get_next_history);
  _rl_bind_if_unbound (term_kr, rl_forward);
  _rl_bind_if_unbound (term_kl, rl_backward);

  _rl_bind_if_unbound (term_kh, rl_beg_of_line);	/* Home */
  _rl_bind_if_unbound (term_kH, rl_end_of_line);	/* End */

#if defined (VI_MODE)
  _rl_keymap = vi_movement_keymap;
  _rl_bind_if_unbound (term_ku, rl_get_previous_history);
  _rl_bind_if_unbound (term_kd, rl_get_next_history);
  _rl_bind_if_unbound (term_kr, rl_forward);
  _rl_bind_if_unbound (term_kl, rl_backward);

  _rl_bind_if_unbound (term_kh, rl_beg_of_line);	/* Home */
  _rl_bind_if_unbound (term_kH, rl_end_of_line);	/* End */
#endif /* VI_MODE */

  _rl_keymap = xkeymap;

  return 0;
}

char *
rl_get_termcap (cap)
     char *cap;
{
  register int i;

  if (tcap_initialized == 0)
    return ((char *)NULL);
  for (i = 0; i < NUM_TC_STRINGS; i++)
    {
      if (tc_strings[i].tc_var[0] == cap[0] && strcmp (tc_strings[i].tc_var, cap) == 0)
        return *(tc_strings[i].tc_value);
    }
  return ((char *)NULL);
}

/* Re-initialize the terminal considering that the TERM/TERMCAP variable
   has changed. */
int
rl_reset_terminal (terminal_name)
     char *terminal_name;
{
  _rl_init_terminal_io (terminal_name);
  return 0;
}

/* A function for the use of tputs () */
#ifdef _MINIX
void
_rl_output_character_function (c)
     int c;
{
  putc (c, _rl_out_stream);
}
#else /* !_MINIX */
int
_rl_output_character_function (c)
     int c;
{
  return putc (c, _rl_out_stream);
}
#endif /* !_MINIX */

/* Write COUNT characters from STRING to the output stream. */
void
_rl_output_some_chars (string, count)
     char *string;
     int count;
{
  fwrite (string, 1, count, _rl_out_stream);
}

/* Move the cursor back. */
int
_rl_backspace (count)
     int count;
{
  register int i;

#ifndef __MSDOS__
  if (term_backspace)
    for (i = 0; i < count; i++)
      tputs (term_backspace, 1, _rl_output_character_function);
  else
#endif
    for (i = 0; i < count; i++)
      putc ('\b', _rl_out_stream);
  return 0;
}

/* Move to the start of the next line. */
int
crlf ()
{
#if defined (NEW_TTY_DRIVER)
  if (term_cr)
    tputs (term_cr, 1, _rl_output_character_function);
#endif /* NEW_TTY_DRIVER */
  putc ('\n', _rl_out_stream);
  return 0;
}

/* Ring the terminal bell. */
int
ding ()
{
  if (readline_echoing_p)
    {
      switch (_rl_bell_preference)
        {
	case NO_BELL:
	default:
	  break;
	case VISIBLE_BELL:
#ifdef __MSDOS__
	  ScreenVisualBell ();
	  break;
#else
	  if (visible_bell)
	    {
	      tputs (visible_bell, 1, _rl_output_character_function);
	      break;
	    }
#endif
	  /* FALLTHROUGH */
	case AUDIBLE_BELL:
	  fprintf (stderr, "\007");
	  fflush (stderr);
	  break;
        }
      return (0);
    }
  return (-1);
}

/* **************************************************************** */
/*								    */
/*	 	Controlling the Meta Key and Keypad		    */
/*								    */
/* **************************************************************** */

void
_rl_enable_meta_key ()
{
#if !defined (__DJGPP__)
  if (term_has_meta && term_mm)
    tputs (term_mm, 1, _rl_output_character_function);
#endif
}

void
_rl_control_keypad (on)
     int on;
{
#if !defined (__DJGPP__)
  if (on && term_ks)
    tputs (term_ks, 1, _rl_output_character_function);
  else if (!on && term_ke)
    tputs (term_ke, 1, _rl_output_character_function);
#endif
}
