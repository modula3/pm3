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


#ifndef SIM_IO_H
#define SIM_IO_H

/* See the file include/callbacks.h for a description */

int sim_io_init (SIM_DESC sd);

int sim_io_shutdown (SIM_DESC sd);

int sim_io_unlink (SIM_DESC sd, const char *);

long sim_io_time (SIM_DESC sd, long *);

int sim_io_system (SIM_DESC sd, const char *);

int sim_io_rename (SIM_DESC sd, const char *, const char *);

int sim_io_write_stdout (SIM_DESC sd, const char *, int);

void sim_io_flush_stdout (SIM_DESC sd);

int sim_io_write_stderr (SIM_DESC sd, const char *, int);

void sim_io_flush_stderr (SIM_DESC sd);

int sim_io_write (SIM_DESC sd, int, const char *, int);

int sim_io_read_stdin (SIM_DESC sd, char *, int);

int sim_io_read (SIM_DESC sd, int, char *, int);

int sim_io_open (SIM_DESC sd, const char *, int);

int sim_io_lseek (SIM_DESC sd, int, long, int);

int sim_io_isatty (SIM_DESC sd, int);

int sim_io_get_errno (SIM_DESC sd);

int sim_io_close (SIM_DESC sd, int);

void sim_io_printf (SIM_DESC sd,
		    const char *fmt,
		    ...) __attribute__ ((format (printf, 2, 3)));

void sim_io_vprintf (SIM_DESC sd, const char *fmt, va_list ap);

void sim_io_eprintf (SIM_DESC sd,
		     const char *fmt,
		     ...) __attribute__ ((format (printf, 2, 3)));

void sim_io_evprintf (SIM_DESC sd, const char *fmt, va_list ap);

void sim_io_error (SIM_DESC sd,
		   const char *fmt,
		   ...) __attribute__ ((format (printf, 2, 3)));

void sim_io_poll_quit (SIM_DESC sd);

/* Returns -1 and sets (host) EAGAIN if not ready. */
int sim_io_poll_read (SIM_DESC sd, int, char *, int);

#endif
