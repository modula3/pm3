/* Declarations for value printing routines for GDB, the GNU debugger.
   Copyright 1986, 1988, 1989, 1991-1994, 2000 Free Software Foundation, Inc.

   This file is part of GDB.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */


extern int prettyprint_arrays;	/* Controls pretty printing of arrays.  */
extern int prettyprint_structs;	/* Controls pretty printing of structures */
extern int prettyprint_arrays;	/* Controls pretty printing of arrays.  */

extern int vtblprint;		/* Controls printing of vtbl's */
extern int unionprint;		/* Controls printing of nested unions.  */
extern int addressprint;	/* Controls pretty printing of addresses.  */
extern int objectprint;		/* Controls looking up an object's derived type
				   using what we find in its vtables.  */

extern unsigned int print_max;	/* Max # of chars for strings/vectors */

/* Print repeat counts if there are more than this many repetitions of an
   element in an array.  Referenced by the low level language dependent
   print routines. */
extern unsigned int repeat_count_threshold;

extern int output_format;

extern int stop_print_at_null;	/* Stop printing at null char? */

extern void val_print_array_elements (struct type *, char *, CORE_ADDR,
				      struct ui_file *, int, int, int,
				      enum val_prettyprint, unsigned int);

extern void val_print_type_code_int (struct type *, char *,
				     struct ui_file *);

extern void print_binary_chars (struct ui_file *, unsigned char *,
				unsigned int);

extern void print_octal_chars (struct ui_file *, unsigned char *,
			       unsigned int);

extern void print_decimal_chars (struct ui_file *, unsigned char *,
				 unsigned int);
