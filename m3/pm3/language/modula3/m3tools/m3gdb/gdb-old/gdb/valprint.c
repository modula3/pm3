/* Print values for GDB, the GNU debugger.
   Copyright 1986, 1988, 1989, 1991, 1992, 1993, 1994
             Free Software Foundation, Inc.

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "gdb_string.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "value.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "target.h"
#include "obstack.h"
#include "language.h"
#include "demangle.h"
#include "annotate.h"
#include "valprint.h"

#include <errno.h>

/* Prototypes for local functions */

static void
print_hex_chars PARAMS ((GDB_FILE *, unsigned char *, unsigned int));

static void
show_print PARAMS ((char *, int));

static void
set_print PARAMS ((char *, int));

static void
set_radix PARAMS ((char *, int));

static void
show_radix PARAMS ((char *, int));

static void
set_input_radix PARAMS ((char *, int, struct cmd_list_element *));

static void
set_input_radix_1 PARAMS ((int, unsigned));

static void
set_output_radix PARAMS ((char *, int, struct cmd_list_element *));

static void
set_output_radix_1 PARAMS ((int, unsigned));

/* Maximum number of chars to print for a string pointer value or vector
   contents, or UINT_MAX for no limit.  Note that "set print elements 0"
   stores UINT_MAX in print_max, which displays in a show command as
   "unlimited". */

unsigned int print_max;
#define PRINT_MAX_DEFAULT 200	/* Start print_max off at this value. */

/* Default input and output radixes, and output format letter.  */

unsigned input_radix = 10;
unsigned output_radix = 10;
int output_format = 0;

/* Print repeat counts if there are more than this many repetitions of an
   element in an array.  Referenced by the low level language dependent
   print routines. */

unsigned int repeat_count_threshold = 10;

/* If nonzero, stops printing of char arrays at first null. */

int stop_print_at_null;

/* Controls pretty printing of structures. */

int prettyprint_structs;

/* Controls pretty printing of arrays.  */

int prettyprint_arrays;

/* If nonzero, causes unions inside structures or other unions to be
   printed. */

int unionprint;			/* Controls printing of nested unions.  */

/* If nonzero, causes machine addresses to be printed in certain contexts. */

int addressprint;		/* Controls printing of machine addresses */


/* Print data of type TYPE located at VALADDR (within GDB), which came from
   the inferior at address ADDRESS, onto stdio stream STREAM according to
   FORMAT (a letter, or 0 for natural format using TYPE).

   If DEREF_REF is nonzero, then dereference references, otherwise just print
   them like pointers.

   The PRETTY parameter controls prettyprinting.

   If the data are a string pointer, returns the number of string characters
   printed.

   FIXME:  The data at VALADDR is in target byte order.  If gdb is ever
   enhanced to be able to debug more than the single target it was compiled
   for (specific CPU type and thus specific target byte ordering), then
   either the print routines are going to have to take this into account,
   or the data is going to have to be passed into here already converted
   to the host byte ordering, whichever is more convenient. */


int
val_print (type, valaddr, address, stream, format, deref_ref, recurse, pretty)
     struct type *type;
     char *valaddr;
     CORE_ADDR address;
     GDB_FILE *stream;
     int format;
     int deref_ref;
     int recurse;
     enum val_prettyprint pretty;
{
  struct type *real_type = check_typedef (type);
  if (pretty == Val_pretty_default)
    {
      pretty = prettyprint_structs ? Val_prettyprint : Val_no_prettyprint;
    }
  
  QUIT;

  /* Ensure that the type is complete and not just a stub.  If the type is
     only a stub and we can't find and substitute its complete type, then
     print appropriate string and return.  */

  if (TYPE_FLAGS (real_type) & TYPE_FLAG_STUB)
    {
      fprintf_filtered (stream, "<incomplete type>");
      gdb_flush (stream);
      return (0);
    }
  
  return (LA_VAL_PRINT (type, valaddr, address, stream, format, deref_ref,
			recurse, pretty));
}

/* Print the value VAL in C-ish syntax on stream STREAM.
   FORMAT is a format-letter, or 0 for print in natural format of data type.
   If the object printed is a string pointer, returns
   the number of string bytes printed.  */

int
value_print (val, stream, format, pretty)
     value_ptr val;
     GDB_FILE *stream;
     int format;
     enum val_prettyprint pretty;
{
  if (val == 0)
    {
      printf_filtered ("<address of value unknown>");
      return 0;
    }
  if (VALUE_OPTIMIZED_OUT (val))
    {
      printf_filtered ("<value optimized out>");
      return 0;
    }
  return LA_VALUE_PRINT (val, stream, format, pretty);
}

/* Called by various <lang>_val_print routines to print
   TYPE_CODE_INT's.  TYPE is the type.  VALADDR is the address of the
   value.  STREAM is where to print the value.  */

void
val_print_type_code_int (type, valaddr, stream)
     struct type *type;
     char *valaddr;
     GDB_FILE *stream;
{
  if (TYPE_LENGTH (type) > sizeof (LONGEST))
    {
      LONGEST val;

      if (TYPE_UNSIGNED (type)
	  && extract_long_unsigned_integer (valaddr, TYPE_LENGTH (type),
					    &val))
	{
	  print_longest (stream, 'u', 0, val);
	}
      else
	{
	  /* Signed, or we couldn't turn an unsigned value into a
	     LONGEST.  For signed values, one could assume two's
	     complement (a reasonable assumption, I think) and do
	     better than this.  */
	  print_hex_chars (stream, (unsigned char *) valaddr,
			   TYPE_LENGTH (type));
	}
    }
  else
    {
#ifdef PRINT_TYPELESS_INTEGER
      PRINT_TYPELESS_INTEGER (stream, type, unpack_long (type, valaddr));
#else
      print_longest (stream, TYPE_UNSIGNED (type) ? 'u' : 'd', 0,
		     unpack_long (type, valaddr));
#endif
    }
}

/* Print a number according to FORMAT which is one of d,u,x,o,b,h,w,g.
   The raison d'etre of this function is to consolidate printing of 
   LONG_LONG's into this one function.  Some platforms have long longs but
   don't have a printf() that supports "ll" in the format string.  We handle
   these by seeing if the number is representable as either a signed or
   unsigned long, depending upon what format is desired, and if not we just
   bail out and print the number in hex.

   The format chars b,h,w,g are from print_scalar_formatted().  If USE_LOCAL,
   format it according to the current language (this should be used for most
   integers which GDB prints, the exception is things like protocols where
   the format of the integer is a protocol thing, not a user-visible thing).
   */

void
print_longest (stream, format, use_local, val_long)
     GDB_FILE *stream;
     int format;
     int use_local;
     LONGEST val_long;
{
#if defined (CC_HAS_LONG_LONG) && !defined (PRINTF_HAS_LONG_LONG)
  if (sizeof (long) < sizeof (LONGEST))
    {
      int punt = 0;
      switch (format)
	{
	case 'd':
	case 'b':
	case 'h':
	case 'w':
	case 'g':
	  /* Print as signed value, must fit completely in signed long */
	  {
	    long temp = val_long;
	    if (temp != val_long)
	      punt++;
	  }
	  break;
	case 'u':
	case 'x':
	case 'o':
	  /* Print as unsigned value, must fit completely in unsigned long */
	  {
	    unsigned long temp = val_long;
	    if (temp != val_long)
	      punt++;
	  }
	  break;
	}
      if (punt)
	{
	  /* Urk, can't represent value in long so print in hex.
	     Do shift in two operations so that if sizeof (long) == sizeof (LONGEST)
	     we can avoid warnings from picky compilers about shifts >= the size of
	     the shiftee in bits */
	  unsigned long vbot = (unsigned long) val_long;
	  unsigned long vtop = (val_long >> (sizeof (long) * HOST_CHAR_BIT - 1));
	  vtop >>= 1;
	  fprintf_filtered (stream, "0x%lx%08lx", vtop, vbot);
	  return;
	}
      }
#endif

#ifdef PRINTF_HAS_LONG_LONG
  switch (format)
    {
    case 'c':
      fprintf_filtered (stream, "%c", (char)val_long);
      break;
    case 'd':
      fprintf_filtered (stream,
			use_local ? local_decimal_format_custom ("ll")
				  : "%lld",
			val_long);
      break;
    case 'u':
      fprintf_filtered (stream, "%llu", val_long);
      break;
    case 'x':
      fprintf_filtered (stream,
			use_local ? local_hex_format_custom ("ll")
				  : "%llx",
			val_long);
      break;
    case 'o':
      fprintf_filtered (stream,
			use_local ? local_octal_format_custom ("ll")
				  : "%llo",
			val_long);
      break;
    case 'b':
      fprintf_filtered (stream, local_hex_format_custom ("02ll"), val_long);
      break;
    case 'h':
      fprintf_filtered (stream, local_hex_format_custom ("04ll"), val_long);
      break;
    case 'w':
      fprintf_filtered (stream, local_hex_format_custom ("08ll"), val_long);
      break;
    case 'g':
      fprintf_filtered (stream, local_hex_format_custom ("016ll"), val_long);
      break;
    default:
      abort ();
    }
#else /* !PRINTF_HAS_LONG_LONG */
  /* In the following it is important to coerce (val_long) to a long. It does
     nothing if !LONG_LONG, but it will chop off the top half (which we know
     we can ignore) if the host supports long longs.  */

  switch (format)
    {
    case 'd':
      fprintf_filtered (stream,
			use_local ? local_decimal_format_custom ("l")
				  : "%ld",
			(long) val_long);
      break;
    case 'u':
      fprintf_filtered (stream, "%lu", (unsigned long) val_long);
      break;
    case 'x':
      fprintf_filtered (stream,
			use_local ? local_hex_format_custom ("l")
				  : "%lx",
			(long) val_long);
      break;
    case 'o':
      fprintf_filtered (stream,
			use_local ? local_octal_format_custom ("l")
				  : "%lo",
			(long) val_long);
      break;
    case 'b':
      fprintf_filtered (stream, local_hex_format_custom ("02l"),
			(long) val_long);
      break;
    case 'h':
      fprintf_filtered (stream, local_hex_format_custom ("04l"),
			(long) val_long);
      break;
    case 'w':
      fprintf_filtered (stream, local_hex_format_custom ("08l"),
			(long) val_long);
      break;
    case 'g':
      fprintf_filtered (stream, local_hex_format_custom ("016l"),
			(long) val_long);
      break;
    default:
      abort ();
    }
#endif /* !PRINTF_HAS_LONG_LONG */
}

/* This used to be a macro, but I don't think it is called often enough
   to merit such treatment.  */
/* Convert a LONGEST to an int.  This is used in contexts (e.g. number of
   arguments to a function, number in a value history, register number, etc.)
   where the value must not be larger than can fit in an int.  */

int
longest_to_int (arg)
     LONGEST arg;
{
  /* Let the compiler do the work */
  int rtnval = (int) arg;

  /* Check for overflows or underflows */
  if (sizeof (LONGEST) > sizeof (int))
    {
      if (rtnval != arg)
	{
	  error ("Value out of range.");
	}
    }
  return (rtnval);
}

/* Print a floating point value of type TYPE, pointed to in GDB by VALADDR,
   on STREAM.  */

void
print_floating (valaddr, type, stream)
     char *valaddr;
     struct type *type;
     GDB_FILE *stream;
{
  DOUBLEST doub;
  int inv;
  unsigned len = TYPE_LENGTH (type);
  
#if defined (IEEE_FLOAT)

  /* Check for NaN's.  Note that this code does not depend on us being
     on an IEEE conforming system.  It only depends on the target
     machine using IEEE representation.  This means (a)
     cross-debugging works right, and (2) IEEE_FLOAT can (and should)
     be defined for systems like the 68881, which uses IEEE
     representation, but is not IEEE conforming.  */

  {
    unsigned long low, high;
    /* Is the sign bit 0?  */
    int nonnegative;
    /* Is it is a NaN (i.e. the exponent is all ones and
       the fraction is nonzero)?  */
    int is_nan;

    if (len == 4)
      {
	/* It's single precision.  */
	/* Assume that floating point byte order is the same as
	   integer byte order.  */
	low = extract_unsigned_integer (valaddr, 4);
	nonnegative = ((low & 0x80000000) == 0);
	is_nan = ((((low >> 23) & 0xFF) == 0xFF) 
		  && 0 != (low & 0x7FFFFF));
	low &= 0x7fffff;
	high = 0;
      }
    else if (len == 8)
      {
	/* It's double precision.  Get the high and low words.  */

	/* Assume that floating point byte order is the same as
	   integer byte order.  */
	if (TARGET_BYTE_ORDER == BIG_ENDIAN)
	  {
	    low = extract_unsigned_integer (valaddr + 4, 4);
	    high = extract_unsigned_integer (valaddr, 4);
	  }
	else
	  {
	    low = extract_unsigned_integer (valaddr, 4);
	    high = extract_unsigned_integer (valaddr + 4, 4);
	  }
	nonnegative = ((high & 0x80000000) == 0);
	is_nan = (((high >> 20) & 0x7ff) == 0x7ff
		  && ! ((((high & 0xfffff) == 0)) && (low == 0)));
	high &= 0xfffff;
      }
    else
      /* Extended.  We can't detect NaNs for extendeds yet.  Also note
	 that currently extendeds get nuked to double in
	 REGISTER_CONVERTIBLE.  */
      is_nan = 0;

    if (is_nan)
      {
	/* The meaning of the sign and fraction is not defined by IEEE.
	   But the user might know what they mean.  For example, they
	   (in an implementation-defined manner) distinguish between
	   signaling and quiet NaN's.  */
	if (high)
	  fprintf_filtered (stream, "-NaN(0x%lx%.8lx)" + nonnegative,
			    high, low);
	else
	  fprintf_filtered (stream, "-NaN(0x%lx)" + nonnegative, low);
	return;
      }
  }
#endif /* IEEE_FLOAT.  */

  doub = unpack_double (type, valaddr, &inv);
  if (inv)
    {
      fprintf_filtered (stream, "<invalid float value>");
      return;
    }

  if (len < sizeof (double))
    fprintf_filtered (stream, "%.9g", (double) doub);
  else if (len == sizeof (double))
    fprintf_filtered (stream, "%.17g", (double) doub);
  else
#ifdef PRINTF_HAS_LONG_DOUBLE
    fprintf_filtered (stream, "%.35Lg", doub);
#else
    /* This at least wins with values that are representable as doubles */
    fprintf_filtered (stream, "%.17g", (double) doub);
#endif
}

/* VALADDR points to an integer of LEN bytes.  Print it in hex on stream.  */

static void
print_hex_chars (stream, valaddr, len)
     GDB_FILE *stream;
     unsigned char *valaddr;
     unsigned len;
{
  unsigned char *p;

  /* FIXME: We should be not printing leading zeroes in most cases.  */

  fprintf_filtered (stream, local_hex_format_prefix ());
  if (TARGET_BYTE_ORDER == BIG_ENDIAN)
    {
      for (p = valaddr;
	   p < valaddr + len;
	   p++)
	{
	  fprintf_filtered (stream, "%02x", *p);
	}
    }
  else
    {
      for (p = valaddr + len - 1;
	   p >= valaddr;
	   p--)
	{
	  fprintf_filtered (stream, "%02x", *p);
	}
    }
  fprintf_filtered (stream, local_hex_format_suffix ());
}

/*  Called by various <lang>_val_print routines to print elements of an
    array in the form "<elem1>, <elem2>, <elem3>, ...".

    (FIXME?)  Assumes array element separator is a comma, which is correct
    for all languages currently handled.
    (FIXME?)  Some languages have a notation for repeated array elements,
    perhaps we should try to use that notation when appropriate.
    */

void
val_print_array_elements (type, valaddr, address, stream, format, deref_ref,
			  recurse, pretty, i)
     struct type *type;
     char *valaddr;
     CORE_ADDR address;
     GDB_FILE *stream;
     int format;
     int deref_ref;
     int recurse;
     enum val_prettyprint pretty;
     unsigned int i;
{
  unsigned int things_printed = 0;
  unsigned len;
  struct type *elttype;
  unsigned eltlen;
  /* Position of the array element we are examining to see
     whether it is repeated.  */
  unsigned int rep1;
  /* Number of repetitions we have detected so far.  */
  unsigned int reps;
      
  elttype = TYPE_TARGET_TYPE (type);
  eltlen = TYPE_LENGTH (check_typedef (elttype));
  len = TYPE_LENGTH (type) / eltlen;

  annotate_array_section_begin (i, elttype);

  for (; i < len && things_printed < print_max; i++)
    {
      if (i != 0)
	{
	  if (prettyprint_arrays)
	    {
	      fprintf_filtered (stream, ",\n");
	      print_spaces_filtered (2 + 2 * recurse, stream);
	    }
	  else
	    {
	      fprintf_filtered (stream, ", ");
	    }
	}
      wrap_here (n_spaces (2 + 2 * recurse));

      rep1 = i + 1;
      reps = 1;
      while ((rep1 < len) && 
	     !memcmp (valaddr + i * eltlen, valaddr + rep1 * eltlen, eltlen))
	{
	  ++reps;
	  ++rep1;
	}

      if (reps > repeat_count_threshold)
	{
	  val_print (elttype, valaddr + i * eltlen, 0, stream, format,
		     deref_ref, recurse + 1, pretty);
	  annotate_elt_rep (reps);
	  fprintf_filtered (stream, " <repeats %u times>", reps);
	  annotate_elt_rep_end ();

	  i = rep1 - 1;
	  things_printed += repeat_count_threshold;
	}
      else
	{
	  val_print (elttype, valaddr + i * eltlen, 0, stream, format,
		     deref_ref, recurse + 1, pretty);
	  annotate_elt ();
	  things_printed++;
	}
    }
  annotate_array_section_end ();
  if (i < len)
    {
      fprintf_filtered (stream, "...");
    }
}

/*  Print a string from the inferior, starting at ADDR and printing up to LEN
    characters, to STREAM.  If LEN is zero, printing stops at the first null
    byte, otherwise printing proceeds (including null bytes) until either
    print_max or LEN characters have been printed, whichever is smaller. */

/* FIXME: All callers supply LEN of zero.  Supplying a non-zero LEN is
   pointless, this routine just then becomes a convoluted version of
   target_read_memory_partial.  Removing all the LEN stuff would simplify
   this routine enormously.

   FIXME: Use target_read_string.  */

int
val_print_string (addr, len, stream)
    CORE_ADDR addr;
    unsigned int len;
    GDB_FILE *stream;
{
  int force_ellipsis = 0;	/* Force ellipsis to be printed if nonzero. */
  int errcode;			/* Errno returned from bad reads. */
  unsigned int fetchlimit;	/* Maximum number of bytes to fetch. */
  unsigned int nfetch;		/* Bytes to fetch / bytes fetched. */
  unsigned int chunksize;	/* Size of each fetch, in bytes. */
  unsigned int bufsize;		/* Size of current fetch buffer. */
  char *buffer = NULL;		/* Dynamically growable fetch buffer. */
  char *bufptr;			/* Pointer to next available byte in buffer. */
  char *limit;			/* First location past end of fetch buffer. */
  struct cleanup *old_chain = NULL; /* Top of the old cleanup chain. */
  char peekchar;		/* Place into which we can read one char. */

  /* First we need to figure out the limit on the number of characters we are
     going to attempt to fetch and print.  This is actually pretty simple.  If
     LEN is nonzero, then the limit is the minimum of LEN and print_max.  If
     LEN is zero, then the limit is print_max.  This is true regardless of
     whether print_max is zero, UINT_MAX (unlimited), or something in between,
     because finding the null byte (or available memory) is what actually
     limits the fetch. */

  fetchlimit = (len == 0 ? print_max : min (len, print_max));

  /* Now decide how large of chunks to try to read in one operation.  This
     is also pretty simple.  If LEN is nonzero, then we want fetchlimit bytes,
     so we might as well read them all in one operation.  If LEN is zero, we
     are looking for a null terminator to end the fetching, so we might as
     well read in blocks that are large enough to be efficient, but not so
     large as to be slow if fetchlimit happens to be large.  So we choose the
     minimum of 8 and fetchlimit.  We used to use 200 instead of 8 but
     200 is way too big for remote debugging over a serial line.  */

  chunksize = (len == 0 ? min (8, fetchlimit) : fetchlimit);

  /* Loop until we either have all the characters to print, or we encounter
     some error, such as bumping into the end of the address space. */

  bufsize = 0;
  do {
    QUIT;
    /* Figure out how much to fetch this time, and grow the buffer to fit. */
    nfetch = min (chunksize, fetchlimit - bufsize);
    bufsize += nfetch;
    if (buffer == NULL)
      {
	buffer = (char *) xmalloc (bufsize);
	bufptr = buffer;
      }
    else
      {
	discard_cleanups (old_chain);
	buffer = (char *) xrealloc (buffer, bufsize);
	bufptr = buffer + bufsize - nfetch;
      }
    old_chain = make_cleanup (free, buffer);

    /* Read as much as we can. */
    nfetch = target_read_memory_partial (addr, bufptr, nfetch, &errcode);
    if (len != 0)
      {
	addr += nfetch;
	bufptr += nfetch;
      }
    else
      {
	/* Scan this chunk for the null byte that terminates the string
	   to print.  If found, we don't need to fetch any more.  Note
	   that bufptr is explicitly left pointing at the next character
	   after the null byte, or at the next character after the end of
	   the buffer. */
	limit = bufptr + nfetch;
	while (bufptr < limit)
	  {
	    ++addr;
	    ++bufptr;
	    if (bufptr[-1] == '\0')
	      {
		/* We don't care about any error which happened after
		   the NULL terminator.  */
		errcode = 0;
		break;
	      }
	  }
      }
  } while (errcode == 0					/* no error */
	   && bufsize < fetchlimit			/* no overrun */
	   && !(len == 0 && *(bufptr - 1) == '\0'));	/* no null term */

  /* bufptr and addr now point immediately beyond the last byte which we
     consider part of the string (including a '\0' which ends the string).  */

  /* We now have either successfully filled the buffer to fetchlimit, or
     terminated early due to an error or finding a null byte when LEN is
     zero.  */

  if (len == 0 && bufptr > buffer && *(bufptr - 1) != '\0')
    {
      /* We didn't find a null terminator we were looking for.  Attempt
	 to peek at the next character.  If not successful, or it is not
	 a null byte, then force ellipsis to be printed.  */
      if (target_read_memory (addr, &peekchar, 1) != 0 || peekchar != '\0')
	{
	  force_ellipsis = 1;
	}
    }
  else if ((len != 0 && errcode != 0) || (len > bufptr - buffer))
    {
      /* Getting an error when we have a requested length, or fetching less
	 than the number of characters actually requested, always make us
	 print ellipsis. */
      force_ellipsis = 1;
    }

  QUIT;

  /* If we get an error before fetching anything, don't print a string.
     But if we fetch something and then get an error, print the string
     and then the error message.  */
  if (errcode == 0 || bufptr > buffer)
    {
      if (addressprint)
	{
	  fputs_filtered (" ", stream);
	}
      LA_PRINT_STRING (stream, buffer, bufptr - buffer, force_ellipsis);
    }

  if (errcode != 0)
    {
      if (errcode == EIO)
	{
	  fprintf_filtered (stream, " <Address ");
	  print_address_numeric (addr, 1, stream);
	  fprintf_filtered (stream, " out of bounds>");
	}
      else
	{
	  fprintf_filtered (stream, " <Error reading address ");
	  print_address_numeric (addr, 1, stream);
	  fprintf_filtered (stream, ": %s>", safe_strerror (errcode));
	}
    }
  gdb_flush (stream);
  do_cleanups (old_chain);
  return (bufptr - buffer);
}


/* Validate an input or output radix setting, and make sure the user
   knows what they really did here.  Radix setting is confusing, e.g.
   setting the input radix to "10" never changes it!  */

/* ARGSUSED */
static void
set_input_radix (args, from_tty, c)
     char *args;
     int from_tty;
     struct cmd_list_element *c;
{
  set_input_radix_1 (from_tty, *(unsigned *)c->var);
}

/* ARGSUSED */
static void
set_input_radix_1 (from_tty, radix)
     int from_tty;
     unsigned radix;
{
  /* We don't currently disallow any input radix except 0 or 1, which don't
     make any mathematical sense.  In theory, we can deal with any input
     radix greater than 1, even if we don't have unique digits for every
     value from 0 to radix-1, but in practice we lose on large radix values.
     We should either fix the lossage or restrict the radix range more.
     (FIXME). */

  if (radix < 2)
    {
      error ("Nonsense input radix ``decimal %u''; input radix unchanged.",
	     radix);
    }
  input_radix = radix;
  if (from_tty)
    {
      printf_filtered ("Input radix now set to decimal %u, hex %x, octal %o.\n",
		       radix, radix, radix);
    }
}

/* ARGSUSED */
static void
set_output_radix (args, from_tty, c)
     char *args;
     int from_tty;
     struct cmd_list_element *c;
{
  set_output_radix_1 (from_tty, *(unsigned *)c->var);
}

static void
set_output_radix_1 (from_tty, radix)
     int from_tty;
     unsigned radix;
{
  /* Validate the radix and disallow ones that we aren't prepared to
     handle correctly, leaving the radix unchanged. */
  switch (radix)
    {
    case 16:
      output_format = 'x';		/* hex */
      break;
    case 10:
      output_format = 0;		/* decimal */
      break;
    case 8:
      output_format = 'o';		/* octal */
      break;
    default:
      error ("Unsupported output radix ``decimal %u''; output radix unchanged.",
	     radix);
    }
  output_radix = radix;
  if (from_tty)
    {
      printf_filtered ("Output radix now set to decimal %u, hex %x, octal %o.\n",
		       radix, radix, radix);
    }
}

/* Set both the input and output radix at once.  Try to set the output radix
   first, since it has the most restrictive range.  An radix that is valid as
   an output radix is also valid as an input radix.

   It may be useful to have an unusual input radix.  If the user wishes to
   set an input radix that is not valid as an output radix, he needs to use
   the 'set input-radix' command. */

static void
set_radix (arg, from_tty)
     char *arg;
     int from_tty;
{
  unsigned radix;

  radix = (arg == NULL) ? 10 : parse_and_eval_address (arg);
  set_output_radix_1 (0, radix);
  set_input_radix_1 (0, radix);
  if (from_tty)
    {
      printf_filtered ("Input and output radices now set to decimal %u, hex %x, octal %o.\n",
		       radix, radix, radix);
    }
}

/* Show both the input and output radices. */

/*ARGSUSED*/
static void
show_radix (arg, from_tty)
     char *arg;
     int from_tty;
{
  if (from_tty)
    {
      if (input_radix == output_radix)
	{
	  printf_filtered ("Input and output radices set to decimal %u, hex %x, octal %o.\n",
			   input_radix, input_radix, input_radix);
	}
      else
	{
	  printf_filtered ("Input radix set to decimal %u, hex %x, octal %o.\n",
			   input_radix, input_radix, input_radix);
	  printf_filtered ("Output radix set to decimal %u, hex %x, octal %o.\n",
			   output_radix, output_radix, output_radix);
	}
    }
}


/*ARGSUSED*/
static void
set_print (arg, from_tty)
     char *arg;
     int from_tty;
{
  printf_unfiltered (
"\"set print\" must be followed by the name of a print subcommand.\n");
  help_list (setprintlist, "set print ", -1, gdb_stdout);
}

/*ARGSUSED*/
static void
show_print (args, from_tty)
     char *args;
     int from_tty;
{
  cmd_show_list (showprintlist, from_tty, "");
}

void
_initialize_valprint ()
{
  struct cmd_list_element *c;

  add_prefix_cmd ("print", no_class, set_print,
		  "Generic command for setting how things print.",
		  &setprintlist, "set print ", 0, &setlist);
  add_alias_cmd ("p", "print", no_class, 1, &setlist); 
  /* prefer set print to set prompt */ 
  add_alias_cmd ("pr", "print", no_class, 1, &setlist);

  add_prefix_cmd ("print", no_class, show_print,
		  "Generic command for showing print settings.",
		  &showprintlist, "show print ", 0, &showlist);
  add_alias_cmd ("p", "print", no_class, 1, &showlist); 
  add_alias_cmd ("pr", "print", no_class, 1, &showlist); 

  add_show_from_set
    (add_set_cmd ("elements", no_class, var_uinteger, (char *)&print_max,
		  "Set limit on string chars or array elements to print.\n\
\"set print elements 0\" causes there to be no limit.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("null-stop", no_class, var_boolean,
		  (char *)&stop_print_at_null,
		  "Set printing of char arrays to stop at first null char.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("repeats", no_class, var_uinteger,
		  (char *)&repeat_count_threshold,
		  "Set threshold for repeated print elements.\n\
\"set print repeats 0\" causes all elements to be individually printed.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("pretty", class_support, var_boolean,
		  (char *)&prettyprint_structs,
		  "Set prettyprinting of structures.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("union", class_support, var_boolean, (char *)&unionprint,
		  "Set printing of unions interior to structures.",
		  &setprintlist),
     &showprintlist);
  
  add_show_from_set
    (add_set_cmd ("array", class_support, var_boolean,
		  (char *)&prettyprint_arrays,
		  "Set prettyprinting of arrays.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("address", class_support, var_boolean, (char *)&addressprint,
		  "Set printing of addresses.",
		  &setprintlist),
     &showprintlist);

  c = add_set_cmd ("input-radix", class_support, var_uinteger,
		   (char *)&input_radix,
		  "Set default input radix for entering numbers.",
		  &setlist);
  add_show_from_set (c, &showlist);
  c->function.sfunc = set_input_radix;

  c = add_set_cmd ("output-radix", class_support, var_uinteger,
		   (char *)&output_radix,
		  "Set default output radix for printing of values.",
		  &setlist);
  add_show_from_set (c, &showlist);
  c->function.sfunc = set_output_radix;

  /* The "set radix" and "show radix" commands are special in that they are
     like normal set and show commands but allow two normally independent
     variables to be either set or shown with a single command.  So the
     usual add_set_cmd() and add_show_from_set() commands aren't really
     appropriate. */
  add_cmd ("radix", class_support, set_radix,
	   "Set default input and output number radices.\n\
Use 'set input-radix' or 'set output-radix' to independently set each.\n\
Without an argument, sets both radices back to the default value of 10.",
	   &setlist);
  add_cmd ("radix", class_support, show_radix,
	   "Show the default input and output number radices.\n\
Use 'show input-radix' or 'show output-radix' to independently show each.",
	   &showlist);

  /* Give people the defaults which they are used to.  */
  prettyprint_structs = 0;
  prettyprint_arrays = 0;
  unionprint = 1;
  addressprint = 1;
  print_max = PRINT_MAX_DEFAULT;
}
