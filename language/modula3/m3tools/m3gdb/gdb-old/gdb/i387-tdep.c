/* Intel 387 floating point stuff.
   Copyright (C) 1988, 1989, 1991-98 Free Software Foundation, Inc.

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
#include "frame.h"
#include "inferior.h"
#include "language.h"
#include "gdbcore.h"
#include "floatformat.h"

/* FIXME:  Eliminate these routines when we have the time to change all
   the callers.  */

void
i387_to_double (from, to)
     char *from;
     char *to;
{
  floatformat_to_double (&floatformat_i387_ext, from, (double *)to);
}

void
double_to_i387 (from, to)
     char *from;
     char *to;
{
  floatformat_from_double (&floatformat_i387_ext, (double *)from, to);
}

static void
print_387_control_bits (control)
     unsigned int control;
{
  switch ((control >> 8) & 3) 
    {
    case 0: puts_unfiltered (" 24 bit; "); break;
    case 1: puts_unfiltered (" (bad); "); break;
    case 2: puts_unfiltered (" 53 bit; "); break;
    case 3: puts_unfiltered (" 64 bit; "); break;
    }
  switch ((control >> 10) & 3) 
    {
    case 0: puts_unfiltered ("NEAR; "); break;
    case 1: puts_unfiltered ("DOWN; "); break;
    case 2: puts_unfiltered ("UP; "); break;
    case 3: puts_unfiltered ("CHOP; "); break;
    }
  if (control & 0x3f) 
    {
      puts_unfiltered ("mask");
      if (control & 0x0001) puts_unfiltered (" INVAL");
      if (control & 0x0002) puts_unfiltered (" DENOR");
      if (control & 0x0004) puts_unfiltered (" DIVZ");
      if (control & 0x0008) puts_unfiltered (" OVERF");
      if (control & 0x0010) puts_unfiltered (" UNDER");
      if (control & 0x0020) puts_unfiltered (" LOS");
      puts_unfiltered (";");
    }
  if (control & 0xe080) warning ("\nreserved bits on: %s",
				local_hex_string(control & 0xe080));
}

void
print_387_control_word (control)
     unsigned int control;
{
  printf_filtered ("control %s:", local_hex_string(control & 0xffff));
  print_387_control_bits (control);
  puts_unfiltered ("\n");
}

static void
print_387_status_bits (status)
     unsigned int status;
{
  printf_unfiltered (" flags %d%d%d%d; ",
	  (status & 0x4000) != 0,
	  (status & 0x0400) != 0,
	  (status & 0x0200) != 0,
	  (status & 0x0100) != 0);
  printf_unfiltered ("top %d; ", (status >> 11) & 7);
  if (status & 0xff) 
    {
      puts_unfiltered ("excep");
      if (status & 0x0001) puts_unfiltered (" INVAL");
      if (status & 0x0002) puts_unfiltered (" DENOR");
      if (status & 0x0004) puts_unfiltered (" DIVZ");
      if (status & 0x0008) puts_unfiltered (" OVERF");
      if (status & 0x0010) puts_unfiltered (" UNDER");
      if (status & 0x0020) puts_unfiltered (" LOS");
      if (status & 0x0040) puts_unfiltered (" STACK");
    }
}

void
print_387_status_word (status)
     unsigned int status;
{
  printf_filtered ("status %s:", local_hex_string (status & 0xffff));
  print_387_status_bits (status);
  puts_unfiltered ("\n");
}


void
i387_print_register (raw_regs, regnum)
     char *raw_regs;
     int regnum;
{
  unsigned char virtual_buffer[MAX_REGISTER_VIRTUAL_SIZE];
  unsigned long val;
  int j, sign, special;
  unsigned swd, tags, expon, top, norm, ls, ms;
  char string[12];

  printf_filtered ("%8.8s: ", reg_names[regnum]);
  if (REGISTER_RAW_SIZE (regnum) == 4)
    {
      val = extract_unsigned_integer (raw_regs + REGISTER_BYTE (regnum), 4);
      switch (regnum)
	{
	case FPCWD_REGNUM:
#if FPCWD_REGNUM != FPSWD_REGNUM        
	case FPSWD_REGNUM:
#endif
#if (FPCWD_REGNUM != FPTWD_REGNUM) && (FPSWD_REGNUM != FPTWD_REGNUM)
	case FPTWD_REGNUM:
#endif
#if (FPCWD_REGNUM != FPOPS_REGNUM) && (FPSWD_REGNUM != FPOPS_REGNUM) && (FPTWD_REGNUM != FPOPS_REGNUM)
	case FPOPS_REGNUM:
#endif
	  /* Don't print the un-modifiable bytes. */
	  sprintf(string, "0x%04x", val & 0xffff);
	  break;

	default:
	  sprintf(string, "0x%08x", val);
	  break;
	}

      printf_unfiltered ("%10.10s", string);

      if ( regnum == FPCWD_REGNUM)
	print_387_control_bits (val);
      else if ( regnum == FPSWD_REGNUM)
	print_387_status_bits (val);
    }
  else
    {
      /* Put the data in the buffer.  No conversions are ever necessary. */
      memcpy (virtual_buffer, raw_regs + REGISTER_BYTE (regnum), 10);

      swd = extract_signed_integer (raw_regs + REGISTER_BYTE (FP0_REGNUM+1),
				    4);
      top = (swd >> 11) & 7;
      tags = extract_signed_integer (raw_regs + REGISTER_BYTE (FP0_REGNUM+2),
				     4);

      puts_unfiltered ("0x");
      for (j = 0; j < 10; j++)
	printf_unfiltered ("%02x",
			   (unsigned char)raw_regs[REGISTER_BYTE (regnum)
						  + 9 - j]);
      
      puts_unfiltered ("  ");
      special = 0;
      switch ((tags >> (((regnum - FP0_REGNUM + top) & 7) * 2)) & 3) 
	{
	case 0: puts_unfiltered ("Valid "); break;
	case 1: puts_unfiltered ("Zero  "); break;
	case 2: puts_unfiltered ("Spec  ");
	  special = 1;
	  break;
	case 3: puts_unfiltered ("Empty "); break;
	}

      expon = extract_unsigned_integer (raw_regs + REGISTER_BYTE (regnum)
					+ 8, 2);
      sign = expon & 0x8000;
      expon &= 0x7fff;
      ms = extract_unsigned_integer (raw_regs + REGISTER_BYTE (regnum) + 4, 4);
      ls = extract_signed_integer (raw_regs + REGISTER_BYTE (regnum), 4);
      norm = ms & 0x80000000;

      if ( expon == 0 )
	{
	  if ( ms | ls )
	    {
	      /* Denormal or Pseudodenormal. */
	      if ( norm )
		puts_unfiltered ("Pseudo ");
	      else
		puts_unfiltered ("Denorm ");
	    }
	  else
	    {
	      /* Zero. */
	      puts_unfiltered ("Zero   ");
	    }
	}
      else if ( expon == 0x7fff )
	{
	  /* Infinity, NaN or unsupported. */
	  if ( (ms == 0x80000000) &&
	       (ls == 0) )
	    {
              puts_unfiltered ("Infty  ");
	    }
	  else if ( norm )
	    {
	      if ( ms & 0x40000000 )
		puts_unfiltered ("QNaN   ");
	      else
		puts_unfiltered ("SNaN   ");
	    }
	  else
	    {
              puts_unfiltered ("Unsupp ");
	    }
	}
      else
	{
	  /* Normal or unsupported. */
	  if ( norm )
	    puts_unfiltered ("Normal ");
	  else
	    puts_unfiltered ("Unsupp ");
	}

      val_print (REGISTER_VIRTUAL_TYPE (regnum), virtual_buffer, 0,
		 gdb_stdout, 0,
		 1, 0, Val_pretty_default);
    }
  puts_filtered ("\n");
}

void
i387_float_info ()
{
  char raw_regs [REGISTER_BYTES];
  int numregs = RUNTIME_NUM_REGS (1);
  int i;

  if (numregs != NUM_REGS)
    {
      printf_filtered ("No floating point info available for this run-time environment.\n");
      return;
    }

  for (i = NUM_REGS - NUM_FREGS; i < numregs; i++)
    read_relative_register_raw_bytes (INFO_REGMAP (i),
				      raw_regs + REGISTER_BYTE (INFO_REGMAP (i)));

  for (i = NUM_REGS - NUM_FREGS; i < numregs; i++)
    i387_print_register (raw_regs, INFO_REGMAP (i));
}

int
i387_hex_long_double_input(p, val)
     char *p;
     DOUBLEST *val;
{
  int c, n, len = 20;

  n = 0;
  for (len = 20-1; len >= 0; len--)
    {
      c = *p++;
      if (c >= 'A' && c <= 'Z')
	c += 'a' - 'A';
      n *= 16;
      if (c >= '0' && c <= '9')
	{
	  n += c - '0';
	}
      else if (c >= 'a' && c <= 'f')
	{
	  n += c - 'a' + 10;
	}
      else
	return 0;	/* Char not a digit */
      if ( ! (len & 1) )
	{
	  ((unsigned char *)val)[len/2] = n;
	  n = 0;
	}
    }
  return 1;
}
