/* Print VAX instructions for GDB, the GNU debugger.
   Copyright 1986, 1989, 1991, 1992, 1995, 1996, 1998, 1999, 2000
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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "symtab.h"
#include "opcode/vax.h"
#include "gdbcore.h"
#include "frame.h"
#include "value.h"

/* Vax instructions are never longer than this.  */
#define MAXLEN 62

/* Number of elements in the opcode table.  */
#define NOPCODES (sizeof votstrs / sizeof votstrs[0])

static unsigned char *print_insn_arg ();

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

CORE_ADDR
vax_skip_prologue (CORE_ADDR pc)
{
  register int op = (unsigned char) read_memory_integer (pc, 1);
  if (op == 0x11)
    pc += 2;			/* skip brb */
  if (op == 0x31)
    pc += 3;			/* skip brw */
  if (op == 0xC2
      && ((unsigned char) read_memory_integer (pc + 2, 1)) == 0x5E)
    pc += 3;			/* skip subl2 */
  if (op == 0x9E
      && ((unsigned char) read_memory_integer (pc + 1, 1)) == 0xAE
      && ((unsigned char) read_memory_integer (pc + 3, 1)) == 0x5E)
    pc += 4;			/* skip movab */
  if (op == 0x9E
      && ((unsigned char) read_memory_integer (pc + 1, 1)) == 0xCE
      && ((unsigned char) read_memory_integer (pc + 4, 1)) == 0x5E)
    pc += 5;			/* skip movab */
  if (op == 0x9E
      && ((unsigned char) read_memory_integer (pc + 1, 1)) == 0xEE
      && ((unsigned char) read_memory_integer (pc + 6, 1)) == 0x5E)
    pc += 7;			/* skip movab */
  return pc;
}

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

int
vax_frame_num_args (struct frame_info *fi)
{
  return (0xff & read_memory_integer (FRAME_ARGS_ADDRESS (fi), 1));
}



/* Print the vax instruction at address MEMADDR in debugged memory,
   from disassembler info INFO.
   Returns length of the instruction, in bytes.  */

static int
vax_print_insn (CORE_ADDR memaddr, disassemble_info *info)
{
  unsigned char buffer[MAXLEN];
  register int i;
  register unsigned char *p;
  const char *d;

  int status = (*info->read_memory_func) (memaddr, buffer, MAXLEN, info);
  if (status != 0)
    {
      (*info->memory_error_func) (status, memaddr, info);
      return -1;
    }

  for (i = 0; i < NOPCODES; i++)
    if (votstrs[i].detail.code == buffer[0]
	|| votstrs[i].detail.code == *(unsigned short *) buffer)
      break;

  /* Handle undefined instructions.  */
  if (i == NOPCODES)
    {
      (*info->fprintf_func) (info->stream, "0%o", buffer[0]);
      return 1;
    }

  (*info->fprintf_func) (info->stream, "%s", votstrs[i].name);

  /* Point at first byte of argument data,
     and at descriptor for first argument.  */
  p = buffer + 1 + (votstrs[i].detail.code >= 0x100);
  d = votstrs[i].detail.args;

  if (*d)
    (*info->fprintf_func) (info->stream, " ");

  while (*d)
    {
      p = print_insn_arg (d, p, memaddr + (p - buffer), info);
      d += 2;
      if (*d)
	(*info->fprintf_func) (info->stream, ",");
    }
  return p - buffer;
}

static unsigned char *
print_insn_arg (char *d, register char *p, CORE_ADDR addr,
		disassemble_info *info)
{
  register int regnum = *p & 0xf;
  float floatlitbuf;

  if (*d == 'b')
    {
      if (d[1] == 'b')
	(*info->fprintf_func) (info->stream, "0x%x", addr + *p++ + 1);
      else
	{
	  (*info->fprintf_func) (info->stream, "0x%x", addr + *(short *) p + 2);
	  p += 2;
	}
    }
  else
    switch ((*p++ >> 4) & 0xf)
      {
      case 0:
      case 1:
      case 2:
      case 3:			/* Literal mode */
	if (d[1] == 'd' || d[1] == 'f' || d[1] == 'g' || d[1] == 'h')
	  {
	    *(int *) &floatlitbuf = 0x4000 + ((p[-1] & 0x3f) << 4);
	    (*info->fprintf_func) (info->stream, "$%f", floatlitbuf);
	  }
	else
	  (*info->fprintf_func) (info->stream, "$%d", p[-1] & 0x3f);
	break;

      case 4:			/* Indexed */
	p = (char *) print_insn_arg (d, p, addr + 1, info);
	(*info->fprintf_func) (info->stream, "[%s]", REGISTER_NAME (regnum));
	break;

      case 5:			/* Register */
	(*info->fprintf_func) (info->stream, REGISTER_NAME (regnum));
	break;

      case 7:			/* Autodecrement */
	(*info->fprintf_func) (info->stream, "-");
      case 6:			/* Register deferred */
	(*info->fprintf_func) (info->stream, "(%s)", REGISTER_NAME (regnum));
	break;

      case 9:			/* Autoincrement deferred */
	(*info->fprintf_func) (info->stream, "@");
	if (regnum == PC_REGNUM)
	  {
	    (*info->fprintf_func) (info->stream, "#");
	    info->target = *(long *) p;
	    (*info->print_address_func) (info->target, info);
	    p += 4;
	    break;
	  }
      case 8:			/* Autoincrement */
	if (regnum == PC_REGNUM)
	  {
	    (*info->fprintf_func) (info->stream, "#");
	    switch (d[1])
	      {
	      case 'b':
		(*info->fprintf_func) (info->stream, "%d", *p++);
		break;

	      case 'w':
		(*info->fprintf_func) (info->stream, "%d", *(short *) p);
		p += 2;
		break;

	      case 'l':
		(*info->fprintf_func) (info->stream, "%d", *(long *) p);
		p += 4;
		break;

	      case 'q':
		(*info->fprintf_func) (info->stream, "0x%x%08x",
				       ((long *) p)[1], ((long *) p)[0]);
		p += 8;
		break;

	      case 'o':
		(*info->fprintf_func) (info->stream, "0x%x%08x%08x%08x",
				       ((long *) p)[3], ((long *) p)[2],
				       ((long *) p)[1], ((long *) p)[0]);
		p += 16;
		break;

	      case 'f':
		if (INVALID_FLOAT (p, 4))
		  (*info->fprintf_func) (info->stream,
					 "<<invalid float 0x%x>>",
					 *(int *) p);
		else
		  (*info->fprintf_func) (info->stream, "%f", *(float *) p);
		p += 4;
		break;

	      case 'd':
		if (INVALID_FLOAT (p, 8))
		  (*info->fprintf_func) (info->stream,
					 "<<invalid float 0x%x%08x>>",
					 ((long *) p)[1], ((long *) p)[0]);
		else
		  (*info->fprintf_func) (info->stream, "%f", *(double *) p);
		p += 8;
		break;

	      case 'g':
		(*info->fprintf_func) (info->stream, "g-float");
		p += 8;
		break;

	      case 'h':
		(*info->fprintf_func) (info->stream, "h-float");
		p += 16;
		break;

	      }
	  }
	else
	  (*info->fprintf_func) (info->stream, "(%s)+", REGISTER_NAME (regnum));
	break;

      case 11:			/* Byte displacement deferred */
	(*info->fprintf_func) (info->stream, "@");
      case 10:			/* Byte displacement */
	if (regnum == PC_REGNUM)
	  {
	    info->target = addr + *p + 2;
	    (*info->print_address_func) (info->target, info);
	  }
	else
	  (*info->fprintf_func) (info->stream, "%d(%s)", *p, REGISTER_NAME (regnum));
	p += 1;
	break;

      case 13:			/* Word displacement deferred */
	(*info->fprintf_func) (info->stream, "@");
      case 12:			/* Word displacement */
	if (regnum == PC_REGNUM)
	  {
	    info->target = addr + *(short *) p + 3;
	    (*info->print_address_func) (info->target, info);
	  }
	else
	  (*info->fprintf_func) (info->stream, "%d(%s)",
				 *(short *) p, REGISTER_NAME (regnum));
	p += 2;
	break;

      case 15:			/* Long displacement deferred */
	(*info->fprintf_func) (info->stream, "@");
      case 14:			/* Long displacement */
	if (regnum == PC_REGNUM)
	  {
	    info->target = addr + *(short *) p + 5;
	    (*info->print_address_func) (info->target, info);
	  }
	else
	  (*info->fprintf_func) (info->stream, "%d(%s)",
				 *(long *) p, REGISTER_NAME (regnum));
	p += 4;
      }

  return (unsigned char *) p;
}

void
_initialize_vax_tdep (void)
{
  tm_print_insn = vax_print_insn;
}
