/* Target-machine dependent code for Zilog Z8000, for GDB.
   Copyright 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000, 2001
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

/*
   Contributed by Steve Chamberlain
   sac@cygnus.com
 */

#include "defs.h"
#include "frame.h"
#include "obstack.h"
#include "symtab.h"
#include "gdbcmd.h"
#include "gdbtypes.h"
#include "dis-asm.h"
#include "gdbcore.h"
#include "regcache.h"

#include "value.h" /* For read_register() */


static int read_memory_pointer (CORE_ADDR x);

/* Return the saved PC from this frame.

   If the frame has a memory copy of SRP_REGNUM, use that.  If not,
   just use the register SRP_REGNUM itself.  */

CORE_ADDR
z8k_frame_saved_pc (struct frame_info *frame)
{
  return read_memory_pointer (frame->frame + (BIG ? 4 : 2));
}

#define IS_PUSHL(x) (BIG ? ((x & 0xfff0) == 0x91e0):((x & 0xfff0) == 0x91F0))
#define IS_PUSHW(x) (BIG ? ((x & 0xfff0) == 0x93e0):((x & 0xfff0)==0x93f0))
#define IS_MOVE_FP(x) (BIG ? x == 0xa1ea : x == 0xa1fa)
#define IS_MOV_SP_FP(x) (BIG ? x == 0x94ea : x == 0x0d76)
#define IS_SUB2_SP(x) (x==0x1b87)
#define IS_MOVK_R5(x) (x==0x7905)
#define IS_SUB_SP(x) ((x & 0xffff) == 0x020f)
#define IS_PUSH_FP(x) (BIG ? (x == 0x93ea) : (x == 0x93fa))

/* work out how much local space is on the stack and
   return the pc pointing to the first push */

static CORE_ADDR
skip_adjust (CORE_ADDR pc, int *size)
{
  *size = 0;

  if (IS_PUSH_FP (read_memory_short (pc))
      && IS_MOV_SP_FP (read_memory_short (pc + 2)))
    {
      /* This is a function with an explict frame pointer */
      pc += 4;
      *size += 2;		/* remember the frame pointer */
    }

  /* remember any stack adjustment */
  if (IS_SUB_SP (read_memory_short (pc)))
    {
      *size += read_memory_short (pc + 2);
      pc += 4;
    }
  return pc;
}

static CORE_ADDR examine_frame (CORE_ADDR, CORE_ADDR * regs, CORE_ADDR);
static CORE_ADDR
examine_frame (CORE_ADDR pc, CORE_ADDR *regs, CORE_ADDR sp)
{
  int w = read_memory_short (pc);
  int offset = 0;
  int regno;

  for (regno = 0; regno < NUM_REGS; regno++)
    regs[regno] = 0;

  while (IS_PUSHW (w) || IS_PUSHL (w))
    {
      /* work out which register is being pushed to where */
      if (IS_PUSHL (w))
	{
	  regs[w & 0xf] = offset;
	  regs[(w & 0xf) + 1] = offset + 2;
	  offset += 4;
	}
      else
	{
	  regs[w & 0xf] = offset;
	  offset += 2;
	}
      pc += 2;
      w = read_memory_short (pc);
    }

  if (IS_MOVE_FP (w))
    {
      /* We know the fp */

    }
  else if (IS_SUB_SP (w))
    {
      /* Subtracting a value from the sp, so were in a function
         which needs stack space for locals, but has no fp.  We fake up
         the values as if we had an fp */
      regs[FP_REGNUM] = sp;
    }
  else
    {
      /* This one didn't have an fp, we'll fake it up */
      regs[SP_REGNUM] = sp;
    }
  /* stack pointer contains address of next frame */
  /*  regs[fp_regnum()] = fp; */
  regs[SP_REGNUM] = sp;
  return pc;
}

CORE_ADDR
z8k_skip_prologue (CORE_ADDR start_pc)
{
  CORE_ADDR dummy[NUM_REGS];

  return examine_frame (start_pc, dummy, 0);
}

CORE_ADDR
z8k_addr_bits_remove (CORE_ADDR addr)
{
  return (addr & PTR_MASK);
}

static int
read_memory_pointer (CORE_ADDR x)
{
  return read_memory_integer (ADDR_BITS_REMOVE (x), BIG ? 4 : 2);
}

CORE_ADDR
z8k_frame_chain (struct frame_info *thisframe)
{
  if (thisframe->prev == 0)
    {
      /* This is the top of the stack, let's get the sp for real */
    }
  if (!inside_entry_file (thisframe->pc))
    {
      return read_memory_pointer (thisframe->frame);
    }
  return 0;
}

void
init_frame_pc (void)
{
  internal_error (__FILE__, __LINE__, "failed internal consistency check");
}

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

void
z8k_frame_init_saved_regs (struct frame_info *frame_info)
{
  CORE_ADDR pc;
  int w;

  frame_saved_regs_zalloc (frame_info);
  pc = get_pc_function_start (frame_info->pc);

  /* wander down the instruction stream */
  examine_frame (pc, frame_info->saved_regs, frame_info->frame);

}

void
z8k_push_dummy_frame (void)
{
  internal_error (__FILE__, __LINE__, "failed internal consistency check");
}

int
gdb_print_insn_z8k (bfd_vma memaddr, disassemble_info *info)
{
  if (BIG)
    return print_insn_z8001 (memaddr, info);
  else
    return print_insn_z8002 (memaddr, info);
}

/* Fetch the instruction at ADDR, returning 0 if ADDR is beyond LIM or
   is not the address of a valid instruction, the address of the next
   instruction beyond ADDR otherwise.  *PWORD1 receives the first word
   of the instruction. */

CORE_ADDR
NEXT_PROLOGUE_INSN (CORE_ADDR addr, CORE_ADDR lim, short *pword1)
{
  char buf[2];
  if (addr < lim + 8)
    {
      read_memory (addr, buf, 2);
      *pword1 = extract_signed_integer (buf, 2);

      return addr + 2;
    }
  return 0;
}

#if 0
/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.

   We cache the result of doing this in the frame_cache_obstack, since
   it is fairly expensive.  */

void
frame_find_saved_regs (struct frame_info *fip, struct frame_saved_regs *fsrp)
{
  int locals;
  CORE_ADDR pc;
  CORE_ADDR adr;
  int i;

  memset (fsrp, 0, sizeof *fsrp);

  pc = skip_adjust (get_pc_function_start (fip->pc), &locals);

  {
    adr = FRAME_FP (fip) - locals;
    for (i = 0; i < 8; i++)
      {
	int word = read_memory_short (pc);

	pc += 2;
	if (IS_PUSHL (word))
	  {
	    fsrp->regs[word & 0xf] = adr;
	    fsrp->regs[(word & 0xf) + 1] = adr - 2;
	    adr -= 4;
	  }
	else if (IS_PUSHW (word))
	  {
	    fsrp->regs[word & 0xf] = adr;
	    adr -= 2;
	  }
	else
	  break;
      }

  }

  fsrp->regs[PC_REGNUM] = fip->frame + 4;
  fsrp->regs[FP_REGNUM] = fip->frame;

}
#endif

int
z8k_saved_pc_after_call (struct frame_info *frame)
{
  return ADDR_BITS_REMOVE
    (read_memory_integer (read_register (SP_REGNUM), PTR_SIZE));
}


void
extract_return_value (struct type *type, char *regbuf, char *valbuf)
{
  int b;
  int len = TYPE_LENGTH (type);

  for (b = 0; b < len; b += 2)
    {
      int todo = len - b;

      if (todo > 2)
	todo = 2;
      memcpy (valbuf + b, regbuf + b, todo);
    }
}

void
write_return_value (struct type *type, char *valbuf)
{
  int reg;
  int len;

  for (len = 0; len < TYPE_LENGTH (type); len += 2)
    write_register_bytes (REGISTER_BYTE (len / 2 + 2), valbuf + len, 2);
}

void
store_struct_return (CORE_ADDR addr, CORE_ADDR sp)
{
  write_register (2, addr);
}


void
z8k_print_register_hook (int regno)
{
  if ((regno & 1) == 0 && regno < 16)
    {
      unsigned short l[2];

      read_relative_register_raw_bytes (regno, (char *) (l + 0));
      read_relative_register_raw_bytes (regno + 1, (char *) (l + 1));
      printf_unfiltered ("\t");
      printf_unfiltered ("%04x%04x", l[0], l[1]);
    }

  if ((regno & 3) == 0 && regno < 16)
    {
      unsigned short l[4];

      read_relative_register_raw_bytes (regno, (char *) (l + 0));
      read_relative_register_raw_bytes (regno + 1, (char *) (l + 1));
      read_relative_register_raw_bytes (regno + 2, (char *) (l + 2));
      read_relative_register_raw_bytes (regno + 3, (char *) (l + 3));

      printf_unfiltered ("\t");
      printf_unfiltered ("%04x%04x%04x%04x", l[0], l[1], l[2], l[3]);
    }
  if (regno == 15)
    {
      unsigned short rval;
      int i;

      read_relative_register_raw_bytes (regno, (char *) (&rval));

      printf_unfiltered ("\n");
      for (i = 0; i < 10; i += 2)
	{
	  printf_unfiltered ("(sp+%d=%04x)", i,
			     (unsigned int)read_memory_short (rval + i));
	}
    }

}

void
z8k_pop_frame (void)
{
}

struct cmd_list_element *setmemorylist;

void
z8k_set_pointer_size (int newsize)
{
  static int oldsize = 0;

  if (oldsize != newsize)
    {
      printf_unfiltered ("pointer size set to %d bits\n", newsize);
      oldsize = newsize;
      if (newsize == 32)
	{
	  BIG = 1;
	}
      else
	{
	  BIG = 0;
	}
      /* FIXME: This code should be using the GDBARCH framework to
         handle changed type sizes.  If this problem is ever fixed
         (the direct reference to _initialize_gdbtypes() below
         eliminated) then Makefile.in should be updated so that
         z8k-tdep.c is again compiled with -Werror. */
      _initialize_gdbtypes ();
    }
}

static void
segmented_command (char *args, int from_tty)
{
  z8k_set_pointer_size (32);
}

static void
unsegmented_command (char *args, int from_tty)
{
  z8k_set_pointer_size (16);
}

static void
set_memory (char *args, int from_tty)
{
  printf_unfiltered ("\"set memory\" must be followed by the name of a memory subcommand.\n");
  help_list (setmemorylist, "set memory ", -1, gdb_stdout);
}

void
_initialize_z8ktdep (void)
{
  tm_print_insn = gdb_print_insn_z8k;

  add_prefix_cmd ("memory", no_class, set_memory,
		  "set the memory model", &setmemorylist, "set memory ", 0,
		  &setlist);
  add_cmd ("segmented", class_support, segmented_command,
	   "Set segmented memory model.", &setmemorylist);
  add_cmd ("unsegmented", class_support, unsegmented_command,
	   "Set unsegmented memory model.", &setmemorylist);

}
