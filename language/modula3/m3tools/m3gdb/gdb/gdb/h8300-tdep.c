/* Target-machine dependent code for Hitachi H8/300, for GDB.
   Copyright 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1998, 1999,
   2000, 2001 Free Software Foundation, Inc.

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
#include "dis-asm.h"
#include "gdbcmd.h"
#include "gdbtypes.h"
#include "gdbcore.h"
#include "gdb_string.h"
#include "value.h"
#include "regcache.h"

extern int h8300hmode, h8300smode;

#undef NUM_REGS
#define NUM_REGS 11

#define UNSIGNED_SHORT(X) ((X) & 0xffff)

#define IS_PUSH(x) ((x & 0xfff0)==0x6df0)
#define IS_PUSH_FP(x) (x == 0x6df6)
#define IS_MOVE_FP(x) (x == 0x0d76 || x == 0x0ff6)
#define IS_MOV_SP_FP(x) (x == 0x0d76 || x == 0x0ff6)
#define IS_SUB2_SP(x) (x==0x1b87)
#define IS_SUB4_SP(x) (x==0x1b97)
#define IS_SUBL_SP(x) (x==0x7a37)
#define IS_MOVK_R5(x) (x==0x7905)
#define IS_SUB_R5SP(x) (x==0x1957)


/* The register names change depending on whether the h8300h processor
   type is selected. */

static char *original_register_names[] = REGISTER_NAMES;

static char *h8300h_register_names[] =
{"er0", "er1", "er2", "er3", "er4", "er5", "er6",
 "sp", "ccr", "pc", "cycles", "tick", "inst"};

char **h8300_register_names = original_register_names;


/* Local function declarations.  */

static CORE_ADDR examine_prologue ();
static void set_machine_hook (char *filename);

CORE_ADDR
h8300_skip_prologue (CORE_ADDR start_pc)
{
  short int w;
  int adjust = 0;

  /* Skip past all push and stm insns.  */
  while (1)
    {
      w = read_memory_unsigned_integer (start_pc, 2);
      /* First look for push insns.  */
      if (w == 0x0100 || w == 0x0110 || w == 0x0120 || w == 0x0130)
	{
	  w = read_memory_unsigned_integer (start_pc + 2, 2);
	  adjust = 2;
	}

      if (IS_PUSH (w))
	{
	  start_pc += 2 + adjust;
	  w = read_memory_unsigned_integer (start_pc, 2);
	  continue;
	}
      adjust = 0;
      break;
    }

  /* Skip past a move to FP, either word or long sized */
  w = read_memory_unsigned_integer (start_pc, 2);
  if (w == 0x0100)
    {
      w = read_memory_unsigned_integer (start_pc + 2, 2);
      adjust += 2;
    }

  if (IS_MOVE_FP (w))
    {
      start_pc += 2 + adjust;
      w = read_memory_unsigned_integer (start_pc, 2);
    }

  /* Check for loading either a word constant into r5;
     long versions are handled by the SUBL_SP below.  */
  if (IS_MOVK_R5 (w))
    {
      start_pc += 2;
      w = read_memory_unsigned_integer (start_pc, 2);
    }

  /* Now check for subtracting r5 from sp, word sized only.  */
  if (IS_SUB_R5SP (w))
    {
      start_pc += 2 + adjust;
      w = read_memory_unsigned_integer (start_pc, 2);
    }

  /* Check for subs #2 and subs #4. */
  while (IS_SUB2_SP (w) || IS_SUB4_SP (w))
    {
      start_pc += 2 + adjust;
      w = read_memory_unsigned_integer (start_pc, 2);
    }

  /* Check for a 32bit subtract.  */
  if (IS_SUBL_SP (w))
    start_pc += 6 + adjust;

  return start_pc;
}

int
gdb_print_insn_h8300 (bfd_vma memaddr, disassemble_info *info)
{
  if (h8300smode)
    return print_insn_h8300s (memaddr, info);
  else if (h8300hmode)
    return print_insn_h8300h (memaddr, info);
  else
    return print_insn_h8300 (memaddr, info);
}

/* Given a GDB frame, determine the address of the calling function's frame.
   This will be used to create a new GDB frame struct, and then
   INIT_EXTRA_FRAME_INFO and INIT_FRAME_PC will be called for the new frame.

   For us, the frame address is its stack pointer value, so we look up
   the function prologue to determine the caller's sp value, and return it.  */

CORE_ADDR
h8300_frame_chain (struct frame_info *thisframe)
{
  if (PC_IN_CALL_DUMMY (thisframe->pc, thisframe->frame, thisframe->frame))
    {				/* initialize the from_pc now */
      thisframe->from_pc = generic_read_register_dummy (thisframe->pc,
							thisframe->frame,
							PC_REGNUM);
      return thisframe->frame;
    }
  h8300_frame_find_saved_regs (thisframe, (struct frame_saved_regs *) 0);
  return thisframe->fsr->regs[SP_REGNUM];
}

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.

   We cache the result of doing this in the frame_obstack, since it is
   fairly expensive.  */

void
h8300_frame_find_saved_regs (struct frame_info *fi,
			     struct frame_saved_regs *fsr)
{
  register struct frame_saved_regs *cache_fsr;
  CORE_ADDR ip;
  struct symtab_and_line sal;
  CORE_ADDR limit;

  if (!fi->fsr)
    {
      cache_fsr = (struct frame_saved_regs *)
	frame_obstack_alloc (sizeof (struct frame_saved_regs));
      memset (cache_fsr, '\0', sizeof (struct frame_saved_regs));

      fi->fsr = cache_fsr;

      if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
	{			/* no more to do. */
	  if (fsr)
	    *fsr = *fi->fsr;
	  return;
	}
      /* Find the start and end of the function prologue.  If the PC
         is in the function prologue, we only consider the part that
         has executed already.  */

      ip = get_pc_function_start (fi->pc);
      sal = find_pc_line (ip, 0);
      limit = (sal.end && sal.end < fi->pc) ? sal.end : fi->pc;

      /* This will fill in fields in *fi as well as in cache_fsr.  */
      examine_prologue (ip, limit, fi->frame, cache_fsr, fi);
    }

  if (fsr)
    *fsr = *fi->fsr;
}

/* Fetch the instruction at ADDR, returning 0 if ADDR is beyond LIM or
   is not the address of a valid instruction, the address of the next
   instruction beyond ADDR otherwise.  *PWORD1 receives the first word
   of the instruction. */

CORE_ADDR
NEXT_PROLOGUE_INSN (CORE_ADDR addr, CORE_ADDR lim, INSN_WORD *pword1)
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

/* Examine the prologue of a function.  `ip' points to the first instruction.
   `limit' is the limit of the prologue (e.g. the addr of the first
   linenumber, or perhaps the program counter if we're stepping through).
   `frame_sp' is the stack pointer value in use in this frame.
   `fsr' is a pointer to a frame_saved_regs structure into which we put
   info about the registers saved by this frame.
   `fi' is a struct frame_info pointer; we fill in various fields in it
   to reflect the offsets of the arg pointer and the locals pointer.  */

static CORE_ADDR
examine_prologue (register CORE_ADDR ip, register CORE_ADDR limit,
		  CORE_ADDR after_prolog_fp, struct frame_saved_regs *fsr,
		  struct frame_info *fi)
{
  register CORE_ADDR next_ip;
  int r;
  int have_fp = 0;
  INSN_WORD insn_word;
  /* Number of things pushed onto stack, starts at 2/4, 'cause the
     PC is already there */
  unsigned int reg_save_depth = h8300hmode ? 4 : 2;

  unsigned int auto_depth = 0;	/* Number of bytes of autos */

  char in_frame[11];		/* One for each reg */

  int adjust = 0;

  memset (in_frame, 1, 11);
  for (r = 0; r < 8; r++)
    {
      fsr->regs[r] = 0;
    }
  if (after_prolog_fp == 0)
    {
      after_prolog_fp = read_register (SP_REGNUM);
    }

  /* If the PC isn't valid, quit now.  */
  if (ip == 0 || ip & (h8300hmode ? ~0xffffff : ~0xffff))
    return 0;

  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);

  if (insn_word == 0x0100)
    {
      insn_word = read_memory_unsigned_integer (ip + 2, 2);
      adjust = 2;
    }

  /* Skip over any fp push instructions */
  fsr->regs[6] = after_prolog_fp;
  while (next_ip && IS_PUSH_FP (insn_word))
    {
      ip = next_ip + adjust;

      in_frame[insn_word & 0x7] = reg_save_depth;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
      reg_save_depth += 2 + adjust;
    }

  /* Is this a move into the fp */
  if (next_ip && IS_MOV_SP_FP (insn_word))
    {
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
      have_fp = 1;
    }

  /* Skip over any stack adjustment, happens either with a number of
     sub#2,sp or a mov #x,r5 sub r5,sp */

  if (next_ip && (IS_SUB2_SP (insn_word) || IS_SUB4_SP (insn_word)))
    {
      while (next_ip && (IS_SUB2_SP (insn_word) || IS_SUB4_SP (insn_word)))
	{
	  auto_depth += IS_SUB2_SP (insn_word) ? 2 : 4;
	  ip = next_ip;
	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
	}
    }
  else
    {
      if (next_ip && IS_MOVK_R5 (insn_word))
	{
	  ip = next_ip;
	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
	  auto_depth += insn_word;

	  next_ip = NEXT_PROLOGUE_INSN (next_ip, limit, &insn_word);
	  auto_depth += insn_word;
	}
      if (next_ip && IS_SUBL_SP (insn_word))
	{
	  ip = next_ip;
	  auto_depth += read_memory_unsigned_integer (ip, 4);
	  ip += 4;

	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
	}
    }

  /* Now examine the push insns to determine where everything lives
     on the stack.  */
  while (1)
    {
      adjust = 0;
      if (!next_ip)
	break;

      if (insn_word == 0x0100)
	{
	  ip = next_ip;
	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
	  adjust = 2;
	}

      if (IS_PUSH (insn_word))
	{
	  ip = next_ip;
	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
	  fsr->regs[r] = after_prolog_fp + auto_depth;
	  auto_depth += 2 + adjust;
	  continue;
	}

      /* Now check for push multiple insns.  */
      if (insn_word == 0x0110 || insn_word == 0x0120 || insn_word == 0x0130)
	{
	  int count = ((insn_word >> 4) & 0xf) + 1;
	  int start, i;

	  ip = next_ip;
	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);
	  start = insn_word & 0x7;

	  for (i = start; i <= start + count; i++)
	    {
	      fsr->regs[i] = after_prolog_fp + auto_depth;
	      auto_depth += 4;
	    }
	}
      break;
    }

  /* The args are always reffed based from the stack pointer */
  fi->args_pointer = after_prolog_fp;
  /* Locals are always reffed based from the fp */
  fi->locals_pointer = after_prolog_fp;
  /* The PC is at a known place */
  fi->from_pc = read_memory_unsigned_integer (after_prolog_fp + BINWORD, BINWORD);

  /* Rememeber any others too */
  in_frame[PC_REGNUM] = 0;

  if (have_fp)
    /* We keep the old FP in the SP spot */
    fsr->regs[SP_REGNUM] = read_memory_unsigned_integer (fsr->regs[6], BINWORD);
  else
    fsr->regs[SP_REGNUM] = after_prolog_fp + auto_depth;

  return (ip);
}

void
h8300_init_extra_frame_info (int fromleaf, struct frame_info *fi)
{
  fi->fsr = 0;			/* Not yet allocated */
  fi->args_pointer = 0;		/* Unknown */
  fi->locals_pointer = 0;	/* Unknown */
  fi->from_pc = 0;
  if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
    {				/* anything special to do? */
      return;
    }
}

/* Return the saved PC from this frame.

   If the frame has a memory copy of SRP_REGNUM, use that.  If not,
   just use the register SRP_REGNUM itself.  */

CORE_ADDR
h8300_frame_saved_pc (struct frame_info *frame)
{
  if (PC_IN_CALL_DUMMY (frame->pc, frame->frame, frame->frame))
    return generic_read_register_dummy (frame->pc, frame->frame, PC_REGNUM);
  else
    return frame->from_pc;
}

CORE_ADDR
h8300_frame_locals_address (struct frame_info *fi)
{
  if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
    return (CORE_ADDR) 0;	/* Not sure what else to do... */
  if (!fi->locals_pointer)
    {
      struct frame_saved_regs ignore;

      get_frame_saved_regs (fi, &ignore);

    }
  return fi->locals_pointer;
}

/* Return the address of the argument block for the frame
   described by FI.  Returns 0 if the address is unknown.  */

CORE_ADDR
h8300_frame_args_address (struct frame_info *fi)
{
  if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
    return (CORE_ADDR) 0;	/* Not sure what else to do... */
  if (!fi->args_pointer)
    {
      struct frame_saved_regs ignore;

      get_frame_saved_regs (fi, &ignore);

    }

  return fi->args_pointer;
}

/* Function: push_arguments
   Setup the function arguments for calling a function in the inferior.

   On the Hitachi H8/300 architecture, there are three registers (R0 to R2)
   which are dedicated for passing function arguments.  Up to the first
   three arguments (depending on size) may go into these registers.
   The rest go on the stack.

   Arguments that are smaller than WORDSIZE bytes will still take up a
   whole register or a whole WORDSIZE word on the stack, and will be
   right-justified in the register or the stack word.  This includes
   chars and small aggregate types.  Note that WORDSIZE depends on the 
   cpu type.

   Arguments that are larger than WORDSIZE bytes will be split between
   two or more registers as available, but will NOT be split between a
   register and the stack.

   An exceptional case exists for struct arguments (and possibly other
   aggregates such as arrays) -- if the size is larger than WORDSIZE
   bytes but not a multiple of WORDSIZE bytes.  In this case the
   argument is never split between the registers and the stack, but
   instead is copied in its entirety onto the stack, AND also copied
   into as many registers as there is room for.  In other words, space
   in registers permitting, two copies of the same argument are passed
   in.  As far as I can tell, only the one on the stack is used,
   although that may be a function of the level of compiler
   optimization.  I suspect this is a compiler bug.  Arguments of
   these odd sizes are left-justified within the word (as opposed to
   arguments smaller than WORDSIZE bytes, which are right-justified).

   If the function is to return an aggregate type such as a struct,
   the caller must allocate space into which the callee will copy the
   return value.  In this case, a pointer to the return value location
   is passed into the callee in register R0, which displaces one of
   the other arguments passed in via registers R0 to R2.  */

CORE_ADDR
h8300_push_arguments (int nargs, struct value **args, CORE_ADDR sp,
		      unsigned char struct_return, CORE_ADDR struct_addr)
{
  int stack_align, stack_alloc, stack_offset;
  int wordsize;
  int argreg;
  int argnum;
  struct type *type;
  CORE_ADDR regval;
  char *val;
  char valbuf[4];
  int len;

  if (h8300hmode || h8300smode)
    {
      stack_align = 3;
      wordsize = 4;
    }
  else
    {
      stack_align = 1;
      wordsize = 2;
    }

  /* first force sp to a n-byte alignment */
  sp = sp & ~stack_align;

  /* Now make sure there's space on the stack */
  for (argnum = 0, stack_alloc = 0;
       argnum < nargs; argnum++)
    stack_alloc += ((TYPE_LENGTH (VALUE_TYPE (args[argnum])) + stack_align)
		    & ~stack_align);
  sp -= stack_alloc;		/* make room on stack for args */
  /* we may over-allocate a little here, but that won't hurt anything */

  argreg = ARG0_REGNUM;
  if (struct_return)		/* "struct return" pointer takes up one argreg */
    {
      write_register (argreg++, struct_addr);
    }

  /* Now load as many as possible of the first arguments into
     registers, and push the rest onto the stack.  There are 3N bytes
     in three registers available.  Loop thru args from first to last.  */

  for (argnum = 0, stack_offset = 0; argnum < nargs; argnum++)
    {
      type = VALUE_TYPE (args[argnum]);
      len = TYPE_LENGTH (type);
      memset (valbuf, 0, sizeof (valbuf));
      if (len < wordsize)
	{
	  /* the purpose of this is to right-justify the value within the word */
	  memcpy (valbuf + (wordsize - len),
		  (char *) VALUE_CONTENTS (args[argnum]), len);
	  val = valbuf;
	}
      else
	val = (char *) VALUE_CONTENTS (args[argnum]);

      if (len > (ARGLAST_REGNUM + 1 - argreg) * REGISTER_RAW_SIZE (ARG0_REGNUM) ||
	  (len > wordsize && (len & stack_align) != 0))
	{			/* passed on the stack */
	  write_memory (sp + stack_offset, val,
			len < wordsize ? wordsize : len);
	  stack_offset += (len + stack_align) & ~stack_align;
	}
      /* NOTE WELL!!!!!  This is not an "else if" clause!!!
         That's because some *&^%$ things get passed on the stack
         AND in the registers!   */
      if (len <= (ARGLAST_REGNUM + 1 - argreg) * REGISTER_RAW_SIZE (ARG0_REGNUM))
	while (len > 0)
	  {			/* there's room in registers */
	    regval = extract_address (val, wordsize);
	    write_register (argreg, regval);
	    len -= wordsize;
	    val += wordsize;
	    argreg++;
	  }
    }
  return sp;
}

/* Function: push_return_address
   Setup the return address for a dummy frame, as called by
   call_function_by_hand.  Only necessary when you are using an
   empty CALL_DUMMY, ie. the target will not actually be executing
   a JSR/BSR instruction.  */

CORE_ADDR
h8300_push_return_address (CORE_ADDR pc, CORE_ADDR sp)
{
  unsigned char buf[4];
  int wordsize;

  if (h8300hmode || h8300smode)
    wordsize = 4;
  else
    wordsize = 2;

  sp -= wordsize;
  store_unsigned_integer (buf, wordsize, CALL_DUMMY_ADDRESS ());
  write_memory (sp, buf, wordsize);
  return sp;
}

/* Function: h8300_pop_frame
   Restore the machine to the state it had before the current frame 
   was created.  Usually used either by the "RETURN" command, or by
   call_function_by_hand after the dummy_frame is finished. */

void
h8300_pop_frame (void)
{
  unsigned regnum;
  struct frame_saved_regs fsr;
  struct frame_info *frame = get_current_frame ();

  if (PC_IN_CALL_DUMMY (frame->pc, frame->frame, frame->frame))
    {
      generic_pop_dummy_frame ();
    }
  else
    {
      get_frame_saved_regs (frame, &fsr);

      for (regnum = 0; regnum < 8; regnum++)
	{
	  /* Don't forget SP_REGNUM is a frame_saved_regs struct is the
	     actual value we want, not the address of the value we want.  */
	  if (fsr.regs[regnum] && regnum != SP_REGNUM)
	    write_register (regnum,
			    read_memory_integer (fsr.regs[regnum], BINWORD));
	  else if (fsr.regs[regnum] && regnum == SP_REGNUM)
	    write_register (regnum, frame->frame + 2 * BINWORD);
	}

      /* Don't forget the update the PC too!  */
      write_pc (frame->from_pc);
    }
  flush_cached_frames ();
}

/* Function: extract_return_value
   Figure out where in REGBUF the called function has left its return value.
   Copy that into VALBUF.  Be sure to account for CPU type.   */

void
h8300_extract_return_value (struct type *type, char *regbuf, char *valbuf)
{
  int wordsize, len;

  if (h8300smode || h8300hmode)
    wordsize = 4;
  else
    wordsize = 2;

  len = TYPE_LENGTH (type);

  switch (len)
    {
    case 1:			/* (char) */
    case 2:			/* (short), (int) */
      memcpy (valbuf, regbuf + REGISTER_BYTE (0) + (wordsize - len), len);
      break;
    case 4:			/* (long), (float) */
      if (h8300smode || h8300hmode)
	{
	  memcpy (valbuf, regbuf + REGISTER_BYTE (0), 4);
	}
      else
	{
	  memcpy (valbuf, regbuf + REGISTER_BYTE (0), 2);
	  memcpy (valbuf + 2, regbuf + REGISTER_BYTE (1), 2);
	}
      break;
    case 8:			/* (double) (doesn't seem to happen, which is good,
				   because this almost certainly isn't right.  */
      error ("I don't know how a double is returned.");
      break;
    }
}

/* Function: store_return_value
   Place the appropriate value in the appropriate registers.
   Primarily used by the RETURN command.  */

void
h8300_store_return_value (struct type *type, char *valbuf)
{
  int wordsize, len, regval;

  if (h8300hmode || h8300smode)
    wordsize = 4;
  else
    wordsize = 2;

  len = TYPE_LENGTH (type);
  switch (len)
    {
    case 1:			/* char */
    case 2:			/* short, int */
      regval = extract_address (valbuf, len);
      write_register (0, regval);
      break;
    case 4:			/* long, float */
      regval = extract_address (valbuf, len);
      if (h8300smode || h8300hmode)
	{
	  write_register (0, regval);
	}
      else
	{
	  write_register (0, regval >> 16);
	  write_register (1, regval & 0xffff);
	}
      break;
    case 8:			/* presumeably double, but doesn't seem to happen */
      error ("I don't know how to return a double.");
      break;
    }
}

struct cmd_list_element *setmemorylist;

static void
set_register_names (void)
{
  if (h8300hmode != 0)
    h8300_register_names = h8300h_register_names;
  else
    h8300_register_names = original_register_names;
}

static void
h8300_command (char *args, int from_tty)
{
  extern int h8300hmode;
  h8300hmode = 0;
  h8300smode = 0;
  set_register_names ();
}

static void
h8300h_command (char *args, int from_tty)
{
  extern int h8300hmode;
  h8300hmode = 1;
  h8300smode = 0;
  set_register_names ();
}

static void
h8300s_command (char *args, int from_tty)
{
  extern int h8300smode;
  extern int h8300hmode;
  h8300smode = 1;
  h8300hmode = 1;
  set_register_names ();
}


static void
set_machine (char *args, int from_tty)
{
  printf_unfiltered ("\"set machine\" must be followed by h8300, h8300h");
  printf_unfiltered ("or h8300s");
  help_list (setmemorylist, "set memory ", -1, gdb_stdout);
}

/* set_machine_hook is called as the exec file is being opened, but
   before the symbol file is opened.  This allows us to set the
   h8300hmode flag based on the machine type specified in the exec
   file.  This in turn will cause subsequently defined pointer types
   to be 16 or 32 bits as appropriate for the machine.  */

static void
set_machine_hook (char *filename)
{
  if (bfd_get_mach (exec_bfd) == bfd_mach_h8300s)
    {
      h8300smode = 1;
      h8300hmode = 1;
    }
  else if (bfd_get_mach (exec_bfd) == bfd_mach_h8300h)
    {
      h8300smode = 0;
      h8300hmode = 1;
    }
  else
    {
      h8300smode = 0;
      h8300hmode = 0;
    }
  set_register_names ();
}

void
_initialize_h8300m (void)
{
  add_prefix_cmd ("machine", no_class, set_machine,
		  "set the machine type",
		  &setmemorylist, "set machine ", 0,
		  &setlist);

  add_cmd ("h8300", class_support, h8300_command,
	   "Set machine to be H8/300.", &setmemorylist);

  add_cmd ("h8300h", class_support, h8300h_command,
	   "Set machine to be H8/300H.", &setmemorylist);

  add_cmd ("h8300s", class_support, h8300s_command,
	   "Set machine to be H8/300S.", &setmemorylist);

  /* Add a hook to set the machine type when we're loading a file. */

  specify_exec_file_hook (set_machine_hook);
}



void
h8300_print_register_hook (int regno)
{
  if (regno == 8)
    {
      /* CCR register */
      int C, Z, N, V;
      unsigned char b[4];
      unsigned char l;
      read_relative_register_raw_bytes (regno, b);
      l = b[REGISTER_VIRTUAL_SIZE (8) - 1];
      printf_unfiltered ("\t");
      printf_unfiltered ("I-%d - ", (l & 0x80) != 0);
      printf_unfiltered ("H-%d - ", (l & 0x20) != 0);
      N = (l & 0x8) != 0;
      Z = (l & 0x4) != 0;
      V = (l & 0x2) != 0;
      C = (l & 0x1) != 0;
      printf_unfiltered ("N-%d ", N);
      printf_unfiltered ("Z-%d ", Z);
      printf_unfiltered ("V-%d ", V);
      printf_unfiltered ("C-%d ", C);
      if ((C | Z) == 0)
	printf_unfiltered ("u> ");
      if ((C | Z) == 1)
	printf_unfiltered ("u<= ");
      if ((C == 0))
	printf_unfiltered ("u>= ");
      if (C == 1)
	printf_unfiltered ("u< ");
      if (Z == 0)
	printf_unfiltered ("!= ");
      if (Z == 1)
	printf_unfiltered ("== ");
      if ((N ^ V) == 0)
	printf_unfiltered (">= ");
      if ((N ^ V) == 1)
	printf_unfiltered ("< ");
      if ((Z | (N ^ V)) == 0)
	printf_unfiltered ("> ");
      if ((Z | (N ^ V)) == 1)
	printf_unfiltered ("<= ");
    }
}

void
_initialize_h8300_tdep (void)
{
  tm_print_insn = gdb_print_insn_h8300;
}
