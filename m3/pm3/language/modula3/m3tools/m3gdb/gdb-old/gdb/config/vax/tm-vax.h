/* Definitions to make GDB run on a vax under 4.2bsd.
   Copyright 1986, 1987, 1989, 1991, 1993 Free Software Foundation, Inc.

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


#define TARGET_BYTE_ORDER LITTLE_ENDIAN

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 2

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc)	\
{ register int op = (unsigned char) read_memory_integer (pc, 1);  \
  if (op == 0x11) pc += 2;  /* skip brb */			  \
  if (op == 0x31) pc += 3;  /* skip brw */			  \
  if (op == 0xC2 &&						  \
      ((unsigned char) read_memory_integer (pc+2, 1)) == 0x5E)	  \
    pc += 3;  /* skip subl2 */					  \
  if (op == 0x9E &&						  \
      ((unsigned char) read_memory_integer (pc+1, 1)) == 0xAE &&  \
      ((unsigned char) read_memory_integer(pc+3, 1)) == 0x5E)	  \
     pc += 4;  /* skip movab */					  \
  if (op == 0x9E &&						  \
      ((unsigned char) read_memory_integer (pc+1, 1)) == 0xCE &&  \
      ((unsigned char) read_memory_integer(pc+4, 1)) == 0x5E)	  \
    pc += 5;  /* skip movab */					  \
  if (op == 0x9E &&						  \
      ((unsigned char) read_memory_integer (pc+1, 1)) == 0xEE &&  \
      ((unsigned char) read_memory_integer(pc+6, 1)) == 0x5E)	  \
    pc += 7;  /* skip movab */					  \
}

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) FRAME_SAVED_PC(frame)

#define TARGET_UPAGES 14
#define TARGET_NBPG 512
#define STACK_END_ADDR (0x80000000 - (TARGET_UPAGES * TARGET_NBPG))

/* On the VAX, sigtramp is in the u area.  Can't check the exact
   addresses because for cross-debugging we don't have VAX include
   files around.  This should be close enough.  */
#define SIGTRAMP_START(pc)	STACK_END_ADDR
#define SIGTRAMP_END(pc)	0x80000000

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {3}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */

#define ABOUT_TO_RETURN(pc) (read_memory_integer (pc, 1) == 04)

/* Return 1 if P points to an invalid floating point value.
   LEN is the length in bytes -- not relevant on the Vax.  */

#define INVALID_FLOAT(p, len) ((*(short *) p & 0xff80) == 0x8000)

/* Say how long (ordinary) registers are.  This is a piece of bogosity
   used in push_word and a few other places; REGISTER_RAW_SIZE is the
   real way to know how big a register is.  */

#define REGISTER_SIZE 4

/* Number of machine registers */

#define NUM_REGS 17

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "ap", "fp", "sp", "pc", "ps"}

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define AP_REGNUM 12
#define FP_REGNUM 13		/* Contains address of executing stack frame */
#define SP_REGNUM 14		/* Contains address of top of stack */
#define PC_REGNUM 15		/* Contains program counter */
#define PS_REGNUM 16		/* Contains processor status */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES (17*4)

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) ((N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the vax, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) 4

/* Number of bytes of storage in the program's representation
   for register N.  On the vax, all regs are 4 bytes.  */

#define REGISTER_VIRTUAL_SIZE(N) 4

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 4

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 4

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) builtin_type_int

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
  { write_register (1, (ADDR)); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  memcpy (VALBUF, REGBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (0, VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer. */

/* In the case of the Vax, the frame's nominal address is the FP value,
   and 12 bytes later comes the saved previous FP value as a 4-byte word.  */

#define FRAME_CHAIN(thisframe)  \
  (!inside_entry_file ((thisframe)->pc) ? \
   read_memory_integer ((thisframe)->frame + 12, 4) :\
   0)

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
/* On the vax, all functions have frames.  */
#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS)  {(FRAMELESS) = 0;}

/* Saved Pc.  Get it from sigcontext if within sigtramp.  */

/* Offset to saved PC in sigcontext, from <sys/signal.h>.  */
#define SIGCONTEXT_PC_OFFSET 12

#define FRAME_SAVED_PC(FRAME) \
  (((FRAME)->signal_handler_caller \
    ? sigtramp_saved_pc (FRAME) \
    : read_memory_integer ((FRAME)->frame + 16, 4)) \
   )

/* Cannot find the AP register value directly from the FP value.  Must
   find it saved in the frame called by this one, or in the AP
   register for the innermost frame.  However, there is no way to tell
   the difference between the innermost frame and a frame for which we
   just don't know the frame that it called (e.g. "info frame
   0x7ffec789").  For the sake of argument suppose that the stack is
   somewhat trashed (which is one reason that "info frame" exists).
   So return 0 (indicating we don't know the address of
   the arglist) if we don't know what frame this frame calls.  */
#define FRAME_ARGS_ADDRESS_CORRECT(fi) \
 (((fi)->next                                  \
   ? read_memory_integer ((fi)->next->frame + 8, 4)   \
   : /* read_register (AP_REGNUM) */ 0))

/* In most of GDB, getting the args address is too important to
   just say "I don't know".  This is sometimes wrong for functions
   that aren't on top of the stack, but c'est la vie.  */
#define FRAME_ARGS_ADDRESS(fi) \
 (((fi)->next                                  \
   ? read_memory_integer ((fi)->next->frame + 8, 4)   \
   : read_register (AP_REGNUM) /* 0 */))

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(numargs, fi)  \
{ numargs = (0xff & read_memory_integer (FRAME_ARGS_ADDRESS (fi), 1)); }

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 4

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs) \
{ register int regnum;     \
  register int regmask = read_memory_integer ((frame_info)->frame+4, 4) >> 16; \
  register CORE_ADDR next_addr;     \
  memset (&frame_saved_regs, '\0', sizeof frame_saved_regs);     \
  next_addr = (frame_info)->frame + 16;     \
  /* Regmask's low bit is for register 0,     \
     which is the first one that would be pushed.  */     \
  for (regnum = 0; regnum < 12; regnum++, regmask >>= 1)  \
    (frame_saved_regs).regs[regnum] = (regmask & 1) ? (next_addr += 4) : 0;  \
  (frame_saved_regs).regs[SP_REGNUM] = next_addr + 4;  \
  if (read_memory_integer ((frame_info)->frame + 4, 4) & 0x20000000)   \
    (frame_saved_regs).regs[SP_REGNUM] += 4 + 4 * read_memory_integer (next_addr + 4, 4);  \
  (frame_saved_regs).regs[PC_REGNUM] = (frame_info)->frame + 16;  \
  (frame_saved_regs).regs[FP_REGNUM] = (frame_info)->frame + 12;  \
  (frame_saved_regs).regs[AP_REGNUM] = (frame_info)->frame + 8;  \
  (frame_saved_regs).regs[PS_REGNUM] = (frame_info)->frame + 4;  \
}

/* Things needed for making the inferior call functions.  */

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME \
{ register CORE_ADDR sp = read_register (SP_REGNUM);\
  register int regnum;				    \
  sp = push_word (sp, 0); /* arglist */		    \
  for (regnum = 11; regnum >= 0; regnum--)	    \
    sp = push_word (sp, read_register (regnum));    \
  sp = push_word (sp, read_register (PC_REGNUM));   \
  sp = push_word (sp, read_register (FP_REGNUM));   \
  sp = push_word (sp, read_register (AP_REGNUM));   \
  sp = push_word (sp, (read_register (PS_REGNUM) & 0xffef)   \
		      + 0x2fff0000);		    \
  sp = push_word (sp, 0); 			    \
  write_register (SP_REGNUM, sp);		    \
  write_register (FP_REGNUM, sp);		    \
  write_register (AP_REGNUM, sp + 17 * sizeof (int)); }

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME  \
{ register CORE_ADDR fp = read_register (FP_REGNUM);		 \
  register int regnum;						 \
  register int regmask = read_memory_integer (fp + 4, 4);	 \
  write_register (PS_REGNUM, 					 \
		  (regmask & 0xffff)				 \
		  | (read_register (PS_REGNUM) & 0xffff0000));	 \
  write_register (PC_REGNUM, read_memory_integer (fp + 16, 4));  \
  write_register (FP_REGNUM, read_memory_integer (fp + 12, 4));  \
  write_register (AP_REGNUM, read_memory_integer (fp + 8, 4));   \
  fp += 16;							 \
  for (regnum = 0; regnum < 12; regnum++)			 \
    if (regmask & (0x10000 << regnum))				 \
      write_register (regnum, read_memory_integer (fp += 4, 4)); \
  fp = fp + 4 + ((regmask >> 30) & 3);				 \
  if (regmask & 0x20000000)					 \
    { regnum = read_memory_integer (fp, 4);			 \
      fp += (regnum + 1) * 4; }					 \
  write_register (SP_REGNUM, fp);				 \
  flush_cached_frames ();					 \
}

/* This sequence of words is the instructions
     calls #69, @#32323232
     bpt
   Note this is 8 bytes.  */

#define CALL_DUMMY {0x329f69fb, 0x03323232}

#define CALL_DUMMY_START_OFFSET 0  /* Start execution at beginning of dummy */

#define CALL_DUMMY_BREAKPOINT_OFFSET 7

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, type, gcc_p)   \
{ *((char *) dummyname + 1) = nargs;		\
  *(int *)((char *) dummyname + 3) = fun; }

/* If vax pcc says CHAR or SHORT, it provides the correct address.  */

#define BELIEVE_PCC_PROMOTION 1
