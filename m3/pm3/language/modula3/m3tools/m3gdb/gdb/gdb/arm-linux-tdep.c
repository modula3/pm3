/* GNU/Linux on ARM target support.
   Copyright 1999, 2000, 2001 Free Software Foundation, Inc.

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
#include "target.h"
#include "value.h"
#include "gdbtypes.h"
#include "floatformat.h"
#include "gdbcore.h"
#include "frame.h"
#include "regcache.h"

/* For arm_linux_skip_solib_resolver.  */
#include "symtab.h"
#include "symfile.h"
#include "objfiles.h"

#ifdef GET_LONGJMP_TARGET

/* Figure out where the longjmp will land.  We expect that we have
   just entered longjmp and haven't yet altered r0, r1, so the
   arguments are still in the registers.  (A1_REGNUM) points at the
   jmp_buf structure from which we extract the pc (JB_PC) that we will
   land at.  The pc is copied into ADDR.  This routine returns true on
   success. */

#define LONGJMP_TARGET_SIZE 	sizeof(int)
#define JB_ELEMENT_SIZE		sizeof(int)
#define JB_SL			18
#define JB_FP			19
#define JB_SP			20
#define JB_PC			21

int
arm_get_longjmp_target (CORE_ADDR * pc)
{
  CORE_ADDR jb_addr;
  char buf[LONGJMP_TARGET_SIZE];

  jb_addr = read_register (A1_REGNUM);

  if (target_read_memory (jb_addr + JB_PC * JB_ELEMENT_SIZE, buf,
			  LONGJMP_TARGET_SIZE))
    return 0;

  *pc = extract_address (buf, LONGJMP_TARGET_SIZE);
  return 1;
}

#endif /* GET_LONGJMP_TARGET */

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

void
arm_linux_extract_return_value (struct type *type,
				char regbuf[REGISTER_BYTES],
				char *valbuf)
{
  /* ScottB: This needs to be looked at to handle the different
     floating point emulators on ARM Linux.  Right now the code
     assumes that fetch inferior registers does the right thing for
     GDB.  I suspect this won't handle NWFPE registers correctly, nor
     will the default ARM version (arm_extract_return_value()).  */

  int regnum = (TYPE_CODE_FLT == TYPE_CODE (type)) ? F0_REGNUM : A1_REGNUM;
  memcpy (valbuf, &regbuf[REGISTER_BYTE (regnum)], TYPE_LENGTH (type));
}

/* Note: ScottB

   This function does not support passing parameters using the FPA
   variant of the APCS.  It passes any floating point arguments in the
   general registers and/or on the stack.
   
   FIXME:  This and arm_push_arguments should be merged.  However this 
   	   function breaks on a little endian host, big endian target
   	   using the COFF file format.  ELF is ok.  
   	   
   	   ScottB.  */
   	   
/* Addresses for calling Thumb functions have the bit 0 set.
   Here are some macros to test, set, or clear bit 0 of addresses.  */
#define IS_THUMB_ADDR(addr)	((addr) & 1)
#define MAKE_THUMB_ADDR(addr)	((addr) | 1)
#define UNMAKE_THUMB_ADDR(addr) ((addr) & ~1)
   	  
CORE_ADDR
arm_linux_push_arguments (int nargs, struct value **args, CORE_ADDR sp,
		          int struct_return, CORE_ADDR struct_addr)
{
  char *fp;
  int argnum, argreg, nstack_size;

  /* Walk through the list of args and determine how large a temporary
     stack is required.  Need to take care here as structs may be
     passed on the stack, and we have to to push them.  */
  nstack_size = -4 * REGISTER_SIZE;	/* Some arguments go into A1-A4.  */

  if (struct_return)			/* The struct address goes in A1.  */
    nstack_size += REGISTER_SIZE;

  /* Walk through the arguments and add their size to nstack_size.  */
  for (argnum = 0; argnum < nargs; argnum++)
    {
      int len;
      struct type *arg_type;

      arg_type = check_typedef (VALUE_TYPE (args[argnum]));
      len = TYPE_LENGTH (arg_type);

      /* ANSI C code passes float arguments as integers, K&R code
         passes float arguments as doubles.  Correct for this here.  */
      if (TYPE_CODE_FLT == TYPE_CODE (arg_type) && REGISTER_SIZE == len)
	nstack_size += FP_REGISTER_VIRTUAL_SIZE;
      else
	nstack_size += len;
    }

  /* Allocate room on the stack, and initialize our stack frame
     pointer.  */
  fp = NULL;
  if (nstack_size > 0)
    {
      sp -= nstack_size;
      fp = (char *) sp;
    }

  /* Initialize the integer argument register pointer.  */
  argreg = A1_REGNUM;

  /* The struct_return pointer occupies the first parameter passing
     register.  */
  if (struct_return)
    write_register (argreg++, struct_addr);

  /* Process arguments from left to right.  Store as many as allowed
     in the parameter passing registers (A1-A4), and save the rest on
     the temporary stack.  */
  for (argnum = 0; argnum < nargs; argnum++)
    {
      int len;
      char *val;
      double dbl_arg;
      CORE_ADDR regval;
      enum type_code typecode;
      struct type *arg_type, *target_type;

      arg_type = check_typedef (VALUE_TYPE (args[argnum]));
      target_type = TYPE_TARGET_TYPE (arg_type);
      len = TYPE_LENGTH (arg_type);
      typecode = TYPE_CODE (arg_type);
      val = (char *) VALUE_CONTENTS (args[argnum]);

      /* ANSI C code passes float arguments as integers, K&R code
         passes float arguments as doubles.  The .stabs record for 
         for ANSI prototype floating point arguments records the
         type as FP_INTEGER, while a K&R style (no prototype)
         .stabs records the type as FP_FLOAT.  In this latter case
         the compiler converts the float arguments to double before
         calling the function.  */
      if (TYPE_CODE_FLT == typecode && REGISTER_SIZE == len)
	{
	  /* Float argument in buffer is in host format.  Read it and 
	     convert to DOUBLEST, and store it in target double.  */
	  DOUBLEST dblval;
	  
	  len = TARGET_DOUBLE_BIT / TARGET_CHAR_BIT;
	  floatformat_to_doublest (HOST_FLOAT_FORMAT, val, &dblval);
	  store_floating (&dbl_arg, len, dblval);
	  val = (char *) &dbl_arg;
	}

      /* If the argument is a pointer to a function, and it is a Thumb
         function, set the low bit of the pointer.  */
      if (TYPE_CODE_PTR == typecode
	  && NULL != target_type
	  && TYPE_CODE_FUNC == TYPE_CODE (target_type))
	{
	  CORE_ADDR regval = extract_address (val, len);
	  if (arm_pc_is_thumb (regval))
	    store_address (val, len, MAKE_THUMB_ADDR (regval));
	}

      /* Copy the argument to general registers or the stack in
         register-sized pieces.  Large arguments are split between
         registers and stack.  */
      while (len > 0)
	{
	  int partial_len = len < REGISTER_SIZE ? len : REGISTER_SIZE;

	  if (argreg <= ARM_LAST_ARG_REGNUM)
	    {
	      /* It's an argument being passed in a general register.  */
	      regval = extract_address (val, partial_len);
	      write_register (argreg++, regval);
	    }
	  else
	    {
	      /* Push the arguments onto the stack.  */
	      write_memory ((CORE_ADDR) fp, val, REGISTER_SIZE);
	      fp += REGISTER_SIZE;
	    }

	  len -= partial_len;
	  val += partial_len;
	}
    }

  /* Return adjusted stack pointer.  */
  return sp;
}

/*
   Dynamic Linking on ARM Linux
   ----------------------------

   Note: PLT = procedure linkage table
   GOT = global offset table

   As much as possible, ELF dynamic linking defers the resolution of
   jump/call addresses until the last minute. The technique used is
   inspired by the i386 ELF design, and is based on the following
   constraints.

   1) The calling technique should not force a change in the assembly
   code produced for apps; it MAY cause changes in the way assembly
   code is produced for position independent code (i.e. shared
   libraries).

   2) The technique must be such that all executable areas must not be
   modified; and any modified areas must not be executed.

   To do this, there are three steps involved in a typical jump:

   1) in the code
   2) through the PLT
   3) using a pointer from the GOT

   When the executable or library is first loaded, each GOT entry is
   initialized to point to the code which implements dynamic name
   resolution and code finding.  This is normally a function in the
   program interpreter (on ARM Linux this is usually ld-linux.so.2,
   but it does not have to be).  On the first invocation, the function
   is located and the GOT entry is replaced with the real function
   address.  Subsequent calls go through steps 1, 2 and 3 and end up
   calling the real code.

   1) In the code: 

   b    function_call
   bl   function_call

   This is typical ARM code using the 26 bit relative branch or branch
   and link instructions.  The target of the instruction
   (function_call is usually the address of the function to be called.
   In position independent code, the target of the instruction is
   actually an entry in the PLT when calling functions in a shared
   library.  Note that this call is identical to a normal function
   call, only the target differs.

   2) In the PLT:

   The PLT is a synthetic area, created by the linker. It exists in
   both executables and libraries. It is an array of stubs, one per
   imported function call. It looks like this:

   PLT[0]:
   str     lr, [sp, #-4]!       @push the return address (lr)
   ldr     lr, [pc, #16]   @load from 6 words ahead
   add     lr, pc, lr      @form an address for GOT[0]
   ldr     pc, [lr, #8]!   @jump to the contents of that addr

   The return address (lr) is pushed on the stack and used for
   calculations.  The load on the second line loads the lr with
   &GOT[3] - . - 20.  The addition on the third leaves:

   lr = (&GOT[3] - . - 20) + (. + 8)
   lr = (&GOT[3] - 12)
   lr = &GOT[0]

   On the fourth line, the pc and lr are both updated, so that:

   pc = GOT[2]
   lr = &GOT[0] + 8
   = &GOT[2]

   NOTE: PLT[0] borrows an offset .word from PLT[1]. This is a little
   "tight", but allows us to keep all the PLT entries the same size.

   PLT[n+1]:
   ldr     ip, [pc, #4]    @load offset from gotoff
   add     ip, pc, ip      @add the offset to the pc
   ldr     pc, [ip]        @jump to that address
   gotoff: .word   GOT[n+3] - .

   The load on the first line, gets an offset from the fourth word of
   the PLT entry.  The add on the second line makes ip = &GOT[n+3],
   which contains either a pointer to PLT[0] (the fixup trampoline) or
   a pointer to the actual code.

   3) In the GOT:

   The GOT contains helper pointers for both code (PLT) fixups and
   data fixups.  The first 3 entries of the GOT are special. The next
   M entries (where M is the number of entries in the PLT) belong to
   the PLT fixups. The next D (all remaining) entries belong to
   various data fixups. The actual size of the GOT is 3 + M + D.

   The GOT is also a synthetic area, created by the linker. It exists
   in both executables and libraries.  When the GOT is first
   initialized , all the GOT entries relating to PLT fixups are
   pointing to code back at PLT[0].

   The special entries in the GOT are:

   GOT[0] = linked list pointer used by the dynamic loader
   GOT[1] = pointer to the reloc table for this module
   GOT[2] = pointer to the fixup/resolver code

   The first invocation of function call comes through and uses the
   fixup/resolver code.  On the entry to the fixup/resolver code:

   ip = &GOT[n+3]
   lr = &GOT[2]
   stack[0] = return address (lr) of the function call
   [r0, r1, r2, r3] are still the arguments to the function call

   This is enough information for the fixup/resolver code to work
   with.  Before the fixup/resolver code returns, it actually calls
   the requested function and repairs &GOT[n+3].  */

/* Find the minimal symbol named NAME, and return both the minsym
   struct and its objfile.  This probably ought to be in minsym.c, but
   everything there is trying to deal with things like C++ and
   SOFUN_ADDRESS_MAYBE_TURQUOISE, ...  Since this is so simple, it may
   be considered too special-purpose for general consumption.  */

static struct minimal_symbol *
find_minsym_and_objfile (char *name, struct objfile **objfile_p)
{
  struct objfile *objfile;

  ALL_OBJFILES (objfile)
    {
      struct minimal_symbol *msym;

      ALL_OBJFILE_MSYMBOLS (objfile, msym)
	{
	  if (SYMBOL_NAME (msym)
	      && STREQ (SYMBOL_NAME (msym), name))
	    {
	      *objfile_p = objfile;
	      return msym;
	    }
	}
    }

  return 0;
}


static CORE_ADDR
skip_hurd_resolver (CORE_ADDR pc)
{
  /* The HURD dynamic linker is part of the GNU C library, so many
     GNU/Linux distributions use it.  (All ELF versions, as far as I
     know.)  An unresolved PLT entry points to "_dl_runtime_resolve",
     which calls "fixup" to patch the PLT, and then passes control to
     the function.

     We look for the symbol `_dl_runtime_resolve', and find `fixup' in
     the same objfile.  If we are at the entry point of `fixup', then
     we set a breakpoint at the return address (at the top of the
     stack), and continue.
  
     It's kind of gross to do all these checks every time we're
     called, since they don't change once the executable has gotten
     started.  But this is only a temporary hack --- upcoming versions
     of Linux will provide a portable, efficient interface for
     debugging programs that use shared libraries.  */

  struct objfile *objfile;
  struct minimal_symbol *resolver 
    = find_minsym_and_objfile ("_dl_runtime_resolve", &objfile);

  if (resolver)
    {
      struct minimal_symbol *fixup
	= lookup_minimal_symbol ("fixup", 0, objfile);

      if (fixup && SYMBOL_VALUE_ADDRESS (fixup) == pc)
	return (SAVED_PC_AFTER_CALL (get_current_frame ()));
    }

  return 0;
}      

/* See the comments for SKIP_SOLIB_RESOLVER at the top of infrun.c.
   This function:
   1) decides whether a PLT has sent us into the linker to resolve
      a function reference, and 
   2) if so, tells us where to set a temporary breakpoint that will
      trigger when the dynamic linker is done.  */

CORE_ADDR
arm_linux_skip_solib_resolver (CORE_ADDR pc)
{
  CORE_ADDR result;

  /* Plug in functions for other kinds of resolvers here.  */
  result = skip_hurd_resolver (pc);

  if (result)
    return result;
  
  return 0;
}

/* The constants below were determined by examining the following files
   in the linux kernel sources:

      arch/arm/kernel/signal.c
	  - see SWI_SYS_SIGRETURN and SWI_SYS_RT_SIGRETURN
      include/asm-arm/unistd.h
	  - see __NR_sigreturn, __NR_rt_sigreturn, and __NR_SYSCALL_BASE */

#define ARM_LINUX_SIGRETURN_INSTR	0xef900077
#define ARM_LINUX_RT_SIGRETURN_INSTR	0xef9000ad

/* arm_linux_in_sigtramp determines if PC points at one of the
   instructions which cause control to return to the Linux kernel upon
   return from a signal handler.  FUNC_NAME is unused.  */

int
arm_linux_in_sigtramp (CORE_ADDR pc, char *func_name)
{
  unsigned long inst;

  inst = read_memory_integer (pc, 4);

  return (inst == ARM_LINUX_SIGRETURN_INSTR
	  || inst == ARM_LINUX_RT_SIGRETURN_INSTR);

}

/* arm_linux_sigcontext_register_address returns the address in the
   sigcontext of register REGNO given a stack pointer value SP and
   program counter value PC.  The value 0 is returned if PC is not
   pointing at one of the signal return instructions or if REGNO is
   not saved in the sigcontext struct.  */

CORE_ADDR
arm_linux_sigcontext_register_address (CORE_ADDR sp, CORE_ADDR pc, int regno)
{
  unsigned long inst;
  CORE_ADDR reg_addr = 0;

  inst = read_memory_integer (pc, 4);

  if (inst == ARM_LINUX_SIGRETURN_INSTR || inst == ARM_LINUX_RT_SIGRETURN_INSTR)
    {
      CORE_ADDR sigcontext_addr;

      /* The sigcontext structure is at different places for the two
         signal return instructions.  For ARM_LINUX_SIGRETURN_INSTR,
	 it starts at the SP value.  For ARM_LINUX_RT_SIGRETURN_INSTR,
	 it is at SP+8.  For the latter instruction, it may also be
	 the case that the address of this structure may be determined
	 by reading the 4 bytes at SP, but I'm not convinced this is
	 reliable.

	 In any event, these magic constants (0 and 8) may be
	 determined by examining struct sigframe and struct
	 rt_sigframe in arch/arm/kernel/signal.c in the Linux kernel
	 sources.  */

      if (inst == ARM_LINUX_RT_SIGRETURN_INSTR)
	sigcontext_addr = sp + 8;
      else /* inst == ARM_LINUX_SIGRETURN_INSTR */
        sigcontext_addr = sp + 0;

      /* The layout of the sigcontext structure for ARM GNU/Linux is
         in include/asm-arm/sigcontext.h in the Linux kernel sources.

	 There are three 4-byte fields which precede the saved r0
	 field.  (This accounts for the 12 in the code below.)  The
	 sixteen registers (4 bytes per field) follow in order.  The
	 PSR value follows the sixteen registers which accounts for
	 the constant 19 below. */

      if (0 <= regno && regno <= PC_REGNUM)
	reg_addr = sigcontext_addr + 12 + (4 * regno);
      else if (regno == PS_REGNUM)
	reg_addr = sigcontext_addr + 19 * 4;
    }

  return reg_addr;
}

void
_initialize_arm_linux_tdep (void)
{
}
