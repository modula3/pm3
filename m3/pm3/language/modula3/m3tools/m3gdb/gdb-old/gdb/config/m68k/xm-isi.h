/* Definitions to make GDB run on an ISI Optimum V (3.05) under 4.3bsd.
   Copyright 1987, 1989, 1992 Free Software Foundation, Inc.

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

#define HOST_BYTE_ORDER BIG_ENDIAN

/* This has not been tested on ISI's running BSD 4.2, but it will probably
   work.  */

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

/*#define KERNEL_U_ADDR 0x10800000*/
#define KERNEL_U_ADDR 0

/* expects blockend to be u.u_ar0 */
extern int rloc[];  	/* Defined in isi-dep.c */
#define REGISTER_U_ADDR(addr, blockend, regno)                  \
{       blockend &= UPAGES*NBPG - 1;                            \
	if (regno < 18) addr = (int)blockend + rloc[regno]*4;  	\
        else if (regno < 26) addr = (int) &((struct user *)0)->u_68881_regs \
            + (regno - 18) * 12;                                \
        else if (regno < 29) addr = (int) &((struct user *)0)->u_68881_regs \
            + 8 * 12 + (regno - 26) * 4;                        \
}

/* Interface definitions for kernel debugger KDB.  */

/* Map machine fault codes into signal numbers.
   First subtract 0, divide by 4, then index in a table.
   Faults for which the entry in this table is 0
   are not handled by KDB; the program's own trap handler
   gets to handle then.  */

#define FAULT_CODE_ORIGIN 0
#define FAULT_CODE_UNITS 4
#define FAULT_TABLE    \
{ 0, 0, 0, 0, SIGTRAP, 0, 0, 0, \
  0, SIGTRAP, 0, 0, 0, 0, 0, SIGKILL, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  SIGILL }

/* Start running with a stack stretching from BEG to END.
   BEG and END should be symbols meaningful to the assembler.
   This is used only for kdb.  */

#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("movl $ end, sp");      \
  asm ("clrl fp"); }

/* Push the frame pointer register on the stack.  */
#define PUSH_FRAME_PTR        \
  asm ("movel fp, -(sp)");

/* Copy the top-of-stack to the frame pointer register.  */
#define POP_FRAME_PTR  \
  asm ("movl (sp), fp");

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#define PUSH_REGISTERS        \
{ asm ("clrw -(sp)");	      \
  asm ("pea 10(sp)");	      \
  asm ("movem $ 0xfffe,-(sp)"); }

/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#define POP_REGISTERS          \
{ asm ("subil $8,28(sp)");     \
  asm ("movem (sp),$ 0xffff"); \
  asm ("rte"); }
