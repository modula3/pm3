/* Parameters for execution on an NEC V850 processor.
   Copyright 1996, 1997, 1998, 1999, 2000
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

#include "regcache.h"

#define TARGET_BYTE_ORDER LITTLE_ENDIAN

#define NUM_REGS 66

#define REGISTER_NAMES \
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", \
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", \
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31", \
    \
  "eipc", "eipsw", "fepc", "fepsw", "ecr", "psw", "sr6", "sr7", \
  "sr8", "sr9", "sr10", "sr11", "sr12", "sr13", "sr14", "sr15", \
  "sr16", "sr17", "sr18", "sr19", "sr20", "sr21", "sr22", "sr23", \
  "sr24", "sr25", "sr26", "sr27", "sr28", "sr29", "sr30", "sr31", \
    \
  "pc", "fp" }

/* Initializer for an array of names of registers.
   Entries beyond the first NUM_REGS are ignored.  */

extern char **v850_register_names;
#define REGISTER_NAME(i) v850_register_names[i]


#define REGISTER_BYTES (NUM_REGS * 4)

#define REGISTER_SIZE 4
#define MAX_REGISTER_RAW_SIZE 4

#define R0_REGNUM 0
#define R1_REGNUM 1
#define SAVE1_START_REGNUM 2
#define SAVE1_END_REGNUM 2
#define SP_REGNUM 3
#define ARG0_REGNUM 6
#define ARGLAST_REGNUM 9
#define V0_REGNUM 10
#define V1_REGNUM 11
#define R12_REGNUM 12
#define SAVE2_START_REGNUM 20
#define SAVE2_END_REGNUM 29
#define EP_REGNUM 30
#define SAVE3_START_REGNUM 31
#define SAVE3_END_REGNUM 31
#define RP_REGNUM 31
#define SR0_REGNUM 32
#define PS_REGNUM (SR0_REGNUM+5)
#define CTBP_REGNUM (SR0_REGNUM+20)
#define PC_REGNUM 64
#define FP_REGNUM 65
#define FP_RAW_REGNUM 29

#define TARGET_READ_FP() read_register (FP_RAW_REGNUM)
#define TARGET_WRITE_FP(VAL) write_register (FP_REGNUM, (VAL))

#define REGISTER_VIRTUAL_TYPE(REG) builtin_type_int

#define REGISTER_BYTE(REG) ((REG) * 4)
#define REGISTER_VIRTUAL_SIZE(REG) 4
#define REGISTER_RAW_SIZE(REG) 4

#define MAX_REGISTER_VIRTUAL_SIZE 4

#define BREAKPOINT {0x85, 0x05} /* little-ended */

#define FUNCTION_START_OFFSET 0

#define DECR_PC_AFTER_BREAK 0

#define INNER_THAN(lhs,rhs) ((lhs) < (rhs))

#define SAVED_PC_AFTER_CALL(fi) read_register (RP_REGNUM)

struct frame_info;
struct frame_saved_regs;
struct type;
struct value;

#define EXTRA_FRAME_INFO struct frame_saved_regs fsr;

extern void v850_init_extra_frame_info (struct frame_info *fi);
#define INIT_EXTRA_FRAME_INFO(fromleaf, fi) v850_init_extra_frame_info (fi)
#define INIT_FRAME_PC		/* Not necessary */

extern void v850_frame_find_saved_regs (struct frame_info *fi,
					struct frame_saved_regs *regaddr);
#define FRAME_FIND_SAVED_REGS(fi, regaddr) regaddr = fi->fsr

extern CORE_ADDR v850_frame_chain (struct frame_info *fi);
#define FRAME_CHAIN(fi) v850_frame_chain (fi)
#define FRAME_CHAIN_VALID(FP, FI)	generic_file_frame_chain_valid (FP, FI)

extern CORE_ADDR v850_find_callers_reg (struct frame_info *fi, int regnum);
extern CORE_ADDR v850_frame_saved_pc (struct frame_info *);
#define FRAME_SAVED_PC(FI) (v850_frame_saved_pc (FI))

#define EXTRACT_RETURN_VALUE(TYPE, REGBUF, VALBUF) \
  memcpy (VALBUF, REGBUF + REGISTER_BYTE (V0_REGNUM), TYPE_LENGTH (TYPE))

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) \
  extract_address (REGBUF + REGISTER_BYTE (V0_REGNUM), \
		   REGISTER_RAW_SIZE (V0_REGNUM))

#define STORE_RETURN_VALUE(TYPE, VALBUF) \
  write_register_bytes(REGISTER_BYTE (V0_REGNUM), VALBUF, TYPE_LENGTH (TYPE));

extern CORE_ADDR v850_skip_prologue (CORE_ADDR pc);
#define SKIP_PROLOGUE(pc) (v850_skip_prologue (pc))

#define FRAME_ARGS_SKIP 0

#define FRAME_ARGS_ADDRESS(fi) ((fi)->frame)
#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)
#define FRAME_NUM_ARGS(fi) (-1)

extern void v850_pop_frame (struct frame_info *frame);
#define POP_FRAME v850_pop_frame (get_current_frame ())

#define USE_GENERIC_DUMMY_FRAMES 1
#define CALL_DUMMY                   {0}
#define CALL_DUMMY_START_OFFSET      (0)
#define CALL_DUMMY_BREAKPOINT_OFFSET (0)
#define CALL_DUMMY_LOCATION          AT_ENTRY_POINT
#define FIX_CALL_DUMMY(DUMMY, START, FUNADDR, NARGS, ARGS, TYPE, GCCP)
#define CALL_DUMMY_ADDRESS()         entry_point_address ()
extern CORE_ADDR v850_push_return_address (CORE_ADDR, CORE_ADDR);
#define PUSH_RETURN_ADDRESS(PC, SP)  v850_push_return_address (PC, SP)


#define PUSH_DUMMY_FRAME	generic_push_dummy_frame ()

extern CORE_ADDR
v850_push_arguments (int nargs, struct value **args, CORE_ADDR sp,
		     unsigned char struct_return, CORE_ADDR struct_addr);
#define PUSH_ARGUMENTS(NARGS, ARGS, SP, STRUCT_RETURN, STRUCT_ADDR) \
  (v850_push_arguments (NARGS, ARGS, SP, STRUCT_RETURN, STRUCT_ADDR))

#define STORE_STRUCT_RETURN(STRUCT_ADDR, SP)


#define PC_IN_CALL_DUMMY(PC, SP, FP) generic_pc_in_call_dummy (PC, SP, FP)

extern use_struct_convention_fn v850_use_struct_convention;
#define USE_STRUCT_CONVENTION(GCC_P, TYPE) v850_use_struct_convention (GCC_P, TYPE);

/* override the default get_saved_register function with
   one that takes account of generic CALL_DUMMY frames */
#define GET_SAVED_REGISTER(raw_buffer, optimized, addrp, frame, regnum, lval) \
      generic_get_saved_register (raw_buffer, optimized, addrp, frame, regnum, lval)

/* Define this for Wingdb */

#define TARGET_V850
