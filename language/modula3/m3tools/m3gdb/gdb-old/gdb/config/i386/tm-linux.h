/* Definitions to target GDB to GNU/Linux on 386.
   Copyright 1992, 1993 Free Software Foundation, Inc.

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

#ifndef TM_LINUX_H
#define TM_LINUX_H

#define I386_LINUX_TARGET

#include "i386/tm-i386.h"

#undef NUM_FREGS
#define NUM_FREGS 15

#undef NUM_REGS
#define NUM_REGS 32

extern int info_regmap [];

#undef INFO_REGMAP
#define INFO_REGMAP(regno) info_regmap [(regno)]

extern int get_runtime_num_regs PARAMS ((int));

#undef RUNTIME_NUM_REGS 
#define RUNTIME_NUM_REGS(fp) get_runtime_num_regs ((fp))

/* Initializer for an array of names of registers. The order of the
   first 19 registers, 8 general purpose registers, %eip, %eflags,
   dummy, and 8 floating point registers, must match the compiler's
   numbering scheme. There should be NUM_REGS strings in this
   initializer. */

#undef  REGISTER_NAMES
#define REGISTER_NAMES { "eax", "ecx", "edx", "ebx",		\
			 "esp", "ebp", "esi", "edi",		\
			 "eip", "eflags", "orig_eax",		\
			 "st0", "st1", "st2", "st3",		\
			 "st4", "st5", "st6", "st7",		\
			 "fctrl", "fstat", "ftag", "fip",	\
			 "fcs", "fopoff", "fopsel",		\
			 "cs", "ss", "ds", "es", "fs", "gs"	\
			}

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#undef FP_REGNUM
#define FP_REGNUM 5	/* (ebp) Contains address of executing stack frame */
#undef  SP_REGNUM
#define SP_REGNUM  4	/* (usp) Contains address of top of stack */
#undef  PS_REGNUM
#define PS_REGNUM  9	/* (ps)  Contains processor status */
#undef  PC_REGNUM
#define PC_REGNUM  8	/* (eip) Contains program counter */
#undef  FP0_REGNUM
#define FP0_REGNUM 11	/* Floating point register 0 */
#undef  FPC_REGNUM
#define FPC_REGNUM 19	/* 80387 control register */
#undef  FPCWD_REGNUM
#define FPCWD_REGNUM FPC_REGNUM
#undef  FPSWD_REGNUM
#define FPSWD_REGNUM 20	/* 80387 status register */
#undef  FPTWD_REGNUM
#define FPTWD_REGNUM 21 /* 80387 tag register */
#undef  FPIPO_REGNUM
#define FPIPO_REGNUM 22	/* 80387 instruction pointer offset register */
#undef  FPIPS_REGNUM
#define FPIPS_REGNUM 23	/* 80387 instruction pointer selector egister */
#undef  FPOOS_REGNUM
#define FPOOS_REGNUM 24	/* 80387 operand pointer offset register */
#undef  FPOPS_REGNUM
#define FPOPS_REGNUM 25 /* 80387 operand pointer selector register */

/* Nonzero if register N requires conversion from raw format to virtual
   format.  */
#undef REGISTER_CONVERTIBLE
#define REGISTER_CONVERTIBLE(N) (((unsigned)((N) - FP0_REGNUM)) < 8)

/* Amount of bytes needed for general purpose registers. */
#undef GREGISTER_BYTES
#define GREGISTER_BYTES (17*4)

/* Amount of bytes needed for floating point registers. */
#undef FPREGISTER_BYTES
#define FPREGISTER_BYTES (7*4 + 8*10)

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */

#undef  REGISTER_BYTES
#define REGISTER_BYTES (GREGISTER_BYTES + FPREGISTER_BYTES)

/* Is register N a general purpose register? */

#undef GREGISTER
#define GREGISTER(N) ((N) < FP0_REGNUM || (N) >= FP0_REGNUM + NUM_FREGS)

/* Index for general purpose register in system calls. */

#undef  GREGISTER_BYTE
#define GREGISTER_BYTE(N) ((N) * 4)

/* Index for floating point register in system calls. */

#undef  FPREGISTER_BYTE
#define FPREGISTER_BYTE(N) \
 ((N) < 7 ? (N) * 4 : (((N) - 7) * 10) + 28)

/* Index within `registers' of the first byte of the space for
   register N.  */

#undef  REGISTER_BYTE
#define REGISTER_BYTE(N) \
 ((N) < FP0_REGNUM ? (N) * 4 : REGISTER_CONVERTIBLE (N) \
  ? (((N) - FP0_REGNUM) * 10) + 44 : (((N) - FP0_REGNUM - 8) * 4) + 124)

/* Number of bytes of storage in the actual machine representation
   for register N.  */

#undef  REGISTER_RAW_SIZE
#define REGISTER_RAW_SIZE(N) (REGISTER_CONVERTIBLE (N) ? 10 : 4)

/* Largest value REGISTER_RAW_SIZE can have.  */
#undef MAX_REGISTER_RAW_SIZE
#define MAX_REGISTER_RAW_SIZE 10

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */
#undef  MAX_REGISTER_VIRTUAL_SIZE
#define MAX_REGISTER_VIRTUAL_SIZE 10

#undef TARGET_LONG_DOUBLE_BIT
#define TARGET_LONG_DOUBLE_BIT 80

/* Does a value fit in a register? Although a long long, double or long
   double doesn't fit in a register, since x86 has to update all bytes
   at once, it should be ok to just watch the first few bytes. */
#undef VALUE_FIT_IN_REG
#define VALUE_FIT_IN_REG(v) \
  (TYPE_LENGTH (VALUE_TYPE (v)) <= REGISTER_SIZE \
   || TYPE_CODE (VALUE_TYPE (v)) == TYPE_CODE_INT \
   || TYPE_CODE (VALUE_TYPE (v)) == TYPE_CODE_FLT)

#if defined(HAVE_LONG_DOUBLE) && defined(HOST_I386)
/* The host and target are i386 machines and the compiler supports
   long doubles. Long doubles on the host therefore have the same
   layout as a 387 FPU stack register. */
#undef LD_I387
#define LD_I387
#endif

#ifdef LD_I387
/* Allow floating point numbers to be specified by
   a raw long double 10 hex bytes number,
   e.g. 1.0 can be input as 0x3fff8000000000000000
   */
extern int i387_hex_long_double_input(char *p, long double *putithere);
#define HEX_LONG_DOUBLE_INPUT(base,p,len,target) \
  ((base) == 16 && (len) == 20 \
   && i387_hex_long_double_input ((p), (target)))
#endif

#undef REGISTER_CONVERT_TO_VIRTUAL
#ifdef LD_I387
#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,TYPE,FROM,TO) \
{ \
  if (TYPE == REGISTER_VIRTUAL_TYPE (REGNUM)) \
    { \
      memcpy (TO, FROM, TYPE_LENGTH (TYPE)); \
    } \
  else \
    { \
      long double val = *((long double *)FROM); \
      store_floating ((TO), TYPE_LENGTH (TYPE), val); \
    } \
}
#else
/* Convert data from raw format for register REGNUM in buffer FROM
   to virtual format with type TYPE in buffer TO.  */
extern void i387_to_double PARAMS ((char *, char *));

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,TYPE,FROM,TO) \
{ \
  double val; \
  i387_to_double ((FROM), (char *)&val); \
  store_floating ((TO), TYPE_LENGTH (TYPE), val); \
}
#endif

#undef REGISTER_CONVERT_TO_RAW
#ifdef LD_I387
#define REGISTER_CONVERT_TO_RAW(TYPE,REGNUM,FROM,TO) \
{ \
  if (TYPE == REGISTER_VIRTUAL_TYPE (REGNUM)) \
    { \
      memcpy (TO, FROM, TYPE_LENGTH (TYPE)); \
    } \
  else \
    { \
      long double val = extract_floating ((FROM), TYPE_LENGTH (TYPE)); \
      *((long double *)TO) = val; \
    } \
}
#else
extern void double_to_i387 PARAMS ((char *, char *));

#define REGISTER_CONVERT_TO_RAW(TYPE,REGNUM,FROM,TO) \
{ \
  double val = extract_floating ((FROM), TYPE_LENGTH (TYPE)); \
  double_to_i387((char *)&val, (TO)); \
}
#endif

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#undef REGISTER_VIRTUAL_TYPE
#ifdef LD_I387
#define REGISTER_VIRTUAL_TYPE(N) \
  ((N < FP0_REGNUM) ? builtin_type_int : builtin_type_long_double)
#else
#define REGISTER_VIRTUAL_TYPE(N) \
  ((N < FP0_REGNUM) ? builtin_type_int : builtin_type_double)
#endif

#define FLOAT_INFO { i387_float_info (); }

/* Define DO_REGISTERS_INFO() to do machine-specific formatting
   of register dumps. */

#define DO_REGISTERS_INFO(_regnum, fp) i386_do_registers_info(_regnum, fp)
extern void i386_do_registers_info PARAMS ((int, int));

extern void i387_print_register PARAMS ((char *, int));

extern void i387_float_info PARAMS ((void));

/* Offset to saved PC in sigcontext, from <linux/signal.h>.  */
#define SIGCONTEXT_PC_OFFSET 38

/* We need this file for the SOLIB_TRAMPOLINE stuff. */

#include "tm-sysv4.h"

/* The following works around a problem with /usr/include/sys/procfs.h  */
#define sys_quotactl 1

#endif  /* #ifndef TM_LINUX_H */
