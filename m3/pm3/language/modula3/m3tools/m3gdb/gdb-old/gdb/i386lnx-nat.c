/* Intel 386 native support for Linux
   Copyright (C) 1998 Free Software Foundation, Inc.

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
#include "inferior.h"
#include "target.h"
#include "gdb_string.h"

#include <sys/types.h>
#include <sys/user.h>
#include <sys/ptrace.h>

#include "gdbcore.h"

#ifdef HAVE_SYS_REG_H
#include <sys/reg.h>
#endif

#ifdef TARGET_HAS_HARDWARE_WATCHPOINTS
#include <sys/debugreg.h>
#endif

#if !defined (PT_READ_I)
#define PT_READ_I	1	/* Read word from text space */
#endif
#if !defined (PT_READ_D)
#define	PT_READ_D	2	/* Read word from data space */
#endif
#if !defined (PT_READ_U)
#define PT_READ_U	3	/* Read word from kernel user struct */
#endif
#if !defined (PT_WRITE_I)
#define PT_WRITE_I	4	/* Write word to text space */
#endif
#if !defined (PT_WRITE_D)
#define PT_WRITE_D	5	/* Write word to data space */
#endif
#if !defined (PT_WRITE_U)
#define PT_WRITE_U	6	/* Write word to kernel user struct */
#endif
#if !defined (PT_CONTINUE)
#define PT_CONTINUE	7	/* Continue after signal */
#endif
#if !defined (PT_STEP)
#define PT_STEP		9	/* Set flag for single stepping */
#endif
#if !defined (PT_KILL)
#define PT_KILL		8	/* Send child a SIGKILL signal */
#endif

#ifndef PT_ATTACH
#define PT_ATTACH PTRACE_ATTACH
#endif
#ifndef PT_DETACH
#define PT_DETACH PTRACE_DETACH
#endif

#if !defined(PT_GETREGS) && defined(PTRACE_GETREGS)
#define PT_GETREGS PTRACE_GETREGS
#endif
#if !defined(PT_SETREGS) && defined(PTRACE_SETREGS)
#define PT_SETREGS PTRACE_SETREGS
#endif
#if !defined(PT_GETFPREGS) && defined(PTRACE_GETFPREGS)
#define PT_GETFPREGS PTRACE_GETFPREGS
#endif
#if !defined(PT_SETFPREGS) && defined(PTRACE_SETFPREGS)
#define PT_SETFPREGS PTRACE_SETFPREGS
#endif

static void fetch_register PARAMS ((int));
static void store_register PARAMS ((int));

static void old_fetch_inferior_registers PARAMS ((int));
static void old_store_inferior_registers PARAMS ((int));

#ifdef PT_GETREGS
static void new_fetch_inferior_registers PARAMS ((int));
static void new_store_inferior_registers PARAMS ((int));
static void init_fetch_inferior_registers PARAMS ((int));
static void init_store_inferior_registers PARAMS ((int));

static void (*fetch_inferior_registers_p) PARAMS ((int))
  = init_fetch_inferior_registers;
static void (*store_inferior_registers_p) PARAMS ((int))
  = init_store_inferior_registers;

static void set_inferior_registers_p PARAMS ((void));
#else
static void (*fetch_inferior_registers_p) PARAMS ((int))
  = old_fetch_inferior_registers;
static void (*store_inferior_registers_p) PARAMS ((int))
  = old_store_inferior_registers;
#endif

/* Default the type of the ptrace transfer to int.  */
#ifndef PTRACE_XFER_TYPE
#define PTRACE_XFER_TYPE int
#endif

#ifndef PTRACE_ARG4_TYPE
#define PTRACE_ARG4_TYPE int
#endif

#if !defined (offsetof)
#define offsetof(TYPE, MEMBER) ((unsigned long) &((TYPE *)0)->MEMBER)
#endif

/* U_REGS_OFFSET is the offset of the registers within the u area.  */
#if !defined (U_REGS_OFFSET)
#define U_REGS_OFFSET \
  ptrace (PT_READ_U, inferior_pid, \
	  (PTRACE_ARG3_TYPE) (offsetof (struct user, u_ar0)), 0) \
    - KERNEL_U_ADDR
#endif

/* symbols like 'EAX' come from <sys/reg.h> or <asm/ptrace.h>. */
#ifndef ORIG_EAX
#define ORIG_EAX 11
#endif

#ifndef ST0
#define ST0 7
#endif

#ifndef ST1
#define ST1 8
#endif

#ifndef ST2
#define ST2 9
#endif

#ifndef ST3
#define ST3 10
#endif

#ifndef ST4
#define ST4 11
#endif

#ifndef ST5
#define ST5 12
#endif

#ifndef ST6
#define ST6 13
#endif

#ifndef ST7
#define ST7 14
#endif

#ifndef CWD
#define CWD 0
#endif

#ifndef SWD
#define SWD 1
#endif

#ifndef TWD
#define TWD 2
#endif

#ifndef FIP
#define FIP 3
#endif

#ifndef FCS
#define FCS 4
#endif

#ifndef FOO
#define FOO 5
#endif

#ifndef FOS
#define FOS 6
#endif

/* Used for register mapping in system calls. */
static int syscall_regmap [] = 
{
  EAX,
  ECX,
  EDX,
  EBX,
  UESP,
  EBP,
  ESI,
  EDI,
  EIP,
  EFL,
  ORIG_EAX,
  ST0,
  ST1,
  ST2,
  ST3,
  ST4,
  ST5,
  ST6,
  ST7,
  CWD,
  SWD,
  TWD,
  FIP,
  FCS,
  FOO,
  FOS,
  CS,
  SS,
  DS,
  ES,
  FS,
  GS,
};

/* Used for register infomation mapping. */
int info_regmap [] = 
{
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  26,
  27,
  28,
  29,
  30,
  31,
  11,
  12,
  13,
  14,
  15,
  16,
  17,
  18,
  19,
  20,
  21,
  22,
  23,
  24,
  25
};

/* Fetch one register.  */
static void
fetch_register (regno)
     int regno;
{
  /* This isn't really an address.  But ptrace thinks of it as one.  */
  CORE_ADDR regaddr;
  char mess[128];				/* For messages */
  register int i;
  unsigned int offset;  /* Offset of registers within the u area.  */
  char buf[MAX_REGISTER_RAW_SIZE];

  offset = U_REGS_OFFSET;

  regaddr = register_addr (regno, offset);
  for (i = 0; i < REGISTER_RAW_SIZE (regno); i += sizeof (PTRACE_XFER_TYPE))
    {
      errno = 0;
      *(PTRACE_XFER_TYPE *) &buf[i] = ptrace (PT_READ_U, inferior_pid,
					      (PTRACE_ARG3_TYPE) regaddr, 0);
      regaddr += sizeof (PTRACE_XFER_TYPE);
      if (errno != 0)
	{
	  sprintf (mess, "reading register %s (#%d)", reg_names[regno], regno);
	  perror_with_name (mess);
	}
    }
  supply_register (regno, buf);
}


/* Fetch register values from the inferior.
   If REGNO is negative, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time). */

static void
old_fetch_inferior_registers (regno)
     int regno;
{
  if (regno >= 0)
    {
      if (GREGISTER (regno))
	fetch_register (regno);
      else
	supply_register (regno, &registers[REGISTER_BYTE (regno)]);
    }
  else
    {
      for (regno = 0; regno < NUM_REGS; regno++)
	{
	  if (GREGISTER (regno))
	    fetch_register (regno);
	  else
	    supply_register (regno, &registers[REGISTER_BYTE (regno)]);
	}
    }
}

/* Store one register. */

static void
store_register (regno)
     int regno;
{
  /* This isn't really an address.  But ptrace thinks of it as one.  */
  CORE_ADDR regaddr;
  char mess[128];				/* For messages */
  register int i;
  unsigned int offset;  /* Offset of registers within the u area.  */

  offset = U_REGS_OFFSET;

  regaddr = register_addr (regno, offset);
  for (i = 0; i < REGISTER_RAW_SIZE (regno); i += sizeof(PTRACE_XFER_TYPE))
    {
      errno = 0;
      ptrace (PT_WRITE_U, inferior_pid, (PTRACE_ARG3_TYPE) regaddr,
	      *(PTRACE_XFER_TYPE *) &registers[REGISTER_BYTE (regno) + i]);
      regaddr += sizeof (PTRACE_XFER_TYPE);
      if (errno != 0)
	{
	  sprintf (mess, "writing register %s (#%d)", reg_names[regno], regno);
	  perror_with_name (mess);
	}
    }
}

/* Store our register values back into the inferior.
   If REGNO is negative, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

static void
old_store_inferior_registers (regno)
     int regno;
{
  if (regno >= 0)
    {
      if (GREGISTER (regno))
	store_register (regno);
    }
  else
    {
      for (regno = 0; regno < NUM_REGS; regno++)
	{
	  if (GREGISTER (regno))
	    store_register (regno);
	}
    }
}

#ifdef PT_GETREGS
static void
new_fetch_inferior_registers(regno)
     int regno;
{
  char inferior_gregisters [GREGISTER_BYTES];
  char inferior_fpregisters [FPREGISTER_BYTES];
  int i;

  ptrace (PT_GETREGS, inferior_pid, (PTRACE_ARG3_TYPE) 0,
	  (PTRACE_ARG4_TYPE) &inferior_gregisters [0]);
  ptrace (PT_GETFPREGS, inferior_pid, (PTRACE_ARG3_TYPE) 0,
	  (PTRACE_ARG4_TYPE) &inferior_fpregisters [0]);
  for (i = 0; i < NUM_REGS; i++)
    if (GREGISTER (i))
      memcpy (&registers[REGISTER_BYTE (i)],
	      &inferior_gregisters [GREGISTER_BYTE (syscall_regmap [i])],
	      REGISTER_RAW_SIZE (i));
    else
      memcpy (&registers[REGISTER_BYTE (i)],
	      &inferior_fpregisters [FPREGISTER_BYTE (syscall_regmap [i])],
	      REGISTER_RAW_SIZE (i));
  registers_fetched ();
}

static void
new_store_inferior_registers(regno)
     int regno;
{
  char inferior_gregisters [GREGISTER_BYTES];
  char inferior_fpregisters [FPREGISTER_BYTES];
  int i;

  for (i = 0; i < NUM_REGS; i++)
    if (GREGISTER (i))
      memcpy (&inferior_gregisters [GREGISTER_BYTE (syscall_regmap [i])],
	      &registers[REGISTER_BYTE (i)],
	      REGISTER_RAW_SIZE (i));
    else
      memcpy (&inferior_fpregisters [FPREGISTER_BYTE (syscall_regmap [i])],
	      &registers[REGISTER_BYTE (i)],
	      REGISTER_RAW_SIZE (i));
  ptrace (PT_SETREGS, inferior_pid, (PTRACE_ARG3_TYPE) 0,
	  (PTRACE_ARG4_TYPE) &inferior_gregisters [0]);
  ptrace (PT_SETFPREGS, inferior_pid, (PTRACE_ARG3_TYPE) 0,
	  (PTRACE_ARG4_TYPE) &inferior_fpregisters [0]);
}

static void
set_inferior_registers_p ()
{
  char inferior_registers [GREGISTER_BYTES];

  if (ptrace (PT_GETREGS, inferior_pid, (PTRACE_ARG3_TYPE) 0,
	      (PTRACE_ARG4_TYPE) &inferior_registers) < 0)
    {
      if (errno == EIO)
	{
	  fetch_inferior_registers_p = old_fetch_inferior_registers;
	  store_inferior_registers_p = old_store_inferior_registers;
	}
    }
  else
    {
      fetch_inferior_registers_p = new_fetch_inferior_registers;
      store_inferior_registers_p = new_store_inferior_registers;
    }
}

void
init_fetch_inferior_registers (regno)
     int regno;
{
  set_inferior_registers_p ();
  (*fetch_inferior_registers_p) (regno);
}

void
init_store_inferior_registers (regno)
     int regno;
{
  set_inferior_registers_p ();
  (*store_inferior_registers_p) (regno);
}
#endif

void
fetch_inferior_registers(regno)
{
  (*fetch_inferior_registers_p) (regno);
}

void
store_inferior_registers(regno)
{
  (*store_inferior_registers_p) (regno);
}

int
get_runtime_num_regs (int fpregs)
{
#ifdef PT_GETREGS
  if (fetch_inferior_registers_p == init_fetch_inferior_registers)
    set_inferior_registers_p ();
  if (!fpregs
      || fetch_inferior_registers_p == old_fetch_inferior_registers)
    return NUM_REGS - NUM_FREGS;
  else
    return NUM_REGS;
#else
  return NUM_REGS - NUM_FREGS;
#endif
}

/* blockend is the value of u.u_ar0, and points to the
 * place where GS is stored
 */

int
i386_register_u_addr (blockend, regnum)
     int blockend;
     int regnum;
{
  return GREGISTER (regnum)
	 ? (blockend + GREGISTER_BYTE (syscall_regmap [regnum])) : -1;
}

int
kernel_u_size ()
{
  return (sizeof (struct user));
}

#ifdef TARGET_HAS_HARDWARE_WATCHPOINTS

/* Record the value of the debug control register.  */
static int debug_control_mirror;

/* Record which address associates with which register.  */
static CORE_ADDR address_lookup[DR_LASTADDR - DR_FIRSTADDR + 1];

static int
i386_insert_aligned_watchpoint PARAMS ((int, CORE_ADDR, CORE_ADDR, int,
					   int));

static int
i386_insert_nonaligned_watchpoint PARAMS ((int, CORE_ADDR, CORE_ADDR, int,
					   int));

/* Insert a watchpoint.  */

int
i386_insert_watchpoint (pid, addr, len, rw)
     int pid;
     CORE_ADDR addr;
     int len;
     int rw;
{
  return i386_insert_aligned_watchpoint (pid, addr, addr, len, rw);
}

static int
i386_insert_aligned_watchpoint (pid, waddr, addr, len, rw)
     int pid;
     CORE_ADDR waddr;
     CORE_ADDR addr;
     int len;
     int rw;
{
  int i;
  int read_write_bits, len_bits;
  int free_debug_register;
  int register_number;
  
  /* Look for a free debug register.  */
  for (i = DR_FIRSTADDR; i <= DR_LASTADDR; i++)
    {
      if (address_lookup[i - DR_FIRSTADDR] == 0
	  || address_lookup[i - DR_FIRSTADDR] == addr)
	break;
    }

  /* No more debug registers!  */
  if (i > DR_LASTADDR)
    return -1;

  read_write_bits = (rw & 1) ? DR_RW_READ : DR_RW_WRITE;

  if (len == 1)
    len_bits = DR_LEN_1;
  else if (len == 2)
    {
      if (addr % 2)
	return i386_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
      len_bits = DR_LEN_2;
    }

  else if (len == 4)
    {
      if (addr % 4)
	return i386_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
      len_bits = DR_LEN_4;
    }
  else if (len == 8 || len == 10 || len == 12)
    {
      /* It should only happen with long long, double or long double.
	 All should be updated at the same time. We just watch the
	 first 4 bytes. */
      len = 4;
      if (addr % 4)
	return i386_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
      len_bits = DR_LEN_4;
    }
  else
#if 1
    /* Don't bother. x86 cannot handle it anyway. Save the hardware
       waitpoint for others. We fake it to make gdb happy. */
    return 0;
#else
    return i386_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
#endif
  
  free_debug_register = i;
  register_number = free_debug_register - DR_FIRSTADDR;
  debug_control_mirror |=
    ((read_write_bits | len_bits)
     << (DR_CONTROL_SHIFT + DR_CONTROL_SIZE * register_number));
  debug_control_mirror |=
    (1 << (DR_LOCAL_ENABLE_SHIFT + DR_ENABLE_SIZE * register_number));
  debug_control_mirror |= DR_LOCAL_SLOWDOWN;
  debug_control_mirror &= ~DR_CONTROL_RESERVED;
  
  ptrace (PT_WRITE_U, pid, offsetof (struct user, u_debugreg[DR_CONTROL]),
	  debug_control_mirror);
  ptrace (PT_WRITE_U, pid, offsetof (struct user, u_debugreg[free_debug_register]),
	  addr);

  /* Record where we came from.  */
  address_lookup[register_number] = addr;
  return 0;
}

static int
i386_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw)
     int pid;
     CORE_ADDR waddr;
     CORE_ADDR addr;
     int len;
     int rw;
{
  int align;
  int size;
  int rv;

  static int size_try_array[16] = {
    1, 1, 1, 1,			/* trying size one */
    2, 1, 2, 1,			/* trying size two */
    2, 1, 2, 1,			/* trying size three */
    4, 1, 2, 1			/* trying size four */
  };

  rv = 0;
  while (len > 0)
    {
      align = addr % 4;
      /* Four is the maximum length for 386.  */
      size = (len > 4) ? 3 : len - 1;
      size = size_try_array[size * 4 + align];

      rv = i386_insert_aligned_watchpoint (pid, waddr, addr, size, rw);
      if (rv)
	{
	  i386_remove_watchpoint (pid, waddr, size);
	  return rv;
	}
      addr += size;
      len -= size;
    }
  return rv;
}

/* Remove a watchpoint.  */

int
i386_remove_watchpoint (pid, addr, len)
     int pid;
     CORE_ADDR addr;
     int len;
{
  int i;
  int register_number;

  for (i = DR_FIRSTADDR; i <= DR_LASTADDR; i++)
    {
      register_number = i - DR_FIRSTADDR;
      if (address_lookup[register_number] == addr)
	{
	  debug_control_mirror &=
	    ~(1 << (DR_LOCAL_ENABLE_SHIFT + DR_ENABLE_SIZE * register_number));
	  address_lookup[register_number] = 0;
	}
    }
  ptrace (PT_WRITE_U, pid, offsetof (struct user, u_debugreg[DR_CONTROL]),
	  debug_control_mirror);
  ptrace (PT_WRITE_U, pid, offsetof (struct user, u_debugreg[DR_STATUS]), 0);

  return 0;
}

/* Check if stopped by a watchpoint.  */

CORE_ADDR
i386_stopped_by_watchpoint (pid)
    int pid;
{
  int i;
  int status;

  status = ptrace (PT_READ_U, pid, offsetof (struct user, u_debugreg[DR_STATUS]), 0);
  ptrace (PT_WRITE_U, pid, offsetof (struct user, u_debugreg[DR_STATUS]), 0);

  for (i = DR_FIRSTADDR; i <= DR_LASTADDR; i++)
    {
      if (status & (1 << (i - DR_FIRSTADDR)))
	return address_lookup[i - DR_FIRSTADDR];
    }

  return 0;
}

#endif /* TARGET_HAS_HARDWARE_WATCHPOINTS */

#ifdef HAVE_SYS_PROCFS_H

#include <sys/procfs.h>

#ifdef HAVE_GREGSET_T

/*  Given a pointer to a general register set in /proc format (gregset_t *),
    unpack the register contents and supply them as gdb's idea of the current
    register values. */

void
supply_gregset (gregsetp)
     gregset_t *gregsetp;
{
  char *g = (char *) gregsetp;
  int i;

  for (i = 0; i < NUM_REGS; i++)
    if (GREGISTER (i))
      *(greg_t *) &registers[REGISTER_BYTE (i)]
	= *(greg_t *) &g [GREGISTER_BYTE (syscall_regmap [i])];
}

void
fill_gregset (gregsetp, regno)
     gregset_t *gregsetp;
     int regno;
{
  char *g = (char *) gregsetp;

  if (regno == -1)
    {
      int i;
      for (i = 0; i < NUM_REGS; i++)
	if (GREGISTER (i))
	  *(greg_t *) &g [GREGISTER_BYTE (syscall_regmap [i])]
	    = *(greg_t *) &registers[REGISTER_BYTE (i)];
    }
  else
    {
      *(greg_t *) &g [GREGISTER_BYTE (syscall_regmap [regno])]
	= *(greg_t *) &registers[REGISTER_BYTE (regno)];
    }
}

#endif	/* HAVE_GREGSET_T */

#if defined (FP0_REGNUM) && defined (HAVE_FPREGSET_T)

/*  Given a pointer to a floating point register set in /proc format
    (fpregset_t *), unpack the register contents and supply them as gdb's
    idea of the current floating point register values. */

void
supply_fpregset (fpregsetp)
     fpregset_t *fpregsetp;
{
  char *fp = (char *) fpregsetp;
  int i;

  for (i = 0; i < NUM_REGS; i++)
    if (!GREGISTER (i))
      memcpy (&registers[REGISTER_BYTE (i)],
	      &fp [FPREGISTER_BYTE (syscall_regmap [i])],
	      REGISTER_RAW_SIZE (i));
}

/*  Given a pointer to a floating point register set in /proc format
    (fpregset_t *), update the register specified by REGNO from gdb's idea
    of the current floating point register set.  If REGNO is -1, update
    them all. */

void
fill_fpregset (fpregsetp, regno)
     fpregset_t *fpregsetp;
     int regno;
{
  char *fp = (char *) fpregsetp;

  if (regno == -1)
    {
      int i;

      for (i = 0; i < NUM_REGS; i++)
	if (!GREGISTER (i))
	  memcpy (&fp [FPREGISTER_BYTE (syscall_regmap [i])],
		  &registers[REGISTER_BYTE (i)],
		  REGISTER_RAW_SIZE (i));
    }
  else
    {
      memcpy (&fp [FPREGISTER_BYTE (syscall_regmap [regno])],
	      &registers[REGISTER_BYTE (regno)],
	      REGISTER_RAW_SIZE (regno));
    }
}

#endif	/* defined (FP0_REGNUM) && defined (HAVE_FPREGSET_T) */

static void
fetch_core_registers (core_reg_sect, core_reg_size, which, ignored)
     char *core_reg_sect;
     unsigned core_reg_size;
     int which;
     CORE_ADDR ignored;
{
  if (core_reg_size == GREGISTER_BYTES)
    supply_gregset (core_reg_sect);
#if defined (FP0_REGNUM) && defined (HAVE_FPREGSET_T)
  else if (core_reg_size == FPREGISTER_BYTES)
    supply_fpregset (core_reg_sect);
#endif
  else
    fprintf_unfiltered (gdb_stderr, "Unknown core size: %d",
			core_reg_size);
}

static struct core_fns elf_core_fns =
{
  bfd_target_elf_flavour,
  fetch_core_registers,
  NULL
};

void
_initialize_i386linux_nat ()
{
  add_core_fns (&elf_core_fns);
}

#endif	/* HAVE_SYS_PROCFS_H */
