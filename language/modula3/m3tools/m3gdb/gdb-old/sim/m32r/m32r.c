/* m32r simulator support code
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

This file is part of GDB, the GNU debugger.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#define WANT_CPU
#define WANT_CPU_M32R

#include "sim-main.h"
#include <signal.h>
#include "libiberty.h"
#include "bfd.h"
#include "targ-vals.h"

/* The contents of BUF are in target byte order.  */

void
m32r_fetch_register (sd, rn, buf)
     SIM_DESC sd;
     int rn;
     unsigned char *buf;
{
  SIM_CPU *current_cpu = STATE_CPU (sd, 0);

  if (rn < 16)
    SETTWI (buf, GET_H_GR (rn));
  else if (rn < 21)
    SETTWI (buf, GET_H_CR (rn - 16));
  else switch (rn) {
    case PC_REGNUM:
      SETTWI (buf, GET_H_PC ());
      break;
    case ACCL_REGNUM:
      SETTWI (buf, GETLODI (GET_H_ACCUM ()));
      break;
    case ACCH_REGNUM:
      SETTWI (buf, GETHIDI (GET_H_ACCUM ()));
      break;
#if 0
    case 23: *reg = STATE_CPU_CPU (sd, 0)->h_cond;		break;
    case 24: *reg = STATE_CPU_CPU (sd, 0)->h_sm;		break;
    case 25: *reg = STATE_CPU_CPU (sd, 0)->h_bsm;		break;
    case 26: *reg = STATE_CPU_CPU (sd, 0)->h_ie;		break;
    case 27: *reg = STATE_CPU_CPU (sd, 0)->h_bie;		break;
    case 28: *reg = STATE_CPU_CPU (sd, 0)->h_bcarry;		break; /* rename: bc */
    case 29: memcpy (buf, &STATE_CPU_CPU (sd, 0)->h_bpc, sizeof(WI));	break; /* duplicate */
#endif
    default: abort ();
  }
}
 
/* The contents of BUF are in target byte order.  */

void
m32r_store_register (sd, rn, buf)
     SIM_DESC sd;
     int rn;
     unsigned char *buf;
{
  SIM_CPU *current_cpu = STATE_CPU (sd, 0);

  if (rn < 16)
    SET_H_GR (rn, GETTWI (buf));
  else if (rn < 21)
    SET_H_CR (rn - 16, GETTWI (buf));
  else switch (rn) {
    case PC_REGNUM:
      SET_H_PC (GETTWI (buf));
      break;
    case ACCL_REGNUM:
      SETLODI (CPU (h_accum), GETTWI (buf));
      break;
    case ACCH_REGNUM:
      SETHIDI (CPU (h_accum), GETTWI (buf));
      break;
#if 0
    case 23: STATE_CPU_CPU (sd, 0)->h_cond   = *reg;			break;
    case 24: STATE_CPU_CPU (sd, 0)->h_sm     = *reg;			break;
    case 25: STATE_CPU_CPU (sd, 0)->h_bsm    = *reg;			break;
    case 26: STATE_CPU_CPU (sd, 0)->h_ie     = *reg;			break;
    case 27: STATE_CPU_CPU (sd, 0)->h_bie    = *reg;			break;
    case 28: STATE_CPU_CPU (sd, 0)->h_bcarry = *reg;			break; /* rename: bc */
    case 29: memcpy (&STATE_CPU_CPU (sd, 0)->h_bpc, buf, sizeof(DI));	break; /* duplicate */
#endif
  }
}

/* Handling the MSPR register is done by creating a device in the core
   mapping that winds up here.  */

device m32r_mspr_device;

int
device_io_read_buffer (device *me, const void *source, int space,
			address_word addr, unsigned nr_bytes,
			SIM_CPU *cpu, sim_cia cia)
{
  abort ();
}

int
device_io_write_buffer (device *me, const void *source, int space,
			address_word addr, unsigned nr_bytes,
			SIM_CPU *cpu, sim_cia cia)
{
#if WITH_SCACHE
  if (addr == MSPR_ADDR
      && (*(char *) source & 1) != 0)
    scache_flush (CPU_STATE (cpu));
#endif
  return nr_bytes;
}

void device_error () {}

#if WITH_PROFILE_MODEL_P

void
m32r_model_mark_get_h_gr (SIM_CPU *cpu, ARGBUF *abuf)
{
  if ((CPU_CGEN_PROFILE (cpu)->h_gr & abuf->h_gr_get) != 0)
    {
      PROFILE_MODEL_LOAD_STALL_COUNT (CPU_PROFILE_DATA (cpu)) += 2;
      if (TRACE_INSN_P (cpu))
	cgen_trace_printf (cpu, " ; Load stall of 2 cycles.");
    }
}

void
m32r_model_mark_set_h_gr (SIM_CPU *cpu, ARGBUF *abuf)
{
}

void
m32r_model_mark_busy_reg (SIM_CPU *cpu, ARGBUF *abuf)
{
  CPU_CGEN_PROFILE (cpu)->h_gr = abuf->h_gr_set;
}

void
m32r_model_mark_unbusy_reg (SIM_CPU *cpu, ARGBUF *abuf)
{
  CPU_CGEN_PROFILE (cpu)->h_gr = 0;
}

#endif /* WITH_PROFILE_MODEL_P */

USI
m32r_h_cr_get (SIM_CPU *current_cpu, UINT cr)
{
  /* FIXME: Create enums H_CR_FOO, etc.  */
  switch (cr)
    {
    case 0 : /* psw */
      return ((CPU (h_bsm) << 15)
	      | (CPU (h_bie) << 14)
	      | (CPU (h_bcond) << 8)
	      | (CPU (h_sm) << 7)
	      | (CPU (h_ie) << 6)
	      | (CPU (h_cond) << 0));
    case 1 : /* condition bit */
      return CPU (h_cond);
    case 2 : /* interrupt stack pointer */
      if (! CPU (h_sm))
	return CPU (h_gr[15]);
      else
	return CPU (h_cr[2]);
    case 3 : /* user stack pointer */
      if (CPU (h_sm))
	return CPU (h_gr[15]);
      else
	return CPU (h_cr[3]);
    case 6 : /* backup pc */
      /* ??? We don't really support this yet.  */
    case 4 : /* unused */
    case 5 : /* unused */
      return CPU (h_cr[cr]);
    default :
      return 0;
    }
}

void
m32r_h_cr_set (SIM_CPU *current_cpu, UINT cr, USI newval)
{
  /* FIXME: Create enums H_CR_FOO, etc.  */
  switch (cr)
    {
    case 0 : /* psw */
      {
	int old_sm = CPU (h_sm);
	CPU (h_bsm) = (newval & (1 << 15)) != 0;
	CPU (h_bie) = (newval & (1 << 14)) != 0;
	CPU (h_bcond) = (newval & (1 << 8)) != 0;
	CPU (h_sm) = (newval & (1 << 7)) != 0;
	CPU (h_ie) = (newval & (1 << 6)) != 0;
	CPU (h_cond) = (newval & (1 << 0)) != 0;
	/* When switching stack modes, update the registers.  */
	if (old_sm != CPU (h_sm))
	  {
	    if (old_sm)
	      {
		/* Switching user -> system.  */
		CPU (h_cr[3]) = CPU (h_gr[15]);
		CPU (h_gr[15]) = CPU (h_cr[2]);
	      }
	    else
	      {
		/* Switching system -> user.  */
		CPU (h_cr[2]) = CPU (h_gr[15]);
		CPU (h_gr[15]) = CPU (h_cr[3]);
	      }
	  }
	break;
      }
    case 1 : /* condition bit */
      CPU (h_cond) = (newval & 1) != 0;
      break;
    case 2 : /* interrupt stack pointer */
      if (! CPU (h_sm))
	CPU (h_gr[15]) = newval;
      else
	CPU (h_cr[2]) = newval;
      break;
    case 3 : /* user stack pointer */
      if (CPU (h_sm))
	CPU (h_gr[15]) = newval;
      else
	CPU (h_cr[3]) = newval;
      break;
    case 4 : /* unused */
    case 5 : /* unused */
    case 6 : /* backup pc */
      CPU (h_cr[cr]) = newval;
      break;
    default :
      /* ignore */
      break;
    }
}

void
do_lock (SIM_CPU *cpu, SI r1, SI r2)
{
  /* Nothing to do [at present].  */
}

void
do_unlock (SIM_CPU *cpu, SI r1, SI r2)
{
  /* Nothing to do [at present].  */
}

void
do_trap (SIM_CPU *current_cpu, int num)
{
  SIM_DESC sd = CPU_STATE (current_cpu);
  host_callback *cb = STATE_CALLBACK (sd);

  switch (num)
    {
    case 0 :
      /* Trap 0 is used for system calls.  */
      {
	/* Store the results in these first so we don't clobber PARM[123]
	   when we set RETVAL, RETERR.  */
	int retval = 0, reterr = 0;
/*	int save_errno = errno;	*/
/*	errno = 0;*/

#define FUNC CPU (h_gr)[0]
#define PARM1 CPU (h_gr)[1]
#define PARM2 CPU (h_gr)[2]
#define PARM3 CPU (h_gr)[3]
#define RETERR CPU (h_gr)[2]
#define RETVAL CPU (h_gr)[0]
#define RETVAL2 CPU (h_gr)[1]

	switch (FUNC)
	  {
#if 0
	  case TARGET_SYS_argvlen :
	    {
	      int addr_size = MACH_ADDR_BITSIZE (MODEL_MACH (STATE_MODEL (current_state))) / 8;
	      int argc,envc,arglen,envlen;
	      char **argv = STATE_ARGV (current_state);
	      char **envp = STATE_ENVP (current_state);

	      argc = arglen = 0;
	      if (argv)
		{
		  for ( ; argv[argc]; ++argc)
		    arglen += strlen (argv[argc]) + 1;
		}
	      envc = envlen = 0;
	      if (envp)
		{
		  for ( ; envp[envc]; ++envc)
		    envlen += strlen (envp[envc]) + 1;
		}
	      retval = ((argc + 1) * addr_size) + ((envc + 1) * addr_size)
		+ ((arglen + envlen + addr_size - 1) & -addr_size);
	      break;
	    }

	  case TARGET_SYS_argv :
	    {
	      /* Pointer to buffer of two pointers for argv, envp.  */
	      SIM_ADDR argv_envp = PARM1;
	      /* P is the target address of the pointers to the strings.  */
	      SIM_ADDR p = PARM2;
	      /* Q is the target address of where all the strings go.  */
	      SIM_ADDR q;
	      int addr_size = MACH_ADDR_BITSIZE (MODEL_MACH (STATE_MODEL (current_state))) / 8;
	      int i,argc,envc,len;
	      char **argv = STATE_ARGV (current_state);
	      char **envp = STATE_ENVP (current_state);

	      argc = 0;
	      if (argv)
		{
		  for ( ; argv[argc]; ++argc)
		    continue;
		}
	      envc = 0;
	      if (envp)
		{
		  for ( ; envp[envc]; ++envc)
		    continue;
		}
	      q = p + ((argc + 1) * addr_size) + ((envc + 1) * addr_size);
	      /* We could standardize on a format and have the caller compute
		 argv,envp; but this obviates the need for that.  */
	      SETMEMAI (current_cpu, argv_envp, p);
	      for (i = 0; i < argc; ++i)
		{
		  SETMEMAI (current_cpu, p, q);
		  len = strlen (argv[i]) + 1;
/*		  SETMEM (current_cpu, q, argv[i], len);*/
		  p += addr_size;
		  q += len;
		}
	      SETMEMAI (current_cpu, p, 0);
	      p += addr_size;
	      SETMEMAI (current_cpu, argv_envp + addr_size, p);
	      for (i = 0; i < envc; ++i)
		{
		  SETMEMAI (current_cpu, p, q);
		  len = strlen (envp[i]) + 1;
/*		  SETMEM (current_cpu, q, envp[i], len);*/
		  p += addr_size;
		  q += len;
		}
	      SETMEMAI (current_cpu, p, 0);
	      retval = argc;
	      break;
	    }
#endif

	  case TARGET_SYS_exit :
	    /* Tell sim_resume program called exit().  */
	    sim_engine_halt (sd, current_cpu, NULL, NULL_CIA,
			     sim_exited, PARM1);
	    break;

	  case TARGET_SYS_open :
	    {
	      char path[1024];
	      char *p, *pend;
	      ADDR addr = PARM1;

	      for (p = path, pend = path + sizeof (path); p < pend; ++p, ++addr)
		{
		  unsigned int count =
		    sim_core_read_buffer (CPU_STATE (current_cpu), current_cpu,
					  sim_core_read_map,
					  p, addr, 1);
		  if (count != 1)
		    {
		      retval = -1;
		      reterr = TARGET_EINVAL;
		      goto FinishSyscall;
		    }
		  if (*p == 0)
		    break;
		}
	      if (p == pend)
		{
		  retval = -1;
		  reterr = TARGET_ENAMETOOLONG;
		  goto FinishSyscall;
		}
	      retval = (*cb->open) (cb, path,
				    PARM2 /*FIXME: , PARM3*/);
	    }
	    break;

	  case TARGET_SYS_close :
	    retval = (*cb->close) (cb, PARM1);
	    break;

	  case TARGET_SYS_read :
	    {
	      /* ??? Perfect handling of error conditions may require only one
		 call to cb->read.  One can't assume all the data is
		 contiguously stored in host memory so that would require
		 malloc'ing/free'ing the space.  Maybe later.  */
	      char buf[1024];
	      int fd = PARM1;
	      ADDR addr = PARM2;
	      size_t count = PARM3;
	      size_t bytes_read = 0;
	      int bytes_written;

	      while (count > 0)
		{
		  retval = (int) (*cb->read) (cb, fd, buf,
					      count < 1024 ? count : 1024);
		  if (retval == -1)
		    goto FinishSyscall;
		  bytes_written =
		    sim_core_write_buffer (CPU_STATE (current_cpu), current_cpu,
					   sim_core_write_map,
					   buf, addr, retval);
		  if (bytes_written != retval)
		    {
		      retval = -1;
		      reterr = TARGET_EINVAL;
		      goto FinishSyscall;
		    }
		  bytes_read += retval;
		  count -= retval;
		  addr += retval;
		}
	      retval = bytes_read;
	    }
	    break;

	  case TARGET_SYS_write :
	    {
	      /* ??? Perfect handling of error conditions may require only one
		 call to cb->write.  One can't assume all the data is
		 contiguously stored in host memory so that would require
		 malloc'ing/free'ing the space.  Maybe later.  */
	      char buf[1024];
	      int fd = PARM1;
	      ADDR addr = PARM2;
	      size_t count = PARM3;
	      int bytes_read;
	      size_t bytes_written = 0;

	      while (count > 0)
		{
		  int bytes_to_read = count < 1024 ? count : 1024;
		  bytes_read =
		    sim_core_read_buffer (CPU_STATE (current_cpu), current_cpu,
					  sim_core_read_map,
					  buf, addr, bytes_to_read);
		  if (bytes_read != bytes_to_read)
		    {
		      retval = -1;
		      reterr = TARGET_EINVAL;
		      goto FinishSyscall;
		    }
		  if (fd == 1)
		    retval = (int) (*cb->write_stdout) (cb, buf, bytes_read);
		  else
		    retval = (int) (*cb->write) (cb, fd, buf, bytes_read);
		  if (retval == -1)
		    goto FinishSyscall;
		  bytes_written += retval;
		  count -= retval;
		  addr += retval;
		}
	      retval = bytes_written;
	    }
	    break;

	  default :
	    retval = -1;
#ifdef TARGET_ENOSYS
	    reterr = TARGET_ENOSYS;
#else
	    reterr = TARGET_EINVAL;
#endif
	    break;
	  }

      FinishSyscall:
	RETVAL = retval;
	if (reterr == 0)
	  RETERR = (*cb->get_errno) (cb);
	else
	  RETERR = reterr;
	break;
      }

    case 1:	/* breakpoint trap */
      sim_engine_halt (sd, current_cpu, NULL, NULL_CIA,
		       sim_stopped, SIM_SIGTRAP);
      break;

    default :
      abort ();
    }
}
