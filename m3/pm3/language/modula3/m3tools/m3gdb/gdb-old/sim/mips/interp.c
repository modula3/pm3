/*> interp.c <*/
/* Simulator for the MIPS architecture.

   This file is part of the MIPS sim

		THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

   $Revision$
     $Author$
       $Date$

NOTEs:

The IDT monitor (found on the VR4300 board), seems to lie about
register contents. It seems to treat the registers as sign-extended
32-bit values. This cause *REAL* problems when single-stepping 64-bit
code on the hardware.

*/

/* The TRACE manifests enable the provision of extra features. If they
   are not defined then a simpler (quicker) simulator is constructed
   without the required run-time checks, etc. */
#if 1 /* 0 to allow user build selection, 1 to force inclusion */
#define TRACE (1)
#endif

#include "bfd.h"
#include "sim-main.h"
#include "sim-utils.h"
#include "sim-options.h"
#include "sim-assert.h"

#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <ansidecl.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#else
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#endif

#include "getopt.h"
#include "libiberty.h"
#include "bfd.h"
#include "callback.h"   /* GDB simulator callback interface */
#include "remote-sim.h" /* GDB simulator interface */

#include "sysdep.h"

#ifndef PARAMS
#define PARAMS(x) 
#endif

char* pr_addr PARAMS ((SIM_ADDR addr));
char* pr_uword64 PARAMS ((uword64 addr));


/* Get the simulator engine description, without including the code: */
#if (WITH_IGEN)
#define LOADDRMASK (WITH_TARGET_WORD_BITSIZE == 64 ? 0x7 : 0x3)
#else
#define SIM_MANIFESTS
#include "oengine.c"
#undef SIM_MANIFESTS
#endif

/* Within interp.c we refer to the sim_state and sim_cpu directly. */
#define SD sd
#define CPU cpu


/* The following reserved instruction value is used when a simulator
   trap is required. NOTE: Care must be taken, since this value may be
   used in later revisions of the MIPS ISA. */
#define RSVD_INSTRUCTION           (0x00000005)
#define RSVD_INSTRUCTION_MASK      (0xFC00003F)

#define RSVD_INSTRUCTION_ARG_SHIFT 6
#define RSVD_INSTRUCTION_ARG_MASK  0xFFFFF  


/* Bits in the Debug register */
#define Debug_DBD 0x80000000   /* Debug Branch Delay */
#define Debug_DM  0x40000000   /* Debug Mode         */
#define Debug_DBp 0x00000002   /* Debug Breakpoint indicator */





/*---------------------------------------------------------------------------*/
/*-- GDB simulator interface ------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static void ColdReset PARAMS((SIM_DESC sd));

/*---------------------------------------------------------------------------*/



#define DELAYSLOT()     {\
                          if (STATE & simDELAYSLOT)\
                            sim_io_eprintf(sd,"Delay slot already activated (branch in delay slot?)\n");\
                          STATE |= simDELAYSLOT;\
                        }

#define JALDELAYSLOT()	{\
			  DELAYSLOT ();\
			  STATE |= simJALDELAYSLOT;\
			}

#define NULLIFY()       {\
                          STATE &= ~simDELAYSLOT;\
                          STATE |= simSKIPNEXT;\
                        }

#define CANCELDELAYSLOT() {\
                            DSSTATE = 0;\
                            STATE &= ~(simDELAYSLOT | simJALDELAYSLOT);\
                          }

#define INDELAYSLOT()	((STATE & simDELAYSLOT) != 0)
#define INJALDELAYSLOT() ((STATE & simJALDELAYSLOT) != 0)

#define K0BASE  (0x80000000)
#define K0SIZE  (0x20000000)
#define K1BASE  (0xA0000000)
#define K1SIZE  (0x20000000)
#define MONITOR_BASE (0xBFC00000)
#define MONITOR_SIZE (1 << 11)
#define MEM_SIZE (2 << 20)

#if defined(TRACE)
static char *tracefile = "trace.din"; /* default filename for trace log */
FILE *tracefh = NULL;
static void open_trace PARAMS((SIM_DESC sd));
#endif /* TRACE */

#define OPTION_DINERO_TRACE  200
#define OPTION_DINERO_FILE   201

static SIM_RC
mips_option_handler (sd, opt, arg)
     SIM_DESC sd;
     int opt;
     char *arg;
{
  int cpu_nr;
  switch (opt)
    {
    case OPTION_DINERO_TRACE: /* ??? */
#if defined(TRACE)
      /* Eventually the simTRACE flag could be treated as a toggle, to
	 allow external control of the program points being traced
	 (i.e. only from main onwards, excluding the run-time setup,
	 etc.). */
      for (cpu_nr = 0; cpu_nr < sim_engine_nr_cpus (sd); cpu_nr++)
	{
	  sim_cpu *cpu = STATE_CPU (sd, cpu_nr);
	  if (arg == NULL)
	    STATE |= simTRACE;
	  else if (strcmp (arg, "yes") == 0)
	    STATE |= simTRACE;
	  else if (strcmp (arg, "no") == 0)
	    STATE &= ~simTRACE;
	  else if (strcmp (arg, "on") == 0)
	    STATE |= simTRACE;
	  else if (strcmp (arg, "off") == 0)
	    STATE &= ~simTRACE;
	  else
	    {
	      fprintf (stderr, "Unreconized dinero-trace option `%s'\n", arg);
	      return SIM_RC_FAIL;
	    }
	}
      return SIM_RC_OK;
#else /* !TRACE */
      fprintf(stderr,"\
Simulator constructed without dinero tracing support (for performance).\n\
Re-compile simulator with \"-DTRACE\" to enable this option.\n");
      return SIM_RC_FAIL;
#endif /* !TRACE */

    case OPTION_DINERO_FILE:
#if defined(TRACE)
      if (optarg != NULL) {
	char *tmp;
	tmp = (char *)malloc(strlen(optarg) + 1);
	if (tmp == NULL)
	  {
	    sim_io_printf(sd,"Failed to allocate buffer for tracefile name \"%s\"\n",optarg);
	    return SIM_RC_FAIL;
	  }
	else {
	  strcpy(tmp,optarg);
	  tracefile = tmp;
	  sim_io_printf(sd,"Placing trace information into file \"%s\"\n",tracefile);
	}
      }
#endif /* TRACE */
      return SIM_RC_OK;

    }

  return SIM_RC_OK;
}

static const OPTION mips_options[] =
{
  { {"dinero-trace", optional_argument, NULL, OPTION_DINERO_TRACE},
      '\0', "on|off", "Enable dinero tracing",
      mips_option_handler },
  { {"dinero-file", required_argument, NULL, OPTION_DINERO_FILE},
      '\0', "FILE", "Write dinero trace to FILE",
      mips_option_handler },
  { {NULL, no_argument, NULL, 0}, '\0', NULL, NULL, NULL }
};


int interrupt_pending;

static void
interrupt_event (SIM_DESC sd, void *data)
{
  sim_cpu *cpu = STATE_CPU (sd, 0); /* FIXME */
  if (SR & status_IE)
    {
      interrupt_pending = 0;
      SignalExceptionInterrupt ();
    }
  else if (!interrupt_pending)
    sim_events_schedule (sd, 1, interrupt_event, data);
}



/*---------------------------------------------------------------------------*/
/*-- GDB simulator interface ------------------------------------------------*/
/*---------------------------------------------------------------------------*/

SIM_DESC
sim_open (kind, cb, abfd, argv)
     SIM_OPEN_KIND kind;
     host_callback *cb;
     struct _bfd *abfd;
     char **argv;
{
  SIM_DESC sd = sim_state_alloc (kind, cb);
  sim_cpu *cpu = STATE_CPU (sd, 0); /* FIXME */

  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);

  /* FIXME: watchpoints code shouldn't need this */
  STATE_WATCHPOINTS (sd)->pc = &(PC);
  STATE_WATCHPOINTS (sd)->sizeof_pc = sizeof (PC);
  STATE_WATCHPOINTS (sd)->interrupt_handler = interrupt_event;

  STATE = 0;
  
  if (sim_pre_argv_init (sd, argv[0]) != SIM_RC_OK)
    return 0;
  sim_add_option_table (sd, mips_options);

  /* Allocate core managed memory */

  /* the monitor  */
  sim_do_commandf (sd, "memory region 0x%lx,0x%lx", MONITOR_BASE, MONITOR_SIZE);
  /* For compatibility with the old code - under this (at level one)
     are the kernel spaces K0 & K1.  Both of these map to a single
     smaller sub region */
  sim_do_commandf (sd, "memory alias 0x%lx@1,0x%lx%%0x%lx,0x%0x",
		   K1BASE, K0SIZE,
		   MEM_SIZE, /* actual size */
		   K0BASE);

  /* getopt will print the error message so we just have to exit if this fails.
     FIXME: Hmmm...  in the case of gdb we need getopt to call
     print_filtered.  */
  if (sim_parse_args (sd, argv) != SIM_RC_OK)
    {
      /* Uninstall the modules to avoid memory leaks,
	 file descriptor leaks, etc.  */
      sim_module_uninstall (sd);
      return 0;
    }

  /* check for/establish the a reference program image */
  if (sim_analyze_program (sd,
			   (STATE_PROG_ARGV (sd) != NULL
			    ? *STATE_PROG_ARGV (sd)
			    : NULL),
			   abfd) != SIM_RC_OK)
    {
      sim_module_uninstall (sd);
      return 0;
    }

  /* Configure/verify the target byte order and other runtime
     configuration options */
  if (sim_config (sd) != SIM_RC_OK)
    {
      sim_module_uninstall (sd);
      return 0;
    }

  if (sim_post_argv_init (sd) != SIM_RC_OK)
    {
      /* Uninstall the modules to avoid memory leaks,
	 file descriptor leaks, etc.  */
      sim_module_uninstall (sd);
      return 0;
    }

  /* verify assumptions the simulator made about the host type system.
     This macro does not return if there is a problem */
  SIM_ASSERT (sizeof(int) == (4 * sizeof(char)));
  SIM_ASSERT (sizeof(word64) == (8 * sizeof(char)));

  /* This is NASTY, in that we are assuming the size of specific
     registers: */
  {
    int rn;
    for (rn = 0; (rn < (LAST_EMBED_REGNUM + 1)); rn++)
      {
	if (rn < 32)
	  cpu->register_widths[rn] = WITH_TARGET_WORD_BITSIZE;
	else if ((rn >= FGRIDX) && (rn < (FGRIDX + NR_FGR)))
	  cpu->register_widths[rn] = WITH_TARGET_FLOATING_POINT_BITSIZE;
	else if ((rn >= 33) && (rn <= 37))
	  cpu->register_widths[rn] = WITH_TARGET_WORD_BITSIZE;
	else if ((rn == SRIDX) || (rn == FCR0IDX) || (rn == FCR31IDX) || ((rn >= 72) && (rn <= 89)))
	  cpu->register_widths[rn] = 32;
	else
	  cpu->register_widths[rn] = 0;
      }
  }

#if defined(TRACE)
  if (STATE & simTRACE)
    open_trace(sd);
#endif /* TRACE */

  /* Write the monitor trap address handlers into the monitor (eeprom)
     address space.  This can only be done once the target endianness
     has been determined. */
  {
    unsigned loop;
    /* Entry into the IDT monitor is via fixed address vectors, and
       not using machine instructions. To avoid clashing with use of
       the MIPS TRAP system, we place our own (simulator specific)
       "undefined" instructions into the relevant vector slots. */
    for (loop = 0; (loop < MONITOR_SIZE); loop += 4)
      {
	address_word vaddr = (MONITOR_BASE + loop);
	unsigned32 insn = (RSVD_INSTRUCTION | (((loop >> 2) & RSVD_INSTRUCTION_ARG_MASK) << RSVD_INSTRUCTION_ARG_SHIFT));
	H2T (insn);
	sim_write (sd, vaddr, (char *)&insn, sizeof (insn));
      }
    /* The PMON monitor uses the same address space, but rather than
       branching into it the address of a routine is loaded. We can
       cheat for the moment, and direct the PMON routine to IDT style
       instructions within the monitor space. This relies on the IDT
       monitor not using the locations from 0xBFC00500 onwards as its
       entry points.*/
    for (loop = 0; (loop < 24); loop++)
      {
        address_word vaddr = (MONITOR_BASE + 0x500 + (loop * 4));
        unsigned32 value = ((0x500 - 8) / 8); /* default UNDEFINED reason code */
        switch (loop)
          {
            case 0: /* read */
              value = 7;
              break;
            case 1: /* write */
              value = 8;
              break;
            case 2: /* open */
              value = 6;
              break;
            case 3: /* close */
              value = 10;
              break;
            case 5: /* printf */
              value = ((0x500 - 16) / 8); /* not an IDT reason code */
              break;
            case 8: /* cliexit */
              value = 17;
              break;
            case 11: /* flush_cache */
              value = 28;
              break;
          }
	/* FIXME - should monitor_base be SIM_ADDR?? */
        value = ((unsigned int)MONITOR_BASE + (value * 8));
	H2T (value);
	sim_write (sd, vaddr, (char *)&value, sizeof (value));

	/* The LSI MiniRISC PMON has its vectors at 0x200, not 0x500.  */
	vaddr -= 0x300;
	sim_write (sd, vaddr, (char *)&value, sizeof (value));
      }
  }

  return sd;
}

#if defined(TRACE)
static void
open_trace(sd)
     SIM_DESC sd;
{
  tracefh = fopen(tracefile,"wb+");
  if (tracefh == NULL)
    {
      sim_io_eprintf(sd,"Failed to create file \"%s\", writing trace information to stderr.\n",tracefile);
      tracefh = stderr;
  }
}
#endif /* TRACE */

void
sim_close (sd, quitting)
     SIM_DESC sd;
     int quitting;
{
#ifdef DEBUG
  printf("DBG: sim_close: entered (quitting = %d)\n",quitting);
#endif

  /* "quitting" is non-zero if we cannot hang on errors */

  /* Ensure that any resources allocated through the callback
     mechanism are released: */
  sim_io_shutdown (sd);

#if defined(TRACE)
  if (tracefh != NULL && tracefh != stderr)
   fclose(tracefh);
  tracefh = NULL;
#endif /* TRACE */

  /* FIXME - free SD */

  return;
}


int
sim_write (sd,addr,buffer,size)
     SIM_DESC sd;
     SIM_ADDR addr;
     unsigned char *buffer;
     int size;
{
  int index;
  sim_cpu *cpu = STATE_CPU (sd, 0); /* FIXME */

  /* Return the number of bytes written, or zero if error. */
#ifdef DEBUG
  sim_io_printf(sd,"sim_write(0x%s,buffer,%d);\n",pr_addr(addr),size);
#endif

  /* We use raw read and write routines, since we do not want to count
     the GDB memory accesses in our statistics gathering. */

  for (index = 0; index < size; index++)
    {
      address_word vaddr = (address_word)addr + index;
      address_word paddr;
      int cca;
      if (!address_translation (SD, CPU, NULL_CIA, vaddr, isDATA, isSTORE, &paddr, &cca, isRAW))
	break;
      if (sim_core_write_buffer (SD, CPU, sim_core_read_map, buffer + index, paddr, 1) != 1)
	break;
    }

  return(index);
}

int
sim_read (sd,addr,buffer,size)
     SIM_DESC sd;
     SIM_ADDR addr;
     unsigned char *buffer;
     int size;
{
  int index;
  sim_cpu *cpu = STATE_CPU (sd, 0); /* FIXME */

  /* Return the number of bytes read, or zero if error. */
#ifdef DEBUG
  sim_io_printf(sd,"sim_read(0x%s,buffer,%d);\n",pr_addr(addr),size);
#endif /* DEBUG */

  for (index = 0; (index < size); index++)
    {
      address_word vaddr = (address_word)addr + index;
      address_word paddr;
      int cca;
      if (!address_translation (SD, CPU, NULL_CIA, vaddr, isDATA, isLOAD, &paddr, &cca, isRAW))
	break;
      if (sim_core_read_buffer (SD, CPU, sim_core_read_map, buffer + index, paddr, 1) != 1)
	break;
    }

  return(index);
}

void
sim_store_register (sd,rn,memory)
     SIM_DESC sd;
     int rn;
     unsigned char *memory;
{
  sim_cpu *cpu = STATE_CPU (sd, 0); /* FIXME */
  /* NOTE: gdb (the client) stores registers in target byte order
     while the simulator uses host byte order */
#ifdef DEBUG
  sim_io_printf(sd,"sim_store_register(%d,*memory=0x%s);\n",rn,pr_addr(*((SIM_ADDR *)memory)));
#endif /* DEBUG */

  /* Unfortunately this suffers from the same problem as the register
     numbering one. We need to know what the width of each logical
     register number is for the architecture being simulated. */

  if (cpu->register_widths[rn] == 0)
    sim_io_eprintf(sd,"Invalid register width for %d (register store ignored)\n",rn);
  else if (rn >= FGRIDX && rn < FGRIDX + NR_FGR)
    {
      if (cpu->register_widths[rn] == 32)
	cpu->fgr[rn - FGRIDX] = T2H_4 (*(unsigned32*)memory);
      else
	cpu->fgr[rn - FGRIDX] = T2H_8 (*(unsigned64*)memory);
    }
  else if (cpu->register_widths[rn] == 32)
    cpu->registers[rn] = T2H_4 (*(unsigned32*)memory);
  else
    cpu->registers[rn] = T2H_8 (*(unsigned64*)memory);

  return;
}

void
sim_fetch_register (sd,rn,memory)
     SIM_DESC sd;
     int rn;
     unsigned char *memory;
{
  sim_cpu *cpu = STATE_CPU (sd, 0); /* FIXME */
  /* NOTE: gdb (the client) stores registers in target byte order
     while the simulator uses host byte order */
#ifdef DEBUG
  sim_io_printf(sd,"sim_fetch_register(%d=0x%s,mem) : place simulator registers into memory\n",rn,pr_addr(registers[rn]));
#endif /* DEBUG */

  if (cpu->register_widths[rn] == 0)
    sim_io_eprintf(sd,"Invalid register width for %d (register fetch ignored)\n",rn);
  else if (rn >= FGRIDX && rn < FGRIDX + NR_FGR)
    {
      if (cpu->register_widths[rn] == 32)
	*(unsigned32*)memory = H2T_4 (cpu->fgr[rn - FGRIDX]);
      else
	*(unsigned64*)memory = H2T_8 (cpu->fgr[rn - FGRIDX]);
    }
  else if (cpu->register_widths[rn] == 32)
    *(unsigned32*)memory = H2T_4 ((unsigned32)(cpu->registers[rn]));
  else /* 64bit register */
    *(unsigned64*)memory = H2T_8 ((unsigned64)(cpu->registers[rn]));

  return;
}


void
sim_info (sd,verbose)
     SIM_DESC sd;
     int verbose;
{
  /* Accessed from the GDB "info files" command: */
  if (STATE_VERBOSE_P (sd) || verbose)
    {
      
      sim_io_printf (sd, "MIPS %d-bit %s endian simulator\n",
		     WITH_TARGET_WORD_BITSIZE,
		     (CURRENT_TARGET_BYTE_ORDER == BIG_ENDIAN ? "Big" : "Little"));
      
#if !defined(FASTSIM)
      /* It would be a useful feature, if when performing multi-cycle
	 simulations (rather than single-stepping) we keep the start and
	 end times of the execution, so that we can give a performance
	 figure for the simulator. */
#endif /* !FASTSIM */
      sim_io_printf (sd, "Number of execution cycles = %ld\n",
		     (long) sim_events_time (sd));
      
      /* print information pertaining to MIPS ISA and architecture being simulated */
      /* things that may be interesting */
      /* instructions executed - if available */
      /* cycles executed - if available */
      /* pipeline stalls - if available */
      /* virtual time taken */
      /* profiling size */
      /* profiling frequency */
      /* profile minpc */
      /* profile maxpc */
    }
  profile_print (sd, STATE_VERBOSE_P (sd), NULL, NULL);
}


SIM_RC
sim_create_inferior (sd, abfd, argv,env)
     SIM_DESC sd;
     struct _bfd *abfd;
     char **argv;
     char **env;
{

#ifdef DEBUG
  printf("DBG: sim_create_inferior entered: start_address = 0x%s\n",
	 pr_addr(PC));
#endif /* DEBUG */

  ColdReset(sd);

  if (abfd != NULL)
    {
      /* override PC value set by ColdReset () */
      int cpu_nr;
      for (cpu_nr = 0; cpu_nr < sim_engine_nr_cpus (sd); cpu_nr++)
	{
	  sim_cpu *cpu = STATE_CPU (sd, cpu_nr);
	  CIA_SET (cpu, (unsigned64) bfd_get_start_address (abfd));
	}
    }

#if 0 /* def DEBUG */
  if (argv || env)
    {
      /* We should really place the argv slot values into the argument
	 registers, and onto the stack as required. However, this
	 assumes that we have a stack defined, which is not
	 necessarily true at the moment. */
      char **cptr;
      sim_io_printf(sd,"sim_create_inferior() : passed arguments ignored\n");
      for (cptr = argv; (cptr && *cptr); cptr++)
	printf("DBG: arg \"%s\"\n",*cptr);
    }
#endif /* DEBUG */

  return SIM_RC_OK;
}

void
sim_do_command (sd,cmd)
     SIM_DESC sd;
     char *cmd;
{
  if (sim_args_command (sd, cmd) != SIM_RC_OK)
    sim_io_printf (sd, "Error: \"%s\" is not a valid MIPS simulator command.\n",
		   cmd);
}

/*---------------------------------------------------------------------------*/
/*-- Private simulator support interface ------------------------------------*/
/*---------------------------------------------------------------------------*/

/* Read a null terminated string from memory, return in a buffer */
static char *
fetch_str (sd, addr)
     SIM_DESC sd;
     address_word addr;
{
  char *buf;
  int nr = 0;
  char null;
  while (sim_read (sd, addr + nr, &null, 1) == 1 && null != 0)
    nr++;
  buf = NZALLOC (char, nr + 1);
  sim_read (sd, addr, buf, nr);
  return buf;
}

/* Simple monitor interface (currently setup for the IDT and PMON monitors) */
static void
sim_monitor (SIM_DESC sd,
	     sim_cpu *cpu,
	     address_word cia,
	     unsigned int reason)
{
#ifdef DEBUG
  printf("DBG: sim_monitor: entered (reason = %d)\n",reason);
#endif /* DEBUG */

  /* The IDT monitor actually allows two instructions per vector
     slot. However, the simulator currently causes a trap on each
     individual instruction. We cheat, and lose the bottom bit. */
  reason >>= 1;

  /* The following callback functions are available, however the
     monitor we are simulating does not make use of them: get_errno,
     isatty, lseek, rename, system, time and unlink */
  switch (reason)
    {

    case 6: /* int open(char *path,int flags) */
      {
	char *path = fetch_str (sd, A0);
	V0 = sim_io_open (sd, path, (int)A1);
	zfree (path);
	break;
      }

    case 7: /* int read(int file,char *ptr,int len) */
      {
	int fd = A0;
	int nr = A2;
	char *buf = zalloc (nr);
	V0 = sim_io_read (sd, fd, buf, nr);
	sim_write (sd, A1, buf, nr);
	zfree (buf);
      }
      break;

    case 8: /* int write(int file,char *ptr,int len) */
      {
	int fd = A0;
	int nr = A2;
	char *buf = zalloc (nr);
	sim_read (sd, A1, buf, nr);
	V0 = sim_io_write (sd, fd, buf, nr);
	zfree (buf);
	break;
      }

    case 10: /* int close(int file) */
      {
	V0 = sim_io_close (sd, (int)A0);
	break;
      }

    case 2:  /* Densan monitor: char inbyte(int waitflag) */
      {
	if (A0 == 0)	/* waitflag == NOWAIT */
	  V0 = (unsigned_word)-1;
      }
     /* Drop through to case 11 */

    case 11: /* char inbyte(void) */
      {
        char tmp;
        if (sim_io_read_stdin (sd, &tmp, sizeof(char)) != sizeof(char))
	  {
	    sim_io_error(sd,"Invalid return from character read");
	    V0 = (unsigned_word)-1;
	  }
        else
	  V0 = (unsigned_word)tmp;
	break;
      }

    case 3:  /* Densan monitor: void co(char chr) */
    case 12: /* void outbyte(char chr) : write a byte to "stdout" */
      {
        char tmp = (char)(A0 & 0xFF);
        sim_io_write_stdout (sd, &tmp, sizeof(char));
	break;
      }

    case 17: /* void _exit() */
      {
	sim_io_eprintf (sd, "sim_monitor(17): _exit(int reason) to be coded\n");
	sim_engine_halt (SD, CPU, NULL, NULL_CIA, sim_exited,
			 (unsigned int)(A0 & 0xFFFFFFFF));
	break;
      }

    case 28 : /* PMON flush_cache */
      break;

    case 55: /* void get_mem_info(unsigned int *ptr) */
      /* in:  A0 = pointer to three word memory location */
      /* out: [A0 + 0] = size */
      /*      [A0 + 4] = instruction cache size */
      /*      [A0 + 8] = data cache size */
      {
	address_word value = MEM_SIZE /* FIXME STATE_MEM_SIZE (sd) */;
	H2T (value);
	sim_write (sd, A0, (char *)&value, sizeof (value));
	/* sim_io_eprintf (sd, "sim: get_mem_info() depreciated\n"); */
	break;
      }
    
    case 158 : /* PMON printf */
      /* in:  A0 = pointer to format string */
      /*      A1 = optional argument 1 */
      /*      A2 = optional argument 2 */
      /*      A3 = optional argument 3 */
      /* out: void */
      /* The following is based on the PMON printf source */
      {
	address_word s = A0;
	char c;
	signed_word *ap = &A1; /* 1st argument */
        /* This isn't the quickest way, since we call the host print
           routine for every character almost. But it does avoid
           having to allocate and manage a temporary string buffer. */
	/* TODO: Include check that we only use three arguments (A1,
           A2 and A3) */
	while (sim_read (sd, s++, &c, 1) && c != '\0')
	  {
            if (c == '%')
	      {
		char tmp[40];
		enum {FMT_RJUST, FMT_LJUST, FMT_RJUST0, FMT_CENTER} fmt = FMT_RJUST;
		int width = 0, trunc = 0, haddot = 0, longlong = 0;
		while (sim_read (sd, s++, &c, 1) && c != '\0')
		  {
		    if (strchr ("dobxXulscefg%", s))
		      break;
		    else if (c == '-')
		      fmt = FMT_LJUST;
		    else if (c == '0')
		      fmt = FMT_RJUST0;
		    else if (c == '~')
		      fmt = FMT_CENTER;
		    else if (c == '*')
		      {
			if (haddot)
			  trunc = (int)*ap++;
			else
			  width = (int)*ap++;
		      }
		    else if (c >= '1' && c <= '9')
		      {
			address_word t = s;
			unsigned int n;
			while (sim_read (sd, s++, &c, 1) == 1 && isdigit (c))
			  tmp[s - t] = c;
			tmp[s - t] = '\0';
			n = (unsigned int)strtol(tmp,NULL,10);
			if (haddot)
			  trunc = n;
			else
			  width = n;
			s--;
		      }
		    else if (c == '.')
		      haddot = 1;
		  }
		switch (c)
		  {
		  case '%':
		    sim_io_printf (sd, "%%");
		    break;
		  case 's':
		    if ((int)*ap != 0)
		      {
			address_word p = *ap++;
			char ch;
			while (sim_read (sd, p++, &ch, 1) == 1 && ch != '\0')
			  sim_io_printf(sd, "%c", ch);
		      }
		    else
		      sim_io_printf(sd,"(null)");
		    break;
		  case 'c':
		    sim_io_printf (sd, "%c", (int)*ap++);
		    break;
		  default:
		    if (c == 'l')
		      {
			sim_read (sd, s++, &c, 1);
			if (c == 'l')
			  {
			    longlong = 1;
			    sim_read (sd, s++, &c, 1);
			  }
		      }
		    if (strchr ("dobxXu", c))
		      {
			word64 lv = (word64) *ap++;
			if (c == 'b')
			  sim_io_printf(sd,"<binary not supported>");
			else
			  {
			    sprintf (tmp, "%%%s%c", longlong ? "ll" : "", c);
			    if (longlong)
			      sim_io_printf(sd, tmp, lv);
			    else
			      sim_io_printf(sd, tmp, (int)lv);
			  }
		      }
		    else if (strchr ("eEfgG", c))
		      {
			double dbl = *(double*)(ap++);
			sprintf (tmp, "%%%d.%d%c", width, trunc, c);
			sim_io_printf (sd, tmp, dbl);
			trunc = 0;
		      }
		  }
	      }
	    else
	      sim_io_printf(sd, "%c", c);
	  }
	break;
      }

    default:
      sim_io_error (sd, "TODO: sim_monitor(%d) : PC = 0x%s\n",
		    reason, pr_addr(cia));
      break;
  }
  return;
}

/* Store a word into memory.  */

static void
store_word (SIM_DESC sd,
	    sim_cpu *cpu,
	    address_word cia,
	    uword64 vaddr,
	    signed_word val)
{
  address_word paddr;
  int uncached;

  if ((vaddr & 3) != 0)
    SignalExceptionAddressStore ();
  else
    {
      if (AddressTranslation (vaddr, isDATA, isSTORE, &paddr, &uncached,
			      isTARGET, isREAL))
	{
	  const uword64 mask = 7;
	  uword64 memval;
	  unsigned int byte;

	  paddr = (paddr & ~mask) | ((paddr & mask) ^ (ReverseEndian << 2));
	  byte = (vaddr & mask) ^ (BigEndianCPU << 2);
	  memval = ((uword64) val) << (8 * byte);
	  StoreMemory (uncached, AccessLength_WORD, memval, 0, paddr, vaddr,
		       isREAL);
	}
    }
}

/* Load a word from memory.  */

static signed_word
load_word (SIM_DESC sd,
	   sim_cpu *cpu,
	   address_word cia,
	   uword64 vaddr)
{
  if ((vaddr & 3) != 0)
    SignalExceptionAddressLoad ();
  else
    {
      address_word paddr;
      int uncached;

      if (AddressTranslation (vaddr, isDATA, isLOAD, &paddr, &uncached,
			      isTARGET, isREAL))
	{
	  const uword64 mask = 0x7;
	  const unsigned int reverse = ReverseEndian ? 1 : 0;
	  const unsigned int bigend = BigEndianCPU ? 1 : 0;
	  uword64 memval;
	  unsigned int byte;

	  paddr = (paddr & ~mask) | ((paddr & mask) ^ (reverse << 2));
	  LoadMemory (&memval,NULL,uncached, AccessLength_WORD, paddr, vaddr,
			       isDATA, isREAL);
	  byte = (vaddr & mask) ^ (bigend << 2);
	  return SIGNEXTEND (((memval >> (8 * byte)) & 0xffffffff), 32);
	}
    }

  return 0;
}

/* Simulate the mips16 entry and exit pseudo-instructions.  These
   would normally be handled by the reserved instruction exception
   code, but for ease of simulation we just handle them directly.  */

static void
mips16_entry (SIM_DESC sd,
	      sim_cpu *cpu,
	      address_word cia,
	      unsigned int insn)
{
  int aregs, sregs, rreg;

#ifdef DEBUG
  printf("DBG: mips16_entry: entered (insn = 0x%08X)\n",insn);
#endif /* DEBUG */

  aregs = (insn & 0x700) >> 8;
  sregs = (insn & 0x0c0) >> 6;
  rreg =  (insn & 0x020) >> 5;

  /* This should be checked by the caller.  */
  if (sregs == 3)
    abort ();

  if (aregs < 5)
    {
      int i;
      signed_word tsp;

      /* This is the entry pseudo-instruction.  */

      for (i = 0; i < aregs; i++)
	store_word (SD, CPU, cia, (uword64) (SP + 4 * i), GPR[i + 4]);

      tsp = SP;
      SP -= 32;

      if (rreg)
	{
	  tsp -= 4;
	  store_word (SD, CPU, cia, (uword64) tsp, RA);
	}

      for (i = 0; i < sregs; i++)
	{
	  tsp -= 4;
	  store_word (SD, CPU, cia, (uword64) tsp, GPR[16 + i]);
	}
    }
  else
    {
      int i;
      signed_word tsp;

      /* This is the exit pseudo-instruction.  */

      tsp = SP + 32;

      if (rreg)
	{
	  tsp -= 4;
	  RA = load_word (SD, CPU, cia, (uword64) tsp);
	}

      for (i = 0; i < sregs; i++)
	{
	  tsp -= 4;
	  GPR[i + 16] = load_word (SD, CPU, cia, (uword64) tsp);
	}

      SP += 32;

      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT)
	{
	  if (aregs == 5)
	    {
	      FGR[0] = WORD64LO (GPR[4]);
	      FPR_STATE[0] = fmt_uninterpreted;
	    }
	  else if (aregs == 6)
	    {
	      FGR[0] = WORD64LO (GPR[5]);
	      FGR[1] = WORD64LO (GPR[4]);
	      FPR_STATE[0] = fmt_uninterpreted;
	      FPR_STATE[1] = fmt_uninterpreted;
	    }
	}	  

      PC = RA;
    }
  
}

/*-- trace support ----------------------------------------------------------*/

/* The TRACE support is provided (if required) in the memory accessing
   routines. Since we are also providing the architecture specific
   features, the architecture simulation code can also deal with
   notifying the TRACE world of cache flushes, etc. Similarly we do
   not need to provide profiling support in the simulator engine,
   since we can sample in the instruction fetch control loop. By
   defining the TRACE manifest, we add tracing as a run-time
   option. */

#if defined(TRACE)
/* Tracing by default produces "din" format (as required by
   dineroIII). Each line of such a trace file *MUST* have a din label
   and address field. The rest of the line is ignored, so comments can
   be included if desired. The first field is the label which must be
   one of the following values:

	0       read data
        1       write data
        2       instruction fetch
        3       escape record (treated as unknown access type)
        4       escape record (causes cache flush)

   The address field is a 32bit (lower-case) hexadecimal address
   value. The address should *NOT* be preceded by "0x".

   The size of the memory transfer is not important when dealing with
   cache lines (as long as no more than a cache line can be
   transferred in a single operation :-), however more information
   could be given following the dineroIII requirement to allow more
   complete memory and cache simulators to provide better
   results. i.e. the University of Pisa has a cache simulator that can
   also take bus size and speed as (variable) inputs to calculate
   complete system performance (a much more useful ability when trying
   to construct an end product, rather than a processor). They
   currently have an ARM version of their tool called ChARM. */


void
dotrace (SIM_DESC sd,
	 sim_cpu *cpu,
	 FILE *tracefh,
	 int type,
	 SIM_ADDR address,
	 int width,
	 char *comment,...)
{
  if (STATE & simTRACE) {
    va_list ap;
    fprintf(tracefh,"%d %s ; width %d ; ", 
		type,
		pr_addr(address),
		width);
    va_start(ap,comment);
    vfprintf(tracefh,comment,ap);
    va_end(ap);
    fprintf(tracefh,"\n");
  }
  /* NOTE: Since the "din" format will only accept 32bit addresses, and
     we may be generating 64bit ones, we should put the hi-32bits of the
     address into the comment field. */

  /* TODO: Provide a buffer for the trace lines. We can then avoid
     performing writes until the buffer is filled, or the file is
     being closed. */

  /* NOTE: We could consider adding a comment field to the "din" file
     produced using type 3 markers (unknown access). This would then
     allow information about the program that the "din" is for, and
     the MIPs world that was being simulated, to be placed into the
     trace file. */

  return;
}
#endif /* TRACE */

/*---------------------------------------------------------------------------*/
/*-- simulator engine -------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static void
ColdReset (SIM_DESC sd)
{
  int cpu_nr;
  for (cpu_nr = 0; cpu_nr < sim_engine_nr_cpus (sd); cpu_nr++)
    {
      sim_cpu *cpu = STATE_CPU (sd, cpu_nr);
      /* RESET: Fixed PC address: */
      PC = UNSIGNED64 (0xFFFFFFFFBFC00000);
      /* The reset vector address is in the unmapped, uncached memory space. */
      
      SR &= ~(status_SR | status_TS | status_RP);
      SR |= (status_ERL | status_BEV);
      
      /* Cheat and allow access to the complete register set immediately */
      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT
	  && WITH_TARGET_WORD_BITSIZE == 64)
	SR |= status_FR; /* 64bit registers */
      
      /* Ensure that any instructions with pending register updates are
	 cleared: */
      PENDING_INVALIDATE();
      
      /* Initialise the FPU registers to the unknown state */
      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT)
	{
	  int rn;
	  for (rn = 0; (rn < 32); rn++)
	    FPR_STATE[rn] = fmt_uninterpreted;
	}
      
    }
}

/* Description from page A-22 of the "MIPS IV Instruction Set" manual
   (revision 3.1) */
/* Translate a virtual address to a physical address and cache
   coherence algorithm describing the mechanism used to resolve the
   memory reference. Given the virtual address vAddr, and whether the
   reference is to Instructions ot Data (IorD), find the corresponding
   physical address (pAddr) and the cache coherence algorithm (CCA)
   used to resolve the reference. If the virtual address is in one of
   the unmapped address spaces the physical address and the CCA are
   determined directly by the virtual address. If the virtual address
   is in one of the mapped address spaces then the TLB is used to
   determine the physical address and access type; if the required
   translation is not present in the TLB or the desired access is not
   permitted the function fails and an exception is taken.

   NOTE: Normally (RAW == 0), when address translation fails, this
   function raises an exception and does not return. */

int
address_translation (SIM_DESC sd,
		     sim_cpu *cpu,
		     address_word cia,
		     address_word vAddr,
		     int IorD,
		     int LorS,
		     address_word *pAddr,
		     int *CCA,
		     int raw)
{
  int res = -1; /* TRUE : Assume good return */

#ifdef DEBUG
  sim_io_printf(sd,"AddressTranslation(0x%s,%s,%s,...);\n",pr_addr(vAddr),(IorD ? "isDATA" : "isINSTRUCTION"),(LorS ? "iSTORE" : "isLOAD"));
#endif

  /* Check that the address is valid for this memory model */

  /* For a simple (flat) memory model, we simply pass virtual
     addressess through (mostly) unchanged. */
  vAddr &= 0xFFFFFFFF;

  *pAddr = vAddr; /* default for isTARGET */
  *CCA = Uncached; /* not used for isHOST */

  return(res);
}

/* Description from page A-23 of the "MIPS IV Instruction Set" manual
   (revision 3.1) */
/* Prefetch data from memory. Prefetch is an advisory instruction for
   which an implementation specific action is taken. The action taken
   may increase performance, but must not change the meaning of the
   program, or alter architecturally-visible state. */

void 
prefetch (SIM_DESC sd,
	  sim_cpu *cpu,
	  address_word cia,
	  int CCA,
	  address_word pAddr,
	  address_word vAddr,
	  int DATA,
	  int hint)
{
#ifdef DEBUG
  sim_io_printf(sd,"Prefetch(%d,0x%s,0x%s,%d,%d);\n",CCA,pr_addr(pAddr),pr_addr(vAddr),DATA,hint);
#endif /* DEBUG */

  /* For our simple memory model we do nothing */
  return;
}

/* Description from page A-22 of the "MIPS IV Instruction Set" manual
   (revision 3.1) */
/* Load a value from memory. Use the cache and main memory as
   specified in the Cache Coherence Algorithm (CCA) and the sort of
   access (IorD) to find the contents of AccessLength memory bytes
   starting at physical location pAddr. The data is returned in the
   fixed width naturally-aligned memory element (MemElem). The
   low-order two (or three) bits of the address and the AccessLength
   indicate which of the bytes within MemElem needs to be given to the
   processor. If the memory access type of the reference is uncached
   then only the referenced bytes are read from memory and valid
   within the memory element. If the access type is cached, and the
   data is not present in cache, an implementation specific size and
   alignment block of memory is read and loaded into the cache to
   satisfy a load reference. At a minimum, the block is the entire
   memory element. */
void
load_memory (SIM_DESC sd,
	     sim_cpu *cpu,
	     address_word cia,
	     uword64* memvalp,
	     uword64* memval1p,
	     int CCA,
	     int AccessLength,
	     address_word pAddr,
	     address_word vAddr,
	     int IorD)
{
  uword64 value = 0;
  uword64 value1 = 0;

#ifdef DEBUG
  sim_io_printf(sd,"DBG: LoadMemory(%p,%p,%d,%d,0x%s,0x%s,%s)\n",memvalp,memval1p,CCA,AccessLength,pr_addr(pAddr),pr_addr(vAddr),(IorD ? "isDATA" : "isINSTRUCTION"));
#endif /* DEBUG */

#if defined(WARN_MEM)
  if (CCA != uncached)
    sim_io_eprintf(sd,"LoadMemory CCA (%d) is not uncached (currently all accesses treated as cached)\n",CCA);
#endif /* WARN_MEM */

  /* If instruction fetch then we need to check that the two lo-order
     bits are zero, otherwise raise a InstructionFetch exception: */
  if ((IorD == isINSTRUCTION)
      && ((pAddr & 0x3) != 0)
      && (((pAddr & 0x1) != 0) || ((vAddr & 0x1) == 0)))
    SignalExceptionInstructionFetch ();

  if (((pAddr & LOADDRMASK) + AccessLength) > LOADDRMASK)
    {
      /* In reality this should be a Bus Error */
      sim_io_error (sd, "AccessLength of %d would extend over %dbit aligned boundary for physical address 0x%s\n",
		    AccessLength,
		    (LOADDRMASK + 1) << 2,
		    pr_addr (pAddr));
    }

#if defined(TRACE)
  dotrace (SD, CPU, tracefh,((IorD == isDATA) ? 0 : 2),(unsigned int)(pAddr&0xFFFFFFFF),(AccessLength + 1),"load%s",((IorD == isDATA) ? "" : " instruction"));
#endif /* TRACE */
  
  /* Read the specified number of bytes from memory.  Adjust for
     host/target byte ordering/ Align the least significant byte
     read. */

  switch (AccessLength)
    {
    case AccessLength_QUADWORD :
      {
	unsigned_16 val = sim_core_read_aligned_16 (cpu, NULL_CIA,
						    sim_core_read_map, pAddr);
	value1 = VH8_16 (val);
	value = VL8_16 (val);
	break;
      }
    case AccessLength_DOUBLEWORD :
      value = sim_core_read_aligned_8 (cpu, NULL_CIA,
				       sim_core_read_map, pAddr);
      break;
    case AccessLength_SEPTIBYTE :
      value = sim_core_read_misaligned_7 (cpu, NULL_CIA,
					  sim_core_read_map, pAddr);
    case AccessLength_SEXTIBYTE :
      value = sim_core_read_misaligned_6 (cpu, NULL_CIA,
					  sim_core_read_map, pAddr);
    case AccessLength_QUINTIBYTE :
      value = sim_core_read_misaligned_5 (cpu, NULL_CIA,
					  sim_core_read_map, pAddr);
    case AccessLength_WORD :
      value = sim_core_read_aligned_4 (cpu, NULL_CIA,
				       sim_core_read_map, pAddr);
      break;
    case AccessLength_TRIPLEBYTE :
      value = sim_core_read_misaligned_3 (cpu, NULL_CIA,
					  sim_core_read_map, pAddr);
    case AccessLength_HALFWORD :
      value = sim_core_read_aligned_2 (cpu, NULL_CIA,
				       sim_core_read_map, pAddr);
      break;
    case AccessLength_BYTE :
      value = sim_core_read_aligned_1 (cpu, NULL_CIA,
				       sim_core_read_map, pAddr);
      break;
    default:
      abort ();
    }
  
#ifdef DEBUG
  printf("DBG: LoadMemory() : (offset %d) : value = 0x%s%s\n",
	 (int)(pAddr & LOADDRMASK),pr_uword64(value1),pr_uword64(value));
#endif /* DEBUG */
  
  /* See also store_memory. */
  if (AccessLength <= AccessLength_DOUBLEWORD)
    {
      if (BigEndianMem)
	/* for big endian target, byte (pAddr&LOADDRMASK == 0) is
	   shifted to the most significant byte position.  */
	value <<= (((7 - (pAddr & LOADDRMASK)) - AccessLength) * 8);
      else
	/* For little endian target, byte (pAddr&LOADDRMASK == 0)
	   is already in the correct postition. */
	value <<= ((pAddr & LOADDRMASK) * 8);
    }
  
#ifdef DEBUG
  printf("DBG: LoadMemory() : shifted value = 0x%s%s\n",
	 pr_uword64(value1),pr_uword64(value));
#endif /* DEBUG */
  
  *memvalp = value;
  if (memval1p) *memval1p = value1;
}


/* Description from page A-23 of the "MIPS IV Instruction Set" manual
   (revision 3.1) */
/* Store a value to memory. The specified data is stored into the
   physical location pAddr using the memory hierarchy (data caches and
   main memory) as specified by the Cache Coherence Algorithm
   (CCA). The MemElem contains the data for an aligned, fixed-width
   memory element (word for 32-bit processors, doubleword for 64-bit
   processors), though only the bytes that will actually be stored to
   memory need to be valid. The low-order two (or three) bits of pAddr
   and the AccessLength field indicates which of the bytes within the
   MemElem data should actually be stored; only these bytes in memory
   will be changed. */

void
store_memory (SIM_DESC sd,
	      sim_cpu *cpu,
	      address_word cia,
	      int CCA,
	      int AccessLength,
	      uword64 MemElem,
	      uword64 MemElem1,   /* High order 64 bits */
	      address_word pAddr,
	      address_word vAddr)
{
#ifdef DEBUG
  sim_io_printf(sd,"DBG: StoreMemory(%d,%d,0x%s,0x%s,0x%s,0x%s)\n",CCA,AccessLength,pr_uword64(MemElem),pr_uword64(MemElem1),pr_addr(pAddr),pr_addr(vAddr));
#endif /* DEBUG */
  
#if defined(WARN_MEM)
  if (CCA != uncached)
    sim_io_eprintf(sd,"StoreMemory CCA (%d) is not uncached (currently all accesses treated as cached)\n",CCA);
#endif /* WARN_MEM */
  
  if (((pAddr & LOADDRMASK) + AccessLength) > LOADDRMASK)
    sim_io_error(sd,"AccessLength of %d would extend over %dbit aligned boundary for physical address 0x%s\n",AccessLength,(LOADDRMASK + 1)<<2,pr_addr(pAddr));
  
#if defined(TRACE)
  dotrace (SD, CPU, tracefh,1,(unsigned int)(pAddr&0xFFFFFFFF),(AccessLength + 1),"store");
#endif /* TRACE */
  
#ifdef DEBUG
  printf("DBG: StoreMemory: offset = %d MemElem = 0x%s%s\n",(unsigned int)(pAddr & LOADDRMASK),pr_uword64(MemElem1),pr_uword64(MemElem));
#endif /* DEBUG */
  
  /* See also load_memory */
  if (AccessLength <= AccessLength_DOUBLEWORD)
    {
      if (BigEndianMem)
	/* for big endian target, byte (pAddr&LOADDRMASK == 0) is
	   shifted to the most significant byte position.  */
	MemElem >>= (((7 - (pAddr & LOADDRMASK)) - AccessLength) * 8);
      else
	/* For little endian target, byte (pAddr&LOADDRMASK == 0)
	   is already in the correct postition. */
	MemElem >>= ((pAddr & LOADDRMASK) * 8);
    }
  
#ifdef DEBUG
  printf("DBG: StoreMemory: shift = %d MemElem = 0x%s%s\n",shift,pr_uword64(MemElem1),pr_uword64(MemElem));
#endif /* DEBUG */
  
  switch (AccessLength)
    {
    case AccessLength_QUADWORD :
      {
	unsigned_16 val = U16_8 (MemElem1, MemElem);
	sim_core_write_aligned_16 (cpu, NULL_CIA,
				   sim_core_write_map, pAddr, val);
	break;
      }
    case AccessLength_DOUBLEWORD :
      sim_core_write_aligned_8 (cpu, NULL_CIA,
				sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_SEPTIBYTE :
      sim_core_write_misaligned_7 (cpu, NULL_CIA,
				   sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_SEXTIBYTE :
      sim_core_write_misaligned_6 (cpu, NULL_CIA,
				   sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_QUINTIBYTE :
      sim_core_write_misaligned_5 (cpu, NULL_CIA,
				   sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_WORD :
      sim_core_write_aligned_4 (cpu, NULL_CIA,
				sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_TRIPLEBYTE :
      sim_core_write_misaligned_3 (cpu, NULL_CIA,
				   sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_HALFWORD :
      sim_core_write_aligned_2 (cpu, NULL_CIA,
				sim_core_write_map, pAddr, MemElem);
      break;
    case AccessLength_BYTE :
      sim_core_write_aligned_1 (cpu, NULL_CIA,
				sim_core_write_map, pAddr, MemElem);
      break;
    default:
      abort ();
    }	
  
  return;
}


unsigned32
ifetch32 (SIM_DESC sd,
	  sim_cpu *cpu,
	  address_word cia,
	  address_word vaddr)
{
  /* Copy the action of the LW instruction */
  address_word reverse = (ReverseEndian ? (LOADDRMASK >> 2) : 0);
  address_word bigend = (BigEndianCPU ? (LOADDRMASK >> 2) : 0);
  unsigned64 value;
  address_word paddr;
  unsigned32 instruction;
  unsigned byte;
  int cca;
  AddressTranslation (vaddr, isINSTRUCTION, isLOAD, &paddr, &cca, isTARGET, isREAL);
  paddr = ((paddr & ~LOADDRMASK) | ((paddr & LOADDRMASK) ^ (reverse << 2)));
  LoadMemory (&value, NULL, cca, AccessLength_WORD, paddr, vaddr, isINSTRUCTION, isREAL);
  byte = ((vaddr & LOADDRMASK) ^ (bigend << 2));
  instruction = ((value >> (8 * byte)) & 0xFFFFFFFF);
  return instruction;
}


/* Description from page A-26 of the "MIPS IV Instruction Set" manual (revision 3.1) */
/* Order loads and stores to synchronise shared memory. Perform the
   action necessary to make the effects of groups of synchronizable
   loads and stores indicated by stype occur in the same order for all
   processors. */
void
sync_operation (SIM_DESC sd,
		sim_cpu *cpu,
		address_word cia,
		int stype)
{
#ifdef DEBUG
  sim_io_printf(sd,"SyncOperation(%d) : TODO\n",stype);
#endif /* DEBUG */
  return;
}

/* Description from page A-26 of the "MIPS IV Instruction Set" manual (revision 3.1) */
/* Signal an exception condition. This will result in an exception
   that aborts the instruction. The instruction operation pseudocode
   will never see a return from this function call. */

void
signal_exception (SIM_DESC sd,
		  sim_cpu *cpu,
		  address_word cia,
		  int exception,...)
{
  int vector;

#ifdef DEBUG
  sim_io_printf(sd,"DBG: SignalException(%d) PC = 0x%s\n",exception,pr_addr(cia));
#endif /* DEBUG */

  /* Ensure that any active atomic read/modify/write operation will fail: */
  LLBIT = 0;

  switch (exception) {
    /* TODO: For testing purposes I have been ignoring TRAPs. In
       reality we should either simulate them, or allow the user to
       ignore them at run-time.
       Same for SYSCALL */
    case Trap :
     sim_io_eprintf(sd,"Ignoring instruction TRAP (PC 0x%s)\n",pr_addr(cia));
     break;

    case SystemCall :
      {
        va_list ap;
        unsigned int instruction;
        unsigned int code;

        va_start(ap,exception);
        instruction = va_arg(ap,unsigned int);
        va_end(ap);

        code = (instruction >> 6) & 0xFFFFF;
        
        sim_io_eprintf(sd,"Ignoring instruction `syscall %d' (PC 0x%s)\n",
                    code, pr_addr(cia));
      }
     break;

    case DebugBreakPoint :
      if (! (Debug & Debug_DM))
        {
          if (INDELAYSLOT())
            {
              CANCELDELAYSLOT();
              
              Debug |= Debug_DBD;  /* signaled from within in delay slot */
              DEPC = cia - 4;      /* reference the branch instruction */
            }
          else
            {
              Debug &= ~Debug_DBD; /* not signaled from within a delay slot */
              DEPC = cia;
            }
        
          Debug |= Debug_DM;            /* in debugging mode */
          Debug |= Debug_DBp;           /* raising a DBp exception */
          PC = 0xBFC00200;
          sim_engine_restart (SD, CPU, NULL, NULL_CIA);
        }
      break;

    case ReservedInstruction :
     {
       va_list ap;
       unsigned int instruction;
       va_start(ap,exception);
       instruction = va_arg(ap,unsigned int);
       va_end(ap);
       /* Provide simple monitor support using ReservedInstruction
          exceptions. The following code simulates the fixed vector
          entry points into the IDT monitor by causing a simulator
          trap, performing the monitor operation, and returning to
          the address held in the $ra register (standard PCS return
          address). This means we only need to pre-load the vector
          space with suitable instruction values. For systems were
          actual trap instructions are used, we would not need to
          perform this magic. */
       if ((instruction & RSVD_INSTRUCTION_MASK) == RSVD_INSTRUCTION)
	 {
	   sim_monitor (SD, CPU, cia, ((instruction >> RSVD_INSTRUCTION_ARG_SHIFT) & RSVD_INSTRUCTION_ARG_MASK) );
	   /* NOTE: This assumes that a branch-and-link style
	      instruction was used to enter the vector (which is the
	      case with the current IDT monitor). */
	   sim_engine_restart (SD, CPU, NULL, RA);
	 }
       /* Look for the mips16 entry and exit instructions, and
          simulate a handler for them.  */
       else if ((cia & 1) != 0
		&& (instruction & 0xf81f) == 0xe809
		&& (instruction & 0x0c0) != 0x0c0)
	 {
	   mips16_entry (SD, CPU, cia, instruction);
	   sim_engine_restart (sd, NULL, NULL, NULL_CIA);
	 }
       /* else fall through to normal exception processing */
       sim_io_eprintf(sd,"ReservedInstruction 0x%08X at PC = 0x%s\n",instruction,pr_addr(cia));
     }

    case BreakPoint:
#ifdef DEBUG
      sim_io_printf(sd,"DBG: SignalException(%d) PC = 0x%s\n",exception,pr_addr(cia));
#endif /* DEBUG */
      /* Keep a copy of the current A0 in-case this is the program exit
	 breakpoint:  */
      {
	va_list ap;
	unsigned int instruction;
	va_start(ap,exception);
	instruction = va_arg(ap,unsigned int);
	va_end(ap);
	/* Check for our special terminating BREAK: */
	if ((instruction & 0x03FFFFC0) == 0x03ff0000) {
	  sim_engine_halt (SD, CPU, NULL, cia,
			   sim_exited, (unsigned int)(A0 & 0xFFFFFFFF));
	}
      }
      if (STATE & simDELAYSLOT)
	PC = cia - 4; /* reference the branch instruction */
      else
	PC = cia;
      sim_engine_halt (SD, CPU, NULL, cia,
		       sim_stopped, SIM_SIGTRAP);

    default:
     /* Store exception code into current exception id variable (used
        by exit code): */

     /* TODO: If not simulating exceptions then stop the simulator
        execution. At the moment we always stop the simulation. */

     /* See figure 5-17 for an outline of the code below */
     if (! (SR & status_EXL))
       {
	 CAUSE = (exception << 2);
	 if (STATE & simDELAYSLOT)
	   {
	     STATE &= ~simDELAYSLOT;
	     CAUSE |= cause_BD;
	     EPC = (cia - 4); /* reference the branch instruction */
	   }
	 else
	   EPC = cia;
	 /* FIXME: TLB et.al. */
	 vector = 0x180;
       }
     else
       {
	 CAUSE = (exception << 2);
	 vector = 0x180;
       }
     SR |= status_EXL;
     /* Store exception code into current exception id variable (used
        by exit code): */
     if (SR & status_BEV)
       PC = (signed)0xBFC00200 + 0x180;
     else
       PC = (signed)0x80000000 + 0x180;

     switch ((CAUSE >> 2) & 0x1F)
       {
       case Interrupt:
	 /* Interrupts arrive during event processing, no need to
            restart */
	 return;
	 
       case TLBModification:
       case TLBLoad:
       case TLBStore:
       case AddressLoad:
       case AddressStore:
       case InstructionFetch:
       case DataReference:
	 /* The following is so that the simulator will continue from the
	    exception address on breakpoint operations. */
	 PC = EPC;
	 sim_engine_halt (SD, CPU, NULL, NULL_CIA,
			  sim_stopped, SIM_SIGBUS);

       case ReservedInstruction:
       case CoProcessorUnusable:
	 PC = EPC;
	 sim_engine_halt (SD, CPU, NULL, NULL_CIA,
			  sim_stopped, SIM_SIGILL);

       case IntegerOverflow:
       case FPE:
	 sim_engine_halt (SD, CPU, NULL, NULL_CIA,
			  sim_stopped, SIM_SIGFPE);

       case Trap:
       case Watch:
       case SystemCall:
	 PC = EPC;
	 sim_engine_halt (SD, CPU, NULL, NULL_CIA,
			  sim_stopped, SIM_SIGTRAP);

       case BreakPoint:
	 PC = EPC;
	 sim_engine_abort (SD, CPU, NULL_CIA,
			   "FATAL: Should not encounter a breakpoint\n");

       default : /* Unknown internal exception */
	 PC = EPC;
	 sim_engine_halt (SD, CPU, NULL, NULL_CIA,
			  sim_stopped, SIM_SIGABRT);

       }

    case SimulatorFault:
     {
       va_list ap;
       char *msg;
       va_start(ap,exception);
       msg = va_arg(ap,char *);
       va_end(ap);
       sim_engine_abort (SD, CPU, NULL_CIA,
			 "FATAL: Simulator error \"%s\"\n",msg);
     }
   }

  return;
}

#if defined(WARN_RESULT)
/* Description from page A-26 of the "MIPS IV Instruction Set" manual (revision 3.1) */
/* This function indicates that the result of the operation is
   undefined. However, this should not affect the instruction
   stream. All that is meant to happen is that the destination
   register is set to an undefined result. To keep the simulator
   simple, we just don't bother updating the destination register, so
   the overall result will be undefined. If desired we can stop the
   simulator by raising a pseudo-exception. */
#define UndefinedResult() undefined_result (sd,cia)
static void
undefined_result(sd,cia)
     SIM_DESC sd;
     address_word cia;
{
  sim_io_eprintf(sd,"UndefinedResult: PC = 0x%s\n",pr_addr(cia));
#if 0 /* Disabled for the moment, since it actually happens a lot at the moment. */
  state |= simSTOP;
#endif
  return;
}
#endif /* WARN_RESULT */

void
cache_op (SIM_DESC sd,
	  sim_cpu *cpu,
	  address_word cia,
	  int op,
	  address_word pAddr,
	  address_word vAddr,
	  unsigned int instruction)
{
#if 1 /* stop warning message being displayed (we should really just remove the code) */
  static int icache_warning = 1;
  static int dcache_warning = 1;
#else
  static int icache_warning = 0;
  static int dcache_warning = 0;
#endif

  /* If CP0 is not useable (User or Supervisor mode) and the CP0
     enable bit in the Status Register is clear - a coprocessor
     unusable exception is taken. */
#if 0
  sim_io_printf(sd,"TODO: Cache availability checking (PC = 0x%s)\n",pr_addr(cia));
#endif

  switch (op & 0x3) {
    case 0: /* instruction cache */
      switch (op >> 2) {
        case 0: /* Index Invalidate */
        case 1: /* Index Load Tag */
        case 2: /* Index Store Tag */
        case 4: /* Hit Invalidate */
        case 5: /* Fill */
        case 6: /* Hit Writeback */
          if (!icache_warning)
            {
              sim_io_eprintf(sd,"Instruction CACHE operation %d to be coded\n",(op >> 2));
              icache_warning = 1;
            }
          break;

        default:
          SignalException(ReservedInstruction,instruction);
          break;
      }
      break;

    case 1: /* data cache */
      switch (op >> 2) {
        case 0: /* Index Writeback Invalidate */
        case 1: /* Index Load Tag */
        case 2: /* Index Store Tag */
        case 3: /* Create Dirty */
        case 4: /* Hit Invalidate */
        case 5: /* Hit Writeback Invalidate */
        case 6: /* Hit Writeback */ 
          if (!dcache_warning)
            {
              sim_io_eprintf(sd,"Data CACHE operation %d to be coded\n",(op >> 2));
              dcache_warning = 1;
            }
          break;

        default:
          SignalException(ReservedInstruction,instruction);
          break;
      }
      break;

    default: /* unrecognised cache ID */
      SignalException(ReservedInstruction,instruction);
      break;
  }

  return;
}

/*-- FPU support routines ---------------------------------------------------*/

/* Numbers are held in normalized form. The SINGLE and DOUBLE binary
   formats conform to ANSI/IEEE Std 754-1985. */
/* SINGLE precision floating:
 *    seeeeeeeefffffffffffffffffffffff
 *      s =  1bit  = sign
 *      e =  8bits = exponent
 *      f = 23bits = fraction
 */
/* SINGLE precision fixed:
 *    siiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
 *      s =  1bit  = sign
 *      i = 31bits = integer
 */
/* DOUBLE precision floating:
 *    seeeeeeeeeeeffffffffffffffffffffffffffffffffffffffffffffffffffff
 *      s =  1bit  = sign
 *      e = 11bits = exponent
 *      f = 52bits = fraction
 */
/* DOUBLE precision fixed:
 *    siiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
 *      s =  1bit  = sign
 *      i = 63bits = integer
 */

/* Extract sign-bit: */
#define FP_S_s(v)    (((v) & ((unsigned)1 << 31)) ? 1 : 0)
#define FP_D_s(v)    (((v) & ((uword64)1 << 63)) ? 1 : 0)
/* Extract biased exponent: */
#define FP_S_be(v)   (((v) >> 23) & 0xFF)
#define FP_D_be(v)   (((v) >> 52) & 0x7FF)
/* Extract unbiased Exponent: */
#define FP_S_e(v)    (FP_S_be(v) - 0x7F)
#define FP_D_e(v)    (FP_D_be(v) - 0x3FF)
/* Extract complete fraction field: */
#define FP_S_f(v)    ((v) & ~((unsigned)0x1FF << 23))
#define FP_D_f(v)    ((v) & ~((uword64)0xFFF << 52))
/* Extract numbered fraction bit: */
#define FP_S_fb(b,v) (((v) & (1 << (23 - (b)))) ? 1 : 0)
#define FP_D_fb(b,v) (((v) & (1 << (52 - (b)))) ? 1 : 0)

/* Explicit QNaN values used when value required: */
#define FPQNaN_SINGLE   (0x7FBFFFFF)
#define FPQNaN_WORD     (0x7FFFFFFF)
#define FPQNaN_DOUBLE   (((uword64)0x7FF7FFFF << 32) | 0xFFFFFFFF)
#define FPQNaN_LONG     (((uword64)0x7FFFFFFF << 32) | 0xFFFFFFFF)

/* Explicit Infinity values used when required: */
#define FPINF_SINGLE    (0x7F800000)
#define FPINF_DOUBLE    (((uword64)0x7FF00000 << 32) | 0x00000000)

#if 1 /* def DEBUG */
#define RMMODE(v) (((v) == FP_RM_NEAREST) ? "Round" : (((v) == FP_RM_TOZERO) ? "Trunc" : (((v) == FP_RM_TOPINF) ? "Ceil" : "Floor")))
#define DOFMT(v)  (((v) == fmt_single) ? "single" : (((v) == fmt_double) ? "double" : (((v) == fmt_word) ? "word" : (((v) == fmt_long) ? "long" : (((v) == fmt_unknown) ? "<unknown>" : (((v) == fmt_uninterpreted) ? "<uninterpreted>" : "<format error>"))))))
#endif /* DEBUG */

uword64
value_fpr (SIM_DESC sd,
	   sim_cpu *cpu,
	   address_word cia,
	   int fpr,
	   FP_formats fmt)
{
  uword64 value = 0;
  int err = 0;

  /* Treat unused register values, as fixed-point 64bit values: */
  if ((fmt == fmt_uninterpreted) || (fmt == fmt_unknown))
#if 1
   /* If request to read data as "uninterpreted", then use the current
      encoding: */
   fmt = FPR_STATE[fpr];
#else
   fmt = fmt_long;
#endif

  /* For values not yet accessed, set to the desired format: */
  if (FPR_STATE[fpr] == fmt_uninterpreted) {
    FPR_STATE[fpr] = fmt;
#ifdef DEBUG
    printf("DBG: Register %d was fmt_uninterpreted. Now %s\n",fpr,DOFMT(fmt));
#endif /* DEBUG */
  }
  if (fmt != FPR_STATE[fpr]) {
    sim_io_eprintf(sd,"FPR %d (format %s) being accessed with format %s - setting to unknown (PC = 0x%s)\n",fpr,DOFMT(FPR_STATE[fpr]),DOFMT(fmt),pr_addr(cia));
    FPR_STATE[fpr] = fmt_unknown;
  }

  if (FPR_STATE[fpr] == fmt_unknown) {
   /* Set QNaN value: */
   switch (fmt) {
    case fmt_single:
     value = FPQNaN_SINGLE;
     break;

    case fmt_double:
     value = FPQNaN_DOUBLE;
     break;

    case fmt_word:
     value = FPQNaN_WORD;
     break;

    case fmt_long:
     value = FPQNaN_LONG;
     break;

    default:
     err = -1;
     break;
   }
  } else if (SizeFGR() == 64) {
    switch (fmt) {
     case fmt_single:
     case fmt_word:
      value = (FGR[fpr] & 0xFFFFFFFF);
      break;

     case fmt_uninterpreted:
     case fmt_double:
     case fmt_long:
      value = FGR[fpr];
      break;

     default :
      err = -1;
      break;
    }
  } else {
    switch (fmt) {
     case fmt_single:
     case fmt_word:
      value = (FGR[fpr] & 0xFFFFFFFF);
      break;

     case fmt_uninterpreted:
     case fmt_double:
     case fmt_long:
      if ((fpr & 1) == 0) { /* even registers only */
	value = ((((uword64)FGR[fpr+1]) << 32) | (FGR[fpr] & 0xFFFFFFFF));
      } else {
	SignalException(ReservedInstruction,0);
      }
      break;

     default :
      err = -1;
      break;
    }
  }

  if (err)
   SignalExceptionSimulatorFault ("Unrecognised FP format in ValueFPR()");

#ifdef DEBUG
  printf("DBG: ValueFPR: fpr = %d, fmt = %s, value = 0x%s : PC = 0x%s : SizeFGR() = %d\n",fpr,DOFMT(fmt),pr_addr(value),pr_addr(cia),SizeFGR());
#endif /* DEBUG */

  return(value);
}

void
store_fpr (SIM_DESC sd,
	   sim_cpu *cpu,
	   address_word cia,
	   int fpr,
	   FP_formats fmt,
	   uword64 value)
{
  int err = 0;

#ifdef DEBUG
  printf("DBG: StoreFPR: fpr = %d, fmt = %s, value = 0x%s : PC = 0x%s : SizeFGR() = %d\n",fpr,DOFMT(fmt),pr_addr(value),pr_addr(cia),SizeFGR());
#endif /* DEBUG */

  if (SizeFGR() == 64) {
    switch (fmt) {
      case fmt_uninterpreted_32:
	fmt = fmt_uninterpreted;
      case fmt_single :
      case fmt_word :
       FGR[fpr] = (((uword64)0xDEADC0DE << 32) | (value & 0xFFFFFFFF));
       FPR_STATE[fpr] = fmt;
       break;

      case fmt_uninterpreted_64:
	fmt = fmt_uninterpreted;
      case fmt_uninterpreted:
      case fmt_double :
      case fmt_long :
       FGR[fpr] = value;
       FPR_STATE[fpr] = fmt;
       break;

      default :
       FPR_STATE[fpr] = fmt_unknown;
       err = -1;
       break;
    }
  } else {
    switch (fmt) {
      case fmt_uninterpreted_32:
	fmt = fmt_uninterpreted;
      case fmt_single :
      case fmt_word :
       FGR[fpr] = (value & 0xFFFFFFFF);
       FPR_STATE[fpr] = fmt;
       break;

      case fmt_uninterpreted_64:
	fmt = fmt_uninterpreted;
      case fmt_uninterpreted:
      case fmt_double :
      case fmt_long :
	if ((fpr & 1) == 0) { /* even register number only */
	  FGR[fpr+1] = (value >> 32);
	  FGR[fpr] = (value & 0xFFFFFFFF);
	  FPR_STATE[fpr + 1] = fmt;
	  FPR_STATE[fpr] = fmt;
	} else {
	  FPR_STATE[fpr] = fmt_unknown;
	  FPR_STATE[fpr + 1] = fmt_unknown;
	  SignalException(ReservedInstruction,0);
	}
       break;

      default :
       FPR_STATE[fpr] = fmt_unknown;
       err = -1;
       break;
    }
  }
#if defined(WARN_RESULT)
  else
    UndefinedResult();
#endif /* WARN_RESULT */

  if (err)
   SignalExceptionSimulatorFault ("Unrecognised FP format in StoreFPR()");

#ifdef DEBUG
  printf("DBG: StoreFPR: fpr[%d] = 0x%s (format %s)\n",fpr,pr_addr(FGR[fpr]),DOFMT(fmt));
#endif /* DEBUG */

  return;
}

int
NaN(op,fmt)
     uword64 op;
     FP_formats fmt; 
{
  int boolean = 0;
  switch (fmt) {
   case fmt_single:
   case fmt_word:
    {
      sim_fpu wop;
      sim_fpu_32to (&wop, op);
      boolean = sim_fpu_is_nan (&wop);
      break;
    }
   case fmt_double:
   case fmt_long:
    {
      sim_fpu wop;
      sim_fpu_64to (&wop, op);
      boolean = sim_fpu_is_nan (&wop);
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
printf("DBG: NaN: returning %d for 0x%s (format = %s)\n",boolean,pr_addr(op),DOFMT(fmt));
#endif /* DEBUG */

  return(boolean);
}

int
Infinity(op,fmt)
     uword64 op;
     FP_formats fmt; 
{
  int boolean = 0;

#ifdef DEBUG
  printf("DBG: Infinity: format %s 0x%s\n",DOFMT(fmt),pr_addr(op));
#endif /* DEBUG */

  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop;
      sim_fpu_32to (&wop, op);
      boolean = sim_fpu_is_infinity (&wop);
      break;
    }
   case fmt_double:
    {
      sim_fpu wop;
      sim_fpu_64to (&wop, op);
      boolean = sim_fpu_is_infinity (&wop);
      break;
    }
   default:
    printf("DBG: TODO: unrecognised format (%s) for Infinity check\n",DOFMT(fmt));
    break;
  }

#ifdef DEBUG
  printf("DBG: Infinity: returning %d for 0x%s (format = %s)\n",boolean,pr_addr(op),DOFMT(fmt));
#endif /* DEBUG */

  return(boolean);
}

int
Less(op1,op2,fmt)
     uword64 op1;
     uword64 op2;
     FP_formats fmt; 
{
  int boolean = 0;

  /* Argument checking already performed by the FPCOMPARE code */

#ifdef DEBUG
  printf("DBG: Less: %s: op1 = 0x%s : op2 = 0x%s\n",DOFMT(fmt),pr_addr(op1),pr_addr(op2));
#endif /* DEBUG */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu_32to (&wop1, op1);
      sim_fpu_32to (&wop2, op2);
      boolean = sim_fpu_is_lt (&wop1, &wop2);
      break;
    }
   case fmt_double:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu_64to (&wop1, op1);
      sim_fpu_64to (&wop2, op2);
      boolean = sim_fpu_is_lt (&wop1, &wop2);
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Less: returning %d (format = %s)\n",boolean,DOFMT(fmt));
#endif /* DEBUG */

  return(boolean);
}

int
Equal(op1,op2,fmt)
     uword64 op1;
     uword64 op2;
     FP_formats fmt; 
{
  int boolean = 0;

  /* Argument checking already performed by the FPCOMPARE code */

#ifdef DEBUG
  printf("DBG: Equal: %s: op1 = 0x%s : op2 = 0x%s\n",DOFMT(fmt),pr_addr(op1),pr_addr(op2));
#endif /* DEBUG */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu_32to (&wop1, op1);
      sim_fpu_32to (&wop2, op2);
      boolean = sim_fpu_is_eq (&wop1, &wop2);
      break;
    }
   case fmt_double:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu_64to (&wop1, op1);
      sim_fpu_64to (&wop2, op2);
      boolean = sim_fpu_is_eq (&wop1, &wop2);
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Equal: returning %d (format = %s)\n",boolean,DOFMT(fmt));
#endif /* DEBUG */

  return(boolean);
}

uword64
AbsoluteValue(op,fmt)
     uword64 op;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: AbsoluteValue: %s: op = 0x%s\n",DOFMT(fmt),pr_addr(op));
#endif /* DEBUG */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop;
      unsigned32 ans;
      sim_fpu_32to (&wop, op);
      sim_fpu_abs (&wop, &wop);
      sim_fpu_to32 (&ans, &wop);
      result = ans;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop;
      unsigned64 ans;
      sim_fpu_64to (&wop, op);
      sim_fpu_abs (&wop, &wop);
      sim_fpu_to64 (&ans, &wop);
      result = ans;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

  return(result);
}

uword64
Negate(op,fmt)
     uword64 op;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: Negate: %s: op = 0x%s\n",DOFMT(fmt),pr_addr(op));
#endif /* DEBUG */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop;
      unsigned32 ans;
      sim_fpu_32to (&wop, op);
      sim_fpu_neg (&wop, &wop);
      sim_fpu_to32 (&ans, &wop);
      result = ans;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop;
      unsigned64 ans;
      sim_fpu_64to (&wop, op);
      sim_fpu_neg (&wop, &wop);
      sim_fpu_to64 (&ans, &wop);
      result = ans;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

  return(result);
}

uword64
Add(op1,op2,fmt)
     uword64 op1;
     uword64 op2;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: Add: %s: op1 = 0x%s : op2 = 0x%s\n",DOFMT(fmt),pr_addr(op1),pr_addr(op2));
#endif /* DEBUG */

  /* The registers must specify FPRs valid for operands of type
     "fmt". If they are not valid, the result is undefined. */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned32 res;
      sim_fpu_32to (&wop1, op1);
      sim_fpu_32to (&wop2, op2);
      sim_fpu_add (&ans, &wop1, &wop2);
      sim_fpu_to32 (&res, &ans);
      result = res;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned64 res;
      sim_fpu_64to (&wop1, op1);
      sim_fpu_64to (&wop2, op2);
      sim_fpu_add (&ans, &wop1, &wop2);
      sim_fpu_to64 (&res, &ans);
      result = res;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Add: returning 0x%s (format = %s)\n",pr_addr(result),DOFMT(fmt));
#endif /* DEBUG */

  return(result);
}

uword64
Sub(op1,op2,fmt)
     uword64 op1;
     uword64 op2;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: Sub: %s: op1 = 0x%s : op2 = 0x%s\n",DOFMT(fmt),pr_addr(op1),pr_addr(op2));
#endif /* DEBUG */

  /* The registers must specify FPRs valid for operands of type
     "fmt". If they are not valid, the result is undefined. */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned32 res;
      sim_fpu_32to (&wop1, op1);
      sim_fpu_32to (&wop2, op2);
      sim_fpu_sub (&ans, &wop1, &wop2);
      sim_fpu_to32 (&res, &ans);
      result = res;
    }
    break;
   case fmt_double:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned64 res;
      sim_fpu_64to (&wop1, op1);
      sim_fpu_64to (&wop2, op2);
      sim_fpu_sub (&ans, &wop1, &wop2);
      sim_fpu_to64 (&res, &ans);
      result = res;
    }
    break;
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Sub: returning 0x%s (format = %s)\n",pr_addr(result),DOFMT(fmt));
#endif /* DEBUG */

  return(result);
}

uword64
Multiply(op1,op2,fmt)
     uword64 op1;
     uword64 op2;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: Multiply: %s: op1 = 0x%s : op2 = 0x%s\n",DOFMT(fmt),pr_addr(op1),pr_addr(op2));
#endif /* DEBUG */

  /* The registers must specify FPRs valid for operands of type
     "fmt". If they are not valid, the result is undefined. */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned32 res;
      sim_fpu_32to (&wop1, op1);
      sim_fpu_32to (&wop2, op2);
      sim_fpu_mul (&ans, &wop1, &wop2);
      sim_fpu_to32 (&res, &ans);
      result = res;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned64 res;
      sim_fpu_64to (&wop1, op1);
      sim_fpu_64to (&wop2, op2);
      sim_fpu_mul (&ans, &wop1, &wop2);
      sim_fpu_to64 (&res, &ans);
      result = res;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Multiply: returning 0x%s (format = %s)\n",pr_addr(result),DOFMT(fmt));
#endif /* DEBUG */

  return(result);
}

uword64
Divide(op1,op2,fmt)
     uword64 op1;
     uword64 op2;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: Divide: %s: op1 = 0x%s : op2 = 0x%s\n",DOFMT(fmt),pr_addr(op1),pr_addr(op2));
#endif /* DEBUG */

  /* The registers must specify FPRs valid for operands of type
     "fmt". If they are not valid, the result is undefined. */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned32 res;
      sim_fpu_32to (&wop1, op1);
      sim_fpu_32to (&wop2, op2);
      sim_fpu_div (&ans, &wop1, &wop2);
      sim_fpu_to32 (&res, &ans);
      result = res;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop1;
      sim_fpu wop2;
      sim_fpu ans;
      unsigned64 res;
      sim_fpu_64to (&wop1, op1);
      sim_fpu_64to (&wop2, op2);
      sim_fpu_div (&ans, &wop1, &wop2);
      sim_fpu_to64 (&res, &ans);
      result = res;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Divide: returning 0x%s (format = %s)\n",pr_addr(result),DOFMT(fmt));
#endif /* DEBUG */

  return(result);
}

uword64 UNUSED
Recip(op,fmt)
     uword64 op;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: Recip: %s: op = 0x%s\n",DOFMT(fmt),pr_addr(op));
#endif /* DEBUG */

  /* The registers must specify FPRs valid for operands of type
     "fmt". If they are not valid, the result is undefined. */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop;
      sim_fpu ans;
      unsigned32 res;
      sim_fpu_32to (&wop, op);
      sim_fpu_inv (&ans, &wop);
      sim_fpu_to32 (&res, &ans);
      result = res;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop;
      sim_fpu ans;
      unsigned64 res;
      sim_fpu_64to (&wop, op);
      sim_fpu_inv (&ans, &wop);
      sim_fpu_to64 (&res, &ans);
      result = res;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: Recip: returning 0x%s (format = %s)\n",pr_addr(result),DOFMT(fmt));
#endif /* DEBUG */

  return(result);
}

uword64
SquareRoot(op,fmt)
     uword64 op;
     FP_formats fmt; 
{
  uword64 result = 0;

#ifdef DEBUG
  printf("DBG: SquareRoot: %s: op = 0x%s\n",DOFMT(fmt),pr_addr(op));
#endif /* DEBUG */

  /* The registers must specify FPRs valid for operands of type
     "fmt". If they are not valid, the result is undefined. */

  /* The format type should already have been checked: */
  switch (fmt) {
   case fmt_single:
    {
      sim_fpu wop;
      sim_fpu ans;
      unsigned32 res;
      sim_fpu_32to (&wop, op);
      sim_fpu_sqrt (&ans, &wop);
      sim_fpu_to32 (&res, &ans);
      result = res;
      break;
    }
   case fmt_double:
    {
      sim_fpu wop;
      sim_fpu ans;
      unsigned64 res;
      sim_fpu_64to (&wop, op);
      sim_fpu_sqrt (&ans, &wop);
      sim_fpu_to64 (&res, &ans);
      result = res;
      break;
    }
   default:
    fprintf (stderr, "Bad switch\n");
    abort ();
  }

#ifdef DEBUG
  printf("DBG: SquareRoot: returning 0x%s (format = %s)\n",pr_addr(result),DOFMT(fmt));
#endif /* DEBUG */

  return(result);
}

uword64
convert (SIM_DESC sd,
	 sim_cpu *cpu,
	 address_word cia,
	 int rm,
	 uword64 op,
	 FP_formats from,
	 FP_formats to)
{
  sim_fpu wop;
  sim_fpu_round round;
  unsigned32 result32;
  unsigned64 result64;

#ifdef DEBUG
  printf("DBG: Convert: mode %s : op 0x%s : from %s : to %s : (PC = 0x%s)\n",RMMODE(rm),pr_addr(op),DOFMT(from),DOFMT(to),pr_addr(IPC));
#endif /* DEBUG */

  switch (rm)
    {
    case FP_RM_NEAREST:
      /* Round result to nearest representable value. When two
	 representable values are equally near, round to the value
	 that has a least significant bit of zero (i.e. is even). */
      round = sim_fpu_round_near;
      break;
    case FP_RM_TOZERO:
      /* Round result to the value closest to, and not greater in
	 magnitude than, the result. */
      round = sim_fpu_round_zero;
      break;
    case FP_RM_TOPINF:
      /* Round result to the value closest to, and not less than,
	 the result. */
      round = sim_fpu_round_up;
      break;
      
    case FP_RM_TOMINF:
      /* Round result to the value closest to, and not greater than,
	 the result. */
      round = sim_fpu_round_down;
      break;
    default:
      round = 0;
      fprintf (stderr, "Bad switch\n");
      abort ();
    }
  
  /* Convert the input to sim_fpu internal format */
  switch (from)
    {
    case fmt_double:
      sim_fpu_64to (&wop, op);
      break;
    case fmt_single:
      sim_fpu_32to (&wop, op);
      break;
    case fmt_word:
      sim_fpu_i32to (&wop, op, round);
      break;
    case fmt_long:
      sim_fpu_i64to (&wop, op, round);
      break;
    default:
      fprintf (stderr, "Bad switch\n");
      abort ();
    }

  /* Convert sim_fpu format into the output */
  /* The value WOP is converted to the destination format, rounding
     using mode RM. When the destination is a fixed-point format, then
     a source value of Infinity, NaN or one which would round to an
     integer outside the fixed point range then an IEEE Invalid
     Operation condition is raised. */
  switch (to)
    {
    case fmt_single:
      sim_fpu_round_32 (&wop, round, 0);
      sim_fpu_to32 (&result32, &wop);
      result64 = result32;
      break;
    case fmt_double:
      sim_fpu_round_64 (&wop, round, 0);
      sim_fpu_to64 (&result64, &wop);
      break;
    case fmt_word:
      sim_fpu_to32i (&result32, &wop, round);
      result64 = result32;
      break;
    case fmt_long:
      sim_fpu_to64i (&result64, &wop, round);
      break;
    default:
      result64 = 0;
      fprintf (stderr, "Bad switch\n");
      abort ();
    }
 
#ifdef DEBUG
  printf("DBG: Convert: returning 0x%s (to format = %s)\n",pr_addr(result64),DOFMT(to));
#endif /* DEBUG */

  return(result64);
}


/*-- co-processor support routines ------------------------------------------*/

static int UNUSED
CoProcPresent(coproc_number)
     unsigned int coproc_number;
{
  /* Return TRUE if simulator provides a model for the given co-processor number */
  return(0);
}

void
cop_lw (SIM_DESC sd,
	sim_cpu *cpu,
	address_word cia,
	int coproc_num,
	int coproc_reg,
	unsigned int memword)
{
  switch (coproc_num)
    {
    case 1:
      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT)
	{
#ifdef DEBUG
	  printf("DBG: COP_LW: memword = 0x%08X (uword64)memword = 0x%s\n",memword,pr_addr(memword));
#endif
	  StoreFPR(coproc_reg,fmt_word,(uword64)memword);
	  FPR_STATE[coproc_reg] = fmt_uninterpreted;
	  break;
	}

    default:
#if 0 /* this should be controlled by a configuration option */
      sim_io_printf(sd,"COP_LW(%d,%d,0x%08X) at PC = 0x%s : TODO (architecture specific)\n",coproc_num,coproc_reg,memword,pr_addr(cia));
#endif
      break;
    }

  return;
}

void
cop_ld (SIM_DESC sd,
	sim_cpu *cpu,
	address_word cia,
	int coproc_num,
	int coproc_reg,
	uword64 memword)
{
  switch (coproc_num) {
    case 1:
      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT)
	{
	  StoreFPR(coproc_reg,fmt_uninterpreted,memword);
	  break;
	}

    default:
#if 0 /* this message should be controlled by a configuration option */
     sim_io_printf(sd,"COP_LD(%d,%d,0x%s) at PC = 0x%s : TODO (architecture specific)\n",coproc_num,coproc_reg,pr_addr(memword),pr_addr(cia));
#endif
     break;
  }

  return;
}

unsigned int
cop_sw (SIM_DESC sd,
	sim_cpu *cpu,
	address_word cia,
	int coproc_num,
	int coproc_reg)
{
  unsigned int value = 0;

  switch (coproc_num)
    {
    case 1:
      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT)
	{
	  FP_formats hold;
	  hold = FPR_STATE[coproc_reg];
	  FPR_STATE[coproc_reg] = fmt_word;
	  value = (unsigned int)ValueFPR(coproc_reg,fmt_uninterpreted);
	  FPR_STATE[coproc_reg] = hold;
	  break;
	}

    default:
#if 0 /* should be controlled by configuration option */
      sim_io_printf(sd,"COP_SW(%d,%d) at PC = 0x%s : TODO (architecture specific)\n",coproc_num,coproc_reg,pr_addr(cia));
#endif
      break;
    }

  return(value);
}

uword64
cop_sd (SIM_DESC sd,
	sim_cpu *cpu,
	address_word cia,
	int coproc_num,
	int coproc_reg)
{
  uword64 value = 0;
  switch (coproc_num)
    {
    case 1:
      if (CURRENT_FLOATING_POINT == HARD_FLOATING_POINT)
	{
	  value = ValueFPR(coproc_reg,fmt_uninterpreted);
	  break;
	}

    default:
#if 0 /* should be controlled by configuration option */
      sim_io_printf(sd,"COP_SD(%d,%d) at PC = 0x%s : TODO (architecture specific)\n",coproc_num,coproc_reg,pr_addr(cia));
#endif
      break;
    }

  return(value);
}

void
decode_coproc (SIM_DESC sd,
	       sim_cpu *cpu,
	       address_word cia,
	       unsigned int instruction)
{
  int coprocnum = ((instruction >> 26) & 3);

  switch (coprocnum)
    {
    case 0: /* standard CPU control and cache registers */
      {
        int code = ((instruction >> 21) & 0x1F);
        /* R4000 Users Manual (second edition) lists the following CP0
           instructions:
	   DMFC0   Doubleword Move From CP0        (VR4100 = 01000000001tttttddddd00000000000)
	   DMTC0   Doubleword Move To CP0          (VR4100 = 01000000101tttttddddd00000000000)
	   MFC0    word Move From CP0              (VR4100 = 01000000000tttttddddd00000000000)
	   MTC0    word Move To CP0                (VR4100 = 01000000100tttttddddd00000000000)
	   TLBR    Read Indexed TLB Entry          (VR4100 = 01000010000000000000000000000001)
	   TLBWI   Write Indexed TLB Entry         (VR4100 = 01000010000000000000000000000010)
	   TLBWR   Write Random TLB Entry          (VR4100 = 01000010000000000000000000000110)
	   TLBP    Probe TLB for Matching Entry    (VR4100 = 01000010000000000000000000001000)
	   CACHE   Cache operation                 (VR4100 = 101111bbbbbpppppiiiiiiiiiiiiiiii)
	   ERET    Exception return                (VR4100 = 01000010000000000000000000011000)
	   */
        if (((code == 0x00) || (code == 0x04)) && ((instruction & 0x7FF) == 0))
	  {
	    int rt = ((instruction >> 16) & 0x1F);
	    int rd = ((instruction >> 11) & 0x1F);
	    
	    switch (rd)  /* NOTEs: Standard CP0 registers */
	      {
		/* 0 = Index               R4000   VR4100  VR4300 */
		/* 1 = Random              R4000   VR4100  VR4300 */
		/* 2 = EntryLo0            R4000   VR4100  VR4300 */
		/* 3 = EntryLo1            R4000   VR4100  VR4300 */
		/* 4 = Context             R4000   VR4100  VR4300 */
		/* 5 = PageMask            R4000   VR4100  VR4300 */
		/* 6 = Wired               R4000   VR4100  VR4300 */
		/* 8 = BadVAddr            R4000   VR4100  VR4300 */
		/* 9 = Count               R4000   VR4100  VR4300 */
		/* 10 = EntryHi            R4000   VR4100  VR4300 */
		/* 11 = Compare            R4000   VR4100  VR4300 */
		/* 12 = SR                 R4000   VR4100  VR4300 */
	      case 12:
		if (code == 0x00)
		  GPR[rt] = SR;
		else
		  SR = GPR[rt];
		break;
		/* 13 = Cause              R4000   VR4100  VR4300 */
	      case 13:
		if (code == 0x00)
		  GPR[rt] = CAUSE;
		else
		  CAUSE = GPR[rt];
		break;
		/* 14 = EPC                R4000   VR4100  VR4300 */
		/* 15 = PRId               R4000   VR4100  VR4300 */
#ifdef SUBTARGET_R3900
                /* 16 = Debug */
              case 16:
                if (code == 0x00)
                  GPR[rt] = Debug;
                else
                  Debug = GPR[rt];
                break;
#else
		/* 16 = Config             R4000   VR4100  VR4300 */
              case 16:
                if (code == 0x00)
                  GPR[rt] = C0_CONFIG;
                else
                  C0_CONFIG = GPR[rt];
                break;
#endif
#ifdef SUBTARGET_R3900
                /* 17 = Debug */
              case 17:
                if (code == 0x00)
                  GPR[rt] = DEPC;
                else
                  DEPC = GPR[rt];
                break;
#else
		/* 17 = LLAddr             R4000   VR4100  VR4300 */
#endif
		/* 18 = WatchLo            R4000   VR4100  VR4300 */
		/* 19 = WatchHi            R4000   VR4100  VR4300 */
		/* 20 = XContext           R4000   VR4100  VR4300 */
		/* 26 = PErr or ECC        R4000   VR4100  VR4300 */
		/* 27 = CacheErr           R4000   VR4100 */
		/* 28 = TagLo              R4000   VR4100  VR4300 */
		/* 29 = TagHi              R4000   VR4100  VR4300 */
		/* 30 = ErrorEPC           R4000   VR4100  VR4300 */
		GPR[rt] = 0xDEADC0DE; /* CPR[0,rd] */
		/* CPR[0,rd] = GPR[rt]; */
	      default:
		if (code == 0x00)
		  sim_io_printf(sd,"Warning: MFC0 %d,%d ignored (architecture specific)\n",rt,rd);
		else
		  sim_io_printf(sd,"Warning: MTC0 %d,%d ignored (architecture specific)\n",rt,rd);
	      }
	  }
	else if (code == 0x10 && (instruction & 0x3f) == 0x18)
	  {
	    /* ERET */
	    if (SR & status_ERL)
	      {
		/* Oops, not yet available */
		sim_io_printf(sd,"Warning: ERET when SR[ERL] set not handled yet");
		PC = EPC;
		SR &= ~status_ERL;
	      }
	    else
	      {
		PC = EPC;
		SR &= ~status_EXL;
	      }
	  }
        else if (code == 0x10 && (instruction & 0x3f) == 0x10)
          {
            /* RFE */
          }
        else if (code == 0x10 && (instruction & 0x3f) == 0x1F)
          {
            /* DERET */
            Debug &= ~Debug_DM;
            DELAYSLOT();
            DSPC = DEPC;
          }
	else
	  sim_io_eprintf(sd,"Unrecognised COP0 instruction 0x%08X at PC = 0x%s : No handler present\n",instruction,pr_addr(cia));
        /* TODO: When executing an ERET or RFE instruction we should
           clear LLBIT, to ensure that any out-standing atomic
           read/modify/write sequence fails. */
      }
    break;
    
    case 2: /* undefined co-processor */
      sim_io_eprintf(sd,"COP2 instruction 0x%08X at PC = 0x%s : No handler present\n",instruction,pr_addr(cia));
      break;
      
    case 1: /* should not occur (FPU co-processor) */
    case 3: /* should not occur (FPU co-processor) */
      SignalException(ReservedInstruction,instruction);
      break;
    }
  
  return;
}

/*-- instruction simulation -------------------------------------------------*/

/* When the IGEN simulator is being built, the function below is be
   replaced by a generated version.  However, WITH_IGEN == 2 indicates
   that the fubction below should be compiled but under a different
   name (to allow backward compatibility) */

#if (WITH_IGEN != 1)
#if (WITH_IGEN > 1)
void old_engine_run PARAMS ((SIM_DESC sd, int next_cpu_nr, int siggnal));
void
old_engine_run (sd, next_cpu_nr, nr_cpus, siggnal)
#else
void
sim_engine_run (sd, next_cpu_nr, nr_cpus, siggnal)
#endif
     SIM_DESC sd;
     int next_cpu_nr; /* ignore */
     int nr_cpus; /* ignore */
     int siggnal; /* ignore */
{
  sim_cpu *cpu = STATE_CPU (sd, 0); /* hardwire to cpu 0 */
#if !defined(FASTSIM)
  unsigned int pipeline_count = 1;
#endif

#ifdef DEBUG
  if (STATE_MEMORY (sd) == NULL) {
    printf("DBG: simulate() entered with no memory\n");
    exit(1);
  }
#endif /* DEBUG */

#if 0 /* Disabled to check that everything works OK */
  /* The VR4300 seems to sign-extend the PC on its first
     access. However, this may just be because it is currently
     configured in 32bit mode. However... */
  PC = SIGNEXTEND(PC,32);
#endif

  /* main controlling loop */
  while (1) {
    /* vaddr is slowly being replaced with cia - current instruction
       address */
    address_word cia = (uword64)PC;
    address_word vaddr = cia;
    address_word paddr;
    int cca;
    unsigned int instruction;	/* uword64? what's this used for?  FIXME! */

#ifdef DEBUG
    {
      printf("DBG: state = 0x%08X :",state);
      if (state & simHALTEX) printf(" simHALTEX");
      if (state & simHALTIN) printf(" simHALTIN");
      printf("\n");
    }
#endif /* DEBUG */

    DSSTATE = (STATE & simDELAYSLOT);
#ifdef DEBUG
    if (dsstate)
     sim_io_printf(sd,"DBG: DSPC = 0x%s\n",pr_addr(DSPC));
#endif /* DEBUG */

    /* Fetch the next instruction from the simulator memory: */
    if (AddressTranslation(cia,isINSTRUCTION,isLOAD,&paddr,&cca,isTARGET,isREAL)) {
      if ((vaddr & 1) == 0) {
	/* Copy the action of the LW instruction */
	unsigned int reverse = (ReverseEndian ? (LOADDRMASK >> 2) : 0);
	unsigned int bigend = (BigEndianCPU ? (LOADDRMASK >> 2) : 0);
	uword64 value;
	unsigned int byte;
	paddr = ((paddr & ~LOADDRMASK) | ((paddr & LOADDRMASK) ^ (reverse << 2)));
	LoadMemory(&value,NULL,cca,AccessLength_WORD,paddr,vaddr,isINSTRUCTION,isREAL);
	byte = ((vaddr & LOADDRMASK) ^ (bigend << 2));
	instruction = ((value >> (8 * byte)) & 0xFFFFFFFF);
      } else {
	/* Copy the action of the LH instruction */
	unsigned int reverse = (ReverseEndian ? (LOADDRMASK >> 1) : 0);
	unsigned int bigend = (BigEndianCPU ? (LOADDRMASK >> 1) : 0);
	uword64 value;
	unsigned int byte;
	paddr = (((paddr & ~ (uword64) 1) & ~LOADDRMASK)
		 | (((paddr & ~ (uword64) 1) & LOADDRMASK) ^ (reverse << 1)));
	LoadMemory(&value,NULL,cca, AccessLength_HALFWORD,
			   paddr & ~ (uword64) 1,
			   vaddr, isINSTRUCTION, isREAL);
	byte = (((vaddr &~ (uword64) 1) & LOADDRMASK) ^ (bigend << 1));
	instruction = ((value >> (8 * byte)) & 0xFFFF);
      }
    } else {
      fprintf(stderr,"Cannot translate address for PC = 0x%s failed\n",pr_addr(PC));
      exit(1);
    }

#ifdef DEBUG
    sim_io_printf(sd,"DBG: fetched 0x%08X from PC = 0x%s\n",instruction,pr_addr(PC));
#endif /* DEBUG */

    /* This is required by exception processing, to ensure that we can
       cope with exceptions in the delay slots of branches that may
       already have changed the PC. */
    if ((vaddr & 1) == 0)
      PC += 4; /* increment ready for the next fetch */
    else
      PC += 2;
    /* NOTE: If we perform a delay slot change to the PC, this
       increment is not requuired. However, it would make the
       simulator more complicated to try and avoid this small hit. */

    /* Currently this code provides a simple model. For more
       complicated models we could perform exception status checks at
       this point, and set the simSTOP state as required. This could
       also include processing any hardware interrupts raised by any
       I/O model attached to the simulator context.

       Support for "asynchronous" I/O events within the simulated world
       could be providing by managing a counter, and calling a I/O
       specific handler when a particular threshold is reached. On most
       architectures a decrement and check for zero operation is
       usually quicker than an increment and compare. However, the
       process of managing a known value decrement to zero, is higher
       than the cost of using an explicit value UINT_MAX into the
       future. Which system is used will depend on how complicated the
       I/O model is, and how much it is likely to affect the simulator
       bandwidth.

       If events need to be scheduled further in the future than
       UINT_MAX event ticks, then the I/O model should just provide its
       own counter, triggered from the event system. */

    /* MIPS pipeline ticks. To allow for future support where the
       pipeline hit of individual instructions is known, this control
       loop manages a "pipeline_count" variable. It is initialised to
       1 (one), and will only be changed by the simulator engine when
       executing an instruction. If the engine does not have access to
       pipeline cycle count information then all instructions will be
       treated as using a single cycle. NOTE: A standard system is not
       provided by the default simulator because different MIPS
       architectures have different cycle counts for the same
       instructions.

       [NOTE: pipeline_count has been replaced the event queue] */

    /* shuffle the floating point status pipeline state */
    ENGINE_ISSUE_PREFIX_HOOK();

/* NOTE: For multi-context simulation environments the "instruction"
   variable should be local to this routine. */

/* Shorthand accesses for engine. Note: If we wanted to use global
   variables (and a single-threaded simulator engine), then we can
   create the actual variables with these names. */

    if (!(STATE & simSKIPNEXT)) {
      /* Include the simulator engine */
#include "oengine.c"
#if ((GPRLEN == 64) && !PROCESSOR_64BIT) || ((GPRLEN == 32) && PROCESSOR_64BIT)
#error "Mismatch between run-time simulator code and simulation engine"
#endif
#if (WITH_TARGET_WORD_BITSIZE != GPRLEN)
#error "Mismatch between configure WITH_TARGET_WORD_BITSIZE and gencode GPRLEN"
#endif
#if ((WITH_FLOATING_POINT == HARD_FLOATING_POINT) != defined (HASFPU))
#error "Mismatch between configure WITH_FLOATING_POINT and gencode HASFPU"
#endif

#if defined(WARN_LOHI)
      /* Decrement the HI/LO validity ticks */
      if (HIACCESS > 0)
       HIACCESS--;
      if (LOACCESS > 0)
       LOACCESS--;
#endif /* WARN_LOHI */

      /* For certain MIPS architectures, GPR[0] is hardwired to zero. We
         should check for it being changed. It is better doing it here,
         than within the simulator, since it will help keep the simulator
         small. */
      if (ZERO != 0) {
#if defined(WARN_ZERO)
        sim_io_eprintf(sd,"The ZERO register has been updated with 0x%s (PC = 0x%s) (reset back to zero)\n",pr_addr(ZERO),pr_addr(cia));
#endif /* WARN_ZERO */
        ZERO = 0; /* reset back to zero before next instruction */
      }
    } else /* simSKIPNEXT check */
     STATE &= ~simSKIPNEXT;

    /* If the delay slot was active before the instruction is
       executed, then update the PC to its new value: */
    if (DSSTATE) {
#ifdef DEBUG
      printf("DBG: dsstate set before instruction execution - updating PC to 0x%s\n",pr_addr(DSPC));
#endif /* DEBUG */
      PC = DSPC;
      CANCELDELAYSLOT();
    }

    if (MIPSISA < 4)
      PENDING_TICK();

#if !defined(FASTSIM)
    if (sim_events_tickn (sd, pipeline_count))
      {
	/* cpu->cia = cia; */
	sim_events_process (sd);
      }
#else
    if (sim_events_tick (sd))
      {
	/* cpu->cia = cia; */
	sim_events_process (sd);
      }
#endif /* FASTSIM */
  }
}
#endif


/* This code copied from gdb's utils.c.  Would like to share this code,
   but don't know of a common place where both could get to it. */

/* Temporary storage using circular buffer */
#define NUMCELLS 16
#define CELLSIZE 32
static char*
get_cell()
{
  static char buf[NUMCELLS][CELLSIZE];
  static int cell=0;
  if (++cell>=NUMCELLS) cell=0;
  return buf[cell];
}     

/* Print routines to handle variable size regs, etc */

/* Eliminate warning from compiler on 32-bit systems */
static int thirty_two = 32;	

char* 
pr_addr(addr)
  SIM_ADDR addr;
{
  char *paddr_str=get_cell();
  switch (sizeof(addr))
    {
      case 8:
        sprintf(paddr_str,"%08lx%08lx",
		(unsigned long)(addr>>thirty_two),(unsigned long)(addr&0xffffffff));
	break;
      case 4:
        sprintf(paddr_str,"%08lx",(unsigned long)addr);
	break;
      case 2:
        sprintf(paddr_str,"%04x",(unsigned short)(addr&0xffff));
	break;
      default:
        sprintf(paddr_str,"%x",addr);
    }
  return paddr_str;
}

char* 
pr_uword64(addr)
  uword64 addr;
{
  char *paddr_str=get_cell();
  sprintf(paddr_str,"%08lx%08lx",
          (unsigned long)(addr>>thirty_two),(unsigned long)(addr&0xffffffff));
  return paddr_str;
}


void
pending_tick (SIM_DESC sd,
	      sim_cpu *cpu,
	      address_word cia)
{
  if (PENDING_TRACE)							
    sim_io_printf (sd, "PENDING_DRAIN - pending_in = %d, pending_out = %d, pending_total = %d\n", PENDING_IN, PENDING_OUT, PENDING_TOTAL); 
  if (PENDING_OUT != PENDING_IN)					
    {									
      int loop;							
      int index = PENDING_OUT;					
      int total = PENDING_TOTAL;					
      if (PENDING_TOTAL == 0)						
	sim_engine_abort (SD, CPU, cia, "PENDING_DRAIN - Mis-match on pending update pointers\n"); 
      for (loop = 0; (loop < total); loop++)				
	{								
	  if (PENDING_SLOT_DEST[index] != NULL)			
	    {								
	      PENDING_SLOT_DELAY[index] -= 1;				
	      if (PENDING_SLOT_DELAY[index] == 0)			
		{							
		  if (PENDING_SLOT_BIT[index] >= 0)			
		    switch (PENDING_SLOT_SIZE[index])                 
		      {						
		      case 32:					
			if (PENDING_SLOT_VALUE[index])		
			  *(unsigned32*)PENDING_SLOT_DEST[index] |= 	
			    BIT32 (PENDING_SLOT_BIT[index]);		
			else						
			  *(unsigned32*)PENDING_SLOT_DEST[index] &= 	
			    BIT32 (PENDING_SLOT_BIT[index]);		
			break;					
		      case 64:					
			if (PENDING_SLOT_VALUE[index])		
			  *(unsigned64*)PENDING_SLOT_DEST[index] |= 	
			    BIT64 (PENDING_SLOT_BIT[index]);		
			else						
			  *(unsigned64*)PENDING_SLOT_DEST[index] &= 	
			    BIT64 (PENDING_SLOT_BIT[index]);		
			break;					
			break;					
		      }
		  else
		    switch (PENDING_SLOT_SIZE[index])                 
		      {						
		      case 32:					
			*(unsigned32*)PENDING_SLOT_DEST[index] = 	
			  PENDING_SLOT_VALUE[index];			
			break;					
		      case 64:					
			*(unsigned64*)PENDING_SLOT_DEST[index] = 	
			  PENDING_SLOT_VALUE[index];			
			break;					
		      }							
		}							
	      if (PENDING_OUT == index)				
		{							
		  PENDING_SLOT_DEST[index] = NULL;			
		  PENDING_OUT = (PENDING_OUT + 1) % PSLOTS;		
		  PENDING_TOTAL--;					
		}							
	    }								
	}								
      index = (index + 1) % PSLOTS;					
    }									
}

/*---------------------------------------------------------------------------*/
/*> EOF interp.c <*/
