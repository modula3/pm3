#include "sim-main.h"
#include "sim-options.h"
#include "v850_sim.h"
#include "sim-assert.h"

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

#include "bfd.h"

#ifndef INLINE
#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif
#endif


/* For compatibility */
SIM_DESC simulator;



/* v850 interrupt model */

enum interrupt_type
{
  int_reset,
  int_nmi,
  int_intov1,
  int_intp10,
  int_intp11,
  int_intp12,
  int_intp13,
  int_intcm4,
  num_int_types
};

char *interrupt_names[] = {
  "reset",
  "nmi",
  "intov1",
  "intp10",
  "intp11",
  "intp12",
  "intp13",
  "intcm4",
  NULL
};

static void
do_interrupt (sd, data)
     SIM_DESC sd;
     void *data;
{
  char **interrupt_name = (char**)data;
  enum interrupt_type inttype;
  inttype = (interrupt_name - STATE_WATCHPOINTS (sd)->interrupt_names);

  /* For a hardware reset, drop everything and jump to the start
     address */
  if (inttype == int_reset)
    {
      PC = 0;
      PSW = 0x20;
      ECR = 0;
      sim_engine_restart (sd, NULL, NULL, NULL_CIA);
    }

  /* Deliver an NMI when allowed */
  if (inttype == int_nmi)
    {
      if (PSW & PSW_NP)
	{
	  /* We're already working on an NMI, so this one must wait
	     around until the previous one is done.  The processor
	     ignores subsequent NMIs, so we don't need to count them.
	     Just keep re-scheduling a single NMI until it manages to
	     be delivered */
	  if (STATE_CPU (sd, 0)->pending_nmi != NULL)
	    sim_events_deschedule (sd, STATE_CPU (sd, 0)->pending_nmi);
	  STATE_CPU (sd, 0)->pending_nmi =
	    sim_events_schedule (sd, 1, do_interrupt, data);
	  return;
	}
      else
	{
	  /* NMI can be delivered.  Do not deschedule pending_nmi as
             that, if still in the event queue, is a second NMI that
             needs to be delivered later. */
	  FEPC = PC;
	  FEPSW = PSW;
	  /* Set the FECC part of the ECR. */
	  ECR &= 0x0000ffff;
	  ECR |= 0x10;
	  PSW |= PSW_NP;
	  PSW &= ~PSW_EP;
	  PSW |= PSW_ID;
	  PC = 0x10;
	  sim_engine_restart (sd, NULL, NULL, NULL_CIA);
	}
    }

  /* deliver maskable interrupt when allowed */
  if (inttype > int_nmi && inttype < num_int_types)
    {
      if ((PSW & PSW_NP) || (PSW & PSW_ID))
	{
	  /* Can't deliver this interrupt, reschedule it for later */
	  sim_events_schedule (sd, 1, do_interrupt, data);
	  return;
	}
      else
	{
	  /* save context */
	  EIPC = PC;
	  EIPSW = PSW;
	  /* Disable further interrupts.  */
	  PSW |= PSW_ID;
	  /* Indicate that we're doing interrupt not exception processing.  */
	  PSW &= ~PSW_EP;
	  /* Clear the EICC part of the ECR, will set below. */
	  ECR &= 0xffff0000;
	  switch (inttype)
	    {
	    case int_intov1:
	      PC = 0x80;
	      ECR |= 0x80;
	      break;
	    case int_intp10:
	      PC = 0x90;
	      ECR |= 0x90;
	      break;
	    case int_intp11:
	      PC = 0xa0;
	      ECR |= 0xa0;
	      break;
	    case int_intp12:
	      PC = 0xb0;
	      ECR |= 0xb0;
	      break;
	    case int_intp13:
	      PC = 0xc0;
	      ECR |= 0xc0;
	      break;
	    case int_intcm4:
	      PC = 0xd0;
	      ECR |= 0xd0;
	      break;
	    default:
	      /* Should never be possible.  */
	      sim_engine_abort (sd, NULL, NULL_CIA,
				"do_interrupt - internal error - bad switch");
	      break;
	    }
	}
      sim_engine_restart (sd, NULL, NULL, NULL_CIA);
    }
  
  /* some other interrupt? */
  sim_engine_abort (sd, NULL, NULL_CIA,
		    "do_interrupt - internal error - interrupt %d unknown",
		    inttype);
}

/* These default values correspond to expected usage for the chip.  */

uint32 OP[4];


SIM_DESC
sim_open (kind, cb, abfd, argv)
     SIM_OPEN_KIND kind;
     host_callback *cb;
     struct _bfd *abfd;
     char **argv;
{
  SIM_DESC sd = sim_state_alloc (kind, cb);
  int mach;

  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);

  /* for compatibility */
  simulator = sd;

  /* FIXME: should be better way of setting up interrupts */
  STATE_WATCHPOINTS (sd)->pc = &(PC);
  STATE_WATCHPOINTS (sd)->sizeof_pc = sizeof (PC);
  STATE_WATCHPOINTS (sd)->interrupt_handler = do_interrupt;
  STATE_WATCHPOINTS (sd)->interrupt_names = interrupt_names;

  if (sim_pre_argv_init (sd, argv[0]) != SIM_RC_OK)
    return 0;

  /* Allocate core managed memory */

  /* "Mirror" the ROM addresses below 1MB. */
  sim_do_commandf (sd, "memory region 0,0x100000,0x%lx", V850_ROM_SIZE);
  /* Chunk of ram adjacent to rom */
  sim_do_commandf (sd, "memory region 0x100000,0x%lx", V850_LOW_END-0x100000);
  /* peripheral I/O region - mirror 1K across 4k (0x1000) */
  sim_do_command (sd, "memory region 0xfff000,0x1000,1024");
  /* similarly if in the internal RAM region */
  sim_do_command (sd, "memory region 0xffe000,0x1000,1024");

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

  /* establish any remaining configuration options */
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


  /* determine the machine type */
  if (STATE_ARCHITECTURE (sd) != NULL
      && STATE_ARCHITECTURE (sd)->arch == bfd_arch_v850)
    mach = STATE_ARCHITECTURE (sd)->mach;
  else
    mach = bfd_mach_v850; /* default */

  /* set machine specific configuration */
  switch (mach)
    {
    case bfd_mach_v850:
    }

  return sd;
}


void
sim_close (sd, quitting)
     SIM_DESC sd;
     int quitting;
{
  sim_module_uninstall (sd);
}

int
sim_stop (sd)
     SIM_DESC sd;
{
  return 0;
}

void
sim_info (sd, verbose)
     SIM_DESC sd;
     int verbose;
{
  profile_print (sd, STATE_VERBOSE_P (sd), NULL, NULL);
}

SIM_RC
sim_create_inferior (sd, prog_bfd, argv, env)
     SIM_DESC sd;
     struct _bfd *prog_bfd;
     char **argv;
     char **env;
{
  memset (&State, 0, sizeof (State));
  if (prog_bfd != NULL)
    PC = bfd_get_start_address (prog_bfd);
  return SIM_RC_OK;
}

void
sim_fetch_register (sd, rn, memory)
     SIM_DESC sd;
     int rn;
     unsigned char *memory;
{
  *(unsigned32*)memory = H2T_4 (State.regs[rn]);
}
 
void
sim_store_register (sd, rn, memory)
     SIM_DESC sd;
     int rn;
     unsigned char *memory;
{
  State.regs[rn] = T2H_4 (*(unsigned32*)memory);
}

void
sim_do_command (sd, cmd)
     SIM_DESC sd;
     char *cmd;
{
  char *mm_cmd = "memory-map";
  char *int_cmd = "interrupt";

  if (sim_args_command (sd, cmd) != SIM_RC_OK)
    {
      if (strncmp (cmd, mm_cmd, strlen (mm_cmd) == 0))
	sim_io_eprintf (sd, "`memory-map' command replaced by `sim memory'\n");
      else if (strncmp (cmd, int_cmd, strlen (int_cmd)) == 0)
	sim_io_eprintf (sd, "`interrupt' command replaced by `sim watch'\n");
      else
	sim_io_eprintf (sd, "Unknown command `%s'\n", cmd);
    }
}
