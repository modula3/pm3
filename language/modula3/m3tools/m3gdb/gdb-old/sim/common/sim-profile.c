/* Default profiling support.
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

#include "sim-main.h"
#include "sim-io.h"
#include "sim-options.h"
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

#define COMMAS(n) sim_add_commas (comma_buf, sizeof (comma_buf), (n))

static MODULE_UNINSTALL_FN profile_uninstall;

#if WITH_PROFILE_INSN_P || WITH_PROFILE_MEMORY_P || WITH_PROFILE_CORE_P || WITH_PROFILE_PC_P
static void print_bar (SIM_DESC, unsigned int, unsigned int, unsigned int);
#endif

static DECLARE_OPTION_HANDLER (profile_option_handler);

#define OPTION_PROFILE_INSN		(OPTION_START + 0)
#define OPTION_PROFILE_MEMORY		(OPTION_START + 1)
#define OPTION_PROFILE_MODEL		(OPTION_START + 2)
#define OPTION_PROFILE_FILE		(OPTION_START + 3)
#define OPTION_PROFILE_RANGE		(OPTION_START + 4)
#define OPTION_PROFILE_CORE		(OPTION_START + 5)
#define OPTION_PROFILE_PC		(OPTION_START + 6)
#define OPTION_PROFILE_PC_RANGE		(OPTION_START + 7)
#define OPTION_PROFILE_PC_GRANULARITY	(OPTION_START + 8)

static const OPTION profile_options[] = {
  { {"profile", no_argument, NULL, 'p'},
      'p', NULL, "Perform profiling",
      profile_option_handler },
  { {"profile-insn", no_argument, NULL, OPTION_PROFILE_INSN},
      '\0', NULL, "Perform instruction profiling",
      profile_option_handler },
  { {"profile-memory", no_argument, NULL, OPTION_PROFILE_MEMORY},
      '\0', NULL, "Perform memory profiling",
      profile_option_handler },
  { {"profile-core", no_argument, NULL, OPTION_PROFILE_CORE},
      '\0', NULL, "Perform CORE profiling",
      profile_option_handler },
  { {"profile-model", no_argument, NULL, OPTION_PROFILE_MODEL},
      '\0', NULL, "Perform model profiling",
      profile_option_handler },

  { {"profile-file", required_argument, NULL, OPTION_PROFILE_FILE},
      '\0', "FILE NAME", "Specify profile output file",
      profile_option_handler },

  { {"profile-pc", no_argument, NULL, OPTION_PROFILE_PC},
      '\0', NULL, "Perform PC profiling",
      profile_option_handler },
  { {"profile-pc-frequency", required_argument, NULL, 'F'},
      'F', "PC PROFILE FREQUENCY", "Specified PC profiling frequency",
      profile_option_handler },
  { {"profile-pc-size", required_argument, NULL, 'S'},
      'S', "PC PROFILE SIZE", "Specify PC profiling size",
      profile_option_handler },
  { {"profile-pc-granularity", required_argument, NULL, OPTION_PROFILE_PC_GRANULARITY},
      '\0', "PC PROFILE GRANULARITY", "Specify PC profiling sample coverage",
      profile_option_handler },
  { {"profile-pc-range", required_argument, NULL, OPTION_PROFILE_PC_RANGE},
      '\0', "BASE,BOUND", "Specify PC profiling address range",
      profile_option_handler },

#if 0 /*FIXME:wip*/
  { {"profile-range", required_argument, NULL, OPTION_PROFILE_RANGE},
      0, NULL, "Specify range of addresses to profile",
      profile_option_handler },
#endif

  { {NULL, no_argument, NULL, 0}, '\0', NULL, NULL, NULL }
};

static SIM_RC
profile_option_handler (SIM_DESC sd,
			int opt,
			char *arg,
			int is_command)
{
  int i,n;

  switch (opt)
    {
    case 'p' :
      if (! WITH_PROFILE)
	sim_io_eprintf (sd, "Profiling not compiled in, -p option ignored\n");
      else
	{
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    for (i = 0; i < MAX_PROFILE_VALUES; ++i)
	      CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[i] = 1;
	}
      break;

    case OPTION_PROFILE_INSN :
#if WITH_PROFILE_INSN_P
      for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_INSN_IDX] = 1;
#else
      sim_io_eprintf (sd, "Instruction profiling not compiled in, `--profile-insn' ignored\n");
#endif
      break;

    case OPTION_PROFILE_MEMORY :
#if WITH_PROFILE_MEMORY_P
      for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_MEMORY_IDX] = 1;
#else
      sim_io_eprintf (sd, "Memory profiling not compiled in, `--profile-memory' ignored\n");
#endif
      break;

    case OPTION_PROFILE_CORE :
#if WITH_PROFILE_CORE_P
      for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_CORE_IDX] = 1;
#else
      sim_io_eprintf (sd, "CORE profiling not compiled in, `--profile-core' ignored\n");
#endif
      break;

    case OPTION_PROFILE_MODEL :
#if WITH_PROFILE_MODEL_P
      for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_MODEL_IDX] = 1;
#else
      sim_io_eprintf (sd, "Model profiling not compiled in, `--profile-model' ignored\n");
#endif
      break;

    case OPTION_PROFILE_FILE :
      /* FIXME: Might want this to apply to pc profiling only,
	 or have two profile file options.  */
      if (! WITH_PROFILE)
	sim_io_eprintf (sd, "Profiling not compiled in, `--profile-file' ignored\n");
      else
	{
	  FILE *f = fopen (arg, "w");

	  if (f == NULL)
	    {
	      sim_io_eprintf (sd, "Unable to open profile output file `%s'\n", arg);
	      return SIM_RC_FAIL;
	    }
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    PROFILE_FILE (CPU_PROFILE_DATA (STATE_CPU (sd, n))) = f;
	}
      break;

    case OPTION_PROFILE_PC:
      if (WITH_PROFILE_PC_P)
	{
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_PC_IDX] = 1;
	}
      else
	sim_io_eprintf (sd, "PC profiling not compiled in, `--profile-pc' ignored\n");
      break;

    case 'F' :
      if (WITH_PROFILE_PC_P)
	{
	  /* FIXME: Validate arg.  */
	  i = atoi (arg);
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    PROFILE_PC_FREQ (CPU_PROFILE_DATA (STATE_CPU (sd, n))) = i;
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_PC_IDX] = 1;
	}
      else
	sim_io_eprintf (sd, "PC profiling not compiled in, `--profile-pc-frequency' ignored\n");
      break;

    case 'S' :
      if (WITH_PROFILE_PC_P)
	{
	  /* FIXME: Validate arg.  */
	  i = atoi (arg);
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    PROFILE_PC_NR_BUCKETS (CPU_PROFILE_DATA (STATE_CPU (sd, n))) = i;
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_PC_IDX] = 1;
	}
      else
	sim_io_eprintf (sd, "PC profiling not compiled in, `--profile-pc-size' ignored\n");
      break;

    case OPTION_PROFILE_PC_GRANULARITY:
      if (WITH_PROFILE_PC_P)
	{
	  int shift;
	  i = atoi (arg);
	  /* check that the granularity is a power of two */
	  shift = 0;
	  while (i > (1 << shift))
	    {
	      shift += 1;
	    }
	  if (i != (1 << shift))
	    {
	      sim_io_eprintf (sd, "PC profiling granularity not a power of two\n");
	      return SIM_RC_FAIL;
	    }
	  if (shift == 0)
	    {
	      sim_io_eprintf (sd, "PC profiling granularity too small");
	      return SIM_RC_FAIL;
	    }
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    PROFILE_PC_SHIFT (CPU_PROFILE_DATA (STATE_CPU (sd, n))) = shift;
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_PC_IDX] = 1;
	}
      else
	sim_io_eprintf (sd, "PC profiling not compiled in, `--profile-pc-granularity' ignored\n");
      break;

    case OPTION_PROFILE_PC_RANGE:
      if (WITH_PROFILE_PC_P)
	{
	  /* FIXME: Validate args */
	  char *chp = arg;
	  unsigned long base;
	  unsigned long bound;
	  base = strtoul (chp, &chp, 0);
	  if (*chp != ',')
	    {
	      sim_io_eprintf (sd, "--profile-pc-range missing BOUND argument\n");
	      return SIM_RC_FAIL;
	    }
	  bound = strtoul (chp + 1, NULL, 0);
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    {
	      PROFILE_PC_START (CPU_PROFILE_DATA (STATE_CPU (sd, n))) = base;
	      PROFILE_PC_END (CPU_PROFILE_DATA (STATE_CPU (sd, n))) = bound;
	    }	      
	  for (n = 0; n < MAX_NR_PROCESSORS; ++n)
	    CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_PC_IDX] = 1;
	}
      else
	sim_io_eprintf (sd, "PC profiling not compiled in, `--profile-pc-range' ignored\n");


#if 0 /* FIXME:wip */
    case OPTION_PROFILE_RANGE :
      break;
#endif
    }

  return SIM_RC_OK;
}

/* PC profiling support */

#if WITH_PROFILE_PC_P

static void
profile_pc_cleanup (SIM_DESC sd)
{
  int n;
  for (n = 0; n < MAX_NR_PROCESSORS; n++)
    {
      sim_cpu *cpu = STATE_CPU (sd, n);
      PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
      if (PROFILE_PC_COUNT (data) != NULL)
	zfree (PROFILE_PC_COUNT (data));
      PROFILE_PC_COUNT (data) = NULL;
      if (PROFILE_PC_EVENT (data) != NULL)
	sim_events_deschedule (sd, PROFILE_PC_EVENT (data));
      PROFILE_PC_EVENT (data) = NULL;
    }
}


static void
profile_pc_uninstall (SIM_DESC sd)
{
  profile_pc_cleanup (sd);
}

static void
profile_pc_event (SIM_DESC sd,
		  void *data)
{
  sim_cpu *cpu = (sim_cpu*) data;
  PROFILE_DATA *profile = CPU_PROFILE_DATA (cpu);
  address_word pc;
  unsigned i;
  switch (STATE_WATCHPOINTS (sd)->sizeof_pc)
    {
    case 2: pc = *(unsigned_2*)(STATE_WATCHPOINTS (sd)->pc) ; break;
    case 4: pc = *(unsigned_4*)(STATE_WATCHPOINTS (sd)->pc) ; break;
    case 8: pc = *(unsigned_8*)(STATE_WATCHPOINTS (sd)->pc) ; break;
    default: pc = 0;
    }
  i = (pc - PROFILE_PC_START (profile)) >> PROFILE_PC_SHIFT (profile);
  if (i < PROFILE_PC_NR_BUCKETS (profile))
    PROFILE_PC_COUNT (profile) [i] += 1; /* Overflow? */
  else
    PROFILE_PC_COUNT (profile) [PROFILE_PC_NR_BUCKETS (profile)] += 1;
  PROFILE_PC_EVENT (profile) = 
    sim_events_schedule (sd, PROFILE_PC_FREQ (profile), profile_pc_event, cpu);
}

static SIM_RC
profile_pc_init (SIM_DESC sd)
{
  int n;
  profile_pc_cleanup (sd);
  for (n = 0; n < MAX_NR_PROCESSORS; n++)
    {
      sim_cpu *cpu = STATE_CPU (sd, n);
      PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
      if (CPU_PROFILE_FLAGS (STATE_CPU (sd, n))[PROFILE_PC_IDX]
	  && STATE_WATCHPOINTS (sd)->pc != NULL)
	{
	  int bucket_size;
	  /* fill in the frequency if not specified */
	  if (PROFILE_PC_FREQ (data) == 0)
	    PROFILE_PC_FREQ (data) = 256;
	  /* fill in the start/end if not specified */
	  if (PROFILE_PC_END (data) == 0)
	    {
	      PROFILE_PC_START (data) = STATE_TEXT_START (sd);
	      PROFILE_PC_END (data) = STATE_TEXT_END (sd);
	    }
	  /* Compute the number of buckets if not specified. */
	  if (PROFILE_PC_NR_BUCKETS (data) == 0)
	    {
	      if (PROFILE_PC_BUCKET_SIZE (data) == 0)
		PROFILE_PC_NR_BUCKETS (data) = 16;
	      else
		{
		  if (PROFILE_PC_END (data) == 0)
		    {
		      /* nr_buckets = (full-address-range / 2) / (bucket_size / 2) */
		      PROFILE_PC_NR_BUCKETS (data) =
			((1 << (STATE_WATCHPOINTS (sd)->sizeof_pc) * (8 - 1))
			 / (PROFILE_PC_BUCKET_SIZE (data) / 2));
		    }
		  else
		    {
		      PROFILE_PC_NR_BUCKETS (data) =
			((PROFILE_PC_END (data)
			  - PROFILE_PC_START (data)
			  + PROFILE_PC_BUCKET_SIZE (data) - 1)
			 / PROFILE_PC_BUCKET_SIZE (data));
		    }
		}
	    }
	  /* Compute the bucket size if not specified.  Ensure that it
             is rounded up to the next power of two */
	  if (PROFILE_PC_BUCKET_SIZE (data) == 0)
	    {
	      if (PROFILE_PC_END (data) == 0)
		/* bucket_size = (full-address-range / 2) / (nr_buckets / 2) */
		bucket_size = ((1 << ((STATE_WATCHPOINTS (sd)->sizeof_pc * 8) - 1))
			       / (PROFILE_PC_NR_BUCKETS (data) / 2));
	      else
		bucket_size = ((PROFILE_PC_END (data)
				- PROFILE_PC_START (data)
				+ PROFILE_PC_NR_BUCKETS (data) - 1)
			       / PROFILE_PC_NR_BUCKETS (data));
	      PROFILE_PC_SHIFT (data) = 0;
	      while (bucket_size < PROFILE_PC_BUCKET_SIZE (data))
		{
		  PROFILE_PC_SHIFT (data) += 1;
		}
	    }
	  /* Align the end address with bucket size */
	  if (PROFILE_PC_END (data) != 0)
	    PROFILE_PC_END (data) = (PROFILE_PC_START (data)
				     + (PROFILE_PC_BUCKET_SIZE (data)
					* PROFILE_PC_NR_BUCKETS (data)));
	  /* create the relevant buffers */
	  PROFILE_PC_COUNT (data) =
	    NZALLOC (unsigned, PROFILE_PC_NR_BUCKETS (data) + 1);
	  PROFILE_PC_EVENT (data) =
	    sim_events_schedule (sd,
				 PROFILE_PC_FREQ (data),
				 profile_pc_event,
				 cpu);
	}
    }
  return SIM_RC_OK;
}

static void
profile_print_pc (sim_cpu *cpu, int verbose)
{
  SIM_DESC sd = CPU_STATE (cpu);
  PROFILE_DATA *profile = CPU_PROFILE_DATA (cpu);
  char comma_buf[20];
  unsigned max_val;
  unsigned total;
  unsigned i;

  sim_io_printf (sd, "Program Counter Statistics:\n\n");

  /* First pass over data computes various things.  */
  max_val = 0;
  total = 0;
  for (i = 0; i <= PROFILE_PC_NR_BUCKETS (profile); ++i)
    {
      total += PROFILE_PC_COUNT (profile) [i];
      if (PROFILE_PC_COUNT (profile) [i] > max_val)
	max_val = PROFILE_PC_COUNT (profile) [i];
    }

  sim_io_printf (sd, "  Total samples: %s\n",
		 COMMAS (total));
  sim_io_printf (sd, "  Granularity: %s bytes per bucket\n",
		 COMMAS (PROFILE_PC_BUCKET_SIZE (profile)));
  sim_io_printf (sd, "  Size: %s buckets\n",
		 COMMAS (PROFILE_PC_NR_BUCKETS (profile)));
  sim_io_printf (sd, "  Frequency: %s cycles per sample\n",
		 COMMAS (PROFILE_PC_FREQ (profile)));

  if (PROFILE_PC_END (profile) != 0)
    sim_io_printf (sd, "  Range: 0x%lx 0x%lx\n",
		   (long) PROFILE_PC_START (profile),
		   (long) PROFILE_PC_END (profile));

  if (verbose && max_val != 0)
    {
      /* Now we can print the histogram.  */
      sim_io_printf (sd, "\n");
      for (i = 0; i <= PROFILE_PC_NR_BUCKETS (profile); ++i)
	{
	  if (PROFILE_PC_COUNT (profile) [i] != 0)
	    {
	      sim_io_printf (sd, "  ");
	      if (i == PROFILE_PC_NR_BUCKETS (profile))
		sim_io_printf (sd, "%10s:", "overflow");
	      else
		sim_io_printf (sd, "0x%08lx:",
			       (long) (PROFILE_PC_START (profile)
				       + (i * PROFILE_PC_BUCKET_SIZE (profile))));
	      sim_io_printf (sd, " %*s",
			     max_val < 10000 ? 5 : 10,
			     COMMAS (PROFILE_PC_COUNT (profile) [i]));
	      sim_io_printf (sd, " %4.1f",
			     (PROFILE_PC_COUNT (profile) [i] * 100.0) / total);
	      sim_io_printf (sd, ": ");
	      print_bar (sd, PROFILE_HISTOGRAM_WIDTH,
			 PROFILE_PC_COUNT (profile) [i],
			 max_val);
	      sim_io_printf (sd, "\n");
	    }
	}
    }

  /* dump the histogram to the file "gmon.out" using BSD's gprof file
     format */
  /* Since a profile data file is in the native format of the host on
     which the profile is being, endian issues are not considered in
     the code below. */
  /* FIXME: Is this the best place for this code? */
  {
    FILE *pf = fopen ("gmon.out", "wb");
    
    if (pf == NULL)
      sim_io_eprintf (sd, "Failed to open \"gmon.out\" profile file\n");
    else
      {
	int ok;
	/* FIXME: what if the target has a 64 bit PC? */
	unsigned32 header[3];
	unsigned loop;
	if (PROFILE_PC_END (profile) != 0)
	  {
	    header[0] = PROFILE_PC_START (profile);
	    header[1] = PROFILE_PC_END (profile);
	  }
	else
	  {
	    header[0] = 0;
	    header[1] = 0;
	  }
	/* size of sample buffer (+ header) */
	header[2] = PROFILE_PC_NR_BUCKETS (profile) * 2 + sizeof (header);
	ok = fwrite (&header, sizeof (header), 1, pf);
	for (loop = 0;
	     ok && (loop < PROFILE_PC_NR_BUCKETS (profile));
	     loop++)
	  {
	    signed16 sample;
	    if (PROFILE_PC_COUNT (profile) [loop] >= 0xffff)
	      sample = 0xffff;
	    else
	      sample = PROFILE_PC_COUNT (profile) [loop];
	    ok = fwrite (&sample, sizeof (sample), 1, pf);
	  }
	if (ok == 0)
	  sim_io_eprintf (sd, "Failed to write to \"gmon.out\" profile file\n");
	fclose(pf);
      }
  }

  sim_io_printf (sd, "\n");
}

#endif

/* Summary printing support.  */

#if WITH_PROFILE_INSN_P

static void
profile_print_insn (sim_cpu *cpu, int verbose)
{
  unsigned int i, n, total, max_val, max_name_len;
  SIM_DESC sd = CPU_STATE (cpu);
  PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
  char comma_buf[20];

  sim_io_printf (sd, "Instruction Statistics:\n\n");

  /* First pass over data computes various things.  */
  max_val = 0;
  total = 0;
  max_name_len = 0;
  for (i = 0; i < MAX_INSNS; ++i)
    {
      if (INSN_NAME (i) == NULL)
	continue;
      total += PROFILE_INSN_COUNT (data) [i];
      if (PROFILE_INSN_COUNT (data) [i] > max_val)
	max_val = PROFILE_INSN_COUNT (data) [i];
      n = strlen (INSN_NAME (i));
      if (n > max_name_len)
	max_name_len = n;
    }
  /* set the total insn count, in case client is being lazy */
  if (PROFILE_TOTAL_INSN_COUNT (data))
    PROFILE_TOTAL_INSN_COUNT (data) = total;

  sim_io_printf (sd, "  Total: %s insns\n", COMMAS (total));

  if (verbose && max_val != 0)
    {
      /* Now we can print the histogram.  */
      sim_io_printf (sd, "\n");
      for (i = 0; i < MAX_INSNS; ++i)
	{
	  if (INSN_NAME (i) == NULL)
	    continue;
	  if (PROFILE_INSN_COUNT (data) [i] != 0)
	    {
	      sim_io_printf (sd, "   %*s: %*s: ",
			     max_name_len, INSN_NAME (i),
			     max_val < 10000 ? 5 : 10,
			     COMMAS (PROFILE_INSN_COUNT (data) [i]));
	      print_bar (sd, PROFILE_HISTOGRAM_WIDTH,
			 PROFILE_INSN_COUNT (data) [i],
			 max_val);
	      sim_io_printf (sd, "\n");
	    }
	}
    }

  sim_io_printf (sd, "\n");
}

#endif

#if WITH_PROFILE_MEMORY_P

static void
profile_print_memory (sim_cpu *cpu, int verbose)
{
  unsigned int i, n;
  unsigned int total_read, total_write;
  unsigned int max_val, max_name_len;
  /* FIXME: Need to add smp support.  */
  SIM_DESC sd = CPU_STATE (cpu);
  PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
  char comma_buf[20];

  sim_io_printf (sd, "Memory Access Statistics:\n\n");

  /* First pass over data computes various things.  */
  max_val = total_read = total_write = max_name_len = 0;
  for (i = 0; i < MAX_MODES; ++i)
    {
      total_read += PROFILE_READ_COUNT (data) [i];
      total_write += PROFILE_WRITE_COUNT (data) [i];
      if (PROFILE_READ_COUNT (data) [i] > max_val)
	max_val = PROFILE_READ_COUNT (data) [i];
      if (PROFILE_WRITE_COUNT (data) [i] > max_val)
	max_val = PROFILE_WRITE_COUNT (data) [i];
      n = strlen (MODE_NAME (i));
      if (n > max_name_len)
	max_name_len = n;
    }

  /* One could use PROFILE_LABEL_WIDTH here.  I chose not to.  */
  sim_io_printf (sd, "  Total read:  %s accesses\n",
		 COMMAS (total_read));
  sim_io_printf (sd, "  Total write: %s accesses\n",
		 COMMAS (total_write));

  if (verbose && max_val != 0)
    {
      /* FIXME: Need to separate instruction fetches from data fetches
	 as the former swamps the latter.  */
      /* Now we can print the histogram.  */
      sim_io_printf (sd, "\n");
      for (i = 0; i < MAX_MODES; ++i)
	{
	  if (PROFILE_READ_COUNT (data) [i] != 0)
	    {
	      sim_io_printf (sd, "   %*s read:  %*s: ",
			     max_name_len, MODE_NAME (i),
			     max_val < 10000 ? 5 : 10,
			     COMMAS (PROFILE_READ_COUNT (data) [i]));
	      print_bar (sd, PROFILE_HISTOGRAM_WIDTH,
			 PROFILE_READ_COUNT (data) [i],
			 max_val);
	      sim_io_printf (sd, "\n");
	    }
	  if (PROFILE_WRITE_COUNT (data) [i] != 0)
	    {
	      sim_io_printf (sd, "   %*s write: %*s: ",
			     max_name_len, MODE_NAME (i),
			     max_val < 10000 ? 5 : 10,
			     COMMAS (PROFILE_WRITE_COUNT (data) [i]));
	      print_bar (sd, PROFILE_HISTOGRAM_WIDTH,
			 PROFILE_WRITE_COUNT (data) [i],
			 max_val);
	      sim_io_printf (sd, "\n");
	    }
	}
    }

  sim_io_printf (sd, "\n");
}

#endif

#if WITH_PROFILE_CORE_P

static void
profile_print_core (sim_cpu *cpu, int verbose)
{
  unsigned int total;
  unsigned int max_val;
  /* FIXME: Need to add smp support.  */
  SIM_DESC sd = CPU_STATE (cpu);
  PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
  char comma_buf[20];

  sim_io_printf (sd, "CORE Statistics:\n\n");

  /* First pass over data computes various things.  */
  {
    sim_core_maps map;
    total = 0;
    max_val = 0;
    for (map = 0; map < nr_sim_core_maps; map++)
      {
	total += PROFILE_CORE_COUNT (data) [map];
	if (PROFILE_CORE_COUNT (data) [map] > max_val)
	  max_val = PROFILE_CORE_COUNT (data) [map];
      }
  }

  /* One could use PROFILE_LABEL_WIDTH here.  I chose not to.  */
  sim_io_printf (sd, "  Total:  %s accesses\n",
		 COMMAS (total));

  if (verbose && max_val != 0)
    {
      sim_core_maps map;
      /* Now we can print the histogram.  */
      sim_io_printf (sd, "\n");
      for (map = 0; map < nr_sim_core_maps; map++)
	{
	  if (PROFILE_CORE_COUNT (data) [map] != 0)
	    {
	      switch (map)
		{
		case sim_core_read_map:
		  sim_io_printf (sd, "     read:");
		  break;
		case sim_core_write_map:
		  sim_io_printf (sd, "    write:");
		  break;
		case sim_core_execute_map:
		  sim_io_printf (sd, "     exec:");
		  break;
		case nr_sim_core_maps:
		  ; /* ignore */
		}
	      sim_io_printf (sd, "%*s: ",
			     max_val < 10000 ? 5 : 10,
			     COMMAS (PROFILE_CORE_COUNT (data) [map]));
	      print_bar (sd, PROFILE_HISTOGRAM_WIDTH,
			 PROFILE_CORE_COUNT (data) [map],
			 max_val);
	      sim_io_printf (sd, "\n");
	    }
	}
    }

  sim_io_printf (sd, "\n");
}

#endif

#if WITH_PROFILE_MODEL_P

static void
profile_print_model (sim_cpu *cpu, int verbose)
{
  SIM_DESC sd = CPU_STATE (cpu);
  PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
  unsigned long cti_stalls = PROFILE_MODEL_CTI_STALL_COUNT (data);
  unsigned long load_stalls = PROFILE_MODEL_LOAD_STALL_COUNT (data);
  unsigned long total = PROFILE_MODEL_CYCLE_COUNT (data)
    + cti_stalls + load_stalls;
  char comma_buf[20];

  sim_io_printf (sd, "Model %s Timing Information\n\n",
		 MODEL_NAME (CPU_MODEL (cpu)));
  sim_io_printf (sd, "  %-*s %s\n",
		 PROFILE_LABEL_WIDTH, "Taken branches:",
		 COMMAS (PROFILE_MODEL_TAKEN_COUNT (data)));
  sim_io_printf (sd, "  %-*s %s\n",
		 PROFILE_LABEL_WIDTH, "Untaken branches:",
		 COMMAS (PROFILE_MODEL_UNTAKEN_COUNT (data)));
  sim_io_printf (sd, "  %-*s %s\n",
		 PROFILE_LABEL_WIDTH, "Cycles stalled due to branches:",
		 COMMAS (cti_stalls));
  sim_io_printf (sd, "  %-*s %s\n",
		 PROFILE_LABEL_WIDTH, "Cycles stalled due to loads:",
		 COMMAS (load_stalls));
  sim_io_printf (sd, "  %-*s %s\n",
		 PROFILE_LABEL_WIDTH, "Total cycles (*approximate*):",
		 COMMAS (total));
  sim_io_printf (sd, "\n");
}

#endif


#if WITH_PROFILE_INSN_P || WITH_PROFILE_MEMORY_P || WITH_PROFILE_CORE_P || WITH_PROFILE_PC_P

static void
print_bar (SIM_DESC sd, unsigned int width,
	   unsigned int val, unsigned int max_val)
{
  unsigned int i, count;

  count = ((double) val / (double) max_val) * (double) width;

  for (i = 0; i < count; ++i)
    sim_io_printf (sd, "*");
}

#endif

/* Print the simulator's execution speed for CPU.  */

static void
profile_print_speed (sim_cpu *cpu)
{
  SIM_DESC sd = CPU_STATE (cpu);
  PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);
  unsigned long milliseconds = sim_events_elapsed_time (sd);
  unsigned long total = PROFILE_TOTAL_INSN_COUNT (data);
  char comma_buf[20];

  sim_io_printf (sd, "Simulator Execution Speed\n\n");

  if (total != 0)
    sim_io_printf (sd, "  Total instructions:   %s\n", COMMAS (total));

  if (milliseconds < 1000)
    sim_io_printf (sd, "  Total Execution Time: < 1 second\n\n");
  else
    {
      /* The printing of the time rounded to 2 decimal places makes the speed
	 calculation seem incorrect [even though it is correct].  So round
	 MILLISECONDS first. This can marginally affect the result, but it's
	 better that the user not perceive there's a math error.  */
      double secs = (double) milliseconds / 1000;
      secs = ((double) (unsigned long) (secs * 100 + .5)) / 100;
      sim_io_printf (sd, "  Total Execution Time: %.2f seconds\n", secs);
      /* Don't confuse things with data that isn't useful.
	 If we ran for less than 2 seconds, only use the data if we
	 executed more than 100,000 insns.  */
      if (secs >= 2 || total >= 100000)
	sim_io_printf (sd, "  Simulator Speed:      %s insns/second\n\n",
		       COMMAS ((unsigned long) ((double) total / secs)));
    }
}

/* Top level function to print all summary profile information.
   It is [currently] intended that all such data is printed by this function.
   I'd rather keep it all in one place for now.  To that end, MISC_CPU and
   MISC are callbacks used to print any miscellaneous data.

   One might want to add a user option that allows printing by type or by cpu
   (i.e. print all insn data for each cpu first, or print data cpu by cpu).
   This may be a case of featuritis so it's currently left out.

   Note that results are indented two spaces to distinguish them from
   section titles.  */

void
profile_print (SIM_DESC sd, int verbose,
	       PROFILE_CALLBACK *misc, PROFILE_CPU_CALLBACK *misc_cpu)
{
  int i,c;
  int print_title_p = 0;

  /* Only print the title if some data has been collected.  */
  /* FIXME: If the number of processors can be selected on the command line,
     then MAX_NR_PROCESSORS will need to take an argument of `sd'.  */

  for (c = 0; c < MAX_NR_PROCESSORS; ++c)
    {
      sim_cpu *cpu = STATE_CPU (sd, c);
      PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);

      for (i = 0; i < MAX_PROFILE_VALUES; ++i)
	if (PROFILE_FLAGS (data) [i])
	  print_title_p = 1;
      /* One could break out early if print_title_p is set.  */
    }
  if (print_title_p)
    sim_io_printf (sd, "Summary profiling results:\n\n");

  /* Loop, cpu by cpu, printing results.  */

  for (c = 0; c < MAX_NR_PROCESSORS; ++c)
    {
      sim_cpu *cpu = STATE_CPU (sd, c);
      PROFILE_DATA *data = CPU_PROFILE_DATA (cpu);

      if (MAX_NR_PROCESSORS > 1
	  && (0
#if WITH_PROFILE_INSN_P
	      || PROFILE_FLAGS (data) [PROFILE_INSN_IDX]
#endif
#if WITH_PROFILE_INSN_P
	      || PROFILE_FLAGS (data) [PROFILE_INSN_IDX]
#endif
#if WITH_PROFILE_MEMORY_P
	      || PROFILE_FLAGS (data) [PROFILE_MEMORY_IDX]
#endif
#if WITH_PROFILE_CORE_P
	      || PROFILE_FLAGS (data) [PROFILE_CORE_IDX]
#endif
#if WITH_PROFILE_MODEL_P
	      || PROFILE_FLAGS (data) [PROFILE_MODEL_IDX]
#endif
#if WITH_PROFILE_SCACHE_P && WITH_SCACHE
	      || PROFILE_FLAGS (data) [PROFILE_SCACHE_IDX]
#endif
#if WITH_PROFILE_PC_P
	      || PROFILE_FLAGS (data) [PROFILE_PC_IDX]
#endif
	      ))
	{
	  sim_io_printf (sd, "CPU %d\n\n", c);
	}

#if WITH_PROFILE_INSN_P
      if (PROFILE_FLAGS (data) [PROFILE_INSN_IDX])
	profile_print_insn (cpu, verbose);
#endif

#if WITH_PROFILE_MEMORY_P
      if (PROFILE_FLAGS (data) [PROFILE_MEMORY_IDX])
	profile_print_memory (cpu, verbose);
#endif

#if WITH_PROFILE_CORE_P
      if (PROFILE_FLAGS (data) [PROFILE_CORE_IDX])
	profile_print_core (cpu, verbose);
#endif

#if WITH_PROFILE_MODEL_P
      if (PROFILE_FLAGS (data) [PROFILE_MODEL_IDX])
	profile_print_model (cpu, verbose);
#endif

#if WITH_PROFILE_SCACHE_P && WITH_SCACHE
      if (PROFILE_FLAGS (data) [PROFILE_SCACHE_IDX])
	scache_print_profile (cpu, verbose);
#endif

#if WITH_PROFILE_PC_P
      if (PROFILE_FLAGS (data) [PROFILE_PC_IDX])
	profile_print_pc (cpu, verbose);
#endif

      /* Print cpu-specific data before the execution speed.  */
      if (misc_cpu != NULL)
	(*misc_cpu) (cpu, verbose);

      /* Always try to print execution time and speed.  */
      if (verbose
	  || PROFILE_FLAGS (data) [PROFILE_INSN_IDX])
	profile_print_speed (cpu);
    }

  /* Finally print non-cpu specific miscellaneous data.  */

  if (misc != NULL)
    (*misc) (sd, verbose);
}

/* Install profiling support in the simulator.  */

SIM_RC
profile_install (SIM_DESC sd)
{
  int i;

  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);
  sim_add_option_table (sd, profile_options);
  for (i = 0; i < MAX_NR_PROCESSORS; ++i)
    memset (CPU_PROFILE_DATA (STATE_CPU (sd, i)), 0,
	    sizeof (* CPU_PROFILE_DATA (STATE_CPU (sd, i))));
#if WITH_PROFILE_PC_P
  sim_module_add_uninstall_fn (sd, profile_pc_uninstall);
  sim_module_add_init_fn (sd, profile_pc_init);
#endif
  sim_module_add_uninstall_fn (sd, profile_uninstall);
  return SIM_RC_OK;
}

static void
profile_uninstall (SIM_DESC sd)
{
  int i,j;

  for (i = 0; i < MAX_NR_PROCESSORS; ++i)
    {
      PROFILE_DATA *data = CPU_PROFILE_DATA (STATE_CPU (sd, i));
      if (PROFILE_FILE (data) != NULL)
	{
	  /* If output from different cpus is going to the same file,
	     avoid closing the file twice.  */
	  for (j = 0; j < i; ++j)
	    if (PROFILE_FILE (CPU_PROFILE_DATA (STATE_CPU (sd, j)))
		== PROFILE_FILE (data))
	      break;
	  if (i == j)
	    fclose (PROFILE_FILE (data));
	}
    }
}
