/* interp.c -- Simulator for Motorola 68HC11
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Written by Stephane Carrez (stcarrez@worldnet.fr)

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
#include "sim-assert.h"
#include "sim-hw.h"
#include "sim-options.h"
#include "hw-tree.h"
#include "hw-device.h"
#include "hw-ports.h"

#ifndef MONITOR_BASE
# define MONITOR_BASE (0x0C000)
# define MONITOR_SIZE (0x04000)
#endif

static void sim_get_info (SIM_DESC sd, char *cmd);


char *interrupt_names[] = {
  "reset",
  "nmi",
  "int",
  NULL
};

#ifndef INLINE
#if defined(__GNUC__) && defined(__OPTIMIZE__)
#define INLINE __inline__
#else
#define INLINE
#endif
#endif

struct sim_info_list
{
  const char *name;
  const char *device;
};

struct sim_info_list dev_list_68hc11[] = {
  {"cpu", "/m68hc11"},
  {"timer", "/m68hc11/m68hc11tim"},
  {"sio", "/m68hc11/m68hc11sio"},
  {"spi", "/m68hc11/m68hc11spi"},
  {"eeprom", "/m68hc11/m68hc11eepr"},
  {0, 0}
};

struct sim_info_list dev_list_68hc12[] = {
  {"cpu", "/m68hc12"},
  {"timer", "/m68hc12/m68hc12tim"},
  {"sio", "/m68hc12/m68hc12sio"},
  {"spi", "/m68hc12/m68hc12spi"},
  {"eeprom", "/m68hc12/m68hc12eepr"},
  {0, 0}
};

/* Cover function of sim_state_free to free the cpu buffers as well.  */

static void
free_state (SIM_DESC sd)
{
  if (STATE_MODULES (sd) != NULL)
    sim_module_uninstall (sd);

  sim_state_free (sd);
}

/* Give some information about the simulator.  */
static void
sim_get_info (SIM_DESC sd, char *cmd)
{
  sim_cpu *cpu;

  cpu = STATE_CPU (sd, 0);
  if (cmd != 0 && (cmd[0] == ' ' || cmd[0] == '-'))
    {
      int i;
      struct hw *hw_dev;
      struct sim_info_list *dev_list;
      const struct bfd_arch_info *arch;

      arch = STATE_ARCHITECTURE (sd);
      cmd++;

      if (arch->arch == bfd_arch_m68hc11)
        dev_list = dev_list_68hc11;
      else
        dev_list = dev_list_68hc12;

      for (i = 0; dev_list[i].name; i++)
	if (strcmp (cmd, dev_list[i].name) == 0)
	  break;

      if (dev_list[i].name == 0)
	{
	  sim_io_eprintf (sd, "Device '%s' not found.\n", cmd);
	  sim_io_eprintf (sd, "Valid devices: cpu timer sio eeprom\n");
	  return;
	}
      hw_dev = sim_hw_parse (sd, dev_list[i].device);
      if (hw_dev == 0)
	{
	  sim_io_eprintf (sd, "Device '%s' not found\n", dev_list[i].device);
	  return;
	}
      hw_ioctl (hw_dev, 23, 0);
      return;
    }

  cpu_info (sd, cpu);
  interrupts_info (sd, &cpu->cpu_interrupts);
}


void
sim_board_reset (SIM_DESC sd)
{
  struct hw *hw_cpu;
  sim_cpu *cpu;
  const struct bfd_arch_info *arch;
  const char *cpu_type;

  cpu = STATE_CPU (sd, 0);
  arch = STATE_ARCHITECTURE (sd);

  /*  hw_cpu = sim_hw_parse (sd, "/"); */
  if (arch->arch == bfd_arch_m68hc11)
    {
      cpu->cpu_type = CPU_M6811;
      cpu_type = "/m68hc11";
    }
  else
    {
      cpu->cpu_type = CPU_M6812;
      cpu_type = "/m68hc12";
    }
  
  hw_cpu = sim_hw_parse (sd, cpu_type);
  if (hw_cpu == 0)
    {
      sim_io_eprintf (sd, "%s cpu not found in device tree.", cpu_type);
      return;
    }

  cpu_reset (cpu);
  hw_port_event (hw_cpu, 3, 0);
  cpu_restart (cpu);
}

int
sim_hw_configure (SIM_DESC sd)
{
  const struct bfd_arch_info *arch;
  struct hw *device_tree;
  int m6811_mode;
  sim_cpu *cpu;
  
  arch = STATE_ARCHITECTURE (sd);
  if (arch == 0)
    return 0;

  cpu = STATE_CPU (sd, 0);
  cpu->cpu_configured_arch = arch;
  device_tree = sim_hw_parse (sd, "/");
  if (arch->arch == bfd_arch_m68hc11)
    {
      cpu->cpu_interpretor = cpu_interp_m6811;
      if (hw_tree_find_property (device_tree, "/m68hc11/reg") == 0)
	{
	  /* Allocate core managed memory */

	  /* the monitor  */
	  sim_do_commandf (sd, "memory region 0x%lx@%d,0x%lx",
			   /* MONITOR_BASE, MONITOR_SIZE */
			   0x8000, M6811_RAM_LEVEL, 0x8000);
	  sim_do_commandf (sd, "memory region 0x000@%d,0x8000",
			   M6811_RAM_LEVEL);
	  sim_hw_parse (sd, "/m68hc11/reg 0x1000 0x03F");
	}

      if (hw_tree_find_property (device_tree, "/m68hc11/m68hc11sio/reg") == 0)
	{
	  sim_hw_parse (sd, "/m68hc11/m68hc11sio/reg 0x2b 0x5");
	  sim_hw_parse (sd, "/m68hc11/m68hc11sio/backend stdio");
	  sim_hw_parse (sd, "/m68hc11 > cpu-reset reset /m68hc11/m68hc11sio");
	}
      if (hw_tree_find_property (device_tree, "/m68hc11/m68hc11tim/reg") == 0)
	{
	  /* M68hc11 Timer configuration. */
	  sim_hw_parse (sd, "/m68hc11/m68hc11tim/reg 0x1b 0x5");
	  sim_hw_parse (sd, "/m68hc11 > cpu-reset reset /m68hc11/m68hc11tim");
	}

      /* Create the SPI device.  */
      if (hw_tree_find_property (device_tree, "/m68hc11/m68hc11spi/reg") == 0)
	{
	  sim_hw_parse (sd, "/m68hc11/m68hc11spi/reg 0x28 0x3");
	  sim_hw_parse (sd, "/m68hc11 > cpu-reset reset /m68hc11/m68hc11spi");
	}
      if (hw_tree_find_property (device_tree, "/m68hc11/nvram/reg") == 0)
	{
	  /* M68hc11 persistent ram configuration. */
	  sim_hw_parse (sd, "/m68hc11/nvram/reg 0x0 256");
	  sim_hw_parse (sd, "/m68hc11/nvram/file m68hc11.ram");
	  sim_hw_parse (sd, "/m68hc11/nvram/mode save-modified");
	  /*sim_hw_parse (sd, "/m68hc11 > cpu-reset reset /m68hc11/pram"); */
	}
      if (hw_tree_find_property (device_tree, "/m68hc11/m68hc11eepr/reg") == 0)
	{
	  sim_hw_parse (sd, "/m68hc11/m68hc11eepr/reg 0xb000 512");
	  sim_hw_parse (sd, "/m68hc11 > cpu-reset reset /m68hc11/m68hc11eepr");
	}
    }
  else
    {
      cpu->cpu_interpretor = cpu_interp_m6812;
      if (hw_tree_find_property (device_tree, "/m68hc12/reg") == 0)
	{
	  /* Allocate core external memory.  */
	  sim_do_commandf (sd, "memory region 0x%lx@%d,0x%lx",
			   0x8000, M6811_RAM_LEVEL, 0x8000);
	  sim_do_commandf (sd, "memory region 0x000@%d,0x8000",
			   M6811_RAM_LEVEL);

	  sim_hw_parse (sd, "/m68hc12/reg 0x0 0x3FF");
	}

      if (!hw_tree_find_property (device_tree, "/m68hc12/m68hc12sio@1/reg"))
	{
	  sim_hw_parse (sd, "/m68hc12/m68hc12sio@1/reg 0xC0 0x8");
	  sim_hw_parse (sd, "/m68hc12/m68hc12sio@1/backend stdio");
	  sim_hw_parse (sd, "/m68hc12 > cpu-reset reset /m68hc12/m68hc12sio@1");
	}
      if (!hw_tree_find_property (device_tree, "/m68hc12/m68hc12sio@2/reg"))
	{
	  sim_hw_parse (sd, "/m68hc12/m68hc12sio@2/reg 0xC8 0x8");
	  sim_hw_parse (sd, "/m68hc12/m68hc12sio@2/backend tcp");
	  sim_hw_parse (sd, "/m68hc12 > cpu-reset reset /m68hc12/m68hc12sio@2");
	}
      if (hw_tree_find_property (device_tree, "/m68hc12/m68hc12tim/reg") == 0)
	{
	  /* M68hc11 Timer configuration. */
	  sim_hw_parse (sd, "/m68hc12/m68hc12tim/reg 0x1b 0x5");
	  sim_hw_parse (sd, "/m68hc12 > cpu-reset reset /m68hc12/m68hc12tim");
	}

      /* Create the SPI device.  */
      if (hw_tree_find_property (device_tree, "/m68hc12/m68hc12spi/reg") == 0)
	{
	  sim_hw_parse (sd, "/m68hc12/m68hc12spi/reg 0x28 0x3");
	  sim_hw_parse (sd, "/m68hc12 > cpu-reset reset /m68hc12/m68hc12spi");
	}
      if (hw_tree_find_property (device_tree, "/m68hc12/nvram/reg") == 0)
	{
	  /* M68hc11 persistent ram configuration. */
	  sim_hw_parse (sd, "/m68hc12/nvram/reg 0x2000 8192");
	  sim_hw_parse (sd, "/m68hc12/nvram/file m68hc12.ram");
	  sim_hw_parse (sd, "/m68hc12/nvram/mode save-modified");
	}
      if (hw_tree_find_property (device_tree, "/m68hc12/m68hc12eepr/reg") == 0)
	{
	  sim_hw_parse (sd, "/m68hc12/m68hc12eepr/reg 0x0800 2048");
	  sim_hw_parse (sd, "/m68hc12 > cpu-reset reset /m68hc12/m68hc12eepr");
	}
    }
  return 0;
}

static int
sim_prepare_for_program (SIM_DESC sd, struct _bfd* abfd)
{
  sim_cpu *cpu;

  cpu = STATE_CPU (sd, 0);

  sim_hw_configure (sd);
  if (abfd != NULL)
    {
      cpu->cpu_elf_start = bfd_get_start_address (abfd);
    }

  /* reset all state information */
  sim_board_reset (sd);

  return SIM_RC_OK;
}

SIM_DESC
sim_open (SIM_OPEN_KIND kind, host_callback *callback,
          struct _bfd *abfd, char **argv)
{
  char **p;
  SIM_DESC sd;
  sim_cpu *cpu;
  struct hw *device_tree;

  sd = sim_state_alloc (kind, callback);
  cpu = STATE_CPU (sd, 0);

  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);

  /* for compatibility */
  current_alignment = NONSTRICT_ALIGNMENT;
  current_target_byte_order = BIG_ENDIAN;

  cpu_initialize (sd, cpu);

  cpu->cpu_use_elf_start = 1;
  if (sim_pre_argv_init (sd, argv[0]) != SIM_RC_OK)
    {
      free_state (sd);
      return 0;
    }

  /* getopt will print the error message so we just have to exit if this fails.
     FIXME: Hmmm...  in the case of gdb we need getopt to call
     print_filtered.  */
  if (sim_parse_args (sd, argv) != SIM_RC_OK)
    {
      /* Uninstall the modules to avoid memory leaks,
         file descriptor leaks, etc.  */
      free_state (sd);
      return 0;
    }

  /* Check for/establish the a reference program image.  */
  if (sim_analyze_program (sd,
			   (STATE_PROG_ARGV (sd) != NULL
			    ? *STATE_PROG_ARGV (sd)
			    : NULL), abfd) != SIM_RC_OK)
    {
      free_state (sd);
      return 0;
    }

  /* Establish any remaining configuration options.  */
  if (sim_config (sd) != SIM_RC_OK)
    {
      free_state (sd);
      return 0;
    }

  if (sim_post_argv_init (sd) != SIM_RC_OK)
    {
      /* Uninstall the modules to avoid memory leaks,
         file descriptor leaks, etc.  */
      free_state (sd);
      return 0;
    }

  sim_hw_configure (sd);

  /* Fudge our descriptor.  */
  return sd;
}


void
sim_close (SIM_DESC sd, int quitting)
{
  /* shut down modules */
  sim_module_uninstall (sd);

  /* Ensure that any resources allocated through the callback
     mechanism are released: */
  sim_io_shutdown (sd);

  /* FIXME - free SD */
  sim_state_free (sd);
  return;
}

void
sim_set_profile (int n)
{
}

void
sim_set_profile_size (int n)
{
}

/* Generic implementation of sim_engine_run that works within the
   sim_engine setjmp/longjmp framework. */

void
sim_engine_run (SIM_DESC sd,
                int next_cpu_nr,	/* ignore */
		int nr_cpus,	/* ignore */
		int siggnal)	/* ignore */
{
  sim_cpu *cpu;

  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);
  cpu = STATE_CPU (sd, 0);
  while (1)
    {
      cpu_single_step (cpu);

      /* process any events */
      if (sim_events_tickn (sd, cpu->cpu_current_cycle))
	{
	  sim_events_process (sd);
	}
    }
}

int
sim_trace (SIM_DESC sd)
{
  sim_resume (sd, 0, 0);
  return 1;
}

void
sim_info (SIM_DESC sd, int verbose)
{
  const char *cpu_type;
  const struct bfd_arch_info *arch;

  /* Nothing to do if there is no verbose flag set.  */
  if (verbose == 0 && STATE_VERBOSE_P (sd) == 0)
    return;

  arch = STATE_ARCHITECTURE (sd);
  if (arch->arch == bfd_arch_m68hc11)
    cpu_type = "68HC11";
  else
    cpu_type = "68HC12";

  sim_io_eprintf (sd, "Simulator info:\n");
  sim_io_eprintf (sd, "  CPU Motorola %s\n", cpu_type);
  sim_get_info (sd, 0);
  sim_module_info (sd, verbose || STATE_VERBOSE_P (sd));
}

SIM_RC
sim_create_inferior (SIM_DESC sd, struct _bfd *abfd,
                     char **argv, char **env)
{
  return sim_prepare_for_program (sd, abfd);
}


void
sim_set_callbacks (host_callback *p)
{
  /*  m6811_callback = p; */
}


int
sim_fetch_register (SIM_DESC sd, int rn, unsigned char *memory, int length)
{
  sim_cpu *cpu;
  uint16 val;

  cpu = STATE_CPU (sd, 0);
  switch (rn)
    {
    case A_REGNUM:
      val = cpu_get_a (cpu);
      break;

    case B_REGNUM:
      val = cpu_get_b (cpu);
      break;

    case D_REGNUM:
      val = cpu_get_d (cpu);
      break;

    case X_REGNUM:
      val = cpu_get_x (cpu);
      break;

    case Y_REGNUM:
      val = cpu_get_y (cpu);
      break;

    case SP_REGNUM:
      val = cpu_get_sp (cpu);
      break;

    case PC_REGNUM:
      val = cpu_get_pc (cpu);
      break;

    case PSW_REGNUM:
      val = cpu_get_ccr (cpu);
      break;

    default:
      val = 0;
      break;
    }
  memory[0] = val >> 8;
  memory[1] = val & 0x0FF;
  return 2;
}

int
sim_store_register (SIM_DESC sd, int rn, unsigned char *memory, int length)
{
  uint16 val;
  sim_cpu *cpu;

  cpu = STATE_CPU (sd, 0);

  val = *memory++;
  if (length == 2)
    val = (val << 8) | *memory;

  switch (rn)
    {
    case D_REGNUM:
      cpu_set_d (cpu, val);
      break;

    case A_REGNUM:
      cpu_set_a (cpu, val);
      break;

    case B_REGNUM:
      cpu_set_b (cpu, val);
      break;

    case X_REGNUM:
      cpu_set_x (cpu, val);
      break;

    case Y_REGNUM:
      cpu_set_y (cpu, val);
      break;

    case SP_REGNUM:
      cpu_set_sp (cpu, val);
      break;

    case PC_REGNUM:
      cpu_set_pc (cpu, val);
      break;

    case PSW_REGNUM:
      cpu_set_ccr (cpu, val);
      break;

    default:
      break;
    }

  return 2;
}

void
sim_size (int s)
{
  ;
}

void
sim_do_command (SIM_DESC sd, char *cmd)
{
  char *mm_cmd = "memory-map";
  char *int_cmd = "interrupt";
  sim_cpu *cpu;

  cpu = STATE_CPU (sd, 0);
  /* Commands available from GDB:   */
  if (sim_args_command (sd, cmd) != SIM_RC_OK)
    {
      if (strncmp (cmd, "info", sizeof ("info") - 1) == 0)
	sim_get_info (sd, &cmd[4]);
      else if (strncmp (cmd, "frame", sizeof ("frame") - 1) == 0)
	cpu_print_frame (sd, STATE_CPU (sd, 0));
      else if (strncmp (cmd, mm_cmd, strlen (mm_cmd) == 0))
	sim_io_eprintf (sd,
			"`memory-map' command replaced by `sim memory'\n");
      else if (strncmp (cmd, int_cmd, strlen (int_cmd)) == 0)
	sim_io_eprintf (sd, "`interrupt' command replaced by `sim watch'\n");
      else
	sim_io_eprintf (sd, "Unknown command `%s'\n", cmd);
    }

  /* If the architecture changed, re-configure.  */
  if (STATE_ARCHITECTURE (sd) != cpu->cpu_configured_arch)
    sim_hw_configure (sd);
}

/* Halt the simulator after just one instruction */

static void
has_stepped (SIM_DESC sd,
	     void *data)
{
  ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);
  sim_engine_halt (sd, NULL, NULL, NULL_CIA, sim_stopped, SIM_SIGTRAP);
}


/* Generic resume - assumes the existance of sim_engine_run */

void
sim_resume (SIM_DESC sd,
	    int step,
	    int siggnal)
{
  sim_engine *engine = STATE_ENGINE (sd);
  jmp_buf buf;
  int jmpval;

  ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);

  /* we only want to be single stepping the simulator once */
  if (engine->stepper != NULL)
    {
      sim_events_deschedule (sd, engine->stepper);
      engine->stepper = NULL;
    }
  sim_module_resume (sd);

  /* run/resume the simulator */
  engine->jmpbuf = &buf;
  jmpval = setjmp (buf);
  if (jmpval == sim_engine_start_jmpval
      || jmpval == sim_engine_restart_jmpval)
    {
      int last_cpu_nr = sim_engine_last_cpu_nr (sd);
      int next_cpu_nr = sim_engine_next_cpu_nr (sd);
      int nr_cpus = sim_engine_nr_cpus (sd);

      sim_events_preprocess (sd, last_cpu_nr >= nr_cpus, next_cpu_nr >= nr_cpus);
      if (next_cpu_nr >= nr_cpus)
	next_cpu_nr = 0;

      /* Only deliver the siggnal ]sic] the first time through - don't
         re-deliver any siggnal during a restart. */
      if (jmpval == sim_engine_restart_jmpval)
	siggnal = 0;

      /* Install the stepping event after having processed some
         pending events.  This is necessary for HC11/HC12 simulator
         because the tick counter is incremented by the number of cycles
         the instruction took.  Some pending ticks to process can still
         be recorded internally by the simulator and sim_events_preprocess
         will handle them.  If the stepping event is inserted before,
         these pending ticks will raise the event and the simulator will
         stop without having executed any instruction.  */
      if (step)
        engine->stepper = sim_events_schedule (sd, 0, has_stepped, sd);

#ifdef SIM_CPU_EXCEPTION_RESUME
      {
	sim_cpu* cpu = STATE_CPU (sd, next_cpu_nr);
	SIM_CPU_EXCEPTION_RESUME(sd, cpu, siggnal);
      }
#endif

      sim_engine_run (sd, next_cpu_nr, nr_cpus, siggnal);
    }
  engine->jmpbuf = NULL;

  sim_module_suspend (sd);
}
