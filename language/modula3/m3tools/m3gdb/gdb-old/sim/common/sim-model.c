/* Model support.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.
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
#include "libiberty.h"
#include "sim-options.h"
#include "sim-io.h"
#include "sim-assert.h"

static SIM_RC set_model (SIM_DESC, char *);

static DECLARE_OPTION_HANDLER (model_option_handler);

#define OPTION_MODEL (OPTION_START + 0)

static const OPTION model_options[] = {
  { {"model", required_argument, NULL, OPTION_MODEL},
      '\0', "MODEL", "Specify model to simulate",
      model_option_handler },
  { {NULL, no_argument, NULL, 0}, '\0', NULL, NULL, NULL }
};

static SIM_RC
model_option_handler (sd, opt, arg, is_command)
     SIM_DESC sd;
     int opt;
     char *arg;
     int is_command;
{
  int n;

  switch (opt)
    {
    case OPTION_MODEL :
      if (set_model (sd, arg) == SIM_RC_FAIL)
	{
	  sim_io_eprintf (sd, "unknown model `%s'", arg);
	  return SIM_RC_FAIL;
	}
      break;
    }

  return SIM_RC_OK;
}

SIM_RC
model_install (sd)
     SIM_DESC sd;
{
  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);

  if (WITH_DEFAULT_MODEL != 0)
    {
      if (set_model (sd, WITH_DEFAULT_MODEL) == SIM_RC_FAIL)
	abort ();
    }

  sim_add_option_table (sd, model_options);

  return SIM_RC_OK;
}

/* Set the current model to MODEL_NAME.  */

static SIM_RC
set_model (SIM_DESC sd, char *model_name)
{
  const MODEL *model;
  const MACH *mach;
  sim_cpu *cpu = STATE_CPU (sd, 0);

  for (mach = & machs[0]; MACH_NAME (mach) != NULL; ++mach)
    {
      for (model = MACH_MODELS (mach); MODEL_NAME (model) != NULL; ++model)
	{
	  if (strcmp (MODEL_NAME (model), model_name) == 0)
	    {
	      /* FIXME: Needs to be specified per-cpu.  */
	      CPU_MACH (cpu) = mach;
	      CPU_MODEL (cpu) = model;
	      return SIM_RC_OK;
	    }
	}
    }
  return SIM_RC_FAIL;
}
