/* Architecture, machine, and model support.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
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

/* Nomenclature:
   architecture = one of sparc, mips, sh, etc.
   in the sparc architecture, mach = one of v6, v7, v8, sparclite, etc.
   in the v8 mach, model = one of supersparc, etc.
*/

/* This file is intended to be included by sim-basics.h.  */

#ifndef SIM_MODEL_H
#define SIM_MODEL_H

/* Function unit and instruction timing support.
   ??? This is obviously insufficiently general.
   It's useful but it needs elaborating upon.  */

typedef struct {
  unsigned char name; /* actually a UNIT_TYPE enum */
  unsigned char issue;
  unsigned char done;
  unsigned char unused; /* fill out to power of 2 so indexing is faster */
} UNIT;

#ifndef MAX_UNITS
#define MAX_UNITS 1
#endif

#ifndef MAX_MODELS
#define MAX_MODELS 1
#endif

/* ??? Temporary.  MAX_INSNS should always be defined.  */
#ifndef MAX_INSNS
#define MAX_INSNS 1
#endif

typedef struct {
  UNIT units[MAX_UNITS];
} INSN_TIMING;

extern INSN_TIMING insn_timing[MAX_MODELS][MAX_INSNS];

/* Struct to describe various implementation properties of a cpu.
   When multiple cpu variants are supported, the sizes of some structs
   can vary.  */

typedef struct {
  /* The size of the SIM_CPU struct.  */
  int sim_cpu_size;
#define IMP_PROPS_SIM_CPU_SIZE(cpu_props) ((cpu_props)->sim_cpu_size)
#if WITH_SCACHE
  /* An SCACHE element can vary in size, depending on the selected cpu.  */
  int scache_elm_size;
#define IMP_PROPS_SCACHE_ELM_SIZE(cpu_props) ((cpu_props)->scache_elm_size)
#endif
} IMP_PROPERTIES;

/* A machine variant.  */

typedef struct {
  const char *name;
#define MACH_NAME(m) ((m)->name)
  int word_bitsize;
#define MACH_WORD_BITSIZE(m) ((m)->word_bitsize)
  int addr_bitsize;
#define MACH_ADDR_BITSIZE(m) ((m)->addr_bitsize)

  /* Pointer to null-entry terminated table of models of this mach.  */
  const struct model *models;
#define MACH_MODELS(m) ((m)->models)

  /* Pointer to the implementation properties of this mach.  */
  const IMP_PROPERTIES *imp_props;
#define MACH_IMP_PROPS(m) ((m)->imp_props)
} MACH;

/* A model (implementation) of a machine.  */

typedef struct model {
  const char *name;
#define MODEL_NAME(m) ((m)->name)
  const MACH *mach;
#define MODEL_MACH(m) ((m)->mach)
  /* Pointer to timing table for this model.  */
  const INSN_TIMING *timing;
#define MODEL_TIMING(m) ((m)->timing)
} MODEL;

/* Tables of supported machines.  */
/* ??? In a simulator of multiple architectures, will need multiple copies of
   this.  Have an `archs' array that contains a pointer to the machs array
   for each (which in turn has a pointer to the models array for each).  */
extern const MACH machs[];

/* Model install handler.  */
extern MODULE_INSTALL_FN model_install;

#endif /* SIM_MODEL_H */
