/*  This file is part of the program psim.

    Copyright (C) 1994-1997, Andrew Cagney <cagney@highland.com.au>

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 
    */


#ifndef _SIM_CORE_C_
#define _SIM_CORE_C_

#include "sim-main.h"
#include "sim-assert.h"

/* "core" module install handler.

   This is called via sim_module_install to install the "core" subsystem
   into the simulator.  */

static MODULE_INIT_FN sim_core_init;
static MODULE_UNINSTALL_FN sim_core_uninstall;

EXTERN_SIM_CORE\
(SIM_RC)
sim_core_install (SIM_DESC sd)
{
  SIM_ASSERT (STATE_MAGIC (sd) == SIM_MAGIC_NUMBER);

  /* establish the other handlers */
  sim_module_add_uninstall_fn (sd, sim_core_uninstall);
  sim_module_add_init_fn (sd, sim_core_init);

  /* establish any initial data structures - none */
  return SIM_RC_OK;
}


/* Uninstall the "core" subsystem from the simulator.  */

STATIC_SIM_CORE\
(void)
sim_core_uninstall (SIM_DESC sd)
{
  sim_core *core = STATE_CORE(sd);
  sim_core_maps map;
  /* blow away any mappings */
  for (map = 0; map < nr_sim_core_maps; map++) {
    sim_core_mapping *curr = core->common.map[map].first;
    while (curr != NULL) {
      sim_core_mapping *tbd = curr;
      curr = curr->next;
      if (tbd->free_buffer != NULL) {
	SIM_ASSERT(tbd->buffer != NULL);
	zfree(tbd->free_buffer);
      }
      zfree(tbd);
    }
    core->common.map[map].first = NULL;
  }
}


STATIC_SIM_CORE\
(SIM_RC)
sim_core_init (SIM_DESC sd)
{
  /* Nothing to do */
  return SIM_RC_OK;
}



#ifndef SIM_CORE_SIGNAL
#define SIM_CORE_SIGNAL(SD,CPU,CIA,MAP,NR_BYTES,ADDR,TRANSFER,ERROR) \
sim_core_signal ((SD), (CPU), (CIA), (MAP), (NR_BYTES), (ADDR), (TRANSFER), (ERROR))

STATIC_SIM_CORE\
(void)
sim_core_signal (SIM_DESC sd,
		 sim_cpu *cpu,
		 sim_cia cia,
		 sim_core_maps map,
		 int nr_bytes,
		 address_word addr,
		 transfer_type transfer,
		 sim_core_signals sig)
{
  const char *copy = (transfer == read_transfer ? "read" : "write");
  address_word ip = CIA_ADDR (cia);
  switch (sig)
    {
    case sim_core_unmapped_signal:
      sim_io_eprintf (sd, "core: %d byte %s to unmapped address 0x%lx at 0x%lx\n",
		      nr_bytes, copy, (unsigned long) addr, (unsigned long) ip);
      sim_engine_halt (sd, cpu, NULL, cia, sim_stopped, SIM_SIGSEGV);
      break;
    case sim_core_unaligned_signal:
      sim_io_eprintf (sd, "core: %d byte misaligned %s to address 0x%lx at 0x%lx\n",
		      nr_bytes, copy, (unsigned long) addr, (unsigned long) ip);
      sim_engine_halt (sd, cpu, NULL, cia, sim_stopped, SIM_SIGBUS);
      break;
    default:
      sim_engine_abort (sd, cpu, cia,
			"sim_core_signal - internal error - bad switch");
    }
}
#endif


STATIC_INLINE_SIM_CORE\
(const char *)
sim_core_map_to_str (sim_core_maps map)
{
  switch (map)
    {
    case sim_core_read_map: return "read";
    case sim_core_write_map: return "write";
    case sim_core_execute_map: return "exec";
    default: return "(invalid-map)";
    }
}


STATIC_SIM_CORE\
(sim_core_mapping *)
new_sim_core_mapping (SIM_DESC sd,
		      int level,
		      int space,
		      address_word addr,
		      address_word nr_bytes,
		      unsigned modulo,
		      device *device,
		      void *buffer,
		      void *free_buffer)
{
  sim_core_mapping *new_mapping = ZALLOC(sim_core_mapping);
  /* common */
  new_mapping->level = level;
  new_mapping->space = space;
  new_mapping->base = addr;
  new_mapping->nr_bytes = nr_bytes;
  new_mapping->bound = addr + (nr_bytes - 1);
  if (modulo == 0)
    new_mapping->mask = (unsigned) 0 - 1;
  else
    new_mapping->mask = modulo - 1;
  new_mapping->buffer = buffer;
  new_mapping->free_buffer = free_buffer;
  new_mapping->device = device;
  return new_mapping;
}


STATIC_SIM_CORE\
(void)
sim_core_map_attach (SIM_DESC sd,
		     sim_core_map *access_map,
		     int level,
		     int space,
		     address_word addr,
		     address_word nr_bytes,
		     unsigned modulo,
		     device *client, /*callback/default*/
		     void *buffer, /*raw_memory*/
		     void *free_buffer) /*raw_memory*/
{
  /* find the insertion point for this additional mapping and then
     insert */
  sim_core_mapping *next_mapping;
  sim_core_mapping **last_mapping;

  SIM_ASSERT ((client == NULL) != (buffer == NULL));
  SIM_ASSERT ((client == NULL) >= (free_buffer != NULL));

  /* actually do occasionally get a zero size map */
  if (nr_bytes == 0)
    {
#if (WITH_DEVICES)
      device_error(client, "called on sim_core_map_attach with size zero");
#else
      sim_io_error (sd, "called on sim_core_map_attach with size zero");
#endif
    }

  /* find the insertion point (between last/next) */
  next_mapping = access_map->first;
  last_mapping = &access_map->first;
  while(next_mapping != NULL
	&& (next_mapping->level < level
	    || (next_mapping->level == level
		&& next_mapping->bound < addr)))
    {
      /* provided levels are the same */
      /* assert: next_mapping->base > all bases before next_mapping */
      /* assert: next_mapping->bound >= all bounds before next_mapping */
      last_mapping = &next_mapping->next;
      next_mapping = next_mapping->next;
    }
  
  /* check insertion point correct */
  SIM_ASSERT (next_mapping == NULL || next_mapping->level >= level);
  if (next_mapping != NULL && next_mapping->level == level
      && next_mapping->base < (addr + (nr_bytes - 1)))
    {
#if (WITH_DEVICES)
      device_error (client, "memory map %d:0x%lx..0x%lx (%ld bytes) overlaps %d:0x%lx..0x%lx (%ld bytes)",
		    space,
		    (long) addr,
		    (long) nr_bytes,
		    (long) (addr + (nr_bytes - 1)),
		    next_mapping->space,
		    (long) next_mapping->base,
		    (long) next_mapping->bound,
		    (long) next_mapping->nr_bytes);
#else
      sim_io_error (sd, "memory map %d:0x%lx..0x%lx (%ld bytes) overlaps %d:0x%lx..0x%lx (%ld bytes)",
		    space,
		    (long) addr,
		    (long) nr_bytes,
		    (long) (addr + (nr_bytes - 1)),
		    next_mapping->space,
		    (long) next_mapping->base,
		    (long) next_mapping->bound,
		    (long) next_mapping->nr_bytes);
#endif
  }

  /* create/insert the new mapping */
  *last_mapping = new_sim_core_mapping(sd,
				       level,
				       space, addr, nr_bytes, modulo,
				       client, buffer, free_buffer);
  (*last_mapping)->next = next_mapping;
}


EXTERN_SIM_CORE\
(void)
sim_core_attach (SIM_DESC sd,
		 sim_cpu *cpu,
		 int level,
		 access_type access,
		 int space,
		 address_word addr,
		 address_word nr_bytes,
		 unsigned modulo,
		 device *client,
		 void *optional_buffer)
{
  sim_core *memory = STATE_CORE(sd);
  sim_core_maps map;
  void *buffer;
  void *free_buffer;

  /* check for for attempt to use unimplemented per-processor core map */
  if (cpu != NULL)
    sim_io_error (sd, "sim_core_map_attach - processor specific memory map not yet supported");

  if ((access & access_read_write_exec) == 0
      || (access & ~access_read_write_exec) != 0)
    {
#if (WITH_DEVICES)
      device_error(client, "invalid access for core attach");
#else
      sim_io_error (sd, "invalid access for core attach");
#endif
    }

  /* verify modulo memory */
  if (!WITH_MODULO_MEMORY && modulo != 0)
    {
#if (WITH_DEVICES)
      device_error (client, "sim_core_attach - internal error - modulo memory disabled");
#else
      sim_io_error (sd, "sim_core_attach - internal error - modulo memory disabled");
#endif
    }
  if (client != NULL && modulo != 0)
    {
#if (WITH_DEVICES)
      device_error (client, "sim_core_attach - internal error - modulo and callback memory conflict");
#else
      sim_io_error (sd, "sim_core_attach - internal error - modulo and callback memory conflict");
#endif
    }
  if (modulo != 0)
    {
      unsigned mask = modulo - 1;
      /* any zero bits */
      while (mask >= sizeof (unsigned64)) /* minimum modulo */
	{
	  if ((mask & 1) == 0)
	    mask = 0;
	  else
	    mask >>= 1;
	}
      if (mask != sizeof (unsigned64) - 1)
	{
#if (WITH_DEVICES)
	  device_error (client, "sim_core_attach - internal error - modulo %lx not power of two", (long) modulo);
#else
	  sim_io_error (sd, "sim_core_attach - internal error - modulo %lx not power of two", (long) modulo);
#endif
	}
    }

  /* verify consistency between device and buffer */
  if (client != NULL && optional_buffer != NULL)
    {
#if (WITH_DEVICES)
      device_error (client, "sim_core_attach - internal error - conflicting buffer and attach arguments");
#else
      sim_io_error (sd, "sim_core_attach - internal error - conflicting buffer and attach arguments");
#endif
    }
  if (client == NULL)
    {
      if (optional_buffer == NULL)
	{
	  int padding = (addr % sizeof (unsigned64));
	  free_buffer = zalloc ((modulo == 0 ? nr_bytes : modulo) + padding);
	  buffer = (char*) free_buffer + padding;
	}
      else
	{
	  buffer = optional_buffer;
	  free_buffer = NULL;
	}
    }
  else
    {
      /* a device */
      buffer = NULL;
      free_buffer = NULL;
    }

  /* attach the region to all applicable access maps */
  for (map = 0; 
       map < nr_sim_core_maps;
       map++)
    {
      switch (map)
	{
	case sim_core_read_map:
	  if (access & access_read)
	    sim_core_map_attach (sd, &memory->common.map[map],
				 level, space, addr, nr_bytes, modulo,
				 client, buffer, free_buffer);
	  free_buffer = NULL;
	  break;
	case sim_core_write_map:
	  if (access & access_write)
	    sim_core_map_attach (sd, &memory->common.map[map],
				 level, space, addr, nr_bytes, modulo,
				 client, buffer, free_buffer);
	  free_buffer = NULL;
	  break;
	case sim_core_execute_map:
	  if (access & access_exec)
	    sim_core_map_attach (sd, &memory->common.map[map],
				 level, space, addr, nr_bytes, modulo,
				 client, buffer, free_buffer);
	  free_buffer = NULL;
	  break;
	case nr_sim_core_maps:
	  sim_io_error (sd, "sim_core_attach - internal error - bad switch");
	  break;
	}
    }
  
  /* Just copy this map to each of the processor specific data structures.
     FIXME - later this will be replaced by true processor specific
     maps. */
  {
    int i;
    for (i = 0; i < MAX_NR_PROCESSORS; i++)
      {
	CPU_CORE (STATE_CPU (sd, i))->common = STATE_CORE (sd)->common;
      }
  }
}


/* Remove any memory reference related to this address */
STATIC_INLINE_SIM_CORE\
(void)
sim_core_map_detach (SIM_DESC sd,
		     sim_core_map *access_map,
		     int level,
		     int space,
		     address_word addr)
{
  sim_core_mapping **entry;
  for (entry = &access_map->first;
       (*entry) != NULL;
       entry = &(*entry)->next)
    {
      if ((*entry)->base == addr
	  && (*entry)->level == level
	  && (*entry)->space == space)
	{
	  sim_core_mapping *dead = (*entry);
	  (*entry) = dead->next;
	  if (dead->free_buffer != NULL)
	    zfree (dead->free_buffer);
	  zfree (dead);
	  return;
	}
    }
}

EXTERN_SIM_CORE\
(void)
sim_core_detach (SIM_DESC sd,
		 sim_cpu *cpu,
		 int level,
		 int address_space,
		 address_word addr)
{
  sim_core *memory = STATE_CORE (sd);
  sim_core_maps map;
  for (map = 0; map < nr_sim_core_maps; map++)
    {
      sim_core_map_detach (sd, &memory->common.map[map],
			   level, address_space, addr);
    }
  /* Just copy this update to each of the processor specific data
     structures.  FIXME - later this will be replaced by true
     processor specific maps. */
  {
    int i;
    for (i = 0; i < MAX_NR_PROCESSORS; i++)
      {
	CPU_CORE (STATE_CPU (sd, i))->common = STATE_CORE (sd)->common;
      }
  }
}


STATIC_INLINE_SIM_CORE\
(sim_core_mapping *)
sim_core_find_mapping(sim_core_common *core,
		      sim_core_maps map,
		      address_word addr,
		      unsigned nr_bytes,
		      transfer_type transfer,
		      int abort, /*either 0 or 1 - hint to inline/-O */
		      sim_cpu *cpu, /* abort => cpu != NULL */
		      sim_cia cia)
{
  sim_core_mapping *mapping = core->map[map].first;
  ASSERT ((addr & (nr_bytes - 1)) == 0); /* must be aligned */
  ASSERT ((addr + (nr_bytes - 1)) >= addr); /* must not wrap */
  ASSERT (!abort || cpu != NULL); /* abort needs a non null CPU */
  while (mapping != NULL)
    {
      if (addr >= mapping->base
	  && (addr + (nr_bytes - 1)) <= mapping->bound)
	return mapping;
      mapping = mapping->next;
    }
  if (abort)
    {
      SIM_CORE_SIGNAL (CPU_STATE (cpu), cpu, cia, map, nr_bytes, addr, transfer,
		       sim_core_unmapped_signal);
    }
  return NULL;
}


STATIC_INLINE_SIM_CORE\
(void *)
sim_core_translate (sim_core_mapping *mapping,
		    address_word addr)
{
  if (WITH_MODULO_MEMORY)
    return (void *)((unsigned8 *) mapping->buffer
		    + ((addr - mapping->base) & mapping->mask));
  else
    return (void *)((unsigned8 *) mapping->buffer
		    + addr - mapping->base);
}


EXTERN_SIM_CORE\
(unsigned)
sim_core_read_buffer (SIM_DESC sd,
		      sim_cpu *cpu,
		      sim_core_maps map,
		      void *buffer,
		      address_word addr,
		      unsigned len)
{
  sim_core_common *core = (cpu == NULL ? &STATE_CORE (sd)->common : &CPU_CORE (cpu)->common);
  unsigned count = 0;
  while (count < len) {
    unsigned_word raddr = addr + count;
    sim_core_mapping *mapping =
      sim_core_find_mapping(core, map,
			    raddr, /*nr-bytes*/1,
			    read_transfer,
			    0 /*dont-abort*/, NULL, NULL_CIA);
    if (mapping == NULL)
      break;
#if (WITH_DEVICES)
    if (mapping->device != NULL) {
      int nr_bytes = len - count;
      if (raddr + nr_bytes - 1> mapping->bound)
	nr_bytes = mapping->bound - raddr + 1;
      if (device_io_read_buffer(mapping->device,
				(unsigned_1*)buffer + count,
				mapping->space,
				raddr,
				nr_bytes) != nr_bytes)
	break;
      count += nr_bytes;
    }
    else
#endif
      {
	((unsigned_1*)buffer)[count] =
	  *(unsigned_1*)sim_core_translate(mapping, raddr);
	count += 1;
      }
  }
  return count;
}


EXTERN_SIM_CORE\
(unsigned)
sim_core_write_buffer (SIM_DESC sd,
		       sim_cpu *cpu,
		       sim_core_maps map,
		       const void *buffer,
		       address_word addr,
		       unsigned len)
{
  sim_core_common *core = (cpu == NULL ? &STATE_CORE (sd)->common : &CPU_CORE (cpu)->common);
  unsigned count = 0;
  while (count < len) {
    unsigned_word raddr = addr + count;
    sim_core_mapping *mapping =
      sim_core_find_mapping(core, map,
			    raddr, /*nr-bytes*/1,
			    write_transfer,
			    0 /*dont-abort*/, NULL, NULL_CIA);
    if (mapping == NULL)
      break;
#if (WITH_DEVICES)
    if (WITH_CALLBACK_MEMORY
	&& mapping->device != NULL) {
      int nr_bytes = len - count;
      if (raddr + nr_bytes - 1 > mapping->bound)
	nr_bytes = mapping->bound - raddr + 1;
      if (device_io_write_buffer(mapping->device,
				 (unsigned_1*)buffer + count,
				 mapping->space,
				 raddr,
				 nr_bytes) != nr_bytes)
	break;
      count += nr_bytes;
    }
    else
#endif
      {
	*(unsigned_1*)sim_core_translate(mapping, raddr) =
	  ((unsigned_1*)buffer)[count];
	count += 1;
      }
  }
  return count;
}


EXTERN_SIM_CORE\
(void)
sim_core_set_xor (SIM_DESC sd,
		  sim_cpu *cpu,
		  int is_xor)
{
  /* set up the XOR map if required. */
  if (WITH_XOR_ENDIAN) {
    {
      sim_core *core = STATE_CORE (sd);
      sim_cpu_core *cpu_core = (cpu != NULL ? CPU_CORE (cpu) : NULL);
      if (cpu_core != NULL)
	{
	  int i = 1;
	  unsigned mask;
	  if (is_xor)
	    mask = WITH_XOR_ENDIAN - 1;
	  else
	    mask = 0;
	  while (i - 1 < WITH_XOR_ENDIAN)
	    {
	      cpu_core->xor[i-1] = mask;
	      mask = (mask << 1) & (WITH_XOR_ENDIAN - 1);
	      i = (i << 1);
	    }
	}
      else
	{
	  if (is_xor)
	    core->byte_xor = WITH_XOR_ENDIAN - 1;
	  else
	    core->byte_xor = 0;
	}	  
    }
  }
  else {
    if (is_xor)
      sim_engine_abort (sd, cpu, NULL_CIA,
			"Attempted to enable xor-endian mode when permenantly disabled.");
  }
}

STATIC_INLINE_SIM_CORE\
(void)
reverse_n (unsigned_1 *dest,
	   const unsigned_1 *src,
	   int nr_bytes)
{
  int i;
  for (i = 0; i < nr_bytes; i++)
    {
      dest [nr_bytes - i - 1] = src [i];
    }
}


EXTERN_SIM_CORE\
(unsigned)
sim_core_xor_read_buffer (SIM_DESC sd,
			  sim_cpu *cpu,
			  sim_core_maps map,
			  void *buffer,
			  address_word addr,
			  unsigned nr_bytes)
{
  address_word byte_xor = (cpu == NULL ? STATE_CORE (sd)->byte_xor : CPU_CORE (cpu)->xor[0]);
  if (!WITH_XOR_ENDIAN || !byte_xor)
    return sim_core_read_buffer (sd, cpu, map, buffer, addr, nr_bytes);
  else
    /* only break up transfers when xor-endian is both selected and enabled */
    {
      unsigned_1 x[WITH_XOR_ENDIAN + 1]; /* +1 to avoid zero-sized array */
      unsigned nr_transfered = 0;
      address_word start = addr;
      unsigned nr_this_transfer = (WITH_XOR_ENDIAN - (addr & ~(WITH_XOR_ENDIAN - 1)));
      address_word stop;
      /* initial and intermediate transfers are broken when they cross
         an XOR endian boundary */
      while (nr_transfered + nr_this_transfer < nr_bytes)
	/* initial/intermediate transfers */
	{
	  /* since xor-endian is enabled stop^xor defines the start
             address of the transfer */
	  stop = start + nr_this_transfer - 1;
	  SIM_ASSERT (start <= stop);
	  SIM_ASSERT ((stop ^ byte_xor) <= (start ^ byte_xor));
	  if (sim_core_read_buffer (sd, cpu, map, x, stop ^ byte_xor, nr_this_transfer)
	      != nr_this_transfer)
	    return nr_transfered;
	  reverse_n (&((unsigned_1*)buffer)[nr_transfered], x, nr_this_transfer);
	  nr_transfered += nr_this_transfer;
	  nr_this_transfer = WITH_XOR_ENDIAN;
	  start = stop + 1;
	}
      /* final transfer */
      nr_this_transfer = nr_bytes - nr_transfered;
      stop = start + nr_this_transfer - 1;
      SIM_ASSERT (stop == (addr + nr_bytes - 1));
      if (sim_core_read_buffer (sd, cpu, map, x, stop ^ byte_xor, nr_this_transfer)
	  != nr_this_transfer)
	return nr_transfered;
      reverse_n (&((unsigned_1*)buffer)[nr_transfered], x, nr_this_transfer);
      return nr_bytes;
    }
}
  
  
EXTERN_SIM_CORE\
(unsigned)
sim_core_xor_write_buffer (SIM_DESC sd,
			   sim_cpu *cpu,
			   sim_core_maps map,
			   const void *buffer,
			   address_word addr,
			   unsigned nr_bytes)
{
  address_word byte_xor = (cpu == NULL ? STATE_CORE (sd)->byte_xor : CPU_CORE (cpu)->xor[0]);
  if (!WITH_XOR_ENDIAN || !byte_xor)
    return sim_core_write_buffer (sd, cpu, map, buffer, addr, nr_bytes);
  else
    /* only break up transfers when xor-endian is both selected and enabled */
    {
      unsigned_1 x[WITH_XOR_ENDIAN + 1]; /* +1 to avoid zero sized array */
      unsigned nr_transfered = 0;
      address_word start = addr;
      unsigned nr_this_transfer = (WITH_XOR_ENDIAN - (addr & ~(WITH_XOR_ENDIAN - 1)));
      address_word stop;
      /* initial and intermediate transfers are broken when they cross
         an XOR endian boundary */
      while (nr_transfered + nr_this_transfer < nr_bytes)
	/* initial/intermediate transfers */
	{
	  /* since xor-endian is enabled stop^xor defines the start
             address of the transfer */
	  stop = start + nr_this_transfer - 1;
	  SIM_ASSERT (start <= stop);
	  SIM_ASSERT ((stop ^ byte_xor) <= (start ^ byte_xor));
	  reverse_n (x, &((unsigned_1*)buffer)[nr_transfered], nr_this_transfer);
	  if (sim_core_read_buffer (sd, cpu, map, x, stop ^ byte_xor, nr_this_transfer)
	      != nr_this_transfer)
	    return nr_transfered;
	  nr_transfered += nr_this_transfer;
	  nr_this_transfer = WITH_XOR_ENDIAN;
	  start = stop + 1;
	}
      /* final transfer */
      nr_this_transfer = nr_bytes - nr_transfered;
      stop = start + nr_this_transfer - 1;
      SIM_ASSERT (stop == (addr + nr_bytes - 1));
      reverse_n (x, &((unsigned_1*)buffer)[nr_transfered], nr_this_transfer);
      if (sim_core_read_buffer (sd, cpu, map, x, stop ^ byte_xor, nr_this_transfer)
	  != nr_this_transfer)
	return nr_transfered;
      return nr_bytes;
    }
}



/* define the read/write 1/2/4/8/16/word functions */

#define N 16
#include "sim-n-core.h"

#define N 8
#include "sim-n-core.h"

#define N 7
#define M 8
#include "sim-n-core.h"

#define N 6
#define M 8
#include "sim-n-core.h"

#define N 5
#define M 8
#include "sim-n-core.h"

#define N 4
#include "sim-n-core.h"

#define N 3
#define M 4
#include "sim-n-core.h"

#define N 2
#include "sim-n-core.h"

#define N 1
#include "sim-n-core.h"

#endif
