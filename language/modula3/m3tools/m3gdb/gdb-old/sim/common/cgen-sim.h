/* Simulator header for Cpu tools GENerated simulators.
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

#ifndef CGEN_SIM_H
#define CGEN_SIM_H

#include "sim-xcat.h"

#define PC CPU (h_pc)

/* Instruction field support macros.  */

#define EXTRACT_SIGNED(val, total, start, length) \
(((((val) >> ((total) - ((start) + (length)))) & ((1 << (length)) - 1)) \
  ^ (1 << ((length) - 1))) \
 - (1 << ((length) - 1)))

#define EXTRACT_UNSIGNED(val, total, start, length) \
(((val) >> ((total) - ((start) + (length)))) & ((1 << (length)) - 1))

/* Compute number of longs required to hold N bits.  */
#define HOST_LONGS_FOR_BITS(n) \
  (((n) + sizeof (long) * 8 - 1) / sizeof (long) * 8)

/* Execution support.  */

/* Forward decls.  Defined in the machine generated arch.h and cpu.h files.  */
typedef struct argbuf ARGBUF;
typedef struct scache SCACHE;
typedef struct parallel_exec PARALLEL_EXEC;

/* Types of the machine generated extract and semantic fns.  */
typedef void (EXTRACT_FN) (SIM_CPU *, PCADDR, insn_t, ARGBUF *);
typedef void (READ_FN) (SIM_CPU *, PCADDR, insn_t, PARALLEL_EXEC *);
/*typedef CIA (SEMANTIC_FN) (SEM_ARG);*/
typedef PCADDR (SEMANTIC_FN) (SIM_CPU *, ARGBUF *);
#if 0 /* wip */
typedef void (EXTRACT_CACHE_FN) (SIM_CPU *, PCADDR, insn_t, ARGBUF *);
#endif
typedef PCADDR (SEMANTIC_CACHE_FN) (SIM_CPU *, SCACHE *);

typedef struct {
  /* Using cgen_insn_type requires <cpu>-opc.h.  */
  int /*enum cgen_insn_type*/ insn_type;
  const struct cgen_insn *opcode;
  EXTRACT_FN *extract;
#ifdef HAVE_PARALLEL_EXEC
#ifdef USE_READ_SWITCH
#ifdef __GNUC__
  void *read;
#else
  int read;
#endif
#else
  READ_FN *read;
#endif
#endif
  SEMANTIC_FN *semantic;
#if 0 /* wip */
  EXTRACT_CACHE_FN *extract_fast;
#endif
  SEMANTIC_CACHE_FN *semantic_fast;
#if WITH_SEM_SWITCH_FULL && defined (__GNUC__)
  /* Set at runtime.  */
  void *sem_full_lab;
#endif
#if WITH_SEM_SWITCH_FAST && defined (__GNUC__)
  /* Set at runtime.  */
  void *semantic_lab; /* FIXME: Rename to sem_fast_lab.  */
#endif
} DECODE;

/* Execution support.

   Semantic functions come in two versions.
   One that uses the cache, and one that doesn't.
   ??? The one that doesn't may eventually be thrown away or replaced with
   something else.  */

#ifdef SCACHE_P

/* instruction address */
typedef PCADDR IADDR;
/* current instruction address */
typedef PCADDR CIA;
/* argument to semantic functions */
typedef SCACHE *SEM_ARG;

#else /* ! SCACHE_P */

/* instruction address */
typedef PCADDR IADDR;
/* current instruction address */
typedef PCADDR CIA;
/* argument to semantic functions */
typedef ARGBUF *SEM_ARG;

#endif /* ! SCACHE_P */

/* Scache data for each cpu.  */

typedef struct cpu_scache {
  /* Simulator cache size.  */
  int size;
#define CPU_SCACHE_SIZE(cpu) ((cpu)->cgen_cpu.scache.size)
  /* Cache.  */
  SCACHE *cache;
#define CPU_SCACHE_CACHE(cpu) ((cpu)->cgen_cpu.scache.cache)
#if 0 /* FIXME: wip */
  /* Free list.  */
  SCACHE *free;
#define CPU_SCACHE_FREE(cpu) ((cpu)->cgen_cpu.scache.free)
  /* Hash table.  */
  SCACHE **hash_table;
#define CPU_SCACHE_HASH_TABLE(cpu) ((cpu)->cgen_cpu.scache.hash_table)
#endif

#if WITH_PROFILE_SCACHE_P
  /* Cache hits, misses.  */
  unsigned long hits, misses;
#define CPU_SCACHE_HITS(cpu) ((cpu)->cgen_cpu.scache.hits)
#define CPU_SCACHE_MISSES(cpu) ((cpu)->cgen_cpu.scache.misses)
#endif
} CPU_SCACHE;

/* Default number of cached blocks.  */
#ifdef CONFIG_SIM_CACHE_SIZE
#define SCACHE_DEFAULT_CACHE_SIZE CONFIG_SIM_CACHE_SIZE
#else
#define SCACHE_DEFAULT_CACHE_SIZE 1024
#endif

/* Hash a PC value.  */
/* FIXME: cpu specific */
#define SCACHE_HASH_PC(state, pc) \
(((pc) >> 1) & (STATE_SCACHE_SIZE (sd) - 1))

/* Non-zero if cache is in use.  */
#define USING_SCACHE_P(sd) (STATE_SCACHE_SIZE (sd) > 0)

/* Install the simulator cache into the simulator.  */
MODULE_INSTALL_FN scache_install;

/* Flush all cpu's caches.  */
void scache_flush (SIM_DESC);

/* Scache profiling support.  */

/* Print summary scache usage information.  */
void scache_print_profile (SIM_CPU *cpu, int verbose);

#if WITH_PROFILE_SCACHE_P
#define PROFILE_COUNT_SCACHE_HIT(cpu) \
do { \
  if (CPU_PROFILE_FLAGS (cpu) [PROFILE_SCACHE_IDX]) \
    ++ CPU_SCACHE_HITS (cpu); \
} while (0)
#define PROFILE_COUNT_SCACHE_MISS(cpu) \
do { \
  if (CPU_PROFILE_FLAGS (cpu) [PROFILE_SCACHE_IDX]) \
    ++ CPU_SCACHE_MISSES (cpu); \
} while (0)
#else
#define PROFILE_COUNT_SCACHE_HIT(cpu)
#define PROFILE_COUNT_SCACHE_MISS(cpu)
#endif

/* Engine support.  */

/* Values to denote parallel/sequential execution.  */
#define EXEC_SEQUENCE 0
#define EXEC_PARALLEL 1

#ifdef SCACHE_P

#define CIA_ADDR(cia) (cia)

/* These are used so that we can compile two copies of the semantic code,
   one with scache support and one without.  */
#define EX_FN_NAME(cpu,fn) XCONCAT3 (cpu,_exc_,fn)
#define SEM_FN_NAME(cpu,fn) XCONCAT3 (cpu,_semc_,fn)

/* extract.c support */
/* scache_unset is a cache entry that is never used.
   It's raison d'etre is so BRANCH_VIA_CACHE doesn't have to test for
   newval.cache == NULL.  */
extern struct scache scache_unset;
#define RECORD_IADDR(fld, val) \
do { (fld) = (val); } while (0)

/* semantics.c support */
#define SEM_ARGBUF(sem_arg) (&(sem_arg)->argbuf)
#define SEM_NEXT_PC(sc) ((sc)->next)
#define SEM_BRANCH_VIA_CACHE(sc, newval) (newval)
#define SEM_BRANCH_VIA_ADDR(sc, newval) (newval)
/* Return address a branch insn will branch to.
   This is only used during tracing.  */
#define SEM_NEW_PC_ADDR(new_pc) (new_pc)

#else /* ! SCACHE_P */

#define CIA_ADDR(cia) (cia)

/* These are used so that we can compile two copies of the semantic code,
   one with scache support and one without.  */
#define EX_FN_NAME(cpu,fn) XCONCAT3 (cpu,_ex_,fn)
#define SEM_FN_NAME(cpu,fn) XCONCAT3 (cpu,_sem_,fn)

/* extract.c support */
#define RECORD_IADDR(fld, val) \
do { (fld) = (val); } while (0)

/* semantics.c support */
#define SEM_ARGBUF(sem_arg) (sem_arg)
#define SEM_NEXT_PC(abuf) (abuf->addr + abuf->length)
#define SEM_BRANCH_VIA_CACHE(abuf, newval) (newval)
#define SEM_BRANCH_VIA_ADDR(abuf, newval) (newval)
#define SEM_NEW_PC_ADDR(new_pc) (new_pc)

#endif /* ! SCACHE_P */

/* GNU C's "computed goto" facility is used to speed things up where
   possible.  These macros provide a portable way to use them.
   Nesting of these switch statements is done by providing an extra argument
   that distinguishes them.  `N' can be a number or symbol.
   Variable `labels_##N' must be initialized with the labels of each case.  */
#ifdef __GNUC__
#define SWITCH(N, X) goto *X;
#define CASE(N, X) case_##N##_##X
#define BREAK(N) goto end_switch_##N
#define DEFAULT(N) default_##N
#define ENDSWITCH(N) end_switch_##N:
#else
#define SWITCH(N, X) switch (X)
#define CASE(N, X) case X /* FIXME: old sem-switch had (@arch@_,X) here */
#define BREAK(N) break
#define DEFAULT(N) default
#define ENDSWITCH(N)
#endif

/* Engine control (FIXME).  */
int engine_stop (SIM_DESC);
void engine_run (SIM_DESC, int, int);
/*void engine_resume (SIM_DESC, int, int);*/

/* Simulator state.  */

/* Records simulator descriptor so utilities like @cpu@_dump_regs can be
   called from gdb.  */
extern SIM_DESC current_state;

/* Simulator state.  */

/* CGEN_STATE contains additional state information not present in
   sim_state_base.  */

typedef struct cgen_state {
  /* FIXME: Moved to sim_state_base.  */
  /* argv, env */
  char **argv;
#define STATE_ARGV(s) ((s)->cgen_state.argv)
  /* FIXME: Move to sim_state_base.  */
  char **envp;
#define STATE_ENVP(s) ((s)->cgen_state.envp)

  /* Non-zero if no tracing or profiling is selected.  */
  int run_fast_p;
#define STATE_RUN_FAST_P(sd) ((sd)->cgen_state.run_fast_p)
} CGEN_STATE;

/* Additional non-machine generated per-cpu data to go in SIM_CPU.
   The member's name must be `cgen_cpu'.  */

typedef struct {
  /* Simulator's execution cache.  */
#if WITH_SCACHE
  CPU_SCACHE scache;
#endif /* WITH_SCACHE */

  /* Allow slop in size calcs for case where multiple cpu types are supported
     and space for the specified cpu is malloc'd at run time.  */
  double slop;
} CGEN_CPU;

/* Various utilities.  */

/* Called after sim_post_argv_init to do any cgen initialization.  */
void cgen_init (SIM_DESC);

void
sim_disassemble_insn (SIM_CPU *, const struct cgen_insn *,
		      const struct argbuf *, PCADDR, char *);

#endif /* CGEN_SIM_H */
