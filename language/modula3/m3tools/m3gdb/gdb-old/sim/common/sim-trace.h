/* Simulator tracing/debugging support.
   Copyright (C) 1997 Free Software Foundation, Inc.
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

/* This file is meant to be included by sim-basics.h.  */

#ifndef SIM_TRACE_H
#define SIM_TRACE_H

/* Standard traceable entities.  */

/* Trace insn execution.  */
#define TRACE_INSN_IDX 0

/* Trace insn decoding.
   ??? This is more of a simulator debugging operation and might best be
   moved to --debug-decode.  */
#define TRACE_DECODE_IDX 1

/* Trace insn extraction.
   ??? This is more of a simulator debugging operation and might best be
   moved to --debug-extract.  */
#define TRACE_EXTRACT_IDX 2

/* Trace insn execution but include line numbers.  */
#define TRACE_LINENUM_IDX 3

/* Trace memory operations.
   The difference between this and TRACE_CORE_IDX is (I think) that this
   is intended to apply to a higher level.  TRACE_CORE_IDX applies to the
   low level core operations.  */
#define TRACE_MEMORY_IDX 4

/* Include model performance data in tracing output.  */
#define TRACE_MODEL_IDX 5

/* Trace ALU operations.  */
#define TRACE_ALU_IDX 6

/* Trace memory core operations.  */
#define TRACE_CORE_IDX 7

/* Trace events.  */
#define TRACE_EVENTS_IDX 8

/* Trace fpu operations.  */
#define TRACE_FPU_IDX 9

/* Trace branching.  */
#define TRACE_BRANCH_IDX 10

/* Add information useful for debugging the simulator to trace output.  */
#define TRACE_DEBUG_IDX 11

/* Simulator specific trace bits begin here.  */
#define TRACE_NEXT_IDX 16

/* Maximum number of traceable entities.  */
#ifndef MAX_TRACE_VALUES
#define MAX_TRACE_VALUES 32
#endif

/* Masks so WITH_TRACE can have symbolic values.
   The case choice here is on purpose.  The lowercase parts are args to
   --with-trace.  */
#define TRACE_insn 1
#define TRACE_decode 2
#define TRACE_extract 4
#define TRACE_linenum 8
#define TRACE_memory 16
#define TRACE_model 32
#define TRACE_alu 64
#define TRACE_core 128
#define TRACE_events 256
#define TRACE_fpu 512
#define TRACE_branch 1024
#define TRACE_debug 2048

/* Preprocessor macros to simplify tests of WITH_TRACE.  */
#define WITH_TRACE_INSN_P	(WITH_TRACE & TRACE_insn)
#define WITH_TRACE_DECODE_P	(WITH_TRACE & TRACE_decode)
#define WITH_TRACE_EXTRACT_P	(WITH_TRACE & TRACE_extract)
#define WITH_TRACE_LINENUM_P	(WITH_TRACE & TRACE_linenum)
#define WITH_TRACE_MEMORY_P	(WITH_TRACE & TRACE_memory)
#define WITH_TRACE_MODEL_P	(WITH_TRACE & TRACE_model)
#define WITH_TRACE_ALU_P	(WITH_TRACE & TRACE_alu)
#define WITH_TRACE_CORE_P	(WITH_TRACE & TRACE_core)
#define WITH_TRACE_EVENTS_P	(WITH_TRACE & TRACE_events)
#define WITH_TRACE_FPU_P	(WITH_TRACE & TRACE_fpu)
#define WITH_TRACE_BRANCH_P	(WITH_TRACE & TRACE_branch)
#define WITH_TRACE_DEBUG_P	(WITH_TRACE & TRACE_debug)

/* Tracing install handler.  */
MODULE_INSTALL_FN trace_install;

/* Struct containing all system and cpu trace data.

   System trace data is stored with the associated module.
   System and cpu tracing must share the same space of bitmasks as they
   are arguments to --with-trace.  One could have --with-trace and
   --with-cpu-trace or some such but that's an over-complication at this point
   in time.  Also, there may be occasions where system and cpu tracing may
   wish to share a name.  */

typedef struct {
  /* Boolean array of specified tracing flags.  */
  /* ??? It's not clear that using an array vs a bit mask is faster.
     Consider the case where one wants to test whether any of several bits
     are set.  */
  char trace_flags[MAX_TRACE_VALUES];
#define TRACE_FLAGS(t) ((t)->trace_flags)

  /* Tracing output goes to this or stderr if NULL.
     We can't store `stderr' here as stderr goes through a callback.  */
  FILE *trace_file;
#define TRACE_FILE(t) ((t)->trace_file)
} TRACE_DATA;

/* System tracing support.  */

#define STATE_TRACE_FLAGS(sd) TRACE_FLAGS (STATE_TRACE_DATA (sd))

/* Return non-zero if tracing of IDX is enabled for non-cpu specific
   components.  The "S" in "STRACE" refers to "System".  */
#define STRACE_P(sd,idx) \
((WITH_TRACE & (1 << (idx))) != 0 \
 && STATE_TRACE_FLAGS (sd)[idx] != 0)

/* Non-zero if --trace-<xxxx> was specified for SD.  */
#define STRACE_DEBUG_P(sd)	STRACE_P (sd, TRACE_DEBUG_IDX)

/* CPU tracing support.  */

#define CPU_TRACE_FLAGS(cpu) TRACE_FLAGS (CPU_TRACE_DATA (cpu))

/* Return non-zero if tracing of IDX is enabled for CPU.  */
#define TRACE_P(cpu,idx) \
((WITH_TRACE & (1 << (idx))) != 0 \
 && CPU_TRACE_FLAGS (cpu)[idx] != 0)

/* Non-zero if --trace-<xxxx> was specified for CPU.  */
#define TRACE_INSN_P(cpu)	TRACE_P (cpu, TRACE_INSN_IDX)
#define TRACE_DECODE_P(cpu)	TRACE_P (cpu, TRACE_DECODE_IDX)
#define TRACE_EXTRACT_P(cpu)	TRACE_P (cpu, TRACE_EXTRACT_IDX)
#define TRACE_LINENUM_P(cpu)	TRACE_P (cpu, TRACE_LINENUM_IDX)
#define TRACE_MEMORY_P(cpu)	TRACE_P (cpu, TRACE_MEMORY_IDX)
#define TRACE_MODEL_P(cpu)	TRACE_P (cpu, TRACE_MODEL_IDX)
#define TRACE_ALU_P(cpu)	TRACE_P (cpu, TRACE_ALU_IDX)
#define TRACE_CORE_P(cpu)	TRACE_P (cpu, TRACE_CORE_IDX)
#define TRACE_EVENTS_P(cpu)	TRACE_P (cpu, TRACE_EVENTS_IDX)
#define TRACE_FPU_P(cpu)	TRACE_P (cpu, TRACE_FPU_IDX)
#define TRACE_BRANCH_P(cpu)	TRACE_P (cpu, TRACE_BRANCH_IDX)
#define TRACE_DEBUG_P(cpu)	TRACE_P (cpu, TRACE_DEBUG_IDX)

extern void trace_one_insn PARAMS ((SIM_DESC sd,
				    sim_cpu * cpu,
				    address_word cia,
				    int print_linenum_p,
				    const char *file_name,
				    int line_nr,
				    const char *unit,
				    const char *fmt,
				    ...))
     __attribute__((format (printf, 8, 9)));

extern void trace_printf PARAMS ((SIM_DESC, sim_cpu *, const char *, ...))
     __attribute__((format (printf, 3, 4)));

extern void trace_vprintf PARAMS ((SIM_DESC, sim_cpu *, const char *, va_list));

/* Debug support.
   This is included here because there isn't enough of it to justify
   a sim-debug.h.  */

/* Return non-zero if debugging of IDX for CPU is enabled.  */
#define DEBUG_P(cpu, idx) \
((WITH_DEBUG & (1 << (idx))) != 0 \
 && CPU_DEBUG_FLAGS (cpu)[idx] != 0)

/* Non-zero if "--debug-insn" specified.  */
#define DEBUG_INSN_P(cpu) DEBUG_P (cpu, DEBUG_INSN_IDX)

extern void debug_printf PARAMS ((sim_cpu *, const char *, ...))
     __attribute__((format (printf, 2, 3)));

#endif /* SIM_TRACE_H */
