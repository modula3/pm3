/* Simulator tracing support for Cpu tools GENerated simulators.
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

#ifndef CGEN_TRACE_H
#define CGEN_TRACE_H

void trace_insn_init (SIM_CPU *);
void trace_insn_fini (SIM_CPU *);
void trace_insn (SIM_CPU *, const struct cgen_insn *,
		 const struct argbuf *, PCADDR);
void trace_extract (SIM_CPU *, PCADDR, char *, ...);
void trace_result (SIM_CPU *, char *, int, ...);
void cgen_trace_printf (SIM_CPU *, char *fmt, ...);

/* Tracing is not currently enabled, but keep watching for it to be
   enabled.  */
#define TRACE_PENDING_IDX 7

/* Trace instruction results.  */
#define TRACE_RESULT_IDX (TRACE_INSN_IDX)

/* FIXME: Later change PC to pointer to CPU state struct.  */
#define TRACE_CHECK(pc) \
do { \
  /*if (trace & TRACE_PENDING_MASK) \
    { \
      if (SIM_TRACE_TRIGGER_P (trace_info) \
	  && (pc) == SIM_TRACE_TRIGGER (trace_info)) \
	trace = SIM_TRACE_VALUE (trace_info); \
    }*/ \
} while (0)

#define TRACE_INSN_INIT(cpu) \
  if (TRACE_P (cpu, TRACE_INSN_IDX)) \
    trace_insn_init (cpu);
#define TRACE_INSN_FINI(cpu) \
  if (TRACE_P (cpu, TRACE_INSN_IDX)) \
    trace_insn_fini (cpu);
#define TRACE_PRINTF(cpu, what, args) \
  if (TRACE_P (cpu, what)) \
    cgen_trace_printf args
#define TRACE_INSN(cpu, name, abuf, pc) \
  if (TRACE_P (cpu, TRACE_INSN_IDX)) \
    trace_insn (cpu, name, abuf, pc)
#define TRACE_EXTRACT(cpu, args) \
  if (TRACE_P (cpu, TRACE_EXTRACT_IDX)) \
    trace_extract args
#define TRACE_RESULT(cpu, name, type, val) \
  if (TRACE_P (cpu, TRACE_RESULT_IDX)) \
    trace_result (cpu, name, type, val)

#endif /* CGEN_TRACE_H */
