/*  dv-m68hc11tim.c -- Simulation of the 68HC11 timer devices.
    Copyright (C) 1999, 2000 Free Software Foundation, Inc.
    Written by Stephane Carrez (stcarrez@worldnet.fr)
    (From a driver model Contributed by Cygnus Solutions.)

    This file is part of the program GDB, the GNU debugger.
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either vertimn 2 of the License, or
    (at your option) any later vertimn.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
    
    */


#include "sim-main.h"
#include "hw-main.h"
#include "sim-assert.h"


/* DEVICE

        m68hc11tim - m68hc11 timer devices

   
   DESCRIPTION
   
        Implements the m68hc11 timer as described in Chapter 10
        of the pink book.

   
   PROPERTIES

        none

   
   PORTS

   reset (input)

        Reset the timer device.  This port must be connected to
        the cpu-reset output port.

   */



/* port ID's */

enum
{
  RESET_PORT
};


static const struct hw_port_descriptor m68hc11tim_ports[] = 
{
  { "reset", RESET_PORT, 0, input_port, },
  { NULL, },
};


/* Timer Controller information.  */
struct m68hc11tim 
{
  unsigned long cop_delay;
  unsigned long rti_delay;
  unsigned long ovf_delay;
  signed64      clock_prescaler;
  signed64      tcnt_adjust;
  signed64      cop_prev_interrupt;
  signed64      rti_prev_interrupt;

  /* Periodic timers.  */
  struct hw_event *rti_timer_event;
  struct hw_event *cop_timer_event;
  struct hw_event *tof_timer_event;
  struct hw_event *cmp_timer_event;
};



/* Finish off the partially created hw device.  Attach our local
   callbacks.  Wire up our port names etc.  */

static hw_io_read_buffer_method m68hc11tim_io_read_buffer;
static hw_io_write_buffer_method m68hc11tim_io_write_buffer;
static hw_port_event_method m68hc11tim_port_event;
static hw_ioctl_method m68hc11tim_ioctl;

#define M6811_TIMER_FIRST_REG (M6811_TCTN)
#define M6811_TIMER_LAST_REG  (M6811_PACNT)


static void
attach_m68hc11tim_regs (struct hw *me,
                        struct m68hc11tim *controller)
{
  hw_attach_address (hw_parent (me), M6811_IO_LEVEL, io_map,
                     M6811_TIMER_FIRST_REG,
                     M6811_TIMER_LAST_REG - M6811_TIMER_FIRST_REG + 1,
		     me);
}

static void
m68hc11tim_finish (struct hw *me)
{
  struct m68hc11tim *controller;

  controller = HW_ZALLOC (me, struct m68hc11tim);
  set_hw_data (me, controller);
  set_hw_io_read_buffer (me, m68hc11tim_io_read_buffer);
  set_hw_io_write_buffer (me, m68hc11tim_io_write_buffer);
  set_hw_ports (me, m68hc11tim_ports);
  set_hw_port_event (me, m68hc11tim_port_event);
#ifdef set_hw_ioctl
  set_hw_ioctl (me, m68hc11tim_ioctl);
#else
  me->to_ioctl = m68hc11tim_ioctl;
#endif
  
  /* Preset defaults.  */
  controller->clock_prescaler = 1;
  controller->tcnt_adjust = 0;
  
  /* Attach ourself to our parent bus.  */
  attach_m68hc11tim_regs (me, controller);
}



/* An event arrives on an interrupt port.  */

static void
m68hc11tim_port_event (struct hw *me,
                       int my_port,
                       struct hw *source,
                       int source_port,
                       int level)
{
  SIM_DESC sd;
  struct m68hc11tim *controller;
  sim_cpu *cpu;
  unsigned8 val;
  
  controller = hw_data (me);
  sd         = hw_system (me);
  cpu        = STATE_CPU (sd, 0);
  switch (my_port)
    {
    case RESET_PORT:
      {
	HW_TRACE ((me, "Timer reset"));

        /* Cancel all timer events.  */
        if (controller->rti_timer_event)
          {
            hw_event_queue_deschedule (me, controller->rti_timer_event);
            controller->rti_timer_event = 0;
            controller->rti_prev_interrupt = 0;
          }
        if (controller->cop_timer_event)
          {
            hw_event_queue_deschedule (me, controller->cop_timer_event);
            controller->cop_timer_event = 0;
            controller->cop_prev_interrupt = 0;
          }
        if (controller->tof_timer_event)
          {
            hw_event_queue_deschedule (me, controller->tof_timer_event);
            controller->tof_timer_event = 0;
          }
        if (controller->cmp_timer_event)
          {
            hw_event_queue_deschedule (me, controller->cmp_timer_event);
            controller->cmp_timer_event = 0;
          }

        /* Reset the state of Timer registers.  This also restarts
           the timer events (overflow and RTI clock).  */
        val = 0;
        m68hc11tim_io_write_buffer (me, &val, io_map,
                                    (unsigned_word) M6811_TMSK2, 1);
        m68hc11tim_io_write_buffer (me, &val, io_map,
                                    (unsigned_word) M6811_TFLG2, 1);
        m68hc11tim_io_write_buffer (me, &val, io_map,
                                    (unsigned_word) M6811_PACTL, 1);
        break;
      }

    default:
      hw_abort (me, "Event on unknown port %d", my_port);
      break;
    }
}

enum event_type
{
  COP_EVENT,
  RTI_EVENT,
  OVERFLOW_EVENT,
  COMPARE_EVENT
};

void
m68hc11tim_timer_event (struct hw *me, void *data)
{
  SIM_DESC sd;
  struct m68hc11tim *controller;
  sim_cpu *cpu;
  enum event_type type;
  unsigned long delay;
  struct hw_event **eventp;
  int check_interrupt = 0;
  unsigned mask;
  unsigned flags;
  unsigned long tcnt_internal;
  unsigned long tcnt;
  int i;
  sim_events *events;
  
  controller = hw_data (me);
  sd         = hw_system (me);
  cpu        = STATE_CPU (sd, 0);
  type       = (enum event_type) ((long) data) & 0x0FF;
  events     = STATE_EVENTS (sd);

  delay = 0;
  switch (type)
    {
    case COP_EVENT:
      eventp = &controller->cop_timer_event;
      delay  = controller->cop_delay;
      delay  = controller->cop_prev_interrupt + controller->cop_delay;
      controller->cop_prev_interrupt = delay;
      delay  = delay - cpu->cpu_absolute_cycle;
      check_interrupt = 1;
      delay += events->nr_ticks_to_process;
      break;

    case RTI_EVENT:
      eventp = &controller->rti_timer_event;
      delay  = controller->rti_prev_interrupt + controller->rti_delay;
      
      if (((long) (data) & 0x0100) == 0)
        {
          cpu->ios[M6811_TFLG2] |= M6811_RTIF;
          check_interrupt = 1;
          controller->rti_prev_interrupt = delay;
          delay += controller->rti_delay;
        }
      delay = delay - cpu->cpu_absolute_cycle;
      delay += events->nr_ticks_to_process;
      break;

    case OVERFLOW_EVENT:
      /* Compute the 68HC11 internal free running counter.
         There may be 'nr_ticks_to_process' pending cycles that are
         not (yet) taken into account by 'sim_events_time'.  */
      tcnt_internal = sim_events_time (sd) - controller->tcnt_adjust;
      tcnt_internal += events->nr_ticks_to_process;

      /* We must take into account the prescaler that comes
         before the counter (it's a power of 2).  */
      tcnt_internal &= 0x0ffff * controller->clock_prescaler;

      /* Compute the time when the overflow will occur.  It occurs when
         the counter increments from 0x0ffff to 0x10000 (and thus resets).  */
      delay = (0x10000 * controller->clock_prescaler) - tcnt_internal;

      /* The 'nr_ticks_to_process' will be subtracted when the event
         is scheduled.  */
      delay += events->nr_ticks_to_process;

      eventp = &controller->tof_timer_event;
      if (((long) (data) & 0x100) == 0)
        {
          cpu->ios[M6811_TFLG2] |= M6811_TOF;
          check_interrupt = 1;
        }
      break;

    case COMPARE_EVENT:
      eventp = &controller->cmp_timer_event;

      /* Compute the 68HC11 internal free running counter.
         There may be 'nr_ticks_to_process' pending cycles that are
         not (yet) taken into account by 'sim_events_time'.  */
      events = STATE_EVENTS (sd);
      tcnt_internal = sim_events_time (sd) - controller->tcnt_adjust;
      tcnt_internal += events->nr_ticks_to_process;

      /* We must take into account the prescaler that comes
         before the counter (it's a power of 2).  */
      tcnt_internal &= 0x0ffff * controller->clock_prescaler;

      /* Get current visible TCNT register value.  */
      tcnt = tcnt_internal / controller->clock_prescaler;
      
      flags = cpu->ios[M6811_TMSK1];
      mask  = 0x80;
      delay = 65536 * controller->clock_prescaler;

      /* Scan each output compare register to see if one matches
         the free running counter.  Set the corresponding OCi flag
         if the output compare is enabled.  */
      for (i = M6811_TOC1; i <= M6811_TOC5; i += 2, mask >>= 1)
        {
          unsigned long compare;
          
          compare = (cpu->ios[i] << 8) + cpu->ios[i+1];
          if (compare == tcnt && (flags & mask))
            {
              cpu->ios[M6811_TFLG1] |= mask;
              check_interrupt++;
            }

          /* Compute how many times for the next match.
             Use the internal counter value to take into account the
             prescaler accurately.  */
          compare = compare * controller->clock_prescaler;
          if (compare > tcnt_internal)
            compare = compare - tcnt_internal;
          else
            compare = compare - tcnt_internal
              + 65536 * controller->clock_prescaler;
          
          if (compare < delay)
            delay = compare;
        }

      /* Deactivate the compare timer if no output compare is enabled.  */
      if ((flags & 0xF0) == 0)
        delay = 0;
      break;

    default:
      eventp = 0;
      break;
    }

  if (*eventp)
    {
      hw_event_queue_deschedule (me, *eventp);
      *eventp = 0;
    }

  if (delay != 0)
    {
      *eventp = hw_event_queue_schedule (me, delay,
                                         m68hc11tim_timer_event,
                                         (void*) type);
    }

  if (check_interrupt)
    interrupts_update_pending (&cpu->cpu_interrupts);
}


/* Descriptions of the Timer I/O ports.  These descriptions are only used to
   give information of the Timer device under GDB.  */
io_reg_desc tmsk2_desc[] = {
  { M6811_TOI,    "TOI   ", "Timer Overflow Interrupt Enable" },
  { M6811_RTII,   "RTII  ", "RTI Interrupt Enable" },
  { M6811_PAOVI,  "PAOVI ", "Pulse Accumulator Overflow Interrupt Enable" },
  { M6811_PAII,   "PAII  ", "Pulse Accumulator Interrupt Enable" },
  { M6811_PR1,    "PR1   ", "Timer prescaler (PR1)" },
  { M6811_PR0,    "PR0   ", "Timer prescaler (PR0)" },
  { M6811_TPR_1,  "TPR_1 ", "Timer prescaler div 1" },
  { M6811_TPR_4,  "TPR_4 ", "Timer prescaler div 4" },
  { M6811_TPR_8,  "TPR_8 ", "Timer prescaler div 8" },
  { M6811_TPR_16, "TPR_16", "Timer prescaler div 16" },
  { 0,  0, 0 }
};

io_reg_desc tflg2_desc[] = {
  { M6811_TOF,   "TOF   ", "Timer Overflow Bit" },
  { M6811_RTIF,  "RTIF  ", "Read Time Interrupt Flag" },
  { M6811_PAOVF, "PAOVF ", "Pulse Accumulator Overflow Interrupt Flag" },
  { M6811_PAIF,  "PAIF  ", "Pulse Accumulator Input Edge" },
  { 0,  0, 0 }
};

io_reg_desc pactl_desc[] = {
  { M6811_DDRA7,  "DDRA7 ", "Data Direction for Port A bit-7" },
  { M6811_PAEN,   "PAEN  ", "Pulse Accumulator System Enable" },
  { M6811_PAMOD,  "PAMOD ", "Pulse Accumulator Mode" },
  { M6811_PEDGE,  "PEDGE ", "Pulse Accumulator Edge Control" },
  { M6811_RTR1,   "RTR1  ", "RTI Interrupt rate select (RTR1)" },
  { M6811_RTR0,   "RTR0  ", "RTI Interrupt rate select (RTR0)" },
  { 0,  0, 0 }
};

static double
to_realtime (sim_cpu *cpu, signed64 t)
{
  return (double) (t) / (double) (cpu->cpu_frequency / 4);
}

const char*
cycle_to_string (sim_cpu *cpu, signed64 t)
{
  double dt;
  static char buf[64];
  
  dt = to_realtime (cpu, t);
  if (dt < 0.001)
    sprintf (buf, "%llu cycle%s (%3.1f us)", t,
             (t > 1 ? "s" : ""), dt * 1000000.0);
  else if (dt < 1.0)
    sprintf (buf, "%llu cycles (%3.1f ms)", t, dt * 1000.0);
  else
    sprintf (buf, "%llu cycles (%3.1f s)", t, dt);

  return buf;
}

static void
m68hc11tim_print_timer (struct hw *me, const char *name,
                        struct hw_event *event)
{
  SIM_DESC sd;
  
  sd = hw_system (me);
  if (event == 0)
    {
      sim_io_printf (sd, "  No %s interrupt will be raised.\n", name);
    }
  else
    {
      signed64 t;
      sim_cpu* cpu;

      cpu = STATE_CPU (sd, 0);

      t  = hw_event_remain_time (me, event);
      sim_io_printf (sd, "  Next %s interrupt in %s\n",
                     name, cycle_to_string (cpu, t));
    }
}

static void
m68hc11tim_info (struct hw *me)
{
  SIM_DESC sd;
  uint16 base = 0;
  sim_cpu *cpu;
  struct m68hc11tim *controller;
  uint8 val;
  
  sd = hw_system (me);
  cpu = STATE_CPU (sd, 0);
  controller = hw_data (me);
  
  sim_io_printf (sd, "M68HC11 Timer:\n");

  base = cpu_get_io_base (cpu);

  val  = cpu->ios[M6811_TMSK2];
  print_io_byte (sd, "TMSK2 ", tmsk2_desc, val, base + M6811_TMSK2);
  sim_io_printf (sd, "\n");

  val = cpu->ios[M6811_TFLG2];
  print_io_byte (sd, "TFLG2", tflg2_desc, val, base + M6811_TFLG2);
  sim_io_printf (sd, "\n");

  val = cpu->ios[M6811_PACTL];
  print_io_byte (sd, "PACTL", pactl_desc, val, base + M6811_PACTL);
  sim_io_printf (sd, "\n");

  /* Give info about the next timer interrupts.  */
  m68hc11tim_print_timer (me, "RTI", controller->rti_timer_event);
  m68hc11tim_print_timer (me, "COP", controller->cop_timer_event);
  m68hc11tim_print_timer (me, "OVERFLOW", controller->tof_timer_event);
  m68hc11tim_print_timer (me, "COMPARE", controller->cmp_timer_event);
}

static int
m68hc11tim_ioctl (struct hw *me,
                  hw_ioctl_request request,
                  va_list ap)
{
  m68hc11tim_info (me);
  return 0;
}

/* generic read/write */

static unsigned
m68hc11tim_io_read_buffer (struct hw *me,
                           void *dest,
                           int space,
                           unsigned_word base,
                           unsigned nr_bytes)
{
  SIM_DESC sd;
  struct m68hc11tim *controller;
  sim_cpu *cpu;
  unsigned8 val;
  unsigned cnt = 0;
  
  HW_TRACE ((me, "read 0x%08lx %d", (long) base, (int) nr_bytes));

  sd  = hw_system (me);
  cpu = STATE_CPU (sd, 0);
  controller = hw_data (me);

  while (nr_bytes)
    {
      switch (base)
        {
          /* The cpu_absolute_cycle is updated after each instruction.
             Reading in a 16-bit register will be split in two accesses
             but this will be atomic within the simulator.  */
        case M6811_TCTN_H:
          val = (uint8) ((cpu->cpu_absolute_cycle - controller->tcnt_adjust)
                         / (controller->clock_prescaler * 256));
          break;

        case M6811_TCTN_L:
          val = (uint8) ((cpu->cpu_absolute_cycle - controller->tcnt_adjust)
                         / controller->clock_prescaler);
          break;

        default:
          val = cpu->ios[base];
          break;
        }
      *((unsigned8*) dest) = val;
      dest++;
      base++;
      nr_bytes--;
      cnt++;
    }
  return cnt;
}

static unsigned
m68hc11tim_io_write_buffer (struct hw *me,
                            const void *source,
                            int space,
                            unsigned_word base,
                            unsigned nr_bytes)
{
  SIM_DESC sd;
  struct m68hc11tim *controller;
  sim_cpu *cpu;
  unsigned8 val, n;
  signed64 adj;
  int reset_compare = 0;
  int reset_overflow = 0;
  int cnt = 0;
  
  HW_TRACE ((me, "write 0x%08lx %d", (long) base, (int) nr_bytes));

  sd  = hw_system (me);
  cpu = STATE_CPU (sd, 0);
  controller = hw_data (me);

  while (nr_bytes)
    {
      val = *((const unsigned8*) source);
      switch (base)
        {
          /* Set the timer counter low part, trying to preserve the low part.
             We compute the absolute cycle adjustment that we have to apply
             to obtain the timer current value.  Computation must be made
             in 64-bit to avoid overflow problems.  */
        case M6811_TCTN_L:
          adj = ((cpu->cpu_absolute_cycle - controller->tcnt_adjust)
                 / (controller->clock_prescaler * (signed64) 256)) & 0x0FF;
          adj = cpu->cpu_absolute_cycle
            - (adj * controller->clock_prescaler * (signed64) 256)
            - ((signed64) adj * controller->clock_prescaler);
          controller->tcnt_adjust = adj;
          reset_compare = 1;
          reset_overflow = 1;
          break;

        case M6811_TCTN_H:
          adj = ((cpu->cpu_absolute_cycle - controller->tcnt_adjust)
                 / controller->clock_prescaler) & 0x0ff;
          adj = cpu->cpu_absolute_cycle
            - ((signed64) val * controller->clock_prescaler * (signed64) 256)
            - (adj * controller->clock_prescaler);
          controller->tcnt_adjust = adj;
          reset_compare = 1;
          reset_overflow = 1;
          break;

        case M6811_TMSK2:

      /* Timer prescaler cannot be changed after 64 bus cycles.  */
          if (cpu->cpu_absolute_cycle >= 64)
            {
              val &= ~(M6811_PR1 | M6811_PR0);
              val |= cpu->ios[M6811_TMSK2] & (M6811_PR1 | M6811_PR0);
            }
          switch (val & (M6811_PR1 | M6811_PR0))
            {
            case 0:
              n = 1;
              break;
            case M6811_PR0:
              n = 4;
              break;
            case M6811_PR1:
              n = 8;
              break;
            default:
            case M6811_PR1 | M6811_PR0:
              n = 16;
              break;
            }
          if (cpu->cpu_absolute_cycle < 64)
            {
              reset_overflow = 1;
              controller->clock_prescaler = n;
            }
          cpu->ios[base] = val;
          interrupts_update_pending (&cpu->cpu_interrupts);
          break;

        case M6811_PACTL:
          n = (1 << ((val & (M6811_RTR1 | M6811_RTR0))));
          cpu->ios[base] = val;

          controller->rti_delay = (long) (n) * 8192;
          m68hc11tim_timer_event (me, (void*) (RTI_EVENT| 0x100));
          break;
      
        case M6811_TFLG2:
          if (val & M6811_TOF)
            val &= ~M6811_TOF;
          else
            val |= cpu->ios[M6811_TFLG2] & M6811_TOF;

      /* Clear the Real Time interrupt flag. */
          if (val & M6811_RTIF)
            val &= ~M6811_RTIF;
          else
            val |= cpu->ios[M6811_TFLG2] & M6811_RTIF;
      
          cpu->ios[base] = val;
          interrupts_update_pending (&cpu->cpu_interrupts);
          break;

        case M6811_TOC1:
        case M6811_TOC2:
        case M6811_TOC3:
        case M6811_TOC4:
        case M6811_TOC5:
          cpu->ios[base] = val;
          reset_compare = 1;
          break;
      
        default:
          break;
        }

      base++;
      nr_bytes--;
      cnt++;
      source++;
    }

  /* Re-compute the next timer compare event.  */
  if (reset_compare)
    {
      m68hc11tim_timer_event (me, (void*) (COMPARE_EVENT));
    }
  if (reset_overflow)
    {
      m68hc11tim_timer_event (me, (void*) (OVERFLOW_EVENT| 0x100));
    }
  return cnt;
}     


const struct hw_descriptor dv_m68hc11tim_descriptor[] = {
  { "m68hc11tim", m68hc11tim_finish },
  { "m68hc12tim", m68hc11tim_finish },
  { NULL },
};

