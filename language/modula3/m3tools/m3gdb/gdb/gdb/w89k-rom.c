/* Remote target glue for the WinBond ROM monitor running on the "Cougar"
   W89k eval board.

   Copyright 1995, 1998, 2000, 2001 Free Software Foundation, Inc.

   This file is part of GDB.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "gdbcore.h"
#include "target.h"
#include "monitor.h"
#include "serial.h"
#include "xmodem.h"
#include "regcache.h"


static void w89k_open (char *args, int from_tty);

/*
 * this array of registers need to match the indexes used by GDB. The
 * whole reason this exists is cause the various ROM monitors use
 * different strings than GDB does, and doesn't support all the
 * registers either. So, typing "info reg sp" becomes a "r30".
 */

static char *w89k_regnames[NUM_REGS] =
{
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",
  "SAR", "PC", NULL, NULL, NULL, "EIEM", "IIR", "IVA",
  "IOR", "IPSW", NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  "CCR", NULL, NULL, "TR0", "TR1",
};

static void
w89k_supply_register (char *regname, int regnamelen, char *val, int vallen)
{
  int numregs;
  int regno;

  numregs = 1;
  regno = -1;

  if (regnamelen == 2)
    switch (regname[0])
      {
      case 'r':
	numregs = 4;
	switch (regname[1])
	  {
	  case '0':
	    regno = R0_REGNUM;
	    break;
	  case '4':
	    regno = R0_REGNUM + 4;
	    break;
	  case '8':
	    regno = R0_REGNUM + 8;
	    break;
	  }
	break;
      case 'P':
	if (regname[1] == 'C')
	  regno = PC_REGNUM;
	break;
      }
  else if (regnamelen == 3)
    switch (regname[0])
      {
      case 'r':
	numregs = 4;
	if (regname[1] == '1' && regname[2] == '2')
	  regno = R0_REGNUM + 12;
	else if (regname[1] == '1' && regname[2] == '6')
	  regno = R0_REGNUM + 16;
	else if (regname[1] == '2' && regname[2] == '0')
	  regno = R0_REGNUM + 20;
	else if (regname[1] == '2' && regname[2] == '4')
	  regno = R0_REGNUM + 24;
	else if (regname[1] == '2' && regname[2] == '8')
	  regno = R0_REGNUM + 28;
	break;
      case 'R':
	if (regname[1] == 'C' && regname[2] == 'R')
	  regno = RCR_REGNUM;
	break;
      case 'C':
	if (regname[1] == 'C' && regname[2] == 'R')
	  regno = CCR_REGNUM;
	break;
      case 'S':
	if (regname[1] == 'A' && regname[2] == 'R')
	  regno = SAR_REGNUM;
	break;
      case 'I':
	if (regname[1] == 'I' && regname[2] == 'R')
	  regno = IIR_REGNUM;
	else if (regname[1] == 'O' && regname[2] == 'R')
	  regno = IOR_REGNUM;
	break;
      case 'T':
	numregs = 4;
	if (regname[1] == 'R')
	  if (regname[2] == '0')
	    regno = TR0_REGNUM;
	  else if (regname[2] == '4')
	    regno = TR0_REGNUM + 4;
	break;
      }
  else if (regnamelen == 4)
    switch (regname[0])
      {
      case 'E':
	if (regname[1] == 'I')
	  if (regname[2] == 'E' && regname[3] == 'M')
	    regno = EIEM_REGNUM;
	break;
      case 'I':
	if (regname[1] == 'P' && regname[2] == 'S' && regname[3] == 'W')
	  regno = IPSW_REGNUM;
	break;
      }
  else if (regnamelen == 5)
    switch (regname[0])
      {
      case 'I':
	if (regname[1] == 'A'
	    && regname[2] == 'O'
	    && regname[3] == 'Q'
	    && regname[4] == 'B')
	  regno = PCOQ_TAIL_REGNUM;
	break;
      }

  if (regno >= 0)
    while (numregs-- > 0)
      val = monitor_supply_register (regno++, val);
}

static int hashmark = 1;	/* flag set by "set hash" */

extern struct monitor_ops w89k_cmds;	/* fwd decl */

static void
w89k_load (struct serial *desc, char *file, int hashmark)
{
  bfd *abfd;
  asection *s;
  char *buffer;
  int i;

  buffer = alloca (XMODEM_PACKETSIZE);

  abfd = bfd_openr (file, 0);
  if (!abfd)
    {
      printf_filtered ("Unable to open file %s\n", file);
      return;
    }

  if (bfd_check_format (abfd, bfd_object) == 0)
    {
      printf_filtered ("File is not an object file\n");
      return;
    }

  for (s = abfd->sections; s; s = s->next)
    if (s->flags & SEC_LOAD)
      {
	bfd_size_type section_size;

	printf_filtered ("%s\t: 0x%4x .. 0x%4x  ", s->name, s->vma,
			 s->vma + s->_raw_size);
	gdb_flush (gdb_stdout);

	monitor_printf (w89k_cmds.load, s->vma);
	if (w89k_cmds.loadresp)
	  monitor_expect (w89k_cmds.loadresp, NULL, 0);

	xmodem_init_xfer (desc);

	section_size = bfd_section_size (abfd, s);

	for (i = 0; i < section_size; i += XMODEM_DATASIZE)
	  {
	    int numbytes;

	    numbytes = min (XMODEM_DATASIZE, section_size - i);

	    bfd_get_section_contents (abfd, s, buffer + XMODEM_DATAOFFSET, i,
				      numbytes);

	    xmodem_send_packet (desc, buffer, numbytes, hashmark);

	    if (hashmark)
	      {
		putchar_unfiltered ('#');
		gdb_flush (gdb_stdout);
	      }
	  }			/* Per-packet (or S-record) loop */

	xmodem_finish_xfer (desc);

	monitor_expect_prompt (NULL, 0);

	putchar_unfiltered ('\n');
      }				/* Loadable sections */

  if (hashmark)
    putchar_unfiltered ('\n');
}

/*
 * Define the monitor command strings. Since these are passed directly
 * through to a printf style function, we need can include formatting
 * strings. We also need a CR or LF on the end.
 */

static struct target_ops w89k_ops;

static char *w89k_inits[] =
{"\n", NULL};

static struct monitor_ops w89k_cmds;
static void
init_w89k_cmds (void)
{
  w89k_cmds.flags = MO_GETMEM_NEEDS_RANGE | MO_FILL_USES_ADDR;	/* flags */
  w89k_cmds.init = w89k_inits;	/* Init strings */
  w89k_cmds.cont = "g\n";	/* continue command */
  w89k_cmds.step = "t\n";	/* single step */
  w89k_cmds.stop = "\003";	/* Interrupt char (^C) */
  w89k_cmds.set_break = "bp %x\n";	/* set a breakpoint */
  w89k_cmds.clr_break = "bc %x\n";	/* clear a breakpoint */
  w89k_cmds.clr_all_break = "bc *\n";	/* clear all breakpoints */
  w89k_cmds.fill = "f %x %x %x\n";	/* memory fill cmd */
  w89k_cmds.setmem.cmdb = "eb %x %x\n";		/* setmem.cmdb (addr, value) */
  w89k_cmds.setmem.cmdw = "eh %x %x\n";		/* setmem.cmdw (addr, value) */
  w89k_cmds.setmem.cmdl = "ew %x %x\n";		/* setmem.cmdl (addr, value) */
  w89k_cmds.setmem.cmdll = NULL;	/* setmem.cmdll (addr, value) */
  w89k_cmds.setmem.resp_delim = NULL;	/* setreg.resp_delim */
  w89k_cmds.setmem.term = NULL;	/* setreg.term */
  w89k_cmds.setmem.term_cmd = NULL;	/* setreg.term_cmd */
  w89k_cmds.getmem.cmdb = "db %x %x\n";		/* getmem.cmdb (startaddr, endaddr) */
  w89k_cmds.getmem.cmdw = "dh %x %x\n";		/* getmem.cmdw (startaddr, endaddr) */
  w89k_cmds.getmem.cmdl = "dw %x %x\n";		/* getmem.cmdl (startaddr, endaddr) */
  w89k_cmds.getmem.cmdll = NULL;	/* getmem.cmdll (startaddr, endaddr) */
  w89k_cmds.getmem.resp_delim = "  ";	/* getmem.resp_delim */
  w89k_cmds.getmem.term = NULL;	/* getmem.term */
  w89k_cmds.getmem.term_cmd = NULL;	/* getmem.term_cmd */
  w89k_cmds.setreg.cmd = "r %s %x\n";	/* setreg.cmd (name, value) */
  w89k_cmds.setreg.resp_delim = NULL;	/* setreg.resp_delim */
  w89k_cmds.setreg.term = NULL;	/* setreg.term */
  w89k_cmds.setreg.term_cmd = NULL;	/* setreg.term_cmd */
  w89k_cmds.getreg.cmd = "r %s\n";	/* getreg.cmd (name) */
  w89k_cmds.getreg.resp_delim = "\r";	/* getreg.resp_delim */
  w89k_cmds.getreg.term = NULL;	/* getreg.term */
  w89k_cmds.getreg.term_cmd = NULL;	/* getreg.term_cmd */
  w89k_cmds.dump_registers = "r\n";	/* dump_registers */
  w89k_cmds.register_pattern = "\\(\\w+\\)\\( +[0-9a-fA-F]+\\b\\)+";
  w89k_cmds.supply_register = w89k_supply_register;	/* supply_register */
  w89k_cmds.load_routine = w89k_load;	/* load routine */
  w89k_cmds.load = "u %x\n";	/* download command */
  w89k_cmds.loadresp = "\021";	/* load response (^Q) */
  w89k_cmds.prompt = "ROM>";	/* monitor command prompt */
  w89k_cmds.line_term = "\n";	/* end-of-line terminator */
  w89k_cmds.cmd_end = NULL;	/* optional command terminator */
  w89k_cmds.target = &w89k_ops;	/* target operations */
  w89k_cmds.stopbits = SERIAL_1_STOPBITS;	/* number of stop bits */
  w89k_cmds.regnames = w89k_regnames;	/* register names */
  w89k_cmds.magic = MONITOR_OPS_MAGIC;	/* magic */
}				/* init_w89k_cmds */

static void
w89k_open (char *args, int from_tty)
{
  monitor_open (args, &w89k_cmds, from_tty);
}

void
_initialize_w89k (void)
{
  init_w89k_cmds ();
  init_monitor_ops (&w89k_ops);

  w89k_ops.to_shortname = "w89k";
  w89k_ops.to_longname = "WinBond's debug monitor for the W89k Eval board";
  w89k_ops.to_doc = "Debug on a WinBond W89K eval board.\n\
Specify the serial device it is connected to (e.g. /dev/ttya).";
  w89k_ops.to_open = w89k_open;

  add_target (&w89k_ops);
}
