/* Parameters for Intel 960 running NINDY monitor, for GDB, the GNU debugger.
   Copyright 1990, 1991, 1996, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Intel Corporation and Cygnus Support.

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

/*****************************************************************************
 * Definitions to target GDB to an i960 debugged over a serial line.
 ******************************************************************************/

#include "i960/tm-i960.h"

/* forward declarations */
struct frame_info;

/* Override the standard gdb prompt when compiled for this target.  */

#define	DEFAULT_PROMPT	"(gdb960) "

/* Additional command line options accepted by nindy gdb's, for handling
   the remote-nindy.c interface.  These should really be target-specific
   rather than architecture-specific.  */

extern int nindy_old_protocol;	/* nonzero if old NINDY serial protocol */
extern int nindy_initial_brk;	/* Send a BREAK to reset board first */
extern char *nindy_ttyname;	/* Name of serial port to talk to nindy */

#define	ADDITIONAL_OPTIONS \
	{"O", no_argument, &nindy_old_protocol, 1},	\
	{"brk", no_argument, &nindy_initial_brk, 1},	\
	{"ser", required_argument, 0, 1004},	/* 1004 is magic cookie for ADDL_CASES */

#define	ADDITIONAL_OPTION_CASES	\
	case 1004:	/* -ser option:  remote nindy auto-start */	\
	  nindy_ttyname = optarg;	\
	  break;

#define	ADDITIONAL_OPTION_HELP \
	"\
  -O                Use old protocol to talk to a Nindy target\n\
  -brk              Send a break to a Nindy target to reset it.\n\
  -ser SERIAL       Open remote Nindy session to SERIAL port.\n\
"

/* If specified on the command line, open tty for talking to nindy,
   and download the executable file if one was specified.  */

extern void nindy_open (char *name, int from_tty);
#define	ADDITIONAL_OPTION_HANDLER					\
	if (nindy_ttyname != NULL)					\
          {								\
            if (catch_command_errors (nindy_open, nindy_ttyname,	\
				      !batch, RETURN_MASK_ALL))		\
	      {								\
                if (execarg != NULL)					\
                  catch_command_errors (target_load, execarg, !batch,	\
					RETURN_MASK_ALL);		\
	      }								\
	  }

/* If configured for i960 target, we take control before main loop
   and demand that we configure for a nindy target.  */

#define	BEFORE_MAIN_LOOP_HOOK	\
  nindy_before_main_loop();

extern void
  nindy_before_main_loop ();	/* In remote-nindy.c */

/* FRAME_CHAIN_VALID returns zero if the given frame is the outermost one
   and has no caller.

   On the i960, each various target system type defines FRAME_CHAIN_VALID,
   since it differs between NINDY and VxWorks, the two currently supported
   targets types.  */

extern int nindy_frame_chain_valid (CORE_ADDR, struct frame_info *);
#define	FRAME_CHAIN_VALID(chain, thisframe) nindy_frame_chain_valid (chain, thisframe)

extern int
  nindy_frame_chain_valid ();	/* See nindy-tdep.c */

/* Sequence of bytes for breakpoint instruction */

#define BREAKPOINT {0x00, 0x3e, 0x00, 0x66}

/* Amount ip must be decremented by after a breakpoint.
 * This is often the number of bytes in BREAKPOINT but not always.
 */

#define DECR_PC_AFTER_BREAK 0

#undef REGISTER_CONVERT_TO_VIRTUAL
#undef REGISTER_CONVERT_TO_RAW
#undef REGISTER_CONVERTIBLE
