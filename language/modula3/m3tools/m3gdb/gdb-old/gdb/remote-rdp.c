/* Remote debugging for the ARM RDP interface.
   Copyright 1994, 1995 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  


 */


/* 
   Much of this file (in particular the SWI stuff) is based on code by
   David Taylor (djt1000@uk.ac.cam.hermes).

   I hacked on and simplified it by removing a lot of sexy features he
   had added, and some of the (unix specific) workarounds he'd done
   for other GDB problems - which if they still exist should be fixed
   in GDB, not in a remote-foo thing .  I also made it conform more to
   the doc I have; which may be wrong.

   Steve Chamberlain (sac@cygnus.com).
 */


#include "defs.h"
#include "inferior.h"
#include "wait.h"
#include "value.h"
#include "callback.h"
#include "command.h"
#ifdef ANSI_PROTOTYPES
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <ctype.h>
#include <fcntl.h>
#include "symfile.h"
#include "remote-utils.h"
#include "gdb_string.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


extern struct target_ops remote_rdp_ops;
static serial_t io;
static host_callback *callback = &default_callback;

struct
  {
    int step_info;
    int break_info;
    int model_info;
    int target_info;
    int can_step;
    char command_line[10];
    int rdi_level;
    int rdi_stopped_status;
  }
ds;



/* Definitions for the RDP protocol. */

#define RDP_MOUTHFULL   		(1<<6)
#define FPU_COPRO_NUMBER 		1

#define RDP_OPEN 	 		0
#define RDP_OPEN_TYPE_COLD 		0
#define RDP_OPEN_TYPE_WARM 		1
#define RDP_OPEN_TYPE_BAUDRATE          2

#define RDP_OPEN_BAUDRATE_9600       	1
#define RDP_OPEN_BAUDRATE_19200        	2
#define RDP_OPEN_BAUDRATE_38400        	3

#define RDP_OPEN_TYPE_RETURN_SEX	(1<<3)

#define RDP_CLOSE 			1

#define RDP_MEM_READ 			2

#define RDP_MEM_WRITE 			3

#define RDP_CPU_READ 			4
#define RDP_CPU_WRITE 			5
#define RDP_CPU_READWRITE_MODE_CURRENT 255
#define RDP_CPU_READWRITE_MASK_PC 	(1<<16)
#define RDP_CPU_READWRITE_MASK_CPSR 	(1<<17)
#define RDP_CPU_READWRITE_MASK_SPSR 	(1<<18)

#define RDP_COPRO_READ   		6
#define RDP_COPRO_WRITE 		7
#define RDP_FPU_READWRITE_MASK_FPS 	(1<<8)

#define RDP_SET_BREAK			0xa
#define RDP_SET_BREAK_TYPE_PC_EQUAL     0
#define RDP_SET_BREAK_TYPE_GET_HANDLE   (0x10)

#define RDP_CLEAR_BREAK 		0xb

#define RDP_EXEC 			0x10
#define RDP_EXEC_TYPE_SYNC 		0

#define RDP_STEP 			0x11

#define RDP_INFO  			0x12
#define RDP_INFO_ABOUT_STEP 		2
#define RDP_INFO_ABOUT_STEP_GT_1	1
#define RDP_INFO_ABOUT_STEP_TO_JMP 	2
#define RDP_INFO_ABOUT_STEP_1		4
#define RDP_INFO_ABOUT_TARGET 		0
#define RDP_INFO_ABOUT_BREAK 		1
#define RDP_INFO_ABOUT_BREAK_COMP	1
#define RDP_INFO_ABOUT_BREAK_RANGE 	2
#define RDP_INFO_ABOUT_BREAK_BYTE_READ 	4
#define RDP_INFO_ABOUT_BREAK_HALFWORD_READ 8
#define RDP_INFO_ABOUT_BREAK_WORD_READ (1<<4)
#define RDP_INFO_ABOUT_BREAK_BYTE_WRITE (1<<5)
#define RDP_INFO_ABOUT_BREAK_HALFWORD_WRITE (1<<6)
#define RDP_INFO_ABOUT_BREAK_WORD_WRITE (1<<7)
#define RDP_INFO_ABOUT_BREAK_MASK 	(1<<8)
#define RDP_INFO_ABOUT_BREAK_THREAD_BREAK (1<<9)
#define RDP_INFO_ABOUT_BREAK_THREAD_WATCH (1<<10)
#define RDP_INFO_ABOUT_BREAK_COND 	(1<<11)

#define RDP_RESET 			0x7f

/* Returns from RDP */
#define RDP_RES_STOPPED 		0x20
#define RDP_RES_SWI 			0x21
#define RDP_RES_FATAL 			0x5e
#define RDP_RES_VALUE 			0x5f
#define RDP_RES_VALUE_LITTLE_ENDIAN     240
#define RDP_RES_VALUE_BIG_ENDIAN 	241
#define RDP_RES_RESET			0x7f
#define RDP_RES_AT_BREAKPOINT    	143
#define RDP_RES_IDUNNO			0xe6
#define RDP_OSOpReply           	0x13
#define RDP_OSOpWord            	2
#define RDP_OSOpNothing         	0

static int timeout = 2;

static int
remote_rdp_xfer_inferior_memory PARAMS ((CORE_ADDR memaddr,
					 char *myaddr,
					 int len,
					 int write,
					 struct target_ops * target));


/* Stuff for talking to the serial layer. */

static unsigned char
get_byte ()
{
  int c = SERIAL_READCHAR (io, timeout);

  if (remote_debug)
    printf ("[%02x]\n", c);

  if (c == SERIAL_TIMEOUT)
    {
      if (timeout == 0)
	return (unsigned char) c;

      error ("Timeout reading from remote_system");
    }

  return c;
}

/* Note that the target always speaks little-endian to us,
   even if it's a big endian machine. */
static unsigned int
get_word ()
{
  unsigned int val = 0;
  unsigned int c;
  int n;
  for (n = 0; n < 4; n++)
    {
      c = get_byte ();
      val |= c << (n * 8);
    }
  return val;
}

static void
put_byte (val)
     char val;
{
  if (remote_debug)
    printf ("(%02x)\n", val);
  SERIAL_WRITE (io, &val, 1);
}

static void
put_word (val)
     int val;
{
  /* We always send in little endian */
  unsigned char b[4];
  b[0] = val;
  b[1] = val >> 8;
  b[2] = val >> 16;
  b[3] = val >> 24;

  if (remote_debug)
    printf ("(%04x)", val);

  SERIAL_WRITE (io, b, 4);
}



/* Stuff for talking to the RDP layer. */

/* This is a bit more fancy that need be so that it syncs even in nasty cases.

   I'be been unable to make it reliably sync up with the change
   baudrate open command.  It likes to sit and say it's been reset,
   with no more action.  So I took all that code out.  I'd rather sync
   reliably at 9600 than wait forever for a possible 19200 connection.

 */
static void
rdp_init (cold, tty)
     int cold;
     int tty;
{
  int sync = 0;
  int type = cold ? RDP_OPEN_TYPE_COLD : RDP_OPEN_TYPE_WARM;
  int baudtry = 9600;

  time_t now = time (0);
  time_t stop_time = now + 10;	/* Try and sync for 10 seconds, then give up */


  while (time (0) < stop_time && !sync)
    {
      int restype;
      QUIT;

      SERIAL_FLUSH_INPUT (io);
      SERIAL_FLUSH_OUTPUT (io);

      if (tty)
	printf_unfiltered ("Trying to connect at %d baud.\n", baudtry);
      put_byte (RDP_OPEN);

      put_byte (type | RDP_OPEN_TYPE_RETURN_SEX);
      put_word (0);

      while (!sync && (restype = SERIAL_READCHAR (io, 1)) > 0)
	{
	  if (remote_debug)
	    printf_unfiltered ("[%02x]\n", restype);

	  switch (restype)
	    {
	    case SERIAL_TIMEOUT:
	      break;
	    case RDP_RESET:
	      while ((restype = SERIAL_READCHAR (io, 1)) == RDP_RESET)
		;
	      while ((restype = SERIAL_READCHAR (io, 1)) > 0)
		{
		  printf_unfiltered ("%c", isgraph (restype) ? restype : ' ');
		}
	      while ((restype = SERIAL_READCHAR (io, 1)) > 0)
		;
	      if (tty)
		{
		  printf_unfiltered ("\nThe board has sent notification that it was reset.\n");
		  printf_unfiltered ("Waiting for it to settle down...\n");
		}
	      sleep (3);
	      if (tty)
		printf_unfiltered ("\nTrying again.\n");
	      break;
	    default:
	      break;
	    case RDP_RES_VALUE:
	      {
		int resval = SERIAL_READCHAR (io, 1);
		switch (resval)
		  {
		  case SERIAL_TIMEOUT:
		    break;
		  case RDP_RES_VALUE_LITTLE_ENDIAN:
		    target_byte_order = LITTLE_ENDIAN;
		    sync = 1;
		    break;
		  case RDP_RES_VALUE_BIG_ENDIAN:
		    target_byte_order = BIG_ENDIAN;
		    sync = 1;
		    break;
		  default:
		    break;
		  }
	      }
	    }
	}
    }

  if (!sync)
    {
      error ("Couldn't reset the board, try pressing the reset button");
    }
}


#ifdef ANSI_PROTOTYPES
void
send_rdp (char *template,...)
#else
void
send_rdp (char *template, va_alist)
     va_dcl
#endif
{
  char buf[200];
  char *dst = buf;
  va_list alist;
#ifdef ANSI_PROTOTYPES
  va_start (alist, template);
#else
  va_start (alist);
#endif

  while (*template)
    {
      unsigned int val;
      int *pi;
      int *pstat;
      char *pc;
      int i;
      switch (*template++)
	{
	case 'b':
	  val = va_arg (alist, int);
	  *dst++ = val;
	  break;
	case 'w':
	  val = va_arg (alist, int);
	  *dst++ = val;
	  *dst++ = val >> 8;
	  *dst++ = val >> 16;
	  *dst++ = val >> 24;
	  break;
	case 'S':
	  val = get_byte ();
	  if (val != RDP_RES_VALUE)
	    {
	      printf_unfiltered ("got bad res value of %d, %x\n", val, val);
	    }
	  break;
	case 'V':
	  pstat = va_arg (alist, int *);
	  pi = va_arg (alist, int *);

	  *pstat = get_byte ();
	  /* Check the result was zero, if not read the syndrome */
	  if (*pstat)
	    {
	      *pi = get_word ();
	    }
	  break;
	case 'Z':
	  /* Check the result code, error if not zero */
	  if (get_byte ())
	    error ("Command garbled");
	  break;
	case 'W':
	  /* Read a word from the target */
	  pi = va_arg (alist, int *);
	  *pi = get_word ();
	  break;
	case 'P':
	  /* Read in some bytes from the target. */
	  pc = va_arg (alist, char *);
	  val = va_arg (alist, int);
	  for (i = 0; i < val; i++)
	    {
	      pc[i] = get_byte ();
	    }
	  break;
	case 'p':
	  /* send what's being pointed at */
	  pc = va_arg (alist, char *);
	  val = va_arg (alist, int);
	  dst = buf;
	  SERIAL_WRITE (io, pc, val);
	  break;
	case '-':
	  /* Send whats in the queue */
	  if (dst != buf)
	    {
	      SERIAL_WRITE (io, buf, dst - buf);
	      dst = buf;
	    }
	  break;
	case 'B':
	  pi = va_arg (alist, int *);
	  *pi = get_byte ();
	  break;
	default:
	  abort ();
	}
    }
  va_end (args);

  if (dst != buf)
    abort ();
}


static int
rdp_write (memaddr, buf, len)
     CORE_ADDR memaddr;
     char *buf;
     int len;
{
  int res;
  int val;

  send_rdp ("bww-p-SV", RDP_MEM_WRITE, memaddr, len, buf, len, &res, &val);

  if (res)
    {
      return val;
    }
  return len;
}


static int
rdp_read (memaddr, buf, len)
     CORE_ADDR memaddr;
     char *buf;
     int len;
{
  int res;
  int val;
  send_rdp ("bww-S-P-V",
	    RDP_MEM_READ, memaddr, len,
	    buf, len,
	    &res, &val);
  if (res)
    {
      return val;
    }
  return len;
}

static void
rdp_fetch_one_register (mask, buf)
     int mask;
     char *buf;
{
  int val;
  send_rdp ("bbw-SWZ", RDP_CPU_READ, RDP_CPU_READWRITE_MODE_CURRENT, mask, &val);
  store_signed_integer (buf, 4, val);
}

static void
rdp_fetch_one_fpu_register (mask, buf)
     int mask;
     char *buf;
{
#if 0
  /* !!! Since the PIE board doesn't work as documented,
     and it doesn't have FPU hardware anyway and since it
     slows everything down, I've disabled this. */
  int val;
  if (mask == RDP_FPU_READWRITE_MASK_FPS)
    {
      /* this guy is only a word */
      send_rdp ("bbw-SWZ", RDP_COPRO_READ, FPU_COPRO_NUMBER, mask, &val);
      store_signed_integer (buf, 4, val);
    }
  else
    {
      /* There are 12 bytes long 
         !! fixme about endianness 
       */
      int dummy;		/* I've seen these come back as four words !! */
      send_rdp ("bbw-SWWWWZ", RDP_COPRO_READ, FPU_COPRO_NUMBER, mask, buf + 0, buf + 4, buf + 8, &dummy);
    }
#endif
  memset (buf, 0, MAX_REGISTER_RAW_SIZE);
}


static void
rdp_store_one_register (mask, buf)
     int mask;
     char *buf;
{
  int val = extract_unsigned_integer (buf, 4);

  send_rdp ("bbww-SZ",
	    RDP_CPU_WRITE, RDP_CPU_READWRITE_MODE_CURRENT, mask, val);
}


static void
rdp_store_one_fpu_register (mask, buf)
     int mask;
     char *buf;
{
#if 0
  /* See comment in fetch_one_fpu_register */
  if (mask == RDP_FPU_READWRITE_MASK_FPS)
    {
      int val = extract_unsigned_integer (buf, 4);
      /* this guy is only a word */
      send_rdp ("bbww-SZ", RDP_COPRO_WRITE,
		FPU_COPRO_NUMBER,
		mask, val);
    }
  else
    {
      /* There are 12 bytes long 
         !! fixme about endianness 
       */
      int dummy = 0;
      /* I've seen these come as four words, not the three advertized !! */
      printf ("Sending mask %x\n", mask);
      send_rdp ("bbwwwww-SZ",
		RDP_COPRO_WRITE,
		FPU_COPRO_NUMBER,
		mask,
		*(int *) (buf + 0),
		*(int *) (buf + 4),
		*(int *) (buf + 8),
		0);

      printf ("done mask %x\n", mask);
    }
#endif
}


/* Convert between GDB requests and the RDP layer. */

static void
remote_rdp_fetch_register (regno)
     int regno;
{
  if (regno == -1)
    {
      for (regno = 0; regno < NUM_REGS; regno++)
	remote_rdp_fetch_register (regno);
    }
  else
    {
      char buf[MAX_REGISTER_RAW_SIZE];
      if (regno < 15)
	rdp_fetch_one_register (1 << regno, buf);
      else if (regno == PC_REGNUM)
	rdp_fetch_one_register (RDP_CPU_READWRITE_MASK_PC, buf);
      else if (regno == PS_REGNUM)
	rdp_fetch_one_register (RDP_CPU_READWRITE_MASK_CPSR, buf);
      else if (regno == FPS_REGNUM)
	rdp_fetch_one_fpu_register (RDP_FPU_READWRITE_MASK_FPS, buf);
      else if (regno >= F0_REGNUM && regno <= F7_REGNUM)
	rdp_fetch_one_fpu_register (1 << (regno - F0_REGNUM), buf);
      else
	{
	  printf ("Help me with fetch reg %d\n", regno);
	}
      supply_register (regno, buf);
    }
}


static void
remote_rdp_store_register (regno)
     int regno;
{
  if (regno == -1)
    {
      for (regno = 0; regno < NUM_REGS; regno++)
	remote_rdp_store_register (regno);
    }
  else
    {
      char tmp[MAX_REGISTER_RAW_SIZE];
      read_register_gen (regno, tmp);
      if (regno < 15)
	rdp_store_one_register (1 << regno, tmp);
      else if (regno == PC_REGNUM)
	rdp_store_one_register (RDP_CPU_READWRITE_MASK_PC, tmp);
      else if (regno == PS_REGNUM)
	rdp_store_one_register (RDP_CPU_READWRITE_MASK_CPSR, tmp);
      else if (regno >= F0_REGNUM && regno <= F7_REGNUM)
	rdp_store_one_fpu_register (1 << (regno - F0_REGNUM), tmp);
      else
	{
	  printf ("Help me with reg %d\n", regno);
	}
    }
}

static void
remote_rdp_kill ()
{
  callback->shutdown (callback);
}


static void
rdp_info ()
{
  send_rdp ("bw-S-W-Z", RDP_INFO, RDP_INFO_ABOUT_STEP,
	    &ds.step_info);
  send_rdp ("bw-S-W-Z", RDP_INFO, RDP_INFO_ABOUT_BREAK,
	    &ds.break_info);
  send_rdp ("bw-S-WW-Z", RDP_INFO, RDP_INFO_ABOUT_TARGET,
	    &ds.target_info,
	    &ds.model_info);

  ds.can_step = ds.step_info & RDP_INFO_ABOUT_STEP_1;

  ds.rdi_level = (ds.target_info >> 5) & 3;
}


static void
rdp_execute_start ()
{
  /* Start it off, but don't wait for it */
  send_rdp ("bb-", RDP_EXEC, RDP_EXEC_TYPE_SYNC);
}



#define a_byte 1
#define a_word 2
#define a_string 3


typedef struct
{
  CORE_ADDR n;
  const char *s;
}
argsin;

#define ABYTE 1
#define AWORD 2
#define ASTRING 3
#define ADDRLEN 4

#define SWI_WriteC                      0x0
#define SWI_Write0                      0x2
#define SWI_ReadC                       0x4
#define SWI_CLI                         0x5
#define SWI_GetEnv                      0x10
#define SWI_Exit                        0x11
#define SWI_EnterOS                     0x16

#define SWI_GetErrno                    0x60
#define SWI_Clock                       0x61

#define SWI_Time                        0x63
#define SWI_Remove                      0x64
#define SWI_Rename                      0x65
#define SWI_Open                        0x66

#define SWI_Close                       0x68
#define SWI_Write                       0x69
#define SWI_Read                        0x6a
#define SWI_Seek                        0x6b
#define SWI_Flen                        0x6c

#define SWI_IsTTY                       0x6e
#define SWI_TmpNam                      0x6f
#define SWI_InstallHandler              0x70
#define SWI_GenerateError               0x71


static int
exec_swi (swi, args)
     int swi;
     argsin *args;
{
  int i;
  char c;
  switch (swi)
    {
    case SWI_WriteC:
      callback->write_stdout (callback, &c, 1);
      return 0;
    case SWI_Write0:
      for (i = 0; i < args->n; i++)
	callback->write_stdout (callback, args->s, strlen (args->s));
      return 0;
    case SWI_ReadC:
      callback->read_stdin (callback, &c, 1);
      args->n = c;
      return 1;
    case SWI_CLI:
      args->n = callback->system (callback, args->s);
      return 1;
    case SWI_GetErrno:
      args->n = callback->get_errno (callback);
      return 1;
    case SWI_Time:
      args->n = callback->time (callback, NULL);
      return 1;
    case SWI_Remove:
      args->n = callback->unlink (callback, args->s);
      return 1;
    case SWI_Rename:
      args->n = callback->rename (callback, args[0].s, args[1].s);
      return 1;
    case SWI_Open:
      i = 0;

#ifdef O_BINARY
      if (args[1].n & 1)
	i |= O_BINARY;
#endif
      if (args[1].n & 2)
	i |= O_RDWR;

      if (args[1].n & 4)
	{
	  i |= O_CREAT;
	}

      if (args[1].n & 8)
	i |= O_APPEND;

      args->n = callback->open (callback, args->s, i);
      return 1;

    case SWI_Close:
      args->n = callback->close (callback, args->n);
      return 1;

    case SWI_Write:
      args->n = callback->write (callback, args[0].n, args[1].s, args[1].n);
      return 1;
    case SWI_Read:
      {
	char *copy = alloca (args[2].n);
	int done = callback->read (callback, args[0].n, copy, args[2].n);
	if (done > 0)
	  remote_rdp_xfer_inferior_memory (args[0].n, copy, done, 1, 0);
	args->n -= done;
	return 1;
      }

    case SWI_Seek:
      args->n = callback->lseek (callback, args[0].n, args[1].n, 0) >= 0;
      return 1;
    case SWI_Flen:
      {
	long old = callback->lseek (callback, args->n, 1, 1);
	args->n = callback->lseek (callback, args->n, 2, 0);
	callback->lseek (callback, args->n, old, 0);
	return 1;
      }

    case SWI_IsTTY:
      args->n = callback->isatty (callback, args->n);
      return 1;

    default:
      return 0;
    }
}


static void
handle_swi ()
{
  argsin args[3];
  char *buf;
  int len;
  int count = 0;

  int swino = get_word ();
  int type = get_byte ();
  while (type != 0)
    {
      switch (type & 0x3)
	{
	case ABYTE:
	  args[count].n = get_byte ();
	  break;

	case AWORD:
	  args[count].n = get_word ();
	  break;

	case ASTRING:
	  /* If the word is under 32 bytes it will be sent otherwise
	     an address to it is passed. Also: Special case of 255 */

	  len = get_byte ();
	  if (len > 32)
	    {
	      if (len == 255)
		{
		  len = get_word ();
		}
	      buf = alloca (len);
	      remote_rdp_xfer_inferior_memory (get_word (),
					       buf,
					       len,
					       0,
					       0);
	    }
	  else
	    {
	      int i;
	      buf = alloca (len + 1);
	      for (i = 0; i < len; i++)
		buf[i] = get_byte ();
	      buf[i] = 0;
	    }
	  args[count].n = len;
	  args[count].s = buf;
	  break;

	default:
	  error ("Unimplented SWI argument");
	}

      type = type >> 2;
      count++;
    }

  if (exec_swi (swino, args))
    {
      /* We have two options here reply with either a byte or a word
         which is stored in args[0].n. There is no harm in replying with
         a word all the time, so thats what I do! */
      send_rdp ("bbw-", RDP_OSOpReply, RDP_OSOpWord, args[0].n);
    }
  else
    {
      send_rdp ("bb-", RDP_OSOpReply, RDP_OSOpNothing);
    }
}

static void
rdp_execute_finish ()
{
  int running = 1;

  while (running)
    {
      int res;
      res = SERIAL_READCHAR (io, 1);
      while (res == SERIAL_TIMEOUT)
	{
	  QUIT;
	  printf_filtered ("Waiting for target..\n");
	  res = SERIAL_READCHAR (io, 1);
	}

      switch (res)
	{
	case RDP_RES_SWI:
	  handle_swi ();
	  break;
	case RDP_RES_VALUE:
	  send_rdp ("B", &ds.rdi_stopped_status);
	  running = 0;
	  break;
	case RDP_RESET:
	  printf_filtered ("Target reset\n");
	  running = 0;
	  break;
	default:
	  printf_filtered ("Ignoring %x\n", res);
	  break;
	}
    }
}


static void
rdp_execute ()
{
  rdp_execute_start ();
  rdp_execute_finish ();
}

static int
remote_rdp_insert_breakpoint (addr, save)
     CORE_ADDR addr;
     char *save;
{
  int res;
  if (ds.rdi_level > 0)
    {
      send_rdp ("bwb-SWB",
		RDP_SET_BREAK,
		addr,
		RDP_SET_BREAK_TYPE_PC_EQUAL | RDP_SET_BREAK_TYPE_GET_HANDLE,
		save,
		&res);
    }
  else
    {
      send_rdp ("bwb-SB",
		RDP_SET_BREAK,
		addr,
		RDP_SET_BREAK_TYPE_PC_EQUAL,
		&res);
    }
  return res;
}

static int
remote_rdp_remove_breakpoint (addr, save)
     CORE_ADDR addr;
     char *save;
{
  int res;
  if (ds.rdi_level > 0)
    {
      send_rdp ("b-p-S-B",
		RDP_CLEAR_BREAK,
		save, 4,
		&res);
    }
  else
    {
      send_rdp ("bw-S-B",
		RDP_CLEAR_BREAK,
		addr,
		&res);
    }
  return res;
}

static void
rdp_step ()
{
  if (ds.can_step && 0)
    {
      /* The pie board can't do steps so I can't test this, and
         the other code will always work. */
      int status;
      send_rdp ("bbw-S-B",
		RDP_STEP, 0, 1,
		&status);
    }
  else
    {
      char handle[4];
      CORE_ADDR pc = read_register (PC_REGNUM);
      pc = arm_get_next_pc (pc);
      remote_rdp_insert_breakpoint (pc, &handle);
      rdp_execute ();
      remote_rdp_remove_breakpoint (pc, &handle);
    }
}

static void
remote_rdp_open (args, from_tty)
     char *args;
     int from_tty;
{
  if (!args)
    error_no_arg ("serial port device name");

  baud_rate = 9600;

  target_preopen (from_tty);

  io = SERIAL_OPEN (args);

  if (!io)
    perror_with_name (args);

  SERIAL_RAW (io);

  rdp_init (1, from_tty);


  if (from_tty)
    {
      printf_unfiltered ("Remote RDP debugging using %s at %d baud\n", args, baud_rate);
    }

  rdp_info ();

  push_target (&remote_rdp_ops);

  callback->init (callback);
  flush_cached_frames ();
  registers_changed ();
  stop_pc = read_pc ();
  set_current_frame (create_new_frame (read_fp (), stop_pc));
  select_frame (get_current_frame (), 0);
  print_stack_frame (selected_frame, -1, 1);
}



/* Close out all files and local state before this target loses control. */

static void
remote_rdp_close (quitting)
     int quitting;
{
  callback->shutdown (callback);
  if (io)
    SERIAL_CLOSE (io);
  io = 0;
}


/* Resume execution of the target process.  STEP says whether to single-step
   or to run free; SIGGNAL is the signal value (e.g. SIGINT) to be given
   to the target, or zero for no signal.  */

static void
remote_rdp_resume (pid, step, siggnal)
     int pid, step;
     enum target_signal siggnal;
{
  if (step)
    rdp_step ();
  else
    rdp_execute ();
}

/* Wait for inferior process to do something.  Return pid of child,
   or -1 in case of error; store status through argument pointer STATUS,
   just as `wait' would.  */

static int
remote_rdp_wait (pid, status)
     int pid;
     struct target_waitstatus *status;
{
  switch (ds.rdi_stopped_status)
    {
    default:
    case RDP_RES_RESET:
    case RDP_RES_SWI:
      status->kind = TARGET_WAITKIND_EXITED;
      status->value.integer = read_register (0);
      break;
    case RDP_RES_AT_BREAKPOINT:
      status->kind = TARGET_WAITKIND_STOPPED;
      /* The signal in sigrc is a host signal.  That probably
         should be fixed.  */
      status->value.sig = TARGET_SIGNAL_TRAP;
      break;
#if 0
    case rdp_signalled:
      status->kind = TARGET_WAITKIND_SIGNALLED;
      /* The signal in sigrc is a host signal.  That probably
         should be fixed.  */
      status->value.sig = target_signal_from_host (sigrc);
      break;
#endif
    }

  return inferior_pid;
}

/* Get ready to modify the registers array.  On machines which store
   individual registers, this doesn't need to do anything.  On machines
   which store all the registers in one fell swoop, this makes sure
   that registers contains all the registers from the program being
   debugged.  */

static void
remote_rdp_prepare_to_store ()
{
  /* Do nothing, since we can store individual regs */
}

static int
remote_rdp_xfer_inferior_memory (memaddr, myaddr, len, write, target)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
     int write;
     struct target_ops *target;	/* ignored */
{
  /* I infer from D Taylor's code that there's a limit on the amount
     we can transfer in one chunk.. */
  int done = 0;
  while (done < len)
    {
      int justdone;
      int thisbite = len - done;
      if (thisbite > RDP_MOUTHFULL)
	thisbite = RDP_MOUTHFULL;

      QUIT;

      if (write)
	{
	  justdone = rdp_write (memaddr + done, myaddr + done, thisbite);
	}
      else
	{
	  justdone = rdp_read (memaddr + done, myaddr + done, thisbite);
	}

      done += justdone;

      if (justdone != thisbite)
	break;
    }
  return done;
}



struct yn
{
  const char *name;
  int bit;
};
static struct yn stepinfo[] =
{
  {"Step more than one instruction", RDP_INFO_ABOUT_STEP_GT_1},
  {"Step to jump", RDP_INFO_ABOUT_STEP_TO_JMP},
  {"Step one instruction", RDP_INFO_ABOUT_STEP_1},
  {0}
};

static struct yn breakinfo[] =
{
  {"comparison breakpoints supported", RDP_INFO_ABOUT_BREAK_COMP},
  {"range breakpoints supported", RDP_INFO_ABOUT_BREAK_RANGE},
  {"watchpoints for byte reads supported", RDP_INFO_ABOUT_BREAK_BYTE_READ},
  {"watchpoints for half-word reads supported", RDP_INFO_ABOUT_BREAK_HALFWORD_READ},
  {"watchpoints for word reads supported", RDP_INFO_ABOUT_BREAK_WORD_READ},
  {"watchpoints for byte writes supported", RDP_INFO_ABOUT_BREAK_BYTE_WRITE},
  {"watchpoints for half-word writes supported", RDP_INFO_ABOUT_BREAK_HALFWORD_WRITE},
  {"watchpoints for word writes supported", RDP_INFO_ABOUT_BREAK_WORD_WRITE},
  {"mask break/watch-points supported", RDP_INFO_ABOUT_BREAK_MASK},
{"thread-specific breakpoints supported", RDP_INFO_ABOUT_BREAK_THREAD_BREAK},
{"thread-specific watchpoints supported", RDP_INFO_ABOUT_BREAK_THREAD_WATCH},
  {"conditional breakpoints supported", RDP_INFO_ABOUT_BREAK_COND},
  {0}
};


static void
dump_bits (t, info)
     struct yn *t;
     int info;
{
  while (t->name)
    {
      printf_unfiltered ("  %-45s : %s\n", t->name, (info & t->bit) ? "Yes" : "No");
      t++;
    }
}

static void
remote_rdp_files_info (target)
     struct target_ops *target;
{
  printf_filtered ("Target capabilities:\n");
  dump_bits (stepinfo, ds.step_info);
  dump_bits (breakinfo, ds.break_info);
  printf_unfiltered ("target level RDI %x\n", (ds.target_info >> 5) & 3);
}


/* Define the target subroutine names */

struct target_ops remote_rdp_ops =
{
  "rdp",			/* to_shortname */
  /* to_longname */
  "Remote Target using the RDProtocol",
  /* to_doc */
  "Use a remote ARM system which uses the ARM Remote Debugging Protocol",
  remote_rdp_open,		/* to_open */
  remote_rdp_close,		/* to_close */
  NULL,				/* to_attach */
  NULL,				/* to_detach */
  remote_rdp_resume,		/* to_resume */
  remote_rdp_wait,		/* to_wait */
  remote_rdp_fetch_register,	/* to_fetch_registers */
  remote_rdp_store_register,	/* to_store_registers */
  remote_rdp_prepare_to_store,	/* to_prepare_to_store */
  remote_rdp_xfer_inferior_memory,	/* to_xfer_memory */
  remote_rdp_files_info,	/* to_files_info */
  remote_rdp_insert_breakpoint,	/* to_insert_breakpoint */
  remote_rdp_remove_breakpoint,	/* to_remove_breakpoint */
  NULL,				/* to_terminal_init */
  NULL,				/* to_terminal_inferior */
  NULL,				/* to_terminal_ours_for_output */
  NULL,				/* to_terminal_ours */
  NULL,				/* to_terminal_info */
  remote_rdp_kill,		/* to_kill */
  generic_load,			/* to_load */
  NULL,				/* to_lookup_symbol */
  NULL,				/* to_create_inferior */
  generic_mourn_inferior,	/* to_mourn_inferior */
  0,				/* to_can_run */
  0,				/* to_notice_signals */
  0,				/* to_thread_alive */
  0,				/* to_stop */
  process_stratum,		/* to_stratum */
  NULL,				/* to_next */
  1,				/* to_has_all_memory */
  1,				/* to_has_memory */
  1,				/* to_has_stack */
  1,				/* to_has_registers */
  1,				/* to_has_execution */
  NULL,				/* sections */
  NULL,				/* sections_end */
  OPS_MAGIC,			/* to_magic */
};

void
_initialize_remote_rdp ()
{
  add_target (&remote_rdp_ops);
}
