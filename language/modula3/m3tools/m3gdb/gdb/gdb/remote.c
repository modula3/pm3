/* Remote target communications for serial-line targets in custom GDB protocol
   Copyright 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

/* See the GDB User Guide for details of the GDB remote protocol. */

#include "defs.h"
#include "gdb_string.h"
#include <ctype.h>
#include <fcntl.h>
#include "inferior.h"
#include "bfd.h"
#include "symfile.h"
#include "target.h"
/*#include "terminal.h" */
#include "gdbcmd.h"
#include "objfiles.h"
#include "gdb-stabs.h"
#include "gdbthread.h"
#include "remote.h"
#include "regcache.h"

#include <ctype.h>
#include <sys/time.h>
#ifdef USG
#include <sys/types.h>
#endif

#include "event-loop.h"
#include "event-top.h"
#include "inf-loop.h"

#include <signal.h>
#include "serial.h"

#include "gdbcore.h" /* for exec_bfd */

/* Prototypes for local functions */
static void cleanup_sigint_signal_handler (void *dummy);
static void initialize_sigint_signal_handler (void);
static int getpkt_sane (char *buf, long sizeof_buf, int forever);

static void handle_remote_sigint (int);
static void handle_remote_sigint_twice (int);
static void async_remote_interrupt (gdb_client_data);
void async_remote_interrupt_twice (gdb_client_data);

static void build_remote_gdbarch_data (void);

static int remote_write_bytes (CORE_ADDR memaddr, char *myaddr, int len);

static int remote_read_bytes (CORE_ADDR memaddr, char *myaddr, int len);

static void remote_files_info (struct target_ops *ignore);

static int remote_xfer_memory (CORE_ADDR memaddr, char *myaddr,
			       int len, int should_write,
			       struct mem_attrib *attrib,
			       struct target_ops *target);

static void remote_prepare_to_store (void);

static void remote_fetch_registers (int regno);

static void remote_resume (ptid_t ptid, int step,
                           enum target_signal siggnal);
static void remote_async_resume (ptid_t ptid, int step,
				 enum target_signal siggnal);
static int remote_start_remote (PTR);

static void remote_open (char *name, int from_tty);
static void remote_async_open (char *name, int from_tty);

static void extended_remote_open (char *name, int from_tty);
static void extended_remote_async_open (char *name, int from_tty);

static void remote_open_1 (char *, int, struct target_ops *, int extended_p);
static void remote_async_open_1 (char *, int, struct target_ops *,
				 int extended_p);

static void remote_close (int quitting);

static void remote_store_registers (int regno);

static void remote_mourn (void);
static void remote_async_mourn (void);

static void extended_remote_restart (void);

static void extended_remote_mourn (void);

static void extended_remote_create_inferior (char *, char *, char **);
static void extended_remote_async_create_inferior (char *, char *, char **);

static void remote_mourn_1 (struct target_ops *);

static void remote_send (char *buf, long sizeof_buf);

static int readchar (int timeout);

static ptid_t remote_wait (ptid_t ptid,
                                 struct target_waitstatus *status);
static ptid_t remote_async_wait (ptid_t ptid,
                                       struct target_waitstatus *status);

static void remote_kill (void);
static void remote_async_kill (void);

static int tohex (int nib);

static void remote_detach (char *args, int from_tty);
static void remote_async_detach (char *args, int from_tty);

static void remote_interrupt (int signo);

static void remote_interrupt_twice (int signo);

static void interrupt_query (void);

static void set_thread (int, int);

static int remote_thread_alive (ptid_t);

static void get_offsets (void);

static long read_frame (char *buf, long sizeof_buf);

static int remote_insert_breakpoint (CORE_ADDR, char *);

static int remote_remove_breakpoint (CORE_ADDR, char *);

static int hexnumlen (ULONGEST num);

static void init_remote_ops (void);

static void init_extended_remote_ops (void);

static void init_remote_cisco_ops (void);

static struct target_ops remote_cisco_ops;

static void remote_stop (void);

static int ishex (int ch, int *val);

static int stubhex (int ch);

static int remote_query (int /*char */ , char *, char *, int *);

static int hexnumstr (char *, ULONGEST);

static int hexnumnstr (char *, ULONGEST, int);

static CORE_ADDR remote_address_masked (CORE_ADDR);

static void print_packet (char *);

static unsigned long crc32 (unsigned char *, int, unsigned int);

static void compare_sections_command (char *, int);

static void packet_command (char *, int);

static int stub_unpack_int (char *buff, int fieldlength);

static ptid_t remote_current_thread (ptid_t oldptid);

static void remote_find_new_threads (void);

static void record_currthread (int currthread);

static int fromhex (int a);

static int hex2bin (const char *hex, char *bin, int count);

static int bin2hex (const char *bin, char *hex, int count);

static int putpkt_binary (char *buf, int cnt);

static void check_binary_download (CORE_ADDR addr);

struct packet_config;

static void show_packet_config_cmd (struct packet_config *config);

static void update_packet_config (struct packet_config *config);

/* Define the target subroutine names */

void open_remote_target (char *, int, struct target_ops *, int);

void _initialize_remote (void);

/* */

static struct target_ops remote_ops;

static struct target_ops extended_remote_ops;

/* Temporary target ops. Just like the remote_ops and
   extended_remote_ops, but with asynchronous support. */
static struct target_ops remote_async_ops;

static struct target_ops extended_async_remote_ops;

/* FIXME: cagney/1999-09-23: Even though getpkt was called with
   ``forever'' still use the normal timeout mechanism.  This is
   currently used by the ASYNC code to guarentee that target reads
   during the initial connect always time-out.  Once getpkt has been
   modified to return a timeout indication and, in turn
   remote_wait()/wait_for_inferior() have gained a timeout parameter
   this can go away. */
static int wait_forever_enabled_p = 1;


/* This variable chooses whether to send a ^C or a break when the user
   requests program interruption.  Although ^C is usually what remote
   systems expect, and that is the default here, sometimes a break is
   preferable instead.  */

static int remote_break;

/* Descriptor for I/O to remote machine.  Initialize it to NULL so that
   remote_open knows that we don't have a file open when the program
   starts.  */
static struct serial *remote_desc = NULL;

/* This is set by the target (thru the 'S' message)
   to denote that the target is in kernel mode.  */
static int cisco_kernel_mode = 0;

/* This variable sets the number of bits in an address that are to be
   sent in a memory ("M" or "m") packet.  Normally, after stripping
   leading zeros, the entire address would be sent. This variable
   restricts the address to REMOTE_ADDRESS_SIZE bits.  HISTORY: The
   initial implementation of remote.c restricted the address sent in
   memory packets to ``host::sizeof long'' bytes - (typically 32
   bits).  Consequently, for 64 bit targets, the upper 32 bits of an
   address was never sent.  Since fixing this bug may cause a break in
   some remote targets this variable is principly provided to
   facilitate backward compatibility. */

static int remote_address_size;

/* Tempoary to track who currently owns the terminal.  See
   target_async_terminal_* for more details.  */

static int remote_async_terminal_ours_p;


/* This is the size (in chars) of the first response to the ``g''
   packet.  It is used as a heuristic when determining the maximum
   size of memory-read and memory-write packets.  A target will
   typically only reserve a buffer large enough to hold the ``g''
   packet.  The size does not include packet overhead (headers and
   trailers). */

static long actual_register_packet_size;

/* This is the maximum size (in chars) of a non read/write packet.  It
   is also used as a cap on the size of read/write packets. */

static long remote_packet_size;
/* compatibility. */
#define PBUFSIZ (remote_packet_size)

/* User configurable variables for the number of characters in a
   memory read/write packet.  MIN (PBUFSIZ, g-packet-size) is the
   default.  Some targets need smaller values (fifo overruns, et.al.)
   and some users need larger values (speed up transfers).  The
   variables ``preferred_*'' (the user request), ``current_*'' (what
   was actually set) and ``forced_*'' (Positive - a soft limit,
   negative - a hard limit). */

struct memory_packet_config
{
  char *name;
  long size;
  int fixed_p;
};

/* Compute the current size of a read/write packet.  Since this makes
   use of ``actual_register_packet_size'' the computation is dynamic.  */

static long
get_memory_packet_size (struct memory_packet_config *config)
{
  /* NOTE: The somewhat arbitrary 16k comes from the knowledge (folk
     law?) that some hosts don't cope very well with large alloca()
     calls.  Eventually the alloca() code will be replaced by calls to
     xmalloc() and make_cleanups() allowing this restriction to either
     be lifted or removed. */
#ifndef MAX_REMOTE_PACKET_SIZE
#define MAX_REMOTE_PACKET_SIZE 16384
#endif
  /* NOTE: 16 is just chosen at random. */
#ifndef MIN_REMOTE_PACKET_SIZE
#define MIN_REMOTE_PACKET_SIZE 16
#endif
  long what_they_get;
  if (config->fixed_p)
    {
      if (config->size <= 0)
	what_they_get = MAX_REMOTE_PACKET_SIZE;
      else
	what_they_get = config->size;
    }
  else
    {
      what_they_get = remote_packet_size;
      /* Limit the packet to the size specified by the user. */
      if (config->size > 0
	  && what_they_get > config->size)
	what_they_get = config->size;
      /* Limit it to the size of the targets ``g'' response. */
      if (actual_register_packet_size > 0
	  && what_they_get > actual_register_packet_size)
	what_they_get = actual_register_packet_size;
    }
  if (what_they_get > MAX_REMOTE_PACKET_SIZE)
    what_they_get = MAX_REMOTE_PACKET_SIZE;
  if (what_they_get < MIN_REMOTE_PACKET_SIZE)
    what_they_get = MIN_REMOTE_PACKET_SIZE;
  return what_they_get;
}

/* Update the size of a read/write packet. If they user wants
   something really big then do a sanity check. */

static void
set_memory_packet_size (char *args, struct memory_packet_config *config)
{
  int fixed_p = config->fixed_p;
  long size = config->size;
  if (args == NULL)
    error ("Argument required (integer, `fixed' or `limited').");
  else if (strcmp (args, "hard") == 0
      || strcmp (args, "fixed") == 0)
    fixed_p = 1;
  else if (strcmp (args, "soft") == 0
	   || strcmp (args, "limit") == 0)
    fixed_p = 0;
  else
    {
      char *end;
      size = strtoul (args, &end, 0);
      if (args == end)
	error ("Invalid %s (bad syntax).", config->name);
#if 0
      /* Instead of explicitly capping the size of a packet to
         MAX_REMOTE_PACKET_SIZE or dissallowing it, the user is
         instead allowed to set the size to something arbitrarily
         large. */
      if (size > MAX_REMOTE_PACKET_SIZE)
	error ("Invalid %s (too large).", config->name);
#endif
    }
  /* Extra checks? */
  if (fixed_p && !config->fixed_p)
    {
      if (! query ("The target may not be able to correctly handle a %s\n"
		   "of %ld bytes. Change the packet size? ",
		   config->name, size))
	error ("Packet size not changed.");
    }
  /* Update the config. */
  config->fixed_p = fixed_p;
  config->size = size;
}

static void
show_memory_packet_size (struct memory_packet_config *config)
{
  printf_filtered ("The %s is %ld. ", config->name, config->size);
  if (config->fixed_p)
    printf_filtered ("Packets are fixed at %ld bytes.\n",
		     get_memory_packet_size (config));
  else
    printf_filtered ("Packets are limited to %ld bytes.\n",
		     get_memory_packet_size (config));
}

static struct memory_packet_config memory_write_packet_config =
{
  "memory-write-packet-size",
};

static void
set_memory_write_packet_size (char *args, int from_tty)
{
  set_memory_packet_size (args, &memory_write_packet_config);
}

static void
show_memory_write_packet_size (char *args, int from_tty)
{
  show_memory_packet_size (&memory_write_packet_config);
}

static long
get_memory_write_packet_size (void)
{
  return get_memory_packet_size (&memory_write_packet_config);
}

static struct memory_packet_config memory_read_packet_config =
{
  "memory-read-packet-size",
};

static void
set_memory_read_packet_size (char *args, int from_tty)
{
  set_memory_packet_size (args, &memory_read_packet_config);
}

static void
show_memory_read_packet_size (char *args, int from_tty)
{
  show_memory_packet_size (&memory_read_packet_config);
}

static long
get_memory_read_packet_size (void)
{
  long size = get_memory_packet_size (&memory_read_packet_config);
  /* FIXME: cagney/1999-11-07: Functions like getpkt() need to get an
     extra buffer size argument before the memory read size can be
     increased beyond PBUFSIZ. */
  if (size > PBUFSIZ)
    size = PBUFSIZ;
  return size;
}

/* Register packet size initialization. Since the bounds change when
   the architecture changes (namely REGISTER_BYTES) this all needs to
   be multi-arched.  */

static void
register_remote_packet_sizes (void)
{
  REGISTER_GDBARCH_SWAP (remote_packet_size);
  REGISTER_GDBARCH_SWAP (actual_register_packet_size);
}

static void
build_remote_packet_sizes (void)
{
  /* Default maximum number of characters in a packet body. Many
     remote stubs have a hardwired buffer size of 400 bytes
     (c.f. BUFMAX in m68k-stub.c and i386-stub.c).  BUFMAX-1 is used
     as the maximum packet-size to ensure that the packet and an extra
     NUL character can always fit in the buffer.  This stops GDB
     trashing stubs that try to squeeze an extra NUL into what is
     already a full buffer (As of 1999-12-04 that was most stubs. */
  remote_packet_size = 400 - 1;
  /* Should REGISTER_BYTES needs more space than the default, adjust
     the size accordingly. Remember that each byte is encoded as two
     characters. 32 is the overhead for the packet header /
     footer. NOTE: cagney/1999-10-26: I suspect that 8
     (``$NN:G...#NN'') is a better guess, the below has been padded a
     little. */
  if (REGISTER_BYTES > ((remote_packet_size - 32) / 2))
    remote_packet_size = (REGISTER_BYTES * 2 + 32);
  
  /* This one is filled in when a ``g'' packet is received. */
  actual_register_packet_size = 0;
}

/* Generic configuration support for packets the stub optionally
   supports. Allows the user to specify the use of the packet as well
   as allowing GDB to auto-detect support in the remote stub. */

enum packet_support
  {
    PACKET_SUPPORT_UNKNOWN = 0,
    PACKET_ENABLE,
    PACKET_DISABLE
  };

struct packet_config
  {
    char *name;
    char *title;
    enum cmd_auto_boolean detect;
    enum packet_support support;
  };

/* Analyze a packet's return value and update the packet config
   accordingly. */

enum packet_result
{
  PACKET_ERROR,
  PACKET_OK,
  PACKET_UNKNOWN
};

static void
update_packet_config (struct packet_config *config)
{
  switch (config->detect)
    {
    case CMD_AUTO_BOOLEAN_TRUE:
      config->support = PACKET_ENABLE;
      break;
    case CMD_AUTO_BOOLEAN_FALSE:
      config->support = PACKET_DISABLE;
      break;
    case CMD_AUTO_BOOLEAN_AUTO:
      config->support = PACKET_SUPPORT_UNKNOWN;
      break;
    }
}

static void
show_packet_config_cmd (struct packet_config *config)
{
  char *support = "internal-error";
  switch (config->support)
    {
    case PACKET_ENABLE:
      support = "enabled";
      break;
    case PACKET_DISABLE:
      support = "disabled";
      break;
    case PACKET_SUPPORT_UNKNOWN:
      support = "unknown";
      break;
    }
  switch (config->detect)
    {
    case CMD_AUTO_BOOLEAN_AUTO:
      printf_filtered ("Support for remote protocol `%s' (%s) packet is auto-detected, currently %s.\n",
		       config->name, config->title, support);
      break;
    case CMD_AUTO_BOOLEAN_TRUE:
    case CMD_AUTO_BOOLEAN_FALSE:
      printf_filtered ("Support for remote protocol `%s' (%s) packet is currently %s.\n",
		       config->name, config->title, support);
      break;
    }
}

static void
add_packet_config_cmd (struct packet_config *config,
		       char *name,
		       char *title,
		       void (*set_func) (char *args, int from_tty,
					 struct cmd_list_element *
					 c),
		       void (*show_func) (char *name,
					  int from_tty),
		       struct cmd_list_element **set_remote_list,
		       struct cmd_list_element **show_remote_list,
		       int legacy)
{
  struct cmd_list_element *set_cmd;
  struct cmd_list_element *show_cmd;
  char *set_doc;
  char *show_doc;
  char *cmd_name;
  config->name = name;
  config->title = title;
  config->detect = CMD_AUTO_BOOLEAN_AUTO;
  config->support = PACKET_SUPPORT_UNKNOWN;
  xasprintf (&set_doc, "Set use of remote protocol `%s' (%s) packet",
	     name, title);
  xasprintf (&show_doc, "Show current use of remote protocol `%s' (%s) packet",
	     name, title);
  /* set/show TITLE-packet {auto,on,off} */
  xasprintf (&cmd_name, "%s-packet", title);
  set_cmd = add_set_auto_boolean_cmd (cmd_name, class_obscure,
				&config->detect, set_doc,
				set_remote_list);
  set_cmd->function.sfunc = set_func;
  show_cmd = add_cmd (cmd_name, class_obscure, show_func, show_doc,
		      show_remote_list);
  /* set/show remote NAME-packet {auto,on,off} -- legacy */
  if (legacy)
    {
      char *legacy_name;
      xasprintf (&legacy_name, "%s-packet", name);
      add_alias_cmd (legacy_name, cmd_name, class_obscure, 0,
		     set_remote_list);
      add_alias_cmd (legacy_name, cmd_name, class_obscure, 0,
		     show_remote_list);
    }
}

static enum packet_result
packet_ok (const char *buf, struct packet_config *config)
{
  if (buf[0] != '\0')
    {
      /* The stub recognized the packet request.  Check that the
	 operation succeeded. */
      switch (config->support)
	{
	case PACKET_SUPPORT_UNKNOWN:
	  if (remote_debug)
	    fprintf_unfiltered (gdb_stdlog,
				    "Packet %s (%s) is supported\n",
				    config->name, config->title);
	  config->support = PACKET_ENABLE;
	  break;
	case PACKET_DISABLE:
	  internal_error (__FILE__, __LINE__,
			  "packet_ok: attempt to use a disabled packet");
	  break;
	case PACKET_ENABLE:
	  break;
	}
      if (buf[0] == 'O' && buf[1] == 'K' && buf[2] == '\0')
	/* "OK" - definitly OK. */
	return PACKET_OK;
      if (buf[0] == 'E'
	  && isxdigit (buf[1]) && isxdigit (buf[2])
	  && buf[3] == '\0')
	/* "Enn"  - definitly an error. */
	return PACKET_ERROR;
      /* The packet may or may not be OK.  Just assume it is */
      return PACKET_OK;
    }
  else
    {
      /* The stub does not support the packet. */
      switch (config->support)
	{
	case PACKET_ENABLE:
	  if (config->detect == CMD_AUTO_BOOLEAN_AUTO)
	    /* If the stub previously indicated that the packet was
	       supported then there is a protocol error.. */
	    error ("Protocol error: %s (%s) conflicting enabled responses.",
		   config->name, config->title);
	  else
	    /* The user set it wrong. */
	    error ("Enabled packet %s (%s) not recognized by stub",
		   config->name, config->title);
	  break;
	case PACKET_SUPPORT_UNKNOWN:
	  if (remote_debug)
	    fprintf_unfiltered (gdb_stdlog,
				"Packet %s (%s) is NOT supported\n",
				config->name, config->title);
	  config->support = PACKET_DISABLE;
	  break;
	case PACKET_DISABLE:
	  break;
	}
      return PACKET_UNKNOWN;
    }
}

/* Should we try the 'qSymbol' (target symbol lookup service) request? */
static struct packet_config remote_protocol_qSymbol;

static void
set_remote_protocol_qSymbol_packet_cmd (char *args, int from_tty,
				  struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_qSymbol);
}

static void
show_remote_protocol_qSymbol_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_qSymbol);
}

/* Should we try the 'e' (step over range) request? */
static struct packet_config remote_protocol_e;

static void
set_remote_protocol_e_packet_cmd (char *args, int from_tty,
				  struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_e);
}

static void
show_remote_protocol_e_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_e);
}
  

/* Should we try the 'E' (step over range / w signal #) request? */
static struct packet_config remote_protocol_E;

static void
set_remote_protocol_E_packet_cmd (char *args, int from_tty,
				  struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_E);
}

static void
show_remote_protocol_E_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_E);
}
  

/* Should we try the 'P' (set register) request?  */

static struct packet_config remote_protocol_P;

static void
set_remote_protocol_P_packet_cmd (char *args, int from_tty,
				  struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_P);
}

static void
show_remote_protocol_P_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_P);
}

/* Should we try one of the 'Z' requests?  */

enum Z_packet_type
{
  Z_PACKET_SOFTWARE_BP,
  Z_PACKET_HARDWARE_BP,
  Z_PACKET_WRITE_WP,
  Z_PACKET_READ_WP,
  Z_PACKET_ACCESS_WP,
  NR_Z_PACKET_TYPES
};

static struct packet_config remote_protocol_Z[NR_Z_PACKET_TYPES];

/* FIXME: Instead of having all these boiler plate functions, the
   command callback should include a context argument. */

static void
set_remote_protocol_Z_software_bp_packet_cmd (char *args, int from_tty,
					      struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_Z[Z_PACKET_SOFTWARE_BP]);
}

static void
show_remote_protocol_Z_software_bp_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_Z[Z_PACKET_SOFTWARE_BP]);
}

static void
set_remote_protocol_Z_hardware_bp_packet_cmd (char *args, int from_tty,
					      struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_Z[Z_PACKET_HARDWARE_BP]);
}

static void
show_remote_protocol_Z_hardware_bp_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_Z[Z_PACKET_HARDWARE_BP]);
}

static void
set_remote_protocol_Z_write_wp_packet_cmd (char *args, int from_tty,
					      struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_Z[Z_PACKET_WRITE_WP]);
}

static void
show_remote_protocol_Z_write_wp_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_Z[Z_PACKET_WRITE_WP]);
}

static void
set_remote_protocol_Z_read_wp_packet_cmd (char *args, int from_tty,
					      struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_Z[Z_PACKET_READ_WP]);
}

static void
show_remote_protocol_Z_read_wp_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_Z[Z_PACKET_READ_WP]);
}

static void
set_remote_protocol_Z_access_wp_packet_cmd (char *args, int from_tty,
					      struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_Z[Z_PACKET_ACCESS_WP]);
}

static void
show_remote_protocol_Z_access_wp_packet_cmd (char *args, int from_tty)
{
  show_packet_config_cmd (&remote_protocol_Z[Z_PACKET_ACCESS_WP]);
}

/* For compatibility with older distributions.  Provide a ``set remote
   Z-packet ...'' command that updates all the Z packet types. */

static enum cmd_auto_boolean remote_Z_packet_detect;

static void
set_remote_protocol_Z_packet_cmd (char *args, int from_tty,
				  struct cmd_list_element *c)
{
  int i;
  for (i = 0; i < NR_Z_PACKET_TYPES; i++)
    {
      remote_protocol_Z[i].detect = remote_Z_packet_detect;
      update_packet_config (&remote_protocol_Z[i]);
    }
}

static void
show_remote_protocol_Z_packet_cmd (char *args, int from_tty)
{
  int i;
  for (i = 0; i < NR_Z_PACKET_TYPES; i++)
    {
      show_packet_config_cmd (&remote_protocol_Z[i]);
    }
}

/* Should we try the 'X' (remote binary download) packet?

   This variable (available to the user via "set remote X-packet")
   dictates whether downloads are sent in binary (via the 'X' packet).
   We assume that the stub can, and attempt to do it. This will be
   cleared if the stub does not understand it. This switch is still
   needed, though in cases when the packet is supported in the stub,
   but the connection does not allow it (i.e., 7-bit serial connection
   only). */

static struct packet_config remote_protocol_binary_download;

/* Should we try the 'ThreadInfo' query packet?

   This variable (NOT available to the user: auto-detect only!)
   determines whether GDB will use the new, simpler "ThreadInfo"
   query or the older, more complex syntax for thread queries.
   This is an auto-detect variable (set to true at each connect, 
   and set to false when the target fails to recognize it).  */

static int use_threadinfo_query;
static int use_threadextra_query;

static void
set_remote_protocol_binary_download_cmd (char *args,
					 int from_tty,
					 struct cmd_list_element *c)
{
  update_packet_config (&remote_protocol_binary_download);
}

static void
show_remote_protocol_binary_download_cmd (char *args,
					  int from_tty)
{
  show_packet_config_cmd (&remote_protocol_binary_download);
}


/* Tokens for use by the asynchronous signal handlers for SIGINT */
PTR sigint_remote_twice_token;
PTR sigint_remote_token;

/* These are pointers to hook functions that may be set in order to
   modify resume/wait behavior for a particular architecture.  */

void (*target_resume_hook) (void);
void (*target_wait_loop_hook) (void);



/* These are the threads which we last sent to the remote system.
   -1 for all or -2 for not sent yet.  */
static int general_thread;
static int continue_thread;

/* Call this function as a result of
   1) A halt indication (T packet) containing a thread id
   2) A direct query of currthread
   3) Successful execution of set thread
 */

static void
record_currthread (int currthread)
{
  general_thread = currthread;

  /* If this is a new thread, add it to GDB's thread list.
     If we leave it up to WFI to do this, bad things will happen.  */
  if (!in_thread_list (pid_to_ptid (currthread)))
    {
      add_thread (pid_to_ptid (currthread));
#ifdef UI_OUT
      ui_out_text (uiout, "[New ");
      ui_out_text (uiout, target_pid_to_str (pid_to_ptid (currthread)));
      ui_out_text (uiout, "]\n");
#else
      printf_filtered ("[New %s]\n",
                       target_pid_to_str (pid_to_ptid (currthread)));
#endif
    }
}

#define MAGIC_NULL_PID 42000

static void
set_thread (int th, int gen)
{
  char *buf = alloca (PBUFSIZ);
  int state = gen ? general_thread : continue_thread;

  if (state == th)
    return;

  buf[0] = 'H';
  buf[1] = gen ? 'g' : 'c';
  if (th == MAGIC_NULL_PID)
    {
      buf[2] = '0';
      buf[3] = '\0';
    }
  else if (th < 0)
    sprintf (&buf[2], "-%x", -th);
  else
    sprintf (&buf[2], "%x", th);
  putpkt (buf);
  getpkt (buf, PBUFSIZ, 0);
  if (gen)
    general_thread = th;
  else
    continue_thread = th;
}

/*  Return nonzero if the thread TH is still alive on the remote system.  */

static int
remote_thread_alive (ptid_t ptid)
{
  int tid = PIDGET (ptid);
  char buf[16];

  if (tid < 0)
    sprintf (buf, "T-%08x", -tid);
  else
    sprintf (buf, "T%08x", tid);
  putpkt (buf);
  getpkt (buf, sizeof (buf), 0);
  return (buf[0] == 'O' && buf[1] == 'K');
}

/* About these extended threadlist and threadinfo packets.  They are
   variable length packets but, the fields within them are often fixed
   length.  They are redundent enough to send over UDP as is the
   remote protocol in general.  There is a matching unit test module
   in libstub.  */

#define OPAQUETHREADBYTES 8

/* a 64 bit opaque identifier */
typedef unsigned char threadref[OPAQUETHREADBYTES];

/* WARNING: This threadref data structure comes from the remote O.S., libstub
   protocol encoding, and remote.c. it is not particularly changable */

/* Right now, the internal structure is int. We want it to be bigger.
   Plan to fix this.
 */

typedef int gdb_threadref;	/* internal GDB thread reference */

/* gdb_ext_thread_info is an internal GDB data structure which is
   equivalint to the reply of the remote threadinfo packet */

struct gdb_ext_thread_info
  {
    threadref threadid;		/* External form of thread reference */
    int active;			/* Has state interesting to GDB? , regs, stack */
    char display[256];		/* Brief state display, name, blocked/syspended */
    char shortname[32];		/* To be used to name threads */
    char more_display[256];	/* Long info, statistics, queue depth, whatever */
  };

/* The volume of remote transfers can be limited by submitting
   a mask containing bits specifying the desired information.
   Use a union of these values as the 'selection' parameter to
   get_thread_info. FIXME: Make these TAG names more thread specific.
 */

#define TAG_THREADID 1
#define TAG_EXISTS 2
#define TAG_DISPLAY 4
#define TAG_THREADNAME 8
#define TAG_MOREDISPLAY 16

#define BUF_THREAD_ID_SIZE (OPAQUETHREADBYTES*2)

char *unpack_varlen_hex (char *buff, int *result);

static char *unpack_nibble (char *buf, int *val);

static char *pack_nibble (char *buf, int nibble);

static char *pack_hex_byte (char *pkt, int /*unsigned char */ byte);

static char *unpack_byte (char *buf, int *value);

static char *pack_int (char *buf, int value);

static char *unpack_int (char *buf, int *value);

static char *unpack_string (char *src, char *dest, int length);

static char *pack_threadid (char *pkt, threadref * id);

static char *unpack_threadid (char *inbuf, threadref * id);

void int_to_threadref (threadref * id, int value);

static int threadref_to_int (threadref * ref);

static void copy_threadref (threadref * dest, threadref * src);

static int threadmatch (threadref * dest, threadref * src);

static char *pack_threadinfo_request (char *pkt, int mode, threadref * id);

static int remote_unpack_thread_info_response (char *pkt,
					       threadref * expectedref,
					       struct gdb_ext_thread_info
					       *info);


static int remote_get_threadinfo (threadref * threadid, int fieldset,	/*TAG mask */
				  struct gdb_ext_thread_info *info);

static int adapt_remote_get_threadinfo (gdb_threadref * ref,
					int selection,
					struct gdb_ext_thread_info *info);

static char *pack_threadlist_request (char *pkt, int startflag,
				      int threadcount,
				      threadref * nextthread);

static int parse_threadlist_response (char *pkt,
				      int result_limit,
				      threadref * original_echo,
				      threadref * resultlist, int *doneflag);

static int remote_get_threadlist (int startflag,
				  threadref * nextthread,
				  int result_limit,
				  int *done,
				  int *result_count, threadref * threadlist);

typedef int (*rmt_thread_action) (threadref * ref, void *context);

static int remote_threadlist_iterator (rmt_thread_action stepfunction,
				       void *context, int looplimit);

static int remote_newthread_step (threadref * ref, void *context);

/* encode 64 bits in 16 chars of hex */

static const char hexchars[] = "0123456789abcdef";

static int
ishex (int ch, int *val)
{
  if ((ch >= 'a') && (ch <= 'f'))
    {
      *val = ch - 'a' + 10;
      return 1;
    }
  if ((ch >= 'A') && (ch <= 'F'))
    {
      *val = ch - 'A' + 10;
      return 1;
    }
  if ((ch >= '0') && (ch <= '9'))
    {
      *val = ch - '0';
      return 1;
    }
  return 0;
}

static int
stubhex (int ch)
{
  if (ch >= 'a' && ch <= 'f')
    return ch - 'a' + 10;
  if (ch >= '0' && ch <= '9')
    return ch - '0';
  if (ch >= 'A' && ch <= 'F')
    return ch - 'A' + 10;
  return -1;
}

static int
stub_unpack_int (char *buff, int fieldlength)
{
  int nibble;
  int retval = 0;

  while (fieldlength)
    {
      nibble = stubhex (*buff++);
      retval |= nibble;
      fieldlength--;
      if (fieldlength)
	retval = retval << 4;
    }
  return retval;
}

char *
unpack_varlen_hex (char *buff,	/* packet to parse */
		   int *result)
{
  int nibble;
  int retval = 0;

  while (ishex (*buff, &nibble))
    {
      buff++;
      retval = retval << 4;
      retval |= nibble & 0x0f;
    }
  *result = retval;
  return buff;
}

static char *
unpack_nibble (char *buf, int *val)
{
  ishex (*buf++, val);
  return buf;
}

static char *
pack_nibble (char *buf, int nibble)
{
  *buf++ = hexchars[(nibble & 0x0f)];
  return buf;
}

static char *
pack_hex_byte (char *pkt, int byte)
{
  *pkt++ = hexchars[(byte >> 4) & 0xf];
  *pkt++ = hexchars[(byte & 0xf)];
  return pkt;
}

static char *
unpack_byte (char *buf, int *value)
{
  *value = stub_unpack_int (buf, 2);
  return buf + 2;
}

static char *
pack_int (char *buf, int value)
{
  buf = pack_hex_byte (buf, (value >> 24) & 0xff);
  buf = pack_hex_byte (buf, (value >> 16) & 0xff);
  buf = pack_hex_byte (buf, (value >> 8) & 0x0ff);
  buf = pack_hex_byte (buf, (value & 0xff));
  return buf;
}

static char *
unpack_int (char *buf, int *value)
{
  *value = stub_unpack_int (buf, 8);
  return buf + 8;
}

#if 0				/* currently unused, uncomment when needed */
static char *pack_string (char *pkt, char *string);

static char *
pack_string (char *pkt, char *string)
{
  char ch;
  int len;

  len = strlen (string);
  if (len > 200)
    len = 200;			/* Bigger than most GDB packets, junk??? */
  pkt = pack_hex_byte (pkt, len);
  while (len-- > 0)
    {
      ch = *string++;
      if ((ch == '\0') || (ch == '#'))
	ch = '*';		/* Protect encapsulation */
      *pkt++ = ch;
    }
  return pkt;
}
#endif /* 0 (unused) */

static char *
unpack_string (char *src, char *dest, int length)
{
  while (length--)
    *dest++ = *src++;
  *dest = '\0';
  return src;
}

static char *
pack_threadid (char *pkt, threadref *id)
{
  char *limit;
  unsigned char *altid;

  altid = (unsigned char *) id;
  limit = pkt + BUF_THREAD_ID_SIZE;
  while (pkt < limit)
    pkt = pack_hex_byte (pkt, *altid++);
  return pkt;
}


static char *
unpack_threadid (char *inbuf, threadref *id)
{
  char *altref;
  char *limit = inbuf + BUF_THREAD_ID_SIZE;
  int x, y;

  altref = (char *) id;

  while (inbuf < limit)
    {
      x = stubhex (*inbuf++);
      y = stubhex (*inbuf++);
      *altref++ = (x << 4) | y;
    }
  return inbuf;
}

/* Externally, threadrefs are 64 bits but internally, they are still
   ints. This is due to a mismatch of specifications.  We would like
   to use 64bit thread references internally.  This is an adapter
   function.  */

void
int_to_threadref (threadref *id, int value)
{
  unsigned char *scan;

  scan = (unsigned char *) id;
  {
    int i = 4;
    while (i--)
      *scan++ = 0;
  }
  *scan++ = (value >> 24) & 0xff;
  *scan++ = (value >> 16) & 0xff;
  *scan++ = (value >> 8) & 0xff;
  *scan++ = (value & 0xff);
}

static int
threadref_to_int (threadref *ref)
{
  int i, value = 0;
  unsigned char *scan;

  scan = (char *) ref;
  scan += 4;
  i = 4;
  while (i-- > 0)
    value = (value << 8) | ((*scan++) & 0xff);
  return value;
}

static void
copy_threadref (threadref *dest, threadref *src)
{
  int i;
  unsigned char *csrc, *cdest;

  csrc = (unsigned char *) src;
  cdest = (unsigned char *) dest;
  i = 8;
  while (i--)
    *cdest++ = *csrc++;
}

static int
threadmatch (threadref *dest, threadref *src)
{
  /* things are broken right now, so just assume we got a match */
#if 0
  unsigned char *srcp, *destp;
  int i, result;
  srcp = (char *) src;
  destp = (char *) dest;

  result = 1;
  while (i-- > 0)
    result &= (*srcp++ == *destp++) ? 1 : 0;
  return result;
#endif
  return 1;
}

/*
   threadid:1,        # always request threadid
   context_exists:2,
   display:4,
   unique_name:8,
   more_display:16
 */

/* Encoding:  'Q':8,'P':8,mask:32,threadid:64 */

static char *
pack_threadinfo_request (char *pkt, int mode, threadref *id)
{
  *pkt++ = 'q';			/* Info Query */
  *pkt++ = 'P';			/* process or thread info */
  pkt = pack_int (pkt, mode);	/* mode */
  pkt = pack_threadid (pkt, id);	/* threadid */
  *pkt = '\0';			/* terminate */
  return pkt;
}

/* These values tag the fields in a thread info response packet */
/* Tagging the fields allows us to request specific fields and to
   add more fields as time goes by */

#define TAG_THREADID 1		/* Echo the thread identifier */
#define TAG_EXISTS 2		/* Is this process defined enough to
				   fetch registers and its stack */
#define TAG_DISPLAY 4		/* A short thing maybe to put on a window */
#define TAG_THREADNAME 8	/* string, maps 1-to-1 with a thread is */
#define TAG_MOREDISPLAY 16	/* Whatever the kernel wants to say about 
				   the process */

static int
remote_unpack_thread_info_response (char *pkt, threadref *expectedref,
				    struct gdb_ext_thread_info *info)
{
  int mask, length;
  unsigned int tag;
  threadref ref;
  char *limit = pkt + PBUFSIZ;	/* plausable parsing limit */
  int retval = 1;

  /* info->threadid = 0; FIXME: implement zero_threadref */
  info->active = 0;
  info->display[0] = '\0';
  info->shortname[0] = '\0';
  info->more_display[0] = '\0';

  /* Assume the characters indicating the packet type have been stripped */
  pkt = unpack_int (pkt, &mask);	/* arg mask */
  pkt = unpack_threadid (pkt, &ref);

  if (mask == 0)
    warning ("Incomplete response to threadinfo request\n");
  if (!threadmatch (&ref, expectedref))
    {				/* This is an answer to a different request */
      warning ("ERROR RMT Thread info mismatch\n");
      return 0;
    }
  copy_threadref (&info->threadid, &ref);

  /* Loop on tagged fields , try to bail if somthing goes wrong */

  while ((pkt < limit) && mask && *pkt)		/* packets are terminated with nulls */
    {
      pkt = unpack_int (pkt, &tag);	/* tag */
      pkt = unpack_byte (pkt, &length);		/* length */
      if (!(tag & mask))	/* tags out of synch with mask */
	{
	  warning ("ERROR RMT: threadinfo tag mismatch\n");
	  retval = 0;
	  break;
	}
      if (tag == TAG_THREADID)
	{
	  if (length != 16)
	    {
	      warning ("ERROR RMT: length of threadid is not 16\n");
	      retval = 0;
	      break;
	    }
	  pkt = unpack_threadid (pkt, &ref);
	  mask = mask & ~TAG_THREADID;
	  continue;
	}
      if (tag == TAG_EXISTS)
	{
	  info->active = stub_unpack_int (pkt, length);
	  pkt += length;
	  mask = mask & ~(TAG_EXISTS);
	  if (length > 8)
	    {
	      warning ("ERROR RMT: 'exists' length too long\n");
	      retval = 0;
	      break;
	    }
	  continue;
	}
      if (tag == TAG_THREADNAME)
	{
	  pkt = unpack_string (pkt, &info->shortname[0], length);
	  mask = mask & ~TAG_THREADNAME;
	  continue;
	}
      if (tag == TAG_DISPLAY)
	{
	  pkt = unpack_string (pkt, &info->display[0], length);
	  mask = mask & ~TAG_DISPLAY;
	  continue;
	}
      if (tag == TAG_MOREDISPLAY)
	{
	  pkt = unpack_string (pkt, &info->more_display[0], length);
	  mask = mask & ~TAG_MOREDISPLAY;
	  continue;
	}
      warning ("ERROR RMT: unknown thread info tag\n");
      break;			/* Not a tag we know about */
    }
  return retval;
}

static int
remote_get_threadinfo (threadref *threadid, int fieldset,	/* TAG mask */
		       struct gdb_ext_thread_info *info)
{
  int result;
  char *threadinfo_pkt = alloca (PBUFSIZ);

  pack_threadinfo_request (threadinfo_pkt, fieldset, threadid);
  putpkt (threadinfo_pkt);
  getpkt (threadinfo_pkt, PBUFSIZ, 0);
  result = remote_unpack_thread_info_response (threadinfo_pkt + 2, threadid,
					       info);
  return result;
}

/* Unfortunately, 61 bit thread-ids are bigger than the internal
   representation of a threadid.  */

static int
adapt_remote_get_threadinfo (gdb_threadref *ref, int selection,
			     struct gdb_ext_thread_info *info)
{
  threadref lclref;

  int_to_threadref (&lclref, *ref);
  return remote_get_threadinfo (&lclref, selection, info);
}

/*    Format: i'Q':8,i"L":8,initflag:8,batchsize:16,lastthreadid:32   */

static char *
pack_threadlist_request (char *pkt, int startflag, int threadcount,
			 threadref *nextthread)
{
  *pkt++ = 'q';			/* info query packet */
  *pkt++ = 'L';			/* Process LIST or threadLIST request */
  pkt = pack_nibble (pkt, startflag);	/* initflag 1 bytes */
  pkt = pack_hex_byte (pkt, threadcount);	/* threadcount 2 bytes */
  pkt = pack_threadid (pkt, nextthread);	/* 64 bit thread identifier */
  *pkt = '\0';
  return pkt;
}

/* Encoding:   'q':8,'M':8,count:16,done:8,argthreadid:64,(threadid:64)* */

static int
parse_threadlist_response (char *pkt, int result_limit,
			   threadref *original_echo, threadref *resultlist,
			   int *doneflag)
{
  char *limit;
  int count, resultcount, done;

  resultcount = 0;
  /* Assume the 'q' and 'M chars have been stripped.  */
  limit = pkt + (PBUFSIZ - BUF_THREAD_ID_SIZE);		/* done parse past here */
  pkt = unpack_byte (pkt, &count);	/* count field */
  pkt = unpack_nibble (pkt, &done);
  /* The first threadid is the argument threadid.  */
  pkt = unpack_threadid (pkt, original_echo);	/* should match query packet */
  while ((count-- > 0) && (pkt < limit))
    {
      pkt = unpack_threadid (pkt, resultlist++);
      if (resultcount++ >= result_limit)
	break;
    }
  if (doneflag)
    *doneflag = done;
  return resultcount;
}

static int
remote_get_threadlist (int startflag, threadref *nextthread, int result_limit,
		       int *done, int *result_count, threadref *threadlist)
{
  static threadref echo_nextthread;
  char *threadlist_packet = alloca (PBUFSIZ);
  char *t_response = alloca (PBUFSIZ);
  int result = 1;

  /* Trancate result limit to be smaller than the packet size */
  if ((((result_limit + 1) * BUF_THREAD_ID_SIZE) + 10) >= PBUFSIZ)
    result_limit = (PBUFSIZ / BUF_THREAD_ID_SIZE) - 2;

  pack_threadlist_request (threadlist_packet,
			   startflag, result_limit, nextthread);
  putpkt (threadlist_packet);
  getpkt (t_response, PBUFSIZ, 0);

  *result_count =
    parse_threadlist_response (t_response + 2, result_limit, &echo_nextthread,
			       threadlist, done);

  if (!threadmatch (&echo_nextthread, nextthread))
    {
      /* FIXME: This is a good reason to drop the packet */
      /* Possably, there is a duplicate response */
      /* Possabilities :
         retransmit immediatly - race conditions
         retransmit after timeout - yes
         exit
         wait for packet, then exit
       */
      warning ("HMM: threadlist did not echo arg thread, dropping it\n");
      return 0;			/* I choose simply exiting */
    }
  if (*result_count <= 0)
    {
      if (*done != 1)
	{
	  warning ("RMT ERROR : failed to get remote thread list\n");
	  result = 0;
	}
      return result;		/* break; */
    }
  if (*result_count > result_limit)
    {
      *result_count = 0;
      warning ("RMT ERROR: threadlist response longer than requested\n");
      return 0;
    }
  return result;
}

/* This is the interface between remote and threads, remotes upper interface */

/* remote_find_new_threads retrieves the thread list and for each
   thread in the list, looks up the thread in GDB's internal list,
   ading the thread if it does not already exist.  This involves
   getting partial thread lists from the remote target so, polling the
   quit_flag is required.  */


/* About this many threadisds fit in a packet. */

#define MAXTHREADLISTRESULTS 32

static int
remote_threadlist_iterator (rmt_thread_action stepfunction, void *context,
			    int looplimit)
{
  int done, i, result_count;
  int startflag = 1;
  int result = 1;
  int loopcount = 0;
  static threadref nextthread;
  static threadref resultthreadlist[MAXTHREADLISTRESULTS];

  done = 0;
  while (!done)
    {
      if (loopcount++ > looplimit)
	{
	  result = 0;
	  warning ("Remote fetch threadlist -infinite loop-\n");
	  break;
	}
      if (!remote_get_threadlist (startflag, &nextthread, MAXTHREADLISTRESULTS,
				  &done, &result_count, resultthreadlist))
	{
	  result = 0;
	  break;
	}
      /* clear for later iterations */
      startflag = 0;
      /* Setup to resume next batch of thread references, set nextthread.  */
      if (result_count >= 1)
	copy_threadref (&nextthread, &resultthreadlist[result_count - 1]);
      i = 0;
      while (result_count--)
	if (!(result = (*stepfunction) (&resultthreadlist[i++], context)))
	  break;
    }
  return result;
}

static int
remote_newthread_step (threadref *ref, void *context)
{
  ptid_t ptid;

  ptid = pid_to_ptid (threadref_to_int (ref));

  if (!in_thread_list (ptid))
    add_thread (ptid);
  return 1;			/* continue iterator */
}

#define CRAZY_MAX_THREADS 1000

static ptid_t
remote_current_thread (ptid_t oldpid)
{
  char *buf = alloca (PBUFSIZ);

  putpkt ("qC");
  getpkt (buf, PBUFSIZ, 0);
  if (buf[0] == 'Q' && buf[1] == 'C')
    return pid_to_ptid (strtol (&buf[2], NULL, 16));
  else
    return oldpid;
}

/* Find new threads for info threads command.  
 * Original version, using John Metzler's thread protocol.  
 */

static void
remote_find_new_threads (void)
{
  remote_threadlist_iterator (remote_newthread_step, 0,
			      CRAZY_MAX_THREADS);
  if (PIDGET (inferior_ptid) == MAGIC_NULL_PID)	/* ack ack ack */
    inferior_ptid = remote_current_thread (inferior_ptid);
}

/*
 * Find all threads for info threads command.
 * Uses new thread protocol contributed by Cisco.
 * Falls back and attempts to use the older method (above)
 * if the target doesn't respond to the new method.
 */

static void
remote_threads_info (void)
{
  char *buf = alloca (PBUFSIZ);
  char *bufp;
  int tid;

  if (remote_desc == 0)		/* paranoia */
    error ("Command can only be used when connected to the remote target.");

  if (use_threadinfo_query)
    {
      putpkt ("qfThreadInfo");
      bufp = buf;
      getpkt (bufp, PBUFSIZ, 0);
      if (bufp[0] != '\0')		/* q packet recognized */
	{	
	  while (*bufp++ == 'm')	/* reply contains one or more TID */
	    {
	      do
		{
		  tid = strtol (bufp, &bufp, 16);
		  if (tid != 0 && !in_thread_list (pid_to_ptid (tid)))
		    add_thread (pid_to_ptid (tid));
		}
	      while (*bufp++ == ',');	/* comma-separated list */
	      putpkt ("qsThreadInfo");
	      bufp = buf;
	      getpkt (bufp, PBUFSIZ, 0);
	    }
	  return;	/* done */
	}
    }

  /* Else fall back to old method based on jmetzler protocol. */
  use_threadinfo_query = 0;
  remote_find_new_threads ();
  return;
}

/* 
 * Collect a descriptive string about the given thread.
 * The target may say anything it wants to about the thread
 * (typically info about its blocked / runnable state, name, etc.).
 * This string will appear in the info threads display.
 * 
 * Optional: targets are not required to implement this function.
 */

static char *
remote_threads_extra_info (struct thread_info *tp)
{
  int result;
  int set;
  threadref id;
  struct gdb_ext_thread_info threadinfo;
  static char display_buf[100];	/* arbitrary... */
  char *bufp = alloca (PBUFSIZ);
  int n = 0;                    /* position in display_buf */

  if (remote_desc == 0)		/* paranoia */
    internal_error (__FILE__, __LINE__,
		    "remote_threads_extra_info");

  if (use_threadextra_query)
    {
      sprintf (bufp, "qThreadExtraInfo,%x", PIDGET (tp->ptid));
      putpkt (bufp);
      getpkt (bufp, PBUFSIZ, 0);
      if (bufp[0] != 0)
	{
	  n = min (strlen (bufp) / 2, sizeof (display_buf));
	  result = hex2bin (bufp, display_buf, n);
	  display_buf [result] = '\0';
	  return display_buf;
	}
    }

  /* If the above query fails, fall back to the old method.  */
  use_threadextra_query = 0;
  set = TAG_THREADID | TAG_EXISTS | TAG_THREADNAME
    | TAG_MOREDISPLAY | TAG_DISPLAY;
  int_to_threadref (&id, PIDGET (tp->ptid));
  if (remote_get_threadinfo (&id, set, &threadinfo))
    if (threadinfo.active)
      {
	if (*threadinfo.shortname)
	  n += sprintf(&display_buf[0], " Name: %s,", threadinfo.shortname);
	if (*threadinfo.display)
	  n += sprintf(&display_buf[n], " State: %s,", threadinfo.display);
	if (*threadinfo.more_display)
	  n += sprintf(&display_buf[n], " Priority: %s",
		       threadinfo.more_display);

	if (n > 0)
	  {
	    /* for purely cosmetic reasons, clear up trailing commas */
	    if (',' == display_buf[n-1])
	      display_buf[n-1] = ' ';
	    return display_buf;
	  }
      }
  return NULL;
}



/*  Restart the remote side; this is an extended protocol operation.  */

static void
extended_remote_restart (void)
{
  char *buf = alloca (PBUFSIZ);

  /* Send the restart command; for reasons I don't understand the
     remote side really expects a number after the "R".  */
  buf[0] = 'R';
  sprintf (&buf[1], "%x", 0);
  putpkt (buf);

  /* Now query for status so this looks just like we restarted
     gdbserver from scratch.  */
  putpkt ("?");
  getpkt (buf, PBUFSIZ, 0);
}

/* Clean up connection to a remote debugger.  */

/* ARGSUSED */
static void
remote_close (int quitting)
{
  if (remote_desc)
    serial_close (remote_desc);
  remote_desc = NULL;
}

/* Query the remote side for the text, data and bss offsets. */

static void
get_offsets (void)
{
  char *buf = alloca (PBUFSIZ);
  char *ptr;
  int lose;
  CORE_ADDR text_addr, data_addr, bss_addr;
  struct section_offsets *offs;

  putpkt ("qOffsets");

  getpkt (buf, PBUFSIZ, 0);

  if (buf[0] == '\000')
    return;			/* Return silently.  Stub doesn't support
				   this command. */
  if (buf[0] == 'E')
    {
      warning ("Remote failure reply: %s", buf);
      return;
    }

  /* Pick up each field in turn.  This used to be done with scanf, but
     scanf will make trouble if CORE_ADDR size doesn't match
     conversion directives correctly.  The following code will work
     with any size of CORE_ADDR.  */
  text_addr = data_addr = bss_addr = 0;
  ptr = buf;
  lose = 0;

  if (strncmp (ptr, "Text=", 5) == 0)
    {
      ptr += 5;
      /* Don't use strtol, could lose on big values.  */
      while (*ptr && *ptr != ';')
	text_addr = (text_addr << 4) + fromhex (*ptr++);
    }
  else
    lose = 1;

  if (!lose && strncmp (ptr, ";Data=", 6) == 0)
    {
      ptr += 6;
      while (*ptr && *ptr != ';')
	data_addr = (data_addr << 4) + fromhex (*ptr++);
    }
  else
    lose = 1;

  if (!lose && strncmp (ptr, ";Bss=", 5) == 0)
    {
      ptr += 5;
      while (*ptr && *ptr != ';')
	bss_addr = (bss_addr << 4) + fromhex (*ptr++);
    }
  else
    lose = 1;

  if (lose)
    error ("Malformed response to offset query, %s", buf);

  if (symfile_objfile == NULL)
    return;

  offs = (struct section_offsets *) alloca (SIZEOF_SECTION_OFFSETS);
  memcpy (offs, symfile_objfile->section_offsets, SIZEOF_SECTION_OFFSETS);

  offs->offsets[SECT_OFF_TEXT (symfile_objfile)] = text_addr;

  /* This is a temporary kludge to force data and bss to use the same offsets
     because that's what nlmconv does now.  The real solution requires changes
     to the stub and remote.c that I don't have time to do right now.  */

  offs->offsets[SECT_OFF_DATA (symfile_objfile)] = data_addr;
  offs->offsets[SECT_OFF_BSS (symfile_objfile)] = data_addr;

  objfile_relocate (symfile_objfile, offs);
}

/*
 * Cisco version of section offsets:
 *
 * Instead of having GDB query the target for the section offsets,
 * Cisco lets the target volunteer the information!  It's also in
 * a different format, so here are the functions that will decode
 * a section offset packet from a Cisco target.
 */

/* 
 * Function: remote_cisco_section_offsets
 *
 * Returns:  zero for success, non-zero for failure 
 */

static int
remote_cisco_section_offsets (bfd_vma text_addr,
			      bfd_vma data_addr,
			      bfd_vma bss_addr,
			      bfd_signed_vma *text_offs,
			      bfd_signed_vma *data_offs,
			      bfd_signed_vma *bss_offs)
{
  bfd_vma text_base, data_base, bss_base;
  struct minimal_symbol *start;
  asection *sect;
  bfd *abfd;
  int len;

  if (symfile_objfile == NULL)
    return -1;			/* no can do nothin' */

  start = lookup_minimal_symbol ("_start", NULL, NULL);
  if (start == NULL)
    return -1;			/* Can't find "_start" symbol */

  data_base = bss_base = 0;
  text_base = SYMBOL_VALUE_ADDRESS (start);

  abfd = symfile_objfile->obfd;
  for (sect = abfd->sections;
       sect != 0;
       sect = sect->next)
    {
      const char *p = bfd_get_section_name (abfd, sect);
      len = strlen (p);
      if (strcmp (p + len - 4, "data") == 0)	/* ends in "data" */
	if (data_base == 0 ||
	    data_base > bfd_get_section_vma (abfd, sect))
	  data_base = bfd_get_section_vma (abfd, sect);
      if (strcmp (p + len - 3, "bss") == 0)	/* ends in "bss" */
	if (bss_base == 0 ||
	    bss_base > bfd_get_section_vma (abfd, sect))
	  bss_base = bfd_get_section_vma (abfd, sect);
    }
  *text_offs = text_addr - text_base;
  *data_offs = data_addr - data_base;
  *bss_offs = bss_addr - bss_base;
  if (remote_debug)
    {
      char tmp[128];

      sprintf (tmp, "VMA:          text = 0x");
      sprintf_vma (tmp + strlen (tmp), text_addr);
      sprintf (tmp + strlen (tmp), " data = 0x");
      sprintf_vma (tmp + strlen (tmp), data_addr);
      sprintf (tmp + strlen (tmp), " bss = 0x");
      sprintf_vma (tmp + strlen (tmp), bss_addr);
      fprintf_filtered (gdb_stdlog, tmp);
      fprintf_filtered (gdb_stdlog,
			"Reloc offset: text = 0x%s data = 0x%s bss = 0x%s\n",
			paddr_nz (*text_offs),
			paddr_nz (*data_offs),
			paddr_nz (*bss_offs));
    }

  return 0;
}

/*
 * Function: remote_cisco_objfile_relocate
 *
 * Relocate the symbol file for a remote target. 
 */

void
remote_cisco_objfile_relocate (bfd_signed_vma text_off, bfd_signed_vma data_off,
			       bfd_signed_vma bss_off)
{
  struct section_offsets *offs;

  if (text_off != 0 || data_off != 0 || bss_off != 0)
    {
      /* FIXME: This code assumes gdb-stabs.h is being used; it's
         broken for xcoff, dwarf, sdb-coff, etc.  But there is no
         simple canonical representation for this stuff.  */

      offs = (struct section_offsets *) alloca (SIZEOF_SECTION_OFFSETS);
      memcpy (offs, symfile_objfile->section_offsets, SIZEOF_SECTION_OFFSETS);

      offs->offsets[SECT_OFF_TEXT (symfile_objfile)] = text_off;
      offs->offsets[SECT_OFF_DATA (symfile_objfile)] = data_off;
      offs->offsets[SECT_OFF_BSS (symfile_objfile)] = bss_off;

      /* First call the standard objfile_relocate.  */
      objfile_relocate (symfile_objfile, offs);

      /* Now we need to fix up the section entries already attached to
         the exec target.  These entries will control memory transfers
         from the exec file.  */

      exec_set_section_offsets (text_off, data_off, bss_off);
    }
}

/* Stub for catch_errors.  */

static int
remote_start_remote_dummy (void *dummy)
{
  start_remote ();		/* Initialize gdb process mechanisms */
  return 1;
}

static int
remote_start_remote (PTR dummy)
{
  immediate_quit++;		/* Allow user to interrupt it */

  /* Ack any packet which the remote side has already sent.  */
  serial_write (remote_desc, "+", 1);

  /* Let the stub know that we want it to return the thread.  */
  set_thread (-1, 0);

  inferior_ptid = remote_current_thread (inferior_ptid);

  get_offsets ();		/* Get text, data & bss offsets */

  putpkt ("?");			/* initiate a query from remote machine */
  immediate_quit--;

  return remote_start_remote_dummy (dummy);
}

/* Open a connection to a remote debugger.
   NAME is the filename used for communication.  */

static void
remote_open (char *name, int from_tty)
{
  remote_open_1 (name, from_tty, &remote_ops, 0);
}

/* Just like remote_open, but with asynchronous support. */
static void
remote_async_open (char *name, int from_tty)
{
  remote_async_open_1 (name, from_tty, &remote_async_ops, 0);
}

/* Open a connection to a remote debugger using the extended
   remote gdb protocol.  NAME is the filename used for communication.  */

static void
extended_remote_open (char *name, int from_tty)
{
  remote_open_1 (name, from_tty, &extended_remote_ops, 1 /*extended_p */ );
}

/* Just like extended_remote_open, but with asynchronous support. */
static void
extended_remote_async_open (char *name, int from_tty)
{
  remote_async_open_1 (name, from_tty, &extended_async_remote_ops, 1 /*extended_p */ );
}

/* Generic code for opening a connection to a remote target.  */

static void
init_all_packet_configs (void)
{
  int i;
  update_packet_config (&remote_protocol_e);
  update_packet_config (&remote_protocol_E);
  update_packet_config (&remote_protocol_P);
  update_packet_config (&remote_protocol_qSymbol);
  for (i = 0; i < NR_Z_PACKET_TYPES; i++)
    update_packet_config (&remote_protocol_Z[i]);
  /* Force remote_write_bytes to check whether target supports binary
     downloading. */
  update_packet_config (&remote_protocol_binary_download);
}

/* Symbol look-up. */

static void
remote_check_symbols (struct objfile *objfile)
{
  char *msg, *reply, *tmp;
  struct minimal_symbol *sym;
  int end;

  if (remote_protocol_qSymbol.support == PACKET_DISABLE)
    return;

  msg   = alloca (PBUFSIZ);
  reply = alloca (PBUFSIZ);

  /* Invite target to request symbol lookups. */

  putpkt ("qSymbol::");
  getpkt (reply, PBUFSIZ, 0);
  packet_ok (reply, &remote_protocol_qSymbol);

  while (strncmp (reply, "qSymbol:", 8) == 0)
    {
      tmp = &reply[8];
      end = hex2bin (tmp, msg, strlen (tmp) / 2);
      msg[end] = '\0';
      sym = lookup_minimal_symbol (msg, NULL, NULL);
      if (sym == NULL)
	sprintf (msg, "qSymbol::%s", &reply[8]);
      else
	sprintf (msg, "qSymbol:%s:%s", 
		 paddr_nz (SYMBOL_VALUE_ADDRESS (sym)),
		 &reply[8]);
      putpkt (msg);
      getpkt (reply, PBUFSIZ, 0);
    }
}

static void
remote_open_1 (char *name, int from_tty, struct target_ops *target,
	       int extended_p)
{
  if (name == 0)
    error ("To open a remote debug connection, you need to specify what\n\
serial device is attached to the remote system\n\
(e.g. /dev/ttyS0, /dev/ttya, COM1, etc.).");

  /* See FIXME above */
  wait_forever_enabled_p = 1;

  target_preopen (from_tty);

  unpush_target (target);

  remote_desc = serial_open (name);
  if (!remote_desc)
    perror_with_name (name);

  if (baud_rate != -1)
    {
      if (serial_setbaudrate (remote_desc, baud_rate))
	{
	  serial_close (remote_desc);
	  perror_with_name (name);
	}
    }

  serial_raw (remote_desc);

  /* If there is something sitting in the buffer we might take it as a
     response to a command, which would be bad.  */
  serial_flush_input (remote_desc);

  if (from_tty)
    {
      puts_filtered ("Remote debugging using ");
      puts_filtered (name);
      puts_filtered ("\n");
    }
  push_target (target);		/* Switch to using remote target now */

  init_all_packet_configs ();
  
  general_thread = -2;
  continue_thread = -2;

  /* Probe for ability to use "ThreadInfo" query, as required.  */
  use_threadinfo_query = 1;
  use_threadextra_query = 1;

  /* Without this, some commands which require an active target (such
     as kill) won't work.  This variable serves (at least) double duty
     as both the pid of the target process (if it has such), and as a
     flag indicating that a target is active.  These functions should
     be split out into seperate variables, especially since GDB will
     someday have a notion of debugging several processes.  */

  inferior_ptid = pid_to_ptid (MAGIC_NULL_PID);
#ifdef SOLIB_CREATE_INFERIOR_HOOK
  /* First delete any symbols previously loaded from shared libraries. */
  no_shared_libraries (NULL, 0);
#endif

  /* Start the remote connection; if error (0), discard this target.
     In particular, if the user quits, be sure to discard it
     (we'd be in an inconsistent state otherwise).  */
  if (!catch_errors (remote_start_remote, NULL,
		     "Couldn't establish connection to remote target\n",
		     RETURN_MASK_ALL))
    {
      pop_target ();
      return;
    }

  if (extended_p)
    {
      /* Tell the remote that we are using the extended protocol.  */
      char *buf = alloca (PBUFSIZ);
      putpkt ("!");
      getpkt (buf, PBUFSIZ, 0);
    }
#ifdef SOLIB_CREATE_INFERIOR_HOOK
  /* FIXME: need a master target_open vector from which all 
     remote_opens can be called, so that stuff like this can 
     go there.  Failing that, the following code must be copied
     to the open function for any remote target that wants to 
     support svr4 shared libraries.  */

  /* Set up to detect and load shared libraries. */
  if (exec_bfd) 	/* No use without an exec file. */
    {
      SOLIB_CREATE_INFERIOR_HOOK (PIDGET (inferior_ptid));
      remote_check_symbols (symfile_objfile);
    }
#endif
}

/* Just like remote_open but with asynchronous support. */
static void
remote_async_open_1 (char *name, int from_tty, struct target_ops *target,
		     int extended_p)
{
  if (name == 0)
    error ("To open a remote debug connection, you need to specify what\n\
serial device is attached to the remote system\n\
(e.g. /dev/ttyS0, /dev/ttya, COM1, etc.).");

  target_preopen (from_tty);

  unpush_target (target);

  remote_desc = serial_open (name);
  if (!remote_desc)
    perror_with_name (name);

  if (baud_rate != -1)
    {
      if (serial_setbaudrate (remote_desc, baud_rate))
	{
	  serial_close (remote_desc);
	  perror_with_name (name);
	}
    }

  serial_raw (remote_desc);

  /* If there is something sitting in the buffer we might take it as a
     response to a command, which would be bad.  */
  serial_flush_input (remote_desc);

  if (from_tty)
    {
      puts_filtered ("Remote debugging using ");
      puts_filtered (name);
      puts_filtered ("\n");
    }

  push_target (target);		/* Switch to using remote target now */

  init_all_packet_configs ();

  general_thread = -2;
  continue_thread = -2;

  /* Probe for ability to use "ThreadInfo" query, as required.  */
  use_threadinfo_query = 1;
  use_threadextra_query = 1;

  /* Without this, some commands which require an active target (such
     as kill) won't work.  This variable serves (at least) double duty
     as both the pid of the target process (if it has such), and as a
     flag indicating that a target is active.  These functions should
     be split out into seperate variables, especially since GDB will
     someday have a notion of debugging several processes.  */
  inferior_ptid = pid_to_ptid (MAGIC_NULL_PID);

  /* With this target we start out by owning the terminal. */
  remote_async_terminal_ours_p = 1;

  /* FIXME: cagney/1999-09-23: During the initial connection it is
     assumed that the target is already ready and able to respond to
     requests. Unfortunately remote_start_remote() eventually calls
     wait_for_inferior() with no timeout.  wait_forever_enabled_p gets
     around this. Eventually a mechanism that allows
     wait_for_inferior() to expect/get timeouts will be
     implemented. */
  wait_forever_enabled_p = 0;

#ifdef SOLIB_CREATE_INFERIOR_HOOK
  /* First delete any symbols previously loaded from shared libraries. */
  no_shared_libraries (NULL, 0);
#endif

  /* Start the remote connection; if error (0), discard this target.
     In particular, if the user quits, be sure to discard it
     (we'd be in an inconsistent state otherwise).  */
  if (!catch_errors (remote_start_remote, NULL,
		     "Couldn't establish connection to remote target\n",
		     RETURN_MASK_ALL))
    {
      pop_target ();
      wait_forever_enabled_p = 1;
      return;
    }

  wait_forever_enabled_p = 1;

  if (extended_p)
    {
      /* Tell the remote that we are using the extended protocol.  */
      char *buf = alloca (PBUFSIZ);
      putpkt ("!");
      getpkt (buf, PBUFSIZ, 0);
    }
#ifdef SOLIB_CREATE_INFERIOR_HOOK
  /* FIXME: need a master target_open vector from which all 
     remote_opens can be called, so that stuff like this can 
     go there.  Failing that, the following code must be copied
     to the open function for any remote target that wants to 
     support svr4 shared libraries.  */

  /* Set up to detect and load shared libraries. */
  if (exec_bfd) 	/* No use without an exec file. */
    {
      SOLIB_CREATE_INFERIOR_HOOK (PIDGET (inferior_ptid));
      remote_check_symbols (symfile_objfile);
    }
#endif
}

/* This takes a program previously attached to and detaches it.  After
   this is done, GDB can be used to debug some other program.  We
   better not have left any breakpoints in the target program or it'll
   die when it hits one.  */

static void
remote_detach (char *args, int from_tty)
{
  char *buf = alloca (PBUFSIZ);

  if (args)
    error ("Argument given to \"detach\" when remotely debugging.");

  /* Tell the remote target to detach.  */
  strcpy (buf, "D");
  remote_send (buf, PBUFSIZ);

  target_mourn_inferior ();
  if (from_tty)
    puts_filtered ("Ending remote debugging.\n");

}

/* Same as remote_detach, but with async support. */
static void
remote_async_detach (char *args, int from_tty)
{
  char *buf = alloca (PBUFSIZ);

  if (args)
    error ("Argument given to \"detach\" when remotely debugging.");

  /* Tell the remote target to detach.  */
  strcpy (buf, "D");
  remote_send (buf, PBUFSIZ);

  /* Unregister the file descriptor from the event loop. */
  if (target_is_async_p ())
    serial_async (remote_desc, NULL, 0);

  target_mourn_inferior ();
  if (from_tty)
    puts_filtered ("Ending remote debugging.\n");
}

/* Convert hex digit A to a number.  */

static int
fromhex (int a)
{
  if (a >= '0' && a <= '9')
    return a - '0';
  else if (a >= 'a' && a <= 'f')
    return a - 'a' + 10;
  else if (a >= 'A' && a <= 'F')
    return a - 'A' + 10;
  else
    error ("Reply contains invalid hex digit %d", a);
}

static int
hex2bin (const char *hex, char *bin, int count)
{
  int i;

  for (i = 0; i < count; i++)
    {
      if (hex[0] == 0 || hex[1] == 0)
	{
	  /* Hex string is short, or of uneven length.
	     Return the count that has been converted so far. */
	  return i;
	}
      *bin++ = fromhex (hex[0]) * 16 + fromhex (hex[1]);
      hex += 2;
    }
  return i;
}

/* Convert number NIB to a hex digit.  */

static int
tohex (int nib)
{
  if (nib < 10)
    return '0' + nib;
  else
    return 'a' + nib - 10;
}

static int
bin2hex (const char *bin, char *hex, int count)
{
  int i;
  /* May use a length, or a nul-terminated string as input. */
  if (count == 0)
    count = strlen (bin);

  for (i = 0; i < count; i++)
    {
      *hex++ = tohex ((*bin >> 4) & 0xf);
      *hex++ = tohex (*bin++ & 0xf);
    }
  *hex = 0;
  return i;
}

/* Tell the remote machine to resume.  */

static enum target_signal last_sent_signal = TARGET_SIGNAL_0;

static int last_sent_step;

static void
remote_resume (ptid_t ptid, int step, enum target_signal siggnal)
{
  char *buf = alloca (PBUFSIZ);
  int pid = PIDGET (ptid);
  char *p;

  if (pid == -1)
    set_thread (0, 0);		/* run any thread */
  else
    set_thread (pid, 0);	/* run this thread */

  last_sent_signal = siggnal;
  last_sent_step = step;

  /* A hook for when we need to do something at the last moment before
     resumption.  */
  if (target_resume_hook)
    (*target_resume_hook) ();


  /* The s/S/c/C packets do not return status.  So if the target does
     not support the S or C packets, the debug agent returns an empty
     string which is detected in remote_wait().  This protocol defect
     is fixed in the e/E packets. */

  if (step && step_range_end)
    {
      /* If the target does not support the 'E' packet, we try the 'S'
	 packet.  Ideally we would fall back to the 'e' packet if that
	 too is not supported.  But that would require another copy of
	 the code to issue the 'e' packet (and fall back to 's' if not
	 supported) in remote_wait().  */
      
      if (siggnal != TARGET_SIGNAL_0)
	{
	  if (remote_protocol_E.support != PACKET_DISABLE)
	    {
	      p = buf;
	      *p++ = 'E';
	      *p++ = tohex (((int) siggnal >> 4) & 0xf);
	      *p++ = tohex (((int) siggnal) & 0xf);
	      *p++ = ',';
	      p += hexnumstr (p, (ULONGEST) step_range_start);
	      *p++ = ',';
	      p += hexnumstr (p, (ULONGEST) step_range_end);
	      *p++ = 0;

	      putpkt (buf);
	      getpkt (buf, PBUFSIZ, 0);

	      if (packet_ok (buf, &remote_protocol_E) == PACKET_OK)
		return;
	    }
	}
      else
	{
	  if (remote_protocol_e.support != PACKET_DISABLE)
	    {
	      p = buf;
	      *p++ = 'e';
	      p += hexnumstr (p, (ULONGEST) step_range_start);
	      *p++ = ',';
	      p += hexnumstr (p, (ULONGEST) step_range_end);
	      *p++ = 0;

	      putpkt (buf);
	      getpkt (buf, PBUFSIZ, 0);

	      if (packet_ok (buf, &remote_protocol_e) == PACKET_OK)
		return;
	    }
	}
    }

  if (siggnal != TARGET_SIGNAL_0)
    {
      buf[0] = step ? 'S' : 'C';
      buf[1] = tohex (((int) siggnal >> 4) & 0xf);
      buf[2] = tohex (((int) siggnal) & 0xf);
      buf[3] = '\0';
    }
  else
    strcpy (buf, step ? "s" : "c");

  putpkt (buf);
}

/* Same as remote_resume, but with async support. */
static void
remote_async_resume (ptid_t ptid, int step, enum target_signal siggnal)
{
  char *buf = alloca (PBUFSIZ);
  int pid = PIDGET (ptid);
  char *p;

  if (pid == -1)
    set_thread (0, 0);		/* run any thread */
  else
    set_thread (pid, 0);	/* run this thread */

  last_sent_signal = siggnal;
  last_sent_step = step;

  /* A hook for when we need to do something at the last moment before
     resumption.  */
  if (target_resume_hook)
    (*target_resume_hook) ();

  /* The s/S/c/C packets do not return status.  So if the target does
     not support the S or C packets, the debug agent returns an empty
     string which is detected in remote_wait().  This protocol defect
     is fixed in the e/E packets. */

  if (step && step_range_end)
    {
      /* If the target does not support the 'E' packet, we try the 'S'
	 packet.  Ideally we would fall back to the 'e' packet if that
	 too is not supported.  But that would require another copy of
	 the code to issue the 'e' packet (and fall back to 's' if not
	 supported) in remote_wait().  */
      
      if (siggnal != TARGET_SIGNAL_0)
	{
	  if (remote_protocol_E.support != PACKET_DISABLE)
	    {
	      p = buf;
	      *p++ = 'E';
	      *p++ = tohex (((int) siggnal >> 4) & 0xf);
	      *p++ = tohex (((int) siggnal) & 0xf);
	      *p++ = ',';
	      p += hexnumstr (p, (ULONGEST) step_range_start);
	      *p++ = ',';
	      p += hexnumstr (p, (ULONGEST) step_range_end);
	      *p++ = 0;

	      putpkt (buf);
	      getpkt (buf, PBUFSIZ, 0);

	      if (packet_ok (buf, &remote_protocol_E) == PACKET_OK)
		goto register_event_loop;
	    }
	}
      else
	{
	  if (remote_protocol_e.support != PACKET_DISABLE)
	    {
	      p = buf;
	      *p++ = 'e';
	      p += hexnumstr (p, (ULONGEST) step_range_start);
	      *p++ = ',';
	      p += hexnumstr (p, (ULONGEST) step_range_end);
	      *p++ = 0;

	      putpkt (buf);
	      getpkt (buf, PBUFSIZ, 0);

	      if (packet_ok (buf, &remote_protocol_e) == PACKET_OK)
		goto register_event_loop;
	    }
	}
    }

  if (siggnal != TARGET_SIGNAL_0)
    {
      buf[0] = step ? 'S' : 'C';
      buf[1] = tohex (((int) siggnal >> 4) & 0xf);
      buf[2] = tohex ((int) siggnal & 0xf);
      buf[3] = '\0';
    }
  else
    strcpy (buf, step ? "s" : "c");
  
  putpkt (buf);

register_event_loop:
  /* We are about to start executing the inferior, let's register it
     with the event loop. NOTE: this is the one place where all the
     execution commands end up. We could alternatively do this in each
     of the execution commands in infcmd.c.*/
  /* FIXME: ezannoni 1999-09-28: We may need to move this out of here
     into infcmd.c in order to allow inferior function calls to work
     NOT asynchronously. */
  if (event_loop_p && target_can_async_p ())
    target_async (inferior_event_handler, 0);
  /* Tell the world that the target is now executing. */
  /* FIXME: cagney/1999-09-23: Is it the targets responsibility to set
     this?  Instead, should the client of target just assume (for
     async targets) that the target is going to start executing?  Is
     this information already found in the continuation block?  */
  if (target_is_async_p ())
    target_executing = 1;
}


/* Set up the signal handler for SIGINT, while the target is
   executing, ovewriting the 'regular' SIGINT signal handler. */
static void
initialize_sigint_signal_handler (void)
{
  sigint_remote_token =
    create_async_signal_handler (async_remote_interrupt, NULL);
  signal (SIGINT, handle_remote_sigint);
}

/* Signal handler for SIGINT, while the target is executing. */
static void
handle_remote_sigint (int sig)
{
  signal (sig, handle_remote_sigint_twice);
  sigint_remote_twice_token =
    create_async_signal_handler (async_remote_interrupt_twice, NULL);
  mark_async_signal_handler_wrapper (sigint_remote_token);
}

/* Signal handler for SIGINT, installed after SIGINT has already been
   sent once.  It will take effect the second time that the user sends
   a ^C. */
static void
handle_remote_sigint_twice (int sig)
{
  signal (sig, handle_sigint);
  sigint_remote_twice_token =
    create_async_signal_handler (inferior_event_handler_wrapper, NULL);
  mark_async_signal_handler_wrapper (sigint_remote_twice_token);
}

/* Perform the real interruption of the target execution, in response
   to a ^C. */
static void
async_remote_interrupt (gdb_client_data arg)
{
  if (remote_debug)
    fprintf_unfiltered (gdb_stdlog, "remote_interrupt called\n");

  target_stop ();
}

/* Perform interrupt, if the first attempt did not succeed. Just give
   up on the target alltogether. */
void
async_remote_interrupt_twice (gdb_client_data arg)
{
  if (remote_debug)
    fprintf_unfiltered (gdb_stdlog, "remote_interrupt_twice called\n");
  /* Do something only if the target was not killed by the previous
     cntl-C. */
  if (target_executing)
    {
      interrupt_query ();
      signal (SIGINT, handle_remote_sigint);
    }
}

/* Reinstall the usual SIGINT handlers, after the target has
   stopped. */
static void
cleanup_sigint_signal_handler (void *dummy)
{
  signal (SIGINT, handle_sigint);
  if (sigint_remote_twice_token)
    delete_async_signal_handler ((struct async_signal_handler **) & sigint_remote_twice_token);
  if (sigint_remote_token)
    delete_async_signal_handler ((struct async_signal_handler **) & sigint_remote_token);
}

/* Send ^C to target to halt it.  Target will respond, and send us a
   packet.  */
static void (*ofunc) (int);

/* The command line interface's stop routine. This function is installed
   as a signal handler for SIGINT. The first time a user requests a
   stop, we call remote_stop to send a break or ^C. If there is no
   response from the target (it didn't stop when the user requested it),
   we ask the user if he'd like to detach from the target. */
static void
remote_interrupt (int signo)
{
  /* If this doesn't work, try more severe steps. */
  signal (signo, remote_interrupt_twice);

  if (remote_debug)
    fprintf_unfiltered (gdb_stdlog, "remote_interrupt called\n");

  target_stop ();
}

/* The user typed ^C twice.  */

static void
remote_interrupt_twice (int signo)
{
  signal (signo, ofunc);
  interrupt_query ();
  signal (signo, remote_interrupt);
}

/* This is the generic stop called via the target vector. When a target
   interrupt is requested, either by the command line or the GUI, we
   will eventually end up here. */
static void
remote_stop (void)
{
  /* Send a break or a ^C, depending on user preference.  */
  if (remote_debug)
    fprintf_unfiltered (gdb_stdlog, "remote_stop called\n");

  if (remote_break)
    serial_send_break (remote_desc);
  else
    serial_write (remote_desc, "\003", 1);
}

/* Ask the user what to do when an interrupt is received.  */

static void
interrupt_query (void)
{
  target_terminal_ours ();

  if (query ("Interrupted while waiting for the program.\n\
Give up (and stop debugging it)? "))
    {
      target_mourn_inferior ();
      return_to_top_level (RETURN_QUIT);
    }

  target_terminal_inferior ();
}

/* Enable/disable target terminal ownership.  Most targets can use
   terminal groups to control terminal ownership.  Remote targets are
   different in that explicit transfer of ownership to/from GDB/target
   is required. */

static void
remote_async_terminal_inferior (void)
{
  /* FIXME: cagney/1999-09-27: Shouldn't need to test for
     sync_execution here.  This function should only be called when
     GDB is resuming the inferior in the forground.  A background
     resume (``run&'') should leave GDB in control of the terminal and
     consequently should not call this code. */
  if (!sync_execution)
    return;
  /* FIXME: cagney/1999-09-27: Closely related to the above.  Make
     calls target_terminal_*() idenpotent. The event-loop GDB talking
     to an asynchronous target with a synchronous command calls this
     function from both event-top.c and infrun.c/infcmd.c.  Once GDB
     stops trying to transfer the terminal to the target when it
     shouldn't this guard can go away.  */
  if (!remote_async_terminal_ours_p)
    return;
  delete_file_handler (input_fd);
  remote_async_terminal_ours_p = 0;
  initialize_sigint_signal_handler ();
  /* NOTE: At this point we could also register our selves as the
     recipient of all input.  Any characters typed could then be
     passed on down to the target. */
}

static void
remote_async_terminal_ours (void)
{
  /* See FIXME in remote_async_terminal_inferior. */
  if (!sync_execution)
    return;
  /* See FIXME in remote_async_terminal_inferior. */
  if (remote_async_terminal_ours_p)
    return;
  cleanup_sigint_signal_handler (NULL);
  add_file_handler (input_fd, stdin_event_handler, 0);
  remote_async_terminal_ours_p = 1;
}

/* If nonzero, ignore the next kill.  */

int kill_kludge;

void
remote_console_output (char *msg)
{
  char *p;

  for (p = msg; p[0] && p[1]; p += 2)
    {
      char tb[2];
      char c = fromhex (p[0]) * 16 + fromhex (p[1]);
      tb[0] = c;
      tb[1] = 0;
      fputs_unfiltered (tb, gdb_stdtarg);
    }
  gdb_flush (gdb_stdtarg);
}

/* Wait until the remote machine stops, then return,
   storing status in STATUS just as `wait' would.
   Returns "pid", which in the case of a multi-threaded 
   remote OS, is the thread-id.  */

static ptid_t
remote_wait (ptid_t ptid, struct target_waitstatus *status)
{
  unsigned char *buf = alloca (PBUFSIZ);
  int thread_num = -1;

  status->kind = TARGET_WAITKIND_EXITED;
  status->value.integer = 0;

  while (1)
    {
      unsigned char *p;

      ofunc = signal (SIGINT, remote_interrupt);
      getpkt (buf, PBUFSIZ, 1);
      signal (SIGINT, ofunc);

      /* This is a hook for when we need to do something (perhaps the
         collection of trace data) every time the target stops.  */
      if (target_wait_loop_hook)
	(*target_wait_loop_hook) ();

      switch (buf[0])
	{
	case 'E':		/* Error of some sort */
	  warning ("Remote failure reply: %s", buf);
	  continue;
	case 'T':		/* Status with PC, SP, FP, ... */
	  {
	    int i;
	    long regno;
	    char* regs = (char*) alloca (MAX_REGISTER_RAW_SIZE);

	    /* Expedited reply, containing Signal, {regno, reg} repeat */
	    /*  format is:  'Tssn...:r...;n...:r...;n...:r...;#cc', where
	       ss = signal number
	       n... = register number
	       r... = register contents
	     */
	    p = &buf[3];	/* after Txx */

	    while (*p)
	      {
		unsigned char *p1;
		char *p_temp;
		int fieldsize;

		/* Read the register number */
		regno = strtol ((const char *) p, &p_temp, 16);
		p1 = (unsigned char *) p_temp;

		if (p1 == p)	/* No register number present here */
		  {
		    p1 = (unsigned char *) strchr ((const char *) p, ':');
		    if (p1 == NULL)
		      warning ("Malformed packet(a) (missing colon): %s\n\
Packet: '%s'\n",
			       p, buf);
		    if (strncmp ((const char *) p, "thread", p1 - p) == 0)
		      {
			p_temp = unpack_varlen_hex (++p1, &thread_num);
			record_currthread (thread_num);
			p = (unsigned char *) p_temp;
		      }
		  }
		else
		  {
		    p = p1;

		    if (*p++ != ':')
		      warning ("Malformed packet(b) (missing colon): %s\n\
Packet: '%s'\n",
			       p, buf);

		    if (regno >= NUM_REGS)
		      warning ("Remote sent bad register number %ld: %s\n\
Packet: '%s'\n",
			       regno, p, buf);

		    fieldsize = hex2bin (p, regs, REGISTER_RAW_SIZE (regno));
		    p += 2 * fieldsize;
		    if (fieldsize < REGISTER_RAW_SIZE (regno))
		      warning ("Remote reply is too short: %s", buf);
		    supply_register (regno, regs);
		  }

		if (*p++ != ';')
		  {
		    warning ("Remote register badly formatted: %s", buf);
		    warning ("            here: %s", p);
		  }
	      }
	  }
	  /* fall through */
	case 'S':		/* Old style status, just signal only */
	  status->kind = TARGET_WAITKIND_STOPPED;
	  status->value.sig = (enum target_signal)
	    (((fromhex (buf[1])) << 4) + (fromhex (buf[2])));

	  if (buf[3] == 'p')
	    {
	      /* Export Cisco kernel mode as a convenience variable
	         (so that it can be used in the GDB prompt if desired). */

	      if (cisco_kernel_mode == 1)
		set_internalvar (lookup_internalvar ("cisco_kernel_mode"),
				 value_from_string ("PDEBUG-"));
	      cisco_kernel_mode = 0;
	      thread_num = strtol ((const char *) &buf[4], NULL, 16);
	      record_currthread (thread_num);
	    }
	  else if (buf[3] == 'k')
	    {
	      /* Export Cisco kernel mode as a convenience variable
	         (so that it can be used in the GDB prompt if desired). */

	      if (cisco_kernel_mode == 1)
		set_internalvar (lookup_internalvar ("cisco_kernel_mode"),
				 value_from_string ("KDEBUG-"));
	      cisco_kernel_mode = 1;
	    }
	  goto got_status;
	case 'N':		/* Cisco special: status and offsets */
	  {
	    bfd_vma text_addr, data_addr, bss_addr;
	    bfd_signed_vma text_off, data_off, bss_off;
	    unsigned char *p1;

	    status->kind = TARGET_WAITKIND_STOPPED;
	    status->value.sig = (enum target_signal)
	      (((fromhex (buf[1])) << 4) + (fromhex (buf[2])));

	    if (symfile_objfile == NULL)
	      {
		warning ("Relocation packet received with no symbol file.  \
Packet Dropped");
		goto got_status;
	      }

	    /* Relocate object file.  Buffer format is NAATT;DD;BB
	     * where AA is the signal number, TT is the new text
	     * address, DD * is the new data address, and BB is the
	     * new bss address.  */

	    p = &buf[3];
	    text_addr = strtoul (p, (char **) &p1, 16);
	    if (p1 == p || *p1 != ';')
	      warning ("Malformed relocation packet: Packet '%s'", buf);
	    p = p1 + 1;
	    data_addr = strtoul (p, (char **) &p1, 16);
	    if (p1 == p || *p1 != ';')
	      warning ("Malformed relocation packet: Packet '%s'", buf);
	    p = p1 + 1;
	    bss_addr = strtoul (p, (char **) &p1, 16);
	    if (p1 == p)
	      warning ("Malformed relocation packet: Packet '%s'", buf);

	    if (remote_cisco_section_offsets (text_addr, data_addr, bss_addr,
					      &text_off, &data_off, &bss_off)
		== 0)
	      if (text_off != 0 || data_off != 0 || bss_off != 0)
		remote_cisco_objfile_relocate (text_off, data_off, bss_off);

	    goto got_status;
	  }
	case 'W':		/* Target exited */
	  {
	    /* The remote process exited.  */
	    status->kind = TARGET_WAITKIND_EXITED;
	    status->value.integer = (fromhex (buf[1]) << 4) + fromhex (buf[2]);
	    goto got_status;
	  }
	case 'X':
	  status->kind = TARGET_WAITKIND_SIGNALLED;
	  status->value.sig = (enum target_signal)
	    (((fromhex (buf[1])) << 4) + (fromhex (buf[2])));
	  kill_kludge = 1;

	  goto got_status;
	case 'O':		/* Console output */
	  remote_console_output (buf + 1);
	  continue;
	case '\0':
	  if (last_sent_signal != TARGET_SIGNAL_0)
	    {
	      /* Zero length reply means that we tried 'S' or 'C' and
	         the remote system doesn't support it.  */
	      target_terminal_ours_for_output ();
	      printf_filtered
		("Can't send signals to this remote system.  %s not sent.\n",
		 target_signal_to_name (last_sent_signal));
	      last_sent_signal = TARGET_SIGNAL_0;
	      target_terminal_inferior ();

	      strcpy ((char *) buf, last_sent_step ? "s" : "c");
	      putpkt ((char *) buf);
	      continue;
	    }
	  /* else fallthrough */
	default:
	  warning ("Invalid remote reply: %s", buf);
	  continue;
	}
    }
got_status:
  if (thread_num != -1)
    {
      return pid_to_ptid (thread_num);
    }
  return inferior_ptid;
}

/* Async version of remote_wait. */
static ptid_t
remote_async_wait (ptid_t ptid, struct target_waitstatus *status)
{
  unsigned char *buf = alloca (PBUFSIZ);
  int thread_num = -1;

  status->kind = TARGET_WAITKIND_EXITED;
  status->value.integer = 0;

  while (1)
    {
      unsigned char *p;

      if (!target_is_async_p ())
	ofunc = signal (SIGINT, remote_interrupt);
      /* FIXME: cagney/1999-09-27: If we're in async mode we should
         _never_ wait for ever -> test on target_is_async_p().
         However, before we do that we need to ensure that the caller
         knows how to take the target into/out of async mode. */
      getpkt (buf, PBUFSIZ, wait_forever_enabled_p);
      if (!target_is_async_p ())
	signal (SIGINT, ofunc);

      /* This is a hook for when we need to do something (perhaps the
         collection of trace data) every time the target stops.  */
      if (target_wait_loop_hook)
	(*target_wait_loop_hook) ();

      switch (buf[0])
	{
	case 'E':		/* Error of some sort */
	  warning ("Remote failure reply: %s", buf);
	  continue;
	case 'T':		/* Status with PC, SP, FP, ... */
	  {
	    int i;
	    long regno;
	    char* regs = (char*) alloca (MAX_REGISTER_RAW_SIZE);

	    /* Expedited reply, containing Signal, {regno, reg} repeat */
	    /*  format is:  'Tssn...:r...;n...:r...;n...:r...;#cc', where
	       ss = signal number
	       n... = register number
	       r... = register contents
	     */
	    p = &buf[3];	/* after Txx */

	    while (*p)
	      {
		unsigned char *p1;
		char *p_temp;
		int fieldsize;

		/* Read the register number */
		regno = strtol ((const char *) p, &p_temp, 16);
		p1 = (unsigned char *) p_temp;

		if (p1 == p)	/* No register number present here */
		  {
		    p1 = (unsigned char *) strchr ((const char *) p, ':');
		    if (p1 == NULL)
		      warning ("Malformed packet(a) (missing colon): %s\n\
Packet: '%s'\n",
			       p, buf);
		    if (strncmp ((const char *) p, "thread", p1 - p) == 0)
		      {
			p_temp = unpack_varlen_hex (++p1, &thread_num);
			record_currthread (thread_num);
			p = (unsigned char *) p_temp;
		      }
		  }
		else
		  {
		    p = p1;

		    if (*p++ != ':')
		      warning ("Malformed packet(b) (missing colon): %s\n\
Packet: '%s'\n",
			       p, buf);

		    if (regno >= NUM_REGS)
		      warning ("Remote sent bad register number %ld: %s\n\
Packet: '%s'\n",
			       regno, p, buf);

		    fieldsize = hex2bin (p, regs, REGISTER_RAW_SIZE (regno));
		    p += 2 * fieldsize;
		    if (fieldsize < REGISTER_RAW_SIZE (regno))
		      warning ("Remote reply is too short: %s", buf);
		    supply_register (regno, regs);
		  }

		if (*p++ != ';')
		  {
		    warning ("Remote register badly formatted: %s", buf);
		    warning ("            here: %s", p);
		  }
	      }
	  }
	  /* fall through */
	case 'S':		/* Old style status, just signal only */
	  status->kind = TARGET_WAITKIND_STOPPED;
	  status->value.sig = (enum target_signal)
	    (((fromhex (buf[1])) << 4) + (fromhex (buf[2])));

	  if (buf[3] == 'p')
	    {
	      /* Export Cisco kernel mode as a convenience variable
	         (so that it can be used in the GDB prompt if desired). */

	      if (cisco_kernel_mode == 1)
		set_internalvar (lookup_internalvar ("cisco_kernel_mode"),
				 value_from_string ("PDEBUG-"));
	      cisco_kernel_mode = 0;
	      thread_num = strtol ((const char *) &buf[4], NULL, 16);
	      record_currthread (thread_num);
	    }
	  else if (buf[3] == 'k')
	    {
	      /* Export Cisco kernel mode as a convenience variable
	         (so that it can be used in the GDB prompt if desired). */

	      if (cisco_kernel_mode == 1)
		set_internalvar (lookup_internalvar ("cisco_kernel_mode"),
				 value_from_string ("KDEBUG-"));
	      cisco_kernel_mode = 1;
	    }
	  goto got_status;
	case 'N':		/* Cisco special: status and offsets */
	  {
	    bfd_vma text_addr, data_addr, bss_addr;
	    bfd_signed_vma text_off, data_off, bss_off;
	    unsigned char *p1;

	    status->kind = TARGET_WAITKIND_STOPPED;
	    status->value.sig = (enum target_signal)
	      (((fromhex (buf[1])) << 4) + (fromhex (buf[2])));

	    if (symfile_objfile == NULL)
	      {
		warning ("Relocation packet recieved with no symbol file.  \
Packet Dropped");
		goto got_status;
	      }

	    /* Relocate object file.  Buffer format is NAATT;DD;BB
	     * where AA is the signal number, TT is the new text
	     * address, DD * is the new data address, and BB is the
	     * new bss address.  */

	    p = &buf[3];
	    text_addr = strtoul (p, (char **) &p1, 16);
	    if (p1 == p || *p1 != ';')
	      warning ("Malformed relocation packet: Packet '%s'", buf);
	    p = p1 + 1;
	    data_addr = strtoul (p, (char **) &p1, 16);
	    if (p1 == p || *p1 != ';')
	      warning ("Malformed relocation packet: Packet '%s'", buf);
	    p = p1 + 1;
	    bss_addr = strtoul (p, (char **) &p1, 16);
	    if (p1 == p)
	      warning ("Malformed relocation packet: Packet '%s'", buf);

	    if (remote_cisco_section_offsets (text_addr, data_addr, bss_addr,
					      &text_off, &data_off, &bss_off)
		== 0)
	      if (text_off != 0 || data_off != 0 || bss_off != 0)
		remote_cisco_objfile_relocate (text_off, data_off, bss_off);

	    goto got_status;
	  }
	case 'W':		/* Target exited */
	  {
	    /* The remote process exited.  */
	    status->kind = TARGET_WAITKIND_EXITED;
	    status->value.integer = (fromhex (buf[1]) << 4) + fromhex (buf[2]);
	    goto got_status;
	  }
	case 'X':
	  status->kind = TARGET_WAITKIND_SIGNALLED;
	  status->value.sig = (enum target_signal)
	    (((fromhex (buf[1])) << 4) + (fromhex (buf[2])));
	  kill_kludge = 1;

	  goto got_status;
	case 'O':		/* Console output */
	  remote_console_output (buf + 1);
	  /* Return immediately to the event loop. The event loop will
             still be waiting on the inferior afterwards. */
          status->kind = TARGET_WAITKIND_IGNORE;
          goto got_status;
	case '\0':
	  if (last_sent_signal != TARGET_SIGNAL_0)
	    {
	      /* Zero length reply means that we tried 'S' or 'C' and
	         the remote system doesn't support it.  */
	      target_terminal_ours_for_output ();
	      printf_filtered
		("Can't send signals to this remote system.  %s not sent.\n",
		 target_signal_to_name (last_sent_signal));
	      last_sent_signal = TARGET_SIGNAL_0;
	      target_terminal_inferior ();

	      strcpy ((char *) buf, last_sent_step ? "s" : "c");
	      putpkt ((char *) buf);
	      continue;
	    }
	  /* else fallthrough */
	default:
	  warning ("Invalid remote reply: %s", buf);
	  continue;
	}
    }
got_status:
  if (thread_num != -1)
    {
      return pid_to_ptid (thread_num);
    }
  return inferior_ptid;
}

/* Number of bytes of registers this stub implements.  */

static int register_bytes_found;

/* Read the remote registers into the block REGS.  */
/* Currently we just read all the registers, so we don't use regno.  */

/* ARGSUSED */
static void
remote_fetch_registers (int regno)
{
  char *buf = alloca (PBUFSIZ);
  int i;
  char *p;
  char *regs = alloca (REGISTER_BYTES);

  set_thread (PIDGET (inferior_ptid), 1);

  sprintf (buf, "g");
  remote_send (buf, PBUFSIZ);

  /* Save the size of the packet sent to us by the target.  Its used
     as a heuristic when determining the max size of packets that the
     target can safely receive. */
  if (actual_register_packet_size == 0)
    actual_register_packet_size = strlen (buf);

  /* Unimplemented registers read as all bits zero.  */
  memset (regs, 0, REGISTER_BYTES);

  /* We can get out of synch in various cases.  If the first character
     in the buffer is not a hex character, assume that has happened
     and try to fetch another packet to read.  */
  while ((buf[0] < '0' || buf[0] > '9')
	 && (buf[0] < 'a' || buf[0] > 'f')
	 && buf[0] != 'x')	/* New: unavailable register value */
    {
      if (remote_debug)
	fprintf_unfiltered (gdb_stdlog,
			    "Bad register packet; fetching a new packet\n");
      getpkt (buf, PBUFSIZ, 0);
    }

  /* Reply describes registers byte by byte, each byte encoded as two
     hex characters.  Suck them all up, then supply them to the
     register cacheing/storage mechanism.  */

  p = buf;
  for (i = 0; i < REGISTER_BYTES; i++)
    {
      if (p[0] == 0)
	break;
      if (p[1] == 0)
	{
	  warning ("Remote reply is of odd length: %s", buf);
	  /* Don't change register_bytes_found in this case, and don't
	     print a second warning.  */
	  goto supply_them;
	}
      if (p[0] == 'x' && p[1] == 'x')
	regs[i] = 0;		/* 'x' */
      else
	regs[i] = fromhex (p[0]) * 16 + fromhex (p[1]);
      p += 2;
    }

  if (i != register_bytes_found)
    {
      register_bytes_found = i;
      if (REGISTER_BYTES_OK_P ()
	  && !REGISTER_BYTES_OK (i))
	warning ("Remote reply is too short: %s", buf);
    }

supply_them:
  for (i = 0; i < NUM_REGS; i++)
    {
      supply_register (i, &regs[REGISTER_BYTE (i)]);
      if (buf[REGISTER_BYTE (i) * 2] == 'x')
	set_register_cached (i, -1);
    }
}

/* Prepare to store registers.  Since we may send them all (using a
   'G' request), we have to read out the ones we don't want to change
   first.  */

static void
remote_prepare_to_store (void)
{
  /* Make sure the entire registers array is valid.  */
  switch (remote_protocol_P.support)
    {
    case PACKET_DISABLE:
    case PACKET_SUPPORT_UNKNOWN:
      read_register_bytes (0, (char *) NULL, REGISTER_BYTES);
      break;
    case PACKET_ENABLE:
      break;
    }
}

/* Helper: Attempt to store REGNO using the P packet.  Return fail IFF
   packet was not recognized. */

static int
store_register_using_P (int regno)
{
  /* Try storing a single register.  */
  char *buf = alloca (PBUFSIZ);
  char *regp;
  char *p;
  int i;

  sprintf (buf, "P%x=", regno);
  p = buf + strlen (buf);
  regp = register_buffer (regno);
  bin2hex (regp, p, REGISTER_RAW_SIZE (regno));
  remote_send (buf, PBUFSIZ);

  return buf[0] != '\0';
}


/* Store register REGNO, or all registers if REGNO == -1, from the contents
   of the register cache buffer.  FIXME: ignores errors.  */

static void
remote_store_registers (int regno)
{
  char *buf = alloca (PBUFSIZ);
  int i;
  char *p;
  char *regs;

  set_thread (PIDGET (inferior_ptid), 1);

  if (regno >= 0)
    {
      switch (remote_protocol_P.support)
	{
	case PACKET_DISABLE:
	  break;
	case PACKET_ENABLE:
	  if (store_register_using_P (regno))
	    return;
	  else
	    error ("Protocol error: P packet not recognized by stub");
	case PACKET_SUPPORT_UNKNOWN:
	  if (store_register_using_P (regno))
	    {
	      /* The stub recognized the 'P' packet.  Remember this.  */
	      remote_protocol_P.support = PACKET_ENABLE;
	      return;
	    }
	  else
	    {
	      /* The stub does not support the 'P' packet.  Use 'G'
	         instead, and don't try using 'P' in the future (it
	         will just waste our time).  */
	      remote_protocol_P.support = PACKET_DISABLE;
	      break;
	    }
	}
    }

  buf[0] = 'G';

  /* Command describes registers byte by byte,
     each byte encoded as two hex characters.  */

  regs = register_buffer (-1);
  p = buf + 1;
  /* remote_prepare_to_store insures that register_bytes_found gets set.  */
  bin2hex (regs, p, register_bytes_found);
  remote_send (buf, PBUFSIZ);
}


/* Return the number of hex digits in num.  */

static int
hexnumlen (ULONGEST num)
{
  int i;

  for (i = 0; num != 0; i++)
    num >>= 4;

  return max (i, 1);
}

/* Set BUF to the minimum number of hex digits representing NUM.  */

static int
hexnumstr (char *buf, ULONGEST num)
{
  int len = hexnumlen (num);
  return hexnumnstr (buf, num, len);
}


/* Set BUF to the hex digits representing NUM, padded to WIDTH characters.  */

static int
hexnumnstr (char *buf, ULONGEST num, int width)
{
  int i;

  buf[width] = '\0';

  for (i = width - 1; i >= 0; i--)
    {
      buf[i] = "0123456789abcdef"[(num & 0xf)];
      num >>= 4;
    }

  return width;
}

/* Mask all but the least significant REMOTE_ADDRESS_SIZE bits. */

static CORE_ADDR
remote_address_masked (CORE_ADDR addr)
{
  if (remote_address_size > 0
      && remote_address_size < (sizeof (ULONGEST) * 8))
    {
      /* Only create a mask when that mask can safely be constructed
         in a ULONGEST variable. */
      ULONGEST mask = 1;
      mask = (mask << remote_address_size) - 1;
      addr &= mask;
    }
  return addr;
}

/* Determine whether the remote target supports binary downloading.
   This is accomplished by sending a no-op memory write of zero length
   to the target at the specified address. It does not suffice to send
   the whole packet, since many stubs strip the eighth bit and subsequently
   compute a wrong checksum, which causes real havoc with remote_write_bytes.

   NOTE: This can still lose if the serial line is not eight-bit
   clean. In cases like this, the user should clear "remote
   X-packet". */

static void
check_binary_download (CORE_ADDR addr)
{
  switch (remote_protocol_binary_download.support)
    {
    case PACKET_DISABLE:
      break;
    case PACKET_ENABLE:
      break;
    case PACKET_SUPPORT_UNKNOWN:
      {
	char *buf = alloca (PBUFSIZ);
	char *p;
	
	p = buf;
	*p++ = 'X';
	p += hexnumstr (p, (ULONGEST) addr);
	*p++ = ',';
	p += hexnumstr (p, (ULONGEST) 0);
	*p++ = ':';
	*p = '\0';
	
	putpkt_binary (buf, (int) (p - buf));
	getpkt (buf, PBUFSIZ, 0);

	if (buf[0] == '\0')
	  {
	    if (remote_debug)
	      fprintf_unfiltered (gdb_stdlog,
				  "binary downloading NOT suppported by target\n");
	    remote_protocol_binary_download.support = PACKET_DISABLE;
	  }
	else
	  {
	    if (remote_debug)
	      fprintf_unfiltered (gdb_stdlog,
				  "binary downloading suppported by target\n");
	    remote_protocol_binary_download.support = PACKET_ENABLE;
	  }
	break;
      }
    }
}

/* Write memory data directly to the remote machine.
   This does not inform the data cache; the data cache uses this.
   MEMADDR is the address in the remote memory space.
   MYADDR is the address of the buffer in our space.
   LEN is the number of bytes.

   Returns number of bytes transferred, or 0 (setting errno) for
   error.  Only transfer a single packet. */

static int
remote_write_bytes (CORE_ADDR memaddr, char *myaddr, int len)
{
  unsigned char *buf;
  int max_buf_size;		/* Max size of packet output buffer */
  unsigned char *p;
  unsigned char *plen;
  long sizeof_buf;
  int plenlen;
  int todo;
  int nr_bytes;

  /* Verify that the target can support a binary download */
  check_binary_download (memaddr);

  /* Determine the max packet size. */
  max_buf_size = get_memory_write_packet_size ();
  sizeof_buf = max_buf_size + 1; /* Space for trailing NUL */
  buf = alloca (sizeof_buf);

  /* Subtract header overhead from max payload size -  $M<memaddr>,<len>:#nn */
  max_buf_size -= 2 + hexnumlen (memaddr + len - 1) + 1 + hexnumlen (len) + 4;

  /* construct "M"<memaddr>","<len>":" */
  /* sprintf (buf, "M%lx,%x:", (unsigned long) memaddr, todo); */
  p = buf;

  /* Append [XM].  Compute a best guess of the number of bytes
     actually transfered. */
  switch (remote_protocol_binary_download.support)
    {
    case PACKET_ENABLE:
      *p++ = 'X';
      /* Best guess at number of bytes that will fit. */
      todo = min (len, max_buf_size);
      break;
    case PACKET_DISABLE:
      *p++ = 'M';
      /* num bytes that will fit */
      todo = min (len, max_buf_size / 2);
      break;
    case PACKET_SUPPORT_UNKNOWN:
      internal_error (__FILE__, __LINE__,
		      "remote_write_bytes: bad internal state");
    default:
      internal_error (__FILE__, __LINE__, "bad switch");
    }
  
  /* Append <memaddr> */
  memaddr = remote_address_masked (memaddr);
  p += hexnumstr (p, (ULONGEST) memaddr);
  *p++ = ',';
  
  /* Append <len>.  Retain the location/size of <len>.  It may
     need to be adjusted once the packet body has been created. */
  plen = p;
  plenlen = hexnumstr (p, (ULONGEST) todo);
  p += plenlen;
  *p++ = ':';
  *p = '\0';
  
  /* Append the packet body. */
  switch (remote_protocol_binary_download.support)
    {
    case PACKET_ENABLE:
      /* Binary mode.  Send target system values byte by byte, in
	 increasing byte addresses.  Only escape certain critical
	 characters.  */
      for (nr_bytes = 0;
	   (nr_bytes < todo) && (p - buf) < (max_buf_size - 2);
	   nr_bytes++)
	{
	  switch (myaddr[nr_bytes] & 0xff)
	    {
	    case '$':
	    case '#':
	    case 0x7d:
	      /* These must be escaped */
	      *p++ = 0x7d;
	      *p++ = (myaddr[nr_bytes] & 0xff) ^ 0x20;
	      break;
	    default:
	      *p++ = myaddr[nr_bytes] & 0xff;
	      break;
	    }
	}
      if (nr_bytes < todo)
	{
	  /* Escape chars have filled up the buffer prematurely, 
	     and we have actually sent fewer bytes than planned.
	     Fix-up the length field of the packet.  Use the same
	     number of characters as before.  */
	  
	  plen += hexnumnstr (plen, (ULONGEST) nr_bytes, plenlen);
	  *plen = ':';  /* overwrite \0 from hexnumnstr() */
	}
      break;
    case PACKET_DISABLE:
      /* Normal mode: Send target system values byte by byte, in
	 increasing byte addresses.  Each byte is encoded as a two hex
	 value.  */
      nr_bytes = bin2hex (myaddr, p, todo);
      p += 2 * nr_bytes;
      break;
    case PACKET_SUPPORT_UNKNOWN:
      internal_error (__FILE__, __LINE__,
		      "remote_write_bytes: bad internal state");
    default:
      internal_error (__FILE__, __LINE__, "bad switch");
    }
  
  putpkt_binary (buf, (int) (p - buf));
  getpkt (buf, sizeof_buf, 0);
  
  if (buf[0] == 'E')
    {
      /* There is no correspondance between what the remote protocol
	 uses for errors and errno codes.  We would like a cleaner way
	 of representing errors (big enough to include errno codes,
	 bfd_error codes, and others).  But for now just return EIO.  */
      errno = EIO;
      return 0;
    }
  
  /* Return NR_BYTES, not TODO, in case escape chars caused us to send fewer
     bytes than we'd planned.  */
  return nr_bytes;
}

/* Read memory data directly from the remote machine.
   This does not use the data cache; the data cache uses this.
   MEMADDR is the address in the remote memory space.
   MYADDR is the address of the buffer in our space.
   LEN is the number of bytes.

   Returns number of bytes transferred, or 0 for error.  */

/* NOTE: cagney/1999-10-18: This function (and its siblings in other
   remote targets) shouldn't attempt to read the entire buffer.
   Instead it should read a single packet worth of data and then
   return the byte size of that packet to the caller.  The caller (its
   caller and its callers caller ;-) already contains code for
   handling partial reads. */

static int
remote_read_bytes (CORE_ADDR memaddr, char *myaddr, int len)
{
  char *buf;
  int max_buf_size;		/* Max size of packet output buffer */
  long sizeof_buf;
  int origlen;

  /* Create a buffer big enough for this packet. */
  max_buf_size = get_memory_read_packet_size ();
  sizeof_buf = max_buf_size + 1; /* Space for trailing NUL */
  buf = alloca (sizeof_buf);

  origlen = len;
  while (len > 0)
    {
      char *p;
      int todo;
      int i;

      todo = min (len, max_buf_size / 2);	/* num bytes that will fit */

      /* construct "m"<memaddr>","<len>" */
      /* sprintf (buf, "m%lx,%x", (unsigned long) memaddr, todo); */
      memaddr = remote_address_masked (memaddr);
      p = buf;
      *p++ = 'm';
      p += hexnumstr (p, (ULONGEST) memaddr);
      *p++ = ',';
      p += hexnumstr (p, (ULONGEST) todo);
      *p = '\0';

      putpkt (buf);
      getpkt (buf, sizeof_buf, 0);

      if (buf[0] == 'E')
	{
	  /* There is no correspondance between what the remote protocol uses
	     for errors and errno codes.  We would like a cleaner way of
	     representing errors (big enough to include errno codes, bfd_error
	     codes, and others).  But for now just return EIO.  */
	  errno = EIO;
	  return 0;
	}

      /* Reply describes memory byte by byte,
         each byte encoded as two hex characters.  */

      p = buf;
      if ((i = hex2bin (p, myaddr, todo)) < todo)
	{
	  /* Reply is short.  This means that we were able to read
	     only part of what we wanted to. */
	  return i + (origlen - len);
	}
      myaddr += todo;
      memaddr += todo;
      len -= todo;
    }
  return origlen;
}

/* Read or write LEN bytes from inferior memory at MEMADDR,
   transferring to or from debugger address BUFFER.  Write to inferior if
   SHOULD_WRITE is nonzero.  Returns length of data written or read; 0
   for error.  TARGET is unused.  */

/* ARGSUSED */
static int
remote_xfer_memory (CORE_ADDR mem_addr, char *buffer, int mem_len,
		    int should_write,
		    struct mem_attrib *attrib ATTRIBUTE_UNUSED,
		    struct target_ops *target)
{
  CORE_ADDR targ_addr;
  int targ_len;
  int res;

  REMOTE_TRANSLATE_XFER_ADDRESS (mem_addr, mem_len, &targ_addr, &targ_len);
  if (targ_len <= 0)
    return 0;

  if (should_write)
    res = remote_write_bytes (targ_addr, buffer, targ_len);
  else
    res = remote_read_bytes (targ_addr, buffer, targ_len);

  return res;
}


#if 0
/* Enable after 4.12.  */

void
remote_search (int len, char *data, char *mask, CORE_ADDR startaddr,
	       int increment, CORE_ADDR lorange, CORE_ADDR hirange,
	       CORE_ADDR *addr_found, char *data_found)
{
  if (increment == -4 && len == 4)
    {
      long mask_long, data_long;
      long data_found_long;
      CORE_ADDR addr_we_found;
      char *buf = alloca (PBUFSIZ);
      long returned_long[2];
      char *p;

      mask_long = extract_unsigned_integer (mask, len);
      data_long = extract_unsigned_integer (data, len);
      sprintf (buf, "t%x:%x,%x", startaddr, data_long, mask_long);
      putpkt (buf);
      getpkt (buf, PBUFSIZ, 0);
      if (buf[0] == '\0')
	{
	  /* The stub doesn't support the 't' request.  We might want to
	     remember this fact, but on the other hand the stub could be
	     switched on us.  Maybe we should remember it only until
	     the next "target remote".  */
	  generic_search (len, data, mask, startaddr, increment, lorange,
			  hirange, addr_found, data_found);
	  return;
	}

      if (buf[0] == 'E')
	/* There is no correspondance between what the remote protocol uses
	   for errors and errno codes.  We would like a cleaner way of
	   representing errors (big enough to include errno codes, bfd_error
	   codes, and others).  But for now just use EIO.  */
	memory_error (EIO, startaddr);
      p = buf;
      addr_we_found = 0;
      while (*p != '\0' && *p != ',')
	addr_we_found = (addr_we_found << 4) + fromhex (*p++);
      if (*p == '\0')
	error ("Protocol error: short return for search");

      data_found_long = 0;
      while (*p != '\0' && *p != ',')
	data_found_long = (data_found_long << 4) + fromhex (*p++);
      /* Ignore anything after this comma, for future extensions.  */

      if (addr_we_found < lorange || addr_we_found >= hirange)
	{
	  *addr_found = 0;
	  return;
	}

      *addr_found = addr_we_found;
      *data_found = store_unsigned_integer (data_we_found, len);
      return;
    }
  generic_search (len, data, mask, startaddr, increment, lorange,
		  hirange, addr_found, data_found);
}
#endif /* 0 */

static void
remote_files_info (struct target_ops *ignore)
{
  puts_filtered ("Debugging a target over a serial line.\n");
}

/* Stuff for dealing with the packets which are part of this protocol.
   See comment at top of file for details.  */

/* Read a single character from the remote end, masking it down to 7 bits. */

static int
readchar (int timeout)
{
  int ch;

  ch = serial_readchar (remote_desc, timeout);

  if (ch >= 0)
    return (ch & 0x7f);

  switch ((enum serial_rc) ch)
    {
    case SERIAL_EOF:
      target_mourn_inferior ();
      error ("Remote connection closed");
      /* no return */
    case SERIAL_ERROR:
      perror_with_name ("Remote communication error");
      /* no return */
    case SERIAL_TIMEOUT:
      break;
    }
  return ch;
}

/* Send the command in BUF to the remote machine, and read the reply
   into BUF.  Report an error if we get an error reply.  */

static void
remote_send (char *buf,
	     long sizeof_buf)
{
  putpkt (buf);
  getpkt (buf, sizeof_buf, 0);

  if (buf[0] == 'E')
    error ("Remote failure reply: %s", buf);
}

/* Display a null-terminated packet on stdout, for debugging, using C
   string notation.  */

static void
print_packet (char *buf)
{
  puts_filtered ("\"");
  fputstr_filtered (buf, '"', gdb_stdout);
  puts_filtered ("\"");
}

int
putpkt (char *buf)
{
  return putpkt_binary (buf, strlen (buf));
}

/* Send a packet to the remote machine, with error checking.  The data
   of the packet is in BUF.  The string in BUF can be at most  PBUFSIZ - 5
   to account for the $, # and checksum, and for a possible /0 if we are
   debugging (remote_debug) and want to print the sent packet as a string */

static int
putpkt_binary (char *buf, int cnt)
{
  int i;
  unsigned char csum = 0;
  char *buf2 = alloca (cnt + 6);
  long sizeof_junkbuf = PBUFSIZ;
  char *junkbuf = alloca (sizeof_junkbuf);

  int ch;
  int tcount = 0;
  char *p;

  /* Copy the packet into buffer BUF2, encapsulating it
     and giving it a checksum.  */

  p = buf2;
  *p++ = '$';

  for (i = 0; i < cnt; i++)
    {
      csum += buf[i];
      *p++ = buf[i];
    }
  *p++ = '#';
  *p++ = tohex ((csum >> 4) & 0xf);
  *p++ = tohex (csum & 0xf);

  /* Send it over and over until we get a positive ack.  */

  while (1)
    {
      int started_error_output = 0;

      if (remote_debug)
	{
	  *p = '\0';
	  fprintf_unfiltered (gdb_stdlog, "Sending packet: ");
	  fputstrn_unfiltered (buf2, p - buf2, 0, gdb_stdlog);
	  fprintf_unfiltered (gdb_stdlog, "...");
	  gdb_flush (gdb_stdlog);
	}
      if (serial_write (remote_desc, buf2, p - buf2))
	perror_with_name ("putpkt: write failed");

      /* read until either a timeout occurs (-2) or '+' is read */
      while (1)
	{
	  ch = readchar (remote_timeout);

	  if (remote_debug)
	    {
	      switch (ch)
		{
		case '+':
		case '-':
		case SERIAL_TIMEOUT:
		case '$':
		  if (started_error_output)
		    {
		      putchar_unfiltered ('\n');
		      started_error_output = 0;
		    }
		}
	    }

	  switch (ch)
	    {
	    case '+':
	      if (remote_debug)
		fprintf_unfiltered (gdb_stdlog, "Ack\n");
	      return 1;
	    case '-':
	      if (remote_debug)
		fprintf_unfiltered (gdb_stdlog, "Nak\n");
	    case SERIAL_TIMEOUT:
	      tcount++;
	      if (tcount > 3)
		return 0;
	      break;		/* Retransmit buffer */
	    case '$':
	      {
	        if (remote_debug)
		  fprintf_unfiltered (gdb_stdlog, "Packet instead of Ack, ignoring it\n");
		/* It's probably an old response, and we're out of sync.
		   Just gobble up the packet and ignore it.  */
		read_frame (junkbuf, sizeof_junkbuf);
		continue;	/* Now, go look for + */
	      }
	    default:
	      if (remote_debug)
		{
		  if (!started_error_output)
		    {
		      started_error_output = 1;
		      fprintf_unfiltered (gdb_stdlog, "putpkt: Junk: ");
		    }
		  fputc_unfiltered (ch & 0177, gdb_stdlog);
		}
	      continue;
	    }
	  break;		/* Here to retransmit */
	}

#if 0
      /* This is wrong.  If doing a long backtrace, the user should be
         able to get out next time we call QUIT, without anything as
         violent as interrupt_query.  If we want to provide a way out of
         here without getting to the next QUIT, it should be based on
         hitting ^C twice as in remote_wait.  */
      if (quit_flag)
	{
	  quit_flag = 0;
	  interrupt_query ();
	}
#endif
    }
}

static int remote_cisco_mode;

/* Come here after finding the start of the frame.  Collect the rest
   into BUF, verifying the checksum, length, and handling run-length
   compression.  No more than sizeof_buf-1 characters are read so that
   the buffer can be NUL terminated.

   Returns -1 on error, number of characters in buffer (ignoring the
   trailing NULL) on success. (could be extended to return one of the
   SERIAL status indications). */

static long
read_frame (char *buf,
	    long sizeof_buf)
{
  unsigned char csum;
  long bc;
  int c;

  csum = 0;
  bc = 0;

  while (1)
    {
      /* ASSERT (bc < sizeof_buf - 1) - space for trailing NUL */
      c = readchar (remote_timeout);
      switch (c)
	{
	case SERIAL_TIMEOUT:
	  if (remote_debug)
	    fputs_filtered ("Timeout in mid-packet, retrying\n", gdb_stdlog);
	  return -1;
	case '$':
	  if (remote_debug)
	    fputs_filtered ("Saw new packet start in middle of old one\n",
			    gdb_stdlog);
	  return -1;		/* Start a new packet, count retries */
	case '#':
	  {
	    unsigned char pktcsum;
	    int check_0 = 0;
	    int check_1 = 0;

	    buf[bc] = '\0';

	    check_0 = readchar (remote_timeout);
	    if (check_0 >= 0)
	      check_1 = readchar (remote_timeout);
	    
	    if (check_0 == SERIAL_TIMEOUT || check_1 == SERIAL_TIMEOUT)
	      {
		if (remote_debug)
		  fputs_filtered ("Timeout in checksum, retrying\n", gdb_stdlog);
		return -1;
	      }
	    else if (check_0 < 0 || check_1 < 0)
	      {
		if (remote_debug)
		  fputs_filtered ("Communication error in checksum\n", gdb_stdlog);
		return -1;
	      }

	    pktcsum = (fromhex (check_0) << 4) | fromhex (check_1);
	    if (csum == pktcsum)
              return bc;

	    if (remote_debug)
	      {
		fprintf_filtered (gdb_stdlog,
			      "Bad checksum, sentsum=0x%x, csum=0x%x, buf=",
				  pktcsum, csum);
		fputs_filtered (buf, gdb_stdlog);
		fputs_filtered ("\n", gdb_stdlog);
	      }
	    /* Number of characters in buffer ignoring trailing
               NUL. */
	    return -1;
	  }
	case '*':		/* Run length encoding */
          {
	    int repeat;
 	    csum += c;

	    if (remote_cisco_mode == 0)
	      {
		c = readchar (remote_timeout);
		csum += c;
		repeat = c - ' ' + 3;	/* Compute repeat count */
	      }
	    else 
	      { 
		/* Cisco's run-length encoding variant uses two 
		   hex chars to represent the repeat count. */

		c = readchar (remote_timeout);
		csum += c;
		repeat  = fromhex (c) << 4;
		c = readchar (remote_timeout);
		csum += c;
		repeat += fromhex (c);
	      }

	    /* The character before ``*'' is repeated. */

	    if (repeat > 0 && repeat <= 255 
		&& bc > 0
                && bc + repeat - 1 < sizeof_buf - 1)
	      {
		memset (&buf[bc], buf[bc - 1], repeat);
		bc += repeat;
		continue;
	      }

	    buf[bc] = '\0';
	    printf_filtered ("Repeat count %d too large for buffer: ", repeat);
	    puts_filtered (buf);
	    puts_filtered ("\n");
	    return -1;
	  }
	default:
	  if (bc < sizeof_buf - 1)
	    {
	      buf[bc++] = c;
	      csum += c;
	      continue;
	    }

	  buf[bc] = '\0';
	  puts_filtered ("Remote packet too long: ");
	  puts_filtered (buf);
	  puts_filtered ("\n");

	  return -1;
	}
    }
}

/* Read a packet from the remote machine, with error checking, and
   store it in BUF.  If FOREVER, wait forever rather than timing out;
   this is used (in synchronous mode) to wait for a target that is is
   executing user code to stop.  */
/* FIXME: ezannoni 2000-02-01 this wrapper is necessary so that we
   don't have to change all the calls to getpkt to deal with the
   return value, because at the moment I don't know what the right
   thing to do it for those. */
void
getpkt (char *buf,
	long sizeof_buf,
	int forever)
{
  int timed_out;

  timed_out = getpkt_sane (buf, sizeof_buf, forever);
}


/* Read a packet from the remote machine, with error checking, and
   store it in BUF.  If FOREVER, wait forever rather than timing out;
   this is used (in synchronous mode) to wait for a target that is is
   executing user code to stop. If FOREVER == 0, this function is
   allowed to time out gracefully and return an indication of this to
   the caller. */
static int
getpkt_sane (char *buf,
	long sizeof_buf,
	int forever)
{
  int c;
  int tries;
  int timeout;
  int val;

  strcpy (buf, "timeout");

  if (forever)
    {
      timeout = watchdog > 0 ? watchdog : -1;
    }

  else
    timeout = remote_timeout;

#define MAX_TRIES 3

  for (tries = 1; tries <= MAX_TRIES; tries++)
    {
      /* This can loop forever if the remote side sends us characters
         continuously, but if it pauses, we'll get a zero from readchar
         because of timeout.  Then we'll count that as a retry.  */

      /* Note that we will only wait forever prior to the start of a packet.
         After that, we expect characters to arrive at a brisk pace.  They
         should show up within remote_timeout intervals.  */

      do
	{
	  c = readchar (timeout);

	  if (c == SERIAL_TIMEOUT)
	    {
	      if (forever)	/* Watchdog went off?  Kill the target. */
		{
		  QUIT;
		  target_mourn_inferior ();
		  error ("Watchdog has expired.  Target detached.\n");
		}
	      if (remote_debug)
		fputs_filtered ("Timed out.\n", gdb_stdlog);
	      goto retry;
	    }
	}
      while (c != '$');

      /* We've found the start of a packet, now collect the data.  */

      val = read_frame (buf, sizeof_buf);

      if (val >= 0)
	{
	  if (remote_debug)
	    {
	      fprintf_unfiltered (gdb_stdlog, "Packet received: ");
	      fputstr_unfiltered (buf, 0, gdb_stdlog);
	      fprintf_unfiltered (gdb_stdlog, "\n");
	    }
	  serial_write (remote_desc, "+", 1);
	  return 0;
	}

      /* Try the whole thing again.  */
    retry:
      serial_write (remote_desc, "-", 1);
    }

  /* We have tried hard enough, and just can't receive the packet.  Give up. */

  printf_unfiltered ("Ignoring packet error, continuing...\n");
  serial_write (remote_desc, "+", 1);
  return 1;
}

static void
remote_kill (void)
{
  /* For some mysterious reason, wait_for_inferior calls kill instead of
     mourn after it gets TARGET_WAITKIND_SIGNALLED.  Work around it.  */
  if (kill_kludge)
    {
      kill_kludge = 0;
      target_mourn_inferior ();
      return;
    }

  /* Use catch_errors so the user can quit from gdb even when we aren't on
     speaking terms with the remote system.  */
  catch_errors ((catch_errors_ftype *) putpkt, "k", "", RETURN_MASK_ERROR);

  /* Don't wait for it to die.  I'm not really sure it matters whether
     we do or not.  For the existing stubs, kill is a noop.  */
  target_mourn_inferior ();
}

/* Async version of remote_kill. */
static void
remote_async_kill (void)
{
  /* Unregister the file descriptor from the event loop. */
  if (target_is_async_p ())
    serial_async (remote_desc, NULL, 0);

  /* For some mysterious reason, wait_for_inferior calls kill instead of
     mourn after it gets TARGET_WAITKIND_SIGNALLED.  Work around it.  */
  if (kill_kludge)
    {
      kill_kludge = 0;
      target_mourn_inferior ();
      return;
    }

  /* Use catch_errors so the user can quit from gdb even when we aren't on
     speaking terms with the remote system.  */
  catch_errors ((catch_errors_ftype *) putpkt, "k", "", RETURN_MASK_ERROR);

  /* Don't wait for it to die.  I'm not really sure it matters whether
     we do or not.  For the existing stubs, kill is a noop.  */
  target_mourn_inferior ();
}

static void
remote_mourn (void)
{
  remote_mourn_1 (&remote_ops);
}

static void
remote_async_mourn (void)
{
  remote_mourn_1 (&remote_async_ops);
}

static void
extended_remote_mourn (void)
{
  /* We do _not_ want to mourn the target like this; this will
     remove the extended remote target  from the target stack,
     and the next time the user says "run" it'll fail. 

     FIXME: What is the right thing to do here?  */
#if 0
  remote_mourn_1 (&extended_remote_ops);
#endif
}

/* Worker function for remote_mourn.  */
static void
remote_mourn_1 (struct target_ops *target)
{
  unpush_target (target);
  generic_mourn_inferior ();
}

/* In the extended protocol we want to be able to do things like
   "run" and have them basically work as expected.  So we need
   a special create_inferior function. 

   FIXME: One day add support for changing the exec file
   we're debugging, arguments and an environment.  */

static void
extended_remote_create_inferior (char *exec_file, char *args, char **env)
{
  /* Rip out the breakpoints; we'll reinsert them after restarting
     the remote server.  */
  remove_breakpoints ();

  /* Now restart the remote server.  */
  extended_remote_restart ();

  /* Now put the breakpoints back in.  This way we're safe if the
     restart function works via a unix fork on the remote side.  */
  insert_breakpoints ();

  /* Clean up from the last time we were running.  */
  clear_proceed_status ();

  /* Let the remote process run.  */
  proceed (-1, TARGET_SIGNAL_0, 0);
}

/* Async version of extended_remote_create_inferior. */
static void
extended_remote_async_create_inferior (char *exec_file, char *args, char **env)
{
  /* Rip out the breakpoints; we'll reinsert them after restarting
     the remote server.  */
  remove_breakpoints ();

  /* If running asynchronously, register the target file descriptor
     with the event loop. */
  if (event_loop_p && target_can_async_p ())
    target_async (inferior_event_handler, 0);

  /* Now restart the remote server.  */
  extended_remote_restart ();

  /* Now put the breakpoints back in.  This way we're safe if the
     restart function works via a unix fork on the remote side.  */
  insert_breakpoints ();

  /* Clean up from the last time we were running.  */
  clear_proceed_status ();

  /* Let the remote process run.  */
  proceed (-1, TARGET_SIGNAL_0, 0);
}


/* On some machines, e.g. 68k, we may use a different breakpoint instruction
   than other targets; in those use REMOTE_BREAKPOINT instead of just
   BREAKPOINT.  Also, bi-endian targets may define LITTLE_REMOTE_BREAKPOINT
   and BIG_REMOTE_BREAKPOINT.  If none of these are defined, we just call
   the standard routines that are in mem-break.c.  */

/* FIXME, these ought to be done in a more dynamic fashion.  For instance,
   the choice of breakpoint instruction affects target program design and
   vice versa, and by making it user-tweakable, the special code here
   goes away and we need fewer special GDB configurations.  */

#if defined (LITTLE_REMOTE_BREAKPOINT) && defined (BIG_REMOTE_BREAKPOINT) && !defined(REMOTE_BREAKPOINT)
#define REMOTE_BREAKPOINT
#endif

#ifdef REMOTE_BREAKPOINT

/* If the target isn't bi-endian, just pretend it is.  */
#if !defined (LITTLE_REMOTE_BREAKPOINT) && !defined (BIG_REMOTE_BREAKPOINT)
#define LITTLE_REMOTE_BREAKPOINT REMOTE_BREAKPOINT
#define BIG_REMOTE_BREAKPOINT REMOTE_BREAKPOINT
#endif

static unsigned char big_break_insn[] = BIG_REMOTE_BREAKPOINT;
static unsigned char little_break_insn[] = LITTLE_REMOTE_BREAKPOINT;

#endif /* REMOTE_BREAKPOINT */

/* Insert a breakpoint on targets that don't have any better breakpoint
   support.  We read the contents of the target location and stash it,
   then overwrite it with a breakpoint instruction.  ADDR is the target
   location in the target machine.  CONTENTS_CACHE is a pointer to 
   memory allocated for saving the target contents.  It is guaranteed
   by the caller to be long enough to save sizeof BREAKPOINT bytes (this
   is accomplished via BREAKPOINT_MAX).  */

static int
remote_insert_breakpoint (CORE_ADDR addr, char *contents_cache)
{
#ifdef REMOTE_BREAKPOINT
  int val;
#endif  
  int bp_size;

  /* Try the "Z" s/w breakpoint packet if it is not already disabled.
     If it succeeds, then set the support to PACKET_ENABLE.  If it
     fails, and the user has explicitly requested the Z support then
     report an error, otherwise, mark it disabled and go on. */
  
  if (remote_protocol_Z[Z_PACKET_SOFTWARE_BP].support != PACKET_DISABLE)
    {
      char *buf = alloca (PBUFSIZ);
      char *p = buf;
      
      addr = remote_address_masked (addr);
      *(p++) = 'Z';
      *(p++) = '0';
      *(p++) = ',';
      p += hexnumstr (p, (ULONGEST) addr);
      BREAKPOINT_FROM_PC (&addr, &bp_size);
      sprintf (p, ",%d", bp_size);
      
      putpkt (buf);
      getpkt (buf, PBUFSIZ, 0);

      switch (packet_ok (buf, &remote_protocol_Z[Z_PACKET_SOFTWARE_BP]))
	{
	case PACKET_ERROR:
	  return -1;
	case PACKET_OK:
	  return 0;
	case PACKET_UNKNOWN:
	  break;
	}
    }

#ifdef REMOTE_BREAKPOINT  
  val = target_read_memory (addr, contents_cache, sizeof big_break_insn);

  if (val == 0)
    {
      if (TARGET_BYTE_ORDER == BIG_ENDIAN)
	val = target_write_memory (addr, (char *) big_break_insn,
				   sizeof big_break_insn);
      else
	val = target_write_memory (addr, (char *) little_break_insn,
				   sizeof little_break_insn);
    }

  return val;
#else
  return memory_insert_breakpoint (addr, contents_cache);
#endif /* REMOTE_BREAKPOINT */
}

static int
remote_remove_breakpoint (CORE_ADDR addr, char *contents_cache)
{
  int bp_size;

  if (remote_protocol_Z[Z_PACKET_SOFTWARE_BP].support != PACKET_DISABLE)
    {
      char *buf = alloca (PBUFSIZ);
      char *p = buf;
      
      *(p++) = 'z';
      *(p++) = '0';
      *(p++) = ',';

      addr = remote_address_masked (addr);
      p += hexnumstr (p, (ULONGEST) addr);
      BREAKPOINT_FROM_PC (&addr, &bp_size);
      sprintf (p, ",%d", bp_size);
      
      putpkt (buf);
      getpkt (buf, PBUFSIZ, 0);

      return (buf[0] == 'E');
    }

#ifdef REMOTE_BREAKPOINT
  return target_write_memory (addr, contents_cache, sizeof big_break_insn);
#else
  return memory_remove_breakpoint (addr, contents_cache);
#endif /* REMOTE_BREAKPOINT */
}

static int
watchpoint_to_Z_packet (int type)
{
  switch (type)
    {
    case hw_write:
      return 2;
      break;
    case hw_read:
      return 3;
      break;
    case hw_access:
      return 4;
      break;
    default:
      internal_error (__FILE__, __LINE__,
		      "hw_bp_to_z: bad watchpoint type %d", type);
    }
}

/* FIXME: This function should be static and a member of the remote
   target vector. */

int
remote_insert_watchpoint (CORE_ADDR addr, int len, int type)
{
  char *buf = alloca (PBUFSIZ);
  char *p;
  enum Z_packet_type packet = watchpoint_to_Z_packet (type);

  if (remote_protocol_Z[packet].support == PACKET_DISABLE)
    error ("Can't set hardware watchpoints without the '%s' (%s) packet\n",
	   remote_protocol_Z[packet].name,
	   remote_protocol_Z[packet].title);
  
  sprintf (buf, "Z%x,", packet);
  p = strchr (buf, '\0');
  addr = remote_address_masked (addr);
  p += hexnumstr (p, (ULONGEST) addr);
  sprintf (p, ",%x", len);
  
  putpkt (buf);
  getpkt (buf, PBUFSIZ, 0);

  switch (packet_ok (buf, &remote_protocol_Z[packet]))
    {
    case PACKET_ERROR:
    case PACKET_UNKNOWN:
      return -1;
    case PACKET_OK:
      return 0;
    }
  internal_error (__FILE__, __LINE__,
		  "remote_insert_watchpoint: reached end of function");
}

/* FIXME: This function should be static and a member of the remote
   target vector. */

int
remote_remove_watchpoint (CORE_ADDR addr, int len, int type)
{
  char *buf = alloca (PBUFSIZ);
  char *p;
  enum Z_packet_type packet = watchpoint_to_Z_packet (type);

  if (remote_protocol_Z[packet].support == PACKET_DISABLE)
    error ("Can't clear hardware watchpoints without the '%s' (%s) packet\n",
	   remote_protocol_Z[packet].name,
	   remote_protocol_Z[packet].title);
  
  sprintf (buf, "z%x,", packet);
  p = strchr (buf, '\0');
  addr = remote_address_masked (addr);
  p += hexnumstr (p, (ULONGEST) addr);
  sprintf (p, ",%x", len);
  putpkt (buf);
  getpkt (buf, PBUFSIZ, 0);

  switch (packet_ok (buf, &remote_protocol_Z[packet]))
    {
    case PACKET_ERROR:
    case PACKET_UNKNOWN:
      return -1;
    case PACKET_OK:
      return 0;
    }
  internal_error (__FILE__, __LINE__,
		  "remote_remove_watchpoint: reached end of function");
}

/* FIXME: This function should be static and a member of the remote
   target vector. */

int
remote_insert_hw_breakpoint (CORE_ADDR addr, int len)
{
  char *buf = alloca (PBUFSIZ);
  char *p = buf;
      
  if (remote_protocol_Z[Z_PACKET_HARDWARE_BP].support == PACKET_DISABLE)
    error ("Can't set hardware breakpoint without the '%s' (%s) packet\n",
	   remote_protocol_Z[Z_PACKET_HARDWARE_BP].name,
	   remote_protocol_Z[Z_PACKET_HARDWARE_BP].title);
  
  *(p++) = 'Z';
  *(p++) = '1';
  *(p++) = ',';
  
  addr = remote_address_masked (addr);
  p += hexnumstr (p, (ULONGEST) addr);
  sprintf (p, ",%x", len);

  putpkt (buf);
  getpkt (buf, PBUFSIZ, 0);

  switch (packet_ok (buf, &remote_protocol_Z[Z_PACKET_HARDWARE_BP]))
    {
    case PACKET_ERROR:
    case PACKET_UNKNOWN:
      return -1;
    case PACKET_OK:
      return 0;
    }
  internal_error (__FILE__, __LINE__,
		  "remote_remove_watchpoint: reached end of function");
}

/* FIXME: This function should be static and a member of the remote
   target vector. */

int 
remote_remove_hw_breakpoint (CORE_ADDR addr, int len)
{
  char *buf = alloca (PBUFSIZ);
  char *p = buf;
  
  if (remote_protocol_Z[Z_PACKET_HARDWARE_BP].support == PACKET_DISABLE)
    error ("Can't clear hardware breakpoint without the '%s' (%s) packet\n",
	   remote_protocol_Z[Z_PACKET_HARDWARE_BP].name,
	   remote_protocol_Z[Z_PACKET_HARDWARE_BP].title);
  
  *(p++) = 'z';
  *(p++) = '1';
  *(p++) = ',';
  
  addr = remote_address_masked (addr);
  p += hexnumstr (p, (ULONGEST) addr);
  sprintf (p, ",%x", len);

  putpkt(buf);
  getpkt (buf, PBUFSIZ, 0);
  
  switch (packet_ok (buf, &remote_protocol_Z[Z_PACKET_HARDWARE_BP]))
    {
    case PACKET_ERROR:
    case PACKET_UNKNOWN:
      return -1;
    case PACKET_OK:
      return 0;
    }
  internal_error (__FILE__, __LINE__,
		  "remote_remove_watchpoint: reached end of function");
}

/* Some targets are only capable of doing downloads, and afterwards
   they switch to the remote serial protocol.  This function provides
   a clean way to get from the download target to the remote target.
   It's basically just a wrapper so that we don't have to expose any
   of the internal workings of remote.c.

   Prior to calling this routine, you should shutdown the current
   target code, else you will get the "A program is being debugged
   already..." message.  Usually a call to pop_target() suffices.  */

void
push_remote_target (char *name, int from_tty)
{
  printf_filtered ("Switching to remote protocol\n");
  remote_open (name, from_tty);
}

/* Other targets want to use the entire remote serial module but with
   certain remote_ops overridden. */

void
open_remote_target (char *name, int from_tty, struct target_ops *target,
		    int extended_p)
{
  printf_filtered ("Selecting the %sremote protocol\n",
		   (extended_p ? "extended-" : ""));
  remote_open_1 (name, from_tty, target, extended_p);
}

/* Table used by the crc32 function to calcuate the checksum. */

static unsigned long crc32_table[256] =
{0, 0};

static unsigned long
crc32 (unsigned char *buf, int len, unsigned int crc)
{
  if (!crc32_table[1])
    {
      /* Initialize the CRC table and the decoding table. */
      int i, j;
      unsigned int c;

      for (i = 0; i < 256; i++)
	{
	  for (c = i << 24, j = 8; j > 0; --j)
	    c = c & 0x80000000 ? (c << 1) ^ 0x04c11db7 : (c << 1);
	  crc32_table[i] = c;
	}
    }

  while (len--)
    {
      crc = (crc << 8) ^ crc32_table[((crc >> 24) ^ *buf) & 255];
      buf++;
    }
  return crc;
}

/* compare-sections command

   With no arguments, compares each loadable section in the exec bfd
   with the same memory range on the target, and reports mismatches.
   Useful for verifying the image on the target against the exec file.
   Depends on the target understanding the new "qCRC:" request.  */

/* FIXME: cagney/1999-10-26: This command should be broken down into a
   target method (target verify memory) and generic version of the
   actual command.  This will allow other high-level code (especially
   generic_load()) to make use of this target functionality. */

static void
compare_sections_command (char *args, int from_tty)
{
  asection *s;
  unsigned long host_crc, target_crc;
  extern bfd *exec_bfd;
  struct cleanup *old_chain;
  char *tmp;
  char *sectdata;
  const char *sectname;
  char *buf = alloca (PBUFSIZ);
  bfd_size_type size;
  bfd_vma lma;
  int matched = 0;
  int mismatched = 0;

  if (!exec_bfd)
    error ("command cannot be used without an exec file");
  if (!current_target.to_shortname ||
      strcmp (current_target.to_shortname, "remote") != 0)
    error ("command can only be used with remote target");

  for (s = exec_bfd->sections; s; s = s->next)
    {
      if (!(s->flags & SEC_LOAD))
	continue;		/* skip non-loadable section */

      size = bfd_get_section_size_before_reloc (s);
      if (size == 0)
	continue;		/* skip zero-length section */

      sectname = bfd_get_section_name (exec_bfd, s);
      if (args && strcmp (args, sectname) != 0)
	continue;		/* not the section selected by user */

      matched = 1;		/* do this section */
      lma = s->lma;
      /* FIXME: assumes lma can fit into long */
      sprintf (buf, "qCRC:%lx,%lx", (long) lma, (long) size);
      putpkt (buf);

      /* be clever; compute the host_crc before waiting for target reply */
      sectdata = xmalloc (size);
      old_chain = make_cleanup (xfree, sectdata);
      bfd_get_section_contents (exec_bfd, s, sectdata, 0, size);
      host_crc = crc32 ((unsigned char *) sectdata, size, 0xffffffff);

      getpkt (buf, PBUFSIZ, 0);
      if (buf[0] == 'E')
	error ("target memory fault, section %s, range 0x%08x -- 0x%08x",
	       sectname, lma, lma + size);
      if (buf[0] != 'C')
	error ("remote target does not support this operation");

      for (target_crc = 0, tmp = &buf[1]; *tmp; tmp++)
	target_crc = target_crc * 16 + fromhex (*tmp);

      printf_filtered ("Section %s, range 0x%s -- 0x%s: ",
		       sectname, paddr (lma), paddr (lma + size));
      if (host_crc == target_crc)
	printf_filtered ("matched.\n");
      else
	{
	  printf_filtered ("MIS-MATCHED!\n");
	  mismatched++;
	}

      do_cleanups (old_chain);
    }
  if (mismatched > 0)
    warning ("One or more sections of the remote executable does not match\n\
the loaded file\n");
  if (args && !matched)
    printf_filtered ("No loaded section named '%s'.\n", args);
}

static int
remote_query (int query_type, char *buf, char *outbuf, int *bufsiz)
{
  int i;
  char *buf2 = alloca (PBUFSIZ);
  char *p2 = &buf2[0];

  if (!bufsiz)
    error ("null pointer to remote bufer size specified");

  /* minimum outbuf size is PBUFSIZ - if bufsiz is not large enough let 
     the caller know and return what the minimum size is   */
  /* Note: a zero bufsiz can be used to query the minimum buffer size */
  if (*bufsiz < PBUFSIZ)
    {
      *bufsiz = PBUFSIZ;
      return -1;
    }

  /* except for querying the minimum buffer size, target must be open */
  if (!remote_desc)
    error ("remote query is only available after target open");

  /* we only take uppercase letters as query types, at least for now */
  if ((query_type < 'A') || (query_type > 'Z'))
    error ("invalid remote query type");

  if (!buf)
    error ("null remote query specified");

  if (!outbuf)
    error ("remote query requires a buffer to receive data");

  outbuf[0] = '\0';

  *p2++ = 'q';
  *p2++ = query_type;

  /* we used one buffer char for the remote protocol q command and another
     for the query type.  As the remote protocol encapsulation uses 4 chars
     plus one extra in case we are debugging (remote_debug),
     we have PBUFZIZ - 7 left to pack the query string */
  i = 0;
  while (buf[i] && (i < (PBUFSIZ - 8)))
    {
      /* bad caller may have sent forbidden characters */
      if ((!isprint (buf[i])) || (buf[i] == '$') || (buf[i] == '#'))
	error ("illegal characters in query string");

      *p2++ = buf[i];
      i++;
    }
  *p2 = buf[i];

  if (buf[i])
    error ("query larger than available buffer");

  i = putpkt (buf2);
  if (i < 0)
    return i;

  getpkt (outbuf, *bufsiz, 0);

  return 0;
}

static void
remote_rcmd (char *command,
	     struct ui_file *outbuf)
{
  int i;
  char *buf = alloca (PBUFSIZ);
  char *p = buf;

  if (!remote_desc)
    error ("remote rcmd is only available after target open");

  /* Send a NULL command across as an empty command */
  if (command == NULL)
    command = "";

  /* The query prefix */
  strcpy (buf, "qRcmd,");
  p = strchr (buf, '\0');

  if ((strlen (buf) + strlen (command) * 2 + 8/*misc*/) > PBUFSIZ)
    error ("\"monitor\" command ``%s'' is too long\n", command);

  /* Encode the actual command */
  bin2hex (command, p, 0);

  if (putpkt (buf) < 0)
    error ("Communication problem with target\n");

  /* get/display the response */
  while (1)
    {
      /* XXX - see also tracepoint.c:remote_get_noisy_reply() */
      buf[0] = '\0';
      getpkt (buf, PBUFSIZ, 0);
      if (buf[0] == '\0')
	error ("Target does not support this command\n");
      if (buf[0] == 'O' && buf[1] != 'K')
	{
	  remote_console_output (buf + 1); /* 'O' message from stub */
	  continue;
	}
      if (strcmp (buf, "OK") == 0)
	break;
      if (strlen (buf) == 3 && buf[0] == 'E'
	  && isdigit (buf[1]) && isdigit (buf[2]))
	{
	  error ("Protocol error with Rcmd");
	}
      for (p = buf; p[0] != '\0' && p[1] != '\0'; p += 2)
	{
	  char c = (fromhex (p[0]) << 4) + fromhex (p[1]);
	  fputc_unfiltered (c, outbuf);
	}
      break;
    }
}

static void
packet_command (char *args, int from_tty)
{
  char *buf = alloca (PBUFSIZ);

  if (!remote_desc)
    error ("command can only be used with remote target");

  if (!args)
    error ("remote-packet command requires packet text as argument");

  puts_filtered ("sending: ");
  print_packet (args);
  puts_filtered ("\n");
  putpkt (args);

  getpkt (buf, PBUFSIZ, 0);
  puts_filtered ("received: ");
  print_packet (buf);
  puts_filtered ("\n");
}

#if 0
/* --------- UNIT_TEST for THREAD oriented PACKETS ------------------------- */

static void display_thread_info (struct gdb_ext_thread_info *info);

static void threadset_test_cmd (char *cmd, int tty);

static void threadalive_test (char *cmd, int tty);

static void threadlist_test_cmd (char *cmd, int tty);

int get_and_display_threadinfo (threadref * ref);

static void threadinfo_test_cmd (char *cmd, int tty);

static int thread_display_step (threadref * ref, void *context);

static void threadlist_update_test_cmd (char *cmd, int tty);

static void init_remote_threadtests (void);

#define SAMPLE_THREAD  0x05060708	/* Truncated 64 bit threadid */

static void
threadset_test_cmd (char *cmd, int tty)
{
  int sample_thread = SAMPLE_THREAD;

  printf_filtered ("Remote threadset test\n");
  set_thread (sample_thread, 1);
}


static void
threadalive_test (char *cmd, int tty)
{
  int sample_thread = SAMPLE_THREAD;

  if (remote_thread_alive (pid_to_ptid (sample_thread)))
    printf_filtered ("PASS: Thread alive test\n");
  else
    printf_filtered ("FAIL: Thread alive test\n");
}

void output_threadid (char *title, threadref * ref);

void
output_threadid (char *title, threadref *ref)
{
  char hexid[20];

  pack_threadid (&hexid[0], ref);	/* Convert threead id into hex */
  hexid[16] = 0;
  printf_filtered ("%s  %s\n", title, (&hexid[0]));
}

static void
threadlist_test_cmd (char *cmd, int tty)
{
  int startflag = 1;
  threadref nextthread;
  int done, result_count;
  threadref threadlist[3];

  printf_filtered ("Remote Threadlist test\n");
  if (!remote_get_threadlist (startflag, &nextthread, 3, &done,
			      &result_count, &threadlist[0]))
    printf_filtered ("FAIL: threadlist test\n");
  else
    {
      threadref *scan = threadlist;
      threadref *limit = scan + result_count;

      while (scan < limit)
	output_threadid (" thread ", scan++);
    }
}

void
display_thread_info (struct gdb_ext_thread_info *info)
{
  output_threadid ("Threadid: ", &info->threadid);
  printf_filtered ("Name: %s\n ", info->shortname);
  printf_filtered ("State: %s\n", info->display);
  printf_filtered ("other: %s\n\n", info->more_display);
}

int
get_and_display_threadinfo (threadref *ref)
{
  int result;
  int set;
  struct gdb_ext_thread_info threadinfo;

  set = TAG_THREADID | TAG_EXISTS | TAG_THREADNAME
    | TAG_MOREDISPLAY | TAG_DISPLAY;
  if (0 != (result = remote_get_threadinfo (ref, set, &threadinfo)))
    display_thread_info (&threadinfo);
  return result;
}

static void
threadinfo_test_cmd (char *cmd, int tty)
{
  int athread = SAMPLE_THREAD;
  threadref thread;
  int set;

  int_to_threadref (&thread, athread);
  printf_filtered ("Remote Threadinfo test\n");
  if (!get_and_display_threadinfo (&thread))
    printf_filtered ("FAIL cannot get thread info\n");
}

static int
thread_display_step (threadref *ref, void *context)
{
  /* output_threadid(" threadstep ",ref); *//* simple test */
  return get_and_display_threadinfo (ref);
}

static void
threadlist_update_test_cmd (char *cmd, int tty)
{
  printf_filtered ("Remote Threadlist update test\n");
  remote_threadlist_iterator (thread_display_step, 0, CRAZY_MAX_THREADS);
}

static void
init_remote_threadtests (void)
{
  add_com ("tlist", class_obscure, threadlist_test_cmd,
     "Fetch and print the remote list of thread identifiers, one pkt only");
  add_com ("tinfo", class_obscure, threadinfo_test_cmd,
	   "Fetch and display info about one thread");
  add_com ("tset", class_obscure, threadset_test_cmd,
	   "Test setting to a different thread");
  add_com ("tupd", class_obscure, threadlist_update_test_cmd,
	   "Iterate through updating all remote thread info");
  add_com ("talive", class_obscure, threadalive_test,
	   " Remote thread alive test ");
}

#endif /* 0 */

/* Convert a thread ID to a string.  Returns the string in a static
   buffer.  */

static char *
remote_pid_to_str (ptid_t ptid)
{
  static char buf[30];

  sprintf (buf, "Thread %d", PIDGET (ptid));
  return buf;
}

static void
init_remote_ops (void)
{
  remote_ops.to_shortname = "remote";
  remote_ops.to_longname = "Remote serial target in gdb-specific protocol";
  remote_ops.to_doc =
    "Use a remote computer via a serial line, using a gdb-specific protocol.\n\
Specify the serial device it is connected to\n\
(e.g. /dev/ttyS0, /dev/ttya, COM1, etc.).";
  remote_ops.to_open = remote_open;
  remote_ops.to_close = remote_close;
  remote_ops.to_detach = remote_detach;
  remote_ops.to_resume = remote_resume;
  remote_ops.to_wait = remote_wait;
  remote_ops.to_fetch_registers = remote_fetch_registers;
  remote_ops.to_store_registers = remote_store_registers;
  remote_ops.to_prepare_to_store = remote_prepare_to_store;
  remote_ops.to_xfer_memory = remote_xfer_memory;
  remote_ops.to_files_info = remote_files_info;
  remote_ops.to_insert_breakpoint = remote_insert_breakpoint;
  remote_ops.to_remove_breakpoint = remote_remove_breakpoint;
  remote_ops.to_kill = remote_kill;
  remote_ops.to_load = generic_load;
  remote_ops.to_mourn_inferior = remote_mourn;
  remote_ops.to_thread_alive = remote_thread_alive;
  remote_ops.to_find_new_threads = remote_threads_info;
  remote_ops.to_pid_to_str = remote_pid_to_str;
  remote_ops.to_extra_thread_info = remote_threads_extra_info;
  remote_ops.to_stop = remote_stop;
  remote_ops.to_query = remote_query;
  remote_ops.to_rcmd = remote_rcmd;
  remote_ops.to_stratum = process_stratum;
  remote_ops.to_has_all_memory = 1;
  remote_ops.to_has_memory = 1;
  remote_ops.to_has_stack = 1;
  remote_ops.to_has_registers = 1;
  remote_ops.to_has_execution = 1;
  remote_ops.to_has_thread_control = tc_schedlock;	/* can lock scheduler */
  remote_ops.to_magic = OPS_MAGIC;
}

/* Set up the extended remote vector by making a copy of the standard
   remote vector and adding to it.  */

static void
init_extended_remote_ops (void)
{
  extended_remote_ops = remote_ops;

  extended_remote_ops.to_shortname = "extended-remote";
  extended_remote_ops.to_longname =
    "Extended remote serial target in gdb-specific protocol";
  extended_remote_ops.to_doc =
    "Use a remote computer via a serial line, using a gdb-specific protocol.\n\
Specify the serial device it is connected to (e.g. /dev/ttya).",
    extended_remote_ops.to_open = extended_remote_open;
  extended_remote_ops.to_create_inferior = extended_remote_create_inferior;
  extended_remote_ops.to_mourn_inferior = extended_remote_mourn;
}

/*
 * Command: info remote-process
 *
 * This implements Cisco's version of the "info proc" command.
 *
 * This query allows the target stub to return an arbitrary string
 * (or strings) giving arbitrary information about the target process.
 * This is optional; the target stub isn't required to implement it.
 *
 * Syntax: qfProcessInfo        request first string
 *         qsProcessInfo        request subsequent string
 * reply:  'O'<hex-encoded-string>
 *         'l'                  last reply (empty)
 */

static void
remote_info_process (char *args, int from_tty)
{
  char *buf = alloca (PBUFSIZ);

  if (remote_desc == 0)
    error ("Command can only be used when connected to the remote target.");

  putpkt ("qfProcessInfo");
  getpkt (buf, PBUFSIZ, 0);
  if (buf[0] == 0)
    return;			/* Silently: target does not support this feature. */

  if (buf[0] == 'E')
    error ("info proc: target error.");

  while (buf[0] == 'O')		/* Capitol-O packet */
    {
      remote_console_output (&buf[1]);
      putpkt ("qsProcessInfo");
      getpkt (buf, PBUFSIZ, 0);
    }
}

/*
 * Target Cisco 
 */

static void
remote_cisco_open (char *name, int from_tty)
{
  if (name == 0)
    error (
	    "To open a remote debug connection, you need to specify what \n\
device is attached to the remote system (e.g. host:port).");

  /* See FIXME above */
  wait_forever_enabled_p = 1;

  target_preopen (from_tty);

  unpush_target (&remote_cisco_ops);

  remote_desc = serial_open (name);
  if (!remote_desc)
    perror_with_name (name);

  /*
   * If a baud rate was specified on the gdb  command line it will
   * be greater than the initial value of -1.  If it is, use it otherwise
   * default to 9600
   */

  baud_rate = (baud_rate > 0) ? baud_rate : 9600;
  if (serial_setbaudrate (remote_desc, baud_rate))
    {
      serial_close (remote_desc);
      perror_with_name (name);
    }

  serial_raw (remote_desc);

  /* If there is something sitting in the buffer we might take it as a
     response to a command, which would be bad.  */
  serial_flush_input (remote_desc);

  if (from_tty)
    {
      puts_filtered ("Remote debugging using ");
      puts_filtered (name);
      puts_filtered ("\n");
    }

  remote_cisco_mode = 1;

  push_target (&remote_cisco_ops);	/* Switch to using cisco target now */

  init_all_packet_configs ();

  general_thread = -2;
  continue_thread = -2;

  /* Probe for ability to use "ThreadInfo" query, as required.  */
  use_threadinfo_query = 1;
  use_threadextra_query = 1;
  
  /* Without this, some commands which require an active target (such
     as kill) won't work.  This variable serves (at least) double duty
     as both the pid of the target process (if it has such), and as a
     flag indicating that a target is active.  These functions should
     be split out into seperate variables, especially since GDB will
     someday have a notion of debugging several processes.  */
  inferior_ptid = pid_to_ptid (MAGIC_NULL_PID);

  /* Start the remote connection; if error (0), discard this target. */

  if (!catch_errors (remote_start_remote_dummy, (char *) 0,
		     "Couldn't establish connection to remote target\n",
		     RETURN_MASK_ALL))
    {
      pop_target ();
      return;
    }
}

static void
remote_cisco_close (int quitting)
{
  remote_cisco_mode = 0;
  remote_close (quitting);
}

static void
remote_cisco_mourn (void)
{
  remote_mourn_1 (&remote_cisco_ops);
}

enum
{
  READ_MORE,
  FATAL_ERROR,
  ENTER_DEBUG,
  DISCONNECT_TELNET
}
minitelnet_return;

/* shared between readsocket() and readtty()  */
static char *tty_input;

static int escape_count;
static int echo_check;
extern int quit_flag;

static int
readsocket (void)
{
  int data;

  /* Loop until the socket doesn't have any more data */

  while ((data = readchar (0)) >= 0)
    {
      /* Check for the escape sequence */
      if (data == '|')
	{
	  /* If this is the fourth escape, get out */
	  if (++escape_count == 4)
	    {
	      return ENTER_DEBUG;
	    }
	  else
	    {			/* This is a '|', but not the fourth in a row. 
				   Continue without echoing it.  If it isn't actually 
				   one of four in a row, it'll be echoed later.  */
	      continue;
	    }
	}
      else
	/* Not a '|' */
	{
	  /* Ensure any pending '|'s are flushed.  */

	  for (; escape_count > 0; escape_count--)
	    putchar ('|');
	}

      if (data == '\r')		/* If this is a return character, */
	continue;		/*  - just supress it. */

      if (echo_check != -1)	/* Check for echo of user input.  */
	{
	  if (tty_input[echo_check] == data)
	    {
	      echo_check++;	/* Character matched user input: */
	      continue;		/* Continue without echoing it.  */
	    }
	  else if ((data == '\n') && (tty_input[echo_check] == '\r'))
	    {			/* End of the line (and of echo checking).  */
	      echo_check = -1;	/* No more echo supression */
	      continue;		/* Continue without echoing.  */
	    }
	  else
	    {			/* Failed check for echo of user input.
				   We now have some suppressed output to flush!  */
	      int j;

	      for (j = 0; j < echo_check; j++)
		putchar (tty_input[j]);
	      echo_check = -1;
	    }
	}
      putchar (data);		/* Default case: output the char.  */
    }

  if (data == SERIAL_TIMEOUT)	/* Timeout returned from readchar.  */
    return READ_MORE;		/* Try to read some more */
  else
    return FATAL_ERROR;		/* Trouble, bail out */
}

static int
readtty (void)
{
  int tty_bytecount;

  /* First, read a buffer full from the terminal */
  tty_bytecount = read (fileno (stdin), tty_input, sizeof (tty_input) - 1);
  if (tty_bytecount == -1)
    {
      perror ("readtty: read failed");
      return FATAL_ERROR;
    }

  /* Remove a quoted newline.  */
  if (tty_input[tty_bytecount - 1] == '\n' &&
      tty_input[tty_bytecount - 2] == '\\')	/* line ending in backslash */
    {
      tty_input[--tty_bytecount] = 0;	/* remove newline */
      tty_input[--tty_bytecount] = 0;	/* remove backslash */
    }

  /* Turn trailing newlines into returns */
  if (tty_input[tty_bytecount - 1] == '\n')
    tty_input[tty_bytecount - 1] = '\r';

  /* If the line consists of a ~, enter debugging mode.  */
  if ((tty_input[0] == '~') && (tty_bytecount == 2))
    return ENTER_DEBUG;

  /* Make this a zero terminated string and write it out */
  tty_input[tty_bytecount] = 0;
  if (serial_write (remote_desc, tty_input, tty_bytecount))
    {
      perror_with_name ("readtty: write failed");
      return FATAL_ERROR;
    }

  return READ_MORE;
}

static int
minitelnet (void)
{
  fd_set input;			/* file descriptors for select */
  int tablesize;		/* max number of FDs for select */
  int status;
  int quit_count = 0;

  extern int escape_count;	/* global shared by readsocket */
  extern int echo_check;	/* ditto */

  escape_count = 0;
  echo_check = -1;

  tablesize = 8 * sizeof (input);

  for (;;)
    {
      /* Check for anything from our socket - doesn't block. Note that
         this must be done *before* the select as there may be
         buffered I/O waiting to be processed.  */

      if ((status = readsocket ()) == FATAL_ERROR)
	{
	  error ("Debugging terminated by communications error");
	}
      else if (status != READ_MORE)
	{
	  return (status);
	}

      fflush (stdout);		/* Flush output before blocking */

      /* Now block on more socket input or TTY input */

      FD_ZERO (&input);
      FD_SET (fileno (stdin), &input);
      FD_SET (deprecated_serial_fd (remote_desc), &input);

      status = select (tablesize, &input, 0, 0, 0);
      if ((status == -1) && (errno != EINTR))
	{
	  error ("Communications error on select %d", errno);
	}

      /* Handle Control-C typed */

      if (quit_flag)
	{
	  if ((++quit_count) == 2)
	    {
	      if (query ("Interrupt GDB? "))
		{
		  printf_filtered ("Interrupted by user.\n");
		  return_to_top_level (RETURN_QUIT);
		}
	      quit_count = 0;
	    }
	  quit_flag = 0;

	  if (remote_break)
	    serial_send_break (remote_desc);
	  else
	    serial_write (remote_desc, "\003", 1);

	  continue;
	}

      /* Handle console input */

      if (FD_ISSET (fileno (stdin), &input))
	{
	  quit_count = 0;
	  echo_check = 0;
	  status = readtty ();
	  if (status == READ_MORE)
	    continue;

	  return status;	/* telnet session ended */
	}
    }
}

static ptid_t
remote_cisco_wait (ptid_t ptid, struct target_waitstatus *status)
{
  if (minitelnet () != ENTER_DEBUG)
    {
      error ("Debugging session terminated by protocol error");
    }
  putpkt ("?");
  return remote_wait (ptid, status);
}

static void
init_remote_cisco_ops (void)
{
  remote_cisco_ops.to_shortname = "cisco";
  remote_cisco_ops.to_longname = "Remote serial target in cisco-specific protocol";
  remote_cisco_ops.to_doc =
    "Use a remote machine via TCP, using a cisco-specific protocol.\n\
Specify the serial device it is connected to (e.g. host:2020).";
  remote_cisco_ops.to_open = remote_cisco_open;
  remote_cisco_ops.to_close = remote_cisco_close;
  remote_cisco_ops.to_detach = remote_detach;
  remote_cisco_ops.to_resume = remote_resume;
  remote_cisco_ops.to_wait = remote_cisco_wait;
  remote_cisco_ops.to_fetch_registers = remote_fetch_registers;
  remote_cisco_ops.to_store_registers = remote_store_registers;
  remote_cisco_ops.to_prepare_to_store = remote_prepare_to_store;
  remote_cisco_ops.to_xfer_memory = remote_xfer_memory;
  remote_cisco_ops.to_files_info = remote_files_info;
  remote_cisco_ops.to_insert_breakpoint = remote_insert_breakpoint;
  remote_cisco_ops.to_remove_breakpoint = remote_remove_breakpoint;
  remote_cisco_ops.to_kill = remote_kill;
  remote_cisco_ops.to_load = generic_load;
  remote_cisco_ops.to_mourn_inferior = remote_cisco_mourn;
  remote_cisco_ops.to_thread_alive = remote_thread_alive;
  remote_cisco_ops.to_find_new_threads = remote_threads_info;
  remote_cisco_ops.to_pid_to_str = remote_pid_to_str;
  remote_cisco_ops.to_extra_thread_info = remote_threads_extra_info;
  remote_cisco_ops.to_stratum = process_stratum;
  remote_cisco_ops.to_has_all_memory = 1;
  remote_cisco_ops.to_has_memory = 1;
  remote_cisco_ops.to_has_stack = 1;
  remote_cisco_ops.to_has_registers = 1;
  remote_cisco_ops.to_has_execution = 1;
  remote_cisco_ops.to_magic = OPS_MAGIC;
}

static int
remote_can_async_p (void)
{
  /* We're async whenever the serial device is. */
  return (current_target.to_async_mask_value) && serial_can_async_p (remote_desc);
}

static int
remote_is_async_p (void)
{
  /* We're async whenever the serial device is. */
  return (current_target.to_async_mask_value) && serial_is_async_p (remote_desc);
}

/* Pass the SERIAL event on and up to the client.  One day this code
   will be able to delay notifying the client of an event until the
   point where an entire packet has been received. */

static void (*async_client_callback) (enum inferior_event_type event_type, void *context);
static void *async_client_context;
static serial_event_ftype remote_async_serial_handler;

static void
remote_async_serial_handler (struct serial *scb, void *context)
{
  /* Don't propogate error information up to the client.  Instead let
     the client find out about the error by querying the target.  */
  async_client_callback (INF_REG_EVENT, async_client_context);
}

static void
remote_async (void (*callback) (enum inferior_event_type event_type, void *context), void *context)
{
  if (current_target.to_async_mask_value == 0)
    internal_error (__FILE__, __LINE__,
		    "Calling remote_async when async is masked");

  if (callback != NULL)
    {
      serial_async (remote_desc, remote_async_serial_handler, NULL);
      async_client_callback = callback;
      async_client_context = context;
    }
  else
    serial_async (remote_desc, NULL, NULL);
}

/* Target async and target extended-async.

   This are temporary targets, until it is all tested.  Eventually
   async support will be incorporated int the usual 'remote'
   target. */

static void
init_remote_async_ops (void)
{
  remote_async_ops.to_shortname = "async";
  remote_async_ops.to_longname = "Remote serial target in async version of the gdb-specific protocol";
  remote_async_ops.to_doc =
    "Use a remote computer via a serial line, using a gdb-specific protocol.\n\
Specify the serial device it is connected to (e.g. /dev/ttya).";
  remote_async_ops.to_open = remote_async_open;
  remote_async_ops.to_close = remote_close;
  remote_async_ops.to_detach = remote_async_detach;
  remote_async_ops.to_resume = remote_async_resume;
  remote_async_ops.to_wait = remote_async_wait;
  remote_async_ops.to_fetch_registers = remote_fetch_registers;
  remote_async_ops.to_store_registers = remote_store_registers;
  remote_async_ops.to_prepare_to_store = remote_prepare_to_store;
  remote_async_ops.to_xfer_memory = remote_xfer_memory;
  remote_async_ops.to_files_info = remote_files_info;
  remote_async_ops.to_insert_breakpoint = remote_insert_breakpoint;
  remote_async_ops.to_remove_breakpoint = remote_remove_breakpoint;
  remote_async_ops.to_terminal_inferior = remote_async_terminal_inferior;
  remote_async_ops.to_terminal_ours = remote_async_terminal_ours;
  remote_async_ops.to_kill = remote_async_kill;
  remote_async_ops.to_load = generic_load;
  remote_async_ops.to_mourn_inferior = remote_async_mourn;
  remote_async_ops.to_thread_alive = remote_thread_alive;
  remote_async_ops.to_find_new_threads = remote_threads_info;
  remote_async_ops.to_pid_to_str = remote_pid_to_str;
  remote_async_ops.to_extra_thread_info = remote_threads_extra_info;
  remote_async_ops.to_stop = remote_stop;
  remote_async_ops.to_query = remote_query;
  remote_async_ops.to_rcmd = remote_rcmd;
  remote_async_ops.to_stratum = process_stratum;
  remote_async_ops.to_has_all_memory = 1;
  remote_async_ops.to_has_memory = 1;
  remote_async_ops.to_has_stack = 1;
  remote_async_ops.to_has_registers = 1;
  remote_async_ops.to_has_execution = 1;
  remote_async_ops.to_has_thread_control = tc_schedlock;	/* can lock scheduler */
  remote_async_ops.to_can_async_p = remote_can_async_p;
  remote_async_ops.to_is_async_p = remote_is_async_p;
  remote_async_ops.to_async = remote_async;
  remote_async_ops.to_async_mask_value = 1;
  remote_async_ops.to_magic = OPS_MAGIC;
}

/* Set up the async extended remote vector by making a copy of the standard
   remote vector and adding to it.  */

static void
init_extended_async_remote_ops (void)
{
  extended_async_remote_ops = remote_async_ops;

  extended_async_remote_ops.to_shortname = "extended-async";
  extended_async_remote_ops.to_longname =
    "Extended remote serial target in async gdb-specific protocol";
  extended_async_remote_ops.to_doc =
    "Use a remote computer via a serial line, using an async gdb-specific protocol.\n\
Specify the serial device it is connected to (e.g. /dev/ttya).",
    extended_async_remote_ops.to_open = extended_remote_async_open;
  extended_async_remote_ops.to_create_inferior = extended_remote_async_create_inferior;
  extended_async_remote_ops.to_mourn_inferior = extended_remote_mourn;
}

static void
set_remote_cmd (char *args, int from_tty)
{
}

static void
show_remote_cmd (char *args, int from_tty)
{
  
  show_remote_protocol_Z_packet_cmd (args, from_tty);
  show_remote_protocol_e_packet_cmd (args, from_tty);
  show_remote_protocol_E_packet_cmd (args, from_tty);
  show_remote_protocol_P_packet_cmd (args, from_tty);
  show_remote_protocol_qSymbol_packet_cmd (args, from_tty);
  show_remote_protocol_binary_download_cmd (args, from_tty);
}

static void
build_remote_gdbarch_data (void)
{
  build_remote_packet_sizes ();

  /* Cisco stuff */
  tty_input = xmalloc (PBUFSIZ);
  remote_address_size = TARGET_ADDR_BIT;
}

/* Saved pointer to previous owner of the new_objfile event. */
static void (*remote_new_objfile_chain) (struct objfile *);

/* Function to be called whenever a new objfile (shlib) is detected. */
static void
remote_new_objfile (struct objfile *objfile)
{
  if (remote_desc != 0)		/* Have a remote connection */
    {
      remote_check_symbols (objfile);
    }
  /* Call predecessor on chain, if any. */
  if (remote_new_objfile_chain != 0 &&
      remote_desc == 0)
    remote_new_objfile_chain (objfile);
}

void
_initialize_remote (void)
{
  static struct cmd_list_element *remote_set_cmdlist;
  static struct cmd_list_element *remote_show_cmdlist;
  struct cmd_list_element *tmpcmd;

  /* architecture specific data */
  build_remote_gdbarch_data ();
  register_gdbarch_swap (&tty_input, sizeof (&tty_input), NULL);
  register_remote_packet_sizes ();
  register_gdbarch_swap (&remote_address_size, 
                         sizeof (&remote_address_size), NULL);
  register_gdbarch_swap (NULL, 0, build_remote_gdbarch_data);

  init_remote_ops ();
  add_target (&remote_ops);

  init_extended_remote_ops ();
  add_target (&extended_remote_ops);

  init_remote_async_ops ();
  add_target (&remote_async_ops);

  init_extended_async_remote_ops ();
  add_target (&extended_async_remote_ops);

  init_remote_cisco_ops ();
  add_target (&remote_cisco_ops);

  /* Hook into new objfile notification.  */
  remote_new_objfile_chain = target_new_objfile_hook;
  target_new_objfile_hook  = remote_new_objfile;

#if 0
  init_remote_threadtests ();
#endif

  /* set/show remote ... */

  add_prefix_cmd ("remote", class_maintenance, set_remote_cmd, "\
Remote protocol specific variables\n\
Configure various remote-protocol specific variables such as\n\
the packets being used",
		  &remote_set_cmdlist, "set remote ",
		  0/*allow-unknown*/, &setlist);
  add_prefix_cmd ("remote", class_maintenance, show_remote_cmd, "\
Remote protocol specific variables\n\
Configure various remote-protocol specific variables such as\n\
the packets being used",
		  &remote_show_cmdlist, "show remote ",
		  0/*allow-unknown*/, &showlist);

  add_cmd ("compare-sections", class_obscure, compare_sections_command,
	   "Compare section data on target to the exec file.\n\
Argument is a single section name (default: all loaded sections).",
	   &cmdlist);

  add_cmd ("packet", class_maintenance, packet_command,
	   "Send an arbitrary packet to a remote target.\n\
   maintenance packet TEXT\n\
If GDB is talking to an inferior via the GDB serial protocol, then\n\
this command sends the string TEXT to the inferior, and displays the\n\
response packet.  GDB supplies the initial `$' character, and the\n\
terminating `#' character and checksum.",
	   &maintenancelist);

  add_show_from_set
    (add_set_cmd ("remotebreak", no_class,
		  var_boolean, (char *) &remote_break,
		  "Set whether to send break if interrupted.\n",
		  &setlist),
     &showlist);

  /* Install commands for configuring memory read/write packets. */

  add_cmd ("remotewritesize", no_class, set_memory_write_packet_size,
	   "Set the maximum number of bytes per memory write packet (deprecated).\n",
	   &setlist);
  add_cmd ("remotewritesize", no_class, show_memory_write_packet_size,
	   "Show the maximum number of bytes per memory write packet (deprecated).\n",
	   &showlist);
  add_cmd ("memory-write-packet-size", no_class,
	   set_memory_write_packet_size,
	   "Set the maximum number of bytes per memory-write packet.\n"
	   "Specify the number of bytes in a packet or 0 (zero) for the\n"
	   "default packet size.  The actual limit is further reduced\n"
	   "dependent on the target.  Specify ``fixed'' to disable the\n"
	   "further restriction and ``limit'' to enable that restriction\n",
	   &remote_set_cmdlist);
  add_cmd ("memory-read-packet-size", no_class,
	   set_memory_read_packet_size,
	   "Set the maximum number of bytes per memory-read packet.\n"
	   "Specify the number of bytes in a packet or 0 (zero) for the\n"
	   "default packet size.  The actual limit is further reduced\n"
	   "dependent on the target.  Specify ``fixed'' to disable the\n"
	   "further restriction and ``limit'' to enable that restriction\n",
	   &remote_set_cmdlist);
  add_cmd ("memory-write-packet-size", no_class,
	   show_memory_write_packet_size,
	   "Show the maximum number of bytes per memory-write packet.\n",
	   &remote_show_cmdlist);
  add_cmd ("memory-read-packet-size", no_class,
	   show_memory_read_packet_size,
	   "Show the maximum number of bytes per memory-read packet.\n",
	   &remote_show_cmdlist);

  add_show_from_set
    (add_set_cmd ("remoteaddresssize", class_obscure,
		  var_integer, (char *) &remote_address_size,
		  "Set the maximum size of the address (in bits) \
in a memory packet.\n",
		  &setlist),
     &showlist);

  add_packet_config_cmd (&remote_protocol_binary_download,
			 "X", "binary-download",
			 set_remote_protocol_binary_download_cmd,
			 show_remote_protocol_binary_download_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 1);
#if 0
  /* XXXX - should ``set remotebinarydownload'' be retained for
     compatibility. */
  add_show_from_set
    (add_set_cmd ("remotebinarydownload", no_class,
		  var_boolean, (char *) &remote_binary_download,
		  "Set binary downloads.\n", &setlist),
     &showlist);
#endif

  add_info ("remote-process", remote_info_process,
	    "Query the remote system for process info.");

  add_packet_config_cmd (&remote_protocol_qSymbol,
			 "qSymbol", "symbol-lookup",
			 set_remote_protocol_qSymbol_packet_cmd,
			 show_remote_protocol_qSymbol_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_e,
			 "e", "step-over-range",
			 set_remote_protocol_e_packet_cmd,
			 show_remote_protocol_e_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_E,
			 "E", "step-over-range-w-signal",
			 set_remote_protocol_E_packet_cmd,
			 show_remote_protocol_E_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_P,
			 "P", "set-register",
			 set_remote_protocol_P_packet_cmd,
			 show_remote_protocol_P_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 1);

  add_packet_config_cmd (&remote_protocol_Z[Z_PACKET_SOFTWARE_BP],
			 "Z0", "software-breakpoint",
			 set_remote_protocol_Z_software_bp_packet_cmd,
			 show_remote_protocol_Z_software_bp_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_Z[Z_PACKET_HARDWARE_BP],
			 "Z1", "hardware-breakpoint",
			 set_remote_protocol_Z_hardware_bp_packet_cmd,
			 show_remote_protocol_Z_hardware_bp_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_Z[Z_PACKET_WRITE_WP],
			 "Z2", "write-watchpoint",
			 set_remote_protocol_Z_write_wp_packet_cmd,
			 show_remote_protocol_Z_write_wp_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_Z[Z_PACKET_READ_WP],
			 "Z3", "read-watchpoint",
			 set_remote_protocol_Z_read_wp_packet_cmd,
			 show_remote_protocol_Z_read_wp_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  add_packet_config_cmd (&remote_protocol_Z[Z_PACKET_ACCESS_WP],
			 "Z4", "access-watchpoint",
			 set_remote_protocol_Z_access_wp_packet_cmd,
			 show_remote_protocol_Z_access_wp_packet_cmd,
			 &remote_set_cmdlist, &remote_show_cmdlist,
			 0);

  /* Keep the old ``set remote Z-packet ...'' working. */
  tmpcmd = add_set_auto_boolean_cmd ("Z-packet", class_obscure,
				     &remote_Z_packet_detect,
				     "\
Set use of remote protocol `Z' packets", &remote_set_cmdlist);
  tmpcmd->function.sfunc = set_remote_protocol_Z_packet_cmd;
  add_cmd ("Z-packet", class_obscure, show_remote_protocol_Z_packet_cmd,
	   "Show use of remote protocol `Z' packets ",
	   &remote_show_cmdlist);
}
