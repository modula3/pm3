/* Everything about breakpoints, for GDB.
   Copyright 1986, 1987, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996
             Free Software Foundation, Inc.

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include <ctype.h>
#include "symtab.h"
#include "frame.h"
#include "breakpoint.h"
#include "gdbtypes.h"
#include "expression.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "value.h"
#include "command.h"
#include "inferior.h"
#include "gdbthread.h"
#include "target.h"
#include "language.h"
#include "gdb_string.h"
#include "demangle.h"
#include "annotate.h"

/* local function prototypes */

static void
catch_command_1 PARAMS ((char *, int, int));

static void
enable_delete_command PARAMS ((char *, int));

static void
enable_delete_breakpoint PARAMS ((struct breakpoint *));

static void
enable_once_command PARAMS ((char *, int));

static void
enable_once_breakpoint PARAMS ((struct breakpoint *));

static void
disable_command PARAMS ((char *, int));

static void
enable_command PARAMS ((char *, int));

static void
map_breakpoint_numbers PARAMS ((char *,	void (*)(struct breakpoint *)));

static void
ignore_command PARAMS ((char *, int));

static long
breakpoint_re_set_one PARAMS ((char *));

static void
delete_command PARAMS ((char *, int));

static void
clear_command PARAMS ((char *, int));

static void
catch_command PARAMS ((char *, int));

static struct symtabs_and_lines
get_catch_sals PARAMS ((int));

static void
watch_command PARAMS ((char *, int));

static int
can_use_hardware_watchpoint PARAMS ((struct value *));

static void
tbreak_command PARAMS ((char *, int));

static void
break_command_1 PARAMS ((char *, int, int));

static void
mention PARAMS ((struct breakpoint *));

static struct breakpoint *
set_raw_breakpoint PARAMS ((struct symtab_and_line));

static void
check_duplicates PARAMS ((CORE_ADDR));

static void
describe_other_breakpoints PARAMS ((CORE_ADDR));

static void
breakpoints_info PARAMS ((char *, int));

static void
breakpoint_1 PARAMS ((int, int));

static bpstat
bpstat_alloc PARAMS ((struct breakpoint *, bpstat));

static long
breakpoint_cond_eval PARAMS ((char *));

static void
cleanup_executing_breakpoints PARAMS ((PTR));

static void
commands_command PARAMS ((char *, int));

static void
condition_command PARAMS ((char *, int));

static int
get_number PARAMS ((char **));

static void
set_breakpoint_count PARAMS ((int));

static int
remove_breakpoint PARAMS ((struct breakpoint *));

static int
print_it_normal PARAMS ((bpstat));

static long
watchpoint_check PARAMS ((char *));

static int
print_it_done PARAMS ((bpstat));

static int
print_it_noop PARAMS ((bpstat));

static void
maintenance_info_breakpoints PARAMS ((char *, int));

#ifdef GET_LONGJMP_TARGET
static void
create_longjmp_breakpoint PARAMS ((char *));
#endif

static int
hw_breakpoint_used_count PARAMS ((void));

static int
hw_watchpoint_used_count PARAMS ((enum bptype, int *));

static void
hbreak_command PARAMS ((char *, int));

static void
thbreak_command PARAMS ((char *, int));

static void
watch_command_1 PARAMS ((char *, int, int));

static void
rwatch_command PARAMS ((char *, int));

static void
awatch_command PARAMS ((char *, int));

static void
do_enable_breakpoint PARAMS ((struct breakpoint *, enum bpdisp));

extern int addressprint;		/* Print machine addresses? */

#if defined (GET_LONGJMP_TARGET) || defined (SOLIB_ADD)
static int internal_breakpoint_number = -1;
#endif

/* Are we executing breakpoint commands?  */
static int executing_breakpoint_commands;

/* Walk the following statement or block through all breakpoints.
   ALL_BREAKPOINTS_SAFE does so even if the statment deletes the current
   breakpoint.  */

#define ALL_BREAKPOINTS(b)  for (b = breakpoint_chain; b; b = b->next)

#define ALL_BREAKPOINTS_SAFE(b,tmp)	\
	for (b = breakpoint_chain;	\
	     b? (tmp=b->next, 1): 0;	\
	     b = tmp)

/* True if breakpoint hit counts should be displayed in breakpoint info.  */

int show_breakpoint_hit_counts = 1;

/* Chain of all breakpoints defined.  */

struct breakpoint *breakpoint_chain;

/* Number of last breakpoint made.  */

static int breakpoint_count;

/* Set breakpoint count to NUM.  */

static void
set_breakpoint_count (num)
     int num;
{
  breakpoint_count = num;
  set_internalvar (lookup_internalvar ("bpnum"),
		   value_from_longest (builtin_type_int, (LONGEST) num));
}

/* Used in run_command to zero the hit count when a new run starts. */

void
clear_breakpoint_hit_counts ()
{
  struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    b->hit_count = 0;
}

/* Default address, symtab and line to put a breakpoint at
   for "break" command with no arg.
   if default_breakpoint_valid is zero, the other three are
   not valid, and "break" with no arg is an error.

   This set by print_stack_frame, which calls set_default_breakpoint.  */

int default_breakpoint_valid;
CORE_ADDR default_breakpoint_address;
struct symtab *default_breakpoint_symtab;
int default_breakpoint_line;

/* *PP is a string denoting a breakpoint.  Get the number of the breakpoint.
   Advance *PP after the string and any trailing whitespace.

   Currently the string can either be a number or "$" followed by the name
   of a convenience variable.  Making it an expression wouldn't work well
   for map_breakpoint_numbers (e.g. "4 + 5 + 6").  */
static int
get_number (pp)
     char **pp;
{
  int retval;
  char *p = *pp;

  if (p == NULL)
    /* Empty line means refer to the last breakpoint.  */
    return breakpoint_count;
  else if (*p == '$')
    {
      /* Make a copy of the name, so we can null-terminate it
	 to pass to lookup_internalvar().  */
      char *varname;
      char *start = ++p;
      value_ptr val;

      while (isalnum (*p) || *p == '_')
	p++;
      varname = (char *) alloca (p - start + 1);
      strncpy (varname, start, p - start);
      varname[p - start] = '\0';
      val = value_of_internalvar (lookup_internalvar (varname));
      if (TYPE_CODE (VALUE_TYPE (val)) != TYPE_CODE_INT)
	error (
"Convenience variables used to specify breakpoints must have integer values."
	       );
      retval = (int) value_as_long (val);
    }
  else
    {
      if (*p == '-')
	++p;
      while (*p >= '0' && *p <= '9')
	++p;
      if (p == *pp)
	/* There is no number here.  (e.g. "cond a == b").  */
	error_no_arg ("breakpoint number");
      retval = atoi (*pp);
    }
  if (!(isspace (*p) || *p == '\0'))
    error ("breakpoint number expected");
  while (isspace (*p))
    p++;
  *pp = p;
  return retval;
}

/* condition N EXP -- set break condition of breakpoint N to EXP.  */

static void
condition_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  register struct breakpoint *b;
  char *p;
  register int bnum;

  if (arg == 0)
    error_no_arg ("breakpoint number");

  p = arg;
  bnum = get_number (&p);

  ALL_BREAKPOINTS (b)
    if (b->number == bnum)
      {
	if (b->cond)
	  {
	    free ((PTR)b->cond);
	    b->cond = 0;
	  }
	if (b->cond_string != NULL)
	  free ((PTR)b->cond_string);

	if (*p == 0)
	  {
	    b->cond = 0;
	    b->cond_string = NULL;
	    if (from_tty)
	      printf_filtered ("Breakpoint %d now unconditional.\n", bnum);
	  }
	else
	  {
	    arg = p;
	    /* I don't know if it matters whether this is the string the user
	       typed in or the decompiled expression.  */
	    b->cond_string = savestring (arg, strlen (arg));
	    b->cond = parse_exp_1 (&arg, block_for_pc (b->address), 0);
	    if (*arg)
	      error ("Junk at end of expression");
	  }
	breakpoints_changed ();
	return;
      }

  error ("No breakpoint number %d.", bnum);
}

/* ARGSUSED */
static void
commands_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  register struct breakpoint *b;
  char *p;
  register int bnum;
  struct command_line *l;

  /* If we allowed this, we would have problems with when to
     free the storage, if we change the commands currently
     being read from.  */

  if (executing_breakpoint_commands)
    error ("Can't use the \"commands\" command among a breakpoint's commands.");

  p = arg;
  bnum = get_number (&p);
  if (p && *p)
    error ("Unexpected extra arguments following breakpoint number.");
      
  ALL_BREAKPOINTS (b)
    if (b->number == bnum)
      {
	char tmpbuf[128];
	sprintf (tmpbuf, "Type commands for when breakpoint %d is hit, one per line.", bnum);
	l = read_command_lines (tmpbuf, from_tty);
	free_command_lines (&b->commands);
	b->commands = l;
	breakpoints_changed ();
	return;
      }
  error ("No breakpoint number %d.", bnum);
}

extern int memory_breakpoint_size; /* from mem-break.c */

/* Like target_read_memory() but if breakpoints are inserted, return
   the shadow contents instead of the breakpoints themselves.

   Read "memory data" from whatever target or inferior we have. 
   Returns zero if successful, errno value if not.  EIO is used
   for address out of bounds.  If breakpoints are inserted, returns
   shadow contents, not the breakpoints themselves.  From breakpoint.c.  */

int
read_memory_nobpt (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     unsigned len;
{
  int status;
  struct breakpoint *b;

  if (memory_breakpoint_size < 0)
    /* No breakpoints on this machine.  FIXME: This should be
       dependent on the debugging target.  Probably want
       target_insert_breakpoint to return a size, saying how many
       bytes of the shadow contents are used, or perhaps have
       something like target_xfer_shadow.  */
    return target_read_memory (memaddr, myaddr, len);
  
  ALL_BREAKPOINTS (b)
    {
      if (b->type == bp_watchpoint
	  || b->type == bp_hardware_watchpoint
	  || b->type == bp_read_watchpoint
	  || b->type == bp_access_watchpoint
	  || !b->inserted)
	continue;
      else if (b->address + memory_breakpoint_size <= memaddr)
	/* The breakpoint is entirely before the chunk of memory
	   we are reading.  */
	continue;
      else if (b->address >= memaddr + len)
	/* The breakpoint is entirely after the chunk of memory we
	   are reading.  */
	continue;
      else
	{
	  /* Copy the breakpoint from the shadow contents, and recurse
	     for the things before and after.  */
	  
	  /* Addresses and length of the part of the breakpoint that
	     we need to copy.  */
	  CORE_ADDR membpt = b->address;
	  unsigned int bptlen = memory_breakpoint_size;
	  /* Offset within shadow_contents.  */
	  int bptoffset = 0;
	  
	  if (membpt < memaddr)
	    {
	      /* Only copy the second part of the breakpoint.  */
	      bptlen -= memaddr - membpt;
	      bptoffset = memaddr - membpt;
	      membpt = memaddr;
	    }

	  if (membpt + bptlen > memaddr + len)
	    {
	      /* Only copy the first part of the breakpoint.  */
	      bptlen -= (membpt + bptlen) - (memaddr + len);
	    }

	  memcpy (myaddr + membpt - memaddr, 
		  b->shadow_contents + bptoffset, bptlen);

	  if (membpt > memaddr)
	    {
	      /* Copy the section of memory before the breakpoint.  */
	      status = read_memory_nobpt (memaddr, myaddr, membpt - memaddr);
	      if (status != 0)
		return status;
	    }

	  if (membpt + bptlen < memaddr + len)
	    {
	      /* Copy the section of memory after the breakpoint.  */
	      status = read_memory_nobpt
		(membpt + bptlen,
		 myaddr + membpt + bptlen - memaddr,
		 memaddr + len - (membpt + bptlen));
	      if (status != 0)
		return status;
	    }
	  return 0;
	}
    }
  /* Nothing overlaps.  Just call read_memory_noerr.  */
  return target_read_memory (memaddr, myaddr, len);
}

/* insert_breakpoints is used when starting or continuing the program.
   remove_breakpoints is used when the program stops.
   Both return zero if successful,
   or an `errno' value if could not write the inferior.  */

int
insert_breakpoints ()
{
  register struct breakpoint *b, *temp;
  int val = 0;
  int disabled_breaks = 0;

  ALL_BREAKPOINTS_SAFE (b, temp)
    if (b->type != bp_watchpoint
	&& b->type != bp_hardware_watchpoint
	&& b->type != bp_read_watchpoint
	&& b->type != bp_access_watchpoint
	&& b->enable != disabled
	&& b->enable != shlib_disabled
	&& ! b->inserted
	&& ! b->duplicate)
      {
	if (b->type == bp_hardware_breakpoint)
	  val = target_insert_hw_breakpoint(b->address, b->shadow_contents);
	else
	  val = target_insert_breakpoint(b->address, b->shadow_contents);
	if (val)
	  {
	    /* Can't set the breakpoint.  */
#if defined (DISABLE_UNSETTABLE_BREAK)
	    if (DISABLE_UNSETTABLE_BREAK (b->address))
	      {
		val = 0;
		b->enable = shlib_disabled;
		if (!disabled_breaks)
		  {
		    target_terminal_ours_for_output ();
		    fprintf_unfiltered (gdb_stderr,
			 "Cannot insert breakpoint %d:\n", b->number);
		    printf_filtered ("Temporarily disabling shared library breakpoints:\n");
		  }
		disabled_breaks = 1;
		printf_filtered ("%d ", b->number);
	      }
	    else
#endif
	      {
		target_terminal_ours_for_output ();
		fprintf_unfiltered (gdb_stderr, "Cannot insert breakpoint %d:\n", b->number);
#ifdef ONE_PROCESS_WRITETEXT
		fprintf_unfiltered (gdb_stderr,
		  "The same program may be running in another process.\n");
#endif
		memory_error (val, b->address);	/* which bombs us out */
	      }
	  }
	else
	  b->inserted = 1;
      }
    else if ((b->type == bp_hardware_watchpoint ||
	      b->type == bp_read_watchpoint ||
	      b->type == bp_access_watchpoint)
	     && b->enable == enabled
	     && ! b->inserted
	     && ! b->duplicate)
      {
	struct frame_info *saved_frame;
	int saved_level, within_current_scope;
	value_ptr mark = value_mark ();
	value_ptr v;

	/* Save the current frame and level so we can restore it after
	   evaluating the watchpoint expression on its own frame.  */
	saved_frame = selected_frame;
	saved_level = selected_frame_level;

	/* Determine if the watchpoint is within scope.  */
	if (b->exp_valid_block == NULL)
	  within_current_scope = 1;
	else
	  {
	    struct frame_info *fi =
	      find_frame_addr_in_frame_chain (b->watchpoint_frame);
	    within_current_scope = (fi != NULL);
	    if (within_current_scope)
	      select_frame (fi, -1);
	  }
	
	if (within_current_scope)
	  {
	    /* Evaluate the expression and cut the chain of values
	       produced off from the value chain.  */
	    v = evaluate_expression (b->exp);
	    value_release_to_mark (mark);
	    
	    b->val_chain = v;
	    b->inserted = 1;

	    /* Look at each value on the value chain.  */
	    for ( ; v; v=v->next)
	      {
		/* If it's a memory location, then we must watch it.  */
		if (v->lval == lval_memory)
		  {
		    int addr, len, type;
		    
		    addr = VALUE_ADDRESS (v) + VALUE_OFFSET (v);
		    len = TYPE_LENGTH (VALUE_TYPE (v));
		    type = 0;
                    if (b->type == bp_read_watchpoint)
		      type = 1;
		    else if (b->type == bp_access_watchpoint)
		      type = 2;

		    val = target_insert_watchpoint (addr, len, type);
		    if (val == -1)
		      {
			b->inserted = 0;
			break;
		      }
		    val = 0;
		  }
	      }
	    /* Failure to insert a watchpoint on any memory value in the
	       value chain brings us here.  */
	    if (!b->inserted)
	      warning ("Hardware watchpoint %d: Could not insert watchpoint\n",
		       b->number);
	  }
	else
	  {
	    printf_filtered ("\
Hardware watchpoint %d deleted because the program has left the block in\n\
which its expression is valid.\n", b->number);
	    if (b->related_breakpoint)
	      {
		b->related_breakpoint->enable = disable;
		b->related_breakpoint->disposition = del_at_next_stop;
	      }
	    b->enable = disable;
	    b->disposition = del_at_next_stop;
	  }

	/* Restore the frame and level.  */
        select_frame (saved_frame, saved_level);
      }
  if (disabled_breaks)
    printf_filtered ("\n");
  return val;
}


int
remove_breakpoints ()
{
  register struct breakpoint *b;
  int val;

  ALL_BREAKPOINTS (b)
    {
      if (b->inserted)
	{
	  val = remove_breakpoint (b);
	  if (val != 0)
	    return val;
	}
    }
  return 0;
}


static int
remove_breakpoint (b)
     struct breakpoint *b;
{
  int val;
  
  if (b->type != bp_watchpoint
      && b->type != bp_hardware_watchpoint
      && b->type != bp_read_watchpoint
      && b->type != bp_access_watchpoint)
    {
      if (b->type == bp_hardware_breakpoint)
        val = target_remove_hw_breakpoint(b->address, b->shadow_contents);
      else
        val = target_remove_breakpoint(b->address, b->shadow_contents);
      if (val)
	return val;
      b->inserted = 0;
    }
  else if ((b->type == bp_hardware_watchpoint ||
            b->type == bp_read_watchpoint ||
  	    b->type == bp_access_watchpoint)
	   && b->enable == enabled
	   && ! b->duplicate)
    {
      value_ptr v, n;
      
      b->inserted = 0;
      /* Walk down the saved value chain.  */
      for (v = b->val_chain; v; v = v->next)
	{
	  /* For each memory reference remove the watchpoint
	     at that address.  */
	  if (v->lval == lval_memory)
	    {
	      int addr, len;
	      
	      addr = VALUE_ADDRESS (v) + VALUE_OFFSET (v);
	      len = TYPE_LENGTH (VALUE_TYPE (v));
	      val = target_remove_watchpoint (addr, len, b->type);
	      if (val == -1)
		b->inserted = 1;
	      val = 0;
	    }
	}
      /* Failure to remove any of the hardware watchpoints comes here.  */
      if (b->inserted)
	warning ("Hardware watchpoint %d: Could not remove watchpoint\n",
		 b->number);
      
      /* Free the saved value chain.  We will construct a new one
	 the next time the watchpoint is inserted.  */
      for (v = b->val_chain; v; v = n)
	{
	  n = v->next;
	  value_free (v);
	}
      b->val_chain = NULL;
    }
  return 0;
}

/* Clear the "inserted" flag in all breakpoints.  */

void
mark_breakpoints_out ()
{
  register struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    b->inserted = 0;
}

/* Clear the "inserted" flag in all breakpoints and delete any breakpoints
   which should go away between runs of the program.  */

void
breakpoint_init_inferior ()
{
  register struct breakpoint *b, *temp;

  ALL_BREAKPOINTS_SAFE (b, temp)
    {
      b->inserted = 0;

      switch (b->type)
	{
	case bp_call_dummy:
	case bp_watchpoint_scope:

	  /* If the call dummy breakpoint is at the entry point it will
	     cause problems when the inferior is rerun, so we better
	     get rid of it. 

	     Also get rid of scope breakpoints.  */
	  delete_breakpoint (b);
	  break;

	case bp_watchpoint:
	case bp_hardware_watchpoint:
	case bp_read_watchpoint:
	case bp_access_watchpoint:

	  /* Likewise for watchpoints on local expressions.  */
	  if (b->exp_valid_block != NULL)
	    delete_breakpoint (b);
	  break;

	default:
	  break;
	}
    }
}

/* breakpoint_here_p (PC) returns 1 if an enabled breakpoint exists at PC.
   When continuing from a location with a breakpoint,
   we actually single step once before calling insert_breakpoints.  */

int
breakpoint_here_p (pc)
     CORE_ADDR pc;
{
  register struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    if (b->enable != disabled
	&& b->enable != shlib_disabled
	&& b->address == pc)
      return 1;

  return 0;
}

/* Return nonzero if FRAME is a dummy frame.  We can't use PC_IN_CALL_DUMMY
   because figuring out the saved SP would take too much time, at least using
   get_saved_register on the 68k.  This means that for this function to
   work right a port must use the bp_call_dummy breakpoint.  */

int
frame_in_dummy (frame)
     struct frame_info *frame;
{
  struct breakpoint *b;

#ifdef CALL_DUMMY
  ALL_BREAKPOINTS (b)
    {
      static unsigned LONGEST dummy[] = CALL_DUMMY;

      if (b->type == bp_call_dummy
	  && b->frame == frame->frame

	  /* We need to check the PC as well as the frame on the sparc,
	     for signals.exp in the testsuite.  */
	  && (frame->pc
	      >= (b->address
		  - sizeof (dummy) / sizeof (LONGEST) * REGISTER_SIZE))
	  && frame->pc <= b->address)
	return 1;
    }
#endif /* CALL_DUMMY */
  return 0;
}

/* breakpoint_match_thread (PC, PID) returns true if the breakpoint at PC
   is valid for process/thread PID.  */

int
breakpoint_thread_match (pc, pid)
     CORE_ADDR pc;
     int pid;
{
  struct breakpoint *b;
  int thread;

  thread = pid_to_thread_id (pid);

  ALL_BREAKPOINTS (b)
    if (b->enable != disabled
	&& b->enable != shlib_disabled
	&& b->address == pc
	&& (b->thread == -1 || b->thread == thread))
      return 1;

  return 0;
}


/* bpstat stuff.  External routines' interfaces are documented
   in breakpoint.h.  */

/* Clear a bpstat so that it says we are not at any breakpoint.
   Also free any storage that is part of a bpstat.  */

void
bpstat_clear (bsp)
     bpstat *bsp;
{
  bpstat p;
  bpstat q;

  if (bsp == 0)
    return;
  p = *bsp;
  while (p != NULL)
    {
      q = p->next;
      if (p->old_val != NULL)
	value_free (p->old_val);
      free ((PTR)p);
      p = q;
    }
  *bsp = NULL;
}

/* Return a copy of a bpstat.  Like "bs1 = bs2" but all storage that
   is part of the bpstat is copied as well.  */

bpstat
bpstat_copy (bs)
     bpstat bs;
{
  bpstat p = NULL;
  bpstat tmp;
  bpstat retval = NULL;

  if (bs == NULL)
    return bs;

  for (; bs != NULL; bs = bs->next)
    {
      tmp = (bpstat) xmalloc (sizeof (*tmp));
      memcpy (tmp, bs, sizeof (*tmp));
      if (p == NULL)
	/* This is the first thing in the chain.  */
	retval = tmp;
      else
	p->next = tmp;
      p = tmp;
    }
  p->next = NULL;
  return retval;
}

/* Find the bpstat associated with this breakpoint */

bpstat
bpstat_find_breakpoint(bsp, breakpoint)
     bpstat bsp;
     struct breakpoint *breakpoint;
{
  if (bsp == NULL) return NULL;

  for (;bsp != NULL; bsp = bsp->next) {
    if (bsp->breakpoint_at == breakpoint) return bsp;
  }
  return NULL;
}

/* Return the breakpoint number of the first breakpoint we are stopped
   at.  *BSP upon return is a bpstat which points to the remaining
   breakpoints stopped at (but which is not guaranteed to be good for
   anything but further calls to bpstat_num).
   Return 0 if passed a bpstat which does not indicate any breakpoints.  */

int
bpstat_num (bsp)
     bpstat *bsp;
{
  struct breakpoint *b;

  if ((*bsp) == NULL)
    return 0;			/* No more breakpoint values */
  else
    {
      b = (*bsp)->breakpoint_at;
      *bsp = (*bsp)->next;
      if (b == NULL)
	return -1;		/* breakpoint that's been deleted since */
      else
        return b->number;	/* We have its number */
    }
}

/* Modify BS so that the actions will not be performed.  */

void
bpstat_clear_actions (bs)
     bpstat bs;
{
  for (; bs != NULL; bs = bs->next)
    {
      bs->commands = NULL;
      if (bs->old_val != NULL)
	{
	  value_free (bs->old_val);
	  bs->old_val = NULL;
	}
    }
}

/* Stub for cleaning up our state if we error-out of a breakpoint command */
/* ARGSUSED */
static void
cleanup_executing_breakpoints (ignore)
     PTR ignore;
{
  executing_breakpoint_commands = 0;
}

/* Execute all the commands associated with all the breakpoints at this
   location.  Any of these commands could cause the process to proceed
   beyond this point, etc.  We look out for such changes by checking
   the global "breakpoint_proceeded" after each command.  */

void
bpstat_do_actions (bsp)
     bpstat *bsp;
{
  bpstat bs;
  struct cleanup *old_chain;
  struct command_line *cmd;

  /* Avoid endless recursion if a `source' command is contained
     in bs->commands.  */
  if (executing_breakpoint_commands)
    return;

  executing_breakpoint_commands = 1;
  old_chain = make_cleanup (cleanup_executing_breakpoints, 0);

top:
  bs = *bsp;

  breakpoint_proceeded = 0;
  for (; bs != NULL; bs = bs->next)
    {
      cmd = bs->commands;
      while (cmd != NULL)
	{
	  execute_control_command (cmd);
	  cmd = cmd->next;
	}
      if (breakpoint_proceeded)
	/* The inferior is proceeded by the command; bomb out now.
	   The bpstat chain has been blown away by wait_for_inferior.
	   But since execution has stopped again, there is a new bpstat
	   to look at, so start over.  */
	goto top;
      else
	bs->commands = NULL;
    }

  executing_breakpoint_commands = 0;
  discard_cleanups (old_chain);
}

/* This is the normal print_it function for a bpstat.  In the future,
   much of this logic could (should?) be moved to bpstat_stop_status,
   by having it set different print_it functions.  */

static int
print_it_normal (bs)
     bpstat bs;
{
  /* bs->breakpoint_at can be NULL if it was a momentary breakpoint
     which has since been deleted.  */
  if (bs->breakpoint_at == NULL
      || (bs->breakpoint_at->type != bp_breakpoint
	  && bs->breakpoint_at->type != bp_hardware_breakpoint
	  && bs->breakpoint_at->type != bp_watchpoint
	  && bs->breakpoint_at->type != bp_read_watchpoint
	  && bs->breakpoint_at->type != bp_access_watchpoint
	  && bs->breakpoint_at->type != bp_hardware_watchpoint))
    return 0;

  if (bs->breakpoint_at->type == bp_breakpoint ||
      bs->breakpoint_at->type == bp_hardware_breakpoint)
    {
      /* I think the user probably only wants to see one breakpoint
	 number, not all of them.  */
      annotate_breakpoint (bs->breakpoint_at->number);
      printf_filtered ("\nBreakpoint %d, ", bs->breakpoint_at->number);
      return 0;
    }
  else if ((bs->old_val != NULL) &&
	(bs->breakpoint_at->type == bp_watchpoint ||
	 bs->breakpoint_at->type == bp_access_watchpoint ||
	 bs->breakpoint_at->type == bp_hardware_watchpoint))
    {
      annotate_watchpoint (bs->breakpoint_at->number);
      mention (bs->breakpoint_at);
      printf_filtered ("\nOld value = ");
      value_print (bs->old_val, gdb_stdout, 0, Val_pretty_default);
      printf_filtered ("\nNew value = ");
      value_print (bs->breakpoint_at->val, gdb_stdout, 0,
		   Val_pretty_default);
      printf_filtered ("\n");
      value_free (bs->old_val);
      bs->old_val = NULL;
      /* More than one watchpoint may have been triggered.  */
      return -1;
    }
  else if (bs->breakpoint_at->type == bp_access_watchpoint ||
	   bs->breakpoint_at->type == bp_read_watchpoint)
    {
      mention (bs->breakpoint_at);
      printf_filtered ("\nValue = ");
      value_print (bs->breakpoint_at->val, gdb_stdout, 0,
                   Val_pretty_default);
      printf_filtered ("\n");
      return -1;
    }
  /* We can't deal with it.  Maybe another member of the bpstat chain can.  */
  return -1;
}

/* Print a message indicating what happened.  Returns nonzero to
   say that only the source line should be printed after this (zero
   return means print the frame as well as the source line).  */
/* Currently we always return zero.  */
int
bpstat_print (bs)
     bpstat bs;
{
  int val;
  
  if (bs == NULL)
    return 0;

  val = (*bs->print_it) (bs);
  if (val >= 0)
    return val;
  
  /* Maybe another breakpoint in the chain caused us to stop.
     (Currently all watchpoints go on the bpstat whether hit or
     not.  That probably could (should) be changed, provided care is taken
     with respect to bpstat_explains_signal).  */
  if (bs->next)
    return bpstat_print (bs->next);

  /* We reached the end of the chain without printing anything.  */
  return 0;
}

/* Evaluate the expression EXP and return 1 if value is zero.
   This is used inside a catch_errors to evaluate the breakpoint condition. 
   The argument is a "struct expression *" that has been cast to char * to 
   make it pass through catch_errors.  */

static long
breakpoint_cond_eval (exp)
     char *exp;
{
  value_ptr mark = value_mark ();
  int i = !value_true (evaluate_expression ((struct expression *)exp));
  value_free_to_mark (mark);
  return i;
}

/* Allocate a new bpstat and chain it to the current one.  */

static bpstat
bpstat_alloc (b, cbs)
     register struct breakpoint *b;
     bpstat cbs;			/* Current "bs" value */
{
  bpstat bs;

  bs = (bpstat) xmalloc (sizeof (*bs));
  cbs->next = bs;
  bs->breakpoint_at = b;
  /* If the condition is false, etc., don't do the commands.  */
  bs->commands = NULL;
  bs->old_val = NULL;
  bs->print_it = print_it_normal;
  return bs;
}

/* Possible return values for watchpoint_check (this can't be an enum
   because of check_errors).  */
/* The watchpoint has been deleted.  */
#define WP_DELETED 1
/* The value has changed.  */
#define WP_VALUE_CHANGED 2
/* The value has not changed.  */
#define WP_VALUE_NOT_CHANGED 3

#define BP_TEMPFLAG 1
#define BP_HARDWAREFLAG 2

/* Check watchpoint condition.  */

static long
watchpoint_check (p)
     char *p;
{
  bpstat bs = (bpstat) p;
  struct breakpoint *b;
  struct frame_info *fr;
  int within_current_scope;

  b = bs->breakpoint_at;

  if (b->exp_valid_block == NULL)
    within_current_scope = 1;
  else
    {
      /* There is no current frame at this moment.  If we're going to have
	 any chance of handling watchpoints on local variables, we'll need
	 the frame chain (so we can determine if we're in scope).  */
      reinit_frame_cache();
      fr = find_frame_addr_in_frame_chain (b->watchpoint_frame);
      within_current_scope = (fr != NULL);
      if (within_current_scope)
	/* If we end up stopping, the current frame will get selected
	   in normal_stop.  So this call to select_frame won't affect
	   the user.  */
	select_frame (fr, -1);
    }
      
  if (within_current_scope)
    {
      /* We use value_{,free_to_}mark because it could be a
         *long* time before we return to the command level and
	 call free_all_values.  We can't call free_all_values because
	 we might be in the middle of evaluating a function call.  */

      value_ptr mark = value_mark ();
      value_ptr new_val = evaluate_expression (bs->breakpoint_at->exp);
      if (!value_equal (b->val, new_val))
	{
	  release_value (new_val);
	  value_free_to_mark (mark);
	  bs->old_val = b->val;
	  b->val = new_val;
	  /* We will stop here */
	  return WP_VALUE_CHANGED;
	}
      else
	{
	  /* Nothing changed, don't do anything.  */
	  value_free_to_mark (mark);
	  /* We won't stop here */
	  return WP_VALUE_NOT_CHANGED;
	}
    }
  else
    {
      /* This seems like the only logical thing to do because
	 if we temporarily ignored the watchpoint, then when
	 we reenter the block in which it is valid it contains
	 garbage (in the case of a function, it may have two
	 garbage values, one before and one after the prologue).
	 So we can't even detect the first assignment to it and
	 watch after that (since the garbage may or may not equal
	 the first value assigned).  */
      printf_filtered ("\
Watchpoint %d deleted because the program has left the block in\n\
which its expression is valid.\n", bs->breakpoint_at->number);
      if (b->related_breakpoint)
	{
	  b->related_breakpoint->enable = disable;
	  b->related_breakpoint->disposition = del_at_next_stop;
	}
      b->enable = disable;
      b->disposition = del_at_next_stop;

      return WP_DELETED;
    }
}

/* This is used when everything which needs to be printed has
   already been printed.  But we still want to print the frame.  */
static int
print_it_done (bs)
     bpstat bs;
{
  return 0;
}

/* This is used when nothing should be printed for this bpstat entry.  */

static int
print_it_noop (bs)
     bpstat bs;
{
  return -1;
}

/* Get a bpstat associated with having just stopped at address *PC
   and frame address CORE_ADDRESS.  Update *PC to point at the
   breakpoint (if we hit a breakpoint).  NOT_A_BREAKPOINT is nonzero
   if this is known to not be a real breakpoint (it could still be a
   watchpoint, though).  */

/* Determine whether we stopped at a breakpoint, etc, or whether we
   don't understand this stop.  Result is a chain of bpstat's such that:

	if we don't understand the stop, the result is a null pointer.

	if we understand why we stopped, the result is not null.

	Each element of the chain refers to a particular breakpoint or
	watchpoint at which we have stopped.  (We may have stopped for
	several reasons concurrently.)

	Each element of the chain has valid next, breakpoint_at,
	commands, FIXME??? fields.

 */

bpstat
bpstat_stop_status (pc, not_a_breakpoint)
     CORE_ADDR *pc;
     int not_a_breakpoint;
{
  register struct breakpoint *b, *temp;
  CORE_ADDR bp_addr;
#if DECR_PC_AFTER_BREAK != 0 || defined (SHIFT_INST_REGS)
  /* True if we've hit a breakpoint (as opposed to a watchpoint).  */
  int real_breakpoint = 0;
#endif
  /* Root of the chain of bpstat's */
  struct bpstats root_bs[1];
  /* Pointer to the last thing in the chain currently.  */
  bpstat bs = root_bs;
  static char message1[] =
            "Error evaluating expression for watchpoint %d\n";
  char message[sizeof (message1) + 30 /* slop */];

  /* Get the address where the breakpoint would have been.  */
  bp_addr = *pc - DECR_PC_AFTER_BREAK;

  ALL_BREAKPOINTS_SAFE (b, temp)
    {
      if (b->enable == disabled
	  || b->enable == shlib_disabled)
	continue;

      if (b->type != bp_watchpoint
	  && b->type != bp_hardware_watchpoint
          && b->type != bp_read_watchpoint
          && b->type != bp_access_watchpoint
	  && b->type != bp_hardware_breakpoint
	  && b->address != bp_addr)
	continue;

      if (b->type == bp_hardware_breakpoint
	  && b->address != (bp_addr - DECR_PC_AFTER_HW_BREAK))
	continue;

      if (b->type != bp_watchpoint
	  && b->type != bp_hardware_watchpoint
	  && b->type != bp_read_watchpoint
	  && b->type != bp_access_watchpoint
	  && not_a_breakpoint)
	continue;

      /* Come here if it's a watchpoint, or if the break address matches */

      ++(b->hit_count);

      bs = bpstat_alloc (b, bs);	/* Alloc a bpstat to explain stop */

      bs->stop = 1;
      bs->print = 1;

      sprintf (message, message1, b->number);
      if (b->type == bp_watchpoint || b->type == bp_hardware_watchpoint)
	{
	  switch (catch_errors (watchpoint_check, (char *) bs, message,
				RETURN_MASK_ALL))
	    {
	    case WP_DELETED:
	      /* We've already printed what needs to be printed.  */
	      bs->print_it = print_it_done;
	      /* Stop.  */
	      break;
	    case WP_VALUE_CHANGED:
	      /* Stop.  */
	      break;
	    case WP_VALUE_NOT_CHANGED:
	      /* Don't stop.  */
	      bs->print_it = print_it_noop;
	      bs->stop = 0;
	      continue;
	    default:
	      /* Can't happen.  */
	      /* FALLTHROUGH */
	    case 0:
	      /* Error from catch_errors.  */
	      printf_filtered ("Watchpoint %d deleted.\n", b->number);
	      if (b->related_breakpoint)
		{
		  b->related_breakpoint->enable = disable;
		  b->related_breakpoint->disposition = del_at_next_stop;
		}
	      b->enable = disable;
	      b->disposition = del_at_next_stop;
	      /* We've already printed what needs to be printed.  */
	      bs->print_it = print_it_done;

	      /* Stop.  */
	      break;
	    }
	}
      else if (b->type == bp_read_watchpoint || b->type == bp_access_watchpoint)
        {
	  CORE_ADDR addr;
	  value_ptr v;
          int found = 0;

	  addr = target_stopped_data_address();
	  if (addr == 0) continue;
          for (v = b->val_chain; v; v = v->next)
            {
              if (v->lval == lval_memory)
                {
                  CORE_ADDR vaddr;

                  vaddr = VALUE_ADDRESS (v) + VALUE_OFFSET (v);
	          if (addr == vaddr)
	            found = 1;
                }
            }
	  if (found) 
	    switch (catch_errors (watchpoint_check, (char *) bs, message,
			 RETURN_MASK_ALL))
   	      {
                case WP_DELETED:
                  /* We've already printed what needs to be printed.  */
                  bs->print_it = print_it_done;
                  /* Stop.  */
                  break;
                case WP_VALUE_CHANGED:
                case WP_VALUE_NOT_CHANGED:
                  /* Stop.  */
                  break;
                default:
                  /* Can't happen.  */
                case 0:
                  /* Error from catch_errors.  */
                  printf_filtered ("Watchpoint %d deleted.\n", b->number);
		  if (b->related_breakpoint)
		    {
		      b->related_breakpoint->enable = disable;
		      b->related_breakpoint->disposition = del_at_next_stop;
		    }
		  b->enable = disable;
		  b->disposition = del_at_next_stop;
                  /* We've already printed what needs to be printed.  */
                  bs->print_it = print_it_done;
                  break;
	      }
        }
#if DECR_PC_AFTER_BREAK != 0 || defined (SHIFT_INST_REGS)
      else
	real_breakpoint = 1;
#endif

      if (b->frame && b->frame != (get_current_frame ())->frame)
	bs->stop = 0;
      else
	{
	  int value_is_zero = 0;

	  if (b->cond)
	    {
	      /* Need to select the frame, with all that implies
		 so that the conditions will have the right context.  */
	      select_frame (get_current_frame (), 0);
	      value_is_zero
		= catch_errors (breakpoint_cond_eval, (char *)(b->cond),
				"Error in testing breakpoint condition:\n",
				RETURN_MASK_ALL);
				/* FIXME-someday, should give breakpoint # */
	      free_all_values ();
	    }
	  if (b->cond && value_is_zero)
	    {
	      bs->stop = 0;
	    }
	  else if (b->ignore_count > 0)
	    {
	      b->ignore_count--;
	      bs->stop = 0;
	    }
	  else
	    {
	      /* We will stop here */
	      if (b->disposition == disable)
		b->enable = disabled;
	      bs->commands = b->commands;
	      if (b->silent)
		bs->print = 0;
	      if (bs->commands && STREQ ("silent", bs->commands->line))
		{
		  bs->commands = bs->commands->next;
		  bs->print = 0;
		}
	    }
	}
      /* Print nothing for this entry if we dont stop or if we dont print.  */
      if (bs->stop == 0 || bs->print == 0)
	bs->print_it = print_it_noop;
    }

  bs->next = NULL;		/* Terminate the chain */
  bs = root_bs->next;		/* Re-grab the head of the chain */
#if DECR_PC_AFTER_BREAK != 0 || defined (SHIFT_INST_REGS)
  if (bs)
    {
      if (real_breakpoint)
	{
	  *pc = bp_addr;
#if defined (SHIFT_INST_REGS)
	  SHIFT_INST_REGS();
#else /* No SHIFT_INST_REGS.  */
	  write_pc (bp_addr);
#endif /* No SHIFT_INST_REGS.  */
	}
    }
#endif /* DECR_PC_AFTER_BREAK != 0.  */

  /* The value of a hardware watchpoint hasn't changed, but the
     intermediate memory locations we are watching may have.  */
  if (bs && ! bs->stop &&
      (bs->breakpoint_at->type == bp_hardware_watchpoint ||
       bs->breakpoint_at->type == bp_read_watchpoint ||
       bs->breakpoint_at->type == bp_access_watchpoint))
    {
      remove_breakpoints ();
      insert_breakpoints ();
    }
  return bs;
}

/* Tell what to do about this bpstat.  */
struct bpstat_what
bpstat_what (bs)
     bpstat bs;
{
  /* Classify each bpstat as one of the following.  */
  enum class {
    /* This bpstat element has no effect on the main_action.  */
    no_effect = 0,

    /* There was a watchpoint, stop but don't print.  */
    wp_silent,

    /* There was a watchpoint, stop and print.  */
    wp_noisy,

    /* There was a breakpoint but we're not stopping.  */
    bp_nostop,

    /* There was a breakpoint, stop but don't print.  */
    bp_silent,

    /* There was a breakpoint, stop and print.  */
    bp_noisy,

    /* We hit the longjmp breakpoint.  */
    long_jump,

    /* We hit the longjmp_resume breakpoint.  */
    long_resume,

    /* We hit the step_resume breakpoint.  */
    step_resume,

    /* We hit the through_sigtramp breakpoint.  */
    through_sig,

    /* We hit the shared library event breakpoint.  */
    shlib_event,

    /* This is just used to count how many enums there are.  */
    class_last
    };

  /* Here is the table which drives this routine.  So that we can
     format it pretty, we define some abbreviations for the
     enum bpstat_what codes.  */
#define kc BPSTAT_WHAT_KEEP_CHECKING
#define ss BPSTAT_WHAT_STOP_SILENT
#define sn BPSTAT_WHAT_STOP_NOISY
#define sgl BPSTAT_WHAT_SINGLE
#define slr BPSTAT_WHAT_SET_LONGJMP_RESUME
#define clr BPSTAT_WHAT_CLEAR_LONGJMP_RESUME
#define clrs BPSTAT_WHAT_CLEAR_LONGJMP_RESUME_SINGLE
#define sr BPSTAT_WHAT_STEP_RESUME
#define ts BPSTAT_WHAT_THROUGH_SIGTRAMP
#define shl BPSTAT_WHAT_CHECK_SHLIBS

/* "Can't happen."  Might want to print an error message.
   abort() is not out of the question, but chances are GDB is just
   a bit confused, not unusable.  */
#define err BPSTAT_WHAT_STOP_NOISY

  /* Given an old action and a class, come up with a new action.  */
  /* One interesting property of this table is that wp_silent is the same
     as bp_silent and wp_noisy is the same as bp_noisy.  That is because
     after stopping, the check for whether to step over a breakpoint
     (BPSTAT_WHAT_SINGLE type stuff) is handled in proceed() without
     reference to how we stopped.  We retain separate wp_silent and bp_silent
     codes in case we want to change that someday.  */

  /* step_resume entries: a step resume breakpoint overrides another
     breakpoint of signal handling (see comment in wait_for_inferior
     at first IN_SIGTRAMP where we set the step_resume breakpoint).  */
  /* We handle the through_sigtramp_breakpoint the same way; having both
     one of those and a step_resume_breakpoint is probably very rare (?).  */

  static const enum bpstat_what_main_action
    table[(int)class_last][(int)BPSTAT_WHAT_LAST] =
      {
	/*                              old action */
	/*       kc   ss   sn   sgl   slr  clr   clrs  sr   ts  shl
	 */
/*no_effect*/	{kc,  ss,  sn,  sgl,  slr, clr,  clrs, sr,  ts, shl},
/*wp_silent*/	{ss,  ss,  sn,  ss,   ss,  ss,   ss,   sr,  ts, shl},
/*wp_noisy*/    {sn,  sn,  sn,  sn,   sn,  sn,   sn,   sr,  ts, shl},
/*bp_nostop*/	{sgl, ss,  sn,  sgl,  slr, clrs, clrs, sr,  ts, shl},
/*bp_silent*/	{ss,  ss,  sn,  ss,   ss,  ss,   ss,   sr,  ts, shl},
/*bp_noisy*/    {sn,  sn,  sn,  sn,   sn,  sn,   sn,   sr,  ts, shl},
/*long_jump*/	{slr, ss,  sn,  slr,  err, err,  err,  sr,  ts, shl},
/*long_resume*/	{clr, ss,  sn,  clrs, err, err,  err,  sr,  ts, shl},
/*step_resume*/	{sr,  sr,  sr,  sr,   sr,  sr,   sr,   sr,  ts, shl},
/*through_sig*/ {ts,  ts,  ts,  ts,   ts,  ts,   ts,   ts,  ts, shl},
/*shlib*/       {shl, shl, shl, shl,  shl, shl,  shl,  shl, ts, shl}
	      };
#undef kc
#undef ss
#undef sn
#undef sgl
#undef slr
#undef clr
#undef clrs
#undef err
#undef sr
#undef ts
#undef shl
  enum bpstat_what_main_action current_action = BPSTAT_WHAT_KEEP_CHECKING;
  struct bpstat_what retval;

  retval.call_dummy = 0;
  for (; bs != NULL; bs = bs->next)
    {
      enum class bs_class = no_effect;
      if (bs->breakpoint_at == NULL)
	/* I suspect this can happen if it was a momentary breakpoint
	   which has since been deleted.  */
	continue;
      switch (bs->breakpoint_at->type)
	{
	case bp_breakpoint:
	case bp_hardware_breakpoint:
	case bp_until:
	case bp_finish:
	  if (bs->stop)
	    {
	      if (bs->print)
		bs_class = bp_noisy;
	      else
		bs_class = bp_silent;
	    }
	  else
	    bs_class = bp_nostop;
	  break;
	case bp_watchpoint:
	case bp_hardware_watchpoint:
	case bp_read_watchpoint:
	case bp_access_watchpoint:
	  if (bs->stop)
	    {
	      if (bs->print)
		bs_class = wp_noisy;
	      else
		bs_class = wp_silent;
	    }
	  else
	    /* There was a watchpoint, but we're not stopping.  This requires
	       no further action.  */
	    bs_class = no_effect;
	  break;
	case bp_longjmp:
	  bs_class = long_jump;
	  break;
	case bp_longjmp_resume:
	  bs_class = long_resume;
	  break;
	case bp_step_resume:
	  if (bs->stop)
	    {
	      bs_class = step_resume;
	    }
	  else
	    /* It is for the wrong frame.  */
	    bs_class = bp_nostop;
	  break;
	case bp_through_sigtramp:
	  bs_class = through_sig;
	  break;
	case bp_watchpoint_scope:
	  bs_class = bp_nostop;
	  break;
	case bp_shlib_event:
	  bs_class = shlib_event;
	  break;
	case bp_call_dummy:
	  /* Make sure the action is stop (silent or noisy), so infrun.c
	     pops the dummy frame.  */
	  bs_class = bp_silent;
	  retval.call_dummy = 1;
	  break;
	}
      current_action = table[(int)bs_class][(int)current_action];
    }
  retval.main_action = current_action;
  return retval;
}

/* Nonzero if we should step constantly (e.g. watchpoints on machines
   without hardware support).  This isn't related to a specific bpstat,
   just to things like whether watchpoints are set.  */

int 
bpstat_should_step ()
{
  struct breakpoint *b;
  ALL_BREAKPOINTS (b)
    if (b->enable == enabled && b->type == bp_watchpoint)
      return 1;
  return 0;
}

/* Print information on breakpoint number BNUM, or -1 if all.
   If WATCHPOINTS is zero, process only breakpoints; if WATCHPOINTS
   is nonzero, process only watchpoints.  */

static void
breakpoint_1 (bnum, allflag)
     int bnum;
     int allflag;
{
  register struct breakpoint *b;
  register struct command_line *l;
  register struct symbol *sym;
  CORE_ADDR last_addr = (CORE_ADDR)-1;
  int found_a_breakpoint = 0;
  static char *bptypes[] = {"breakpoint", "hw breakpoint",
		              "until", "finish", "watchpoint",
			      "hw watchpoint", "read watchpoint",
			      "acc watchpoint", "longjmp",
			      "longjmp resume", "step resume",
			      "sigtramp",
			      "watchpoint scope", "call dummy",
			      "shlib events" };
  static char *bpdisps[] = {"del", "dstp", "dis", "keep"};
  static char bpenables[] = "nyn";
  char wrap_indent[80];

  ALL_BREAKPOINTS (b)
    if (bnum == -1
	|| bnum == b->number)
      {
/*  We only print out user settable breakpoints unless the allflag is set. */
	if (!allflag
	    && b->type != bp_breakpoint
	    && b->type != bp_hardware_breakpoint
	    && b->type != bp_watchpoint
	    && b->type != bp_read_watchpoint
	    && b->type != bp_access_watchpoint
	    && b->type != bp_hardware_watchpoint)
	  continue;

	if (!found_a_breakpoint++)
	  {
            annotate_breakpoints_headers ();
  
            annotate_field (0);
            printf_filtered ("Num ");
            annotate_field (1);
            printf_filtered ("Type           ");
            annotate_field (2);
            printf_filtered ("Disp ");
            annotate_field (3);
            printf_filtered ("Enb ");
            if (addressprint)
              {
                annotate_field (4);
                printf_filtered ("Address    ");
              }
            annotate_field (5);
            printf_filtered ("What\n");
  
            annotate_breakpoints_table ();
          }
  
        annotate_record ();
        annotate_field (0);
        printf_filtered ("%-3d ", b->number);
        annotate_field (1);
        printf_filtered ("%-14s ", bptypes[(int)b->type]);
        annotate_field (2);
        printf_filtered ("%-4s ", bpdisps[(int)b->disposition]);
        annotate_field (3);
	printf_filtered ("%-3c ", bpenables[(int)b->enable]);

	strcpy (wrap_indent, "                           ");
	if (addressprint)
	  strcat (wrap_indent, "           ");
	switch (b->type)
	  {
	  case bp_watchpoint:
	  case bp_hardware_watchpoint:
	  case bp_read_watchpoint:
	  case bp_access_watchpoint:
	    /* Field 4, the address, is omitted (which makes the columns
	       not line up too nicely with the headers, but the effect
	       is relatively readable).  */
	    annotate_field (5);
	    print_expression (b->exp, gdb_stdout);
	    break;

	  case bp_breakpoint:
	  case bp_hardware_breakpoint:
	  case bp_until:
	  case bp_finish:
	  case bp_longjmp:
	  case bp_longjmp_resume:
	  case bp_step_resume:
	  case bp_through_sigtramp:
	  case bp_watchpoint_scope:
	  case bp_call_dummy:
	  case bp_shlib_event:
	    if (addressprint)
	      {
	        annotate_field (4);
		/* FIXME-32x64: need a print_address_numeric with
                   field width */
		printf_filtered
		  ("%s ",
		   local_hex_string_custom
		   ((unsigned long) b->address, "08l"));
	      }

	    annotate_field (5);

	    last_addr = b->address;
	    if (b->source_file)
	      {
		sym = find_pc_function (b->address);
		if (sym)
		  {
		    fputs_filtered ("in ", gdb_stdout);
		    fputs_filtered (SYMBOL_SOURCE_NAME (sym), gdb_stdout);
		    wrap_here (wrap_indent);
		    fputs_filtered (" at ", gdb_stdout);
		  }
		fputs_filtered (b->source_file, gdb_stdout);
		printf_filtered (":%d", b->line_number);
	      }
	    else
	      print_address_symbolic (b->address, gdb_stdout, demangle, " ");
	    break;
	  }

	printf_filtered ("\n");

	if (b->frame)
	  {
            annotate_field (6);

	    printf_filtered ("\tstop only in stack frame at ");
	    print_address_numeric (b->frame, 1, gdb_stdout);
	    printf_filtered ("\n");
	  }

	if (b->cond)
	  {
            annotate_field (7);

	    printf_filtered ("\tstop only if ");
	    print_expression (b->cond, gdb_stdout);
	    printf_filtered ("\n");
	  }

	if (b->thread != -1)
	  {
	    /* FIXME should make an annotation for this */
	    printf_filtered ("\tstop only in thread %d\n", b->thread);
	  }

        if (show_breakpoint_hit_counts && b->hit_count)
	  {
	    /* FIXME should make an annotation for this */

	    printf_filtered ("\tbreakpoint already hit %d time%s\n",
			     b->hit_count, (b->hit_count == 1 ? "" : "s"));
	  }

	if (b->ignore_count)
	  {
            annotate_field (8);

	    printf_filtered ("\tignore next %d hits\n", b->ignore_count);
	  }

	if ((l = b->commands))
	  {
            annotate_field (9);

	    while (l)
	      {
		print_command_line (l, 4);
		l = l->next;
	      }
	  }
      }

  if (!found_a_breakpoint)
    {
      if (bnum == -1)
        printf_filtered ("No breakpoints or watchpoints.\n");
      else
        printf_filtered ("No breakpoint or watchpoint number %d.\n", bnum);
    }
  else
    /* Compare against (CORE_ADDR)-1 in case some compiler decides
       that a comparison of an unsigned with -1 is always false.  */
    if (last_addr != (CORE_ADDR)-1)
      set_next_address (last_addr);

  annotate_breakpoints_table_end ();
}

/* ARGSUSED */
static void
breakpoints_info (bnum_exp, from_tty)
     char *bnum_exp;
     int from_tty;
{
  int bnum = -1;

  if (bnum_exp)
    bnum = parse_and_eval_address (bnum_exp);

  breakpoint_1 (bnum, 0);
}

#if MAINTENANCE_CMDS

/* ARGSUSED */
static void
maintenance_info_breakpoints (bnum_exp, from_tty)
     char *bnum_exp;
     int from_tty;
{
  int bnum = -1;

  if (bnum_exp)
    bnum = parse_and_eval_address (bnum_exp);

  breakpoint_1 (bnum, 1);
}

#endif

/* Print a message describing any breakpoints set at PC.  */

static void
describe_other_breakpoints (pc)
     register CORE_ADDR pc;
{
  register int others = 0;
  register struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    if (b->address == pc)
      others++;
  if (others > 0)
    {
      printf_filtered ("Note: breakpoint%s ", (others > 1) ? "s" : "");
      ALL_BREAKPOINTS (b)
	if (b->address == pc)
	  {
	    others--;
	    printf_filtered
	      ("%d%s%s ",
	       b->number,
	       ((b->enable == disabled || b->enable == shlib_disabled)
		? " (disabled)" : ""),
	       (others > 1) ? "," : ((others == 1) ? " and" : ""));
	  }
      printf_filtered ("also set at pc ");
      print_address_numeric (pc, 1, gdb_stdout);
      printf_filtered (".\n");
    }
}

/* Set the default place to put a breakpoint
   for the `break' command with no arguments.  */

void
set_default_breakpoint (valid, addr, symtab, line)
     int valid;
     CORE_ADDR addr;
     struct symtab *symtab;
     int line;
{
  default_breakpoint_valid = valid;
  default_breakpoint_address = addr;
  default_breakpoint_symtab = symtab;
  default_breakpoint_line = line;
}

/* Rescan breakpoints at address ADDRESS,
   marking the first one as "first" and any others as "duplicates".
   This is so that the bpt instruction is only inserted once.  */

static void
check_duplicates (address)
     CORE_ADDR address;
{
  register struct breakpoint *b;
  register int count = 0;

  if (address == 0)		/* Watchpoints are uninteresting */
    return;

  ALL_BREAKPOINTS (b)
    if (b->enable != disabled
	&& b->enable != shlib_disabled
	&& b->address == address)
      {
	count++;
	b->duplicate = count > 1;
      }
}

/* Low level routine to set a breakpoint.
   Takes as args the three things that every breakpoint must have.
   Returns the breakpoint object so caller can set other things.
   Does not set the breakpoint number!
   Does not print anything.

   ==> This routine should not be called if there is a chance of later
   error(); otherwise it leaves a bogus breakpoint on the chain.  Validate
   your arguments BEFORE calling this routine!  */

static struct breakpoint *
set_raw_breakpoint (sal)
     struct symtab_and_line sal;
{
  register struct breakpoint *b, *b1;

  b = (struct breakpoint *) xmalloc (sizeof (struct breakpoint));
  memset (b, 0, sizeof (*b));
  b->address = sal.pc;
  if (sal.symtab == NULL)
    b->source_file = NULL;
  else
    b->source_file = savestring (sal.symtab->filename,
				 strlen (sal.symtab->filename));
  b->language = current_language->la_language;
  b->input_radix = input_radix;
  b->thread = -1;
  b->line_number = sal.line;
  b->enable = enabled;
  b->next = 0;
  b->silent = 0;
  b->ignore_count = 0;
  b->commands = NULL;
  b->frame = 0;

  /* Add this breakpoint to the end of the chain
     so that a list of breakpoints will come out in order
     of increasing numbers.  */

  b1 = breakpoint_chain;
  if (b1 == 0)
    breakpoint_chain = b;
  else
    {
      while (b1->next)
	b1 = b1->next;
      b1->next = b;
    }

  check_duplicates (sal.pc);
  breakpoints_changed ();

  return b;
}

#ifdef GET_LONGJMP_TARGET

static void
create_longjmp_breakpoint (func_name)
     char *func_name;
{
  struct symtab_and_line sal;
  struct breakpoint *b;

  if (func_name != NULL)
    {
      struct minimal_symbol *m;

      m = lookup_minimal_symbol_text (func_name, NULL, (struct objfile *)NULL);
      if (m)
	sal.pc = SYMBOL_VALUE_ADDRESS (m);
      else
	return;
    }
  else
    sal.pc = 0;

  sal.symtab = NULL;
  sal.line = 0;

  b = set_raw_breakpoint (sal);
  if (!b) return;

  b->type = func_name != NULL ? bp_longjmp : bp_longjmp_resume;
  b->disposition = donttouch;
  b->enable = disabled;
  b->silent = 1;
  if (func_name)
    b->addr_string = strsave(func_name);
  b->number = internal_breakpoint_number--;
}

#endif	/* #ifdef GET_LONGJMP_TARGET */

/* Call this routine when stepping and nexting to enable a breakpoint if we do
   a longjmp().  When we hit that breakpoint, call
   set_longjmp_resume_breakpoint() to figure out where we are going. */

void
enable_longjmp_breakpoint()
{
  register struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    if (b->type == bp_longjmp)
      {
	b->enable = enabled;
	check_duplicates (b->address);
      }
}

void
disable_longjmp_breakpoint()
{
  register struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    if (   b->type == bp_longjmp
	|| b->type == bp_longjmp_resume)
      {
	b->enable = disabled;
	check_duplicates (b->address);
      }
}

#ifdef SOLIB_ADD
void
remove_solib_event_breakpoints ()
{
  register struct breakpoint *b, *temp;

  ALL_BREAKPOINTS_SAFE (b, temp)
    if (b->type == bp_shlib_event)
      delete_breakpoint (b);
}

void
create_solib_event_breakpoint (address)
     CORE_ADDR address;
{
  struct breakpoint *b;
  struct symtab_and_line sal;

  sal.pc = address;
  sal.symtab = NULL;
  sal.line = 0;
  b = set_raw_breakpoint (sal);
  b->number = internal_breakpoint_number--;
  b->disposition = donttouch;
  b->type = bp_shlib_event;
}

/* Try to reenable any breakpoints in shared libraries.  */
void
re_enable_breakpoints_in_shlibs ()
{
  struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    if (b->enable == shlib_disabled)
      {
	char buf[1];

	/* Do not reenable the breakpoint if the shared library
	   is still not mapped in.  */
	if (target_read_memory (b->address, buf, 1) == 0)
	  b->enable = enabled;
      }
}

#endif

static int
hw_breakpoint_used_count()
{
  register struct breakpoint *b;
  int i = 0;

  ALL_BREAKPOINTS (b)
    {
      if (b->type == bp_hardware_breakpoint && b->enable == enabled)
	i++;
    }

  return i;
}

static int
hw_watchpoint_used_count(type, other_type_used)
    enum bptype type;
    int *other_type_used;
{
  register struct breakpoint *b;
  int i = 0;

  *other_type_used = 0;
  ALL_BREAKPOINTS (b)
    {
      if (b->enable == enabled)
	{
          if (b->type == type) i++;
          else if ((b->type == bp_hardware_watchpoint ||
	       b->type == bp_read_watchpoint ||
	       b->type == bp_access_watchpoint)
	       && b->enable == enabled)
	    *other_type_used = 1;
        }
    }
  return i;
}

/* Call this after hitting the longjmp() breakpoint.  Use this to set a new
   breakpoint at the target of the jmp_buf.

   FIXME - This ought to be done by setting a temporary breakpoint that gets
   deleted automatically...
*/

void
set_longjmp_resume_breakpoint(pc, frame)
     CORE_ADDR pc;
     struct frame_info *frame;
{
  register struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    if (b->type == bp_longjmp_resume)
      {
	b->address = pc;
	b->enable = enabled;
	if (frame != NULL)
	  b->frame = frame->frame;
	else
	  b->frame = 0;
	check_duplicates (b->address);
	return;
      }
}

/* Set a breakpoint that will evaporate an end of command
   at address specified by SAL.
   Restrict it to frame FRAME if FRAME is nonzero.  */

struct breakpoint *
set_momentary_breakpoint (sal, frame, type)
     struct symtab_and_line sal;
     struct frame_info *frame;
     enum bptype type;
{
  register struct breakpoint *b;
  b = set_raw_breakpoint (sal);
  b->type = type;
  b->enable = enabled;
  b->disposition = donttouch;
  b->frame = (frame ? frame->frame : 0);

  /* If we're debugging a multi-threaded program, then we
     want momentary breakpoints to be active in only a 
     single thread of control.  */
  if (in_thread_list (inferior_pid))
    b->thread = pid_to_thread_id (inferior_pid);

  return b;
}


/* Tell the user we have just set a breakpoint B.  */

static void
mention (b)
     struct breakpoint *b;
{
  int say_where = 0;

  /* FIXME: This is misplaced; mention() is called by things (like hitting a
     watchpoint) other than breakpoint creation.  It should be possible to
     clean this up and at the same time replace the random calls to
     breakpoint_changed with this hook, as has already been done for
     delete_breakpoint_hook and so on.  */
  if (create_breakpoint_hook)
    create_breakpoint_hook (b);

  switch (b->type)
    {
    case bp_watchpoint:
      printf_filtered ("Watchpoint %d: ", b->number);
      print_expression (b->exp, gdb_stdout);
      break;
    case bp_hardware_watchpoint:
      printf_filtered ("Hardware watchpoint %d: ", b->number);
      print_expression (b->exp, gdb_stdout);
      break;
    case bp_read_watchpoint:
      printf_filtered ("Hardware read watchpoint %d: ", b->number);
      print_expression (b->exp, gdb_stdout);
      break;
    case bp_access_watchpoint:
      printf_filtered ("Hardware access(read/write) watchpoint %d: ",b->number);
      print_expression (b->exp, gdb_stdout);
      break;
    case bp_breakpoint:
      printf_filtered ("Breakpoint %d", b->number);
      say_where = 1;
      break;
    case bp_hardware_breakpoint:
      printf_filtered ("Hardware assisted breakpoint %d", b->number);
      say_where = 1;
      break;
    case bp_until:
    case bp_finish:
    case bp_longjmp:
    case bp_longjmp_resume:
    case bp_step_resume:
    case bp_through_sigtramp:
    case bp_call_dummy:
    case bp_watchpoint_scope:
    case bp_shlib_event:
      break;
    }
  if (say_where)
    {
      if (addressprint || b->source_file == NULL)
	{
	  printf_filtered (" at ");
	  print_address_numeric (b->address, 1, gdb_stdout);
	}
      if (b->source_file)
	printf_filtered (": file %s, line %d.",
			 b->source_file, b->line_number);
    }
  printf_filtered ("\n");
}


/* Set a breakpoint according to ARG (function, linenum or *address)
   flag: first bit  : 0 non-temporary, 1 temporary.
	 second bit : 0 normal breakpoint, 1 hardware breakpoint. */

static void
break_command_1 (arg, flag, from_tty)
     char *arg;
     int flag, from_tty;
{
  int tempflag, hardwareflag;
  struct symtabs_and_lines sals;
  struct symtab_and_line sal;
  register struct expression *cond = 0;
  register struct breakpoint *b;

  /* Pointers in arg to the start, and one past the end, of the condition.  */
  char *cond_start = NULL;
  char *cond_end = NULL;
  /* Pointers in arg to the start, and one past the end,
     of the address part.  */
  char *addr_start = NULL;
  char *addr_end = NULL;
  struct cleanup *old_chain;
  struct cleanup *canonical_strings_chain = NULL;
  char **canonical = (char **)NULL;
  int i;
  int thread;

  hardwareflag = flag & BP_HARDWAREFLAG;
  tempflag = flag & BP_TEMPFLAG;

  sals.sals = NULL;
  sals.nelts = 0;

  sal.line = sal.pc = sal.end = 0;
  sal.symtab = 0;

  /* If no arg given, or if first arg is 'if ', use the default breakpoint. */

  if (!arg || (arg[0] == 'i' && arg[1] == 'f' 
	       && (arg[2] == ' ' || arg[2] == '\t')))
    {
      if (default_breakpoint_valid)
	{
	  sals.sals = (struct symtab_and_line *) 
	    xmalloc (sizeof (struct symtab_and_line));
	  sal.pc = default_breakpoint_address;
	  sal.line = default_breakpoint_line;
	  sal.symtab = default_breakpoint_symtab;
	  sals.sals[0] = sal;
	  sals.nelts = 1;
	}
      else
	error ("No default breakpoint address now.");
    }
  else
    {
      addr_start = arg;

      /* Force almost all breakpoints to be in terms of the
	 current_source_symtab (which is decode_line_1's default).  This
	 should produce the results we want almost all of the time while
	 leaving default_breakpoint_* alone.  */
      if (default_breakpoint_valid
	  && (!current_source_symtab
	      || (arg && (*arg == '+' || *arg == '-'))))
	sals = decode_line_1 (&arg, 1, default_breakpoint_symtab,
			      default_breakpoint_line, &canonical);
      else
	sals = decode_line_1 (&arg, 1, (struct symtab *)NULL, 0, &canonical);

      addr_end = arg;
    }
  
  if (! sals.nelts) 
    return;

  /* Make sure that all storage allocated in decode_line_1 gets freed in case
     the following `for' loop errors out.  */
  old_chain = make_cleanup (free, sals.sals);
  if (canonical != (char **)NULL)
    {
      make_cleanup (free, canonical);
      canonical_strings_chain = make_cleanup (null_cleanup, 0);
      for (i = 0; i < sals.nelts; i++)
	{
	  if (canonical[i] != NULL)
	    make_cleanup (free, canonical[i]);
	}
    }

  thread = -1;			/* No specific thread yet */

  /* Resolve all line numbers to PC's, and verify that conditions
     can be parsed, before setting any breakpoints.  */
  for (i = 0; i < sals.nelts; i++)
    {
      char *tok, *end_tok;
      int toklen;

      resolve_sal_pc (&sals.sals[i]);
      
      tok = arg;

      while (tok && *tok)
	{
	  while (*tok == ' ' || *tok == '\t')
	    tok++;

	  end_tok = tok;

	  while (*end_tok != ' ' && *end_tok != '\t' && *end_tok != '\000')
	    end_tok++;

	  toklen = end_tok - tok;

	  if (toklen >= 1 && strncmp (tok, "if", toklen) == 0)
	    {
	      tok = cond_start = end_tok + 1;
	      cond = parse_exp_1 (&tok, block_for_pc (sals.sals[i].pc), 0);
	      cond_end = tok;
	    }
	  else if (toklen >= 1 && strncmp (tok, "thread", toklen) == 0)
	    {
	      char *tmptok;

	      tok = end_tok + 1;
	      tmptok = tok;
	      thread = strtol (tok, &tok, 0);
	      if (tok == tmptok)
		error ("Junk after thread keyword.");
	      if (!valid_thread_id (thread))
		error ("Unknown thread %d\n", thread);
	    }
	  else
	    error ("Junk at end of arguments.");
	}
    }
  if (hardwareflag)
    {
      int i, target_resources_ok;

      i = hw_breakpoint_used_count ();  
      target_resources_ok = TARGET_CAN_USE_HARDWARE_WATCHPOINT (
		bp_hardware_breakpoint, i + sals.nelts, 0);
      if (target_resources_ok == 0)
	error ("No hardware breakpoint support in the target.");
      else if (target_resources_ok < 0)
        error ("Hardware breakpoints used exceeds limit.");
    }

  /* Remove the canonical strings from the cleanup, they are needed below.  */
  if (canonical != (char **)NULL)
    discard_cleanups (canonical_strings_chain);

  /* Now set all the breakpoints.  */
  for (i = 0; i < sals.nelts; i++)
    {
      sal = sals.sals[i];

      if (from_tty)
	describe_other_breakpoints (sal.pc);

      b = set_raw_breakpoint (sal);
      set_breakpoint_count (breakpoint_count + 1);
      b->number = breakpoint_count;
      b->type = hardwareflag ? bp_hardware_breakpoint : bp_breakpoint;
      b->cond = cond;
      b->thread = thread;

      /* If a canonical line spec is needed use that instead of the
	 command string.  */
      if (canonical != (char **)NULL && canonical[i] != NULL)
	b->addr_string = canonical[i];
      else if (addr_start)
	b->addr_string = savestring (addr_start, addr_end - addr_start);
      if (cond_start)
	b->cond_string = savestring (cond_start, cond_end - cond_start);
				     
      b->enable = enabled;
      b->disposition = tempflag ? del : donttouch;

      mention (b);
    }

  if (sals.nelts > 1)
    {
      printf_filtered ("Multiple breakpoints were set.\n");
      printf_filtered ("Use the \"delete\" command to delete unwanted breakpoints.\n");
    }
  do_cleanups (old_chain);
}

/* Helper function for break_command_1 and disassemble_command.  */

void
resolve_sal_pc (sal)
     struct symtab_and_line *sal;
{
  CORE_ADDR pc;

  if (sal->pc == 0 && sal->symtab != 0)
    {
      pc = find_line_pc (sal->symtab, sal->line);
      if (pc == 0)
	error ("No line %d in file \"%s\".",
	       sal->line, sal->symtab->filename);
      sal->pc = pc;
    }
}

void
break_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  break_command_1 (arg, 0, from_tty);
}

static void
tbreak_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  break_command_1 (arg, BP_TEMPFLAG, from_tty);
}

static void
hbreak_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  break_command_1 (arg, BP_HARDWAREFLAG, from_tty);
}

static void
thbreak_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  break_command_1 (arg, (BP_TEMPFLAG | BP_HARDWAREFLAG), from_tty);
}

/* ARGSUSED */
/* accessflag:  0: watch write, 1: watch read, 2: watch access(read or write)
*/
static void
watch_command_1 (arg, accessflag, from_tty)
     char *arg;
     int accessflag;
     int from_tty;
{
  struct breakpoint *b;
  struct symtab_and_line sal;
  struct expression *exp;
  struct block *exp_valid_block;
  struct value *val, *mark;
  struct frame_info *frame;
  struct frame_info *prev_frame = NULL;
  char *exp_start = NULL;
  char *exp_end = NULL;
  char *tok, *end_tok;
  int toklen;
  char *cond_start = NULL;
  char *cond_end = NULL;
  struct expression *cond = NULL;
  int i, other_type_used, target_resources_ok;
  enum bptype bp_type;
  int mem_cnt = 0;

  sal.pc = 0;
  sal.symtab = NULL;
  sal.line = 0;
  
  /* Parse arguments.  */
  innermost_block = NULL;
  exp_start = arg;
  exp = parse_exp_1 (&arg, 0, 0);
  exp_end = arg;
  exp_valid_block = innermost_block;
  mark = value_mark ();
  val = evaluate_expression (exp);
  release_value (val);
  if (VALUE_LAZY (val))
    value_fetch_lazy (val);

  tok = arg;
  while (*tok == ' ' || *tok == '\t')
    tok++;
  end_tok = tok;

  while (*end_tok != ' ' && *end_tok != '\t' && *end_tok != '\000')
    end_tok++;

  toklen = end_tok - tok;
  if (toklen >= 1 && strncmp (tok, "if", toklen) == 0)
    {
      tok = cond_start = end_tok + 1;
      cond = parse_exp_1 (&tok, 0, 0);
      cond_end = tok;
    }
  if (*tok)
    error("Junk at end of command.");

  if (accessflag == 1) bp_type = bp_read_watchpoint;
  else if (accessflag == 2) bp_type = bp_access_watchpoint;
  else bp_type = bp_hardware_watchpoint;

  mem_cnt = can_use_hardware_watchpoint (val);
  if (mem_cnt == 0 && bp_type != bp_hardware_watchpoint)
    error ("Expression cannot be implemented with read/access watchpoint.");
  if (mem_cnt != 0) { 
    i = hw_watchpoint_used_count (bp_type, &other_type_used);
    target_resources_ok = TARGET_CAN_USE_HARDWARE_WATCHPOINT(
		bp_type, i + mem_cnt, other_type_used);
    if (target_resources_ok == 0 && bp_type != bp_hardware_watchpoint)
      error ("Target does not have this type of hardware watchpoint support.");
    if (target_resources_ok < 0 && bp_type != bp_hardware_watchpoint)
      error ("Target resources have been allocated for other types of watchpoints.");
  }
  
  /* Now set up the breakpoint.  */
  b = set_raw_breakpoint (sal);
  set_breakpoint_count (breakpoint_count + 1);
  b->number = breakpoint_count;
  b->disposition = donttouch;
  b->exp = exp;
  b->exp_valid_block = exp_valid_block;
  b->exp_string = savestring (exp_start, exp_end - exp_start);
  b->val = val;
  b->cond = cond;
  if (cond_start)
    b->cond_string = savestring (cond_start, cond_end - cond_start);
  else
    b->cond_string = 0;
         
  frame = block_innermost_frame (exp_valid_block);
  if (frame)
    {
      prev_frame = get_prev_frame (frame);
      b->watchpoint_frame = frame->frame;
    }
  else
    b->watchpoint_frame = (CORE_ADDR)0;

  if (mem_cnt && target_resources_ok > 0)
    b->type = bp_type;
  else
    b->type = bp_watchpoint;

  /* If the expression is "local", then set up a "watchpoint scope"
     breakpoint at the point where we've left the scope of the watchpoint
     expression.  */
  if (innermost_block)
    {
      struct breakpoint *scope_breakpoint;
      struct symtab_and_line scope_sal;

      if (prev_frame)
	{
	  scope_sal.pc = get_frame_pc (prev_frame);
	  scope_sal.symtab = NULL;
	  scope_sal.line = 0;
	  
	  scope_breakpoint = set_raw_breakpoint (scope_sal);
	  set_breakpoint_count (breakpoint_count + 1);
	  scope_breakpoint->number = breakpoint_count;

	  scope_breakpoint->type = bp_watchpoint_scope;
	  scope_breakpoint->enable = enabled;

	  /* Automatically delete the breakpoint when it hits.  */
	  scope_breakpoint->disposition = del;

	  /* Only break in the proper frame (help with recursion).  */
	  scope_breakpoint->frame = prev_frame->frame;

	  /* Set the address at which we will stop.  */
	  scope_breakpoint->address = get_frame_pc (prev_frame);

	  /* The scope breakpoint is related to the watchpoint.  We
	     will need to act on them together.  */
	  b->related_breakpoint = scope_breakpoint;
	}
    }
  value_free_to_mark (mark);
  mention (b);
}

/* Return count of locations need to be watched and can be handled
   in hardware.  If the watchpoint can not be handled
   in hardware return zero.  */

static int
can_use_hardware_watchpoint (v)
     struct value *v;
{
  int found_memory_cnt = 0;
	
  /* Make sure all the intermediate values are in memory.  Also make sure
     we found at least one memory expression.  Guards against watch 0x12345,
     which is meaningless, but could cause errors if one tries to insert a 
     hardware watchpoint for the constant expression.  */
  for ( ; v; v = v->next)
    {
      if (v->lval == lval_memory)
	{
	  if (TYPE_LENGTH (VALUE_TYPE (v)) <= REGISTER_SIZE)
	    found_memory_cnt++;
        }
      else if (v->lval != not_lval && v->modifiable == 0)
	return 0;
    }

  /* The expression itself looks suitable for using a hardware
     watchpoint, but give the target machine a chance to reject it.  */
  return found_memory_cnt;
}

static void watch_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  watch_command_1 (arg, 0, from_tty);
}

static void rwatch_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  watch_command_1 (arg, 1, from_tty);
}

static void awatch_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  watch_command_1 (arg, 2, from_tty);
}


/* Helper routine for the until_command routine in infcmd.c.  Here
   because it uses the mechanisms of breakpoints.  */

/* ARGSUSED */
void
until_break_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  struct symtabs_and_lines sals;
  struct symtab_and_line sal;
  struct frame_info *prev_frame = get_prev_frame (selected_frame);
  struct breakpoint *breakpoint;
  struct cleanup *old_chain;

  clear_proceed_status ();

  /* Set a breakpoint where the user wants it and at return from
     this function */
  
  if (default_breakpoint_valid)
    sals = decode_line_1 (&arg, 1, default_breakpoint_symtab,
			  default_breakpoint_line, (char ***)NULL);
  else
    sals = decode_line_1 (&arg, 1, (struct symtab *)NULL, 0, (char ***)NULL);
  
  if (sals.nelts != 1)
    error ("Couldn't get information on specified line.");
  
  sal = sals.sals[0];
  free ((PTR)sals.sals);		/* malloc'd, so freed */
  
  if (*arg)
    error ("Junk at end of arguments.");
  
  resolve_sal_pc (&sal);
  
  breakpoint = set_momentary_breakpoint (sal, selected_frame, bp_until);
  
  old_chain = make_cleanup(delete_breakpoint, breakpoint);

  /* Keep within the current frame */
  
  if (prev_frame)
    {
      sal = find_pc_line (prev_frame->pc, 0);
      sal.pc = prev_frame->pc;
      breakpoint = set_momentary_breakpoint (sal, prev_frame, bp_until);
      make_cleanup(delete_breakpoint, breakpoint);
    }
  
  proceed (-1, TARGET_SIGNAL_DEFAULT, 0);
  do_cleanups(old_chain);
}

#if 0
/* These aren't used; I don't konw what they were for.  */
/* Set a breakpoint at the catch clause for NAME.  */
static int
catch_breakpoint (name)
     char *name;
{
}

static int
disable_catch_breakpoint ()
{
}

static int
delete_catch_breakpoint ()
{
}

static int
enable_catch_breakpoint ()
{
}
#endif /* 0 */

struct sal_chain
{
  struct sal_chain *next;
  struct symtab_and_line sal;
};

#if 0
/* This isn't used; I don't know what it was for.  */
/* For each catch clause identified in ARGS, run FUNCTION
   with that clause as an argument.  */
static struct symtabs_and_lines
map_catch_names (args, function)
     char *args;
     int (*function)();
{
  register char *p = args;
  register char *p1;
  struct symtabs_and_lines sals;
#if 0
  struct sal_chain *sal_chain = 0;
#endif

  if (p == 0)
    error_no_arg ("one or more catch names");

  sals.nelts = 0;
  sals.sals = NULL;

  while (*p)
    {
      p1 = p;
      /* Don't swallow conditional part.  */
      if (p1[0] == 'i' && p1[1] == 'f'
	  && (p1[2] == ' ' || p1[2] == '\t'))
	break;

      if (isalpha (*p1))
	{
	  p1++;
	  while (isalnum (*p1) || *p1 == '_' || *p1 == '$')
	    p1++;
	}

      if (*p1 && *p1 != ' ' && *p1 != '\t')
	error ("Arguments must be catch names.");

      *p1 = 0;
#if 0
      if (function (p))
	{
	  struct sal_chain *next
	    = (struct sal_chain *)alloca (sizeof (struct sal_chain));
	  next->next = sal_chain;
	  next->sal = get_catch_sal (p);
	  sal_chain = next;
	  goto win;
	}
#endif
      printf_unfiltered ("No catch clause for exception %s.\n", p);
#if 0
    win:
#endif
      p = p1;
      while (*p == ' ' || *p == '\t') p++;
    }
}
#endif /* 0 */

/* This shares a lot of code with `print_frame_label_vars' from stack.c.  */

static struct symtabs_and_lines
get_catch_sals (this_level_only)
     int this_level_only;
{
  register struct blockvector *bl;
  register struct block *block;
  int index, have_default = 0;
  CORE_ADDR pc;
  struct symtabs_and_lines sals;
  struct sal_chain *sal_chain = 0;
  char *blocks_searched;

  /* Not sure whether an error message is always the correct response,
     but it's better than a core dump.  */
  if (selected_frame == NULL)
    error ("No selected frame.");
  block = get_frame_block (selected_frame);
  pc = selected_frame->pc;

  sals.nelts = 0;
  sals.sals = NULL;

  if (block == 0)
    error ("No symbol table info available.\n");

  bl = blockvector_for_pc (BLOCK_END (block) - 4, &index);
  blocks_searched = (char *) alloca (BLOCKVECTOR_NBLOCKS (bl) * sizeof (char));
  memset (blocks_searched, 0, BLOCKVECTOR_NBLOCKS (bl) * sizeof (char));

  while (block != 0)
    {
      CORE_ADDR end = BLOCK_END (block) - 4;
      int last_index;

      if (bl != blockvector_for_pc (end, &index))
	error ("blockvector blotch");
      if (BLOCKVECTOR_BLOCK (bl, index) != block)
	error ("blockvector botch");
      last_index = BLOCKVECTOR_NBLOCKS (bl);
      index += 1;

      /* Don't print out blocks that have gone by.  */
      while (index < last_index
	     && BLOCK_END (BLOCKVECTOR_BLOCK (bl, index)) < pc)
	index++;

      while (index < last_index
	     && BLOCK_END (BLOCKVECTOR_BLOCK (bl, index)) < end)
	{
	  if (blocks_searched[index] == 0)
	    {
	      struct block *b = BLOCKVECTOR_BLOCK (bl, index);
	      int nsyms;
	      register int i;
	      register struct symbol *sym;

	      nsyms = BLOCK_NSYMS (b);

	      for (i = 0; i < nsyms; i++)
		{
		  sym = BLOCK_SYM (b, i);
		  if (STREQ (SYMBOL_NAME (sym), "default"))
		    {
		      if (have_default)
			continue;
		      have_default = 1;
		    }
		  if (SYMBOL_CLASS (sym) == LOC_LABEL)
		    {
		      struct sal_chain *next = (struct sal_chain *)
			alloca (sizeof (struct sal_chain));
		      next->next = sal_chain;
		      next->sal = find_pc_line (SYMBOL_VALUE_ADDRESS (sym), 0);
		      sal_chain = next;
		    }
		}
	      blocks_searched[index] = 1;
	    }
	  index++;
	}
      if (have_default)
	break;
      if (sal_chain && this_level_only)
	break;

      /* After handling the function's top-level block, stop.
	 Don't continue to its superblock, the block of
	 per-file symbols.  */
      if (BLOCK_FUNCTION (block))
	break;
      block = BLOCK_SUPERBLOCK (block);
    }

  if (sal_chain)
    {
      struct sal_chain *tmp_chain;

      /* Count the number of entries.  */
      for (index = 0, tmp_chain = sal_chain; tmp_chain;
	   tmp_chain = tmp_chain->next)
	index++;

      sals.nelts = index;
      sals.sals = (struct symtab_and_line *)
	xmalloc (index * sizeof (struct symtab_and_line));
      for (index = 0; sal_chain; sal_chain = sal_chain->next, index++)
	sals.sals[index] = sal_chain->sal;
    }

  return sals;
}

/* Commands to deal with catching exceptions.  */

static void
catch_command_1 (arg, tempflag, from_tty)
     char *arg;
     int tempflag;
     int from_tty;
{
  /* First, translate ARG into something we can deal with in terms
     of breakpoints.  */

  struct symtabs_and_lines sals;
  struct symtab_and_line sal;
  register struct expression *cond = 0;
  register struct breakpoint *b;
  char *save_arg;
  int i;

  sal.line = sal.pc = sal.end = 0;
  sal.symtab = 0;

  /* If no arg given, or if first arg is 'if ', all active catch clauses
     are breakpointed. */

  if (!arg || (arg[0] == 'i' && arg[1] == 'f' 
	       && (arg[2] == ' ' || arg[2] == '\t')))
    {
      /* Grab all active catch clauses.  */
      sals = get_catch_sals (0);
    }
  else
    {
      /* Grab selected catch clauses.  */
      error ("catch NAME not implemented");
#if 0
      /* This isn't used; I don't know what it was for.  */
      sals = map_catch_names (arg, catch_breakpoint);
#endif
    }

  if (! sals.nelts) 
    return;

  save_arg = arg;
  for (i = 0; i < sals.nelts; i++)
    {
      resolve_sal_pc (&sals.sals[i]);
      
      while (arg && *arg)
	{
	  if (arg[0] == 'i' && arg[1] == 'f'
	      && (arg[2] == ' ' || arg[2] == '\t'))
	    cond = parse_exp_1 ((arg += 2, &arg), 
				block_for_pc (sals.sals[i].pc), 0);
	  else
	    error ("Junk at end of arguments.");
	}
      arg = save_arg;
    }

  for (i = 0; i < sals.nelts; i++)
    {
      sal = sals.sals[i];

      if (from_tty)
	describe_other_breakpoints (sal.pc);

      b = set_raw_breakpoint (sal);
      set_breakpoint_count (breakpoint_count + 1);
      b->number = breakpoint_count;
      b->type = bp_breakpoint;
      b->cond = cond;
      b->enable = enabled;
      b->disposition = tempflag ? del : donttouch;

      mention (b);
    }

  if (sals.nelts > 1)
    {
      printf_unfiltered ("Multiple breakpoints were set.\n");
      printf_unfiltered ("Use the \"delete\" command to delete unwanted breakpoints.\n");
    }
  free ((PTR)sals.sals);
}

/* Used by the gui, could be made a worker for other things. */

struct breakpoint *
set_breakpoint_sal (sal)
struct symtab_and_line sal;
{
  struct breakpoint *b;
  b = set_raw_breakpoint (sal);
  set_breakpoint_count (breakpoint_count + 1);
  b->number = breakpoint_count;
  b->type = bp_breakpoint;
  b->cond = 0;
  b->thread = -1;
  return b;
}

#if 0
/* These aren't used; I don't know what they were for.  */
/* Disable breakpoints on all catch clauses described in ARGS.  */
static void
disable_catch (args)
     char *args;
{
  /* Map the disable command to catch clauses described in ARGS.  */
}

/* Enable breakpoints on all catch clauses described in ARGS.  */
static void
enable_catch (args)
     char *args;
{
  /* Map the disable command to catch clauses described in ARGS.  */
}

/* Delete breakpoints on all catch clauses in the active scope.  */
static void
delete_catch (args)
     char *args;
{
  /* Map the delete command to catch clauses described in ARGS.  */
}
#endif /* 0 */

static void
catch_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  catch_command_1 (arg, 0, from_tty);
}

static void
clear_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  register struct breakpoint *b, *b1;
  struct symtabs_and_lines sals;
  struct symtab_and_line sal;
  register struct breakpoint *found;
  int i;

  if (arg)
    {
      sals = decode_line_spec (arg, 1);
    }
  else
    {
      sals.sals = (struct symtab_and_line *) xmalloc (sizeof (struct symtab_and_line));
      sal.line = default_breakpoint_line;
      sal.symtab = default_breakpoint_symtab;
      sal.pc = 0;
      if (sal.symtab == 0)
	error ("No source file specified.");

      sals.sals[0] = sal;
      sals.nelts = 1;
    }

  for (i = 0; i < sals.nelts; i++)
    {
      /* If exact pc given, clear bpts at that pc.
	 But if sal.pc is zero, clear all bpts on specified line.  */
      sal = sals.sals[i];
      found = (struct breakpoint *) 0;
      while (breakpoint_chain
	     && (sal.pc
		 ? breakpoint_chain->address == sal.pc
		 : (breakpoint_chain->source_file != NULL
		    && sal.symtab != NULL
		    && STREQ (breakpoint_chain->source_file,
			      sal.symtab->filename)
		    && breakpoint_chain->line_number == sal.line)))
	{
	  b1 = breakpoint_chain;
	  breakpoint_chain = b1->next;
	  b1->next = found;
	  found = b1;
	}

      ALL_BREAKPOINTS (b)
	while (b->next
	       && b->next->type != bp_watchpoint
	       && b->next->type != bp_hardware_watchpoint
	       && b->next->type != bp_read_watchpoint
	       && b->next->type != bp_access_watchpoint
	       && (sal.pc
		   ? b->next->address == sal.pc
		   : (b->next->source_file != NULL
		      && sal.symtab != NULL
		      && STREQ (b->next->source_file, sal.symtab->filename)
		      && b->next->line_number == sal.line)))
	  {
	    b1 = b->next;
	    b->next = b1->next;
	    b1->next = found;
	    found = b1;
	  }

      if (found == 0)
	{
	  if (arg)
	    error ("No breakpoint at %s.", arg);
	  else
	    error ("No breakpoint at this line.");
	}

      if (found->next) from_tty = 1; /* Always report if deleted more than one */
      if (from_tty) printf_unfiltered ("Deleted breakpoint%s ", found->next ? "s" : "");
      breakpoints_changed ();
      while (found)
	{
	  if (from_tty) printf_unfiltered ("%d ", found->number);
	  b1 = found->next;
	  delete_breakpoint (found);
	  found = b1;
	}
      if (from_tty) putchar_unfiltered ('\n');
    }
  free ((PTR)sals.sals);
}

/* Delete breakpoint in BS if they are `delete' breakpoints and
   all breakpoints that are marked for deletion, whether hit or not.
   This is called after any breakpoint is hit, or after errors.  */

void
breakpoint_auto_delete (bs)
     bpstat bs;
{
  struct breakpoint *b, *temp;

  for (; bs; bs = bs->next)
    if (bs->breakpoint_at && bs->breakpoint_at->disposition == del 
	&& bs->stop)
      delete_breakpoint (bs->breakpoint_at);

  ALL_BREAKPOINTS_SAFE (b, temp)
    {
      if (b->disposition == del_at_next_stop)
	delete_breakpoint (b);
    }
}

/* Delete a breakpoint and clean up all traces of it in the data structures. */

void
delete_breakpoint (bpt)
     struct breakpoint *bpt;
{
  register struct breakpoint *b;
  register bpstat bs;

  if (delete_breakpoint_hook)
    delete_breakpoint_hook (bpt);

  if (bpt->inserted)
    remove_breakpoint (bpt);
      
  if (breakpoint_chain == bpt)
    breakpoint_chain = bpt->next;

  ALL_BREAKPOINTS (b)
    if (b->next == bpt)
      {
	b->next = bpt->next;
	break;
      }

  check_duplicates (bpt->address);
  /* If this breakpoint was inserted, and there is another breakpoint
     at the same address, we need to insert the other breakpoint.  */
  if (bpt->inserted
      && bpt->type != bp_hardware_watchpoint
      && bpt->type != bp_read_watchpoint
      && bpt->type != bp_access_watchpoint)
    {
      ALL_BREAKPOINTS (b)
	if (b->address == bpt->address
	    && !b->duplicate
	    && b->enable != disabled
	    && b->enable != shlib_disabled)
	  {
	    int val;
	    val = target_insert_breakpoint (b->address, b->shadow_contents);
	    if (val != 0)
	      {
		target_terminal_ours_for_output ();
		fprintf_unfiltered (gdb_stderr, "Cannot insert breakpoint %d:\n", b->number);
		memory_error (val, b->address);	/* which bombs us out */
	      }
	    else
	      b->inserted = 1;
	  }
    }

  free_command_lines (&bpt->commands);
  if (bpt->cond)
    free (bpt->cond);
  if (bpt->cond_string != NULL)
    free (bpt->cond_string);
  if (bpt->addr_string != NULL)
    free (bpt->addr_string);
  if (bpt->exp != NULL)
    free (bpt->exp);
  if (bpt->exp_string != NULL)
    free (bpt->exp_string);
  if (bpt->val != NULL)
    value_free (bpt->val);
  if (bpt->source_file != NULL)
    free (bpt->source_file);

  /* Be sure no bpstat's are pointing at it after it's been freed.  */
  /* FIXME, how can we find all bpstat's?
     We just check stop_bpstat for now.  */
  for (bs = stop_bpstat; bs; bs = bs->next)
    if (bs->breakpoint_at == bpt)
      bs->breakpoint_at = NULL;
  free ((PTR)bpt);
}

static void
delete_command (arg, from_tty)
     char *arg;
     int from_tty;
{

  if (arg == 0)
    {
      /* Ask user only if there are some breakpoints to delete.  */
      if (!from_tty
	  || (breakpoint_chain && query ("Delete all breakpoints? ")))
	{
	  /* No arg; clear all breakpoints.  */
	  while (breakpoint_chain)
	    delete_breakpoint (breakpoint_chain);
	}
    }
  else
    map_breakpoint_numbers (arg, delete_breakpoint);
}

/* Reset a breakpoint given it's struct breakpoint * BINT.
   The value we return ends up being the return value from catch_errors.
   Unused in this case.  */

static long
breakpoint_re_set_one (bint)
     char *bint;
{
  struct breakpoint *b = (struct breakpoint *)bint;  /* get past catch_errs */
  struct value *mark;
  int i;
  struct symtabs_and_lines sals;
  char *s;
  enum enable save_enable;

  switch (b->type)
    {
    case bp_breakpoint:
    case bp_hardware_breakpoint:
      if (b->addr_string == NULL)
	{
	  /* Anything without a string can't be re-set. */
	  delete_breakpoint (b);
	  return 0;
	}
      /* In case we have a problem, disable this breakpoint.  We'll restore
	 its status if we succeed.  */
      save_enable = b->enable;
      b->enable = disabled;

      set_language (b->language);
      input_radix = b->input_radix;
      s = b->addr_string;
      sals = decode_line_1 (&s, 1, (struct symtab *)NULL, 0, (char ***)NULL);
      for (i = 0; i < sals.nelts; i++)
	{
	  resolve_sal_pc (&sals.sals[i]);

	  /* Reparse conditions, they might contain references to the
	     old symtab.  */
	  if (b->cond_string != NULL)
	    {
	      s = b->cond_string;
	      if (b->cond)
		free ((PTR)b->cond);
	      b->cond = parse_exp_1 (&s, block_for_pc (sals.sals[i].pc), 0);
	    }

	  /* We need to re-set the breakpoint if the address changes...*/
	  if (b->address != sals.sals[i].pc
	      /* ...or new and old breakpoints both have source files, and
		 the source file name or the line number changes...  */
	      || (b->source_file != NULL
		  && sals.sals[i].symtab != NULL
		  && (!STREQ (b->source_file, sals.sals[i].symtab->filename)
		      || b->line_number != sals.sals[i].line)
		  )
	      /* ...or we switch between having a source file and not having
		 one.  */
	      || ((b->source_file == NULL) != (sals.sals[i].symtab == NULL))
	      )
	    {
	      if (b->source_file != NULL)
		free (b->source_file);
	      if (sals.sals[i].symtab == NULL)
		b->source_file = NULL;
	      else
		b->source_file =
		  savestring (sals.sals[i].symtab->filename,
			      strlen (sals.sals[i].symtab->filename));
	      b->line_number = sals.sals[i].line;
	      b->address = sals.sals[i].pc;
	  
	      check_duplicates (b->address);

	      mention (b);

	      /* Might be better to do this just once per breakpoint_re_set,
		 rather than once for every breakpoint.  */
	      breakpoints_changed ();
	    }
	  b->enable = save_enable;	/* Restore it, this worked. */
	}
      free ((PTR)sals.sals);
      break;

    case bp_watchpoint:
    case bp_hardware_watchpoint:
    case bp_read_watchpoint:
    case bp_access_watchpoint:
      innermost_block = NULL;
      /* The issue arises of what context to evaluate this in.  The same
	 one as when it was set, but what does that mean when symbols have
	 been re-read?  We could save the filename and functionname, but
	 if the context is more local than that, the best we could do would
	 be something like how many levels deep and which index at that
	 particular level, but that's going to be less stable than filenames
	 or functionnames.  */
      /* So for now, just use a global context.  */
      if (b->exp)
	free ((PTR)b->exp);
      b->exp = parse_expression (b->exp_string);
      b->exp_valid_block = innermost_block;
      mark = value_mark ();
      if (b->val)
	value_free (b->val);
      b->val = evaluate_expression (b->exp);
      release_value (b->val);
      if (VALUE_LAZY (b->val))
	value_fetch_lazy (b->val);

      if (b->cond_string != NULL)
	{
	  s = b->cond_string;
	  if (b->cond)
	    free ((PTR)b->cond);
	  b->cond = parse_exp_1 (&s, (struct block *)0, 0);
	}
      if (b->enable == enabled)
	mention (b);
      value_free_to_mark (mark);
      break;

    default:
      printf_filtered ("Deleting unknown breakpoint type %d\n", b->type);
      /* fall through */
    /* Delete longjmp breakpoints, they will be reset later by
       breakpoint_re_set.  */
    case bp_longjmp:
    case bp_longjmp_resume:
      delete_breakpoint (b);
      break;

    /* This breakpoint is special, it's set up when the inferior
       starts and we really don't want to touch it.  */
    case bp_shlib_event:

    /* Keep temporary breakpoints, which can be encountered when we step
       over a dlopen call and SOLIB_ADD is resetting the breakpoints.
       Otherwise these should have been blown away via the cleanup chain
       or by breakpoint_init_inferior when we rerun the executable.  */
    case bp_until:
    case bp_finish:
    case bp_watchpoint_scope:
    case bp_call_dummy:
    case bp_step_resume:
      break;
    }

  return 0;
}

/* Re-set all breakpoints after symbols have been re-loaded.  */
void
breakpoint_re_set ()
{
  struct breakpoint *b, *temp;
  enum language save_language;
  int save_input_radix;
  static char message1[] = "Error in re-setting breakpoint %d:\n";
  char message[sizeof (message1) + 30 /* slop */];
  
  save_language = current_language->la_language;
  save_input_radix = input_radix;
  ALL_BREAKPOINTS_SAFE (b, temp)
    {
      sprintf (message, message1, b->number);	/* Format possible error msg */
      catch_errors (breakpoint_re_set_one, (char *) b, message,
		    RETURN_MASK_ALL);
    }
  set_language (save_language);
  input_radix = save_input_radix;

#ifdef GET_LONGJMP_TARGET
  create_longjmp_breakpoint ("longjmp");
  create_longjmp_breakpoint ("_longjmp");
  create_longjmp_breakpoint ("siglongjmp");
  create_longjmp_breakpoint (NULL);
#endif

#if 0
  /* Took this out (temporarily at least), since it produces an extra 
     blank line at startup. This messes up the gdbtests. -PB */
  /* Blank line to finish off all those mention() messages we just printed.  */
  printf_filtered ("\n");
#endif
}

/* Set ignore-count of breakpoint number BPTNUM to COUNT.
   If from_tty is nonzero, it prints a message to that effect,
   which ends with a period (no newline).  */

void
set_ignore_count (bptnum, count, from_tty)
     int bptnum, count, from_tty;
{
  register struct breakpoint *b;

  if (count < 0)
    count = 0;

  ALL_BREAKPOINTS (b)
    if (b->number == bptnum)
      {
	b->ignore_count = count;
	if (!from_tty)
	  return;
	else if (count == 0)
	  printf_filtered ("Will stop next time breakpoint %d is reached.",
			   bptnum);
	else if (count == 1)
	  printf_filtered ("Will ignore next crossing of breakpoint %d.",
			   bptnum);
	else
	  printf_filtered ("Will ignore next %d crossings of breakpoint %d.",
		  count, bptnum);
	breakpoints_changed ();
	return;
      }

  error ("No breakpoint number %d.", bptnum);
}

/* Clear the ignore counts of all breakpoints.  */
void
breakpoint_clear_ignore_counts ()
{
  struct breakpoint *b;

  ALL_BREAKPOINTS (b)
    b->ignore_count = 0;
}

/* Command to set ignore-count of breakpoint N to COUNT.  */

static void
ignore_command (args, from_tty)
     char *args;
     int from_tty;
{
  char *p = args;
  register int num;

  if (p == 0)
    error_no_arg ("a breakpoint number");
  
  num = get_number (&p);

  if (*p == 0)
    error ("Second argument (specified ignore-count) is missing.");

  set_ignore_count (num,
		    longest_to_int (value_as_long (parse_and_eval (p))),
		    from_tty);
  printf_filtered ("\n");
  breakpoints_changed ();
}

/* Call FUNCTION on each of the breakpoints
   whose numbers are given in ARGS.  */

static void
map_breakpoint_numbers (args, function)
     char *args;
     void (*function) PARAMS ((struct breakpoint *));
{
  register char *p = args;
  char *p1;
  register int num;
  register struct breakpoint *b;

  if (p == 0)
    error_no_arg ("one or more breakpoint numbers");

  while (*p)
    {
      p1 = p;
      
      num = get_number (&p1);

      ALL_BREAKPOINTS (b)
	if (b->number == num)
	  {
	    struct breakpoint *related_breakpoint = b->related_breakpoint;
	    function (b);
	    if (related_breakpoint)
	      function (related_breakpoint);
	    goto win;
	  }
      printf_unfiltered ("No breakpoint number %d.\n", num);
    win:
      p = p1;
    }
}

void
disable_breakpoint (bpt)
     struct breakpoint *bpt;
{
  /* Never disable a watchpoint scope breakpoint; we want to
     hit them when we leave scope so we can delete both the
     watchpoint and its scope breakpoint at that time.  */
  if (bpt->type == bp_watchpoint_scope)
    return;

  bpt->enable = disabled;

  check_duplicates (bpt->address);

  if (modify_breakpoint_hook)
    modify_breakpoint_hook (bpt);
}

/* ARGSUSED */
static void
disable_command (args, from_tty)
     char *args;
     int from_tty;
{
  register struct breakpoint *bpt;
  if (args == 0)
    ALL_BREAKPOINTS (bpt)
      switch (bpt->type)
	{
	case bp_breakpoint:
        case bp_hardware_breakpoint:
        case bp_watchpoint:
        case bp_hardware_watchpoint:
        case bp_read_watchpoint:
        case bp_access_watchpoint:
	  disable_breakpoint (bpt);
	default:
	  continue;
	}
  else
    map_breakpoint_numbers (args, disable_breakpoint);
}

static void
do_enable_breakpoint (bpt, disposition)
     struct breakpoint *bpt;
     enum bpdisp disposition;
{
  struct frame_info *save_selected_frame = NULL;
  int save_selected_frame_level = -1;
  int target_resources_ok, other_type_used;
  struct value *mark;

  if (bpt->type == bp_hardware_breakpoint)
    {
      int i;
      i = hw_breakpoint_used_count();
      target_resources_ok = TARGET_CAN_USE_HARDWARE_WATCHPOINT(
		bp_hardware_breakpoint, i+1, 0);
      if (target_resources_ok == 0)
        error ("No hardware breakpoint support in the target.");
      else if (target_resources_ok < 0)
        error ("Hardware breakpoints used exceeds limit.");
    }

  bpt->enable = enabled;
  bpt->disposition = disposition;
  check_duplicates (bpt->address);
  breakpoints_changed ();

  if (bpt->type == bp_watchpoint || bpt->type == bp_hardware_watchpoint ||
      bpt->type == bp_read_watchpoint || bpt->type == bp_access_watchpoint)
    {
      if (bpt->exp_valid_block != NULL)
	{
	  struct frame_info *fr =
	    find_frame_addr_in_frame_chain (bpt->watchpoint_frame);
	  if (fr == NULL)
	    {
	      printf_filtered ("\
Cannot enable watchpoint %d because the block in which its expression\n\
is valid is not currently in scope.\n", bpt->number);
	      bpt->enable = disabled;
	      return;
	    }

	  save_selected_frame = selected_frame;
	  save_selected_frame_level = selected_frame_level;
	  select_frame (fr, -1);
	}

      value_free (bpt->val);
      mark = value_mark ();
      bpt->val = evaluate_expression (bpt->exp);
      release_value (bpt->val);
      if (VALUE_LAZY (bpt->val))
	value_fetch_lazy (bpt->val);

      if (bpt->type == bp_hardware_watchpoint ||
           bpt->type == bp_read_watchpoint ||
           bpt->type == bp_access_watchpoint)
      {
        int i = hw_watchpoint_used_count (bpt->type, &other_type_used);
        int mem_cnt = can_use_hardware_watchpoint (bpt->val);

        target_resources_ok = TARGET_CAN_USE_HARDWARE_WATCHPOINT(
                bpt->type, i + mem_cnt, other_type_used);
        /* we can consider of type is bp_hardware_watchpoint, convert to 
	   bp_watchpoint in the following condition */
        if (target_resources_ok < 0)
	  {
             printf_filtered("\
Cannot enable watchpoint %d because target watch resources\n\
have been allocated for other watchpoints.\n", bpt->number);
	     bpt->enable = disabled;
	     value_free_to_mark (mark);
	     return;
          }
      }

      if (save_selected_frame_level >= 0)
	select_frame (save_selected_frame, save_selected_frame_level);
      value_free_to_mark (mark);
    }
  if (modify_breakpoint_hook)
    modify_breakpoint_hook (bpt);
}

void
enable_breakpoint (bpt)
     struct breakpoint *bpt;
{
  do_enable_breakpoint (bpt, donttouch);
}

/* The enable command enables the specified breakpoints (or all defined
   breakpoints) so they once again become (or continue to be) effective
   in stopping the inferior. */

/* ARGSUSED */
static void
enable_command (args, from_tty)
     char *args;
     int from_tty;
{
  register struct breakpoint *bpt;
  if (args == 0)
    ALL_BREAKPOINTS (bpt)
      switch (bpt->type)
	{
	case bp_breakpoint:
	case bp_hardware_breakpoint:
	case bp_watchpoint:
	case bp_hardware_watchpoint:
	case bp_read_watchpoint:
	case bp_access_watchpoint:
	  enable_breakpoint (bpt);
	default:
	  continue;
	}
  else
    map_breakpoint_numbers (args, enable_breakpoint);
}

static void
enable_once_breakpoint (bpt)
     struct breakpoint *bpt;
{
  do_enable_breakpoint (bpt, disable);
}

/* ARGSUSED */
static void
enable_once_command (args, from_tty)
     char *args;
     int from_tty;
{
  map_breakpoint_numbers (args, enable_once_breakpoint);
}

static void
enable_delete_breakpoint (bpt)
     struct breakpoint *bpt;
{
  do_enable_breakpoint (bpt, del);
}

/* ARGSUSED */
static void
enable_delete_command (args, from_tty)
     char *args;
     int from_tty;
{
  map_breakpoint_numbers (args, enable_delete_breakpoint);
}

/* Use default_breakpoint_'s, or nothing if they aren't valid.  */

struct symtabs_and_lines
decode_line_spec_1 (string, funfirstline)
     char *string;
     int funfirstline;
{
  struct symtabs_and_lines sals;
  if (string == 0)
    error ("Empty line specification.");
  if (default_breakpoint_valid)
    sals = decode_line_1 (&string, funfirstline,
			  default_breakpoint_symtab, default_breakpoint_line,
			  (char ***)NULL);
  else
    sals = decode_line_1 (&string, funfirstline,
			  (struct symtab *)NULL, 0, (char ***)NULL);
  if (*string)
    error ("Junk at end of line specification: %s", string);
  return sals;
}

void
_initialize_breakpoint ()
{
  breakpoint_chain = 0;
  /* Don't bother to call set_breakpoint_count.  $bpnum isn't useful
     before a breakpoint is set.  */
  breakpoint_count = 0;

  add_com ("ignore", class_breakpoint, ignore_command,
	   "Set ignore-count of breakpoint number N to COUNT.\n\
Usage is `ignore N COUNT'.");

  add_com ("commands", class_breakpoint, commands_command,
	   "Set commands to be executed when a breakpoint is hit.\n\
Give breakpoint number as argument after \"commands\".\n\
With no argument, the targeted breakpoint is the last one set.\n\
The commands themselves follow starting on the next line.\n\
Type a line containing \"end\" to indicate the end of them.\n\
Give \"silent\" as the first line to make the breakpoint silent;\n\
then no output is printed when it is hit, except what the commands print.");

  add_com ("condition", class_breakpoint, condition_command,
	   "Specify breakpoint number N to break only if COND is true.\n\
Usage is `condition N COND', where N is an integer and COND is an\n\
expression to be evaluated whenever breakpoint N is reached.  ");

  add_com ("tbreak", class_breakpoint, tbreak_command,
	   "Set a temporary breakpoint.  Args like \"break\" command.\n\
Like \"break\" except the breakpoint is only temporary,\n\
so it will be deleted when hit.  Equivalent to \"break\" followed\n\
by using \"enable delete\" on the breakpoint number.");

  add_com ("hbreak", class_breakpoint, hbreak_command,
	   "Set a hardware assisted  breakpoint. Args like \"break\" command.\n\
Like \"break\" except the breakpoint requires hardware support,\n\
some target hardware may not have this support.");

  add_com ("thbreak", class_breakpoint, thbreak_command,
	   "Set a temporary hardware assisted breakpoint. Args like \"break\" command.\n\
Like \"hbreak\" except the breakpoint is only temporary,\n\
so it will be deleted when hit.");

  add_prefix_cmd ("enable", class_breakpoint, enable_command,
		  "Enable some breakpoints.\n\
Give breakpoint numbers (separated by spaces) as arguments.\n\
With no subcommand, breakpoints are enabled until you command otherwise.\n\
This is used to cancel the effect of the \"disable\" command.\n\
With a subcommand you can enable temporarily.",
		  &enablelist, "enable ", 1, &cmdlist);

  add_abbrev_prefix_cmd ("breakpoints", class_breakpoint, enable_command,
		  "Enable some breakpoints.\n\
Give breakpoint numbers (separated by spaces) as arguments.\n\
This is used to cancel the effect of the \"disable\" command.\n\
May be abbreviated to simply \"enable\".\n",
		  &enablebreaklist, "enable breakpoints ", 1, &enablelist);

  add_cmd ("once", no_class, enable_once_command,
	   "Enable breakpoints for one hit.  Give breakpoint numbers.\n\
If a breakpoint is hit while enabled in this fashion, it becomes disabled.",
	   &enablebreaklist);

  add_cmd ("delete", no_class, enable_delete_command,
	   "Enable breakpoints and delete when hit.  Give breakpoint numbers.\n\
If a breakpoint is hit while enabled in this fashion, it is deleted.",
	   &enablebreaklist);

  add_cmd ("delete", no_class, enable_delete_command,
	   "Enable breakpoints and delete when hit.  Give breakpoint numbers.\n\
If a breakpoint is hit while enabled in this fashion, it is deleted.",
	   &enablelist);

  add_cmd ("once", no_class, enable_once_command,
	   "Enable breakpoints for one hit.  Give breakpoint numbers.\n\
If a breakpoint is hit while enabled in this fashion, it becomes disabled.",
	   &enablelist);

  add_prefix_cmd ("disable", class_breakpoint, disable_command,
	   "Disable some breakpoints.\n\
Arguments are breakpoint numbers with spaces in between.\n\
To disable all breakpoints, give no argument.\n\
A disabled breakpoint is not forgotten, but has no effect until reenabled.",
		  &disablelist, "disable ", 1, &cmdlist);
  add_com_alias ("dis", "disable", class_breakpoint, 1);
  add_com_alias ("disa", "disable", class_breakpoint, 1);

  add_cmd ("breakpoints", class_alias, disable_command,
	   "Disable some breakpoints.\n\
Arguments are breakpoint numbers with spaces in between.\n\
To disable all breakpoints, give no argument.\n\
A disabled breakpoint is not forgotten, but has no effect until reenabled.\n\
This command may be abbreviated \"disable\".",
	   &disablelist);

  add_prefix_cmd ("delete", class_breakpoint, delete_command,
	   "Delete some breakpoints or auto-display expressions.\n\
Arguments are breakpoint numbers with spaces in between.\n\
To delete all breakpoints, give no argument.\n\
\n\
Also a prefix command for deletion of other GDB objects.\n\
The \"unset\" command is also an alias for \"delete\".",
		  &deletelist, "delete ", 1, &cmdlist);
  add_com_alias ("d", "delete", class_breakpoint, 1);

  add_cmd ("breakpoints", class_alias, delete_command,
	   "Delete some breakpoints or auto-display expressions.\n\
Arguments are breakpoint numbers with spaces in between.\n\
To delete all breakpoints, give no argument.\n\
This command may be abbreviated \"delete\".",
	   &deletelist);

  add_com ("clear", class_breakpoint, clear_command,
	   concat ("Clear breakpoint at specified line or function.\n\
Argument may be line number, function name, or \"*\" and an address.\n\
If line number is specified, all breakpoints in that line are cleared.\n\
If function is specified, breakpoints at beginning of function are cleared.\n\
If an address is specified, breakpoints at that address are cleared.\n\n",
"With no argument, clears all breakpoints in the line that the selected frame\n\
is executing in.\n\
\n\
See also the \"delete\" command which clears breakpoints by number.", NULL));

  add_com ("break", class_breakpoint, break_command,
	   concat ("Set breakpoint at specified line or function.\n\
Argument may be line number, function name, or \"*\" and an address.\n\
If line number is specified, break at start of code for that line.\n\
If function is specified, break at start of code for that function.\n\
If an address is specified, break at that exact address.\n",
"With no arg, uses current execution address of selected stack frame.\n\
This is useful for breaking on return to a stack frame.\n\
\n\
Multiple breakpoints at one place are permitted, and useful if conditional.\n\
\n\
Do \"help breakpoints\" for info on other commands dealing with breakpoints.", NULL));
  add_com_alias ("b", "break", class_run, 1);
  add_com_alias ("br", "break", class_run, 1);
  add_com_alias ("bre", "break", class_run, 1);
  add_com_alias ("brea", "break", class_run, 1);

  add_info ("breakpoints", breakpoints_info,
	    concat ("Status of user-settable breakpoints, or breakpoint number NUMBER.\n\
The \"Type\" column indicates one of:\n\
\tbreakpoint     - normal breakpoint\n\
\twatchpoint     - watchpoint\n\
The \"Disp\" column contains one of \"keep\", \"del\", or \"dis\" to indicate\n\
the disposition of the breakpoint after it gets hit.  \"dis\" means that the\n\
breakpoint will be disabled.  The \"Address\" and \"What\" columns indicate the\n\
address and file/line number respectively.\n\n",
"Convenience variable \"$_\" and default examine address for \"x\"\n\
are set to the address of the last breakpoint listed.\n\n\
Convenience variable \"$bpnum\" contains the number of the last\n\
breakpoint set.", NULL));

#if MAINTENANCE_CMDS

  add_cmd ("breakpoints", class_maintenance, maintenance_info_breakpoints,
	    concat ("Status of all breakpoints, or breakpoint number NUMBER.\n\
The \"Type\" column indicates one of:\n\
\tbreakpoint     - normal breakpoint\n\
\twatchpoint     - watchpoint\n\
\tlongjmp        - internal breakpoint used to step through longjmp()\n\
\tlongjmp resume - internal breakpoint at the target of longjmp()\n\
\tuntil          - internal breakpoint used by the \"until\" command\n\
\tfinish         - internal breakpoint used by the \"finish\" command\n",
"The \"Disp\" column contains one of \"keep\", \"del\", or \"dis\" to indicate\n\
the disposition of the breakpoint after it gets hit.  \"dis\" means that the\n\
breakpoint will be disabled.  The \"Address\" and \"What\" columns indicate the\n\
address and file/line number respectively.\n\n",
"Convenience variable \"$_\" and default examine address for \"x\"\n\
are set to the address of the last breakpoint listed.\n\n\
Convenience variable \"$bpnum\" contains the number of the last\n\
breakpoint set.", NULL),
	   &maintenanceinfolist);

#endif	/* MAINTENANCE_CMDS */

  add_com ("catch", class_breakpoint, catch_command,
         "Set breakpoints to catch exceptions that are raised.\n\
Argument may be a single exception to catch, multiple exceptions\n\
to catch, or the default exception \"default\".  If no arguments\n\
are given, breakpoints are set at all exception handlers catch clauses\n\
within the current scope.\n\
\n\
A condition specified for the catch applies to all breakpoints set\n\
with this command\n\
\n\
Do \"help breakpoints\" for info on other commands dealing with breakpoints.");

  add_com ("watch", class_breakpoint, watch_command,
	   "Set a watchpoint for an expression.\n\
A watchpoint stops execution of your program whenever the value of\n\
an expression changes.");

  add_com ("rwatch", class_breakpoint, rwatch_command,
	   "Set a read watchpoint for an expression.\n\
A watchpoint stops execution of your program whenever the value of\n\
an expression is read.");

  add_com ("awatch", class_breakpoint, awatch_command,
	   "Set a watchpoint for an expression.\n\
A watchpoint stops execution of your program whenever the value of\n\
an expression is either read or written.");

  add_info ("watchpoints", breakpoints_info,
	    "Synonym for ``info breakpoints''.");

}
