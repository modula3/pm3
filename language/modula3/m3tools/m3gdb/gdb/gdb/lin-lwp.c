/* Multi-threaded debugging support for Linux (LWP layer).
   Copyright 2000, 2001 Free Software Foundation, Inc.

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

#include "gdb_assert.h"
#include <errno.h>
#include <signal.h>
#include <sys/ptrace.h>
#include "gdb_wait.h"

#include "gdbthread.h"
#include "inferior.h"
#include "target.h"
#include "regcache.h"
#include "gdbcmd.h"

static int debug_lin_lwp;
extern const char *strsignal (int sig);

/* On Linux there are no real LWP's.  The closest thing to LWP's are
   processes sharing the same VM space.  A multi-threaded process is
   basically a group of such processes.  However, such a grouping is
   almost entirely a user-space issue; the kernel doesn't enforce such
   a grouping at all (this might change in the future).  In general,
   we'll rely on the threads library (i.e. the LinuxThreads library)
   to provide such a grouping.

   It is perfectly well possible to write a multi-threaded application
   without the assistance of a threads library, by using the clone
   system call directly.  This module should be able to give some
   rudimentary support for debugging such applications if developers
   specify the CLONE_PTRACE flag in the clone system call, and are
   using Linux 2.4 or above.

   Note that there are some peculiarities in Linux that affect this
   code:

   - In general one should specify the __WCLONE flag to waitpid in
     order to make it report events for any of the cloned processes
     (and leave it out for the initial process).  However, if a cloned
     process has exited the exit status is only reported if the
     __WCLONE flag is absent.  Linux 2.4 has a __WALL flag, but we
     cannot use it since GDB must work on older systems too.

   - When a traced, cloned process exits and is waited for by the
     debugger, the kernel reassigns it to the original parent and
     keeps it around as a "zombie".  Somehow, the LinuxThreads library
     doesn't notice this, which leads to the "zombie problem": When
     debugged a multi-threaded process that spawns a lot of threads
     will run out of processes, even if the threads exit, because the
     "zombies" stay around.  */

/* Structure describing a LWP.  */
struct lwp_info
{
  /* The process id of the LWP.  This is a combination of the LWP id
     and overall process id.  */
  ptid_t ptid;

  /* Non-zero if we sent this LWP a SIGSTOP (but the LWP didn't report
     it back yet).  */
  int signalled;

  /* Non-zero if this LWP is stopped.  */
  int stopped;

  /* Non-zero if this LWP will be/has been resumed.  Note that an LWP
     can be marked both as stopped and resumed at the same time.  This
     happens if we try to resume an LWP that has a wait status
     pending.  We shouldn't let the LWP run until that wait status has
     been processed, but we should not report that wait status if GDB
     didn't try to let the LWP run.  */
  int resumed;

  /* If non-zero, a pending wait status.  */
  int status;

  /* Non-zero if we were stepping this LWP.  */
  int step;

  /* Next LWP in list.  */
  struct lwp_info *next;
};

/* List of known LWPs.  */
static struct lwp_info *lwp_list;

/* Number of LWPs in the list.  */
static int num_lwps;

/* Non-zero if we're running in "threaded" mode.  */
static int threaded;


#define GET_LWP(ptid)		ptid_get_lwp (ptid)
#define GET_PID(ptid)		ptid_get_pid (ptid)
#define is_lwp(ptid)		(GET_LWP (ptid) != 0)
#define BUILD_LWP(lwp, pid)	ptid_build (pid, lwp, 0)

#define is_cloned(pid)	(GET_LWP (pid) != GET_PID (pid))

/* If the last reported event was a SIGTRAP, this variable is set to
   the process id of the LWP/thread that got it.  */
ptid_t trap_ptid;


/* This module's target-specific operations.  */
static struct target_ops lin_lwp_ops;

/* The standard child operations.  */
extern struct target_ops child_ops;

/* Since we cannot wait (in lin_lwp_wait) for the initial process and
   any cloned processes with a single call to waitpid, we have to use
   the WNOHANG flag and call waitpid in a loop.  To optimize
   things a bit we use `sigsuspend' to wake us up when a process has
   something to report (it will send us a SIGCHLD if it has).  To make
   this work we have to juggle with the signal mask.  We save the
   original signal mask such that we can restore it before creating a
   new process in order to avoid blocking certain signals in the
   inferior.  We then block SIGCHLD during the waitpid/sigsuspend
   loop.  */

/* Original signal mask.  */
static sigset_t normal_mask;

/* Signal mask for use with sigsuspend in lin_lwp_wait, initialized in
   _initialize_lin_lwp.  */
static sigset_t suspend_mask;

/* Signals to block to make that sigsuspend work.  */
static sigset_t blocked_mask;


/* Prototypes for local functions.  */
static int stop_wait_callback (struct lwp_info *lp, void *data);

/* Convert wait status STATUS to a string.  Used for printing debug
   messages only.  */

static char *
status_to_str (int status)
{
  static char buf[64];

  if (WIFSTOPPED (status))
    snprintf (buf, sizeof (buf), "%s (stopped)",
	      strsignal (WSTOPSIG (status)));
  else if (WIFSIGNALED (status))
    snprintf (buf, sizeof (buf), "%s (terminated)",
	      strsignal (WSTOPSIG (status)));
  else
    snprintf (buf, sizeof (buf), "%d (exited)",
	      WEXITSTATUS (status));

  return buf;
}

/* Initialize the list of LWPs.  Note that this module, contrary to
   what GDB's generic threads layer does for its thread list,
   re-initializes the LWP lists whenever we mourn or detach (which
   doesn't involve mourning) the inferior.  */

static void
init_lwp_list (void)
{
  struct lwp_info *lp, *lpnext;

  for (lp = lwp_list; lp; lp = lpnext)
    {
      lpnext = lp->next;
      xfree (lp);
    }

  lwp_list = NULL;
  num_lwps = 0;
  threaded = 0;
}

/* Add the LWP specified by PID to the list.  If this causes the
   number of LWPs to become larger than one, go into "threaded" mode.
   Return a pointer to the structure describing the new LWP.  */

static struct lwp_info *
add_lwp (ptid_t ptid)
{
  struct lwp_info *lp;

  gdb_assert (is_lwp (ptid));

  lp = (struct lwp_info *) xmalloc (sizeof (struct lwp_info));

  memset (lp, 0, sizeof (struct lwp_info));

  lp->ptid = ptid;

  lp->next = lwp_list;
  lwp_list = lp;
  if (++num_lwps > 1)
    threaded = 1;

  return lp;
}

/* Remove the LWP specified by PID from the list.  */

static void
delete_lwp (ptid_t ptid)
{
  struct lwp_info *lp, *lpprev;

  lpprev = NULL;

  for (lp = lwp_list; lp; lpprev = lp, lp = lp->next)
    if (ptid_equal (lp->ptid, ptid))
      break;

  if (!lp)
    return;

  /* We don't go back to "non-threaded" mode if the number of threads
     becomes less than two.  */
  num_lwps--;

  if (lpprev)
    lpprev->next = lp->next;
  else
    lwp_list = lp->next;

  xfree (lp);
}

/* Return a pointer to the structure describing the LWP corresponding
   to PID.  If no corresponding LWP could be found, return NULL.  */

static struct lwp_info *
find_lwp_pid (ptid_t ptid)
{
  struct lwp_info *lp;
  int lwp;

  if (is_lwp (ptid))
    lwp = GET_LWP (ptid);
  else
    lwp = GET_PID (ptid);

  for (lp = lwp_list; lp; lp = lp->next)
    if (lwp == GET_LWP (lp->ptid))
      return lp;

  return NULL;
}

/* Call CALLBACK with its second argument set to DATA for every LWP in
   the list.  If CALLBACK returns 1 for a particular LWP, return a
   pointer to the structure describing that LWP immediately.
   Otherwise return NULL.  */

struct lwp_info *
iterate_over_lwps (int (*callback) (struct lwp_info *, void *), void *data)
{
  struct lwp_info *lp, *lpnext;

  for (lp = lwp_list; lp; lp = lpnext)
    {
      lpnext = lp->next;
      if ((*callback) (lp, data))
	return lp;
    }

  return NULL;
}


/* Implementation of the PREPARE_TO_PROCEED hook for the Linux LWP
   layer.

   Note that this implementation is potentially redundant now that
   default_prepare_to_proceed() has been added.

   FIXME This may not support switching threads after Ctrl-C
   correctly. The default implementation does support this. */

int
lin_lwp_prepare_to_proceed (void)
{
  if (! ptid_equal (trap_ptid, null_ptid)
      && ! ptid_equal (inferior_ptid, trap_ptid))
    {
      /* Switched over from TRAP_PID.  */
      CORE_ADDR stop_pc = read_pc ();
      CORE_ADDR trap_pc;

      /* Avoid switching where it wouldn't do any good, i.e. if both
         threads are at the same breakpoint.  */
      trap_pc = read_pc_pid (trap_ptid);
      if (trap_pc != stop_pc && breakpoint_here_p (trap_pc))
	{
	  /* User hasn't deleted the breakpoint.  Return non-zero, and
             switch back to TRAP_PID.  */
	  inferior_ptid = trap_ptid;

	  /* FIXME: Is this stuff really necessary?  */
	  flush_cached_frames ();
	  registers_changed ();

	  return 1;
	}
    }

  return 0;
}


#if 0
static void
lin_lwp_open (char *args, int from_tty)
{
  push_target (&lin_lwp_ops);
}
#endif

/* Attach to the LWP specified by PID.  If VERBOSE is non-zero, print
   a message telling the user that a new LWP has been added to the
   process.  */

void
lin_lwp_attach_lwp (ptid_t ptid, int verbose)
{
  struct lwp_info *lp;

  gdb_assert (is_lwp (ptid));

  if (verbose)
    printf_filtered ("[New %s]\n", target_pid_to_str (ptid));

  /* We assume that we're already tracing the initial process.  */
  if (is_cloned (ptid) && ptrace (PTRACE_ATTACH, GET_LWP (ptid), 0, 0) < 0)
    error ("Can't attach %s: %s", target_pid_to_str (ptid), strerror (errno));

  lp = find_lwp_pid (ptid);
  if (lp == NULL)
    lp = add_lwp (ptid);

  if (is_cloned (ptid))
    {
      lp->signalled = 1;
      stop_wait_callback (lp, NULL);
    }
}

static void
lin_lwp_attach (char *args, int from_tty)
{
  struct lwp_info *lp;

  /* FIXME: We should probably accept a list of process id's, and
     attach all of them.  */
  child_ops.to_attach (args, from_tty);

  /* Add the initial process as the first LWP to the list.  */
  lp = add_lwp (BUILD_LWP (PIDGET (inferior_ptid), PIDGET (inferior_ptid)));

  /* Make sure the initial process is stopped.  The user-level threads
     layer might want to poke around in the inferior, and that won't
     work if things haven't stabilized yet.  */
  lp->signalled = 1;
  stop_wait_callback (lp, NULL);
  gdb_assert (lp->status == 0);

  /* Fake the SIGSTOP that core GDB expects.  */
  lp->status = W_STOPCODE (SIGSTOP);
  lp->resumed = 1;
}

static int
detach_callback (struct lwp_info *lp, void *data)
{
  gdb_assert (lp->status == 0 || WIFSTOPPED (lp->status));

  if (debug_lin_lwp && lp->status)
    fprintf_unfiltered (gdb_stdlog, "Pending %s for LWP %ld on detach.\n",
			strsignal (WSTOPSIG (lp->status)), GET_LWP (lp->ptid));

  while (lp->signalled && lp->stopped)
    {
      if (ptrace (PTRACE_CONT, GET_LWP (lp->ptid), 0,
		  WSTOPSIG (lp->status)) < 0)
	error ("Can't continue %s: %s", target_pid_to_str (lp->ptid),
	       strerror (errno));

      lp->stopped = 0;
      lp->signalled = 0;
      lp->status = 0;
      stop_wait_callback (lp, NULL);

      gdb_assert (lp->status == 0 || WIFSTOPPED (lp->status));
    }

  if (is_cloned (lp->ptid))
    {
      if (ptrace (PTRACE_DETACH, GET_LWP (lp->ptid), 0,
		  WSTOPSIG (lp->status)) < 0)
	error ("Can't detach %s: %s", target_pid_to_str (lp->ptid),
	       strerror (errno));

      delete_lwp (lp->ptid);
    }

  return 0;
}

static void
lin_lwp_detach (char *args, int from_tty)
{
  iterate_over_lwps (detach_callback, NULL);

  /* Only the initial (uncloned) process should be left right now.  */
  gdb_assert (num_lwps == 1);

  trap_ptid = null_ptid;

  /* Destroy LWP info; it's no longer valid.  */
  init_lwp_list ();

  /* Restore the original signal mask.  */
  sigprocmask (SIG_SETMASK, &normal_mask, NULL);
  sigemptyset (&blocked_mask);

  inferior_ptid = pid_to_ptid (GET_PID (inferior_ptid));
  child_ops.to_detach (args, from_tty);
}


struct private_thread_info
{
  int lwpid;
};

/* Return non-zero if TP corresponds to the LWP specified by DATA
   (which is assumed to be a pointer to a `struct lwp_info'.  */

static int
find_lwp_callback (struct thread_info *tp, void *data)
{
  struct lwp_info *lp = data;

  if (tp->private->lwpid == GET_LWP (lp->ptid))
    return 1;

  return 0;
}

/* Resume LP.  */

static int
resume_callback (struct lwp_info *lp, void *data)
{
  if (lp->stopped && lp->status == 0)
    {
      struct thread_info *tp;

#if 0
      /* FIXME: kettenis/2000-08-26: This should really be handled
         properly by core GDB.  */

      tp = find_thread_pid (lp->ptid);
      if (tp == NULL)
	tp = iterate_over_threads (find_lwp_callback, lp);
      gdb_assert (tp);

      /* If we were previously stepping the thread, and now continue
         the thread we must invalidate the stepping range.  However,
         if there is a step_resume breakpoint for this thread, we must
         preserve the stepping range to make it possible to continue
         stepping once we hit it.  */
      if (tp->step_range_end && tp->step_resume_breakpoint == NULL)
	{
	  gdb_assert (lp->step);
	  tp->step_range_start = tp->step_range_end = 0;
	}
#endif

      child_resume (pid_to_ptid (GET_LWP (lp->ptid)), 0, TARGET_SIGNAL_0);
      lp->stopped = 0;
      lp->step = 0;
    }

  return 0;
}

static int
resume_clear_callback (struct lwp_info *lp, void *data)
{
  lp->resumed = 0;
  return 0;
}

static int
resume_set_callback (struct lwp_info *lp, void *data)
{
  lp->resumed = 1;
  return 0;
}

static void
lin_lwp_resume (ptid_t ptid, int step, enum target_signal signo)
{
  struct lwp_info *lp;
  int resume_all;

  /* Apparently the interpretation of PID is dependent on STEP: If
     STEP is non-zero, a specific PID means `step only this process
     id'.  But if STEP is zero, then PID means `continue *all*
     processes, but give the signal only to this one'.  */
  resume_all = (PIDGET (ptid) == -1) || !step;

  if (resume_all)
    iterate_over_lwps (resume_set_callback, NULL);
  else
    iterate_over_lwps (resume_clear_callback, NULL);

  /* If PID is -1, it's the current inferior that should be
     handled specially.  */
  if (PIDGET (ptid) == -1)
    ptid = inferior_ptid;

  lp = find_lwp_pid (ptid);
  if (lp)
    {
      ptid = pid_to_ptid (GET_LWP (lp->ptid));

      /* Remember if we're stepping.  */
      lp->step = step;

      /* Mark this LWP as resumed.  */
      lp->resumed = 1;

      /* If we have a pending wait status for this thread, there is no
         point in resuming the process.  */
      if (lp->status)
	{
	  /* FIXME: What should we do if we are supposed to continue
             this thread with a signal?  */
	  gdb_assert (signo == TARGET_SIGNAL_0);
	  return;
	}

      /* Mark LWP as not stopped to prevent it from being continued by
	 resume_callback.  */
      lp->stopped = 0;
    }

  if (resume_all)
    iterate_over_lwps (resume_callback, NULL);

  child_resume (ptid, step, signo);
}


/* Send a SIGSTOP to LP.  */

static int
stop_callback (struct lwp_info *lp, void *data)
{
  if (! lp->stopped && ! lp->signalled)
    {
      int ret;

      ret = kill (GET_LWP (lp->ptid), SIGSTOP);
      gdb_assert (ret == 0);

      lp->signalled = 1;
      gdb_assert (lp->status == 0);
    }

  return 0;
}

/* Wait until LP is stopped.  If DATA is non-null it is interpreted as
   a pointer to a set of signals to be flushed immediately.  */

static int
stop_wait_callback (struct lwp_info *lp, void *data)
{
  sigset_t *flush_mask = data;

  if (! lp->stopped && lp->signalled)
    {
      pid_t pid;
      int status;

      gdb_assert (lp->status == 0);

      pid = waitpid (GET_LWP (lp->ptid), &status,
		     is_cloned (lp->ptid) ? __WCLONE : 0);
      if (pid == -1 && errno == ECHILD)
	/* OK, the proccess has disappeared.  We'll catch the actual
	   exit event in lin_lwp_wait.  */
	return 0;

      gdb_assert (pid == GET_LWP (lp->ptid));

      if (WIFEXITED (status) || WIFSIGNALED (status))
	{
	  gdb_assert (num_lwps > 1);

	  if (in_thread_list (lp->ptid))
	    {
	      /* Core GDB cannot deal with us deleting the current
		 thread.  */
	      if (!ptid_equal (lp->ptid, inferior_ptid))
		delete_thread (lp->ptid);
	      printf_unfiltered ("[%s exited]\n",
				 target_pid_to_str (lp->ptid));
	    }
	  if (debug_lin_lwp)
	    fprintf_unfiltered (gdb_stdlog, 
				"%s exited.\n", target_pid_to_str (lp->ptid));

	  delete_lwp (lp->ptid);
	  return 0;
	}

      gdb_assert (WIFSTOPPED (status));

      /* Ignore any signals in FLUSH_MASK.  */
      if (flush_mask && sigismember (flush_mask, WSTOPSIG (status)))
	{
	  ptrace (PTRACE_CONT, GET_LWP (lp->ptid), 0, 0);
	  return stop_wait_callback (lp, flush_mask);
	}

      if (WSTOPSIG (status) != SIGSTOP)
	{
	  if (WSTOPSIG (status) == SIGTRAP)
	    {
	      /* If a LWP other than the LWP that we're reporting an
                 event for has hit a GDB breakpoint (as opposed to
                 some random trap signal), then just arrange for it to
                 hit it again later.  We don't keep the SIGTRAP status
                 and don't forward the SIGTRAP signal to the LWP.  We
                 will handle the current event, eventually we will
                 resume all LWPs, and this one will get its breakpoint
                 trap again.

		 If we do not do this, then we run the risk that the
		 user will delete or disable the breakpoint, but the
		 thread will have already tripped on it.  */

	      /* Now resume this LWP and get the SIGSTOP event. */
	      ptrace (PTRACE_CONT, GET_LWP (lp->ptid), 0, 0);
	      if (debug_lin_lwp)
		{
		  fprintf_unfiltered (gdb_stderr, 
				      "SWC: Candidate SIGTRAP event in %ld\n",
				      GET_LWP (lp->ptid));
		}
	      /* Hold the SIGTRAP for handling by lin_lwp_wait. */
	      stop_wait_callback (lp, data);
	      /* If there's another event, throw it back into the queue. */
	      if (lp->status)
		kill (GET_LWP (lp->ptid), WSTOPSIG (lp->status));
	      /* Save the sigtrap event. */
	      lp->status = status;
	      return 0;
	    }
	  else
	    {
	      /* The thread was stopped with a signal other than
		 SIGSTOP, and didn't accidentally trip a breakpoint. */

	      if (debug_lin_lwp)
		{
		  fprintf_unfiltered (gdb_stderr, 
				      "SWC: Pending event %d in %ld\n",
				      WSTOPSIG (status), GET_LWP (lp->ptid));
		}
	      /* Now resume this LWP and get the SIGSTOP event. */
	      ptrace (PTRACE_CONT, GET_LWP (lp->ptid), 0, 0);

	      /* Hold this event/waitstatus while we check to see if
		 there are any more (we still want to get that SIGSTOP). */
	      stop_wait_callback (lp, data);
	      /* If the lp->status field is still empty, use it to hold
		 this event.  If not, then this event must be returned
		 to the event queue of the LWP.  */
	      if (lp->status == 0)
		lp->status = status;
	      else
		kill (GET_LWP (lp->ptid), WSTOPSIG (status));
	      return 0;
	    }
	}
      else
	{
	  /* We caught the SIGSTOP that we intended to catch, so
             there's no SIGSTOP pending.  */
	  lp->stopped = 1;
	  lp->signalled = 0;
	}
    }

  return 0;
}

/* Return non-zero if LP has a wait status pending.  */

static int
status_callback (struct lwp_info *lp, void *data)
{
  /* Only report a pending wait status if we pretend that this has
     indeed been resumed.  */
  return (lp->status != 0 && lp->resumed);
}

/* Return non-zero if LP isn't stopped.  */

static int
running_callback (struct lwp_info *lp, void *data)
{
  return (lp->stopped == 0);
}

/* Count the LWP's that have had events.  */

static int
count_events_callback (struct lwp_info *lp, void *data)
{
  int *count = data;

  gdb_assert (count != NULL);

  /* Count only LWPs that have a SIGTRAP event pending.  */
  if (lp->status != 0
      && WIFSTOPPED (lp->status) && WSTOPSIG (lp->status) == SIGTRAP)
    (*count)++;

  return 0;
}

/* Select the LWP (if any) that is currently being single-stepped.  */

static int
select_singlestep_lwp_callback (struct lwp_info *lp, void *data)
{
  if (lp->step && lp->status != 0)
    return 1;
  else
    return 0;
}

/* Select the Nth LWP that has had a SIGTRAP event.  */

static int
select_event_lwp_callback (struct lwp_info *lp, void *data)
{
  int *selector = data;

  gdb_assert (selector != NULL);

  /* Select only LWPs that have a SIGTRAP event pending. */
  if (lp->status != 0
      && WIFSTOPPED (lp->status) && WSTOPSIG (lp->status) == SIGTRAP)
    if ((*selector)-- == 0)
      return 1;

  return 0;
}

static int
cancel_breakpoints_callback (struct lwp_info *lp, void *data)
{
  struct lwp_info *event_lp = data;

  /* Leave the LWP that has been elected to receive a SIGTRAP alone.  */
  if (lp == event_lp)
    return 0;

  /* If a LWP other than the LWP that we're reporting an event for has
     hit a GDB breakpoint (as opposed to some random trap signal),
     then just arrange for it to hit it again later.  We don't keep
     the SIGTRAP status and don't forward the SIGTRAP signal to the
     LWP.  We will handle the current event, eventually we will resume
     all LWPs, and this one will get its breakpoint trap again.

     If we do not do this, then we run the risk that the user will
     delete or disable the breakpoint, but the LWP will have already
     tripped on it.  */

  if (lp->status != 0
      && WIFSTOPPED (lp->status) &&  WSTOPSIG (lp->status) == SIGTRAP
      && breakpoint_inserted_here_p (read_pc_pid (lp->ptid) - 
				     DECR_PC_AFTER_BREAK))
    {
      if (debug_lin_lwp)
	fprintf_unfiltered (gdb_stdlog,
			    "Push back breakpoint for LWP %ld\n",
			    GET_LWP (lp->ptid));

      /* Back up the PC if necessary.  */
      if (DECR_PC_AFTER_BREAK)
	write_pc_pid (read_pc_pid (lp->ptid) - DECR_PC_AFTER_BREAK, lp->ptid);

      /* Throw away the SIGTRAP.  */
      lp->status = 0;
    }

  return 0;
}

/* Select one LWP out of those that have events pending.  */

static void
select_event_lwp (struct lwp_info **orig_lp, int *status)
{
  int num_events = 0;
  int random_selector;
  struct lwp_info *event_lp;

  /* Record the wait status for the origional LWP.  */
  (*orig_lp)->status = *status;

  /* Give preference to any LWP that is being single-stepped.  */
  event_lp = iterate_over_lwps (select_singlestep_lwp_callback, NULL);
  if (event_lp != NULL)
    {
      if (debug_lin_lwp)
	fprintf_unfiltered (gdb_stdlog,
			    "Select single-step LWP %ld\n",
			    GET_LWP (event_lp->ptid));
    }
  else
    {
      /* No single-stepping LWP.  Select one at random, out of those
	 which have had SIGTRAP events.  */

      /* First see how many SIGTRAP events we have.  */
      iterate_over_lwps (count_events_callback, &num_events);

      /* Now randomly pick a LWP out of those that have had a SIGTRAP.  */
      random_selector = (int)
	((num_events * (double) rand ()) / (RAND_MAX + 1.0));

      if (debug_lin_lwp && num_events > 1)
	fprintf_unfiltered (gdb_stdlog, 
			    "Found %d SIGTRAP events, selecting #%d\n", 
			    num_events, random_selector);

      event_lp = iterate_over_lwps (select_event_lwp_callback,
				    &random_selector);
    }

  if (event_lp != NULL)
    {
      /* Switch the event LWP.  */
      *orig_lp = event_lp;
      *status  = event_lp->status;
    }

  /* Flush the wait status for the event LWP.  */
  (*orig_lp)->status = 0;
}

/* Return non-zero if LP has been resumed.  */

static int
resumed_callback (struct lwp_info *lp, void *data)
{
  return lp->resumed;
}

static ptid_t
lin_lwp_wait (ptid_t ptid, struct target_waitstatus *ourstatus)
{
  struct lwp_info *lp = NULL;
  int options = 0;
  int status = 0;
  pid_t pid = PIDGET (ptid);
  sigset_t flush_mask;

  sigemptyset (&flush_mask);

  /* Make sure SIGCHLD is blocked.  */
  if (! sigismember (&blocked_mask, SIGCHLD))
    {
      sigaddset (&blocked_mask, SIGCHLD);
      sigprocmask (SIG_BLOCK, &blocked_mask, NULL);
    }

 retry:

  /* Make sure there is at least one LWP that has been resumed, at
     least if there are any LWPs at all.  */
  gdb_assert (num_lwps == 0 || iterate_over_lwps (resumed_callback, NULL));

  /* First check if there is a LWP with a wait status pending.  */
  if (pid == -1)
    {
      /* Any LWP that's been resumed will do.  */
      lp = iterate_over_lwps (status_callback, NULL);
      if (lp)
	{
	  status = lp->status;
	  lp->status = 0;

	  if (debug_lin_lwp && status)
	    fprintf_unfiltered (gdb_stdlog,
				"Using pending wait status %s for LWP %ld.\n",
				status_to_str (status), GET_LWP (lp->ptid));
	}

      /* But if we don't fine one, we'll have to wait, and check both
         cloned and uncloned processes.  We start with the cloned
         processes.  */
      options = __WCLONE | WNOHANG;
    }
  else if (is_lwp (ptid))
    {
      if (debug_lin_lwp)
	fprintf_unfiltered (gdb_stdlog, 
			    "Waiting for specific LWP %ld.\n",
			    GET_LWP (ptid));

      /* We have a specific LWP to check.  */
      lp = find_lwp_pid (ptid);
      gdb_assert (lp);
      status = lp->status;
      lp->status = 0;

      if (debug_lin_lwp && status)
	fprintf_unfiltered (gdb_stdlog,
			    "Using pending wait status %s for LWP %ld.\n",
			    status_to_str (status), GET_LWP (lp->ptid));

      /* If we have to wait, take into account whether PID is a cloned
         process or not.  And we have to convert it to something that
         the layer beneath us can understand.  */
      options = is_cloned (lp->ptid) ? __WCLONE : 0;
      pid = GET_LWP (ptid);
    }

  if (status && lp->signalled)
    {
      /* A pending SIGSTOP may interfere with the normal stream of
	 events.  In a typical case where interference is a problem,
	 we have a SIGSTOP signal pending for LWP A while
	 single-stepping it, encounter an event in LWP B, and take the
	 pending SIGSTOP while trying to stop LWP A.  After processing
	 the event in LWP B, LWP A is continued, and we'll never see
	 the SIGTRAP associated with the last time we were
	 single-stepping LWP A.  */

      /* Resume the thread.  It should halt immediately returning the
	 pending SIGSTOP.  */
      child_resume (pid_to_ptid (GET_LWP (lp->ptid)), lp->step,
                    TARGET_SIGNAL_0);
      lp->stopped = 0;
      gdb_assert (lp->resumed);

      /* This should catch the pending SIGSTOP.  */
      stop_wait_callback (lp, NULL);
    }

  set_sigint_trap ();	/* Causes SIGINT to be passed on to the
			   attached process. */
  set_sigio_trap ();

  while (status == 0)
    {
      pid_t lwpid;

      lwpid = waitpid (pid, &status, options);
      if (lwpid > 0)
	{
	  gdb_assert (pid == -1 || lwpid == pid);

	  lp = find_lwp_pid (pid_to_ptid (lwpid));
	  if (! lp)
	    {
	      lp = add_lwp (BUILD_LWP (lwpid, GET_PID (inferior_ptid)));
	      if (threaded)
		{
		  gdb_assert (WIFSTOPPED (status)
			      && WSTOPSIG (status) == SIGSTOP);
		  lp->signalled = 1;

		  if (! in_thread_list (inferior_ptid))
		    {
		      inferior_ptid = BUILD_LWP (GET_PID (inferior_ptid),
		                                 GET_PID (inferior_ptid));
		      add_thread (inferior_ptid);
		    }

		  add_thread (lp->ptid);
		  printf_unfiltered ("[New %s]\n",
				     target_pid_to_str (lp->ptid));
		}
	    }

	  /* Make sure we don't report a TARGET_WAITKIND_EXITED or
             TARGET_WAITKIND_SIGNALLED event if there are still LWP's
             left in the process.  */
	  if ((WIFEXITED (status) || WIFSIGNALED (status)) && num_lwps > 1)
	    {
	      if (in_thread_list (lp->ptid))
		{
		  /* Core GDB cannot deal with us deleting the current
                     thread.  */
		  if (! ptid_equal (lp->ptid, inferior_ptid))
		    delete_thread (lp->ptid);
		  printf_unfiltered ("[%s exited]\n",
				     target_pid_to_str (lp->ptid));
		}
	      if (debug_lin_lwp)
		fprintf_unfiltered (gdb_stdlog, 
				    "%s exited.\n", 
				    target_pid_to_str (lp->ptid));

	      delete_lwp (lp->ptid);

	      /* Make sure there is at least one thread running.  */
	      gdb_assert (iterate_over_lwps (running_callback, NULL));

	      /* Discard the event.  */
	      status = 0;
	      continue;
	    }

	  /* Make sure we don't report a SIGSTOP that we sent
             ourselves in an attempt to stop an LWP.  */
	  if (lp->signalled && WIFSTOPPED (status)
	      && WSTOPSIG (status) == SIGSTOP)
	    {
	      if (debug_lin_lwp)
		fprintf_unfiltered (gdb_stdlog, 
				    "Delayed SIGSTOP caught for %s.\n",
				    target_pid_to_str (lp->ptid));

	      /* This is a delayed SIGSTOP.  */
	      lp->signalled = 0;

	      child_resume (pid_to_ptid (GET_LWP (lp->ptid)), lp->step,
	                    TARGET_SIGNAL_0);
	      lp->stopped = 0;
	      gdb_assert (lp->resumed);

	      /* Discard the event.  */
	      status = 0;
	      continue;
	    }

	  break;
	}

      if (pid == -1)
	{
	  /* Alternate between checking cloned and uncloned processes.  */
	  options ^= __WCLONE;

	  /* And suspend every time we have checked both.  */
	  if (options & __WCLONE)
	    sigsuspend (&suspend_mask);
	}

      /* We shouldn't end up here unless we want to try again.  */
      gdb_assert (status == 0);
    }

  clear_sigio_trap ();
  clear_sigint_trap ();

  gdb_assert (lp);

  /* Don't report signals that GDB isn't interested in, such as
     signals that are neither printed nor stopped upon.  Stopping all
     threads can be a bit time-consuming so if we want decent
     performance with heavily multi-threaded programs, especially when
     they're using a high frequency timer, we'd better avoid it if we
     can.  */

  if (WIFSTOPPED (status))
    {
      int signo = target_signal_from_host (WSTOPSIG (status));

      if (signal_stop_state (signo) == 0
	  && signal_print_state (signo) == 0
	  && signal_pass_state (signo) == 1)
	{
	  /* FIMXE: kettenis/2001-06-06: Should we resume all threads
             here?  It is not clear we should.  GDB may not expect
             other threads to run.  On the other hand, not resuming
             newly attached threads may cause an unwanted delay in
             getting them running.  */
	  child_resume (pid_to_ptid (GET_LWP (lp->ptid)), lp->step, signo);
	  lp->stopped = 0;
	  status = 0;
	  goto retry;
	}

      if (signo == TARGET_SIGNAL_INT
	  && signal_pass_state (signo) == 0)
	{
	  /* If ^C/BREAK is typed at the tty/console, SIGINT gets
             forwarded to the entire process group, that is, all LWP's
             will receive it.  Since we only want to report it once,
             we try to flush it from all LWPs except this one.  */
	  sigaddset (&flush_mask, SIGINT);
	}
    }

  /* This LWP is stopped now.  */
  lp->stopped = 1;

  if (debug_lin_lwp)
    fprintf_unfiltered (gdb_stdlog, "Candidate event %s in LWP %ld.\n",
			status_to_str (status), GET_LWP (lp->ptid));

  /* Now stop all other LWP's ...  */
  iterate_over_lwps (stop_callback, NULL);

  /* ... and wait until all of them have reported back that they're no
     longer running.  */
  iterate_over_lwps (stop_wait_callback, &flush_mask);

  /* If we're not waiting for a specific LWP, choose an event LWP from
     among those that have had events.  Giving equal priority to all
     LWPs that have had events helps prevent starvation.  */
  if (pid == -1)
    select_event_lwp (&lp, &status);

  /* Now that we've selected our final event LWP, cancel any
     breakpoints in other LWPs that have hit a GDB breakpoint.  See
     the comment in cancel_breakpoints_callback to find out why.  */
  iterate_over_lwps (cancel_breakpoints_callback, lp);

  /* If we're not running in "threaded" mode, we'll report the bare
     process id.  */

  if (WIFSTOPPED (status) && WSTOPSIG (status) == SIGTRAP)
    {
      trap_ptid = (threaded ? lp->ptid : pid_to_ptid (GET_LWP (lp->ptid)));
      if (debug_lin_lwp)
	fprintf_unfiltered (gdb_stdlog, 
			    "LLW: trap_ptid is %ld\n",
			    GET_LWP (trap_ptid));
    }
  else
    trap_ptid = null_ptid;

  store_waitstatus (ourstatus, status);
  return (threaded ? lp->ptid : pid_to_ptid (GET_LWP (lp->ptid)));
}

static int
kill_callback (struct lwp_info *lp, void *data)
{
  ptrace (PTRACE_KILL, GET_LWP (lp->ptid), 0, 0);
  return 0;
}

static int
kill_wait_callback (struct lwp_info *lp, void *data)
{
  pid_t pid;

  /* We must make sure that there are no pending events (delayed
     SIGSTOPs, pending SIGTRAPs, etc.) to make sure the current
     program doesn't interfere with any following debugging session.  */

  /* For cloned processes we must check both with __WCLONE and
     without, since the exit status of a cloned process isn't reported
     with __WCLONE.  */
  if (is_cloned (lp->ptid))
    {
      do
	{
	  pid = waitpid (GET_LWP (lp->ptid), NULL, __WCLONE);
	}
      while (pid == GET_LWP (lp->ptid));

      gdb_assert (pid == -1 && errno == ECHILD);
    }

  do
    {
      pid = waitpid (GET_LWP (lp->ptid), NULL, 0);
    }
  while (pid == GET_LWP (lp->ptid));

  gdb_assert (pid == -1 && errno == ECHILD);
  return 0;
}

static void
lin_lwp_kill (void)
{
  /* Kill all LWP's ...  */
  iterate_over_lwps (kill_callback, NULL);

  /* ... and wait until we've flushed all events.  */
  iterate_over_lwps (kill_wait_callback, NULL);

  target_mourn_inferior ();
}

static void
lin_lwp_create_inferior (char *exec_file, char *allargs, char **env)
{
  child_ops.to_create_inferior (exec_file, allargs, env);
}

static void  
lin_lwp_mourn_inferior (void)
{
  trap_ptid = null_ptid;

  /* Destroy LWP info; it's no longer valid.  */
  init_lwp_list ();

  /* Restore the original signal mask.  */
  sigprocmask (SIG_SETMASK, &normal_mask, NULL);
  sigemptyset (&blocked_mask);

  child_ops.to_mourn_inferior ();
}

static void
lin_lwp_fetch_registers (int regno)
{
  struct cleanup *old_chain = save_inferior_ptid ();

  if (is_lwp (inferior_ptid))
    inferior_ptid = pid_to_ptid (GET_LWP (inferior_ptid));

  fetch_inferior_registers (regno);

  do_cleanups (old_chain);
}

static void
lin_lwp_store_registers (int regno)
{
  struct cleanup *old_chain = save_inferior_ptid ();

  if (is_lwp (inferior_ptid))
    inferior_ptid = pid_to_ptid (GET_LWP (inferior_ptid));

  store_inferior_registers (regno);

  do_cleanups (old_chain);
}

static int
lin_lwp_xfer_memory (CORE_ADDR memaddr, char *myaddr, int len, int write,
		     struct mem_attrib *attrib,
		     struct target_ops *target)
{
  struct cleanup *old_chain = save_inferior_ptid ();
  int xfer;

  if (is_lwp (inferior_ptid))
    inferior_ptid = pid_to_ptid (GET_LWP (inferior_ptid));

  xfer = child_xfer_memory (memaddr, myaddr, len, write, attrib, target);

  do_cleanups (old_chain);
  return xfer;
}

static int
lin_lwp_thread_alive (ptid_t ptid)
{
  gdb_assert (is_lwp (ptid));

  errno = 0;
  ptrace (PTRACE_PEEKUSER, GET_LWP (ptid), 0, 0);
  if (errno)
    return 0;

  return 1;
}

static char *
lin_lwp_pid_to_str (ptid_t ptid)
{
  static char buf[64];

  if (is_lwp (ptid))
    {
      snprintf (buf, sizeof (buf), "LWP %ld", GET_LWP (ptid));
      return buf;
    }

  return normal_pid_to_str (ptid);
}

static void
init_lin_lwp_ops (void)
{
#if 0
  lin_lwp_ops.to_open = lin_lwp_open;
#endif
  lin_lwp_ops.to_shortname = "lwp-layer";
  lin_lwp_ops.to_longname = "lwp-layer";
  lin_lwp_ops.to_doc = "Low level threads support (LWP layer)";
  lin_lwp_ops.to_attach = lin_lwp_attach;
  lin_lwp_ops.to_detach = lin_lwp_detach;
  lin_lwp_ops.to_resume = lin_lwp_resume;
  lin_lwp_ops.to_wait = lin_lwp_wait;
  lin_lwp_ops.to_fetch_registers = lin_lwp_fetch_registers;
  lin_lwp_ops.to_store_registers = lin_lwp_store_registers;
  lin_lwp_ops.to_xfer_memory = lin_lwp_xfer_memory;
  lin_lwp_ops.to_kill = lin_lwp_kill;
  lin_lwp_ops.to_create_inferior = lin_lwp_create_inferior;
  lin_lwp_ops.to_mourn_inferior = lin_lwp_mourn_inferior;
  lin_lwp_ops.to_thread_alive = lin_lwp_thread_alive;
  lin_lwp_ops.to_pid_to_str = lin_lwp_pid_to_str;
  lin_lwp_ops.to_stratum = thread_stratum;
  lin_lwp_ops.to_has_thread_control = tc_schedlock;
  lin_lwp_ops.to_magic = OPS_MAGIC;
}

static void
sigchld_handler (int signo)
{
  /* Do nothing.  The only reason for this handler is that it allows
     us to use sigsuspend in lin_lwp_wait above to wait for the
     arrival of a SIGCHLD.  */
}

void
_initialize_lin_lwp (void)
{
  struct sigaction action;

  extern void thread_db_init (struct target_ops *);

  init_lin_lwp_ops ();
  add_target (&lin_lwp_ops);
  thread_db_init (&lin_lwp_ops);

  /* Save the original signal mask.  */
  sigprocmask (SIG_SETMASK, NULL, &normal_mask);

  action.sa_handler = sigchld_handler;
  sigemptyset (&action.sa_mask);
  action.sa_flags = 0;
  sigaction (SIGCHLD, &action, NULL);

  /* Make sure we don't block SIGCHLD during a sigsuspend.  */
  sigprocmask (SIG_SETMASK, NULL, &suspend_mask);
  sigdelset (&suspend_mask, SIGCHLD);

  sigemptyset (&blocked_mask);

  add_show_from_set (add_set_cmd ("lin-lwp", no_class, var_zinteger,
				  (char *) &debug_lin_lwp, 
				  "Set debugging of linux lwp module.\n\
Enables printf debugging output.\n",
				      &setdebuglist),
		     &showdebuglist);
}


/* FIXME: kettenis/2000-08-26: The stuff on this page is specific to
   the LinuxThreads library and therefore doesn't really belong here.  */

/* Read variable NAME in the target and return its value if found.
   Otherwise return zero.  It is assumed that the type of the variable
   is `int'.  */

static int
get_signo (const char *name)
{
  struct minimal_symbol *ms;
  int signo;

  ms = lookup_minimal_symbol (name, NULL, NULL);
  if (ms == NULL)
    return 0;

  if (target_read_memory (SYMBOL_VALUE_ADDRESS (ms), (char *) &signo,
			  sizeof (signo)) != 0)
    return 0;

  return signo;
}

/* Return the set of signals used by the threads library in *SET.  */

void
lin_thread_get_thread_signals (sigset_t *set)
{
  struct sigaction action;
  int restart, cancel;

  sigemptyset (set);

  restart = get_signo ("__pthread_sig_restart");
  if (restart == 0)
    return;

  cancel = get_signo ("__pthread_sig_cancel");
  if (cancel == 0)
    return;

  sigaddset (set, restart);
  sigaddset (set, cancel);

  /* The LinuxThreads library makes terminating threads send a special
     "cancel" signal instead of SIGCHLD.  Make sure we catch those (to
     prevent them from terminating GDB itself, which is likely to be
     their default action) and treat them the same way as SIGCHLD.  */

  action.sa_handler = sigchld_handler;
  sigemptyset (&action.sa_mask);
  action.sa_flags = 0;
  sigaction (cancel, &action, NULL);

  /* We block the "cancel" signal throughout this code ...  */
  sigaddset (&blocked_mask, cancel);
  sigprocmask (SIG_BLOCK, &blocked_mask, NULL);

  /* ... except during a sigsuspend.  */
  sigdelset (&suspend_mask, cancel);
}
