/* Dummy data flow analysis for GNU compiler in nonoptimizing mode.
   Copyright (C) 1987, 1991, 1994, 1995, 1996 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This file performs stupid register allocation, which is used
   when cc1 gets the -noreg switch (which is when cc does not get -O).

   Stupid register allocation goes in place of the the flow_analysis,
   local_alloc and global_alloc passes.  combine_instructions cannot
   be done with stupid allocation because the data flow info that it needs
   is not computed here.

   In stupid allocation, the only user-defined variables that can
   go in registers are those declared "register".  They are assumed
   to have a life span equal to their scope.  Other user variables
   are given stack slots in the rtl-generation pass and are not
   represented as pseudo regs.  A compiler-generated temporary
   is assumed to live from its first mention to its last mention.

   Since each pseudo-reg's life span is just an interval, it can be
   represented as a pair of numbers, each of which identifies an insn by
   its position in the function (number of insns before it).  The first
   thing done for stupid allocation is to compute such a number for each
   insn.  It is called the suid.  Then the life-interval of each
   pseudo reg is computed.  Then the pseudo regs are ordered by priority
   and assigned hard regs in priority order.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "flags.h"

/* Vector mapping INSN_UIDs to suids.
   The suids are like uids but increase monotonically always.
   We use them to see whether a subroutine call came
   between a variable's birth and its death.  */

static int *uid_suid;

/* Get the suid of an insn.  */

#define INSN_SUID(INSN) (uid_suid[INSN_UID (INSN)])

/* Record the suid of the last CALL_INSN
   so we can tell whether a pseudo reg crosses any calls.  */

static int last_call_suid;

/* Record the suid of the last NOTE_INSN_SETJMP
   so we can tell whether a pseudo reg crosses any setjmp.  */

static int last_setjmp_suid;

/* Element N is suid of insn where life span of pseudo reg N ends.
   Element is  0 if register N has not been seen yet on backward scan.  */

static int *reg_where_dead;

/* Element N is suid of insn where life span of pseudo reg N begins.  */

static int *reg_where_born;

/* Numbers of pseudo-regs to be allocated, highest priority first.  */

static int *reg_order;

/* Indexed by reg number (hard or pseudo), nonzero if register is live
   at the current point in the instruction stream.  */

static char *regs_live;

/* Indexed by reg number, nonzero if reg was used in a SUBREG that changes
   its size.  */

static char *regs_change_size;

/* Indexed by reg number, nonzero if reg crosses a setjmp.  */

static char *regs_crosses_setjmp;

/* Indexed by insn's suid, the set of hard regs live after that insn.  */

static HARD_REG_SET *after_insn_hard_regs;

/* Record that hard reg REGNO is live after insn INSN.  */

#define MARK_LIVE_AFTER(INSN,REGNO)  \
  SET_HARD_REG_BIT (after_insn_hard_regs[INSN_SUID (INSN)], (REGNO))

static int stupid_reg_compare	PROTO((int *, int *));
static int stupid_find_reg	PROTO((int, enum reg_class, enum machine_mode,
				       int, int, int));
static void stupid_mark_refs	PROTO((rtx, rtx));

/* Stupid life analysis is for the case where only variables declared
   `register' go in registers.  For this case, we mark all
   pseudo-registers that belong to register variables as
   dying in the last instruction of the function, and all other
   pseudo registers as dying in the last place they are referenced.
   Hard registers are marked as dying in the last reference before
   the end or before each store into them.  */

void
stupid_life_analysis (f, nregs, file)
     rtx f;
     int nregs;
     FILE *file;
{
  register int i;
  register rtx last, insn;
  int max_uid, max_suid;

  bzero (regs_ever_live, sizeof regs_ever_live);

  regs_live = (char *) alloca (nregs);

  /* First find the last real insn, and count the number of insns,
     and assign insns their suids.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    if (INSN_UID (insn) > i)
      i = INSN_UID (insn);

  max_uid = i + 1;
  uid_suid = (int *) alloca ((i + 1) * sizeof (int));

  /* Compute the mapping from uids to suids.
     Suids are numbers assigned to insns, like uids,
     except that suids increase monotonically through the code.  */

  last = 0;			/* In case of empty function body */
  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	last = insn;

      INSN_SUID (insn) = ++i;
    }

  last_call_suid = i + 1;
  last_setjmp_suid = i + 1;
  max_suid = i + 1;

  max_regno = nregs;

  /* Allocate tables to record info about regs.  */

  reg_where_dead = (int *) alloca (nregs * sizeof (int));
  bzero ((char *) reg_where_dead, nregs * sizeof (int));

  reg_where_born = (int *) alloca (nregs * sizeof (int));
  bzero ((char *) reg_where_born, nregs * sizeof (int));

  reg_order = (int *) alloca (nregs * sizeof (int));
  bzero ((char *) reg_order, nregs * sizeof (int));

  regs_change_size = (char *) alloca (nregs * sizeof (char));
  bzero ((char *) regs_change_size, nregs * sizeof (char));

  regs_crosses_setjmp = (char *) alloca (nregs * sizeof (char));
  bzero ((char *) regs_crosses_setjmp, nregs * sizeof (char));

  reg_renumber = (short *) oballoc (nregs * sizeof (short));
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    reg_renumber[i] = i;

  for (i = FIRST_VIRTUAL_REGISTER; i < max_regno; i++)
    reg_renumber[i] = -1;

  after_insn_hard_regs
    = (HARD_REG_SET *) alloca (max_suid * sizeof (HARD_REG_SET));

  bzero ((char *) after_insn_hard_regs, max_suid * sizeof (HARD_REG_SET));

  /* Allocate and zero out many data structures
     that will record the data from lifetime analysis.  */

  allocate_for_life_analysis ();

  for (i = 0; i < max_regno; i++)
    reg_n_deaths[i] = 1;

  bzero (regs_live, nregs);

  /* Find where each pseudo register is born and dies,
     by scanning all insns from the end to the start
     and noting all mentions of the registers.

     Also find where each hard register is live
     and record that info in after_insn_hard_regs.
     regs_live[I] is 1 if hard reg I is live
     at the current point in the scan.  */

  for (insn = last; insn; insn = PREV_INSN (insn))
    {
      register HARD_REG_SET *p = after_insn_hard_regs + INSN_SUID (insn);

      /* Copy the info in regs_live into the element of after_insn_hard_regs
	 for the current position in the rtl code.  */

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (regs_live[i])
	  SET_HARD_REG_BIT (*p, i);

      /* Update which hard regs are currently live
	 and also the birth and death suids of pseudo regs
	 based on the pattern of this insn.  */

      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	stupid_mark_refs (PATTERN (insn), insn);

      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	last_setjmp_suid = INSN_SUID (insn);

      /* Mark all call-clobbered regs as live after each call insn
	 so that a pseudo whose life span includes this insn
	 will not go in one of them.
	 Then mark those regs as all dead for the continuing scan
	 of the insns before the call.  */

      if (GET_CODE (insn) == CALL_INSN)
	{
	  last_call_suid = INSN_SUID (insn);
	  IOR_HARD_REG_SET (after_insn_hard_regs[last_call_suid],
			    call_used_reg_set);

	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (call_used_regs[i])
	      regs_live[i] = 0;

	  /* It is important that this be done after processing the insn's
	     pattern because we want the function result register to still
	     be live if it's also used to pass arguments.  */
	  stupid_mark_refs (CALL_INSN_FUNCTION_USAGE (insn), insn);
	}
    }

  /* Now decide the order in which to allocate the pseudo registers.  */

  for (i = LAST_VIRTUAL_REGISTER + 1; i < max_regno; i++)
    reg_order[i] = i;

  qsort (&reg_order[LAST_VIRTUAL_REGISTER + 1],
	 max_regno - LAST_VIRTUAL_REGISTER - 1, sizeof (int),
	 stupid_reg_compare);

  /* Now, in that order, try to find hard registers for those pseudo regs.  */

  for (i = LAST_VIRTUAL_REGISTER + 1; i < max_regno; i++)
    {
      register int r = reg_order[i];

      /* Some regnos disappear from the rtl.  Ignore them to avoid crash. 
	 Also don't allocate registers that cross a setjmp.  */
      if (regno_reg_rtx[r] == 0 || regs_crosses_setjmp[r])
	continue;

      /* Now find the best hard-register class for this pseudo register */
      if (N_REG_CLASSES > 1)
	reg_renumber[r] = stupid_find_reg (reg_n_calls_crossed[r], 
					   reg_preferred_class (r),
					   PSEUDO_REGNO_MODE (r),
					   reg_where_born[r],
					   reg_where_dead[r],
					   regs_change_size[r]);

      /* If no reg available in that class, try alternate class.  */
      if (reg_renumber[r] == -1 && reg_alternate_class (r) != NO_REGS)
	reg_renumber[r] = stupid_find_reg (reg_n_calls_crossed[r],
					   reg_alternate_class (r),
					   PSEUDO_REGNO_MODE (r),
					   reg_where_born[r],
					   reg_where_dead[r],
					   regs_change_size[r]);
    }

  if (file)
    dump_flow_info (file);
}

/* Comparison function for qsort.
   Returns -1 (1) if register *R1P is higher priority than *R2P.  */

static int
stupid_reg_compare (r1p, r2p)
     int *r1p, *r2p;
{
  register int r1 = *r1p, r2 = *r2p;
  register int len1 = reg_where_dead[r1] - reg_where_born[r1];
  register int len2 = reg_where_dead[r2] - reg_where_born[r2];
  int tem;

  tem = len2 - len1;
  if (tem != 0)
    return tem;

  tem = reg_n_refs[r1] - reg_n_refs[r2];
  if (tem != 0)
    return tem;

  /* If regs are equally good, sort by regno,
     so that the results of qsort leave nothing to chance.  */
  return r1 - r2;
}

/* Find a block of SIZE words of hard registers in reg_class CLASS
   that can hold a value of machine-mode MODE
     (but actually we test only the first of the block for holding MODE)
   currently free from after insn whose suid is BORN_INSN
   through the insn whose suid is DEAD_INSN,
   and return the number of the first of them.
   Return -1 if such a block cannot be found.

   If CALL_PRESERVED is nonzero, insist on registers preserved
   over subroutine calls, and return -1 if cannot find such.

   If CHANGES_SIZE is nonzero, it means this register was used as the
   operand of a SUBREG that changes its size.  */

static int
stupid_find_reg (call_preserved, class, mode,
		 born_insn, dead_insn, changes_size)
     int call_preserved;
     enum reg_class class;
     enum machine_mode mode;
     int born_insn, dead_insn;
     int changes_size;
{
  register int i, ins;
#ifdef HARD_REG_SET
  register		/* Declare them register if they are scalars.  */
#endif
    HARD_REG_SET used, this_reg;
#ifdef ELIMINABLE_REGS
  static struct {int from, to; } eliminables[] = ELIMINABLE_REGS;
#endif

  /* If this register's life is more than 5,000 insns, we probably
     can't allocate it, so don't waste the time trying.  This avoids
     quadratic behavior on programs that have regularly-occurring
     SAVE_EXPRs.  */
  if (dead_insn > born_insn + 5000)
    return -1;

  COPY_HARD_REG_SET (used,
		     call_preserved ? call_used_reg_set : fixed_reg_set);

#ifdef ELIMINABLE_REGS
  for (i = 0; i < sizeof eliminables / sizeof eliminables[0]; i++)
    SET_HARD_REG_BIT (used, eliminables[i].from);
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
  SET_HARD_REG_BIT (used, HARD_FRAME_POINTER_REGNUM);
#endif
#else
  SET_HARD_REG_BIT (used, FRAME_POINTER_REGNUM);
#endif

  for (ins = born_insn; ins < dead_insn; ins++)
    IOR_HARD_REG_SET (used, after_insn_hard_regs[ins]);

  IOR_COMPL_HARD_REG_SET (used, reg_class_contents[(int) class]);

#ifdef CLASS_CANNOT_CHANGE_SIZE
  if (changes_size)
    IOR_HARD_REG_SET (used,
		      reg_class_contents[(int) CLASS_CANNOT_CHANGE_SIZE]);
#endif

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
#ifdef REG_ALLOC_ORDER
      int regno = reg_alloc_order[i];
#else
      int regno = i;
#endif

      /* If a register has screwy overlap problems,
	 don't use it at all if not optimizing.
	 Actually this is only for the 387 stack register,
	 and it's because subsequent code won't work.  */
#ifdef OVERLAPPING_REGNO_P
      if (OVERLAPPING_REGNO_P (regno))
	continue;
#endif

      if (! TEST_HARD_REG_BIT (used, regno)
	  && HARD_REGNO_MODE_OK (regno, mode))
	{
	  register int j;
	  register int size1 = HARD_REGNO_NREGS (regno, mode);
	  for (j = 1; j < size1 && ! TEST_HARD_REG_BIT (used, regno + j); j++);
	  if (j == size1)
	    {
	      CLEAR_HARD_REG_SET (this_reg);
	      while (--j >= 0)
		SET_HARD_REG_BIT (this_reg, regno + j);
	      for (ins = born_insn; ins < dead_insn; ins++)
		{
		  IOR_HARD_REG_SET (after_insn_hard_regs[ins], this_reg);
		}
	      return regno;
	    }
#ifndef REG_ALLOC_ORDER
	  i += j;		/* Skip starting points we know will lose */
#endif
	}
    }

  return -1;
}

/* Walk X, noting all assignments and references to registers
   and recording what they imply about life spans.
   INSN is the current insn, supplied so we can find its suid.  */

static void
stupid_mark_refs (x, insn)
     rtx x, insn;
{
  register RTX_CODE code;
  register char *fmt;
  register int regno, i;

  if (x == 0)
    return;

  code = GET_CODE (x);

  if (code == SET || code == CLOBBER)
    {
      if (SET_DEST (x) != 0
	  && (GET_CODE (SET_DEST (x)) == REG
	      || (GET_CODE (SET_DEST (x)) == SUBREG
		  && GET_CODE (SUBREG_REG (SET_DEST (x))) == REG
		  && (REGNO (SUBREG_REG (SET_DEST (x)))
		      >= FIRST_PSEUDO_REGISTER))))
	{
	  /* Register is being assigned.  */
	  /* If setting a SUBREG, we treat the entire reg as being set.  */
	  if (GET_CODE (SET_DEST (x)) == SUBREG)
	    regno = REGNO (SUBREG_REG (SET_DEST (x)));
	  else
	    regno = REGNO (SET_DEST (x));

	  /* For hard regs, update the where-live info.  */
	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      register int j
		= HARD_REGNO_NREGS (regno, GET_MODE (SET_DEST (x)));

	      while (--j >= 0)
		{
		  regs_ever_live[regno+j] = 1;
		  regs_live[regno+j] = 0;

		  /* The following line is for unused outputs;
		     they do get stored even though never used again.  */
		  MARK_LIVE_AFTER (insn, regno+j);

		  /* When a hard reg is clobbered, mark it in use
		     just before this insn, so it is live all through.  */
		  if (code == CLOBBER && INSN_SUID (insn) > 0)
		    SET_HARD_REG_BIT (after_insn_hard_regs[INSN_SUID (insn) - 1],
				      regno+j);
		}
	    }
	  /* For pseudo regs, record where born, where dead, number of
	     times used, and whether live across a call.  */
	  else
	    {
	      /* Update the life-interval bounds of this pseudo reg.  */

	      /* When a pseudo-reg is CLOBBERed, it is born just before
		 the clobbering insn.  When setting, just after.  */
	      int where_born = INSN_SUID (insn) - (code == CLOBBER);

	      reg_where_born[regno] = where_born;

	      /* The reg must live at least one insn even
		 in it is never again used--because it has to go
		 in SOME hard reg.  Mark it as dying after the current
		 insn so that it will conflict with any other outputs of
		 this insn.  */
	      if (reg_where_dead[regno] < where_born + 2)
		{
		  reg_where_dead[regno] = where_born + 2;
		  regs_live[regno] = 1;
		}

	      /* Count the refs of this reg.  */
	      reg_n_refs[regno]++;

	      if (last_call_suid < reg_where_dead[regno])
		reg_n_calls_crossed[regno] += 1;

	      if (last_setjmp_suid < reg_where_dead[regno])
		regs_crosses_setjmp[regno] = 1;
	    }
	}

      /* Record references from the value being set,
	 or from addresses in the place being set if that's not a reg.
	 If setting a SUBREG, we treat the entire reg as *used*.  */
      if (code == SET)
	{
	  stupid_mark_refs (SET_SRC (x), insn);
	  if (GET_CODE (SET_DEST (x)) != REG)
	    stupid_mark_refs (SET_DEST (x), insn);
	}
      return;
    }

  else if (code == SUBREG
	   && GET_CODE (SUBREG_REG (x)) == REG
	   && REGNO (SUBREG_REG (x)) >= FIRST_PSEUDO_REGISTER
	   && (GET_MODE_SIZE (GET_MODE (x))
	       != GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	   && (INTEGRAL_MODE_P (GET_MODE (x))
	       || INTEGRAL_MODE_P (GET_MODE (SUBREG_REG (x)))))
    regs_change_size[REGNO (SUBREG_REG (x))] = 1;

  /* Register value being used, not set.  */

  else if (code == REG)
    {
      regno = REGNO (x);
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  /* Hard reg: mark it live for continuing scan of previous insns.  */
	  register int j = HARD_REGNO_NREGS (regno, GET_MODE (x));
	  while (--j >= 0)
	    {
	      regs_ever_live[regno+j] = 1;
	      regs_live[regno+j] = 1;
	    }
	}
      else
	{
	  /* Pseudo reg: record first use, last use and number of uses.  */

	  reg_where_born[regno] = INSN_SUID (insn);
	  reg_n_refs[regno]++;
	  if (regs_live[regno] == 0)
	    {
	      regs_live[regno] = 1;
	      reg_where_dead[regno] = INSN_SUID (insn);
	    }
	}
      return;
    }

  /* Recursive scan of all other rtx's.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	stupid_mark_refs (XEXP (x, i), insn);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    stupid_mark_refs (XVECEXP (x, i, j), insn);
	}
    }
}
