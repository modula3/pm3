/* Xstormy16 target functions.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "toplev.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "output.h"
#include "except.h"
#include "function.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"

static rtx emit_addhi3_postreload PARAMS ((rtx, rtx, rtx));
static void xstormy16_asm_out_constructor PARAMS ((rtx, int));
static void xstormy16_asm_out_destructor PARAMS ((rtx, int));

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */
struct rtx_def * xstormy16_compare_op0;
struct rtx_def * xstormy16_compare_op1;

/* Return 1 if this is a LT, GE, LTU, or GEU operator.  */

int
xstormy16_ineqsi_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == LT || code == GE || code == LTU || code == GEU));
}

/* Return 1 if this is an EQ or NE operator.  */

int
equality_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (GET_CODE (op) == EQ || GET_CODE (op) == NE));
}

/* Return 1 if this is a comparison operator but not an EQ or NE operator.  */

int
inequality_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return comparison_operator (op, mode) && ! equality_operator (op, mode);
}

/* Branches are handled as follows:

   1. HImode compare-and-branches.  The machine supports these
      natively, so the appropriate pattern is emitted directly.

   2. SImode EQ and NE.  These are emitted as pairs of HImode
      compare-and-branches.      

   3. SImode LT, GE, LTU and GEU.  These are emitted as a sequence
      of a SImode subtract followed by a branch (not a compare-and-branch),
      like this:
      sub
      sbc
      blt

   4. SImode GT, LE, GTU, LEU.  These are emitted as a sequence like:
      sub
      sbc
      blt
      or
      bne
*/

/* Emit a branch of kind CODE to location LOC.  */

void
xstormy16_emit_cbranch (code, loc)
     enum rtx_code code;
     rtx loc;
{
  rtx op0 = xstormy16_compare_op0;
  rtx op1 = xstormy16_compare_op1;
  rtx condition_rtx, loc_ref, branch, cy_clobber;
  rtvec vec;
  enum machine_mode mode;
  
  mode = GET_MODE (op0);
  if (mode != HImode && mode != SImode)
    abort ();

  if (mode == SImode
      && (code == GT || code == LE || code == GTU || code == LEU))
    {
      int unsigned_p = (code == GTU || code == LEU);
      int gt_p = (code == GT || code == GTU);
      rtx lab = NULL_RTX;
      
      if (gt_p)
	lab = gen_label_rtx ();
      xstormy16_emit_cbranch (unsigned_p ? LTU : LT, gt_p ? lab : loc);
      /* This should be generated as a comparison against the temporary
	 created by the previous insn, but reload can't handle that.  */
      xstormy16_emit_cbranch (gt_p ? NE : EQ, loc);
      if (gt_p)
	emit_label (lab);
      return;
    }
  else if (mode == SImode 
	   && (code == NE || code == EQ)
	   && op1 != const0_rtx)
    {
      rtx lab = NULL_RTX;
      int num_words = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
      int i;
      
      if (code == EQ)
	lab = gen_label_rtx ();
      
      for (i = 0; i < num_words - 1; i++)
	{
	  xstormy16_compare_op0 = simplify_gen_subreg (word_mode, op0, mode, 
						      i * UNITS_PER_WORD);
	  xstormy16_compare_op1 = simplify_gen_subreg (word_mode, op1, mode, 
						      i * UNITS_PER_WORD);
	  xstormy16_emit_cbranch (NE, code == EQ ? lab : loc);
	}
      xstormy16_compare_op0 = simplify_gen_subreg (word_mode, op0, mode, 
						  i * UNITS_PER_WORD);
      xstormy16_compare_op1 = simplify_gen_subreg (word_mode, op1, mode, 
						  i * UNITS_PER_WORD);
      xstormy16_emit_cbranch (code, loc);

      if (code == EQ)
	emit_label (lab);
      return;
    }

  /* We can't allow reload to try to generate any reload after a branch,
     so when some register must match we must make the temporary ourselves.  */
  if (mode != HImode)
    {
      rtx tmp;
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, op0);
      op0 = tmp;
    }

  condition_rtx = gen_rtx (code, mode, op0, op1);
  loc_ref = gen_rtx_LABEL_REF (VOIDmode, loc);
  branch = gen_rtx_SET (VOIDmode, pc_rtx,
			gen_rtx_IF_THEN_ELSE (VOIDmode, condition_rtx,
					      loc_ref, pc_rtx));

  cy_clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (BImode));

  if (mode == HImode)
    vec = gen_rtvec (2, branch, cy_clobber);
  else if (code == NE || code == EQ)
    vec = gen_rtvec (2, branch, gen_rtx_CLOBBER (VOIDmode, op0));
  else
    {
      rtx sub;
#if 0
      sub = gen_rtx_SET (VOIDmode, op0, gen_rtx_MINUS (SImode, op0, op1));
#else
      sub = gen_rtx_CLOBBER (SImode, op0);
#endif
      vec = gen_rtvec (3, branch, sub, cy_clobber);
    }

  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, vec));
}

/* Take a SImode conditional branch, one of GT/LE/GTU/LEU, and split
   the arithmetic operation.  Most of the work is done by
   xstormy16_expand_arith.  */

void
xstormy16_split_cbranch (mode, label, comparison, dest, carry)
     enum machine_mode mode;
     rtx label;
     rtx comparison;
     rtx dest;
     rtx carry;
{
  rtx op0 = XEXP (comparison, 0);
  rtx op1 = XEXP (comparison, 1);
  rtx seq;
  rtx compare;
  
  start_sequence ();
  xstormy16_expand_arith (mode, COMPARE, dest, op0, op1, carry);
  seq = gen_sequence ();
  end_sequence ();
  compare = SET_SRC (XVECEXP (PATTERN (XVECEXP (seq, 0, XVECLEN (seq, 0) - 1)),
			      0, 0));
  PUT_CODE (XEXP (compare, 0), GET_CODE (comparison));
  XEXP (compare, 1) = gen_rtx_LABEL_REF (VOIDmode, label);
  emit_insn (seq);
}


/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label.

   OP is the conditional expression, or NULL for branch-always.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   INSN is the insn.  */

char *
xstormy16_output_cbranch_hi (op, label, reversed, insn)
     rtx op;
     const char * label;
     int reversed;
     rtx insn;
{
  static char string[64];
  int need_longbranch = (op != NULL_RTX
			 ? get_attr_length (insn) == 8
			 : get_attr_length (insn) == 4);
  int really_reversed = reversed ^ need_longbranch;
  const char *ccode;
  const char *template;
  const char *operands;
  enum rtx_code code;
  
  if (! op)
    {
      if (need_longbranch)
	ccode = "jmpf";
      else
	ccode = "br";
      sprintf (string, "%s %s", ccode, label);
      return string;
    }

  code = GET_CODE (op);

  if (GET_CODE (XEXP (op, 0)) != REG)
    {
      code = swap_condition (code);
      operands = "%3,%2";
    }
  else
      operands = "%2,%3";

  /* Work out which way this really branches.  */
  if (really_reversed)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:   ccode = "z";   break;
    case NE:   ccode = "nz";  break;
    case GE:   ccode = "ge";  break;
    case LT:   ccode = "lt";  break;
    case GT:   ccode = "gt";  break;
    case LE:   ccode = "le";  break;
    case GEU:  ccode = "nc";  break;
    case LTU:  ccode = "c";   break;
    case GTU:  ccode = "hi";  break;
    case LEU:  ccode = "ls";  break;
      
    default:
      abort ();
    }

  if (need_longbranch)
    template = "b%s %s,.+8 | jmpf %s";
  else
    template = "b%s %s,%s";
  sprintf (string, template, ccode, operands, label);
  
  return string;
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label, but suitable for the tail of a
   SImode branch.

   OP is the conditional expression (OP is never NULL_RTX).

   REVERSED is non-zero if we should reverse the sense of the comparison.

   INSN is the insn.  */

char *
xstormy16_output_cbranch_si (op, label, reversed, insn)
     rtx op;
     const char * label;
     int reversed;
     rtx insn;
{
  static char string[64];
  int need_longbranch = get_attr_length (insn) >= 8;
  int really_reversed = reversed ^ need_longbranch;
  const char *ccode;
  const char *template;
  char prevop[16];
  enum rtx_code code;
  
  code = GET_CODE (op);

  /* Work out which way this really branches.  */
  if (really_reversed)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:   ccode = "z";   break;
    case NE:   ccode = "nz";  break;
    case GE:   ccode = "ge";  break;
    case LT:   ccode = "lt";  break;
    case GEU:  ccode = "nc";  break;
    case LTU:  ccode = "c";   break;

      /* The missing codes above should never be generated.  */
    default:
      abort ();
    }

  switch (code)
    {
    case EQ: case NE:
      {
	int regnum;
	
	if (GET_CODE (XEXP (op, 0)) != REG)
	  abort ();
      
	regnum = REGNO (XEXP (op, 0));
	sprintf (prevop, "or %s,%s", reg_names[regnum], reg_names[regnum+1]);
      }
      break;

    case GE: case LT: case GEU: case LTU:
      strcpy (prevop, "sbc %2,%3");
      break;

    default:
      abort ();
    }

  if (need_longbranch)
    template = "%s | b%s .+6 | jmpf %s";
  else
    template = "%s | b%s %s";
  sprintf (string, template, prevop, ccode, label);
  
  return string;
}

/* Many machines have some registers that cannot be copied directly to or from
   memory or even from other types of registers.  An example is the `MQ'
   register, which on most machines, can only be copied to or from general
   registers, but not memory.  Some machines allow copying all registers to and
   from memory, but require a scratch register for stores to some memory
   locations (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the Sparc when compiling PIC).  In some cases,
   both an intermediate and a scratch register are required.

   You should define these macros to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   CLASS in MODE requires an intermediate register, you should define
   `SECONDARY_INPUT_RELOAD_CLASS' to return the largest register class all of
   whose registers can be used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate or scratch
   register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be defined to return the
   largest register class required.  If the requirements for input and output
   reloads are the same, the macro `SECONDARY_RELOAD_CLASS' should be used
   instead of defining both macros identically.

   The values returned by these macros are often `GENERAL_REGS'.  Return
   `NO_REGS' if no spare register is needed; i.e., if X can be directly copied
   to or from a register of CLASS in MODE without requiring a scratch register.
   Do not define this macro if it would always return `NO_REGS'.

   If a scratch register is required (either with or without an intermediate
   register), you should define patterns for `reload_inM' or `reload_outM', as
   required..  These patterns, which will normally be implemented with a
   `define_expand', should be similar to the `movM' patterns, except that
   operand 2 is the scratch register.

   Define constraints for the reload register and scratch register that contain
   a single register class.  If the original reload register (whose class is
   CLASS) can meet the constraint given in the pattern, the value returned by
   these macros is used for the class of the scratch register.  Otherwise, two
   additional reload registers are required.  Their classes are obtained from
   the constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register, which could
   either be in a hard register or in memory.  Use `true_regnum' to find out;
   it will return -1 if the pseudo is in memory and the hard register number if
   it is in a register.

   These macros should not be used in the case where a particular class of
   registers can only be copied to memory and not to another class of
   registers.  In that case, secondary reload registers are not needed and
   would not be helpful.  Instead, a stack location must be used to perform the
   copy and the `movM' pattern should use memory as an intermediate storage.
   This case often occurs between floating-point and general registers.  */

enum reg_class
xstormy16_secondary_reload_class (class, mode, x)
     enum reg_class class;
     enum machine_mode mode;
     rtx x;
{
  /* This chip has the interesting property that only the first eight
     registers can be moved to/from memory.  */
  if ((GET_CODE (x) == MEM
       || ((GET_CODE (x) == SUBREG || GET_CODE (x) == REG)
	   && (true_regnum (x) == -1
	       || true_regnum (x) >= FIRST_PSEUDO_REGISTER)))
      && ! reg_class_subset_p (class, EIGHT_REGS))
    return EIGHT_REGS;

  /* When reloading a PLUS, the carry register will be required
     unless the inc or dec instructions can be used.  */
  if (xstormy16_carry_plus_operand (x, mode))
    return CARRY_REGS;

  return NO_REGS;
}

/* Recognise a PLUS that needs the carry register.  */
int
xstormy16_carry_plus_operand (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (x) == PLUS
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (INTVAL (XEXP (x, 1)) < -4 || INTVAL (XEXP (x, 1)) > 4));
}


enum reg_class
xstormy16_preferred_reload_class (x, class)
     enum reg_class class;
     rtx x;
{
  if (class == GENERAL_REGS
      && GET_CODE (x) == MEM)
    return EIGHT_REGS;

  return class;
}

#define LEGITIMATE_ADDRESS_INTEGER_P(X, OFFSET)				\
 (GET_CODE (X) == CONST_INT						\
  && (unsigned HOST_WIDE_INT) (INTVAL (X) + (OFFSET) + 2048) < 4096)

#define LEGITIMATE_ADDRESS_CONST_INT_P(X, OFFSET)			 \
 (GET_CODE (X) == CONST_INT						 \
  && INTVAL (X) + (OFFSET) >= 0						 \
  && INTVAL (X) + (OFFSET) < 0x8000					 \
  && (INTVAL (X) + (OFFSET) < 0x100 || INTVAL (X) + (OFFSET) >= 0x7F00))

int
xstormy16_legitimate_address_p (mode, x, strict)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x;
     int strict;
{
  if (LEGITIMATE_ADDRESS_CONST_INT_P (x, 0))
    return 1;

  if (GET_CODE (x) == PLUS
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (x, 1), 0))
    x = XEXP (x, 0);
  
  if (GET_CODE (x) == POST_INC
      || GET_CODE (x) == PRE_DEC)
    x = XEXP (x, 0);
  
  if (GET_CODE (x) == REG && REGNO_OK_FOR_BASE_P (REGNO (x))
      && (! strict || REGNO (x) < FIRST_PSEUDO_REGISTER))
    return 1;
  
  return 0;
}

/* Return nonzero if memory address X (an RTX) can have different
   meanings depending on the machine mode of the memory reference it
   is used for or if the address is valid for some modes but not
   others.

   Autoincrement and autodecrement addresses typically have mode-dependent
   effects because the amount of the increment or decrement is the size of the
   operand being addressed.  Some machines have other mode-dependent addresses.
   Many RISC machines have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  
   
   On this chip, this is true if the address is valid with an offset
   of 0 but not of 6, because in that case it cannot be used as an
   address for DImode or DFmode, or if the address is a post-increment
   or pre-decrement address.  */
int
xstormy16_mode_dependent_address_p (x)
     rtx x;
{
  if (LEGITIMATE_ADDRESS_CONST_INT_P (x, 0)
      && ! LEGITIMATE_ADDRESS_CONST_INT_P (x, 6))
    return 1;
  
  if (GET_CODE (x) == PLUS
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (x, 1), 0)
      && ! LEGITIMATE_ADDRESS_INTEGER_P (XEXP (x, 1), 6))
    return 1;

  if (GET_CODE (x) == PLUS)
    x = XEXP (x, 0);

  if (GET_CODE (x) == POST_INC
      || GET_CODE (x) == PRE_DEC)
    return 1;

  return 0;
}

/* A C expression that defines the optional machine-dependent constraint
   letters (`Q', `R', `S', `T', `U') that can be used to segregate specific
   types of operands, usually memory references, for the target machine.
   Normally this macro will not be defined.  If it is required for a particular
   target machine, it should return 1 if VALUE corresponds to the operand type
   represented by the constraint letter C.  If C is not defined as an extra
   constraint, the value returned should be 0 regardless of VALUE.  */
int
xstormy16_extra_constraint_p (x, c)
     rtx x;
     int c;
{
  switch (c)
    {
      /* 'Q' is for pushes.  */
    case 'Q':
      return (GET_CODE (x) == MEM
	      && GET_CODE (XEXP (x, 0)) == POST_INC
	      && XEXP (XEXP (x, 0), 0) == stack_pointer_rtx);

      /* 'R' is for pops.  */
    case 'R':
      return (GET_CODE (x) == MEM
	      && GET_CODE (XEXP (x, 0)) == PRE_DEC
	      && XEXP (XEXP (x, 0), 0) == stack_pointer_rtx);

      /* 'S' is for immediate memory addresses.  */
    case 'S':
      return (GET_CODE (x) == MEM
	      && GET_CODE (XEXP (x, 0)) == CONST_INT
	      && xstormy16_legitimate_address_p (VOIDmode, XEXP (x, 0), 0));

      /* 'T' is for Rx.  */
    case 'T':
      /* Not implemented yet.  */
      return 0;

      /* 'U' is for CONST_INT values not between 2 and 15 inclusive,
	 for allocating a scratch register for 32-bit shifts.  */
    case 'U':
      return (GET_CODE (x) == CONST_INT
	      && (INTVAL (x) < 2 || INTVAL (x) > 15));

    default:
      return 0;
    }
}

int
short_memory_operand (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (! memory_operand (x, mode))
    return 0;
  return (GET_CODE (XEXP (x, 0)) != PLUS);
}

int
nonimmediate_nonstack_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* 'Q' is for pushes, 'R' for pops.  */
  return (nonimmediate_operand (op, mode) 
	  && ! xstormy16_extra_constraint_p (op, 'Q')
	  && ! xstormy16_extra_constraint_p (op, 'R'));
}

/* Splitter for the 'move' patterns, for modes not directly implemeted
   by hardware.  Emit insns to copy a value of mode MODE from SRC to
   DEST.

   This function is only called when reload_completed.
   */

void 
xstormy16_split_move (mode, dest, src)
     enum machine_mode mode;
     rtx dest;
     rtx src;
{
  int num_words = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
  int direction, end, i;
  int src_modifies = 0;
  int dest_modifies = 0;
  int src_volatile = 0;
  int dest_volatile = 0;
  rtx mem_operand;
  rtx auto_inc_reg_rtx = NULL_RTX;
  
  /* Check initial conditions.  */
  if (! reload_completed
      || mode == QImode || mode == HImode
      || ! nonimmediate_operand (dest, mode)
      || ! general_operand (src, mode))
    abort ();

  /* This case is not supported below, and shouldn't be generated.  */
  if (GET_CODE (dest) == MEM
      && GET_CODE (src) == MEM)
    abort ();

  /* This case is very very bad after reload, so trap it now.  */
  if (GET_CODE (dest) == SUBREG
      || GET_CODE (src) == SUBREG)
    abort ();

  /* The general idea is to copy by words, offsetting the source and
     destination.  Normally the least-significant word will be copied
     first, but for pre-dec operations it's better to copy the 
     most-significant word first.  Only one operand can be a pre-dec
     or post-inc operand.  

     It's also possible that the copy overlaps so that the direction
     must be reversed.  */
  direction = 1;
  
  if (GET_CODE (dest) == MEM)
    {
      mem_operand = XEXP (dest, 0);
      dest_modifies = side_effects_p (mem_operand);
      if (auto_inc_p (mem_operand))
        auto_inc_reg_rtx = XEXP (mem_operand, 0);
      dest_volatile = MEM_VOLATILE_P (dest);
      if (dest_volatile)
	{
	  dest = copy_rtx (dest);
	  MEM_VOLATILE_P (dest) = 0;
	}
    }
  else if (GET_CODE (src) == MEM)
    {
      mem_operand = XEXP (src, 0);
      src_modifies = side_effects_p (mem_operand);
      if (auto_inc_p (mem_operand))
        auto_inc_reg_rtx = XEXP (mem_operand, 0);
      src_volatile = MEM_VOLATILE_P (src);
      if (src_volatile)
	{
	  src = copy_rtx (src);
	  MEM_VOLATILE_P (src) = 0;
	}
    }
  else
    mem_operand = NULL_RTX;

  if (mem_operand == NULL_RTX)
    {
      if (GET_CODE (src) == REG
	  && GET_CODE (dest) == REG
	  && reg_overlap_mentioned_p (dest, src)
	  && REGNO (dest) > REGNO (src))
	direction = -1;
    }
  else if (GET_CODE (mem_operand) == PRE_DEC
      || (GET_CODE (mem_operand) == PLUS 
	  && GET_CODE (XEXP (mem_operand, 0)) == PRE_DEC))
    direction = -1;
  else if (GET_CODE (src) == MEM
	   && reg_overlap_mentioned_p (dest, src))
    {
      int regno;
      if (GET_CODE (dest) != REG)
	abort ();
      regno = REGNO (dest);
      
      if (! refers_to_regno_p (regno, regno + num_words, mem_operand, 0))
	abort ();
      
      if (refers_to_regno_p (regno, regno + 1, mem_operand, 0))
	direction = -1;
      else if (refers_to_regno_p (regno + num_words - 1, regno + num_words,
				  mem_operand, 0))
	direction = 1;
      else
	/* This means something like
	   (set (reg:DI r0) (mem:DI (reg:HI r1)))
	   which we'd need to support by doing the set of the second word
	   last.  */
	abort ();
    }

  end = direction < 0 ? -1 : num_words;
  for (i = direction < 0 ? num_words - 1 : 0; i != end; i += direction)
    {
      rtx w_src, w_dest, insn;

      if (src_modifies)
	w_src = gen_rtx_MEM (word_mode, mem_operand);
      else
	w_src = simplify_gen_subreg (word_mode, src, mode, i * UNITS_PER_WORD);
      if (src_volatile)
	MEM_VOLATILE_P (w_src) = 1;
      if (dest_modifies)
	w_dest = gen_rtx_MEM (word_mode, mem_operand);
      else
	w_dest = simplify_gen_subreg (word_mode, dest, mode, 
				      i * UNITS_PER_WORD);
      if (dest_volatile)
	MEM_VOLATILE_P (w_dest) = 1;
      
      /* The simplify_subreg calls must always be able to simplify.  */
      if (GET_CODE (w_src) == SUBREG
	  || GET_CODE (w_dest) == SUBREG)
	abort ();
      
      insn = emit_insn (gen_rtx_SET (VOIDmode, w_dest, w_src));
      if (auto_inc_reg_rtx)
        REG_NOTES (insn) = alloc_EXPR_LIST (REG_INC,
                                            auto_inc_reg_rtx,
					    REG_NOTES (insn));
    }
}

/* Expander for the 'move' patterns.  Emit insns to copy a value of
   mode MODE from SRC to DEST.  */

void 
xstormy16_expand_move (mode, dest, src)
     enum machine_mode mode;
     rtx dest;
     rtx src;
{
  /* There are only limited immediate-to-memory move instructions.  */
  if (! reload_in_progress
      && ! reload_completed
      && GET_CODE (dest) == MEM
      && (GET_CODE (XEXP (dest, 0)) != CONST_INT
	  || ! xstormy16_legitimate_address_p (mode, XEXP (dest, 0), 0))
      && GET_CODE (src) != REG
      && GET_CODE (src) != SUBREG)
    src = copy_to_mode_reg (mode, src);

  /* Don't emit something we would immediately split.  */
  if (reload_completed
      && mode != HImode && mode != QImode)
    {
      xstormy16_split_move (mode, dest, src);
      return;
    }
  
  emit_insn (gen_rtx_SET (VOIDmode, dest, src));
}


/* Stack Layout:

   The stack is laid out as follows:

SP->
FP->	Local variables
	Register save area (up to 4 words)
	Argument register save area for stdarg (NUM_ARGUMENT_REGISTERS words)

AP->	Return address (two words)
	9th procedure parameter word
	10th procedure parameter word
	...
	last procedure parameter word

  The frame pointer location is tuned to make it most likely that all
  parameters and local variables can be accessed using a load-indexed
  instruction.  */

/* A structure to describe the layout.  */
struct xstormy16_stack_layout
{
  /* Size of the topmost three items on the stack.  */
  int locals_size;
  int register_save_size;
  int stdarg_save_size;
  /* Sum of the above items.  */
  int frame_size;
  /* Various offsets.  */
  int first_local_minus_ap;
  int sp_minus_fp;
  int fp_minus_ap;
};

/* Does REGNO need to be saved?  */
#define REG_NEEDS_SAVE(REGNUM, IFUN)					\
  ((regs_ever_live[REGNUM] && ! call_used_regs[REGNUM])			\
   || (IFUN && ! fixed_regs[REGNUM] && call_used_regs[REGNUM]		\
       && (regs_ever_live[REGNUM] || ! current_function_is_leaf)))

/* Compute the stack layout.  */
struct xstormy16_stack_layout 
xstormy16_compute_stack_layout ()
{
  struct xstormy16_stack_layout layout;
  int regno;
  const int ifun = xstormy16_interrupt_function_p ();

  layout.locals_size = get_frame_size ();
  
  layout.register_save_size = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (REG_NEEDS_SAVE (regno, ifun))
      layout.register_save_size += UNITS_PER_WORD;
  
  if (current_function_varargs || current_function_stdarg)
    layout.stdarg_save_size = NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD;
  else
    layout.stdarg_save_size = 0;
  
  layout.frame_size = (layout.locals_size 
		       + layout.register_save_size 
		       + layout.stdarg_save_size);
  
  if (current_function_args_size <= 2048 && current_function_args_size != -1)
    {
      if (layout.frame_size + INCOMING_FRAME_SP_OFFSET 
	  + current_function_args_size <= 2048)
	layout.fp_minus_ap = layout.frame_size + INCOMING_FRAME_SP_OFFSET;
      else
	layout.fp_minus_ap = 2048 - current_function_args_size;
    }
  else
    layout.fp_minus_ap = (layout.stdarg_save_size 
			  + layout.register_save_size
			  + INCOMING_FRAME_SP_OFFSET);
  layout.sp_minus_fp = (layout.frame_size + INCOMING_FRAME_SP_OFFSET 
			- layout.fp_minus_ap);
  layout.first_local_minus_ap = layout.sp_minus_fp - layout.locals_size;
  return layout;
}

/* Determine how all the special registers get eliminated.  */
int
xstormy16_initial_elimination_offset (from, to)
     int from, to;
{
  struct xstormy16_stack_layout layout;
  int result;
  
  layout = xstormy16_compute_stack_layout ();

  if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    result = layout.sp_minus_fp - layout.locals_size;
  else if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    result = -layout.locals_size;
  else if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    result = -layout.fp_minus_ap;
  else if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    result = -(layout.sp_minus_fp + layout.fp_minus_ap);
  else
    abort ();

  return result;
}

static rtx
emit_addhi3_postreload (dest, src0, src1)
     rtx dest;
     rtx src0;
     rtx src1;
{
  rtx set, clobber, insn;
  
  set = gen_rtx_SET (VOIDmode, dest, gen_rtx_PLUS (HImode, src0, src1));
  clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (BImode, 16));
  insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
  return insn;
}

/* Called after register allocation to add any instructions needed for
   the prologue.  Using a prologue insn is favored compared to putting
   all of the instructions in the TARGET_ASM_FUNCTION_PROLOGUE macro,
   since it allows the scheduler to intermix instructions with the
   saves of the caller saved registers.  In some cases, it might be
   necessary to emit a barrier instruction as the last insn to prevent
   such scheduling.

   Also any insns generated here should have RTX_FRAME_RELATED_P(insn) = 1
   so that the debug info generation code can handle them properly.  */
void
xstormy16_expand_prologue ()
{
  struct xstormy16_stack_layout layout;
  int regno;
  rtx insn;
  rtx mem_push_rtx;
  rtx mem_fake_push_rtx;
  const int ifun = xstormy16_interrupt_function_p ();
  
  mem_push_rtx = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
  mem_push_rtx = gen_rtx_MEM (HImode, mem_push_rtx);
  mem_fake_push_rtx = gen_rtx_PRE_INC (Pmode, stack_pointer_rtx);
  mem_fake_push_rtx = gen_rtx_MEM (HImode, mem_fake_push_rtx);
    
  layout = xstormy16_compute_stack_layout ();

  /* Save the argument registers if necessary.  */
  if (layout.stdarg_save_size)
    for (regno = FIRST_ARGUMENT_REGISTER; 
	 regno < FIRST_ARGUMENT_REGISTER + NUM_ARGUMENT_REGISTERS;
	 regno++)
      {
	rtx reg = gen_rtx_REG (HImode, regno);
	insn = emit_move_insn (mem_push_rtx, reg);
	RTX_FRAME_RELATED_P (insn) = 1;
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					      gen_rtx_SET (VOIDmode,
							   mem_fake_push_rtx,
							   reg),
					      REG_NOTES (insn));
      }
  
  /* Push each of the registers to save.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (REG_NEEDS_SAVE (regno, ifun))
      {
	rtx reg = gen_rtx_REG (HImode, regno);
	insn = emit_move_insn (mem_push_rtx, reg);
	RTX_FRAME_RELATED_P (insn) = 1;
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					      gen_rtx_SET (VOIDmode,
							   mem_fake_push_rtx,
							   reg),
					      REG_NOTES (insn));
      }

  /* It's just possible that the SP here might be what we need for
     the new FP...  */
  if (frame_pointer_needed && layout.sp_minus_fp == layout.locals_size)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Allocate space for local variables.  */
  if (layout.locals_size)
    {
      insn = emit_addhi3_postreload (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (layout.locals_size));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Set up the frame pointer, if required.  */
  if (frame_pointer_needed && layout.sp_minus_fp != layout.locals_size)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
      if (layout.sp_minus_fp)
	{
	  insn = emit_addhi3_postreload (hard_frame_pointer_rtx,
					 hard_frame_pointer_rtx,
					 GEN_INT (-layout.sp_minus_fp));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Do we need an epilogue at all?  */
int
direct_return ()
{
  return (reload_completed 
	  && xstormy16_compute_stack_layout ().frame_size == 0);
}

/* Called after register allocation to add any instructions needed for
   the epilogue.  Using an epilogue insn is favored compared to putting
   all of the instructions in the TARGET_ASM_FUNCTION_PROLOGUE macro,
   since it allows the scheduler to intermix instructions with the
   saves of the caller saved registers.  In some cases, it might be
   necessary to emit a barrier instruction as the last insn to prevent
   such scheduling.  */

void
xstormy16_expand_epilogue ()
{
  struct xstormy16_stack_layout layout;
  rtx mem_pop_rtx;
  int regno;
  const int ifun = xstormy16_interrupt_function_p ();
  
  mem_pop_rtx = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
  mem_pop_rtx = gen_rtx_MEM (HImode, mem_pop_rtx);
  
  layout = xstormy16_compute_stack_layout ();

  /* Pop the stack for the locals.  */
  if (layout.locals_size)
    {
      if (frame_pointer_needed && layout.sp_minus_fp == layout.locals_size)
	emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      else
	emit_addhi3_postreload (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (- layout.locals_size));
    }

  /* Restore any call-saved registers.  */
  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
    if (REG_NEEDS_SAVE (regno, ifun))
      emit_move_insn (gen_rtx_REG (HImode, regno), mem_pop_rtx);
  
  /* Pop the stack for the stdarg save area.  */
  if (layout.stdarg_save_size)
    emit_addhi3_postreload (stack_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (- layout.stdarg_save_size));

  /* Return.  */
  if (ifun)
    emit_jump_insn (gen_return_internal_interrupt ());
  else
    emit_jump_insn (gen_return_internal ());
}

int
xstormy16_epilogue_uses (regno)
     int regno;
{
  if (reload_completed && call_used_regs[regno])
    {
      const int ifun = xstormy16_interrupt_function_p ();
      return REG_NEEDS_SAVE (regno, ifun);
    }
  return 0;
}

/* Return an updated summarizer variable CUM to advance past an
   argument in the argument list.  The values MODE, TYPE and NAMED
   describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with
   `FUNCTION_ARG', etc.

   This function need not do anything if the argument in question was
   passed on the stack.  The compiler knows how to track the amount of
   stack space used for arguments without any special help.  However,
   it makes life easier for xstormy16_build_va_list if it does update
   the word count.  */
CUMULATIVE_ARGS
xstormy16_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  /* If an argument would otherwise be passed partially in registers,
     and partially on the stack, the whole of it is passed on the
     stack.  */
  if (cum < NUM_ARGUMENT_REGISTERS
      && cum + XSTORMY16_WORD_SIZE (type, mode) > NUM_ARGUMENT_REGISTERS)
    cum = NUM_ARGUMENT_REGISTERS;
  
  cum += XSTORMY16_WORD_SIZE (type, mode);
  
  return cum;
}

/* Do any needed setup for a variadic function.  CUM has not been updated
   for the last named argument which has type TYPE and mode MODE.  */
void
xstormy16_setup_incoming_varargs (cum, int_mode, type, pretend_size)
     CUMULATIVE_ARGS cum ATTRIBUTE_UNUSED;
     int             int_mode ATTRIBUTE_UNUSED;
     tree            type ATTRIBUTE_UNUSED;
     int *           pretend_size ATTRIBUTE_UNUSED;
{
}

/* Build the va_list type.

   For this chip, va_list is a record containing a counter and a pointer.
   The counter is of type 'int' and indicates how many bytes
   have been used to date.  The pointer indicates the stack position
   for arguments that have not been passed in registers.  
   To keep the layout nice, the pointer is first in the structure.  */

tree
xstormy16_build_va_list ()
{
  tree f_1, f_2, record, type_decl;

  record = make_lang_type (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_1 = build_decl (FIELD_DECL, get_identifier ("base"),
		      ptr_type_node);
  f_2 = build_decl (FIELD_DECL, get_identifier ("count"), 
		      unsigned_type_node);

  DECL_FIELD_CONTEXT (f_1) = record;
  DECL_FIELD_CONTEXT (f_2) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_1;
  TREE_CHAIN (f_1) = f_2;

  layout_type (record);

  return record;
}

/* Implement the stdarg/varargs va_start macro.  STDARG_P is non-zero if this
   is stdarg.h instead of varargs.h.  VALIST is the tree of the va_list
   variable to initialize.  NEXTARG is the machine independent notion of the
   'next' argument after the variable arguments.  */
void
xstormy16_expand_builtin_va_start (stdarg_p, valist, nextarg)
     int stdarg_p ATTRIBUTE_UNUSED;
     tree valist;
     rtx nextarg ATTRIBUTE_UNUSED;
{
  tree f_base, f_count;
  tree base, count;
  tree t;

  if (xstormy16_interrupt_function_p ())
    error ("cannot use va_start in interrupt function");
  
  f_base = TYPE_FIELDS (va_list_type_node);
  f_count = TREE_CHAIN (f_base);
  
  base = build (COMPONENT_REF, TREE_TYPE (f_base), valist, f_base);
  count = build (COMPONENT_REF, TREE_TYPE (f_count), valist, f_count);

  t = make_tree (TREE_TYPE (base), virtual_incoming_args_rtx);
  t = build (PLUS_EXPR, TREE_TYPE (base), t, 
	     build_int_2 (INCOMING_FRAME_SP_OFFSET, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (base), base, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build (MODIFY_EXPR, TREE_TYPE (count), count, 
	     build_int_2 (current_function_args_info * UNITS_PER_WORD, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement the stdarg/varargs va_arg macro.  VALIST is the variable
   of type va_list as a tree, TYPE is the type passed to va_arg.
   Note:  This algorithm is documented in stormy-abi.  */
   
rtx
xstormy16_expand_builtin_va_arg (valist, type)
     tree valist;
     tree type;
{
  tree f_base, f_count;
  tree base, count;
  rtx count_rtx, addr_rtx, r;
  rtx lab_gotaddr, lab_fromstack;
  tree t;
  int size, size_of_reg_args;
  tree size_tree, count_plus_size;
  rtx count_plus_size_rtx;
  
  f_base = TYPE_FIELDS (va_list_type_node);
  f_count = TREE_CHAIN (f_base);
  
  base = build (COMPONENT_REF, TREE_TYPE (f_base), valist, f_base);
  count = build (COMPONENT_REF, TREE_TYPE (f_count), valist, f_count);

  size = PUSH_ROUNDING (int_size_in_bytes (type));
  size_tree = round_up (size_in_bytes (type), UNITS_PER_WORD);
  
  size_of_reg_args = NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD;

  count_rtx = expand_expr (count, NULL_RTX, HImode, EXPAND_NORMAL);
  lab_gotaddr = gen_label_rtx ();
  lab_fromstack = gen_label_rtx ();
  addr_rtx = gen_reg_rtx (Pmode);

  count_plus_size = build (PLUS_EXPR, TREE_TYPE (count), count, size_tree);
  count_plus_size_rtx = expand_expr (count_plus_size, NULL_RTX, HImode, EXPAND_NORMAL);
  emit_cmp_and_jump_insns (count_plus_size_rtx, GEN_INT (size_of_reg_args),
			   GTU, const1_rtx, HImode, 1, lab_fromstack);
  
  t = build (PLUS_EXPR, ptr_type_node, base, count);
  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);

  emit_jump_insn (gen_jump (lab_gotaddr));
  emit_barrier ();
  emit_label (lab_fromstack);
  
  /* Arguments larger than a word might need to skip over some
     registers, since arguments are either passed entirely in
     registers or entirely on the stack.  */
  if (size > 2 || size < 0)
    {
      rtx lab_notransition = gen_label_rtx ();
      emit_cmp_and_jump_insns (count_rtx, GEN_INT (NUM_ARGUMENT_REGISTERS 
						   * UNITS_PER_WORD),
			       GEU, const1_rtx, HImode, 1, lab_notransition);
      
      t = build (MODIFY_EXPR, TREE_TYPE (count), count, 
		 build_int_2 (NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD, 0));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
      
      emit_label (lab_notransition);
    }

  t = build (PLUS_EXPR, sizetype, size_tree,
	     build_int_2 ((- NUM_ARGUMENT_REGISTERS * UNITS_PER_WORD
			   + INCOMING_FRAME_SP_OFFSET),
			  -1));
  t = build (PLUS_EXPR, TREE_TYPE (count), count, fold (t));
  t = build (MINUS_EXPR, TREE_TYPE (base), base, t);
  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);
	     
  emit_label (lab_gotaddr);

  count_plus_size = build (PLUS_EXPR, TREE_TYPE (count), count, size_tree);
  t = build (MODIFY_EXPR, TREE_TYPE (count), count, count_plus_size);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return addr_rtx;
}

/* Initialize the variable parts of a trampoline.  ADDR is an RTX for
   the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain
   value that should be passed to the function when it is called.  */
void
xstormy16_initialize_trampoline (addr, fnaddr, static_chain)
     rtx addr;
     rtx fnaddr;
     rtx static_chain;
{
  rtx reg_addr = gen_reg_rtx (Pmode);
  rtx temp = gen_reg_rtx (HImode);
  rtx reg_fnaddr = gen_reg_rtx (HImode);
  rtx reg_addr_mem;

  reg_addr_mem = gen_rtx_MEM (HImode, reg_addr);
    
  emit_move_insn (reg_addr, addr);
  emit_move_insn (temp, GEN_INT (0x3130 | STATIC_CHAIN_REGNUM));
  emit_move_insn (reg_addr_mem, temp);
  emit_insn (gen_addhi3 (reg_addr, reg_addr, const2_rtx));
  emit_move_insn (temp, static_chain);
  emit_move_insn (reg_addr_mem, temp);
  emit_insn (gen_addhi3 (reg_addr, reg_addr, const2_rtx));
  emit_move_insn (reg_fnaddr, fnaddr);
  emit_move_insn (temp, reg_fnaddr);
  emit_insn (gen_andhi3 (temp, temp, GEN_INT (0xFF)));
  emit_insn (gen_iorhi3 (temp, temp, GEN_INT (0x0200)));
  emit_move_insn (reg_addr_mem, temp);
  emit_insn (gen_addhi3 (reg_addr, reg_addr, const2_rtx));
  emit_insn (gen_lshrhi3 (reg_fnaddr, reg_fnaddr, GEN_INT (8)));
  emit_move_insn (reg_addr_mem, reg_fnaddr);
}

/* Create an RTX representing the place where a function returns a
   value of data type VALTYPE.  VALTYPE is a tree node representing a
   data type.  Write `TYPE_MODE (VALTYPE)' to get the machine mode
   used to represent that type.  On many machines, only the mode is
   relevant.  (Actually, on most machines, scalar values are returned
   in the same place regardless of mode).

   If `PROMOTE_FUNCTION_RETURN' is defined, you must apply the same promotion
   rules specified in `PROMOTE_MODE' if VALTYPE is a scalar type.

   If the precise function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This makes it
   possible to use a different value-returning convention for specific
   functions when all their calls are known.

   `FUNCTION_VALUE' is not used for return vales with aggregate data types,
   because these are returned in another way.  See `STRUCT_VALUE_REGNUM' and
   related macros.  */
rtx
xstormy16_function_value (valtype, func)
     tree valtype;
     tree func ATTRIBUTE_UNUSED;
{
  enum machine_mode mode;
  mode = TYPE_MODE (valtype);
  PROMOTE_MODE (mode, 0, valtype);
  return gen_rtx_REG (mode, RETURN_VALUE_REGNUM);
}

/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  The
   thunk acts as a wrapper around a virtual function, adjusting the implicit
   object parameter before handing control off to the real function.

   First, emit code to add the integer DELTA to the location that contains the
   incoming first argument.  Assume that this argument contains a pointer, and
   is the one used to pass the `this' pointer in C++.  This is the incoming
   argument *before* the function prologue, e.g. `%o0' on a sparc.  The
   addition must preserve the values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
   the return address.  Hence returning from FUNCTION will return to whoever
   called the current `thunk'.

   The effect must be as if @var{function} had been called directly
   with the adjusted first argument.  This macro is responsible for
   emitting all of the code for a thunk function;
   TARGET_ASM_FUNCTION_PROLOGUE and TARGET_ASM_FUNCTION_EPILOGUE are
   not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
   extracted from it.)  It might possibly be useful on some targets, but
   probably not.  */

void
xstormy16_asm_output_mi_thunk (file, thunk_fndecl, delta, function)
     FILE *file;
     tree thunk_fndecl ATTRIBUTE_UNUSED;
     int delta;
     tree function;
{
  int regnum = FIRST_ARGUMENT_REGISTER;
  
  /* There might be a hidden first argument for a returned structure.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function))))
    regnum += 1;
  
  fprintf (file, "\tadd %s,#0x%x\n", reg_names[regnum], (delta) & 0xFFFF);
  fputs ("\tjmpf ", file);
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  putc ('\n', file);
}

/* Mark functions with SYMBOL_REF_FLAG.  */

void
xstormy16_encode_section_info (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
}

/* Output constructors and destructors.  Just like 
   default_named_section_asm_out_* but don't set the sections writable.  */
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR xstormy16_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR xstormy16_asm_out_destructor

static void
xstormy16_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority;
{
  const char *section = ".dtors";
  char buf[16];

  /* ??? This only works reliably with the GNU linker.   */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".dtors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  named_section_flags (section, 0);
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

static void
xstormy16_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority;
{
  const char *section = ".ctors";
  char buf[16];

  /* ??? This only works reliably with the GNU linker.   */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".ctors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  named_section_flags (section, 0);
  assemble_align (POINTER_SIZE);
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

/* Print a memory address as an operand to reference that memory location.  */
void
xstormy16_print_operand_address (file, address)
     FILE * file;
     rtx    address;
{
  HOST_WIDE_INT offset;
  int pre_dec, post_inc;

  /* There are a few easy cases.  */
  if (GET_CODE (address) == CONST_INT)
    {
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (address) & 0xFFFF);
      return;
    }
  
  if (CONSTANT_P (address) || GET_CODE (address) == CODE_LABEL)
    {
      output_addr_const (file, address);
      return;
    }
  
  /* Otherwise, it's hopefully something of the form 
     (plus:HI (pre_dec:HI (reg:HI ...)) (const_int ...))
  */

  if (GET_CODE (address) == PLUS)
    {
      if (GET_CODE (XEXP (address, 1)) != CONST_INT)
	abort ();
      offset = INTVAL (XEXP (address, 1));
      address = XEXP (address, 0);
    }
  else
    offset = 0;

  pre_dec = (GET_CODE (address) == PRE_DEC);
  post_inc = (GET_CODE (address) == POST_INC);
  if (pre_dec || post_inc)
    address = XEXP (address, 0);
  
  if (GET_CODE (address) != REG)
    abort ();

  fputc ('(', file);
  if (pre_dec)
    fputs ("--", file);
  fputs (reg_names [REGNO (address)], file);
  if (post_inc)
    fputs ("++", file);
  if (offset != 0)
    {
      fputc (',', file);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, offset);
    }
  fputc (')', file);
}

/* Print an operand to an assembler instruction.  */
void
xstormy16_print_operand (file, x, code)
     FILE * file;
     rtx    x;
     int    code;
{
  switch (code)
    {
    case 'B':
	/* There is either one bit set, or one bit clear, in X.
	   Print it preceded by '#'.  */
      {
	HOST_WIDE_INT xx = 1;
	HOST_WIDE_INT l;

	if (GET_CODE (x) == CONST_INT)
	  xx = INTVAL (x);
	else
	  output_operand_lossage ("`B' operand is not constant");
	
	l = exact_log2 (xx);
	if (l == -1)
	  l = exact_log2 (~xx);
	if (l == -1)
	  output_operand_lossage ("`B' operand has multiple bits set");
	
	fputs (IMMEDIATE_PREFIX, file);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, l);
	return;
      }

    case 'C':
      /* Print the symbol without a surrounding @fptr().  */
      if (GET_CODE (x) == SYMBOL_REF)
	assemble_name (file, XSTR (x, 0));
      else if (GET_CODE (x) == LABEL_REF)
	output_asm_label (x);
      else
	xstormy16_print_operand_address (file, x);
      return;

    case 'o':
    case 'O':
      /* Print the immediate operand less one, preceded by '#'.  
         For 'O', negate it first.  */
      {
	HOST_WIDE_INT xx = 0;
	
	if (GET_CODE (x) == CONST_INT)
	  xx = INTVAL (x);
	else
	  output_operand_lossage ("`o' operand is not constant");
	
	if (code == 'O')
	  xx = -xx;
	
	fputs (IMMEDIATE_PREFIX, file);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, xx - 1);
	return;
      }

    case 0:
      /* Handled below.  */
      break;
      
    default:
      output_operand_lossage ("xstormy16_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      xstormy16_print_operand_address (file, XEXP (x, 0));
      break;

    default:
      /* Some kind of constant or label; an immediate operand,
         so prefix it with '#' for the assembler.  */
      fputs (IMMEDIATE_PREFIX, file);
      output_addr_const (file, x);
      break;
    }

  return;
}


/* Expander for the `casesi' pattern.
   INDEX is the index of the switch statement.
   LOWER_BOUND is a CONST_INT that is the value of INDEX corresponding
     to the first table entry.
   RANGE is the number of table entries.
   TABLE is an ADDR_VEC that is the jump table.
   DEFAULT_LABEL is the address to branch to if INDEX is outside the
     range LOWER_BOUND to LOWER_BOUND+RANGE-1.
*/

void 
xstormy16_expand_casesi (index, lower_bound, range, table, default_label)
     rtx index;
     rtx lower_bound;
     rtx range;
     rtx table;
     rtx default_label;
{
  HOST_WIDE_INT range_i = INTVAL (range);
  rtx int_index;

  /* This code uses 'br', so it can deal only with tables of size up to
     8192 entries.  */
  if (range_i >= 8192)
    sorry ("switch statement of size %lu entries too large", 
	   (unsigned long) range_i);

  index = expand_binop (SImode, sub_optab, index, lower_bound, NULL_RTX, 0,
			OPTAB_LIB_WIDEN);
  emit_cmp_and_jump_insns (index, range, GTU, NULL_RTX, SImode, 1,
			   default_label);
  int_index = gen_lowpart_common (HImode, index);
  emit_insn (gen_ashlhi3 (int_index, int_index, GEN_INT (2)));
  emit_jump_insn (gen_tablejump_pcrel (int_index, table));
}

/* Output an ADDR_VEC.  It is output as a sequence of 'jmpf'
   instructions, without label or alignment or any other special
   constructs.  We know that the previous instruction will be the
   `tablejump_pcrel' output above.

   TODO: it might be nice to output 'br' instructions if they could
   all reach.  */

void
xstormy16_output_addr_vec (file, label, table)
     FILE *file;
     rtx label ATTRIBUTE_UNUSED;
     rtx table;
{ 
  int vlen, idx;
  
  function_section (current_function_decl);

  vlen = XVECLEN (table, 0);
  for (idx = 0; idx < vlen; idx++)
    {
      fputs ("\tjmpf ", file);
      output_asm_label (XEXP (XVECEXP (table, 0, idx), 0));
      fputc ('\n', file);
    }
}


/* Expander for the `call' patterns.
   INDEX is the index of the switch statement.
   LOWER_BOUND is a CONST_INT that is the value of INDEX corresponding
     to the first table entry.
   RANGE is the number of table entries.
   TABLE is an ADDR_VEC that is the jump table.
   DEFAULT_LABEL is the address to branch to if INDEX is outside the
     range LOWER_BOUND to LOWER_BOUND+RANGE-1.
*/

void 
xstormy16_expand_call (retval, dest, counter)
     rtx retval;
     rtx dest;
     rtx counter;
{
  rtx call, temp;
  enum machine_mode mode;

  if (GET_CODE (dest) != MEM)
    abort ();
  dest = XEXP (dest, 0);

  if (! CONSTANT_P (dest)
      && GET_CODE (dest) != REG)
    dest = force_reg (Pmode, dest);
  
  if (retval == NULL)
    mode = VOIDmode;
  else
    mode = GET_MODE (retval);

  call = gen_rtx_CALL (mode, gen_rtx_MEM (FUNCTION_MODE, dest),
		       counter);
  if (retval)
    call = gen_rtx_SET (VOIDmode, retval, call);
  
  if (! CONSTANT_P (dest))
    {
      temp = gen_reg_rtx (HImode);
      emit_move_insn (temp, const0_rtx);
    }
  else
    temp = const0_rtx;
  
  call = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, call, 
						gen_rtx_USE (VOIDmode, temp)));
  emit_call_insn (call);
}

/* Expanders for multiword computational operations.  */

/* Expander for arithmetic operations; emit insns to compute

   (set DEST (CODE:MODE SRC0 SRC1))
   
   using CARRY as a temporary.  When CODE is COMPARE, a branch
   template is generated (this saves duplicating code in
   xstormy16_split_cbranch).  */

void 
xstormy16_expand_arith (mode, code, dest, src0, src1, carry)
     enum machine_mode mode;
     enum rtx_code code;
     rtx dest;
     rtx src0;
     rtx src1;
     rtx carry;
{
  int num_words = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;
  int i;
  int firstloop = 1;

  if (code == NEG)
    {
      rtx zero_reg = gen_reg_rtx (word_mode);
      emit_move_insn (zero_reg, src0);
      src0 = zero_reg;
    }
  
  for (i = 0; i < num_words; i++)
    {
      rtx w_src0, w_src1, w_dest;
      rtx insn;
      
      if (code == NEG)
	w_src0 = src0;
      else
	w_src0 = simplify_gen_subreg (word_mode, src0, mode, 
				      i * UNITS_PER_WORD);
      w_src1 = simplify_gen_subreg (word_mode, src1, mode, i * UNITS_PER_WORD);
      w_dest = simplify_gen_subreg (word_mode, dest, mode, i * UNITS_PER_WORD);

      switch (code)
	{
	case PLUS:
	  if (firstloop
	      && GET_CODE (w_src1) == CONST_INT && INTVAL (w_src1) == 0)
	    continue;
	  
	  if (firstloop)
	    insn = gen_addchi4 (w_dest, w_src0, w_src1, carry);
	  else
	    insn = gen_addchi5 (w_dest, w_src0, w_src1, carry, carry);
	  break;

	case NEG:
	case MINUS:
	case COMPARE:
	  if (code == COMPARE && i == num_words - 1)
	    {
	      rtx branch, sub, clobber, sub_1;
	      
	      sub_1 = gen_rtx_MINUS (HImode, w_src0, 
				     gen_rtx_ZERO_EXTEND (HImode, carry));
	      sub = gen_rtx_SET (VOIDmode, w_dest,
				 gen_rtx_MINUS (HImode, sub_1, w_src1));
	      clobber = gen_rtx_CLOBBER (VOIDmode, carry);
	      branch = gen_rtx_SET (VOIDmode, pc_rtx,
				    gen_rtx_IF_THEN_ELSE (VOIDmode,
							  gen_rtx_EQ (HImode,
								      sub_1,
								      w_src1),
							  pc_rtx,
							  pc_rtx));
	      insn = gen_rtx_PARALLEL (VOIDmode,
				       gen_rtvec (3, branch, sub, clobber));
	    }
	  else if (firstloop
		   && code != COMPARE
		   && GET_CODE (w_src1) == CONST_INT && INTVAL (w_src1) == 0)
	    continue;
	  else if (firstloop)
	    insn = gen_subchi4 (w_dest, w_src0, w_src1, carry);
	  else
	    insn = gen_subchi5 (w_dest, w_src0, w_src1, carry, carry);
	  break;

	case IOR:
	case XOR:
	case AND:
	  if (GET_CODE (w_src1) == CONST_INT 
	      && INTVAL (w_src1) == -(code == AND))
	    continue;
	  
	  insn = gen_rtx_SET (VOIDmode, w_dest, gen_rtx (code, mode,
							 w_src0, w_src1));
	  break;

	case NOT:
	  insn = gen_rtx_SET (VOIDmode, w_dest, gen_rtx_NOT (mode, w_src0));
	  break;

	default:
	  abort ();
	}
      
      firstloop = 0;
      emit (insn);
    }
}

/* Return 1 if OP is a shift operator.  */

int
shift_operator (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  return (code == ASHIFT
	  || code == ASHIFTRT
	  || code == LSHIFTRT);
}

/* The shift operations are split at output time for constant values;
   variable-width shifts get handed off to a library routine.  

   Generate an output string to do (set X (CODE:MODE X SIZE_R))
   SIZE_R will be a CONST_INT, X will be a hard register.  */

const char * 
xstormy16_output_shift (mode, code, x, size_r, temp)
     enum machine_mode mode;
     enum rtx_code code;
     rtx x;
     rtx size_r;
     rtx temp;
{
  HOST_WIDE_INT size;
  const char *r0, *r1, *rt;
  static char r[64];

  if (GET_CODE (size_r) != CONST_INT
      || GET_CODE (x) != REG
      || mode != SImode)
    abort ();
  size = INTVAL (size_r) & (GET_MODE_BITSIZE (mode) - 1);

  if (size == 0)
    return "";

  r0 = reg_names [REGNO (x)];
  r1 = reg_names [REGNO (x) + 1];

  /* For shifts of size 1, we can use the rotate instructions.  */
  if (size == 1)
    {
      switch (code)
	{
	case ASHIFT:
	  sprintf (r, "shl %s,#1 | rlc %s,#1", r0, r1);
	  break;
	case ASHIFTRT:
	  sprintf (r, "asr %s,#1 | rrc %s,#1", r1, r0);
	  break;
	case LSHIFTRT:
	  sprintf (r, "shr %s,#1 | rrc %s,#1", r1, r0);
	  break;
	default:
	  abort ();
	}
      return r;
    }
  
  /* For large shifts, there are easy special cases.  */
  if (size == 16)
    {
      switch (code)
	{
	case ASHIFT:
	  sprintf (r, "mov %s,%s | mov %s,#0", r1, r0, r0);
	  break;
	case ASHIFTRT:
	  sprintf (r, "mov %s,%s | asr %s,#15", r0, r1, r1);
	  break;
	case LSHIFTRT:
	  sprintf (r, "mov %s,%s | mov %s,#0", r0, r1, r1);
	  break;
	default:
	  abort ();
	}
      return r;
    }
  if (size > 16)
    {
      switch (code)
	{
	case ASHIFT:
	  sprintf (r, "mov %s,%s | mov %s,#0 | shl %s,#%d", 
		   r1, r0, r0, r1, (int) size - 16);
	  break;
	case ASHIFTRT:
	  sprintf (r, "mov %s,%s | asr %s,#15 | asr %s,#%d", 
		   r0, r1, r1, r0, (int) size - 16);
	  break;
	case LSHIFTRT:
	  sprintf (r, "mov %s,%s | mov %s,#0 | shr %s,#%d", 
		   r0, r1, r1, r0, (int) size - 16);
	  break;
	default:
	  abort ();
	}
      return r;
    }

  /* For the rest, we have to do more work.  In particular, we
     need a temporary.  */
  rt = reg_names [REGNO (temp)];
  switch (code)
    {
    case ASHIFT:
      sprintf (r, 
	       "mov %s,%s | shl %s,#%d | shl %s,#%d | shr %s,#%d | or %s,%s", 
	       rt, r0, r0, (int) size, r1, (int) size, rt, (int) 16-size,
	       r1, rt);
      break;
    case ASHIFTRT:
      sprintf (r, 
	       "mov %s,%s | asr %s,#%d | shr %s,#%d | shl %s,#%d | or %s,%s", 
	       rt, r1, r1, (int) size, r0, (int) size, rt, (int) 16-size,
	       r0, rt);
      break;
    case LSHIFTRT:
      sprintf (r, 
	       "mov %s,%s | shr %s,#%d | shr %s,#%d | shl %s,#%d | or %s,%s", 
	       rt, r1, r1, (int) size, r0, (int) size, rt, (int) 16-size,
	       r0, rt);
      break;
    default:
      abort ();
    }
  return r;
}

/* Attribute handling.  */

/* Return nonzero if the function is an interrupt function.  */
int
xstormy16_interrupt_function_p ()
{
  tree attributes;
  
  /* The dwarf2 mechanism asks for INCOMING_FRAME_SP_OFFSET before
     any functions are declared, which is demonstrably wrong, but
     it is worked around here.  FIXME.  */
  if (!cfun)
    return 0;

  attributes = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  return lookup_attribute ("interrupt", attributes) != NULL_TREE;
}

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE xstormy16_attribute_table
static tree xstormy16_handle_interrupt_attribute PARAMS ((tree *, tree, tree, int, bool *));
static const struct attribute_spec xstormy16_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "interrupt", 0, 0, false, true,  true,  xstormy16_handle_interrupt_attribute },
  { NULL,        0, 0, false, false, false, NULL }
};

/* Handle an "interrupt" attribute;
   arguments as in struct attribute_spec.handler.  */
static tree
xstormy16_handle_interrupt_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) != FUNCTION_TYPE)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

struct gcc_target targetm = TARGET_INITIALIZER;
