/* Subroutines for insn-output.c for Matsushita MN10300 series
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "function.h"
#include "obstack.h"
#include "toplev.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

/* The size of the callee register save area.  Right now we save everything
   on entry since it costs us nothing in code size.  It does cost us from a
   speed standpoint, so we want to optimize this sooner or later.  */
#define REG_SAVE_BYTES (4 * regs_ever_live[2] \
			+ 4 * regs_ever_live[3] \
		        + 4 * regs_ever_live[6] \
			+ 4 * regs_ever_live[7] \
			+ 16 * (regs_ever_live[14] || regs_ever_live[15] \
				|| regs_ever_live[16] || regs_ever_live[17]))

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

struct gcc_target targetm = TARGET_INITIALIZER;

void
asm_file_start (file)
     FILE *file;
{
  fprintf (file, "#\tGCC For the Matsushita MN10300\n");
  if (optimize)
    fprintf (file, "# -O%d\n", optimize);
  else
    fprintf (file, "\n\n");

  if (TARGET_AM33)
    fprintf (file, "\t.am33\n");
  output_file_directive (file, main_input_filename);
}


/* Print operand X using operand code CODE to assembly language output file
   FILE.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
      case 'b':
      case 'B':
	/* These are normal and reversed branches.  */
	switch (code == 'b' ? GET_CODE (x) : reverse_condition (GET_CODE (x)))
	  {
	  case NE:
	    fprintf (file, "ne");
	    break;
	  case EQ:
	    fprintf (file, "eq");
	    break;
	  case GE:
	    fprintf (file, "ge");
	    break;
	  case GT:
	    fprintf (file, "gt");
	    break;
	  case LE:
	    fprintf (file, "le");
	    break;
	  case LT:
	    fprintf (file, "lt");
	    break;
	  case GEU:
	    fprintf (file, "cc");
	    break;
	  case GTU:
	    fprintf (file, "hi");
	    break;
	  case LEU:
	    fprintf (file, "ls");
	    break;
	  case LTU:
	    fprintf (file, "cs");
	    break;
	  default:
	    abort ();
	  }
	break;
      case 'C':
	/* This is used for the operand to a call instruction;
	   if it's a REG, enclose it in parens, else output
	   the operand normally.  */
	if (GET_CODE (x) == REG)
	  {
	    fputc ('(', file);
	    print_operand (file, x, 0);
	    fputc (')', file);
	  }
	else
	  print_operand (file, x, 0);
	break;
     
      /* These are the least significant word in a 64bit value.  */
      case 'L':
	switch (GET_CODE (x))
	  {
	  case MEM:
	    fputc ('(', file);
	    output_address (XEXP (x, 0));
	    fputc (')', file);
	    break;

	  case REG:
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	    break;

	  case SUBREG:
	    fprintf (file, "%s", reg_names[subreg_regno (x)]);
	    break;

	  case CONST_DOUBLE:
	      {
		long val[2];
		REAL_VALUE_TYPE rv;

		switch (GET_MODE (x))
		  {
		    case DFmode:
		      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
		      REAL_VALUE_TO_TARGET_DOUBLE (rv, val);
		      fprintf (file, "0x%lx", val[0]);
		      break;;
		    case SFmode:
		      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
		      REAL_VALUE_TO_TARGET_SINGLE (rv, val[0]);
		      fprintf (file, "0x%lx", val[0]);
		      break;;
		    case VOIDmode:
		    case DImode:
		      print_operand_address (file,
					     GEN_INT (CONST_DOUBLE_LOW (x)));
		      break;
		    default:
		      break;
		  }
		break;
	      }

	  case CONST_INT:
	    {
	      rtx low, high;
	      split_double (x, &low, &high);
	      fprintf (file, "%ld", (long)INTVAL (low));
	      break;
	    }

	  default:
	    abort ();
	  }
	break;

      /* Similarly, but for the most significant word.  */
      case 'H':
	switch (GET_CODE (x))
	  {
	  case MEM:
	    fputc ('(', file);
	    x = adjust_address (x, SImode, 4);
	    output_address (XEXP (x, 0));
	    fputc (')', file);
	    break;

	  case REG:
	    fprintf (file, "%s", reg_names[REGNO (x) + 1]);
	    break;

	  case SUBREG:
	    fprintf (file, "%s", reg_names[subreg_regno (x) + 1]);
	    break;

	  case CONST_DOUBLE:
	      {
		long val[2];
		REAL_VALUE_TYPE rv;

		switch (GET_MODE (x))
		  {
		    case DFmode:
		      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
		      REAL_VALUE_TO_TARGET_DOUBLE (rv, val);
		      fprintf (file, "0x%lx", val[1]);
		      break;;
		    case SFmode:
		      abort ();
		    case VOIDmode:
		    case DImode:
		      print_operand_address (file, 
					     GEN_INT (CONST_DOUBLE_HIGH (x)));
		      break;
		    default:
		      break;
		  }
		break;
	      }

	  case CONST_INT:
	    {
	      rtx low, high;
	      split_double (x, &low, &high);
	      fprintf (file, "%ld", (long)INTVAL (high));
	      break;
	    }

	  default:
	    abort ();
	  }
	break;

      case 'A':
	fputc ('(', file);
	if (GET_CODE (XEXP (x, 0)) == REG)
	  output_address (gen_rtx_PLUS (SImode, XEXP (x, 0), GEN_INT (0)));
	else
	  output_address (XEXP (x, 0));
	fputc (')', file);
	break;

      case 'N':
	output_address (GEN_INT ((~INTVAL (x)) & 0xff));
	break;

      /* For shift counts.  The hardware ignores the upper bits of
	 any immediate, but the assembler will flag an out of range
	 shift count as an error.  So we mask off the high bits
	 of the immediate here.  */
      case 'S':
	if (GET_CODE (x) == CONST_INT)
	  {
	    fprintf (file, "%d", INTVAL (x) & 0x1f);
	    break;
	  }
	/* FALL THROUGH */

      default:
	switch (GET_CODE (x))
	  {
	  case MEM:
	    fputc ('(', file);
	    output_address (XEXP (x, 0));
	    fputc (')', file);
	    break;

	  case PLUS:
	    output_address (x);
	    break;

	  case REG:
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	    break;

	  case SUBREG:
	    fprintf (file, "%s", reg_names[subreg_regno (x)]);
	    break;

	  /* This will only be single precision....  */
	  case CONST_DOUBLE:
	    {
	      unsigned long val;
	      REAL_VALUE_TYPE rv;

	      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	      REAL_VALUE_TO_TARGET_SINGLE (rv, val);
	      fprintf (file, "0x%lx", val);
	      break;
	    }

	  case CONST_INT:
	  case SYMBOL_REF:
	  case CONST:
	  case LABEL_REF:
	  case CODE_LABEL:
	    print_operand_address (file, x);
	    break;
	  default:
	    abort ();
	  }
	break;
   }
}

/* Output assembly language output for the address ADDR to FILE.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case POST_INC:
      print_operand_address (file, XEXP (addr, 0));
      fputc ('+', file);
      break;
    case REG:
      print_operand (file, addr, 0);
      break;
    case PLUS:
      {
	rtx base, index;
	if (REG_P (XEXP (addr, 0))
	    && REG_OK_FOR_BASE_P (XEXP (addr, 0)))
	  base = XEXP (addr, 0), index = XEXP (addr, 1);
	else if (REG_P (XEXP (addr, 1))
	    && REG_OK_FOR_BASE_P (XEXP (addr, 1)))
	  base = XEXP (addr, 1), index = XEXP (addr, 0);
      	else
	  abort ();
	print_operand (file, index, 0);
	fputc (',', file);
	print_operand (file, base, 0);;
	break;
      }
    case SYMBOL_REF:
      output_addr_const (file, addr);
      break;
    default:
      output_addr_const (file, addr);
      break;
    }
}

/* Print a set of registers in the format required by "movm" and "ret".
   Register K is saved if bit K of MASK is set.  The data and address
   registers can be stored individually, but the extended registers cannot.
   We assume that the mask alread takes that into account.  For instance,
   bits 14 to 17 must have the same value. */

void
mn10300_print_reg_list (file, mask)
     FILE *file;
     int mask;
{
  int need_comma;
  int i;

  need_comma = 0;
  fputc ('[', file);

  for (i = 0; i < FIRST_EXTENDED_REGNUM; i++)
    if ((mask & (1 << i)) != 0)
      {
	if (need_comma)
	  fputc (',', file);
	fputs (reg_names [i], file);
	need_comma = 1;
      }

  if ((mask & 0x3c000) != 0)
    {
      if ((mask & 0x3c000) != 0x3c000)
	abort();
      if (need_comma)
	fputc (',', file);
      fputs ("exreg1", file);
      need_comma = 1;
    }

  fputc (']', file);
}

int
can_use_return_insn ()
{
  /* size includes the fixed stack space needed for function calls.  */
  int size = get_frame_size () + current_function_outgoing_args_size;

  /* And space for the return pointer.  */
  size += current_function_outgoing_args_size ? 4 : 0;

  return (reload_completed
	  && size == 0
	  && !regs_ever_live[2]
	  && !regs_ever_live[3]
	  && !regs_ever_live[6]
	  && !regs_ever_live[7]
	  && !regs_ever_live[14]
	  && !regs_ever_live[15]
	  && !regs_ever_live[16]
	  && !regs_ever_live[17]
	  && !frame_pointer_needed);
}

/* Returns the set of live, callee-saved registers as a bitmask.  The
   callee-saved extended registers cannot be stored individually, so
   all of them will be included in the mask if any one of them is used. */

int
mn10300_get_live_callee_saved_regs ()
{
  int mask;
  int i;

  mask = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i] && ! call_used_regs[i])
      mask |= (1 << i);
  if ((mask & 0x3c000) != 0)
    mask |= 0x3c000;

  return mask;
}

/* Generate an instruction that pushes several registers onto the stack.
   Register K will be saved if bit K in MASK is set.  The function does
   nothing if MASK is zero.

   To be compatible with the "movm" instruction, the lowest-numbered
   register must be stored in the lowest slot.  If MASK is the set
   { R1,...,RN }, where R1...RN are ordered least first, the generated
   instruction will have the form:

       (parallel
         (set (reg:SI 9) (plus:SI (reg:SI 9) (const_int -N*4)))
	 (set (mem:SI (plus:SI (reg:SI 9)
	                       (const_int -1*4)))
	      (reg:SI RN))
	 ...
	 (set (mem:SI (plus:SI (reg:SI 9)
	                       (const_int -N*4)))
	      (reg:SI R1))) */

void
mn10300_gen_multiple_store (mask)
     int mask;
{
  if (mask != 0)
    {
      int i;
      int count;
      rtx par;
      int pari;

      /* Count how many registers need to be saved. */
      count = 0;
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if ((mask & (1 << i)) != 0)
	  count += 1;

      /* We need one PARALLEL element to update the stack pointer and
	 an additional element for each register that is stored. */
      par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count + 1));

      /* Create the instruction that updates the stack pointer. */
      XVECEXP (par, 0, 0)
	= gen_rtx_SET (SImode,
		       stack_pointer_rtx,
		       gen_rtx_PLUS (SImode,
				     stack_pointer_rtx,
				     GEN_INT (-count * 4)));

      /* Create each store. */
      pari = 1;
      for (i = FIRST_PSEUDO_REGISTER - 1; i >= 0; i--)
	if ((mask & (1 << i)) != 0)
	  {
	    rtx address = gen_rtx_PLUS (SImode,
					stack_pointer_rtx,
					GEN_INT (-pari * 4));
	    XVECEXP(par, 0, pari)
	      = gen_rtx_SET (VOIDmode,
			     gen_rtx_MEM (SImode, address),
			     gen_rtx_REG (SImode, i));
	    pari += 1;
	  }

      par = emit_insn (par);
      RTX_FRAME_RELATED_P (par) = 1;
    }
}

void
expand_prologue ()
{
  HOST_WIDE_INT size;

  /* SIZE includes the fixed stack space needed for function calls.  */
  size = get_frame_size () + current_function_outgoing_args_size;
  size += (current_function_outgoing_args_size ? 4 : 0);

  /* If this is an old-style varargs function, then its arguments
     need to be flushed back to the stack.  */
  if (current_function_varargs)
    {
      emit_move_insn (gen_rtx_MEM (SImode,
				   plus_constant (stack_pointer_rtx, 4)),
		      gen_rtx_REG (SImode, 0));
      emit_move_insn (gen_rtx_MEM (SImode,
				   plus_constant (stack_pointer_rtx, 8)),
		      gen_rtx_REG (SImode, 1));
    }

  /* If we use any of the callee-saved registers, save them now. */
  mn10300_gen_multiple_store (mn10300_get_live_callee_saved_regs ());

  /* Now put the frame pointer into the frame pointer register.  */
  if (frame_pointer_needed)
    emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);

  /* Allocate stack for this frame.  */
  if (size)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   GEN_INT (-size)));
}

void
expand_epilogue ()
{
  HOST_WIDE_INT size;

  /* SIZE includes the fixed stack space needed for function calls.  */
  size = get_frame_size () + current_function_outgoing_args_size;
  size += (current_function_outgoing_args_size ? 4 : 0);

  /* Maybe cut back the stack, except for the register save area.

     If the frame pointer exists, then use the frame pointer to
     cut back the stack.

     If the stack size + register save area is more than 255 bytes,
     then the stack must be cut back here since the size + register
     save size is too big for a ret/retf instruction. 

     Else leave it alone, it will be cut back as part of the
     ret/retf instruction, or there wasn't any stack to begin with.

     Under no circumstanes should the register save area be
     deallocated here, that would leave a window where an interrupt
     could occur and trash the register save area.  */
  if (frame_pointer_needed)
    {
      emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
      size = 0;
    }
  else if (size + REG_SAVE_BYTES > 255)
    {
      emit_insn (gen_addsi3 (stack_pointer_rtx,
			     stack_pointer_rtx,
			     GEN_INT (size)));
      size = 0;
    }

  /* Adjust the stack and restore callee-saved registers, if any.  */
  if (size || regs_ever_live[2] || regs_ever_live[3]
      || regs_ever_live[6] || regs_ever_live[7]
      || regs_ever_live[14] || regs_ever_live[15]
      || regs_ever_live[16] || regs_ever_live[17]
      || frame_pointer_needed)
    emit_jump_insn (gen_return_internal_regs
		    (GEN_INT (size + REG_SAVE_BYTES)));
  else
    emit_jump_insn (gen_return_internal ());
}

/* Update the condition code from the insn.  */

void
notice_update_cc (body, insn)
     rtx body;
     rtx insn;
{
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

    case CC_NONE_0HIT:
      /* Insn does not change CC, but the 0'th operand has been changed.  */
      if (cc_status.value1 != 0
	  && reg_overlap_mentioned_p (recog_data.operand[0], cc_status.value1))
	cc_status.value1 = 0;
      break;

    case CC_SET_ZN:
      /* Insn sets the Z,N flags of CC to recog_data.operand[0].
	 V,C are unusable.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_CARRY | CC_OVERFLOW_UNUSABLE;
      cc_status.value1 = recog_data.operand[0];
      break;

    case CC_SET_ZNV:
      /* Insn sets the Z,N,V flags of CC to recog_data.operand[0].
	 C is unusable.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_CARRY;
      cc_status.value1 = recog_data.operand[0];
      break;

    case CC_COMPARE:
      /* The insn is a compare instruction.  */
      CC_STATUS_INIT;
      cc_status.value1 = SET_SRC (body);
      break;

    case CC_INVERT:
      /* The insn is a compare instruction.  */
      CC_STATUS_INIT;
      cc_status.value1 = SET_SRC (body);
      cc_status.flags |= CC_INVERTED;
      break;

    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;
      break;

    default:
      abort ();
    }
}

/* Recognise the PARALLEL rtx generated by mn10300_gen_multiple_store().
   This function is for MATCH_PARALLEL and so assumes OP is known to be
   parallel.  If OP is a multiple store, return a mask indicating which
   registers it saves.  Return 0 otherwise.  */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count;
  int mask;
  int i;
  unsigned int last;
  rtx elt;

  count = XVECLEN (op, 0);
  if (count < 2)
    return 0;

  /* Check that first instruction has the form (set (sp) (plus A B)) */
  elt = XVECEXP (op, 0, 0);
  if (GET_CODE (elt) != SET
      || GET_CODE (SET_DEST (elt)) != REG
      || REGNO (SET_DEST (elt)) != STACK_POINTER_REGNUM
      || GET_CODE (SET_SRC (elt)) != PLUS)
    return 0;

  /* Check that A is the stack pointer and B is the expected stack size.
     For OP to match, each subsequent instruction should push a word onto
     the stack.  We therefore expect the first instruction to create
     COUNT-1 stack slots. */
  elt = SET_SRC (elt);
  if (GET_CODE (XEXP (elt, 0)) != REG
      || REGNO (XEXP (elt, 0)) != STACK_POINTER_REGNUM
      || GET_CODE (XEXP (elt, 1)) != CONST_INT
      || INTVAL (XEXP (elt, 1)) != -(count - 1) * 4)
    return 0;

  /* Now go through the rest of the vector elements.  They must be
     ordered so that the first instruction stores the highest-numbered
     register to the highest stack slot and that subsequent instructions
     store a lower-numbered register to the slot below.

     LAST keeps track of the smallest-numbered register stored so far.
     MASK is the set of stored registers. */
  last = FIRST_PSEUDO_REGISTER;
  mask = 0;
  for (i = 1; i < count; i++)
    {
      /* Check that element i is a (set (mem M) R) and that R is valid. */
      elt = XVECEXP (op, 0, i);
      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_CODE (SET_SRC (elt)) != REG
	  || REGNO (SET_SRC (elt)) >= last)
	return 0;

      /* R was OK, so provisionally add it to MASK.  We return 0 in any
	 case if the rest of the instruction has a flaw. */
      last = REGNO (SET_SRC (elt));
      mask |= (1 << last);

      /* Check that M has the form (plus (sp) (const_int -I*4)) */
      elt = XEXP (SET_DEST (elt), 0);
      if (GET_CODE (elt) != PLUS
	  || GET_CODE (XEXP (elt, 0)) != REG
	  || REGNO (XEXP (elt, 0)) != STACK_POINTER_REGNUM
	  || GET_CODE (XEXP (elt, 1)) != CONST_INT
	  || INTVAL (XEXP (elt, 1)) != -i * 4)
	return 0;
    }

  /* All or none of the callee-saved extended registers must be in the set. */
  if ((mask & 0x3c000) != 0
      && (mask & 0x3c000) != 0x3c000)
    return 0;

  return mask;
}

/* Return true if OP is a valid call operand.  */

int
call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG);
}

/* What (if any) secondary registers are needed to move IN with mode
   MODE into a register in register class CLASS. 

   We might be able to simplify this.  */
enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  /* Memory loads less than a full word wide can't have an
     address or stack pointer destination.  They must use
     a data register as an intermediate register.  */
  if ((GET_CODE (in) == MEM
       || (GET_CODE (in) == REG
	   && REGNO (in) >= FIRST_PSEUDO_REGISTER)
       || (GET_CODE (in) == SUBREG
	   && GET_CODE (SUBREG_REG (in)) == REG
	   && REGNO (SUBREG_REG (in)) >= FIRST_PSEUDO_REGISTER))
      && (mode == QImode || mode == HImode)
      && (class == ADDRESS_REGS || class == SP_REGS
	  || class == SP_OR_ADDRESS_REGS))
    {
      if (TARGET_AM33)
	return DATA_OR_EXTENDED_REGS;
      return DATA_REGS;
    }

  /* We can't directly load sp + const_int into a data register;
     we must use an address register as an intermediate.  */
  if (class != SP_REGS
      && class != ADDRESS_REGS
      && class != SP_OR_ADDRESS_REGS
      && class != SP_OR_EXTENDED_REGS
      && class != ADDRESS_OR_EXTENDED_REGS
      && class != SP_OR_ADDRESS_OR_EXTENDED_REGS
      && (in == stack_pointer_rtx
	  || (GET_CODE (in) == PLUS
	      && (XEXP (in, 0) == stack_pointer_rtx
		  || XEXP (in, 1) == stack_pointer_rtx))))
    return ADDRESS_REGS;

  if (GET_CODE (in) == PLUS
      && (XEXP (in, 0) == stack_pointer_rtx
	  || XEXP (in, 1) == stack_pointer_rtx))
    {
      if (TARGET_AM33)
	return DATA_OR_EXTENDED_REGS;
      return DATA_REGS;
    }
 
  /* Otherwise assume no secondary reloads are needed.  */
  return NO_REGS;
}

int
initial_offset (from, to)
     int from, to;
{
  /* The difference between the argument pointer and the frame pointer
     is the size of the callee register save area.  */
  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      if (regs_ever_live[2] || regs_ever_live[3]
	  || regs_ever_live[6] || regs_ever_live[7]
	  || regs_ever_live[14] || regs_ever_live[15]
	  || regs_ever_live[16] || regs_ever_live[17]
	  || frame_pointer_needed)
	return REG_SAVE_BYTES;
      else
	return 0;
    }

  /* The difference between the argument pointer and the stack pointer is
     the sum of the size of this function's frame, the callee register save
     area, and the fixed stack space needed for function calls (if any).  */
  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      if (regs_ever_live[2] || regs_ever_live[3]
	  || regs_ever_live[6] || regs_ever_live[7]
	  || regs_ever_live[14] || regs_ever_live[15]
	  || regs_ever_live[16] || regs_ever_live[17]
	  || frame_pointer_needed)
	return (get_frame_size () + REG_SAVE_BYTES
		+ (current_function_outgoing_args_size
		   ? current_function_outgoing_args_size + 4 : 0)); 
      else
	return (get_frame_size ()
		+ (current_function_outgoing_args_size
		   ? current_function_outgoing_args_size + 4 : 0)); 
    }

  /* The difference between the frame pointer and stack pointer is the sum
     of the size of this function's frame and the fixed stack space needed
     for function calls (if any).  */
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return (get_frame_size ()
	    + (current_function_outgoing_args_size
	       ? current_function_outgoing_args_size + 4 : 0)); 

  abort ();
}

/* Flush the argument registers to the stack for a stdarg function;
   return the new argument pointer.  */
rtx
mn10300_builtin_saveregs ()
{
  rtx offset, mem;
  tree fntype = TREE_TYPE (current_function_decl);
  int argadj = ((!(TYPE_ARG_TYPES (fntype) != 0
                   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
                       != void_type_node)))
                ? UNITS_PER_WORD : 0);
  int set = get_varargs_alias_set ();

  if (argadj)
    offset = plus_constant (current_function_arg_offset_rtx, argadj);
  else
    offset = current_function_arg_offset_rtx;

  mem = gen_rtx_MEM (SImode, current_function_internal_arg_pointer);
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (SImode, 0));

  mem = gen_rtx_MEM (SImode,
		     plus_constant (current_function_internal_arg_pointer, 4));
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (SImode, 1));

  return copy_to_reg (expand_binop (Pmode, add_optab,
				    current_function_internal_arg_pointer,
				    offset, 0, 0, OPTAB_LIB_WIDEN));
}

void
mn10300_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg;
{
  if (stdarg_p)
    nextarg = expand_builtin_saveregs ();

  std_expand_builtin_va_start (stdarg_p, valist, nextarg);
}

rtx
mn10300_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT align, rsize;
  tree t, ptr, pptr;

  /* Compute the rounded size of the type.  */
  align = PARM_BOUNDARY / BITS_PER_UNIT;
  rsize = (((int_size_in_bytes (type) + align - 1) / align) * align);

  t = build (POSTINCREMENT_EXPR, TREE_TYPE (valist), valist, 
	     build_int_2 ((rsize > 8 ? 4 : rsize), 0));
  TREE_SIDE_EFFECTS (t) = 1;

  ptr = build_pointer_type (type);

  /* "Large" types are passed by reference.  */
  if (rsize > 8)
    {
      pptr = build_pointer_type (ptr);
      t = build1 (NOP_EXPR, pptr, t);
      TREE_SIDE_EFFECTS (t) = 1;

      t = build1 (INDIRECT_REF, ptr, t);
      TREE_SIDE_EFFECTS (t) = 1;
    }
  else
    {
      t = build1 (NOP_EXPR, ptr, t);
      TREE_SIDE_EFFECTS (t) = 1;
    }

  /* Calculate!  */
  return expand_expr (t, NULL_RTX, Pmode, EXPAND_NORMAL);
}

/* Return an RTX to represent where a value with mode MODE will be returned
   from a function.  If the result is 0, the argument is pushed.  */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  rtx result = 0;
  int size, align;

  /* We only support using 2 data registers as argument registers.  */
  int nregs = 2;

  /* Figure out the size of the object to be passed.  */
  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  /* Figure out the alignment of the object to be passed.  */
  align = size;

  cum->nbytes = (cum->nbytes + 3) & ~3;

  /* Don't pass this arg via a register if all the argument registers
     are used up.  */
  if (cum->nbytes > nregs * UNITS_PER_WORD)
    return 0;

  /* Don't pass this arg via a register if it would be split between
     registers and memory.  */
  if (type == NULL_TREE
      && cum->nbytes + size > nregs * UNITS_PER_WORD)
    return 0;

  switch (cum->nbytes / UNITS_PER_WORD)
    {
    case 0:
      result = gen_rtx_REG (mode, 0);
      break;
    case 1:
      result = gen_rtx_REG (mode, 1);
      break;
    default:
      result = 0;
    }

  return result;
}

/* Return the number of registers to use for an argument passed partially
   in registers and partially in memory.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int size, align;

  /* We only support using 2 data registers as argument registers.  */
  int nregs = 2;

  /* Figure out the size of the object to be passed.  */
  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  /* Figure out the alignment of the object to be passed.  */
  align = size;

  cum->nbytes = (cum->nbytes + 3) & ~3;

  /* Don't pass this arg via a register if all the argument registers
     are used up.  */
  if (cum->nbytes > nregs * UNITS_PER_WORD)
    return 0;

  if (cum->nbytes + size <= nregs * UNITS_PER_WORD)
    return 0;

  /* Don't pass this arg via a register if it would be split between
     registers and memory.  */
  if (type == NULL_TREE
      && cum->nbytes + size > nregs * UNITS_PER_WORD)
    return 0;

  return (nregs * UNITS_PER_WORD - cum->nbytes) / UNITS_PER_WORD;
}

/* Output a tst insn.  */
const char *
output_tst (operand, insn)
     rtx operand, insn;
{
  rtx temp;
  int past_call = 0;

  /* We can save a byte if we can find a register which has the value
     zero in it.  */
  temp = PREV_INSN (insn);
  while (optimize && temp)
    {
      rtx set;

      /* We allow the search to go through call insns.  We record
	 the fact that we've past a CALL_INSN and reject matches which
	 use call clobbered registers.  */
      if (GET_CODE (temp) == CODE_LABEL
	  || GET_CODE (temp) == JUMP_INSN
	  || GET_CODE (temp) == BARRIER)
	break;

      if (GET_CODE (temp) == CALL_INSN)
	past_call = 1;

      if (GET_CODE (temp) == NOTE)
	{
	  temp = PREV_INSN (temp);
	  continue;
	}

      /* It must be an insn, see if it is a simple set. */
      set = single_set (temp);
      if (!set)
	{
	  temp = PREV_INSN (temp);
	  continue;
	}

      /* Are we setting a data register to zero (this does not win for
	 address registers)? 

	 If it's a call clobbered register, have we past a call?

	 Make sure the register we find isn't the same as ourself;
	 the mn10300 can't encode that.

	 ??? reg_set_between_p return nonzero anytime we pass a CALL_INSN
	 so the code to detect calls here isn't doing anything useful.  */
      if (REG_P (SET_DEST (set))
	  && SET_SRC (set) == CONST0_RTX (GET_MODE (SET_DEST (set)))
	  && !reg_set_between_p (SET_DEST (set), temp, insn)
	  && (REGNO_REG_CLASS (REGNO (SET_DEST (set)))
	      == REGNO_REG_CLASS (REGNO (operand)))
	  && REGNO_REG_CLASS (REGNO (SET_DEST (set))) != EXTENDED_REGS
	  && REGNO (SET_DEST (set)) != REGNO (operand)
	  && (!past_call 
	      || !call_used_regs[REGNO (SET_DEST (set))]))
	{
	  rtx xoperands[2];
	  xoperands[0] = operand;
	  xoperands[1] = SET_DEST (set);

	  output_asm_insn ("cmp %1,%0", xoperands);
	  return "";
	}

      if (REGNO_REG_CLASS (REGNO (operand)) == EXTENDED_REGS
	  && REG_P (SET_DEST (set))
	  && SET_SRC (set) == CONST0_RTX (GET_MODE (SET_DEST (set)))
	  && !reg_set_between_p (SET_DEST (set), temp, insn)
	  && (REGNO_REG_CLASS (REGNO (SET_DEST (set)))
	      != REGNO_REG_CLASS (REGNO (operand)))
	  && REGNO_REG_CLASS (REGNO (SET_DEST (set))) == EXTENDED_REGS
	  && REGNO (SET_DEST (set)) != REGNO (operand)
	  && (!past_call 
	      || !call_used_regs[REGNO (SET_DEST (set))]))
	{
	  rtx xoperands[2];
	  xoperands[0] = operand;
	  xoperands[1] = SET_DEST (set);

	  output_asm_insn ("cmp %1,%0", xoperands);
	  return "";
	}
      temp = PREV_INSN (temp);
    }
  return "cmp 0,%0";
}

int
impossible_plus_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != PLUS)
    return 0;

  if (XEXP (op, 0) == stack_pointer_rtx
      || XEXP (op, 1) == stack_pointer_rtx)
    return 1;

  return 0;
}

/* Return 1 if X is a CONST_INT that is only 8 bits wide.  This is used
   for the btst insn which may examine memory or a register (the memory
   variant only allows an unsigned 8 bit integer).  */
int
const_8bit_operand (op, mode)
    register rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= 0
	  && INTVAL (op) < 256);
}

/* Similarly, but when using a zero_extract pattern for a btst where
   the source operand might end up in memory.  */
int
mask_ok_for_mem_btst (len, bit)
     int len;
     int bit;
{
  int mask = 0;

  while (len > 0)
    {
      mask |= (1 << bit);
      bit++;
      len--;
    }

  /* MASK must bit into an 8bit value.  */
  return (((mask & 0xff) == mask)
	  || ((mask & 0xff00) == mask)
	  || ((mask & 0xff0000) == mask)
	  || ((mask & 0xff000000) == mask));
}

/* Return 1 if X contains a symbolic expression.  We know these
   expressions will have one of a few well defined forms, so
   we need only check those forms.  */
int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
               || GET_CODE (XEXP (op, 0)) == LABEL_REF)
              && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

/* Try machine dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   Normally it is always safe for this macro to do nothing.  It exists to
   recognize opportunities to optimize the output.

   But on a few ports with segmented architectures and indexed addressing
   (mn10300, hppa) it is used to rewrite certain problematical addresses.  */
rtx
legitimize_address (x, oldx, mode)
     rtx x;
     rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* Uh-oh.  We might have an address for x[n-100000].  This needs
     special handling to avoid creating an indexed memory address
     with x-100000 as the base.  */
  if (GET_CODE (x) == PLUS
      && symbolic_operand (XEXP (x, 1), VOIDmode))
    {
      /* Ugly.  We modify things here so that the address offset specified
         by the index expression is computed first, then added to x to form
         the entire address.  */

      rtx regx1, regy1, regy2, y;

      /* Strip off any CONST.  */
      y = XEXP (x, 1);
      if (GET_CODE (y) == CONST)
        y = XEXP (y, 0);

      if (GET_CODE (y) == PLUS || GET_CODE (y) == MINUS)
	{
	  regx1 = force_reg (Pmode, force_operand (XEXP (x, 0), 0));
	  regy1 = force_reg (Pmode, force_operand (XEXP (y, 0), 0));
	  regy2 = force_reg (Pmode, force_operand (XEXP (y, 1), 0));
	  regx1 = force_reg (Pmode,
			     gen_rtx (GET_CODE (y), Pmode, regx1, regy2));
	  return force_reg (Pmode, gen_rtx_PLUS (Pmode, regx1, regy1));
	}
    }
  return x;
}

int
mn10300_address_cost (x, unsig)
     rtx x;
     int *unsig;
{
  int _s = 0;
  if (unsig == 0)
    unsig = &_s;
  
  switch (GET_CODE (x))
    {
    case REG:
      switch (REGNO_REG_CLASS (REGNO (x)))
	{
	case SP_REGS:
	  *unsig = 1;
	  return 0;

	case ADDRESS_REGS:
	  return 1;

	case DATA_REGS:
	case EXTENDED_REGS:
	  return 3;

	case NO_REGS:
	  return 5;

	default:
	  abort ();
	}

    case PLUS:
    case MINUS:
    case ASHIFT:
    case AND:
    case IOR:
      return (mn10300_address_cost (XEXP (x, 0), unsig)
	      + mn10300_address_cost (XEXP (x, 1), unsig));

    case EXPR_LIST:
    case SUBREG:
    case MEM:
      return ADDRESS_COST (XEXP (x, 0));

    case ZERO_EXTEND:
      *unsig = 1;
      return mn10300_address_cost (XEXP (x, 0), unsig);

    case CONST_INT:
      if (INTVAL (x) == 0)
	return 0;
      if (INTVAL (x) + (*unsig ? 0 : 0x80) < 0x100)
	return 1;
      if (INTVAL (x) + (*unsig ? 0 : 0x8000) < 0x10000)
	return 3;
      if (INTVAL (x) + (*unsig ? 0 : 0x800000) < 0x1000000)
	return 5;
      return 7;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 8;

    case ADDRESSOF:
      switch (GET_CODE (XEXP (x, 0)))
	{
	case MEM:
	  return ADDRESS_COST (XEXP (x, 0));

	case REG:
	  return 1;

	default:
	  abort ();
	}

    default:
      abort ();

    }
}
