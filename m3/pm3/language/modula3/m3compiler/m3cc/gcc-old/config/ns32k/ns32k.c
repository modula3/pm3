/* Subroutines for assembler code output on the NS32000.
   Copyright (C) 1988, 1994, 1995, 1996 Free Software Foundation, Inc.

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

/* Some output-actions in ns32k.md need these.  */
#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"

#ifdef OSF_OS
int ns32k_num_files = 0;
#endif

void
trace (s, s1, s2)
     char *s, *s1, *s2;
{
  fprintf (stderr, s, s1, s2);
}

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */ 

int
hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  switch (mode)
    {
    case QImode:
    case HImode:
    case PSImode:
    case SImode:
    case PDImode:
    case VOIDmode:
    case BLKmode:
      if (regno < 8 || regno == 16 || regno == 17)
	return 1;
      else
	return 0;

    case DImode:
      if (regno < 8 && (regno & 1) == 0)
	return 1;
      else
	return 0;

    case SFmode:
    case SCmode:
      if (TARGET_32081)
	{
	  if (regno < 16)
	    return 1;
	  else
	    return 0;
	}
      else
	{
	  if (regno < 8)
	    return 1;
	  else 
	    return 0;
	}

    case DFmode:
    case DCmode:
      if ((regno & 1) == 0)
	{	
	  if (TARGET_32081)
	    {
	      if (regno < 16)
		return 1;
	      else
		return 0;
	    }
	  else
	    {
	      if (regno < 8)
		return 1;
	      else
		return 0;
	    }
	}
      else
	return 0;
    }

  /* Used to abort here, but simply saying "no" handles TImode
     much better.  */
  return 0;
}

/* ADDRESS_COST calls this.  This function is not optimal
   for the 32032 & 32332, but it probably is better than
   the default. */

int
calc_address_cost (operand)
     rtx operand;
{
  int i;
  int cost = 0;
  
  if (GET_CODE (operand) == MEM)
    cost += 3;
  if (GET_CODE (operand) == MULT)
    cost += 2;
#if 0
  if (GET_CODE (operand) == REG)
    cost += 1;			/* not really, but the documentation
				   says different amount of registers
				   shouldn't return the same costs */
#endif
  switch (GET_CODE (operand))
    {
    case REG:
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case POST_DEC:
    case PRE_DEC:
      break;
    case MULT:
    case MEM:
    case PLUS:
      for (i = 0; i < GET_RTX_LENGTH (GET_CODE (operand)); i++)
	{
	  cost += calc_address_cost (XEXP (operand, i));
	}
    default:
      break;
    }
  return cost;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = true_regnum (in);

  if (regno >= FIRST_PSEUDO_REGISTER)
    regno = -1;

  /* We can place anything into GENERAL_REGS and can put GENERAL_REGS
     into anything.  */
  if (class == GENERAL_REGS || (regno >= 0 && regno < 8))
    return NO_REGS;

  /* Constants, memory, and FP registers can go into FP registers.  */
  if ((regno == -1 || (regno >= 8 && regno < 16)) && (class == FLOAT_REGS))
    return NO_REGS;

#if 0 /* This isn't strictly true (can't move fp to sp or vice versa),
	 so it's cleaner to use PREFERRED_RELOAD_CLASS
	 to make the right things happen.  */
  if (regno >= 16 && class == GEN_AND_MEM_REGS)
    return NO_REGS;
#endif

  /* Otherwise, we need GENERAL_REGS. */
  return GENERAL_REGS;
}
/* Generate the rtx that comes from an address expression in the md file */
/* The expression to be build is BASE[INDEX:SCALE].  To recognize this,
   scale must be converted from an exponent (from ASHIFT) to a
   multiplier (for MULT). */
rtx
gen_indexed_expr (base, index, scale)
     rtx base, index, scale;
{
  rtx addr;

  /* This generates an invalid addressing mode, if BASE is
     fp or sp.  This is handled by PRINT_OPERAND_ADDRESS.  */
  if (GET_CODE (base) != REG && GET_CODE (base) != CONST_INT)
    base = gen_rtx (MEM, SImode, base);
  addr = gen_rtx (MULT, SImode, index,
		  gen_rtx (CONST_INT, VOIDmode, 1 << INTVAL (scale)));
  addr = gen_rtx (PLUS, SImode, base, addr);
  return addr;
}

/* Return 1 if OP is a valid operand of mode MODE.  This
   predicate rejects operands which do not have a mode
   (such as CONST_INT which are VOIDmode).  */
int
reg_or_mem_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_MODE (op) == mode
	  && (GET_CODE (op) == REG
	      || GET_CODE (op) == SUBREG
	      || GET_CODE (op) == MEM));
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands". */

void
split_di (operands, num, lo_half, hi_half)
     rtx operands[];
     int num;
     rtx lo_half[], hi_half[];
{
  while (num--)
    {
      if (GET_CODE (operands[num]) == REG)
	{
	  lo_half[num] = gen_rtx (REG, SImode, REGNO (operands[num]));
	  hi_half[num] = gen_rtx (REG, SImode, REGNO (operands[num]) + 1);
	}
      else if (CONSTANT_P (operands[num]))
	{
	  split_double (operands[num], &lo_half[num], &hi_half[num]);
	}
      else if (offsettable_memref_p (operands[num]))
	{
	  lo_half[num] = operands[num];
	  hi_half[num] = adj_offsettable_operand (operands[num], 4);
	}
      else
	abort();
    }
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) <= 7
      && INTVAL (operands[1]) >= -8)
    return "movqd %1,%0";
  return "movd %1,%0";
}

char *
output_move_double (operands)
     rtx *operands;
{
  enum anon1 { REGOP, OFFSOP, PUSHOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1])
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If insn is effectively movd N(sp),tos then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4(sp))
     for the low word as well, to compensate for the first decrement of sp.
     Given this, it doesn't matter which half we do "first".  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = latehalf[1];

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */
  else if (optype0 == PUSHOP || optype1 == PUSHOP)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  /* Loading into a register which overlaps a register used in the address.  */
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (operands[0], operands[1]))
    {
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0))
	  && reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  /* If both halves of dest are used in the src memory address,
	     load the destination address into the low reg (operands[0]).
	     Then it works to load latehalf first.  */
	  rtx xops[2];
	  xops[0] = XEXP (operands[1], 0);
	  xops[1] = operands[0];
	  output_asm_insn ("addr %a0,%1", xops);
	  operands[1] = gen_rtx (MEM, DImode, operands[0]);
	  latehalf[1] = adj_offsettable_operand (operands[1], 4);
	  /* The first half has the overlap, Do the late half first.  */
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  /* Then clobber.  */
	  return singlemove_string (operands);
	}
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0)))
	{
	  /* The first half has the overlap, Do the late half first.  */
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  /* Then clobber.  */
	  return singlemove_string (operands);
	}
    }

  /* Normal case.  Do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  operands[0] = latehalf[0];
  operands[1] = latehalf[1];
  return singlemove_string (operands);
}

int
check_reg (oper, reg)
     rtx oper;
     int reg;
{
  register int i;

  if (oper == 0)
    return 0;
  switch (GET_CODE(oper))
    {
    case REG:
      return (REGNO(oper) == reg) ? 1 : 0;
    case MEM:
      return check_reg(XEXP(oper, 0), reg);
    case PLUS:
    case MULT:
      return check_reg(XEXP(oper, 0), reg) || check_reg(XEXP(oper, 1), reg);
    }
  return 0;
}

/* Returns 1 if OP contains a global symbol reference */

int
global_symbolic_reference_mentioned_p (op, f)
     rtx op;
     int f;
{
  register char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF)
    {
      if (! SYMBOL_REF_FLAG (op))
	return 1;
      else
        return 0;
    }
  else if (f && GET_CODE (op) != CONST)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (global_symbolic_reference_mentioned_p (XVECEXP (op, i, j), 0))
	      return 1;
	}
      else if (fmt[i] == 'e' 
	       && global_symbolic_reference_mentioned_p (XEXP (op, i), 0))
	return 1;
    }

  return 0;
}


/* PRINT_OPERAND is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in ns32k.h.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     char code;
{
  if (code == '$')
    PUT_IMMEDIATE_PREFIX (file);
  else if (code == '?')
    PUT_EXTERNAL_PREFIX (file);
  else if (GET_CODE (x) == REG)
    fprintf (file, "%s", reg_names[REGNO (x)]);
  else if (GET_CODE (x) == MEM)
    {
      rtx tmp = XEXP (x, 0);
      output_address (XEXP (x, 0));
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != VOIDmode)
    {
      if (GET_MODE (x) == DFmode)
	{ 
	  union { double d; int i[2]; } u;
	  u.i[0] = CONST_DOUBLE_LOW (x); u.i[1] = CONST_DOUBLE_HIGH (x);
	  PUT_IMMEDIATE_PREFIX(file);
#ifdef SEQUENT_ASM
	  /* Sequent likes it's floating point constants as integers */
	  fprintf (file, "0Dx%08x%08x", u.i[1], u.i[0]);
#else
#ifdef ENCORE_ASM
	  fprintf (file, "0f%.20e", u.d); 
#else
	  fprintf (file, "0d%.20e", u.d); 
#endif
#endif
	}
      else
	{ 
	  union { double d; int i[2]; } u;
	  u.i[0] = CONST_DOUBLE_LOW (x); u.i[1] = CONST_DOUBLE_HIGH (x);
	  PUT_IMMEDIATE_PREFIX (file);
#ifdef SEQUENT_ASM
	  /* We have no way of winning if we can't get the bits
	     for a sequent floating point number.  */
#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
	  abort ();
#endif
	  {
	    union { float f; long l; } uu;
	    uu.f = u.d;
	    fprintf (file, "0Fx%08x", uu.l);
	  }
#else
	  fprintf (file, "0f%.20e", u.d); 
#endif
	}
    }
  else
    {
#ifdef NO_IMMEDIATE_PREFIX_IF_SYMBOLIC
      if (GET_CODE (x) == CONST_INT)
#endif
	PUT_IMMEDIATE_PREFIX (file);
      output_addr_const (file, x);
    }
}

/* PRINT_OPERAND_ADDRESS is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in ns32k.h .  */

/* Completely rewritten to get this to work with Gas for PC532 Mach.
   This function didn't work and I just wasn't able (nor very willing) to
   figure out how it worked.
   90-11-25 Tatu Yl|nen <ylo@cs.hut.fi> */

print_operand_address (file, addr)
     register FILE *file;
     register rtx addr;
{
  static char scales[] = { 'b', 'w', 'd', 0, 'q', };
  rtx offset, base, indexexp, tmp;
  int scale;
  extern int flag_pic;

  if (GET_CODE (addr) == PRE_DEC || GET_CODE (addr) == POST_DEC)
    {
      fprintf (file, "tos");
      return;
    }

  offset = NULL;
  base = NULL;
  indexexp = NULL;
  while (addr != NULL)
    {
      if (GET_CODE (addr) == PLUS)
	{
	  if (GET_CODE (XEXP (addr, 0)) == PLUS)
	    {
	      tmp = XEXP (addr, 1);
	      addr = XEXP (addr, 0);
	    }
	  else
	    {
	      tmp = XEXP (addr,0);
	      addr = XEXP (addr,1);
	    }
	}
      else
	{
	  tmp = addr;
	  addr = NULL;
	}
      switch (GET_CODE (tmp))
	{
	case PLUS:
	  abort ();
	case MEM:
	  if (base)
	    {
	      indexexp = base;
	      base = tmp;
	    }
	  else
	    base = tmp;
	  break;
	case REG:
	  if (REGNO (tmp) < 8)
	    if (base)
	      {
		indexexp = tmp;
	      }
	    else
	      base = tmp;
	  else
	    if (base)
	      {
		indexexp = base;
		base = tmp;
	      }
	    else
	      base = tmp;
	  break;
	case MULT:
	  indexexp = tmp;
	  break;
	case SYMBOL_REF:
	  if (flag_pic && ! CONSTANT_POOL_ADDRESS_P (tmp)
	      && ! SYMBOL_REF_FLAG (tmp))
	    {
	      if (base)
		{
		  if (indexexp)
		    abort ();
		  indexexp = base;
		}
	      base = tmp;
	      break;
	    }
	case CONST:
	  if (flag_pic && GET_CODE (tmp) == CONST)
	    {
	      rtx sym, off, tmp1;
	      tmp1 = XEXP (tmp,0);
	      if (GET_CODE (tmp1)  != PLUS)
		abort ();

	      sym = XEXP (tmp1,0);
	      if (GET_CODE (sym) != SYMBOL_REF)
	        {
	          off = sym;
		  sym = XEXP (tmp1,1);
		}
	      else
	        off = XEXP (tmp1,1);
	      if (GET_CODE (sym) == SYMBOL_REF)
		{
		  if (GET_CODE (off) != CONST_INT)
		    abort ();

		  if (CONSTANT_POOL_ADDRESS_P (sym)
		      || SYMBOL_REF_FLAG (sym))
		    {
		      SYMBOL_REF_FLAG (tmp) = 1;
		    }
		  else
		    {
		      if (base)
			{
			  if (indexexp)
			    abort ();

			  indexexp = base;
			}

		      if (offset != 0)
			abort ();

		      base = sym;
		      offset = off;
		      break;
		    }
		}
	    }
	case CONST_INT:
	case LABEL_REF:
	  if (offset)
	    offset = gen_rtx (PLUS, SImode, tmp, offset);
	  else
	    offset = tmp;
	  break;
	default:
	  abort ();
	}
    }
  if (! offset)
    offset = const0_rtx;

  if (base
#ifndef INDEX_RATHER_THAN_BASE
      && (flag_pic || TARGET_HIMEM)
      && GET_CODE (base) != SYMBOL_REF 
      && GET_CODE (offset) != CONST_INT
#else
  /* This is a re-implementation of the SEQUENT_ADDRESS_BUG fix.  */
#endif
      && !indexexp && GET_CODE (base) == REG
      && REG_OK_FOR_INDEX_P (base))
    {
      indexexp = base;
      base = NULL;
    }

  /* now, offset, base and indexexp are set */
#ifndef BASE_REG_NEEDED
  if (! base)
    {
#if defined (PC_RELATIVE) || defined (NO_ABSOLUTE_PREFIX_IF_SYMBOLIC)
      if (GET_CODE (offset) == CONST_INT)
#endif
	PUT_ABSOLUTE_PREFIX (file);
    }
#endif

  output_addr_const (file, offset);
  if (base) /* base can be (REG ...) or (MEM ...) */
    switch (GET_CODE (base))
      {
	/* now we must output base.  Possible alternatives are:
	   (rN)       (REG ...)
	   (sp)	      (REG ...)
	   (fp)       (REG ...)
	   (pc)       (REG ...)  used for SYMBOL_REF and LABEL_REF, output
	   (disp(fp)) (MEM ...)       just before possible [rX:y]
	   (disp(sp)) (MEM ...)
	   (disp(sb)) (MEM ...)
	   */
      case REG:
	fprintf (file, "(%s)", reg_names[REGNO (base)]);
	break;
      case SYMBOL_REF:
	if (! flag_pic)
	  abort ();

        fprintf (file, "(");
	output_addr_const (file, base);
	fprintf (file, "(sb))");
        break;
      case MEM:
	addr = XEXP(base,0);
	base = NULL;
	offset = NULL;
	while (addr != NULL)
	  {
	    if (GET_CODE (addr) == PLUS)
	      {
		if (GET_CODE (XEXP (addr, 0)) == PLUS)
		  {
		    tmp = XEXP (addr, 1);
		    addr = XEXP (addr, 0);
		  }
		else
		  {
		    tmp = XEXP (addr, 0);
		    addr = XEXP (addr, 1);
		  }
	      }
	    else
	      {
		tmp = addr;
		addr = NULL;
	      }
	    switch (GET_CODE (tmp))
	      {
	      case REG:
		base = tmp;
		break;
	      case CONST:
	      case CONST_INT:
	      case SYMBOL_REF:
	      case LABEL_REF:
		if (offset)
		  offset = gen_rtx (PLUS, SImode, tmp, offset);
		else
		  offset = tmp;
		break;
	      default:
		abort ();
	      }
	  }
	if (! offset)
	  offset = const0_rtx;
	fprintf (file, "(");
	output_addr_const (file, offset);
	if (base)
	  fprintf (file, "(%s)", reg_names[REGNO (base)]);
	else if (TARGET_SB)
	  fprintf (file, "(sb)");
	else
	  abort ();
	fprintf (file, ")");
	break;
      default:
	abort ();
      }
#ifdef PC_RELATIVE
  else if (GET_CODE (offset) != CONST_INT)
    fprintf (file, "(pc)");
#ifdef BASE_REG_NEEDED
  else if (TARGET_SB)
    fprintf (file, "(sb)");
  else
    abort ();
#endif
#endif /* PC_RELATIVE */

  /* now print index if we have one */
  if (indexexp)
    {
      if (GET_CODE (indexexp) == MULT)
	{
	  scale = INTVAL (XEXP (indexexp, 1)) >> 1;
	  indexexp = XEXP (indexexp, 0);
	}
      else
	scale = 0;
      if (GET_CODE (indexexp) != REG || REGNO (indexexp) >= 8)
	abort ();

#ifdef UTEK_ASM
      fprintf (file, "[%c`%s]",
	       scales[scale],
	       reg_names[REGNO (indexexp)]);
#else
      fprintf (file, "[%s:%c]",
	       reg_names[REGNO (indexexp)],
	       scales[scale]);
#endif
    }
}

/* National 32032 shifting is so bad that we can get
   better performance in many common cases by using other
   techniques.  */
char *
output_shift_insn (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) <= 3)
    if (GET_CODE (operands[0]) == REG)
      {
	if (GET_CODE (operands[1]) == REG)
	  {
	    if (REGNO (operands[0]) == REGNO (operands[1]))
	      {
		if (operands[2] == const1_rtx)
		  return "addd %0,%0";
		else if (INTVAL (operands[2]) == 2)
		  return "addd %0,%0\n\taddd %0,%0";
	      }
	    if (operands[2] == const1_rtx)
	      return "movd %1,%0\n\taddd %0,%0";
	    
	    operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	    return "addr %a1,%0";
	  }
	if (operands[2] == const1_rtx)
	  return "movd %1,%0\n\taddd %0,%0";
      }
    else if (GET_CODE (operands[1]) == REG)
      {
	operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	return "addr %a1,%0";
      }
    else if (INTVAL (operands[2]) == 1
	     && GET_CODE (operands[1]) == MEM
	     && rtx_equal_p (operands [0], operands[1]))
      {
	rtx temp = XEXP (operands[1], 0);
	
	if (GET_CODE (temp) == REG
	    || (GET_CODE (temp) == PLUS
		&& GET_CODE (XEXP (temp, 0)) == REG
		&& GET_CODE (XEXP (temp, 1)) == CONST_INT))
	  return "addd %0,%0";
      }
    else return "ashd %2,%0";
  return "ashd %2,%0";
}

char *
output_move_dconst (n, s)
	int n;
	char *s;
{
  static char r[32];

  if (n > -9 && n < 8)
    strcpy (r, "movqd ");
  else if (n > 0 && n < 256)
    strcpy (r, "movzbd ");
  else if (n > 0 && n < 65536)
    strcpy (r, "movzwd ");
  else if (n < 0 && n > -129)
    strcpy (r, "movxbd ");
  else if (n < 0 && n > -32769)
    strcpy (r, "movxwd ");
  else
    strcpy (r, "movd ");
  strcat (r, s);
  return r;
}
