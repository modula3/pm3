/* Subroutines for insn-output.c for ATMEL AVR micro controllers
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (denisc@overta.ru)

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
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "tree.h"
#include "expr.h"
#include "toplev.h"
#include "obstack.h"
#include "function.h"
#include "recog.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

/* Maximal allowed offset for an address in the LD command */
#define MAX_LD_OFFSET(MODE) (64 - (signed)GET_MODE_SIZE (MODE))

static int    avr_naked_function_p PARAMS ((tree));
static int    interrupt_function_p PARAMS ((tree));
static int    signal_function_p    PARAMS ((tree));
static int    sequent_regs_live    PARAMS ((void));
static const char * ptrreg_to_str  PARAMS ((int));
static const char * cond_string    PARAMS ((enum rtx_code));
static int    avr_num_arg_regs     PARAMS ((enum machine_mode, tree));
static int    out_adj_frame_ptr    PARAMS ((FILE *, int));
static int    out_set_stack_ptr    PARAMS ((FILE *, int, int));
static RTX_CODE compare_condition  PARAMS ((rtx insn));
static int    compare_sign_p       PARAMS ((rtx insn));
static int    reg_was_0            PARAMS ((rtx insn, rtx op));
static int    io_address_p         PARAMS ((rtx x, int size));
void          debug_hard_reg_set   PARAMS ((HARD_REG_SET set));
static tree   avr_handle_progmem_attribute PARAMS ((tree *, tree, tree, int, bool *));
static tree   avr_handle_fndecl_attribute PARAMS ((tree *, tree, tree, int, bool *));
const struct attribute_spec avr_attribute_table[];
static bool   avr_assemble_integer PARAMS ((rtx, unsigned int, int));
static void   avr_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void   avr_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

/* Allocate registers from r25 to r8 for parameters for function calls */
#define FIRST_CUM_REG 26

/* Temporary register RTX (gen_rtx (REG,QImode,TMP_REGNO)) */
rtx tmp_reg_rtx;

/* Zeroed register RTX (gen_rtx (REG,QImode,ZERO_REGNO)) */
rtx zero_reg_rtx;

/* RTX for register which will be used for loading immediate values to
   r0-r15 registers.  */
rtx ldi_reg_rtx;

/* AVR register names {"r0", "r1", ..., "r31"} */
static const char *const avr_regnames[] = REGISTER_NAMES;

/* This holds the last insn address.  */
static int last_insn_address = 0;

/* Commands count in the compiled file */
static int commands_in_file;

/* Commands in the functions prologues in the compiled file */
static int commands_in_prologues;

/* Commands in the functions epilogues in the compiled file */
static int commands_in_epilogues;

/* Prologue/Epilogue size in words */
static int prologue_size;
static int epilogue_size;

/* Size of all jump tables in the current function, in words.  */
static int jump_tables_size;

/* Initial stack value specified by the `-minit-stack=' option */
const char *avr_init_stack = "__stack";

/* Default MCU name */
const char *avr_mcu_name = "avr2";

/* More than 8K of program memory: use "call" and "jmp".  */
int avr_mega_p = 0;

/* Enhanced core: use "movw", "mul", ...  */
int avr_enhanced_p = 0;

enum avr_arch {
  AVR1 = 1,
  AVR2,
  AVR3,
  AVR4,
  AVR5
};

struct mcu_type_s {
  const char *const name;
  const enum avr_arch arch;
};

/* List of all known AVR MCU types - if updated, it has to be kept
   in sync in several places (FIXME: is there a better way?):
    - here
    - avr.h (CPP_SPEC, LINK_SPEC, CRT_BINUTILS_SPECS)
    - t-avr (MULTILIB_MATCHES)
    - gas/config/tc-avr.c
    - avr-libc  */

static const struct mcu_type_s avr_mcu_types[] = {
    /* Classic, <= 8K.  */
  { "avr2",      AVR2 },
  { "at90s2313", AVR2 },
  { "at90s2323", AVR2 },
  { "attiny22",  AVR2 },
  { "at90s2333", AVR2 },
  { "at90s2343", AVR2 },
  { "at90s4414", AVR2 },
  { "at90s4433", AVR2 },
  { "at90s4434", AVR2 },
  { "at90s8515", AVR2 },
  { "at90c8534", AVR2 },
  { "at90s8535", AVR2 },
    /* Classic, > 8K.  */
  { "avr3",      AVR3 },
  { "atmega103", AVR3 },
  { "atmega603", AVR3 },
  { "at43usb320", AVR3 },
  { "at76c711",  AVR3 },
    /* Enhanced, <= 8K.  */
  { "avr4",      AVR4 },
  { "atmega8",   AVR4 },
  { "atmega83",  AVR4 },
  { "atmega85",  AVR4 },
    /* Enhanced, > 8K.  */
  { "avr5",      AVR5 },
  { "atmega16",  AVR5 },
  { "atmega161", AVR5 },
  { "atmega163", AVR5 },
  { "atmega32",  AVR5 },
  { "atmega323", AVR5 },
  { "atmega64",  AVR5 },
  { "atmega128", AVR5 },
  { "at43usb355", AVR5 },
  { "at94k",     AVR5 },
    /* Assembler only.  */
  { "avr1",      AVR1 },
  { "at90s1200", AVR1 },
  { "attiny10",  AVR1 },
  { "attiny11",  AVR1 },
  { "attiny12",  AVR1 },
  { "attiny15",  AVR1 },
  { "attiny28",  AVR1 },
  { NULL, 0 }
};

int avr_case_values_threshold = 30000;

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER avr_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE avr_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE avr_output_function_epilogue
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE avr_attribute_table

struct gcc_target targetm = TARGET_INITIALIZER;

void
avr_override_options ()
{
  const struct mcu_type_s *t;

  for (t = avr_mcu_types; t->name; t++)
    if (strcmp (t->name, avr_mcu_name) == 0)
      break;

  if (!t->name)
    {
      fprintf (stderr, "unknown MCU `%s' specified\nKnown MCU names:\n",
	       avr_mcu_name);
      for (t = avr_mcu_types; t->name; t++)
	fprintf (stderr,"   %s\n", t->name);
    }

  switch (t->arch)
    {
    case AVR1:
    default:
      error ("MCU `%s' not supported", avr_mcu_name);
      /* ... fall through ... */
    case AVR2: avr_enhanced_p = 0; avr_mega_p = 0; break;
    case AVR3: avr_enhanced_p = 0; avr_mega_p = 1; break;
    case AVR4: avr_enhanced_p = 1; avr_mega_p = 0; break;
    case AVR5: avr_enhanced_p = 1; avr_mega_p = 1; break;
    }

  if (optimize && !TARGET_NO_TABLEJUMP)
    avr_case_values_threshold = (!AVR_MEGA || TARGET_CALL_PROLOGUES) ? 8 : 17;
}


/* Initialize TMP_REG_RTX and ZERO_REG_RTX */
void
avr_init_once ()
{
  tmp_reg_rtx = xmalloc (sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  memset (tmp_reg_rtx, 0, sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  PUT_CODE (tmp_reg_rtx, REG);
  PUT_MODE (tmp_reg_rtx, QImode);
  XINT (tmp_reg_rtx, 0) = TMP_REGNO;

  zero_reg_rtx = xmalloc (sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  memset (zero_reg_rtx, 0, sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  PUT_CODE (zero_reg_rtx, REG);
  PUT_MODE (zero_reg_rtx, QImode);
  XINT (zero_reg_rtx, 0) = ZERO_REGNO;

  ldi_reg_rtx = xmalloc (sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  memset (ldi_reg_rtx, 0, sizeof (struct rtx_def) + 1 * sizeof (rtunion));
  PUT_CODE (ldi_reg_rtx, REG);
  PUT_MODE (ldi_reg_rtx, QImode);
  XINT (ldi_reg_rtx, 0) = LDI_REG_REGNO;
}

/*  return register class from register number */

static const int reg_class_tab[]={
  GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,
  GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,
  GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,GENERAL_REGS,
  GENERAL_REGS, /* r0 - r15 */
  LD_REGS,LD_REGS,LD_REGS,LD_REGS,LD_REGS,LD_REGS,LD_REGS,
  LD_REGS,                      /* r16 - 23 */
  ADDW_REGS,ADDW_REGS,          /* r24,r25 */
  POINTER_X_REGS,POINTER_X_REGS, /* r26,27 */
  POINTER_Y_REGS,POINTER_Y_REGS, /* r28,r29 */
  POINTER_Z_REGS,POINTER_Z_REGS, /* r30,r31 */
  STACK_REG,STACK_REG           /* SPL,SPH */
};

/* Return register class for register R */

enum reg_class
avr_regno_reg_class (r)
     int r;
{
  if (r <= 33)
    return reg_class_tab[r];
  return ALL_REGS;
}


/* A C expression which defines the machine-dependent operand
   constraint letters for register classes.  If C is such a
   letter, the value should be the register class corresponding to
   it.  Otherwise, the value should be `NO_REGS'.  The register
   letter `r', corresponding to class `GENERAL_REGS', will not be
   passed to this macro; you do not need to handle it.  */

enum reg_class
avr_reg_class_from_letter  (c)
     int c;
{
  switch (c)
    {
    case 't' : return R0_REG;
    case 'b' : return BASE_POINTER_REGS;
    case 'e' : return POINTER_REGS;
    case 'w' : return ADDW_REGS;
    case 'd' : return LD_REGS;
    case 'l' : return NO_LD_REGS;
    case 'a' : return SIMPLE_LD_REGS;
    case 'x' : return POINTER_X_REGS;
    case 'y' : return POINTER_Y_REGS;
    case 'z' : return POINTER_Z_REGS;
    case 'q' : return STACK_REG;
    default: break;
    }
  return NO_REGS;
}

/* Return non-zero if FUNC is a naked function.  */

static int
avr_naked_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();
  
  a = lookup_attribute ("naked", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is an interrupt function as specified
   by the "interrupt" attribute.  */

static int
interrupt_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("interrupt", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return nonzero if FUNC is a signal function as specified
   by the "signal" attribute.  */

static int
signal_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("signal", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Compute offset between arg_pointer and frame_pointer */

int
initial_elimination_offset (from, to)
     int from;
     int to;
{
  int reg;
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;
  else
    {
      int interrupt_func_p = interrupt_function_p (current_function_decl);
      int signal_func_p = signal_function_p (current_function_decl);
      int leaf_func_p = leaf_function_p ();
      int offset= frame_pointer_needed ? 2 : 0;

      for (reg = 0; reg < 32; ++reg)
	{
	  if ((!leaf_func_p && (call_used_regs[reg]
				&& (interrupt_func_p || signal_func_p)))
	      || (regs_ever_live[reg]
		  && (!call_used_regs[reg] || interrupt_func_p || signal_func_p)
		  && ! (frame_pointer_needed
			&& (reg == REG_Y || reg == (REG_Y+1)))))
	    {
	      ++offset;
	    }
	}
      return get_frame_size () + 2 + 1 + offset;
    }
  return 0;
}

/* This function checks sequence of live registers */

static int
sequent_regs_live ()
{
  int reg;
  int live_seq=0;
  int cur_seq=0;

  for (reg = 0; reg < 18; ++reg)
    {
      if (!call_used_regs[reg])
	{
	  if (regs_ever_live[reg])
	    {
	      ++live_seq;
	      ++cur_seq;
	    }
	  else
	    cur_seq = 0;
	}
    }

  if (!frame_pointer_needed)
    {
      if (regs_ever_live[REG_Y])
	{
	  ++live_seq;
	  ++cur_seq;
	}
      else
	cur_seq = 0;

      if (regs_ever_live[REG_Y+1])
	{
	  ++live_seq;
	  ++cur_seq;
	}
      else
	cur_seq = 0;
    }
  else
    {
      cur_seq += 2;
      live_seq += 2;
    }
  return (cur_seq == live_seq) ? live_seq : 0;
}


/* Output to FILE the asm instructions to adjust the frame pointer by
   ADJ (r29:r28 -= ADJ;) which can be positive (prologue) or negative
   (epilogue).  Returns the number of instructions generated.  */

static int
out_adj_frame_ptr (file, adj)
     FILE *file;
     int adj;
{
  int size = 0;

  if (adj)
    {
      if (TARGET_TINY_STACK)
	{
	  if (adj < -63 || adj > 63)
	    warning ("large frame pointer change (%d) with -mtiny-stack", adj);

	  /* The high byte (r29) doesn't change - prefer "subi" (1 cycle)
	     over "sbiw" (2 cycles, same size).  */

	  fprintf (file, (AS2 (subi, r28, %d) CR_TAB), adj);
	  size++;
	}
      else if (adj < -63 || adj > 63)
	{
	  fprintf (file, (AS2 (subi, r28, lo8(%d)) CR_TAB
			  AS2 (sbci, r29, hi8(%d)) CR_TAB),
		   adj, adj);
	  size += 2;
	}
      else if (adj < 0)
	{
	  fprintf (file, (AS2 (adiw, r28, %d) CR_TAB), -adj);
	  size++;
	}
      else
	{
	  fprintf (file, (AS2 (sbiw, r28, %d) CR_TAB), adj);
	  size++;
	}
    }
  return size;
}


/* Output to FILE the asm instructions to copy r29:r28 to SPH:SPL,
   handling various cases of interrupt enable flag state BEFORE and AFTER
   (0=disabled, 1=enabled, -1=unknown/unchanged) and target_flags.
   Returns the number of instructions generated.  */

static int
out_set_stack_ptr (file, before, after)
     FILE *file;
     int before;
     int after;
{
  int do_sph, do_cli, do_save, do_sei, lock_sph, size;

  /* The logic here is so that -mno-interrupts actually means
     "it is safe to write SPH in one instruction, then SPL in the
     next instruction, without disabling interrupts first".
     The after != -1 case (interrupt/signal) is not affected.  */

  do_sph = !TARGET_TINY_STACK;
  lock_sph = do_sph && !TARGET_NO_INTERRUPTS;
  do_cli = (before != 0 && (after == 0 || lock_sph));
  do_save = (do_cli && before == -1 && after == -1);
  do_sei = ((do_cli || before != 1) && after == 1);
  size = 1;

  if (do_save)
    {
      fprintf (file, AS2 (in, __tmp_reg__, __SREG__) CR_TAB);
      size++;
    }

  if (do_cli)
    {
      fprintf (file, "cli" CR_TAB);
      size++;
    }

  /* Do SPH first - maybe this will disable interrupts for one instruction
     someday (a suggestion has been sent to avr@atmel.com for consideration
     in future devices - that would make -mno-interrupts always safe).  */
  if (do_sph)
    {
      fprintf (file, AS2 (out, __SP_H__, r29) CR_TAB);
      size++;
    }

  /* Set/restore the I flag now - interrupts will be really enabled only
     after the next instruction.  This is not clearly documented, but
     believed to be true for all AVR devices.  */
  if (do_save)
    {
      fprintf (file, AS2 (out, __SREG__, __tmp_reg__) CR_TAB);
      size++;
    }
  else if (do_sei)
    {
      fprintf (file, "sei" CR_TAB);
      size++;
    }

  fprintf (file, AS2 (out, __SP_L__, r28) "\n");

  return size;
}


/* Output function prologue */

static void
avr_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  int reg;
  int interrupt_func_p;
  int signal_func_p;
  int leaf_func_p;
  int main_p;
  int live_seq;
  int minimize;
  
  if (avr_naked_function_p (current_function_decl))
    {
      fprintf (file, "/* prologue: naked */\n");
      return;
    }

  interrupt_func_p = interrupt_function_p (current_function_decl);
  signal_func_p = signal_function_p (current_function_decl);
  leaf_func_p = leaf_function_p ();
  main_p = MAIN_NAME_P (DECL_NAME (current_function_decl));
  live_seq = sequent_regs_live ();
  minimize = (TARGET_CALL_PROLOGUES
	      && !interrupt_func_p && !signal_func_p && live_seq);

  last_insn_address = 0;
  jump_tables_size = 0;
  prologue_size = 0;
  fprintf (file, "/* prologue: frame size=%d */\n", size);
  
  if (interrupt_func_p)
    {
      fprintf (file,"\tsei\n");
      ++prologue_size;
    }
  if (interrupt_func_p | signal_func_p)
    {
      fprintf (file, "\t"
               AS1 (push,__zero_reg__)   CR_TAB
               AS1 (push,__tmp_reg__)    CR_TAB
	       AS2 (in,__tmp_reg__,__SREG__) CR_TAB
	       AS1 (push,__tmp_reg__)    CR_TAB
	       AS1 (clr,__zero_reg__)    "\n");
      prologue_size += 5;
    }
  if (main_p)
    {
      fprintf (file, ("\t" 
		      AS2 (ldi,r28,lo8(%s - %d)) CR_TAB
		      AS2 (ldi,r29,hi8(%s - %d)) CR_TAB
		      AS2 (out,__SP_H__,r29)     CR_TAB
		      AS2 (out,__SP_L__,r28) "\n"),
	       avr_init_stack, size, avr_init_stack, size);
      
      prologue_size += 4;
    }
  else if (minimize && (frame_pointer_needed || live_seq > 6)) 
    {
      fprintf (file, ("\t"
		      AS2 (ldi, r26, lo8(%d)) CR_TAB
		      AS2 (ldi, r27, hi8(%d)) CR_TAB), size, size);

      fprintf (file, (AS2 (ldi, r30, pm_lo8(.L_%s_body)) CR_TAB
		      AS2 (ldi, r31, pm_hi8(.L_%s_body)) CR_TAB)
	       ,current_function_name, current_function_name);
      
      prologue_size += 4;
      
      if (AVR_MEGA)
	{
	  fprintf (file, AS1 (jmp,__prologue_saves__+%d) "\n",
		   (18 - live_seq) * 2);
	  prologue_size += 2;
	}
      else
	{
	  fprintf (file, AS1 (rjmp,__prologue_saves__+%d) "\n",
		   (18 - live_seq) * 2);
	  ++prologue_size;
	}
      fprintf (file, ".L_%s_body:\n", current_function_name);
    }
  else
    {
      for (reg = 0; reg < 32; ++reg)
	{
	  if ((!leaf_func_p
	       && (call_used_regs[reg]
		   && (interrupt_func_p || signal_func_p)
		   && !(reg == TMP_REGNO || reg == ZERO_REGNO)))
	      || (regs_ever_live[reg]
		  && (!call_used_regs[reg]
		      || interrupt_func_p || signal_func_p)
		  && ! (frame_pointer_needed
			&& (reg == REG_Y || reg == (REG_Y+1)))))
	    {
	      fprintf (file, "\t" AS1 (push,%s) "\n", avr_regnames[reg]);
	      ++prologue_size;
	    }
	}
      if (frame_pointer_needed)
	{
	  {
	    fprintf (file, "\t"
		     AS1 (push,r28) CR_TAB
		     AS1 (push,r29) CR_TAB
		     AS2 (in,r28,__SP_L__) CR_TAB
		     AS2 (in,r29,__SP_H__) "\n");
	    prologue_size += 4;
	    if (size)
	      {
		fputs ("\t", file);
		prologue_size += out_adj_frame_ptr (file, size);

		if (interrupt_func_p)
		  {
		    prologue_size += out_set_stack_ptr (file, 1, 1);
		  }
		else if (signal_func_p)
		  {
		    prologue_size += out_set_stack_ptr (file, 0, 0);
		  }
		else
		  {
		    prologue_size += out_set_stack_ptr (file, -1, -1);
		  }
	      }
	  }
	}
    }
  fprintf (file, "/* prologue end (size=%d) */\n", prologue_size);
}

/* Output function epilogue */

static void
avr_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  int reg;
  int interrupt_func_p;
  int signal_func_p;
  int leaf_func_p;
  int main_p;
  int function_size;
  int live_seq;
  int minimize;

  if (avr_naked_function_p (current_function_decl))
    {
      fprintf (file, "/* epilogue: naked */\n");
      return;
    }

  interrupt_func_p = interrupt_function_p (current_function_decl);
  signal_func_p = signal_function_p (current_function_decl);
  leaf_func_p = leaf_function_p ();
  main_p = MAIN_NAME_P (DECL_NAME (current_function_decl));
  function_size = (INSN_ADDRESSES (INSN_UID (get_last_insn ()))
		   - INSN_ADDRESSES (INSN_UID (get_insns ())));
  function_size += jump_tables_size;
  live_seq = sequent_regs_live ();
  minimize = (TARGET_CALL_PROLOGUES
	      && !interrupt_func_p && !signal_func_p && live_seq);
  
  epilogue_size = 0;
  fprintf (file, "/* epilogue: frame size=%d */\n", size);
  if (main_p)
    {
      fprintf (file, "__stop_progIi__:\n\trjmp __stop_progIi__\n");
      ++epilogue_size;
    }
  else if (minimize && (frame_pointer_needed || live_seq > 4))
    {
      fprintf (file, ("\t" AS2 (ldi, r30, %d) CR_TAB), live_seq);
      ++epilogue_size;
      if (frame_pointer_needed)
	{
	  epilogue_size += out_adj_frame_ptr (file, -size);
	}
      else
	{
	  fprintf (file, (AS2 (in , r28, __SP_L__) CR_TAB
			  AS2 (in , r29, __SP_H__) CR_TAB));
	  epilogue_size += 2;
	}
      
      if (AVR_MEGA)
	{
	  fprintf (file, AS1 (jmp,__epilogue_restores__+%d) "\n",
		   (18 - live_seq) * 2);
	  epilogue_size += 2;
	}
      else
	{
	  fprintf (file, AS1 (rjmp,__epilogue_restores__+%d) "\n",
		   (18 - live_seq) * 2);
	  ++epilogue_size;
	}
    }
  else
    {
      if (frame_pointer_needed)
	{
	  if (size)
	    {
	      fputs ("\t", file);
	      epilogue_size += out_adj_frame_ptr (file, -size);

	      if (interrupt_func_p | signal_func_p)
		{
		  epilogue_size += out_set_stack_ptr (file, -1, 0);
		}
	      else
		{
		  epilogue_size += out_set_stack_ptr (file, -1, -1);
		}
	    }
	  fprintf (file, "\t"
		   AS1 (pop,r29) CR_TAB
		   AS1 (pop,r28) "\n");
	  epilogue_size += 2;
	}

      for (reg = 31; reg >= 0; --reg)
	{
	  if ((!leaf_func_p
	       && (call_used_regs[reg]
		   && (interrupt_func_p || signal_func_p)
		   && !(reg == TMP_REGNO || reg == ZERO_REGNO)))
	      || (regs_ever_live[reg]
		  && (!call_used_regs[reg]
		      || interrupt_func_p || signal_func_p)
		  && ! (frame_pointer_needed
			&& (reg == REG_Y || reg == (REG_Y+1)))))
	    {
	      fprintf (file, "\t" AS1 (pop,%s) "\n", avr_regnames[reg]);
	      ++epilogue_size;
	    }
	}
      
      if (interrupt_func_p | signal_func_p)
	{
	  fprintf (file, "\t"
		   AS1 (pop,__tmp_reg__)      CR_TAB
		   AS2 (out,__SREG__,__tmp_reg__) CR_TAB
		   AS1 (pop,__tmp_reg__)      CR_TAB
		   AS1 (pop,__zero_reg__)     "\n");
	  epilogue_size += 4;
	  fprintf (file, "\treti\n");
	}
      else
	fprintf (file, "\tret\n");
      ++epilogue_size;
    }
  
  fprintf (file, "/* epilogue end (size=%d) */\n", epilogue_size);
  fprintf (file, "/* function %s size %d (%d) */\n", current_function_name,
	   prologue_size + function_size + epilogue_size, function_size);
  commands_in_file += prologue_size + function_size + epilogue_size;
  commands_in_prologues += prologue_size;
  commands_in_epilogues += epilogue_size;
}


/* Return nonzero if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.  */

int
legitimate_address_p (mode, x, strict)
     enum machine_mode mode;
     rtx x;
     int strict;
{
  enum reg_class r = NO_REGS;
  
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "mode: (%s) %s %s %s %s:",
	       GET_MODE_NAME(mode),
	       strict ? "(strict)": "",
	       reload_completed ? "(reload_completed)": "",
	       reload_in_progress ? "(reload_in_progress)": "",
	       reg_renumber ? "(reg_renumber)" : "");
      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) <= MAX_LD_OFFSET (mode)
	  && reg_renumber
	  )
	fprintf (stderr, "(r%d ---> r%d)", REGNO (XEXP (x, 0)),
		 true_regnum (XEXP (x, 0)));
      debug_rtx (x);
    }
  if (REG_P (x) && (strict ? REG_OK_FOR_BASE_STRICT_P (x)
                    : REG_OK_FOR_BASE_NOSTRICT_P (x)))
    r = POINTER_REGS;
  else if (CONSTANT_ADDRESS_P (x))
    r = ALL_REGS;
  else if (GET_CODE (x) == PLUS
           && REG_P (XEXP (x, 0))
	   && GET_CODE (XEXP (x, 1)) == CONST_INT
	   && INTVAL (XEXP (x, 1)) >= 0)
    {
      int fit = INTVAL (XEXP (x, 1)) <= MAX_LD_OFFSET (mode);
      if (fit)
	{
	  if (! strict
	      || REGNO (XEXP (x,0)) == REG_Y
	      || REGNO (XEXP (x,0)) == REG_Z)
	    r = BASE_POINTER_REGS;
	  if (XEXP (x,0) == frame_pointer_rtx
	      || XEXP (x,0) == arg_pointer_rtx)
	    r = BASE_POINTER_REGS;
	}
      else if (frame_pointer_needed && XEXP (x,0) == frame_pointer_rtx)
	r = POINTER_Y_REGS;
    }
  else if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == POST_INC)
           && REG_P (XEXP (x, 0))
           && (strict ? REG_OK_FOR_BASE_STRICT_P (XEXP (x, 0))
               : REG_OK_FOR_BASE_NOSTRICT_P (XEXP (x, 0))))
    {
      r = POINTER_REGS;
    }
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "   ret = %c\n", r);
    }
  return r == NO_REGS ? 0 : (int)r;
}

/* Attempts to replace X with a valid
   memory address for an operand of mode MODE  */

rtx
legitimize_address (x, oldx, mode)
     rtx x;
     rtx oldx;
     enum machine_mode mode;
{
  x = oldx;
  if (TARGET_ALL_DEBUG)
    {
      fprintf (stderr, "legitimize_address mode: %s", GET_MODE_NAME(mode));
      debug_rtx (oldx);
    }
  
  if (GET_CODE (oldx) == PLUS
      && REG_P (XEXP (oldx,0)))
    {
      if (REG_P (XEXP (oldx,1)))
	x = force_reg (GET_MODE (oldx), oldx);
      else if (GET_CODE (XEXP (oldx, 1)) == CONST_INT)
	{
	  int offs = INTVAL (XEXP (oldx,1));
	  if (frame_pointer_rtx != XEXP (oldx,0))
	    if (offs > MAX_LD_OFFSET (mode))
	      {
		if (TARGET_ALL_DEBUG)
		  fprintf (stderr, "force_reg (big offset)\n");
		x = force_reg (GET_MODE (oldx), oldx);
	      }
	}
    }
  return x;
}


/* Return a pointer register name as a string */

static const char *
ptrreg_to_str (regno)
     int regno;
{
  switch (regno)
    {
    case REG_X: return "X";
    case REG_Y: return "Y";
    case REG_Z: return "Z";
    default:
      abort ();
    }
  return NULL;
}

/* Return the condition name as a string.
   Used in conditional jump constructing  */

static const char *
cond_string (code)
     enum rtx_code code;
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return "pl";
      else
	return "ge";
    case LT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return "mi";
      else
	return "lt";
    case GEU:
      return "sh";
    case LTU:
      return "lo";
    default:
      abort ();
    }
}

/* Output ADDR to FILE as address */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, ptrreg_to_str (REGNO (addr)));
      break;

    case PRE_DEC:
      fprintf (file, "-%s", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    case POST_INC:
      fprintf (file, "%s+", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    default:
      if (CONSTANT_ADDRESS_P (addr)
	  && (SYMBOL_REF_FLAG (addr) || GET_CODE (addr) == LABEL_REF))
	{
	  fprintf (file, "pm(");
	  output_addr_const (file,addr);
	  fprintf (file ,")");
	}
      else
	output_addr_const (file, addr);
    }
}


/* Output X as assembler operand to file FILE */
     
void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  int abcd = 0;

  if (code >= 'A' && code <= 'D')
    abcd = code - 'A';

  if (code == '~')
    {
      if (!AVR_MEGA)
	fputc ('r', file);
    }
  else if (REG_P (x))
    {
      if (x == zero_reg_rtx)
	fprintf (file, "__zero_reg__");
      else
	fprintf (file, reg_names[true_regnum (x) + abcd]);
    }
  else if (GET_CODE (x) == CONST_INT)
    fprintf (file, "%d", INTVAL (x) + abcd);
  else if (GET_CODE (x) == MEM)
    {
      rtx addr = XEXP (x,0);

      if (CONSTANT_P (addr) && abcd)
	{
	  fputc ('(', file);
	  output_address (addr);
	  fprintf (file, ")+%d", abcd);
	}
      else if (code == 'o')
	{
	  if (GET_CODE (addr) != PLUS)
	    fatal_insn ("bad address, not (reg+disp):", addr);

	  print_operand (file, XEXP (addr, 1), 0);
	}
      else if (GET_CODE (addr) == PLUS)
	{
	  print_operand_address (file, XEXP (addr,0));
	  if (REGNO (XEXP (addr, 0)) == REG_X)
	    fatal_insn ("internal compiler error.  Bad address:"
			,addr);
	  fputc ('+', file);
	  print_operand (file, XEXP (addr,1), code);
	}
      else
	print_operand_address (file, addr);
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    {
      long val;
      REAL_VALUE_TYPE rv;
      if (GET_MODE (x) != SFmode)
	fatal_insn ("internal compiler error.  Unknown mode:", x);
      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, val);
      asm_fprintf (file, "0x%lx", val);
    }
  else if (code == 'j')
    asm_fprintf (file, cond_string (GET_CODE (x)));
  else if (code == 'k')
    asm_fprintf (file, cond_string (reverse_condition (GET_CODE (x))));
  else
    print_operand_address (file, x);
}

/* Recognize operand OP of mode MODE used in call instructions */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (register_operand (inside, Pmode))
        return 1;
      if (CONSTANT_ADDRESS_P (inside))
        return 1;
    }
  return 0;
}

/* Update the condition code in the INSN.  */

void
notice_update_cc (body, insn)
     rtx body ATTRIBUTE_UNUSED;
     rtx insn;
{
  rtx set;
  
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

    case CC_SET_N:
      CC_STATUS_INIT;
      break;

    case CC_SET_ZN:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
	{
	  cc_status.flags |= CC_NO_OVERFLOW;
	  cc_status.value1 = SET_DEST (set);
	}
      break;

    case CC_SET_CZN:
      /* Insn sets the Z,N,C flags of CC to recog_operand[0].
         The V flag may or may not be known but that's ok because
         alter_cond will change tests to use EQ/NE.  */
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
	{
	  cc_status.value1 = SET_DEST (set);
	  cc_status.flags |= CC_OVERFLOW_UNUSABLE;
	}
      break;

    case CC_COMPARE:
      set = single_set (insn);
      CC_STATUS_INIT;
      if (set)
	cc_status.value1 = SET_SRC (set);
      break;
      
    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;

      /* Correct CC for the ashrqi3 with the shift count as CONST_INT != 6 */
      set = single_set (insn);
      if (set)
	{
	  rtx src = SET_SRC (set);
	  
	  if (GET_CODE (src) == ASHIFTRT
	      && GET_MODE (src) == QImode)
	    {
	      rtx x = XEXP (src, 1);

	      if (GET_CODE (x) == CONST_INT
		  && INTVAL (x) != 6)
		{
		  cc_status.value1 = SET_DEST (set);
		  cc_status.flags |= CC_OVERFLOW_UNUSABLE;
		}
	    }
	}
      break;
    }
}

/* Return maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.  */

int
class_max_nregs (class, mode)
     enum reg_class class ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
}

/* Choose mode for jump insn:
   1 - relative jump in range -63 <= x <= 62 ;
   2 - relative jump in range -2046 <= x <= 2045 ;
   3 - absolute jump (only for ATmega[16]03).  */

int
avr_jump_mode (x, insn)
     rtx x;                     /* jump operand */
     rtx insn;                  /* jump insn */
{
  int dest_addr = INSN_ADDRESSES (INSN_UID (GET_MODE (x) == LABEL_REF
					    ? XEXP (x, 0) : x));
  int cur_addr = INSN_ADDRESSES (INSN_UID (insn));
  int jump_distance = cur_addr - dest_addr;
  
  if (-63 <= jump_distance && jump_distance <= 62)
    return 1;
  else if (-2046 <= jump_distance && jump_distance <= 2045)
    return 2;
  else if (AVR_MEGA)
    return 3;
  
  return 2;
}

/* return an AVR condition jump commands.
   X is a comparison RTX.
   LEN is a number returned by avr_jump_mode function.
   if REVERSE nonzero then condition code in X must be reversed.  */

const char *
ret_cond_branch (x, len, reverse)
     rtx x;
     int len;
     int reverse;
{
  RTX_CODE cond = reverse ? reverse_condition (GET_CODE (x)) : GET_CODE (x);
  
  switch (cond)
    {
    case GT:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brpl,%0)) :
		len == 2 ? (AS1 (breq,.+4) CR_TAB
			    AS1 (brmi,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+6) CR_TAB
		 AS1 (brmi,.+4) CR_TAB
		 AS1 (jmp,%0)));
	  
      else
	return (len == 1 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brge,%0)) :
		len == 2 ? (AS1 (breq,.+4) CR_TAB
			    AS1 (brlt,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+6) CR_TAB
		 AS1 (brlt,.+4) CR_TAB
		 AS1 (jmp,%0)));
    case GTU:
      return (len == 1 ? (AS1 (breq,.+2) CR_TAB
                          AS1 (brsh,%0)) :
              len == 2 ? (AS1 (breq,.+4) CR_TAB
                          AS1 (brlo,.+2) CR_TAB
                          AS1 (rjmp,%0)) :
              (AS1 (breq,.+6) CR_TAB
               AS1 (brlo,.+4) CR_TAB
               AS1 (jmp,%0)));
    case LE:
      if (cc_prev_status.flags & CC_OVERFLOW_UNUSABLE)
	return (len == 1 ? (AS1 (breq,%0) CR_TAB
			    AS1 (brmi,%0)) :
		len == 2 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brpl,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+2) CR_TAB
		 AS1 (brpl,.+4) CR_TAB
		 AS1 (jmp,%0)));
      else
	return (len == 1 ? (AS1 (breq,%0) CR_TAB
			    AS1 (brlt,%0)) :
		len == 2 ? (AS1 (breq,.+2) CR_TAB
			    AS1 (brge,.+2) CR_TAB
			    AS1 (rjmp,%0)) :
		(AS1 (breq,.+2) CR_TAB
		 AS1 (brge,.+4) CR_TAB
		 AS1 (jmp,%0)));
    case LEU:
      return (len == 1 ? (AS1 (breq,%0) CR_TAB
                          AS1 (brlo,%0)) :
              len == 2 ? (AS1 (breq,.+2) CR_TAB
                          AS1 (brsh,.+2) CR_TAB
			  AS1 (rjmp,%0)) :
              (AS1 (breq,.+2) CR_TAB
               AS1 (brsh,.+4) CR_TAB
	       AS1 (jmp,%0)));
    default:
      if (reverse)
	{
	  switch (len)
	    {
	    case 1:
	      return AS1 (br%k1,%0);
	    case 2:
	      return (AS1 (br%j1,.+2) CR_TAB
		      AS1 (rjmp,%0));
	    default:
	      return (AS1 (br%j1,.+4) CR_TAB
		      AS1 (jmp,%0));
	    }
	}
	else
	  {
	    switch (len)
	      {
	      case 1:
		return AS1 (br%j1,%0);
	      case 2:
		return (AS1 (br%k1,.+2) CR_TAB
			AS1 (rjmp,%0));
	      default:
		return (AS1 (br%k1,.+4) CR_TAB
			AS1 (jmp,%0));
	      }
	  }
    }
  return "";
}

/* Predicate function for immediate operand which fits to byte (8bit) */

int
byte_immediate_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
          && INTVAL (op) <= 0xff && INTVAL (op) >= 0);
}

/* Output all insn addresses and their sizes into the assembly language
   output file.  This is helpful for debugging whether the length attributes
   in the md file are correct.
   Output insn cost for next insn.  */

void
final_prescan_insn (insn, operand, num_operands)
     rtx insn, *operand ATTRIBUTE_UNUSED;
     int num_operands ATTRIBUTE_UNUSED;
{
  int uid = INSN_UID (insn);

  if (TARGET_INSN_SIZE_DUMP || TARGET_ALL_DEBUG)
    {
      fprintf (asm_out_file, "/*DEBUG: 0x%x\t\t%d\t%d */\n",
	       INSN_ADDRESSES (uid),
               INSN_ADDRESSES (uid) - last_insn_address,
	       rtx_cost (PATTERN (insn), INSN));
    }
  last_insn_address = INSN_ADDRESSES (uid);

  if (TARGET_RTL_DUMP)
    {
      fprintf (asm_out_file, "/*****************\n");
      print_rtl_single (asm_out_file, insn);
      fprintf (asm_out_file, "*****************/\n");
    }
}

/* Return 0 if undefined, 1 if always true or always false.  */

int
avr_simplify_comparision_p (mode, operator, x)
     enum machine_mode mode;
     RTX_CODE operator;
     rtx x;
{
  unsigned int max = (mode == QImode ? 0xff :
                      mode == HImode ? 0xffff :
                      mode == SImode ? 0xffffffff : 0);
  if (max && operator && GET_CODE (x) == CONST_INT)
    {
      if (unsigned_condition (operator) != operator)
	max >>= 1;

      if (max != (INTVAL (x) & max)
	  && INTVAL (x) != 0xff)
	return 1;
    }
  return 0;
}


/* Returns nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */

int
function_arg_regno_p(r)
     int r;
{
  return (r >= 8 && r <= 25);
}

/* Initializing the variable cum for the state at the beginning
   of the argument list.  */

void
init_cumulative_args (cum, fntype, libname, indirect)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname;
     int indirect ATTRIBUTE_UNUSED;
{
  cum->nregs = 18;
  cum->regno = FIRST_CUM_REG;
  if (!libname)
    {
      int stdarg = (TYPE_ARG_TYPES (fntype) != 0
                    && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
                        != void_type_node));
      if (stdarg)
        cum->nregs = 0;
    }
}

/* Returns the number of registers to allocate for a function argument.  */

static int
avr_num_arg_regs (mode, type)
     enum machine_mode mode;
     tree type;
{
  int size;

  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  /* Align all function arguments to start in even-numbered registers.
     Odd-sized arguments leave holes above them.  */

  return (size + 1) & ~1;
}

/* Controls whether a function argument is passed
   in a register, and which register. */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int bytes = avr_num_arg_regs (mode, type);

  if (cum->nregs && bytes <= cum->nregs)
    return gen_rtx (REG, mode, cum->regno - bytes);

  return NULL_RTX;
}

/* Update the summarizer variable CUM to advance past an argument
   in the argument list.  */
   
void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;      /* current arg information */
     enum machine_mode mode;    /* current arg mode */
     tree type;                 /* type of the argument or 0 if lib support */
     int named ATTRIBUTE_UNUSED; /* whether or not the argument was named */
{
  int bytes = avr_num_arg_regs (mode, type);

  cum->nregs -= bytes;
  cum->regno -= bytes;

  if (cum->nregs <= 0)
    {
      cum->nregs = 0;
      cum->regno = FIRST_CUM_REG;
    }
}

/***********************************************************************
  Functions for outputting various mov's for a various modes
************************************************************************/
const char *
output_movqi (insn, operands, l)
     rtx insn;
     rtx operands[];
     int *l;
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;
  
  if (!l)
    l = &dummy;

  *l = 1;
  
  if (register_operand (dest, QImode))
    {
      if (register_operand (src, QImode)) /* mov r,r */
	{
	  if (test_hard_reg_class (STACK_REG, dest))
	    return AS2 (out,%0,%1);
	  else if (test_hard_reg_class (STACK_REG, src))
	    return AS2 (in,%0,%1);
	  
	  return AS2 (mov,%0,%1);
	}
      else if (CONSTANT_P (src))
	{
	  if (test_hard_reg_class (LD_REGS, dest)) /* ldi d,i */
	    return AS2 (ldi,%0,lo8(%1));
	  
	  if (GET_CODE (src) == CONST_INT)
	    {
	      if (src == const0_rtx) /* mov r,L */
		return AS1 (clr,%0);
	      else if (src == const1_rtx)
		{
		  if (reg_was_0 (insn, dest))
		    return AS1 (inc,%0 ; reg_was_0);

		  *l = 2;
		  return (AS1 (clr,%0) CR_TAB
			  AS1 (inc,%0));
		}
	      else if (src == constm1_rtx)
		{
		  /* Immediate constants -1 to any register */
		  if (reg_was_0 (insn, dest))
		    return AS1 (dec,%0 ; reg_was_0);

		  *l = 2;
		  return (AS1 (clr,%0) CR_TAB
			  AS1 (dec,%0));
		}
	      else
		{
		  int bit_nr = exact_log2 (INTVAL (src));

		  if (bit_nr >= 0)
		    {
		      if (reg_was_0 (insn, dest))
			{
			  *l = 2;
			  if (!real_l)
			    output_asm_insn ("set ; reg_was_0", operands);
			}
		      else
			{
			  *l = 3;
			  if (!real_l)
			    output_asm_insn ((AS1 (clr,%0) CR_TAB
					      "set"), operands);
			}
		      if (!real_l)
			avr_output_bld (operands, bit_nr);

		      return "";
		    }
		}
	    }
	  
	  /* Last resort, larger than loading from memory.  */
	  *l = 4;
	  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
		  AS2 (ldi,r31,lo8(%1))     CR_TAB
		  AS2 (mov,%0,r31)          CR_TAB
		  AS2 (mov,r31,__tmp_reg__));
	}
      else if (GET_CODE (src) == MEM)
	return out_movqi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (GET_CODE (dest) == MEM)
    {
      const char *template;

      if (src == const0_rtx)
	operands[1] = zero_reg_rtx;

      template = out_movqi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (template, operands);

      operands[1] = src;
    }
  return "";
}


const char *
output_movhi (insn, operands, l)
     rtx insn;
     rtx operands[];
     int *l;
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;
  
  if (!l)
    l = &dummy;
  
  if (register_operand (dest, HImode))
    {
      if (register_operand (src, HImode)) /* mov r,r */
	{
	  if (test_hard_reg_class (STACK_REG, dest))
	    {
	      if (TARGET_TINY_STACK)
		{
		  *l = 1;
		  return AS2 (out,__SP_L__,%A1);
		}
	      else if (TARGET_NO_INTERRUPTS)
		{
		  *l = 2;
		  return (AS2 (out,__SP_H__,%B1) CR_TAB
			  AS2 (out,__SP_L__,%A1));
		}

	      *l = 5;
	      return (AS2 (in,__tmp_reg__,__SREG__)  CR_TAB
		      "cli"                          CR_TAB
		      AS2 (out,__SP_H__,%B1)         CR_TAB
		      AS2 (out,__SREG__,__tmp_reg__) CR_TAB
		      AS2 (out,__SP_L__,%A1));
	    }
	  else if (test_hard_reg_class (STACK_REG, src))
	    {
	      *l = 2;	
	      return (AS2 (in,%A0,__SP_L__) CR_TAB
		      AS2 (in,%B0,__SP_H__));
	    }

	  if (AVR_ENHANCED)
	    {
	      *l = 1;
	      return (AS2 (movw,%0,%1));
	    }

	  if (true_regnum (dest) > true_regnum (src))
	    {
	      *l = 2;
	      return (AS2 (mov,%B0,%B1) CR_TAB
		      AS2 (mov,%A0,%A1));
	    }
	  else
	    {
	      *l = 2;
	      return (AS2 (mov,%A0,%A1) CR_TAB
		      AS2 (mov,%B0,%B1));
	    }
	}
      else if (CONSTANT_P (src))
	{
	  if (test_hard_reg_class (LD_REGS, dest)) /* ldi d,i */
	    {
	      if (byte_immediate_operand (src, HImode)
		  && reg_was_0 (insn, dest))
		{
		  *l = 1;
		  return (AS2 (ldi,%A0,lo8(%1) ; reg_was_0));
		}

	      *l = 2;
	      return (AS2 (ldi,%A0,lo8(%1)) CR_TAB
		      AS2 (ldi,%B0,hi8(%1)));
	    }
	  
	  if (GET_CODE (src) == CONST_INT)
	    {
	      if (src == const0_rtx) /* mov r,L */
		{
		  *l = 2;
		  return (AS1 (clr,%A0) CR_TAB
			  AS1 (clr,%B0));
		}
	      else if (src == const1_rtx)
		{
		  if (reg_was_0 (insn, dest))
		    {
		      *l = 1;
		      return AS1 (inc,%0 ; reg_was_0);
		    }

		  *l = 3;
		  return (AS1 (clr,%A0) CR_TAB
			  AS1 (clr,%B0) CR_TAB
			  AS1 (inc,%A0));
		}
	      else if (src == constm1_rtx)
		{
		  /* Immediate constants -1 to any register */
		  if (reg_was_0 (insn, dest))
		    {
		      *l = 2;
		      return (AS1 (dec,%A0 ; reg_was_0) CR_TAB
			      AS1 (dec,%B0));
		    }

		  *l = 3;
		  return (AS1 (clr,%0)  CR_TAB
			  AS1 (dec,%A0) CR_TAB
			  AS2 (mov,%B0,%A0));
		}
	      else
		{
		  int bit_nr = exact_log2 (INTVAL (src));

		  if (bit_nr >= 0)
		    {
		      if (reg_was_0 (insn, dest))
			{
			  *l = 2;
			  if (!real_l)
			    output_asm_insn ("set ; reg_was_0", operands);
			}
		      else
			{
			  *l = 4;
			  if (!real_l)
			    output_asm_insn ((AS1 (clr,%A0) CR_TAB
					      AS1 (clr,%B0) CR_TAB
					      "set"), operands);
			}
		      if (!real_l)
			avr_output_bld (operands, bit_nr);

		      return "";
		    }
		}

	      if ((INTVAL (src) & 0xff) == 0)
		{
		  *l = 5;
		  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
			  AS1 (clr,%A0)             CR_TAB
			  AS2 (ldi,r31,hi8(%1))     CR_TAB
			  AS2 (mov,%B0,r31)         CR_TAB
			  AS2 (mov,r31,__tmp_reg__));
		}
	      else if ((INTVAL (src) & 0xff00) == 0)
		{
		  *l = 5;
		  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
			  AS2 (ldi,r31,lo8(%1))     CR_TAB
			  AS2 (mov,%A0,r31)         CR_TAB
			  AS1 (clr,%B0)             CR_TAB
			  AS2 (mov,r31,__tmp_reg__));
		}
	    }
	  
	  /* Last resort, equal to loading from memory.  */
	  *l = 6;
	  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
		  AS2 (ldi,r31,lo8(%1))     CR_TAB
		  AS2 (mov,%A0,r31)         CR_TAB
		  AS2 (ldi,r31,hi8(%1))     CR_TAB
		  AS2 (mov,%B0,r31)         CR_TAB
		  AS2 (mov,r31,__tmp_reg__));
	}
      else if (GET_CODE (src) == MEM)
	return out_movhi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (GET_CODE (dest) == MEM)
    {
      const char *template;

      if (src == const0_rtx)
	operands[1] = zero_reg_rtx;

      template = out_movhi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (template, operands);

      operands[1] = src;
      return "";
    }
  fatal_insn ("invalid insn:", insn);
  return "";
}

const char *
out_movqi_r_mr (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);
  int dummy;
  
  if (!l)
    l = &dummy;
  
  if (CONSTANT_ADDRESS_P (x))
    {
      if (io_address_p (x, 1))
	{
	  *l = 1;
	  return AS2 (in,%0,%1-0x20);
	}
      *l = 2;
      return AS2 (lds,%0,%1);
    }
  /* memory access by reg+disp */
  else if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x,0))
      && GET_CODE (XEXP (x,1)) == CONST_INT)
    {
      if ((INTVAL (XEXP (x,1)) - GET_MODE_SIZE (GET_MODE (src))) >= 63)
	{
	  int disp = INTVAL (XEXP (x,1));
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 3, (AS2 (adiw,r28,%o1-63) CR_TAB
			    AS2 (ldd,%0,Y+63)     CR_TAB
			    AS2 (sbiw,r28,%o1-63));

	  return *l = 5, (AS2 (subi,r28,lo8(-%o1)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o1)) CR_TAB
			  AS2 (ld,%0,Y)            CR_TAB
			  AS2 (subi,r28,lo8(%o1))  CR_TAB
			  AS2 (sbci,r29,hi8(%o1)));
	}
      else if (REGNO (XEXP (x,0)) == REG_X)
	{
	  /* This is a paranoid case LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal optimizing options.  */
	  if (reg_overlap_mentioned_p (dest, XEXP (x,0))
	      || reg_unused_after (insn, XEXP (x,0)))
	    return *l = 2, (AS2 (adiw,r26,%o1) CR_TAB
			    AS2 (ld,%0,X));

	  return *l = 3, (AS2 (adiw,r26,%o1) CR_TAB
			  AS2 (ld,%0,X)      CR_TAB
			  AS2 (sbiw,r26,%o1));
	}
      *l = 1;
      return AS2 (ldd,%0,%1);
    }
  *l = 1;
  return AS2 (ld,%0,%1);
}

const char *
out_movhi_r_mr (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  int tmp;

  if (!l)
    l = &tmp;

  if (reg_base > 0)
    {
      if (reg_dest == reg_base)         /* R = (R) */
	{
	  *l = 3;
	  return (AS2 (ld,__tmp_reg__,%1+) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (mov,%A0,__tmp_reg__));
	}
      else if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_unused_after (insn, base))
	    {
	      *l = 2;
	      return (AS2 (ld,%A0,X+) CR_TAB
		      AS2 (ld,%B0,X));
	    }
	  *l  = 3;
	  return (AS2 (ld,%A0,X+) CR_TAB
		  AS2 (ld,%B0,X) CR_TAB
		  AS2 (sbiw,r26,1));
        }
      else                      /* (R)  */
	{
	  *l = 2;
	  return (AS2 (ld,%A0,%1)    CR_TAB
		  AS2 (ldd,%B0,%1+1));
	}
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      int reg_base = true_regnum (XEXP (base, 0));
      
      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);
	  
	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 4, (AS2 (adiw,r28,%o1-62) CR_TAB
			    AS2 (ldd,%A0,Y+62)    CR_TAB
			    AS2 (ldd,%B0,Y+63)    CR_TAB
			    AS2 (sbiw,r28,%o1-62));

	  return *l = 6, (AS2 (subi,r28,lo8(-%o1)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o1)) CR_TAB
			  AS2 (ld,%A0,Y)           CR_TAB
			  AS2 (ldd,%B0,Y+1)        CR_TAB
			  AS2 (subi,r28,lo8(%o1))  CR_TAB
			  AS2 (sbci,r29,hi8(%o1)));
	}
      if (reg_base == REG_X)
	{
	  /* This is a paranoid case. LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal
	     optimization options.  */
	  
	  *l = 4;
	  if (reg_base == reg_dest)
	    return (AS2 (adiw,r26,%o1)      CR_TAB
		    AS2 (ld,__tmp_reg__,X+) CR_TAB
		    AS2 (ld,%B0,X)          CR_TAB
		    AS2 (mov,%A0,__tmp_reg__));

	  return (AS2 (adiw,r26,%o1) CR_TAB
		  AS2 (ld,%A0,X+)    CR_TAB
		  AS2 (ld,%B0,X)     CR_TAB
		  AS2 (sbiw,r26,%o1+1));
	}

      if (reg_base == reg_dest)
	{
	  *l = 3;
	  return (AS2 (ldd,__tmp_reg__,%A1) CR_TAB
		  AS2 (ldd,%B0,%B1)         CR_TAB
		  AS2 (mov,%A0,__tmp_reg__));
	}
      
      *l = 2;
      return (AS2 (ldd,%A0,%A1) CR_TAB
	      AS2 (ldd,%B0,%B1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
	fatal_insn ("incorrect insn:", insn);

      *l = 2;
      return (AS2 (ld,%B0,%1) CR_TAB
	      AS2 (ld,%A0,%1));
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
	fatal_insn ("incorrect insn:", insn);

      *l = 2;
      return (AS2 (ld,%A0,%1)  CR_TAB
	      AS2 (ld,%B0,%1));
    }
  else if (CONSTANT_ADDRESS_P (base))
    {
      if (io_address_p (base, 2))
	{
	  *l = 2;
	  return (AS2 (in,%A0,%A1-0x20) CR_TAB
		  AS2 (in,%B0,%B1-0x20));
	}
      *l = 4;
      return (AS2 (lds,%A0,%A1) CR_TAB
	      AS2 (lds,%B0,%B1));
    }
  
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
out_movsi_r_mr (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  int tmp;

  if (!l)
    l = &tmp;
  
  if (reg_base > 0)
    {
      if (reg_base == REG_X)        /* (R26) */
        {
          if (reg_dest == REG_X)
	    /* "ld r26,-X" is undefined */
	    return *l=7, (AS2 (adiw,r26,3)        CR_TAB
			  AS2 (ld,r29,X)          CR_TAB
			  AS2 (ld,r28,-X)         CR_TAB
			  AS2 (ld,__tmp_reg__,-X) CR_TAB
			  AS2 (sbiw,r26,1)        CR_TAB
			  AS2 (ld,r26,X)          CR_TAB
			  AS2 (mov,r27,__tmp_reg__));
          else if (reg_dest == REG_X - 2)
            return *l=5, (AS2 (ld,%A0,X+)  CR_TAB
                          AS2 (ld,%B0,X+) CR_TAB
                          AS2 (ld,__tmp_reg__,X+)  CR_TAB
                          AS2 (ld,%D0,X)  CR_TAB
                          AS2 (mov,%C0,__tmp_reg__));
          else if (reg_unused_after (insn, base))
            return  *l=4, (AS2 (ld,%A0,X+)  CR_TAB
                           AS2 (ld,%B0,X+) CR_TAB
                           AS2 (ld,%C0,X+) CR_TAB
                           AS2 (ld,%D0,X));
          else
            return  *l=5, (AS2 (ld,%A0,X+)  CR_TAB
                           AS2 (ld,%B0,X+) CR_TAB
                           AS2 (ld,%C0,X+) CR_TAB
                           AS2 (ld,%D0,X)  CR_TAB
                           AS2 (sbiw,r26,3));
        }
      else
        {
          if (reg_dest == reg_base)
            return *l=5, (AS2 (ldd,%D0,%1+3) CR_TAB
                          AS2 (ldd,%C0,%1+2) CR_TAB
                          AS2 (ldd,__tmp_reg__,%1+1)  CR_TAB
                          AS2 (ld,%A0,%1)  CR_TAB
                          AS2 (mov,%B0,__tmp_reg__));
          else if (reg_base == reg_dest + 2)
            return *l=5, (AS2 (ld ,%A0,%1)    CR_TAB
                          AS2 (ldd,%B0,%1+1) CR_TAB
                          AS2 (ldd,__tmp_reg__,%1+2)  CR_TAB
                          AS2 (ldd,%D0,%1+3) CR_TAB
                          AS2 (mov,%C0,__tmp_reg__));
          else
            return *l=4, (AS2 (ld ,%A0,%1)   CR_TAB
                          AS2 (ldd,%B0,%1+1) CR_TAB
                          AS2 (ldd,%C0,%1+2) CR_TAB
                          AS2 (ldd,%D0,%1+3));
        }
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      
      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 6, (AS2 (adiw,r28,%o1-60) CR_TAB
			    AS2 (ldd,%A0,Y+60)    CR_TAB
			    AS2 (ldd,%B0,Y+61)    CR_TAB
			    AS2 (ldd,%C0,Y+62)    CR_TAB
			    AS2 (ldd,%D0,Y+63)    CR_TAB
			    AS2 (sbiw,r28,%o1-60));

	  return *l = 8, (AS2 (subi,r28,lo8(-%o1)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o1)) CR_TAB
			  AS2 (ld,%A0,Y)           CR_TAB
			  AS2 (ldd,%B0,Y+1)        CR_TAB
			  AS2 (ldd,%C0,Y+2)        CR_TAB
			  AS2 (ldd,%D0,Y+3)        CR_TAB
			  AS2 (subi,r28,lo8(%o1))  CR_TAB
			  AS2 (sbci,r29,hi8(%o1)));
	}

      reg_base = true_regnum (XEXP (base, 0));
      if (reg_base == REG_X)
	{
	  /* R = (X + d) */
	  if (reg_dest == REG_X)
	    {
	      *l = 7;
	      /* "ld r26,-X" is undefined */
	      return (AS2 (adiw,r26,%o1+3)    CR_TAB
		      AS2 (ld,r29,X)          CR_TAB
		      AS2 (ld,r28,-X)         CR_TAB
		      AS2 (ld,__tmp_reg__,-X) CR_TAB
		      AS2 (sbiw,r26,1)        CR_TAB
		      AS2 (ld,r26,X)          CR_TAB
		      AS2 (mov,r27,__tmp_reg__));
	    }
	  *l = 6;
	  if (reg_dest == REG_X - 2)
	    return (AS2 (adiw,r26,%o1)      CR_TAB
		    AS2 (ld,r24,X+)         CR_TAB
		    AS2 (ld,r25,X+)         CR_TAB
		    AS2 (ld,__tmp_reg__,X+) CR_TAB
		    AS2 (ld,r27,X)          CR_TAB
		    AS2 (mov,r26,__tmp_reg__));

	  return (AS2 (adiw,r26,%o1) CR_TAB
		  AS2 (ld,%A0,X+)    CR_TAB
		  AS2 (ld,%B0,X+)    CR_TAB
		  AS2 (ld,%C0,X+)    CR_TAB
		  AS2 (ld,%D0,X)     CR_TAB
		  AS2 (sbiw,r26,%o1+3));
	}
      if (reg_dest == reg_base)
        return *l=5, (AS2 (ldd,%D0,%D1) CR_TAB
                      AS2 (ldd,%C0,%C1) CR_TAB
                      AS2 (ldd,__tmp_reg__,%B1)  CR_TAB
                      AS2 (ldd,%A0,%A1) CR_TAB
                      AS2 (mov,%B0,__tmp_reg__));
      else if (reg_dest == reg_base - 2)
        return *l=5, (AS2 (ldd,%A0,%A1) CR_TAB
                      AS2 (ldd,%B0,%B1) CR_TAB
                      AS2 (ldd,__tmp_reg__,%C1)  CR_TAB
                      AS2 (ldd,%D0,%D1) CR_TAB
                      AS2 (mov,%C0,__tmp_reg__));
      return *l=4, (AS2 (ldd,%A0,%A1) CR_TAB
                    AS2 (ldd,%B0,%B1) CR_TAB
                    AS2 (ldd,%C0,%C1) CR_TAB
                    AS2 (ldd,%D0,%D1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, (AS2 (ld,%D0,%1) CR_TAB
		  AS2 (ld,%C0,%1) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (ld,%A0,%1));
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, (AS2 (ld,%A0,%1) CR_TAB
		  AS2 (ld,%B0,%1) CR_TAB
		  AS2 (ld,%C0,%1) CR_TAB
		  AS2 (ld,%D0,%1));
  else if (CONSTANT_ADDRESS_P (base))
      return *l=8, (AS2 (lds,%A0,%A1) CR_TAB
		    AS2 (lds,%B0,%B1) CR_TAB
		    AS2 (lds,%C0,%C1) CR_TAB
		    AS2 (lds,%D0,%D1));
    
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
out_movsi_mr_r (insn, op, l)
     rtx insn;
     rtx op[];
     int *l;
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  int tmp;
  
  if (!l)
    l = &tmp;
  
  if (CONSTANT_ADDRESS_P (base))
    return *l=8,(AS2 (sts,%A0,%A1) CR_TAB
		 AS2 (sts,%B0,%B1) CR_TAB
		 AS2 (sts,%C0,%C1) CR_TAB
		 AS2 (sts,%D0,%D1));
  if (reg_base > 0)                 /* (r) */
    {
      if (reg_base == REG_X)                /* (R26) */
        {
          if (reg_src == REG_X)
            {
	      /* "st X+,r26" is undefined */
              if (reg_unused_after (insn, base))
		return *l=6, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X+,__tmp_reg__)   CR_TAB
			      AS2 (st,X+,r28)           CR_TAB
			      AS2 (st,X,r29));
              else
                return *l=7, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X+,__tmp_reg__)   CR_TAB
			      AS2 (st,X+,r28)           CR_TAB
			      AS2 (st,X,r29)            CR_TAB
			      AS2 (sbiw,r26,3));
            }
          else if (reg_base == reg_src + 2)
            {
              if (reg_unused_after (insn, base))
                return *l=7, (AS2 (mov,__zero_reg__,%C1) CR_TAB
                              AS2 (mov,__tmp_reg__,%D1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,%B1) CR_TAB
                              AS2 (st,%0+,__zero_reg__)  CR_TAB
                              AS2 (st,%0,__tmp_reg__)   CR_TAB
                              AS1 (clr,__zero_reg__));
              else
                return *l=8, (AS2 (mov,__zero_reg__,%C1) CR_TAB
                              AS2 (mov,__tmp_reg__,%D1) CR_TAB
                              AS2 (st,%0+,%A1) CR_TAB
                              AS2 (st,%0+,%B1) CR_TAB
                              AS2 (st,%0+,__zero_reg__)  CR_TAB
                              AS2 (st,%0,__tmp_reg__)   CR_TAB
                              AS1 (clr,__zero_reg__)     CR_TAB
                              AS2 (sbiw,r26,3));
            }
          return *l=5, (AS2 (st,%0+,%A1)  CR_TAB
                        AS2 (st,%0+,%B1) CR_TAB
                        AS2 (st,%0+,%C1) CR_TAB
                        AS2 (st,%0,%D1)  CR_TAB
                        AS2 (sbiw,r26,3));
        }
      else
        return *l=4, (AS2 (st,%0,%A1)    CR_TAB
		      AS2 (std,%0+1,%B1) CR_TAB
		      AS2 (std,%0+2,%C1) CR_TAB
		      AS2 (std,%0+3,%D1));
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 6, (AS2 (adiw,r28,%o0-60) CR_TAB
			    AS2 (std,Y+60,%A1)    CR_TAB
			    AS2 (std,Y+61,%B1)    CR_TAB
			    AS2 (std,Y+62,%C1)    CR_TAB
			    AS2 (std,Y+63,%D1)    CR_TAB
			    AS2 (sbiw,r28,%o0-60));

	  return *l = 8, (AS2 (subi,r28,lo8(-%o0)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o0)) CR_TAB
			  AS2 (st,Y,%A1)           CR_TAB
			  AS2 (std,Y+1,%B1)        CR_TAB
			  AS2 (std,Y+2,%C1)        CR_TAB
			  AS2 (std,Y+3,%D1)        CR_TAB
			  AS2 (subi,r28,lo8(%o0))  CR_TAB
			  AS2 (sbci,r29,hi8(%o0)));
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  if (reg_src == REG_X)
	    {
	      *l = 9;
	      return (AS2 (mov,__tmp_reg__,r26)  CR_TAB
		      AS2 (mov,__zero_reg__,r27) CR_TAB
		      AS2 (adiw,r26,%o0)         CR_TAB
		      AS2 (st,X+,__tmp_reg__)    CR_TAB
		      AS2 (st,X+,__zero_reg__)   CR_TAB
		      AS2 (st,X+,r28)            CR_TAB
		      AS2 (st,X,r29)             CR_TAB
		      AS1 (clr,__zero_reg__)     CR_TAB
		      AS2 (sbiw,r26,%o0+3));
	    }
	  else if (reg_src == REG_X - 2)
	    {
	      *l = 9;
	      return (AS2 (mov,__tmp_reg__,r26)  CR_TAB
		      AS2 (mov,__zero_reg__,r27) CR_TAB
		      AS2 (adiw,r26,%o0)         CR_TAB
		      AS2 (st,X+,r24)            CR_TAB
		      AS2 (st,X+,r25)            CR_TAB
		      AS2 (st,X+,__tmp_reg__)    CR_TAB
		      AS2 (st,X,__zero_reg__)    CR_TAB
		      AS1 (clr,__zero_reg__)     CR_TAB
		      AS2 (sbiw,r26,%o0+3));
	    }
	  *l = 6;
	  return (AS2 (adiw,r26,%o0) CR_TAB
		  AS2 (st,X+,%A1)    CR_TAB
		  AS2 (st,X+,%B1)    CR_TAB
		  AS2 (st,X+,%C1)    CR_TAB
		  AS2 (st,X,%D1)     CR_TAB
		  AS2 (sbiw,r26,%o0+3));
	}
      return *l=4, (AS2 (std,%A0,%A1)    CR_TAB
		    AS2 (std,%B0,%B1) CR_TAB
		    AS2 (std,%C0,%C1) CR_TAB
		    AS2 (std,%D0,%D1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, (AS2 (st,%0,%D1) CR_TAB
		  AS2 (st,%0,%C1) CR_TAB
		  AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%A1));
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, (AS2 (st,%0,%A1)  CR_TAB
		  AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%C1) CR_TAB
		  AS2 (st,%0,%D1));
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
output_movsisf(insn, operands, l)
     rtx insn;
     rtx operands[];
     int *l;
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;
  
  if (!l)
    l = &dummy;
  
  if (register_operand (dest, VOIDmode))
    {
      if (register_operand (src, VOIDmode)) /* mov r,r */
	{
	  if (true_regnum (dest) > true_regnum (src))
	    {
	      if (AVR_ENHANCED)
		{
		  *l = 2;
		  return (AS2 (movw,%C0,%C1) CR_TAB
			  AS2 (movw,%A0,%A1));
		}
	      *l = 4;
	      return (AS2 (mov,%D0,%D1) CR_TAB
		      AS2 (mov,%C0,%C1) CR_TAB
		      AS2 (mov,%B0,%B1) CR_TAB
		      AS2 (mov,%A0,%A1));
	    }
	  else
	    {
	      if (AVR_ENHANCED)
		{
		  *l = 2;
		  return (AS2 (movw,%A0,%A1) CR_TAB
			  AS2 (movw,%C0,%C1));
		}
	      *l = 4;
	      return (AS2 (mov,%A0,%A1) CR_TAB
		      AS2 (mov,%B0,%B1) CR_TAB
		      AS2 (mov,%C0,%C1) CR_TAB
		      AS2 (mov,%D0,%D1));
	    }
	}
      else if (CONSTANT_P (src))
	{
	  if (test_hard_reg_class (LD_REGS, dest)) /* ldi d,i */
	    {
	      if (byte_immediate_operand (src, SImode)
		  && reg_was_0 (insn, dest))
		{
		  *l = 1;
		  return (AS2 (ldi,%A0,lo8(%1) ; reg_was_0));
		}

	      *l = 4;
	      return (AS2 (ldi,%A0,lo8(%1))  CR_TAB
		      AS2 (ldi,%B0,hi8(%1))  CR_TAB
		      AS2 (ldi,%C0,hlo8(%1)) CR_TAB
		      AS2 (ldi,%D0,hhi8(%1)));
	    }
	  
	  if (GET_CODE (src) == CONST_INT)
	    {
	      const char *const clr_op0 =
		AVR_ENHANCED ? (AS1 (clr,%A0) CR_TAB
				AS1 (clr,%B0) CR_TAB
				AS2 (movw,%C0,%A0))
			     : (AS1 (clr,%A0) CR_TAB
				AS1 (clr,%B0) CR_TAB
				AS1 (clr,%C0) CR_TAB
				AS1 (clr,%D0));

	      if (src == const0_rtx) /* mov r,L */
		{
		  *l = AVR_ENHANCED ? 3 : 4;
		  return clr_op0;
		}
	      else if (src == const1_rtx)
		{
		  if (reg_was_0 (insn, dest))
		    {
		      *l = 1;
		      return AS1 (inc,%A0 ; reg_was_0);
		    }
		  if (!real_l)
		    output_asm_insn (clr_op0, operands);
		  *l = AVR_ENHANCED ? 4 : 5;
		  return AS1 (inc,%A0);
		}
	      else if (src == constm1_rtx)
		{
		  /* Immediate constants -1 to any register */
		  if (reg_was_0 (insn, dest))
		    {
		      if (AVR_ENHANCED)
			{
			  *l = 3;
			  return (AS1 (dec,%A0) CR_TAB
				  AS1 (dec,%B0) CR_TAB
				  AS2 (movw,%C0,%A0));
			}
		      *l = 4;
		      return (AS1 (dec,%D0 ; reg_was_0) CR_TAB
			      AS1 (dec,%C0)             CR_TAB
			      AS1 (dec,%B0)             CR_TAB
			      AS1 (dec,%A0));
		    }
		  if (AVR_ENHANCED)
		    {
		      *l = 4;
		      return (AS1 (clr,%A0)     CR_TAB
			      AS1 (dec,%A0)     CR_TAB
			      AS2 (mov,%B0,%A0) CR_TAB
			      AS2 (movw,%C0,%A0));
		    }
		  *l = 5;
		  return (AS1 (clr,%A0)     CR_TAB
			  AS1 (dec,%A0)     CR_TAB
			  AS2 (mov,%B0,%A0) CR_TAB
			  AS2 (mov,%C0,%A0) CR_TAB
			  AS2 (mov,%D0,%A0));
		}
	      else
		{
		  int bit_nr = exact_log2 (INTVAL (src));

		  if (bit_nr >= 0)
		    {
		      if (reg_was_0 (insn, dest))
			{
			  *l = 2;
			  if (!real_l)
			    output_asm_insn ("set ; reg_was_0", operands);
			}
		      else
			{
			  *l = AVR_ENHANCED ? 5 : 6;
			  if (!real_l)
			    {
			      output_asm_insn (clr_op0, operands);
			      output_asm_insn ("set", operands);
			    }
			}
		      if (!real_l)
			avr_output_bld (operands, bit_nr);

		      return "";
		    }
		}
	    }
	  
	  /* Last resort, better than loading from memory.  */
	  *l = 10;
	  return (AS2 (mov,__tmp_reg__,r31) CR_TAB
		  AS2 (ldi,r31,lo8(%1))     CR_TAB
		  AS2 (mov,%A0,r31)         CR_TAB
		  AS2 (ldi,r31,hi8(%1))     CR_TAB
		  AS2 (mov,%B0,r31)         CR_TAB
		  AS2 (ldi,r31,hlo8(%1))    CR_TAB
		  AS2 (mov,%C0,r31)         CR_TAB
		  AS2 (ldi,r31,hhi8(%1))    CR_TAB
		  AS2 (mov,%D0,r31)         CR_TAB
		  AS2 (mov,r31,__tmp_reg__));
	}
      else if (GET_CODE (src) == MEM)
	return out_movsi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (GET_CODE (dest) == MEM)
    {
      const char *template;

      if (src == const0_rtx)
	  operands[1] = zero_reg_rtx;

      template = out_movsi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (template, operands);

      operands[1] = src;
      return "";
    }
  fatal_insn ("invalid insn:", insn);
  return "";
}

const char *
out_movqi_mr_r (insn, op, l)
     rtx insn;
     rtx op[];
     int *l; /* instruction length */
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (dest, 0);
  int dummy;

  if (!l)
    l = &dummy;
  
  if (CONSTANT_ADDRESS_P (x))
    {
      if (io_address_p (x, 1))
	{
	  *l = 1;
	  return AS2 (out,%0-0x20,%1);
	}
      *l = 2;
      return AS2 (sts,%0,%1);
    }
  /* memory access by reg+disp */
  else if (GET_CODE (x) == PLUS	
      && REG_P (XEXP (x,0))
      && GET_CODE (XEXP (x,1)) == CONST_INT)
    {
      if ((INTVAL (XEXP (x,1)) - GET_MODE_SIZE (GET_MODE (dest))) >= 63)
	{
	  int disp = INTVAL (XEXP (x,1));
	  if (REGNO (XEXP (x,0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 3, (AS2 (adiw,r28,%o0-63) CR_TAB
			    AS2 (std,Y+63,%1)     CR_TAB
			    AS2 (sbiw,r28,%o0-63));

	  return *l = 5, (AS2 (subi,r28,lo8(-%o0)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o0)) CR_TAB
			  AS2 (st,Y,%1)            CR_TAB
			  AS2 (subi,r28,lo8(%o0))  CR_TAB
			  AS2 (sbci,r29,hi8(%o0)));
	}
      else if (REGNO (XEXP (x,0)) == REG_X)
	{
	  if (reg_overlap_mentioned_p (src, XEXP (x, 0)))
	    {
	      if (reg_unused_after (insn, XEXP (x,0)))
		return *l = 3, (AS2 (mov,__tmp_reg__,%1) CR_TAB
				AS2 (adiw,r26,%o0)       CR_TAB
				AS2 (st,X,__tmp_reg__));

	      return *l = 4, (AS2 (mov,__tmp_reg__,%1) CR_TAB
			      AS2 (adiw,r26,%o0)       CR_TAB
			      AS2 (st,X,__tmp_reg__)   CR_TAB
			      AS2 (sbiw,r26,%o0));
	    }
	  else
	    {
	      if (reg_unused_after (insn, XEXP (x,0)))
		return *l = 2, (AS2 (adiw,r26,%o0) CR_TAB
				AS2 (st,X,%1));

	      return *l = 3, (AS2 (adiw,r26,%o0) CR_TAB
			      AS2 (st,X,%1)      CR_TAB
			      AS2 (sbiw,r26,%o0));
	    }
	}
      *l = 1;
      return AS2 (std,%0,%1);
    }
  *l = 1;
  return AS2 (st,%0,%1);
}

const char *
out_movhi_mr_r (insn, op, l)
     rtx insn;
     rtx op[];
     int *l;
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  int tmp;
  if (!l)
    l = &tmp;
  if (CONSTANT_ADDRESS_P (base))
    {
      if (io_address_p (base, 2))
	{
	  *l = 2;
	  return (AS2 (out,%B0-0x20,%B1) CR_TAB
		  AS2 (out,%A0-0x20,%A1));
	}
      return *l = 4, (AS2 (sts,%B0,%B1) CR_TAB
		      AS2 (sts,%A0,%A1));
    }
  if (reg_base > 0)
    {
      if (reg_base == REG_X)
        {
          if (reg_src == REG_X)
            {
	      /* "st X+,r26" is undefined */
              if (reg_unused_after (insn, src))
		return *l=4, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X,__tmp_reg__));
              else
		return *l=5, (AS2 (mov,__tmp_reg__,r27) CR_TAB
			      AS2 (st,X,r26)            CR_TAB
			      AS2 (adiw,r26,1)          CR_TAB
			      AS2 (st,X,__tmp_reg__)    CR_TAB
			      AS2 (sbiw,r26,1));
            }
          else
            {
              if (reg_unused_after (insn, base))
                return *l=2, (AS2 (st,X+,%A1) CR_TAB
                              AS2 (st,X,%B1));
              else
                return *l=3, (AS2 (st  ,X+,%A1) CR_TAB
                              AS2 (st  ,X,%B1) CR_TAB
                              AS2 (sbiw,r26,1));
            }
        }
      else
        return  *l=2, (AS2 (st ,%0,%A1)    CR_TAB
                       AS2 (std,%0+1,%B1));
    }
  else if (GET_CODE (base) == PLUS)
    {
      int disp = INTVAL (XEXP (base, 1));
      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 4, (AS2 (adiw,r28,%o0-62) CR_TAB
			    AS2 (std,Y+62,%A1)    CR_TAB
			    AS2 (std,Y+63,%B1)    CR_TAB
			    AS2 (sbiw,r28,%o0-62));

	  return *l = 6, (AS2 (subi,r28,lo8(-%o0)) CR_TAB
			  AS2 (sbci,r29,hi8(-%o0)) CR_TAB
			  AS2 (st,Y,%A1)           CR_TAB
			  AS2 (std,Y+1,%B1)        CR_TAB
			  AS2 (subi,r28,lo8(%o0))  CR_TAB
			  AS2 (sbci,r29,hi8(%o0)));
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  if (reg_src == REG_X)
	    {
	      *l = 7;
	      return (AS2 (mov,__tmp_reg__,r26)  CR_TAB
		      AS2 (mov,__zero_reg__,r27) CR_TAB
		      AS2 (adiw,r26,%o0)         CR_TAB
		      AS2 (st,X+,__tmp_reg__)    CR_TAB
		      AS2 (st,X,__zero_reg__)    CR_TAB
		      AS1 (clr,__zero_reg__)     CR_TAB
		      AS2 (sbiw,r26,%o0+1));
	    }
	  *l = 4;
	  return (AS2 (adiw,r26,%o0) CR_TAB
		  AS2 (st,X+,%A1)    CR_TAB
		  AS2 (st,X,%B1)     CR_TAB
		  AS2 (sbiw,r26,%o0+1));
	}
      return *l=2, (AS2 (std,%A0,%A1)    CR_TAB
		    AS2 (std,%B0,%B1));
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=2, (AS2 (st,%0,%B1) CR_TAB
		  AS2 (st,%0,%A1));
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=2, (AS2 (st,%0,%A1)  CR_TAB
		  AS2 (st,%0,%B1));
  fatal_insn ("unknown move insn:",insn);
  return "";
}

/* Return 1 if frame pointer for current function required */

int
frame_pointer_required_p ()
{
  return (current_function_calls_alloca
	  || current_function_args_info.nregs == 0
	  || current_function_varargs
  	  || get_frame_size () > 0);
}

/* Returns the condition of compare insn INSN, or UNKNOWN.  */

static RTX_CODE
compare_condition (insn)
     rtx insn;
{
  rtx next = next_real_insn (insn);
  RTX_CODE cond = UNKNOWN;
  if (next && GET_CODE (next) == JUMP_INSN)
    {
      rtx pat = PATTERN (next);
      rtx src = SET_SRC (pat);
      rtx t = XEXP (src, 0);
      cond = GET_CODE (t);
    }
  return cond;
}

/* Returns nonzero if INSN is a tst insn that only tests the sign.  */

static int
compare_sign_p (insn)
     rtx insn;
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == GE || cond == LT);
}

/* Returns nonzero if the next insn is a JUMP_INSN with a condition
   that needs to be swapped (GT, GTU, LE, LEU).  */

int
compare_diff_p (insn)
     rtx insn;
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == GT || cond == GTU || cond == LE || cond == LEU) ? cond : 0;
}

/* Returns nonzero if INSN is a compare insn with the EQ or NE condition.  */

int
compare_eq_p (insn)
     rtx insn;
{
  RTX_CODE cond = compare_condition (insn);
  return (cond == EQ || cond == NE);
}


/* Output test instruction for HImode */

const char *
out_tsthi (insn, l)
     rtx insn;
     int *l;
{
  if (compare_sign_p (insn))
    {
      if (l) *l = 1;
      return AS1 (tst,%B0);
    }
  if (reg_unused_after (insn, SET_SRC (PATTERN (insn)))
      && compare_eq_p (insn))
    {
      /* faster than sbiw if we can clobber the operand */
      if (l) *l = 1;
      return AS2 (or,%A0,%B0);
    }
  if (test_hard_reg_class (ADDW_REGS, SET_SRC (PATTERN (insn))))
    {
      if (l) *l = 1;
      return AS2 (sbiw,%0,0);
    }
  if (l) *l = 2;
  return (AS2 (cp,%A0,__zero_reg__) CR_TAB
          AS2 (cpc,%B0,__zero_reg__));
}


/* Output test instruction for SImode */

const char *
out_tstsi (insn, l)
     rtx insn;
     int *l;
{
  if (compare_sign_p (insn))
    {
      if (l) *l = 1;
      return AS1 (tst,%D0);
    }
  if (test_hard_reg_class (ADDW_REGS, SET_SRC (PATTERN (insn))))
    {
      if (l) *l = 3;
      return (AS2 (sbiw,%A0,0) CR_TAB
              AS2 (cpc,%C0,__zero_reg__) CR_TAB
              AS2 (cpc,%D0,__zero_reg__));
    }
  if (l) *l = 4;
  return (AS2 (cp,%A0,__zero_reg__) CR_TAB
          AS2 (cpc,%B0,__zero_reg__) CR_TAB
          AS2 (cpc,%C0,__zero_reg__) CR_TAB
          AS2 (cpc,%D0,__zero_reg__));
}


/* Generate asm equivalent for various shifts.
   Shift count is a CONST_INT, MEM or REG.
   This only handles cases that are not already
   carefully hand-optimized in ?sh??i3_out.  */

void
out_shift_with_cnt (template, insn, operands, len, t_len)
     const char *template;
     rtx insn;
     rtx operands[];
     int *len;
     int t_len;  /* Length of template.  */
{
  rtx op[10];
  char str[500];
  int second_label = 1;
  int saved_in_tmp = 0;
  int use_zero_reg = 0;

  op[0] = operands[0];
  op[1] = operands[1];
  op[2] = operands[2];
  op[3] = operands[3];
  str[0] = 0;

  if (len)
    *len = 1;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int count = INTVAL (operands[2]);
      int max_len = 10;  /* If larger than this, always use a loop.  */

      if (count < 8 && !scratch)
	use_zero_reg = 1;

      if (optimize_size)
	max_len = t_len + (scratch ? 3 : (use_zero_reg ? 4 : 5));

      if (t_len * count <= max_len)
	{
	  /* Output shifts inline with no loop - faster.  */
	  if (len)
	    *len = t_len * count;
	  else
	    {
	      while (count-- > 0)
		output_asm_insn (template, op);
	    }

	  return;
	}

      if (scratch)
	{
	  if (!len)
	    strcat (str, AS2 (ldi,%3,%2));
	}
      else if (use_zero_reg)
	{
	  /* Hack to save one word: use __zero_reg__ as loop counter.
	     Set one bit, then shift in a loop until it is 0 again.  */

	  op[3] = zero_reg_rtx;
	  if (len)
	    *len = 2;
	  else
	    strcat (str, ("set" CR_TAB
			  AS2 (bld,%3,%2-1)));
	}
      else
	{
	  /* No scratch register available, use one from LD_REGS (saved in
	     __tmp_reg__) that doesn't overlap with registers to shift.  */

	  op[3] = gen_rtx (REG, QImode,
			   ((true_regnum (operands[0]) - 1) & 15) + 16);
	  op[4] = tmp_reg_rtx;
	  saved_in_tmp = 1;

	  if (len)
	    *len = 3;  /* Includes "mov %3,%4" after the loop.  */
	  else
	    strcat (str, (AS2 (mov,%4,%3) CR_TAB
			  AS2 (ldi,%3,%2)));
	}

      second_label = 0;
    }
  else if (GET_CODE (operands[2]) == MEM)
    {
      rtx op_mov[10];
      
      op[3] = op_mov[0] = tmp_reg_rtx;
      op_mov[1] = op[2];

      if (len)
	out_movqi_r_mr (insn, op_mov, len);
      else
	output_asm_insn (out_movqi_r_mr (insn, op_mov, NULL), op_mov);
    }
  else if (register_operand (operands[2], QImode))
    {
      if (reg_unused_after (insn, operands[2]))
	op[3] = op[2];
      else
	{
	  op[3] = tmp_reg_rtx;
	  if (!len)
	    strcat (str, (AS2 (mov,%3,%2) CR_TAB));
	}
    }
  else
    fatal_insn ("bad shift insn:", insn);

  if (second_label)
    {
      if (len)
	++*len;
      else
	strcat (str, AS1 (rjmp,2f));
    }

  if (len)
    *len += t_len + 2;  /* template + dec + brXX */
  else
    {
      strcat (str, "\n1:\t");
      strcat (str, template);
      strcat (str, second_label ? "\n2:\t" : "\n\t");
      strcat (str, use_zero_reg ? AS1 (lsr,%3) : AS1 (dec,%3));
      strcat (str, CR_TAB);
      strcat (str, second_label ? AS1 (brpl,1b) : AS1 (brne,1b));
      if (saved_in_tmp)
	strcat (str, (CR_TAB AS2 (mov,%3,%4)));
      output_asm_insn (str, op);
    }
}


/* 8bit shift left ((char)x << i)   */

const char *
ashlqi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;			/* insn length (may be NULL) */
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  *len = 1;
	  return AS1 (clr,%0);
	  
	case 1:
	  *len = 1;
	  return AS1 (lsl,%0);
	  
	case 2:
	  *len = 2;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 3:
	  *len = 3;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 2;
	      return (AS1 (swap,%0) CR_TAB
		      AS2 (andi,%0,0xf0));
	    }
	  *len = 4;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS2 (andi,%0,0xe0));
	    }
	  *len = 5;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS1 (lsl,%0)  CR_TAB
		      AS2 (andi,%0,0xc0));
	    }
	  *len = 6;
	  return (AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0) CR_TAB
		  AS1 (lsl,%0));

	case 7:
	  *len = 3;
	  return (AS1 (ror,%0) CR_TAB
		  AS1 (clr,%0) CR_TAB
		  AS1 (ror,%0));
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt (AS1 (lsl,%0),
		      insn, operands, len, 1);
  return "";
}


/* 16bit shift left ((short)x << i)   */

const char *
ashlhi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;
      
      switch (INTVAL (operands[2]))
	{
	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return (AS1 (swap,%A0)      CR_TAB
		      AS1 (swap,%B0)      CR_TAB
		      AS2 (andi,%B0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0)   CR_TAB
		      AS2 (andi,%A0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return (AS1 (swap,%A0)    CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (ldi,%3,0xf0) CR_TAB
		      AS2 (and,%B0,%3)  CR_TAB
		      AS2 (eor,%B0,%A0) CR_TAB
		      AS2 (and,%A0,%3)  CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return (AS1 (lsl,%A0)       CR_TAB
		      AS1 (rol,%B0)       CR_TAB
		      AS1 (swap,%A0)      CR_TAB
		      AS1 (swap,%B0)      CR_TAB
		      AS2 (andi,%B0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0)   CR_TAB
		      AS2 (andi,%A0,0xf0) CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return (AS1 (lsl,%A0)     CR_TAB
		      AS1 (rol,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (ldi,%3,0xf0) CR_TAB
		      AS2 (and,%B0,%3)  CR_TAB
		      AS2 (eor,%B0,%A0) CR_TAB
		      AS2 (and,%A0,%3)  CR_TAB
		      AS2 (eor,%B0,%A0));
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return (AS1 (clr,__tmp_reg__) CR_TAB
		  AS1 (lsr,%B0)         CR_TAB
		  AS1 (ror,%A0)         CR_TAB
		  AS1 (ror,__tmp_reg__) CR_TAB
		  AS1 (lsr,%B0)         CR_TAB
		  AS1 (ror,%A0)         CR_TAB
		  AS1 (ror,__tmp_reg__) CR_TAB
		  AS2 (mov,%B0,%A0)     CR_TAB
		  AS2 (mov,%A0,__tmp_reg__));

	case 7:
	  *len = 5;
	  return (AS1 (lsr,%B0)     CR_TAB
		  AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (ror,%B0)     CR_TAB
		  AS1 (ror,%A0));

	case 8:
	  if (true_regnum (operands[0]) + 1 == true_regnum (operands[1]))
	    return *len = 1, AS1 (clr,%A0);
	  else
	    return *len = 2, (AS2 (mov,%B0,%A1) CR_TAB
			      AS1 (clr,%A0));

	case 9:
	  *len = 3;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0));

	case 10:
	  *len = 4;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 11:
	  *len = 5;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (andi,%B0,0xf0));
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS2 (ldi,%3,0xf0) CR_TAB
		      AS2 (and,%B0,%3));
	    }
	  *len = 6;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS1 (lsl,%B0)     CR_TAB
		      AS2 (andi,%B0,0xe0));
	    }
	  if (AVR_ENHANCED && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x20) CR_TAB
		      AS2 (mul,%A0,%3)  CR_TAB
		      AS2 (mov,%B0,r0)  CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS1 (lsl,%B0)     CR_TAB
		      AS2 (ldi,%3,0xe0) CR_TAB
		      AS2 (and,%B0,%3));
	    }
	  if (AVR_ENHANCED)
	    {
	      *len = 6;
	      return ("set"            CR_TAB
		      AS2 (bld,r1,5)   CR_TAB
		      AS2 (mul,%A0,r1) CR_TAB
		      AS2 (mov,%B0,r0) CR_TAB
		      AS1 (clr,%A0)    CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  *len = 7;
	  return (AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (clr,%A0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS1 (lsl,%B0));

	case 14:
	  if (AVR_ENHANCED && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%B0,0x40) CR_TAB
		      AS2 (mul,%A0,%B0)  CR_TAB
		      AS2 (mov,%B0,r0)   CR_TAB
		      AS1 (clr,%A0)      CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (AVR_ENHANCED && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x40) CR_TAB
		      AS2 (mul,%A0,%3)  CR_TAB
		      AS2 (mov,%B0,r0)  CR_TAB
		      AS1 (clr,%A0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%B0,%A0) CR_TAB
		      AS2 (ldi,%A0,6) "\n1:\t"
		      AS1 (lsl,%B0)     CR_TAB
		      AS1 (dec,%A0)     CR_TAB
		      AS1 (brne,1b));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return (AS1 (clr,%B0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (clr,%A0));

	case 15:
	  *len = 4;
	  return (AS1 (clr,%B0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%B0) CR_TAB
		  AS1 (clr,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsl,%A0) CR_TAB
		       AS1 (rol,%B0)),
		       insn, operands, len, 2);
  return "";
}


/* 32bit shift left ((long)x << i)   */

const char *
ashlsi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t = len;
      
      if (!len)
	len = &k;
      
      switch (INTVAL (operands[2]))
	{
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 >= reg1)
	      return (AS2 (mov,%D0,%C1)  CR_TAB
		      AS2 (mov,%C0,%B1)  CR_TAB
		      AS2 (mov,%B0,%A1)  CR_TAB
		      AS1 (clr,%A0));
	    else if (reg0 + 1 == reg1)
	      {
		*len = 1;
		return AS1 (clr,%A0);
	      }
	    else
	      return (AS1 (clr,%A0)      CR_TAB
		      AS2 (mov,%B0,%A1)  CR_TAB
		      AS2 (mov,%C0,%B1)  CR_TAB
		      AS2 (mov,%D0,%C1));
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (AVR_ENHANCED && (reg0 + 2 != reg1))
	      {
		*len = 3;
		return (AS2 (movw,%C0,%A1) CR_TAB
			AS1 (clr,%B0)      CR_TAB
			AS1 (clr,%A0));
	      }
	    if (reg0 + 1 >= reg1)
	      return (AS2 (mov,%D0,%B1)  CR_TAB
		      AS2 (mov,%C0,%A1)  CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,%A0));
	    if (reg0 + 2 == reg1)
	      {
		*len = 2;
		return (AS1 (clr,%B0)      CR_TAB
			AS1 (clr,%A0));
	      }
	    else
	      return (AS2 (mov,%C0,%A1)  CR_TAB
		      AS2 (mov,%D0,%B1)  CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,%A0));
	  }

	case 24:
	  *len = 4;
	  if (true_regnum (operands[0]) + 3 != true_regnum (operands[1]))
	    return (AS2 (mov,%D0,%A1)  CR_TAB
		    AS1 (clr,%C0)      CR_TAB
		    AS1 (clr,%B0)      CR_TAB
		    AS1 (clr,%A0));
	  else
	    {
	      *len = 3;
	      return (AS1 (clr,%C0)      CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,%A0));
	    }

	case 31:
	  *len = 6;
	  return (AS1 (clr,%D0) CR_TAB
		  AS1 (lsr,%A0) CR_TAB
		  AS1 (ror,%D0) CR_TAB
		  AS1 (clr,%C0) CR_TAB
		  AS1 (clr,%B0) CR_TAB
		  AS1 (clr,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsl,%A0) CR_TAB
		       AS1 (rol,%B0) CR_TAB
		       AS1 (rol,%C0) CR_TAB
		       AS1 (rol,%D0)),
		       insn, operands, len, 4);
  return "";
}

/* 8bit arithmetic shift right  ((signed char)x >> i) */

const char *
ashrqi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len; /* insn length */
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 1:
	  *len = 1;
	  return AS1 (asr,%0);

	case 2:
	  *len = 2;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 3:
	  *len = 3;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 4:
	  *len = 4;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 5:
	  *len = 5;
	  return (AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0) CR_TAB
		  AS1 (asr,%0));

	case 6:
	  *len = 4;
	  return (AS2 (bst,%0,6)  CR_TAB
		  AS1 (lsl,%0)    CR_TAB
		  AS2 (sbc,%0,%0) CR_TAB
		  AS2 (bld,%0,0));

	default:
	case 7:
	  *len = 2;
	  return (AS1 (lsl,%0) CR_TAB
		  AS2 (sbc,%0,%0));
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt (AS1 (asr,%0),
		      insn, operands, len, 1);
  return "";
}


/* 16bit arithmetic shift right  ((signed short)x >> i) */

const char *
ashrhi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;
      
      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 4:
	case 5:
	  /* XXX try to optimize this too? */
	  break;

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 8;
	  return (AS2 (mov,__tmp_reg__,%A0) CR_TAB
		  AS2 (mov,%A0,%B0)         CR_TAB
		  AS1 (lsl,__tmp_reg__)     CR_TAB
		  AS1 (rol,%A0)             CR_TAB
		  AS2 (sbc,%B0,%B0)         CR_TAB
		  AS1 (lsl,__tmp_reg__)     CR_TAB
		  AS1 (rol,%A0)             CR_TAB
		  AS1 (rol,%B0));

	case 7:
	  *len = 4;
	  return (AS1 (lsl,%A0)     CR_TAB
		  AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (rol,%A0)     CR_TAB
		  AS2 (sbc,%B0,%B0));

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1)
	      return *len = 3, (AS2 (mov,%A0,%B0) CR_TAB
				AS1 (lsl,%B0)     CR_TAB
				AS2 (sbc,%B0,%B0));
	    else if (reg0 == reg1 + 1)
	      return *len = 3, (AS1 (clr,%B0)    CR_TAB
				AS2 (sbrc,%A0,7) CR_TAB
				AS1 (dec,%B0));

	    return *len = 4, (AS2 (mov,%A0,%B1) CR_TAB
			      AS1 (clr,%B0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (dec,%B0));
	  }

	case 9:
	  *len = 4;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)      CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0));

	case 10:
	  *len = 5;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 11:
	  if (AVR_ENHANCED && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x20) CR_TAB
		      AS2 (muls,%B0,%A0) CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS2 (sbc,%B0,%B0)  CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 12:
	  if (AVR_ENHANCED && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x10) CR_TAB
		      AS2 (muls,%B0,%A0) CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS2 (sbc,%B0,%B0)  CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 7;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 13:
	  if (AVR_ENHANCED && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x08) CR_TAB
		      AS2 (muls,%B0,%A0) CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS2 (sbc,%B0,%B0)  CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size)
	    break;  /* scratch ? 5 : 7 */
	  *len = 8;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0)     CR_TAB
		  AS1 (asr,%A0));

	case 14:
	  *len = 5;
	  return (AS1 (lsl,%B0)     CR_TAB
		  AS2 (sbc,%A0,%A0) CR_TAB
		  AS1 (lsl,%B0)     CR_TAB
		  AS2 (mov,%B0,%A0) CR_TAB
		  AS1 (rol,%A0));

	case 15:
	  return *len = 3, (AS1 (lsl,%B0)     CR_TAB
			    AS2 (sbc,%A0,%A0) CR_TAB
			    AS2 (mov,%B0,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (asr,%B0) CR_TAB
		       AS1 (ror,%A0)),
		       insn, operands, len, 2);
  return "";
}


/* 32bit arithmetic shift right  ((signed long)x >> i) */

const char *
ashrsi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t = len;
      
      if (!len)
	len = &k;
      
      switch (INTVAL (operands[2]))
	{
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=6;
	    if (reg0 <= reg1)
	      return (AS2 (mov,%A0,%B1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%C0,7)  CR_TAB
		      AS1 (dec,%D0));
	    else if (reg0 == reg1 + 1)
	      {
		*len = 3;
		return (AS1 (clr,%D0)     CR_TAB
			AS2 (sbrc,%C0,7)  CR_TAB
			AS1 (dec,%D0));
	      }
	    else
	      return (AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%D1,7)  CR_TAB
		      AS1 (dec,%D0)     CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%A0,%B1));
	  }
	  
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=6;
	    if (AVR_ENHANCED && (reg0 != reg1 + 2))
	      {
		*len = 5;
		return (AS2 (movw,%A0,%C1) CR_TAB
			AS1 (clr,%D0)      CR_TAB
			AS2 (sbrc,%B0,7)   CR_TAB
			AS1 (com,%D0)      CR_TAB
			AS2 (mov,%C0,%D0));
	      }
	    if (reg0 <= reg1 + 1)
	      return (AS2 (mov,%A0,%C1) CR_TAB
		      AS2 (mov,%B0,%D1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%B0,7)  CR_TAB
		      AS1 (com,%D0)     CR_TAB
		      AS2 (mov,%C0,%D0));
	    else if (reg0 == reg1 + 2)
	      return *len = 4, (AS1 (clr,%D0)     CR_TAB
				AS2 (sbrc,%B0,7)  CR_TAB
				AS1 (com,%D0)     CR_TAB
				AS2 (mov,%C0,%D0));
	    else
	      return (AS2 (mov,%B0,%D1) CR_TAB
		      AS2 (mov,%A0,%C1) CR_TAB
		      AS1 (clr,%D0)     CR_TAB
		      AS2 (sbrc,%B0,7)  CR_TAB
		      AS1 (com,%D0)     CR_TAB
		      AS2 (mov,%C0,%D0));
	  }

	case 24:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 3)
	    return *len = 6, (AS2 (mov,%A0,%D1) CR_TAB
			      AS1 (clr,%D0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (com,%D0)     CR_TAB
			      AS2 (mov,%B0,%D0) CR_TAB
			      AS2 (mov,%C0,%D0));
	  else
	    return *len = 5, (AS1 (clr,%D0)     CR_TAB
			      AS2 (sbrc,%A0,7)  CR_TAB
			      AS1 (com,%D0)     CR_TAB
			      AS2 (mov,%B0,%D0) CR_TAB
			      AS2 (mov,%C0,%D0));

	case 31:
	  if (AVR_ENHANCED)
	    return *len = 4, (AS1 (lsl,%D0)     CR_TAB
			      AS2 (sbc,%A0,%A0) CR_TAB
			      AS2 (mov,%B0,%A0) CR_TAB
			      AS2 (movw,%C0,%A0));
	  else
	    return *len = 5, (AS1 (lsl,%D0)     CR_TAB
			      AS2 (sbc,%A0,%A0) CR_TAB
			      AS2 (mov,%B0,%A0) CR_TAB
			      AS2 (mov,%C0,%A0) CR_TAB
			      AS2 (mov,%D0,%A0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (asr,%D0) CR_TAB
		       AS1 (ror,%C0) CR_TAB
		       AS1 (ror,%B0) CR_TAB
		       AS1 (ror,%A0)),
		       insn, operands, len, 4);
  return "";
}

/* 8bit logic shift right ((unsigned char)x >> i) */

const char *
lshrqi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;

      if (!len)
	len = &k;
      
      switch (INTVAL (operands[2]))
	{
	default:
	  *len = 1;
	  return AS1 (clr,%0);

	case 1:
	  *len = 1;
	  return AS1 (lsr,%0);

	case 2:
	  *len = 2;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	case 3:
	  *len = 3;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len=2;
	      return (AS1 (swap,%0) CR_TAB
		      AS2 (andi,%0,0x0f));
	    }
	  *len = 4;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS2 (andi,%0,0x7));
	    }
	  *len = 5;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return (AS1 (swap,%0) CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS1 (lsr,%0)  CR_TAB
		      AS2 (andi,%0,0x3));
	    }
	  *len = 6;
	  return (AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0) CR_TAB
		  AS1 (lsr,%0));
	  
	case 7:
	  *len = 3;
	  return (AS1 (rol,%0) CR_TAB
		  AS1 (clr,%0) CR_TAB
		  AS1 (rol,%0));
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);
  
  out_shift_with_cnt (AS1 (lsr,%0),
		      insn, operands, len, 1);
  return "";
}

/* 16bit logic shift right ((unsigned short)x >> i) */

const char *
lshrhi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL);
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;
      
      switch (INTVAL (operands[2]))
	{
	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return (AS1 (swap,%B0)      CR_TAB
		      AS1 (swap,%A0)      CR_TAB
		      AS2 (andi,%A0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0)   CR_TAB
		      AS2 (andi,%B0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return (AS1 (swap,%B0)    CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (ldi,%3,0x0f) CR_TAB
		      AS2 (and,%A0,%3)  CR_TAB
		      AS2 (eor,%A0,%B0) CR_TAB
		      AS2 (and,%B0,%3)  CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return (AS1 (lsr,%B0)       CR_TAB
		      AS1 (ror,%A0)       CR_TAB
		      AS1 (swap,%B0)      CR_TAB
		      AS1 (swap,%A0)      CR_TAB
		      AS2 (andi,%A0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0)   CR_TAB
		      AS2 (andi,%B0,0x0f) CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return (AS1 (lsr,%B0)     CR_TAB
		      AS1 (ror,%A0)     CR_TAB
		      AS1 (swap,%B0)    CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (ldi,%3,0x0f) CR_TAB
		      AS2 (and,%A0,%3)  CR_TAB
		      AS2 (eor,%A0,%B0) CR_TAB
		      AS2 (and,%B0,%3)  CR_TAB
		      AS2 (eor,%A0,%B0));
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return (AS1 (clr,__tmp_reg__) CR_TAB
		  AS1 (lsl,%A0)         CR_TAB
		  AS1 (rol,%B0)         CR_TAB
		  AS1 (rol,__tmp_reg__) CR_TAB
		  AS1 (lsl,%A0)         CR_TAB
		  AS1 (rol,%B0)         CR_TAB
		  AS1 (rol,__tmp_reg__) CR_TAB
		  AS2 (mov,%A0,%B0)     CR_TAB
		  AS2 (mov,%B0,__tmp_reg__));

	case 7:
	  *len = 5;
	  return (AS1 (lsl,%A0)     CR_TAB
		  AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (rol,%A0)     CR_TAB
		  AS2 (sbc,%B0,%B0) CR_TAB
		  AS1 (neg,%B0));

	case 8:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 1)
	    return *len = 2, (AS2 (mov,%A0,%B1) CR_TAB
			      AS1 (clr,%B0));
	  else
	    return *len = 1, AS1 (clr,%B0);

	case 9:
	  *len = 3;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0));

	case 10:
	  *len = 4;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 11:
	  *len = 5;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (andi,%A0,0x0f));
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS2 (ldi,%3,0x0f) CR_TAB
		      AS2 (and,%A0,%3));
	    }
	  *len = 6;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS1 (lsr,%A0)     CR_TAB
		      AS2 (andi,%A0,0x07));
	    }
	  if (AVR_ENHANCED && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x08) CR_TAB
		      AS2 (mul,%B0,%3)  CR_TAB
		      AS2 (mov,%A0,r1)  CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (swap,%A0)    CR_TAB
		      AS1 (lsr,%A0)     CR_TAB
		      AS2 (ldi,%3,0x07) CR_TAB
		      AS2 (and,%A0,%3));
	    }
	  if (AVR_ENHANCED)
	    {
	      *len = 6;
	      return ("set"            CR_TAB
		      AS2 (bld,r1,3)   CR_TAB
		      AS2 (mul,%B0,r1) CR_TAB
		      AS2 (mov,%A0,r1) CR_TAB
		      AS1 (clr,%B0)    CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  *len = 7;
	  return (AS2 (mov,%A0,%B0) CR_TAB
		  AS1 (clr,%B0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0)     CR_TAB
		  AS1 (lsr,%A0));

	case 14:
	  if (AVR_ENHANCED && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (ldi,%A0,0x04) CR_TAB
		      AS2 (mul,%B0,%A0)  CR_TAB
		      AS2 (mov,%A0,r1)   CR_TAB
		      AS1 (clr,%B0)      CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (AVR_ENHANCED && scratch)
	    {
	      *len = 5;
	      return (AS2 (ldi,%3,0x04) CR_TAB
		      AS2 (mul,%B0,%3)  CR_TAB
		      AS2 (mov,%A0,r1)  CR_TAB
		      AS1 (clr,%B0)     CR_TAB
		      AS1 (clr,__zero_reg__));
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return (AS2 (mov,%A0,%B0) CR_TAB
		      AS2 (ldi,%B0,6) "\n1:\t"
		      AS1 (lsr,%A0)     CR_TAB
		      AS1 (dec,%B0)     CR_TAB
		      AS1 (brne,1b));
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return (AS1 (clr,%A0) CR_TAB
		  AS1 (lsl,%B0) CR_TAB
		  AS1 (rol,%A0) CR_TAB
		  AS1 (lsl,%B0) CR_TAB
		  AS1 (rol,%A0) CR_TAB
		  AS1 (clr,%B0));

	case 15:
	  *len = 4;
	  return (AS1 (clr,%A0) CR_TAB
		  AS1 (lsl,%B0) CR_TAB
		  AS1 (rol,%A0) CR_TAB
		  AS1 (clr,%B0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsr,%B0) CR_TAB
		       AS1 (ror,%A0)),
		       insn, operands, len, 2);
  return "";
}

/* 32bit logic shift right ((unsigned int)x >> i) */

const char *
lshrsi3_out (insn, operands, len)
     rtx insn;
     rtx operands[];
     int *len;
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int k;
      int *t = len;
      
      if (!len)
	len = &k;
      
      switch (INTVAL (operands[2]))
	{
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 <= reg1)
	      return (AS2 (mov,%A0,%B1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS1 (clr,%D0));
	    else if (reg0 == reg1 + 1)
	      return *len = 1, AS1 (clr,%D0);
	    else
	      return (AS1 (clr,%D0)     CR_TAB
		      AS2 (mov,%C0,%D1) CR_TAB
		      AS2 (mov,%B0,%C1) CR_TAB
		      AS2 (mov,%A0,%B1)); 
	  }
	  
	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (AVR_ENHANCED && (reg0 != reg1 + 2))
	      {
		*len = 3;
		return (AS2 (movw,%A0,%C1) CR_TAB
			AS1 (clr,%C0)      CR_TAB
			AS1 (clr,%D0));
	      }
	    if (reg0 <= reg1 + 1)
	      return (AS2 (mov,%A0,%C1) CR_TAB
		      AS2 (mov,%B0,%D1) CR_TAB
		      AS1 (clr,%C0)     CR_TAB
		      AS1 (clr,%D0));
	    else if (reg0 == reg1 + 2)
	      return *len = 2, (AS1 (clr,%C0)     CR_TAB
				AS1 (clr,%D0));
	    else
	      return (AS2 (mov,%B0,%D1) CR_TAB
		      AS2 (mov,%A0,%C1) CR_TAB
		      AS1 (clr,%C0)     CR_TAB
		      AS1 (clr,%D0));
	  }
	  
	case 24:
	  if (true_regnum (operands[0]) != true_regnum (operands[1]) + 3)
	    return *len = 4, (AS2 (mov,%A0,%D1) CR_TAB
			      AS1 (clr,%B0)     CR_TAB
			      AS1 (clr,%C0)     CR_TAB
			      AS1 (clr,%D0));
	  else
	    return *len = 3, (AS1 (clr,%B0)     CR_TAB
			      AS1 (clr,%C0)     CR_TAB
			      AS1 (clr,%D0));

	case 31:
	  *len = 6;
	  return (AS1 (clr,%A0)    CR_TAB
		  AS2 (sbrc,%D0,7) CR_TAB
		  AS1 (inc,%A0)    CR_TAB
		  AS1 (clr,%B0)    CR_TAB
		  AS1 (clr,%C0)    CR_TAB
		  AS1 (clr,%D0));
	}
      len = t;
    }
  out_shift_with_cnt ((AS1 (lsr,%D0) CR_TAB
		       AS1 (ror,%C0) CR_TAB
		       AS1 (ror,%B0) CR_TAB
		       AS1 (ror,%A0)),
		      insn, operands, len, 4);
  return "";
}

/* Modifies the length assigned to instruction INSN
 LEN is the initially computed length of the insn.  */

int
adjust_insn_length (insn, len)
     rtx insn;
     int len;
{
  rtx patt = PATTERN (insn);
  rtx set;

  if (GET_CODE (patt) == SET)
    {
      rtx op[10];
      op[1] = SET_SRC (patt);
      op[0] = SET_DEST (patt);
      if (general_operand (op[1], VOIDmode)
	  && general_operand (op[0], VOIDmode))
	{
	  switch (GET_MODE (op[0]))
	    {
	    case QImode:
	      output_movqi (insn, op, &len);
	      break;
	    case HImode:
	      output_movhi (insn, op, &len);
	      break;
	    case SImode:
	    case SFmode:
	      output_movsisf (insn, op, &len);
	      break;
	    default:
	      break;
	    }
	}
      else if (op[0] == cc0_rtx && REG_P (op[1]))
	{
	  switch (GET_MODE (op[1]))
	    {
	    case HImode: out_tsthi (insn,&len); break;
	    case SImode: out_tstsi (insn,&len); break;
	    default: break;
	    }
	}
      else if (GET_CODE (op[1]) == AND)
	{
	  if (GET_CODE (XEXP (op[1],1)) == CONST_INT)
	    {
	      HOST_WIDE_INT mask = INTVAL (XEXP (op[1],1));
	      if (GET_MODE (op[1]) == SImode)
		len = (((mask & 0xff) != 0xff)
		       + ((mask & 0xff00) != 0xff00)
		       + ((mask & 0xff0000L) != 0xff0000L)
		       + ((mask & 0xff000000L) != 0xff000000L));
	      else if (GET_MODE (op[1]) == HImode)
		len = (((mask & 0xff) != 0xff)
		       + ((mask & 0xff00) != 0xff00));
	    }
	}
      else if (GET_CODE (op[1]) == IOR)
	{
	  if (GET_CODE (XEXP (op[1],1)) == CONST_INT)
	    {
	      HOST_WIDE_INT mask = INTVAL (XEXP (op[1],1));
	      if (GET_MODE (op[1]) == SImode)
		len = (((mask & 0xff) != 0)
		       + ((mask & 0xff00) != 0)
		       + ((mask & 0xff0000L) != 0)
		       + ((mask & 0xff000000L) != 0));
	      else if (GET_MODE (op[1]) == HImode)
		len = (((mask & 0xff) != 0)
		       + ((mask & 0xff00) != 0));
	    }
	}
    }
  set = single_set (insn);
  if (set)
    {
      rtx op[10];

      op[1] = SET_SRC (set);
      op[0] = SET_DEST (set);

      if (GET_CODE (patt) == PARALLEL
	  && general_operand (op[1], VOIDmode)
	  && general_operand (op[0], VOIDmode))
	{
	  if (XVECLEN (patt, 0) == 2)
	    op[2] = XVECEXP (patt, 0, 1);

	  switch (GET_MODE (op[0]))
	    {
	    case QImode:
	      len = 2;
	      break;
	    case HImode:
	      output_reload_inhi (insn, op, &len);
	      break;
	    case SImode:
	    case SFmode:
	      output_reload_insisf (insn, op, &len);
	      break;
	    default:
	      break;
	    }
	}
      else if (GET_CODE (op[1]) == ASHIFT
	  || GET_CODE (op[1]) == ASHIFTRT
	  || GET_CODE (op[1]) == LSHIFTRT)
	{
	  rtx ops[10];
	  ops[0] = op[0];
	  ops[1] = XEXP (op[1],0);
	  ops[2] = XEXP (op[1],1);
	  switch (GET_CODE (op[1]))
	    {
	    case ASHIFT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: ashlqi3_out (insn,ops,&len); break;
		case HImode: ashlhi3_out (insn,ops,&len); break;
		case SImode: ashlsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    case ASHIFTRT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: ashrqi3_out (insn,ops,&len); break;
		case HImode: ashrhi3_out (insn,ops,&len); break;
		case SImode: ashrsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    case LSHIFTRT:
	      switch (GET_MODE (op[0]))
		{
		case QImode: lshrqi3_out (insn,ops,&len); break;
		case HImode: lshrhi3_out (insn,ops,&len); break;
		case SImode: lshrsi3_out (insn,ops,&len); break;
		default: break;
		}
	      break;
	    default:
	      break;
	    }
	}
    }
  return len;
}

/* Return non-zero if register REG dead after INSN */

int
reg_unused_after (insn, reg)
     rtx insn;
     rtx reg;
{
  return (dead_or_set_p (insn, reg)
	  || (REG_P(reg) && _reg_unused_after (insn, reg)));
}

/* Return non-zero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */

int
_reg_unused_after (insn, reg)
     rtx insn;
     rtx reg;
{
  enum rtx_code code;
  rtx set;

  /* If the reg is set by this instruction, then it is safe for our
     case.  Disregard the case where this is a store to memory, since
     we are checking a register used in the store address.  */
  set = single_set (insn);
  if (set && GET_CODE (SET_DEST (set)) != MEM
      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
    return 1;

  while ((insn = NEXT_INSN (insn)))
    {
      code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 if dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return 1;
      /* else */
#endif

      if (code == JUMP_INSN)
	return 0;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and an insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  int i;
	  int retval = 0;

	  for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	    {
	      rtx this_insn = XVECEXP (PATTERN (insn), 0, i);
	      rtx set = single_set (this_insn);

	      if (GET_CODE (this_insn) == CALL_INSN)
		code = CALL_INSN;
	      else if (GET_CODE (this_insn) == JUMP_INSN)
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return 0;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return 0;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (GET_CODE (SET_DEST (set)) != MEM)
		    retval = 1;
		  else
		    return 0;
		}
	      if (set == 0
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return 0;
	    }
	  if (retval == 1)
	    return 1;
	  else if (code == JUMP_INSN)
	    return 0;
	}

      if (code == CALL_INSN)
	{
	  rtx tem;
	  for (tem = CALL_INSN_FUNCTION_USAGE (insn); tem; tem = XEXP (tem, 1))
	    if (GET_CODE (XEXP (tem, 0)) == USE
		&& REG_P (XEXP (XEXP (tem, 0), 0))
		&& reg_overlap_mentioned_p (reg, XEXP (XEXP (tem, 0), 0)))
	      return 0;
	  if (call_used_regs[REGNO (reg)]) 
	    return 1;
	}

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx set = single_set (insn);

	  if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	    return 0;
	  if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	    return GET_CODE (SET_DEST (set)) != MEM;
	  if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	    return 0;
	}
    }
  return 1;
}

/* Target hook for assembling integer objects.  The AVR version needs
   special handling for references to certain labels.  */

static bool
avr_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (size == POINTER_SIZE / BITS_PER_UNIT && aligned_p
      && ((GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_FLAG (x))
	  || GET_CODE (x) == LABEL_REF))
    {
      fputs ("\t.word\tpm(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Sets section name for declaration DECL */
  
void
unique_section (decl, reloc)
     tree decl;
     int reloc ATTRIBUTE_UNUSED;
{
  int len;
  const char *name, *prefix;
  char *string;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  /* Strip off any encoding in name.  */
  STRIP_NAME_ENCODING (name, name);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (flag_function_sections)
	prefix = ".text.";
      else
	prefix = ".text";
    }
  else 
    abort ();

  if (flag_function_sections)
    {
      len = strlen (name) + strlen (prefix);
      string = alloca (len + 1);
      sprintf (string, "%s%s", prefix, name);
      DECL_SECTION_NAME (decl) = build_string (len, string);
    }
}


/* The routine used to output NUL terminated strings.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable, especially for targets like the i386
   (where the only alternative is to output character sequences as
   comma separated lists of numbers).   */

void
gas_output_limited_string(file, str)
     FILE *file;
     const char * str;
{
  const unsigned char *_limited_str = (unsigned char *) str;
  unsigned ch;
  fprintf (file, "%s\"", STRING_ASM_OP);
  for (; (ch = *_limited_str); _limited_str++)
    {
      int escape;
      switch (escape = ESCAPES[ch])
	{
	case 0:
	  putc (ch, file);
	  break;
	case 1:
	  fprintf (file, "\\%03o", ch);
	  break;
	default:
	  putc ('\\', file);
	  putc (escape, file);
	  break;
	}
    }
  fprintf (file, "\"\n");
}

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

void
gas_output_ascii(file, str, length)
     FILE * file;
     const char * str;
     size_t length;
{
  const unsigned char *_ascii_bytes = (const unsigned char *) str;
  const unsigned char *limit = _ascii_bytes + length;
  unsigned bytes_in_chunk = 0;
  for (; _ascii_bytes < limit; _ascii_bytes++)
    {
      const unsigned char *p;
      if (bytes_in_chunk >= 60)
	{
	  fprintf (file, "\"\n");
	  bytes_in_chunk = 0;
	}
      for (p = _ascii_bytes; p < limit && *p != '\0'; p++)
	continue;
      if (p < limit && (p - _ascii_bytes) <= (signed)STRING_LIMIT)
	{
	  if (bytes_in_chunk > 0)
	    {
	      fprintf (file, "\"\n");
	      bytes_in_chunk = 0;
	    }
	  gas_output_limited_string (file, (char*)_ascii_bytes);
	  _ascii_bytes = p;
	}
      else
	{
	  int escape;
	  unsigned ch;
	  if (bytes_in_chunk == 0)
	    fprintf (file, "\t.ascii\t\"");
	  switch (escape = ESCAPES[ch = *_ascii_bytes])
	    {
	    case 0:
	      putc (ch, file);
	      bytes_in_chunk++;
	      break;
	    case 1:
	      fprintf (file, "\\%03o", ch);
	      bytes_in_chunk += 4;
	      break;
	    default:
	      putc ('\\', file);
	      putc (escape, file);
	      bytes_in_chunk += 2;
	      break;
	    }
	}
    }
  if (bytes_in_chunk > 0)
    fprintf (file, "\"\n");
}

/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

enum reg_class
class_likely_spilled_p (c)
     int c;
{
  return (c != ALL_REGS && c != ADDW_REGS);
}

/* Valid attributes:
   progmem - put data to program memory;
   signal - make a function to be hardware interrupt. After function
   prologue interrupts are disabled;
   interrupt - make a function to be hardware interrupt. After function
   prologue interrupts are enabled;
   naked     - don't generate function prologue/epilogue and `ret' command.

   Only `progmem' attribute valid for type.  */

const struct attribute_spec avr_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "progmem",   0, 0, false, false, false,  avr_handle_progmem_attribute },
  { "signal",    0, 0, true,  false, false,  avr_handle_fndecl_attribute },
  { "interrupt", 0, 0, true,  false, false,  avr_handle_fndecl_attribute },
  { "naked",     0, 0, true,  false, false,  avr_handle_fndecl_attribute },
  { NULL,        0, 0, false, false, false, NULL }
};

/* Handle a "progmem" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
avr_handle_progmem_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	{
	  /* This is really a decl attribute, not a type attribute,
	     but try to handle it for GCC 3.0 backwards compatibility.  */

	  tree type = TREE_TYPE (*node);
	  tree attr = tree_cons (name, args, TYPE_ATTRIBUTES (type));
	  tree newtype = build_type_attribute_variant (type, attr);

	  TYPE_MAIN_VARIANT (newtype) = TYPE_MAIN_VARIANT (type);
	  TREE_TYPE (*node) = newtype;
	  *no_add_attrs = true;
        }
      else if (TREE_STATIC (*node) || DECL_EXTERNAL (*node))
	{
	  if (DECL_INITIAL (*node) == NULL_TREE && !DECL_EXTERNAL (*node))
	    {
	      warning ("only initialized variables can be placed into "
		       "program memory area");
	      *no_add_attrs = true;
	    }
	}
      else
	{
	  warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */
static tree
avr_handle_fndecl_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Look for attribute `progmem' in DECL
   if found return 1, otherwise 0.  */

int
avr_progmem_p (decl)
     tree decl;
{
  tree a;

  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  if (NULL_TREE
      != lookup_attribute ("progmem", DECL_ATTRIBUTES (decl)))
    return 1;

  a=decl;
  do
    a = TREE_TYPE(a);
  while (TREE_CODE (a) == ARRAY_TYPE);

  if (a == error_mark_node)
    return 0;

  if (NULL_TREE != lookup_attribute ("progmem", TYPE_ATTRIBUTES (a)))
    return 1;
  
  return 0;
}

/* Encode section information about tree DECL */
  
void
encode_section_info (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
  else if ((TREE_STATIC (decl) || DECL_EXTERNAL (decl))
	   && TREE_CODE (decl) == VAR_DECL
	   && avr_progmem_p (decl))
    {
      static const char *const dsec = ".progmem.data";
      DECL_SECTION_NAME (decl) = build_string (strlen (dsec), dsec);
      TREE_READONLY (decl) = 1;
    }
}   

/* Outputs to the stdio stream FILE some
   appropriate text to go at the start of an assembler file.  */

void
asm_file_start (file)
     FILE *file;
{
  output_file_directive (file, main_input_filename);
  fprintf (file, "\t.arch %s\n", avr_mcu_name);
  fputs ("__SREG__ = 0x3f\n"
	 "__SP_H__ = 0x3e\n"
	 "__SP_L__ = 0x3d\n", file);
  
  fputs ("__tmp_reg__ = 0\n" 
	 "__zero_reg__ = 1\n", file);
  
  commands_in_file = 0;
  commands_in_prologues = 0;
  commands_in_epilogues = 0;
}

/* Outputs to the stdio stream FILE some
   appropriate text to go at the end of an assembler file.  */

void
asm_file_end (file)
     FILE *file;
{
  fprintf (file,
	   "/* File %s: code %4d = 0x%04x (%4d), prologues %3d, epilogues %3d */\n",
	   main_input_filename,
	   commands_in_file,
	   commands_in_file,
	   commands_in_file - commands_in_prologues - commands_in_epilogues,
	   commands_in_prologues, commands_in_epilogues);
}

/* Choose the order in which to allocate hard registers for
   pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.  */

void
order_regs_for_local_alloc ()
{
  unsigned int i;
  static const int order_0[] = {
    24,25,
    18,19,
    20,21,
    22,23,
    30,31,
    26,27,
    28,29,
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    0,1,
    32,33,34,35
  };
  static const int order_1[] = {
    18,19,
    20,21,
    22,23,
    24,25,
    30,31,
    26,27,
    28,29,
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    0,1,
    32,33,34,35
  };
  static const int order_2[] = {
    25,24,
    23,22,
    21,20,
    19,18,
    30,31,
    26,27,
    28,29,
    17,16,
    15,14,13,12,11,10,9,8,7,6,5,4,3,2,
    1,0,
    32,33,34,35
  };
  
  const int *order = (TARGET_ORDER_1 ? order_1 :
		      TARGET_ORDER_2 ? order_2 :
		      order_0);
  for (i=0; i < ARRAY_SIZE (order_0); ++i)
      reg_alloc_order[i] = order[i];
}

/* Calculate the cost of X code of the expression in which it is contained,
   found in OUTER_CODE */

int
default_rtx_costs (X, code, outer_code)
     rtx X;
     enum rtx_code code;
     enum rtx_code outer_code;
{
  int cost=0;
  switch (code)
    {
    case SYMBOL_REF:
    case LABEL_REF:
      cost = 2 * GET_MODE_SIZE (GET_MODE (X));
      break;
    case MEM:
      if (outer_code != SET)
	cost = 1;
      if (GET_CODE (XEXP (X,0)) == SYMBOL_REF)
	cost += 2 * GET_MODE_SIZE (GET_MODE (X));
      else
	cost += GET_MODE_SIZE (GET_MODE (X));
      break;
    case CONST_INT:
      cost = 0;
      break;
    case SIGN_EXTEND:
      if (outer_code == SET)
	cost = GET_MODE_SIZE (GET_MODE (X));
      else
	cost = -GET_MODE_SIZE (GET_MODE (X));
      break;
    case ZERO_EXTEND:
      if (outer_code == SET)
	cost = GET_MODE_SIZE (GET_MODE (X));
      else
	cost = -1;
      break;
    case PLUS:
    case MINUS:
      if (outer_code == SET)
	{
	  if (X == stack_pointer_rtx)
	    cost = -10;
	  else if (GET_CODE (XEXP (X,1)) == CONST_INT)
	    cost = (INTVAL (XEXP (X,1)) <= 63 ? 1 :
		     GET_MODE_SIZE (GET_MODE (X)));
	  else
	    cost = GET_MODE_SIZE (GET_MODE (X));
	}
      break;
    case COMPARE:
      if (GET_CODE (XEXP (X,1)) == CONST_INT)
	cost = GET_MODE_SIZE (GET_MODE (XEXP (X,0)));
      break;
    default:
      break;
    }
  return cost;
}

/* Calculate the cost of a memory address */

int
avr_address_cost (x)
     rtx x;
{
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x,1)) == CONST_INT
      && (REG_P (XEXP (x,0)) || GET_CODE (XEXP (x,0)) == SUBREG)
      && INTVAL (XEXP (x,1)) >= 61)
    return 18;
  if (CONSTANT_ADDRESS_P (x))
    {
      if (io_address_p (x, 1))
	return 2;
      return 4;
    }
  return 4;
}

/*  EXTRA_CONSTRAINT helper */

int
extra_constraint (x, c)
     rtx x;
     int c;
{
  if (c == 'Q'
      && GET_CODE (x) == MEM
      && GET_CODE (XEXP (x,0)) == PLUS)
    {
	  if (TARGET_ALL_DEBUG)
	    {
	      fprintf (stderr, ("extra_constraint:\n"
				"reload_completed: %d\n"
				"reload_in_progress: %d\n"),
		       reload_completed, reload_in_progress);
	      debug_rtx (x);
	    }
      if (GET_CODE (x) == MEM
	  && GET_CODE (XEXP (x,0)) == PLUS
	  && REG_P (XEXP (XEXP (x,0), 0))
	  && GET_CODE (XEXP (XEXP (x,0), 1)) == CONST_INT
	  && (INTVAL (XEXP (XEXP (x,0), 1))
	      <= MAX_LD_OFFSET (GET_MODE (x))))
	{
	  rtx xx = XEXP (XEXP (x,0), 0);
	  int regno = REGNO (xx);
	  if (TARGET_ALL_DEBUG)
	    {
	      fprintf (stderr, ("extra_constraint:\n"
				"reload_completed: %d\n"
				"reload_in_progress: %d\n"),
		       reload_completed, reload_in_progress);
	      debug_rtx (x);
	    }
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    return 1;		/* allocate pseudos */
	  else if (regno == REG_Z || regno == REG_Y)
	    return 1;		/* strictly check */
	  else if (xx == frame_pointer_rtx
		   || xx == arg_pointer_rtx)
	    return 1;		/* XXX frame & arg pointer checks */
	}
    }
  return 0;
}

/* Convert condition code CONDITION to the valid AVR condition code */

RTX_CODE
avr_normalize_condition (condition)
     RTX_CODE condition;
{
  switch (condition)
    {
    case GT:
      return GE;
    case GTU:
      return GEU;
    case LE:
      return LT;
    case LEU:
      return LTU;
    default:
      abort ();
    }
}

/* This fnction optimizes conditional jumps */

void
machine_dependent_reorg (first_insn)
     rtx first_insn;
{
  rtx insn, pattern;
  
  for (insn = first_insn; insn; insn = NEXT_INSN (insn))
    {
      if (! (GET_CODE (insn) == INSN
	     || GET_CODE (insn) == CALL_INSN
	     || GET_CODE (insn) == JUMP_INSN)
	  || !single_set (insn))
	continue;

      pattern = PATTERN (insn);

      if (GET_CODE (pattern) == PARALLEL)
	pattern = XVECEXP (pattern, 0, 0);
      if (GET_CODE (pattern) == SET
	  && SET_DEST (pattern) == cc0_rtx
	  && compare_diff_p (insn))
	{
	  if (GET_CODE (SET_SRC (pattern)) == COMPARE)
	    {
	      /* Now we work under compare insn */
	      
	      pattern = SET_SRC (pattern);
	      if (true_regnum (XEXP (pattern,0)) >= 0
		  && true_regnum (XEXP (pattern,1)) >= 0 )
		{
		  rtx x = XEXP (pattern,0);
		  rtx next = next_real_insn (insn);
		  rtx pat = PATTERN (next);
		  rtx src = SET_SRC (pat);
		  rtx t = XEXP (src,0);
		  PUT_CODE (t, swap_condition (GET_CODE (t)));
		  XEXP (pattern,0) = XEXP (pattern,1);
		  XEXP (pattern,1) = x;
		  INSN_CODE (next) = -1;
		}
	      else if (true_regnum (XEXP (pattern,0)) >= 0
		       && GET_CODE (XEXP (pattern,1)) == CONST_INT)
		{
		  rtx x = XEXP (pattern,1);
		  rtx next = next_real_insn (insn);
		  rtx pat = PATTERN (next);
		  rtx src = SET_SRC (pat);
		  rtx t = XEXP (src,0);
		  enum machine_mode mode = GET_MODE (XEXP (pattern, 0));

		  if (avr_simplify_comparision_p (mode, GET_CODE (t), x))
		    {
		      XEXP (pattern, 1) = gen_int_mode (INTVAL (x) + 1, mode);
		      PUT_CODE (t, avr_normalize_condition (GET_CODE (t)));
		      INSN_CODE (next) = -1;
		      INSN_CODE (insn) = -1;
		    }
		}
	    }
	  else if (true_regnum (SET_SRC (pattern)) >= 0)
	    {
	      /* This is a tst insn */
	      rtx next = next_real_insn (insn);
	      rtx pat = PATTERN (next);
	      rtx src = SET_SRC (pat);
	      rtx t = XEXP (src,0);

	      PUT_CODE (t, swap_condition (GET_CODE (t)));
	      SET_SRC (pattern) = gen_rtx (NEG,
					   GET_MODE (SET_SRC (pattern)),
					   SET_SRC (pattern));
	      INSN_CODE (next) = -1;
	      INSN_CODE (insn) = -1;
	    }
	}
    }
}

/* Returns register number for function return value.*/

int
avr_ret_register ()
{
  return 24;
}

/* Ceate an RTX representing the place where a
   library function returns a value of mode MODE.  */

rtx
avr_libcall_value (mode)
     enum machine_mode mode;
{
  int offs = GET_MODE_SIZE (mode);
  if (offs < 2)
    offs = 2;
  return gen_rtx (REG, mode, RET_REGISTER + 2 - offs);
}

/* Create an RTX representing the place where a
   function returns a value of data type VALTYPE.  */

rtx
avr_function_value (type, func)
     tree type;
     tree func ATTRIBUTE_UNUSED;
{
  unsigned int offs;
  
  if (TYPE_MODE (type) != BLKmode)
    return avr_libcall_value (TYPE_MODE (type));
  
  offs = int_size_in_bytes (type);
  if (offs < 2)
    offs = 2;
  if (offs > 2 && offs < GET_MODE_SIZE (SImode))
    offs = GET_MODE_SIZE (SImode);
  else if (offs > GET_MODE_SIZE (SImode) && offs < GET_MODE_SIZE (DImode))
    offs = GET_MODE_SIZE (DImode);
  
  return gen_rtx (REG, BLKmode, RET_REGISTER + 2 - offs);
}

/* Returns non-zero if the number MASK has only one bit set.  */

int
mask_one_bit_p (mask)
     HOST_WIDE_INT mask;
{
  int i;
  unsigned HOST_WIDE_INT n=mask;
  for (i = 0; i < 32; ++i)
    {
      if (n & 0x80000000L)
	{
	  if (n & 0x7fffffffL)
	    return 0;
	  else
	    return 32-i;
	}
      n<<=1;
    }
  return 0; 
}


/* Places additional restrictions on the register class to
   use when it is necessary to copy value X into a register
   in class CLASS.  */

enum reg_class
preferred_reload_class (x, class)
     rtx x ATTRIBUTE_UNUSED;
     enum reg_class class;
{
  return class;
}

int
test_hard_reg_class (class, x)
     enum reg_class class;
     rtx x;
{
  int regno = true_regnum (x);
  if (regno < 0)
    return 0;

  if (TEST_HARD_REG_CLASS (class, regno))
    return 1;

  return 0;
}

void
debug_hard_reg_set (set)
     HARD_REG_SET set;
{
  int i;
  for (i=0; i < FIRST_PSEUDO_REGISTER; ++i)
    {
      if (TEST_HARD_REG_BIT (set, i))
	{
	  fprintf (stderr, "r%-2d ", i);
	}
    }
  fprintf (stderr, "\n");
}

int
jump_over_one_insn_p (insn, dest)
     rtx insn;
     rtx dest;
{
  int uid = INSN_UID (GET_CODE (dest) == LABEL_REF
		      ? XEXP (dest, 0)
		      : dest);
  int jump_addr = INSN_ADDRESSES (INSN_UID (insn));
  int dest_addr = INSN_ADDRESSES (uid);
  return dest_addr - jump_addr == 2;
}

/* Returns 1 if a value of mode MODE can be stored starting with hard
   register number REGNO.  On the enhanced core, anything larger than
   1 byte must start in even numbered register for "movw" to work
   (this way we don't have to check for odd registers everywhere).  */

int
avr_hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  /* Bug workaround: recog.c (peep2_find_free_register) and probably
     a few other places assume that the frame pointer is a single hard
     register, so r29 may be allocated and overwrite the high byte of
     the frame pointer.  Do not allow any value to start in r29.  */
  if (regno == REG_Y + 1)
    return 0;

  if (mode == QImode)
    return 1;
  /*  if (regno < 24 && !AVR_ENHANCED)
      return 1;*/
  return !(regno & 1);
}

/* Returns 1 if we know register operand OP was 0 before INSN.  */

static int
reg_was_0 (insn, op)
     rtx insn;
     rtx op;
{
  rtx link;
  return (optimize > 0 && insn && op && REG_P (op)
	  && (link = find_reg_note (insn, REG_WAS_0, 0))
	  /* Make sure the insn that stored the 0 is still present.  */
	  && ! INSN_DELETED_P (XEXP (link, 0))
	  && GET_CODE (XEXP (link, 0)) != NOTE
	  /* Make sure cross jumping didn't happen here.  */
	  && no_labels_between_p (XEXP (link, 0), insn)
	  /* Make sure the reg hasn't been clobbered.  */
	  && ! reg_set_between_p (op, XEXP (link, 0), insn));
}

/* Returns 1 if X is a valid address for an I/O register of size SIZE
   (1 or 2).  Used for lds/sts -> in/out optimization.  */

static int
io_address_p (x, size)
     rtx x;
     int size;
{
  return (optimize > 0 && GET_CODE (x) == CONST_INT
	  && INTVAL (x) >= 0x20 && INTVAL (x) <= 0x60 - size);
}

/* Returns nonzero (bit number + 1) if X, or -X, is a constant power of 2.  */

int
const_int_pow2_p (x)
     rtx x;
{
  if (GET_CODE (x) == CONST_INT)
    {
      HOST_WIDE_INT d = INTVAL (x);
      HOST_WIDE_INT abs_d = (d >= 0) ? d : -d;
      return exact_log2 (abs_d) + 1;
    }
  return 0;
}

const char *
output_reload_inhi (insn, operands, len)
     rtx insn ATTRIBUTE_UNUSED;
     rtx *operands;
     int *len;
{
  int tmp;
  if (!len)
    len = &tmp;
      
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int val = INTVAL (operands[1]);
      if ((val & 0xff) == 0)
	{
	  *len = 3;
	  return (AS2 (mov,%A0,__zero_reg__) CR_TAB
		  AS2 (ldi,%2,hi8(%1))       CR_TAB
		  AS2 (mov,%B0,%2));
	}
      else if ((val & 0xff00) == 0)
	{
	  *len = 3;
	  return (AS2 (ldi,%2,lo8(%1)) CR_TAB
		  AS2 (mov,%A0,%2)     CR_TAB
		  AS2 (mov,%B0,__zero_reg__));
	}
      else if ((val & 0xff) == ((val & 0xff00) >> 8))
	{
	  *len = 3;
	  return (AS2 (ldi,%2,lo8(%1)) CR_TAB
		  AS2 (mov,%A0,%2)     CR_TAB
		  AS2 (mov,%B0,%2));
	}
    }
  *len = 4;
  return (AS2 (ldi,%2,lo8(%1)) CR_TAB
	  AS2 (mov,%A0,%2)     CR_TAB
	  AS2 (ldi,%2,hi8(%1)) CR_TAB
	  AS2 (mov,%B0,%2));
}


const char *
output_reload_insisf (insn, operands, len)
     rtx insn ATTRIBUTE_UNUSED;
     rtx *operands;
     int *len;
{
  rtx src = operands[1];
  int cnst = (GET_CODE (src) == CONST_INT);

  if (len)
    {
      if (cnst)
	*len = 4 + ((INTVAL (src) & 0xff) != 0)
		+ ((INTVAL (src) & 0xff00) != 0)
		+ ((INTVAL (src) & 0xff0000) != 0)
		+ ((INTVAL (src) & 0xff000000) != 0);
      else
	*len = 8;

      return "";
    }

  if (cnst && ((INTVAL (src) & 0xff) == 0))
    output_asm_insn (AS2 (mov, %A0, __zero_reg__), operands);
  else
    {
      output_asm_insn (AS2 (ldi, %2, lo8(%1)), operands);
      output_asm_insn (AS2 (mov, %A0, %2), operands);
    }
  if (cnst && ((INTVAL (src) & 0xff00) == 0))
    output_asm_insn (AS2 (mov, %B0, __zero_reg__), operands);
  else
    {
      output_asm_insn (AS2 (ldi, %2, hi8(%1)), operands);
      output_asm_insn (AS2 (mov, %B0, %2), operands);
    }
  if (cnst && ((INTVAL (src) & 0xff0000) == 0))
    output_asm_insn (AS2 (mov, %C0, __zero_reg__), operands);
  else
    {
      output_asm_insn (AS2 (ldi, %2, hlo8(%1)), operands);
      output_asm_insn (AS2 (mov, %C0, %2), operands);
    }
  if (cnst && ((INTVAL (src) & 0xff000000) == 0))
    output_asm_insn (AS2 (mov, %D0, __zero_reg__), operands);
  else
    {
      output_asm_insn (AS2 (ldi, %2, hhi8(%1)), operands);
      output_asm_insn (AS2 (mov, %D0, %2), operands);
    }
  return "";
}

void
avr_output_bld (operands, bit_nr)
     rtx operands[];
     int bit_nr;
{
  static char s[] = "bld %A0,0";

  s[5] = 'A' + (bit_nr >> 3);
  s[8] = '0' + (bit_nr & 7);
  output_asm_insn (s, operands);
}

void
avr_output_addr_vec_elt (stream, value)
     FILE *stream;
     int value;
{
  if (AVR_MEGA)
    fprintf (stream, "\t.word pm(.L%d)\n", value);
  else
    fprintf (stream, "\trjmp .L%d\n", value);

  jump_tables_size++;
}

/* Returns 1 if SCRATCH are safe to be allocated as a scratch
   registers (for a define_peephole2) in the current function.  */

int
avr_peep2_scratch_safe (scratch)
     rtx scratch;
{
  if ((interrupt_function_p (current_function_decl)
       || signal_function_p (current_function_decl))
      && leaf_function_p ())
    {
      int first_reg = true_regnum (scratch);
      int last_reg = first_reg + GET_MODE_SIZE (GET_MODE (scratch)) - 1;
      int reg;

      for (reg = first_reg; reg <= last_reg; reg++)
	{
	  if (!regs_ever_live[reg])
	    return 0;
	}
    }
  return 1;
}
