/* Definitions of target machine for GNU compiler, for MMIX.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Hans-Peter Nilsson (hp@bitrange.com)

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
#include "hashtab.h"
#include "insn-config.h"
#include "output.h"
#include "flags.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "dwarf2.h"
#include "debug.h"
#include "tm_p.h"
#include "integrate.h"
#include "target.h"
#include "target-def.h"

/* First some local helper definitions.  */
#define MMIX_FIRST_GLOBAL_REGNUM 32

/* We'd need a current_function_has_landing_pad.  It's marked as such when
   a nonlocal_goto_receiver is expanded.  Not just a C++ thing, but
   mostly.  */
#define MMIX_CFUN_HAS_LANDING_PAD (cfun->machine->has_landing_pad != 0)

/* We have no means to tell DWARF 2 about the register stack, so we need
   to store the return address on the stack if an exception can get into
   this function.  FIXME: Narrow condition.  */
#define MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS \
 (flag_exceptions && ! leaf_function_p ())

#define IS_MMIX_EH_RETURN_DATA_REG(REGNO)	\
 (current_function_calls_eh_return		\
  && (EH_RETURN_DATA_REGNO (0) == REGNO		\
      || EH_RETURN_DATA_REGNO (1) == REGNO	\
      || EH_RETURN_DATA_REGNO (2) == REGNO	\
      || EH_RETURN_DATA_REGNO (3) == REGNO))

/* The canonical saved comparison operands for non-cc0 machines, set in
   the compare expander.  */
rtx mmix_compare_op0;
rtx mmix_compare_op1;

/* We ignore some options with arguments.  They are passed to the linker,
   but also ends up here because they start with "-m".  We tell the driver
   to store them in a variable we don't inspect.  */
const char *mmix_cc1_ignored_option;

/* Declarations of locals.  */

/* This is used in the prologue for what number to pass in a PUSHJ or
   PUSHGO insn.  */
static int mmix_highest_saved_stack_register;

/* Intermediate for insn output.  */
static int mmix_output_destination_register;

static void mmix_output_shiftvalue_op_from_str
  PARAMS ((FILE *, const char *, HOST_WIDEST_INT));
static void mmix_output_shifted_value PARAMS ((FILE *, HOST_WIDEST_INT));
static void mmix_output_condition PARAMS ((FILE *, rtx, int));
static HOST_WIDEST_INT mmix_intval PARAMS ((rtx));
static void mmix_output_octa PARAMS ((FILE *, HOST_WIDEST_INT, int));
static bool mmix_assemble_integer PARAMS ((rtx, unsigned int, int));
static void mmix_init_machine_status PARAMS ((struct function *));

extern void mmix_target_asm_function_prologue
  PARAMS ((FILE *, HOST_WIDE_INT));
extern void mmix_target_asm_function_epilogue
  PARAMS ((FILE *, HOST_WIDE_INT));


/* Target structure macros.  Listed by node.  See `Using and Porting GCC'
   for a general description.  */

/* Node: Function Entry */

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP NULL
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP NULL
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP NULL
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER mmix_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE mmix_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE mmix_target_asm_function_epilogue

struct gcc_target targetm = TARGET_INITIALIZER;

/* Functions that are expansions for target macros.
   See Target Macros in `Using and Porting GCC'.  */

/* OVERRIDE_OPTIONS.  */

void
mmix_override_options ()
{
  /* Should we err or should we warn?  Hmm.  At least we must neutralize
     it.  For example the wrong kind of case-tables will be generated with
     PIC; we use absolute address items for mmixal compatibility.  FIXME:
     They could be relative if we just elide them to after all pertinent
     labels.  */
  if (flag_pic)
    {
      warning ("-f%s not supported: ignored", (flag_pic > 1) ? "PIC" : "pic");
      flag_pic = 0;
    }

  /* All other targets add GC roots from their override_options function,
     so play along.  */
  ggc_add_rtx_root (&mmix_compare_op0, 1);
  ggc_add_rtx_root (&mmix_compare_op1, 1);
}

/* INIT_EXPANDERS.  */

void
mmix_init_expanders ()
{
  init_machine_status = mmix_init_machine_status;
}

/* Set the per-function data.  */

static void
mmix_init_machine_status (f)
     struct function *f;
{
  f->machine = xcalloc (1, sizeof (struct machine_function));
}

/* DATA_ALIGNMENT.
   We have trouble getting the address of stuff that is located at other
   than 32-bit alignments (GETA requirements), so try to give everything
   at least 32-bit alignment. */

int
mmix_data_alignment (type, basic_align)
     tree type ATTRIBUTE_UNUSED;
     int basic_align;
{
  if (basic_align < 32)
    return 32;

  return basic_align;
}

/* CONSTANT_ALIGNMENT.  */

int
mmix_constant_alignment (constant, basic_align)
     tree constant ATTRIBUTE_UNUSED;
     int basic_align;
{
  if (basic_align < 32)
    return 32;

  return basic_align;
}

/* LOCAL_ALIGNMENT.  */

int
mmix_local_alignment (type, basic_align)
     tree type ATTRIBUTE_UNUSED;
     int basic_align;
{
  if (basic_align < 32)
    return 32;

  return basic_align;
}

/* CONDITIONAL_REGISTER_USAGE.  */

void
mmix_conditional_register_usage ()
{
  int i;

  if (TARGET_ABI_GNU)
    {
      static const int gnu_abi_reg_alloc_order[]
	= MMIX_GNU_ABI_REG_ALLOC_ORDER;

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	reg_alloc_order[i] = gnu_abi_reg_alloc_order[i];

      /* Change the default from the mmixware ABI.  For the GNU ABI,
	 $15..$30 are call-saved just as $0..$14.  There must be one
	 call-clobbered local register for the "hole" describing number of
	 saved local registers saved by PUSHJ/PUSHGO during the function
	 call, receiving the return value at return.  So best is to use
	 the highest, $31.  It's already marked call-clobbered for the
	 mmixware ABI.  */
      for (i = 15; i <= 30; i++)
	call_used_regs[i] = 0;

      /* "Unfix" the parameter registers.  */
      for (i = MMIX_RESERVED_GNU_ARG_0_REGNUM;
	   i < MMIX_RESERVED_GNU_ARG_0_REGNUM + MMIX_MAX_ARGS_IN_REGS;
	   i++)
	fixed_regs[i] = 0;
    }

  /* Step over the ":" in special register names.  */
  if (! TARGET_TOPLEVEL_SYMBOLS)
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (reg_names[i][0] == ':')
	reg_names[i]++;
}

/* PREFERRED_RELOAD_CLASS.
   We need to extend the reload class of REMAINDER_REG and HIMULT_REG.  */

enum reg_class
mmix_preferred_reload_class (x, class)
     rtx x ATTRIBUTE_UNUSED;
     enum reg_class class;
{
  /* FIXME: Revisit.  */
  return GET_CODE (x) == MOD && GET_MODE (x) == DImode
    ? REMAINDER_REG : class;
}

/* PREFERRED_OUTPUT_RELOAD_CLASS.
   We need to extend the reload class of REMAINDER_REG and HIMULT_REG.  */

enum reg_class
mmix_preferred_output_reload_class (x, class)
     rtx x ATTRIBUTE_UNUSED;
     enum reg_class class;
{
  /* FIXME: Revisit.  */
  return GET_CODE (x) == MOD && GET_MODE (x) == DImode
    ? REMAINDER_REG : class;
}

/* SECONDARY_RELOAD_CLASS.
   We need to reload regs of REMAINDER_REG and HIMULT_REG elsewhere.  */

enum reg_class
mmix_secondary_reload_class (class, mode, x, in_p)
     enum reg_class class;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x ATTRIBUTE_UNUSED;
     int in_p ATTRIBUTE_UNUSED;
{
  if (class == REMAINDER_REG
      || class == HIMULT_REG
      || class == SYSTEM_REGS)
    return GENERAL_REGS;

  return NO_REGS;
}

/* CONST_OK_FOR_LETTER_P.  */

int
mmix_const_ok_for_letter_p (value, c)
     HOST_WIDE_INT value;
     int c;
{
  return
    (c == 'I' ? value >= 0 && value <= 255
     : c == 'J' ? value >= 0 && value <= 65535
     : c == 'K' ? value <= 0 && value >= -255
     : c == 'L' ? mmix_shiftable_wyde_value (value)
     : c == 'M' ? value == 0
     : c == 'N' ? mmix_shiftable_wyde_value (~value)
     : c == 'O' ? (value == 3 || value == 5 || value == 9
		   || value == 17)
     : 0);
}

/* CONST_DOUBLE_OK_FOR_LETTER_P.  */

int
mmix_const_double_ok_for_letter_p (value, c)
     rtx value;
     int c;
{
  return
    (c == 'G' ? value == CONST0_RTX (GET_MODE (value))
     : 0);
}

/* EXTRA_CONSTRAINT.
   We need this since our constants are not always expressible as
   CONST_INT:s, but rather often as CONST_DOUBLE:s.  */

int
mmix_extra_constraint (x, c, strict)
     rtx x;
     int c;
     int strict;
{
  HOST_WIDEST_INT value;

  /* When checking for an address, we need to handle strict vs. non-strict
     register checks.  Don't use address_operand, but instead its
     equivalent (its callee, which it is just a wrapper for),
     memory_operand_p and the strict-equivalent strict_memory_address_p.  */
  if (c == 'U')
    return
      strict
      ? strict_memory_address_p (Pmode, x)
      : memory_address_p (Pmode, x);

  /* R asks whether x is to be loaded with GETA or something else.  Right
     now, only a SYMBOL_REF and LABEL_REF can fit for
     TARGET_BASE_ADDRESSES.

     Only constant symbolic addresses apply.  With TARGET_BASE_ADDRESSES,
     we just allow straight LABEL_REF or SYMBOL_REFs with SYMBOL_REF_FLAG
     set right now; only function addresses and code labels.  If we change
     to let SYMBOL_REF_FLAG be set on other symbols, we have to check
     inside CONST expressions.  When TARGET_BASE_ADDRESSES is not in
     effect, a "raw" constant check together with mmix_constant_address_p
     is all that's needed; we want all constant addresses to be loaded
     with GETA then.  */
  if (c == 'R')
    return
      GET_CODE (x) != CONST_INT && GET_CODE (x) != CONST_DOUBLE
      && mmix_constant_address_p (x)
      && (! TARGET_BASE_ADDRESSES
	  || (GET_CODE (x) == LABEL_REF
	      || (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_FLAG (x))));

  if (GET_CODE (x) != CONST_DOUBLE || GET_MODE (x) != VOIDmode)
    return 0;

  value = mmix_intval (x);

  /* We used to map Q->J, R->K, S->L, T->N, U->O, but we don't have to any
     more ('U' taken for address_operand, 'R' similarly).  Some letters map
     outside of CONST_INT, though; we still use 'S' and 'T'.  */
  if (c == 'S')
    return mmix_shiftable_wyde_value (value);
  else if (c == 'T')
    return mmix_shiftable_wyde_value (~value);
  return 0;
}

/* DYNAMIC_CHAIN_ADDRESS.  */

rtx
mmix_dynamic_chain_address (frame)
     rtx frame;
{
  /* FIXME: the frame-pointer is stored at offset -8 from the current
     frame-pointer.  Unfortunately, the caller assumes that a
     frame-pointer is present for *all* previous frames.  There should be
     a way to say that that cannot be done, like for RETURN_ADDR_RTX.  */
  return plus_constant (frame, -8);
}

/* STARTING_FRAME_OFFSET.  */

int
mmix_starting_frame_offset ()
{
  /* The old frame pointer is in the slot below the new one, so
     FIRST_PARM_OFFSET does not need to depend on whether the
     frame-pointer is needed or not.  We have to adjust for the register
     stack pointer being located below the saved frame pointer.
     Similarly, we store the return address on the stack too, for
     exception handling, and always if we save the register stack pointer.  */
  return
    (-8
     + (MMIX_CFUN_HAS_LANDING_PAD
	? -16 : (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS ? -8 : 0)));
}

/* RETURN_ADDR_RTX.  */

rtx
mmix_return_addr_rtx (count, frame)
     int count;
     rtx frame ATTRIBUTE_UNUSED;
{
  return count == 0
    ? (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS
       /* FIXME: Set frame_alias_set on the following.  (Why?)
	  See mmix_initial_elimination_offset for the reason we can't use
	  get_hard_reg_initial_val for both.  Always using a stack slot
	  and not a register would be suboptimal.  */
       ? validize_mem (gen_rtx_MEM (Pmode, plus_constant (frame_pointer_rtx, -16)))
       : get_hard_reg_initial_val (Pmode, MMIX_INCOMING_RETURN_ADDRESS_REGNUM))
    : NULL_RTX;
}

/* SETUP_FRAME_ADDRESSES.  */

void
mmix_setup_frame_addresses ()
{
  /* Nothing needed at the moment.  */
}

/* The difference between the (imaginary) frame pointer and the stack
   pointer.  Used to eliminate the frame pointer.  */

int
mmix_initial_elimination_offset (fromreg, toreg)
     int fromreg;
     int toreg;
{
  int regno;
  int fp_sp_offset
    = (get_frame_size () + current_function_outgoing_args_size + 7) & ~7;

  /* There is no actual offset between these two virtual values, but for
     the frame-pointer, we have the old one in the stack position below
     it, so the offset for the frame-pointer to the stack-pointer is one
     octabyte larger.  */
  if (fromreg == MMIX_ARG_POINTER_REGNUM
      && toreg == MMIX_FRAME_POINTER_REGNUM)
    return 0;

  /* The difference is the size of local variables plus the size of
     outgoing function arguments that would normally be passed as
     registers but must be passed on stack because we're out of
     function-argument registers.  Only global saved registers are
     counted; the others go on the register stack.

     The frame-pointer is counted too if it is what is eliminated, as we
     need to balance the offset for it from STARTING_FRAME_OFFSET.

     Also add in the slot for the register stack pointer we save if we
     have a landing pad.

     Unfortunately, we can't access $0..$14, from unwinder code easily, so
     store the return address in a frame slot too.  FIXME: Only for
     non-leaf functions.  FIXME: Always with a landing pad, because it's
     hard to know whether we need the other at the time we know we need
     the offset for one (and have to state it).  It's a kludge until we
     can express the register stack in the EH frame info.

     We have to do alignment here; get_frame_size will not return a
     multiple of STACK_BOUNDARY.  FIXME: Add note in manual.  */

  for (regno = MMIX_FIRST_GLOBAL_REGNUM;
       regno <= 255;
       regno++)
    if ((regs_ever_live[regno] && ! call_used_regs[regno])
	|| IS_MMIX_EH_RETURN_DATA_REG (regno))
      fp_sp_offset += 8;

  return fp_sp_offset
    + (MMIX_CFUN_HAS_LANDING_PAD
       ? 16 : (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS ? 8 : 0))
    + (fromreg == MMIX_ARG_POINTER_REGNUM ? 0 : 8);
}

/* Return an rtx for a function argument to go in a register, and 0 for
   one that must go on stack.  */

rtx
mmix_function_arg (argsp, mode, type, named, incoming)
     const CUMULATIVE_ARGS * argsp;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
     int incoming;
{
  /* Handling of the positional dummy parameter for varargs gets nasty.
     Check execute/991216-3 and function.c:assign_params.  We have to say
     that the dummy parameter goes on stack in order to get the correct
     offset when va_start and va_arg is applied.  FIXME: Should do TRT by
     itself in the gcc core.  */
  if ((! named && incoming && current_function_varargs) || argsp->now_varargs)
    return NULL_RTX;

  /* Last-argument marker.  */
  if (type == void_type_node)
    return (argsp->regs < MMIX_MAX_ARGS_IN_REGS)
      ? gen_rtx_REG (mode,
		     (incoming
		      ? MMIX_FIRST_INCOMING_ARG_REGNUM
		      : MMIX_FIRST_ARG_REGNUM) + argsp->regs)
      : NULL_RTX;

  return (argsp->regs < MMIX_MAX_ARGS_IN_REGS
	  && !MUST_PASS_IN_STACK (mode, type)
	  && (GET_MODE_BITSIZE (mode) <= 64
	      || argsp->lib
	      || TARGET_LIBFUNC))
    ? gen_rtx_REG (mode,
		   (incoming
		    ? MMIX_FIRST_INCOMING_ARG_REGNUM
		    : MMIX_FIRST_ARG_REGNUM)
		   + argsp->regs)
    : NULL_RTX;
}

/* Returns nonzero for everything that goes by reference, 0 for
   everything that goes by value.  */

int
mmix_function_arg_pass_by_reference (argsp, mode, type, named)
     const CUMULATIVE_ARGS * argsp;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  /* FIXME: Check: I'm not sure the MUST_PASS_IN_STACK check is
     necessary.  */
  return
    MUST_PASS_IN_STACK (mode, type)
    || (MMIX_FUNCTION_ARG_SIZE (mode, type) > 8
	&& !TARGET_LIBFUNC
	&& !argsp->lib);
}

/* Return nonzero if regno is a register number where a parameter is
   passed, and 0 otherwise.  */

int
mmix_function_arg_regno_p (regno, incoming)
     int regno;
     int incoming;
{
  int first_arg_regnum
    = incoming ? MMIX_FIRST_INCOMING_ARG_REGNUM : MMIX_FIRST_ARG_REGNUM;

  return regno >= first_arg_regnum
    && regno < first_arg_regnum + MMIX_MAX_ARGS_IN_REGS;
}

/* FUNCTION_OUTGOING_VALUE.  */

rtx
mmix_function_outgoing_value (valtype, func)
     tree valtype;
     tree func ATTRIBUTE_UNUSED;
{
  enum machine_mode mode = TYPE_MODE (valtype);
  enum machine_mode cmode;
  int first_val_regnum = MMIX_OUTGOING_RETURN_VALUE_REGNUM;
  rtx vec[MMIX_MAX_REGS_FOR_VALUE];
  int i;
  int nregs;

  /* Return values that fit in a register need no special handling.
     There's no register hole when parameters are passed in global
     registers.  */
  if (TARGET_ABI_GNU
      || GET_MODE_BITSIZE (mode) <= BITS_PER_WORD)
    return
      gen_rtx_REG (mode, MMIX_OUTGOING_RETURN_VALUE_REGNUM);

  /* A complex type, made up of components.  */
  cmode = TYPE_MODE (TREE_TYPE (valtype));
  nregs = ((GET_MODE_BITSIZE (mode) + BITS_PER_WORD - 1) / BITS_PER_WORD);

  /* We need to take care of the effect of the register hole on return
     values of large sizes; the last register will appear as the first
     register, with the rest shifted.  (For complex modes, this is just
     swapped registers.)  */

  if (nregs > MMIX_MAX_REGS_FOR_VALUE)
    internal_error ("too large function value type, needs %d registers,\
 have only %d registers for this", nregs, MMIX_MAX_REGS_FOR_VALUE);

  /* FIXME: Maybe we should handle structure values like this too
     (adjusted for BLKmode), perhaps for both ABI:s.  */
  for (i = 0; i < nregs - 1; i++)
    vec[i]
      = gen_rtx_EXPR_LIST (VOIDmode,
			   gen_rtx_REG (cmode, first_val_regnum + i),
			   GEN_INT ((i + 1) * BITS_PER_UNIT));

  vec[nregs - 1]
    = gen_rtx_EXPR_LIST (VOIDmode,
			 gen_rtx_REG (cmode, first_val_regnum + nregs - 1),
			 GEN_INT (0));

  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nregs, vec));
}

/* EH_RETURN_DATA_REGNO. */

int
mmix_eh_return_data_regno (n)
     int n ATTRIBUTE_UNUSED;
{
  if (n >= 0 && n < 4)
    return MMIX_EH_RETURN_DATA_REGNO_START + n;

  return INVALID_REGNUM;
}

/* EH_RETURN_STACKADJ_RTX. */

rtx
mmix_eh_return_stackadj_rtx ()
{
  return gen_rtx_REG (Pmode, MMIX_EH_RETURN_STACKADJ_REGNUM);
}

/* EH_RETURN_HANDLER_RTX.  */

rtx
mmix_eh_return_handler_rtx ()
{
  return
    gen_rtx_REG (Pmode, MMIX_INCOMING_RETURN_ADDRESS_REGNUM);
}

/* ASM_PREFERRED_EH_DATA_FORMAT. */

int
mmix_asm_preferred_eh_data_format (code, global)
     int code ATTRIBUTE_UNUSED;
     int global ATTRIBUTE_UNUSED;
{
  /* This is the default (was at 2001-07-20).  Revisit when needed.  */
  return DW_EH_PE_absptr;
}

/* Emit the function prologue.  For simplicity while the port is still
   in a flux, we do it as text rather than the now preferred RTL way,
   as (define_insn "function_prologue").

   FIXME: Translate to RTL and/or optimize some of the DWARF 2 stuff.  */

void
mmix_target_asm_function_prologue (stream, locals_size)
     FILE *stream;
     HOST_WIDE_INT locals_size;
{
  int regno;
  int stack_space_to_allocate
    = (current_function_outgoing_args_size
       + current_function_pretend_args_size
       + (int) locals_size + 7) & ~7;
  int offset = -8;
  int doing_dwarf = dwarf2out_do_frame ();
  long cfa_offset = 0;

  /* Guard our assumptions.  Very low priority FIXME.  */
  if (locals_size != (int) locals_size)
    error ("stack frame too big");

  /* Add room needed to save global non-register-stack registers.  */
  for (regno = 255;
       regno >= MMIX_FIRST_GLOBAL_REGNUM;
       regno--)
    /* Note that we assume that the frame-pointer-register is one of these
       registers, in which case we don't count it here.  */
    if ((((regno != MMIX_FRAME_POINTER_REGNUM || !frame_pointer_needed)
	  && regs_ever_live[regno] && !call_used_regs[regno]))
	|| IS_MMIX_EH_RETURN_DATA_REG (regno))
      stack_space_to_allocate += 8;

  /* If we do have a frame-pointer, add room for it.  */
  if (frame_pointer_needed)
    stack_space_to_allocate += 8;

  /* If we have a non-local label, we need to be able to unwind to it, so
     store the current register stack pointer.  Also store the return
     address if we do that.  */
  if (MMIX_CFUN_HAS_LANDING_PAD)
    stack_space_to_allocate += 16;
  else if (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
    /* If we do have a saved return-address slot, add room for it.  */
    stack_space_to_allocate += 8;

  /* Make sure we don't get an unaligned stack.  */
  if ((stack_space_to_allocate % 8) != 0)
    internal_error ("stack frame not a multiple of 8 bytes: %d",
		    stack_space_to_allocate);

  if (current_function_pretend_args_size)
    {
      int mmix_first_vararg_reg
	= (MMIX_FIRST_INCOMING_ARG_REGNUM
	   + (MMIX_MAX_ARGS_IN_REGS
	      - current_function_pretend_args_size / 8));

      for (regno
	     = MMIX_FIRST_INCOMING_ARG_REGNUM + MMIX_MAX_ARGS_IN_REGS - 1;
	   regno >= mmix_first_vararg_reg;
	   regno--)
	{
	  if (offset < 0)
	    {
	      int stack_chunk
		= stack_space_to_allocate > (256 - 8)
		? (256 - 8) : stack_space_to_allocate;

	      fprintf (stream, "\tSUBU %s,%s,%d\n",
		       reg_names[MMIX_STACK_POINTER_REGNUM],
		       reg_names[MMIX_STACK_POINTER_REGNUM],
		       stack_chunk);

	      if (doing_dwarf)
		{
		  /* Each call to dwarf2out_def_cfa overrides the previous
		     setting; they don't accumulate.  We must keep track
		     of the offset ourselves.  */
		  cfa_offset += stack_chunk;
		  if (!frame_pointer_needed)
		    dwarf2out_def_cfa ("", MMIX_STACK_POINTER_REGNUM,
				       cfa_offset);
		}
	      offset += stack_chunk;
	      stack_space_to_allocate -= stack_chunk;
	    }

	  fprintf (stream, "\tSTOU %s,%s,%d\n", reg_names[regno],
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   offset);

	  /* These registers aren't actually saved (as in "will be
	     restored"), so don't tell DWARF2 they're saved.  */

	  offset -= 8;
	}
    }

  /* Store the frame-pointer.  */

  if (frame_pointer_needed)
    {
      if (offset < 0)
	{
	  /* Get 8 less than otherwise, since we need to reach offset + 8.  */
	  int stack_chunk
	    = stack_space_to_allocate > (256 - 8 - 8)
	    ? (256 - 8 - 8) : stack_space_to_allocate;

	  fprintf (stream, "\tSUBU %s,%s,%d\n",
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   stack_chunk);
	  if (doing_dwarf)
	    cfa_offset += stack_chunk;
	  offset += stack_chunk;
	  stack_space_to_allocate -= stack_chunk;
	}

      fprintf (stream, "\tSTOU %s,%s,%d\n\tADDU %s,%s,%d\n",
	       reg_names[MMIX_FRAME_POINTER_REGNUM],
	       reg_names[MMIX_STACK_POINTER_REGNUM],
	       offset,
	       reg_names[MMIX_FRAME_POINTER_REGNUM],
	       reg_names[MMIX_STACK_POINTER_REGNUM],
	       offset + 8);
      if (doing_dwarf)
	{
	  /* If we're using the frame-pointer, then we just need this CFA
	     definition basing on that value (often equal to the CFA).
	     Further changes to the stack-pointer do not affect the
	     frame-pointer, so we conditionalize them below on
	     !frame_pointer_needed.  */
	  dwarf2out_def_cfa ("", MMIX_FRAME_POINTER_REGNUM,
			     -cfa_offset + offset + 8);

	  dwarf2out_reg_save ("", MMIX_FRAME_POINTER_REGNUM,
			      -cfa_offset + offset);
	}

      offset -= 8;
    }

  if (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
    {
      /* Store the return-address, if one is needed on the stack.  We
	 usually store it in a register when needed, but that doesn't work
	 with -fexceptions.  */

      if (offset < 0)
	{
	  /* Get 8 less than otherwise, since we need to reach offset + 8.  */
	  int stack_chunk
	    = stack_space_to_allocate > (256 - 8 - 8)
	    ? (256 - 8 - 8) : stack_space_to_allocate;

	  fprintf (stream, "\tSUBU %s,%s,%d\n",
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   stack_chunk);
	  if (doing_dwarf)
	    {
	      cfa_offset += stack_chunk;
	      if (!frame_pointer_needed)
		dwarf2out_def_cfa ("", MMIX_STACK_POINTER_REGNUM,
				   cfa_offset);
	    }
	  offset += stack_chunk;
	  stack_space_to_allocate -= stack_chunk;
	}

      fprintf (stream, "\tGET $255,rJ\n\tSTOU $255,%s,%d\n",
	       reg_names[MMIX_STACK_POINTER_REGNUM],
	       offset);
      if (doing_dwarf)
	dwarf2out_return_save ("", -cfa_offset + offset);
      offset -= 8;
    }
  else if (MMIX_CFUN_HAS_LANDING_PAD)
    offset -= 8;

  if (MMIX_CFUN_HAS_LANDING_PAD)
    {
      /* Store the register defining the numbering of local registers, so
	 we know how long to unwind the register stack.  */

      if (offset < 0)
	{
	  /* Get 8 less than otherwise, since we need to reach offset + 8.  */
	  int stack_chunk
	    = stack_space_to_allocate > (256 - 8 - 8)
	    ? (256 - 8 - 8) : stack_space_to_allocate;

	  fprintf (stream, "\tSUBU %s,%s,%d\n",
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   stack_chunk);
	  offset += stack_chunk;
	  stack_space_to_allocate -= stack_chunk;

	  if (doing_dwarf)
	    {
	      cfa_offset += stack_chunk;
	      if (!frame_pointer_needed)
		dwarf2out_def_cfa ("", MMIX_STACK_POINTER_REGNUM,
				 cfa_offset);
	    }
	}

      /* We don't tell dwarf2 about this one; we just have it to unwind
	 the register stack at landing pads.  FIXME: It's a kludge because
	 we can't describe the effect of the PUSHJ and PUSHGO insns on the
	 register stack at the moment.  Best thing would be to handle it
	 like stack-pointer offsets.  Better: some hook into dwarf2out.c
	 to produce DW_CFA_expression:s that specify the increment of rO,
	 and unwind it at eh_return (preferred) or at the landing pad.
	 Then saves to $0..$G-1 could be specified through that register.  */

      fprintf (stream, "\tGET $255,rO\n\tSTOU $255,%s,%d\n",
	       reg_names[MMIX_STACK_POINTER_REGNUM], offset);

      offset -= 8;
    }

  /* After the return-address and the frame-pointer, we have the local
     variables.  They're the ones that may have an "unaligned" size.  */
  offset -= (locals_size + 7) & ~7;

  /* Now store all registers that are global, i.e. not saved by the
     register file machinery.

     It is assumed that the frame-pointer is one of these registers, so it
     is explicitly excluded in the count.  */

  for (regno = 255;
       regno >= MMIX_FIRST_GLOBAL_REGNUM;
       regno--)
    if (((regno != MMIX_FRAME_POINTER_REGNUM || !frame_pointer_needed)
	 && regs_ever_live[regno] && ! call_used_regs[regno])
	|| IS_MMIX_EH_RETURN_DATA_REG (regno))
      {
	if (offset < 0)
	  {
	    int stack_chunk;

	    /* Since the local variables go above, we may get a large
	       offset here.  */
	    if (offset < -248)
	      {
		/* We're not going to access the locals area in the
		   prologue, so we'll just silently subtract the slab we
		   will not access.  */
		stack_chunk =
		  stack_space_to_allocate > (256 - offset - 8)
		  ? (256 - offset - 8) : stack_space_to_allocate;

		mmix_output_register_setting (stream, 255, stack_chunk, 1);
		fprintf (stream, "\tSUBU %s,%s,$255\n",
			 reg_names[MMIX_STACK_POINTER_REGNUM],
			 reg_names[MMIX_STACK_POINTER_REGNUM]);

		if (doing_dwarf)
		  {
		    cfa_offset += stack_chunk;
		    if (!frame_pointer_needed)
		      dwarf2out_def_cfa ("", MMIX_STACK_POINTER_REGNUM,
					 cfa_offset);
		  }
	      }
	    else
	      {
		stack_chunk = stack_space_to_allocate > (256 - 8)
		  ? (256 - 8) : stack_space_to_allocate;

		fprintf (stream, "\tSUBU %s,%s,%d\n",
			 reg_names[MMIX_STACK_POINTER_REGNUM],
			 reg_names[MMIX_STACK_POINTER_REGNUM], stack_chunk);
		if (doing_dwarf)
		  {
		    cfa_offset += stack_chunk;
		    if (!frame_pointer_needed)
		      dwarf2out_def_cfa ("", MMIX_STACK_POINTER_REGNUM,
					 cfa_offset);
		  }
	      }

	    offset += stack_chunk;
	    stack_space_to_allocate -= stack_chunk;
	  }

	fprintf (stream, "\tSTOU %s,%s,%d\n", reg_names[regno],
	       reg_names[MMIX_STACK_POINTER_REGNUM], offset);
	if (doing_dwarf)
	  dwarf2out_reg_save ("", regno, -cfa_offset + offset);
	offset -= 8;
      }

  /* Finally, allocate room for outgoing args and local vars if room
     wasn't allocated above.  This might be any number of bytes (well, we
     assume it fits in a host-int).  */
  if (stack_space_to_allocate)
    {
      if (stack_space_to_allocate < 256)
	{
	  fprintf (stream, "\tSUBU %s,%s,%d\n",
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   stack_space_to_allocate);
	}
      else
	{
	  mmix_output_register_setting (stream, 255,
					stack_space_to_allocate, 1);
	  fprintf (stream, "\tSUBU %s,%s,$255\n",
		   reg_names[MMIX_STACK_POINTER_REGNUM],
		   reg_names[MMIX_STACK_POINTER_REGNUM]);
	}

      if (doing_dwarf)
	{
	  cfa_offset += stack_space_to_allocate;
	  if (!frame_pointer_needed)
	    dwarf2out_def_cfa ("", MMIX_STACK_POINTER_REGNUM,
			       cfa_offset);
	}
    }

  /* We put the number of the highest saved register-file register in a
     location convenient for the call-patterns to output.  Note that we
     don't tell dwarf2 about these registers, since it can't restore them
     anyway.  */
  for (regno = MMIX_LAST_REGISTER_FILE_REGNUM;
       regno >= 0;
       regno--)
    if ((regs_ever_live[regno] && !call_used_regs[regno])
	|| (regno == MMIX_FRAME_POINTER_REGNUM && frame_pointer_needed))
      break;

  mmix_highest_saved_stack_register = regno;
}

/* TARGET_ASM_FUNCTION_EPILOGUE.  */

void
mmix_target_asm_function_epilogue (stream, locals_size)
     FILE *stream;
     HOST_WIDE_INT locals_size;

{
  int regno;
  int stack_space_to_deallocate
    = (current_function_outgoing_args_size
       + current_function_pretend_args_size
       + (int) locals_size + 7) & ~7;

  /* The assumption that locals_size fits in an int is asserted in
     mmix_target_asm_function_prologue.  */

  /* The first address to access is beyond the outgoing_args area.  */
  int offset = current_function_outgoing_args_size;

  /* Add the space for global non-register-stack registers.
     It is assumed that the frame-pointer register can be one of these
     registers, in which case it is excluded from the count when needed.  */
  for (regno = 255;
       regno >= MMIX_FIRST_GLOBAL_REGNUM;
       regno--)
    if (((regno != MMIX_FRAME_POINTER_REGNUM || !frame_pointer_needed)
	 && regs_ever_live[regno] && !call_used_regs[regno])
	|| IS_MMIX_EH_RETURN_DATA_REG (regno))
      stack_space_to_deallocate += 8;

  /* Add in the space for register stack-pointer.  If so, always add room
     for the saved PC.  */
  if (MMIX_CFUN_HAS_LANDING_PAD)
    stack_space_to_deallocate += 16;
  else if (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
    /* If we have a saved return-address slot, add it in.  */
    stack_space_to_deallocate += 8;

  /* Add in the frame-pointer.  */
  if (frame_pointer_needed)
    stack_space_to_deallocate += 8;

  /* Make sure we don't get an unaligned stack.  */
  if ((stack_space_to_deallocate % 8) != 0)
    internal_error ("stack frame not a multiple of octabyte: %d",
		    stack_space_to_deallocate);

  /* We will add back small offsets to the stack pointer as we go.
     First, we restore all registers that are global, i.e. not saved by
     the register file machinery.  */

  for (regno = MMIX_FIRST_GLOBAL_REGNUM;
       regno <= 255;
       regno++)
    if (((regno != MMIX_FRAME_POINTER_REGNUM || !frame_pointer_needed)
	 && regs_ever_live[regno] && !call_used_regs[regno])
	|| IS_MMIX_EH_RETURN_DATA_REG (regno))
      {
	if (offset > 255)
	  {
	    if (offset > 65535)
	      {
		/* There's better support for incrementing than
		   decrementing, so we might be able to optimize this as
		   we see a need.  */
		mmix_output_register_setting (stream, 255, offset, 1);
		fprintf (stream, "\tADDU %s,%s,$255\n",
			 reg_names[MMIX_STACK_POINTER_REGNUM],
			 reg_names[MMIX_STACK_POINTER_REGNUM]);
	      }
	    else
	      fprintf (stream, "\tINCL %s,%d\n",
		       reg_names[MMIX_STACK_POINTER_REGNUM], offset);

	    stack_space_to_deallocate -= offset;
	    offset = 0;
	  }

	fprintf (stream, "\tLDOU %s,%s,%d\n",
		 reg_names[regno],
		 reg_names[MMIX_STACK_POINTER_REGNUM],
		 offset);
	offset += 8;
      }

  /* Here is where the local variables were.  As in the prologue, they
     might be of an unaligned size.  */
  offset += (locals_size + 7) & ~7;


  /* The saved register stack pointer is just below the frame-pointer
     register.  We don't need to restore it "manually"; the POP
     instruction does that.  */
  if (MMIX_CFUN_HAS_LANDING_PAD)
    offset += 16;
  else if (MMIX_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
    /* The return-address slot is just below the frame-pointer register.
       We don't need to restore it because we don't really use it.  */
    offset += 8;

  /* Get back the old frame-pointer-value.  */
  if (frame_pointer_needed)
    {
      if (offset > 255)
	{
	  if (offset > 65535)
	    {
	      /* There's better support for incrementing than
		 decrementing, so we might be able to optimize this as
		 we see a need.  */
	      mmix_output_register_setting (stream, 255, offset, 1);
	      fprintf (stream, "\tADDU %s,%s,$255\n",
		       reg_names[MMIX_STACK_POINTER_REGNUM],
		       reg_names[MMIX_STACK_POINTER_REGNUM]);
	    }
	  else
	    fprintf (stream, "\tINCL %s,%d\n",
		     reg_names[MMIX_STACK_POINTER_REGNUM], offset);

	  stack_space_to_deallocate -= offset;
	  offset = 0;
	}

      fprintf (stream, "\tLDOU %s,%s,%d\n",
	       reg_names[MMIX_FRAME_POINTER_REGNUM],
	       reg_names[MMIX_STACK_POINTER_REGNUM],
	       offset);
      offset += 8;
    }

  /* We do not need to restore pretended incoming args, just add back
     offset to sp.  */
  if (stack_space_to_deallocate > 65535)
    {
      /* There's better support for incrementing than decrementing, so
	 we might be able to optimize this as we see a need.  */
      mmix_output_register_setting (stream, 255,
				    stack_space_to_deallocate, 1);
      fprintf (stream, "\tADDU %s,%s,$255\n",
	       reg_names[MMIX_STACK_POINTER_REGNUM],
	       reg_names[MMIX_STACK_POINTER_REGNUM]);
    }
  else if (stack_space_to_deallocate != 0)
    fprintf (stream, "\tINCL %s,%d\n",
	     reg_names[MMIX_STACK_POINTER_REGNUM],
	     stack_space_to_deallocate);

  if (current_function_calls_eh_return)
    /* Adjustment the (normal) stack-pointer to that of the receiver.
       FIXME: It would be nice if we could also adjust the register stack
       here, but we need to express it through DWARF 2 too.  */
    fprintf (stream, "\tADDU %s,%s,%s\n",
	     reg_names [MMIX_STACK_POINTER_REGNUM],
	     reg_names [MMIX_STACK_POINTER_REGNUM],
	     reg_names [MMIX_EH_RETURN_STACKADJ_REGNUM]);

  /* The extra \n is so we have a blank line between the assembly code of
     separate functions.  */
  fprintf (stream, "\tPOP %d,0\n\n",
	   (! TARGET_ABI_GNU
	    && current_function_return_rtx != NULL
	    && ! current_function_returns_struct)
	   ? (GET_CODE (current_function_return_rtx) == PARALLEL
	      ? GET_NUM_ELEM (XVEC (current_function_return_rtx, 0)) : 1)
	   : 0);
}

/* ASM_OUTPUT_MI_THUNK.  */

void
mmix_asm_output_mi_thunk (stream, fndecl, delta, func)
     FILE * stream;
     tree fndecl ATTRIBUTE_UNUSED;
     int delta;
     tree func;
{
  /* If you define STRUCT_VALUE to 0, rather than use STRUCT_VALUE_REGNUM,
     (i.e. pass location of structure to return as invisible first
     argument) you need to tweak this code too.  */
  const char *regname = reg_names[MMIX_FIRST_INCOMING_ARG_REGNUM];

  if (delta >= 0 && delta < 65536)
    asm_fprintf (stream, "\tINCL %s,%d\n", delta, regname);
  else if (delta < 0 && delta >= -255)
    asm_fprintf (stream, "\tSUBU %s,%s,%d\n", regname, regname, -delta);
  else
    {
      mmix_output_register_setting (stream, 255, delta, 1);
      asm_fprintf (stream, "\tADDU %s,%s,$255\n", regname, regname);
    }

  fprintf (stream, "\tJMP ");
  assemble_name (stream, XSTR (XEXP (DECL_RTL (func), 0), 0));
  fprintf (stream, "\n");
}

/* FUNCTION_PROFILER.  */

void
mmix_function_profiler (stream, labelno)
     FILE *stream ATTRIBUTE_UNUSED;
     int labelno ATTRIBUTE_UNUSED;
{
  sorry ("function_profiler support for MMIX");
}

/* SETUP_INCOMING_VARARGS.  */

void
mmix_setup_incoming_varargs (args_so_farp, mode, vartype, pretend_sizep,
			     second_time)
     CUMULATIVE_ARGS * args_so_farp;
     enum machine_mode mode;
     tree vartype;
     int * pretend_sizep;
     int second_time ATTRIBUTE_UNUSED;
{
  /* For stdarg, the last named variable has been handled, but
     args_so_farp has not been advanced for it.  For varargs, the current
     argument is to be counted to the anonymous ones.  */
  if (current_function_stdarg)
    {
      if (args_so_farp->regs + 1 < MMIX_MAX_ARGS_IN_REGS)
	*pretend_sizep
	  = (MMIX_MAX_ARGS_IN_REGS - (args_so_farp->regs + 1)) * 8;
    }
  else if (current_function_varargs)
    {
      if (args_so_farp->regs < MMIX_MAX_ARGS_IN_REGS)
	*pretend_sizep
	  = (MMIX_MAX_ARGS_IN_REGS - args_so_farp->regs) * 8;

      /* For varargs, we get here when we see the last named parameter,
	 which will actually be passed on stack.  So make the next call
	 (there will be one) to FUNCTION_ARG return 0, to count it on
	 stack, so va_arg for it will get right.  FIXME: The GCC core
	 should provide TRT.  */
      args_so_farp->now_varargs = 1;
    }
  else
    internal_error ("neither varargs or stdarg in mmix_setup_incoming_varargs");


  /* We assume that one argument takes up one register here.  That should
     be true until we start messing with multi-reg parameters.   */
  if ((7 + (MMIX_FUNCTION_ARG_SIZE (mode, vartype))) / 8 != 1)
    internal_error ("MMIX Internal: Last named vararg would not fit in a register");
}

/* EXPAND_BUILTIN_VA_ARG.  */

/* This is modified from the "standard" implementation of va_arg: read the
   value from the current (padded) address and increment by the (padded)
   size.  The difference for MMIX is that if the type is
   pass-by-reference, then perform an indirection.  */

rtx
mmix_expand_builtin_va_arg (valist, type)
     tree valist;
     tree type;
{
  tree ptr_size = size_int (BITS_PER_WORD / BITS_PER_UNIT);
  tree addr_tree, type_size = NULL;
  tree align, alignm1;
  tree rounded_size;
  rtx addr;

  /* Compute the rounded size of the type.  */

  /* Get AP.  */
  addr_tree = valist;
  align = size_int (PARM_BOUNDARY / BITS_PER_UNIT);
  alignm1 = size_int (PARM_BOUNDARY / BITS_PER_UNIT - 1);
  if (type == error_mark_node
      || (type_size = TYPE_SIZE_UNIT (TYPE_MAIN_VARIANT (type))) == NULL
      || TREE_OVERFLOW (type_size))
    /* Presumably an error; the size isn't computable.  A message has
       supposedly been emitted elsewhere.  */
    rounded_size = size_zero_node;
  else
    rounded_size = fold (build (MULT_EXPR, sizetype,
				fold (build (TRUNC_DIV_EXPR, sizetype,
					     fold (build (PLUS_EXPR, sizetype,
							  type_size, alignm1)),
					     align)),
				align));

 if (AGGREGATE_TYPE_P (type)
     && GET_MODE_UNIT_SIZE (TYPE_MODE (type)) < 8
     && GET_MODE_UNIT_SIZE (TYPE_MODE (type)) != 0)
   {
     /* Adjust for big-endian the location of aggregates passed in a
	register, but where the aggregate is accessed in a shorter mode
	than the natural register mode (i.e. it is accessed as SFmode(?),
	SImode, HImode or QImode rather than DImode or DFmode(?)).  FIXME:
	Or should we adjust the mode in which the aggregate is read, to be
	a register size mode?  (Hum, nah, a small offset is generally
	cheaper than a wider memory access on MMIX.)  */
     addr_tree
       = build (PLUS_EXPR, TREE_TYPE (addr_tree), addr_tree,
		size_int ((BITS_PER_WORD / BITS_PER_UNIT)
			  - GET_MODE_UNIT_SIZE (TYPE_MODE (type))));
   }
 else if (!integer_zerop (rounded_size))
   {
     if (!really_constant_p (type_size))
       /* Varying-size types come in by reference.  */
       addr_tree
	 = build1 (INDIRECT_REF, build_pointer_type (type), addr_tree);
     else
       {
	 /* If the size is less than a register, then we need to pad the
	    address by adding the difference.  */
	 tree addend
	   = fold (build (COND_EXPR, sizetype,
			  fold (build (GT_EXPR, sizetype,
				       rounded_size,
				       align)),
			  size_zero_node,
			  fold (build (MINUS_EXPR, sizetype,
				       rounded_size,
				       type_size))));
	 tree addr_tree1
	   = fold (build (PLUS_EXPR, TREE_TYPE (addr_tree), addr_tree,
			  addend));

	 /* If this type is larger than what fits in a register, then it
	    is passed by reference.  */
	 addr_tree
	   = fold (build (COND_EXPR, TREE_TYPE (addr_tree1),
			  fold (build (GT_EXPR, sizetype,
				       rounded_size,
				       ptr_size)),
			  build1 (INDIRECT_REF, build_pointer_type (type),
				  addr_tree1),
			  addr_tree1));
       }
   }

  addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
  addr = copy_to_reg (addr);

  if (!integer_zerop (rounded_size))
    {
      /* Compute new value for AP.  For MMIX, it is always advanced by the
	 size of a register.  */
      tree t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
		      build (PLUS_EXPR, TREE_TYPE (valist), valist,
			     ptr_size));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  return addr;
}

/* TRAMPOLINE_SIZE.  */
/* Four 4-byte insns plus two 8-byte values.  */
int mmix_trampoline_size = 32;


/* TRAMPOLINE_TEMPLATE.  */

void
mmix_trampoline_template (stream)
     FILE * stream;
{
  /* Read a value into the static-chain register and jump somewhere.  The
     static chain is stored at offset 16, and the function address is
     stored at offset 24.  */
  /* FIXME: GCC copies this using *intsize* (tetra), when it should use
     register size (octa).  */
  fprintf (stream, "\tGETA $255,1F\n\t");
  fprintf (stream, "LDOU %s,$255,0\n\t",
	   reg_names[MMIX_STATIC_CHAIN_REGNUM]);
  fprintf (stream, "LDOU $255,$255,8\n\t");
  fprintf (stream, "GO $255,$255,0\n");
  fprintf (stream, "1H\tOCTA 0\n\t");
  fprintf (stream, "OCTA 0\n");
}

/* INITIALIZE_TRAMPOLINE.  */
/* Set the static chain and function pointer field in the trampoline.
   We also SYNCID here to be sure (doesn't matter in the simulator, but
   some day it will).  */

void
mmix_initialize_trampoline (trampaddr, fnaddr, static_chain)
     rtx trampaddr;
     rtx fnaddr;
     rtx static_chain;
{
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant (trampaddr, 16)),
		  static_chain);
  emit_move_insn (gen_rtx_MEM (DImode,
			       plus_constant (trampaddr, 24)),
		  fnaddr);
  emit_insn (gen_sync_icache (validize_mem (gen_rtx_MEM (DImode,
							 trampaddr)),
			      GEN_INT (mmix_trampoline_size - 1)));
}

/* We must exclude constant addresses that have an increment that is not a
   multiple of four bytes because of restrictions of the GETA
   instruction, unless TARGET_BASE_ADDRESSES.  */

int
mmix_constant_address_p (x)
     rtx x;
{
  RTX_CODE code = GET_CODE (x);
  int addend = 0;
  /* When using "base addresses", anything constant goes.  */
  int constant_ok = TARGET_BASE_ADDRESSES != 0;

  if (code == LABEL_REF || code == SYMBOL_REF)
    return 1;

  if (code == CONSTANT_P_RTX || code == HIGH)
    /* FIXME: Don't know how to dissect these.  Avoid them for now.  */
    return constant_ok;

  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
      return 1;

    case CONSTANT_P_RTX:
    case HIGH:
      /* FIXME: Don't know how to dissect these.  Avoid them for now,
	 except we know they're constants.  */
      return constant_ok;

    case CONST_INT:
      addend = INTVAL (x);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) != VOIDmode)
	/* Strange that we got here.  FIXME: Check if we do.  */
	return constant_ok;
      addend = CONST_DOUBLE_LOW (x);
      break;

    case CONST:
      /* Note that expressions with arithmetic on forward references don't
	 work in mmixal.  People using gcc assembly code with mmixal might
	 need to move arrays and such to before the point of use.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  rtx x0 = XEXP (XEXP (x, 0), 0);
	  rtx x1 = XEXP (XEXP (x, 0), 1);

	  if ((GET_CODE (x0) == SYMBOL_REF
	       || GET_CODE (x0) == LABEL_REF)
	      && (GET_CODE (x1) == CONST_INT
		  || (GET_CODE (x1) == CONST_DOUBLE
		      && GET_MODE (x1) == VOIDmode)))
	    addend = mmix_intval (x1);
	  else
	    return constant_ok;
	}
      else
	return constant_ok;
      break;

    default:
      return 0;
    }

  return constant_ok || (addend & 3) == 0;
}

/* Return 1 if the address is OK, otherwise 0.
   Used by GO_IF_LEGITIMATE_ADDRESS.  */

int
mmix_legitimate_address (mode, x, strict_checking)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x;
     int strict_checking;
{
#define MMIX_REG_OK(X)							\
  ((strict_checking							\
    && (REGNO (X) <= MMIX_LAST_GENERAL_REGISTER				\
	|| (reg_renumber[REGNO (X)] > 0					\
	    && reg_renumber[REGNO (X)] <= MMIX_LAST_GENERAL_REGISTER)))	\
   || (!strict_checking							\
       && (REGNO (X) <= MMIX_LAST_GENERAL_REGISTER			\
	   || REGNO (X) >= FIRST_PSEUDO_REGISTER			\
	   || REGNO (X) == ARG_POINTER_REGNUM)))

  /* We only accept:
     (mem reg)
     (mem (plus reg reg))
     (mem (plus reg 0..255)).
     unless TARGET_BASE_ADDRESSES, in which case we accept all
     (mem constant_address) too.  */


    /* (mem reg) */
  if (REG_P (x) && MMIX_REG_OK (x))
    return 1;

  if (GET_CODE(x) == PLUS)
    {
      rtx x1 = XEXP (x, 0);
      rtx x2 = XEXP (x, 1);

      /* Try swapping the order.  FIXME: Do we need this?  */
      if (! REG_P (x1))
	{
	  rtx tem = x1;
	  x1 = x2;
	  x2 = tem;
	}

      /* (mem (plus (reg?) (?))) */
      if (!REG_P (x1) || !MMIX_REG_OK (x1))
	return TARGET_BASE_ADDRESSES && mmix_constant_address_p (x);

      /* (mem (plus (reg) (reg?))) */
      if (REG_P (x2) && MMIX_REG_OK (x2))
	return 1;

      /* (mem (plus (reg) (0..255?))) */
      if (GET_CODE (x2) == CONST_INT
	  && CONST_OK_FOR_LETTER_P (INTVAL (x2), 'I'))
	return 1;

      return 0;
    }

  return TARGET_BASE_ADDRESSES && mmix_constant_address_p (x);
}

/* LEGITIMATE_CONSTANT_P.  */

int
mmix_legitimate_constant_p (x)
     rtx x;
{
  RTX_CODE code = GET_CODE (x);

  /* We must allow any number due to the way the cse passes works; if we
     do not allow any number here, general_operand will fail, and insns
     will fatally fail recognition instead of "softly".  */
  if (code == CONST_INT || code == CONST_DOUBLE)
    return 1;

  return CONSTANT_ADDRESS_P (x);
}

/* SELECT_CC_MODE.  */

enum machine_mode
mmix_select_cc_mode (op, x, y)
     RTX_CODE op;
     rtx x;
     rtx y ATTRIBUTE_UNUSED;
{
  /* We use CCmode, CC_UNSmode, CC_FPmode, CC_FPEQmode and CC_FUNmode to
     output different compare insns.  Note that we do not check the
     validity of the comparison here.  */

  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      if (op == ORDERED || op == UNORDERED || op == UNGE
	  || op == UNGT || op == UNLE || op == UNLT)
	return CC_FUNmode;

      if (op == EQ || op == NE)
	return CC_FPEQmode;

      return CC_FPmode;
    }

  if (op == GTU || op == LTU || op == GEU || op == LEU)
    return CC_UNSmode;

  return CCmode;
}

/* CANONICALIZE_COMPARISON.
   FIXME: Check if the number adjustments trig.  */

void
mmix_canonicalize_comparison (codep, op0p, op1p)
     RTX_CODE * codep;
     rtx * op0p ATTRIBUTE_UNUSED;
     rtx * op1p;
{
  /* Change -1 to zero, if possible.  */
  if ((*codep == LE || *codep == GT)
      && GET_CODE (*op1p) == CONST_INT
      && *op1p == constm1_rtx)
    {
      *codep = *codep == LE ? LT : GE;
      *op1p = const0_rtx;
    }

  /* Fix up 256 to 255, if possible.  */
  if ((*codep == LT || *codep == LTU || *codep == GE || *codep == GEU)
      && GET_CODE (*op1p) == CONST_INT
      && INTVAL (*op1p) == 256)
    {
      /* FIXME: Remove when I know this trigs.  */
      fatal_insn ("oops, not debugged; fixing up value:", *op1p);
      *codep = *codep == LT ? LE : *codep == LTU ? LEU : *codep
	== GE ? GT : GTU;
      *op1p = GEN_INT (255);
    }
}

/* REVERSIBLE_CC_MODE.  */

int
mmix_reversible_cc_mode (mode)
     enum machine_mode mode;
{
  /* That is, all integer and the EQ, NE, ORDERED and UNORDERED float
     cmpares.  */
  return mode != CC_FPmode;
}

/* DEFAULT_RTX_COSTS.  */

int
mmix_rtx_cost_recalculated (x, code, outer_code, costp)
     rtx x ATTRIBUTE_UNUSED;
     RTX_CODE code ATTRIBUTE_UNUSED;
     RTX_CODE outer_code ATTRIBUTE_UNUSED;
     int *costp ATTRIBUTE_UNUSED;
{
  /* For the time being, this is just a stub and we'll accept the
     generic calculations, until we can do measurements, at least.
     Say we did not modify any calculated costs.  */
  return 0;
}

/* ADDRESS_COST.  */

int
mmix_address_cost (addr)
     rtx addr ATTRIBUTE_UNUSED;
{
  /* There's no difference in the address costs and we have lots of
     registers.  Some targets use constant 0, many others use 1 to say
     this.  Let's start with 1.  */
  return 1;
}

/* REGISTER_MOVE_COST.  */

int
mmix_register_move_cost (mode, from, to)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     enum reg_class from;
     enum reg_class to;
{
  return (from == GENERAL_REGS && from == to) ? 2 : 3;
}

/* Note that we don't have a TEXT_SECTION_ASM_OP, because it has to be a
   compile-time constant; it's used in an asm in crtstuff.c, compiled for
   the target.  */

/* DATA_SECTION_ASM_OP.  */

const char *
mmix_data_section_asm_op ()
{
  return "\t.data ! mmixal:= 8H LOC 9B";
}

/* SELECT_SECTION.
   The meat is from elfos.h, which we will eventually consider using.  */

void
mmix_select_section (decl, reloc, align)
     tree decl;
     int reloc;
     int align ATTRIBUTE_UNUSED;
{
  if (TREE_CODE (decl) == STRING_CST)
    {
      if (! flag_writable_strings)
	const_section ();
      else
	data_section ();
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      if ((flag_pic && reloc)
	  || !TREE_READONLY (decl) || TREE_SIDE_EFFECTS (decl)
	  || !DECL_INITIAL (decl)
	  || (DECL_INITIAL (decl) != error_mark_node
	      && !TREE_CONSTANT (DECL_INITIAL (decl))))
	data_section ();
      else
	const_section ();
    }
  else if (TREE_CODE (decl) == CONSTRUCTOR)
    {
      if ((flag_pic && reloc)
	  || !TREE_READONLY (decl) || TREE_SIDE_EFFECTS (decl)
	  || ! TREE_CONSTANT (decl))
	data_section ();
      else
	const_section ();
    }
  else
    const_section ();
}

/* ENCODE_SECTION_INFO.  */

void
mmix_encode_section_info (decl)
     tree decl;
{
  /* Test for an external declaration, and do nothing if it is one.  */
  if ((TREE_CODE (decl) == VAR_DECL
       && (DECL_EXTERNAL (decl) || TREE_PUBLIC (decl)))
      || (TREE_CODE (decl) == FUNCTION_DECL && TREE_PUBLIC (decl)))
    ;
  else if (DECL_P (decl))
    {
      /* For non-visible declarations, add a "@" prefix, which we skip
	 when the label is output.  If the label does not have this
	 prefix, a ":" is output if -mtoplevel-symbols.

	 Note that this does not work for data that is declared extern and
	 later defined as static.  If there's code in between, that code
	 will refer to the extern declaration, and vice versa.  This just
	 means that when -mtoplevel-symbols is in use, we can just handle
	 well-behaved ISO-compliant code.  */

      const char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
      int len = strlen (str);
      char *newstr;

      /* Why is the return type of ggc_alloc_string const?  */
      newstr = (char *) ggc_alloc_string ("", len + 1);

      strcpy (newstr + 1, str);
      *newstr = '@';
      XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;
    }

  /* Set SYMBOL_REF_FLAG for things that we want to access with GETA.  We
     may need different options to reach for different things with GETA.
     For now, functions and things we know or have been told are constant.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      || TREE_CONSTANT (decl)
      || (TREE_CODE (decl) == VAR_DECL
	  && TREE_READONLY (decl)
	  && !TREE_SIDE_EFFECTS (decl)
	  && (!DECL_INITIAL (decl)
	      || TREE_CONSTANT (DECL_INITIAL (decl)))))
    {
      rtx rtl = (TREE_CODE_CLASS (TREE_CODE (decl)) != 'd'
                 ? TREE_CST_RTL (decl) : DECL_RTL (decl));
      SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;
    }
}

/* STRIP_NAME_ENCODING.  */

const char *
mmix_strip_name_encoding (name)
     const char *name;
{
  for (; (*name == '@' || *name == '*'); name++)
    ;

  return name;
}

/* UNIQUE_SECTION.
   The meat is from elfos.h, which we should consider using.  */

void
mmix_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  int len;
  int sec;
  const char *name;
  char *string;
  const char *prefix;
  static const char *const prefixes[4][2] =
  {
    { ".text.",   ".gnu.linkonce.t." },
    { ".rodata.", ".gnu.linkonce.r." },
    { ".data.",   ".gnu.linkonce.d." },
    { ".bss.",    ".gnu.linkonce.b." }
  };

  if (TREE_CODE (decl) == FUNCTION_DECL)
    sec = 0;
  else if (DECL_INITIAL (decl) == 0
	   || DECL_INITIAL (decl) == error_mark_node)
    sec =  3;
  else if (DECL_READONLY_SECTION (decl, reloc))
    sec = 1;
  else
    sec = 2;

  name   = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  /* Strip off any encoding in name.  */
  STRIP_NAME_ENCODING (name, name);
  prefix = prefixes[sec][DECL_ONE_ONLY (decl)];
  len    = strlen (name) + strlen (prefix);
  string = alloca (len + 1);

  sprintf (string, "%s%s", prefix, name);

  DECL_SECTION_NAME (decl) = build_string (len, string);
}

/* ASM_FILE_START.  */

void
mmix_asm_file_start (stream)
     FILE * stream;
{
  /* We just emit a little comment for the time being.  FIXME: Perhaps add
     -mstandalone and some segment and prefix setup here.  */
  ASM_OUTPUT_SOURCE_FILENAME (stream, main_input_filename);

  fprintf (stream, "! mmixal:= 8H LOC Data_Section\n");

  /* Make sure each file starts with the text section. */
  text_section ();
}

/* ASM_FILE_END.  */

void
mmix_asm_file_end (stream)
     FILE * stream ATTRIBUTE_UNUSED;
{
  /* Make sure each file ends with the data section. */
  data_section ();
}

/* ASM_OUTPUT_SOURCE_FILENAME.  */

void
mmix_asm_output_source_filename (stream, name)
     FILE * stream;
     const char * name;
{
  fprintf (stream, "# 1 ");
  OUTPUT_QUOTED_STRING (stream, name);
  fprintf (stream, "\n");
}

/* OUTPUT_QUOTED_STRING.  */

void
mmix_output_quoted_string (stream, string, length)
     FILE * stream;
     const char * string;
     int length;
{
  const char * string_end = string + length;
  static const char *const unwanted_chars = "\"[]\\";

  /* Output "any character except newline and double quote character".  We
     play it safe and avoid all control characters too.  We also do not
     want [] as characters, should input be passed through m4 with [] as
     quotes.  Further, we avoid "\", because the GAS port handles it as a
     quoting character.  */
  while (string < string_end)
    {
      if (*string
	  && (unsigned char) *string < 128
	  && !ISCNTRL (*string)
	  && strchr (unwanted_chars, *string) == NULL)
	{
	  fputc ('"', stream);
	  while (*string
		 && (unsigned char) *string < 128
		 && !ISCNTRL (*string)
		 && strchr (unwanted_chars, *string) == NULL
		 && string < string_end)
	    {
	      fputc (*string, stream);
	      string++;
	    }
	  fputc ('"', stream);
	  if (string < string_end)
	    fprintf (stream, ",");
	}
      if (string < string_end)
	{
	  fprintf (stream, "#%x", *string & 255);
	  string++;
	  if (string < string_end)
	    fprintf (stream, ",");
	}
    }
}

/* ASM_OUTPUT_SOURCE_LINE.  */

void
mmix_asm_output_source_line  (stream, lineno)
     FILE * stream;
     int lineno;
{
  fprintf (stream, "# %d ", lineno);
  OUTPUT_QUOTED_STRING (stream, main_input_filename);
  fprintf (stream, "\n");
}

/* Target hook for assembling integer objects.  Use mmix_print_operand
   for WYDE and TETRA.  Use mmix_output_octa to output 8-byte
   CONST_DOUBLEs.  */

static bool
mmix_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (aligned_p)
    switch (size)
      {
	/* We handle a limited number of types of operands in here.  But
	   that's ok, because we can punt to generic functions.  We then
	   pretend that aligned data isn't needed, so the usual .<pseudo>
	   syntax is used (which works for aligned data too).  We actually
	   *must* do that, since we say we don't have simple aligned
	   pseudos, causing this function to be called.  We just try and
	   keep as much compatibility as possible with mmixal syntax for
	   normal cases (i.e. without GNU extensions and C only).  */
      case 1:
	if (GET_CODE (x) != CONST_INT)
	  {
	    aligned_p = 0;
	    break;
	  }
	fputs ("\tBYTE\t", asm_out_file);
	mmix_print_operand (asm_out_file, x, 'B');
	fputc ('\n', asm_out_file);
	return true;

      case 2:
	if (GET_CODE (x) != CONST_INT)
	  {
	    aligned_p = 0;
	    break;
	  }
	fputs ("\tWYDE\t", asm_out_file);
	mmix_print_operand (asm_out_file, x, 'W');
	fputc ('\n', asm_out_file);
	return true;

      case 4:
	if (GET_CODE (x) != CONST_INT)
	  {
	    aligned_p = 0;
	    break;
	  }
	fputs ("\tTETRA\t", asm_out_file);
	mmix_print_operand (asm_out_file, x, 'L');
	fputc ('\n', asm_out_file);
	return true;

      case 8:
	if (GET_CODE (x) == CONST_DOUBLE)
	  /* We don't get here anymore for CONST_DOUBLE, because DImode
	     isn't expressed as CONST_DOUBLE, and DFmode is handled
	     elsewhere.  */
	  abort ();
	assemble_integer_with_op ("\tOCTA\t", x);
	return true;
      }
  return default_assemble_integer (x, size, aligned_p);
}

/* ASM_OUTPUT_ASCII.  */

void
mmix_asm_output_ascii (stream, string, length)
     FILE *stream;
     const char *string;
     int length;
{
  while (length > 0)
    {
      int chunk_size = length > 60 ? 60 : length;
      fprintf (stream, "\tBYTE ");
      mmix_output_quoted_string (stream, string, chunk_size);
      string += chunk_size;
      length -= chunk_size;
      fprintf (stream, "\n");
    }
}

/* ASM_OUTPUT_ALIGNED_COMMON.  */

void
mmix_asm_output_aligned_common (stream, name, size, align)
     FILE *stream;
     const char *name;
     int size;
     int align;
{
  /* This is mostly the elfos.h one.  There doesn't seem to be a way to
     express this in a mmixal-compatible way.  */
  fprintf (stream, "\t.comm\t");
  assemble_name (stream, name);
  fprintf (stream, ",%u,%u ! mmixal-incompatible COMMON\n",
	   size, align / BITS_PER_UNIT);
}

/* ASM_OUTPUT_ALIGNED_LOCAL.  */

void
mmix_asm_output_aligned_local (stream, name, size, align)
     FILE * stream;
     const char * name;
     int size;
     int align;
{
  data_section ();

  ASM_OUTPUT_ALIGN (stream, exact_log2 (align/BITS_PER_UNIT));
  assemble_name (stream, name);
  fprintf (stream, "\tLOC @+%d\n", size);
}

/* ASM_OUTPUT_LABEL.  */

void
mmix_asm_output_label (stream, name)
     FILE *stream;
     const char * name;
{
  assemble_name (stream, name);
  fprintf (stream, "\tIS @\n");
}

/* ASM_DECLARE_REGISTER_GLOBAL.  */

void
mmix_asm_declare_register_global (stream, decl, regno, name)
     FILE *stream ATTRIBUTE_UNUSED;
     tree decl ATTRIBUTE_UNUSED;
     int regno ATTRIBUTE_UNUSED;
     const char *name ATTRIBUTE_UNUSED;
{
  /* Nothing to do here, but there *will* be, therefore the framework is
     here.  */
}

/* ASM_GLOBALIZE_LABEL.  */

void
mmix_asm_globalize_label (stream, name)
     FILE * stream ATTRIBUTE_UNUSED;
     const char * name ATTRIBUTE_UNUSED;
{
  asm_fprintf (stream, "\t.global ");
  assemble_name (stream, name);
  putc ('\n', stream);
}

/* ASM_WEAKEN_LABEL.  */

void
mmix_asm_weaken_label (stream, name)
     FILE * stream ATTRIBUTE_UNUSED;
     const char * name ATTRIBUTE_UNUSED;
{
  asm_fprintf (stream, "\t.weak ");
  assemble_name (stream, name);
  asm_fprintf (stream, " ! mmixal-incompatible\n");
}

/* MAKE_DECL_ONE_ONLY.  */

void
mmix_make_decl_one_only (decl)
     tree decl;
{
  DECL_WEAK (decl) = 1;
}

/* ASM_OUTPUT_LABELREF.
   Strip GCC's '*' and our own '@'.  No order is assumed.  */

void
mmix_asm_output_labelref (stream, name)
     FILE *stream;
     const char *name;
{
  int is_extern = 1;

  for (; (*name == '@' || *name == '*'); name++)
    if (*name == '@')
      is_extern = 0;

  asm_fprintf (stream, "%s%U%s",
	       is_extern && TARGET_TOPLEVEL_SYMBOLS ? ":" : "",
	       name);
}

/* ASM_OUTPUT_INTERNAL_LABEL.  */

void
mmix_asm_output_internal_label (stream, name, num)
     FILE * stream;
     const char * name;
     int num;
{
  fprintf (stream, "%s:%d\tIS @\n", name, num);
}

/* ASM_OUTPUT_DEF.  */

void
mmix_asm_output_def (stream, name, value)
     FILE * stream;
     const char * name;
     const char * value;
{
  assemble_name (stream, name);
  fprintf (stream, "\tIS ");
  assemble_name (stream, value);
  fputc ('\n', stream);
}

/* ASM_OUTPUT_DEFINE_LABEL_DIFFERENCE_SYMBOL.  */

void
mmix_asm_output_define_label_difference_symbol (stream, symbol, hi, lo)
     FILE *stream;
     const char *symbol;
     const char *hi;
     const char *lo;
{
  assemble_name (stream, symbol);
  fprintf (stream, "\tIS\t");
  assemble_name (stream, hi);
  fputc ('-', stream);
  assemble_name (stream, lo);
  fprintf (stream, "\n");
}

/* PRINT_OPERAND.  */

void
mmix_print_operand (stream, x, code)
     FILE * stream;
     rtx x;
     int code;
{
  /* When we add support for different codes later, we can, when needed,
     drop through to the main handler with a modified operand.  */
  rtx modified_x = x;

  switch (code)
    {
      /* Unrelated codes are in alphabetic order.  */

    case '+':
      /* For conditional branches, output "P" for a probable branch.  */
      if (TARGET_BRANCH_PREDICT)
	{
	  x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	  if (x && INTVAL (XEXP (x, 0)) > REG_BR_PROB_BASE / 2)
	    putc ('P', stream);
	}
      return;

    case 'B':
      if (GET_CODE (x) != CONST_INT)
	fatal_insn ("MMIX Internal: Expected a CONST_INT, not this", x);
      fprintf (stream, "%d", (int) (INTVAL (x) & 0xff));
      return;

    case 'H':
      /* Highpart.  Must be general register, and not the last one, as
	 that one cannot be part of a consecutive register pair.  */
      if (REGNO (x) > MMIX_LAST_GENERAL_REGISTER - 1)
	internal_error ("MMIX Internal: Bad register: %d", REGNO (x));

      /* This is big-endian, so the high-part is the first one.  */
      fprintf (stream, "%s", reg_names[REGNO (x)]);
      return;

    case 'L':
      /* Lowpart.  Must be CONST_INT or general register, and not the last
	 one, as that one cannot be part of a consecutive register pair.  */
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (stream, "#%lx",
		   (unsigned long) (INTVAL (x)
				    & ((unsigned int) 0x7fffffff * 2 + 1)));
	  return;
	}

      if (GET_CODE (x) == SYMBOL_REF)
	{
	  output_addr_const (stream, x);
	  return;
	}

      if (REGNO (x) > MMIX_LAST_GENERAL_REGISTER - 1)
	internal_error ("MMIX Internal: Bad register: %d", REGNO (x));

      /* This is big-endian, so the low-part is + 1.  */
      fprintf (stream, "%s", reg_names[REGNO (x) + 1]);
      return;

      /* Can't use 'a' because that's a generic modifier for address
	 output.  */
    case 'A':
      mmix_output_shiftvalue_op_from_str (stream, "ANDN",
					  ~(unsigned HOST_WIDEST_INT)
					  mmix_intval (x));
      return;

    case 'i':
      mmix_output_shiftvalue_op_from_str (stream, "INC",
					  (unsigned HOST_WIDEST_INT)
					  mmix_intval (x));
      return;

    case 'o':
      mmix_output_shiftvalue_op_from_str (stream, "OR",
					  (unsigned HOST_WIDEST_INT)
					  mmix_intval (x));
      return;

    case 's':
      mmix_output_shiftvalue_op_from_str (stream, "SET",
					  (unsigned HOST_WIDEST_INT)
					  mmix_intval (x));
      return;

    case 'd':
    case 'D':
      mmix_output_condition (stream, x, (code == 'D'));
      return;

    case 'e':
      /* Output an extra "e" to make fcmpe, fune.  */
      if (TARGET_FCMP_EPSILON)
	fprintf (stream, "e");
      return;

    case 'm':
      /* Output the number minus 1.  */
      if (GET_CODE (x) != CONST_INT)
	{
	  fatal_insn ("MMIX Internal: Bad value for 'm', not a CONST_INT",
		      x);
	}
      fprintf (stream, HOST_WIDEST_INT_PRINT_DEC,
	       (HOST_WIDEST_INT) (mmix_intval (x) - 1));
      return;

    case 'p':
      /* Store the number of registers we want to save.  This was setup
	 by the prologue.  The actual operand contains the number of
	 registers to pass, but we don't use it currently.  Anyway, we
	 need to output the number of saved registers here.  */
      if (TARGET_ABI_GNU)
	fprintf (stream, "%d", mmix_highest_saved_stack_register + 1);
      else
	/* FIXME: Get the effect of renaming $16, $17.. to the first
	   unused call-saved reg.  */
	fprintf (stream, "15");
      return;

    case 'r':
      /* Store the register to output a constant to.  */
      if (! REG_P (x))
	fatal_insn ("MMIX Internal: Expected a register, not this", x);
      mmix_output_destination_register = REGNO (x);
      return;

    case 'I':
      /* Output the constant.  Note that we use this for floats as well.  */
      if (GET_CODE (x) != CONST_INT
	  && (GET_CODE (x) != CONST_DOUBLE
	      || (GET_MODE (x) != VOIDmode && GET_MODE (x) != DFmode
		  && GET_MODE (x) != SFmode)))
	fatal_insn ("MMIX Internal: Expected a constant, not this", x);
      mmix_output_register_setting (stream,
				    mmix_output_destination_register,
				    mmix_intval (x), 0);
      return;

    case 'U':
      /* An U for unsigned, if TARGET_ZERO_EXTEND.  Ignore the operand.  */
      if (TARGET_ZERO_EXTEND)
	putc ('U', stream);
      return;

    case 'v':
      mmix_output_shifted_value (stream, (HOST_WIDEST_INT) mmix_intval (x));
      return;

    case 'V':
      mmix_output_shifted_value (stream, (HOST_WIDEST_INT) ~mmix_intval (x));
      return;

    case 'W':
      if (GET_CODE (x) != CONST_INT)
	fatal_insn ("MMIX Internal: Expected a CONST_INT, not this", x);
      fprintf (stream, "#%x", (int) (INTVAL (x) & 0xffff));
      return;

    case 0:
      /* Nothing to do.  */
      break;

    default:
      /* Presumably there's a missing case above if we get here.  */
      internal_error ("MMIX Internal: Missing `%c' case in mmix_print_operand", code);
    }

  switch (GET_CODE (modified_x))
    {
    case REG:
      if (REGNO (modified_x) >= FIRST_PSEUDO_REGISTER)
	internal_error ("MMIX Internal: Bad register: %d", REGNO (modified_x));
      fprintf (stream, "%s", reg_names[REGNO (modified_x)]);
      return;

    case MEM:
      output_address (XEXP (modified_x, 0));
      return;

    case CONST_INT:
      /* For -2147483648, mmixal complains that the constant does not fit
	 in 4 bytes, so let's output it as hex.  Take care to handle hosts
	 where HOST_WIDE_INT is longer than an int.

	 Print small constants +-255 using decimal.  */

      if (INTVAL (modified_x) > -256 && INTVAL (modified_x) < 256)
	fprintf (stream, "%d", (int) (INTVAL (modified_x)));
      else
	fprintf (stream, "#%x",
		 (int) (INTVAL (modified_x)) & (unsigned int) ~0);
      return;

    case CONST_DOUBLE:
      /* Do somewhat as CONST_INT.  */
      mmix_output_octa (stream, mmix_intval (modified_x), 0);
      return;

    case CONST:
      output_addr_const (stream, modified_x);
      return;

    default:
      /* No need to test for all strange things.  Let output_addr_const do
	 it for us.  */
      if (CONSTANT_P (modified_x)
	  /* Strangely enough, this is not included in CONSTANT_P.
	     FIXME: Ask/check about sanity here.  */
	  || GET_CODE (modified_x) == CODE_LABEL)
	{
	  output_addr_const (stream, modified_x);
	  return;
	}

      /* We need the original here.  */
      fatal_insn ("MMIX Internal: Cannot decode this operand", x);
    }
}

/* PRINT_OPERAND_PUNCT_VALID_P.  */

int
mmix_print_operand_punct_valid_p (code)
     int code ATTRIBUTE_UNUSED;
{
  /* A '+' is used for branch prediction, similar to other ports.  */
  return code == '+';
}

/* PRINT_OPERAND_ADDRESS.  */

void
mmix_print_operand_address (stream, x)
     FILE *stream;
     rtx x;
{
  if (REG_P (x))
    {
      /* I find the generated assembly code harder to read without
	 the ",0".  */
      fprintf (stream, "%s,0",reg_names[REGNO (x)]);
      return;
    }
  else if (GET_CODE (x) == PLUS)
    {
      rtx x1 = XEXP (x, 0);
      rtx x2 = XEXP (x, 1);

      /* Try swap the order.  FIXME: Do we need this?  */
      if (! REG_P (x1))
	{
	  rtx tem = x1;
	  x1 = x2;
	  x2 = tem;
	}

      if (REG_P (x1))
	{
	  fprintf (stream, "%s,", reg_names[REGNO (x1)]);

	  if (REG_P (x2))
	    {
	      fprintf (stream, "%s", reg_names[REGNO (x2)]);
	      return;
	    }
	  else if (GET_CODE (x2) == CONST_INT
		   && CONST_OK_FOR_LETTER_P (INTVAL (x2), 'I'))
	    {
	      output_addr_const (stream, x2);
	      return;
	    }
	}
    }

  if (TARGET_BASE_ADDRESSES && mmix_legitimate_constant_p (x))
    {
      output_addr_const (stream, x);
      return;
    }

  fatal_insn ("MMIX Internal: This is not a recognized address", x);
}

/* ASM_OUTPUT_REG_PUSH.  */

void
mmix_asm_output_reg_push (stream, regno)
     FILE * stream;
     int regno;
{
  fprintf (stream, "\tSUBU %s,%s,8\n\tSTOU %s,%s,0\n",
	   reg_names[MMIX_STACK_POINTER_REGNUM],
	   reg_names[MMIX_STACK_POINTER_REGNUM],
	   reg_names[regno],
	   reg_names[MMIX_STACK_POINTER_REGNUM]);
}

/* ASM_OUTPUT_REG_POP.  */

void
mmix_asm_output_reg_pop (stream, regno)
     FILE * stream;
     int regno;
{
  fprintf (stream, "\tLDOU %s,%s,0\n\tINCL %s,8\n",
	   reg_names[regno],
	   reg_names[MMIX_STACK_POINTER_REGNUM],
	   reg_names[MMIX_STACK_POINTER_REGNUM]);
}

/* ASM_OUTPUT_ADDR_DIFF_ELT.  */

void
mmix_asm_output_addr_diff_elt (stream, body, value, rel)
     FILE *stream;
     rtx body ATTRIBUTE_UNUSED;
     int value;
     int rel;
{
  fprintf (stream, "\tTETRA L%d-L%d\n", value, rel);
}

/* ASM_OUTPUT_ADDR_VEC_ELT.  */

void
mmix_asm_output_addr_vec_elt (stream, value)
     FILE *stream;
     int value;
{
  fprintf (stream, "\tOCTA L:%d\n", value);
}

/* ASM_OUTPUT_SKIP.  */

void
mmix_asm_output_skip (stream, nbytes)
     FILE *stream;
     int nbytes;
{
  fprintf (stream, "\tLOC @+%d\n", nbytes);
}

/* ASM_OUTPUT_ALIGN.  */

void
mmix_asm_output_align (stream, power)
     FILE *stream;
     int power;
{
  /* We need to record the needed alignment of this section in the object,
     so we have to output an alignment directive.  Use a .p2align (not
     .align) so people will never have to wonder about whether the
     argument is in number of bytes or the log2 thereof.  We do it in
     addition to the LOC directive, so nothing needs tweaking when
     copy-pasting assembly into mmixal.  */
 fprintf (stream, "\t.p2align %d\n", power);
 fprintf (stream, "\tLOC @+(%d-@)&%d\n", 1 << power, (1 << power) - 1);
}

/* DBX_REGISTER_NUMBER.  */

int
mmix_dbx_register_number (regno)
     int regno;
{
  /* FIXME: Implement final register renumbering if necessary.  (Use
     target state in cfun).  */

  /* We need to renumber registers to get the number of the return address
     register in the range 0..255.  It is also space-saving if registers
     mentioned in the call-frame information (which uses this function by
     defaulting DWARF_FRAME_REGNUM to DBX_REGISTER_NUMBER) are numbered
     0 .. 63.  So map 224 .. 256+15 -> 0 .. 47 and 0 .. 223 -> 48..223+48.  */
  return regno >= 224 ? (regno - 224) : (regno + 48);
}

/* End of target macro support functions.

   Now MMIX's own functions.  First the exported ones.  */

/* Output an optimal sequence for setting a register to a specific
   constant.  Used in an alternative for const_ints in movdi, and when
   using large stack-frame offsets.

   Use do_begin_end to say if a line-starting TAB and newline before the
   first insn and after the last insn is wanted.  */

void
mmix_output_register_setting (stream, regno, value, do_begin_end)
     FILE *stream;
     int regno;
     HOST_WIDEST_INT value;
     int do_begin_end;
{
  if (do_begin_end)
    fprintf (stream, "\t");

  if (mmix_shiftable_wyde_value ((unsigned HOST_WIDEST_INT) value))
    {
      /* First, the one-insn cases.  */
      mmix_output_shiftvalue_op_from_str (stream, "SET",
					  (unsigned HOST_WIDEST_INT)
					  value);
      fprintf (stream, " %s,", reg_names[regno]);
      mmix_output_shifted_value (stream, (unsigned HOST_WIDEST_INT) value);
    }
  else if (mmix_shiftable_wyde_value (-(unsigned HOST_WIDEST_INT) value))
    {
      /* We do this to get a bit more legible assembly code.  The next
	 alternative is mostly redundant with this.  */

      mmix_output_shiftvalue_op_from_str (stream, "SET",
					  -(unsigned HOST_WIDEST_INT)
					  value);
      fprintf (stream, " %s,", reg_names[regno]);
      mmix_output_shifted_value (stream, -(unsigned HOST_WIDEST_INT) value);
      fprintf (stream, "\n\tNEGU %s,0,%s", reg_names[regno],
	       reg_names[regno]);
    }
  else if (mmix_shiftable_wyde_value (~(unsigned HOST_WIDEST_INT) value))
    {
      /* Slightly more expensive, the two-insn cases.  */

      /* FIXME: We could of course also test if 0..255-N or ~(N | 1..255)
	 is shiftable, or any other one-insn transformation of the value.
	 FIXME: Check first if the value is "shiftable" by two loading
	 with two insns, since it makes more readable assembly code (if
	 anyone else cares).  */

      mmix_output_shiftvalue_op_from_str (stream, "SET",
					  ~(unsigned HOST_WIDEST_INT)
					  value);
      fprintf (stream, " %s,", reg_names[regno]);
      mmix_output_shifted_value (stream, ~(unsigned HOST_WIDEST_INT) value);
      fprintf (stream, "\n\tNOR %s,%s,0", reg_names[regno],
	       reg_names[regno]);
    }
  else
    {
      /* The generic case.  2..4 insns.  */
      static const char *const higher_parts[] = {"L", "ML", "MH", "H"};
      const char *op = "SET";
      const char *line_begin = "";
      int insns = 0;
      int i;
      HOST_WIDEST_INT tmpvalue = value;

      /* Compute the number of insns needed to output this constant.  */
      for (i = 0; i < 4 && tmpvalue != 0; i++)
	{
	  if (tmpvalue & 65535)
	    insns++;
	  tmpvalue >>= 16;
	}
      if (TARGET_BASE_ADDRESSES && insns == 3)
	{
	  /* The number three is based on a static observation on
	     ghostscript-6.52.  Two and four are excluded because there
	     are too many such constants, and each unique constant (maybe
	     offset by 1..255) were used few times compared to other uses,
	     e.g. addresses.

	     We use base-plus-offset addressing to force it into a global
	     register; we just use a "LDA reg,VALUE", which will cause the
	     assembler and linker to DTRT (for constants as well as
	     addresses).  */
	  fprintf (stream, "LDA %s,", reg_names[regno]);
	  mmix_output_octa (stream, value, 0);
	}
      else
	{
	  /* Output pertinent parts of the 4-wyde sequence.
	     Still more to do if we want this to be optimal, but hey...
	     Note that the zero case has been handled above.  */
	  for (i = 0; i < 4 && value != 0; i++)
	    {
	      if (value & 65535)
		{
		  fprintf (stream, "%s%s%s %s,#%x", line_begin, op,
			   higher_parts[i], reg_names[regno],
			   (int) (value & 65535));
		  /* The first one sets the rest of the bits to 0, the next
		     ones add set bits.  */
		  op = "INC";
		  line_begin = "\n\t";
		}

	      value >>= 16;
	    }
	}
    }

  if (do_begin_end)
    fprintf (stream, "\n");
}

/* Return 1 if value is 0..65535*2**(16*N) for N=0..3.
   else return 0.  */

int
mmix_shiftable_wyde_value (value)
     unsigned HOST_WIDEST_INT value;
{
  /* Shift by 16 bits per group, stop when we've found two groups with
     nonzero bits.  */
  int i;
  int has_candidate = 0;

  for (i = 0; i < 4; i++)
    {
      if (value & 65535)
	{
	  if (has_candidate)
	    return 0;
	  else
	    has_candidate = 1;
	}

      value >>= 16;
    }

  return 1;
}

/* True if this is an address_operand or a symbolic operand.  */

int
mmix_symbolic_or_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      if ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	   || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	  && (GET_CODE (XEXP (op, 1)) == CONST_INT
	      || (GET_CODE (XEXP (op, 1)) == CONST_DOUBLE
		  && GET_MODE (XEXP (op, 1)) == VOIDmode)))
	return 1;
      /* FALLTHROUGH */
    default:
      return address_operand (op, mode);
    }
}

/* True if this is a register or CONST_INT (or CONST_DOUBLE for DImode).
   We could narrow the value down with a couple of predicated, but that
   doesn't seem to be worth it at the moment.  */

int
mmix_reg_or_constant_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode)
    || (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == VOIDmode)
    || GET_CODE (op) == CONST_INT;
}

/* True if this is a register with a condition-code mode.  */

int
mmix_reg_cc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  return register_operand (op, mode)
    && (mode == CCmode || mode == CC_UNSmode || mode == CC_FPmode
	|| mode == CC_FPEQmode || mode == CC_FUNmode);
}

/* True if this is a foldable comparison operator
   - one where a the result of (compare:CC (reg) (const_int 0)) can be
   replaced by (reg).  */

int
mmix_foldable_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  RTX_CODE code = GET_CODE (op);

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  if (mode == VOIDmode && GET_RTX_CLASS (GET_CODE (op)) == '<')
    mode = GET_MODE (XEXP (op, 0));

  return ((mode == CCmode || mode == DImode)
	  && (code == NE || code == EQ || code == GE || code == GT
	      || code == LE))
    /* FIXME: This may be a stupid trick.  What happens when GCC wants to
       reverse the condition?  Can it do that by itself?  Maybe it can
       even reverse the condition to fit a foldable one in the first
       place?  */
    || (mode == CC_UNSmode && (code == GTU || code == LEU));
}

/* Like comparison_operator, but only true if this comparison operator is
   applied to a valid mode.  Needed to avoid jump.c generating invalid
   code with -ffast-math (gcc.dg/20001228-1.c).  */

int
mmix_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  RTX_CODE code = GET_CODE (op);

  /* Comparison operators usually don't have a mode, but let's try and get
     one anyway for the day that changes.  */
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* Get the mode from the first operand if we don't have one.  */
  if (mode == VOIDmode && GET_RTX_CLASS (GET_CODE (op)) == '<')
    mode = GET_MODE (XEXP (op, 0));

  /* FIXME: This needs to be kept in sync with the tables in
     mmix_output_condition.  */
  return
    (mode == VOIDmode && GET_RTX_CLASS (GET_CODE (op)) == '<')
    || (mode == CC_FUNmode
	&& (code == ORDERED || code == UNORDERED))
    || (mode == CC_FPmode
	&& (code == GT || code == LT))
    || (mode == CC_FPEQmode
	&& (code == NE || code == EQ))
    || (mode == CC_UNSmode
	&& (code == GEU || code == GTU || code == LEU || code == LTU))
    || (mode == CCmode
	&& (code == NE || code == EQ || code == GE || code == GT
	    || code == LE || code == LT))
    || (mode == DImode
	&& (code == NE || code == EQ || code == GE || code == GT
	    || code == LE || code == LT || code == LEU || code == GTU));
}

/* True if this is a register or 0 (int or float).  */

int
mmix_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* FIXME: Is mode calculation necessary and correct?  */
  return
    op == CONST0_RTX (mode == VOIDmode ? GET_MODE (op) : mode)
    || register_operand (op, mode);
}

/* True if this is a register or an int 0..255.  */

int
mmix_reg_or_8bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode)
    || (GET_CODE (op) == CONST_INT
	&& CONST_OK_FOR_LETTER_P (INTVAL (op), 'I'));
}

/* True if this is a register or an int 0..256.  We include 256,
   because it can be canonicalized into 255 for comparisons, which is
   currently the only use of this predicate.
   FIXME:  Check that this happens and does TRT.  */

int
mmix_reg_or_8bit_or_256_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return mmix_reg_or_8bit_operand (op, mode)
    || (GET_CODE (op) == CONST_INT && INTVAL (op) == 256);
}

/* Returns zero if code and mode is not a valid condition from a
   compare-type insn.  Nonzero if it is.  The parameter op, if non-NULL,
   is the comparison of mode is CC-somethingmode.  */

int
mmix_valid_comparison (code, mode, op)
     RTX_CODE code;
     enum machine_mode mode;
     rtx op;
{
  if (mode == VOIDmode && op != NULL_RTX)
    mode = GET_MODE (op);

  /* We don't care to look at these, they should always be valid.  */
  if (mode == CCmode || mode == CC_UNSmode || mode == DImode)
    return 1;

  if ((mode == CC_FPmode || mode == DFmode)
      && (code == GT || code == LT))
    return 1;

  if ((mode == CC_FPEQmode || mode == DFmode)
      && (code == EQ || code == NE))
    return 1;

  if ((mode == CC_FUNmode || mode == DFmode)
      && (code == ORDERED || code == UNORDERED))
    return 1;

  return 0;
}

/* X and Y are two things to compare using CODE.  Emit a compare insn if
   possible and return the rtx for the cc-reg in the proper mode, or
   NULL_RTX if this is not a valid comparison.  */

rtx
mmix_gen_compare_reg (code, x, y)
     RTX_CODE code;
     rtx x, y;
{
  enum machine_mode ccmode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg;

  /* FIXME: Do we get constants here?  Of double mode?  */
  enum machine_mode mode
    = GET_MODE (x) == VOIDmode
    ? GET_MODE (y)
    : GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT ? DFmode : DImode;

  if (! mmix_valid_comparison (code, mode, x))
    return NULL_RTX;

  cc_reg = gen_reg_rtx (ccmode);

  /* FIXME:  Can we avoid emitting a compare insn here?  */
  if (! REG_P (x) && ! REG_P (y))
    x = force_reg (mode, x);

  CANONICALIZE_COMPARISON (code, x, y);

  /* If it's not quite right yet, put y in a register.  */
  if (! REG_P (y)
      && (GET_CODE (y) != CONST_INT
	  || ! CONST_OK_FOR_LETTER_P (INTVAL (y), 'I')))
    y = force_reg (mode, y);

  emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
			  gen_rtx_COMPARE (ccmode, x, y)));

  return cc_reg;
}

/* Local (static) helper functions.  */

/* Print operator suitable for doing something with a shiftable
   wyde.  The type of operator is passed as an asm output modifier.  */

static void
mmix_output_shiftvalue_op_from_str (stream, mainop, value)
     FILE *stream;
     const char *mainop;
     HOST_WIDEST_INT value;
{
  static const char *const op_part[] = {"L", "ML", "MH", "H"};
  int i;

  if (! mmix_shiftable_wyde_value (value))
    {
      char s[sizeof ("0xffffffffffffffff")];
      sprintf (s, HOST_WIDEST_INT_PRINT_HEX, value);
      internal_error ("MMIX Internal: %s is not a shiftable int", s);
    }

  for (i = 0; i < 4; i++)
    {
      /* We know we're through when we find one-bits in the low
	 16 bits.  */
      if (value & 0xffff)
	{
	  fprintf (stream, "%s%s", mainop, op_part[i]);
	  return;
	}
      value >>= 16;
    }

  /* No bits set?  Then it must have been zero.  */
  fprintf (stream, "%sL", mainop);
}

/* Print a 64-bit value, optionally prefixed by assembly pseudo.  */

static void
mmix_output_octa (stream, value, do_begin_end)
     FILE *stream;
     HOST_WIDEST_INT value;
     int do_begin_end;
{
  /* Snipped from final.c:output_addr_const.  We need to avoid the
     presumed universal "0x" prefix.  We can do it by replacing "0x" with
     "#0" here; we must avoid a space in the operands and no, the zero
     won't cause the number to be assumed in octal format.  */
  char hex_format[sizeof (HOST_WIDEST_INT_PRINT_HEX)];

  if (do_begin_end)
    fprintf (stream, "\tOCTA ");

  strcpy (hex_format, HOST_WIDEST_INT_PRINT_HEX);
  hex_format[0] = '#';
  hex_format[1] = '0';

  /* Provide a few alternative output formats depending on the number, to
     improve legibility of assembler output.  */
  if ((value < (HOST_WIDEST_INT) 0 && value > (HOST_WIDEST_INT) -10000)
      || (value >= (HOST_WIDEST_INT) 0 && value <= (HOST_WIDEST_INT) 16384))
    fprintf (stream, "%d", (int) value);
  else if (value > (HOST_WIDEST_INT) 0
	   && value < ((HOST_WIDEST_INT) 1 << 31) * 2)
    fprintf (stream, "#%x", (unsigned int) value);
  else
    fprintf (stream, hex_format, value);

  if (do_begin_end)
    fprintf (stream, "\n");
}

/* Print the presumed shiftable wyde argument shifted into place (to
   be output with an operand).  */

static void
mmix_output_shifted_value (stream, value)
     FILE * stream;
     HOST_WIDEST_INT value;
{
  int i;

  if (! mmix_shiftable_wyde_value (value))
    {
      char s[16+2+1];
      sprintf (s, HOST_WIDEST_INT_PRINT_HEX, value);
      internal_error ("MMIX Internal: %s is not a shiftable int", s);
    }

  for (i = 0; i < 4; i++)
  {
    /* We know we're through when we find one-bits in the low 16 bits.  */
    if (value & 0xffff)
    {
      fprintf (stream, "#%x", (int) (value & 0xffff));
      return;
    }

    value >>= 16;
  }

  /* No bits set?  Then it must have been zero.  */
  fprintf (stream, "0");
}

/* Output an MMIX condition name corresponding to an operator
   and operands:
   (comparison_operator [(comparison_operator ...) (const_int 0)])
   which means we have to look at *two* operators.

   The argument "reversed" refers to reversal of the condition (not the
   same as swapping the arguments).  */

static void
mmix_output_condition (stream, x, reversed)
     FILE *stream;
     rtx x;
     int reversed;
{
  struct cc_conv
  {
    RTX_CODE cc;

    /* The normal output cc-code.  */
    const char *const normal;

    /* The reversed cc-code, or NULL if invalid.  */
    const char *const reversed;
  };

  struct cc_type_conv
  {
    enum machine_mode cc_mode;

    /* Terminated with {NIL, NULL, NULL} */
    const struct cc_conv *const convs;
  };

#undef CCEND
#define CCEND {NIL, NULL, NULL}

  static const struct cc_conv cc_fun_convs[]
    = {{ORDERED, "Z", "P"},
       {UNORDERED, "P", "Z"},
       CCEND};
  static const struct cc_conv cc_fp_convs[]
    = {{GT, "P", NULL},
       {LT, "N", NULL},
       CCEND};
  static const struct cc_conv cc_fpeq_convs[]
    = {{NE, "Z", "P"},
       {EQ, "P", "Z"},
       CCEND};
  static const struct cc_conv cc_uns_convs[]
    = {{GEU, "NN", "N"},
       {GTU, "P", "NP"},
       {LEU, "NP", "P"},
       {LTU, "N", "NN"},
       CCEND};
  static const struct cc_conv cc_signed_convs[]
    = {{NE, "NZ", "Z"},
       {EQ, "Z", "NZ"},
       {GE, "NN", "N"},
       {GT, "P", "NP"},
       {LE, "NP", "P"},
       {LT, "N", "NN"},
       CCEND};
  static const struct cc_conv cc_di_convs[]
    = {{NE, "NZ", "Z"},
       {EQ, "Z", "NZ"},
       {GE, "NN", "N"},
       {GT, "P", "NP"},
       {LE, "NP", "P"},
       {LT, "N", "NN"},
       {GTU, "NZ", "Z"},
       {LEU, "Z", "NZ"},
       CCEND};
#undef CCEND

  static const struct cc_type_conv cc_convs[]
    = {{CC_FUNmode, cc_fun_convs},
       {CC_FPmode, cc_fp_convs},
       {CC_FPEQmode, cc_fpeq_convs},
       {CC_UNSmode, cc_uns_convs},
       {CCmode, cc_signed_convs},
       {DImode, cc_di_convs}};

  unsigned int i;
  int j;

  enum machine_mode mode = GET_MODE (XEXP (x, 0));
  RTX_CODE cc = GET_CODE (x);

  for (i = 0; i < sizeof (cc_convs)/sizeof(*cc_convs); i++)
    {
      if (mode == cc_convs[i].cc_mode)
	{
	  for (j = 0; cc_convs[i].convs[j].cc != NIL; j++)
	    if (cc == cc_convs[i].convs[j].cc)
	      {
		const char *mmix_cc
		  = (reversed ? cc_convs[i].convs[j].reversed
		     : cc_convs[i].convs[j].normal);

		if (mmix_cc == NULL)
		  fatal_insn ("MMIX Internal: Trying to output invalidly\
 reversed condition:", x);

		fprintf (stream, "%s", mmix_cc);
		return;
	      }

	  fatal_insn ("MMIX Internal: What's the CC of this?", x);
	}
    }

  fatal_insn ("MMIX Internal: What is the CC of this?", x);
}

/* Return the bit-value for a const_int or const_double.  */

static HOST_WIDEST_INT
mmix_intval (x)
     rtx x;
{
  unsigned HOST_WIDEST_INT retval;

  if (GET_CODE (x) == CONST_INT)
    return INTVAL (x);

  /* We make a little song and dance because converting to long long in
     gcc-2.7.2 is broken.  I still want people to be able to use it for
     cross-compilation to MMIX.  */
  if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == VOIDmode)
    {
      if (sizeof (HOST_WIDE_INT) < sizeof (HOST_WIDEST_INT))
	{
	  retval = (unsigned) CONST_DOUBLE_LOW (x) / 2;
	  retval *= 2;
	  retval |= CONST_DOUBLE_LOW (x) & 1;

	  retval |=
	    (unsigned HOST_WIDEST_INT) CONST_DOUBLE_HIGH (x)
	      << (HOST_BITS_PER_LONG);
	}
      else
	retval = CONST_DOUBLE_HIGH (x);

      return retval;
    }

  if (GET_CODE (x) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE value;

      /* FIXME:  This macro is not in the manual but should be.  */
      REAL_VALUE_FROM_CONST_DOUBLE (value, x);

      if (GET_MODE (x) == DFmode)
	{
	  long bits[2];

	  REAL_VALUE_TO_TARGET_DOUBLE (value, bits);

	  if (sizeof (long) < sizeof (HOST_WIDEST_INT))
	    {
	      retval = (unsigned long) bits[1] / 2;
	      retval *= 2;
	      retval |= (unsigned long) bits[1] & 1;
	      retval
		|= (unsigned HOST_WIDEST_INT) bits[0]
		  << (sizeof (bits[0]) * 8);
	    }
	  else
	    retval = (unsigned long) bits[1];

	  return retval;
	}
      else if (GET_MODE (x) == SFmode)
	{
	  long bits;
	  REAL_VALUE_TO_TARGET_SINGLE (value, bits);

	  return (unsigned long) bits;
	}
    }

  fatal_insn ("MMIX Internal: This is not a constant:", x);
}

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
