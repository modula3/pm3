/* *INDENT-OFF* */ /* THIS FILE IS GENERATED */

/* Dynamic architecture support for GDB, the GNU debugger.
   Copyright 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

/* This file was created with the aid of ``gdbarch.sh''.

   The Bourne shell script ``gdbarch.sh'' creates the files
   ``new-gdbarch.c'' and ``new-gdbarch.h and then compares them
   against the existing ``gdbarch.[hc]''.  Any differences found
   being reported.

   If editing this file, please also run gdbarch.sh and merge any
   changes into that script. Conversely, when making sweeping changes
   to this file, modifying gdbarch.sh and using its output may prove
   easier. */


#include "defs.h"
#include "arch-utils.h"

#if GDB_MULTI_ARCH
#include "gdbcmd.h"
#include "inferior.h" /* enum CALL_DUMMY_LOCATION et.al. */
#else
/* Just include everything in sight so that the every old definition
   of macro is visible. */
#include "gdb_string.h"
#include <ctype.h>
#include "symtab.h"
#include "frame.h"
#include "inferior.h"
#include "breakpoint.h"
#include "gdb_wait.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "target.h"
#include "gdbthread.h"
#include "annotate.h"
#include "symfile.h"		/* for overlay functions */
#endif
#include "symcat.h"

#include "floatformat.h"

#include "gdb_assert.h"

/* Static function declarations */

static void verify_gdbarch (struct gdbarch *gdbarch);
static void alloc_gdbarch_data (struct gdbarch *);
static void init_gdbarch_data (struct gdbarch *);
static void free_gdbarch_data (struct gdbarch *);
static void init_gdbarch_swap (struct gdbarch *);
static void swapout_gdbarch_swap (struct gdbarch *);
static void swapin_gdbarch_swap (struct gdbarch *);

/* Convenience macro for allocting typesafe memory. */

#ifndef XMALLOC
#define XMALLOC(TYPE) (TYPE*) xmalloc (sizeof (TYPE))
#endif


/* Non-zero if we want to trace architecture code.  */

#ifndef GDBARCH_DEBUG
#define GDBARCH_DEBUG 0
#endif
int gdbarch_debug = GDBARCH_DEBUG;


/* Maintain the struct gdbarch object */

struct gdbarch
{
  /* basic architectural information */
  const struct bfd_arch_info * bfd_arch_info;
  int byte_order;

  /* target specific vector. */
  struct gdbarch_tdep *tdep;
  gdbarch_dump_tdep_ftype *dump_tdep;

  /* per-architecture data-pointers */
  unsigned nr_data;
  void **data;

  /* per-architecture swap-regions */
  struct gdbarch_swap *swap;

  /* Multi-arch values.

     When extending this structure you must:

     Add the field below.

     Declare set/get functions and define the corresponding
     macro in gdbarch.h.

     gdbarch_alloc(): If zero/NULL is not a suitable default,
     initialize the new field.

     verify_gdbarch(): Confirm that the target updated the field
     correctly.

     gdbarch_dump(): Add a fprintf_unfiltered call so that the new
     field is dumped out

     ``startup_gdbarch()'': Append an initial value to the static
     variable (base values on the host's c-type system).

     get_gdbarch(): Implement the set/get functions (probably using
     the macro's as shortcuts).

     */

  int short_bit;
  int int_bit;
  int long_bit;
  int long_long_bit;
  int float_bit;
  int double_bit;
  int long_double_bit;
  int ptr_bit;
  int addr_bit;
  int bfd_vma_bit;
  int ieee_float;
  gdbarch_read_pc_ftype *read_pc;
  gdbarch_write_pc_ftype *write_pc;
  gdbarch_read_fp_ftype *read_fp;
  gdbarch_write_fp_ftype *write_fp;
  gdbarch_read_sp_ftype *read_sp;
  gdbarch_write_sp_ftype *write_sp;
  gdbarch_register_read_ftype *register_read;
  gdbarch_register_write_ftype *register_write;
  int num_regs;
  int num_pseudo_regs;
  int sp_regnum;
  int fp_regnum;
  int pc_regnum;
  int fp0_regnum;
  int npc_regnum;
  int nnpc_regnum;
  gdbarch_stab_reg_to_regnum_ftype *stab_reg_to_regnum;
  gdbarch_ecoff_reg_to_regnum_ftype *ecoff_reg_to_regnum;
  gdbarch_dwarf_reg_to_regnum_ftype *dwarf_reg_to_regnum;
  gdbarch_sdb_reg_to_regnum_ftype *sdb_reg_to_regnum;
  gdbarch_dwarf2_reg_to_regnum_ftype *dwarf2_reg_to_regnum;
  gdbarch_register_name_ftype *register_name;
  int register_size;
  int register_bytes;
  gdbarch_register_byte_ftype *register_byte;
  gdbarch_register_raw_size_ftype *register_raw_size;
  int max_register_raw_size;
  gdbarch_register_virtual_size_ftype *register_virtual_size;
  int max_register_virtual_size;
  gdbarch_register_virtual_type_ftype *register_virtual_type;
  gdbarch_do_registers_info_ftype *do_registers_info;
  gdbarch_register_sim_regno_ftype *register_sim_regno;
  gdbarch_register_bytes_ok_ftype *register_bytes_ok;
  gdbarch_cannot_fetch_register_ftype *cannot_fetch_register;
  gdbarch_cannot_store_register_ftype *cannot_store_register;
  int use_generic_dummy_frames;
  int call_dummy_location;
  gdbarch_call_dummy_address_ftype *call_dummy_address;
  CORE_ADDR call_dummy_start_offset;
  CORE_ADDR call_dummy_breakpoint_offset;
  int call_dummy_breakpoint_offset_p;
  int call_dummy_length;
  gdbarch_pc_in_call_dummy_ftype *pc_in_call_dummy;
  int call_dummy_p;
  LONGEST * call_dummy_words;
  int sizeof_call_dummy_words;
  int call_dummy_stack_adjust_p;
  int call_dummy_stack_adjust;
  gdbarch_fix_call_dummy_ftype *fix_call_dummy;
  gdbarch_init_frame_pc_first_ftype *init_frame_pc_first;
  gdbarch_init_frame_pc_ftype *init_frame_pc;
  int believe_pcc_promotion;
  int believe_pcc_promotion_type;
  gdbarch_coerce_float_to_double_ftype *coerce_float_to_double;
  gdbarch_get_saved_register_ftype *get_saved_register;
  gdbarch_register_convertible_ftype *register_convertible;
  gdbarch_register_convert_to_virtual_ftype *register_convert_to_virtual;
  gdbarch_register_convert_to_raw_ftype *register_convert_to_raw;
  gdbarch_fetch_pseudo_register_ftype *fetch_pseudo_register;
  gdbarch_store_pseudo_register_ftype *store_pseudo_register;
  gdbarch_pointer_to_address_ftype *pointer_to_address;
  gdbarch_address_to_pointer_ftype *address_to_pointer;
  gdbarch_return_value_on_stack_ftype *return_value_on_stack;
  gdbarch_extract_return_value_ftype *extract_return_value;
  gdbarch_push_arguments_ftype *push_arguments;
  gdbarch_push_dummy_frame_ftype *push_dummy_frame;
  gdbarch_push_return_address_ftype *push_return_address;
  gdbarch_pop_frame_ftype *pop_frame;
  gdbarch_store_struct_return_ftype *store_struct_return;
  gdbarch_store_return_value_ftype *store_return_value;
  gdbarch_extract_struct_value_address_ftype *extract_struct_value_address;
  gdbarch_use_struct_convention_ftype *use_struct_convention;
  gdbarch_frame_init_saved_regs_ftype *frame_init_saved_regs;
  gdbarch_init_extra_frame_info_ftype *init_extra_frame_info;
  gdbarch_skip_prologue_ftype *skip_prologue;
  gdbarch_prologue_frameless_p_ftype *prologue_frameless_p;
  gdbarch_inner_than_ftype *inner_than;
  gdbarch_breakpoint_from_pc_ftype *breakpoint_from_pc;
  gdbarch_memory_insert_breakpoint_ftype *memory_insert_breakpoint;
  gdbarch_memory_remove_breakpoint_ftype *memory_remove_breakpoint;
  CORE_ADDR decr_pc_after_break;
  gdbarch_prepare_to_proceed_ftype *prepare_to_proceed;
  CORE_ADDR function_start_offset;
  gdbarch_remote_translate_xfer_address_ftype *remote_translate_xfer_address;
  CORE_ADDR frame_args_skip;
  gdbarch_frameless_function_invocation_ftype *frameless_function_invocation;
  gdbarch_frame_chain_ftype *frame_chain;
  gdbarch_frame_chain_valid_ftype *frame_chain_valid;
  gdbarch_frame_saved_pc_ftype *frame_saved_pc;
  gdbarch_frame_args_address_ftype *frame_args_address;
  gdbarch_frame_locals_address_ftype *frame_locals_address;
  gdbarch_saved_pc_after_call_ftype *saved_pc_after_call;
  gdbarch_frame_num_args_ftype *frame_num_args;
  gdbarch_stack_align_ftype *stack_align;
  int extra_stack_alignment_needed;
  gdbarch_reg_struct_has_addr_ftype *reg_struct_has_addr;
  gdbarch_save_dummy_frame_tos_ftype *save_dummy_frame_tos;
  int parm_boundary;
  const struct floatformat * float_format;
  const struct floatformat * double_format;
  const struct floatformat * long_double_format;
  gdbarch_convert_from_func_ptr_addr_ftype *convert_from_func_ptr_addr;
  gdbarch_addr_bits_remove_ftype *addr_bits_remove;
  gdbarch_software_single_step_ftype *software_single_step;
};


/* The default architecture uses host values (for want of a better
   choice). */

extern const struct bfd_arch_info bfd_default_arch_struct;

struct gdbarch startup_gdbarch =
{
  /* basic architecture information */
  &bfd_default_arch_struct,
  BIG_ENDIAN,
  /* target specific vector and its dump routine */
  NULL, NULL,
  /*per-architecture data-pointers and swap regions */
  0, NULL, NULL,
  /* Multi-arch values */
  8 * sizeof (short),
  8 * sizeof (int),
  8 * sizeof (long),
  8 * sizeof (LONGEST),
  8 * sizeof (float),
  8 * sizeof (double),
  8 * sizeof (long double),
  8 * sizeof (void*),
  8 * sizeof (void*),
  8 * sizeof (void*),
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  generic_get_saved_register,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  /* startup_gdbarch() */
};

struct gdbarch *current_gdbarch = &startup_gdbarch;

/* Do any initialization needed for a non-multiarch configuration
   after the _initialize_MODULE functions have been run.  */
void
initialize_non_multiarch ()
{
  alloc_gdbarch_data (&startup_gdbarch);
  init_gdbarch_data (&startup_gdbarch);
}


/* Create a new ``struct gdbarch'' based on information provided by
   ``struct gdbarch_info''. */

struct gdbarch *
gdbarch_alloc (const struct gdbarch_info *info,
               struct gdbarch_tdep *tdep)
{
  struct gdbarch *gdbarch = XMALLOC (struct gdbarch);
  memset (gdbarch, 0, sizeof (*gdbarch));

  alloc_gdbarch_data (gdbarch);

  gdbarch->tdep = tdep;

  gdbarch->bfd_arch_info = info->bfd_arch_info;
  gdbarch->byte_order = info->byte_order;

  /* Force the explicit initialization of these. */
  gdbarch->short_bit = 2*TARGET_CHAR_BIT;
  gdbarch->int_bit = 4*TARGET_CHAR_BIT;
  gdbarch->long_bit = 4*TARGET_CHAR_BIT;
  gdbarch->long_long_bit = 2*TARGET_LONG_BIT;
  gdbarch->float_bit = 4*TARGET_CHAR_BIT;
  gdbarch->double_bit = 8*TARGET_CHAR_BIT;
  gdbarch->long_double_bit = 2*TARGET_DOUBLE_BIT;
  gdbarch->ptr_bit = TARGET_INT_BIT;
  gdbarch->bfd_vma_bit = TARGET_ARCHITECTURE->bits_per_address;
  gdbarch->read_pc = generic_target_read_pc;
  gdbarch->write_pc = generic_target_write_pc;
  gdbarch->read_fp = generic_target_read_fp;
  gdbarch->write_fp = generic_target_write_fp;
  gdbarch->read_sp = generic_target_read_sp;
  gdbarch->write_sp = generic_target_write_sp;
  gdbarch->num_regs = -1;
  gdbarch->sp_regnum = -1;
  gdbarch->fp_regnum = -1;
  gdbarch->pc_regnum = -1;
  gdbarch->fp0_regnum = -1;
  gdbarch->npc_regnum = -1;
  gdbarch->nnpc_regnum = -1;
  gdbarch->stab_reg_to_regnum = no_op_reg_to_regnum;
  gdbarch->ecoff_reg_to_regnum = no_op_reg_to_regnum;
  gdbarch->dwarf_reg_to_regnum = no_op_reg_to_regnum;
  gdbarch->sdb_reg_to_regnum = no_op_reg_to_regnum;
  gdbarch->dwarf2_reg_to_regnum = no_op_reg_to_regnum;
  gdbarch->register_name = legacy_register_name;
  gdbarch->register_size = -1;
  gdbarch->register_bytes = -1;
  gdbarch->max_register_raw_size = -1;
  gdbarch->max_register_virtual_size = -1;
  gdbarch->do_registers_info = do_registers_info;
  gdbarch->register_sim_regno = default_register_sim_regno;
  gdbarch->cannot_fetch_register = cannot_register_not;
  gdbarch->cannot_store_register = cannot_register_not;
  gdbarch->use_generic_dummy_frames = -1;
  gdbarch->call_dummy_start_offset = -1;
  gdbarch->call_dummy_breakpoint_offset = -1;
  gdbarch->call_dummy_breakpoint_offset_p = -1;
  gdbarch->call_dummy_length = -1;
  gdbarch->call_dummy_p = -1;
  gdbarch->call_dummy_words = legacy_call_dummy_words;
  gdbarch->sizeof_call_dummy_words = legacy_sizeof_call_dummy_words;
  gdbarch->call_dummy_stack_adjust_p = -1;
  gdbarch->init_frame_pc_first = init_frame_pc_noop;
  gdbarch->init_frame_pc = init_frame_pc_default;
  gdbarch->coerce_float_to_double = default_coerce_float_to_double;
  gdbarch->register_convertible = generic_register_convertible_not;
  gdbarch->pointer_to_address = unsigned_pointer_to_address;
  gdbarch->address_to_pointer = unsigned_address_to_pointer;
  gdbarch->return_value_on_stack = generic_return_value_on_stack_not;
  gdbarch->prologue_frameless_p = generic_prologue_frameless_p;
  gdbarch->breakpoint_from_pc = legacy_breakpoint_from_pc;
  gdbarch->memory_insert_breakpoint = default_memory_insert_breakpoint;
  gdbarch->memory_remove_breakpoint = default_memory_remove_breakpoint;
  gdbarch->decr_pc_after_break = -1;
  gdbarch->prepare_to_proceed = default_prepare_to_proceed;
  gdbarch->function_start_offset = -1;
  gdbarch->remote_translate_xfer_address = generic_remote_translate_xfer_address;
  gdbarch->frame_args_skip = -1;
  gdbarch->frameless_function_invocation = generic_frameless_function_invocation_not;
  gdbarch->extra_stack_alignment_needed = 1;
  gdbarch->convert_from_func_ptr_addr = core_addr_identity;
  gdbarch->addr_bits_remove = core_addr_identity;
  /* gdbarch_alloc() */

  return gdbarch;
}


/* Free a gdbarch struct.  This should never happen in normal
   operation --- once you've created a gdbarch, you keep it around.
   However, if an architecture's init function encounters an error
   building the structure, it may need to clean up a partially
   constructed gdbarch.  */

void
gdbarch_free (struct gdbarch *arch)
{
  gdb_assert (arch != NULL);
  free_gdbarch_data (arch);
  xfree (arch);
}


/* Ensure that all values in a GDBARCH are reasonable. */

static void
verify_gdbarch (struct gdbarch *gdbarch)
{
  /* Only perform sanity checks on a multi-arch target. */
  if (!GDB_MULTI_ARCH)
    return;
  /* fundamental */
  if (gdbarch->byte_order == 0)
    internal_error (__FILE__, __LINE__,
                    "verify_gdbarch: byte-order unset");
  if (gdbarch->bfd_arch_info == NULL)
    internal_error (__FILE__, __LINE__,
                    "verify_gdbarch: bfd_arch_info unset");
  /* Check those that need to be defined for the given multi-arch level. */
  /* Skip verify of short_bit, invalid_p == 0 */
  /* Skip verify of int_bit, invalid_p == 0 */
  /* Skip verify of long_bit, invalid_p == 0 */
  /* Skip verify of long_long_bit, invalid_p == 0 */
  /* Skip verify of float_bit, invalid_p == 0 */
  /* Skip verify of double_bit, invalid_p == 0 */
  /* Skip verify of long_double_bit, invalid_p == 0 */
  /* Skip verify of ptr_bit, invalid_p == 0 */
  if (gdbarch->addr_bit == 0)
    gdbarch->addr_bit = TARGET_PTR_BIT;
  /* Skip verify of bfd_vma_bit, invalid_p == 0 */
  /* Skip verify of ieee_float, invalid_p == 0 */
  /* Skip verify of read_pc, invalid_p == 0 */
  /* Skip verify of write_pc, invalid_p == 0 */
  /* Skip verify of read_fp, invalid_p == 0 */
  /* Skip verify of write_fp, invalid_p == 0 */
  /* Skip verify of read_sp, invalid_p == 0 */
  /* Skip verify of write_sp, invalid_p == 0 */
  /* Skip verify of register_read, has predicate */
  /* Skip verify of register_write, has predicate */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->num_regs == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: num_regs invalid");
  /* Skip verify of num_pseudo_regs, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->sp_regnum == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: sp_regnum invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->fp_regnum == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: fp_regnum invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->pc_regnum == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: pc_regnum invalid");
  /* Skip verify of fp0_regnum, invalid_p == 0 */
  /* Skip verify of npc_regnum, invalid_p == 0 */
  /* Skip verify of nnpc_regnum, invalid_p == 0 */
  /* Skip verify of stab_reg_to_regnum, invalid_p == 0 */
  /* Skip verify of ecoff_reg_to_regnum, invalid_p == 0 */
  /* Skip verify of dwarf_reg_to_regnum, invalid_p == 0 */
  /* Skip verify of sdb_reg_to_regnum, invalid_p == 0 */
  /* Skip verify of dwarf2_reg_to_regnum, invalid_p == 0 */
  /* Skip verify of register_name, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->register_size == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: register_size invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->register_bytes == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: register_bytes invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->register_byte == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: register_byte invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->register_raw_size == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: register_raw_size invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->max_register_raw_size == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: max_register_raw_size invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->register_virtual_size == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: register_virtual_size invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->max_register_virtual_size == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: max_register_virtual_size invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->register_virtual_type == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: register_virtual_type invalid");
  /* Skip verify of do_registers_info, invalid_p == 0 */
  /* Skip verify of register_sim_regno, invalid_p == 0 */
  /* Skip verify of register_bytes_ok, has predicate */
  /* Skip verify of cannot_fetch_register, invalid_p == 0 */
  /* Skip verify of cannot_store_register, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->use_generic_dummy_frames == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: use_generic_dummy_frames invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->call_dummy_location == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_location invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->call_dummy_location == AT_ENTRY_POINT && gdbarch->call_dummy_address == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_address invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->call_dummy_start_offset == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_start_offset invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->call_dummy_breakpoint_offset_p && gdbarch->call_dummy_breakpoint_offset == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_breakpoint_offset invalid");
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->call_dummy_breakpoint_offset_p == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_breakpoint_offset_p invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->call_dummy_length == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_length invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->pc_in_call_dummy == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: pc_in_call_dummy invalid");
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->call_dummy_p == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_p invalid");
  /* Skip verify of call_dummy_words, invalid_p == 0 */
  /* Skip verify of sizeof_call_dummy_words, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->call_dummy_stack_adjust_p == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_stack_adjust_p invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->call_dummy_stack_adjust_p && gdbarch->call_dummy_stack_adjust == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: call_dummy_stack_adjust invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->fix_call_dummy == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: fix_call_dummy invalid");
  /* Skip verify of init_frame_pc_first, invalid_p == 0 */
  /* Skip verify of init_frame_pc, invalid_p == 0 */
  /* Skip verify of coerce_float_to_double, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->get_saved_register == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: get_saved_register invalid");
  /* Skip verify of register_convertible, invalid_p == 0 */
  /* Skip verify of register_convert_to_virtual, invalid_p == 0 */
  /* Skip verify of register_convert_to_raw, invalid_p == 0 */
  /* Skip verify of fetch_pseudo_register, invalid_p == 0 */
  /* Skip verify of store_pseudo_register, invalid_p == 0 */
  /* Skip verify of pointer_to_address, invalid_p == 0 */
  /* Skip verify of address_to_pointer, invalid_p == 0 */
  /* Skip verify of return_value_on_stack, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->extract_return_value == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: extract_return_value invalid");
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->push_arguments == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: push_arguments invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->push_dummy_frame == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: push_dummy_frame invalid");
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->push_return_address == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: push_return_address invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->pop_frame == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: pop_frame invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->store_struct_return == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: store_struct_return invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->store_return_value == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: store_return_value invalid");
  /* Skip verify of extract_struct_value_address, has predicate */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->use_struct_convention == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: use_struct_convention invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_init_saved_regs == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_init_saved_regs invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->init_extra_frame_info == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: init_extra_frame_info invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->skip_prologue == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: skip_prologue invalid");
  /* Skip verify of prologue_frameless_p, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->inner_than == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: inner_than invalid");
  /* Skip verify of breakpoint_from_pc, invalid_p == 0 */
  /* Skip verify of memory_insert_breakpoint, invalid_p == 0 */
  /* Skip verify of memory_remove_breakpoint, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->decr_pc_after_break == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: decr_pc_after_break invalid");
  /* Skip verify of prepare_to_proceed, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->function_start_offset == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: function_start_offset invalid");
  /* Skip verify of remote_translate_xfer_address, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_args_skip == -1))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_args_skip invalid");
  /* Skip verify of frameless_function_invocation, invalid_p == 0 */
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_chain == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_chain invalid");
  if ((GDB_MULTI_ARCH >= 1)
      && (gdbarch->frame_chain_valid == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_chain_valid invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_saved_pc == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_saved_pc invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_args_address == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_args_address invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_locals_address == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_locals_address invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->saved_pc_after_call == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: saved_pc_after_call invalid");
  if ((GDB_MULTI_ARCH >= 2)
      && (gdbarch->frame_num_args == 0))
    internal_error (__FILE__, __LINE__,
                    "gdbarch: verify_gdbarch: frame_num_args invalid");
  /* Skip verify of stack_align, has predicate */
  /* Skip verify of extra_stack_alignment_needed, invalid_p == 0 */
  /* Skip verify of reg_struct_has_addr, has predicate */
  /* Skip verify of save_dummy_frame_tos, has predicate */
  if (gdbarch->float_format == 0)
    gdbarch->float_format = default_float_format (gdbarch);
  if (gdbarch->double_format == 0)
    gdbarch->double_format = default_double_format (gdbarch);
  if (gdbarch->long_double_format == 0)
    gdbarch->long_double_format = &floatformat_unknown;
  /* Skip verify of convert_from_func_ptr_addr, invalid_p == 0 */
  /* Skip verify of addr_bits_remove, invalid_p == 0 */
  /* Skip verify of software_single_step, has predicate */
}


/* Print out the details of the current architecture. */

/* NOTE/WARNING: The parameter is called ``current_gdbarch'' so that it
   just happens to match the global variable ``current_gdbarch''.  That
   way macros refering to that variable get the local and not the global
   version - ulgh.  Once everything is parameterised with gdbarch, this
   will go away. */

void
gdbarch_dump (struct gdbarch *gdbarch, struct ui_file *file)
{
  fprintf_unfiltered (file,
                      "gdbarch_dump: GDB_MULTI_ARCH = %d\n",
                      GDB_MULTI_ARCH);
#ifdef TARGET_ARCHITECTURE
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_ARCHITECTURE # %s\n",
                      XSTRING (TARGET_ARCHITECTURE));
#endif
#ifdef TARGET_BYTE_ORDER
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_BYTE_ORDER # %s\n",
                      XSTRING (TARGET_BYTE_ORDER));
#endif
#ifdef TARGET_SHORT_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_SHORT_BIT # %s\n",
                      XSTRING (TARGET_SHORT_BIT));
#endif
#ifdef TARGET_INT_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_INT_BIT # %s\n",
                      XSTRING (TARGET_INT_BIT));
#endif
#ifdef TARGET_LONG_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_BIT # %s\n",
                      XSTRING (TARGET_LONG_BIT));
#endif
#ifdef TARGET_LONG_LONG_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_LONG_BIT # %s\n",
                      XSTRING (TARGET_LONG_LONG_BIT));
#endif
#ifdef TARGET_FLOAT_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_FLOAT_BIT # %s\n",
                      XSTRING (TARGET_FLOAT_BIT));
#endif
#ifdef TARGET_DOUBLE_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_DOUBLE_BIT # %s\n",
                      XSTRING (TARGET_DOUBLE_BIT));
#endif
#ifdef TARGET_LONG_DOUBLE_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_DOUBLE_BIT # %s\n",
                      XSTRING (TARGET_LONG_DOUBLE_BIT));
#endif
#ifdef TARGET_PTR_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_PTR_BIT # %s\n",
                      XSTRING (TARGET_PTR_BIT));
#endif
#ifdef TARGET_ADDR_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_ADDR_BIT # %s\n",
                      XSTRING (TARGET_ADDR_BIT));
#endif
#ifdef TARGET_BFD_VMA_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_BFD_VMA_BIT # %s\n",
                      XSTRING (TARGET_BFD_VMA_BIT));
#endif
#ifdef IEEE_FLOAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: IEEE_FLOAT # %s\n",
                      XSTRING (IEEE_FLOAT));
#endif
#ifdef TARGET_READ_PC
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "TARGET_READ_PC(ptid)",
                      XSTRING (TARGET_READ_PC (ptid)));
#endif
#if defined (TARGET_WRITE_PC) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "TARGET_WRITE_PC(val, ptid)",
                      XSTRING (TARGET_WRITE_PC (val, ptid)));
#endif
#ifdef TARGET_READ_FP
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "TARGET_READ_FP()",
                      XSTRING (TARGET_READ_FP ()));
#endif
#if defined (TARGET_WRITE_FP) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "TARGET_WRITE_FP(val)",
                      XSTRING (TARGET_WRITE_FP (val)));
#endif
#ifdef TARGET_READ_SP
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "TARGET_READ_SP()",
                      XSTRING (TARGET_READ_SP ()));
#endif
#if defined (TARGET_WRITE_SP) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "TARGET_WRITE_SP(val)",
                      XSTRING (TARGET_WRITE_SP (val)));
#endif
#ifdef NUM_REGS
  fprintf_unfiltered (file,
                      "gdbarch_dump: NUM_REGS # %s\n",
                      XSTRING (NUM_REGS));
#endif
#ifdef NUM_PSEUDO_REGS
  fprintf_unfiltered (file,
                      "gdbarch_dump: NUM_PSEUDO_REGS # %s\n",
                      XSTRING (NUM_PSEUDO_REGS));
#endif
#ifdef SP_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: SP_REGNUM # %s\n",
                      XSTRING (SP_REGNUM));
#endif
#ifdef FP_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: FP_REGNUM # %s\n",
                      XSTRING (FP_REGNUM));
#endif
#ifdef PC_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: PC_REGNUM # %s\n",
                      XSTRING (PC_REGNUM));
#endif
#ifdef FP0_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: FP0_REGNUM # %s\n",
                      XSTRING (FP0_REGNUM));
#endif
#ifdef NPC_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: NPC_REGNUM # %s\n",
                      XSTRING (NPC_REGNUM));
#endif
#ifdef NNPC_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: NNPC_REGNUM # %s\n",
                      XSTRING (NNPC_REGNUM));
#endif
#ifdef STAB_REG_TO_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "STAB_REG_TO_REGNUM(stab_regnr)",
                      XSTRING (STAB_REG_TO_REGNUM (stab_regnr)));
#endif
#ifdef ECOFF_REG_TO_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "ECOFF_REG_TO_REGNUM(ecoff_regnr)",
                      XSTRING (ECOFF_REG_TO_REGNUM (ecoff_regnr)));
#endif
#ifdef DWARF_REG_TO_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "DWARF_REG_TO_REGNUM(dwarf_regnr)",
                      XSTRING (DWARF_REG_TO_REGNUM (dwarf_regnr)));
#endif
#ifdef SDB_REG_TO_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "SDB_REG_TO_REGNUM(sdb_regnr)",
                      XSTRING (SDB_REG_TO_REGNUM (sdb_regnr)));
#endif
#ifdef DWARF2_REG_TO_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "DWARF2_REG_TO_REGNUM(dwarf2_regnr)",
                      XSTRING (DWARF2_REG_TO_REGNUM (dwarf2_regnr)));
#endif
#ifdef REGISTER_NAME
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_NAME(regnr)",
                      XSTRING (REGISTER_NAME (regnr)));
#endif
#ifdef REGISTER_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: REGISTER_SIZE # %s\n",
                      XSTRING (REGISTER_SIZE));
#endif
#ifdef REGISTER_BYTES
  fprintf_unfiltered (file,
                      "gdbarch_dump: REGISTER_BYTES # %s\n",
                      XSTRING (REGISTER_BYTES));
#endif
#ifdef REGISTER_BYTE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_BYTE(reg_nr)",
                      XSTRING (REGISTER_BYTE (reg_nr)));
#endif
#ifdef REGISTER_RAW_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_RAW_SIZE(reg_nr)",
                      XSTRING (REGISTER_RAW_SIZE (reg_nr)));
#endif
#ifdef MAX_REGISTER_RAW_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: MAX_REGISTER_RAW_SIZE # %s\n",
                      XSTRING (MAX_REGISTER_RAW_SIZE));
#endif
#ifdef REGISTER_VIRTUAL_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_VIRTUAL_SIZE(reg_nr)",
                      XSTRING (REGISTER_VIRTUAL_SIZE (reg_nr)));
#endif
#ifdef MAX_REGISTER_VIRTUAL_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: MAX_REGISTER_VIRTUAL_SIZE # %s\n",
                      XSTRING (MAX_REGISTER_VIRTUAL_SIZE));
#endif
#ifdef REGISTER_VIRTUAL_TYPE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_VIRTUAL_TYPE(reg_nr)",
                      XSTRING (REGISTER_VIRTUAL_TYPE (reg_nr)));
#endif
#if defined (DO_REGISTERS_INFO) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "DO_REGISTERS_INFO(reg_nr, fpregs)",
                      XSTRING (DO_REGISTERS_INFO (reg_nr, fpregs)));
#endif
#ifdef REGISTER_SIM_REGNO
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_SIM_REGNO(reg_nr)",
                      XSTRING (REGISTER_SIM_REGNO (reg_nr)));
#endif
#ifdef REGISTER_BYTES_OK
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_BYTES_OK(nr_bytes)",
                      XSTRING (REGISTER_BYTES_OK (nr_bytes)));
#endif
#ifdef CANNOT_FETCH_REGISTER
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "CANNOT_FETCH_REGISTER(regnum)",
                      XSTRING (CANNOT_FETCH_REGISTER (regnum)));
#endif
#ifdef CANNOT_STORE_REGISTER
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "CANNOT_STORE_REGISTER(regnum)",
                      XSTRING (CANNOT_STORE_REGISTER (regnum)));
#endif
#ifdef USE_GENERIC_DUMMY_FRAMES
  fprintf_unfiltered (file,
                      "gdbarch_dump: USE_GENERIC_DUMMY_FRAMES # %s\n",
                      XSTRING (USE_GENERIC_DUMMY_FRAMES));
#endif
#ifdef CALL_DUMMY_LOCATION
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_LOCATION # %s\n",
                      XSTRING (CALL_DUMMY_LOCATION));
#endif
#ifdef CALL_DUMMY_ADDRESS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "CALL_DUMMY_ADDRESS()",
                      XSTRING (CALL_DUMMY_ADDRESS ()));
#endif
#ifdef CALL_DUMMY_START_OFFSET
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_START_OFFSET # %s\n",
                      XSTRING (CALL_DUMMY_START_OFFSET));
#endif
#ifdef CALL_DUMMY_BREAKPOINT_OFFSET
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_BREAKPOINT_OFFSET # %s\n",
                      XSTRING (CALL_DUMMY_BREAKPOINT_OFFSET));
#endif
#ifdef CALL_DUMMY_BREAKPOINT_OFFSET_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_BREAKPOINT_OFFSET_P # %s\n",
                      XSTRING (CALL_DUMMY_BREAKPOINT_OFFSET_P));
#endif
#ifdef CALL_DUMMY_LENGTH
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_LENGTH # %s\n",
                      XSTRING (CALL_DUMMY_LENGTH));
#endif
#ifdef PC_IN_CALL_DUMMY
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "PC_IN_CALL_DUMMY(pc, sp, frame_address)",
                      XSTRING (PC_IN_CALL_DUMMY (pc, sp, frame_address)));
#endif
#ifdef CALL_DUMMY_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_P # %s\n",
                      XSTRING (CALL_DUMMY_P));
#endif
#ifdef CALL_DUMMY_WORDS
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_WORDS # %s\n",
                      XSTRING (CALL_DUMMY_WORDS));
#endif
#ifdef SIZEOF_CALL_DUMMY_WORDS
  fprintf_unfiltered (file,
                      "gdbarch_dump: SIZEOF_CALL_DUMMY_WORDS # %s\n",
                      XSTRING (SIZEOF_CALL_DUMMY_WORDS));
#endif
#ifdef CALL_DUMMY_STACK_ADJUST_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_STACK_ADJUST_P # %s\n",
                      XSTRING (CALL_DUMMY_STACK_ADJUST_P));
#endif
#ifdef CALL_DUMMY_STACK_ADJUST
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_STACK_ADJUST # %s\n",
                      XSTRING (CALL_DUMMY_STACK_ADJUST));
#endif
#if defined (FIX_CALL_DUMMY) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FIX_CALL_DUMMY(dummy, pc, fun, nargs, args, type, gcc_p)",
                      XSTRING (FIX_CALL_DUMMY (dummy, pc, fun, nargs, args, type, gcc_p)));
#endif
#if defined (INIT_FRAME_PC_FIRST) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "INIT_FRAME_PC_FIRST(fromleaf, prev)",
                      XSTRING (INIT_FRAME_PC_FIRST (fromleaf, prev)));
#endif
#if defined (INIT_FRAME_PC) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "INIT_FRAME_PC(fromleaf, prev)",
                      XSTRING (INIT_FRAME_PC (fromleaf, prev)));
#endif
#ifdef BELIEVE_PCC_PROMOTION
  fprintf_unfiltered (file,
                      "gdbarch_dump: BELIEVE_PCC_PROMOTION # %s\n",
                      XSTRING (BELIEVE_PCC_PROMOTION));
#endif
#ifdef BELIEVE_PCC_PROMOTION_TYPE
  fprintf_unfiltered (file,
                      "gdbarch_dump: BELIEVE_PCC_PROMOTION_TYPE # %s\n",
                      XSTRING (BELIEVE_PCC_PROMOTION_TYPE));
#endif
#ifdef COERCE_FLOAT_TO_DOUBLE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "COERCE_FLOAT_TO_DOUBLE(formal, actual)",
                      XSTRING (COERCE_FLOAT_TO_DOUBLE (formal, actual)));
#endif
#if defined (GET_SAVED_REGISTER) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "GET_SAVED_REGISTER(raw_buffer, optimized, addrp, frame, regnum, lval)",
                      XSTRING (GET_SAVED_REGISTER (raw_buffer, optimized, addrp, frame, regnum, lval)));
#endif
#ifdef REGISTER_CONVERTIBLE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_CONVERTIBLE(nr)",
                      XSTRING (REGISTER_CONVERTIBLE (nr)));
#endif
#if defined (REGISTER_CONVERT_TO_VIRTUAL) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_CONVERT_TO_VIRTUAL(regnum, type, from, to)",
                      XSTRING (REGISTER_CONVERT_TO_VIRTUAL (regnum, type, from, to)));
#endif
#if defined (REGISTER_CONVERT_TO_RAW) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REGISTER_CONVERT_TO_RAW(type, regnum, from, to)",
                      XSTRING (REGISTER_CONVERT_TO_RAW (type, regnum, from, to)));
#endif
#if defined (FETCH_PSEUDO_REGISTER) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FETCH_PSEUDO_REGISTER(regnum)",
                      XSTRING (FETCH_PSEUDO_REGISTER (regnum)));
#endif
#if defined (STORE_PSEUDO_REGISTER) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "STORE_PSEUDO_REGISTER(regnum)",
                      XSTRING (STORE_PSEUDO_REGISTER (regnum)));
#endif
#ifdef POINTER_TO_ADDRESS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "POINTER_TO_ADDRESS(type, buf)",
                      XSTRING (POINTER_TO_ADDRESS (type, buf)));
#endif
#if defined (ADDRESS_TO_POINTER) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "ADDRESS_TO_POINTER(type, buf, addr)",
                      XSTRING (ADDRESS_TO_POINTER (type, buf, addr)));
#endif
#ifdef RETURN_VALUE_ON_STACK
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "RETURN_VALUE_ON_STACK(type)",
                      XSTRING (RETURN_VALUE_ON_STACK (type)));
#endif
#if defined (EXTRACT_RETURN_VALUE) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "EXTRACT_RETURN_VALUE(type, regbuf, valbuf)",
                      XSTRING (EXTRACT_RETURN_VALUE (type, regbuf, valbuf)));
#endif
#ifdef PUSH_ARGUMENTS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "PUSH_ARGUMENTS(nargs, args, sp, struct_return, struct_addr)",
                      XSTRING (PUSH_ARGUMENTS (nargs, args, sp, struct_return, struct_addr)));
#endif
#if defined (PUSH_DUMMY_FRAME) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "PUSH_DUMMY_FRAME(-)",
                      XSTRING (PUSH_DUMMY_FRAME (-)));
#endif
#ifdef PUSH_RETURN_ADDRESS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "PUSH_RETURN_ADDRESS(pc, sp)",
                      XSTRING (PUSH_RETURN_ADDRESS (pc, sp)));
#endif
#if defined (POP_FRAME) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "POP_FRAME(-)",
                      XSTRING (POP_FRAME (-)));
#endif
#if defined (STORE_STRUCT_RETURN) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "STORE_STRUCT_RETURN(addr, sp)",
                      XSTRING (STORE_STRUCT_RETURN (addr, sp)));
#endif
#if defined (STORE_RETURN_VALUE) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "STORE_RETURN_VALUE(type, valbuf)",
                      XSTRING (STORE_RETURN_VALUE (type, valbuf)));
#endif
#ifdef EXTRACT_STRUCT_VALUE_ADDRESS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "EXTRACT_STRUCT_VALUE_ADDRESS(regbuf)",
                      XSTRING (EXTRACT_STRUCT_VALUE_ADDRESS (regbuf)));
#endif
#ifdef USE_STRUCT_CONVENTION
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "USE_STRUCT_CONVENTION(gcc_p, value_type)",
                      XSTRING (USE_STRUCT_CONVENTION (gcc_p, value_type)));
#endif
#if defined (FRAME_INIT_SAVED_REGS) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_INIT_SAVED_REGS(frame)",
                      XSTRING (FRAME_INIT_SAVED_REGS (frame)));
#endif
#if defined (INIT_EXTRA_FRAME_INFO) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "INIT_EXTRA_FRAME_INFO(fromleaf, frame)",
                      XSTRING (INIT_EXTRA_FRAME_INFO (fromleaf, frame)));
#endif
#ifdef SKIP_PROLOGUE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "SKIP_PROLOGUE(ip)",
                      XSTRING (SKIP_PROLOGUE (ip)));
#endif
#ifdef PROLOGUE_FRAMELESS_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "PROLOGUE_FRAMELESS_P(ip)",
                      XSTRING (PROLOGUE_FRAMELESS_P (ip)));
#endif
#ifdef INNER_THAN
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "INNER_THAN(lhs, rhs)",
                      XSTRING (INNER_THAN (lhs, rhs)));
#endif
#ifdef BREAKPOINT_FROM_PC
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "BREAKPOINT_FROM_PC(pcptr, lenptr)",
                      XSTRING (BREAKPOINT_FROM_PC (pcptr, lenptr)));
#endif
#ifdef MEMORY_INSERT_BREAKPOINT
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "MEMORY_INSERT_BREAKPOINT(addr, contents_cache)",
                      XSTRING (MEMORY_INSERT_BREAKPOINT (addr, contents_cache)));
#endif
#ifdef MEMORY_REMOVE_BREAKPOINT
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "MEMORY_REMOVE_BREAKPOINT(addr, contents_cache)",
                      XSTRING (MEMORY_REMOVE_BREAKPOINT (addr, contents_cache)));
#endif
#ifdef DECR_PC_AFTER_BREAK
  fprintf_unfiltered (file,
                      "gdbarch_dump: DECR_PC_AFTER_BREAK # %s\n",
                      XSTRING (DECR_PC_AFTER_BREAK));
#endif
#ifdef PREPARE_TO_PROCEED
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "PREPARE_TO_PROCEED(select_it)",
                      XSTRING (PREPARE_TO_PROCEED (select_it)));
#endif
#ifdef FUNCTION_START_OFFSET
  fprintf_unfiltered (file,
                      "gdbarch_dump: FUNCTION_START_OFFSET # %s\n",
                      XSTRING (FUNCTION_START_OFFSET));
#endif
#if defined (REMOTE_TRANSLATE_XFER_ADDRESS) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REMOTE_TRANSLATE_XFER_ADDRESS(gdb_addr, gdb_len, rem_addr, rem_len)",
                      XSTRING (REMOTE_TRANSLATE_XFER_ADDRESS (gdb_addr, gdb_len, rem_addr, rem_len)));
#endif
#ifdef FRAME_ARGS_SKIP
  fprintf_unfiltered (file,
                      "gdbarch_dump: FRAME_ARGS_SKIP # %s\n",
                      XSTRING (FRAME_ARGS_SKIP));
#endif
#ifdef FRAMELESS_FUNCTION_INVOCATION
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAMELESS_FUNCTION_INVOCATION(fi)",
                      XSTRING (FRAMELESS_FUNCTION_INVOCATION (fi)));
#endif
#ifdef FRAME_CHAIN
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_CHAIN(frame)",
                      XSTRING (FRAME_CHAIN (frame)));
#endif
#ifdef FRAME_CHAIN_VALID
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_CHAIN_VALID(chain, thisframe)",
                      XSTRING (FRAME_CHAIN_VALID (chain, thisframe)));
#endif
#ifdef FRAME_SAVED_PC
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_SAVED_PC(fi)",
                      XSTRING (FRAME_SAVED_PC (fi)));
#endif
#ifdef FRAME_ARGS_ADDRESS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_ARGS_ADDRESS(fi)",
                      XSTRING (FRAME_ARGS_ADDRESS (fi)));
#endif
#ifdef FRAME_LOCALS_ADDRESS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_LOCALS_ADDRESS(fi)",
                      XSTRING (FRAME_LOCALS_ADDRESS (fi)));
#endif
#ifdef SAVED_PC_AFTER_CALL
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "SAVED_PC_AFTER_CALL(frame)",
                      XSTRING (SAVED_PC_AFTER_CALL (frame)));
#endif
#ifdef FRAME_NUM_ARGS
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "FRAME_NUM_ARGS(frame)",
                      XSTRING (FRAME_NUM_ARGS (frame)));
#endif
#ifdef STACK_ALIGN
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "STACK_ALIGN(sp)",
                      XSTRING (STACK_ALIGN (sp)));
#endif
#ifdef EXTRA_STACK_ALIGNMENT_NEEDED
  fprintf_unfiltered (file,
                      "gdbarch_dump: EXTRA_STACK_ALIGNMENT_NEEDED # %s\n",
                      XSTRING (EXTRA_STACK_ALIGNMENT_NEEDED));
#endif
#ifdef REG_STRUCT_HAS_ADDR
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "REG_STRUCT_HAS_ADDR(gcc_p, type)",
                      XSTRING (REG_STRUCT_HAS_ADDR (gcc_p, type)));
#endif
#if defined (SAVE_DUMMY_FRAME_TOS) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "SAVE_DUMMY_FRAME_TOS(sp)",
                      XSTRING (SAVE_DUMMY_FRAME_TOS (sp)));
#endif
#ifdef PARM_BOUNDARY
  fprintf_unfiltered (file,
                      "gdbarch_dump: PARM_BOUNDARY # %s\n",
                      XSTRING (PARM_BOUNDARY));
#endif
#ifdef TARGET_FLOAT_FORMAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_FLOAT_FORMAT # %s\n",
                      XSTRING (TARGET_FLOAT_FORMAT));
#endif
#ifdef TARGET_DOUBLE_FORMAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_DOUBLE_FORMAT # %s\n",
                      XSTRING (TARGET_DOUBLE_FORMAT));
#endif
#ifdef TARGET_LONG_DOUBLE_FORMAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_DOUBLE_FORMAT # %s\n",
                      XSTRING (TARGET_LONG_DOUBLE_FORMAT));
#endif
#ifdef CONVERT_FROM_FUNC_PTR_ADDR
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "CONVERT_FROM_FUNC_PTR_ADDR(addr)",
                      XSTRING (CONVERT_FROM_FUNC_PTR_ADDR (addr)));
#endif
#ifdef ADDR_BITS_REMOVE
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "ADDR_BITS_REMOVE(addr)",
                      XSTRING (ADDR_BITS_REMOVE (addr)));
#endif
#if defined (SOFTWARE_SINGLE_STEP) && GDB_MULTI_ARCH
  /* Macro might contain `[{}]' when not multi-arch */
  fprintf_unfiltered (file,
                      "gdbarch_dump: %s # %s\n",
                      "SOFTWARE_SINGLE_STEP(sig, insert_breakpoints_p)",
                      XSTRING (SOFTWARE_SINGLE_STEP (sig, insert_breakpoints_p)));
#endif
#ifdef TARGET_ARCHITECTURE
  if (TARGET_ARCHITECTURE != NULL)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_ARCHITECTURE = %s\n",
                        TARGET_ARCHITECTURE->printable_name);
#endif
#ifdef TARGET_BYTE_ORDER
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_BYTE_ORDER = %ld\n",
                      (long) TARGET_BYTE_ORDER);
#endif
#ifdef TARGET_SHORT_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_SHORT_BIT = %ld\n",
                      (long) TARGET_SHORT_BIT);
#endif
#ifdef TARGET_INT_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_INT_BIT = %ld\n",
                      (long) TARGET_INT_BIT);
#endif
#ifdef TARGET_LONG_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_BIT = %ld\n",
                      (long) TARGET_LONG_BIT);
#endif
#ifdef TARGET_LONG_LONG_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_LONG_BIT = %ld\n",
                      (long) TARGET_LONG_LONG_BIT);
#endif
#ifdef TARGET_FLOAT_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_FLOAT_BIT = %ld\n",
                      (long) TARGET_FLOAT_BIT);
#endif
#ifdef TARGET_DOUBLE_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_DOUBLE_BIT = %ld\n",
                      (long) TARGET_DOUBLE_BIT);
#endif
#ifdef TARGET_LONG_DOUBLE_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_DOUBLE_BIT = %ld\n",
                      (long) TARGET_LONG_DOUBLE_BIT);
#endif
#ifdef TARGET_PTR_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_PTR_BIT = %ld\n",
                      (long) TARGET_PTR_BIT);
#endif
#ifdef TARGET_ADDR_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_ADDR_BIT = %ld\n",
                      (long) TARGET_ADDR_BIT);
#endif
#ifdef TARGET_BFD_VMA_BIT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_BFD_VMA_BIT = %ld\n",
                      (long) TARGET_BFD_VMA_BIT);
#endif
#ifdef IEEE_FLOAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: IEEE_FLOAT = %ld\n",
                      (long) IEEE_FLOAT);
#endif
#ifdef TARGET_READ_PC
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_READ_PC = 0x%08lx\n",
                        (long) current_gdbarch->read_pc
                        /*TARGET_READ_PC ()*/);
#endif
#ifdef TARGET_WRITE_PC
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_WRITE_PC = 0x%08lx\n",
                        (long) current_gdbarch->write_pc
                        /*TARGET_WRITE_PC ()*/);
#endif
#ifdef TARGET_READ_FP
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_READ_FP = 0x%08lx\n",
                        (long) current_gdbarch->read_fp
                        /*TARGET_READ_FP ()*/);
#endif
#ifdef TARGET_WRITE_FP
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_WRITE_FP = 0x%08lx\n",
                        (long) current_gdbarch->write_fp
                        /*TARGET_WRITE_FP ()*/);
#endif
#ifdef TARGET_READ_SP
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_READ_SP = 0x%08lx\n",
                        (long) current_gdbarch->read_sp
                        /*TARGET_READ_SP ()*/);
#endif
#ifdef TARGET_WRITE_SP
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: TARGET_WRITE_SP = 0x%08lx\n",
                        (long) current_gdbarch->write_sp
                        /*TARGET_WRITE_SP ()*/);
#endif
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: register_read = 0x%08lx\n",
                        (long) current_gdbarch->register_read);
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: register_write = 0x%08lx\n",
                        (long) current_gdbarch->register_write);
#ifdef NUM_REGS
  fprintf_unfiltered (file,
                      "gdbarch_dump: NUM_REGS = %ld\n",
                      (long) NUM_REGS);
#endif
#ifdef NUM_PSEUDO_REGS
  fprintf_unfiltered (file,
                      "gdbarch_dump: NUM_PSEUDO_REGS = %ld\n",
                      (long) NUM_PSEUDO_REGS);
#endif
#ifdef SP_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: SP_REGNUM = %ld\n",
                      (long) SP_REGNUM);
#endif
#ifdef FP_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: FP_REGNUM = %ld\n",
                      (long) FP_REGNUM);
#endif
#ifdef PC_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: PC_REGNUM = %ld\n",
                      (long) PC_REGNUM);
#endif
#ifdef FP0_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: FP0_REGNUM = %ld\n",
                      (long) FP0_REGNUM);
#endif
#ifdef NPC_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: NPC_REGNUM = %ld\n",
                      (long) NPC_REGNUM);
#endif
#ifdef NNPC_REGNUM
  fprintf_unfiltered (file,
                      "gdbarch_dump: NNPC_REGNUM = %ld\n",
                      (long) NNPC_REGNUM);
#endif
#ifdef STAB_REG_TO_REGNUM
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: STAB_REG_TO_REGNUM = 0x%08lx\n",
                        (long) current_gdbarch->stab_reg_to_regnum
                        /*STAB_REG_TO_REGNUM ()*/);
#endif
#ifdef ECOFF_REG_TO_REGNUM
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: ECOFF_REG_TO_REGNUM = 0x%08lx\n",
                        (long) current_gdbarch->ecoff_reg_to_regnum
                        /*ECOFF_REG_TO_REGNUM ()*/);
#endif
#ifdef DWARF_REG_TO_REGNUM
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: DWARF_REG_TO_REGNUM = 0x%08lx\n",
                        (long) current_gdbarch->dwarf_reg_to_regnum
                        /*DWARF_REG_TO_REGNUM ()*/);
#endif
#ifdef SDB_REG_TO_REGNUM
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: SDB_REG_TO_REGNUM = 0x%08lx\n",
                        (long) current_gdbarch->sdb_reg_to_regnum
                        /*SDB_REG_TO_REGNUM ()*/);
#endif
#ifdef DWARF2_REG_TO_REGNUM
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: DWARF2_REG_TO_REGNUM = 0x%08lx\n",
                        (long) current_gdbarch->dwarf2_reg_to_regnum
                        /*DWARF2_REG_TO_REGNUM ()*/);
#endif
#ifdef REGISTER_NAME
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_NAME = 0x%08lx\n",
                        (long) current_gdbarch->register_name
                        /*REGISTER_NAME ()*/);
#endif
#ifdef REGISTER_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: REGISTER_SIZE = %ld\n",
                      (long) REGISTER_SIZE);
#endif
#ifdef REGISTER_BYTES
  fprintf_unfiltered (file,
                      "gdbarch_dump: REGISTER_BYTES = %ld\n",
                      (long) REGISTER_BYTES);
#endif
#ifdef REGISTER_BYTE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_BYTE = 0x%08lx\n",
                        (long) current_gdbarch->register_byte
                        /*REGISTER_BYTE ()*/);
#endif
#ifdef REGISTER_RAW_SIZE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_RAW_SIZE = 0x%08lx\n",
                        (long) current_gdbarch->register_raw_size
                        /*REGISTER_RAW_SIZE ()*/);
#endif
#ifdef MAX_REGISTER_RAW_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: MAX_REGISTER_RAW_SIZE = %ld\n",
                      (long) MAX_REGISTER_RAW_SIZE);
#endif
#ifdef REGISTER_VIRTUAL_SIZE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_VIRTUAL_SIZE = 0x%08lx\n",
                        (long) current_gdbarch->register_virtual_size
                        /*REGISTER_VIRTUAL_SIZE ()*/);
#endif
#ifdef MAX_REGISTER_VIRTUAL_SIZE
  fprintf_unfiltered (file,
                      "gdbarch_dump: MAX_REGISTER_VIRTUAL_SIZE = %ld\n",
                      (long) MAX_REGISTER_VIRTUAL_SIZE);
#endif
#ifdef REGISTER_VIRTUAL_TYPE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_VIRTUAL_TYPE = 0x%08lx\n",
                        (long) current_gdbarch->register_virtual_type
                        /*REGISTER_VIRTUAL_TYPE ()*/);
#endif
#ifdef DO_REGISTERS_INFO
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: DO_REGISTERS_INFO = 0x%08lx\n",
                        (long) current_gdbarch->do_registers_info
                        /*DO_REGISTERS_INFO ()*/);
#endif
#ifdef REGISTER_SIM_REGNO
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_SIM_REGNO = 0x%08lx\n",
                        (long) current_gdbarch->register_sim_regno
                        /*REGISTER_SIM_REGNO ()*/);
#endif
#ifdef REGISTER_BYTES_OK
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_BYTES_OK = 0x%08lx\n",
                        (long) current_gdbarch->register_bytes_ok
                        /*REGISTER_BYTES_OK ()*/);
#endif
#ifdef CANNOT_FETCH_REGISTER
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CANNOT_FETCH_REGISTER = 0x%08lx\n",
                        (long) current_gdbarch->cannot_fetch_register
                        /*CANNOT_FETCH_REGISTER ()*/);
#endif
#ifdef CANNOT_STORE_REGISTER
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CANNOT_STORE_REGISTER = 0x%08lx\n",
                        (long) current_gdbarch->cannot_store_register
                        /*CANNOT_STORE_REGISTER ()*/);
#endif
#ifdef USE_GENERIC_DUMMY_FRAMES
  fprintf_unfiltered (file,
                      "gdbarch_dump: USE_GENERIC_DUMMY_FRAMES = %ld\n",
                      (long) USE_GENERIC_DUMMY_FRAMES);
#endif
#ifdef CALL_DUMMY_LOCATION
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_LOCATION = %ld\n",
                      (long) CALL_DUMMY_LOCATION);
#endif
#ifdef CALL_DUMMY_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CALL_DUMMY_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->call_dummy_address
                        /*CALL_DUMMY_ADDRESS ()*/);
#endif
#ifdef CALL_DUMMY_START_OFFSET
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_START_OFFSET = 0x%08lx\n",
                      (long) CALL_DUMMY_START_OFFSET);
#endif
#ifdef CALL_DUMMY_BREAKPOINT_OFFSET
  if (CALL_DUMMY_BREAKPOINT_OFFSET_P)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CALL_DUMMY_BREAKPOINT_OFFSET = 0x%08lx\n",
                        (long) CALL_DUMMY_BREAKPOINT_OFFSET);
#endif
#ifdef CALL_DUMMY_BREAKPOINT_OFFSET_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_BREAKPOINT_OFFSET_P = %ld\n",
                      (long) CALL_DUMMY_BREAKPOINT_OFFSET_P);
#endif
#ifdef CALL_DUMMY_LENGTH
  if (CALL_DUMMY_LOCATION == BEFORE_TEXT_END || CALL_DUMMY_LOCATION == AFTER_TEXT_END)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CALL_DUMMY_LENGTH = %ld\n",
                        (long) CALL_DUMMY_LENGTH);
#endif
#ifdef PC_IN_CALL_DUMMY
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: PC_IN_CALL_DUMMY = 0x%08lx\n",
                        (long) current_gdbarch->pc_in_call_dummy
                        /*PC_IN_CALL_DUMMY ()*/);
#endif
#ifdef CALL_DUMMY_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_P = %ld\n",
                      (long) CALL_DUMMY_P);
#endif
#ifdef CALL_DUMMY_WORDS
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_WORDS = 0x%08lx\n",
                      (long) CALL_DUMMY_WORDS);
#endif
#ifdef SIZEOF_CALL_DUMMY_WORDS
  fprintf_unfiltered (file,
                      "gdbarch_dump: SIZEOF_CALL_DUMMY_WORDS = 0x%08lx\n",
                      (long) SIZEOF_CALL_DUMMY_WORDS);
#endif
#ifdef CALL_DUMMY_STACK_ADJUST_P
  fprintf_unfiltered (file,
                      "gdbarch_dump: CALL_DUMMY_STACK_ADJUST_P = 0x%08lx\n",
                      (long) CALL_DUMMY_STACK_ADJUST_P);
#endif
#ifdef CALL_DUMMY_STACK_ADJUST
  if (CALL_DUMMY_STACK_ADJUST_P)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CALL_DUMMY_STACK_ADJUST = 0x%08lx\n",
                        (long) CALL_DUMMY_STACK_ADJUST);
#endif
#ifdef FIX_CALL_DUMMY
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FIX_CALL_DUMMY = 0x%08lx\n",
                        (long) current_gdbarch->fix_call_dummy
                        /*FIX_CALL_DUMMY ()*/);
#endif
#ifdef INIT_FRAME_PC_FIRST
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: INIT_FRAME_PC_FIRST = 0x%08lx\n",
                        (long) current_gdbarch->init_frame_pc_first
                        /*INIT_FRAME_PC_FIRST ()*/);
#endif
#ifdef INIT_FRAME_PC
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: INIT_FRAME_PC = 0x%08lx\n",
                        (long) current_gdbarch->init_frame_pc
                        /*INIT_FRAME_PC ()*/);
#endif
#ifdef BELIEVE_PCC_PROMOTION
  fprintf_unfiltered (file,
                      "gdbarch_dump: BELIEVE_PCC_PROMOTION = %ld\n",
                      (long) BELIEVE_PCC_PROMOTION);
#endif
#ifdef BELIEVE_PCC_PROMOTION_TYPE
  fprintf_unfiltered (file,
                      "gdbarch_dump: BELIEVE_PCC_PROMOTION_TYPE = %ld\n",
                      (long) BELIEVE_PCC_PROMOTION_TYPE);
#endif
#ifdef COERCE_FLOAT_TO_DOUBLE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: COERCE_FLOAT_TO_DOUBLE = 0x%08lx\n",
                        (long) current_gdbarch->coerce_float_to_double
                        /*COERCE_FLOAT_TO_DOUBLE ()*/);
#endif
#ifdef GET_SAVED_REGISTER
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: GET_SAVED_REGISTER = 0x%08lx\n",
                        (long) current_gdbarch->get_saved_register
                        /*GET_SAVED_REGISTER ()*/);
#endif
#ifdef REGISTER_CONVERTIBLE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_CONVERTIBLE = 0x%08lx\n",
                        (long) current_gdbarch->register_convertible
                        /*REGISTER_CONVERTIBLE ()*/);
#endif
#ifdef REGISTER_CONVERT_TO_VIRTUAL
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_CONVERT_TO_VIRTUAL = 0x%08lx\n",
                        (long) current_gdbarch->register_convert_to_virtual
                        /*REGISTER_CONVERT_TO_VIRTUAL ()*/);
#endif
#ifdef REGISTER_CONVERT_TO_RAW
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REGISTER_CONVERT_TO_RAW = 0x%08lx\n",
                        (long) current_gdbarch->register_convert_to_raw
                        /*REGISTER_CONVERT_TO_RAW ()*/);
#endif
#ifdef FETCH_PSEUDO_REGISTER
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FETCH_PSEUDO_REGISTER = 0x%08lx\n",
                        (long) current_gdbarch->fetch_pseudo_register
                        /*FETCH_PSEUDO_REGISTER ()*/);
#endif
#ifdef STORE_PSEUDO_REGISTER
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: STORE_PSEUDO_REGISTER = 0x%08lx\n",
                        (long) current_gdbarch->store_pseudo_register
                        /*STORE_PSEUDO_REGISTER ()*/);
#endif
#ifdef POINTER_TO_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: POINTER_TO_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->pointer_to_address
                        /*POINTER_TO_ADDRESS ()*/);
#endif
#ifdef ADDRESS_TO_POINTER
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: ADDRESS_TO_POINTER = 0x%08lx\n",
                        (long) current_gdbarch->address_to_pointer
                        /*ADDRESS_TO_POINTER ()*/);
#endif
#ifdef RETURN_VALUE_ON_STACK
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: RETURN_VALUE_ON_STACK = 0x%08lx\n",
                        (long) current_gdbarch->return_value_on_stack
                        /*RETURN_VALUE_ON_STACK ()*/);
#endif
#ifdef EXTRACT_RETURN_VALUE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: EXTRACT_RETURN_VALUE = 0x%08lx\n",
                        (long) current_gdbarch->extract_return_value
                        /*EXTRACT_RETURN_VALUE ()*/);
#endif
#ifdef PUSH_ARGUMENTS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: PUSH_ARGUMENTS = 0x%08lx\n",
                        (long) current_gdbarch->push_arguments
                        /*PUSH_ARGUMENTS ()*/);
#endif
#ifdef PUSH_DUMMY_FRAME
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: PUSH_DUMMY_FRAME = 0x%08lx\n",
                        (long) current_gdbarch->push_dummy_frame
                        /*PUSH_DUMMY_FRAME ()*/);
#endif
#ifdef PUSH_RETURN_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: PUSH_RETURN_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->push_return_address
                        /*PUSH_RETURN_ADDRESS ()*/);
#endif
#ifdef POP_FRAME
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: POP_FRAME = 0x%08lx\n",
                        (long) current_gdbarch->pop_frame
                        /*POP_FRAME ()*/);
#endif
#ifdef STORE_STRUCT_RETURN
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: STORE_STRUCT_RETURN = 0x%08lx\n",
                        (long) current_gdbarch->store_struct_return
                        /*STORE_STRUCT_RETURN ()*/);
#endif
#ifdef STORE_RETURN_VALUE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: STORE_RETURN_VALUE = 0x%08lx\n",
                        (long) current_gdbarch->store_return_value
                        /*STORE_RETURN_VALUE ()*/);
#endif
#ifdef EXTRACT_STRUCT_VALUE_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: EXTRACT_STRUCT_VALUE_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->extract_struct_value_address
                        /*EXTRACT_STRUCT_VALUE_ADDRESS ()*/);
#endif
#ifdef USE_STRUCT_CONVENTION
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: USE_STRUCT_CONVENTION = 0x%08lx\n",
                        (long) current_gdbarch->use_struct_convention
                        /*USE_STRUCT_CONVENTION ()*/);
#endif
#ifdef FRAME_INIT_SAVED_REGS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_INIT_SAVED_REGS = 0x%08lx\n",
                        (long) current_gdbarch->frame_init_saved_regs
                        /*FRAME_INIT_SAVED_REGS ()*/);
#endif
#ifdef INIT_EXTRA_FRAME_INFO
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: INIT_EXTRA_FRAME_INFO = 0x%08lx\n",
                        (long) current_gdbarch->init_extra_frame_info
                        /*INIT_EXTRA_FRAME_INFO ()*/);
#endif
#ifdef SKIP_PROLOGUE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: SKIP_PROLOGUE = 0x%08lx\n",
                        (long) current_gdbarch->skip_prologue
                        /*SKIP_PROLOGUE ()*/);
#endif
#ifdef PROLOGUE_FRAMELESS_P
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: PROLOGUE_FRAMELESS_P = 0x%08lx\n",
                        (long) current_gdbarch->prologue_frameless_p
                        /*PROLOGUE_FRAMELESS_P ()*/);
#endif
#ifdef INNER_THAN
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: INNER_THAN = 0x%08lx\n",
                        (long) current_gdbarch->inner_than
                        /*INNER_THAN ()*/);
#endif
#ifdef BREAKPOINT_FROM_PC
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: BREAKPOINT_FROM_PC = 0x%08lx\n",
                        (long) current_gdbarch->breakpoint_from_pc
                        /*BREAKPOINT_FROM_PC ()*/);
#endif
#ifdef MEMORY_INSERT_BREAKPOINT
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: MEMORY_INSERT_BREAKPOINT = 0x%08lx\n",
                        (long) current_gdbarch->memory_insert_breakpoint
                        /*MEMORY_INSERT_BREAKPOINT ()*/);
#endif
#ifdef MEMORY_REMOVE_BREAKPOINT
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: MEMORY_REMOVE_BREAKPOINT = 0x%08lx\n",
                        (long) current_gdbarch->memory_remove_breakpoint
                        /*MEMORY_REMOVE_BREAKPOINT ()*/);
#endif
#ifdef DECR_PC_AFTER_BREAK
  fprintf_unfiltered (file,
                      "gdbarch_dump: DECR_PC_AFTER_BREAK = %ld\n",
                      (long) DECR_PC_AFTER_BREAK);
#endif
#ifdef PREPARE_TO_PROCEED
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: PREPARE_TO_PROCEED = 0x%08lx\n",
                        (long) current_gdbarch->prepare_to_proceed
                        /*PREPARE_TO_PROCEED ()*/);
#endif
#ifdef FUNCTION_START_OFFSET
  fprintf_unfiltered (file,
                      "gdbarch_dump: FUNCTION_START_OFFSET = %ld\n",
                      (long) FUNCTION_START_OFFSET);
#endif
#ifdef REMOTE_TRANSLATE_XFER_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REMOTE_TRANSLATE_XFER_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->remote_translate_xfer_address
                        /*REMOTE_TRANSLATE_XFER_ADDRESS ()*/);
#endif
#ifdef FRAME_ARGS_SKIP
  fprintf_unfiltered (file,
                      "gdbarch_dump: FRAME_ARGS_SKIP = %ld\n",
                      (long) FRAME_ARGS_SKIP);
#endif
#ifdef FRAMELESS_FUNCTION_INVOCATION
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAMELESS_FUNCTION_INVOCATION = 0x%08lx\n",
                        (long) current_gdbarch->frameless_function_invocation
                        /*FRAMELESS_FUNCTION_INVOCATION ()*/);
#endif
#ifdef FRAME_CHAIN
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_CHAIN = 0x%08lx\n",
                        (long) current_gdbarch->frame_chain
                        /*FRAME_CHAIN ()*/);
#endif
#ifdef FRAME_CHAIN_VALID
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_CHAIN_VALID = 0x%08lx\n",
                        (long) current_gdbarch->frame_chain_valid
                        /*FRAME_CHAIN_VALID ()*/);
#endif
#ifdef FRAME_SAVED_PC
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_SAVED_PC = 0x%08lx\n",
                        (long) current_gdbarch->frame_saved_pc
                        /*FRAME_SAVED_PC ()*/);
#endif
#ifdef FRAME_ARGS_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_ARGS_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->frame_args_address
                        /*FRAME_ARGS_ADDRESS ()*/);
#endif
#ifdef FRAME_LOCALS_ADDRESS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_LOCALS_ADDRESS = 0x%08lx\n",
                        (long) current_gdbarch->frame_locals_address
                        /*FRAME_LOCALS_ADDRESS ()*/);
#endif
#ifdef SAVED_PC_AFTER_CALL
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: SAVED_PC_AFTER_CALL = 0x%08lx\n",
                        (long) current_gdbarch->saved_pc_after_call
                        /*SAVED_PC_AFTER_CALL ()*/);
#endif
#ifdef FRAME_NUM_ARGS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: FRAME_NUM_ARGS = 0x%08lx\n",
                        (long) current_gdbarch->frame_num_args
                        /*FRAME_NUM_ARGS ()*/);
#endif
#ifdef STACK_ALIGN
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: STACK_ALIGN = 0x%08lx\n",
                        (long) current_gdbarch->stack_align
                        /*STACK_ALIGN ()*/);
#endif
#ifdef EXTRA_STACK_ALIGNMENT_NEEDED
  fprintf_unfiltered (file,
                      "gdbarch_dump: EXTRA_STACK_ALIGNMENT_NEEDED = %ld\n",
                      (long) EXTRA_STACK_ALIGNMENT_NEEDED);
#endif
#ifdef REG_STRUCT_HAS_ADDR
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: REG_STRUCT_HAS_ADDR = 0x%08lx\n",
                        (long) current_gdbarch->reg_struct_has_addr
                        /*REG_STRUCT_HAS_ADDR ()*/);
#endif
#ifdef SAVE_DUMMY_FRAME_TOS
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: SAVE_DUMMY_FRAME_TOS = 0x%08lx\n",
                        (long) current_gdbarch->save_dummy_frame_tos
                        /*SAVE_DUMMY_FRAME_TOS ()*/);
#endif
#ifdef PARM_BOUNDARY
  fprintf_unfiltered (file,
                      "gdbarch_dump: PARM_BOUNDARY = %ld\n",
                      (long) PARM_BOUNDARY);
#endif
#ifdef TARGET_FLOAT_FORMAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_FLOAT_FORMAT = %ld\n",
                      (long) TARGET_FLOAT_FORMAT);
#endif
#ifdef TARGET_DOUBLE_FORMAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_DOUBLE_FORMAT = %ld\n",
                      (long) TARGET_DOUBLE_FORMAT);
#endif
#ifdef TARGET_LONG_DOUBLE_FORMAT
  fprintf_unfiltered (file,
                      "gdbarch_dump: TARGET_LONG_DOUBLE_FORMAT = %ld\n",
                      (long) TARGET_LONG_DOUBLE_FORMAT);
#endif
#ifdef CONVERT_FROM_FUNC_PTR_ADDR
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: CONVERT_FROM_FUNC_PTR_ADDR = 0x%08lx\n",
                        (long) current_gdbarch->convert_from_func_ptr_addr
                        /*CONVERT_FROM_FUNC_PTR_ADDR ()*/);
#endif
#ifdef ADDR_BITS_REMOVE
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: ADDR_BITS_REMOVE = 0x%08lx\n",
                        (long) current_gdbarch->addr_bits_remove
                        /*ADDR_BITS_REMOVE ()*/);
#endif
#ifdef SOFTWARE_SINGLE_STEP
  if (GDB_MULTI_ARCH)
    fprintf_unfiltered (file,
                        "gdbarch_dump: SOFTWARE_SINGLE_STEP = 0x%08lx\n",
                        (long) current_gdbarch->software_single_step
                        /*SOFTWARE_SINGLE_STEP ()*/);
#endif
  if (current_gdbarch->dump_tdep != NULL)
    current_gdbarch->dump_tdep (current_gdbarch, file);
}

struct gdbarch_tdep *
gdbarch_tdep (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_tdep called\n");
  return gdbarch->tdep;
}


const struct bfd_arch_info *
gdbarch_bfd_arch_info (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_bfd_arch_info called\n");
  return gdbarch->bfd_arch_info;
}

int
gdbarch_byte_order (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_byte_order called\n");
  return gdbarch->byte_order;
}

int
gdbarch_short_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of short_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_short_bit called\n");
  return gdbarch->short_bit;
}

void
set_gdbarch_short_bit (struct gdbarch *gdbarch,
                       int short_bit)
{
  gdbarch->short_bit = short_bit;
}

int
gdbarch_int_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of int_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_int_bit called\n");
  return gdbarch->int_bit;
}

void
set_gdbarch_int_bit (struct gdbarch *gdbarch,
                     int int_bit)
{
  gdbarch->int_bit = int_bit;
}

int
gdbarch_long_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of long_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_long_bit called\n");
  return gdbarch->long_bit;
}

void
set_gdbarch_long_bit (struct gdbarch *gdbarch,
                      int long_bit)
{
  gdbarch->long_bit = long_bit;
}

int
gdbarch_long_long_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of long_long_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_long_long_bit called\n");
  return gdbarch->long_long_bit;
}

void
set_gdbarch_long_long_bit (struct gdbarch *gdbarch,
                           int long_long_bit)
{
  gdbarch->long_long_bit = long_long_bit;
}

int
gdbarch_float_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of float_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_float_bit called\n");
  return gdbarch->float_bit;
}

void
set_gdbarch_float_bit (struct gdbarch *gdbarch,
                       int float_bit)
{
  gdbarch->float_bit = float_bit;
}

int
gdbarch_double_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of double_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_double_bit called\n");
  return gdbarch->double_bit;
}

void
set_gdbarch_double_bit (struct gdbarch *gdbarch,
                        int double_bit)
{
  gdbarch->double_bit = double_bit;
}

int
gdbarch_long_double_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of long_double_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_long_double_bit called\n");
  return gdbarch->long_double_bit;
}

void
set_gdbarch_long_double_bit (struct gdbarch *gdbarch,
                             int long_double_bit)
{
  gdbarch->long_double_bit = long_double_bit;
}

int
gdbarch_ptr_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of ptr_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_ptr_bit called\n");
  return gdbarch->ptr_bit;
}

void
set_gdbarch_ptr_bit (struct gdbarch *gdbarch,
                     int ptr_bit)
{
  gdbarch->ptr_bit = ptr_bit;
}

int
gdbarch_addr_bit (struct gdbarch *gdbarch)
{
  if (gdbarch->addr_bit == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_addr_bit invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_addr_bit called\n");
  return gdbarch->addr_bit;
}

void
set_gdbarch_addr_bit (struct gdbarch *gdbarch,
                      int addr_bit)
{
  gdbarch->addr_bit = addr_bit;
}

int
gdbarch_bfd_vma_bit (struct gdbarch *gdbarch)
{
  /* Skip verify of bfd_vma_bit, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_bfd_vma_bit called\n");
  return gdbarch->bfd_vma_bit;
}

void
set_gdbarch_bfd_vma_bit (struct gdbarch *gdbarch,
                         int bfd_vma_bit)
{
  gdbarch->bfd_vma_bit = bfd_vma_bit;
}

int
gdbarch_ieee_float (struct gdbarch *gdbarch)
{
  /* Skip verify of ieee_float, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_ieee_float called\n");
  return gdbarch->ieee_float;
}

void
set_gdbarch_ieee_float (struct gdbarch *gdbarch,
                        int ieee_float)
{
  gdbarch->ieee_float = ieee_float;
}

CORE_ADDR
gdbarch_read_pc (struct gdbarch *gdbarch, ptid_t ptid)
{
  if (gdbarch->read_pc == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_read_pc invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_read_pc called\n");
  return gdbarch->read_pc (ptid);
}

void
set_gdbarch_read_pc (struct gdbarch *gdbarch,
                     gdbarch_read_pc_ftype read_pc)
{
  gdbarch->read_pc = read_pc;
}

void
gdbarch_write_pc (struct gdbarch *gdbarch, CORE_ADDR val, ptid_t ptid)
{
  if (gdbarch->write_pc == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_write_pc invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_write_pc called\n");
  gdbarch->write_pc (val, ptid);
}

void
set_gdbarch_write_pc (struct gdbarch *gdbarch,
                      gdbarch_write_pc_ftype write_pc)
{
  gdbarch->write_pc = write_pc;
}

CORE_ADDR
gdbarch_read_fp (struct gdbarch *gdbarch)
{
  if (gdbarch->read_fp == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_read_fp invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_read_fp called\n");
  return gdbarch->read_fp ();
}

void
set_gdbarch_read_fp (struct gdbarch *gdbarch,
                     gdbarch_read_fp_ftype read_fp)
{
  gdbarch->read_fp = read_fp;
}

void
gdbarch_write_fp (struct gdbarch *gdbarch, CORE_ADDR val)
{
  if (gdbarch->write_fp == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_write_fp invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_write_fp called\n");
  gdbarch->write_fp (val);
}

void
set_gdbarch_write_fp (struct gdbarch *gdbarch,
                      gdbarch_write_fp_ftype write_fp)
{
  gdbarch->write_fp = write_fp;
}

CORE_ADDR
gdbarch_read_sp (struct gdbarch *gdbarch)
{
  if (gdbarch->read_sp == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_read_sp invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_read_sp called\n");
  return gdbarch->read_sp ();
}

void
set_gdbarch_read_sp (struct gdbarch *gdbarch,
                     gdbarch_read_sp_ftype read_sp)
{
  gdbarch->read_sp = read_sp;
}

void
gdbarch_write_sp (struct gdbarch *gdbarch, CORE_ADDR val)
{
  if (gdbarch->write_sp == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_write_sp invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_write_sp called\n");
  gdbarch->write_sp (val);
}

void
set_gdbarch_write_sp (struct gdbarch *gdbarch,
                      gdbarch_write_sp_ftype write_sp)
{
  gdbarch->write_sp = write_sp;
}

int
gdbarch_register_read_p (struct gdbarch *gdbarch)
{
  return gdbarch->register_read != 0;
}

void
gdbarch_register_read (struct gdbarch *gdbarch, int regnum, char *buf)
{
  if (gdbarch->register_read == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_read invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_read called\n");
  gdbarch->register_read (gdbarch, regnum, buf);
}

void
set_gdbarch_register_read (struct gdbarch *gdbarch,
                           gdbarch_register_read_ftype register_read)
{
  gdbarch->register_read = register_read;
}

int
gdbarch_register_write_p (struct gdbarch *gdbarch)
{
  return gdbarch->register_write != 0;
}

void
gdbarch_register_write (struct gdbarch *gdbarch, int regnum, char *buf)
{
  if (gdbarch->register_write == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_write invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_write called\n");
  gdbarch->register_write (gdbarch, regnum, buf);
}

void
set_gdbarch_register_write (struct gdbarch *gdbarch,
                            gdbarch_register_write_ftype register_write)
{
  gdbarch->register_write = register_write;
}

int
gdbarch_num_regs (struct gdbarch *gdbarch)
{
  if (gdbarch->num_regs == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_num_regs invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_num_regs called\n");
  return gdbarch->num_regs;
}

void
set_gdbarch_num_regs (struct gdbarch *gdbarch,
                      int num_regs)
{
  gdbarch->num_regs = num_regs;
}

int
gdbarch_num_pseudo_regs (struct gdbarch *gdbarch)
{
  /* Skip verify of num_pseudo_regs, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_num_pseudo_regs called\n");
  return gdbarch->num_pseudo_regs;
}

void
set_gdbarch_num_pseudo_regs (struct gdbarch *gdbarch,
                             int num_pseudo_regs)
{
  gdbarch->num_pseudo_regs = num_pseudo_regs;
}

int
gdbarch_sp_regnum (struct gdbarch *gdbarch)
{
  if (gdbarch->sp_regnum == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_sp_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_sp_regnum called\n");
  return gdbarch->sp_regnum;
}

void
set_gdbarch_sp_regnum (struct gdbarch *gdbarch,
                       int sp_regnum)
{
  gdbarch->sp_regnum = sp_regnum;
}

int
gdbarch_fp_regnum (struct gdbarch *gdbarch)
{
  if (gdbarch->fp_regnum == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_fp_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_fp_regnum called\n");
  return gdbarch->fp_regnum;
}

void
set_gdbarch_fp_regnum (struct gdbarch *gdbarch,
                       int fp_regnum)
{
  gdbarch->fp_regnum = fp_regnum;
}

int
gdbarch_pc_regnum (struct gdbarch *gdbarch)
{
  if (gdbarch->pc_regnum == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_pc_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_pc_regnum called\n");
  return gdbarch->pc_regnum;
}

void
set_gdbarch_pc_regnum (struct gdbarch *gdbarch,
                       int pc_regnum)
{
  gdbarch->pc_regnum = pc_regnum;
}

int
gdbarch_fp0_regnum (struct gdbarch *gdbarch)
{
  /* Skip verify of fp0_regnum, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_fp0_regnum called\n");
  return gdbarch->fp0_regnum;
}

void
set_gdbarch_fp0_regnum (struct gdbarch *gdbarch,
                        int fp0_regnum)
{
  gdbarch->fp0_regnum = fp0_regnum;
}

int
gdbarch_npc_regnum (struct gdbarch *gdbarch)
{
  /* Skip verify of npc_regnum, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_npc_regnum called\n");
  return gdbarch->npc_regnum;
}

void
set_gdbarch_npc_regnum (struct gdbarch *gdbarch,
                        int npc_regnum)
{
  gdbarch->npc_regnum = npc_regnum;
}

int
gdbarch_nnpc_regnum (struct gdbarch *gdbarch)
{
  /* Skip verify of nnpc_regnum, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_nnpc_regnum called\n");
  return gdbarch->nnpc_regnum;
}

void
set_gdbarch_nnpc_regnum (struct gdbarch *gdbarch,
                         int nnpc_regnum)
{
  gdbarch->nnpc_regnum = nnpc_regnum;
}

int
gdbarch_stab_reg_to_regnum (struct gdbarch *gdbarch, int stab_regnr)
{
  if (gdbarch->stab_reg_to_regnum == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_stab_reg_to_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_stab_reg_to_regnum called\n");
  return gdbarch->stab_reg_to_regnum (stab_regnr);
}

void
set_gdbarch_stab_reg_to_regnum (struct gdbarch *gdbarch,
                                gdbarch_stab_reg_to_regnum_ftype stab_reg_to_regnum)
{
  gdbarch->stab_reg_to_regnum = stab_reg_to_regnum;
}

int
gdbarch_ecoff_reg_to_regnum (struct gdbarch *gdbarch, int ecoff_regnr)
{
  if (gdbarch->ecoff_reg_to_regnum == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_ecoff_reg_to_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_ecoff_reg_to_regnum called\n");
  return gdbarch->ecoff_reg_to_regnum (ecoff_regnr);
}

void
set_gdbarch_ecoff_reg_to_regnum (struct gdbarch *gdbarch,
                                 gdbarch_ecoff_reg_to_regnum_ftype ecoff_reg_to_regnum)
{
  gdbarch->ecoff_reg_to_regnum = ecoff_reg_to_regnum;
}

int
gdbarch_dwarf_reg_to_regnum (struct gdbarch *gdbarch, int dwarf_regnr)
{
  if (gdbarch->dwarf_reg_to_regnum == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_dwarf_reg_to_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_dwarf_reg_to_regnum called\n");
  return gdbarch->dwarf_reg_to_regnum (dwarf_regnr);
}

void
set_gdbarch_dwarf_reg_to_regnum (struct gdbarch *gdbarch,
                                 gdbarch_dwarf_reg_to_regnum_ftype dwarf_reg_to_regnum)
{
  gdbarch->dwarf_reg_to_regnum = dwarf_reg_to_regnum;
}

int
gdbarch_sdb_reg_to_regnum (struct gdbarch *gdbarch, int sdb_regnr)
{
  if (gdbarch->sdb_reg_to_regnum == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_sdb_reg_to_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_sdb_reg_to_regnum called\n");
  return gdbarch->sdb_reg_to_regnum (sdb_regnr);
}

void
set_gdbarch_sdb_reg_to_regnum (struct gdbarch *gdbarch,
                               gdbarch_sdb_reg_to_regnum_ftype sdb_reg_to_regnum)
{
  gdbarch->sdb_reg_to_regnum = sdb_reg_to_regnum;
}

int
gdbarch_dwarf2_reg_to_regnum (struct gdbarch *gdbarch, int dwarf2_regnr)
{
  if (gdbarch->dwarf2_reg_to_regnum == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_dwarf2_reg_to_regnum invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_dwarf2_reg_to_regnum called\n");
  return gdbarch->dwarf2_reg_to_regnum (dwarf2_regnr);
}

void
set_gdbarch_dwarf2_reg_to_regnum (struct gdbarch *gdbarch,
                                  gdbarch_dwarf2_reg_to_regnum_ftype dwarf2_reg_to_regnum)
{
  gdbarch->dwarf2_reg_to_regnum = dwarf2_reg_to_regnum;
}

char *
gdbarch_register_name (struct gdbarch *gdbarch, int regnr)
{
  if (gdbarch->register_name == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_name invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_name called\n");
  return gdbarch->register_name (regnr);
}

void
set_gdbarch_register_name (struct gdbarch *gdbarch,
                           gdbarch_register_name_ftype register_name)
{
  gdbarch->register_name = register_name;
}

int
gdbarch_register_size (struct gdbarch *gdbarch)
{
  if (gdbarch->register_size == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_size invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_size called\n");
  return gdbarch->register_size;
}

void
set_gdbarch_register_size (struct gdbarch *gdbarch,
                           int register_size)
{
  gdbarch->register_size = register_size;
}

int
gdbarch_register_bytes (struct gdbarch *gdbarch)
{
  if (gdbarch->register_bytes == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_bytes invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_bytes called\n");
  return gdbarch->register_bytes;
}

void
set_gdbarch_register_bytes (struct gdbarch *gdbarch,
                            int register_bytes)
{
  gdbarch->register_bytes = register_bytes;
}

int
gdbarch_register_byte (struct gdbarch *gdbarch, int reg_nr)
{
  if (gdbarch->register_byte == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_byte invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_byte called\n");
  return gdbarch->register_byte (reg_nr);
}

void
set_gdbarch_register_byte (struct gdbarch *gdbarch,
                           gdbarch_register_byte_ftype register_byte)
{
  gdbarch->register_byte = register_byte;
}

int
gdbarch_register_raw_size (struct gdbarch *gdbarch, int reg_nr)
{
  if (gdbarch->register_raw_size == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_raw_size invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_raw_size called\n");
  return gdbarch->register_raw_size (reg_nr);
}

void
set_gdbarch_register_raw_size (struct gdbarch *gdbarch,
                               gdbarch_register_raw_size_ftype register_raw_size)
{
  gdbarch->register_raw_size = register_raw_size;
}

int
gdbarch_max_register_raw_size (struct gdbarch *gdbarch)
{
  if (gdbarch->max_register_raw_size == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_max_register_raw_size invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_max_register_raw_size called\n");
  return gdbarch->max_register_raw_size;
}

void
set_gdbarch_max_register_raw_size (struct gdbarch *gdbarch,
                                   int max_register_raw_size)
{
  gdbarch->max_register_raw_size = max_register_raw_size;
}

int
gdbarch_register_virtual_size (struct gdbarch *gdbarch, int reg_nr)
{
  if (gdbarch->register_virtual_size == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_virtual_size invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_virtual_size called\n");
  return gdbarch->register_virtual_size (reg_nr);
}

void
set_gdbarch_register_virtual_size (struct gdbarch *gdbarch,
                                   gdbarch_register_virtual_size_ftype register_virtual_size)
{
  gdbarch->register_virtual_size = register_virtual_size;
}

int
gdbarch_max_register_virtual_size (struct gdbarch *gdbarch)
{
  if (gdbarch->max_register_virtual_size == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_max_register_virtual_size invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_max_register_virtual_size called\n");
  return gdbarch->max_register_virtual_size;
}

void
set_gdbarch_max_register_virtual_size (struct gdbarch *gdbarch,
                                       int max_register_virtual_size)
{
  gdbarch->max_register_virtual_size = max_register_virtual_size;
}

struct type *
gdbarch_register_virtual_type (struct gdbarch *gdbarch, int reg_nr)
{
  if (gdbarch->register_virtual_type == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_virtual_type invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_virtual_type called\n");
  return gdbarch->register_virtual_type (reg_nr);
}

void
set_gdbarch_register_virtual_type (struct gdbarch *gdbarch,
                                   gdbarch_register_virtual_type_ftype register_virtual_type)
{
  gdbarch->register_virtual_type = register_virtual_type;
}

void
gdbarch_do_registers_info (struct gdbarch *gdbarch, int reg_nr, int fpregs)
{
  if (gdbarch->do_registers_info == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_do_registers_info invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_do_registers_info called\n");
  gdbarch->do_registers_info (reg_nr, fpregs);
}

void
set_gdbarch_do_registers_info (struct gdbarch *gdbarch,
                               gdbarch_do_registers_info_ftype do_registers_info)
{
  gdbarch->do_registers_info = do_registers_info;
}

int
gdbarch_register_sim_regno (struct gdbarch *gdbarch, int reg_nr)
{
  if (gdbarch->register_sim_regno == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_sim_regno invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_sim_regno called\n");
  return gdbarch->register_sim_regno (reg_nr);
}

void
set_gdbarch_register_sim_regno (struct gdbarch *gdbarch,
                                gdbarch_register_sim_regno_ftype register_sim_regno)
{
  gdbarch->register_sim_regno = register_sim_regno;
}

int
gdbarch_register_bytes_ok_p (struct gdbarch *gdbarch)
{
  return gdbarch->register_bytes_ok != 0;
}

int
gdbarch_register_bytes_ok (struct gdbarch *gdbarch, long nr_bytes)
{
  if (gdbarch->register_bytes_ok == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_bytes_ok invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_bytes_ok called\n");
  return gdbarch->register_bytes_ok (nr_bytes);
}

void
set_gdbarch_register_bytes_ok (struct gdbarch *gdbarch,
                               gdbarch_register_bytes_ok_ftype register_bytes_ok)
{
  gdbarch->register_bytes_ok = register_bytes_ok;
}

int
gdbarch_cannot_fetch_register (struct gdbarch *gdbarch, int regnum)
{
  if (gdbarch->cannot_fetch_register == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_cannot_fetch_register invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_cannot_fetch_register called\n");
  return gdbarch->cannot_fetch_register (regnum);
}

void
set_gdbarch_cannot_fetch_register (struct gdbarch *gdbarch,
                                   gdbarch_cannot_fetch_register_ftype cannot_fetch_register)
{
  gdbarch->cannot_fetch_register = cannot_fetch_register;
}

int
gdbarch_cannot_store_register (struct gdbarch *gdbarch, int regnum)
{
  if (gdbarch->cannot_store_register == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_cannot_store_register invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_cannot_store_register called\n");
  return gdbarch->cannot_store_register (regnum);
}

void
set_gdbarch_cannot_store_register (struct gdbarch *gdbarch,
                                   gdbarch_cannot_store_register_ftype cannot_store_register)
{
  gdbarch->cannot_store_register = cannot_store_register;
}

int
gdbarch_use_generic_dummy_frames (struct gdbarch *gdbarch)
{
  if (gdbarch->use_generic_dummy_frames == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_use_generic_dummy_frames invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_use_generic_dummy_frames called\n");
  return gdbarch->use_generic_dummy_frames;
}

void
set_gdbarch_use_generic_dummy_frames (struct gdbarch *gdbarch,
                                      int use_generic_dummy_frames)
{
  gdbarch->use_generic_dummy_frames = use_generic_dummy_frames;
}

int
gdbarch_call_dummy_location (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_location == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_location invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_location called\n");
  return gdbarch->call_dummy_location;
}

void
set_gdbarch_call_dummy_location (struct gdbarch *gdbarch,
                                 int call_dummy_location)
{
  gdbarch->call_dummy_location = call_dummy_location;
}

CORE_ADDR
gdbarch_call_dummy_address (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_address called\n");
  return gdbarch->call_dummy_address ();
}

void
set_gdbarch_call_dummy_address (struct gdbarch *gdbarch,
                                gdbarch_call_dummy_address_ftype call_dummy_address)
{
  gdbarch->call_dummy_address = call_dummy_address;
}

CORE_ADDR
gdbarch_call_dummy_start_offset (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_start_offset == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_start_offset invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_start_offset called\n");
  return gdbarch->call_dummy_start_offset;
}

void
set_gdbarch_call_dummy_start_offset (struct gdbarch *gdbarch,
                                     CORE_ADDR call_dummy_start_offset)
{
  gdbarch->call_dummy_start_offset = call_dummy_start_offset;
}

CORE_ADDR
gdbarch_call_dummy_breakpoint_offset (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_breakpoint_offset_p && gdbarch->call_dummy_breakpoint_offset == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_breakpoint_offset invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_breakpoint_offset called\n");
  return gdbarch->call_dummy_breakpoint_offset;
}

void
set_gdbarch_call_dummy_breakpoint_offset (struct gdbarch *gdbarch,
                                          CORE_ADDR call_dummy_breakpoint_offset)
{
  gdbarch->call_dummy_breakpoint_offset = call_dummy_breakpoint_offset;
}

int
gdbarch_call_dummy_breakpoint_offset_p (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_breakpoint_offset_p == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_breakpoint_offset_p invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_breakpoint_offset_p called\n");
  return gdbarch->call_dummy_breakpoint_offset_p;
}

void
set_gdbarch_call_dummy_breakpoint_offset_p (struct gdbarch *gdbarch,
                                            int call_dummy_breakpoint_offset_p)
{
  gdbarch->call_dummy_breakpoint_offset_p = call_dummy_breakpoint_offset_p;
}

int
gdbarch_call_dummy_length (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_length == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_length invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_length called\n");
  return gdbarch->call_dummy_length;
}

void
set_gdbarch_call_dummy_length (struct gdbarch *gdbarch,
                               int call_dummy_length)
{
  gdbarch->call_dummy_length = call_dummy_length;
}

int
gdbarch_pc_in_call_dummy (struct gdbarch *gdbarch, CORE_ADDR pc, CORE_ADDR sp, CORE_ADDR frame_address)
{
  if (gdbarch->pc_in_call_dummy == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_pc_in_call_dummy invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_pc_in_call_dummy called\n");
  return gdbarch->pc_in_call_dummy (pc, sp, frame_address);
}

void
set_gdbarch_pc_in_call_dummy (struct gdbarch *gdbarch,
                              gdbarch_pc_in_call_dummy_ftype pc_in_call_dummy)
{
  gdbarch->pc_in_call_dummy = pc_in_call_dummy;
}

int
gdbarch_call_dummy_p (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_p == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_p invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_p called\n");
  return gdbarch->call_dummy_p;
}

void
set_gdbarch_call_dummy_p (struct gdbarch *gdbarch,
                          int call_dummy_p)
{
  gdbarch->call_dummy_p = call_dummy_p;
}

LONGEST *
gdbarch_call_dummy_words (struct gdbarch *gdbarch)
{
  /* Skip verify of call_dummy_words, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_words called\n");
  return gdbarch->call_dummy_words;
}

void
set_gdbarch_call_dummy_words (struct gdbarch *gdbarch,
                              LONGEST * call_dummy_words)
{
  gdbarch->call_dummy_words = call_dummy_words;
}

int
gdbarch_sizeof_call_dummy_words (struct gdbarch *gdbarch)
{
  /* Skip verify of sizeof_call_dummy_words, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_sizeof_call_dummy_words called\n");
  return gdbarch->sizeof_call_dummy_words;
}

void
set_gdbarch_sizeof_call_dummy_words (struct gdbarch *gdbarch,
                                     int sizeof_call_dummy_words)
{
  gdbarch->sizeof_call_dummy_words = sizeof_call_dummy_words;
}

int
gdbarch_call_dummy_stack_adjust_p (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_stack_adjust_p == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_stack_adjust_p invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_stack_adjust_p called\n");
  return gdbarch->call_dummy_stack_adjust_p;
}

void
set_gdbarch_call_dummy_stack_adjust_p (struct gdbarch *gdbarch,
                                       int call_dummy_stack_adjust_p)
{
  gdbarch->call_dummy_stack_adjust_p = call_dummy_stack_adjust_p;
}

int
gdbarch_call_dummy_stack_adjust (struct gdbarch *gdbarch)
{
  if (gdbarch->call_dummy_stack_adjust_p && gdbarch->call_dummy_stack_adjust == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_call_dummy_stack_adjust invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_call_dummy_stack_adjust called\n");
  return gdbarch->call_dummy_stack_adjust;
}

void
set_gdbarch_call_dummy_stack_adjust (struct gdbarch *gdbarch,
                                     int call_dummy_stack_adjust)
{
  gdbarch->call_dummy_stack_adjust = call_dummy_stack_adjust;
}

void
gdbarch_fix_call_dummy (struct gdbarch *gdbarch, char *dummy, CORE_ADDR pc, CORE_ADDR fun, int nargs, struct value **args, struct type *type, int gcc_p)
{
  if (gdbarch->fix_call_dummy == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_fix_call_dummy invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_fix_call_dummy called\n");
  gdbarch->fix_call_dummy (dummy, pc, fun, nargs, args, type, gcc_p);
}

void
set_gdbarch_fix_call_dummy (struct gdbarch *gdbarch,
                            gdbarch_fix_call_dummy_ftype fix_call_dummy)
{
  gdbarch->fix_call_dummy = fix_call_dummy;
}

void
gdbarch_init_frame_pc_first (struct gdbarch *gdbarch, int fromleaf, struct frame_info *prev)
{
  if (gdbarch->init_frame_pc_first == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_init_frame_pc_first invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_init_frame_pc_first called\n");
  gdbarch->init_frame_pc_first (fromleaf, prev);
}

void
set_gdbarch_init_frame_pc_first (struct gdbarch *gdbarch,
                                 gdbarch_init_frame_pc_first_ftype init_frame_pc_first)
{
  gdbarch->init_frame_pc_first = init_frame_pc_first;
}

void
gdbarch_init_frame_pc (struct gdbarch *gdbarch, int fromleaf, struct frame_info *prev)
{
  if (gdbarch->init_frame_pc == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_init_frame_pc invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_init_frame_pc called\n");
  gdbarch->init_frame_pc (fromleaf, prev);
}

void
set_gdbarch_init_frame_pc (struct gdbarch *gdbarch,
                           gdbarch_init_frame_pc_ftype init_frame_pc)
{
  gdbarch->init_frame_pc = init_frame_pc;
}

int
gdbarch_believe_pcc_promotion (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_believe_pcc_promotion called\n");
  return gdbarch->believe_pcc_promotion;
}

void
set_gdbarch_believe_pcc_promotion (struct gdbarch *gdbarch,
                                   int believe_pcc_promotion)
{
  gdbarch->believe_pcc_promotion = believe_pcc_promotion;
}

int
gdbarch_believe_pcc_promotion_type (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_believe_pcc_promotion_type called\n");
  return gdbarch->believe_pcc_promotion_type;
}

void
set_gdbarch_believe_pcc_promotion_type (struct gdbarch *gdbarch,
                                        int believe_pcc_promotion_type)
{
  gdbarch->believe_pcc_promotion_type = believe_pcc_promotion_type;
}

int
gdbarch_coerce_float_to_double (struct gdbarch *gdbarch, struct type *formal, struct type *actual)
{
  if (gdbarch->coerce_float_to_double == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_coerce_float_to_double invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_coerce_float_to_double called\n");
  return gdbarch->coerce_float_to_double (formal, actual);
}

void
set_gdbarch_coerce_float_to_double (struct gdbarch *gdbarch,
                                    gdbarch_coerce_float_to_double_ftype coerce_float_to_double)
{
  gdbarch->coerce_float_to_double = coerce_float_to_double;
}

void
gdbarch_get_saved_register (struct gdbarch *gdbarch, char *raw_buffer, int *optimized, CORE_ADDR *addrp, struct frame_info *frame, int regnum, enum lval_type *lval)
{
  if (gdbarch->get_saved_register == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_get_saved_register invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_get_saved_register called\n");
  gdbarch->get_saved_register (raw_buffer, optimized, addrp, frame, regnum, lval);
}

void
set_gdbarch_get_saved_register (struct gdbarch *gdbarch,
                                gdbarch_get_saved_register_ftype get_saved_register)
{
  gdbarch->get_saved_register = get_saved_register;
}

int
gdbarch_register_convertible (struct gdbarch *gdbarch, int nr)
{
  if (gdbarch->register_convertible == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_convertible invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_convertible called\n");
  return gdbarch->register_convertible (nr);
}

void
set_gdbarch_register_convertible (struct gdbarch *gdbarch,
                                  gdbarch_register_convertible_ftype register_convertible)
{
  gdbarch->register_convertible = register_convertible;
}

void
gdbarch_register_convert_to_virtual (struct gdbarch *gdbarch, int regnum, struct type *type, char *from, char *to)
{
  if (gdbarch->register_convert_to_virtual == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_convert_to_virtual invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_convert_to_virtual called\n");
  gdbarch->register_convert_to_virtual (regnum, type, from, to);
}

void
set_gdbarch_register_convert_to_virtual (struct gdbarch *gdbarch,
                                         gdbarch_register_convert_to_virtual_ftype register_convert_to_virtual)
{
  gdbarch->register_convert_to_virtual = register_convert_to_virtual;
}

void
gdbarch_register_convert_to_raw (struct gdbarch *gdbarch, struct type *type, int regnum, char *from, char *to)
{
  if (gdbarch->register_convert_to_raw == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_register_convert_to_raw invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_register_convert_to_raw called\n");
  gdbarch->register_convert_to_raw (type, regnum, from, to);
}

void
set_gdbarch_register_convert_to_raw (struct gdbarch *gdbarch,
                                     gdbarch_register_convert_to_raw_ftype register_convert_to_raw)
{
  gdbarch->register_convert_to_raw = register_convert_to_raw;
}

void
gdbarch_fetch_pseudo_register (struct gdbarch *gdbarch, int regnum)
{
  if (gdbarch->fetch_pseudo_register == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_fetch_pseudo_register invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_fetch_pseudo_register called\n");
  gdbarch->fetch_pseudo_register (regnum);
}

void
set_gdbarch_fetch_pseudo_register (struct gdbarch *gdbarch,
                                   gdbarch_fetch_pseudo_register_ftype fetch_pseudo_register)
{
  gdbarch->fetch_pseudo_register = fetch_pseudo_register;
}

void
gdbarch_store_pseudo_register (struct gdbarch *gdbarch, int regnum)
{
  if (gdbarch->store_pseudo_register == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_store_pseudo_register invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_store_pseudo_register called\n");
  gdbarch->store_pseudo_register (regnum);
}

void
set_gdbarch_store_pseudo_register (struct gdbarch *gdbarch,
                                   gdbarch_store_pseudo_register_ftype store_pseudo_register)
{
  gdbarch->store_pseudo_register = store_pseudo_register;
}

CORE_ADDR
gdbarch_pointer_to_address (struct gdbarch *gdbarch, struct type *type, void *buf)
{
  if (gdbarch->pointer_to_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_pointer_to_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_pointer_to_address called\n");
  return gdbarch->pointer_to_address (type, buf);
}

void
set_gdbarch_pointer_to_address (struct gdbarch *gdbarch,
                                gdbarch_pointer_to_address_ftype pointer_to_address)
{
  gdbarch->pointer_to_address = pointer_to_address;
}

void
gdbarch_address_to_pointer (struct gdbarch *gdbarch, struct type *type, void *buf, CORE_ADDR addr)
{
  if (gdbarch->address_to_pointer == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_address_to_pointer invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_address_to_pointer called\n");
  gdbarch->address_to_pointer (type, buf, addr);
}

void
set_gdbarch_address_to_pointer (struct gdbarch *gdbarch,
                                gdbarch_address_to_pointer_ftype address_to_pointer)
{
  gdbarch->address_to_pointer = address_to_pointer;
}

int
gdbarch_return_value_on_stack (struct gdbarch *gdbarch, struct type *type)
{
  if (gdbarch->return_value_on_stack == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_return_value_on_stack invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_return_value_on_stack called\n");
  return gdbarch->return_value_on_stack (type);
}

void
set_gdbarch_return_value_on_stack (struct gdbarch *gdbarch,
                                   gdbarch_return_value_on_stack_ftype return_value_on_stack)
{
  gdbarch->return_value_on_stack = return_value_on_stack;
}

void
gdbarch_extract_return_value (struct gdbarch *gdbarch, struct type *type, char *regbuf, char *valbuf)
{
  if (gdbarch->extract_return_value == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_extract_return_value invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_extract_return_value called\n");
  gdbarch->extract_return_value (type, regbuf, valbuf);
}

void
set_gdbarch_extract_return_value (struct gdbarch *gdbarch,
                                  gdbarch_extract_return_value_ftype extract_return_value)
{
  gdbarch->extract_return_value = extract_return_value;
}

CORE_ADDR
gdbarch_push_arguments (struct gdbarch *gdbarch, int nargs, struct value **args, CORE_ADDR sp, int struct_return, CORE_ADDR struct_addr)
{
  if (gdbarch->push_arguments == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_push_arguments invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_push_arguments called\n");
  return gdbarch->push_arguments (nargs, args, sp, struct_return, struct_addr);
}

void
set_gdbarch_push_arguments (struct gdbarch *gdbarch,
                            gdbarch_push_arguments_ftype push_arguments)
{
  gdbarch->push_arguments = push_arguments;
}

void
gdbarch_push_dummy_frame (struct gdbarch *gdbarch)
{
  if (gdbarch->push_dummy_frame == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_push_dummy_frame invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_push_dummy_frame called\n");
  gdbarch->push_dummy_frame ();
}

void
set_gdbarch_push_dummy_frame (struct gdbarch *gdbarch,
                              gdbarch_push_dummy_frame_ftype push_dummy_frame)
{
  gdbarch->push_dummy_frame = push_dummy_frame;
}

CORE_ADDR
gdbarch_push_return_address (struct gdbarch *gdbarch, CORE_ADDR pc, CORE_ADDR sp)
{
  if (gdbarch->push_return_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_push_return_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_push_return_address called\n");
  return gdbarch->push_return_address (pc, sp);
}

void
set_gdbarch_push_return_address (struct gdbarch *gdbarch,
                                 gdbarch_push_return_address_ftype push_return_address)
{
  gdbarch->push_return_address = push_return_address;
}

void
gdbarch_pop_frame (struct gdbarch *gdbarch)
{
  if (gdbarch->pop_frame == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_pop_frame invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_pop_frame called\n");
  gdbarch->pop_frame ();
}

void
set_gdbarch_pop_frame (struct gdbarch *gdbarch,
                       gdbarch_pop_frame_ftype pop_frame)
{
  gdbarch->pop_frame = pop_frame;
}

void
gdbarch_store_struct_return (struct gdbarch *gdbarch, CORE_ADDR addr, CORE_ADDR sp)
{
  if (gdbarch->store_struct_return == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_store_struct_return invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_store_struct_return called\n");
  gdbarch->store_struct_return (addr, sp);
}

void
set_gdbarch_store_struct_return (struct gdbarch *gdbarch,
                                 gdbarch_store_struct_return_ftype store_struct_return)
{
  gdbarch->store_struct_return = store_struct_return;
}

void
gdbarch_store_return_value (struct gdbarch *gdbarch, struct type *type, char *valbuf)
{
  if (gdbarch->store_return_value == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_store_return_value invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_store_return_value called\n");
  gdbarch->store_return_value (type, valbuf);
}

void
set_gdbarch_store_return_value (struct gdbarch *gdbarch,
                                gdbarch_store_return_value_ftype store_return_value)
{
  gdbarch->store_return_value = store_return_value;
}

int
gdbarch_extract_struct_value_address_p (struct gdbarch *gdbarch)
{
  return gdbarch->extract_struct_value_address != 0;
}

CORE_ADDR
gdbarch_extract_struct_value_address (struct gdbarch *gdbarch, char *regbuf)
{
  if (gdbarch->extract_struct_value_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_extract_struct_value_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_extract_struct_value_address called\n");
  return gdbarch->extract_struct_value_address (regbuf);
}

void
set_gdbarch_extract_struct_value_address (struct gdbarch *gdbarch,
                                          gdbarch_extract_struct_value_address_ftype extract_struct_value_address)
{
  gdbarch->extract_struct_value_address = extract_struct_value_address;
}

int
gdbarch_use_struct_convention (struct gdbarch *gdbarch, int gcc_p, struct type *value_type)
{
  if (gdbarch->use_struct_convention == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_use_struct_convention invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_use_struct_convention called\n");
  return gdbarch->use_struct_convention (gcc_p, value_type);
}

void
set_gdbarch_use_struct_convention (struct gdbarch *gdbarch,
                                   gdbarch_use_struct_convention_ftype use_struct_convention)
{
  gdbarch->use_struct_convention = use_struct_convention;
}

void
gdbarch_frame_init_saved_regs (struct gdbarch *gdbarch, struct frame_info *frame)
{
  if (gdbarch->frame_init_saved_regs == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_init_saved_regs invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_init_saved_regs called\n");
  gdbarch->frame_init_saved_regs (frame);
}

void
set_gdbarch_frame_init_saved_regs (struct gdbarch *gdbarch,
                                   gdbarch_frame_init_saved_regs_ftype frame_init_saved_regs)
{
  gdbarch->frame_init_saved_regs = frame_init_saved_regs;
}

void
gdbarch_init_extra_frame_info (struct gdbarch *gdbarch, int fromleaf, struct frame_info *frame)
{
  if (gdbarch->init_extra_frame_info == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_init_extra_frame_info invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_init_extra_frame_info called\n");
  gdbarch->init_extra_frame_info (fromleaf, frame);
}

void
set_gdbarch_init_extra_frame_info (struct gdbarch *gdbarch,
                                   gdbarch_init_extra_frame_info_ftype init_extra_frame_info)
{
  gdbarch->init_extra_frame_info = init_extra_frame_info;
}

CORE_ADDR
gdbarch_skip_prologue (struct gdbarch *gdbarch, CORE_ADDR ip)
{
  if (gdbarch->skip_prologue == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_skip_prologue invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_skip_prologue called\n");
  return gdbarch->skip_prologue (ip);
}

void
set_gdbarch_skip_prologue (struct gdbarch *gdbarch,
                           gdbarch_skip_prologue_ftype skip_prologue)
{
  gdbarch->skip_prologue = skip_prologue;
}

int
gdbarch_prologue_frameless_p (struct gdbarch *gdbarch, CORE_ADDR ip)
{
  if (gdbarch->prologue_frameless_p == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_prologue_frameless_p invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_prologue_frameless_p called\n");
  return gdbarch->prologue_frameless_p (ip);
}

void
set_gdbarch_prologue_frameless_p (struct gdbarch *gdbarch,
                                  gdbarch_prologue_frameless_p_ftype prologue_frameless_p)
{
  gdbarch->prologue_frameless_p = prologue_frameless_p;
}

int
gdbarch_inner_than (struct gdbarch *gdbarch, CORE_ADDR lhs, CORE_ADDR rhs)
{
  if (gdbarch->inner_than == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_inner_than invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_inner_than called\n");
  return gdbarch->inner_than (lhs, rhs);
}

void
set_gdbarch_inner_than (struct gdbarch *gdbarch,
                        gdbarch_inner_than_ftype inner_than)
{
  gdbarch->inner_than = inner_than;
}

unsigned char *
gdbarch_breakpoint_from_pc (struct gdbarch *gdbarch, CORE_ADDR *pcptr, int *lenptr)
{
  if (gdbarch->breakpoint_from_pc == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_breakpoint_from_pc invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_breakpoint_from_pc called\n");
  return gdbarch->breakpoint_from_pc (pcptr, lenptr);
}

void
set_gdbarch_breakpoint_from_pc (struct gdbarch *gdbarch,
                                gdbarch_breakpoint_from_pc_ftype breakpoint_from_pc)
{
  gdbarch->breakpoint_from_pc = breakpoint_from_pc;
}

int
gdbarch_memory_insert_breakpoint (struct gdbarch *gdbarch, CORE_ADDR addr, char *contents_cache)
{
  if (gdbarch->memory_insert_breakpoint == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_memory_insert_breakpoint invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_memory_insert_breakpoint called\n");
  return gdbarch->memory_insert_breakpoint (addr, contents_cache);
}

void
set_gdbarch_memory_insert_breakpoint (struct gdbarch *gdbarch,
                                      gdbarch_memory_insert_breakpoint_ftype memory_insert_breakpoint)
{
  gdbarch->memory_insert_breakpoint = memory_insert_breakpoint;
}

int
gdbarch_memory_remove_breakpoint (struct gdbarch *gdbarch, CORE_ADDR addr, char *contents_cache)
{
  if (gdbarch->memory_remove_breakpoint == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_memory_remove_breakpoint invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_memory_remove_breakpoint called\n");
  return gdbarch->memory_remove_breakpoint (addr, contents_cache);
}

void
set_gdbarch_memory_remove_breakpoint (struct gdbarch *gdbarch,
                                      gdbarch_memory_remove_breakpoint_ftype memory_remove_breakpoint)
{
  gdbarch->memory_remove_breakpoint = memory_remove_breakpoint;
}

CORE_ADDR
gdbarch_decr_pc_after_break (struct gdbarch *gdbarch)
{
  if (gdbarch->decr_pc_after_break == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_decr_pc_after_break invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_decr_pc_after_break called\n");
  return gdbarch->decr_pc_after_break;
}

void
set_gdbarch_decr_pc_after_break (struct gdbarch *gdbarch,
                                 CORE_ADDR decr_pc_after_break)
{
  gdbarch->decr_pc_after_break = decr_pc_after_break;
}

int
gdbarch_prepare_to_proceed (struct gdbarch *gdbarch, int select_it)
{
  if (gdbarch->prepare_to_proceed == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_prepare_to_proceed invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_prepare_to_proceed called\n");
  return gdbarch->prepare_to_proceed (select_it);
}

void
set_gdbarch_prepare_to_proceed (struct gdbarch *gdbarch,
                                gdbarch_prepare_to_proceed_ftype prepare_to_proceed)
{
  gdbarch->prepare_to_proceed = prepare_to_proceed;
}

CORE_ADDR
gdbarch_function_start_offset (struct gdbarch *gdbarch)
{
  if (gdbarch->function_start_offset == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_function_start_offset invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_function_start_offset called\n");
  return gdbarch->function_start_offset;
}

void
set_gdbarch_function_start_offset (struct gdbarch *gdbarch,
                                   CORE_ADDR function_start_offset)
{
  gdbarch->function_start_offset = function_start_offset;
}

void
gdbarch_remote_translate_xfer_address (struct gdbarch *gdbarch, CORE_ADDR gdb_addr, int gdb_len, CORE_ADDR *rem_addr, int *rem_len)
{
  if (gdbarch->remote_translate_xfer_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_remote_translate_xfer_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_remote_translate_xfer_address called\n");
  gdbarch->remote_translate_xfer_address (gdb_addr, gdb_len, rem_addr, rem_len);
}

void
set_gdbarch_remote_translate_xfer_address (struct gdbarch *gdbarch,
                                           gdbarch_remote_translate_xfer_address_ftype remote_translate_xfer_address)
{
  gdbarch->remote_translate_xfer_address = remote_translate_xfer_address;
}

CORE_ADDR
gdbarch_frame_args_skip (struct gdbarch *gdbarch)
{
  if (gdbarch->frame_args_skip == -1)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_args_skip invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_args_skip called\n");
  return gdbarch->frame_args_skip;
}

void
set_gdbarch_frame_args_skip (struct gdbarch *gdbarch,
                             CORE_ADDR frame_args_skip)
{
  gdbarch->frame_args_skip = frame_args_skip;
}

int
gdbarch_frameless_function_invocation (struct gdbarch *gdbarch, struct frame_info *fi)
{
  if (gdbarch->frameless_function_invocation == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frameless_function_invocation invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frameless_function_invocation called\n");
  return gdbarch->frameless_function_invocation (fi);
}

void
set_gdbarch_frameless_function_invocation (struct gdbarch *gdbarch,
                                           gdbarch_frameless_function_invocation_ftype frameless_function_invocation)
{
  gdbarch->frameless_function_invocation = frameless_function_invocation;
}

CORE_ADDR
gdbarch_frame_chain (struct gdbarch *gdbarch, struct frame_info *frame)
{
  if (gdbarch->frame_chain == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_chain invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_chain called\n");
  return gdbarch->frame_chain (frame);
}

void
set_gdbarch_frame_chain (struct gdbarch *gdbarch,
                         gdbarch_frame_chain_ftype frame_chain)
{
  gdbarch->frame_chain = frame_chain;
}

int
gdbarch_frame_chain_valid (struct gdbarch *gdbarch, CORE_ADDR chain, struct frame_info *thisframe)
{
  if (gdbarch->frame_chain_valid == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_chain_valid invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_chain_valid called\n");
  return gdbarch->frame_chain_valid (chain, thisframe);
}

void
set_gdbarch_frame_chain_valid (struct gdbarch *gdbarch,
                               gdbarch_frame_chain_valid_ftype frame_chain_valid)
{
  gdbarch->frame_chain_valid = frame_chain_valid;
}

CORE_ADDR
gdbarch_frame_saved_pc (struct gdbarch *gdbarch, struct frame_info *fi)
{
  if (gdbarch->frame_saved_pc == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_saved_pc invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_saved_pc called\n");
  return gdbarch->frame_saved_pc (fi);
}

void
set_gdbarch_frame_saved_pc (struct gdbarch *gdbarch,
                            gdbarch_frame_saved_pc_ftype frame_saved_pc)
{
  gdbarch->frame_saved_pc = frame_saved_pc;
}

CORE_ADDR
gdbarch_frame_args_address (struct gdbarch *gdbarch, struct frame_info *fi)
{
  if (gdbarch->frame_args_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_args_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_args_address called\n");
  return gdbarch->frame_args_address (fi);
}

void
set_gdbarch_frame_args_address (struct gdbarch *gdbarch,
                                gdbarch_frame_args_address_ftype frame_args_address)
{
  gdbarch->frame_args_address = frame_args_address;
}

CORE_ADDR
gdbarch_frame_locals_address (struct gdbarch *gdbarch, struct frame_info *fi)
{
  if (gdbarch->frame_locals_address == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_locals_address invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_locals_address called\n");
  return gdbarch->frame_locals_address (fi);
}

void
set_gdbarch_frame_locals_address (struct gdbarch *gdbarch,
                                  gdbarch_frame_locals_address_ftype frame_locals_address)
{
  gdbarch->frame_locals_address = frame_locals_address;
}

CORE_ADDR
gdbarch_saved_pc_after_call (struct gdbarch *gdbarch, struct frame_info *frame)
{
  if (gdbarch->saved_pc_after_call == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_saved_pc_after_call invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_saved_pc_after_call called\n");
  return gdbarch->saved_pc_after_call (frame);
}

void
set_gdbarch_saved_pc_after_call (struct gdbarch *gdbarch,
                                 gdbarch_saved_pc_after_call_ftype saved_pc_after_call)
{
  gdbarch->saved_pc_after_call = saved_pc_after_call;
}

int
gdbarch_frame_num_args (struct gdbarch *gdbarch, struct frame_info *frame)
{
  if (gdbarch->frame_num_args == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_frame_num_args invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_frame_num_args called\n");
  return gdbarch->frame_num_args (frame);
}

void
set_gdbarch_frame_num_args (struct gdbarch *gdbarch,
                            gdbarch_frame_num_args_ftype frame_num_args)
{
  gdbarch->frame_num_args = frame_num_args;
}

int
gdbarch_stack_align_p (struct gdbarch *gdbarch)
{
  return gdbarch->stack_align != 0;
}

CORE_ADDR
gdbarch_stack_align (struct gdbarch *gdbarch, CORE_ADDR sp)
{
  if (gdbarch->stack_align == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_stack_align invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_stack_align called\n");
  return gdbarch->stack_align (sp);
}

void
set_gdbarch_stack_align (struct gdbarch *gdbarch,
                         gdbarch_stack_align_ftype stack_align)
{
  gdbarch->stack_align = stack_align;
}

int
gdbarch_extra_stack_alignment_needed (struct gdbarch *gdbarch)
{
  /* Skip verify of extra_stack_alignment_needed, invalid_p == 0 */
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_extra_stack_alignment_needed called\n");
  return gdbarch->extra_stack_alignment_needed;
}

void
set_gdbarch_extra_stack_alignment_needed (struct gdbarch *gdbarch,
                                          int extra_stack_alignment_needed)
{
  gdbarch->extra_stack_alignment_needed = extra_stack_alignment_needed;
}

int
gdbarch_reg_struct_has_addr_p (struct gdbarch *gdbarch)
{
  return gdbarch->reg_struct_has_addr != 0;
}

int
gdbarch_reg_struct_has_addr (struct gdbarch *gdbarch, int gcc_p, struct type *type)
{
  if (gdbarch->reg_struct_has_addr == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_reg_struct_has_addr invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_reg_struct_has_addr called\n");
  return gdbarch->reg_struct_has_addr (gcc_p, type);
}

void
set_gdbarch_reg_struct_has_addr (struct gdbarch *gdbarch,
                                 gdbarch_reg_struct_has_addr_ftype reg_struct_has_addr)
{
  gdbarch->reg_struct_has_addr = reg_struct_has_addr;
}

int
gdbarch_save_dummy_frame_tos_p (struct gdbarch *gdbarch)
{
  return gdbarch->save_dummy_frame_tos != 0;
}

void
gdbarch_save_dummy_frame_tos (struct gdbarch *gdbarch, CORE_ADDR sp)
{
  if (gdbarch->save_dummy_frame_tos == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_save_dummy_frame_tos invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_save_dummy_frame_tos called\n");
  gdbarch->save_dummy_frame_tos (sp);
}

void
set_gdbarch_save_dummy_frame_tos (struct gdbarch *gdbarch,
                                  gdbarch_save_dummy_frame_tos_ftype save_dummy_frame_tos)
{
  gdbarch->save_dummy_frame_tos = save_dummy_frame_tos;
}

int
gdbarch_parm_boundary (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_parm_boundary called\n");
  return gdbarch->parm_boundary;
}

void
set_gdbarch_parm_boundary (struct gdbarch *gdbarch,
                           int parm_boundary)
{
  gdbarch->parm_boundary = parm_boundary;
}

const struct floatformat *
gdbarch_float_format (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_float_format called\n");
  return gdbarch->float_format;
}

void
set_gdbarch_float_format (struct gdbarch *gdbarch,
                          const struct floatformat * float_format)
{
  gdbarch->float_format = float_format;
}

const struct floatformat *
gdbarch_double_format (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_double_format called\n");
  return gdbarch->double_format;
}

void
set_gdbarch_double_format (struct gdbarch *gdbarch,
                           const struct floatformat * double_format)
{
  gdbarch->double_format = double_format;
}

const struct floatformat *
gdbarch_long_double_format (struct gdbarch *gdbarch)
{
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_long_double_format called\n");
  return gdbarch->long_double_format;
}

void
set_gdbarch_long_double_format (struct gdbarch *gdbarch,
                                const struct floatformat * long_double_format)
{
  gdbarch->long_double_format = long_double_format;
}

CORE_ADDR
gdbarch_convert_from_func_ptr_addr (struct gdbarch *gdbarch, CORE_ADDR addr)
{
  if (gdbarch->convert_from_func_ptr_addr == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_convert_from_func_ptr_addr invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_convert_from_func_ptr_addr called\n");
  return gdbarch->convert_from_func_ptr_addr (addr);
}

void
set_gdbarch_convert_from_func_ptr_addr (struct gdbarch *gdbarch,
                                        gdbarch_convert_from_func_ptr_addr_ftype convert_from_func_ptr_addr)
{
  gdbarch->convert_from_func_ptr_addr = convert_from_func_ptr_addr;
}

CORE_ADDR
gdbarch_addr_bits_remove (struct gdbarch *gdbarch, CORE_ADDR addr)
{
  if (gdbarch->addr_bits_remove == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_addr_bits_remove invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_addr_bits_remove called\n");
  return gdbarch->addr_bits_remove (addr);
}

void
set_gdbarch_addr_bits_remove (struct gdbarch *gdbarch,
                              gdbarch_addr_bits_remove_ftype addr_bits_remove)
{
  gdbarch->addr_bits_remove = addr_bits_remove;
}

int
gdbarch_software_single_step_p (struct gdbarch *gdbarch)
{
  return gdbarch->software_single_step != 0;
}

void
gdbarch_software_single_step (struct gdbarch *gdbarch, enum target_signal sig, int insert_breakpoints_p)
{
  if (gdbarch->software_single_step == 0)
    internal_error (__FILE__, __LINE__,
                    "gdbarch: gdbarch_software_single_step invalid");
  if (gdbarch_debug >= 2)
    fprintf_unfiltered (gdb_stdlog, "gdbarch_software_single_step called\n");
  gdbarch->software_single_step (sig, insert_breakpoints_p);
}

void
set_gdbarch_software_single_step (struct gdbarch *gdbarch,
                                  gdbarch_software_single_step_ftype software_single_step)
{
  gdbarch->software_single_step = software_single_step;
}


/* Keep a registry of per-architecture data-pointers required by GDB
   modules. */

struct gdbarch_data
{
  unsigned index;
  gdbarch_data_init_ftype *init;
  gdbarch_data_free_ftype *free;
};

struct gdbarch_data_registration
{
  struct gdbarch_data *data;
  struct gdbarch_data_registration *next;
};

struct gdbarch_data_registry
{
  unsigned nr;
  struct gdbarch_data_registration *registrations;
};

struct gdbarch_data_registry gdbarch_data_registry =
{
  0, NULL,
};

struct gdbarch_data *
register_gdbarch_data (gdbarch_data_init_ftype *init,
                       gdbarch_data_free_ftype *free)
{
  struct gdbarch_data_registration **curr;
  for (curr = &gdbarch_data_registry.registrations;
       (*curr) != NULL;
       curr = &(*curr)->next);
  (*curr) = XMALLOC (struct gdbarch_data_registration);
  (*curr)->next = NULL;
  (*curr)->data = XMALLOC (struct gdbarch_data);
  (*curr)->data->index = gdbarch_data_registry.nr++;
  (*curr)->data->init = init;
  (*curr)->data->free = free;
  return (*curr)->data;
}


/* Walk through all the registered users initializing each in turn. */

static void
init_gdbarch_data (struct gdbarch *gdbarch)
{
  struct gdbarch_data_registration *rego;
  for (rego = gdbarch_data_registry.registrations;
       rego != NULL;
       rego = rego->next)
    {
      struct gdbarch_data *data = rego->data;
      gdb_assert (data->index < gdbarch->nr_data);
      if (data->init != NULL)
        {
          void *pointer = data->init (gdbarch);
          set_gdbarch_data (gdbarch, data, pointer);
        }
    }
}

/* Create/delete the gdbarch data vector. */

static void
alloc_gdbarch_data (struct gdbarch *gdbarch)
{
  gdb_assert (gdbarch->data == NULL);
  gdbarch->nr_data = gdbarch_data_registry.nr;
  gdbarch->data = xcalloc (gdbarch->nr_data, sizeof (void*));
}

static void
free_gdbarch_data (struct gdbarch *gdbarch)
{
  struct gdbarch_data_registration *rego;
  gdb_assert (gdbarch->data != NULL);
  for (rego = gdbarch_data_registry.registrations;
       rego != NULL;
       rego = rego->next)
    {
      struct gdbarch_data *data = rego->data;
      gdb_assert (data->index < gdbarch->nr_data);
      if (data->free != NULL && gdbarch->data[data->index] != NULL)
        {
          data->free (gdbarch, gdbarch->data[data->index]);
          gdbarch->data[data->index] = NULL;
        }
    }
  xfree (gdbarch->data);
  gdbarch->data = NULL;
}


/* Initialize the current value of thee specified per-architecture
   data-pointer. */

void
set_gdbarch_data (struct gdbarch *gdbarch,
                  struct gdbarch_data *data,
                  void *pointer)
{
  gdb_assert (data->index < gdbarch->nr_data);
  if (data->free != NULL && gdbarch->data[data->index] != NULL)
    data->free (gdbarch, gdbarch->data[data->index]);
  gdbarch->data[data->index] = pointer;
}

/* Return the current value of the specified per-architecture
   data-pointer. */

void *
gdbarch_data (struct gdbarch_data *data)
{
  gdb_assert (data->index < current_gdbarch->nr_data);
  return current_gdbarch->data[data->index];
}



/* Keep a registry of swapped data required by GDB modules. */

struct gdbarch_swap
{
  void *swap;
  struct gdbarch_swap_registration *source;
  struct gdbarch_swap *next;
};

struct gdbarch_swap_registration
{
  void *data;
  unsigned long sizeof_data;
  gdbarch_swap_ftype *init;
  struct gdbarch_swap_registration *next;
};

struct gdbarch_swap_registry
{
  int nr;
  struct gdbarch_swap_registration *registrations;
};

struct gdbarch_swap_registry gdbarch_swap_registry = 
{
  0, NULL,
};

void
register_gdbarch_swap (void *data,
		       unsigned long sizeof_data,
		       gdbarch_swap_ftype *init)
{
  struct gdbarch_swap_registration **rego;
  for (rego = &gdbarch_swap_registry.registrations;
       (*rego) != NULL;
       rego = &(*rego)->next);
  (*rego) = XMALLOC (struct gdbarch_swap_registration);
  (*rego)->next = NULL;
  (*rego)->init = init;
  (*rego)->data = data;
  (*rego)->sizeof_data = sizeof_data;
}


static void
init_gdbarch_swap (struct gdbarch *gdbarch)
{
  struct gdbarch_swap_registration *rego;
  struct gdbarch_swap **curr = &gdbarch->swap;
  for (rego = gdbarch_swap_registry.registrations;
       rego != NULL;
       rego = rego->next)
    {
      if (rego->data != NULL)
	{
	  (*curr) = XMALLOC (struct gdbarch_swap);
	  (*curr)->source = rego;
	  (*curr)->swap = xmalloc (rego->sizeof_data);
	  (*curr)->next = NULL;
	  memset (rego->data, 0, rego->sizeof_data);
	  curr = &(*curr)->next;
	}
      if (rego->init != NULL)
	rego->init ();
    }
}

static void
swapout_gdbarch_swap (struct gdbarch *gdbarch)
{
  struct gdbarch_swap *curr;
  for (curr = gdbarch->swap;
       curr != NULL;
       curr = curr->next)
    memcpy (curr->swap, curr->source->data, curr->source->sizeof_data);
}

static void
swapin_gdbarch_swap (struct gdbarch *gdbarch)
{
  struct gdbarch_swap *curr;
  for (curr = gdbarch->swap;
       curr != NULL;
       curr = curr->next)
    memcpy (curr->source->data, curr->swap, curr->source->sizeof_data);
}


/* Keep a registry of the architectures known by GDB. */

struct gdbarch_registration
{
  enum bfd_architecture bfd_architecture;
  gdbarch_init_ftype *init;
  gdbarch_dump_tdep_ftype *dump_tdep;
  struct gdbarch_list *arches;
  struct gdbarch_registration *next;
};

static struct gdbarch_registration *gdbarch_registry = NULL;

static void
append_name (const char ***buf, int *nr, const char *name)
{
  *buf = xrealloc (*buf, sizeof (char**) * (*nr + 1));
  (*buf)[*nr] = name;
  *nr += 1;
}

const char **
gdbarch_printable_names (void)
{
  if (GDB_MULTI_ARCH)
    {
      /* Accumulate a list of names based on the registed list of
         architectures. */
      enum bfd_architecture a;
      int nr_arches = 0;
      const char **arches = NULL;
      struct gdbarch_registration *rego;
      for (rego = gdbarch_registry;
	   rego != NULL;
	   rego = rego->next)
	{
	  const struct bfd_arch_info *ap;
	  ap = bfd_lookup_arch (rego->bfd_architecture, 0);
	  if (ap == NULL)
	    internal_error (__FILE__, __LINE__,
                            "gdbarch_architecture_names: multi-arch unknown");
	  do
	    {
	      append_name (&arches, &nr_arches, ap->printable_name);
	      ap = ap->next;
	    }
	  while (ap != NULL);
	}
      append_name (&arches, &nr_arches, NULL);
      return arches;
    }
  else
    /* Just return all the architectures that BFD knows.  Assume that
       the legacy architecture framework supports them. */
    return bfd_arch_list ();
}


void
gdbarch_register (enum bfd_architecture bfd_architecture,
                  gdbarch_init_ftype *init,
		  gdbarch_dump_tdep_ftype *dump_tdep)
{
  struct gdbarch_registration **curr;
  const struct bfd_arch_info *bfd_arch_info;
  /* Check that BFD recognizes this architecture */
  bfd_arch_info = bfd_lookup_arch (bfd_architecture, 0);
  if (bfd_arch_info == NULL)
    {
      internal_error (__FILE__, __LINE__,
                      "gdbarch: Attempt to register unknown architecture (%d)",
                      bfd_architecture);
    }
  /* Check that we haven't seen this architecture before */
  for (curr = &gdbarch_registry;
       (*curr) != NULL;
       curr = &(*curr)->next)
    {
      if (bfd_architecture == (*curr)->bfd_architecture)
	internal_error (__FILE__, __LINE__,
                        "gdbarch: Duplicate registraration of architecture (%s)",
	                bfd_arch_info->printable_name);
    }
  /* log it */
  if (gdbarch_debug)
    fprintf_unfiltered (gdb_stdlog, "register_gdbarch_init (%s, 0x%08lx)\n",
			bfd_arch_info->printable_name,
			(long) init);
  /* Append it */
  (*curr) = XMALLOC (struct gdbarch_registration);
  (*curr)->bfd_architecture = bfd_architecture;
  (*curr)->init = init;
  (*curr)->dump_tdep = dump_tdep;
  (*curr)->arches = NULL;
  (*curr)->next = NULL;
  /* When non- multi-arch, install whatever target dump routine we've
     been provided - hopefully that routine has been written correctly
     and works regardless of multi-arch. */
  if (!GDB_MULTI_ARCH && dump_tdep != NULL
      && startup_gdbarch.dump_tdep == NULL)
    startup_gdbarch.dump_tdep = dump_tdep;
}

void
register_gdbarch_init (enum bfd_architecture bfd_architecture,
		       gdbarch_init_ftype *init)
{
  gdbarch_register (bfd_architecture, init, NULL);
}


/* Look for an architecture using gdbarch_info.  Base search on only
   BFD_ARCH_INFO and BYTE_ORDER. */

struct gdbarch_list *
gdbarch_list_lookup_by_info (struct gdbarch_list *arches,
                             const struct gdbarch_info *info)
{
  for (; arches != NULL; arches = arches->next)
    {
      if (info->bfd_arch_info != arches->gdbarch->bfd_arch_info)
	continue;
      if (info->byte_order != arches->gdbarch->byte_order)
	continue;
      return arches;
    }
  return NULL;
}


/* Update the current architecture. Return ZERO if the update request
   failed. */

int
gdbarch_update_p (struct gdbarch_info info)
{
  struct gdbarch *new_gdbarch;
  struct gdbarch_list **list;
  struct gdbarch_registration *rego;

  /* Fill in missing parts of the INFO struct using a number of
     sources: ``set ...''; INFOabfd supplied; existing target.  */

  /* ``(gdb) set architecture ...'' */
  if (info.bfd_arch_info == NULL
      && !TARGET_ARCHITECTURE_AUTO)
    info.bfd_arch_info = TARGET_ARCHITECTURE;
  if (info.bfd_arch_info == NULL
      && info.abfd != NULL
      && bfd_get_arch (info.abfd) != bfd_arch_unknown
      && bfd_get_arch (info.abfd) != bfd_arch_obscure)
    info.bfd_arch_info = bfd_get_arch_info (info.abfd);
  if (info.bfd_arch_info == NULL)
    info.bfd_arch_info = TARGET_ARCHITECTURE;

  /* ``(gdb) set byte-order ...'' */
  if (info.byte_order == 0
      && !TARGET_BYTE_ORDER_AUTO)
    info.byte_order = TARGET_BYTE_ORDER;
  /* From the INFO struct. */
  if (info.byte_order == 0
      && info.abfd != NULL)
    info.byte_order = (bfd_big_endian (info.abfd) ? BIG_ENDIAN
		       : bfd_little_endian (info.abfd) ? LITTLE_ENDIAN
		       : 0);
  /* From the current target. */
  if (info.byte_order == 0)
    info.byte_order = TARGET_BYTE_ORDER;

  /* Must have found some sort of architecture. */
  gdb_assert (info.bfd_arch_info != NULL);

  if (gdbarch_debug)
    {
      fprintf_unfiltered (gdb_stdlog,
			  "gdbarch_update: info.bfd_arch_info %s\n",
			  (info.bfd_arch_info != NULL
			   ? info.bfd_arch_info->printable_name
			   : "(null)"));
      fprintf_unfiltered (gdb_stdlog,
			  "gdbarch_update: info.byte_order %d (%s)\n",
			  info.byte_order,
			  (info.byte_order == BIG_ENDIAN ? "big"
			   : info.byte_order == LITTLE_ENDIAN ? "little"
			   : "default"));
      fprintf_unfiltered (gdb_stdlog,
			  "gdbarch_update: info.abfd 0x%lx\n",
			  (long) info.abfd);
      fprintf_unfiltered (gdb_stdlog,
			  "gdbarch_update: info.tdep_info 0x%lx\n",
			  (long) info.tdep_info);
    }

  /* Find the target that knows about this architecture. */
  for (rego = gdbarch_registry;
       rego != NULL;
       rego = rego->next)
    if (rego->bfd_architecture == info.bfd_arch_info->arch)
      break;
  if (rego == NULL)
    {
      if (gdbarch_debug)
	fprintf_unfiltered (gdb_stdlog, "gdbarch_update: No matching architecture\n");
      return 0;
    }

  /* Ask the target for a replacement architecture. */
  new_gdbarch = rego->init (info, rego->arches);

  /* Did the target like it?  No. Reject the change. */
  if (new_gdbarch == NULL)
    {
      if (gdbarch_debug)
	fprintf_unfiltered (gdb_stdlog, "gdbarch_update: Target rejected architecture\n");
      return 0;
    }

  /* Did the architecture change?  No. Do nothing. */
  if (current_gdbarch == new_gdbarch)
    {
      if (gdbarch_debug)
	fprintf_unfiltered (gdb_stdlog, "gdbarch_update: Architecture 0x%08lx (%s) unchanged\n",
			    (long) new_gdbarch,
			    new_gdbarch->bfd_arch_info->printable_name);
      return 1;
    }

  /* Swap all data belonging to the old target out */
  swapout_gdbarch_swap (current_gdbarch);

  /* Is this a pre-existing architecture?  Yes. Swap it in.  */
  for (list = &rego->arches;
       (*list) != NULL;
       list = &(*list)->next)
    {
      if ((*list)->gdbarch == new_gdbarch)
	{
	  if (gdbarch_debug)
	    fprintf_unfiltered (gdb_stdlog,
                                "gdbarch_update: Previous architecture 0x%08lx (%s) selected\n",
				(long) new_gdbarch,
				new_gdbarch->bfd_arch_info->printable_name);
	  current_gdbarch = new_gdbarch;
	  swapin_gdbarch_swap (new_gdbarch);
	  return 1;
	}
    }

  /* Append this new architecture to this targets list. */
  (*list) = XMALLOC (struct gdbarch_list);
  (*list)->next = NULL;
  (*list)->gdbarch = new_gdbarch;

  /* Switch to this new architecture.  Dump it out. */
  current_gdbarch = new_gdbarch;
  if (gdbarch_debug)
    {
      fprintf_unfiltered (gdb_stdlog,
			  "gdbarch_update: New architecture 0x%08lx (%s) selected\n",
			  (long) new_gdbarch,
			  new_gdbarch->bfd_arch_info->printable_name);
    }
  
  /* Check that the newly installed architecture is valid.  Plug in
     any post init values.  */
  new_gdbarch->dump_tdep = rego->dump_tdep;
  verify_gdbarch (new_gdbarch);

  /* Initialize the per-architecture memory (swap) areas.
     CURRENT_GDBARCH must be update before these modules are
     called. */
  init_gdbarch_swap (new_gdbarch);
  
  /* Initialize the per-architecture data-pointer of all parties that
     registered an interest in this architecture.  CURRENT_GDBARCH
     must be updated before these modules are called. */
  init_gdbarch_data (new_gdbarch);
  
  if (gdbarch_debug)
    gdbarch_dump (current_gdbarch, gdb_stdlog);

  return 1;
}


/* Disassembler */

/* Pointer to the target-dependent disassembly function.  */
int (*tm_print_insn) (bfd_vma, disassemble_info *);
disassemble_info tm_print_insn_info;


extern void _initialize_gdbarch (void);

void
_initialize_gdbarch (void)
{
  struct cmd_list_element *c;

  INIT_DISASSEMBLE_INFO_NO_ARCH (tm_print_insn_info, gdb_stdout, (fprintf_ftype)fprintf_filtered);
  tm_print_insn_info.flavour = bfd_target_unknown_flavour;
  tm_print_insn_info.read_memory_func = dis_asm_read_memory;
  tm_print_insn_info.memory_error_func = dis_asm_memory_error;
  tm_print_insn_info.print_address_func = dis_asm_print_address;

  add_show_from_set (add_set_cmd ("arch",
				  class_maintenance,
				  var_zinteger,
				  (char *)&gdbarch_debug,
				  "Set architecture debugging.\n\
When non-zero, architecture debugging is enabled.", &setdebuglist),
		     &showdebuglist);
  c = add_set_cmd ("archdebug",
		   class_maintenance,
		   var_zinteger,
		   (char *)&gdbarch_debug,
		   "Set architecture debugging.\n\
When non-zero, architecture debugging is enabled.", &setlist);

  deprecate_cmd (c, "set debug arch");
  deprecate_cmd (add_show_from_set (c, &showlist), "show debug arch");
}
