/* Target dependent code for CRIS, for GDB, the GNU debugger.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by Axis Communications AB.
   Written by Hendrik Ruijter, Stefan Andersson, and Orjan Friberg.

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
#include "frame.h"
#include "symtab.h"
#include "inferior.h"
#include "gdbtypes.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "target.h"
#include "value.h"
#include "opcode/cris.h"
#include "arch-utils.h"
#include "regcache.h"

/* To get entry_point_address.  */
#include "symfile.h"

enum cris_num_regs
{
  /* There are no floating point registers.  Used in gdbserver low-linux.c.  */
  NUM_FREGS = 0,
  
  /* There are 16 general registers.  */
  NUM_GENREGS = 16,
  
  /* There are 16 special registers.  */
  NUM_SPECREGS = 16
};

/* Register numbers of various important registers.
   FP_REGNUM   Contains address of executing stack frame.
   STR_REGNUM  Contains the address of structure return values.
   RET_REGNUM  Contains the return value when shorter than or equal to 32 bits
   ARG1_REGNUM Contains the first parameter to a function.
   ARG2_REGNUM Contains the second parameter to a function.
   ARG3_REGNUM Contains the third parameter to a function.
   ARG4_REGNUM Contains the fourth parameter to a function. Rest on stack.
   SP_REGNUM   Contains address of top of stack.
   PC_REGNUM   Contains address of next instruction.
   SRP_REGNUM  Subroutine return pointer register.
   BRP_REGNUM  Breakpoint return pointer register.  */

/* FP_REGNUM = 8, SP_REGNUM = 14, and PC_REGNUM = 15 have been incorporated
   into the multi-arch framework.  */

enum cris_regnums
{
  /* Enums with respect to the general registers, valid for all 
     CRIS versions.  */
  STR_REGNUM  = 9,
  RET_REGNUM  = 10,
  ARG1_REGNUM = 10,
  ARG2_REGNUM = 11,
  ARG3_REGNUM = 12,
  ARG4_REGNUM = 13,
  
  /* Enums with respect to the special registers, some of which may not be
     applicable to all CRIS versions.  */
  P0_REGNUM   = 16,
  VR_REGNUM   = 17,
  P2_REGNUM   = 18,
  P3_REGNUM   = 19,
  P4_REGNUM   = 20,
  CCR_REGNUM  = 21,
  MOF_REGNUM  = 23,
  P8_REGNUM   = 24,
  IBR_REGNUM  = 25,
  IRP_REGNUM  = 26,
  SRP_REGNUM  = 27,
  BAR_REGNUM  = 28,
  BRP_REGNUM  = 30,
  USP_REGNUM  = 31
};

extern const struct cris_spec_reg cris_spec_regs[];

/* CRIS version, set via the user command 'set cris-version'.  Affects
   register names and sizes.*/
static int usr_cmd_cris_version;

/* Indicates whether to trust the above variable.  */
static int usr_cmd_cris_version_valid = 0;

/* CRIS mode, set via the user command 'set cris-mode'.  Affects availability
   of some registers.  */
static const char *usr_cmd_cris_mode;

/* Indicates whether to trust the above variable.  */
static int usr_cmd_cris_mode_valid = 0;

static const char CRIS_MODE_USER[] = "CRIS_MODE_USER";
static const char CRIS_MODE_SUPERVISOR[] = "CRIS_MODE_SUPERVISOR";
static const char *cris_mode_enums[] = 
{
  CRIS_MODE_USER,
  CRIS_MODE_SUPERVISOR,
  0
};

/* CRIS ABI, set via the user command 'set cris-abi'.  
   There are two flavours:
   1. Original ABI with 32-bit doubles, where arguments <= 4 bytes are 
   passed by value.
   2. New ABI with 64-bit doubles, where arguments <= 8 bytes are passed by 
   value.  */
static const char *usr_cmd_cris_abi;

/* Indicates whether to trust the above variable.  */
static int usr_cmd_cris_abi_valid = 0;

/* These variables are strings instead of enums to make them usable as 
   parameters to add_set_enum_cmd.  */
static const char CRIS_ABI_ORIGINAL[] = "CRIS_ABI_ORIGINAL";
static const char CRIS_ABI_V2[] = "CRIS_ABI_V2";
static const char CRIS_ABI_SYMBOL[] = ".$CRIS_ABI_V2";
static const char *cris_abi_enums[] = 
{
  CRIS_ABI_ORIGINAL,
  CRIS_ABI_V2,
  0
};

/* CRIS architecture specific information.  */
struct gdbarch_tdep
{
  int cris_version;
  const char *cris_mode;
  const char *cris_abi;
};

/* Functions for accessing target dependent data.  */

static int
cris_version (void)
{
  return (gdbarch_tdep (current_gdbarch)->cris_version);
}

static const char *
cris_mode (void)
{
  return (gdbarch_tdep (current_gdbarch)->cris_mode);
}

static const char *
cris_abi (void)
{
  return (gdbarch_tdep (current_gdbarch)->cris_abi);
}

/* For saving call-clobbered contents in R9 when returning structs.  */
static CORE_ADDR struct_return_address;

struct frame_extra_info
{
  CORE_ADDR return_pc;
  int leaf_function;
};

/* The instruction environment needed to find single-step breakpoints.  */
typedef 
struct instruction_environment
{
  unsigned long reg[NUM_GENREGS];
  unsigned long preg[NUM_SPECREGS];
  unsigned long branch_break_address;
  unsigned long delay_slot_pc;
  unsigned long prefix_value;
  int   branch_found;
  int   prefix_found;
  int   invalid;
  int   slot_needed;
  int   delay_slot_pc_active;
  int   xflag_found;
  int   disable_interrupt;
} inst_env_type;

/* Save old breakpoints in order to restore the state before a single_step. 
   At most, two breakpoints will have to be remembered.  */
typedef 
char binsn_quantum[BREAKPOINT_MAX];
static binsn_quantum break_mem[2];
static CORE_ADDR next_pc = 0;
static CORE_ADDR branch_target_address = 0;
static unsigned char branch_break_inserted = 0;

/* Machine-dependencies in CRIS for opcodes.  */

/* Instruction sizes.  */
enum cris_instruction_sizes
{
  INST_BYTE_SIZE  = 0,
  INST_WORD_SIZE  = 1,
  INST_DWORD_SIZE = 2
};

/* Addressing modes.  */
enum cris_addressing_modes
{
  REGISTER_MODE = 1,
  INDIRECT_MODE = 2,
  AUTOINC_MODE  = 3
};

/* Prefix addressing modes.  */
enum cris_prefix_addressing_modes
{
  PREFIX_INDEX_MODE  = 2,
  PREFIX_ASSIGN_MODE = 3,

  /* Handle immediate byte offset addressing mode prefix format.  */
  PREFIX_OFFSET_MODE = 2
};

/* Masks for opcodes.  */
enum cris_opcode_masks
{
  BRANCH_SIGNED_SHORT_OFFSET_MASK = 0x1,
  SIGNED_EXTEND_BIT_MASK          = 0x2,
  SIGNED_BYTE_MASK                = 0x80,
  SIGNED_BYTE_EXTEND_MASK         = 0xFFFFFF00,
  SIGNED_WORD_MASK                = 0x8000,
  SIGNED_WORD_EXTEND_MASK         = 0xFFFF0000,
  SIGNED_DWORD_MASK               = 0x80000000,
  SIGNED_QUICK_VALUE_MASK         = 0x20,
  SIGNED_QUICK_VALUE_EXTEND_MASK  = 0xFFFFFFC0
};

/* Functions for opcodes.  The general form of the ETRAX 16-bit instruction:
   Bit 15 - 12   Operand2
       11 - 10   Mode
        9 -  6   Opcode
        5 -  4   Size
        3 -  0   Operand1  */

static int 
cris_get_operand2 (unsigned short insn)
{
  return ((insn & 0xF000) >> 12);
}

static int
cris_get_mode (unsigned short insn)
{
  return ((insn & 0x0C00) >> 10);
}

static int
cris_get_opcode (unsigned short insn)
{
  return ((insn & 0x03C0) >> 6);
}

static int
cris_get_size (unsigned short insn)
{
  return ((insn & 0x0030) >> 4);
}

static int
cris_get_operand1 (unsigned short insn)
{
  return (insn & 0x000F);
}

/* Additional functions in order to handle opcodes.  */

static int
cris_get_wide_opcode (unsigned short insn)
{
  return ((insn & 0x03E0) >> 5);
}

static int
cris_get_short_size (unsigned short insn)
{
  return ((insn & 0x0010) >> 4);
}

static int
cris_get_quick_value (unsigned short insn)
{
  return (insn & 0x003F);
}

static int
cris_get_bdap_quick_offset (unsigned short insn)
{
  return (insn & 0x00FF);
}

static int
cris_get_branch_short_offset (unsigned short insn)
{
  return (insn & 0x00FF);
}

static int
cris_get_asr_shift_steps (unsigned long value)
{
  return (value & 0x3F);
}

static int
cris_get_asr_quick_shift_steps (unsigned short insn)
{
  return (insn & 0x1F);
}

static int
cris_get_clear_size (unsigned short insn)
{
  return ((insn) & 0xC000);
}

static int
cris_is_signed_extend_bit_on (unsigned short insn)
{
  return (((insn) & 0x20) == 0x20);
}

static int
cris_is_xflag_bit_on (unsigned short insn)
{
  return (((insn) & 0x1000) == 0x1000);
}

static void
cris_set_size_to_dword (unsigned short *insn)
{
  *insn &= 0xFFCF; 
  *insn |= 0x20; 
}

static char
cris_get_signed_offset (unsigned short insn)
{
  return ((char) (insn & 0x00FF));
}

/* Calls an op function given the op-type, working on the insn and the
   inst_env.  */
static void cris_gdb_func (enum cris_op_type, unsigned short, inst_env_type *);

static CORE_ADDR cris_skip_prologue_main (CORE_ADDR pc, int frameless_p);

static struct gdbarch *cris_gdbarch_init (struct gdbarch_info,
                                          struct gdbarch_list *);

static int cris_delayed_get_disassembler (bfd_vma, disassemble_info *);

static void cris_dump_tdep (struct gdbarch *, struct ui_file *);

static void cris_version_update (char *ignore_args, int from_tty, 
                                 struct cmd_list_element *c);

static void cris_mode_update (char *ignore_args, int from_tty, 
                              struct cmd_list_element *c);

static void cris_abi_update (char *ignore_args, int from_tty, 
                             struct cmd_list_element *c);

static CORE_ADDR bfd_lookup_symbol (bfd *, const char *);

/* Frames information. The definition of the struct frame_info is

   CORE_ADDR frame
   CORE_ADDR pc
   int signal_handler_caller
   CORE_ADDR return_pc
   int leaf_function

   If the compilation option -fno-omit-frame-pointer is present the
   variable frame will be set to the content of R8 which is the frame
   pointer register.

   The variable pc contains the address where execution is performed
   in the present frame.  The innermost frame contains the current content
   of the register PC.  All other frames contain the content of the
   register PC in the next frame.

   The variable signal_handler_caller is non-zero when the frame is
   associated with the call of a signal handler.

   The variable return_pc contains the address where execution should be
   resumed when the present frame has finished, the return address.

   The variable leaf_function is 1 if the return address is in the register
   SRP, and 0 if it is on the stack.

   Prologue instructions C-code.
   The prologue may consist of (-fno-omit-frame-pointer)
   1)                2)
   push   srp
   push   r8         push   r8
   move.d sp,r8      move.d sp,r8
   subq   X,sp       subq   X,sp
   movem  rY,[sp]    movem  rY,[sp]
   move.S rZ,[r8-U]  move.S rZ,[r8-U]

   where 1 is a non-terminal function, and 2 is a leaf-function.

   Note that this assumption is extremely brittle, and will break at the
   slightest change in GCC's prologue.

   If local variables are declared or register contents are saved on stack
   the subq-instruction will be present with X as the number of bytes
   needed for storage.  The reshuffle with respect to r8 may be performed
   with any size S (b, w, d) and any of the general registers Z={0..13}. 
   The offset U should be representable by a signed 8-bit value in all cases. 
   Thus, the prefix word is assumed to be immediate byte offset mode followed
   by another word containing the instruction.

   Degenerate cases:
   3)
   push   r8
   move.d sp,r8
   move.d r8,sp
   pop    r8   

   Prologue instructions C++-code.
   Case 1) and 2) in the C-code may be followed by

   move.d r10,rS    ; this
   move.d r11,rT    ; P1
   move.d r12,rU    ; P2
   move.d r13,rV    ; P3
   move.S [r8+U],rZ ; P4

   if any of the call parameters are stored. The host expects these 
   instructions to be executed in order to get the call parameters right.  */

/* Examine the prologue of a function.  The variable ip is the address of 
   the first instruction of the prologue.  The variable limit is the address 
   of the first instruction after the prologue.  The variable fi contains the 
   information in struct frame_info.  The variable frameless_p controls whether
   the entire prologue is examined (0) or just enough instructions to 
   determine that it is a prologue (1).  */

CORE_ADDR 
cris_examine (CORE_ADDR ip, CORE_ADDR limit, struct frame_info *fi, 
              int frameless_p)
{
  /* Present instruction.  */
  unsigned short insn;

  /* Next instruction, lookahead.  */
  unsigned short insn_next; 
  int regno;

  /* Is there a push fp?  */
  int have_fp; 

  /* Number of byte on stack used for local variables and movem.  */
  int val; 

  /* Highest register number in a movem.  */
  int regsave;

  /* move.d r<source_register>,rS */
  short source_register; 

  /* This frame is with respect to a leaf until a push srp is found.  */
  fi->extra_info->leaf_function = 1;

  /* This frame is without the FP until a push fp is found.  */
  have_fp = 0;

  /* Assume nothing on stack.  */
  val = 0;
  regsave = -1;

  /* No information about register contents so far.  */

  /* We only want to know the end of the prologue when fi->saved_regs == 0.
     When the saved registers are allocated full information is required.  */
  if (fi->saved_regs)
    {
      for (regno = 0; regno < NUM_REGS; regno++)
        fi->saved_regs[regno] = 0;
    }
 
  /* Find the prologue instructions.  */
  do
    {
      insn = read_memory_unsigned_integer (ip, sizeof (short));
      ip += sizeof (short);
      if (insn == 0xE1FC)
        {
          /* push <reg> 32 bit instruction */
          insn_next = read_memory_unsigned_integer (ip, sizeof (short));
          ip += sizeof (short);
          regno = cris_get_operand2 (insn_next);
          if (regno == (SRP_REGNUM - NUM_GENREGS))
            {
              if (frameless_p)
                {
                  return ip;
                }
              fi->extra_info->leaf_function = 0;
            }
          else if (regno == FP_REGNUM)
            {
              have_fp = 1;
            }
        }
      else if (insn == 0x866E)
        {
          /* move.d sp,r8 */
          if (frameless_p)
            {
              return ip;
            }
          continue;
        }
      else if (cris_get_operand2 (insn) == SP_REGNUM 
               && cris_get_mode (insn) == 0x0000
               && cris_get_opcode (insn) == 0x000A)
        {
          /* subq <val>,sp */
          val = cris_get_quick_value (insn);
        }
      else if (cris_get_mode (insn) == 0x0002 
               && cris_get_opcode (insn) == 0x000F
               && cris_get_size (insn) == 0x0003
               && cris_get_operand1 (insn) == SP_REGNUM)
        {
          /* movem r<regsave>,[sp] */
          if (frameless_p)
            {
              return ip;
            }
          regsave = cris_get_operand2 (insn);
        }
      else if (cris_get_operand2 (insn) == SP_REGNUM
               && ((insn & 0x0F00) >> 8) == 0x0001
               && (cris_get_signed_offset (insn) < 0))
        {
          /* Immediate byte offset addressing prefix word with sp as base 
             register.  Used for CRIS v8 i.e. ETRAX 100 and newer if <val> 
             is between 64 and 128. 
             movem r<regsave>,[sp=sp-<val>] */
          val = -cris_get_signed_offset (insn);
          insn_next = read_memory_unsigned_integer (ip, sizeof (short));
          ip += sizeof (short);
          if (cris_get_mode (insn_next) == PREFIX_ASSIGN_MODE
              && cris_get_opcode (insn_next) == 0x000F
              && cris_get_size (insn_next) == 0x0003
              && cris_get_operand1 (insn_next) == SP_REGNUM)
            {
              if (frameless_p)
                {
                  return ip;
                }
              regsave = cris_get_operand2 (insn_next);
            }
          else
            {
              /* The prologue ended before the limit was reached.  */
              ip -= 2 * sizeof (short);
              break;
            }
        }
      else if (cris_get_mode (insn) == 0x0001
               && cris_get_opcode (insn) == 0x0009
               && cris_get_size (insn) == 0x0002)
        {
          /* move.d r<10..13>,r<0..15> */
          if (frameless_p)
            {
              return ip;
            }
          source_register = cris_get_operand1 (insn);
          if (source_register < ARG1_REGNUM || source_register > ARG4_REGNUM)
            {
              /* The prologue ended before the limit was reached.  */
              ip -= sizeof (short);
              break;
            }
        }
      else if (cris_get_operand2 (insn) == FP_REGNUM 
               /* The size is a fixed-size.  */
               && ((insn & 0x0F00) >> 8) == 0x0001 
               /* A negative offset.  */
               && (cris_get_signed_offset (insn) < 0))  
        {
          /* move.S rZ,[r8-U] (?) */
          insn_next = read_memory_unsigned_integer (ip, sizeof (short));
          ip += sizeof (short);
          regno = cris_get_operand2 (insn_next);
          if ((regno >= 0 && regno < SP_REGNUM)
              && cris_get_mode (insn_next) == PREFIX_OFFSET_MODE
              && cris_get_opcode (insn_next) == 0x000F)
            {
              /* move.S rZ,[r8-U] */
              continue;
            }
          else
            {
              /* The prologue ended before the limit was reached.  */
              ip -= 2 * sizeof (short);
              break;
            }
        }
      else if (cris_get_operand2 (insn) == FP_REGNUM 
               /* The size is a fixed-size.  */
               && ((insn & 0x0F00) >> 8) == 0x0001 
               /* A positive offset.  */
               && (cris_get_signed_offset (insn) > 0))  
        {
          /* move.S [r8+U],rZ (?) */
          insn_next = read_memory_unsigned_integer (ip, sizeof (short));
          ip += sizeof (short);
          regno = cris_get_operand2 (insn_next);
          if ((regno >= 0 && regno < SP_REGNUM)
              && cris_get_mode (insn_next) == PREFIX_OFFSET_MODE
              && cris_get_opcode (insn_next) == 0x0009
              && cris_get_operand1 (insn_next) == regno)
            {
              /* move.S [r8+U],rZ */
              continue;
            }
          else
            {
              /* The prologue ended before the limit was reached.  */
              ip -= 2 * sizeof (short);
              break;
            }
        }
      else
        {
          /* The prologue ended before the limit was reached.  */
          ip -= sizeof (short);
          break;
        }
    }
  while (ip < limit);

  /* We only want to know the end of the prologue when
     fi->saved_regs == 0.  */ 
  if (!fi->saved_regs)
    return ip;

  if (have_fp)
    {
      fi->saved_regs[FP_REGNUM] = FRAME_FP (fi);
      
      /* Calculate the addresses.  */
      for (regno = regsave; regno >= 0; regno--)
        {
          fi->saved_regs[regno] = FRAME_FP (fi) - val;
          val -= 4;
        }
      if (fi->extra_info->leaf_function)
        {
          /* Set the register SP to contain the stack pointer of 
             the caller.  */
          fi->saved_regs[SP_REGNUM] = FRAME_FP (fi) + 4;
        }
      else
        {
          /* Set the register SP to contain the stack pointer of 
             the caller.  */
          fi->saved_regs[SP_REGNUM] = FRAME_FP (fi) + 8;
      
          /* Set the register SRP to contain the return address of 
             the caller.  */
          fi->saved_regs[SRP_REGNUM] = FRAME_FP (fi) + 4;
        }
    }
  return ip;
}

/* Advance pc beyond any function entry prologue instructions at pc
   to reach some "real" code.  */

CORE_ADDR
cris_skip_prologue (CORE_ADDR pc)
{
  return cris_skip_prologue_main (pc, 0);
}

/* As cris_skip_prologue, but stops as soon as it knows that the function 
   has a frame.  Its result is equal to its input pc if the function is 
   frameless, unequal otherwise.  */

CORE_ADDR
cris_skip_prologue_frameless_p (CORE_ADDR pc)
{
  return cris_skip_prologue_main (pc, 1);
}

/* Given a PC value corresponding to the start of a function, return the PC
   of the first instruction after the function prologue.  */

CORE_ADDR
cris_skip_prologue_main (CORE_ADDR pc, int frameless_p)
{
  struct frame_info fi;
  static struct frame_extra_info fei;
  struct symtab_and_line sal = find_pc_line (pc, 0);
  int best_limit;
  CORE_ADDR pc_after_prologue;
  
  /* frame_info now contains dynamic memory.  Since fi is a dummy here,
     I use static memory for extra_info, and don't bother allocating
     memory for saved_regs.  */
  fi.saved_regs = 0;
  fi.extra_info = &fei;

  /* If there is no symbol information then sal.end == 0, and we end up
     examining only the first instruction in the function prologue. 
     Exaggerating the limit seems to be harmless.  */
  if (sal.end > 0)
    best_limit = sal.end;
  else
    best_limit = pc + 100; 

  pc_after_prologue = cris_examine (pc, best_limit, &fi, frameless_p);
  return pc_after_prologue;
}

/* Use the program counter to determine the contents and size of a breakpoint
   instruction.  It returns a pointer to a string of bytes that encode a
   breakpoint instruction, stores the length of the string to *lenptr, and
   adjusts pcptr (if necessary) to point to the actual memory location where
   the breakpoint should be inserted.  */

unsigned char *
cris_breakpoint_from_pc (CORE_ADDR *pcptr, int *lenptr)
{
  static unsigned char break_insn[] = {0x38, 0xe9};
  *lenptr = 2;

  return break_insn;
}

/* Returns the register SRP (subroutine return pointer) which must contain 
   the content of the register PC after a function call.  */

CORE_ADDR
cris_saved_pc_after_call ()
{
  return read_register (SRP_REGNUM);
}

/* Returns 1 if spec_reg is applicable to the current gdbarch's CRIS version,
   0 otherwise.  */

int
cris_spec_reg_applicable (struct cris_spec_reg spec_reg)
{
  int version = cris_version ();
  
  switch (spec_reg.applicable_version)
    {
    case cris_ver_version_all:
      return 1;
    case cris_ver_warning:
      /* Indeterminate/obsolete.  */
      return 0;
    case cris_ver_sim:
      /* Simulator only.  */
      return 0;
    case cris_ver_v0_3:
      return (version >= 0 && version <= 3);
    case cris_ver_v3p:
      return (version >= 3);
    case cris_ver_v8:
      return (version == 8 || version == 9);
    case cris_ver_v8p:
      return (version >= 8);
    case cris_ver_v10p:
      return (version >= 10);
    default:
      /* Invalid cris version.  */
      return 0;
    }
}

/* Returns the register size in unit byte.  Returns 0 for an unimplemented
   register, -1 for an invalid register.  */

int
cris_register_size (int regno)
{
  int i;
  int spec_regno;
  
  if (regno >= 0 && regno < NUM_GENREGS)
    {
      /* General registers (R0 - R15) are 32 bits.  */
      return 4;
    }
  else if (regno >= NUM_GENREGS && regno < NUM_REGS)
    {
      /* Special register (R16 - R31).  cris_spec_regs is zero-based. 
         Adjust regno accordingly.  */
      spec_regno = regno - NUM_GENREGS;
      
      /* The entries in cris_spec_regs are stored in register number order,
         which means we can shortcut into the array when searching it.  */
      for (i = spec_regno; cris_spec_regs[i].name != NULL; i++)
        {
          if (cris_spec_regs[i].number == spec_regno 
              && cris_spec_reg_applicable (cris_spec_regs[i]))
            /* Go with the first applicable register.  */
            return cris_spec_regs[i].reg_size;
        }
      /* Special register not applicable to this CRIS version.  */
      return 0;
    }
  else
    {
      /* Invalid register.  */
      return -1;
    }
}

/* Nonzero if regno should not be fetched from the target.  This is the case
   for unimplemented (size 0) and non-existant registers.  */

int
cris_cannot_fetch_register (int regno)
{
  return ((regno < 0 || regno >= NUM_REGS) 
          || (cris_register_size (regno) == 0));
}

/* Nonzero if regno should not be written to the target, for various 
   reasons.  */

int
cris_cannot_store_register (int regno)
{
  /* There are three kinds of registers we refuse to write to.
     1. Those that not implemented.
     2. Those that are read-only (depends on the processor mode).
     3. Those registers to which a write has no effect.
  */

  if (regno < 0 || regno >= NUM_REGS || cris_register_size (regno) == 0)
    /* Not implemented.  */
    return 1;

  else if  (regno == VR_REGNUM)
    /* Read-only.  */
    return 1;

  else if  (regno == P0_REGNUM || regno == P4_REGNUM || regno == P8_REGNUM)
    /* Writing has no effect.  */
    return 1;

  else if (cris_mode () == CRIS_MODE_USER)
    {
      if (regno == IBR_REGNUM || regno == BAR_REGNUM || regno == BRP_REGNUM 
          || regno == IRP_REGNUM)
        /* Read-only in user mode.  */
        return 1;
    }
  
  return 0;
}

/* Returns the register offset for the first byte of register regno's space 
   in the saved register state.  Returns -1 for an invalid or unimplemented
   register.  */

int
cris_register_offset (int regno)
{
  int i;
  int reg_size;
  int offset = 0;
  
  if (regno >= 0 && regno < NUM_REGS)
    {
      /* FIXME: The offsets should be cached and calculated only once,
         when the architecture being debugged has changed.  */
      for (i = 0; i < regno; i++)
        offset += cris_register_size (i);
      
      return offset;
    }
  else
    {
      /* Invalid register. */
      return -1;
    }
}

/* Return the GDB type (defined in gdbtypes.c) for the "standard" data type
   of data in register regno.  */

struct type *
cris_register_virtual_type (int regno)
{
  if (regno == SP_REGNUM || regno == PC_REGNUM
      || (regno > P8_REGNUM && regno < USP_REGNUM))
    {
      /* SP, PC, IBR, IRP, SRP, BAR, DCCR, BRP */
      return lookup_pointer_type (builtin_type_void);
    }
  else if (regno == P8_REGNUM || regno == USP_REGNUM
           || (regno >= 0 && regno < SP_REGNUM))
    {
      /* R0 - R13, P8, P15 */
      return builtin_type_unsigned_long;
    }
  else if (regno > P3_REGNUM && regno < P8_REGNUM)
    {
      /* P4, CCR, DCR0, DCR1 */
      return builtin_type_unsigned_short;
    }
  else if (regno > PC_REGNUM && regno < P4_REGNUM)
    {
      /* P0, P1, P2, P3 */
      return builtin_type_unsigned_char;
    }
  else
    {
      /* Invalid register.  */
      return builtin_type_void;
    }
}

/* Stores a function return value of type type, where valbuf is the address 
   of the value to be stored.  */

/* In the original CRIS ABI, R10 is used to store return values.  */

void
cris_abi_original_store_return_value (struct type *type, char *valbuf)
{
  int len = TYPE_LENGTH (type);
  
  if (len <= REGISTER_SIZE) 
    write_register_bytes (REGISTER_BYTE (RET_REGNUM), valbuf, len);
  else
    internal_error (__FILE__, __LINE__, "cris_abi_original_store_return_value: type length too large.");
}

/* In the CRIS ABI V2, R10 and R11 are used to store return values.  */

void
cris_abi_v2_store_return_value (struct type *type, char *valbuf)
{
  int len = TYPE_LENGTH (type);
  
  if (len <= 2 * REGISTER_SIZE)
    {
      /* Note that this works since R10 and R11 are consecutive registers.  */
      write_register_bytes (REGISTER_BYTE (RET_REGNUM), valbuf, len);
    }
  else
    internal_error (__FILE__, __LINE__, "cris_abi_v2_store_return_value: type length too large.");
}

/* Return the name of register regno as a string. Return NULL for an invalid or
   unimplemented register.  */

char *
cris_register_name (int regno)
{
  static char *cris_genreg_names[] =
  { "r0",  "r1",  "r2",  "r3", \
    "r4",  "r5",  "r6",  "r7", \
    "r8",  "r9",  "r10", "r11", \
    "r12", "r13", "sp",  "pc" };

  int i;
  int spec_regno;

  if (regno >= 0 && regno < NUM_GENREGS)
    {
      /* General register.  */
      return cris_genreg_names[regno];
    }
  else if (regno >= NUM_GENREGS && regno < NUM_REGS)
    {
      /* Special register (R16 - R31).  cris_spec_regs is zero-based. 
         Adjust regno accordingly.  */
      spec_regno = regno - NUM_GENREGS;
      
      /* The entries in cris_spec_regs are stored in register number order,
         which means we can shortcut into the array when searching it.  */
      for (i = spec_regno; cris_spec_regs[i].name != NULL; i++)
        {
          if (cris_spec_regs[i].number == spec_regno 
              && cris_spec_reg_applicable (cris_spec_regs[i]))
            /* Go with the first applicable register.  */
            return cris_spec_regs[i].name;
        }
      /* Special register not applicable to this CRIS version.  */
      return NULL;
    }
  else
    {
      /* Invalid register.  */
      return NULL;
    }
}

int
cris_register_bytes_ok (long bytes)
{
  return (bytes == REGISTER_BYTES);
}

/* Extract from an array regbuf containing the raw register state a function
   return value of type type, and copy that, in virtual format, into 
   valbuf.  */

/* In the original CRIS ABI, R10 is used to return values.  */

void
cris_abi_original_extract_return_value (struct type *type, char *regbuf, 
                                        char *valbuf)
{
  int len = TYPE_LENGTH (type);
  
  if (len <= REGISTER_SIZE)
    memcpy (valbuf, regbuf + REGISTER_BYTE (RET_REGNUM), len);
  else
    internal_error (__FILE__, __LINE__, "cris_abi_original_extract_return_value: type length too large");
}

/* In the CRIS ABI V2, R10 and R11 are used to store return values.  */

void
cris_abi_v2_extract_return_value (struct type *type, char *regbuf, 
                                  char *valbuf)
{
  int len = TYPE_LENGTH (type);
  
  if (len <= 2 * REGISTER_SIZE)
    memcpy (valbuf, regbuf + REGISTER_BYTE (RET_REGNUM), len);
  else
    internal_error (__FILE__, __LINE__, "cris_abi_v2_extract_return_value: type length too large");
}

/* Store the address of the place in which to copy the structure the
   subroutine will return.  In the CRIS ABI, R9 is used in order to pass 
   the address of the allocated area where a structure return value must 
   be stored.  R9 is call-clobbered, which means we must save it here for
   later use.  */

void
cris_store_struct_return (CORE_ADDR addr, CORE_ADDR sp)
{
  write_register (STR_REGNUM, addr);
  struct_return_address = addr;
}

/* Extract from regbuf the address where a function should return a 
   structure value.  It's not there in the CRIS ABI, so we must do it another
   way.  */

CORE_ADDR
cris_extract_struct_value_address (char *regbuf)
{
  return struct_return_address;
}

/* Returns 1 if a value of the given type being returned from a function 
   must have space allocated for it on the stack.  gcc_p is true if the 
   function being considered is known to have been compiled by GCC. 
   In the CRIS ABI, structure return values are passed to the called 
   function by reference in register R9 to a caller-allocated area, so
   this is always true.  */

int
cris_use_struct_convention (int gcc_p, struct type *type)
{
  return 1;
}

/* Returns 1 if the given type will be passed by pointer rather than 
   directly.  */

/* In the original CRIS ABI, arguments shorter than or equal to 32 bits are 
   passed by value.  */

int 
cris_abi_original_reg_struct_has_addr (int gcc_p, struct type *type)
{ 
  return (TYPE_LENGTH (type) > 4);
}

/* In the CRIS ABI V2, arguments shorter than or equal to 64 bits are passed
   by value.  */

int 
cris_abi_v2_reg_struct_has_addr (int gcc_p, struct type *type)
{ 
  return (TYPE_LENGTH (type) > 8);
}

/* Returns 1 if the function invocation represented by fi does not have a 
   stack frame associated with it.  Otherwise return 0.  */

int
cris_frameless_function_invocation (struct frame_info *fi)
{
  if (fi->signal_handler_caller)
    return 0;
  else
    return frameless_look_for_prologue (fi);
}

/* See frame.h.  Determines the address of all registers in the current stack
   frame storing each in frame->saved_regs.  Space for frame->saved_regs shall
   be allocated by FRAME_INIT_SAVED_REGS using either frame_saved_regs_zalloc
   or frame_obstack_alloc.  */

void
cris_frame_init_saved_regs (struct frame_info *fi)
{
  CORE_ADDR ip;
  struct symtab_and_line sal;
  int best_limit;
  char *dummy_regs = generic_find_dummy_frame (fi->pc, fi->frame);
  
  /* Examine the entire prologue.  */
  register int frameless_p = 0; 

  /* Has this frame's registers already been initialized?  */
  if (fi->saved_regs)
    return;

  frame_saved_regs_zalloc (fi);
  
  if (dummy_regs)
    {
      /* I don't see this ever happening, considering the context in which
         cris_frame_init_saved_regs is called (always when we're not in
         a dummy frame).  */
      memcpy (&fi->saved_regs, dummy_regs, sizeof (fi->saved_regs));
    }
  else
    {    
      ip = get_pc_function_start (fi->pc);
      sal = find_pc_line (ip, 0);

      /* If there is no symbol information then sal.end == 0, and we end up
         examining only the first instruction in the function prologue. 
         Exaggerating the limit seems to be harmless.  */
      if (sal.end > 0)
        best_limit = sal.end;
      else
        best_limit = ip + 100;

      cris_examine (ip, best_limit, fi, frameless_p);
    }
}

/* Initialises the extra frame information at the creation of a new frame. 
   The inparameter fromleaf is 0 when the call is from create_new_frame. 
   When the call is from get_prev_frame_info, fromleaf is determined by
   cris_frameless_function_invocation.  */

void
cris_init_extra_frame_info (int fromleaf, struct frame_info *fi)
{
  if (fi->next)
    {
      /* Called from get_prev_frame.  */
      fi->pc = FRAME_SAVED_PC (fi->next);
    }
 
  fi->extra_info = (struct frame_extra_info *)
    frame_obstack_alloc (sizeof (struct frame_extra_info));
 
  fi->extra_info->return_pc = 0;
  fi->extra_info->leaf_function = 0;

  if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
    {    
      /* We need to setup fi->frame here because run_stack_dummy gets it wrong
         by assuming it's always FP.  */
      fi->frame = generic_read_register_dummy (fi->pc, fi->frame, SP_REGNUM);
      fi->extra_info->return_pc = 
        generic_read_register_dummy (fi->pc, fi->frame, PC_REGNUM);

      /* FIXME: Is this necessarily true?  */
      fi->extra_info->leaf_function = 0;
    }
  else
    {
      cris_frame_init_saved_regs (fi);

      /* Check fromleaf/frameless_function_invocation.  (FIXME)  */

      if (fi->saved_regs[SRP_REGNUM] != 0)
        {
          /* SRP was saved on the stack; non-leaf function.  */
          fi->extra_info->return_pc =
            read_memory_integer (fi->saved_regs[SRP_REGNUM], 
                                 REGISTER_RAW_SIZE (SRP_REGNUM));
        }
      else
        {
          /* SRP is still in a register; leaf function.  */
          fi->extra_info->return_pc = read_register (SRP_REGNUM);
          /* FIXME: Should leaf_function be set to 1 here?  */
          fi->extra_info->leaf_function = 1;
        }
    }
}

/* Return the content of the frame pointer in the present frame.  In other
   words, determine the address of the calling function's frame.  */

CORE_ADDR
cris_frame_chain (struct frame_info *fi)
{
  if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
    {
      return fi->frame;
    }
  else if (!inside_entry_file (fi->pc))
    {
      return read_memory_unsigned_integer (FRAME_FP (fi), 4);
    }
  else
    {
      return 0;
    }
}

/* Return the saved PC (which equals the return address) of this frame.  */

CORE_ADDR
cris_frame_saved_pc (struct frame_info *fi)
{
  return fi->extra_info->return_pc;
}

/* Return the address of the argument block for the frame described 
   by struct frame_info.  */

CORE_ADDR
cris_frame_args_address (struct frame_info *fi)
{
  return FRAME_FP (fi);
}

/* Return the address of the locals block for the frame
   described by struct frame_info.  */

CORE_ADDR
cris_frame_locals_address (struct frame_info *fi)
{
  return FRAME_FP (fi);
}

/* Setup the function arguments for calling a function in the inferior.  */

CORE_ADDR 
cris_abi_original_push_arguments (int nargs, struct value **args, 
                                  CORE_ADDR sp, int struct_return, 
                                  CORE_ADDR struct_addr)
{
  int stack_alloc;
  int stack_offset;
  int argreg;
  int argnum;
  struct type *type;
  int len;
  CORE_ADDR regval;
  char *val;

  /* Data and parameters reside in different areas on the stack. 
     Both frame pointers grow toward higher addresses.  */  
  CORE_ADDR fp_params;
  CORE_ADDR fp_data;
  
  /* Are we returning a value using a structure return or a normal value 
     return?  struct_addr is the address of the reserved space for the return 
     structure to be written on the stack.  */
  if (struct_return)
    {
      write_register (STR_REGNUM, struct_addr);
    }

  /* Make sure there's space on the stack.  Allocate space for data and a 
     parameter to refer to that data.  */
  for (argnum = 0, stack_alloc = 0; argnum < nargs; argnum++)
    stack_alloc += (TYPE_LENGTH (VALUE_TYPE (args[argnum])) + REGISTER_SIZE);
  sp -= stack_alloc;
  /* We may over-allocate a little here, but that won't hurt anything.  */

  /* Initialize stack frame pointers.  */
  fp_params = sp;
  fp_data = sp + (nargs * REGISTER_SIZE);

  /* Now load as many as possible of the first arguments into
     registers, and push the rest onto the stack.  */
  argreg = ARG1_REGNUM; 
  stack_offset = 0;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      type = VALUE_TYPE (args[argnum]);
      len = TYPE_LENGTH (type);
      val = (char *) VALUE_CONTENTS (args[argnum]);
    
      if (len <= REGISTER_SIZE && argreg <= ARG4_REGNUM)
        {
          /* Data fits in a register; put it in the first available 
             register.  */
          write_register (argreg, *(unsigned long *) val);
          argreg++;
        }
      else if (len > REGISTER_SIZE && argreg <= ARG4_REGNUM)
        {
          /* Data does not fit in register; pass it on the stack and
             put its address in the first available register.  */
          write_memory (fp_data, val, len);
          write_register (argreg, fp_data);
          fp_data += len;
          argreg++;      
        }
      else if (len > REGISTER_SIZE)
        {
          /* Data does not fit in register; put both data and 
             parameter on the stack.  */
          write_memory (fp_data, val, len);
          write_memory (fp_params, (char *) (&fp_data), REGISTER_SIZE);
          fp_data += len;
          fp_params += REGISTER_SIZE;
        }
      else
        {
          /* Data fits in a register, but we are out of registers;
             put the parameter on the stack.  */
          write_memory (fp_params, val, REGISTER_SIZE);
          fp_params += REGISTER_SIZE;
        }
    }

  return sp;
}

CORE_ADDR 
cris_abi_v2_push_arguments (int nargs, struct value **args, CORE_ADDR sp, 
                     int struct_return, CORE_ADDR struct_addr)
{
  int stack_alloc;
  int stack_offset;
  int argreg;
  int argnum;

  CORE_ADDR regval;

  /* The function's arguments and memory allocated by gdb for the arguments to
     point at reside in separate areas on the stack.
     Both frame pointers grow toward higher addresses.  */
  CORE_ADDR fp_arg;
  CORE_ADDR fp_mem;
  
  /* Are we returning a value using a structure return or a normal value 
     return?  struct_addr is the address of the reserved space for the return 
     structure to be written on the stack.  */
  if (struct_return)
    {
      write_register (STR_REGNUM, struct_addr);
    }

  /* Allocate enough to keep things word-aligned on both parts of the 
     stack.  */
  stack_alloc = 0;
  for (argnum = 0; argnum < nargs; argnum++)
    {
      int len;
      int reg_demand;
      
      len = TYPE_LENGTH (VALUE_TYPE (args[argnum]));
      reg_demand = (len / REGISTER_SIZE) + (len % REGISTER_SIZE != 0 ? 1 : 0);

      /* reg_demand * REGISTER_SIZE is the amount of memory we might need to
         allocate for this argument.  2 * REGISTER_SIZE is the amount of stack
         space we might need to pass the argument itself (either by value or by
         reference).  */
      stack_alloc += (reg_demand * REGISTER_SIZE + 2 * REGISTER_SIZE);
    }
  sp -= stack_alloc;
  /* We may over-allocate a little here, but that won't hurt anything.  */

  /* Initialize frame pointers.  */
  fp_arg = sp;
  fp_mem = sp + (nargs * (2 * REGISTER_SIZE));

  /* Now load as many as possible of the first arguments into registers,
     and push the rest onto the stack.  */
  argreg = ARG1_REGNUM; 
  stack_offset = 0;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      int len;
      char *val;
      int reg_demand;
      int i;
      
      len = TYPE_LENGTH (VALUE_TYPE (args[argnum]));
      val = (char *) VALUE_CONTENTS (args[argnum]);
      
      /* How may registers worth of storage do we need for this argument?  */
      reg_demand = (len / REGISTER_SIZE) + (len % REGISTER_SIZE != 0 ? 1 : 0);
        
      if (len <= (2 * REGISTER_SIZE)
          && (argreg + reg_demand - 1 <= ARG4_REGNUM)) 
        {
          /* Data passed by value.  Fits in available register(s).  */
          for (i = 0; i < reg_demand; i++)
            {
              write_register (argreg, *(unsigned long *) val);
              argreg++;
              val += REGISTER_SIZE;
            }
        }
      else if (len <= (2 * REGISTER_SIZE) && argreg <= ARG4_REGNUM)
        {
          /* Data passed by value. Does not fit in available register(s).  
             Use the register(s) first, then the stack.  */
          for (i = 0; i < reg_demand; i++)
            {
              if (argreg <= ARG4_REGNUM)
                {
                  write_register (argreg, *(unsigned long *) val);
                  argreg++;
                  val += REGISTER_SIZE;
                }
              else
                {
                  /* I guess this memory write could write the remaining data
                     all at once instead of in REGISTER_SIZE chunks.  */
                  write_memory (fp_arg, val, REGISTER_SIZE);
                  fp_arg += REGISTER_SIZE;
                  val += REGISTER_SIZE;              
                }
            }    
        }
      else if (len > (2 * REGISTER_SIZE))
        {
          /* Data passed by reference.  Put it on the stack.  */
          write_memory (fp_mem, val, len);
          write_memory (fp_arg, (char *) (&fp_mem), REGISTER_SIZE);

          /* fp_mem need not be word-aligned since it's just a chunk of
             memory being pointed at.  That is, += len would do.  */
          fp_mem += reg_demand * REGISTER_SIZE;
          fp_arg += REGISTER_SIZE;
        }
      else
        {
          /* Data passed by value.  No available registers.  Put it on 
             the stack.  */
          write_memory (fp_arg, val, len);

          /* fp_arg must be word-aligned (i.e., don't += len) to match
             the function prologue.  */
          fp_arg += reg_demand * REGISTER_SIZE;
        }
    }

  return sp;
}

/* Never put the return address on the stack.  The register SRP is pushed
   by the called function unless it is a leaf-function.  Due to the BRP
   register the PC will change when continue is sent.  */

CORE_ADDR
cris_push_return_address (CORE_ADDR pc, CORE_ADDR sp)
{
  write_register (SRP_REGNUM, CALL_DUMMY_ADDRESS ());
  return sp;
}

/* Restore the machine to the state it had before the current frame 
   was created.  Discard the innermost frame from the stack and restore 
   all saved registers.  */

void 
cris_pop_frame ()
{
  register struct frame_info *fi = get_current_frame ();
  register int regno;
  register int stack_offset = 0;
  
  if (PC_IN_CALL_DUMMY (fi->pc, fi->frame, fi->frame))
    {
      /* This happens when we hit a breakpoint set at the entry point,
         when returning from a dummy frame.  */
      generic_pop_dummy_frame ();
    }
  else
    {
      cris_frame_init_saved_regs (fi);

      /* For each register, the address of where it was saved on entry to
         the frame now lies in fi->saved_regs[regno], or zero if it was not 
         saved.  This includes special registers such as PC and FP saved in
         special ways in the stack frame.  The SP_REGNUM is even more
         special, the address here is the SP for the next frame, not the
         address where the SP was saved.  */
                                                     
      /* Restore general registers R0 - R7.  They were pushed on the stack 
         after SP was saved.  */
      for (regno = 0; regno < FP_REGNUM; regno++)
        {
          if (fi->saved_regs[regno])
            {
              write_register (regno, 
                              read_memory_integer (fi->saved_regs[regno], 4));
            }
        }
     
      if (fi->saved_regs[FP_REGNUM])
        {
          /* Pop the frame pointer (R8).  It was pushed before SP 
             was saved.  */
          write_register (FP_REGNUM, 
                          read_memory_integer (fi->saved_regs[FP_REGNUM], 4));
          stack_offset += 4;

          /* Not a leaf function.  */
          if (fi->saved_regs[SRP_REGNUM])
            {     
              /* SRP was pushed before SP was saved.  */
              stack_offset += 4;
            }
      
          /* Restore the SP and adjust for R8 and (possibly) SRP.  */
          write_register (SP_REGNUM, fi->saved_regs[FP_REGNUM] + stack_offset);
        } 
      else
        {
          /* Currently, we can't get the correct info into fi->saved_regs 
             without a frame pointer.  */
        }
    
      /* Restore the PC.  */
      write_register (PC_REGNUM, fi->extra_info->return_pc);
    }
  flush_cached_frames ();
}

/* Calculates a value that measures how good inst_args constraints an 
   instruction.  It stems from cris_constraint, found in cris-dis.c.  */

static int
constraint (unsigned int insn, const signed char *inst_args, 
            inst_env_type *inst_env)
{
  int retval = 0;
  int tmp, i;

  const char *s = inst_args;

  for (; *s; s++)
    switch (*s) 
      {
      case 'm':
        if ((insn & 0x30) == 0x30)
          return -1;
        break;
        
      case 'S':
        /* A prefix operand.  */
        if (inst_env->prefix_found)
          break;
        else
          return -1;

      case 'B':
        /* A "push" prefix.  (This check was REMOVED by san 970921.)  Check for
           valid "push" size.  In case of special register, it may be != 4.  */
        if (inst_env->prefix_found)
          break;
        else
          return -1;

      case 'D':
        retval = (((insn >> 0xC) & 0xF) == (insn & 0xF));
        if (!retval)
          return -1;
        else 
          retval += 4;
        break;

      case 'P':
        tmp = (insn >> 0xC) & 0xF;

        for (i = 0; cris_spec_regs[i].name != NULL; i++)
          {
            /* Since we match four bits, we will give a value of
               4 - 1 = 3 in a match.  If there is a corresponding
               exact match of a special register in another pattern, it
               will get a value of 4, which will be higher.  This should
               be correct in that an exact pattern would match better that
               a general pattern.
               Note that there is a reason for not returning zero; the
               pattern for "clear" is partly  matched in the bit-pattern
               (the two lower bits must be zero), while the bit-pattern
               for a move from a special register is matched in the
               register constraint.
               This also means we will will have a race condition if
               there is a partly match in three bits in the bit pattern.  */
            if (tmp == cris_spec_regs[i].number)
              {
                retval += 3;
                break;
              }
          }
        
        if (cris_spec_regs[i].name == NULL)
          return -1;
        break;
      }
  return retval;
}

/* Returns the number of bits set in the variable value.  */

static int
number_of_bits (unsigned int value)
{
  int number_of_bits = 0;
  
  while (value != 0)
    {
      number_of_bits += 1;
      value &= (value - 1);
    }
  return number_of_bits;
}

/* Finds the address that should contain the single step breakpoint(s). 
   It stems from code in cris-dis.c.  */

static int
find_cris_op (unsigned short insn, inst_env_type *inst_env)
{
  int i;
  int max_level_of_match = -1;
  int max_matched = -1;
  int level_of_match;

  for (i = 0; cris_opcodes[i].name != NULL; i++)
    {
      if (((cris_opcodes[i].match & insn) == cris_opcodes[i].match) 
          && ((cris_opcodes[i].lose & insn) == 0))
        {
          level_of_match = constraint (insn, cris_opcodes[i].args, inst_env);
          if (level_of_match >= 0)
            {
              level_of_match +=
                number_of_bits (cris_opcodes[i].match | cris_opcodes[i].lose);
              if (level_of_match > max_level_of_match)
                {
                  max_matched = i;
                  max_level_of_match = level_of_match;
                  if (level_of_match == 16)
                    {
                      /* All bits matched, cannot find better.  */
                      break;
                    }
                }
            }
        }
    }
  return max_matched;
}

/* Attempts to find single-step breakpoints.  Returns -1 on failure which is
   actually an internal error.  */

static int
find_step_target (inst_env_type *inst_env)
{
  int i;
  int offset;
  unsigned short insn;

  /* Create a local register image and set the initial state.  */
  for (i = 0; i < NUM_GENREGS; i++)
    {
      inst_env->reg[i] = (unsigned long) read_register (i);
    }
  offset = NUM_GENREGS;
  for (i = 0; i < NUM_SPECREGS; i++)
    {
      inst_env->preg[i] = (unsigned long) read_register (offset + i);
    }
  inst_env->branch_found = 0;
  inst_env->slot_needed = 0;
  inst_env->delay_slot_pc_active = 0;
  inst_env->prefix_found = 0;
  inst_env->invalid = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;

  /* Look for a step target.  */
  do
    {
      /* Read an instruction from the client.  */
      insn = read_memory_unsigned_integer (inst_env->reg[PC_REGNUM], 2);

      /* If the instruction is not in a delay slot the new content of the
         PC is [PC] + 2.  If the instruction is in a delay slot it is not
         that simple.  Since a instruction in a delay slot cannot change 
         the content of the PC, it does not matter what value PC will have. 
         Just make sure it is a valid instruction.  */
      if (!inst_env->delay_slot_pc_active)
        {
          inst_env->reg[PC_REGNUM] += 2;
        }
      else
        {
          inst_env->delay_slot_pc_active = 0;
          inst_env->reg[PC_REGNUM] = inst_env->delay_slot_pc;
        }
      /* Analyse the present instruction.  */
      i = find_cris_op (insn, inst_env);
      if (i == -1)
        {
          inst_env->invalid = 1;
        }
      else
        {
          cris_gdb_func (cris_opcodes[i].op, insn, inst_env);
        }
    } while (!inst_env->invalid 
             && (inst_env->prefix_found || inst_env->xflag_found 
                 || inst_env->slot_needed));
  return i;
}

/* There is no hardware single-step support.  The function find_step_target
   digs through the opcodes in order to find all possible targets. 
   Either one ordinary target or two targets for branches may be found.  */

void
cris_software_single_step (enum target_signal ignore, int insert_breakpoints)
{
  inst_env_type inst_env;
  
  if (insert_breakpoints)
    {
      /* Analyse the present instruction environment and insert 
         breakpoints.  */
      int status = find_step_target (&inst_env);
      if (status == -1)
        {
          /* Could not find a target.  FIXME: Should do something.  */
        }
      else
        {
          /* Insert at most two breakpoints.  One for the next PC content
             and possibly another one for a branch, jump, etc.  */
          next_pc = (CORE_ADDR) inst_env.reg[PC_REGNUM];
          target_insert_breakpoint (next_pc, break_mem[0]);
          if (inst_env.branch_found 
              && (CORE_ADDR) inst_env.branch_break_address != next_pc)
            {
              branch_target_address = 
                (CORE_ADDR) inst_env.branch_break_address;
              target_insert_breakpoint (branch_target_address, break_mem[1]);
              branch_break_inserted = 1;
            }
        }
    }
  else
    {
      /* Remove breakpoints.  */
      target_remove_breakpoint (next_pc, break_mem[0]);
      if (branch_break_inserted)
        {
          target_remove_breakpoint (branch_target_address, break_mem[1]);
          branch_break_inserted = 0;
        }
    }
}

/* Calculates the prefix value for quick offset addressing mode.  */

void
quick_mode_bdap_prefix (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to be in a delay slot.  You can't have a prefix to this
     instruction (not 100% sure).  */
  if (inst_env->slot_needed || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return; 
    }
 
  inst_env->prefix_value = inst_env->reg[cris_get_operand2 (inst)];
  inst_env->prefix_value += cris_get_bdap_quick_offset (inst);

  /* A prefix doesn't change the xflag_found.  But the rest of the flags
     need updating.  */
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 1;
}

/* Updates the autoincrement register.  The size of the increment is derived 
   from the size of the operation.  The PC is always kept aligned on even
   word addresses.  */

void 
process_autoincrement (int size, unsigned short inst, inst_env_type *inst_env)
{
  if (size == INST_BYTE_SIZE)
    {
      inst_env->reg[cris_get_operand1 (inst)] += 1;

      /* The PC must be word aligned, so increase the PC with one
         word even if the size is byte.  */
      if (cris_get_operand1 (inst) == REG_PC)
        {
          inst_env->reg[REG_PC] += 1;
        }
    }
  else if (size == INST_WORD_SIZE)
    {
      inst_env->reg[cris_get_operand1 (inst)] += 2;
    }
  else if (size == INST_DWORD_SIZE)
    {
      inst_env->reg[cris_get_operand1 (inst)] += 4;
    }
  else
    {
      /* Invalid size.  */
      inst_env->invalid = 1;
    }
}

/* Calculates the prefix value for the general case of offset addressing 
   mode.  */

void
bdap_prefix (unsigned short inst, inst_env_type *inst_env)
{

  long offset;

  /* It's invalid to be in a delay slot.  */
  if (inst_env->slot_needed || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return; 
    }

  /* The calculation of prefix_value used to be after process_autoincrement,
     but that fails for an instruction such as jsr [$r0+12] which is encoded
     as 5f0d 0c00 30b9 when compiled with -fpic.  Since PC is operand1 it
     mustn't be incremented until we have read it and what it points at.  */
  inst_env->prefix_value = inst_env->reg[cris_get_operand2 (inst)];

  /* The offset is an indirection of the contents of the operand1 register.  */
  inst_env->prefix_value += 
    read_memory_integer (inst_env->reg[cris_get_operand1 (inst)], 
                         cris_get_size (inst));

  if (cris_get_mode (inst) == AUTOINC_MODE)
    {
      process_autoincrement (cris_get_size (inst), inst, inst_env); 
    }
   
  /* A prefix doesn't change the xflag_found.  But the rest of the flags
     need updating.  */
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 1;
}

/* Calculates the prefix value for the index addressing mode.  */

void
biap_prefix (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to be in a delay slot.  I can't see that it's possible to
     have a prefix to this instruction.  So I will treat this as invalid.  */
  if (inst_env->slot_needed || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  
  inst_env->prefix_value = inst_env->reg[cris_get_operand1 (inst)];

  /* The offset is the operand2 value shifted the size of the instruction 
     to the left.  */
  inst_env->prefix_value += 
    inst_env->reg[cris_get_operand2 (inst)] << cris_get_size (inst);
  
  /* If the PC is operand1 (base) the address used is the address after 
     the main instruction, i.e. address + 2 (the PC is already compensated
     for the prefix operation).  */
  if (cris_get_operand1 (inst) == REG_PC)
    {
      inst_env->prefix_value += 2;
    }

  /* A prefix doesn't change the xflag_found.  But the rest of the flags
     need updating.  */
  inst_env->slot_needed = 0;
  inst_env->xflag_found = 0;
  inst_env->prefix_found = 1;
}

/* Calculates the prefix value for the double indirect addressing mode.  */

void 
dip_prefix (unsigned short inst, inst_env_type *inst_env)
{

  CORE_ADDR address;

  /* It's invalid to be in a delay slot.  */
  if (inst_env->slot_needed || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  
  /* The prefix value is one dereference of the contents of the operand1
     register.  */
  address = (CORE_ADDR) inst_env->reg[cris_get_operand1 (inst)];
  inst_env->prefix_value = read_memory_unsigned_integer (address, 4);
    
  /* Check if the mode is autoincrement.  */
  if (cris_get_mode (inst) == AUTOINC_MODE)
    {
      inst_env->reg[cris_get_operand1 (inst)] += 4;
    }

  /* A prefix doesn't change the xflag_found.  But the rest of the flags
     need updating.  */
  inst_env->slot_needed = 0;
  inst_env->xflag_found = 0;
  inst_env->prefix_found = 1;
}

/* Finds the destination for a branch with 8-bits offset.  */

void
eight_bit_offset_branch_op (unsigned short inst, inst_env_type *inst_env)
{

  short offset;

  /* If we have a prefix or are in a delay slot it's bad.  */
  if (inst_env->slot_needed || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  
  /* We have a branch, find out where the branch will land.  */
  offset = cris_get_branch_short_offset (inst);

  /* Check if the offset is signed.  */
  if (offset & BRANCH_SIGNED_SHORT_OFFSET_MASK)
    {
      offset |= 0xFF00;
    }
  
  /* The offset ends with the sign bit, set it to zero.  The address
     should always be word aligned.  */
  offset &= ~BRANCH_SIGNED_SHORT_OFFSET_MASK;
  
  inst_env->branch_found = 1;
  inst_env->branch_break_address = inst_env->reg[REG_PC] + offset;

  inst_env->slot_needed = 1;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Finds the destination for a branch with 16-bits offset.  */

void 
sixteen_bit_offset_branch_op (unsigned short inst, inst_env_type *inst_env)
{
  short offset;

  /* If we have a prefix or is in a delay slot it's bad.  */
  if (inst_env->slot_needed || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* We have a branch, find out the offset for the branch.  */
  offset = read_memory_integer (inst_env->reg[REG_PC], 2);

  /* The instruction is one word longer than normal, so add one word
     to the PC.  */
  inst_env->reg[REG_PC] += 2;

  inst_env->branch_found = 1;
  inst_env->branch_break_address = inst_env->reg[REG_PC] + offset;


  inst_env->slot_needed = 1;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles the ABS instruction.  */

void 
abs_op (unsigned short inst, inst_env_type *inst_env)
{

  long value;
  
  /* ABS can't have a prefix, so it's bad if it does.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Check if the operation affects the PC.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
    
      /* It's invalid to change to the PC if we are in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }

      value = (long) inst_env->reg[REG_PC];

      /* The value of abs (SIGNED_DWORD_MASK) is SIGNED_DWORD_MASK.  */
      if (value != SIGNED_DWORD_MASK)
        {
          value = -value;
          inst_env->reg[REG_PC] = (long) value;
        }
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the ADDI instruction.  */

void 
addi_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to have the PC as base register.  And ADDI can't have
     a prefix.  */
  if (inst_env->prefix_found || (cris_get_operand1 (inst) == REG_PC))
    {
      inst_env->invalid = 1;
      return;
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the ASR instruction.  */

void 
asr_op (unsigned short inst, inst_env_type *inst_env)
{
  int shift_steps;
  unsigned long value;
  unsigned long signed_extend_mask = 0;

  /* ASR can't have a prefix, so check that it doesn't.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Check if the PC is the target register.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      /* Get the number of bits to shift.  */
      shift_steps = cris_get_asr_shift_steps (inst_env->reg[cris_get_operand1 (inst)]);
      value = inst_env->reg[REG_PC];

      /* Find out how many bits the operation should apply to.  */
      if (cris_get_size (inst) == INST_BYTE_SIZE)
        {
          if (value & SIGNED_BYTE_MASK)
            {
              signed_extend_mask = 0xFF;
              signed_extend_mask = signed_extend_mask >> shift_steps;
              signed_extend_mask = ~signed_extend_mask;
            }
          value = value >> shift_steps;
          value |= signed_extend_mask;
          value &= 0xFF;
          inst_env->reg[REG_PC] &= 0xFFFFFF00;
          inst_env->reg[REG_PC] |= value;
        }
      else if (cris_get_size (inst) == INST_WORD_SIZE)
        {
          if (value & SIGNED_WORD_MASK)
            {
              signed_extend_mask = 0xFFFF;
              signed_extend_mask = signed_extend_mask >> shift_steps;
              signed_extend_mask = ~signed_extend_mask;
            }
          value = value >> shift_steps;
          value |= signed_extend_mask;
          value &= 0xFFFF;
          inst_env->reg[REG_PC] &= 0xFFFF0000;
          inst_env->reg[REG_PC] |= value;
        }
      else if (cris_get_size (inst) == INST_DWORD_SIZE)
        {
          if (value & SIGNED_DWORD_MASK)
            {
              signed_extend_mask = 0xFFFFFFFF;
              signed_extend_mask = signed_extend_mask >> shift_steps;
              signed_extend_mask = ~signed_extend_mask;
            }
          value = value >> shift_steps;
          value |= signed_extend_mask;
          inst_env->reg[REG_PC]  = value;
        }
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the ASRQ instruction.  */

void 
asrq_op (unsigned short inst, inst_env_type *inst_env)
{

  int shift_steps;
  unsigned long value;
  unsigned long signed_extend_mask = 0;
  
  /* ASRQ can't have a prefix, so check that it doesn't.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Check if the PC is the target register.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {

      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      /* The shift size is given as a 5 bit quick value, i.e. we don't
         want the the sign bit of the quick value.  */
      shift_steps = cris_get_asr_shift_steps (inst);
      value = inst_env->reg[REG_PC];
      if (value & SIGNED_DWORD_MASK)
        {
          signed_extend_mask = 0xFFFFFFFF;
          signed_extend_mask = signed_extend_mask >> shift_steps;
          signed_extend_mask = ~signed_extend_mask;
        }
      value = value >> shift_steps;
      value |= signed_extend_mask;
      inst_env->reg[REG_PC]  = value;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the AX, EI and SETF instruction.  */

void 
ax_ei_setf_op (unsigned short inst, inst_env_type *inst_env)
{
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  /* Check if the instruction is setting the X flag.  */
  if (cris_is_xflag_bit_on (inst))
    {
      inst_env->xflag_found = 1;
    }
  else
    {
      inst_env->xflag_found = 0;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Checks if the instruction is in assign mode.  If so, it updates the assign 
   register.  Note that check_assign assumes that the caller has checked that
   there is a prefix to this instruction.  The mode check depends on this.  */

void 
check_assign (unsigned short inst, inst_env_type *inst_env)
{
  /* Check if it's an assign addressing mode.  */
  if (cris_get_mode (inst) == PREFIX_ASSIGN_MODE)
    {
      /* Assign the prefix value to operand 1.  */
      inst_env->reg[cris_get_operand1 (inst)] = inst_env->prefix_value;
    }
}

/* Handles the 2-operand BOUND instruction.  */

void 
two_operand_bound_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to have the PC as the index operand.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      inst_env->invalid = 1;
      return;
    }
  /* Check if we have a prefix.  */
  if (inst_env->prefix_found)
    {
      check_assign (inst, inst_env);
    }
  /* Check if this is an autoincrement mode.  */
  else if (cris_get_mode (inst) == AUTOINC_MODE)
    {
      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      process_autoincrement (cris_get_size (inst), inst, inst_env);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the 3-operand BOUND instruction.  */

void 
three_operand_bound_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's an error if we haven't got a prefix.  And it's also an error
     if the PC is the destination register.  */
  if ((!inst_env->prefix_found) || (cris_get_operand1 (inst) == REG_PC))
    {
      inst_env->invalid = 1;
      return;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Clears the status flags in inst_env.  */

void 
btst_nop_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's an error if we have got a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Clears the status flags in inst_env.  */

void 
clearf_di_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's an error if we have got a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles the CLEAR instruction if it's in register mode.  */

void 
reg_mode_clear_op (unsigned short inst, inst_env_type *inst_env)
{
  /* Check if the target is the PC.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      /* The instruction will clear the instruction's size bits.  */
      int clear_size = cris_get_clear_size (inst);
      if (clear_size == INST_BYTE_SIZE)
        {
          inst_env->delay_slot_pc = inst_env->reg[REG_PC] & 0xFFFFFF00;
        }
      if (clear_size == INST_WORD_SIZE)
        {
          inst_env->delay_slot_pc = inst_env->reg[REG_PC] & 0xFFFF0000;
        }
      if (clear_size == INST_DWORD_SIZE)
        {
          inst_env->delay_slot_pc = 0x0;
        }
      /* The jump will be delayed with one delay slot.  So we need a delay 
         slot.  */
      inst_env->slot_needed = 1;
      inst_env->delay_slot_pc_active = 1;
    }
  else
    {
      /* The PC will not change => no delay slot.  */
      inst_env->slot_needed = 0;
    }
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the TEST instruction if it's in register mode.  */

void
reg_mode_test_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's an error if we have got a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;

}

/* Handles the CLEAR and TEST instruction if the instruction isn't 
   in register mode.  */

void 
none_reg_mode_clear_test_op (unsigned short inst, inst_env_type *inst_env)
{
  /* Check if we are in a prefix mode.  */
  if (inst_env->prefix_found)
    {
      /* The only way the PC can change is if this instruction is in
         assign addressing mode.  */
      check_assign (inst, inst_env);
    }
  /* Indirect mode can't change the PC so just check if the mode is
     autoincrement.  */
  else if (cris_get_mode (inst) == AUTOINC_MODE)
    {
      process_autoincrement (cris_get_size (inst), inst, inst_env);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Checks that the PC isn't the destination register or the instructions has
   a prefix.  */

void 
dstep_logshift_mstep_neg_not_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to have the PC as the destination.  The instruction can't
     have a prefix.  */
  if ((cris_get_operand2 (inst) == REG_PC) || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Checks that the instruction doesn't have a prefix.  */

void
break_op (unsigned short inst, inst_env_type *inst_env)
{
  /* The instruction can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Checks that the PC isn't the destination register and that the instruction
   doesn't have a prefix.  */

void
scc_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to have the PC as the destination.  The instruction can't
     have a prefix.  */
  if ((cris_get_operand2 (inst) == REG_PC) || inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles the register mode JUMP instruction.  */

void 
reg_mode_jump_op (unsigned short inst, inst_env_type *inst_env)
{
  /* It's invalid to do a JUMP in a delay slot.  The mode is register, so 
     you can't have a prefix.  */
  if ((inst_env->slot_needed) || (inst_env->prefix_found))
    {
      inst_env->invalid = 1;
      return;
    }
  
  /* Just change the PC.  */
  inst_env->reg[REG_PC] = inst_env->reg[cris_get_operand1 (inst)];
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles the JUMP instruction for all modes except register.  */

void none_reg_mode_jump_op (unsigned short inst, inst_env_type *inst_env)
{
  unsigned long newpc;
  CORE_ADDR address;

  /* It's invalid to do a JUMP in a delay slot.  */
  if (inst_env->slot_needed)
    {
      inst_env->invalid = 1;
    }
  else
    {
      /* Check if we have a prefix.  */
      if (inst_env->prefix_found)
        {
          check_assign (inst, inst_env);

          /* Get the new value for the the PC.  */
          newpc = 
            read_memory_unsigned_integer ((CORE_ADDR) inst_env->prefix_value,
                                          4);
        }
      else
        {
          /* Get the new value for the PC.  */
          address = (CORE_ADDR) inst_env->reg[cris_get_operand1 (inst)];
          newpc = read_memory_unsigned_integer (address, 4);

          /* Check if we should increment a register.  */
          if (cris_get_mode (inst) == AUTOINC_MODE)
            {
              inst_env->reg[cris_get_operand1 (inst)] += 4;
            }
        }
      inst_env->reg[REG_PC] = newpc;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles moves to special registers (aka P-register) for all modes.  */

void 
move_to_preg_op (unsigned short inst, inst_env_type *inst_env)
{
  if (inst_env->prefix_found)
    {
      /* The instruction has a prefix that means we are only interested if
         the instruction is in assign mode.  */
      if (cris_get_mode (inst) == PREFIX_ASSIGN_MODE)
        {
          /* The prefix handles the problem if we are in a delay slot.  */
          if (cris_get_operand1 (inst) == REG_PC)
            {
              /* Just take care of the assign.  */
              check_assign (inst, inst_env);
            }
        }
    }
  else if (cris_get_mode (inst) == AUTOINC_MODE)
    {
      /* The instruction doesn't have a prefix, the only case left that we
         are interested in is the autoincrement mode.  */
      if (cris_get_operand1 (inst) == REG_PC)
        {
          /* If the PC is to be incremented it's invalid to be in a 
             delay slot.  */
          if (inst_env->slot_needed)
            {
              inst_env->invalid = 1;
              return;
            }
          /* The increment depends on the size of the special register. 
             Register P0 to P3 has the size byte, register P4 to P7 has the 
             size word and register P8 to P15 has the size dword.  */
          if (cris_get_operand2 (inst) < 4)
            {
              process_autoincrement (INST_BYTE_SIZE, inst, inst_env);
            }
          if (cris_get_operand2 (inst) < 8)
            {
              process_autoincrement (INST_WORD_SIZE, inst, inst_env);
            }
          else
            {
              process_autoincrement (INST_DWORD_SIZE, inst, inst_env);
            }
        }
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles moves from special registers (aka P-register) for all modes
   except register.  */

void 
none_reg_mode_move_from_preg_op (unsigned short inst, inst_env_type *inst_env)
{
  if (inst_env->prefix_found)
    {
      /* The instruction has a prefix that means we are only interested if
         the instruction is in assign mode.  */
      if (cris_get_mode (inst) == PREFIX_ASSIGN_MODE)
        {
          /* The prefix handles the problem if we are in a delay slot.  */
          if (cris_get_operand1 (inst) == REG_PC)
            {
              /* Just take care of the assign.  */
              check_assign (inst, inst_env);
            }
        }
    }    
  /* The instruction doesn't have a prefix, the only case left that we
     are interested in is the autoincrement mode.  */
  else if (cris_get_mode (inst) == AUTOINC_MODE)
    {
      if (cris_get_operand1 (inst) == REG_PC)
        {
          /* If the PC is to be incremented it's invalid to be in a 
             delay slot.  */
          if (inst_env->slot_needed)
            {
              inst_env->invalid = 1;
              return;
            }
          /* The increment depends on the size of the special register.  
             Register P0 to P3 has the size byte, register P4 to P7 has 
             the size word and register P8 to P15 has the size dword.  */
          if (cris_get_operand2 (inst) < 4)
            {
              process_autoincrement (INST_BYTE_SIZE, inst, inst_env);
            }
          if (cris_get_operand2 (inst) < 8)
            {
              process_autoincrement (INST_WORD_SIZE, inst, inst_env);
            }
          else
            {
              process_autoincrement (INST_DWORD_SIZE, inst, inst_env);
            }
        }
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles moves from special registers (aka P-register) when the mode
   is register.  */

void 
reg_mode_move_from_preg_op (unsigned short inst, inst_env_type *inst_env)
{
  /* Register mode move from special register can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  if (cris_get_operand1 (inst) == REG_PC)
    {
      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      /* The destination is the PC, the jump will have a delay slot.  */
      inst_env->delay_slot_pc = inst_env->preg[cris_get_operand2 (inst)];
      inst_env->slot_needed = 1;
      inst_env->delay_slot_pc_active = 1;
    }
  else
    {
      /* If the destination isn't PC, there will be no jump.  */
      inst_env->slot_needed = 0;
    }
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 1;
}

/* Handles the MOVEM from memory to general register instruction.  */

void 
move_mem_to_reg_movem_op (unsigned short inst, inst_env_type *inst_env)
{
  if (inst_env->prefix_found)
    {
      /* The prefix handles the problem if we are in a delay slot.  Is the
         MOVEM instruction going to change the PC?  */
      if (cris_get_operand2 (inst) >= REG_PC)
        {
          inst_env->reg[REG_PC] = 
            read_memory_unsigned_integer (inst_env->prefix_value, 4);
        }
      /* The assign value is the value after the increment.  Normally, the   
         assign value is the value before the increment.  */
      if ((cris_get_operand1 (inst) == REG_PC) 
          && (cris_get_mode (inst) == PREFIX_ASSIGN_MODE))
        {
          inst_env->reg[REG_PC] = inst_env->prefix_value;
          inst_env->reg[REG_PC] += 4 * (cris_get_operand2 (inst) + 1);
        }
    }
  else
    {
      /* Is the MOVEM instruction going to change the PC?  */
      if (cris_get_operand2 (inst) == REG_PC)
        {
          /* It's invalid to change the PC in a delay slot.  */
          if (inst_env->slot_needed)
            {
              inst_env->invalid = 1;
              return;
            }
          inst_env->reg[REG_PC] =
            read_memory_unsigned_integer (inst_env->reg[cris_get_operand1 (inst)], 
                                          4);
        }
      /* The increment is not depending on the size, instead it's depending
         on the number of registers loaded from memory.  */
      if ((cris_get_operand1 (inst) == REG_PC) && (cris_get_mode (inst) == AUTOINC_MODE))
        {
          /* It's invalid to change the PC in a delay slot.  */
          if (inst_env->slot_needed)
            {
              inst_env->invalid = 1;
              return;
            }
          inst_env->reg[REG_PC] += 4 * (cris_get_operand2 (inst) + 1); 
        }
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the MOVEM to memory from general register instruction.  */

void 
move_reg_to_mem_movem_op (unsigned short inst, inst_env_type *inst_env)
{
  if (inst_env->prefix_found)
    {
      /* The assign value is the value after the increment.  Normally, the
         assign value is the value before the increment.  */
      if ((cris_get_operand1 (inst) == REG_PC) &&
          (cris_get_mode (inst) == PREFIX_ASSIGN_MODE))
        {
          /* The prefix handles the problem if we are in a delay slot.  */
          inst_env->reg[REG_PC] = inst_env->prefix_value;
          inst_env->reg[REG_PC] += 4 * (cris_get_operand2 (inst) + 1);
        }
    }
  else
    {
      /* The increment is not depending on the size, instead it's depending
         on the number of registers loaded to memory.  */
      if ((cris_get_operand1 (inst) == REG_PC) && (cris_get_mode (inst) == AUTOINC_MODE))
        {
          /* It's invalid to change the PC in a delay slot.  */
          if (inst_env->slot_needed)
            {
              inst_env->invalid = 1;
              return;
            }
          inst_env->reg[REG_PC] += 4 * (cris_get_operand2 (inst) + 1);
        }
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the pop instruction to a general register. 
   POP is a assembler macro for MOVE.D [SP+], Rd.  */

void 
reg_pop_op (unsigned short inst, inst_env_type *inst_env)
{
  /* POP can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  if (cris_get_operand2 (inst) == REG_PC)
    {
      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      inst_env->reg[REG_PC] = 
        read_memory_unsigned_integer (inst_env->reg[REG_SP], 4);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles moves from register to memory.  */

void 
move_reg_to_mem_index_inc_op (unsigned short inst, inst_env_type *inst_env)
{
  /* Check if we have a prefix.  */
  if (inst_env->prefix_found)
    {
      /* The only thing that can change the PC is an assign.  */
      check_assign (inst, inst_env);
    }
  else if ((cris_get_operand1 (inst) == REG_PC) 
           && (cris_get_mode (inst) == AUTOINC_MODE))
    {
      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      process_autoincrement (cris_get_size (inst), inst, inst_env);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the intructions that's not yet implemented, by setting 
   inst_env->invalid to true.  */

void 
not_implemented_op (unsigned short inst, inst_env_type *inst_env)
{
  inst_env->invalid = 1;
}

/* Handles the XOR instruction.  */

void 
xor_op (unsigned short inst, inst_env_type *inst_env)
{
  /* XOR can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Check if the PC is the target.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      /* It's invalid to change the PC in a delay slot.  */
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      inst_env->reg[REG_PC] ^= inst_env->reg[cris_get_operand1 (inst)];
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the MULS instruction.  */

void 
muls_op (unsigned short inst, inst_env_type *inst_env)
{
  /* MULS/U can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Consider it invalid if the PC is the target.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      inst_env->invalid = 1;
      return;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the MULU instruction.  */

void 
mulu_op (unsigned short inst, inst_env_type *inst_env)
{
  /* MULS/U can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Consider it invalid if the PC is the target.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      inst_env->invalid = 1;
      return;
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Calculate the result of the instruction for ADD, SUB, CMP AND, OR and MOVE. 
   The MOVE instruction is the move from source to register.  */

void 
add_sub_cmp_and_or_move_action (unsigned short inst, inst_env_type *inst_env, 
                                unsigned long source1, unsigned long source2)
{
  unsigned long pc_mask;
  unsigned long operation_mask;
  
  /* Find out how many bits the operation should apply to.  */
  if (cris_get_size (inst) == INST_BYTE_SIZE)
    {
      pc_mask = 0xFFFFFF00; 
      operation_mask = 0xFF;
    }
  else if (cris_get_size (inst) == INST_WORD_SIZE)
    {
      pc_mask = 0xFFFF0000;
      operation_mask = 0xFFFF;
    }
  else if (cris_get_size (inst) == INST_DWORD_SIZE)
    {
      pc_mask = 0x0;
      operation_mask = 0xFFFFFFFF;
    }
  else
    {
      /* The size is out of range.  */
      inst_env->invalid = 1;
      return;
    }

  /* The instruction just works on uw_operation_mask bits.  */
  source2 &= operation_mask;
  source1 &= operation_mask;

  /* Now calculate the result.  The opcode's 3 first bits separates
     the different actions.  */
  switch (cris_get_opcode (inst) & 7)
    {
    case 0:  /* add */
      source1 += source2;
      break;

    case 1:  /* move */
      source1 = source2;
      break;

    case 2:  /* subtract */
      source1 -= source2;
      break;

    case 3:  /* compare */
      break;

    case 4:  /* and */
      source1 &= source2;
      break;

    case 5:  /* or */
      source1 |= source2;
      break;

    default:
      inst_env->invalid = 1;
      return;

      break;
    }

  /* Make sure that the result doesn't contain more than the instruction
     size bits.  */
  source2 &= operation_mask;

  /* Calculate the new breakpoint address.  */
  inst_env->reg[REG_PC] &= pc_mask;
  inst_env->reg[REG_PC] |= source1;

}

/* Extends the value from either byte or word size to a dword.  If the mode
   is zero extend then the value is extended with zero.  If instead the mode
   is signed extend the sign bit of the value is taken into consideration.  */

unsigned long 
do_sign_or_zero_extend (unsigned long value, unsigned short *inst)
{
  /* The size can be either byte or word, check which one it is. 
     Don't check the highest bit, it's indicating if it's a zero
     or sign extend.  */
  if (cris_get_size (*inst) & INST_WORD_SIZE)
    {
      /* Word size.  */
      value &= 0xFFFF;

      /* Check if the instruction is signed extend.  If so, check if value has
         the sign bit on.  */
      if (cris_is_signed_extend_bit_on (*inst) && (value & SIGNED_WORD_MASK))
        {
          value |= SIGNED_WORD_EXTEND_MASK;
        } 
    }
  else
    {
      /* Byte size.  */
      value &= 0xFF;

      /* Check if the instruction is signed extend.  If so, check if value has
         the sign bit on.  */
      if (cris_is_signed_extend_bit_on (*inst) && (value & SIGNED_BYTE_MASK))
        {
          value |= SIGNED_BYTE_EXTEND_MASK;
        }
    }
  /* The size should now be dword.  */
  cris_set_size_to_dword (inst);
  return value;
}

/* Handles the register mode for the ADD, SUB, CMP, AND, OR and MOVE
   instruction.  The MOVE instruction is the move from source to register.  */

void 
reg_mode_add_sub_cmp_and_or_move_op (unsigned short inst,
                                     inst_env_type *inst_env)
{
  unsigned long operand1;
  unsigned long operand2;

  /* It's invalid to have a prefix to the instruction.  This is a register 
     mode instruction and can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  /* Check if the instruction has PC as its target.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      /* The instruction has the PC as its target register.  */
      operand1 = inst_env->reg[cris_get_operand1 (inst)]; 
      operand2 = inst_env->reg[REG_PC];

      /* Check if it's a extend, signed or zero instruction.  */
      if (cris_get_opcode (inst) < 4)
        {
          operand1 = do_sign_or_zero_extend (operand1, &inst);
        }
      /* Calculate the PC value after the instruction, i.e. where the
         breakpoint should be.  The order of the udw_operands is vital.  */
      add_sub_cmp_and_or_move_action (inst, inst_env, operand2, operand1); 
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Returns the data contained at address.  The size of the data is derived from
   the size of the operation.  If the instruction is a zero or signed
   extend instruction, the size field is changed in instruction.  */

unsigned long 
get_data_from_address (unsigned short *inst, CORE_ADDR address)
{
  int size = cris_get_size (*inst);
  unsigned long value;

  /* If it's an extend instruction we don't want the signed extend bit,
     because it influences the size.  */
  if (cris_get_opcode (*inst) < 4)
    {
      size &= ~SIGNED_EXTEND_BIT_MASK;
    }
  /* Is there a need for checking the size?  Size should contain the number of
     bytes to read.  */
  size = 1 << size;
  value = read_memory_unsigned_integer (address, size);

  /* Check if it's an extend, signed or zero instruction.  */
  if (cris_get_opcode (*inst) < 4)
    {
      value = do_sign_or_zero_extend (value, inst);
    }
  return value;
}

/* Handles the assign addresing mode for the ADD, SUB, CMP, AND, OR and MOVE 
   instructions.  The MOVE instruction is the move from source to register.  */

void 
handle_prefix_assign_mode_for_aritm_op (unsigned short inst, 
                                        inst_env_type *inst_env)
{
  unsigned long operand2;
  unsigned long operand3;

  check_assign (inst, inst_env);
  if (cris_get_operand2 (inst) == REG_PC)
    {
      operand2 = inst_env->reg[REG_PC];

      /* Get the value of the third operand.  */
      operand3 = get_data_from_address (&inst, inst_env->prefix_value);

      /* Calculate the PC value after the instruction, i.e. where the
         breakpoint should be.  The order of the udw_operands is vital.  */
      add_sub_cmp_and_or_move_action (inst, inst_env, operand2, operand3);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the three-operand addressing mode for the ADD, SUB, CMP, AND and
   OR instructions.  Note that for this to work as expected, the calling
   function must have made sure that there is a prefix to this instruction.  */

void 
three_operand_add_sub_cmp_and_or_op (unsigned short inst, 
                                     inst_env_type *inst_env)
{
  unsigned long operand2;
  unsigned long operand3;

  if (cris_get_operand1 (inst) == REG_PC)
    {
      /* The PC will be changed by the instruction.  */
      operand2 = inst_env->reg[cris_get_operand2 (inst)];

      /* Get the value of the third operand.  */
      operand3 = get_data_from_address (&inst, inst_env->prefix_value);

      /* Calculate the PC value after the instruction, i.e. where the
         breakpoint should be.  */
      add_sub_cmp_and_or_move_action (inst, inst_env, operand2, operand3);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the index addresing mode for the ADD, SUB, CMP, AND, OR and MOVE
   instructions.  The MOVE instruction is the move from source to register.  */

void 
handle_prefix_index_mode_for_aritm_op (unsigned short inst, 
                                       inst_env_type *inst_env)
{
  if (cris_get_operand1 (inst) != cris_get_operand2 (inst))
    {
      /* If the instruction is MOVE it's invalid.  If the instruction is ADD,
         SUB, AND or OR something weird is going on (if everything works these
         instructions should end up in the three operand version).  */
      inst_env->invalid = 1;
      return;
    }
  else
    {
      /* three_operand_add_sub_cmp_and_or does the same as we should do here
         so use it.  */
      three_operand_add_sub_cmp_and_or_op (inst, inst_env);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the autoincrement and indirect addresing mode for the ADD, SUB,
   CMP, AND OR and MOVE instruction.  The MOVE instruction is the move from
   source to register.  */

void 
handle_inc_and_index_mode_for_aritm_op (unsigned short inst, 
                                        inst_env_type *inst_env)
{
  unsigned long operand1;
  unsigned long operand2;
  unsigned long operand3;
  int size;

  /* The instruction is either an indirect or autoincrement addressing mode. 
     Check if the destination register is the PC.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      /* Must be done here, get_data_from_address may change the size 
         field.  */
      size = cris_get_size (inst);
      operand2 = inst_env->reg[REG_PC];

      /* Get the value of the third operand, i.e. the indirect operand.  */
      operand1 = inst_env->reg[cris_get_operand1 (inst)];
      operand3 = get_data_from_address (&inst, operand1);

      /* Calculate the PC value after the instruction, i.e. where the
         breakpoint should be.  The order of the udw_operands is vital.  */
      add_sub_cmp_and_or_move_action (inst, inst_env, operand2, operand3); 
    }
  /* If this is an autoincrement addressing mode, check if the increment
     changes the PC.  */
  if ((cris_get_operand1 (inst) == REG_PC) && (cris_get_mode (inst) == AUTOINC_MODE))
    {
      /* Get the size field.  */
      size = cris_get_size (inst);

      /* If it's an extend instruction we don't want the signed extend bit,
         because it influences the size.  */
      if (cris_get_opcode (inst) < 4)
        {
          size &= ~SIGNED_EXTEND_BIT_MASK;
        }
      process_autoincrement (size, inst, inst_env);
    } 
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the two-operand addressing mode, all modes except register, for
   the ADD, SUB CMP, AND and OR instruction.  */

void 
none_reg_mode_add_sub_cmp_and_or_move_op (unsigned short inst, 
                                          inst_env_type *inst_env)
{
  if (inst_env->prefix_found)
    {
      if (cris_get_mode (inst) == PREFIX_INDEX_MODE)
        {
          handle_prefix_index_mode_for_aritm_op (inst, inst_env);
        }
      else if (cris_get_mode (inst) == PREFIX_ASSIGN_MODE)
        {
          handle_prefix_assign_mode_for_aritm_op (inst, inst_env);
        }
      else
        {
          /* The mode is invalid for a prefixed base instruction.  */
          inst_env->invalid = 1;
          return;
        }
    }
  else
    {
      handle_inc_and_index_mode_for_aritm_op (inst, inst_env);
    }
}

/* Handles the quick addressing mode for the ADD and SUB instruction.  */

void 
quick_mode_add_sub_op (unsigned short inst, inst_env_type *inst_env)
{
  unsigned long operand1;
  unsigned long operand2;

  /* It's a bad idea to be in a prefix instruction now.  This is a quick mode
     instruction and can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }

  /* Check if the instruction has PC as its target.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      operand1 = cris_get_quick_value (inst);
      operand2 = inst_env->reg[REG_PC];

      /* The size should now be dword.  */
      cris_set_size_to_dword (&inst);

      /* Calculate the PC value after the instruction, i.e. where the
         breakpoint should be.  */
      add_sub_cmp_and_or_move_action (inst, inst_env, operand2, operand1);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Handles the quick addressing mode for the CMP, AND and OR instruction.  */

void 
quick_mode_and_cmp_move_or_op (unsigned short inst, inst_env_type *inst_env)
{
  unsigned long operand1;
  unsigned long operand2;

  /* It's a bad idea to be in a prefix instruction now.  This is a quick mode
     instruction and can't have a prefix.  */
  if (inst_env->prefix_found)
    {
      inst_env->invalid = 1;
      return;
    }
  /* Check if the instruction has PC as its target.  */
  if (cris_get_operand2 (inst) == REG_PC)
    {
      if (inst_env->slot_needed)
        {
          inst_env->invalid = 1;
          return;
        }
      /* The instruction has the PC as its target register.  */
      operand1 = cris_get_quick_value (inst);
      operand2 = inst_env->reg[REG_PC];

      /* The quick value is signed, so check if we must do a signed extend.  */
      if (operand1 & SIGNED_QUICK_VALUE_MASK)
        {
          /* sign extend  */
          operand1 |= SIGNED_QUICK_VALUE_EXTEND_MASK;
        }
      /* The size should now be dword.  */
      cris_set_size_to_dword (&inst);

      /* Calculate the PC value after the instruction, i.e. where the
         breakpoint should be.  */
      add_sub_cmp_and_or_move_action (inst, inst_env, operand2, operand1);
    }
  inst_env->slot_needed = 0;
  inst_env->prefix_found = 0;
  inst_env->xflag_found = 0;
  inst_env->disable_interrupt = 0;
}

/* Translate op_type to a function and call it.  */

static void cris_gdb_func (enum cris_op_type op_type, unsigned short inst, 
                           inst_env_type *inst_env)
{
  switch (op_type)
    {
    case cris_not_implemented_op:
      not_implemented_op (inst, inst_env);
      break;

    case cris_abs_op:
      abs_op (inst, inst_env);
      break;

    case cris_addi_op:
      addi_op (inst, inst_env);
      break;

    case cris_asr_op:
      asr_op (inst, inst_env);
      break;

    case cris_asrq_op:
      asrq_op (inst, inst_env);
      break;

    case cris_ax_ei_setf_op:
      ax_ei_setf_op (inst, inst_env);
      break;

    case cris_bdap_prefix:
      bdap_prefix (inst, inst_env);
      break;

    case cris_biap_prefix:
      biap_prefix (inst, inst_env);
      break;

    case cris_break_op:
      break_op (inst, inst_env);
      break;

    case cris_btst_nop_op:
      btst_nop_op (inst, inst_env);
      break;

    case cris_clearf_di_op:
      clearf_di_op (inst, inst_env);
      break;

    case cris_dip_prefix:
      dip_prefix (inst, inst_env);
      break;

    case cris_dstep_logshift_mstep_neg_not_op:
      dstep_logshift_mstep_neg_not_op (inst, inst_env);
      break;

    case cris_eight_bit_offset_branch_op:
      eight_bit_offset_branch_op (inst, inst_env);
      break;

    case cris_move_mem_to_reg_movem_op:
      move_mem_to_reg_movem_op (inst, inst_env);
      break;

    case cris_move_reg_to_mem_movem_op:
      move_reg_to_mem_movem_op (inst, inst_env);
      break;

    case cris_move_to_preg_op:
      move_to_preg_op (inst, inst_env);
      break;

    case cris_muls_op:
      muls_op (inst, inst_env);
      break;

    case cris_mulu_op:
      mulu_op (inst, inst_env);
      break;

    case cris_none_reg_mode_add_sub_cmp_and_or_move_op:
      none_reg_mode_add_sub_cmp_and_or_move_op (inst, inst_env);
      break;

    case cris_none_reg_mode_clear_test_op:
      none_reg_mode_clear_test_op (inst, inst_env);
      break;

    case cris_none_reg_mode_jump_op:
      none_reg_mode_jump_op (inst, inst_env);
      break;

    case cris_none_reg_mode_move_from_preg_op:
      none_reg_mode_move_from_preg_op (inst, inst_env);
      break;

    case cris_quick_mode_add_sub_op:
      quick_mode_add_sub_op (inst, inst_env);
      break;

    case cris_quick_mode_and_cmp_move_or_op:
      quick_mode_and_cmp_move_or_op (inst, inst_env);
      break;

    case cris_quick_mode_bdap_prefix:
      quick_mode_bdap_prefix (inst, inst_env);
      break;

    case cris_reg_mode_add_sub_cmp_and_or_move_op:
      reg_mode_add_sub_cmp_and_or_move_op (inst, inst_env);
      break;

    case cris_reg_mode_clear_op:
      reg_mode_clear_op (inst, inst_env);
      break;

    case cris_reg_mode_jump_op:
      reg_mode_jump_op (inst, inst_env);
      break;

    case cris_reg_mode_move_from_preg_op:
      reg_mode_move_from_preg_op (inst, inst_env);
      break;

    case cris_reg_mode_test_op:
      reg_mode_test_op (inst, inst_env);
      break;

    case cris_scc_op:
      scc_op (inst, inst_env);
      break;

    case cris_sixteen_bit_offset_branch_op:
      sixteen_bit_offset_branch_op (inst, inst_env);
      break;

    case cris_three_operand_add_sub_cmp_and_or_op:
      three_operand_add_sub_cmp_and_or_op (inst, inst_env);
      break;

    case cris_three_operand_bound_op:
      three_operand_bound_op (inst, inst_env);
      break;

    case cris_two_operand_bound_op:
      two_operand_bound_op (inst, inst_env);
      break;

    case cris_xor_op:
      xor_op (inst, inst_env);
      break;
    }
}

/* This wrapper is to avoid cris_get_assembler being called before 
   exec_bfd has been set.  */

static int
cris_delayed_get_disassembler (bfd_vma addr, disassemble_info *info)
{
  tm_print_insn = cris_get_disassembler (exec_bfd);
  return (*tm_print_insn) (addr, info);
}

void
_initialize_cris_tdep (void)
{
  struct cmd_list_element *c;

  gdbarch_register (bfd_arch_cris, cris_gdbarch_init, cris_dump_tdep);
  
  /* Used in disassembly.  */
  tm_print_insn = cris_delayed_get_disassembler;

  /* CRIS-specific user-commands.  */
  c = add_set_cmd ("cris-version", class_support, var_integer, 
                   (char *) &usr_cmd_cris_version, 
                   "Set the current CRIS version.", &setlist);
  c->function.sfunc = cris_version_update;
  add_show_from_set (c, &showlist);
  
  c = add_set_enum_cmd ("cris-mode", class_support, cris_mode_enums, 
                        &usr_cmd_cris_mode, 
                        "Set the current CRIS mode.", &setlist);
  c->function.sfunc = cris_mode_update;
  add_show_from_set (c, &showlist);

  c = add_set_enum_cmd ("cris-abi", class_support, cris_abi_enums, 
                        &usr_cmd_cris_abi, 
                        "Set the current CRIS ABI version.", &setlist);
  c->function.sfunc = cris_abi_update;
  add_show_from_set (c, &showlist);
}

/* Prints out all target specific values.  */

static void
cris_dump_tdep (struct gdbarch *gdbarch, struct ui_file *file)
{
  struct gdbarch_tdep *tdep = gdbarch_tdep (current_gdbarch);
  if (tdep != NULL)
    {
      fprintf_unfiltered (file, "cris_dump_tdep: tdep->cris_version = %i\n",
                          tdep->cris_version);
      fprintf_unfiltered (file, "cris_dump_tdep: tdep->cris_mode = %s\n",
                          tdep->cris_mode);
      fprintf_unfiltered (file, "cris_dump_tdep: tdep->cris_abi = %s\n",
                          tdep->cris_abi);

    }
}

static void
cris_version_update (char *ignore_args, int from_tty, 
                     struct cmd_list_element *c)
{
  struct gdbarch_info info;

  /* From here on, trust the user's CRIS version setting.  */
  if (c->type == set_cmd)
    {
      usr_cmd_cris_version_valid = 1;
  
      /* Update the current architecture, if needed.  */
      memset (&info, 0, sizeof info);
      if (!gdbarch_update_p (info))
        internal_error (__FILE__, __LINE__, "cris_gdbarch_update: failed to update architecture.");
    }  
}

static void
cris_mode_update (char *ignore_args, int from_tty, 
                 struct cmd_list_element *c)
{
  struct gdbarch_info info;
  
  /* From here on, trust the user's CRIS mode setting.  */
  if (c->type == set_cmd)
    {
      usr_cmd_cris_mode_valid = 1;
  
      /* Update the current architecture, if needed.  */
      memset (&info, 0, sizeof info);
      if (!gdbarch_update_p (info))
        internal_error (__FILE__, __LINE__, "cris_gdbarch_update: failed to update architecture.");
    }
}

static void
cris_abi_update (char *ignore_args, int from_tty, 
                 struct cmd_list_element *c)
{
  struct gdbarch_info info;
  
  /* From here on, trust the user's CRIS ABI setting.  */
  if (c->type == set_cmd)
    {
      usr_cmd_cris_abi_valid = 1;
  
      /* Update the current architecture, if needed.  */
      memset (&info, 0, sizeof info);
      if (!gdbarch_update_p (info))
        internal_error (__FILE__, __LINE__, "cris_gdbarch_update: failed to update architecture.");
    }
}

/* Copied from pa64solib.c, with a couple of minor changes.  */

static CORE_ADDR
bfd_lookup_symbol (bfd *abfd, const char *symname)
{
  unsigned int storage_needed;
  asymbol *sym;
  asymbol **symbol_table;
  unsigned int number_of_symbols;
  unsigned int i;
  struct cleanup *back_to;
  CORE_ADDR symaddr = 0;

  storage_needed = bfd_get_symtab_upper_bound (abfd);

  if (storage_needed > 0)
    {
      symbol_table = (asymbol **) xmalloc (storage_needed);
      back_to = make_cleanup (free, (PTR) symbol_table);
      number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);

      for (i = 0; i < number_of_symbols; i++)
	{
	  sym = *symbol_table++;
	  if (!strcmp (sym->name, symname))
	    {
	      /* Bfd symbols are section relative.  */
	      symaddr = sym->value + sym->section->vma;
	      break;
	    }
	}
      do_cleanups (back_to);
    }
  return (symaddr);
}

static struct gdbarch *
cris_gdbarch_init (struct gdbarch_info info, struct gdbarch_list *arches)
{
  struct gdbarch *gdbarch;
  struct gdbarch_tdep *tdep;
  int cris_version;
  const char *cris_mode;
  const char *cris_abi;
  CORE_ADDR cris_abi_sym = 0;
  int register_bytes;

  if (usr_cmd_cris_version_valid)
    {
      /* Trust the user's CRIS version setting.  */ 
      cris_version = usr_cmd_cris_version;
    }
  else
    {
      /* Assume it's CRIS version 10.  */
      cris_version = 10;
    }

  if (usr_cmd_cris_mode_valid)
    {
      /* Trust the user's CRIS mode setting.  */ 
      cris_mode = usr_cmd_cris_mode;
    }
  else if (cris_version == 10)
    {
      /* Assume CRIS version 10 is in user mode.  */
      cris_mode = CRIS_MODE_USER;
    }
  else
    {
      /* Strictly speaking, older CRIS version don't have a supervisor mode,
         but we regard its only mode as supervisor mode.  */
      cris_mode = CRIS_MODE_SUPERVISOR;
    }

  if (usr_cmd_cris_abi_valid)
    {
      /* Trust the user's ABI setting.  */
      cris_abi = usr_cmd_cris_abi;
    }
  else if (info.abfd)
    {
      if (bfd_get_flavour (info.abfd) == bfd_target_elf_flavour)
        {
          /* An elf target uses the new ABI.  */
          cris_abi = CRIS_ABI_V2;
        }
      else if (bfd_get_flavour (info.abfd) == bfd_target_aout_flavour)
        {
          /* An a.out target may use either ABI.  Look for hints in the
             symbol table.  */
          cris_abi_sym = bfd_lookup_symbol (info.abfd, CRIS_ABI_SYMBOL);
          cris_abi = cris_abi_sym ? CRIS_ABI_V2 : CRIS_ABI_ORIGINAL;
        }
      else
        {
          /* Unknown bfd flavour.  Assume it's the new ABI.  */
          cris_abi = CRIS_ABI_V2;
        }
    }
  else if (gdbarch_tdep (current_gdbarch))
    {
      /* No bfd available.  Stick with whatever ABI we're currently using.  
         (This is to avoid changing the ABI when the user updates the 
         architecture with the 'set cris-version' command.)  */
      cris_abi = gdbarch_tdep (current_gdbarch)->cris_abi;
    }
  else
    {
      /* No bfd, and no current architecture available.  Assume it's the 
         new ABI.  */
      cris_abi = CRIS_ABI_V2;
    }

  /* Make the current settings visible to the user.  */
  usr_cmd_cris_version = cris_version;
  usr_cmd_cris_mode = cris_mode;
  usr_cmd_cris_abi = cris_abi;
  
  /* Find a candidate among the list of pre-declared architectures.  Both
     CRIS version and ABI must match.  */
  for (arches = gdbarch_list_lookup_by_info (arches, &info); 
       arches != NULL;
       arches = gdbarch_list_lookup_by_info (arches->next, &info))
    {
      if ((gdbarch_tdep (arches->gdbarch)->cris_version == cris_version)
          && (gdbarch_tdep (arches->gdbarch)->cris_mode == cris_mode)
          && (gdbarch_tdep (arches->gdbarch)->cris_abi == cris_abi))
        return arches->gdbarch;
    }

  /* No matching architecture was found.  Create a new one.  */
  tdep = (struct gdbarch_tdep *) xmalloc (sizeof (struct gdbarch_tdep));
  gdbarch = gdbarch_alloc (&info, tdep);

  tdep->cris_version = cris_version;
  tdep->cris_mode = cris_mode;
  tdep->cris_abi = cris_abi;

  /* INIT shall ensure that the INFO.BYTE_ORDER is non-zero.  */
  switch (info.byte_order)
    {
    case LITTLE_ENDIAN:
      /* Ok.  */
      break;

    case BIG_ENDIAN:
      internal_error (__FILE__, __LINE__, "cris_gdbarch_init: big endian byte order in info");
      break;
    
    default:
      internal_error (__FILE__, __LINE__, "cris_gdbarch_init: unknown byte order in info");
    }

  /* Initialize the ABI dependent things.  */
  if (tdep->cris_abi == CRIS_ABI_ORIGINAL)
    {
      set_gdbarch_double_bit (gdbarch, 32);
      set_gdbarch_push_arguments (gdbarch, cris_abi_original_push_arguments);
      set_gdbarch_store_return_value (gdbarch, 
                                      cris_abi_original_store_return_value);
      set_gdbarch_extract_return_value 
        (gdbarch, cris_abi_original_extract_return_value);
      set_gdbarch_reg_struct_has_addr 
        (gdbarch, cris_abi_original_reg_struct_has_addr);
    }
  else if (tdep->cris_abi == CRIS_ABI_V2)
    {
      set_gdbarch_double_bit (gdbarch, 64);
      set_gdbarch_push_arguments (gdbarch, cris_abi_v2_push_arguments);
      set_gdbarch_store_return_value (gdbarch, cris_abi_v2_store_return_value);
      set_gdbarch_extract_return_value (gdbarch, 
                                        cris_abi_v2_extract_return_value);
      set_gdbarch_reg_struct_has_addr (gdbarch, 
                                       cris_abi_v2_reg_struct_has_addr);
    }
  else
    internal_error (__FILE__, __LINE__, "cris_gdbarch_init: unknown CRIS ABI");

  /* The default definition of a long double is 2 * TARGET_DOUBLE_BIT,
     which means we have to set this explicitly.  */
  set_gdbarch_long_double_bit (gdbarch, 64);
    
  /* Floating point is IEEE compatible.  */
  set_gdbarch_ieee_float (gdbarch, 1);

  /* There are 32 registers (some of which may not be implemented).  */
  set_gdbarch_num_regs (gdbarch, 32);
  set_gdbarch_sp_regnum (gdbarch, 14);
  set_gdbarch_fp_regnum (gdbarch, 8);
  set_gdbarch_pc_regnum (gdbarch, 15);

  set_gdbarch_register_name (gdbarch, cris_register_name);
  
  /* Length of ordinary registers used in push_word and a few other places. 
     REGISTER_RAW_SIZE is the real way to know how big a register is.  */
  set_gdbarch_register_size (gdbarch, 4);
  
  /* NEW */
  set_gdbarch_register_bytes_ok (gdbarch, cris_register_bytes_ok);
  set_gdbarch_software_single_step (gdbarch, cris_software_single_step);

  
  set_gdbarch_cannot_store_register (gdbarch, cris_cannot_store_register);
  set_gdbarch_cannot_fetch_register (gdbarch, cris_cannot_fetch_register);


  /* The total amount of space needed to store (in an array called registers)
     GDB's copy of the machine's register state.  Note: We can not use
     cris_register_size at this point, since it relies on current_gdbarch
     being set.  */
  switch (tdep->cris_version)
    {
    case 0:
    case 1:
    case 2:
    case 3:
      /* Support for these may be added later.  */
      internal_error (__FILE__, __LINE__, "cris_gdbarch_init: unsupported CRIS version");
      break;
      
    case 8:
    case 9:
      /* CRIS v8 and v9, a.k.a. ETRAX 100.  General registers R0 - R15 
         (32 bits), special registers P0 - P1 (8 bits), P4 - P5 (16 bits), 
         and P8 - P14 (32 bits).  */
      register_bytes = (16 * 4) + (2 * 1) + (2 * 2) + (7 * 4);
      break;

    case 10:
    case 11: 
      /* CRIS v10 and v11, a.k.a. ETRAX 100LX.  In addition to ETRAX 100, 
         P7 (32 bits), and P15 (32 bits) have been implemented.  */
      register_bytes = (16 * 4) + (2 * 1) + (2 * 2) + (9 * 4);
      break;

    default:
      internal_error (__FILE__, __LINE__, "cris_gdbarch_init: unknown CRIS version");
    }

  set_gdbarch_register_bytes (gdbarch, register_bytes);

  /* Returns the register offset for the first byte of register regno's space 
     in the saved register state.  */
  set_gdbarch_register_byte (gdbarch, cris_register_offset);
  
  /* The length of the registers in the actual machine representation.  */
  set_gdbarch_register_raw_size (gdbarch, cris_register_size);
  
  /* The largest value REGISTER_RAW_SIZE can have.  */
  set_gdbarch_max_register_raw_size (gdbarch, 32);
  
  /* The length of the registers in the program's representation.  */
  set_gdbarch_register_virtual_size (gdbarch, cris_register_size);
  
  /* The largest value REGISTER_VIRTUAL_SIZE can have.  */
  set_gdbarch_max_register_virtual_size (gdbarch, 32);

  set_gdbarch_register_virtual_type (gdbarch, cris_register_virtual_type);
  
  /* Use generic dummy frames.  */
  set_gdbarch_use_generic_dummy_frames (gdbarch, 1);
  
  /* Where to execute the call in the memory segments.  */
  set_gdbarch_call_dummy_location (gdbarch, AT_ENTRY_POINT);
  set_gdbarch_call_dummy_address (gdbarch, entry_point_address);
  
  /* Start execution at the beginning of dummy.  */
  set_gdbarch_call_dummy_start_offset (gdbarch, 0);
  set_gdbarch_call_dummy_breakpoint_offset (gdbarch, 0);
  
  /* Set to 1 since call_dummy_breakpoint_offset was defined.  */
  set_gdbarch_call_dummy_breakpoint_offset_p (gdbarch, 1);
  
  /* Read all about dummy frames in blockframe.c.  */
  set_gdbarch_call_dummy_length (gdbarch, 0);
  set_gdbarch_pc_in_call_dummy (gdbarch, pc_in_call_dummy_at_entry_point);
  
  /* Defined to 1 to indicate that the target supports inferior function 
     calls.  */
  set_gdbarch_call_dummy_p (gdbarch, 1);
  set_gdbarch_call_dummy_words (gdbarch, 0);
  set_gdbarch_sizeof_call_dummy_words (gdbarch, 0);
  
  /* No stack adjustment needed when peforming an inferior function call.  */
  set_gdbarch_call_dummy_stack_adjust_p (gdbarch, 0);
  set_gdbarch_fix_call_dummy (gdbarch, generic_fix_call_dummy);

  set_gdbarch_get_saved_register (gdbarch, generic_get_saved_register);
  
  /* No register requires conversion from raw format to virtual format.  */
  set_gdbarch_register_convertible (gdbarch, generic_register_convertible_not);

  set_gdbarch_push_dummy_frame (gdbarch, generic_push_dummy_frame);
  set_gdbarch_push_return_address (gdbarch, cris_push_return_address);
  set_gdbarch_pop_frame (gdbarch, cris_pop_frame);

  set_gdbarch_store_struct_return (gdbarch, cris_store_struct_return);
  set_gdbarch_extract_struct_value_address (gdbarch, 
                                            cris_extract_struct_value_address);
  set_gdbarch_use_struct_convention (gdbarch, cris_use_struct_convention);

  set_gdbarch_frame_init_saved_regs (gdbarch, cris_frame_init_saved_regs);
  set_gdbarch_init_extra_frame_info (gdbarch, cris_init_extra_frame_info);
  set_gdbarch_skip_prologue (gdbarch, cris_skip_prologue);
  set_gdbarch_prologue_frameless_p (gdbarch, generic_prologue_frameless_p);
  
  /* The stack grows downward.  */
  set_gdbarch_inner_than (gdbarch, core_addr_lessthan);

  set_gdbarch_breakpoint_from_pc (gdbarch, cris_breakpoint_from_pc);
  
  /* The PC must not be decremented after a breakpoint.  (The breakpoint
     handler takes care of that.)  */
  set_gdbarch_decr_pc_after_break (gdbarch, 0);
  
  /* Offset from address of function to start of its code.  */
  set_gdbarch_function_start_offset (gdbarch, 0);  
  
  /* The number of bytes at the start of arglist that are not really args,
     0 in the CRIS ABI.  */
  set_gdbarch_frame_args_skip (gdbarch, 0);
  set_gdbarch_frameless_function_invocation 
    (gdbarch, cris_frameless_function_invocation);
  set_gdbarch_frame_chain (gdbarch, cris_frame_chain);
  set_gdbarch_frame_chain_valid (gdbarch, generic_file_frame_chain_valid);

  set_gdbarch_frame_saved_pc (gdbarch, cris_frame_saved_pc);
  set_gdbarch_frame_args_address (gdbarch, cris_frame_args_address);
  set_gdbarch_frame_locals_address (gdbarch, cris_frame_locals_address);
  set_gdbarch_saved_pc_after_call (gdbarch, cris_saved_pc_after_call);

  set_gdbarch_frame_num_args (gdbarch, frame_num_args_unknown);
  
  /* No extra stack alignment needed.  Set to 1 by default.  */
  set_gdbarch_extra_stack_alignment_needed (gdbarch, 0);
  
  /* Helpful for backtracing and returning in a call dummy.  */
  set_gdbarch_save_dummy_frame_tos (gdbarch, generic_save_dummy_frame_tos);

  return gdbarch;
}
