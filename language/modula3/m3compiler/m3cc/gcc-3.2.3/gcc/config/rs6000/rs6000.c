/* Subroutines used for code generation on IBM RS/6000.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 
   2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "output.h"
#include "basic-block.h"
#include "integrate.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "reload.h"

#ifndef TARGET_NO_PROTOTYPE
#define TARGET_NO_PROTOTYPE 0
#endif

#define min(A,B)	((A) < (B) ? (A) : (B))
#define max(A,B)	((A) > (B) ? (A) : (B))

/* Target cpu type */

enum processor_type rs6000_cpu;
struct rs6000_cpu_select rs6000_select[3] =
{
  /* switch		name,			tune	arch */
  { (const char *)0,	"--with-cpu=",		1,	1 },
  { (const char *)0,	"-mcpu=",		1,	1 },
  { (const char *)0,	"-mtune=",		1,	0 },
};

/* Size of long double */
const char *rs6000_long_double_size_string;
int rs6000_long_double_type_size;

/* Whether -mabi=altivec has appeared */
int rs6000_altivec_abi;

/* Set to non-zero once AIX common-mode calls have been defined.  */
static int common_mode_defined;

/* Private copy of original value of flag_pic for ABI_AIX.  */
static int rs6000_flag_pic;

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */
rtx rs6000_compare_op0, rs6000_compare_op1;
int rs6000_compare_fp_p;

/* Label number of label created for -mrelocatable, to call to so we can
   get the address of the GOT section */
int rs6000_pic_labelno;

#ifdef USING_ELFOS_H
/* Which abi to adhere to */
const char *rs6000_abi_name = RS6000_ABI_NAME;

/* Semantics of the small data area */
enum rs6000_sdata_type rs6000_sdata = SDATA_DATA;

/* Which small data model to use */
const char *rs6000_sdata_name = (char *)0;

/* Counter for labels which are to be placed in .fixup.  */
int fixuplabelno = 0;
#endif

/* ABI enumeration available for subtarget to use.  */
enum rs6000_abi rs6000_current_abi;

/* ABI string from -mabi= option.  */
const char *rs6000_abi_string;

/* Debug flags */
const char *rs6000_debug_name;
int rs6000_debug_stack;		/* debug stack applications */
int rs6000_debug_arg;		/* debug argument handling */

/* Flag to say the TOC is initialized */
int toc_initialized;
char toc_label_name[10];

/* Alias set for saves and restores from the rs6000 stack.  */
static int rs6000_sr_alias_set;

static void rs6000_add_gc_roots PARAMS ((void));
static int num_insns_constant_wide PARAMS ((HOST_WIDE_INT));
static void validate_condition_mode 
  PARAMS ((enum rtx_code, enum machine_mode));
static rtx rs6000_generate_compare PARAMS ((enum rtx_code));
static void rs6000_maybe_dead PARAMS ((rtx));
static void rs6000_emit_stack_tie PARAMS ((void));
static void rs6000_frame_related PARAMS ((rtx, rtx, HOST_WIDE_INT, rtx, rtx));
static void rs6000_emit_allocate_stack PARAMS ((HOST_WIDE_INT, int));
static unsigned rs6000_hash_constant PARAMS ((rtx));
static unsigned toc_hash_function PARAMS ((const void *));
static int toc_hash_eq PARAMS ((const void *, const void *));
static int toc_hash_mark_entry PARAMS ((void **, void *));
static void toc_hash_mark_table PARAMS ((void *));
static int constant_pool_expr_1 PARAMS ((rtx, int *, int *));
static void rs6000_free_machine_status PARAMS ((struct function *));
static void rs6000_init_machine_status PARAMS ((struct function *));
static bool rs6000_assemble_integer PARAMS ((rtx, unsigned int, int));
static int rs6000_ra_ever_killed PARAMS ((void));
static tree rs6000_handle_longcall_attribute PARAMS ((tree *, tree, tree, int, bool *));
const struct attribute_spec rs6000_attribute_table[];
static void rs6000_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void rs6000_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static rtx rs6000_emit_set_long_const PARAMS ((rtx,
  HOST_WIDE_INT, HOST_WIDE_INT));
#if TARGET_ELF
static unsigned int rs6000_elf_section_type_flags PARAMS ((tree, const char *,
							   int));
static void rs6000_elf_asm_out_constructor PARAMS ((rtx, int));
static void rs6000_elf_asm_out_destructor PARAMS ((rtx, int));
#endif
#ifdef OBJECT_FORMAT_COFF
static void xcoff_asm_named_section PARAMS ((const char *, unsigned int));
#endif
static bool rs6000_binds_local_p PARAMS ((tree));
static int rs6000_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int rs6000_adjust_priority PARAMS ((rtx, int));
static int rs6000_issue_rate PARAMS ((void));

static void rs6000_init_builtins PARAMS ((void));
static void altivec_init_builtins PARAMS ((void));
static rtx rs6000_expand_builtin PARAMS ((tree, rtx, rtx, enum machine_mode, int));
static rtx altivec_expand_builtin PARAMS ((tree, rtx));
static rtx altivec_expand_unop_builtin PARAMS ((enum insn_code, tree, rtx));
static rtx altivec_expand_binop_builtin PARAMS ((enum insn_code, tree, rtx));
static rtx altivec_expand_abs_builtin PARAMS ((enum insn_code, tree, rtx));
static rtx altivec_expand_predicate_builtin PARAMS ((enum insn_code, const char *, tree, rtx));
static rtx altivec_expand_ternop_builtin PARAMS ((enum insn_code, tree, rtx));
static rtx altivec_expand_stv_builtin PARAMS ((enum insn_code, tree));
static void rs6000_parse_abi_options PARAMS ((void));
static int first_altivec_reg_to_save PARAMS ((void));
static unsigned int compute_vrsave_mask PARAMS ((void));
static void is_altivec_return_reg PARAMS ((rtx, void *));
int vrsave_operation PARAMS ((rtx, enum machine_mode));
static rtx generate_set_vrsave PARAMS ((rtx, rs6000_stack_t *, int));
static void altivec_frame_fixup PARAMS ((rtx, rtx, HOST_WIDE_INT));
static int easy_vector_constant PARAMS ((rtx));

/* Default register names.  */
char rs6000_reg_names[][8] =
{
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
     "mq", "lr", "ctr","ap",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "xer",
      /* AltiVec registers.  */
      "0",  "1",  "2",  "3",  "4",  "5",  "6", "7",
      "8",  "9",  "10", "11", "12", "13", "14", "15",
      "16", "17", "18", "19", "20", "21", "22", "23",
      "24", "25", "26", "27", "28", "29", "30", "31",
      "vrsave"
};

#ifdef TARGET_REGNAMES
static const char alt_reg_names[][8] =
{
   "%r0",   "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
   "%r8",   "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
  "%r16",  "%r17", "%r18", "%r19", "%r20", "%r21", "%r22", "%r23",
  "%r24",  "%r25", "%r26", "%r27", "%r28", "%r29", "%r30", "%r31",
   "%f0",   "%f1",  "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7",
   "%f8",   "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",
  "%f16",  "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",
  "%f24",  "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31",
    "mq",    "lr",  "ctr",   "ap",
  "%cr0",  "%cr1", "%cr2", "%cr3", "%cr4", "%cr5", "%cr6", "%cr7",
   "xer",
   /* AltiVec registers.  */
   "%v0",  "%v1",  "%v2",  "%v3",  "%v4",  "%v5",  "%v6", "%v7",
   "%v8",  "%v9",  "%v10", "%v11", "%v12", "%v13", "%v14", "%v15",
   "%v16", "%v17", "%v18", "%v19", "%v20", "%v21", "%v22", "%v23",
   "%v24", "%v25", "%v26", "%v27", "%v28", "%v29", "%v30", "%v31",
   "vrsave"
};
#endif

#ifndef MASK_STRICT_ALIGN
#define MASK_STRICT_ALIGN 0
#endif

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE rs6000_attribute_table

#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP DOUBLE_INT_ASM_OP

/* Default unaligned ops are only provided for ELF.  Find the ops needed
   for non-ELF systems.  */
#ifndef OBJECT_FORMAT_ELF
#ifdef OBJECT_FORMAT_COFF
/* For XCOFF.  rs6000_assemble_integer will handle unaligned DIs on
   64-bit targets.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.vbyte\t2,"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.vbyte\t4,"
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP "\t.vbyte\t8,"
#else
/* For Darwin.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.short\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#endif
#endif

/* This hook deals with fixups for relocatable code and DI-mode objects
   in 64-bit code.  */
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER rs6000_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE rs6000_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE rs6000_output_function_epilogue

#if TARGET_ELF
#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS  rs6000_elf_section_type_flags
#endif

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE rs6000_issue_rate
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST rs6000_adjust_cost
#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY rs6000_adjust_priority

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS rs6000_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN rs6000_expand_builtin

/* The VRSAVE bitmask puts bit %v0 as the most significant bit.  */
#define ALTIVEC_REG_BIT(REGNO) (0x80000000 >> ((REGNO) - FIRST_ALTIVEC_REGNO))

struct gcc_target targetm = TARGET_INITIALIZER;

/* Override command line options.  Mostly we process the processor
   type and sometimes adjust other TARGET_ options.  */

void
rs6000_override_options (default_cpu)
     const char *default_cpu;
{
  size_t i, j;
  struct rs6000_cpu_select *ptr;

  /* Simplify the entries below by making a mask for any POWER
     variant and any PowerPC variant.  */

#define POWER_MASKS (MASK_POWER | MASK_POWER2 | MASK_MULTIPLE | MASK_STRING)
#define POWERPC_MASKS (MASK_POWERPC | MASK_PPC_GPOPT \
		       | MASK_PPC_GFXOPT | MASK_POWERPC64)
#define POWERPC_OPT_MASKS (MASK_PPC_GPOPT | MASK_PPC_GFXOPT)

  static struct ptt
    {
      const char *const name;		/* Canonical processor name.  */
      const enum processor_type processor; /* Processor type enum value.  */
      const int target_enable;	/* Target flags to enable.  */
      const int target_disable;	/* Target flags to disable.  */
    } const processor_target_table[]
      = {{"common", PROCESSOR_COMMON, MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_MASKS},
	 {"power", PROCESSOR_POWER,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"power2", PROCESSOR_POWER,
	    MASK_POWER | MASK_POWER2 | MASK_MULTIPLE | MASK_STRING,
	    POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"power3", PROCESSOR_PPC630,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT},
	 {"powerpc", PROCESSOR_POWERPC,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"powerpc64", PROCESSOR_POWERPC64,
	    MASK_POWERPC | MASK_POWERPC64 | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS},
	 {"rios", PROCESSOR_RIOS1,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rios1", PROCESSOR_RIOS1,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rsc", PROCESSOR_PPC601,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rsc1", PROCESSOR_PPC601,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rios2", PROCESSOR_RIOS2,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING | MASK_POWER2,
	    POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rs64a", PROCESSOR_RS64A,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS},
	 {"401", PROCESSOR_PPC403,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"403", PROCESSOR_PPC403,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS | MASK_STRICT_ALIGN,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"405", PROCESSOR_PPC405,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"505", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"601", PROCESSOR_PPC601,
	    MASK_POWER | MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"602", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"603", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"603e", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"ec603e", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"604", PROCESSOR_PPC604,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"604e", PROCESSOR_PPC604e,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"620", PROCESSOR_PPC620,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT},
	 {"630", PROCESSOR_PPC630,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT},
	 {"740", PROCESSOR_PPC750,
 	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
 	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"750", PROCESSOR_PPC750,
 	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
 	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"7400", PROCESSOR_PPC7400,
            MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
            POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"7450", PROCESSOR_PPC7450,
            MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
            POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"801", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"821", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"823", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"860", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64}};

  size_t ptt_size = sizeof (processor_target_table) / sizeof (struct ptt);

  /* Save current -mmultiple/-mno-multiple status.  */
  int multiple = TARGET_MULTIPLE;
  /* Save current -mstring/-mno-string status.  */
  int string = TARGET_STRING;

  /* Identify the processor type.  */
  rs6000_select[0].string = default_cpu;
  rs6000_cpu = TARGET_POWERPC64 ? PROCESSOR_DEFAULT64 : PROCESSOR_DEFAULT;

  for (i = 0; i < ARRAY_SIZE (rs6000_select); i++)
    {
      ptr = &rs6000_select[i];
      if (ptr->string != (char *)0 && ptr->string[0] != '\0')
	{
	  for (j = 0; j < ptt_size; j++)
	    if (! strcmp (ptr->string, processor_target_table[j].name))
	      {
		if (ptr->set_tune_p)
		  rs6000_cpu = processor_target_table[j].processor;

		if (ptr->set_arch_p)
		  {
		    target_flags |= processor_target_table[j].target_enable;
		    target_flags &= ~processor_target_table[j].target_disable;
		  }
		break;
	      }

	  if (j == ptt_size)
	    error ("bad value (%s) for %s switch", ptr->string, ptr->name);
	}
    }

  /* If we are optimizing big endian systems for space, use the store
     multiple instructions.  */
  if (BYTES_BIG_ENDIAN && optimize_size)
    target_flags |= MASK_MULTIPLE;

  /* If -mmultiple or -mno-multiple was explicitly used, don't
     override with the processor default */
  if (TARGET_MULTIPLE_SET)
    target_flags = (target_flags & ~MASK_MULTIPLE) | multiple;

  /* If -mstring or -mno-string was explicitly used, don't override
     with the processor default.  */
  if (TARGET_STRING_SET)
    target_flags = (target_flags & ~MASK_STRING) | string;

  /* Don't allow -mmultiple or -mstring on little endian systems
     unless the cpu is a 750, because the hardware doesn't support the
     instructions used in little endian mode, and causes an alignment
     trap.  The 750 does not cause an alignment trap (except when the
     target is unaligned).  */

  if (! BYTES_BIG_ENDIAN && rs6000_cpu != PROCESSOR_PPC750)
    {
      if (TARGET_MULTIPLE)
	{
	  target_flags &= ~MASK_MULTIPLE;
	  if (TARGET_MULTIPLE_SET)
	    warning ("-mmultiple is not supported on little endian systems");
	}

      if (TARGET_STRING)
	{
	  target_flags &= ~MASK_STRING;
	  if (TARGET_STRING_SET)
	    warning ("-mstring is not supported on little endian systems");
	}
    }

  if (flag_pic != 0 && DEFAULT_ABI == ABI_AIX)
    {
      rs6000_flag_pic = flag_pic;
      flag_pic = 0;
    }

#ifdef XCOFF_DEBUGGING_INFO
  if (flag_function_sections && (write_symbols != NO_DEBUG)
      && DEFAULT_ABI == ABI_AIX)
    {
      warning ("-ffunction-sections disabled on AIX when debugging");
      flag_function_sections = 0;
    }

  if (flag_data_sections && (DEFAULT_ABI == ABI_AIX))
    {
      warning ("-fdata-sections not supported on AIX");
      flag_data_sections = 0;
    }
#endif

  /* Set debug flags */
  if (rs6000_debug_name)
    {
      if (! strcmp (rs6000_debug_name, "all"))
	rs6000_debug_stack = rs6000_debug_arg = 1;
      else if (! strcmp (rs6000_debug_name, "stack"))
	rs6000_debug_stack = 1;
      else if (! strcmp (rs6000_debug_name, "arg"))
	rs6000_debug_arg = 1;
      else
	error ("unknown -mdebug-%s switch", rs6000_debug_name);
    }

  /* Set size of long double */
  rs6000_long_double_type_size = 64;
  if (rs6000_long_double_size_string)
    {
      char *tail;
      int size = strtol (rs6000_long_double_size_string, &tail, 10);
      if (*tail != '\0' || (size != 64 && size != 128))
	error ("Unknown switch -mlong-double-%s",
	       rs6000_long_double_size_string);
      else
	rs6000_long_double_type_size = size;
    }

  /* Handle -mabi= options.  */
  rs6000_parse_abi_options ();

#ifdef TARGET_REGNAMES
  /* If the user desires alternate register names, copy in the
     alternate names now.  */
  if (TARGET_REGNAMES)
    memcpy (rs6000_reg_names, alt_reg_names, sizeof (rs6000_reg_names));
#endif

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif
#ifdef SUBSUBTARGET_OVERRIDE_OPTIONS
  SUBSUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Set TARGET_AIX_STRUCT_RET last, after the ABI is determined.
     If -maix-struct-return or -msvr4-struct-return was explicitly
     used, don't override with the ABI default.  */
  if (!(target_flags & MASK_AIX_STRUCT_RET_SET))
    {
      if (DEFAULT_ABI == ABI_V4 && !DRAFT_V4_STRUCT_RET)
	target_flags = (target_flags & ~MASK_AIX_STRUCT_RET);
      else
	target_flags |= MASK_AIX_STRUCT_RET;
    }

  /* Register global variables with the garbage collector.  */
  rs6000_add_gc_roots ();

  /* Allocate an alias set for register saves & restores from stack.  */
  rs6000_sr_alias_set = new_alias_set ();

  if (TARGET_TOC) 
    ASM_GENERATE_INTERNAL_LABEL (toc_label_name, "LCTOC", 1);

  /* We can only guarantee the availability of DI pseudo-ops when
     assembling for 64-bit targets.  */
  if (!TARGET_64BIT)
    {
      targetm.asm_out.aligned_op.di = NULL;
      targetm.asm_out.unaligned_op.di = NULL;
    }

  /* Arrange to save and restore machine status around nested functions.  */
  init_machine_status = rs6000_init_machine_status;
  free_machine_status = rs6000_free_machine_status;
}

/* Handle -mabi= options.  */
static void
rs6000_parse_abi_options ()
{
  if (rs6000_abi_string == 0)
    return;
  else if (! strcmp (rs6000_abi_string, "altivec"))
    rs6000_altivec_abi = 1;
  else if (! strcmp (rs6000_abi_string, "no-altivec"))
    rs6000_altivec_abi = 0;
  else
    error ("unknown ABI specified: '%s'", rs6000_abi_string);
}

void
optimization_options (level, size)
     int level ATTRIBUTE_UNUSED;
     int size ATTRIBUTE_UNUSED;
{
}

/* Do anything needed at the start of the asm file.  */

void
rs6000_file_start (file, default_cpu)
     FILE *file;
     const char *default_cpu;
{
  size_t i;
  char buffer[80];
  const char *start = buffer;
  struct rs6000_cpu_select *ptr;

  if (flag_verbose_asm)
    {
      sprintf (buffer, "\n%s rs6000/powerpc options:", ASM_COMMENT_START);
      rs6000_select[0].string = default_cpu;

      for (i = 0; i < ARRAY_SIZE (rs6000_select); i++)
	{
	  ptr = &rs6000_select[i];
	  if (ptr->string != (char *)0 && ptr->string[0] != '\0')
	    {
	      fprintf (file, "%s %s%s", start, ptr->name, ptr->string);
	      start = "";
	    }
	}

#ifdef USING_ELFOS_H
      switch (rs6000_sdata)
	{
	case SDATA_NONE: fprintf (file, "%s -msdata=none", start); start = ""; break;
	case SDATA_DATA: fprintf (file, "%s -msdata=data", start); start = ""; break;
	case SDATA_SYSV: fprintf (file, "%s -msdata=sysv", start); start = ""; break;
	case SDATA_EABI: fprintf (file, "%s -msdata=eabi", start); start = ""; break;
	}

      if (rs6000_sdata && g_switch_value)
	{
	  fprintf (file, "%s -G %d", start, g_switch_value);
	  start = "";
	}
#endif

      if (*start == '\0')
	putc ('\n', file);
    }
}


/* Create a CONST_DOUBLE from a string.  */

struct rtx_def *
rs6000_float_const (string, mode)
     const char *string;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE value;
  value = REAL_VALUE_ATOF (string, mode);
  return immed_real_const_1 (value, mode);
}

/* Return non-zero if this function is known to have a null epilogue.  */

int
direct_return ()
{
  if (reload_completed)
    {
      rs6000_stack_t *info = rs6000_stack_info ();

      if (info->first_gp_reg_save == 32
	  && info->first_fp_reg_save == 64
	  && info->first_altivec_reg_save == LAST_ALTIVEC_REGNO + 1
	  && ! info->lr_save_p
	  && ! info->cr_save_p
	  && info->vrsave_mask == 0
	  && ! info->push_p)
	return 1;
    }

  return 0;
}

/* Returns 1 always.  */

int
any_operand (op, mode)
     rtx op ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return 1;
}

/* Returns 1 if op is the count register.  */
int
count_register_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != REG)
    return 0;

  if (REGNO (op) == COUNT_REGISTER_REGNUM)
    return 1;

  if (REGNO (op) > FIRST_PSEUDO_REGISTER)
    return 1;

  return 0;
}

/* Returns 1 if op is an altivec register.  */
int
altivec_register_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) > FIRST_PSEUDO_REGISTER
	      || ALTIVEC_REGNO_P (REGNO (op))));
}

int
xer_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != REG)
    return 0;

  if (XER_REGNO_P (REGNO (op)))
    return 1;

  return 0;
}

/* Return 1 if OP is a signed 8-bit constant.  Int multiplication
   by such constants completes more quickly.  */

int
s8bit_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ( GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= -128 && INTVAL (op) <= 127));
}

/* Return 1 if OP is a constant that can fit in a D field.  */

int
short_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && CONST_OK_FOR_LETTER_P (INTVAL (op), 'I'));
}

/* Similar for an unsigned D field.  */

int
u_short_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && CONST_OK_FOR_LETTER_P (INTVAL (op) & GET_MODE_MASK (mode), 'K'));
}

/* Return 1 if OP is a CONST_INT that cannot fit in a signed D field.  */

int
non_short_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned HOST_WIDE_INT) (INTVAL (op) + 0x8000) >= 0x10000);
}

/* Returns 1 if OP is a CONST_INT that is a positive value
   and an exact power of 2.  */

int
exact_log2_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) > 0
	  && exact_log2 (INTVAL (op)) >= 0);
}

/* Returns 1 if OP is a register that is not special (i.e., not MQ,
   ctr, or lr).  */

int
gpc_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || (REGNO (op) >= ARG_POINTER_REGNUM 
		  && !XER_REGNO_P (REGNO (op)))
	      || REGNO (op) < MQ_REGNO));
}

/* Returns 1 if OP is either a pseudo-register or a register denoting a
   CR field.  */

int
cc_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || CR_REGNO_P (REGNO (op))));
}

/* Returns 1 if OP is either a pseudo-register or a register denoting a
   CR field that isn't CR0.  */

int
cc_reg_not_cr0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || CR_REGNO_NOT_CR0_P (REGNO (op))));
}

/* Returns 1 if OP is either a constant integer valid for a D-field or
   a non-special register.  If a register, it must be in the proper
   mode unless MODE is VOIDmode.  */

int
reg_or_short_operand (op, mode)
      rtx op;
      enum machine_mode mode;
{
  return short_cint_operand (op, mode) || gpc_reg_operand (op, mode);
}

/* Similar, except check if the negation of the constant would be
   valid for a D-field.  */

int
reg_or_neg_short_operand (op, mode)
      rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return CONST_OK_FOR_LETTER_P (INTVAL (op), 'P');

  return gpc_reg_operand (op, mode);
}

/* Returns 1 if OP is either a constant integer valid for a DS-field or
   a non-special register.  If a register, it must be in the proper
   mode unless MODE is VOIDmode.  */

int
reg_or_aligned_short_operand (op, mode)
      rtx op;
      enum machine_mode mode;
{
  if (gpc_reg_operand (op, mode))
    return 1;
  else if (short_cint_operand (op, mode) && !(INTVAL (op) & 3))
    return 1;

  return 0;
}


/* Return 1 if the operand is either a register or an integer whose
   high-order 16 bits are zero.  */

int
reg_or_u_short_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return u_short_cint_operand (op, mode) || gpc_reg_operand (op, mode);
}

/* Return 1 is the operand is either a non-special register or ANY
   constant integer.  */

int
reg_or_cint_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT || gpc_reg_operand (op, mode));
}

/* Return 1 is the operand is either a non-special register or ANY
   32-bit signed constant integer.  */

int
reg_or_arith_cint_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
#if HOST_BITS_PER_WIDE_INT != 32
	      && ((unsigned HOST_WIDE_INT) (INTVAL (op) + 0x80000000)
		  < (unsigned HOST_WIDE_INT) 0x100000000ll)
#endif
	      ));
}

/* Return 1 is the operand is either a non-special register or a 32-bit
   signed constant integer valid for 64-bit addition.  */

int
reg_or_add_cint64_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && INTVAL (op) < 0x7fff8000
#if HOST_BITS_PER_WIDE_INT != 32
	      && ((unsigned HOST_WIDE_INT) (INTVAL (op) + 0x80008000)
		  < 0x100000000ll)
#endif
	      ));
}

/* Return 1 is the operand is either a non-special register or a 32-bit
   signed constant integer valid for 64-bit subtraction.  */

int
reg_or_sub_cint64_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (- INTVAL (op)) < 0x7fff8000
#if HOST_BITS_PER_WIDE_INT != 32
	      && ((unsigned HOST_WIDE_INT) ((- INTVAL (op)) + 0x80008000)
		  < 0x100000000ll)
#endif
	      ));
}

/* Return 1 is the operand is either a non-special register or ANY
   32-bit unsigned constant integer.  */

int
reg_or_logical_cint_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      if (GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT)
	{
	  if (GET_MODE_BITSIZE (mode) <= 32)
	    abort ();

	  if (INTVAL (op) < 0)
	    return 0;
	}

      return ((INTVAL (op) & GET_MODE_MASK (mode)
	       & (~ (unsigned HOST_WIDE_INT) 0xffffffff)) == 0);
    }
  else if (GET_CODE (op) == CONST_DOUBLE)
    {
      if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	  || mode != DImode)
	abort ();

      return CONST_DOUBLE_HIGH (op) == 0;
    }
  else 
    return gpc_reg_operand (op, mode);
}

/* Return 1 if the operand is an operand that can be loaded via the GOT.  */

int
got_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == CONST
	  || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if the operand is a simple references that can be loaded via
   the GOT (labels involving addition aren't allowed).  */

int
got_no_const_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF);
}

/* Return the number of instructions it takes to form a constant in an
   integer register.  */

static int
num_insns_constant_wide (value)
     HOST_WIDE_INT value;
{
  /* signed constant loadable with {cal|addi} */
  if (CONST_OK_FOR_LETTER_P (value, 'I'))
    return 1;

  /* constant loadable with {cau|addis} */
  else if (CONST_OK_FOR_LETTER_P (value, 'L'))
    return 1;

#if HOST_BITS_PER_WIDE_INT == 64
  else if (TARGET_POWERPC64)
    {
      HOST_WIDE_INT low  = value & 0xffffffff;
      HOST_WIDE_INT high = value >> 32;

      low = (low ^ 0x80000000) - 0x80000000;  /* sign extend */

      if (high == 0 && (low & 0x80000000) == 0)
	return 2;

      else if (high == -1 && (low & 0x80000000) != 0)
	return 2;

      else if (! low)
	return num_insns_constant_wide (high) + 1;

      else
	return (num_insns_constant_wide (high)
		+ num_insns_constant_wide (low) + 1);
    }
#endif

  else
    return 2;
}

int
num_insns_constant (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
#if HOST_BITS_PER_WIDE_INT == 64
      if ((INTVAL (op) >> 31) != 0 && (INTVAL (op) >> 31) != -1
	  && mask64_operand (op, mode))
	    return 2;
      else
#endif
	return num_insns_constant_wide (INTVAL (op));
    }

  else if (GET_CODE (op) == CONST_DOUBLE && mode == SFmode)
    {
      long l;
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);
      return num_insns_constant_wide ((HOST_WIDE_INT) l);
    }

  else if (GET_CODE (op) == CONST_DOUBLE)
    {
      HOST_WIDE_INT low;
      HOST_WIDE_INT high;
      long l[2];
      REAL_VALUE_TYPE rv;
      int endian = (WORDS_BIG_ENDIAN == 0);

      if (mode == VOIDmode || mode == DImode)
	{
	  high = CONST_DOUBLE_HIGH (op);
	  low  = CONST_DOUBLE_LOW (op);
	}
      else
	{
	  REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
	  REAL_VALUE_TO_TARGET_DOUBLE (rv, l);
	  high = l[endian];
	  low  = l[1 - endian];
	}

      if (TARGET_32BIT)
	return (num_insns_constant_wide (low)
		+ num_insns_constant_wide (high));

      else
	{
	  if (high == 0 && low >= 0)
	    return num_insns_constant_wide (low);

	  else if (high == -1 && low < 0)
	    return num_insns_constant_wide (low);

	  else if (mask64_operand (op, mode))
	    return 2;

	  else if (low == 0)
	    return num_insns_constant_wide (high) + 1;

	  else
	    return (num_insns_constant_wide (high)
		    + num_insns_constant_wide (low) + 1);
	}
    }

  else
    abort ();
}

/* Return 1 if the operand is a CONST_DOUBLE and it can be put into a
   register with one instruction per word.  We only do this if we can
   safely read CONST_DOUBLE_{LOW,HIGH}.  */

int
easy_fp_constant (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_DOUBLE
      || GET_MODE (op) != mode
      || (GET_MODE_CLASS (mode) != MODE_FLOAT && mode != DImode))
    return 0;

  /* Consider all constants with -msoft-float to be easy.  */
  if (TARGET_SOFT_FLOAT && mode != DImode)
    return 1;

  /* If we are using V.4 style PIC, consider all constants to be hard.  */
  if (flag_pic && DEFAULT_ABI == ABI_V4)
    return 0;

#ifdef TARGET_RELOCATABLE
  /* Similarly if we are using -mrelocatable, consider all constants
     to be hard.  */
  if (TARGET_RELOCATABLE)
    return 0;
#endif

  if (mode == DFmode)
    {
      long k[2];
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_DOUBLE (rv, k);

      return (num_insns_constant_wide ((HOST_WIDE_INT)k[0]) == 1
	      && num_insns_constant_wide ((HOST_WIDE_INT)k[1]) == 1);
    }

  else if (mode == SFmode)
    {
      long l;
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);

      return num_insns_constant_wide (l) == 1;
    }

  else if (mode == DImode)
    return ((TARGET_POWERPC64
	     && GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_LOW (op) == 0)
	    || (num_insns_constant (op, DImode) <= 2));

  else if (mode == SImode)
    return 1;
  else
    abort ();
}

/* Return 1 if the operand is a CONST_INT and can be put into a
   register with one instruction.  */

static int
easy_vector_constant (op)
     rtx op;
{
  rtx elt;
  int units, i;

  if (GET_CODE (op) != CONST_VECTOR)
    return 0;

  units = CONST_VECTOR_NUNITS (op);

  /* We can generate 0 easily.  Look for that.  */
  for (i = 0; i < units; ++i)
    {
      elt = CONST_VECTOR_ELT (op, i);

      /* We could probably simplify this by just checking for equality
	 with CONST0_RTX for the current mode, but let's be safe
	 instead.  */

      switch (GET_CODE (elt))
	{
	case CONST_INT:
	  if (INTVAL (elt) != 0)
	    return 0;
	  break;
	case CONST_DOUBLE:
	  if (CONST_DOUBLE_LOW (elt) != 0 || CONST_DOUBLE_HIGH (elt) != 0)
	    return 0;
	  break;
	default:
	  return 0;
	}
    }

  /* We could probably generate a few other constants trivially, but
     gcc doesn't generate them yet.  FIXME later.  */
  return 1;
}

/* Return 1 if the operand is the constant 0.  This works for scalars
   as well as vectors.  */
int
zero_constant (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return op == CONST0_RTX (mode);
}

/* Return 1 if the operand is 0.0.  */
int
zero_fp_constant (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_MODE_CLASS (mode) == MODE_FLOAT && op == CONST0_RTX (mode);
}

/* Return 1 if the operand is in volatile memory.  Note that during
   the RTL generation phase, memory_operand does not return TRUE for
   volatile memory references.  So this function allows us to
   recognize volatile references where its safe.  */

int
volatile_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (!MEM_VOLATILE_P (op))
    return 0;

  if (mode != GET_MODE (op))
    return 0;

  if (reload_completed)
    return memory_operand (op, mode);

  if (reload_in_progress)
    return strict_memory_address_p (mode, XEXP (op, 0));

  return memory_address_p (mode, XEXP (op, 0));
}

/* Return 1 if the operand is an offsettable memory operand.  */

int
offsettable_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == MEM)
	  && offsettable_address_p (reload_completed || reload_in_progress,
				    mode, XEXP (op, 0)));
}

/* Return 1 if the operand is either an easy FP constant (see above) or
   memory.  */

int
mem_or_easy_const_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return memory_operand (op, mode) || easy_fp_constant (op, mode);
}

/* Return 1 if the operand is either a non-special register or an item
   that can be used as the operand of a `mode' add insn.  */

int
add_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'I')
	    || CONST_OK_FOR_LETTER_P (INTVAL (op), 'L'));

  return gpc_reg_operand (op, mode);
}

/* Return 1 if OP is a constant but not a valid add_operand.  */

int
non_add_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && !CONST_OK_FOR_LETTER_P (INTVAL (op), 'I')
	  && !CONST_OK_FOR_LETTER_P (INTVAL (op), 'L'));
}

/* Return 1 if the operand is a non-special register or a constant that
   can be used as the operand of an OR or XOR insn on the RS/6000.  */

int
logical_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  HOST_WIDE_INT opl, oph;

  if (gpc_reg_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    {
      opl = INTVAL (op) & GET_MODE_MASK (mode);

#if HOST_BITS_PER_WIDE_INT <= 32
      if (GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT && opl < 0)
	return 0;
#endif
    }
  else if (GET_CODE (op) == CONST_DOUBLE)
    {
      if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	abort ();

      opl = CONST_DOUBLE_LOW (op);
      oph = CONST_DOUBLE_HIGH (op);
      if (oph != 0)
	return 0;
    }
  else
    return 0;

  return ((opl & ~ (unsigned HOST_WIDE_INT) 0xffff) == 0
	  || (opl & ~ (unsigned HOST_WIDE_INT) 0xffff0000) == 0);
}

/* Return 1 if C is a constant that is not a logical operand (as
   above), but could be split into one.  */

int
non_logical_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE)
	  && ! logical_operand (op, mode)
	  && reg_or_logical_cint_operand (op, mode));
}

/* Return 1 if C is a constant that can be encoded in a 32-bit mask on the
   RS/6000.  It is if there are no more than two 1->0 or 0->1 transitions.
   Reject all ones and all zeros, since these should have been optimized
   away and confuse the making of MB and ME.  */

int
mask_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT c, lsb;

  if (GET_CODE (op) != CONST_INT)
    return 0;

  c = INTVAL (op);

  /* Fail in 64-bit mode if the mask wraps around because the upper
     32-bits of the mask will all be 1s, contrary to GCC's internal view.  */
  if (TARGET_POWERPC64 && (c & 0x80000001) == 0x80000001)
    return 0;

  /* We don't change the number of transitions by inverting,
     so make sure we start with the LS bit zero.  */
  if (c & 1)
    c = ~c;

  /* Reject all zeros or all ones.  */
  if (c == 0)
    return 0;

  /* Find the first transition.  */
  lsb = c & -c;

  /* Invert to look for a second transition.  */
  c = ~c;

  /* Erase first transition.  */
  c &= -lsb;

  /* Find the second transition (if any).  */
  lsb = c & -c;

  /* Match if all the bits above are 1's (or c is zero).  */
  return c == -lsb;
}

/* Return 1 if the operand is a constant that is a PowerPC64 mask.
   It is if there are no more than one 1->0 or 0->1 transitions.
   Reject all ones and all zeros, since these should have been optimized
   away and confuse the making of MB and ME.  */

int
mask64_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      HOST_WIDE_INT c, lsb;

      /* We don't change the number of transitions by inverting,
	 so make sure we start with the LS bit zero.  */
      c = INTVAL (op);
      if (c & 1)
	c = ~c;

      /* Reject all zeros or all ones.  */
      if (c == 0)
	return 0;

      /* Find the transition, and check that all bits above are 1's.  */
      lsb = c & -c;
      return c == -lsb;
    }
  else if (GET_CODE (op) == CONST_DOUBLE
	   && (mode == VOIDmode || mode == DImode))
    {
      HOST_WIDE_INT low, high, lsb;

      if (HOST_BITS_PER_WIDE_INT < 64)
	high = CONST_DOUBLE_HIGH (op);

      low = CONST_DOUBLE_LOW (op);
      if (low & 1)
	{
	  if (HOST_BITS_PER_WIDE_INT < 64)
	    high = ~high;
	  low = ~low;
	}

      if (low == 0)
	{
	  if (HOST_BITS_PER_WIDE_INT >= 64 || high == 0)
	    return 0;

	  lsb = high & -high;
	  return high == -lsb;
	}

      lsb = low & -low;
      return low == -lsb && (HOST_BITS_PER_WIDE_INT >= 64 || high == ~0);
    }
  else
    return 0;
}

/* Return 1 if the operand is either a non-special register or a constant
   that can be used as the operand of a PowerPC64 logical AND insn.  */

int
and64_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (fixed_regs[CR0_REGNO])	/* CR0 not available, don't do andi./andis.  */
    return (gpc_reg_operand (op, mode) || mask64_operand (op, mode));

  return (logical_operand (op, mode) || mask64_operand (op, mode));
}

/* Return 1 if the operand is either a non-special register or a
   constant that can be used as the operand of an RS/6000 logical AND insn.  */

int
and_operand (op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (fixed_regs[CR0_REGNO])	/* CR0 not available, don't do andi./andis.  */
    return (gpc_reg_operand (op, mode) || mask_operand (op, mode));

  return (logical_operand (op, mode) || mask_operand (op, mode));
}

/* Return 1 if the operand is a general register or memory operand.  */

int
reg_or_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || memory_operand (op, mode)
	  || volatile_mem_operand (op, mode));
}

/* Return 1 if the operand is a general register or memory operand without
   pre_inc or pre_dec which produces invalid form of PowerPC lwa
   instruction.  */

int
lwa_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx inner = op;

  if (reload_completed && GET_CODE (inner) == SUBREG)
    inner = SUBREG_REG (inner);
    
  return gpc_reg_operand (inner, mode)
    || (memory_operand (inner, mode)
	&& GET_CODE (XEXP (inner, 0)) != PRE_INC
	&& GET_CODE (XEXP (inner, 0)) != PRE_DEC
	&& (GET_CODE (XEXP (inner, 0)) != PLUS
	    || GET_CODE (XEXP (XEXP (inner, 0), 1)) != CONST_INT
	    || INTVAL (XEXP (XEXP (inner, 0), 1)) % 4 == 0));
}

/* Return 1 if the operand, used inside a MEM, is a valid first argument
   to CALL.  This is a SYMBOL_REF or a pseudo-register, which will be
   forced to lr.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  return (GET_CODE (op) == SYMBOL_REF
	  || (GET_CODE (op) == REG && REGNO (op) >= FIRST_PSEUDO_REGISTER));
}

/* Return 1 if the operand is a SYMBOL_REF for a function known to be in
   this file and the function is not weakly defined.  */

int
current_file_function_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF
	  && (SYMBOL_REF_FLAG (op)
	      || (op == XEXP (DECL_RTL (current_function_decl), 0)
	          && ! DECL_WEAK (current_function_decl))));
}

/* Return 1 if this operand is a valid input for a move insn.  */

int
input_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Memory is always valid.  */
  if (memory_operand (op, mode))
    return 1;

  /* Only a tiny bit of handling for CONSTANT_P_RTX is necessary.  */
  if (GET_CODE (op) == CONSTANT_P_RTX)
    return 1;

  /* For floating-point, easy constants are valid.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && CONSTANT_P (op)
      && easy_fp_constant (op, mode))
    return 1;

  /* Allow any integer constant.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && (GET_CODE (op) == CONST_INT
	  || GET_CODE (op) == CONST_DOUBLE))
    return 1;

  /* For floating-point or multi-word mode, the only remaining valid type
     is a register.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      || GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return register_operand (op, mode);

  /* The only cases left are integral modes one word or smaller (we
     do not get called for MODE_CC values).  These can be in any
     register.  */
  if (register_operand (op, mode))
    return 1;

  /* A SYMBOL_REF referring to the TOC is valid.  */
  if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (op))
    return 1;

  /* A constant pool expression (relative to the TOC) is valid */
  if (TOC_RELATIVE_EXPR_P (op))
    return 1;

  /* V.4 allows SYMBOL_REFs and CONSTs that are in the small data region
     to be valid.  */
  if (DEFAULT_ABI == ABI_V4
      && (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST)
      && small_data_operand (op, Pmode))
    return 1;

  return 0;
}

/* Return 1 for an operand in small memory on V.4/eabi.  */

int
small_data_operand (op, mode)
     rtx op ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if TARGET_ELF
  rtx sym_ref;

  if (rs6000_sdata == SDATA_NONE || rs6000_sdata == SDATA_DATA)
    return 0;

  if (DEFAULT_ABI != ABI_V4)
    return 0;

  if (GET_CODE (op) == SYMBOL_REF)
    sym_ref = op;

  else if (GET_CODE (op) != CONST
	   || GET_CODE (XEXP (op, 0)) != PLUS
	   || GET_CODE (XEXP (XEXP (op, 0), 0)) != SYMBOL_REF
	   || GET_CODE (XEXP (XEXP (op, 0), 1)) != CONST_INT)
    return 0;

  else
    {
      rtx sum = XEXP (op, 0);
      HOST_WIDE_INT summand;

      /* We have to be careful here, because it is the referenced address
        that must be 32k from _SDA_BASE_, not just the symbol.  */
      summand = INTVAL (XEXP (sum, 1));
      if (summand < 0 || summand > g_switch_value)
       return 0;

      sym_ref = XEXP (sum, 0);
    }

  if (*XSTR (sym_ref, 0) != '@')
    return 0;

  return 1;

#else
  return 0;
#endif
}

static int 
constant_pool_expr_1 (op, have_sym, have_toc) 
    rtx op;
    int *have_sym;
    int *have_toc;
{
  switch (GET_CODE(op)) 
    {
    case SYMBOL_REF:
      if (CONSTANT_POOL_ADDRESS_P (op))
	{
	  if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (op), Pmode))
	    {
	      *have_sym = 1;
	      return 1;
	    }
	  else
	    return 0;
	}
      else if (! strcmp (XSTR (op, 0), toc_label_name))
	{
	  *have_toc = 1;
	  return 1;
	}
      else
	return 0;
    case PLUS:
    case MINUS:
      return (constant_pool_expr_1 (XEXP (op, 0), have_sym, have_toc)
	      && constant_pool_expr_1 (XEXP (op, 1), have_sym, have_toc));
    case CONST:
      return constant_pool_expr_1 (XEXP (op, 0), have_sym, have_toc);
    case CONST_INT:
      return 1;
    default:
      return 0;
    }
}

int
constant_pool_expr_p (op)
    rtx op;
{
  int have_sym = 0;
  int have_toc = 0;
  return constant_pool_expr_1 (op, &have_sym, &have_toc) && have_sym;
}

int
toc_relative_expr_p (op)
    rtx op;
{
    int have_sym = 0;
    int have_toc = 0;
    return constant_pool_expr_1 (op, &have_sym, &have_toc) && have_toc;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This is used from only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was
   called.  In some cases it is useful to look at this to decide what
   needs to be done.

   MODE is passed so that this function can use GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this function to do nothing.  It exists to
   recognize opportunities to optimize the output.

   On RS/6000, first check for the sum of a register with a constant
   integer that is out of range.  If so, generate code to add the
   constant with the low-order 16 bits masked to the register and force
   this result into another register (this can be done with `cau').
   Then generate an address of REG+(CONST&0xffff), allowing for the
   possibility of bit 16 being a one.

   Then check for the sum of a register and something not constant, try to
   load the other things into a register and return the sum.  */
rtx
rs6000_legitimize_address (x, oldx, mode)
     rtx x;
     rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  if (GET_CODE (x) == PLUS 
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (unsigned HOST_WIDE_INT) (INTVAL (XEXP (x, 1)) + 0x8000) >= 0x10000)
    { 
      HOST_WIDE_INT high_int, low_int;
      rtx sum;
      high_int = INTVAL (XEXP (x, 1)) & (~ (HOST_WIDE_INT) 0xffff);
      low_int = INTVAL (XEXP (x, 1)) & 0xffff;
      if (low_int & 0x8000)
	high_int += 0x10000, low_int |= ((HOST_WIDE_INT) -1) << 16;
      sum = force_operand (gen_rtx_PLUS (Pmode, XEXP (x, 0),
					 GEN_INT (high_int)), 0);
      return gen_rtx_PLUS (Pmode, sum, GEN_INT (low_int));
    }
  else if (GET_CODE (x) == PLUS 
	   && GET_CODE (XEXP (x, 0)) == REG
	   && GET_CODE (XEXP (x, 1)) != CONST_INT
	   && GET_MODE_NUNITS (mode) == 1
	   && (TARGET_HARD_FLOAT || TARGET_POWERPC64 || mode != DFmode)
	   && (TARGET_POWERPC64 || mode != DImode)
	   && mode != TImode)
    {
      return gen_rtx_PLUS (Pmode, XEXP (x, 0),
			   force_reg (Pmode, force_operand (XEXP (x, 1), 0)));
    }
  else if (ALTIVEC_VECTOR_MODE (mode))
    {
      rtx reg;

      /* Make sure both operands are registers.  */
      if (GET_CODE (x) == PLUS)
	return gen_rtx_PLUS (Pmode, force_reg (Pmode, XEXP (x, 0)),
			     force_reg (Pmode, XEXP (x, 1)));

      reg = force_reg (Pmode, x);
      return reg;
    }
  else if (TARGET_ELF && TARGET_32BIT && TARGET_NO_TOC && ! flag_pic
	   && GET_CODE (x) != CONST_INT
	   && GET_CODE (x) != CONST_DOUBLE 
	   && CONSTANT_P (x)
	   && GET_MODE_NUNITS (mode) == 1
	   && (GET_MODE_BITSIZE (mode) <= 32
	       || (TARGET_HARD_FLOAT && mode == DFmode)))
    {
      rtx reg = gen_reg_rtx (Pmode);
      emit_insn (gen_elf_high (reg, (x)));
      return gen_rtx_LO_SUM (Pmode, reg, (x));
    }
  else if (TARGET_MACHO && TARGET_32BIT && TARGET_NO_TOC
	   && ! flag_pic
	   && GET_CODE (x) != CONST_INT
	   && GET_CODE (x) != CONST_DOUBLE 
	   && CONSTANT_P (x)
	   && (TARGET_HARD_FLOAT || mode != DFmode)
	   && mode != DImode 
	   && mode != TImode)
    {
      rtx reg = gen_reg_rtx (Pmode);
      emit_insn (gen_macho_high (reg, (x)));
      return gen_rtx_LO_SUM (Pmode, reg, (x));
    }
  else if (TARGET_TOC 
	   && CONSTANT_POOL_EXPR_P (x)
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (x), Pmode))
    {
      return create_TOC_reference (x);
    }
  else
    return NULL_RTX;
}

/* The convention appears to be to define this wherever it is used.
   With legitimize_reload_address now defined here, REG_MODE_OK_FOR_BASE_P
   is now used here.  */
#ifndef REG_MODE_OK_FOR_BASE_P
#define REG_MODE_OK_FOR_BASE_P(REGNO, MODE) REG_OK_FOR_BASE_P (REGNO)
#endif

/* Our implementation of LEGITIMIZE_RELOAD_ADDRESS.  Returns a value to
   replace the input X, or the original X if no replacement is called for.
   The output parameter *WIN is 1 if the calling macro should goto WIN,
   0 if it should not.

   For RS/6000, we wish to handle large displacements off a base
   register by splitting the addend across an addiu/addis and the mem insn.
   This cuts number of extra insns needed from 3 to 1.

   On Darwin, we use this to generate code for floating point constants.
   A movsf_low is generated so we wind up with 2 instructions rather than 3.
   The Darwin code is inside #if TARGET_MACHO because only then is
   machopic_function_base_name() defined.  */
rtx
rs6000_legitimize_reload_address (x, mode, opnum, type, ind_levels, win)
    rtx x;
    enum machine_mode mode;
    int opnum;
    int type;
    int ind_levels ATTRIBUTE_UNUSED;
    int *win;
{
  /* We must recognize output that we have already generated ourselves.  */ 
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
                   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
                   opnum, (enum reload_type)type);
      *win = 1;
      return x;
    }

#if TARGET_MACHO
  if (DEFAULT_ABI == ABI_DARWIN && flag_pic
      && GET_CODE (x) == LO_SUM
      && GET_CODE (XEXP (x, 0)) == PLUS
      && XEXP (XEXP (x, 0), 0) == pic_offset_table_rtx
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == HIGH
      && GET_CODE (XEXP (XEXP (XEXP (x, 0), 1), 0)) == CONST
      && XEXP (XEXP (XEXP (x, 0), 1), 0) == XEXP (x, 1)
      && GET_CODE (XEXP (XEXP (x, 1), 0)) == MINUS
      && GET_CODE (XEXP (XEXP (XEXP (x, 1), 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (XEXP (x, 1), 0), 1)) == SYMBOL_REF)
    {
      /* Result of previous invocation of this function on Darwin
	 floating point constant.  */
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		opnum, (enum reload_type)type);
      *win = 1;
      return x;
    }
#endif
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER
      && REG_MODE_OK_FOR_BASE_P (XEXP (x, 0), mode)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && !ALTIVEC_VECTOR_MODE (mode))
    {
      HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
      HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
      HOST_WIDE_INT high
        = (((val - low) & 0xffffffff) ^ 0x80000000) - 0x80000000;

      /* Check for 32-bit overflow.  */
      if (high + low != val)
        {
	  *win = 0;
	  return x;
	}

      /* Reload the high part into a base reg; leave the low part
         in the mem directly.  */

      x = gen_rtx_PLUS (GET_MODE (x),
                        gen_rtx_PLUS (GET_MODE (x), XEXP (x, 0),
                                      GEN_INT (high)),
                        GEN_INT (low));

      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
                   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
                   opnum, (enum reload_type)type);
      *win = 1;
      return x;
    }
#if TARGET_MACHO
  if (GET_CODE (x) == SYMBOL_REF
      && DEFAULT_ABI == ABI_DARWIN
      && !ALTIVEC_VECTOR_MODE (mode)
      && flag_pic)
    {
      /* Darwin load of floating point constant.  */
      rtx offset = gen_rtx (CONST, Pmode,
		    gen_rtx (MINUS, Pmode, x,
		    gen_rtx (SYMBOL_REF, Pmode,
			machopic_function_base_name ())));
      x = gen_rtx (LO_SUM, GET_MODE (x),
	    gen_rtx (PLUS, Pmode, pic_offset_table_rtx,
		gen_rtx (HIGH, Pmode, offset)), offset);
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		opnum, (enum reload_type)type);
      *win = 1;
      return x;
    }
#endif
  if (TARGET_TOC
      && CONSTANT_POOL_EXPR_P (x)
      && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (x), mode))
    {
      (x) = create_TOC_reference (x);
      *win = 1;
      return x;
    }
  *win = 0;
  return x;
}    

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On the RS/6000, there are four valid address: a SYMBOL_REF that
   refers to a constant pool entry of an address (or the sum of it
   plus a constant), a short (16-bit signed) constant plus a register,
   the sum of two registers, or a register indirect, possibly with an
   auto-increment.  For DFmode and DImode with an constant plus register,
   we must ensure that both words are addressable or PowerPC64 with offset
   word aligned.

   For modes spanning multiple registers (DFmode in 32-bit GPRs,
   32-bit DImode, TImode), indexed addressing cannot be used because
   adjacent memory cells are accessed by adding word-sized offsets
   during assembly output.  */
int
rs6000_legitimate_address (mode, x, reg_ok_strict)
    enum machine_mode mode;
    rtx x;
    int reg_ok_strict;
{
  if (LEGITIMATE_INDIRECT_ADDRESS_P (x, reg_ok_strict))
    return 1;
  if ((GET_CODE (x) == PRE_INC || GET_CODE (x) == PRE_DEC)
      && !ALTIVEC_VECTOR_MODE (mode)
      && TARGET_UPDATE
      && LEGITIMATE_INDIRECT_ADDRESS_P (XEXP (x, 0), reg_ok_strict))
    return 1;
  if (LEGITIMATE_SMALL_DATA_P (mode, x))
    return 1;
  if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (x))
    return 1;
  /* If not REG_OK_STRICT (before reload) let pass any stack offset.  */
  if (! reg_ok_strict
      && GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && XEXP (x, 0) == virtual_stack_vars_rtx
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return 1;
  if (LEGITIMATE_OFFSET_ADDRESS_P (mode, x, reg_ok_strict))
    return 1;
  if (mode != TImode
      && (TARGET_HARD_FLOAT || TARGET_POWERPC64 || mode != DFmode)
      && (TARGET_POWERPC64 || mode != DImode)
      && LEGITIMATE_INDEXED_ADDRESS_P (x, reg_ok_strict))
    return 1;
  if (LEGITIMATE_LO_SUM_ADDRESS_P (mode, x, reg_ok_strict))
    return 1;
  return 0;
}

/* Try to output insns to set TARGET equal to the constant C if it can
   be done in less than N insns.  Do all computations in MODE.
   Returns the place where the output has been placed if it can be
   done and the insns have been emitted.  If it would take more than N
   insns, zero is returned and no insns and emitted.  */

rtx
rs6000_emit_set_const (dest, mode, source, n)
     rtx dest, source;
     enum machine_mode mode;
     int n ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT c0, c1;

  if (mode == QImode || mode == HImode || mode == SImode)
    {
      if (dest == NULL)
        dest = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (VOIDmode, dest, source));
      return dest;
    }

  if (GET_CODE (source) == CONST_INT)
    {
      c0 = INTVAL (source);
      c1 = -(c0 < 0);
    }
  else if (GET_CODE (source) == CONST_DOUBLE)
    {
#if HOST_BITS_PER_WIDE_INT >= 64
      c0 = CONST_DOUBLE_LOW (source);
      c1 = -(c0 < 0);
#else
      c0 = CONST_DOUBLE_LOW (source);
      c1 = CONST_DOUBLE_HIGH (source);
#endif
    }
  else
    abort ();

  return rs6000_emit_set_long_const (dest, c0, c1);
}

/* Having failed to find a 3 insn sequence in rs6000_emit_set_const,
   fall back to a straight forward decomposition.  We do this to avoid
   exponential run times encountered when looking for longer sequences
   with rs6000_emit_set_const.  */
static rtx
rs6000_emit_set_long_const (dest, c1, c2)
     rtx dest;
     HOST_WIDE_INT c1, c2;
{
  if (!TARGET_POWERPC64)
    {
      rtx operand1, operand2;

      operand1 = operand_subword_force (dest, WORDS_BIG_ENDIAN == 0,
					DImode);
      operand2 = operand_subword_force (dest, WORDS_BIG_ENDIAN != 0,
					DImode);
      emit_move_insn (operand1, GEN_INT (c1));
      emit_move_insn (operand2, GEN_INT (c2));
    }
  else
    {
      HOST_WIDE_INT ud1, ud2, ud3, ud4;

      ud1 = c1 & 0xffff;
      ud2 = (c1 & 0xffff0000) >> 16;
#if HOST_BITS_PER_WIDE_INT >= 64
      c2 = c1 >> 32;
#endif
      ud3 = c2 & 0xffff;
      ud4 = (c2 & 0xffff0000) >> 16;

      if ((ud4 == 0xffff && ud3 == 0xffff && ud2 == 0xffff && (ud1 & 0x8000)) 
	  || (ud4 == 0 && ud3 == 0 && ud2 == 0 && ! (ud1 & 0x8000)))
	{
	  if (ud1 & 0x8000)
	    emit_move_insn (dest, GEN_INT (((ud1  ^ 0x8000) -  0x8000)));
	  else
	    emit_move_insn (dest, GEN_INT (ud1));
	}

      else if ((ud4 == 0xffff && ud3 == 0xffff && (ud2 & 0x8000)) 
	       || (ud4 == 0 && ud3 == 0 && ! (ud2 & 0x8000)))
	{
	  if (ud2 & 0x8000)
	    emit_move_insn (dest, GEN_INT (((ud2 << 16) ^ 0x80000000) 
					   - 0x80000000));
	  else
	    emit_move_insn (dest, GEN_INT (ud2 << 16));
	  if (ud1 != 0)
	    emit_move_insn (dest, gen_rtx_IOR (DImode, dest, GEN_INT (ud1)));
	}
      else if ((ud4 == 0xffff && (ud3 & 0x8000)) 
	       || (ud4 == 0 && ! (ud3 & 0x8000)))
	{
	  if (ud3 & 0x8000)
	    emit_move_insn (dest, GEN_INT (((ud3 << 16) ^ 0x80000000) 
					   - 0x80000000));
	  else
	    emit_move_insn (dest, GEN_INT (ud3 << 16));

	  if (ud2 != 0)
	    emit_move_insn (dest, gen_rtx_IOR (DImode, dest, GEN_INT (ud2)));
	  emit_move_insn (dest, gen_rtx_ASHIFT (DImode, dest, GEN_INT (16)));
	  if (ud1 != 0)
	    emit_move_insn (dest, gen_rtx_IOR (DImode, dest, GEN_INT (ud1)));
	}
      else 
	{
	  if (ud4 & 0x8000)
	    emit_move_insn (dest, GEN_INT (((ud4 << 16) ^ 0x80000000) 
					   - 0x80000000));
	  else
	    emit_move_insn (dest, GEN_INT (ud4 << 16));

	  if (ud3 != 0)
	    emit_move_insn (dest, gen_rtx_IOR (DImode, dest, GEN_INT (ud3)));

	  emit_move_insn (dest, gen_rtx_ASHIFT (DImode, dest, GEN_INT (32)));
	  if (ud2 != 0)
	    emit_move_insn (dest, gen_rtx_IOR (DImode, dest, 
					       GEN_INT (ud2 << 16)));	
	  if (ud1 != 0)
	    emit_move_insn (dest, gen_rtx_IOR (DImode, dest, GEN_INT (ud1)));
	}
    }
  return dest;
}

/* Emit a move from SOURCE to DEST in mode MODE.  */
void
rs6000_emit_move (dest, source, mode)
     rtx dest;
     rtx source;
     enum machine_mode mode;
{
  rtx operands[2];
  operands[0] = dest;
  operands[1] = source;
  
  /* Sanity checks.  Check that we get CONST_DOUBLE only when we should.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && ! FLOAT_MODE_P (mode)
      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      /* FIXME.  This should never happen.  */
      /* Since it seems that it does, do the safe thing and convert
	 to a CONST_INT.  */
      operands[1] = 
	GEN_INT (trunc_int_for_mode (CONST_DOUBLE_LOW (operands[1]), mode));
    }
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && ! FLOAT_MODE_P (mode)
      && ((CONST_DOUBLE_HIGH (operands[1]) == 0
	   && CONST_DOUBLE_LOW (operands[1]) >= 0)
	  || (CONST_DOUBLE_HIGH (operands[1]) == -1
	      && CONST_DOUBLE_LOW (operands[1]) < 0)))
    abort ();

  /* Check if GCC is setting up a block move that will end up using FP
     registers as temporaries.  We must make sure this is acceptable.  */
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (operands[1]) == MEM
      && mode == DImode
      && (SLOW_UNALIGNED_ACCESS (DImode, MEM_ALIGN (operands[0]))
	  || SLOW_UNALIGNED_ACCESS (DImode, MEM_ALIGN (operands[1])))
      && ! (SLOW_UNALIGNED_ACCESS (SImode, (MEM_ALIGN (operands[0]) > 32
					    ? 32 : MEM_ALIGN (operands[0])))
	    || SLOW_UNALIGNED_ACCESS (SImode, (MEM_ALIGN (operands[1]) > 32
					       ? 32 
					       : MEM_ALIGN (operands[1]))))
      && ! MEM_VOLATILE_P (operands [0])
      && ! MEM_VOLATILE_P (operands [1]))
    {
      emit_move_insn (adjust_address (operands[0], SImode, 0),
		      adjust_address (operands[1], SImode, 0));
      emit_move_insn (adjust_address (operands[0], SImode, 4),
		      adjust_address (operands[1], SImode, 4));
      return;
    }
  
  if (! no_new_pseudos && GET_CODE (operands[0]) != REG)
    operands[1] = force_reg (mode, operands[1]);

  if (mode == SFmode && ! TARGET_POWERPC && TARGET_HARD_FLOAT
      && GET_CODE (operands[0]) == MEM)
    {
      int regnum;

      if (reload_in_progress || reload_completed)
	regnum = true_regnum (operands[1]);
      else if (GET_CODE (operands[1]) == REG)
	regnum = REGNO (operands[1]);
      else
	regnum = -1;
      
      /* If operands[1] is a register, on POWER it may have
	 double-precision data in it, so truncate it to single
	 precision.  */
      if (FP_REGNO_P (regnum) || regnum >= FIRST_PSEUDO_REGISTER)
	{
	  rtx newreg;
	  newreg = (no_new_pseudos ? operands[1] : gen_reg_rtx (mode));
	  emit_insn (gen_aux_truncdfsf2 (newreg, operands[1]));
	  operands[1] = newreg;
	}
    }

  /* Handle the case where reload calls us with an invalid address;
     and the case of CONSTANT_P_RTX.  */
  if (!VECTOR_MODE_P (mode)
      && (! general_operand (operands[1], mode)
	  || ! nonimmediate_operand (operands[0], mode)
	  || GET_CODE (operands[1]) == CONSTANT_P_RTX))
    {
      emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
      return;
    }
  
  /* FIXME:  In the long term, this switch statement should go away
     and be replaced by a sequence of tests based on things like
     mode == Pmode.  */
  switch (mode)
    {
    case HImode:
    case QImode:
      if (CONSTANT_P (operands[1])
	  && GET_CODE (operands[1]) != CONST_INT)
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case TFmode:
    case DFmode:
    case SFmode:
      if (CONSTANT_P (operands[1]) 
	  && ! easy_fp_constant (operands[1], mode))
	operands[1] = force_const_mem (mode, operands[1]);
      break;
      
    case V16QImode:
    case V8HImode:
    case V4SFmode:
    case V4SImode:
      if (CONSTANT_P (operands[1])
	  && !easy_vector_constant (operands[1]))
	operands[1] = force_const_mem (mode, operands[1]);
      break;
      
    case SImode:
    case DImode:
      /* Use default pattern for address of ELF small data */
      if (TARGET_ELF
	  && mode == Pmode
	  && DEFAULT_ABI == ABI_V4
	  && (GET_CODE (operands[1]) == SYMBOL_REF 
	      || GET_CODE (operands[1]) == CONST)
	  && small_data_operand (operands[1], mode))
	{
	  emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
	  return;
	}

      if (DEFAULT_ABI == ABI_V4
	  && mode == Pmode && mode == SImode
	  && flag_pic == 1 && got_operand (operands[1], mode))
	{
	  emit_insn (gen_movsi_got (operands[0], operands[1]));
	  return;
	}

      if ((TARGET_ELF || DEFAULT_ABI == ABI_DARWIN)
	  && TARGET_NO_TOC && ! flag_pic
	  && mode == Pmode
	  && CONSTANT_P (operands[1])
	  && GET_CODE (operands[1]) != HIGH
	  && GET_CODE (operands[1]) != CONST_INT)
	{
	  rtx target = (no_new_pseudos ? operands[0] : gen_reg_rtx (mode));

	  /* If this is a function address on -mcall-aixdesc,
	     convert it to the address of the descriptor.  */
	  if (DEFAULT_ABI == ABI_AIX
	      && GET_CODE (operands[1]) == SYMBOL_REF
	      && XSTR (operands[1], 0)[0] == '.')
	    {
	      const char *name = XSTR (operands[1], 0);
	      rtx new_ref;
	      while (*name == '.')
		name++;
	      new_ref = gen_rtx_SYMBOL_REF (Pmode, name);
	      CONSTANT_POOL_ADDRESS_P (new_ref)
		= CONSTANT_POOL_ADDRESS_P (operands[1]);
	      SYMBOL_REF_FLAG (new_ref) = SYMBOL_REF_FLAG (operands[1]);
	      SYMBOL_REF_USED (new_ref) = SYMBOL_REF_USED (operands[1]);
	      operands[1] = new_ref;
	    }

	  if (DEFAULT_ABI == ABI_DARWIN)
	    {
	      emit_insn (gen_macho_high (target, operands[1]));
	      emit_insn (gen_macho_low (operands[0], target, operands[1]));
	      return;
	    }

	  emit_insn (gen_elf_high (target, operands[1]));
	  emit_insn (gen_elf_low (operands[0], target, operands[1]));
	  return;
	}

      /* If this is a SYMBOL_REF that refers to a constant pool entry,
	 and we have put it in the TOC, we just need to make a TOC-relative
	 reference to it.  */
      if (TARGET_TOC
	  && GET_CODE (operands[1]) == SYMBOL_REF
	  && CONSTANT_POOL_EXPR_P (operands[1])
	  && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (operands[1]),
					      get_pool_mode (operands[1])))
	{
	  operands[1] = create_TOC_reference (operands[1]);
	}
      else if (mode == Pmode
	       && CONSTANT_P (operands[1])
	       && ((GET_CODE (operands[1]) != CONST_INT
		    && ! easy_fp_constant (operands[1], mode))
		   || (GET_CODE (operands[1]) == CONST_INT
		       && num_insns_constant (operands[1], mode) > 2)
		   || (GET_CODE (operands[0]) == REG
		       && FP_REGNO_P (REGNO (operands[0]))))
	       && GET_CODE (operands[1]) != HIGH
	       && ! LEGITIMATE_CONSTANT_POOL_ADDRESS_P (operands[1])
	       && ! TOC_RELATIVE_EXPR_P (operands[1]))
	{
	  /* Emit a USE operation so that the constant isn't deleted if
	     expensive optimizations are turned on because nobody
	     references it.  This should only be done for operands that
	     contain SYMBOL_REFs with CONSTANT_POOL_ADDRESS_P set.
	     This should not be done for operands that contain LABEL_REFs.
	     For now, we just handle the obvious case.  */
	  if (GET_CODE (operands[1]) != LABEL_REF)
	    emit_insn (gen_rtx_USE (VOIDmode, operands[1]));

#if TARGET_MACHO
	  /* Darwin uses a special PIC legitimizer.  */
	  if (DEFAULT_ABI == ABI_DARWIN && flag_pic)
	    {
	      operands[1] =
		rs6000_machopic_legitimize_pic_address (operands[1], mode,
							operands[0]);
	      if (operands[0] != operands[1])
		emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
	      return;
	    }
#endif

	  /* If we are to limit the number of things we put in the TOC and
	     this is a symbol plus a constant we can add in one insn,
	     just put the symbol in the TOC and add the constant.  Don't do
	     this if reload is in progress.  */
	  if (GET_CODE (operands[1]) == CONST
	      && TARGET_NO_SUM_IN_TOC && ! reload_in_progress
	      && GET_CODE (XEXP (operands[1], 0)) == PLUS
	      && add_operand (XEXP (XEXP (operands[1], 0), 1), mode)
	      && (GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == LABEL_REF
		  || GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == SYMBOL_REF)
	      && ! side_effects_p (operands[0]))
	    {
	      rtx sym =
		force_const_mem (mode, XEXP (XEXP (operands[1], 0), 0));
	      rtx other = XEXP (XEXP (operands[1], 0), 1);

	      sym = force_reg (mode, sym);
	      if (mode == SImode)
		emit_insn (gen_addsi3 (operands[0], sym, other));
	      else
		emit_insn (gen_adddi3 (operands[0], sym, other));
	      return;
	    }

	  operands[1] = force_const_mem (mode, operands[1]);

	  if (TARGET_TOC 
	      && CONSTANT_POOL_EXPR_P (XEXP (operands[1], 0))
	      && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (
			get_pool_constant (XEXP (operands[1], 0)),
			get_pool_mode (XEXP (operands[1], 0))))
	    {
	      operands[1]
		= gen_rtx_MEM (mode,
			       create_TOC_reference (XEXP (operands[1], 0)));
	      set_mem_alias_set (operands[1], get_TOC_alias_set ());
	      RTX_UNCHANGING_P (operands[1]) = 1;
	    }
	}
      break;

    case TImode:
      if (GET_CODE (operands[0]) == MEM
	  && GET_CODE (XEXP (operands[0], 0)) != REG
	  && ! reload_in_progress)
	operands[0]
	  = replace_equiv_address (operands[0],
				   copy_addr_to_reg (XEXP (operands[0], 0)));

      if (GET_CODE (operands[1]) == MEM
	  && GET_CODE (XEXP (operands[1], 0)) != REG
	  && ! reload_in_progress)
	operands[1]
	  = replace_equiv_address (operands[1],
				   copy_addr_to_reg (XEXP (operands[1], 0)));
      break;

    default:
      abort ();
    }

  /* Above, we may have called force_const_mem which may have returned
     an invalid address.  If we can, fix this up; otherwise, reload will
     have to deal with it.  */
  if (GET_CODE (operands[1]) == MEM
      && ! memory_address_p (mode, XEXP (operands[1], 0))
      && ! reload_in_progress)
    operands[1] = adjust_address (operands[1], mode, 0);

  emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
  return;
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   For incoming args we set the number of arguments in the prototype large
   so we never return a PARALLEL.  */

void
init_cumulative_args (cum, fntype, libname, incoming)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname ATTRIBUTE_UNUSED;
     int incoming;
{
  static CUMULATIVE_ARGS zero_cumulative;

  *cum = zero_cumulative;
  cum->words = 0;
  cum->fregno = FP_ARG_MIN_REG;
  cum->vregno = ALTIVEC_ARG_MIN_REG;
  cum->prototype = (fntype && TYPE_ARG_TYPES (fntype));
  cum->call_cookie = CALL_NORMAL;
  cum->sysv_gregno = GP_ARG_MIN_REG;

  if (incoming)
    cum->nargs_prototype = 1000;		/* don't return a PARALLEL */

  else if (cum->prototype)
    cum->nargs_prototype = (list_length (TYPE_ARG_TYPES (fntype)) - 1
			    + (TYPE_MODE (TREE_TYPE (fntype)) == BLKmode
			       || RETURN_IN_MEMORY (TREE_TYPE (fntype))));

  else
    cum->nargs_prototype = 0;

  cum->orig_nargs = cum->nargs_prototype;

  /* Check for longcall's */
  if (fntype && lookup_attribute ("longcall", TYPE_ATTRIBUTES (fntype)))
    cum->call_cookie = CALL_LONG;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " ret code = %s,",
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}

      if (cum->call_cookie & CALL_LONG)
	fprintf (stderr, " longcall,");

      fprintf (stderr, " proto = %d, nargs = %d\n",
	       cum->prototype, cum->nargs_prototype);
    }
}

/* If defined, a C expression which determines whether, and in which
   direction, to pad out an argument with extra space.  The value
   should be of type `enum direction': either `upward' to pad above
   the argument, `downward' to pad below, or `none' to inhibit
   padding.

   For the AIX ABI structs are always stored left shifted in their
   argument slot.  */

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  if (type != 0 && AGGREGATE_TYPE_P (type))
    return upward;

  /* This is the default definition.  */
  return (! BYTES_BIG_ENDIAN
          ? upward
          : ((mode == BLKmode
              ? (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
                 && int_size_in_bytes (type) < (PARM_BOUNDARY / BITS_PER_UNIT))
              : GET_MODE_BITSIZE (mode) < PARM_BOUNDARY)
             ? downward : upward));
}

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined, 
   PARM_BOUNDARY is used for all arguments.
   
   V.4 wants long longs to be double word aligned.  */

int
function_arg_boundary (mode, type)
     enum machine_mode mode;
     tree type ATTRIBUTE_UNUSED;
{
  if (DEFAULT_ABI == ABI_V4 && (mode == DImode || mode == DFmode))
    return 64;
  else if (TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
    return 128;
  else
    return PARM_BOUNDARY;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  cum->nargs_prototype--;

  if (TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
    {
      if (cum->vregno <= ALTIVEC_ARG_MAX_REG && cum->nargs_prototype >= 0)
	cum->vregno++;
      else
	cum->words += RS6000_ARG_SIZE (mode, type);
    }
  else if (DEFAULT_ABI == ABI_V4)
    {
      if (TARGET_HARD_FLOAT
	  && (mode == SFmode || mode == DFmode))
	{
	  if (cum->fregno <= FP_ARG_V4_MAX_REG)
	    cum->fregno++;
	  else
	    {
	      if (mode == DFmode)
	        cum->words += cum->words & 1;
	      cum->words += RS6000_ARG_SIZE (mode, type);
	    }
	}
      else
	{
	  int n_words;
	  int gregno = cum->sysv_gregno;

	  /* Aggregates and IEEE quad get passed by reference.  */
	  if ((type && AGGREGATE_TYPE_P (type))
	      || mode == TFmode)
	    n_words = 1;
	  else 
	    n_words = RS6000_ARG_SIZE (mode, type);

	  /* Long long is put in odd registers.  */
	  if (n_words == 2 && (gregno & 1) == 0)
	    gregno += 1;

	  /* Long long is not split between registers and stack.  */
	  if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	    {
	      /* Long long is aligned on the stack.  */
	      if (n_words == 2)
		cum->words += cum->words & 1;
	      cum->words += n_words;
	    }

	  /* Note: continuing to accumulate gregno past when we've started
	     spilling to the stack indicates the fact that we've started
	     spilling to the stack to expand_builtin_saveregs.  */
	  cum->sysv_gregno = gregno + n_words;
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "gregno = %2d, nargs = %4d, proto = %d, ",
		   cum->sysv_gregno, cum->nargs_prototype, cum->prototype);
	  fprintf (stderr, "mode = %4s, named = %d\n",
		   GET_MODE_NAME (mode), named);
	}
    }
  else
    {
      int align = (TARGET_32BIT && (cum->words & 1) != 0
		   && function_arg_boundary (mode, type) == 64) ? 1 : 0;

      cum->words += align + RS6000_ARG_SIZE (mode, type);

      if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_HARD_FLOAT)
	cum->fregno++;

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "nargs = %4d, proto = %d, mode = %4s, ",
		   cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode));
	  fprintf (stderr, "named = %d, align = %d\n", named, align);
	}
    }
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On RS/6000 the first eight words of non-FP are normally in registers
   and the rest are pushed.  Under AIX, the first 13 FP args are in registers.
   Under V.4, the first 8 FP args are in registers.

   If this is floating-point and no prototype is specified, we use
   both an FP and integer register (or possibly FP reg and stack).  Library
   functions (when TYPE is zero) always have the proper types for args,
   so we can pass the FP value just in one register.  emit_library_function
   doesn't support PARALLEL anyway.  */

struct rtx_def *
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  enum rs6000_abi abi = DEFAULT_ABI;

  /* Return a marker to indicate whether CR1 needs to set or clear the
     bit that V.4 uses to say fp args were passed in registers.
     Assume that we don't need the marker for software floating point,
     or compiler generated library calls.  */
  if (mode == VOIDmode)
    {
      if (abi == ABI_V4
	  && TARGET_HARD_FLOAT
	  && cum->nargs_prototype < 0
	  && type && (cum->prototype || TARGET_NO_PROTOTYPE))
	{
	  return GEN_INT (cum->call_cookie
			  | ((cum->fregno == FP_ARG_MIN_REG)
			     ? CALL_V4_SET_FP_ARGS
			     : CALL_V4_CLEAR_FP_ARGS));
	}

      return GEN_INT (cum->call_cookie);
    }

  if (TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
    {
      if (named && cum->vregno <= ALTIVEC_ARG_MAX_REG)
	return gen_rtx_REG (mode, cum->vregno);
      else
	return NULL;
    }
  else if (abi == ABI_V4)
    {
      if (TARGET_HARD_FLOAT
	  && (mode == SFmode || mode == DFmode))
	{
	  if (cum->fregno <= FP_ARG_V4_MAX_REG)
	    return gen_rtx_REG (mode, cum->fregno);
	  else
	    return NULL;
	}
      else
	{
	  int n_words;
	  int gregno = cum->sysv_gregno;

	  /* Aggregates and IEEE quad get passed by reference.  */
	  if ((type && AGGREGATE_TYPE_P (type))
	      || mode == TFmode)
	    n_words = 1;
	  else 
	    n_words = RS6000_ARG_SIZE (mode, type);

	  /* Long long is put in odd registers.  */
	  if (n_words == 2 && (gregno & 1) == 0)
	    gregno += 1;

	  /* Long long is not split between registers and stack.  */
	  if (gregno + n_words - 1 <= GP_ARG_MAX_REG)
	    return gen_rtx_REG (mode, gregno);
	  else
	    return NULL;
	}
    }
  else
    {
      int align = (TARGET_32BIT && (cum->words & 1) != 0
	           && function_arg_boundary (mode, type) == 64) ? 1 : 0;
      int align_words = cum->words + align;

      if (type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
        return NULL_RTX;

      if (USE_FP_FOR_ARG_P (*cum, mode, type))
	{
	  if (! type
	      || ((cum->nargs_prototype > 0)
	          /* IBM AIX extended its linkage convention definition always
		     to require FP args after register save area hole on the
		     stack.  */
	          && (DEFAULT_ABI != ABI_AIX
		      || ! TARGET_XL_CALL
		      || (align_words < GP_ARG_NUM_REG))))
	    return gen_rtx_REG (mode, cum->fregno);

          return gen_rtx_PARALLEL (mode,
	    gen_rtvec (2,
		       gen_rtx_EXPR_LIST (VOIDmode,
				((align_words >= GP_ARG_NUM_REG)
				 ? NULL_RTX
				 : (align_words
				    + RS6000_ARG_SIZE (mode, type)
				    > GP_ARG_NUM_REG
				    /* If this is partially on the stack, then
				       we only include the portion actually
				       in registers here.  */
				    ? gen_rtx_REG (SImode,
					       GP_ARG_MIN_REG + align_words)
				    : gen_rtx_REG (mode,
					       GP_ARG_MIN_REG + align_words))),
				const0_rtx),
		       gen_rtx_EXPR_LIST (VOIDmode,
				gen_rtx_REG (mode, cum->fregno),
				const0_rtx)));
	}
      else if (align_words < GP_ARG_NUM_REG)
	return gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);
      else
	return NULL_RTX;
    }
}

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if (DEFAULT_ABI == ABI_V4)
    return 0;

  if (USE_FP_FOR_ARG_P (*cum, mode, type)
      || USE_ALTIVEC_FOR_ARG_P (*cum, mode, type))
    {
      if (cum->nargs_prototype >= 0)
	return 0;
    }

  if (cum->words < GP_ARG_NUM_REG
      && GP_ARG_NUM_REG < (cum->words + RS6000_ARG_SIZE (mode, type)))
    {
      int ret = GP_ARG_NUM_REG - cum->words;
      if (ret && TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_partial_nregs: %d\n", ret);

      return ret;
    }

  return 0;
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.

   Under V.4, structures and unions are passed by reference.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if (DEFAULT_ABI == ABI_V4
      && ((type && AGGREGATE_TYPE_P (type))
	  || mode == TFmode))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: aggregate\n");

      return 1;
    }

  return 0;
}

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

void
setup_incoming_varargs (cum, mode, type, pretend_size, no_rtl)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int *pretend_size;
     int no_rtl;

{
  CUMULATIVE_ARGS next_cum;
  int reg_size = TARGET_32BIT ? 4 : 8;
  rtx save_area = NULL_RTX, mem;
  int first_reg_offset, set;
  tree fntype;
  int stdarg_p;

  fntype = TREE_TYPE (current_function_decl);
  stdarg_p = (TYPE_ARG_TYPES (fntype) != 0
	      && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		  != void_type_node));

  /* For varargs, we do not want to skip the dummy va_dcl argument.
     For stdargs, we do want to skip the last named argument.  */
  next_cum = *cum;
  if (stdarg_p)
    function_arg_advance (&next_cum, mode, type, 1);

  if (DEFAULT_ABI == ABI_V4)
    {
      /* Indicate to allocate space on the stack for varargs save area.  */
      /* ??? Does this really have to be located at a magic spot on the
	 stack, or can we allocate this with assign_stack_local instead.  */
      cfun->machine->sysv_varargs_p = 1;
      if (! no_rtl)
	save_area = plus_constant (virtual_stack_vars_rtx,
				   - RS6000_VARARGS_SIZE);

      first_reg_offset = next_cum.sysv_gregno - GP_ARG_MIN_REG;
    }
  else
    {
      first_reg_offset = next_cum.words;
      save_area = virtual_incoming_args_rtx;
      cfun->machine->sysv_varargs_p = 0;

      if (MUST_PASS_IN_STACK (mode, type))
	first_reg_offset += RS6000_ARG_SIZE (TYPE_MODE (type), type);
    }

  set = get_varargs_alias_set ();
  if (! no_rtl && first_reg_offset < GP_ARG_NUM_REG)
    {
      mem = gen_rtx_MEM (BLKmode,
		         plus_constant (save_area,
					first_reg_offset * reg_size)),
      set_mem_alias_set (mem, set);
      set_mem_align (mem, BITS_PER_WORD);

      move_block_from_reg
	(GP_ARG_MIN_REG + first_reg_offset, mem,
	 GP_ARG_NUM_REG - first_reg_offset,
	 (GP_ARG_NUM_REG - first_reg_offset) * UNITS_PER_WORD);

      /* ??? Does ABI_V4 need this at all?  */
      *pretend_size = (GP_ARG_NUM_REG - first_reg_offset) * UNITS_PER_WORD;
    }

  /* Save FP registers if needed.  */
  if (DEFAULT_ABI == ABI_V4
      && TARGET_HARD_FLOAT && ! no_rtl
      && next_cum.fregno <= FP_ARG_V4_MAX_REG)
    {
      int fregno = next_cum.fregno;
      rtx cr1 = gen_rtx_REG (CCmode, CR1_REGNO);
      rtx lab = gen_label_rtx ();
      int off = (GP_ARG_NUM_REG * reg_size) + ((fregno - FP_ARG_MIN_REG) * 8);

      emit_jump_insn (gen_rtx_SET (VOIDmode,
				   pc_rtx,
				   gen_rtx_IF_THEN_ELSE (VOIDmode,
					    gen_rtx_NE (VOIDmode, cr1,
						        const0_rtx),
					    gen_rtx_LABEL_REF (VOIDmode, lab),
					    pc_rtx)));

      while (fregno <= FP_ARG_V4_MAX_REG)
	{
	  mem = gen_rtx_MEM (DFmode, plus_constant (save_area, off));
          set_mem_alias_set (mem, set);
	  emit_move_insn (mem, gen_rtx_REG (DFmode, fregno));
	  fregno++;
	  off += 8;
	}

      emit_label (lab);
    }
}

/* Create the va_list data type.  */

tree
rs6000_build_va_list ()
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  /* For AIX, prefer 'char *' because that's what the system
     header files like.  */
  if (DEFAULT_ABI != ABI_V4)
    return build_pointer_type (char_type_node);

  record = make_lang_type (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (FIELD_DECL, get_identifier ("gpr"), 
		      unsigned_char_type_node);
  f_fpr = build_decl (FIELD_DECL, get_identifier ("fpr"), 
		      unsigned_char_type_node);
  f_ovf = build_decl (FIELD_DECL, get_identifier ("overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (FIELD_DECL, get_identifier ("reg_save_area"),
		      ptr_type_node);

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  TREE_CHAIN (f_gpr) = f_fpr;
  TREE_CHAIN (f_fpr) = f_ovf;
  TREE_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start.  */

void
rs6000_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg;
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  /* Only SVR4 needs something special.  */
  if (DEFAULT_ABI != ABI_V4)
    {
      std_expand_builtin_va_start (stdarg_p, valist, nextarg);
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  /* Count number of gp and fp argument registers used.  */
  words = current_function_args_info.words;
  n_gpr = current_function_args_info.sysv_gregno - GP_ARG_MIN_REG;
  n_fpr = current_function_args_info.fregno - FP_ARG_MIN_REG;

  if (TARGET_DEBUG_ARG)
    {
      fputs ("va_start: words = ", stderr);
      fprintf (stderr, HOST_WIDE_INT_PRINT_DEC, words);
      fputs (", n_gpr = ", stderr);
      fprintf (stderr, HOST_WIDE_INT_PRINT_DEC, n_gpr);
      fputs (", n_fpr = ", stderr);
      fprintf (stderr, HOST_WIDE_INT_PRINT_DEC, n_fpr);
      putc ('\n', stderr);
    }

  t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr, build_int_2 (n_gpr, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build (MODIFY_EXPR, TREE_TYPE (fpr), fpr, build_int_2 (n_fpr, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the overflow area.  */
  t = make_tree (TREE_TYPE (ovf), virtual_incoming_args_rtx);
  if (words != 0)
    t = build (PLUS_EXPR, TREE_TYPE (ovf), t,
	       build_int_2 (words * UNITS_PER_WORD, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the register save area.  */
  t = make_tree (TREE_TYPE (sav), virtual_stack_vars_rtx);
  t = build (PLUS_EXPR, TREE_TYPE (sav), t,
	     build_int_2 (-RS6000_VARARGS_SIZE, -1));
  t = build (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement va_arg.  */

rtx
rs6000_va_arg (valist, type)
     tree valist, type;
{
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, reg, t, u;
  int indirect_p, size, rsize, n_reg, sav_ofs, sav_scale;
  rtx lab_false, lab_over, addr_rtx, r;

  /* For AIX, the rule is that structures are passed left-aligned in
     their stack slot.  However, GCC does not presently do this:
     structures which are the same size as integer types are passed
     right-aligned, as if they were in fact integers.  This only
     matters for structures of size 1 or 2, or 4 when TARGET_64BIT.  */
  if (DEFAULT_ABI != ABI_V4)
    return std_expand_builtin_va_arg (valist, type);

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (AGGREGATE_TYPE_P (type) || TYPE_MODE (type) == TFmode)
    {
      /* Aggregates and long doubles are passed by reference.  */
      indirect_p = 1;
      reg = gpr;
      n_reg = 1;
      sav_ofs = 0;
      sav_scale = 4;
      size = UNITS_PER_WORD;
      rsize = 1;
    }
  else if (FLOAT_TYPE_P (type) && ! TARGET_SOFT_FLOAT)
    {
      /* FP args go in FP registers, if present.  */
      indirect_p = 0;
      reg = fpr;
      n_reg = 1;
      sav_ofs = 8*4;
      sav_scale = 8;
    }
  else
    {
      /* Otherwise into GP registers.  */
      indirect_p = 0;
      reg = gpr;
      n_reg = rsize;
      sav_ofs = 0;
      sav_scale = 4;
    }

  /* Pull the value out of the saved registers ...  */

  lab_false = gen_label_rtx ();
  lab_over = gen_label_rtx ();
  addr_rtx = gen_reg_rtx (Pmode);

  /*  Vectors never go in registers.  */
  if (TREE_CODE (type) != VECTOR_TYPE)
    {
      TREE_THIS_VOLATILE (reg) = 1;
      emit_cmp_and_jump_insns
	(expand_expr (reg, NULL_RTX, QImode, EXPAND_NORMAL),
	 GEN_INT (8 - n_reg + 1), GE, const1_rtx, QImode, 1,
	 lab_false);

      /* Long long is aligned in the registers.  */
      if (n_reg > 1)
	{
	  u = build (BIT_AND_EXPR, TREE_TYPE (reg), reg,
		     build_int_2 (n_reg - 1, 0));
	  u = build (PLUS_EXPR, TREE_TYPE (reg), reg, u);
	  u = build (MODIFY_EXPR, TREE_TYPE (reg), reg, u);
	  TREE_SIDE_EFFECTS (u) = 1;
	  expand_expr (u, const0_rtx, VOIDmode, EXPAND_NORMAL);
	}

      if (sav_ofs)
	t = build (PLUS_EXPR, ptr_type_node, sav, build_int_2 (sav_ofs, 0));
      else
	t = sav;

      u = build (POSTINCREMENT_EXPR, TREE_TYPE (reg), reg,
		 build_int_2 (n_reg, 0));
      TREE_SIDE_EFFECTS (u) = 1;

      u = build1 (CONVERT_EXPR, integer_type_node, u);
      TREE_SIDE_EFFECTS (u) = 1;

      u = build (MULT_EXPR, integer_type_node, u, build_int_2 (sav_scale, 0));
      TREE_SIDE_EFFECTS (u) = 1;

      t = build (PLUS_EXPR, ptr_type_node, t, u);
      TREE_SIDE_EFFECTS (t) = 1;

      r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
      if (r != addr_rtx)
	emit_move_insn (addr_rtx, r);

      emit_jump_insn (gen_jump (lab_over));
      emit_barrier ();
    }

  emit_label (lab_false);

  /* ... otherwise out of the overflow area.  */

  /* Make sure we don't find reg 7 for the next int arg.

     All AltiVec vectors go in the overflow area.  So in the AltiVec
     case we need to get the vectors from the overflow area, but
     remember where the GPRs and FPRs are.  */
  if (n_reg > 1 && TREE_CODE (type) != VECTOR_TYPE)
    {
      t = build (MODIFY_EXPR, TREE_TYPE (reg), reg, build_int_2 (8, 0));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  /* Care for on-stack alignment if needed.  */
  if (rsize <= 1)
    t = ovf;
  else
    {
      int align;

      /* Vectors are 16 byte aligned.  */
      if (TREE_CODE (type) == VECTOR_TYPE)
	align = 15;
      else
	align = 7;

      t = build (PLUS_EXPR, TREE_TYPE (ovf), ovf, build_int_2 (align, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t, build_int_2 (-align-1, -1));
    }
  t = save_expr (t);

  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);

  t = build (PLUS_EXPR, TREE_TYPE (t), t, build_int_2 (size, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  emit_label (lab_over);

  if (indirect_p)
    {
      r = gen_rtx_MEM (Pmode, addr_rtx);
      set_mem_alias_set (r, get_varargs_alias_set ());
      emit_move_insn (addr_rtx, r);
    }

  return addr_rtx;
}

/* Builtins.  */

#define def_builtin(MASK, NAME, TYPE, CODE)				\
do {									\
  if ((MASK) & target_flags)						\
    builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD, NULL);	\
} while (0)

struct builtin_description
{
  const unsigned int mask;
  const enum insn_code icode;
  const char *const name;
  const enum rs6000_builtins code;
};

/* Simple ternary operations: VECd = foo (VECa, VECb, VECc).  */

static const struct builtin_description bdesc_3arg[] =
{
  { MASK_ALTIVEC, CODE_FOR_altivec_vmaddfp, "__builtin_altivec_vmaddfp", ALTIVEC_BUILTIN_VMADDFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmhaddshs, "__builtin_altivec_vmhaddshs", ALTIVEC_BUILTIN_VMHADDSHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmhraddshs, "__builtin_altivec_vmhraddshs", ALTIVEC_BUILTIN_VMHRADDSHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmladduhm, "__builtin_altivec_vmladduhm", ALTIVEC_BUILTIN_VMLADDUHM},
  { MASK_ALTIVEC, CODE_FOR_altivec_vmsumubm, "__builtin_altivec_vmsumubm", ALTIVEC_BUILTIN_VMSUMUBM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmsummbm, "__builtin_altivec_vmsummbm", ALTIVEC_BUILTIN_VMSUMMBM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmsumuhm, "__builtin_altivec_vmsumuhm", ALTIVEC_BUILTIN_VMSUMUHM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmsumshm, "__builtin_altivec_vmsumshm", ALTIVEC_BUILTIN_VMSUMSHM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmsumuhs, "__builtin_altivec_vmsumuhs", ALTIVEC_BUILTIN_VMSUMUHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmsumshs, "__builtin_altivec_vmsumshs", ALTIVEC_BUILTIN_VMSUMSHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vnmsubfp, "__builtin_altivec_vnmsubfp", ALTIVEC_BUILTIN_VNMSUBFP }, 
  { MASK_ALTIVEC, CODE_FOR_altivec_vperm_4sf, "__builtin_altivec_vperm_4sf", ALTIVEC_BUILTIN_VPERM_4SF },
  { MASK_ALTIVEC, CODE_FOR_altivec_vperm_4si, "__builtin_altivec_vperm_4si", ALTIVEC_BUILTIN_VPERM_4SI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vperm_8hi, "__builtin_altivec_vperm_8hi", ALTIVEC_BUILTIN_VPERM_8HI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vperm_16qi, "__builtin_altivec_vperm_16qi", ALTIVEC_BUILTIN_VPERM_16QI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsel_4sf, "__builtin_altivec_vsel_4sf", ALTIVEC_BUILTIN_VSEL_4SF },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsel_4si, "__builtin_altivec_vsel_4si", ALTIVEC_BUILTIN_VSEL_4SI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsel_8hi, "__builtin_altivec_vsel_8hi", ALTIVEC_BUILTIN_VSEL_8HI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsel_16qi, "__builtin_altivec_vsel_16qi", ALTIVEC_BUILTIN_VSEL_16QI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsldoi_16qi, "__builtin_altivec_vsldoi_16qi", ALTIVEC_BUILTIN_VSLDOI_16QI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsldoi_8hi, "__builtin_altivec_vsldoi_8hi", ALTIVEC_BUILTIN_VSLDOI_8HI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsldoi_4si, "__builtin_altivec_vsldoi_4si", ALTIVEC_BUILTIN_VSLDOI_4SI },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsldoi_4sf, "__builtin_altivec_vsldoi_4sf", ALTIVEC_BUILTIN_VSLDOI_4SF },
};

/* DST operations: void foo (void *, const int, const char).  */

static const struct builtin_description bdesc_dst[] =
{
  { MASK_ALTIVEC, CODE_FOR_altivec_dst, "__builtin_altivec_dst", ALTIVEC_BUILTIN_DST },
  { MASK_ALTIVEC, CODE_FOR_altivec_dstt, "__builtin_altivec_dstt", ALTIVEC_BUILTIN_DSTT },
  { MASK_ALTIVEC, CODE_FOR_altivec_dstst, "__builtin_altivec_dstst", ALTIVEC_BUILTIN_DSTST },
  { MASK_ALTIVEC, CODE_FOR_altivec_dststt, "__builtin_altivec_dststt", ALTIVEC_BUILTIN_DSTSTT }
};

/* Simple binary operations: VECc = foo (VECa, VECb).  */

static const struct builtin_description bdesc_2arg[] =
{
  { MASK_ALTIVEC, CODE_FOR_addv16qi3, "__builtin_altivec_vaddubm", ALTIVEC_BUILTIN_VADDUBM },
  { MASK_ALTIVEC, CODE_FOR_addv8hi3, "__builtin_altivec_vadduhm", ALTIVEC_BUILTIN_VADDUHM },
  { MASK_ALTIVEC, CODE_FOR_addv4si3, "__builtin_altivec_vadduwm", ALTIVEC_BUILTIN_VADDUWM },
  { MASK_ALTIVEC, CODE_FOR_addv4sf3, "__builtin_altivec_vaddfp", ALTIVEC_BUILTIN_VADDFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vaddcuw, "__builtin_altivec_vaddcuw", ALTIVEC_BUILTIN_VADDCUW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vaddubs, "__builtin_altivec_vaddubs", ALTIVEC_BUILTIN_VADDUBS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vaddsbs, "__builtin_altivec_vaddsbs", ALTIVEC_BUILTIN_VADDSBS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vadduhs, "__builtin_altivec_vadduhs", ALTIVEC_BUILTIN_VADDUHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vaddshs, "__builtin_altivec_vaddshs", ALTIVEC_BUILTIN_VADDSHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vadduws, "__builtin_altivec_vadduws", ALTIVEC_BUILTIN_VADDUWS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vaddsws, "__builtin_altivec_vaddsws", ALTIVEC_BUILTIN_VADDSWS },
  { MASK_ALTIVEC, CODE_FOR_andv4si3, "__builtin_altivec_vand", ALTIVEC_BUILTIN_VAND },
  { MASK_ALTIVEC, CODE_FOR_altivec_vandc, "__builtin_altivec_vandc", ALTIVEC_BUILTIN_VANDC },
  { MASK_ALTIVEC, CODE_FOR_altivec_vavgub, "__builtin_altivec_vavgub", ALTIVEC_BUILTIN_VAVGUB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vavgsb, "__builtin_altivec_vavgsb", ALTIVEC_BUILTIN_VAVGSB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vavguh, "__builtin_altivec_vavguh", ALTIVEC_BUILTIN_VAVGUH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vavgsh, "__builtin_altivec_vavgsh", ALTIVEC_BUILTIN_VAVGSH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vavguw, "__builtin_altivec_vavguw", ALTIVEC_BUILTIN_VAVGUW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vavgsw, "__builtin_altivec_vavgsw", ALTIVEC_BUILTIN_VAVGSW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcfux, "__builtin_altivec_vcfux", ALTIVEC_BUILTIN_VCFUX },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcfsx, "__builtin_altivec_vcfsx", ALTIVEC_BUILTIN_VCFSX },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpbfp, "__builtin_altivec_vcmpbfp", ALTIVEC_BUILTIN_VCMPBFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpequb, "__builtin_altivec_vcmpequb", ALTIVEC_BUILTIN_VCMPEQUB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpequh, "__builtin_altivec_vcmpequh", ALTIVEC_BUILTIN_VCMPEQUH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpequw, "__builtin_altivec_vcmpequw", ALTIVEC_BUILTIN_VCMPEQUW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpeqfp, "__builtin_altivec_vcmpeqfp", ALTIVEC_BUILTIN_VCMPEQFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgefp, "__builtin_altivec_vcmpgefp", ALTIVEC_BUILTIN_VCMPGEFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtub, "__builtin_altivec_vcmpgtub", ALTIVEC_BUILTIN_VCMPGTUB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtsb, "__builtin_altivec_vcmpgtsb", ALTIVEC_BUILTIN_VCMPGTSB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtuh, "__builtin_altivec_vcmpgtuh", ALTIVEC_BUILTIN_VCMPGTUH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtsh, "__builtin_altivec_vcmpgtsh", ALTIVEC_BUILTIN_VCMPGTSH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtuw, "__builtin_altivec_vcmpgtuw", ALTIVEC_BUILTIN_VCMPGTUW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtsw, "__builtin_altivec_vcmpgtsw", ALTIVEC_BUILTIN_VCMPGTSW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vcmpgtfp, "__builtin_altivec_vcmpgtfp", ALTIVEC_BUILTIN_VCMPGTFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vctsxs, "__builtin_altivec_vctsxs", ALTIVEC_BUILTIN_VCTSXS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vctuxs, "__builtin_altivec_vctuxs", ALTIVEC_BUILTIN_VCTUXS },
  { MASK_ALTIVEC, CODE_FOR_umaxv16qi3, "__builtin_altivec_vmaxub", ALTIVEC_BUILTIN_VMAXUB },
  { MASK_ALTIVEC, CODE_FOR_smaxv16qi3, "__builtin_altivec_vmaxsb", ALTIVEC_BUILTIN_VMAXSB },
  { MASK_ALTIVEC, CODE_FOR_uminv8hi3, "__builtin_altivec_vmaxuh", ALTIVEC_BUILTIN_VMAXUH },
  { MASK_ALTIVEC, CODE_FOR_sminv8hi3, "__builtin_altivec_vmaxsh", ALTIVEC_BUILTIN_VMAXSH },
  { MASK_ALTIVEC, CODE_FOR_uminv4si3, "__builtin_altivec_vmaxuw", ALTIVEC_BUILTIN_VMAXUW },
  { MASK_ALTIVEC, CODE_FOR_sminv4si3, "__builtin_altivec_vmaxsw", ALTIVEC_BUILTIN_VMAXSW },
  { MASK_ALTIVEC, CODE_FOR_sminv4sf3, "__builtin_altivec_vmaxfp", ALTIVEC_BUILTIN_VMAXFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmrghb, "__builtin_altivec_vmrghb", ALTIVEC_BUILTIN_VMRGHB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmrghh, "__builtin_altivec_vmrghh", ALTIVEC_BUILTIN_VMRGHH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmrghw, "__builtin_altivec_vmrghw", ALTIVEC_BUILTIN_VMRGHW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmrglb, "__builtin_altivec_vmrglb", ALTIVEC_BUILTIN_VMRGLB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmrglh, "__builtin_altivec_vmrglh", ALTIVEC_BUILTIN_VMRGLH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmrglw, "__builtin_altivec_vmrglw", ALTIVEC_BUILTIN_VMRGLW },
  { MASK_ALTIVEC, CODE_FOR_uminv16qi3, "__builtin_altivec_vminub", ALTIVEC_BUILTIN_VMINUB },
  { MASK_ALTIVEC, CODE_FOR_sminv16qi3, "__builtin_altivec_vminsb", ALTIVEC_BUILTIN_VMINSB },
  { MASK_ALTIVEC, CODE_FOR_uminv8hi3, "__builtin_altivec_vminuh", ALTIVEC_BUILTIN_VMINUH },
  { MASK_ALTIVEC, CODE_FOR_sminv8hi3, "__builtin_altivec_vminsh", ALTIVEC_BUILTIN_VMINSH },
  { MASK_ALTIVEC, CODE_FOR_uminv4si3, "__builtin_altivec_vminuw", ALTIVEC_BUILTIN_VMINUW },
  { MASK_ALTIVEC, CODE_FOR_sminv4si3, "__builtin_altivec_vminsw", ALTIVEC_BUILTIN_VMINSW },
  { MASK_ALTIVEC, CODE_FOR_sminv4sf3, "__builtin_altivec_vminfp", ALTIVEC_BUILTIN_VMINFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmuleub, "__builtin_altivec_vmuleub", ALTIVEC_BUILTIN_VMULEUB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmulesb, "__builtin_altivec_vmulesb", ALTIVEC_BUILTIN_VMULESB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmuleuh, "__builtin_altivec_vmuleuh", ALTIVEC_BUILTIN_VMULEUH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmulesh, "__builtin_altivec_vmulesh", ALTIVEC_BUILTIN_VMULESH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmuloub, "__builtin_altivec_vmuloub", ALTIVEC_BUILTIN_VMULOUB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmulosb, "__builtin_altivec_vmulosb", ALTIVEC_BUILTIN_VMULOSB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmulouh, "__builtin_altivec_vmulouh", ALTIVEC_BUILTIN_VMULOUH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vmulosh, "__builtin_altivec_vmulosh", ALTIVEC_BUILTIN_VMULOSH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vnor, "__builtin_altivec_vnor", ALTIVEC_BUILTIN_VNOR },
  { MASK_ALTIVEC, CODE_FOR_iorv4si3, "__builtin_altivec_vor", ALTIVEC_BUILTIN_VOR },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkuhum, "__builtin_altivec_vpkuhum", ALTIVEC_BUILTIN_VPKUHUM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkuwum, "__builtin_altivec_vpkuwum", ALTIVEC_BUILTIN_VPKUWUM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkpx, "__builtin_altivec_vpkpx", ALTIVEC_BUILTIN_VPKPX },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkuhss, "__builtin_altivec_vpkuhss", ALTIVEC_BUILTIN_VPKUHSS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkshss, "__builtin_altivec_vpkshss", ALTIVEC_BUILTIN_VPKSHSS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkuwss, "__builtin_altivec_vpkuwss", ALTIVEC_BUILTIN_VPKUWSS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkswss, "__builtin_altivec_vpkswss", ALTIVEC_BUILTIN_VPKSWSS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkuhus, "__builtin_altivec_vpkuhus", ALTIVEC_BUILTIN_VPKUHUS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkshus, "__builtin_altivec_vpkshus", ALTIVEC_BUILTIN_VPKSHUS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkuwus, "__builtin_altivec_vpkuwus", ALTIVEC_BUILTIN_VPKUWUS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vpkswus, "__builtin_altivec_vpkswus", ALTIVEC_BUILTIN_VPKSWUS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrlb, "__builtin_altivec_vrlb", ALTIVEC_BUILTIN_VRLB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrlh, "__builtin_altivec_vrlh", ALTIVEC_BUILTIN_VRLH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrlw, "__builtin_altivec_vrlw", ALTIVEC_BUILTIN_VRLW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vslb, "__builtin_altivec_vslb", ALTIVEC_BUILTIN_VSLB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vslh, "__builtin_altivec_vslh", ALTIVEC_BUILTIN_VSLH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vslw, "__builtin_altivec_vslw", ALTIVEC_BUILTIN_VSLW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsl, "__builtin_altivec_vsl", ALTIVEC_BUILTIN_VSL },
  { MASK_ALTIVEC, CODE_FOR_altivec_vslo, "__builtin_altivec_vslo", ALTIVEC_BUILTIN_VSLO },
  { MASK_ALTIVEC, CODE_FOR_altivec_vspltb, "__builtin_altivec_vspltb", ALTIVEC_BUILTIN_VSPLTB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsplth, "__builtin_altivec_vsplth", ALTIVEC_BUILTIN_VSPLTH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vspltw, "__builtin_altivec_vspltw", ALTIVEC_BUILTIN_VSPLTW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsrb, "__builtin_altivec_vsrb", ALTIVEC_BUILTIN_VSRB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsrh, "__builtin_altivec_vsrh", ALTIVEC_BUILTIN_VSRH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsrw, "__builtin_altivec_vsrw", ALTIVEC_BUILTIN_VSRW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsrab, "__builtin_altivec_vsrab", ALTIVEC_BUILTIN_VSRAB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsrah, "__builtin_altivec_vsrah", ALTIVEC_BUILTIN_VSRAH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsraw, "__builtin_altivec_vsraw", ALTIVEC_BUILTIN_VSRAW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsr, "__builtin_altivec_vsr", ALTIVEC_BUILTIN_VSR },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsro, "__builtin_altivec_vsro", ALTIVEC_BUILTIN_VSRO },
  { MASK_ALTIVEC, CODE_FOR_subv16qi3, "__builtin_altivec_vsububm", ALTIVEC_BUILTIN_VSUBUBM },
  { MASK_ALTIVEC, CODE_FOR_subv8hi3, "__builtin_altivec_vsubuhm", ALTIVEC_BUILTIN_VSUBUHM },
  { MASK_ALTIVEC, CODE_FOR_subv4si3, "__builtin_altivec_vsubuwm", ALTIVEC_BUILTIN_VSUBUWM },
  { MASK_ALTIVEC, CODE_FOR_subv4sf3, "__builtin_altivec_vsubfp", ALTIVEC_BUILTIN_VSUBFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsubcuw, "__builtin_altivec_vsubcuw", ALTIVEC_BUILTIN_VSUBCUW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsububs, "__builtin_altivec_vsububs", ALTIVEC_BUILTIN_VSUBUBS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsubsbs, "__builtin_altivec_vsubsbs", ALTIVEC_BUILTIN_VSUBSBS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsubuhs, "__builtin_altivec_vsubuhs", ALTIVEC_BUILTIN_VSUBUHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsubshs, "__builtin_altivec_vsubshs", ALTIVEC_BUILTIN_VSUBSHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsubuws, "__builtin_altivec_vsubuws", ALTIVEC_BUILTIN_VSUBUWS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsubsws, "__builtin_altivec_vsubsws", ALTIVEC_BUILTIN_VSUBSWS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsum4ubs, "__builtin_altivec_vsum4ubs", ALTIVEC_BUILTIN_VSUM4UBS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsum4sbs, "__builtin_altivec_vsum4sbs", ALTIVEC_BUILTIN_VSUM4SBS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsum4shs, "__builtin_altivec_vsum4shs", ALTIVEC_BUILTIN_VSUM4SHS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsum2sws, "__builtin_altivec_vsum2sws", ALTIVEC_BUILTIN_VSUM2SWS },
  { MASK_ALTIVEC, CODE_FOR_altivec_vsumsws, "__builtin_altivec_vsumsws", ALTIVEC_BUILTIN_VSUMSWS },
  { MASK_ALTIVEC, CODE_FOR_xorv4si3, "__builtin_altivec_vxor", ALTIVEC_BUILTIN_VXOR },
};

/* AltiVec predicates.  */

struct builtin_description_predicates
{
  const unsigned int mask;
  const enum insn_code icode;
  const char *opcode;
  const char *const name;
  const enum rs6000_builtins code;
};

static const struct builtin_description_predicates bdesc_altivec_preds[] =
{
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4sf, "*vcmpbfp.", "__builtin_altivec_vcmpbfp_p", ALTIVEC_BUILTIN_VCMPBFP_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4sf, "*vcmpeqfp.", "__builtin_altivec_vcmpeqfp_p", ALTIVEC_BUILTIN_VCMPEQFP_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4sf, "*vcmpgefp.", "__builtin_altivec_vcmpgefp_p", ALTIVEC_BUILTIN_VCMPGEFP_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4sf, "*vcmpgtfp.", "__builtin_altivec_vcmpgtfp_p", ALTIVEC_BUILTIN_VCMPGTFP_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4si, "*vcmpequw.", "__builtin_altivec_vcmpequw_p", ALTIVEC_BUILTIN_VCMPEQUW_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4si, "*vcmpgtsw.", "__builtin_altivec_vcmpgtsw_p", ALTIVEC_BUILTIN_VCMPGTSW_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v4si, "*vcmpgtuw.", "__builtin_altivec_vcmpgtuw_p", ALTIVEC_BUILTIN_VCMPGTUW_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v8hi, "*vcmpgtuh.", "__builtin_altivec_vcmpgtuh_p", ALTIVEC_BUILTIN_VCMPGTUH_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v8hi, "*vcmpgtsh.", "__builtin_altivec_vcmpgtsh_p", ALTIVEC_BUILTIN_VCMPGTSH_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v8hi, "*vcmpequh.", "__builtin_altivec_vcmpequh_p", ALTIVEC_BUILTIN_VCMPEQUH_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v16qi, "*vcmpequb.", "__builtin_altivec_vcmpequb_p", ALTIVEC_BUILTIN_VCMPEQUB_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v16qi, "*vcmpgtsb.", "__builtin_altivec_vcmpgtsb_p", ALTIVEC_BUILTIN_VCMPGTSB_P },
  { MASK_ALTIVEC, CODE_FOR_altivec_predicate_v16qi, "*vcmpgtub.", "__builtin_altivec_vcmpgtub_p", ALTIVEC_BUILTIN_VCMPGTUB_P }
};

/* ABS* opreations.  */

static const struct builtin_description bdesc_abs[] =
{
  { MASK_ALTIVEC, CODE_FOR_absv4si2, "__builtin_altivec_abs_v4si", ALTIVEC_BUILTIN_ABS_V4SI },
  { MASK_ALTIVEC, CODE_FOR_absv8hi2, "__builtin_altivec_abs_v8hi", ALTIVEC_BUILTIN_ABS_V8HI },
  { MASK_ALTIVEC, CODE_FOR_absv4sf2, "__builtin_altivec_abs_v4sf", ALTIVEC_BUILTIN_ABS_V4SF },
  { MASK_ALTIVEC, CODE_FOR_absv16qi2, "__builtin_altivec_abs_v16qi", ALTIVEC_BUILTIN_ABS_V16QI },
  { MASK_ALTIVEC, CODE_FOR_altivec_abss_v4si, "__builtin_altivec_abss_v4si", ALTIVEC_BUILTIN_ABSS_V4SI },
  { MASK_ALTIVEC, CODE_FOR_altivec_abss_v8hi, "__builtin_altivec_abss_v8hi", ALTIVEC_BUILTIN_ABSS_V8HI },
  { MASK_ALTIVEC, CODE_FOR_altivec_abss_v16qi, "__builtin_altivec_abss_v16qi", ALTIVEC_BUILTIN_ABSS_V16QI }
};

/* Simple unary operations: VECb = foo (unsigned literal) or VECb =
   foo (VECa).  */

static const struct builtin_description bdesc_1arg[] =
{
  { MASK_ALTIVEC, CODE_FOR_altivec_vexptefp, "__builtin_altivec_vexptefp", ALTIVEC_BUILTIN_VEXPTEFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vlogefp, "__builtin_altivec_vlogefp", ALTIVEC_BUILTIN_VLOGEFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrefp, "__builtin_altivec_vrefp", ALTIVEC_BUILTIN_VREFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrfim, "__builtin_altivec_vrfim", ALTIVEC_BUILTIN_VRFIM },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrfin, "__builtin_altivec_vrfin", ALTIVEC_BUILTIN_VRFIN },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrfip, "__builtin_altivec_vrfip", ALTIVEC_BUILTIN_VRFIP },
  { MASK_ALTIVEC, CODE_FOR_ftruncv4sf2, "__builtin_altivec_vrfiz", ALTIVEC_BUILTIN_VRFIZ },
  { MASK_ALTIVEC, CODE_FOR_altivec_vrsqrtefp, "__builtin_altivec_vrsqrtefp", ALTIVEC_BUILTIN_VRSQRTEFP },
  { MASK_ALTIVEC, CODE_FOR_altivec_vspltisb, "__builtin_altivec_vspltisb", ALTIVEC_BUILTIN_VSPLTISB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vspltish, "__builtin_altivec_vspltish", ALTIVEC_BUILTIN_VSPLTISH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vspltisw, "__builtin_altivec_vspltisw", ALTIVEC_BUILTIN_VSPLTISW },
  { MASK_ALTIVEC, CODE_FOR_altivec_vupkhsb, "__builtin_altivec_vupkhsb", ALTIVEC_BUILTIN_VUPKHSB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vupkhpx, "__builtin_altivec_vupkhpx", ALTIVEC_BUILTIN_VUPKHPX },
  { MASK_ALTIVEC, CODE_FOR_altivec_vupkhsh, "__builtin_altivec_vupkhsh", ALTIVEC_BUILTIN_VUPKHSH },
  { MASK_ALTIVEC, CODE_FOR_altivec_vupklsb, "__builtin_altivec_vupklsb", ALTIVEC_BUILTIN_VUPKLSB },
  { MASK_ALTIVEC, CODE_FOR_altivec_vupklpx, "__builtin_altivec_vupklpx", ALTIVEC_BUILTIN_VUPKLPX },
  { MASK_ALTIVEC, CODE_FOR_altivec_vupklsh, "__builtin_altivec_vupklsh", ALTIVEC_BUILTIN_VUPKLSH },
};

static rtx
altivec_expand_unop_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node)
    return NULL_RTX;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
altivec_expand_abs_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat, scratch1, scratch2;
  tree arg0 = TREE_VALUE (arglist);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  /* If we have invalid arguments, bail out before generating bad rtl.  */
  if (arg0 == error_mark_node)
    return NULL_RTX;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  scratch1 = gen_reg_rtx (mode0);
  scratch2 = gen_reg_rtx (mode0);

  pat = GEN_FCN (icode) (target, op0, scratch1, scratch2);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
altivec_expand_binop_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return NULL_RTX;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
altivec_expand_predicate_builtin (icode, opcode, arglist, target)
     enum insn_code icode;
     const char *opcode;
     tree arglist;
     rtx target;
{
  rtx pat, scratch;
  tree cr6_form = TREE_VALUE (arglist);
  tree arg0 = TREE_VALUE (TREE_CHAIN (arglist));
  tree arg1 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = SImode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;
  int cr6_form_int;

  if (TREE_CODE (cr6_form) != INTEGER_CST)
    {
      error ("argument 1 of __builtin_altivec_predicate must be a constant");
      return NULL_RTX;
    }
  else
    cr6_form_int = TREE_INT_CST_LOW (cr6_form);

  if (mode0 != mode1)
    abort ();

  /* If we have invalid arguments, bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return NULL_RTX;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  scratch = gen_reg_rtx (mode0);

  pat = GEN_FCN (icode) (scratch, op0, op1,
			 gen_rtx (SYMBOL_REF, Pmode, opcode));
  if (! pat)
    return 0;
  emit_insn (pat);

  /* The vec_any* and vec_all* predicates use the same opcodes for two
     different operations, but the bits in CR6 will be different
     depending on what information we want.  So we have to play tricks
     with CR6 to get the right bits out.

     If you think this is disgusting, look at the specs for the
     AltiVec predicates.  */

     switch (cr6_form_int)
       {
       case 0:
	 emit_insn (gen_cr6_test_for_zero (target));
	 break;
       case 1:
	 emit_insn (gen_cr6_test_for_zero_reverse (target));
	 break;
       case 2:
	 emit_insn (gen_cr6_test_for_lt (target));
	 break;
       case 3:
	 emit_insn (gen_cr6_test_for_lt_reverse (target));
	 break;
       default:
	 error ("argument 1 of __builtin_altivec_predicate is out of range");
	 break;
       }

  return target;
}

static rtx
altivec_expand_stv_builtin (icode, arglist)
     enum insn_code icode;
     tree arglist;
{
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  tree arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  rtx op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
  rtx pat;
  enum machine_mode mode0 = insn_data[icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[icode].operand[1].mode;
  enum machine_mode mode2 = insn_data[icode].operand[2].mode;

  /* Invalid arguments.  Bail before doing anything stoopid!  */
  if (arg0 == error_mark_node
      || arg1 == error_mark_node
      || arg2 == error_mark_node)
    return NULL_RTX;

  if (! (*insn_data[icode].operand[2].predicate) (op0, mode2))
    op0 = copy_to_mode_reg (mode2, op0);
  if (! (*insn_data[icode].operand[0].predicate) (op1, mode0))
    op1 = copy_to_mode_reg (mode0, op1);
  if (! (*insn_data[icode].operand[1].predicate) (op2, mode1))
    op2 = copy_to_mode_reg (mode1, op2);

  pat = GEN_FCN (icode) (op1, op2, op0);
  if (pat)
    emit_insn (pat);
  return NULL_RTX;
}

static rtx
altivec_expand_ternop_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  tree arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  rtx op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;
  enum machine_mode mode2 = insn_data[icode].operand[3].mode;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node
      || arg1 == error_mark_node
      || arg2 == error_mark_node)
    return NULL_RTX;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);
  if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
    op2 = copy_to_mode_reg (mode2, op2);

  pat = GEN_FCN (icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}
static rtx
altivec_expand_builtin (exp, target)
     tree exp;
     rtx target;
{
  struct builtin_description *d;
  struct builtin_description_predicates *dp;
  size_t i;
  enum insn_code icode;
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  tree arg0, arg1, arg2;
  rtx op0, op1, op2, pat;
  enum machine_mode tmode, mode0, mode1, mode2;
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  
  switch (fcode)
    {
    case ALTIVEC_BUILTIN_LD_INTERNAL_16qi:
      icode = CODE_FOR_altivec_lvx_16qi;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));

      pat = GEN_FCN (icode) (target, op0);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case ALTIVEC_BUILTIN_LD_INTERNAL_8hi:
      icode = CODE_FOR_altivec_lvx_8hi;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));

      pat = GEN_FCN (icode) (target, op0);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case ALTIVEC_BUILTIN_LD_INTERNAL_4si:
      icode = CODE_FOR_altivec_lvx_4si;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));

      pat = GEN_FCN (icode) (target, op0);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case ALTIVEC_BUILTIN_LD_INTERNAL_4sf:
      icode = CODE_FOR_altivec_lvx_4sf;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));

      pat = GEN_FCN (icode) (target, op0);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case ALTIVEC_BUILTIN_ST_INTERNAL_16qi:
      icode = CODE_FOR_altivec_stvx_16qi;
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (op0, op1);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;

    case ALTIVEC_BUILTIN_ST_INTERNAL_8hi:
      icode = CODE_FOR_altivec_stvx_8hi;
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (op0, op1);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;

    case ALTIVEC_BUILTIN_ST_INTERNAL_4si:
      icode = CODE_FOR_altivec_stvx_4si;
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (op0, op1);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;

    case ALTIVEC_BUILTIN_ST_INTERNAL_4sf:
      icode = CODE_FOR_altivec_stvx_4sf;
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (op0, op1);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;

    case ALTIVEC_BUILTIN_STVX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx, arglist);
    case ALTIVEC_BUILTIN_STVEBX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvebx, arglist);
    case ALTIVEC_BUILTIN_STVEHX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvehx, arglist);
    case ALTIVEC_BUILTIN_STVEWX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvewx, arglist);
    case ALTIVEC_BUILTIN_STVXL:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl, arglist);
  
    case ALTIVEC_BUILTIN_MFVSCR:
      icode = CODE_FOR_altivec_mfvscr;
      tmode = insn_data[icode].operand[0].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      
      pat = GEN_FCN (icode) (target);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case ALTIVEC_BUILTIN_MTVSCR:
      icode = CODE_FOR_altivec_mtvscr;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;

      /* If we got invalid arguments bail out before generating bad rtl.  */
      if (arg0 == error_mark_node)
	return NULL_RTX;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (op0);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;
      
    case ALTIVEC_BUILTIN_DSSALL:
      emit_insn (gen_altivec_dssall ());
      return NULL_RTX;

    case ALTIVEC_BUILTIN_DSS:
      icode = CODE_FOR_altivec_dss;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;

      /* If we got invalid arguments bail out before generating bad rtl.  */
      if (arg0 == error_mark_node)
	return NULL_RTX;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      emit_insn (gen_altivec_dss (op0));
      return NULL_RTX;
    }

  /* Handle DST variants.  */
  d = (struct builtin_description *) bdesc_dst;
  for (i = 0; i < sizeof (bdesc_dst) / sizeof *d; i++, d++)
    if (d->code == fcode)
      {
	arg0 = TREE_VALUE (arglist);
	arg1 = TREE_VALUE (TREE_CHAIN (arglist));
	arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
	op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
	mode0 = insn_data[d->icode].operand[0].mode;
	mode1 = insn_data[d->icode].operand[1].mode;
	mode2 = insn_data[d->icode].operand[2].mode;

	/* Invalid arguments, bail out before generating bad rtl.  */
	if (arg0 == error_mark_node
	    || arg1 == error_mark_node
	    || arg2 == error_mark_node)
	  return NULL_RTX;

	if (! (*insn_data[d->icode].operand[0].predicate) (op0, mode0))
	  op0 = copy_to_mode_reg (mode0, op0);
	if (! (*insn_data[d->icode].operand[1].predicate) (op1, mode1))
	  op1 = copy_to_mode_reg (mode1, op1);

	if (GET_CODE (op2) != CONST_INT || INTVAL (op2) > 3)
	  {
	    error ("argument 3 of `%s' must be a 2-bit literal", d->name);
	    return NULL_RTX;
	  }

	pat = GEN_FCN (d->icode) (op0, op1, op2);
	if (pat != 0)
	  emit_insn (pat);

	return NULL_RTX;
      }

  /* Expand abs* operations.  */
  d = (struct builtin_description *) bdesc_abs;
  for (i = 0; i < sizeof (bdesc_abs) / sizeof *d; i++, d++)
    if (d->code == fcode)
      return altivec_expand_abs_builtin (d->icode, arglist, target);

  /* Handle simple unary operations.  */
  d = (struct builtin_description *) bdesc_1arg;
  for (i = 0; i < sizeof (bdesc_1arg) / sizeof *d; i++, d++)
    if (d->code == fcode)
      return altivec_expand_unop_builtin (d->icode, arglist, target);

  /* Handle simple binary operations.  */
  d = (struct builtin_description *) bdesc_2arg;
  for (i = 0; i < sizeof (bdesc_2arg) / sizeof *d; i++, d++)
    if (d->code == fcode)
      return altivec_expand_binop_builtin (d->icode, arglist, target);

  /* Expand the AltiVec predicates.  */
  dp = (struct builtin_description_predicates *) bdesc_altivec_preds;
  for (i = 0; i < sizeof (bdesc_altivec_preds) / sizeof *dp; i++, dp++)
    if (dp->code == fcode)
      return altivec_expand_predicate_builtin (dp->icode, dp->opcode, arglist, target);

  /* LV* are funky.  We initialized them differently.  */
  switch (fcode)
    {
    case ALTIVEC_BUILTIN_LVSL:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvsl,
					   arglist, target);
    case ALTIVEC_BUILTIN_LVSR:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvsr,
					   arglist, target);
    case ALTIVEC_BUILTIN_LVEBX:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvebx,
					   arglist, target);
    case ALTIVEC_BUILTIN_LVEHX:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvehx,
					   arglist, target);
    case ALTIVEC_BUILTIN_LVEWX:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvewx,
					   arglist, target);
    case ALTIVEC_BUILTIN_LVXL:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvxl,
					   arglist, target);
    case ALTIVEC_BUILTIN_LVX:
      return altivec_expand_binop_builtin (CODE_FOR_altivec_lvx,
					   arglist, target);
    default:
      break;
      /* Fall through.  */
    }

  /* Handle simple ternary operations.  */
  d = (struct builtin_description *) bdesc_3arg;
  for (i = 0; i < sizeof  (bdesc_3arg) / sizeof *d; i++, d++)
    if (d->code == fcode)
      return altivec_expand_ternop_builtin (d->icode, arglist, target);

  abort ();
  return NULL_RTX;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
rs6000_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
  if (TARGET_ALTIVEC)
    return altivec_expand_builtin (exp, target);

  abort ();
}

static void
rs6000_init_builtins ()
{
  if (TARGET_ALTIVEC)
    altivec_init_builtins ();
}

static void
altivec_init_builtins (void)
{
  struct builtin_description *d;
  struct builtin_description_predicates *dp;
  size_t i;

  tree endlink = void_list_node;

  tree pint_type_node = build_pointer_type (integer_type_node);
  tree pvoid_type_node = build_pointer_type (void_type_node);
  tree pshort_type_node = build_pointer_type (short_integer_type_node);
  tree pchar_type_node = build_pointer_type (char_type_node);
  tree pfloat_type_node = build_pointer_type (float_type_node);

  tree v4sf_ftype_v4sf_v4sf_v16qi
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 tree_cons (NULL_TREE, 
							    V16QI_type_node,
							    endlink))));
  tree v4si_ftype_v4si_v4si_v16qi
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 tree_cons (NULL_TREE, 
							    V16QI_type_node,
							    endlink))));
  tree v8hi_ftype_v8hi_v8hi_v16qi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 tree_cons (NULL_TREE, 
							    V16QI_type_node,
							    endlink))));
  tree v16qi_ftype_v16qi_v16qi_v16qi
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 tree_cons (NULL_TREE, 
							    V16QI_type_node,
							    endlink))));

  /* V4SI foo (char).  */
  tree v4si_ftype_char
    = build_function_type (V4SI_type_node,
		           tree_cons (NULL_TREE, char_type_node, endlink));

  /* V8HI foo (char).  */
  tree v8hi_ftype_char
    = build_function_type (V8HI_type_node,
		           tree_cons (NULL_TREE, char_type_node, endlink));

  /* V16QI foo (char).  */
  tree v16qi_ftype_char
    = build_function_type (V16QI_type_node,
		           tree_cons (NULL_TREE, char_type_node, endlink));
  /* V4SF foo (V4SF).  */
  tree v4sf_ftype_v4sf
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node, endlink));

  /* V4SI foo (int *).  */
  tree v4si_ftype_pint
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, pint_type_node, endlink));
  /* V8HI foo (short *).  */
  tree v8hi_ftype_pshort
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, pshort_type_node, endlink));
  /* V16QI foo (char *).  */
  tree v16qi_ftype_pchar
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, pchar_type_node, endlink));
  /* V4SF foo (float *).  */
  tree v4sf_ftype_pfloat
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, pfloat_type_node, endlink));

  /* V8HI foo (V16QI).  */
  tree v8hi_ftype_v16qi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node, endlink));

  /* void foo (void *, int, char/literal).  */
  tree void_ftype_pvoid_int_char
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pvoid_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    char_type_node,
							    endlink))));

  /* void foo (int *, V4SI).  */
  tree void_ftype_pint_v4si
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pint_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 endlink)));
  /* void foo (short *, V8HI).  */
  tree void_ftype_pshort_v8hi
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pshort_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 endlink)));
  /* void foo (char *, V16QI).  */
  tree void_ftype_pchar_v16qi
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pchar_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 endlink)));
  /* void foo (float *, V4SF).  */
  tree void_ftype_pfloat_v4sf
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pfloat_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 endlink)));

  /* void foo (V4SI).  */
  tree void_ftype_v4si
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      endlink));

  /* void foo (vint, int, void *).  */
  tree void_ftype_v4si_int_pvoid
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    pvoid_type_node,
							    endlink))));

  /* void foo (vchar, int, void *).  */
  tree void_ftype_v16qi_int_pvoid
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    pvoid_type_node,
							    endlink))));

  /* void foo (vshort, int, void *).  */
  tree void_ftype_v8hi_int_pvoid
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    pvoid_type_node,
							    endlink))));

  /* void foo (char).  */
  tree void_ftype_qi
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, char_type_node,
				      endlink));

  /* void foo (void).  */
  tree void_ftype_void
    = build_function_type (void_type_node, void_list_node);

  /* vshort foo (void).  */
  tree v8hi_ftype_void
    = build_function_type (V8HI_type_node, void_list_node);

  tree v4si_ftype_v4si_v4si
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 endlink)));

  /* These are for the unsigned 5 bit literals.  */

  tree v4sf_ftype_v4si_char
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, char_type_node,
						 endlink)));
  tree v4si_ftype_v4sf_char
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, char_type_node,
						 endlink)));
  tree v4si_ftype_v4si_char
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, char_type_node,
						 endlink)));
  tree v8hi_ftype_v8hi_char
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, char_type_node,
						 endlink)));
  tree v16qi_ftype_v16qi_char
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, char_type_node,
						 endlink)));

  /* These are for the unsigned 4 bit literals.  */

  tree v16qi_ftype_v16qi_v16qi_char
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 tree_cons (NULL_TREE,
							    char_type_node,
							    endlink))));

  tree v8hi_ftype_v8hi_v8hi_char
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 tree_cons (NULL_TREE,
							    char_type_node,
							    endlink))));

  tree v4si_ftype_v4si_v4si_char
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 tree_cons (NULL_TREE,
							    char_type_node,
							    endlink))));

  tree v4sf_ftype_v4sf_v4sf_char
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 tree_cons (NULL_TREE,
							    char_type_node,
							    endlink))));

  /* End of 4 bit literals.  */

  tree v4sf_ftype_v4sf_v4sf
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 endlink)));
  tree v4sf_ftype_v4sf_v4sf_v4si
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 tree_cons (NULL_TREE,
							    V4SI_type_node,
							    endlink))));
  tree v4sf_ftype_v4sf_v4sf_v4sf
    = build_function_type (V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 tree_cons (NULL_TREE, 
							    V4SF_type_node,
							    endlink))));
  tree v4si_ftype_v4si_v4si_v4si 
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 tree_cons (NULL_TREE,
							    V4SI_type_node,
							    endlink))));

  tree v8hi_ftype_v8hi_v8hi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 endlink)));
  tree v8hi_ftype_v8hi_v8hi_v8hi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 tree_cons (NULL_TREE, 
							    V8HI_type_node,
							    endlink))));
 tree v4si_ftype_v8hi_v8hi_v4si
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 tree_cons (NULL_TREE,
							    V4SI_type_node,
							    endlink))));
 tree v4si_ftype_v16qi_v16qi_v4si
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 tree_cons (NULL_TREE,
							    V4SI_type_node,
							    endlink))));
  
  tree v16qi_ftype_v16qi_v16qi
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 endlink)));
  
  tree v4si_ftype_v4sf_v4sf
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 endlink)));

  tree v4si_ftype_v4si
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node, endlink));

  tree v8hi_ftype_v8hi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node, endlink));

  tree v16qi_ftype_v16qi
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node, endlink));

  tree v8hi_ftype_v16qi_v16qi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 endlink)));

  tree v4si_ftype_v8hi_v8hi
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 endlink)));

  tree v8hi_ftype_v4si_v4si
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 endlink)));

  tree v16qi_ftype_v8hi_v8hi
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 endlink)));

  tree v4si_ftype_v16qi_v4si
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 endlink)));

  tree v4si_ftype_v16qi_v16qi
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 endlink)));

  tree v4si_ftype_v8hi_v4si
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 endlink)));

  tree v4si_ftype_v8hi
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node, endlink));

  tree int_ftype_v4si_v4si
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      tree_cons (NULL_TREE, V4SI_type_node,
						 endlink)));

  tree int_ftype_v4sf_v4sf
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      tree_cons (NULL_TREE, V4SF_type_node,
						 endlink)));

  tree int_ftype_v16qi_v16qi
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      tree_cons (NULL_TREE, V16QI_type_node,
						 endlink)));

  tree int_ftype_int_v4si_v4si
    = build_function_type
    (integer_type_node,
     tree_cons (NULL_TREE, integer_type_node,
		tree_cons (NULL_TREE, V4SI_type_node,
			   tree_cons (NULL_TREE, V4SI_type_node,
				      endlink))));

  tree int_ftype_int_v4sf_v4sf
    = build_function_type
    (integer_type_node,
     tree_cons (NULL_TREE, integer_type_node,
		tree_cons (NULL_TREE, V4SF_type_node,
			   tree_cons (NULL_TREE, V4SF_type_node,
				      endlink))));

  tree int_ftype_int_v8hi_v8hi
    = build_function_type
    (integer_type_node,
     tree_cons (NULL_TREE, integer_type_node,
		 tree_cons (NULL_TREE, V8HI_type_node,
			    tree_cons (NULL_TREE, V8HI_type_node,
				       endlink))));

  tree int_ftype_int_v16qi_v16qi
    = build_function_type
    (integer_type_node,
     tree_cons (NULL_TREE, integer_type_node,
		tree_cons (NULL_TREE, V16QI_type_node,
			   tree_cons (NULL_TREE, V16QI_type_node,
				      endlink))));

  tree v16qi_ftype_int_pvoid
    = build_function_type (V16QI_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, pvoid_type_node,
						 endlink)));

  tree v4si_ftype_int_pvoid
    = build_function_type (V4SI_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, pvoid_type_node,
						 endlink)));

  tree v8hi_ftype_int_pvoid
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, pvoid_type_node,
						 endlink)));

  tree int_ftype_v8hi_v8hi
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 endlink)));

  def_builtin (MASK_ALTIVEC, "__builtin_altivec_ld_internal_4sf", v4sf_ftype_pfloat, ALTIVEC_BUILTIN_LD_INTERNAL_4sf);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_st_internal_4sf", void_ftype_pfloat_v4sf, ALTIVEC_BUILTIN_ST_INTERNAL_4sf);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_ld_internal_4si", v4si_ftype_pint, ALTIVEC_BUILTIN_LD_INTERNAL_4si);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_st_internal_4si", void_ftype_pint_v4si, ALTIVEC_BUILTIN_ST_INTERNAL_4si);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_ld_internal_8hi", v8hi_ftype_pshort, ALTIVEC_BUILTIN_LD_INTERNAL_8hi);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_st_internal_8hi", void_ftype_pshort_v8hi, ALTIVEC_BUILTIN_ST_INTERNAL_8hi);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_ld_internal_16qi", v16qi_ftype_pchar, ALTIVEC_BUILTIN_LD_INTERNAL_16qi);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_st_internal_16qi", void_ftype_pchar_v16qi, ALTIVEC_BUILTIN_ST_INTERNAL_16qi);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_mtvscr", void_ftype_v4si, ALTIVEC_BUILTIN_MTVSCR);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_mfvscr", v8hi_ftype_void, ALTIVEC_BUILTIN_MFVSCR);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_dssall", void_ftype_void, ALTIVEC_BUILTIN_DSSALL);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_dss", void_ftype_qi, ALTIVEC_BUILTIN_DSS);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvsl", v16qi_ftype_int_pvoid, ALTIVEC_BUILTIN_LVSL);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvsr", v16qi_ftype_int_pvoid, ALTIVEC_BUILTIN_LVSR);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvebx", v16qi_ftype_int_pvoid, ALTIVEC_BUILTIN_LVEBX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvehx", v8hi_ftype_int_pvoid, ALTIVEC_BUILTIN_LVEHX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvewx", v4si_ftype_int_pvoid, ALTIVEC_BUILTIN_LVEWX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvxl", v4si_ftype_int_pvoid, ALTIVEC_BUILTIN_LVXL);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_lvx", v4si_ftype_int_pvoid, ALTIVEC_BUILTIN_LVX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_stvx", void_ftype_v4si_int_pvoid, ALTIVEC_BUILTIN_STVX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_stvebx", void_ftype_v16qi_int_pvoid, ALTIVEC_BUILTIN_STVEBX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_stvehx", void_ftype_v8hi_int_pvoid, ALTIVEC_BUILTIN_STVEHX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_stvewx", void_ftype_v4si_int_pvoid, ALTIVEC_BUILTIN_STVEWX);
  def_builtin (MASK_ALTIVEC, "__builtin_altivec_stvxl", void_ftype_v4si_int_pvoid, ALTIVEC_BUILTIN_STVXL);

  /* Add the simple ternary operators.  */
  d = (struct builtin_description *) bdesc_3arg;
  for (i = 0; i < sizeof (bdesc_3arg) / sizeof *d; i++, d++)
    {
      
      enum machine_mode mode0, mode1, mode2, mode3;
      tree type;

      if (d->name == 0)
	continue;
      
      mode0 = insn_data[d->icode].operand[0].mode;
      mode1 = insn_data[d->icode].operand[1].mode;
      mode2 = insn_data[d->icode].operand[2].mode;
      mode3 = insn_data[d->icode].operand[3].mode;
      
      /* When all four are of the same mode.  */
      if (mode0 == mode1 && mode1 == mode2 && mode2 == mode3)
	{
	  switch (mode0)
	    {
	    case V4SImode:
	      type = v4si_ftype_v4si_v4si_v4si;
	      break;
	    case V4SFmode:
	      type = v4sf_ftype_v4sf_v4sf_v4sf;
	      break;
	    case V8HImode:
	      type = v8hi_ftype_v8hi_v8hi_v8hi;
	      break;	      
	    case V16QImode:
	      type = v16qi_ftype_v16qi_v16qi_v16qi;
	      break;	      
	    default:
	      abort();	      
	    }
	}
      else if (mode0 == mode1 && mode1 == mode2 && mode3 == V16QImode)
        {
	  switch (mode0)
	    {
	    case V4SImode:
	      type = v4si_ftype_v4si_v4si_v16qi;
	      break;
	    case V4SFmode:
	      type = v4sf_ftype_v4sf_v4sf_v16qi;
	      break;
	    case V8HImode:
	      type = v8hi_ftype_v8hi_v8hi_v16qi;
	      break;	      
	    case V16QImode:
	      type = v16qi_ftype_v16qi_v16qi_v16qi;
	      break;	      
	    default:
	      abort();	      
	    }
	}
      else if (mode0 == V4SImode && mode1 == V16QImode && mode2 == V16QImode 
	       && mode3 == V4SImode)
	type = v4si_ftype_v16qi_v16qi_v4si;
      else if (mode0 == V4SImode && mode1 == V8HImode && mode2 == V8HImode 
	       && mode3 == V4SImode)
	type = v4si_ftype_v8hi_v8hi_v4si;
      else if (mode0 == V4SFmode && mode1 == V4SFmode && mode2 == V4SFmode 
	       && mode3 == V4SImode)
	type = v4sf_ftype_v4sf_v4sf_v4si;

      /* vchar, vchar, vchar, 4 bit literal.  */
      else if (mode0 == V16QImode && mode1 == mode0 && mode2 == mode0
	       && mode3 == QImode)
	type = v16qi_ftype_v16qi_v16qi_char;

      /* vshort, vshort, vshort, 4 bit literal.  */
      else if (mode0 == V8HImode && mode1 == mode0 && mode2 == mode0
	       && mode3 == QImode)
	type = v8hi_ftype_v8hi_v8hi_char;

      /* vint, vint, vint, 4 bit literal.  */
      else if (mode0 == V4SImode && mode1 == mode0 && mode2 == mode0
	       && mode3 == QImode)
	type = v4si_ftype_v4si_v4si_char;

      /* vfloat, vfloat, vfloat, 4 bit literal.  */
      else if (mode0 == V4SFmode && mode1 == mode0 && mode2 == mode0
	       && mode3 == QImode)
	type = v4sf_ftype_v4sf_v4sf_char;

      else
	abort ();

      def_builtin (d->mask, d->name, type, d->code);
    }

  /* Add the DST variants.  */
  d = (struct builtin_description *) bdesc_dst;
  for (i = 0; i < sizeof (bdesc_dst) / sizeof *d; i++, d++)
    def_builtin (d->mask, d->name, void_ftype_pvoid_int_char, d->code);

  /* Initialize the predicates.  */
  dp = (struct builtin_description_predicates *) bdesc_altivec_preds;
  for (i = 0; i < sizeof (bdesc_altivec_preds) / sizeof *dp; i++, dp++)
    {
      enum machine_mode mode1;
      tree type;

      mode1 = insn_data[dp->icode].operand[1].mode;

      switch (mode1)
	{
	case V4SImode:
	  type = int_ftype_int_v4si_v4si;
	  break;
	case V8HImode:
	  type = int_ftype_int_v8hi_v8hi;
	  break;
	case V16QImode:
	  type = int_ftype_int_v16qi_v16qi;
	  break;
	case V4SFmode:
	  type = int_ftype_int_v4sf_v4sf;
	  break;
	default:
	  abort ();
	}
      
      def_builtin (dp->mask, dp->name, type, dp->code);
    }

  /* Add the simple binary operators.  */
  d = (struct builtin_description *) bdesc_2arg;
  for (i = 0; i < sizeof (bdesc_2arg) / sizeof *d; i++, d++)
    {
      enum machine_mode mode0, mode1, mode2;
      tree type;

      if (d->name == 0)
	continue;
      
      mode0 = insn_data[d->icode].operand[0].mode;
      mode1 = insn_data[d->icode].operand[1].mode;
      mode2 = insn_data[d->icode].operand[2].mode;

      /* When all three operands are of the same mode.  */
      if (mode0 == mode1 && mode1 == mode2)
	{
	  switch (mode0)
	    {
	    case V4SFmode:
	      type = v4sf_ftype_v4sf_v4sf;
	      break;
	    case V4SImode:
	      type = v4si_ftype_v4si_v4si;
	      break;
	    case V16QImode:
	      type = v16qi_ftype_v16qi_v16qi;
	      break;
	    case V8HImode:
	      type = v8hi_ftype_v8hi_v8hi;
	      break;
	    default:
	      abort ();
	    }
	}

      /* A few other combos we really don't want to do manually.  */

      /* vint, vfloat, vfloat.  */
      else if (mode0 == V4SImode && mode1 == V4SFmode && mode2 == V4SFmode)
	type = v4si_ftype_v4sf_v4sf;

      /* vshort, vchar, vchar.  */
      else if (mode0 == V8HImode && mode1 == V16QImode && mode2 == V16QImode)
	type = v8hi_ftype_v16qi_v16qi;

      /* vint, vshort, vshort.  */
      else if (mode0 == V4SImode && mode1 == V8HImode && mode2 == V8HImode)
	type = v4si_ftype_v8hi_v8hi;

      /* vshort, vint, vint.  */
      else if (mode0 == V8HImode && mode1 == V4SImode && mode2 == V4SImode)
	type = v8hi_ftype_v4si_v4si;

      /* vchar, vshort, vshort.  */
      else if (mode0 == V16QImode && mode1 == V8HImode && mode2 == V8HImode)
	type = v16qi_ftype_v8hi_v8hi;

      /* vint, vchar, vint.  */
      else if (mode0 == V4SImode && mode1 == V16QImode && mode2 == V4SImode)
	type = v4si_ftype_v16qi_v4si;

      /* vint, vchar, vchar.  */
      else if (mode0 == V4SImode && mode1 == V16QImode && mode2 == V16QImode)
	type = v4si_ftype_v16qi_v16qi;

      /* vint, vshort, vint.  */
      else if (mode0 == V4SImode && mode1 == V8HImode && mode2 == V4SImode)
	type = v4si_ftype_v8hi_v4si;
      
      /* vint, vint, 5 bit literal.  */
      else if (mode0 == V4SImode && mode1 == V4SImode && mode2 == QImode)
	type = v4si_ftype_v4si_char;
      
      /* vshort, vshort, 5 bit literal.  */
      else if (mode0 == V8HImode && mode1 == V8HImode && mode2 == QImode)
	type = v8hi_ftype_v8hi_char;
      
      /* vchar, vchar, 5 bit literal.  */
      else if (mode0 == V16QImode && mode1 == V16QImode && mode2 == QImode)
	type = v16qi_ftype_v16qi_char;

      /* vfloat, vint, 5 bit literal.  */
      else if (mode0 == V4SFmode && mode1 == V4SImode && mode2 == QImode)
	type = v4sf_ftype_v4si_char;
      
      /* vint, vfloat, 5 bit literal.  */
      else if (mode0 == V4SImode && mode1 == V4SFmode && mode2 == QImode)
	type = v4si_ftype_v4sf_char;

      /* int, x, x.  */
      else if (mode0 == SImode)
	{
	  switch (mode1)
	    {
	    case V4SImode:
	      type = int_ftype_v4si_v4si;
	      break;
	    case V4SFmode:
	      type = int_ftype_v4sf_v4sf;
	      break;
	    case V16QImode:
	      type = int_ftype_v16qi_v16qi;
	      break;
	    case V8HImode:
	      type = int_ftype_v8hi_v8hi;
	      break;
	    default:
	      abort ();
	    }
	}

      else
	abort ();

      def_builtin (d->mask, d->name, type, d->code);
    }

  /* Initialize the abs* operators.  */
  d = (struct builtin_description *) bdesc_abs;
  for (i = 0; i < sizeof (bdesc_abs) / sizeof *d; i++, d++)
    {
      enum machine_mode mode0;
      tree type;

      mode0 = insn_data[d->icode].operand[0].mode;

      switch (mode0)
	{
	case V4SImode:
	  type = v4si_ftype_v4si;
	  break;
	case V8HImode:
	  type = v8hi_ftype_v8hi;
	  break;
	case V16QImode:
	  type = v16qi_ftype_v16qi;
	  break;
	case V4SFmode:
	  type = v4sf_ftype_v4sf;
	  break;
	default:
	  abort ();
	}
      
      def_builtin (d->mask, d->name, type, d->code);
    }

  /* Add the simple unary operators.  */
  d = (struct builtin_description *) bdesc_1arg;
  for (i = 0; i < sizeof (bdesc_1arg) / sizeof *d; i++, d++)
    {
      enum machine_mode mode0, mode1;
      tree type;

      if (d->name == 0)
	continue;
      
      mode0 = insn_data[d->icode].operand[0].mode;
      mode1 = insn_data[d->icode].operand[1].mode;

      if (mode0 == V4SImode && mode1 == QImode)
        type = v4si_ftype_char;
      else if (mode0 == V8HImode && mode1 == QImode)
        type = v8hi_ftype_char;
      else if (mode0 == V16QImode && mode1 == QImode)
        type = v16qi_ftype_char;
      else if (mode0 == V4SFmode && mode1 == V4SFmode)
	type = v4sf_ftype_v4sf;
      else if (mode0 == V8HImode && mode1 == V16QImode)
	type = v8hi_ftype_v16qi;
      else if (mode0 == V4SImode && mode1 == V8HImode)
	type = v4si_ftype_v8hi;
      else
	abort ();

      def_builtin (d->mask, d->name, type, d->code);
    }
}


/* Expand a block move operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the source
   operands[2] is the length
   operands[3] is the alignment */

#define MAX_MOVE_REG 4

int
expand_block_move (operands)
     rtx operands[];
{
  rtx orig_dest = operands[0];
  rtx orig_src	= operands[1];
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align;
  int bytes;
  int offset;
  int move_bytes;

  /* If this is not a fixed size move, just call memcpy */
  if (! constp)
    return 0;

  /* If this is not a fixed size alignment, abort */
  if (GET_CODE (align_rtx) != CONST_INT)
    abort ();
  align = INTVAL (align_rtx);

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  /* store_one_arg depends on expand_block_move to handle at least the size of
     reg_parm_stack_space.  */	
  if (bytes > (TARGET_POWERPC64 ? 64 : 32))
    return 0;

  if (TARGET_STRING)	/* string instructions are available */
    {
      for (offset = 0; bytes > 0; offset += move_bytes, bytes -= move_bytes)
	{
	  union {
	    rtx (*movstrsi) PARAMS ((rtx, rtx, rtx, rtx));
	    rtx (*mov) PARAMS ((rtx, rtx));
	  } gen_func;
	  enum machine_mode mode = BLKmode;
	  rtx src, dest;

	  if (bytes > 24		/* move up to 32 bytes at a time */
	      && ! fixed_regs[5]
	      && ! fixed_regs[6]
	      && ! fixed_regs[7]
	      && ! fixed_regs[8]
	      && ! fixed_regs[9]
	      && ! fixed_regs[10]
	      && ! fixed_regs[11]
	      && ! fixed_regs[12])
	    {
	      move_bytes = (bytes > 32) ? 32 : bytes;
	      gen_func.movstrsi = gen_movstrsi_8reg;
	    }
	  else if (bytes > 16	/* move up to 24 bytes at a time */
		   && ! fixed_regs[5]
		   && ! fixed_regs[6]
		   && ! fixed_regs[7]
		   && ! fixed_regs[8]
		   && ! fixed_regs[9]
		   && ! fixed_regs[10])
	    {
	      move_bytes = (bytes > 24) ? 24 : bytes;
	      gen_func.movstrsi = gen_movstrsi_6reg;
	    }
	  else if (bytes > 8	/* move up to 16 bytes at a time */
		   && ! fixed_regs[5]
		   && ! fixed_regs[6]
		   && ! fixed_regs[7]
		   && ! fixed_regs[8])
	    {
	      move_bytes = (bytes > 16) ? 16 : bytes;
	      gen_func.movstrsi = gen_movstrsi_4reg;
	    }
	  else if (bytes >= 8 && TARGET_POWERPC64
		   /* 64-bit loads and stores require word-aligned
                      displacements.  */
		   && (align >= 8 || (! STRICT_ALIGNMENT && align >= 4)))
	    {
	      move_bytes = 8;
	      mode = DImode;
	      gen_func.mov = gen_movdi;
	    }
	  else if (bytes > 4 && !TARGET_POWERPC64)
	    {			/* move up to 8 bytes at a time */
	      move_bytes = (bytes > 8) ? 8 : bytes;
	      gen_func.movstrsi = gen_movstrsi_2reg;
	    }
	  else if (bytes >= 4 && (align >= 4 || ! STRICT_ALIGNMENT))
	    {			/* move 4 bytes */
	      move_bytes = 4;
	      mode = SImode;
	      gen_func.mov = gen_movsi;
	    }
	  else if (bytes == 2 && (align >= 2 || ! STRICT_ALIGNMENT))
	    {			/* move 2 bytes */
	      move_bytes = 2;
	      mode = HImode;
	      gen_func.mov = gen_movhi;
	    }
	  else if (bytes == 1)	/* move 1 byte */
	    {
	      move_bytes = 1;
	      mode = QImode;
	      gen_func.mov = gen_movqi;
	    }
	  else
	    {			/* move up to 4 bytes at a time */
	      move_bytes = (bytes > 4) ? 4 : bytes;
	      gen_func.movstrsi = gen_movstrsi_1reg;
	    }

	  src = adjust_address (orig_src, mode, offset);
	  dest = adjust_address (orig_dest, mode, offset);

	  if (mode == BLKmode)
	    {
	      /* Move the address into scratch registers.  The movstrsi
		 patterns require zero offset.  */
	      if (!REG_P (XEXP (src, 0)))
		{
		  rtx src_reg = copy_addr_to_reg (XEXP (src, 0));
		  src = replace_equiv_address (src, src_reg);
		}
	      set_mem_size (src, GEN_INT (move_bytes));

	      if (!REG_P (XEXP (dest, 0)))
		{
		  rtx dest_reg = copy_addr_to_reg (XEXP (dest, 0));
		  dest = replace_equiv_address (dest, dest_reg);
		}
	      set_mem_size (dest, GEN_INT (move_bytes));

	      emit_insn ((*gen_func.movstrsi) (dest, src,
					       GEN_INT (move_bytes & 31),
					       align_rtx));
	    }
	  else
	    {
	      rtx tmp_reg = gen_reg_rtx (mode);

	      emit_insn ((*gen_func.mov) (tmp_reg, src));
	      emit_insn ((*gen_func.mov) (dest, tmp_reg));
	    }
	}
    }

  else			/* string instructions not available */
    {
      rtx stores[MAX_MOVE_REG];
      int num_reg = 0;
      int i;

      for (offset = 0; bytes > 0; offset += move_bytes, bytes -= move_bytes)
	{
	  rtx (*gen_mov_func) PARAMS ((rtx, rtx));
	  enum machine_mode mode;
	  rtx src, dest, tmp_reg;

	  /* Generate the appropriate load and store, saving the stores
	     for later.  */
	  if (bytes >= 8 && TARGET_POWERPC64
	      /* 64-bit loads and stores require word-aligned
                 displacements.  */
	      && (align >= 8 || (! STRICT_ALIGNMENT && align >= 4)))
	    {
	      move_bytes = 8;
	      mode = DImode;
	      gen_mov_func = gen_movdi;
	    }
	  else if (bytes >= 4 && (align >= 4 || ! STRICT_ALIGNMENT))
	    {
	      move_bytes = 4;
	      mode = SImode;
	      gen_mov_func = gen_movsi;
	    }
	  else if (bytes >= 2 && (align >= 2 || ! STRICT_ALIGNMENT))
	    {
	      move_bytes = 2;
	      mode = HImode;
	      gen_mov_func = gen_movhi;
	    }
	  else
	    {
	      move_bytes = 1;
	      mode = QImode;
	      gen_mov_func = gen_movqi;
	    }

	  src = adjust_address (orig_src, mode, offset);
	  dest = adjust_address (orig_dest, mode, offset);
	  tmp_reg = gen_reg_rtx (mode);

	  emit_insn ((*gen_mov_func) (tmp_reg, src));
	  stores[num_reg++] = (*gen_mov_func) (dest, tmp_reg);

	  if (num_reg >= MAX_MOVE_REG)
	    {
	      for (i = 0; i < num_reg; i++)
		emit_insn (stores[i]);
	      num_reg = 0;
	    }
	}

      for (i = 0; i < num_reg; i++)
	emit_insn (stores[i]);
    }

  return 1;
}


/* Return 1 if OP is a load multiple operation.  It is known to be a
   PARALLEL and the first section will be tested.  */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Similar, but tests for store multiple.  Here, the second vector element
   is a CLOBBER.  It will be tested later.  */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0) - 1;
  unsigned int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 1);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Return a string to perform a load_multiple operation.
   operands[0] is the vector.
   operands[1] is the source address.
   operands[2] is the first destination register.  */

const char *
rs6000_output_load_multiple (operands)
     rtx operands[3];
{
  /* We have to handle the case where the pseudo used to contain the address
     is assigned to one of the output registers.  */
  int i, j;
  int words = XVECLEN (operands[0], 0);
  rtx xop[10];

  if (XVECLEN (operands[0], 0) == 1)
    return "{l|lwz} %2,0(%1)";

  for (i = 0; i < words; i++)
    if (refers_to_regno_p (REGNO (operands[2]) + i,
			   REGNO (operands[2]) + i + 1, operands[1], 0))
      {
	if (i == words-1)
	  {
	    xop[0] = GEN_INT (4 * (words-1));
	    xop[1] = operands[1];
	    xop[2] = operands[2];
	    output_asm_insn ("{lsi|lswi} %2,%1,%0\n\t{l|lwz} %1,%0(%1)", xop);
	    return "";
	  }
	else if (i == 0)
	  {
	    xop[0] = GEN_INT (4 * (words-1));
	    xop[1] = operands[1];
	    xop[2] = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);
	    output_asm_insn ("{cal %1,4(%1)|addi %1,%1,4}\n\t{lsi|lswi} %2,%1,%0\n\t{l|lwz} %1,-4(%1)", xop);
	    return "";
	  }
	else
	  {
	    for (j = 0; j < words; j++)
	      if (j != i)
		{
		  xop[0] = GEN_INT (j * 4);
		  xop[1] = operands[1];
		  xop[2] = gen_rtx_REG (SImode, REGNO (operands[2]) + j);
		  output_asm_insn ("{l|lwz} %2,%0(%1)", xop);
		}
	    xop[0] = GEN_INT (i * 4);
	    xop[1] = operands[1];
	    output_asm_insn ("{l|lwz} %1,%0(%1)", xop);
	    return "";
	  }
      }

  return "{lsi|lswi} %2,%1,%N0";
}

/* Return 1 for a parallel vrsave operation.  */

int
vrsave_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno, src_regno;
  int i;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC_VOLATILE)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_regno  = REGNO (SET_SRC (XVECEXP (op, 0, 0)));

  if (dest_regno != VRSAVE_REGNO
      && src_regno != VRSAVE_REGNO)
    return 0;

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != CLOBBER
	  && GET_CODE (elt) != SET)
	return 0;
    }

  return 1;
}

/* Return 1 for an PARALLEL suitable for mtcrf.  */

int
mtcrf_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  int i;
  rtx src_reg;

  /* Perform a quick check so we don't blow up below.  */
  if (count < 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC
      || XVECLEN (SET_SRC (XVECEXP (op, 0, 0)), 0) != 2)
    return 0;
  src_reg = XVECEXP (SET_SRC (XVECEXP (op, 0, 0)), 0, 0);
  
  if (GET_CODE (src_reg) != REG
      || GET_MODE (src_reg) != SImode
      || ! INT_REGNO_P (REGNO (src_reg)))
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx exp = XVECEXP (op, 0, i);
      rtx unspec;
      int maskval;
      
      if (GET_CODE (exp) != SET
	  || GET_CODE (SET_DEST (exp)) != REG
	  || GET_MODE (SET_DEST (exp)) != CCmode
	  || ! CR_REGNO_P (REGNO (SET_DEST (exp))))
	return 0;
      unspec = SET_SRC (exp);
      maskval = 1 << (MAX_CR_REGNO - REGNO (SET_DEST (exp)));
      
      if (GET_CODE (unspec) != UNSPEC
	  || XINT (unspec, 1) != 20
	  || XVECLEN (unspec, 0) != 2
	  || XVECEXP (unspec, 0, 0) != src_reg
	  || GET_CODE (XVECEXP (unspec, 0, 1)) != CONST_INT
	  || INTVAL (XVECEXP (unspec, 0, 1)) != maskval)
	return 0;
    }
  return 1;
}

/* Return 1 for an PARALLEL suitable for lmw.  */

int
lmw_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  unsigned int base_regno;
  HOST_WIDE_INT offset;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  if (dest_regno > 31
      || count != 32 - (int) dest_regno)
    return 0;

  if (LEGITIMATE_INDIRECT_ADDRESS_P (src_addr, 0))
    {
      offset = 0;
      base_regno = REGNO (src_addr);
      if (base_regno == 0)
	return 0;
    }
  else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, src_addr, 0))
    {
      offset = INTVAL (XEXP (src_addr, 1));
      base_regno = REGNO (XEXP (src_addr, 0));
    }
  else
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      rtx newaddr;
      rtx addr_reg;
      HOST_WIDE_INT newoffset;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode)
	return 0;
      newaddr = XEXP (SET_SRC (elt), 0);
      if (LEGITIMATE_INDIRECT_ADDRESS_P (newaddr, 0))
	{
	  newoffset = 0;
	  addr_reg = newaddr;
	}
      else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, newaddr, 0))
	{
	  addr_reg = XEXP (newaddr, 0);
	  newoffset = INTVAL (XEXP (newaddr, 1));
	}
      else
	return 0;
      if (REGNO (addr_reg) != base_regno
	  || newoffset != offset + 4 * i)
	return 0;
    }

  return 1;
}

/* Return 1 for an PARALLEL suitable for stmw.  */

int
stmw_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  unsigned int src_regno;
  rtx dest_addr;
  unsigned int base_regno;
  HOST_WIDE_INT offset;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  if (src_regno > 31
      || count != 32 - (int) src_regno)
    return 0;

  if (LEGITIMATE_INDIRECT_ADDRESS_P (dest_addr, 0))
    {
      offset = 0;
      base_regno = REGNO (dest_addr);
      if (base_regno == 0)
	return 0;
    }
  else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, dest_addr, 0))
    {
      offset = INTVAL (XEXP (dest_addr, 1));
      base_regno = REGNO (XEXP (dest_addr, 0));
    }
  else
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      rtx newaddr;
      rtx addr_reg;
      HOST_WIDE_INT newoffset;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode)
	return 0;
      newaddr = XEXP (SET_DEST (elt), 0);
      if (LEGITIMATE_INDIRECT_ADDRESS_P (newaddr, 0))
	{
	  newoffset = 0;
	  addr_reg = newaddr;
	}
      else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, newaddr, 0))
	{
	  addr_reg = XEXP (newaddr, 0);
	  newoffset = INTVAL (XEXP (newaddr, 1));
	}
      else
	return 0;
      if (REGNO (addr_reg) != base_regno
	  || newoffset != offset + 4 * i)
	return 0;
    }

  return 1;
}

/* A validation routine: say whether CODE, a condition code, and MODE
   match.  The other alternatives either don't make sense or should
   never be generated.  */

static void
validate_condition_mode (code, mode)
     enum rtx_code code;
     enum machine_mode mode;
{
  if (GET_RTX_CLASS (code) != '<' 
      || GET_MODE_CLASS (mode) != MODE_CC)
    abort ();

  /* These don't make sense.  */
  if ((code == GT || code == LT || code == GE || code == LE)
      && mode == CCUNSmode)
    abort ();

  if ((code == GTU || code == LTU || code == GEU || code == LEU)
      && mode != CCUNSmode)
    abort ();

  if (mode != CCFPmode
      && (code == ORDERED || code == UNORDERED
	  || code == UNEQ || code == LTGT
	  || code == UNGT || code == UNLT
	  || code == UNGE || code == UNLE))
    abort ();
  
  /* These should never be generated except for 
     flag_unsafe_math_optimizations.  */
  if (mode == CCFPmode
      && ! flag_unsafe_math_optimizations
      && (code == LE || code == GE
	  || code == UNEQ || code == LTGT
	  || code == UNGT || code == UNLT))
    abort ();

  /* These are invalid; the information is not there.  */
  if (mode == CCEQmode 
      && code != EQ && code != NE)
    abort ();
}

/* Return 1 if OP is a comparison operation that is valid for a branch insn.
   We only check the opcode against the mode of the CC value here.  */

int
branch_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  cc_mode = GET_MODE (XEXP (op, 0));
  if (GET_MODE_CLASS (cc_mode) != MODE_CC)
    return 0;

  validate_condition_mode (code, cc_mode);

  return 1;
}

/* Return 1 if OP is a comparison operation that is valid for a branch
   insn and which is true if the corresponding bit in the CC register
   is set.  */

int
branch_positive_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  enum rtx_code code;

  if (! branch_comparison_operator (op, mode))
    return 0;

  code = GET_CODE (op);
  return (code == EQ || code == LT || code == GT
	  || code == LTU || code == GTU
	  || code == UNORDERED);
}

/* Return 1 if OP is a comparison operation that is valid for an scc insn.
   We check the opcode against the mode of the CC value and disallow EQ or
   NE comparisons for integers.  */

int
scc_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  cc_mode = GET_MODE (XEXP (op, 0));
  if (GET_MODE_CLASS (cc_mode) != MODE_CC)
    return 0;

  validate_condition_mode (code, cc_mode);

  if (code == NE && cc_mode != CCFPmode)
    return 0;

  return 1;
}

int
trap_comparison_operator (op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  return GET_RTX_CLASS (GET_CODE (op)) == '<';
}

int
boolean_operator (op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);
  return (code == AND || code == IOR || code == XOR);
}

int
boolean_or_operator (op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);
  return (code == IOR || code == XOR);
}

int
min_max_operator (op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);
  return (code == SMIN || code == SMAX || code == UMIN || code == UMAX);
}

/* Return 1 if ANDOP is a mask that has no bits on that are not in the
   mask required to convert the result of a rotate insn into a shift
   left insn of SHIFTOP bits.  Both are known to be SImode CONST_INT.  */

int
includes_lshift_p (shiftop, andop)
     rtx shiftop;
     rtx andop;
{
  unsigned HOST_WIDE_INT shift_mask = ~(unsigned HOST_WIDE_INT) 0;

  shift_mask <<= INTVAL (shiftop);

  return (INTVAL (andop) & 0xffffffff & ~shift_mask) == 0;
}

/* Similar, but for right shift.  */

int
includes_rshift_p (shiftop, andop)
     rtx shiftop;
     rtx andop;
{
  unsigned HOST_WIDE_INT shift_mask = ~(unsigned HOST_WIDE_INT) 0;

  shift_mask >>= INTVAL (shiftop);

  return (INTVAL (andop) & 0xffffffff & ~shift_mask) == 0;
}

/* Return 1 if ANDOP is a mask suitable for use with an rldic insn
   to perform a left shift.  It must have exactly SHIFTOP least
   signifigant 0's, then one or more 1's, then zero or more 0's.  */

int
includes_rldic_lshift_p (shiftop, andop)
     rtx shiftop;
     rtx andop;
{
  if (GET_CODE (andop) == CONST_INT)
    {
      HOST_WIDE_INT c, lsb, shift_mask;

      c = INTVAL (andop);
      if (c == 0 || c == ~0)
	return 0;

      shift_mask = ~0;
      shift_mask <<= INTVAL (shiftop);

      /* Find the least signifigant one bit.  */
      lsb = c & -c;

      /* It must coincide with the LSB of the shift mask.  */
      if (-lsb != shift_mask)
	return 0;

      /* Invert to look for the next transition (if any).  */
      c = ~c;

      /* Remove the low group of ones (originally low group of zeros).  */
      c &= -lsb;

      /* Again find the lsb, and check we have all 1's above.  */
      lsb = c & -c;
      return c == -lsb;
    }
  else if (GET_CODE (andop) == CONST_DOUBLE
	   && (GET_MODE (andop) == VOIDmode || GET_MODE (andop) == DImode))
    {
      HOST_WIDE_INT low, high, lsb;
      HOST_WIDE_INT shift_mask_low, shift_mask_high;

      low = CONST_DOUBLE_LOW (andop);
      if (HOST_BITS_PER_WIDE_INT < 64)
	high = CONST_DOUBLE_HIGH (andop);

      if ((low == 0 && (HOST_BITS_PER_WIDE_INT >= 64 || high == 0))
	  || (low == ~0 && (HOST_BITS_PER_WIDE_INT >= 64 || high == ~0)))
	return 0;

      if (HOST_BITS_PER_WIDE_INT < 64 && low == 0)
	{
	  shift_mask_high = ~0;
	  if (INTVAL (shiftop) > 32)
	    shift_mask_high <<= INTVAL (shiftop) - 32;

	  lsb = high & -high;

	  if (-lsb != shift_mask_high || INTVAL (shiftop) < 32)
	    return 0;

	  high = ~high;
	  high &= -lsb;

	  lsb = high & -high;
	  return high == -lsb;
	}

      shift_mask_low = ~0;
      shift_mask_low <<= INTVAL (shiftop);

      lsb = low & -low;

      if (-lsb != shift_mask_low)
	return 0;

      if (HOST_BITS_PER_WIDE_INT < 64)
	high = ~high;
      low = ~low;
      low &= -lsb;

      if (HOST_BITS_PER_WIDE_INT < 64 && low == 0)
	{
	  lsb = high & -high;
	  return high == -lsb;
	}

      lsb = low & -low;
      return low == -lsb && (HOST_BITS_PER_WIDE_INT >= 64 || high == ~0);
    }
  else
    return 0;
}

/* Return 1 if ANDOP is a mask suitable for use with an rldicr insn
   to perform a left shift.  It must have SHIFTOP or more least
   signifigant 0's, with the remainder of the word 1's.  */

int
includes_rldicr_lshift_p (shiftop, andop)
     rtx shiftop;
     rtx andop;
{
  if (GET_CODE (andop) == CONST_INT)
    {
      HOST_WIDE_INT c, lsb, shift_mask;

      shift_mask = ~0;
      shift_mask <<= INTVAL (shiftop);
      c = INTVAL (andop);

      /* Find the least signifigant one bit.  */
      lsb = c & -c;

      /* It must be covered by the shift mask.
	 This test also rejects c == 0.  */
      if ((lsb & shift_mask) == 0)
	return 0;

      /* Check we have all 1's above the transition, and reject all 1's.  */
      return c == -lsb && lsb != 1;
    }
  else if (GET_CODE (andop) == CONST_DOUBLE
	   && (GET_MODE (andop) == VOIDmode || GET_MODE (andop) == DImode))
    {
      HOST_WIDE_INT low, lsb, shift_mask_low;

      low = CONST_DOUBLE_LOW (andop);

      if (HOST_BITS_PER_WIDE_INT < 64)
	{
	  HOST_WIDE_INT high, shift_mask_high;

	  high = CONST_DOUBLE_HIGH (andop);

	  if (low == 0)
	    {
	      shift_mask_high = ~0;
	      if (INTVAL (shiftop) > 32)
		shift_mask_high <<= INTVAL (shiftop) - 32;

	      lsb = high & -high;

	      if ((lsb & shift_mask_high) == 0)
		return 0;

	      return high == -lsb;
	    }
	  if (high != ~0)
	    return 0;
	}

      shift_mask_low = ~0;
      shift_mask_low <<= INTVAL (shiftop);

      lsb = low & -low;

      if ((lsb & shift_mask_low) == 0)
	return 0;

      return low == -lsb && lsb != 1;
    }
  else
    return 0;
}

/* Return 1 if REGNO (reg1) == REGNO (reg2) - 1 making them candidates
   for lfq and stfq insns.

   Note reg1 and reg2 *must* be hard registers.  To be sure we will
   abort if we are passed pseudo registers.  */

int
registers_ok_for_quad_peep (reg1, reg2)
     rtx reg1, reg2;
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG) 
    return 0;

  return (REGNO (reg1) == REGNO (reg2) - 1);
}

/* Return 1 if addr1 and addr2 are suitable for lfq or stfq insn.
   addr1 and addr2 must be in consecutive memory locations
   (addr2 == addr1 + 8).  */

int
addrs_ok_for_quad_peep (addr1, addr2)
     rtx addr1;
     rtx addr2;
{
  unsigned int reg1;
  int offset1;

  /* Extract an offset (if used) from the first addr.  */
  if (GET_CODE (addr1) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (GET_CODE (XEXP (addr1, 0)) != REG)
	return 0;
      else
	{
          reg1 = REGNO (XEXP (addr1, 0));
	  /* The offset must be constant!  */
	  if (GET_CODE (XEXP (addr1, 1)) != CONST_INT)
            return 0;
          offset1 = INTVAL (XEXP (addr1, 1));
	}
    }
  else if (GET_CODE (addr1) != REG)
    return 0;
  else
    {
      reg1 = REGNO (addr1);
      /* This was a simple (mem (reg)) expression.  Offset is 0.  */
      offset1 = 0;
    }

  /* Make sure the second address is a (mem (plus (reg) (const_int))).  */
  if (GET_CODE (addr2) != PLUS)
    return 0;

  if (GET_CODE (XEXP (addr2, 0)) != REG
      || GET_CODE (XEXP (addr2, 1)) != CONST_INT)
    return 0;

  if (reg1 != REGNO (XEXP (addr2, 0)))
    return 0;

  /* The offset for the second addr must be 8 more than the first addr.  */
  if (INTVAL (XEXP (addr2, 1)) != offset1 + 8)
    return 0;

  /* All the tests passed.  addr1 and addr2 are valid for lfq or stfq
     instructions.  */
  return 1;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx in;
{
  int regno;

  if (TARGET_ELF || (DEFAULT_ABI == ABI_DARWIN && flag_pic))
    {
      /* We cannot copy a symbolic operand directly into anything
         other than BASE_REGS for TARGET_ELF.  So indicate that a
         register from BASE_REGS is needed as an intermediate
         register.
         
	 On Darwin, pic addresses require a load from memory, which
	 needs a base register.  */
      if (class != BASE_REGS
          && (GET_CODE (in) == SYMBOL_REF
              || GET_CODE (in) == HIGH
              || GET_CODE (in) == LABEL_REF
              || GET_CODE (in) == CONST))
        return BASE_REGS;
    }

  if (GET_CODE (in) == REG)
    {
      regno = REGNO (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  regno = true_regnum (in);
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    regno = -1;
	}
    }
  else if (GET_CODE (in) == SUBREG)
    {
      regno = true_regnum (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = -1;
    }
  else
    regno = -1;

  /* We can place anything into GENERAL_REGS and can put GENERAL_REGS
     into anything.  */
  if (class == GENERAL_REGS || class == BASE_REGS
      || (regno >= 0 && INT_REGNO_P (regno)))
    return NO_REGS;

  /* Constants, memory, and FP registers can go into FP registers.  */
  if ((regno == -1 || FP_REGNO_P (regno))
      && (class == FLOAT_REGS || class == NON_SPECIAL_REGS))
    return NO_REGS;

  /* Memory, and AltiVec registers can go into AltiVec registers.  */
  if ((regno == -1 || ALTIVEC_REGNO_P (regno))
      && class == ALTIVEC_REGS)
    return NO_REGS;

  /* We can copy among the CR registers.  */
  if ((class == CR_REGS || class == CR0_REGS)
      && regno >= 0 && CR_REGNO_P (regno))
    return NO_REGS;

  /* Otherwise, we need GENERAL_REGS.  */
  return GENERAL_REGS;
}

/* Given a comparison operation, return the bit number in CCR to test.  We
   know this is a valid comparison.  

   SCC_P is 1 if this is for an scc.  That means that %D will have been
   used instead of %C, so the bits will be in different places.

   Return -1 if OP isn't a valid comparison for some reason.  */

int
ccr_bit (op, scc_p)
     rtx op;
     int scc_p;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;
  int cc_regnum;
  int base_bit;
  rtx reg;

  if (GET_RTX_CLASS (code) != '<')
    return -1;

  reg = XEXP (op, 0);

  if (GET_CODE (reg) != REG
      || ! CR_REGNO_P (REGNO (reg)))
    abort ();

  cc_mode = GET_MODE (reg);
  cc_regnum = REGNO (reg);
  base_bit = 4 * (cc_regnum - CR0_REGNO);

  validate_condition_mode (code, cc_mode);

  switch (code)
    {
    case NE:
      return scc_p ? base_bit + 3 : base_bit + 2;
    case EQ:
      return base_bit + 2;
    case GT:  case GTU:  case UNLE:
      return base_bit + 1;
    case LT:  case LTU:  case UNGE:
      return base_bit;
    case ORDERED:  case UNORDERED:
      return base_bit + 3;

    case GE:  case GEU:
      /* If scc, we will have done a cror to put the bit in the
	 unordered position.  So test that bit.  For integer, this is ! LT
	 unless this is an scc insn.  */
      return scc_p ? base_bit + 3 : base_bit;

    case LE:  case LEU:
      return scc_p ? base_bit + 3 : base_bit + 1;

    default:
      abort ();
    }
}

/* Return the GOT register.  */

struct rtx_def *
rs6000_got_register (value)
     rtx value ATTRIBUTE_UNUSED;
{
  /* The second flow pass currently (June 1999) can't update
     regs_ever_live without disturbing other parts of the compiler, so
     update it here to make the prolog/epilogue code happy.  */
  if (no_new_pseudos && ! regs_ever_live[RS6000_PIC_OFFSET_TABLE_REGNUM])
    regs_ever_live[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  current_function_uses_pic_offset_table = 1;

  return pic_offset_table_rtx;
}

/* Functions to init, mark and free struct machine_function.
   These will be called, via pointer variables,
   from push_function_context and pop_function_context.  */

static void
rs6000_init_machine_status (p)
     struct function *p;
{
  p->machine = (machine_function *) xcalloc (1, sizeof (machine_function));
}

static void
rs6000_free_machine_status (p)
     struct function *p;
{
  if (p->machine == NULL)
    return;

  free (p->machine);
  p->machine = NULL;
}


/* Print an operand.  Recognize special options, documented below.  */

#if TARGET_ELF
#define SMALL_DATA_RELOC ((rs6000_sdata == SDATA_EABI) ? "sda21" : "sdarel")
#define SMALL_DATA_REG ((rs6000_sdata == SDATA_EABI) ? 0 : 13)
#else
#define SMALL_DATA_RELOC "sda21"
#define SMALL_DATA_REG 0
#endif

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    int code;
{
  int i;
  HOST_WIDE_INT val;

  /* These macros test for integers and extract the low-order bits.  */
#define INT_P(X)  \
((GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE)	\
 && GET_MODE (X) == VOIDmode)

#define INT_LOWPART(X) \
  (GET_CODE (X) == CONST_INT ? INTVAL (X) : CONST_DOUBLE_LOW (X))

  switch (code)
    {
    case '.':
      /* Write out an instruction after the call which may be replaced
	 with glue code by the loader.  This depends on the AIX version.  */
      asm_fprintf (file, RS6000_CALL_GLUE);
      return;

      /* %a is output_address.  */

    case 'A':
      /* If X is a constant integer whose low-order 5 bits are zero,
	 write 'l'.  Otherwise, write 'r'.  This is a kludge to fix a bug
	 in the AIX assembler where "sri" with a zero shift count
	 writes a trash instruction.  */
      if (GET_CODE (x) == CONST_INT && (INTVAL (x) & 31) == 0)
	putc ('l', file);
      else
	putc ('r', file);
      return;

    case 'b':
      /* If constant, low-order 16 bits of constant, unsigned.
	 Otherwise, write normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 0xffff);
      else
	print_operand (file, x, 0);
      return;

    case 'B':
      /* If the low-order bit is zero, write 'r'; otherwise, write 'l'
	 for 64-bit mask direction.  */
      putc (((INT_LOWPART(x) & 1) == 0 ? 'r' : 'l'), file);
      return;

      /* %c is output_addr_const if a CONSTANT_ADDRESS_P, otherwise
	 output_operand.  */

    case 'D':
      /* There used to be a comment for 'C' reading "This is an
	   optional cror needed for certain floating-point
	   comparisons.  Otherwise write nothing."  */

      /* Similar, except that this is for an scc, so we must be able to
	 encode the test in a single bit that is one.  We do the above
	 for any LE, GE, GEU, or LEU and invert the bit for NE.  */
      if (GET_CODE (x) == LE || GET_CODE (x) == GE
	  || GET_CODE (x) == LEU || GET_CODE (x) == GEU)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - CR0_REGNO);

	  fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2,
		   base_bit + (GET_CODE (x) == GE || GET_CODE (x) == GEU));
	}

      else if (GET_CODE (x) == NE)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - CR0_REGNO);

	  fprintf (file, "crnor %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2, base_bit + 2);
	}
      return;

    case 'E':
      /* X is a CR register.  Print the number of the EQ bit of the CR */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%E value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO) + 2);
      return;

    case 'f':
      /* X is a CR register.  Print the shift count needed to move it
	 to the high-order four bits.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%f value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'F':
      /* Similar, but print the count for the rotate in the opposite
	 direction.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%F value");
      else
	fprintf (file, "%d", 32 - 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'G':
      /* X is a constant integer.  If it is negative, print "m",
	 otherwise print "z".  This is to make a aze or ame insn.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%G value");
      else if (INTVAL (x) >= 0)
	putc ('z', file);
      else
	putc ('m', file);
      return;

    case 'h':
      /* If constant, output low-order five bits.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 31);
      else
	print_operand (file, x, 0);
      return;

    case 'H':
      /* If constant, output low-order six bits.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 63);
      else
	print_operand (file, x, 0);
      return;

    case 'I':
      /* Print `i' if this is a constant, else nothing.  */
      if (INT_P (x))
	putc ('i', file);
      return;

    case 'j':
      /* Write the bit number in CCR for jump.  */
      i = ccr_bit (x, 0);
      if (i == -1)
	output_operand_lossage ("invalid %%j code");
      else
	fprintf (file, "%d", i);
      return;

    case 'J':
      /* Similar, but add one for shift count in rlinm for scc and pass
	 scc flag to `ccr_bit'.  */
      i = ccr_bit (x, 1);
      if (i == -1)
	output_operand_lossage ("invalid %%J code");
      else
	/* If we want bit 31, write a shift count of zero, not 32.  */
	fprintf (file, "%d", i == 31 ? 0 : i + 1);
      return;

    case 'k':
      /* X must be a constant.  Write the 1's complement of the
	 constant.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%k value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, ~ INT_LOWPART (x));
      return;

    case 'K':
      /* X must be a symbolic constant on ELF.  Write an
	 expression suitable for an 'addi' that adds in the low 16
	 bits of the MEM.  */
      if (GET_CODE (x) != CONST)
	{
	  print_operand_address (file, x);
	  fputs ("@l", file);
	}
      else
	{
	  if (GET_CODE (XEXP (x, 0)) != PLUS
	      || (GET_CODE (XEXP (XEXP (x, 0), 0)) != SYMBOL_REF
		  && GET_CODE (XEXP (XEXP (x, 0), 0)) != LABEL_REF)
	      || GET_CODE (XEXP (XEXP (x, 0), 1)) != CONST_INT)
	    output_operand_lossage ("invalid %%K value");
	  print_operand_address (file, XEXP (XEXP (x, 0), 0));
	  fputs ("@l", file);
	  print_operand (file, XEXP (XEXP (x, 0), 1), 0);
	}
      return;

      /* %l is output_asm_label.  */

    case 'L':
      /* Write second word of DImode or DFmode reference.  Works on register
	 or non-indexed memory only.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 1]);
      else if (GET_CODE (x) == MEM)
	{
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of word.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0),
					   UNITS_PER_WORD));
	  else
	    output_address (XEXP (adjust_address_nv (x, SImode,
						     UNITS_PER_WORD),
				  0));

	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;
			    
    case 'm':
      /* MB value for a mask operand.  */
      if (! mask_operand (x, SImode))
	output_operand_lossage ("invalid %%m value");

      val = INT_LOWPART (x);

      /* If the high bit is set and the low bit is not, the value is zero.
	 If the high bit is zero, the value is the first 1 bit we find from
	 the left.  */
      if ((val & 0x80000000) && ((val & 1) == 0))
	{
	  putc ('0', file);
	  return;
	}
      else if ((val & 0x80000000) == 0)
	{
	  for (i = 1; i < 32; i++)
	    if ((val <<= 1) & 0x80000000)
	      break;
	  fprintf (file, "%d", i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the right.  The result is its
	 number plus 1. We know the low-order bit is one.  */
      for (i = 0; i < 32; i++)
	if (((val >>= 1) & 1) == 0)
	  break;

      /* If we ended in ...01, i would be 0.  The correct value is 31, so
	 we want 31 - i.  */
      fprintf (file, "%d", 31 - i);
      return;

    case 'M':
      /* ME value for a mask operand.  */
      if (! mask_operand (x, SImode))
	output_operand_lossage ("invalid %%M value");

      val = INT_LOWPART (x);

      /* If the low bit is set and the high bit is not, the value is 31.
	 If the low bit is zero, the value is the first 1 bit we find from
	 the right.  */
      if ((val & 1) && ((val & 0x80000000) == 0))
	{
	  fputs ("31", file);
	  return;
	}
      else if ((val & 1) == 0)
	{
	  for (i = 0; i < 32; i++)
	    if ((val >>= 1) & 1)
	      break;

	  /* If we had ....10, i would be 0.  The result should be
	     30, so we need 30 - i.  */
	  fprintf (file, "%d", 30 - i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the left.  The result is its
	 number minus 1. We know the high-order bit is one.  */
      for (i = 0; i < 32; i++)
	if (((val <<= 1) & 0x80000000) == 0)
	  break;

      fprintf (file, "%d", i);
      return;

      /* %n outputs the negative of its operand.  */

    case 'N':
      /* Write the number of elements in the vector times 4.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");
      else
	fprintf (file, "%d", XVECLEN (x, 0) * 4);
      return;

    case 'O':
      /* Similar, but subtract 1 first.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%O value");
      else
	fprintf (file, "%d", (XVECLEN (x, 0) - 1) * 4);
      return;

    case 'p':
      /* X is a CONST_INT that is a power of two.  Output the logarithm.  */
      if (! INT_P (x)
	  || INT_LOWPART (x) < 0
	  || (i = exact_log2 (INT_LOWPART (x))) < 0)
	output_operand_lossage ("invalid %%p value");
      else
	fprintf (file, "%d", i);
      return;

    case 'P':
      /* The operand must be an indirect memory reference.  The result
	 is the register number.  */
      if (GET_CODE (x) != MEM || GET_CODE (XEXP (x, 0)) != REG
	  || REGNO (XEXP (x, 0)) >= 32)
	output_operand_lossage ("invalid %%P value");
      else
	fprintf (file, "%d", REGNO (XEXP (x, 0)));
      return;

    case 'q':
      /* This outputs the logical code corresponding to a boolean
	 expression.  The expression may have one or both operands
	 negated (if one, only the first one).  For condition register
         logical operations, it will also treat the negated
         CR codes as NOTs, but not handle NOTs of them.  */
      {
	const char *const *t = 0;
	const char *s;
	enum rtx_code code = GET_CODE (x);
	static const char * const tbl[3][3] = {
	  { "and", "andc", "nor" },
	  { "or", "orc", "nand" },
	  { "xor", "eqv", "xor" } };

	if (code == AND)
	  t = tbl[0];
	else if (code == IOR)
	  t = tbl[1];
	else if (code == XOR)
	  t = tbl[2];
	else
	  output_operand_lossage ("invalid %%q value");

	if (GET_CODE (XEXP (x, 0)) != NOT)
	  s = t[0];
	else
	  {
	    if (GET_CODE (XEXP (x, 1)) == NOT)
	      s = t[2];
	    else
	      s = t[1];
	  }
	
	fputs (s, file);
      }
      return;

    case 'R':
      /* X is a CR register.  Print the mask for `mtcrf'.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%R value");
      else
	fprintf (file, "%d", 128 >> (REGNO (x) - CR0_REGNO));
      return;

    case 's':
      /* Low 5 bits of 32 - value */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%s value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, (32 - INT_LOWPART (x)) & 31);
      return;

    case 'S':
      /* PowerPC64 mask position.  All 0's and all 1's are excluded.
	 CONST_INT 32-bit mask is considered sign-extended so any
	 transition must occur within the CONST_INT, not on the boundary.  */
      if (! mask64_operand (x, DImode))
	output_operand_lossage ("invalid %%S value");

      val = INT_LOWPART (x);

      if (val & 1)      /* Clear Left */
	{
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT; i++)
	    if (!((val >>= 1) & 1))
	      break;

#if HOST_BITS_PER_WIDE_INT == 32
	  if (GET_CODE (x) == CONST_DOUBLE && i == 32)
	    {
	      val = CONST_DOUBLE_HIGH (x);

	      if (val == 0)
		--i;
	      else
		for (i = 32; i < 64; i++)
		  if (!((val >>= 1) & 1))
		    break;
	    }
#endif
	/* i = index of last set bit from right
	   mask begins at 63 - i from left */
	  if (i > 63)
	    output_operand_lossage ("%%S computed all 1's mask");

	  fprintf (file, "%d", 63 - i);
	  return;
	}
      else	/* Clear Right */
	{
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT; i++)
	    if ((val >>= 1) & 1)
	      break;

#if HOST_BITS_PER_WIDE_INT == 32
	if (GET_CODE (x) == CONST_DOUBLE && i == 32)
	  {
	    val = CONST_DOUBLE_HIGH (x);

	    if (val == (HOST_WIDE_INT) -1)
	      --i;
	    else
	      for (i = 32; i < 64; i++)
		if ((val >>= 1) & 1)
		  break;
	  }
#endif
	/* i = index of last clear bit from right
	   mask ends at 62 - i from left */
	  if (i > 62)
	    output_operand_lossage ("%%S computed all 0's mask");

	  fprintf (file, "%d", 62 - i);
	  return;
	}

    case 'T':
      /* Print the symbolic name of a branch target register.  */
      if (GET_CODE (x) != REG || (REGNO (x) != LINK_REGISTER_REGNUM
				  && REGNO (x) != COUNT_REGISTER_REGNUM))
	output_operand_lossage ("invalid %%T value");
      else if (REGNO (x) == LINK_REGISTER_REGNUM)
	fputs (TARGET_NEW_MNEMONICS ? "lr" : "r", file);
      else
	fputs ("ctr", file);
      return;

    case 'u':
      /* High-order 16 bits of constant for use in unsigned operand.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%u value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, 
		 (INT_LOWPART (x) >> 16) & 0xffff);
      return;

    case 'v':
      /* High-order 16 bits of constant for use in signed operand.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%v value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		 (INT_LOWPART (x) >> 16) & 0xffff);
      return;

    case 'U':
      /* Print `u' if this has an auto-increment or auto-decrement.  */
      if (GET_CODE (x) == MEM
	  && (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC))
	putc ('u', file);
      return;

    case 'V':
      /* Print the trap code for this operand.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("eq", file);   /* 4 */
	  break;
	case NE:
	  fputs ("ne", file);   /* 24 */
	  break;
	case LT:
	  fputs ("lt", file);   /* 16 */
	  break;
	case LE:
	  fputs ("le", file);   /* 20 */
	  break;
	case GT:
	  fputs ("gt", file);   /* 8 */
	  break;
	case GE:
	  fputs ("ge", file);   /* 12 */
	  break;
	case LTU:
	  fputs ("llt", file);  /* 2 */
	  break;
	case LEU:
	  fputs ("lle", file);  /* 6 */
	  break;
	case GTU:
	  fputs ("lgt", file);  /* 1 */
	  break;
	case GEU:
	  fputs ("lge", file);  /* 5 */
	  break;
	default:
	  abort ();
	}
      break;

    case 'w':
      /* If constant, low-order 16 bits of constant, signed.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, 
		 ((INT_LOWPART (x) & 0xffff) ^ 0x8000) - 0x8000);
      else
	print_operand (file, x, 0);
      return;

    case 'W':
      /* MB value for a PowerPC64 rldic operand.  */
      val = (GET_CODE (x) == CONST_INT
	     ? INTVAL (x) : CONST_DOUBLE_HIGH (x));

      if (val < 0)
	i = -1;
      else
	for (i = 0; i < HOST_BITS_PER_WIDE_INT; i++)
	  if ((val <<= 1) < 0)
	    break;

#if HOST_BITS_PER_WIDE_INT == 32
      if (GET_CODE (x) == CONST_INT && i >= 0)
	i += 32;  /* zero-extend high-part was all 0's */
      else if (GET_CODE (x) == CONST_DOUBLE && i == 32)
	{
	  val = CONST_DOUBLE_LOW (x);

	  if (val == 0)
	    abort ();
	  else if (val < 0)
	    --i;
	  else
	    for ( ; i < 64; i++)
	      if ((val <<= 1) < 0)
		break;
	}
#endif

      fprintf (file, "%d", i + 1);
      return;

    case 'X':
      if (GET_CODE (x) == MEM
	  && LEGITIMATE_INDEXED_ADDRESS_P (XEXP (x, 0), 0))
	putc ('x', file);
      return;

    case 'Y':
      /* Like 'L', for third word of TImode  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 2]);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 8));
	  else
	    output_address (XEXP (adjust_address_nv (x, SImode, 8), 0));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;
			    
    case 'z':
      /* X is a SYMBOL_REF.  Write out the name preceded by a
	 period and without any trailing data in brackets.  Used for function
	 names.  If we are configured for System V (or the embedded ABI) on
	 the PowerPC, do not emit the period, since those systems do not use
	 TOCs and the like.  */
      if (GET_CODE (x) != SYMBOL_REF)
	abort ();

      if (XSTR (x, 0)[0] != '.')
	{
	  switch (DEFAULT_ABI)
	    {
	    default:
	      abort ();

	    case ABI_AIX:
	      putc ('.', file);
	      break;

	    case ABI_V4:
	    case ABI_AIX_NODESC:
	    case ABI_DARWIN:
	      break;
	    }
	}
#if TARGET_AIX
      RS6000_OUTPUT_BASENAME (file, XSTR (x, 0));
#else
      assemble_name (file, XSTR (x, 0));
#endif
      return;

    case 'Z':
      /* Like 'L', for last word of TImode.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 3]);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 12));
	  else
	    output_address (XEXP (adjust_address_nv (x, SImode, 12), 0));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

      /* Print AltiVec memory operand.  */
    case 'y':
      {
	rtx tmp;

	if (GET_CODE (x) != MEM)
	  abort ();

	tmp = XEXP (x, 0);

	if (GET_CODE (tmp) == REG)
	  fprintf (file, "0,%s", reg_names[REGNO (tmp)]);
	else if (GET_CODE (tmp) == PLUS && GET_CODE (XEXP (tmp, 1)) == REG)
	  {
	    if (REGNO (XEXP (tmp, 0)) == 0)
	      fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (tmp, 1)) ],
		       reg_names[ REGNO (XEXP (tmp, 0)) ]);
	    else
	      fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (tmp, 0)) ],
		       reg_names[ REGNO (XEXP (tmp, 1)) ]);
	  }
	else
	  abort ();
	break;
      }
			    
    case 0:
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (GET_CODE (x) == MEM)
	{
	  /* We need to handle PRE_INC and PRE_DEC here, since we need to
	     know the width from the mode.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC)
	    fprintf (file, "%d(%s)", GET_MODE_SIZE (GET_MODE (x)),
		     reg_names[REGNO (XEXP (XEXP (x, 0), 0))]);
	  else if (GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fprintf (file, "%d(%s)", - GET_MODE_SIZE (GET_MODE (x)),
		     reg_names[REGNO (XEXP (XEXP (x, 0), 0))]);
	  else
	    output_address (XEXP (x, 0));
	}
      else
	output_addr_const (file, x);
      return;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* Print the address of an operand.  */

void
print_operand_address (file, x)
     FILE *file;
     rtx x;
{
  if (GET_CODE (x) == REG)
    fprintf (file, "0(%s)", reg_names[ REGNO (x) ]);
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST
	   || GET_CODE (x) == LABEL_REF)
    {
      output_addr_const (file, x);
      if (small_data_operand (x, GET_MODE (x)))
	fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		 reg_names[SMALL_DATA_REG]);
      else if (TARGET_TOC)
	abort ();
    }
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == REG)
    {
      if (REGNO (XEXP (x, 0)) == 0)
	fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (x, 1)) ],
		 reg_names[ REGNO (XEXP (x, 0)) ]);
      else
	fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (x, 0)) ],
		 reg_names[ REGNO (XEXP (x, 1)) ]);
    }
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (XEXP (x, 1)));
      fprintf (file, "(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#if TARGET_ELF
  else if (GET_CODE (x) == LO_SUM && GET_CODE (XEXP (x, 0)) == REG
           && CONSTANT_P (XEXP (x, 1)))
    {
      output_addr_const (file, XEXP (x, 1));
      fprintf (file, "@l(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#endif
#if TARGET_MACHO
  else if (GET_CODE (x) == LO_SUM && GET_CODE (XEXP (x, 0)) == REG
           && CONSTANT_P (XEXP (x, 1)))
    {
      fprintf (file, "lo16(");
      output_addr_const (file, XEXP (x, 1));
      fprintf (file, ")(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#endif
  else if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (x))
    {
      if (TARGET_AIX && (!TARGET_ELF || !TARGET_MINIMAL_TOC))
	{
	  rtx contains_minus = XEXP (x, 1);
	  rtx minus, symref;
	  const char *name;
	  
	  /* Find the (minus (sym) (toc)) buried in X, and temporarily
	     turn it into (sym) for output_addr_const.  */
	  while (GET_CODE (XEXP (contains_minus, 0)) != MINUS)
	    contains_minus = XEXP (contains_minus, 0);

	  minus = XEXP (contains_minus, 0);
	  symref = XEXP (minus, 0);
	  XEXP (contains_minus, 0) = symref;
	  if (TARGET_ELF)
	    {
	      char *newname;

	      name = XSTR (symref, 0);
	      newname = alloca (strlen (name) + sizeof ("@toc"));
	      strcpy (newname, name);
	      strcat (newname, "@toc");
	      XSTR (symref, 0) = newname;
	    }
	  output_addr_const (file, XEXP (x, 1));
	  if (TARGET_ELF)
	    XSTR (symref, 0) = name;
	  XEXP (contains_minus, 0) = minus;
	}
      else
	output_addr_const (file, XEXP (x, 1));

      fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
    }
  else
    abort ();
}

/* Target hook for assembling integer objects.  The powerpc version has
   to handle fixup entries for relocatable code if RELOCATABLE_NEEDS_FIXUP
   is defined.  It also needs to handle DI-mode objects on 64-bit
   targets.  */

static bool
rs6000_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
#ifdef RELOCATABLE_NEEDS_FIXUP
  /* Special handling for SI values.  */
  if (size == 4 && aligned_p)
    {
      extern int in_toc_section PARAMS ((void));
      static int recurse = 0;
      
      /* For -mrelocatable, we mark all addresses that need to be fixed up
	 in the .fixup section.  */
      if (TARGET_RELOCATABLE
	  && !in_toc_section ()
	  && !in_text_section ()
	  && !recurse
	  && GET_CODE (x) != CONST_INT
	  && GET_CODE (x) != CONST_DOUBLE
	  && CONSTANT_P (x))
	{
	  char buf[256];

	  recurse = 1;
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", fixuplabelno);
	  fixuplabelno++;
	  ASM_OUTPUT_LABEL (asm_out_file, buf);
	  fprintf (asm_out_file, "\t.long\t(");
	  output_addr_const (asm_out_file, x);
	  fprintf (asm_out_file, ")@fixup\n");
	  fprintf (asm_out_file, "\t.section\t\".fixup\",\"aw\"\n");
	  ASM_OUTPUT_ALIGN (asm_out_file, 2);
	  fprintf (asm_out_file, "\t.long\t");
	  assemble_name (asm_out_file, buf);
	  fprintf (asm_out_file, "\n\t.previous\n");
	  recurse = 0;
	  return true;
	}
      /* Remove initial .'s to turn a -mcall-aixdesc function
	 address into the address of the descriptor, not the function
	 itself.  */
      else if (GET_CODE (x) == SYMBOL_REF
	       && XSTR (x, 0)[0] == '.'
	       && DEFAULT_ABI == ABI_AIX)
	{
	  const char *name = XSTR (x, 0);
	  while (*name == '.')
	    name++;

	  fprintf (asm_out_file, "\t.long\t%s\n", name);
	  return true;
	}
    }
#endif /* RELOCATABLE_NEEDS_FIXUP */
  return default_assemble_integer (x, size, aligned_p);
}

enum rtx_code
rs6000_reverse_condition (mode, code)
     enum machine_mode mode;
     enum rtx_code code;
{
  /* Reversal of FP compares takes care -- an ordered compare
     becomes an unordered compare and vice versa.  */
  if (mode == CCFPmode && !flag_unsafe_math_optimizations)
    return reverse_condition_maybe_unordered (code);
  else
    return reverse_condition (code);
}

/* Generate a compare for CODE.  Return a brand-new rtx that
   represents the result of the compare.  */

static rtx
rs6000_generate_compare (code)
     enum rtx_code code;
{
  enum machine_mode comp_mode;
  rtx compare_result;

  if (rs6000_compare_fp_p)
    comp_mode = CCFPmode;
  else if (code == GTU || code == LTU
	  || code == GEU || code == LEU)
    comp_mode = CCUNSmode;
  else
    comp_mode = CCmode;

  /* First, the compare.  */
  compare_result = gen_reg_rtx (comp_mode);
  emit_insn (gen_rtx_SET (VOIDmode, compare_result,
			  gen_rtx_COMPARE (comp_mode,
					   rs6000_compare_op0, 
					   rs6000_compare_op1)));
  
  /* Some kinds of FP comparisons need an OR operation;
     except for flag_unsafe_math_optimizations we don't bother.  */
  if (rs6000_compare_fp_p
      && ! flag_unsafe_math_optimizations
      && (code == LE || code == GE
	  || code == UNEQ || code == LTGT
	  || code == UNGT || code == UNLT))
    {
      enum rtx_code or1, or2;
      rtx or1_rtx, or2_rtx, compare2_rtx;
      rtx or_result = gen_reg_rtx (CCEQmode);
      
      switch (code)
	{
	case LE: or1 = LT;  or2 = EQ;  break;
	case GE: or1 = GT;  or2 = EQ;  break;
	case UNEQ: or1 = UNORDERED;  or2 = EQ;  break;
	case LTGT: or1 = LT;  or2 = GT;  break;
	case UNGT: or1 = UNORDERED;  or2 = GT;  break;
	case UNLT: or1 = UNORDERED;  or2 = LT;  break;
	default:  abort ();
	}
      validate_condition_mode (or1, comp_mode);
      validate_condition_mode (or2, comp_mode);
      or1_rtx = gen_rtx (or1, SImode, compare_result, const0_rtx);
      or2_rtx = gen_rtx (or2, SImode, compare_result, const0_rtx);
      compare2_rtx = gen_rtx_COMPARE (CCEQmode,
				      gen_rtx_IOR (SImode, or1_rtx, or2_rtx),
				      const_true_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, or_result, compare2_rtx));

      compare_result = or_result;
      code = EQ;
    }

  validate_condition_mode (code, GET_MODE (compare_result));
  
  return gen_rtx (code, VOIDmode, compare_result, const0_rtx);
}


/* Emit the RTL for an sCOND pattern.  */

void
rs6000_emit_sCOND (code, result)
     enum rtx_code code;
     rtx result;
{
  rtx condition_rtx;
  enum machine_mode op_mode;

  condition_rtx = rs6000_generate_compare (code);

  op_mode = GET_MODE (rs6000_compare_op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (rs6000_compare_op1);

  if (TARGET_POWERPC64 && (op_mode == DImode || rs6000_compare_fp_p))
    {
      PUT_MODE (condition_rtx, DImode);
      convert_move (result, condition_rtx, 0);
    }
  else
    {
      PUT_MODE (condition_rtx, SImode);
      emit_insn (gen_rtx_SET (VOIDmode, result, condition_rtx));
    }
}

/* Emit a branch of kind CODE to location LOC.  */

void
rs6000_emit_cbranch (code, loc)
     enum rtx_code code;
     rtx loc;
{
  rtx condition_rtx, loc_ref;

  condition_rtx = rs6000_generate_compare (code);
  loc_ref = gen_rtx_LABEL_REF (VOIDmode, loc);
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode, condition_rtx,
						     loc_ref, pc_rtx)));
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label, or -1 if the branch is really a
   conditional return.  

   OP is the conditional expression.  XEXP (OP, 0) is assumed to be a
   condition code register and its mode specifies what kind of
   comparison we made.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   INSN is the insn.  */

char *
output_cbranch (op, label, reversed, insn)
     rtx op;
     const char * label;
     int reversed;
     rtx insn;
{
  static char string[64];
  enum rtx_code code = GET_CODE (op);
  rtx cc_reg = XEXP (op, 0);
  enum machine_mode mode = GET_MODE (cc_reg);
  int cc_regno = REGNO (cc_reg) - CR0_REGNO;
  int need_longbranch = label != NULL && get_attr_length (insn) == 8;
  int really_reversed = reversed ^ need_longbranch;
  char *s = string;
  const char *ccode;
  const char *pred;
  rtx note;

  validate_condition_mode (code, mode);

  /* Work out which way this really branches.  We could use
     reverse_condition_maybe_unordered here always but this
     makes the resulting assembler clearer.  */
  if (really_reversed)
    {
      /* Reversal of FP compares takes care -- an ordered compare
	 becomes an unordered compare and vice versa.  */
      if (mode == CCFPmode)
	code = reverse_condition_maybe_unordered (code);
      else
	code = reverse_condition (code);
    }

  switch (code)
    {
      /* Not all of these are actually distinct opcodes, but
	 we distinguish them for clarity of the resulting assembler.  */
    case NE: case LTGT:
      ccode = "ne"; break;
    case EQ: case UNEQ:
      ccode = "eq"; break;
    case GE: case GEU: 
      ccode = "ge"; break;
    case GT: case GTU: case UNGT: 
      ccode = "gt"; break;
    case LE: case LEU: 
      ccode = "le"; break;
    case LT: case LTU: case UNLT: 
      ccode = "lt"; break;
    case UNORDERED: ccode = "un"; break;
    case ORDERED: ccode = "nu"; break;
    case UNGE: ccode = "nl"; break;
    case UNLE: ccode = "ng"; break;
    default:
      abort ();
    }
  
  /* Maybe we have a guess as to how likely the branch is.  
     The old mnemonics don't have a way to specify this information.  */
  note = find_reg_note (insn, REG_BR_PROB, NULL_RTX);
  if (note != NULL_RTX)
    {
      /* PROB is the difference from 50%.  */
      int prob = INTVAL (XEXP (note, 0)) - REG_BR_PROB_BASE / 2;
      
      /* For branches that are very close to 50%, assume not-taken.  */
      if (abs (prob) > REG_BR_PROB_BASE / 20
	  && ((prob > 0) ^ need_longbranch))
	pred = "+";
      else
	pred = "-";
    }
  else
    pred = "";

  if (label == NULL)
    s += sprintf (s, "{b%sr|b%slr%s} ", ccode, ccode, pred);
  else
    s += sprintf (s, "{b%s|b%s%s} ", ccode, ccode, pred);

  /* We need to escape any '%' characters in the reg_names string.
     Assume they'd only be the first character...  */
  if (reg_names[cc_regno + CR0_REGNO][0] == '%')
    *s++ = '%';
  s += sprintf (s, "%s", reg_names[cc_regno + CR0_REGNO]);

  if (label != NULL)
    {
      /* If the branch distance was too far, we may have to use an
	 unconditional branch to go the distance.  */
      if (need_longbranch)
	s += sprintf (s, ",$+8\n\tb %s", label);
      else
	s += sprintf (s, ",%s", label);
    }

  return string;
}

/* Emit a conditional move: move TRUE_COND to DEST if OP of the
   operands of the last comparison is nonzero/true, FALSE_COND if it
   is zero/false.  Return 0 if the hardware has no such operation.  */

int
rs6000_emit_cmove (dest, op, true_cond, false_cond)
     rtx dest;
     rtx op;
     rtx true_cond;
     rtx false_cond;
{
  enum rtx_code code = GET_CODE (op);
  rtx op0 = rs6000_compare_op0;
  rtx op1 = rs6000_compare_op1;
  REAL_VALUE_TYPE c1;
  enum machine_mode compare_mode = GET_MODE (op0);
  enum machine_mode result_mode = GET_MODE (dest);
  rtx temp;

  /* These modes should always match. */
  if (GET_MODE (op1) != compare_mode)
    return 0;
  if (GET_MODE (true_cond) != result_mode)
    return 0;
  if (GET_MODE (false_cond) != result_mode)
    return 0;

  /* First, work out if the hardware can do this at all, or
     if it's too slow...  */
  /* If the comparison is an integer one, since we only have fsel
     it'll be cheaper to use a branch.  */
  if (! rs6000_compare_fp_p)
    return 0;

  /* Eliminate half of the comparisons by switching operands, this
     makes the remaining code simpler.  */
  if (code == UNLT || code == UNGT || code == UNORDERED || code == NE
      || code == LTGT || code == LT)
    {
      code = reverse_condition_maybe_unordered (code);
      temp = true_cond;
      true_cond = false_cond;
      false_cond = temp;
    }

  /* UNEQ and LTGT take four instructions for a comparison with zero,
     it'll probably be faster to use a branch here too.  */
  if (code == UNEQ)
    return 0;
  
  if (GET_CODE (op1) == CONST_DOUBLE)
    REAL_VALUE_FROM_CONST_DOUBLE (c1, op1);
    
  /* We're going to try to implement comparions by performing
     a subtract, then comparing against zero.  Unfortunately,
     Inf - Inf is NaN which is not zero, and so if we don't
     know that the the operand is finite and the comparison
     would treat EQ different to UNORDERED, we can't do it.  */
  if (! flag_unsafe_math_optimizations
      && code != GT && code != UNGE
      && (GET_CODE (op1) != CONST_DOUBLE || target_isinf (c1))
      /* Constructs of the form (a OP b ? a : b) are safe.  */
      && ((! rtx_equal_p (op0, false_cond) && ! rtx_equal_p (op1, false_cond))
	  || (! rtx_equal_p (op0, true_cond) 
	      && ! rtx_equal_p (op1, true_cond))))
    return 0;
  /* At this point we know we can use fsel.  */

  /* Reduce the comparison to a comparison against zero.  */
  temp = gen_reg_rtx (compare_mode);
  emit_insn (gen_rtx_SET (VOIDmode, temp,
			  gen_rtx_MINUS (compare_mode, op0, op1)));
  op0 = temp;
  op1 = CONST0_RTX (compare_mode);

  /* If we don't care about NaNs we can reduce some of the comparisons
     down to faster ones.  */
  if (flag_unsafe_math_optimizations)
    switch (code)
      {
      case GT:
	code = LE;
	temp = true_cond;
	true_cond = false_cond;
	false_cond = temp;
	break;
      case UNGE:
	code = GE;
	break;
      case UNEQ:
	code = EQ;
	break;
      default:
	break;
      }

  /* Now, reduce everything down to a GE.  */
  switch (code)
    {
    case GE:
      break;

    case LE:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    case ORDERED:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_ABS (compare_mode, op0)));
      op0 = temp;
      break;

    case EQ:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, 
			      gen_rtx_NEG (compare_mode,
					   gen_rtx_ABS (compare_mode, op0))));
      op0 = temp;
      break;

    case UNGE:
      temp = gen_reg_rtx (result_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp,
			      gen_rtx_IF_THEN_ELSE (result_mode,
						    gen_rtx_GE (VOIDmode,
								op0, op1),
						    true_cond, false_cond)));
      false_cond = temp;
      true_cond = false_cond;

      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    case GT:
      temp = gen_reg_rtx (result_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp,
			      gen_rtx_IF_THEN_ELSE (result_mode, 
						    gen_rtx_GE (VOIDmode,
								op0, op1),
						    true_cond, false_cond)));
      true_cond = temp;
      false_cond = true_cond;

      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    default:
      abort ();
    }

  emit_insn (gen_rtx_SET (VOIDmode, dest,
			  gen_rtx_IF_THEN_ELSE (result_mode,
						gen_rtx_GE (VOIDmode,
							    op0, op1),
						true_cond, false_cond)));
  return 1;
}

void
rs6000_emit_minmax (dest, code, op0, op1)
     rtx dest;
     enum rtx_code code;
     rtx op0;
     rtx op1;
{
  enum machine_mode mode = GET_MODE (op0);
  rtx target;
  if (code == SMAX || code == UMAX)
    target = emit_conditional_move (dest, GE, op0, op1, mode, 
				    op0, op1, mode, 0);
  else
    target = emit_conditional_move (dest, GE, op0, op1, mode, 
				    op1, op0, mode, 0);
  if (target == NULL_RTX)
    abort ();
  if (target != dest)
    emit_move_insn (dest, target);
}

/* This page contains routines that are used to determine what the
   function prologue and epilogue code will do and write them out.  */

/* Return the first fixed-point register that is required to be
   saved. 32 if none.  */

int
first_reg_to_save ()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 13; first_reg <= 31; first_reg++)
    if (regs_ever_live[first_reg] 
	&& (! call_used_regs[first_reg]
	    || (first_reg == RS6000_PIC_OFFSET_TABLE_REGNUM
		&& ((DEFAULT_ABI == ABI_V4 && flag_pic != 0)
		    || (DEFAULT_ABI == ABI_DARWIN && flag_pic)))))
      break;

#if TARGET_MACHO
  if (flag_pic && current_function_uses_pic_offset_table &&
      (first_reg > RS6000_PIC_OFFSET_TABLE_REGNUM))
    return RS6000_PIC_OFFSET_TABLE_REGNUM;
#endif

  return first_reg;
}

/* Similar, for FP regs.  */

int
first_fp_reg_to_save ()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 14 + 32; first_reg <= 63; first_reg++)
    if (regs_ever_live[first_reg])
      break;

  return first_reg;
}

/* Similar, for AltiVec regs.  */

static int
first_altivec_reg_to_save ()
{
  int i;

  /* Stack frame remains as is unless we are in AltiVec ABI.  */
  if (! TARGET_ALTIVEC_ABI)
    return LAST_ALTIVEC_REGNO + 1;

  /* Find lowest numbered live register.  */
  for (i = FIRST_ALTIVEC_REGNO + 20; i <= LAST_ALTIVEC_REGNO; ++i)
    if (regs_ever_live[i])
      break;

  return i;
}

/* Return a 32-bit mask of the AltiVec registers we need to set in
   VRSAVE.  Bit n of the return value is 1 if Vn is live.  The MSB in
   the 32-bit word is 0.  */

static unsigned int
compute_vrsave_mask ()
{
  unsigned int i, mask = 0;

  /* First, find out if we use _any_ altivec registers.  */
  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (regs_ever_live[i])
      mask |= ALTIVEC_REG_BIT (i);

  if (mask == 0)
    return mask;

  /* Next, add all registers that are call-clobbered.  We do this
     because post-reload register optimizers such as regrename_optimize
     may choose to use them.  They never change the register class
     chosen by reload, so cannot create new uses of altivec registers
     if there were none before, so the early exit above is safe.  */
  /* ??? Alternately, we could define HARD_REGNO_RENAME_OK to disallow
     altivec registers not saved in the mask, which might well make the
     adjustments below more effective in eliding the save/restore of
     VRSAVE in small functions.  */
  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (call_used_regs[i])
      mask |= ALTIVEC_REG_BIT (i);

  /* Next, remove the argument registers from the set.  These must
     be in the VRSAVE mask set by the caller, so we don't need to add
     them in again.  More importantly, the mask we compute here is
     used to generate CLOBBERs in the set_vrsave insn, and we do not
     wish the argument registers to die.  */
  for (i = cfun->args_info.vregno; i >= ALTIVEC_ARG_MIN_REG; --i)
    mask &= ~ALTIVEC_REG_BIT (i);

  /* Similarly, remove the return value from the set.  */
  {
    bool yes = false;
    diddle_return_value (is_altivec_return_reg, &yes);
    if (yes)
      mask &= ~ALTIVEC_REG_BIT (ALTIVEC_ARG_RETURN);
  }

  return mask;
}

static void
is_altivec_return_reg (reg, xyes)
     rtx reg;
     void *xyes;
{
  bool *yes = (bool *) xyes;
  if (REGNO (reg) == ALTIVEC_ARG_RETURN)
    *yes = true;
}


/* Calculate the stack information for the current function.  This is
   complicated by having two separate calling sequences, the AIX calling
   sequence and the V.4 calling sequence.

   AIX (and Darwin/Mac OS X) stack frames look like:
							  32-bit  64-bit
	SP---->	+---------------------------------------+
		| back chain to caller			| 0	  0
		+---------------------------------------+
		| saved CR				| 4       8 (8-11)
		+---------------------------------------+
		| saved LR				| 8       16
		+---------------------------------------+
		| reserved for compilers		| 12      24
		+---------------------------------------+
		| reserved for binders			| 16      32
		+---------------------------------------+
		| saved TOC pointer			| 20      40
		+---------------------------------------+
		| Parameter save area (P)		| 24      48
		+---------------------------------------+
		| Alloca space (A)			| 24+P    etc.
		+---------------------------------------+
		| Local variable space (L)		| 24+P+A
		+---------------------------------------+
		| Float/int conversion temporary (X)	| 24+P+A+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	| 24+P+A+L+X
		+---------------------------------------+
		| AltiVec alignment padding (Y)		| 24+P+A+L+X+W
		+---------------------------------------+
		| Save area for VRSAVE register (Z)	| 24+P+A+L+X+W+Y
		+---------------------------------------+
		| Save area for GP registers (G)	| 24+P+A+X+L+X+W+Y+Z
		+---------------------------------------+
		| Save area for FP registers (F)	| 24+P+A+X+L+X+W+Y+Z+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   The required alignment for AIX configurations is two words (i.e., 8
   or 16 bytes).


   V.4 stack frames look like:

	SP---->	+---------------------------------------+
		| back chain to caller			| 0
		+---------------------------------------+
		| caller's saved LR			| 4
		+---------------------------------------+
		| Parameter save area (P)		| 8
		+---------------------------------------+
		| Alloca space (A)			| 8+P
		+---------------------------------------+    
		| Varargs save area (V)			| 8+P+A
		+---------------------------------------+    
		| Local variable space (L)		| 8+P+A+V
		+---------------------------------------+    
		| Float/int conversion temporary (X)	| 8+P+A+V+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	| 8+P+A+V+L+X
		+---------------------------------------+
		| AltiVec alignment padding (Y)		| 8+P+A+V+L+X+W
		+---------------------------------------+
		| Save area for VRSAVE register (Z)	| 8+P+A+V+L+X+W+Y
		+---------------------------------------+
		| saved CR (C)				| 8+P+A+V+L+X+W+Y+Z
		+---------------------------------------+    
		| Save area for GP registers (G)	| 8+P+A+V+L+X+W+Y+Z+C
		+---------------------------------------+    
		| Save area for FP registers (F)	| 8+P+A+V+L+X+W+Y+Z+C+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   The required alignment for V.4 is 16 bytes, or 8 bytes if -meabi is
   given.  (But note below and in sysv4.h that we require only 8 and
   may round up the size of our stack frame anyways.  The historical
   reason is early versions of powerpc-linux which didn't properly
   align the stack at program startup.  A happy side-effect is that
   -mno-eabi libraries can be used with -meabi programs.)

   The EABI configuration defaults to the V.4 layout, unless
   -mcall-aix is used, in which case the AIX layout is used.  However,
   the stack alignment requirements may differ.  If -mno-eabi is not
   given, the required stack alignment is 8 bytes; if -mno-eabi is
   given, the required alignment is 16 bytes.  (But see V.4 comment
   above.)  */

#ifndef ABI_STACK_BOUNDARY
#define ABI_STACK_BOUNDARY STACK_BOUNDARY
#endif

rs6000_stack_t *
rs6000_stack_info ()
{
  static rs6000_stack_t info, zero_info;
  rs6000_stack_t *info_ptr = &info;
  int reg_size = TARGET_POWERPC64 ? 8 : 4;
  enum rs6000_abi abi;
  int ehrd_size;
  int total_raw_size;

  /* Zero all fields portably.  */
  info = zero_info;

  /* Select which calling sequence.  */
  info_ptr->abi = abi = DEFAULT_ABI;

  /* Calculate which registers need to be saved & save area size.  */
  info_ptr->first_gp_reg_save = first_reg_to_save ();
  /* Assume that we will have to save RS6000_PIC_OFFSET_TABLE_REGNUM, 
     even if it currently looks like we won't.  */
  if (((TARGET_TOC && TARGET_MINIMAL_TOC)
       || (flag_pic == 1 && abi == ABI_V4)
       || (flag_pic && abi == ABI_DARWIN))
      && info_ptr->first_gp_reg_save > RS6000_PIC_OFFSET_TABLE_REGNUM)
    info_ptr->gp_size = reg_size * (32 - RS6000_PIC_OFFSET_TABLE_REGNUM);
  else
    info_ptr->gp_size = reg_size * (32 - info_ptr->first_gp_reg_save);

  info_ptr->first_fp_reg_save = first_fp_reg_to_save ();
  info_ptr->fp_size = 8 * (64 - info_ptr->first_fp_reg_save);

  info_ptr->first_altivec_reg_save = first_altivec_reg_to_save ();
  info_ptr->altivec_size = 16 * (LAST_ALTIVEC_REGNO + 1
				 - info_ptr->first_altivec_reg_save);

  /* Does this function call anything?  */
  info_ptr->calls_p = (! current_function_is_leaf
		       || cfun->machine->ra_needs_full_frame);

  /* Determine if we need to save the link register.  */
  if (rs6000_ra_ever_killed ()
      || (DEFAULT_ABI == ABI_AIX && current_function_profile)
#ifdef TARGET_RELOCATABLE
      || (TARGET_RELOCATABLE && (get_pool_size () != 0))
#endif
      || (info_ptr->first_fp_reg_save != 64
	  && !FP_SAVE_INLINE (info_ptr->first_fp_reg_save))
      || info_ptr->first_altivec_reg_save <= LAST_ALTIVEC_REGNO
      || (abi == ABI_V4 && current_function_calls_alloca)
      || (DEFAULT_ABI == ABI_DARWIN
	  && flag_pic
	  && current_function_uses_pic_offset_table)
      || info_ptr->calls_p)
    {
      info_ptr->lr_save_p = 1;
      regs_ever_live[LINK_REGISTER_REGNUM] = 1;
    }

  /* Determine if we need to save the condition code registers.  */
  if (regs_ever_live[CR2_REGNO] 
      || regs_ever_live[CR3_REGNO]
      || regs_ever_live[CR4_REGNO])
    {
      info_ptr->cr_save_p = 1;
      if (abi == ABI_V4)
	info_ptr->cr_size = reg_size;
    }

  /* If the current function calls __builtin_eh_return, then we need
     to allocate stack space for registers that will hold data for
     the exception handler.  */
  if (current_function_calls_eh_return)
    {
      unsigned int i;
      for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; ++i)
	continue;
      ehrd_size = i * UNITS_PER_WORD;
    }
  else
    ehrd_size = 0;

  /* Determine various sizes.  */
  info_ptr->reg_size     = reg_size;
  info_ptr->fixed_size   = RS6000_SAVE_AREA;
  info_ptr->varargs_size = RS6000_VARARGS_AREA;
  info_ptr->vars_size    = RS6000_ALIGN (get_frame_size (), 8);
  info_ptr->parm_size    = RS6000_ALIGN (current_function_outgoing_args_size,
					 8);

  if (TARGET_ALTIVEC_ABI)
    {
      info_ptr->vrsave_mask = compute_vrsave_mask ();
      info_ptr->vrsave_size  = info_ptr->vrsave_mask ? 4 : 0;
    }
  else
    {
      info_ptr->vrsave_mask = 0;
      info_ptr->vrsave_size = 0;
    }

  /* Calculate the offsets.  */
  switch (abi)
    {
    case ABI_NONE:
    default:
      abort ();

    case ABI_AIX:
    case ABI_AIX_NODESC:
    case ABI_DARWIN:
      info_ptr->fp_save_offset   = - info_ptr->fp_size;
      info_ptr->gp_save_offset   = info_ptr->fp_save_offset - info_ptr->gp_size;

      if (TARGET_ALTIVEC_ABI)
	{
	  info_ptr->vrsave_save_offset
	    = info_ptr->gp_save_offset - info_ptr->vrsave_size;

	  /* Align stack so vector save area is on a quadword boundary.  */
	  if (info_ptr->altivec_size != 0)
	    info_ptr->altivec_padding_size
	      = 16 - (-info_ptr->vrsave_save_offset % 16);
	  else
	    info_ptr->altivec_padding_size = 0;

	  info_ptr->altivec_save_offset
	    = info_ptr->vrsave_save_offset
	    - info_ptr->altivec_padding_size
	    - info_ptr->altivec_size;

	  /* Adjust for AltiVec case.  */
	  info_ptr->ehrd_offset = info_ptr->altivec_save_offset - ehrd_size;
	}
      else
	info_ptr->ehrd_offset      = info_ptr->gp_save_offset - ehrd_size;
      info_ptr->cr_save_offset   = reg_size; /* first word when 64-bit.  */
      info_ptr->lr_save_offset   = 2*reg_size;
      break;

    case ABI_V4:
      info_ptr->fp_save_offset   = - info_ptr->fp_size;
      info_ptr->gp_save_offset   = info_ptr->fp_save_offset - info_ptr->gp_size;
      info_ptr->cr_save_offset   = info_ptr->gp_save_offset - info_ptr->cr_size;

      if (TARGET_ALTIVEC_ABI)
	{
	  info_ptr->vrsave_save_offset
	    = info_ptr->cr_save_offset - info_ptr->vrsave_size;

	  /* Align stack so vector save area is on a quadword boundary.  */
	  if (info_ptr->altivec_size != 0)
	    info_ptr->altivec_padding_size
	      = 16 - (-info_ptr->vrsave_save_offset % 16);
	  else
	    info_ptr->altivec_padding_size = 0;

	  info_ptr->altivec_save_offset
	    = info_ptr->vrsave_save_offset
	    - info_ptr->altivec_padding_size
	    - info_ptr->altivec_size;

	  /* Adjust for AltiVec case.  */
	  info_ptr->toc_save_offset
	    = info_ptr->altivec_save_offset - info_ptr->toc_size;
	}
      else
	info_ptr->toc_save_offset  = info_ptr->cr_save_offset - info_ptr->toc_size;
      info_ptr->ehrd_offset      = info_ptr->toc_save_offset - ehrd_size;
      info_ptr->lr_save_offset   = reg_size;
      break;
    }

  info_ptr->save_size    = RS6000_ALIGN (info_ptr->fp_size
					 + info_ptr->gp_size
					 + info_ptr->altivec_size
					 + info_ptr->altivec_padding_size
					 + info_ptr->vrsave_size
					 + ehrd_size
					 + info_ptr->cr_size
					 + info_ptr->lr_size
					 + info_ptr->vrsave_size
					 + info_ptr->toc_size,
					 (TARGET_ALTIVEC_ABI || ABI_DARWIN)
					 ? 16 : 8);

  total_raw_size	 = (info_ptr->vars_size
			    + info_ptr->parm_size
			    + info_ptr->save_size
			    + info_ptr->varargs_size
			    + info_ptr->fixed_size);

  info_ptr->total_size =
    RS6000_ALIGN (total_raw_size, ABI_STACK_BOUNDARY / BITS_PER_UNIT);

  /* Determine if we need to allocate any stack frame:

     For AIX we need to push the stack if a frame pointer is needed
     (because the stack might be dynamically adjusted), if we are
     debugging, if we make calls, or if the sum of fp_save, gp_save,
     and local variables are more than the space needed to save all
     non-volatile registers: 32-bit: 18*8 + 19*4 = 220 or 64-bit: 18*8
     + 18*8 = 288 (GPR13 reserved).

     For V.4 we don't have the stack cushion that AIX uses, but assume
     that the debugger can handle stackless frames.  */

  if (info_ptr->calls_p)
    info_ptr->push_p = 1;

  else if (abi == ABI_V4)
    info_ptr->push_p = total_raw_size > info_ptr->fixed_size;

  else
    info_ptr->push_p = (frame_pointer_needed
			|| (abi != ABI_DARWIN && write_symbols != NO_DEBUG)
			|| ((total_raw_size - info_ptr->fixed_size)
			    > (TARGET_32BIT ? 220 : 288)));

  /* Zero offsets if we're not saving those registers.  */
  if (info_ptr->fp_size == 0)
    info_ptr->fp_save_offset = 0;

  if (info_ptr->gp_size == 0)
    info_ptr->gp_save_offset = 0;

  if (! TARGET_ALTIVEC_ABI || info_ptr->altivec_size == 0)
    info_ptr->altivec_save_offset = 0;

  if (! TARGET_ALTIVEC_ABI || info_ptr->vrsave_mask == 0)
    info_ptr->vrsave_save_offset = 0;

  if (! info_ptr->lr_save_p)
    info_ptr->lr_save_offset = 0;

  if (! info_ptr->cr_save_p)
    info_ptr->cr_save_offset = 0;

  if (! info_ptr->toc_save_p)
    info_ptr->toc_save_offset = 0;

  return info_ptr;
}

void
debug_stack_info (info)
     rs6000_stack_t *info;
{
  const char *abi_string;

  if (! info)
    info = rs6000_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  switch (info->abi)
    {
    default:		 abi_string = "Unknown";	break;
    case ABI_NONE:	 abi_string = "NONE";		break;
    case ABI_AIX:
    case ABI_AIX_NODESC: abi_string = "AIX";		break;
    case ABI_DARWIN:	 abi_string = "Darwin";		break;
    case ABI_V4:	 abi_string = "V.4";		break;
    }

  fprintf (stderr, "\tABI                 = %5s\n", abi_string);

  if (TARGET_ALTIVEC_ABI)
    fprintf (stderr, "\tALTIVEC ABI extensions enabled.\n");

  if (info->first_gp_reg_save != 32)
    fprintf (stderr, "\tfirst_gp_reg_save   = %5d\n", info->first_gp_reg_save);

  if (info->first_fp_reg_save != 64)
    fprintf (stderr, "\tfirst_fp_reg_save   = %5d\n", info->first_fp_reg_save);

  if (info->first_altivec_reg_save <= LAST_ALTIVEC_REGNO)
    fprintf (stderr, "\tfirst_altivec_reg_save = %5d\n",
	     info->first_altivec_reg_save);

  if (info->lr_save_p)
    fprintf (stderr, "\tlr_save_p           = %5d\n", info->lr_save_p);

  if (info->cr_save_p)
    fprintf (stderr, "\tcr_save_p           = %5d\n", info->cr_save_p);

  if (info->toc_save_p)
    fprintf (stderr, "\ttoc_save_p          = %5d\n", info->toc_save_p);

  if (info->vrsave_mask)
    fprintf (stderr, "\tvrsave_mask         = 0x%x\n", info->vrsave_mask);

  if (info->push_p)
    fprintf (stderr, "\tpush_p              = %5d\n", info->push_p);

  if (info->calls_p)
    fprintf (stderr, "\tcalls_p             = %5d\n", info->calls_p);

  if (info->gp_save_offset)
    fprintf (stderr, "\tgp_save_offset      = %5d\n", info->gp_save_offset);

  if (info->fp_save_offset)
    fprintf (stderr, "\tfp_save_offset      = %5d\n", info->fp_save_offset);

  if (info->altivec_save_offset)
    fprintf (stderr, "\taltivec_save_offset = %5d\n",
	     info->altivec_save_offset);

  if (info->vrsave_save_offset)
    fprintf (stderr, "\tvrsave_save_offset  = %5d\n",
	     info->vrsave_save_offset);

  if (info->lr_save_offset)
    fprintf (stderr, "\tlr_save_offset      = %5d\n", info->lr_save_offset);

  if (info->cr_save_offset)
    fprintf (stderr, "\tcr_save_offset      = %5d\n", info->cr_save_offset);

  if (info->toc_save_offset)
    fprintf (stderr, "\ttoc_save_offset     = %5d\n", info->toc_save_offset);

  if (info->varargs_save_offset)
    fprintf (stderr, "\tvarargs_save_offset = %5d\n", info->varargs_save_offset);

  if (info->total_size)
    fprintf (stderr, "\ttotal_size          = %5d\n", info->total_size);

  if (info->varargs_size)
    fprintf (stderr, "\tvarargs_size        = %5d\n", info->varargs_size);

  if (info->vars_size)
    fprintf (stderr, "\tvars_size           = %5d\n", info->vars_size);

  if (info->parm_size)
    fprintf (stderr, "\tparm_size           = %5d\n", info->parm_size);

  if (info->fixed_size)
    fprintf (stderr, "\tfixed_size          = %5d\n", info->fixed_size);

  if (info->gp_size)
    fprintf (stderr, "\tgp_size             = %5d\n", info->gp_size);

  if (info->fp_size)
    fprintf (stderr, "\tfp_size             = %5d\n", info->fp_size);

  if (info->altivec_size)
    fprintf (stderr, "\taltivec_size        = %5d\n", info->altivec_size);

  if (info->vrsave_size)
    fprintf (stderr, "\tvrsave_size         = %5d\n", info->vrsave_size);

  if (info->altivec_padding_size)
    fprintf (stderr, "\taltivec_padding_size= %5d\n",
	     info->altivec_padding_size);

  if (info->lr_size)
    fprintf (stderr, "\tlr_size             = %5d\n", info->lr_size);

  if (info->cr_size)
    fprintf (stderr, "\tcr_size             = %5d\n", info->cr_size);

  if (info->toc_size)
    fprintf (stderr, "\ttoc_size            = %5d\n", info->toc_size);

  if (info->save_size)
    fprintf (stderr, "\tsave_size           = %5d\n", info->save_size);

  if (info->reg_size != 4)
    fprintf (stderr, "\treg_size            = %5d\n", info->reg_size);

  fprintf (stderr, "\n");
}

rtx
rs6000_return_addr (count, frame)
     int count;
     rtx frame;
{
  /* Currently we don't optimize very well between prolog and body
     code and for PIC code the code can be actually quite bad, so
     don't try to be too clever here.  */
  if (count != 0
      || flag_pic != 0
      || DEFAULT_ABI == ABI_AIX
      || DEFAULT_ABI == ABI_AIX_NODESC)
    {
      cfun->machine->ra_needs_full_frame = 1;

      return
	gen_rtx_MEM
	  (Pmode,
	   memory_address
	   (Pmode,
	    plus_constant (copy_to_reg
			   (gen_rtx_MEM (Pmode,
					 memory_address (Pmode, frame))),
			   RETURN_ADDRESS_OFFSET)));
    }

  return get_hard_reg_initial_val (Pmode, LINK_REGISTER_REGNUM);
}

static int
rs6000_ra_ever_killed ()
{
  rtx top;

#ifdef ASM_OUTPUT_MI_THUNK
  if (current_function_is_thunk)
    return 0;
#endif
  if (!has_hard_reg_initial_val (Pmode, LINK_REGISTER_REGNUM)
      || cfun->machine->ra_needs_full_frame)
    return regs_ever_live[LINK_REGISTER_REGNUM];

  push_topmost_sequence ();
  top = get_insns ();
  pop_topmost_sequence ();

  return reg_set_between_p (gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM), 
			    top, NULL_RTX);
}

/* Add a REG_MAYBE_DEAD note to the insn.  */
static void
rs6000_maybe_dead (insn)
     rtx insn;
{
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD,
					const0_rtx,
					REG_NOTES (insn));
}

/* Emit instructions needed to load the TOC register.
   This is only needed when TARGET_TOC, TARGET_MINIMAL_TOC, and there is
   a constant pool; or for SVR4 -fpic.  */

void
rs6000_emit_load_toc_table (fromprolog)
     int fromprolog;
{
  rtx dest, insn;
  dest = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);

  if (TARGET_ELF && DEFAULT_ABI == ABI_V4 && flag_pic == 1)
    {
      rtx temp = (fromprolog
		  ? gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)
		  : gen_reg_rtx (Pmode));
      insn = emit_insn (gen_load_toc_v4_pic_si (temp));
      if (fromprolog)
	rs6000_maybe_dead (insn);
      insn = emit_move_insn (dest, temp);
      if (fromprolog)
	rs6000_maybe_dead (insn);
    }
  else if (TARGET_ELF && DEFAULT_ABI != ABI_AIX && flag_pic == 2)
    {
      char buf[30];
      rtx tempLR = (fromprolog
		    ? gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)
		    : gen_reg_rtx (Pmode));
      rtx temp0 = (fromprolog
		   ? gen_rtx_REG (Pmode, 0)
		   : gen_reg_rtx (Pmode));
      rtx symF;

      /* possibly create the toc section */
      if (! toc_initialized)
	{
	  toc_section ();
	  function_section (current_function_decl);
	}

      if (fromprolog)
	{
	  rtx symL;

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  symF = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCL", rs6000_pic_labelno);
	  symL = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  rs6000_maybe_dead (emit_insn (gen_load_toc_v4_PIC_1 (tempLR,
							       symF)));
	  rs6000_maybe_dead (emit_move_insn (dest, tempLR));
	  rs6000_maybe_dead (emit_insn (gen_load_toc_v4_PIC_2 (temp0, dest,
							       symL,
							       symF)));
	}
      else
	{
	  rtx tocsym;
	  static int reload_toc_labelno = 0;

	  tocsym = gen_rtx_SYMBOL_REF (Pmode, toc_label_name);

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCG", reload_toc_labelno++);
	  symF = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  emit_insn (gen_load_toc_v4_PIC_1b (tempLR, symF, tocsym));
	  emit_move_insn (dest, tempLR);
	  emit_move_insn (temp0, gen_rtx_MEM (Pmode, dest));
	}
      insn = emit_insn (gen_addsi3 (dest, temp0, dest));
      if (fromprolog)
	rs6000_maybe_dead (insn);
    }
  else if (TARGET_ELF && !TARGET_AIX && flag_pic == 0 && TARGET_MINIMAL_TOC)
    {
      /* This is for AIX code running in non-PIC ELF32.  */
      char buf[30];
      rtx realsym;
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
      realsym = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

      insn = emit_insn (gen_elf_high (dest, realsym));
      if (fromprolog)
	rs6000_maybe_dead (insn);
      insn = emit_insn (gen_elf_low (dest, dest, realsym));
      if (fromprolog)
	rs6000_maybe_dead (insn);
    }
  else if (DEFAULT_ABI == ABI_AIX)
    {
      if (TARGET_32BIT)
	insn = emit_insn (gen_load_toc_aix_si (dest));
      else
	insn = emit_insn (gen_load_toc_aix_di (dest));
      if (fromprolog)
	rs6000_maybe_dead (insn);
    }
  else
    abort ();
}

int   
get_TOC_alias_set ()
{
    static int set = -1;
    if (set == -1)
      set = new_alias_set ();
    return set;
}   

/* This retuns nonzero if the current function uses the TOC.  This is
   determined by the presence of (unspec ... 7), which is generated by
   the various load_toc_* patterns.  */

int
uses_TOC () 
{
    rtx insn;

    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (INSN_P (insn))
	{
	  rtx pat = PATTERN (insn);
	  int i;

	  if (GET_CODE (pat) == PARALLEL) 
	    for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	      if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == UNSPEC 
		 && XINT (XVECEXP (PATTERN (insn), 0, i), 1) == 7)
		  return 1;
	}
    return 0;
}

rtx
create_TOC_reference (symbol) 
    rtx symbol;
{
  return gen_rtx_PLUS (Pmode, 
	   gen_rtx_REG (Pmode, TOC_REGISTER),
	     gen_rtx_CONST (Pmode, 
	       gen_rtx_MINUS (Pmode, symbol, 
		 gen_rtx_SYMBOL_REF (Pmode, toc_label_name))));
}

#if TARGET_AIX
/* __throw will restore its own return address to be the same as the
   return address of the function that the throw is being made to.
   This is unfortunate, because we want to check the original
   return address to see if we need to restore the TOC.
   So we have to squirrel it away here.  
   This is used only in compiling __throw and __rethrow. 

   Most of this code should be removed by CSE.  */
static rtx insn_after_throw;

/* This does the saving...  */
void
rs6000_aix_emit_builtin_unwind_init ()
{
  rtx mem;
  rtx stack_top = gen_reg_rtx (Pmode);
  rtx opcode_addr = gen_reg_rtx (Pmode);

  insn_after_throw = gen_reg_rtx (SImode);

  mem = gen_rtx_MEM (Pmode, hard_frame_pointer_rtx);
  emit_move_insn (stack_top, mem);

  mem = gen_rtx_MEM (Pmode, 
		     gen_rtx_PLUS (Pmode, stack_top, 
				   GEN_INT (2 * GET_MODE_SIZE (Pmode))));
  emit_move_insn (opcode_addr, mem);
  emit_move_insn (insn_after_throw, gen_rtx_MEM (SImode, opcode_addr));
}

/* Emit insns to _restore_ the TOC register, at runtime (specifically
   in _eh.o).  Only used on AIX.

   The idea is that on AIX, function calls look like this:
	bl  somefunction-trampoline
	lwz r2,20(sp)

   and later,
	somefunction-trampoline:
	stw r2,20(sp)
	 ... load function address in the count register ...
	bctr
   or like this, if the linker determines that this is not a cross-module call
   and so the TOC need not be restored:
	bl  somefunction
	nop
   or like this, if the compiler could determine that this is not a
   cross-module call:
	bl  somefunction
   now, the tricky bit here is that register 2 is saved and restored
   by the _linker_, so we can't readily generate debugging information
   for it.  So we need to go back up the call chain looking at the
   insns at return addresses to see which calls saved the TOC register
   and so see where it gets restored from.

   Oh, and all this gets done in RTL inside the eh_epilogue pattern,
   just before the actual epilogue.

   On the bright side, this incurs no space or time overhead unless an
   exception is thrown, except for the extra code in libgcc.a.  

   The parameter STACKSIZE is a register containing (at runtime)
   the amount to be popped off the stack in addition to the stack frame
   of this routine (which will be __throw or __rethrow, and so is
   guaranteed to have a stack frame).  */

void
rs6000_emit_eh_toc_restore (stacksize)
     rtx stacksize;
{
  rtx top_of_stack;
  rtx bottom_of_stack = gen_reg_rtx (Pmode);
  rtx tocompare = gen_reg_rtx (SImode);
  rtx opcode = gen_reg_rtx (SImode);
  rtx opcode_addr = gen_reg_rtx (Pmode);
  rtx mem;
  rtx loop_start = gen_label_rtx ();
  rtx no_toc_restore_needed = gen_label_rtx ();
  rtx loop_exit = gen_label_rtx ();
  
  mem = gen_rtx_MEM (Pmode, hard_frame_pointer_rtx);
  set_mem_alias_set (mem, rs6000_sr_alias_set);
  emit_move_insn (bottom_of_stack, mem);

  top_of_stack = expand_binop (Pmode, add_optab, 
			       bottom_of_stack, stacksize,
			       NULL_RTX, 1, OPTAB_WIDEN);

  emit_move_insn (tocompare, 
		  GEN_INT (trunc_int_for_mode (TARGET_32BIT 
					       ? 0x80410014 
					       : 0xE8410028, SImode)));

  if (insn_after_throw == NULL_RTX)
    abort ();
  emit_move_insn (opcode, insn_after_throw);
  
  emit_note (NULL, NOTE_INSN_LOOP_BEG);
  emit_label (loop_start);
  
  do_compare_rtx_and_jump (opcode, tocompare, NE, 1,
			   SImode, NULL_RTX, NULL_RTX,
			   no_toc_restore_needed);
  
  mem = gen_rtx_MEM (Pmode, 
		     gen_rtx_PLUS (Pmode, bottom_of_stack, 
				   GEN_INT (5 * GET_MODE_SIZE (Pmode))));
  emit_move_insn (gen_rtx_REG (Pmode, 2), mem);

  emit_label (no_toc_restore_needed);
  do_compare_rtx_and_jump (top_of_stack, bottom_of_stack, EQ, 1,
			   Pmode, NULL_RTX, NULL_RTX,
			   loop_exit);

  mem = gen_rtx_MEM (Pmode, bottom_of_stack);
  set_mem_alias_set (mem, rs6000_sr_alias_set);
  emit_move_insn (bottom_of_stack, mem);
  
  mem = gen_rtx_MEM (Pmode, 
		     gen_rtx_PLUS (Pmode, bottom_of_stack, 
				   GEN_INT (2 * GET_MODE_SIZE (Pmode))));
  emit_move_insn (opcode_addr, mem);
  emit_move_insn (opcode, gen_rtx_MEM (SImode, opcode_addr));

  emit_note (NULL, NOTE_INSN_LOOP_CONT);
  emit_jump (loop_start);
  emit_note (NULL, NOTE_INSN_LOOP_END);
  emit_label (loop_exit);
}
#endif /* TARGET_AIX */

/* This ties together stack memory (MEM with an alias set of
   rs6000_sr_alias_set) and the change to the stack pointer.  */

static void
rs6000_emit_stack_tie ()
{
  rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM));

  set_mem_alias_set (mem, rs6000_sr_alias_set);
  emit_insn (gen_stack_tie (mem));
}

/* Emit the correct code for allocating stack space, as insns.
   If COPY_R12, make sure a copy of the old frame is left in r12.
   The generated code may use hard register 0 as a temporary.  */

static void
rs6000_emit_allocate_stack (size, copy_r12)
     HOST_WIDE_INT size;
     int copy_r12;
{
  rtx insn;
  rtx stack_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx tmp_reg = gen_rtx_REG (Pmode, 0);
  rtx todec = GEN_INT (-size);

  if (current_function_limit_stack)
    {
      if (REG_P (stack_limit_rtx)
	  && REGNO (stack_limit_rtx) > 1 
	  && REGNO (stack_limit_rtx) <= 31)
	{
	  emit_insn (Pmode == SImode
		     ? gen_addsi3 (tmp_reg,
				   stack_limit_rtx,
				   GEN_INT (size))
		     : gen_adddi3 (tmp_reg,
				   stack_limit_rtx,
				   GEN_INT (size)));
	  
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg,
				    const0_rtx));
	}
      else if (GET_CODE (stack_limit_rtx) == SYMBOL_REF
	       && TARGET_32BIT
	       && DEFAULT_ABI == ABI_V4)
	{
	  rtx toload = gen_rtx_CONST (VOIDmode,
				      gen_rtx_PLUS (Pmode, 
						    stack_limit_rtx, 
						    GEN_INT (size)));
	  
	  emit_insn (gen_elf_high (tmp_reg, toload));
	  emit_insn (gen_elf_low (tmp_reg, tmp_reg, toload));
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg,
				    const0_rtx));
	}
      else
	warning ("stack limit expression is not supported");
    }

  if (copy_r12 || ! TARGET_UPDATE)
    emit_move_insn (gen_rtx_REG (Pmode, 12), stack_reg);

  if (TARGET_UPDATE)
    {
      if (size > 32767)
	{
	  /* Need a note here so that try_split doesn't get confused.  */
	  if (get_last_insn() == NULL_RTX)
	    emit_note (0, NOTE_INSN_DELETED);
	  insn = emit_move_insn (tmp_reg, todec);
	  try_split (PATTERN (insn), insn, 0);
	  todec = tmp_reg;
	}
      
      if (Pmode == SImode)
	insn = emit_insn (gen_movsi_update (stack_reg, stack_reg, 
					    todec, stack_reg));
      else
	insn = emit_insn (gen_movdi_update (stack_reg, stack_reg, 
					    todec, stack_reg));
    }
  else
    {
      if (Pmode == SImode)
	insn = emit_insn (gen_addsi3 (stack_reg, stack_reg, todec));
      else
	insn = emit_insn (gen_adddi3 (stack_reg, stack_reg, todec));
      emit_move_insn (gen_rtx_MEM (Pmode, stack_reg),
		      gen_rtx_REG (Pmode, 12));
    }
  
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = 
    gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		       gen_rtx_SET (VOIDmode, stack_reg, 
				    gen_rtx_PLUS (Pmode, stack_reg,
						  GEN_INT (-size))),
		       REG_NOTES (insn));
}

/* Add a RTX_FRAME_RELATED note so that dwarf2out_frame_debug_expr
   knows that:

     (mem (plus (blah) (regXX)))

   is really:

     (mem (plus (blah) (const VALUE_OF_REGXX))).  */

static void
altivec_frame_fixup (insn, reg, val)
     rtx insn, reg;
     HOST_WIDE_INT val;
{
  rtx real;

  real = copy_rtx (PATTERN (insn));

  real = replace_rtx (real, reg, GEN_INT (val));

  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					real,
					REG_NOTES (insn));
}

/* Add to 'insn' a note which is PATTERN (INSN) but with REG replaced
   with (plus:P (reg 1) VAL), and with REG2 replaced with RREG if REG2
   is not NULL.  It would be nice if dwarf2out_frame_debug_expr could
   deduce these equivalences by itself so it wasn't necessary to hold
   its hand so much.  */

static void
rs6000_frame_related (insn, reg, val, reg2, rreg)
     rtx insn;
     rtx reg;
     HOST_WIDE_INT val;
     rtx reg2;
     rtx rreg;
{
  rtx real, temp;

  real = copy_rtx (PATTERN (insn));

  real = replace_rtx (real, reg, 
		      gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode,
							STACK_POINTER_REGNUM),
				    GEN_INT (val)));
  
  /* We expect that 'real' is either a SET or a PARALLEL containing
     SETs (and possibly other stuff).  In a PARALLEL, all the SETs
     are important so they all have to be marked RTX_FRAME_RELATED_P.  */

  if (GET_CODE (real) == SET)
    {
      rtx set = real;
      
      temp = simplify_rtx (SET_SRC (set));
      if (temp)
	SET_SRC (set) = temp;
      temp = simplify_rtx (SET_DEST (set));
      if (temp)
	SET_DEST (set) = temp;
      if (GET_CODE (SET_DEST (set)) == MEM)
	{
	  temp = simplify_rtx (XEXP (SET_DEST (set), 0));
	  if (temp)
	    XEXP (SET_DEST (set), 0) = temp;
	}
    }
  else if (GET_CODE (real) == PARALLEL)
    {
      int i;
      for (i = 0; i < XVECLEN (real, 0); i++)
	if (GET_CODE (XVECEXP (real, 0, i)) == SET)
	  {
	    rtx set = XVECEXP (real, 0, i);
	    
	    temp = simplify_rtx (SET_SRC (set));
	    if (temp)
	      SET_SRC (set) = temp;
	    temp = simplify_rtx (SET_DEST (set));
	    if (temp)
	      SET_DEST (set) = temp;
	    if (GET_CODE (SET_DEST (set)) == MEM)
	      {
		temp = simplify_rtx (XEXP (SET_DEST (set), 0));
		if (temp)
		  XEXP (SET_DEST (set), 0) = temp;
	      }
	    RTX_FRAME_RELATED_P (set) = 1;
	  }
    }
  else
    abort ();
  
  if (reg2 != NULL_RTX)
    real = replace_rtx (real, reg2, rreg);
  
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					real,
					REG_NOTES (insn));
}

/* Returns an insn that has a vrsave set operation with the
   appropriate CLOBBERs.  */

static rtx
generate_set_vrsave (reg, info, epiloguep)
     rtx reg;
     rs6000_stack_t *info;
     int epiloguep;
{
  int nclobs, i;
  rtx insn, clobs[TOTAL_ALTIVEC_REGS + 1];
  rtx vrsave = gen_rtx_REG (SImode, VRSAVE_REGNO);

  clobs[0]
    = gen_rtx_SET (VOIDmode,
		   vrsave,
		   gen_rtx_UNSPEC_VOLATILE (SImode,
					    gen_rtvec (2, reg, vrsave),
					    30));

  nclobs = 1;

  /* We need to clobber the registers in the mask so the scheduler
     does not move sets to VRSAVE before sets of AltiVec registers.

     However, if the function receives nonlocal gotos, reload will set
     all call saved registers live.  We will end up with:

     	(set (reg 999) (mem))
	(parallel [ (set (reg vrsave) (unspec blah))
		    (clobber (reg 999))])

     The clobber will cause the store into reg 999 to be dead, and
     flow will attempt to delete an epilogue insn.  In this case, we
     need an unspec use/set of the register.  */

  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (info->vrsave_mask != 0 && ALTIVEC_REG_BIT (i) != 0)
      {
	if (!epiloguep || call_used_regs [i])
	  clobs[nclobs++] = gen_rtx_CLOBBER (VOIDmode,
					     gen_rtx_REG (V4SImode, i));
	else
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);

	    clobs[nclobs++]
	      = gen_rtx_SET (VOIDmode,
			     reg,
			     gen_rtx_UNSPEC (V4SImode,
					     gen_rtvec (1, reg), 27));
	  }
      }

  insn = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nclobs));

  for (i = 0; i < nclobs; ++i)
    XVECEXP (insn, 0, i) = clobs[i];

  return insn;
}

/* Emit function prologue as insns.  */

void
rs6000_emit_prologue ()
{
  rs6000_stack_t *info = rs6000_stack_info ();
  enum machine_mode reg_mode = TARGET_POWERPC64 ? DImode : SImode;
  int reg_size = TARGET_POWERPC64 ? 8 : 4;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx frame_ptr_rtx = gen_rtx_REG (Pmode, 12);
  rtx frame_reg_rtx = sp_reg_rtx;
  rtx cr_save_rtx = NULL;
  rtx insn;
  int saving_FPRs_inline;
  int using_store_multiple;
  HOST_WIDE_INT sp_offset = 0;
  
  using_store_multiple = (TARGET_MULTIPLE && ! TARGET_POWERPC64
			  && info->first_gp_reg_save < 31);
  saving_FPRs_inline = (info->first_fp_reg_save == 64
			|| FP_SAVE_INLINE (info->first_fp_reg_save));

  /* For V.4, update stack before we do any saving and set back pointer.  */
  if (info->push_p && DEFAULT_ABI == ABI_V4)
    {
      if (info->total_size < 32767)
	sp_offset = info->total_size;
      else
	frame_reg_rtx = frame_ptr_rtx;
      rs6000_emit_allocate_stack (info->total_size, 
				  (frame_reg_rtx != sp_reg_rtx
				   && (info->cr_save_p
				       || info->lr_save_p
				       || info->first_fp_reg_save < 64
				       || info->first_gp_reg_save < 32
				       )));
      if (frame_reg_rtx != sp_reg_rtx)
	rs6000_emit_stack_tie ();
    }

  /* Save AltiVec registers if needed.  */
  if (TARGET_ALTIVEC_ABI && info->altivec_size != 0)
    {
      int i;

      /* There should be a non inline version of this, for when we
	 are saving lots of vector registers.  */
      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	  {
	    rtx areg, savereg, mem;
	    int offset;

	    offset = info->altivec_save_offset + sp_offset
	      + 16 * (i - info->first_altivec_reg_save);

	    savereg = gen_rtx_REG (V4SImode, i);

	    areg = gen_rtx_REG (Pmode, 0);
	    emit_move_insn (areg, GEN_INT (offset));

	    /* AltiVec addressing mode is [reg+reg].  */
	    mem = gen_rtx_MEM (V4SImode,
			       gen_rtx_PLUS (Pmode, frame_reg_rtx, areg));
			       
	    set_mem_alias_set (mem, rs6000_sr_alias_set);

	    insn = emit_move_insn (mem, savereg);

	    altivec_frame_fixup (insn, areg, offset);
	  }
    }

  /* VRSAVE is a bit vector representing which AltiVec registers
     are used.  The OS uses this to determine which vector
     registers to save on a context switch.  We need to save
     VRSAVE on the stack frame, add whatever AltiVec registers we
     used in this function, and do the corresponding magic in the
     epilogue.  */

  if (TARGET_ALTIVEC && info->vrsave_mask != 0)
    {
      rtx reg, mem, vrsave;
      int offset;

      /* Get VRSAVE onto a GPR.  */
      reg = gen_rtx_REG (SImode, 12);
      vrsave = gen_rtx_REG (SImode, VRSAVE_REGNO);
      if (TARGET_MACHO)
	emit_insn (gen_get_vrsave_internal (reg));
      else
	emit_insn (gen_rtx_SET (VOIDmode, reg, vrsave));

      /* Save VRSAVE.  */
      offset = info->vrsave_save_offset + sp_offset;
      mem
	= gen_rtx_MEM (SImode,
		       gen_rtx_PLUS (Pmode, frame_reg_rtx, GEN_INT (offset)));
      set_mem_alias_set (mem, rs6000_sr_alias_set);
      insn = emit_move_insn (mem, reg);

      /* Include the registers in the mask.  */
      emit_insn (gen_iorsi3 (reg, reg, GEN_INT ((int) info->vrsave_mask)));

      insn = emit_insn (generate_set_vrsave (reg, info, 0));
    }

  /* If we use the link register, get it into r0.  */
  if (info->lr_save_p)
    emit_move_insn (gen_rtx_REG (Pmode, 0),
		    gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM));

  /* If we need to save CR, put it into r12.  */
  if (info->cr_save_p && frame_reg_rtx != frame_ptr_rtx)
    {
      cr_save_rtx = gen_rtx_REG (SImode, 12);
      emit_insn (gen_movesi_from_cr (cr_save_rtx));
    }

  /* Do any required saving of fpr's.  If only one or two to save, do
     it ourselves.  Otherwise, call function.  */
  if (saving_FPRs_inline)
    {
      int i;
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	if ((regs_ever_live[info->first_fp_reg_save+i] 
	     && ! call_used_regs[info->first_fp_reg_save+i]))
	  {
	    rtx addr, reg, mem;
	    reg = gen_rtx_REG (DFmode, info->first_fp_reg_save + i);
	    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
				 GEN_INT (info->fp_save_offset 
					  + sp_offset 
					  + 8 * i));
	    mem = gen_rtx_MEM (DFmode, addr);
	    set_mem_alias_set (mem, rs6000_sr_alias_set);

	    insn = emit_move_insn (mem, reg);
	    rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
				  NULL_RTX, NULL_RTX);
	  }
    }
  else if (info->first_fp_reg_save != 64)
    {
      int i;
      char rname[30];
      const char *alloc_rname;
      rtvec p;
      p = rtvec_alloc (2 + 64 - info->first_fp_reg_save);
      
      RTVEC_ELT (p, 0) = gen_rtx_CLOBBER (VOIDmode, 
					  gen_rtx_REG (Pmode, 
						       LINK_REGISTER_REGNUM));
      sprintf (rname, "%s%d%s", SAVE_FP_PREFIX,
	       info->first_fp_reg_save - 32, SAVE_FP_SUFFIX);
      alloc_rname = ggc_strdup (rname);
      RTVEC_ELT (p, 1) = gen_rtx_USE (VOIDmode,
				      gen_rtx_SYMBOL_REF (Pmode,
							  alloc_rname));
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	{
	  rtx addr, reg, mem;
	  reg = gen_rtx_REG (DFmode, info->first_fp_reg_save + i);
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->fp_save_offset 
					+ sp_offset + 8*i));
	  mem = gen_rtx_MEM (DFmode, addr);
	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  RTVEC_ELT (p, i + 2) = gen_rtx_SET (VOIDmode, mem, reg);
	}
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    NULL_RTX, NULL_RTX);
    }

  /* Save GPRs.  This is done as a PARALLEL if we are using
     the store-multiple instructions.  */
  if (using_store_multiple)
    {
      rtvec p, dwarfp;
      int i;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      dwarfp = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	{
	  rtx addr, reg, mem;
	  reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
			       GEN_INT (info->gp_save_offset 
					+ sp_offset 
					+ reg_size * i));
	  mem = gen_rtx_MEM (reg_mode, addr);
	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  RTVEC_ELT (p, i) = gen_rtx_SET (VOIDmode, mem, reg);
	}
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    NULL_RTX, NULL_RTX);
    }
  else
    {
      int i;
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	if ((regs_ever_live[info->first_gp_reg_save+i] 
	     && ! call_used_regs[info->first_gp_reg_save+i])
	    || (i+info->first_gp_reg_save == RS6000_PIC_OFFSET_TABLE_REGNUM
		&& ((DEFAULT_ABI == ABI_V4 && flag_pic != 0)
		    || (DEFAULT_ABI == ABI_DARWIN && flag_pic))))
	  {
	    rtx addr, reg, mem;
	    reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);
	    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
				 GEN_INT (info->gp_save_offset 
					  + sp_offset 
					  + reg_size * i));
	    mem = gen_rtx_MEM (reg_mode, addr);
	    set_mem_alias_set (mem, rs6000_sr_alias_set);

	    insn = emit_move_insn (mem, reg);
	    rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
				  NULL_RTX, NULL_RTX);
	  }
    }

  /* ??? There's no need to emit actual instructions here, but it's the
     easiest way to get the frame unwind information emitted.  */
  if (current_function_calls_eh_return)
    {
      unsigned int i, regno;

      for (i = 0; ; ++i)
	{
	  rtx addr, reg, mem;

	  regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;

	  reg = gen_rtx_REG (reg_mode, regno);
	  addr = plus_constant (frame_reg_rtx,
				info->ehrd_offset + sp_offset
				+ reg_size * (int) i);
	  mem = gen_rtx_MEM (reg_mode, addr);
	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  insn = emit_move_insn (mem, reg);
	  rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
				NULL_RTX, NULL_RTX);
	}
    }

  /* Save lr if we used it.  */
  if (info->lr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->lr_save_offset + sp_offset));
      rtx reg = gen_rtx_REG (Pmode, 0);
      rtx mem = gen_rtx_MEM (Pmode, addr);
      /* This should not be of rs6000_sr_alias_set, because of
	 __builtin_return_address.  */
      
      insn = emit_move_insn (mem, reg);
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    reg, gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM));
    }

  /* Save CR if we use any that must be preserved.  */
  if (info->cr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->cr_save_offset + sp_offset));
      rtx mem = gen_rtx_MEM (SImode, addr);

      set_mem_alias_set (mem, rs6000_sr_alias_set);

      /* If r12 was used to hold the original sp, copy cr into r0 now
	 that it's free.  */
      if (REGNO (frame_reg_rtx) == 12)
	{
	  cr_save_rtx = gen_rtx_REG (SImode, 0);
	  emit_insn (gen_movesi_from_cr (cr_save_rtx));
	}
      insn = emit_move_insn (mem, cr_save_rtx);

      /* Now, there's no way that dwarf2out_frame_debug_expr is going
	 to understand '(unspec:SI [(reg:CC 68) ...] 19)'.  But that's
	 OK.  All we have to do is specify that _one_ condition code
	 register is saved in this stack slot.  The thrower's epilogue
	 will then restore all the call-saved registers.
	 We use CR2_REGNO (70) to be compatible with gcc-2.95 on Linux.  */
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    cr_save_rtx, gen_rtx_REG (SImode, CR2_REGNO));
    }

  /* Update stack and set back pointer unless this is V.4, 
     for which it was done previously.  */
  if (info->push_p && DEFAULT_ABI != ABI_V4)
    rs6000_emit_allocate_stack (info->total_size, FALSE);

  /* Set frame pointer, if needed.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM),
			     sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If we are using RS6000_PIC_OFFSET_TABLE_REGNUM, we need to set it up.  */
  if ((TARGET_TOC && TARGET_MINIMAL_TOC && get_pool_size () != 0)
      || (DEFAULT_ABI == ABI_V4 && flag_pic == 1
	  && regs_ever_live[RS6000_PIC_OFFSET_TABLE_REGNUM]))
  {
    /* If emit_load_toc_table will use the link register, we need to save
       it.  We use R11 for this purpose because emit_load_toc_table
       can use register 0.  This allows us to use a plain 'blr' to return
       from the procedure more often.  */
    int save_LR_around_toc_setup = (TARGET_ELF && flag_pic != 0
				    && ! info->lr_save_p
				    && EXIT_BLOCK_PTR->pred != NULL);
    if (save_LR_around_toc_setup)
      emit_move_insn (gen_rtx_REG (Pmode, 11), 
		      gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM));
    
    rs6000_emit_load_toc_table (TRUE);

    if (save_LR_around_toc_setup)
      emit_move_insn (gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM), 
		      gen_rtx_REG (Pmode, 11));
  }

  if (DEFAULT_ABI == ABI_DARWIN
      && flag_pic && current_function_uses_pic_offset_table)
    {
      rtx dest = gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM);

      rs6000_maybe_dead (emit_insn (gen_load_macho_picbase (dest)));

      rs6000_maybe_dead (
	emit_move_insn (gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM),
			gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)));
    }
}

/* Write function prologue.  */

static void
rs6000_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (TARGET_DEBUG_STACK)
    debug_stack_info (info);

  /* Write .extern for any function we will call to save and restore
     fp values.  */
  if (info->first_fp_reg_save < 64
      && !FP_SAVE_INLINE (info->first_fp_reg_save))
    fprintf (file, "\t.extern %s%d%s\n\t.extern %s%d%s\n",
	     SAVE_FP_PREFIX, info->first_fp_reg_save - 32, SAVE_FP_SUFFIX,
	     RESTORE_FP_PREFIX, info->first_fp_reg_save - 32,
	     RESTORE_FP_SUFFIX);

  /* Write .extern for AIX common mode routines, if needed.  */
  if (! TARGET_POWER && ! TARGET_POWERPC && ! common_mode_defined)
    {
      fputs ("\t.extern __mulh\n", file);
      fputs ("\t.extern __mull\n", file);
      fputs ("\t.extern __divss\n", file);
      fputs ("\t.extern __divus\n", file);
      fputs ("\t.extern __quoss\n", file);
      fputs ("\t.extern __quous\n", file);
      common_mode_defined = 1;
    }

  if (! HAVE_prologue)
    {
      start_sequence ();

      /* A NOTE_INSN_DELETED is supposed to be at the start and end of
	 the "toplevel" insn chain.  */
      emit_note (0, NOTE_INSN_DELETED);
      rs6000_emit_prologue ();
      emit_note (0, NOTE_INSN_DELETED);

      /* Expand INSN_ADDRESSES so final() doesn't crash. */
      {
	rtx insn;
	unsigned addr = 0;
	for (insn = get_insns (); insn != 0; insn = NEXT_INSN (insn))
	  {
	    INSN_ADDRESSES_NEW (insn, addr);
	    addr += 4;
	  }
      }

      if (TARGET_DEBUG_STACK)
	debug_rtx_list (get_insns (), 100);
      final (get_insns (), file, FALSE, FALSE);
      end_sequence ();
    }

  rs6000_pic_labelno++;
}
  
/* Emit function epilogue as insns.

   At present, dwarf2out_frame_debug_expr doesn't understand
   register restores, so we don't bother setting RTX_FRAME_RELATED_P
   anywhere in the epilogue.  Most of the insns below would in any case
   need special notes to explain where r11 is in relation to the stack.  */

void
rs6000_emit_epilogue (sibcall)
     int sibcall;
{
  rs6000_stack_t *info;
  int restoring_FPRs_inline;
  int using_load_multiple;
  int using_mfcr_multiple;
  int use_backchain_to_restore_sp;
  int sp_offset = 0;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, 1);
  rtx frame_reg_rtx = sp_reg_rtx;
  enum machine_mode reg_mode = TARGET_POWERPC64 ? DImode : SImode;
  int reg_size = TARGET_POWERPC64 ? 8 : 4;
  int i;

  info = rs6000_stack_info ();
  using_load_multiple = (TARGET_MULTIPLE && ! TARGET_POWERPC64
			 && info->first_gp_reg_save < 31);
  restoring_FPRs_inline = (sibcall
			   || current_function_calls_eh_return
			   || info->first_fp_reg_save == 64
			   || FP_SAVE_INLINE (info->first_fp_reg_save));
  use_backchain_to_restore_sp = (frame_pointer_needed 
				 || current_function_calls_alloca
				 || info->total_size > 32767);
  using_mfcr_multiple = (rs6000_cpu == PROCESSOR_PPC601
			 || rs6000_cpu == PROCESSOR_PPC603
			 || rs6000_cpu == PROCESSOR_PPC750
			 || optimize_size);

  /* If we have a frame pointer, a call to alloca,  or a large stack
     frame, restore the old stack pointer using the backchain.  Otherwise,
     we know what size to update it with.  */
  if (use_backchain_to_restore_sp)
    {
      /* Under V.4, don't reset the stack pointer until after we're done
	 loading the saved registers.  */
      if (DEFAULT_ABI == ABI_V4)
	frame_reg_rtx = gen_rtx_REG (Pmode, 11);

      emit_move_insn (frame_reg_rtx,
		      gen_rtx_MEM (Pmode, sp_reg_rtx));
      
    }
  else if (info->push_p)
    {
      if (DEFAULT_ABI == ABI_V4)
	sp_offset = info->total_size;
      else
	{
	  emit_insn (TARGET_32BIT
		     ? gen_addsi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (info->total_size))
		     : gen_adddi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (info->total_size)));
	}
    }
  
  /* Restore AltiVec registers if needed.  */
  if (TARGET_ALTIVEC_ABI && info->altivec_size != 0)
    {
      int i;

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	  {
	    rtx addr, areg, mem;

	    areg = gen_rtx_REG (Pmode, 0);
	    emit_move_insn
	      (areg, GEN_INT (info->altivec_save_offset
			      + sp_offset
			      + 16 * (i - info->first_altivec_reg_save)));

	    /* AltiVec addressing mode is [reg+reg].  */
	    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, areg);
	    mem = gen_rtx_MEM (V4SImode, addr);
	    set_mem_alias_set (mem, rs6000_sr_alias_set);

	    emit_move_insn (gen_rtx_REG (V4SImode, i), mem);
	  }
    }

  /* Restore VRSAVE if needed.  */
  if (TARGET_ALTIVEC_ABI && info->vrsave_mask != 0)
    {
      rtx addr, mem, reg;

      addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			   GEN_INT (info->vrsave_save_offset + sp_offset));
      mem = gen_rtx_MEM (SImode, addr);
      set_mem_alias_set (mem, rs6000_sr_alias_set);
      reg = gen_rtx_REG (SImode, 12);
      emit_move_insn (reg, mem);

      emit_insn (generate_set_vrsave (reg, info, 1));
    }

  /* Get the old lr if we saved it.  */
  if (info->lr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->lr_save_offset + sp_offset));
      rtx mem = gen_rtx_MEM (Pmode, addr);

      set_mem_alias_set (mem, rs6000_sr_alias_set);

      emit_move_insn (gen_rtx_REG (Pmode, 0), mem);
    }
  
  /* Get the old cr if we saved it.  */
  if (info->cr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->cr_save_offset + sp_offset));
      rtx mem = gen_rtx_MEM (SImode, addr);

      set_mem_alias_set (mem, rs6000_sr_alias_set);

      emit_move_insn (gen_rtx_REG (SImode, 12), mem);
    }
  
  /* Set LR here to try to overlap restores below.  */
  if (info->lr_save_p)
    emit_move_insn (gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM),
		    gen_rtx_REG (Pmode, 0));
  
  /* Load exception handler data registers, if needed.  */
  if (current_function_calls_eh_return)
    {
      unsigned int i, regno;

      for (i = 0; ; ++i)
	{
	  rtx addr, mem;

	  regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;

	  addr = plus_constant (frame_reg_rtx,
				info->ehrd_offset + sp_offset
				+ reg_size * (int) i);
	  mem = gen_rtx_MEM (reg_mode, addr);
	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  emit_move_insn (gen_rtx_REG (reg_mode, regno), mem);
	}
    }
  
  /* Restore GPRs.  This is done as a PARALLEL if we are using
     the load-multiple instructions.  */
  if (using_load_multiple)
    {
      rtvec p;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	{
	  rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
				   GEN_INT (info->gp_save_offset 
					    + sp_offset 
					    + reg_size * i));
	  rtx mem = gen_rtx_MEM (reg_mode, addr);

	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  RTVEC_ELT (p, i) = 
	    gen_rtx_SET (VOIDmode,
			 gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			 mem);
	}
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }
  else
    for (i = 0; i < 32 - info->first_gp_reg_save; i++)
      if ((regs_ever_live[info->first_gp_reg_save+i] 
	   && ! call_used_regs[info->first_gp_reg_save+i])
	  || (i+info->first_gp_reg_save == RS6000_PIC_OFFSET_TABLE_REGNUM
	      && ((DEFAULT_ABI == ABI_V4 && flag_pic != 0)
		  || (DEFAULT_ABI == ABI_DARWIN && flag_pic))))
	{
	  rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
				   GEN_INT (info->gp_save_offset 
					    + sp_offset 
					    + reg_size * i));
	  rtx mem = gen_rtx_MEM (reg_mode, addr);

	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  emit_move_insn (gen_rtx_REG (reg_mode, 
				       info->first_gp_reg_save + i),
			  mem);
	}

  /* Restore fpr's if we need to do it without calling a function.  */
  if (restoring_FPRs_inline)
    for (i = 0; i < 64 - info->first_fp_reg_save; i++)
      if ((regs_ever_live[info->first_fp_reg_save+i] 
	   && ! call_used_regs[info->first_fp_reg_save+i]))
	{
	  rtx addr, mem;
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->fp_save_offset 
					+ sp_offset 
					+ 8 * i));
	  mem = gen_rtx_MEM (DFmode, addr);
	  set_mem_alias_set (mem, rs6000_sr_alias_set);

	  emit_move_insn (gen_rtx_REG (DFmode, 
				       info->first_fp_reg_save + i),
			  mem);
	}

  /* If we saved cr, restore it here.  Just those that were used.  */
  if (info->cr_save_p)
    {
      rtx r12_rtx = gen_rtx_REG (SImode, 12);
      int count = 0;
      
      if (using_mfcr_multiple)
	{
	  for (i = 0; i < 8; i++)
	    if (regs_ever_live[CR0_REGNO+i] && ! call_used_regs[CR0_REGNO+i])
	      count++;
	  if (count == 0)
	    abort ();
	}

      if (using_mfcr_multiple && count > 1)
	{
	  rtvec p;
	  int ndx;
	  
	  p = rtvec_alloc (count);

	  ndx = 0;
	  for (i = 0; i < 8; i++)
	    if (regs_ever_live[CR0_REGNO+i] && ! call_used_regs[CR0_REGNO+i])
	      {
		rtvec r = rtvec_alloc (2);
		RTVEC_ELT (r, 0) = r12_rtx;
		RTVEC_ELT (r, 1) = GEN_INT (1 << (7-i));
		RTVEC_ELT (p, ndx) =
		  gen_rtx_SET (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO+i), 
			       gen_rtx_UNSPEC (CCmode, r, 20));
		ndx++;
	      }
	  emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
	  if (ndx != count)
	    abort ();
	}
      else
	for (i = 0; i < 8; i++)
	  if (regs_ever_live[CR0_REGNO+i] && ! call_used_regs[CR0_REGNO+i])
	    {
	      emit_insn (gen_movsi_to_cr_one (gen_rtx_REG (CCmode, 
							   CR0_REGNO+i),
					      r12_rtx));
	    }
    }

  /* If this is V.4, unwind the stack pointer after all of the loads
     have been done.  We need to emit a block here so that sched
     doesn't decide to move the sp change before the register restores
     (which may not have any obvious dependency on the stack).  This
     doesn't hurt performance, because there is no scheduling that can
     be done after this point.  */
  if (DEFAULT_ABI == ABI_V4)
    {
      if (frame_reg_rtx != sp_reg_rtx)
	  rs6000_emit_stack_tie ();

      if (use_backchain_to_restore_sp)
	{
	  emit_move_insn (sp_reg_rtx, frame_reg_rtx);
	}
      else if (sp_offset != 0)
	{
	  emit_insn (Pmode == SImode
		     ? gen_addsi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (sp_offset))
		     : gen_adddi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (sp_offset)));
	}
    }

  if (current_function_calls_eh_return)
    {
      rtx sa = EH_RETURN_STACKADJ_RTX;
      emit_insn (Pmode == SImode
		 ? gen_addsi3 (sp_reg_rtx, sp_reg_rtx, sa)
		 : gen_adddi3 (sp_reg_rtx, sp_reg_rtx, sa));
    }

  if (!sibcall)
    {
      rtvec p;
      if (! restoring_FPRs_inline)
	p = rtvec_alloc (3 + 64 - info->first_fp_reg_save);
      else
	p = rtvec_alloc (2);

      RTVEC_ELT (p, 0) = gen_rtx_RETURN (VOIDmode);
      RTVEC_ELT (p, 1) = gen_rtx_USE (VOIDmode, 
				      gen_rtx_REG (Pmode, 
						   LINK_REGISTER_REGNUM));

      /* If we have to restore more than two FP registers, branch to the
	 restore function.  It will return to our caller.  */
      if (! restoring_FPRs_inline)
	{
	  int i;
	  char rname[30];
	  const char *alloc_rname;

	  sprintf (rname, "%s%d%s", RESTORE_FP_PREFIX, 
		   info->first_fp_reg_save - 32, RESTORE_FP_SUFFIX);
	  alloc_rname = ggc_strdup (rname);
	  RTVEC_ELT (p, 2) = gen_rtx_USE (VOIDmode,
					  gen_rtx_SYMBOL_REF (Pmode,
							      alloc_rname));

	  for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	    {
	      rtx addr, mem;
	      addr = gen_rtx_PLUS (Pmode, sp_reg_rtx,
				   GEN_INT (info->fp_save_offset + 8*i));
	      mem = gen_rtx_MEM (DFmode, addr);
	      set_mem_alias_set (mem, rs6000_sr_alias_set);

	      RTVEC_ELT (p, i+3) = 
		gen_rtx_SET (VOIDmode,
			     gen_rtx_REG (DFmode, info->first_fp_reg_save + i),
			     mem);
	    }
	}
      
      emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }
}

/* Write function epilogue.  */

static void
rs6000_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  rs6000_stack_t *info = rs6000_stack_info ();
  int optional_tbtab = (optimize_size || TARGET_ELF) ? 0 : 1;

  if (! HAVE_epilogue)
    {
      rtx insn = get_last_insn ();
      /* If the last insn was a BARRIER, we don't have to write anything except
	 the trace table.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn == 0 ||  GET_CODE (insn) != BARRIER)
	{
	  /* This is slightly ugly, but at least we don't have two
	     copies of the epilogue-emitting code.  */
	  start_sequence ();

	  /* A NOTE_INSN_DELETED is supposed to be at the start
	     and end of the "toplevel" insn chain.  */
	  emit_note (0, NOTE_INSN_DELETED);
	  rs6000_emit_epilogue (FALSE);
	  emit_note (0, NOTE_INSN_DELETED);

	  /* Expand INSN_ADDRESSES so final() doesn't crash. */
	  {
	    rtx insn;
	    unsigned addr = 0;
	    for (insn = get_insns (); insn != 0; insn = NEXT_INSN (insn))
	      {
		INSN_ADDRESSES_NEW (insn, addr);
		addr += 4;
	      }
	  }

	  if (TARGET_DEBUG_STACK)
	    debug_rtx_list (get_insns (), 100);
	  final (get_insns (), file, FALSE, FALSE);
	  end_sequence ();
	}
    }

  /* Output a traceback table here.  See /usr/include/sys/debug.h for info
     on its format.

     We don't output a traceback table if -finhibit-size-directive was
     used.  The documentation for -finhibit-size-directive reads
     ``don't output a @code{.size} assembler directive, or anything
     else that would cause trouble if the function is split in the
     middle, and the two halves are placed at locations far apart in
     memory.''  The traceback table has this property, since it
     includes the offset from the start of the function to the
     traceback table itself.

     System V.4 Powerpc's (and the embedded ABI derived from it) use a
     different traceback table.  */
  if (DEFAULT_ABI == ABI_AIX && ! flag_inhibit_size_directive)
    {
      const char *fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
      const char *language_string = lang_hooks.name;
      int fixed_parms = 0, float_parms = 0, parm_info = 0;
      int i;

      while (*fname == '.')	/* V.4 encodes . in the name */
	fname++;

      /* Need label immediately before tbtab, so we can compute its offset
	 from the function start.  */
      if (*fname == '*')
	++fname;
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
      ASM_OUTPUT_LABEL (file, fname);

      /* The .tbtab pseudo-op can only be used for the first eight
	 expressions, since it can't handle the possibly variable
	 length fields that follow.  However, if you omit the optional
	 fields, the assembler outputs zeros for all optional fields
	 anyways, giving each variable length field is minimum length
	 (as defined in sys/debug.h).  Thus we can not use the .tbtab
	 pseudo-op at all.  */

      /* An all-zero word flags the start of the tbtab, for debuggers
	 that have to find it by searching forward from the entry
	 point or from the current pc.  */
      fputs ("\t.long 0\n", file);

      /* Tbtab format type.  Use format type 0.  */
      fputs ("\t.byte 0,", file);

      /* Language type.  Unfortunately, there doesn't seem to be any
	 official way to get this info, so we use language_string.  C
	 is 0.  C++ is 9.  No number defined for Obj-C, so use the
	 value for C for now.  There is no official value for Java,
         although IBM appears to be using 13.  There is no official value
	 for Chill, so we've chosen 44 pseudo-randomly.  */
      if (! strcmp (language_string, "GNU C")
	  || ! strcmp (language_string, "GNU Objective-C"))
	i = 0;
      else if (! strcmp (language_string, "GNU F77"))
	i = 1;
      else if (! strcmp (language_string, "GNU Ada"))
	i = 3;
      else if (! strcmp (language_string, "GNU Pascal"))
	i = 2;
      else if (! strcmp (language_string, "GNU C++"))
	i = 9;
      else if (! strcmp (language_string, "GNU Java"))
	i = 13;
      else if (! strcmp (language_string, "GNU CHILL"))
	i = 44;
      else
	abort ();
      fprintf (file, "%d,", i);

      /* 8 single bit fields: global linkage (not set for C extern linkage,
	 apparently a PL/I convention?), out-of-line epilogue/prologue, offset
	 from start of procedure stored in tbtab, internal function, function
	 has controlled storage, function has no toc, function uses fp,
	 function logs/aborts fp operations.  */
      /* Assume that fp operations are used if any fp reg must be saved.  */
      fprintf (file, "%d,",
	       (optional_tbtab << 5) | ((info->first_fp_reg_save != 64) << 1));

      /* 6 bitfields: function is interrupt handler, name present in
	 proc table, function calls alloca, on condition directives
	 (controls stack walks, 3 bits), saves condition reg, saves
	 link reg.  */
      /* The `function calls alloca' bit seems to be set whenever reg 31 is
	 set up as a frame pointer, even when there is no alloca call.  */
      fprintf (file, "%d,",
	       ((optional_tbtab << 6)
		| ((optional_tbtab & frame_pointer_needed) << 5)
		| (info->cr_save_p << 1)
		| (info->lr_save_p)));

      /* 3 bitfields: saves backchain, fixup code, number of fpr saved
	 (6 bits).  */
      fprintf (file, "%d,",
	       (info->push_p << 7) | (64 - info->first_fp_reg_save));

      /* 2 bitfields: spare bits (2 bits), number of gpr saved (6 bits).  */
      fprintf (file, "%d,", (32 - first_reg_to_save ()));

      if (optional_tbtab)
	{
	  /* Compute the parameter info from the function decl argument
	     list.  */
	  tree decl;
	  int next_parm_info_bit = 31;

	  for (decl = DECL_ARGUMENTS (current_function_decl);
	       decl; decl = TREE_CHAIN (decl))
	    {
	      rtx parameter = DECL_INCOMING_RTL (decl);
	      enum machine_mode mode = GET_MODE (parameter);

	      if (GET_CODE (parameter) == REG)
		{
		  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
		    {
		      int bits;

		      float_parms++;

		      if (mode == SFmode)
			bits = 0x2;
		      else if (mode == DFmode)
			bits = 0x3;
		      else
			abort ();

		      /* If only one bit will fit, don't or in this entry.  */
		      if (next_parm_info_bit > 0)
			parm_info |= (bits << (next_parm_info_bit - 1));
		      next_parm_info_bit -= 2;
		    }
		  else
		    {
		      fixed_parms += ((GET_MODE_SIZE (mode)
				       + (UNITS_PER_WORD - 1))
				      / UNITS_PER_WORD);
		      next_parm_info_bit -= 1;
		    }
		}
	    }
	}

      /* Number of fixed point parameters.  */
      /* This is actually the number of words of fixed point parameters; thus
	 an 8 byte struct counts as 2; and thus the maximum value is 8.  */
      fprintf (file, "%d,", fixed_parms);

      /* 2 bitfields: number of floating point parameters (7 bits), parameters
	 all on stack.  */
      /* This is actually the number of fp registers that hold parameters;
	 and thus the maximum value is 13.  */
      /* Set parameters on stack bit if parameters are not in their original
	 registers, regardless of whether they are on the stack?  Xlc
	 seems to set the bit when not optimizing.  */
      fprintf (file, "%d\n", ((float_parms << 1) | (! optimize)));

      if (! optional_tbtab)
	return;

      /* Optional fields follow.  Some are variable length.  */

      /* Parameter types, left adjusted bit fields: 0 fixed, 10 single float,
	 11 double float.  */
      /* There is an entry for each parameter in a register, in the order that
	 they occur in the parameter list.  Any intervening arguments on the
	 stack are ignored.  If the list overflows a long (max possible length
	 34 bits) then completely leave off all elements that don't fit.  */
      /* Only emit this long if there was at least one parameter.  */
      if (fixed_parms || float_parms)
	fprintf (file, "\t.long %d\n", parm_info);

      /* Offset from start of code to tb table.  */
      fputs ("\t.long ", file);
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
#if TARGET_AIX
      RS6000_OUTPUT_BASENAME (file, fname);
#else
      assemble_name (file, fname);
#endif
      fputs ("-.", file);
#if TARGET_AIX
      RS6000_OUTPUT_BASENAME (file, fname);
#else
      assemble_name (file, fname);
#endif
      putc ('\n', file);

      /* Interrupt handler mask.  */
      /* Omit this long, since we never set the interrupt handler bit
	 above.  */

      /* Number of CTL (controlled storage) anchors.  */
      /* Omit this long, since the has_ctl bit is never set above.  */

      /* Displacement into stack of each CTL anchor.  */
      /* Omit this list of longs, because there are no CTL anchors.  */

      /* Length of function name.  */
      fprintf (file, "\t.short %d\n", (int) strlen (fname));

      /* Function name.  */
      assemble_string (fname, strlen (fname));

      /* Register for alloca automatic storage; this is always reg 31.
	 Only emit this if the alloca bit was set above.  */
      if (frame_pointer_needed)
	fputs ("\t.byte 31\n", file);

      fputs ("\t.align 2\n", file);
    }
  return;
}

/* A C compound statement that outputs the assembler code for a thunk
   function, used to implement C++ virtual function calls with
   multiple inheritance.  The thunk acts as a wrapper around a virtual
   function, adjusting the implicit object parameter before handing
   control off to the real function.

   First, emit code to add the integer DELTA to the location that
   contains the incoming first argument.  Assume that this argument
   contains a pointer, and is the one used to pass the `this' pointer
   in C++.  This is the incoming argument *before* the function
   prologue, e.g. `%o0' on a sparc.  The addition must preserve the
   values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does
   not touch the return address.  Hence returning from FUNCTION will
   return to whoever called the current `thunk'.

   The effect must be as if FUNCTION had been called directly with the
   adjusted first argument.  This macro is responsible for emitting
   all of the code for a thunk function; output_function_prologue()
   and output_function_epilogue() are not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already
   been extracted from it.)  It might possibly be useful on some
   targets, but probably not.

   If you do not define this macro, the target-independent code in the
   C++ frontend will generate a less efficient heavyweight thunk that
   calls FUNCTION instead of jumping to it.  The generic approach does
   not support varargs.  */

void
output_mi_thunk (file, thunk_fndecl, delta, function)
     FILE *file;
     tree thunk_fndecl ATTRIBUTE_UNUSED;
     int delta;
     tree function;
{
  const char *this_reg =
    reg_names[ aggregate_value_p (TREE_TYPE (TREE_TYPE (function))) ? 4 : 3 ];
  const char *prefix;
  const char *fname;
  const char *r0	 = reg_names[0];
  const char *toc	 = reg_names[2];
  const char *schain	 = reg_names[11];
  const char *r12	 = reg_names[12];
  char buf[512];
  static int labelno = 0;

  /* Small constants that can be done by one add instruction.  */
  if (delta >= -32768 && delta <= 32767)
    {
      if (! TARGET_NEW_MNEMONICS)
	fprintf (file, "\tcal %s,%d(%s)\n", this_reg, delta, this_reg);
      else
	fprintf (file, "\taddi %s,%s,%d\n", this_reg, this_reg, delta);
    }

  /* 64-bit constants.  If "int" is 32 bits, we'll never hit this abort.  */
  else if (TARGET_64BIT && (delta < -2147483647 - 1 || delta > 2147483647))
    abort ();

  /* Large constants that can be done by one addis instruction.  */
  else if ((delta & 0xffff) == 0)
    asm_fprintf (file, "\t{cau|addis} %s,%s,%d\n", this_reg, this_reg,
		 delta >> 16);

  /* 32-bit constants that can be done by an add and addis instruction.  */
  else
    {
      /* Break into two pieces, propagating the sign bit from the low
	 word to the upper word.  */
      int delta_low  = ((delta & 0xffff) ^ 0x8000) - 0x8000;
      int delta_high = (delta - delta_low) >> 16;

      asm_fprintf (file, "\t{cau|addis} %s,%s,%d\n", this_reg, this_reg,
		   delta_high);

      if (! TARGET_NEW_MNEMONICS)
	fprintf (file, "\tcal %s,%d(%s)\n", this_reg, delta_low, this_reg);
      else
	fprintf (file, "\taddi %s,%s,%d\n", this_reg, this_reg, delta_low);
    }

  /* Get the prefix in front of the names.  */
  switch (DEFAULT_ABI)
    {
    default:
      abort ();

    case ABI_AIX:
      prefix = ".";
      break;

    case ABI_V4:
    case ABI_AIX_NODESC:
      prefix = "";
      break;
    }

  /* If the function is compiled in this module, jump to it directly.
     Otherwise, load up its address and jump to it.  */

  fname = XSTR (XEXP (DECL_RTL (function), 0), 0);

  if (current_file_function_operand (XEXP (DECL_RTL (function), 0), VOIDmode)
      && ! lookup_attribute ("longcall",
			     TYPE_ATTRIBUTES (TREE_TYPE (function))))
    {
      fprintf (file, "\tb %s", prefix);
      assemble_name (file, fname);
      if (DEFAULT_ABI == ABI_V4 && flag_pic) fputs ("@local", file);
      putc ('\n', file);
    }

  else
    {
      switch (DEFAULT_ABI)
	{
	default:
	  abort ();

	case ABI_AIX:
	  /* Set up a TOC entry for the function.  */
	  ASM_GENERATE_INTERNAL_LABEL (buf, "Lthunk", labelno);
	  toc_section ();
	  ASM_OUTPUT_INTERNAL_LABEL (file, "Lthunk", labelno);
	  labelno++;

	  if (TARGET_MINIMAL_TOC)
	    fputs (TARGET_32BIT ? "\t.long " : DOUBLE_INT_ASM_OP, file);
	  else
	    {
	      fputs ("\t.tc ", file);
	      assemble_name (file, fname);
	      fputs ("[TC],", file);
	    }
	  assemble_name (file, fname);
	  putc ('\n', file);
	  if (TARGET_ELF)
	    function_section (current_function_decl);
	  else
	    text_section ();
	  if (TARGET_MINIMAL_TOC)
	    asm_fprintf (file, (TARGET_32BIT)
			 ? "\t{l|lwz} %s,%s(%s)\n" : "\tld %s,%s(%s)\n", r12,
			 TARGET_ELF ? ".LCTOC0@toc" : ".LCTOC..1", toc);
	  asm_fprintf (file, (TARGET_32BIT) ? "\t{l|lwz} %s," : "\tld %s,", r12);
	  assemble_name (file, buf);
	  if (TARGET_ELF && TARGET_MINIMAL_TOC)
	    fputs ("-(.LCTOC1)", file);
	  asm_fprintf (file, "(%s)\n", TARGET_MINIMAL_TOC ? r12 : toc);
	  asm_fprintf (file,
		       (TARGET_32BIT) ? "\t{l|lwz} %s,0(%s)\n" : "\tld %s,0(%s)\n",
		       r0, r12);

	  asm_fprintf (file,
		       (TARGET_32BIT) ? "\t{l|lwz} %s,4(%s)\n" : "\tld %s,8(%s)\n",
		       toc, r12);

	  asm_fprintf (file, "\tmtctr %s\n", r0);
	  asm_fprintf (file,
		       (TARGET_32BIT) ? "\t{l|lwz} %s,8(%s)\n" : "\tld %s,16(%s)\n",
		       schain, r12);

	  asm_fprintf (file, "\tbctr\n");
	  break;

	case ABI_AIX_NODESC:
	case ABI_V4:
	  fprintf (file, "\tb %s", prefix);
	  assemble_name (file, fname);
	  if (flag_pic) fputs ("@plt", file);
	  putc ('\n', file);
	  break;

#if TARGET_MACHO
	case ABI_DARWIN:
	  fprintf (file, "\tb %s", prefix);
	  if (flag_pic && !machopic_name_defined_p (fname))
	    assemble_name (file, machopic_stub_name (fname));
	  else
	    assemble_name (file, fname);
	  putc ('\n', file);
	  break;
#endif
	}
    }
}


/* A quick summary of the various types of 'constant-pool tables'
   under PowerPC:

   Target	Flags		Name		One table per	
   AIX		(none)		AIX TOC		object file
   AIX		-mfull-toc	AIX TOC		object file
   AIX		-mminimal-toc	AIX minimal TOC	translation unit
   SVR4/EABI	(none)		SVR4 SDATA	object file
   SVR4/EABI	-fpic		SVR4 pic	object file
   SVR4/EABI	-fPIC		SVR4 PIC	translation unit
   SVR4/EABI	-mrelocatable	EABI TOC	function
   SVR4/EABI	-maix		AIX TOC		object file
   SVR4/EABI	-maix -mminimal-toc 
				AIX minimal TOC	translation unit

   Name			Reg.	Set by	entries	      contains:
					made by	 addrs?	fp?	sum?

   AIX TOC		2	crt0	as	 Y	option	option
   AIX minimal TOC	30	prolog	gcc	 Y	Y	option
   SVR4 SDATA		13	crt0	gcc	 N	Y	N
   SVR4 pic		30	prolog	ld	 Y	not yet	N
   SVR4 PIC		30	prolog	gcc	 Y	option	option
   EABI TOC		30	prolog	gcc	 Y	option	option

*/

/* Hash table stuff for keeping track of TOC entries.  */

struct toc_hash_struct 
{
  /* `key' will satisfy CONSTANT_P; in fact, it will satisfy
     ASM_OUTPUT_SPECIAL_POOL_ENTRY_P.  */
  rtx key;
  enum machine_mode key_mode;
  int labelno;
};

static htab_t toc_hash_table;

/* Hash functions for the hash table.  */

static unsigned
rs6000_hash_constant (k)
     rtx k;
{
  unsigned result = (GET_CODE (k) << 3) ^ GET_MODE (k);
  const char *format = GET_RTX_FORMAT (GET_CODE (k));
  int flen = strlen (format);
  int fidx;
  
  if (GET_CODE (k) == LABEL_REF)
    return result * 1231 + X0INT (XEXP (k, 0), 3);

  if (GET_CODE (k) == CONST_DOUBLE)
    fidx = 1;
  else if (GET_CODE (k) == CODE_LABEL)
    fidx = 3;
  else
    fidx = 0;

  for (; fidx < flen; fidx++)
    switch (format[fidx])
      {
      case 's':
	{
	  unsigned i, len;
	  const char *str = XSTR (k, fidx);
	  len = strlen (str);
	  result = result * 613 + len;
	  for (i = 0; i < len; i++)
	    result = result * 613 + (unsigned) str[i];
	  break;
	}
      case 'u':
      case 'e':
	result = result * 1231 + rs6000_hash_constant (XEXP (k, fidx));
	break;
      case 'i':
      case 'n':
	result = result * 613 + (unsigned) XINT (k, fidx);
	break;
      case 'w':
	if (sizeof (unsigned) >= sizeof (HOST_WIDE_INT))
	  result = result * 613 + (unsigned) XWINT (k, fidx);
	else
	  {
	    size_t i;
	    for (i = 0; i < sizeof(HOST_WIDE_INT)/sizeof(unsigned); i++)
	      result = result * 613 + (unsigned) (XWINT (k, fidx)
						  >> CHAR_BIT * i);
	  }
	break;
      default:
	abort ();
      }
  return result;
}

static unsigned
toc_hash_function (hash_entry)
     const void * hash_entry;
{
  const struct toc_hash_struct *thc = 
    (const struct toc_hash_struct *) hash_entry;
  return rs6000_hash_constant (thc->key) ^ thc->key_mode;
}

/* Compare H1 and H2 for equivalence.  */

static int
toc_hash_eq (h1, h2)
     const void * h1;
     const void * h2;
{
  rtx r1 = ((const struct toc_hash_struct *) h1)->key;
  rtx r2 = ((const struct toc_hash_struct *) h2)->key;

  if (((const struct toc_hash_struct *) h1)->key_mode
      != ((const struct toc_hash_struct *) h2)->key_mode)
    return 0;

  /* Gotcha:  One of these const_doubles will be in memory.
     The other may be on the constant-pool chain.
     So rtx_equal_p will think they are different...  */
  if (r1 == r2)
    return 1;
  if (GET_CODE (r1) != GET_CODE (r2)
      || GET_MODE (r1) != GET_MODE (r2))
    return 0;
  if (GET_CODE (r1) == CONST_DOUBLE)
    {
      int format_len = strlen (GET_RTX_FORMAT (CONST_DOUBLE));
      int i;
      for (i = 1; i < format_len; i++)
	if (XWINT (r1, i) != XWINT (r2, i))
	  return 0;
      
      return 1;
    }
  else if (GET_CODE (r1) == LABEL_REF)
    return (CODE_LABEL_NUMBER (XEXP (r1, 0)) 
	    == CODE_LABEL_NUMBER (XEXP (r2, 0)));
  else
    return rtx_equal_p (r1, r2);
}

/* Mark the hash table-entry HASH_ENTRY.  */

static int
toc_hash_mark_entry (hash_slot, unused)
     void ** hash_slot;
     void * unused ATTRIBUTE_UNUSED;
{
  const struct toc_hash_struct * hash_entry = 
    *(const struct toc_hash_struct **) hash_slot;
  rtx r = hash_entry->key;
  ggc_set_mark (hash_entry);
  /* For CODE_LABELS, we don't want to drag in the whole insn chain...  */
  if (GET_CODE (r) == LABEL_REF)
    {
      ggc_set_mark (r);
      ggc_set_mark (XEXP (r, 0));
    }
  else
    ggc_mark_rtx (r);
  return 1;
}

/* Mark all the elements of the TOC hash-table *HT.  */

static void
toc_hash_mark_table (vht)
     void *vht;
{
  htab_t *ht = vht;
  
  htab_traverse (*ht, toc_hash_mark_entry, (void *)0);
}

/* These are the names given by the C++ front-end to vtables, and
   vtable-like objects.  Ideally, this logic should not be here;
   instead, there should be some programmatic way of inquiring as
   to whether or not an object is a vtable.  */

#define VTABLE_NAME_P(NAME)				\
  (strncmp ("_vt.", name, strlen("_vt.")) == 0		\
  || strncmp ("_ZTV", name, strlen ("_ZTV")) == 0	\
  || strncmp ("_ZTT", name, strlen ("_ZTT")) == 0	\
  || strncmp ("_ZTC", name, strlen ("_ZTC")) == 0) 

void
rs6000_output_symbol_ref (file, x)
     FILE *file;
     rtx x;
{
  /* Currently C++ toc references to vtables can be emitted before it
     is decided whether the vtable is public or private.  If this is
     the case, then the linker will eventually complain that there is
     a reference to an unknown section.  Thus, for vtables only, 
     we emit the TOC reference to reference the symbol and not the
     section.  */
  const char *name = XSTR (x, 0);

  if (VTABLE_NAME_P (name)) 
    {
      RS6000_OUTPUT_BASENAME (file, name);
    }
  else
    assemble_name (file, name);
}

/* Output a TOC entry.  We derive the entry name from what is being
   written.  */

void
output_toc (file, x, labelno, mode)
     FILE *file;
     rtx x;
     int labelno;
     enum machine_mode mode;
{
  char buf[256];
  const char *name = buf;
  const char *real_name;
  rtx base = x;
  int offset = 0;

  if (TARGET_NO_TOC)
    abort ();

  /* When the linker won't eliminate them, don't output duplicate
     TOC entries (this happens on AIX if there is any kind of TOC,
     and on SVR4 under -fPIC or -mrelocatable).  */
  if (TARGET_TOC)
    {
      struct toc_hash_struct *h;
      void * * found;
      
      h = ggc_alloc (sizeof (*h));
      h->key = x;
      h->key_mode = mode;
      h->labelno = labelno;
      
      found = htab_find_slot (toc_hash_table, h, 1);
      if (*found == NULL)
	*found = h;
      else  /* This is indeed a duplicate.  
	       Set this label equal to that label.  */
	{
	  fputs ("\t.set ", file);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d,", labelno);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d\n", ((*(const struct toc_hash_struct **) 
					      found)->labelno));
	  return;
	}
    }

  /* If we're going to put a double constant in the TOC, make sure it's
     aligned properly when strict alignment is on.  */
  if (GET_CODE (x) == CONST_DOUBLE
      && STRICT_ALIGNMENT
      && GET_MODE_BITSIZE (mode) >= 64
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)) {
    ASM_OUTPUT_ALIGN (file, 3);
  }

  ASM_OUTPUT_INTERNAL_LABEL (file, "LC", labelno);

  /* Handle FP constants specially.  Note that if we have a minimal
     TOC, things we put here aren't actually in the TOC, so we can allow
     FP constants.  */
  if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
    {
      REAL_VALUE_TYPE rv;
      long k[2];

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_DOUBLE (rv, k);

      if (TARGET_64BIT)
	{
	  if (TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc FD_%lx_%lx[TC],",
		     k[0] & 0xffffffff, k[1] & 0xffffffff);
	  fprintf (file, "0x%lx%08lx\n",
		   k[0] & 0xffffffff, k[1] & 0xffffffff);
	  return;
	}
      else
	{
	  if (TARGET_MINIMAL_TOC)
	    fputs ("\t.long ", file);
	  else
	    fprintf (file, "\t.tc FD_%lx_%lx[TC],",
		     k[0] & 0xffffffff, k[1] & 0xffffffff);
	  fprintf (file, "0x%lx,0x%lx\n",
		   k[0] & 0xffffffff, k[1] & 0xffffffff);
	  return;
	}
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      REAL_VALUE_TYPE rv;
      long l;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);

      if (TARGET_64BIT)
	{
	  if (TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc FS_%lx[TC],", l & 0xffffffff);
	  fprintf (file, "0x%lx00000000\n", l & 0xffffffff);
	  return;
	}
      else
	{
	  if (TARGET_MINIMAL_TOC)
	    fputs ("\t.long ", file);
	  else
	    fprintf (file, "\t.tc FS_%lx[TC],", l & 0xffffffff);
	  fprintf (file, "0x%lx\n", l & 0xffffffff);
	  return;
	}
    }
  else if (GET_MODE (x) == VOIDmode
	   && (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE))
    {
      unsigned HOST_WIDE_INT low;
      HOST_WIDE_INT high;

      if (GET_CODE (x) == CONST_DOUBLE)
	{
	  low = CONST_DOUBLE_LOW (x);
	  high = CONST_DOUBLE_HIGH (x);
	}
      else
#if HOST_BITS_PER_WIDE_INT == 32
	{
	  low = INTVAL (x);
	  high = (low & 0x80000000) ? ~0 : 0;
	}
#else
	{
          low = INTVAL (x) & 0xffffffff;
          high = (HOST_WIDE_INT) INTVAL (x) >> 32;
	}
#endif

      /* TOC entries are always Pmode-sized, but since this
	 is a bigendian machine then if we're putting smaller
	 integer constants in the TOC we have to pad them.
	 (This is still a win over putting the constants in
	 a separate constant pool, because then we'd have
	 to have both a TOC entry _and_ the actual constant.)

	 For a 32-bit target, CONST_INT values are loaded and shifted
	 entirely within `low' and can be stored in one TOC entry.  */

      if (TARGET_64BIT && POINTER_SIZE < GET_MODE_BITSIZE (mode))
	abort ();/* It would be easy to make this work, but it doesn't now.  */

      if (POINTER_SIZE > GET_MODE_BITSIZE (mode))
	{
#if HOST_BITS_PER_WIDE_INT == 32
	  lshift_double (low, high, POINTER_SIZE - GET_MODE_BITSIZE (mode),
			 POINTER_SIZE, &low, &high, 0);
#else
	  low |= high << 32;
	  low <<= POINTER_SIZE - GET_MODE_BITSIZE (mode);
	  high = (HOST_WIDE_INT) low >> 32;
	  low &= 0xffffffff;
#endif
	}

      if (TARGET_64BIT)
	{
	  if (TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc ID_%lx_%lx[TC],",
		     (long) high & 0xffffffff, (long) low & 0xffffffff);
	  fprintf (file, "0x%lx%08lx\n",
		   (long) high & 0xffffffff, (long) low & 0xffffffff);
	  return;
	}
      else
	{
	  if (POINTER_SIZE < GET_MODE_BITSIZE (mode))
	    {
	      if (TARGET_MINIMAL_TOC)
		fputs ("\t.long ", file);
	      else
		fprintf (file, "\t.tc ID_%lx_%lx[TC],",
			 (long) high & 0xffffffff, (long) low & 0xffffffff);
	      fprintf (file, "0x%lx,0x%lx\n",
		       (long) high & 0xffffffff, (long) low & 0xffffffff);
	    }
	  else
	    {
	      if (TARGET_MINIMAL_TOC)
		fputs ("\t.long ", file);
	      else
		fprintf (file, "\t.tc IS_%lx[TC],", (long) low & 0xffffffff);
	      fprintf (file, "0x%lx\n", (long) low & 0xffffffff);
	    }
	  return;
	}
    }

  if (GET_CODE (x) == CONST)
    {
      if (GET_CODE (XEXP (x, 0)) != PLUS)
	abort ();

      base = XEXP (XEXP (x, 0), 0);
      offset = INTVAL (XEXP (XEXP (x, 0), 1));
    }
  
  if (GET_CODE (base) == SYMBOL_REF)
    name = XSTR (base, 0);
  else if (GET_CODE (base) == LABEL_REF)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (base, 0)));
  else if (GET_CODE (base) == CODE_LABEL)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (base));
  else
    abort ();

  STRIP_NAME_ENCODING (real_name, name);
  if (TARGET_MINIMAL_TOC)
    fputs (TARGET_32BIT ? "\t.long " : DOUBLE_INT_ASM_OP, file);
  else
    {
      fprintf (file, "\t.tc %s", real_name);

      if (offset < 0)
	fprintf (file, ".N%d", - offset);
      else if (offset)
	fprintf (file, ".P%d", offset);

      fputs ("[TC],", file);
    }

  /* Currently C++ toc references to vtables can be emitted before it
     is decided whether the vtable is public or private.  If this is
     the case, then the linker will eventually complain that there is
     a TOC reference to an unknown section.  Thus, for vtables only,
     we emit the TOC reference to reference the symbol and not the
     section.  */
  if (VTABLE_NAME_P (name))
    {
      RS6000_OUTPUT_BASENAME (file, name);
      if (offset < 0)
	fprintf (file, "%d", offset);
      else if (offset > 0)
	fprintf (file, "+%d", offset);
    }
  else
    output_addr_const (file, x);
  putc ('\n', file);
}

/* Output an assembler pseudo-op to write an ASCII string of N characters
   starting at P to FILE.

   On the RS/6000, we have to do this using the .byte operation and
   write out special characters outside the quoted string.
   Also, the assembler is broken; very long strings are truncated,
   so we must artificially break them up early.  */

void
output_ascii (file, p, n)
     FILE *file;
     const char *p;
     int n;
{
  char c;
  int i, count_string;
  const char *for_string = "\t.byte \"";
  const char *for_decimal = "\t.byte ";
  const char *to_close = NULL;

  count_string = 0;
  for (i = 0; i < n; i++)
    {
      c = *p++;
      if (c >= ' ' && c < 0177)
	{
	  if (for_string)
	    fputs (for_string, file);
	  putc (c, file);

	  /* Write two quotes to get one.  */
	  if (c == '"')
	    {
	      putc (c, file);
	      ++count_string;
	    }

	  for_string = NULL;
	  for_decimal = "\"\n\t.byte ";
	  to_close = "\"\n";
	  ++count_string;

	  if (count_string >= 512)
	    {
	      fputs (to_close, file);

	      for_string = "\t.byte \"";
	      for_decimal = "\t.byte ";
	      to_close = NULL;
	      count_string = 0;
	    }
	}
      else
	{
	  if (for_decimal)
	    fputs (for_decimal, file);
	  fprintf (file, "%d", c);

	  for_string = "\n\t.byte \"";
	  for_decimal = ", ";
	  to_close = "\n";
	  count_string = 0;
	}
    }

  /* Now close the string if we have written one.  Then end the line.  */
  if (to_close)
    fputs (to_close, file);
}

/* Generate a unique section name for FILENAME for a section type
   represented by SECTION_DESC.  Output goes into BUF.

   SECTION_DESC can be any string, as long as it is different for each
   possible section type.

   We name the section in the same manner as xlc.  The name begins with an
   underscore followed by the filename (after stripping any leading directory
   names) with the last period replaced by the string SECTION_DESC.  If
   FILENAME does not contain a period, SECTION_DESC is appended to the end of
   the name.  */

void
rs6000_gen_section_name (buf, filename, section_desc)
     char **buf;
     const char *filename;
     const char *section_desc;
{
  const char *q, *after_last_slash, *last_period = 0;
  char *p;
  int len;

  after_last_slash = filename;
  for (q = filename; *q; q++)
    {
      if (*q == '/')
	after_last_slash = q + 1;
      else if (*q == '.')
	last_period = q;
    }

  len = strlen (after_last_slash) + strlen (section_desc) + 2;
  *buf = (char *) permalloc (len);

  p = *buf;
  *p++ = '_';

  for (q = after_last_slash; *q; q++)
    {
      if (q == last_period)
        {
	  strcpy (p, section_desc);
	  p += strlen (section_desc);
        }

      else if (ISALNUM (*q))
        *p++ = *q;
    }

  if (last_period == 0)
    strcpy (p, section_desc);
  else
    *p = '\0';
}

/* Emit profile function.  */

void
output_profile_hook (labelno)
     int labelno ATTRIBUTE_UNUSED;
{
  if (DEFAULT_ABI == ABI_AIX)
    {
#ifdef NO_PROFILE_COUNTERS
      emit_library_call (init_one_libfunc (RS6000_MCOUNT), 0, VOIDmode, 0);
#else
      char buf[30];
      const char *label_name;
      rtx fun;

      ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
      STRIP_NAME_ENCODING (label_name, ggc_strdup (buf));
      fun = gen_rtx_SYMBOL_REF (Pmode, label_name);

      emit_library_call (init_one_libfunc (RS6000_MCOUNT), 0, VOIDmode, 1,
                         fun, Pmode);
#endif
    }
  else if (DEFAULT_ABI == ABI_DARWIN)
    {
      const char *mcount_name = RS6000_MCOUNT;
      int caller_addr_regno = LINK_REGISTER_REGNUM;

      /* Be conservative and always set this, at least for now.  */
      current_function_uses_pic_offset_table = 1;

#if TARGET_MACHO
      /* For PIC code, set up a stub and collect the caller's address
	 from r0, which is where the prologue puts it.  */
      if (flag_pic)
	{
	  mcount_name = machopic_stub_name (mcount_name);
	  if (current_function_uses_pic_offset_table)
	    caller_addr_regno = 0;
	}
#endif
      emit_library_call (gen_rtx_SYMBOL_REF (Pmode, mcount_name),
			 0, VOIDmode, 1,
			 gen_rtx_REG (Pmode, caller_addr_regno), Pmode);
    }
}

/* Write function profiler code.  */

void
output_function_profiler (file, labelno)
  FILE *file;
  int labelno;
{
  char buf[100];
  int save_lr = 8;

  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
  switch (DEFAULT_ABI)
    {
    default:
      abort ();

    case ABI_V4:
      save_lr = 4;
      /* Fall through.  */

    case ABI_AIX_NODESC:
      if (!TARGET_32BIT)
	{
	  warning ("no profiling of 64-bit code for this ABI");
	  return;
	}
      fprintf (file, "\tmflr %s\n", reg_names[0]);
      if (flag_pic == 1)
	{
	  fputs ("\tbl _GLOBAL_OFFSET_TABLE_@local-4\n", file);
	  asm_fprintf (file, "\t{st|stw} %s,%d(%s)\n",
		       reg_names[0], save_lr, reg_names[1]);
	  asm_fprintf (file, "\tmflr %s\n", reg_names[12]);
	  asm_fprintf (file, "\t{l|lwz} %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@got(%s)\n", reg_names[12]);
	}
      else if (flag_pic > 1)
	{
	  asm_fprintf (file, "\t{st|stw} %s,%d(%s)\n",
		       reg_names[0], save_lr, reg_names[1]);
	  /* Now, we need to get the address of the label.  */
	  fputs ("\tbl 1f\n\t.long ", file);
	  assemble_name (file, buf);
	  fputs ("-.\n1:", file);
	  asm_fprintf (file, "\tmflr %s\n", reg_names[11]);
	  asm_fprintf (file, "\t{l|lwz} %s,0(%s)\n", 
		       reg_names[0], reg_names[11]);
	  asm_fprintf (file, "\t{cax|add} %s,%s,%s\n",
		       reg_names[0], reg_names[0], reg_names[11]);
	}
      else
	{
	  asm_fprintf (file, "\t{liu|lis} %s,", reg_names[12]);
	  assemble_name (file, buf);
	  fputs ("@ha\n", file);
	  asm_fprintf (file, "\t{st|stw} %s,%d(%s)\n",
		       reg_names[0], save_lr, reg_names[1]);
	  asm_fprintf (file, "\t{cal|la} %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@l(%s)\n", reg_names[12]);
	}

      if (current_function_needs_context && DEFAULT_ABI == ABI_AIX_NODESC)
	{
	  asm_fprintf (file, "\t{st|stw} %s,%d(%s)\n",
		       reg_names[STATIC_CHAIN_REGNUM],
		       12, reg_names[1]);
	  fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
	  asm_fprintf (file, "\t{l|lwz} %s,%d(%s)\n",
		       reg_names[STATIC_CHAIN_REGNUM],
		       12, reg_names[1]);
	}
      else
	/* ABI_V4 saves the static chain reg with ASM_OUTPUT_REG_PUSH.  */
	fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
      break;

    case ABI_AIX:
    case ABI_DARWIN:
      /* Don't do anything, done in output_profile_hook ().  */
      break;
    }
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
rs6000_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn ATTRIBUTE_UNUSED;
     int cost;
{
  if (! recog_memoized (insn))
    return 0;

  if (REG_NOTE_KIND (link) != 0)
    return 0;

  if (REG_NOTE_KIND (link) == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads
	 some cycles later.  */
      switch (get_attr_type (insn))
	{
	case TYPE_JMPREG:
          /* Tell the first scheduling pass about the latency between
	     a mtctr and bctr (and mtlr and br/blr).  The first
	     scheduling pass will not know about this latency since
	     the mtctr instruction, which has the latency associated
	     to it, will be generated by reload.  */
          return TARGET_POWER ? 5 : 4;
	case TYPE_BRANCH:
	  /* Leave some extra cycles between a compare and its
	     dependent branch, to inhibit expensive mispredicts.  */
	  if ((rs6000_cpu_attr == CPU_PPC750
               || rs6000_cpu_attr == CPU_PPC7400
               || rs6000_cpu_attr == CPU_PPC7450)
	      && recog_memoized (dep_insn)
	      && (INSN_CODE (dep_insn) >= 0)
	      && (get_attr_type (dep_insn) == TYPE_COMPARE
		  || get_attr_type (dep_insn) == TYPE_DELAYED_COMPARE
		  || get_attr_type (dep_insn) == TYPE_FPCOMPARE
		  || get_attr_type (dep_insn) == TYPE_CR_LOGICAL))
	    return cost + 2;
	default:
	  break;
	}
      /* Fall out to return default cost.  */
    }

  return cost;
}

/* A C statement (sans semicolon) to update the integer scheduling
   priority INSN_PRIORITY (INSN).  Reduce the priority to execute the
   INSN earlier, increase the priority to execute INSN later.  Do not
   define this macro if you do not need to adjust the scheduling
   priorities of insns.  */

static int
rs6000_adjust_priority (insn, priority)
     rtx insn ATTRIBUTE_UNUSED;
     int priority;
{
  /* On machines (like the 750) which have asymmetric integer units,
     where one integer unit can do multiply and divides and the other
     can't, reduce the priority of multiply/divide so it is scheduled
     before other integer operations.  */

#if 0
  if (! INSN_P (insn))
    return priority;

  if (GET_CODE (PATTERN (insn)) == USE)
    return priority;

  switch (rs6000_cpu_attr) {
  case CPU_PPC750:
    switch (get_attr_type (insn))
      {
      default:
	break;

      case TYPE_IMUL:
      case TYPE_IDIV:
	fprintf (stderr, "priority was %#x (%d) before adjustment\n",
		 priority, priority);
	if (priority >= 0 && priority < 0x01000000)
	  priority >>= 3;
	break;
      }
  }
#endif

  return priority;
}

/* Return how many instructions the machine can issue per cycle.  */

static int
rs6000_issue_rate ()
{
  switch (rs6000_cpu_attr) {
  case CPU_RIOS1:  /* ? */
  case CPU_RS64A:
  case CPU_PPC601: /* ? */
  case CPU_PPC7450:
    return 3;
  case CPU_PPC603:
  case CPU_PPC750:
  case CPU_PPC7400:
    return 2; 
  case CPU_RIOS2:
  case CPU_PPC604:
  case CPU_PPC604E:
  case CPU_PPC620:
  case CPU_PPC630:
    return 4;
  default:
    return 1;
  }
}


/* Length in units of the trampoline for entering a nested function.  */

int
rs6000_trampoline_size ()
{
  int ret = 0;

  switch (DEFAULT_ABI)
    {
    default:
      abort ();

    case ABI_AIX:
      ret = (TARGET_32BIT) ? 12 : 24;
      break;

    case ABI_DARWIN:
    case ABI_V4:
    case ABI_AIX_NODESC:
      ret = (TARGET_32BIT) ? 40 : 48;
      break;
    }

  return ret;
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

void
rs6000_initialize_trampoline (addr, fnaddr, cxt)
     rtx addr;
     rtx fnaddr;
     rtx cxt;
{
  enum machine_mode pmode = Pmode;
  int regsize = (TARGET_32BIT) ? 4 : 8;
  rtx ctx_reg = force_reg (pmode, cxt);

  switch (DEFAULT_ABI)
    {
    default:
      abort ();

/* Macros to shorten the code expansions below.  */
#define MEM_DEREF(addr) gen_rtx_MEM (pmode, memory_address (pmode, addr))
#define MEM_PLUS(addr,offset) \
  gen_rtx_MEM (pmode, memory_address (pmode, plus_constant (addr, offset)))

    /* Under AIX, just build the 3 word function descriptor */
    case ABI_AIX:
      {
	rtx fn_reg = gen_reg_rtx (pmode);
	rtx toc_reg = gen_reg_rtx (pmode);
	emit_move_insn (fn_reg, MEM_DEREF (fnaddr));
	emit_move_insn (toc_reg, MEM_PLUS (fnaddr, regsize));
	emit_move_insn (MEM_DEREF (addr), fn_reg);
	emit_move_insn (MEM_PLUS (addr, regsize), toc_reg);
	emit_move_insn (MEM_PLUS (addr, 2*regsize), ctx_reg);
      }
      break;

    /* Under V.4/eabi/darwin, __trampoline_setup does the real work.  */
    case ABI_DARWIN:
    case ABI_V4:
    case ABI_AIX_NODESC:
      emit_library_call (gen_rtx_SYMBOL_REF (SImode, "__trampoline_setup"),
			 FALSE, VOIDmode, 4,
			 addr, pmode,
			 GEN_INT (rs6000_trampoline_size ()), SImode,
			 fnaddr, pmode,
			 ctx_reg, pmode);
      break;
    }

  return;
}


/* Table of valid machine attributes.  */

const struct attribute_spec rs6000_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "longcall", 0, 0, false, true,  true,  rs6000_handle_longcall_attribute },
  { NULL,       0, 0, false, false, false, NULL }
};

/* Handle a "longcall" attribute; arguments as in struct
   attribute_spec.handler.  */

static tree
rs6000_handle_longcall_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Return a reference suitable for calling a function with the
   longcall attribute.  */

struct rtx_def *
rs6000_longcall_ref (call_ref)
     rtx call_ref;
{
  const char *call_name;
  tree node;

  if (GET_CODE (call_ref) != SYMBOL_REF)
    return call_ref;

  /* System V adds '.' to the internal name, so skip them.  */
  call_name = XSTR (call_ref, 0);
  if (*call_name == '.')
    {
      while (*call_name == '.')
	call_name++;

      node = get_identifier (call_name);
      call_ref = gen_rtx_SYMBOL_REF (VOIDmode, IDENTIFIER_POINTER (node));
    }

  return force_reg (Pmode, call_ref);
}


/* A C statement or statements to switch to the appropriate section
   for output of RTX in mode MODE.  You can assume that RTX is some
   kind of constant in RTL.  The argument MODE is redundant except in
   the case of a `const_int' rtx.  Select the section by calling
   `text_section' or one of the alternatives for other sections.

   Do not define this macro if you put all constants in the read-only
   data section.  */

#ifdef USING_ELFOS_H

void
rs6000_select_rtx_section (mode, x)
     enum machine_mode mode;
     rtx x;
{
  if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (x, mode))
    toc_section ();
  else if (flag_pic
	   && (GET_CODE (x) == SYMBOL_REF
	       || GET_CODE (x) == LABEL_REF
	       || GET_CODE (x) == CONST))
    data_section ();
  else
    const_section ();
}

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

void
rs6000_select_section (decl, reloc)
     tree decl;
     int reloc;
{
  int size = int_size_in_bytes (TREE_TYPE (decl));
  int needs_sdata;
  int readonly;
  static void (* const sec_funcs[4]) PARAMS ((void)) = {
    &const_section,
    &sdata2_section,
    &data_section,
    &sdata_section
  };
  
  needs_sdata = (size > 0 
		 && size <= g_switch_value
		 && rs6000_sdata != SDATA_NONE
		 && (rs6000_sdata != SDATA_DATA || TREE_PUBLIC (decl)));

  if (TREE_CODE (decl) == STRING_CST)
    readonly = ! flag_writable_strings;
  else if (TREE_CODE (decl) == VAR_DECL)
    readonly = (! ((flag_pic || DEFAULT_ABI == ABI_AIX) && reloc)
		&& TREE_READONLY (decl)
		&& ! TREE_SIDE_EFFECTS (decl)
		&& DECL_INITIAL (decl)
		&& DECL_INITIAL (decl) != error_mark_node
		&& TREE_CONSTANT (DECL_INITIAL (decl)));
  else if (TREE_CODE (decl) == CONSTRUCTOR)
    readonly = (! ((flag_pic || DEFAULT_ABI == ABI_AIX) && reloc)
		&& ! TREE_SIDE_EFFECTS (decl)
		&& TREE_CONSTANT (decl));
  else
    readonly = ! ((flag_pic || DEFAULT_ABI == ABI_AIX) && reloc);

  if (needs_sdata && rs6000_sdata != SDATA_EABI)
    readonly = 0;
  
  (*sec_funcs[(readonly ? 0 : 2) + (needs_sdata ? 1 : 0)])();
}

/* A C statement to build up a unique section name, expressed as a
   STRING_CST node, and assign it to DECL_SECTION_NAME (decl).
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  If you do not define this macro, GCC will use
   the symbol name prefixed by `.' as the section name.  Note - this
   macro can now be called for uninitialized data items as well as
   initialised data and functions.  */

void
rs6000_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  int len;
  int sec;
  const char *name;
  char *string;
  const char *prefix;

  static const char *const prefixes[7][2] =
  {
    { ".rodata.", ".gnu.linkonce.r." },
    { ".sdata2.", ".gnu.linkonce.s2." },
    { ".data.",   ".gnu.linkonce.d." },
    { ".sdata.",  ".gnu.linkonce.s." },
    { ".bss.",    ".gnu.linkonce.b." },
    { ".sbss.",   ".gnu.linkonce.sb." },
    { ".text.",   ".gnu.linkonce.t." }
  };

  if (TREE_CODE (decl) == FUNCTION_DECL)
    sec = 6;
  else
    {
      int readonly;
      int needs_sdata;
      int size;

      if (TREE_CODE (decl) == STRING_CST)
	readonly = ! flag_writable_strings;
      else if (TREE_CODE (decl) == VAR_DECL)
	readonly = (! ((flag_pic || DEFAULT_ABI == ABI_AIX) && reloc)
		    && TREE_READONLY (decl)
		    && ! TREE_SIDE_EFFECTS (decl)
		    && TREE_CONSTANT (DECL_INITIAL (decl)));
      else
	readonly = ! ((flag_pic || DEFAULT_ABI == ABI_AIX) && reloc);

      size = int_size_in_bytes (TREE_TYPE (decl));
      needs_sdata = (size > 0 
		     && size <= g_switch_value
		     && rs6000_sdata != SDATA_NONE
		     && (rs6000_sdata != SDATA_DATA || TREE_PUBLIC (decl)));

      if (DECL_INITIAL (decl) == 0
	  || DECL_INITIAL (decl) == error_mark_node)
	sec = 4;
      else if (! readonly)
	sec = 2;
      else
	sec = 0;

      if (needs_sdata)
	{
	  /* .sdata2 is only for EABI.  */
	  if (sec == 0 && rs6000_sdata != SDATA_EABI)
	    sec = 2;
	  sec += 1;
	}
    }

  STRIP_NAME_ENCODING (name, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
  prefix = prefixes[sec][DECL_ONE_ONLY (decl)];
  len    = strlen (name) + strlen (prefix);
  string = alloca (len + 1);
  
  sprintf (string, "%s%s", prefix, name);
  
  DECL_SECTION_NAME (decl) = build_string (len, string);
}


static bool
rs6000_binds_local_p (exp)
     tree exp;
{
  bool local_p;
  tree attr;

  /* A non-decl is an entry in the constant pool.  */
  if (!DECL_P (exp))
    local_p = true;
  /* Static variables are always local.  */
  else if (! TREE_PUBLIC (exp))
    local_p = true;
  /* Otherwise, variables defined outside this object may not be local.  */
  else if (DECL_EXTERNAL (exp))
    local_p = false;
  /* Linkonce and weak data are never local.  */
  else if (DECL_ONE_ONLY (exp) || DECL_WEAK (exp))
    local_p = false;
  /* If PIC, then assume that any global name can be overridden by
   *      symbols resolved from other modules.  */
  else if (flag_pic || rs6000_flag_pic)
    local_p = false;
  /* Uninitialized COMMON variable may be unified with symbols
   *      resolved from other modules.  */
  else if (DECL_COMMON (exp)
	   && (DECL_INITIAL (exp) == NULL
	       || DECL_INITIAL (exp) == error_mark_node))
    local_p = false;
  /* Otherwise we're left with initialized (or non-common) global data
   *      which is of necessity defined locally.  */
  else
    local_p = true;

  return local_p;
}


/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  For real AIX calling sequences, we also replace the
   function name with the real name (1 or 2 leading .'s), rather than
   the function descriptor name.  This saves a lot of overriding code
   to read the prefixes.  */

void
rs6000_encode_section_info (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      rtx sym_ref = XEXP (DECL_RTL (decl), 0);
      if (rs6000_binds_local_p (decl))
	SYMBOL_REF_FLAG (sym_ref) = 1;

      if (DEFAULT_ABI == ABI_AIX)
	{
	  size_t len1 = (DEFAULT_ABI == ABI_AIX) ? 1 : 2;
	  size_t len2 = strlen (XSTR (sym_ref, 0));
	  char *str = alloca (len1 + len2 + 1);
	  str[0] = '.';
	  str[1] = '.';
	  memcpy (str + len1, XSTR (sym_ref, 0), len2 + 1);

	  XSTR (sym_ref, 0) = ggc_alloc_string (str, len1 + len2);
	}
    }
  else if (rs6000_sdata != SDATA_NONE
	   && DEFAULT_ABI == ABI_V4
	   && TREE_CODE (decl) == VAR_DECL)
    {
      rtx sym_ref = XEXP (DECL_RTL (decl), 0);
      int size = int_size_in_bytes (TREE_TYPE (decl));
      tree section_name = DECL_SECTION_NAME (decl);
      const char *name = (char *)0;
      int len = 0;

      if (rs6000_binds_local_p (decl))
	SYMBOL_REF_FLAG (sym_ref) = 1;

      if (section_name)
	{
	  if (TREE_CODE (section_name) == STRING_CST)
	    {
	      name = TREE_STRING_POINTER (section_name);
	      len = TREE_STRING_LENGTH (section_name);
	    }
	  else
	    abort ();
	}

      if ((size > 0 && size <= g_switch_value)
	  || (name
	      && ((len == sizeof (".sdata") - 1
		   && strcmp (name, ".sdata") == 0)
		  || (len == sizeof (".sdata2") - 1
		      && strcmp (name, ".sdata2") == 0)
		  || (len == sizeof (".sbss") - 1
		      && strcmp (name, ".sbss") == 0)
		  || (len == sizeof (".sbss2") - 1
		      && strcmp (name, ".sbss2") == 0)
		  || (len == sizeof (".PPC.EMB.sdata0") - 1
		      && strcmp (name, ".PPC.EMB.sdata0") == 0)
		  || (len == sizeof (".PPC.EMB.sbss0") - 1
		      && strcmp (name, ".PPC.EMB.sbss0") == 0))))
	{
	  rtx sym_ref = XEXP (DECL_RTL (decl), 0);
	  size_t len = strlen (XSTR (sym_ref, 0));
	  char *str = alloca (len + 2);

	  str[0] = '@';
	  memcpy (str + 1, XSTR (sym_ref, 0), len + 1);
	  XSTR (sym_ref, 0) = ggc_alloc_string (str, len + 1);
	}
    }
}

#endif /* USING_ELFOS_H */


/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.

   r0 is special and we must not select it as an address
   register by this routine since our caller will try to
   increment the returned register via an "la" instruction.  */

struct rtx_def *
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && REGNO (XEXP (addr, 0)) != 0)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG
	       && REGNO (XEXP (addr, 1)) != 0)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG && REGNO (addr) != 0)
    return addr;
  abort ();
}

void
rs6000_fatal_bad_address (op)
  rtx op;
{
  fatal_insn ("bad address", op);
}

/* Called to register all of our global variables with the garbage
   collector.  */

static void
rs6000_add_gc_roots ()
{
  ggc_add_rtx_root (&rs6000_compare_op0, 1);
  ggc_add_rtx_root (&rs6000_compare_op1, 1);

  toc_hash_table = htab_create (1021, toc_hash_function, toc_hash_eq, NULL);
  ggc_add_root (&toc_hash_table, 1, sizeof (toc_hash_table), 
		toc_hash_mark_table);

#if TARGET_MACHO
  machopic_add_gc_roots ();
#endif
}

#if TARGET_MACHO

#if 0
/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
symbolic_operand (op)
     rtx op;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (op) == SYMBOL_REF ||
	      (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}
#endif

#ifdef RS6000_LONG_BRANCH

static tree stub_list = 0;

/* ADD_COMPILER_STUB adds the compiler generated stub for handling 
   procedure calls to the linked list.  */

void 
add_compiler_stub (label_name, function_name, line_number)
     tree label_name;
     tree function_name;
     int line_number;
{
  tree stub = build_tree_list (function_name, label_name);
  TREE_TYPE (stub) = build_int_2 (line_number, 0);
  TREE_CHAIN (stub) = stub_list;
  stub_list = stub;
}

#define STUB_LABEL_NAME(STUB)     TREE_VALUE (STUB)
#define STUB_FUNCTION_NAME(STUB)  TREE_PURPOSE (STUB)
#define STUB_LINE_NUMBER(STUB)    TREE_INT_CST_LOW (TREE_TYPE (STUB))

/* OUTPUT_COMPILER_STUB outputs the compiler generated stub for
   handling procedure calls from the linked list and initializes the
   linked list.  */

void
output_compiler_stub ()
{
  char tmp_buf[256];
  char label_buf[256];
  char *label;
  tree tmp_stub, stub;

  if (!flag_pic)
    for (stub = stub_list; stub; stub = TREE_CHAIN (stub))
      {
	fprintf (asm_out_file,
		 "%s:\n", IDENTIFIER_POINTER(STUB_LABEL_NAME(stub)));

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
	if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	  fprintf (asm_out_file, "\t.stabd 68,0,%d\n", STUB_LINE_NUMBER(stub));
#endif /* DBX_DEBUGGING_INFO || XCOFF_DEBUGGING_INFO */

	if (IDENTIFIER_POINTER (STUB_FUNCTION_NAME (stub))[0] == '*')
	  strcpy (label_buf,
		  IDENTIFIER_POINTER (STUB_FUNCTION_NAME (stub))+1);
	else
	  {
	    label_buf[0] = '_';
	    strcpy (label_buf+1,
		    IDENTIFIER_POINTER (STUB_FUNCTION_NAME (stub)));
	  }

	strcpy (tmp_buf, "lis r12,hi16(");
	strcat (tmp_buf, label_buf);
	strcat (tmp_buf, ")\n\tori r12,r12,lo16(");
	strcat (tmp_buf, label_buf);
	strcat (tmp_buf, ")\n\tmtctr r12\n\tbctr");
	output_asm_insn (tmp_buf, 0);

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
	if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	  fprintf(asm_out_file, "\t.stabd 68,0,%d\n", STUB_LINE_NUMBER (stub));
#endif /* DBX_DEBUGGING_INFO || XCOFF_DEBUGGING_INFO */
      }

  stub_list = 0;
}

/* NO_PREVIOUS_DEF checks in the link list whether the function name is
   already there or not.  */

int
no_previous_def (function_name)
     tree function_name;
{
  tree stub;
  for (stub = stub_list; stub; stub = TREE_CHAIN (stub))
    if (function_name == STUB_FUNCTION_NAME (stub))
      return 0;
  return 1;
}

/* GET_PREV_LABEL gets the label name from the previous definition of
   the function.  */

tree
get_prev_label (function_name)
     tree function_name;
{
  tree stub;
  for (stub = stub_list; stub; stub = TREE_CHAIN (stub))
    if (function_name == STUB_FUNCTION_NAME (stub))
      return STUB_LABEL_NAME (stub);
  return 0;
}

/* INSN is either a function call or a millicode call.  It may have an
   unconditional jump in its delay slot.  

   CALL_DEST is the routine we are calling.  */

char *
output_call (insn, call_dest, operand_number)
     rtx insn;
     rtx call_dest;
     int operand_number;
{
  static char buf[256];
  if (GET_CODE (call_dest) == SYMBOL_REF && TARGET_LONG_BRANCH && !flag_pic)
    {
      tree labelname;
      tree funname = get_identifier (XSTR (call_dest, 0));
      
      if (no_previous_def (funname))
	{
	  int line_number;
	  rtx label_rtx = gen_label_rtx ();
	  char *label_buf, temp_buf[256];
	  ASM_GENERATE_INTERNAL_LABEL (temp_buf, "L",
				       CODE_LABEL_NUMBER (label_rtx));
	  label_buf = temp_buf[0] == '*' ? temp_buf + 1 : temp_buf;
	  labelname = get_identifier (label_buf);
	  for (; insn && GET_CODE (insn) != NOTE; insn = PREV_INSN (insn));
	  if (insn)
	    line_number = NOTE_LINE_NUMBER (insn);
	  add_compiler_stub (labelname, funname, line_number);
	}
      else
	labelname = get_prev_label (funname);

      sprintf (buf, "jbsr %%z%d,%.246s",
	       operand_number, IDENTIFIER_POINTER (labelname));
      return buf;
    }
  else
    {
      sprintf (buf, "bl %%z%d", operand_number);
      return buf;
    }
}

#endif /* RS6000_LONG_BRANCH */

#define GEN_LOCAL_LABEL_FOR_SYMBOL(BUF,SYMBOL,LENGTH,N)		\
  do {								\
    const char *const symbol_ = (SYMBOL);			\
    char *buffer_ = (BUF);					\
    if (symbol_[0] == '"')					\
      {								\
        sprintf(buffer_, "\"L%d$%s", (N), symbol_+1);		\
      }								\
    else if (name_needs_quotes(symbol_))			\
      {								\
        sprintf(buffer_, "\"L%d$%s\"", (N), symbol_);		\
      }								\
    else							\
      {								\
        sprintf(buffer_, "L%d$%s", (N), symbol_);		\
      }								\
  } while (0)


/* Generate PIC and indirect symbol stubs.  */

void
machopic_output_stub (file, symb, stub)
     FILE *file;
     const char *symb, *stub;
{
  unsigned int length;
  char *symbol_name, *lazy_ptr_name;
  char *local_label_0;
  static int label = 0;

  /* Lose our funky encoding stuff so it doesn't contaminate the stub.  */
  STRIP_NAME_ENCODING (symb, symb);

  label += 1;

  length = strlen (symb);
  symbol_name = alloca (length + 32);
  GEN_SYMBOL_NAME_FOR_SYMBOL (symbol_name, symb, length);

  lazy_ptr_name = alloca (length + 32);
  GEN_LAZY_PTR_NAME_FOR_SYMBOL (lazy_ptr_name, symb, length);

  local_label_0 = alloca (length + 32);
  GEN_LOCAL_LABEL_FOR_SYMBOL (local_label_0, symb, length, 0);

  if (flag_pic == 2)
    machopic_picsymbol_stub_section ();
  else
    machopic_symbol_stub_section ();

  fprintf (file, "%s:\n", stub);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

  if (flag_pic == 2)
    {
      fprintf (file, "\tmflr r0\n");
      fprintf (file, "\tbcl 20,31,%s\n", local_label_0);
      fprintf (file, "%s:\n\tmflr r11\n", local_label_0);
      fprintf (file, "\taddis r11,r11,ha16(%s-%s)\n",
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tmtlr r0\n");
      fprintf (file, "\tlwz r12,lo16(%s-%s)(r11)\n",
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tmtctr r12\n");
      fprintf (file, "\taddi r11,r11,lo16(%s-%s)\n",
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tbctr\n");
    }
  else
    fprintf (file, "non-pure not supported\n");
  
  machopic_lazy_symbol_ptr_section ();
  fprintf (file, "%s:\n", lazy_ptr_name);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);
  fprintf (file, "\t.long dyld_stub_binding_helper\n");
}

/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go into a reg.  This is REG if non
   zero, otherwise we allocate register(s) as necessary.  */

#define SMALL_INT(X) ((unsigned) (INTVAL(X) + 0x8000) < 0x10000)

rtx
rs6000_machopic_legitimize_pic_address (orig, mode, reg)
     rtx orig;
     enum machine_mode mode;
     rtx reg;
{
  rtx base, offset;

  if (reg == NULL && ! reload_in_progress && ! reload_completed)
    reg = gen_reg_rtx (Pmode);

  if (GET_CODE (orig) == CONST)
    {
      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base =
	    rs6000_machopic_legitimize_pic_address (XEXP (XEXP (orig, 0), 0),
						    Pmode, reg);
	  offset =
	    rs6000_machopic_legitimize_pic_address (XEXP (XEXP (orig, 0), 1),
						    Pmode, reg);
	}
      else
	abort ();

      if (GET_CODE (offset) == CONST_INT)
	{
	  if (SMALL_INT (offset))
	    return plus_constant (base, INTVAL (offset));
	  else if (! reload_in_progress && ! reload_completed)
	    offset = force_reg (Pmode, offset);
	  else
	    {
 	      rtx mem = force_const_mem (Pmode, orig);
	      return machopic_legitimize_pic_address (mem, Pmode, reg);
	    }
	}
      return gen_rtx (PLUS, Pmode, base, offset);
    }

  /* Fall back on generic machopic code.  */
  return machopic_legitimize_pic_address (orig, mode, reg);
}

/* This is just a placeholder to make linking work without having to
   add this to the generic Darwin EXTRA_SECTIONS.  If -mcall-aix is
   ever needed for Darwin (not too likely!) this would have to get a
   real definition.  */

void
toc_section ()
{
}

#endif /* TARGET_MACHO */

#if TARGET_ELF
static unsigned int
rs6000_elf_section_type_flags (decl, name, reloc)
     tree decl;
     const char *name;
     int reloc;
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  if (TARGET_RELOCATABLE)
    flags |= SECTION_WRITE;

  return flags;
}

/* Record an element in the table of global constructors.  SYMBOL is
   a SYMBOL_REF of the function to be called; PRIORITY is a number
   between 0 and MAX_INIT_PRIORITY.

   This differs from default_named_section_asm_out_constructor in
   that we have special handling for -mrelocatable.  */

static void
rs6000_elf_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority;
{
  const char *section = ".ctors";
  char buf[16];

  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".ctors.%.5u",
               /* Invert the numbering so the linker puts us in the proper
                  order; constructors are run from right to left, and the
                  linker sorts in increasing order.  */
               MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  named_section_flags (section, SECTION_WRITE);
  assemble_align (POINTER_SIZE);

  if (TARGET_RELOCATABLE)
    {
      fputs ("\t.long (", asm_out_file);
      output_addr_const (asm_out_file, symbol);
      fputs (")@fixup\n", asm_out_file);
    }
  else
    assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

static void
rs6000_elf_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority;
{
  const char *section = ".dtors";
  char buf[16];

  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".dtors.%.5u",
               /* Invert the numbering so the linker puts us in the proper
                  order; constructors are run from right to left, and the
                  linker sorts in increasing order.  */
               MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  named_section_flags (section, SECTION_WRITE);
  assemble_align (POINTER_SIZE);

  if (TARGET_RELOCATABLE)
    {
      fputs ("\t.long (", asm_out_file);
      output_addr_const (asm_out_file, symbol);
      fputs (")@fixup\n", asm_out_file);
    }
  else
    assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}
#endif

#ifdef OBJECT_FORMAT_COFF
static void
xcoff_asm_named_section (name, flags)
     const char *name;
     unsigned int flags ATTRIBUTE_UNUSED;
{
  fprintf (asm_out_file, "\t.csect %s\n", name);
}
#endif
