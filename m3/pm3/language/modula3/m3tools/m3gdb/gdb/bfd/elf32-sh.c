/* Hitachi SH specific support for 32-bit ELF
   Copyright 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Ian Lance Taylor, Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

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

#include "bfd.h"
#include "sysdep.h"
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"
#include "elf/sh.h"

static bfd_reloc_status_type sh_elf_reloc
  PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));
static bfd_reloc_status_type sh_elf_ignore_reloc
  PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));
static reloc_howto_type *sh_elf_reloc_type_lookup
  PARAMS ((bfd *, bfd_reloc_code_real_type));
static void sh_elf_info_to_howto
  PARAMS ((bfd *, arelent *, Elf_Internal_Rela *));
static boolean sh_elf_set_private_flags
  PARAMS ((bfd *, flagword));
static boolean sh_elf_copy_private_data
  PARAMS ((bfd *, bfd *));
static boolean sh_elf_merge_private_data
  PARAMS ((bfd *, bfd *));
static boolean sh_elf_set_mach_from_flags
  PARAMS ((bfd *));
static boolean sh_elf_relax_section
  PARAMS ((bfd *, asection *, struct bfd_link_info *, boolean *));
static boolean sh_elf_relax_delete_bytes
  PARAMS ((bfd *, asection *, bfd_vma, int));
static boolean sh_elf_align_loads
  PARAMS ((bfd *, asection *, Elf_Internal_Rela *, bfd_byte *, boolean *));
static boolean sh_elf_swap_insns
  PARAMS ((bfd *, asection *, PTR, bfd_byte *, bfd_vma));
static boolean sh_elf_relocate_section
  PARAMS ((bfd *, struct bfd_link_info *, bfd *, asection *, bfd_byte *,
	   Elf_Internal_Rela *, Elf_Internal_Sym *, asection **));
static bfd_byte *sh_elf_get_relocated_section_contents
  PARAMS ((bfd *, struct bfd_link_info *, struct bfd_link_order *,
	   bfd_byte *, boolean, asymbol **));
static boolean sh_elf_check_relocs
  PARAMS ((bfd *, struct bfd_link_info *, asection *,
	   const Elf_Internal_Rela *));
static struct bfd_hash_entry *sh_elf_link_hash_newfunc
  PARAMS ((struct bfd_hash_entry *, struct bfd_hash_table *, const char *));
static struct bfd_link_hash_table *sh_elf_link_hash_table_create
  PARAMS ((bfd *));
static boolean sh_elf_adjust_dynamic_symbol
  PARAMS ((struct bfd_link_info *, struct elf_link_hash_entry *));
static boolean sh_elf_size_dynamic_sections
  PARAMS ((bfd *, struct bfd_link_info *));
static boolean sh_elf_finish_dynamic_symbol
  PARAMS ((bfd *, struct bfd_link_info *, struct elf_link_hash_entry *,
	   Elf_Internal_Sym *));
static boolean sh_elf_finish_dynamic_sections
  PARAMS ((bfd *, struct bfd_link_info *));

/* The name of the dynamic interpreter.  This is put in the .interp
   section.  */

#define ELF_DYNAMIC_INTERPRETER "/usr/lib/libc.so.1"

static reloc_howto_type sh_elf_howto_table[] =
{
  /* No relocation.  */
  HOWTO (R_SH_NONE,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_NONE",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* 32 bit absolute relocation.  Setting partial_inplace to true and
     src_mask to a non-zero value is similar to the COFF toolchain.  */
  HOWTO (R_SH_DIR32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 sh_elf_reloc,		/* special_function */
	 "R_SH_DIR32",		/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* 32 bit PC relative relocation.  */
  HOWTO (R_SH_REL32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_REL32",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffffffff,		/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit PC relative branch divided by 2.  */
  HOWTO (R_SH_DIR8WPN,		/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DIR8WPN",	/* name */
	 true,			/* partial_inplace */
	 0xff,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 12 bit PC relative branch divided by 2.  */
  HOWTO (R_SH_IND12W,		/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 12,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed, /* complain_on_overflow */
	 sh_elf_reloc,		/* special_function */
	 "R_SH_IND12W",		/* name */
	 true,			/* partial_inplace */
	 0xfff,			/* src_mask */
	 0xfff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit unsigned PC relative divided by 4.  */
  HOWTO (R_SH_DIR8WPL,		/* type */
	 2,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DIR8WPL",	/* name */
	 true,			/* partial_inplace */
	 0xff,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit unsigned PC relative divided by 2.  */
  HOWTO (R_SH_DIR8WPZ,		/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DIR8WPZ",	/* name */
	 true,			/* partial_inplace */
	 0xff,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit GBR relative.  FIXME: This only makes sense if we have some
     special symbol for the GBR relative area, and that is not
     implemented.  */
  HOWTO (R_SH_DIR8BP,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DIR8BP",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit GBR relative divided by 2.  FIXME: This only makes sense if
     we have some special symbol for the GBR relative area, and that
     is not implemented.  */
  HOWTO (R_SH_DIR8W,		/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DIR8W",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit GBR relative divided by 4.  FIXME: This only makes sense if
     we have some special symbol for the GBR relative area, and that
     is not implemented.  */
  HOWTO (R_SH_DIR8L,		/* type */
	 2,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DIR8L",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  EMPTY_HOWTO (10),
  EMPTY_HOWTO (11),
  EMPTY_HOWTO (12),
  EMPTY_HOWTO (13),
  EMPTY_HOWTO (14),
  EMPTY_HOWTO (15),
  EMPTY_HOWTO (16),
  EMPTY_HOWTO (17),
  EMPTY_HOWTO (18),
  EMPTY_HOWTO (19),
  EMPTY_HOWTO (20),
  EMPTY_HOWTO (21),
  EMPTY_HOWTO (22),
  EMPTY_HOWTO (23),
  EMPTY_HOWTO (24),

  /* The remaining relocs are a GNU extension used for relaxing.  The
     final pass of the linker never needs to do anything with any of
     these relocs.  Any required operations are handled by the
     relaxation code.  */

  /* A 16 bit switch table entry.  This is generated for an expression
     such as ``.word L1 - L2''.  The offset holds the difference
     between the reloc address and L2.  */
  HOWTO (R_SH_SWITCH16,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_SWITCH16",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* A 32 bit switch table entry.  This is generated for an expression
     such as ``.long L1 - L2''.  The offset holds the difference
     between the reloc address and L2.  */
  HOWTO (R_SH_SWITCH32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_SWITCH32",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* Indicates a .uses pseudo-op.  The compiler will generate .uses
     pseudo-ops when it finds a function call which can be relaxed.
     The offset field holds the PC relative offset to the instruction
     which loads the register used in the function call.  */
  HOWTO (R_SH_USES,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_USES",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* The assembler will generate this reloc for addresses referred to
     by the register loads associated with USES relocs.  The offset
     field holds the number of times the address is referenced in the
     object file.  */
  HOWTO (R_SH_COUNT,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_COUNT",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* Indicates an alignment statement.  The offset field is the power
     of 2 to which subsequent portions of the object file must be
     aligned.  */
  HOWTO (R_SH_ALIGN,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_ALIGN",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* The assembler will generate this reloc before a block of
     instructions.  A section should be processed as assumining it
     contains data, unless this reloc is seen.  */
  HOWTO (R_SH_CODE,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_CODE",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* The assembler will generate this reloc after a block of
     instructions when it sees data that is not instructions.  */
  HOWTO (R_SH_DATA,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_DATA",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* The assembler generates this reloc for each label within a block
     of instructions.  This permits the linker to avoid swapping
     instructions which are the targets of branches.  */
  HOWTO (R_SH_LABEL,		/* type */
	 0,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_LABEL",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* An 8 bit switch table entry.  This is generated for an expression
     such as ``.word L1 - L2''.  The offset holds the difference
     between the reloc address and L2.  */
  HOWTO (R_SH_SWITCH8,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_unsigned, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_SWITCH8",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* GNU extension to record C++ vtable hierarchy */
  HOWTO (R_SH_GNU_VTINHERIT, /* type */
         0,                     /* rightshift */
         2,                     /* size (0 = byte, 1 = short, 2 = long) */
         0,                     /* bitsize */
         false,                 /* pc_relative */
         0,                     /* bitpos */
         complain_overflow_dont, /* complain_on_overflow */
         NULL,                  /* special_function */
         "R_SH_GNU_VTINHERIT", /* name */
         false,                 /* partial_inplace */
         0,                     /* src_mask */
         0,                     /* dst_mask */
         false),                /* pcrel_offset */

  /* GNU extension to record C++ vtable member usage */
  HOWTO (R_SH_GNU_VTENTRY,     /* type */
         0,                     /* rightshift */
         2,                     /* size (0 = byte, 1 = short, 2 = long) */
         0,                     /* bitsize */
         false,                 /* pc_relative */
         0,                     /* bitpos */
         complain_overflow_dont, /* complain_on_overflow */
         _bfd_elf_rel_vtable_reloc_fn,  /* special_function */
         "R_SH_GNU_VTENTRY",   /* name */
         false,                 /* partial_inplace */
         0,                     /* src_mask */
         0,                     /* dst_mask */
         false),                /* pcrel_offset */

  /* 8 bit PC relative divided by 2 - but specified in a very odd way.  */
  HOWTO (R_SH_LOOP_START,	/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_LOOP_START",	/* name */
	 true,			/* partial_inplace */
	 0xff,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* 8 bit PC relative divided by 2 - but specified in a very odd way.  */
  HOWTO (R_SH_LOOP_END,		/* type */
	 1,			/* rightshift */
	 1,			/* size (0 = byte, 1 = short, 2 = long) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_signed, /* complain_on_overflow */
	 sh_elf_ignore_reloc,	/* special_function */
	 "R_SH_LOOP_END",	/* name */
	 true,			/* partial_inplace */
	 0xff,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  EMPTY_HOWTO (38),
  EMPTY_HOWTO (39),
  EMPTY_HOWTO (40),
  EMPTY_HOWTO (41),
  EMPTY_HOWTO (42),
  EMPTY_HOWTO (43),
  EMPTY_HOWTO (44),
  EMPTY_HOWTO (45),
  EMPTY_HOWTO (46),
  EMPTY_HOWTO (47),
  EMPTY_HOWTO (48),
  EMPTY_HOWTO (49),
  EMPTY_HOWTO (50),
  EMPTY_HOWTO (51),
  EMPTY_HOWTO (52),
  EMPTY_HOWTO (53),
  EMPTY_HOWTO (54),
  EMPTY_HOWTO (55),
  EMPTY_HOWTO (56),
  EMPTY_HOWTO (57),
  EMPTY_HOWTO (58),
  EMPTY_HOWTO (59),
  EMPTY_HOWTO (60),
  EMPTY_HOWTO (61),
  EMPTY_HOWTO (62),
  EMPTY_HOWTO (63),
  EMPTY_HOWTO (64),
  EMPTY_HOWTO (65),
  EMPTY_HOWTO (66),
  EMPTY_HOWTO (67),
  EMPTY_HOWTO (68),
  EMPTY_HOWTO (69),
  EMPTY_HOWTO (70),
  EMPTY_HOWTO (71),
  EMPTY_HOWTO (72),
  EMPTY_HOWTO (73),
  EMPTY_HOWTO (74),
  EMPTY_HOWTO (75),
  EMPTY_HOWTO (76),
  EMPTY_HOWTO (77),
  EMPTY_HOWTO (78),
  EMPTY_HOWTO (79),
  EMPTY_HOWTO (80),
  EMPTY_HOWTO (81),
  EMPTY_HOWTO (82),
  EMPTY_HOWTO (83),
  EMPTY_HOWTO (84),
  EMPTY_HOWTO (85),
  EMPTY_HOWTO (86),
  EMPTY_HOWTO (87),
  EMPTY_HOWTO (88),
  EMPTY_HOWTO (89),
  EMPTY_HOWTO (90),
  EMPTY_HOWTO (91),
  EMPTY_HOWTO (92),
  EMPTY_HOWTO (93),
  EMPTY_HOWTO (94),
  EMPTY_HOWTO (95),
  EMPTY_HOWTO (96),
  EMPTY_HOWTO (97),
  EMPTY_HOWTO (98),
  EMPTY_HOWTO (99),
  EMPTY_HOWTO (100),
  EMPTY_HOWTO (101),
  EMPTY_HOWTO (102),
  EMPTY_HOWTO (103),
  EMPTY_HOWTO (104),
  EMPTY_HOWTO (105),
  EMPTY_HOWTO (106),
  EMPTY_HOWTO (107),
  EMPTY_HOWTO (108),
  EMPTY_HOWTO (109),
  EMPTY_HOWTO (110),
  EMPTY_HOWTO (111),
  EMPTY_HOWTO (112),
  EMPTY_HOWTO (113),
  EMPTY_HOWTO (114),
  EMPTY_HOWTO (115),
  EMPTY_HOWTO (116),
  EMPTY_HOWTO (117),
  EMPTY_HOWTO (118),
  EMPTY_HOWTO (119),
  EMPTY_HOWTO (120),
  EMPTY_HOWTO (121),
  EMPTY_HOWTO (122),
  EMPTY_HOWTO (123),
  EMPTY_HOWTO (124),
  EMPTY_HOWTO (125),
  EMPTY_HOWTO (126),
  EMPTY_HOWTO (127),
  EMPTY_HOWTO (128),
  EMPTY_HOWTO (129),
  EMPTY_HOWTO (130),
  EMPTY_HOWTO (131),
  EMPTY_HOWTO (132),
  EMPTY_HOWTO (133),
  EMPTY_HOWTO (134),
  EMPTY_HOWTO (135),
  EMPTY_HOWTO (136),
  EMPTY_HOWTO (137),
  EMPTY_HOWTO (138),
  EMPTY_HOWTO (139),
  EMPTY_HOWTO (140),
  EMPTY_HOWTO (141),
  EMPTY_HOWTO (142),
  EMPTY_HOWTO (143),
  EMPTY_HOWTO (144),
  EMPTY_HOWTO (145),
  EMPTY_HOWTO (146),
  EMPTY_HOWTO (147),
  EMPTY_HOWTO (148),
  EMPTY_HOWTO (149),
  EMPTY_HOWTO (150),
  EMPTY_HOWTO (151),
  EMPTY_HOWTO (152),
  EMPTY_HOWTO (153),
  EMPTY_HOWTO (154),
  EMPTY_HOWTO (155),
  EMPTY_HOWTO (156),
  EMPTY_HOWTO (157),
  EMPTY_HOWTO (158),
  EMPTY_HOWTO (159),

  HOWTO (R_SH_GOT32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_GOT32",		/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO (R_SH_PLT32,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_PLT32",		/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 true),			/* pcrel_offset */

  HOWTO (R_SH_COPY,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_COPY",		/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO (R_SH_GLOB_DAT,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_GLOB_DAT",	/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO (R_SH_JMP_SLOT,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_JMP_SLOT",	/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO (R_SH_RELATIVE,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_RELATIVE",	/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO (R_SH_GOTOFF,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_GOTOFF",		/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO (R_SH_GOTPC,		/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 32,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* */
	 "R_SH_GOTPC",		/* name */
	 true,			/* partial_inplace */
	 0xffffffff,		/* src_mask */
	 0xffffffff,		/* dst_mask */
	 true),			/* pcrel_offset */

};

static bfd_reloc_status_type
sh_elf_reloc_loop (r_type, input_bfd, input_section, contents, addr,
		   symbol_section, start, end)
     int r_type ATTRIBUTE_UNUSED;
     bfd *input_bfd;
     asection *input_section;
     bfd_byte *contents;
     bfd_vma addr;
     asection *symbol_section;
     bfd_vma start, end;
{
  static bfd_vma last_addr;
  static asection *last_symbol_section;
  bfd_byte *free_contents = NULL;
  bfd_byte *start_ptr, *ptr, *last_ptr;
  int diff, cum_diff;
  bfd_signed_vma x;
  int insn;

  /* Sanity check the address.  */
  if (addr > input_section->_raw_size)
    return bfd_reloc_outofrange;

  /* We require the start and end relocations to be processed consecutively -
     although we allow then to be processed forwards or backwards.  */
  if (! last_addr)
    {
      last_addr = addr;
      last_symbol_section = symbol_section;
      return bfd_reloc_ok;
    }
  if (last_addr != addr)
    abort ();
  last_addr = 0;

  if (! symbol_section || last_symbol_section != symbol_section || end < start)
    return bfd_reloc_outofrange;

  /* Get the symbol_section contents.  */
  if (symbol_section != input_section)
    {
      if (elf_section_data (symbol_section)->this_hdr.contents != NULL)
	contents = elf_section_data (symbol_section)->this_hdr.contents;
      else
	{
	  free_contents = contents
	    = (bfd_byte *) bfd_malloc (symbol_section->_raw_size);
	  if (contents == NULL)
	    return bfd_reloc_outofrange;
	  if (! bfd_get_section_contents (input_bfd, symbol_section, contents,
					  (file_ptr) 0,
					  symbol_section->_raw_size))
	    {
	      free (contents);
	      return bfd_reloc_outofrange;
	    }
	}
    }
#define IS_PPI(PTR) ((bfd_get_16 (input_bfd, (PTR)) & 0xfc00) == 0xf800)
  start_ptr = contents + start;
  for (cum_diff = -6, ptr = contents + end; cum_diff < 0 && ptr > start_ptr;)
    {
      for (last_ptr = ptr, ptr -= 4; ptr >= start_ptr && IS_PPI (ptr);)
	ptr -= 2;
      ptr += 2;
      diff = (last_ptr - ptr) >> 1;
      cum_diff += diff & 1;
      cum_diff += diff;
    }
  /* Calculate the start / end values to load into rs / re minus four -
     so that will cancel out the four we would otherwise have to add to
     addr to get the value to subtract in order to get relative addressing.  */
  if (cum_diff >= 0)
    {
      start -= 4;
      end = (ptr + cum_diff * 2) - contents;
    }
  else
    {
      bfd_vma start0 = start - 4;

      while (start0 && IS_PPI (contents + start0))
	start0 -= 2;
      start0 = start - 2 - ((start - start0) & 2);
      start = start0 - cum_diff - 2;
      end = start0;
    }

  if (free_contents)
    free (free_contents);

  insn = bfd_get_16 (input_bfd, contents + addr);

  x = (insn & 0x200 ? end : start) - addr;
  if (input_section != symbol_section)
    x += ((symbol_section->output_section->vma + symbol_section->output_offset)
	  - (input_section->output_section->vma
	     + input_section->output_offset));
  x >>= 1;
  if (x < -128 || x > 127)
    return bfd_reloc_overflow;

  x = (insn & ~0xff) | (x & 0xff);
  bfd_put_16 (input_bfd, x, contents + addr);

  return bfd_reloc_ok;
}

/* This function is used for normal relocs.  This used to be like the COFF
   function, and is almost certainly incorrect for other ELF targets.  */

static bfd_reloc_status_type
sh_elf_reloc (abfd, reloc_entry, symbol_in, data, input_section, output_bfd,
	      error_message)
     bfd *abfd;
     arelent *reloc_entry;
     asymbol *symbol_in;
     PTR data;
     asection *input_section;
     bfd *output_bfd;
     char **error_message ATTRIBUTE_UNUSED;
{
  unsigned long insn;
  bfd_vma sym_value;
  enum elf_sh_reloc_type r_type;
  bfd_vma addr = reloc_entry->address;
  bfd_byte *hit_data = addr + (bfd_byte *) data;

  r_type = (enum elf_sh_reloc_type) reloc_entry->howto->type;

  if (output_bfd != NULL)
    {
      /* Partial linking--do nothing.  */
      reloc_entry->address += input_section->output_offset;
      return bfd_reloc_ok;
    }

  /* Almost all relocs have to do with relaxing.  If any work must be
     done for them, it has been done in sh_relax_section.  */
  if (r_type == R_SH_IND12W && (symbol_in->flags & BSF_LOCAL) != 0)
    return bfd_reloc_ok;

  if (symbol_in != NULL
      && bfd_is_und_section (symbol_in->section))
    return bfd_reloc_undefined;

  if (bfd_is_com_section (symbol_in->section))
    sym_value = 0;
  else
    sym_value = (symbol_in->value +
		 symbol_in->section->output_section->vma +
		 symbol_in->section->output_offset);

  switch (r_type)
    {
    case R_SH_DIR32:
      insn = bfd_get_32 (abfd, hit_data);
      insn += sym_value + reloc_entry->addend;
      bfd_put_32 (abfd, insn, hit_data);
      break;
    case R_SH_IND12W:
      insn = bfd_get_16 (abfd, hit_data);
      sym_value += reloc_entry->addend;
      sym_value -= (input_section->output_section->vma
		    + input_section->output_offset
		    + addr
		    + 4);
      sym_value += (insn & 0xfff) << 1;
      if (insn & 0x800)
	sym_value -= 0x1000;
      insn = (insn & 0xf000) | (sym_value & 0xfff);
      bfd_put_16 (abfd, insn, hit_data);
      if (sym_value < (bfd_vma) -0x1000 || sym_value >= 0x1000)
	return bfd_reloc_overflow;
      break;
    default:
      abort ();
      break;
    }

  return bfd_reloc_ok;
}

/* This function is used for relocs which are only used for relaxing,
   which the linker should otherwise ignore.  */

static bfd_reloc_status_type
sh_elf_ignore_reloc (abfd, reloc_entry, symbol, data, input_section,
		     output_bfd, error_message)
     bfd *abfd ATTRIBUTE_UNUSED;
     arelent *reloc_entry;
     asymbol *symbol ATTRIBUTE_UNUSED;
     PTR data ATTRIBUTE_UNUSED;
     asection *input_section;
     bfd *output_bfd;
     char **error_message ATTRIBUTE_UNUSED;
{
  if (output_bfd != NULL)
    reloc_entry->address += input_section->output_offset;
  return bfd_reloc_ok;
}

/* This structure is used to map BFD reloc codes to SH ELF relocs.  */

struct elf_reloc_map
{
  bfd_reloc_code_real_type bfd_reloc_val;
  unsigned char elf_reloc_val;
};

/* An array mapping BFD reloc codes to SH ELF relocs.  */

static const struct elf_reloc_map sh_reloc_map[] =
{
  { BFD_RELOC_NONE, R_SH_NONE },
  { BFD_RELOC_32, R_SH_DIR32 },
  { BFD_RELOC_CTOR, R_SH_DIR32 },
  { BFD_RELOC_32_PCREL, R_SH_REL32 },
  { BFD_RELOC_SH_PCDISP8BY2, R_SH_DIR8WPN },
  { BFD_RELOC_SH_PCDISP12BY2, R_SH_IND12W },
  { BFD_RELOC_SH_PCRELIMM8BY2, R_SH_DIR8WPZ },
  { BFD_RELOC_SH_PCRELIMM8BY4, R_SH_DIR8WPL },
  { BFD_RELOC_8_PCREL, R_SH_SWITCH8 },
  { BFD_RELOC_SH_SWITCH16, R_SH_SWITCH16 },
  { BFD_RELOC_SH_SWITCH32, R_SH_SWITCH32 },
  { BFD_RELOC_SH_USES, R_SH_USES },
  { BFD_RELOC_SH_COUNT, R_SH_COUNT },
  { BFD_RELOC_SH_ALIGN, R_SH_ALIGN },
  { BFD_RELOC_SH_CODE, R_SH_CODE },
  { BFD_RELOC_SH_DATA, R_SH_DATA },
  { BFD_RELOC_SH_LABEL, R_SH_LABEL },
  { BFD_RELOC_VTABLE_INHERIT, R_SH_GNU_VTINHERIT },
  { BFD_RELOC_VTABLE_ENTRY, R_SH_GNU_VTENTRY },
  { BFD_RELOC_SH_LOOP_START, R_SH_LOOP_START },
  { BFD_RELOC_SH_LOOP_END, R_SH_LOOP_END },
  { BFD_RELOC_32_GOT_PCREL, R_SH_GOT32 },
  { BFD_RELOC_32_PLT_PCREL, R_SH_PLT32 },
  { BFD_RELOC_SH_COPY, R_SH_COPY },
  { BFD_RELOC_SH_GLOB_DAT, R_SH_GLOB_DAT },
  { BFD_RELOC_SH_JMP_SLOT, R_SH_JMP_SLOT },
  { BFD_RELOC_SH_RELATIVE, R_SH_RELATIVE },
  { BFD_RELOC_32_GOTOFF, R_SH_GOTOFF },
  { BFD_RELOC_SH_GOTPC, R_SH_GOTPC },
};

/* Given a BFD reloc code, return the howto structure for the
   corresponding SH ELf reloc.  */

static reloc_howto_type *
sh_elf_reloc_type_lookup (abfd, code)
     bfd *abfd ATTRIBUTE_UNUSED;
     bfd_reloc_code_real_type code;
{
  unsigned int i;

  for (i = 0; i < sizeof (sh_reloc_map) / sizeof (struct elf_reloc_map); i++)
    {
      if (sh_reloc_map[i].bfd_reloc_val == code)
	return &sh_elf_howto_table[(int) sh_reloc_map[i].elf_reloc_val];
    }

  return NULL;
}

/* Given an ELF reloc, fill in the howto field of a relent.  */

static void
sh_elf_info_to_howto (abfd, cache_ptr, dst)
     bfd *abfd ATTRIBUTE_UNUSED;
     arelent *cache_ptr;
     Elf_Internal_Rela *dst;
{
  unsigned int r;

  r = ELF32_R_TYPE (dst->r_info);

  BFD_ASSERT (r < (unsigned int) R_SH_max);
  BFD_ASSERT (r < R_SH_FIRST_INVALID_RELOC || r > R_SH_LAST_INVALID_RELOC);
  BFD_ASSERT (r < R_SH_FIRST_INVALID_RELOC_2 || r > R_SH_LAST_INVALID_RELOC_2);

  cache_ptr->howto = &sh_elf_howto_table[r];
}

/* This function handles relaxing for SH ELF.  See the corresponding
   function in coff-sh.c for a description of what this does.  FIXME:
   There is a lot of duplication here between this code and the COFF
   specific code.  The format of relocs and symbols is wound deeply
   into this code, but it would still be better if the duplication
   could be eliminated somehow.  Note in particular that although both
   functions use symbols like R_SH_CODE, those symbols have different
   values; in coff-sh.c they come from include/coff/sh.h, whereas here
   they come from enum elf_sh_reloc_type in include/elf/sh.h.  */

static boolean
sh_elf_relax_section (abfd, sec, link_info, again)
     bfd *abfd;
     asection *sec;
     struct bfd_link_info *link_info;
     boolean *again;
{
  Elf_Internal_Shdr *symtab_hdr;
  Elf_Internal_Rela *internal_relocs;
  Elf_Internal_Rela *free_relocs = NULL;
  boolean have_code;
  Elf_Internal_Rela *irel, *irelend;
  bfd_byte *contents = NULL;
  bfd_byte *free_contents = NULL;
  Elf32_External_Sym *extsyms = NULL;
  Elf32_External_Sym *free_extsyms = NULL;

  *again = false;

  if (link_info->relocateable
      || (sec->flags & SEC_RELOC) == 0
      || sec->reloc_count == 0)
    return true;

  /* If this is the first time we have been called for this section,
     initialize the cooked size.  */
  if (sec->_cooked_size == 0)
    sec->_cooked_size = sec->_raw_size;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;

  internal_relocs = (_bfd_elf32_link_read_relocs
		     (abfd, sec, (PTR) NULL, (Elf_Internal_Rela *) NULL,
		      link_info->keep_memory));
  if (internal_relocs == NULL)
    goto error_return;
  if (! link_info->keep_memory)
    free_relocs = internal_relocs;

  have_code = false;

  irelend = internal_relocs + sec->reloc_count;
  for (irel = internal_relocs; irel < irelend; irel++)
    {
      bfd_vma laddr, paddr, symval;
      unsigned short insn;
      Elf_Internal_Rela *irelfn, *irelscan, *irelcount;
      bfd_signed_vma foff;

      if (ELF32_R_TYPE (irel->r_info) == (int) R_SH_CODE)
	have_code = true;

      if (ELF32_R_TYPE (irel->r_info) != (int) R_SH_USES)
	continue;

      /* Get the section contents.  */
      if (contents == NULL)
	{
	  if (elf_section_data (sec)->this_hdr.contents != NULL)
	    contents = elf_section_data (sec)->this_hdr.contents;
	  else
	    {
	      contents = (bfd_byte *) bfd_malloc (sec->_raw_size);
	      if (contents == NULL)
		goto error_return;
	      free_contents = contents;

	      if (! bfd_get_section_contents (abfd, sec, contents,
					      (file_ptr) 0, sec->_raw_size))
		goto error_return;
	    }
	}

      /* The r_addend field of the R_SH_USES reloc will point us to
         the register load.  The 4 is because the r_addend field is
         computed as though it were a jump offset, which are based
         from 4 bytes after the jump instruction.  */
      laddr = irel->r_offset + 4 + irel->r_addend;
      if (laddr >= sec->_raw_size)
	{
	  (*_bfd_error_handler) (_("%s: 0x%lx: warning: bad R_SH_USES offset"),
				 bfd_get_filename (abfd),
				 (unsigned long) irel->r_offset);
	  continue;
	}
      insn = bfd_get_16 (abfd, contents + laddr);

      /* If the instruction is not mov.l NN,rN, we don't know what to
         do.  */
      if ((insn & 0xf000) != 0xd000)
	{
	  ((*_bfd_error_handler)
	   (_("%s: 0x%lx: warning: R_SH_USES points to unrecognized insn 0x%x"),
	    bfd_get_filename (abfd), (unsigned long) irel->r_offset, insn));
	  continue;
	}

      /* Get the address from which the register is being loaded.  The
      	 displacement in the mov.l instruction is quadrupled.  It is a
      	 displacement from four bytes after the movl instruction, but,
      	 before adding in the PC address, two least significant bits
      	 of the PC are cleared.  We assume that the section is aligned
      	 on a four byte boundary.  */
      paddr = insn & 0xff;
      paddr *= 4;
      paddr += (laddr + 4) & ~3;
      if (paddr >= sec->_raw_size)
	{
	  ((*_bfd_error_handler)
	   (_("%s: 0x%lx: warning: bad R_SH_USES load offset"),
	    bfd_get_filename (abfd), (unsigned long) irel->r_offset));
	  continue;
	}

      /* Get the reloc for the address from which the register is
         being loaded.  This reloc will tell us which function is
         actually being called.  */
      for (irelfn = internal_relocs; irelfn < irelend; irelfn++)
	if (irelfn->r_offset == paddr
	    && ELF32_R_TYPE (irelfn->r_info) == (int) R_SH_DIR32)
	  break;
      if (irelfn >= irelend)
	{
	  ((*_bfd_error_handler)
	   (_("%s: 0x%lx: warning: could not find expected reloc"),
	    bfd_get_filename (abfd), (unsigned long) paddr));
	  continue;
	}

      /* Read this BFD's symbols if we haven't done so already.  */
      if (extsyms == NULL)
	{
	  if (symtab_hdr->contents != NULL)
	    extsyms = (Elf32_External_Sym *) symtab_hdr->contents;
	  else
	    {
	      extsyms = ((Elf32_External_Sym *)
			 bfd_malloc (symtab_hdr->sh_size));
	      if (extsyms == NULL)
		goto error_return;
	      free_extsyms = extsyms;
	      if (bfd_seek (abfd, symtab_hdr->sh_offset, SEEK_SET) != 0
		  || (bfd_read (extsyms, 1, symtab_hdr->sh_size, abfd)
		      != symtab_hdr->sh_size))
		goto error_return;
	    }
	}

      /* Get the value of the symbol referred to by the reloc.  */
      if (ELF32_R_SYM (irelfn->r_info) < symtab_hdr->sh_info)
	{
	  Elf_Internal_Sym isym;

	  /* A local symbol.  */
	  bfd_elf32_swap_symbol_in (abfd,
				    extsyms + ELF32_R_SYM (irelfn->r_info),
				    &isym);

	  if (isym.st_shndx != _bfd_elf_section_from_bfd_section (abfd, sec))
	    {
	      ((*_bfd_error_handler)
	       (_("%s: 0x%lx: warning: symbol in unexpected section"),
		bfd_get_filename (abfd), (unsigned long) paddr));
	      continue;
	    }

	  symval = (isym.st_value
		    + sec->output_section->vma
		    + sec->output_offset);
	}
      else
	{
	  unsigned long indx;
	  struct elf_link_hash_entry *h;

	  indx = ELF32_R_SYM (irelfn->r_info) - symtab_hdr->sh_info;
	  h = elf_sym_hashes (abfd)[indx];
	  BFD_ASSERT (h != NULL);
	  if (h->root.type != bfd_link_hash_defined
	      && h->root.type != bfd_link_hash_defweak)
	    {
	      /* This appears to be a reference to an undefined
                 symbol.  Just ignore it--it will be caught by the
                 regular reloc processing.  */
	      continue;
	    }

	  symval = (h->root.u.def.value
		    + h->root.u.def.section->output_section->vma
		    + h->root.u.def.section->output_offset);
	}

      symval += bfd_get_32 (abfd, contents + paddr);

      /* See if this function call can be shortened.  */
      foff = (symval
	      - (irel->r_offset
		 + sec->output_section->vma
		 + sec->output_offset
		 + 4));
      if (foff < -0x1000 || foff >= 0x1000)
	{
	  /* After all that work, we can't shorten this function call.  */
	  continue;
	}

      /* Shorten the function call.  */

      /* For simplicity of coding, we are going to modify the section
	 contents, the section relocs, and the BFD symbol table.  We
	 must tell the rest of the code not to free up this
	 information.  It would be possible to instead create a table
	 of changes which have to be made, as is done in coff-mips.c;
	 that would be more work, but would require less memory when
	 the linker is run.  */

      elf_section_data (sec)->relocs = internal_relocs;
      free_relocs = NULL;

      elf_section_data (sec)->this_hdr.contents = contents;
      free_contents = NULL;

      symtab_hdr->contents = (bfd_byte *) extsyms;
      free_extsyms = NULL;

      /* Replace the jsr with a bsr.  */

      /* Change the R_SH_USES reloc into an R_SH_IND12W reloc, and
         replace the jsr with a bsr.  */
      irel->r_info = ELF32_R_INFO (ELF32_R_SYM (irelfn->r_info), R_SH_IND12W);
      if (ELF32_R_SYM (irelfn->r_info) < symtab_hdr->sh_info)
	{
	  /* If this needs to be changed because of future relaxing,
             it will be handled here like other internal IND12W
             relocs.  */
	  bfd_put_16 (abfd,
		      0xb000 | ((foff >> 1) & 0xfff),
		      contents + irel->r_offset);
	}
      else
	{
	  /* We can't fully resolve this yet, because the external
             symbol value may be changed by future relaxing.  We let
             the final link phase handle it.  */
	  bfd_put_16 (abfd, 0xb000, contents + irel->r_offset);
	}

      /* See if there is another R_SH_USES reloc referring to the same
         register load.  */
      for (irelscan = internal_relocs; irelscan < irelend; irelscan++)
	if (ELF32_R_TYPE (irelscan->r_info) == (int) R_SH_USES
	    && laddr == irelscan->r_offset + 4 + irelscan->r_addend)
	  break;
      if (irelscan < irelend)
	{
	  /* Some other function call depends upon this register load,
	     and we have not yet converted that function call.
	     Indeed, we may never be able to convert it.  There is
	     nothing else we can do at this point.  */
	  continue;
	}

      /* Look for a R_SH_COUNT reloc on the location where the
         function address is stored.  Do this before deleting any
         bytes, to avoid confusion about the address.  */
      for (irelcount = internal_relocs; irelcount < irelend; irelcount++)
	if (irelcount->r_offset == paddr
	    && ELF32_R_TYPE (irelcount->r_info) == (int) R_SH_COUNT)
	  break;

      /* Delete the register load.  */
      if (! sh_elf_relax_delete_bytes (abfd, sec, laddr, 2))
	goto error_return;

      /* That will change things, so, just in case it permits some
         other function call to come within range, we should relax
         again.  Note that this is not required, and it may be slow.  */
      *again = true;

      /* Now check whether we got a COUNT reloc.  */
      if (irelcount >= irelend)
	{
	  ((*_bfd_error_handler)
	   (_("%s: 0x%lx: warning: could not find expected COUNT reloc"),
	    bfd_get_filename (abfd), (unsigned long) paddr));
	  continue;
	}

      /* The number of uses is stored in the r_addend field.  We've
         just deleted one.  */
      if (irelcount->r_addend == 0)
	{
	  ((*_bfd_error_handler) (_("%s: 0x%lx: warning: bad count"),
				  bfd_get_filename (abfd),
				  (unsigned long) paddr));
	  continue;
	}

      --irelcount->r_addend;

      /* If there are no more uses, we can delete the address.  Reload
         the address from irelfn, in case it was changed by the
         previous call to sh_elf_relax_delete_bytes.  */
      if (irelcount->r_addend == 0)
	{
	  if (! sh_elf_relax_delete_bytes (abfd, sec, irelfn->r_offset, 4))
	    goto error_return;
	}

      /* We've done all we can with that function call.  */
    }

  /* Look for load and store instructions that we can align on four
     byte boundaries.  */
  if (have_code)
    {
      boolean swapped;

      /* Get the section contents.  */
      if (contents == NULL)
	{
	  if (elf_section_data (sec)->this_hdr.contents != NULL)
	    contents = elf_section_data (sec)->this_hdr.contents;
	  else
	    {
	      contents = (bfd_byte *) bfd_malloc (sec->_raw_size);
	      if (contents == NULL)
		goto error_return;
	      free_contents = contents;

	      if (! bfd_get_section_contents (abfd, sec, contents,
					      (file_ptr) 0, sec->_raw_size))
		goto error_return;
	    }
	}

      if (! sh_elf_align_loads (abfd, sec, internal_relocs, contents,
				&swapped))
	goto error_return;

      if (swapped)
	{
	  elf_section_data (sec)->relocs = internal_relocs;
	  free_relocs = NULL;

	  elf_section_data (sec)->this_hdr.contents = contents;
	  free_contents = NULL;

	  symtab_hdr->contents = (bfd_byte *) extsyms;
	  free_extsyms = NULL;
	}
    }

  if (free_relocs != NULL)
    {
      free (free_relocs);
      free_relocs = NULL;
    }

  if (free_contents != NULL)
    {
      if (! link_info->keep_memory)
	free (free_contents);
      else
	{
	  /* Cache the section contents for elf_link_input_bfd.  */
	  elf_section_data (sec)->this_hdr.contents = contents;
	}
      free_contents = NULL;
    }

  if (free_extsyms != NULL)
    {
      if (! link_info->keep_memory)
	free (free_extsyms);
      else
	{
	  /* Cache the symbols for elf_link_input_bfd.  */
	  symtab_hdr->contents = extsyms;
	}
      free_extsyms = NULL;
    }

  return true;

 error_return:
  if (free_relocs != NULL)
    free (free_relocs);
  if (free_contents != NULL)
    free (free_contents);
  if (free_extsyms != NULL)
    free (free_extsyms);
  return false;
}

/* Delete some bytes from a section while relaxing.  FIXME: There is a
   lot of duplication between this function and sh_relax_delete_bytes
   in coff-sh.c.  */

static boolean
sh_elf_relax_delete_bytes (abfd, sec, addr, count)
     bfd *abfd;
     asection *sec;
     bfd_vma addr;
     int count;
{
  Elf_Internal_Shdr *symtab_hdr;
  Elf32_External_Sym *extsyms;
  int shndx, index;
  bfd_byte *contents;
  Elf_Internal_Rela *irel, *irelend;
  Elf_Internal_Rela *irelalign;
  bfd_vma toaddr;
  Elf32_External_Sym *esym, *esymend;
  struct elf_link_hash_entry *sym_hash;
  asection *o;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  extsyms = (Elf32_External_Sym *) symtab_hdr->contents;

  shndx = _bfd_elf_section_from_bfd_section (abfd, sec);

  contents = elf_section_data (sec)->this_hdr.contents;

  /* The deletion must stop at the next ALIGN reloc for an aligment
     power larger than the number of bytes we are deleting.  */

  irelalign = NULL;
  toaddr = sec->_cooked_size;

  irel = elf_section_data (sec)->relocs;
  irelend = irel + sec->reloc_count;
  for (; irel < irelend; irel++)
    {
      if (ELF32_R_TYPE (irel->r_info) == (int) R_SH_ALIGN
	  && irel->r_offset > addr
	  && count < (1 << irel->r_addend))
	{
	  irelalign = irel;
	  toaddr = irel->r_offset;
	  break;
	}
    }

  /* Actually delete the bytes.  */
  memmove (contents + addr, contents + addr + count, toaddr - addr - count);
  if (irelalign == NULL)
    sec->_cooked_size -= count;
  else
    {
      int i;

#define NOP_OPCODE (0x0009)

      BFD_ASSERT ((count & 1) == 0);
      for (i = 0; i < count; i += 2)
	bfd_put_16 (abfd, NOP_OPCODE, contents + toaddr - count + i);
    }

  /* Adjust all the relocs.  */
  for (irel = elf_section_data (sec)->relocs; irel < irelend; irel++)
    {
      bfd_vma nraddr, stop;
      bfd_vma start = 0;
      int insn = 0;
      Elf_Internal_Sym sym;
      int off, adjust, oinsn;
      bfd_signed_vma voff = 0;
      boolean overflow;

      /* Get the new reloc address.  */
      nraddr = irel->r_offset;
      if ((irel->r_offset > addr
	   && irel->r_offset < toaddr)
	  || (ELF32_R_TYPE (irel->r_info) == (int) R_SH_ALIGN
	      && irel->r_offset == toaddr))
	nraddr -= count;

      /* See if this reloc was for the bytes we have deleted, in which
	 case we no longer care about it.  Don't delete relocs which
	 represent addresses, though.  */
      if (irel->r_offset >= addr
	  && irel->r_offset < addr + count
	  && ELF32_R_TYPE (irel->r_info) != (int) R_SH_ALIGN
	  && ELF32_R_TYPE (irel->r_info) != (int) R_SH_CODE
	  && ELF32_R_TYPE (irel->r_info) != (int) R_SH_DATA
	  && ELF32_R_TYPE (irel->r_info) != (int) R_SH_LABEL)
	irel->r_info = ELF32_R_INFO (ELF32_R_SYM (irel->r_info),
				     (int) R_SH_NONE);

      /* If this is a PC relative reloc, see if the range it covers
         includes the bytes we have deleted.  */
      switch ((enum elf_sh_reloc_type) ELF32_R_TYPE (irel->r_info))
	{
	default:
	  break;

	case R_SH_DIR8WPN:
	case R_SH_IND12W:
	case R_SH_DIR8WPZ:
	case R_SH_DIR8WPL:
	  start = irel->r_offset;
	  insn = bfd_get_16 (abfd, contents + nraddr);
	  break;
	}

      switch ((enum elf_sh_reloc_type) ELF32_R_TYPE (irel->r_info))
	{
	default:
	  start = stop = addr;
	  break;

	case R_SH_DIR32:
	  /* If this reloc is against a symbol defined in this
             section, and the symbol will not be adjusted below, we
             must check the addend to see it will put the value in
             range to be adjusted, and hence must be changed.  */
	  if (ELF32_R_SYM (irel->r_info) < symtab_hdr->sh_info)
	    {
	      bfd_elf32_swap_symbol_in (abfd,
					extsyms + ELF32_R_SYM (irel->r_info),
					&sym);
	      if (sym.st_shndx == shndx
		  && (sym.st_value <= addr
		      || sym.st_value >= toaddr))
		{
		  bfd_vma val;

		  val = bfd_get_32 (abfd, contents + nraddr);
		  val += sym.st_value;
		  if (val > addr && val < toaddr)
		    bfd_put_32 (abfd, val - count, contents + nraddr);
		}
	    }
	  start = stop = addr;
	  break;

	case R_SH_DIR8WPN:
	  off = insn & 0xff;
	  if (off & 0x80)
	    off -= 0x100;
	  stop = (bfd_vma) ((bfd_signed_vma) start + 4 + off * 2);
	  break;

	case R_SH_IND12W:
	  if (ELF32_R_SYM (irel->r_info) >= symtab_hdr->sh_info)
	    start = stop = addr;
	  else
	    {
	      off = insn & 0xfff;
	      if (off & 0x800)
		off -= 0x1000;
	      stop = (bfd_vma) ((bfd_signed_vma) start + 4 + off * 2);
	    }
	  break;

	case R_SH_DIR8WPZ:
	  off = insn & 0xff;
	  stop = start + 4 + off * 2;
	  break;

	case R_SH_DIR8WPL:
	  off = insn & 0xff;
	  stop = (start & ~(bfd_vma) 3) + 4 + off * 4;
	  break;

	case R_SH_SWITCH8:
	case R_SH_SWITCH16:
	case R_SH_SWITCH32:
	  /* These relocs types represent
	       .word L2-L1
	     The r_addend field holds the difference between the reloc
	     address and L1.  That is the start of the reloc, and
	     adding in the contents gives us the top.  We must adjust
	     both the r_offset field and the section contents.
	     N.B. in gas / coff bfd, the elf bfd r_addend is called r_offset,
	     and the elf bfd r_offset is called r_vaddr.  */

	  stop = irel->r_offset;
	  start = (bfd_vma) ((bfd_signed_vma) stop - (long) irel->r_addend);

	  if (start > addr
	      && start < toaddr
	      && (stop <= addr || stop >= toaddr))
	    irel->r_addend += count;
	  else if (stop > addr
		   && stop < toaddr
		   && (start <= addr || start >= toaddr))
	    irel->r_addend -= count;

	  if (ELF32_R_TYPE (irel->r_info) == (int) R_SH_SWITCH16)
	    voff = bfd_get_signed_16 (abfd, contents + nraddr);
	  else if (ELF32_R_TYPE (irel->r_info) == (int) R_SH_SWITCH8)
	    voff = bfd_get_8 (abfd, contents + nraddr);
	  else
	    voff = bfd_get_signed_32 (abfd, contents + nraddr);
	  stop = (bfd_vma) ((bfd_signed_vma) start + voff);

	  break;

	case R_SH_USES:
	  start = irel->r_offset;
	  stop = (bfd_vma) ((bfd_signed_vma) start
			    + (long) irel->r_addend
			    + 4);
	  break;
	}

      if (start > addr
	  && start < toaddr
	  && (stop <= addr || stop >= toaddr))
	adjust = count;
      else if (stop > addr
	       && stop < toaddr
	       && (start <= addr || start >= toaddr))
	adjust = - count;
      else
	adjust = 0;

      if (adjust != 0)
	{
	  oinsn = insn;
	  overflow = false;
	  switch ((enum elf_sh_reloc_type) ELF32_R_TYPE (irel->r_info))
	    {
	    default:
	      abort ();
	      break;

	    case R_SH_DIR8WPN:
	    case R_SH_DIR8WPZ:
	      insn += adjust / 2;
	      if ((oinsn & 0xff00) != (insn & 0xff00))
		overflow = true;
	      bfd_put_16 (abfd, insn, contents + nraddr);
	      break;

	    case R_SH_IND12W:
	      insn += adjust / 2;
	      if ((oinsn & 0xf000) != (insn & 0xf000))
		overflow = true;
	      bfd_put_16 (abfd, insn, contents + nraddr);
	      break;

	    case R_SH_DIR8WPL:
	      BFD_ASSERT (adjust == count || count >= 4);
	      if (count >= 4)
		insn += adjust / 4;
	      else
		{
		  if ((irel->r_offset & 3) == 0)
		    ++insn;
		}
	      if ((oinsn & 0xff00) != (insn & 0xff00))
		overflow = true;
	      bfd_put_16 (abfd, insn, contents + nraddr);
	      break;

	    case R_SH_SWITCH8:
	      voff += adjust;
	      if (voff < 0 || voff >= 0xff)
		overflow = true;
	      bfd_put_8 (abfd, voff, contents + nraddr);
	      break;

	    case R_SH_SWITCH16:
	      voff += adjust;
	      if (voff < - 0x8000 || voff >= 0x8000)
		overflow = true;
	      bfd_put_signed_16 (abfd, voff, contents + nraddr);
	      break;

	    case R_SH_SWITCH32:
	      voff += adjust;
	      bfd_put_signed_32 (abfd, voff, contents + nraddr);
	      break;

	    case R_SH_USES:
	      irel->r_addend += adjust;
	      break;
	    }

	  if (overflow)
	    {
	      ((*_bfd_error_handler)
	       (_("%s: 0x%lx: fatal: reloc overflow while relaxing"),
		bfd_get_filename (abfd), (unsigned long) irel->r_offset));
	      bfd_set_error (bfd_error_bad_value);
	      return false;
	    }
	}

      irel->r_offset = nraddr;
    }

  /* Look through all the other sections.  If there contain any IMM32
     relocs against internal symbols which we are not going to adjust
     below, we may need to adjust the addends.  */
  for (o = abfd->sections; o != NULL; o = o->next)
    {
      Elf_Internal_Rela *internal_relocs;
      Elf_Internal_Rela *irelscan, *irelscanend;
      bfd_byte *ocontents;

      if (o == sec
	  || (o->flags & SEC_RELOC) == 0
	  || o->reloc_count == 0)
	continue;

      /* We always cache the relocs.  Perhaps, if info->keep_memory is
         false, we should free them, if we are permitted to, when we
         leave sh_coff_relax_section.  */
      internal_relocs = (_bfd_elf32_link_read_relocs
			 (abfd, o, (PTR) NULL, (Elf_Internal_Rela *) NULL,
			  true));
      if (internal_relocs == NULL)
	return false;

      ocontents = NULL;
      irelscanend = internal_relocs + o->reloc_count;
      for (irelscan = internal_relocs; irelscan < irelscanend; irelscan++)
	{
	  Elf_Internal_Sym sym;

	  /* Dwarf line numbers use R_SH_SWITCH32 relocs.  */
	  if (ELF32_R_TYPE (irelscan->r_info) == (int) R_SH_SWITCH32)
	    {
	      bfd_vma start, stop;
	      bfd_signed_vma voff;

	      if (ocontents == NULL)
		{
		  if (elf_section_data (o)->this_hdr.contents != NULL)
		    ocontents = elf_section_data (o)->this_hdr.contents;
		  else
		    {
		      /* We always cache the section contents.
                         Perhaps, if info->keep_memory is false, we
                         should free them, if we are permitted to,
                         when we leave sh_coff_relax_section.  */
		      ocontents = (bfd_byte *) bfd_malloc (o->_raw_size);
		      if (ocontents == NULL)
			return false;
		      if (! bfd_get_section_contents (abfd, o, ocontents,
						      (file_ptr) 0,
						      o->_raw_size))
			return false;
		      elf_section_data (o)->this_hdr.contents = ocontents;
		    }
		}

	      stop = irelscan->r_offset;
	      start
		= (bfd_vma) ((bfd_signed_vma) stop - (long) irelscan->r_addend);

	      /* STOP is in a different section, so it won't change.  */
	      if (start > addr && start < toaddr)
		irelscan->r_addend += count;

	      voff = bfd_get_signed_32 (abfd, ocontents + irelscan->r_offset);
	      stop = (bfd_vma) ((bfd_signed_vma) start + voff);

	      if (start > addr
		  && start < toaddr
		  && (stop <= addr || stop >= toaddr))
		bfd_put_signed_32 (abfd, voff + count,
				   ocontents + irelscan->r_offset);
	      else if (stop > addr
		       && stop < toaddr
		       && (start <= addr || start >= toaddr))
		bfd_put_signed_32 (abfd, voff - count,
				   ocontents + irelscan->r_offset);
	    }

	  if (ELF32_R_TYPE (irelscan->r_info) != (int) R_SH_DIR32)
	    continue;

	  if (ELF32_R_SYM (irelscan->r_info) >= symtab_hdr->sh_info)
	    continue;

	  bfd_elf32_swap_symbol_in (abfd,
				    extsyms + ELF32_R_SYM (irelscan->r_info),
				    &sym);

	  if (sym.st_shndx == shndx
	      && (sym.st_value <= addr
		  || sym.st_value >= toaddr))
	    {
	      bfd_vma val;

	      if (ocontents == NULL)
		{
		  if (elf_section_data (o)->this_hdr.contents != NULL)
		    ocontents = elf_section_data (o)->this_hdr.contents;
		  else
		    {
		      /* We always cache the section contents.
                         Perhaps, if info->keep_memory is false, we
                         should free them, if we are permitted to,
                         when we leave sh_coff_relax_section.  */
		      ocontents = (bfd_byte *) bfd_malloc (o->_raw_size);
		      if (ocontents == NULL)
			return false;
		      if (! bfd_get_section_contents (abfd, o, ocontents,
						      (file_ptr) 0,
						      o->_raw_size))
			return false;
		      elf_section_data (o)->this_hdr.contents = ocontents;
		    }
		}

	      val = bfd_get_32 (abfd, ocontents + irelscan->r_offset);
	      val += sym.st_value;
	      if (val > addr && val < toaddr)
		bfd_put_32 (abfd, val - count,
			    ocontents + irelscan->r_offset);
	    }
	}
    }

  /* Adjust the local symbols defined in this section.  */
  esym = extsyms;
  esymend = esym + symtab_hdr->sh_info;
  for (; esym < esymend; esym++)
    {
      Elf_Internal_Sym isym;

      bfd_elf32_swap_symbol_in (abfd, esym, &isym);

      if (isym.st_shndx == shndx
	  && isym.st_value > addr
	  && isym.st_value < toaddr)
	{
	  isym.st_value -= count;
	  bfd_elf32_swap_symbol_out (abfd, &isym, esym);
	}
    }

  /* Now adjust the global symbols defined in this section.  */
  esym = extsyms + symtab_hdr->sh_info;
  esymend = extsyms + (symtab_hdr->sh_size / sizeof (Elf32_External_Sym));
  for (index = 0; esym < esymend; esym++, index++)
    {
      Elf_Internal_Sym isym;

      bfd_elf32_swap_symbol_in (abfd, esym, &isym);
      sym_hash = elf_sym_hashes (abfd)[index];
      if (isym.st_shndx == shndx
	  && ((sym_hash)->root.type == bfd_link_hash_defined
	      || (sym_hash)->root.type == bfd_link_hash_defweak)
	  && (sym_hash)->root.u.def.section == sec
	  && (sym_hash)->root.u.def.value > addr
	  && (sym_hash)->root.u.def.value < toaddr)
	{
	  (sym_hash)->root.u.def.value -= count;
	}
    }

  /* See if we can move the ALIGN reloc forward.  We have adjusted
     r_offset for it already.  */
  if (irelalign != NULL)
    {
      bfd_vma alignto, alignaddr;

      alignto = BFD_ALIGN (toaddr, 1 << irelalign->r_addend);
      alignaddr = BFD_ALIGN (irelalign->r_offset,
			     1 << irelalign->r_addend);
      if (alignto != alignaddr)
	{
	  /* Tail recursion.  */
	  return sh_elf_relax_delete_bytes (abfd, sec, alignaddr,
					    alignto - alignaddr);
	}
    }

  return true;
}

/* Look for loads and stores which we can align to four byte
   boundaries.  This is like sh_align_loads in coff-sh.c.  */

static boolean
sh_elf_align_loads (abfd, sec, internal_relocs, contents, pswapped)
     bfd *abfd;
     asection *sec;
     Elf_Internal_Rela *internal_relocs;
     bfd_byte *contents;
     boolean *pswapped;
{
  Elf_Internal_Rela *irel, *irelend;
  bfd_vma *labels = NULL;
  bfd_vma *label, *label_end;

  *pswapped = false;

  irelend = internal_relocs + sec->reloc_count;

  /* Get all the addresses with labels on them.  */
  labels = (bfd_vma *) bfd_malloc (sec->reloc_count * sizeof (bfd_vma));
  if (labels == NULL)
    goto error_return;
  label_end = labels;
  for (irel = internal_relocs; irel < irelend; irel++)
    {
      if (ELF32_R_TYPE (irel->r_info) == (int) R_SH_LABEL)
	{
	  *label_end = irel->r_offset;
	  ++label_end;
	}
    }

  /* Note that the assembler currently always outputs relocs in
     address order.  If that ever changes, this code will need to sort
     the label values and the relocs.  */

  label = labels;

  for (irel = internal_relocs; irel < irelend; irel++)
    {
      bfd_vma start, stop;

      if (ELF32_R_TYPE (irel->r_info) != (int) R_SH_CODE)
	continue;

      start = irel->r_offset;

      for (irel++; irel < irelend; irel++)
	if (ELF32_R_TYPE (irel->r_info) == (int) R_SH_DATA)
	  break;
      if (irel < irelend)
	stop = irel->r_offset;
      else
	stop = sec->_cooked_size;

      if (! _bfd_sh_align_load_span (abfd, sec, contents, sh_elf_swap_insns,
				     (PTR) internal_relocs, &label,
				     label_end, start, stop, pswapped))
	goto error_return;
    }

  free (labels);

  return true;

 error_return:
  if (labels != NULL)
    free (labels);
  return false;
}

/* Swap two SH instructions.  This is like sh_swap_insns in coff-sh.c.  */

static boolean
sh_elf_swap_insns (abfd, sec, relocs, contents, addr)
     bfd *abfd;
     asection *sec;
     PTR relocs;
     bfd_byte *contents;
     bfd_vma addr;
{
  Elf_Internal_Rela *internal_relocs = (Elf_Internal_Rela *) relocs;
  unsigned short i1, i2;
  Elf_Internal_Rela *irel, *irelend;

  /* Swap the instructions themselves.  */
  i1 = bfd_get_16 (abfd, contents + addr);
  i2 = bfd_get_16 (abfd, contents + addr + 2);
  bfd_put_16 (abfd, i2, contents + addr);
  bfd_put_16 (abfd, i1, contents + addr + 2);

  /* Adjust all reloc addresses.  */
  irelend = internal_relocs + sec->reloc_count;
  for (irel = internal_relocs; irel < irelend; irel++)
    {
      enum elf_sh_reloc_type type;
      int add;

      /* There are a few special types of relocs that we don't want to
         adjust.  These relocs do not apply to the instruction itself,
         but are only associated with the address.  */
      type = (enum elf_sh_reloc_type) ELF32_R_TYPE (irel->r_info);
      if (type == R_SH_ALIGN
	  || type == R_SH_CODE
	  || type == R_SH_DATA
	  || type == R_SH_LABEL)
	continue;

      /* If an R_SH_USES reloc points to one of the addresses being
         swapped, we must adjust it.  It would be incorrect to do this
         for a jump, though, since we want to execute both
         instructions after the jump.  (We have avoided swapping
         around a label, so the jump will not wind up executing an
         instruction it shouldn't).  */
      if (type == R_SH_USES)
	{
	  bfd_vma off;

	  off = irel->r_offset + 4 + irel->r_addend;
	  if (off == addr)
	    irel->r_offset += 2;
	  else if (off == addr + 2)
	    irel->r_offset -= 2;
	}

      if (irel->r_offset == addr)
	{
	  irel->r_offset += 2;
	  add = -2;
	}
      else if (irel->r_offset == addr + 2)
	{
	  irel->r_offset -= 2;
	  add = 2;
	}
      else
	add = 0;

      if (add != 0)
	{
	  bfd_byte *loc;
	  unsigned short insn, oinsn;
	  boolean overflow;

	  loc = contents + irel->r_offset;
	  overflow = false;
	  switch (type)
	    {
	    default:
	      break;

	    case R_SH_DIR8WPN:
	    case R_SH_DIR8WPZ:
	      insn = bfd_get_16 (abfd, loc);
	      oinsn = insn;
	      insn += add / 2;
	      if ((oinsn & 0xff00) != (insn & 0xff00))
		overflow = true;
	      bfd_put_16 (abfd, insn, loc);
	      break;

	    case R_SH_IND12W:
	      insn = bfd_get_16 (abfd, loc);
	      oinsn = insn;
	      insn += add / 2;
	      if ((oinsn & 0xf000) != (insn & 0xf000))
		overflow = true;
	      bfd_put_16 (abfd, insn, loc);
	      break;

	    case R_SH_DIR8WPL:
	      /* This reloc ignores the least significant 3 bits of
                 the program counter before adding in the offset.
                 This means that if ADDR is at an even address, the
                 swap will not affect the offset.  If ADDR is an at an
                 odd address, then the instruction will be crossing a
                 four byte boundary, and must be adjusted.  */
	      if ((addr & 3) != 0)
		{
		  insn = bfd_get_16 (abfd, loc);
		  oinsn = insn;
		  insn += add / 2;
		  if ((oinsn & 0xff00) != (insn & 0xff00))
		    overflow = true;
		  bfd_put_16 (abfd, insn, loc);
		}

	      break;
	    }

	  if (overflow)
	    {
	      ((*_bfd_error_handler)
	       (_("%s: 0x%lx: fatal: reloc overflow while relaxing"),
		bfd_get_filename (abfd), (unsigned long) irel->r_offset));
	      bfd_set_error (bfd_error_bad_value);
	      return false;
	    }
	}
    }

  return true;
}

/* The size in bytes of an entry in the procedure linkage table.  */

#define PLT_ENTRY_SIZE 28

/* First entry in an absolute procedure linkage table look like this.  */

#if 1
/* Note - this code has been "optimised" not to use r2.  r2 is used by
   GCC to return the address of large strutcures, so it should not be
   corrupted here.  This does mean however, that this PLT does not conform
   to the SH PIC ABI.  That spec says that r0 contains the type of the PLT
   and r2 contains the GOT id.  This version stores the GOT id in r0 and
   ignores the type.  Loaders can easily detect this difference however,
   since the type will always be 0 or 8, and the GOT ids will always be
   greater than or equal to 12.  */
static const bfd_byte elf_sh_plt0_entry_be[PLT_ENTRY_SIZE] =
{
  0xd0, 0x05,	/* mov.l 2f,r0 */
  0x60, 0x02,	/* mov.l @r0,r0 */
  0x2f, 0x06,	/* mov.l r0,@-r15 */
  0xd0, 0x03,	/* mov.l 1f,r0 */
  0x60, 0x02,	/* mov.l @r0,r0 */
  0x40, 0x2b,	/* jmp @r0 */
  0x60, 0xf6,	/*  mov.l @r15+,r0 */
  0x00, 0x09,	/* nop */
  0x00, 0x09,	/* nop */
  0x00, 0x09,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of .got.plt + 8.  */
  0, 0, 0, 0,	/* 2: replaced with address of .got.plt + 4.  */
};

static const bfd_byte elf_sh_plt0_entry_le[PLT_ENTRY_SIZE] =
{
  0x05, 0xd0,	/* mov.l 2f,r0 */
  0x02, 0x60,	/* mov.l @r0,r0 */
  0x06, 0x2f,	/* mov.l r0,@-r15 */
  0x03, 0xd0,	/* mov.l 1f,r0 */
  0x02, 0x60,	/* mov.l @r0,r0 */
  0x2b, 0x40,	/* jmp @r0 */
  0xf6, 0x60,	/*  mov.l @r15+,r0 */
  0x09, 0x00,	/* nop */
  0x09, 0x00,	/* nop */
  0x09, 0x00,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of .got.plt + 8.  */
  0, 0, 0, 0,	/* 2: replaced with address of .got.plt + 4.  */
};

/* Sebsequent entries in an absolute procedure linkage table look like
   this.  */

static const bfd_byte elf_sh_plt_entry_be[PLT_ENTRY_SIZE] =
{
  0xd0, 0x04,	/* mov.l 1f,r0 */
  0x60, 0x02,	/* mov.l @r0,r0 */
  0xd1, 0x02,	/* mov.l 0f,r1 */
  0x40, 0x2b,   /* jmp @r0 */
  0x60, 0x13,	/*  mov r1,r0 */
  0xd1, 0x03,	/* mov.l 2f,r1 */
  0x40, 0x2b,	/* jmp @r0 */
  0x00, 0x09,	/* nop */
  0, 0, 0, 0,	/* 0: replaced with address of .PLT0.  */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0,	/* 2: replaced with offset into relocation table.  */
};

static const bfd_byte elf_sh_plt_entry_le[PLT_ENTRY_SIZE] =
{
  0x04, 0xd0,	/* mov.l 1f,r0 */
  0x02, 0x60,	/* mov.l @r0,r0 */
  0x02, 0xd1,	/* mov.l 0f,r1 */
  0x2b, 0x40,   /* jmp @r0 */
  0x13, 0x60,	/*  mov r1,r0 */
  0x03, 0xd1,	/* mov.l 2f,r1 */
  0x2b, 0x40,	/* jmp @r0 */
  0x09, 0x00,	/*  nop */
  0, 0, 0, 0,	/* 0: replaced with address of .PLT0.  */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0,	/* 2: replaced with offset into relocation table.  */
};

/* Entries in a PIC procedure linkage table look like this.  */

static const bfd_byte elf_sh_pic_plt_entry_be[PLT_ENTRY_SIZE] =
{
  0xd0, 0x04,	/* mov.l 1f,r0 */
  0x00, 0xce,	/* mov.l @(r0,r12),r0 */
  0x40, 0x2b,	/* jmp @r0 */
  0x00, 0x09,	/*  nop */
  0x50, 0xc2,	/* mov.l @(8,r12),r0 */
  0xd1, 0x03,	/* mov.l 2f,r1 */
  0x40, 0x2b,	/* jmp @r0 */
  0x50, 0xc1,	/*  mov.l @(4,r12),r0 */
  0x00, 0x09,	/* nop */
  0x00, 0x09,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0    /* 2: replaced with offset into relocation table.  */
};

static const bfd_byte elf_sh_pic_plt_entry_le[PLT_ENTRY_SIZE] =
{
  0x04, 0xd0,	/* mov.l 1f,r0 */
  0xce, 0x00,	/* mov.l @(r0,r12),r0 */
  0x2b, 0x40,	/* jmp @r0 */
  0x09, 0x00,	/*  nop */
  0xc2, 0x50,	/* mov.l @(8,r12),r0 */
  0x03, 0xd1,	/* mov.l 2f,r1 */
  0x2b, 0x40,	/* jmp @r0 */
  0xc1, 0x50,	/*  mov.l @(4,r12),r0 */
  0x09, 0x00,	/*  nop */
  0x09, 0x00,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0    /* 2: replaced with offset into relocation table.  */
};

#else /* These are the old style PLT entries.  */
static const bfd_byte elf_sh_plt0_entry_be[PLT_ENTRY_SIZE] =
{
  0xd0, 0x04,	/* mov.l 1f,r0 */
  0xd2, 0x05,	/* mov.l 2f,r2 */
  0x60, 0x02,	/* mov.l @r0,r0 */
  0x62, 0x22,	/* mov.l @r2,r2 */
  0x40, 0x2b,	/* jmp @r0 */
  0xe0, 0x00,	/*  mov #0,r0 */
  0x00, 0x09,	/* nop */
  0x00, 0x09,	/* nop */
  0x00, 0x09,	/* nop */
  0x00, 0x09,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of .got.plt + 8.  */
  0, 0, 0, 0,	/* 2: replaced with address of .got.plt + 4.  */
};

static const bfd_byte elf_sh_plt0_entry_le[PLT_ENTRY_SIZE] =
{
  0x04, 0xd0,	/* mov.l 1f,r0 */
  0x05, 0xd2,	/* mov.l 2f,r2 */
  0x02, 0x60,	/* mov.l @r0,r0 */
  0x22, 0x62,	/* mov.l @r2,r2 */
  0x2b, 0x40,	/* jmp @r0 */
  0x00, 0xe0,	/*  mov #0,r0 */
  0x09, 0x00,	/* nop */
  0x09, 0x00,	/* nop */
  0x09, 0x00,	/* nop */
  0x09, 0x00,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of .got.plt + 8.  */
  0, 0, 0, 0,	/* 2: replaced with address of .got.plt + 4.  */
};

/* Sebsequent entries in an absolute procedure linkage table look like
   this.  */

static const bfd_byte elf_sh_plt_entry_be[PLT_ENTRY_SIZE] =
{
  0xd0, 0x04,	/* mov.l 1f,r0 */
  0x60, 0x02,	/* mov.l @r0,r0 */
  0xd2, 0x02,	/* mov.l 0f,r2 */
  0x40, 0x2b,   /* jmp @r0 */
  0x60, 0x23,	/*  mov r2,r0 */
  0xd1, 0x03,	/* mov.l 2f,r1 */
  0x40, 0x2b,	/* jmp @r0 */
  0x00, 0x09,	/* nop */
  0, 0, 0, 0,	/* 0: replaced with address of .PLT0.  */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0,	/* 2: replaced with offset into relocation table.  */
};

static const bfd_byte elf_sh_plt_entry_le[PLT_ENTRY_SIZE] =
{
  0x04, 0xd0,	/* mov.l 1f,r0 */
  0x02, 0x60,	/* mov.l @r0,r0 */
  0x02, 0xd2,	/* mov.l 0f,r2 */
  0x2b, 0x40,   /* jmp @r0 */
  0x23, 0x60,	/*  mov r2,r0 */
  0x03, 0xd1,	/* mov.l 2f,r1 */
  0x2b, 0x40,	/* jmp @r0 */
  0x09, 0x00,	/*  nop */
  0, 0, 0, 0,	/* 0: replaced with address of .PLT.  */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0,	/* 2: replaced with offset into relocation table.  */
};

/* Entries in a PIC procedure linkage table look like this.  */

static const bfd_byte elf_sh_pic_plt_entry_be[PLT_ENTRY_SIZE] =
{
  0xd0, 0x04,	/* mov.l 1f,r0 */
  0x00, 0xce,	/* mov.l @(r0,r12),r0 */
  0x40, 0x2b,	/* jmp @r0 */
  0x00, 0x09,	/*  nop */
  0x50, 0xc2,	/* 0: mov.l @(8,r12),r0 */
  0x52, 0xc1,	/* 1: mov.l @(4,r12),r2 */
  0xd1, 0x02,	/* mov.l 2f,r1 */
  0x40, 0x2b,	/* jmp @r0 */
  0xe0, 0x00,	/*  mov #0,r0 ! shows the type of PLT.  */
  0x00, 0x09,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0    /* 2: replaced with offset into relocation table.  */
};

static const bfd_byte elf_sh_pic_plt_entry_le[PLT_ENTRY_SIZE] =
{
  0x04, 0xd0,	/* mov.l 1f,r0 */
  0xce, 0x00,	/* mov.l @(r0,r12),r0 */
  0x2b, 0x40,	/* jmp @r0 */
  0x09, 0x00,	/*  nop */
  0xc2, 0x50,	/* 0: mov.l @(8,r12),r0 */
  0xc1, 0x52,	/* 1: mov.l @(4,r12),r2 */
  0x02, 0xd1,	/* mov.l 2f,r1 */
  0x2b, 0x40,	/* jmp @r0 */
  0x00, 0xe0,	/*  mov #0,r0 ! shows the type of PLT.  */
  0x09, 0x00,	/* nop */
  0, 0, 0, 0,	/* 1: replaced with address of this symbol in .got.  */
  0, 0, 0, 0    /* 2: replaced with offset into relocation table.  */
};
#endif /* old style PLT entries.  */

static const bfd_byte *elf_sh_plt0_entry;
static const bfd_byte *elf_sh_plt_entry;
static const bfd_byte *elf_sh_pic_plt_entry;

/* Return size of a PLT entry.  */
#define elf_sh_sizeof_plt(info) PLT_ENTRY_SIZE

/* Return offset of the PLT0 address in an absolute PLT entry.  */
#define elf_sh_plt_plt0_offset(info) 16

/* Return offset of the linker in PLT0 entry.  */
#define elf_sh_plt0_linker_offset(info) 20

/* Return offset of the GOT id in PLT0 entry.  */
#define elf_sh_plt0_gotid_offset(info) 24

/* Return offset of the tempoline in PLT entry */
#define elf_sh_plt_temp_offset(info) 8

/* Return offset of the symbol in PLT entry.  */
#define elf_sh_plt_symbol_offset(info) 20

/* Return offset of the relocation in PLT entry.  */
#define elf_sh_plt_reloc_offset(info) 24

/* The sh linker needs to keep track of the number of relocs that it
   decides to copy in check_relocs for each symbol.  This is so that
   it can discard PC relative relocs if it doesn't need them when
   linking with -Bsymbolic.  We store the information in a field
   extending the regular ELF linker hash table.  */

/* This structure keeps track of the number of PC relative relocs we
   have copied for a given symbol.  */

struct elf_sh_pcrel_relocs_copied
{
  /* Next section.  */
  struct elf_sh_pcrel_relocs_copied *next;
  /* A section in dynobj.  */
  asection *section;
  /* Number of relocs copied in this section.  */
  bfd_size_type count;
};

/* sh ELF linker hash entry.  */

struct elf_sh_link_hash_entry
{
  struct elf_link_hash_entry root;

  /* Number of PC relative relocs copied for this symbol.  */
  struct elf_sh_pcrel_relocs_copied *pcrel_relocs_copied;
};

/* sh ELF linker hash table.  */

struct elf_sh_link_hash_table
{
  struct elf_link_hash_table root;
};

/* Declare this now that the above structures are defined.  */

static boolean sh_elf_discard_copies
  PARAMS ((struct elf_sh_link_hash_entry *, PTR));

/* Traverse an sh ELF linker hash table.  */

#define sh_elf_link_hash_traverse(table, func, info)			\
  (elf_link_hash_traverse						\
   (&(table)->root,							\
    (boolean (*) PARAMS ((struct elf_link_hash_entry *, PTR))) (func),	\
    (info)))

/* Get the sh ELF linker hash table from a link_info structure.  */

#define sh_elf_hash_table(p) \
  ((struct elf_sh_link_hash_table *) ((p)->hash))

/* Create an entry in an sh ELF linker hash table.  */

static struct bfd_hash_entry *
sh_elf_link_hash_newfunc (entry, table, string)
     struct bfd_hash_entry *entry;
     struct bfd_hash_table *table;
     const char *string;
{
  struct elf_sh_link_hash_entry *ret =
    (struct elf_sh_link_hash_entry *) entry;

  /* Allocate the structure if it has not already been allocated by a
     subclass.  */
  if (ret == (struct elf_sh_link_hash_entry *) NULL)
    ret = ((struct elf_sh_link_hash_entry *)
	   bfd_hash_allocate (table,
			      sizeof (struct elf_sh_link_hash_entry)));
  if (ret == (struct elf_sh_link_hash_entry *) NULL)
    return (struct bfd_hash_entry *) ret;

  /* Call the allocation method of the superclass.  */
  ret = ((struct elf_sh_link_hash_entry *)
	 _bfd_elf_link_hash_newfunc ((struct bfd_hash_entry *) ret,
				     table, string));
  if (ret != (struct elf_sh_link_hash_entry *) NULL)
    {
      ret->pcrel_relocs_copied = NULL;
    }

  return (struct bfd_hash_entry *) ret;
}

/* Create an sh ELF linker hash table.  */

static struct bfd_link_hash_table *
sh_elf_link_hash_table_create (abfd)
     bfd *abfd;
{
  struct elf_sh_link_hash_table *ret;

  ret = ((struct elf_sh_link_hash_table *)
	 bfd_alloc (abfd, sizeof (struct elf_sh_link_hash_table)));
  if (ret == (struct elf_sh_link_hash_table *) NULL)
    return NULL;

  if (! _bfd_elf_link_hash_table_init (&ret->root, abfd,
				       sh_elf_link_hash_newfunc))
    {
      bfd_release (abfd, ret);
      return NULL;
    }

  return &ret->root.root;
}

/* Create dynamic sections when linking against a dynamic object.  */

static boolean
sh_elf_create_dynamic_sections (abfd, info)
     bfd *abfd;
     struct bfd_link_info *info;
{
  flagword flags, pltflags;
  register asection *s;
  struct elf_backend_data *bed = get_elf_backend_data (abfd);
  int ptralign = 0;

  switch (bed->s->arch_size)
    {
    case 32:
      ptralign = 2;
      break;

    case 64:
      ptralign = 3;
      break;

    default:
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* We need to create .plt, .rel[a].plt, .got, .got.plt, .dynbss, and
     .rel[a].bss sections.  */

  flags = (SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS | SEC_IN_MEMORY
	   | SEC_LINKER_CREATED);

  pltflags = flags;
  pltflags |= SEC_CODE;
  if (bed->plt_not_loaded)
    pltflags &= ~ (SEC_LOAD | SEC_HAS_CONTENTS);
  if (bed->plt_readonly)
    pltflags |= SEC_READONLY;

  s = bfd_make_section (abfd, ".plt");
  if (s == NULL
      || ! bfd_set_section_flags (abfd, s, pltflags)
      || ! bfd_set_section_alignment (abfd, s, bed->plt_alignment))
    return false;

  if (bed->want_plt_sym)
    {
      /* Define the symbol _PROCEDURE_LINKAGE_TABLE_ at the start of the
	 .plt section.  */
      struct elf_link_hash_entry *h = NULL;
      if (! (_bfd_generic_link_add_one_symbol
	     (info, abfd, "_PROCEDURE_LINKAGE_TABLE_", BSF_GLOBAL, s,
	      (bfd_vma) 0, (const char *) NULL, false,
	      get_elf_backend_data (abfd)->collect,
	      (struct bfd_link_hash_entry **) &h)))
	return false;
      h->elf_link_hash_flags |= ELF_LINK_HASH_DEF_REGULAR;
      h->type = STT_OBJECT;

      if (info->shared
	  && ! _bfd_elf_link_record_dynamic_symbol (info, h))
	return false;
    }

  s = bfd_make_section (abfd,
			bed->default_use_rela_p ? ".rela.plt" : ".rel.plt");
  if (s == NULL
      || ! bfd_set_section_flags (abfd, s, flags | SEC_READONLY)
      || ! bfd_set_section_alignment (abfd, s, ptralign))
    return false;

  if (! _bfd_elf_create_got_section (abfd, info))
    return false;

  {
    const char *secname;
    char *relname;
    flagword secflags;
    asection *sec;

    for (sec = abfd->sections; sec; sec = sec->next)
      {
	secflags = bfd_get_section_flags (abfd, sec);
	if ((secflags & (SEC_DATA | SEC_LINKER_CREATED))
	    || ((secflags & SEC_HAS_CONTENTS) != SEC_HAS_CONTENTS))
	  continue;
	secname = bfd_get_section_name (abfd, sec);
	relname = (char *) bfd_malloc (strlen (secname) + 6);
	strcpy (relname, ".rela");
	strcat (relname, secname);
	s = bfd_make_section (abfd, relname);
	if (s == NULL
	    || ! bfd_set_section_flags (abfd, s, flags | SEC_READONLY)
	    || ! bfd_set_section_alignment (abfd, s, ptralign))
	  return false;
      }
  }

  if (bed->want_dynbss)
    {
      /* The .dynbss section is a place to put symbols which are defined
	 by dynamic objects, are referenced by regular objects, and are
	 not functions.  We must allocate space for them in the process
	 image and use a R_*_COPY reloc to tell the dynamic linker to
	 initialize them at run time.  The linker script puts the .dynbss
	 section into the .bss section of the final image.  */
      s = bfd_make_section (abfd, ".dynbss");
      if (s == NULL
	  || ! bfd_set_section_flags (abfd, s, SEC_ALLOC))
	return false;

      /* The .rel[a].bss section holds copy relocs.  This section is not
	 normally needed.  We need to create it here, though, so that the
	 linker will map it to an output section.  We can't just create it
	 only if we need it, because we will not know whether we need it
	 until we have seen all the input files, and the first time the
	 main linker code calls BFD after examining all the input files
	 (size_dynamic_sections) the input sections have already been
	 mapped to the output sections.  If the section turns out not to
	 be needed, we can discard it later.  We will never need this
	 section when generating a shared object, since they do not use
	 copy relocs.  */
      if (! info->shared)
	{
	  s = bfd_make_section (abfd,
				(bed->default_use_rela_p
				 ? ".rela.bss" : ".rel.bss"));
	  if (s == NULL
	      || ! bfd_set_section_flags (abfd, s, flags | SEC_READONLY)
	      || ! bfd_set_section_alignment (abfd, s, ptralign))
	    return false;
	}
    }

  return true;
}

/* Adjust a symbol defined by a dynamic object and referenced by a
   regular object.  The current definition is in some section of the
   dynamic object, but we're not including those sections.  We have to
   change the definition to something the rest of the link can
   understand.  */

static boolean
sh_elf_adjust_dynamic_symbol (info, h)
     struct bfd_link_info *info;
     struct elf_link_hash_entry *h;
{
  bfd *dynobj;
  asection *s;
  unsigned int power_of_two;

  dynobj = elf_hash_table (info)->dynobj;

  /* Make sure we know what is going on here.  */
  BFD_ASSERT (dynobj != NULL
	      && ((h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_PLT)
		  || h->weakdef != NULL
		  || ((h->elf_link_hash_flags
		       & ELF_LINK_HASH_DEF_DYNAMIC) != 0
		      && (h->elf_link_hash_flags
			  & ELF_LINK_HASH_REF_REGULAR) != 0
		      && (h->elf_link_hash_flags
			  & ELF_LINK_HASH_DEF_REGULAR) == 0)));

  /* If this is a function, put it in the procedure linkage table.  We
     will fill in the contents of the procedure linkage table later,
     when we know the address of the .got section.  */
  if (h->type == STT_FUNC
      || (h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_PLT) != 0)
    {
      if (! info->shared
	  && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_DYNAMIC) == 0
	  && (h->elf_link_hash_flags & ELF_LINK_HASH_REF_DYNAMIC) == 0)
	{
	  /* This case can occur if we saw a PLT reloc in an input
	     file, but the symbol was never referred to by a dynamic
	     object.  In such a case, we don't actually need to build
	     a procedure linkage table, and we can just do a REL32
	     reloc instead.  */
	  BFD_ASSERT ((h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_PLT) != 0);
	  return true;
	}

      /* Make sure this symbol is output as a dynamic symbol.  */
      if (h->dynindx == -1)
	{
	  if (! bfd_elf32_link_record_dynamic_symbol (info, h))
	    return false;
	}

      s = bfd_get_section_by_name (dynobj, ".plt");
      BFD_ASSERT (s != NULL);

      /* If this is the first .plt entry, make room for the special
	 first entry.  */
      if (s->_raw_size == 0)
	s->_raw_size += PLT_ENTRY_SIZE;

      /* If this symbol is not defined in a regular file, and we are
	 not generating a shared library, then set the symbol to this
	 location in the .plt.  This is required to make function
	 pointers compare as equal between the normal executable and
	 the shared library.  */
      if (! info->shared
	  && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR) == 0)
	{
	  h->root.u.def.section = s;
	  h->root.u.def.value = s->_raw_size;
	}

      h->plt.offset = s->_raw_size;

      /* Make room for this entry.  */
      s->_raw_size += elf_sh_sizeof_plt (info);

      /* We also need to make an entry in the .got.plt section, which
	 will be placed in the .got section by the linker script.  */

      s = bfd_get_section_by_name (dynobj, ".got.plt");
      BFD_ASSERT (s != NULL);
      s->_raw_size += 4;

      /* We also need to make an entry in the .rela.plt section.  */

      s = bfd_get_section_by_name (dynobj, ".rela.plt");
      BFD_ASSERT (s != NULL);
      s->_raw_size += sizeof (Elf32_External_Rela);

      return true;
    }

  /* If this is a weak symbol, and there is a real definition, the
     processor independent code will have arranged for us to see the
     real definition first, and we can just use the same value.  */
  if (h->weakdef != NULL)
    {
      BFD_ASSERT (h->weakdef->root.type == bfd_link_hash_defined
		  || h->weakdef->root.type == bfd_link_hash_defweak);
      h->root.u.def.section = h->weakdef->root.u.def.section;
      h->root.u.def.value = h->weakdef->root.u.def.value;
      return true;
    }

  /* This is a reference to a symbol defined by a dynamic object which
     is not a function.  */

  /* If we are creating a shared library, we must presume that the
     only references to the symbol are via the global offset table.
     For such cases we need not do anything here; the relocations will
     be handled correctly by relocate_section.  */
  if (info->shared)
    return true;

  /* If there are no references to this symbol that do not use the
     GOT, we don't need to generate a copy reloc.  */
  if ((h->elf_link_hash_flags & ELF_LINK_NON_GOT_REF) == 0)
    return true;

  /* We must allocate the symbol in our .dynbss section, which will
     become part of the .bss section of the executable.  There will be
     an entry for this symbol in the .dynsym section.  The dynamic
     object will contain position independent code, so all references
     from the dynamic object to this symbol will go through the global
     offset table.  The dynamic linker will use the .dynsym entry to
     determine the address it must put in the global offset table, so
     both the dynamic object and the regular object will refer to the
     same memory location for the variable.  */

  s = bfd_get_section_by_name (dynobj, ".dynbss");
  BFD_ASSERT (s != NULL);

  /* We must generate a R_SH_COPY reloc to tell the dynamic linker to
     copy the initial value out of the dynamic object and into the
     runtime process image.  We need to remember the offset into the
     .rela.bss section we are going to use.  */
  if ((h->root.u.def.section->flags & SEC_ALLOC) != 0)
    {
      asection *srel;

      srel = bfd_get_section_by_name (dynobj, ".rela.bss");
      BFD_ASSERT (srel != NULL);
      srel->_raw_size += sizeof (Elf32_External_Rela);
      h->elf_link_hash_flags |= ELF_LINK_HASH_NEEDS_COPY;
    }

  /* We need to figure out the alignment required for this symbol.  I
     have no idea how ELF linkers handle this.  */
  power_of_two = bfd_log2 (h->size);
  if (power_of_two > 3)
    power_of_two = 3;

  /* Apply the required alignment.  */
  s->_raw_size = BFD_ALIGN (s->_raw_size,
			    (bfd_size_type) (1 << power_of_two));
  if (power_of_two > bfd_get_section_alignment (dynobj, s))
    {
      if (! bfd_set_section_alignment (dynobj, s, power_of_two))
	return false;
    }

  /* Define the symbol as being at this point in the section.  */
  h->root.u.def.section = s;
  h->root.u.def.value = s->_raw_size;

  /* Increment the section size to make room for the symbol.  */
  s->_raw_size += h->size;

  return true;
}

/* Set the sizes of the dynamic sections.  */

static boolean
sh_elf_size_dynamic_sections (output_bfd, info)
     bfd *output_bfd;
     struct bfd_link_info *info;
{
  bfd *dynobj;
  asection *s;
  boolean plt;
  boolean relocs;
  boolean reltext;

  dynobj = elf_hash_table (info)->dynobj;
  BFD_ASSERT (dynobj != NULL);

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      /* Set the contents of the .interp section to the interpreter.  */
      if (! info->shared)
	{
	  s = bfd_get_section_by_name (dynobj, ".interp");
	  BFD_ASSERT (s != NULL);
	  s->_raw_size = sizeof ELF_DYNAMIC_INTERPRETER;
	  s->contents = (unsigned char *) ELF_DYNAMIC_INTERPRETER;
	}
    }
  else
    {
      /* We may have created entries in the .rela.got section.
	 However, if we are not creating the dynamic sections, we will
	 not actually use these entries.  Reset the size of .rela.got,
	 which will cause it to get stripped from the output file
	 below.  */
      s = bfd_get_section_by_name (dynobj, ".rela.got");
      if (s != NULL)
	s->_raw_size = 0;
    }

  /* If this is a -Bsymbolic shared link, then we need to discard all
     PC relative relocs against symbols defined in a regular object.
     We allocated space for them in the check_relocs routine, but we
     will not fill them in in the relocate_section routine.  */
  if (info->shared && info->symbolic)
    sh_elf_link_hash_traverse (sh_elf_hash_table (info),
				 sh_elf_discard_copies,
				 (PTR) NULL);

  /* The check_relocs and adjust_dynamic_symbol entry points have
     determined the sizes of the various dynamic sections.  Allocate
     memory for them.  */
  plt = false;
  relocs = false;
  reltext = false;
  for (s = dynobj->sections; s != NULL; s = s->next)
    {
      const char *name;
      boolean strip;

      if ((s->flags & SEC_LINKER_CREATED) == 0)
	continue;

      /* It's OK to base decisions on the section name, because none
	 of the dynobj section names depend upon the input files.  */
      name = bfd_get_section_name (dynobj, s);

      strip = false;

      if (strcmp (name, ".plt") == 0)
	{
	  if (s->_raw_size == 0)
	    {
	      /* Strip this section if we don't need it; see the
		 comment below.  */
	      strip = true;
	    }
	  else
	    {
	      /* Remember whether there is a PLT.  */
	      plt = true;
	    }
	}
      else if (strncmp (name, ".rela", 5) == 0)
	{
	  if (s->_raw_size == 0)
	    {
	      /* If we don't need this section, strip it from the
		 output file.  This is mostly to handle .rela.bss and
		 .rela.plt.  We must create both sections in
		 create_dynamic_sections, because they must be created
		 before the linker maps input sections to output
		 sections.  The linker does that before
		 adjust_dynamic_symbol is called, and it is that
		 function which decides whether anything needs to go
		 into these sections.  */
	      strip = true;
	    }
	  else
	    {
	      asection *target;

	      /* Remember whether there are any reloc sections other
		 than .rela.plt.  */
	      if (strcmp (name, ".rela.plt") != 0)
		{
		  const char *outname;

		  relocs = true;

		  /* If this relocation section applies to a read only
		     section, then we probably need a DT_TEXTREL
		     entry.  The entries in the .rela.plt section
		     really apply to the .got section, which we
		     created ourselves and so know is not readonly.  */
		  outname = bfd_get_section_name (output_bfd,
						  s->output_section);
		  target = bfd_get_section_by_name (output_bfd, outname + 5);
		  if (target != NULL
		      && (target->flags & SEC_READONLY) != 0
		      && (target->flags & SEC_ALLOC) != 0)
		    reltext = true;
		}

	      /* We use the reloc_count field as a counter if we need
		 to copy relocs into the output file.  */
	      s->reloc_count = 0;
	    }
	}
      else if (strncmp (name, ".got", 4) != 0)
	{
	  /* It's not one of our sections, so don't allocate space.  */
	  continue;
	}

      if (strip)
	{
	  _bfd_strip_section_from_output (info, s);
	  continue;
	}

      /* Allocate memory for the section contents.  */
      s->contents = (bfd_byte *) bfd_alloc (dynobj, s->_raw_size);
      if (s->contents == NULL && s->_raw_size != 0)
	return false;
    }

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      /* Add some entries to the .dynamic section.  We fill in the
	 values later, in sh_elf_finish_dynamic_sections, but we
	 must add the entries now so that we get the correct size for
	 the .dynamic section.  The DT_DEBUG entry is filled in by the
	 dynamic linker and used by the debugger.  */
      if (! info->shared)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_DEBUG, 0))
	    return false;
	}

      if (plt)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_PLTGOT, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_PLTRELSZ, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_PLTREL, DT_RELA)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_JMPREL, 0))
	    return false;
	}

      if (relocs)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_RELA, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_RELASZ, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_RELAENT,
						sizeof (Elf32_External_Rela)))
	    return false;
	}

      if (reltext)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_TEXTREL, 0))
	    return false;
	}
    }

  return true;
}

/* This function is called via sh_elf_link_hash_traverse if we are
   creating a shared object with -Bsymbolic.  It discards the space
   allocated to copy PC relative relocs against symbols which are
   defined in regular objects.  We allocated space for them in the
   check_relocs routine, but we won't fill them in in the
   relocate_section routine.  */

static boolean
sh_elf_discard_copies (h, ignore)
     struct elf_sh_link_hash_entry *h;
     PTR ignore ATTRIBUTE_UNUSED;
{
  struct elf_sh_pcrel_relocs_copied *s;

  /* We only discard relocs for symbols defined in a regular object.  */
  if ((h->root.elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR) == 0)
    return true;

  for (s = h->pcrel_relocs_copied; s != NULL; s = s->next)
    s->section->_raw_size -= s->count * sizeof (Elf32_External_Rela);

  return true;
}

/* Relocate an SH ELF section.  */

static boolean
sh_elf_relocate_section (output_bfd, info, input_bfd, input_section,
			 contents, relocs, local_syms, local_sections)
     bfd *output_bfd ATTRIBUTE_UNUSED;
     struct bfd_link_info *info;
     bfd *input_bfd;
     asection *input_section;
     bfd_byte *contents;
     Elf_Internal_Rela *relocs;
     Elf_Internal_Sym *local_syms;
     asection **local_sections;
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  Elf_Internal_Rela *rel, *relend;
  bfd *dynobj;
  bfd_vma *local_got_offsets;
  asection *sgot;
  asection *splt;
  asection *sreloc;

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);
  dynobj = elf_hash_table (info)->dynobj;
  local_got_offsets = elf_local_got_offsets (input_bfd);

  sgot = NULL;
  splt = NULL;
  sreloc = NULL;

  rel = relocs;
  relend = relocs + input_section->reloc_count;
  for (; rel < relend; rel++)
    {
      int r_type;
      reloc_howto_type *howto;
      unsigned long r_symndx;
      Elf_Internal_Sym *sym;
      asection *sec;
      struct elf_link_hash_entry *h;
      bfd_vma relocation;
      bfd_vma addend = (bfd_vma) 0;
      bfd_reloc_status_type r;

      r_symndx = ELF32_R_SYM (rel->r_info);

      r_type = ELF32_R_TYPE (rel->r_info);

      /* Many of the relocs are only used for relaxing, and are
         handled entirely by the relaxation code.  */
      if (r_type > (int) R_SH_LAST_INVALID_RELOC
	  && r_type < (int) R_SH_LOOP_START)
	continue;
      if (r_type == (int) R_SH_NONE)
	continue;

      if (r_type < 0
	  || r_type >= R_SH_max
	  || (r_type >= (int) R_SH_FIRST_INVALID_RELOC
	      && r_type <= (int) R_SH_LAST_INVALID_RELOC)
	  || (r_type >= (int) R_SH_FIRST_INVALID_RELOC_2
	      && r_type <= (int) R_SH_LAST_INVALID_RELOC_2))
	{
	  bfd_set_error (bfd_error_bad_value);
	  return false;
	}

      howto = sh_elf_howto_table + r_type;

      /* This is a final link.  */
      h = NULL;
      sym = NULL;
      sec = NULL;
      if (r_symndx < symtab_hdr->sh_info)
	{
	  sym = local_syms + r_symndx;
	  sec = local_sections[r_symndx];
	  relocation = (sec->output_section->vma
			+ sec->output_offset
			+ sym->st_value);

	  if (info->relocateable)
	    {
	      /* This is a relocateable link.  We don't have to change
		 anything, unless the reloc is against a section symbol,
		 in which case we have to adjust according to where the
		 section symbol winds up in the output section.  */
	      sym = local_syms + r_symndx;
	      if (ELF_ST_TYPE (sym->st_info) == STT_SECTION)
		goto final_link_relocate;

	      continue;
	    }
	}
      else
	{
	  /* Section symbol are never (?) placed in the hash table, so
	     we can just ignore hash relocations when creating a
	     relocateable object file.  */
	  if (info->relocateable)
	    continue;

	  h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	  while (h->root.type == bfd_link_hash_indirect
		 || h->root.type == bfd_link_hash_warning)
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;
	  if (h->root.type == bfd_link_hash_defined
	      || h->root.type == bfd_link_hash_defweak)
	    {
	      sec = h->root.u.def.section;
	      /* In these cases, we don't need the relocation value.
		 We check specially because in some obscure cases
		 sec->output_section will be NULL.  */
	      if (r_type == R_SH_GOTPC
		  || (r_type == R_SH_PLT32
		      && h->plt.offset != (bfd_vma) -1)
		  || (r_type == R_SH_GOT32
		      && elf_hash_table (info)->dynamic_sections_created
		      && (! info->shared
			  || (! info->symbolic && h->dynindx != -1)
			  || (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) == 0))
		  /* The cases above are those in which relocation is
		     overwritten in the switch block below.  The cases
		     below are those in which we must defer relocation
		     to run-time, because we can't resolve absolute
		     addresses when creating a shared library.  */
		  || (info->shared
		      && ((! info->symbolic && h->dynindx != -1)
			  || (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) == 0)
		      && ((r_type == R_SH_DIR32
			   && !(ELF_ST_VISIBILITY (h->other) == STV_INTERNAL
				|| ELF_ST_VISIBILITY (h->other) == STV_HIDDEN))
			  || r_type == R_SH_REL32)
		      && ((input_section->flags & SEC_ALLOC) != 0
			  /* DWARF will emit R_SH_DIR32 relocations in its
			     sections against symbols defined externally
			     in shared libraries.  We can't do anything
			     with them here.  */
			  || (input_section->flags & SEC_DEBUGGING) != 0)))
		relocation = 0;
	      else if (sec->output_section == NULL)
		{
		  (*_bfd_error_handler)
		    (_("%s: warning: unresolvable relocation against symbol `%s' from %s section"),
		     bfd_get_filename (input_bfd), h->root.root.string,
		     bfd_get_section_name (input_bfd, input_section));
		  relocation = 0;
		}
	      else
		relocation = (h->root.u.def.value
			      + sec->output_section->vma
			      + sec->output_offset);
	    }
	  else if (h->root.type == bfd_link_hash_undefweak)
	    relocation = 0;
	  else if (info->shared && !info->symbolic && !info->no_undefined)
	    relocation = 0;
	  else
	    {
	      if (! ((*info->callbacks->undefined_symbol)
		     (info, h->root.root.string, input_bfd,
		      input_section, rel->r_offset, true)))
		return false;
	      relocation = 0;
	    }
	}

      switch ((int) r_type)
	{
	final_link_relocate:
	  /* COFF relocs don't use the addend. The addend is used for
	     R_SH_DIR32 to be compatible with other compilers.  */
	  r = _bfd_final_link_relocate (howto, input_bfd, input_section,
					contents, rel->r_offset,
					relocation, addend);
	  break;

	case R_SH_IND12W:
	  relocation -= 4;
	  goto final_link_relocate;

	case R_SH_DIR8WPN:
	case R_SH_DIR8WPZ:
	case R_SH_DIR8WPL:
	  /* If the reloc is against the start of this section, then
	     the assembler has already taken care of it and the reloc
	     is here only to assist in relaxing.  If the reloc is not
	     against the start of this section, then it's against an
	     external symbol and we must deal with it ourselves.  */
	  if (input_section->output_section->vma + input_section->output_offset
	      != relocation)
	    {
	      int disp = (relocation
			  - input_section->output_section->vma
			  - input_section->output_offset
			  - rel->r_offset);
	      int mask = 0;
	      switch (r_type)
		{
		case R_SH_DIR8WPN:
		case R_SH_DIR8WPZ: mask = 1; break;
		case R_SH_DIR8WPL: mask = 3; break;
		default: mask = 0; break;
		}
	      if (disp & mask)
		{
		  ((*_bfd_error_handler)
		   (_("%s: 0x%lx: fatal: unaligned branch target for relax-support relocation"),
		    bfd_get_filename (input_section->owner),
		    (unsigned long) rel->r_offset));
		  bfd_set_error (bfd_error_bad_value);
		  return false;
		}
	      relocation -= 4;
	      goto final_link_relocate;
	    }
	  r = bfd_reloc_ok;
	  break;

	default:
	  bfd_set_error (bfd_error_bad_value);
	  return false;

	case R_SH_DIR32:
	case R_SH_REL32:
	  if (info->shared
	      && (input_section->flags & SEC_ALLOC) != 0
	      && (r_type != R_SH_REL32
		  || (h != NULL
		      && h->dynindx != -1
		      && (! info->symbolic
			  || (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) == 0))))
	    {
	      Elf_Internal_Rela outrel;
	      boolean skip, relocate;

	      /* When generating a shared object, these relocations
		 are copied into the output file to be resolved at run
		 time.  */

	      if (sreloc == NULL)
		{
		  const char *name;

		  name = (bfd_elf_string_from_elf_section
			  (input_bfd,
			   elf_elfheader (input_bfd)->e_shstrndx,
			   elf_section_data (input_section)->rel_hdr.sh_name));
		  if (name == NULL)
		    return false;

		  BFD_ASSERT (strncmp (name, ".rela", 5) == 0
			      && strcmp (bfd_get_section_name (input_bfd,
							       input_section),
					 name + 5) == 0);

		  sreloc = bfd_get_section_by_name (dynobj, name);
		  BFD_ASSERT (sreloc != NULL);
		}

	      skip = false;

	      if (elf_section_data (input_section)->stab_info == NULL)
		outrel.r_offset = rel->r_offset;
	      else
		{
		  bfd_vma off;

		  off = (_bfd_stab_section_offset
			 (output_bfd, &elf_hash_table (info)->stab_info,
			  input_section,
			  &elf_section_data (input_section)->stab_info,
			  rel->r_offset));
		  if (off == (bfd_vma) -1)
		    skip = true;
		  outrel.r_offset = off;
		}

	      outrel.r_offset += (input_section->output_section->vma
				  + input_section->output_offset);

	      if (skip)
		{
		  memset (&outrel, 0, sizeof outrel);
		  relocate = false;
		}
	      else if (r_type == R_SH_REL32)
		{
		  BFD_ASSERT (h != NULL && h->dynindx != -1);
		  relocate = false;
		  outrel.r_info = ELF32_R_INFO (h->dynindx, R_SH_REL32);
		  outrel.r_addend = rel->r_addend;
		}
	      else
		{
		  /* h->dynindx may be -1 if this symbol was marked to
		     become local.  */
		  if (h == NULL
		      || ((info->symbolic || h->dynindx == -1)
			  && (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) != 0))
		    {
		      relocate = true;
		      outrel.r_info = ELF32_R_INFO (0, R_SH_RELATIVE);
		      outrel.r_addend = relocation + rel->r_addend;
		    }
		  else
		    {
		      BFD_ASSERT (h->dynindx != -1);
		      relocate = false;
		      outrel.r_info = ELF32_R_INFO (h->dynindx, R_SH_DIR32);
		      outrel.r_addend = relocation + rel->r_addend;
		    }
		}

	      bfd_elf32_swap_reloca_out (output_bfd, &outrel,
					 (((Elf32_External_Rela *)
					   sreloc->contents)
					  + sreloc->reloc_count));
	      ++sreloc->reloc_count;

	      /* If this reloc is against an external symbol, we do
		 not want to fiddle with the addend.  Otherwise, we
		 need to include the symbol value so that it becomes
		 an addend for the dynamic reloc.  */
	      if (! relocate)
		continue;
	    }
	  else if (r_type == R_SH_DIR32)
	    addend = rel->r_addend;
	  goto final_link_relocate;

	case R_SH_GOT32:
	  /* Relocation is to the entry for this symbol in the global
	     offset table.  */
	  if (sgot == NULL)
	    {
	      sgot = bfd_get_section_by_name (dynobj, ".got");
	      BFD_ASSERT (sgot != NULL);
	    }

	  if (h != NULL)
	    {
	      bfd_vma off;

	      off = h->got.offset;
	      BFD_ASSERT (off != (bfd_vma) -1);

	      if (! elf_hash_table (info)->dynamic_sections_created
		  || (info->shared
		      && (info->symbolic || h->dynindx == -1
			  || ELF_ST_VISIBILITY (h->other) == STV_INTERNAL
			  || ELF_ST_VISIBILITY (h->other) == STV_HIDDEN)
		      && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR)))
		{
		  /* This is actually a static link, or it is a
		     -Bsymbolic link and the symbol is defined
		     locally, or the symbol was forced to be local
		     because of a version file.  We must initialize
		     this entry in the global offset table.  Since the
		     offset must always be a multiple of 4, we use the
		     least significant bit to record whether we have
		     initialized it already.

		     When doing a dynamic link, we create a .rela.got
		     relocation entry to initialize the value.  This
		     is done in the finish_dynamic_symbol routine.  */
		  if ((off & 1) != 0)
		    off &= ~1;
		  else
		    {
		      bfd_put_32 (output_bfd, relocation,
				  sgot->contents + off);
		      h->got.offset |= 1;
		    }
		}

	      relocation = sgot->output_offset + off;
	    }
	  else
	    {
	      bfd_vma off;

	      BFD_ASSERT (local_got_offsets != NULL
			  && local_got_offsets[r_symndx] != (bfd_vma) -1);

	      off = local_got_offsets[r_symndx];

	      /* The offset must always be a multiple of 4.  We use
		 the least significant bit to record whether we have
		 already generated the necessary reloc.  */
	      if ((off & 1) != 0)
		off &= ~1;
	      else
		{
		  bfd_put_32 (output_bfd, relocation, sgot->contents + off);

		  if (info->shared)
		    {
		      asection *srelgot;
		      Elf_Internal_Rela outrel;

		      srelgot = bfd_get_section_by_name (dynobj, ".rela.got");
		      BFD_ASSERT (srelgot != NULL);

		      outrel.r_offset = (sgot->output_section->vma
					 + sgot->output_offset
					 + off);
		      outrel.r_info = ELF32_R_INFO (0, R_SH_RELATIVE);
		      outrel.r_addend = relocation;
		      bfd_elf32_swap_reloca_out (output_bfd, &outrel,
						(((Elf32_External_Rela *)
						  srelgot->contents)
						 + srelgot->reloc_count));
		      ++srelgot->reloc_count;
		    }

		  local_got_offsets[r_symndx] |= 1;
		}

	      relocation = sgot->output_offset + off;
	    }

	  goto final_link_relocate;

	case R_SH_GOTOFF:
	  /* Relocation is relative to the start of the global offset
	     table.  */

	  if (sgot == NULL)
	    {
	      sgot = bfd_get_section_by_name (dynobj, ".got");
	      BFD_ASSERT (sgot != NULL);
	    }

	  /* Note that sgot->output_offset is not involved in this
	     calculation.  We always want the start of .got.  If we
	     defined _GLOBAL_OFFSET_TABLE in a different way, as is
	     permitted by the ABI, we might have to change this
	     calculation.  */
	  relocation -= sgot->output_section->vma;

	  goto final_link_relocate;

	case R_SH_GOTPC:
	  /* Use global offset table as symbol value.  */

	  if (sgot == NULL)
	    {
	      sgot = bfd_get_section_by_name (dynobj, ".got");
	      BFD_ASSERT (sgot != NULL);
	    }

	  relocation = sgot->output_section->vma;

	  goto final_link_relocate;

	case R_SH_PLT32:
	  /* Relocation is to the entry for this symbol in the
	     procedure linkage table.  */

	  /* Resolve a PLT reloc against a local symbol directly,
	     without using the procedure linkage table.  */
	  if (h == NULL)
	    goto final_link_relocate;

	  if (ELF_ST_VISIBILITY (h->other) == STV_INTERNAL
	      || ELF_ST_VISIBILITY (h->other) == STV_HIDDEN)
	    goto final_link_relocate;

	  if (h->plt.offset == (bfd_vma) -1)
	    {
	      /* We didn't make a PLT entry for this symbol.  This
		 happens when statically linking PIC code, or when
		 using -Bsymbolic.  */
	      goto final_link_relocate;
	    }

	  if (splt == NULL)
	    {
	      splt = bfd_get_section_by_name (dynobj, ".plt");
	      BFD_ASSERT (splt != NULL);
	    }

	  relocation = (splt->output_section->vma
			+ splt->output_offset
			+ h->plt.offset);

	  goto final_link_relocate;

	case R_SH_LOOP_START:
	  {
	    static bfd_vma start, end;

	    start = (relocation + rel->r_addend
		     - (sec->output_section->vma + sec->output_offset));
	    r = sh_elf_reloc_loop (r_type, input_bfd, input_section, contents,
				   rel->r_offset, sec, start, end);
	    break;

	case R_SH_LOOP_END:
	    end = (relocation + rel->r_addend
		   - (sec->output_section->vma + sec->output_offset));
	    r = sh_elf_reloc_loop (r_type, input_bfd, input_section, contents,
				   rel->r_offset, sec, start, end);
	    break;
	  }
	}

      if (r != bfd_reloc_ok)
	{
	  switch (r)
	    {
	    default:
	    case bfd_reloc_outofrange:
	      abort ();
	    case bfd_reloc_overflow:
	      {
		const char *name;

		if (h != NULL)
		  name = h->root.root.string;
		else
		  {
		    name = (bfd_elf_string_from_elf_section
			    (input_bfd, symtab_hdr->sh_link, sym->st_name));
		    if (name == NULL)
		      return false;
		    if (*name == '\0')
		      name = bfd_section_name (input_bfd, sec);
		  }
		if (! ((*info->callbacks->reloc_overflow)
		       (info, name, howto->name, (bfd_vma) 0,
			input_bfd, input_section, rel->r_offset)))
		  return false;
	      }
	      break;
	    }
	}
    }

  return true;
}

/* This is a version of bfd_generic_get_relocated_section_contents
   which uses sh_elf_relocate_section.  */

static bfd_byte *
sh_elf_get_relocated_section_contents (output_bfd, link_info, link_order,
				       data, relocateable, symbols)
     bfd *output_bfd;
     struct bfd_link_info *link_info;
     struct bfd_link_order *link_order;
     bfd_byte *data;
     boolean relocateable;
     asymbol **symbols;
{
  Elf_Internal_Shdr *symtab_hdr;
  asection *input_section = link_order->u.indirect.section;
  bfd *input_bfd = input_section->owner;
  asection **sections = NULL;
  Elf_Internal_Rela *internal_relocs = NULL;
  Elf32_External_Sym *external_syms = NULL;
  Elf_Internal_Sym *internal_syms = NULL;

  /* We only need to handle the case of relaxing, or of having a
     particular set of section contents, specially.  */
  if (relocateable
      || elf_section_data (input_section)->this_hdr.contents == NULL)
    return bfd_generic_get_relocated_section_contents (output_bfd, link_info,
						       link_order, data,
						       relocateable,
						       symbols);

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;

  memcpy (data, elf_section_data (input_section)->this_hdr.contents,
	  input_section->_raw_size);

  if ((input_section->flags & SEC_RELOC) != 0
      && input_section->reloc_count > 0)
    {
      Elf_Internal_Sym *isymp;
      asection **secpp;
      Elf32_External_Sym *esym, *esymend;

      if (symtab_hdr->contents != NULL)
	external_syms = (Elf32_External_Sym *) symtab_hdr->contents;
      else
	{
	  external_syms = ((Elf32_External_Sym *)
			   bfd_malloc (symtab_hdr->sh_info
				       * sizeof (Elf32_External_Sym)));
	  if (external_syms == NULL && symtab_hdr->sh_info > 0)
	    goto error_return;
	  if (bfd_seek (input_bfd, symtab_hdr->sh_offset, SEEK_SET) != 0
	      || (bfd_read (external_syms, sizeof (Elf32_External_Sym),
			    symtab_hdr->sh_info, input_bfd)
		  != (symtab_hdr->sh_info * sizeof (Elf32_External_Sym))))
	    goto error_return;
	}

      internal_relocs = (_bfd_elf32_link_read_relocs
			 (input_bfd, input_section, (PTR) NULL,
			  (Elf_Internal_Rela *) NULL, false));
      if (internal_relocs == NULL)
	goto error_return;

      internal_syms = ((Elf_Internal_Sym *)
		       bfd_malloc (symtab_hdr->sh_info
				   * sizeof (Elf_Internal_Sym)));
      if (internal_syms == NULL && symtab_hdr->sh_info > 0)
	goto error_return;

      sections = (asection **) bfd_malloc (symtab_hdr->sh_info
					   * sizeof (asection *));
      if (sections == NULL && symtab_hdr->sh_info > 0)
	goto error_return;

      isymp = internal_syms;
      secpp = sections;
      esym = external_syms;
      esymend = esym + symtab_hdr->sh_info;
      for (; esym < esymend; ++esym, ++isymp, ++secpp)
	{
	  asection *isec;

	  bfd_elf32_swap_symbol_in (input_bfd, esym, isymp);

	  if (isymp->st_shndx == SHN_UNDEF)
	    isec = bfd_und_section_ptr;
	  else if (isymp->st_shndx > 0 && isymp->st_shndx < SHN_LORESERVE)
	    isec = bfd_section_from_elf_index (input_bfd, isymp->st_shndx);
	  else if (isymp->st_shndx == SHN_ABS)
	    isec = bfd_abs_section_ptr;
	  else if (isymp->st_shndx == SHN_COMMON)
	    isec = bfd_com_section_ptr;
	  else
	    {
	      /* Who knows?  */
	      isec = NULL;
	    }

	  *secpp = isec;
	}

      if (! sh_elf_relocate_section (output_bfd, link_info, input_bfd,
				     input_section, data, internal_relocs,
				     internal_syms, sections))
	goto error_return;

      if (sections != NULL)
	free (sections);
      sections = NULL;
      if (internal_syms != NULL)
	free (internal_syms);
      internal_syms = NULL;
      if (external_syms != NULL && symtab_hdr->contents == NULL)
	free (external_syms);
      external_syms = NULL;
      if (internal_relocs != elf_section_data (input_section)->relocs)
	free (internal_relocs);
      internal_relocs = NULL;
    }

  return data;

 error_return:
  if (internal_relocs != NULL
      && internal_relocs != elf_section_data (input_section)->relocs)
    free (internal_relocs);
  if (external_syms != NULL && symtab_hdr->contents == NULL)
    free (external_syms);
  if (internal_syms != NULL)
    free (internal_syms);
  if (sections != NULL)
    free (sections);
  return NULL;
}
static asection *
sh_elf_gc_mark_hook (abfd, info, rel, h, sym)
     bfd *abfd;
     struct bfd_link_info *info ATTRIBUTE_UNUSED;
     Elf_Internal_Rela *rel;
     struct elf_link_hash_entry *h;
     Elf_Internal_Sym *sym;
{
  if (h != NULL)
    {
      switch (ELF32_R_TYPE (rel->r_info))
	{
	case R_SH_GNU_VTINHERIT:
	case R_SH_GNU_VTENTRY:
	  break;

	default:
	  switch (h->root.type)
	    {
	    case bfd_link_hash_defined:
	    case bfd_link_hash_defweak:
	      return h->root.u.def.section;

	    case bfd_link_hash_common:
	      return h->root.u.c.p->section;

	    default:
	      break;
	    }
	}
    }
  else
    {
      if (!(elf_bad_symtab (abfd)
	    && ELF_ST_BIND (sym->st_info) != STB_LOCAL)
	  && ! ((sym->st_shndx <= 0 || sym->st_shndx >= SHN_LORESERVE)
                && sym->st_shndx != SHN_COMMON))
	return bfd_section_from_elf_index (abfd, sym->st_shndx);
    }
  return NULL;
}

/* Update the got entry reference counts for the section being removed.  */

static boolean
sh_elf_gc_sweep_hook (abfd, info, sec, relocs)
     bfd *abfd ATTRIBUTE_UNUSED;
     struct bfd_link_info *info ATTRIBUTE_UNUSED;
     asection *sec ATTRIBUTE_UNUSED;
     const Elf_Internal_Rela *relocs ATTRIBUTE_UNUSED;
{
  /* We use got and plt entries for sh, but it would seem that the
     existing SH code does no sort of reference counting or whatnot on
     its GOT and PLT entries, so it is not possible to garbage collect
     them at this time.  */
  return true;
}

/* Look through the relocs for a section during the first phase.
   Since we don't do .gots or .plts, we just need to consider the
   virtual table relocs for gc.  */

static boolean
sh_elf_check_relocs (abfd, info, sec, relocs)
     bfd *abfd;
     struct bfd_link_info *info;
     asection *sec;
     const Elf_Internal_Rela *relocs;
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes, **sym_hashes_end;
  const Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *rel_end;
  bfd *dynobj;
  bfd_vma *local_got_offsets;
  asection *sgot;
  asection *srelgot;
  asection *sreloc;

  sgot = NULL;
  srelgot = NULL;
  sreloc = NULL;

  if (info->relocateable)
    return true;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  sym_hashes_end = sym_hashes + symtab_hdr->sh_size/sizeof (Elf32_External_Sym);
  if (!elf_bad_symtab (abfd))
    sym_hashes_end -= symtab_hdr->sh_info;

  dynobj = elf_hash_table (info)->dynobj;
  local_got_offsets = elf_local_got_offsets (abfd);

  rel_end = relocs + sec->reloc_count;
  for (rel = relocs; rel < rel_end; rel++)
    {
      struct elf_link_hash_entry *h;
      unsigned long r_symndx;

      r_symndx = ELF32_R_SYM (rel->r_info);
      if (r_symndx < symtab_hdr->sh_info)
	h = NULL;
      else
	h = sym_hashes[r_symndx - symtab_hdr->sh_info];

      /* Some relocs require a global offset table.  */
      if (dynobj == NULL)
	{
	  switch (ELF32_R_TYPE (rel->r_info))
	    {
	    case R_SH_GOT32:
	    case R_SH_GOTOFF:
	    case R_SH_GOTPC:
	      elf_hash_table (info)->dynobj = dynobj = abfd;
	      if (! _bfd_elf_create_got_section (dynobj, info))
		return false;
	      break;

	    default:
	      break;
	    }
	}

      switch (ELF32_R_TYPE (rel->r_info))
        {
        /* This relocation describes the C++ object vtable hierarchy.
           Reconstruct it for later use during GC.  */
	case R_SH_GNU_VTINHERIT:
	  if (!_bfd_elf32_gc_record_vtinherit (abfd, sec, h, rel->r_offset))
	    return false;
	  break;

        /* This relocation describes which C++ vtable entries are actually
           used.  Record for later use during GC.  */
	case R_SH_GNU_VTENTRY:
	  if (!_bfd_elf32_gc_record_vtentry (abfd, sec, h, rel->r_addend))
	    return false;
	  break;

	case R_SH_GOT32:
	  /* This symbol requires a global offset table entry.  */

	  if (sgot == NULL)
	    {
	      sgot = bfd_get_section_by_name (dynobj, ".got");
	      BFD_ASSERT (sgot != NULL);
	    }

	  if (srelgot == NULL
	      && (h != NULL || info->shared))
	    {
	      srelgot = bfd_get_section_by_name (dynobj, ".rela.got");
	      if (srelgot == NULL)
		{
		  srelgot = bfd_make_section (dynobj, ".rela.got");
		  if (srelgot == NULL
		      || ! bfd_set_section_flags (dynobj, srelgot,
						  (SEC_ALLOC
						   | SEC_LOAD
						   | SEC_HAS_CONTENTS
						   | SEC_IN_MEMORY
						   | SEC_LINKER_CREATED
						   | SEC_READONLY))
		      || ! bfd_set_section_alignment (dynobj, srelgot, 2))
		    return false;
		}
	    }

	  if (h != NULL)
	    {
	      if (h->got.offset != (bfd_vma) -1)
		{
		  /* We have already allocated space in the .got.  */
		  break;
		}
	      h->got.offset = sgot->_raw_size;

	      /* Make sure this symbol is output as a dynamic symbol.  */
	      if (h->dynindx == -1)
		{
		  if (! bfd_elf32_link_record_dynamic_symbol (info, h))
		    return false;
		}

	      srelgot->_raw_size += sizeof (Elf32_External_Rela);
	    }
	  else
	    {
	      /* This is a global offset table entry for a local
	         symbol.  */
	      if (local_got_offsets == NULL)
		{
		  size_t size;
		  register unsigned int i;

		  size = symtab_hdr->sh_info * sizeof (bfd_vma);
		  local_got_offsets = (bfd_vma *) bfd_alloc (abfd, size);
		  if (local_got_offsets == NULL)
		    return false;
		  elf_local_got_offsets (abfd) = local_got_offsets;
		  for (i = 0; i < symtab_hdr->sh_info; i++)
		    local_got_offsets[i] = (bfd_vma) -1;
		}
	      if (local_got_offsets[r_symndx] != (bfd_vma) -1)
		{
		  /* We have already allocated space in the .got.  */
		  break;
		}
	      local_got_offsets[r_symndx] = sgot->_raw_size;

	      if (info->shared)
		{
		  /* If we are generating a shared object, we need to
		     output a R_SH_RELATIVE reloc so that the dynamic
		     linker can adjust this GOT entry.  */
		  srelgot->_raw_size += sizeof (Elf32_External_Rela);
		}
	    }

	  sgot->_raw_size += 4;

	  break;

	case R_SH_PLT32:
	  /* This symbol requires a procedure linkage table entry.  We
	     actually build the entry in adjust_dynamic_symbol,
	     because this might be a case of linking PIC code which is
	     never referenced by a dynamic object, in which case we
	     don't need to generate a procedure linkage table entry
	     after all.  */

	  /* If this is a local symbol, we resolve it directly without
	     creating a procedure linkage table entry.  */
	  if (h == NULL)
	    continue;

	  if (ELF_ST_VISIBILITY (h->other) == STV_INTERNAL
	      || ELF_ST_VISIBILITY (h->other) == STV_HIDDEN)
	    break;

	  h->elf_link_hash_flags |= ELF_LINK_HASH_NEEDS_PLT;

	  break;

	case R_SH_DIR32:
	case R_SH_REL32:
	  if (h != NULL)
	    h->elf_link_hash_flags |= ELF_LINK_NON_GOT_REF;

	  /* If we are creating a shared library, and this is a reloc
	     against a global symbol, or a non PC relative reloc
	     against a local symbol, then we need to copy the reloc
	     into the shared library.  However, if we are linking with
	     -Bsymbolic, we do not need to copy a reloc against a
	     global symbol which is defined in an object we are
	     including in the link (i.e., DEF_REGULAR is set).  At
	     this point we have not seen all the input files, so it is
	     possible that DEF_REGULAR is not set now but will be set
	     later (it is never cleared).  We account for that
	     possibility below by storing information in the
	     pcrel_relocs_copied field of the hash table entry.  */
	  if (info->shared
	      && (sec->flags & SEC_ALLOC) != 0
	      && (ELF32_R_TYPE (rel->r_info) != R_SH_REL32
		  || (h != NULL
		      && (! info->symbolic
			  || (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) == 0))))
	    {
	      /* When creating a shared object, we must copy these
		 reloc types into the output file.  We create a reloc
		 section in dynobj and make room for this reloc.  */
	      if (sreloc == NULL)
		{
		  const char *name;

		  name = (bfd_elf_string_from_elf_section
			  (abfd,
			   elf_elfheader (abfd)->e_shstrndx,
			   elf_section_data (sec)->rel_hdr.sh_name));
		  if (name == NULL)
		    return false;

		  BFD_ASSERT (strncmp (name, ".rela", 5) == 0
			      && strcmp (bfd_get_section_name (abfd, sec),
					 name + 5) == 0);

		  sreloc = bfd_get_section_by_name (dynobj, name);
		  if (sreloc == NULL)
		    {
		      flagword flags;

		      sreloc = bfd_make_section (dynobj, name);
		      flags = (SEC_HAS_CONTENTS | SEC_READONLY
			       | SEC_IN_MEMORY | SEC_LINKER_CREATED);
		      if ((sec->flags & SEC_ALLOC) != 0)
			flags |= SEC_ALLOC | SEC_LOAD;
		      if (sreloc == NULL
			  || ! bfd_set_section_flags (dynobj, sreloc, flags)
			  || ! bfd_set_section_alignment (dynobj, sreloc, 2))
			return false;
		    }
		}

	      sreloc->_raw_size += sizeof (Elf32_External_Rela);

	      /* If we are linking with -Bsymbolic, and this is a
		 global symbol, we count the number of PC relative
		 relocations we have entered for this symbol, so that
		 we can discard them again if the symbol is later
		 defined by a regular object.  Note that this function
		 is only called if we are using an elf_sh linker
		 hash table, which means that h is really a pointer to
		 an elf_sh_link_hash_entry.  */
	      if (h != NULL && info->symbolic
		  && ELF32_R_TYPE (rel->r_info) == R_SH_REL32)
		{
		  struct elf_sh_link_hash_entry *eh;
		  struct elf_sh_pcrel_relocs_copied *p;

		  eh = (struct elf_sh_link_hash_entry *) h;

		  for (p = eh->pcrel_relocs_copied; p != NULL; p = p->next)
		    if (p->section == sreloc)
		      break;

		  if (p == NULL)
		    {
		      p = ((struct elf_sh_pcrel_relocs_copied *)
			   bfd_alloc (dynobj, sizeof *p));
		      if (p == NULL)
			return false;
		      p->next = eh->pcrel_relocs_copied;
		      eh->pcrel_relocs_copied = p;
		      p->section = sreloc;
		      p->count = 0;
		    }

		  ++p->count;
		}
	    }

	  break;
	}
    }

  return true;
}

static boolean
sh_elf_set_mach_from_flags (abfd)
     bfd *abfd;
{
  flagword flags = elf_elfheader (abfd)->e_flags;

  switch (flags & EF_SH_MACH_MASK)
    {
    case EF_SH1:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh);
      break;
    case EF_SH2:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh2);
      break;
    case EF_SH_DSP:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh_dsp);
      break;
    case EF_SH3:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh3);
      break;
    case EF_SH3_DSP:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh3_dsp);
      break;
    case EF_SH3E:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh3e);
      break;
    case EF_SH_UNKNOWN:
    case EF_SH4:
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh4);
      break;
    default:
      return false;
    }
  return true;
}

/* Function to keep SH specific file flags.  */

static boolean
sh_elf_set_private_flags (abfd, flags)
     bfd *abfd;
     flagword flags;
{
  BFD_ASSERT (! elf_flags_init (abfd)
	      || elf_elfheader (abfd)->e_flags == flags);

  elf_elfheader (abfd)->e_flags = flags;
  elf_flags_init (abfd) = true;
  return sh_elf_set_mach_from_flags (abfd);
}

/* Copy backend specific data from one object module to another */

static boolean
sh_elf_copy_private_data (ibfd, obfd)
     bfd * ibfd;
     bfd * obfd;
{
  if (   bfd_get_flavour (ibfd) != bfd_target_elf_flavour
      || bfd_get_flavour (obfd) != bfd_target_elf_flavour)
    return true;

  return sh_elf_set_private_flags (obfd, elf_elfheader (ibfd)->e_flags);
}

/* This routine checks for linking big and little endian objects
   together, and for linking sh-dsp with sh3e / sh4 objects.  */

static boolean
sh_elf_merge_private_data (ibfd, obfd)
     bfd *ibfd;
     bfd *obfd;
{
  flagword old_flags, new_flags;

  if (_bfd_generic_verify_endian_match (ibfd, obfd) == false)
    return false;

  if (   bfd_get_flavour (ibfd) != bfd_target_elf_flavour
      || bfd_get_flavour (obfd) != bfd_target_elf_flavour)
    return true;

  if (! elf_flags_init (obfd))
    {
      /* This happens when ld starts out with a 'blank' output file.  */
      elf_flags_init (obfd) = true;
      elf_elfheader (obfd)->e_flags = EF_SH1;
    }
  old_flags = elf_elfheader (obfd)->e_flags;
  new_flags = elf_elfheader (ibfd)->e_flags;
  if ((EF_SH_HAS_DSP (old_flags) && EF_SH_HAS_FP (new_flags))
      || (EF_SH_HAS_DSP (new_flags) && EF_SH_HAS_FP (old_flags)))
    {
      (*_bfd_error_handler)
	("%s: uses %s instructions while previous modules use %s instructions",
	 bfd_get_filename (ibfd),
	 EF_SH_HAS_DSP (new_flags) ? "dsp" : "floating point",
	 EF_SH_HAS_DSP (new_flags) ? "floating point" : "dsp");
      bfd_set_error (bfd_error_bad_value);
      return false;
    }
  elf_elfheader (obfd)->e_flags = EF_SH_MERGE_MACH (old_flags, new_flags);

  return sh_elf_set_mach_from_flags (obfd);
}

/* Finish up dynamic symbol handling.  We set the contents of various
   dynamic sections here.  */

static boolean
sh_elf_finish_dynamic_symbol (output_bfd, info, h, sym)
     bfd *output_bfd;
     struct bfd_link_info *info;
     struct elf_link_hash_entry *h;
     Elf_Internal_Sym *sym;
{
  bfd *dynobj;

  dynobj = elf_hash_table (info)->dynobj;

  if (h->plt.offset != (bfd_vma) -1)
    {
      asection *splt;
      asection *sgot;
      asection *srel;

      bfd_vma plt_index;
      bfd_vma got_offset;
      Elf_Internal_Rela rel;

      /* This symbol has an entry in the procedure linkage table.  Set
	 it up.  */

      BFD_ASSERT (h->dynindx != -1);

      splt = bfd_get_section_by_name (dynobj, ".plt");
      sgot = bfd_get_section_by_name (dynobj, ".got.plt");
      srel = bfd_get_section_by_name (dynobj, ".rela.plt");
      BFD_ASSERT (splt != NULL && sgot != NULL && srel != NULL);

      /* Get the index in the procedure linkage table which
	 corresponds to this symbol.  This is the index of this symbol
	 in all the symbols for which we are making plt entries.  The
	 first entry in the procedure linkage table is reserved.  */
      plt_index = h->plt.offset / elf_sh_sizeof_plt (info) - 1;

      /* Get the offset into the .got table of the entry that
	 corresponds to this function.  Each .got entry is 4 bytes.
	 The first three are reserved.  */
      got_offset = (plt_index + 3) * 4;

      /* Fill in the entry in the procedure linkage table.  */
      if (! info->shared)
	{
	  if (elf_sh_plt_entry == NULL)
	    {
	      elf_sh_plt_entry = (bfd_big_endian (output_bfd) ?
				  elf_sh_plt_entry_be : elf_sh_plt_entry_le);
	    }
	  memcpy (splt->contents + h->plt.offset, elf_sh_plt_entry,
		  elf_sh_sizeof_plt (info));
	  bfd_put_32 (output_bfd,
		      (sgot->output_section->vma
		       + sgot->output_offset
		       + got_offset),
		      (splt->contents + h->plt.offset
		       + elf_sh_plt_symbol_offset (info)));

	  bfd_put_32 (output_bfd,
		      (splt->output_section->vma + splt->output_offset),
		      (splt->contents + h->plt.offset
		       + elf_sh_plt_plt0_offset (info)));
	}
      else
	{
	  if (elf_sh_pic_plt_entry == NULL)
	    {
	      elf_sh_pic_plt_entry = (bfd_big_endian (output_bfd) ?
				      elf_sh_pic_plt_entry_be :
				      elf_sh_pic_plt_entry_le);
	    }
	  memcpy (splt->contents + h->plt.offset, elf_sh_pic_plt_entry,
		  elf_sh_sizeof_plt (info));
	  bfd_put_32 (output_bfd, got_offset,
		      (splt->contents + h->plt.offset
		       + elf_sh_plt_symbol_offset (info)));
	}

      bfd_put_32 (output_bfd, plt_index * sizeof (Elf32_External_Rela),
		  (splt->contents + h->plt.offset
		   + elf_sh_plt_reloc_offset (info)));

      /* Fill in the entry in the global offset table.  */
      bfd_put_32 (output_bfd,
		  (splt->output_section->vma
		   + splt->output_offset
		   + h->plt.offset
		   + elf_sh_plt_temp_offset (info)),
		  sgot->contents + got_offset);

      /* Fill in the entry in the .rela.plt section.  */
      rel.r_offset = (sgot->output_section->vma
		      + sgot->output_offset
		      + got_offset);
      rel.r_info = ELF32_R_INFO (h->dynindx, R_SH_JMP_SLOT);
      rel.r_addend = 0;
      bfd_elf32_swap_reloca_out (output_bfd, &rel,
				((Elf32_External_Rela *) srel->contents
				 + plt_index));

      if ((h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR) == 0)
	{
	  /* Mark the symbol as undefined, rather than as defined in
	     the .plt section.  Leave the value alone.  */
	  sym->st_shndx = SHN_UNDEF;
	}
    }

  if (h->got.offset != (bfd_vma) -1)
    {
      asection *sgot;
      asection *srel;
      Elf_Internal_Rela rel;

      /* This symbol has an entry in the global offset table.  Set it
	 up.  */

      sgot = bfd_get_section_by_name (dynobj, ".got");
      srel = bfd_get_section_by_name (dynobj, ".rela.got");
      BFD_ASSERT (sgot != NULL && srel != NULL);

      rel.r_offset = (sgot->output_section->vma
		      + sgot->output_offset
		      + (h->got.offset &~ 1));

      /* If this is a -Bsymbolic link, and the symbol is defined
	 locally, we just want to emit a RELATIVE reloc.  Likewise if
	 the symbol was forced to be local because of a version file.
	 The entry in the global offset table will already have been
	 initialized in the relocate_section function.  */
      if (info->shared
	  && (info->symbolic || h->dynindx == -1)
	  && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR))
	{
	  rel.r_info = ELF32_R_INFO (0, R_SH_RELATIVE);
	  rel.r_addend = (h->root.u.def.value
			  + h->root.u.def.section->output_section->vma
			  + h->root.u.def.section->output_offset);
	}
      else
	{
	  bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents + h->got.offset);
	  rel.r_info = ELF32_R_INFO (h->dynindx, R_SH_GLOB_DAT);
	  rel.r_addend = 0;
	}

      bfd_elf32_swap_reloca_out (output_bfd, &rel,
				 ((Elf32_External_Rela *) srel->contents
				  + srel->reloc_count));
      ++srel->reloc_count;
    }

  if ((h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_COPY) != 0)
    {
      asection *s;
      Elf_Internal_Rela rel;

      /* This symbol needs a copy reloc.  Set it up.  */

      BFD_ASSERT (h->dynindx != -1
		  && (h->root.type == bfd_link_hash_defined
		      || h->root.type == bfd_link_hash_defweak));

      s = bfd_get_section_by_name (h->root.u.def.section->owner,
				   ".rela.bss");
      BFD_ASSERT (s != NULL);

      rel.r_offset = (h->root.u.def.value
		      + h->root.u.def.section->output_section->vma
		      + h->root.u.def.section->output_offset);
      rel.r_info = ELF32_R_INFO (h->dynindx, R_SH_COPY);
      rel.r_addend = 0;
      bfd_elf32_swap_reloca_out (output_bfd, &rel,
				 ((Elf32_External_Rela *) s->contents
				  + s->reloc_count));
      ++s->reloc_count;
    }

  /* Mark _DYNAMIC and _GLOBAL_OFFSET_TABLE_ as absolute.  */
  if (strcmp (h->root.root.string, "_DYNAMIC") == 0
      || strcmp (h->root.root.string, "_GLOBAL_OFFSET_TABLE_") == 0)
    sym->st_shndx = SHN_ABS;

  return true;
}

/* Finish up the dynamic sections.  */

static boolean
sh_elf_finish_dynamic_sections (output_bfd, info)
     bfd *output_bfd;
     struct bfd_link_info *info;
{
  bfd *dynobj;
  asection *sgot;
  asection *sdyn;

  dynobj = elf_hash_table (info)->dynobj;

  sgot = bfd_get_section_by_name (dynobj, ".got.plt");
  BFD_ASSERT (sgot != NULL);
  sdyn = bfd_get_section_by_name (dynobj, ".dynamic");

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      asection *splt;
      Elf32_External_Dyn *dyncon, *dynconend;

      BFD_ASSERT (sdyn != NULL);

      dyncon = (Elf32_External_Dyn *) sdyn->contents;
      dynconend = (Elf32_External_Dyn *) (sdyn->contents + sdyn->_raw_size);
      for (; dyncon < dynconend; dyncon++)
	{
	  Elf_Internal_Dyn dyn;
	  const char *name;
	  asection *s;

	  bfd_elf32_swap_dyn_in (dynobj, dyncon, &dyn);

	  switch (dyn.d_tag)
	    {
	    default:
	      break;

	    case DT_PLTGOT:
	      name = ".got";
	      goto get_vma;

	    case DT_JMPREL:
	      name = ".rela.plt";
	    get_vma:
	      s = bfd_get_section_by_name (output_bfd, name);
	      BFD_ASSERT (s != NULL);
	      dyn.d_un.d_ptr = s->vma;
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;

	    case DT_PLTRELSZ:
	      s = bfd_get_section_by_name (output_bfd, ".rela.plt");
	      BFD_ASSERT (s != NULL);
	      if (s->_cooked_size != 0)
		dyn.d_un.d_val = s->_cooked_size;
	      else
		dyn.d_un.d_val = s->_raw_size;
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;

	    case DT_RELASZ:
	      /* My reading of the SVR4 ABI indicates that the
		 procedure linkage table relocs (DT_JMPREL) should be
		 included in the overall relocs (DT_RELA).  This is
		 what Solaris does.  However, UnixWare can not handle
		 that case.  Therefore, we override the DT_RELASZ entry
		 here to make it not include the JMPREL relocs.  Since
		 the linker script arranges for .rela.plt to follow all
		 other relocation sections, we don't have to worry
		 about changing the DT_RELA entry.  */
	      s = bfd_get_section_by_name (output_bfd, ".rela.plt");
	      if (s != NULL)
		{
		  if (s->_cooked_size != 0)
		    dyn.d_un.d_val -= s->_cooked_size;
		  else
		    dyn.d_un.d_val -= s->_raw_size;
		}
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;
	    }
	}

      /* Fill in the first entry in the procedure linkage table.  */
      splt = bfd_get_section_by_name (dynobj, ".plt");
      if (splt && splt->_raw_size > 0)
	{
	  if (info->shared)
	    {
	      if (elf_sh_pic_plt_entry == NULL)
		{
		  elf_sh_pic_plt_entry = (bfd_big_endian (output_bfd) ?
					  elf_sh_pic_plt_entry_be :
					  elf_sh_pic_plt_entry_le);
		}
	      memcpy (splt->contents, elf_sh_pic_plt_entry,
		      elf_sh_sizeof_plt (info));
	    }
	  else
	    {
	      if (elf_sh_plt0_entry == NULL)
		{
		  elf_sh_plt0_entry = (bfd_big_endian (output_bfd) ?
				       elf_sh_plt0_entry_be :
				       elf_sh_plt0_entry_le);
		}
	      memcpy (splt->contents, elf_sh_plt0_entry, PLT_ENTRY_SIZE);
	      bfd_put_32 (output_bfd,
			  sgot->output_section->vma + sgot->output_offset + 4,
			  splt->contents + elf_sh_plt0_gotid_offset (info));
	      bfd_put_32 (output_bfd,
			  sgot->output_section->vma + sgot->output_offset + 8,
			  splt->contents + elf_sh_plt0_linker_offset (info));
	    }

	  /* UnixWare sets the entsize of .plt to 4, although that doesn't
	     really seem like the right value.  */
	  elf_section_data (splt->output_section)->this_hdr.sh_entsize = 4;
	}
    }

  /* Fill in the first three entries in the global offset table.  */
  if (sgot->_raw_size > 0)
    {
      if (sdyn == NULL)
	bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents);
      else
	bfd_put_32 (output_bfd,
		    sdyn->output_section->vma + sdyn->output_offset,
		    sgot->contents);
      bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents + 4);
      bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents + 8);
    }

  elf_section_data (sgot->output_section)->this_hdr.sh_entsize = 4;

  return true;
}

#ifndef ELF_ARCH
#define TARGET_BIG_SYM		bfd_elf32_sh_vec
#define TARGET_BIG_NAME		"elf32-sh"
#define TARGET_LITTLE_SYM	bfd_elf32_shl_vec
#define TARGET_LITTLE_NAME	"elf32-shl"
#define ELF_ARCH		bfd_arch_sh
#define ELF_MACHINE_CODE	EM_SH
#define ELF_MAXPAGESIZE		128

#define elf_symbol_leading_char '_'
#endif /* ELF_ARCH */

#define bfd_elf32_bfd_reloc_type_lookup	sh_elf_reloc_type_lookup
#define elf_info_to_howto		sh_elf_info_to_howto
#define bfd_elf32_bfd_relax_section	sh_elf_relax_section
#define elf_backend_relocate_section	sh_elf_relocate_section
#define bfd_elf32_bfd_get_relocated_section_contents \
					sh_elf_get_relocated_section_contents
#define elf_backend_object_p		sh_elf_set_mach_from_flags
#define bfd_elf32_bfd_set_private_bfd_flags \
					sh_elf_set_private_flags
#define bfd_elf32_bfd_copy_private_bfd_data \
					sh_elf_copy_private_data
#define bfd_elf32_bfd_merge_private_bfd_data \
					sh_elf_merge_private_data

#define elf_backend_gc_mark_hook        sh_elf_gc_mark_hook
#define elf_backend_gc_sweep_hook       sh_elf_gc_sweep_hook
#define elf_backend_check_relocs        sh_elf_check_relocs

#define elf_backend_can_gc_sections	1
#define elf_backend_create_dynamic_sections \
					sh_elf_create_dynamic_sections
#define bfd_elf32_bfd_link_hash_table_create \
					sh_elf_link_hash_table_create
#define elf_backend_adjust_dynamic_symbol \
					sh_elf_adjust_dynamic_symbol
#define elf_backend_size_dynamic_sections \
					sh_elf_size_dynamic_sections
#define elf_backend_finish_dynamic_symbol \
					sh_elf_finish_dynamic_symbol
#define elf_backend_finish_dynamic_sections \
					sh_elf_finish_dynamic_sections

#define elf_backend_want_got_plt	1
#define elf_backend_plt_readonly	1
#define elf_backend_want_plt_sym	0
#define elf_backend_got_header_size	12
#define elf_backend_plt_header_size	PLT_ENTRY_SIZE
#include "elf32-target.h"
