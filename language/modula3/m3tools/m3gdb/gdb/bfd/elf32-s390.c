/* IBM S/390-specific support for 32-bit ELF
   Copyright 2000, 2001 Free Software Foundation, Inc.
   Contributed by Carl B. Pedersen and Martin Schwidefsky.

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
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "bfd.h"
#include "sysdep.h"
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"

static reloc_howto_type *elf_s390_reloc_type_lookup
  PARAMS ((bfd *, bfd_reloc_code_real_type));
static void elf_s390_info_to_howto
  PARAMS ((bfd *, arelent *, Elf_Internal_Rela *));
static boolean elf_s390_is_local_label_name PARAMS ((bfd *, const char *));
static struct bfd_hash_entry *elf_s390_link_hash_newfunc
  PARAMS ((struct bfd_hash_entry *, struct bfd_hash_table *, const char *));
static struct bfd_link_hash_table *elf_s390_link_hash_table_create
  PARAMS ((bfd *));
static boolean elf_s390_check_relocs
  PARAMS ((bfd *, struct bfd_link_info *, asection *,
	   const Elf_Internal_Rela *));
static boolean elf_s390_adjust_dynamic_symbol
  PARAMS ((struct bfd_link_info *, struct elf_link_hash_entry *));
static boolean elf_s390_size_dynamic_sections
  PARAMS ((bfd *, struct bfd_link_info *));
static boolean elf_s390_relocate_section
  PARAMS ((bfd *, struct bfd_link_info *, bfd *, asection *, bfd_byte *,
	   Elf_Internal_Rela *, Elf_Internal_Sym *, asection **));
static boolean elf_s390_finish_dynamic_symbol
  PARAMS ((bfd *, struct bfd_link_info *, struct elf_link_hash_entry *,
	   Elf_Internal_Sym *));
static boolean elf_s390_finish_dynamic_sections
  PARAMS ((bfd *, struct bfd_link_info *));

#define USE_RELA 1		/* We want RELA relocations, not REL.  */

#include "elf/s390.h"

/* The relocation "howto" table.  */

static reloc_howto_type elf_howto_table[] =
{
  HOWTO (R_390_NONE,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont, /* complain_on_overflow */
	 bfd_elf_generic_reloc, /* special_function */
	 "R_390_NONE",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  HOWTO(R_390_8,         0, 0,  8, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_8",       false, 0,0x000000ff, false),
  HOWTO(R_390_12,        0, 1, 12, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_390_12",      false, 0,0x00000fff, false),
  HOWTO(R_390_16,        0, 1, 16, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_16",      false, 0,0x0000ffff, false),
  HOWTO(R_390_32,        0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_32",      false, 0,0xffffffff, false),
  HOWTO(R_390_PC32,	 0, 2, 32,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_PC32",    false, 0,0xffffffff,  true),
  HOWTO(R_390_GOT12,	 0, 1, 12, false, 0, complain_overflow_dont, bfd_elf_generic_reloc, "R_390_GOT12",   false, 0,0x00000fff, false),
  HOWTO(R_390_GOT32,	 0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_GOT32",   false, 0,0xffffffff, false),
  HOWTO(R_390_PLT32,	 0, 2, 32,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_PLT32",   false, 0,0xffffffff,  true),
  HOWTO(R_390_COPY,      0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_COPY",    false, 0,0xffffffff, false),
  HOWTO(R_390_GLOB_DAT,  0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_GLOB_DAT",false, 0,0xffffffff, false),
  HOWTO(R_390_JMP_SLOT,  0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_JMP_SLOT",false, 0,0xffffffff, false),
  HOWTO(R_390_RELATIVE,  0, 2, 32,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_RELATIVE",false, 0,0xffffffff, false),
  HOWTO(R_390_GOTOFF,    0, 2, 32, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_GOTOFF",  false, 0,0xffffffff, false),
  HOWTO(R_390_GOTPC,     0, 2, 32,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_GOTPC",   false, 0,0xffffffff,  true),
  HOWTO(R_390_GOT16,     0, 1, 16, false, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_GOT16",   false, 0,0x0000ffff, false),
  HOWTO(R_390_PC16,      0, 1, 16,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_PC16",    false, 0,0x0000ffff,  true),
  HOWTO(R_390_PC16DBL,   1, 1, 16,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_PC16DBL", false, 0,0x0000ffff,  true),
  HOWTO(R_390_PLT16DBL,  1, 1, 16,  true, 0, complain_overflow_bitfield, bfd_elf_generic_reloc, "R_390_PLT16DBL", false, 0,0x0000ffff,  true),
};

/* GNU extension to record C++ vtable hierarchy.  */
static reloc_howto_type elf32_s390_vtinherit_howto =
  HOWTO (R_390_GNU_VTINHERIT, 0,2,0,false,0,complain_overflow_dont, NULL, "R_390_GNU_VTINHERIT", false,0, 0, false);
static reloc_howto_type elf32_s390_vtentry_howto =
  HOWTO (R_390_GNU_VTENTRY, 0,2,0,false,0,complain_overflow_dont, _bfd_elf_rel_vtable_reloc_fn,"R_390_GNU_VTENTRY", false,0,0, false); 

static reloc_howto_type *
elf_s390_reloc_type_lookup (abfd, code)
     bfd *abfd ATTRIBUTE_UNUSED;
     bfd_reloc_code_real_type code;
{
  switch (code) {
  case BFD_RELOC_NONE:
    return &elf_howto_table[(int) R_390_NONE];
  case BFD_RELOC_8:
    return &elf_howto_table[(int) R_390_8];
  case BFD_RELOC_390_12:
    return &elf_howto_table[(int) R_390_12];
  case BFD_RELOC_16:
    return &elf_howto_table[(int) R_390_16];
  case BFD_RELOC_32:
    return &elf_howto_table[(int) R_390_32];
  case BFD_RELOC_CTOR:
    return &elf_howto_table[(int) R_390_32];
  case BFD_RELOC_32_PCREL:
    return &elf_howto_table[(int) R_390_PC32];
  case BFD_RELOC_390_GOT12:
    return &elf_howto_table[(int) R_390_GOT12];
  case BFD_RELOC_32_GOT_PCREL:
    return &elf_howto_table[(int) R_390_GOT32];
  case BFD_RELOC_390_PLT32:
    return &elf_howto_table[(int) R_390_PLT32];
  case BFD_RELOC_390_COPY:
    return &elf_howto_table[(int) R_390_COPY];
  case BFD_RELOC_390_GLOB_DAT:
    return &elf_howto_table[(int) R_390_GLOB_DAT];
  case BFD_RELOC_390_JMP_SLOT:
    return &elf_howto_table[(int) R_390_JMP_SLOT];
  case BFD_RELOC_390_RELATIVE:
    return &elf_howto_table[(int) R_390_RELATIVE];
  case BFD_RELOC_32_GOTOFF:
    return &elf_howto_table[(int) R_390_GOTOFF];
  case BFD_RELOC_390_GOTPC:
    return &elf_howto_table[(int) R_390_GOTPC];
  case BFD_RELOC_390_GOT16:
    return &elf_howto_table[(int) R_390_GOT16];
  case BFD_RELOC_16_PCREL:
    return &elf_howto_table[(int) R_390_PC16];
  case BFD_RELOC_390_PC16DBL:
    return &elf_howto_table[(int) R_390_PC16DBL];
  case BFD_RELOC_390_PLT16DBL:
    return &elf_howto_table[(int) R_390_PLT16DBL];
  case BFD_RELOC_VTABLE_INHERIT:
    return &elf32_s390_vtinherit_howto;
  case BFD_RELOC_VTABLE_ENTRY:
    return &elf32_s390_vtentry_howto;
  default:
    break;                                         
  }
  return 0;
}

/* We need to use ELF32_R_TYPE so we have our own copy of this function,
   and elf32-s390.c has its own copy.  */

static void
elf_s390_info_to_howto (abfd, cache_ptr, dst)
     bfd *abfd ATTRIBUTE_UNUSED;
     arelent *cache_ptr;
     Elf_Internal_Rela *dst;
{
  switch (ELF32_R_TYPE(dst->r_info))
    {
    case R_390_GNU_VTINHERIT:
      cache_ptr->howto = &elf32_s390_vtinherit_howto;
      break;

    case R_390_GNU_VTENTRY:
      cache_ptr->howto = &elf32_s390_vtentry_howto;
      break;

    default:
      BFD_ASSERT (ELF32_R_TYPE(dst->r_info) < (unsigned int) R_390_max);
      cache_ptr->howto = &elf_howto_table[ELF32_R_TYPE(dst->r_info)];
    }     
}

static boolean
elf_s390_is_local_label_name (abfd, name)
     bfd *abfd;
     const char *name;
{
  if (name[0] == '.' && (name[1] == 'X' || name[1] == 'L'))
    return true;

  return _bfd_elf_is_local_label_name (abfd, name);
}

/* Functions for the 390 ELF linker.  */

/* The name of the dynamic interpreter.  This is put in the .interp
   section.  */

#define ELF_DYNAMIC_INTERPRETER "/usr/lib/ld.so.1"

/* The nop opcode we use.  */

#define s390_NOP 0x07070707


/* The size in bytes of the first entry in the procedure linkage table.  */
#define PLT_FIRST_ENTRY_SIZE 32
/* The size in bytes of an entry in the procedure linkage table.  */
#define PLT_ENTRY_SIZE 32 

#define GOT_ENTRY_SIZE 4

/* The first three entries in a procedure linkage table are reserved,
   and the initial contents are unimportant (we zero them out).
   Subsequent entries look like this.  See the SVR4 ABI 386
   supplement to see how this works.  */

/* For the s390, simple addr offset can only be 0 - 4096.
   To use the full 2 GB address space, several instructions
   are needed to load an address in a register and execute
   a branch( or just saving the address)

   Furthermore, only r 0 and 1 are free to use!!!  */ 

/* The first 3 words in the GOT are then reserved.
   Word 0 is the address of the dynamic table.
   Word 1 is a pointer to a structure describing the object
   Word 2 is used to point to the loader entry address.

   The code for position independand PLT entries looks like this:

   r12 holds addr of the current GOT at entry to the PLT

   The GOT holds the address in the PLT to be executed.
   The loader then gets:
   24(15) =  Pointer to the structure describing the object.
   28(15) =  Offset in symbol table                                             

   The loader  must  then find the module where the function is
   and insert the address in the GOT.

  Note: 390 can only address +- 64 K relative.
        We check if offset > 65536, then make a relative branch -64xxx
        back to a previous defined branch

PLT1: BASR 1,0         # 2 bytes
      L    1,22(1)     # 4 bytes  Load offset in GOT in r 1
      L    1,(1,12)    # 4 bytes  Load address from GOT in r1
      BCR  15,1        # 2 bytes  Jump to address
RET1: BASR 1,0         # 2 bytes  Return from GOT 1st time
      L    1,14(1)     # 4 bytes  Load offset in symol table in r1
      BRC  15,-x       # 4 bytes  Jump to start of PLT
      .word 0          # 2 bytes filler
      .long ?          # 4 bytes  offset in GOT
      .long ?          # 4 bytes  offset into symbol table

  This was the general case. There are two additional, optimizes PLT
  definitions. One for GOT offsets < 4096 and one for GOT offsets < 32768.
  First the one for GOT offsets < 4096:

PLT1: L    1,<offset>(12) # 4 bytes  Load address from GOT in R1
      BCR  15,1           # 2 bytes  Jump to address
      .word 0,0,0         # 6 bytes  filler
RET1: BASR 1,0            # 2 bytes  Return from GOT 1st time
      L    1,14(1)        # 4 bytes  Load offset in symbol table in r1
      BRC  15,-x          # 4 bytes  Jump to start of PLT
      .word 0,0,0         # 6 bytes  filler
      .long ?             # 4 bytes  offset into symbol table

  Second the one for GOT offsets < 32768:

PLT1: LHI  1,<offset>     # 4 bytes  Load offset in GOT to r1
      L    1,(1,12)       # 4 bytes  Load address from GOT to r1
      BCR  15,1           # 2 bytes  Jump to address
      .word 0             # 2 bytes  filler
RET1: BASR 1,0            # 2 bytes  Return from GOT 1st time
      L    1,14(1)        # 4 bytes  Load offset in symbol table in r1
      BRC  15,-x          # 4 bytes  Jump to start of PLT
      .word 0,0,0         # 6 bytes  filler
      .long ?             # 4 bytes  offset into symbol table

Total = 32 bytes per PLT entry

   The code for static build PLT entries looks like this:

PLT1: BASR 1,0         # 2 bytes
      L    1,22(1)     # 4 bytes  Load address of GOT entry
      L    1,0(0,1)    # 4 bytes  Load address from GOT in r1
      BCR  15,1        # 2 bytes  Jump to address
RET1: BASR 1,0         # 2 bytes  Return from GOT 1st time
      L    1,14(1)     # 4 bytes  Load offset in symbol table in r1
      BRC  15,-x       # 4 bytes  Jump to start of PLT
      .word 0          # 2 bytes  filler
      .long ?          # 4 bytes  address of GOT entry
      .long ?          # 4 bytes  offset into symbol table  */

#define PLT_PIC_ENTRY_WORD0 0x0d105810
#define PLT_PIC_ENTRY_WORD1 0x10165811
#define PLT_PIC_ENTRY_WORD2 0xc00007f1
#define PLT_PIC_ENTRY_WORD3 0x0d105810
#define PLT_PIC_ENTRY_WORD4 0x100ea7f4

#define PLT_PIC12_ENTRY_WORD0 0x5810c000
#define PLT_PIC12_ENTRY_WORD1 0x07f10000
#define PLT_PIC12_ENTRY_WORD2 0x00000000
#define PLT_PIC12_ENTRY_WORD3 0x0d105810
#define PLT_PIC12_ENTRY_WORD4 0x100ea7f4

#define PLT_PIC16_ENTRY_WORD0 0xa7180000
#define PLT_PIC16_ENTRY_WORD1 0x5811c000
#define PLT_PIC16_ENTRY_WORD2 0x07f10000
#define PLT_PIC16_ENTRY_WORD3 0x0d105810
#define PLT_PIC16_ENTRY_WORD4 0x100ea7f4

#define PLT_ENTRY_WORD0     0x0d105810
#define PLT_ENTRY_WORD1     0x10165810
#define PLT_ENTRY_WORD2     0x100007f1
#define PLT_ENTRY_WORD3     0x0d105810
#define PLT_ENTRY_WORD4     0x100ea7f4

/* The first PLT entry pushes the offset into the symbol table
   from R1 onto the stack at 8(15) and the loader object info
   at 12(15), loads the loader address in R1 and jumps to it.  */

/* The first entry in the PLT for PIC code:

PLT0:
   ST   1,28(15)  # R1 has offset into symbol table
   L    1,4(12)   # Get loader ino(object struct address)
   ST   1,24(15)  # Store address 
   L    1,8(12)   # Entry address of loader in R1
   BR   1         # Jump to loader

   The first entry in the PLT for static code:

PLT0:
   ST   1,28(15)      # R1 has offset into symbol table
   BASR 1,0
   L    1,18(0,1)     # Get address of GOT
   MVC  24(4,15),4(1) # Move loader ino to stack
   L    1,8(1)        # Get address of loader
   BR   1             # Jump to loader
   .word 0            # filler
   .long got          # address of GOT  */

#define PLT_PIC_FIRST_ENTRY_WORD0 0x5010f01c
#define PLT_PIC_FIRST_ENTRY_WORD1 0x5810c004
#define PLT_PIC_FIRST_ENTRY_WORD2 0x5010f018
#define PLT_PIC_FIRST_ENTRY_WORD3 0x5810c008
#define PLT_PIC_FIRST_ENTRY_WORD4 0x07f10000

#define PLT_FIRST_ENTRY_WORD0     0x5010f01c
#define PLT_FIRST_ENTRY_WORD1     0x0d105810
#define PLT_FIRST_ENTRY_WORD2     0x1012D203
#define PLT_FIRST_ENTRY_WORD3     0xf0181004
#define PLT_FIRST_ENTRY_WORD4     0x58101008
#define PLT_FIRST_ENTRY_WORD5     0x07f10000

/* The s390 linker needs to keep track of the number of relocs that it
   decides to copy in check_relocs for each symbol.  This is so that
   it can discard PC relative relocs if it doesn't need them when
   linking with -Bsymbolic.  We store the information in a field
   extending the regular ELF linker hash table.  */

/* This structure keeps track of the number of PC relative relocs we
   have copied for a given symbol.  */

struct elf_s390_pcrel_relocs_copied
{
  /* Next section.  */
  struct elf_s390_pcrel_relocs_copied *next;
  /* A section in dynobj.  */
  asection *section;
  /* Number of relocs copied in this section.  */
  bfd_size_type count;
};

/* s390 ELF linker hash entry.  */

struct elf_s390_link_hash_entry
{
  struct elf_link_hash_entry root;

  /* Number of PC relative relocs copied for this symbol.  */
  struct elf_s390_pcrel_relocs_copied *pcrel_relocs_copied;
};

/* s390 ELF linker hash table.  */

struct elf_s390_link_hash_table
{
  struct elf_link_hash_table root;
};

/* Declare this now that the above structures are defined.  */

static boolean elf_s390_discard_copies
  PARAMS ((struct elf_s390_link_hash_entry *, PTR));

/* Traverse an s390 ELF linker hash table.  */

#define elf_s390_link_hash_traverse(table, func, info)			\
  (elf_link_hash_traverse						\
   (&(table)->root,							\
    (boolean (*) PARAMS ((struct elf_link_hash_entry *, PTR))) (func),	\
    (info)))

/* Get the s390 ELF linker hash table from a link_info structure.  */

#define elf_s390_hash_table(p) \
  ((struct elf_s390_link_hash_table *) ((p)->hash))

/* Create an entry in an s390 ELF linker hash table.  */

static struct bfd_hash_entry *
elf_s390_link_hash_newfunc (entry, table, string)
     struct bfd_hash_entry *entry;
     struct bfd_hash_table *table;
     const char *string;
{
  struct elf_s390_link_hash_entry *ret =
    (struct elf_s390_link_hash_entry *) entry;

  /* Allocate the structure if it has not already been allocated by a
     subclass.  */
  if (ret == (struct elf_s390_link_hash_entry *) NULL)
    ret = ((struct elf_s390_link_hash_entry *)
	   bfd_hash_allocate (table,
			      sizeof (struct elf_s390_link_hash_entry)));
  if (ret == (struct elf_s390_link_hash_entry *) NULL)
    return (struct bfd_hash_entry *) ret;

  /* Call the allocation method of the superclass.  */
  ret = ((struct elf_s390_link_hash_entry *)
	 _bfd_elf_link_hash_newfunc ((struct bfd_hash_entry *) ret,
				     table, string));
  if (ret != (struct elf_s390_link_hash_entry *) NULL)
    {
      ret->pcrel_relocs_copied = NULL;
    }

  return (struct bfd_hash_entry *) ret;
}

/* Create an s390 ELF linker hash table.  */

static struct bfd_link_hash_table *
elf_s390_link_hash_table_create (abfd)
     bfd *abfd;
{
  struct elf_s390_link_hash_table *ret;

  ret = ((struct elf_s390_link_hash_table *)
	 bfd_alloc (abfd, sizeof (struct elf_s390_link_hash_table)));
  if (ret == (struct elf_s390_link_hash_table *) NULL)
    return NULL;

  if (! _bfd_elf_link_hash_table_init (&ret->root, abfd,
				       elf_s390_link_hash_newfunc))
    {
      bfd_release (abfd, ret);
      return NULL;
    }

  return &ret->root.root;
}


/* Look through the relocs for a section during the first phase, and
   allocate space in the global offset table or procedure linkage
   table.  */

static boolean
elf_s390_check_relocs (abfd, info, sec, relocs)
     bfd *abfd;
     struct bfd_link_info *info;
     asection *sec;
     const Elf_Internal_Rela *relocs;
{
  bfd *dynobj;
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  bfd_signed_vma *local_got_refcounts;
  const Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *rel_end;
  asection *sgot;
  asection *srelgot;
  asection *sreloc;

  if (info->relocateable)
    return true;

  dynobj = elf_hash_table (info)->dynobj;
  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  local_got_refcounts = elf_local_got_offsets (abfd);

  sgot = NULL;
  srelgot = NULL;
  sreloc = NULL;

  rel_end = relocs + sec->reloc_count;
  for (rel = relocs; rel < rel_end; rel++)
    {
      unsigned long r_symndx;
      struct elf_link_hash_entry *h;

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
	    case R_390_GOT12:
            case R_390_GOT16:
	    case R_390_GOT32:
	    case R_390_GOTOFF:
	    case R_390_GOTPC:
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
	case R_390_GOT12:
        case R_390_GOT16:
	case R_390_GOT32:
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
	      if (h->got.refcount == -1)
		{
		  h->got.refcount = 1;

		  /* Make sure this symbol is output as a dynamic symbol.  */
		  if (h->dynindx == -1)
		    {
		      if (! bfd_elf32_link_record_dynamic_symbol (info, h))
			return false;
		    }

		  sgot->_raw_size += 4;
		  srelgot->_raw_size += sizeof (Elf32_External_Rela);
		}
	      else
		h->got.refcount += 1;
	    }
	  else
	    {
     	      /* This is a global offset table entry for a local symbol.  */
	      if (local_got_refcounts == NULL)
		{
		  size_t size;

		  size = symtab_hdr->sh_info * sizeof (bfd_signed_vma);
		  local_got_refcounts = (bfd_signed_vma *)
		                         bfd_alloc (abfd, size);
		  if (local_got_refcounts == NULL)
		    return false;
		  elf_local_got_refcounts (abfd) = local_got_refcounts;
		  memset (local_got_refcounts, -1, size);
		}
	      if (local_got_refcounts[r_symndx] == -1)
		{
		  local_got_refcounts[r_symndx] = 1;

		  sgot->_raw_size += 4;
		  if (info->shared)
		    {
		      /* If we are generating a shared object, we need to
			 output a R_390_RELATIVE reloc so that the dynamic
			 linker can adjust this GOT entry.  */
		      srelgot->_raw_size += sizeof (Elf32_External_Rela);
		    }
		}
	      else
		local_got_refcounts[r_symndx] += 1;
	    }
	  break;

        case R_390_PLT16DBL:
	case R_390_PLT32:
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

	  if (h->plt.refcount == -1)
	    {
	      h->plt.refcount = 1;
	      h->elf_link_hash_flags |= ELF_LINK_HASH_NEEDS_PLT;
	    }
	  else
	    h->plt.refcount += 1;
	  break;

        case R_390_8:
        case R_390_16:
	case R_390_32:
        case R_390_PC16:
        case R_390_PC16DBL:
	case R_390_PC32:
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
	      && ((ELF32_R_TYPE (rel->r_info) != R_390_PC16 &&
                   ELF32_R_TYPE (rel->r_info) != R_390_PC16DBL &&
                   ELF32_R_TYPE (rel->r_info) != R_390_PC32)
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
                 is only called if we are using an elf_s390 linker
                 hash table, which means that h is really a pointer to
                 an elf_s390_link_hash_entry.  */
	      if (h != NULL
		  && (ELF32_R_TYPE (rel->r_info) == R_390_PC16 ||
                      ELF32_R_TYPE (rel->r_info) == R_390_PC16DBL ||
                      ELF32_R_TYPE (rel->r_info) == R_390_PC32))
		{
		  struct elf_s390_link_hash_entry *eh;
		  struct elf_s390_pcrel_relocs_copied *p;

		  eh = (struct elf_s390_link_hash_entry *) h;

		  for (p = eh->pcrel_relocs_copied; p != NULL; p = p->next)
		    if (p->section == sreloc)
		      break;

		  if (p == NULL)
		    {
		      p = ((struct elf_s390_pcrel_relocs_copied *)
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

	  /* This relocation describes the C++ object vtable hierarchy.
	     Reconstruct it for later use during GC.  */
        case R_390_GNU_VTINHERIT:
          if (!_bfd_elf32_gc_record_vtinherit (abfd, sec, h, rel->r_offset))
            return false;
          break;

	  /* This relocation describes which C++ vtable entries are actually
	     used.  Record for later use during GC.  */
        case R_390_GNU_VTENTRY:
          if (!_bfd_elf32_gc_record_vtentry (abfd, sec, h, rel->r_addend))
            return false;
          break;
                   
	default:
	  break;
	}
    }

  return true;
}

/* Return the section that should be marked against GC for a given
   relocation.  */

static asection *
elf_s390_gc_mark_hook (abfd, info, rel, h, sym)
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
	case R_390_GNU_VTINHERIT:
	case R_390_GNU_VTENTRY:
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
	{
	  return bfd_section_from_elf_index (abfd, sym->st_shndx);
	}
    }

  return NULL;
}

/* Update the got entry reference counts for the section being removed.  */

static boolean
elf_s390_gc_sweep_hook (abfd, info, sec, relocs)
     bfd *abfd ATTRIBUTE_UNUSED;
     struct bfd_link_info *info ATTRIBUTE_UNUSED;
     asection *sec ATTRIBUTE_UNUSED;
     const Elf_Internal_Rela *relocs ATTRIBUTE_UNUSED;
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  bfd_signed_vma *local_got_refcounts;
  const Elf_Internal_Rela *rel, *relend;
  unsigned long r_symndx;
  struct elf_link_hash_entry *h;
  bfd *dynobj;
  asection *sgot;
  asection *srelgot;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  local_got_refcounts = elf_local_got_refcounts (abfd);

  dynobj = elf_hash_table (info)->dynobj;
  if (dynobj == NULL)
    return true;

  sgot = bfd_get_section_by_name (dynobj, ".got");
  srelgot = bfd_get_section_by_name (dynobj, ".rela.got");

  relend = relocs + sec->reloc_count;
  for (rel = relocs; rel < relend; rel++)
    switch (ELF32_R_TYPE (rel->r_info))
      {
      case R_390_GOT12:
      case R_390_GOT16:
      case R_390_GOT32:
      case R_390_GOTOFF:
      case R_390_GOTPC:
	r_symndx = ELF32_R_SYM (rel->r_info);
	if (r_symndx >= symtab_hdr->sh_info)
	  {
	    h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	    if (h->got.refcount > 0)
	      {
		h->got.refcount -= 1;
		if (h->got.refcount == 0)
		  {
		    sgot->_raw_size -= 4;
		    srelgot->_raw_size -= sizeof (Elf32_External_Rela);
		  }
	      }
	  }
	else if (local_got_refcounts != NULL)
	  {
	    if (local_got_refcounts[r_symndx] > 0)
	      {
		local_got_refcounts[r_symndx] -= 1;
		if (local_got_refcounts[r_symndx] == 0)
		  {
		    sgot->_raw_size -= 4;
		    if (info->shared)
		      srelgot->_raw_size -= sizeof (Elf32_External_Rela);
		  }
	      }
	  }
	break;

      case R_390_PLT16DBL:
      case R_390_PLT32:
	r_symndx = ELF32_R_SYM (rel->r_info);
	if (r_symndx >= symtab_hdr->sh_info)
	  {
	    h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	    if (h->plt.refcount > 0)
	      h->plt.refcount -= 1;
	  }
	break;

      default:
	break;
      }

  return true;
}

/* Adjust a symbol defined by a dynamic object and referenced by a
   regular object.  The current definition is in some section of the
   dynamic object, but we're not including those sections.  We have to
   change the definition to something the rest of the link can
   understand.  */

static boolean
elf_s390_adjust_dynamic_symbol (info, h)
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
     will fill in the contents of the procedure linkage table later
     (although we could actually do it here). */
  if (h->type == STT_FUNC
      || (h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_PLT) != 0)
    {
      if ((! info->shared
	   && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_DYNAMIC) == 0
	   && (h->elf_link_hash_flags & ELF_LINK_HASH_REF_DYNAMIC) == 0)
	  || (info->shared && h->plt.refcount <= 0))
	{
	  /* This case can occur if we saw a PLT32 reloc in an input
             file, but the symbol was never referred to by a dynamic
             object, or if all references were garbage collected.  In
	     such a case, we don't actually need to build a procedure
	     linkage table, and we can just do a PC32 reloc instead.  */
	  h->plt.offset = (bfd_vma) -1;
	  h->elf_link_hash_flags &= ~ELF_LINK_HASH_NEEDS_PLT;
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

      /* The first entry in .plt is reserved.  */
      if (s->_raw_size == 0)
	s->_raw_size = PLT_FIRST_ENTRY_SIZE;

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
      s->_raw_size += PLT_ENTRY_SIZE;

      /* We also need to make an entry in the .got.plt section, which
	 will be placed in the .got section by the linker script.  */
      s = bfd_get_section_by_name (dynobj, ".got.plt");
      BFD_ASSERT (s != NULL);
      s->_raw_size += GOT_ENTRY_SIZE;

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

  /* We must generate a R_390_COPY reloc to tell the dynamic linker
     to copy the initial value out of the dynamic object and into the
     runtime process image.  We need to remember the offset into the
     .rel.bss section we are going to use.  */
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
elf_s390_size_dynamic_sections (output_bfd, info)
     bfd *output_bfd;
     struct bfd_link_info *info;
{
  bfd *dynobj;
  asection *s;
  boolean reltext;
  boolean relocs;
  boolean plt;

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
  if (info->shared)
    elf_s390_link_hash_traverse (elf_s390_hash_table (info),
				 elf_s390_discard_copies,
				 (PTR) info);

  /* The check_relocs and adjust_dynamic_symbol entry points have
     determined the sizes of the various dynamic sections.  Allocate
     memory for them.  */
  plt = false;
  reltext = false;
  relocs = false;
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
		 output file.  This is to handle .rela.bss and
		 .rel.plt.  We must create it in
		 create_dynamic_sections, because it must be created
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
	 values later, in elf_s390_finish_dynamic_sections, but we
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
	  info->flags |= DF_TEXTREL;
	}
    }

  return true;
}

/* This function is called via elf_s390_link_hash_traverse if we are
   creating a shared object with -Bsymbolic.  It discards the space
   allocated to copy PC relative relocs against symbols which are
   defined in regular objects.  We allocated space for them in the
   check_relocs routine, but we won't fill them in in the
   relocate_section routine.  */

/*ARGSUSED*/
static boolean
elf_s390_discard_copies (h, inf)
     struct elf_s390_link_hash_entry *h;
     PTR inf;
{
  struct elf_s390_pcrel_relocs_copied *s;
  struct bfd_link_info *info = (struct bfd_link_info *) inf;

  /* If a symbol has been forced local or we have found a regular
     definition for the symbolic link case, then we won't be needing
     any relocs.  */
  if ((h->root.elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR) != 0
      && ((h->root.elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) != 0
	  || info->symbolic))
    {
      for (s = h->pcrel_relocs_copied; s != NULL; s = s->next)
	s->section->_raw_size -= s->count * sizeof (Elf32_External_Rela);
    }
  return true;
}
/* Relocate a 390 ELF section.  */

static boolean
elf_s390_relocate_section (output_bfd, info, input_bfd, input_section,
			      contents, relocs, local_syms, local_sections)
     bfd *output_bfd;
     struct bfd_link_info *info;
     bfd *input_bfd;
     asection *input_section;
     bfd_byte *contents;
     Elf_Internal_Rela *relocs;
     Elf_Internal_Sym *local_syms;
     asection **local_sections;
{
  bfd *dynobj;
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  bfd_vma *local_got_offsets;
  asection *sgot;
  asection *splt;
  asection *sreloc;
  Elf_Internal_Rela *rel;
  Elf_Internal_Rela *relend;

  dynobj = elf_hash_table (info)->dynobj;
  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);
  local_got_offsets = elf_local_got_offsets (input_bfd);

  sgot = NULL;
  splt = NULL;
  sreloc = NULL;
  if (dynobj != NULL)
    {
      splt = bfd_get_section_by_name (dynobj, ".plt");
      sgot = bfd_get_section_by_name (dynobj, ".got");
    }

  rel = relocs;
  relend = relocs + input_section->reloc_count;
  for (; rel < relend; rel++)
    {
      int r_type;
      reloc_howto_type *howto;
      unsigned long r_symndx;
      struct elf_link_hash_entry *h;
      Elf_Internal_Sym *sym;
      asection *sec;
      bfd_vma relocation;
      bfd_reloc_status_type r;

      r_type = ELF32_R_TYPE (rel->r_info);
      if (r_type == (int) R_390_GNU_VTINHERIT
          || r_type == (int) R_390_GNU_VTENTRY)
        continue;
      if (r_type < 0 || r_type >= (int) R_390_max)
	{
	  bfd_set_error (bfd_error_bad_value);
	  return false;
	}
      howto = elf_howto_table + r_type;

      r_symndx = ELF32_R_SYM (rel->r_info);

      if (info->relocateable)
	{
	  /* This is a relocateable link.  We don't have to change
	     anything, unless the reloc is against a section symbol,
	     in which case we have to adjust according to where the
	     section symbol winds up in the output section.  */
	  if (r_symndx < symtab_hdr->sh_info)
	    {
	      sym = local_syms + r_symndx;
	      if (ELF_ST_TYPE (sym->st_info) == STT_SECTION)
		{
		  sec = local_sections[r_symndx];
		  rel->r_addend += sec->output_offset + sym->st_value;
		}
	    }

	  continue;
	}

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
	}
      else
	{
	  h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	  while (h->root.type == bfd_link_hash_indirect
		 || h->root.type == bfd_link_hash_warning)
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;
	  if (h->root.type == bfd_link_hash_defined
	      || h->root.type == bfd_link_hash_defweak)
	    {
	      sec = h->root.u.def.section;
	      if (r_type == R_390_GOTPC
		  || ((r_type == R_390_PLT16DBL ||
                       r_type == R_390_PLT32)
		      && splt != NULL
		      && h->plt.offset != (bfd_vma) -1)
		  || ((r_type == R_390_GOT12 ||
                       r_type == R_390_GOT16 ||
                       r_type == R_390_GOT32)
		      && elf_hash_table (info)->dynamic_sections_created
		      && (! info->shared
			  || (! info->symbolic && h->dynindx != -1)
			  || (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) == 0))
		  || (info->shared
		      && ((! info->symbolic && h->dynindx != -1)
			  || (h->elf_link_hash_flags
			      & ELF_LINK_HASH_DEF_REGULAR) == 0)
		      && ( r_type == R_390_8 ||
			   r_type == R_390_16 ||
                           r_type == R_390_32 ||
                           r_type == R_390_PC16 ||
                           r_type == R_390_PC16DBL ||
                           r_type == R_390_PC32)
                      && ((input_section->flags & SEC_ALLOC) != 0
                          /* DWARF will emit R_386_32 relocations in its
                             sections against symbols defined externally
                             in shared libraries.  We can't do anything
                             with them here.  */
                          || ((input_section->flags & SEC_DEBUGGING) != 0
			      && (h->elf_link_hash_flags
				  & ELF_LINK_HASH_DEF_DYNAMIC) != 0))))
		{
		  /* In these cases, we don't need the relocation
                     value.  We check specially because in some
                     obscure cases sec->output_section will be NULL.  */
		  relocation = 0;
		}
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
	  else if (info->shared && !info->symbolic
		   && !info->no_undefined
		   && ELF_ST_VISIBILITY (h->other) == STV_DEFAULT)
	    relocation = 0;
	  else
	    {
	      if (! ((*info->callbacks->undefined_symbol)
		     (info, h->root.root.string, input_bfd,
          	      input_section, rel->r_offset,
	 	     (!info->shared || info->no_undefined
		      || ELF_ST_VISIBILITY (h->other)))))
		return false;
	      relocation = 0;
	    }
	}

      switch (r_type)
	{
        case R_390_GOT12:
        case R_390_GOT16:
        case R_390_GOT32:
          /* Relocation is to the entry for this symbol in the global
             offset table.  */
	  BFD_ASSERT (sgot != NULL);

          if (h != NULL)
            {
              bfd_vma off;

              off = h->got.offset;
              BFD_ASSERT (off != (bfd_vma) -1);

              if (! elf_hash_table (info)->dynamic_sections_created
                  || (info->shared
                      && (info->symbolic || h->dynindx == -1)
                      && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR)))
                {
                  /* This is actually a static link, or it is a
                     -Bsymbolic link and the symbol is defined
                     locally, or the symbol was forced to be local
                     because of a version file.  We must initialize
                     this entry in the global offset table.  Since the
                     offset must always be a multiple of 2, we use the
                     least significant bit to record whether we have
                     initialized it already.

                     When doing a dynamic link, we create a .rel.got
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
                      outrel.r_info = ELF32_R_INFO (0, R_390_RELATIVE);
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


          break;
 
        case R_390_GOTOFF:
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

          break;

        case R_390_GOTPC:
          /* Use global offset table as symbol value.  */

          if (sgot == NULL)
            {
              sgot = bfd_get_section_by_name (dynobj, ".got");
              BFD_ASSERT (sgot != NULL);
            }

          relocation = sgot->output_section->vma;

          break;

        case R_390_PLT16DBL:
        case R_390_PLT32:
          /* Relocation is to the entry for this symbol in the
             procedure linkage table.  */

          /* Resolve a PLT32 reloc against a local symbol directly,
             without using the procedure linkage table.  */
          if (h == NULL)
            break;

          if (h->plt.offset == (bfd_vma) -1 || splt == NULL)
            {
              /* We didn't make a PLT entry for this symbol.  This
                 happens when statically linking PIC code, or when
                 using -Bsymbolic.  */
              break;
            }

          relocation = (splt->output_section->vma
                        + splt->output_offset
                        + h->plt.offset);

          break;

        case R_390_8:
        case R_390_16:
        case R_390_32:
        case R_390_PC16:
        case R_390_PC16DBL:
        case R_390_PC32:
          if (info->shared
              && (input_section->flags & SEC_ALLOC) != 0
              && ((r_type != R_390_PC16 &&
                   r_type != R_390_PC16DBL &&
                   r_type != R_390_PC32)
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
              else if (r_type == R_390_PC16 ||
                       r_type == R_390_PC16DBL ||
                       r_type == R_390_PC32)
                {
                  BFD_ASSERT (h != NULL && h->dynindx != -1);
		  relocate = false;
                  outrel.r_info = ELF32_R_INFO (h->dynindx, r_type);
		  outrel.r_addend = relocation + rel->r_addend;
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
                      outrel.r_info = ELF32_R_INFO (0, R_390_RELATIVE);
		      outrel.r_addend = relocation + rel->r_addend;
                    }
		  else
		    {
		      BFD_ASSERT (h->dynindx != -1);
		      relocate = false;
		      outrel.r_info = ELF32_R_INFO (h->dynindx, R_390_32);
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

          break;

        default:
          break;
        }

      r = _bfd_final_link_relocate (howto, input_bfd, input_section,
				      contents, rel->r_offset,
				      relocation, rel->r_addend);

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
		    name = bfd_elf_string_from_elf_section (input_bfd,
							    symtab_hdr->sh_link,
							    sym->st_name);
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

/* Finish up dynamic symbol handling.  We set the contents of various
   dynamic sections here.  */

static boolean
elf_s390_finish_dynamic_symbol (output_bfd, info, h, sym)
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
      asection *srela;
      Elf_Internal_Rela rela;
      bfd_vma relative_offset;
      bfd_vma got_offset;
      bfd_vma plt_index;
      asection *sgot;

      /* This symbol has an entry in the procedure linkage table.  Set
         it up.  */

      BFD_ASSERT (h->dynindx != -1);

      splt = bfd_get_section_by_name (dynobj, ".plt");
      sgot = bfd_get_section_by_name (dynobj, ".got.plt");
      srela = bfd_get_section_by_name (dynobj, ".rela.plt");
      BFD_ASSERT (splt != NULL && sgot != NULL && srela != NULL);

      /* Calc. index no. 
         Current offset - size first entry / entry size.  */
      plt_index = (h->plt.offset - PLT_FIRST_ENTRY_SIZE) / PLT_ENTRY_SIZE;

      /* Offset in GOT is PLT index plus GOT headers(3) times 4,
         addr & GOT addr.  */
      got_offset = (plt_index + 3) * GOT_ENTRY_SIZE;

      /* S390 uses halfwords for relative branch calc!  */
      relative_offset = - ((PLT_FIRST_ENTRY_SIZE + 
                           (PLT_ENTRY_SIZE * plt_index) + 18)/2);
      /* If offset is > 32768, branch to a previous branch
         390 can only handle +-64 K jumps.  */
      if ( -32768 > (int)relative_offset )
          relative_offset = -(((65536/PLT_ENTRY_SIZE-1)*PLT_ENTRY_SIZE)/2);

      /* Fill in the entry in the procedure linkage table.  */
      if (!info->shared)
       {
        bfd_put_32 (output_bfd, PLT_ENTRY_WORD0,
                    splt->contents + h->plt.offset);
        bfd_put_32 (output_bfd, PLT_ENTRY_WORD1,
                    splt->contents + h->plt.offset + 4);
        bfd_put_32 (output_bfd, PLT_ENTRY_WORD2,
                    splt->contents + h->plt.offset + 8);
        bfd_put_32 (output_bfd, PLT_ENTRY_WORD3,
                    splt->contents + h->plt.offset + 12);
        bfd_put_32 (output_bfd, PLT_ENTRY_WORD4,
                    splt->contents + h->plt.offset + 16);
        bfd_put_32 (output_bfd, 0+(relative_offset << 16),
                    splt->contents + h->plt.offset + 20);
        bfd_put_32 (output_bfd,
                    (sgot->output_section->vma +
                     sgot->output_offset +
                     got_offset),
                     splt->contents + h->plt.offset + 24);
       }
      else if (got_offset < 4096)
       {
        bfd_put_32 (output_bfd, PLT_PIC12_ENTRY_WORD0 + got_offset,
                    splt->contents + h->plt.offset);
        bfd_put_32 (output_bfd, PLT_PIC12_ENTRY_WORD1,
                    splt->contents + h->plt.offset + 4);
        bfd_put_32 (output_bfd, PLT_PIC12_ENTRY_WORD2,
                    splt->contents + h->plt.offset + 8);
        bfd_put_32 (output_bfd, PLT_PIC12_ENTRY_WORD3,
                    splt->contents + h->plt.offset + 12);
        bfd_put_32 (output_bfd, PLT_PIC12_ENTRY_WORD4,
                    splt->contents + h->plt.offset + 16);
        bfd_put_32 (output_bfd, 0+(relative_offset << 16),
                    splt->contents + h->plt.offset + 20);
        bfd_put_32 (output_bfd, 0,
                    splt->contents + h->plt.offset + 24);
       }
      else if (got_offset < 32768)
       {
        bfd_put_32 (output_bfd, PLT_PIC16_ENTRY_WORD0 + got_offset,
                    splt->contents + h->plt.offset);
        bfd_put_32 (output_bfd, PLT_PIC16_ENTRY_WORD1,
                    splt->contents + h->plt.offset + 4);
        bfd_put_32 (output_bfd, PLT_PIC16_ENTRY_WORD2,
                    splt->contents + h->plt.offset + 8);
        bfd_put_32 (output_bfd, PLT_PIC16_ENTRY_WORD3,
                    splt->contents + h->plt.offset + 12);
        bfd_put_32 (output_bfd, PLT_PIC16_ENTRY_WORD4,
                    splt->contents + h->plt.offset + 16);
        bfd_put_32 (output_bfd, 0+(relative_offset << 16),
                    splt->contents + h->plt.offset + 20);
        bfd_put_32 (output_bfd, 0,
                    splt->contents + h->plt.offset + 24);
       }
      else
       {
        bfd_put_32 (output_bfd, PLT_PIC_ENTRY_WORD0,
		    splt->contents + h->plt.offset);
        bfd_put_32 (output_bfd, PLT_PIC_ENTRY_WORD1,
		    splt->contents + h->plt.offset + 4);
        bfd_put_32 (output_bfd, PLT_PIC_ENTRY_WORD2,
		    splt->contents + h->plt.offset + 8);
        bfd_put_32 (output_bfd, PLT_PIC_ENTRY_WORD3, 
		    splt->contents + h->plt.offset + 12);
        bfd_put_32 (output_bfd, PLT_PIC_ENTRY_WORD4,
		    splt->contents + h->plt.offset + 16);
        bfd_put_32 (output_bfd, 0+(relative_offset << 16),
		    splt->contents + h->plt.offset + 20);
        bfd_put_32 (output_bfd, got_offset,
		    splt->contents + h->plt.offset + 24);
       }
      /* Insert offset into  reloc. table here.  */
      bfd_put_32 (output_bfd, plt_index * sizeof (Elf32_External_Rela),
                  splt->contents + h->plt.offset + 28);
      /* Fill in the entry in the .rela.plt section.  */
      rela.r_offset = (sgot->output_section->vma
		       + sgot->output_offset
		       + got_offset);
      rela.r_info = ELF32_R_INFO (h->dynindx, R_390_JMP_SLOT);
      rela.r_addend = 0;
      bfd_elf32_swap_reloca_out (output_bfd, &rela,
				 ((Elf32_External_Rela *) srela->contents
				  + plt_index ));

      /* Fill in the entry in the global offset table.
         Points to instruction after GOT offset.  */
      bfd_put_32 (output_bfd,
		  (splt->output_section->vma
		   + splt->output_offset
		   + h->plt.offset
		   + 12),
		  sgot->contents + got_offset);


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
      asection *srela;
      Elf_Internal_Rela rela;

      /* This symbol has an entry in the global offset table.  Set it
         up.  */

      sgot = bfd_get_section_by_name (dynobj, ".got");
      srela = bfd_get_section_by_name (dynobj, ".rela.got");
      BFD_ASSERT (sgot != NULL && srela != NULL);

      rela.r_offset = (sgot->output_section->vma
		       + sgot->output_offset
		       + (h->got.offset &~ 1));

      /* If this is a static link, or it is a -Bsymbolic link and the
	 symbol is defined locally or was forced to be local because
	 of a version file, we just want to emit a RELATIVE reloc.
	 The entry in the global offset table will already have been
	 initialized in the relocate_section function.  */
      if (! elf_hash_table (info)->dynamic_sections_created
	  || (info->shared
	      && (info->symbolic || h->dynindx == -1)
	      && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR)))
        {
          rela.r_info = ELF32_R_INFO (0, R_390_RELATIVE);
          rela.r_addend = (h->root.u.def.value
                           + h->root.u.def.section->output_section->vma
                           + h->root.u.def.section->output_offset);
        }
      else
	{
	  BFD_ASSERT((h->got.offset & 1) == 0);
	  bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents + h->got.offset);
	  rela.r_info = ELF32_R_INFO (h->dynindx, R_390_GLOB_DAT);
          rela.r_addend = 0;
        }

      bfd_elf32_swap_reloca_out (output_bfd, &rela,
				 ((Elf32_External_Rela *) srela->contents
				  + srela->reloc_count));
      ++srela->reloc_count;
    }

  if ((h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_COPY) != 0)
    {
      asection *s;
      Elf_Internal_Rela rela;

      /* This symbols needs a copy reloc.  Set it up.  */

      BFD_ASSERT (h->dynindx != -1
		  && (h->root.type == bfd_link_hash_defined
		      || h->root.type == bfd_link_hash_defweak));


      s = bfd_get_section_by_name (h->root.u.def.section->owner,
				   ".rela.bss");
      BFD_ASSERT (s != NULL);

      rela.r_offset = (h->root.u.def.value
		       + h->root.u.def.section->output_section->vma
		       + h->root.u.def.section->output_offset);
      rela.r_info = ELF32_R_INFO (h->dynindx, R_390_COPY);
      rela.r_addend = 0;
      bfd_elf32_swap_reloca_out (output_bfd, &rela,
				 ((Elf32_External_Rela *) s->contents
				  + s->reloc_count));
      ++s->reloc_count;
    }

  /* Mark some specially defined symbols as absolute.  */
  if (strcmp (h->root.root.string, "_DYNAMIC") == 0
      || strcmp (h->root.root.string, "_GLOBAL_OFFSET_TABLE_") == 0
      || strcmp (h->root.root.string, "_PROCEDURE_LINKAGE_TABLE_") == 0)
    sym->st_shndx = SHN_ABS;

  return true;
}

/* Finish up the dynamic sections.  */

static boolean
elf_s390_finish_dynamic_sections (output_bfd, info)
     bfd *output_bfd;
     struct bfd_link_info *info;
{
  bfd *dynobj;
  asection *sdyn;
  asection *sgot;

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
	      s = bfd_get_section_by_name(output_bfd, name);
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
	    }
	}

      /* Fill in the special first entry in the procedure linkage table.  */
      splt = bfd_get_section_by_name (dynobj, ".plt");
      if (splt && splt->_raw_size > 0)
	{
          memset (splt->contents, 0, PLT_FIRST_ENTRY_SIZE);
          if (info->shared)
           {
	      bfd_put_32 (output_bfd, PLT_PIC_FIRST_ENTRY_WORD0,
		          splt->contents );
	      bfd_put_32 (output_bfd, PLT_PIC_FIRST_ENTRY_WORD1,
		          splt->contents +4 );
	      bfd_put_32 (output_bfd, PLT_PIC_FIRST_ENTRY_WORD2,
		          splt->contents +8 );
	      bfd_put_32 (output_bfd, PLT_PIC_FIRST_ENTRY_WORD3,
		          splt->contents +12 );
	      bfd_put_32 (output_bfd, PLT_PIC_FIRST_ENTRY_WORD4,
		          splt->contents +16 );
           }
          else
           {
              bfd_put_32 (output_bfd, PLT_FIRST_ENTRY_WORD0,
                          splt->contents );
              bfd_put_32 (output_bfd, PLT_FIRST_ENTRY_WORD1,
                          splt->contents +4 );
              bfd_put_32 (output_bfd, PLT_FIRST_ENTRY_WORD2,
                          splt->contents +8 );
              bfd_put_32 (output_bfd, PLT_FIRST_ENTRY_WORD3,
                          splt->contents +12 );
              bfd_put_32 (output_bfd, PLT_FIRST_ENTRY_WORD4,
                          splt->contents +16 );
              bfd_put_32 (output_bfd, PLT_FIRST_ENTRY_WORD5,
                          splt->contents +20 );
              bfd_put_32 (output_bfd,
                          sgot->output_section->vma + sgot->output_offset,
                          splt->contents + 24);
           }
	  elf_section_data (splt->output_section)->this_hdr.sh_entsize = 4;
	}

    }

  /* Set the first entry in the global offset table to the address of
     the dynamic section.  */
  if (sgot->_raw_size > 0)
    {
      if (sdyn == NULL)
	bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents);
      else
	bfd_put_32 (output_bfd,
		    sdyn->output_section->vma + sdyn->output_offset,
		    sgot->contents);

      /* One entry for shared object struct ptr.  */
      bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents + 4);
      /* One entry for _dl_runtime_resolve.  */
      bfd_put_32 (output_bfd, (bfd_vma) 0, sgot->contents + 8);
    }

  elf_section_data (sgot->output_section)->this_hdr.sh_entsize = 4;

  return true;
}

static boolean
elf_s390_object_p (abfd)
     bfd *abfd;
{
  return bfd_default_set_arch_mach (abfd, bfd_arch_s390, bfd_mach_s390_esa);
}

#define TARGET_BIG_SYM	bfd_elf32_s390_vec
#define TARGET_BIG_NAME	"elf32-s390"
#define ELF_ARCH	bfd_arch_s390
#define ELF_MACHINE_CODE EM_S390
#define ELF_MACHINE_ALT1 EM_S390_OLD
#define ELF_MAXPAGESIZE 0x1000

#define elf_backend_can_gc_sections	1
#define elf_backend_want_got_plt	1
#define elf_backend_plt_readonly	1
#define elf_backend_want_plt_sym	0
#define elf_backend_got_header_size	12
#define elf_backend_plt_header_size	PLT_ENTRY_SIZE

#define elf_info_to_howto                     elf_s390_info_to_howto

#define bfd_elf32_bfd_final_link	      _bfd_elf32_gc_common_final_link
#define bfd_elf32_bfd_is_local_label_name     elf_s390_is_local_label_name
#define bfd_elf32_bfd_link_hash_table_create  elf_s390_link_hash_table_create
#define bfd_elf32_bfd_reloc_type_lookup	      elf_s390_reloc_type_lookup

#define elf_backend_adjust_dynamic_symbol     elf_s390_adjust_dynamic_symbol
#define elf_backend_check_relocs              elf_s390_check_relocs
#define elf_backend_create_dynamic_sections   _bfd_elf_create_dynamic_sections
#define elf_backend_finish_dynamic_sections   elf_s390_finish_dynamic_sections
#define elf_backend_finish_dynamic_symbol     elf_s390_finish_dynamic_symbol
#define elf_backend_gc_mark_hook              elf_s390_gc_mark_hook
#define elf_backend_gc_sweep_hook             elf_s390_gc_sweep_hook
#define elf_backend_relocate_section          elf_s390_relocate_section
#define elf_backend_size_dynamic_sections     elf_s390_size_dynamic_sections

#define elf_backend_object_p            elf_s390_object_p

#include "elf32-target.h"
