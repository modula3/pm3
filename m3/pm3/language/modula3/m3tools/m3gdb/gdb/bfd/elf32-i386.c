/* Intel 80386/80486-specific support for 32-bit ELF
   Copyright 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.

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

static reloc_howto_type *elf_i386_reloc_type_lookup
  PARAMS ((bfd *, bfd_reloc_code_real_type));
static void elf_i386_info_to_howto
  PARAMS ((bfd *, arelent *, Elf32_Internal_Rela *));
static void elf_i386_info_to_howto_rel
  PARAMS ((bfd *, arelent *, Elf32_Internal_Rel *));
static boolean elf_i386_is_local_label_name PARAMS ((bfd *, const char *));
static struct bfd_hash_entry *elf_i386_link_hash_newfunc
  PARAMS ((struct bfd_hash_entry *, struct bfd_hash_table *, const char *));
static struct bfd_link_hash_table *elf_i386_link_hash_table_create
  PARAMS ((bfd *));
static boolean create_got_section PARAMS((bfd *, struct bfd_link_info *));
static boolean elf_i386_create_dynamic_sections
  PARAMS((bfd *, struct bfd_link_info *));
static boolean elf_i386_check_relocs
  PARAMS ((bfd *, struct bfd_link_info *, asection *,
	   const Elf_Internal_Rela *));
static boolean elf_i386_adjust_dynamic_symbol
  PARAMS ((struct bfd_link_info *, struct elf_link_hash_entry *));
static boolean allocate_plt_and_got_and_discard_relocs
  PARAMS ((struct elf_link_hash_entry *, PTR));
static boolean elf_i386_size_dynamic_sections
  PARAMS ((bfd *, struct bfd_link_info *));
static boolean elf_i386_relocate_section
  PARAMS ((bfd *, struct bfd_link_info *, bfd *, asection *, bfd_byte *,
	   Elf_Internal_Rela *, Elf_Internal_Sym *, asection **));
static boolean elf_i386_finish_dynamic_symbol
  PARAMS ((bfd *, struct bfd_link_info *, struct elf_link_hash_entry *,
	   Elf_Internal_Sym *));
static boolean elf_i386_finish_dynamic_sections
  PARAMS ((bfd *, struct bfd_link_info *));

#define USE_REL	1		/* 386 uses REL relocations instead of RELA */

#include "elf/i386.h"

static reloc_howto_type elf_howto_table[]=
{
  HOWTO(R_386_NONE, 0, 0, 0, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_NONE",
	true, 0x00000000, 0x00000000, false),
  HOWTO(R_386_32, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_32",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_PC32, 0, 2, 32, true, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_PC32",
	true, 0xffffffff, 0xffffffff, true),
  HOWTO(R_386_GOT32, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_GOT32",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_PLT32, 0, 2, 32, true, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_PLT32",
	true, 0xffffffff, 0xffffffff, true),
  HOWTO(R_386_COPY, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_COPY",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_GLOB_DAT, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_GLOB_DAT",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_JUMP_SLOT, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_JUMP_SLOT",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_RELATIVE, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_RELATIVE",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_GOTOFF, 0, 2, 32, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_GOTOFF",
	true, 0xffffffff, 0xffffffff, false),
  HOWTO(R_386_GOTPC, 0, 2, 32, true, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_GOTPC",
	true, 0xffffffff, 0xffffffff, true),

  /* We have a gap in the reloc numbers here.
     R_386_standard counts the number up to this point, and
     R_386_ext_offset is the value to subtract from a reloc type of
     R_386_16 thru R_386_PC8 to form an index into this table.  */
#define R_386_standard ((unsigned int) R_386_GOTPC + 1)
#define R_386_ext_offset ((unsigned int) R_386_16 - R_386_standard)

  /* The remaining relocs are a GNU extension.  */
  HOWTO(R_386_16, 0, 1, 16, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_16",
	true, 0xffff, 0xffff, false),
  HOWTO(R_386_PC16, 0, 1, 16, true, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_PC16",
	true, 0xffff, 0xffff, true),
  HOWTO(R_386_8, 0, 0, 8, false, 0, complain_overflow_bitfield,
	bfd_elf_generic_reloc, "R_386_8",
	true, 0xff, 0xff, false),
  HOWTO(R_386_PC8, 0, 0, 8, true, 0, complain_overflow_signed,
	bfd_elf_generic_reloc, "R_386_PC8",
	true, 0xff, 0xff, true),

  /* Another gap.  */
#define R_386_ext ((unsigned int) R_386_PC8 + 1 - R_386_ext_offset)
#define R_386_vt_offset ((unsigned int) R_386_GNU_VTINHERIT - R_386_ext)

/* GNU extension to record C++ vtable hierarchy.  */
  HOWTO (R_386_GNU_VTINHERIT,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont, /* complain_on_overflow */
	 NULL,			/* special_function */
	 "R_386_GNU_VTINHERIT",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),

/* GNU extension to record C++ vtable member usage.  */
  HOWTO (R_386_GNU_VTENTRY,	/* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont, /* complain_on_overflow */
	 _bfd_elf_rel_vtable_reloc_fn, /* special_function */
	 "R_386_GNU_VTENTRY",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false)

#define R_386_vt ((unsigned int) R_386_GNU_VTENTRY + 1 - R_386_vt_offset)

};

#ifdef DEBUG_GEN_RELOC
#define TRACE(str) fprintf (stderr, "i386 bfd reloc lookup %d (%s)\n", code, str)
#else
#define TRACE(str)
#endif

static reloc_howto_type *
elf_i386_reloc_type_lookup (abfd, code)
     bfd *abfd ATTRIBUTE_UNUSED;
     bfd_reloc_code_real_type code;
{
  switch (code)
    {
    case BFD_RELOC_NONE:
      TRACE ("BFD_RELOC_NONE");
      return &elf_howto_table[(unsigned int) R_386_NONE ];

    case BFD_RELOC_32:
      TRACE ("BFD_RELOC_32");
      return &elf_howto_table[(unsigned int) R_386_32 ];

    case BFD_RELOC_CTOR:
      TRACE ("BFD_RELOC_CTOR");
      return &elf_howto_table[(unsigned int) R_386_32 ];

    case BFD_RELOC_32_PCREL:
      TRACE ("BFD_RELOC_PC32");
      return &elf_howto_table[(unsigned int) R_386_PC32 ];

    case BFD_RELOC_386_GOT32:
      TRACE ("BFD_RELOC_386_GOT32");
      return &elf_howto_table[(unsigned int) R_386_GOT32 ];

    case BFD_RELOC_386_PLT32:
      TRACE ("BFD_RELOC_386_PLT32");
      return &elf_howto_table[(unsigned int) R_386_PLT32 ];

    case BFD_RELOC_386_COPY:
      TRACE ("BFD_RELOC_386_COPY");
      return &elf_howto_table[(unsigned int) R_386_COPY ];

    case BFD_RELOC_386_GLOB_DAT:
      TRACE ("BFD_RELOC_386_GLOB_DAT");
      return &elf_howto_table[(unsigned int) R_386_GLOB_DAT ];

    case BFD_RELOC_386_JUMP_SLOT:
      TRACE ("BFD_RELOC_386_JUMP_SLOT");
      return &elf_howto_table[(unsigned int) R_386_JUMP_SLOT ];

    case BFD_RELOC_386_RELATIVE:
      TRACE ("BFD_RELOC_386_RELATIVE");
      return &elf_howto_table[(unsigned int) R_386_RELATIVE ];

    case BFD_RELOC_386_GOTOFF:
      TRACE ("BFD_RELOC_386_GOTOFF");
      return &elf_howto_table[(unsigned int) R_386_GOTOFF ];

    case BFD_RELOC_386_GOTPC:
      TRACE ("BFD_RELOC_386_GOTPC");
      return &elf_howto_table[(unsigned int) R_386_GOTPC ];

      /* The remaining relocs are a GNU extension.  */
    case BFD_RELOC_16:
      TRACE ("BFD_RELOC_16");
      return &elf_howto_table[(unsigned int) R_386_16 - R_386_ext_offset];

    case BFD_RELOC_16_PCREL:
      TRACE ("BFD_RELOC_16_PCREL");
      return &elf_howto_table[(unsigned int) R_386_PC16 - R_386_ext_offset];

    case BFD_RELOC_8:
      TRACE ("BFD_RELOC_8");
      return &elf_howto_table[(unsigned int) R_386_8 - R_386_ext_offset];

    case BFD_RELOC_8_PCREL:
      TRACE ("BFD_RELOC_8_PCREL");
      return &elf_howto_table[(unsigned int) R_386_PC8 - R_386_ext_offset];

    case BFD_RELOC_VTABLE_INHERIT:
      TRACE ("BFD_RELOC_VTABLE_INHERIT");
      return &elf_howto_table[(unsigned int) R_386_GNU_VTINHERIT
			     - R_386_vt_offset];

    case BFD_RELOC_VTABLE_ENTRY:
      TRACE ("BFD_RELOC_VTABLE_ENTRY");
      return &elf_howto_table[(unsigned int) R_386_GNU_VTENTRY
			     - R_386_vt_offset];

    default:
      break;
    }

  TRACE ("Unknown");
  return 0;
}

static void
elf_i386_info_to_howto (abfd, cache_ptr, dst)
     bfd		*abfd ATTRIBUTE_UNUSED;
     arelent		*cache_ptr ATTRIBUTE_UNUSED;
     Elf32_Internal_Rela *dst ATTRIBUTE_UNUSED;
{
  abort ();
}

static void
elf_i386_info_to_howto_rel (abfd, cache_ptr, dst)
     bfd *abfd ATTRIBUTE_UNUSED;
     arelent *cache_ptr;
     Elf32_Internal_Rel *dst;
{
  unsigned int r_type = ELF32_R_TYPE (dst->r_info);
  unsigned int indx;

  if ((indx = r_type) >= R_386_standard
      && ((indx = r_type - R_386_ext_offset) - R_386_standard
	  >= R_386_ext - R_386_standard)
      && ((indx = r_type - R_386_vt_offset) - R_386_ext
	  >= R_386_vt - R_386_ext))
    {
      (*_bfd_error_handler) (_("%s: invalid relocation type %d"),
			     bfd_get_filename (abfd), (int) r_type);
      indx = (unsigned int) R_386_NONE;
    }
  cache_ptr->howto = &elf_howto_table[indx];
}

/* Return whether a symbol name implies a local label.  The UnixWare
   2.1 cc generates temporary symbols that start with .X, so we
   recognize them here.  FIXME: do other SVR4 compilers also use .X?.
   If so, we should move the .X recognition into
   _bfd_elf_is_local_label_name.  */

static boolean
elf_i386_is_local_label_name (abfd, name)
     bfd *abfd;
     const char *name;
{
  if (name[0] == '.' && name[1] == 'X')
    return true;

  return _bfd_elf_is_local_label_name (abfd, name);
}

/* Functions for the i386 ELF linker.  */

/* The name of the dynamic interpreter.  This is put in the .interp
   section.  */

#define ELF_DYNAMIC_INTERPRETER "/usr/lib/libc.so.1"

/* The size in bytes of an entry in the procedure linkage table.  */

#define PLT_ENTRY_SIZE 16

/* The first entry in an absolute procedure linkage table looks like
   this.  See the SVR4 ABI i386 supplement to see how this works.  */

static const bfd_byte elf_i386_plt0_entry[PLT_ENTRY_SIZE] =
{
  0xff, 0x35,	/* pushl contents of address */
  0, 0, 0, 0,	/* replaced with address of .got + 4.  */
  0xff, 0x25,	/* jmp indirect */
  0, 0, 0, 0,	/* replaced with address of .got + 8.  */
  0, 0, 0, 0	/* pad out to 16 bytes.  */
};

/* Subsequent entries in an absolute procedure linkage table look like
   this.  */

static const bfd_byte elf_i386_plt_entry[PLT_ENTRY_SIZE] =
{
  0xff, 0x25,	/* jmp indirect */
  0, 0, 0, 0,	/* replaced with address of this symbol in .got.  */
  0x68,		/* pushl immediate */
  0, 0, 0, 0,	/* replaced with offset into relocation table.  */
  0xe9,		/* jmp relative */
  0, 0, 0, 0	/* replaced with offset to start of .plt.  */
};

/* The first entry in a PIC procedure linkage table look like this.  */

static const bfd_byte elf_i386_pic_plt0_entry[PLT_ENTRY_SIZE] =
{
  0xff, 0xb3, 4, 0, 0, 0,	/* pushl 4(%ebx) */
  0xff, 0xa3, 8, 0, 0, 0,	/* jmp *8(%ebx) */
  0, 0, 0, 0			/* pad out to 16 bytes.  */
};

/* Subsequent entries in a PIC procedure linkage table look like this.  */

static const bfd_byte elf_i386_pic_plt_entry[PLT_ENTRY_SIZE] =
{
  0xff, 0xa3,	/* jmp *offset(%ebx) */
  0, 0, 0, 0,	/* replaced with offset of this symbol in .got.  */
  0x68,		/* pushl immediate */
  0, 0, 0, 0,	/* replaced with offset into relocation table.  */
  0xe9,		/* jmp relative */
  0, 0, 0, 0	/* replaced with offset to start of .plt.  */
};

/* The i386 linker needs to keep track of the number of relocs that it
   decides to copy as dynamic relocs in check_relocs for each symbol.
   This is so that it can later discard them if they are found to be
   unnecessary.  We store the information in a field extending the
   regular ELF linker hash table.  */

struct elf_i386_dyn_relocs
{
  /* Next section.  */
  struct elf_i386_dyn_relocs *next;
  /* A section in dynobj.  */
  asection *section;
  /* Number of relocs copied in this section.  */
  bfd_size_type count;
};

/* i386 ELF linker hash entry.  */

struct elf_i386_link_hash_entry
{
  struct elf_link_hash_entry root;

  /* Number of PC relative relocs copied for this symbol.  */
  struct elf_i386_dyn_relocs *dyn_relocs;
};

/* i386 ELF linker hash table.  */

struct elf_i386_link_hash_table
{
  struct elf_link_hash_table root;

  /* Short-cuts to get to dynamic linker sections.  */
  asection *sgot;
  asection *sgotplt;
  asection *srelgot;
  asection *splt;
  asection *srelplt;
  asection *sdynbss;
  asection *srelbss;
};

/* Get the i386 ELF linker hash table from a link_info structure.  */

#define elf_i386_hash_table(p) \
  ((struct elf_i386_link_hash_table *) ((p)->hash))

/* Create an entry in an i386 ELF linker hash table.  */

static struct bfd_hash_entry *
elf_i386_link_hash_newfunc (entry, table, string)
     struct bfd_hash_entry *entry;
     struct bfd_hash_table *table;
     const char *string;
{
  struct elf_i386_link_hash_entry *ret =
    (struct elf_i386_link_hash_entry *) entry;

  /* Allocate the structure if it has not already been allocated by a
     subclass.  */
  if (ret == (struct elf_i386_link_hash_entry *) NULL)
    ret = ((struct elf_i386_link_hash_entry *)
	   bfd_hash_allocate (table,
			      sizeof (struct elf_i386_link_hash_entry)));
  if (ret == (struct elf_i386_link_hash_entry *) NULL)
    return (struct bfd_hash_entry *) ret;

  /* Call the allocation method of the superclass.  */
  ret = ((struct elf_i386_link_hash_entry *)
	 _bfd_elf_link_hash_newfunc ((struct bfd_hash_entry *) ret,
				     table, string));
  if (ret != (struct elf_i386_link_hash_entry *) NULL)
    {
      ret->dyn_relocs = NULL;
    }

  return (struct bfd_hash_entry *) ret;
}

/* Create an i386 ELF linker hash table.  */

static struct bfd_link_hash_table *
elf_i386_link_hash_table_create (abfd)
     bfd *abfd;
{
  struct elf_i386_link_hash_table *ret;

  ret = ((struct elf_i386_link_hash_table *)
	 bfd_alloc (abfd, sizeof (struct elf_i386_link_hash_table)));
  if (ret == (struct elf_i386_link_hash_table *) NULL)
    return NULL;

  if (! _bfd_elf_link_hash_table_init (&ret->root, abfd,
				       elf_i386_link_hash_newfunc))
    {
      bfd_release (abfd, ret);
      return NULL;
    }

  ret->sgot = NULL;
  ret->sgotplt = NULL;
  ret->srelgot = NULL;
  ret->splt = NULL;
  ret->srelplt = NULL;
  ret->sdynbss = NULL;
  ret->srelbss = NULL;

  return &ret->root.root;
}

/* Create .got, .gotplt, and .rel.got sections in DYNOBJ, and set up
   shortcuts to them in our hash table.  */

static boolean
create_got_section (dynobj, info)
     bfd *dynobj;
     struct bfd_link_info *info;
{
  struct elf_i386_link_hash_table *htab;

  if (! _bfd_elf_create_got_section (dynobj, info))
    return false;

  htab = elf_i386_hash_table (info);
  htab->sgot = bfd_get_section_by_name (dynobj, ".got");
  htab->sgotplt = bfd_get_section_by_name (dynobj, ".got.plt");
  if (!htab->sgot || !htab->sgotplt)
    abort ();

  htab->srelgot = bfd_make_section (dynobj, ".rel.got");
  if (htab->srelgot == NULL
      || ! bfd_set_section_flags (dynobj, htab->srelgot,
				  (SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS
				   | SEC_IN_MEMORY | SEC_LINKER_CREATED
				   | SEC_READONLY))
      || ! bfd_set_section_alignment (dynobj, htab->srelgot, 2))
    return false;
  return true;
}

/* Create .plt, .rel.plt, .got, .got.plt, .rel.got, .dynbss, and
   .rel.bss sections in DYNOBJ, and set up shortcuts to them in our
   hash table.  */

static boolean
elf_i386_create_dynamic_sections (dynobj, info)
     bfd *dynobj;
     struct bfd_link_info *info;
{
  struct elf_i386_link_hash_table *htab;

  htab = elf_i386_hash_table (info);
  if (!htab->sgot && !create_got_section (dynobj, info))
    return false;

  if (!_bfd_elf_create_dynamic_sections (dynobj, info))
    return false;

  htab->splt = bfd_get_section_by_name (dynobj, ".plt");
  htab->srelplt = bfd_get_section_by_name (dynobj, ".rel.plt");
  htab->sdynbss = bfd_get_section_by_name (dynobj, ".dynbss");
  if (!info->shared)
    htab->srelbss = bfd_get_section_by_name (dynobj, ".rel.bss");

  if (!htab->splt || !htab->srelplt || !htab->sdynbss
      || (!info->shared && !htab->srelbss))
    abort ();

  return true;
}

/* Look through the relocs for a section during the first phase, and
   allocate space in the global offset table or procedure linkage
   table.  */

static boolean
elf_i386_check_relocs (abfd, info, sec, relocs)
     bfd *abfd;
     struct bfd_link_info *info;
     asection *sec;
     const Elf_Internal_Rela *relocs;
{
  struct elf_i386_link_hash_table *htab;
  bfd *dynobj;
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  bfd_signed_vma *local_got_refcounts;
  const Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *rel_end;
  asection *sreloc;

  if (info->relocateable)
    return true;

  htab = elf_i386_hash_table (info);
  dynobj = htab->root.dynobj;
  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  local_got_refcounts = elf_local_got_refcounts (abfd);

  sreloc = NULL;

  rel_end = relocs + sec->reloc_count;
  for (rel = relocs; rel < rel_end; rel++)
    {
      unsigned long r_symndx;
      struct elf_link_hash_entry *h;

      r_symndx = ELF32_R_SYM (rel->r_info);

      if (r_symndx >= NUM_SHDR_ENTRIES (symtab_hdr))
	{
	  if (abfd->my_archive)
	    (*_bfd_error_handler) (_("%s(%s): bad symbol index: %d"),
				   bfd_get_filename (abfd->my_archive),
				   bfd_get_filename (abfd),
				   r_symndx);
	  else
	    (*_bfd_error_handler) (_("%s: bad symbol index: %d"),
				   bfd_get_filename (abfd),
				   r_symndx);
	  return false;
	}

      if (r_symndx < symtab_hdr->sh_info)
	h = NULL;
      else
	h = sym_hashes[r_symndx - symtab_hdr->sh_info];

      /* Some relocs require a global offset table.  */
      if (htab->sgot == NULL)
	{
	  switch (ELF32_R_TYPE (rel->r_info))
	    {
	    case R_386_GOT32:
	    case R_386_GOTOFF:
	    case R_386_GOTPC:
	      if (dynobj == NULL)
		htab->root.dynobj = dynobj = abfd;
	      if (!create_got_section (dynobj, info))
		return false;
	      break;

	    default:
	      break;
	    }
	}

      switch (ELF32_R_TYPE (rel->r_info))
	{
	case R_386_GOT32:
	  /* This symbol requires a global offset table entry.  */
	  if (h != NULL)
	    {
	      if (h->got.refcount == -1)
		h->got.refcount = 1;
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
		  local_got_refcounts = ((bfd_signed_vma *)
					 bfd_alloc (abfd, size));
		  if (local_got_refcounts == NULL)
		    return false;
		  elf_local_got_refcounts (abfd) = local_got_refcounts;
		  memset (local_got_refcounts, -1, size);
		}
	      if (local_got_refcounts[r_symndx] == -1)
		local_got_refcounts[r_symndx] = 1;
	      else
		local_got_refcounts[r_symndx] += 1;
	    }
	  break;

	case R_386_PLT32:
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
	      h->elf_link_hash_flags |= ELF_LINK_HASH_NEEDS_PLT;
	      h->plt.refcount = 1;
	    }
	  else
	    h->plt.refcount += 1;
	  break;

	case R_386_32:
	case R_386_PC32:
	  if (h != NULL && !info->shared)
	    {
	      /* If this reloc is in a read-only section, we might
		 need a copy reloc.  */
	      if ((sec->flags & SEC_READONLY) != 0)
		h->elf_link_hash_flags |= ELF_LINK_NON_GOT_REF;

	      /* We may need a .plt entry if the function this reloc
		 refers to is in a shared lib.  */
	      if (h->plt.refcount == -1)
		h->plt.refcount = 1;
	      else
		h->plt.refcount += 1;
	    }

	  /* If we are creating a shared library, and this is a reloc
	     against a global symbol, or a non PC relative reloc
	     against a local symbol, then we need to copy the reloc
	     into the shared library.  However, if we are linking with
	     -Bsymbolic, we do not need to copy a reloc against a
	     global symbol which is defined in an object we are
	     including in the link (i.e., DEF_REGULAR is set).  At
	     this point we have not seen all the input files, so it is
	     possible that DEF_REGULAR is not set now but will be set
	     later (it is never cleared).  In case of a weak definition,
	     DEF_REGULAR may be cleared later by a strong definition in
	     a shared library. We account for that possibility below by
	     storing information in the relocs_copied field of the hash
	     table entry.  A similar situation occurs when creating
	     shared libraries and symbol visibility changes render the
	     symbol local.
	     If on the other hand, we are creating an executable, we
	     may need to keep relocations for symbols satisfied by a
	     dynamic library if we manage to avoid copy relocs for the
	     symbol.  */
	  if ((info->shared
	       && (sec->flags & SEC_ALLOC) != 0
	       && (ELF32_R_TYPE (rel->r_info) != R_386_PC32
		   || (h != NULL
		       && (! info->symbolic
			   || h->root.type == bfd_link_hash_defweak
			   || (h->elf_link_hash_flags
			       & ELF_LINK_HASH_DEF_REGULAR) == 0))))
	      || (!info->shared
		  && (sec->flags & SEC_ALLOC) != 0
		  && h != NULL
		  && (h->elf_link_hash_flags & ELF_LINK_NON_GOT_REF) == 0
		  && (h->root.type == bfd_link_hash_defweak
		      || (h->elf_link_hash_flags
			  & ELF_LINK_HASH_DEF_REGULAR) == 0)))
	    {
	      /* We must copy these reloc types into the output file.
		 Create a reloc section in dynobj and make room for
		 this reloc.  */
	      if (dynobj == NULL)
		htab->root.dynobj = dynobj = abfd;

	      if (sreloc == NULL)
		{
		  const char *name;

		  name = (bfd_elf_string_from_elf_section
			  (abfd,
			   elf_elfheader (abfd)->e_shstrndx,
			   elf_section_data (sec)->rel_hdr.sh_name));
		  if (name == NULL)
		    return false;

		  if (strncmp (name, ".rel", 4) != 0
		      || strcmp (bfd_get_section_name (abfd, sec),
				 name + 4) != 0)
		    {
		      if (abfd->my_archive)
			(*_bfd_error_handler) (_("%s(%s): bad relocation section name `%s\'"),
					       bfd_get_filename (abfd->my_archive),
					       bfd_get_filename (abfd),
					       name);
		      else
			(*_bfd_error_handler) (_("%s: bad relocation section name `%s\'"),
					       bfd_get_filename (abfd),
					       name);
		    }

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

	      sreloc->_raw_size += sizeof (Elf32_External_Rel);

	      /* If this is a global symbol, we count the number of PC
		 relative relocations we have entered for this symbol,
		 so that we can discard them later as necessary.  Note
		 that this function is only called if we are using an
		 elf_i386 linker hash table, which means that h is
		 really a pointer to an elf_i386_link_hash_entry.  */
	      if (!info->shared
		  || (h != NULL
		      && ELF32_R_TYPE (rel->r_info) == R_386_PC32))
		{
		  struct elf_i386_link_hash_entry *eh;
		  struct elf_i386_dyn_relocs *p;

		  eh = (struct elf_i386_link_hash_entry *) h;

		  for (p = eh->dyn_relocs; p != NULL; p = p->next)
		    if (p->section == sreloc)
		      break;

		  if (p == NULL)
		    {
		      p = ((struct elf_i386_dyn_relocs *)
			   bfd_alloc (dynobj, sizeof *p));
		      if (p == NULL)
			return false;
		      p->next = eh->dyn_relocs;
		      eh->dyn_relocs = p;
		      p->section = sreloc;
		      p->count = 0;
		    }

		  ++p->count;
		}
	    }

	  break;

	  /* This relocation describes the C++ object vtable hierarchy.
	     Reconstruct it for later use during GC.  */
	case R_386_GNU_VTINHERIT:
	  if (!_bfd_elf32_gc_record_vtinherit (abfd, sec, h, rel->r_offset))
	    return false;
	  break;

	  /* This relocation describes which C++ vtable entries are actually
	     used.  Record for later use during GC.  */
	case R_386_GNU_VTENTRY:
	  if (!_bfd_elf32_gc_record_vtentry (abfd, sec, h, rel->r_offset))
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
elf_i386_gc_mark_hook (abfd, info, rel, h, sym)
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
	case R_386_GNU_VTINHERIT:
	case R_386_GNU_VTENTRY:
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
elf_i386_gc_sweep_hook (abfd, info, sec, relocs)
     bfd *abfd;
     struct bfd_link_info *info;
     asection *sec;
     const Elf_Internal_Rela *relocs;
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  bfd_signed_vma *local_got_refcounts;
  const Elf_Internal_Rela *rel, *relend;
  unsigned long r_symndx;
  struct elf_link_hash_entry *h;
  bfd *dynobj;

  dynobj = elf_hash_table (info)->dynobj;
  if (dynobj == NULL)
    return true;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  local_got_refcounts = elf_local_got_refcounts (abfd);

  relend = relocs + sec->reloc_count;
  for (rel = relocs; rel < relend; rel++)
    switch (ELF32_R_TYPE (rel->r_info))
      {
      case R_386_GOT32:
      case R_386_GOTOFF:
      case R_386_GOTPC:
	r_symndx = ELF32_R_SYM (rel->r_info);
	if (r_symndx >= symtab_hdr->sh_info)
	  {
	    h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	    if (h->got.refcount > 0)
	      h->got.refcount -= 1;
	  }
	else if (local_got_refcounts != NULL)
	  {
	    if (local_got_refcounts[r_symndx] > 0)
	      local_got_refcounts[r_symndx] -= 1;
	  }
	break;

      case R_386_32:
      case R_386_PC32:
	if (info->shared)
	  break;
	/* Fall through.  */

      case R_386_PLT32:
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
elf_i386_adjust_dynamic_symbol (info, h)
     struct bfd_link_info *info;
     struct elf_link_hash_entry *h;
{
  struct elf_i386_link_hash_table *htab;
  bfd *dynobj;
  asection *s;
  unsigned int power_of_two;

  htab = elf_i386_hash_table (info);
  dynobj = htab->root.dynobj;

  /* If this is a function, put it in the procedure linkage table.  We
     will fill in the contents of the procedure linkage table later,
     when we know the address of the .got section.  */
  if (h->type == STT_FUNC
      || (h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_PLT) != 0)
    {
      if (h->plt.refcount <= 0
	  || (! info->shared
	      && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_DYNAMIC) == 0
	      && (h->elf_link_hash_flags & ELF_LINK_HASH_REF_DYNAMIC) == 0))
	{
	  /* This case can occur if we saw a PLT32 reloc in an input
	     file, but the symbol was never referred to by a dynamic
	     object, or if all references were garbage collected.  In
	     such a case, we don't actually need to build a procedure
	     linkage table, and we can just do a PC32 reloc instead.  */
	  h->plt.refcount = (bfd_vma) -1;
	  h->elf_link_hash_flags &= ~ELF_LINK_HASH_NEEDS_PLT;
	}

      return true;
    }
  else
    /* It's possible that we incorrectly decided a .plt reloc was
       needed for an R_386_PC32 reloc to a non-function sym in
       check_relocs.  We can't decide accurately between function and
       non-function syms in check-relocs;  Objects loaded later in
       the link may change h->type.  So fix it now.  */
    h->plt.refcount = (bfd_vma) -1;

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

  s = htab->sdynbss;
  if (s == NULL)
    abort ();

  /* We must generate a R_386_COPY reloc to tell the dynamic linker to
     copy the initial value out of the dynamic object and into the
     runtime process image.  We need to remember the offset into the
     .rel.bss section we are going to use.  */
  if ((h->root.u.def.section->flags & SEC_ALLOC) != 0)
    {
      asection *srel;

      srel = htab->srelbss;
      if (srel == NULL)
	abort ();
      srel->_raw_size += sizeof (Elf32_External_Rel);
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

/* This is the condition under which elf_i386_finish_dynamic_symbol
   will be called from elflink.h.  If elflink.h doesn't call our
   finish_dynamic_symbol routine, we'll need to do something about
   initializing any .plt and .got entries in elf_i386_relocate_section.  */
#define WILL_CALL_FINISH_DYNAMIC_SYMBOL(DYN, INFO, H) \
  ((DYN)								\
   && ((INFO)->shared							\
       || ((H)->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) == 0)	\
   && ((H)->dynindx != -1						\
       || ((H)->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) != 0))

/* Allocate space in .plt, .got and associated reloc sections for
   global syms. Also discards space allocated for relocs in the
   check_relocs function that we subsequently have found to be
   unneeded.  */

static boolean
allocate_plt_and_got_and_discard_relocs (h, inf)
     struct elf_link_hash_entry *h;
     PTR inf;
{
  struct bfd_link_info *info;
  struct elf_i386_link_hash_table *htab;
  asection *s;
  struct elf_i386_link_hash_entry *eh;

  if (h->root.type == bfd_link_hash_indirect
      || h->root.type == bfd_link_hash_warning)
    return true;

  info = (struct bfd_link_info *) inf;
  htab = elf_i386_hash_table (info);

  if (htab->root.dynamic_sections_created
      && h->plt.refcount > 0)
    {
      /* Make sure this symbol is output as a dynamic symbol.
	 Undefined weak syms won't yet be marked as dynamic.  */
      if (h->dynindx == -1
	  && (h->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) == 0)
	{
	  if (! bfd_elf32_link_record_dynamic_symbol (info, h))
	    return false;
	}

      s = htab->splt;
      if (s == NULL)
	abort ();

      /* If this is the first .plt entry, make room for the special
	 first entry.  */
      if (s->_raw_size == 0)
	s->_raw_size += PLT_ENTRY_SIZE;

      h->plt.offset = s->_raw_size;

      /* If this symbol is not defined in a regular file, and we are
	 not generating a shared library, then set the symbol to this
	 location in the .plt.  This is required to make function
	 pointers compare as equal between the normal executable and
	 the shared library.  */
      if (! info->shared
	  && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR) == 0)
	{
	  h->root.u.def.section = s;
	  h->root.u.def.value = h->plt.offset;
	}

      /* Make room for this entry.  */
      s->_raw_size += PLT_ENTRY_SIZE;

      /* We also need to make an entry in the .got.plt section, which
	 will be placed in the .got section by the linker script.  */
      s = htab->sgotplt;
      if (s == NULL)
	abort ();
      s->_raw_size += 4;

      if (WILL_CALL_FINISH_DYNAMIC_SYMBOL (1, info, h))
	{
	  /* We also need to make an entry in the .rel.plt section.  */
	  s = htab->srelplt;
	  if (s == NULL)
	    abort ();
	  s->_raw_size += sizeof (Elf32_External_Rel);
	}
    }
  else
    {
      h->plt.offset = (bfd_vma) -1;
      h->elf_link_hash_flags &= ~ELF_LINK_HASH_NEEDS_PLT;
    }

  if (h->got.refcount > 0)
    {
      boolean dyn;

      /* Make sure this symbol is output as a dynamic symbol.
	 Undefined weak syms won't yet be marked as dynamic.  */
      if (h->dynindx == -1
	  && (h->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) == 0)
	{
	  if (! bfd_elf32_link_record_dynamic_symbol (info, h))
	    return false;
	}

      s = htab->sgot;
      h->got.offset = s->_raw_size;
      s->_raw_size += 4;
      dyn = htab->root.dynamic_sections_created;
      if (WILL_CALL_FINISH_DYNAMIC_SYMBOL (dyn, info, h))
	htab->srelgot->_raw_size += sizeof (Elf32_External_Rel);
    }
  else
    h->got.offset = (bfd_vma) -1;

  /* In the shared -Bsymbolic case, discard space allocated for
     dynamic relocs against symbols which turn out to be defined
     in regular objects.  For the normal shared case, discard space
     for relocs that have become local due to symbol visibility
     changes.  For the non-shared case, discard space for symbols
     which turn out to need copy relocs or are not dynamic.  */

  eh = (struct elf_i386_link_hash_entry *) h;
  if (eh->dyn_relocs == NULL)
    return true;

  if (!info->shared
      && (h->elf_link_hash_flags & ELF_LINK_NON_GOT_REF) == 0
      && ((h->elf_link_hash_flags & ELF_LINK_HASH_DEF_DYNAMIC) != 0
	  || h->root.type == bfd_link_hash_undefweak
	  || h->root.type == bfd_link_hash_undefined))
    {
      /* Make sure this symbol is output as a dynamic symbol.
	 Undefined weak syms won't yet be marked as dynamic.  */
      if (h->dynindx == -1
	  && (h->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) == 0)
	{
	  if (! bfd_elf32_link_record_dynamic_symbol (info, h))
	    return false;
	}

      /* If that succeeded, we know we'll be keeping all the relocs.  */
      if (h->dynindx != -1)
	return true;
    }

  if (!info->shared
      || ((h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR) != 0
	  && ((h->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL) != 0
	      || info->symbolic)))
    {
      struct elf_i386_dyn_relocs *c;

      for (c = eh->dyn_relocs; c != NULL; c = c->next)
	c->section->_raw_size -= c->count * sizeof (Elf32_External_Rel);
    }

  return true;
}

/* Set the sizes of the dynamic sections.  */

static boolean
elf_i386_size_dynamic_sections (output_bfd, info)
     bfd *output_bfd;
     struct bfd_link_info *info;
{
  struct elf_i386_link_hash_table *htab;
  bfd *dynobj;
  asection *s;
  boolean relocs;
  boolean reltext;
  bfd *i;

  htab = elf_i386_hash_table (info);
  dynobj = htab->root.dynobj;
  if (dynobj == NULL)
    abort ();

  if (htab->root.dynamic_sections_created)
    {
      /* Set the contents of the .interp section to the interpreter.  */
      if (! info->shared)
	{
	  s = bfd_get_section_by_name (dynobj, ".interp");
	  if (s == NULL)
	    abort ();
	  s->_raw_size = sizeof ELF_DYNAMIC_INTERPRETER;
	  s->contents = (unsigned char *) ELF_DYNAMIC_INTERPRETER;
	}
    }

  /* Set up .got offsets for local syms.  */
  for (i = info->input_bfds; i; i = i->link_next)
    {
      bfd_signed_vma *local_got;
      bfd_signed_vma *end_local_got;
      bfd_size_type locsymcount;
      Elf_Internal_Shdr *symtab_hdr;
      asection *srel;

      if (bfd_get_flavour (i) != bfd_target_elf_flavour)
	continue;

      local_got = elf_local_got_refcounts (i);
      if (!local_got)
	continue;

      symtab_hdr = &elf_tdata (i)->symtab_hdr;
      locsymcount = symtab_hdr->sh_info;
      end_local_got = local_got + locsymcount;
      s = htab->sgot;
      srel = htab->srelgot;
      for (; local_got < end_local_got; ++local_got)
	{
	  if (*local_got > 0)
	    {
	      *local_got = s->_raw_size;
	      s->_raw_size += 4;
	      if (info->shared)
		srel->_raw_size += sizeof (Elf32_External_Rel);
	    }
	  else
	    *local_got = (bfd_vma) -1;
	}
    }

  /* Allocate global sym .plt and .got entries.  Also discard all
     unneeded relocs.  */
  elf_link_hash_traverse (&htab->root,
			  allocate_plt_and_got_and_discard_relocs,
			  (PTR) info);

  /* We now have determined the sizes of the various dynamic sections.
     Allocate memory for them.  */
  relocs = false;
  reltext = false;
  for (s = dynobj->sections; s != NULL; s = s->next)
    {
      if ((s->flags & SEC_LINKER_CREATED) == 0)
	continue;

      if (s == htab->splt
	  || s == htab->sgot
	  || s == htab->sgotplt)
	{
	  /* Strip this section if we don't need it; see the
	     comment below.  */
	}
      else if (strncmp (bfd_get_section_name (dynobj, s), ".rel", 4) == 0)
	{
	  if (s->_raw_size == 0)
	    {
	      /* If we don't need this section, strip it from the
		 output file.  This is mostly to handle .rel.bss and
		 .rel.plt.  We must create both sections in
		 create_dynamic_sections, because they must be created
		 before the linker maps input sections to output
		 sections.  The linker does that before
		 adjust_dynamic_symbol is called, and it is that
		 function which decides whether anything needs to go
		 into these sections.  */
	    }
	  else
	    {
	      asection *target;

	      /* Remember whether there are any reloc sections other
		 than .rel.plt.  */
	      if (s != htab->srelplt)
		{
		  const char *outname;

		  relocs = true;

		  /* If this relocation section applies to a read only
		     section, then we probably need a DT_TEXTREL
		     entry.  The entries in the .rel.plt section
		     really apply to the .got section, which we
		     created ourselves and so know is not readonly.  */
		  outname = bfd_get_section_name (output_bfd,
						  s->output_section);
		  target = bfd_get_section_by_name (output_bfd, outname + 4);
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
      else
	{
	  /* It's not one of our sections, so don't allocate space.  */
	  continue;
	}

      if (s->_raw_size == 0)
	{
	  _bfd_strip_section_from_output (info, s);
	  continue;
	}

      /* Allocate memory for the section contents.  We use bfd_zalloc
	 here in case unused entries are not reclaimed before the
	 section's contents are written out.  This should not happen,
	 but this way if it does, we get a R_386_NONE reloc instead
	 of garbage.  */
      s->contents = (bfd_byte *) bfd_zalloc (dynobj, s->_raw_size);
      if (s->contents == NULL)
	return false;
    }

  if (htab->root.dynamic_sections_created)
    {
      /* Add some entries to the .dynamic section.  We fill in the
	 values later, in elf_i386_finish_dynamic_sections, but we
	 must add the entries now so that we get the correct size for
	 the .dynamic section.  The DT_DEBUG entry is filled in by the
	 dynamic linker and used by the debugger.  */
      if (! info->shared)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_DEBUG, 0))
	    return false;
	}

      if (htab->splt->_raw_size != 0)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_PLTGOT, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_PLTRELSZ, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_PLTREL, DT_REL)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_JMPREL, 0))
	    return false;
	}

      if (relocs)
	{
	  if (! bfd_elf32_add_dynamic_entry (info, DT_REL, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_RELSZ, 0)
	      || ! bfd_elf32_add_dynamic_entry (info, DT_RELENT,
						sizeof (Elf32_External_Rel)))
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

/* Relocate an i386 ELF section.  */

static boolean
elf_i386_relocate_section (output_bfd, info, input_bfd, input_section,
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
  struct elf_i386_link_hash_table *htab;
  bfd *dynobj;
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  bfd_vma *local_got_offsets;
  asection *sreloc;
  Elf_Internal_Rela *rel;
  Elf_Internal_Rela *relend;

  htab = elf_i386_hash_table (info);
  dynobj = htab->root.dynobj;
  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);
  local_got_offsets = elf_local_got_offsets (input_bfd);

  sreloc = NULL;
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
      bfd_vma off;
      bfd_vma relocation;
      boolean unresolved_reloc;
      bfd_reloc_status_type r;
      unsigned int indx;

      r_type = ELF32_R_TYPE (rel->r_info);
      if (r_type == (int) R_386_GNU_VTINHERIT
	  || r_type == (int) R_386_GNU_VTENTRY)
	continue;

      if ((indx = (unsigned) r_type) >= R_386_standard
	  && ((indx = (unsigned) r_type - R_386_ext_offset) - R_386_standard
	      >= R_386_ext - R_386_standard))
	{
	  bfd_set_error (bfd_error_bad_value);
	  return false;
	}
      howto = elf_howto_table + indx;

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
		  bfd_vma val;

		  sec = local_sections[r_symndx];
		  val = bfd_get_32 (input_bfd, contents + rel->r_offset);
		  val += sec->output_offset + sym->st_value;
		  bfd_put_32 (input_bfd, val, contents + rel->r_offset);
		}
	    }

	  continue;
	}

      /* This is a final link.  */
      h = NULL;
      sym = NULL;
      sec = NULL;
      unresolved_reloc = false;
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

	  relocation = 0;
	  if (h->root.type == bfd_link_hash_defined
	      || h->root.type == bfd_link_hash_defweak)
	    {
	      sec = h->root.u.def.section;
	      if (sec->output_section == NULL)
		/* Set a flag that will be cleared later if we find a
		   relocation value for this symbol.  output_section
		   is typically NULL for symbols satisfied by a shared
		   library.  */
		unresolved_reloc = true;
	      else
		relocation = (h->root.u.def.value
			      + sec->output_section->vma
			      + sec->output_offset);
	    }
	  else if (h->root.type == bfd_link_hash_undefweak)
	    ;
	  else if (info->shared && !info->symbolic
		   && !info->no_undefined
		   && ELF_ST_VISIBILITY (h->other) == STV_DEFAULT)
	    ;
	  else
	    {
	      if (! ((*info->callbacks->undefined_symbol)
		     (info, h->root.root.string, input_bfd,
		      input_section, rel->r_offset,
		      (!info->shared || info->no_undefined
		       || ELF_ST_VISIBILITY (h->other)))))
		return false;
	    }
	}

      switch (r_type)
	{
	case R_386_GOT32:
	  /* Relocation is to the entry for this symbol in the global
	     offset table.  */
	  if (htab->sgot == NULL)
	    abort ();

	  if (h != NULL)
	    {
	      boolean dyn;

	      off = h->got.offset;
	      dyn = htab->root.dynamic_sections_created;
	      if (! WILL_CALL_FINISH_DYNAMIC_SYMBOL (dyn, info, h)
		  || (info->shared
		      && (info->symbolic
			  || h->dynindx == -1
			  || (h->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL))
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

		     When doing a dynamic link, we create a .rel.got
		     relocation entry to initialize the value.  This
		     is done in the finish_dynamic_symbol routine.  */
		  if ((off & 1) != 0)
		    off &= ~1;
		  else
		    {
		      bfd_put_32 (output_bfd, relocation,
				  htab->sgot->contents + off);
		      h->got.offset |= 1;
		    }
		}
	      else
		unresolved_reloc = false;
	    }
	  else
	    {
	      if (local_got_offsets == NULL)
		abort ();

	      off = local_got_offsets[r_symndx];

	      /* The offset must always be a multiple of 4.  We use
		 the least significant bit to record whether we have
		 already generated the necessary reloc.  */
	      if ((off & 1) != 0)
		off &= ~1;
	      else
		{
		  bfd_put_32 (output_bfd, relocation,
			      htab->sgot->contents + off);

		  if (info->shared)
		    {
		      asection *srelgot;
		      Elf_Internal_Rel outrel;

		      srelgot = htab->srelgot;
		      if (srelgot == NULL)
			abort ();

		      outrel.r_offset = (htab->sgot->output_section->vma
					 + htab->sgot->output_offset
					 + off);
		      outrel.r_info = ELF32_R_INFO (0, R_386_RELATIVE);
		      bfd_elf32_swap_reloc_out (output_bfd, &outrel,
						(((Elf32_External_Rel *)
						  srelgot->contents)
						 + srelgot->reloc_count));
		      ++srelgot->reloc_count;
		    }

		  local_got_offsets[r_symndx] |= 1;
		}
	    }

	  if (off >= (bfd_vma) -2)
	    abort ();

	  relocation = htab->sgot->output_offset + off;
	  break;

	case R_386_GOTOFF:
	  /* Relocation is relative to the start of the global offset
	     table.  */

	  /* Note that sgot->output_offset is not involved in this
	     calculation.  We always want the start of .got.  If we
	     defined _GLOBAL_OFFSET_TABLE in a different way, as is
	     permitted by the ABI, we might have to change this
	     calculation.  */
	  relocation -= htab->sgot->output_section->vma;
	  break;

	case R_386_GOTPC:
	  /* Use global offset table as symbol value.  */
	  relocation = htab->sgot->output_section->vma;
	  unresolved_reloc = false;
	  break;

	case R_386_PLT32:
	  /* Relocation is to the entry for this symbol in the
	     procedure linkage table.  */

	  /* Resolve a PLT32 reloc against a local symbol directly,
	     without using the procedure linkage table.  */
	  if (h == NULL)
	    break;

	  if (h->plt.offset == (bfd_vma) -1
	      || htab->splt == NULL)
	    {
	      /* We didn't make a PLT entry for this symbol.  This
		 happens when statically linking PIC code, or when
		 using -Bsymbolic.  */
	      break;
	    }

	  relocation = (htab->splt->output_section->vma
			+ htab->splt->output_offset
			+ h->plt.offset);
	  unresolved_reloc = false;
	  break;

	case R_386_32:
	case R_386_PC32:
	  if ((info->shared
	       && (input_section->flags & SEC_ALLOC) != 0
	       && (r_type != R_386_PC32
		   || (h != NULL
		       && h->dynindx != -1
		       && (! info->symbolic
			   || (h->elf_link_hash_flags
			       & ELF_LINK_HASH_DEF_REGULAR) == 0))))
	      || (!info->shared
		  && (input_section->flags & SEC_ALLOC) != 0
		  && h != NULL
		  && h->dynindx != -1
		  && (h->elf_link_hash_flags & ELF_LINK_NON_GOT_REF) == 0
		  && ((h->elf_link_hash_flags
		       & ELF_LINK_HASH_DEF_DYNAMIC) != 0
		      || h->root.type == bfd_link_hash_undefweak
		      || h->root.type == bfd_link_hash_undefined)))
	    {
	      Elf_Internal_Rel outrel;
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

		  if (strncmp (name, ".rel", 4) != 0
		      || strcmp (bfd_get_section_name (input_bfd,
						       input_section),
				 name + 4) != 0)
		    {
		      if (input_bfd->my_archive)
			(*_bfd_error_handler)\
			  (_("%s(%s): bad relocation section name `%s\'"),
			   bfd_get_filename (input_bfd->my_archive),
			   bfd_get_filename (input_bfd),
			   name);
		      else
			(*_bfd_error_handler)
			  (_("%s: bad relocation section name `%s\'"),
			   bfd_get_filename (input_bfd),
			   name);
		      return false;
		    }

		  sreloc = bfd_get_section_by_name (dynobj, name);
		  if (sreloc == NULL)
		    abort ();
		}

	      skip = false;

	      if (elf_section_data (input_section)->stab_info == NULL)
		outrel.r_offset = rel->r_offset;
	      else
		{
		  bfd_vma off;

		  off = (_bfd_stab_section_offset
			 (output_bfd, htab->root.stab_info, input_section,
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
	      else if (h != NULL
		       && h->dynindx != -1
		       && (r_type == R_386_PC32
			   || !info->shared
			   || !info->symbolic
			   || (h->elf_link_hash_flags
			       & ELF_LINK_HASH_DEF_REGULAR) == 0))

		{
		  relocate = false;
		  outrel.r_info = ELF32_R_INFO (h->dynindx, r_type);
		}
	      else
		{
		  /* This symbol is local, or marked to become local.  */
		  relocate = true;
		  outrel.r_info = ELF32_R_INFO (0, R_386_RELATIVE);
		}

	      bfd_elf32_swap_reloc_out (output_bfd, &outrel,
					(((Elf32_External_Rel *)
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

      /* FIXME: Why do we allow debugging sections to escape this error?
	 More importantly, why do we not emit dynamic relocs for
	 R_386_32 above in debugging sections (which are ! SEC_ALLOC)?
	 If we had emitted the dynamic reloc, we could remove the
	 fudge here.  */
      if (unresolved_reloc
	  && !(info->shared
	       && (input_section->flags & SEC_DEBUGGING) != 0
	       && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_DYNAMIC) != 0))
	(*_bfd_error_handler)
	  (_("%s(%s+0x%lx): unresolvable relocation against symbol `%s'"),
	   bfd_get_filename (input_bfd),
	   bfd_get_section_name (input_bfd, input_section),
	   (long) rel->r_offset,
	   h->root.root.string);

      r = _bfd_final_link_relocate (howto, input_bfd, input_section,
				    contents, rel->r_offset,
				    relocation, (bfd_vma) 0);

      switch (r)
	{
	case bfd_reloc_ok:
	  break;

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

	default:
	case bfd_reloc_outofrange:
	  abort ();
	  break;
	}
    }

  return true;
}

/* Finish up dynamic symbol handling.  We set the contents of various
   dynamic sections here.  */

static boolean
elf_i386_finish_dynamic_symbol (output_bfd, info, h, sym)
     bfd *output_bfd;
     struct bfd_link_info *info;
     struct elf_link_hash_entry *h;
     Elf_Internal_Sym *sym;
{
  struct elf_i386_link_hash_table *htab;
  bfd *dynobj;

  htab = elf_i386_hash_table (info);
  dynobj = htab->root.dynobj;

  if (h->plt.offset != (bfd_vma) -1)
    {
      bfd_vma plt_index;
      bfd_vma got_offset;
      Elf_Internal_Rel rel;

      /* This symbol has an entry in the procedure linkage table.  Set
	 it up.  */

      if (h->dynindx == -1
	  || htab->splt == NULL
	  || htab->sgotplt == NULL
	  || htab->srelplt == NULL)
	abort ();

      /* Get the index in the procedure linkage table which
	 corresponds to this symbol.  This is the index of this symbol
	 in all the symbols for which we are making plt entries.  The
	 first entry in the procedure linkage table is reserved.  */
      plt_index = h->plt.offset / PLT_ENTRY_SIZE - 1;

      /* Get the offset into the .got table of the entry that
	 corresponds to this function.  Each .got entry is 4 bytes.
	 The first three are reserved.  */
      got_offset = (plt_index + 3) * 4;

      /* Fill in the entry in the procedure linkage table.  */
      if (! info->shared)
	{
	  memcpy (htab->splt->contents + h->plt.offset, elf_i386_plt_entry,
		  PLT_ENTRY_SIZE);
	  bfd_put_32 (output_bfd,
		      (htab->sgotplt->output_section->vma
		       + htab->sgotplt->output_offset
		       + got_offset),
		      htab->splt->contents + h->plt.offset + 2);
	}
      else
	{
	  memcpy (htab->splt->contents + h->plt.offset, elf_i386_pic_plt_entry,
		  PLT_ENTRY_SIZE);
	  bfd_put_32 (output_bfd, got_offset,
		      htab->splt->contents + h->plt.offset + 2);
	}

      bfd_put_32 (output_bfd, plt_index * sizeof (Elf32_External_Rel),
		  htab->splt->contents + h->plt.offset + 7);
      bfd_put_32 (output_bfd, - (h->plt.offset + PLT_ENTRY_SIZE),
		  htab->splt->contents + h->plt.offset + 12);

      /* Fill in the entry in the global offset table.  */
      bfd_put_32 (output_bfd,
		  (htab->splt->output_section->vma
		   + htab->splt->output_offset
		   + h->plt.offset
		   + 6),
		  htab->sgotplt->contents + got_offset);

      /* Fill in the entry in the .rel.plt section.  */
      rel.r_offset = (htab->sgotplt->output_section->vma
		      + htab->sgotplt->output_offset
		      + got_offset);
      rel.r_info = ELF32_R_INFO (h->dynindx, R_386_JUMP_SLOT);
      bfd_elf32_swap_reloc_out (output_bfd, &rel,
				((Elf32_External_Rel *) htab->srelplt->contents
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
      Elf_Internal_Rel rel;

      /* This symbol has an entry in the global offset table.  Set it
	 up.  */

      if (htab->sgot == NULL || htab->srelgot == NULL)
	abort ();

      rel.r_offset = (htab->sgot->output_section->vma
		      + htab->sgot->output_offset
		      + (h->got.offset &~ 1));

      /* If this is a static link, or it is a -Bsymbolic link and the
	 symbol is defined locally or was forced to be local because
	 of a version file, we just want to emit a RELATIVE reloc.
	 The entry in the global offset table will already have been
	 initialized in the relocate_section function.  */
      if (info->shared
	  && (info->symbolic
	      || h->dynindx == -1
	      || (h->elf_link_hash_flags & ELF_LINK_FORCED_LOCAL))
	  && (h->elf_link_hash_flags & ELF_LINK_HASH_DEF_REGULAR))
	{
	  BFD_ASSERT((h->got.offset & 1) != 0);
	  rel.r_info = ELF32_R_INFO (0, R_386_RELATIVE);
	}
      else
	{
	  BFD_ASSERT((h->got.offset & 1) == 0);
	  bfd_put_32 (output_bfd, (bfd_vma) 0,
		      htab->sgot->contents + h->got.offset);
	  rel.r_info = ELF32_R_INFO (h->dynindx, R_386_GLOB_DAT);
	}

      bfd_elf32_swap_reloc_out (output_bfd, &rel,
				((Elf32_External_Rel *) htab->srelgot->contents
				 + htab->srelgot->reloc_count));
      ++htab->srelgot->reloc_count;
    }

  if ((h->elf_link_hash_flags & ELF_LINK_HASH_NEEDS_COPY) != 0)
    {
      Elf_Internal_Rel rel;

      /* This symbol needs a copy reloc.  Set it up.  */

      if (h->dynindx == -1
	  || (h->root.type != bfd_link_hash_defined
	      && h->root.type != bfd_link_hash_defweak)
	  || htab->srelbss == NULL)
	abort ();

      rel.r_offset = (h->root.u.def.value
		      + h->root.u.def.section->output_section->vma
		      + h->root.u.def.section->output_offset);
      rel.r_info = ELF32_R_INFO (h->dynindx, R_386_COPY);
      bfd_elf32_swap_reloc_out (output_bfd, &rel,
				((Elf32_External_Rel *) htab->srelbss->contents
				 + htab->srelbss->reloc_count));
      ++htab->srelbss->reloc_count;
    }

  /* Mark _DYNAMIC and _GLOBAL_OFFSET_TABLE_ as absolute.  */
  if (strcmp (h->root.root.string, "_DYNAMIC") == 0
      || strcmp (h->root.root.string, "_GLOBAL_OFFSET_TABLE_") == 0)
    sym->st_shndx = SHN_ABS;

  return true;
}

/* Finish up the dynamic sections.  */

static boolean
elf_i386_finish_dynamic_sections (output_bfd, info)
     bfd *output_bfd;
     struct bfd_link_info *info;
{
  struct elf_i386_link_hash_table *htab;
  bfd *dynobj;
  asection *sdyn;

  htab = elf_i386_hash_table (info);
  dynobj = htab->root.dynobj;
  sdyn = bfd_get_section_by_name (dynobj, ".dynamic");

  if (htab->root.dynamic_sections_created)
    {
      Elf32_External_Dyn *dyncon, *dynconend;

      if (sdyn == NULL || htab->sgot == NULL)
	abort ();

      dyncon = (Elf32_External_Dyn *) sdyn->contents;
      dynconend = (Elf32_External_Dyn *) (sdyn->contents + sdyn->_raw_size);
      for (; dyncon < dynconend; dyncon++)
	{
	  Elf_Internal_Dyn dyn;

	  bfd_elf32_swap_dyn_in (dynobj, dyncon, &dyn);

	  switch (dyn.d_tag)
	    {
	    default:
	      break;

	    case DT_PLTGOT:
	      dyn.d_un.d_ptr = htab->sgot->output_section->vma;
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;

	    case DT_JMPREL:
	      dyn.d_un.d_ptr = htab->srelplt->output_section->vma;
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;

	    case DT_PLTRELSZ:
	      if (htab->srelplt->output_section->_cooked_size != 0)
		dyn.d_un.d_val = htab->srelplt->output_section->_cooked_size;
	      else
		dyn.d_un.d_val = htab->srelplt->output_section->_raw_size;
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;

	    case DT_RELSZ:
	      /* My reading of the SVR4 ABI indicates that the
		 procedure linkage table relocs (DT_JMPREL) should be
		 included in the overall relocs (DT_REL).  This is
		 what Solaris does.  However, UnixWare can not handle
		 that case.  Therefore, we override the DT_RELSZ entry
		 here to make it not include the JMPREL relocs.  Since
		 the linker script arranges for .rel.plt to follow all
		 other relocation sections, we don't have to worry
		 about changing the DT_REL entry.  */
	      if (htab->srelplt != NULL)
		{
		  if (htab->srelplt->output_section->_cooked_size != 0)
		    dyn.d_un.d_val -= htab->srelplt->output_section->_cooked_size;
		  else
		    dyn.d_un.d_val -= htab->srelplt->output_section->_raw_size;
		}
	      bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	      break;
	    }
	}

      /* Fill in the first entry in the procedure linkage table.  */
      if (htab->splt && htab->splt->_raw_size > 0)
	{
	  if (info->shared)
	    memcpy (htab->splt->contents,
		    elf_i386_pic_plt0_entry, PLT_ENTRY_SIZE);
	  else
	    {
	      memcpy (htab->splt->contents,
		      elf_i386_plt0_entry, PLT_ENTRY_SIZE);
	      bfd_put_32 (output_bfd,
			  (htab->sgotplt->output_section->vma
			   + htab->sgotplt->output_offset
			   + 4),
			  htab->splt->contents + 2);
	      bfd_put_32 (output_bfd,
			  (htab->sgotplt->output_section->vma
			   + htab->sgotplt->output_offset
			   + 8),
			  htab->splt->contents + 8);
	    }

	  /* UnixWare sets the entsize of .plt to 4, although that doesn't
	     really seem like the right value.  */
	  elf_section_data (htab->splt->output_section)
	    ->this_hdr.sh_entsize = 4;
	}
    }

  if (htab->sgotplt)
    {
      /* Fill in the first three entries in the global offset table.  */
      if (htab->sgotplt->_raw_size > 0)
	{
	  bfd_put_32 (output_bfd,
		      (sdyn == NULL ? (bfd_vma) 0
		       : sdyn->output_section->vma + sdyn->output_offset),
		      htab->sgotplt->contents);
	  bfd_put_32 (output_bfd, (bfd_vma) 0, htab->sgotplt->contents + 4);
	  bfd_put_32 (output_bfd, (bfd_vma) 0, htab->sgotplt->contents + 8);
	}

      elf_section_data (htab->sgotplt->output_section)->this_hdr.sh_entsize = 4;
    }
  return true;
}

/* Set the correct type for an x86 ELF section.  We do this by the
   section name, which is a hack, but ought to work.  */

static boolean
elf_i386_fake_sections (abfd, hdr, sec)
     bfd *abfd ATTRIBUTE_UNUSED;
     Elf32_Internal_Shdr *hdr;
     asection *sec;
{
  register const char *name;

  name = bfd_get_section_name (abfd, sec);

  if (strcmp (name, ".reloc") == 0)
    /*
     * This is an ugly, but unfortunately necessary hack that is
     * needed when producing EFI binaries on x86. It tells
     * elf.c:elf_fake_sections() not to consider ".reloc" as a section
     * containing ELF relocation info.  We need this hack in order to
     * be able to generate ELF binaries that can be translated into
     * EFI applications (which are essentially COFF objects).  Those
     * files contain a COFF ".reloc" section inside an ELFNN object,
     * which would normally cause BFD to segfault because it would
     * attempt to interpret this section as containing relocation
     * entries for section "oc".  With this hack enabled, ".reloc"
     * will be treated as a normal data section, which will avoid the
     * segfault.  However, you won't be able to create an ELFNN binary
     * with a section named "oc" that needs relocations, but that's
     * the kind of ugly side-effects you get when detecting section
     * types based on their names...  In practice, this limitation is
     * unlikely to bite.
     */
    hdr->sh_type = SHT_PROGBITS;

  return true;
}


#define TARGET_LITTLE_SYM		bfd_elf32_i386_vec
#define TARGET_LITTLE_NAME		"elf32-i386"
#define ELF_ARCH			bfd_arch_i386
#define ELF_MACHINE_CODE		EM_386
#define ELF_MAXPAGESIZE			0x1000

#define elf_backend_can_gc_sections	1
#define elf_backend_want_got_plt	1
#define elf_backend_plt_readonly	1
#define elf_backend_want_plt_sym	0
#define elf_backend_got_header_size	12
#define elf_backend_plt_header_size	PLT_ENTRY_SIZE

#define elf_info_to_howto		      elf_i386_info_to_howto
#define elf_info_to_howto_rel		      elf_i386_info_to_howto_rel

#define bfd_elf32_bfd_is_local_label_name     elf_i386_is_local_label_name
#define bfd_elf32_bfd_link_hash_table_create  elf_i386_link_hash_table_create
#define bfd_elf32_bfd_reloc_type_lookup	      elf_i386_reloc_type_lookup

#define elf_backend_adjust_dynamic_symbol     elf_i386_adjust_dynamic_symbol
#define elf_backend_check_relocs	      elf_i386_check_relocs
#define elf_backend_create_dynamic_sections   elf_i386_create_dynamic_sections
#define elf_backend_finish_dynamic_sections   elf_i386_finish_dynamic_sections
#define elf_backend_finish_dynamic_symbol     elf_i386_finish_dynamic_symbol
#define elf_backend_gc_mark_hook	      elf_i386_gc_mark_hook
#define elf_backend_gc_sweep_hook	      elf_i386_gc_sweep_hook
#define elf_backend_relocate_section	      elf_i386_relocate_section
#define elf_backend_size_dynamic_sections     elf_i386_size_dynamic_sections
#define elf_backend_fake_sections	      elf_i386_fake_sections

#include "elf32-target.h"
