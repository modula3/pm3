/*  This file is part of the program psim.
    
    Copyright (C) 1994-1996, Andrew Cagney <cagney@highland.com.au>
    
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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
    
    */


#ifndef _PK_DISKLABEL_C_
#define _PK_DISKLABEL_C_

#ifndef STATIC_INLINE_PK_DISKLABEL
#define STATIC_INLINE_PK_DISKLABEL STATIC_INLINE
#endif

#include "device_table.h"

#include "pk.h"

/* PACKAGE

   disk-label - all knowing disk I/O package

   DESCRIPTION

   The disk-label package provides a generic interface to disk
   devices.  It uses the arguments specified when an instance is being
   created to determine if the raw disk, a partition, or a file within
   a partition should be opened.

   An instance create call to disk-label could result, for instance,
   in the opening of a DOS file system contained within a dos
   partition contained within a physical disk.

   */

/* taken from bfd/ppcboot.c by Michael Meissner */

/* PPCbug location structure */
typedef struct ppcboot_location {
  unsigned8 ind;
  unsigned8 head;
  unsigned8 sector;
  unsigned8 cylinder;
} ppcboot_location_t;

/* PPCbug partition table layout */
typedef struct ppcboot_partition {
  ppcboot_location_t partition_begin;	/* partition begin */
  ppcboot_location_t partition_end;	/* partition end */
  unsigned8 sector_begin[4];		/* 32-bit start RBA (zero-based), little endian */
  unsigned8 sector_length[4];		/* 32-bit RBA count (one-based), little endian */
} ppcboot_partition_t;

/* Fdisk block */
typedef struct ppcboot_record {
  unsigned8 pc_compatibility[446];	/* x86 instruction field */
  ppcboot_partition_t partition[4];	/* partition information */
  unsigned8 signature[2];		/* 0x55 and 0xaa */
} ppcboot_record_t;

/* Signature bytes for last 2 bytes of the 512 byte record */
#define SIGNATURE0 0x55
#define SIGNATURE1 0xaa

typedef struct _disklabel {
  device_instance *parent;
  device_instance *raw_disk;
  unsigned_word pos;
  unsigned_word sector_begin;
  unsigned_word sector_length;
} disklabel;


static unsigned_word
sector2uw(unsigned8 s[4])
{
  return ((s[3] << 24)
	  + (s[2] << 16)
	  + (s[1] << 8)
	  + (s[0] << 0));
}


static void
disklabel_delete(device_instance *instance)
{
  device_error(device_instance_device(instance),
	       "disklabel_delete not implemented\n");
}


static int
disklabel_read(device_instance *instance,
	       void *buf,
	       unsigned_word len)
{
  disklabel *label = device_instance_data(instance);
  if (label->pos + len > label->sector_length)
    len = label->sector_length - label->pos;
  if (device_instance_seek(label->raw_disk, 0, label->sector_begin) < 0)
    return -1;
  return device_instance_read(label->raw_disk, buf, len);
}

static int
disklabel_write(device_instance *instance,
		const void *buf,
		unsigned_word len)
{
  disklabel *label = device_instance_data(instance);
  if (label->pos + len > label->sector_length)
    len = label->sector_length - label->pos;
  if (device_instance_seek(label->raw_disk, 0, label->sector_begin) < 0)
    return -1;
  return device_instance_write(label->raw_disk, buf, len);
}

static int
disklabel_seek(device_instance *instance,
	       unsigned_word pos_hi,
	       unsigned_word pos_lo)
{
  disklabel *label = device_instance_data(instance);
  if (pos_lo >= label->sector_length || pos_hi != 0)
    return -1;
  label->pos = pos_lo;
  return 0;
}


static const device_instance_callbacks package_disklabel_callbacks = {
  disklabel_delete,
  disklabel_read,
  disklabel_write,
  disklabel_seek,
};

device_instance *
pk_disklabel_create_instance(device_instance *raw_disk,
			     const char *args)
{
  disklabel *label;
  ppcboot_record_t boot_block;
  int partition = 0;

  /* read in and verify the boot record */
  if (device_instance_seek(raw_disk, 0, 0) < 0
      || (device_instance_read(raw_disk, &boot_block, sizeof(boot_block))
	  != sizeof(boot_block))
      || boot_block.signature[0] != SIGNATURE0
      || boot_block.signature[1] != SIGNATURE1) {
    return raw_disk;
  }

  /* return an instance */
  label = ZALLOC(disklabel);
  label->raw_disk = raw_disk;
  label->pos = 0;
  label->sector_begin = 512 * sector2uw(boot_block.partition[partition].sector_begin);
  label->sector_length = 512 * sector2uw(boot_block.partition[partition].sector_length);

  return device_create_instance_from(NULL, raw_disk,
				     label,
				     NULL, args,
				     &package_disklabel_callbacks);
}


#endif /* _PK_DISKLABEL_C_ */

