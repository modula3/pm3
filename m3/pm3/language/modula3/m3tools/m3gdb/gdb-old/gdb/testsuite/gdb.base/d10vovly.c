
/*
 * Ovlymgr.c -- Runtime Overlay Manager for the Mitsubishi D10V
 */

#include "ovlymgr.h"

/* Local functions and data: */

extern unsigned long _ovly_table[][4], _novlys;
enum ovly_index { VMA, SIZE, LMA, MAPPED };
enum ovly_direction { IN, OUT };

static void D10VCopy (long dst, long src, long size);

/* OverlayLoad:
 * Copy the overlay into its runtime region,
 * and mark the overlay as "mapped".
 */

void
OverlayLoad (int ovlyno)
{
  int i;

  if (ovlyno < 0 || ovlyno >= _novlys)
    exit (-1);	/* fail, bad ovly number */

  if (_ovly_table[ovlyno][MAPPED])
    return;	/* this overlay already mapped -- nothing to do! */

  for (i = 0; i < _novlys; i++)
    if (i == ovlyno)
      _ovly_table[i][MAPPED] = 1;	/* this one now mapped */
    else if (_ovly_table[i][VMA] == _ovly_table[ovlyno][VMA])
      _ovly_table[i][MAPPED] = 0;	/* this one now un-mapped */

  /* copy overlay using D10V DMAP register */
  D10VCopy (_ovly_table[ovlyno][VMA], _ovly_table[ovlyno][LMA], 
	    _ovly_table[ovlyno][SIZE]);
}

/* OverlayUnload:
 * Copy the overlay back into its "load" region.
 * Does NOT mark overlay as "unmapped", therefore may be called 
 * more than once for the same mapped overlay.
 */

void
OverlayUnload (int ovlyno)
{
  if (ovlyno < 0 || ovlyno >= _novlys)
    exit (-1);	/* fail, bad ovly number */

  if (!_ovly_table[ovlyno][MAPPED])
    exit (-1);	/* error, can't copy out a segment that's not "in" */

  D10VCopy (_ovly_table[ovlyno][LMA], _ovly_table[ovlyno][VMA], 
	    _ovly_table[ovlyno][SIZE]);
}

/* D10VCopy:
 * Copy a region of memory from src to dest.
 * Like memcpy, but can copy anywhere in Universal, Data or Instruction memory.
 */

#define DMAP       (*(int *)(0xff04))

static void 
D10VCopy (long dst, long src, long size)
{
  register long *s, *d;
  register long tmp, i;
  register short dmap_src, dmap_dst;
  short          dmap_save;

  /* all section sizes should by multiples of 4 bytes */
  dmap_save = DMAP;

  while (size > 0)
    {
      switch (src >> 24) 
	{
	case 0:	/* src is a 24-bit unified memory address */
	  dmap_src = src >> 14;
	  s = (long *)(0x8000 + (int)(src & 0x3fff));
	  break;
	case 1:	/* src is an 18-bit instruction address */
	  src &= 0x3ffff;  /* mask off any high bits */
	  dmap_src = (src >> 14) | 0x1000;
	  s = (long *)(0x8000 + (int)(src & 0x3fff));
	  break;
	case 2:	/* src is a 15 bit data address */
	  dmap_src = 0;
	  s = (long *) ((int)src & 0x7fff);
	  break;
	case 3:	/* src is a ?? */
	default:
	  exit (-1);	/* error */
	}

      switch (dst >> 24) 
	{
	case 0:	/* dst is a 24-bit unified memory address */
	  dmap_dst = dst >> 14;
	  d = (long *)(0x8000 + (int)(dst & 0x3fff));
	  break;
	case 1:	/* dst is an 18-bit instruction address */
	  dst &= 0x3ffff;  /* mask off any high bits */
	  dmap_dst = (dst >> 14) | 0x1000;
	  d = (long *)(0x8000 + (int)(dst & 0x3fff));
	  break;
	case 2:	/* dst is a 15 bit data address */
	  dmap_dst = 0;
	  d = (long *) ((int)dst & 0x7fff);
	  break;
	case 3:	/* dst is a ?? */
	default:
	  exit (-1);	/* error */
	}

      for (i = 0; i <= 0xfff && size > 0; i++, size -= 4)
	{ /* copy up to 16k bytes */
	  DMAP = dmap_src;
	  tmp = *s++;
	  DMAP = dmap_dst;
	  *d++ = tmp; 
	}
      if (size > 0)
	{
	  src += 0x4000;
	  dst += 0x4000;
	}
    }
  DMAP = dmap_save;
}
