
/*
 * Ovlymgr.c -- Runtime Overlay Manager for the Mitsubishi D10V
 */

#include "ovlymgr.h"

extern unsigned long _ovly_table[][4], _novlys;
enum ovly_index { VMA, SIZE, LMA, MAPPED};

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

  memcpy ((void *) _ovly_table[ovlyno][VMA], 
	  (void *) _ovly_table[ovlyno][LMA], 
		   _ovly_table[ovlyno][SIZE]);

  return;
}
