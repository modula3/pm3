/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Sep  7 09:21:54 PDT 1994 by kalsow  
 *      modified on Tue Jan 18 16:56:45 1994 by harrison
 */

#ifndef UTILS_H
#define UTILS_H

typedef struct BulkAlloc {
    unsigned int size, count, next;
    Refany buffer;
} BulkAlloc;

extern Refany
    Utils_GetBulk ARGS((BulkAlloc *bulk));

extern char
    *Utils_StringSave ARGS((char *s));

extern Boolean
    Utils_ReadInteger ARGS((char *s, int *i));

extern BulkAlloc
    *Utils_NewBulkAlloc ARGS((unsigned int size, unsigned int count));

#ifdef	DEBUG_MALLOC
#include "malloc.h"

#define Utils_GetMemory(size)			malloc(size)
#define Utils_AdjustMemory(orig, new_size)	realloc(orig, new_size)
#define Utils_FreeMemory(p)			free(p)
#define Utils_CopyMemory(from, to, bytes)	memcpy(to, from, bytes)
#define Utils_ClearMemory(dest, bytes)		memset(dest, 0, bytes)
#else
extern Refany
    Utils_GetMemory ARGS((int size)),
    Utils_AdjustMemory ARGS((Refany orig, int new_size));

extern void
    Utils_CopyMemory ARGS((Refany from, Refany to, int bytes)),
    Utils_ClearMemory ARGS((Refany dest, int bytes)),
    Utils_FreeMemory ARGS((Refany m));
#endif /* DEBUG_MALLOC */

#endif /* UTILS_H */
