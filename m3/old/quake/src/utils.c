/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Nov 30 08:33:02 PST 1994 by kalsow  
 *      modified on Fri Jan 21 15:04:18 1994 by harrison
 */

#include "quake.h"
#if !defined(TARGET_NEXT)
#include <malloc.h>
#endif

#ifndef DEBUG_MALLOC
Refany Utils_GetMemory(size)
int size;
{
    char *m = malloc((unsigned) size);
    
    if (m == NULL && size > 0)
	yyerror("internal error: out of memory (in Utils_GetMemory)");

    return (Refany) m;
}

Refany Utils_AdjustMemory(orig, new_size)
Refany orig;
int new_size;
{
    char *m = orig == NULL ?
	    malloc((unsigned) new_size) :
	    realloc(orig, (unsigned) new_size);

    if (m == NULL)
	yyerror("internal error: out of memory (in Utils_AdjustMemory)");

    return (Refany) m;
}

void Utils_FreeMemory(m)
Refany m;
{
    free(m);
}

void Utils_CopyMemory(from, to, bytes)
Refany from;
Refany to;
int bytes;
{
    memcpy(to, from, bytes);
}

void Utils_ClearMemory(dest, bytes)
Refany dest;
int bytes;
{
    memset(dest, 0, bytes);
}
#endif /* !DEBUG_MALLOC */

char *Utils_StringSave(s)
char *s;
{
    return strcpy(NEW_ARRAY(char, strlen(s) + 1), s);
}

Boolean Utils_ReadInteger(s, i)
char *s;
int *i;
{
    int result = 0;
    char *p;
    Boolean valid = FALSE;

    for (p = s; *p != '\0'; p++)
	switch (*p) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    result = result * 10 + (*p - '0');
	    valid = TRUE;
	    break;
	default:
	    return FALSE;
	}

    *i = result;

    return valid;
}

BulkAlloc *Utils_NewBulkAlloc(size, count)
unsigned int size;
unsigned int count;
{
    BulkAlloc *bulk = NEW(struct BulkAlloc);

    bulk->size = size;
    bulk->count = count;
    bulk->next = count;		/* force reload in Utils_GetBulk */
    bulk->buffer = NULL;

    return bulk;
}

Refany Utils_GetBulk(bulk)
BulkAlloc *bulk;
{
    if (bulk->next >= bulk->count) {
	bulk->buffer = NEW_ARRAY(char, bulk->size * bulk->count);
	bulk->next = 0;
    }

    RANGE_CHECK(bulk->next, 0, bulk->count);
    return (Refany) &((char *) bulk->buffer)[bulk->next++ * bulk->size];
}


