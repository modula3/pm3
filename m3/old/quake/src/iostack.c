/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 17:29:50 PDT 1993 by harrison
 */

#include "quake.h"

static List
    IO_Stack = NULL;

void IOStack_Push(fp)
FILE *fp;
{
    List_Push(&IO_Stack, fp);
}

FILE *IOStack_Pop()
{
    return (FILE *) List_Pop(&IO_Stack);
}

FILE *IOStack_Current()
{
    return IO_Stack == NULL ? stdout : (FILE *) IO_Stack->first;
}

void IOStack_Initialize()
{
    IO_Stack = NULL;
}

