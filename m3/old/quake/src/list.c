/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Tue Jan 18 15:23:41 1994 by harrison
 */

#include "quake.h"

List List_New(first, tail)
Refany first;
List tail;
{
    List list = NEW(struct List);

    list->first = first;
    list->tail = tail;

    return list;
}

void List_Push(list, entry)
List *list;
Refany entry;
{
    *list = List_New(entry, *list);
}

Refany List_Pop(list)
List *list;
{
    Refany x = (*list)->first;
    List temp = *list;

    *list = (*list)->tail;

    Utils_FreeMemory(temp);

    return x;
}

int List_Length(list)
List list;
{
    int i = 0;
    List rest;

    for (rest = list; rest != NULL; rest = rest->tail)
	i++;

    return i;
}

