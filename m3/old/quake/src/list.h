/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 17:29:49 PDT 1993 by harrison
 */

#ifndef LIST_H
#define LIST_H

typedef struct List {
    Refany first;
    struct List *tail;
} *List;

extern List List_New ARGS((Refany first, List tail));
extern void List_Push ARGS((List *list, Refany entry));
extern Refany List_Pop ARGS((List *list));
extern int List_Length ARGS((List list));

#endif /* LIST_H */
