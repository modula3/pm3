/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Tue Jan 18 17:00:13 1994 by harrison 
 *      modified on Wed Apr 28 17:10:40 PDT 1993 by mjordan 
 */

#ifndef ARRAY_H
#define ARRAY_H

typedef struct Array {
    int length, next;
    Atom_ForwardDecl *body;
} *Array;

extern Array
    Array_AppendAtom ARGS((Array array, Atom_ForwardDecl atom)),
    Array_Convert ARGS((Atom_ForwardDecl atom)),
    Array_Flatten ARGS((Array array)),
    Array_New ARGS((int length));

extern void
    Array_AppendArray ARGS((Array array, Array suffix)),
    Array_Dump ARGS((FILE *stream, Array array));

extern int
    Array_GetIndex ARGS((Array array, Atom_ForwardDecl idx));

extern Boolean
    Array_IsEmpty ARGS((Array array));

#endif /* ARRAY_H */
