/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:50:29 PDT 1993 by harrison
 */

#ifndef NAME_H
#define NAME_H

typedef struct Name {
    char *text;
    unsigned int hash_value;
} *Name;

extern void
    Name_Initialize ARGS((void)),
    Name_Dump ARGS((FILE *stream));

extern Name
    Name_Register ARGS((char *text)),
    Name_Convert ARGS((Atom_ForwardDecl atom));

#endif /* NAME_H */
