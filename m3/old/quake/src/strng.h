/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed May 11 15:43:52 PDT 1994 by kalsow  
 *      modified on Wed Aug 25 15:55:28 PDT 1993 by harrison
 */

#ifndef STRING_H
#define STRING_H

typedef struct String {
    char *body;
} *String;

extern String
    String_New ARGS((char *body)),
    String_Catenate ARGS((String s1, String s2)),
    String_Escape ARGS((String s)),
    String_Convert ARGS((Atom_ForwardDecl atom)),
    String_FromBoolean ARGS((Boolean boolean));

extern int
    String_Length ARGS((String string));

extern Boolean
    String_ToBoolean ARGS((String string)),
    String_IsEmpty ARGS((String string));

#endif /* STRING_H */
