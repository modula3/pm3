/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:50:32 PDT 1993 by harrison
 */

#ifndef CODE_H
#define CODE_H

typedef struct Code {
    Array array;
    FileLocation file_location;
    int line_number;
} *Code;

extern void
    Code_Compile ARGS((Atom_ForwardDecl atom)),
    Code_New ARGS((void)),
    Code_Dump ARGS((FILE *stream, Code code));

extern Code
    Code_Retrieve ARGS((void));

extern Boolean
    Code_IsFunction ARGS((int flags));

#endif /* CODE_H */
