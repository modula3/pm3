/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:55:28 PDT 1993 by harrison
 */

#ifndef STACK_H
#define STACK_H

extern void
    Push ARGS((Atom));

extern Atom
    Pop ARGS((Tag tag)),
    Pop_Any ARGS((void));

extern Array
    Pop_Array ARGS((void));
extern Code
    Pop_Code ARGS((void));
extern Designator
    Pop_Designator ARGS((void));
extern Integer
    Pop_Integer ARGS((void));
extern Table
    Pop_Table ARGS((void));
extern String
    Pop_String ARGS((void));
extern void
    Pop_Mark ARGS((void));

extern void
    Stack_Dump ARGS((FILE *stream));
extern int Stack_CountToMark ARGS((void));

#endif /* STACK_H */
