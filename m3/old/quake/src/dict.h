/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Feb 21 21:12:06 1994 by harrison
 */

#ifndef DICT_H
#define DICT_H

typedef Hash_Table Dictionary;

typedef unsigned int DictFlags;

#define	DICTFLAGS_NONE		(0)
#define	DICTFLAGS_LOCAL		BIT(0)
#define	DICTFLAGS_READONLY	BIT(1)

extern struct Atom
    *Dict_Lookup ARGS((Name name, Boolean create)),
    /* Install returns a pointer the the installed symbol's atom.  This can
       save us some time in, say, the foreach operator, but is rather ugly! */
    **Dict_Install ARGS((Name name, Atom_ForwardDecl atom, DictFlags flags));

extern void
    Dict_DecrementScope ARGS((void)),
    Dict_DumpAll ARGS((FILE *stream)),
    Dict_IncrementScope ARGS((void)),
    Dict_Initialize ARGS((void));

#endif /* DICT_H */
