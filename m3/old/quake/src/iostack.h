/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:50:30 PDT 1993 by harrison
 */

#ifndef IOSTACK_H
#define IOSTACK_H

extern void
    IOStack_Push ARGS((FILE *fp)),
    IOStack_Initialize ARGS((void));

extern FILE
    *IOStack_Pop ARGS((void)),
    *IOStack_Current ARGS((void));

#endif /* IOSTACK_H */
