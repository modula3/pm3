/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Tue Oct  4 13:12:23 PDT 1994 by ericv      */
/*      modified on Mon Dec  6 16:17:23 PST 1993 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (setjmp(*from) == 0) longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int ThreadF__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

