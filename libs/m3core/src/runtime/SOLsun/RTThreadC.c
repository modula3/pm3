/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Mon Oct 24 14:54:13 PDT 1994 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <ucontext.h>

void RTThread__Transfer (ucontext_t *from, ucontext_t *to)
{
  if (getcontext(from) == 0) {
    to->uc_mcontext.gregs[REG_O0] = (greg_t)1; /* emulate longjmp return */
    setcontext(to);		/* fire it up */
  }
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int RT0u__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

