





/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The clock module contains routines for measuring time.  Simply put,
 it gives the user access to one or more stop watches.

 See also bmglobal.h for type descriptions.

 Creation date: 18.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_clock_h
#define _bm_clock_h

#include "bm_system.h"


typedef struct
  {
    int running;			       /* boolean telling whether the clock is running or not */
    bm_time start;			       /* if the clock is running: the time at which it */
    /* was last started */
    bm_time elapsed;			       /* running time since the clock was reset */
  }
bm_clock;


extern void ResetClock(bm_clock * aClock);

/*
 * OUT  aClock
 *
 * This function clears the clock 'aClock' given as argument.  This
 * must be done before the clock is used and every time you want to
 * reset it (for measuring a new time).
 */


extern void StartClock(bm_clock * aClock);

/*
 * IN/OUT  aClock
 *
 * This function starts the clock.  It does not reset it, this must be
 * done with ResetClock.  Starting a clock which is already started
 * have no effect.
 */


extern void StopClock(bm_clock * aClock);

/*
 * IN/OUT  aClock
 *
 * This function stops the clock.  Stopping a clock which has already
 * been stopped will have no effect.
 */


extern void SetClock(bm_clock * aClock, bm_time new_time);

/*
 * IN  aClock
 * IN  new_time
 *
 * This function sets the elapsed time of the clock 'aClock' to 'new_time'
 * (regardless of whether the clock is running or not).
 */


extern void ReadClock(bm_clock aClock, bm_time * elapsed_time);

/*
 * IN  aClock
 * OUT elapsed_time
 *
 * This function returns the current time of the clock 'aClock', i.e.
 * total running time since reset (regardless of whether the clock is
 * running now or not).
 */


#endif
