/***************************************************************************
 File:    bm_clock.c
 Author:  Frode H\ogberg
 Project: Diploma Thesis
 SCCS ID: %W%  %G%

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 See header file for closer description.

 Creation date: 20.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#include "bm_system.h"
#include "bm_global.h"
#include "bm_clock.h"

void
ResetClock(bm_clock * aClock)
{
  aClock->running = FALSE;
  aClock->elapsed.sec = 0;
  aClock->elapsed.usec = 0;
}


void
StartClock(bm_clock * aClock)
{
  if (aClock->running)
    return;				       /* Clock is already running */

  aClock->running = TRUE;
  get_time(&(aClock->start));
}



void
StopClock(bm_clock * aClock)
{
  bm_time stop_time, run_time;

  if (!(aClock->running))
    return;				       /* clock is already stopped */

  aClock->running = FALSE;
  get_time(&stop_time);
  diff_time(aClock->start, stop_time, &run_time);
  add_time(aClock->elapsed, run_time, &(aClock->elapsed));
}


void
SetClock(bm_clock * aClock, bm_time new_time)
{
  aClock->elapsed.sec = new_time.sec;
  aClock->elapsed.usec = new_time.usec;
}


void
ReadClock(bm_clock aClock, bm_time * elapsed_time)
{
  bm_time lap_time, run_time;

  if (aClock.running)
    {
      get_time(&lap_time);
      diff_time(aClock.start, lap_time, &run_time);
      add_time(aClock.elapsed, run_time, elapsed_time);
    }
  else
    {
      elapsed_time->sec = aClock.elapsed.sec;
      elapsed_time->usec = aClock.elapsed.usec;
    }
}
