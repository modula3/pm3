/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The log module provides functions to be able to write a log of the
 benchmark run.

 Creation date: 26.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include "bm_system.h"
#include "bm_log.h"
#include <ansi.h>

FILE *logfile = NULL;
int qmode = FALSE;

void
OpenLog(char *filename, int quietmode, int append)
{
  char *systeminfo;

  if (!filename && quietmode)		       /* must have log for -q */
    bm_error("open_log",
     "Illegal combination of parameters: filename==NULL && quietmode==TRUE",
	     FALSE);

  qmode = quietmode;

  if (filename)
    {
      if (append)
	logfile = fopen(filename, "a");	       /* append */
      else
	logfile = fopen(filename, "w");	       /* write */

      if (!logfile)
	{
	  sprintf(err_msg_buf, "Could not open log file: %s", filename);
	  bm_error("open_log", err_msg_buf, TRUE);
	}
    }

  if (append)
    Log_printf("\n\n\n(append)\n\n\n");
  Log_printf("Software Engineering Database Benchmark (S E B)\n");
  Log_printf("====================================================\n");
  Log_printf("The benchmark was started: %s", get_local_timedate());
  Log_printf("----------------------------------------------------\n");
  Log_printf("System information:\n");
  systeminfo = get_system_info();
  if (!systeminfo)
    Log_printf("\t(not available)\n");
  else
    Log_printf(systeminfo);
  Log_printf("----------------------------------------------------\n");
}



void
Log_printf(const char *format, ...)
{
  va_list args;

  va_start(args, format);

  if (logfile)
    vfprintf(logfile, format, args);
  if (!qmode)
    vfprintf(stderr, format, args);
  va_end(args);
}


void
LogResult(const char *comment, int iter, bm_time bmtime)
{
  int h, min;
  float s;
  bm_time avg_time;

  bm_time2hms(bmtime, &h, &min, &s);

  Log_printf("\n%s\n\tTotal time: %i:%i:%f\n", comment, h, min, s);

  if (iter > 0)
    {
      div_time(bmtime, iter, &avg_time);
      bm_time2hms(avg_time, &h, &min, &s);
      Log_printf("\tAverage time: %i:%i:%f (%i entities)\n", h, min, s, iter);
    }
}


void
CloseLog()
{
  Log_printf("----------------------------------------------------\n");
  Log_printf("The benchmark ended: %s", get_local_timedate());
  if (logfile)
    fclose(logfile);
}
