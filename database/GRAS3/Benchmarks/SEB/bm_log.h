/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The log module provides functions to be able to write a log of the
 benchmark run.

 Creation date: 21.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_log_h
#define _bm_log_h

#include "bm_system.h"



extern void OpenLog(char *filename, int quietmode, int append);

/*
 * IN  filename
 * IN  quietmode (TRUE/FALSE)
 * IN  append  (TRUE/FALSE)
 *
 * This function prepares the log for writing.  The log is written to
 * by Log_printf and LogResult, normally both to screen and to a
 * file.  The name of the log file is given in 'filename'.
 *
 * If the file already exists and append is true, the file is opened
 * for appended write.  If append is false and the file already exist,
 * an error message is written and the program exits.  This is also
 * the case if the path is invalid.  Otherwise the new file is created.
 * If 'filename' is NULL, no logfile is used.
 *
 * If quietmode is TRUE, the log will not be written to the screen.
 * Setting both quietmode=TRUE and filename=NULL is not allowed and
 * will result in a run time error.
 *
 * OpenLog must be called before Log_printf or LogResult.
 */


extern void Log_printf(const char *format, ...);

/*
 * Log_printf is a printf-style function writing the argument either to
 * screen or to the log file or to both of them.  LogOpen must be
 * called before calling Log_printf.
 */


extern void LogResult(const char *comment, int iter, bm_time time);

/*
 * LogResult gives a standard way of presenting the results of the
 * benchmark.  A comment describing the operation is given together
 * with how many times it was carried out ('iter') and the duration of the
 * operation.  The comment, total time (in hh:mm:ss.ms), and average
 * time per iteration will be written to the log.  If 'iter' is 0, no
 * average time will be computed.
 */



extern void CloseLog(void);

/*
 * This function closes the log file if a log file was specified.
 */





#endif
