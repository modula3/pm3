




/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The phase 3 module contains the tests of phase 3 of the benchmark.

 Creation date: 20.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/
#ifndef _bm_phase3_h
#define _bm_phase3_h

extern void BMNoTransactions(char *name);
extern void BMAbortTransaction(char *name);
extern void BMCommitTransaction(char *name);
extern void BMManyTransactions(char *name);

/*
 * IN  name
 *
 * This function makes up the third phase of the Software Engineering
 * Database Benchmark.  For a thorough description, see the benchmark
 * paper.  The results of the measurements are written both to the
 * screen and to a log file (see bm_log.h).  The 'name'
 * parameter gives the name of the benchmark test database which must
 * already have been created by BMBuildDatabase.  For a closer
 * description of the semantics of the database name see
 * BMBuildDatabase() (bm_buildDB.h).
 */


#endif
