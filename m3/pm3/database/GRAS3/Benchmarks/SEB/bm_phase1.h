





/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis


 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The phase 1 module contains the tests of phase 1 of the benchmark.

 Creation date: 19.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_phase1_h
#define _bm_phase1_h

extern void BMInternalIDLookup(char *name);    /* 1  */
extern void BMAttributeRead(char *name);       /* 2  */
extern void BMAttributeWrite(char *name);      /* 3  */
extern void BMTextAttributeRead(char *name);   /* 4  */
extern void BMTextAttributeWrite(char *name);  /* 5  */
extern void BMClosureRead1N(char *name);       /* 6  */
extern void BMClosureReadMN(char *name);       /* 7  */
extern void BMClosureReverseRead1N(char *name);		/* 8  */
extern void BMClosureReverseReadMN(char *name);		/* 9  */
extern void BMSubTreeDeleteAndInsert(char *name);	/* 10 */

/*
 * IN  name
 *
 * These functions are the tests of the first phase of the Software
 * Engineering Database Benchmark.  For a thorough description, see the
 * benchmark paper.  The results of the measurements are written both to the
 * screen and to a log file (see bm_log.h).  The 'name'
 * parameter gives the name of the benchmark test database which must
 * already have been created by BMBuildDatabase.  For a closer
 * description of the semantics of the database name see
 * BMBuildDatabase() (bm_buildDB.h).
 */


#endif
