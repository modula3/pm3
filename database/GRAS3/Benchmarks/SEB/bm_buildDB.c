/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The build-up module creates an initial benchmark database.

 Creation date: 30.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#include "bm_global.h"
#include "bm_log.h"
#include "bm_clock.h"
#include "bm_basic.h"
#include "bm_buildDB.h"


void
BMBuildDatabase(char *name, int no_of_levels)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time build_time;
  long dbsize;

  BBLoginDB();  

  ResetClock(&clk1);
  StartClock(&clk1);

  BBCreateDB(name, &dbref);

  BBTransactionStartDB(name);
  BBCreateTree(dbref, no_of_levels);
  BBCreateRefs(dbref, no_of_levels);
  BBTransactionCommitDB(name);

  BBCloseDB(dbref);

  StopClock(&clk1);
  ReadClock(clk1, &build_time);

  LogResult("Test database creation::", 0, build_time);

  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  BBCloseDB(dbref);

  Log_printf("Original database size (bytes):: %i\n", dbsize);

  BBLogoutDB();
  BBBackupDB(name);			       /* save copy of original database */
}
