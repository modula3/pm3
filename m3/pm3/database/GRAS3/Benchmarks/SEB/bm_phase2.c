/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis


 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The phase 2 module contains the tests of phase 2 of the benchmark.

 Creation date: 11.02.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 26.03.93   F.H.         Corrected bug: type of 'selected' should be intIDtype
 (was extIDtype), needed new variable to hold extIDs
 for nodes selected for delete/insert: 'selected2'
 ***************************************************************************/

#include "bm_phase2.h"
#include "bm_basic.h"
#include "bm_global.h"
#include "bm_clock.h"
#include "bm_random.h"
#include "bm_system.h"
#include "bm_log.h"

void
BMRestructuring(char *name)
{
  bm_clock clk1;			       /* the clock doing the measurements */
  dbreftype dbref;			       /* database pointer */
  bm_time run_time;			       /* running time of operations */
  extIDtype extID;
  extIDtype selected[CLOSURE_SELECT];	       /* nodes selected for delete/insert */

  /* of subtree */
  intIDtype selected2[CLOSURE_SELECT];	       /* nodes selected for closure ops */
  int no_of_children[CLOSURE_SELECT];	       /* no of children for the */

  /* root of new subtrees */
  /* to be inserted */
  int size_of_closure;			       /* size of closure */

  /* deleted/created/traversed */
  int totalsize;			       /* summing up closures */
  int already_chosen;			       /* boolean telling whether a */

  /* subtree has already been */
  /* chosen for delete/insert */
  int i, j, k;				       /* counters */
  long dbsize;				       /* size of database */



  BBRestoreDB(name);
  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("----------------------------------------------------\n");
  Log_printf("Original database size (bytes):: %i\n\n", dbsize);

  /* Restructure "all" of database */
  for (k = 0; k < (power(AVG_CHILDREN, CLOSURE_LEVEL) / CLOSURE_SELECT); k++)
    {

      for (i = 0; i < CLOSURE_SELECT; i += 2)
	{				       /* Find nodes for subtree */
	  /* delete/insert */
	  do
	    {
	      already_chosen = FALSE;
	      selected[i] = GetRandomClosureLevelNode();
	      for (j = 0; j < i; j++)
		if (selected[j] == selected[i])
		  already_chosen = TRUE;
	    }
	  while (already_chosen);
	  no_of_children[i] = GetRandomNoOfChildren();
	  selected[i + 1] = (selected[i] % 2 == 0) ?	/* determine other node in pair */
	    selected[i] + 1 : selected[i] - 1;
	  no_of_children[i + 1] = MAX_CHILDREN - no_of_children[i];
	}

      BBEmptyCacheDB(name, &dbref);	       /* Subtree delete */

      totalsize = 0;
      ResetClock(&clk1);
      StartClock(&clk1);

      BBTransactionStartDB(name);
      for (i = 0; i < CLOSURE_SELECT; i++)
	{
	  BBSubTreeDelete(dbref, selected[i], &size_of_closure);
	  totalsize += size_of_closure;
	}
      BBTransactionCommitDB(name);

      StopClock(&clk1);
      ReadClock(clk1, &run_time);

      LogResult("SubTreeDelete", totalsize, run_time);

      BBEmptyCacheDB(name, &dbref);

      totalsize = 0;			       /* Subtree insert */
      ResetClock(&clk1);
      StartClock(&clk1);

      BBTransactionStartDB(name);
      for (i = 0; i < CLOSURE_SELECT; i++)
	{
	  BBSubTreeCreate(dbref, selected[i], no_of_children[i], &size_of_closure);
	  totalsize += size_of_closure;
	}
      for (i = 0; i < CLOSURE_SELECT; i++)
	BBSubTreeRefsCreate(dbref, selected[i]);
      BBTransactionCommitDB(name);

      StopClock(&clk1);
      ReadClock(clk1, &run_time);

      LogResult("SubTreeInsert::", totalsize, run_time);

      BBEmptyCacheDB(name, &dbref);	       /* ClosureRead1N */

      BBTransactionStartDB(name);
      
      for (i = 0; i < CLOSURE_SELECT; i++)
	{
	  extID = GetRandomClosureLevelNode();
	  selected2[i] = BBInternalIDLookup(dbref, extID);
	}

      BBTransactionCommitDB(name);
      
      totalsize = 0;
      ResetClock(&clk1);
      StartClock(&clk1);

      BBTransactionStartDB(name);
      for (i = 0; i < CLOSURE_SELECT; i++)
	{
	  BBClosureRead1N(dbref, selected2[i], FALSE, &size_of_closure);
	  totalsize += size_of_closure;
	}
      BBTransactionCommitDB(name);

      StopClock(&clk1);
      ReadClock(clk1, &run_time);

      LogResult("ClosureRead1N (unordered)::", totalsize, run_time);

      BBEmptyCacheDB(name, &dbref);	       /* ClosureReadMN */

      BBTransactionStartDB(name);
      
      for (i = 0; i < CLOSURE_SELECT; i++)
	{
	  extID = GetRandomClosureLevelNode();
	  selected2[i] = BBInternalIDLookup(dbref, extID);
	}

      BBTransactionCommitDB(name);

      totalsize = 0;
      ResetClock(&clk1);
      StartClock(&clk1);

      BBTransactionStartDB(name);
      for (i = 0; i < CLOSURE_SELECT; i++)
	{
	  BBClosureReadMN(dbref, selected2[i], FALSE, &size_of_closure);
	  totalsize += size_of_closure;
	}
      BBTransactionCommitDB(name);

      StopClock(&clk1);
      ReadClock(clk1, &run_time);

      LogResult("ClosureReadMN (unordered)::", totalsize, run_time);
      BBGetSizeDB(name, &dbref, &dbsize);
      Log_printf("Database size (bytes) after %i iteration(s) ", k + 1);
      Log_printf("of restructuring :: %i\n\n", dbsize);
    }

  Log_printf("----------------------------------------------------\n");
  BBCloseDB(dbref);
  BBLogoutDB();
}
