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
 26.03.92   F.H.         Bug correction: type of 'selected2' intIDtype
 (was extIDtype)
 ----------------------------------------------------------------------

 ***************************************************************************/

#include "bm_phase3.h"
#include "bm_basic.h"
#include "bm_global.h"
#include "bm_clock.h"
#include "bm_random.h"
#include "bm_log.h"

void
BMNoTransactions(char *name)
{
  bm_clock clk1;				       /* the clock doing the measurements */
  dbreftype dbref;			       /* database pointer */
  bm_time run_time;			       /* running time of operations */
  extIDtype extID;
  extIDtype selected[CLOSURE_SELECT];	       /* nodes selected for delete/insert ops */
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
  int i, j;				       /* counters */
  long dbsize;				       /* size of database (bytes) */

  BBRestoreDB(name);

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("----------------------------------------------------\n");
  Log_printf("Original database size (bytes):: %i\n\n", dbsize);

  for (i = 0; i < CLOSURE_SELECT; i += 2)
    {					       /* Find nodes for subtree */
      /* delete/insert */
      do
	{				       /* avoid duplicates */
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

  BBTransactionStartDB(name);
  
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      extID = GetRandomClosureLevelNode();
      selected2[i] = BBInternalIDLookup(dbref, extID);
    }

  BBTransactionCommitDB(name);

  BBEmptyCacheDB(name, &dbref);

  totalsize = 0;
  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  
  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree delete */
      BBSubTreeDelete(dbref, selected[i], &size_of_closure);
      totalsize += size_of_closure;
    }

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree insert */
      BBSubTreeCreate(dbref, selected[i], no_of_children[i], &size_of_closure);
      totalsize += size_of_closure;
    }
  for (i = 0; i < CLOSURE_SELECT; i++)	       /* subtree refs create */
    BBSubTreeRefsCreate(dbref, selected[i]);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* closureread1n */
      BBClosureRead1N(dbref, selected2[i], FALSE, &size_of_closure);
      totalsize += size_of_closure;
    }

  BBTransactionCommitDB(name);
  
  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("Single transaction::", totalsize, run_time);

  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("Database size after 'SingleTransaction' (bytes):: %i\n\n", dbsize);
  Log_printf("----------------------------------------------------\n");

  BBCloseDB(dbref);
  BBLogoutDB();
}





void
BMAbortTransaction(char *name)
{
  bm_clock clk1, clk2, clk3;		       /* the clocks doing the measurements */
  dbreftype dbref;			       /* database pointer */
  bm_time run_time;			       /* running time of operations */
  extIDtype extID;
  extIDtype selected[CLOSURE_SELECT];	       /* nodes selected for delete/insert ops */
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
  int i, j;				       /* counters */
  long dbsize;				       /* size of database (bytes) */

  BBRestoreDB(name);

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("----------------------------------------------------\n");
  Log_printf("Original database size (bytes):: %i\n\n", dbsize);

  for (i = 0; i < CLOSURE_SELECT; i += 2)
    {					       /* Find nodes for subtree */
      /* delete/insert */
      do
	{				       /* avoid duplicates */
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

  BBTransactionStartDB(name);
  
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      extID = GetRandomClosureLevelNode();
      selected2[i] = BBInternalIDLookup(dbref, extID);
    }

  BBTransactionCommitDB(name);

  BBEmptyCacheDB(name, &dbref);

  totalsize = 0;
  ResetClock(&clk1);
  ResetClock(&clk2);
  ResetClock(&clk3);
  StartClock(&clk1);
  StartClock(&clk2);

  BBTransactionStartDB(name);

  StopClock(&clk2);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree delete */
      BBSubTreeDelete(dbref, selected[i], &size_of_closure);
      totalsize += size_of_closure;
    }

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree insert */
      BBSubTreeCreate(dbref, selected[i], no_of_children[i], &size_of_closure);
      totalsize += size_of_closure;
    }
  for (i = 0; i < CLOSURE_SELECT; i++)	       /* subtree refs create */
    BBSubTreeRefsCreate(dbref, selected[i]);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* closureread1n */
      BBClosureRead1N(dbref, selected2[i], FALSE, &size_of_closure);
      totalsize += size_of_closure;
    }

  StartClock(&clk3);

  BBTransactionAbortDB(name);

  StopClock(&clk3);
  StopClock(&clk1);

  ReadClock(clk1, &run_time);
  LogResult("Abort transaction (total time)::", totalsize, run_time);

  ReadClock(clk2, &run_time);
  LogResult("Abort transaction (start transaction time)::", 0, run_time);

  ReadClock(clk3, &run_time);
  LogResult("Abort transaction (abort transaction time)::", 0, run_time);

  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("Database size after 'AbortTransactions' (bytes):: ");
  Log_printf("%i\n\n", dbsize);
  Log_printf("----------------------------------------------------\n");

  BBCloseDB(dbref);
  BBLogoutDB();

}


void
BMCommitTransaction(char *name)
{
  bm_clock clk1, clk2, clk3;		       /* the clocks doing the measurements */
  dbreftype dbref;			       /* database pointer */
  bm_time run_time;			       /* running time of operations */
  extIDtype extID;
  extIDtype selected[CLOSURE_SELECT];	       /* nodes selected for delete/insert ops */
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
  int i, j;				       /* counters */
  long dbsize;				       /* size of database (bytes) */

  BBRestoreDB(name);

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("----------------------------------------------------\n");
  Log_printf("Original database size (bytes):: %i\n\n", dbsize);

  for (i = 0; i < CLOSURE_SELECT; i += 2)
    {					       /* Find nodes for subtree */
      /* delete/insert */
      do
	{				       /* avoid duplicates */
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

  BBTransactionStartDB(name);
  
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      extID = GetRandomClosureLevelNode();
      selected2[i] = BBInternalIDLookup(dbref, extID);
    }

  BBTransactionCommitDB(name);

  BBEmptyCacheDB(name, &dbref);

  totalsize = 0;
  ResetClock(&clk1);
  ResetClock(&clk2);
  ResetClock(&clk3);
  StartClock(&clk1);
  StartClock(&clk2);

  BBTransactionStartDB(name);

  StopClock(&clk2);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree delete */
      BBSubTreeDelete(dbref, selected[i], &size_of_closure);
      totalsize += size_of_closure;
    }

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree insert */
      BBSubTreeCreate(dbref, selected[i], no_of_children[i], &size_of_closure);
      totalsize += size_of_closure;
    }
  for (i = 0; i < CLOSURE_SELECT; i++)	       /* subtree refs create */
    BBSubTreeRefsCreate(dbref, selected[i]);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* closureread1n */
      BBClosureRead1N(dbref, selected2[i], FALSE, &size_of_closure);
      totalsize += size_of_closure;
    }

  StartClock(&clk3);

  BBTransactionCommitDB(name);

  StopClock(&clk3);
  StopClock(&clk1);

  ReadClock(clk1, &run_time);
  LogResult("Commit transaction (total time)::", totalsize, run_time);

  ReadClock(clk2, &run_time);
  LogResult("Commit transaction (start transaction time)::", 0, run_time);

  ReadClock(clk3, &run_time);
  LogResult("Commit transaction (commit transaction time)::", 0, run_time);

  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("Database size after 'CommitTransactions' (bytes):: ");
  Log_printf("%i\n\n", dbsize);
  Log_printf("----------------------------------------------------\n");

  BBCloseDB(dbref);
  BBLogoutDB();
}




void
BMManyTransactions(char *name)
{
  bm_clock clk1, clk2, clk3;		       /* the clocks doing the measurements */
  dbreftype dbref;			       /* database pointer */
  bm_time run_time;			       /* running time of operations */
  extIDtype extID;
  extIDtype selected[CLOSURE_SELECT];	       /* nodes selected for delete/insert ops */
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
  int i, j;				       /* counters */
  long dbsize;				       /* size of database (bytes) */

  BBRestoreDB(name);

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("----------------------------------------------------\n");
  Log_printf("Original database size (bytes):: %i\n\n", dbsize);

  for (i = 0; i < CLOSURE_SELECT; i += 2)
    {					       /* Find nodes for subtree */
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
  
  BBTransactionStartDB(name);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      extID = GetRandomClosureLevelNode();
      selected2[i] = BBInternalIDLookup(dbref, extID);
    }

  BBTransactionCommitDB(name);

  BBEmptyCacheDB(name, &dbref);

  totalsize = 0;
  ResetClock(&clk1);
  ResetClock(&clk2);
  ResetClock(&clk3);

  StartClock(&clk1);

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree delete */
      StartClock(&clk2);
      BBTransactionStartDB(name);
      StopClock(&clk2);
      BBSubTreeDelete(dbref, selected[i], &size_of_closure);
      StartClock(&clk3);
      BBTransactionCommitDB(name);
      StopClock(&clk3);
      totalsize += size_of_closure;
    }

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree insert */
      StartClock(&clk2);
      BBTransactionStartDB(name);
      StopClock(&clk2);
      BBSubTreeCreate(dbref, selected[i], no_of_children[i], &size_of_closure);
      StartClock(&clk3);
      BBTransactionCommitDB(name);
      StopClock(&clk3);
      totalsize += size_of_closure;
    }
  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* subtree refs create */
      StartClock(&clk2);
      BBTransactionStartDB(name);
      StopClock(&clk2);
      BBSubTreeRefsCreate(dbref, selected[i]);
      StartClock(&clk3);
      BBTransactionCommitDB(name);
      StopClock(&clk3);
    }

  for (i = 0; i < CLOSURE_SELECT; i++)
    {					       /* closureread1n */
      StartClock(&clk2);
      BBTransactionStartDB(name);
      StopClock(&clk2);
      BBClosureRead1N(dbref, selected2[i], FALSE, &size_of_closure);
      StartClock(&clk3);
      BBTransactionCommitDB(name);
      StopClock(&clk3);
      totalsize += size_of_closure;
    }

  StopClock(&clk1);

  ReadClock(clk1, &run_time);
  LogResult("\nCommit transactions (many) (total time)::", totalsize, run_time);

  ReadClock(clk2, &run_time);
  LogResult("Commit transactions (many) (start transaction time)::",
	    0, run_time);

  ReadClock(clk3, &run_time);
  LogResult("Commit transactions (many) (commit transaction time)::",
	    0, run_time);

  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("Database size after 'CommitTransactions (many)' (bytes)::");
  Log_printf(" %i\n\n", dbsize);
  Log_printf("----------------------------------------------------\n");

  BBCloseDB(dbref);
  BBLogoutDB();

}
