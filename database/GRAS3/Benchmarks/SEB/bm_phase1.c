/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis


 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The phase 1 module contains the tests of phase 1 of the benchmark.

 NB!  BBRestoreDB() is not run by any of the functions in this module,
 thus, if phase 1 is run after phase 2 or phase 3, a changed,
 not the original database will be used.

 Creation date: 03.02.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 06.04.93   F.H.         Added measurements of OpenDB, CloseDB, LoginDB,
 and LogoutDB (in BMInternalIDLookup)
 ----------------------------------------------------------------------
 ***************************************************************************/

#include "bm_global.h"
#include "bm_system.h"
#include "bm_clock.h"
#include "bm_random.h"
#include "bm_log.h"
#include "bm_phase1.h"
#include "bm_basic.h"

void
BMInternalIDLookup(char *name)
{
  bm_clock clk1;
  dbreftype dbref;			       /* database */
  bm_time run_time;			       /* running time of test */
  extIDtype selected[SINGLE_SELECT];	       /* random nodes selected for */

  /* test */
  intIDtype intID;			       /* internalID returned by */

  /* BBInternalIDLookup */
  int i;				       /* counter */

  ResetClock(&clk1);
  StartClock(&clk1);
  BBLoginDB();
  StopClock(&clk1);
  ReadClock(clk1, &run_time);
  LogResult("LoginDB::", 0, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);
  BBOpenDB(name, &dbref);
  StopClock(&clk1);
  ReadClock(clk1, &run_time);
  LogResult("OpenDB::", 0, run_time);

  BBEmptyCacheDB(name, &dbref);

  for (i = 0; i < SINGLE_SELECT; i++)
    selected[i] = GetRandomAllNode();

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    intID = BBInternalIDLookup(dbref, selected[i]);
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ExternalIDLookup (cold)::", SINGLE_SELECT, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    intID = BBInternalIDLookup(dbref, selected[i]);
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ExternalIDLookup (warm)::", SINGLE_SELECT, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);
  BBCloseDB(dbref);
  StopClock(&clk1);
  ReadClock(clk1, &run_time);
  LogResult("CloseDB::", 0, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);
  BBLogoutDB();
  StopClock(&clk1);
  ReadClock(clk1, &run_time);
  LogResult("LogoutDB::", 0, run_time);

}


void
BMAttributeRead(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype selected[SINGLE_SELECT];
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  for (i = 0; i < SINGLE_SELECT; i++)
    selected[i] = GetRandomAllNode();

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    BBAttributeRead(dbref, selected[i]);       /* million attribute is read but */
  /* not returned */
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("AttributeRead (cold)::", SINGLE_SELECT, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    BBAttributeRead(dbref, selected[i]);
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("AttributeRead (warm)::", SINGLE_SELECT, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}




void
BMAttributeWrite(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype selected[SINGLE_SELECT];
  int million;
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  i = 0;
  while (i < SINGLE_SELECT)
    {
      selected[i] = GetRandomAllNode();
      if (selected[i] != 1)
	i++;				       /* don't write to million */
      /* attribute of node nr 1 */
      /* (contains height of DB) */
    }

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      million = GetRandomMillionValue();
      BBAttributeWrite(dbref, selected[i], million);
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("AttributeWrite (cold)::", SINGLE_SELECT, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      million = GetRandomMillionValue();
      BBAttributeWrite(dbref, selected[i], million);
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("AttributeWrite (warm)::", SINGLE_SELECT, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}


void
BMTextAttributeRead(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype selected[SINGLE_SELECT];
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  for (i = 0; i < SINGLE_SELECT; i++)
    selected[i] = GetRandomTextNode();

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    BBTextAttributeRead(dbref, selected[i]);   /* text attribute is read but */
  /* not returned */
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("TextAttributeRead (cold)::", SINGLE_SELECT, run_time);

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    BBTextAttributeRead(dbref, selected[i]);
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("TextAttributeRead (warm)::", SINGLE_SELECT, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}


void
BMTextAttributeWrite(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype selected[SINGLE_SELECT];
  char text[TEXT_MAX_LEN + 1];
  int textlen;
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  for (i = 0; i < SINGLE_SELECT; i++)
    selected[i] = GetRandomTextNode();

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      GetRandomText(text, &textlen);
      BBTextAttributeWrite(dbref, selected[i], text, textlen);
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("TextAttributeWrite (cold)::", SINGLE_SELECT, run_time);


  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      GetRandomText(text, &textlen);
      BBTextAttributeWrite(dbref, selected[i], text, textlen);
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("TextAttributeWrite (warm)::", SINGLE_SELECT, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}


void
BMClosureRead1N(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype extID;
  intIDtype selected[CLOSURE_SELECT];
  int size_of_closure;
  int totalsize;
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  /* rene */

  BBTransactionStartDB(name);

  /* rene */
  
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      extID = GetRandomClosureLevelNode();
      selected[i] = BBInternalIDLookup(dbref, extID);
    }
  
  /* rene */

  BBTransactionCommitDB(name);

  /* rene */

  /* unordered */

  totalsize = 0;

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      BBClosureRead1N(dbref, selected[i], FALSE, &size_of_closure);
      totalsize += size_of_closure;
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ClosureRead1N (unordered)::", totalsize, run_time);

  /* ordered */

  BBEmptyCacheDB(name, &dbref);
  totalsize = 0;
  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      BBClosureRead1N(dbref, selected[i], TRUE, &size_of_closure);
      totalsize += size_of_closure;
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ClosureRead1N (ordered)::", totalsize, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}


void
BMClosureReadMN(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype extID;
  intIDtype selected[CLOSURE_SELECT];
  int size_of_closure;
  int totalsize;
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  /* rene */

  BBTransactionStartDB(name);

  /* rene */
  
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      extID = GetRandomClosureLevelNode();
      selected[i] = BBInternalIDLookup(dbref, extID);
    }

  /* rene */

  BBTransactionCommitDB(name);

  /* rene */
  
  totalsize = 0;

  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      BBClosureReadMN(dbref, selected[i], FALSE, &size_of_closure);
      totalsize += size_of_closure;
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);


  LogResult("ClosureReadMN (unordered)::", totalsize, run_time);

  BBEmptyCacheDB(name, &dbref);
  totalsize = 0;
  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < CLOSURE_SELECT; i++)
    {
      BBClosureReadMN(dbref, selected[i], TRUE, &size_of_closure);
      totalsize += size_of_closure;
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ClosureReadMN (ordered)::", totalsize, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}


void
BMClosureReverseRead1N(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype extID;
  intIDtype selected[SINGLE_SELECT];
  int size_of_closure;
  int totalsize;
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  /* rene */

  BBTransactionStartDB(name);

  /* rene */
  
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      extID = GetRandomTextNode();
      selected[i] = BBInternalIDLookup(dbref, extID);
    }

  /* rene */

  BBTransactionCommitDB(name);

  /* rene */
  
  totalsize = 0;
  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      BBClosureReverseRead1N(dbref, selected[i], &size_of_closure);
      totalsize += size_of_closure;
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ClosureReverseRead1N::", totalsize, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}



void
BMClosureReverseReadMN(char *name)
{
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype extID;
  intIDtype selected[SINGLE_SELECT];
  int size_of_closure;
  int totalsize;
  int i;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBEmptyCacheDB(name, &dbref);

  /* rene */

  BBTransactionStartDB(name);

  /* rene */
  
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      extID = GetRandomTextNode();
      selected[i] = BBInternalIDLookup(dbref, extID);
    }

  /* rene */

  BBTransactionCommitDB(name);

  /* rene */
  
  totalsize = 0;
  ResetClock(&clk1);
  StartClock(&clk1);

  BBTransactionStartDB(name);
  for (i = 0; i < SINGLE_SELECT; i++)
    {
      BBClosureReverseReadMN(dbref, selected[i], &size_of_closure);
      totalsize += size_of_closure;
    }
  BBTransactionCommitDB(name);

  StopClock(&clk1);
  ReadClock(clk1, &run_time);

  LogResult("ClosureReverseReadMN::", totalsize, run_time);

  BBCloseDB(dbref);
  BBLogoutDB();
}



void
BMSubTreeDeleteAndInsert(char *name)
{
  long dbsize;
  bm_clock clk1;
  dbreftype dbref;
  bm_time run_time;
  extIDtype selected[CLOSURE_SELECT];
  int no_of_children[CLOSURE_SELECT];
  int size_of_closure;
  int totalsize;
  int already_chosen;
  int i, j;

  BBLoginDB();
  BBOpenDB(name, &dbref);
  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("\nOriginal database size (bytes):: %i\n", dbsize);
  BBEmptyCacheDB(name, &dbref);

  for (i = 0; i < CLOSURE_SELECT; i += 2)
    {
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

  LogResult("SubTreeDelete::", totalsize, run_time);

  BBGetSizeDB(name, &dbref, &dbsize);
  Log_printf("\nSize of database after deletion of subtrees (bytes):: %i\n",
	     dbsize);
  BBEmptyCacheDB(name, &dbref);

  totalsize = 0;
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

  BBGetSizeDB(name, &dbref, &dbsize);
  LogResult("SubTreeInsert::", totalsize, run_time);
  Log_printf("\nSize of database after reinsertion of subtrees (bytes):: ");
  Log_printf("%i\n", dbsize);
  Log_printf("\n");

  BBCloseDB(dbref);
  BBLogoutDB();
}
