/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 This is the Benchmark module, the main module.  It contains the main()
 function and is thus the entry point of the benchmark program.  The
 benchmark may either be run interactively or quietly (i.e. no
 input/output from/to stdio).

 Synopsis

 seb [-p 0123][-t 1234567890][-h 567][-s seed][[-a] -l logfile][databasename]
 seb -q [-p 0123][-t 1234567890][-h 567][-s seed][-a] -l logfile databasename

 The parameters are as follows

 -q   quiet mode -- no input is expected from the keyboard and no
 output (except error messages) is written to
 the screen.  Quiet mode may only be specified
 if both the logfile and the databasename is
 also specified in the arguments.

 -p   phases     -- the arguments of the p flag tell which phases
 of the benchmark should be run (0=build-up).

 -t   tests      -- the arguments of the t flag tell which benchmark
 operations in phase 1 should be run.  Operations
 10 and 11 (SubTreeDelete/SubTreeInsert) can only be
 run in combination and by including '0' in the list.

 -h   height     -- number of levels of test database.  This
 parameter only has an effect when creating a
 new test database.  The valid heights are
 defined by the constants MIN_LEVELS and
 MAX_LEVELS in bm_global.h.

 -s   seed       -- seed for random number generator (not used directly).
 The number must be a long integer >= 0.

 -l   logfile    -- the results from the benchmark will be written
 to this file.  If the file already exists and
 append is not specified, the program exits
 with an error message.

 -a   append     -- append log info to the end of an already
 existing log file specified with -l.

 databasename    -- the name of the benchmark test database.


 In interactive mode (no quiet flag) the program will prompt for
 additional information if something is missing in the parameters.  In
 quiet mode, this will cause an immediate exit with an error message.

 Creation date: 20.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#include <stdio.h>
#include <string.h>

#include "bm_global.h"

#include "bm_buildDB.h"
#include "bm_phase1.h"
#include "bm_phase2.h"
#include "bm_phase3.h"

#include "bm_basic.h"                          /* for BBGetHeight() */
#include "bm_log.h"                            /* to write summary of
						  benchmark run parameters */
#include "bm_random.h"                         /* for InitRandom()  */
#include "bm_smallset.h"

#include "ansi.h"

GroupType lockduration = Transaction;

static char buffer[NAME_LEN];                  /* This buffer is used for
						  user input in the
						  interactive mode */



static void
include_all_phases(smallset * phases)
/*
 * This function is just a convenience to include all phases in the
 * set of phases to be run in the benchmark.
 */
{
  add_to_set(phases, 0);
  add_to_set(phases, 1);
  add_to_set(phases, 2);
  add_to_set(phases, 3);
}


static void
include_all_tests(smallset * tests)
/*
 * This function is just a convenience to include all tests in the set
 * of tests to be run in phase 1 of the benchmark.
 */
{
  add_to_set(tests, 1);
  add_to_set(tests, 2);
  add_to_set(tests, 3);
  add_to_set(tests, 4);
  add_to_set(tests, 5);
  add_to_set(tests, 6);
  add_to_set(tests, 7);
  add_to_set(tests, 8);
  add_to_set(tests, 9);
  add_to_set(tests, 10);
}


static void
parse_arguments(int argc, char *argv[], char program[NAME_LEN],
                int *quietmode,
                smallset * phases, smallset * tests, int *levels,
                long *seed, int *append, char *logfile, char* agent,
                char *databasename)
/*
 * IN  argc, argv
 * OUT program, quietmode, phases, tests, levels, append,
 * logfile, databasename
 *
 * This function parses the command line parameters of the program and
 * returns the corresponding values.
 */

{
  int i;                                       /* loop counter */

  empty_set(phases);
  empty_set(tests);


  strncpy(program, argv[0], NAME_LEN - 1);     /* save program name */
  program[NAME_LEN - 1] = '\0';
  /* (for error msg */
  /* etc.) */

  while (--argc > 0)
    {                                          /* more arguments ? */
      argv++;

      if (strcmp(*argv, "-q") == 0)
        *quietmode = TRUE;                     /* quiet mode ? */

      else if (strcmp(*argv, "-S") == 0)
        lockduration = Session;

      else if (strcmp(*argv, "-T") == 0)
        lockduration = Transaction;

      else if (strcmp(*argv, "-p") == 0 && argc > 0)
        {                                      /* phases ? */
          argc--;
          argv++;
          for (i = 0; (*argv)[i]; i++)
            if ((*argv)[i] >= '0' && (*argv)[i] <= '3')
              add_to_set(phases, (*argv)[i] - '0');
        }
      else if (strcmp(*argv, "-t") == 0 && argc > 0)
        {                                      /* tests ? */
          argc--;
          argv++;
          for (i = 0; (*argv)[i]; i++)
            {
              if ((*argv)[i] > '0' && (*argv)[i] <= '9')
                add_to_set(tests, (*argv)[i] - '0');
              if ((*argv)[i] == '0')
                add_to_set(tests, 10);
            }
        }
      else if (strcmp(*argv, "-s") == 0 && argc > 0)
        {                                      /* seed ? */
          argc--;
          argv++;
          *seed = atol(*argv);
        }
      else if (strcmp(*argv, "-l") == 0 && argc > 0)
        {                                      /* logfile ? */
          argc--;
          argv++;
          strncpy(logfile, *argv, NAME_LEN);
          logfile[NAME_LEN] = '\0';
        }
      else if (strcmp(*argv, "-a") == 0)
        *append = TRUE;                        /* append ? */
      else if (strcmp(*argv, "-h") == 0 && argc > 0)
        {                                      /* height of database */
          argc--;
          argv++;
          if (0 < argc)
	    {
	      *levels = (*argv)[0] - '0';
	      if (*levels < MIN_LEVELS || *levels > MAX_LEVELS)
		{
		  sprintf(err_msg_buf,
			  "Number of levels of test database must between %i and %i\n",
			  MIN_LEVELS, MAX_LEVELS);
		  param_error(program, err_msg_buf);
		}
	    }
	  else
	    {
	      param_error(program, NULL);
	    }
	}
      else if (strcmp(*argv, "-agent") == 0 && argc > 0)
        {                                      /* name server */
          argc--;
          argv++;
          strncpy(agent, *argv, NAME_LEN);
          agent[NAME_LEN] = '\0';
        }
      else if (argc == 1)
        {                                      /* databasename ? (last argument) */
          strncpy(databasename, *argv, NAME_LEN);
          databasename[NAME_LEN] = '\0';
        }
      else
        param_error(program, NULL);            /* parameter error : exit */
    }

  if (!is_empty(*tests))
    add_to_set(phases, 1);                     /* -t x -> -p 1 */
}



static void
check_quietmode_args(char *program, smallset * phases, smallset * tests,
                     int *levels, char *logfile, char *databasename)
/*
 * IN  program
 * IN/OUT phases
 * IN/OUT tests
 * IN/OUT levels
 * IN  logfile
 * IN  databasename
 *
 * Check whether the arguments given as parameters to the program in
 * quietmode are correct and sufficient.  Complement missing parameters.
 */
{
  char err_msg[] =                             /* error message to return if
						  logfile and databasename has
						  not been specified */
    "You must specify a logfile and the\ndatabase name when running quiet mode.";

  if (!(*logfile) || !(*databasename))         /*no logfile or database -> exit */
    param_error(program, err_msg);

  if (is_empty(*phases))
    {                                          /* -p parameter specified ? */
      include_all_phases(phases);              /* NO -> include all phases */
    }

  if (is_member(*phases, 1) && is_empty(*tests))
    include_all_tests(tests);                  /* no -t parameter, but phase 1 */
  /* -> include all tests of phase 1 */

  if (*levels == 0 && is_member(*phases, 0))   /* -h parameter specified ? */
    *levels = DEFAULT_LEVELS;                  /* NO -> use default */
}



static void
get_user_args(smallset * phases, smallset * tests, int *levels,
              char *logfile, char *databasename)
/*
 * IN/OUT  phases
 * IN/OUT  tests
 * IN/OUT  levels
 * IN/OUT  logfile
 * IN/OUT  database
 *
 * Complement arguments not given as command line parameters by asking
 * the user (interactive mode only).
 */
{
  char *p;
  int i;

  while (!(*databasename))
    {                                          /* Must specify database */
      fprintf(stderr, "Enter the name of the benchmark database: ");
      fgets(buffer, NAME_LEN, stdin);
      sscanf(buffer, "%s", databasename);
    }

  if (!(*logfile))
    {                                          /* Want a log file? */
      fprintf(stderr, "Enter a log file name (CR for no file): ");
      fgets(buffer, NAME_LEN, stdin);
      sscanf(buffer, "%s", logfile);
    }

  if (is_empty(*phases))
    {                                          /* Phases */
      fprintf(stderr, "Enter phases to run (CR for all):\n");
      for (i = 0; i < 4; i++)
        fprintf(stderr, "\t%i - %s\n", i, phasedesc[i]);
      fprintf(stderr, "Phases: ");
      fgets(buffer, NAME_LEN, stdin);
      p = buffer;
      while (*p)
        {
          if ('0' <= *p && *p <= '3')
            add_to_set(phases, *p - '0');
          p++;
        }
      if (is_empty(*phases))
        include_all_phases(phases);

      if (is_member(*phases, 1))
        {                                      /* Tests of phase 1 */
          fprintf(stderr, "Enter tests of phase 1 to run (CR for all):\n");
          for (i = 1; i < 10; i++)
            fprintf(stderr, "\t%i - %s\n", i, testdesc[i]);
          fprintf(stderr, "\t%i - %s\n", 0, testdesc[10]);
          fprintf(stderr, "Tests: ");
          fgets(buffer, NAME_LEN, stdin);
          p = buffer;
          while (*p)
            {
              if ('1' <= *p && *p <= '9')
                add_to_set(tests, *p - '0');
              if (*p == '0')
                add_to_set(tests, 10);
              p++;
            }
        }
    }
  if (is_member(*phases, 1) && is_empty(*tests))        /* phases not specified */
    /* -> include all */
    include_all_tests(tests);

  if (is_member(*phases, 0) && *levels == 0)   /* levels of test database */
    while (*levels < MIN_LEVELS || *levels > MAX_LEVELS)
      {
        fprintf(stderr, "Enter number of levels of new test database ");
        fprintf(stderr, "(%i-%i, CR=%i)", MIN_LEVELS, MAX_LEVELS, DEFAULT_LEVELS);
        fprintf(stderr, ": ");
        fgets(buffer, NAME_LEN, stdin);
        if (buffer[0] == '\n')
          *levels = DEFAULT_LEVELS;
        else
          *levels = buffer[0] - '0';
      }
}




static void
print_status(smallset phases, smallset tests, int levels, int append,
             char *logfile, char* agent, char *database, long seed)
/*
 * IN phases, tests, levels, append, logfile, database, seed
 *
 * Print the parameters used when running the benchmark.
 */
{
  int i;
  int j;

  Log_printf("Database: %s\n", database);
  Log_printf("Logfile: %s ", logfile);
  if (append)
    Log_printf("(append)");
  if (!strcmp(agent, ""))
    {
      Log_printf("Agent: %s\n", agent);
    }
  Log_printf("\nNumber of levels in database: %i", levels);
  Log_printf("\nSeed value: %i", seed);
  Log_printf("\nBenchmark phases:\n");
  for (i = 0; i < 4; i++)
    {
      if (is_member(phases, i))
        Log_printf("\t%i - %s\n", i, phasedesc[i]);
      if (i == 1 && is_member(phases, 1))
        for (j = 1; j <= 10; j++)
          if (is_member(tests, j))
            Log_printf("\t\t%2i - %s\n", j, testdesc[j]);
    }
  Log_printf("----------------------------------------------------\n");
  Log_printf("\n\n");
}


char DBNameServerAgent[NAME_LEN + 1]; /* the name of the name server */


int
main(int argc, char *argv[], char **envp)
{

  char program[NAME_LEN + 1];                  /* name that program was invoked with */
  int quietmode = FALSE;                       /* flag telling whether the */

  /* program should be running */
  /* quietmode or not */
  smallset phases;                             /* set telling which phases are */

  /* to be included in the benchmark */
  smallset tests;                              /* set telling which tests of */

  /* phase 1 are to be included in */
  /* the benchmark (10 = test 10/11) */
  long seed = 0;                               /* seed of random number generator */
  int levels = 0;                              /* the number of levels in test database */
  char logfile[NAME_LEN + 1] = "";             /* the name of the log file */
  int append = FALSE;                          /* append to end of logfile */
  char databasename[NAME_LEN + 1] = "";        /* the name of the database */


  char *m3args[] = {"benchmark"};
  M3Initialize(1, m3args, envp);
  DBNameServerAgent[0] = 0;
  parse_arguments(argc, argv, program, &quietmode, &phases, &tests, &levels,
                  &seed, &append, logfile, DBNameServerAgent, databasename);

  if (quietmode)
    check_quietmode_args(program, &phases, &tests, &levels,
                         logfile, databasename);
  else
    get_user_args(&phases, &tests, &levels, logfile, databasename);

  OpenLog(logfile, quietmode, append);

  if (levels == 0)
    BBGetHeightDB(databasename, &levels);
  InitRandom(levels, seed);

  print_status(phases, tests, levels, append, logfile, DBNameServerAgent,
	       databasename, seed);

  if (is_member(phases, 0))
    BMBuildDatabase(databasename, levels);
  if (is_member(phases, 1))
    {
      if (is_member(tests, 1))
        BMInternalIDLookup(databasename);
      if (is_member(tests, 2))
        BMAttributeRead(databasename);
      if (is_member(tests, 3))
        BMAttributeWrite(databasename);
      if (is_member(tests, 4))
        BMTextAttributeRead(databasename);
      if (is_member(tests, 5))
        BMTextAttributeWrite(databasename);
      if (is_member(tests, 6))
        BMClosureRead1N(databasename);
      if (is_member(tests, 7))
        BMClosureReadMN(databasename);
      if (is_member(tests, 8))
        BMClosureReverseRead1N(databasename);
      if (is_member(tests, 9))
        BMClosureReverseReadMN(databasename);
      if (is_member(tests, 10))
        BMSubTreeDeleteAndInsert(databasename);
    }
  if (is_member(phases, 2))
    BMRestructuring(databasename);
  if (is_member(phases, 3))
    {
      BMNoTransactions(databasename);
      BMAbortTransaction(databasename);
      BMCommitTransaction(databasename);
      BMManyTransactions(databasename);
    }

  CloseLog();
  return (BM_OK);
}
