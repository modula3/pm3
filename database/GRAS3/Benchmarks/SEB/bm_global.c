/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 This is a global module.

 Creation date: 25.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#include <stdio.h>
#include <errno.h>
#include "bm_global.h"
#include "bm_log.h"
#include "ansi.h"

char err_msg_buf[256];

char phasedesc[4][20] =
{"Build-up",
 "Database operations",
 "Restructuring",
 "Transactions"};

char testdesc[11][30] =
{"",
 "InternalIDLookup",
 "AttributeRead",
 "AttributeWrite",
 "TextAttributeRead",
 "TextAttributeWrite",
 "ClosureRead1N",
 "ClosureReadMN",
 "ClosureReverseRead1N",
 "ClosureReverseReadMN",
 "SubTreeDelete/SubTreeInsert"};


static void
printsynopsis(const char *program)
{
  fprintf(stderr, "SYNOPSIS:\n");
  fprintf(stderr,
	  "%s [-p 0123][ -t 1234567890][ -h 567][[ -a] -l logfile][ databasename]\n", program);
  fprintf(stderr,
	  "%s -q [ -p 0123][ -t 1234567890][ -h 567][ -a] -l logfile databasename\n", program);
};


void
bm_error(const char *where, const char *reason, const int sys)
{
  Log_printf("Software Engineering Database Benchmark\n");
  Log_printf("I N T E R N A L   E R R O R\n");
  if (where)
    Log_printf("In %s:\n", where);
  if (reason)
    Log_printf("%s\n", reason);
  if (sys && errno)
    {
      Log_printf("System error message: errno=%i\n", errno);
      perror("\t");
    }
  Log_printf("Exiting...\n");
  CloseLog();
  abort();
}


void
param_error(const char *program, const char *reason)
{
  fprintf(stderr, "Software Engineering Database Benchmark\n");
  fprintf(stderr, "P A R A M E T E R   E R R O R\n");
  if (reason)
    fprintf(stderr, "%s\n", reason);
  printsynopsis(program);
  CloseLog();
  exit(PARAM_ERROR);
}
