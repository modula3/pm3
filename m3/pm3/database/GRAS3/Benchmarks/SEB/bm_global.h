


/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 All constants, types, and variables global to the benchmark are
 defined in this module.

 Creation date: 15.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_global_h
#define _bm_global_h

#include "rgglobal.h"
#include "config.h"

#define MIN_CHILDREN 0	        /* Minimum number of children */
				/* (parent/children relation) */
				/* MUST BE AN INTEGER */

#define MAX_CHILDREN 8		/* Maximum number of children */
				/* (parent/children relation) */
				/* MUST BE AN INTEGER */

#define AVG_CHILDREN (MIN_CHILDREN+(MAX_CHILDREN-MIN_CHILDREN)/2)
				/* Average number of children */
				/* * * M U S T * * * BE AN *EVEN* INTEGER */

#define MIN_REFS 0		/* Minimum number of references */
				/* (refTo/refFrom relation) */
				/* MUST BE AN INTEGER */

#define MAX_REFS 8		/* Maximum number of references */
				/* (refTo/refFrom relation) */
				/* MUST BE AN INTEGER */

#define AVG_REFS (MIN_REFS+(MAX_REFS-MIN_REFS)/2)
				/* Average number of references */

#define MAX_REVREFS 512		/* Maximum number of reverse references */
				/* handled by the database */

#define TEXT_MIN_LEN 0		/* Minimum length of text attribute of */
				/* 'TextNode' nodes */
#define TEXT_MAX_LEN 512	/* Maximum length of text attribute of */
				/* 'TextNode' nodes */

#define CHAR_MIN_VAL 33		/* Minimum ASCII value of character in */
				/* string for text attribute */

#define CHAR_MAX_VAL 126	/* Maximum ASCII value of character in */
				/* string for text attribute */

#define MILLION_MIN_VAL 1	/* Minimum value of million attribute */
				/* of nodetype 'Node' and relationtypes */
				/* parent/children and refTo/refFrom */

#define MILLION_MAX_VAL 1000000	/* Maximum value of million attribute */


#define SINGLE_SELECT 128	/* Number of random values to generate */
				/* for benchmark operations accessing */
				/* single nodes. */

#define CLOSURE_SELECT 16	/* Number of random values to generate */
				/* for benchmark operations accessing */
				/* closures. */

#define CLOSURE_LEVEL 3		/* Level of database on which to */
				/* select input for benchmark closure */
				/* operations from. */

#define MIN_LEVELS 3		/* Minimum number of levels in */
				/* database. */

#define DEFAULT_LEVELS 5	/* Default number of levels in */
				/* database. */

#define MAX_LEVELS 7		/* Maximum number of levels in */
				/* database. */


#define NAME_LEN 256		/* Length of name for database and log file */

#define MAX_PATH 1024		/* Max length of path */


#undef TRUE
#undef FALSE

#define TRUE   1
#define FALSE  0

#define BM_OK 0
#define BM_ERROR 1
#define PARAM_ERROR 2


typedef GraphNumber dbreftype;	/* The type of the database reference */

				/* (see database.h: DBOpen()) */
				/* dbreftype differs with the database */
				/* for which the benchmark is being */
				/* implemented */

typedef long int extIDtype;	/* The type of the externalID attribute */

				/* of the node type 'Node' */

typedef NodeNumber intIDtype;	/* The type of the internal identifiers */

				/* of the nodes in the database (see */
				/* database.h: DBCreateRootNode) */
				/* intIDtype differs with the database */
				/* for which the benchmark is being */
				/* implemented */


extern char err_msg_buf[256];	/* used as an intermediate buffer with */

				/* sprintf to compose error message to */
				/* bm_error */

extern char phasedesc[4][20];	/* a textual description of */

				/* the phases of the benchmark */
				/* (0-3) */

extern char testdesc[11][30];	/* a textual description of */

				/* the tests of phase 1 */
				/* (1-10) */

extern void
NORETURN(bm_error(const char *where, const char *reason, const int sys));

extern void param_error(const char *program, const char *reason);

#endif
