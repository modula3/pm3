/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Sep 14 09:05:58 PDT 1994 by kalsow   
 *      modified on Mon Mar 14 10:14:25 PST 1994 by harrison 
 *      modified on Tue May  4 23:03:19 PDT 1993 by mjordan 
 */

#ifndef BASIC_H
#define BASIC_H

#include "config.h"

/*
 * Set the directory separator string if not already defined in config.h
 */

#ifndef DIR_SEPARATOR
#define	DIR_SEPARATOR	'/'
#endif

#ifndef VOL_SEPARATOR
#define	VOL_SEPARATOR	':'
#endif

typedef int Boolean;
typedef int Integer;

#ifndef FALSE
#define FALSE	(0)
#endif

#ifndef TRUE
#define TRUE    (1)
#endif

#define BIT(N)	(1 << (N))

#define MAX_NAMELENGTH	128
#define	MAX_ARGS	10	/* How many args fit on a command line */

#define NUMBER(ARRAY)	(sizeof(ARRAY) / sizeof((ARRAY)[0]))

#define	NEW(TYPE)	((TYPE *) Utils_GetMemory(sizeof(TYPE)))
#define	RENEW(WHAT,TYPE)	((TYPE *) Utils_AdjustMemory(WHAT, sizeof(TYPE)))
#define	NEW_ARRAY(TYPE,N)	((TYPE *) Utils_GetMemory((N) * sizeof(TYPE)))
#define	RENEW_ARRAY(WHAT,TYPE,N)	((TYPE *) Utils_AdjustMemory(WHAT, (N) * sizeof(TYPE)))

#ifdef DEBUG_RANGE_CHECKING
#define RANGE_CHECK(I,L,H) \
        do { \
	    if ((L) > (I) || (I) >= (H)) { \
		fprintf(stderr, "Range check: %s, line %d: %d not in [%d..%d)\n", __FILE__, __LINE__, I, L, H); \
		exit(1); \
	    } \
	} while (0)
#else
#define RANGE_CHECK(I,L,H)
#endif

typedef struct {
    char *template_name;
    Boolean dont_execute;
    Boolean trace_files;
} Parameters;

#define	RETURN_NOTHING	BIT(0)	/* No return found */
#define	RETURN_SIMPLE	BIT(1)	/* Simple return */
#define	RETURN_VALUE	BIT(2)	/* Value return */

#ifndef STDOUT_NAME
#define	STDOUT_NAME	"_stdout"
#endif

#ifndef STDERR_NAME
#define	STDERR_NAME	"_stderr"
#endif

typedef enum {
    Exit_Void,			/* Not an exit code */
    Exit_OK,			/* Normal exit */
    Exit_Return 		/* Exit via return statement */
} ExitCode;

typedef struct Atom *Atom_ForwardDecl;
typedef struct Code *Code_ForwardDecl;

#endif /* BASIC_H */
