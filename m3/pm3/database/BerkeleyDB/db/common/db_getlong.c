/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996-2002
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id$";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "db_int.h"

/*
 * __db_getlong --
 *	Return a long value inside of basic parameters.
 *
 * PUBLIC: int __db_getlong
 * PUBLIC:     __P((DB *, const char *, char *, long, long, long *));
 */
int
__db_getlong(dbp, progname, p, min, max, storep)
	DB *dbp;
	const char *progname;
	char *p;
	long min, max, *storep;
{
	long val;
	char *end;

	__os_set_errno(0);
	val = strtol(p, &end, 10);
	if ((val == LONG_MIN || val == LONG_MAX) &&
	    __os_get_errno() == ERANGE) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: %s\n", progname, p, strerror(ERANGE));
		else
			dbp->err(dbp, ERANGE, "%s", p);
		return (1);
	}
	if (p[0] == '\0' || (end[0] != '\0' && end[0] != '\n')) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: Invalid numeric argument\n", progname, p);
		else
			dbp->errx(dbp, "%s: Invalid numeric argument", p);
		return (1);
	}
	if (val < min) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: Less than minimum value (%ld)\n",
			    progname, p, min);
		else
			dbp->errx(dbp,
			    "%s: Less than minimum value (%ld)", p, min);
		return (1);
	}
	if (val > max) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: Greater than maximum value (%ld)\n",
			    progname, p, max);
		else
			dbp->errx(dbp,
			    "%s: Greater than maximum value (%ld)", p, max);
		return (1);
	}
	*storep = val;
	return (0);
}

/*
 * __db_getulong --
 *	Return an unsigned long value inside of basic parameters.
 *
 * PUBLIC: int __db_getulong
 * PUBLIC:     __P((DB *, const char *, char *, u_long, u_long, u_long *));
 */
int
__db_getulong(dbp, progname, p, min, max, storep)
	DB *dbp;
	const char *progname;
	char *p;
	u_long min, max, *storep;
{
#if !defined(HAVE_STRTOUL)
	COMPQUIET(min, 0);

	return (__db_getlong(dbp, progname, p, 0, max, (long *)storep));
#else
	u_long val;
	char *end;

	__os_set_errno(0);
	val = strtoul(p, &end, 10);
	if (val == ULONG_MAX && __os_get_errno() == ERANGE) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: %s\n", progname, p, strerror(ERANGE));
		else
			dbp->err(dbp, ERANGE, "%s", p);
		return (1);
	}
	if (p[0] == '\0' || (end[0] != '\0' && end[0] != '\n')) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: Invalid numeric argument\n", progname, p);
		else
			dbp->errx(dbp, "%s: Invalid numeric argument", p);
		return (1);
	}
	if (val < min) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: Less than minimum value (%lu)\n",
			    progname, p, min);
		else
			dbp->errx(dbp,
			    "%s: Less than minimum value (%lu)", p, min);
		return (1);
	}

	/*
	 * We allow a 0 to substitute as a max value for ULONG_MAX because
	 * 1) accepting only a 0 value is unlikely to be necessary, and 2)
	 * we don't want callers to have to use ULONG_MAX explicitly, as it
	 * may not exist on all platforms.
	 */
	if (max != 0 && val > max) {
		if (dbp == NULL)
			fprintf(stderr,
			    "%s: %s: Greater than maximum value (%lu)\n",
			    progname, p, max);
		else
			dbp->errx(dbp,
			    "%s: Greater than maximum value (%lu)", p, max);
		return (1);
	}
	*storep = val;
	return (0);
#endif	/* !defined(HAVE_STRTOUL) */
}
