/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996-2002
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

/*******************************************************
 * Global variables.
 *
 * Held in a single structure to minimize the name-space pollution.
 *******************************************************/
#ifdef HAVE_VXWORKS
#include "semLib.h"
#endif

typedef struct __db_globals {
	u_int32_t no_write_errors;	/* write error testing disallowed */
#ifdef HAVE_VXWORKS
	u_int32_t db_global_init;	/* VxWorks: inited */
	SEM_ID db_global_lock;		/* VxWorks: global semaphore */
#endif
					/* XA: list of opened environments. */
	TAILQ_HEAD(__db_envq, __db_env) db_envq;

	int	(*j_close) __P((int));	/* Underlying OS interface jump table.*/
	void	(*j_dirfree) __P((char **, int));
	int	(*j_dirlist) __P((const char *, char ***, int *));
	int	(*j_exists) __P((const char *, int *));
	void	(*j_free) __P((void *));
	int	(*j_fsync) __P((int));
	int	(*j_ioinfo) __P((const char *,
		    int, u_int32_t *, u_int32_t *, u_int32_t *));
	void   *(*j_malloc) __P((size_t));
	int	(*j_map) __P((char *, size_t, int, int, void **));
	int	(*j_open) __P((const char *, int, ...));
	ssize_t	(*j_read) __P((int, void *, size_t));
	void   *(*j_realloc) __P((void *, size_t));
	int	(*j_rename) __P((const char *, const char *));
	int	(*j_seek) __P((int, size_t, db_pgno_t, u_int32_t, int, int));
	int	(*j_sleep) __P((u_long, u_long));
	int	(*j_unlink) __P((const char *));
	int	(*j_unmap) __P((void *, size_t));
	ssize_t	(*j_write) __P((int, const void *, size_t));
	int	(*j_yield) __P((void));
} DB_GLOBALS;

#ifdef DB_INITIALIZE_DB_GLOBALS
DB_GLOBALS __db_global_values = {
	0,				/* write error testing disallowed */
#ifdef HAVE_VXWORKS
	0,				/* VxWorks: initialized */
	NULL,				/* VxWorks: global semaphore */
#endif
					/* XA: list of opened environments. */
	{NULL, &__db_global_values.db_envq.tqh_first},
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};
#else
extern	DB_GLOBALS	__db_global_values;
#endif

#define	DB_GLOBAL(v)	__db_global_values.v
