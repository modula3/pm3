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
#endif

#include "db_int.h"
#include "dbinc/db_shash.h"
#include "dbinc/mp.h"

/*
 * memp_register --
 *	Register a file type's pgin, pgout routines.
 *
 * PUBLIC: int __memp_register __P((DB_ENV *, int,
 * PUBLIC:     int (*)(DB_ENV *, db_pgno_t, void *, DBT *),
 * PUBLIC:     int (*)(DB_ENV *, db_pgno_t, void *, DBT *)));
 */
int
__memp_register(dbenv, ftype, pgin, pgout)
	DB_ENV *dbenv;
	int ftype;
	int (*pgin) __P((DB_ENV *, db_pgno_t, void *, DBT *));
	int (*pgout) __P((DB_ENV *, db_pgno_t, void *, DBT *));
{
	DB_MPOOL *dbmp;
	DB_MPREG *mpreg;
	int ret;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv,
	    dbenv->mp_handle, "DB_ENV->memp_register", DB_INIT_MPOOL);

	dbmp = dbenv->mp_handle;

	/*
	 * Chances are good that the item has already been registered, as the
	 * DB access methods are the folks that call this routine.  If already
	 * registered, just update the entry, although it's probably unchanged.
	 */
	MUTEX_THREAD_LOCK(dbenv, dbmp->mutexp);
	for (mpreg = LIST_FIRST(&dbmp->dbregq);
	    mpreg != NULL; mpreg = LIST_NEXT(mpreg, q))
		if (mpreg->ftype == ftype) {
			mpreg->pgin = pgin;
			mpreg->pgout = pgout;
			break;
		}
	MUTEX_THREAD_UNLOCK(dbenv, dbmp->mutexp);
	if (mpreg != NULL)
		return (0);

	/* New entry. */
	if ((ret = __os_malloc(dbenv, sizeof(DB_MPREG), &mpreg)) != 0)
		return (ret);

	mpreg->ftype = ftype;
	mpreg->pgin = pgin;
	mpreg->pgout = pgout;

	MUTEX_THREAD_LOCK(dbenv, dbmp->mutexp);
	LIST_INSERT_HEAD(&dbmp->dbregq, mpreg, q);
	MUTEX_THREAD_UNLOCK(dbenv, dbmp->mutexp);

	return (0);
}
