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

#include <string.h>
#endif

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/hash.h"

/*
 * __ham_reclaim --
 *	Reclaim the pages from a subdatabase and return them to the
 * parent free list.  For now, we link each freed page on the list
 * separately.  If people really store hash databases in subdatabases
 * and do a lot of creates and deletes, this is going to be a problem,
 * because hash needs chunks of contiguous storage.  We may eventually
 * need to go to a model where we maintain the free list with chunks of
 * contiguous pages as well.
 *
 * PUBLIC: int __ham_reclaim __P((DB *, DB_TXN *txn));
 */
int
__ham_reclaim(dbp, txn)
	DB *dbp;
	DB_TXN *txn;
{
	DBC *dbc;
	HASH_CURSOR *hcp;
	int ret;

	/* Open up a cursor that we'll use for traversing. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, 0)) != 0)
		return (ret);
	hcp = (HASH_CURSOR *)dbc->internal;

	if ((ret = __ham_get_meta(dbc)) != 0)
		goto err;

	if ((ret = __ham_traverse(dbc,
	    DB_LOCK_WRITE, __db_reclaim_callback, dbc, 1)) != 0)
		goto err;
	if ((ret = dbc->c_close(dbc)) != 0)
		goto err;
	if ((ret = __ham_release_meta(dbc)) != 0)
		goto err;
	return (0);

err:	if (hcp->hdr != NULL)
		(void)__ham_release_meta(dbc);
	(void)dbc->c_close(dbc);
	return (ret);
}

/*
 * __ham_truncate --
 *	Reclaim the pages from a subdatabase and return them to the
 * parent free list.
 *
 * PUBLIC: int __ham_truncate __P((DB *, DB_TXN *txn, u_int32_t *));
 */
int
__ham_truncate(dbp, txn, countp)
	DB *dbp;
	DB_TXN *txn;
	u_int32_t *countp;
{
	DBC *dbc;
	HASH_CURSOR *hcp;
	db_trunc_param trunc;
	int ret;

	/* Open up a cursor that we'll use for traversing. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, 0)) != 0)
		return (ret);
	hcp = (HASH_CURSOR *)dbc->internal;

	if ((ret = __ham_get_meta(dbc)) != 0)
		goto err;

	trunc.count = 0;
	trunc.dbc = dbc;

	if ((ret = __ham_traverse(dbc,
	    DB_LOCK_WRITE, __db_truncate_callback, &trunc, 1)) != 0)
		goto err;
	if ((ret = __ham_release_meta(dbc)) != 0)
		goto err;
	if ((ret = dbc->c_close(dbc)) != 0)
		goto err;
	*countp = trunc.count;
	return (0);

err:	if (hcp->hdr != NULL)
		(void)__ham_release_meta(dbc);
	(void)dbc->c_close(dbc);
	return (ret);
}
