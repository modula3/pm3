/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001-2002
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
#include "dbinc/db_shash.h"
#include "dbinc/db_am.h"
#include "dbinc/fop.h"
#include "dbinc/lock.h"
#include "dbinc/log.h"

static int __db_subdb_rename __P(( DB *, DB_TXN *,
		const char *, const char *, const char *));

/*
 * __dbenv_dbrename
 *	Rename method for DB_ENV.
 *
 * PUBLIC: int __dbenv_dbrename __P((DB_ENV *, DB_TXN *,
 * PUBLIC:     const char *, const char *, const char *, u_int32_t));
 */
int
__dbenv_dbrename(dbenv, txn, name, subdb, newname, flags)
	DB_ENV *dbenv;
	DB_TXN *txn;
	const char *name, *subdb, *newname;
	u_int32_t flags;
{
	DB *dbp;
	int ret, t_ret, txn_local;

	txn_local = 0;

	PANIC_CHECK(dbenv);
	ENV_ILLEGAL_BEFORE_OPEN(dbenv, "DB_ENV->dbrename");

	/* Validate arguments. */
	if ((ret = __db_fchk(dbenv, "DB->rename", flags, DB_AUTO_COMMIT)) != 0)
		return (ret);

	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (ret);

	/*
	 * Create local transaction as necessary, check for consistent
	 * transaction usage.
	 */
	if (IS_AUTO_COMMIT(dbenv, txn, flags)) {
		if ((ret = __db_txn_auto(dbp, &txn)) != 0)
			return (ret);
		txn_local = 1;
	} else
		if (txn != NULL && !TXN_ON(dbenv))
			return (__db_not_txn_env(dbenv));

	ret = __db_rename_i(dbp, txn, name, subdb, newname);

	/* Commit for DB_AUTO_COMMIT. */
	if (txn_local) {
		if (ret == 0)
			ret = txn->commit(txn, 0);
		else
			if ((t_ret = txn->abort(txn)) != 0)
				ret = __db_panic(dbenv, t_ret);

		/*
		 * We created the DBP here and when we committed/aborted,
		 * we release all the tranasctional locks, which includes
		 * the handle lock; mark the handle cleared explicitly.
		 */
		LOCK_INIT(dbp->handle_lock);
		dbp->lid = DB_LOCK_INVALIDID;
	}

	/*
	 * We never opened this dbp for real, so don't call the transactional
	 * version of DB->close, and use NOSYNC to avoid calling into mpool.
	 */
	if ((t_ret = dbp->close(dbp, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_rename
 *	Rename method for DB.
 *
 * PUBLIC: int __db_rename __P((DB *,
 * PUBLIC:     const char *, const char *, const char *, u_int32_t));
 */
int
__db_rename(dbp, name, subdb, newname, flags)
	DB *dbp;
	const char *name, *subdb, *newname;
	u_int32_t flags;
{
	DB_ENV *dbenv;
	int ret, t_ret;

	dbenv = dbp->dbenv;

	PANIC_CHECK(dbenv);

	/*
	 * Validate arguments, continuing to destroy the handle on failure.
	 *
	 * Cannot use DB_ILLEGAL_AFTER_OPEN directly because it returns.
	 *
	 * !!!
	 * We have a serious problem if we're here with a handle used to open
	 * a database -- we'll destroy the handle, and the application won't
	 * ever be able to close the database.
	 */
	if (F_ISSET(dbp, DB_AM_OPEN_CALLED)) {
		ret = __db_mi_open(dbenv, "DB->rename", 1);
		goto err;
	}

	/* Validate arguments. */
	if ((ret = __db_fchk(dbenv, "DB->rename", flags, 0)) != 0)
		goto err;

	/* Check for consistent transaction usage. */
	if ((ret = __db_check_txn(dbp, NULL, DB_LOCK_INVALIDID, 0)) != 0)
		goto err;

	/* Rename the file. */
	ret = __db_rename_i(dbp, NULL, name, subdb, newname);

	/*
	 * We never opened this dbp for real, use NOSYNC to avoid calling into
	 * mpool.
	 */
err:	if ((t_ret = dbp->close(dbp, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_rename_i
 *	Internal rename method for DB.
 *
 * PUBLIC: int __db_rename_i __P((DB *,
 * PUBLIC:      DB_TXN *, const char *, const char *, const char *));
 */
int
__db_rename_i(dbp, txn, name, subdb, newname)
	DB *dbp;
	DB_TXN *txn;
	const char *name, *subdb, *newname;
{
	DB_ENV *dbenv;
	int ret;
	char *real_name;

	dbenv = dbp->dbenv;
	real_name = NULL;

	DB_TEST_RECOVERY(dbp, DB_TEST_PREDESTROY, ret, name);

	if (subdb != NULL) {
		ret = __db_subdb_rename(dbp, txn, name, subdb, newname);
		goto err;
	}

	/* From here on down, this pertains to files. */

	/* Find the real name of the file. */
	if ((ret = __db_appname(dbenv,
	    DB_APP_DATA, name, 0, NULL, &real_name)) != 0)
		goto err;

	if ((ret = __fop_remove_setup(dbp, txn, real_name, 0)) != 0)
		goto err;

	if (dbp->db_am_rename != NULL &&
	    (ret = dbp->db_am_rename(dbp, txn, name, subdb, newname)) != 0)
		goto err;

	/*
	 * The transactional case and non-transactional case are
	 * quite different.  In the non-transactional case, we simply
	 * do the rename.  In the transactional case, since we need
	 * the ability to back out and maintain locking, we have to
	 * create a temporary object as a placeholder.  This is all
	 * taken care of in the fop layer.
	 */
	if (txn != NULL) {
		if ((ret = __fop_dummy(dbp, txn, name, newname, 0)) != 0)
			goto err;
	} else {
		if ((ret = __fop_dbrename(dbp, name, newname)) != 0)
			goto err;
	}

	/*
	 * I am pretty sure that we haven't gotten a dbreg id, so calling
	 * dbreg_filelist_update is not necessary.
	 */
	DB_ASSERT(dbp->log_filename == NULL ||
	    dbp->log_filename->id == DB_LOGFILEID_INVALID);

	DB_TEST_RECOVERY(dbp, DB_TEST_POSTDESTROY, ret, newname);

DB_TEST_RECOVERY_LABEL
err:
	if (real_name != NULL)
		__os_free(dbenv, real_name);

	return (ret);
}

/*
 * __db_subdb_rename --
 *	Rename a subdatabase.
 */
static int
__db_subdb_rename(dbp, txn, name, subdb, newname)
	DB *dbp;
	DB_TXN *txn;
	const char *name, *subdb, *newname;
{
	DB *mdbp;
	DB_ENV *dbenv;
	PAGE *meta;
	int ret, t_ret;

	mdbp = NULL;
	meta = NULL;
	dbenv = dbp->dbenv;

	/*
	 * We have not opened this dbp so it isn't marked as a subdb,
	 * but it ought to be.
	 */
	F_SET(dbp, DB_AM_SUBDB);

	/*
	 * Rename the entry in the main database.  We need to first
	 * get the meta-data page number (via MU_OPEN) so that we can
	 * read the meta-data page and obtain a handle lock.  Once we've
	 * done that, we can proceed to do the rename in the master.
	 */
	if ((ret = __db_master_open(dbp, txn, name, 0, 0, &mdbp)) != 0)
		goto err;

	if ((ret = __db_master_update(mdbp, dbp, txn, subdb, dbp->type,
	    MU_OPEN, NULL, 0)) != 0)
		goto err;

	if ((ret = mdbp->mpf->get(mdbp->mpf, &dbp->meta_pgno, 0, &meta)) != 0)
		goto err;
	memcpy(&dbp->fileid, ((DBMETA *)meta)->uid, DB_FILE_ID_LEN);
	if ((ret = __fop_lock_handle(dbenv,
	    dbp, mdbp->lid, DB_LOCK_WRITE, NULL, 0)) != 0)
		goto err;

	ret = mdbp->mpf->put(mdbp->mpf, meta, 0);
	meta = NULL;
	if (ret != 0)
		goto err;

	if ((ret = __db_master_update(mdbp, dbp, txn,
	    subdb, dbp->type, MU_RENAME, newname, 0)) != 0)
		goto err;

	DB_TEST_RECOVERY(dbp, DB_TEST_POSTDESTROY, ret, name);

DB_TEST_RECOVERY_LABEL
err:
	if (meta != NULL &&
	    (t_ret = mdbp->mpf->put(mdbp->mpf, meta, 0)) != 0 && ret == 0)
		ret = t_ret;

	if (mdbp != NULL &&
	    (t_ret = __db_close_i(mdbp, txn, 0)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}
