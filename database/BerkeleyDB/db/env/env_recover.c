/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996-2002
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char copyright[] =
    "Copyright (c) 1996-2002\nSleepycat Software Inc.  All rights reserved.\n";
static const char revid[] =
    "$Id$";
#endif

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#include <string.h>
#endif

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_shash.h"
#include "dbinc/lock.h"
#include "dbinc/log.h"
#include "dbinc/rep.h"
#include "dbinc/txn.h"
#include "dbinc/db_am.h"

static int    __log_backup __P((DB_ENV *, DB_LOGC *, DB_LSN *, DB_LSN *));
static int    __log_earliest __P((DB_ENV *, DB_LOGC *, int32_t *, DB_LSN *));
static double __lsn_diff __P((DB_LSN *, DB_LSN *, DB_LSN *, u_int32_t, int));

/*
 * __db_apprec --
 *	Perform recovery.  If max_lsn is non-NULL, then we are trying
 * to synchronize this system up with another system that has a max
 * LSN of max_lsn, so we need to roll back sufficiently far for that
 * to work.  See __log_backup for details.
 *
 * PUBLIC: int __db_apprec __P((DB_ENV *, DB_LSN *, u_int32_t));
 */
int
__db_apprec(dbenv, max_lsn, flags)
	DB_ENV *dbenv;
	DB_LSN *max_lsn;
	u_int32_t flags;
{
	DBT data;
	DB_LOGC *logc;
	DB_LSN ckp_lsn, first_lsn, last_lsn, lowlsn, lsn, stop_lsn;
	DB_TXNREGION *region;
	__txn_ckp_args *ckp_args;
	time_t now, tlow;
	int32_t log_size, low;
	double nfiles;
	int have_rec, is_thread, progress, ret, t_ret;
	int (**dtab) __P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t dtabsize;
	u_int32_t hi_txn, lockid, txnid;
	char *p, *pass, t1[60], t2[60];
	void *txninfo;

	COMPQUIET(nfiles, (double)0);

	logc = NULL;
	ckp_args = NULL;
	dtab = NULL;
	hi_txn = TXN_MAXIMUM;
	lockid = DB_LOCK_INVALIDID;
	txninfo = NULL;
	pass = "initial";

	/*
	 * XXX
	 * Get the log size.  No locking required because we're single-threaded
	 * during recovery.
	 */
	log_size =
	   ((LOG *)(((DB_LOG *)dbenv->lg_handle)->reginfo.primary))->log_size;

	/*
	 * Save the state of the thread flag -- we don't need it on at the
	 * moment because we're single-threaded until recovery is complete.
	 */
	is_thread = F_ISSET(dbenv, DB_ENV_THREAD) ? 1 : 0;
	F_CLR(dbenv, DB_ENV_THREAD);

	/* Set in-recovery flags. */
	F_SET((DB_LOG *)dbenv->lg_handle, DBLOG_RECOVER);
	region = ((DB_TXNMGR *)dbenv->tx_handle)->reginfo.primary;
	F_SET(region, TXN_IN_RECOVERY);

	/* Allocate a cursor for the log. */
	if ((ret = dbenv->log_cursor(dbenv, &logc, 0)) != 0)
		goto err;

	/*
	 * If the user is specifying recovery to a particular point in time
	 * or to a particular LSN, find the point to start recovery from.
	 */
	ZERO_LSN(lowlsn);
	if (max_lsn != NULL) {
		if ((ret = __log_backup(dbenv, logc, max_lsn, &lowlsn)) != 0)
			goto err;
	} else if (dbenv->tx_timestamp != 0) {
		if ((ret = __log_earliest(dbenv, logc, &low, &lowlsn)) != 0)
			goto err;
		if ((int32_t)dbenv->tx_timestamp < low) {
			(void)snprintf(t1, sizeof(t1),
			    "%s", ctime(&dbenv->tx_timestamp));
			if ((p = strchr(t1, '\n')) != NULL)
				*p = '\0';
			tlow = (time_t)low;
			(void)snprintf(t2, sizeof(t2), "%s", ctime(&tlow));
			if ((p = strchr(t2, '\n')) != NULL)
				*p = '\0';
			__db_err(dbenv,
		    "Invalid recovery timestamp %s; earliest time is %s",
			    t1, t2);
			ret = EINVAL;
			goto err;
		}
	}

	/*
	 * Recovery is done in three passes:
	 * Pass #0:
	 *	We need to find the position from which we will open files.
	 *	We need to open files beginning with the earlier of the
	 *	most recent checkpoint LSN and a checkpoint LSN before the
	 *	recovery timestamp, if specified.  We need to be before the
	 *	most recent checkpoint LSN because we are going to collect
	 *	information about which transactions were begun before we
	 *	start rolling forward.  Those that were should never be undone
	 *	because queue cannot use LSNs to determine what operations can
	 *	safely be aborted and it cannot rollback operations in
	 *	transactions for which there may be records not processed
	 *	during recovery.  We need to consider earlier points in time
	 *	in case we are recovering to a particular timestamp.
	 *
	 * Pass #1:
	 *	Read forward through the log from the position found in pass 0
	 *	opening and closing files, and recording transactions for which
	 *	we've seen their first record (the transaction's prev_lsn is
	 *	0,0).  At the end of this pass, we know all transactions for
	 *	which we've seen begins and we have the "current" set of files
	 *	open.
	 *
	 * Pass #2:
	 *	Read backward through the log undoing any uncompleted TXNs.
	 *	There are four cases:
	 *	    1.  If doing catastrophic recovery, we read to the
	 *		beginning of the log
	 *	    2.  If we are doing normal reovery, then we have to roll
	 *		back to the most recent checkpoint LSN.
	 *	    3.  If we are recovering to a point in time, then we have
	 *		to roll back to the checkpoint whose ckp_lsn is earlier
	 *		than the specified time.  __log_earliest will figure
	 *		this out for us.
	 *	    4.	If we are recovering back to a particular LSN, then
	 *		we have to roll back to the checkpoint whose ckp_lsn
	 *		is earlier than the max_lsn.  __log_backup will figure
	 *		that out for us.
	 *	In case 2, "uncompleted TXNs" include all those who commited
	 *	after the user's specified timestamp.
	 *
	 * Pass #3:
	 *	Read forward through the log from the LSN found in pass #2,
	 *	redoing any committed TXNs (which commited after any user-
	 *	specified rollback point).  During this pass, checkpoint
	 *	file information is ignored, and file openings and closings
	 *	are redone.
	 *
	 * ckp_lsn   -- lsn of the last checkpoint or the first in the log.
	 * first_lsn -- the lsn where the forward passes begin.
	 * last_lsn  -- the last lsn in the log, used for feedback
	 * lowlsn    -- the lsn we are rolling back to, if we are recovering
	 *		to a point in time.
	 * lsn       -- temporary use lsn.
	 * stop_lsn  -- the point at which forward roll should stop
	 */

	/*
	 * Find out the last lsn, so that we can estimate how far along we
	 * are in recovery.  This will help us determine how much log there
	 * is between the first LSN that we're going to be working with and
	 * the last one.  We assume that each of the three phases takes the
	 * same amount of time (a false assumption) and then use the %-age
	 * of the amount of log traversed to figure out how much of the
	 * pass we've accomplished.
	 *
	 * If we can't find any log records, we're kind of done.
	 */
#ifdef UMRW
	ZERO_LSN(last_lsn);
#endif
	memset(&data, 0, sizeof(data));
	if ((ret = logc->get(logc, &last_lsn, &data, DB_LAST)) != 0) {
		if (ret == DB_NOTFOUND)
			ret = 0;
		else
			__db_err(dbenv, "Last log record not found");
		goto err;
	}

	do {
		/* txnid is after rectype, which is a u_int32. */
		memcpy(&txnid,
		    (u_int8_t *)data.data + sizeof(u_int32_t), sizeof(txnid));

		if (txnid != 0)
			break;
	} while ((ret = logc->get(logc, &lsn, &data, DB_PREV)) == 0);

	/*
	 * There are no transactions, so there is nothing to do unless
	 * we're recovering to an LSN.  If we are, we need to proceed since
	 * we'll still need to do a vtruncate based on information we haven't
	 * yet collected.
	 */
	if (ret == DB_NOTFOUND) {
		ret = 0;
		if (max_lsn == NULL)
			goto done;
	}
	if (ret != 0)
		goto err;

	hi_txn = txnid;

	/*
	 * Pass #0
	 * Find the LSN from which we begin OPENFILES.
	 *
	 * If this is a catastrophic recovery, or if no checkpoint exists
	 * in the log, the LSN is the first LSN in the log.
	 *
	 * Otherwise, it is the minimum of (1) the LSN in the last checkpoint
	 * and (2) the LSN in the checkpoint before any specified recovery
	 * timestamp or max_lsn.
	 */
	/*
	 * Get the first LSN in the log; it's an initial default
	 * even if this is not a catastrophic recovery.
	 */
	if ((ret = logc->get(logc, &ckp_lsn, &data, DB_FIRST)) != 0) {
		if (ret == DB_NOTFOUND)
			ret = 0;
		else
			__db_err(dbenv, "First log record not found");
		goto err;
	}
	first_lsn = ckp_lsn;
	have_rec = 1;

	if (!LF_ISSET(DB_RECOVER_FATAL)) {
		if ((ret = __txn_getckp(dbenv, &ckp_lsn)) == 0 &&
		    (ret = logc->get(logc, &ckp_lsn, &data, DB_SET)) == 0) {
			/* We have a recent checkpoint.  This is LSN (1). */
			if ((ret = __txn_ckp_read(dbenv,
			    data.data, &ckp_args)) != 0) {
				__db_err(dbenv,
			    "Invalid checkpoint record at [%ld][%ld]",
				    (u_long)ckp_lsn.file,
				    (u_long)ckp_lsn.offset);
				goto err;
			}
			first_lsn = ckp_args->ckp_lsn;
			have_rec = 0;
		}

		/*
		 * If LSN (2) exists, use it if it's before LSN (1).
		 * (If LSN (1) doesn't exist, first_lsn is the
		 * beginning of the log, so will "win" this check.)
		 *
		 * XXX
		 * In the recovery-to-a-timestamp case, lowlsn is chosen by
		 * __log_earliest, and is the checkpoint LSN of the
		 * *earliest* checkpoint in the unreclaimed log.  I
		 * (krinsky) believe that we could optimize this by looking
		 * instead for the LSN of the *latest* checkpoint before
		 * the timestamp of interest, but I'm not sure that this
		 * is worth doing right now.  (We have to look for lowlsn
		 * and low anyway, to make sure the requested timestamp is
		 * somewhere in the logs we have, and all that's required
		 * is that we pick *some* checkpoint after the beginning of
		 * the logs and before the timestamp.
		 */
		if ((dbenv->tx_timestamp != 0 || max_lsn != NULL) &&
		    log_compare(&lowlsn, &first_lsn) < 0) {
			DB_ASSERT(have_rec == 0);
			first_lsn = lowlsn;
		}
	}

	/* Get the record at first_lsn if we don't have it already. */
	if (!have_rec &&
	    (ret = logc->get(logc, &first_lsn, &data, DB_SET)) != 0) {
		__db_err(dbenv, "Checkpoint LSN record [%ld][%ld] not found",
		    (u_long)first_lsn.file, (u_long)first_lsn.offset);
		goto err;
	}

	if (dbenv->db_feedback != NULL) {
		if (last_lsn.file == first_lsn.file)
			nfiles = (double)
			    (last_lsn.offset - first_lsn.offset) / log_size;
		else
			nfiles = (double)(last_lsn.file - first_lsn.file) +
			    (double)(log_size - first_lsn.offset +
			    last_lsn.offset) / log_size;
		/* We are going to divide by nfiles; make sure it isn't 0. */
		if (nfiles == 0)
			nfiles = (double)0.001;
	}

	/* Find a low txnid. */
	ret = 0;
	do {
		/* txnid is after rectype, which is a u_int32. */
		memcpy(&txnid,
		    (u_int8_t *)data.data + sizeof(u_int32_t), sizeof(txnid));

		if (txnid != 0)
			break;
	} while ((ret = logc->get(logc, &lsn, &data, DB_NEXT)) == 0);

	/*
	 * There are no transactions and we're not recovering to an LSN (see
	 * above), so there is nothing to do.
	 */
	if (ret == DB_NOTFOUND) {
		ret = 0;
		if (max_lsn == NULL)
			goto done;
	}

	/* Reset to the first lsn. */
	if (ret != 0 || (ret = logc->get(logc, &first_lsn, &data, DB_SET)) != 0)
		goto err;

	/* Initialize the transaction list. */
	if ((ret =
	    __db_txnlist_init(dbenv, txnid, hi_txn, max_lsn, &txninfo)) != 0)
		goto err;

	/*
	 * Pass #1
	 * Run forward through the log starting at the first relevant lsn.
	 */
	if ((ret = __env_openfiles(dbenv, logc,
	    txninfo, &data, &first_lsn, &last_lsn, nfiles, 1)) != 0)
		goto err;

	/*
	 * Pass #2.
	 *
	 * We used first_lsn to tell us how far back we need to recover,
	 * use it here.
	 */

	if (FLD_ISSET(dbenv->verbose, DB_VERB_RECOVERY))
		__db_err(dbenv, "Recovery starting from [%lu][%lu]",
		    (u_long)first_lsn.file, (u_long)first_lsn.offset);

	/*
	 * If we are doing client recovery, then we need to allocate
	 * the page-info lock table.
	 */
	if (max_lsn != NULL) {
		if ((ret = __rep_lockpgno_init(dbenv, &dtab, &dtabsize)) != 0)
			goto err;
		if ((ret = dbenv->lock_id(dbenv, &lockid)) != 0)
			goto err;
	}

	pass = "backward";
	for (ret = logc->get(logc, &lsn, &data, DB_LAST);
	    ret == 0 && log_compare(&lsn, &first_lsn) >= 0;
	    ret = logc->get(logc, &lsn, &data, DB_PREV)) {
		if (dbenv->db_feedback != NULL) {
			progress = 34 + (int)(33 * (__lsn_diff(&first_lsn,
			    &last_lsn, &lsn, log_size, 0) / nfiles));
			dbenv->db_feedback(dbenv, DB_RECOVER, progress);
		}
		if (max_lsn != NULL && (ret = __rep_lockpages(dbenv,
		    dtab, dtabsize, &lsn, NULL, NULL, lockid)) != 0)
			continue;

		ret = __db_dispatch(dbenv, dbenv->recover_dtab,
		    dbenv->recover_dtab_size, &data, &lsn,
		    DB_TXN_BACKWARD_ROLL, txninfo);
		if (ret != 0) {
			if (ret != DB_TXN_CKP)
				goto msgerr;
			else
				ret = 0;
		}
	}
	if (ret != 0 && ret != DB_NOTFOUND)
		goto err;

	/*
	 * Pass #3.  If we are recovering to a timestamp or to an LSN,
	 * we need to make sure that we don't roll-forward beyond that
	 * point because there may be non-transactional operations (e.g.,
	 * closes that would fail).  The last_lsn variable is used for
	 * feedback calculations, but use it to set an initial stopping
	 * point for the forward pass, and then reset appropriately to
	 * derive a real stop_lsn that tells how far the forward pass
	 * should go.
	 */
	pass = "forward";
	stop_lsn = last_lsn;
	if (max_lsn != NULL || dbenv->tx_timestamp != 0)
		stop_lsn = ((DB_TXNHEAD *)txninfo)->maxlsn;

	for (ret = logc->get(logc, &lsn, &data, DB_NEXT);
	    ret == 0; ret = logc->get(logc, &lsn, &data, DB_NEXT)) {
		/*
		 * If we are recovering to a timestamp or an LSN,
		 * we need to make sure that we don't try to roll
		 * forward beyond the soon-to-be end of log.
		 */
		if (log_compare(&lsn, &stop_lsn) > 0)
			break;

		if (dbenv->db_feedback != NULL) {
			progress = 67 + (int)(33 * (__lsn_diff(&first_lsn,
			    &last_lsn, &lsn, log_size, 1) / nfiles));
			dbenv->db_feedback(dbenv, DB_RECOVER, progress);
		}
		ret = __db_dispatch(dbenv, dbenv->recover_dtab,
		    dbenv->recover_dtab_size, &data, &lsn,
		    DB_TXN_FORWARD_ROLL, txninfo);
		if (ret != 0) {
			if (ret != DB_TXN_CKP)
				goto msgerr;
			else
				ret = 0;
		}

	}
	if (ret != 0 && ret != DB_NOTFOUND)
		goto err;

	/*
	 * Process any pages that were on the limbo list and move them to
	 * the free list.  Do this before checkpointing the database.
	 */
	 if ((ret = __db_do_the_limbo(dbenv, NULL, NULL, txninfo)) != 0)
		goto err;

	if (max_lsn == NULL)
		region->last_txnid = ((DB_TXNHEAD *)txninfo)->maxid;

	/* Take a checkpoint here to force any dirty data pages to disk. */
	if (dbenv->tx_timestamp != 0) {
		region->last_ckp = ((DB_TXNHEAD *)txninfo)->ckplsn;
		__log_vtruncate(dbenv, &((DB_TXNHEAD *)txninfo)->maxlsn,
		    &((DB_TXNHEAD *)txninfo)->ckplsn);
	}

	if ((ret = dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE)) != 0)
		goto err;

	/* Close all the db files that are open. */
	if ((ret = __dbreg_close_files(dbenv)) != 0)
		goto err;

	if (max_lsn != NULL) {
		region->last_ckp = ((DB_TXNHEAD *)txninfo)->ckplsn;

		/* We are going to truncate, so we'd best close the cursor. */
		if (logc != NULL && (ret = logc->close(logc, 0)) != 0)
			goto err;
		__log_vtruncate(dbenv,
		    max_lsn, &((DB_TXNHEAD *)txninfo)->ckplsn);

		/*
		 * Now we need to open files that should be open in order for
		 * client processing to continue.  However, since we've
		 * truncated the log, we need to recompute from where the
		 * openfiles pass should begin.
		 */
		if ((ret = dbenv->log_cursor(dbenv, &logc, 0)) != 0)
			goto err;
		if ((ret = logc->get(logc, &first_lsn, &data, DB_FIRST)) != 0) {
			if (ret == DB_NOTFOUND)
				ret = 0;
			else
				__db_err(dbenv, "First log record not found");
			goto err;
		}
		if ((ret = __txn_getckp(dbenv, &first_lsn)) == 0 &&
		    (ret = logc->get(logc, &first_lsn, &data, DB_SET)) == 0) {
			/* We have a recent checkpoint.  This is LSN (1). */
			if ((ret = __txn_ckp_read(dbenv,
			    data.data, &ckp_args)) != 0) {
				__db_err(dbenv,
			    "Invalid checkpoint record at [%ld][%ld]",
				    (u_long)first_lsn.file,
				    (u_long)first_lsn.offset);
				goto err;
			}
			first_lsn = ckp_args->ckp_lsn;
		}
		if ((ret = logc->get(logc, &first_lsn, &data, DB_SET)) != 0)
			goto err;
		if ((ret = __env_openfiles(dbenv, logc,
		    txninfo, &data, &first_lsn, NULL, nfiles, 1)) != 0)
			goto err;
	} else if (region->stat.st_nrestores == 0)
		/*
		 * If there are no prepared transactions that need resolution,
		 * we need to reset the transaction ID space and log this fact.
		 */
		if ((ret = __txn_reset(dbenv)) != 0)
			goto err;

	if (FLD_ISSET(dbenv->verbose, DB_VERB_RECOVERY)) {
		(void)time(&now);
		__db_err(dbenv, "Recovery complete at %.24s", ctime(&now));
		__db_err(dbenv, "%s %lx %s [%lu][%lu]",
		    "Maximum transaction ID",
		    ((DB_TXNHEAD *)txninfo)->maxid,
		    "Recovery checkpoint",
		    (u_long)region->last_ckp.file,
		    (u_long)region->last_ckp.offset);
	}

	if (0) {
msgerr:		__db_err(dbenv,
		    "Recovery function for LSN %lu %lu failed on %s pass",
		    (u_long)lsn.file, (u_long)lsn.offset, pass);
	}

done:
err:	if (lockid != DB_LOCK_INVALIDID) {
		if ((t_ret = __rep_unlockpages(dbenv, lockid)) != 0 && ret == 0)
			ret = t_ret;

		if ((t_ret =
		    dbenv->lock_id_free(dbenv, lockid)) != 0 && ret == 0)
			ret = t_ret;
	}

	if (logc != NULL && (t_ret = logc->close(logc, 0)) != 0 && ret == 0)
		ret = t_ret;

	if (txninfo != NULL)
		__db_txnlist_end(dbenv, txninfo);

	if (dtab != NULL)
		__os_free(dbenv, dtab);

	if (ckp_args != NULL)
		__os_free(dbenv, ckp_args);

	dbenv->tx_timestamp = 0;

	/* Restore the state of the thread flag, clear in-recovery flags. */
	if (is_thread)
		F_SET(dbenv, DB_ENV_THREAD);
	F_CLR((DB_LOG *)dbenv->lg_handle, DBLOG_RECOVER);
	F_CLR(region, TXN_IN_RECOVERY);

	return (ret);
}

/*
 * Figure out how many logfiles we have processed.  If we are moving
 * forward (is_forward != 0), then we're computing current - low.  If
 * we are moving backward, we are computing high - current.  max is
 * the number of bytes per logfile.
 */
static double
__lsn_diff(low, high, current, max, is_forward)
	DB_LSN *low, *high, *current;
	u_int32_t max;
	int is_forward;
{
	double nf;

	/*
	 * There are three cases in each direction.  If you are in the
	 * same file, then all you need worry about is the difference in
	 * offsets.  If you are in different files, then either your offsets
	 * put you either more or less than the integral difference in the
	 * number of files -- we need to handle both of these.
	 */
	if (is_forward) {
		if (current->file == low->file)
			nf = (double)(current->offset - low->offset) / max;
		else if (current->offset < low->offset)
			nf = (double)(current->file - low->file - 1) +
			    (double)(max - low->offset + current->offset) / max;
		else
			nf = (double)(current->file - low->file) +
			    (double)(current->offset - low->offset) / max;
	} else {
		if (current->file == high->file)
			nf = (double)(high->offset - current->offset) / max;
		else if (current->offset > high->offset)
			nf = (double)(high->file - current->file - 1) +
			    (double)
			    (max - current->offset + high->offset) / max;
		else
			nf = (double)(high->file - current->file) +
			    (double)(high->offset - current->offset) / max;
	}
	return (nf);
}

/*
 * __log_backup --
 *
 * This is used to find the earliest log record to process when a client
 * is trying to sync up with a master whose max LSN is less than this
 * client's max lsn; we want to roll back everything after that
 *
 * Find the latest checkpoint whose ckp_lsn is less than the max lsn.
 */
static int
__log_backup(dbenv, logc, max_lsn, start_lsn)
	DB_ENV *dbenv;
	DB_LOGC *logc;
	DB_LSN *max_lsn, *start_lsn;
{
	DB_LSN lsn;
	DBT data;
	__txn_ckp_args *ckp_args;
	int ret;

	memset(&data, 0, sizeof(data));
	ckp_args = NULL;

	/*
	 * Follow checkpoints through the log until we find one with
	 * a ckp_lsn less than max_lsn.
	 */
	if ((ret = __txn_getckp(dbenv, &lsn)) != 0)
		goto err;
	while ((ret = logc->get(logc, &lsn, &data, DB_SET)) == 0) {
		if ((ret = __txn_ckp_read(dbenv, data.data, &ckp_args)) != 0)
			return (ret);
		if (log_compare(&ckp_args->ckp_lsn, max_lsn) <= 0) {
			*start_lsn = ckp_args->ckp_lsn;
			break;
		}

		lsn = ckp_args->prev_lsn;
		if (IS_ZERO_LSN(lsn))
			break;
		__os_free(dbenv, ckp_args);
	}

	if (ckp_args != NULL)
		__os_free(dbenv, ckp_args);
err:	if (IS_ZERO_LSN(*start_lsn) && (ret == 0 || ret == DB_NOTFOUND))
		ret = logc->get(logc, start_lsn, &data, DB_FIRST);
	return (ret);
}

/*
 * __log_earliest --
 *
 * Return the earliest recovery point for the log files present.  The
 * earliest recovery time is the time stamp of the first checkpoint record
 * whose checkpoint LSN is greater than the first LSN we process.
 */
static int
__log_earliest(dbenv, logc, lowtime, lowlsn)
	DB_ENV *dbenv;
	DB_LOGC *logc;
	int32_t *lowtime;
	DB_LSN *lowlsn;
{
	DB_LSN first_lsn, lsn;
	DBT data;
	__txn_ckp_args *ckpargs;
	u_int32_t rectype;
	int cmp, ret;

	memset(&data, 0, sizeof(data));
	/*
	 * Read forward through the log looking for the first checkpoint
	 * record whose ckp_lsn is greater than first_lsn.
	 */

	for (ret = logc->get(logc, &first_lsn, &data, DB_FIRST);
	    ret == 0; ret = logc->get(logc, &lsn, &data, DB_NEXT)) {
		memcpy(&rectype, data.data, sizeof(rectype));
		if (rectype != DB___txn_ckp)
			continue;
		if ((ret = __txn_ckp_read(dbenv, data.data, &ckpargs)) == 0) {
			cmp = log_compare(&ckpargs->ckp_lsn, &first_lsn);
			*lowlsn = ckpargs->ckp_lsn;
			*lowtime = ckpargs->timestamp;

			__os_free(dbenv, ckpargs);
			if (cmp >= 0)
				break;
		}
	}

	return (ret);
}

/*
 * __env_openfiles --
 * Perform the pass of recovery that opens files.  This is used
 * both during regular recovery and an initial call to txn_recover (since
 * we need files open in order to abort prepared, but not yet committed
 * transactions).
 *
 * See the comments in db_apprec for a detailed description of the
 * various recovery passes.
 *
 * If we are not doing feedback processing (i.e., we are doing txn_recover
 * processing and in_recovery is zero), then last_lsn can be NULL.
 *
 * PUBLIC: int __env_openfiles __P((DB_ENV *, DB_LOGC *,
 * PUBLIC:     void *, DBT *, DB_LSN *, DB_LSN *, double, int));
 */
int
__env_openfiles(dbenv, logc, txninfo,
    data, open_lsn, last_lsn, nfiles, in_recovery)
	DB_ENV *dbenv;
	DB_LOGC *logc;
	void *txninfo;
	DBT *data;
	DB_LSN *open_lsn, *last_lsn;
	int in_recovery;
	double nfiles;
{
	DB_LSN lsn;
	u_int32_t log_size;
	int progress, ret;

	/*
	 * XXX
	 * Get the log size.  No locking required because we're single-threaded
	 * during recovery.
	 */
	log_size =
	   ((LOG *)(((DB_LOG *)dbenv->lg_handle)->reginfo.primary))->log_size;

	lsn = *open_lsn;
	for (;;) {
		if (in_recovery && dbenv->db_feedback != NULL) {
			DB_ASSERT(last_lsn != NULL);
			progress = (int)(33 * (__lsn_diff(open_lsn,
			   last_lsn, &lsn, log_size, 1) / nfiles));
			dbenv->db_feedback(dbenv, DB_RECOVER, progress);
		}
		ret = __db_dispatch(dbenv,
		    dbenv->recover_dtab, dbenv->recover_dtab_size, data, &lsn,
		    in_recovery ? DB_TXN_OPENFILES : DB_TXN_POPENFILES,
		    txninfo);
		if (ret != 0 && ret != DB_TXN_CKP) {
			__db_err(dbenv,
			    "Recovery function for LSN %lu %lu failed",
			    (u_long)lsn.file, (u_long)lsn.offset);
			break;
		}
		if ((ret = logc->get(logc, &lsn, data, DB_NEXT)) != 0) {
			if (ret == DB_NOTFOUND)
				ret = 0;
			break;
		}
	}

	return (ret);
}
