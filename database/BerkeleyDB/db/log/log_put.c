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

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "dbinc/crypto.h"
#include "dbinc/hmac.h"
#include "dbinc/log.h"
#include "dbinc/rep.h"
#include "dbinc/txn.h"

static int __log_encrypt_record __P((DB_ENV *, DBT *, HDR *, u_int32_t));
static int __log_fill __P((DB_LOG *, DB_LSN *, void *, u_int32_t));
static int __log_flush_commit __P((DB_ENV *, const DB_LSN *, u_int32_t));
static int __log_flush_int __P((DB_LOG *, const DB_LSN *, int));
static int __log_newfh __P((DB_LOG *));
static int __log_put_next __P((DB_ENV *,
    DB_LSN *, const DBT *, HDR *, DB_LSN *));
static int __log_putr __P((DB_LOG *,
    DB_LSN *, const DBT *, u_int32_t, HDR *));
static int __log_write __P((DB_LOG *, void *, u_int32_t));

/*
 * __log_put --
 *	Write a log record.  This is the public interface, DB_ENV->log_put.
 *
 * PUBLIC: int __log_put __P((DB_ENV *, DB_LSN *, const DBT *, u_int32_t));
 */
int
__log_put(dbenv, lsnp, udbt, flags)
	DB_ENV *dbenv;
	DB_LSN *lsnp;
	const DBT *udbt;
	u_int32_t flags;
{
	DB_CIPHER *db_cipher;
	DBT *dbt, t;
	DB_LOG *dblp;
	DB_LSN lsn, old_lsn;
	HDR hdr;
	LOG *lp;
	u_int32_t do_flush, op, writeonly;
	int lock_held, need_free, ret;
	u_int8_t *key;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv,
	    dbenv->lg_handle, "DB_ENV->log_put", DB_INIT_LOG);

	/* Validate arguments. */
	op = DB_OPFLAGS_MASK & flags;
	if (op != 0 && op != DB_COMMIT)
		return (__db_ferr(dbenv, "DB_ENV->log_put", 0));

	/* Check for allowed bit-flags. */
	if (LF_ISSET(~(DB_OPFLAGS_MASK |
	    DB_FLUSH | DB_NOCOPY | DB_PERMANENT | DB_WRNOSYNC)))
		return (__db_ferr(dbenv, "DB_ENV->log_put", 0));

	/* DB_WRNOSYNC and DB_FLUSH are mutually exclusive. */
	if (LF_ISSET(DB_WRNOSYNC) && LF_ISSET(DB_FLUSH))
		return (__db_ferr(dbenv, "DB_ENV->log_put", 1));

	/* Replication clients should never write log records. */
	if (F_ISSET(dbenv, DB_ENV_REP_CLIENT) ||
	    F_ISSET(dbenv, DB_ENV_REP_LOGSONLY)) {
		__db_err(dbenv,
		    "DB_ENV->log_put is illegal on replication clients");
		return (EINVAL);
	}

	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;
	db_cipher = dbenv->crypto_handle;
	dbt = &t;
	t = *udbt;
	lock_held = need_free = 0;
	do_flush = LF_ISSET(DB_FLUSH);
	writeonly = LF_ISSET(DB_WRNOSYNC);

	/*
	 * If we are coming from the logging code, we use an internal
	 * flag, DB_NOCOPY, because we know we can overwrite/encrypt
	 * the log record in place.  Otherwise, if a user called log_put
	 * then we must copy it to new memory so that we know we can
	 * write it.
	 *
	 * We also must copy it to new memory if we are a replication
	 * master so that we retain an unencrypted copy of the log
	 * record to send to clients.
	 */
	if (!LF_ISSET(DB_NOCOPY) || F_ISSET(dbenv, DB_ENV_REP_MASTER)) {
		if (CRYPTO_ON(dbenv))
			t.size += db_cipher->adj_size(udbt->size);
		if ((ret = __os_calloc(dbenv, 1, t.size, &t.data)) != 0)
			goto err;
		need_free = 1;
		memcpy(t.data, udbt->data, udbt->size);
	}
	if ((ret = __log_encrypt_record(dbenv, dbt, &hdr, udbt->size)) != 0)
		goto err;
	if (CRYPTO_ON(dbenv))
		key = db_cipher->mac_key;
	else
		key = NULL;
	/* Otherwise, we actually have a record to put.  Put it. */

	/* Before we grab the region lock, calculate the record's checksum. */
	__db_chksum(dbt->data, dbt->size, key, hdr.chksum);

	R_LOCK(dbenv, &dblp->reginfo);
	lock_held = 1;

	ZERO_LSN(old_lsn);
	if ((ret = __log_put_next(dbenv, &lsn, dbt, &hdr, &old_lsn)) != 0)
		goto err;

	if (F_ISSET(dbenv, DB_ENV_REP_MASTER)) {
		/*
		 * Replication masters need to drop the lock to send
		 * messages, but we want to drop and reacquire it a minimal
		 * number of times.
		 */
		R_UNLOCK(dbenv, &dblp->reginfo);
		lock_held = 0;

		/*
		 * If we changed files and we're in a replicated
		 * environment, we need to inform our clients now that
		 * we've dropped the region lock.
		 *
		 * Note that a failed NEWFILE send is a dropped message
		 * that our client can handle, so we can ignore it.  It's
		 * possible that the record we already put is a commit, so
		 * we don't just want to return failure.
		 */
		if (!IS_ZERO_LSN(old_lsn))
			(void)__rep_send_message(dbenv,
			    DB_EID_BROADCAST, REP_NEWFILE, &old_lsn, NULL, 0);

		/*
		 * Then send the log record itself on to our clients.
		 *
		 * If the send fails and we're a commit or checkpoint,
		 * there's nothing we can do;  the record's in the log.
		 * Flush it, even if we're running with TXN_NOSYNC, on the
		 * grounds that it should be in durable form somewhere.
		 */
		/*
		 * !!!
		 * In the crypto case, we MUST send the udbt, not the
		 * now-encrypted dbt.  Clients have no way to decrypt
		 * without the header.
		 */
		if ((__rep_send_message(dbenv,
		    DB_EID_BROADCAST, REP_LOG, &lsn, udbt, flags) != 0) &&
		    LF_ISSET(DB_PERMANENT))
			do_flush |= DB_FLUSH;
	}

	/*
	 * If needed, do a flush.  Note that failures at this point
	 * are only permissible if we know we haven't written a commit
	 * record;  __log_flush_commit is responsible for enforcing this.
	 *
	 * If a flush is not needed, see if WRITE_NOSYNC was set and we
	 * need to write out the log buffer.
	 */
	if (do_flush || writeonly) {
		if (!lock_held) {
			R_LOCK(dbenv, &dblp->reginfo);
			lock_held = 1;
		}
		if (do_flush)
			ret = __log_flush_commit(dbenv, &lsn, flags);
		else if (lp->b_off != 0)
			/*
			 * writeonly: if there's anything in the current
			 * log buffer, we need to write it out.
			 */
			if ((ret = __log_write(dblp,
			    dblp->bufp, (u_int32_t)lp->b_off)) == 0)
				lp->b_off = 0;
	}

err:	if (lock_held)
		R_UNLOCK(dbenv, &dblp->reginfo);
	if (need_free)
		__os_free(dbenv, dbt->data);

	if (ret == 0)
		*lsnp = lsn;

	return (ret);
}

/*
 * __log_txn_lsn --
 *
 * PUBLIC: void __log_txn_lsn
 * PUBLIC:     __P((DB_ENV *, DB_LSN *, u_int32_t *, u_int32_t *));
 */
void
__log_txn_lsn(dbenv, lsnp, mbytesp, bytesp)
	DB_ENV *dbenv;
	DB_LSN *lsnp;
	u_int32_t *mbytesp, *bytesp;
{
	DB_LOG *dblp;
	LOG *lp;

	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;

	R_LOCK(dbenv, &dblp->reginfo);

	/*
	 * We are trying to get the LSN of the last entry in the log.  We use
	 * this in two places: 1) DB_ENV->txn_checkpiont uses it as a first
	 * value when trying to compute an LSN such that all transactions begun
	 * before it are complete.   2) DB_ENV->txn_begin uses it as the
	 * begin_lsn.
	 *
	 * Typically, it's easy to get the last written LSN, you simply look
	 * at the current log pointer and back up the number of bytes of the
	 * last log record.  However, if the last thing we did was write the
	 * log header of a new log file, then, this doesn't work, so we return
	 * the first log record that will be written in this new file.
	 */
	*lsnp = lp->lsn;
	if (lp->lsn.offset > lp->len)
		lsnp->offset -= lp->len;

	/*
	 * Since we're holding the log region lock, return the bytes put into
	 * the log since the last checkpoint, transaction checkpoint needs it.
	 *
	 * We add the current buffer offset so as to count bytes that have not
	 * yet been written, but are sitting in the log buffer.
	 */
	if (mbytesp != NULL) {
		*mbytesp = lp->stat.st_wc_mbytes;
		*bytesp = (u_int32_t)(lp->stat.st_wc_bytes + lp->b_off);

		lp->stat.st_wc_mbytes = lp->stat.st_wc_bytes = 0;
	}

	R_UNLOCK(dbenv, &dblp->reginfo);
}

/*
 * __log_put_next --
 *	Put the given record as the next in the log, wherever that may
 * turn out to be.
 */
static int
__log_put_next(dbenv, lsn, dbt, hdr, old_lsnp)
	DB_ENV *dbenv;
	DB_LSN *lsn;
	const DBT *dbt;
	HDR *hdr;
	DB_LSN *old_lsnp;
{
	DB_LOG *dblp;
	DB_LSN old_lsn;
	LOG *lp;
	int newfile, ret;

	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;

	/*
	 * Save a copy of lp->lsn before we might decide to switch log
	 * files and change it.  If we do switch log files, and we're
	 * doing replication, we'll need to tell our clients about the
	 * switch, and they need to receive a NEWFILE message
	 * with this "would-be" LSN in order to know they're not
	 * missing any log records.
	 */
	old_lsn = lp->lsn;
	newfile = 0;

	/*
	 * If this information won't fit in the file, or if we're a
	 * replication client environment and have been told to do so,
	 * swap files.
	 */
	if (lp->lsn.offset == 0 ||
	    lp->lsn.offset + hdr->size + dbt->size > lp->log_size) {
		if (hdr->size + sizeof(LOGP) + dbt->size > lp->log_size) {
			__db_err(dbenv,
		    "DB_ENV->log_put: record larger than maximum file size");
			return (EINVAL);
		}

		if ((ret = __log_newfile(dblp, NULL)) != 0)
			return (ret);

		/*
		 * Flag that we switched files, in case we're a master
		 * and need to send this information to our clients.
		 * We postpone doing the actual send until we can
		 * safely release the log region lock and are doing so
		 * anyway.
		 */
		newfile = 1;
	}

	/*
	 * The offset into the log file at this point is the LSN where
	 * we're about to put this record, and is the LSN the caller wants.
	 */
	*lsn = lp->lsn;

	/* If we switched log files, let our caller know where. */
	if (newfile)
		*old_lsnp = old_lsn;

	/* Actually put the record. */
	return (__log_putr(dblp, lsn, dbt, lp->lsn.offset - lp->len, hdr));
}

/*
 * __log_flush_commit --
 *	Flush a record for which the DB_FLUSH flag to log_put has been set.
 */
static int
__log_flush_commit(dbenv, lsnp, flags)
	DB_ENV *dbenv;
	const DB_LSN *lsnp;
	u_int32_t flags;
{
	DB_LOG *dblp;
	DB_LSN flush_lsn;
	LOG *lp;
	int ret;
	u_int32_t op;

	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;
	flush_lsn = *lsnp;
	op = DB_OPFLAGS_MASK & flags;

	if ((ret = __log_flush_int(dblp, &flush_lsn, 1)) == 0)
		return (0);

	/*
	 * If a flush supporting a transaction commit fails, we must abort the
	 * transaction.  (If we aren't doing a commit, return the failure; if
	 * if the commit we care about made it to disk successfully, we just
	 * ignore the failure, because there's no way to undo the commit.)
	 */
	if (op != DB_COMMIT)
		return (ret);

	if (flush_lsn.file != lp->lsn.file || flush_lsn.offset < lp->w_off)
		return (0);

	/*
	 * Else, make sure that the commit record does not get out after we
	 * abort the transaction.  Do this by overwriting the commit record
	 * in the buffer.  (Note that other commits in this buffer will wait
	 * wait until a sucessful write happens, we do not wake them.)  We
	 * point at the right part of the buffer and write an abort record
	 * over the commit.  We must then try and flush the buffer again,
	 * since the interesting part of the buffer may have actually made
	 * it out to disk before there was a failure, we can't know for sure.
	 */
	if (__txn_force_abort(dbenv,
	    dblp->bufp + flush_lsn.offset - lp->w_off) == 0)
		(void)__log_flush_int(dblp, &flush_lsn, 0);

	return (ret);
}

/*
 * __log_newfile --
 *	Initialize and switch to a new log file.  (Note that this is
 * called both when no log yet exists and when we fill a log file.)
 *
 * PUBLIC: int __log_newfile __P((DB_LOG *, DB_LSN *));
 */
int
__log_newfile(dblp, lsnp)
	DB_LOG *dblp;
	DB_LSN *lsnp;
{
	DB_CIPHER *db_cipher;
	DB_ENV *dbenv;
	DB_LSN lsn;
	DBT t;
	HDR hdr;
	LOG *lp;
	int need_free, ret;
	u_int32_t lastoff;
	size_t tsize;
	u_int8_t *tmp;

	dbenv = dblp->dbenv;
	lp = dblp->reginfo.primary;

	/* If we're not at the beginning of a file already, start a new one. */
	if (lp->lsn.offset != 0) {
		/*
		 * Flush the log so this file is out and can be closed.  We
		 * cannot release the region lock here because we need to
		 * protect the end of the file while we switch.  In
		 * particular, a thread with a smaller record than ours
		 * could detect that there is space in the log. Even
		 * blocking that event by declaring the file full would
		 * require all threads to wait here so that the lsn.file
		 * can be moved ahead after the flush completes.  This
		 * probably can be changed if we had an lsn for the
		 * previous file and one for the curent, but it does not
		 * seem like this would get much more throughput, if any.
		 */
		if ((ret = __log_flush_int(dblp, NULL, 0)) != 0)
			return (ret);

		DB_ASSERT(lp->b_off == 0);
		/*
		 * Save the last known offset from the previous file, we'll
		 * need it to initialize the persistent header information.
		 */
		lastoff = lp->lsn.offset;

		/* Point the current LSN to the new file. */
		++lp->lsn.file;
		lp->lsn.offset = 0;

		/* Reset the file write offset. */
		lp->w_off = 0;
	} else
		lastoff = 0;

	/*
	 * Insert persistent information as the first record in every file.
	 * Note that the previous length is wrong for the very first record
	 * of the log, but that's okay, we check for it during retrieval.
	 */
	DB_ASSERT(lp->b_off == 0);

	memset(&t, 0, sizeof(t));
	memset(&hdr, 0, sizeof(HDR));

	need_free = 0;
	tsize = sizeof(LOGP);
	db_cipher = dbenv->crypto_handle;
	if (CRYPTO_ON(dbenv))
		tsize += db_cipher->adj_size(tsize);
	if ((ret = __os_calloc(dbenv, 1, tsize, &tmp)) != 0)
		return (ret);
	lp->persist.log_size = lp->log_size = lp->log_nsize;
	memcpy(tmp, &lp->persist, sizeof(LOGP));
	t.data = tmp;
	t.size = (u_int32_t)tsize;
	need_free = 1;

	if ((ret =
	    __log_encrypt_record(dbenv, &t, &hdr, (u_int32_t)tsize)) != 0)
		goto err;
	__db_chksum(t.data, t.size,
	    (CRYPTO_ON(dbenv)) ? db_cipher->mac_key : NULL, hdr.chksum);
	lsn = lp->lsn;
	if ((ret = __log_putr(dblp, &lsn,
	    &t, lastoff == 0 ? 0 : lastoff - lp->len, &hdr)) != 0)
		goto err;

	/* Update the LSN information returned to the caller. */
	if (lsnp != NULL)
		*lsnp = lp->lsn;

err:
	if (need_free)
		__os_free(dbenv, tmp);
	return (ret);
}

/*
 * __log_putr --
 *	Actually put a record into the log.
 */
static int
__log_putr(dblp, lsn, dbt, prev, h)
	DB_LOG *dblp;
	DB_LSN *lsn;
	const DBT *dbt;
	u_int32_t prev;
	HDR *h;
{
	DB_CIPHER *db_cipher;
	DB_ENV *dbenv;
	DB_LSN f_lsn;
	LOG *lp;
	HDR tmp, *hdr;
	int ret, t_ret;
	size_t b_off, nr;
	u_int32_t w_off;

	dbenv = dblp->dbenv;
	lp = dblp->reginfo.primary;

	/*
	 * If we weren't given a header, use a local one.
	 */
	db_cipher = dbenv->crypto_handle;
	if (h == NULL) {
		hdr = &tmp;
		memset(hdr, 0, sizeof(HDR));
		if (CRYPTO_ON(dbenv))
			hdr->size = HDR_CRYPTO_SZ;
		else
			hdr->size = HDR_NORMAL_SZ;
	} else
		hdr = h;

	/* Save our position in case we fail. */
	b_off = lp->b_off;
	w_off = lp->w_off;
	f_lsn = lp->f_lsn;

	/*
	 * Initialize the header.  If we just switched files, lsn.offset will
	 * be 0, and what we really want is the offset of the previous record
	 * in the previous file.  Fortunately, prev holds the value we want.
	 */
	hdr->prev = prev;
	hdr->len = (u_int32_t)hdr->size + dbt->size;

	/*
	 * If we were passed in a nonzero checksum, our caller calculated
	 * the checksum before acquiring the log mutex, as an optimization.
	 *
	 * If our caller calculated a real checksum of 0, we'll needlessly
	 * recalculate it.  C'est la vie;  there's no out-of-bounds value
	 * here.
	 */
	if (hdr->chksum[0] == 0)
		__db_chksum(dbt->data, dbt->size,
		    (CRYPTO_ON(dbenv)) ? db_cipher->mac_key : NULL,
		    hdr->chksum);

	if ((ret = __log_fill(dblp, lsn, hdr, (u_int32_t)hdr->size)) != 0)
		goto err;

	if ((ret = __log_fill(dblp, lsn, dbt->data, dbt->size)) != 0)
		goto err;

	lp->len = (u_int32_t)(hdr->size + dbt->size);
	lp->lsn.offset += (u_int32_t)(hdr->size + dbt->size);
	return (0);
err:
	/*
	 * If we wrote more than one buffer before failing, get the
	 * first one back.  The extra buffers will fail the checksums
	 * and be ignored.
	 */
	if (w_off + lp->buffer_size < lp->w_off) {
		if ((t_ret =
		    __os_seek(dbenv,
		    &dblp->lfh, 0, 0, w_off, 0, DB_OS_SEEK_SET)) != 0 ||
		    (t_ret = __os_read(dbenv, &dblp->lfh, dblp->bufp,
		    b_off, &nr)) != 0)
			return (__db_panic(dbenv, t_ret));
		if (nr != b_off) {
			__db_err(dbenv, "Short read while restoring log");
			return (__db_panic(dbenv, EIO));
		}
	}

	/* Reset to where we started. */
	lp->w_off = w_off;
	lp->b_off = b_off;
	lp->f_lsn = f_lsn;

	return (ret);
}

/*
 * __log_flush --
 *	Write all records less than or equal to the specified LSN.
 *
 * PUBLIC: int __log_flush __P((DB_ENV *, const DB_LSN *));
 */
int
__log_flush(dbenv, lsn)
	DB_ENV *dbenv;
	const DB_LSN *lsn;
{
	DB_LOG *dblp;
	int ret;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv,
	    dbenv->lg_handle, "DB_ENV->log_flush", DB_INIT_LOG);

	dblp = dbenv->lg_handle;
	R_LOCK(dbenv, &dblp->reginfo);
	ret = __log_flush_int(dblp, lsn, 1);
	R_UNLOCK(dbenv, &dblp->reginfo);
	return (ret);
}

/*
 * __log_flush_int --
 *	Write all records less than or equal to the specified LSN; internal
 *	version.
 */
static int
__log_flush_int(dblp, lsnp, release)
	DB_LOG *dblp;
	const DB_LSN *lsnp;
	int release;
{
	DB_ENV *dbenv;
	DB_LSN flush_lsn, f_lsn;
	DB_MUTEX *flush_mutexp;
	LOG *lp;
	int current, do_flush, first, ret;
	size_t b_off;
	struct __db_commit *commit;
	u_int32_t ncommit, w_off;

	ret = 0;
	ncommit = 0;
	dbenv = dblp->dbenv;
	lp = dblp->reginfo.primary;
	flush_mutexp = R_ADDR(&dblp->reginfo, lp->flush_mutex_off);

	/*
	 * If no LSN specified, flush the entire log by setting the flush LSN
	 * to the last LSN written in the log.  Otherwise, check that the LSN
	 * isn't a non-existent record for the log.
	 */
	if (lsnp == NULL) {
		flush_lsn.file = lp->lsn.file;
		flush_lsn.offset = lp->lsn.offset - lp->len;
	} else if (lsnp->file > lp->lsn.file ||
	    (lsnp->file == lp->lsn.file &&
	    lsnp->offset > lp->lsn.offset - lp->len)) {
		__db_err(dbenv,
		    "DB_ENV->log_flush: LSN past current end-of-log");
		return (EINVAL);
	} else {
		/*
		 * See if we need to wait.  s_lsn is not locked so some
		 * care is needed.  The sync point can only move forward.
		 * If the file we want is in the past we are done.
		 * If the file numbers are the same check the offset.
		 * If this fails check the file numbers again since the
		 * offset might have changed while we were looking.
		 * This all assumes we can read an integer in one
		 * state or the other, not in transition.
		 */
		if (lp->s_lsn.file > lsnp->file)
			return (0);

		if (lp->s_lsn.file == lsnp->file &&
		    lp->s_lsn.offset > lsnp->offset)
			return (0);

		if (lp->s_lsn.file > lsnp->file)
			return (0);

		flush_lsn = *lsnp;
	}

	/*
	 * If a flush is in progress and we're allowed to do so, drop
	 * the region lock and block waiting for the next flush.
	 */
	if (release && lp->in_flush != 0) {
		if ((commit = SH_TAILQ_FIRST(
		    &lp->free_commits, __db_commit)) == NULL) {
			if ((ret =
			    __db_shalloc(dblp->reginfo.addr,
			    sizeof(struct __db_commit),
			    MUTEX_ALIGN, &commit)) != 0)
				goto flush;
			memset(commit, 0, sizeof(*commit));
			if ((ret = __db_mutex_setup(dbenv, &dblp->reginfo,
			    &commit->mutex, MUTEX_SELF_BLOCK |
			    MUTEX_NO_RLOCK)) != 0) {
				__db_shalloc_free(dblp->reginfo.addr, commit);
				return (ret);
			}
			MUTEX_LOCK(dbenv, &commit->mutex);
		} else
			SH_TAILQ_REMOVE(
			    &lp->free_commits, commit, links, __db_commit);

		lp->ncommit++;

		/*
		 * Flushes may be requested out of LSN order;  be
		 * sure we only move lp->t_lsn forward.
		 */
		if (log_compare(&lp->t_lsn, &flush_lsn) < 0)
			lp->t_lsn = flush_lsn;

		commit->lsn = flush_lsn;
		SH_TAILQ_INSERT_HEAD(
		    &lp->commits, commit, links, __db_commit);
		R_UNLOCK(dbenv, &dblp->reginfo);
		/* Wait here for the in-progress flush to finish. */
		MUTEX_LOCK(dbenv, &commit->mutex);
		R_LOCK(dbenv, &dblp->reginfo);

		lp->ncommit--;
		/*
		 * Grab the flag before freeing the struct to see if
		 * we need to flush the log to commit.  If so,
		 * use the maximal lsn for any committing thread.
		 */
		do_flush = F_ISSET(commit, DB_COMMIT_FLUSH);
		F_CLR(commit, DB_COMMIT_FLUSH);
		SH_TAILQ_INSERT_HEAD(
		    &lp->free_commits, commit, links, __db_commit);
		if (do_flush) {
			lp->in_flush--;
			flush_lsn = lp->t_lsn;
		} else
			return (0);
	}

	/*
	 * Protect flushing with its own mutex so we can release
	 * the region lock except during file switches.
	 */
flush:	MUTEX_LOCK(dbenv, flush_mutexp);

	/*
	 * If the LSN is less than or equal to the last-sync'd LSN, we're done.
	 * Note, the last-sync LSN saved in s_lsn is the LSN of the first byte
	 * after the byte we absolutely know was written to disk, so the test
	 * is <, not <=.
	 */
	if (flush_lsn.file < lp->s_lsn.file ||
	    (flush_lsn.file == lp->s_lsn.file &&
	    flush_lsn.offset < lp->s_lsn.offset)) {
		MUTEX_UNLOCK(dbenv, flush_mutexp);
		goto done;
	}

	/*
	 * We may need to write the current buffer.  We have to write the
	 * current buffer if the flush LSN is greater than or equal to the
	 * buffer's starting LSN.
	 */
	current = 0;
	if (lp->b_off != 0 && log_compare(&flush_lsn, &lp->f_lsn) >= 0) {
		if ((ret = __log_write(dblp,
		    dblp->bufp, (u_int32_t)lp->b_off)) != 0) {
			MUTEX_UNLOCK(dbenv, flush_mutexp);
			goto done;
		}

		lp->b_off = 0;
		current = 1;
	}

	/*
	 * It's possible that this thread may never have written to this log
	 * file.  Acquire a file descriptor if we don't already have one.
	 * One last check -- if we're not writing anything from the current
	 * buffer, don't bother.  We have nothing to write and nothing to
	 * sync.
	 */
	if (!F_ISSET(&dblp->lfh, DB_FH_VALID) || dblp->lfname != lp->lsn.file)
		if (!current || (ret = __log_newfh(dblp)) != 0) {
			MUTEX_UNLOCK(dbenv, flush_mutexp);
			goto done;
		}

	/*
	 * We are going to flush, release the region.
	 * First get the current state of the buffer since
	 * another write may come in, but we may not flush it.
	 */
	b_off = lp->b_off;
	w_off = lp->w_off;
	f_lsn = lp->f_lsn;
	lp->in_flush++;
	if (release)
		R_UNLOCK(dbenv, &dblp->reginfo);

	/* Sync all writes to disk. */
	if ((ret = __os_fsync(dbenv, &dblp->lfh)) != 0) {
		MUTEX_UNLOCK(dbenv, flush_mutexp);
		if (release)
			R_LOCK(dbenv, &dblp->reginfo);
		ret = __db_panic(dbenv, ret);
		return (ret);
	}

	/*
	 * Set the last-synced LSN.
	 * This value must be set to the LSN past the last complete
	 * record that has been flushed.  This is at least the first
	 * lsn, f_lsn.  If the buffer is empty, b_off == 0, then
	 * we can move up to write point since the first lsn is not
	 * set for the new buffer.
	 */
	lp->s_lsn = f_lsn;
	if (b_off == 0)
		lp->s_lsn.offset = w_off;

	MUTEX_UNLOCK(dbenv, flush_mutexp);
	if (release)
		R_LOCK(dbenv, &dblp->reginfo);

	lp->in_flush--;
	++lp->stat.st_scount;

	/*
	 * How many flush calls (usually commits) did this call actually sync?
	 * At least one, if it got here.
	 */
	ncommit = 1;
done:
	if (lp->ncommit != 0) {
		first = 1;
		for (commit = SH_TAILQ_FIRST(&lp->commits, __db_commit);
		    commit != NULL;
		    commit = SH_TAILQ_NEXT(commit, links, __db_commit))
			if (log_compare(&lp->s_lsn, &commit->lsn) > 0) {
				MUTEX_UNLOCK(dbenv, &commit->mutex);
				SH_TAILQ_REMOVE(
				    &lp->commits, commit, links, __db_commit);
				ncommit++;
			} else if (first == 1) {
				F_SET(commit, DB_COMMIT_FLUSH);
				MUTEX_UNLOCK(dbenv, &commit->mutex);
				SH_TAILQ_REMOVE(
				    &lp->commits, commit, links, __db_commit);
				/*
				 * This thread will wake and flush.
				 * If another thread commits and flushes
				 * first we will waste a trip trough the
				 * mutex.
				 */
				lp->in_flush++;
				first = 0;
			}
	}
	if (lp->stat.st_maxcommitperflush < ncommit)
		lp->stat.st_maxcommitperflush = ncommit;
	if (lp->stat.st_mincommitperflush > ncommit ||
	    lp->stat.st_mincommitperflush == 0)
		lp->stat.st_mincommitperflush = ncommit;

	return (ret);
}

/*
 * __log_fill --
 *	Write information into the log.
 */
static int
__log_fill(dblp, lsn, addr, len)
	DB_LOG *dblp;
	DB_LSN *lsn;
	void *addr;
	u_int32_t len;
{
	LOG *lp;
	u_int32_t bsize, nrec;
	size_t nw, remain;
	int ret;

	lp = dblp->reginfo.primary;
	bsize = lp->buffer_size;

	while (len > 0) {			/* Copy out the data. */
		/*
		 * If we're beginning a new buffer, note the user LSN to which
		 * the first byte of the buffer belongs.  We have to know this
		 * when flushing the buffer so that we know if the in-memory
		 * buffer needs to be flushed.
		 */
		if (lp->b_off == 0)
			lp->f_lsn = *lsn;

		/*
		 * If we're on a buffer boundary and the data is big enough,
		 * copy as many records as we can directly from the data.
		 */
		if (lp->b_off == 0 && len >= bsize) {
			nrec = len / bsize;
			if ((ret = __log_write(dblp, addr, nrec * bsize)) != 0)
				return (ret);
			addr = (u_int8_t *)addr + nrec * bsize;
			len -= nrec * bsize;
			++lp->stat.st_wcount_fill;
			continue;
		}

		/* Figure out how many bytes we can copy this time. */
		remain = bsize - lp->b_off;
		nw = remain > len ? len : remain;
		memcpy(dblp->bufp + lp->b_off, addr, nw);
		addr = (u_int8_t *)addr + nw;
		len -= (u_int32_t)nw;
		lp->b_off += nw;

		/* If we fill the buffer, flush it. */
		if (lp->b_off == bsize) {
			if ((ret = __log_write(dblp, dblp->bufp, bsize)) != 0)
				return (ret);
			lp->b_off = 0;
			++lp->stat.st_wcount_fill;
		}
	}
	return (0);
}

/*
 * __log_write --
 *	Write the log buffer to disk.
 */
static int
__log_write(dblp, addr, len)
	DB_LOG *dblp;
	void *addr;
	u_int32_t len;
{
	DB_ENV *dbenv;
	LOG *lp;
	size_t nw;
	int ret;

	dbenv = dblp->dbenv;
	lp = dblp->reginfo.primary;

	/*
	 * If we haven't opened the log file yet or the current one
	 * has changed, acquire a new log file.
	 */
	if (!F_ISSET(&dblp->lfh, DB_FH_VALID) || dblp->lfname != lp->lsn.file)
		if ((ret = __log_newfh(dblp)) != 0)
			return (ret);

	/*
	 * Seek to the offset in the file (someone may have written it
	 * since we last did).
	 */
	if ((ret =
	    __os_seek(dbenv,
	    &dblp->lfh, 0, 0, lp->w_off, 0, DB_OS_SEEK_SET)) != 0 ||
	    (ret = __os_write(dbenv, &dblp->lfh, addr, len, &nw)) != 0)
		return (ret);

	/* Reset the buffer offset and update the seek offset. */
	lp->w_off += len;

	/* Update written statistics. */
	if ((lp->stat.st_w_bytes += len) >= MEGABYTE) {
		lp->stat.st_w_bytes -= MEGABYTE;
		++lp->stat.st_w_mbytes;
	}
	if ((lp->stat.st_wc_bytes += len) >= MEGABYTE) {
		lp->stat.st_wc_bytes -= MEGABYTE;
		++lp->stat.st_wc_mbytes;
	}
	++lp->stat.st_wcount;

	return (0);
}

/*
 * __log_file --
 *	Map a DB_LSN to a file name.
 *
 * PUBLIC: int __log_file __P((DB_ENV *, const DB_LSN *, char *, size_t));
 */
int
__log_file(dbenv, lsn, namep, len)
	DB_ENV *dbenv;
	const DB_LSN *lsn;
	char *namep;
	size_t len;
{
	DB_LOG *dblp;
	int ret;
	char *name;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv,
	    dbenv->lg_handle, "DB_ENV->log_file", DB_INIT_LOG);

	dblp = dbenv->lg_handle;
	R_LOCK(dbenv, &dblp->reginfo);
	ret = __log_name(dblp, lsn->file, &name, NULL, 0);
	R_UNLOCK(dbenv, &dblp->reginfo);
	if (ret != 0)
		return (ret);

	/* Check to make sure there's enough room and copy the name. */
	if (len < strlen(name) + 1) {
		*namep = '\0';
		__db_err(dbenv, "DB_ENV->log_file: name buffer is too short");
		return (EINVAL);
	}
	(void)strcpy(namep, name);
	__os_free(dbenv, name);

	return (0);
}

/*
 * __log_newfh --
 *	Acquire a file handle for the current log file.
 */
static int
__log_newfh(dblp)
	DB_LOG *dblp;
{
	DB_ENV *dbenv;
	LOG *lp;
	int ret;
	char *name;

	dbenv = dblp->dbenv;
	lp = dblp->reginfo.primary;

	/* Close any previous file descriptor. */
	if (F_ISSET(&dblp->lfh, DB_FH_VALID))
		(void)__os_closehandle(dbenv, &dblp->lfh);

	/*
	 * Get the path of the new file and open it.
	 *
	 * Adding DB_OSO_LOG to the flags may add additional platform-specific
	 * optimizations.  On WinNT, the logfile is preallocated, which may
	 * have a time penalty at startup, but have better overall throughput.
	 * We are not certain that this works reliably, so enable at your own
	 * risk.
	 *
	 * XXX:
	 * Initialize the log file size.  This is a hack to push the log's
	 * maximum size down into the Windows __os_open routine, because it
	 * wants to pre-allocate it.
	 */
	dblp->lfname = lp->lsn.file;
	dblp->lfh.log_size = lp->log_size;
	if ((ret = __log_name(dblp, dblp->lfname,
	    &name, &dblp->lfh,
	    DB_OSO_CREATE |/* DB_OSO_LOG |*/ DB_OSO_SEQ |
	    (F_ISSET(dbenv, DB_ENV_DIRECT_LOG) ? DB_OSO_DIRECT : 0))) != 0)
		__db_err(dbenv,
		    "DB_ENV->log_put: %s: %s", name, db_strerror(ret));

	__os_free(dbenv, name);
	return (ret);
}

/*
 * __log_name --
 *	Return the log name for a particular file, and optionally open it.
 *
 * PUBLIC: int __log_name __P((DB_LOG *,
 * PUBLIC:     u_int32_t, char **, DB_FH *, u_int32_t));
 */
int
__log_name(dblp, filenumber, namep, fhp, flags)
	DB_LOG *dblp;
	u_int32_t filenumber, flags;
	char **namep;
	DB_FH *fhp;
{
	DB_ENV *dbenv;
	LOG *lp;
	int ret;
	char *oname;
	char old[sizeof(LFPREFIX) + 5 + 20], new[sizeof(LFPREFIX) + 10 + 20];

	dbenv = dblp->dbenv;
	lp = dblp->reginfo.primary;

	/*
	 * !!!
	 * The semantics of this routine are bizarre.
	 *
	 * The reason for all of this is that we need a place where we can
	 * intercept requests for log files, and, if appropriate, check for
	 * both the old-style and new-style log file names.  The trick is
	 * that all callers of this routine that are opening the log file
	 * read-only want to use an old-style file name if they can't find
	 * a match using a new-style name.  The only down-side is that some
	 * callers may check for the old-style when they really don't need
	 * to, but that shouldn't mess up anything, and we only check for
	 * the old-style name when we've already failed to find a new-style
	 * one.
	 *
	 * Create a new-style file name, and if we're not going to open the
	 * file, return regardless.
	 */
	(void)snprintf(new, sizeof(new), LFNAME, filenumber);
	if ((ret = __db_appname(dbenv,
	    DB_APP_LOG, new, 0, NULL, namep)) != 0 || fhp == NULL)
		return (ret);

	/* Open the new-style file -- if we succeed, we're done. */
	if ((ret = __os_open(dbenv, *namep, flags, lp->persist.mode, fhp)) == 0)
		return (0);

	/*
	 * The open failed... if the DB_RDONLY flag isn't set, we're done,
	 * the caller isn't interested in old-style files.
	 */
	if (!LF_ISSET(DB_OSO_RDONLY)) {
		__db_err(dbenv,
		    "%s: log file open failed: %s", *namep, db_strerror(ret));
		return (__db_panic(dbenv, ret));
	}

	/* Create an old-style file name. */
	(void)snprintf(old, sizeof(old), LFNAME_V1, filenumber);
	if ((ret = __db_appname(dbenv, DB_APP_LOG, old, 0, NULL, &oname)) != 0)
		goto err;

	/*
	 * Open the old-style file -- if we succeed, we're done.  Free the
	 * space allocated for the new-style name and return the old-style
	 * name to the caller.
	 */
	if ((ret = __os_open(dbenv,
	    oname, flags, lp->persist.mode, fhp)) == 0) {
		__os_free(dbenv, *namep);
		*namep = oname;
		return (0);
	}

	/*
	 * Couldn't find either style of name -- return the new-style name
	 * for the caller's error message.  If it's an old-style name that's
	 * actually missing we're going to confuse the user with the error
	 * message, but that implies that not only were we looking for an
	 * old-style name, but we expected it to exist and we weren't just
	 * looking for any log file.  That's not a likely error.
	 */
err:	__os_free(dbenv, oname);
	return (ret);
}

/*
 * __log_rep_put --
 *	Short-circuit way for replication clients to put records into the
 * log.  Replication clients' logs need to be laid out exactly their masters'
 * are, so we let replication take responsibility for when the log gets
 * flushed, when log switches files, etc.  This is just a thin PUBLIC wrapper
 * for __log_putr with a slightly prettier interface.
 *
 * Note that the log region mutex should be held when this is called.
 *
 * PUBLIC: int __log_rep_put __P((DB_ENV *, DB_LSN *, const DBT *));
 */
int
__log_rep_put(dbenv, lsnp, rec)
	DB_ENV *dbenv;
	DB_LSN *lsnp;
	const DBT *rec;
{
	DB_CIPHER *db_cipher;
	DB_LOG *dblp;
	HDR hdr;
	DBT *dbt, t;
	LOG *lp;
	int need_free, ret;

	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;

	memset(&hdr, 0, sizeof(HDR));
	t = *rec;
	dbt = &t;
	need_free = 0;
	db_cipher = (DB_CIPHER *)dbenv->crypto_handle;
	if (CRYPTO_ON(dbenv))
		t.size += db_cipher->adj_size(rec->size);
	if ((ret = __os_calloc(dbenv, 1, t.size, &t.data)) != 0)
		goto err;
	need_free = 1;
	memcpy(t.data, rec->data, rec->size);

	if ((ret = __log_encrypt_record(dbenv, dbt, &hdr, rec->size)) != 0)
		goto err;
	__db_chksum(t.data, t.size,
	    (CRYPTO_ON(dbenv)) ? db_cipher->mac_key : NULL, hdr.chksum);

	DB_ASSERT(log_compare(lsnp, &lp->lsn) == 0);
	ret = __log_putr(dblp, lsnp, dbt, lp->lsn.offset - lp->len, &hdr);
err:
	if (need_free)
		__os_free(dbenv, t.data);
	return (ret);
}

static int
__log_encrypt_record(dbenv, dbt, hdr, orig)
	DB_ENV *dbenv;
	DBT *dbt;
	HDR *hdr;
	u_int32_t orig;
{
	DB_CIPHER *db_cipher;
	int ret;

	if (CRYPTO_ON(dbenv)) {
		db_cipher = (DB_CIPHER *)dbenv->crypto_handle;
		hdr->size = HDR_CRYPTO_SZ;
		hdr->orig_size = orig;
		if ((ret = db_cipher->encrypt(dbenv, db_cipher->data,
		    hdr->iv, dbt->data, dbt->size)) != 0)
			return (ret);
	} else {
		hdr->size = HDR_NORMAL_SZ;
	}
	return (0);
}
