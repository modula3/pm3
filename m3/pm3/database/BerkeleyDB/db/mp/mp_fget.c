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
#include "dbinc/db_shash.h"
#include "dbinc/mp.h"

#ifdef HAVE_FILESYSTEM_NOTZERO
static int __memp_fs_notzero
    __P((DB_ENV *, DB_MPOOLFILE *, MPOOLFILE *, db_pgno_t *));
#endif

/*
 * __memp_fget --
 *	Get a page from the file.
 *
 * PUBLIC: int __memp_fget
 * PUBLIC:     __P((DB_MPOOLFILE *, db_pgno_t *, u_int32_t, void *));
 */
int
__memp_fget(dbmfp, pgnoaddr, flags, addrp)
	DB_MPOOLFILE *dbmfp;
	db_pgno_t *pgnoaddr;
	u_int32_t flags;
	void *addrp;
{
	enum { FIRST_FOUND, FIRST_MISS, SECOND_FOUND, SECOND_MISS } state;
	BH *alloc_bhp, *bhp;
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	DB_MPOOL_HASH *hp;
	MPOOL *c_mp, *mp;
	MPOOLFILE *mfp;
	roff_t mf_offset;
	u_int32_t n_cache, st_hsearch;
	int b_incr, extending, first, ret;

	*(void **)addrp = NULL;

	dbmp = dbmfp->dbmp;
	dbenv = dbmp->dbenv;

	PANIC_CHECK(dbenv);

	mp = dbmp->reginfo[0].primary;
	mfp = dbmfp->mfp;
	mf_offset = R_OFFSET(dbmp->reginfo, mfp);
	alloc_bhp = bhp = NULL;
	hp = NULL;
	b_incr = extending = ret = 0;

	/*
	 * Validate arguments.
	 *
	 * !!!
	 * Don't test for DB_MPOOL_CREATE and DB_MPOOL_NEW flags for readonly
	 * files here, and create non-existent pages in readonly files if the
	 * flags are set, later.  The reason is that the hash access method
	 * wants to get empty pages that don't really exist in readonly files.
	 * The only alternative is for hash to write the last "bucket" all the
	 * time, which we don't want to do because one of our big goals in life
	 * is to keep database files small.  It's sleazy as hell, but we catch
	 * any attempt to actually write the file in memp_fput().
	 */
#define	OKFLAGS		(DB_MPOOL_CREATE | DB_MPOOL_LAST | DB_MPOOL_NEW)
	if (flags != 0) {
		if ((ret = __db_fchk(dbenv, "memp_fget", flags, OKFLAGS)) != 0)
			return (ret);

		switch (flags) {
		case DB_MPOOL_CREATE:
			break;
		case DB_MPOOL_LAST:
			/* Get the last page number in the file. */
			if (flags == DB_MPOOL_LAST) {
				R_LOCK(dbenv, dbmp->reginfo);
				*pgnoaddr = mfp->last_pgno;
				R_UNLOCK(dbenv, dbmp->reginfo);
			}
			break;
		case DB_MPOOL_NEW:
			/*
			 * If always creating a page, skip the first search
			 * of the hash bucket.
			 */
			if (flags == DB_MPOOL_NEW)
				goto alloc;
			break;
		default:
			return (__db_ferr(dbenv, "memp_fget", 1));
		}
	}

	/*
	 * If mmap'ing the file and the page is not past the end of the file,
	 * just return a pointer.
	 *
	 * The page may be past the end of the file, so check the page number
	 * argument against the original length of the file.  If we previously
	 * returned pages past the original end of the file, last_pgno will
	 * have been updated to match the "new" end of the file, and checking
	 * against it would return pointers past the end of the mmap'd region.
	 *
	 * If another process has opened the file for writing since we mmap'd
	 * it, we will start playing the game by their rules, i.e. everything
	 * goes through the cache.  All pages previously returned will be safe,
	 * as long as the correct locking protocol was observed.
	 *
	 * We don't discard the map because we don't know when all of the
	 * pages will have been discarded from the process' address space.
	 * It would be possible to do so by reference counting the open
	 * pages from the mmap, but it's unclear to me that it's worth it.
	 */
	if (dbmfp->addr != NULL &&
	    F_ISSET(mfp, MP_CAN_MMAP) && *pgnoaddr <= mfp->orig_last_pgno) {
		*(void **)addrp =
		    R_ADDR(dbmfp, *pgnoaddr * mfp->stat.st_pagesize);
		++mfp->stat.st_map;
		return (0);
	}

hb_search:
	/*
	 * Determine the cache and hash bucket where this page lives and get
	 * local pointers to them.  Reset on each pass through this code, the
	 * page number can change.
	 */
	n_cache = NCACHE(mp, mf_offset, *pgnoaddr);
	c_mp = dbmp->reginfo[n_cache].primary;
	hp = R_ADDR(&dbmp->reginfo[n_cache], c_mp->htab);
	hp = &hp[NBUCKET(c_mp, mf_offset, *pgnoaddr)];

	/* Search the hash chain for the page. */
retry:	st_hsearch = 0;
	MUTEX_LOCK(dbenv, &hp->hash_mutex);
	for (bhp = SH_TAILQ_FIRST(&hp->hash_bucket, __bh);
	    bhp != NULL; bhp = SH_TAILQ_NEXT(bhp, hq, __bh)) {
		++st_hsearch;
		if (bhp->pgno != *pgnoaddr || bhp->mf_offset != mf_offset)
			continue;

		/*
		 * Increment the reference count.  We may discard the hash
		 * bucket lock as we evaluate and/or read the buffer, so we
		 * need to ensure it doesn't move and its contents remain
		 * unchanged.
		 */
		if (bhp->ref == UINT16_T_MAX) {
			__db_err(dbenv,
			    "%s: page %lu: reference count overflow",
			    __memp_fn(dbmfp), (u_long)bhp->pgno);
			ret = EINVAL;
			MUTEX_UNLOCK(dbenv, &hp->hash_mutex);
			goto err;
		}
		++bhp->ref;
		b_incr = 1;

		/*
		 * BH_LOCKED --
		 * I/O is in progress or sync is waiting on the buffer to write
		 * it.  Because we've incremented the buffer reference count,
		 * we know the buffer can't move.  Unlock the bucket lock, wait
		 * for the buffer to become available, reacquire the bucket.
		 */
		for (first = 1; F_ISSET(bhp, BH_LOCKED) &&
		    !F_ISSET(dbenv, DB_ENV_NOLOCKING); first = 0) {
			/*
			 * If someone is trying to sync this buffer and the
			 * buffer is hot, they may never get in.  Give up
			 * and try again.
			 */
			if (!first && bhp->ref_sync != 0) {
				--bhp->ref;
				b_incr = 0;
				MUTEX_UNLOCK(dbenv, &hp->hash_mutex);
				__os_yield(dbenv, 1);
				goto retry;
			}

			MUTEX_UNLOCK(dbenv, &hp->hash_mutex);
			/*
			 * Explicitly yield the processor if not the first pass
			 * through this loop -- if we don't, we might run to the
			 * end of our CPU quantum as we will simply be swapping
			 * between the two locks.
			 */
			if (!first)
				__os_yield(dbenv, 1);

			MUTEX_LOCK(dbenv, &bhp->mutex);
			/* Wait for I/O to finish... */
			MUTEX_UNLOCK(dbenv, &bhp->mutex);
			MUTEX_LOCK(dbenv, &hp->hash_mutex);
		}

		++mfp->stat.st_cache_hit;
		break;
	}

	/*
	 * Update the hash bucket search statistics -- do now because our next
	 * search may be for a different bucket.
	 */
	++c_mp->stat.st_hash_searches;
	if (st_hsearch > c_mp->stat.st_hash_longest)
		c_mp->stat.st_hash_longest = st_hsearch;
	c_mp->stat.st_hash_examined += st_hsearch;

	/*
	 * There are 4 possible paths to this location:
	 *
	 * FIRST_MISS:
	 *	Didn't find the page in the hash bucket on our first pass:
	 *	bhp == NULL, alloc_bhp == NULL
	 *
	 * FIRST_FOUND:
	 *	Found the page in the hash bucket on our first pass:
	 *	bhp != NULL, alloc_bhp == NULL
	 *
	 * SECOND_FOUND:
	 *	Didn't find the page in the hash bucket on the first pass,
	 *	allocated space, and found the page in the hash bucket on
	 *	our second pass:
	 *	bhp != NULL, alloc_bhp != NULL
	 *
	 * SECOND_MISS:
	 *	Didn't find the page in the hash bucket on the first pass,
	 *	allocated space, and didn't find the page in the hash bucket
	 *	on our second pass:
	 *	bhp == NULL, alloc_bhp != NULL
	 */
	state = bhp == NULL ?
	    (alloc_bhp == NULL ? FIRST_MISS : SECOND_MISS) :
	    (alloc_bhp == NULL ? FIRST_FOUND : SECOND_FOUND);
	switch (state) {
	case FIRST_FOUND:
		/* We found the buffer in our first check -- we're done. */
		break;
	case FIRST_MISS:
		/*
		 * We didn't find the buffer in our first check.  Figure out
		 * if the page exists, and allocate structures so we can add
		 * the page to the buffer pool.
		 */
		MUTEX_UNLOCK(dbenv, &hp->hash_mutex);

alloc:		/*
		 * If DB_MPOOL_NEW is set, we have to allocate a page number.
		 * If neither DB_MPOOL_CREATE or DB_MPOOL_CREATE is set, then
		 * it's an error to try and get a page past the end of file.
		 */
		COMPQUIET(n_cache, 0);

		extending = ret = 0;
		R_LOCK(dbenv, dbmp->reginfo);
		switch (flags) {
		case DB_MPOOL_NEW:
			extending = 1;
			*pgnoaddr = mfp->last_pgno + 1;
			break;
		case DB_MPOOL_CREATE:
			extending = *pgnoaddr > mfp->last_pgno;
			break;
		default:
			ret = *pgnoaddr > mfp->last_pgno ? DB_PAGE_NOTFOUND : 0;
			break;
		}
		R_UNLOCK(dbenv, dbmp->reginfo);
		if (ret != 0)
			goto err;

		/*
		 * !!!
		 * In the DB_MPOOL_NEW code path, mf_offset and n_cache have
		 * not yet been initialized.
		 */
		mf_offset = R_OFFSET(dbmp->reginfo, mfp);
		n_cache = NCACHE(mp, mf_offset, *pgnoaddr);

		/* Allocate a new buffer header and data space. */
		if ((ret = __memp_alloc(dbmp,
		    &dbmp->reginfo[n_cache], mfp, 0, NULL, &alloc_bhp)) != 0)
			goto err;
#ifdef DIAGNOSTIC
		if ((db_alignp_t)alloc_bhp->buf & (sizeof(size_t) - 1)) {
			__db_err(dbenv,
			    "Error: buffer data is NOT size_t aligned");
			ret = EINVAL;
			goto err;
		}
#endif
		/*
		 * If we are extending the file, we'll need the region lock
		 * again.
		 */
		if (extending)
			R_LOCK(dbenv, dbmp->reginfo);

		/*
		 * DB_MPOOL_NEW does not guarantee you a page unreferenced by
		 * any other thread of control.  (That guarantee is interesting
		 * for DB_MPOOL_NEW, unlike DB_MPOOL_CREATE, because the caller
		 * did not specify the page number, and so, may reasonably not
		 * have any way to lock the page outside of mpool.) Regardless,
		 * if we allocate the page, and some other thread of control
		 * requests the page by number, we will not detect that and the
		 * thread of control that allocated using DB_MPOOL_NEW may not
		 * have a chance to initialize the page.  (Note: we *could*
		 * detect this case if we set a flag in the buffer header which
		 * guaranteed that no gets of the page would succeed until the
		 * reference count went to 0, that is, until the creating page
		 * put the page.)  What we do guarantee is that if two threads
		 * of control are both doing DB_MPOOL_NEW calls, they won't
		 * collide, that is, they won't both get the same page.
		 *
		 * There's a possibility that another thread allocated the page
		 * we were planning to allocate while we were off doing buffer
		 * allocation.  We can do that by making sure the page number
		 * we were going to use is still available.  If it's not, then
		 * we check to see if the next available page number hashes to
		 * the same mpool region as the old one -- if it does, we can
		 * continue, otherwise, we have to start over.
		 */
		if (flags == DB_MPOOL_NEW && *pgnoaddr != mfp->last_pgno + 1) {
			*pgnoaddr = mfp->last_pgno + 1;
			if (n_cache != NCACHE(mp, mf_offset, *pgnoaddr)) {
				__db_shalloc_free(
				    dbmp->reginfo[n_cache].addr, alloc_bhp);
				/*
				 * flags == DB_MPOOL_NEW, so extending is set
				 * and we're holding the region locked.
				 */
				R_UNLOCK(dbenv, dbmp->reginfo);

				alloc_bhp = NULL;
				goto alloc;
			}
		}

		/*
		 * We released the region lock, so another thread might have
		 * extended the file.  Update the last_pgno and initialize
		 * the file, as necessary, if we extended the file.
		 */
		if (extending) {
#ifdef HAVE_FILESYSTEM_NOTZERO
			if (*pgnoaddr > mfp->last_pgno &&
			    __os_fs_notzero() &&
			    F_ISSET(dbmfp->fhp, DB_FH_VALID))
				ret = __memp_fs_notzero(
				    dbenv, dbmfp, mfp, pgnoaddr);
			else
				ret = 0;
#endif
			if (ret == 0 && *pgnoaddr > mfp->last_pgno)
				mfp->last_pgno = *pgnoaddr;

			R_UNLOCK(dbenv, dbmp->reginfo);
			if (ret != 0)
				goto err;
		}
		goto hb_search;
	case SECOND_FOUND:
		/*
		 * We allocated buffer space for the requested page, but then
		 * found the page in the buffer cache on our second check.
		 * That's OK -- we can use the page we found in the pool,
		 * unless DB_MPOOL_NEW is set.
		 *
		 * Free the allocated memory, we no longer need it.  Since we
		 * can't acquire the region lock while holding the hash bucket
		 * lock, we have to release the hash bucket and re-acquire it.
		 * That's OK, because we have the buffer pinned down.
		 */
		MUTEX_UNLOCK(dbenv, &hp->hash_mutex);
		R_LOCK(dbenv, &dbmp->reginfo[n_cache]);
		__db_shalloc_free(dbmp->reginfo[n_cache].addr, alloc_bhp);
		alloc_bhp = NULL;
		R_UNLOCK(dbenv, &dbmp->reginfo[n_cache]);
		MUTEX_LOCK(dbenv, &hp->hash_mutex);

		/*
		 * We can't use the page we found in the pool if DB_MPOOL_NEW
		 * was set.  (For details, see the above comment beginning
		 * "DB_MPOOL_NEW does not guarantee you a page unreferenced by
		 * any other thread of control".)  If DB_MPOOL_NEW is set, we
		 * release our pin on this particular buffer, and try to get
		 * another one.
		 */
		if (flags == DB_MPOOL_NEW) {
			--bhp->ref;
			b_incr = 0;
			goto alloc;
		}
		break;
	case SECOND_MISS:
		/*
		 * We allocated buffer space for the requested page, and found
		 * the page still missing on our second pass through the buffer
		 * cache.  Instantiate the page.
		 */
		bhp = alloc_bhp;
		alloc_bhp = NULL;

		/*
		 * Initialize all the BH and hash bucket fields so we can call
		 * __memp_bhfree if an error occurs.
		 *
		 * Append the buffer to the tail of the bucket list and update
		 * the hash bucket's priority.
		 */
		b_incr = 1;

		memset(bhp, 0, sizeof(BH));
		bhp->ref = 1;
		bhp->priority = UINT32_T_MAX;
		bhp->pgno = *pgnoaddr;
		bhp->mf_offset = mf_offset;
		SH_TAILQ_INSERT_TAIL(&hp->hash_bucket, bhp, hq);
		hp->hash_priority =
		    SH_TAILQ_FIRST(&hp->hash_bucket, __bh)->priority;

		/* If we extended the file, make sure the page is never lost. */
		if (extending) {
			++hp->hash_page_dirty;
			F_SET(bhp, BH_DIRTY | BH_DIRTY_CREATE);
		}

		/*
		 * If we created the page, zero it out.  If we didn't create
		 * the page, read from the backing file.
		 *
		 * !!!
		 * DB_MPOOL_NEW doesn't call the pgin function.
		 *
		 * If DB_MPOOL_CREATE is used, then the application's pgin
		 * function has to be able to handle pages of 0's -- if it
		 * uses DB_MPOOL_NEW, it can detect all of its page creates,
		 * and not bother.
		 *
		 * If we're running in diagnostic mode, smash any bytes on the
		 * page that are unknown quantities for the caller.
		 *
		 * Otherwise, read the page into memory, optionally creating it
		 * if DB_MPOOL_CREATE is set.
		 */
		if (extending) {
			if (mfp->clear_len == 0)
				memset(bhp->buf, 0, mfp->stat.st_pagesize);
			else {
				memset(bhp->buf, 0, mfp->clear_len);
#if defined(DIAGNOSTIC) || defined(UMRW)
				memset(bhp->buf + mfp->clear_len, CLEAR_BYTE,
				    mfp->stat.st_pagesize - mfp->clear_len);
#endif
			}

			if (flags == DB_MPOOL_CREATE && mfp->ftype != 0)
				F_SET(bhp, BH_CALLPGIN);

			++mfp->stat.st_page_create;
		} else {
			F_SET(bhp, BH_TRASH);
			++mfp->stat.st_cache_miss;
		}

		/* Increment buffer count referenced by MPOOLFILE. */
		MUTEX_LOCK(dbenv, &mfp->mutex);
		++mfp->block_cnt;
		MUTEX_UNLOCK(dbenv, &mfp->mutex);

		/*
		 * Initialize the mutex.  This is the last initialization step,
		 * because it's the only one that can fail, and everything else
		 * must be set up or we can't jump to the err label because it
		 * will call __memp_bhfree.
		 */
		if ((ret = __db_mutex_setup(dbenv,
		    &dbmp->reginfo[n_cache], &bhp->mutex, 0)) != 0)
			goto err;
	}

	DB_ASSERT(bhp->ref != 0);

	/*
	 * If we're the only reference, update buffer and bucket priorities.
	 * We may be about to release the hash bucket lock, and everything
	 * should be correct, first.  (We've already done this if we created
	 * the buffer, so there is no need to do it again.)
	 */
	if (state != SECOND_MISS && bhp->ref == 1) {
		bhp->priority = UINT32_T_MAX;
		SH_TAILQ_REMOVE(&hp->hash_bucket, bhp, hq, __bh);
		SH_TAILQ_INSERT_TAIL(&hp->hash_bucket, bhp, hq);
		hp->hash_priority =
		    SH_TAILQ_FIRST(&hp->hash_bucket, __bh)->priority;
	}

	/*
	 * BH_TRASH --
	 * The buffer we found may need to be filled from the disk.
	 *
	 * It's possible for the read function to fail, which means we fail as
	 * well.  Note, the __memp_pgread() function discards and reacquires
	 * the hash lock, so the buffer must be pinned down so that it cannot
	 * move and its contents are unchanged.  Discard the buffer on failure
	 * unless another thread is waiting on our I/O to complete.  It's OK to
	 * leave the buffer around, as the waiting thread will see the BH_TRASH
	 * flag set, and will also attempt to discard it.  If there's a waiter,
	 * we need to decrement our reference count.
	 */
	if (F_ISSET(bhp, BH_TRASH) &&
	    (ret = __memp_pgread(dbmfp,
	    &hp->hash_mutex, bhp, LF_ISSET(DB_MPOOL_CREATE) ? 1 : 0)) != 0)
		goto err;

	/*
	 * BH_CALLPGIN --
	 * The buffer was processed for being written to disk, and now has
	 * to be re-converted for use.
	 */
	if (F_ISSET(bhp, BH_CALLPGIN)) {
		if ((ret = __memp_pg(dbmfp, bhp, 1)) != 0)
			goto err;
		F_CLR(bhp, BH_CALLPGIN);
	}

	MUTEX_UNLOCK(dbenv, &hp->hash_mutex);

#ifdef DIAGNOSTIC
	/* Update the file's pinned reference count. */
	R_LOCK(dbenv, dbmp->reginfo);
	++dbmfp->pinref;
	R_UNLOCK(dbenv, dbmp->reginfo);

	/*
	 * We want to switch threads as often as possible, and at awkward
	 * times.  Yield every time we get a new page to ensure contention.
	 */
	if (F_ISSET(dbenv, DB_ENV_YIELDCPU))
		__os_yield(dbenv, 1);
#endif

	*(void **)addrp = bhp->buf;
	return (0);

err:	/*
	 * Discard our reference.  If we're the only reference, discard the
	 * the buffer entirely.  If we held a reference to a buffer, we are
	 * also still holding the hash bucket mutex.
	 */
	if (b_incr) {
		if (bhp->ref == 1)
			(void)__memp_bhfree(dbmp, hp, bhp, 1);
		else {
			--bhp->ref;
			MUTEX_UNLOCK(dbenv, &hp->hash_mutex);
		}
	}

	/* If alloc_bhp is set, free the memory. */
	if (alloc_bhp != NULL)
		__db_shalloc_free(dbmp->reginfo[n_cache].addr, alloc_bhp);

	return (ret);
}

#ifdef HAVE_FILESYSTEM_NOTZERO
/*
 * __memp_fs_notzero --
 *	Initialize the underlying allocated pages in the file.
 */
static int
__memp_fs_notzero(dbenv, dbmfp, mfp, pgnoaddr)
	DB_ENV *dbenv;
	DB_MPOOLFILE *dbmfp;
	MPOOLFILE *mfp;
	db_pgno_t *pgnoaddr;
{
	DB_IO db_io;
	u_int32_t i, npages;
	size_t nw;
	int ret;
	u_int8_t *page;
	char *fail;

	/*
	 * Pages allocated by writing pages past end-of-file are not zeroed,
	 * on some systems.  Recovery could theoretically be fooled by a page
	 * showing up that contained garbage.  In order to avoid this, we
	 * have to write the pages out to disk, and flush them.  The reason
	 * for the flush is because if we don't sync, the allocation of another
	 * page subsequent to this one might reach the disk first, and if we
	 * crashed at the right moment, leave us with this page as the one
	 * allocated by writing a page past it in the file.
	 *
	 * Hash is the only access method that allocates groups of pages.  We
	 * know that it will use the existence of the last page in a group to
	 * signify that the entire group is OK; so, write all the pages but
	 * the last one in the group, flush them to disk, and then write the
	 * last one to disk and flush it.
	 */
	if ((ret = __os_calloc(dbenv, 1, mfp->stat.st_pagesize, &page)) != 0)
		return (ret);

	db_io.fhp = dbmfp->fhp;
	db_io.mutexp = dbmfp->mutexp;
	db_io.pagesize = db_io.bytes = mfp->stat.st_pagesize;
	db_io.buf = page;

	npages = *pgnoaddr - mfp->last_pgno;
	for (i = 1; i < npages; ++i) {
		db_io.pgno = mfp->last_pgno + i;
		if ((ret = __os_io(dbenv, &db_io, DB_IO_WRITE, &nw)) != 0) {
			fail = "write";
			goto err;
		}
	}
	if (i != 1 && (ret = __os_fsync(dbenv, dbmfp->fhp)) != 0) {
		fail = "sync";
		goto err;
	}

	db_io.pgno = mfp->last_pgno + npages;
	if ((ret = __os_io(dbenv, &db_io, DB_IO_WRITE, &nw)) != 0) {
		fail = "write";
		goto err;
	}
	if ((ret = __os_fsync(dbenv, dbmfp->fhp)) != 0) {
		fail = "sync";
err:		__db_err(dbenv, "%s: %s failed for page %lu",
		    __memp_fn(dbmfp), fail, (u_long)db_io.pgno);
	}

	__os_free(dbenv, page);
	return (ret);
}
#endif
