/* DO NOT EDIT: automatically built by dist/s_vxworks. */
/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996-2002
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

#ifndef _DB_INTERNAL_H_
#define	_DB_INTERNAL_H_

/*******************************************************
 * System includes, db.h, a few general DB includes.  The DB includes are
 * here because it's OK if db_int.h includes queue structure declarations.
 *******************************************************/
#ifndef NO_SYSTEM_INCLUDES
#if defined(__STDC__) || defined(__cplusplus)
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <errno.h>
#endif

#include "db.h"

#include "dbinc/queue.h"
#include "dbinc/shqueue.h"

#if defined(__cplusplus)
extern "C" {
#endif

/*******************************************************
 * General purpose constants and macros.
 *******************************************************/
#define	UINT16_T_MAX	    0xffff	/* Maximum 16 bit unsigned. */
#define	UINT32_T_MAX	0xffffffff	/* Maximum 32 bit unsigned. */

#define	MEGABYTE	1048576
#define	GIGABYTE	1073741824

#define	MS_PER_SEC	1000		/* Milliseconds in a second. */
#define	USEC_PER_MS	1000		/* Microseconds in a millisecond. */

#define	RECNO_OOB	0		/* Illegal record number. */

/* Test for a power-of-two (tests true for zero, which doesn't matter here). */
#define	POWER_OF_TWO(x)	(((x) & ((x) - 1)) == 0)

/* Test for valid page sizes. */
#define	DB_MIN_PGSIZE	0x000200	/* Minimum page size (512). */
#define	DB_MAX_PGSIZE	0x010000	/* Maximum page size (65536). */
#define	IS_VALID_PAGESIZE(x)						\
	(POWER_OF_TWO(x) && (x) >= DB_MIN_PGSIZE && ((x) <= DB_MAX_PGSIZE))

/* Minimum number of pages cached, by default. */
#define	DB_MINPAGECACHE	16

/*
 * If we are unable to determine the underlying filesystem block size, use
 * 8K on the grounds that most OS's use less than 8K for a VM page size.
 */
#define	DB_DEF_IOSIZE	(8 * 1024)

/*
 * Aligning items to particular sizes or in pages or memory.
 *
 * db_align_t --
 * Largest integral type, used to align structures in memory.  We don't store
 * floating point types in structures, so integral types should be sufficient
 * (and we don't have to worry about systems that store floats in other than
 * power-of-2 numbers of bytes).  Additionally this fixes compiler that rewrite
 * structure assignments and ANSI C memcpy calls to be in-line instructions
 * that happen to require alignment.  Note: this alignment isn't sufficient for
 * mutexes, which depend on things like cache line alignment.  Mutex alignment
 * is handled separately, in mutex.h.
 *
 * db_alignp_t --
 * Integral type that's the same size as a pointer.  There are places where
 * DB modifies pointers by discarding the bottom bits to guarantee alignment.
 * We can't use db_align_t, it may be larger than the pointer, and compilers
 * get upset about that.  So far we haven't run on any machine where there
 * isn't an integral type the same size as a pointer -- here's hoping.
 */
typedef unsigned long db_align_t;
typedef unsigned long db_alignp_t;

/* Align an integer to a specific boundary. */
#undef	ALIGN
#define	ALIGN(v, bound)	(((v) + (bound) - 1) & ~(((db_align_t)bound) - 1))

/*
 * Print an address as a u_long (a u_long is the largest type we can print
 * portably).  Most 64-bit systems have made longs 64-bits, so this should
 * work.
 */
#define	P_TO_ULONG(p)	((u_long)(db_alignp_t)(p))

/*
 * Convert a pointer to a small integral value.
 *
 * The (u_int16_t)(db_alignp_t) cast avoids warnings: the (db_alignp_t) cast
 * converts the value to an integral type, and the (u_int16_t) cast converts
 * it to a small integral type so we don't get complaints when we assign the
 * final result to an integral type smaller than db_alignp_t.
 */
#define	P_TO_UINT32(p)	((u_int32_t)(db_alignp_t)(p))
#define	P_TO_UINT16(p)	((u_int16_t)(db_alignp_t)(p))

/*
 * There are several on-page structures that are declared to have a number of
 * fields followed by a variable length array of items.  The structure size
 * without including the variable length array or the address of the first of
 * those elements can be found using SSZ.
 *
 * This macro can also be used to find the offset of a structure element in a
 * structure.  This is used in various places to copy structure elements from
 * unaligned memory references, e.g., pointers into a packed page.
 *
 * There are two versions because compilers object if you take the address of
 * an array.
 */
#undef	SSZ
#define	SSZ(name, field)  P_TO_UINT16(&(((name *)0)->field))

#undef	SSZA
#define	SSZA(name, field) P_TO_UINT16(&(((name *)0)->field[0]))

/* Structure used to print flag values. */
typedef struct __fn {
	u_int32_t mask;			/* Flag value. */
	const char *name;		/* Flag name. */
} FN;

/* Set, clear and test flags. */
#define	FLD_CLR(fld, f)		(fld) &= ~(f)
#define	FLD_ISSET(fld, f)	((fld) & (f))
#define	FLD_SET(fld, f)		(fld) |= (f)
#define	F_CLR(p, f)		(p)->flags &= ~(f)
#define	F_ISSET(p, f)		((p)->flags & (f))
#define	F_SET(p, f)		(p)->flags |= (f)
#define	LF_CLR(f)		((flags) &= ~(f))
#define	LF_ISSET(f)		((flags) & (f))
#define	LF_SET(f)		((flags) |= (f))

/* Display separator string. */
#undef	DB_LINE
#define	DB_LINE "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="

/* Unused, or not-used-yet variable.  "Shut that bloody compiler up!" */
#define	COMPQUIET(n, v)	(n) = (v)

/*******************************************************
 * API return values
 *******************************************************/
 /*
  * Return values that are OK for each different call.  Most calls have
  * a standard 'return of 0 is only OK value', but some, like db->get
  * have DB_NOTFOUND as a return value, but it really isn't an error.
  */
#define	DB_RETOK_STD(ret)	((ret) == 0)
#define	DB_RETOK_DBCDEL(ret)	((ret) == 0 || (ret) == DB_KEYEMPTY || \
				    (ret) == DB_NOTFOUND)
#define	DB_RETOK_DBCGET(ret)	DB_RETOK_DBGET(ret)
#define	DB_RETOK_DBCPUT(ret)	((ret) == 0 || (ret) == DB_KEYEXIST || \
				    (ret) == DB_NOTFOUND)
#define	DB_RETOK_DBDEL(ret)	((ret) == 0 || (ret) == DB_NOTFOUND)
#define	DB_RETOK_DBGET(ret)	((ret) == 0 || (ret) == DB_KEYEMPTY || \
				    (ret) == DB_NOTFOUND)
#define	DB_RETOK_DBPUT(ret)	((ret) == 0 || (ret) == DB_KEYEXIST)
#define	DB_RETOK_LGGET(ret)	((ret) == 0 || (ret) == DB_NOTFOUND)
#define	DB_RETOK_MPGET(ret)	((ret) == 0 || (ret) == DB_PAGE_NOTFOUND)
#define	DB_RETOK_REPPMSG(ret)	((ret) == 0 || (ret) == DB_REP_NEWMASTER || \
				    (ret) == DB_REP_NEWSITE)

/*******************************************************
 * Files.
 *******************************************************/
 /*
  * We use 1024 as the maximum path length.  It's too hard to figure out what
  * the real path length is, as it was traditionally stored in <sys/param.h>,
  * and that file isn't always available.
  */
#undef	MAXPATHLEN
#define	MAXPATHLEN	1024

#define	PATH_DOT	"."	/* Current working directory. */
#define	PATH_SEPARATOR	"/\\"	/* Path separator character(s). */

/*
 * Flags understood by __os_open.
 */
#define	DB_OSO_CREATE	0x0001		/* POSIX: O_CREAT */
#define	DB_OSO_DIRECT	0x0002		/* Don't buffer the file in the OS. */
#define	DB_OSO_EXCL	0x0004		/* POSIX: O_EXCL */
#define	DB_OSO_LOG	0x0008		/* Opening a log file. */
#define	DB_OSO_RDONLY	0x0010		/* POSIX: O_RDONLY */
#define	DB_OSO_REGION	0x0020		/* Opening a region file. */
#define	DB_OSO_SEQ	0x0040		/* Expected sequential access. */
#define	DB_OSO_TEMP	0x0080		/* Remove after last close. */
#define	DB_OSO_TRUNC	0x0100		/* POSIX: O_TRUNC */

/*
 * Seek options understood by __os_seek.
 */
typedef enum {
	DB_OS_SEEK_CUR,			/* POSIX: SEEK_CUR */
	DB_OS_SEEK_END,			/* POSIX: SEEK_END */
	DB_OS_SEEK_SET			/* POSIX: SEEK_SET */
} DB_OS_SEEK;

/*******************************************************
 * Environment.
 *******************************************************/
/* Type passed to __db_appname(). */
typedef enum {
	DB_APP_NONE=0,			/* No type (region). */
	DB_APP_DATA,			/* Data file. */
	DB_APP_LOG,			/* Log file. */
	DB_APP_TMP			/* Temporary file. */
} APPNAME;

/*
 * CDB_LOCKING	CDB product locking.
 * CRYPTO_ON	Security has been configured.
 * LOCKING_ON	Locking has been configured.
 * LOGGING_ON	Logging has been configured.
 * MPOOL_ON	Memory pool has been configured.
 * RPC_ON	RPC has been configured.
 * TXN_ON	Transactions have been configured.
 */
#define	CDB_LOCKING(dbenv)	F_ISSET(dbenv, DB_ENV_CDB)
#define	CRYPTO_ON(dbenv)	((dbenv)->crypto_handle != NULL)
#define	LOCKING_ON(dbenv)	((dbenv)->lk_handle != NULL)
#define	LOGGING_ON(dbenv)	((dbenv)->lg_handle != NULL)
#define	MPOOL_ON(dbenv)		((dbenv)->mp_handle != NULL)
#define	RPC_ON(dbenv)		((dbenv)->cl_handle != NULL)
#define	TXN_ON(dbenv)		((dbenv)->tx_handle != NULL)

/*
 * STD_LOCKING	Standard locking, that is, locking was configured and CDB
 *		was not.  We do not do locking in off-page duplicate trees,
 *		so we check for that in the cursor first.
 */
#define	STD_LOCKING(dbc)						\
	(!F_ISSET(dbc, DBC_OPD) &&					\
	    !CDB_LOCKING((dbc)->dbp->dbenv) && LOCKING_ON((dbc)->dbp->dbenv))

/*
 * IS_RECOVERING: The system is running recovery.
 */
#define	IS_RECOVERING(dbenv)						\
	(LOGGING_ON(dbenv) &&						\
	    F_ISSET((DB_LOG *)(dbenv)->lg_handle, DBLOG_RECOVER))

/* Initialization methods are often illegal before/after open is called. */
#define	ENV_ILLEGAL_AFTER_OPEN(dbenv, name)				\
	if (F_ISSET((dbenv), DB_ENV_OPEN_CALLED))			\
		return (__db_mi_open(dbenv, name, 1));
#define	ENV_ILLEGAL_BEFORE_OPEN(dbenv, name)				\
	if (!F_ISSET((dbenv), DB_ENV_OPEN_CALLED))			\
		return (__db_mi_open(dbenv, name, 0));

/* We're not actually user hostile, honest. */
#define	ENV_REQUIRES_CONFIG(dbenv, handle, i, flags)			\
	if (handle == NULL)						\
		return (__db_env_config(dbenv, i, flags));

/*******************************************************
 * Database Access Methods.
 *******************************************************/
/*
 * DB_IS_THREADED --
 *	The database handle is free-threaded (was opened with DB_THREAD).
 */
#define	DB_IS_THREADED(dbp)						\
	((dbp)->mutexp != NULL)

/* Initialization methods are often illegal before/after open is called. */
#define	DB_ILLEGAL_AFTER_OPEN(dbp, name)				\
	if (F_ISSET((dbp), DB_AM_OPEN_CALLED))				\
		return (__db_mi_open((dbp)->dbenv, name, 1));
#define	DB_ILLEGAL_BEFORE_OPEN(dbp, name)				\
	if (!F_ISSET((dbp), DB_AM_OPEN_CALLED))				\
		return (__db_mi_open((dbp)->dbenv, name, 0));
/* Some initialization methods are illegal if environment isn't local. */
#define	DB_ILLEGAL_IN_ENV(dbp, name)					\
	if (!F_ISSET((dbp)->dbenv, DB_ENV_DBLOCAL))			\
		return (__db_mi_env((dbp)->dbenv, name));
#define	DB_ILLEGAL_METHOD(dbp, flags) {					\
	int __ret;							\
	if ((__ret = __dbh_am_chk(dbp, flags)) != 0)			\
		return (__ret);						\
}

/*
 * Common DBC->internal fields.  Each access method adds additional fields
 * to this list, but the initial fields are common.
 */
#define	__DBC_INTERNAL							\
	DBC	 *opd;			/* Off-page duplicate cursor. */\
									\
	void	 *page;			/* Referenced page. */		\
	db_pgno_t root;			/* Tree root. */		\
	db_pgno_t pgno;			/* Referenced page number. */	\
	db_indx_t indx;			/* Referenced key item index. */\
									\
	DB_LOCK		lock;		/* Cursor lock. */		\
	db_lockmode_t	lock_mode;	/* Lock mode. */

struct __dbc_internal {
	__DBC_INTERNAL
};

/* Actions that __db_master_update can take. */
typedef enum { MU_REMOVE, MU_RENAME, MU_OPEN } mu_action;

/*
 * Access-method-common macro for determining whether a cursor
 * has been initialized.
 */
#define	IS_INITIALIZED(dbc)	((dbc)->internal->pgno != PGNO_INVALID)

/* Free the callback-allocated buffer, if necessary, hanging off of a DBT. */
#define	FREE_IF_NEEDED(sdbp, dbt)					\
	if (F_ISSET((dbt), DB_DBT_APPMALLOC)) {				\
		__os_ufree((sdbp)->dbenv, (dbt)->data);			\
		F_CLR((dbt), DB_DBT_APPMALLOC);				\
	}

/*
 * Use memory belonging to object "owner" to return the results of
 * any no-DBT-flag get ops on cursor "dbc".
 */
#define	SET_RET_MEM(dbc, owner)				\
	do {						\
		(dbc)->rskey = &(owner)->my_rskey;	\
		(dbc)->rkey = &(owner)->my_rkey;	\
		(dbc)->rdata = &(owner)->my_rdata;	\
	} while (0)

/* Use the return-data memory src is currently set to use in dest as well. */
#define	COPY_RET_MEM(src, dest)				\
	do {						\
		(dest)->rskey = (src)->rskey;		\
		(dest)->rkey = (src)->rkey;		\
		(dest)->rdata = (src)->rdata;		\
	} while (0)

/* Reset the returned-memory pointers to their defaults. */
#define	RESET_RET_MEM(dbc)				\
	do {						\
		(dbc)->rskey = &(dbc)->my_rskey;	\
		(dbc)->rkey = &(dbc)->my_rkey;		\
		(dbc)->rdata = &(dbc)->my_rdata;	\
	} while (0)

/*******************************************************
 * Mpool.
 *******************************************************/
/*
 * File types for DB access methods.  Negative numbers are reserved to DB.
 */
#define	DB_FTYPE_SET		-1	/* Call pgin/pgout functions. */
#define	DB_FTYPE_NOTSET		 0	/* Don't call... */

/* Structure used as the DB pgin/pgout pgcookie. */
typedef struct __dbpginfo {
	size_t	db_pagesize;		/* Underlying page size. */
	u_int32_t flags;		/* Some DB_AM flags needed. */
	DBTYPE  type;			/* DB type */
} DB_PGINFO;

/*******************************************************
 * Log.
 *******************************************************/
/* Initialize an LSN to 'zero'. */
#define	ZERO_LSN(LSN) do {						\
	(LSN).file = 0;							\
	(LSN).offset = 0;						\
} while (0)
#define	IS_ZERO_LSN(LSN)	((LSN).file == 0)

#define	IS_INIT_LSN(LSN)	((LSN).file == 1 && (LSN).offset == 0)
#define	INIT_LSN(LSN)		do {					\
	(LSN).file = 1;							\
	(LSN).offset = 0;						\
} while (0)

#define	MAX_LSN(LSN) do {						\
	(LSN).file = UINT32_T_MAX;					\
	(LSN).offset = UINT32_T_MAX;					\
} while (0)
#define	IS_MAX_LSN(LSN) \
	((LSN).file == UINT32_T_MAX && (LSN).offset == UINT32_T_MAX)

/* If logging is turned off, smash the lsn. */
#define	LSN_NOT_LOGGED(LSN) do {					\
	(LSN).file = 0;							\
	(LSN).offset = 1;						\
} while (0)
#define	IS_NOT_LOGGED_LSN(LSN) \
	((LSN).file == 0 && (LSN).offset == 1)

/*
 * Test if the environment is currently logging changes.  If we're in
 * recovery or we're a replication client, we don't need to log changes
 * because they're already in the log, even though we have a fully functional
 * log system.
 */
#define	DBENV_LOGGING(dbenv)						\
	(LOGGING_ON(dbenv) && !F_ISSET((dbenv), DB_ENV_REP_CLIENT) &&	\
	    (!IS_RECOVERING(dbenv)))

/*
 * Test if we need to log a change.  Note that the DBC_RECOVER flag is set
 * when we're in abort, as well as during recovery;  thus DBC_LOGGING may be
 * false for a particular dbc even when DBENV_LOGGING is true.
 *
 * We explicitly use LOGGING_ON/DB_ENV_REP_CLIENT here because we don't
 * want to have to pull in the log headers, which IS_RECOVERING (and thus
 * DBENV_LOGGING) rely on, and because DBC_RECOVER should be set anytime
 * IS_RECOVERING would be true.
 */
#define	DBC_LOGGING(dbc)						\
	(LOGGING_ON((dbc)->dbp->dbenv) && !F_ISSET((dbc), DBC_RECOVER) && \
	 !F_ISSET((dbc)->dbp->dbenv, DB_ENV_REP_CLIENT))

/*******************************************************
 * Txn.
 *******************************************************/
#define	DB_NONBLOCK(C)	((C)->txn != NULL && F_ISSET((C)->txn, TXN_NOWAIT))
#define	IS_SUBTRANSACTION(txn)						\
	((txn) != NULL && (txn)->parent != NULL)

/*******************************************************
 * Crypto.
 *******************************************************/
#define	DB_IV_BYTES     16		/* Bytes per IV */
#define	DB_MAC_KEY	20		/* Bytes per MAC checksum */

/*******************************************************
 * Forward structure declarations.
 *******************************************************/
struct __db_reginfo_t;	typedef struct __db_reginfo_t REGINFO;
struct __db_txnhead;	typedef struct __db_txnhead DB_TXNHEAD;
struct __db_txnlist;	typedef struct __db_txnlist DB_TXNLIST;
struct __vrfy_childinfo; typedef struct __vrfy_childinfo VRFY_CHILDINFO;
struct __vrfy_dbinfo;   typedef struct __vrfy_dbinfo VRFY_DBINFO;
struct __vrfy_pageinfo; typedef struct __vrfy_pageinfo VRFY_PAGEINFO;

#if defined(__cplusplus)
}
#endif

/*******************************************************
 * Remaining general DB includes.
 *******************************************************/


#include "dbinc/globals.h"
#include "dbinc/debug.h"
#include "dbinc/mutex.h"
#include "dbinc/region.h"
#include "dbinc_auto/mutex_ext.h"	/* XXX: Include after region.h. */
#include "dbinc_auto/env_ext.h"
#include "dbinc/os.h"
#include "dbinc_auto/clib_ext.h"
#include "dbinc_auto/common_ext.h"

#endif /* !_DB_INTERNAL_H_ */
