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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"
#include "dbinc/db_shash.h"
#include "dbinc/lock.h"
#include "dbinc/log.h"
#include "dbinc/txn.h"

/*
 * __db_fchk --
 *	General flags checking routine.
 *
 * PUBLIC: int __db_fchk __P((DB_ENV *, const char *, u_int32_t, u_int32_t));
 */
int
__db_fchk(dbenv, name, flags, ok_flags)
	DB_ENV *dbenv;
	const char *name;
	u_int32_t flags, ok_flags;
{
	return (LF_ISSET(~ok_flags) ? __db_ferr(dbenv, name, 0) : 0);
}

/*
 * __db_fcchk --
 *	General combination flags checking routine.
 *
 * PUBLIC: int __db_fcchk
 * PUBLIC:    __P((DB_ENV *, const char *, u_int32_t, u_int32_t, u_int32_t));
 */
int
__db_fcchk(dbenv, name, flags, flag1, flag2)
	DB_ENV *dbenv;
	const char *name;
	u_int32_t flags, flag1, flag2;
{
	return (LF_ISSET(flag1) &&
	    LF_ISSET(flag2) ? __db_ferr(dbenv, name, 1) : 0);
}

/*
 * __db_ferr --
 *	Common flag errors.
 *
 * PUBLIC: int __db_ferr __P((const DB_ENV *, const char *, int));
 */
int
__db_ferr(dbenv, name, iscombo)
	const DB_ENV *dbenv;
	const char *name;
	int iscombo;
{
	__db_err(dbenv, "illegal flag %sspecified to %s",
	    iscombo ? "combination " : "", name);
	return (EINVAL);
}

/*
 * __db_pgerr --
 *	Error when unable to retrieve a specified page.
 *
 * PUBLIC: void __db_pgerr __P((DB *, db_pgno_t, int));
 */
void
__db_pgerr(dbp, pgno, errval)
	DB *dbp;
	db_pgno_t pgno;
	int errval;
{
	/*
	 * Three things are certain:
	 * Death, taxes, and lost data.
	 * Guess which has occurred.
	 */
	__db_err(dbp->dbenv,
	    "unable to create/retrieve page %lu", (u_long)pgno);
	(void)__db_panic(dbp->dbenv, errval);
}

/*
 * __db_pgfmt --
 *	Error when a page has the wrong format.
 *
 * PUBLIC: int __db_pgfmt __P((DB_ENV *, db_pgno_t));
 */
int
__db_pgfmt(dbenv, pgno)
	DB_ENV *dbenv;
	db_pgno_t pgno;
{
	__db_err(dbenv, "page %lu: illegal page type or format", (u_long)pgno);
	return (__db_panic(dbenv, EINVAL));
}

/*
 * __db_eopnotsup --
 *	Common operation not supported message.
 *
 * PUBLIC: int __db_eopnotsup __P((const DB_ENV *));
 */
int
__db_eopnotsup(dbenv)
	const DB_ENV *dbenv;
{
	__db_err(dbenv, "operation not supported");
#ifdef EOPNOTSUPP
	return (EOPNOTSUPP);
#else
	return (EINVAL);
#endif
}

#ifdef DIAGNOSTIC
/*
 * __db_assert --
 *	Error when an assertion fails.  Only checked if #DIAGNOSTIC defined.
 *
 * PUBLIC: #ifdef DIAGNOSTIC
 * PUBLIC: void __db_assert __P((const char *, const char *, int));
 * PUBLIC: #endif
 */
void
__db_assert(failedexpr, file, line)
	const char *failedexpr, *file;
	int line;
{
	(void)fprintf(stderr,
	    "__db_assert: \"%s\" failed: file \"%s\", line %d\n",
	    failedexpr, file, line);
	(void)fflush(stderr);

	/* We want a stack trace of how this could possibly happen. */
	abort();

	/* NOTREACHED */
}
#endif

/*
 * __db_panic_msg --
 *	Just report that someone else paniced.
 *
 * PUBLIC: int __db_panic_msg __P((DB_ENV *));
 */
int
__db_panic_msg(dbenv)
	DB_ENV *dbenv;
{
	__db_err(dbenv, "fatal region error detected; run recovery");
	return (DB_RUNRECOVERY);
}

/*
 * __db_panic --
 *	Lock out the tree due to unrecoverable error.
 *
 * PUBLIC: int __db_panic __P((DB_ENV *, int));
 */
int
__db_panic(dbenv, errval)
	DB_ENV *dbenv;
	int errval;
{
	if (dbenv != NULL) {
		PANIC_SET(dbenv, 1);

		dbenv->panic_errval = errval;

		__db_err(dbenv, "PANIC: %s", db_strerror(errval));

		if (dbenv->db_paniccall != NULL)
			dbenv->db_paniccall(dbenv, errval);
	}

#if defined(DIAGNOSTIC) && !defined(CONFIG_TEST)
	/*
	 * We want a stack trace of how this could possibly happen.
	 *
	 * Don't drop core if it's the test suite -- it's reasonable for the
	 * test suite to check to make sure that DB_RUNRECOVERY is returned
	 * under certain conditions.
	 */
	abort();
#endif

	/*
	 * Chaos reigns within.
	 * Reflect, repent, and reboot.
	 * Order shall return.
	 */
	return (DB_RUNRECOVERY);
}

/*
 * db_strerror --
 *	ANSI C strerror(3) for DB.
 *
 * EXTERN: char *db_strerror __P((int));
 */
char *
db_strerror(error)
	int error;
{
	if (error == 0)
		return ("Successful return: 0");
	if (error > 0)
		return (strerror(error));

	/*
	 * !!!
	 * The Tcl API requires that some of these return strings be compared
	 * against strings stored in application scripts.  So, any of these
	 * errors that do not invariably result in a Tcl exception may not be
	 * altered.
	 */
	switch (error) {
	case DB_DONOTINDEX:
		return ("DB_DONOTINDEX: Secondary index callback returns null");
	case DB_KEYEMPTY:
		return ("DB_KEYEMPTY: Non-existent key/data pair");
	case DB_KEYEXIST:
		return ("DB_KEYEXIST: Key/data pair already exists");
	case DB_LOCK_DEADLOCK:
		return
		    ("DB_LOCK_DEADLOCK: Locker killed to resolve a deadlock");
	case DB_LOCK_NOTGRANTED:
		return ("DB_LOCK_NOTGRANTED: Lock not granted");
	case DB_NOSERVER:
		return ("DB_NOSERVER: Fatal error, no server");
	case DB_NOSERVER_HOME:
		return ("DB_NOSERVER_HOME: Home unrecognized at server");
	case DB_NOSERVER_ID:
		return ("DB_NOSERVER_ID: Identifier unrecognized at server");
	case DB_NOTFOUND:
		return ("DB_NOTFOUND: No matching key/data pair found");
	case DB_OLD_VERSION:
		return ("DB_OLDVERSION: Database requires a version upgrade");
	case DB_PAGE_NOTFOUND:
		return ("DB_PAGE_NOTFOUND: Requested page not found");
	case DB_REP_DUPMASTER:
		return ("DB_REP_DUPMASTER: A second master site appeared");
	case DB_REP_HOLDELECTION:
		return ("DB_REP_HOLDELECTION: Need to hold an election");
	case DB_REP_NEWMASTER:
		return ("DB_REP_NEWMASTER: A new master has declared itself");
	case DB_REP_NEWSITE:
		return ("DB_REP_NEWSITE: A new site has entered the system");
	case DB_REP_OUTDATED:
		return
		    ("DB_REP_OUTDATED: Insufficient logs on master to recover");
	case DB_REP_UNAVAIL:
		return ("DB_REP_UNAVAIL: Unable to elect a master");
	case DB_RUNRECOVERY:
		return ("DB_RUNRECOVERY: Fatal error, run database recovery");
	case DB_SECONDARY_BAD:
		return
	    ("DB_SECONDARY_BAD: Secondary index item missing from primary");
	case DB_VERIFY_BAD:
		return ("DB_VERIFY_BAD: Database verification failed");
	default: {
		/*
		 * !!!
		 * Room for a 64-bit number + slop.  This buffer is only used
		 * if we're given an unknown error, which should never happen.
		 * Note, however, we're no longer thread-safe if it does.
		 */
		static char ebuf[40];

		(void)snprintf(ebuf, sizeof(ebuf), "Unknown error: %d", error);
		return (ebuf);
	}
	}
}

/*
 * __db_err --
 *	Standard DB error routine.  The same as errx, except we don't write
 *	to stderr if no output mechanism was specified.
 *
 * PUBLIC: void __db_err __P((const DB_ENV *, const char *, ...));
 */
void
#ifdef __STDC__
__db_err(const DB_ENV *dbenv, const char *fmt, ...)
#else
__db_err(dbenv, fmt, va_alist)
	const DB_ENV *dbenv;
	const char *fmt;
	va_dcl
#endif
{
	DB_REAL_ERR(dbenv, 0, 0, 0, fmt);
}

/*
 * __db_errcall --
 *	Do the error message work for callback functions.
 *
 * PUBLIC: void __db_errcall
 * PUBLIC:          __P((const DB_ENV *, int, int, const char *, va_list));
 */
void
__db_errcall(dbenv, error, error_set, fmt, ap)
	const DB_ENV *dbenv;
	int error, error_set;
	const char *fmt;
	va_list ap;
{
	char *p;
	char errbuf[2048];	/* !!!: END OF THE STACK DON'T TRUST SPRINTF. */

	p = errbuf;
	if (fmt != NULL)
		p += vsnprintf(errbuf, sizeof(errbuf), fmt, ap);
	if (error_set)
		p += snprintf(p,
		    sizeof(errbuf) - (p - errbuf), ": %s", db_strerror(error));
	/*
	 * !!!
	 * We're potentially manipulating strings handed us by the application,
	 * and on systems without a real snprintf() the sprintf() calls could
	 * have overflowed the buffer.  We can't do anything about it now, but
	 * we don't want to return control to the application, we might have
	 * overwritten the stack with a Trojan horse.  We're not trying to do
	 * anything recoverable here because systems without snprintf support
	 * are pretty rare anymore.
	 */
	if ((size_t)(p - errbuf) > sizeof(errbuf)) {
		(void)fprintf(stderr,
		    "Berkeley DB: error callback interface buffer overflow\n");
		(void)fflush(stderr);

		abort();
		/* NOTREACHED */
	}

	dbenv->db_errcall(dbenv->db_errpfx, errbuf);
}

/*
 * __db_errfile --
 *	Do the error message work for FILE *s.
 *
 * PUBLIC: void __db_errfile
 * PUBLIC:          __P((const DB_ENV *, int, int, const char *, va_list));
 */
void
__db_errfile(dbenv, error, error_set, fmt, ap)
	const DB_ENV *dbenv;
	int error, error_set;
	const char *fmt;
	va_list ap;
{
	FILE *fp;

	fp = dbenv == NULL ||
	    dbenv->db_errfile == NULL ? stderr : dbenv->db_errfile;

	if (dbenv != NULL && dbenv->db_errpfx != NULL)
		(void)fprintf(fp, "%s: ", dbenv->db_errpfx);
	if (fmt != NULL) {
		(void)vfprintf(fp, fmt, ap);
		if (error_set)
			(void)fprintf(fp, ": ");
	}
	if (error_set)
		(void)fprintf(fp, "%s", db_strerror(error));
	(void)fprintf(fp, "\n");
	(void)fflush(fp);
}

/*
 * __db_logmsg --
 *	Write information into the DB log.
 *
 * PUBLIC: void __db_logmsg __P((const DB_ENV *,
 * PUBLIC:     DB_TXN *, const char *, u_int32_t, const char *, ...));
 */
void
#ifdef __STDC__
__db_logmsg(const DB_ENV *dbenv,
    DB_TXN *txnid, const char *opname, u_int32_t flags, const char *fmt, ...)
#else
__db_logmsg(dbenv, txnid, opname, flags, fmt, va_alist)
	const DB_ENV *dbenv;
	DB_TXN *txnid;
	const char *opname, *fmt;
	u_int32_t flags;
	va_dcl
#endif
{
	DBT opdbt, msgdbt;
	DB_LSN lsn;
	va_list ap;
	char __logbuf[2048];	/* !!!: END OF THE STACK DON'T TRUST SPRINTF. */

	if (!LOGGING_ON(dbenv))
		return;

#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	memset(&opdbt, 0, sizeof(opdbt));
	opdbt.data = (void *)opname;
	opdbt.size = (u_int32_t)(strlen(opname) + 1);

	memset(&msgdbt, 0, sizeof(msgdbt));
	msgdbt.data = __logbuf;
	msgdbt.size = vsnprintf(__logbuf, sizeof(__logbuf), fmt, ap);

	/*
	 * XXX
	 * Explicitly discard the const.  Otherwise, we have to const DB_ENV
	 * references throughout the logging subsystem.
	 */
	__db_debug_log(
	    (DB_ENV *)dbenv, txnid, &lsn, flags, &opdbt, -1, &msgdbt, NULL, 0);

	va_end(ap);
}

/*
 * __db_unknown_flag -- report internal error
 *
 * PUBLIC: int __db_unknown_flag __P((DB_ENV *, char *, u_int32_t));
 */
int
__db_unknown_flag(dbenv, routine, flag)
	DB_ENV *dbenv;
	char *routine;
	u_int32_t flag;
{
	__db_err(dbenv, "%s: Unknown flag: 0x%x", routine, flag);
	DB_ASSERT(0);
	return (EINVAL);
}

/*
 * __db_unknown_type -- report internal error
 *
 * PUBLIC: int __db_unknown_type __P((DB_ENV *, char *, DBTYPE));
 */
int
__db_unknown_type(dbenv, routine, type)
	DB_ENV *dbenv;
	char *routine;
	DBTYPE type;
{
	__db_err(dbenv, "%s: Unknown db type: 0x%x", routine, type);
	DB_ASSERT(0);
	return (EINVAL);
}

/*
 * __db_check_txn --
 *	Check for common transaction errors.
 *
 * PUBLIC: int __db_check_txn __P((DB *, DB_TXN *, u_int32_t, int));
 */
int
__db_check_txn(dbp, txn, assoc_lid, read_op)
	DB *dbp;
	DB_TXN *txn;
	u_int32_t assoc_lid;
	int read_op;
{
	DB_ENV *dbenv;

	dbenv = dbp->dbenv;

	/*
	 * If we are in recovery or aborting a transaction, then we
	 * don't need to enforce the rules about dbp's not allowing
	 * transactional operations in non-transactional dbps and
	 * vica-versa.  This happens all the time as the dbp during
	 * an abort may be transactional, but we undo operations
	 * outside a transaction since we're aborting.
	 */
	if (IS_RECOVERING(dbenv) || F_ISSET(dbp, DB_AM_RECOVER))
		return (0);

	/*
	 * Check for common transaction errors:
	 *	Failure to pass a transaction handle to a DB operation
	 *	Failure to configure the DB handle in a proper environment
	 *	Operation on a handle whose open commit hasn't completed.
	 *
	 * Read operations don't require a txn even if we've used one before
	 * with this handle, although if they do have a txn, we'd better be
	 * prepared for it.
	 */
	if (txn == NULL) {
		if (!read_op && F_ISSET(dbp, DB_AM_TXN)) {
			__db_err(dbenv,
    "DB handle previously used in transaction, missing transaction handle");
			return (EINVAL);
		}

		if (dbp->cur_lid >= TXN_MINIMUM)
			goto open_err;
	} else {
		if (dbp->cur_lid >= TXN_MINIMUM && dbp->cur_lid != txn->txnid)
			goto open_err;

		if (!TXN_ON(dbenv))
			 return (__db_not_txn_env(dbenv));

		if (!F_ISSET(dbp, DB_AM_TXN)) {
			__db_err(dbenv,
    "Transaction specified for a DB handle opened outside a transaction");
			return (EINVAL);
		}
	}

	/*
	 * If dbp->associate_lid is not DB_LOCK_INVALIDID, that means we're in
	 * the middle of a DB->associate with DB_CREATE (i.e., a secondary index
	 * creation).
	 *
	 * In addition to the usual transaction rules, we need to lock out
	 * non-transactional updates that aren't part of the associate (and
	 * thus are using some other locker ID).
	 *
	 * Transactional updates should simply block;  from the time we
	 * decide to build the secondary until commit, we'll hold a write
	 * lock on all of its pages, so it should be safe to attempt to update
	 * the secondary in another transaction (presumably by updating the
	 * primary).
	 */
	if (!read_op && dbp->associate_lid != DB_LOCK_INVALIDID &&
	    txn != NULL && dbp->associate_lid != assoc_lid) {
		__db_err(dbenv,
	    "Operation forbidden while secondary index is being created");
		return (EINVAL);
	}

	return (0);
open_err:
	__db_err(dbenv,
	    "Transaction that opened the DB handle is still active");
	return (EINVAL);
}

/*
 * __db_not_txn_env --
 *	DB handle must be in an environment that supports transactions.
 *
 * PUBLIC: int __db_not_txn_env __P((DB_ENV *));
 */
int
__db_not_txn_env(dbenv)
	DB_ENV *dbenv;
{
	__db_err(dbenv, "DB environment not configured for transactions");
	return (EINVAL);
}
