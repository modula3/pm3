/* Do not edit: automatically built by gen_rec.awk. */
#include "db_config.h"

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <ctype.h>
#include <string.h>
#endif

#include "db_int.h"
#include "dbinc/crypto.h"
#include "dbinc/db_page.h"
#include "dbinc/db_dispatch.h"
#include "dbinc/db_am.h"
#include "dbinc/log.h"
#include "dbinc/rep.h"
#include "dbinc/txn.h"

/*
 * PUBLIC: int __crdel_metasub_log __P((DB *, DB_TXN *, DB_LSN *,
 * PUBLIC:     u_int32_t, db_pgno_t, const DBT *, DB_LSN *));
 */
int
__crdel_metasub_log(dbp, txnid, ret_lsnp, flags, pgno, page, lsn)
	DB *dbp;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	db_pgno_t pgno;
	const DBT *page;
	DB_LSN * lsn;
{
	DBT logrec;
	DB_ENV *dbenv;
	DB_LSN *lsnp, null_lsn;
	u_int32_t zero;
	u_int32_t uinttmp;
	u_int32_t npad, rectype, txn_num;
	int ret;
	u_int8_t *bp;

	dbenv = dbp->dbenv;
	rectype = DB___crdel_metasub;
	npad = 0;

	if (txnid == NULL) {
		txn_num = 0;
		null_lsn.file = 0;
		null_lsn.offset = 0;
		lsnp = &null_lsn;
	} else {
		if (TAILQ_FIRST(&txnid->kids) != NULL &&
		    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
			return (ret);
		txn_num = txnid->txnid;
		lsnp = &txnid->last_lsn;
	}

	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t) + (page == NULL ? 0 : page->size)
	    + sizeof(*lsn);
	if (CRYPTO_ON(dbenv)) {
		npad =
		    ((DB_CIPHER *)dbenv->crypto_handle)->adj_size(logrec.size);
		logrec.size += npad;
	}

	if ((ret = __os_malloc(dbenv,
	    logrec.size, &logrec.data)) != 0)
		return (ret);

	if (npad > 0)
		memset((u_int8_t *)logrec.data + logrec.size - npad, 0, npad);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	DB_ASSERT(dbp->log_filename != NULL);
	if (dbp->log_filename->id == DB_LOGFILEID_INVALID &&
	    (ret = __dbreg_lazy_id(dbp)) != 0)
		return (ret);

	uinttmp = (u_int32_t)dbp->log_filename->id;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)pgno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (page == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &page->size, sizeof(page->size));
		bp += sizeof(page->size);
		memcpy(bp, page->data, page->size);
		bp += page->size;
	}

	if (lsn != NULL)
		memcpy(bp, lsn, sizeof(*lsn));
	else
		memset(bp, 0, sizeof(*lsn));
	bp += sizeof(*lsn);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) <= logrec.size);
	ret = dbenv->log_put(dbenv,
	   ret_lsnp, (DBT *)&logrec, flags | DB_NOCOPY);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__crdel_metasub_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data);
	return (ret);
}

/*
 * PUBLIC: int __crdel_metasub_getpgnos __P((DB_ENV *, DBT *,
 * PUBLIC:     DB_LSN *, db_recops, void *));
 */
int
__crdel_metasub_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	DB *dbp;
	TXN_RECS *t;
	__crdel_metasub_args *argp;
	u_int32_t ret;

	COMPQUIET(notused1, DB_TXN_ABORT);

	argp = NULL;
	t = (TXN_RECS *)summary;

	if ((ret = __crdel_metasub_read(dbenv, rec->data, &argp)) != 0)
		return (ret);

	if ((ret = __dbreg_id_to_db(dbenv,
	    argp->txnid, &dbp, argp->fileid, 0)) != 0)
		goto err;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		goto err;

	t->array[t->npages].flags = 0;
	t->array[t->npages].fid = argp->fileid;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].pgdesc.pgno = argp->pgno;
	t->array[t->npages].pgdesc.type = DB_PAGE_LOCK;
	memcpy(t->array[t->npages].pgdesc.fileid, dbp->fileid,
	    DB_FILE_ID_LEN);
	t->npages++;

err:	if (argp != NULL)
	__os_free(dbenv, argp);
	return (ret);
}

/*
 * PUBLIC: int __crdel_metasub_print __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__crdel_metasub_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__crdel_metasub_args *argp;
	u_int32_t i;
	int ch;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __crdel_metasub_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]__crdel_metasub: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tfileid: %ld\n", (long)argp->fileid);
	(void)printf("\tpgno: %lu\n", (u_long)argp->pgno);
	(void)printf("\tpage: ");
	for (i = 0; i < argp->page.size; i++) {
		ch = ((u_int8_t *)argp->page.data)[i];
		printf(isprint(ch) || ch == 0x0a ? "%c" : "%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\tlsn: [%lu][%lu]\n",
	    (u_long)argp->lsn.file, (u_long)argp->lsn.offset);
	(void)printf("\n");
	__os_free(dbenv, argp);
	return (0);
}

/*
 * PUBLIC: int __crdel_metasub_read __P((DB_ENV *, void *,
 * PUBLIC:     __crdel_metasub_args **));
 */
int
__crdel_metasub_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__crdel_metasub_args **argpp;
{
	__crdel_metasub_args *argp;
	u_int32_t uinttmp;
	u_int8_t *bp;
	int ret;

	if ((ret = __os_malloc(dbenv,
	    sizeof(__crdel_metasub_args) + sizeof(DB_TXN), &argp)) != 0)
		return (ret);

	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->fileid = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->pgno = (db_pgno_t)uinttmp;
	bp += sizeof(uinttmp);

	memset(&argp->page, 0, sizeof(argp->page));
	memcpy(&argp->page.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->page.data = bp;
	bp += argp->page.size;

	memcpy(&argp->lsn, bp,  sizeof(argp->lsn));
	bp += sizeof(argp->lsn);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __crdel_init_print __P((DB_ENV *, int (***)(DB_ENV *,
 * PUBLIC:     DBT *, DB_LSN *, db_recops, void *), size_t *));
 */
int
__crdel_init_print(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __crdel_metasub_print, DB___crdel_metasub)) != 0)
		return (ret);
	return (0);
}

/*
 * PUBLIC: int __crdel_init_getpgnos __P((DB_ENV *,
 * PUBLIC:     int (***)(DB_ENV *, DBT *, DB_LSN *, db_recops, void *),
 * PUBLIC:     size_t *));
 */
int
__crdel_init_getpgnos(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __crdel_metasub_getpgnos, DB___crdel_metasub)) != 0)
		return (ret);
	return (0);
}

/*
 * PUBLIC: int __crdel_init_recover __P((DB_ENV *, int (***)(DB_ENV *,
 * PUBLIC:     DBT *, DB_LSN *, db_recops, void *), size_t *));
 */
int
__crdel_init_recover(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __crdel_metasub_recover, DB___crdel_metasub)) != 0)
		return (ret);
	return (0);
}
