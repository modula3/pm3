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
 * PUBLIC: int __txn_regop_log __P((DB_ENV *, DB_TXN *, DB_LSN *,
 * PUBLIC:     u_int32_t, u_int32_t, int32_t));
 */
int
__txn_regop_log(dbenv, txnid, ret_lsnp, flags,
    opcode, timestamp)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	u_int32_t opcode;
	int32_t timestamp;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t npad, rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB___txn_regop;
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
	    + sizeof(u_int32_t);
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

	uinttmp = (u_int32_t)opcode;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)timestamp;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) <= logrec.size);
	ret = dbenv->log_put(dbenv,
	   ret_lsnp, (DBT *)&logrec, flags | DB_NOCOPY);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__txn_regop_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data);
	return (ret);
}

/*
 * PUBLIC: int __txn_regop_getpgnos __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_regop_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __txn_regop_print __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_regop_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__txn_regop_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __txn_regop_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]__txn_regop: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\topcode: %lu\n", (u_long)argp->opcode);
	(void)printf("\ttimestamp: %ld\n", (long)argp->timestamp);
	(void)printf("\n");
	__os_free(dbenv, argp);
	return (0);
}

/*
 * PUBLIC: int __txn_regop_read __P((DB_ENV *, void *, __txn_regop_args **));
 */
int
__txn_regop_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__txn_regop_args **argpp;
{
	__txn_regop_args *argp;
	u_int32_t uinttmp;
	u_int8_t *bp;
	int ret;

	if ((ret = __os_malloc(dbenv,
	    sizeof(__txn_regop_args) + sizeof(DB_TXN), &argp)) != 0)
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
	argp->opcode = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->timestamp = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __txn_ckp_log __P((DB_ENV *, DB_TXN *, DB_LSN *,
 * PUBLIC:     u_int32_t, DB_LSN *, DB_LSN *, int32_t));
 */
int
__txn_ckp_log(dbenv, txnid, ret_lsnp, flags,
    ckp_lsn, last_ckp, timestamp)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	DB_LSN * ckp_lsn;
	DB_LSN * last_ckp;
	int32_t timestamp;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t npad, rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB___txn_ckp;
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
	    + sizeof(*ckp_lsn)
	    + sizeof(*last_ckp)
	    + sizeof(u_int32_t);
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

	if (ckp_lsn != NULL)
		memcpy(bp, ckp_lsn, sizeof(*ckp_lsn));
	else
		memset(bp, 0, sizeof(*ckp_lsn));
	bp += sizeof(*ckp_lsn);

	if (last_ckp != NULL)
		memcpy(bp, last_ckp, sizeof(*last_ckp));
	else
		memset(bp, 0, sizeof(*last_ckp));
	bp += sizeof(*last_ckp);

	uinttmp = (u_int32_t)timestamp;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) <= logrec.size);
	ret = dbenv->log_put(dbenv,
	   ret_lsnp, (DBT *)&logrec, flags | DB_NOCOPY);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__txn_ckp_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data);
	return (ret);
}

/*
 * PUBLIC: int __txn_ckp_getpgnos __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_ckp_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __txn_ckp_print __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_ckp_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__txn_ckp_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __txn_ckp_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]__txn_ckp: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tckp_lsn: [%lu][%lu]\n",
	    (u_long)argp->ckp_lsn.file, (u_long)argp->ckp_lsn.offset);
	(void)printf("\tlast_ckp: [%lu][%lu]\n",
	    (u_long)argp->last_ckp.file, (u_long)argp->last_ckp.offset);
	(void)printf("\ttimestamp: %ld\n", (long)argp->timestamp);
	(void)printf("\n");
	__os_free(dbenv, argp);
	return (0);
}

/*
 * PUBLIC: int __txn_ckp_read __P((DB_ENV *, void *, __txn_ckp_args **));
 */
int
__txn_ckp_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__txn_ckp_args **argpp;
{
	__txn_ckp_args *argp;
	u_int32_t uinttmp;
	u_int8_t *bp;
	int ret;

	if ((ret = __os_malloc(dbenv,
	    sizeof(__txn_ckp_args) + sizeof(DB_TXN), &argp)) != 0)
		return (ret);

	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&argp->ckp_lsn, bp,  sizeof(argp->ckp_lsn));
	bp += sizeof(argp->ckp_lsn);

	memcpy(&argp->last_ckp, bp,  sizeof(argp->last_ckp));
	bp += sizeof(argp->last_ckp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->timestamp = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __txn_child_log __P((DB_ENV *, DB_TXN *, DB_LSN *,
 * PUBLIC:     u_int32_t, u_int32_t, DB_LSN *));
 */
int
__txn_child_log(dbenv, txnid, ret_lsnp, flags,
    child, c_lsn)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	u_int32_t child;
	DB_LSN * c_lsn;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t npad, rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB___txn_child;
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
	    + sizeof(*c_lsn);
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

	uinttmp = (u_int32_t)child;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (c_lsn != NULL)
		memcpy(bp, c_lsn, sizeof(*c_lsn));
	else
		memset(bp, 0, sizeof(*c_lsn));
	bp += sizeof(*c_lsn);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) <= logrec.size);
	ret = dbenv->log_put(dbenv,
	   ret_lsnp, (DBT *)&logrec, flags | DB_NOCOPY);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__txn_child_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data);
	return (ret);
}

/*
 * PUBLIC: int __txn_child_getpgnos __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_child_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __txn_child_print __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_child_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__txn_child_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __txn_child_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]__txn_child: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tchild: 0x%lx\n", (u_long)argp->child);
	(void)printf("\tc_lsn: [%lu][%lu]\n",
	    (u_long)argp->c_lsn.file, (u_long)argp->c_lsn.offset);
	(void)printf("\n");
	__os_free(dbenv, argp);
	return (0);
}

/*
 * PUBLIC: int __txn_child_read __P((DB_ENV *, void *, __txn_child_args **));
 */
int
__txn_child_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__txn_child_args **argpp;
{
	__txn_child_args *argp;
	u_int32_t uinttmp;
	u_int8_t *bp;
	int ret;

	if ((ret = __os_malloc(dbenv,
	    sizeof(__txn_child_args) + sizeof(DB_TXN), &argp)) != 0)
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
	argp->child = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&argp->c_lsn, bp,  sizeof(argp->c_lsn));
	bp += sizeof(argp->c_lsn);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __txn_xa_regop_log __P((DB_ENV *, DB_TXN *, DB_LSN *,
 * PUBLIC:     u_int32_t, u_int32_t, const DBT *, int32_t, u_int32_t, u_int32_t,
 * PUBLIC:     DB_LSN *));
 */
int
__txn_xa_regop_log(dbenv, txnid, ret_lsnp, flags,
    opcode, xid, formatID, gtrid, bqual, begin_lsn)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	u_int32_t opcode;
	const DBT *xid;
	int32_t formatID;
	u_int32_t gtrid;
	u_int32_t bqual;
	DB_LSN * begin_lsn;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t zero;
	u_int32_t uinttmp;
	u_int32_t npad, rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB___txn_xa_regop;
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
	    + sizeof(u_int32_t) + (xid == NULL ? 0 : xid->size)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(*begin_lsn);
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

	uinttmp = (u_int32_t)opcode;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (xid == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &xid->size, sizeof(xid->size));
		bp += sizeof(xid->size);
		memcpy(bp, xid->data, xid->size);
		bp += xid->size;
	}

	uinttmp = (u_int32_t)formatID;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)gtrid;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)bqual;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (begin_lsn != NULL)
		memcpy(bp, begin_lsn, sizeof(*begin_lsn));
	else
		memset(bp, 0, sizeof(*begin_lsn));
	bp += sizeof(*begin_lsn);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) <= logrec.size);
	ret = dbenv->log_put(dbenv,
	   ret_lsnp, (DBT *)&logrec, flags | DB_NOCOPY);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__txn_xa_regop_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data);
	return (ret);
}

/*
 * PUBLIC: int __txn_xa_regop_getpgnos __P((DB_ENV *, DBT *,
 * PUBLIC:     DB_LSN *, db_recops, void *));
 */
int
__txn_xa_regop_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __txn_xa_regop_print __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_xa_regop_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__txn_xa_regop_args *argp;
	u_int32_t i;
	int ch;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __txn_xa_regop_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]__txn_xa_regop: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\topcode: %lu\n", (u_long)argp->opcode);
	(void)printf("\txid: ");
	for (i = 0; i < argp->xid.size; i++) {
		ch = ((u_int8_t *)argp->xid.data)[i];
		printf(isprint(ch) || ch == 0x0a ? "%c" : "%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\tformatID: %ld\n", (long)argp->formatID);
	(void)printf("\tgtrid: %u\n", argp->gtrid);
	(void)printf("\tbqual: %u\n", argp->bqual);
	(void)printf("\tbegin_lsn: [%lu][%lu]\n",
	    (u_long)argp->begin_lsn.file, (u_long)argp->begin_lsn.offset);
	(void)printf("\n");
	__os_free(dbenv, argp);
	return (0);
}

/*
 * PUBLIC: int __txn_xa_regop_read __P((DB_ENV *, void *,
 * PUBLIC:     __txn_xa_regop_args **));
 */
int
__txn_xa_regop_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__txn_xa_regop_args **argpp;
{
	__txn_xa_regop_args *argp;
	u_int32_t uinttmp;
	u_int8_t *bp;
	int ret;

	if ((ret = __os_malloc(dbenv,
	    sizeof(__txn_xa_regop_args) + sizeof(DB_TXN), &argp)) != 0)
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
	argp->opcode = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memset(&argp->xid, 0, sizeof(argp->xid));
	memcpy(&argp->xid.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->xid.data = bp;
	bp += argp->xid.size;

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->formatID = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->gtrid = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->bqual = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&argp->begin_lsn, bp,  sizeof(argp->begin_lsn));
	bp += sizeof(argp->begin_lsn);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __txn_recycle_log __P((DB_ENV *, DB_TXN *, DB_LSN *,
 * PUBLIC:     u_int32_t, u_int32_t, u_int32_t));
 */
int
__txn_recycle_log(dbenv, txnid, ret_lsnp, flags,
    min, max)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	u_int32_t min;
	u_int32_t max;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t npad, rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB___txn_recycle;
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
	    + sizeof(u_int32_t);
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

	uinttmp = (u_int32_t)min;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)max;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) <= logrec.size);
	ret = dbenv->log_put(dbenv,
	   ret_lsnp, (DBT *)&logrec, flags | DB_NOCOPY);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__txn_recycle_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data);
	return (ret);
}

/*
 * PUBLIC: int __txn_recycle_getpgnos __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_recycle_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __txn_recycle_print __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:     db_recops, void *));
 */
int
__txn_recycle_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__txn_recycle_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __txn_recycle_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]__txn_recycle: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tmin: %u\n", argp->min);
	(void)printf("\tmax: %u\n", argp->max);
	(void)printf("\n");
	__os_free(dbenv, argp);
	return (0);
}

/*
 * PUBLIC: int __txn_recycle_read __P((DB_ENV *, void *,
 * PUBLIC:     __txn_recycle_args **));
 */
int
__txn_recycle_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__txn_recycle_args **argpp;
{
	__txn_recycle_args *argp;
	u_int32_t uinttmp;
	u_int8_t *bp;
	int ret;

	if ((ret = __os_malloc(dbenv,
	    sizeof(__txn_recycle_args) + sizeof(DB_TXN), &argp)) != 0)
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
	argp->min = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->max = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __txn_init_print __P((DB_ENV *, int (***)(DB_ENV *,
 * PUBLIC:     DBT *, DB_LSN *, db_recops, void *), size_t *));
 */
int
__txn_init_print(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_regop_print, DB___txn_regop)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_ckp_print, DB___txn_ckp)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_child_print, DB___txn_child)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_xa_regop_print, DB___txn_xa_regop)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_recycle_print, DB___txn_recycle)) != 0)
		return (ret);
	return (0);
}

/*
 * PUBLIC: int __txn_init_getpgnos __P((DB_ENV *, int (***)(DB_ENV *,
 * PUBLIC:     DBT *, DB_LSN *, db_recops, void *), size_t *));
 */
int
__txn_init_getpgnos(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_regop_getpgnos, DB___txn_regop)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_ckp_getpgnos, DB___txn_ckp)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_child_getpgnos, DB___txn_child)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_xa_regop_getpgnos, DB___txn_xa_regop)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_recycle_getpgnos, DB___txn_recycle)) != 0)
		return (ret);
	return (0);
}

/*
 * PUBLIC: int __txn_init_recover __P((DB_ENV *, int (***)(DB_ENV *,
 * PUBLIC:     DBT *, DB_LSN *, db_recops, void *), size_t *));
 */
int
__txn_init_recover(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_regop_recover, DB___txn_regop)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_ckp_recover, DB___txn_ckp)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_child_recover, DB___txn_child)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_xa_regop_recover, DB___txn_xa_regop)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __txn_recycle_recover, DB___txn_recycle)) != 0)
		return (ret);
	return (0);
}
