#include <stdlib.h>
#include <db.h>

int db_env_close (DB_ENV *env, u_int32_t flags)
{
  return env->close(env, flags);
}

int db_env_dbremove (DB_ENV *env, DB_TXN *txnid, const char *file,
		     const char *database, u_int32_t flags)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return env->dbremove(env, txnid, file, database, flags);
#else
  return DB_OLD_VERSION;
#endif
}

int db_env_dbrename (DB_ENV *env, DB_TXN *txnid, const char *file,
		     const char *database, const char *new, u_int32_t flags)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return env->dbrename(env, txnid, file, database, new, flags);
#else
  return DB_OLD_VERSION;
#endif
}

void db_env_err (DB_ENV *env, int error, const char *format)
{
  env->err(env, error, format);
}

void db_env_errx (DB_ENV *env, const char *format)
{
  env->errx(env, format);
}

int db_env_open (DB_ENV *env, const char *db_home, u_int32_t flags, int mode)
{
  return env->open(env, db_home, flags, mode);
}

int db_env_remove (DB_ENV *env, const char *db_home, u_int32_t flags)
{
  return env->remove(env, db_home, flags);
}

int db_env_set_app_dispatch (DB_ENV *env,
			     int (*tx_recover)(DB_ENV *env, DBT *log_rec,
					       DB_LSN *lsn, db_recops op))
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return env->set_app_dispatch(env, tx_recover);
#else
  return DB_OLD_VERSION;
#endif
}

int db_env_set_alloc (DB_ENV *env,
		      void *(*app_malloc)(size_t),
		      void *(*app_realloc)(void *, size_t),
		      void (*app_free)(void *))
{
  return env->set_alloc(env, app_malloc, app_realloc, app_free);
}

int db_env_set_data_dir (DB_ENV *env, const char *dir)
{
  return env->set_data_dir(env, dir);
}

int db_env_set_encrypt (DB_ENV *env, const char *passwd, u_int32_t flags)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return env->set_encrypt(env, passwd, flags);
#else
  return DB_OLD_VERSION;
#endif
}

void db_env_set_errcall (DB_ENV *env,
			 void (*db_errcall_fcn)(const char *errpfx, char *msg))
{
  env->set_errcall(env, db_errcall_fcn);
}

void db_env_set_errfile (DB_ENV *env, FILE *errfile)
{
  env->set_errfile(env, errfile);
}

void db_env_set_errpfx (DB_ENV *env, const char *errpfx)
{
  env->set_errpfx(env, errpfx);
}

int db_env_set_feedback (DB_ENV *env,
			 void (*db_feedback_fcn)(DB_ENV *, int opcode, int pct))
{
  return env->set_feedback(env, db_feedback_fcn);
}

int db_env_set_flags (DB_ENV *env, u_int32_t flags, int onoff)
{
  return env->set_flags(env, flags, onoff);
}

int db_env_set_paniccall (DB_ENV *env, void (*paniccall)(DB_ENV *, int errval))
{
  return env->set_paniccall(env, paniccall);
}

int db_env_set_rpc_server (DB_ENV *env, char *client, char *host,
			   long cl_timeout, long sv_timeout, u_int32_t flags)
{
  return env->set_rpc_server(env, client, host, cl_timeout, sv_timeout, flags);
}

int db_env_set_shm_key (DB_ENV *env, long shm_key)
{
  return env->set_shm_key(env, shm_key);
}

int db_env_set_tas_spins (DB_ENV *env, u_int32_t tas_spins)
{
  return env->set_tas_spins(env, tas_spins);
}

int db_env_set_timeout (DB_ENV *env, db_timeout_t timeout, u_int32_t flags)
{
  return env->set_timeout(env, timeout, flags);
}

int db_env_set_tmp_dir (DB_ENV *env, const char *dir)
{
  return env->set_tmp_dir(env, dir);
}

int db_env_set_verbose (DB_ENV *env, u_int32_t which, int onoff)
{
  return env->set_verbose(env, which, onoff);
}

int db_associate (DB *primary, DB_TXN *txnid, DB *secondary,
		  int (*callback)(DB *, const DBT *, const DBT *, DBT *),
		  u_int32_t flags)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return primary->associate(primary, txnid, secondary, callback, flags);
#else
  return primary->associate(primary, secondary, callback, flags);
#endif
}

int db_close (DB *db, u_int32_t flags)
{
  return db->close(db, flags);
}

int db_del (DB *db, DB_TXN *txnid, DBT *key, u_int32_t flags)
{
  return db->del(db, txnid, key, flags);
}

void db_err (DB *db, int error, const char *fmt)
{
  db->err(db, error, fmt);
}

void db_errx (DB *db, const char *fmt)
{
  db->errx(db, fmt);
}

int db_fd (DB *db, int *fdp)
{
  return db->fd(db, fdp);
}


int db_get (DB *db, DB_TXN *txnid, DBT *key, DBT *data, u_int32_t flags)
{
  return db->get(db, txnid, key, data, flags);
}

int db_pget (DB *db, DB_TXN *txnid, DBT *key, DBT *pkey, DBT *data,
	     u_int32_t flags)
{
  return db->pget(db, txnid, key, pkey, data, flags);
}

int db_get_byteswapped (DB *db, int *isswapped)
{
  return db->get_byteswapped(db, isswapped);
}

int db_get_type (DB *db, DBTYPE *type)
{
  return db->get_type(db, type);
}

int db_join (DB *primary, DBC **curslist, DBC **dbcp, u_int32_t flags)
{
  return primary->join(primary, curslist, dbcp, flags);
}

int db_key_range (DB *db, DB_TXN *txnid, DBT *key, DB_KEY_RANGE *key_range,
		  u_int32_t flags)
{
  return db->key_range(db, txnid, key, key_range, flags);
}

int db_open (DB *db, DB_TXN *txnid, const char *file, const char *database,
	     DBTYPE type, u_int32_t flags, int mode)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return db->open(db, txnid, file, database, type, flags, mode);
#else
  return db->open(db, file, database, type, flags, mode);
#endif
}

int db_put (DB *db, DB_TXN *txnid, DBT *key, DBT *data, u_int32_t flags)
{
  return db->put(db, txnid, key, data, flags);
}

int db_remove (DB *db, const char *file, const char *database, u_int32_t flags)
{
  return db->remove(db, file, database, flags);
}

int db_rename (DB *db, const char *file, const char *database,
	       const char *newname, u_int32_t flags)
{
  return db->rename(db, file, database, newname, flags);
}

int db_stat (DB *db, void *sp, u_int32_t flags)
{
  return db->stat(db, sp, flags);
}

int db_sync (DB *db, u_int32_t flags)
{
  return db->sync(db, flags);
}

int db_truncate (DB *db, DB_TXN *txnid, u_int32_t *countp, u_int32_t flags)
{
  return db->truncate(db, txnid, countp, flags);
}

int db_upgrade (DB *db, const char *file, u_int32_t flags)
{
  return db->upgrade(db, file, flags);
}

int db_verify (DB *db, const char *file, const char *database, FILE *outfile,
	       u_int32_t flags)
{
  return db->verify(db, file, database, outfile, flags);
}

int db_set_alloc (DB *db,
		  void *(*app_malloc)(size_t),
		  void *(*app_realloc)(void *, size_t),
		  void (*app_free)(void *))
{
  return db->set_alloc(db, app_malloc, app_realloc, app_free);
}

#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
int db_set_cache_priority (DB *db, DB_CACHE_PRIORITY priority)
{
  return db->set_cache_priority(db, priority);
}
#else
int db_set_cache_priority (DB *db, int priority)
{
  return DB_OLD_VERSION;
}
#endif

int db_set_cachesize (DB *db, u_int32_t gbytes, u_int32_t bytes, int ncache)
{
  return db->set_cachesize(db, gbytes, bytes, ncache);
}

int db_set_dup_compare (DB *db,
			int (*dup_compare_fcn)(DB *, const DBT *, const DBT *))
{
  return db->set_dup_compare(db, dup_compare_fcn);
}

int db_set_encrypt (DB *db, const char *passwd, u_int32_t flags)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return db->set_encrypt(db, passwd, flags);
#else
  return DB_OLD_VERSION;
#endif
}

void db_set_errcall (DB *db,
		     void (*db_errcall_fcn)(const char *errpfx, char *msg))
{
  db->set_errcall(db, db_errcall_fcn);
}

void db_set_errfile (DB *db, FILE *errfile)
{
  db->set_errfile(db, errfile);
}

void db_set_errpfx (DB *db, const char *errpfx)
{
  db->set_errpfx(db, errpfx);
}

int db_set_feedback (DB *db,
		     void (*db_feedback_fcn)(DB *dbp, int opcode, int pct))
{
  return db->set_feedback(db, db_feedback_fcn);
}

int db_set_flags (DB *db, u_int32_t flags)
{
  return db->set_flags(db, flags);
}

int db_set_lorder (DB *db, int lorder)
{
  return db->set_lorder(db, lorder);
}

int db_set_pagesize (DB *db, u_int32_t pagesize)
{
  return db->set_pagesize(db, pagesize);
}

int db_set_paniccall (DB *db, void (*paniccall)(DB_ENV *, int errval))
{
  return db->set_paniccall(db, paniccall);
}

int db_set_append_recno (DB *db,
			 int (*db_append_recno_fcn)(DB *dbp, DBT *data,
						    db_recno_t recno))
{
  return db->set_append_recno(db, db_append_recno_fcn);
}

int db_set_bt_compare (DB *db,
		       int (*bt_compare_fcn)(DB *, const DBT *, const DBT *))
{
  return db->set_bt_compare(db, bt_compare_fcn);
}

int db_set_bt_minkey (DB *db, u_int32_t bt_minkey)
{
  return db->set_bt_minkey(db, bt_minkey);
}

int db_set_bt_prefix (DB *db,
		      size_t (*bt_prefix_fcn)(DB *, const DBT *, const DBT *))
{
  return db->set_bt_prefix(db, bt_prefix_fcn);
}

int db_set_re_delim (DB *db, int re_delim)
{
  return db->set_re_delim(db, re_delim);
}

int db_set_re_len (DB *db, u_int32_t re_len)
{
  return db->set_re_len(db, re_len);
}

int db_set_re_pad (DB *db, int re_pad)
{
  return db->set_re_pad(db, re_pad);
}

int db_set_re_source (DB *db, char *re_source)
{
  return db->set_re_source(db, re_source);
}

int db_set_h_ffactor (DB *db, u_int32_t h_ffactor)
{
  return db->set_h_ffactor(db, h_ffactor);
}

int db_set_h_hash (DB *db,
		   u_int32_t (*h_hash_fcn)(DB *, const void *bytes,
					   u_int32_t length))
{
  return db->set_h_hash(db, h_hash_fcn);
}

int db_set_h_nelem (DB *db, u_int32_t h_nelem)
{
  return db->set_h_nelem(db, h_nelem);
}

int db_set_q_extentsize (DB *db, u_int32_t extentsize)
{
  return db->set_q_extentsize(db, extentsize);
}

int db_cursor (DB *db, DB_TXN *txnid, DBC **cursorp, u_int32_t flags)
{
  return db->cursor(db, txnid, cursorp, flags);
}

int dbcursor_c_close (DBC *cursor)
{
  return cursor->c_close(cursor);
}

int dbcursor_c_count (DBC *cursor, db_recno_t *countp, u_int32_t flags)
{
  return cursor->c_count(cursor, countp, flags);
}

int dbcursor_c_del (DBC *cursor, u_int32_t flags)
{
  return cursor->c_del(cursor, flags);
}

int dbcursor_c_dup (DBC *cursor, DBC **cursorp, u_int32_t flags)
{
  return cursor->c_dup(cursor, cursorp, flags);
}

int dbcursor_c_get (DBC *cursor, DBT *key, DBT *data, u_int32_t flags)
{
  return cursor->c_get(cursor, key, data, flags);
}

int dbcursor_c_pget (DBC *cursor, DBT *key, DBT *pkey, DBT *data, u_int32_t flags)
{
  return cursor->c_pget(cursor, key, pkey, data, flags);
}

int dbcursor_c_put (DBC *cursor, DBT *key, DBT *data, u_int32_t flags)
{
  return cursor->c_put(cursor, key, data, flags);
}

int db_undo (db_recops op)
{
  return DB_UNDO(op);
}

int db_redo (db_recops op)
{
  return DB_REDO(op);
}

void db_multiple_init (void *pointer, DBT *data)
{
  DB_MULTIPLE_INIT(pointer, data);
}

void db_multiple_next (void *pointer, DBT *data, void *retdata, size_t retdlen)
{
  DB_MULTIPLE_NEXT(pointer, data, retdata, retdlen);
}

void db_multiple_key_next (void *pointer, DBT *data, void *retkey, size_t retklen,
			   void *retdata, size_t retdlen)
{
  DB_MULTIPLE_KEY_NEXT(pointer, data, retkey, retklen, retdata, retdlen);
}

void db_multiple_recno_next (void *pointer, DBT *data, db_recno_t recno,
			     void * retdata, size_t retdlen)
{
  DB_MULTIPLE_RECNO_NEXT(pointer, data, recno, retdata, retdlen);
}

int db_env_set_lk_conflicts (DB_ENV *env, u_int8_t *conflicts, int nmodes)
{
  return env->set_lk_conflicts(env, conflicts, nmodes);
}

int db_env_set_lk_detect (DB_ENV *env, u_int32_t detect)
{
  return env->set_lk_detect(env, detect);
}

int db_env_set_lk_max_lockers (DB_ENV *env, u_int32_t max)
{
  return env->set_lk_max_lockers(env, max);
}

int db_env_set_lk_max_locks (DB_ENV *env, u_int32_t max)
{
  return env->set_lk_max_locks (env, max);
}

int db_env_set_lk_max_objects (DB_ENV *env, u_int32_t max)
{
  return env->set_lk_max_objects (env, max);
}

int db_env_lock_detect (DB_ENV *env, u_int32_t flags, u_int32_t atype,
			int *aborted)
{
  return env->lock_detect(env, flags, atype, aborted);
}

int db_env_lock_get (DB_ENV *env, u_int32_t locker, u_int32_t flags,
		     const DBT *obj, const db_lockmode_t lock_mode,
		     DB_LOCK *lock)
{
  return env->lock_get(env, locker, flags, obj, lock_mode, lock);
}

int db_env_lock_id (DB_ENV *env, u_int32_t *idp)
{
  return env->lock_id(env, idp);
}

int db_env_lock_id_free (DB_ENV *env, u_int32_t id)
{
  return env->lock_id_free(env, id);
}

int db_env_lock_put (DB_ENV *env, DB_LOCK *lock)
{
  return env->lock_put(env, lock);
}

int db_env_lock_stat (DB_ENV *env, DB_LOCK_STAT **statp, u_int32_t flags)
{
  return env->lock_stat(env, statp, flags);
}

int db_env_lock_vec (DB_ENV *env, u_int32_t locker, u_int32_t flags,
		     DB_LOCKREQ list[], int nlist, DB_LOCKREQ **elistp)
{
  return env->lock_vec(env, locker, flags, list, nlist, elistp);
}

int db_env_set_lg_bsize (DB_ENV *env, u_int32_t lg_bsize)
{
  return env->set_lg_bsize(env, lg_bsize);
}

int db_env_set_lg_dir (DB_ENV *env, const char *dir)
{
  return env->set_lg_dir(env, dir);
}

int db_env_set_lg_max (DB_ENV *env, u_int32_t lg_max)
{
  return env->set_lg_max(env, lg_max);
}

int db_env_set_lg_regionmax (DB_ENV *env, u_int32_t lg_regionmax)
{
  return env->set_lg_regionmax(env, lg_regionmax);
}

int db_env_log_archive (DB_ENV *env, char **listp[], u_int32_t flags)
{
  return env->log_archive(env, listp, flags);
}

int db_env_log_file (DB_ENV *env, const DB_LSN *lsn, char *namep, size_t len)
{
  return env->log_file(env, lsn, namep, len);
}

int db_env_log_flush (DB_ENV *env, const DB_LSN *lsn)
{
  return env->log_flush(env, lsn);
}

int db_env_log_put (DB_ENV *env, DB_LSN *lsn, const DBT *data, u_int32_t flags)
{
  return env->log_put(env, lsn, data, flags);
}

int db_env_log_stat (DB_ENV *env, DB_LOG_STAT **spp, u_int32_t flags)
{
  return env->log_stat(env, spp, flags);
}

int db_env_log_cursor (DB_ENV *env, DB_LOGC **cursorp, u_int32_t flags)
{
  return env->log_cursor(env, cursorp, flags);
}

int db_logc_close (DB_LOGC *cursor, u_int32_t flags)
{
  return cursor->close(cursor, flags);
}

int db_logc_get (DB_LOGC *logc, DB_LSN *lsn, DBT *data, u_int32_t flags)
{
  return logc->get(logc, lsn, data, flags);
}

int db_env_set_cachesize (DB_ENV *env, u_int32_t gbytes, u_int32_t bytes,
			  int ncache)
{
  return env->set_cachesize(env, gbytes, bytes, ncache);
}

int db_env_set_mp_mmapsize (DB_ENV *env, size_t mp_mmapsize)
{
  return env->set_mp_mmapsize(env, mp_mmapsize);
}

int db_env_memp_register (DB_ENV *env, int ftype,
			  int (*pgin_fcn)(DB_ENV *, db_pgno_t pgno,
					  void *pgaddr, DBT *pgcookie),
			  int (*pgout_fcn)(DB_ENV *, db_pgno_t pgno,
					   void *pgaddr, DBT *pgcookie))
{
  return env->memp_register(env, ftype, pgin_fcn, pgout_fcn);
}

int db_env_memp_stat (DB_ENV *env, DB_MPOOL_STAT **gsp, DB_MPOOL_FSTAT **fsp[],
		      u_int32_t flags)
{
  return env->memp_stat(env, gsp, fsp, flags);
}

int db_env_memp_sync (DB_ENV *env, DB_LSN *lsn)
{
  return env->memp_sync(env, lsn);
}

int db_env_memp_trickle (DB_ENV *env, int pct, int *nwrotep)
{
  return env->memp_trickle(env, pct, nwrotep);
}

int db_env_memp_fcreate (DB_ENV *env, DB_MPOOLFILE **dbmfp, u_int32_t flags)
{
  return env->memp_fcreate(env, dbmfp, flags);
}

int db_mpoolfile_close (DB_MPOOLFILE *mpf, u_int32_t flags)
{
  return mpf->close(mpf, flags);
}

int db_mpoolfile_get (DB_MPOOLFILE *mpf, db_pgno_t *pgnoaddr, u_int32_t flags,
		      void **pagep)
{
  return mpf->get(mpf, pgnoaddr, flags, pagep);
}

int db_mpoolfile_open (DB_MPOOLFILE *mpf, char *file, u_int32_t flags, int mode,
		       size_t pagesize)
{
  return mpf->open(mpf, file, flags, mode, pagesize);
}

int db_mpoolfile_put (DB_MPOOLFILE *mpf, void *pgaddr, u_int32_t flags)
{
  return mpf->put(mpf, pgaddr, flags);
}

int db_mpoolfile_set (DB_MPOOLFILE *mpf, void *pgaddr, u_int32_t flags)
{
  return mpf->set(mpf, pgaddr, flags);
}

int db_mpoolfile_sync (DB_MPOOLFILE *mpf)
{
  return mpf->sync(mpf);
}

int db_mpoolfile_set_clear_len (DB_MPOOLFILE *mpf, u_int32_t len)
{
  return mpf->set_clear_len(mpf, len);
}

int db_mpoolfile_set_fileid (DB_MPOOLFILE *mpf, u_int8_t *fileid)
{
  return mpf->set_fileid(mpf, fileid);
}

int db_mpoolfile_set_ftype (DB_MPOOLFILE *mpf, int ftype)
{
  return mpf->set_ftype(mpf, ftype);
}

int db_mpoolfile_set_lsn_offset (DB_MPOOLFILE *mpf, int32_t lsn_offset)
{
  return mpf->set_lsn_offset(mpf, lsn_offset);
}

int db_mpoolfile_set_pgcookie (DB_MPOOLFILE *mpf, DBT *pgcookie)
{
  return mpf->set_pgcookie(mpf, pgcookie);
}

int db_env_set_tx_max (DB_ENV *env, u_int32_t tx_max)
{
  return env->set_tx_max(env, tx_max);
}

int db_env_set_tx_timestamp (DB_ENV *env, time_t *timestamp)
{
  return env->set_tx_timestamp(env, timestamp);
}

int db_env_txn_checkpoint (DB_ENV *env, u_int32_t kbyte, u_int32_t min,
			   u_int32_t flags)
{
  return env->txn_checkpoint(env, kbyte, min, flags);
}

int db_env_txn_recover (DB_ENV *env, DB_PREPLIST preplist[], long count,
			long *retp, u_int32_t flags)
{
  return env->txn_recover(env, preplist, count, retp, flags);
}

int db_env_txn_stat (DB_ENV *env, DB_TXN_STAT **statp, u_int32_t flags)
{
  return env->txn_stat(env, statp, flags);
}

int db_env_txn_begin (DB_ENV *env, DB_TXN *parent, DB_TXN **tid, u_int32_t flags)
{
  return env->txn_begin(env, parent, tid, flags);
}

int db_txn_abort (DB_TXN *tid)
{
  return tid->abort(tid);
}

int db_txn_commit (DB_TXN *tid, u_int32_t flags)
{
  return tid->commit(tid, flags);
}

int db_txn_discard (DB_TXN *tid, u_int32_t flags)
{
  return tid->discard(tid, flags);
}

u_int32_t db_txn_id (DB_TXN *tid)
{
  return tid->id(tid);
}

int db_txn_prepare (DB_TXN *tid, u_int8_t gid[DB_XIDDATASIZE])
{
  return tid->prepare(tid, gid);
}

u_int32_t db_txn_set_timeout (DB_TXN *tid, db_timeout_t timeout, u_int32_t flags)
{
  return tid->set_timeout(tid, timeout, flags);
}

int db_env_set_rep_transport (DB_ENV *env, int envid,
			      int (*send)(DB_ENV *env, const DBT *control,
					  const DBT *rec, int envid,
					  u_int32_t flags))
{
  return env->set_rep_transport(env, envid, send);
}

int db_env_rep_elect (DB_ENV *env, int nsites, int priority, u_int32_t timeout,
		      int *envid)
{
  return env->rep_elect(env, nsites, priority, timeout, envid);
}

int db_env_set_rep_limit (DB_ENV *env, u_int32_t gbytes, u_int32_t bytes)
{
#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
  return env->set_rep_limit(env, gbytes, bytes);
#else
  return DB_OLD_VERSION;
#endif
}

int db_env_rep_process_message (DB_ENV *env, DBT *control, DBT *rec, int *envid)
{
  return env->rep_process_message(env, control, rec, envid);
}

int db_env_rep_start (DB_ENV *env, DBT *cdata, u_int32_t flags) {
  return env->rep_start(env, cdata, flags);
}

#if (DB_VERSION_MAJOR > 4) || (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR > 0)
int db_env_rep_stat (DB_ENV *env, DB_REP_STAT **statp, u_int32_t flags) {
  return env->rep_stat(env, statp, flags);
}
#else
int db_env_rep_stat (DB_ENV *env, void **statp, u_int32_t flags) {
  return DB_OLD_VERSION;
}
#endif
