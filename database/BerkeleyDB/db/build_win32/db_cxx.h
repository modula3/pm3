/* DO NOT EDIT: automatically built by dist/s_win32. */
/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2002
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

#ifndef _DB_CXX_H_
#define	_DB_CXX_H_
//
// C++ assumptions:
//
// To ensure portability to many platforms, both new and old, we make
// few assumptions about the C++ compiler and library.  For example,
// we do not expect STL, templates or namespaces to be available.  The
// "newest" C++ feature used is exceptions, which are used liberally
// to transmit error information.  Even the use of exceptions can be
// disabled at runtime, to do so, use the DB_CXX_NO_EXCEPTIONS flags
// with the DbEnv or Db constructor.
//
// C++ naming conventions:
//
//  - All top level class names start with Db.
//  - All class members start with lower case letter.
//  - All private data members are suffixed with underscore.
//  - Use underscores to divide names into multiple words.
//  - Simple data accessors are named with get_ or set_ prefix.
//  - All method names are taken from names of functions in the C
//    layer of db (usually by dropping a prefix like "db_").
//    These methods have the same argument types and order,
//    other than dropping the explicit arg that acts as "this".
//
// As a rule, each DbFoo object has exactly one underlying DB_FOO struct
// (defined in db.h) associated with it.  In some cases, we inherit directly
// from the DB_FOO structure to make this relationship explicit.  Often,
// the underlying C layer allocates and deallocates these structures, so
// there is no easy way to add any data to the DbFoo class.  When you see
// a comment about whether data is permitted to be added, this is what
// is going on.  Of course, if we need to add data to such C++ classes
// in the future, we will arrange to have an indirect pointer to the
// DB_FOO struct (as some of the classes already have).
//

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Forward declarations
//

#include <stdarg.h>

#define	HAVE_CXX_STDHEADERS 1
#ifdef HAVE_CXX_STDHEADERS
#include <iostream>
#define	__DB_OSTREAMCLASS	std::ostream
#else
#include <iostream.h>
#define	__DB_OSTREAMCLASS	ostream
#endif

#include "db.h"
#include "cxx_common.h"
#include "cxx_except.h"

class Db;                                        // forward
class Dbc;                                       // forward
class DbEnv;                                     // forward
class DbInfo;                                    // forward
class DbLock;                                    // forward
class DbLogc;                                    // forward
class DbLsn;                                     // forward
class DbMpoolFile;                               // forward
class DbPreplist;                                // forward
class Dbt;                                       // forward
class DbTxn;                                     // forward

// These classes are not defined here and should be invisible
// to the user, but some compilers require forward references.
// There is one for each use of the DEFINE_DB_CLASS macro.

class DbImp;
class DbEnvImp;
class DbMpoolFileImp;
class DbTxnImp;

// DEFINE_DB_CLASS defines an imp_ data member and imp() accessor.
// The underlying type is a pointer to an opaque *Imp class, that
// gets converted to the correct implementation class by the implementation.
//
// Since these defines use "private/public" labels, and leave the access
// being "private", we always use these by convention before any data
// members in the private section of a class.  Keeping them in the
// private section also emphasizes that they are off limits to user code.
//
#define	DEFINE_DB_CLASS(name) \
	public: class name##Imp* imp() { return (imp_); } \
	public: const class name##Imp* constimp() const { return (imp_); } \
	private: class name##Imp* imp_

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Turn off inappropriate compiler warnings
//

#ifdef _MSC_VER

// These are level 4 warnings that are explicitly disabled.
// With Visual C++, by default you do not see above level 3 unless
// you use /W4.  But we like to compile with the highest level
// warnings to catch other errors.
//
// 4201: nameless struct/union
//       triggered by standard include file <winnt.h>
//
// 4514: unreferenced inline function has been removed
//       certain include files in MSVC define methods that are not called
//
#pragma warning(disable: 4201 4514)

#endif

// Some interfaces can be customized by allowing users to define
// callback functions.  For performance and logistical reasons, some
// callback functions must be declared in extern "C" blocks.  For others,
// we allow you to declare the callbacks in C++ or C (or an extern "C"
// block) as you wish.  See the set methods for the callbacks for
// the choices.
//
extern "C" {
	typedef void * (*db_malloc_fcn_type)
		(size_t);
	typedef void * (*db_realloc_fcn_type)
		(void *, size_t);
	typedef void (*db_free_fcn_type)
		(void *);
	typedef int (*bt_compare_fcn_type)          /*C++ version available*/
		(DB *, const DBT *, const DBT *);
	typedef size_t (*bt_prefix_fcn_type)        /*C++ version available*/
		(DB *, const DBT *, const DBT *);
	typedef int (*dup_compare_fcn_type)         /*C++ version available*/
		(DB *, const DBT *, const DBT *);
	typedef u_int32_t (*h_hash_fcn_type)        /*C++ version available*/
		(DB *, const void *, u_int32_t);
	typedef int (*pgin_fcn_type)
		(DB_ENV *dbenv, db_pgno_t pgno, void *pgaddr, DBT *pgcookie);
	typedef int (*pgout_fcn_type)
		(DB_ENV *dbenv, db_pgno_t pgno, void *pgaddr, DBT *pgcookie);
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Lock classes
//

class _exported DbLock
{
	friend class DbEnv;

public:
	DbLock();
	DbLock(const DbLock &);
	DbLock &operator = (const DbLock &);

protected:
	// We can add data to this class if needed
	// since its contained class is not allocated by db.
	// (see comment at top)

	DbLock(DB_LOCK);
	DB_LOCK lock_;
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Log classes
//

class _exported DbLsn : protected DB_LSN
{
	friend class DbEnv;          // friendship needed to cast to base class
	friend class DbLogc;         // friendship needed to cast to base class
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Memory pool classes
//

class _exported DbMpoolFile
{
	friend class DbEnv;

private:
	// Put this first to allow inlining with some C++ compilers (g++-2.95)
	DEFINE_DB_CLASS(DbMpoolFile);

public:
	int close(u_int32_t flags);
	int get(db_pgno_t *pgnoaddr, u_int32_t flags, void *pagep);
	void last_pgno(db_pgno_t *pgnoaddr);
	int open(const char *file, u_int32_t flags, int mode, size_t pagesize);
	int put(void *pgaddr, u_int32_t flags);
	void refcnt(db_pgno_t *pgnoaddr);
	int set(void *pgaddr, u_int32_t flags);
	int set_clear_len(u_int32_t len);
	int set_fileid(u_int8_t *fileid);
	int set_ftype(int ftype);
	int set_lsn_offset(int32_t offset);
	int set_pgcookie(DBT *dbt);
	void set_unlink(int);
	int sync();

	virtual DB_MPOOLFILE *get_DB_MPOOLFILE()
	{
		return (DB_MPOOLFILE *)imp();
	}

	virtual const DB_MPOOLFILE *get_const_DB_MPOOLFILE() const
	{
		return (const DB_MPOOLFILE *)constimp();
	}

private:
	// We can add data to this class if needed
	// since it is implemented via a pointer.
	// (see comment at top)

	// Note: use DbEnv::memp_fcreate() to get pointers to a DbMpoolFile,
	// and call DbMpoolFile::close() rather than delete to release them.
	//
	DbMpoolFile();

	// Shut g++ up.
protected:
	virtual ~DbMpoolFile();

private:
	// no copying
	DbMpoolFile(const DbMpoolFile &);
	void operator = (const DbMpoolFile &);
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// This is filled in and returned by the DbEnv::txn_recover() method.
//

class _exported DbPreplist
{
public:
	DbTxn *txn;
	u_int8_t gid[DB_XIDDATASIZE];
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Transaction classes
//

class _exported DbTxn
{
	friend class DbEnv;

private:
	// Put this first to allow inlining with some C++ compilers (g++-2.95)
	DEFINE_DB_CLASS(DbTxn);

public:
	int abort();
	int commit(u_int32_t flags);
	int discard(u_int32_t flags);
	u_int32_t id();
	int prepare(u_int8_t *gid);
	int set_timeout(db_timeout_t timeout, u_int32_t flags);

	virtual DB_TXN *get_DB_TXN()
	{
		return (DB_TXN *)imp();
	}

	virtual const DB_TXN *get_const_DB_TXN() const
	{
		return (const DB_TXN *)constimp();
	}

	static DbTxn* get_DbTxn(DB_TXN *txn)
	{
		return (DbTxn *)txn->api_internal;
	}

	static const DbTxn* get_const_DbTxn(const DB_TXN *txn)
	{
		return (const DbTxn *)txn->api_internal;
	}

	// For internal use only.
	static DbTxn* wrap_DB_TXN(DB_TXN *txn);

private:
	// We can add data to this class if needed
	// since it is implemented via a pointer.
	// (see comment at top)

	// Note: use DbEnv::txn_begin() to get pointers to a DbTxn,
	// and call DbTxn::abort() or DbTxn::commit rather than
	// delete to release them.
	//
	DbTxn();
	// For internal use only.
	DbTxn(DB_TXN *txn);
	virtual ~DbTxn();

	// no copying
	DbTxn(const DbTxn &);
	void operator = (const DbTxn &);
};

//
// Berkeley DB environment class.  Provides functions for opening databases.
// User of this library can use this class as a starting point for
// developing a DB application - derive their application class from
// this one, add application control logic.
//
// Note that if you use the default constructor, you must explicitly
// call appinit() before any other db activity (e.g. opening files)
//
class _exported DbEnv
{
	friend class Db;
	friend class DbLock;
	friend class DbMpoolFile;

private:
	// Put this first to allow inlining with some C++ compilers (g++-2.95)
	DEFINE_DB_CLASS(DbEnv);

public:
	// After using this constructor, you can set any needed
	// parameters for the environment using the set_* methods.
	// Then call open() to finish initializing the environment
	// and attaching it to underlying files.
	//
	DbEnv(u_int32_t flags);

	virtual ~DbEnv();

	// These methods match those in the C interface.
	//
	virtual int close(u_int32_t);
	virtual int dbremove(DbTxn *txn, const char *name, const char *subdb,
	    u_int32_t flags);
	virtual int dbrename(DbTxn *txn, const char *name, const char *subdb,
	    const char *newname, u_int32_t flags);
	virtual void err(int, const char *, ...);
	virtual void errx(const char *, ...);
	virtual void *get_app_private() const;
	virtual int open(const char *, u_int32_t, int);
	virtual int remove(const char *, u_int32_t);
	virtual int set_alloc(db_malloc_fcn_type, db_realloc_fcn_type,
		      db_free_fcn_type);
	virtual void set_app_private(void *);
	virtual int set_cachesize(u_int32_t, u_int32_t, int);
	virtual int set_data_dir(const char *);
	virtual int set_encrypt(const char *, int);
	virtual void set_errcall(void (*)(const char *, char *));
	virtual void set_errfile(FILE *);
	virtual void set_errpfx(const char *);
	virtual int set_flags(u_int32_t, int);
	virtual int set_feedback(void (*)(DbEnv *, int, int));
	virtual int set_lg_bsize(u_int32_t);
	virtual int set_lg_dir(const char *);
	virtual int set_lg_max(u_int32_t);
	virtual int set_lg_regionmax(u_int32_t);
	virtual int set_lk_conflicts(u_int8_t *, int);
	virtual int set_lk_detect(u_int32_t);
	virtual int set_lk_max(u_int32_t);
	virtual int set_lk_max_lockers(u_int32_t);
	virtual int set_lk_max_locks(u_int32_t);
	virtual int set_lk_max_objects(u_int32_t);
	virtual int set_mp_mmapsize(size_t);
	virtual int set_paniccall(void (*)(DbEnv *, int));
	virtual int set_rpc_server(void *, char *, long, long, u_int32_t);
	virtual int set_shm_key(long);
	virtual int set_timeout(db_timeout_t timeout, u_int32_t flags);
	virtual int set_tmp_dir(const char *);
	virtual int set_tas_spins(u_int32_t);
	virtual int set_tx_max(u_int32_t);
	virtual int set_app_dispatch(int (*)(DbEnv *,
	    Dbt *, DbLsn *, db_recops));
	virtual int set_tx_timestamp(time_t *);
	virtual int set_verbose(u_int32_t which, int onoff);

	// Version information.  A static method so it can be obtained anytime.
	//
	static char *version(int *major, int *minor, int *patch);

	// Convert DB errors to strings
	static char *strerror(int);

	// If an error is detected and the error call function
	// or stream is set, a message is dispatched or printed.
	// If a prefix is set, each message is prefixed.
	//
	// You can use set_errcall() or set_errfile() above to control
	// error functionality.  Alternatively, you can call
	// set_error_stream() to force all errors to a C++ stream.
	// It is unwise to mix these approaches.
	//
	virtual void set_error_stream(__DB_OSTREAMCLASS *);

	// used internally
	static void runtime_error(const char *caller, int err,
				  int error_policy);
	static void runtime_error_dbt(const char *caller, Dbt *dbt,
				  int error_policy);
	static void runtime_error_lock_get(const char *caller, int err,
				  db_lockop_t op, db_lockmode_t mode,
				  const Dbt *obj, DbLock lock, int index,
				  int error_policy);

	// Lock functions
	//
	virtual int lock_detect(u_int32_t flags, u_int32_t atype, int *aborted);
	virtual int lock_get(u_int32_t locker, u_int32_t flags, const Dbt *obj,
		     db_lockmode_t lock_mode, DbLock *lock);
	virtual int lock_id(u_int32_t *idp);
	virtual int lock_id_free(u_int32_t id);
	virtual int lock_put(DbLock *lock);
	virtual int lock_stat(DB_LOCK_STAT **statp, u_int32_t flags);
	virtual int lock_vec(u_int32_t locker, u_int32_t flags, DB_LOCKREQ list[],
		     int nlist, DB_LOCKREQ **elistp);

	// Log functions
	//
	virtual int log_archive(char **list[], u_int32_t flags);
	static int log_compare(const DbLsn *lsn0, const DbLsn *lsn1);
	virtual int log_cursor(DbLogc **cursorp, u_int32_t flags);
	virtual int log_file(DbLsn *lsn, char *namep, size_t len);
	virtual int log_flush(const DbLsn *lsn);
	virtual int log_put(DbLsn *lsn, const Dbt *data, u_int32_t flags);

	virtual int log_stat(DB_LOG_STAT **spp, u_int32_t flags);

	// Mpool functions
	//
	virtual int memp_fcreate(DbMpoolFile **dbmfp, u_int32_t flags);
	virtual int memp_register(int ftype,
			  pgin_fcn_type pgin_fcn,
			  pgout_fcn_type pgout_fcn);
	virtual int memp_stat(DB_MPOOL_STAT
		      **gsp, DB_MPOOL_FSTAT ***fsp, u_int32_t flags);
	virtual int memp_sync(DbLsn *lsn);
	virtual int memp_trickle(int pct, int *nwrotep);

	// Transaction functions
	//
	virtual int txn_begin(DbTxn *pid, DbTxn **tid, u_int32_t flags);
	virtual int txn_checkpoint(u_int32_t kbyte, u_int32_t min, u_int32_t flags);
	virtual int txn_recover(DbPreplist *preplist, long count,
			long *retp, u_int32_t flags);
	virtual int txn_stat(DB_TXN_STAT **statp, u_int32_t flags);

	// Replication functions
	//
	virtual int rep_elect(int, int, u_int32_t, int *);
	virtual int rep_process_message(Dbt *, Dbt *, int *);
	virtual int rep_start(Dbt *, u_int32_t);
	virtual int rep_stat(DB_REP_STAT **statp, u_int32_t flags);
	virtual int set_rep_limit(u_int32_t, u_int32_t);
	virtual int set_rep_transport(u_int32_t,
	    int (*)(DbEnv *, const Dbt *, const Dbt *, int, u_int32_t));

	// Conversion functions
	//
	virtual DB_ENV *get_DB_ENV()
	{
		return (DB_ENV *)imp();
	}

	virtual const DB_ENV *get_const_DB_ENV() const
	{
		return (const DB_ENV *)constimp();
	}

	static DbEnv* get_DbEnv(DB_ENV *dbenv)
	{
		return (DbEnv *)dbenv->api1_internal;
	}

	static const DbEnv* get_const_DbEnv(const DB_ENV *dbenv)
	{
		return (const DbEnv *)dbenv->api1_internal;
	}

	// For internal use only.
	static DbEnv* wrap_DB_ENV(DB_ENV *dbenv);

	// These are public only because they need to be called
	// via C functions.  They should never be called by users
	// of this class.
	//
	static void _stream_error_function(const char *, char *);
	static int _app_dispatch_intercept(DB_ENV *env, DBT *dbt, DB_LSN *lsn,
					db_recops op);
	static void _paniccall_intercept(DB_ENV *env, int errval);
	static void _feedback_intercept(DB_ENV *env, int opcode, int pct);
	static int _rep_send_intercept(DB_ENV *env,
				       const DBT *cntrl, const DBT *data,
				       int id, u_int32_t flags);

private:
	void cleanup();
	int initialize(DB_ENV *env);
	int error_policy();

	// For internal use only.
	DbEnv(DB_ENV *, u_int32_t flags);

	// no copying
	DbEnv(const DbEnv &);
	void operator = (const DbEnv &);

	// instance data
	int construct_error_;
	u_int32_t construct_flags_;
	int (*app_dispatch_callback_)(DbEnv *, Dbt *, DbLsn *, db_recops);
	void (*feedback_callback_)(DbEnv *, int, int);
	void (*paniccall_callback_)(DbEnv *, int);
	int (*pgin_callback_)(DbEnv *dbenv, db_pgno_t pgno,
			      void *pgaddr, Dbt *pgcookie);
	int (*pgout_callback_)(DbEnv *dbenv, db_pgno_t pgno,
			       void *pgaddr, Dbt *pgcookie);
	int (*rep_send_callback_)(DbEnv *,
	    const Dbt *, const Dbt *, int, u_int32_t);

	// class data
	static __DB_OSTREAMCLASS *error_stream_;
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//
// Table access classes
//

//
// Represents a database table = a set of keys with associated values.
//
class _exported Db
{
	friend class DbEnv;

private:
	// Put this first to allow inlining with some C++ compilers (g++-2.95)
	DEFINE_DB_CLASS(Db);

public:
	Db(DbEnv*, u_int32_t);      // create a Db object, then call open()
	virtual ~Db();              // does *not* call close.

	// These methods exactly match those in the C interface.
	//
	virtual int associate(DbTxn *txn, Db *secondary,
	    int (*callback)(Db *, const Dbt *, const Dbt *, Dbt *),
	    u_int32_t flags);
	virtual int close(u_int32_t flags);
        virtual int cursor(DbTxn *txnid, Dbc **cursorp, u_int32_t flags);
	virtual int del(DbTxn *txnid, Dbt *key, u_int32_t flags);
	virtual void err(int, const char *, ...);
	virtual void errx(const char *, ...);
	virtual int fd(int *fdp);
	virtual int get(DbTxn *txnid, Dbt *key, Dbt *data, u_int32_t flags);
	virtual void *get_app_private() const;
	virtual int get_byteswapped(int *);
	virtual int get_type(DBTYPE *);
	virtual int join(Dbc **curslist, Dbc **dbcp, u_int32_t flags);
	virtual int key_range(DbTxn *, Dbt *, DB_KEY_RANGE *, u_int32_t);
	virtual int open(DbTxn *txnid,
	    const char *, const char *subname, DBTYPE, u_int32_t, int);
	virtual int pget(DbTxn *txnid, Dbt *key, Dbt *pkey, Dbt *data,
		 u_int32_t flags);
	virtual int put(DbTxn *, Dbt *, Dbt *, u_int32_t);
	virtual int remove(const char *, const char *, u_int32_t);
	virtual int rename(const char *, const char *, const char *, u_int32_t);
	virtual int set_alloc(db_malloc_fcn_type, db_realloc_fcn_type,
		      db_free_fcn_type);
	virtual void set_app_private(void *);
	virtual int set_append_recno(int (*)(Db *, Dbt *, db_recno_t));
	virtual int set_bt_compare(bt_compare_fcn_type); /*deprecated*/
	virtual int set_bt_compare(int (*)(Db *, const Dbt *, const Dbt *));
	virtual int set_bt_maxkey(u_int32_t);
	virtual int set_bt_minkey(u_int32_t);
	virtual int set_bt_prefix(bt_prefix_fcn_type); /*deprecated*/
	virtual int set_bt_prefix(size_t (*)(Db *, const Dbt *, const Dbt *));
	virtual int set_cachesize(u_int32_t, u_int32_t, int);
	virtual int set_cache_priority(DB_CACHE_PRIORITY);
	virtual int set_dup_compare(dup_compare_fcn_type); /*deprecated*/
	virtual int set_dup_compare(int (*)(Db *, const Dbt *, const Dbt *));
	virtual int set_encrypt(const char *, int);
	virtual void set_errcall(void (*)(const char *, char *));
	virtual void set_errfile(FILE *);
	virtual void set_errpfx(const char *);
	virtual int set_feedback(void (*)(Db *, int, int));
	virtual int set_flags(u_int32_t);
	virtual int set_h_ffactor(u_int32_t);
	virtual int set_h_hash(h_hash_fcn_type); /*deprecated*/
	virtual int set_h_hash(u_int32_t (*)(Db *, const void *, u_int32_t));
	virtual int set_h_nelem(u_int32_t);
	virtual int set_lorder(int);
	virtual int set_pagesize(u_int32_t);
	virtual int set_paniccall(void (*)(DbEnv *, int));
	virtual int set_re_delim(int);
	virtual int set_re_len(u_int32_t);
	virtual int set_re_pad(int);
	virtual int set_re_source(char *);
	virtual int set_q_extentsize(u_int32_t);
	virtual int stat(void *sp, u_int32_t flags);
	virtual int sync(u_int32_t flags);
	virtual int truncate(DbTxn *, u_int32_t *, u_int32_t);
	virtual int upgrade(const char *name, u_int32_t flags);
	virtual int verify(const char *, const char *, __DB_OSTREAMCLASS *, u_int32_t);

	// These additional methods are not in the C interface, and
	// are only available for C++.
	//
	virtual void set_error_stream(__DB_OSTREAMCLASS *);

	virtual DB *get_DB()
	{
		return (DB *)imp();
	}

	virtual const DB *get_const_DB() const
	{
		return (const DB *)constimp();
	}

	static Db* get_Db(DB *db)
	{
		return (Db *)db->api_internal;
	}

	static const Db* get_const_Db(const DB *db)
	{
		return (const Db *)db->api_internal;
	}

private:
	// no copying
	Db(const Db &);
	Db &operator = (const Db &);

	void cleanup();
	int initialize();
	int error_policy();

	// instance data
	DbEnv *env_;
	int construct_error_;
	u_int32_t flags_;
	u_int32_t construct_flags_;

public:
	// These are public only because they need to be called
	// via C callback functions.  They should never be used by
	// external users of this class.
	//
	int (*append_recno_callback_)(Db *, Dbt *, db_recno_t);
	int (*associate_callback_)(Db *, const Dbt *, const Dbt *, Dbt *);
	int (*bt_compare_callback_)(Db *, const Dbt *, const Dbt *);
	size_t (*bt_prefix_callback_)(Db *, const Dbt *, const Dbt *);
	int (*dup_compare_callback_)(Db *, const Dbt *, const Dbt *);
	void (*feedback_callback_)(Db *, int, int);
	u_int32_t (*h_hash_callback_)(Db *, const void *, u_int32_t);
};

//
// A chunk of data, maybe a key or value.
//
class _exported Dbt : private DBT
{
	friend class Dbc;
	friend class Db;
	friend class DbEnv;
	friend class DbLogc;

public:

	// key/data
	void *get_data() const                 { return data; }
	void set_data(void *value)             { data = value; }

	// key/data length
	u_int32_t get_size() const             { return size; }
	void set_size(u_int32_t value)         { size = value; }

	// RO: length of user buffer.
	u_int32_t get_ulen() const             { return ulen; }
	void set_ulen(u_int32_t value)         { ulen = value; }

	// RO: get/put record length.
	u_int32_t get_dlen() const             { return dlen; }
	void set_dlen(u_int32_t value)         { dlen = value; }

	// RO: get/put record offset.
	u_int32_t get_doff() const             { return doff; }
	void set_doff(u_int32_t value)         { doff = value; }

	// flags
	u_int32_t get_flags() const            { return flags; }
	void set_flags(u_int32_t value)        { flags = value; }

	// Conversion functions
	DBT *get_DBT()                         { return (DBT *)this; }
	const DBT *get_const_DBT() const       { return (const DBT *)this; }

	static Dbt* get_Dbt(DBT *dbt)          { return (Dbt *)dbt; }
	static const Dbt* get_const_Dbt(const DBT *dbt)
					       { return (const Dbt *)dbt; }

	Dbt(void *data, u_int32_t size);
	Dbt();
	~Dbt();
	Dbt(const Dbt &);
	Dbt &operator = (const Dbt &);

private:
	// Note: no extra data appears in this class (other than
	// inherited from DBT) since we need DBT and Dbt objects
	// to have interchangable pointers.
	//
	// When subclassing this class, remember that callback
	// methods like bt_compare, bt_prefix, dup_compare may
	// internally manufacture DBT objects (which later are
	// cast to Dbt), so such callbacks might receive objects
	// not of your subclassed type.
};

class _exported Dbc : protected DBC
{
	friend class Db;

public:
	int close();
	int count(db_recno_t *countp, u_int32_t flags);
	int del(u_int32_t flags);
	int dup(Dbc** cursorp, u_int32_t flags);
	int get(Dbt* key, Dbt *data, u_int32_t flags);
	int pget(Dbt* key, Dbt* pkey, Dbt *data, u_int32_t flags);
	int put(Dbt* key, Dbt *data, u_int32_t flags);

private:
	// No data is permitted in this class (see comment at top)

	// Note: use Db::cursor() to get pointers to a Dbc,
	// and call Dbc::close() rather than delete to release them.
	//
	Dbc();
	~Dbc();

	// no copying
	Dbc(const Dbc &);
	Dbc &operator = (const Dbc &);
};

class _exported DbLogc : protected DB_LOGC
{
	friend class DbEnv;

public:
	int close(u_int32_t _flags);
	int get(DbLsn *lsn, Dbt *data, u_int32_t _flags);

private:
	// No data is permitted in this class (see comment at top)

	// Note: use Db::cursor() to get pointers to a Dbc,
	// and call Dbc::close() rather than delete to release them.
	//
	DbLogc();
	~DbLogc();

	// no copying
	DbLogc(const Dbc &);
	DbLogc &operator = (const Dbc &);
};
#endif /* !_DB_CXX_H_ */
