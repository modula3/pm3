/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2002
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id$
 */

#ifndef _DB_TCL_DB_H_
#define	_DB_TCL_DB_H_

#define	MSG_SIZE 100		/* Message size */

enum INFOTYPE {
    I_ENV, I_DB, I_DBC, I_TXN, I_MP, I_PG, I_LOCK, I_LOGC, I_NDBM, I_MUTEX };

#define	MAX_ID		8	/* Maximum number of sub-id's we need */
#define	DBTCL_PREP	64	/* Size of txn_recover preplist */

#define	DBTCL_DBM	1
#define	DBTCL_NDBM	2

typedef struct _mutex_entry {
	union {
		struct {
			DB_MUTEX	real_m;
			u_int32_t	real_val;
		} r;
		/*
		 * This is here to make sure that each of the mutex structures
		 * are 16-byte aligned, which is required on HP architectures.
		 * The db_mutex_t structure might be >32 bytes itself, or the
		 * real_val might push it over the 32 byte boundary.  The best
		 * we can do is use a 48 byte boundary.
		 */
		char c[48];
	} u;
} _MUTEX_ENTRY;

#define	m	u.r.real_m
#define	val	u.r.real_val

typedef struct _mutex_data {
	DB_ENV		*env;
	REGINFO		 reginfo;
	_MUTEX_ENTRY	*marray;
	size_t		 size;
	u_int32_t	 n_mutex;
} _MUTEX_DATA;

/*
 * Why use a home grown package over the Tcl_Hash functions?
 *
 * We could have implemented the stuff below without maintaining our
 * own list manipulation, efficiently hashing it with the available
 * Tcl functions (Tcl_CreateHashEntry, Tcl_GetHashValue, etc).  I chose
 * not to do so for these reasons:
 *
 * We still need the information below.  Using the hashing only removes
 * us from needing the next/prev pointers.  We still need the structure
 * itself because we need more than one value associated with a widget.
 * We need to keep track of parent pointers for sub-widgets (like cursors)
 * so we can correctly close.  We need to keep track of individual widget's
 * id counters for any sub-widgets they may have.  We need to be able to
 * associate the name/client data outside the scope of the widget.
 *
 * So, is it better to use the hashing rather than
 * the linear list we have now?  I decided against it for the simple reason
 * that to access the structure would require two calls.  The first is
 * Tcl_FindHashEntry(table, key) and then, once we have the entry, we'd
 * have to do Tcl_GetHashValue(entry) to get the pointer of the structure.
 *
 * I believe the number of simultaneous DB widgets in existence at one time
 * is not going to be that large (more than several dozen) such that
 * linearly searching the list is not going to impact performance in a
 * noticable way.  Should performance be impacted due to the size of the
 * info list, then perhaps it is time to revisit this decision.
 */
typedef struct dbtcl_info {
	LIST_ENTRY(dbtcl_info) entries;
	Tcl_Interp *i_interp;
	char *i_name;
	enum INFOTYPE i_type;
	union infop {
		DB_ENV *envp;
		void *anyp;
		DB *dbp;
		DBC *dbcp;
		DB_TXN *txnp;
		DB_MPOOLFILE *mp;
		DB_LOCK *lock;
		_MUTEX_DATA *mutex;
		DB_LOGC *logc;
	} un;
	union data {
		int anydata;
		db_pgno_t pgno;
		u_int32_t lockid;
	} und;
	union data2 {
		int anydata;
		size_t pagesz;
	} und2;
	DBT i_lockobj;
	FILE *i_err;
	char *i_errpfx;

	/* Callbacks--Tcl_Objs containing proc names */
	Tcl_Obj *i_btcompare;
	Tcl_Obj *i_dupcompare;
	Tcl_Obj *i_hashproc;
	Tcl_Obj *i_rep_send;
	Tcl_Obj *i_second_call;

	/* Environment ID for the i_rep_send callback. */
	Tcl_Obj *i_rep_eid;

	struct dbtcl_info *i_parent;
	int	i_otherid[MAX_ID];
} DBTCL_INFO;

#define	i_anyp un.anyp
#define	i_pagep un.anyp
#define	i_envp un.envp
#define	i_dbp un.dbp
#define	i_dbcp un.dbcp
#define	i_txnp un.txnp
#define	i_mp un.mp
#define	i_lock un.lock
#define	i_mutex un.mutex
#define	i_logc un.logc

#define	i_data und.anydata
#define	i_pgno und.pgno
#define	i_locker und.lockid
#define	i_data2 und2.anydata
#define	i_pgsz und2.pagesz

#define	i_envtxnid i_otherid[0]
#define	i_envmpid i_otherid[1]
#define	i_envlockid i_otherid[2]
#define	i_envmutexid i_otherid[3]
#define	i_envlogcid i_otherid[4]

#define	i_mppgid  i_otherid[0]

#define	i_dbdbcid i_otherid[0]

extern int __debug_on, __debug_print, __debug_stop, __debug_test;

typedef struct dbtcl_global {
	LIST_HEAD(infohead, dbtcl_info) g_infohead;
} DBTCL_GLOBAL;
#define	__db_infohead __dbtcl_global.g_infohead

extern DBTCL_GLOBAL __dbtcl_global;

#define	NAME_TO_ENV(name) (DB_ENV *)_NameToPtr((name))
#define	NAME_TO_DB(name) (DB *)_NameToPtr((name))
#define	NAME_TO_DBC(name) (DBC *)_NameToPtr((name))
#define	NAME_TO_TXN(name) (DB_TXN *)_NameToPtr((name))
#define	NAME_TO_MP(name) (DB_MPOOLFILE *)_NameToPtr((name))
#define	NAME_TO_LOCK(name) (DB_LOCK *)_NameToPtr((name))

/*
 * MAKE_STAT_LIST appends a {name value} pair to a result list
 * that MUST be called 'res' that is a Tcl_Obj * in the local
 * function.  This macro also assumes a label "error" to go to
 * in the even of a Tcl error.  For stat functions this will
 * typically go before the "free" function to free the stat structure
 * returned by DB.
 */
#define	MAKE_STAT_LIST(s,v)					\
do {								\
	result = _SetListElemInt(interp, res, (s), (v));	\
	if (result != TCL_OK)					\
		goto error;					\
} while (0)

/*
 * MAKE_STAT_LSN appends a {name {LSNfile LSNoffset}} pair to a result list
 * that MUST be called 'res' that is a Tcl_Obj * in the local
 * function.  This macro also assumes a label "error" to go to
 * in the even of a Tcl error.  For stat functions this will
 * typically go before the "free" function to free the stat structure
 * returned by DB.
 */
#define	MAKE_STAT_LSN(s, lsn)						\
do {									\
	myobjc = 2;							\
	myobjv[0] = Tcl_NewLongObj((long)(lsn)->file);			\
	myobjv[1] = Tcl_NewLongObj((long)(lsn)->offset);		\
	lsnlist = Tcl_NewListObj(myobjc, myobjv);			\
	myobjc = 2;							\
	myobjv[0] = Tcl_NewStringObj((s), strlen(s));			\
	myobjv[1] = lsnlist;						\
	thislist = Tcl_NewListObj(myobjc, myobjv);			\
	result = Tcl_ListObjAppendElement(interp, res, thislist);	\
	if (result != TCL_OK)						\
		goto error;						\
} while (0)

/*
 * MAKE_STAT_STRLIST appends a {name string} pair to a result list
 * that MUST be called 'res' that is a Tcl_Obj * in the local
 * function.  This macro also assumes a label "error" to go to
 * in the even of a Tcl error.  For stat functions this will
 * typically go before the "free" function to free the stat structure
 * returned by DB.
 */
#define	MAKE_STAT_STRLIST(s,s1)					\
do {								\
	result = _SetListElem(interp, res, (s), strlen(s),	\
	    (s1), strlen(s1));					\
	if (result != TCL_OK)					\
		goto error;					\
} while (0)

/*
 * FLAG_CHECK checks that the given flag is not set yet.
 * If it is, it sets up an error message.
 */
#define	FLAG_CHECK(flag)					\
do {								\
	if ((flag) != 0) {					\
		Tcl_SetResult(interp,				\
		    " Only 1 policy can be specified.\n",	\
		    TCL_STATIC);				\
		result = TCL_ERROR;				\
		break;						\
	}							\
} while (0)

/*
 * FLAG_CHECK2 checks that the given flag is not set yet or is
 * only set to the given allowed value.
 * If it is, it sets up an error message.
 */
#define	FLAG_CHECK2(flag,val)					\
do {								\
	if (((flag) & ~(val)) != 0) {				\
		Tcl_SetResult(interp,				\
		    " Only 1 policy can be specified.\n",	\
		    TCL_STATIC);				\
		result = TCL_ERROR;				\
		break;						\
	}							\
} while (0)

/*
 * IS_HELP checks whether the arg we bombed on is -?, which is a help option.
 * If it is, we return TCL_OK (but leave the result set to whatever
 * Tcl_GetIndexFromObj says, which lists all the valid options.  Otherwise
 * return TCL_ERROR.
 */
#define	IS_HELP(s)						\
    (strcmp(Tcl_GetStringFromObj(s,NULL), "-?") == 0) ? TCL_OK : TCL_ERROR

#include "dbinc_auto/tcl_ext.h"
#endif /* !_DB_TCL_DB_H_ */
