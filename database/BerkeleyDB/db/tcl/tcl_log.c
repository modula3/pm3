/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2002
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id$";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#endif

#include "db_int.h"
#include "dbinc/log.h"
#include "dbinc/tcl_db.h"
#include "dbinc/txn.h"

#ifdef CONFIG_TEST
static int tcl_LogcGet __P((Tcl_Interp *, int, Tcl_Obj * CONST*, DB_LOGC *));

/*
 * tcl_LogArchive --
 *
 * PUBLIC: int tcl_LogArchive __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*, DB_ENV *));
 */
int
tcl_LogArchive(interp, objc, objv, envp)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
	DB_ENV *envp;			/* Environment pointer */
{
	static char *archopts[] = {
		"-arch_abs",	"-arch_data",	"-arch_log",
		NULL
	};
	enum archopts {
		ARCH_ABS,	ARCH_DATA,	ARCH_LOG
	};
	Tcl_Obj *fileobj, *res;
	u_int32_t flag;
	int i, optindex, result, ret;
	char **file, **list;

	result = TCL_OK;
	flag = 0;
	/*
	 * Get the flag index from the object based on the options
	 * defined above.
	 */
	i = 2;
	while (i < objc) {
		if (Tcl_GetIndexFromObj(interp, objv[i],
		    archopts, "option", TCL_EXACT, &optindex) != TCL_OK)
			return (IS_HELP(objv[i]));
		i++;
		switch ((enum archopts)optindex) {
		case ARCH_ABS:
			flag |= DB_ARCH_ABS;
			break;
		case ARCH_DATA:
			flag |= DB_ARCH_DATA;
			break;
		case ARCH_LOG:
			flag |= DB_ARCH_LOG;
			break;
		}
	}
	_debug_check();
	list = NULL;
	ret = envp->log_archive(envp, &list, flag);
	result = _ReturnSetup(interp, ret, DB_RETOK_STD(ret), "log archive");
	if (result == TCL_OK) {
		res = Tcl_NewListObj(0, NULL);
		for (file = list; file != NULL && *file != NULL; file++) {
			fileobj = Tcl_NewStringObj(*file, strlen(*file));
			result = Tcl_ListObjAppendElement(interp, res, fileobj);
			if (result != TCL_OK)
				break;
		}
		Tcl_SetObjResult(interp, res);
	}
	if (list != NULL)
		__os_ufree(envp, list);
	return (result);
}

/*
 * tcl_LogCompare --
 *
 * PUBLIC: int tcl_LogCompare __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*));
 */
int
tcl_LogCompare(interp, objc, objv)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
{
	DB_LSN lsn0, lsn1;
	Tcl_Obj *res;
	int result, ret;

	result = TCL_OK;
	/*
	 * No flags, must be 4 args.
	 */
	if (objc != 4) {
		Tcl_WrongNumArgs(interp, 2, objv, "lsn1 lsn2");
		return (TCL_ERROR);
	}

	result = _GetLsn(interp, objv[2], &lsn0);
	if (result == TCL_ERROR)
		return (result);
	result = _GetLsn(interp, objv[3], &lsn1);
	if (result == TCL_ERROR)
		return (result);

	_debug_check();
	ret = log_compare(&lsn0, &lsn1);
	res = Tcl_NewIntObj(ret);
	Tcl_SetObjResult(interp, res);
	return (result);
}

/*
 * tcl_LogFile --
 *
 * PUBLIC: int tcl_LogFile __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*, DB_ENV *));
 */
int
tcl_LogFile(interp, objc, objv, envp)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
	DB_ENV *envp;			/* Environment pointer */
{
	DB_LSN lsn;
	Tcl_Obj *res;
	size_t len;
	int result, ret;
	char *name;

	result = TCL_OK;
	/*
	 * No flags, must be 3 args.
	 */
	if (objc != 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "lsn");
		return (TCL_ERROR);
	}

	result = _GetLsn(interp, objv[2], &lsn);
	if (result == TCL_ERROR)
		return (result);

	len = MSG_SIZE;
	ret = ENOMEM;
	name = NULL;
	while (ret == ENOMEM) {
		if (name != NULL)
			__os_free(envp, name);
		ret = __os_malloc(envp, len, &name);
		if (ret != 0) {
			Tcl_SetResult(interp, db_strerror(ret), TCL_STATIC);
			break;
		}
		_debug_check();
		ret = envp->log_file(envp, &lsn, name, len);
		len *= 2;
	}
	result = _ReturnSetup(interp, ret, DB_RETOK_STD(ret), "log_file");
	if (ret == 0) {
		res = Tcl_NewStringObj(name, strlen(name));
		Tcl_SetObjResult(interp, res);
	}

	if (name != NULL)
		__os_free(envp, name);

	return (result);
}

/*
 * tcl_LogFlush --
 *
 * PUBLIC: int tcl_LogFlush __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*, DB_ENV *));
 */
int
tcl_LogFlush(interp, objc, objv, envp)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
	DB_ENV *envp;			/* Environment pointer */
{
	DB_LSN lsn, *lsnp;
	int result, ret;

	result = TCL_OK;
	/*
	 * No flags, must be 2 or 3 args.
	 */
	if (objc > 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?lsn?");
		return (TCL_ERROR);
	}

	if (objc == 3) {
		lsnp = &lsn;
		result = _GetLsn(interp, objv[2], &lsn);
		if (result == TCL_ERROR)
			return (result);
	} else
		lsnp = NULL;

	_debug_check();
	ret = envp->log_flush(envp, lsnp);
	result = _ReturnSetup(interp, ret, DB_RETOK_STD(ret), "log_flush");
	return (result);
}

/*
 * tcl_LogGet --
 *
 * PUBLIC: int tcl_LogGet __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*, DB_ENV *));
 */
int
tcl_LogGet(interp, objc, objv, envp)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
	DB_ENV *envp;			/* Environment pointer */
{

	COMPQUIET(objv, NULL);
	COMPQUIET(objc, 0);
	COMPQUIET(envp, NULL);

	Tcl_SetResult(interp, "FAIL: log_get deprecated\n", TCL_STATIC);
	return (TCL_ERROR);
}

/*
 * tcl_LogPut --
 *
 * PUBLIC: int tcl_LogPut __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*, DB_ENV *));
 */
int
tcl_LogPut(interp, objc, objv, envp)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
	DB_ENV *envp;			/* Environment pointer */
{
	static char *logputopts[] = {
		"-flush",
		NULL
	};
	enum logputopts {
		LOGPUT_FLUSH
	};
	DB_LSN lsn;
	DBT data;
	Tcl_Obj *intobj, *res;
	void *dtmp;
	u_int32_t flag;
	int freedata, optindex, result, ret;

	result = TCL_OK;
	flag = 0;
	freedata = 0;
	if (objc < 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?-args? record");
		return (TCL_ERROR);
	}

	/*
	 * Data/record must be the last arg.
	 */
	memset(&data, 0, sizeof(data));
	ret = _CopyObjBytes(interp, objv[objc-1], &dtmp,
	    &data.size, &freedata);
	if (ret != 0) {
		result = _ReturnSetup(interp, ret,
		    DB_RETOK_STD(ret), "log put");
		return (result);
	}
	data.data = dtmp;

	/*
	 * Get the command name index from the object based on the options
	 * defined above.
	 */
	if (objc == 4) {
		if (Tcl_GetIndexFromObj(interp, objv[2],
		    logputopts, "option", TCL_EXACT, &optindex) != TCL_OK) {
			return (IS_HELP(objv[2]));
		}
		switch ((enum logputopts)optindex) {
		case LOGPUT_FLUSH:
			flag = DB_FLUSH;
			break;
		}
	}

	if (result == TCL_ERROR)
		return (result);

	_debug_check();
	ret = envp->log_put(envp, &lsn, &data, flag);
	result = _ReturnSetup(interp, ret, DB_RETOK_STD(ret), "log_put");
	if (result == TCL_ERROR)
		return (result);
	res = Tcl_NewListObj(0, NULL);
	intobj = Tcl_NewLongObj((long)lsn.file);
	result = Tcl_ListObjAppendElement(interp, res, intobj);
	intobj = Tcl_NewLongObj((long)lsn.offset);
	result = Tcl_ListObjAppendElement(interp, res, intobj);
	Tcl_SetObjResult(interp, res);
	if (freedata)
		(void)__os_free(NULL, dtmp);
	return (result);
}
/*
 * tcl_LogStat --
 *
 * PUBLIC: int tcl_LogStat __P((Tcl_Interp *, int,
 * PUBLIC:    Tcl_Obj * CONST*, DB_ENV *));
 */
int
tcl_LogStat(interp, objc, objv, envp)
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
	DB_ENV *envp;			/* Environment pointer */
{
	DB_LOG_STAT *sp;
	Tcl_Obj *res;
	int result, ret;

	result = TCL_OK;
	/*
	 * No args for this.  Error if there are some.
	 */
	if (objc != 2) {
		Tcl_WrongNumArgs(interp, 2, objv, NULL);
		return (TCL_ERROR);
	}
	_debug_check();
	ret = envp->log_stat(envp, &sp, 0);
	result = _ReturnSetup(interp, ret, DB_RETOK_STD(ret), "log stat");
	if (result == TCL_ERROR)
		return (result);

	/*
	 * Have our stats, now construct the name value
	 * list pairs and free up the memory.
	 */
	res = Tcl_NewObj();
	/*
	 * MAKE_STAT_LIST assumes 'res' and 'error' label.
	 */
	MAKE_STAT_LIST("Magic", sp->st_magic);
	MAKE_STAT_LIST("Log file Version", sp->st_version);
	MAKE_STAT_LIST("Region size", sp->st_regsize);
	MAKE_STAT_LIST("Log file mode", sp->st_mode);
	MAKE_STAT_LIST("Log record cache size", sp->st_lg_bsize);
	MAKE_STAT_LIST("Current log file size", sp->st_lg_size);
	MAKE_STAT_LIST("Mbytes written", sp->st_w_mbytes);
	MAKE_STAT_LIST("Bytes written (over Mb)", sp->st_w_bytes);
	MAKE_STAT_LIST("Mbytes written since checkpoint", sp->st_wc_mbytes);
	MAKE_STAT_LIST("Bytes written (over Mb) since checkpoint",
	    sp->st_wc_bytes);
	MAKE_STAT_LIST("Times log written", sp->st_wcount);
	MAKE_STAT_LIST("Times log written because cache filled up",
	    sp->st_wcount_fill);
	MAKE_STAT_LIST("Times log flushed", sp->st_scount);
	MAKE_STAT_LIST("Current log file number", sp->st_cur_file);
	MAKE_STAT_LIST("Current log file offset", sp->st_cur_offset);
	MAKE_STAT_LIST("On-disk log file number", sp->st_disk_file);
	MAKE_STAT_LIST("On-disk log file offset", sp->st_disk_offset);
	MAKE_STAT_LIST("Max commits in a log flush", sp->st_maxcommitperflush);
	MAKE_STAT_LIST("Min commits in a log flush", sp->st_mincommitperflush);
	MAKE_STAT_LIST("Number of region lock waits", sp->st_region_wait);
	MAKE_STAT_LIST("Number of region lock nowaits", sp->st_region_nowait);
	Tcl_SetObjResult(interp, res);
error:
	free(sp);
	return (result);
}

/*
 * logc_Cmd --
 *	Implements the log cursor command.
 *
 * PUBLIC: int logc_Cmd __P((ClientData, Tcl_Interp *, int, Tcl_Obj * CONST*));
 */
int
logc_Cmd(clientData, interp, objc, objv)
	ClientData clientData;		/* Cursor handle */
	Tcl_Interp *interp;		/* Interpreter */
	int objc;			/* How many arguments? */
	Tcl_Obj *CONST objv[];		/* The argument objects */
{
	static char *logccmds[] = {
		"close",
		"get",
		NULL
	};
	enum logccmds {
		LOGCCLOSE,
		LOGCGET
	};
	DB_LOGC *logc;
	DBTCL_INFO *logcip;
	int cmdindex, result, ret;

	Tcl_ResetResult(interp);
	logc = (DB_LOGC *)clientData;
	logcip = _PtrToInfo((void *)logc);
	result = TCL_OK;

	if (objc <= 1) {
		Tcl_WrongNumArgs(interp, 1, objv, "command cmdargs");
		return (TCL_ERROR);
	}
	if (logc == NULL) {
		Tcl_SetResult(interp, "NULL logc pointer", TCL_STATIC);
		return (TCL_ERROR);
	}
	if (logcip == NULL) {
		Tcl_SetResult(interp, "NULL logc info pointer", TCL_STATIC);
		return (TCL_ERROR);
	}

	/*
	 * Get the command name index from the object based on the berkdbcmds
	 * defined above.
	 */
	if (Tcl_GetIndexFromObj(interp, objv[1], logccmds, "command",
	    TCL_EXACT, &cmdindex) != TCL_OK)
		return (IS_HELP(objv[1]));
	switch ((enum logccmds)cmdindex) {
	case LOGCCLOSE:
		/*
		 * No args for this.  Error if there are some.
		 */
		if (objc > 2) {
			Tcl_WrongNumArgs(interp, 2, objv, NULL);
			return (TCL_ERROR);
		}
		_debug_check();
		ret = logc->close(logc, 0);
		result = _ReturnSetup(interp, ret, DB_RETOK_STD(ret),
		    "logc close");
		if (result == TCL_OK) {
			(void)Tcl_DeleteCommand(interp, logcip->i_name);
			_DeleteInfo(logcip);
		}
		break;
	case LOGCGET:
		result = tcl_LogcGet(interp, objc, objv, logc);
		break;
	}
	return (result);
}

static int
tcl_LogcGet(interp, objc, objv, logc)
	Tcl_Interp *interp;
	int objc;
	Tcl_Obj * CONST *objv;
	DB_LOGC *logc;
{
	static char *logcgetopts[] = {
		"-current",
		"-first",
		"-last",
		"-next",
		"-prev",
		"-set",
		NULL
	};
	enum logcgetopts {
		LOGCGET_CURRENT,
		LOGCGET_FIRST,
		LOGCGET_LAST,
		LOGCGET_NEXT,
		LOGCGET_PREV,
		LOGCGET_SET
	};
	DB_LSN lsn;
	DBT data;
	Tcl_Obj *dataobj, *lsnlist, *myobjv[2], *res;
	u_int32_t flag;
	int i, myobjc, optindex, result, ret;

	result = TCL_OK;
	res = NULL;
	flag = 0;

	if (objc < 3) {
		Tcl_WrongNumArgs(interp, 2, objv, "?-args? lsn");
		return (TCL_ERROR);
	}

	/*
	 * Get the command name index from the object based on the options
	 * defined above.
	 */
	i = 2;
	while (i < objc) {
		if (Tcl_GetIndexFromObj(interp, objv[i],
		    logcgetopts, "option", TCL_EXACT, &optindex) != TCL_OK)
			return (IS_HELP(objv[i]));
		i++;
		switch ((enum logcgetopts)optindex) {
		case LOGCGET_CURRENT:
			FLAG_CHECK(flag);
			flag |= DB_CURRENT;
			break;
		case LOGCGET_FIRST:
			FLAG_CHECK(flag);
			flag |= DB_FIRST;
			break;
		case LOGCGET_LAST:
			FLAG_CHECK(flag);
			flag |= DB_LAST;
			break;
		case LOGCGET_NEXT:
			FLAG_CHECK(flag);
			flag |= DB_NEXT;
			break;
		case LOGCGET_PREV:
			FLAG_CHECK(flag);
			flag |= DB_PREV;
			break;
		case LOGCGET_SET:
			FLAG_CHECK(flag);
			flag |= DB_SET;
			if (i == objc) {
				Tcl_WrongNumArgs(interp, 2, objv, "?-set lsn?");
				result = TCL_ERROR;
				break;
			}
			result = _GetLsn(interp, objv[i++], &lsn);
			break;
		}
	}

	if (result == TCL_ERROR)
		return (result);

	memset(&data, 0, sizeof(data));

	_debug_check();
	ret = logc->get(logc, &lsn, &data, flag);

	res = Tcl_NewListObj(0, NULL);
	if (res == NULL)
		goto memerr;

	if (ret == 0) {
		/*
		 * Success.  Set up return list as {LSN data} where LSN
		 * is a sublist {file offset}.
		 */
		myobjc = 2;
		myobjv[0] = Tcl_NewLongObj((long)lsn.file);
		myobjv[1] = Tcl_NewLongObj((long)lsn.offset);
		lsnlist = Tcl_NewListObj(myobjc, myobjv);
		if (lsnlist == NULL)
			goto memerr;

		result = Tcl_ListObjAppendElement(interp, res, lsnlist);
		dataobj = Tcl_NewStringObj(data.data, data.size);
		if (dataobj == NULL) {
			goto memerr;
		}
		result = Tcl_ListObjAppendElement(interp, res, dataobj);
	} else
		result = _ReturnSetup(interp, ret, DB_RETOK_LGGET(ret),
		    "DB_LOGC->get");

	Tcl_SetObjResult(interp, res);

	if (0) {
memerr:		if (res != NULL)
			Tcl_DecrRefCount(res);
		Tcl_SetResult(interp, "allocation failed", TCL_STATIC);
	}

	return (result);
}
#endif
