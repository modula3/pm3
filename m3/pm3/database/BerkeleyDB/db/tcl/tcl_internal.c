/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2001
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
#include "dbinc/tcl_db.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"
#include "dbinc_auto/db_ext.h"

/*
 *
 * internal.c --
 *
 *	This file contains internal functions we need to maintain
 *	state for our Tcl interface.
 *
 *	NOTE: This all uses a linear linked list.  If we end up with
 *	too many info structs such that this is a performance hit, it
 *	should be redone using hashes or a list per type.  The assumption
 *	is that the user won't have more than a few dozen info structs
 *	in operation at any given point in time.  Even a complicated
 *	application with a few environments, nested transactions, locking,
 *	and several databases open, using cursors should not have a
 *	negative performance impact, in terms of searching the list to
 *	get/manipulate the info structure.
 */

/*
 * Prototypes for procedures defined later in this file:
 */
static void tcl_flag_callback __P((u_int32_t, const FN *, void *));

/*
 * Private structure type used to pass both an interp and an object into
 * a callback's single void *.
 */
struct __tcl_callback_bundle {
	Tcl_Interp *interp;
	Tcl_Obj *obj;
};

#define	GLOB_CHAR(c)	((c) == '*' || (c) == '?')

/*
 * PUBLIC: DBTCL_INFO *_NewInfo __P((Tcl_Interp *,
 * PUBLIC:    void *, char *, enum INFOTYPE));
 *
 * _NewInfo --
 *
 * This function will create a new info structure and fill it in
 * with the name and pointer, id and type.
 */
DBTCL_INFO *
_NewInfo(interp, anyp, name, type)
	Tcl_Interp *interp;
	void *anyp;
	char *name;
	enum INFOTYPE type;
{
	DBTCL_INFO *p;
	int i, ret;

	if ((ret = __os_malloc(NULL, sizeof(DBTCL_INFO), &p)) != 0) {
		Tcl_SetResult(interp, db_strerror(ret), TCL_STATIC);
		return (NULL);
	}

	if ((ret = __os_strdup(NULL, name, &p->i_name)) != 0) {
		Tcl_SetResult(interp, db_strerror(ret), TCL_STATIC);
		__os_free(NULL, p);
		return (NULL);
	}
	p->i_interp = interp;
	p->i_anyp = anyp;
	p->i_data = 0;
	p->i_data2 = 0;
	p->i_type = type;
	p->i_parent = NULL;
	p->i_err = NULL;
	p->i_errpfx = NULL;
	p->i_lockobj.data = NULL;
	p->i_btcompare = NULL;
	p->i_dupcompare = NULL;
	p->i_hashproc = NULL;
	p->i_second_call = NULL;
	p->i_rep_eid = NULL;
	p->i_rep_send = NULL;
	for (i = 0; i < MAX_ID; i++)
		p->i_otherid[i] = 0;

	LIST_INSERT_HEAD(&__db_infohead, p, entries);
	return (p);
}

/*
 * PUBLIC: void *_NameToPtr __P((CONST char *));
 */
void	*
_NameToPtr(name)
	CONST char *name;
{
	DBTCL_INFO *p;

	for (p = LIST_FIRST(&__db_infohead); p != NULL;
	    p = LIST_NEXT(p, entries))
		if (strcmp(name, p->i_name) == 0)
			return (p->i_anyp);
	return (NULL);
}

/*
 * PUBLIC: DBTCL_INFO *_PtrToInfo __P((CONST void *));
 */
DBTCL_INFO *
_PtrToInfo(ptr)
	CONST void *ptr;
{
	DBTCL_INFO *p;

	for (p = LIST_FIRST(&__db_infohead); p != NULL;
	    p = LIST_NEXT(p, entries))
		if (p->i_anyp == ptr)
			return (p);
	return (NULL);
}

/*
 * PUBLIC: DBTCL_INFO *_NameToInfo __P((CONST char *));
 */
DBTCL_INFO *
_NameToInfo(name)
	CONST char *name;
{
	DBTCL_INFO *p;

	for (p = LIST_FIRST(&__db_infohead); p != NULL;
	    p = LIST_NEXT(p, entries))
		if (strcmp(name, p->i_name) == 0)
			return (p);
	return (NULL);
}

/*
 * PUBLIC: void  _SetInfoData __P((DBTCL_INFO *, void *));
 */
void
_SetInfoData(p, data)
	DBTCL_INFO *p;
	void *data;
{
	if (p == NULL)
		return;
	p->i_anyp = data;
	return;
}

/*
 * PUBLIC: void  _DeleteInfo __P((DBTCL_INFO *));
 */
void
_DeleteInfo(p)
	DBTCL_INFO *p;
{
	if (p == NULL)
		return;
	LIST_REMOVE(p, entries);
	if (p->i_lockobj.data != NULL)
		__os_free(NULL, p->i_lockobj.data);
	if (p->i_err != NULL) {
		fclose(p->i_err);
		p->i_err = NULL;
	}
	if (p->i_errpfx != NULL)
		__os_free(NULL, p->i_errpfx);
	if (p->i_btcompare != NULL)
		Tcl_DecrRefCount(p->i_btcompare);
	if (p->i_dupcompare != NULL)
		Tcl_DecrRefCount(p->i_dupcompare);
	if (p->i_hashproc != NULL)
		Tcl_DecrRefCount(p->i_hashproc);
	if (p->i_second_call != NULL)
		Tcl_DecrRefCount(p->i_second_call);
	if (p->i_rep_eid != NULL)
		Tcl_DecrRefCount(p->i_rep_eid);
	if (p->i_rep_send != NULL)
		Tcl_DecrRefCount(p->i_rep_send);
	__os_free(NULL, p->i_name);
	__os_free(NULL, p);

	return;
}

/*
 * PUBLIC: int _SetListElem __P((Tcl_Interp *,
 * PUBLIC:    Tcl_Obj *, void *, int, void *, int));
 */
int
_SetListElem(interp, list, elem1, e1cnt, elem2, e2cnt)
	Tcl_Interp *interp;
	Tcl_Obj *list;
	void *elem1, *elem2;
	int e1cnt, e2cnt;
{
	Tcl_Obj *myobjv[2], *thislist;
	int myobjc;

	myobjc = 2;
	myobjv[0] = Tcl_NewByteArrayObj((u_char *)elem1, e1cnt);
	myobjv[1] = Tcl_NewByteArrayObj((u_char *)elem2, e2cnt);
	thislist = Tcl_NewListObj(myobjc, myobjv);
	if (thislist == NULL)
		return (TCL_ERROR);
	return (Tcl_ListObjAppendElement(interp, list, thislist));

}

/*
 * PUBLIC: int _SetListElemInt __P((Tcl_Interp *, Tcl_Obj *, void *, int));
 */
int
_SetListElemInt(interp, list, elem1, elem2)
	Tcl_Interp *interp;
	Tcl_Obj *list;
	void *elem1;
	int elem2;
{
	Tcl_Obj *myobjv[2], *thislist;
	int myobjc;

	myobjc = 2;
	myobjv[0] = Tcl_NewByteArrayObj((u_char *)elem1, strlen((char *)elem1));
	myobjv[1] = Tcl_NewIntObj(elem2);
	thislist = Tcl_NewListObj(myobjc, myobjv);
	if (thislist == NULL)
		return (TCL_ERROR);
	return (Tcl_ListObjAppendElement(interp, list, thislist));
}

/*
 * PUBLIC: int _SetListRecnoElem __P((Tcl_Interp *, Tcl_Obj *,
 * PUBLIC:     db_recno_t, u_char *, int));
 */
int
_SetListRecnoElem(interp, list, elem1, elem2, e2size)
	Tcl_Interp *interp;
	Tcl_Obj *list;
	db_recno_t elem1;
	u_char *elem2;
	int e2size;
{
	Tcl_Obj *myobjv[2], *thislist;
	int myobjc;

	myobjc = 2;
	myobjv[0] = Tcl_NewLongObj((long)elem1);
	myobjv[1] = Tcl_NewByteArrayObj(elem2, e2size);
	thislist = Tcl_NewListObj(myobjc, myobjv);
	if (thislist == NULL)
		return (TCL_ERROR);
	return (Tcl_ListObjAppendElement(interp, list, thislist));

}

/*
 * _Set3DBTList --
 *	This is really analogous to both _SetListElem and
 *	_SetListRecnoElem--it's used for three-DBT lists returned by
 *	DB->pget and DBC->pget().  We'd need a family of four functions
 *	to handle all the recno/non-recno cases, however, so we make
 *	this a little more aware of the internals and do the logic inside.
 *
 *	XXX
 *	One of these days all these functions should probably be cleaned up
 *	to eliminate redundancy and bring them into the standard DB
 *	function namespace.
 *
 * PUBLIC: int _Set3DBTList __P((Tcl_Interp *, Tcl_Obj *, DBT *, int,
 * PUBLIC:     DBT *, int, DBT *));
 */
int
_Set3DBTList(interp, list, elem1, is1recno, elem2, is2recno, elem3)
	Tcl_Interp *interp;
	Tcl_Obj *list;
	DBT *elem1, *elem2, *elem3;
	int is1recno, is2recno;
{

	Tcl_Obj *myobjv[3], *thislist;

	if (is1recno)
		myobjv[0] = Tcl_NewLongObj((long)*(db_recno_t *)elem1->data);
	else
		myobjv[0] =
		    Tcl_NewByteArrayObj((u_char *)elem1->data, elem1->size);

	if (is2recno)
		myobjv[1] = Tcl_NewLongObj((long)*(db_recno_t *)elem2->data);
	else
		myobjv[1] =
		    Tcl_NewByteArrayObj((u_char *)elem2->data, elem2->size);

	myobjv[2] = Tcl_NewByteArrayObj((u_char *)elem3->data, elem3->size);

	thislist = Tcl_NewListObj(3, myobjv);

	if (thislist == NULL)
		return (TCL_ERROR);
	return (Tcl_ListObjAppendElement(interp, list, thislist));
}

/*
 * _SetMultiList -- build a list for return from multiple get.
 *
 * PUBLIC: int _SetMultiList __P((Tcl_Interp *,
 * PUBLIC:	    Tcl_Obj *, DBT *, DBT*, int, int));
 */
int
_SetMultiList(interp, list, key, data, type, flag)
	Tcl_Interp *interp;
	Tcl_Obj *list;
	DBT *key, *data;
	int type, flag;
{
	db_recno_t recno;
	u_int32_t dlen, klen;
	int result;
	void *pointer, *dp, *kp;

	recno = 0;
	dlen = 0;
	kp = NULL;

	DB_MULTIPLE_INIT(pointer, data);
	result = TCL_OK;

	if (type == DB_RECNO || type == DB_QUEUE)
		recno = *(db_recno_t *) key->data;
	else
		kp = key->data;
	klen = key->size;
	do {
		if (flag & DB_MULTIPLE_KEY) {
			if (type == DB_RECNO || type == DB_QUEUE)
				DB_MULTIPLE_RECNO_NEXT(pointer,
				    data, recno, dp, dlen);
			else
				DB_MULTIPLE_KEY_NEXT(pointer,
				    data, kp, klen, dp, dlen);
		} else
			DB_MULTIPLE_NEXT(pointer, data, dp, dlen);

		if (pointer == NULL)
			break;

		if (type == DB_RECNO || type == DB_QUEUE) {
			result =
			    _SetListRecnoElem(interp, list, recno, dp, dlen);
			recno++;
		} else
			result = _SetListElem(interp, list, kp, klen, dp, dlen);
	} while (result == TCL_OK);

	return (result);
}
/*
 * PUBLIC: int _GetGlobPrefix __P((char *, char **));
 */
int
_GetGlobPrefix(pattern, prefix)
	char *pattern;
	char **prefix;
{
	int i, j;
	char *p;

	/*
	 * Duplicate it, we get enough space and most of the work is done.
	 */
	if (__os_strdup(NULL, pattern, prefix) != 0)
		return (1);

	p = *prefix;
	for (i = 0, j = 0; p[i] && !GLOB_CHAR(p[i]); i++, j++)
		/*
		 * Check for an escaped character and adjust
		 */
		if (p[i] == '\\' && p[i+1]) {
			p[j] = p[i+1];
			i++;
		} else
			p[j] = p[i];
	p[j] = 0;
	return (0);
}

/*
 * PUBLIC: int _ReturnSetup __P((Tcl_Interp *, int, int, char *));
 */
int
_ReturnSetup(interp, ret, ok, errmsg)
	Tcl_Interp *interp;
	int ret, ok;
	char *errmsg;
{
	char *msg;

	if (ret > 0)
		return (_ErrorSetup(interp, ret, errmsg));

	/*
	 * We either have success or a DB error.  If a DB error, set up the
	 * string.  We return an error if not one of the errors we catch.
	 * If anyone wants to reset the result to return anything different,
	 * then the calling function is responsible for doing so via
	 * Tcl_ResetResult or another Tcl_SetObjResult.
	 */
	if (ret == 0) {
		Tcl_SetResult(interp, "0", TCL_STATIC);
		return (TCL_OK);
	}

	msg = db_strerror(ret);
	Tcl_AppendResult(interp, msg, NULL);

	if (ok)
		return (TCL_OK);
	else {
		Tcl_SetErrorCode(interp, "BerkeleyDB", msg, NULL);
		return (TCL_ERROR);
	}
}

/*
 * PUBLIC: int _ErrorSetup __P((Tcl_Interp *, int, char *));
 */
int
_ErrorSetup(interp, ret, errmsg)
	Tcl_Interp *interp;
	int ret;
	char *errmsg;
{
	Tcl_SetErrno(ret);
	Tcl_AppendResult(interp, errmsg, ":", Tcl_PosixError(interp), NULL);
	return (TCL_ERROR);
}

/*
 * PUBLIC: void _ErrorFunc __P((CONST char *, char *));
 */
void
_ErrorFunc(pfx, msg)
	CONST char *pfx;
	char *msg;
{
	DBTCL_INFO *p;
	Tcl_Interp *interp;
	int size;
	char *err;

	p = _NameToInfo(pfx);
	if (p == NULL)
		return;
	interp = p->i_interp;

	size = strlen(pfx) + strlen(msg) + 4;
	/*
	 * If we cannot allocate enough to put together the prefix
	 * and message then give them just the message.
	 */
	if (__os_malloc(NULL, size, &err) != 0) {
		Tcl_AddErrorInfo(interp, msg);
		Tcl_AppendResult(interp, msg, "\n", NULL);
		return;
	}
	snprintf(err, size, "%s: %s", pfx, msg);
	Tcl_AddErrorInfo(interp, err);
	Tcl_AppendResult(interp, err, "\n", NULL);
	__os_free(NULL, err);
	return;
}

#define	INVALID_LSNMSG "Invalid LSN with %d parts. Should have 2.\n"

/*
 * PUBLIC: int _GetLsn __P((Tcl_Interp *, Tcl_Obj *, DB_LSN *));
 */
int
_GetLsn(interp, obj, lsn)
	Tcl_Interp *interp;
	Tcl_Obj *obj;
	DB_LSN *lsn;
{
	Tcl_Obj **myobjv;
	char msg[MSG_SIZE];
	int myobjc, result;
	u_int32_t tmp;

	result = Tcl_ListObjGetElements(interp, obj, &myobjc, &myobjv);
	if (result == TCL_ERROR)
		return (result);
	if (myobjc != 2) {
		result = TCL_ERROR;
		snprintf(msg, MSG_SIZE, INVALID_LSNMSG, myobjc);
		Tcl_SetResult(interp, msg, TCL_VOLATILE);
		return (result);
	}
	result = _GetUInt32(interp, myobjv[0], &tmp);
	if (result == TCL_ERROR)
		return (result);
	lsn->file = tmp;
	result = _GetUInt32(interp, myobjv[1], &tmp);
	lsn->offset = tmp;
	return (result);
}

/*
 * _GetUInt32 --
 *	Get a u_int32_t from a Tcl object.  Tcl_GetIntFromObj does the
 * right thing most of the time, but on machines where a long is 8 bytes
 * and an int is 4 bytes, it errors on integers between the maximum
 * int32_t and the maximum u_int32_t.  This is correct, but we generally
 * want a u_int32_t in the end anyway, so we use Tcl_GetLongFromObj and do
 * the bounds checking ourselves.
 *
 * This code looks much like Tcl_GetIntFromObj, only with a different
 * bounds check.  It's essentially Tcl_GetUnsignedIntFromObj, which
 * unfortunately doesn't exist.
 *
 * PUBLIC: int _GetUInt32 __P((Tcl_Interp *, Tcl_Obj *, u_int32_t *));
 */
int
_GetUInt32(interp, obj, resp)
	Tcl_Interp *interp;
	Tcl_Obj *obj;
	u_int32_t *resp;
{
	int result;
	long ltmp;

	result = Tcl_GetLongFromObj(interp, obj, &ltmp);
	if (result != TCL_OK)
		return (result);

	if ((unsigned long)ltmp != (u_int32_t)ltmp) {
		if (interp != NULL) {
			Tcl_ResetResult(interp);
			Tcl_AppendToObj(Tcl_GetObjResult(interp),
			    "integer value too large for u_int32_t", -1);
		}
		return (TCL_ERROR);
	}

	*resp = (u_int32_t)ltmp;
	return (TCL_OK);
}

/*
 * tcl_flag_callback --
 *	Callback for db_pr.c functions that contain the FN struct mapping
 * flag values to meaningful strings.  This function appends a Tcl_Obj
 * containing each pertinent flag string to the specified Tcl list.
 */
static void
tcl_flag_callback(flags, fn, vtcbp)
	u_int32_t flags;
	const FN *fn;
	void *vtcbp;
{
	const FN *fnp;
	Tcl_Interp *interp;
	Tcl_Obj *newobj, *listobj;
	int result;
	struct __tcl_callback_bundle *tcbp;

	tcbp = (struct __tcl_callback_bundle *)vtcbp;
	interp = tcbp->interp;
	listobj = tcbp->obj;

	for (fnp = fn; fnp->mask != 0; ++fnp)
		if (LF_ISSET(fnp->mask)) {
			newobj = Tcl_NewStringObj(fnp->name, strlen(fnp->name));
			result =
			    Tcl_ListObjAppendElement(interp, listobj, newobj);

			/*
			 * Tcl_ListObjAppendElement is defined to return TCL_OK
			 * unless listobj isn't actually a list (or convertible
			 * into one).  If this is the case, we screwed up badly
			 * somehow.
			 */
			DB_ASSERT(result == TCL_OK);
		}
}

/*
 * _GetFlagsList --
 *	Get a new Tcl object, containing a list of the string values
 * associated with a particular set of flag values, given a function
 * that can extract the right names for the right flags.
 *
 * PUBLIC: Tcl_Obj *_GetFlagsList __P((Tcl_Interp *, u_int32_t,
 * PUBLIC:     void (*)(u_int32_t, void *,
 * PUBLIC:     void (*)(u_int32_t, const FN *, void *))));
 */
Tcl_Obj *
_GetFlagsList(interp, flags, func)
	Tcl_Interp *interp;
	u_int32_t flags;
	void (*func)
	    __P((u_int32_t, void *, void (*)(u_int32_t, const FN *, void *)));
{
	Tcl_Obj *newlist;
	struct __tcl_callback_bundle tcb;

	newlist = Tcl_NewObj();

	memset(&tcb, 0, sizeof(tcb));
	tcb.interp = interp;
	tcb.obj = newlist;

	func(flags, &tcb, tcl_flag_callback);

	return (newlist);
}

int __debug_stop, __debug_on, __debug_print, __debug_test;

/*
 * PUBLIC: void _debug_check  __P((void));
 */
void
_debug_check()
{
	if (__debug_on == 0)
		return;

	if (__debug_print != 0) {
		printf("\r%7d:", __debug_on);
		fflush(stdout);
	}
	if (__debug_on++ == __debug_test || __debug_stop)
		__db_loadme();
}

/*
 * XXX
 * Tcl 8.1+ Tcl_GetByteArrayFromObj/Tcl_GetIntFromObj bug.
 *
 * There is a bug in Tcl 8.1+ and byte arrays in that if it happens
 * to use an object as both a byte array and something else like
 * an int, and you've done a Tcl_GetByteArrayFromObj, then you
 * do a Tcl_GetIntFromObj, your memory is deleted.
 *
 * Workaround is for all byte arrays we want to use, if it can be
 * represented as an integer, we copy it so that we don't lose the
 * memory.
 */
/*
 * PUBLIC: int _CopyObjBytes  __P((Tcl_Interp *, Tcl_Obj *obj, void **,
 * PUBLIC:     u_int32_t *, int *));
 */
int
_CopyObjBytes(interp, obj, newp, sizep, freep)
	Tcl_Interp *interp;
	Tcl_Obj *obj;
	void **newp;
	u_int32_t *sizep;
	int *freep;
{
	void *tmp, *new;
	int i, len, ret;

	/*
	 * If the object is not an int, then just return the byte
	 * array because it won't be transformed out from under us.
	 * If it is a number, we need to copy it.
	 */
	*freep = 0;
	ret = Tcl_GetIntFromObj(interp, obj, &i);
	tmp = Tcl_GetByteArrayFromObj(obj, &len);
	*sizep = len;
	if (ret == TCL_ERROR) {
		Tcl_ResetResult(interp);
		*newp = tmp;
		return (0);
	}

	/*
	 * If we get here, we have an integer that might be reused
	 * at some other point so we cannot count on GetByteArray
	 * keeping our pointer valid.
	 */
	if ((ret = __os_malloc(NULL, len, &new)) != 0)
		return (ret);
	memcpy(new, tmp, len);
	*newp = new;
	*freep = 1;
	return (0);
}
