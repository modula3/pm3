/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2002
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id$";
#endif /* not lint */

#include "db_int.h"

/*
 * __os_rename --
 *	Rename a file.
 */
int
__os_rename(dbenv, oldname, newname, flags)
	DB_ENV *dbenv;
	const char *oldname, *newname;
	u_int32_t flags;
{
	int ret;

	ret = 0;
	if (DB_GLOBAL(j_rename) != NULL) {
		if (DB_GLOBAL(j_rename)(oldname, newname) == -1)
			ret = __os_get_errno();
		goto done;
	}

	if (!MoveFile(oldname, newname))
		ret = __os_win32_errno();

	if (ret == EEXIST) {
		ret = 0;
		if (__os_is_winnt()) {
			if (!MoveFileEx(
			    oldname, newname, MOVEFILE_REPLACE_EXISTING))
				ret = __os_win32_errno();
		} else {
			/*
			 * There is no MoveFileEx for Win9x/Me, so we have to
			 * do the best we can.  Note that MoveFile returns 1
			 * if the names refer to the same file, so we don't
			 * need to check that here.
			 */
			(void)DeleteFile(newname);
			if (!MoveFile(oldname, newname))
				ret = __os_win32_errno();
		}
	}

done:	if (ret != 0 && flags == 0)
		__db_err(dbenv,
		    "Rename %s %s: %s", oldname, newname, strerror(ret));

	return (ret);
}
