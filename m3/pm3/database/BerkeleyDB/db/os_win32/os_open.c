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

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"

/*
 * __os_open --
 *	Open a file descriptor.
 */
int
__os_open(dbenv, name, flags, mode, fhp)
	DB_ENV *dbenv;
	const char *name;
	u_int32_t flags;
	int mode;
	DB_FH *fhp;
{
	DWORD bytesWritten;
	u_int32_t log_size, pagesize, sectorsize;
	int access, attr, oflags, share, createflag;
	int ret, nrepeat;
	char *drive, dbuf[4]; /* <letter><colon><slosh><nul> */

#ifdef DIAGNOSTIC
#define	OKFLAGS								\
	(DB_OSO_CREATE | DB_OSO_DIRECT | DB_OSO_EXCL | DB_OSO_LOG |	\
	 DB_OSO_RDONLY | DB_OSO_REGION | DB_OSO_SEQ | DB_OSO_TEMP |	\
	 DB_OSO_TRUNC)
	if ((ret = __db_fchk(dbenv, "__os_open", flags, OKFLAGS)) != 0)
		return (ret);
#endif

	/*
	 * The "public" interface to the __os_open routine passes around POSIX
	 * 1003.1 flags, not DB flags.  If the user has defined their own open
	 * interface, use the POSIX flags.
	 */
	if (DB_GLOBAL(j_open) != NULL) {
		oflags = O_BINARY | O_NOINHERIT;

		if (LF_ISSET(DB_OSO_CREATE))
			oflags |= O_CREAT;

		if (LF_ISSET(DB_OSO_EXCL))
			oflags |= O_EXCL;

		if (LF_ISSET(DB_OSO_RDONLY))
			oflags |= O_RDONLY;
		else
			oflags |= O_RDWR;

		if (LF_ISSET(DB_OSO_SEQ))
			oflags |= _O_SEQUENTIAL;
		else
			oflags |= _O_RANDOM;

		if (LF_ISSET(DB_OSO_TEMP))
			oflags |= _O_TEMPORARY;

		if (LF_ISSET(DB_OSO_TRUNC))
			oflags |= O_TRUNC;

		return (__os_openhandle(dbenv, name, oflags, mode, fhp));
	}

	ret = 0;

	if (LF_ISSET(DB_OSO_LOG))
		log_size = fhp->log_size;			/* XXX: Gag. */

	pagesize = fhp->pagesize;

	memset(fhp, 0, sizeof(*fhp));
	fhp->fd = -1;

	/*
	 * Otherwise, use the Windows/32 CreateFile interface so that we can
	 * play magic games with log files to get data flush effects similar
	 * to the POSIX O_DSYNC flag.
	 *
	 * !!!
	 * We currently ignore the 'mode' argument.  It would be possible
	 * to construct a set of security attributes that we could pass to
	 * CreateFile that would accurately represents the mode.  In worst
	 * case, this would require looking up user and all group names and
	 * creating an entry for each.  Alternatively, we could call the
	 * _chmod (partial emulation) function after file creation, although
	 * this leaves us with an obvious race.  However, these efforts are
	 * largely meaningless on FAT, the most common file system, which
	 * only has a "readable" and "writeable" flag, applying to all users.
	 */
	access = GENERIC_READ;
	if (!LF_ISSET(DB_OSO_RDONLY))
		access |= GENERIC_WRITE;

	share = FILE_SHARE_READ | FILE_SHARE_WRITE;
	attr = FILE_ATTRIBUTE_NORMAL;

	/*
	 * Reproduce POSIX 1003.1 semantics: if O_CREATE and O_EXCL are both
	 * specified, fail, returning EEXIST, unless we create the file.
	 */
	if (LF_ISSET(DB_OSO_CREATE) && LF_ISSET(DB_OSO_EXCL))
		createflag = CREATE_NEW;	/* create only if !exist*/
	else if (!LF_ISSET(DB_OSO_CREATE) && LF_ISSET(DB_OSO_TRUNC))
		createflag = TRUNCATE_EXISTING; /* truncate, fail if !exist */
	else if (LF_ISSET(DB_OSO_TRUNC))
		createflag = CREATE_ALWAYS;	/* create and truncate */
	else if (LF_ISSET(DB_OSO_CREATE))
		createflag = OPEN_ALWAYS;	/* open or create */
	else
		createflag = OPEN_EXISTING;	/* open only if existing */

	if (LF_ISSET(DB_OSO_LOG)) {
		F_SET(fhp, DB_FH_NOSYNC);
		attr |= FILE_FLAG_WRITE_THROUGH;
	}

	if (LF_ISSET(DB_OSO_SEQ))
		attr |= FILE_FLAG_SEQUENTIAL_SCAN;
	else
		attr |= FILE_FLAG_RANDOM_ACCESS;

	if (LF_ISSET(DB_OSO_TEMP))
		attr |= FILE_FLAG_DELETE_ON_CLOSE;

	/*
	 * We can turn filesystem buffering off if the page size is a
	 * multiple of the disk's sector size. To find the sector size,
	 * we call GetDiskFreeSpace, which expects a drive name like "d:\\"
	 * or NULL for the current disk (i.e., a relative path)
	 */
	if (LF_ISSET(DB_OSO_DIRECT) && pagesize != 0 && name[0] != '\0') {
		if (name[1] == ':') {
			drive = dbuf;
			snprintf(dbuf, sizeof(dbuf), "%c:\\", name[0]);
		} else
			drive = NULL;

		if (GetDiskFreeSpace(drive, NULL, &sectorsize, NULL, NULL) &&
		    pagesize % sectorsize == 0)
			attr |= FILE_FLAG_NO_BUFFERING;
	}

	for (nrepeat = 1;; ++nrepeat) {
		fhp->handle =
		    CreateFile(name, access, share, NULL, createflag, attr, 0);
		if (fhp->handle == INVALID_HANDLE_VALUE) {
			/*
			 * If it's a "temporary" error, we retry up to 3 times,
			 * waiting up to 12 seconds.  While it's not a problem
			 * if we can't open a database, an inability to open a
			 * log file is cause for serious dismay.
			 */
			ret = __os_win32_errno();
			if ((ret != ENFILE && ret != EMFILE && ret != ENOSPC) ||
			    nrepeat > 3)
				goto err;

			(void)__os_sleep(dbenv, nrepeat * 2, 0);
		} else
			break;
	}

	/*
	 * Special handling needed for log files.  To get Windows to not update
	 * the MFT metadata on each write, extend the file to its maximum size.
	 * Windows will allocate all the data blocks and store them in the MFT
	 * (inode) area.  In addition, flush the MFT area to disk.
	 * This strategy only works for Win/NT; Win/9X does not
	 * guarantee that the logs will be zero filled.
	 */
	if (LF_ISSET(DB_OSO_LOG) && log_size != 0 && __os_is_winnt()) {
		if (SetFilePointer(fhp->handle,
		    log_size - 1, NULL, FILE_BEGIN) == (DWORD)-1)
			goto err;
		if (WriteFile(fhp->handle, "\x00", 1, &bytesWritten, NULL) == 0)
			goto err;
		if (bytesWritten != 1)
			goto err;
		if (SetEndOfFile(fhp->handle) == 0)
			goto err;
		if (SetFilePointer(
		    fhp->handle, 0, NULL, FILE_BEGIN) == (DWORD)-1)
			goto err;
		if (FlushFileBuffers(fhp->handle) == 0)
			goto err;
	}

	F_SET(fhp, DB_FH_VALID);
	return (0);

err:	if (ret == 0)
		ret = __os_win32_errno();
	if (fhp->handle != INVALID_HANDLE_VALUE)
		(void)CloseHandle(fhp->handle);
	return (ret);
}
