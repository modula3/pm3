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
#endif

#include "db_int.h"

/*
 * EXTERN: int db_env_set_func_close __P((int (*)(int)));
 */
int
db_env_set_func_close(func_close)
	int (*func_close) __P((int));
{
	DB_GLOBAL(j_close) = func_close;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_dirfree __P((void (*)(char **, int)));
 */
int
db_env_set_func_dirfree(func_dirfree)
	void (*func_dirfree) __P((char **, int));
{
	DB_GLOBAL(j_dirfree) = func_dirfree;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_dirlist
 * EXTERN:     __P((int (*)(const char *, char ***, int *)));
 */
int
db_env_set_func_dirlist(func_dirlist)
	int (*func_dirlist) __P((const char *, char ***, int *));
{
	DB_GLOBAL(j_dirlist) = func_dirlist;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_exists __P((int (*)(const char *, int *)));
 */
int
db_env_set_func_exists(func_exists)
	int (*func_exists) __P((const char *, int *));
{
	DB_GLOBAL(j_exists) = func_exists;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_free __P((void (*)(void *)));
 */
int
db_env_set_func_free(func_free)
	void (*func_free) __P((void *));
{
	DB_GLOBAL(j_free) = func_free;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_fsync __P((int (*)(int)));
 */
int
db_env_set_func_fsync(func_fsync)
	int (*func_fsync) __P((int));
{
	DB_GLOBAL(j_fsync) = func_fsync;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_ioinfo __P((int (*)(const char *,
 * EXTERN:     int, u_int32_t *, u_int32_t *, u_int32_t *)));
 */
int
db_env_set_func_ioinfo(func_ioinfo)
	int (*func_ioinfo)
	    __P((const char *, int, u_int32_t *, u_int32_t *, u_int32_t *));
{
	DB_GLOBAL(j_ioinfo) = func_ioinfo;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_malloc __P((void *(*)(size_t)));
 */
int
db_env_set_func_malloc(func_malloc)
	void *(*func_malloc) __P((size_t));
{
	DB_GLOBAL(j_malloc) = func_malloc;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_map
 * EXTERN:     __P((int (*)(char *, size_t, int, int, void **)));
 */
int
db_env_set_func_map(func_map)
	int (*func_map) __P((char *, size_t, int, int, void **));
{
	DB_GLOBAL(j_map) = func_map;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_open __P((int (*)(const char *, int, ...)));
 */
int
db_env_set_func_open(func_open)
	int (*func_open) __P((const char *, int, ...));
{
	DB_GLOBAL(j_open) = func_open;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_read __P((ssize_t (*)(int, void *, size_t)));
 */
int
db_env_set_func_read(func_read)
	ssize_t (*func_read) __P((int, void *, size_t));
{
	DB_GLOBAL(j_read) = func_read;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_realloc __P((void *(*)(void *, size_t)));
 */
int
db_env_set_func_realloc(func_realloc)
	void *(*func_realloc) __P((void *, size_t));
{
	DB_GLOBAL(j_realloc) = func_realloc;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_rename
 * EXTERN:     __P((int (*)(const char *, const char *)));
 */
int
db_env_set_func_rename(func_rename)
	int (*func_rename) __P((const char *, const char *));
{
	DB_GLOBAL(j_rename) = func_rename;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_seek
 * EXTERN:     __P((int (*)(int, size_t, db_pgno_t, u_int32_t, int, int)));
 */
int
db_env_set_func_seek(func_seek)
	int (*func_seek) __P((int, size_t, db_pgno_t, u_int32_t, int, int));
{
	DB_GLOBAL(j_seek) = func_seek;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_sleep __P((int (*)(u_long, u_long)));
 */
int
db_env_set_func_sleep(func_sleep)
	int (*func_sleep) __P((u_long, u_long));
{
	DB_GLOBAL(j_sleep) = func_sleep;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_unlink __P((int (*)(const char *)));
 */
int
db_env_set_func_unlink(func_unlink)
	int (*func_unlink) __P((const char *));
{
	DB_GLOBAL(j_unlink) = func_unlink;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_unmap __P((int (*)(void *, size_t)));
 */
int
db_env_set_func_unmap(func_unmap)
	int (*func_unmap) __P((void *, size_t));
{
	DB_GLOBAL(j_unmap) = func_unmap;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_write
 * EXTERN:     __P((ssize_t (*)(int, const void *, size_t)));
 */
int
db_env_set_func_write(func_write)
	ssize_t (*func_write) __P((int, const void *, size_t));
{
	DB_GLOBAL(j_write) = func_write;
	return (0);
}

/*
 * EXTERN: int db_env_set_func_yield __P((int (*)(void)));
 */
int
db_env_set_func_yield(func_yield)
	int (*func_yield) __P((void));
{
	DB_GLOBAL(j_yield) = func_yield;
	return (0);
}
