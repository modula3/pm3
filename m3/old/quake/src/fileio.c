/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Jan 30 08:19:52 PST 1995 by kalsow   
 *      modified on Thu Sep 29 15:59:41 PDT 1994 by isard    
 *      modified on Mon Mar 14 10:29:13 PST 1994 by harrison 
 *      modified on Thu May  6 16:05:41 PDT 1993 by mjordan 
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "quake.h"

static List TempFiles = NULL;

#define COPYBUF_LEN 4096

#ifndef O_BINARY
#define O_BINARY 0
#endif

#if defined(TARGET_NEXT)

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#ifndef S_ISDIR
#define S_ISDIR(m) ((m & _S_IFMT) == _S_IFDIR)
#endif

char *tempnam(const char *dir, const char *prefix)
{
    static int count;
    char *path = (char *)malloc(1024);

    if (!dir)
	dir = "/tmp";
    if (!prefix)
	prefix = "t";

    if (path) {
	sprintf(path, "%s/%s%d.%d", dir, prefix, count++, (int)getpid());
    }

    return path;
}
#endif

#if defined(TARGET_NT386)

#define S_ISDIR(m) ((m & _S_IFMT) == _S_IFDIR)

#endif  

Boolean FileIO_IsStale(file, dependency)
String file;
String dependency;
{
    struct stat file_buf, dep_buf;

    return
	stat(file->body, &file_buf) != 0 ||
	stat(dependency->body, &dep_buf) != 0 ||
        file_buf.st_mtime < dep_buf.st_mtime;
}

Boolean FileIO_Exists(path)
char *path;
{
    struct stat buf;

    return stat(path, &buf) == 0;
}

Boolean FileIO_Unlink(path)
char *path;
{
    if (Params.dont_execute)
	return TRUE;
    else
	return unlink(path) == 0;
}

Boolean FileIO_IsNormal(path)
char *path;
{
    struct stat buf;

    return stat(path, &buf) == 0 && S_ISREG(buf.st_mode);
}

Boolean FileIO_IsDirectory(path)
char *path;
{
    struct stat buf;

    return stat(path, &buf) == 0 && S_ISDIR(buf.st_mode);
}

char *FileIO_MakeTemp()
{
    char *tmp = tempnam(getenv ("TEMP"), "qk");

    if (tmp == NULL)
	yyerror("cannot create the temporary file name: something's seriously bogus");

    List_Push((List *) &TempFiles, (Refany) Utils_StringSave(tmp));

    return tmp;
}

void FileIO_Finalize()
{
    while (TempFiles != NULL)
	FileIO_Unlink((char *) List_Pop(&TempFiles));
}


void FileIO_Copy(src, dest)
char *src;
char *dest;
{
    FILE *in, *out;
    char buf[COPYBUF_LEN];
    int len;

    if ((in = fopen(src, "rb")) == NULL)
        yyerror("copy: Can't open %s for reading", src);

    if ((out = fopen(dest, "wb")) == NULL)
        yyerror("copy: Can't open %s for writing", dest);

    do {
        len = fread(buf, 1, COPYBUF_LEN, in);
	if (len > 0) fwrite(buf, 1, len, out);
    } while (len >= COPYBUF_LEN);

    fclose(in);
    fclose(out);
}

Boolean FileIO_IsEqual(a, b)
char *a;
char *b;
{
    int src, other;
    char buf1[COPYBUF_LEN], buf2[COPYBUF_LEN];
    int len1, len2;
    int i, equal = TRUE;
    struct stat srcstat, otherstat;

    if ((src = open(a, O_RDONLY | O_BINARY)) < 0)
        yyerror("isequal: Can't open %s for reading", a);

    if ((other = open(b, O_RDONLY | O_BINARY)) < 0) {
        close(src);
        return FALSE;
    };

    fstat(src, &srcstat);
    fstat(other, &otherstat);

    if (srcstat.st_size != otherstat.st_size) {
        close(src);
	close(other);
        return FALSE;
    };

    do {
        len1 = read(src, buf1, COPYBUF_LEN);
	len2 = read(other, buf2, COPYBUF_LEN);
	if (len1 != len2) { equal = FALSE; break; }

	for (i = 0; (i < len1) && (buf1[i] == buf2[i]); i++) ;
        if (i < len1) { equal = FALSE; break; }
    } while (len1 > 0);

    close(src);
    close(other);

    return equal;
}

void FileIO_CopyIfNew (src, dest)
char *src;
char *dest;
{
    char destfile[MAXPATHLEN + 1];
    char *src_tail;

    if (FileIO_IsDirectory(dest)) {
        src_tail = Path_LastSep (src);
	src_tail = (src_tail == NULL) ? (src) : (src_tail + 1);
        sprintf (destfile, "%s%c%s", dest, DIR_SEPARATOR, src_tail);
	dest = destfile;
    }

    if (!FileIO_IsEqual(src, dest))
        FileIO_Copy(src, dest);
}
