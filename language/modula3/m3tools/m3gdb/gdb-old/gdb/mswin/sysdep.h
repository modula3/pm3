#include <stdlib.h>
#include <stddef.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
/*#include <sys/file.h>*/

#ifndef	O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif
#define SEEK_SET 0
#define SEEK_CUR 1
#define __ALMOST_STDC__
#define NO_FCNTL

#include "fopen-bin.h"

#define SIGQUIT 5
#define SIGTRAP 6
#define SIGHUP  7


/* Used to set up bfd/targets.c and bfd/archures.c */
#include "bfdtarget.h"







