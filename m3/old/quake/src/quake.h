/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Feb 15 08:05:31 PST 1995 by kalsow  
 *      modified on Fri Oct 29 11:31:58 PDT 1993 by harrison
 */

#ifndef QUAKE_H
#define QUAKE_H

#ifdef  __STDC__
#include <stdarg.h>
#define va_init va_start

#define ARGS(args) args

typedef void *Refany;
#else
#include <varargs.h>
#define va_init(a,b) va_start(a)

#define ARGS(args) ()

typedef char *Refany;
#endif /* __STDC__ */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#ifdef TARGET_NT386

#  include <io.h>
#  include <direct.h>
#  define  MAXPATHLEN _MAX_PATH

#else
   /*
    * The default includes for quake
    */
#  include <unistd.h>
#  include <sys/param.h>
#  include <sys/types.h>
#  include <sys/wait.h>

#ifndef TARGET_FreeBSD2
extern char *sys_errlist[];
#endif

#endif

#include <errno.h>

#include "basic.h"
#include "list.h"
#include "strng.h"
#include "iostack.h"
#include "name.h"
#include "hash.h"
#include "execute.h"
#include "lexical.h"
#include "array.h"
#include "code.h"
#include "table.h"
#include "utils.h"
#include "parser.h"
#include "dict.h"
#include "operator.h"
#include "atom.h"
#include "stack.h"
#include "fileio.h"
#include "path.h"
#include "builtin.h"

extern Parameters
    Params;

#endif /* QUAKE_H */
