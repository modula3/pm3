/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Jan 30 08:31:27 PST 1995 by kalsow   
 *      modified on Fri Oct 29 11:41:24 PDT 1993 by harrison 
 */

#ifndef CONFIG_H
#define CONFIG_H

#ifdef	TARGET_NT386
/*
 * Directory separators are not the default "/"
 */
#define	DIR_SEPARATOR  '\\'
#define VOL_SEPARATOR  ':' 

/*
 * The system(3) library call is different under NT
 */
#define SYSTEM_FOR_WIN32

/*
 * We need different include files...
 */
#define WIN32_INCLUDES

/*
 * These are used in "system()" in system.c.  Unix systems define these
 * in /usr/include/sys/file.h or somewhere similar.  I can't track them down
 * in my copy of NT (Beta 3.1).  Please let me know.
 */
#ifndef F_OK
#define	F_OK	(0)
#define	X_OK	(1)
#define	W_OK	(2)
#define	R_OK	(4)
#endif

/*
 * Again, used in FileIO_IsNormal but not defined on NT/3.1 ...
 */
#ifndef S_ISREG
#define S_ISREG(m) ((m & _S_IFMT) == _S_IFREG)
#endif

#endif	/* TARGET_NT386 */

#endif	/* CONFIG_H */
