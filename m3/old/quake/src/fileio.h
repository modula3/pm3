/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Oct 12 13:17:14 PDT 1994 by kalsow  
 *      modified on Thu Sep 29 15:57:55 PDT 1994 by isard   
 *      modified on Wed Aug 25 15:50:31 PDT 1993 by harrison
 */

#ifndef FILEIO_H
#define FILEIO_H

extern Boolean
    FileIO_Exists ARGS((char *path)),
    FileIO_Unlink ARGS((char *path)),
    FileIO_IsNormal ARGS((char *path)),
    FileIO_IsStale ARGS((String file, String dependency));
extern char
    *FileIO_MakeTemp ARGS((void));
extern void
    FileIO_Finalize ARGS((void)),
    FileIO_CopyIfNew ARGS((char *src, char *dest));

#endif /* FILEIO_H */
