/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Thu Oct 13 08:14:00 PDT 1994 by kalsow   
 *      modified on Wed Aug 25 15:55:29 PDT 1993 by harrison 
 *      modified on Wed Apr 21 15:55:05 PDT 1993 by mjordan 
 */

#ifndef PATH_H
#define PATH_H

extern Boolean
    Path_IsAbsolute ARGS((String path));

extern String
    /* Return canonical version of /unfixed_path/ */
    Path_Normalize ARGS((String unfixed_path)),
    /* Return /unfixed_path/ relative to /current_path/ */
    Path_MakeRelative ARGS((String current_path, String unfixed_path)),
    Path_ExtractPath ARGS((String anything)),
    Path_ExtractFile ARGS((String anything));

extern char* Path_LastSep ARGS((char *s));
    /* Return a pointer to the last slash in 's', or NULL */

#endif /* PATH_H */
