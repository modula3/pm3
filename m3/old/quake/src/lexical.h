/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:50:30 PDT 1993 by harrison
 */

#ifndef LEXICAL_H
#define LEXICAL_H

typedef struct FileLocation {
    struct String *path_prefix;
    struct String *file_name;
} *FileLocation;

extern int
    yylex ARGS((void));

#endif /* LEXICAL_H */
