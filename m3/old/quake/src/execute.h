/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:50:31 PDT 1993 by harrison
 */

#ifndef EXECUTE_H
#define EXECUTE_H

extern Name
    Execute_LastName;

extern String
    Execute_CurrentFileName,
    Execute_CurrentPathPrefix;

extern Integer
    Execute_CurrentLineNumber;

extern FILE
    *Execute_CurrentStream;

extern void
    Execute_Initialize ARGS((void)),
    Execute_PushContext ARGS((void)),
    Execute_PopContext ARGS((void));

extern ExitCode
    Execute_Atom ARGS((Atom_ForwardDecl atom)),
    Execute_Code ARGS((Code_ForwardDecl code)),
    Execute_Stream ARGS((FILE *stream, String stream_name)),
    Execute_File ARGS((String path_prefix, String file_name));

#endif /* EXECUTE_H */
