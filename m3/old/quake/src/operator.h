/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:50:28 PDT 1993 by harrison
 */

#ifndef OPERATOR_H
#define OPERATOR_H

extern ExitCode
    Op_And ARGS((void)),
    Op_Append ARGS((void)),
    Op_ArrayDesignator ARGS((void)),
    Op_ArraySelection ARGS((void)),
    Op_AssignWithOptions ARGS((void)),
    Op_EndArray ARGS((void)),
    Op_EndRedirect ARGS((void)),
    Op_EndTable ARGS((void)),
    Op_Exchange ARGS((void)),
    Op_Foreach ARGS((void)),
    Op_ForeachTwoArgs ARGS((void)),
    Op_FunctionCall ARGS((void)),
    Op_Getenv ARGS((void)),
    Op_If ARGS((void)),
    Op_IfElse ARGS((void)),
    Op_Indirect ARGS((void)),
    Op_Not ARGS((void)),
    Op_Or ARGS((void)),
    Op_ProcedureCall ARGS((void)),
    Op_Return ARGS((void)),
    Op_ReturnValue ARGS((void)),
    Op_TableDesignator ARGS((void)),
    Op_TableMembership ARGS((void)),
    Op_TableSelection ARGS((void)),
    Op_Assign ARGS((void)),
    Op_StartAppendRedirect ARGS((void)),
    Op_StartArray ARGS((void)),
    Op_StartRedirect ARGS((void)),
    Op_StartTable ARGS((void)),
    Op_StringCatenate ARGS((void));

/*
 * Does not really belong here.
 */
extern void
    CallStack_BackTrace ARGS((FILE *stream));

#endif /* OPERATOR_H */

