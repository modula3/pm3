/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Feb 23 14:13:35 1994 by harrison
 */

#include "quake.h"

static List CodeStack = NULL;

void Code_Compile(atom)
Atom atom;
{
    if (CodeStack == NULL) {
	Execute_Atom(atom);
    } else {
	Code code = (Code) CodeStack->first;

	Array_AppendAtom(code->array, atom);
    }
}

void Code_New()
{
    Code code = NEW(struct Code);
    
    code->array = Array_New(0);
    code->file_location = NEW(struct FileLocation);

    code->file_location->path_prefix = Execute_CurrentPathPrefix;
    code->file_location->file_name   = Execute_CurrentFileName;
    code->line_number                = Execute_CurrentLineNumber;

    List_Push(&CodeStack, code);
}

Code Code_Retrieve()
{
    Code code = (Code) List_Pop(&CodeStack);

    return code;
}

Boolean Code_IsFunction(flags)
int flags;
{
    Boolean simple_return = (flags & (RETURN_NOTHING |
				      RETURN_SIMPLE)) != 0;
    Boolean value_return = (flags & RETURN_VALUE) != 0;

    if (simple_return == value_return)
	yyerror("inconsistent returns from procedure");

    return value_return;
}

void Code_Dump(stream, code)
FILE *stream;
Code code;
{
    Array_Dump(stream, code->array);
}

