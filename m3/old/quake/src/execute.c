/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Sep 14 09:07:31 PDT 1994 by kalsow  
 *      modified on Mon Mar 14 10:13:07 PST 1994 by harrison
 */

#include "quake.h"

typedef struct ExecuteContext {
    FILE *stream;
    FileLocation file_location;
    int line_number;
} *ExecuteContext;

Name Execute_LastName = NULL;

String
    Execute_CurrentFileName = NULL,
    Execute_CurrentPathPrefix = NULL;

Integer
    Execute_CurrentLineNumber = 0;

FILE
    *Execute_CurrentStream = NULL;

static List
    ExecuteContextStack = NULL;

void Execute_PushContext()
{
    ExecuteContext context = NEW(struct ExecuteContext);

    context->file_location = NEW(struct FileLocation);
    context->file_location->file_name   = Execute_CurrentFileName;
    context->file_location->path_prefix = Execute_CurrentPathPrefix;
    context->line_number                = Execute_CurrentLineNumber;
    context->stream                     = Execute_CurrentStream;

    List_Push(&ExecuteContextStack, context);
}

void Execute_PopContext()
{
    ExecuteContext context = (ExecuteContext) List_Pop(&ExecuteContextStack);

    Execute_CurrentFileName   = context->file_location->file_name;
    Execute_CurrentPathPrefix = context->file_location->path_prefix;
    Execute_CurrentLineNumber = context->line_number;
    Execute_CurrentStream     = context->stream;

    Parser_FileName   = Execute_CurrentFileName;
    Parser_PathPrefix = Execute_CurrentPathPrefix;
    Parser_LineNumber = Execute_CurrentLineNumber;

    Utils_FreeMemory(context);
}

ExitCode Execute_Atom(atom)
Atom atom;
{
    switch (atom->tag) {
    case Tag_Name:
	Execute_LastName = atom->u.name;
	return Execute_Atom(Dict_Lookup(atom->u.name, TRUE));
    case Tag_Operator:
	return (*atom->u.operator)();
    case Tag_FileLocation: {
	FileLocation file_location = atom->u.file_location;

	Parser_PathPrefix = file_location->path_prefix;
	Parser_FileName   = file_location->file_name;
	break;
    }
    case Tag_LineNumber:
	Parser_LineNumber = atom->u.line_number;
	break;
    default:
	Push(atom);
	break;
    }

    return Exit_OK;
}

ExitCode Execute_Code(code)
Code code;
{
    ExitCode e = Exit_OK;
    int pc;

    Execute_PushContext();

    Parser_PathPrefix = code->file_location->path_prefix;
    Parser_FileName   = code->file_location->file_name;
    Parser_LineNumber = code->line_number;

    for (pc = 0; pc < code->array->next; pc++) {
	RANGE_CHECK(pc, 0, code->array->length);
	if ((e = Execute_Atom(code->array->body[pc])) != Exit_OK)
	    break;
    }

    Execute_PopContext();

    return e;
}

static ExitCode GenericExecute(stream, path_prefix, file_name, line_number)
FILE *stream;
String path_prefix;
String file_name;
int line_number;
{
    Execute_PushContext();

    Execute_CurrentStream     = stream;
    Execute_CurrentPathPrefix = path_prefix;
    Execute_CurrentFileName   = file_name;
    Execute_CurrentLineNumber = line_number;

    Parser_FileName   = Execute_CurrentFileName;
    Parser_PathPrefix = Execute_CurrentPathPrefix;
    Parser_LineNumber = Execute_CurrentLineNumber;

    if (yyparse() != 0)
	yyerror("parse error in %s", file_name->body);

    Execute_PopContext();

    return Exit_OK;
}

ExitCode Execute_Stream(stream, stream_name)
FILE *stream;
String stream_name;
{
    return GenericExecute(stream, String_New(""), stream_name, -1);
}
    
ExitCode Execute_File(path_prefix, file_name)
String path_prefix;
String file_name;
{
    String real_path_prefix;

    if (Path_IsAbsolute(path_prefix)) {
	/* path is absolute---just use it */
	real_path_prefix = path_prefix;
    } else {
	/* path is relative---append the path to existing one */
	char junk[MAXPATHLEN + 1 + MAXPATHLEN + 1];

	if (Execute_CurrentPathPrefix == NULL ||
	    strlen(Execute_CurrentPathPrefix->body) == 0)
	    sprintf(junk, "%s", path_prefix->body);
	else
	    sprintf(junk, "%s%c%s",
		    Execute_CurrentPathPrefix->body,
		    DIR_SEPARATOR,
		    path_prefix->body);
	real_path_prefix = Path_Normalize(String_New(junk));
    }

    {
	FILE *stream;
	ExitCode result;
	char junk[MAXPATHLEN + 1];

	sprintf(junk, "%s%c%s",
		real_path_prefix->body,
		DIR_SEPARATOR,
		file_name->body);
	
	if (!FileIO_Exists(junk))
	    yyerror("file \"%s\" does not exist", junk);

	if (!FileIO_IsNormal(junk))
	    yyerror("file \"%s\" is not regular and plain", junk);

	stream = fopen(junk, "r");
	if (stream == NULL)
	    yyerror("cannot open file \"%s\" for reading: the reason might be %s", junk, sys_errlist[errno]);

	if (Params.trace_files)
	    fprintf(stderr, "quake: reading \"%s\"\n", junk);

	result = GenericExecute(stream, real_path_prefix, file_name, 1);

	fclose(stream);

	return result;
    }
}

void Execute_Initialize()
{
    Execute_CurrentStream = NULL;
    Execute_CurrentPathPrefix = String_New("");
    Execute_CurrentFileName = String_New("");
    Execute_CurrentLineNumber = 0;
}


