/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Aug  9 21:36:23 PDT 1996 by najork   
 *      modified on Tue Feb  7 08:06:36 PST 1995 by kalsow   
 *      modified on Thu Sep 29 15:51:23 PDT 1994 by isard    
 *      modified on Thu Apr  7 09:50:13 PDT 1994 by harrison 
 *      modified on Thu May  6 17:12:46 PDT 1993 by mjordan 
 */

#include "quake.h"

typedef struct {
    char *text;
    Boolean is_function;
    int argc;
    BuiltinOperator operator;
} FunctionEntry;

typedef struct {
    char *text;
    char *value;
} VariableEntry;

static void ExecuteCommand(command)
char *command;
{
    Boolean ignore_error = FALSE;
    Boolean quiet = FALSE;
    char *real_command = command;

    while (strchr("@-", real_command[0]) != NULL) {
	switch (real_command[0]) {
	case '@':
	    quiet = TRUE;
	    break;
	case '-':
	    ignore_error = TRUE;
	    break;
	}
	real_command++;
    }

    if (!quiet)
	fprintf(stdout, "%s\n", real_command);

    fflush(stdout);
    fflush(stderr);

    if (Params.dont_execute)
	return;

    {
/*
 * These machine-dependent fragments call the system(3) library call (or
 * equivalent) the right way.  Each fragment defines a variable `exit_code'
 * to be non-zero if the call failed.
 */

#ifdef SYSTEM_FOR_WIN32

        int exit_code = system(real_command);

#else /* Use the POSIX default to invoke system */

	int result = system(real_command);
	int exit_code = WEXITSTATUS(result);

#endif
	
	if (exit_code != 0) {
	    fprintf(stderr, "*** error code %d", exit_code);
	    if (ignore_error)
		fprintf(stderr, " (ignored)");
	    fprintf(stderr, "\n");
	    
	    if (!ignore_error)
		yyerror("command execute failed");
	}
    }
}

static ExitCode Builtin_Exec(argc)
int argc;
{
    int length = 0;
    char **args = NEW_ARRAY(char *, argc);
    int buffer_length;
    char *buffer;

    {
	int i;

	for (i = argc - 1; i >= 0; i--) {
	    RANGE_CHECK(i, 0, argc);
	    args[i] = String_Convert(Pop_Any())->body;
	    length += strlen(args[i]) + 1;
	}
    }

    buffer = NEW_ARRAY(char, length);

    buffer[0] = '\0';

    {
	char *sep = "";
	int i;

	for (i = 0; i < argc; i++, sep = " ") {
	    RANGE_CHECK(strlen(buffer) + strlen(sep) + strlen(args[i]), 0, length);
	    strcat(buffer, sep);
	    strcat(buffer, args[i]);
	}
    }

    ExecuteCommand(buffer);

    Utils_FreeMemory(args);
    Utils_FreeMemory(buffer);

    return Exit_OK;
}	

static ExitCode Builtin_Empty(argc)
int argc;
{
    Atom atom = Pop_Any();
    Boolean is_empty = FALSE;

    switch (atom->tag) {
    case Tag_Array:
	is_empty = Array_IsEmpty(atom->u.array);
	break;
    case Tag_Table:
	is_empty = Table_IsEmpty(atom->u.table);
	break;
    case Tag_String:
	is_empty = String_IsEmpty(atom->u.string);
	break;
    default:
	yyerror("type for built-in empty must be an array, a table or a string: type <%s> found", Atom_TagText(atom->tag));
    }

    Push(Atom_Boolean(is_empty));

    return Exit_OK;
}	

static ExitCode Include(path_prefix, file_name)
String path_prefix;
String file_name;
{
    return Execute_File(path_prefix, file_name);
}
    
static ExitCode Builtin_Include(argc)
int argc;
{
    String anything = Pop_String();
    String path_prefix = Path_ExtractPath(anything);
    String file_name = Path_ExtractFile(anything);

    return Include(path_prefix, file_name);
}	

static ExitCode Builtin_DumpDicts(argc)
int argc;
{
    Dict_DumpAll(stderr);
    Name_Dump(stderr);

    return Exit_OK;
}	

static ExitCode Builtin_Defined(argc)
int argc;
{
    String string = Atom_GetString(Pop_Any());
    Atom atom = Dict_Lookup(Name_Register(string->body), FALSE);
    Boolean found = atom != NULL;

    Push(Atom_Boolean(found));

    return Exit_OK;
}	

static ExitCode Builtin_Path(argc)
int argc;
{
    Push(Atom_String(Execute_CurrentPathPrefix));

    return Exit_OK;
}

static ExitCode Builtin_File(argc)
int argc;
{
    Push(Atom_String(Execute_CurrentFileName));

    return Exit_OK;
}

static ExitCode Builtin_Stale(argc)
int argc;
{
    Array deps    = Array_Convert(Pop_Any());
    String target = Atom_GetString(Pop_Any());
    int i;

    if (!FileIO_Exists(target->body)) {
	Push(Atom_Boolean(TRUE));

	return Exit_OK;
    }

    for (i = 0; i < deps->next; i++) {
	String d = String_Convert(deps->body[i]);

	if (FileIO_IsStale(target, d)) {
	    Push(Atom_Boolean(TRUE));
	    
	    return Exit_OK;
	}
    }

    Push(Atom_Boolean(FALSE));

    return Exit_OK;
}	

static ExitCode Builtin_CopyIfNew(argc)
int argc;
{
    String dest = Atom_GetString(Pop_Any());
    String src = Atom_GetString(Pop_Any());

    FileIO_CopyIfNew(src->body, dest->body);

    return Exit_OK;
}

static ExitCode Builtin_UnlinkFile(argc)
int argc;
{
    String file_name = Atom_GetString(Pop_Any());

    Push(Atom_Boolean(FileIO_Unlink(file_name->body)));

    return Exit_OK;
}	

/*
 * Really lazy use of the stack
 */

static void WriteHelper(argc)
int argc;
{
    if (argc > 0) {
	String arg = String_Convert(Pop_Any());

	WriteHelper(argc - 1);
	fputs(arg->body, IOStack_Current());
    }
}
	
static ExitCode Builtin_Write(argc)
int argc;
{
    WriteHelper(argc);
    fflush(IOStack_Current());

    return Exit_OK;
}	

static ExitCode Builtin_Error(argc)
int argc;
{
    yyerror(String_Convert(Pop_Any())->body);

    return Exit_OK;
}	

static ExitCode Builtin_ArgList(argc)
int argc;
{
    Array arg_list = Atom_GetArray(Pop_Any());
    String prefix = Atom_GetString(Pop_Any());

    if (arg_list->next <= MAX_ARGS)
	Push(Atom_String(String_Convert(Atom_Array(arg_list))));
    else {
	int i;
	char *temp_file = FileIO_MakeTemp();
	FILE *fp = fopen(temp_file, "w");

	if (fp == NULL)
	    yyerror("cannot open temporary file \"%s\": %s", temp_file, sys_errlist[errno]);
	for (i = 0; i < arg_list->next; i++) {
	    Atom this_arg = arg_list->body[i];
	    
	    switch (this_arg->tag) {
	    case Tag_String:
		fprintf(fp, "%s\n", this_arg->u.string->body);
		break;
	    default: {
		String string = String_Convert(this_arg);
		
		fprintf(fp, "%s\n", string->body);
		break;
	    }
	    }
	}

	fclose(fp);
	
	{
	    char junk[MAXPATHLEN];
	    
	    sprintf(junk, "%s%s", prefix->body, temp_file);
	    Push(Atom_String(String_New(junk)));
	}
    }

    return Exit_OK;
}	

static ExitCode Builtin_Normalize(argc)
int argc;
{
    String unfixed = String_Convert(Pop_Any());
    String prefix = String_Convert(Pop_Any());
    String result = Path_MakeRelative(prefix, Path_Normalize(unfixed));

    Push(Atom_String(result));

    return Exit_OK;
}	

static ExitCode Builtin_Escape(argc)
int argc;
{
    String value = String_Convert(Pop_Any());
    String result = String_Escape(value);

    Push(Atom_String(result));

    return Exit_OK;
}	

static ExitCode Builtin_Format(argc)
int argc;
{
    String format;
    int how_many_format_args = 0;
    int how_many_actual_args = argc - 1;
    char *p;
    char *result, *result_ptr;
    String *arg_array;
    int i, arg_index, length = 0;

    if (how_many_actual_args < 0)
	yyerror("format requires at least a format string");

    arg_array = NEW_ARRAY(String, how_many_actual_args);
    for (i = how_many_actual_args - 1; i >= 0; i--) {
	RANGE_CHECK(i, 0, how_many_actual_args);
	arg_array[i] = String_Convert(Pop_Any());
    }

    format = String_Convert(Pop_Any());

    for (p = format->body; *p != '\0'; p++) {
	switch (*p) {
	case '%':
	    switch (*++p) {
	    case '%':
		length++;
		break;
	    case 's':
		how_many_format_args++;
		break;
	    }
	    break;
	default:
	    length++;
	    break;
	}
    }
    
    if (how_many_format_args != how_many_actual_args)
	yyerror("wrong number of arguments in format string---%d found for %d arg(s)", how_many_format_args, how_many_actual_args);
    
    /* how long is the result string? */
    for (i = 0; i < how_many_format_args; i++) {
	RANGE_CHECK(i, 0, how_many_actual_args);
	length += strlen(arg_array[i]->body);
    }

    result = NEW_ARRAY(char, length + 1); /* for \0 */

    result_ptr = result;
    arg_index = 0;
    for (p = format->body; *p != '\0'; p++) {
	switch (*p) {
	case '%':
	    switch (*++p) {
	    case '%':
		*result_ptr++ = '%';
		break;
	    case 's': {
		int arg_length = strlen(arg_array[arg_index]->body);
		
		Utils_CopyMemory(arg_array[arg_index]->body,
				 result_ptr,
				 arg_length);
		result_ptr += arg_length;

		arg_index++;
		break;
	    }
	    }
	    break;
	default:
	    *result_ptr++ = *p;
	    break;
	}
    }

    *result_ptr = '\0';

    Push(Atom_String(String_New(result)));

    Utils_FreeMemory(arg_array);
    Utils_FreeMemory(result);

    return Exit_OK;
}	

static ExitCode Builtin_Equal(argc)
int argc;
{
    String s1 = Atom_GetString(Pop_Any());
    String s2 = Atom_GetString(Pop_Any());
    
    Push(Atom_Boolean(String_Equal(s1, s2)));

    return Exit_OK;
}

#define FUNCTION	TRUE
#define PROCEDURE	FALSE

static FunctionEntry Functions[] = {
    /* name            what?      nArgs     procedure */
    {"arglist",        FUNCTION,  2,        Builtin_ArgList},
    {"defined",        FUNCTION,  1,        Builtin_Defined},
    {"empty",          FUNCTION,  1,        Builtin_Empty},
    {"equal",          FUNCTION,  2,        Builtin_Equal},
    {"error",          PROCEDURE, 1,        Builtin_Error},
    {"exec",           PROCEDURE, VARIADIC_ARGS, Builtin_Exec},
    {"file",           PROCEDURE, 0,        Builtin_File},
    {"format",         FUNCTION,  VARIADIC_ARGS, Builtin_Format},
    {"include",        PROCEDURE, 1,        Builtin_Include},
    {"normalize",      FUNCTION,  2,        Builtin_Normalize},
    {"escape",         FUNCTION,  1,        Builtin_Escape},
    {"path",           FUNCTION,  0,        Builtin_Path},
    {"cp_if",          PROCEDURE, 2,        Builtin_CopyIfNew},
    {"stale",          FUNCTION,  2,        Builtin_Stale},
    {"unlink_file",    FUNCTION,  1,        Builtin_UnlinkFile},
    {"write",          PROCEDURE, VARIADIC_ARGS, Builtin_Write},

    {"_dump_dicts",    PROCEDURE, 0,        Builtin_DumpDicts},
};

static void Initialize_Functions()
{
    FunctionEntry *f;

    for (f = Functions; f < Functions + NUMBER(Functions); f++) {
	Name name = Name_Register(f->text);

	Dict_Install(name, Atom_Builtin(name,
					f->is_function,
					f->argc,
					f->operator), DICTFLAGS_READONLY);
    }
}

void Builtin_Initialize()
{
    Initialize_Functions();
}
