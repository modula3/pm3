/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Mar 14 10:14:02 PST 1994 by harrison 
 *      modified on Tue May  4 23:05:38 PDT 1993 by mjordan 
 */

#include "quake.h"

Parameters Params;

extern int StackHWM;

static void Usage(progname)
char *progname;
{
    fprintf(stderr, "Usage: %s [-d dir] [-f file] [-D symbol] ...\n", progname);

    exit(1);
}

static void AddPredefinition(argument)
char *argument;
{
    char *string = Utils_StringSave(argument);
    char *sep = strchr(string, '=');

    if (sep == NULL)
	sep = "";
    else
	*sep++ = '\0';		/* step over '=' and terminate name */

    Dict_Install(Name_Register(string),
		 Atom_String(String_New(sep)),
		 DICTFLAGS_NONE);
}

static int ParseOptions(argc, argv)
int argc;
char *argv[];
{
    int argn, next_arg;
    char *progname = argv[0];

    for (argn = 1, next_arg = 2; argn < argc; argn = next_arg++) {
	if (argv[argn][0] != '-') {
	    /* Regular file name---execute it */
	    char *source = argv[argn];

	    if (strcmp(source, "-") == 0)
		Execute_Stream(stdin, String_New("* stdin *"));
	    else {
		String temp = String_New(source);

		Execute_File(Path_ExtractPath(temp), Path_ExtractFile(temp));
	    }
	} else {
	    /* command-line option---process it */
	    char *s;
	    Boolean done = FALSE;
	
	    for (s = &argv[argn][1]; !done && *s != '\0'; s++)
		switch (*s) {
		case 'n':
		    Params.dont_execute = TRUE;
		    break;
		case 'D':
		    if (s[1] != '\0') {
			AddPredefinition(s + 1);
			done = TRUE;
		    } else if (next_arg <= argc - 1) {
			AddPredefinition(argv[next_arg]);
		    } else {
			Usage(progname);
		    }
		    break;
		case 'V':
		    Params.trace_files = TRUE;
		    break;
#ifdef	YYDEBUG
		case 'y':
		    yydebug = 1;
		    break;
#endif
		default:
		    yyerror("unknown command-line option '%c'\n", *s);
		    Usage(progname);
		    break;
		}
	}
    }

    return argn;
}

int main(argc, argv)
int argc;
char *argv[];
{
    int argn;

#ifdef	YYDEBUG
    yydebug = 0;
#endif
    Params.dont_execute = FALSE;
    Params.trace_files = FALSE;

    Atom_Initialize();
    Name_Initialize();
    Dict_Initialize();
    Execute_Initialize();
    Builtin_Initialize();
    IOStack_Initialize();
    Parser_Initialize();

    argn = ParseOptions(argc, argv);

    if (argn != argc)
	Usage(argv[0]);

    FileIO_Finalize();
    
    return 0;
}

