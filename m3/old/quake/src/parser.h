/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Thu Aug 26 11:32:53 PDT 1993 by harrison
 */

#ifndef PARSER_H
#define PARSER_H

extern void
    Parser_Initialize ARGS((void)),
    Parser_ErrorFormat ARGS((FILE *stream,
			     String path_prefix,
			     String file_name,
			     int line_number));

extern String
    Parser_FileName,
    Parser_PathPrefix;

extern Integer
    Parser_LineNumber;

extern int
    yydebug;

extern void
    yywarning ARGS((char *format, ...));

extern int
    yyparse ARGS((void)),
    yyerror ARGS((char *format, ...));

#endif /* PARSER_H */
