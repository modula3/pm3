%{

/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Jan 27 15:39:31 PST 1995 by kalsow  
 *      modified on Thu Sep 16 12:01:08 PDT 1993 by harrison
 */

#include "quake.h"

String
    Parser_FileName,
    Parser_PathPrefix;

Integer
    Parser_LineNumber;

static void GenerateAssignment();

%}

/*
 * We expect 1 shift/reduce due to the `if ... end' and `if ... else ... end'
 * ambiguity.
 */

/*
%expect 1
*/

/*
 * keywords
 */

%token AND
%token CONTAINS
%token ELSE
%token END
%token FOREACH
%token IF
%token IN
%token IS
%token LOCAL
%token NOT
%token OR
%token PROC
%token READONLY
%token RETURN

/*
 * other lexical tokens
 */

%token '$'
%token '&'
%token '('
%token ')'
%token '+'
%token ','
%token ':'
%token '<'
%token '='
%token '>'
%token '['
%token ']'
%token '{'
%token '}'

%token <integer> INTEGER
%token <name> ID
%token <string> STRING

%type <integer> actual_arg_spec actual_arg_list 
%type <integer> opt_decl_modifier_list decl_modifier_list decl_modifier
%type <integer> statement saved_block unsaved_block statement_list
%type <list> formal_arg_spec formal_arg_list

%union {
    Array array;
    Atom atom;
    Integer integer;
    List list;
    Name name;
    String string;
}

%left OR
%left AND
%left NOT
%left CONTAINS
%left '&'

%start program

%%

program
    :	{
	    Code_New();
	} unsaved_block {
	    (void) Execute_Code(Code_Retrieve());
	}
    ;

unsaved_block
    :   statement_list { $$ = $1; }
    ;

statement_list
    :	/* empty */ { $$ = RETURN_NOTHING; }
    |	statement_list statement {
	    switch ($2) {
	    case RETURN_VALUE:
	    case RETURN_SIMPLE:
		/* explicit return overrides default action */
		$$ = ($1 | $2) & ~RETURN_NOTHING;
		break;
	    default:
		$$ = $1 | $2;
		break;
	    }
	}
    ;
  
saved_block
    :	{
	    Code_New();
	} unsaved_block {
	    Code_Compile(Atom_Code(Code_Retrieve()));

	    $$ = $2;
	}
    ;

statement
    :   '>' expression IN {
	    Code_Compile(Atom_Operator(Op_StartRedirect));
	} statement_list END {
	    Code_Compile(Atom_Operator(Op_EndRedirect));

	    $$ = RETURN_NOTHING;
	}	
    |   '>' '>' expression IN {
	    Code_Compile(Atom_Operator(Op_StartAppendRedirect));
	} statement_list END {
	    Code_Compile(Atom_Operator(Op_EndRedirect));

	    $$ = RETURN_NOTHING;
	}
    /* procedure call */
    |   ID actual_arg_spec {
	    Code_Compile(Atom_Integer($2));
	    Code_Compile(Atom_Name($1));
	    Code_Compile(Atom_Operator(Op_ProcedureCall));

	    $$ = RETURN_NOTHING;
	}
    |   opt_decl_modifier_list PROC ID formal_arg_spec IS {
	    Code_New();
        } unsaved_block END {
	    Code code = Code_Retrieve();

	    Code_Compile(Atom_Designator(Designator_Name($3)));
	    Code_Compile(Atom_Procedure($3, $4, Code_IsFunction($7), code));
	    /* procedures are always local */
	    GenerateAssignment(/*???DICTFLAGS_LOCAL | */$1);

	    $$ = RETURN_NOTHING;
	}
    |   RETURN {
	    Code_Compile(Atom_Operator(Op_Return));

	    $$ = RETURN_SIMPLE;
	}
    |   RETURN expression {
	    Code_Compile(Atom_Operator(Op_ReturnValue));

	    $$ = RETURN_VALUE;
	}
    |	lvalue '=' expression {
	    GenerateAssignment(DICTFLAGS_NONE);

	    $$ = RETURN_NOTHING;
	}
    |	decl_modifier_list lvalue '=' expression {
	    GenerateAssignment($1);

	    $$ = RETURN_NOTHING;
	}
    |	lvalue '+' '=' expression {
    	    Code_Compile(Atom_Operator(Op_Append));

	    $$ = RETURN_NOTHING;
	}
    |   IF expression saved_block END {
	    Code_Compile(Atom_Operator(Op_If));

	    $$ = $3 | RETURN_NOTHING;
	}
    |	IF expression saved_block ELSE saved_block END {
	    Code_Compile(Atom_Operator(Op_IfElse));

	    $$ = $3 | $5;
	}
    |   FOREACH ID {
	    Code_Compile(Atom_Designator(Designator_Name($2)));
        } IN expression saved_block END {
	    Code_Compile(Atom_Operator(Op_Foreach));

	    $$ = $6;
	}
    ;

lvalue
    :	ID {
	   Code_Compile(Atom_Designator(Designator_Name($1)));
        }
    |   ID {
	   Code_Compile(Atom_Designator(Designator_Name($1)));
        } lhs_qualifier_list
    ;
 
lhs_qualifier_list
    :   lhs_qualifier
    |   lhs_qualifier_list lhs_qualifier
    ;

lhs_qualifier
    :   '[' expression ']' {
	    Code_Compile(Atom_Operator(Op_ArrayDesignator));
	}
    |   '{' expression '}' {
	    Code_Compile(Atom_Operator(Op_TableDesignator));
	}
    ;

opt_decl_modifier_list
    :   /* empty */ { $$ = DICTFLAGS_NONE; }
    |   decl_modifier_list { $$ = $1; }
    ;

decl_modifier_list
    :  decl_modifier { $$ = $1; }
    |  decl_modifier_list decl_modifier { $$ = $1 | $2; }
    ;

decl_modifier
    :  LOCAL { $$ = DICTFLAGS_LOCAL; }
    |  READONLY { $$ = DICTFLAGS_READONLY; }
    ;
    
expression
    :   STRING { Code_Compile(Atom_String($1)); }
    |   ID {
	    Code_Compile(Atom_Name($1));
        } opt_rhs_qualifier_list
    |   '@' ID {
	    Code_Compile(Atom_Designator(Designator_Name($2)));
	    Code_Compile(Atom_Operator(Op_Indirect));
	}
    |   INTEGER { Code_Compile(Atom_Integer($1)); }
        /* function call */
    |   ID actual_arg_spec {
	    Code_Compile(Atom_Integer($2));
	    Code_Compile(Atom_Name($1));
	    Code_Compile(Atom_Operator(Op_FunctionCall));
	}
    |   '(' expression ')'
    |   expression '&' expression {
	    Code_Compile(Atom_Operator(Op_StringCatenate));
	}
    |   '[' {
	    Code_Compile(Atom_Operator(Op_StartArray));
	} expression_list ']' {
	    Code_Compile(Atom_Operator(Op_EndArray));
	}
    |   '{' {
	    Code_Compile(Atom_Operator(Op_StartTable));
	} table_constructor_list '}' {
	    Code_Compile(Atom_Operator(Op_EndTable));
	}
    |   expression OR expression {
            Code_Compile(Atom_Operator(Op_Or));
        }
    |   expression AND expression {
            Code_Compile(Atom_Operator(Op_And));
        }
    |   NOT expression {
            Code_Compile(Atom_Operator(Op_Not));
        }
    |   expression CONTAINS expression {
	    Code_Compile(Atom_Operator(Op_TableMembership));
	}
    |   '$' ID {
	    Code_Compile(Atom_String(String_New($2->text)));
	    Code_Compile(Atom_Operator(Op_Getenv));
	}
    ;

opt_rhs_qualifier_list
    :	/* empty */
    |   rhs_qualifier_list
    ;

rhs_qualifier_list
    :   rhs_qualifier
    |   rhs_qualifier_list rhs_qualifier
    ;

rhs_qualifier
    :   '[' expression ']' {
	    Code_Compile(Atom_Operator(Op_ArraySelection));
	}
    |   '{' expression '}' {
	    Code_Compile(Atom_Operator(Op_TableSelection));
	}
    ;

expression_list
    :	/* empty */
    |   expression
    |	expression_list ',' expression
    ;

table_constructor_list
    :	/* empty */
    |   table_element
    |	table_constructor_list ',' table_element
    ;

table_element
    :	expression {
	    Code_Compile(Atom_String(String_FromBoolean(FALSE)));
        }
    |   expression ':' expression
    ;

formal_arg_spec
    :	'(' formal_arg_list ')' {
	    $$ = $2;
	}
    ;

formal_arg_list
    :	/* empty */ { $$ = NULL; }
    |	ID { $$ = List_New($1, NULL); }
    |	formal_arg_list ',' ID { $$ = List_New($3, $1); }
    ;

actual_arg_spec
    :	'(' actual_arg_list ')' {
	    $$ = $2;
	}
    ;

actual_arg_list
    :	/* empty */ { $$ = 0; }
    |	expression { $$ = 1; }
    |	actual_arg_list ',' expression { $$ = $1 + 1; }
    ;

%%

static void GenerateAssignment(flags)
DictFlags flags;
{
    if (flags == DICTFLAGS_NONE) {
	Code_Compile(Atom_Operator(Op_Assign));
    } else {
	Code_Compile(Atom_Integer(flags));
	Code_Compile(Atom_Operator(Op_AssignWithOptions));
    }
}

void Parser_ErrorFormat(stream, path_prefix, file_name, line_number)
FILE *stream;
String path_prefix;
String file_name;
int line_number;
{
    if (!String_IsEmpty(path_prefix) &&
	!String_IsEmpty(file_name) &&
	line_number > 0)
	fprintf(stream, "\"%s%c%s\", line %d: ",
		path_prefix->body,
		DIR_SEPARATOR,
		file_name->body,
		line_number);
}

#ifdef __STDC__
void yywarning(char *format, ...)
#else
void yywarning(format, va_alist)
char *format;
va_dcl
#endif
{
    va_list args;

    va_init(args, format);

    Parser_ErrorFormat(stderr,
		       Parser_PathPrefix,
		       Parser_FileName,
		       Parser_LineNumber);
    
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

    va_end(args);
}

#ifdef __STDC__
int yyerror(char *format, ...)
#else
int yyerror(format, va_alist)
char *format;
va_dcl
#endif
{
    va_list args;

    va_init(args, format);
    Parser_ErrorFormat(stderr,
		       Parser_PathPrefix,
		       Parser_FileName,
		       Parser_LineNumber);
    
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

    CallStack_BackTrace(stderr);

    FileIO_Finalize();

    exit(1);
    return 0;
}

void Parser_Initialize()
{
    Parser_PathPrefix = String_New("");
    Parser_FileName = String_New("");
    Parser_LineNumber = -1;
}
