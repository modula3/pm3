/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Tue Sep 20 17:09:36 PDT 1994 by kalsow  
 *      modified on Mon Feb 21 23:14:31 1994 by harrison
 */

#include "quake.h"

typedef struct CallEntry {
    Atom what;
    FileLocation file_location;
    int line_number;
} *CallEntry;

static List CallStack = NULL;

static char *ProcOrFunc(is_function)
Boolean is_function;
{
    return is_function ? "function" : "procedure";
}

static void DumpCallEntry(stream, e)
FILE *stream;
CallEntry e;
{
    switch (e->what->tag) {
    case Tag_Builtin:
	fprintf(stream, "call to built-in %s",
		e->what->u.builtin->name->text);
	break;
    case Tag_Procedure: {
	Procedure procedure = e->what->u.procedure;

	fprintf(stream, "call to %s %s",
		ProcOrFunc(procedure->is_function),
		procedure->name->text);
	break;
    }
    default:
	yyerror("internal error: in DumpCallEntry");
	break;
    }
}

static void PushCall(what)
Atom what;
{
    CallEntry e = NEW(struct CallEntry);

    e->what = what;

    e->file_location = NEW(struct FileLocation);
    e->file_location->path_prefix = Parser_PathPrefix;
    e->file_location->file_name   = Parser_FileName;
    e->line_number                = Parser_LineNumber;

    List_Push(&CallStack, e);
}

static void PopCall()
{
    CallEntry e = (CallEntry) List_Pop(&CallStack);

    Utils_FreeMemory(e->file_location);
    Utils_FreeMemory(e);
}

void CallStack_BackTrace(stream)
FILE *stream;
{
    List f;

    if (CallStack == NULL)
	return;

    fprintf(stream, "*** call stack ***\n");

    for (f = CallStack; f != NULL; f = f->tail) {
	CallEntry e = (CallEntry) f->first;

	Parser_ErrorFormat(stream,
			   e->file_location->path_prefix,
			   e->file_location->file_name,
			   e->line_number);

	DumpCallEntry(stream, e);

	fprintf(stream, "\n");
    }
}

static ExitCode GenericAssign(dict_flags)
DictFlags dict_flags;
{
    Atom value = Pop_Any();
    Designator designator = Pop_Designator();
    
    switch (designator->type) {
    case DesignatorType_Name:
	Dict_Install(designator->u.name, value, dict_flags);
	break;
    case DesignatorType_Array: {
	Array array = designator->u.array.array;
	int inx = Array_GetIndex(array, designator->u.array.idx);

	RANGE_CHECK(inx, 0, array->length);
	array->body[inx] = value;
	break;
    }
    case DesignatorType_Table: {
	Table table = designator->u.table.table;
	Atom key = designator->u.table.key;

	Table_Put(table, key, value);
	break;
    }
    default:
	yyerror("internal error: invalid designator (%d) found in GenericAssign", designator->type);
	break;
    }

    return Exit_OK;
}

ExitCode Op_Assign()
{
    return GenericAssign(DICTFLAGS_NONE);
}

ExitCode Op_AssignWithOptions()
{
    return GenericAssign(Pop_Integer());
}

ExitCode Op_Append()
{
    Atom rhs = Pop_Any();
    Atom atom = Designator_Get(Pop_Designator());

    switch(atom->tag) {
    case Tag_Array:
	Array_AppendArray(atom->u.array, Array_Convert(rhs));
	break;
    default:
	yyerror("can only use '+=' (the append operator) on arrays");
	break;
    }

    return Exit_OK;
}

ExitCode Op_StartArray()
{
    Push(Atom_Mark());

    return Exit_OK;
}

ExitCode Op_StartTable()
{
    Push(Atom_Mark());

    return Exit_OK;
}

ExitCode Op_StringCatenate()
{
    String str2 = String_Convert(Pop_Any()),
           str1 = String_Convert(Pop_Any());

    Push(Atom_String(String_Catenate(str1, str2)));

    return Exit_OK;
}

ExitCode Op_EndArray()
{
    int length = Stack_CountToMark();
    Array array = Array_New(length);
    int i;
    
    /* save the stack in /array/ (in reverse order) */
    for (i = length - 1; i >= 0; i--) {
	RANGE_CHECK(i, 0, length);
	array->body[i] = Pop_Any();
    }
    array->next = length;	/* fix up the array */

    Pop_Mark();
    Push(Atom_Array(array));

    return Exit_OK;
}

ExitCode Op_EndTable()
{
    int length = Stack_CountToMark();
    Table table = Table_New();
    int i;

    if (length % 2 != 0)
	yyerror("internal error: odd number of elements constructing a table");

    for (i = 0; i < length; i += 2) {
	Atom value = Pop_Any();
	Atom key   = Pop_Any();

	Table_InsertAtom(table, key, value);
    }

    Pop_Mark();
    Push(Atom_Table(table));

    return Exit_OK;
}

ExitCode Op_If()
{
    Code code = Pop_Code();
    Boolean cond = String_ToBoolean(Pop_String());

    if (cond)
	return Execute_Code(code);

    return Exit_OK;
}

ExitCode Op_IfElse()
{
    Code
	false_code = Pop_Code(),
	true_code  = Pop_Code();
    
    return Execute_Code(String_ToBoolean(Pop_String()) ?
			true_code :
			false_code);
}

ExitCode Op_Indirect()
{
    Designator designator = Pop_Designator();

    switch (designator->type) {
    case DesignatorType_Name: {
	Name name = designator->u.name;
	Atom atom = Designator_Get(designator);

	if (atom == NULL)
	    yyerror("\"%s\" is undefined", name->text);

	Push(Atom_Designator(Designator_Name(Name_Convert(atom))));
	break;
    }
    default:
	yyerror("internal error: invalid designator type (%d) in Op_Indirect", designator->type);
	break;
    }

    return Exit_OK;
}

/*
 * We wade into the dictionary a little.  This is saves us some time, but is
 * a poor excuse for sloppy design of that interface.
 */
static ExitCode ForeachArray(array, subject, code)
Array array;
Name subject;
Code code;
{
    ExitCode e = Exit_OK;
    int i;
    Atom *atom = Dict_Install(subject, NULL, DICTFLAGS_LOCAL);

    for (i = 0; i < array->next; i++) {
	RANGE_CHECK(i, 0, array->length);
	*atom = array->body[i];
	if ((e = Execute_Code(code)) != Exit_OK)
	    break;
    }

    return e;
}

static ExitCode ForeachTable(table, subject, code)
Table table;
Name subject;
Code code;
{
    List *l;
    Atom *atom = Dict_Install(subject, NULL, DICTFLAGS_LOCAL);

    for (l = table->body->buckets;
	 l < table->body->buckets + table->body->nBuckets;
	 l++) {
	if (*l != NULL) {
	    List b;

	    for (b = *l; b != NULL; b = b->tail) {
		Hash_Bucket hash_bucket = (Hash_Bucket) b->first;
		TableData table_data = (TableData) hash_bucket->data;
		ExitCode e;

		*atom = table_data->key_atom;
		
		if ((e = Execute_Code(code)) != Exit_OK)
		    return e;
	    }
	}
    }

    return Exit_OK;
}

ExitCode Op_Foreach()
{
    Code code = Pop_Code();
    Atom atom = Pop_Any();
    Designator designator = Pop_Designator();
    Name subject;
    ExitCode e = Exit_OK;

    switch (designator->type) {
    case DesignatorType_Name:
	subject = designator->u.name;
	break;
    default:
	yyerror("internal error: invalid designator type (%s) in Op_Foreach", designator->type);
	break;
    }

    Dict_IncrementScope(); /* So that /subject/ gets its own local value */

    switch (atom->tag) {
    case Tag_Array:
	e = ForeachArray(atom->u.array, subject, code);
	break;
    case Tag_Table:
	e = ForeachTable(atom->u.table, subject, code);
	break;
    default:
	Dict_Install(subject, atom, DICTFLAGS_LOCAL);
	e = Execute_Code(code);
	break;
    }

    Dict_DecrementScope();
	
    return e;
}

/* Make sure we always evaluate both Pop_Strings, and don't fall foul to C's
 * evaluation rules. */

ExitCode Op_And()
{
    Boolean b1 = String_ToBoolean(Pop_String());
    Boolean b2 = String_ToBoolean(Pop_String());

    Push(Atom_Boolean(b1 && b2));

    return Exit_OK;
}

ExitCode Op_Or()
{
    Boolean b1 = String_ToBoolean(Pop_String());
    Boolean b2 = String_ToBoolean(Pop_String());

    Push(Atom_Boolean(b1 || b2));

    return Exit_OK;
}

ExitCode Op_Not()
{
    Push(Atom_Boolean(!String_ToBoolean(Pop_String())));

    return Exit_OK;
}

static ExitCode GenericCall(expecting_function)
Boolean expecting_function;
{
    Atom what = Pop_Any();
    ExitCode e = Exit_OK;

    switch (what->tag) {
    case Tag_Procedure: {
	Procedure procedure = what->u.procedure;
	Integer argc = Pop_Integer();
	int arg;
	
	PushCall(what);

	if (expecting_function != procedure->is_function)
	    yyerror("call to %s is out of context",
		    ProcOrFunc(procedure->is_function));
	if (argc != procedure->argc)
	    yyerror("expecting %d arg%s for %s \"%s\": found %d", 
		    procedure->argc,
		    procedure->argc == 1 ? "" : "s",
		    ProcOrFunc(procedure->is_function),
		    procedure->name->text,
		    argc);
	
	Dict_IncrementScope();
	
	for (arg = 0; arg < procedure->argc; arg++)
	    Dict_Install(procedure->arg_names[arg],
			 Pop_Any(),
			 DICTFLAGS_LOCAL);

	Execute_Code(procedure->code);

	Dict_DecrementScope();

	PopCall();

	break;
    }
    case Tag_Builtin: {
	Builtin builtin = what->u.builtin;
	Integer argc = Pop_Integer();
	
	PushCall(what);

	if (builtin->is_function != expecting_function)
	    yyerror("built-in \"%s\" is a %s, you must call it in the proper context",
			builtin->name->text,
			ProcOrFunc(builtin->is_function));
	if (builtin->argc != VARIADIC_ARGS &&
	    argc != builtin->argc)
	    yyerror("expecting %d arg(s) for builtin \"%s\": found %d\n", builtin->argc, builtin->name->text, argc);

	e = (*builtin->operator)(argc);
	
	PopCall();

	break;
    }
    default:
#ifdef undef
	yyerror("\"%s\" is not a user-defined or a built-in %s",
		Parser_ErrorLastName->text,
		ProcOrFunc(expecting_function));
#endif

	yyerror("undefined %s", ProcOrFunc(expecting_function));
	break;
    }

    return e;
}

ExitCode Op_ProcedureCall()
{
    return GenericCall(FALSE);
}

ExitCode Op_FunctionCall()
{
    return GenericCall(TRUE);
}

ExitCode Op_Return()
{
    return Exit_Return;
}

ExitCode Op_ReturnValue()
{
    return Exit_Return;
}

ExitCode Op_Getenv()
{
    String string = Pop_String();
    char *env = getenv(string->body);
    
    if (env == NULL)
	yyerror("environment variable \"%s\" is undefined", string->body);

    Push(Atom_String(String_New(env)));

    return Exit_OK;
}

static ExitCode GenericRedirect(mode)
char *mode;
{
    String file = Pop_String();
    FILE *fp;

    if (strcmp(file->body, STDOUT_NAME) == 0)
	fp = stdout;
    else if (strcmp(file->body, STDERR_NAME) == 0)
	fp = stderr;
    else if ((fp = fopen(file->body, mode)) == NULL)
	yyerror("cannot open file \"%s\" (in mode \"%s\")", file->body, mode);
    
    IOStack_Push(fp);

    return Exit_OK;
}    

ExitCode Op_StartRedirect()
{
    return GenericRedirect("wb");
}

ExitCode Op_StartAppendRedirect()
{
    return GenericRedirect("ab");
}

ExitCode Op_EndRedirect()
{
    FILE *fp = IOStack_Pop();

    if (fp != stdout && fp != stderr)
	fclose(fp);

    return Exit_OK;
}

ExitCode Op_ArraySelection()
{
    Atom idx = Pop_Any();
    Array array = Pop_Array();
    int inx = Array_GetIndex(array, idx);

    RANGE_CHECK(inx, 0, array->length);
    Push(array->body[inx]);

    return Exit_OK;
}

ExitCode Op_TableSelection()
{
    String key = Pop_String();
    Table table = Pop_Table();
    Hash_Bucket bucket = Hash_FindString(table->body, key);

    if (bucket == NULL)
	yyerror("table does not contain an entry for \"%s\"", key->body);

    Push(((TableData) bucket->data)->value_atom);

    return Exit_OK;
}

ExitCode Op_ArrayDesignator()
{
    Atom inx = Pop_Any();
    Array array = Atom_GetArray(Designator_Get(Pop_Designator()));

    Push(Atom_Designator(Designator_Array(array, inx)));

    return Exit_OK;
}

ExitCode Op_TableDesignator()
{
    Atom key = Pop_Any();
    Table table = Atom_GetTable(Designator_Get(Pop_Designator()));

    Push(Atom_Designator(Designator_Table(table, key)));

    return Exit_OK;
}

/*
 * Swap the top two elements of the stack
 */

ExitCode Op_Exchange()
{
    Atom top = Pop_Any();
    Atom next = Pop_Any();

    Push(top);
    Push(next);

    return Exit_OK;
}

ExitCode Op_TableMembership()
{
    String key = Pop_String();
    Table table = Pop_Table();

    Push(Atom_Boolean(Hash_FindString(table->body, key) != NULL));

    return Exit_OK;
}
