/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed May 11 13:49:10 PDT 1994 by kalsow  
 *      modified on Thu Aug 26 12:12:57 PDT 1993 by harrison
 */

#include "quake.h"

typedef struct Symbol {
    Atom_ForwardDecl atom;
    Boolean readonly;
} *Symbol;

static List DictionaryStack = NULL;
static Hash_Table GlobalDictionary = NULL;

void Dict_Initialize()
{
    GlobalDictionary = Hash_NewTable(0);
    List_Push(&DictionaryStack, GlobalDictionary);
}

static BulkAlloc *BulkSymbols = NULL;

static Symbol Symbol_New(atom, flags)
Atom atom;
DictFlags flags;
{
    Symbol symbol;

    if (BulkSymbols == NULL)
	BulkSymbols = Utils_NewBulkAlloc(sizeof(struct Symbol), 1000);

    symbol = (Symbol) Utils_GetBulk(BulkSymbols);

    symbol->atom = atom;
    symbol->readonly = (flags & DICTFLAGS_READONLY) != 0;

    return symbol;
}

Atom Dict_Lookup(name, create)
Name name;
Boolean create;
{
    List f;
    Hash_Bucket bucket = NULL;

    for (f = DictionaryStack; bucket == NULL && f != NULL; f = f->tail)
	bucket = Hash_FindName((Dictionary) f->first, name);

    if (bucket != NULL) {
	Symbol symbol = (Symbol) bucket->data;
	
	return symbol->atom;
    }
    
    return create ? Atom_String(String_New(name->text)) : NULL;
}

Atom *Dict_Install(name, atom, flags)
Name name;
Atom atom;
DictFlags flags;
{
    Hash_Bucket bucket = NULL;
    Dictionary the_dict = GlobalDictionary;
    List f;

    for (f = DictionaryStack; f != NULL; f = f->tail) {
	Dictionary d = (Dictionary) f->first;

	if ((flags & DICTFLAGS_LOCAL) != 0) {
	    the_dict = d;
	    break;
	}

	if ((bucket = Hash_FindName(d, name)) != NULL)
	    break;
    }

    if (bucket == NULL)
	bucket = Hash_InsertName(the_dict, name);

    {
	Symbol symbol = (Symbol) bucket->data;

	if (symbol == NULL) {
	    bucket->data = (Refany) Symbol_New(atom, flags);
	} else {
	    if (symbol->readonly)
		yyerror("\"%s\" is read only--you cannot change its value", name->text);

	    symbol->atom = atom;
	    symbol->readonly = (flags & DICTFLAGS_READONLY) != 0;
	}

	return &(((Symbol) bucket->data)->atom);
    }
}

void Dict_IncrementScope()
{
    List_Push(&DictionaryStack, Hash_NewTable(0));
}

void Dict_DecrementScope()
{
    Hash_Destroy(List_Pop(&DictionaryStack));
}

static FILE *DumpStream = NULL;

static void DumpDictEntry(hash_bucket)
Hash_Bucket hash_bucket;
{
    Symbol symbol = (Symbol) hash_bucket->data;
    Atom atom = symbol->atom;

    fprintf(DumpStream, "%s", hash_bucket->key);

    if (symbol->readonly)
	fprintf(DumpStream, "[RO]");

#ifdef undef
    fprintf(DumpStream, ": (");

    if (atom->context->file_name == NULL)
	fprintf(DumpStream, "???");
    else
	fprintf(DumpStream, "%s", atom->context->file_name->body);

    if (atom->context->line_number == -1)
	fprintf(DumpStream, ", ???");
    else
	fprintf(DumpStream, ", %d", atom->context->line_number);

    fprintf(DumpStream, ")\t");
#endif

    Atom_Dump(DumpStream, atom);

    fprintf(DumpStream, "\n");
}

static void DumpDict(stream, table)
FILE *stream;
Hash_Table table;
{
    Hash_DumpTable(stream, table);

    DumpStream = stream;
    Hash_Walk(table, DumpDictEntry);
}

void Dict_DumpAll(stream)
FILE *stream;
{
    List f;

    for (f = DictionaryStack; f != NULL; f = f->tail) {
	Dictionary dict = (Dictionary) f->first;

	if (dict == GlobalDictionary)
	    fprintf(stream, "<<<<<< Global Dictionary >>>>>>\n");
	else
	    fprintf(stream, "<<<<<< Local Dictionary >>>>>>\n");

	DumpDict(stream, dict);
    }
}

void Dict_DumpAllToStderr()
{
    Dict_DumpAll(stderr);
}

