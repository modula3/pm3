/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Tue Jan 18 15:26:49 1994 by harrison 
 *      modified on Wed Apr 28 22:29:05 PDT 1993 by mjordan 
 */

#include "quake.h"

static TableData DataNotFound = NULL;

Table Table_New()
{
    Table table = NEW(struct Table);

    table->body = Hash_NewTable(0);

    return table;
}

static TableData NewTableData(key, value)
Table_KeyType key;
Table_ValueType value;
{
    TableData table_data = NEW(struct TableData);

    table_data->key_atom = key;
    table_data->value_atom = value;

    return table_data;
}

Table Table_InsertAtom(table, key, value)
Table table;
Table_KeyType key;
Table_ValueType value;
{
    TableData table_data = NewTableData(key, value);

    switch (key->tag) {
    case Tag_String:
	Hash_InsertString(table->body, key->u.string)->data = (Refany) table_data;
	break;
    default:
	Hash_InsertString(table->body, String_Convert(key))->data = (Refany) table_data;

	/*
	 * we can't dispose of the string, since it's used in the hash table
	 */
	break;
    }
    
    return table;
}

TableData Table_Get(table, key)
Table table;
Table_KeyType key;
{
    Hash_Bucket bucket;

    switch (key->tag) {
    case Tag_String:
	bucket = Hash_FindString(table->body, key->u.string);
	break;
    default:
	bucket = Hash_FindString(table->body, String_Convert(key));
	break;
    }

    if (bucket == NULL) {
	if (DataNotFound == NULL) {
	    Atom NullAtom = Atom_Boolean(FALSE);

	    DataNotFound = NewTableData(NullAtom, NullAtom);
	}

	return DataNotFound;
    }

    return (TableData) bucket->data;
}

void Table_Put(table, key, value)
Table table;
Table_KeyType key;
Table_ValueType value;
{
    switch (key->tag) {
    case Tag_String:
	Hash_InsertString(table->body, key->u.string)->data =
	    (Refany) NewTableData(key, value);
	break;
    default:
	Hash_InsertString(table->body, String_Convert(key))->data =
	    (Refany) NewTableData(key, value);
	break;
    }
}
 
static FILE *DumpStream = NULL;
static Table DumpTable = NULL;
static char *DumpSep = "";

static void DumpCallback(bucket)
Hash_Bucket bucket;
{
    fprintf(DumpStream, "%s%s->", DumpSep, bucket->key);
    Atom_Dump(DumpStream, (Atom) ((TableData) bucket->data)->key_atom);

    DumpSep = ", ";
}

void Table_Dump(stream, table)
FILE *stream;
Table table;
{
    DumpStream = stream;
    DumpTable = table;
    DumpSep = "";

    fprintf(stream, "{");
    Hash_Walk(table->body, DumpCallback);
    fprintf(stream, "}");
}

Table Table_Convert(atom)
Atom atom;
{
    if (atom->tag == Tag_Table)
	return atom->u.table;
    else
	return Table_InsertAtom(Table_New(),
			      atom,
			      Atom_Boolean(FALSE));
}

static Table ToArrayTable = NULL;
static Array ToArrayTarget = NULL;

static void ToArrayCallback(bucket)
Hash_Bucket bucket;
{
    Array_AppendAtom(ToArrayTarget, Atom_String(String_New(bucket->key)));
}

Array Table_ToArray(table)
Table table;
{
    ToArrayTable = table;
    ToArrayTarget = Array_New(0);

    Hash_Walk(table->body, ToArrayCallback);

    return ToArrayTarget;
}

Boolean Table_IsEmpty(table)
Table table;
{
    return table->body == NULL || table->body->nEntries == 0;
}


