/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Thu Aug 26 12:14:30 PDT 1993 by harrison
 */

#include "quake.h"

static Hash_Table Names = NULL;

static BulkAlloc *BulkNames = NULL;

Name Name_Register(text)
char *text;
{
    Hash_Bucket bucket;

    if (BulkNames == NULL)
	BulkNames = Utils_NewBulkAlloc(sizeof(struct Name), 1000);

    bucket = Hash_InsertChars(Names, text);

    if (bucket->data == NULL) {
	Name name = (Name) Utils_GetBulk(BulkNames);

	name->text = text;
	name->hash_value = bucket->hash_value;

	bucket->data = (Refany) name;
    }

    return (Name) bucket->data;
}

Name Name_Convert(atom)
Atom atom;
{
    return Name_Register(String_Convert(atom)->body);
}

void Name_Initialize()
{
    Names = Hash_NewTable(0);
}

void Name_Dump(stream)
FILE *stream;
{
    fprintf(stream, "<<<<<< Names >>>>>>\n");
    Hash_DumpTable(stream, Names);
}

void Name_DumpToStderr()
{
    Name_Dump(stderr);
}

