/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Jan 21 15:18:33 1994 by harrison 
 *      modified on Wed Apr 28 17:20:57 PDT 1993 by mjordan 
 */

#include "quake.h"

static BulkAlloc *BulkArrays = NULL;

static void
    FlattenAtom ARGS((Array target, Atom atom));

Array Array_New(length)
int length;
{
    Array array;

    if (BulkArrays == NULL)
	BulkArrays = Utils_NewBulkAlloc(sizeof(struct Array), 1000);

    array = (Array) Utils_GetBulk(BulkArrays);

    array->length = length;
    array->next = 0;
    if (length > 0)
	array->body = NEW_ARRAY(Atom, length);
    else
	array->body = NULL;

    return array;
}

static Array Array_MaybeExtend(array, new_length)
Array array;
int new_length;
{
    if (new_length > array->length) {
	/* golden sectionish */
	while (new_length > array->length)
	    array->length = 1 + (1618 * array->length) / 1000;
	if (array->body == NULL)
	    array->body = NEW_ARRAY(Atom, array->length);
	else
	    array->body = RENEW_ARRAY(array->body, Atom, array->length);
    }

    return array;
}

Array Array_AppendAtom(array, atom)
Array array;
Atom atom;
{
    array = Array_MaybeExtend(array, array->next + 1);
    RANGE_CHECK(array->next, 0, array->length);
    array->body[array->next++] = atom;

    return array;
}

void Array_AppendArray(array, suffix)
Array array, suffix;
{
    int i;

    for (i = 0; i < suffix->next; i++) {
	RANGE_CHECK(i, 0, suffix->length);
	Array_AppendAtom(array, suffix->body[i]);
    }
}

void Array_Dump(stream, array)
FILE *stream;
Array array;
{
    int i;
    char *sep = "";

    fprintf(stream, "[");
    for (i = 0; i < array->next; i++) {
	fprintf(stream, "%s", sep);
	RANGE_CHECK(i, 0, array->length);
	Atom_Dump(stream, array->body[i]);
	sep = ", ";
    }
    fprintf(stream, "]");
}

Array Array_Convert(atom)
Atom atom;
{
    if (atom->tag == Tag_Array)
	return atom->u.array;
    else
	return Array_AppendAtom(Array_New(0), atom);
}

static void FlattenArray(target, array)
Array target, array;
{
    int i;

    for (i = 0; i < array->next; i++) {
	RANGE_CHECK(i, 0, array->length);
	FlattenAtom(target, array->body[i]);
    }
}

static void FlattenAtom(target, atom)
Array target;
Atom atom;
{
    switch (atom->tag) {
    case Tag_Array:
	FlattenArray(target, atom->u.array);
	break;
    default:
	Array_AppendAtom(target, atom);
	break;
    }
}

/* An array is flat unless it contains other arrays */
static Boolean ArrayIsFlat(array)
Array array;
{
    int i;

    for (i = 0; i < array->next; i++) {
	RANGE_CHECK(i, 0, array->length);
	switch (array->body[i]->tag) {
	case Tag_Array:
	    return FALSE;
	default:
	    break;
	}
    }

    return TRUE;
}

Array Array_Flatten(array)
Array array;
{
    if (ArrayIsFlat(array))
	return array;

    {
	Array new = Array_New(0);
	int i;
	
	for (i = 0; i < array->next; i++) {
	    RANGE_CHECK(i, 0, array->length);
	    FlattenAtom(new, array->body[i]);
	}

	return new;
    }
}

int Array_GetIndex(array, idx)
Array array;
Atom idx;
{
    int i;

    switch (idx->tag) {
    case Tag_Integer:
	i = idx->u.integer;
	break;
    default: {
	String string = String_Convert(idx);

	if (!Utils_ReadInteger(string->body, &i))
	    yyerror("\"%s\" cannot be converted to an integer", string->body);
    }
    }

    if (0 > i || i >= array->next)
	yyerror("index value of %d is out of range for array [0 .. %d]",
		i,
		array->next - 1);

    return i;
}

Boolean Array_IsEmpty(array)
Array array;
{
    return array->body == NULL || array->next == 0;
}

