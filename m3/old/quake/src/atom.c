/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon May  2 12:08:35 PDT 1994 by harrison
 */

#include "quake.h"

typedef struct {
    Tag tag;   /* sanity check */
    char *text;
} TagInfoEntry;

static TagInfoEntry TagInfo[] = {
    { Tag_Void, "void" },

    { Tag_Array, "array" },
    { Tag_Builtin, "builtin" },
    { Tag_Code, "code" },
    { Tag_Designator, "designator" },
    { Tag_Integer, "integer" },
    { Tag_Mark, "mark" },
    { Tag_Name, "name" },
    { Tag_Operator, "operator" },
    { Tag_Procedure, "procedure" },
    { Tag_String, "string" },
    { Tag_Table, "table" },

    { Tag_FileLocation, "file_location" },
    { Tag_LineNumber, "line_number" }
};

Atom Atom_Boolean(boolean)
Boolean boolean;
{
    static Atom
	atom_true = NULL,
	atom_false = NULL;

    if (atom_true == NULL) {
	atom_true  = Atom_String(String_FromBoolean(TRUE));
	atom_false = Atom_String(String_FromBoolean(FALSE));
    }

    return boolean ? atom_true : atom_false;
}
	    
void Atom_Initialize()
{
    int i;

    for (i = 0; i < NUMBER(TagInfo); i++)
	if (TagInfo[i].tag != (Tag) i)
	    yyerror("internal error: taginfo entry (%d) invalid", i);

}

void Atom_Dump(stream, atom)
FILE *stream;
Atom atom;
{
    static Boolean tag_info_ok = FALSE;

    if (!tag_info_ok) {
	int i;

	for (i = 0; i < NUMBER(TagInfo); i++)
	    if ((Tag) i != TagInfo[i].tag)
		yyerror("internal error: taginfo[%d] (%s?) out of sequence", i, TagInfo[i].text);
	tag_info_ok = TRUE;
    }

    if (atom == NULL) {
	fprintf(stream, "NULL");
    } else {
	switch (atom->tag) {
	case Tag_Array:
	    Array_Dump(stream, atom->u.array);
	    break;
	case Tag_Code:
	    Code_Dump(stream, atom->u.code);
	    break;
	case Tag_Table:
	    Table_Dump(stream, atom->u.table);
	    break;
	case Tag_Integer:
	    fprintf(stream, "%d", atom->u.integer);
	    break;
	case Tag_Name:
	    fprintf(stream, "%s", atom->u.name->text);
	    break;
	case Tag_String:
	    fprintf(stream, "\"%s\"", atom->u.string->body);
	    break;
	case Tag_Designator: {
	    Designator designator = atom->u.designator;
	    switch (designator->type) {
	    case DesignatorType_Name:
		fprintf(stream, "=%s", designator->u.name->text);
		break;
	    case DesignatorType_Array:
		fprintf(stream, "=array");
		break;
	    case DesignatorType_Table:
		fprintf(stream, "=table");
		break;
	    default:
		fprintf(stream, "=???");
		break;
	    }
	    break;
	}
	case Tag_FileLocation: {
	    FileLocation file_location = atom->u.file_location;

	    fprintf(stream, "\"%s/%s\"",
		    file_location->path_prefix->body,
		    file_location->file_name->body);
	    break;
	}
	case Tag_LineNumber:
	    fprintf(stream, "%d", atom->u.line_number);
	    break;
	default:
	    fprintf(stream, "<%s>", TagInfo[(int) atom->tag].text);
	    break;
	}
    }
}

/*
 * We resort to ifdefs here, since tag will otherwise be promoted to int
 */  
#ifdef __STDC__
Atom Atom_CheckType(Atom atom, Tag tag)
#else
Atom Atom_CheckType(atom, tag)
Atom atom;
Tag tag;
#endif
{
    if (atom->tag != tag)
	yyerror("type check: wanted type <%s>, found type <%s>",
		Atom_TagText(tag),
		Atom_TagText(atom->tag));

    return atom;
}

#ifdef __STDC__
char *Atom_TagText(Tag tag)
#else
char *Atom_TagText(tag)
Tag tag;
#endif
{
    return TagInfo[(int) tag].text;
}

static BulkAlloc *BulkSkeletons = NULL;

#ifdef __STDC__
Atom Atom_Skeleton(Tag tag)
#else
Atom Atom_Skeleton(tag)
Tag tag;
#endif
{
    Atom atom;

    if (BulkSkeletons == NULL)
	BulkSkeletons = Utils_NewBulkAlloc(sizeof(struct Atom), 1000);

    atom = (Atom) Utils_GetBulk(BulkSkeletons);

    atom->tag = tag;

    return atom;
}

Atom Atom_Integer(integer)
Integer integer;
{
    Atom atom = Atom_Skeleton(Tag_Integer);

    atom->u.integer = integer;

    return atom;
}

Atom Atom_Mark()
{
    return Atom_Skeleton(Tag_Mark);
}

Atom Atom_Designator(designator)
Designator designator;
{
    Atom atom = Atom_Skeleton(Tag_Designator);

    atom->u.designator = designator;

    return atom;
}

Atom Atom_String(string)
String string;
{
    Atom atom = Atom_Skeleton(Tag_String);

    atom->u.string = string;

    return atom;
}

Atom Atom_Name(name)
Name name;
{
    Atom atom = Atom_Skeleton(Tag_Name);

    atom->u.name = name;

    return atom;
}

Atom Atom_Array(array)
Array array;
{
    Atom atom = Atom_Skeleton(Tag_Array);

    atom->u.array = array;

    return atom;
}

Atom Atom_Code(code)
Code code;
{
    Atom atom = Atom_Skeleton(Tag_Code);

    atom->u.code = code;

    return atom;
}

Atom Atom_Table(table)
Table table;
{
    Atom atom = Atom_Skeleton(Tag_Table);

    atom->u.table = table;

    return atom;
}

Atom Atom_Operator(operator)
Operator operator;
{
    Atom atom = Atom_Skeleton(Tag_Operator);
    
    atom->u.operator = operator;

    return atom;
}

Atom Atom_FileLocation(path_prefix, file_name)
String path_prefix, file_name;
{
    Atom atom = Atom_Skeleton(Tag_FileLocation);

    atom->u.file_location = NEW(struct FileLocation);
    atom->u.file_location->path_prefix = path_prefix;
    atom->u.file_location->file_name   = file_name;

    return atom;
}

Atom Atom_LineNumber(line_number)
int line_number;
{
    Atom atom = Atom_Skeleton(Tag_LineNumber);

    atom->u.line_number = line_number;

    return atom;
}

/*
 * Return NULL of and array of names
 */ 
static Name *ConvertArgs(list)
List list;
{
    int len = List_Length(list);

    if (len <= 0) {
	return (Name *) NULL;
    }

    {
	Name *args = NEW_ARRAY(Name, len);
	int i = 0;
	List f = list;

	for (; f != NULL; f = f->tail) {
	    RANGE_CHECK(i, 0, len);
	    args[i++] = (Name) f->first;
	}

	return args;
    }
}

Atom Atom_Procedure(name, args, is_function, code)
Name name;
List args;
Boolean is_function;
Code code;
{
    Atom atom = Atom_Skeleton(Tag_Procedure);
    
    atom->u.procedure = NEW(struct Procedure);
    atom->u.procedure->name = name;
    atom->u.procedure->argc = List_Length(args);
    atom->u.procedure->arg_names = ConvertArgs(args);
    atom->u.procedure->is_function = is_function;
    atom->u.procedure->code = code;

    return atom;
}

Atom Atom_Builtin(name, is_function, argc, operator)
Name name;
Boolean is_function;
int argc;
BuiltinOperator operator;
{
    Atom atom = Atom_Skeleton(Tag_Builtin);
    
    atom->u.builtin = NEW(struct Builtin);
    atom->u.builtin->is_function = is_function;
    atom->u.builtin->name = name;
    atom->u.builtin->argc = argc;
    atom->u.builtin->operator = operator;

    return atom;
}

String Atom_GetString(atom)
Atom atom;
{
    return Atom_CheckType(atom, Tag_String)->u.string;
}

Array Atom_GetArray(atom)
Atom atom;
{
    return Atom_CheckType(atom, Tag_Array)->u.array;
}

Table Atom_GetTable(atom)
Atom atom;
{
    return Atom_CheckType(atom, Tag_Table)->u.table;
}

Atom Atom_Flatten(atom)
Atom atom;
{
    switch (atom->tag) {
    case Tag_Array:
	atom->u.array = Array_Flatten(atom->u.array);
	break;
    default:
	break;
    }

    return atom;
}

/*
 * Designator stuff
 */

Atom Designator_Get(designator)
Designator designator;
{
    switch (designator->type) {
    case DesignatorType_Name:
	return Dict_Lookup(designator->u.name, TRUE);
	break;
    case DesignatorType_Array: {
	Array array = designator->u.array.array;
	int inx = Array_GetIndex(array, designator->u.array.idx);

	RANGE_CHECK(inx, 0, array->length);
	return array->body[inx];
	break;
    }
    case DesignatorType_Table: {
	Table table = designator->u.table.table;
	Atom key = designator->u.table.key;

	return Table_Get(table, key)->value_atom;
	break;
    }
    default:
	break;
    }

    yyerror("internal error: invalid designator (%d) found in Designator_Get", designator->type);
    return NULL;		/* not reached */
}

static BulkAlloc *BulkDesignators = NULL;

static Designator Designator_New(type)
DesignatorType type;
{
    Designator designator;

    if (BulkDesignators == NULL)
	BulkDesignators = Utils_NewBulkAlloc(sizeof(struct Designator), 1000);

    designator = (Designator) Utils_GetBulk(BulkDesignators);

    designator->type = type;

    return designator;
}

Designator Designator_Name(name)
Name name;
{
    Designator designator = Designator_New(DesignatorType_Name);

    designator->u.name = name;

    return designator;
}

Designator Designator_Array(array, idx)
Array array;
Atom idx;
{
    Designator designator = Designator_New(DesignatorType_Array);

    designator->u.array.array = array;
    designator->u.array.idx = idx;

    return designator;
}

Designator Designator_Table(table, key)
Table table;
Atom key;
{
    Designator designator = Designator_New(DesignatorType_Table);

    designator->u.table.table = table;
    designator->u.table.key = key;

    return designator;
}

