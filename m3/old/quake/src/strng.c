/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed May 11 16:12:10 PDT 1994 by kalsow  
 *      modified on Fri Jan 21 15:18:31 1994 by harrison
 */

#include "quake.h"

static BulkAlloc
    *BulkStrings = NULL;

static int
    String_LengthOfArray ARGS((Array array));

static void
    String_LoadCharBufferFromArray ARGS((Array array, char **p));

String String_New(body)
char *body;
{
    String string;

    if (BulkStrings == NULL)
	BulkStrings = Utils_NewBulkAlloc(sizeof(struct String), 1000);

    string = (String) Utils_GetBulk(BulkStrings);

    string->body = body == NULL ?
	Utils_StringSave("") :
	Utils_StringSave(body);

    return string;
}

static String
    String_False = NULL,
    String_True  = NULL;

String String_FromBoolean(boolean)
Boolean boolean;
{
    static initialized = FALSE;

    if (!initialized) {
	String_False = String_New("");
	String_True  = String_New("true");

	initialized = TRUE;
    }

    return boolean ?
	String_True :
	String_False;
}

Boolean String_ToBoolean(string)
String string;
{
    return !String_IsEmpty(string);
}

Boolean String_IsEmpty(string)
String string;
{
    return string->body == NULL || strlen(string->body) == 0;
}

Boolean String_Equal(string1, string2)
String string1;
String string2;
{
    return strcmp(string1->body, string2->body) == 0;
}

int String_Length(string)
String string;
{
    return string == NULL || string->body == NULL ? 0 : strlen(string->body);
}

String String_Catenate(s1, s2)
String s1;
String s2;
{
    int l1 = String_Length(s1),
        l2 = String_Length(s2);
    char *new_body = NEW_ARRAY(char, l1 + l2 + 1);

    strcpy(new_body, s1->body);
    strcpy(new_body + l1, s2->body);

    return String_New(new_body);
}

String String_Escape(s)
String s;
{
    int len = String_Length(s);
    int n_escapes = 0;
    char *p;

    for (p = s->body; *p; p++) {
      if (*p == '\\') n_escapes++;
      }

    { char *new_body = NEW_ARRAY(char, len + n_escapes + 1);
      char *q = new_body;

      for (p = s->body; *p; p++, q++) {
	*q = *p;
        if (*p == '\\') *(++q) = '\\';
        }
      *q = 0;

      return String_New(new_body);
    }
}

static int String_LengthOfAtom(atom)
Atom atom;
{
    switch (atom->tag) {
    case Tag_String:
	return strlen(atom->u.string->body);
    case Tag_Name:
	return strlen(atom->u.name->text);
    case Tag_Array:
	return String_LengthOfArray(atom->u.array);
    case Tag_Table:
	return String_LengthOfArray(Table_ToArray(atom->u.table));
    case Tag_Integer:
	{
	    char buf[100];

	    sprintf(buf, "%d", atom->u.integer);
	    return strlen(buf);
	}
    default:
	yyerror("cannot convert an object of type <%s> to a string",
		Atom_TagText(atom->tag));
	break;
    }

    return 0;			/* keep lint happy */
}

static int String_LengthOfArray(array)
Array array;
{
    int i, length = 0, separators = array->next > 0 ? array->next - 1 : 0;

    for (i = 0; i < array->next; i++) {
	RANGE_CHECK(i, 0, array->length);
	length += String_LengthOfAtom(array->body[i]);
    }

    return length + separators;
}

static void String_LoadCharBufferFromAtom(atom, p)
Atom atom;
char **p;
{
    switch (atom->tag) {
    case Tag_String:
	strcpy(*p, atom->u.string->body);
	*p += strlen(atom->u.string->body);
	break;
    case Tag_Name:
	strcpy(*p, atom->u.name->text);
	*p += strlen(atom->u.name->text);
	break;
    case Tag_Array:
	String_LoadCharBufferFromArray(atom->u.array, p);
	break;
    case Tag_Table:
	String_LoadCharBufferFromArray(Table_ToArray(atom->u.table), p);
	break;
    case Tag_Integer:
	sprintf(*p, "%d", atom->u.integer);
	*p += strlen(*p);
	break;
    default:
	yyerror("cannot convert an object of type <%s> to a string",
		Atom_TagText(atom->tag));
	break;
    }
}

static void String_LoadCharBufferFromArray(array, p)
Array array;
char **p;
{
    int i;

    for (i = 0; i < array->next; i++) {
	if (i > 0)
	    strcpy((*p)++, " ");
	RANGE_CHECK(i, 0, array->length);
	String_LoadCharBufferFromAtom(array->body[i], p);
    }
}

String String_Convert(atom)
Atom atom;
{
    switch (atom->tag) {
    case Tag_String:
	return atom->u.string;
    case Tag_Name:
	return String_New(atom->u.name->text);
    case Tag_Array: {
	int length = String_LengthOfArray(atom->u.array);
	char *body = NEW_ARRAY(char, length + 1);	
	char *p = body;
	
	String_LoadCharBufferFromArray(atom->u.array, &p);
	*p = '\0';

	return String_New(body);
    }
    case Tag_Table:
	return String_Convert(Atom_Array(Table_ToArray(atom->u.table)));
    case Tag_Integer:
	{
	    char buf[100];

	    sprintf(buf, "%d", atom->u.integer);
	    return String_New(buf);
	}
    default:
	yyerror("cannot convert an object of type <%s> to a string",
		Atom_TagText(atom->tag));
	break;
    }

    return 0;			/* keep lint happy */
}

