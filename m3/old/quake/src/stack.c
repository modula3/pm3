/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Jan 21 15:18:31 1994 by harrison
 */

#include "quake.h"

#define	STACK_INCREMENT	(1000)

static Atom *Stack = NULL;
static int StackSize = 0;

static int StackPtr = 0;

void Push(atom)
Atom atom;
{
    if (StackPtr >= StackSize) {
	StackSize += STACK_INCREMENT;
	if (Stack == NULL)
	    Stack = NEW_ARRAY(Atom, StackSize);
	else
	    Stack = RENEW_ARRAY(Stack, Atom, StackSize);
    }

    RANGE_CHECK(StackPtr, 0, StackSize);
    Stack[StackPtr++] = Atom_Flatten(atom);
}

#ifdef __STDC__
Atom Pop(Tag tag)
#else
Atom Pop(tag)
Tag tag;
#endif
{
    if (--StackPtr < 0)
	yyerror("internal error: stack underflow");
    if (tag != Tag_Void) {
	RANGE_CHECK(StackPtr, 0, StackSize);
	Atom_CheckType(Stack[StackPtr], tag);
    }

    return Stack[StackPtr];
}

Atom Pop_Any()
{
    return Pop(Tag_Void);
}

Integer Pop_Integer()
{
    return Pop(Tag_Integer)->u.integer;
}

void Pop_Mark()
{
    Pop(Tag_Mark);
}

String Pop_String()
{
    return Pop(Tag_String)->u.string;
}

Array Pop_Array()
{
    return Pop(Tag_Array)->u.array;
}

Code Pop_Code()
{
    return Pop(Tag_Code)->u.code;
}

Designator Pop_Designator()
{
    return Pop(Tag_Designator)->u.designator;
}

Table Pop_Table()
{
    return Pop(Tag_Table)->u.table;
}

int Stack_CountToMark()
{
    int sp;

    for (sp = StackPtr - 1; sp >= 0; sp--) {
	RANGE_CHECK(sp, 0, StackSize);
	if (Stack[sp]->tag == Tag_Mark)
	    return StackPtr - sp - 1;	/* mark does not count */
    }

    yyerror("internal error: unmatched mark");
    return 0;
}

void Stack_Dump(stream)
FILE *stream;
{
    int i;

    fprintf(stream, "stack --------\n");
    for (i = StackPtr - 1; i >= 0; i--) {
	Atom_Dump(stream, Stack[i]);
	fprintf(stream, "\n");
    }
    fprintf(stream, "--------------\n");
}

