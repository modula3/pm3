/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Jan 21 16:03:53 1994 by harrison
 */

#ifndef ATOM_H
#define ATOM_H

typedef enum {
    Tag_Void, 	/* Not an Atom tag */

    Tag_Array,
    Tag_Builtin,
    Tag_Code,
    Tag_Designator,
    Tag_Integer,
    Tag_Mark,
    Tag_Name,
    Tag_Operator,
    Tag_Procedure,
    Tag_String,
    Tag_Table,

    Tag_FileLocation,
    Tag_LineNumber
} Tag;

typedef ExitCode (*Operator) ARGS((void));

typedef ExitCode (*BuiltinOperator) ARGS((int argc));

typedef struct Atom {
    Tag tag;
    union {
	Array array;
	Code code;
	Integer integer;
	Name name;
	Operator operator;
	Table table;
	String string;
	struct Builtin *builtin;
	struct Designator *designator;
	struct Procedure *procedure;
	struct FileLocation *file_location;
	int line_number;
    } u;
} *Atom;

#define VARIADIC_ARGS	(-1)

typedef struct Procedure {
    Name name;
    Boolean is_function;
    int argc;			/* # args */
    Name *arg_names;		/* array of names, or NULL if argc == 0 */
    Code code;    
} *Procedure;

typedef struct Builtin {
    Name name;
    Boolean is_function;
    int argc;			/* # args, or VARIADIC_ARGS */
    BuiltinOperator operator;
} *Builtin;

typedef enum {
    DesignatorType_Void, /* Not a designator type */
    DesignatorType_Name,
    DesignatorType_Array,
    DesignatorType_Table
} DesignatorType;

typedef struct Designator {
    DesignatorType type;
    union {
	Name name;
	struct {
	    Array array;
	    Atom idx;
	} array;
	struct {
	    Table table;
	    Atom key;
	} table;
    } u;
} *Designator;

extern void
    Atom_Initialize ARGS((void)),
    Atom_Dump ARGS((FILE *stream, Atom atom));

extern char
    *Atom_TagText ARGS((Tag tag));

extern Atom
    Atom_Boolean ARGS((Boolean boolean)), /* actually returns a string atom */
    Atom_Array ARGS((Array array)),
    Atom_Code ARGS((Code code)),
    Atom_CheckType ARGS((Atom atom, Tag tag)),
    Atom_Builtin ARGS((Name name,
		 Boolean is_function,
		 int argc,
		 BuiltinOperator operator)),
    Atom_Designator ARGS((Designator designator)),
    Atom_Flatten ARGS((Atom atom)),
    Atom_Integer ARGS((Integer integer)),
    Atom_Mark ARGS((void)),
    Atom_Name ARGS((Name name)),
    Atom_Operator ARGS((Operator operator)),
    Atom_Procedure ARGS((Name name, List args, Boolean is_function, Code code)),
    Atom_Table ARGS((Table table)),
    Atom_Skeleton ARGS((Tag tag)),
    Atom_String ARGS((String string)),
    Atom_FileLocation ARGS((String path_prefix, String file_name)),
    Atom_LineNumber ARGS((int line_number));

extern String
    Atom_GetString ARGS((Atom atom));

extern Array
    Atom_GetArray ARGS((Atom atom));

extern Table
    Atom_GetTable ARGS((Atom atom));

/*
 * Designator stuff
 */

extern Atom
    Designator_Get ARGS((Designator designator));

extern Designator
    Designator_Name ARGS((Name name)),
    Designator_Array ARGS((Array array, Atom inx)),
    Designator_Table ARGS((Table table, Atom key));

#endif /* ATOM_H */
