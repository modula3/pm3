/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Aug 25 15:55:27 PDT 1993 by harrison 
 *      modified on Tue Apr 27 11:05:34 PDT 1993 by mjordan 
 */

#ifndef TABLE_H
#define TABLE_H

typedef struct Table {
    Hash_Table body;
} *Table;

typedef Atom_ForwardDecl Table_KeyType;
typedef Atom_ForwardDecl Table_ValueType;

typedef struct TableData {
    Table_KeyType key_atom;
    Table_ValueType value_atom;
} *TableData;

extern Table
    Table_InsertAtom ARGS((Table table, Table_KeyType key, Table_ValueType value)),
    Table_Convert ARGS((Atom_ForwardDecl atom)),
    Table_New ARGS((void));

extern TableData
    Table_Get ARGS((Table table, Table_KeyType key));

extern void
    Table_Put ARGS((Table table, Table_KeyType key, Table_ValueType value));

extern Array
    Table_ToArray ARGS((Table table));

extern void
    Table_Dump ARGS((FILE *stream, Table table));

extern Boolean
    Table_IsEmpty ARGS((Table table));

#endif /* TABLE_H */
