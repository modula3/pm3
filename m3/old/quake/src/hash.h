/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Jan 21 16:32:06 1994 by harrison 
 */

#ifndef HASH_H
#define HASH_H

typedef unsigned int HashValue;

typedef struct Hash_Bucket {
    char *key;
    HashValue hash_value;
    Refany data;
} *Hash_Bucket;

typedef struct Hash_Table {
    List *buckets;		/* List of Hash_Bucket */
    int nBuckets, nEntries;
#ifdef HASH_PROFILE
    int inserts, finds, probes, rehashes;
#endif
} *Hash_Table;

extern Hash_Bucket
    Hash_FindName   ARGS((Hash_Table table, Name name)),
    Hash_DeleteName ARGS((Hash_Table table, Name name)),
    Hash_InsertName ARGS((Hash_Table table, Name name)),

    Hash_FindString   ARGS((Hash_Table table, String string)),
    Hash_DeleteString ARGS((Hash_Table table, String string)),
    Hash_InsertString ARGS((Hash_Table table, String string)),

    Hash_FindChars   ARGS((Hash_Table table, char *chars)),
    Hash_DeleteChars ARGS((Hash_Table table, char *chars)),
    Hash_InsertChars ARGS((Hash_Table table, char *chars));

extern HashValue
    Hash_Chars ARGS((char *chars)),
    Hash_String ARGS((String string));

extern Hash_Table
    Hash_InitializeTable ARGS((Hash_Table table)),
    Hash_NewTable ARGS((int initial_buckets));

typedef void (*Hash_WalkProc) ARGS((Hash_Bucket bucket));

extern void
    Hash_Walk ARGS((Hash_Table table, Hash_WalkProc proc)),
    Hash_Destroy ARGS((Hash_Table table)),
    Hash_DumpTable ARGS((FILE *stream, Hash_Table table));

/* Default number of buckets in the hash table. */
#define DEFAULT_HASH_BUCKETS (1)

#endif /* HASH_H */
