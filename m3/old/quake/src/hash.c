/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Apr 18 16:31:06 PDT 1994 by harrison
 */

#include "quake.h"

static BulkAlloc *BulkBuckets = NULL;

Hash_Table Hash_InitializeTable(hash_table)
Hash_Table hash_table;
{
    hash_table->nEntries = 0;

    Utils_ClearMemory(hash_table->buckets,
		      hash_table->nBuckets * sizeof(hash_table->buckets[0]));

    return hash_table;
}

Hash_Table Hash_NewTable(initial_buckets)
int initial_buckets;
{
    Hash_Table hash_table = NEW(struct Hash_Table);

    if (initial_buckets == 0)
	initial_buckets = DEFAULT_HASH_BUCKETS;

    hash_table->buckets = NEW_ARRAY(List, initial_buckets);
    hash_table->nBuckets = initial_buckets;

#ifdef HASH_PROFILE
    hash_table->inserts = 0;
    hash_table->finds = 0;
    hash_table->probes = 0;
    hash_table->rehashes = 0;
#endif

    return Hash_InitializeTable(hash_table);
}

static Hash_Bucket NewBucket(key, hash_value, data)
char *key;
HashValue hash_value;
Refany data;
{
    Hash_Bucket new_bucket = (Hash_Bucket) Utils_GetBulk(BulkBuckets);
	
    new_bucket->key        = key;
    new_bucket->hash_value = hash_value;
    new_bucket->data       = data;

    return new_bucket;
}

HashValue Hash_Chars(chars)
char *chars;
{
    static int random_numbers[256];
    static Boolean initialized = FALSE;

    HashValue hash_value = 0;

    if (!initialized) {
	int i;

	for (i = 0; i < NUMBER(random_numbers); i++) {
	    /* Some versions of HP/UX and Solaris do not appear to have
               random(3) in easy-to-find libraries.  Use the rather
               less-random rand(3) instead. */
	    random_numbers[i] = rand();
	}

	initialized = TRUE;
    }

    {
	int i;

	for (i = 0; chars[i] != '\0'; i++)
	    hash_value = (hash_value << 1) ^
		random_numbers[chars[i] % NUMBER(random_numbers)];
    }

    return hash_value;
}

HashValue Hash_String(string)
String string;
{
    return Hash_Chars(string->body);
}

static Boolean KeysEqual(key1, hash_value1, key2, hash_value2)
char *key1;
HashValue hash_value1;
char *key2;
HashValue hash_value2;
{
    return hash_value1 == hash_value2 && strcmp(key1, key2) == 0;
}

static Hash_Bucket GenericFind(hash_table, key, hash_value)
Hash_Table hash_table;
char *key;
HashValue hash_value;
{
    List l;

#ifdef HASH_PROFILE
    hash_table->finds++;
#endif
    RANGE_CHECK(hash_value % hash_table->nBuckets, 0, hash_table->nBuckets);
    for (l = hash_table->buckets[hash_value % hash_table->nBuckets];
	 l != NULL;
	 l = l->tail) {
	Hash_Bucket b = (Hash_Bucket) l->first;

#ifdef HASH_PROFILE
	hash_table->probes++;
#endif
	if (b->hash_value == hash_value && strcmp(b->key, key) == 0)
	    return b;
    }

    return (Hash_Bucket) NULL;
}

static Hash_Bucket GenericDelete(hash_table, key, hash_value)
Hash_Table hash_table;
char *key;
HashValue hash_value;
{
    int bucket_index = hash_value % hash_table->nBuckets;
    List prev = NULL, temp;

    RANGE_CHECK(bucket_index, 0, hash_table->nBuckets);
    for (temp = hash_table->buckets[bucket_index];
	 temp != NULL;
	 temp = temp->tail) {
	Hash_Bucket b = (Hash_Bucket) temp->first;

	if (KeysEqual(b->key, b->hash_value, key, hash_value)) {
	    if (prev != NULL)
		prev->tail = temp->tail;
	    else
		hash_table->buckets[bucket_index] = temp->tail;

	    hash_table->nEntries--;

	    return b;
	}
	prev = temp;
    }

    return (Hash_Bucket) NULL;
}

static Hash_Bucket GenericInsert(hash_table, key, hash_value)
Hash_Table hash_table;
char *key;
HashValue hash_value;
{
    List l;

#ifdef HASH_PROFILE
    hash_table->inserts++;
#endif
    RANGE_CHECK(hash_value % hash_table->nBuckets, 0, hash_table->nBuckets);
    for (l = hash_table->buckets[hash_value % hash_table->nBuckets];
	 l != NULL;
	 l = l->tail) {
	Hash_Bucket b = (Hash_Bucket) l->first;

#ifdef HASH_PROFILE
	hash_table->probes++;
#endif
	if (KeysEqual(b->key, b->hash_value, key, hash_value))
	    return b;
    }

    if (BulkBuckets == NULL)
	BulkBuckets = Utils_NewBulkAlloc(sizeof(BulkBuckets[0]), 1000);

    /*
     * Expand table if more than 62% crowded
     */
    if (hash_table->nEntries * 162 > hash_table->nBuckets * 100) {
	/* always expand by at least 1 */
	int new_length = 1 + hash_table->nBuckets * 162 / 100;
	List *new_buckets = NEW_ARRAY(List, new_length);

#ifdef HASH_PROFILE
    hash_table->rehashes++;
#endif
	Utils_ClearMemory(new_buckets, new_length * sizeof(new_buckets[0]));
	
	/* move all entries to buckets in the new hash table */
	{
	    int i;

	    for (i = 0; i < hash_table->nBuckets; i++)
		while (hash_table->buckets[i] != NULL) {
		    Hash_Bucket b = (Hash_Bucket) hash_table->buckets[i]->first;
		    int new_index = b->hash_value % new_length;
		    List new_head = new_buckets[new_index];
		    
		    RANGE_CHECK(i, 0, hash_table->nBuckets);
		    RANGE_CHECK(new_index, 0, new_length);
		    new_buckets[new_index] = hash_table->buckets[i];
		    hash_table->buckets[i] = hash_table->buckets[i]->tail;
		    new_buckets[new_index]->tail = new_head;
		}
	}

	hash_table->nBuckets = new_length;

	Utils_FreeMemory(hash_table->buckets);
	hash_table->buckets = new_buckets;
    }

    {
	Hash_Bucket new_bucket = NewBucket(key, hash_value, (Refany) NULL);

	RANGE_CHECK(hash_value % hash_table->nBuckets, 0, hash_table->nBuckets);
	List_Push(&hash_table->buckets[hash_value % hash_table->nBuckets], new_bucket);
	
	hash_table->nEntries++;
	
	return new_bucket;
    }
}

Hash_Bucket Hash_FindName(hash_table, name)
Hash_Table hash_table;
Name name;
{
    return GenericFind(hash_table, name->text, name->hash_value);
}

Hash_Bucket Hash_FindString(hash_table, string)
Hash_Table hash_table;
String string;
{
    return GenericFind(hash_table, string->body, Hash_String(string));
}

Hash_Bucket Hash_FindChars(hash_table, chars)
Hash_Table hash_table;
char *chars;
{
    return GenericFind(hash_table, chars, Hash_Chars(chars));
}

Hash_Bucket Hash_InsertName(hash_table, name)
Hash_Table hash_table;
Name name;
{
    return GenericInsert(hash_table, name->text, name->hash_value);
}

Hash_Bucket Hash_InsertString(hash_table, string)
Hash_Table hash_table;
String string;
{
    return GenericInsert(hash_table, string->body, Hash_String(string));
}

Hash_Bucket Hash_InsertChars(hash_table, chars)
Hash_Table hash_table;
char *chars;
{
    return GenericInsert(hash_table, chars, Hash_Chars(chars));
}

Hash_Bucket Hash_DeleteName(hash_table, name)
Hash_Table hash_table;
Name name;
{
    return GenericDelete(hash_table, name->text, name->hash_value);
}

Hash_Bucket Hash_DeleteString(hash_table, string)
Hash_Table hash_table;
String string;
{
    return GenericDelete(hash_table, string->body, Hash_String(string));
}

Hash_Bucket Hash_DeleteChars(hash_table, chars)
Hash_Table hash_table;
char *chars;
{
    return GenericDelete(hash_table, chars, Hash_Chars(chars));
}

void Hash_Destroy(hash_table)
Hash_Table hash_table;
{
    int i;

    for (i = 0; i < hash_table->nBuckets; i++) {
	List l = hash_table->buckets[i];

	while (l != NULL) {
	    List temp = l;

	    l = l->tail;

/*???	    Utils_FreeMemory((Hash_Bucket) temp->first);*/
	    Utils_FreeMemory(temp);
	}
    }

    Utils_FreeMemory(hash_table);
}

void Hash_Walk(hash_table, walk_proc)
Hash_Table hash_table;
Hash_WalkProc walk_proc;
{
    List *l;

    for (l = hash_table->buckets;
	 l < hash_table->buckets + hash_table->nBuckets;
	 l++) {
	if (*l != NULL) {
	    List b;

	    for (b = *l; b != NULL; b = b->tail)
		(*walk_proc)((Hash_Bucket) b->first);
	}
    }
}

void Hash_DumpTable(stream, hash_table)
FILE *stream;
Hash_Table hash_table;
{
    int i;

    fprintf(stream, "%d buckets\n", hash_table->nBuckets);
    fprintf(stream, "%d entries\n", hash_table->nEntries);

#ifdef HASH_PROFILE
    fprintf(stream, "%d finds\n", hash_table->finds);
    fprintf(stream, "%d inserts\n", hash_table->inserts);
    fprintf(stream, "%d probes\n", hash_table->probes);
    fprintf(stream, "%d rehashes\n", hash_table->rehashes);
#endif

    for (i = 0; i < hash_table->nBuckets; i++)
	if (hash_table->buckets[i] != NULL) {
	    List l;

	    fprintf(stream, "\t[%d]:", i);

	    for (l = hash_table->buckets[i]; l != NULL; l = l->tail) {
		Hash_Bucket b = (Hash_Bucket) l->first;

		fprintf(stream, " \"%s\"", b->key);
	    }

	    fprintf(stream, "\n");
	}
}

void Hash_DumpTableToStderr(hash_table)
Hash_Table hash_table;
{
    Hash_DumpTable(stderr, hash_table);
}

