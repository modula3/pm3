
/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The random module contains random number generators for the various
 tests of the benchmark.  The random number generators are
 deterministic and separate, i.e. the sequence of numbers delivered
 by e.g. GetRandomNoOfChildren will always be the same and is not
 affected by intermediate calls to one of the other functions.  The
 random number generators must first be initialized by a call to
 InitRandom.

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 02.03.93   F.H.         Added GetRandomExtIDLevelN()
 ***************************************************************************/

#ifndef _bm_random_h
#define _bm_random_h

#include "bm_global.h"
#include "bm_system.h"

extern void InitRandom(int levels, long seed);

/*
 * This function initializes all of the random number generators.  It
 * should be called before any of the other functions in this module
 * is called.  The 'levels' argument tells for which database size the
 * random numbers are generated.  The 'seed' argument is used to
 * compute the seed of the random number generators.
 */


extern int GetRandomNoOfChildren(void);

/*
 * RETURN no_of_children
 *
 * This function returns a random even number in the range
 * [MIN_CHILDREN..MAX_CHILDREN].  It is used for determining the
 * number of children for a node when inserting nodes in the database
 * during the build-up phase or the SubTreeInsert operation in phase
 * 1.
 */


extern int GetRandomNoOfRefs(void);

/*
 * RETURN no_of_references
 *
 * This function returns a random number in the range
 * [MIN_REFS..MAX_REFS].  It is used for determining the
 * number of references for a node when inserting nodes in the database
 * during the build-up phase or the SubTreeInsert operation in phase 1.
 */




extern extIDtype GetRandomExtID(void);

/*
 * RETURN extID
 *
 * This function returns a random externalID among those supposedly
 * present in the database (based on the number of levels).  It is used
 * for determining nodes to connect when creating refTo/refFrom
 * relations.
 */


extern extIDtype GetRandomExtIDLevelN(int level);

/*
 * IN  level
 * RETURN extID
 *
 * This function returns the externalID of a random node on level 'level'.
 */


extern int GetRandomMillionValue(void);

/*
 * RETURN million
 *
 * This function returns a random number in the range
 * [MILLION_MIN_VAL..MILLION_MAX_VAL].  This value is used for
 * initializing and updating the million attribute of the two node
 * types.
 */


extern void GetRandomText(char text[TEXT_MAX_LEN + 1], int *length);

/*
 * OUT  text
 *
 * This function returns in 'text' a string of length
 * [TEXT_MIN_LENGTH..TEXT_MAX_LENGTH] consisting of ascii printable
 * characters in the range [CHAR_MIN_VAL..CHAR_MAX_VAL].
 * The actual length of the string is also returned.
 */


extern extIDtype GetRandomAllNode(void);

/*
 * RETURN extID
 *
 * This function returns a random externalID among those supposedly
 * present in the database (based on the number of levels).  It is
 * used for determining which nodes to access for the
 * InternalIDLookup, AttributeRead, and AttributeWrite operations in
 * phase 1 of the benchmark.
 */


extern extIDtype GetRandomClosureLevelNode(void);

/*
 * RETURN extID
 *
 * This function returns a random externalID chosen from the ones supposedly
 * present on level CLOSURE_LEVEL in the database.  It is used for
 * selecting nodes for the Closure operations in both phase 1, phase 2
 * and phase 3.
 */

extern extIDtype GetRandomTextNode(void);

/*
 * RETURN extID
 *
 * This function returns a random externalID chosen from the ones
 * supposed to be nodes of type 'TextNode' in the test database.  It
 * is used for selecting nodes for the TextAttributeRead and
 * TextAttributeWrite operations in phase 1 of the benchmark.
 */


#endif
