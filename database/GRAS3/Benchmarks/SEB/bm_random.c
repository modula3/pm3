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

 Creation date: 25.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 02.03.93   F.H.         Added GetRandomExtIDLevelN()
 ***************************************************************************/

#include "bm_random.h"
#include "bm_system.h"
#include "bm_global.h"


#define NO_OF_CHILDREN_SEED  ((MAX_CHILDREN-MIN_CHILDREN)/2)
#define NO_OF_REFS_SEED      AVG_REFS
#define EXT_ID_SEED          1
#define EXT_ID_LEVELN_SEED   1
#define MILLION_SEED         MILLION_MIN_VAL
#define TEXT_LEN_SEED        TEXT_MIN_LEN
#define CHAR_SEED            CHAR_MIN_VAL
#define ALLNODE_SEL_SEED     1
#define CLOSURENODE_SEL_SEED 1
#define TEXTNODE_SEL_SEED    1


generator no_of_children;
generator no_of_refs;
generator ext_id;
generator ext_id_levelN;
generator million;
generator text_len;
generator text_char;
generator allnode_sel;
generator closurenode_sel;
generator textnode_sel;

int DBlevels = DEFAULT_LEVELS;		       /* number of levels in benchmark test database */
long evenMIN_CHILDREN;			       /* lowest even number in [MIN_CHILDREN..MAX_CHILDREN] */
long children_values;			       /* number of different values for no_of_children */

		      /* (number of even numbers in interval */
		      /* [MIN_CHILDREN..MAX_CHILDREN]) */
long maxDB;				       /* max value of externalID in DB */
long minLevelC;				       /* min value of externalID of node on CLOSURE_LEVEL */
long maxLevelC;				       /* max value of externalID of node on CLOSURE_LEVEL */
long minLevelT;				       /* min value of externalID of node on last level */
long maxLevelT;				       /* max value of externalID of node on last level */



void
InitRandom(int levels, long seed)
{
  if (!(MIN_LEVELS <= levels && MAX_LEVELS))
    bm_error("InitRandom",
     "Illegal parameter:levels not in [MIN_LEVEL..MAX_LEVEL] range", FALSE);

  DBlevels = levels;

  if (even(MIN_CHILDREN))
    evenMIN_CHILDREN = MIN_CHILDREN;
  else
    evenMIN_CHILDREN = MIN_CHILDREN + 1;
  children_values = (MAX_CHILDREN - MIN_CHILDREN) / 2;
  if (even(MIN_CHILDREN) || even(MAX_CHILDREN))
    children_values++;

  maxDB = geometric(AVG_CHILDREN, DBlevels);
  minLevelC = geometric(AVG_CHILDREN, CLOSURE_LEVEL - 1) + 1;
  maxLevelC = geometric(AVG_CHILDREN, CLOSURE_LEVEL);
  minLevelT = geometric(AVG_CHILDREN, DBlevels - 1) + 1;
  maxLevelT = geometric(AVG_CHILDREN, DBlevels);

  no_of_children = new_generator(seed + NO_OF_CHILDREN_SEED);
  no_of_refs = new_generator(seed + NO_OF_REFS_SEED);
  ext_id = new_generator(seed + EXT_ID_SEED);
  ext_id_levelN = new_generator(seed + EXT_ID_LEVELN_SEED);
  million = new_generator(seed + MILLION_SEED);
  text_len = new_generator(seed + TEXT_LEN_SEED);
  text_char = new_generator(seed + CHAR_SEED);
  allnode_sel = new_generator(seed + ALLNODE_SEL_SEED);
  closurenode_sel = new_generator(seed + CLOSURENODE_SEL_SEED);
  textnode_sel = new_generator(seed + TEXTNODE_SEL_SEED);
}


int
GetRandomNoOfChildren(void)
{


  return (evenMIN_CHILDREN
	  + (2 * (int) get_random(no_of_children, 0, children_values - 1)));
}


int
GetRandomNoOfRefs(void)
{
  return ((int) get_random(no_of_refs, MIN_REFS, MAX_REFS));
}


extIDtype
GetRandomExtID(void)
{
  return ((extIDtype) get_random(ext_id, 1, maxDB));
}


extIDtype
GetRandomExtIDLevelN(int level)
{
  if (level > DBlevels)
    bm_error("GetRandomExtIDLevelN",
	     "level argument > number of levels in database", FALSE);
  if (level == 0)
    return 1;
  else
    return ((extIDtype) get_random(ext_id_levelN,
				   geometric(AVG_CHILDREN, level - 1) + 1,
				   geometric(AVG_CHILDREN, level)));
}


int
GetRandomMillionValue(void)
{
  return ((int) get_random(million, MILLION_MIN_VAL, MILLION_MAX_VAL));
}


void
GetRandomText(char text[TEXT_MAX_LEN + 1], int *length)
{
  int pos;

  *length = (int) get_random(text_len, TEXT_MIN_LEN, TEXT_MAX_LEN);
  for (pos = 0; pos < *length; pos++)
    text[pos] = (char) get_random(text_char, CHAR_MIN_VAL, CHAR_MAX_VAL);
  text[*length] = '\0';
}


extIDtype
GetRandomAllNode(void)
{
  return ((extIDtype) get_random(allnode_sel, 1, maxDB));
}


extIDtype
GetRandomClosureLevelNode(void)
{
  return ((extIDtype) get_random(closurenode_sel, minLevelC, maxLevelC));
}


extIDtype
GetRandomTextNode(void)
{
  return ((extIDtype) get_random(textnode_sel, minLevelT, maxLevelT));
}
