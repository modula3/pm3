/***************************************************************************
 SCCS ID: %W%  %G%
 Author:  Frode H\ogberg
 Project: Diploma Thesis

 Description:
 This file is a part of the implementation of the "Software Engineering
 Database Benchmark - SEB".

 The system module contains functions aimed at isolating the
 benchmark from the operation system.

 Creation date: 20.01.93

 Change record:
 ----------------------------------------------------------------------
 Date     | Name       | Description
 ----------------------------------------------------------------------
 00.00.00   XXXXXXXXXX   XXXXXXXXX
 ***************************************************************************/

#ifndef _bm_system_h
#define _bm_system_h

#include "bm_global.h"


typedef struct
  {					       /* The bm_time type is a structure */
    long sec;				       /* used for holding time information; */
    long usec;				       /* either datetime (elapsed time since */
  }
bm_time;				       /* January 1 1970 00:00:00.0 GMT) or */

				  /* interval time */

typedef void *generator;		       /* The generator type is an opaque */

				  /* type storing state information */
				  /* for a random number generator */

extern generator new_generator(long seed);

/*
 * IN  seed
 * RETURN generator
 *
 * This function creates and initializes a structure for storing state
 * information of the random number generation algorithm used by
 * get_random.  A pointer to this structure is returned and is to be
 * used by subsequent calls to get_random.  In this way it is possible
 * to create several separate independent streams of random numbers.
 *
 * The algorithm generates pseudo-random numbers using the linear
 * congruential algorithm and 48-bit integer arithmetic:
 *
 * X(n+1) = (a * X(n) + c) mod m             n >= 0
 *
 * where m=2^48, a=0x5DEECE66D, and c=0xB.
 *
 * The 'seed' parameter determines the high-order 32 bits of X(i)
 * while the low-order 16 bits of X(i) are arbitrarily set to 0x330E.
 *
 * See also get_random.
 */


extern void destroy_generator(generator gen);

/*
 * IN  gen
 *
 * Destroy a random number generator after use.
 */


extern long get_random(generator rang, long low, long high);

/*
 * IN/OUT rang
 * IN  low
 * IN  high
 * RETURN random_number  [low,high]
 *
 * This function returns a random integer in the interval
 * ['low','high'], using the random number algorithm described in
 * new_generator.  'rang' supplies X(n) when called and will contain
 * X(n+1) when the function returns.
 *
 * The algorithm works with 48-bit integer arithmetics, but only the
 * high-order 31 bits are used, thus providing uniformly distributed
 * random numbers in the range [0,2^31) which is then normalized
 * according to 'low' and 'high'.
 */


extern void get_time(bm_time * tp);

/*
 * OUT  tp
 *
 * This function returns the current time expressed in elapsed seconds
 * and microseconds since 00:00:00 GMT, January 1, 1970.
 * The time is filled into the structure pointed to by 'tp'.
 */


extern void diff_time(bm_time time1, bm_time time2, bm_time * timediff);

/*
 * IN  time1
 * IN  time2
 * OUT timediff
 *
 * This function computes the difference between the two times 'time1'
 * and 'time2'.  'time1' must be before (<) 'time2', if not 'timediff'
 * is 0.  The result is returned in 'timediff'.  The parameters may
 * either be datetimes or interval times.  The only combination not
 * meaningful is time1 containing an interval time and 'time2'
 * containing a date time.
 */


extern void add_time(bm_time time1, bm_time bm_time2, bm_time * timesum);

/*
 * IN  time1
 * IN  time2
 * OUT timesum
 *
 * This function adds the two times 'time1' and 'time2' together,
 * returning the result in 'timesum'.  The parameters may either be
 * datetimes or intervaltimes.
 */


extern void div_time(bm_time time, int dividend, bm_time * avg_time);

/*
 * IN  time
 * IN  dividend
 * OUT avg_time
 *
 * This function divides the time by a number 'dividend', returning
 * the average time 'avg_time'.
 */


extern void bm_time2hms(bm_time time, int *h, int *min, float *s);

/*
 * IN  time
 * OUT h
 * OUT min
 * OUT s
 *
 * Convert a bm_time structure to hours, minutes, seconds (which can
 * be a rational number).
 */


extern long power(long x, long y);

/*
 * IN x
 * IN y
 * RETURN power
 *
 * Recursively computes x^y
 */


extern long geometric(long k, long n);

/*
 * IN  k
 * IN  n
 * RETURN geometric_sum
 *
 * Computes 1+k+k^2+ ... + k^n
 */


extern int level_of(extIDtype extID);

/*
 * IN extID
 * RETURN level
 *
 * Computes which level a node with externalID 'extID' is on.
 */


extern int even(long k);

/*
 * IN  k
 * RETURN boolean
 *
 * Returns 1 (TRUE) if k is an even number, 0 (FALSE) if it is odd.
 */


extern void system_empty_cache(void);

/*
 * NO ARGUMENTS OR RETURN VALUE
 *
 * Runs the program chill or similar to empty virtual memory of any
 * useful pages, so that new reads are forced to access disk.
 */


#define FILE_OK     0
#define FILE_ERROR  1

extern int get_file_size(char *path, long *size);

/*
 * IN  path
 * OUT size
 * RETURN status
 *
 * This function returns the size of the file with path 'path' in
 * 'size'.  If no error occurs, FILE_OK is returned, otherwise
 * FILE_ERROR is returned.  For now the path has to be the name of a
 * regular file.  It may later be extended also to be able to retrieve
 * the size of directories.
 */


extern int file_copy(char *frompath, char *topath);

/*
 * IN  frompath
 * IN  topath
 * RETURN status
 *
 * This function copies the file 'frompath' to 'topath'.  If no error
 * occurs, FILE_OK is returned, otherwise FILE_ERROR is returned.
 * The path may be absolute or relative.
 */



extern char *get_env_var(const char *name);

/*
 * RETURN envval_string
 *
 * The function reads the environment variable with name 'name',
 * returning a pointer to a string containing the current value.  If
 * the variable is not found, NULL is returned.
 */



extern char *get_system_info(void);

/*
 * RETURN systeminfo_string
 *
 * Get information about the system on which the benchmark is run (for
 * logging).  This should at least include the host name.  The
 * information is returned in a string.
 */


extern char *get_local_timedate(void);

/*
 * RETURN  timedate_string
 *
 * This functions returns a string with information about the current
 * local time and date (for logging).
 */


#endif
