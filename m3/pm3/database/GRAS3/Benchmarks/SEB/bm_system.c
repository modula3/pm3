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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "bm_system.h"
#include "bm_global.h"
#include "ansi.h"

/* prototypes of some system/library calls used */
extern long nrand48(unsigned short xsubi[3]);
extern time_t time(time_t * tloc);
/*
#ifndef SOL2
extern int gettimeofday(struct timeval *tp, struct timezone *tzp);
#endif
*/


typedef unsigned short xsubi[3];
typedef xsubi *xp;

generator
new_generator(long seed)
{
  xp new_gen;

  if (!(new_gen = (xp) malloc(sizeof(xsubi))))
    bm_error("new_generator", "Could not allocate memory (malloc)!", TRUE);

  (*new_gen)[2] = seed >> 16;		       /* 16 high-order bits */
  (*new_gen)[1] = seed & 0xFFFF;	       /* 16 low-order bits */
  (*new_gen)[0] = 0x330E;

  return (new_gen);
}


void
destroy_generator(generator gen)
{
  free(gen);
}


long
get_random(generator ran, long low, long high)
{
  return ((nrand48(*(xp) ran) % (high - low + 1)) + low);
}



void
get_time(bm_time * tp)
{
  struct timeval tv;

  gettimeofday(&tv, NULL);		       /* system call */
  tp->sec = tv.tv_sec;
  tp->usec = tv.tv_usec;
}


static double
frac(double x)
{
  return (x - (long) x);
}


void
diff_time(bm_time time1, bm_time time2, bm_time * timediff)
{
  double t1, t2;

  t1 = (double) time1.sec + (double) time1.usec * 0.000001;
  t2 = (double) time2.sec + (double) time2.usec * 0.000001;

  if (t1 < t2)
    {
      t1 = t2 - t1;
      timediff->sec = (long) t1;
      timediff->usec = (long) (frac(t1) * 1000000);
    }
  else
    {
      timediff->sec = 0;
      timediff->usec = 0;
    }
}


void
add_time(bm_time time1, bm_time time2, bm_time * timesum)
{
  double t1, t2;

  t1 = (double) time1.sec + (double) time1.usec * 0.000001;
  t2 = (double) time2.sec + (double) time2.usec * 0.000001;
  t1 += t2;

  timesum->sec = (long) t1;
  timesum->usec = (long) (frac(t1) * 1000000);
}


void
div_time(bm_time bmtime, int dividend, bm_time * avg_time)
{
  double avg_sec;

  avg_sec = ((double) bmtime.sec + (double) bmtime.usec * 0.000001) / (double) dividend;
  avg_time->sec = (long) avg_sec;
  avg_time->usec = (long) (frac(avg_sec) * 1000000);
}


void
bm_time2hms(bm_time bmtime, int *h, int *min, float *s)
{
  *h = bmtime.sec / 3600;
  *min = (bmtime.sec % 3600) / 60;
  *s = (bmtime.sec % 3600) % 60;
  *s += 0.000001 * bmtime.usec;
}


long
power(long x, long y)
{
  return ((long) pow((double) x, (double) y));
}



long
geometric(long k, long n)
/*
 * Computes 1+k+k^2+ ... + k^n
 */
{
  return ((1 - power(k, n + 1)) / (1 - k));
}


int
level_of(extIDtype extID)
/*
 * IN extID
 * RETURN level
 *
 * Computes which level a node with externalID 'extID' is on.
 */
{
  static long level_max[MAX_LEVELS + 1];
  static init = FALSE;
  int i;

  if (!init)
    {
      init = TRUE;
      for (i = 0; i < MAX_LEVELS + 1; i++)
	level_max[i] = geometric(AVG_CHILDREN, i);
    }
  for (i = 0; i < MAX_LEVELS + 1; i++)
    if (extID <= level_max[i])
      return i;


  /* should't reach this far */
  bm_error("level_of", "invalid value of extID", FALSE);
}


int
even(long k)
/*
 * Returns 1 (TRUE) if k is an even number, 0 (FALSE) if it is odd.
 */
{
  return (k % 2 == 0);
}


void
system_empty_cache(void)
{
  system("./coolscript");
}



int
get_file_size(char *path, long *size)
{
  struct stat buf;

  if (stat(path, &buf) == -1)
    return (FILE_ERROR);
  if (S_ISREG(buf.st_mode))
    {
      *size = buf.st_size;
      return (FILE_OK);
    }
  else
    return (FILE_ERROR);
}



int
file_copy(char *frompath, char *topath)
{
  char buffer[MAX_PATH + MAX_PATH + 10];

  sprintf(buffer, "cp %s %s", frompath, topath);
  if (system(buffer) >> 8 != 0)
    return (FILE_ERROR);
  else
    return (FILE_OK);
}


char *
get_env_var(const char *name)
{
  return (getenv(name));
}


char *
get_system_info(void)
{
  char *info;
  char *host;
  char *user;
  char *hosttype;

  if ((host = get_env_var("HOST")) == NULL
      || (user = get_env_var("USER")) == NULL
      || (hosttype = get_env_var("HOSTTYPE")) == NULL)
    return NULL;
  info = (char *) malloc(strlen(host) + strlen(user) + strlen(hosttype) + 50);
  if (!info)
    bm_error("get_system_info", "Couldn't allocate memory", TRUE);
  sprintf(info, "\tHostname: %s\n\tHosttype: %s\n\tUser: %s\n",
	  host, hosttype, user);
  return (info);
}


char *
get_local_timedate(void)
{
  time_t time_now;
  char *timedate;

  time_now = time(NULL);
  timedate = ctime(&time_now);
  return (timedate);
}
