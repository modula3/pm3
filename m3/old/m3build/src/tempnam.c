/* Copyright (C) 1994, Digital Equipment Corporation              */
/* All rights reserved.                                           */
/* See the file COPYRIGHT for a full description.                 */
/*                                                                */
/* Last modified on Fri Sep  9 08:24:57 PDT 1994 by kalsow        */
/*                                                                */
/* contributed by Thomas Neumann <tom@smart.ruhr.de>, 07.09.1994  */

#ifdef NeXT

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

char *
tempnam(const char *dir, const char *prefix)
{
  static int count;
  char *path = (char *)malloc(1024);

  if (!dir)
    dir = "/tmp";
  if (!prefix)
    prefix = "t";

  if (path) {
    sprintf(path, "%s/%s%d.%d", dir, prefix, count++, (int)getpid());
  }

  return path;
}

#endif
