/* Copyright (C) 1994, Digital Equipment Corporation              */
/* All rights reserved.                                           */
/* See the file COPYRIGHT for a full description.                 */
/*                                                                */
/* Last modified on Fri Sep  9 08:25:19 PDT 1994 by kalsow        */
/*                                                                */
/* contributed by Thomas Neumann <tom@smart.ruhr.de>, 07.09.1994  */

#ifdef NeXT

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

extern char *getwd(char *);

#ifdef __cplusplus
}
#endif

char *
getcwd(char *buf, size_t size)
{
  if (!buf)
    buf = (char *)malloc(size + 1);
  return getwd(buf);
}

#endif
