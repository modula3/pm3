#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_getsockname(int s, struct sockaddr *name, int *namelen)
{
  return getsockname(s, name, namelen);
}
