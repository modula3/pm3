#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

ssize_t
m3_recv(int s, void *buf, size_t len, int flags)
{
  return recv(s, buf, len, flags);
}
