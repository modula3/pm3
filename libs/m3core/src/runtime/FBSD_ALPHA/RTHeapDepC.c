/* Copyright (C) 1992, Digital Equipment Corporation */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

/*      modified on Tue Feb  2 11:15:57 PST 1993 by jdd */

/* This is RTHeapDepC.c for FreeBSD running on 386/486 processors. */
/*
 * ow Sat Oct 29 14:10:19 MET 1994
 * ow Sun Nov  6 16:39:26 MET 1994
 * ow Sun Dec  4 17:58:49     1994 changes for FreeBSD 2.0
 *
 * I just tried to check the calls implemented for Ultrix,
 * (almost) nothing else has been changed or added.
 * So be careful to use any other system calls, some are
 * probably missing.
 */
/* This file implements wrappers for almost all Ultrix system calls
   that take pointers as arguments.  These wrappers allow the system
   calls to take arguments that might point to the traced heap, which
   may be VM-protected in the Ultrix implementation of the collector.
   The wrappers read and write the referents of all pointers about to
   passed to the system call, which ensures that the pages are not
   protected when the call is made.

   Each wrapper is a critical section, with ThreadF__inCritical non-zero,
   so that another thread cannot cause the pages to become reprotected
   before the system call is performed.

   A few system calls are not handled here, or are handled only
   partially.  This restricts the system calls that can be made from
   Modula-3, or from libraries that are passed pointers into the
   Modula-3 traced heap.  These system calls are:

   1) sigvec.  Sigvec takes 3 arguments, but passes 4 to the kernel.
      The extra argument is the address of the sigtramp code, which is
      copyrighted.  The sigtramp code appears in the the standard .o
      file that also defines sigvec, so that sigvec cannot be
      redefined here without including sigtramp.  Rewriting sigtramp
      seemed too error-prone, so sigvec is not supported here, meaning
      that sigvec cannot be called with arguments on the heap.

   2) syscall.  Implementing syscall would require a huge case
      statement, with one case per system call.  This seemed too
      error-prone, so syscall cannot take arguments that point into
      the traced heap.

   3) ioctl.  Ioctl's third argument may be a pointer, and some device
      drivers might interpret the referent as containing more
      pointers.  These second-level pointers are not handled here, so
      they must not point into the traced heap if they exist.
      Handling this problem in general is impossible, since the set of
      device drivers is open-ended.

   4) profil.  The memory referenced by the "buff" argument is updated
      after the call returns, and there is no mechanism to permanently
      unprotect it.

   5) Undocumented system calls.  There are private system calls with
      no manual pages, so it was impossible to write wrappers.

   6) audgen, whose manpage is incomprehensible.

   (Some calls in Section 2 are already wrappers for other system
   calls; it is not necessary to reimplement them here.)

   Also, longjmp must not be used from a signal handler to abnormally
   exit from a system call.

   Finally, if a system call references an object on the heap, each
   pointer must reference only one object.  Therefore, it is not
   possible to write the heap contents with a single write. */

#define MFS
#define NFS
#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#include <sys/syscall.h>
#include <sys/file.h>
#include <sys/param.h>
#if __FreeBSD__ >= 2
#include <sys/sysctl.h>
#include <osreldate.h>
#endif
#include <sys/mount.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <ufs/ufs/quota.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/wait.h>

#if __FreeBSD__ >= 3
#include <sys/time.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsproto.h>
#include <nfs/nfs.h>
#include <ufs/ufs/ufsmount.h>
#endif

#include <string.h>
#include <unistd.h>

#if __FreeBSD_version >= 400013 
#define SOCKLEN_T       socklen_t
#else 
#define SOCKLEN_T       int 
#endif

#ifdef   NULL
#undef   NULL
#endif
#define  NULL (void *)(0)
extern long ThreadF__inCritical;
#define ENTER_CRITICAL ThreadF__inCritical++
#define EXIT_CRITICAL  ThreadF__inCritical--

void (*RTHeapRep_Fault)(char*);
void (*RTCSRC_FinishVM)();

static char RTHeapDepC__c;
#define MAKE_READABLE(x) if (x != 0) { RTHeapDepC__c = *(char*)(x); }
#define MAKE_WRITABLE(x) if (x != 0) { *(char*)(x) = RTHeapDepC__c = *(char*)(x); }

/*
 * Modula-3 compilation units are always compiled PIC.  This causes
 * the a.out linker to complain that there is no reference to
 * _DYNAMIC if the program is linked with "-static".  Provide a
 * weak reference to it here so that static linking will work.
 *
 * This has nothing to do with the heap, so it doesn't really belong
 * here.  But it needs to be someplace that is guaranteed to be pulled
 * into every executable.
 */
#pragma weak _DYNAMIC
extern int _DYNAMIC;
static int *i __unused = &_DYNAMIC;

/* Unless otherwise noted, all the following wrappers have the same
   structure. */

int access(path, mode)   /* ok */
const char *path;
int mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_access, path, mode);
  EXIT_CRITICAL;
  return result;
}

int acct(file)   /* ok */
const char *file;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  result = syscall(SYS_acct, file);
  EXIT_CRITICAL;
  return result;
}

int adjtime(delta, olddelta)   /* ok */
#if __FreeBSD__ >= 2
const struct timeval *delta;
#else
struct timeval *delta;
#endif
struct timeval *olddelta;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(delta);
  MAKE_WRITABLE(olddelta);
  result = syscall(SYS_adjtime, delta, olddelta);
  EXIT_CRITICAL;
  return result;
}

/* not implemented
int atomic_op(op, addr)
int op;
int *addr;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(addr);
  result = syscall(SYS_atomic_op, op, addr);
  EXIT_CRITICAL;
  return result;
}
*/

/* not implemented
int audcntl(request, argp, len, flag, audit_id)
int request;
char *argp;
int len;
int flag;
audit_ID_t audit_id;
{ int result;

  ENTER_CRITICAL;
  switch (request) {
  case GET_SYS_AMASK:
  case GET_TRUSTED_AMASK:
  case GET_PROC_AMASK:
    MAKE_WRITABLE(argp);
    break;
  case SET_SYS_AMASK:
  case SET_TRUSTED_AMASK:
  case SET_PROC_AMASK:
    MAKE_READABLE(argp);
    break;
  default:
    break;
  }
  result = syscall(SYS_audcntl, request, argp, len, flag, audit_id);
  EXIT_CRITICAL;
  return result;
}

int audgen(event, tokenp, argv)
int event;
char *tokenp, *argv[];
{ int result;

  ENTER_CRITICAL;
  
  { char *t, **a;

    for (t = tokenp, a = argv; t; t++, a++) {
      if (A_TOKEN_PTR(*t)) {
        MAKE_READABLE(*a);
      }
    }
  }
  result = syscall(SYS_audgen, tokenp, argv);
  EXIT_CRITICAL;
  return result;
}
*/

/* not implemented
int cachectl(addr, nbytes, op)
char *addr;
int nbytes, op;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(addr);
  result = syscall(SYS_cachectl, addr, nbytes, op);
  EXIT_CRITICAL;
  return result;
}

int cacheflush(addr, nbytes, cache)
char *addr;
int nbytes, cache;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(addr);
  result = syscall(SYS_cacheflush, addr, nbytes, cache);
  EXIT_CRITICAL;
  return result;
}
*/

int chdir(path)   /* ok */
const char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chdir, path);
  EXIT_CRITICAL;
  return result;
}

int chflags(path, flags)
const char *path;
u_long flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chflags, path, flags);
  EXIT_CRITICAL;
  return result;
}

int chmod(path, mode)   /* ok */
const char *path;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chmod, path, mode);
  EXIT_CRITICAL;
  return result;
}

int chown(path, owner, group)   /* ok */
const char *path;
uid_t owner;
gid_t group;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chown, path, owner, group);
  EXIT_CRITICAL;
  return result;
}

int chroot(dirname)   /* ok */
const char *dirname;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(dirname);
  result = syscall(SYS_chroot, dirname);
  EXIT_CRITICAL;
  return result;
}

/* not implemented (obsolete)
int creat(name, mode)   
const char *name;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_creat, name, mode);
  EXIT_CRITICAL;
  return result;
}
*/

/* execve is implemented differently since it does not return, which
   would leave ThreadF__inCritical set in the parent if called in the child
   of a vfork. Many calls leave the process in an undefined state in the
   case of EFAULT, but we assume that execve is not one of these. */

int execve(name, argv, envp)   /* ok */
const char *name;
char * const argv[];
char * const envp[];
{ int result;

  for (;;) {
    result = syscall(SYS_execve, name, argv, envp);
    if (result == -1 && errno == EFAULT) {
      MAKE_READABLE(name);
      { char * const *a; for (a = argv; *a; a++) MAKE_READABLE(*a); }
      { char * const *e; for (e = envp; *e; e++) MAKE_READABLE(*e); }
    } else {
      return result;
    }
  }
}

/* not implemented
int exportfs(name, rootuid, exflags)
char *name;
int rootuid, exflags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_exportfs, name, rootuid, exflags);
  EXIT_CRITICAL;
  return result;
}
*/

int ufcntl(int fd, int request, int arg)   /* ok */
{ int result;
/*  int arg;
  va_list ap;

  va_start(ap, request);
  arg = va_arg(ap, int);
  va_end(ap);
*/

  ENTER_CRITICAL;
  switch (request) {
  case F_GETLK:
    MAKE_WRITABLE(arg);
    break;
  case F_SETLK:
  case F_SETLKW:
    MAKE_READABLE(arg);
    break;
  default:
    break;
  }
  result = syscall(SYS_fcntl, fd, request, arg);
  EXIT_CRITICAL;
  return result;
}

int fstat(fd, buf)   /* ok */
int fd;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_fstat, fd, buf);
  EXIT_CRITICAL;
  return result;
}

int getdirentries(fd, buf, nbytes, basep)   /* ok */
int fd;
char *buf;
int nbytes;
long *basep;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  MAKE_WRITABLE(basep);
  result = syscall(SYS_getdirentries, fd, buf, nbytes, basep);
  EXIT_CRITICAL;
  return result;
}

int getdomainname(name, namelen)   /* ok */
char *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = syscall(SYS_getdomainname, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int gethostname(name, namelen)   /* ok */
char *name;
int namelen;
{ int result;
#if __FreeBSD__ >= 2
  int mib[2];
  size_t size;
#endif

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
#if __FreeBSD__ >= 2
  mib[0] = CTL_KERN;
  mib[1] = KERN_HOSTNAME; 
  size = namelen;
  if (sysctl(mib, 2, name, &size, NULL, 0) == -1){
    result = -1; 
  }else{
    result = 0;
  }
#else
  result = syscall(SYS_gethostname, name, namelen);
#endif
  EXIT_CRITICAL;
  return result;
}

int getgroups(gidsetsize, grouplist)   /* ok */
int gidsetsize;
gid_t grouplist[];
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(grouplist);
  result = syscall(SYS_getgroups, gidsetsize, grouplist);
  EXIT_CRITICAL;
  return result;
}

int getitimer(which, value)   /* ok */
int which;
struct itimerval *value;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(value);
  result = syscall(SYS_getitimer, which, value);
  EXIT_CRITICAL;
  return result;
}

/* not implemented
int getmnt(start, buffer, nbytes, mode, path)
int *start;
struct fs_data *buffer;
int nbytes, mode;
char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(start);
  MAKE_WRITABLE(buffer);
  MAKE_READABLE(path);
  result = syscall(SYS_getmnt, start, buffer, nbytes, mode, path);
  EXIT_CRITICAL;
  return result;
}
*/

int getrlimit(resource, rlp)   /* ok */
int resource;
struct rlimit *rlp;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rlp);
  result = syscall(SYS_getrlimit, resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int getrusage(who, rusage)   /* ok */
int who;
struct rusage *rusage;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_getrusage, who, rusage);
  EXIT_CRITICAL;
  return result;
}

int getsockopt(s, level, optname, optval, optlen)   /* ok */
int s, level, optname;
void *optval;
SOCKLEN_T *optlen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(optval);
  MAKE_WRITABLE(optlen);
  result = syscall(SYS_getsockopt, s, level, optname, optval, optlen);
  EXIT_CRITICAL;
  return result;
}
/* not implemented
int getsysinfo(op, buffer, nbytes, start, arg)
unsigned op;
char *buffer;
unsigned nbytes;
int *start;
char *arg;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buffer);
  MAKE_WRITABLE(start);
  MAKE_WRITABLE(arg);
  result = syscall(SYS_getsysinfo, op, buffer, nbytes, start, arg);
  EXIT_CRITICAL;
  return result;
}
*/

int gettimeofday(tp, tzp)   /* ok */
struct timeval *tp;
struct timezone *tzp;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(tp);
  MAKE_WRITABLE(tzp);
  result = syscall(SYS_gettimeofday, tp, tzp);
  EXIT_CRITICAL;
  return result;
}

/* ioctl must test the argp argument carefully.  It may be a pointer,
   or maybe not.  At a slight expense, we call RTHeapRep.Fault to
   unprotect the page if it's in the traced heap, but do nothing
   otherwise. */

int ioctl(d, request, argp)   /* ok */
int d;
unsigned long request;
char *argp;
{ int result;

  ENTER_CRITICAL;
  if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it readable */
  if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it writable */
  result = syscall(SYS_ioctl, d, request, argp);
  EXIT_CRITICAL;
  return result;
}

#ifdef SYS_lchown
int lchown(path, owner, group)   /* ok */
const char *path;
uid_t owner;
gid_t group;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_lchown, path, owner, group);
  EXIT_CRITICAL;
  return result;
}
#endif /* SYS_lchown */

int link(name1, name2)   /* ok */
const char *name1;
const char *name2;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = syscall(SYS_link, name1, name2);
  EXIT_CRITICAL;
  return result;
}

int lstat(path, buf)   /* ok */
const char *path;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_lstat, path, buf);
  EXIT_CRITICAL;
  return result;
}

int mkdir(path, mode)   /* ok */
const char *path;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mkdir, path, mode);
  EXIT_CRITICAL;
  return result;
}

int mkfifo(path, mode)   /* ok */
const char *path;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mkfifo, path, mode);
  EXIT_CRITICAL;
  return result;
}

int mknod(path, mode, dev)   /* ok */
const char *path;
mode_t mode;
dev_t dev;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mknod, path, mode, dev);
  EXIT_CRITICAL;
  return result;
}

#if __FreeBSD_version >= 300001		/* New form of mount(2) */
int
mount(type, dir, flags, data)
  const char *type;
  const char *dir;
  int flags;
  void *data;
{ int result;
  struct ufs_args *u_data;
  struct mfs_args *m_data;
  struct nfs_args *n_data;

  ENTER_CRITICAL;
  MAKE_READABLE(type);
  MAKE_READABLE(dir);
  if (strcmp(type, "ufs") == 0) {
    u_data = (struct ufs_args*) data;
    MAKE_READABLE(u_data);
    MAKE_READABLE(u_data->fspec);
    result = syscall(SYS_mount, type, dir, flags, data);
  } else if (strcmp(type, "mfs") == 0) {
    m_data = (struct mfs_args*) data;
    MAKE_READABLE(m_data);
    MAKE_READABLE(m_data->fspec);
    result = syscall(SYS_mount, type, dir, flags, data);
  } else if (strcmp(type, "nfs") == 0) {
    n_data = (struct nfs_args*) data;
    MAKE_READABLE(n_data);
    MAKE_READABLE(n_data->addr); 
    MAKE_READABLE(n_data->fh);
    MAKE_READABLE(n_data->hostname);
    result = syscall(SYS_mount, type, dir, flags, data);
  } else {	/* Not anything we recognize. */
    MAKE_READABLE(data);
    result = syscall(SYS_mount, type, dir, flags, data);
  }
  EXIT_CRITICAL;
  if (result != -1) {
    result = 0;
  }
  return result;
}

#else /* __FreeBSD_version >= 300001 */

int mount(type, dir, flags, data)
int type;
const char *dir;
int flags;
void *data;
{ int result;
  struct ufs_args *u_data;
  struct mfs_args *m_data;
  struct nfs_args *n_data;

  ENTER_CRITICAL;
  MAKE_READABLE(dir);
  switch(type) {
     case MOUNT_UFS:  u_data = (struct ufs_args*) data;
                      MAKE_READABLE(u_data);
                      MAKE_READABLE(u_data->fspec); break;
     case MOUNT_MFS:  m_data = (struct mfs_args*) data;
                      MAKE_READABLE(m_data);
#if __FreeBSD__ >= 2
                      MAKE_READABLE(m_data->fspec); break;
#else
                      MAKE_READABLE(m_data->name); break;
#endif
     case MOUNT_NFS:  n_data = (struct nfs_args*) data;
                      MAKE_READABLE(n_data);
                      MAKE_READABLE(n_data->addr); 
                      MAKE_READABLE(n_data->fh);
                      MAKE_READABLE(n_data->hostname); break;
  }
  result = syscall(SYS_mount, type, dir, flags, data);
  EXIT_CRITICAL;
  if (result != -1) {
    result = 0;
  }
  return result;
}
#endif /* __FreeBSD_version >= 300001 */

int msgctl(msqid, cmd, buf)   /* ok */
int msqid, cmd;
struct msqid_ds *buf;
{ int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case IPC_SET:
    MAKE_READABLE(buf);
    break;
  case IPC_STAT:
    MAKE_WRITABLE(buf);
    break;
  default:
    break;
  }
  result = syscall(SYS_msgsys, 0, msqid, cmd, buf);
  EXIT_CRITICAL;
  return result;
}

int msgrcv(msqid, msgp, msgsz, msgtyp, msgflg)   /* ok */
int msqid;
void *msgp;
size_t msgsz;
long msgtyp;
int msgflg;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msgp);
  result = syscall(SYS_msgsys, 3, msqid, msgp, msgsz, msgtyp, msgflg);
  EXIT_CRITICAL;
  return result;
}

int msgsnd(msqid, msgp, msgsz, msgflg)   /* ok */
int msqid;
void *msgp;
size_t msgsz;
int msgflg;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msgp);
  result = syscall(SYS_msgsys, 2, msqid, msgp, msgsz, msgflg);
  EXIT_CRITICAL;
  return result;
}

int uopen(const char* path, int flags, mode_t mode)   /* ok */
{ int result;
/*  mode_t mode;
  va_list ap;

  va_start(ap, flags);
  mode = va_arg(ap, mode_t);
  va_end(ap);
  this does not work. Why?
*/          

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_open, path, flags, mode);
  EXIT_CRITICAL;
  return result;
}

int quotactl(path, cmd, uid, addr)   /* ok */
  const char *path;
  int cmd, uid;
#if __FreeBSD_version >= 400002
  void *addr;
#else
  char *addr;
#endif
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  switch (cmd) {
  case Q_QUOTAON:
  case Q_QUOTAOFF:
  case Q_SETUSE:
  case Q_SETQUOTA:
  case Q_GETQUOTA:
    MAKE_READABLE(addr);
    break;
  /*
  case Q_GETDLIM:
    MAKE_WRITABLE(addr);
    break;
   */
  default:
    break;
  }
  result = syscall(SYS_quotactl, path, cmd, uid,  addr);
  EXIT_CRITICAL;
  return result;
}

int readlink(path, buf, bufsiz)   /* ok */
const char *path;
char *buf;
int bufsiz;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_readlink, path, buf, bufsiz);
  EXIT_CRITICAL;
  return result;
}

ssize_t readv(d, iov, iovcnt)   /* ok */
int d;
const struct iovec *iov;
int iovcnt;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < iovcnt; i++) {
      MAKE_WRITABLE(iov[i].iov_base);
    }
  }
  result = syscall(SYS_readv, d, iov, iovcnt);
  EXIT_CRITICAL;
  return result;
}

ssize_t recvmsg(s, msg, flags)   /* ok */
int s;
struct msghdr msg[];
int flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msg->msg_name);
  { int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_WRITABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_control);
  result = syscall(SYS_recvmsg, s, msg, flags);
  EXIT_CRITICAL;
  return result;
}

int rename(from, to)   /* ok */
char *from;
char *to;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(from);
  MAKE_READABLE(to);
  result = syscall(SYS_rename, from, to);
  EXIT_CRITICAL;
  return result;
}

int rmdir(path)   /* ok */
const char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_rmdir, path);
  EXIT_CRITICAL;
  return result;
}

int
#if (227002 <= __FreeBSD_version && __FreeBSD_version < 300000) || \
  __FreeBSD_version >= 300002
semctl(int semid, int semnum, int cmd, ...)
#else
semctl(int semid, int semnum, int cmd, union semun arg)
#endif
{
  int result;
#if (227002 <= __FreeBSD_version && __FreeBSD_version < 300000) || \
  __FreeBSD_version >= 300002
  union semun arg;
  va_list ap;

  va_start(ap, cmd);
  arg = va_arg(ap, union semun);
  va_end(ap);
#endif

  ENTER_CRITICAL;
  switch (cmd) {

  case IPC_SET:
    MAKE_READABLE(arg.buf);
    break;

  case SETALL:
    MAKE_READABLE(arg.array);
    break;

  case IPC_STAT:
    MAKE_WRITABLE(arg.buf);
    break;

  case GETALL:
    MAKE_WRITABLE(arg.array);
    break;
  }
  result = syscall(SYS_semsys, 0, semid, semnum, cmd, arg);
  EXIT_CRITICAL;
  return result;
}

int semop(semid, sops, nsops)   /* ok ? */
int semid;
struct sembuf *sops;
unsigned int nsops;
{ int result;

  ENTER_CRITICAL;
  { unsigned int i;
    for (i = 0; i < nsops; i++) {
      MAKE_READABLE(sops);
    }
  }
  result = syscall(SYS_semsys, 2, semid, sops, nsops);
  EXIT_CRITICAL;
  return result;
}

ssize_t sendmsg(s, msg, flags)   /* ok */
int s;
const struct msghdr msg[];
int flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msg->msg_name);
  { int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_READABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_control);
  result = syscall(SYS_sendmsg, s, msg, flags);
  EXIT_CRITICAL;
  return result;
}

int setdomainname(name, namelen)   /* ok */
const char *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_setdomainname, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int setgroups(ngroups, gidset)   /* ok */
int ngroups;
const gid_t *gidset;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(gidset);
  result = syscall(SYS_setgroups, ngroups, gidset);
  EXIT_CRITICAL;
  return result;
}

int sethostname(name, namelen)   /* ok */
const char *name;
int namelen;
{ int result;
#if __FreeBSD__ >= 2
  int mib[2];
#endif

  ENTER_CRITICAL;
  MAKE_READABLE(name);
#if __FreeBSD__ >= 2
  mib[0] = CTL_KERN; 
  mib[1] = KERN_HOSTNAME;
  if (sysctl(mib, 2, NULL, NULL, (void *)name, namelen) == -1){
    result = -1;
  }else{
    result = 0;
  }
#else
  result = syscall(SYS_sethostname, name, namelen);
#endif
  EXIT_CRITICAL;
  return result;
}

int setitimer(which, value, ovalue)   /* ok */
int which;
#if __FreeBSD__ >= 2
const struct itimerval *value;
#else
struct itimerval *value;
#endif
struct itimerval *ovalue;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(value);
  MAKE_WRITABLE(ovalue);
  result = syscall(SYS_setitimer, which, value, ovalue);
  EXIT_CRITICAL;
  return result;
}
/* not implemented
int setquota(special, file)
char *special;
char *file;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  MAKE_READABLE(file);
  result = syscall(SYS_setquota, special, file);
  EXIT_CRITICAL;
  return result;
}
*/

int setrlimit(resource, rlp)   /* ok */
int resource;
struct rlimit *rlp;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(rlp);
  result = syscall(SYS_setrlimit, resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int setsockopt(s, level, optname, optval, optlen)   /* ok */
int s, level, optname;
const void *optval;
SOCKLEN_T optlen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(optval);
  result = syscall(SYS_setsockopt, s, level, optname, optval, optlen);
  EXIT_CRITICAL;
  return result;
}

/* not implemented
int setsysinfo(op, buffer, nbytes, arg, flag)
unsigned op;
char *buffer;
unsigned nbytes;
unsigned arg;
unsigned flag;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buffer);
  result = syscall(SYS_setsysinfo, op, buffer, nbytes, arg, flag);
  EXIT_CRITICAL;
  return result;
}
*/

int settimeofday(tp, tzp)   /* ok */
#if __FreeBSD__ >= 2
const struct timeval *tp;
const struct timezone *tzp;
#else
struct timeval *tp;
struct timezone *tzp;
#endif
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tp);
  MAKE_READABLE(tzp);
  result = syscall(SYS_settimeofday, tp, tzp);
  EXIT_CRITICAL;
  return result;
}

int sigaction(sig, act, oact)
int sig;
const struct sigaction *act;
struct sigaction *oact;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(act);
  MAKE_WRITABLE(oact);
  result = syscall(SYS_sigaction, sig, act, oact);
  EXIT_CRITICAL;
  return result;
}

int sigaltstack(ss, oss)   /* ok */
const struct sigaltstack *ss;
struct sigaltstack *oss;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ss);
  MAKE_WRITABLE(oss);
  result = syscall(SYS_sigaltstack, ss, oss);
  EXIT_CRITICAL;
  return result;
}

int socketpair(d, type, protocol, sv)   /* ok */
int d, type, protocol;
int sv[2];
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sv);
  result = syscall(SYS_socketpair, d, type, protocol, sv);
  EXIT_CRITICAL;
  return result;
}

int stat(path, buf)   /* ok */
const char *path;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_stat, path, buf);
  EXIT_CRITICAL;
  return result;
}

int swapon(special)   /* ok */
const char *special;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  result = syscall(SYS_swapon, special);
  EXIT_CRITICAL;
  return result;
}

int symlink(name1, name2)   /* ok */
const char *name1;
const char *name2;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = syscall(SYS_symlink, name1, name2);
  EXIT_CRITICAL;
  return result;
}

int truncate(path, length)   /* ok */
const char *path;
off_t length;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  /* The casts below pad "path" out to a 64-bit value as required. */
  result = __syscall(SYS_truncate, (u_quad_t)(u_long)path, length);
  EXIT_CRITICAL;
  return result;
}

int uname(name)   /* ok */
struct utsname *name;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = syscall(SYS_uname, name);
  EXIT_CRITICAL;
  return result;
}

int unlink(path)   /* ok */
const char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_unlink, path);
  EXIT_CRITICAL;
  return result;
}

/* not implemented
int ustat(dev, buf)
dev_t dev;
struct ustat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_ustat, dev, buf);
  EXIT_CRITICAL;
  return result;
}
*/

int utimes(file, tvp)   /* ok */
#if __FreeBSD__ >= 2
const char *file;
const struct timeval *tvp;
#else
char *file;
struct timeval *tvp;
#endif
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  MAKE_READABLE(tvp);
  result = syscall(SYS_utimes, file, tvp);
  EXIT_CRITICAL;
  return result;
}

pid_t wait(status)   /* ok */
int *status;
{
  return wait3(status, 0, 0);
}

pid_t wait3(status, options, rusage)   /* ok */
int *status;
int options;
struct rusage *rusage;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_wait4, -1, status, options, rusage);
  EXIT_CRITICAL;
  return result;
}

pid_t wait4(wpid, status, options, rusage)   /* ok */
pid_t wpid;
int *status;
int options;
struct rusage *rusage;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_wait4, wpid, status, options, rusage);
  EXIT_CRITICAL;
  return result;
}

pid_t waitpid(pid, status, options)   /* ok */
pid_t pid;
int *status;
int options;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  result = syscall(SYS_wait4, pid, status, options, NULL);
  EXIT_CRITICAL;
  return result;
}

ssize_t writev(fd, iov, ioveclen)   /* ok */
int fd;
const struct iovec *iov;
int ioveclen;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < ioveclen; i++) {
      if (iov[i].iov_len > 0) {
        MAKE_READABLE(iov[i].iov_base);
      }
    }
  }
  result = syscall(SYS_writev, fd, iov, ioveclen);
  EXIT_CRITICAL;
  return result;
}

/* fork also requires special treatment, although it takes no
   argument.  fork crashes Ultrix if some pages are unreadable, so we
   must unprotect the heap before calling fork */
/* I don't know what happens in FreeBSD, so I just leave it in here */
pid_t fork()
{
  pid_t result;
  pid_t me = getpid();

  ENTER_CRITICAL;
  if (RTCSRC_FinishVM)  RTCSRC_FinishVM();
  result = syscall(SYS_fork);
  EXIT_CRITICAL;
  if (result == me) {
    result = 0;
  }
  return result;
}

