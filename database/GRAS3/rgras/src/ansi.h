/*
 *
 * Headerfile with ansi style prototypes of system calls and
 * subroutines offered by various libraries. Including this file
 * in other modules enables the gcc to do more checks (correct
 * parameter profile, e.g. order of parameters and appropriate
 * type of each parameter, correct return value, ...). If
 * prototypes are missing, you are free to introduce them in this
 * file. Please try to use the style I followed, e.g.
 * selfspeaking parameters (in most cases).  Additionally
 * preserve the alphabetical order of prototypes. Additional
 * header files should only be included if they are neccessary
 * for this header!  Take care of the fact, that a declaration of
 * the form 'type functionname()' differs significantly from the
 * declaration of the form 'type functionname(void)'. The former
 * allowes a random parameter list while the latter forces an
 * empty list.
 *
 * Style in which prototypes are declared:
 *
 * extern 'returntype'
 *         'functionname'('param-1-type' 'param-1-name',
 *                        ...
 *                        'param-n-type' 'param-n-name');
 *
 * To get quite near the place you want to insert some new declaration
 * or to take a look at, a comment exist for every letter of the
 * alphabet in the form 'xxx XXX label to search for' (x and X
 * represent the lowercase and uppercase letters, respectively). So
 * searching for 'xxx' or 'XXX' offers a good chance to get quite near
 * the location you want to be.
 *
 */
#if !defined (_ANSI_PT_)

#define _ANSI_PT_

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <dirent.h>
#include <setjmp.h>

/* !!! neu */

#ifdef SOL2
#include <rpc/rpc.h>
#include <rpc/clnt_soc.h>
#include <rpc/svc_soc.h>
/*#undef gettimeofday*/
#define getdtablesize() 64

#include <netdb.h>  /* MAXHOSTNAMELEN */
#include <stdlib.h>
#include <string.h> /* strrchr */
#include <unistd.h> /* getpid */
#include <limits.h> /* INT_MAX,...*/
#include <ctype.h> /* isspace*/

extern long int random( void);

#define bzero(dest, count) memset( dest, 0, count)
#define bcmp(src,dest, count) memcmp( src,dest, count)

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif
#endif

#ifdef LINUXELF
#include <ctype.h> /* isspace*/
#include <sys/ioctl.h>
#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif
#endif

#if 0
extern int
     getopt(int	       argc,
	    char       **argv,
	    const char *optstring);
extern int
     fprintf(FILE       *stream,
	     const char *format,
	     ...);


extern int
     system(const char *string);
#endif

#ifdef SUNOS4
/*
 * aaa AAA label to search for
 */
extern void
     abort(void);

extern int
     abs(int i);

extern int
     accept(int		    sock,
	    struct sockaddr *addr,
	    int		    *addrlen);

extern int
     access(const char *path,
	    int	       mode);

extern int
     acct(const char *path);

extern double
     acos(double x);

extern double
     acosh(double x);

extern int
     addexportent(FILE *filep,
		  char *dirname,
		  char *options);

extern int
     adjtime(struct timeval *delta,
	     struct timeval *old_delta);

extern unsigned int
     alarm(unsigned int seconds);

extern char *
     asctime(const struct tm *timep);

extern double
     asin(double x);

extern double
     asinh(double x);

extern double
     atan(double x);

extern double
     atanh(double x);

extern double
     atof(const char *str);

extern int
     atoi(const char *str);

extern long int
     atol(const char *str);


/*
 * bbb BBB label to search for
 */
extern char *
     basename(char *path);

extern int
     bcmp(const void *str1,
	  const void *str2,
	  int	     cmp_length);

extern void
     bcopy(const void *str1,
	   const void *str2,
	   int	      cmp_length);

extern int
     beep(void);

extern int
     bind(int		  sock,
	  struct sockaddr *name,
	  int		  namelen);

extern int
     bindresvport(int		     sock,
		  struct sockaddr_in *addr);

extern int
     brk(void *addr);

extern void *
     bsearch(const void *key,
	     const void *base,
	     size_t     num_of_el,
	     size_t     size_of_key,
	     int        (*) (const void *, const void *));

extern void
     bzero(void	  *str,
	   size_t length);

/*
 * ccc CCC label to search for
 */
extern void *
     calloc(size_t num_of_el,
	    size_t el_size);

extern double
     ceil(double x);

extern int
     chdir(const char *path);

extern int
     chmod(const char *path,
	   mode_t     mode);

extern int
     chown(const char *path,
	   uid_t      owner,
	   gid_t      group);

extern int
     chroot(const char *dirname);

extern clock_t
     clock(void);

extern int
     close(int fd);

extern int
     closedir(DIR *dirname);

extern int
     connect(int	     sock,
	     struct sockaddr *name,
	     int	     namelen);

extern double
     cos(double x);

extern double
     cosh(double x);

extern int
     creat(const char *path,
	   mode_t     mode);

extern int
     crmode(void);

extern char *
     crypt(const char *key,
	   const char *salt);

extern char *
     ctermid(char *str);

extern char *
     ctime(const time_t *theclock);

extern char *
     cuserid(char *str);

/*
 * ddd DDD label to search for
 */
extern void
     dbmclose(void);

extern void
     dbminit(const char *file);

extern int
     def_prog_mode(void);

extern int
     def_shell_mode(void);

extern int
     dlclose(void *handle);

extern char *
     dlerror(void);

extern void *
     dlopen(char *path,
	    int	 mode);

extern void *
     dlsym(void *handle,
	   char *symbol);

extern double
     drand48(void);

extern int
     dup(int fd);

extern int
     dup2(int fd1,
	  int fd2);

/*
 * eee EEE label to search for
 */
extern char *
     ecvt(double value,
	  int	 ndigit,
	  int	 *decpt,
	  int	 *sign);

extern void
     encrypt(char *block,
	     int  edflag);

extern void
     endgrent(void);

extern void
     endhostent(void);

extern void
     endnetent(void);

extern void
     endprotoent(void);

extern void
     endpwent(void);

extern void
     endrpcent(void);

extern void
     endservent(void);

extern void
     endspent(void);

extern void
     endutent(void);

extern void
     endutxent(void);

extern int
     endwin(void);

extern double
     erand48(short unsigned int xsubi[3]);

extern char
     erasechar(void);

extern double
     erf(double x);

extern double
     erfc(double x);

extern int
     execl(const char *path,
	   const char *arg0,
	   ...);

extern int
     execle(const char *path,
	    const char *arg0,
	    ...);

extern int
     execlp(const char *file,
	    const char *arg0,
	    ...);

extern int
     execv(const char  *path,
	   char *const *argv);

extern int
     execve(const char *path,
	    char *const *argv,
	    char *const *envp);

extern int
     execvp(const char  *file,
	    char *const *argv);

extern void
     exit(int status);

extern double
     exp(double x);

/*
 * fff FFF label to search for
 */
extern double
     fabs(double x);

extern int
     fchdir(int fd);

extern int
     fchmod(int	   fd,
	    mode_t mode);

extern int
     fchown(int	  fd,
	    uid_t owner,
	    gid_t group);

extern int
     fclose(FILE *filep);

extern int
     fcntl(int fd,
	   int cmd,
	   ...);

extern char *
     fcvt(double value,
	  int	 ndigit,
	  int	 *decpt,
	  int	 *sign);

extern FILE *
     fdopen(int	       fd,
	    const char *type);

extern int
     fflush(FILE *filep);

extern int
     ffs(int i);

extern int
     fgetc(FILE *filep);

extern struct group *
     fgetgrent(FILE *filep);

extern struct passwd *
     fgetpwent(FILE *filep);

extern char *
     fgets(char *str,
	   int  n,
	   FILE *stream);

extern int
     filter(void);

extern int
     finite(double x);

extern int
     flash(void);

extern int
     flock(int fd,
	   int operation);

extern double
     floor(double x);

extern double
     fmod(double x,
	  double y);

extern FILE *
     fopen(const char *filename,
	   const char *type);

extern pid_t
     fork(void);

extern long int
     fpathconf(int fd,
	       int name);

extern int
     fprintf(FILE       *stream,
	     const char *format,
	     ...);

extern int
     fputc(int  ch,
	   FILE *stream);

extern int
     fputs(const char *str,
	   FILE	      *stream);

extern size_t
     fread(void	  *ptr,
	   size_t size,
	   size_t nitems,
	   FILE	  *stream);

extern void
     free(void *ptr);

extern FILE *
     freopen(const char *filename,
	     const char *type,
	     FILE       *stream);

extern double
     frexp(double value,
	   int	  *eptr);

extern int
     fscanf(FILE       *stream,
	    const char *format,
	    ...);

extern int
     fseek(FILE	    *stream,
	   long int offset,
	   int	    ptrname);

extern int
     fsync(int fd);

extern long int
     ftell(FILE *stream);

extern key_t
     ftok(const char *path,
	  int	     id);

extern int
     ftruncate(int   fd,
	       off_t length);

extern size_t
     fwrite(const void *ptr,
	    size_t     size,
	    size_t     nitems,
	    FILE       *stream);

/*
 * ggg GGG label to search for
 */
extern double
     gamma(double x);

extern char *
     gcvt(double value,
	  int	 ndigit,
	  char	 *buf);

extern char *
     getcwd(char   *buf,
	    size_t size);

extern int
     getdents(int	    fd,
	      struct dirent *buf,
	      unsigned int  nbytes);

extern int
     getdomainname(char *name,
		   int  namelen);

extern int
     getdtablesize(void);

extern gid_t
     getegid(void);

extern char *
     getenv(const char *name);

extern uid_t
     geteuid(void);

extern gid_t
     getgid(void);

extern struct group *
     getgrent(void);

extern struct group *
     getgrgid(gid_t gid);

extern struct group *
     getgrnam(const char *name);

extern int
     getgroups(int   gidsetlen,
	       gid_t *gidset);

extern struct hostent *
     gethostbyaddr(void *addr,
		   int  len,
		   int  type);

extern struct hostent *
     gethostbyname(char *name);

extern struct hostent *
     gethostent(void);

extern long int
     gethostid(void);

extern int
     gethostname(char *name,
		 int  namelen);

extern int
     getitimer(int	        which,
	       struct itimerval *value);

extern char *
     getlogin(void);

extern long unsigned int
     getmouse(void);

extern int
     getmsg(int		  fd,
	    struct strbuf *ctlptr,
	    struct strbuf *dataptr,
	    int		  *flags);

extern void
     get_myaddress(struct sockaddr_in *addr);

extern struct netent *
     getnetbyaddr(long int net,
		  int	   type);

extern struct netent *
     getnetbyname(long int net,
		  int	   type);

extern struct netent *
     getnetent(void);

extern int
     getopt(int	       argc,
	    char       **argv,
	    const char *optstring);

extern int
     getpagesize(void);

extern char *
     getpass(const char *prompt);

extern pid_t
     getpgid(pid_t pid);

extern pid_t
     getpgrp(void);

extern pid_t
     getpgrp2(pid_t pid);

extern pid_t
     getpid(void);

extern pid_t
     getppid(void);

extern int
     getpriority(int which,
		 int who);

extern struct protoent *
     getprotobyname(const char *name);

extern struct protoent *
     getprotobynumber(int proto);

extern struct protoent *
     getprotoent(void);

extern int
     getpw(int uid,
	   char *buf);

extern struct passwd *
     getpwent(void);

extern struct passwd *
     getpwnam(const char *name);

extern struct passwd *
     getpwuid(uid_t uid);

extern void
     getrlimit(int resource,
	       struct rlimit *rlp);

extern struct rpcent *
     getrpcbyname(const char *name);

extern struct rpcent *
     getrpcbynumber(int number);

extern struct rpcent *
     getrpcent(void);

extern int
     getrpcport(char *host,
		int  prognum,
		int  versnum,
		int  proto);

extern void
     getrusage(int	     who,
	       struct rusage *rusage);

extern char *
     gets(char *str);

extern struct servent *
     getservbyname(char *name,
		   char *protocol);

extern struct servent *
     getservbyport(int  port,
		   char *protocol);

extern struct servent *
     getservent(void);

extern int
     getsockname(int		 sock,
		 struct sockaddr *name,
		 int		 *namelen);

extern int
     getsockopt(int  sock,
		int  level,
		int  optname,
		void *optval,
		int  *optlen);

extern int
     gettimeofday(struct timeval  *tm,
		  struct timezone *tz);

extern uid_t
     getuid(void);

extern int
     getw(FILE *stream);

extern char *
     getwd(char *pathname);

extern struct tm *
     gmtime(const time_t *theclock);

/*
 * hhh HHH label to search for
 */
extern int
     host2netname(char *netname,
		  char *host,
		  char *domain);

extern double
     hypot(double x,
	   double y);

/*
 * iii III label to search for
 */
extern char *
     index(const char *str,
	   int	      c);

extern long unsigned int
     inet_addr(char *cp);

extern int
     inet_lnaof(struct in_addr in);

extern struct in_addr
     inet_makeaddr(int net,
		   int lna);

extern long unsigned int
     inet_network(char *cp);

extern char *
     inet_ntoa(struct in_addr in);

extern int
     inet_netof(struct in_addr in);

extern int
     ioctl(int fd,
	   int request,
	   ...);

extern int
     isalnum(int c);

extern int
     isalpha(int c);

extern int
     isascii(int c);

extern int
     isatty(int c);

extern int
     iscntrl(int c);

extern int
     isdigit(int c);

extern int
     isendwin(void);

extern int
     isgraph(int c);

extern int
     isinf(double x);

extern int
     islower(int c);

extern int
     isnan(double x);

extern int
     isnand(double x);

extern int
     isprint(int c);

extern int
     ispunct(int c);

extern int
     isspace(int c);

extern int
     isupper(int c);

extern int
     isxdigit(int c);

/*
 * jjj JJJ label to search for
 */
extern long int
     jrand48(short unsigned int xsubi[3]);

/*
 * kkk KKK label to search for
 */
extern int
     kill(pid_t pid,
	  int sig);

extern char
     killchar(void);

extern int
     killpg(int pgrp,
	    int sig);

/*
 * lll LLL label to search for
 */
extern long int
     labs(long int n);

extern void
     lcong48(short unsigned int param[7]);

extern double
     ldexp(double x,
	   int	  n);

extern void *
     lfind(const void *key,
	   const void *base,
	   size_t     *nelp,
	   size_t     width,
	   int        (*) (const void *, const void *));

extern double
     lgamma(double x);

extern int
     link(const char *path1,
	  const char *path2);

extern int
     listen(int sock,
	    int backlog);

extern struct lconv *
     localeconv(void);

extern struct tm *
     localtime(const time_t *theclock);

extern double
     log(double x);

extern double
     log10(double x);

extern void
     longjmp(jmp_buf env,
	     int     val);

extern char *
     longname(void);

extern long int
     lrand48(void);

extern void *
     lsearch(const void *key,
	     void       *base,
	     size_t     *nelp,
	     size_t     width,
	     int        (*) (const void *, const void *));

extern off_t
     lseek(int	 fd,
	   off_t offset,
	   int	 whence);

/*
 * mmm MMM label to search for
 */
extern struct mallinfo
     mallinfo(void);

extern void *
     malloc(size_t size);

extern void *
     memalign(size_t alignment,
	      size_t size);

extern void *
     memccpy(void       *dest,
	     const void *src,
	     int        stop_char,
	     size_t     n);

extern void *
     memchr(const void *str,
	    int	       search_char,
	    size_t     n);

extern int
     memcmp(const void *str1,
	    const void *str2,
	    size_t     n);

extern void *
     memcpy(void       *dest,
	    const void *src,
	    size_t     n);

extern void *
     memset(void   *str,
	    int	   set_char,
	    size_t n);

extern int
     mkdir(const char *path,
	   mode_t     type);

extern time_t
     mktime(struct tm *theclock);

extern long int
     mrand48(void);

extern int
     msgctl(int msqid,
	    int cmd,
	    ...);

extern int
     msgget(key_t key,
	    int	  msgflag);

extern int
     msgrcv(int	     msqid,
	    void     *msgp,
	    size_t   msgsz,
	    long int msgtyp,
	    int	     msgflag);

extern int
     msgsnd(int	       msqid,
	    const void *msgp,
	    size_t     msgsz,
	    int	       msgflag);

/*
 * nnn NNN label to search for
 */
extern int
     nice(int nicelevel);

extern long int
     nrand48(short unsigned int xsubi[3]);

/*
 * ooo OOO label to search for
 */
extern int
     open(const char *path,
	  int        flags,
	  ...);

extern DIR *
     opendir(const char *dirname);

/*
 * ppp PPP label to search for
 */
extern long int
     pathconf(const char *path,
	      int	 name);

extern int
     pause(void);

extern void
     perror(const char *message);

extern int
     pipe(int fd[2]);

extern int
     plock(int op);

extern FILE *
     popen(const char *command,
	   const char *type);

extern double
     pow(double x,
	 double y);

extern int
     printf(const char *format,
	    ...);

extern int
     putenv(char *envstr);

extern int
     putmsg(int		        fd,
	    const struct strbuf *ctlptr,
	    const struct strbuf *dataptr,
	    int		        flags);

extern int
     putpwent(const struct passwd *password,
	      FILE		  *filep);

extern int
     puts(const char *str);

extern int
     putw(int w,
	  FILE *stream);

/*
 * qqq QQQ label to search for
 */
extern void
     qsort(void   *base,
	   size_t nel,
	   size_t width,
	   int    (*) (const void *, const void *));

/*
 * rrr RRR label to search for
 */
extern int
     rand(void);

extern long int
     random(void);

extern int
     raw(void);

extern struct dirent *
     readdir(DIR *dirp);

extern int
     readlink(const char *path,
	      void	 *buf,
	      int	 bufsize);

extern void *
     realloc(void   *ptr,
	     size_t size);

extern char *
     realpath(char *path,
	      char *resolved_path);

extern int
     recvfrom(int	      sock,
	      void	      *buf,
	      int	      len,
	      int	      flags,
	      struct sockaddr *from,
	      int	      *fromlen);


extern int
     rename(const char *path1,
	    const char *path2);

extern int
     resetty(void);

extern void
     rewind(FILE *stream);

extern double
     rint(double x);

extern int
     rmdir(const char *dirname);

/*
 * sss SSS label to search for
 */
extern int
     savetty(void);

extern void *
     sbrk(int incr);

extern double
     scalb(double x,
	   double y);

extern int
     scanf(const char *format,
	   ...);

extern short unsigned int *
     seed48(short unsigned int seed16v[3]);

extern void
     seekdir(DIR      *dirp,
	     long int loc);

extern int
     select(int		   width,
	    fd_set	   *readfds,
	    fd_set	   *writefds,
	    fd_set	   *execptfds,
	    struct timeval *timeout);

extern int
     send(int  sock,
	  char *msg,
	  int  len,
	  int  flags);

extern int
     sendto(int		    sock,
	    char	    *msg,
	    int		    len,
	    int		    flags,
	    struct sockaddr *to,
	    int		    tolen);

extern void
     setbuf(FILE *stream,
	    char *buf);

extern int
     setdomainname(char *name,
		   int  namelen);

extern FILE *
     setexportent(void);

extern int
     setgid(gid_t gid);

extern void
     setgrent(void);

extern int
     setgroups(int	   ngroups,
	       const gid_t *gidset);

extern void
     sethostent(int stayopen);

extern int
     sethostname(char *name,
		 int  namelen);

extern int
     setitimer(int	        which,
	       struct itimerval *value,
	       struct itimerval *oldvalue);

extern int
     setjmp(jmp_buf env);

extern void
     setkey(const char *key);

extern void *
     setnetconfig(void);

extern void
     setnetent(int stayopen);

extern void *
     setnetpath(void);

extern int
     setpgid(pid_t pid,
	     pid_t pgid);

extern pid_t
     setpgrp(void);

extern int
     setpriority(int which,
		 int who,
		 int niceval);

extern void
     setprotoent(int stayopen);

extern void
     setpwent(void);

extern int
     setregid(gid_t rgid,
	      gid_t egid);

extern int
     setreuid(uid_t ruid,
	      uid_t euid);

extern void
     setrlimit(int	     resource,
	       struct rlimit *rlp);

extern void
     setrpcent(int stayopen);

extern void
     setservent(int stayopen);

extern pid_t
     setsid(void);

extern int
     setsockopt(int  sock,
		int  level,
		int  optname,
		void *optval,
		int  optlen);

extern void
     setspent(void);

extern char *
     setstate(char *state);

extern int
     setuid(uid_t uid);

extern int
     sigblock(int mask);

extern void
     siglongjmp(sigjmp_buf env,
		int	   val);

extern void (*
     signal (int  sig,
	     void (*) (int raisedsig))) (int raisedsig);

extern int
     sigpause(int sigmask);

extern int
     sigpending(sigset_t *sigmask);

extern void (*
     sigset (int  sig,
	     void (*) (int raisedsig))) (int raisedsig);

extern int
     sigsetjmp(sigjmp_buf env,
	       int	  val);

extern int
     sigsetmask(int mask);

extern int
     sigsuspend(sigset_t *sigmask);

extern double
     sin(double x);

extern double
     sinh(double x);

extern unsigned int
     sleep(unsigned int seconds);

extern int
     socket(int domain,
	    int type,
	    int protocol);

extern double
     sqrt(double x);

extern void
     srand(unsigned int seed);

extern void
     srand48(long int seedval);

extern void
     srandom(int seed);

extern int
     sscanf(const char *str,
	    const char *format,
	    ...);

extern void (*
     ssignal (int  sig,
	      void (*) (int raisedsig))) (int raisedsig);

extern int
     stime(const time_t *theclock);

extern char *
     strcat(char       *str1,
	    const char *str2);

extern char *
     strchr(const char *str,
	    int	       search_char);

extern int
     strcmp(const char *str1,
	    const char *str2);

extern int
     strcasecmp(const char *str1,
	    const char *str2);

extern char *
     strcpy(char       *dest,
	    const char *src);

extern size_t
     strcspn(const char *str,
	     const char *pattern);

extern char *
     strdup(const char *str);

extern char *
     strerror(int status);

extern size_t
     strftime(char	      *buf,
	      size_t	      bufsize,
	      const char      *fmt,
	      const struct tm *tm);

extern size_t
     strlen(const char *str);

extern char *
     strncat(char       *str1,
	     const char *str2,
	     size_t     n);

extern int
     strncmp(const char *str1,
	     const char *str2,
	     size_t     n);

extern char *
     strncpy(char       *dest,
	     const char *src,
	     size_t     length);

extern char *
     strpbrk(const char *str1,
	     const char *str2);

extern char *
     strrchr(const char *str,
	     int        c);

extern char *
     strrspn(const char *str1,
	     const char *str2);

extern size_t
     strspn(const char *str1,
	    const char *str2);

extern char *
     strstr(const char *str1,
	    const char *str2);

extern double
     strtod(const char *str,
	    char       **ptr);

extern char *
     strtok(char       *str1,
	    const char *str2);

extern long int
     strtol(const char *str,
	    char       **ptr,
	    int	       base);

extern long unsigned int
     strtoul(const char *str,
	     char       **ptr,
	     int        base);

extern int
     symlink(const char *name1,
	     const char *name2);

extern void
     sync(void);

extern int
     syscall(int number,
	     ...);

extern long int
     sysconf(int name);

extern int
     system(const char *string);

/*
 * ttt TTT label to search for
 */
extern double
     tan(double x);

extern double
     tanh(double x);

extern long int
     telldir(DIR *dirp);

extern char *
     tempnam(const char *dir,
	     const char *pfx);

extern void
     termerr(void);

extern char *
     termname(void);

extern time_t
     time(time_t *theclock);

extern clock_t
     times(struct tms *buffer);

extern char *
     timezone(int zone,
	      int dst);

extern FILE *
     tmpfile(void);

extern char *
     tmpnam(char *dir);

extern int
     toascii(int c);

extern int
     tolower(int c);

extern int
     toupper(int c);

extern int
     truncate(const char *path,
	      off_t	 length);

extern void
     tzset(void);

/*
 * uuu UUU label to search for
 */
extern long int
     ulimit(int	     cmd,
	    long int newlimit);

extern mode_t
     umask(mode_t mask);

extern int
     umount(const char *name);

extern int
     ungetc(int	 c,
	    FILE *stream);

extern int
     ungetch(int c);

extern int
     unlink(const char *path);

/*
 * vvv VVV label to search for
 */
extern void *
     valloc(size_t size);

extern pid_t
     vfork(void);

extern int
     vhangup(void);

/*
 * www WWW label to search for
 */
extern pid_t
     wait3(union wait	 *statusp,
	   int		 options,
	   struct rusage *rusage);

extern pid_t
     waitpid(pid_t pid,
	     int   *statusp,
	     int   options);


/*
 * yyy YYY label to search for
 */

/*
 * zzz ZZZ label to search for
 */

/*
 * ___ label to search for
 */
extern int
     _rpc_dtablesize(void);

#endif


#ifdef __cplusplus
}
#endif

#endif

