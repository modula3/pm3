#include <windows.h>
#define abort ff



void ff() {
printf("OUUCH!!!\n");
}
#if 0
#endif

fork () { abort(); }


kill() { abort(); }

wait() { abort(); }
vfork() {abort(); }		   






getkey() { abort(); }
getgid() { return 1; }
getpagesize() { return 4096;}

char **environ;

getuit() { return 0;}


ScreenRows() { return 20; }
ScreenCols() { return 80;}


/*int sys_nerr=1;*/
char *sys_siglist[] = {"OOPS"};


mmalloc(x,y) { return malloc(y) ;}
mrealloc(x,y,z) { return realloc(y,z);}
mcalloc(x,y,z) { return calloc(y,z) ;}
mfree(x,y) { return free(y); }
mmcheck() { return 1;}
mmtrace() {}
#undef isascii
isascii (x) { return __isascii (x);}

getuid() { return 1;}


ScreenGetCursor() { abort(); }
ScreenSetCursor() { abort(); }
char *environ_vector[20];



#if 0
c_error() { abort(); }
c_parse() { abort(); }
#endif





chown() {}
pipe() {}
sbrk() { return 0;}

operator_chars() {}


#if 0
core_file_matches_executable_p(x,y) { abort();}
coffstab_build_psymtabs() { abort(); }
char *chill_demangle(const char *a) { return 0;}
f_error () { abort(); }
f_parse() { abort(); }


#endif
char *tilde_expand(char *n) {return strdup(n);}

isnan() { return 0;}

sleep(int secs) 
{
  unsigned long stop = GetTickCount() + secs * 1000;

  /* on win32s, sleep returns immediately if there's
     nothing else ready to run, so loop. */
  while (GetTickCount() < stop)
    Sleep (100);
}



strsignal()
{
return 0;
}
