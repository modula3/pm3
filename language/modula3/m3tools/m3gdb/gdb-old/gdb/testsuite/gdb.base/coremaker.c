/* Simple little program that just generates a core dump from inside some
   nested function calls. */

int coremaker_data = 1;	/* In Data section */
int coremaker_bss;	/* In BSS section */
#ifndef __STDC__
#define	const	/**/
#endif
const int coremaker_ro = 201;	/* In Read-Only Data section */
#include <signal.h>

void
func2 ()
{
  int coremaker_local[5];
  int i;

#ifdef SA_FULLDUMP
  /* Force a corefile that includes the data section for AIX.  */
  {
    struct sigaction sa;

    sigaction (SIGABRT, (struct sigaction *)0, &sa);
    sa.sa_flags |= SA_FULLDUMP;
    sigaction (SIGABRT, &sa, (struct sigaction *)0);
  }
#endif

  /* Make sure that coremaker_local doesn't get optimized away. */
  for (i = 0; i < 5; i++)
    coremaker_local[i] = i;
  coremaker_bss = 0;
  for (i = 0; i < 5; i++)
    coremaker_bss += coremaker_local[i];
  coremaker_data = coremaker_ro + 1;
  abort ();
}

void
func1 ()
{
  func2 ();
}

main ()
{
  func1 ();
}

