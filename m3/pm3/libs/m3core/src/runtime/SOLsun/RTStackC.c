#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/frame.h>

/* jmp_buf indices */
#define JB_SP 1
#define JB_PC 2
#define JB_FP 3
#define JB_I7 4

/* TYPE Frame = RECORD pc, sp, fp: ADDRESS END; */
/* On the SPARC local variables are referred to as an offset from the FP.
   However, the RTException model assumes offsets for local variables from the
   sp field of the RTStack.Frame structure, so we store the true FP in sp and
   the true SP in fp.  That is, we reverse the true sense of SPARC FP and
   SP. */
typedef struct {
  void *pc;
  struct frame *sp;		/* this is actually SPARC fp */
  struct frame *fp;		/* this is actually SPARC sp */
} Frame;

void RTStack__CurFrame (Frame *); /* defined in RTStackASM.s */

/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void RTStack__GetThreadFrame (Frame *f, char *start, int len)
{
  jmp_buf *env = (jmp_buf *)start;

  if (len == sizeof (jmp_buf)) {
    f->pc = (void *)(*env)[JB_PC];
    f->sp = (void *)(*env)[JB_FP];
    f->fp = (void *)(*env)[JB_SP];
  } else
    f->pc = f->sp = 0;
}

char* RTStack__ProcName (Frame* f)
{
  /* No data - Always return nil. */
  return 0;
}

void RTStack__PrevFrame (Frame* callee, Frame* caller)
{
  Frame f;

  if (callee == 0) abort();
  if (caller == 0) abort();

  /* This seems to work just fine for Solaris even when unwinding through
     signal trampoline frames */
  f.fp = callee->sp;
  f.sp = (f.fp) ? f.fp->fr_savfp : 0;
  (int)f.pc = callee->fp->fr_savpc;

  *caller = f;
}

void RTStack__Unwind (Frame* target)
{
  jmp_buf env;
  {
    setjmp (env);
    env[JB_PC] = (int)target->pc + 8; /* +8 for return address */
    env[JB_SP] = (int)target->fp;
    env[JB_FP] = (int)target->sp;
    env[JB_I7] = target->fp->fr_savpc;
  }
  longjmp(env, 1);
}
