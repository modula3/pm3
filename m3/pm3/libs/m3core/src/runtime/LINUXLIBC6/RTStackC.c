#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

/* This is a partial stack walker for LINUXELF (dagenais@vlsi.polymtl.ca) */

/* TYPE Frame = RECORD pc, sp, fp: ADDRESS END; */

/* Local variables are referred to as an offset from the FP.
   However, the RTException model assumes offsets for local variables from the
   sp field of the RTStack.Frame structure, so we store the true FP in sp and
   the true SP in fp. */

typedef struct {
  void *pc;
  void *sp;
  void *fp;
} Frame;

/*---------------------------------------------------------------------------*/
/* PROCEDURE GetThreadFrame (VAR f: Frame;  start: ADDRESS;  len: INTEGER);
   Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. */

void RTStack__GetThreadFrame (Frame *f, char *start, int len)
{
  jmp_buf *env = (jmp_buf *)start;

  if (len == sizeof (jmp_buf)) {
    f->pc = (void *)(env[JB_PC]);
    f->sp = (void *)(env[JB_BP]);
    f->fp = (void *)(env[JB_SP]);
  } else
    f->pc = f->sp = 0;
}

char* RTStack__ProcName (Frame* f)
{
  /* No data - Always return nil. */
  return 0;
}

/*---------------------------------------------------------------------------*/
/* PROCEDURE Unwind (READONLY f: Frame);
   Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call. */

void RTStack__Unwind (target)
     Frame *target;
{
  abort ();
}

