/* Simulator for the Hitachi SH architecture.

   Written by Steve Chamberlain of Cygnus Support.
   sac@cygnus.com

   This file is part of SH sim


		THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

*/

#include "config.h"

#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "sysdep.h"
#include "bfd.h"
#include "callback.h"
#include "remote-sim.h"

/* This file is local - if newlib changes, then so should this.  */
#include "syscall.h"

#include <math.h>

#ifdef _WIN32
#include <float.h>		/* Needed for _isnan() */
#define isnan _isnan
#endif

#ifndef SIGBUS
#define SIGBUS SIGSEGV
#endif

#ifndef SIGQUIT
#define SIGQUIT SIGTERM
#endif

#ifndef SIGTRAP
#define SIGTRAP 5
#endif

extern unsigned char sh_jump_table[], sh_dsp_table[0x1000], ppi_table[];

int sim_write (SIM_DESC sd, SIM_ADDR addr, unsigned char *buffer, int size);

#define O_RECOMPILE 85
#define DEFINE_TABLE
#define DISASSEMBLER_TABLE

/* Define the rate at which the simulator should poll the host
   for a quit. */
#define POLL_QUIT_INTERVAL 0x60000

typedef union
{

  struct
  {
    int regs[16];
    int pc;

    /* System registers.  For sh-dsp this also includes A0 / X0 / X1 / Y0 / Y1
       which are located in fregs, i.e. strictly speaking, these are
       out-of-bounds accesses of sregs.i .  This wart of the code could be
       fixed by making fregs part of sregs, and including pc too - to avoid
       alignment repercussions - but this would cause very onerous union /
       structure nesting, which would only be managable with anonymous
       unions and structs.  */
    union
      {
	struct
	  {
	    int mach;
	    int macl;
	    int pr;
	    int dummy3, dummy4;
	    int fpul; /* A1 for sh-dsp -  but only for movs etc.  */
	    int fpscr; /* dsr for sh-dsp */
	  } named;
	int i[7];
      } sregs;

    /* sh3e / sh-dsp */
    union fregs_u
      {
	float f[16];
	double d[8];
	int i[16];
      }
    fregs[2];

    /* Control registers; on the SH4, ldc / stc is privileged, except when
       accessing gbr.  */
    union
      {
	struct
	  {
	    int sr;
	    int gbr;
	    int vbr;
	    int ssr;
	    int spc;
	    int mod;
	    /* sh-dsp */
	    int rs;
	    int re;
	    /* sh3 */
	    int bank[8];
	  } named;
	int i[16];
      } cregs;

    unsigned char *insn_end;

    int ticks;
    int stalls;
    int memstalls;
    int cycles;
    int insts;

    int prevlock;
    int thislock;
    int exception;

    int end_of_registers;

    int msize;
#define PROFILE_FREQ 1
#define PROFILE_SHIFT 2
    int profile;
    unsigned short *profile_hist;
    unsigned char *memory;
    int xyram_select, xram_start, yram_start;
    unsigned char *xmem;
    unsigned char *ymem;
    unsigned char *xmem_offset;
    unsigned char *ymem_offset;
  }
  asregs;
  int asints[40];
} saved_state_type;

saved_state_type saved_state;

struct loop_bounds { unsigned char *start, *end; };

/* These variables are at file scope so that functions other than
   sim_resume can use the fetch/store macros */

static int target_little_endian;
static int global_endianw, endianb;
static int target_dsp;
static int host_little_endian;
static char **prog_argv;

#if 1
static int maskw = 0;
#endif

static SIM_OPEN_KIND sim_kind;
static char *myname;


/* Short hand definitions of the registers */

#define SBIT(x) ((x)&sbit)
#define R0 	saved_state.asregs.regs[0]
#define Rn 	saved_state.asregs.regs[n]
#define Rm 	saved_state.asregs.regs[m]
#define UR0 	(unsigned int)(saved_state.asregs.regs[0])
#define UR 	(unsigned int)R
#define UR 	(unsigned int)R
#define SR0 	saved_state.asregs.regs[0]
#define CREG(n)	(saved_state.asregs.cregs.i[(n)])
#define GBR 	saved_state.asregs.cregs.named.gbr
#define VBR 	saved_state.asregs.cregs.named.vbr
#define SSR	saved_state.asregs.cregs.named.ssr
#define SPC	saved_state.asregs.cregs.named.spc
#define SREG(n)	(saved_state.asregs.sregs.i[(n)])
#define MACH 	saved_state.asregs.sregs.named.mach
#define MACL 	saved_state.asregs.sregs.named.macl
#define PR	saved_state.asregs.sregs.named.pr
#define FPUL	saved_state.asregs.sregs.named.fpul

#define PC insn_ptr



/* Alternate bank of registers r0-r7 */

/* Note: code controling SR handles flips between BANK0 and BANK1 */
#define Rn_BANK(n) (saved_state.asregs.cregs.named.bank[(n)])
#define SET_Rn_BANK(n, EXP) do { saved_state.asregs.cregs.named.bank[(n)] = (EXP); } while (0)


/* Manipulate SR */

#define SR_MASK_DMY (1 << 11)
#define SR_MASK_DMX (1 << 10)
#define SR_MASK_M (1 << 9)
#define SR_MASK_Q (1 << 8)
#define SR_MASK_I (0xf << 4)
#define SR_MASK_S (1 << 1)
#define SR_MASK_T (1 << 0)

#define SR_MASK_BL (1 << 28)
#define SR_MASK_RB (1 << 29)
#define SR_MASK_MD (1 << 30)
#define SR_MASK_RC 0x0fff0000
#define SR_RC_INCREMENT -0x00010000

#define M 	((saved_state.asregs.cregs.named.sr & SR_MASK_M) != 0)
#define Q 	((saved_state.asregs.cregs.named.sr & SR_MASK_Q) != 0)
#define S 	((saved_state.asregs.cregs.named.sr & SR_MASK_S) != 0)
#define T 	((saved_state.asregs.cregs.named.sr & SR_MASK_T) != 0)

#define SR_BL ((saved_state.asregs.cregs.named.sr & SR_MASK_BL) != 0)
#define SR_RB ((saved_state.asregs.cregs.named.sr & SR_MASK_RB) != 0)
#define SR_MD ((saved_state.asregs.cregs.named.sr & SR_MASK_MD) != 0)
#define SR_DMY ((saved_state.asregs.cregs.named.sr & SR_MASK_DMY) != 0)
#define SR_DMX ((saved_state.asregs.cregs.named.sr & SR_MASK_DMX) != 0)
#define SR_RC ((saved_state.asregs.cregs.named.sr & SR_MASK_RC))

/* Note: don't use this for privileged bits */
#define SET_SR_BIT(EXP, BIT) \
do { \
  if ((EXP) & 1) \
    saved_state.asregs.cregs.named.sr |= (BIT); \
  else \
    saved_state.asregs.cregs.named.sr &= ~(BIT); \
} while (0)

#define SET_SR_M(EXP) SET_SR_BIT ((EXP), SR_MASK_M)
#define SET_SR_Q(EXP) SET_SR_BIT ((EXP), SR_MASK_Q)
#define SET_SR_S(EXP) SET_SR_BIT ((EXP), SR_MASK_S)
#define SET_SR_T(EXP) SET_SR_BIT ((EXP), SR_MASK_T)

/* stc currently relies on being able to read SR without modifications.  */
#define GET_SR() (saved_state.asregs.cregs.named.sr - 0)

#define SET_SR(x) set_sr (x)

#define SET_RC(x) \
  (saved_state.asregs.cregs.named.sr \
   = saved_state.asregs.cregs.named.sr & 0xf000ffff | ((x) & 0xfff) << 16)

/* Manipulate FPSCR */

#define FPSCR_MASK_FR (1 << 21)
#define FPSCR_MASK_SZ (1 << 20)
#define FPSCR_MASK_PR (1 << 19)

#define FPSCR_FR  ((GET_FPSCR() & FPSCR_MASK_FR) != 0)
#define FPSCR_SZ  ((GET_FPSCR() & FPSCR_MASK_SZ) != 0)
#define FPSCR_PR  ((GET_FPSCR() & FPSCR_MASK_PR) != 0)

/* Count the number of arguments in an argv.  */
static int
count_argc (char **argv)
{
  int i;

  if (! argv)
    return -1;

  for (i = 0; argv[i] != NULL; ++i)
    continue;
  return i;
}

static void
set_fpscr1 (x)
	int x;
{
  int old = saved_state.asregs.sregs.named.fpscr;
  saved_state.asregs.sregs.named.fpscr = (x);
  /* swap the floating point register banks */
  if ((saved_state.asregs.sregs.named.fpscr ^ old) & FPSCR_MASK_FR
      /* Ignore bit change if simulating sh-dsp.  */
      && ! target_dsp)
    {
      union fregs_u tmpf = saved_state.asregs.fregs[0];
      saved_state.asregs.fregs[0] = saved_state.asregs.fregs[1];
      saved_state.asregs.fregs[1] = tmpf;
    }
}

/* sts relies on being able to read fpscr directly.  */
#define GET_FPSCR()  (saved_state.asregs.sregs.named.fpscr)
#define SET_FPSCR(x) \
do { \
  set_fpscr1 (x); \
} while (0)

#define DSR  (saved_state.asregs.sregs.named.fpscr)

int 
fail ()
{
  abort ();
}

#define RAISE_EXCEPTION(x) \
  (saved_state.asregs.exception = x, saved_state.asregs.insn_end = 0)

/* This function exists mainly for the purpose of setting a breakpoint to
   catch simulated bus errors when running the simulator under GDB.  */

void
raise_exception (x)
     int x;
{
  RAISE_EXCEPTION(x);
}

void
raise_buserror ()
{
  raise_exception (SIGBUS);
}

#define PROCESS_SPECIAL_ADDRESS(addr, endian, ptr, bits_written, \
				forbidden_addr_bits, data, retval) \
do { \
  if (addr & forbidden_addr_bits) \
    { \
      raise_buserror (); \
      return retval; \
    } \
  else if ((addr & saved_state.asregs.xyram_select) \
	   == saved_state.asregs.xram_start) \
    ptr = (void *) &saved_state.asregs.xmem_offset[addr ^ endian]; \
  else if ((addr & saved_state.asregs.xyram_select) \
	   == saved_state.asregs.yram_start) \
    ptr = (void *) &saved_state.asregs.ymem_offset[addr ^ endian]; \
  else if ((unsigned) addr >> 24 == 0xf0 \
	   && bits_written == 32 && (data & 1) == 0) \
    /* This invalidates (if not associative) or might invalidate \
       (if associative) an instruction cache line.  This is used for \
       trampolines.  Since we don't simulate the cache, this is a no-op \
       as far as the simulator is concerned.  */ \
    return retval; \
  else \
    { \
      if (bits_written == 8 && addr > 0x5000000) \
	IOMEM (addr, 1, data); \
      /* We can't do anything useful with the other stuff, so fail.  */ \
      raise_buserror (); \
      return retval; \
    } \
} while (0)

/* FIXME: sim_resume should be renamed to sim_engine_run.  sim_resume
   being implemented by ../common/sim_resume.c and the below should
   make a call to sim_engine_halt */

#define BUSERROR(addr, mask) ((addr) & (mask))

#define WRITE_BUSERROR(addr, mask, data, addr_func) \
  do \
    { \
      if (addr & mask) \
	{ \
	  addr_func (addr, data); \
	  return; \
	} \
    } \
  while (0)

#define READ_BUSERROR(addr, mask, addr_func) \
  do \
    { \
      if (addr & mask) \
	return addr_func (addr); \
    } \
  while (0)

/* Define this to enable register lifetime checking.
   The compiler generates "add #0,rn" insns to mark registers as invalid,
   the simulator uses this info to call fail if it finds a ref to an invalid
   register before a def

   #define PARANOID
*/

#ifdef PARANOID
int valid[16];
#define CREF(x)  if(!valid[x]) fail();
#define CDEF(x)  valid[x] = 1;
#define UNDEF(x) valid[x] = 0;
#else
#define CREF(x)
#define CDEF(x)
#define UNDEF(x)
#endif

static void parse_and_set_memory_size PARAMS ((char *str));
static int IOMEM PARAMS ((int addr, int write, int value));
static struct loop_bounds get_loop_bounds PARAMS((int, int, unsigned char *,
						  unsigned char *, int, int));
static void process_wlat_addr PARAMS((int, int));
static void process_wwat_addr PARAMS((int, int));
static void process_wbat_addr PARAMS((int, int));
static int process_rlat_addr PARAMS((int));
static int process_rwat_addr PARAMS((int));
static int process_rbat_addr PARAMS((int));
static void INLINE wlat_fast PARAMS ((unsigned char *, int, int, int));
static void INLINE wwat_fast PARAMS ((unsigned char *, int, int, int, int));
static void INLINE wbat_fast PARAMS ((unsigned char *, int, int, int));
static int INLINE rlat_fast PARAMS ((unsigned char *, int, int));
static int INLINE rwat_fast PARAMS ((unsigned char *, int, int, int));
static int INLINE rbat_fast PARAMS ((unsigned char *, int, int));

static host_callback *callback;



/* Floating point registers */

#define DR(n) (get_dr (n))
static double
get_dr (n)
     int n;
{
  n = (n & ~1);
  if (host_little_endian)
    {
      union
      {
	int i[2];
	double d;
      } dr;
      dr.i[1] = saved_state.asregs.fregs[0].i[n + 0];
      dr.i[0] = saved_state.asregs.fregs[0].i[n + 1];
      return dr.d;
    }
  else
    return (saved_state.asregs.fregs[0].d[n >> 1]);
}

#define SET_DR(n, EXP) set_dr ((n), (EXP))
static void
set_dr (n, exp)
     int n;
     double exp;
{
  n = (n & ~1);
  if (host_little_endian)
    {
      union
      {
	int i[2];
	double d;
      } dr;
      dr.d = exp;
      saved_state.asregs.fregs[0].i[n + 0] = dr.i[1];
      saved_state.asregs.fregs[0].i[n + 1] = dr.i[0];
    }
  else
    saved_state.asregs.fregs[0].d[n >> 1] = exp;
}

#define SET_FI(n,EXP) (saved_state.asregs.fregs[0].i[(n)] = (EXP))
#define FI(n) (saved_state.asregs.fregs[0].i[(n)])

#define FR(n) (saved_state.asregs.fregs[0].f[(n)])
#define SET_FR(n,EXP) (saved_state.asregs.fregs[0].f[(n)] = (EXP))

#define XD_TO_XF(n) ((((n) & 1) << 5) | ((n) & 0x1e))
#define XF(n) (saved_state.asregs.fregs[(n) >> 5].i[(n) & 0x1f])
#define SET_XF(n,EXP) (saved_state.asregs.fregs[(n) >> 5].i[(n) & 0x1f] = (EXP))

#define RS saved_state.asregs.cregs.named.rs
#define RE saved_state.asregs.cregs.named.re
#define MOD (saved_state.asregs.cregs.named.mod)
#define SET_MOD(i) \
(MOD = (i), \
 MOD_ME = (unsigned) MOD >> 16 | (SR_DMY ? ~0xffff : (SR_DMX ? 0 : 0x10000)), \
 MOD_DELTA = (MOD & 0xffff) - ((unsigned) MOD >> 16))

#define DSP_R(n) saved_state.asregs.sregs.i[(n)]
#define DSP_GRD(n) DSP_R ((n) + 8)
#define GET_DSP_GRD(n) ((n | 2) == 7 ? SEXT (DSP_GRD (n)) : SIGN32 (DSP_R (n)))
#define A1 DSP_R (5)
#define A0 DSP_R (7)
#define X0 DSP_R (8)
#define X1 DSP_R (9)
#define Y0 DSP_R (10)
#define Y1 DSP_R (11)
#define M0 DSP_R (12)
#define A1G DSP_R (13)
#define M1 DSP_R (14)
#define A0G DSP_R (15)
/* DSP_R (16) / DSP_GRD (16) are used as a fake destination for pcmp.  */
#define MOD_ME DSP_GRD (17)
#define MOD_DELTA DSP_GRD (18)

#define FP_OP(n, OP, m) \
{ \
  if (FPSCR_PR) \
    { \
      if (((n) & 1) || ((m) & 1)) \
	RAISE_EXCEPTION (SIGILL); \
      else \
	SET_DR(n, (DR(n) OP DR(m))); \
    } \
  else \
    SET_FR(n, (FR(n) OP FR(m))); \
} while (0)

#define FP_UNARY(n, OP) \
{ \
  if (FPSCR_PR) \
    { \
      if ((n) & 1) \
	RAISE_EXCEPTION (SIGILL); \
      else \
	SET_DR(n, (OP (DR(n)))); \
    } \
  else \
    SET_FR(n, (OP (FR(n)))); \
} while (0)

#define FP_CMP(n, OP, m) \
{ \
  if (FPSCR_PR) \
    { \
      if (((n) & 1) || ((m) & 1)) \
	RAISE_EXCEPTION (SIGILL); \
      else \
	SET_SR_T (DR(n) OP DR(m)); \
    } \
  else \
    SET_SR_T (FR(n) OP FR(m)); \
} while (0)

static void
set_sr (new_sr)
     int new_sr;
{
  /* do we need to swap banks */
  int old_gpr = SR_MD && SR_RB;
  int new_gpr = (new_sr & SR_MASK_MD) && (new_sr & SR_MASK_RB);
  if (old_gpr != new_gpr)
    {
      int i, tmp;
      for (i = 0; i < 8; i++)
	{
	  tmp = saved_state.asregs.cregs.named.bank[i];
	  saved_state.asregs.cregs.named.bank[i] = saved_state.asregs.regs[i];
	  saved_state.asregs.regs[i] = tmp;
	}
    }
  saved_state.asregs.cregs.named.sr = new_sr;
  SET_MOD (MOD);
}

static void INLINE 
wlat_fast (memory, x, value, maskl)
     unsigned char *memory;
{
  int v = value;
  unsigned int *p = (unsigned int *)(memory + x);
  WRITE_BUSERROR (x, maskl, v, process_wlat_addr);
  *p = v;
}

static void INLINE 
wwat_fast (memory, x, value, maskw, endianw)
     unsigned char *memory;
{
  int v = value;
  unsigned short *p = (unsigned short *)(memory + (x ^ endianw));
  WRITE_BUSERROR (x, maskw, v, process_wwat_addr);
  *p = v;
}

static void INLINE 
wbat_fast (memory, x, value, maskb)
     unsigned char *memory;
{
  unsigned char *p = memory + (x ^ endianb);
  WRITE_BUSERROR (x, maskb, value, process_wbat_addr);

  p[0] = value;
}

/* Read functions */

static int INLINE 
rlat_fast (memory, x, maskl)
     unsigned char *memory;
{
  unsigned int *p = (unsigned int *)(memory + x);
  READ_BUSERROR (x, maskl, process_rlat_addr);

  return *p;
}

static int INLINE 
rwat_fast (memory, x, maskw, endianw)
     unsigned char *memory;
     int x, maskw, endianw;
{
  unsigned short *p = (unsigned short *)(memory + (x ^ endianw));
  READ_BUSERROR (x, maskw, process_rwat_addr);

  return *p;
}

static int INLINE 
riat_fast (insn_ptr, endianw)
     unsigned char *insn_ptr;
{
  unsigned short *p = (unsigned short *)((size_t) insn_ptr ^ endianw);

  return *p;
}

static int INLINE 
rbat_fast (memory, x, maskb)
     unsigned char *memory;
{
  unsigned char *p = memory + (x ^ endianb);
  READ_BUSERROR (x, maskb, process_rbat_addr);

  return *p;
}

#define RWAT(x) 	(rwat_fast (memory, x, maskw, endianw))
#define RLAT(x) 	(rlat_fast (memory, x, maskl))
#define RBAT(x)         (rbat_fast (memory, x, maskb))
#define RIAT(p)		(riat_fast ((p), endianw))
#define WWAT(x,v) 	(wwat_fast (memory, x, v, maskw, endianw))
#define WLAT(x,v) 	(wlat_fast (memory, x, v, maskl))
#define WBAT(x,v)       (wbat_fast (memory, x, v, maskb))

#define RUWAT(x)  (RWAT(x) & 0xffff)
#define RSWAT(x)  ((short)(RWAT(x)))
#define RSBAT(x)  (SEXT(RBAT(x)))

#define RDAT(x, n) (do_rdat (memory, (x), (n), (maskl)))
static int
do_rdat (memory, x, n, maskl)
     char *memory;
     int x;
     int n;
     int maskl;
{
  int f0;
  int f1;
  int i = (n & 1);
  int j = (n & ~1);
  f0 = rlat_fast (memory, x + 0, maskl);
  f1 = rlat_fast (memory, x + 4, maskl);
  saved_state.asregs.fregs[i].i[(j + 0)] = f0;
  saved_state.asregs.fregs[i].i[(j + 1)] = f1;
  return 0;
}

#define WDAT(x, n) (do_wdat (memory, (x), (n), (maskl)))
static int
do_wdat (memory, x, n, maskl)
     char *memory;
     int x;
     int n;
     int maskl;
{
  int f0;
  int f1;
  int i = (n & 1);
  int j = (n & ~1);
  f0 = saved_state.asregs.fregs[i].i[(j + 0)];
  f1 = saved_state.asregs.fregs[i].i[(j + 1)];
  wlat_fast (memory, (x + 0), f0, maskl);
  wlat_fast (memory, (x + 4), f1, maskl);
  return 0;
}

static void
process_wlat_addr (addr, value)
     int addr;
     int value;
{
  unsigned int *ptr;

  PROCESS_SPECIAL_ADDRESS (addr, endianb, ptr, 32, 3, value, );
  *ptr = value;
}

static void
process_wwat_addr (addr, value)
     int addr;
     int value;
{
  unsigned short *ptr;

  PROCESS_SPECIAL_ADDRESS (addr, endianb, ptr, 16, 1, value, );
  *ptr = value;
}

static void
process_wbat_addr (addr, value)
     int addr;
     int value;
{
  unsigned char *ptr;

  PROCESS_SPECIAL_ADDRESS (addr, endianb, ptr, 8, 0, value, );
  *ptr = value;
}

static int
process_rlat_addr (addr)
     int addr;
{
  unsigned char *ptr;

  PROCESS_SPECIAL_ADDRESS (addr, endianb, ptr, -32, 3, -1, 0);
  return *ptr;
}

static int
process_rwat_addr (addr)
     int addr;
{
  unsigned char *ptr;

  PROCESS_SPECIAL_ADDRESS (addr, endianb, ptr, -16, 1, -1, 0);
  return *ptr;
}

static int
process_rbat_addr (addr)
     int addr;
{
  unsigned char *ptr;

  PROCESS_SPECIAL_ADDRESS (addr, endianb, ptr, -8, 0, -1, 0);
  return *ptr;
}

#define SEXT(x)     	(((x &  0xff) ^ (~0x7f))+0x80)
#define SEXT12(x)	(((x & 0xfff) ^ 0x800) - 0x800)
#define SEXTW(y)    	((int)((short)y))
#if 0
#define SEXT32(x)	((int)((x & 0xffffffff) ^ 0x80000000U) - 0x7fffffff - 1)
#else
#define SEXT32(x)	((int)(x))
#endif
#define SIGN32(x)	(SEXT32 (x) >> 31)

/* convert pointer from target to host value.  */
#define PT2H(x) ((x) + memory)
/* convert pointer from host to target value.  */
#define PH2T(x) ((x) - memory)

#define SKIP_INSN(p) ((p) += ((RIAT (p) & 0xfc00) == 0xf800 ? 4 : 2))

#define SET_NIP(x) nip = (x); CHECK_INSN_PTR (nip);

#define Delay_Slot(TEMPPC)  	iword = RIAT (TEMPPC); goto top;

#define CHECK_INSN_PTR(p) \
do { \
  if (saved_state.asregs.exception || PH2T (p) & maskw) \
    saved_state.asregs.insn_end = 0; \
  else if (p < loop.end) \
    saved_state.asregs.insn_end = loop.end; \
  else \
    saved_state.asregs.insn_end = mem_end; \
} while (0)

#ifdef ACE_FAST

#define MA(n)
#define L(x)
#define TL(x)
#define TB(x)

#else

#define MA(n) \
  do { memstalls += ((((int) PC & 3) != 0) ? (n) : ((n) - 1)); } while (0)

#define L(x)   thislock = x;
#define TL(x)  if ((x) == prevlock) stalls++;
#define TB(x,y)  if ((x) == prevlock || (y)==prevlock) stalls++;

#endif

#if defined(__GO32__) || defined(_WIN32)
int sim_memory_size = 19;
#else
int sim_memory_size = 24;
#endif

static int sim_profile_size = 17;
static int nsamples;

#undef TB
#define TB(x,y)

#define SMR1 (0x05FFFEC8)	/* Channel 1  serial mode register */
#define BRR1 (0x05FFFEC9)	/* Channel 1  bit rate register */
#define SCR1 (0x05FFFECA)	/* Channel 1  serial control register */
#define TDR1 (0x05FFFECB)	/* Channel 1  transmit data register */
#define SSR1 (0x05FFFECC)	/* Channel 1  serial status register */
#define RDR1 (0x05FFFECD)	/* Channel 1  receive data register */

#define SCI_RDRF  	 0x40	/* Recieve data register full */
#define SCI_TDRE	0x80	/* Transmit data register empty */

static int
IOMEM (addr, write, value)
     int addr;
     int write;
     int value;
{
  if (write)
    {
      switch (addr)
	{
	case TDR1:
	  if (value != '\r')
	    {
	      putchar (value);
	      fflush (stdout);
	    }
	  break;
	}
    }
  else
    {
      switch (addr)
	{
	case RDR1:
	  return getchar ();
	}
    }
  return 0;
}

static int
get_now ()
{
  return time ((long *) 0);
}

static int
now_persec ()
{
  return 1;
}

static FILE *profile_file;

static unsigned INLINE
swap (n)
     unsigned n;
{
  if (endianb)
    n = (n << 24 | (n & 0xff00) << 8
	 | (n & 0xff0000) >> 8 | (n & 0xff000000) >> 24);
  return n;
}

static unsigned short INLINE
swap16 (n)
     unsigned short n;
{
  if (endianb)
    n = n << 8 | (n & 0xff00) >> 8;
  return n;
}

static void
swapout (n)
     int n;
{
  if (profile_file)
    {
      union { char b[4]; int n; } u;
      u.n = swap (n);
      fwrite (u.b, 4, 1, profile_file);
    }
}

static void
swapout16 (n)
     int n;
{
  union { char b[4]; int n; } u;
  u.n = swap16 (n);
  fwrite (u.b, 2, 1, profile_file);
}

/* Turn a pointer in a register into a pointer into real memory. */

static char *
ptr (x)
     int x;
{
  return (char *) (x + saved_state.asregs.memory);
}

static int
strswaplen (str)
     int str;
{
  unsigned char *memory = saved_state.asregs.memory;
  int start, end;
  int endian = endianb;

  if (! endian)
    return 0;
  end = str;
  for (end = str; memory[end ^ endian]; end++) ;
  return end - str;
}

static void
strnswap (str, len)
     int str;
     int len;
{
  int *start, *end;

  if (! endianb || ! len)
    return;
  start = (int *) ptr (str & ~3);
  end = (int *) ptr (str + len);
  do
    {
      int old = *start;
      *start = (old << 24 | (old & 0xff00) << 8
		| (old & 0xff0000) >> 8 | (old & 0xff000000) >> 24);
      start++;
    }
  while (start < end);
}

/* Simulate a monitor trap, put the result into r0 and errno into r1 */

static void
trap (i, regs, memory, maskl, maskw, endianw)
     int i;
     int *regs;
     unsigned char *memory;
{
  switch (i)
    {
    case 1:
      printf ("%c", regs[0]);
      break;
    case 2:
      raise_exception (SIGQUIT);
      break;
    case 3:			/* FIXME: for backwards compat, should be removed */
    case 34:
      {
	extern int errno;
	int perrno = errno;
	errno = 0;

	switch (regs[4])
	  {

#if !defined(__GO32__) && !defined(_WIN32)
	  case SYS_fork:
	    regs[0] = fork ();
	    break;
/* This would work only if endianness matched between host and target.
   Besides, it's quite dangerous.  */
#if 0
	  case SYS_execve:
	    regs[0] = execve (ptr (regs[5]), (char **)ptr (regs[6]), (char **)ptr (regs[7]));
	    break;
	  case SYS_execv:
	    regs[0] = execve (ptr (regs[5]),(char **) ptr (regs[6]), 0);
	    break;
#endif
	  case SYS_pipe:
	    {
	      regs[0] = (BUSERROR (regs[5], maskl)
			 ? -EINVAL
			 : pipe ((int *) ptr (regs[5])));
	    }
	    break;

	  case SYS_wait:
	    regs[0] = wait (ptr (regs[5]));
	    break;
#endif /* !defined(__GO32__) && !defined(_WIN32) */

	  case SYS_read:
	    strnswap (regs[6], regs[7]);
	    regs[0]
	      = callback->read (callback, regs[5], ptr (regs[6]), regs[7]);
	    strnswap (regs[6], regs[7]);
	    break;
	  case SYS_write:
	    strnswap (regs[6], regs[7]);
	    if (regs[5] == 1)
	      regs[0] = (int)callback->write_stdout (callback, ptr(regs[6]), regs[7]);
	    else
	      regs[0] = (int)callback->write (callback, regs[5], ptr (regs[6]), regs[7]);
	    strnswap (regs[6], regs[7]);
	    break;
	  case SYS_lseek:
	    regs[0] = callback->lseek (callback,regs[5], regs[6], regs[7]);
	    break;
	  case SYS_close:
	    regs[0] = callback->close (callback,regs[5]);
	    break;
	  case SYS_open:
	    {
	      int len = strswaplen (regs[5]);
	      strnswap (regs[5], len);
	      regs[0] = callback->open (callback,ptr (regs[5]), regs[6]);
	      strnswap (regs[5], len);
	      break;
	    }
	  case SYS_exit:
	    /* EXIT - caller can look in r5 to work out the reason */
	    raise_exception (SIGQUIT);
	    regs[0] = regs[5];
	    break;

	  case SYS_stat:	/* added at hmsi */
	    /* stat system call */
	    {
	      struct stat host_stat;
	      int buf;
	      int len = strswaplen (regs[5]);

	      strnswap (regs[5], len);
	      regs[0] = stat (ptr (regs[5]), &host_stat);
	      strnswap (regs[5], len);

	      buf = regs[6];

	      WWAT (buf, host_stat.st_dev);
	      buf += 2;
	      WWAT (buf, host_stat.st_ino);
	      buf += 2;
	      WLAT (buf, host_stat.st_mode);
	      buf += 4;
	      WWAT (buf, host_stat.st_nlink);
	      buf += 2;
	      WWAT (buf, host_stat.st_uid);
	      buf += 2;
	      WWAT (buf, host_stat.st_gid);
	      buf += 2;
	      WWAT (buf, host_stat.st_rdev);
	      buf += 2;
	      WLAT (buf, host_stat.st_size);
	      buf += 4;
	      WLAT (buf, host_stat.st_atime);
	      buf += 4;
	      WLAT (buf, 0);
	      buf += 4;
	      WLAT (buf, host_stat.st_mtime);
	      buf += 4;
	      WLAT (buf, 0);
	      buf += 4;
	      WLAT (buf, host_stat.st_ctime);
	      buf += 4;
	      WLAT (buf, 0);
	      buf += 4;
	      WLAT (buf, 0);
	      buf += 4;
	      WLAT (buf, 0);
	      buf += 4;
	    }
	    break;

#ifndef _WIN32
	  case SYS_chown:
	    {
	      int len = strswaplen (regs[5]);

	      strnswap (regs[5], len);
	      regs[0] = chown (ptr (regs[5]), regs[6], regs[7]);
	      strnswap (regs[5], len);
	      break;
	    }
#endif /* _WIN32 */
	  case SYS_chmod:
	    {
	      int len = strswaplen (regs[5]);

	      strnswap (regs[5], len);
	      regs[0] = chmod (ptr (regs[5]), regs[6]);
	      strnswap (regs[5], len);
	      break;
	    }
	  case SYS_utime:
	    {
	      /* Cast the second argument to void *, to avoid type mismatch
		 if a prototype is present.  */
	      int len = strswaplen (regs[5]);

	      strnswap (regs[5], len);
	      regs[0] = utime (ptr (regs[5]), (void *) ptr (regs[6]));
	      strnswap (regs[5], len);
	      break;
	    }
	  case SYS_argc:
	    regs[0] = count_argc (prog_argv);
	    break;
	  case SYS_argnlen:
	    if (regs[5] < count_argc (prog_argv))
	      regs[0] = strlen (prog_argv[regs[5]]);
	    else
	      regs[0] = -1;
	    break;
	  case SYS_argn:
	    if (regs[5] < count_argc (prog_argv))
	      {
		/* Include the termination byte.  */
		int i = strlen (prog_argv[regs[5]]) + 1;
		regs[0] = sim_write (0, regs[6], prog_argv[regs[5]], i);
	      }
	    else
	      regs[0] = -1;
	    break;
	  case SYS_time:
	    regs[0] = get_now ();
	    break;
	  default:
	    regs[0] = -1;
	    break;
	  }
	regs[1] = callback->get_errno (callback);
	errno = perrno;
      }
      break;

    case 0xc3:
    case 255:
      raise_exception (SIGTRAP);
      break;
    }

}

void
control_c (sig, code, scp, addr)
     int sig;
     int code;
     char *scp;
     char *addr;
{
  raise_exception (SIGINT);
}

static int
div1 (R, iRn2, iRn1/*, T*/)
     int *R;
     int iRn1;
     int iRn2;
     /* int T;*/
{
  unsigned long tmp0;
  unsigned char old_q, tmp1;

  old_q = Q;
  SET_SR_Q ((unsigned char) ((0x80000000 & R[iRn1]) != 0));
  R[iRn1] <<= 1;
  R[iRn1] |= (unsigned long) T;

  switch (old_q)
    {
    case 0:
      switch (M)
	{
	case 0:
	  tmp0 = R[iRn1];
	  R[iRn1] -= R[iRn2];
	  tmp1 = (R[iRn1] > tmp0);
	  switch (Q)
	    {
	    case 0:
	      SET_SR_Q (tmp1);
	      break;
	    case 1:
	      SET_SR_Q ((unsigned char) (tmp1 == 0));
	      break;
	    }
	  break;
	case 1:
	  tmp0 = R[iRn1];
	  R[iRn1] += R[iRn2];
	  tmp1 = (R[iRn1] < tmp0);
	  switch (Q)
	    {
	    case 0:
	      SET_SR_Q ((unsigned char) (tmp1 == 0));
	      break;
	    case 1:
	      SET_SR_Q (tmp1);
	      break;
	    }
	  break;
	}
      break;
    case 1:
      switch (M)
	{
	case 0:
	  tmp0 = R[iRn1];
	  R[iRn1] += R[iRn2];
	  tmp1 = (R[iRn1] < tmp0);
	  switch (Q)
	    {
	    case 0:
	      SET_SR_Q (tmp1);
	      break;
	    case 1:
	      SET_SR_Q ((unsigned char) (tmp1 == 0));
	      break;
	    }
	  break;
	case 1:
	  tmp0 = R[iRn1];
	  R[iRn1] -= R[iRn2];
	  tmp1 = (R[iRn1] > tmp0);
	  switch (Q)
	    {
	    case 0:
	      SET_SR_Q ((unsigned char) (tmp1 == 0));
	      break;
	    case 1:
	      SET_SR_Q (tmp1);
	      break;
	    }
	  break;
	}
      break;
    }
  /*T = (Q == M);*/
  SET_SR_T (Q == M);
  /*return T;*/
}

static void
dmul (sign, rm, rn)
     int sign;
     unsigned int rm;
     unsigned int rn;
{
  unsigned long RnL, RnH;
  unsigned long RmL, RmH;
  unsigned long temp0, temp1, temp2, temp3;
  unsigned long Res2, Res1, Res0;

  RnL = rn & 0xffff;
  RnH = (rn >> 16) & 0xffff;
  RmL = rm & 0xffff;
  RmH = (rm >> 16) & 0xffff;
  temp0 = RmL * RnL;
  temp1 = RmH * RnL;
  temp2 = RmL * RnH;
  temp3 = RmH * RnH;
  Res2 = 0;
  Res1 = temp1 + temp2;
  if (Res1 < temp1)
    Res2 += 0x00010000;
  temp1 = (Res1 << 16) & 0xffff0000;
  Res0 = temp0 + temp1;
  if (Res0 < temp0)
    Res2 += 1;
  Res2 += ((Res1 >> 16) & 0xffff) + temp3;
  
  if (sign)
    {
      if (rn & 0x80000000)
	Res2 -= rm;
      if (rm & 0x80000000)
	Res2 -= rn;
    }

  MACH = Res2;
  MACL = Res0;
}

static void
macw (regs, memory, n, m, endianw)
     int *regs;
     unsigned char *memory;
     int m, n;
     int endianw;
{
  long tempm, tempn;
  long prod, macl, sum;

  tempm=RSWAT(regs[m]); regs[m]+=2;
  tempn=RSWAT(regs[n]); regs[n]+=2;

  macl = MACL;
  prod = (long)(short) tempm * (long)(short) tempn;
  sum = prod + macl;
  if (S)
    {
      if ((~(prod ^ macl) & (sum ^ prod)) < 0)
	{
	  /* MACH's lsb is a sticky overflow bit.  */
	  MACH |= 1;
	  /* Store the smallest negative number in MACL if prod is
	     negative, and the largest positive number otherwise.  */
	  sum = 0x7fffffff + (prod < 0);
	}
    }
  else
    {
      long mach;
      /* Add to MACH the sign extended product, and carry from low sum.  */
      mach = MACH + (-(prod < 0)) + ((unsigned long) sum < prod);
      /* Sign extend at 10:th bit in MACH.  */
      MACH = (mach & 0x1ff) | -(mach & 0x200);
    }
  MACL = sum;
}

static struct loop_bounds
get_loop_bounds (rs, re, memory, mem_end, maskw, endianw)
     int rs, re;
     unsigned char *memory, *mem_end;
     int maskw, endianw;
{
  struct loop_bounds loop;

  if (SR_RC)
    {
      if (RS >= RE)
	{
	  loop.start = PT2H (RE - 4);
	  SKIP_INSN (loop.start);
	  loop.end = loop.start;
	  if (RS - RE == 0)
	    SKIP_INSN (loop.end);
	  if (RS - RE <= 2)
	    SKIP_INSN (loop.end);
	  SKIP_INSN (loop.end);
	}
      else
	{
	  loop.start = PT2H (RS);
	  loop.end = PT2H (RE - 4);
	  SKIP_INSN (loop.end);
	  SKIP_INSN (loop.end);
	  SKIP_INSN (loop.end);
	  SKIP_INSN (loop.end);
	}
      if (loop.end >= mem_end)
	loop.end = PT2H (0);
    }
  else
    loop.end = PT2H (0);

  return loop;
}

static void
ppi_insn();

#include "ppi.c"

/* Set the memory size to the power of two provided. */

void
sim_size (power)
     int power;

{
  saved_state.asregs.msize = 1 << power;

  sim_memory_size = power;

  if (saved_state.asregs.memory)
    {
      free (saved_state.asregs.memory);
    }

  saved_state.asregs.memory =
    (unsigned char *) calloc (64, saved_state.asregs.msize / 64);

  if (!saved_state.asregs.memory)
    {
      fprintf (stderr,
	       "Not enough VM for simulation of %d bytes of RAM\n",
	       saved_state.asregs.msize);

      saved_state.asregs.msize = 1;
      saved_state.asregs.memory = (unsigned char *) calloc (1, 1);
    }
}

static void
init_dsp (abfd)
     struct _bfd *abfd;
{
  int was_dsp = target_dsp;
  unsigned long mach = bfd_get_mach (abfd);

  if (mach == bfd_mach_sh_dsp || mach == bfd_mach_sh3_dsp)
    {
      int ram_area_size, xram_start, yram_start;
      int new_select;

      target_dsp = 1;
      if (mach == bfd_mach_sh_dsp)
	{
	  /* SH7410 (orig. sh-sdp):
	     4KB each for X & Y memory;
	     On-chip X RAM 0x0800f000-0x0800ffff
	     On-chip Y RAM 0x0801f000-0x0801ffff  */
	  xram_start = 0x0800f000;
	  ram_area_size = 0x1000;
	}
      if (mach == bfd_mach_sh3_dsp)
	{
	  /* SH7612:
	     8KB each for X & Y memory;
	     On-chip X RAM 0x1000e000-0x1000ffff
	     On-chip Y RAM 0x1001e000-0x1001ffff  */
	  xram_start = 0x1000e000;
	  ram_area_size = 0x2000;
	}
      yram_start = xram_start + 0x10000;
      new_select = ~(ram_area_size - 1);
      if (saved_state.asregs.xyram_select != new_select)
	{
	  saved_state.asregs.xyram_select = new_select;
	  free (saved_state.asregs.xmem);
	  free (saved_state.asregs.ymem);
	  saved_state.asregs.xmem = (unsigned char *) calloc (1, ram_area_size);
	  saved_state.asregs.ymem = (unsigned char *) calloc (1, ram_area_size);

	  /* Disable use of X / Y mmeory if not allocated.  */
	  if (! saved_state.asregs.xmem || ! saved_state.asregs.ymem)
	    {
	      saved_state.asregs.xyram_select = 0;
	      if (saved_state.asregs.xmem)
		free (saved_state.asregs.xmem);
	      if (saved_state.asregs.ymem)
		free (saved_state.asregs.ymem);
	    }
	}
      saved_state.asregs.xram_start = xram_start;
      saved_state.asregs.yram_start = yram_start;
      saved_state.asregs.xmem_offset = saved_state.asregs.xmem - xram_start;
      saved_state.asregs.ymem_offset = saved_state.asregs.ymem - yram_start;
    }
  else
    {
      target_dsp = 0;
      if (saved_state.asregs.xyram_select)
	{
	  saved_state.asregs.xyram_select = 0;
	  free (saved_state.asregs.xmem);
	  free (saved_state.asregs.ymem);
	}
    }

  if (! saved_state.asregs.xyram_select)
    {
      saved_state.asregs.xram_start = 1;
      saved_state.asregs.yram_start = 1;
    }

  if (target_dsp != was_dsp)
    {
      int i, tmp;

      for (i = sizeof sh_dsp_table - 1; i >= 0; i--)
	{
	  tmp = sh_jump_table[0xf000 + i];
	  sh_jump_table[0xf000 + i] = sh_dsp_table[i];
	  sh_dsp_table[i] = tmp;
	}
    }
}

static void
init_pointers ()
{
  host_little_endian = 0;
  *(char*)&host_little_endian = 1;
  host_little_endian &= 1;

  if (saved_state.asregs.msize != 1 << sim_memory_size)
    {
      sim_size (sim_memory_size);
    }

  if (saved_state.asregs.profile && !profile_file)
    {
      profile_file = fopen ("gmon.out", "wb");
      /* Seek to where to put the call arc data */
      nsamples = (1 << sim_profile_size);

      fseek (profile_file, nsamples * 2 + 12, 0);

      if (!profile_file)
	{
	  fprintf (stderr, "Can't open gmon.out\n");
	}
      else
	{
	  saved_state.asregs.profile_hist =
	    (unsigned short *) calloc (64, (nsamples * sizeof (short) / 64));
	}
    }
}

static void
dump_profile ()
{
  unsigned int minpc;
  unsigned int maxpc;
  unsigned short *p;
  int i;

  p = saved_state.asregs.profile_hist;
  minpc = 0;
  maxpc = (1 << sim_profile_size);

  fseek (profile_file, 0L, 0);
  swapout (minpc << PROFILE_SHIFT);
  swapout (maxpc << PROFILE_SHIFT);
  swapout (nsamples * 2 + 12);
  for (i = 0; i < nsamples; i++)
    swapout16 (saved_state.asregs.profile_hist[i]);

}

static void
gotcall (from, to)
     int from;
     int to;
{
  swapout (from);
  swapout (to);
  swapout (1);
}

#define MMASKB ((saved_state.asregs.msize -1) & ~0)

int
sim_stop (sd)
     SIM_DESC sd;
{
  raise_exception (SIGINT);
  return 1;
}

void
sim_resume (sd, step, siggnal)
     SIM_DESC sd;
     int step, siggnal;
{
  register unsigned char *insn_ptr;
  unsigned char *mem_end;
  struct loop_bounds loop;
  register int cycles = 0;
  register int stalls = 0;
  register int memstalls = 0;
  register int insts = 0;
  register int prevlock;
  register int thislock;
  register unsigned int doprofile;
  register int pollcount = 0;
  /* endianw is used for every insn fetch, hence it makes sense to cache it.
     endianb is used less often.  */
  register int endianw = global_endianw;

  int tick_start = get_now ();
  void (*prev) ();
  void (*prev_fpe) ();

  register unsigned char *jump_table = sh_jump_table;

  register int *R = &(saved_state.asregs.regs[0]);
  /*register int T;*/
#ifndef PR
  register int PR;
#endif

  register int maskb = ~((saved_state.asregs.msize - 1) & ~0);
  register int maskw = ~((saved_state.asregs.msize - 1) & ~1);
  register int maskl = ~((saved_state.asregs.msize - 1) & ~3);
  register unsigned char *memory;
  register unsigned int sbit = ((unsigned int) 1 << 31);

  prev = signal (SIGINT, control_c);
  prev_fpe = signal (SIGFPE, SIG_IGN);

  init_pointers ();
  saved_state.asregs.exception = 0;

  memory = saved_state.asregs.memory;
  mem_end = memory + saved_state.asregs.msize;

  loop = get_loop_bounds (RS, RE, memory, mem_end, maskw, endianw);
  insn_ptr = PT2H (saved_state.asregs.pc);
  CHECK_INSN_PTR (insn_ptr);

#ifndef PR
  PR = saved_state.asregs.sregs.named.pr;
#endif
  /*T = GET_SR () & SR_MASK_T;*/
  prevlock = saved_state.asregs.prevlock;
  thislock = saved_state.asregs.thislock;
  doprofile = saved_state.asregs.profile;

  /* If profiling not enabled, disable it by asking for
     profiles infrequently. */
  if (doprofile == 0)
    doprofile = ~0;

 loop:
  if (step && insn_ptr < saved_state.asregs.insn_end)
    {
      if (saved_state.asregs.exception)
	/* This can happen if we've already been single-stepping and
	   encountered a loop end.  */
	saved_state.asregs.insn_end = insn_ptr;
      else
	{
	  saved_state.asregs.exception = SIGTRAP;
	  saved_state.asregs.insn_end = insn_ptr + 2;
	}
    }

  while (insn_ptr < saved_state.asregs.insn_end)
    {
      register unsigned int iword = RIAT (insn_ptr);
      register unsigned int ult;
      register unsigned char *nip = insn_ptr + 2;

#ifndef ACE_FAST
      insts++;
#endif
    top:

#include "code.c"


      insn_ptr = nip;

      if (--pollcount < 0)
	{
	  pollcount = POLL_QUIT_INTERVAL;
	  if ((*callback->poll_quit) != NULL
	      && (*callback->poll_quit) (callback))
	    {
	      sim_stop (sd);
	    }	    
	}

#ifndef ACE_FAST
      prevlock = thislock;
      thislock = 30;
      cycles++;

      if (cycles >= doprofile)
	{

	  saved_state.asregs.cycles += doprofile;
	  cycles -= doprofile;
	  if (saved_state.asregs.profile_hist)
	    {
	      int n = PH2T (insn_ptr) >> PROFILE_SHIFT;
	      if (n < nsamples)
		{
		  int i = saved_state.asregs.profile_hist[n];
		  if (i < 65000)
		    saved_state.asregs.profile_hist[n] = i + 1;
		}

	    }
	}
#endif
    }
  if (saved_state.asregs.insn_end == loop.end)
    {
      saved_state.asregs.cregs.named.sr += SR_RC_INCREMENT;
      if (SR_RC)
	insn_ptr = loop.start;
      else
	{
	  saved_state.asregs.insn_end = mem_end;
	  loop.end = PT2H (0);
	}
      goto loop;
    }

  if (saved_state.asregs.exception == SIGILL
      || saved_state.asregs.exception == SIGBUS)
    {
      insn_ptr -= 2;
    }
  /* Check for SIGBUS due to insn fetch.  */
  else if (! saved_state.asregs.exception)
    saved_state.asregs.exception == SIGBUS;

  saved_state.asregs.ticks += get_now () - tick_start;
  saved_state.asregs.cycles += cycles;
  saved_state.asregs.stalls += stalls;
  saved_state.asregs.memstalls += memstalls;
  saved_state.asregs.insts += insts;
  saved_state.asregs.pc = PH2T (insn_ptr);
#ifndef PR
  saved_state.asregs.sregs.named.pr = PR;
#endif

  saved_state.asregs.prevlock = prevlock;
  saved_state.asregs.thislock = thislock;

  if (profile_file)
    {
      dump_profile ();
    }

  signal (SIGFPE, prev_fpe);
  signal (SIGINT, prev);
}

int
sim_write (sd, addr, buffer, size)
     SIM_DESC sd;
     SIM_ADDR addr;
     unsigned char *buffer;
     int size;
{
  int i;

  init_pointers ();

  for (i = 0; i < size; i++)
    {
      saved_state.asregs.memory[(MMASKB & (addr + i)) ^ endianb] = buffer[i];
    }
  return size;
}

int
sim_read (sd, addr, buffer, size)
     SIM_DESC sd;
     SIM_ADDR addr;
     unsigned char *buffer;
     int size;
{
  int i;

  init_pointers ();

  for (i = 0; i < size; i++)
    {
      buffer[i] = saved_state.asregs.memory[(MMASKB & (addr + i)) ^ endianb];
    }
  return size;
}

int
sim_store_register (sd, rn, memory, length)
     SIM_DESC sd;
     int rn;
     unsigned char *memory;
     int length;
{
  unsigned val;

  init_pointers ();
  val = swap (* (int *)memory);
  switch (rn)
    {
    case  0: case  1: case  2: case  3: case  4: case  5: case  6: case  7:
    case  8: case  9: case 10: case 11: case 12: case 13: case 14: case 15:
      saved_state.asregs.regs[rn] = val;
      break;
    case 16:
      saved_state.asregs.pc = val;
      break;
    case 17:
      PR = val;
      break;
    case 18:
      GBR = val;
      break;
    case 19:
      VBR = val;
      break;
    case 20:
      MACH = val;
      break;
    case 21:
      MACL = val;
      break;
    case 22:
      SET_SR (val);
      break;
    case 23:
      FPUL = val;
      break;
    case 24:
      SET_FPSCR (val);
      break;
    case 25:
      if (target_dsp)
	A0G = val;
    else case 26:
      if (target_dsp)
	A0 = val;
    else case 27:
      if (target_dsp)
	A1G = val;
    else case 28:
      if (target_dsp)
	A1 = val;
    else case 29:
      if (target_dsp)
	M0 = val;
    else case 30:
      if (target_dsp)
	M1 = val;
    else case 31:
      if (target_dsp)
	X0 = val;
    else case 32:
      if (target_dsp)
	X1 = val;
    else case 33:
      if (target_dsp)
	Y0 = val;
    else case 34:
      if (target_dsp)
	Y1 = val;
    else case 40:
      if (target_dsp)
	SET_MOD (val);
    else case 35: case 36: case 37: case 38: case 39:
	SET_FI (rn - 25, val);
      break;
    case 41:
      SSR = val;
      break;
    case 42:
      SPC = val;
      break;
    /* The rn_bank idiosyncracies are not due to hardware differences, but to
       a weird aliasing naming scheme for sh3 / sh3e / sh4.  */
    case 43:
      if (target_dsp)
	RS = val;
    else case 44:
      if (target_dsp)
	RE = val;
    else case 45: case 46: case 47: case 48: case 49: case 50:
      if (SR_MD && SR_RB)
	Rn_BANK (rn - 43) = val;
      else
	saved_state.asregs.regs[rn - 43] = val;
      break;
    case 51: case 52: case 53: case 54: case 55: case 56: case 57: case 58:
      if (target_dsp || ! SR_MD || ! SR_RB)
	SET_Rn_BANK (rn - 51, val);
      else
	saved_state.asregs.regs[rn - 51] = val;
      break;
    default:
      return 0;
    }
  return -1;
}

int
sim_fetch_register (sd, rn, memory, length)
     SIM_DESC sd;
     int rn;
     unsigned char *memory;
     int length;
{
  int val;

  init_pointers ();
  switch (rn)
    {
    case  0: case  1: case  2: case  3: case  4: case  5: case  6: case  7:
    case  8: case  9: case 10: case 11: case 12: case 13: case 14: case 15:
      val = saved_state.asregs.regs[rn];
      break;
    case 16:
      val = saved_state.asregs.pc;
      break;
    case 17:
      val = PR;
      break;
    case 18:
      val = GBR;
      break;
    case 19:
      val = VBR;
      break;
    case 20:
      val = MACH;
      break;
    case 21:
      val = MACL;
      break;
    case 22:
      val = GET_SR ();
      break;
    case 23:
      val = FPUL;
      break;
    case 24:
      val = GET_FPSCR ();
      break;
    case 25:
      val = target_dsp ? SEXT (A0G) : FI (0);
      break;
    case 26:
      val = target_dsp ? A0 : FI (1);
      break;
    case 27:
      val = target_dsp ? SEXT (A1G) : FI (2);
      break;
    case 28:
      val = target_dsp ? A1 : FI (3);
      break;
    case 29:
      val = target_dsp ? M0 : FI (4);
      break;
    case 30:
      val = target_dsp ? M1 : FI (5);
      break;
    case 31:
      val = target_dsp ? X0 : FI (6);
      break;
    case 32:
      val = target_dsp ? X1 : FI (7);
      break;
    case 33:
      val = target_dsp ? Y0 : FI (8);
      break;
    case 34:
      val = target_dsp ? Y1 : FI (9);
      break;
    case 35: case 36: case 37: case 38: case 39:
      val = FI (rn - 25);
      break;
    case 40:
      val = target_dsp ? MOD : FI (15);
      break;
    case 41:
      val = SSR;
      break;
    case 42:
      val = SPC;
      break;
    /* The rn_bank idiosyncracies are not due to hardware differences, but to
       a weird aliasing naming scheme for sh3 / sh3e / sh4.  */
    case 43:
      if (target_dsp)
	val = RS;
    else case 44:
      if (target_dsp)
	val = RE;
    else case 45: case 46: case 47: case 48: case 49: case 50:
	val = (SR_MD && SR_RB
	       ? Rn_BANK (rn - 43)
	       : saved_state.asregs.regs[rn - 43]);
      break;
    case 51: case 52: case 53: case 54: case 55: case 56: case 57: case 58:
      val = (target_dsp || ! SR_MD || ! SR_RB
	     ? Rn_BANK (rn - 51)
	     : saved_state.asregs.regs[rn - 51]);
      break;
    default:
      return 0;
    }
  * (int *) memory = swap (val);
  return -1;
}

int
sim_trace (sd)
     SIM_DESC sd;
{
  return 0;
}

void
sim_stop_reason (sd, reason, sigrc)
     SIM_DESC sd;
     enum sim_stop *reason;
     int *sigrc;
{
  /* The SH simulator uses SIGQUIT to indicate that the program has
     exited, so we must check for it here and translate it to exit.  */
  if (saved_state.asregs.exception == SIGQUIT)
    {
      *reason = sim_exited;
      *sigrc = saved_state.asregs.regs[5];
    }
  else
    {
      *reason = sim_stopped;
      *sigrc = saved_state.asregs.exception;
    }
}

void
sim_info (sd, verbose)
     SIM_DESC sd;
     int verbose;
{
  double timetaken = (double) saved_state.asregs.ticks / (double) now_persec ();
  double virttime = saved_state.asregs.cycles / 36.0e6;

  callback->printf_filtered (callback, "\n\n# instructions executed  %10d\n", 
			     saved_state.asregs.insts);
  callback->printf_filtered (callback, "# cycles                 %10d\n",
			     saved_state.asregs.cycles);
  callback->printf_filtered (callback, "# pipeline stalls        %10d\n",
			     saved_state.asregs.stalls);
  callback->printf_filtered (callback, "# misaligned load/store  %10d\n",
			     saved_state.asregs.memstalls);
  callback->printf_filtered (callback, "# real time taken        %10.4f\n",
			     timetaken);
  callback->printf_filtered (callback, "# virtual time taken     %10.4f\n",
			     virttime);
  callback->printf_filtered (callback, "# profiling size         %10d\n",
			     sim_profile_size);
  callback->printf_filtered (callback, "# profiling frequency    %10d\n",
			     saved_state.asregs.profile);
  callback->printf_filtered (callback, "# profile maxpc          %10x\n",
			     (1 << sim_profile_size) << PROFILE_SHIFT);

  if (timetaken != 0)
    {
      callback->printf_filtered (callback, "# cycles/second          %10d\n", 
				 (int) (saved_state.asregs.cycles / timetaken));
      callback->printf_filtered (callback, "# simulation ratio       %10.4f\n", 
				 virttime / timetaken);
    }
}

void
sim_set_profile (n)
     int n;
{
  saved_state.asregs.profile = n;
}

void
sim_set_profile_size (n)
     int n;
{
  sim_profile_size = n;
}

SIM_DESC
sim_open (kind, cb, abfd, argv)
     SIM_OPEN_KIND kind;
     host_callback *cb;
     struct _bfd *abfd;
     char **argv;
{
  char **p;
  int endian_set = 0;
  int i;
  union
    {
      int i;
      short s[2];
      char c[4];
    }
  mem_word;

  sim_kind = kind;
  myname = argv[0];
  callback = cb;

  for (p = argv + 1; *p != NULL; ++p)
    {
      if (strcmp (*p, "-E") == 0)
	{
	  ++p;
	  if (*p == NULL)
	    {
	      /* FIXME: This doesn't use stderr, but then the rest of the
		 file doesn't either.  */
	      callback->printf_filtered (callback, "Missing argument to `-E'.\n");
	      return 0;
	    }
	  target_little_endian = strcmp (*p, "big") != 0;
          endian_set = 1;
	}
      else if (isdigit (**p))
	parse_and_set_memory_size (*p);
    }

  if (abfd != NULL && ! endian_set)
      target_little_endian = ! bfd_big_endian (abfd);

  if (abfd)
    init_dsp (abfd);

  for (i = 4; (i -= 2) >= 0; )
    mem_word.s[i >> 1] = i;
  global_endianw = mem_word.i >> (target_little_endian ? 0 : 16) & 0xffff;

  for (i = 4; --i >= 0; )
    mem_word.c[i] = i;
  endianb = mem_word.i >> (target_little_endian ? 0 : 24) & 0xff;

  /* fudge our descriptor for now */
  return (SIM_DESC) 1;
}

static void
parse_and_set_memory_size (str)
     char *str;
{
  int n;

  n = strtol (str, NULL, 10);
  if (n > 0 && n <= 24)
    sim_memory_size = n;
  else
    callback->printf_filtered (callback, "Bad memory size %d; must be 1 to 24, inclusive\n", n);
}

void
sim_close (sd, quitting)
     SIM_DESC sd;
     int quitting;
{
  /* nothing to do */
}

SIM_RC
sim_load (sd, prog, abfd, from_tty)
     SIM_DESC sd;
     char *prog;
     bfd *abfd;
     int from_tty;
{
  extern bfd *sim_load_file (); /* ??? Don't know where this should live.  */
  bfd *prog_bfd;

  prog_bfd = sim_load_file (sd, myname, callback, prog, abfd,
			    sim_kind == SIM_OPEN_DEBUG,
			    0, sim_write);
  if (prog_bfd == NULL)
    return SIM_RC_FAIL;
  if (abfd == NULL)
    bfd_close (prog_bfd);
  return SIM_RC_OK;
}

SIM_RC
sim_create_inferior (sd, prog_bfd, argv, env)
     SIM_DESC sd;
     struct _bfd *prog_bfd;
     char **argv;
     char **env;
{
  /* Clear the registers. */
  memset (&saved_state, 0,
	  (char*)&saved_state.asregs.end_of_registers - (char*)&saved_state);

  /* Set the PC.  */
  if (prog_bfd != NULL)
    saved_state.asregs.pc = bfd_get_start_address (prog_bfd);

  /* Record the program's arguments. */
  prog_argv = argv;

  return SIM_RC_OK;
}

void
sim_do_command (sd, cmd)
     SIM_DESC sd;
     char *cmd;
{
  char *sms_cmd = "set-memory-size";
  int cmdsize;

  if (cmd == NULL || *cmd == '\0')
    {
      cmd = "help";
    }

  cmdsize = strlen (sms_cmd);
  if (strncmp (cmd, sms_cmd, cmdsize) == 0 && strchr (" \t", cmd[cmdsize]) != NULL)
    {
      parse_and_set_memory_size (cmd + cmdsize + 1);
    }
  else if (strcmp (cmd, "help") == 0)
    {
      (callback->printf_filtered) (callback, "List of SH simulator commands:\n\n");
      (callback->printf_filtered) (callback, "set-memory-size <n> -- Set the number of address bits to use\n");
      (callback->printf_filtered) (callback, "\n");
    }
  else
    {
      (callback->printf_filtered) (callback, "Error: \"%s\" is not a valid SH simulator command.\n", cmd);
    }
}

void
sim_set_callbacks (p)
     host_callback *p;
{
  callback = p;
}
