#include <stdio.h>
#include <ctype.h>
#include "ansidecl.h"
#include "callback.h"
#include "opcode/mn10300.h"
#include <limits.h>
#include "remote-sim.h"

#ifndef INLINE
#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif
#endif

extern host_callback *mn10300_callback;

#define DEBUG_TRACE		0x00000001
#define DEBUG_VALUES		0x00000002

extern int mn10300_debug;

#if UCHAR_MAX == 255
typedef unsigned char uint8;
typedef signed char int8;
#else
#error "Char is not an 8-bit type"
#endif

#if SHRT_MAX == 32767
typedef unsigned short uint16;
typedef signed short int16;
#else
#error "Short is not a 16-bit type"
#endif

#if INT_MAX == 2147483647

typedef unsigned int uint32;
typedef signed int int32;

#else
#  if LONG_MAX == 2147483647

typedef unsigned long uint32;
typedef signed long int32;

#  else
#  error "Neither int nor long is a 32-bit type"
#  endif
#endif

typedef uint32 reg_t;

struct simops 
{
  long opcode;
  long mask;
  void (*func)();
  int length;
  int format;
  int numops;
  int operands[16];
};

/* The current state of the processor; registers, memory, etc.  */

struct _state
{
  reg_t regs[15];		/* registers, d0-d3, a0-a3, sp, pc, mdr, psw,
				   lir, lar, mdrq */
  uint8 *mem;			/* main memory */
  int exception;
  int exited;
} State;

extern uint32 OP[4];
extern struct simops Simops[];

#define PC	(State.regs[9])

#define PSW (State.regs[11])
#define PSW_Z 0x1
#define PSW_N 0x2
#define PSW_C 0x4
#define PSW_V 0x8

#define REG_D0 0
#define REG_A0 4
#define REG_SP 8
#define REG_PC 9
#define REG_MDR 10
#define REG_PSW 11
#define REG_LIR 12
#define REG_LAR 13
#define REG_MDRQ 14

#define SEXT3(x)	((((x)&0x7)^(~0x3))+0x4)	

/* sign-extend a 4-bit number */
#define SEXT4(x)	((((x)&0xf)^(~0x7))+0x8)	

/* sign-extend a 5-bit number */
#define SEXT5(x)	((((x)&0x1f)^(~0xf))+0x10)	

/* sign-extend an 8-bit number */
#define SEXT8(x)	((((x)&0xff)^(~0x7f))+0x80)

/* sign-extend a 9-bit number */
#define SEXT9(x)	((((x)&0x1ff)^(~0xff))+0x100)

/* sign-extend a 16-bit number */
#define SEXT16(x)	((((x)&0xffff)^(~0x7fff))+0x8000)

/* sign-extend a 22-bit number */
#define SEXT22(x)	((((x)&0x3fffff)^(~0x1fffff))+0x200000)

#define MAX32	0x7fffffffLL
#define MIN32	0xff80000000LL
#define MASK32	0xffffffffLL
#define MASK40	0xffffffffffLL

#ifdef _WIN32
#define SIGTRAP 5
#define SIGQUIT 3
#endif

#define load_mem_big(addr,len) \
  (len == 1 ? *((addr) + State.mem) : \
   len == 2 ? ((*((addr) + State.mem) << 8) \
	       | *(((addr) + 1) + State.mem)) : \
   len == 3 ? ((*((addr) + State.mem) << 16) \
	       | (*(((addr) + 1) + State.mem) << 8) \
	       | *(((addr) + 2) + State.mem)) : \
	      ((*((addr) + State.mem) << 24) \
	       | (*(((addr) + 1) + State.mem) << 16) \
	       | (*(((addr) + 2) + State.mem) << 8) \
	       | *(((addr) + 3) + State.mem)))

static INLINE uint32
load_byte (addr)
     SIM_ADDR addr;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  return p[0];
}

static INLINE uint32
load_half (addr)
     SIM_ADDR addr;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  return p[1] << 8 | p[0];
}

static INLINE uint32
load_3_byte (addr)
     SIM_ADDR addr;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  return p[2] << 16 | p[1] << 8 | p[0];
}

static INLINE uint32
load_word (addr)
     SIM_ADDR addr;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  return p[3] << 24 | p[2] << 16 | p[1] << 8 | p[0];
}

static INLINE uint32
load_mem (addr, len)
     SIM_ADDR addr;
     int len;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  switch (len)
    {
    case 1:
      return p[0];
    case 2:
      return p[1] << 8 | p[0];
    case 3:
      return p[2] << 16 | p[1] << 8 | p[0];
    case 4:
      return p[3] << 24 | p[2] << 16 | p[1] << 8 | p[0];
    default:
      abort ();
    }
}

static INLINE void
store_byte (addr, data)
     SIM_ADDR addr;
     uint32 data;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  p[0] = data;
}

static INLINE void
store_half (addr, data)
     SIM_ADDR addr;
     uint32 data;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  p[0] = data;
  p[1] = data >> 8;
}

static INLINE void
store_3_byte (addr, data)
     SIM_ADDR addr;
     uint32 data;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  p[0] = data;
  p[1] = data >> 8;
  p[2] = data >> 16;
}

static INLINE void
store_word (addr, data)
     SIM_ADDR addr;
     uint32 data;
{
  uint8 *p = (addr & 0xffffff) + State.mem;

#ifdef CHECK_ADDR
  if ((addr & 0xffffff) > max_mem)
    abort ();
#endif

  p[0] = data;
  p[1] = data >> 8;
  p[2] = data >> 16;
  p[3] = data >> 24;
}

/* Function declarations.  */

uint32 get_word PARAMS ((uint8 *));
uint16 get_half PARAMS ((uint8 *));
uint8 get_byte PARAMS ((uint8 *));
void put_word PARAMS ((uint8 *, uint32));
void put_half PARAMS ((uint8 *, uint16));
void put_byte PARAMS ((uint8 *, uint8));

extern uint8 *map PARAMS ((SIM_ADDR addr));
