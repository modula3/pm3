#include "sim-main.h"
#include "v850_sim.h"
#include "simops.h"

#ifdef HAVE_UTIME_H
#include <utime.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#else
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#endif





 /* FIXME - should be including a version of syscall.h that does not
    pollute the name space */
#include "../../libgloss/v850/sys/syscall.h"

#include "libiberty.h"

#include <errno.h>
#if !defined(__GO32__) && !defined(_WIN32)
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/time.h>
#endif


#ifdef DEBUG
#ifndef SIZE_INSTRUCTION
#define SIZE_INSTRUCTION 18
#endif

#ifndef SIZE_VALUES
#define SIZE_VALUES 11
#endif


unsigned32 trace_values[3];
int trace_num_values;
unsigned32 trace_pc;
const char *trace_name;
const char *trace_module;


void
trace_input (name, type, size)
     char *name;
     enum op_types type;
     int size;
{

  if (!TRACE_ALU_P (STATE_CPU (simulator, 0)))
    return;

  trace_pc = PC;
  trace_name = name;
  trace_module = "alu";

  switch (type)
    {
    default:
    case OP_UNKNOWN:
    case OP_NONE:
    case OP_TRAP:
      trace_num_values = 0;
      break;
      
    case OP_REG:
    case OP_REG_REG_MOVE:
      trace_values[0] = State.regs[OP[0]];
      trace_num_values = 1;
      break;
      
    case OP_REG_REG:
    case OP_REG_REG_CMP:
      trace_values[0] = State.regs[OP[1]];
      trace_values[1] = State.regs[OP[0]];
      trace_num_values = 2;
      break;
      
    case OP_IMM_REG:
    case OP_IMM_REG_CMP:
      trace_values[0] = SEXT5 (OP[0]);
      trace_values[1] = OP[1];
      trace_num_values = 2;
      break;
      
    case OP_IMM_REG_MOVE:
      trace_values[0] = SEXT5 (OP[0]);
      trace_num_values = 1;
      break;
      
    case OP_COND_BR:
      trace_values[0] = State.pc;
      trace_values[1] = SEXT9 (OP[0]);
      trace_values[2] = PSW;
      trace_num_values = 3;
      break;
      
    case OP_LOAD16:
      trace_values[0] = OP[1] * size;
      trace_values[1] = State.regs[30];
      trace_num_values = 2;
      break;
      
    case OP_STORE16:
      trace_values[0] = State.regs[OP[0]];
      trace_values[1] = OP[1] * size;
      trace_values[2] = State.regs[30];
      trace_num_values = 3;
      break;
      
    case OP_LOAD32:
      trace_values[0] = EXTEND16 (OP[2]);
      trace_values[1] = State.regs[OP[0]];
      trace_num_values = 2;
      break;
      
    case OP_STORE32:
      trace_values[0] = State.regs[OP[1]];
      trace_values[1] = EXTEND16 (OP[2]);
      trace_values[2] = State.regs[OP[0]];
      trace_num_values = 3;
      break;
      
    case OP_JUMP:
      trace_values[0] = SEXT22 (OP[0]);
      trace_values[1] = State.pc;
      trace_num_values = 2;
      break;
      
    case OP_IMM_REG_REG:
      trace_values[0] = EXTEND16 (OP[0]) << size;
      trace_values[1] = State.regs[OP[1]];
      trace_num_values = 2;
      break;
      
    case OP_IMM16_REG_REG:
      trace_values[0] = EXTEND16 (OP[2]) << size;
      trace_values[1] = State.regs[OP[1]];
      trace_num_values = 2;
      break;
      
    case OP_UIMM_REG_REG:
      trace_values[0] = (OP[0] & 0xffff) << size;
      trace_values[1] = State.regs[OP[1]];
      trace_num_values = 2;
      break;
      
    case OP_UIMM16_REG_REG:
      trace_values[0] = (OP[2]) << size;
      trace_values[1] = State.regs[OP[1]];
      trace_num_values = 2;
      break;
      
    case OP_BIT:
      trace_num_values = 0;
      break;
      
    case OP_EX1:
      trace_values[0] = PSW;
      trace_num_values = 1;
      break;
      
    case OP_EX2:
      trace_num_values = 0;
      break;
      
    case OP_LDSR:
      trace_values[0] = State.regs[OP[0]];
      trace_num_values = 1;
      break;
      
    case OP_STSR:
      trace_values[0] = State.sregs[OP[1]];
      trace_num_values = 1;
    }
  
}

void
trace_result (int has_result, unsigned32 result)
{
  char buf[1000];
  char *chp;

  buf[0] = '\0';
  chp = buf;

  /* write out the values saved during the trace_input call */
  {
    int i;
    for (i = 0; i < trace_num_values; i++)
      {
	sprintf (chp, "%*s0x%.8lx", SIZE_VALUES - 10, "", trace_values[i]);
	chp = strchr (chp, '\0');
      }
    while (i++ < 3)
      {
	sprintf (chp, "%*s", SIZE_VALUES, "");
	chp = strchr (chp, '\0');
      }
  }

  /* append any result to the end of the buffer */
  if (has_result)
    sprintf (chp, " :: 0x%.8lx", (unsigned long)result);
  
  trace_one_insn (simulator, STATE_CPU (simulator, 0), trace_pc,
		  TRACE_LINENUM_P (STATE_CPU (simulator, 0)),
		  "simops", __LINE__, trace_module, 
		  "%-*s -%s", SIZE_INSTRUCTION, trace_name, buf);
}

void
trace_output (result)
     enum op_types result;
{
  if (!TRACE_ALU_P (STATE_CPU (simulator, 0)))
    return;

  switch (result)
    {
    default:
    case OP_UNKNOWN:
    case OP_NONE:
    case OP_TRAP:
    case OP_REG:
    case OP_REG_REG_CMP:
    case OP_IMM_REG_CMP:
    case OP_COND_BR:
    case OP_STORE16:
    case OP_STORE32:
    case OP_BIT:
    case OP_EX2:
      trace_result (0, 0);
      break;
      
    case OP_LOAD16:
    case OP_STSR:
      trace_result (1, State.regs[OP[0]]);
      break;
      
    case OP_REG_REG:
    case OP_REG_REG_MOVE:
    case OP_IMM_REG:
    case OP_IMM_REG_MOVE:
    case OP_LOAD32:
    case OP_EX1:
      trace_result (1, State.regs[OP[1]]);
      break;
      
    case OP_IMM_REG_REG:
    case OP_UIMM_REG_REG:
    case OP_IMM16_REG_REG:
    case OP_UIMM16_REG_REG:
      trace_result (1, State.regs[OP[1]]);
      break;
      
    case OP_JUMP:
      if (OP[1] != 0)
	trace_result (1, State.regs[OP[1]]);
      else
	trace_result (0, 0);
      break;
      
    case OP_LDSR:
      trace_result (1, State.sregs[OP[1]]);
      break;
    }
}
#endif


/* Returns 1 if the specific condition is met, returns 0 otherwise.  */
int
condition_met (unsigned code)
{
  unsigned int psw = PSW;

  switch (code & 0xf)
    {
      case 0x0: return ((psw & PSW_OV) != 0); 
      case 0x1:	return ((psw & PSW_CY) != 0);
      case 0x2:	return ((psw & PSW_Z) != 0);
      case 0x3:	return ((((psw & PSW_CY) != 0) | ((psw & PSW_Z) != 0)) != 0);
      case 0x4:	return ((psw & PSW_S) != 0);
    /*case 0x5:	return 1;*/
      case 0x6: return ((((psw & PSW_S) != 0) ^ ((psw & PSW_OV) != 0)) != 0);
      case 0x7:	return (((((psw & PSW_S) != 0) ^ ((psw & PSW_OV) != 0)) || ((psw & PSW_Z) != 0)) != 0);
      case 0x8:	return ((psw & PSW_OV) == 0);
      case 0x9:	return ((psw & PSW_CY) == 0);
      case 0xa:	return ((psw & PSW_Z) == 0);
      case 0xb:	return ((((psw & PSW_CY) != 0) | ((psw & PSW_Z) != 0)) == 0);
      case 0xc:	return ((psw & PSW_S) == 0);
      case 0xd:	return ((psw & PSW_SAT) != 0);
      case 0xe:	return ((((psw & PSW_S) != 0) ^ ((psw & PSW_OV) != 0)) == 0);
      case 0xf:	return (((((psw & PSW_S) != 0) ^ ((psw & PSW_OV) != 0)) || ((psw & PSW_Z) != 0)) == 0);
    }
  
  return 1;
}

/* Read a null terminated string from memory, return in a buffer */
static char *
fetch_str (sd, addr)
     SIM_DESC sd;
     address_word addr;
{
  char *buf;
  int nr = 0;
  while (sim_core_read_1 (STATE_CPU (sd, 0),
			  PC, sim_core_read_map, addr + nr) != 0)
    nr++;
  buf = NZALLOC (char, nr + 1);
  sim_read (simulator, addr, buf, nr);
  return buf;
}

/* Read a null terminated argument vector from memory, return in a
   buffer */
static char **
fetch_argv (sd, addr)
     SIM_DESC sd;
     address_word addr;
{
  int max_nr = 64;
  int nr = 0;
  char **buf = xmalloc (max_nr * sizeof (char*));
  while (1)
    {
      unsigned32 a = sim_core_read_4 (STATE_CPU (sd, 0),
				      PC, sim_core_read_map, addr + nr * 4);
      if (a == 0) break;
      buf[nr] = fetch_str (sd, a);
      nr ++;
      if (nr == max_nr - 1)
	{
	  max_nr += 50;
	  buf = xrealloc (buf, max_nr * sizeof (char*));
	}
    }
  buf[nr] = 0;
  return buf;
}


/* sst.b */
int
OP_380 ()
{
  trace_input ("sst.b", OP_STORE16, 1);

  store_mem (State.regs[30] + (OP[3] & 0x7f), 1, State.regs[ OP[1] ]);
  
  trace_output (OP_STORE16);

  return 2;
}

/* sst.h */
int
OP_480 ()
{
  trace_input ("sst.h", OP_STORE16, 2);

  store_mem (State.regs[30] + ((OP[3] & 0x7f) << 1), 2, State.regs[ OP[1] ]);
  
  trace_output (OP_STORE16);

  return 2;
}

/* sst.w */
int
OP_501 ()
{
  trace_input ("sst.w", OP_STORE16, 4);

  store_mem (State.regs[30] + ((OP[3] & 0x7e) << 1), 4, State.regs[ OP[1] ]);
  
  trace_output (OP_STORE16);

  return 2;
}

/* ld.b */
int
OP_700 ()
{
  int adr;

  trace_input ("ld.b", OP_LOAD32, 1);

  adr = State.regs[ OP[0] ] + EXTEND16 (OP[2]);

  State.regs[ OP[1] ] = EXTEND8 (load_mem (adr, 1));
  
  trace_output (OP_LOAD32);

  return 4;
}

/* ld.h */
int
OP_720 ()
{
  int adr;

  trace_input ("ld.h", OP_LOAD32, 2);

  adr = State.regs[ OP[0] ] + EXTEND16 (OP[2]);
  adr &= ~0x1;
  
  State.regs[ OP[1] ] = EXTEND16 (load_mem (adr, 2));
  
  trace_output (OP_LOAD32);

  return 4;
}

/* ld.w */
int
OP_10720 ()
{
  int adr;

  trace_input ("ld.w", OP_LOAD32, 4);

  adr = State.regs[ OP[0] ] + EXTEND16 (OP[2] & ~1);
  adr &= ~0x3;
  
  State.regs[ OP[1] ] = load_mem (adr, 4);
  
  trace_output (OP_LOAD32);

  return 4;
}

/* st.b */
int
OP_740 ()
{
  trace_input ("st.b", OP_STORE32, 1);

  store_mem (State.regs[ OP[0] ] + EXTEND16 (OP[2]), 1, State.regs[ OP[1] ]);
  
  trace_output (OP_STORE32);

  return 4;
}

/* st.h */
int
OP_760 ()
{
  int adr;
  
  trace_input ("st.h", OP_STORE32, 2);

  adr = State.regs[ OP[0] ] + EXTEND16 (OP[2]);
  adr &= ~1;
  
  store_mem (adr, 2, State.regs[ OP[1] ]);
  
  trace_output (OP_STORE32);

  return 4;
}

/* st.w */
int
OP_10760 ()
{
  int adr;
  
  trace_input ("st.w", OP_STORE32, 4);

  adr = State.regs[ OP[0] ] + EXTEND16 (OP[2] & ~1);
  adr &= ~3;
  
  store_mem (adr, 4, State.regs[ OP[1] ]);
  
  trace_output (OP_STORE32);

  return 4;
}

/* add reg, reg */
int
OP_1C0 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;

  trace_input ("add", OP_REG_REG, 0);
  
  /* Compute the result.  */
  
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  
  result = op0 + op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (result < op0 || result < op1);
  ov = ((op0 & 0x80000000) == (op1 & 0x80000000)
	&& (op0 & 0x80000000) != (result & 0x80000000));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		     | (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* add sign_extend(imm5), reg */
int
OP_240 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;
  int temp;

  trace_input ("add", OP_IMM_REG, 0);

  /* Compute the result.  */
  temp = SEXT5 (OP[0]);
  op0 = temp;
  op1 = State.regs[OP[1]];
  result = op0 + op1;
  
  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (result < op0 || result < op1);
  ov = ((op0 & 0x80000000) == (op1 & 0x80000000)
	&& (op0 & 0x80000000) != (result & 0x80000000));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_IMM_REG);

  return 2;
}

/* addi sign_extend(imm16), reg, reg */
int
OP_600 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;

  trace_input ("addi", OP_IMM16_REG_REG, 0);

  /* Compute the result.  */

  op0 = EXTEND16 (OP[2]);
  op1 = State.regs[ OP[0] ];
  result = op0 + op1;
  
  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (result < op0 || result < op1);
  ov = ((op0 & 0x80000000) == (op1 & 0x80000000)
	&& (op0 & 0x80000000) != (result & 0x80000000));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_IMM16_REG_REG);

  return 4;
}

/* sub reg1, reg2 */
int
OP_1A0 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;

  trace_input ("sub", OP_REG_REG, 0);
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op1 - op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 < op0);
  ov = ((op1 & 0x80000000) != (op0 & 0x80000000)
	&& (op1 & 0x80000000) != (result & 0x80000000));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* subr reg1, reg2 */
int
OP_180 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;

  trace_input ("subr", OP_REG_REG, 0);
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 - op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op0 < op1);
  ov = ((op0 & 0x80000000) != (op1 & 0x80000000)
	&& (op0 & 0x80000000) != (result & 0x80000000));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* sxh reg1 */
int
OP_E0 ()
{
  trace_input ("mulh", OP_REG_REG, 0);
      
  State.regs[ OP[1] ] = (EXTEND16 (State.regs[ OP[1] ]) * EXTEND16 (State.regs[ OP[0] ]));
      
  trace_output (OP_REG_REG);

  return 2;
}

/* mulh sign_extend(imm5), reg2 */
int
OP_2E0 ()
{
  trace_input ("mulh", OP_IMM_REG, 0);
  
  State.regs[ OP[1] ] = EXTEND16 (State.regs[ OP[1] ]) * SEXT5 (OP[0]);
  
  trace_output (OP_IMM_REG);

  return 2;
}

/* mulhi imm16, reg1, reg2 */
int
OP_6E0 ()
{
  trace_input ("mulhi", OP_IMM16_REG_REG, 0);
  
  State.regs[ OP[1] ] = EXTEND16 (State.regs[ OP[0] ]) * EXTEND16 (OP[2]);
      
  trace_output (OP_IMM16_REG_REG);
  
  return 4;
}

/* divh reg1, reg2 */
int
OP_40 ()
{
  unsigned int op0, op1, result, ov, s, z;
  int temp;

  trace_input ("divh", OP_REG_REG, 0);

  /* Compute the result.  */
  temp = EXTEND16 (State.regs[ OP[0] ]);
  op0 = temp;
  op1 = State.regs[OP[1]];
  
  if (op0 == 0xffffffff && op1 == 0x80000000)
    {
      result = 0x80000000;
      ov = 1;
    }
  else if (op0 != 0)
    {
      result = op1 / op0;
      ov = 0;
    }
  else
    {
      result = 0x0;
      ov = 1;
    }
  
  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  
  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
	  | (ov ? PSW_OV : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* cmp reg, reg */
int
OP_1E0 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;

  trace_input ("cmp", OP_REG_REG_CMP, 0);
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op1 - op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 < op0);
  ov = ((op1 & 0x80000000) != (op0 & 0x80000000)
	&& (op1 & 0x80000000) != (result & 0x80000000));

  /* Set condition codes.  */
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_REG_REG_CMP);

  return 2;
}

/* cmp sign_extend(imm5), reg */
int
OP_260 ()
{
  unsigned int op0, op1, result, z, s, cy, ov;
  int temp;

  /* Compute the result.  */
  trace_input ("cmp", OP_IMM_REG_CMP, 0);
  temp = SEXT5 (OP[0]);
  op0 = temp;
  op1 = State.regs[OP[1]];
  result = op1 - op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 < op0);
  ov = ((op1 & 0x80000000) != (op0 & 0x80000000)
	&& (op1 & 0x80000000) != (result & 0x80000000));

  /* Set condition codes.  */
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0));
  trace_output (OP_IMM_REG_CMP);

  return 2;
}

/* setf cccc,reg2 */
int
OP_7E0 ()
{
  trace_input ("setf", OP_EX1, 0);

  State.regs[ OP[1] ] = condition_met (OP[0]);
  
  trace_output (OP_EX1);

  return 4;
}

/* satadd reg,reg */
int
OP_C0 ()
{
  unsigned int op0, op1, result, z, s, cy, ov, sat;
  
  trace_input ("satadd", OP_REG_REG, 0);
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 + op1;
  
  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (result < op0 || result < op1);
  ov = ((op0 & 0x80000000) == (op1 & 0x80000000)
	&& (op0 & 0x80000000) != (result & 0x80000000));
  sat = ov;
  
  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
	  | (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0)
	  | (sat ? PSW_SAT : 0));
  
  /* Handle saturated results.  */
  if (sat && s)
    State.regs[OP[1]] = 0x80000000;
  else if (sat)
    State.regs[OP[1]] = 0x7fffffff;
  trace_output (OP_REG_REG);

  return 2;
}

/* satadd sign_extend(imm5), reg */
int
OP_220 ()
{
  unsigned int op0, op1, result, z, s, cy, ov, sat;

  int temp;

  trace_input ("satadd", OP_IMM_REG, 0);

  /* Compute the result.  */
  temp = SEXT5 (OP[0]);
  op0 = temp;
  op1 = State.regs[OP[1]];
  result = op0 + op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (result < op0 || result < op1);
  ov = ((op0 & 0x80000000) == (op1 & 0x80000000)
	&& (op0 & 0x80000000) != (result & 0x80000000));
  sat = ov;

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0)
		| (sat ? PSW_SAT : 0));

  /* Handle saturated results.  */
  if (sat && s)
    State.regs[OP[1]] = 0x80000000;
  else if (sat)
    State.regs[OP[1]] = 0x7fffffff;
  trace_output (OP_IMM_REG);

  return 2;
}

/* satsub reg1, reg2 */
int
OP_A0 ()
{
  unsigned int op0, op1, result, z, s, cy, ov, sat;
  
  trace_input ("satsub", OP_REG_REG, 0);
  
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op1 - op0;
  
  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 < op0);
  ov = ((op1 & 0x80000000) != (op0 & 0x80000000)
	&& (op1 & 0x80000000) != (result & 0x80000000));
  sat = ov;
  
  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
	  | (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0)
	  | (sat ? PSW_SAT : 0));
  
  /* Handle saturated results.  */
  if (sat && s)
    State.regs[OP[1]] = 0x80000000;
  else if (sat)
    State.regs[OP[1]] = 0x7fffffff;
  trace_output (OP_REG_REG);
  return 2;
}

/* satsubi sign_extend(imm16), reg */
int
OP_660 ()
{
  unsigned int op0, op1, result, z, s, cy, ov, sat;
  int temp;

  trace_input ("satsubi", OP_IMM_REG, 0);

  /* Compute the result.  */
  temp = EXTEND16 (OP[2]);
  op0 = temp;
  op1 = State.regs[ OP[0] ];
  result = op1 - op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 < op0);
  ov = ((op1 & 0x80000000) != (op0 & 0x80000000)
	&& (op1 & 0x80000000) != (result & 0x80000000));
  sat = ov;

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0)
		| (sat ? PSW_SAT : 0));

  /* Handle saturated results.  */
  if (sat && s)
    State.regs[OP[1]] = 0x80000000;
  else if (sat)
    State.regs[OP[1]] = 0x7fffffff;
  trace_output (OP_IMM_REG);

  return 4;
}

/* satsubr reg,reg */
int
OP_80 ()
{
  unsigned int op0, op1, result, z, s, cy, ov, sat;
  
  trace_input ("satsubr", OP_REG_REG, 0);
  
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 - op1;
  
  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (result < op0);
  ov = ((op1 & 0x80000000) != (op0 & 0x80000000)
	&& (op1 & 0x80000000) != (result & 0x80000000));
  sat = ov;
  
  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_CY | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
	  | (cy ? PSW_CY : 0) | (ov ? PSW_OV : 0)
	  | (sat ? PSW_SAT : 0));
  
  /* Handle saturated results.  */
  if (sat && s)
    State.regs[OP[1]] = 0x80000000;
  else if (sat)
    State.regs[OP[1]] = 0x7fffffff;
  trace_output (OP_REG_REG);

  return 2;
}

/* tst reg,reg */
int
OP_160 ()
{
  unsigned int op0, op1, result, z, s;

  trace_input ("tst", OP_REG_REG_CMP, 0);

  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 & op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the condition codes.  */
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_REG_REG_CMP);

  return 2;
}

/* mov sign_extend(imm5), reg */
int
OP_200 ()
{
  int value = SEXT5 (OP[0]);
  
  trace_input ("mov", OP_IMM_REG_MOVE, 0);
  
  State.regs[ OP[1] ] = value;
  
  trace_output (OP_IMM_REG_MOVE);
  
  return 2;
}

/* movhi imm16, reg, reg */
int
OP_640 ()
{
  trace_input ("movhi", OP_UIMM16_REG_REG, 16);
      
  State.regs[ OP[1] ] = State.regs[ OP[0] ] + (OP[2] << 16);
      
  trace_output (OP_UIMM16_REG_REG);

  return 4;
}

/* sar zero_extend(imm5),reg1 */
int
OP_2A0 ()
{
  unsigned int op0, op1, result, z, s, cy;

  trace_input ("sar", OP_IMM_REG, 0);
  op0 = OP[0];
  op1 = State.regs[ OP[1] ];
  result = (signed)op1 >> op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 & (1 << (op0 - 1)));

  /* Store the result and condition codes.  */
  State.regs[ OP[1] ] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV | PSW_CY);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0));
  trace_output (OP_IMM_REG);

  return 2;
}

/* sar reg1, reg2 */
int
OP_A007E0 ()
{
  unsigned int op0, op1, result, z, s, cy;

  trace_input ("sar", OP_REG_REG, 0);
  
  op0 = State.regs[ OP[0] ] & 0x1f;
  op1 = State.regs[ OP[1] ];
  result = (signed)op1 >> op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 & (1 << (op0 - 1)));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV | PSW_CY);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0));
  trace_output (OP_REG_REG);

  return 4;
}

/* shl zero_extend(imm5),reg1 */
int
OP_2C0 ()
{
  unsigned int op0, op1, result, z, s, cy;

  trace_input ("shl", OP_IMM_REG, 0);
  op0 = OP[0];
  op1 = State.regs[ OP[1] ];
  result = op1 << op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 & (1 << (32 - op0)));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV | PSW_CY);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0));
  trace_output (OP_IMM_REG);

  return 2;
}

/* shl reg1, reg2 */
int
OP_C007E0 ()
{
  unsigned int op0, op1, result, z, s, cy;

  trace_input ("shl", OP_REG_REG, 0);
  op0 = State.regs[ OP[0] ] & 0x1f;
  op1 = State.regs[ OP[1] ];
  result = op1 << op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 & (1 << (32 - op0)));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV | PSW_CY);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0));
  trace_output (OP_REG_REG);

  return 4;
}

/* shr zero_extend(imm5),reg1 */
int
OP_280 ()
{
  unsigned int op0, op1, result, z, s, cy;

  trace_input ("shr", OP_IMM_REG, 0);
  op0 = OP[0];
  op1 = State.regs[ OP[1] ];
  result = op1 >> op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 & (1 << (op0 - 1)));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV | PSW_CY);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0));
  trace_output (OP_IMM_REG);

  return 2;
}

/* shr reg1, reg2 */
int
OP_8007E0 ()
{
  unsigned int op0, op1, result, z, s, cy;

  trace_input ("shr", OP_REG_REG, 0);
  op0 = State.regs[ OP[0] ] & 0x1f;
  op1 = State.regs[ OP[1] ];
  result = op1 >> op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);
  cy = (op1 & (1 << (op0 - 1)));

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV | PSW_CY);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0)
		| (cy ? PSW_CY : 0));
  trace_output (OP_REG_REG);

  return 4;
}

/* or reg, reg */
int
OP_100 ()
{
  unsigned int op0, op1, result, z, s;

  trace_input ("or", OP_REG_REG, 0);

  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 | op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* ori zero_extend(imm16), reg, reg */
int
OP_680 ()
{
  unsigned int op0, op1, result, z, s;

  trace_input ("ori", OP_UIMM16_REG_REG, 0);
  op0 = OP[2];
  op1 = State.regs[ OP[0] ];
  result = op0 | op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_UIMM16_REG_REG);

  return 4;
}

/* and reg, reg */
int
OP_140 ()
{
  unsigned int op0, op1, result, z, s;

  trace_input ("and", OP_REG_REG, 0);

  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 & op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* andi zero_extend(imm16), reg, reg */
int
OP_6C0 ()
{
  unsigned int result, z;

  trace_input ("andi", OP_UIMM16_REG_REG, 0);

  result = OP[2] & State.regs[ OP[0] ];

  /* Compute the condition codes.  */
  z = (result == 0);

  /* Store the result and condition codes.  */
  State.regs[ OP[1] ] = result;
  
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= (z ? PSW_Z : 0);
  
  trace_output (OP_UIMM16_REG_REG);

  return 4;
}

/* xor reg, reg */
int
OP_120 ()
{
  unsigned int op0, op1, result, z, s;

  trace_input ("xor", OP_REG_REG, 0);

  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  op1 = State.regs[ OP[1] ];
  result = op0 ^ op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_REG_REG);

  return 2;
}

/* xori zero_extend(imm16), reg, reg */
int
OP_6A0 ()
{
  unsigned int op0, op1, result, z, s;

  trace_input ("xori", OP_UIMM16_REG_REG, 0);
  op0 = OP[2];
  op1 = State.regs[ OP[0] ];
  result = op0 ^ op1;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_UIMM16_REG_REG);

  return 4;
}

/* not reg1, reg2 */
int
OP_20 ()
{
  unsigned int op0, result, z, s;

  trace_input ("not", OP_REG_REG_MOVE, 0);
  /* Compute the result.  */
  op0 = State.regs[ OP[0] ];
  result = ~op0;

  /* Compute the condition codes.  */
  z = (result == 0);
  s = (result & 0x80000000);

  /* Store the result and condition codes.  */
  State.regs[OP[1]] = result;
  PSW &= ~(PSW_Z | PSW_S | PSW_OV);
  PSW |= ((z ? PSW_Z : 0) | (s ? PSW_S : 0));
  trace_output (OP_REG_REG_MOVE);

  return 2;
}

/* set1 */
int
OP_7C0 ()
{
  unsigned int op0, op1, op2;
  int temp;

  trace_input ("set1", OP_BIT, 0);
  op0 = State.regs[ OP[0] ];
  op1 = OP[1] & 0x7;
  temp = EXTEND16 (OP[2]);
  op2 = temp;
  temp = load_mem (op0 + op2, 1);
  PSW &= ~PSW_Z;
  if ((temp & (1 << op1)) == 0)
    PSW |= PSW_Z;
  temp |= (1 << op1);
  store_mem (op0 + op2, 1, temp);
  trace_output (OP_BIT);

  return 4;
}

/* not1 */
int
OP_47C0 ()
{
  unsigned int op0, op1, op2;
  int temp;

  trace_input ("not1", OP_BIT, 0);
  op0 = State.regs[ OP[0] ];
  op1 = OP[1] & 0x7;
  temp = EXTEND16 (OP[2]);
  op2 = temp;
  temp = load_mem (op0 + op2, 1);
  PSW &= ~PSW_Z;
  if ((temp & (1 << op1)) == 0)
    PSW |= PSW_Z;
  temp ^= (1 << op1);
  store_mem (op0 + op2, 1, temp);
  trace_output (OP_BIT);

  return 4;
}

/* clr1 */
int
OP_87C0 ()
{
  unsigned int op0, op1, op2;
  int temp;

  trace_input ("clr1", OP_BIT, 0);
  op0 = State.regs[ OP[0] ];
  op1 = OP[1] & 0x7;
  temp = EXTEND16 (OP[2]);
  op2 = temp;
  temp = load_mem (op0 + op2, 1);
  PSW &= ~PSW_Z;
  if ((temp & (1 << op1)) == 0)
    PSW |= PSW_Z;
  temp &= ~(1 << op1);
  store_mem (op0 + op2, 1, temp);
  trace_output (OP_BIT);

  return 4;
}

/* tst1 */
int
OP_C7C0 ()
{
  unsigned int op0, op1, op2;
  int temp;

  trace_input ("tst1", OP_BIT, 0);
  op0 = State.regs[ OP[0] ];
  op1 = OP[1] & 0x7;
  temp = EXTEND16 (OP[2]);
  op2 = temp;
  temp = load_mem (op0 + op2, 1);
  PSW &= ~PSW_Z;
  if ((temp & (1 << op1)) == 0)
    PSW |= PSW_Z;
  trace_output (OP_BIT);

  return 4;
}

/* di */
int
OP_16007E0 ()
{
  trace_input ("di", OP_NONE, 0);
  PSW |= PSW_ID;
  trace_output (OP_NONE);

  return 4;
}

/* ei */
int
OP_16087E0 ()
{
  trace_input ("ei", OP_NONE, 0);
  PSW &= ~PSW_ID;
  trace_output (OP_NONE);

  return 4;
}

/* halt */
int
OP_12007E0 ()
{
  trace_input ("halt", OP_NONE, 0);
  /* FIXME this should put processor into a mode where NMI still handled */
  trace_output (OP_NONE);
  sim_engine_halt (simulator, STATE_CPU (simulator, 0), NULL, PC,
		   sim_stopped, SIM_SIGTRAP);
  return 0;
}

/* trap */
int
OP_10007E0 ()
{
  trace_input ("trap", OP_TRAP, 0);
  trace_output (OP_TRAP);

  /* Trap 31 is used for simulating OS I/O functions */

  if (OP[0] == 31)
    {
      int save_errno = errno;	
      errno = 0;

/* Registers passed to trap 0 */

#define FUNC   State.regs[6]	/* function number, return value */
#define PARM1  State.regs[7]	/* optional parm 1 */
#define PARM2  State.regs[8]	/* optional parm 2 */
#define PARM3  State.regs[9]	/* optional parm 3 */

/* Registers set by trap 0 */

#define RETVAL State.regs[10]	/* return value */
#define RETERR State.regs[11]	/* return error code */

/* Turn a pointer in a register into a pointer into real memory. */

#define MEMPTR(x) (map (x))

      switch (FUNC)
	{

#ifdef HAVE_FORK
#ifdef SYS_fork
	case SYS_fork:
	  RETVAL = fork ();
	  break;
#endif
#endif

#ifdef HAVE_EXECVE
#ifdef SYS_execv
	case SYS_execve:
	  {
	    char *path = fetch_str (simulator, PARM1);
	    char **argv = fetch_argv (simulator, PARM2);
	    char **envp = fetch_argv (simulator, PARM3);
	    RETVAL = execve (path, argv, envp);
	    zfree (path);
	    freeargv (argv);
	    freeargv (envp);
	    break;
	  }
#endif
#endif

#if HAVE_EXECV
#ifdef SYS_execv
	case SYS_execv:
	  {
	    char *path = fetch_str (simulator, PARM1);
	    char **argv = fetch_argv (simulator, PARM2);
	    RETVAL = execv (path, argv);
	    zfree (path);
	    freeargv (argv);
	    break;
	  }
#endif
#endif

#if 0
#ifdef SYS_pipe
	case SYS_pipe:
	  {
	    reg_t buf;
	    int host_fd[2];

	    buf = PARM1;
	    RETVAL = pipe (host_fd);
	    SW (buf, host_fd[0]);
	    buf += sizeof(uint16);
	    SW (buf, host_fd[1]);
	  }
	  break;
#endif
#endif

#if 0
#ifdef SYS_wait
	case SYS_wait:
	  {
	    int status;

	    RETVAL = wait (&status);
	    SW (PARM1, status);
	  }
	  break;
#endif
#endif

#ifdef SYS_read
	case SYS_read:
	  {
	    char *buf = zalloc (PARM3);
	    RETVAL = sim_io_read (simulator, PARM1, buf, PARM3);
	    sim_write (simulator, PARM2, buf, PARM3);
	    zfree (buf);
	    break;
	  }
#endif

#ifdef SYS_write
	case SYS_write:
	  {
	    char *buf = zalloc (PARM3);
	    sim_read (simulator, PARM2, buf, PARM3);
	    if (PARM1 == 1)
	      RETVAL = sim_io_write_stdout (simulator, buf, PARM3);
	    else
	      RETVAL = sim_io_write (simulator, PARM1, buf, PARM3);
	    zfree (buf);
	    break;
	  }
#endif

#ifdef SYS_lseek
	case SYS_lseek:
	  RETVAL = sim_io_lseek (simulator, PARM1, PARM2, PARM3);
	  break;
#endif

#ifdef SYS_close
	case SYS_close:
	  RETVAL = sim_io_close (simulator, PARM1);
	  break;
#endif

#ifdef SYS_open
	case SYS_open:
	  {
	    char *buf = fetch_str (simulator, PARM1);
	    RETVAL = sim_io_open (simulator, buf, PARM2);
	    zfree (buf);
	    break;
	  }
#endif

#ifdef SYS_exit
	case SYS_exit:
	  if ((PARM1 & 0xffff0000) == 0xdead0000 && (PARM1 & 0xffff) != 0)
	    /* get signal encoded by kill */
	    sim_engine_halt (simulator, STATE_CPU (simulator, 0), NULL, PC,
			     sim_signalled, PARM1 & 0xffff);
	  else if (PARM1 == 0xdead)
	    /* old libraries */
	    sim_engine_halt (simulator, STATE_CPU (simulator, 0), NULL, PC,
			     sim_stopped, SIM_SIGABRT);
	  else
	    /* PARM1 has exit status */
	    sim_engine_halt (simulator, STATE_CPU (simulator, 0), NULL, PC,
			     sim_exited, PARM1);
	  break;
#endif

#if !defined(__GO32__) && !defined(_WIN32)
#ifdef SYS_stat
	case SYS_stat:	/* added at hmsi */
	  /* stat system call */
	  {
	    struct stat host_stat;
	    reg_t buf;
	    char *path = fetch_str (simulator, PARM1);

	    RETVAL = stat (path, &host_stat);

	    zfree (path);
	    buf = PARM2;

	    /* Just wild-assed guesses.  */
	    store_mem (buf, 2, host_stat.st_dev);
	    store_mem (buf + 2, 2, host_stat.st_ino);
	    store_mem (buf + 4, 4, host_stat.st_mode);
	    store_mem (buf + 8, 2, host_stat.st_nlink);
	    store_mem (buf + 10, 2, host_stat.st_uid);
	    store_mem (buf + 12, 2, host_stat.st_gid);
	    store_mem (buf + 14, 2, host_stat.st_rdev);
	    store_mem (buf + 16, 4, host_stat.st_size);
	    store_mem (buf + 20, 4, host_stat.st_atime);
	    store_mem (buf + 28, 4, host_stat.st_mtime);
	    store_mem (buf + 36, 4, host_stat.st_ctime);
	  }
	  break;
#endif
#endif

#ifdef HAVE_CHOWN
#ifdef SYS_chown
	case SYS_chown:
	  {
	    char *path = fetch_str (simulator, PARM1);
	    RETVAL = chown (path, PARM2, PARM3);
	    zfree (path);
	  }
	  break;
#endif
#endif

#if HAVE_CHMOD
#ifdef SYS_chmod
	case SYS_chmod:
	  {
	    char *path = fetch_str (simulator, PARM1);
	    RETVAL = chmod (path, PARM2);
	    zfree (path);
	  }
	  break;
#endif
#endif

#ifdef SYS_time
#if HAVE_TIME
	case SYS_time:
	  {
	    time_t now;
	    RETVAL = time (&now);
	    store_mem (PARM1, 4, now);
	  }
	  break;
#endif
#endif

#if !defined(__GO32__) && !defined(_WIN32)
#ifdef SYS_times
	case SYS_times:
	  {
	    struct tms tms;
	    RETVAL = times (&tms);
	    store_mem (PARM1, 4, tms.tms_utime);
	    store_mem (PARM1 + 4, 4, tms.tms_stime);
	    store_mem (PARM1 + 8, 4, tms.tms_cutime);
	    store_mem (PARM1 + 12, 4, tms.tms_cstime);
	    break;
	  }
#endif
#endif

#ifdef SYS_gettimeofday
#if !defined(__GO32__) && !defined(_WIN32)
	case SYS_gettimeofday:
	  {
	    struct timeval t;
	    struct timezone tz;
	    RETVAL = gettimeofday (&t, &tz);
	    store_mem (PARM1, 4, t.tv_sec);
	    store_mem (PARM1 + 4, 4, t.tv_usec);
	    store_mem (PARM2, 4, tz.tz_minuteswest);
	    store_mem (PARM2 + 4, 4, tz.tz_dsttime);
	    break;
	  }
#endif
#endif

#ifdef SYS_utime
#if HAVE_UTIME
	case SYS_utime:
	  {
	    /* Cast the second argument to void *, to avoid type mismatch
	       if a prototype is present.  */
	    sim_io_error (simulator, "Utime not supported");
	    /* RETVAL = utime (path, (void *) MEMPTR (PARM2)); */
	  }
	  break;
#endif
#endif

	default:
	  abort ();
	}
      RETERR = errno;
      errno = save_errno;

      return 4;
    }
  else
    {				/* Trap 0 -> 30 */
      EIPC = PC + 4;
      EIPSW = PSW;
      /* Mask out EICC */
      ECR &= 0xffff0000;
      ECR |= 0x40 + OP[0];
      /* Flag that we are now doing exception processing.  */
      PSW |= PSW_EP | PSW_ID;
      PC = ((OP[0] < 0x10) ? 0x40 : 0x50) - 4;

      return 0;
    }
}

