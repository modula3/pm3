/* s390-mkopc.c -- Generates opcode table out of s390-opc.txt
   Copyright 2000, 2001 Free Software Foundation, Inc.
   Contributed by Martin Schwidefsky (schwidefsky@de.ibm.com).

   This file is part of GDB, GAS, and the GNU binutils.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ARCHBITS_ESA and ARCH_ESAME correspond to the bit numbers defined
   by s390_opcode_arch_val in include/opcode/s390.h:
     ARCHBITS_ESAONLY = (1<<S390_OPCODE_ESA)
     ARCHBITS_ESA     = (1<<S390_OPCODE_ESA) + (1<<S390_OPCODE_ESAME)
     ARCHBITS_ESA     = (1<<S390_OPCODE_ESAME).  */
#define ARCHBITS_ESAONLY 1
#define ARCHBITS_ESA     3
#define ARCHBITS_ESAME   2

struct op_struct {
	char  opcode[16];
	char  mnemonic[16];
	char  format[16];
        int   archbits;
        unsigned long long sort_value;
        int   no_nibbles;
};

struct op_struct *op_array;
int max_ops;
int no_ops;

static void
createTable(void)
{
    max_ops = 256;
    op_array = malloc(max_ops*sizeof(struct op_struct));
    no_ops = 0;
}

/*
 *  `insertOpcode': insert an op_struct into sorted opcode array 
 */
static void
insertOpcode(char *opcode, char *mnemonic, char *format, int archbits)
{
    char *str;
    unsigned long long sort_value;
    int no_nibbles;
    int ix, k;

    while (no_ops >= max_ops) {
      max_ops = max_ops*2;
      op_array = realloc(op_array, max_ops*sizeof(struct op_struct));
    }
    sort_value = 0;
    str = opcode;
    for (ix = 0; ix < 16; ix++) {
      if (*str >= '0' && *str <= '9')
	sort_value = (sort_value << 4) + (*str - '0');
      else if (*str >= 'a' && *str <= 'f')
	sort_value = (sort_value << 4) + (*str - 'a' + 10);
      else if (*str >= 'A' && *str <= 'F')
	sort_value = (sort_value << 4) + (*str - 'A' + 10);
      else if (*str == '?')
	sort_value <<= 4;
      else
	break;
      str++;
    }
    sort_value <<= 4*(16 - ix);
    no_nibbles = ix;
    for (ix = 0; ix < no_ops; ix++)
      if (sort_value > op_array[ix].sort_value)
        break;
    for (k = no_ops; k > ix; k--)
      op_array[k] = op_array[k-1];
    strcpy(op_array[ix].opcode, opcode);
    strcpy(op_array[ix].mnemonic, mnemonic);
    strcpy(op_array[ix].format, format);
    op_array[ix].sort_value = sort_value;
    op_array[ix].no_nibbles = no_nibbles;
    op_array[ix].archbits = archbits;
    no_ops++;
}


/*
 *  `dumpTable': write opcode table
 */
static void
dumpTable(void)
{
    char *str;
    int  ix;

    /*  Write hash table entries (slots). */
    printf("const struct s390_opcode s390_opcodes[] = {\n");
    for (ix = 0; ix < no_ops; ix++) {
      printf("  { \"%s\", ", op_array[ix].mnemonic);
      for (str = op_array[ix].opcode; *str != 0; str++)
	if (*str == '?')
	  *str = '0';
      printf("OP%i(0x%sLL), ", 
	     op_array[ix].no_nibbles*4, op_array[ix].opcode);
      printf("MASK_%s, INSTR_%s, ",
             op_array[ix].format, op_array[ix].format);
      printf("%i}", op_array[ix].archbits);
      if (ix < no_ops-1)
	printf(",\n");
      else
	printf("\n");
    }
    printf("};\n\n");
    printf("const int s390_num_opcodes =\n");
    printf("  sizeof (s390_opcodes) / sizeof (s390_opcodes[0]);\n\n");
}


int
main(void)
{
    char currentLine[256];

    createTable();
    /*  Read opcode descriptions from `stdin'.  For each mnemonic,
     *  make an entry into the opcode table.
     */
    while (fgets(currentLine, sizeof(currentLine), stdin) != NULL) {
      char  opcode[16];
      char  mnemonic[16];
      char  format[16];
      char  description[64];
      char  archtag[16];
      int   archbits;

      if (currentLine[0] == '#')
        continue;
      memset(opcode, 0, 8);
      if (sscanf(currentLine, "%15s %15s %15s \"%[^\"]\" %15s",
                 opcode, mnemonic, format, description, archtag) == 5) {
        if (strcmp(archtag, "esaonly") == 0)
          archbits = ARCHBITS_ESAONLY;
        else if (strcmp(archtag, "esa") == 0)
          archbits = ARCHBITS_ESA;
        else if (strcmp(archtag, "esame") == 0)
          archbits = ARCHBITS_ESAME;
        else
          archbits = 0;
        insertOpcode(opcode, mnemonic, format, archbits);
      } else
        fprintf(stderr, "Couldn't scan line %s\n", currentLine);
    }

    dumpTable();
    return 0;
}
