/* Modula-3 language support definitions for GDB, the GNU debugger.
   Copyright 1992, 2001 Free Software Foundation, Inc.

This file is part of GDB.

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if !defined (M3_LANG_H)
#define M3_LANG_H 1
 
#include "value.h"

extern int
m3_parse PARAMS ((void));	/* Defined in c-exp.y */

/* Defined in c-typeprint.c */
extern void m3_print_type (struct type *, char *, struct ui_file *, int, int);

extern void m3_emit_char (int, struct ui_file *, int);
extern void m3_emit_widechar (int, struct ui_file *, int);

extern int m3_val_print (struct type *, char *, int, CORE_ADDR,
			 struct ui_file *, int, int, int,
			 enum val_prettyprint);
extern int m3_val_print2 (struct type *, char *, int, int, struct ui_file *,
			  int, int, int);


extern struct type *m3_find_export_type (struct type *);

extern struct type *builtin_type_m3_address;
extern struct type *builtin_type_m3_boolean;
extern struct type *builtin_type_m3_cardinal;
extern struct type *builtin_type_m3_char;
extern struct type *builtin_type_m3_extended;
extern struct type *builtin_type_m3_integer;
extern struct type *builtin_type_m3_longreal;
extern struct type *builtin_type_m3_mutex;
extern struct type *builtin_type_m3_null;
extern struct type *builtin_type_m3_real;
extern struct type *builtin_type_m3_refany;
extern struct type *builtin_type_m3_root;
extern struct type *builtin_type_m3_text;
extern struct type *builtin_type_m3_untraced_root;
extern struct type *builtin_type_m3_void;
extern struct type *builtin_type_m3_widechar;

extern LONGEST m3_unpack_ord (char *, int, int, int);

extern CORE_ADDR m3_unpack_pointer (char *, int);

extern LONGEST m3_unpack_int2 (value_ptr);

extern double m3_unpack_float2 (value_ptr);

extern CORE_ADDR m3_unpack_pointer2 (value_ptr);

extern struct type *find_m3_type_with_uid (int);

extern struct type *find_m3_type_named (char *);

extern struct type *find_m3_exported_interfaces (char *);

extern struct symbol *find_m3_ir (int, char *);

extern char *find_m3_type_name (struct type *);


/* given a heap reference,
   find the address of the typecell for the actual type */
extern CORE_ADDR find_m3_heap_tc_addr (CORE_ADDR);

/* given the address of a typecell, find the gdb type for it */
extern struct type *find_m3_type_from_tc (CORE_ADDR);

/* given a gdb type, find the address of the corresponding typecell */
extern CORE_ADDR find_tc_from_m3_type (struct type *t);

/* given a heap reference, find it's actual type */
extern struct type *find_m3_heap_type (CORE_ADDR);

extern int tc_address_to_dataOffset (CORE_ADDR);

extern int tc_address_to_methodOffset (CORE_ADDR);

extern int tc_address_to_dataSize (CORE_ADDR);

extern CORE_ADDR tc_address_to_parent_tc_address (CORE_ADDR);

extern CORE_ADDR tc_address_to_defaultMethods (CORE_ADDR);

extern value_ptr m3_value_from_longest (struct type *, LONGEST);

extern int is_m3_ordinal_type (struct type *);

extern void m3_ordinal_bounds (struct type *, LONGEST *, LONGEST *);

extern void m3_printchar (int, struct ui_file *);
extern void m3_printwidechar (int, struct ui_file *);

extern int m3_value_print (struct value *, struct ui_file *, int,
			   enum val_prettyprint);

extern void m3_decode_struct (struct type *);

extern int find_m3_rec_field (struct type *, char *, int *, int *,
			      struct type **);

extern int find_m3_obj_field (struct type *, char *, int *, int *,
			      struct type **);

extern char *m3_read_object_fields_bits (CORE_ADDR);

#endif /* !defined (M3_LANG_H) */
