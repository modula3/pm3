/* Java language support routines for GDB, the GNU debugger.
   Copyright 1997 Free Software Foundation, Inc.

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
#include "gdbtypes.h"
#include "symtab.h"
#include "symfile.h"
#include "objfiles.h"
#include "gdb_string.h"
#include "value.h"
#include "c-lang.h"
#include "jv-lang.h"
#include "gdbcore.h"

struct type *java_int_type;
struct type *java_byte_type;
struct type *java_short_type;
struct type *java_long_type;
struct type *java_boolean_type;
struct type *java_char_type;
struct type *java_float_type;
struct type *java_double_type;
struct type *java_void_type;

struct type *java_object_type;

/* This objfile contains symtabs that have been dynamically created
   to record dynamically loaded Java classes and dynamically
   compiled java methods. */
struct objfile *dynamics_objfile = NULL;

struct type *java_link_class_type PARAMS((struct type*, value_ptr));

struct objfile *
get_dynamics_objfile ()
{
  if (dynamics_objfile == NULL)
    {
      dynamics_objfile = allocate_objfile (NULL, 0);
    }
  return dynamics_objfile;
}

#if 1
/* symtab contains classes read from the inferior. */

static struct symtab *class_symtab = NULL;

/* Maximum number of class in class_symtab before relocation is needed. */

static int class_symtab_space;

struct symtab *
get_java_class_symtab ()
{
  if (class_symtab == NULL)
    {
      struct objfile *objfile = get_dynamics_objfile();
      struct blockvector *bv;
      struct block *bl;
      class_symtab = allocate_symtab ("<java-classes>", objfile);
      class_symtab->language = language_java;
      bv = (struct blockvector *)
	obstack_alloc (&objfile->symbol_obstack, sizeof (struct blockvector));
      BLOCKVECTOR_NBLOCKS (bv) = 1;
      BLOCKVECTOR (class_symtab) = bv;

      /* Allocate dummy STATIC_BLOCK. */
      bl = (struct block *)
	obstack_alloc (&objfile->symbol_obstack, sizeof (struct block));
      BLOCK_NSYMS (bl) = 0;
      BLOCK_START (bl) = 0;
      BLOCK_END (bl) = 0;
      BLOCK_FUNCTION (bl) = NULL;
      BLOCK_SUPERBLOCK (bl) = NULL;
      BLOCK_GCC_COMPILED (bl) = 0;
      BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK) = bl;

      /* Allocate GLOBAL_BLOCK.  This has to be relocatable. */
      class_symtab_space = 128;
      bl = (struct block *)
	mmalloc (objfile->md,
		 sizeof (struct block)
		 + ((class_symtab_space - 1) * sizeof (struct symbol *)));
      *bl = *BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
      BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK) = bl;
      class_symtab->free_ptr = (char *) bl;
    }
  return class_symtab;
}

static void
add_class_symtab_symbol (sym)
     struct symbol *sym;
{
  struct symtab *symtab = get_java_class_symtab ();
  struct blockvector *bv = BLOCKVECTOR (symtab);
  struct block *bl = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
  if (BLOCK_NSYMS (bl) >= class_symtab_space)
    {
      /* Need to re-allocate. */
      class_symtab_space *= 2;
      bl = (struct block *)
	mrealloc (symtab->objfile->md, bl,
		  sizeof (struct block)
		  + ((class_symtab_space - 1) * sizeof (struct symbol *)));
      class_symtab->free_ptr = (char *) bl;
      BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK) = bl;
    }
  
  BLOCK_SYM (bl, BLOCK_NSYMS (bl)) = sym;
  BLOCK_NSYMS (bl) = BLOCK_NSYMS (bl) + 1;
}

struct symbol *
add_class_symbol (type, addr)
     struct type *type;
     CORE_ADDR addr;
{
  struct symbol *sym;
  sym = (struct symbol *)
    obstack_alloc (&dynamics_objfile->symbol_obstack, sizeof (struct symbol));
  memset (sym, 0, sizeof (struct symbol));
  SYMBOL_LANGUAGE (sym) = language_java;
  SYMBOL_NAME (sym) = TYPE_TAG_NAME (type);
  SYMBOL_CLASS (sym) = LOC_TYPEDEF;
  /*  SYMBOL_VALUE (sym) = valu;*/
  SYMBOL_TYPE (sym) = type;
  SYMBOL_NAMESPACE (sym) = STRUCT_NAMESPACE;
  SYMBOL_VALUE_ADDRESS (sym) = addr;
  return sym;
}
#endif

struct type *
java_lookup_class (name)
     char *name;
{
  struct symbol *sym;
  sym = lookup_symbol (name, expression_context_block, STRUCT_NAMESPACE,
		       (int *) 0, (struct symtab **) NULL);
  if (sym != NULL)
    return SYMBOL_TYPE (sym);
#if 0
  CORE_ADDR addr;
  if (called from parser)
    {
      call lookup_class (or similar) in inferior;
      if not found:
	return NULL;
      addr = found in inferior;
    }
  else
    addr = 0;
  struct type *type;
  type = alloc_type (objfile);
  TYPE_CODE (type) = TYPE_CODE_STRUCT;
  INIT_CPLUS_SPECIFIC (type);
  TYPE_TAG_NAME (type) = obsavestring (name, strlen(name), &objfile->type_obstack);
  TYPE_FLAGS (type) |= TYPE_FLAG_STUB;
  TYPE ? = addr;
  return type;
#else
  /* FIXME - should search inferior's symbol table. */
  return NULL;
#endif
}

/* Return a nul-terminated string (allocated on OBSTACK) for
   a name given by NAME (which has type Utf8Const*). */

char *
get_java_utf8_name (obstack, name)
     struct obstack *obstack;
     value_ptr name;
{
  char *chrs;
  value_ptr temp = name;
  int name_length;
  CORE_ADDR data_addr;
  temp = value_struct_elt (&temp, NULL, "length", NULL, "structure");
  name_length = (int) value_as_long (temp);
  data_addr = VALUE_ADDRESS (temp) + VALUE_OFFSET (temp)
    + TYPE_LENGTH (VALUE_TYPE (temp));
  chrs = obstack_alloc (obstack, name_length+1);
  chrs [name_length] = '\0';
  read_memory_section (data_addr, chrs, name_length, NULL);
  return chrs;
}

value_ptr
java_class_from_object (obj_val)
     value_ptr obj_val;
{
  /* This is all rather inefficient, since the offsets of dtable and
     class are fixed.  FIXME */
  value_ptr dtable_val;

  if (TYPE_CODE (VALUE_TYPE (obj_val)) == TYPE_CODE_PTR
      && TYPE_LENGTH (TYPE_TARGET_TYPE (VALUE_TYPE (obj_val))) == 0)
    {
      struct symbol *sym;
      sym = lookup_symbol ("Hjava_lang_Object", NULL, STRUCT_NAMESPACE,
			   (int *) 0, (struct symtab **) NULL);
      if (sym != NULL)
	obj_val = value_at (VALUE_TYPE (sym),
			    value_as_pointer (obj_val), NULL);
    }

  dtable_val = value_struct_elt (&obj_val, NULL, "dtable", NULL, "structure");
  return value_struct_elt (&dtable_val, NULL, "class", NULL, "structure");
}

/* Check if CLASS_IS_PRIMITIVE(value of clas): */
int
java_class_is_primitive (clas)
     value_ptr clas;
{
  value_ptr dtable = value_struct_elt (&clas, NULL, "dtable", NULL, "struct");
  CORE_ADDR i = value_as_pointer (dtable);
  return (int) (i & 0x7fffffff) == (int) 0x7fffffff;
}

/* Read a Kaffe Class object, and generated a gdb (TYPE_CODE_STRUCT) type. */

struct type *
type_from_class (clas)
     value_ptr clas;
{
  struct type *type;
  char *name;
  value_ptr temp;
  struct objfile *objfile;
  value_ptr utf8_name;
  char *nptr;
  CORE_ADDR addr;
  struct block *bl;
  int i;
  int is_array = 0;

  type = check_typedef (VALUE_TYPE (clas));
  if (TYPE_CODE (type) == TYPE_CODE_PTR)
    {
      if (value_logical_not (clas))
	return NULL;
      clas = value_ind (clas);
    }
  addr = VALUE_ADDRESS (clas) + VALUE_OFFSET (clas);

#if 0
  get_java_class_symtab ();
  bl = BLOCKVECTOR_BLOCK (BLOCKVECTOR (class_symtab), GLOBAL_BLOCK);
  for (i = BLOCK_NSYMS (bl);  --i >= 0; )
    {
      struct symbol *sym = BLOCK_SYM (bl, i);
      if (SYMBOL_VALUE_ADDRESS (sym) == addr)
	return SYMBOL_TYPE (sym);
    }
#endif

  objfile = get_dynamics_objfile();
  if (java_class_is_primitive (clas))
    {
      value_ptr sig;
      temp = clas;
      sig = value_struct_elt (&temp, NULL, "msize", NULL, "structure");
      return java_primitive_type (value_as_long (sig));
    }

  /* Get Class name. */
  /* if clasloader non-null, prepend loader address. FIXME */
  temp = clas;
  utf8_name = value_struct_elt (&temp, NULL, "name", NULL, "structure");
  name = get_java_utf8_name (&objfile->type_obstack, utf8_name);
  for (nptr = name;  *nptr != 0;  nptr++)
    {
      if (*nptr == '/')
	*nptr = '.';
    }

  type = java_lookup_class (name);
  if (type != NULL)
    return type;

  type = alloc_type (objfile);
  TYPE_CODE (type) = TYPE_CODE_STRUCT;
  INIT_CPLUS_SPECIFIC (type);

  if (name[0] == '[')
    {
      is_array = 1;
      temp = clas;
      /* Set array element type. */
      temp = value_struct_elt (&temp, NULL, "methods", NULL, "structure");
      VALUE_TYPE (temp) = lookup_pointer_type (VALUE_TYPE (clas));
      TYPE_TARGET_TYPE (type) = type_from_class (temp);
    }

  ALLOCATE_CPLUS_STRUCT_TYPE (type);
  TYPE_TAG_NAME (type) = name;

  add_class_symtab_symbol (add_class_symbol (type, addr));
  return java_link_class_type (type, clas);
}

/* Fill in class TYPE with data from the CLAS value. */ 

struct type *
java_link_class_type (type, clas)
     struct type *type;
     value_ptr clas;
{
  value_ptr temp;
  char *unqualified_name;
  char *name = TYPE_TAG_NAME (type);
  int ninterfaces, nfields, nmethods;
  int type_is_object = 0;
  struct fn_field *fn_fields;
  struct fn_fieldlist *fn_fieldlists;
  value_ptr fields, field, method, methods;
  int i, j;
  struct objfile *objfile = get_dynamics_objfile();
  struct type *tsuper;

  unqualified_name = strrchr (name, '.');
  if (unqualified_name == NULL)
    unqualified_name = name;

  temp = clas;
  temp = value_struct_elt (&temp, NULL, "superclass", NULL, "structure");
  if (name != NULL && strcmp (name, "java.lang.Object") == 0)
    {
      tsuper = get_java_object_type ();
      if (tsuper && TYPE_CODE (tsuper) == TYPE_CODE_PTR)
	tsuper = TYPE_TARGET_TYPE (tsuper);
      type_is_object = 1;
    }
  else
    tsuper = type_from_class (temp);

#if 1
  ninterfaces = 0;
#else
  temp = clas;
  ninterfaces = value_as_long (value_struct_elt (&temp, NULL, "interface_len", NULL, "structure"));
#endif
  TYPE_N_BASECLASSES (type) = (tsuper == NULL ? 0 : 1) + ninterfaces;
  temp = clas;
  nfields = value_as_long (value_struct_elt (&temp, NULL, "nfields", NULL, "structure"));
  nfields += TYPE_N_BASECLASSES (type);
  nfields++;  /* Add one for dummy "class" field. */
  TYPE_NFIELDS (type) = nfields;
  TYPE_FIELDS (type) = (struct field *)
    TYPE_ALLOC (type, sizeof (struct field) * nfields);

  memset (TYPE_FIELDS (type), 0, sizeof (struct field) * nfields);

  TYPE_FIELD_PRIVATE_BITS (type) =
    (B_TYPE *) TYPE_ALLOC (type, B_BYTES (nfields));
  B_CLRALL (TYPE_FIELD_PRIVATE_BITS (type), nfields);

  TYPE_FIELD_PROTECTED_BITS (type) =
    (B_TYPE *) TYPE_ALLOC (type, B_BYTES (nfields));
  B_CLRALL (TYPE_FIELD_PROTECTED_BITS (type), nfields);

  TYPE_FIELD_IGNORE_BITS (type) =
    (B_TYPE *) TYPE_ALLOC (type, B_BYTES (nfields));
  B_CLRALL (TYPE_FIELD_IGNORE_BITS (type), nfields);

  TYPE_FIELD_VIRTUAL_BITS (type) = (B_TYPE *)
    TYPE_ALLOC (type, B_BYTES (TYPE_N_BASECLASSES (type)));
  B_CLRALL (TYPE_FIELD_VIRTUAL_BITS (type), TYPE_N_BASECLASSES (type));

  if (tsuper != NULL)
    {
      TYPE_BASECLASS (type, 0) = tsuper;
      if (type_is_object)
	SET_TYPE_FIELD_PRIVATE (type, 0);
    }


  if (name[0] == '[' && tsuper != NULL)
    {
      TYPE_LENGTH (type) = TYPE_LENGTH (tsuper) + 4;  /* size with "length" */
    }
  else
    {
      temp = clas;
      temp = value_struct_elt (&temp, NULL, "bfsize", NULL, "structure");
      TYPE_LENGTH (type) = value_as_long (temp);
    }

  fields = NULL;
  nfields--;  /* First set up dummy "class" field. */
  SET_FIELD_PHYSADDR (TYPE_FIELD (type, nfields),
		      VALUE_ADDRESS (clas) + VALUE_OFFSET (clas));
  TYPE_FIELD_NAME (type, nfields) = "class";
  TYPE_FIELD_TYPE (type, nfields) = VALUE_TYPE (clas);
  SET_TYPE_FIELD_PRIVATE (type, nfields);
  
  for (i = TYPE_N_BASECLASSES (type);  i < nfields;  i++)
    {
      int accflags;
      int boffset;
      if (fields == NULL)
	{
	  temp = clas;
	  fields = value_struct_elt (&temp, NULL, "fields", NULL, "structure");
	  field = value_ind (fields);
	}
      else
	{ /* Re-use field value for next field. */
	  VALUE_ADDRESS (field) += TYPE_LENGTH (VALUE_TYPE (field));
	  VALUE_LAZY (field) = 1;
	}
      temp = field;
      temp = value_struct_elt (&temp, NULL, "name", NULL, "structure");
      TYPE_FIELD_NAME (type, i) =
	get_java_utf8_name (&objfile->type_obstack, temp);
      temp = field;
      accflags = value_as_long (value_struct_elt (&temp, NULL, "accflags",
						  NULL, "structure"));
      temp = field;
      temp = value_struct_elt (&temp, NULL, "info", NULL, "structure");
      boffset = value_as_long (value_struct_elt (&temp, NULL, "boffset",
						  NULL, "structure"));
      if (accflags & 0x0001) /* public access */
	{
	  /* ??? */
	}
      if (accflags & 0x0002) /* private access */
	{
	  SET_TYPE_FIELD_PRIVATE (type, i);
	}
      if (accflags & 0x0004) /* protected access */
	{
	  SET_TYPE_FIELD_PROTECTED (type, i);
	}
      if (accflags & 0x0008)  /* ACC_STATIC */
	SET_FIELD_PHYSADDR(TYPE_FIELD(type, i), boffset);
      else
	TYPE_FIELD_BITPOS (type, i) = 8 * boffset;
      if (accflags & 0x8000) /* FIELD_UNRESOLVED_FLAG */
	{
	  TYPE_FIELD_TYPE (type, i) = get_java_object_type (); /* FIXME */
	}
      else
	{
	  struct type *ftype;
	  temp = field;
	  temp = value_struct_elt (&temp, NULL, "type", NULL, "structure");
	  ftype = type_from_class (temp);
	  if (TYPE_CODE (ftype) == TYPE_CODE_STRUCT)
	    ftype = lookup_pointer_type (ftype);
	  TYPE_FIELD_TYPE (type, i) = ftype;
	}
    }

  temp = clas;
  nmethods = value_as_long (value_struct_elt (&temp, NULL, "nmethods",
					      NULL, "structure"));
  TYPE_NFN_FIELDS_TOTAL (type) = nmethods;
  j = nmethods * sizeof (struct fn_field);
  fn_fields = (struct fn_field*)
    obstack_alloc (&dynamics_objfile->symbol_obstack, j);
  memset (fn_fields, 0, j);
  fn_fieldlists = (struct fn_fieldlist*)
    alloca (nmethods * sizeof (struct fn_fieldlist));

  methods = NULL;
  for (i = 0;  i < nmethods;  i++)
    {
      char *mname;
      int k;
      if (methods == NULL)
	{
	  temp = clas;
	  methods = value_struct_elt (&temp, NULL, "methods", NULL, "structure");
	  method = value_ind (methods);
	}
      else
	{ /* Re-use method value for next method. */
	  VALUE_ADDRESS (method) += TYPE_LENGTH (VALUE_TYPE (method));
	  VALUE_LAZY (method) = 1;
	}

      /* Get method name. */
      temp = method;
      temp = value_struct_elt (&temp, NULL, "name", NULL, "structure");
      mname = get_java_utf8_name (&objfile->type_obstack, temp);
      if (strcmp (mname, "<init>") == 0)
	mname = unqualified_name;

      /* Check for an existing method with the same name.
       * This makes building the fn_fieldslists an O(nmethods**2)
       * operation.  That could be using hashing, but I doubt it
       * is worth it.  Note that we do maintain the order of methods
       * in the inferior's Method table (as long as that is grouped
       * by method name), which I think is desirable.  --PB */
      for (k = 0, j = TYPE_NFN_FIELDS (type);  ; )
	{
	  if (--j < 0)
	    { /* No match - new method name. */
	      j = TYPE_NFN_FIELDS(type)++;
	      fn_fieldlists[j].name = mname;
	      fn_fieldlists[j].length = 1;
	      fn_fieldlists[j].fn_fields = &fn_fields[i];
	      k = i;
	      break;
	    }
	  if (strcmp (mname, fn_fieldlists[j].name) == 0)
	    { /* Found an existing method with the same name. */
	      int l;
	      if (mname != unqualified_name)
		obstack_free (&objfile->type_obstack, mname);
	      mname = fn_fieldlists[j].name;
	      fn_fieldlists[j].length++;
	      k = i - k;  /* Index of new slot. */
	      /* Shift intervening fn_fields (between k and i) down. */
	      for (l = i;  l > k;  l--) fn_fields[l] = fn_fields[l-1];
	      for (l = TYPE_NFN_FIELDS (type);  --l > j; )
		fn_fieldlists[l].fn_fields++;
	      break;
	    }
	  k += fn_fieldlists[j].length;
	}
      fn_fields[k].physname = "";
      fn_fields[k].is_stub = 1;
      fn_fields[k].type = make_function_type (java_void_type, NULL); /* FIXME*/
      TYPE_CODE (fn_fields[k].type) = TYPE_CODE_METHOD;
    }

  j = TYPE_NFN_FIELDS(type) * sizeof (struct fn_fieldlist);
  TYPE_FN_FIELDLISTS (type) = (struct fn_fieldlist*)
    obstack_alloc (&dynamics_objfile->symbol_obstack, j);
  memcpy (TYPE_FN_FIELDLISTS (type), fn_fieldlists, j);
 
  return type;
}

struct type*
get_java_object_type ()
{
  return java_object_type;
}

int
is_object_type (type)
     struct type *type;
{
  CHECK_TYPEDEF (type);
  if (TYPE_CODE (type) == TYPE_CODE_PTR)
    {
      struct type *ttype = check_typedef (TYPE_TARGET_TYPE (type));
      char *name;
      if (TYPE_CODE (ttype) != TYPE_CODE_STRUCT)
	return 0;
      while (TYPE_N_BASECLASSES (ttype) > 0)
	ttype = TYPE_BASECLASS (ttype, 0);
      name = TYPE_TAG_NAME (ttype);
      if (name != NULL && strcmp (name, "java.lang.Object") == 0)
	return 1;
      name = TYPE_NFIELDS (ttype) > 0 ? TYPE_FIELD_NAME (ttype, 0) : (char*)0;
      if (name != NULL && strcmp (name, "dtable") == 0)
	{
	  if (java_object_type == NULL)
	    java_object_type = type;
	  return 1;
	}
    }
  return 0;
}

struct type*
java_primitive_type (signature)
     int signature;
{
  switch (signature)
    {
    case 'B':  return java_byte_type;
    case 'S':  return java_short_type;
    case 'I':  return java_int_type;
    case 'J':  return java_long_type;
    case 'Z':  return java_boolean_type;
    case 'C':  return java_char_type;
    case 'F':  return java_float_type;
    case 'D':  return java_double_type;
    case 'V':  return java_void_type;
    }
  error ("unknown signature '%c' for primitive type", (char) signature);
}

/* Return the demangled name of the Java type signature string SIGNATURE,
   as a freshly allocated copy. */

char *
java_demangle_type_signature (signature)
     char *signature;
{
  int array = 0;
  char *result;
  char *ptr;
  int i;
  while (*signature == '[')
    {
      array++;
      signature++;
    }
  switch (signature[0])
    {
    case 'L':
      /* Substract 2 for 'L' and ';', but add 1 for final nul. */
      result = xmalloc (strlen (signature) - 1 + 2 * array);
      signature++;
      ptr = result;
      for ( ; *signature != ';' && *signature != '\0'; signature++)
	{
	  if (*signature == '/')
	    *ptr++ = '.';
	  else
	    *ptr++ = *signature;
	}
      break;
    default:
      ptr = TYPE_NAME (java_primitive_type (signature[0]));
      i = strlen (ptr);
      result = xmalloc (i + 1 + 2 * array);
      strcpy (result, ptr);
      ptr = result + i;
      break;
    }
  while (--array >= 0)
    {
      *ptr++ = '[';
      *ptr++ = ']';
    }
  *ptr = '\0';
  return result;
}

struct type *
java_lookup_type (signature)
     char *signature;
{
  switch (signature[0])
    {
    case 'L':
    case '[':
      error ("java_lookup_type not fully inmplemented");
    default:
      return java_primitive_type (signature[0]);
    }
}

/* Return the type of TYPE followed by DIMS pairs of [ ].
   If DIMS == 0, TYPE is returned. */

struct type *
java_array_type (type, dims)
     struct type *type;
     int dims;
{
  if (dims == 0)
    return type;
  error ("array types not implemented");
}

/* Create a Java string in the inferior from a (Utf8) literal. */

value_ptr
java_value_string (ptr, len)
     char *ptr;
     int len;
{
  error ("not implemented - java_value_string"); /* FIXME */
}

/* Print the character C on STREAM as part of the contents of a literal
   string whose delimiter is QUOTER.  Note that that format for printing
   characters and strings is language specific. */

void
java_emit_char (c, stream, quoter)
     register int c;
     GDB_FILE *stream;
     int quoter;
{
  if (PRINT_LITERAL_FORM (c))
    {
      if (c == '\\' || c == quoter)
	{
	  fputs_filtered ("\\", stream);
	}
      fprintf_filtered (stream, "%c", c);
    }
  else
    {
      switch (c)
	{
	case '\n':
	  fputs_filtered ("\\n", stream);
	  break;
	case '\b':
	  fputs_filtered ("\\b", stream);
	  break;
	case '\t':
	  fputs_filtered ("\\t", stream);
	  break;
	case '\f':
	  fputs_filtered ("\\f", stream);
	  break;
	case '\r':
	  fputs_filtered ("\\r", stream);
	  break;
	case '\033':
	  fputs_filtered ("\\e", stream);
	  break;
	case '\007':
	  fputs_filtered ("\\a", stream);
	  break;
	default:
	  if (c < 256)
	    fprintf_filtered (stream, "\\%.3o", (unsigned int) c);
	  else
	    fprintf_filtered (stream, "\\u%.4x", (unsigned int) c);
	  break;
	}
    }
}

void
java_printchar (c, stream)
     int c;
     GDB_FILE *stream;
{
  fputs_filtered ("'", stream);
  java_emit_char (c, stream, '\'');
  fputs_filtered ("'", stream);
}

static value_ptr
evaluate_subexp_java (expect_type, exp, pos, noside)
     struct type *expect_type;
     register struct expression *exp;
     register int *pos;
     enum noside noside;
{
  int pc = *pos;
  int i;
  char *name;
  enum exp_opcode op = exp->elts[*pos].opcode;
  value_ptr arg1, arg2;
  struct type *type;
  switch (op)
    {
    case UNOP_IND:
      if (noside == EVAL_SKIP)
	goto standard;
      (*pos)++;
      arg1 = evaluate_subexp_standard (expect_type, exp, pos, EVAL_NORMAL);
      if (is_object_type (VALUE_TYPE (arg1)))
	{
	  struct type *type = type_from_class (java_class_from_object (arg1));
	  arg1 = value_cast (lookup_pointer_type (type), arg1);
	}
      if (noside == EVAL_SKIP)
	goto nosideret;
      return value_ind (arg1);

    case BINOP_SUBSCRIPT:
      (*pos)++;
      arg1 = evaluate_subexp_with_coercion (exp, pos, noside);
      arg2 = evaluate_subexp_with_coercion (exp, pos, noside);
      if (noside == EVAL_SKIP)
	goto nosideret;
      /* If the user attempts to subscript something that is not an
	 array or pointer type (like a plain int variable for example),
	 then report this as an error. */
      
      COERCE_REF (arg1);
      type = check_typedef (VALUE_TYPE (arg1));
      name = TYPE_NAME (type);
      if (TYPE_CODE (type) == TYPE_CODE_PTR)
	{
	  type = check_typedef (TYPE_TARGET_TYPE (type));
	  if (TYPE_CODE (type) == TYPE_CODE_STRUCT
	      && TYPE_TAG_NAME (type) != NULL 
	      && TYPE_TAG_NAME (type)[0] == '[')
	    {
	      CORE_ADDR address;
	      long length, index;
	      struct type *el_type;
	      char buf4[4];

	      value_ptr clas = java_class_from_object(arg1);
	      value_ptr temp = clas;
	      /* Get CLASS_ELEMENT_TYPE of the array type. */
	      temp = value_struct_elt (&temp, NULL, "methods",
				       NULL, "structure"); 
	      VALUE_TYPE (temp) = VALUE_TYPE (clas);
	      el_type = type_from_class (temp);
	      if (TYPE_CODE (el_type) == TYPE_CODE_STRUCT)
		el_type = lookup_pointer_type (el_type);

	      if (noside == EVAL_AVOID_SIDE_EFFECTS)
		return value_zero (el_type, VALUE_LVAL (arg1));
	      address = value_as_pointer (arg1);
	      address += JAVA_OBJECT_SIZE;
	      read_memory (address, buf4, 4);
	      length = (long) extract_signed_integer (buf4, 4);
	      index = (long) value_as_long (arg2);
	      if (index >= length || index < 0)
		error ("array index (%ld) out of bounds (length: %ld)",
		       index, length);
	      address = (address + 4) + index * TYPE_LENGTH (el_type);
	      return value_at (el_type, address, NULL);
	    }
	}
      else if (TYPE_CODE (type) == TYPE_CODE_ARRAY)
	{
	  if (noside == EVAL_AVOID_SIDE_EFFECTS)
	    return value_zero (TYPE_TARGET_TYPE (type), VALUE_LVAL (arg1));
	  else
	    return value_subscript (arg1, arg2);
	}
      if (name == NULL)
	name == TYPE_TAG_NAME (type);
      if (name)
	error ("cannot subscript something of type `%s'", name);
      else
	error ("cannot subscript requested type");

    case OP_STRING:
      (*pos)++;
      i = longest_to_int (exp->elts[pc + 1].longconst);
      (*pos) += 3 + BYTES_TO_EXP_ELEM (i + 1);
      if (noside == EVAL_SKIP)
	goto nosideret;
      return java_value_string (&exp->elts[pc + 2].string, i);

    case STRUCTOP_STRUCT:
      arg1 = evaluate_subexp_standard (expect_type, exp, pos, noside);
      /* Convert object field (such as TYPE.class) to reference. */
      if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_STRUCT)
	arg1 = value_addr (arg1);
      return arg1;
    default:
      break;
    }
standard:
  return evaluate_subexp_standard (expect_type, exp, pos, noside);
 nosideret:
  return value_from_longest (builtin_type_long, (LONGEST) 1);
}

static struct type *
java_create_fundamental_type (objfile, typeid)
     struct objfile *objfile;
     int typeid;
{
  switch (typeid)
    {
    case FT_VOID:           return java_void_type;
    case FT_BOOLEAN:        return java_boolean_type;
    case FT_CHAR:           return java_char_type;
    case FT_FLOAT:          return java_float_type;
    case FT_DBL_PREC_FLOAT: return java_double_type;
    case FT_BYTE: case FT_SIGNED_CHAR:       return java_byte_type;
    case FT_SHORT: case FT_SIGNED_SHORT:     return java_short_type;
    case FT_INTEGER: case FT_SIGNED_INTEGER: return java_int_type;
    case FT_LONG: case FT_SIGNED_LONG:       return java_long_type;
    }
  return c_create_fundamental_type (objfile, typeid);
}

/* Table mapping opcodes into strings for printing operators
   and precedences of the operators.  */

const struct op_print java_op_print_tab[] =
  {
    {",",  BINOP_COMMA, PREC_COMMA, 0},
    {"=",  BINOP_ASSIGN, PREC_ASSIGN, 1},
    {"||", BINOP_LOGICAL_OR, PREC_LOGICAL_OR, 0},
    {"&&", BINOP_LOGICAL_AND, PREC_LOGICAL_AND, 0},
    {"|",  BINOP_BITWISE_IOR, PREC_BITWISE_IOR, 0},
    {"^",  BINOP_BITWISE_XOR, PREC_BITWISE_XOR, 0},
    {"&",  BINOP_BITWISE_AND, PREC_BITWISE_AND, 0},
    {"==", BINOP_EQUAL, PREC_EQUAL, 0},
    {"!=", BINOP_NOTEQUAL, PREC_EQUAL, 0},
    {"<=", BINOP_LEQ, PREC_ORDER, 0},
    {">=", BINOP_GEQ, PREC_ORDER, 0},
    {">",  BINOP_GTR, PREC_ORDER, 0},
    {"<",  BINOP_LESS, PREC_ORDER, 0},
    {">>", BINOP_RSH, PREC_SHIFT, 0},
    {"<<", BINOP_LSH, PREC_SHIFT, 0},
#if 0
    {">>>", BINOP_???, PREC_SHIFT, 0},
#endif
    {"+",  BINOP_ADD, PREC_ADD, 0},
    {"-",  BINOP_SUB, PREC_ADD, 0},
    {"*",  BINOP_MUL, PREC_MUL, 0},
    {"/",  BINOP_DIV, PREC_MUL, 0},
    {"%",  BINOP_REM, PREC_MUL, 0},
    {"-",  UNOP_NEG, PREC_PREFIX, 0},
    {"!",  UNOP_LOGICAL_NOT, PREC_PREFIX, 0},
    {"~",  UNOP_COMPLEMENT, PREC_PREFIX, 0},
    {"*",  UNOP_IND, PREC_PREFIX, 0},
#if 0
    {"instanceof", ???, ???, 0},
#endif
    {"++", UNOP_PREINCREMENT, PREC_PREFIX, 0},
    {"--", UNOP_PREDECREMENT, PREC_PREFIX, 0},
    {NULL, 0, 0, 0}
};

const struct language_defn java_language_defn = {
  "java",				/* Language name */
  language_java,
  c_builtin_types,
  range_check_off,
  type_check_off,
  java_parse,
  java_error,
  evaluate_subexp_java,
  java_printchar,		/* Print a character constant */
  c_printstr,			/* Function to print string constant */
  java_create_fundamental_type,	/* Create fundamental type in this language */
  java_print_type,		/* Print a type using appropriate syntax */
  java_val_print,		/* Print a value using appropriate syntax */
  java_value_print,		/* Print a top-level value */
  {"",      "",    "",   ""},	/* Binary format info */
  {"0%lo",   "0",   "o",  ""},	/* Octal format info */
  {"%ld",    "",    "d",  ""},	/* Decimal format info */
  {"0x%lx",  "0x",  "x",  ""},	/* Hex format info */
  java_op_print_tab,		/* expression operators for printing */
  0,				/* not c-style arrays */
  0,				/* String lower bound */
  &builtin_type_char,		/* Type of string elements */ 
  LANG_MAGIC
};

void
_initialize_java_language ()
{

  java_int_type    = init_type (TYPE_CODE_INT,  4, 0, "int", NULL);
  java_short_type  = init_type (TYPE_CODE_INT,  2, 0, "short", NULL);
  java_long_type   = init_type (TYPE_CODE_INT,  8, 0, "long", NULL);
  java_byte_type   = init_type (TYPE_CODE_INT,  1, 0, "byte", NULL);
  java_boolean_type= init_type (TYPE_CODE_BOOL, 1, 0, "boolean", NULL);
  java_char_type   = init_type (TYPE_CODE_CHAR, 2, 0, "char", NULL);
  java_float_type  = init_type (TYPE_CODE_FLT,  4, 0, "float", NULL);
  java_double_type = init_type (TYPE_CODE_FLT,  8, 0, "double", NULL);
  java_void_type   = init_type (TYPE_CODE_VOID, 1, 0, "void", NULL);

  add_language (&java_language_defn);
}

/* Cleanup code that should be run on every "run".
   We should use make_run_cleanup to have this be called.
   But will that mess up values in value histry?  FIXME */

void java_rerun_cleanup ()
{
  if (class_symtab != NULL)
    {
      free_symtab (class_symtab); /* ??? */
      class_symtab = NULL;
    }
  if (dynamics_objfile != NULL)
    {
      free_objfile (dynamics_objfile);
      dynamics_objfile = NULL;
    }

  java_object_type = NULL;
}
