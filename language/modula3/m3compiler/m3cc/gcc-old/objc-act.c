/* Implement classes and message passing for Objective C.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Steve Naroff.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Purpose: This module implements the Objective-C 4.0 language.

   compatibility issues (with the Stepstone translator):

   - does not recognize the following 3.3 constructs.
     @requires, @classes, @messages, = (...)
   - methods with variable arguments must conform to ANSI standard.
   - tagged structure definitions that appear in BOTH the interface
     and implementation are not allowed.
   - public/private: all instance variables are public within the
     context of the implementation...I consider this to be a bug in
     the translator.
   - statically allocated objects are not supported. the user will
     receive an error if this service is requested.

   code generation `options':

   - OBJC_INT_SELECTORS  */

#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "c-tree.h"
#include "c-lex.h"
#include "flags.h"
#include "objc-act.h"
#include "input.h"
#include "except.h"
#include "function.h"


/* This is the default way of generating a method name.  */
/* I am not sure it is really correct.
   Perhaps there's a danger that it will make name conflicts
   if method names contain underscores. -- rms.  */
#ifndef OBJC_GEN_METHOD_LABEL
#define OBJC_GEN_METHOD_LABEL(BUF, IS_INST, CLASS_NAME, CAT_NAME, SEL_NAME, NUM) \
  do {					    \
    char *temp;				    \
    sprintf ((BUF), "_%s_%s_%s_%s",	    \
	     ((IS_INST) ? "i" : "c"),	    \
	     (CLASS_NAME),		    \
	     ((CAT_NAME)? (CAT_NAME) : ""), \
	     (SEL_NAME));		    \
    for (temp = (BUF); *temp; temp++)	    \
      if (*temp == ':') *temp = '_';	    \
  } while (0)
#endif

/* These need specifying.  */
#ifndef OBJC_FORWARDING_STACK_OFFSET
#define OBJC_FORWARDING_STACK_OFFSET 0
#endif

#ifndef OBJC_FORWARDING_MIN_OFFSET
#define OBJC_FORWARDING_MIN_OFFSET 0
#endif

/* Define the special tree codes that we use.  */

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See objc-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *objc_tree_code_type[] = {
  "x",
#include "objc-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int objc_tree_code_length[] = {
  0,
#include "objc-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *objc_tree_code_name[] = {
  "@@dummy",
#include "objc-tree.def"
};
#undef DEFTREECODE

/* Set up for use of obstacks.  */

#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* This obstack is used to accumulate the encoding of a data type.  */
static struct obstack util_obstack;
/* This points to the beginning of obstack contents,
   so we can free the whole contents.  */
char *util_firstobj;

/* List of classes with list of their static instances.  */
static tree objc_static_instances;

/* The declaration of the array administrating the static instances.  */
static tree static_instances_decl;

/* for encode_method_def */
#include "rtl.h"
#include "c-parse.h"

#define OBJC_VERSION	(flag_next_runtime ? 5 : 7)
#define PROTOCOL_VERSION 2

#define OBJC_ENCODE_INLINE_DEFS 	0
#define OBJC_ENCODE_DONT_INLINE_DEFS	1

/*** Private Interface (procedures) ***/

/* Used by compile_file.  */

static void init_objc				PROTO((void));
static void finish_objc				PROTO((void));

/* Code generation.  */

static void synth_module_prologue		PROTO((void));
static tree build_constructor			PROTO((tree, tree));
static char *build_module_descriptor		PROTO((void));
static tree init_module_descriptor		PROTO((tree));
static tree build_objc_method_call		PROTO((int, tree, tree,
						       tree, tree, tree));
static void generate_strings			PROTO((void));
static tree get_proto_encoding 			PROTO((tree));
static void build_selector_translation_table	PROTO((void));
static tree build_ivar_chain			PROTO((tree, int));

static tree objc_add_static_instance		PROTO((tree, tree));

static tree build_ivar_template			PROTO((void));
static tree build_method_template		PROTO((void));
static tree build_private_template		PROTO((tree));
static void build_class_template		PROTO((void));
static void build_selector_template		PROTO((void));
static void build_category_template		PROTO((void));
static tree build_super_template		PROTO((void));
static tree build_category_initializer		PROTO((tree, tree, tree,
						       tree, tree, tree));
static tree build_protocol_initializer		PROTO((tree, tree, tree,
						       tree, tree));

static void synth_forward_declarations		PROTO((void));
static void generate_ivar_lists			PROTO((void));
static void generate_dispatch_tables		PROTO((void));
static void generate_shared_structures		PROTO((void));
static tree generate_protocol_list		PROTO((tree));
static void generate_forward_declaration_to_string_table PROTO((void));
static void build_protocol_reference		PROTO((tree));

static tree init_selector			PROTO((int));
static tree build_keyword_selector		PROTO((tree));
static tree synth_id_with_class_suffix		PROTO((char *, tree));

/* From expr.c */
extern int apply_args_register_offset           PROTO((int));

/* Misc. bookkeeping */

typedef struct hashed_entry 	*hash;
typedef struct hashed_attribute  *attr;

struct hashed_attribute
{
  attr next;
  tree value;
};
struct hashed_entry
{
  attr list;
  hash next;
  tree key;
};

static void hash_init				PROTO((void));
static void hash_enter				PROTO((hash *, tree));
static hash hash_lookup				PROTO((hash *, tree));
static void hash_add_attr			PROTO((hash, tree));
static tree lookup_method			PROTO((tree, tree));
static tree lookup_instance_method_static	PROTO((tree, tree));
static tree lookup_class_method_static		PROTO((tree, tree));
static tree add_class				PROTO((tree));
static void add_category			PROTO((tree, tree));

enum string_section
{
  class_names,		/* class, category, protocol, module names */
  meth_var_names,	/* method and variable names */
  meth_var_types	/* method and variable type descriptors */
};

static tree add_objc_string			PROTO((tree,
						       enum string_section));
static tree build_objc_string_decl		PROTO((tree,
						       enum string_section));
static tree build_selector_reference_decl	PROTO((tree));

/* Protocol additions.  */

static tree add_protocol			PROTO((tree));
static tree lookup_protocol			PROTO((tree));
static tree lookup_and_install_protocols	PROTO((tree));

/* Type encoding.  */

static void encode_type_qualifiers		PROTO((tree));
static void encode_pointer			PROTO((tree, int, int));
static void encode_array			PROTO((tree, int, int));
static void encode_aggregate			PROTO((tree, int, int));
static void encode_bitfield			PROTO((int, int));
static void encode_type				PROTO((tree, int, int));
static void encode_field_decl			PROTO((tree, int, int));

static void really_start_method			PROTO((tree, tree));
static int comp_method_with_proto		PROTO((tree, tree));
static int comp_proto_with_proto		PROTO((tree, tree));
static tree get_arg_type_list			PROTO((tree, int, int));
static tree expr_last				PROTO((tree));

/* Utilities for debugging and error diagnostics.  */

static void warn_with_method			PROTO((char *, int, tree));
static void error_with_ivar			PROTO((char *, tree, tree));
static char *gen_method_decl			PROTO((tree, char *));
static char *gen_declaration			PROTO((tree, char *));
static char *gen_declarator			PROTO((tree, char *, char *));
static int is_complex_decl			PROTO((tree));
static void adorn_decl				PROTO((tree, char *));
static void dump_interface			PROTO((FILE *, tree));

/* Everything else.  */

static void objc_fatal				PROTO((void));
static tree define_decl				PROTO((tree, tree));
static tree lookup_method_in_protocol_list	PROTO((tree, tree, int));
static tree lookup_protocol_in_reflist		PROTO((tree, tree));
static tree create_builtin_decl			PROTO((enum tree_code,
						       tree, char *));
static tree my_build_string			PROTO((int, char *));
static void build_objc_symtab_template		PROTO((void));
static tree init_def_list			PROTO((tree));
static tree init_objc_symtab			PROTO((tree));
static void forward_declare_categories		PROTO((void));
static void generate_objc_symtab_decl		PROTO((void));
static tree build_selector			PROTO((tree));
static tree build_msg_pool_reference		PROTO((int));
static tree build_typed_selector_reference     	PROTO((tree, tree));
static tree build_selector_reference		PROTO((tree));
static tree build_class_reference_decl		PROTO((tree));
static void add_class_reference			PROTO((tree));
static tree objc_copy_list			PROTO((tree, tree *));
static tree build_protocol_template		PROTO((void));
static tree build_descriptor_table_initializer	PROTO((tree, tree));
static tree build_method_prototype_list_template PROTO((tree, int));
static tree build_method_prototype_template	PROTO((void));
static int forwarding_offset			PROTO((tree));
static tree encode_method_prototype		PROTO((tree, tree));
static tree generate_descriptor_table		PROTO((tree, char *, int, tree, tree));
static void generate_method_descriptors		PROTO((tree));
static tree build_tmp_function_decl		PROTO((void));
static void hack_method_prototype		PROTO((tree, tree));
static void generate_protocol_references	PROTO((tree));
static void generate_protocols			PROTO((void));
static void check_ivars				PROTO((tree, tree));
static tree build_ivar_list_template		PROTO((tree, int));
static tree build_method_list_template		PROTO((tree, int));
static tree build_ivar_list_initializer		PROTO((tree, tree));
static tree generate_ivars_list			PROTO((tree, char *,
						       int, tree));
static tree build_dispatch_table_initializer	PROTO((tree, tree));
static tree generate_dispatch_table		PROTO((tree, char *,
						       int, tree));
static tree build_shared_structure_initializer	PROTO((tree, tree, tree, tree,
						       tree, int, tree, tree,
						       tree));
static void generate_category			PROTO((tree));
static int is_objc_type_qualifier		PROTO((tree));
static tree adjust_type_for_id_default		PROTO((tree));
static tree check_duplicates			PROTO((hash));
static tree receiver_is_class_object		PROTO((tree));
static int check_methods			PROTO((tree, tree, int));
static int conforms_to_protocol			PROTO((tree, tree));
static void check_protocols			PROTO((tree, char *, char *));
static tree encode_method_def			PROTO((tree));
static void gen_declspecs			PROTO((tree, char *, int));
static void generate_classref_translation_entry	PROTO((tree));
static void handle_class_ref			PROTO((tree));

/*** Private Interface (data) ***/

/* Reserved tag definitions.  */

#define TYPE_ID			"id"
#define TAG_OBJECT		"objc_object"
#define TAG_CLASS		"objc_class"
#define TAG_SUPER		"objc_super"
#define TAG_SELECTOR		"objc_selector"

#define UTAG_CLASS		"_objc_class"
#define UTAG_IVAR		"_objc_ivar"
#define UTAG_IVAR_LIST		"_objc_ivar_list"
#define UTAG_METHOD		"_objc_method"
#define UTAG_METHOD_LIST	"_objc_method_list"
#define UTAG_CATEGORY		"_objc_category"
#define UTAG_MODULE		"_objc_module"
#define UTAG_STATICS		"_objc_statics"
#define UTAG_SYMTAB		"_objc_symtab"
#define UTAG_SUPER		"_objc_super"
#define UTAG_SELECTOR		"_objc_selector"

#define UTAG_PROTOCOL		"_objc_protocol"
#define UTAG_PROTOCOL_LIST	"_objc_protocol_list"
#define UTAG_METHOD_PROTOTYPE	"_objc_method_prototype"
#define UTAG_METHOD_PROTOTYPE_LIST "_objc__method_prototype_list"

#define STRING_OBJECT_CLASS_NAME "NXConstantString"
#define PROTOCOL_OBJECT_CLASS_NAME "Protocol"

static char *TAG_GETCLASS;
static char *TAG_GETMETACLASS;
static char *TAG_MSGSEND;
static char *TAG_MSGSENDSUPER;
static char *TAG_EXECCLASS;

/* Set by `continue_class' and checked by `is_public'.  */

#define TREE_STATIC_TEMPLATE(record_type) (TREE_PUBLIC (record_type))
#define TYPED_OBJECT(type) \
       (TREE_CODE (type) == RECORD_TYPE && TREE_STATIC_TEMPLATE (type))

/* Some commonly used instances of "identifier_node".  */

static tree self_id, ucmd_id;
static tree unused_list;

static tree self_decl, umsg_decl, umsg_super_decl;
static tree objc_get_class_decl, objc_get_meta_class_decl;

static tree super_type, selector_type, id_type, objc_class_type;
static tree instance_type, protocol_type;

/* Type checking macros.  */

#define IS_ID(TYPE) \
  (TYPE_MAIN_VARIANT (TYPE) == TYPE_MAIN_VARIANT (id_type))
#define IS_PROTOCOL_QUALIFIED_ID(TYPE) \
  (IS_ID (TYPE) && TYPE_PROTOCOL_LIST (TYPE))
#define IS_SUPER(TYPE) \
  (super_type && TYPE_MAIN_VARIANT (TYPE) == TYPE_MAIN_VARIANT (super_type))

static tree class_chain = NULL_TREE;
static tree alias_chain = NULL_TREE;
static tree interface_chain = NULL_TREE;
static tree protocol_chain = NULL_TREE;

/* Chains to manage selectors that are referenced and defined in the
   module.  */

static tree cls_ref_chain = NULL_TREE;	/* Classes referenced.  */
static tree sel_ref_chain = NULL_TREE;	/* Selectors referenced.  */

/* Chains to manage uniquing of strings.  */

static tree class_names_chain = NULL_TREE;
static tree meth_var_names_chain = NULL_TREE;
static tree meth_var_types_chain = NULL_TREE;

/* Hash tables to manage the global pool of method prototypes.  */

static hash *nst_method_hash_list = 0;
static hash *cls_method_hash_list = 0;

/* Backend data declarations.  */

static tree UOBJC_SYMBOLS_decl;
static tree UOBJC_INSTANCE_VARIABLES_decl, UOBJC_CLASS_VARIABLES_decl;
static tree UOBJC_INSTANCE_METHODS_decl, UOBJC_CLASS_METHODS_decl;
static tree UOBJC_CLASS_decl, UOBJC_METACLASS_decl;
static tree UOBJC_SELECTOR_TABLE_decl;
static tree UOBJC_MODULES_decl;
static tree UOBJC_STRINGS_decl;

/* The following are used when compiling a class implementation.
   implementation_template will normally be an interface, however if
   none exists this will be equal to implementation_context...it is
   set in start_class.  */

static tree implementation_context = NULL_TREE;
static tree implementation_template = NULL_TREE;

struct imp_entry
{
  struct imp_entry *next;
  tree imp_context;
  tree imp_template;
  tree class_decl;		/* _OBJC_CLASS_<my_name>; */
  tree meta_decl;		/* _OBJC_METACLASS_<my_name>; */
};

static void handle_impent			PROTO((struct imp_entry *));

static struct imp_entry *imp_list = 0;
static int imp_count = 0;	/* `@implementation' */
static int cat_count = 0;	/* `@category' */

static tree objc_class_template, objc_category_template, uprivate_record;
static tree objc_protocol_template, objc_selector_template;
static tree ucls_super_ref, uucls_super_ref;

static tree objc_method_template, objc_ivar_template;
static tree objc_symtab_template, objc_module_template;
static tree objc_super_template, objc_object_reference;

static tree objc_object_id, objc_class_id, objc_id_id;
static tree constant_string_id;
static tree constant_string_type;
static tree UOBJC_SUPER_decl;

static tree method_context = NULL_TREE;
static int  method_slot = 0;	/* Used by start_method_def, */

#define BUFSIZE		1024

static char *errbuf;	/* Buffer for error diagnostics */

/* Data imported from tree.c.  */

extern enum debug_info_type write_symbols;

/* Data imported from toplev.c.  */

extern char *dump_base_name;

/* Generate code for GNU or NeXT runtime environment.  */

#ifdef NEXT_OBJC_RUNTIME
int flag_next_runtime = 1;
#else
int flag_next_runtime = 0;
#endif

int flag_typed_selectors;

/* Open and close the file for outputting class declarations, if requested.  */

int flag_gen_declaration = 0;

FILE *gen_declaration_file;

/* Warn if multiple methods are seen for the same selector, but with
   different argument types.  */

int warn_selector = 0;

/* Warn if methods required by a protocol are not implemented in the 
   class adopting it.  When turned off, methods inherited to that
   class are also considered implemented */

int flag_warn_protocol = 1;

/* Tells "encode_pointer/encode_aggregate" whether we are generating
   type descriptors for instance variables (as opposed to methods).
   Type descriptors for instance variables contain more information
   than methods (for static typing and embedded structures). This
   was added to support features being planned for dbkit2.  */

static int generating_instance_variables = 0;

void
lang_init ()
{
  /* The beginning of the file is a new line; check for #.
     With luck, we discover the real source file's name from that
     and put it in input_filename.  */
  ungetc (check_newline (), finput);

  /* The line number can be -1 if we had -g3 and the input file
     had a directive specifying line 0.  But we want predefined
     functions to have a line number of 0, not -1.  */
  if (lineno == -1)
    lineno = 0;

  /* If gen_declaration desired, open the output file.  */
  if (flag_gen_declaration)
    {
      int dump_base_name_length = strlen (dump_base_name);
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".decl");
      gen_declaration_file = fopen (dumpname, "w");
      if (gen_declaration_file == 0)
	pfatal_with_name (dumpname);
    }

  if (flag_next_runtime)
    {
      TAG_GETCLASS = "objc_getClass";
      TAG_GETMETACLASS = "objc_getMetaClass";
      TAG_MSGSEND = "objc_msgSend";
      TAG_MSGSENDSUPER = "objc_msgSendSuper";
      TAG_EXECCLASS = "__objc_execClass";
    }
  else
    {
      TAG_GETCLASS = "objc_get_class";
      TAG_GETMETACLASS = "objc_get_meta_class";
      TAG_MSGSEND = "objc_msg_lookup";
      TAG_MSGSENDSUPER = "objc_msg_lookup_super";
      TAG_EXECCLASS = "__objc_exec_class";
      flag_typed_selectors = 1;
    }

  if (doing_objc_thang)
    init_objc ();
}

static void
objc_fatal ()
{
  fatal ("Objective-C text in C source file");
}

void
finish_file ()
{
  if (doing_objc_thang)
    finish_objc ();		/* Objective-C finalization */

  if (gen_declaration_file)
    fclose (gen_declaration_file);
}

void
lang_finish ()
{
}

char *
lang_identify ()
{
  return "objc";
}

int
lang_decode_option (p)
     char *p;
{
  if (!strcmp (p, "-lang-objc"))
    doing_objc_thang = 1;
  else if (!strcmp (p, "-gen-decls"))
    flag_gen_declaration = 1;
  else if (!strcmp (p, "-Wselector"))
    warn_selector = 1;
  else if (!strcmp (p, "-Wno-selector"))
    warn_selector = 0;
  else if (!strcmp (p, "-Wprotocol"))
    flag_warn_protocol = 1;
  else if (!strcmp (p, "-Wno-protocol"))
    flag_warn_protocol = 0;
  else if (!strcmp (p, "-fgnu-runtime"))
    flag_next_runtime = 0;
  else if (!strcmp (p, "-fno-next-runtime"))
    flag_next_runtime = 0;
  else if (!strcmp (p, "-fno-gnu-runtime"))
    flag_next_runtime = 1;
  else if (!strcmp (p, "-fnext-runtime"))
    flag_next_runtime = 1;
  else
    return c_decode_option (p);

  return 1;
}

static tree
define_decl (declarator, declspecs)
     tree declarator;
     tree declspecs;
{
  tree decl = start_decl (declarator, declspecs, 0, NULL_TREE, NULL_TREE);
  finish_decl (decl, NULL_TREE, NULL_TREE);
  return decl;
}

/* Return 1 if LHS and RHS are compatible types for assignment or
   various other operations.  Return 0 if they are incompatible, and
   return -1 if we choose to not decide.  When the operation is
   REFLEXIVE, check for compatibility in either direction.

   For statically typed objects, an assignment of the form `a' = `b'
   is permitted if:

   `a' is of type "id",
   `a' and `b' are the same class type, or
   `a' and `b' are of class types A and B such that B is a descendant of A.  */

int
maybe_objc_comptypes (lhs, rhs, reflexive)
     tree lhs, rhs;
     int reflexive;
{
  if (doing_objc_thang)
    return objc_comptypes (lhs, rhs, reflexive);
  return -1;
}

static tree
lookup_method_in_protocol_list (rproto_list, sel_name, class_meth)
   tree rproto_list;
   tree sel_name;
   int class_meth;
{
   tree rproto, p;
   tree fnd = 0;

   for (rproto = rproto_list; rproto; rproto = TREE_CHAIN (rproto))
     {
        p = TREE_VALUE (rproto);

	if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	  {
	    if ((fnd = lookup_method (class_meth
				      ? PROTOCOL_CLS_METHODS (p)
				      : PROTOCOL_NST_METHODS (p), sel_name)))
	      ;
	    else if (PROTOCOL_LIST (p))
	      fnd = lookup_method_in_protocol_list (PROTOCOL_LIST (p),
						    sel_name, class_meth);
	  }
	else
	  ; /* An identifier...if we could not find a protocol.  */

	if (fnd)
	  return fnd;
     }

   return 0;
}

static tree
lookup_protocol_in_reflist (rproto_list, lproto)
   tree rproto_list;
   tree lproto;
{
   tree rproto, p;

   /* Make sure the protocol is support by the object on the rhs.  */
   if (TREE_CODE (lproto) == PROTOCOL_INTERFACE_TYPE)
     {
       tree fnd = 0;
       for (rproto = rproto_list; rproto; rproto = TREE_CHAIN (rproto))
	 {
	   p = TREE_VALUE (rproto);

	   if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	     {
	       if (lproto == p)
		 fnd = lproto;

	       else if (PROTOCOL_LIST (p))
		 fnd = lookup_protocol_in_reflist (PROTOCOL_LIST (p), lproto);
	     }

	   if (fnd)
	     return fnd;
	 }
     }
   else
     ; /* An identifier...if we could not find a protocol.  */

   return 0;
}

/* Return 1 if LHS and RHS are compatible types for assignment
   or various other operations.  Return 0 if they are incompatible,
   and return -1 if we choose to not decide.  When the operation
   is REFLEXIVE, check for compatibility in either direction.  */

int
objc_comptypes (lhs, rhs, reflexive)
     tree lhs;
     tree rhs;
     int reflexive;
{
  /* New clause for protocols.  */

  if (TREE_CODE (lhs) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (lhs)) == RECORD_TYPE
      && TREE_CODE (rhs) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (rhs)) == RECORD_TYPE)
    {
      int lhs_is_proto = IS_PROTOCOL_QUALIFIED_ID (lhs);
      int rhs_is_proto = IS_PROTOCOL_QUALIFIED_ID (rhs);

      if (lhs_is_proto)
        {
	  tree lproto, lproto_list = TYPE_PROTOCOL_LIST (lhs);
	  tree rproto, rproto_list;
	  tree p;

	  if (rhs_is_proto)
	    {
	      rproto_list = TYPE_PROTOCOL_LIST (rhs);

	      /* Make sure the protocol is supported by the object
		 on the rhs.  */
	      for (lproto = lproto_list; lproto; lproto = TREE_CHAIN (lproto))
		{
		  p = TREE_VALUE (lproto);
		  rproto = lookup_protocol_in_reflist (rproto_list, p);

		  if (!rproto)
		    warning ("object does not conform to the `%s' protocol",
			     IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
		}
	    }
	  else if (TYPED_OBJECT (TREE_TYPE (rhs)))
	    {
	      tree rname = TYPE_NAME (TREE_TYPE (rhs));
	      tree rinter;

	      /* Make sure the protocol is supported by the object
		 on the rhs.  */
	      for (lproto = lproto_list; lproto; lproto = TREE_CHAIN (lproto))
		{
		  p = TREE_VALUE (lproto);
		  rproto = 0;
		  rinter = lookup_interface (rname);

		  while (rinter && !rproto)
		    {
		      tree cat;

		      rproto_list = CLASS_PROTOCOL_LIST (rinter);
		      rproto = lookup_protocol_in_reflist (rproto_list, p);

		      /* Check for protocols adopted by categories.  */
		      cat = CLASS_CATEGORY_LIST (rinter);
		      while (cat && !rproto)
			{
			  rproto_list = CLASS_PROTOCOL_LIST (cat);
			  rproto = lookup_protocol_in_reflist (rproto_list, p);

			  cat = CLASS_CATEGORY_LIST (cat);
			}

		      rinter = lookup_interface (CLASS_SUPER_NAME (rinter));
		    }

		  if (!rproto)
		    warning ("class `%s' does not implement the `%s' protocol",
	                     IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (rhs))),
		             IDENTIFIER_POINTER (PROTOCOL_NAME (p)));
		}
	    }

	  /* May change...based on whether there was any mismatch */
          return 1;
        }
      else if (rhs_is_proto)
	/* Lhs is not a protocol...warn if it is statically typed */
	return (TYPED_OBJECT (TREE_TYPE (lhs)) != 0);

      else
	/* Defer to comptypes .*/
	return -1;
    }

  else if (TREE_CODE (lhs) == RECORD_TYPE && TREE_CODE (rhs) == RECORD_TYPE)
    ; /* Fall thru.  This is the case we have been handling all along */
  else
    /* Defer to comptypes.  */
    return -1;

  /* `id' = `<class> *', `<class> *' = `id' */

  if ((TYPE_NAME (lhs) == objc_object_id && TYPED_OBJECT (rhs))
      || (TYPE_NAME (rhs) == objc_object_id && TYPED_OBJECT (lhs)))
    return 1;

  /* `id' = `Class', `Class' = `id' */

  else if ((TYPE_NAME (lhs) == objc_object_id
	    && TYPE_NAME (rhs) == objc_class_id)
	   || (TYPE_NAME (lhs) == objc_class_id
	       && TYPE_NAME (rhs) == objc_object_id))
    return 1;

  /* `<class> *' = `<class> *' */

  else if (TYPED_OBJECT (lhs) && TYPED_OBJECT (rhs))
    {
      tree lname = TYPE_NAME (lhs);
      tree rname = TYPE_NAME (rhs);
      tree inter;

      if (lname == rname)
	return 1;

      /* If the left hand side is a super class of the right hand side,
	 allow it.  */
      for (inter = lookup_interface (rname); inter;
	   inter = lookup_interface (CLASS_SUPER_NAME (inter)))
	if (lname == CLASS_SUPER_NAME (inter))
	  return 1;

      /* Allow the reverse when reflexive.  */
      if (reflexive)
	for (inter = lookup_interface (lname); inter;
	     inter = lookup_interface (CLASS_SUPER_NAME (inter)))
	  if (rname == CLASS_SUPER_NAME (inter))
	    return 1;

      return 0;
    }
  else
    /* Defer to comptypes.  */
    return -1;
}

/* Called from c-decl.c before all calls to rest_of_decl_compilation.  */

void
objc_check_decl (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == RECORD_TYPE
      && TREE_STATIC_TEMPLATE (type)
      && type != constant_string_type)
    {
      error_with_decl (decl, "`%s' cannot be statically allocated");
      fatal ("statically allocated objects not supported");
    }
}

void
maybe_objc_check_decl (decl)
     tree decl;
{
  if (doing_objc_thang)
    objc_check_decl (decl);
}

/* Implement static typing.  At this point, we know we have an interface.  */

tree
get_static_reference (interface, protocols)
     tree interface;
     tree protocols;
{
  tree type = xref_tag (RECORD_TYPE, interface);

  if (protocols)
    {
      tree t, m = TYPE_MAIN_VARIANT (type);

      push_obstacks_nochange ();
      end_temporary_allocation ();
      t = copy_node (type);
      TYPE_BINFO (t) = make_tree_vec (2);
      pop_obstacks ();

      /* Add this type to the chain of variants of TYPE.  */
      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
      TYPE_NEXT_VARIANT (m) = t;

      /* Look up protocols and install in lang specific list.  */
      TYPE_PROTOCOL_LIST (t) = lookup_and_install_protocols (protocols);

      /* This forces a new pointer type to be created later
	 (in build_pointer_type)...so that the new template
	 we just created will actually be used...what a hack!  */
      if (TYPE_POINTER_TO (t))
	TYPE_POINTER_TO (t) = 0;

      type = t;
    }

  return type;
}

tree
get_object_reference (protocols)
     tree protocols;
{
  tree type_decl = lookup_name (objc_id_id);
  tree type;

  if (type_decl && TREE_CODE (type_decl) == TYPE_DECL)
    {
      type = TREE_TYPE (type_decl);
      if (TYPE_MAIN_VARIANT (type) != id_type)
	warning ("Unexpected type for `id' (%s)",
		gen_declaration (type, errbuf));
    }
  else
    fatal ("Undefined type `id', please import <objc/objc.h>");

  /* This clause creates a new pointer type that is qualified with
     the protocol specification...this info is used later to do more
     elaborate type checking.  */

  if (protocols)
    {
      tree t, m = TYPE_MAIN_VARIANT (type);

      push_obstacks_nochange ();
      end_temporary_allocation ();
      t = copy_node (type);
      TYPE_BINFO (t) = make_tree_vec (2);
      pop_obstacks ();

      /* Add this type to the chain of variants of TYPE.  */
      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
      TYPE_NEXT_VARIANT (m) = t;

      /* Look up protocols...and install in lang specific list */
      TYPE_PROTOCOL_LIST (t) = lookup_and_install_protocols (protocols);

      /* This forces a new pointer type to be created later
	 (in build_pointer_type)...so that the new template
	 we just created will actually be used...what a hack!  */
      if (TYPE_POINTER_TO (t))
	TYPE_POINTER_TO (t) = NULL;

      type = t;
    }
  return type;
}

static tree
lookup_and_install_protocols (protocols)
     tree protocols;
{
  tree proto;
  tree prev = NULL;
  tree return_value = protocols;

  for (proto = protocols; proto; proto = TREE_CHAIN (proto))
    {
      tree ident = TREE_VALUE (proto);
      tree p = lookup_protocol (ident);

      if (!p)
	{
	  error ("Cannot find protocol declaration for `%s'",
		 IDENTIFIER_POINTER (ident));
	  if (prev)
	    TREE_CHAIN (prev) = TREE_CHAIN (proto);
	  else
	    return_value = TREE_CHAIN (proto);
	}
      else
	{
	  /* Replace identifier with actual protocol node.  */
	  TREE_VALUE (proto) = p;
	  prev = proto;
	}
    }

  return return_value;
}

/* Create and push a decl for a built-in external variable or field NAME.
   CODE says which.
   TYPE is its data type.  */

static tree
create_builtin_decl (code, type, name)
     enum tree_code code;
     tree type;
     char *name;
{
  tree decl = build_decl (code, get_identifier (name), type);

  if (code == VAR_DECL)
    {
      TREE_STATIC (decl) = 1;
      make_decl_rtl (decl, 0, 1);
      pushdecl (decl);
    }

  DECL_ARTIFICIAL (decl) = 1;
  return decl;
}

/* Purpose: "play" parser, creating/installing representations
   of the declarations that are required by Objective-C.

   Model:

 	type_spec--------->sc_spec
 	(tree_list)        (tree_list)
 	    |                  |
 	    |                  |
 	identifier_node    identifier_node  */

static void
synth_module_prologue ()
{
  tree temp_type;
  tree super_p;

  /* Defined in `objc.h' */
  objc_object_id = get_identifier (TAG_OBJECT);

  objc_object_reference = xref_tag (RECORD_TYPE, objc_object_id);

  id_type = build_pointer_type (objc_object_reference);

  objc_id_id = get_identifier (TYPE_ID);
  objc_class_id = get_identifier (TAG_CLASS);

  objc_class_type = build_pointer_type (xref_tag (RECORD_TYPE, objc_class_id));
  protocol_type = build_pointer_type (xref_tag (RECORD_TYPE,
				get_identifier (PROTOCOL_OBJECT_CLASS_NAME)));

  /* Declare type of selector-objects that represent an operation name.  */

#ifdef OBJC_INT_SELECTORS
  /* `unsigned int' */
  selector_type = unsigned_type_node;
#else
  /* `struct objc_selector *' */
  selector_type
    = build_pointer_type (xref_tag (RECORD_TYPE,
				    get_identifier (TAG_SELECTOR)));
#endif /* not OBJC_INT_SELECTORS */

  /* Forward declare type, or else the prototype for msgSendSuper will
     complain.  */

  super_p = build_pointer_type (xref_tag (RECORD_TYPE,
					  get_identifier (TAG_SUPER)));


  /* id objc_msgSend (id, SEL, ...); */

  temp_type
    = build_function_type (id_type,
			   tree_cons (NULL_TREE, id_type,
				      tree_cons (NULL_TREE, selector_type,
						 NULL_TREE)));

  if (! flag_next_runtime)
    {
      umsg_decl = build_decl (FUNCTION_DECL,
			      get_identifier (TAG_MSGSEND), temp_type);
      DECL_EXTERNAL (umsg_decl) = 1;
      TREE_PUBLIC (umsg_decl) = 1;
      DECL_INLINE (umsg_decl) = 1;
      DECL_ARTIFICIAL (umsg_decl) = 1;

      if (flag_traditional && TAG_MSGSEND[0] != '_')
	DECL_BUILT_IN_NONANSI (umsg_decl) = 1;

      make_decl_rtl (umsg_decl, NULL_PTR, 1);
      pushdecl (umsg_decl);
    }
  else
    umsg_decl = builtin_function (TAG_MSGSEND, temp_type, NOT_BUILT_IN, 0);

  /* id objc_msgSendSuper (struct objc_super *, SEL, ...); */

  temp_type
    = build_function_type (id_type,
			   tree_cons (NULL_TREE, super_p,
				      tree_cons (NULL_TREE, selector_type,
						 NULL_TREE)));

  umsg_super_decl = builtin_function (TAG_MSGSENDSUPER,
				     temp_type, NOT_BUILT_IN, 0);

  /* id objc_getClass (const char *); */

  temp_type = build_function_type (id_type,
			tree_cons (NULL_TREE,
				   const_string_type_node,
				   tree_cons (NULL_TREE, void_type_node,
					      NULL_TREE)));

  objc_get_class_decl
    = builtin_function (TAG_GETCLASS, temp_type, NOT_BUILT_IN, 0);

  /* id objc_getMetaClass (const char *); */

  objc_get_meta_class_decl
    = builtin_function (TAG_GETMETACLASS, temp_type, NOT_BUILT_IN, 0);

  /* static SEL _OBJC_SELECTOR_TABLE[]; */

  if (! flag_next_runtime)
    {
      if (flag_typed_selectors)
	{
	  /* Suppress outputting debug symbols, because
	     dbxout_init hasn'r been called yet.  */
	  enum debug_info_type save_write_symbols = write_symbols;
	  write_symbols = NO_DEBUG;

	  build_selector_template ();
	  temp_type = build_array_type (objc_selector_template, NULL_TREE);

	  write_symbols = save_write_symbols;
	}
      else
	temp_type = build_array_type (selector_type, NULL_TREE);

      layout_type (temp_type);
      UOBJC_SELECTOR_TABLE_decl
	= create_builtin_decl (VAR_DECL, temp_type,
			       "_OBJC_SELECTOR_TABLE");

      /* Avoid warning when not sending messages.  */
      TREE_USED (UOBJC_SELECTOR_TABLE_decl) = 1;
    }

  generate_forward_declaration_to_string_table ();

  /* Forward declare constant_string_id and constant_string_type.  */
  constant_string_id = get_identifier (STRING_OBJECT_CLASS_NAME);
  constant_string_type = xref_tag (RECORD_TYPE, constant_string_id);
}

/* Custom build_string which sets TREE_TYPE!  */

static tree
my_build_string (len, str)
     int len;
     char *str;
{
  int wide_flag = 0;
  tree a_string = build_string (len, str);

  /* Some code from combine_strings, which is local to c-parse.y.  */
  if (TREE_TYPE (a_string) == int_array_type_node)
    wide_flag = 1;

  TREE_TYPE (a_string)
    = build_array_type (wide_flag ? integer_type_node : char_type_node,
			build_index_type (build_int_2 (len - 1, 0)));

  TREE_CONSTANT (a_string) = 1;	/* Puts string in the readonly segment */
  TREE_STATIC (a_string) = 1;

  return a_string;
}

/* Return a newly constructed OBJC_STRING_CST node whose value is
   the LEN characters at STR.
   The TREE_TYPE is not initialized.  */

tree
build_objc_string (len, str)
     int len;
     char *str;
{
  tree s = build_string (len, str);

  TREE_SET_CODE (s, OBJC_STRING_CST);
  return s;
}

/* Given a chain of OBJC_STRING_CST's, build a static instance of
   NXConstanString which points at the concatenation of those strings.
   We place the string object in the __string_objects section of the
   __OBJC segment.  The Objective-C runtime will initialize the isa
   pointers of the string objects to point at the NXConstandString class
   object.  */

tree
build_objc_string_object (strings)
     tree strings;
{
  tree string, initlist, constructor;
  int length;

  if (!doing_objc_thang)
    objc_fatal ();

  if (lookup_interface (constant_string_id) == NULL_TREE)
    {
      error ("Cannot find interface declaration for `%s'",
	     IDENTIFIER_POINTER (constant_string_id));
      return error_mark_node;
    }

  add_class_reference (constant_string_id);

  /* Combine_strings will work for OBJC_STRING_CST's too.  */
  string = combine_strings (strings);
  TREE_SET_CODE (string, STRING_CST);
  length = TREE_STRING_LENGTH (string) - 1;

  if (! flag_next_runtime)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      if (! TREE_PERMANENT (strings))
	string = my_build_string (length + 1,
				  TREE_STRING_POINTER (string));
    }

  /* & ((NXConstantString) {0, string, length})  */

  initlist = build_tree_list (NULL_TREE, build_int_2 (0, 0));
  initlist
    = tree_cons (NULL_TREE, copy_node (build_unary_op (ADDR_EXPR, string, 1)),
		 initlist);
  initlist = tree_cons (NULL_TREE, build_int_2 (length, 0), initlist);
  constructor = build_constructor (constant_string_type, nreverse (initlist));

  if (!flag_next_runtime)
    {
      constructor
	= objc_add_static_instance (constructor, constant_string_type);
      pop_obstacks ();
    }

  return (build_unary_op (ADDR_EXPR, constructor, 1));
}

/* Declare a static instance of CLASS_DECL initialized by CONSTRUCTOR.  */

static tree
objc_add_static_instance (constructor, class_decl)
     tree constructor, class_decl;
{
  static int num_static_inst;
  tree *chain, decl, decl_spec, decl_expr;
  char buf[256];

  push_obstacks_nochange ();
  end_temporary_allocation ();

  /* Find the list of static instances for the CLASS_DECL.  Create one if
     not found.  */
  for (chain = &objc_static_instances;
       *chain && TREE_VALUE (*chain) != class_decl;
       chain = &TREE_CHAIN (*chain));
  if (!*chain)
    {
      *chain = tree_cons (NULL_TREE, class_decl, NULL_TREE);
      add_objc_string (TYPE_NAME (class_decl), class_names);
    }

  sprintf (buf, "_OBJC_INSTANCE_%d", num_static_inst++);
  decl = build_decl (VAR_DECL, get_identifier (buf), class_decl);
  DECL_COMMON (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  pushdecl_top_level (decl);
  rest_of_decl_compilation (decl, 0, 1, 0);

  /* Do this here so it gets output later instead of possibly
     inside something else we are writing.  */
  DECL_INITIAL (decl) = constructor;

  /* Add the DECL to the head of this CLASS' list.  */
  TREE_PURPOSE (*chain) = tree_cons (NULL_TREE, decl, TREE_PURPOSE (*chain));

  pop_obstacks ();
  return decl;
}

/* Build a static constant CONSTRUCTOR
   with type TYPE and elements ELTS.  */

static tree
build_constructor (type, elts)
     tree type, elts;
{
  tree constructor = build (CONSTRUCTOR, type, NULL_TREE, elts);

  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;
  TREE_READONLY (constructor) = 1;

  return constructor;
}

/* Take care of defining and initializing _OBJC_SYMBOLS.  */

/* Predefine the following data type:

   struct _objc_symtab
   {
     long sel_ref_cnt;
     SEL *refs;
     short cls_def_cnt;
     short cat_def_cnt;
     void *defs[cls_def_cnt + cat_def_cnt];
   }; */

static void
build_objc_symtab_template ()
{
  tree field_decl, field_decl_chain, index;

  objc_symtab_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_SYMTAB));

  /* long sel_ref_cnt; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    long_integer_type_node,
				    "sel_ref_cnt");
  field_decl_chain = field_decl;

  /* SEL *refs; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    build_pointer_type (selector_type),
				    "refs");
  chainon (field_decl_chain, field_decl);

  /* short cls_def_cnt; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    short_integer_type_node,
				    "cls_def_cnt");
  chainon (field_decl_chain, field_decl);

  /* short cat_def_cnt; */

  field_decl = create_builtin_decl (FIELD_DECL,
				    short_integer_type_node,
				    "cat_def_cnt");
  chainon (field_decl_chain, field_decl);

  /* void *defs[cls_def_cnt + cat_def_cnt]; */

  index = build_index_type (build_int_2 (imp_count + cat_count - 1,
					 imp_count == 0 && cat_count == 0
					 ? -1 : 0));
  field_decl = create_builtin_decl (FIELD_DECL,
				    build_array_type (ptr_type_node, index),
				    "defs");
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_symtab_template, field_decl_chain, NULL_TREE);
}

/* Create the initial value for the `defs' field of _objc_symtab.
   This is a CONSTRUCTOR.  */

static tree
init_def_list (type)
     tree type;
{
  tree expr, initlist = NULL_TREE;
  struct imp_entry *impent;

  if (imp_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
	  {
	    expr = build_unary_op (ADDR_EXPR, impent->class_decl, 0);
	    initlist = tree_cons (NULL_TREE, expr, initlist);
	  }
      }

  if (cat_count)
    for (impent = imp_list; impent; impent = impent->next)
      {
	if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
	  {
	    expr = build_unary_op (ADDR_EXPR, impent->class_decl, 0);
	    initlist = tree_cons (NULL_TREE, expr, initlist);
	  }
      }

  return build_constructor (type, nreverse (initlist));
}

/* Construct the initial value for all of _objc_symtab.  */

static tree
init_objc_symtab (type)
     tree type;
{
  tree initlist;

  /* sel_ref_cnt = { ..., 5, ... } */

  initlist = build_tree_list (NULL_TREE, build_int_2 (0, 0));

  /* refs = { ..., _OBJC_SELECTOR_TABLE, ... } */

  if (flag_next_runtime || ! sel_ref_chain)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    initlist = tree_cons (NULL_TREE,
			  build_unary_op (ADDR_EXPR,
					  UOBJC_SELECTOR_TABLE_decl, 1),
			  initlist);

  /* cls_def_cnt = { ..., 5, ... } */

  initlist = tree_cons (NULL_TREE, build_int_2 (imp_count, 0), initlist);

  /* cat_def_cnt = { ..., 5, ... } */

  initlist = tree_cons (NULL_TREE, build_int_2 (cat_count, 0), initlist);

  /* cls_def = { ..., { &Foo, &Bar, ...}, ... } */

  if (imp_count || cat_count)
    {
      tree field = TYPE_FIELDS (type);
      field = TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (field))));

      initlist = tree_cons (NULL_TREE, init_def_list (TREE_TYPE (field)),
			    initlist);
    }

  return build_constructor (type, nreverse (initlist));
}

/* Push forward-declarations of all the categories
   so that init_def_list can use them in a CONSTRUCTOR.  */

static void
forward_declare_categories ()
{
  struct imp_entry *impent;
  tree sav = implementation_context;

  for (impent = imp_list; impent; impent = impent->next)
    {
      if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
	{
	  /* Set an invisible arg to synth_id_with_class_suffix.  */
	  implementation_context = impent->imp_context;
	  impent->class_decl
	    = create_builtin_decl (VAR_DECL, objc_category_template,
				   IDENTIFIER_POINTER (synth_id_with_class_suffix ("_OBJC_CATEGORY", implementation_context)));
	}
    }
  implementation_context = sav;
}

/* Create the declaration of _OBJC_SYMBOLS, with type `strict _objc_symtab'
   and initialized appropriately.  */

static void
generate_objc_symtab_decl ()
{
  tree sc_spec;

  if (!objc_category_template)
    build_category_template ();

  /* forward declare categories */
  if (cat_count)
    forward_declare_categories ();

  if (!objc_symtab_template)
    build_objc_symtab_template ();

  sc_spec = build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]);

  UOBJC_SYMBOLS_decl = start_decl (get_identifier ("_OBJC_SYMBOLS"),
				   tree_cons (NULL_TREE,
					      objc_symtab_template, sc_spec),
				   1,
				   NULL_TREE, NULL_TREE);

  TREE_USED (UOBJC_SYMBOLS_decl) = 1;
  DECL_IGNORED_P (UOBJC_SYMBOLS_decl) = 1;
  DECL_ARTIFICIAL (UOBJC_SYMBOLS_decl) = 1;
  finish_decl (UOBJC_SYMBOLS_decl,
	       init_objc_symtab (TREE_TYPE (UOBJC_SYMBOLS_decl)),
	       NULL_TREE);
}

static tree
init_module_descriptor (type)
     tree type;
{
  tree initlist, expr;

  /* version = { 1, ... } */

  expr = build_int_2 (OBJC_VERSION, 0);
  initlist = build_tree_list (NULL_TREE, expr);

  /* size = { ..., sizeof (struct objc_module), ... } */

  expr = size_in_bytes (objc_module_template);
  initlist = tree_cons (NULL_TREE, expr, initlist);

  /* name = { ..., "foo.m", ... } */

  expr = add_objc_string (get_identifier (input_filename), class_names);
  initlist = tree_cons (NULL_TREE, expr, initlist);


  if (!flag_next_runtime)
    {
      /* statics = { ..., _OBJC_STATIC_INSTANCES, ... }  */
      if (static_instances_decl)
	expr = build_unary_op (ADDR_EXPR, static_instances_decl, 0);
      else
	expr = build_int_2 (0, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  /* symtab = { ..., _OBJC_SYMBOLS, ... } */

  if (UOBJC_SYMBOLS_decl)
    expr = build_unary_op (ADDR_EXPR, UOBJC_SYMBOLS_decl, 0);
  else
    expr = build_int_2 (0, 0);
  initlist = tree_cons (NULL_TREE, expr, initlist);

  return build_constructor (type, nreverse (initlist));
}

/* Write out the data structures to describe Objective C classes defined.
   If appropriate, compile and output a setup function to initialize them.
   Return a string which is the name of a function to call to initialize
   the Objective C data structures for this file (and perhaps for other files
   also).

   struct objc_module { ... } _OBJC_MODULE = { ... };   */

static char *
build_module_descriptor ()
{
  tree decl_specs, field_decl, field_decl_chain;

  objc_module_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_MODULE));

  /* Long version; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("version");
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* long  size; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("size");
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* char  *name; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("name"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);


  if (!flag_next_runtime)
    {
      /* void *statics */

      decl_specs = get_identifier (UTAG_STATICS);
      decl_specs
	= build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE, decl_specs));
      field_decl
	= build1 (INDIRECT_REF, NULL_TREE, get_identifier ("statics"));
      field_decl = grokfield (input_filename, lineno, field_decl,
			      decl_specs, NULL_TREE);
      chainon (field_decl_chain, field_decl);
    }


  /* struct objc_symtab *symtab; */

  decl_specs = get_identifier (UTAG_SYMTAB);
  decl_specs = build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE, decl_specs));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("symtab"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_module_template, field_decl_chain, NULL_TREE);

  /* Create an instance of "objc_module".  */

  decl_specs = tree_cons (NULL_TREE, objc_module_template,
			  build_tree_list (NULL_TREE,
					   ridpointers[(int) RID_STATIC]));

  UOBJC_MODULES_decl = start_decl (get_identifier ("_OBJC_MODULES"),
				   decl_specs, 1, NULL_TREE, NULL_TREE);

  DECL_ARTIFICIAL (UOBJC_MODULES_decl) = 1;
  DECL_IGNORED_P (UOBJC_MODULES_decl) = 1;
  finish_decl (UOBJC_MODULES_decl,
	       init_module_descriptor (TREE_TYPE (UOBJC_MODULES_decl)),
	       NULL_TREE);

  /* Mark the decl to avoid "defined but not used" warning.  */
  DECL_IN_SYSTEM_HEADER (UOBJC_MODULES_decl) = 1;

  /* Generate a constructor call for the module descriptor.
     This code was generated by reading the grammar rules
     of c-parse.in;  Therefore, it may not be the most efficient
     way of generating the requisite code.  */

  if (flag_next_runtime)
    return 0;

  {
    tree parms, function_decl, decelerator, void_list_node;
    tree function_type;
    extern tree get_file_function_name ();
    tree init_function_name = get_file_function_name ('I');

    /* Declare void __objc_execClass (void *); */

    void_list_node = build_tree_list (NULL_TREE, void_type_node);
    function_type
      = build_function_type (void_type_node,
			     tree_cons (NULL_TREE, ptr_type_node,
					void_list_node));
    function_decl = build_decl (FUNCTION_DECL,
				get_identifier (TAG_EXECCLASS),
				function_type);
    DECL_EXTERNAL (function_decl) = 1;
    DECL_ARTIFICIAL (function_decl) = 1;
    TREE_PUBLIC (function_decl) = 1;

    pushdecl (function_decl);
    rest_of_decl_compilation (function_decl, 0, 0, 0);

    parms
      = build_tree_list (NULL_TREE,
			 build_unary_op (ADDR_EXPR, UOBJC_MODULES_decl, 0));
    decelerator = build_function_call (function_decl, parms);

    /* void _GLOBAL_$I$<gnyf> () {objc_execClass (&L_OBJC_MODULES);}  */

    start_function (void_list_node,
		    build_parse_node (CALL_EXPR, init_function_name,
				      /* This has the format of the output
					 of get_parm_info.  */
				      tree_cons (NULL_TREE, NULL_TREE,
						 void_list_node),
				      NULL_TREE),
		    NULL_TREE, NULL_TREE, 0);
#if 0 /* This should be turned back on later
	 for the systems where collect is not needed.  */
    /* Make these functions nonglobal
       so each file can use the same name.  */
    TREE_PUBLIC (current_function_decl) = 0;
#endif
    TREE_USED (current_function_decl) = 1;
    store_parm_decls ();

    assemble_external (function_decl);
    c_expand_expr_stmt (decelerator);

    TREE_PUBLIC (current_function_decl) = 1;

    function_decl = current_function_decl;
    finish_function (0);

    /* Return the name of the constructor function.  */
    return XSTR (XEXP (DECL_RTL (function_decl), 0), 0);
  }
}

/* extern const char _OBJC_STRINGS[]; */

static void
generate_forward_declaration_to_string_table ()
{
  tree sc_spec, decl_specs, expr_decl;

  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_EXTERN], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_CHAR], sc_spec);

  expr_decl
    = build_nt (ARRAY_REF, get_identifier ("_OBJC_STRINGS"), NULL_TREE);

  UOBJC_STRINGS_decl = define_decl (expr_decl, decl_specs);
}

/* Return the DECL of the string IDENT in the SECTION.  */

static tree
get_objc_string_decl (ident, section)
     tree ident;
     enum string_section section;
{
  tree chain, decl;

  if (section == class_names)
    chain = class_names_chain;
  else if (section == meth_var_names)
    chain = meth_var_names_chain;
  else if (section == meth_var_types)
    chain = meth_var_types_chain;

  for (; chain != 0; chain = TREE_VALUE (chain))
    if (TREE_VALUE (chain) == ident)
      return (TREE_PURPOSE (chain));

  abort ();
  return NULL_TREE;
}

/* Output references to all statically allocated objects.  Return the DECL
   for the array built.  */

static tree
generate_static_references ()
{
  tree decls = NULL_TREE, ident, decl_spec, expr_decl, expr = NULL_TREE;
  tree class_name, class, decl, instance, idecl, initlist;
  tree cl_chain, in_chain, type;
  int num_inst, num_class;
  char buf[256];

  if (flag_next_runtime)
    abort ();

  for (cl_chain = objc_static_instances, num_class = 0;
       cl_chain; cl_chain = TREE_CHAIN (cl_chain), num_class++)
    {
      for (num_inst = 0, in_chain = TREE_PURPOSE (cl_chain);
	   in_chain; num_inst++, in_chain = TREE_CHAIN (in_chain));

      sprintf (buf, "_OBJC_STATIC_INSTANCES_%d", num_class);
      ident = get_identifier (buf);

      expr_decl = build_nt (ARRAY_REF, ident, NULL_TREE);
      decl_spec = tree_cons (NULL_TREE, build_pointer_type (void_type_node),
			     build_tree_list (NULL_TREE,
					      ridpointers[(int) RID_STATIC]));
      decl = start_decl (expr_decl, decl_spec, 1, NULL_TREE, NULL_TREE);
      DECL_CONTEXT (decl) = 0;
      DECL_ARTIFICIAL (decl) = 1;

      /* Output {class_name, ...}.  */
      class = TREE_VALUE (cl_chain);
      class_name = get_objc_string_decl (TYPE_NAME (class), class_names);
      initlist = build_tree_list (NULL_TREE,
				  build_unary_op (ADDR_EXPR, class_name, 1));

      /* Output {..., instance, ...}.  */
      for (in_chain = TREE_PURPOSE (cl_chain);
	   in_chain; in_chain = TREE_CHAIN (in_chain))
	{
	  expr = build_unary_op (ADDR_EXPR, TREE_VALUE (in_chain), 1);
	  initlist = tree_cons (NULL_TREE, expr, initlist);
	}

      /* Output {..., NULL}.  */
      initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);

      expr = build_constructor (TREE_TYPE (decl), nreverse (initlist));
      finish_decl (decl, expr, NULL_TREE);
      TREE_USED (decl) = 1;

      type = build_array_type (build_pointer_type (void_type_node), 0);
      decl = build_decl (VAR_DECL, ident, type);
      make_decl_rtl (decl, 0, 1);
      TREE_USED (decl) = 1;
      decls
	= tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, decl, 1), decls);
    }

  decls = tree_cons (NULL_TREE, build_int_2 (0, 0), decls);
  ident = get_identifier ("_OBJC_STATIC_INSTANCES");
  expr_decl = build_nt (ARRAY_REF, ident, NULL_TREE);
  decl_spec = tree_cons (NULL_TREE, build_pointer_type (void_type_node),
			 build_tree_list (NULL_TREE,
					  ridpointers[(int) RID_STATIC]));
  static_instances_decl
    = start_decl (expr_decl, decl_spec, 1, NULL_TREE, NULL_TREE);
  DECL_CONTEXT (static_instances_decl) = 0;
  DECL_ARTIFICIAL (static_instances_decl) = 1;
  end_temporary_allocation ();
  expr = build_constructor (TREE_TYPE (static_instances_decl),
			    nreverse (decls));
  finish_decl (static_instances_decl, expr, NULL_TREE);
}

/* Output all strings.  */

static void
generate_strings ()
{
  tree sc_spec, decl_specs, expr_decl;
  tree chain, string_expr;
  tree string, decl;

  for (chain = class_names_chain; chain; chain = TREE_CHAIN (chain))
    {
      string = TREE_VALUE (chain);
      decl = TREE_PURPOSE (chain);
      sc_spec
	= tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
      decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_CHAR], sc_spec);
      expr_decl = build_nt (ARRAY_REF, DECL_NAME (decl), NULL_TREE);
      decl = start_decl (expr_decl, decl_specs, 1, NULL_TREE, NULL_TREE);
      end_temporary_allocation ();
      string_expr = my_build_string (IDENTIFIER_LENGTH (string) + 1,
				     IDENTIFIER_POINTER (string));
      finish_decl (decl, string_expr, NULL_TREE);
    }

  for (chain = meth_var_names_chain; chain; chain = TREE_CHAIN (chain))
    {
      string = TREE_VALUE (chain);
      decl = TREE_PURPOSE (chain);
      sc_spec
	= tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
      decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_CHAR], sc_spec);
      expr_decl = build_nt (ARRAY_REF, DECL_NAME (decl), NULL_TREE);
      decl = start_decl (expr_decl, decl_specs, 1, NULL_TREE, NULL_TREE);
      string_expr = my_build_string (IDENTIFIER_LENGTH (string) + 1,
				     IDENTIFIER_POINTER (string));
      finish_decl (decl, string_expr, NULL_TREE);
    }

  for (chain = meth_var_types_chain; chain; chain = TREE_CHAIN (chain))
    {
      string = TREE_VALUE (chain);
      decl = TREE_PURPOSE (chain);
      sc_spec
	= tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
      decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_CHAR], sc_spec);
      expr_decl = build_nt (ARRAY_REF, DECL_NAME (decl), NULL_TREE);
      decl = start_decl (expr_decl, decl_specs, 1, NULL_TREE, NULL_TREE);
      string_expr = my_build_string (IDENTIFIER_LENGTH (string) + 1,
				IDENTIFIER_POINTER (string));
      finish_decl (decl, string_expr, NULL_TREE);
    }
}

static tree
build_selector_reference_decl (name)
      tree name;
{
  tree decl, ident;
  char buf[256];
  static int idx = 0;

  sprintf (buf, "_OBJC_SELECTOR_REFERENCES_%d", idx++);

  push_obstacks_nochange ();
  end_temporary_allocation ();

  ident = get_identifier (buf);

  decl = build_decl (VAR_DECL, ident, selector_type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  TREE_USED (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_CONTEXT (decl) = 0;

  make_decl_rtl (decl, 0, 1);
  pushdecl_top_level (decl);

  pop_obstacks ();

  return decl;
}

/* Just a handy wrapper for add_objc_string.  */

static tree
build_selector (ident)
     tree ident;
{
  tree expr = add_objc_string (ident, meth_var_names);
  if (flag_typed_selectors)
    return expr;
  else
    return build_c_cast (selector_type, expr); /* cast! */
}

/* Synthesize the following expr: (char *)&_OBJC_STRINGS[<offset>]
   The cast stops the compiler from issuing the following message:
   grok.m: warning: initialization of non-const * pointer from const *
   grok.m: warning: initialization between incompatible pointer types.  */

static tree
build_msg_pool_reference (offset)
     int offset;
{
  tree expr = build_int_2 (offset, 0);
  tree cast;

  expr = build_array_ref (UOBJC_STRINGS_decl, expr);
  expr = build_unary_op (ADDR_EXPR, expr, 0);

  cast = build_tree_list (build_tree_list (NULL_TREE,
					   ridpointers[(int) RID_CHAR]),
			  build1 (INDIRECT_REF, NULL_TREE, NULL_TREE));
  TREE_TYPE (expr) = groktypename (cast);
  return expr;
}

static tree
init_selector (offset)
     int offset;
{
  tree expr = build_msg_pool_reference (offset);
  TREE_TYPE (expr) = selector_type;
  return expr;
}

static void
build_selector_translation_table ()
{
  tree sc_spec, decl_specs;
  tree chain, initlist = NULL_TREE;
  int offset = 0;
  tree decl, var_decl, name;

  /* The corresponding pop_obstacks is in finish_decl,
     called at the end of this function.  */
  if (! flag_next_runtime)
    push_obstacks_nochange ();

  for (chain = sel_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      tree expr;

      expr = build_selector (TREE_VALUE (chain));

      if (flag_next_runtime)
	{
	  name = DECL_NAME (TREE_PURPOSE (chain));

	  sc_spec = build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]);

	  /* static SEL _OBJC_SELECTOR_REFERENCES_n = ...; */
	  decl_specs = tree_cons (NULL_TREE, selector_type, sc_spec);

	  var_decl = name;

	  /* The `decl' that is returned from start_decl is the one that we
	     forward declared in `build_selector_reference'  */
	  decl = start_decl (var_decl, decl_specs, 1, NULL_TREE, NULL_TREE);
	}

      /* add one for the '\0' character */
      offset += IDENTIFIER_LENGTH (TREE_VALUE (chain)) + 1;

      if (flag_next_runtime)
	finish_decl (decl, expr, NULL_TREE);
      else 
	{
	  if (flag_typed_selectors)
	    {
	      tree eltlist = NULL_TREE;
	      tree encoding = get_proto_encoding (TREE_PURPOSE (chain));
	      eltlist = tree_cons (NULL_TREE, expr, NULL_TREE);
	      eltlist = tree_cons (NULL_TREE, encoding, eltlist);
	      expr = build_constructor (objc_selector_template,
					nreverse (eltlist));
	    }
	  initlist = tree_cons (NULL_TREE, expr, initlist);
	  
	}
    }

  if (! flag_next_runtime)
    {
      /* Cause the variable and its initial value to be actually output.  */
      DECL_EXTERNAL (UOBJC_SELECTOR_TABLE_decl) = 0;
      TREE_STATIC (UOBJC_SELECTOR_TABLE_decl) = 1;
      /* NULL terminate the list and fix the decl for output.  */
      initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
      DECL_INITIAL (UOBJC_SELECTOR_TABLE_decl) = (tree) 1;
      initlist = build_constructor (TREE_TYPE (UOBJC_SELECTOR_TABLE_decl),
				    nreverse (initlist));
      finish_decl (UOBJC_SELECTOR_TABLE_decl, initlist, NULL_TREE);
      current_function_decl = NULL_TREE;
    }
}

static tree
get_proto_encoding (proto)
     tree proto;
{
  tree encoding;
  if (proto)
    {
      tree tmp_decl;

      if (! METHOD_ENCODING (proto))
	{
	    tmp_decl = build_tmp_function_decl ();
	    hack_method_prototype (proto, tmp_decl);
	    encoding = encode_method_prototype (proto, tmp_decl);
	    METHOD_ENCODING (proto) = encoding;
	  }
      else
	encoding = METHOD_ENCODING (proto);

      return add_objc_string (encoding, meth_var_types);
    }
  else
    return build_int_2 (0, 0);
}

/* sel_ref_chain is a list whose "value" fields will be instances of
   identifier_node that represent the selector.  */

static tree
build_typed_selector_reference (ident, proto)
     tree ident, proto;
{
  tree *chain = &sel_ref_chain;
  tree expr;
  int index = 0;

  while (*chain)
    {
      if (TREE_PURPOSE (*chain) == ident && TREE_VALUE (*chain) == proto)
	goto return_at_index;

      index++;
      chain = &TREE_CHAIN (*chain);
    }

  *chain = perm_tree_cons (proto, ident, NULL_TREE);

 return_at_index:
  expr = build_unary_op (ADDR_EXPR,
			 build_array_ref (UOBJC_SELECTOR_TABLE_decl,
					  build_int_2 (index, 0)),
			 1);
  return build_c_cast (selector_type, expr);
}

static tree
build_selector_reference (ident)
     tree ident;
{
  tree *chain = &sel_ref_chain;
  tree expr;
  int index = 0;

  while (*chain)
    {
      if (TREE_VALUE (*chain) == ident)
	return (flag_next_runtime
		? TREE_PURPOSE (*chain)
		: build_array_ref (UOBJC_SELECTOR_TABLE_decl,
				   build_int_2 (index, 0)));

      index++;
      chain = &TREE_CHAIN (*chain);
    }

  expr = build_selector_reference_decl (ident);

  *chain = perm_tree_cons (expr, ident, NULL_TREE);

  return (flag_next_runtime
	  ? expr
	  : build_array_ref (UOBJC_SELECTOR_TABLE_decl,
			     build_int_2 (index, 0)));
}

static tree
build_class_reference_decl (name)
      tree name;
{
  tree decl, ident;
  char buf[256];
  static int idx = 0;

  sprintf (buf, "_OBJC_CLASS_REFERENCES_%d", idx++);

  push_obstacks_nochange ();
  end_temporary_allocation ();

  ident = get_identifier (buf);

  decl = build_decl (VAR_DECL, ident, objc_class_type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  TREE_USED (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_CONTEXT (decl) = 0;
  DECL_ARTIFICIAL (decl) = 1;

  make_decl_rtl (decl, 0, 1);
  pushdecl_top_level (decl);

  pop_obstacks ();

  return decl;
}

/* Create a class reference, but don't create a variable to reference
   it.  */

static void
add_class_reference (ident)
     tree ident;
{
  tree chain;

  if ((chain = cls_ref_chain))
    {
      tree tail;
      do
        {
	  if (ident == TREE_VALUE (chain))
	    return;

	  tail = chain;
	  chain = TREE_CHAIN (chain);
        }
      while (chain);

      /* Append to the end of the list */
      TREE_CHAIN (tail) = perm_tree_cons (NULL_TREE, ident, NULL_TREE);
    }
  else
    cls_ref_chain = perm_tree_cons (NULL_TREE, ident, NULL_TREE);
}

/* Get a class reference, creating it if necessary.  Also create the
   reference variable.  */

tree
get_class_reference (ident)
    tree ident;
{
  if (flag_next_runtime)
    {
      tree *chain;
      tree decl;

      for (chain = &cls_ref_chain; *chain; chain = &TREE_CHAIN (*chain))
	if (TREE_VALUE (*chain) == ident)
	  {
	    if (! TREE_PURPOSE (*chain))
	      TREE_PURPOSE (*chain) = build_class_reference_decl (ident);

	    return TREE_PURPOSE (*chain);
	  }

      decl = build_class_reference_decl (ident);
      *chain = perm_tree_cons (decl, ident, NULL_TREE);
      return decl;
    }
  else
    {
      tree params;

      add_class_reference (ident);

      params = build_tree_list (NULL_TREE,
				my_build_string (IDENTIFIER_LENGTH (ident) + 1,
						 IDENTIFIER_POINTER (ident)));

      assemble_external (objc_get_class_decl);
      return build_function_call (objc_get_class_decl, params);
    }
}

/* SEL_REFDEF_CHAIN is a list whose "value" fields will be instances
   of identifier_node that represent the selector. It returns the
   offset of the selector from the beginning of the _OBJC_STRINGS
   pool. This offset is typically used by init_selector during code
   generation.

   For each string section we have a chain which maps identifier nodes
   to decls for the strings.  */

static tree
add_objc_string (ident, section)
     tree ident;
     enum string_section section;
{
  tree *chain, decl;

  if (section == class_names)
    chain = &class_names_chain;
  else if (section == meth_var_names)
    chain = &meth_var_names_chain;
  else if (section == meth_var_types)
    chain = &meth_var_types_chain;

  while (*chain)
    {
      if (TREE_VALUE (*chain) == ident)
	return build_unary_op (ADDR_EXPR, TREE_PURPOSE (*chain), 1);

      chain = &TREE_CHAIN (*chain);
    }

  decl = build_objc_string_decl (ident, section);

  *chain = perm_tree_cons (decl, ident, NULL_TREE);

  return build_unary_op (ADDR_EXPR, decl, 1);
}

static tree
build_objc_string_decl (name, section)
     tree name;
     enum string_section section;
{
  tree decl, ident;
  char buf[256];
  static int class_names_idx = 0;
  static int meth_var_names_idx = 0;
  static int meth_var_types_idx = 0;

  if (section == class_names)
    sprintf (buf, "_OBJC_CLASS_NAME_%d", class_names_idx++);
  else if (section == meth_var_names)
    sprintf (buf, "_OBJC_METH_VAR_NAME_%d", meth_var_names_idx++);
  else if (section == meth_var_types)
    sprintf (buf, "_OBJC_METH_VAR_TYPE_%d", meth_var_types_idx++);

  push_obstacks_nochange ();
  end_temporary_allocation ();
  ident = get_identifier (buf);

  decl = build_decl (VAR_DECL, ident, build_array_type (char_type_node, 0));
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  TREE_USED (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_CONSTANT (decl) = 1;
  DECL_CONTEXT (decl) = 0;
  DECL_ARTIFICIAL (decl) = 1;
 
  make_decl_rtl (decl, 0, 1);
  pushdecl_top_level (decl);

  pop_obstacks ();

  return decl;
}


void
objc_declare_alias (alias_ident, class_ident)
     tree alias_ident;
     tree class_ident;
{
  if (!doing_objc_thang)
    objc_fatal ();

  if (is_class_name (class_ident) != class_ident)
    warning ("Cannot find class `%s'", IDENTIFIER_POINTER (class_ident));
  else if (is_class_name (alias_ident))
    warning ("Class `%s' already exists", IDENTIFIER_POINTER (alias_ident));
  else
    alias_chain = tree_cons (class_ident, alias_ident, alias_chain);
}

void
objc_declare_class (ident_list)
     tree ident_list;
{
  tree list;

  if (!doing_objc_thang)
    objc_fatal ();

  for (list = ident_list; list; list = TREE_CHAIN (list))
    {
      tree ident = TREE_VALUE (list);
      tree decl;

      if ((decl = lookup_name (ident)))
	{
	  error ("`%s' redeclared as different kind of symbol",
		  IDENTIFIER_POINTER (ident));
	  error_with_decl (decl, "previous declaration of `%s'");
	}

      if (! is_class_name (ident))
        {
	  tree record = xref_tag (RECORD_TYPE, ident);
	  TREE_STATIC_TEMPLATE (record) = 1;
	  class_chain = tree_cons (NULL_TREE, ident, class_chain);
	}
    }
}

tree
is_class_name (ident)
     tree ident;
{
  tree chain;

  if (lookup_interface (ident))
    return ident;

  for (chain = class_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == TREE_VALUE (chain))
	return ident;
    }

  for (chain = alias_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == TREE_VALUE (chain))
	return TREE_PURPOSE (chain);
    }

  return 0;
}

tree
lookup_interface (ident)
     tree ident;
{
  tree chain;

  for (chain = interface_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == CLASS_NAME (chain))
	return chain;
    }
  return NULL_TREE;
}

static tree
objc_copy_list (list, head)
     tree list;
     tree *head;
{
  tree newlist = NULL_TREE, tail = NULL_TREE;

  while (list)
    {
      tail = copy_node (list);

      /* The following statement fixes a bug when inheriting instance
	 variables that are declared to be bitfields. finish_struct
	 expects to find the width of the bitfield in DECL_INITIAL,
	 which it nulls out after processing the decl of the super
	 class...rather than change the way finish_struct works (which
	 is risky), I create the situation it expects...s.naroff
	 (7/23/89).  */

      if (DECL_BIT_FIELD (tail) && DECL_INITIAL (tail) == 0)
	DECL_INITIAL (tail) = build_int_2 (DECL_FIELD_SIZE (tail), 0);

      newlist = chainon (newlist, tail);
      list = TREE_CHAIN (list);
    }

  *head = newlist;
  return tail;
}

/* Used by: build_private_template, get_class_ivars, and
   continue_class.  COPY is 1 when called from @defs.  In this case
   copy all fields.  Otherwise don't copy leaf ivars since we rely on
   them being side-effected exactly once by finish_struct.  */

static tree
build_ivar_chain (interface, copy)
     tree interface;
     int copy;
{
  tree my_name, super_name, ivar_chain;

  my_name = CLASS_NAME (interface);
  super_name = CLASS_SUPER_NAME (interface);

  /* Possibly copy leaf ivars.  */
  if (copy)
    objc_copy_list (CLASS_IVARS (interface), &ivar_chain);
  else
    ivar_chain = CLASS_IVARS (interface);

  while (super_name)
    {
      tree op1;
      tree super_interface = lookup_interface (super_name);

      if (!super_interface)
        {
	  /* fatal did not work with 2 args...should fix */
	  error ("Cannot find interface declaration for `%s', superclass of `%s'",
		 IDENTIFIER_POINTER (super_name),
		 IDENTIFIER_POINTER (my_name));
	  exit (FATAL_EXIT_CODE);
        }

      if (super_interface == interface)
        {
          fatal ("Circular inheritance in interface declaration for `%s'",
                 IDENTIFIER_POINTER (super_name));
        }

      interface = super_interface;
      my_name = CLASS_NAME (interface);
      super_name = CLASS_SUPER_NAME (interface);

      op1 = CLASS_IVARS (interface);
      if (op1)
        {
	  tree head, tail = objc_copy_list (op1, &head);

	  /* Prepend super class ivars...make a copy of the list, we
	     do not want to alter the original.  */
	  TREE_CHAIN (tail) = ivar_chain;
	  ivar_chain = head;
        }
    }
  return ivar_chain;
}

/* struct <classname> {
     struct objc_class *isa;
     ...
   };  */

static tree
build_private_template (class)
     tree class;
{
  tree ivar_context;

  if (CLASS_STATIC_TEMPLATE (class))
    {
      uprivate_record = CLASS_STATIC_TEMPLATE (class);
      ivar_context = TYPE_FIELDS (CLASS_STATIC_TEMPLATE (class));
    }
  else
    {
      uprivate_record = start_struct (RECORD_TYPE, CLASS_NAME (class));

      ivar_context = build_ivar_chain (class, 0);

      finish_struct (uprivate_record, ivar_context, NULL_TREE);

      CLASS_STATIC_TEMPLATE (class) = uprivate_record;

      /* mark this record as class template - for class type checking */
      TREE_STATIC_TEMPLATE (uprivate_record) = 1;
    }

  instance_type
    = groktypename (build_tree_list (build_tree_list (NULL_TREE,
						      uprivate_record),
				     build1 (INDIRECT_REF, NULL_TREE,
					     NULL_TREE)));

  return ivar_context;
}

/* Begin code generation for protocols...  */

/* struct objc_protocol {
     char *protocol_name;
     struct objc_protocol **protocol_list;
     struct objc_method_desc *instance_methods;
     struct objc_method_desc *class_methods;
   };  */

static tree
build_protocol_template ()
{
  tree decl_specs, field_decl, field_decl_chain;
  tree template;

  template = start_struct (RECORD_TYPE, get_identifier (UTAG_PROTOCOL));

  /* struct objc_class *isa; */

  decl_specs = build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE,
					get_identifier (UTAG_CLASS)));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("isa"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* char *protocol_name; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("protocol_name"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_protocol **protocol_list; */

  decl_specs = build_tree_list (NULL_TREE, template);
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("protocol_list"));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, field_decl);
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *instance_methods; */

  decl_specs
    = build_tree_list (NULL_TREE,
		       xref_tag (RECORD_TYPE,
				 get_identifier (UTAG_METHOD_PROTOTYPE_LIST)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("instance_methods"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *class_methods; */

  decl_specs
    = build_tree_list (NULL_TREE,
		       xref_tag (RECORD_TYPE,
				 get_identifier (UTAG_METHOD_PROTOTYPE_LIST)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("class_methods"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  return finish_struct (template, field_decl_chain, NULL_TREE);
}

static tree
build_descriptor_table_initializer (type, entries)
     tree type;
     tree entries;
{
  tree initlist = NULL_TREE;

  do
    {
      tree eltlist = NULL_TREE;

      eltlist
	= tree_cons (NULL_TREE,
		     build_selector (METHOD_SEL_NAME (entries)), NULL_TREE);
      eltlist
	= tree_cons (NULL_TREE,
		     add_objc_string (METHOD_ENCODING (entries),
				      meth_var_types),
		     eltlist);

      initlist
	= tree_cons (NULL_TREE,
		     build_constructor (type, nreverse (eltlist)), initlist);

      entries = TREE_CHAIN (entries);
    }
  while (entries);

  return build_constructor (build_array_type (type, 0), nreverse (initlist));
}

/* struct objc_method_prototype_list {
     int count;
     struct objc_method_prototype {
 	SEL name;
 	char *types;
     } list[1];
   };  */

static tree
build_method_prototype_list_template (list_type, size)
     tree list_type;
     int size;
{
  tree objc_ivar_list_record;
  tree decl_specs, field_decl, field_decl_chain;

  /* Generate an unnamed struct definition.  */

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULL_TREE);

  /* int method_count; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("method_count");

  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* struct objc_method method_list[]; */

  decl_specs = build_tree_list (NULL_TREE, list_type);
  field_decl = build_nt (ARRAY_REF, get_identifier ("method_list"),
			 build_int_2 (size, 0));

  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain, NULL_TREE);

  return objc_ivar_list_record;
}

static tree
build_method_prototype_template ()
{
  tree proto_record;
  tree decl_specs, field_decl, field_decl_chain;

  proto_record
    = start_struct (RECORD_TYPE, get_identifier (UTAG_METHOD_PROTOTYPE));

#ifdef OBJC_INT_SELECTORS
  /* unsigned int _cmd; */
  decl_specs
    = tree_cons (NULL_TREE, ridpointers[(int) RID_UNSIGNED], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_INT], decl_specs);
  field_decl = get_identifier ("_cmd");
#else /* OBJC_INT_SELECTORS */
  /* struct objc_selector *_cmd; */
  decl_specs = tree_cons (NULL_TREE, xref_tag (RECORD_TYPE,
		          get_identifier (TAG_SELECTOR)), NULL_TREE);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("_cmd"));
#endif /* OBJC_INT_SELECTORS */

  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_CHAR], NULL_TREE);
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("method_types"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (proto_record, field_decl_chain, NULL_TREE);

  return proto_record;
}

/* True if last call to forwarding_offset yielded a register offset.  */
static int offset_is_register;

static int
forwarding_offset (parm)
      tree parm;
{
  int offset_in_bytes;

  if (GET_CODE (DECL_INCOMING_RTL (parm)) == MEM)
    {
      rtx addr = XEXP (DECL_INCOMING_RTL (parm), 0);

      /* ??? Here we assume that the parm address is indexed
	  off the frame pointer or arg pointer.
	  If that is not true, we produce meaningless results,
	  but do not crash.  */
      if (GET_CODE (addr) == PLUS
	  && GET_CODE (XEXP (addr, 1)) == CONST_INT)
	offset_in_bytes = INTVAL (XEXP (addr, 1));
      else
	offset_in_bytes = 0;

      offset_in_bytes += OBJC_FORWARDING_STACK_OFFSET;
      offset_is_register = 0;
    }
  else if (GET_CODE (DECL_INCOMING_RTL (parm)) == REG)
    {
      int regno = REGNO (DECL_INCOMING_RTL (parm));
      offset_in_bytes = apply_args_register_offset (regno);
      offset_is_register = 1;
    }
  else
    return 0;

  /* This is the case where the parm is passed as an int or double
     and it is converted to a char, short or float and stored back
     in the parmlist.  In this case, describe the parm
     with the variable's declared type, and adjust the address
     if the least significant bytes (which we are using) are not
     the first ones.  */
  if (BYTES_BIG_ENDIAN && TREE_TYPE (parm) != DECL_ARG_TYPE (parm))
    offset_in_bytes += (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parm)))
			- GET_MODE_SIZE (GET_MODE (DECL_RTL (parm))));

  return offset_in_bytes;
}

static tree
encode_method_prototype (method_decl, func_decl)
      tree method_decl;
      tree func_decl;
{
  tree parms;
  int stack_size, i;
  tree user_args;
  int max_parm_end = 0;
  char buf[40];
  tree result;

  /* ONEWAY and BYCOPY, for remote object are the only method qualifiers.  */
  encode_type_qualifiers (TREE_PURPOSE (TREE_TYPE (method_decl)));

  /* C type.  */
  encode_type (TREE_TYPE (TREE_TYPE (func_decl)),
	       obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);

  /* Stack size.  */
  for (parms = DECL_ARGUMENTS (func_decl); parms;
       parms = TREE_CHAIN (parms))
    {
      int parm_end = (forwarding_offset (parms)
		      + (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (parms)))
			 / BITS_PER_UNIT));

      if (!offset_is_register && max_parm_end < parm_end)
	max_parm_end = parm_end;
    }

  stack_size = max_parm_end - OBJC_FORWARDING_MIN_OFFSET;

  sprintf (buf, "%d", stack_size);
  obstack_grow (&util_obstack, buf, strlen (buf));

  user_args = METHOD_SEL_ARGS (method_decl);

  /* Argument types.  */
  for (parms = DECL_ARGUMENTS (func_decl), i = 0; parms;
       parms = TREE_CHAIN (parms), i++)
    {
      /* Process argument qualifiers for user supplied arguments.  */
      if (i > 1)
        {
	  encode_type_qualifiers (TREE_PURPOSE (TREE_TYPE (user_args)));
	  user_args = TREE_CHAIN (user_args);
 	}

      /* Type.  */
      encode_type (TREE_TYPE (parms),
		   obstack_object_size (&util_obstack),
		   OBJC_ENCODE_INLINE_DEFS);

      /* Compute offset.  */
      sprintf (buf, "%d", forwarding_offset (parms));

      /* Indicate register.  */
      if (offset_is_register)
	obstack_1grow (&util_obstack, '+');
      
      obstack_grow (&util_obstack, buf, strlen (buf));
    }

  obstack_1grow (&util_obstack, '\0');
  result = get_identifier (obstack_finish (&util_obstack));
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

static tree
generate_descriptor_table (type, name, size, list, proto)
     tree type;
     char *name;
     int size;
     tree list;
     tree proto;
{
  tree sc_spec, decl_specs, decl, initlist;

  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, type, sc_spec);

  decl = start_decl (synth_id_with_class_suffix (name, proto),
				decl_specs, 1, NULL_TREE, NULL_TREE);

  initlist = build_tree_list (NULL_TREE, build_int_2 (size, 0));
  initlist = tree_cons (NULL_TREE, list, initlist);

  finish_decl (decl, build_constructor (type, nreverse (initlist)),
	       NULL_TREE);

  return decl;
}

static void
generate_method_descriptors (protocol)	/* generate_dispatch_tables */
  tree protocol;
{
  static tree objc_method_prototype_template;
  tree initlist, chain, method_list_template;
  tree cast, variable_length_type;
  int size;

  if (!objc_method_prototype_template)
    objc_method_prototype_template = build_method_prototype_template ();

  cast = build_tree_list (build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE,
				get_identifier (UTAG_METHOD_PROTOTYPE_LIST))),
			  NULL_TREE);
  variable_length_type = groktypename (cast);

  chain = PROTOCOL_CLS_METHODS (protocol);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_prototype_list_template (objc_method_prototype_template,
						size);

      initlist 
	= build_descriptor_table_initializer (objc_method_prototype_template,
					      chain);

      UOBJC_CLASS_METHODS_decl
	= generate_descriptor_table (method_list_template,
				     "_OBJC_PROTOCOL_CLASS_METHODS",
				     size, initlist, protocol);
      TREE_TYPE (UOBJC_CLASS_METHODS_decl) = variable_length_type;
    }
  else
    UOBJC_CLASS_METHODS_decl = 0;

  chain = PROTOCOL_NST_METHODS (protocol);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_prototype_list_template (objc_method_prototype_template,
						size);
      initlist
	= build_descriptor_table_initializer (objc_method_prototype_template,
					      chain);

      UOBJC_INSTANCE_METHODS_decl
	= generate_descriptor_table (method_list_template,
				     "_OBJC_PROTOCOL_INSTANCE_METHODS",
				     size, initlist, protocol);
      TREE_TYPE (UOBJC_INSTANCE_METHODS_decl) = variable_length_type;
    }
  else
    UOBJC_INSTANCE_METHODS_decl = 0;
}

static tree
build_tmp_function_decl ()
{
  tree decl_specs, expr_decl, parms;
  static int xxx = 0;
  char buffer[80];

  /* struct objc_object *objc_xxx (id, SEL, ...); */
  pushlevel (0);
  decl_specs = build_tree_list (NULL_TREE, objc_object_reference);
  push_parm_decl (build_tree_list
		  (build_tree_list (decl_specs,
				    build1 (INDIRECT_REF, NULL_TREE,
					    NULL_TREE)),
		   build_tree_list (NULL_TREE, NULL_TREE)));

  decl_specs = build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE,
					  get_identifier (TAG_SELECTOR)));
  expr_decl = build1 (INDIRECT_REF, NULL_TREE, NULL_TREE);

  push_parm_decl (build_tree_list (build_tree_list (decl_specs, expr_decl),
				   build_tree_list (NULL_TREE, NULL_TREE)));
  parms = get_parm_info (0);
  poplevel (0, 0, 0);

  decl_specs = build_tree_list (NULL_TREE, objc_object_reference);
  sprintf (buffer, "__objc_tmp_%x", xxx++);
  expr_decl = build_nt (CALL_EXPR, get_identifier (buffer), parms, NULL_TREE);
  expr_decl = build1 (INDIRECT_REF, NULL_TREE, expr_decl);

  return define_decl (expr_decl, decl_specs);
}

static void
hack_method_prototype (nst_methods, tmp_decl)
     tree nst_methods;
     tree tmp_decl;
{
  tree parms;
  tree parm;

  /* Hack to avoid problem with static typing of self arg.  */
  TREE_SET_CODE (nst_methods, CLASS_METHOD_DECL);
  start_method_def (nst_methods);
  TREE_SET_CODE (nst_methods, INSTANCE_METHOD_DECL);

  if (METHOD_ADD_ARGS (nst_methods) == (tree) 1)
    parms = get_parm_info (0); /* we have a `, ...' */
  else
    parms = get_parm_info (1); /* place a `void_at_end' */

  poplevel (0, 0, 0);	/* Must be called BEFORE start_function.  */

  /* Usually called from store_parm_decls -> init_function_start.  */

  DECL_ARGUMENTS (tmp_decl) = TREE_PURPOSE (parms);
  current_function_decl = tmp_decl;

  {
    /* Code taken from start_function.  */
    tree restype = TREE_TYPE (TREE_TYPE (tmp_decl));
    /* Promote the value to int before returning it.  */
    if (TREE_CODE (restype) == INTEGER_TYPE
	&& TYPE_PRECISION (restype) < TYPE_PRECISION (integer_type_node))
      restype = integer_type_node;
    DECL_RESULT (tmp_decl) = build_decl (RESULT_DECL, 0, restype);
  }

  for (parm = DECL_ARGUMENTS (tmp_decl); parm; parm = TREE_CHAIN (parm))
    DECL_CONTEXT (parm) = tmp_decl;

  init_function_start (tmp_decl, "objc-act", 0);

  /* Typically called from expand_function_start for function definitions.  */
  assign_parms (tmp_decl, 0);

  /* install return type */
  TREE_TYPE (TREE_TYPE (tmp_decl)) = groktypename (TREE_TYPE (nst_methods));

}

static void
generate_protocol_references (plist)
     tree plist;
{
  tree lproto;

  /* Forward declare protocols referenced.  */
  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    {
      tree proto = TREE_VALUE (lproto);

      if (TREE_CODE (proto) == PROTOCOL_INTERFACE_TYPE
	  && PROTOCOL_NAME (proto))
	{
          if (! PROTOCOL_FORWARD_DECL (proto))
            build_protocol_reference (proto);

          if (PROTOCOL_LIST (proto))
            generate_protocol_references (PROTOCOL_LIST (proto));
        }
    }
}

static void
generate_protocols ()
{
  tree p, tmp_decl, encoding;
  tree sc_spec, decl_specs, decl;
  tree initlist, protocol_name_expr, refs_decl, refs_expr;
  tree cast_type2 = 0;

  tmp_decl = build_tmp_function_decl ();

  if (! objc_protocol_template)
    objc_protocol_template = build_protocol_template ();

  /* If a protocol was directly referenced, pull in indirect references.  */
  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    if (PROTOCOL_FORWARD_DECL (p) && PROTOCOL_LIST (p))
      generate_protocol_references (PROTOCOL_LIST (p));

  for (p = protocol_chain; p; p = TREE_CHAIN (p))
    {
      tree nst_methods = PROTOCOL_NST_METHODS (p);
      tree cls_methods = PROTOCOL_CLS_METHODS (p);

      /* If protocol wasn't referenced, don't generate any code.  */
      if (! PROTOCOL_FORWARD_DECL (p))
	continue;

      /* Make sure we link in the Protocol class.  */
      add_class_reference (get_identifier (PROTOCOL_OBJECT_CLASS_NAME));

      while (nst_methods)
	{
	  if (! METHOD_ENCODING (nst_methods))
	    {
	      hack_method_prototype (nst_methods, tmp_decl);
	      encoding = encode_method_prototype (nst_methods, tmp_decl);
	      METHOD_ENCODING (nst_methods) = encoding;
	    }
	  nst_methods = TREE_CHAIN (nst_methods);
	}

      while (cls_methods)
	{
	  if (! METHOD_ENCODING (cls_methods))
	    {
	      hack_method_prototype (cls_methods, tmp_decl);
	      encoding = encode_method_prototype (cls_methods, tmp_decl);
	      METHOD_ENCODING (cls_methods) = encoding;
	    }

	  cls_methods = TREE_CHAIN (cls_methods);
	}
      generate_method_descriptors (p);

      if (PROTOCOL_LIST (p))
	refs_decl = generate_protocol_list (p);
      else
	refs_decl = 0;

      /* static struct objc_protocol _OBJC_PROTOCOL_<mumble>; */

      sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC],
			   NULL_TREE);
      decl_specs = tree_cons (NULL_TREE, objc_protocol_template, sc_spec);

      decl = start_decl (synth_id_with_class_suffix ("_OBJC_PROTOCOL", p),
			 decl_specs, 1, NULL_TREE, NULL_TREE);

      protocol_name_expr = add_objc_string (PROTOCOL_NAME (p), class_names);

      if (refs_decl)
	{
	  if (!cast_type2)
	    cast_type2
	      = groktypename
		(build_tree_list (build_tree_list (NULL_TREE,
						   objc_protocol_template),
				  build1 (INDIRECT_REF, NULL_TREE,
					  build1 (INDIRECT_REF, NULL_TREE,
						  NULL_TREE))));

	  refs_expr = build_unary_op (ADDR_EXPR, refs_decl, 0);
	  TREE_TYPE (refs_expr) = cast_type2;
	}
      else
	refs_expr = build_int_2 (0, 0);

      /* UOBJC_INSTANCE_METHODS_decl/UOBJC_CLASS_METHODS_decl are set
	 by generate_method_descriptors, which is called above.  */
      initlist = build_protocol_initializer (TREE_TYPE (decl),
					     protocol_name_expr, refs_expr,
					     UOBJC_INSTANCE_METHODS_decl,
					     UOBJC_CLASS_METHODS_decl);
      finish_decl (decl, initlist, NULL_TREE);

      /* Mark the decl as used to avoid "defined but not used" warning.  */
      TREE_USED (decl) = 1;
    }
}

static tree
build_protocol_initializer (type, protocol_name, protocol_list,
			    instance_methods, class_methods)
     tree type;
     tree protocol_name;
     tree protocol_list;
     tree instance_methods;
     tree class_methods;
{
  tree initlist = NULL_TREE, expr;
  static tree cast_type = 0;

  if (!cast_type)
    cast_type
      = groktypename
	(build_tree_list
	 (build_tree_list (NULL_TREE,
			   xref_tag (RECORD_TYPE,
				     get_identifier (UTAG_CLASS))),
	  build1 (INDIRECT_REF, NULL_TREE, NULL_TREE)));

  /* Filling the "isa" in with one allows the runtime system to
     detect that the version change...should remove before final release.  */

  expr = build_int_2 (PROTOCOL_VERSION, 0);
  TREE_TYPE (expr) = cast_type;
  initlist = tree_cons (NULL_TREE, expr, initlist);
  initlist = tree_cons (NULL_TREE, protocol_name, initlist);
  initlist = tree_cons (NULL_TREE, protocol_list, initlist);

  if (!instance_methods)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, instance_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  if (!class_methods)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, class_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  return build_constructor (type, nreverse (initlist));
}

/* struct objc_category {
     char *category_name;
     char *class_name;
     struct objc_method_list *instance_methods;
     struct objc_method_list *class_methods;
     struct objc_protocol_list *protocols;
   };   */

static void
build_category_template ()
{
  tree decl_specs, field_decl, field_decl_chain;

  objc_category_template = start_struct (RECORD_TYPE,
					 get_identifier (UTAG_CATEGORY));
  /* char *category_name; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("category_name"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* char *class_name; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("class_name"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *instance_methods; */

  decl_specs = build_tree_list (NULL_TREE,
				xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_METHOD_LIST)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("instance_methods"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *class_methods; */

  decl_specs = build_tree_list (NULL_TREE,
				xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_METHOD_LIST)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("class_methods"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_protocol **protocol_list; */

  decl_specs = build_tree_list (NULL_TREE,
				xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_PROTOCOL)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("protocol_list"));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, field_decl);
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_category_template, field_decl_chain, NULL_TREE);
}

/* struct objc_selector {
     void *sel_id;
     char *sel_type;
   }; */

static void
build_selector_template ()
{

  tree decl_specs, field_decl, field_decl_chain;

  objc_selector_template 
    = start_struct (RECORD_TYPE, get_identifier (UTAG_SELECTOR));

  /* void *sel_id; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_VOID]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("sel_id"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* char *sel_type; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("sel_type"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_selector_template, field_decl_chain, NULL_TREE);
}

/* struct objc_class {
     struct objc_class *isa;
     struct objc_class *super_class;
     char *name;
     long version;
     long info;
     long instance_size;
     struct objc_ivar_list *ivars;
     struct objc_method_list *methods;
     if (flag_next_runtime)
       struct objc_cache *cache;
     else {
       struct sarray *dtable;
       struct objc_class *subclass_list;
       struct objc_class *sibling_class;
     }
     struct objc_protocol_list *protocols;
   };  */

static void
build_class_template ()
{
  tree decl_specs, field_decl, field_decl_chain;

  objc_class_template
    = start_struct (RECORD_TYPE, get_identifier (UTAG_CLASS));

  /* struct objc_class *isa; */

  decl_specs = build_tree_list (NULL_TREE, objc_class_template);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("isa"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* struct objc_class *super_class; */

  decl_specs = build_tree_list (NULL_TREE, objc_class_template);
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("super_class"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* char *name; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("name"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* long version; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("version");
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* long info; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("info");
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* long instance_size; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]);
  field_decl = get_identifier ("instance_size");
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_ivar_list *ivars; */

  decl_specs = build_tree_list (NULL_TREE,
				xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_IVAR_LIST)));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("ivars"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method_list *methods; */

  decl_specs = build_tree_list (NULL_TREE,
				xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_METHOD_LIST)));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("methods"));
  field_decl
    = grokfield (input_filename, lineno, field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  if (flag_next_runtime)
    {
      /* struct objc_cache *cache; */

      decl_specs = build_tree_list (NULL_TREE,
				    xref_tag (RECORD_TYPE,
					      get_identifier ("objc_cache")));
      field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("cache"));
      field_decl = grokfield (input_filename, lineno, field_decl,
			      decl_specs, NULL_TREE);
      chainon (field_decl_chain, field_decl);
    }
  else
    {
      /* struct sarray *dtable; */

      decl_specs = build_tree_list (NULL_TREE,
				    xref_tag (RECORD_TYPE,
					      get_identifier ("sarray")));
      field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("dtable"));
      field_decl = grokfield (input_filename, lineno, field_decl,
			      decl_specs, NULL_TREE);
      chainon (field_decl_chain, field_decl);

      /* struct objc_class *subclass_list; */

      decl_specs = build_tree_list (NULL_TREE, objc_class_template);
      field_decl
	= build1 (INDIRECT_REF, NULL_TREE, get_identifier ("subclass_list"));
      field_decl = grokfield (input_filename, lineno, field_decl,
			      decl_specs, NULL_TREE);
      chainon (field_decl_chain, field_decl);

      /* struct objc_class *sibling_class; */

      decl_specs = build_tree_list (NULL_TREE, objc_class_template);
      field_decl
	= build1 (INDIRECT_REF, NULL_TREE, get_identifier ("sibling_class"));
      field_decl = grokfield (input_filename, lineno, field_decl,
			      decl_specs, NULL_TREE);
      chainon (field_decl_chain, field_decl);
    }

  /* struct objc_protocol **protocol_list; */

  decl_specs = build_tree_list (NULL_TREE, 
				xref_tag (RECORD_TYPE,
					  get_identifier (UTAG_PROTOCOL)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("protocol_list"));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, field_decl);
  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);


  finish_struct (objc_class_template, field_decl_chain, NULL_TREE);
}

/* Generate appropriate forward declarations for an implementation.  */

static void
synth_forward_declarations ()
{
  tree sc_spec, decl_specs, an_id;

  /* extern struct objc_class _OBJC_CLASS_<my_name>; */

  an_id = synth_id_with_class_suffix ("_OBJC_CLASS", implementation_context);

  sc_spec = build_tree_list (NULL_TREE, ridpointers[(int) RID_EXTERN]);
  decl_specs = tree_cons (NULL_TREE, objc_class_template, sc_spec);
  UOBJC_CLASS_decl = define_decl (an_id, decl_specs);
  TREE_USED (UOBJC_CLASS_decl) = 1;
  DECL_ARTIFICIAL (UOBJC_CLASS_decl) = 1;

  /* extern struct objc_class _OBJC_METACLASS_<my_name>; */

  an_id = synth_id_with_class_suffix ("_OBJC_METACLASS",
				      implementation_context);

  UOBJC_METACLASS_decl = define_decl (an_id, decl_specs);
  TREE_USED (UOBJC_METACLASS_decl) = 1;
  DECL_ARTIFICIAL(UOBJC_METACLASS_decl) = 1;

  /* Pre-build the following entities - for speed/convenience.  */

  an_id = get_identifier ("super_class");
  ucls_super_ref = build_component_ref (UOBJC_CLASS_decl, an_id);
  uucls_super_ref = build_component_ref (UOBJC_METACLASS_decl, an_id);
}

static void
error_with_ivar (message, decl, rawdecl)
     char *message;
     tree decl;
     tree rawdecl;
{
  count_error (0);

  report_error_function (DECL_SOURCE_FILE (decl));

  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
  bzero (errbuf, BUFSIZE);
  fprintf (stderr, "%s `%s'\n", message, gen_declaration (rawdecl, errbuf));
}

#define USERTYPE(t) \
 (TREE_CODE (t) == RECORD_TYPE || TREE_CODE (t) == UNION_TYPE \
  ||  TREE_CODE (t) == ENUMERAL_TYPE)

static void
check_ivars (inter, imp)
     tree inter;
     tree imp;
{
  tree intdecls = CLASS_IVARS (inter);
  tree impdecls = CLASS_IVARS (imp);
  tree rawintdecls = CLASS_RAW_IVARS (inter);
  tree rawimpdecls = CLASS_RAW_IVARS (imp);

  while (1)
    {
      tree t1, t2;

      if (intdecls == 0 && impdecls == 0)
	break;
      if (intdecls == 0 || impdecls == 0)
	{
	  error ("inconsistent instance variable specification");
	  break;
	}

      t1 = TREE_TYPE (intdecls); t2 = TREE_TYPE (impdecls);

      if (!comptypes (t1, t2))
	{
	  if (DECL_NAME (intdecls) == DECL_NAME (impdecls))
	    {
	      error_with_ivar ("conflicting instance variable type",
			       impdecls, rawimpdecls);
	      error_with_ivar ("previous declaration of",
			       intdecls, rawintdecls);
	    }
	  else			/* both the type and the name don't match */
	    {
	      error ("inconsistent instance variable specification");
	      break;
	    }
	}

      else if (DECL_NAME (intdecls) != DECL_NAME (impdecls))
	{
	  error_with_ivar ("conflicting instance variable name",
			   impdecls, rawimpdecls);
	  error_with_ivar ("previous declaration of",
			   intdecls, rawintdecls);
	}

      intdecls = TREE_CHAIN (intdecls);
      impdecls = TREE_CHAIN (impdecls);
      rawintdecls = TREE_CHAIN (rawintdecls);
      rawimpdecls = TREE_CHAIN (rawimpdecls);
    }
}

/* Set super_type to the data type node for struct objc_super *,
   first defining struct objc_super itself.
   This needs to be done just once per compilation.  */

static tree
build_super_template ()
{
  tree record, decl_specs, field_decl, field_decl_chain;

  record = start_struct (RECORD_TYPE, get_identifier (UTAG_SUPER));

  /* struct objc_object *self; */

  decl_specs = build_tree_list (NULL_TREE, objc_object_reference);
  field_decl = get_identifier ("self");
  field_decl = build1 (INDIRECT_REF, NULL_TREE, field_decl);
  field_decl = grokfield (input_filename, lineno,
			  field_decl, decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* struct objc_class *class; */

  decl_specs = get_identifier (UTAG_CLASS);
  decl_specs = build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE, decl_specs));
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("class"));

  field_decl = grokfield (input_filename, lineno,
			  field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (record, field_decl_chain, NULL_TREE);

  /* `struct objc_super *' */
  super_type = groktypename (build_tree_list (build_tree_list (NULL_TREE,
							       record),
					      build1 (INDIRECT_REF,
						      NULL_TREE, NULL_TREE)));
  return record;
}

/* struct objc_ivar {
     char *ivar_name;
     char *ivar_type;
     int ivar_offset;
   };  */

static tree
build_ivar_template ()
{
  tree objc_ivar_id, objc_ivar_record;
  tree decl_specs, field_decl, field_decl_chain;

  objc_ivar_id = get_identifier (UTAG_IVAR);
  objc_ivar_record = start_struct (RECORD_TYPE, objc_ivar_id);

  /* char *ivar_name; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("ivar_name"));

  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* char *ivar_type; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("ivar_type"));

  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* int ivar_offset; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("ivar_offset");

  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_record, field_decl_chain, NULL_TREE);

  return objc_ivar_record;
}

/* struct {
     int ivar_count;
     struct objc_ivar ivar_list[ivar_count];
   };  */

static tree
build_ivar_list_template (list_type, size)
     tree list_type;
     int size;
{
  tree objc_ivar_list_record;
  tree decl_specs, field_decl, field_decl_chain;

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULL_TREE);

  /* int ivar_count; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("ivar_count");

  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* struct objc_ivar ivar_list[]; */

  decl_specs = build_tree_list (NULL_TREE, list_type);
  field_decl = build_nt (ARRAY_REF, get_identifier ("ivar_list"),
			 build_int_2 (size, 0));

  field_decl = grokfield (input_filename, lineno,
			  field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain, NULL_TREE);

  return objc_ivar_list_record;
}

/* struct {
     int method_next;
     int method_count;
     struct objc_method method_list[method_count];
   };  */

static tree
build_method_list_template (list_type, size)
     tree list_type;
     int size;
{
  tree objc_ivar_list_record;
  tree decl_specs, field_decl, field_decl_chain;

  objc_ivar_list_record = start_struct (RECORD_TYPE, NULL_TREE);

  /* int method_next; */

  decl_specs
    = build_tree_list
      (NULL_TREE, 
       xref_tag (RECORD_TYPE,
		 get_identifier (UTAG_METHOD_PROTOTYPE_LIST)));
  field_decl
    = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("method_next"));
  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  /* int method_count; */

  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_INT]);
  field_decl = get_identifier ("method_count");

  field_decl = grokfield (input_filename, lineno,
			  field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* struct objc_method method_list[]; */

  decl_specs = build_tree_list (NULL_TREE, list_type);
  field_decl = build_nt (ARRAY_REF, get_identifier ("method_list"),
			 build_int_2 (size, 0));

  field_decl = grokfield (input_filename, lineno,
			  field_decl, decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (objc_ivar_list_record, field_decl_chain, NULL_TREE);

  return objc_ivar_list_record;
}

static tree
build_ivar_list_initializer (type, field_decl)
     tree type;
     tree field_decl;
{
  tree initlist = NULL_TREE;

  do
    {
      tree ivar = NULL_TREE;

      /* Set name.  */
      if (DECL_NAME (field_decl))
	ivar = tree_cons (NULL_TREE,
			  add_objc_string (DECL_NAME (field_decl),
					   meth_var_names),
			  ivar);
      else
	/* Unnamed bit-field ivar (yuck).  */
	ivar = tree_cons (NULL_TREE, build_int_2 (0, 0), ivar);

      /* Set type.  */
      encode_field_decl (field_decl,
			 obstack_object_size (&util_obstack),
			 OBJC_ENCODE_DONT_INLINE_DEFS);

      /* Null terminate string.  */
      obstack_1grow (&util_obstack, 0);
      ivar
	= tree_cons
	  (NULL_TREE,
	   add_objc_string (get_identifier (obstack_finish (&util_obstack)),
			    meth_var_types),
	   ivar);
      obstack_free (&util_obstack, util_firstobj);

      /* set offset */
      ivar
	= tree_cons
	  (NULL_TREE,
	   build_int_2 ((TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field_decl))
			 / BITS_PER_UNIT),
			0),
	   ivar);

      initlist = tree_cons (NULL_TREE, 
			    build_constructor (type, nreverse (ivar)),
			    initlist);

      field_decl = TREE_CHAIN (field_decl);
    }
  while (field_decl);

  return build_constructor (build_array_type (type, 0), nreverse (initlist));
}

static tree
generate_ivars_list (type, name, size, list)
     tree type;
     char *name;
     int size;
     tree list;
{
  tree sc_spec, decl_specs, decl, initlist;

  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, type, sc_spec);

  decl = start_decl (synth_id_with_class_suffix (name, implementation_context),
		     decl_specs, 1, NULL_TREE, NULL_TREE);

  initlist = build_tree_list (NULL_TREE, build_int_2 (size, 0));
  initlist = tree_cons (NULL_TREE, list, initlist);

  finish_decl (decl,
	       build_constructor (TREE_TYPE (decl), nreverse (initlist)),
	       NULL_TREE);

  return decl;
}

static void
generate_ivar_lists ()
{
  tree initlist, ivar_list_template, chain;
  tree cast, variable_length_type;
  int size;

  generating_instance_variables = 1;

  if (!objc_ivar_template)
    objc_ivar_template = build_ivar_template ();

  cast
    = build_tree_list
      (build_tree_list (NULL_TREE, xref_tag (RECORD_TYPE,
					 get_identifier (UTAG_IVAR_LIST))),
       NULL_TREE);
  variable_length_type = groktypename (cast);

  /* Only generate class variables for the root of the inheritance
     hierarchy since these will be the same for every class.  */

  if (CLASS_SUPER_NAME (implementation_template) == NULL_TREE
      && (chain = TYPE_FIELDS (objc_class_template)))
    {
      size = list_length (chain);

      ivar_list_template = build_ivar_list_template (objc_ivar_template, size);
      initlist = build_ivar_list_initializer (objc_ivar_template, chain);

      UOBJC_CLASS_VARIABLES_decl
	= generate_ivars_list (ivar_list_template, "_OBJC_CLASS_VARIABLES",
			       size, initlist);
      TREE_TYPE (UOBJC_CLASS_VARIABLES_decl) = variable_length_type;
    }
  else
    UOBJC_CLASS_VARIABLES_decl = 0;

  chain = CLASS_IVARS (implementation_template);
  if (chain)
    {
      size = list_length (chain);
      ivar_list_template = build_ivar_list_template (objc_ivar_template, size);
      initlist = build_ivar_list_initializer (objc_ivar_template, chain);

      UOBJC_INSTANCE_VARIABLES_decl
	= generate_ivars_list (ivar_list_template, "_OBJC_INSTANCE_VARIABLES",
			       size, initlist);
      TREE_TYPE (UOBJC_INSTANCE_VARIABLES_decl) = variable_length_type;
    }
  else
    UOBJC_INSTANCE_VARIABLES_decl = 0;

  generating_instance_variables = 0;
}

static tree
build_dispatch_table_initializer (type, entries)
     tree type;
     tree entries;
{
  tree initlist = NULL_TREE;

  do
    {
      tree elemlist = NULL_TREE;

      elemlist = tree_cons (NULL_TREE,
			    build_selector (METHOD_SEL_NAME (entries)),
			    NULL_TREE);

      elemlist = tree_cons (NULL_TREE,
			    add_objc_string (METHOD_ENCODING (entries),
					     meth_var_types),
			    elemlist);

      elemlist = tree_cons (NULL_TREE, 
			    build_unary_op (ADDR_EXPR,
					    METHOD_DEFINITION (entries), 1),
			    elemlist);

      initlist = tree_cons (NULL_TREE, 
			    build_constructor (type, nreverse (elemlist)),
			    initlist);

      entries = TREE_CHAIN (entries);
    }
  while (entries);

  return build_constructor (build_array_type (type, 0), nreverse (initlist));
}

/* To accomplish method prototyping without generating all kinds of
   inane warnings, the definition of the dispatch table entries were
   changed from:

   	struct objc_method { SEL _cmd; ...; id (*_imp)(); };
   to:
   	struct objc_method { SEL _cmd; ...; void *_imp; };  */

static tree
build_method_template ()
{
  tree _SLT_record;
  tree decl_specs, field_decl, field_decl_chain;

  _SLT_record = start_struct (RECORD_TYPE, get_identifier (UTAG_METHOD));

#ifdef OBJC_INT_SELECTORS
  /* unsigned int _cmd; */
  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_UNSIGNED],
			  NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_INT], decl_specs);
  field_decl = get_identifier ("_cmd");
#else /* not OBJC_INT_SELECTORS */
  /* struct objc_selector *_cmd; */
  decl_specs = tree_cons (NULL_TREE,
			  xref_tag (RECORD_TYPE,
				    get_identifier (TAG_SELECTOR)),
			  NULL_TREE);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("_cmd"));
#endif /* not OBJC_INT_SELECTORS */

  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  field_decl_chain = field_decl;

  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_CHAR], NULL_TREE);
  field_decl = build1 (INDIRECT_REF, NULL_TREE,
		       get_identifier ("method_types"));
  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  /* void *_imp; */

  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_VOID], NULL_TREE);
  field_decl = build1 (INDIRECT_REF, NULL_TREE, get_identifier ("_imp"));
  field_decl = grokfield (input_filename, lineno, field_decl,
			  decl_specs, NULL_TREE);
  chainon (field_decl_chain, field_decl);

  finish_struct (_SLT_record, field_decl_chain, NULL_TREE);

  return _SLT_record;
}


static tree
generate_dispatch_table (type, name, size, list)
     tree type;
     char *name;
     int size;
     tree list;
{
  tree sc_spec, decl_specs, decl, initlist;

  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, type, sc_spec);

  decl = start_decl (synth_id_with_class_suffix (name, implementation_context),
		     decl_specs, 1, NULL_TREE, NULL_TREE);

  initlist = build_tree_list (NULL_TREE, build_int_2 (0, 0));
  initlist = tree_cons (NULL_TREE, build_int_2 (size, 0), initlist);
  initlist = tree_cons (NULL_TREE, list, initlist);

  finish_decl (decl,
	       build_constructor (TREE_TYPE (decl), nreverse (initlist)),
	       NULL_TREE);

  return decl;
}

static void
generate_dispatch_tables ()
{
  tree initlist, chain, method_list_template;
  tree cast, variable_length_type;
  int size;

  if (!objc_method_template)
    objc_method_template = build_method_template ();

  cast
    = build_tree_list
      (build_tree_list (NULL_TREE,
			xref_tag (RECORD_TYPE,
				  get_identifier (UTAG_METHOD_LIST))),
       NULL_TREE);

  variable_length_type = groktypename (cast);

  chain = CLASS_CLS_METHODS (implementation_context);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_list_template (objc_method_template, size);
      initlist
	= build_dispatch_table_initializer (objc_method_template, chain);

      UOBJC_CLASS_METHODS_decl
	= generate_dispatch_table (method_list_template,
				   ((TREE_CODE (implementation_context)
				     == CLASS_IMPLEMENTATION_TYPE)
				    ? "_OBJC_CLASS_METHODS"
				    : "_OBJC_CATEGORY_CLASS_METHODS"),
				   size, initlist);
      TREE_TYPE (UOBJC_CLASS_METHODS_decl) = variable_length_type;
    }
  else
    UOBJC_CLASS_METHODS_decl = 0;

  chain = CLASS_NST_METHODS (implementation_context);
  if (chain)
    {
      size = list_length (chain);

      method_list_template
	= build_method_list_template (objc_method_template, size);
      initlist
	= build_dispatch_table_initializer (objc_method_template, chain);

      if (TREE_CODE (implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	UOBJC_INSTANCE_METHODS_decl
	  = generate_dispatch_table (method_list_template,
				     "_OBJC_INSTANCE_METHODS",
				     size, initlist);
      else
	/* We have a category.  */
	UOBJC_INSTANCE_METHODS_decl
	  = generate_dispatch_table (method_list_template,
				     "_OBJC_CATEGORY_INSTANCE_METHODS",
				     size, initlist);
      TREE_TYPE (UOBJC_INSTANCE_METHODS_decl) = variable_length_type;
    }
  else
    UOBJC_INSTANCE_METHODS_decl = 0;
}

static tree
generate_protocol_list (i_or_p)
     tree i_or_p;
{
  static tree cast_type = 0;
  tree initlist, decl_specs, sc_spec;
  tree refs_decl, expr_decl, lproto, e, plist;
  int size = 0;

  if (TREE_CODE (i_or_p) == CLASS_INTERFACE_TYPE
      || TREE_CODE (i_or_p) == CATEGORY_INTERFACE_TYPE)
    plist = CLASS_PROTOCOL_LIST (i_or_p);
  else if (TREE_CODE (i_or_p) == PROTOCOL_INTERFACE_TYPE)
    plist = PROTOCOL_LIST (i_or_p);
  else
    abort ();

  if (!cast_type)
    cast_type
      = groktypename
	(build_tree_list
	 (build_tree_list (NULL_TREE,
			   xref_tag (RECORD_TYPE,
				     get_identifier (UTAG_PROTOCOL))),
	  build1 (INDIRECT_REF, NULL_TREE, NULL_TREE)));

  /* Compute size.  */
  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    if (TREE_CODE (TREE_VALUE (lproto)) == PROTOCOL_INTERFACE_TYPE
	&& PROTOCOL_FORWARD_DECL (TREE_VALUE (lproto)))
      size++;

  /* Build initializer.  */
  initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), NULL_TREE);

  e = build_int_2 (size, 0);
  TREE_TYPE (e) = cast_type;
  initlist = tree_cons (NULL_TREE, e, initlist);

  for (lproto = plist; lproto; lproto = TREE_CHAIN (lproto))
    {
      tree pval = TREE_VALUE (lproto);

      if (TREE_CODE (pval) == PROTOCOL_INTERFACE_TYPE
	  && PROTOCOL_FORWARD_DECL (pval))
	{
	  e = build_unary_op (ADDR_EXPR, PROTOCOL_FORWARD_DECL (pval), 0);
	  initlist = tree_cons (NULL_TREE, e, initlist);
	}
    }

  /* static struct objc_protocol *refs[n]; */

  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, xref_tag (RECORD_TYPE,
					   get_identifier (UTAG_PROTOCOL)),
			  sc_spec);

  if (TREE_CODE (i_or_p) == PROTOCOL_INTERFACE_TYPE)
    expr_decl = build_nt (ARRAY_REF,
			  synth_id_with_class_suffix ("_OBJC_PROTOCOL_REFS",
						      i_or_p),
			  build_int_2 (size + 2, 0));
  else if (TREE_CODE (i_or_p) == CLASS_INTERFACE_TYPE)
    expr_decl = build_nt (ARRAY_REF,
			  synth_id_with_class_suffix ("_OBJC_CLASS_PROTOCOLS",
						      i_or_p),
			  build_int_2 (size + 2, 0));
  else if (TREE_CODE (i_or_p) == CATEGORY_INTERFACE_TYPE)
    expr_decl
      = build_nt (ARRAY_REF,
		  synth_id_with_class_suffix ("_OBJC_CATEGORY_PROTOCOLS",
					      i_or_p),
		  build_int_2 (size + 2, 0));

  expr_decl = build1 (INDIRECT_REF, NULL_TREE, expr_decl);

  refs_decl = start_decl (expr_decl, decl_specs, 1, NULL_TREE, NULL_TREE);

  finish_decl (refs_decl, build_constructor (TREE_TYPE (refs_decl),
					     nreverse (initlist)),
	       NULL_TREE);

  return refs_decl;
}

static tree
build_category_initializer (type, cat_name, class_name,
			    instance_methods, class_methods, protocol_list)
     tree type;
     tree cat_name;
     tree class_name;
     tree instance_methods;
     tree class_methods;
     tree protocol_list;
{
  tree initlist = NULL_TREE, expr;

  initlist = tree_cons (NULL_TREE, cat_name, initlist);
  initlist = tree_cons (NULL_TREE, class_name, initlist);

  if (!instance_methods)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, instance_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }
  if (!class_methods)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, class_methods, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  /* protocol_list = */
  if (!protocol_list)
     initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
     {
	static tree cast_type2;

	if (!cast_type2)
	  cast_type2
	    = groktypename
	      (build_tree_list
	       (build_tree_list (NULL_TREE,
				 xref_tag (RECORD_TYPE,
					   get_identifier (UTAG_PROTOCOL))),
		build1 (INDIRECT_REF, NULL_TREE,
			build1 (INDIRECT_REF, NULL_TREE, NULL_TREE))));

	expr = build_unary_op (ADDR_EXPR, protocol_list, 0);
	TREE_TYPE (expr) = cast_type2;
	initlist = tree_cons (NULL_TREE, expr, initlist);
     }

  return build_constructor (type, nreverse (initlist));
}

/* struct objc_class {
     struct objc_class *isa;
     struct objc_class *super_class;
     char *name;
     long version;
     long info;
     long instance_size;
     struct objc_ivar_list *ivars;
     struct objc_method_list *methods;
     if (flag_next_runtime)
       struct objc_cache *cache;
     else {
       struct sarray *dtable;
       struct objc_class *subclass_list;
       struct objc_class *sibling_class;
     }
     struct objc_protocol_list *protocols;
   };  */

static tree
build_shared_structure_initializer (type, isa, super, name, size, status,
				    dispatch_table, ivar_list, protocol_list)
     tree type;
     tree isa;
     tree super;
     tree name;
     tree size;
     int status;
     tree dispatch_table;
     tree ivar_list;
     tree protocol_list;
{
  tree initlist = NULL_TREE, expr;

  /* isa = */
  initlist = tree_cons (NULL_TREE, isa, initlist);

  /* super_class = */
  initlist = tree_cons (NULL_TREE, super, initlist);

  /* name = */
  initlist = tree_cons (NULL_TREE, default_conversion (name), initlist);

  /* version = */
  initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);

  /* info = */
  initlist = tree_cons (NULL_TREE, build_int_2 (status, 0), initlist);

  /* instance_size = */
  initlist = tree_cons (NULL_TREE, size, initlist);

  /* objc_ivar_list = */
  if (!ivar_list)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, ivar_list, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  /* objc_method_list = */
  if (!dispatch_table)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      expr = build_unary_op (ADDR_EXPR, dispatch_table, 0);
      initlist = tree_cons (NULL_TREE, expr, initlist);
    }

  if (flag_next_runtime)
    /* method_cache = */
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
    {
      /* dtable = */
      initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);

      /* subclass_list = */
      initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);

      /* sibling_class = */
      initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
    }

  /* protocol_list = */
  if (! protocol_list)
    initlist = tree_cons (NULL_TREE, build_int_2 (0, 0), initlist);
  else
     {
     static tree cast_type2;

     if (!cast_type2)
        cast_type2
	  = groktypename
	    (build_tree_list
	     (build_tree_list (NULL_TREE,
			       xref_tag (RECORD_TYPE,
					 get_identifier (UTAG_PROTOCOL))),
	      build1 (INDIRECT_REF, NULL_TREE,
		      build1 (INDIRECT_REF, NULL_TREE, NULL_TREE))));

     expr = build_unary_op (ADDR_EXPR, protocol_list, 0);
     TREE_TYPE (expr) = cast_type2;
     initlist = tree_cons (NULL_TREE, expr, initlist);
     }

  return build_constructor (type, nreverse (initlist));
}

/* static struct objc_category _OBJC_CATEGORY_<name> = { ... };  */

static void
generate_category (cat)
     tree cat;
{
  tree sc_spec, decl_specs, decl;
  tree initlist, cat_name_expr, class_name_expr;
  tree protocol_decl, category;

  add_class_reference (CLASS_NAME (cat));
  cat_name_expr = add_objc_string (CLASS_SUPER_NAME (cat), class_names);

  class_name_expr = add_objc_string (CLASS_NAME (cat), class_names);

  category = CLASS_CATEGORY_LIST (implementation_template);

  /* find the category interface from the class it is associated with */
  while (category)
    {
      if (CLASS_SUPER_NAME (cat) == CLASS_SUPER_NAME (category))
	break;
      category = CLASS_CATEGORY_LIST (category);
    }

  if (category && CLASS_PROTOCOL_LIST (category))
    {
      generate_protocol_references (CLASS_PROTOCOL_LIST (category));
      protocol_decl = generate_protocol_list (category);
    }
  else
    protocol_decl = 0;

  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
  decl_specs = tree_cons (NULL_TREE, objc_category_template, sc_spec);

  decl = start_decl (synth_id_with_class_suffix ("_OBJC_CATEGORY",
						 implementation_context),
		     decl_specs, 1, NULL_TREE, NULL_TREE);

  initlist = build_category_initializer (TREE_TYPE (decl),
					 cat_name_expr, class_name_expr,
					 UOBJC_INSTANCE_METHODS_decl,
					 UOBJC_CLASS_METHODS_decl,
					 protocol_decl);

  TREE_USED (decl) = 1;
  finish_decl (decl, initlist, NULL_TREE);
}

/* static struct objc_class _OBJC_METACLASS_Foo={ ... };
   static struct objc_class _OBJC_CLASS_Foo={ ... };  */

static void
generate_shared_structures ()
{
  tree sc_spec, decl_specs, decl;
  tree name_expr, super_expr, root_expr;
  tree my_root_id = NULL_TREE, my_super_id = NULL_TREE;
  tree cast_type, initlist, protocol_decl;

  my_super_id = CLASS_SUPER_NAME (implementation_template);
  if (my_super_id)
    {
      add_class_reference (my_super_id);

      /* Compute "my_root_id" - this is required for code generation.
         the "isa" for all meta class structures points to the root of
         the inheritance hierarchy (e.g. "__Object")...  */
      my_root_id = my_super_id;
      do
	{
	  tree my_root_int = lookup_interface (my_root_id);

	  if (my_root_int && CLASS_SUPER_NAME (my_root_int))
	    my_root_id = CLASS_SUPER_NAME (my_root_int);
	  else
	    break;
	}
      while (1);
    }
  else
    /* No super class.  */
    my_root_id = CLASS_NAME (implementation_template);

  cast_type
    = groktypename (build_tree_list (build_tree_list (NULL_TREE,
						      objc_class_template),
				     build1 (INDIRECT_REF,
					     NULL_TREE, NULL_TREE)));

  name_expr = add_objc_string (CLASS_NAME (implementation_template),
			       class_names);

  /* Install class `isa' and `super' pointers at runtime.  */
  if (my_super_id)
    {
      super_expr = add_objc_string (my_super_id, class_names);
      super_expr = build_c_cast (cast_type, super_expr); /* cast! */
    }
  else
    super_expr = build_int_2 (0, 0);

  root_expr = add_objc_string (my_root_id, class_names);
  root_expr = build_c_cast (cast_type, root_expr); /* cast! */

  if (CLASS_PROTOCOL_LIST (implementation_template))
    {
      generate_protocol_references
	(CLASS_PROTOCOL_LIST (implementation_template));
      protocol_decl = generate_protocol_list (implementation_template);
    }
  else
    protocol_decl = 0;

  /* static struct objc_class _OBJC_METACLASS_Foo = { ... }; */

  sc_spec = build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]);
  decl_specs = tree_cons (NULL_TREE, objc_class_template, sc_spec);

  decl = start_decl (DECL_NAME (UOBJC_METACLASS_decl), decl_specs, 1,
		     NULL_TREE, NULL_TREE);

  initlist
    = build_shared_structure_initializer
      (TREE_TYPE (decl),
       root_expr, super_expr, name_expr,
       build_int_2 ((TREE_INT_CST_LOW (TYPE_SIZE (objc_class_template))
		    / BITS_PER_UNIT),
		    0),
       2 /*CLS_META*/,
       UOBJC_CLASS_METHODS_decl,
       UOBJC_CLASS_VARIABLES_decl,
       protocol_decl);

  finish_decl (decl, initlist, NULL_TREE);

  /* static struct objc_class _OBJC_CLASS_Foo={ ... }; */

  decl = start_decl (DECL_NAME (UOBJC_CLASS_decl), decl_specs, 1,
		     NULL_TREE, NULL_TREE);

  initlist
    = build_shared_structure_initializer
      (TREE_TYPE (decl),
       build_unary_op (ADDR_EXPR, UOBJC_METACLASS_decl, 0),
       super_expr, name_expr,
       build_int_2
       ((TREE_INT_CST_LOW
	 (TYPE_SIZE (CLASS_STATIC_TEMPLATE (implementation_template)))
	 / BITS_PER_UNIT),
	0),
       1 /*CLS_FACTORY*/,
       UOBJC_INSTANCE_METHODS_decl,
       UOBJC_INSTANCE_VARIABLES_decl,
       protocol_decl);

  finish_decl (decl, initlist, NULL_TREE);
}

static tree
synth_id_with_class_suffix (preamble, ctxt)
     char *preamble;
     tree ctxt;
{
  char *string;
  if (TREE_CODE (ctxt) == CLASS_IMPLEMENTATION_TYPE
      || TREE_CODE (ctxt) == CLASS_INTERFACE_TYPE)
    {
      char *class_name
	= IDENTIFIER_POINTER (CLASS_NAME (implementation_context));
      string = (char *) alloca (strlen (preamble) + strlen (class_name) + 3);
      sprintf (string, "%s_%s", preamble,
	       IDENTIFIER_POINTER (CLASS_NAME (ctxt)));
    }
  else if (TREE_CODE (ctxt) == CATEGORY_IMPLEMENTATION_TYPE
	   || TREE_CODE (ctxt) == CATEGORY_INTERFACE_TYPE)
    {
      /* We have a category.  */
      char *class_name
	= IDENTIFIER_POINTER (CLASS_NAME (implementation_context));
      char *class_super_name
	= IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context));
      string = (char *) alloca (strlen (preamble)
				+ strlen (class_name)
				+ strlen (class_super_name)
				+ 3);
      sprintf (string, "%s_%s_%s", preamble, class_name, class_super_name);
    }
  else if (TREE_CODE (ctxt) == PROTOCOL_INTERFACE_TYPE)
    {
      char *protocol_name = IDENTIFIER_POINTER (PROTOCOL_NAME (ctxt));
      string
	= (char *) alloca (strlen (preamble) + strlen (protocol_name) + 3);
      sprintf (string, "%s_%s", preamble, protocol_name);
    }
  return get_identifier (string);
}

static int
is_objc_type_qualifier (node)
     tree node;
{
  return (TREE_CODE (node) == IDENTIFIER_NODE
	  && (node == ridpointers [(int) RID_CONST]
	      || node == ridpointers [(int) RID_VOLATILE]
	      || node == ridpointers [(int) RID_IN]
	      || node == ridpointers [(int) RID_OUT]
	      || node == ridpointers [(int) RID_INOUT]
	      || node == ridpointers [(int) RID_BYCOPY]
	      || node == ridpointers [(int) RID_ONEWAY]));
}

/* If type is empty or only type qualifiers are present, add default
   type of id (otherwise grokdeclarator will default to int).  */

static tree
adjust_type_for_id_default (type)
     tree type;
{
  tree declspecs, chain;

  if (!type)
    return build_tree_list (build_tree_list (NULL_TREE, objc_object_reference),
			    build1 (INDIRECT_REF, NULL_TREE, NULL_TREE));

  declspecs = TREE_PURPOSE (type);

  /* Determine if a typespec is present.  */
  for (chain = declspecs;
       chain;
       chain = TREE_CHAIN (chain))
    {
      if (!is_objc_type_qualifier (TREE_VALUE (chain)))
	return type;
    }

  return build_tree_list (tree_cons (NULL_TREE, objc_object_reference,
				     declspecs),
			  build1 (INDIRECT_REF, NULL_TREE, NULL_TREE));
}

/*   Usage:
  		keyworddecl:
  			selector ':' '(' typename ')' identifier
  
     Purpose:
  		Transform an Objective-C keyword argument into
  		the C equivalent parameter declarator.
  
     In:	key_name, an "identifier_node" (optional).
  		arg_type, a  "tree_list" (optional).
  		arg_name, an "identifier_node".
  
     Note:	It would be really nice to strongly type the preceding
  		arguments in the function prototype; however, then I
  		could not use the "accessor" macros defined in "tree.h".
  
     Out:	an instance of "keyword_decl".  */

tree
build_keyword_decl (key_name, arg_type, arg_name)
     tree key_name;
     tree arg_type;
     tree arg_name;
{
  tree keyword_decl;

  /* If no type is specified, default to "id".  */
  arg_type = adjust_type_for_id_default (arg_type);

  keyword_decl = make_node (KEYWORD_DECL);

  TREE_TYPE (keyword_decl) = arg_type;
  KEYWORD_ARG_NAME (keyword_decl) = arg_name;
  KEYWORD_KEY_NAME (keyword_decl) = key_name;

  return keyword_decl;
}

/* Given a chain of keyword_decl's, synthesize the full keyword selector.  */

static tree
build_keyword_selector (selector)
     tree selector;
{
  int len = 0;
  tree key_chain, key_name;
  char *buf;

  for (key_chain = selector; key_chain; key_chain = TREE_CHAIN (key_chain))
    {
      if (TREE_CODE (selector) == KEYWORD_DECL)
	key_name = KEYWORD_KEY_NAME (key_chain);
      else if (TREE_CODE (selector) == TREE_LIST)
	key_name = TREE_PURPOSE (key_chain);

      if (key_name)
	len += IDENTIFIER_LENGTH (key_name) + 1;
      else
	/* Just a ':' arg.  */
	len++;
    }

  buf = (char *)alloca (len + 1);
  bzero (buf, len + 1);

  for (key_chain = selector; key_chain; key_chain = TREE_CHAIN (key_chain))
    {
      if (TREE_CODE (selector) == KEYWORD_DECL)
	key_name = KEYWORD_KEY_NAME (key_chain);
      else if (TREE_CODE (selector) == TREE_LIST)
	key_name = TREE_PURPOSE (key_chain);

      if (key_name)
	strcat (buf, IDENTIFIER_POINTER (key_name));
      strcat (buf, ":");
    }

  return get_identifier (buf);
}

/* Used for declarations and definitions.  */

tree
build_method_decl (code, ret_type, selector, add_args)
     enum tree_code code;
     tree ret_type;
     tree selector;
     tree add_args;
{
  tree method_decl;

  /* If no type is specified, default to "id".  */
  ret_type = adjust_type_for_id_default (ret_type);

  method_decl = make_node (code);
  TREE_TYPE (method_decl) = ret_type;

  /* If we have a keyword selector, create an identifier_node that
     represents the full selector name (`:' included)...  */
  if (TREE_CODE (selector) == KEYWORD_DECL)
    {
      METHOD_SEL_NAME (method_decl) = build_keyword_selector (selector);
      METHOD_SEL_ARGS (method_decl) = selector;
      METHOD_ADD_ARGS (method_decl) = add_args;
    }
  else
    {
      METHOD_SEL_NAME (method_decl) = selector;
      METHOD_SEL_ARGS (method_decl) = NULL_TREE;
      METHOD_ADD_ARGS (method_decl) = NULL_TREE;
    }

  return method_decl;
}

#define METHOD_DEF 0
#define METHOD_REF 1

/* Used by `build_message_expr' and `comp_method_types'.  Return an
   argument list for method METH.  CONTEXT is either METHOD_DEF or
   METHOD_REF, saying whether we are trying to define a method or call
   one.  SUPERFLAG says this is for a send to super; this makes a
   difference for the NeXT calling sequence in which the lookup and
   the method call are done together.  */

static tree
get_arg_type_list (meth, context, superflag)
     tree meth;
     int context;
     int superflag;
{
  tree arglist, akey;

  /* Receiver type.  */
  if (flag_next_runtime && superflag)
    arglist = build_tree_list (NULL_TREE, super_type);
  else if (context == METHOD_DEF)
    arglist = build_tree_list (NULL_TREE, TREE_TYPE (self_decl));
  else
    arglist = build_tree_list (NULL_TREE, id_type);

  /* Selector type - will eventually change to `int'.  */
  chainon (arglist, build_tree_list (NULL_TREE, selector_type));

  /* Build a list of argument types.  */
  for (akey = METHOD_SEL_ARGS (meth); akey; akey = TREE_CHAIN (akey))
    {
      tree arg_decl = groktypename_in_parm_context (TREE_TYPE (akey));
      chainon (arglist, build_tree_list (NULL_TREE, TREE_TYPE (arg_decl)));
    }

  if (METHOD_ADD_ARGS (meth) == (tree)1)
    /* We have a `, ...' immediately following the selector,
       finalize the arglist...simulate get_parm_info (0).  */
    ;
  else if (METHOD_ADD_ARGS (meth))
    {
      /* we have a variable length selector */
      tree add_arg_list = TREE_CHAIN (METHOD_ADD_ARGS (meth));
      chainon (arglist, add_arg_list);
    }
  else
    /* finalize the arglist...simulate get_parm_info (1) */
    chainon (arglist, build_tree_list (NULL_TREE, void_type_node));

  return arglist;
}

static tree
check_duplicates (hsh)
     hash hsh;
{
  tree meth = NULL_TREE;

  if (hsh)
    {
      meth = hsh->key;

      if (hsh->list)
        {
	  /* We have two methods with the same name and different types.  */
	  attr loop;
	  char type = (TREE_CODE (meth) == INSTANCE_METHOD_DECL) ? '-' : '+';

	  warning ("multiple declarations for method `%s'",
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));

	  warn_with_method ("using", type, meth);
	  for (loop = hsh->list; loop; loop = loop->next)
	    warn_with_method ("also found", type, loop->value);
        }
    }
  return meth;
}

/* If RECEIVER is a class reference, return the identifier node for the
   referenced class.  RECEIVER is created by get_class_reference, so we
   check the exact form created depending on which runtimes are used.  */

static tree
receiver_is_class_object (receiver)
      tree receiver;
{
  tree chain, exp, arg;
  if (flag_next_runtime)
    {
      /* The receiver is a variable created by build_class_reference_decl.  */
      if (TREE_CODE (receiver) == VAR_DECL
	  && TREE_TYPE (receiver) == objc_class_type)
	/* Look up the identifier.  */
	for (chain = cls_ref_chain; chain; chain = TREE_CHAIN (chain))
	  if (TREE_PURPOSE (chain) == receiver)
	    return TREE_VALUE (chain);
    }
  else
    {
      /* The receiver is a function call that returns an id.  Check if
	 it is a call to objc_getClass, if so, pick up the class name.  */
      if ((exp = TREE_OPERAND (receiver, 0))
	  && TREE_CODE (exp) == ADDR_EXPR
	  && (exp = TREE_OPERAND (exp, 0))
	  && TREE_CODE (exp) == FUNCTION_DECL
	  && exp == objc_get_class_decl
	  /* we have a call to objc_getClass! */
	  && (arg = TREE_OPERAND (receiver, 1))
	  && TREE_CODE (arg) == TREE_LIST
	  && (arg = TREE_VALUE (arg)))
	{
	  STRIP_NOPS (arg);
	  if (TREE_CODE (arg) == ADDR_EXPR
	      && (arg = TREE_OPERAND (arg, 0))
	      && TREE_CODE (arg) == STRING_CST)
	    /* Finally, we have the class name.  */
	    return get_identifier (TREE_STRING_POINTER (arg));
	}
    }
  return 0;
}

/* If we are currently building a message expr, this holds
   the identifier of the selector of the message.  This is
   used when printing warnings about argument mismatches.  */

static tree building_objc_message_expr = 0;

tree
maybe_building_objc_message_expr ()
{
  return building_objc_message_expr;
}

/* Construct an expression for sending a message.
   MESS has the object to send to in TREE_PURPOSE
   and the argument list (including selector) in TREE_VALUE.

   (*(<abstract_decl>(*)())_msg)(receiver, selTransTbl[n], ...);
   (*(<abstract_decl>(*)())_msgSuper)(receiver, selTransTbl[n], ...);  */

tree
build_message_expr (mess)
     tree mess;
{
  tree receiver = TREE_PURPOSE (mess);
  tree selector, self_object;
  tree rtype, sel_name;
  tree args = TREE_VALUE (mess);
  tree method_params = NULL_TREE;
  tree method_prototype = NULL_TREE;
  tree retval;
  int statically_typed = 0, statically_allocated = 0;
  tree class_ident = 0;

  /* 1 if this is sending to the superclass.  */
  int super;

  if (!doing_objc_thang)
    objc_fatal ();

  if (TREE_CODE (receiver) == ERROR_MARK)
    return error_mark_node;

  /* Determine receiver type.  */
  rtype = TREE_TYPE (receiver);
  super = IS_SUPER (rtype);

  if (! super)
    {
      if (TREE_STATIC_TEMPLATE (rtype))
	statically_allocated = 1;
      else if (TREE_CODE (rtype) == POINTER_TYPE
	       && TREE_STATIC_TEMPLATE (TREE_TYPE (rtype)))
	statically_typed = 1;
      else if ((flag_next_runtime
		|| (TREE_CODE (receiver) == CALL_EXPR && IS_ID (rtype)))
	       && (class_ident = receiver_is_class_object (receiver)))
	;
      else if (! IS_ID (rtype)
	       /* Allow any type that matches objc_class_type.  */
	       && ! comptypes (rtype, objc_class_type))
	{
	  bzero (errbuf, BUFSIZE);
	  warning ("invalid receiver type `%s'",
		   gen_declaration (rtype, errbuf));
	}

      if (statically_allocated)
	receiver = build_unary_op (ADDR_EXPR, receiver, 0);

      /* Don't evaluate the receiver twice.  */
      receiver = save_expr (receiver);
      self_object = receiver;
    }
  else
    /* If sending to `super', use current self as the object.  */
    self_object = self_decl;

  /* Obtain the full selector name.  */

  if (TREE_CODE (args) == IDENTIFIER_NODE)
    /* A unary selector.  */
    sel_name = args;
  else if (TREE_CODE (args) == TREE_LIST)
    sel_name = build_keyword_selector (args);

  /* Build the parameter list to give to the method.  */

  method_params = NULL_TREE;
  if (TREE_CODE (args) == TREE_LIST)
    {
      tree chain = args, prev = NULL_TREE;

      /* We have a keyword selector--check for comma expressions.  */
      while (chain)
	{
	  tree element = TREE_VALUE (chain);

	  /* We have a comma expression, must collapse...  */
	  if (TREE_CODE (element) == TREE_LIST)
	    {
	      if (prev)
		TREE_CHAIN (prev) = element;
	      else
		args = element;
	    }
	  prev = chain;
	  chain = TREE_CHAIN (chain);
        }
      method_params = args;
    }

  /* Determine operation return type.  */

  if (IS_SUPER (rtype))
    {
      tree iface;

      if (CLASS_SUPER_NAME (implementation_template))
	{
	  iface
	    = lookup_interface (CLASS_SUPER_NAME (implementation_template));

	  if (TREE_CODE (method_context) == INSTANCE_METHOD_DECL)
	    method_prototype = lookup_instance_method_static (iface, sel_name);
	  else
	    method_prototype = lookup_class_method_static (iface, sel_name);

	  if (iface && !method_prototype)
	    warning ("`%s' does not respond to `%s'",
		     IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_template)),
		     IDENTIFIER_POINTER (sel_name));
	}
      else
	{
	  error ("no super class declared in interface for `%s'",
		 IDENTIFIER_POINTER (CLASS_NAME (implementation_template)));
	  return error_mark_node;
	}

    }
  else if (statically_allocated)
    {
      tree ctype = TREE_TYPE (rtype);
      tree iface = lookup_interface (TYPE_NAME (rtype));

      if (iface)
	method_prototype = lookup_instance_method_static (iface, sel_name);

      if (! method_prototype && TYPE_PROTOCOL_LIST (ctype))
	method_prototype
	  = lookup_method_in_protocol_list (TYPE_PROTOCOL_LIST (ctype),
					    sel_name, 0);

      if (!method_prototype)
	warning ("`%s' does not respond to `%s'",
		 IDENTIFIER_POINTER (TYPE_NAME (rtype)),
		 IDENTIFIER_POINTER (sel_name));
    }
  else if (statically_typed)
    {
      tree ctype = TREE_TYPE (rtype);

      /* `self' is now statically_typed.  All methods should be visible
         within the context of the implementation.  */
      if (implementation_context
	  && CLASS_NAME (implementation_context) == TYPE_NAME (ctype))
	{
	  method_prototype
	    = lookup_instance_method_static (implementation_template,
					     sel_name);

	  if (! method_prototype && TYPE_PROTOCOL_LIST (ctype))
	    method_prototype
	      = lookup_method_in_protocol_list (TYPE_PROTOCOL_LIST (ctype),
						sel_name, 0);

	  if (! method_prototype
	      && implementation_template != implementation_context)
	    /* The method is not published in the interface.  Check
               locally.  */
	    method_prototype
	      = lookup_method (CLASS_NST_METHODS (implementation_context),
			       sel_name);
	}
      else
	{
	  tree iface;

	  if ((iface = lookup_interface (TYPE_NAME (ctype))))
	    method_prototype = lookup_instance_method_static (iface, sel_name);

          if (! method_prototype)
	    {
	      tree protocol_list = TYPE_PROTOCOL_LIST (ctype);
	      if (protocol_list)
		method_prototype
		  = lookup_method_in_protocol_list (protocol_list,
						    sel_name, 0);
	    }
	}

      if (!method_prototype)
        warning ("`%s' does not respond to `%s'",
		 IDENTIFIER_POINTER (TYPE_NAME (ctype)),
		 IDENTIFIER_POINTER (sel_name));
    }
  else if (class_ident)
    {
      if (implementation_context
	  && CLASS_NAME (implementation_context) == class_ident)
	{
	  method_prototype
	    = lookup_class_method_static (implementation_template, sel_name);

	  if (!method_prototype
	      && implementation_template != implementation_context)
	    /* The method is not published in the interface. Check
               locally.  */
	    method_prototype
	      = lookup_method (CLASS_CLS_METHODS (implementation_context),
			       sel_name);
	}
      else
	{
	  tree iface;

	  if ((iface = lookup_interface (class_ident)))
	    method_prototype = lookup_class_method_static (iface, sel_name);
	}

      if (!method_prototype)
	{
	  warning ("cannot find class (factory) method.");
	  warning ("return type for `%s' defaults to id",
		   IDENTIFIER_POINTER (sel_name));
	}
    }
  else if (IS_PROTOCOL_QUALIFIED_ID (rtype))
    {
      /* An anonymous object that has been qualified with a protocol.  */

      tree protocol_list = TYPE_PROTOCOL_LIST (rtype);

      method_prototype = lookup_method_in_protocol_list (protocol_list,
							 sel_name, 0);

      if (!method_prototype)
	{
          hash hsh;

	  warning ("method `%s' not implemented by protocol.",
		   IDENTIFIER_POINTER (sel_name));

          /* Try and find the method signature in the global pools.  */

          if (!(hsh = hash_lookup (nst_method_hash_list, sel_name)))
	    hsh = hash_lookup (cls_method_hash_list, sel_name);

          if (!(method_prototype = check_duplicates (hsh)))
	    warning ("return type defaults to id");
	}
    }
  else
    {
      hash hsh;

      /* We think we have an instance...loophole: extern id Object; */
      hsh = hash_lookup (nst_method_hash_list, sel_name);
      if (!hsh)
	/* For various loopholes, like sending messages to self in a
	   factory context.  */
	hsh = hash_lookup (cls_method_hash_list, sel_name);

      method_prototype = check_duplicates (hsh);
      if (!method_prototype)
	{
	  warning ("cannot find method.");
	  warning ("return type for `%s' defaults to id",
		   IDENTIFIER_POINTER (sel_name));
	}
    }

  /* Save the selector name for printing error messages.  */
  building_objc_message_expr = sel_name;

  /* Build the parameters list for looking up the method.
     These are the object itself and the selector.  */

  if (flag_typed_selectors)
    selector = build_typed_selector_reference (sel_name, method_prototype);
  else
    selector = build_selector_reference (sel_name);

  retval = build_objc_method_call (super, method_prototype,
				   receiver, self_object,
				   selector, method_params);

  building_objc_message_expr = 0;

  return retval;
}

/* Build a tree expression to send OBJECT the operation SELECTOR,
   looking up the method on object LOOKUP_OBJECT (often same as OBJECT),
   assuming the method has prototype METHOD_PROTOTYPE.
   (That is an INSTANCE_METHOD_DECL or CLASS_METHOD_DECL.)
   Use METHOD_PARAMS as list of args to pass to the method.
   If SUPER_FLAG is nonzero, we look up the superclass's method.  */

static tree
build_objc_method_call (super_flag, method_prototype, lookup_object, object,
			selector, method_params)
     int super_flag;
     tree method_prototype, lookup_object, object, selector, method_params;
{
  tree sender = (super_flag ? umsg_super_decl : umsg_decl);
  tree rcv_p = (super_flag
		? build_pointer_type (xref_tag (RECORD_TYPE,
						get_identifier (TAG_SUPER)))
		: id_type);

  if (flag_next_runtime)
    {
      if (! method_prototype)
	{
	  method_params = tree_cons (NULL_TREE, lookup_object,
				     tree_cons (NULL_TREE, selector,
						method_params));
	  assemble_external (sender);
	  return build_function_call (sender, method_params);
	}
      else
	{
	  /* This is a real kludge, but it is used only for the Next.
	     Clobber the data type of SENDER temporarily to accept
	     all the arguments for this operation, and to return
	     whatever this operation returns.  */
	  tree arglist = NULL_TREE;
	  tree retval;

	  /* Save the proper contents of SENDER's data type.  */
	  tree savarg = TYPE_ARG_TYPES (TREE_TYPE (sender));
	  tree savret = TREE_TYPE (TREE_TYPE (sender));

	  /* Install this method's argument types.  */
	  arglist = get_arg_type_list (method_prototype, METHOD_REF,
				       super_flag);
	  TYPE_ARG_TYPES (TREE_TYPE (sender)) = arglist;

	  /* Install this method's return type.  */
	  TREE_TYPE (TREE_TYPE (sender))
	    = groktypename (TREE_TYPE (method_prototype));

	  /* Call SENDER with all the parameters.  This will do type
	     checking using the arg types for this method.  */
	  method_params = tree_cons (NULL_TREE, lookup_object,
				     tree_cons (NULL_TREE, selector,
						method_params));
	  assemble_external (sender);
	  retval = build_function_call (sender, method_params);

	  /* Restore SENDER's return/argument types.  */
	  TYPE_ARG_TYPES (TREE_TYPE (sender)) = savarg;
	  TREE_TYPE (TREE_TYPE (sender)) = savret;
	  return retval;
	}
    }
  else
    {
      /* This is the portable way.
	 First call the lookup function to get a pointer to the method,
	 then cast the pointer, then call it with the method arguments.  */
      tree method;

      /* Avoid trouble since we may evaluate each of these twice.  */
      object = save_expr (object);
      selector = save_expr (selector);

      lookup_object = build_c_cast (rcv_p, lookup_object);

      assemble_external (sender);
      method
	= build_function_call (sender,
			       tree_cons (NULL_TREE, lookup_object,
					  tree_cons (NULL_TREE, selector,
						     NULL_TREE)));

      /* If we have a method prototype, construct the data type this
	 method needs, and cast what we got from SENDER into a pointer
	 to that type.  */
      if (method_prototype)
	{
	  tree arglist = get_arg_type_list (method_prototype, METHOD_REF,
					    super_flag);
	  tree valtype = groktypename (TREE_TYPE (method_prototype));
	  tree fake_function_type = build_function_type (valtype, arglist);
	  TREE_TYPE (method) = build_pointer_type (fake_function_type);
	}
      else
	TREE_TYPE (method)
	  = build_pointer_type (build_function_type (ptr_type_node, NULL_TREE));

      /* Pass the object to the method.  */
      assemble_external (method);
      return build_function_call (method,
				  tree_cons (NULL_TREE, object,
					     tree_cons (NULL_TREE, selector,
							method_params)));
    }
}

static void
build_protocol_reference (p)
     tree p;
{
  tree decl, ident, ptype;

  push_obstacks_nochange ();
  end_temporary_allocation ();

  /* extern struct objc_protocol _OBJC_PROTOCOL_<mumble>; */

  ident = synth_id_with_class_suffix ("_OBJC_PROTOCOL", p);
  ptype
    = groktypename (build_tree_list (build_tree_list (NULL_TREE,
						      objc_protocol_template),
				     NULL_TREE));

  if (IDENTIFIER_GLOBAL_VALUE (ident))
    decl = IDENTIFIER_GLOBAL_VALUE (ident); /* Set by pushdecl.  */
  else
    {
      decl = build_decl (VAR_DECL, ident, ptype);
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
      TREE_USED (decl) = 1;
      DECL_ARTIFICIAL (decl) = 1;

      make_decl_rtl (decl, 0, 1);
      pushdecl_top_level (decl);
   }

  PROTOCOL_FORWARD_DECL (p) = decl;
  pop_obstacks ();
}

tree
build_protocol_expr (protoname)
     tree protoname;
{
  tree expr;
  tree p;

  if (!doing_objc_thang)
    objc_fatal ();

  p = lookup_protocol (protoname);

  if (!p)
    {
      error ("Cannot find protocol declaration for `%s'",
	     IDENTIFIER_POINTER (protoname));
      return error_mark_node;
    }

  if (!PROTOCOL_FORWARD_DECL (p))
    build_protocol_reference (p);

  expr = build_unary_op (ADDR_EXPR, PROTOCOL_FORWARD_DECL (p), 0);

  TREE_TYPE (expr) = protocol_type;

  return expr;
}

tree
build_selector_expr (selnamelist)
     tree selnamelist;
{
  tree selname;

  if (!doing_objc_thang)
    objc_fatal ();

  /* Obtain the full selector name.  */
  if (TREE_CODE (selnamelist) == IDENTIFIER_NODE)
    /* A unary selector.  */
    selname = selnamelist;
  else if (TREE_CODE (selnamelist) == TREE_LIST)
    selname = build_keyword_selector (selnamelist);

  if (flag_typed_selectors)
    return build_typed_selector_reference (selname, 0);
  else
    return build_selector_reference (selname);
}

tree
build_encode_expr (type)
     tree type;
{
  tree result;
  char *string;

  if (!doing_objc_thang)
    objc_fatal ();

  encode_type (type, obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);
  obstack_1grow (&util_obstack, 0);    /* null terminate string */
  string = obstack_finish (&util_obstack);

  /* Synthesize a string that represents the encoded struct/union.  */
  result = my_build_string (strlen (string) + 1, string);
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

tree
build_ivar_reference (id)
     tree id;
{
  if (TREE_CODE (method_context) == CLASS_METHOD_DECL)
    {
      /* Historically, a class method that produced objects (factory
	 method) would assign `self' to the instance that it
	 allocated.  This would effectively turn the class method into
	 an instance method.  Following this assignment, the instance
	 variables could be accessed.  That practice, while safe,
	 violates the simple rule that a class method should not refer
	 to an instance variable.  It's better to catch the cases
	 where this is done unknowingly than to support the above
	 paradigm.  */
      warning ("instance variable `%s' accessed in class method",
	       IDENTIFIER_POINTER (id));
      TREE_TYPE (self_decl) = instance_type; /* cast */
    }

  return build_component_ref (build_indirect_ref (self_decl, "->"), id);
}

#define HASH_ALLOC_LIST_SIZE	170
#define ATTR_ALLOC_LIST_SIZE	170
#define SIZEHASHTABLE 		257

/* make positive */
#define HASHFUNCTION(key)	((HOST_WIDE_INT) key & 0x7fffffff)

static void
hash_init ()
{
  nst_method_hash_list = (hash *)xmalloc (SIZEHASHTABLE * sizeof (hash));
  cls_method_hash_list = (hash *)xmalloc (SIZEHASHTABLE * sizeof (hash));

  if (!nst_method_hash_list || !cls_method_hash_list)
    perror ("unable to allocate space in objc-tree.c");
  else
    {
      int i;

      for (i = 0; i < SIZEHASHTABLE; i++)
	{
	  nst_method_hash_list[i] = 0;
	  cls_method_hash_list[i] = 0;
	}
    }
}

static void
hash_enter (hashlist, method)
     hash *hashlist;
     tree method;
{
  static hash 	hash_alloc_list = 0;
  static int	hash_alloc_index = 0;
  hash obj;
  int slot = HASHFUNCTION (METHOD_SEL_NAME (method)) % SIZEHASHTABLE;

  if (! hash_alloc_list || hash_alloc_index >= HASH_ALLOC_LIST_SIZE)
    {
      hash_alloc_index = 0;
      hash_alloc_list = (hash) xmalloc (sizeof (struct hashed_entry)
					* HASH_ALLOC_LIST_SIZE);
      if (! hash_alloc_list)
	perror ("unable to allocate in objc-tree.c");
    }
  obj = &hash_alloc_list[hash_alloc_index++];
  obj->list = 0;
  obj->next = hashlist[slot];
  obj->key = method;

  hashlist[slot] = obj;		/* append to front */
}

static hash
hash_lookup (hashlist, sel_name)
     hash *hashlist;
     tree sel_name;
{
  hash target;

  target = hashlist[HASHFUNCTION (sel_name) % SIZEHASHTABLE];

  while (target)
    {
      if (sel_name == METHOD_SEL_NAME (target->key))
	return target;

      target = target->next;
    }
  return 0;
}

static void
hash_add_attr (entry, value)
     hash entry;
     tree value;
{
  static attr 	attr_alloc_list = 0;
  static int	attr_alloc_index = 0;
  attr obj;

  if (! attr_alloc_list || attr_alloc_index >= ATTR_ALLOC_LIST_SIZE)
    {
      attr_alloc_index = 0;
      attr_alloc_list = (attr) xmalloc (sizeof (struct hashed_attribute)
					* ATTR_ALLOC_LIST_SIZE);
      if (! attr_alloc_list)
	perror ("unable to allocate in objc-tree.c");
    }
  obj = &attr_alloc_list[attr_alloc_index++];
  obj->next = entry->list;
  obj->value = value;

  entry->list = obj;		/* append to front */
}

static tree
lookup_method (mchain, method)
     tree mchain;
     tree method;
{
  tree key;

  if (TREE_CODE (method) == IDENTIFIER_NODE)
    key = method;
  else
    key = METHOD_SEL_NAME (method);

  while (mchain)
    {
      if (METHOD_SEL_NAME (mchain) == key)
	return mchain;
      mchain = TREE_CHAIN (mchain);
    }
  return NULL_TREE;
}

static tree
lookup_instance_method_static (interface, ident)
     tree interface;
     tree ident;
{
  tree inter = interface;
  tree chain = CLASS_NST_METHODS (inter);
  tree meth = NULL_TREE;

  do
    {
      if ((meth = lookup_method (chain, ident)))
	return meth;

      if (CLASS_CATEGORY_LIST (inter))
	{
	  tree category = CLASS_CATEGORY_LIST (inter);
	  chain = CLASS_NST_METHODS (category);

	  do
	    {
	      if ((meth = lookup_method (chain, ident)))
		return meth;

	      /* Check for instance methods in protocols in categories.  */
	      if (CLASS_PROTOCOL_LIST (category))
		{
		  if ((meth = (lookup_method_in_protocol_list
			       (CLASS_PROTOCOL_LIST (category), ident, 0))))
		    return meth;
		}

	      if ((category = CLASS_CATEGORY_LIST (category)))
		chain = CLASS_NST_METHODS (category);
	    }
	  while (category);
	}

      if (CLASS_PROTOCOL_LIST (inter))
	{
	  if ((meth = (lookup_method_in_protocol_list
		       (CLASS_PROTOCOL_LIST (inter), ident, 0))))
	    return meth;
	}

      if ((inter = lookup_interface (CLASS_SUPER_NAME (inter))))
	chain = CLASS_NST_METHODS (inter);
    }
  while (inter);

  return meth;
}

static tree
lookup_class_method_static (interface, ident)
     tree interface;
     tree ident;
{
  tree inter = interface;
  tree chain = CLASS_CLS_METHODS (inter);
  tree meth = NULL_TREE;
  tree root_inter = NULL_TREE;

  do
    {
      if ((meth = lookup_method (chain, ident)))
	return meth;

      if (CLASS_CATEGORY_LIST (inter))
	{
	  tree category = CLASS_CATEGORY_LIST (inter);
	  chain = CLASS_CLS_METHODS (category);

	  do
	    {
	      if ((meth = lookup_method (chain, ident)))
		return meth;

	      /* Check for class methods in protocols in categories.  */
	      if (CLASS_PROTOCOL_LIST (category))
		{
		  if ((meth = (lookup_method_in_protocol_list
			       (CLASS_PROTOCOL_LIST (category), ident, 1))))
		    return meth;
		}

	      if ((category = CLASS_CATEGORY_LIST (category)))
		chain = CLASS_CLS_METHODS (category);
	    }
	  while (category);
	}

      /* Check for class methods in protocols.  */
      if (CLASS_PROTOCOL_LIST (inter))
	{
	  if ((meth = (lookup_method_in_protocol_list
		       (CLASS_PROTOCOL_LIST (inter), ident, 1))))
	    return meth;
	}

      root_inter = inter;
      if ((inter = lookup_interface (CLASS_SUPER_NAME (inter))))
	chain = CLASS_CLS_METHODS (inter);
    }
  while (inter);

  /* Simulate wrap around.  */
  return lookup_instance_method_static (root_inter, ident);
}

tree
add_class_method (class, method)
     tree class;
     tree method;
{
  tree mth;
  hash hsh;

  /* We will have allocated the method parameter declarations on the
     maybepermanent_obstack.  Need to make sure they stick around!  */
  preserve_data ();

  if (!(mth = lookup_method (CLASS_CLS_METHODS (class), method)))
    {
      /* put method on list in reverse order */
      TREE_CHAIN (method) = CLASS_CLS_METHODS (class);
      CLASS_CLS_METHODS (class) = method;
    }
  else
    {
      if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE)
	error ("duplicate definition of class method `%s'.",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
      else
        {
	  /* Check types; if different, complain.  */
	  if (!comp_proto_with_proto (method, mth))
	    error ("duplicate declaration of class method `%s'.",
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
        }
    }

  if (!(hsh = hash_lookup (cls_method_hash_list, METHOD_SEL_NAME (method))))
    {
      /* Install on a global chain.  */
      hash_enter (cls_method_hash_list, method);
    }
  else
    {
      /* Check types; if different, add to a list.  */
      if (!comp_proto_with_proto (method, hsh->key))
        hash_add_attr (hsh, method);
    }
  return method;
}

tree
add_instance_method (class, method)
     tree class;
     tree method;
{
  tree mth;
  hash hsh;

  /* We will have allocated the method parameter declarations on the
     maybepermanent_obstack.  Need to make sure they stick around!  */
  preserve_data ();

  if (!(mth = lookup_method (CLASS_NST_METHODS (class), method)))
    {
      /* Put method on list in reverse order.  */
      TREE_CHAIN (method) = CLASS_NST_METHODS (class);
      CLASS_NST_METHODS (class) = method;
    }
  else
    {
      if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE)
	error ("duplicate definition of instance method `%s'.",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
      else
        {
	  /* Check types; if different, complain.  */
	  if (!comp_proto_with_proto (method, mth))
	    error ("duplicate declaration of instance method `%s'.",
		   IDENTIFIER_POINTER (METHOD_SEL_NAME (mth)));
        }
    }

  if (!(hsh = hash_lookup (nst_method_hash_list, METHOD_SEL_NAME (method))))
    {
      /* Install on a global chain.  */
      hash_enter (nst_method_hash_list, method);
    }
  else
    {
      /* Check types; if different, add to a list.  */
      if (!comp_proto_with_proto (method, hsh->key))
        hash_add_attr (hsh, method);
    }
  return method;
}

static tree
add_class (class)
     tree class;
{
  /* Put interfaces on list in reverse order.  */
  TREE_CHAIN (class) = interface_chain;
  interface_chain = class;
  return interface_chain;
}

static void
add_category (class, category)
      tree class;
      tree category;
{
  /* Put categories on list in reverse order.  */
  tree cat = CLASS_CATEGORY_LIST (class);

  while (cat)
    {
      if (CLASS_SUPER_NAME (cat) == CLASS_SUPER_NAME (category))
	warning ("duplicate interface declaration for category `%s(%s)'",
		 IDENTIFIER_POINTER (CLASS_NAME (class)),
		 IDENTIFIER_POINTER (CLASS_SUPER_NAME (category)));
      cat = CLASS_CATEGORY_LIST (cat);
    }

  CLASS_CATEGORY_LIST (category) = CLASS_CATEGORY_LIST (class);
  CLASS_CATEGORY_LIST (class) = category;
}

/* Called after parsing each instance variable declaration. Necessary to
   preserve typedefs and implement public/private...

   PUBLIC is 1 for public, 0 for protected, and 2 for private.  */

tree
add_instance_variable (class, public, declarator, declspecs, width)
     tree class;
     int public;
     tree declarator;
     tree declspecs;
     tree width;
{
  tree field_decl, raw_decl;

  raw_decl = build_tree_list (declspecs, declarator);

  if (CLASS_RAW_IVARS (class))
    chainon (CLASS_RAW_IVARS (class), raw_decl);
  else
    CLASS_RAW_IVARS (class) = raw_decl;

  field_decl = grokfield (input_filename, lineno,
			  declarator, declspecs, width);

  /* Overload the public attribute, it is not used for FIELD_DECLs.  */
  switch (public)
    {
    case 0:
      TREE_PUBLIC (field_decl) = 0;
      TREE_PRIVATE (field_decl) = 0;
      TREE_PROTECTED (field_decl) = 1;
      break;

    case 1:
      TREE_PUBLIC (field_decl) = 1;
      TREE_PRIVATE (field_decl) = 0;
      TREE_PROTECTED (field_decl) = 0;
      break;

    case 2:
      TREE_PUBLIC (field_decl) = 0;
      TREE_PRIVATE (field_decl) = 1;
      TREE_PROTECTED (field_decl) = 0;
      break;

    }

  if (CLASS_IVARS (class))
    chainon (CLASS_IVARS (class), field_decl);
  else
    CLASS_IVARS (class) = field_decl;

  return class;
}

tree
is_ivar (decl_chain, ident)
     tree decl_chain;
     tree ident;
{
  for ( ; decl_chain; decl_chain = TREE_CHAIN (decl_chain))
    if (DECL_NAME (decl_chain) == ident)
      return decl_chain;
  return NULL_TREE;
}

/* True if the ivar is private and we are not in its implementation.  */

int
is_private (decl)
     tree decl;
{
  if (TREE_PRIVATE (decl)
      && ! is_ivar (CLASS_IVARS (implementation_template), DECL_NAME (decl)))
    {
      error ("instance variable `%s' is declared private",
	     IDENTIFIER_POINTER (DECL_NAME (decl)));
      return 1;
    }
  else
    return 0;
}

/* We have an instance variable reference;, check to see if it is public.  */

int
is_public (expr, identifier)
     tree expr;
     tree identifier;
{
  tree basetype = TREE_TYPE (expr);
  enum tree_code code = TREE_CODE (basetype);
  tree decl;

  if (code == RECORD_TYPE)
    {
      if (TREE_STATIC_TEMPLATE (basetype))
	{
	  if (!lookup_interface (TYPE_NAME (basetype)))
	    {
	      error ("Cannot find interface declaration for `%s'",
		     IDENTIFIER_POINTER (TYPE_NAME (basetype)));
	      return 0;
	    }

	  if ((decl = is_ivar (TYPE_FIELDS (basetype), identifier)))
	    {
	      if (TREE_PUBLIC (decl))
		return 1;

	      /* Important difference between the Stepstone translator:
		 all instance variables should be public within the context
		 of the implementation.  */
	      if (implementation_context
		  && (((TREE_CODE (implementation_context)
			== CLASS_IMPLEMENTATION_TYPE)
		       || (TREE_CODE (implementation_context)
			   == CATEGORY_IMPLEMENTATION_TYPE))
		      && (CLASS_NAME (implementation_context)
			  == TYPE_NAME (basetype))))
		return ! is_private (decl);

	      error ("instance variable `%s' is declared %s",
		     IDENTIFIER_POINTER (identifier),
		     TREE_PRIVATE (decl) ? "private" : "protected");
	      return 0;
	    }
	}

      else if (implementation_context && (basetype == objc_object_reference))
	{
	  TREE_TYPE (expr) = uprivate_record;
	  warning ("static access to object of type `id'");
	}
    }

  return 1;
}

/* Implement @defs (<classname>) within struct bodies.  */

tree
get_class_ivars (interface)
     tree interface;
{
  if (!doing_objc_thang)
    objc_fatal ();

  return build_ivar_chain (interface, 1);
}

/* Make sure all entries in CHAIN are also in LIST.  */

static int
check_methods (chain, list, mtype)
     tree chain;
     tree list;
     int mtype;
{
  int first = 1;

  while (chain)
    {
      if (!lookup_method (list, chain))
	{
	  if (first)
	    {
	      if (TREE_CODE (implementation_context)
		  == CLASS_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of class `%s'",
			 IDENTIFIER_POINTER (CLASS_NAME (implementation_context)));
	      else if (TREE_CODE (implementation_context)
		       == CATEGORY_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of category `%s'",
			 IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)));
	      first = 0;
	    }

	  warning ("method definition for `%c%s' not found",
		   mtype, IDENTIFIER_POINTER (METHOD_SEL_NAME (chain)));
	}

      chain = TREE_CHAIN (chain);
    }

    return first;
}

static int
conforms_to_protocol (class, protocol)
     tree class;
     tree protocol;
{
   while (protocol)
     {
       tree p = CLASS_PROTOCOL_LIST (class);

       while (p && TREE_VALUE (p) != TREE_VALUE (protocol))
	 p = TREE_CHAIN (p);

       if (!p)
	 {
	   tree super = (CLASS_SUPER_NAME (class)
			 ? lookup_interface (CLASS_SUPER_NAME (class))
			 : NULL_TREE);
	   int tmp = super ? conforms_to_protocol (super, protocol) : 0;
	   if (!tmp)
	     return 0;
	 }

       protocol = TREE_CHAIN (protocol);
     }

   return 1;
}

/* Make sure all methods in CHAIN are accessible as MTYPE methods in 
   CONTEXT.  This is one of two mechanisms to check protocol integrity.  */

static int
check_methods_accessible (chain, context, mtype)
     tree chain;
     tree context;
     int mtype;
{
  int first = 1;
  tree list;
  tree base_context = context;

  while (chain)
    {
      context = base_context;
      while (context)
	{
	  if (mtype == '+')
	    list = CLASS_CLS_METHODS (context);
	  else
	    list = CLASS_NST_METHODS (context);

	  if (lookup_method (list, chain))
	      break; 

	  else if (TREE_CODE (context) == CLASS_IMPLEMENTATION_TYPE
		   || TREE_CODE (context) == CLASS_INTERFACE_TYPE)
	    context = (CLASS_SUPER_NAME (context) 
		       ? lookup_interface (CLASS_SUPER_NAME (context))
		       : NULL_TREE);

	  else if (TREE_CODE (context) == CATEGORY_IMPLEMENTATION_TYPE
		   || TREE_CODE (context) == CATEGORY_INTERFACE_TYPE)
	    context = (CLASS_NAME (context) 
		       ? lookup_interface (CLASS_NAME (context))
		       : NULL_TREE);
	  else
	    abort ();
	}

      if (context == NULL_TREE)
	{
	  if (first)
	    {
	      if (TREE_CODE (implementation_context)
		  == CLASS_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of class `%s'",
			 IDENTIFIER_POINTER
			   (CLASS_NAME (implementation_context)));
	      else if (TREE_CODE (implementation_context)
		       == CATEGORY_IMPLEMENTATION_TYPE)
		warning ("incomplete implementation of category `%s'",
			 IDENTIFIER_POINTER
			   (CLASS_SUPER_NAME (implementation_context)));
	      first = 0;
	    }
	  warning ("method definition for `%c%s' not found",
		   mtype, IDENTIFIER_POINTER (METHOD_SEL_NAME (chain)));
	}

      chain = TREE_CHAIN (chain); /* next method...  */
    }
    return first;
}

static void
check_protocols (proto_list, type, name)
     tree proto_list;
     char *type;
     char *name;
{
  for ( ; proto_list; proto_list = TREE_CHAIN (proto_list))
    {
      tree p = TREE_VALUE (proto_list);

      if (TREE_CODE (p) == PROTOCOL_INTERFACE_TYPE)
	{
	  int f1, f2;
	  
	  /* Ensure that all protocols have bodies.  */
	  if (flag_warn_protocol) {
	    f1 = check_methods (PROTOCOL_CLS_METHODS (p),
				CLASS_CLS_METHODS (implementation_context),
				'+');
	    f2 = check_methods (PROTOCOL_NST_METHODS (p),
				CLASS_NST_METHODS (implementation_context),
				'-');
	  } else {
	    f1 = check_methods_accessible (PROTOCOL_CLS_METHODS (p),
					   implementation_context,
					   '+');
	    f2 = check_methods_accessible (PROTOCOL_NST_METHODS (p),
					   implementation_context,
					   '-');
	  }

	  if (!f1 || !f2)
	    warning ("%s `%s' does not fully implement the `%s' protocol",
		     type, name, IDENTIFIER_POINTER (PROTOCOL_NAME (p)));

	}
      else
	; /* An identifier if we could not find a protocol.  */

      /* Check protocols recursively.  */
      if (PROTOCOL_LIST (p))
	{
	  tree super_class
	    = lookup_interface (CLASS_SUPER_NAME (implementation_template));
	  if (! conforms_to_protocol (super_class, PROTOCOL_LIST (p)))
	    check_protocols (PROTOCOL_LIST (p), type, name);
	}
    }
}

/* Make sure that the class CLASS_NAME is defined
   CODE says which kind of thing CLASS_NAME ought to be.
   It can be CLASS_INTERFACE_TYPE, CLASS_IMPLEMENTATION_TYPE,
   CATEGORY_INTERFACE_TYPE, or CATEGORY_IMPLEMENTATION_TYPE.

   If CODE is CLASS_INTERFACE_TYPE, we also do a push_obstacks_nochange
   whose matching pop is in continue_class.  */

tree
start_class (code, class_name, super_name, protocol_list)
     enum tree_code code;
     tree class_name;
     tree super_name;
     tree protocol_list;
{
  tree class, decl;

  if (code == CLASS_INTERFACE_TYPE)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
    }

  if (!doing_objc_thang)
    objc_fatal ();

  class = make_node (code);
  TYPE_BINFO (class) = make_tree_vec (5);

  CLASS_NAME (class) = class_name;
  CLASS_SUPER_NAME (class) = super_name;
  CLASS_CLS_METHODS (class) = NULL_TREE;

  if (! is_class_name (class_name) && (decl = lookup_name (class_name)))
    {
      error ("`%s' redeclared as different kind of symbol",
	     IDENTIFIER_POINTER (class_name));
      error_with_decl (decl, "previous declaration of `%s'");
    }

  if (code == CLASS_IMPLEMENTATION_TYPE)
    {
      {
        static tree implemented_classes = 0;
        tree chain = implemented_classes;
        for (chain = implemented_classes; chain; chain = TREE_CHAIN (chain))
           if (TREE_VALUE (chain) == class_name)
	     {
	       error ("reimplementation of class `%s'",
		      IDENTIFIER_POINTER (class_name));
	       return error_mark_node;
	     }
        implemented_classes = perm_tree_cons (NULL_TREE, class_name,
					      implemented_classes);
      }

      /* Pre-build the following entities - for speed/convenience.  */
      if (!self_id)
	  self_id = get_identifier ("self");
      if (!ucmd_id)
        ucmd_id = get_identifier ("_cmd");
      if (!unused_list)
	unused_list
	  = build_tree_list (get_identifier ("__unused__"), NULL_TREE);
      if (!objc_super_template)
	objc_super_template = build_super_template ();

      /* Reset for multiple classes per file.  */
      method_slot = 0;

      implementation_context = class;

      /* Lookup the interface for this implementation.  */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  warning ("Cannot find interface declaration for `%s'",
		   IDENTIFIER_POINTER (class_name));
	  add_class (implementation_template = implementation_context);
        }

      /* If a super class has been specified in the implementation,
	 insure it conforms to the one specified in the interface.  */

      if (super_name
	  && (super_name != CLASS_SUPER_NAME (implementation_template)))
        {
	  tree previous_name = CLASS_SUPER_NAME (implementation_template);
          char *name = previous_name ? IDENTIFIER_POINTER (previous_name) : "";
	  error ("conflicting super class name `%s'",
		 IDENTIFIER_POINTER (super_name));
	  error ("previous declaration of `%s'", name);
        }

      else if (! super_name)
	{
	  CLASS_SUPER_NAME (implementation_context) 
	    = CLASS_SUPER_NAME (implementation_template);
	}
    }

  else if (code == CLASS_INTERFACE_TYPE)
    {
      if (lookup_interface (class_name))
        warning ("duplicate interface declaration for class `%s'",
                 IDENTIFIER_POINTER (class_name));
      else
        add_class (class);

      if (protocol_list)
	CLASS_PROTOCOL_LIST (class)
	  = lookup_and_install_protocols (protocol_list);
    }

  else if (code == CATEGORY_INTERFACE_TYPE)
    {
      tree class_category_is_assoc_with;

      /* For a category, class_name is really the name of the class that
	 the following set of methods will be associated with. We must
	 find the interface so that can derive the objects template.  */

      if (!(class_category_is_assoc_with = lookup_interface (class_name)))
	{
	  error ("Cannot find interface declaration for `%s'",
		 IDENTIFIER_POINTER (class_name));
	  exit (FATAL_EXIT_CODE);
	}
      else
        add_category (class_category_is_assoc_with, class);

      if (protocol_list)
	CLASS_PROTOCOL_LIST (class)
	  = lookup_and_install_protocols (protocol_list);
    }

  else if (code == CATEGORY_IMPLEMENTATION_TYPE)
    {
      /* Pre-build the following entities for speed/convenience.  */
      if (!self_id)
        self_id = get_identifier ("self");
      if (!ucmd_id)
        ucmd_id = get_identifier ("_cmd");
      if (!unused_list)
	unused_list
	  = build_tree_list (get_identifier ("__unused__"), NULL_TREE);
      if (!objc_super_template)
	objc_super_template = build_super_template ();

      /* Reset for multiple classes per file.  */
      method_slot = 0;

      implementation_context = class;

      /* For a category, class_name is really the name of the class that
	 the following set of methods will be associated with.  We must
	 find the interface so that can derive the objects template.  */

      if (!(implementation_template = lookup_interface (class_name)))
        {
	  error ("Cannot find interface declaration for `%s'",
		 IDENTIFIER_POINTER (class_name));
	  exit (FATAL_EXIT_CODE);
        }
    }
  return class;
}

tree
continue_class (class)
     tree class;
{
  if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE
      || TREE_CODE (class) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      struct imp_entry *imp_entry;
      tree ivar_context;

      /* Check consistency of the instance variables.  */

      if (CLASS_IVARS (class))
	check_ivars (implementation_template, class);

      /* code generation */

      ivar_context = build_private_template (implementation_template);

      if (!objc_class_template)
	build_class_template ();

      if (!(imp_entry
	    = (struct imp_entry *) xmalloc (sizeof (struct imp_entry))))
	perror ("unable to allocate in objc-tree.c");

      imp_entry->next = imp_list;
      imp_entry->imp_context = class;
      imp_entry->imp_template = implementation_template;

      synth_forward_declarations ();
      imp_entry->class_decl = UOBJC_CLASS_decl;
      imp_entry->meta_decl = UOBJC_METACLASS_decl;

      /* Append to front and increment count.  */
      imp_list = imp_entry;
      if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE)
	imp_count++;
      else
	cat_count++;

      return ivar_context;
    }

  else if (TREE_CODE (class) == CLASS_INTERFACE_TYPE)
    {
      tree record = xref_tag (RECORD_TYPE, CLASS_NAME (class));

      if (!TYPE_FIELDS (record))
	{
	  finish_struct (record, build_ivar_chain (class, 0), NULL_TREE);
	  CLASS_STATIC_TEMPLATE (class) = record;

	  /* Mark this record as a class template for static typing.  */
	  TREE_STATIC_TEMPLATE (record) = 1;
	}

      return NULL_TREE;
    }

  else
    return error_mark_node;
}

/* This is called once we see the "@end" in an interface/implementation.  */

void
finish_class (class)
     tree class;
{
  if (TREE_CODE (class) == CLASS_IMPLEMENTATION_TYPE)
    {
      /* All code generation is done in finish_objc.  */

      if (implementation_template != implementation_context)
	{
	  /* Ensure that all method listed in the interface contain bodies.  */
	  check_methods (CLASS_CLS_METHODS (implementation_template),
			 CLASS_CLS_METHODS (implementation_context), '+');
	  check_methods (CLASS_NST_METHODS (implementation_template),
			 CLASS_NST_METHODS (implementation_context), '-');

	  if (CLASS_PROTOCOL_LIST (implementation_template))
	    check_protocols (CLASS_PROTOCOL_LIST (implementation_template),
			     "class",
			     IDENTIFIER_POINTER (CLASS_NAME (implementation_context)));
	}
    }

  else if (TREE_CODE (class) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      tree category = CLASS_CATEGORY_LIST (implementation_template);

      /* Find the category interface from the class it is associated with.  */
      while (category)
	{
	  if (CLASS_SUPER_NAME (class) == CLASS_SUPER_NAME (category))
	    break;
	  category = CLASS_CATEGORY_LIST (category);
	}

      if (category)
	{
	  /* Ensure all method listed in the interface contain bodies.  */
	  check_methods (CLASS_CLS_METHODS (category),
			 CLASS_CLS_METHODS (implementation_context), '+');
	  check_methods (CLASS_NST_METHODS (category),
			 CLASS_NST_METHODS (implementation_context), '-');

	  if (CLASS_PROTOCOL_LIST (category))
	    check_protocols (CLASS_PROTOCOL_LIST (category),
			     "category",
			     IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)));
	}
    }

  else if (TREE_CODE (class) == CLASS_INTERFACE_TYPE)
    {
      tree decl_specs;
      char *class_name = IDENTIFIER_POINTER (CLASS_NAME (class));
      char *string = (char *) alloca (strlen (class_name) + 3);

      /* extern struct objc_object *_<my_name>; */

      sprintf (string, "_%s", class_name);

      decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_EXTERN]);
      decl_specs = tree_cons (NULL_TREE, objc_object_reference, decl_specs);
      define_decl (build1 (INDIRECT_REF, NULL_TREE, get_identifier (string)),
		   decl_specs);
    }
}

static tree
add_protocol (protocol)
     tree protocol;
{
  /* Put protocol on list in reverse order.  */
  TREE_CHAIN (protocol) = protocol_chain;
  protocol_chain = protocol;
  return protocol_chain;
}

static tree
lookup_protocol (ident)
     tree ident;
{
  tree chain;

  for (chain = protocol_chain; chain; chain = TREE_CHAIN (chain))
    {
      if (ident == PROTOCOL_NAME (chain))
	return chain;
    }

  return NULL_TREE;
}

tree
start_protocol (code, name, list)
     enum tree_code code;
     tree name;
     tree list;
{
  tree protocol;

  if (!doing_objc_thang)
    objc_fatal ();

  /* This is as good a place as any.  Need to invoke push_tag_toplevel.  */
  if (!objc_protocol_template)
    objc_protocol_template = build_protocol_template ();

  protocol = make_node (code);
  TYPE_BINFO (protocol) = make_tree_vec (2);

  PROTOCOL_NAME (protocol) = name;
  PROTOCOL_LIST (protocol) = list;

  lookup_and_install_protocols (list);

  if (lookup_protocol (name))
    warning ("duplicate declaration for protocol `%s'",
	   IDENTIFIER_POINTER (name));
  else
    add_protocol (protocol);

  PROTOCOL_FORWARD_DECL (protocol) = NULL_TREE;

  return protocol;
}

void
finish_protocol (protocol)
	tree protocol;
{
}


/* "Encode" a data type into a string, which grows in util_obstack.
   ??? What is the FORMAT?  Someone please document this!  */

static void
encode_type_qualifiers (declspecs)
     tree declspecs;
{
  tree spec;

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      if (ridpointers[(int) RID_CONST] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'r');
      else if (ridpointers[(int) RID_IN] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'n');
      else if (ridpointers[(int) RID_INOUT] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'N');
      else if (ridpointers[(int) RID_OUT] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'o');
      else if (ridpointers[(int) RID_BYCOPY] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'O');
      else if (ridpointers[(int) RID_ONEWAY] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'V');
    }
}

/* Encode a pointer type.  */

static void
encode_pointer (type, curtype, format)
     tree type;
     int curtype;
     int format;
{
  tree pointer_to = TREE_TYPE (type);

  if (TREE_CODE (pointer_to) == RECORD_TYPE)
    {
      if (TYPE_NAME (pointer_to)
	  && TREE_CODE (TYPE_NAME (pointer_to)) == IDENTIFIER_NODE)
	{
	  char *name = IDENTIFIER_POINTER (TYPE_NAME (pointer_to));

	  if (strcmp (name, TAG_OBJECT) == 0) /* '@' */
	    {
	      obstack_1grow (&util_obstack, '@');
	      return;
	    }
	  else if (TREE_STATIC_TEMPLATE (pointer_to))
	    {
              if (generating_instance_variables)
	        {
	          obstack_1grow (&util_obstack, '@');
	          obstack_1grow (&util_obstack, '"');
	          obstack_grow (&util_obstack, name, strlen (name));
	          obstack_1grow (&util_obstack, '"');
	          return;
		}
              else
	        {
	          obstack_1grow (&util_obstack, '@');
	          return;
		}
	    }
	  else if (strcmp (name, TAG_CLASS) == 0) /* '#' */
	    {
	      obstack_1grow (&util_obstack, '#');
	      return;
	    }
#ifndef OBJC_INT_SELECTORS
	  else if (strcmp (name, TAG_SELECTOR) == 0) /* ':' */
	    {
	      obstack_1grow (&util_obstack, ':');
	      return;
	    }
#endif /* OBJC_INT_SELECTORS */
	}
    }
  else if (TREE_CODE (pointer_to) == INTEGER_TYPE
	   && TYPE_MODE (pointer_to) == QImode)
    {
      obstack_1grow (&util_obstack, '*');
      return;
    }

  /* We have a type that does not get special treatment.  */

  /* NeXT extension */
  obstack_1grow (&util_obstack, '^');
  encode_type (pointer_to, curtype, format);
}

static void
encode_array (type, curtype, format)
     tree type;
     int curtype;
     int format;
{
  tree an_int_cst = TYPE_SIZE (type);
  tree array_of = TREE_TYPE (type);
  char buffer[40];

  /* An incomplete array is treated like a pointer.  */
  if (an_int_cst == NULL)
    {
      encode_pointer (type, curtype, format);
      return;
    }

  sprintf (buffer, "[%d",
	   (TREE_INT_CST_LOW (an_int_cst)
	    / TREE_INT_CST_LOW (TYPE_SIZE (array_of))));

  obstack_grow (&util_obstack, buffer, strlen (buffer));
  encode_type (array_of, curtype, format);
  obstack_1grow (&util_obstack, ']');
  return;
}

static void
encode_aggregate (type, curtype, format)
     tree type;
     int curtype;
     int format;
{
  enum tree_code code = TREE_CODE (type);

  switch (code)
    {
    case RECORD_TYPE:
      {
	if (obstack_object_size (&util_obstack) > 0
	    && *(obstack_next_free (&util_obstack) - 1) == '^')
	  {
	    tree name = TYPE_NAME (type);

	    /* We have a reference; this is a NeXT extension.  */

	    if (obstack_object_size (&util_obstack) - curtype == 1
		&& format == OBJC_ENCODE_INLINE_DEFS)
	      {
		/* Output format of struct for first level only.  */
		tree fields = TYPE_FIELDS (type);

		if (name && TREE_CODE (name) == IDENTIFIER_NODE)
		  {
		    obstack_1grow (&util_obstack, '{');
		    obstack_grow (&util_obstack,
				  IDENTIFIER_POINTER (name),
				  strlen (IDENTIFIER_POINTER (name)));
		    obstack_1grow (&util_obstack, '=');
		  }

		else
		  obstack_grow (&util_obstack, "{?=", 3);

		for ( ; fields; fields = TREE_CHAIN (fields))
		  encode_field_decl (fields, curtype, format);

		obstack_1grow (&util_obstack, '}');
	      }

            else if (name && TREE_CODE (name) == IDENTIFIER_NODE)
	      {
		obstack_1grow (&util_obstack, '{');
		obstack_grow (&util_obstack,
			      IDENTIFIER_POINTER (name),
			      strlen (IDENTIFIER_POINTER (name)));
		obstack_1grow (&util_obstack, '}');
	      }

	    else
	      /* We have an untagged structure or a typedef.  */
	      obstack_grow (&util_obstack, "{?}", 3);
	  }

	else
	  {
	    tree name = TYPE_NAME (type);
	    tree fields = TYPE_FIELDS (type);

	    if (format == OBJC_ENCODE_INLINE_DEFS
		|| generating_instance_variables)
	      {
		obstack_1grow (&util_obstack, '{');
		if (name && TREE_CODE (name) == IDENTIFIER_NODE)
		  obstack_grow (&util_obstack,
				IDENTIFIER_POINTER (name),
				strlen (IDENTIFIER_POINTER (name)));

		else
		  obstack_1grow (&util_obstack, '?');

		obstack_1grow (&util_obstack, '=');

		for (; fields; fields = TREE_CHAIN (fields))
		  {
                  if (generating_instance_variables)
                    {
                      tree fname = DECL_NAME (fields);

		      obstack_1grow (&util_obstack, '"');
		      if (fname && TREE_CODE (fname) == IDENTIFIER_NODE)
		        {
			  obstack_grow (&util_obstack,
					IDENTIFIER_POINTER (fname),
					strlen (IDENTIFIER_POINTER (fname)));
			}

		      obstack_1grow (&util_obstack, '"');
                    }

		  encode_field_decl (fields, curtype, format);
		  }

		obstack_1grow (&util_obstack, '}');
	      }

	    else
	      {
		obstack_1grow (&util_obstack, '{');
		if (name && TREE_CODE (name) == IDENTIFIER_NODE)
		  obstack_grow (&util_obstack,
				IDENTIFIER_POINTER (name),
				strlen (IDENTIFIER_POINTER (name)));
		else
		  /* We have an untagged structure or a typedef.  */
		  obstack_1grow (&util_obstack, '?');

		obstack_1grow (&util_obstack, '}');
	      }
	  }
	break;
      }

    case UNION_TYPE:
      {
	if (*obstack_next_free (&util_obstack) == '^'
	    || format != OBJC_ENCODE_INLINE_DEFS)
	  {
	    /* We have a reference (this is a NeXT extension)
	       or we don't want the details.  */
            if (TYPE_NAME (type)
		&& TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	      {
		obstack_1grow (&util_obstack, '(');
		obstack_grow (&util_obstack,
			      IDENTIFIER_POINTER (TYPE_NAME (type)),
			      strlen (IDENTIFIER_POINTER (TYPE_NAME (type))));
		obstack_1grow (&util_obstack, ')');
	      }

	    else
	      /* We have an untagged structure or a typedef.  */
	      obstack_grow (&util_obstack, "(?)", 3);
	  }
	else
	  {
	    tree fields = TYPE_FIELDS (type);
	    obstack_1grow (&util_obstack, '(');
	    for ( ; fields; fields = TREE_CHAIN (fields))
	      encode_field_decl (fields, curtype, format);

	    obstack_1grow (&util_obstack, ')');
	  }
	break;
      }

    case ENUMERAL_TYPE:
      obstack_1grow (&util_obstack, 'i');
      break;
    }
}

/* Support bitfields.  The current version of Objective-C does not support
   them.  The string will consist of one or more "b:n"'s where n is an
   integer describing the width of the bitfield. Currently, classes in
   the kit implement a method "-(char *)describeBitfieldStruct:" that
   simulates this. If they do not implement this method, the archiver
   assumes the bitfield is 16 bits wide (padded if necessary) and packed
   according to the GNU compiler. After looking at the "kit", it appears
   that all classes currently rely on this default behavior, rather than
   hand generating this string (which is tedious).  */

static void
encode_bitfield (width, format)
     int width;
     int format;
{
  char buffer[40];
  sprintf (buffer, "b%d", width);
  obstack_grow (&util_obstack, buffer, strlen (buffer));
}

/* FORMAT will be OBJC_ENCODE_INLINE_DEFS or OBJC_ENCODE_DONT_INLINE_DEFS.  */

static void
encode_type (type, curtype, format)
     tree type;
     int curtype;
     int format;
{
  enum tree_code code = TREE_CODE (type);

  if (code == INTEGER_TYPE)
    {
      if (TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) == 0
	  && TREE_INT_CST_HIGH (TYPE_MIN_VALUE (type)) == 0)
	{
	  /* Unsigned integer types.  */

	  if (TYPE_MODE (type) == QImode)
	    obstack_1grow (&util_obstack, 'C');
	  else if (TYPE_MODE (type) == HImode)
	    obstack_1grow (&util_obstack, 'S');
	  else if (TYPE_MODE (type) == SImode)
	    {
	      if (type == long_unsigned_type_node)
		obstack_1grow (&util_obstack, 'L');
	      else
		obstack_1grow (&util_obstack, 'I');
	    }
	  else if (TYPE_MODE (type) == DImode)
	    obstack_1grow (&util_obstack, 'Q');
	}

      else
	/* Signed integer types.  */
	{
	  if (TYPE_MODE (type) == QImode)
	    obstack_1grow (&util_obstack, 'c');
	  else if (TYPE_MODE (type) == HImode)
	    obstack_1grow (&util_obstack, 's');
	  else if (TYPE_MODE (type) == SImode)
	    {
	      if (type == long_integer_type_node)
		obstack_1grow (&util_obstack, 'l');
	      else
		obstack_1grow (&util_obstack, 'i');
	    }

	  else if (TYPE_MODE (type) == DImode)
	    obstack_1grow (&util_obstack, 'q');
	}
    }

  else if (code == REAL_TYPE)
    {
      /* Floating point types.  */

      if (TYPE_MODE (type) == SFmode)
	obstack_1grow (&util_obstack, 'f');
      else if (TYPE_MODE (type) == DFmode
	       || TYPE_MODE (type) == TFmode)
	obstack_1grow (&util_obstack, 'd');
    }

  else if (code == VOID_TYPE)
    obstack_1grow (&util_obstack, 'v');

  else if (code == ARRAY_TYPE)
    encode_array (type, curtype, format);

  else if (code == POINTER_TYPE)
    encode_pointer (type, curtype, format);

  else if (code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
    encode_aggregate (type, curtype, format);

  else if (code == FUNCTION_TYPE) /* '?' */
    obstack_1grow (&util_obstack, '?');
}

static void
encode_field_decl (field_decl, curtype, format)
     tree field_decl;
     int curtype;
     int format;
{
  tree type;

 /* If this field is obviously a bitfield, or is a bitfield that has been
     clobbered to look like a ordinary integer mode, go ahead and generate
     the bitfield typing information.  */
  type = TREE_TYPE (field_decl);
  if (DECL_BIT_FIELD (field_decl))
    encode_bitfield (DECL_FIELD_SIZE (field_decl), format);
  else if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	   && DECL_FIELD_SIZE (field_decl)
	   && TYPE_MODE (type) > DECL_MODE (field_decl))
    encode_bitfield (DECL_FIELD_SIZE (field_decl), format);
  else
    encode_type (TREE_TYPE (field_decl), curtype, format);
}

static tree
expr_last (complex_expr)
     tree complex_expr;
{
  tree next;

  if (complex_expr)
    while ((next = TREE_OPERAND (complex_expr, 0)))
      complex_expr = next;

  return complex_expr;
}

/* The selector of the current method,
   or NULL if we aren't compiling a method.  */

tree
maybe_objc_method_name (decl)
      tree decl;
{
  if (method_context)
    return METHOD_SEL_NAME (method_context);
  else
    return 0;
}

/* Transform a method definition into a function definition as follows:
   - synthesize the first two arguments, "self" and "_cmd".  */

void
start_method_def (method)
     tree method;
{
  tree decl_specs;

  /* Required to implement _msgSuper.  */
  method_context = method;
  UOBJC_SUPER_decl = NULL_TREE;

  /* Must be called BEFORE start_function.  */
  pushlevel (0);

  /* Generate prototype declarations for arguments..."new-style".  */

  if (TREE_CODE (method_context) == INSTANCE_METHOD_DECL)
    decl_specs = build_tree_list (NULL_TREE, uprivate_record);
  else
    /* Really a `struct objc_class *'. However, we allow people to
       assign to self, which changes its type midstream.  */
    decl_specs = build_tree_list (NULL_TREE, objc_object_reference);

  push_parm_decl (build_tree_list
		  (build_tree_list (decl_specs,
				    build1 (INDIRECT_REF, NULL_TREE, self_id)),
		   build_tree_list (unused_list, NULL_TREE)));

#ifdef OBJC_INT_SELECTORS
  decl_specs = build_tree_list (NULL_TREE, ridpointers[(int) RID_UNSIGNED]);
  decl_specs = tree_cons (NULL_TREE, ridpointers[(int) RID_INT], decl_specs);
  push_parm_decl (build_tree_list (build_tree_list (decl_specs, ucmd_id),
				   build_tree_list (unused_list, NULL_TREE)));
#else /* not OBJC_INT_SELECTORS */
  decl_specs = build_tree_list (NULL_TREE,
				xref_tag (RECORD_TYPE,
					  get_identifier (TAG_SELECTOR)));
  push_parm_decl (build_tree_list
		  (build_tree_list (decl_specs,
				    build1 (INDIRECT_REF, NULL_TREE, ucmd_id)),
		   build_tree_list (unused_list, NULL_TREE)));
#endif /* not OBJC_INT_SELECTORS */

  /* Generate argument declarations if a keyword_decl.  */
  if (METHOD_SEL_ARGS (method))
    {
      tree arglist = METHOD_SEL_ARGS (method);
      do
	{
	  tree arg_spec = TREE_PURPOSE (TREE_TYPE (arglist));
	  tree arg_decl = TREE_VALUE (TREE_TYPE (arglist));

	  if (arg_decl)
	    {
	      tree last_expr = expr_last (arg_decl);

	      /* Unite the abstract decl with its name.  */
	      TREE_OPERAND (last_expr, 0) = KEYWORD_ARG_NAME (arglist);
	      push_parm_decl (build_tree_list
			      (build_tree_list (arg_spec, arg_decl),
			       build_tree_list (NULL_TREE, NULL_TREE)));

	      /* Unhook: restore the abstract declarator.  */
	      TREE_OPERAND (last_expr, 0) = NULL_TREE;
	    }

	  else
	    push_parm_decl (build_tree_list
			    (build_tree_list (arg_spec,
					      KEYWORD_ARG_NAME (arglist)),
			     build_tree_list (NULL_TREE, NULL_TREE)));

	  arglist = TREE_CHAIN (arglist);
	}
      while (arglist);
    }

  if (METHOD_ADD_ARGS (method) > (tree)1)
    {
      /* We have a variable length selector - in "prototype" format.  */
      tree akey = TREE_PURPOSE (METHOD_ADD_ARGS (method));
      while (akey)
	{
	  /* This must be done prior to calling pushdecl.  pushdecl is
	     going to change our chain on us.  */
	  tree nextkey = TREE_CHAIN (akey);
	  pushdecl (akey);
	  akey = nextkey;
	}
    }
}

static void
warn_with_method (message, mtype, method)
     char *message;
     int mtype;
     tree method;
{
  if (count_error (1) == 0)
    return;

  report_error_function (DECL_SOURCE_FILE (method));

  fprintf (stderr, "%s:%d: warning: ",
	   DECL_SOURCE_FILE (method), DECL_SOURCE_LINE (method));
  bzero (errbuf, BUFSIZE);
  fprintf (stderr, "%s `%c%s'\n",
	   message, mtype, gen_method_decl (method, errbuf));
}

/* Return 1 if METHOD is consistent with PROTO.  */

static int
comp_method_with_proto (method, proto)
     tree method, proto;
{
  static tree function_type = 0;

  /* Create a function_type node once.  */
  if (!function_type)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      function_type = make_node (FUNCTION_TYPE);
      pop_obstacks ();
    }

  /* Install argument types - normally set by build_function_type.  */
  TYPE_ARG_TYPES (function_type) = get_arg_type_list (proto, METHOD_DEF, 0);

  /* install return type */
  TREE_TYPE (function_type) = groktypename (TREE_TYPE (proto));

  return comptypes (TREE_TYPE (METHOD_DEFINITION (method)), function_type);
}

/* Return 1 if PROTO1 is consistent with PROTO2.  */

static int
comp_proto_with_proto (proto1, proto2)
     tree proto1, proto2;
{
  static tree function_type1 = 0, function_type2 = 0;

  /* Create a couple function_type node's once.  */
  if (!function_type1)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      function_type1 = make_node (FUNCTION_TYPE);
      function_type2 = make_node (FUNCTION_TYPE);
      pop_obstacks ();
    }

  /* Install argument types; normally set by build_function_type.  */
  TYPE_ARG_TYPES (function_type1) = get_arg_type_list (proto1, METHOD_REF, 0);
  TYPE_ARG_TYPES (function_type2) = get_arg_type_list (proto2, METHOD_REF, 0);

  /* Install return type.  */
  TREE_TYPE (function_type1) = groktypename (TREE_TYPE (proto1));
  TREE_TYPE (function_type2) = groktypename (TREE_TYPE (proto2));

  return comptypes (function_type1, function_type2);
}

/* - Generate an identifier for the function. the format is "_n_cls",
     where 1 <= n <= nMethods, and cls is the name the implementation we
     are processing.
   - Install the return type from the method declaration.
   - If we have a prototype, check for type consistency.  */

static void
really_start_method (method, parmlist)
     tree method, parmlist;
{
  tree sc_spec, ret_spec, ret_decl, decl_specs;
  tree method_decl, method_id;
  char *buf, *sel_name, *class_name, *cat_name;

  /* Synth the storage class & assemble the return type.  */
  sc_spec = tree_cons (NULL_TREE, ridpointers[(int) RID_STATIC], NULL_TREE);
  ret_spec = TREE_PURPOSE (TREE_TYPE (method));
  decl_specs = chainon (sc_spec, ret_spec);

  sel_name = IDENTIFIER_POINTER (METHOD_SEL_NAME (method));
  class_name = IDENTIFIER_POINTER (CLASS_NAME (implementation_context));
  cat_name = ((TREE_CODE (implementation_context)
	       == CLASS_IMPLEMENTATION_TYPE)
	      ? NULL
	      : IDENTIFIER_POINTER (CLASS_SUPER_NAME (implementation_context)));
  method_slot++;

  /* Make sure this is big enough for any plausible method label.  */
  buf = (char *) alloca (50 + strlen (sel_name) + strlen (class_name)
			 + (cat_name ? strlen (cat_name) : 0));

  OBJC_GEN_METHOD_LABEL (buf, TREE_CODE (method) == INSTANCE_METHOD_DECL,
			 class_name, cat_name, sel_name, method_slot);

  method_id = get_identifier (buf);

  method_decl = build_nt (CALL_EXPR, method_id, parmlist, NULL_TREE);

  /* Check the declarator portion of the return type for the method.  */
  if ((ret_decl = TREE_VALUE (TREE_TYPE (method))))
    {
      /* Unite the complex decl (specified in the abstract decl) with the
	 function decl just synthesized..(int *), (int (*)()), (int (*)[]).  */
      tree save_expr = expr_last (ret_decl);

      TREE_OPERAND (save_expr, 0) = method_decl;
      method_decl = ret_decl;

      /* Fool the parser into thinking it is starting a function.  */
      start_function (decl_specs, method_decl, NULL_TREE, NULL_TREE, 0);

      /* Unhook: this has the effect of restoring the abstract declarator.  */
      TREE_OPERAND (save_expr, 0) = NULL_TREE;
    }

  else
    {
      TREE_VALUE (TREE_TYPE (method)) = method_decl;

      /* Fool the parser into thinking it is starting a function.  */
      start_function (decl_specs, method_decl, NULL_TREE, NULL_TREE, 0);

      /* Unhook: this has the effect of restoring the abstract declarator.  */
      TREE_VALUE (TREE_TYPE (method)) = NULL_TREE;
    }

  METHOD_DEFINITION (method) = current_function_decl;

  if (implementation_template != implementation_context)
    {
      tree proto;

      if (TREE_CODE (method) == INSTANCE_METHOD_DECL)
	proto = lookup_instance_method_static (implementation_template,
					       METHOD_SEL_NAME (method));
      else
	proto = lookup_class_method_static (implementation_template,
					    METHOD_SEL_NAME (method));

      if (proto && ! comp_method_with_proto (method, proto))
	{
	  char type = (TREE_CODE (method) == INSTANCE_METHOD_DECL ? '-' : '+');

	  warn_with_method ("conflicting types for", type, method);
	  warn_with_method ("previous declaration of", type, proto);
	}
    }
}

/* The following routine is always called...this "architecture" is to
   accommodate "old-style" variable length selectors.
 
   - a:a b:b // prototype  ; id c; id d; // old-style.  */

void
continue_method_def ()
{
  tree parmlist;

  if (METHOD_ADD_ARGS (method_context) == (tree)1)
    /* We have a `, ...' immediately following the selector.  */
    parmlist = get_parm_info (0);
  else
    parmlist = get_parm_info (1); /* place a `void_at_end' */

  /* Set self_decl from the first argument...this global is used by
     build_ivar_reference calling build_indirect_ref.  */
  self_decl = TREE_PURPOSE (parmlist);

  poplevel (0, 0, 0);
  really_start_method (method_context, parmlist);
  store_parm_decls ();
}

/* Called by the parser, from the `pushlevel' production.  */

void
add_objc_decls ()
{
  if (!UOBJC_SUPER_decl)
    {
      UOBJC_SUPER_decl = start_decl (get_identifier (UTAG_SUPER),
				     build_tree_list (NULL_TREE,
						      objc_super_template),
				     0, NULL_TREE, NULL_TREE);

      finish_decl (UOBJC_SUPER_decl, NULL_TREE, NULL_TREE);

      /* This prevents `unused variable' warnings when compiling with -Wall.  */
      TREE_USED (UOBJC_SUPER_decl) = 1;
      DECL_ARTIFICIAL (UOBJC_SUPER_decl) = 1;
    }
}

/* _n_Method (id self, SEL sel, ...)
     {
       struct objc_super _S;
       _msgSuper ((_S.self = self, _S.class = _cls, &_S), ...);
     }  */

tree
get_super_receiver ()
{
  if (method_context)
    {
      tree super_expr, super_expr_list;

      /* Set receiver to self.  */
      super_expr = build_component_ref (UOBJC_SUPER_decl, self_id);
      super_expr = build_modify_expr (super_expr, NOP_EXPR, self_decl);
      super_expr_list = build_tree_list (NULL_TREE, super_expr);

      /* Set class to begin searching.  */
      super_expr = build_component_ref (UOBJC_SUPER_decl,
					get_identifier ("class"));

      if (TREE_CODE (implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	{
	  /* [_cls, __cls]Super are "pre-built" in
	     synth_forward_declarations.  */

	  super_expr = build_modify_expr (super_expr, NOP_EXPR,
					  ((TREE_CODE (method_context)
					    == INSTANCE_METHOD_DECL)
					   ? ucls_super_ref
					   : uucls_super_ref));
	}

      else
	/* We have a category.  */
	{
	  tree super_name = CLASS_SUPER_NAME (implementation_template);
	  tree super_class;

	  if (!super_name)
	    {
	      error ("no super class declared in interface for `%s'",
		    IDENTIFIER_POINTER (CLASS_NAME (implementation_template)));
	      return error_mark_node;
	    }

	  if (flag_next_runtime)
	    {
	      super_class = get_class_reference (super_name);
	      if (TREE_CODE (method_context) == CLASS_METHOD_DECL)
		super_class
		  = build_component_ref (build_indirect_ref (super_class, "->"),
					 get_identifier ("isa"));
	    }
	  else
	    {
	      add_class_reference (super_name);
	      super_class = (TREE_CODE (method_context) == INSTANCE_METHOD_DECL
			     ? objc_get_class_decl : objc_get_meta_class_decl);
	      assemble_external (super_class);
	      super_class
		= build_function_call
		  (super_class,
		   build_tree_list
		   (NULL_TREE,
		    my_build_string (IDENTIFIER_LENGTH (super_name) + 1,
				     IDENTIFIER_POINTER (super_name))));
	    }

	  TREE_TYPE (super_class) = TREE_TYPE (ucls_super_ref);
	  super_expr = build_modify_expr (super_expr, NOP_EXPR, super_class);
	}

      chainon (super_expr_list, build_tree_list (NULL_TREE, super_expr));

      super_expr = build_unary_op (ADDR_EXPR, UOBJC_SUPER_decl, 0);
      chainon (super_expr_list, build_tree_list (NULL_TREE, super_expr));

      return build_compound_expr (super_expr_list);
    }
  else
    {
      error ("[super ...] must appear in a method context");
      return error_mark_node;
    }
}

static tree
encode_method_def (func_decl)
      tree func_decl;
{
  tree parms;
  int stack_size;
  int max_parm_end = 0;
  char buffer[40];
  tree result;

  /* Return type.  */
  encode_type (TREE_TYPE (TREE_TYPE (func_decl)),
	       obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);

  /* Stack size.  */
  for (parms = DECL_ARGUMENTS (func_decl); parms;
       parms = TREE_CHAIN (parms))
    {
      int parm_end = (forwarding_offset (parms)
		      + (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (parms)))
			 / BITS_PER_UNIT));

      if (!offset_is_register && parm_end > max_parm_end)
	max_parm_end = parm_end;
    }

  stack_size = max_parm_end - OBJC_FORWARDING_MIN_OFFSET;

  sprintf (buffer, "%d", stack_size);
  obstack_grow (&util_obstack, buffer, strlen (buffer));

  /* Argument types.  */
  for (parms = DECL_ARGUMENTS (func_decl); parms;
       parms = TREE_CHAIN (parms))
    {
      /* Type.  */
      encode_type (TREE_TYPE (parms),
		   obstack_object_size (&util_obstack),
		   OBJC_ENCODE_INLINE_DEFS);

      /* Compute offset.  */
      sprintf (buffer, "%d", forwarding_offset (parms));

      /* Indicate register.  */
      if (offset_is_register)
	obstack_1grow (&util_obstack, '+');

      obstack_grow (&util_obstack, buffer, strlen (buffer));
    }

  obstack_1grow (&util_obstack, 0);
  result = get_identifier (obstack_finish (&util_obstack));
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

void
finish_method_def ()
{
  METHOD_ENCODING (method_context) = encode_method_def (current_function_decl);

  finish_function (0);

  /* Required to implement _msgSuper. This must be done AFTER finish_function,
     since the optimizer may find "may be used before set" errors.  */
  method_context = NULL_TREE;
}

int
lang_report_error_function (decl)
      tree decl;
{
  if (method_context)
    {
      fprintf (stderr, "In method `%s'\n",
	       IDENTIFIER_POINTER (METHOD_SEL_NAME (method_context)));
      return 1;
    }

  else
    return 0;
}

static int
is_complex_decl (type)
     tree type;
{
  return (TREE_CODE (type) == ARRAY_TYPE
	  || TREE_CODE (type) == FUNCTION_TYPE
	  || (TREE_CODE (type) == POINTER_TYPE && ! IS_ID (type)));
}


/* Code to convert a decl node into text for a declaration in C.  */

static char tmpbuf[256];

static void
adorn_decl (decl, str)
     tree decl;
     char *str;
{
  enum tree_code code = TREE_CODE (decl);

  if (code == ARRAY_REF)
    {
      tree an_int_cst = TREE_OPERAND (decl, 1);

      if (an_int_cst && TREE_CODE (an_int_cst) == INTEGER_CST)
	sprintf (str + strlen (str), "[%d]", TREE_INT_CST_LOW (an_int_cst));
      else
	strcat (str, "[]");
    }

  else if (code == ARRAY_TYPE)
    {
      tree an_int_cst = TYPE_SIZE (decl);
      tree array_of = TREE_TYPE (decl);

      if (an_int_cst && TREE_CODE (an_int_cst) == INTEGER_TYPE)
	sprintf (str + strlen (str), "[%d]",
		 (TREE_INT_CST_LOW (an_int_cst)
		  / TREE_INT_CST_LOW (TYPE_SIZE (array_of))));
      else
	strcat (str, "[]");
    }

  else if (code == CALL_EXPR)
    {
      tree chain = TREE_PURPOSE (TREE_OPERAND (decl, 1));

      strcat (str, "(");
      while (chain)
	{
	  gen_declaration (chain, str);
	  chain = TREE_CHAIN (chain);
	  if (chain)
	    strcat (str, ", ");
	}
      strcat (str, ")");
    }

  else if (code == FUNCTION_TYPE)
    {
      tree chain  = TYPE_ARG_TYPES (decl);

      strcat (str, "(");
      while (chain && TREE_VALUE (chain) != void_type_node)
	{
	  gen_declaration (TREE_VALUE (chain), str);
	  chain = TREE_CHAIN (chain);
	  if (chain && TREE_VALUE (chain) != void_type_node)
	    strcat (str, ", ");
	}
      strcat (str, ")");
    }

  else if (code == INDIRECT_REF)
    {
      strcpy (tmpbuf, "*");
      if (TREE_TYPE (decl) && TREE_CODE (TREE_TYPE (decl)) == TREE_LIST)
	{
	  tree chain;

	  for (chain = nreverse (copy_list (TREE_TYPE (decl)));
	       chain;
	       chain = TREE_CHAIN (chain))
	    {
	      if (TREE_CODE (TREE_VALUE (chain)) == IDENTIFIER_NODE)
		{
		  strcat (tmpbuf, " ");
		  strcat (tmpbuf, IDENTIFIER_POINTER (TREE_VALUE (chain)));
		}
	    }
	  if (str[0])
	    strcat (tmpbuf, " ");
	}
      strcat (tmpbuf, str);
      strcpy (str, tmpbuf);
    }

  else if (code == POINTER_TYPE)
    {
      strcpy (tmpbuf, "*");
      if (TREE_READONLY (decl) || TYPE_VOLATILE (decl))
	{
	  if (TREE_READONLY (decl))
	    strcat (tmpbuf, " const");
	  if (TYPE_VOLATILE (decl))
	    strcat (tmpbuf, " volatile");
	  if (str[0])
	    strcat (tmpbuf, " ");
	}
      strcat (tmpbuf, str);
      strcpy (str, tmpbuf);
    }
}

static char *
gen_declarator (decl, buf, name)
     tree decl;
     char *buf;
     char *name;
{
  if (decl)
    {
      enum tree_code code = TREE_CODE (decl);
      char *str;
      tree op;
      int wrap = 0;

      switch (code)
	{
	case ARRAY_REF:
	case INDIRECT_REF:
	case CALL_EXPR:
	  op = TREE_OPERAND (decl, 0);

	  /* We have a pointer to a function or array...(*)(), (*)[] */
	  if ((code == ARRAY_REF || code == CALL_EXPR)
	      && op && TREE_CODE (op) == INDIRECT_REF)
	    wrap = 1;

	  str = gen_declarator (op, buf, name);

	  if (wrap)
	    {
	      strcpy (tmpbuf, "(");
	      strcat (tmpbuf, str);
	      strcat (tmpbuf, ")");
	      strcpy (str, tmpbuf);
	    }

	  adorn_decl (decl, str);
	  break;

	case ARRAY_TYPE:
	case FUNCTION_TYPE:
	case POINTER_TYPE:
	  strcpy (buf, name);
	  str = buf;

	  /* This clause is done iteratively rather than recursively.  */
	  do
	    {
	      op = (is_complex_decl (TREE_TYPE (decl))
		    ? TREE_TYPE (decl) : NULL_TREE);

	      adorn_decl (decl, str);

	      /* We have a pointer to a function or array...(*)(), (*)[] */
	      if (code == POINTER_TYPE
		  && op && (TREE_CODE (op) == FUNCTION_TYPE
			    || TREE_CODE (op) == ARRAY_TYPE))
		{
		  strcpy (tmpbuf, "(");
		  strcat (tmpbuf, str);
		  strcat (tmpbuf, ")");
		  strcpy (str, tmpbuf);
		}

	      decl = (is_complex_decl (TREE_TYPE (decl))
		      ? TREE_TYPE (decl) : NULL_TREE);
	    }

	  while (decl && (code = TREE_CODE (decl)))
	    ;

	  break;

	case IDENTIFIER_NODE:
	  /* Will only happen if we are processing a "raw" expr-decl.  */
	  strcpy (buf, IDENTIFIER_POINTER (decl));
	  return buf;
	}

      return str;
    }

  else
    /* We have an abstract declarator or a _DECL node.  */
    {
      strcpy (buf, name);
      return buf;
    }
}

static void
gen_declspecs (declspecs, buf, raw)
     tree declspecs;
     char *buf;
     int raw;
{
  if (raw)
    {
      tree chain;

      for (chain = nreverse (copy_list (declspecs));
	   chain; chain = TREE_CHAIN (chain))
	{
	  tree aspec = TREE_VALUE (chain);

	  if (TREE_CODE (aspec) == IDENTIFIER_NODE)
	    strcat (buf, IDENTIFIER_POINTER (aspec));
	  else if (TREE_CODE (aspec) == RECORD_TYPE)
	    {
	      if (TYPE_NAME (aspec))
		{
		  tree protocol_list = TYPE_PROTOCOL_LIST (aspec);

		  if (! TREE_STATIC_TEMPLATE (aspec))
		    strcat (buf, "struct ");
		  strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (aspec)));

		  /* NEW!!! */
		  if (protocol_list)
		    {
		      tree chain = protocol_list;

		      strcat (buf, " <");
		      while (chain)
			{
			  strcat (buf,
				  IDENTIFIER_POINTER
				  (PROTOCOL_NAME (TREE_VALUE (chain))));
			  chain = TREE_CHAIN (chain);
			  if (chain)
			    strcat (buf, ", ");
			}
		      strcat (buf, ">");
		    }
		}

	      else
		strcat (buf, "untagged struct");
	    }

	  else if (TREE_CODE (aspec) == UNION_TYPE)
	    {
	      if (TYPE_NAME (aspec))
		{
		  if (! TREE_STATIC_TEMPLATE (aspec))
		    strcat (buf, "union ");
		  strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (aspec)));
		}
	      else
		strcat (buf, "untagged union");
	    }

	  else if (TREE_CODE (aspec) == ENUMERAL_TYPE)
	    {
	      if (TYPE_NAME (aspec))
		{
		  if (! TREE_STATIC_TEMPLATE (aspec))
		    strcat (buf, "enum ");
		  strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (aspec)));
		}
	      else
		strcat (buf, "untagged enum");
	    }

	  else if (TREE_CODE (aspec) == TYPE_DECL && DECL_NAME (aspec))
	    strcat (buf, IDENTIFIER_POINTER (DECL_NAME (aspec)));

	  else if (IS_ID (aspec))
	    {
	      tree protocol_list = TYPE_PROTOCOL_LIST (aspec);

	      strcat (buf, "id");
	      if (protocol_list)
		{
		  tree chain = protocol_list;

		  strcat (buf, " <");
		  while (chain)
		    {
		      strcat (buf,
			      IDENTIFIER_POINTER
			      (PROTOCOL_NAME (TREE_VALUE (chain))));
		      chain = TREE_CHAIN (chain);
		      if (chain)
			strcat (buf, ", ");
		    }
		  strcat (buf, ">");
		}
	    }
	  if (TREE_CHAIN (chain))
	    strcat (buf, " ");
	}
    }
  else
    {
      /* Type qualifiers.  */
      if (TREE_READONLY (declspecs))
	strcat (buf, "const ");
      if (TYPE_VOLATILE (declspecs))
	strcat (buf, "volatile ");

      switch (TREE_CODE (declspecs))
	{
	  /* Type specifiers.  */

	case INTEGER_TYPE:
	  declspecs = TYPE_MAIN_VARIANT (declspecs);

	  /* Signed integer types.  */

	  if (declspecs == short_integer_type_node)
	    strcat (buf, "short int ");
	  else if (declspecs == integer_type_node)
	    strcat (buf, "int ");
	  else if (declspecs == long_integer_type_node)
	    strcat (buf, "long int ");
	  else if (declspecs == long_long_integer_type_node)
	    strcat (buf, "long long int ");
	  else if (declspecs == signed_char_type_node
		   || declspecs == char_type_node)
	    strcat (buf, "char ");

	  /* Unsigned integer types.  */

	  else if (declspecs == short_unsigned_type_node)
	    strcat (buf, "unsigned short ");
	  else if (declspecs == unsigned_type_node)
	    strcat (buf, "unsigned int ");
	  else if (declspecs == long_unsigned_type_node)
	    strcat (buf, "unsigned long ");
	  else if (declspecs == long_long_unsigned_type_node)
	    strcat (buf, "unsigned long long ");
	  else if (declspecs == unsigned_char_type_node)
	    strcat (buf, "unsigned char ");
	  break;

	case REAL_TYPE:
	  declspecs = TYPE_MAIN_VARIANT (declspecs);

	  if (declspecs == float_type_node)
	    strcat (buf, "float ");
	  else if (declspecs == double_type_node)
	    strcat (buf, "double ");
	  else if (declspecs == long_double_type_node)
	    strcat (buf, "long double ");
	  break;

      case RECORD_TYPE:
	  if (TYPE_NAME (declspecs)
	      && TREE_CODE (TYPE_NAME (declspecs)) == IDENTIFIER_NODE)
	    {
	      tree protocol_list = TYPE_PROTOCOL_LIST (declspecs);

	      if (! TREE_STATIC_TEMPLATE (declspecs))
		strcat (buf, "struct ");
	      strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (declspecs)));

	      if (protocol_list)
		{
		  tree chain = protocol_list;

		  strcat (buf, " <");
		  while (chain)
		    {
		      strcat (buf,
			      IDENTIFIER_POINTER
			      (PROTOCOL_NAME (TREE_VALUE (chain))));
		      chain = TREE_CHAIN (chain);
		      if (chain)
			strcat (buf, ", ");
		    }
		  strcat (buf, ">");
		}
	    }

	  else
	    strcat (buf, "untagged struct");

	  strcat (buf, " ");
	  break;

	case UNION_TYPE:
	  if (TYPE_NAME (declspecs)
	      && TREE_CODE (TYPE_NAME (declspecs)) == IDENTIFIER_NODE)
	    {
	      strcat (buf, "union ");
	      strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (declspecs)));
	      strcat (buf, " ");
	    }

	  else
	    strcat (buf, "untagged union ");
	  break;

	case ENUMERAL_TYPE:
	  if (TYPE_NAME (declspecs)
	      && TREE_CODE (TYPE_NAME (declspecs)) == IDENTIFIER_NODE)
	    {
	      strcat (buf, "enum ");
	      strcat (buf, IDENTIFIER_POINTER (TYPE_NAME (declspecs)));
	      strcat (buf, " ");
	    }

	  else
	    strcat (buf, "untagged enum ");
	  break;

	case VOID_TYPE:
	  strcat (buf, "void ");
	  break;

	case POINTER_TYPE:
	  {
	    tree protocol_list = TYPE_PROTOCOL_LIST (declspecs);

	    strcat (buf, "id");
	    if (protocol_list)
	      {
		tree chain = protocol_list;

		strcat (buf, " <");
		while (chain)
		  {
		    strcat (buf,
			    IDENTIFIER_POINTER
			    (PROTOCOL_NAME (TREE_VALUE (chain))));
		    chain = TREE_CHAIN (chain);
		    if (chain)
		      strcat (buf, ", ");
		  }

		strcat (buf, ">");
	      }
	  }
	}
    }
}

static char *
gen_declaration (atype_or_adecl, buf)
     tree atype_or_adecl;
     char *buf;
{
  char declbuf[256];

  if (TREE_CODE (atype_or_adecl) == TREE_LIST)
    {
      tree declspecs;	/* "identifier_node", "record_type" */
      tree declarator;	/* "array_ref", "indirect_ref", "call_expr"...  */

      /* We have a "raw", abstract declarator (typename).  */
      declarator = TREE_VALUE (atype_or_adecl);
      declspecs  = TREE_PURPOSE (atype_or_adecl);

      gen_declspecs (declspecs, buf, 1);
      if (declarator)
	{
	  strcat (buf, " ");
	  strcat (buf, gen_declarator (declarator, declbuf, ""));
	}
    }

  else
    {
      tree atype;
      tree declspecs;	/* "integer_type", "real_type", "record_type"...  */
      tree declarator;	/* "array_type", "function_type", "pointer_type".  */

      if (TREE_CODE (atype_or_adecl) == FIELD_DECL
	  || TREE_CODE (atype_or_adecl) == PARM_DECL
	  || TREE_CODE (atype_or_adecl) == FUNCTION_DECL)
	atype = TREE_TYPE (atype_or_adecl);
      else
	/* Assume we have a *_type node.  */
	atype = atype_or_adecl;

      if (is_complex_decl (atype))
	{
	  tree chain;

	  /* Get the declaration specifier; it is at the end of the list.  */
	  declarator = chain = atype;
	  do
	    chain = TREE_TYPE (chain); /* not TREE_CHAIN (chain); */
	  while (is_complex_decl (chain));
	  declspecs = chain;
	}

      else
	{
	  declspecs = atype;
	  declarator = NULL_TREE;
	}

      gen_declspecs (declspecs, buf, 0);

      if (TREE_CODE (atype_or_adecl) == FIELD_DECL
	  || TREE_CODE (atype_or_adecl) == PARM_DECL
	  || TREE_CODE (atype_or_adecl) == FUNCTION_DECL)
	{
	  char *decl_name = (DECL_NAME (atype_or_adecl)
			     ? IDENTIFIER_POINTER (DECL_NAME (atype_or_adecl))
			     : "");

	  if (declarator)
	    {
	      strcat (buf, " ");
	      strcat (buf, gen_declarator (declarator, declbuf, decl_name));
	    }

	  else if (decl_name[0])
	    {
	      strcat (buf, " ");
	      strcat (buf, decl_name);
	    }
	}
      else if (declarator)
	{
	  strcat (buf, " ");
	  strcat (buf, gen_declarator (declarator, declbuf, ""));
	}
    }

  return buf;
}

#define RAW_TYPESPEC(meth) (TREE_VALUE (TREE_PURPOSE (TREE_TYPE (meth))))

static char *
gen_method_decl (method, buf)
     tree method;
     char *buf;
{
  tree chain;

  if (RAW_TYPESPEC (method) != objc_object_reference)
    {
      strcpy (buf, "(");
      gen_declaration (TREE_TYPE (method), buf);
      strcat (buf, ")");
    }

  chain = METHOD_SEL_ARGS (method);
  if (chain)
    {
      /* We have a chain of keyword_decls.  */
      do
        {
	  if (KEYWORD_KEY_NAME (chain))
	    strcat (buf, IDENTIFIER_POINTER (KEYWORD_KEY_NAME (chain)));

	  strcat (buf, ":");
	  if (RAW_TYPESPEC (chain) != objc_object_reference)
	    {
	      strcat (buf, "(");
	      gen_declaration (TREE_TYPE (chain), buf);
	      strcat (buf, ")");
	    }

	  strcat (buf, IDENTIFIER_POINTER (KEYWORD_ARG_NAME (chain)));
	  if ((chain = TREE_CHAIN (chain)))
	    strcat (buf, " ");
        }
      while (chain);

      if (METHOD_ADD_ARGS (method) == (tree)1)
        strcat (buf, ", ...");
      else if (METHOD_ADD_ARGS (method))
        {
	  /* We have a tree list node as generate by get_parm_info.  */
	  chain  = TREE_PURPOSE (METHOD_ADD_ARGS (method));

          /* Know we have a chain of parm_decls.  */
          while (chain)
            {
	      strcat (buf, ", ");
	      gen_declaration (chain, buf);
	      chain = TREE_CHAIN (chain);
            }
	}
    }

  else
    /* We have a unary selector.  */
    strcat (buf, IDENTIFIER_POINTER (METHOD_SEL_NAME (method)));

  return buf;
}

/* Debug info.  */

static void
dump_interface (fp, chain)
     FILE *fp;
     tree chain;
{
  char *buf = (char *)xmalloc (256);
  char *my_name = IDENTIFIER_POINTER (CLASS_NAME (chain));
  tree ivar_decls = CLASS_RAW_IVARS (chain);
  tree nst_methods = CLASS_NST_METHODS (chain);
  tree cls_methods = CLASS_CLS_METHODS (chain);

  fprintf (fp, "\n@interface %s", my_name);

  if (CLASS_SUPER_NAME (chain))
    {
      char *super_name = IDENTIFIER_POINTER (CLASS_SUPER_NAME (chain));
      fprintf (fp, " : %s\n", super_name);
    }
  else
    fprintf (fp, "\n");

  if (ivar_decls)
    {
      fprintf (fp, "{\n");
      do
	{
	  bzero (buf, 256);
	  fprintf (fp, "\t%s;\n", gen_declaration (ivar_decls, buf));
	  ivar_decls = TREE_CHAIN (ivar_decls);
	}
      while (ivar_decls);
      fprintf (fp, "}\n");
    }

  while (nst_methods)
    {
      bzero (buf, 256);
      fprintf (fp, "- %s;\n", gen_method_decl (nst_methods, buf));
      nst_methods = TREE_CHAIN (nst_methods);
    }

  while (cls_methods)
    {
      bzero (buf, 256);
      fprintf (fp, "+ %s;\n", gen_method_decl (cls_methods, buf));
      cls_methods = TREE_CHAIN (cls_methods);
    }
  fprintf (fp, "\n@end");
}

static void
init_objc ()
{
  /* Add the special tree codes of Objective C to the tables.  */

#define LAST_CODE LAST_AND_UNUSED_TREE_CODE

  gcc_obstack_init (&util_obstack);
  util_firstobj = (char *) obstack_finish (&util_obstack);

  tree_code_type
    = (char **) xrealloc (tree_code_type,
			  sizeof (char *) * LAST_OBJC_TREE_CODE);
  tree_code_length
    = (int *) xrealloc (tree_code_length,
			sizeof (int) * LAST_OBJC_TREE_CODE);
  tree_code_name
    = (char **) xrealloc (tree_code_name,
			  sizeof (char *) * LAST_OBJC_TREE_CODE);
  bcopy ((char *) objc_tree_code_type,
	 (char *) (tree_code_type + (int) LAST_CODE),
	 (((int) LAST_OBJC_TREE_CODE - (int) LAST_CODE)
	  * sizeof (char *)));
  bcopy ((char *) objc_tree_code_length,
	 (char *) (tree_code_length + (int) LAST_CODE),
	 (((int) LAST_OBJC_TREE_CODE - (int) LAST_CODE)
	  * sizeof (int)));
  bcopy ((char *) objc_tree_code_name,
	 (char *) (tree_code_name + (int) LAST_CODE),
	 (((int) LAST_OBJC_TREE_CODE - (int) LAST_CODE)
	  * sizeof (char *)));

  errbuf = (char *)xmalloc (BUFSIZE);
  hash_init ();
  synth_module_prologue ();
}

static void
finish_objc ()
{
  struct imp_entry *impent;
  tree chain;
  /* The internally generated initializers appear to have missing braces.
     Don't warn about this.  */
  int save_warn_missing_braces = warn_missing_braces;
  warn_missing_braces = 0;

  generate_forward_declaration_to_string_table ();

#ifdef OBJC_PROLOGUE
  OBJC_PROLOGUE;
#endif

  if (implementation_context || class_names_chain
      || meth_var_names_chain || meth_var_types_chain || sel_ref_chain)
    generate_objc_symtab_decl ();

  for (impent = imp_list; impent; impent = impent->next)
    {
      implementation_context = impent->imp_context;
      implementation_template = impent->imp_template;

      UOBJC_CLASS_decl = impent->class_decl;
      UOBJC_METACLASS_decl = impent->meta_decl;

      if (TREE_CODE (implementation_context) == CLASS_IMPLEMENTATION_TYPE)
	{
	  /* all of the following reference the string pool...  */
	  generate_ivar_lists ();
	  generate_dispatch_tables ();
	  generate_shared_structures ();
	}
      else
	{
	  generate_dispatch_tables ();
	  generate_category (implementation_context);
	}
    }

  /* If we are using an array of selectors, we must always
     finish up the array decl even if no selectors were used.  */
  if (! flag_next_runtime || sel_ref_chain)
    build_selector_translation_table ();

  if (protocol_chain)
    generate_protocols ();

  if (objc_static_instances)
    generate_static_references ();

  if (implementation_context || class_names_chain || objc_static_instances
      || meth_var_names_chain || meth_var_types_chain || sel_ref_chain)
    {
      /* Arrange for Objc data structures to be initialized at run time.  */
      char *init_name = build_module_descriptor ();
      if (init_name)
	assemble_constructor (init_name);
    }

  /* Dump the class references.  This forces the appropriate classes
     to be linked into the executable image, preserving unix archive
     semantics.  This can be removed when we move to a more dynamically
     linked environment.  */

  for (chain = cls_ref_chain; chain; chain = TREE_CHAIN (chain))
    {
      handle_class_ref (chain);
      if (TREE_PURPOSE (chain))
	generate_classref_translation_entry (chain);
    }

  for (impent = imp_list; impent; impent = impent->next)
    handle_impent (impent);

  /* Dump the string table last.  */

  generate_strings ();

  if (flag_gen_declaration)
    {
      add_class (implementation_context);
      dump_interface (gen_declaration_file, implementation_context);
    }

  if (warn_selector)
    {
      int slot;
      hash hsh;

      /* Run through the selector hash tables and print a warning for any
         selector which has multiple methods.  */

      for (slot = 0; slot < SIZEHASHTABLE; slot++)
	for (hsh = cls_method_hash_list[slot]; hsh; hsh = hsh->next)
	  if (hsh->list)
	    {
	      tree meth = hsh->key;
	      char type = (TREE_CODE (meth) == INSTANCE_METHOD_DECL
			   ? '-' : '+');
	      attr loop;

	      warning ("potential selector conflict for method `%s'",
		       IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));
	      warn_with_method ("found", type, meth);
	      for (loop = hsh->list; loop; loop = loop->next)
		warn_with_method ("found", type, loop->value);
	    }

      for (slot = 0; slot < SIZEHASHTABLE; slot++)
	for (hsh = nst_method_hash_list[slot]; hsh; hsh = hsh->next)
	  if (hsh->list)
	    {
	      tree meth = hsh->key;
	      char type = (TREE_CODE (meth) == INSTANCE_METHOD_DECL
			   ? '-' : '+');
	      attr loop;

	      warning ("potential selector conflict for method `%s'",
		       IDENTIFIER_POINTER (METHOD_SEL_NAME (meth)));
	      warn_with_method ("found", type, meth);
	      for (loop = hsh->list; loop; loop = loop->next)
		warn_with_method ("found", type, loop->value);
	    }
    }

  warn_missing_braces = save_warn_missing_braces;
}

/* Subroutines of finish_objc.  */

static void
generate_classref_translation_entry (chain)
    tree chain;
{
  tree expr, name, decl_specs, decl, sc_spec;
  tree type;

  type = TREE_TYPE (TREE_PURPOSE (chain));

  expr = add_objc_string (TREE_VALUE (chain), class_names);
  expr = build_c_cast (type, expr); /* cast! */

  name = DECL_NAME (TREE_PURPOSE (chain));

  sc_spec = build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]);

  /* static struct objc_class * _OBJC_CLASS_REFERENCES_n = ...; */
  decl_specs = tree_cons (NULL_TREE, type, sc_spec);

  /* The decl that is returned from start_decl is the one that we
     forward declared in build_class_reference.  */
  decl = start_decl (name, decl_specs, 1, NULL_TREE, NULL_TREE);
  finish_decl (decl, expr, NULL_TREE);
  return;
}

static void
handle_class_ref (chain)
     tree chain;
{
  char *name = IDENTIFIER_POINTER (TREE_VALUE (chain));
  if (! flag_next_runtime)
    {
      tree decl;
      char *string = (char *) alloca (strlen (name) + 30);
      tree exp;

      sprintf (string, "%sobjc_class_name_%s",
	       (flag_next_runtime ? "." : "__"), name);

      /* Make a decl for this name, so we can use its address in a tree.  */
      decl = build_decl (VAR_DECL, get_identifier (string), char_type_node);
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;

      pushdecl (decl);
      rest_of_decl_compilation (decl, 0, 0, 0);

      /* Make following constant read-only (why not)?  */
      readonly_data_section ();

      exp = build1 (ADDR_EXPR, string_type_node, decl);

      /* Align the section properly.  */
      assemble_constant_align (exp);

      /* Inform the assembler about this new external thing.  */
      assemble_external (decl);

      /* Output a constant to reference this address.  */
      output_constant (exp, int_size_in_bytes (string_type_node));
    }
  else
    {
      /* This overreliance on our assembler (i.e. lack of portability)
	 should be dealt with at some point.  The GNU strategy (above)
	 won't work either, but it is a start.  */
      char *string = (char *) alloca (strlen (name) + 30);
      sprintf (string, ".reference .objc_class_name_%s", name);
      assemble_asm (my_build_string (strlen (string) + 1, string));
    }
}

static void
handle_impent (impent)
     struct imp_entry *impent;
{
  implementation_context = impent->imp_context;
  implementation_template = impent->imp_template;

  if (TREE_CODE (impent->imp_context) == CLASS_IMPLEMENTATION_TYPE)
    {
      char *class_name = IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context));
      char *string = (char *) alloca (strlen (class_name) + 30);

      if (flag_next_runtime)
	{
	  /* Grossly unportable.
	     People should know better than to assume
	     such things about assembler syntax!  */
	  sprintf (string, ".objc_class_name_%s=0", class_name);
	  assemble_asm (my_build_string (strlen (string) + 1, string));

	  sprintf (string, ".globl .objc_class_name_%s", class_name);
	  assemble_asm (my_build_string (strlen (string) + 1, string));
	}

      else
	{
	  sprintf (string, "%sobjc_class_name_%s",
		   (flag_next_runtime ? "." : "__"), class_name);
	  assemble_global (string);
	  assemble_label (string);
	}
    }

  else if (TREE_CODE (impent->imp_context) == CATEGORY_IMPLEMENTATION_TYPE)
    {
      char *class_name = IDENTIFIER_POINTER (CLASS_NAME (impent->imp_context));
      char *class_super_name
	= IDENTIFIER_POINTER (CLASS_SUPER_NAME (impent->imp_context));
      char *string = (char *) alloca (strlen (class_name)
				      + strlen (class_super_name) + 30);

      /* Do the same for categories.  Even though no references to these
	 symbols are generated automatically by the compiler, it gives
	 you a handle to pull them into an archive by hand.  */
      if (flag_next_runtime)
	{
	  /* Grossly unportable.  */
	  sprintf (string, ".objc_category_name_%s_%s=0",
		   class_name, class_super_name);
	  assemble_asm (my_build_string (strlen (string) + 1, string));

	  sprintf (string, ".globl .objc_category_name_%s_%s",
		   class_name, class_super_name);
	  assemble_asm (my_build_string (strlen (string) + 1, string));
	}

      else
	{
	  sprintf (string, "%sobjc_category_name_%s_%s",
		   (flag_next_runtime ? "." : "__"),
		   class_name, class_super_name);
	  assemble_global (string);
	  assemble_label (string);
	}
    }
}

#ifdef DEBUG

static void
objc_debug (fp)
     FILE *fp;
{
  char *buf = (char *)xmalloc (256);

  {				/* dump function prototypes */
    tree loop = UOBJC_MODULES_decl;

    fprintf (fp, "\n\nfunction prototypes:\n");
    while (loop)
      {
	if (TREE_CODE (loop) == FUNCTION_DECL && DECL_INITIAL (loop))
	  {
	    /* We have a function definition: generate prototype.  */
            bzero (errbuf, BUFSIZE);
	    gen_declaration (loop, errbuf);
	    fprintf (fp, "%s;\n", errbuf);
	  }
	loop = TREE_CHAIN (loop);
      }
  }
  {
    /* Dump global chains.  */
    tree loop;
    int i, index = 0, offset = 0;
    hash hashlist;

    for (i = 0; i < SIZEHASHTABLE; i++)
      {
	if (hashlist = nst_method_hash_list[i])
	  {
	    fprintf (fp, "\n\nnst_method_hash_list[%d]:\n", i);
	    do
	      {
		bzero (buf, 256);
		fprintf (fp, "-%s;\n", gen_method_decl (hashlist->key, buf));
		hashlist = hashlist->next;
	      }
	    while (hashlist);
	  }
      }

    for (i = 0; i < SIZEHASHTABLE; i++)
      {
	if (hashlist = cls_method_hash_list[i])
	  {
	    fprintf (fp, "\n\ncls_method_hash_list[%d]:\n", i);
	    do
	      {
		bzero (buf, 256);
		fprintf (fp, "-%s;\n", gen_method_decl (hashlist->key, buf));
		hashlist = hashlist->next;
	      }
	    while (hashlist);
	  }
      }

    fprintf (fp, "\nsel_refdef_chain:\n");
    for (loop = sel_refdef_chain; loop; loop = TREE_CHAIN (loop))
      {
	fprintf (fp, "(index: %4d offset: %4d) %s\n", index, offset,
		 IDENTIFIER_POINTER (TREE_VALUE (loop)));
	index++;
	/* add one for the '\0' character */
	offset += IDENTIFIER_LENGTH (TREE_VALUE (loop)) + 1;
      }

    fprintf (fp, "\n (max_selector_index: %4d.\n", max_selector_index);
  }
}
#endif

void
print_lang_statistics ()
{
}
