/* Abstraction of GNU v2 abi.
   Contributed by Daniel Berlin <dberlin@redhat.com>
   Copyright 2001 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or
   modify
   it under the terms of the GNU General Public License as published
   by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "gdb_string.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "value.h"
#include "demangle.h"
#include "cp-abi.h"

#include <ctype.h>

struct cp_abi_ops gnu_v2_abi_ops;

static int vb_match (struct type *, int, struct type *);

static enum dtor_kinds
gnuv2_is_destructor_name (const char *name)
{
  if ((name[0] == '_' && is_cplus_marker (name[1]) && name[2] == '_')
      || strncmp (name, "__dt__", 6) == 0)
    return complete_object_dtor;
  else
    return 0;
}

static enum ctor_kinds
gnuv2_is_constructor_name (const char *name)
{
  if ((name[0] == '_' && name[1] == '_'
       && (isdigit (name[2]) || strchr ("Qt", name[2])))
      || strncmp (name, "__ct__", 6) == 0)
    return complete_object_ctor;
  else
    return 0;
}

static int
gnuv2_is_vtable_name (const char *name)
{
  return (((name)[0] == '_'
	   && (((name)[1] == 'V' && (name)[2] == 'T')
	       || ((name)[1] == 'v' && (name)[2] == 't'))
	   && is_cplus_marker ((name)[3])) ||
	  ((name)[0] == '_' && (name)[1] == '_'
	   && (name)[2] == 'v' && (name)[3] == 't' && (name)[4] == '_'));
}

static int
gnuv2_is_operator_name (const char *name)
{
  return strncmp (name, "operator", 8) == 0;
}


/* Return a virtual function as a value.
   ARG1 is the object which provides the virtual function
   table pointer.  *ARG1P is side-effected in calling this function.
   F is the list of member functions which contains the desired virtual
   function.
   J is an index into F which provides the desired virtual function.

   TYPE is the type in which F is located.  */
static value_ptr
gnuv2_virtual_fn_field (value_ptr * arg1p, struct fn_field * f, int j,
			struct type * type, int offset)
{
  value_ptr arg1 = *arg1p;
  struct type *type1 = check_typedef (VALUE_TYPE (arg1));


  struct type *entry_type;
  /* First, get the virtual function table pointer.  That comes
     with a strange type, so cast it to type `pointer to long' (which
     should serve just fine as a function type).  Then, index into
     the table, and convert final value to appropriate function type.  */
  value_ptr entry, vfn, vtbl;
  value_ptr vi = value_from_longest (builtin_type_int,
				     (LONGEST) TYPE_FN_FIELD_VOFFSET (f, j));
  struct type *fcontext = TYPE_FN_FIELD_FCONTEXT (f, j);
  struct type *context;
  if (fcontext == NULL)
    /* We don't have an fcontext (e.g. the program was compiled with
       g++ version 1).  Try to get the vtbl from the TYPE_VPTR_BASETYPE.
       This won't work right for multiple inheritance, but at least we
       should do as well as GDB 3.x did.  */
    fcontext = TYPE_VPTR_BASETYPE (type);
  context = lookup_pointer_type (fcontext);
  /* Now context is a pointer to the basetype containing the vtbl.  */
  if (TYPE_TARGET_TYPE (context) != type1)
    {
      value_ptr tmp = value_cast (context, value_addr (arg1));
      arg1 = value_ind (tmp);
      type1 = check_typedef (VALUE_TYPE (arg1));
    }

  context = type1;
  /* Now context is the basetype containing the vtbl.  */

  /* This type may have been defined before its virtual function table
     was.  If so, fill in the virtual function table entry for the
     type now.  */
  if (TYPE_VPTR_FIELDNO (context) < 0)
    fill_in_vptr_fieldno (context);

  /* The virtual function table is now an array of structures
     which have the form { int16 offset, delta; void *pfn; }.  */
  vtbl = value_primitive_field (arg1, 0, TYPE_VPTR_FIELDNO (context),
				TYPE_VPTR_BASETYPE (context));

  /* With older versions of g++, the vtbl field pointed to an array
     of structures.  Nowadays it points directly to the structure. */
  if (TYPE_CODE (VALUE_TYPE (vtbl)) == TYPE_CODE_PTR
      && TYPE_CODE (TYPE_TARGET_TYPE (VALUE_TYPE (vtbl))) == TYPE_CODE_ARRAY)
    {
      /* Handle the case where the vtbl field points to an
         array of structures. */
      vtbl = value_ind (vtbl);

      /* Index into the virtual function table.  This is hard-coded because
         looking up a field is not cheap, and it may be important to save
         time, e.g. if the user has set a conditional breakpoint calling
         a virtual function.  */
      entry = value_subscript (vtbl, vi);
    }
  else
    {
      /* Handle the case where the vtbl field points directly to a structure. */
      vtbl = value_add (vtbl, vi);
      entry = value_ind (vtbl);
    }

  entry_type = check_typedef (VALUE_TYPE (entry));

  if (TYPE_CODE (entry_type) == TYPE_CODE_STRUCT)
    {
      /* Move the `this' pointer according to the virtual function table. */
      VALUE_OFFSET (arg1) += value_as_long (value_field (entry, 0));

      if (!VALUE_LAZY (arg1))
	{
	  VALUE_LAZY (arg1) = 1;
	  value_fetch_lazy (arg1);
	}

      vfn = value_field (entry, 2);
    }
  else if (TYPE_CODE (entry_type) == TYPE_CODE_PTR)
    vfn = entry;
  else
    error ("I'm confused:  virtual function table has bad type");
  /* Reinstantiate the function pointer with the correct type.  */
  VALUE_TYPE (vfn) = lookup_pointer_type (TYPE_FN_FIELD_TYPE (f, j));

  *arg1p = arg1;
  return vfn;
}


struct type *
gnuv2_value_rtti_type (value_ptr v, int *full, int *top, int *using_enc)
{
  struct type *known_type;
  struct type *rtti_type;
  CORE_ADDR coreptr;
  value_ptr vp;
  int using_enclosing = 0;
  long top_offset = 0;
  char rtti_type_name[256];
  CORE_ADDR vtbl;
  struct minimal_symbol *minsym;
  struct symbol *sym;
  char *demangled_name;
  struct type *btype;

  if (full)
    *full = 0;
  if (top)
    *top = -1;
  if (using_enc)
    *using_enc = 0;

  /* Get declared type */
  known_type = VALUE_TYPE (v);
  CHECK_TYPEDEF (known_type);
  /* RTTI works only or class objects */
  if (TYPE_CODE (known_type) != TYPE_CODE_CLASS)
    return NULL;

  /* Plan on this changing in the future as i get around to setting
     the vtables properly for G++ compiled stuff.  Also, I'll be using
     the type info functions, which are always right.  Deal with it
     until then.  */

  /* If the type has no vptr fieldno, try to get it filled in */
  if (TYPE_VPTR_FIELDNO(known_type) < 0)
    fill_in_vptr_fieldno(known_type);

  /* If we still can't find one, give up */
  if (TYPE_VPTR_FIELDNO(known_type) < 0)
    return NULL;

  /* Make sure our basetype and known type match, otherwise, cast
     so we can get at the vtable properly.
  */
  btype = TYPE_VPTR_BASETYPE (known_type);
  CHECK_TYPEDEF (btype);
  if (btype != known_type )
    {
      v = value_cast (btype, v);
      if (using_enc)
        *using_enc=1;
    }
  /*
    We can't use value_ind here, because it would want to use RTTI, and
    we'd waste a bunch of time figuring out we already know the type.
    Besides, we don't care about the type, just the actual pointer
  */
  if (VALUE_ADDRESS (value_field (v, TYPE_VPTR_FIELDNO (known_type))) == 0)
    return NULL;

  /*
    If we are enclosed by something that isn't us, adjust the
    address properly and set using_enclosing.
  */
  if (VALUE_ENCLOSING_TYPE(v) != VALUE_TYPE(v))
    {
      value_ptr tempval;
      int bitpos = TYPE_BASECLASS_BITPOS (known_type,
                                          TYPE_VPTR_FIELDNO (known_type));
      tempval=value_field (v, TYPE_VPTR_FIELDNO(known_type));
      VALUE_ADDRESS(tempval) += bitpos / 8;
      vtbl=value_as_pointer (tempval);
      using_enclosing=1;
    }
  else
    {
      vtbl=value_as_pointer(value_field(v,TYPE_VPTR_FIELDNO(known_type)));
      using_enclosing=0;
    }

  /* Try to find a symbol that is the vtable */
  minsym=lookup_minimal_symbol_by_pc(vtbl);
  if (minsym==NULL
      || (demangled_name=SYMBOL_NAME(minsym))==NULL
      || !is_vtable_name (demangled_name))
    return NULL;

  /* If we just skip the prefix, we get screwed by namespaces */
  demangled_name=cplus_demangle(demangled_name,DMGL_PARAMS|DMGL_ANSI);
  *(strchr(demangled_name,' '))=0;

  /* Lookup the type for the name */
  rtti_type=lookup_typename(demangled_name, (struct block *)0,1);

  if (rtti_type==NULL)
    return NULL;

  if (TYPE_N_BASECLASSES(rtti_type) > 1 &&  full && (*full) != 1)
    {
      if (top)
        *top=TYPE_BASECLASS_BITPOS(rtti_type,TYPE_VPTR_FIELDNO(rtti_type))/8;
      if (top && ((*top) >0))
        {
          if (TYPE_LENGTH(rtti_type) > TYPE_LENGTH(known_type))
            {
              if (full)
                *full=0;
            }
          else
            {
              if (full)
                *full=1;
            }
        }
    }
  else
    {
      if (full)
        *full=1;
    }
  if (using_enc)
    *using_enc=using_enclosing;

  return rtti_type;
}


static void
init_gnuv2_ops (void)
{
  gnu_v2_abi_ops.shortname = "gnu-v2";
  gnu_v2_abi_ops.longname = "GNU G++ Version 2 ABI";
  gnu_v2_abi_ops.doc = "G++ Version 2 ABI";
  gnu_v2_abi_ops.is_destructor_name = gnuv2_is_destructor_name;
  gnu_v2_abi_ops.is_constructor_name = gnuv2_is_constructor_name;
  gnu_v2_abi_ops.is_vtable_name = gnuv2_is_vtable_name;
  gnu_v2_abi_ops.is_operator_name = gnuv2_is_operator_name;
  gnu_v2_abi_ops.virtual_fn_field = gnuv2_virtual_fn_field;
  gnu_v2_abi_ops.rtti_type = gnuv2_value_rtti_type;
}

void
_initialize_gnu_v2_abi (void)
{
  init_gnuv2_ops ();
  register_cp_abi (gnu_v2_abi_ops);
  switch_to_cp_abi ("gnu-v2");
}
