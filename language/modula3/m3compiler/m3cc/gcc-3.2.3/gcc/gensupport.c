/* Support routines for the various generation passes.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"
#include "gensupport.h"


/* In case some macros used by files we include need it, define this here.  */
int target_flags;

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

static int sequence_num;
static int errors;

static int predicable_default;
static const char *predicable_true;
static const char *predicable_false;

static char *base_dir = NULL;

/* We initially queue all patterns, process the define_insn and
   define_cond_exec patterns, then return them one at a time.  */

struct queue_elem
{
  rtx data;
  int lineno;
  struct queue_elem *next;
};

static struct queue_elem *define_attr_queue;
static struct queue_elem **define_attr_tail = &define_attr_queue;
static struct queue_elem *define_insn_queue;
static struct queue_elem **define_insn_tail = &define_insn_queue;
static struct queue_elem *define_cond_exec_queue;
static struct queue_elem **define_cond_exec_tail = &define_cond_exec_queue;
static struct queue_elem *other_queue;
static struct queue_elem **other_tail = &other_queue;

static void queue_pattern PARAMS ((rtx, struct queue_elem ***, int));

/* Current maximum length of directory names in the search path
   for include files.  (Altered as we get more of them.)  */

size_t max_include_len;

struct file_name_list
  {
    struct file_name_list *next;
    const char *fname;
  };

struct file_name_list *include = 0;     /* First dir to search */
        /* First dir to search for <file> */
struct file_name_list *first_bracket_include = 0;
struct file_name_list *last_include = 0;        /* Last in chain */

static void remove_constraints PARAMS ((rtx));
static void process_rtx PARAMS ((rtx, int));

static int is_predicable PARAMS ((struct queue_elem *));
static void identify_predicable_attribute PARAMS ((void));
static int n_alternatives PARAMS ((const char *));
static void collect_insn_data PARAMS ((rtx, int *, int *));
static rtx alter_predicate_for_insn PARAMS ((rtx, int, int, int));
static const char *alter_test_for_insn PARAMS ((struct queue_elem *,
						struct queue_elem *));
static char *shift_output_template PARAMS ((char *, const char *, int));
static const char *alter_output_for_insn PARAMS ((struct queue_elem *,
						  struct queue_elem *,
						  int, int));
static void process_one_cond_exec PARAMS ((struct queue_elem *));
static void process_define_cond_exec PARAMS ((void));
static int process_include PARAMS ((rtx, int));
static char *save_string PARAMS ((const char *, int));
static int init_include_reader PARAMS ((FILE  *));

void
message_with_line VPARAMS ((int lineno, const char *msg, ...))
{
  VA_OPEN (ap, msg);
  VA_FIXEDARG (ap, int, lineno);
  VA_FIXEDARG (ap, const char *, msg);

  fprintf (stderr, "%s:%d: ", read_rtx_filename, lineno);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);

  VA_CLOSE (ap);
}

/* Make a version of gen_rtx_CONST_INT so that GEN_INT can be used in
   the gensupport programs.  */

rtx
gen_rtx_CONST_INT (mode, arg)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     HOST_WIDE_INT arg;
{
  rtx rt = rtx_alloc (CONST_INT);

  XWINT (rt, 0) = arg;
  return rt;
}

/* Queue PATTERN on LIST_TAIL.  */

static void
queue_pattern (pattern, list_tail, lineno)
     rtx pattern;
     struct queue_elem ***list_tail;
     int lineno;
{
  struct queue_elem *e = (struct queue_elem *) xmalloc (sizeof (*e));
  e->data = pattern;
  e->lineno = lineno;
  e->next = NULL;
  **list_tail = e;
  *list_tail = &e->next;
}

/* Recursively remove constraints from an rtx.  */

static void
remove_constraints (part)
     rtx part;
{
  int i, j;
  const char *format_ptr;

  if (part == 0)
    return;

  if (GET_CODE (part) == MATCH_OPERAND)
    XSTR (part, 2) = "";
  else if (GET_CODE (part) == MATCH_SCRATCH)
    XSTR (part, 1) = "";

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
      case 'u':
	remove_constraints (XEXP (part, i));
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    remove_constraints (XVECEXP (part, i, j));
	break;
      }
}

/* The entry point for initializing the reader.  */

static int
init_include_reader (inf)
     FILE *inf;
{
  int c;

  errors = 0;

  /* Read the entire file.  */
  while (1)
    {
      rtx desc;
      int lineno;

      c = read_skip_spaces (inf);
      if (c == EOF)
	break;

      ungetc (c, inf);
      lineno = read_rtx_lineno;
      desc = read_rtx (inf);
      process_rtx (desc, lineno);
    }
  fclose (inf);

  /* Process define_cond_exec patterns.  */
  if (define_cond_exec_queue != NULL)
    process_define_cond_exec ();

  return errors ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE;
}

/* Process an include file assuming that it lives in gcc/config/{target}/ 
   if the include looks line (include "file" )  */
static int
process_include (desc, lineno)
     rtx desc;
     int lineno;
{
  const char *filename = XSTR (desc, 0);
  char *pathname = NULL;
  FILE *input_file;
  char *fname = NULL;
  struct file_name_list *stackp;
  int flen;

  stackp = include;

  /* If specified file name is absolute, just open it.  */
  if (IS_ABSOLUTE_PATHNAME (filename) || !stackp)
    {
      if (base_dir)
        {
          pathname = xmalloc (strlen (base_dir) + strlen (filename) + 1);
          pathname = strcpy (pathname, base_dir);
          strcat (pathname, filename);
          strcat (pathname, "\0");
	}
      else
        {
	  pathname = xstrdup (filename);
        }
      read_rtx_filename = pathname;
      input_file = fopen (pathname, "r");

      if (input_file == 0)
	{
	  perror (pathname);
	  return FATAL_EXIT_CODE;
	}
    }
  else if (stackp)
    {

      flen = strlen (filename);

      fname = (char *) xmalloc (max_include_len + flen + 2);

      /* + 2 above for slash and terminating null.  */

      /* Search directory path, trying to open the file.
         Copy each filename tried into FNAME.  */

      for (; stackp; stackp = stackp->next)
	{
	  if (stackp->fname)
	    {
	      strcpy (fname, stackp->fname);
	      strcat (fname, "/");
	      fname[strlen (fname) + flen] = 0;
	    }
	  else
	    {
	      fname[0] = 0;
	    }
	  strncat (fname, (const char *) filename, flen);
	  read_rtx_filename = fname;
	  input_file = fopen (fname, "r");
	  if (input_file != NULL) 
	    break;
	}
      if (stackp == NULL)
	{
	  if (strchr (fname, '/') == NULL || strchr (fname, '\\' ) || base_dir)
	    {
	      if (base_dir)
		{
		  pathname =
		    xmalloc (strlen (base_dir) + strlen (filename) + 1);
		  pathname = strcpy (pathname, base_dir);
		  strcat (pathname, filename);
		  strcat (pathname, "\0");
		}
	      else
		pathname = xstrdup (filename);
	    }
	  read_rtx_filename = pathname;
	  input_file = fopen (pathname, "r");

	  if (input_file == 0)
	    {
	      perror (filename);
	      return FATAL_EXIT_CODE;
	    }
	}

    }

  if (init_include_reader (input_file) == FATAL_EXIT_CODE)
    message_with_line (lineno, "read errors found in include file  %s\n", pathname);

  if (fname)
    free (fname);
  return SUCCESS_EXIT_CODE;
}

/* Process a top level rtx in some way, queueing as appropriate.  */

static void
process_rtx (desc, lineno)
     rtx desc;
     int lineno;
{
  switch (GET_CODE (desc))
    {
    case DEFINE_INSN:
      queue_pattern (desc, &define_insn_tail, lineno);
      break;

    case DEFINE_COND_EXEC:
      queue_pattern (desc, &define_cond_exec_tail, lineno);
      break;

    case DEFINE_ATTR:
      queue_pattern (desc, &define_attr_tail, lineno);
      break;

    case INCLUDE:
      if (process_include (desc, lineno) == FATAL_EXIT_CODE)
	{
	  const char *filename = XSTR (desc, 0);
	  message_with_line (lineno, "include file at  %s not found\n",
			     filename);
	}
      break;

    case DEFINE_INSN_AND_SPLIT:
      {
	const char *split_cond;
	rtx split;
	rtvec attr;
	int i;

	/* Create a split with values from the insn_and_split.  */
	split = rtx_alloc (DEFINE_SPLIT);

	i = XVECLEN (desc, 1);
	XVEC (split, 0) = rtvec_alloc (i);
	while (--i >= 0)
	  {
	    XVECEXP (split, 0, i) = copy_rtx (XVECEXP (desc, 1, i));
	    remove_constraints (XVECEXP (split, 0, i));
	  }

	/* If the split condition starts with "&&", append it to the
	   insn condition to create the new split condition.  */
	split_cond = XSTR (desc, 4);
	if (split_cond[0] == '&' && split_cond[1] == '&')
	  {
	    const char *insn_cond = XSTR (desc, 2);
	    size_t insn_cond_len = strlen (insn_cond);
	    size_t split_cond_len = strlen (split_cond);
	    char *combined;

	    combined = (char *) xmalloc (insn_cond_len + split_cond_len + 1);
	    memcpy (combined, insn_cond, insn_cond_len);
	    memcpy (combined + insn_cond_len, split_cond, split_cond_len + 1);

	    split_cond = combined;
	  }
	XSTR (split, 1) = split_cond;
	XVEC (split, 2) = XVEC (desc, 5);
	XSTR (split, 3) = XSTR (desc, 6);

	/* Fix up the DEFINE_INSN.  */
	attr = XVEC (desc, 7);
	PUT_CODE (desc, DEFINE_INSN);
	XVEC (desc, 4) = attr;

	/* Queue them.  */
	queue_pattern (desc, &define_insn_tail, lineno);
	queue_pattern (split, &other_tail, lineno);
	break;
      }

    default:
      queue_pattern (desc, &other_tail, lineno);
      break;
    }
}

/* Return true if attribute PREDICABLE is true for ELEM, which holds
   a DEFINE_INSN.  */

static int
is_predicable (elem)
     struct queue_elem *elem;
{
  rtvec vec = XVEC (elem->data, 4);
  const char *value;
  int i;

  if (! vec)
    return predicable_default;

  for (i = GET_NUM_ELEM (vec) - 1; i >= 0; --i)
    {
      rtx sub = RTVEC_ELT (vec, i);
      switch (GET_CODE (sub))
	{
	case SET_ATTR:
	  if (strcmp (XSTR (sub, 0), "predicable") == 0)
	    {
	      value = XSTR (sub, 1);
	      goto found;
	    }
	  break;

	case SET_ATTR_ALTERNATIVE:
	  if (strcmp (XSTR (sub, 0), "predicable") == 0)
	    {
	      message_with_line (elem->lineno,
				 "multiple alternatives for `predicable'");
	      errors = 1;
	      return 0;
	    }
	  break;

	case SET:
	  if (GET_CODE (SET_DEST (sub)) != ATTR
	      || strcmp (XSTR (SET_DEST (sub), 0), "predicable") != 0)
	    break;
	  sub = SET_SRC (sub);
	  if (GET_CODE (sub) == CONST_STRING)
	    {
	      value = XSTR (sub, 0);
	      goto found;
	    }

	  /* ??? It would be possible to handle this if we really tried.
	     It's not easy though, and I'm not going to bother until it
	     really proves necessary.  */
	  message_with_line (elem->lineno,
			     "non-constant value for `predicable'");
	  errors = 1;
	  return 0;

	default:
	  abort ();
	}
    }

  return predicable_default;

 found:
  /* Verify that predicability does not vary on the alternative.  */
  /* ??? It should be possible to handle this by simply eliminating
     the non-predicable alternatives from the insn.  FRV would like
     to do this.  Delay this until we've got the basics solid.  */
  if (strchr (value, ',') != NULL)
    {
      message_with_line (elem->lineno,
			 "multiple alternatives for `predicable'");
      errors = 1;
      return 0;
    }

  /* Find out which value we're looking at.  */
  if (strcmp (value, predicable_true) == 0)
    return 1;
  if (strcmp (value, predicable_false) == 0)
    return 0;

  message_with_line (elem->lineno,
		     "unknown value `%s' for `predicable' attribute",
		     value);
  errors = 1;
  return 0;
}

/* Examine the attribute "predicable"; discover its boolean values
   and its default.  */

static void
identify_predicable_attribute ()
{
  struct queue_elem *elem;
  char *p_true, *p_false;
  const char *value;
  size_t len;

  /* Look for the DEFINE_ATTR for `predicable', which must exist.  */
  for (elem = define_attr_queue; elem ; elem = elem->next)
    if (strcmp (XSTR (elem->data, 0), "predicable") == 0)
      goto found;

  message_with_line (define_cond_exec_queue->lineno,
		     "attribute `predicable' not defined");
  errors = 1;
  return;

 found:
  value = XSTR (elem->data, 1);
  len = strlen (value);
  p_false = (char *) xmalloc (len + 1);
  memcpy (p_false, value, len + 1);

  p_true = strchr (p_false, ',');
  if (p_true == NULL || strchr (++p_true, ',') != NULL)
    {
      message_with_line (elem->lineno,
			 "attribute `predicable' is not a boolean");
      errors = 1;
      return;
    }
  p_true[-1] = '\0';

  predicable_true = p_true;
  predicable_false = p_false;

  switch (GET_CODE (XEXP (elem->data, 2)))
    {
    case CONST_STRING:
      value = XSTR (XEXP (elem->data, 2), 0);
      break;

    case CONST:
      message_with_line (elem->lineno,
			 "attribute `predicable' cannot be const");
      errors = 1;
      return;

    default:
      message_with_line (elem->lineno,
			 "attribute `predicable' must have a constant default");
      errors = 1;
      return;
    }

  if (strcmp (value, p_true) == 0)
    predicable_default = 1;
  else if (strcmp (value, p_false) == 0)
    predicable_default = 0;
  else
    {
      message_with_line (elem->lineno,
			 "unknown value `%s' for `predicable' attribute",
			 value);
      errors = 1;
    }
}

/* Return the number of alternatives in constraint S.  */

static int
n_alternatives (s)
     const char *s;
{
  int n = 1;

  if (s)
    while (*s)
      n += (*s++ == ',');

  return n;
}

/* Determine how many alternatives there are in INSN, and how many
   operands.  */

static void
collect_insn_data (pattern, palt, pmax)
     rtx pattern;
     int *palt, *pmax;
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_OPERAND:
      i = n_alternatives (XSTR (pattern, 2));
      *palt = (i > *palt ? i : *palt);
      /* FALLTHRU */

    case MATCH_OPERATOR:
    case MATCH_SCRATCH:
    case MATCH_PARALLEL:
    case MATCH_INSN:
      i = XINT (pattern, 0);
      if (i > *pmax)
	*pmax = i;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  collect_insn_data (XEXP (pattern, i), palt, pmax);
	  break;

	case 'V':
	  if (XVEC (pattern, i) == NULL)
	    break;
	  /* FALLTHRU */
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    collect_insn_data (XVECEXP (pattern, i, j), palt, pmax);
	  break;

	case 'i': case 'w': case '0': case 's': case 'S': case 'T':
	  break;

	default:
	  abort ();
	}
    }
}

static rtx
alter_predicate_for_insn (pattern, alt, max_op, lineno)
     rtx pattern;
     int alt, max_op, lineno;
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_OPERAND:
      {
	const char *c = XSTR (pattern, 2);

	if (n_alternatives (c) != 1)
	  {
	    message_with_line (lineno,
			       "too many alternatives for operand %d",
			       XINT (pattern, 0));
	    errors = 1;
	    return NULL;
	  }

	/* Replicate C as needed to fill out ALT alternatives.  */
	if (c && *c && alt > 1)
	  {
	    size_t c_len = strlen (c);
	    size_t len = alt * (c_len + 1);
	    char *new_c = (char *) xmalloc (len);

	    memcpy (new_c, c, c_len);
	    for (i = 1; i < alt; ++i)
	      {
		new_c[i * (c_len + 1) - 1] = ',';
		memcpy (&new_c[i * (c_len + 1)], c, c_len);
	      }
	    new_c[len - 1] = '\0';
	    XSTR (pattern, 2) = new_c;
	  }
      }
      /* FALLTHRU */

    case MATCH_OPERATOR:
    case MATCH_SCRATCH:
    case MATCH_PARALLEL:
    case MATCH_INSN:
      XINT (pattern, 0) += max_op;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      rtx r;

      switch (fmt[i])
	{
	case 'e': case 'u':
	  r = alter_predicate_for_insn (XEXP (pattern, i), alt,
					max_op, lineno);
	  if (r == NULL)
	    return r;
	  break;

	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    {
	      r = alter_predicate_for_insn (XVECEXP (pattern, i, j),
					    alt, max_op, lineno);
	      if (r == NULL)
		return r;
	    }
	  break;

	case 'i': case 'w': case '0': case 's':
	  break;

	default:
	  abort ();
	}
    }

  return pattern;
}

static const char *
alter_test_for_insn (ce_elem, insn_elem)
     struct queue_elem *ce_elem, *insn_elem;
{
  const char *ce_test, *insn_test;
  char *new_test;
  size_t len, ce_len, insn_len;

  ce_test = XSTR (ce_elem->data, 1);
  insn_test = XSTR (insn_elem->data, 2);
  if (!ce_test || *ce_test == '\0')
    return insn_test;
  if (!insn_test || *insn_test == '\0')
    return ce_test;

  ce_len = strlen (ce_test);
  insn_len = strlen (insn_test);
  len = 1 + ce_len + 1 + 4 + 1 + insn_len + 1 + 1;
  new_test = (char *) xmalloc (len);

  sprintf (new_test, "(%s) && (%s)", ce_test, insn_test);

  return new_test;
}

/* Adjust all of the operand numbers in OLD to match the shift they'll
   get from an operand displacement of DISP.  Return a pointer after the
   adjusted string.  */

static char *
shift_output_template (new, old, disp)
     char *new;
     const char *old;
     int disp;
{
  while (*old)
    {
      char c = *old++;
      *new++ = c;
      if (c == '%')
	{
	  c = *old++;
	  if (ISDIGIT ((unsigned char) c))
	    c += disp;
	  else if (ISALPHA (c))
	    {
	      *new++ = c;
	      c = *old++ + disp;
	    }
	  *new++ = c;
	}
    }

  return new;
}

static const char *
alter_output_for_insn (ce_elem, insn_elem, alt, max_op)
     struct queue_elem *ce_elem, *insn_elem;
     int alt, max_op;
{
  const char *ce_out, *insn_out;
  char *new, *p;
  size_t len, ce_len, insn_len;

  /* ??? Could coordinate with genoutput to not duplicate code here.  */

  ce_out = XSTR (ce_elem->data, 2);
  insn_out = XTMPL (insn_elem->data, 3);
  if (!ce_out || *ce_out == '\0')
    return insn_out;

  ce_len = strlen (ce_out);
  insn_len = strlen (insn_out);

  if (*insn_out == '*')
    /* You must take care of the predicate yourself.  */
    return insn_out;

  if (*insn_out == '@')
    {
      len = (ce_len + 1) * alt + insn_len + 1;
      p = new = xmalloc (len);

      do
	{
	  do
	    *p++ = *insn_out++;
	  while (ISSPACE ((unsigned char) *insn_out));

	  if (*insn_out != '#')
	    {
	      p = shift_output_template (p, ce_out, max_op);
	      *p++ = ' ';
	    }

	  do
	    *p++ = *insn_out++;
	  while (*insn_out && *insn_out != '\n');
	}
      while (*insn_out);
      *p = '\0';
    }
  else
    {
      len = ce_len + 1 + insn_len + 1;
      new = xmalloc (len);

      p = shift_output_template (new, ce_out, max_op);
      *p++ = ' ';
      memcpy (p, insn_out, insn_len + 1);
    }

  return new;
}

/* Replicate insns as appropriate for the given DEFINE_COND_EXEC.  */

static void
process_one_cond_exec (ce_elem)
     struct queue_elem *ce_elem;
{
  struct queue_elem *insn_elem;
  for (insn_elem = define_insn_queue; insn_elem ; insn_elem = insn_elem->next)
    {
      int alternatives, max_operand;
      rtx pred, insn, pattern;

      if (! is_predicable (insn_elem))
	continue;

      alternatives = 1;
      max_operand = -1;
      collect_insn_data (insn_elem->data, &alternatives, &max_operand);
      max_operand += 1;

      if (XVECLEN (ce_elem->data, 0) != 1)
	{
	  message_with_line (ce_elem->lineno,
			     "too many patterns in predicate");
	  errors = 1;
	  return;
	}

      pred = copy_rtx (XVECEXP (ce_elem->data, 0, 0));
      pred = alter_predicate_for_insn (pred, alternatives, max_operand,
				       ce_elem->lineno);
      if (pred == NULL)
	return;

      /* Construct a new pattern for the new insn.  */
      insn = copy_rtx (insn_elem->data);
      XSTR (insn, 0) = "";
      pattern = rtx_alloc (COND_EXEC);
      XEXP (pattern, 0) = pred;
      if (XVECLEN (insn, 1) == 1)
	{
	  XEXP (pattern, 1) = XVECEXP (insn, 1, 0);
	  XVECEXP (insn, 1, 0) = pattern;
	  PUT_NUM_ELEM (XVEC (insn, 1), 1);
	}
      else
	{
	  XEXP (pattern, 1) = rtx_alloc (PARALLEL);
	  XVEC (XEXP (pattern, 1), 0) = XVEC (insn, 1);
	  XVEC (insn, 1) = rtvec_alloc (1);
	  XVECEXP (insn, 1, 0) = pattern;
	}

      XSTR (insn, 2) = alter_test_for_insn (ce_elem, insn_elem);
      XTMPL (insn, 3) = alter_output_for_insn (ce_elem, insn_elem,
					      alternatives, max_operand);

      /* ??? Set `predicable' to false.  Not crucial since it's really
         only used here, and we won't reprocess this new pattern.  */

      /* Put the new pattern on the `other' list so that it
	 (a) is not reprocessed by other define_cond_exec patterns
	 (b) appears after all normal define_insn patterns.

	 ??? B is debatable.  If one has normal insns that match
	 cond_exec patterns, they will be preferred over these
	 generated patterns.  Whether this matters in practice, or if
	 it's a good thing, or whether we should thread these new
	 patterns into the define_insn chain just after their generator
	 is something we'll have to experiment with.  */

      queue_pattern (insn, &other_tail, insn_elem->lineno);
    }
}

/* If we have any DEFINE_COND_EXEC patterns, expand the DEFINE_INSN
   patterns appropriately.  */

static void
process_define_cond_exec ()
{
  struct queue_elem *elem;

  identify_predicable_attribute ();
  if (errors)
    return;

  for (elem = define_cond_exec_queue; elem ; elem = elem->next)
    process_one_cond_exec (elem);
}

static char *
save_string (s, len)
     const char *s;
     int len;
{
  register char *result = xmalloc (len + 1);

  memcpy (result, s, len);
  result[len] = 0;
  return result;
}


/* The entry point for initializing the reader.  */

int
init_md_reader_args (argc, argv)
     int argc;
     char **argv;
{
  int i;
  const char *in_fname;

  max_include_len = 0;
  in_fname = NULL;
  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] != '-')
	{
	  if (in_fname == NULL)
	    in_fname = argv[i];
	}
      else
	{
	  int c = argv[i][1];
	  switch (c)
	    {
	    case 'I':		/* Add directory to path for includes.  */
	      {
		struct file_name_list *dirtmp;

		dirtmp = (struct file_name_list *)
		  xmalloc (sizeof (struct file_name_list));
		dirtmp->next = 0;	/* New one goes on the end */
		if (include == 0)
		  include = dirtmp;
		else
		  last_include->next = dirtmp;
		last_include = dirtmp;	/* Tail follows the last one */
		if (argv[i][1] == 'I' && argv[i][2] != 0)
		  dirtmp->fname = argv[i] + 2;
		else if (i + 1 == argc)
		  fatal ("directory name missing after -I option");
		else
		  dirtmp->fname = argv[++i];
		if (strlen (dirtmp->fname) > max_include_len)
		  max_include_len = strlen (dirtmp->fname);
	      }
	      break;
	    default:
	      fatal ("invalid option `%s'", argv[i]);

	    }
	}
    }
    return init_md_reader (in_fname);
}

/* The entry point for initializing the reader.  */

int
init_md_reader (filename)
     const char *filename;
{
  FILE *input_file;
  int c;
  char *lastsl;

  if (!IS_ABSOLUTE_PATHNAME (filename))
    {
      lastsl = strrchr (filename, '/');
      if (lastsl != NULL) 
	base_dir = save_string (filename, lastsl - filename + 1 );
    }

  read_rtx_filename = filename;
  input_file = fopen (filename, "r");
  if (input_file == 0)
    {
      perror (filename);
      return FATAL_EXIT_CODE;
    }

  obstack_init (rtl_obstack);
  errors = 0;
  sequence_num = 0;

  /* Read the entire file.  */
  while (1)
    {
      rtx desc;
      int lineno;

      c = read_skip_spaces (input_file);
      if (c == EOF)
        break;

      ungetc (c, input_file);
      lineno = read_rtx_lineno;
      desc = read_rtx (input_file);
      process_rtx (desc, lineno);
    }
  fclose (input_file);

  /* Process define_cond_exec patterns.  */
  if (define_cond_exec_queue != NULL)
    process_define_cond_exec ();

  return errors ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE;
}

/* The entry point for reading a single rtx from an md file.  */

rtx
read_md_rtx (lineno, seqnr)
     int *lineno;
     int *seqnr;
{
  struct queue_elem **queue, *elem;
  rtx desc;

  /* Read all patterns from a given queue before moving on to the next.  */
  if (define_attr_queue != NULL)
    queue = &define_attr_queue;
  else if (define_insn_queue != NULL)
    queue = &define_insn_queue;
  else if (other_queue != NULL)
    queue = &other_queue;
  else
    return NULL_RTX;

  elem = *queue;
  *queue = elem->next;
  desc = elem->data;
  *lineno = elem->lineno;
  *seqnr = sequence_num;

  free (elem);

  switch (GET_CODE (desc))
    {
    case DEFINE_INSN:
    case DEFINE_EXPAND:
    case DEFINE_SPLIT:
    case DEFINE_PEEPHOLE:
    case DEFINE_PEEPHOLE2:
      sequence_num++;
      break;

    default:
      break;
    }

  return desc;
}
