/* CPP Library.
   Copyright (C) 1986, 87, 89, 92-98, 1999 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

#include "cpplib.h"
#include "cpphash.h"
#include "intl.h"

#define SKIP_WHITE_SPACE(p) do { while (is_hor_space[*p]) p++; } while (0)
#define SKIP_ALL_WHITE_SPACE(p) do { while (is_space[*p]) p++; } while (0)

#define PEEKN(N) (CPP_BUFFER (pfile)->rlimit - CPP_BUFFER (pfile)->cur >= (N) ? CPP_BUFFER (pfile)->cur[N] : EOF)
#define FORWARD(N) CPP_FORWARD (CPP_BUFFER (pfile), (N))
#define GETC() CPP_BUF_GET (CPP_BUFFER (pfile))
#define PEEKC() CPP_BUF_PEEK (CPP_BUFFER (pfile))
/* CPP_IS_MACRO_BUFFER is true if the buffer contains macro expansion.
   (Note that it is false while we're expanding macro *arguments*.) */
#define CPP_IS_MACRO_BUFFER(PBUF) ((PBUF)->data != NULL)

/* Forward declarations.  */

static char *my_strerror		PROTO ((int));
static void validate_else		PROTO ((cpp_reader *, char *));
static HOST_WIDEST_INT eval_if_expression	PROTO ((cpp_reader *));

static void conditional_skip		PROTO ((cpp_reader *, int,
						enum node_type, U_CHAR *));
static void skip_if_group		PROTO ((cpp_reader *));

static void parse_name			PARAMS ((cpp_reader *, int));
static void parse_string		PARAMS ((cpp_reader *, int));
static int parse_assertion		PARAMS ((cpp_reader *));

/* External declarations.  */

extern HOST_WIDEST_INT cpp_parse_expr PARAMS ((cpp_reader *));

/* `struct directive' defines one #-directive, including how to handle it.  */

struct directive {
  int length;			/* Length of name */
  int (*func)			/* Function to handle directive */
    PARAMS ((cpp_reader *, struct directive *));
  char *name;			/* Name of directive */
  enum node_type type;		/* Code which describes which directive.  */
};

/* These functions are declared to return int instead of void since they
   are going to be placed in a table and some old compilers have trouble with
   pointers to functions returning void.  */

static int do_define PARAMS ((cpp_reader *, struct directive *));
static int do_line PARAMS ((cpp_reader *, struct directive *));
static int do_include PARAMS ((cpp_reader *, struct directive *));
static int do_undef PARAMS ((cpp_reader *, struct directive *));
static int do_error PARAMS ((cpp_reader *, struct directive *));
static int do_pragma PARAMS ((cpp_reader *, struct directive *));
static int do_ident PARAMS ((cpp_reader *, struct directive *));
static int do_if PARAMS ((cpp_reader *, struct directive *));
static int do_xifdef PARAMS ((cpp_reader *, struct directive *));
static int do_else PARAMS ((cpp_reader *, struct directive *));
static int do_elif PARAMS ((cpp_reader *, struct directive *));
static int do_endif PARAMS ((cpp_reader *, struct directive *));
#ifdef SCCS_DIRECTIVE
static int do_sccs PARAMS ((cpp_reader *, struct directive *));
#endif
static int do_assert PARAMS ((cpp_reader *, struct directive *));
static int do_unassert PARAMS ((cpp_reader *, struct directive *));
static int do_warning PARAMS ((cpp_reader *, struct directive *));

#define IS_INCLUDE_DIRECTIVE_TYPE(t) \
((int) T_INCLUDE <= (int) (t) && (int) (t) <= (int) T_IMPORT)

/* Here is the actual list of #-directives, most-often-used first.
   The initialize_builtins function assumes #define is the very first.  */

static struct directive directive_table[] = {
  {  6, do_define,   "define",       T_DEFINE },
  {  5, do_xifdef,   "ifdef",        T_IFDEF },
  {  6, do_xifdef,   "ifndef",       T_IFNDEF },
  {  7, do_include,  "include",      T_INCLUDE },
  { 12, do_include,  "include_next", T_INCLUDE_NEXT },
  {  6, do_include,  "import",       T_IMPORT },
  {  5, do_endif,    "endif",        T_ENDIF },
  {  4, do_else,     "else",         T_ELSE },
  {  2, do_if,       "if",           T_IF },
  {  4, do_elif,     "elif",         T_ELIF },
  {  5, do_undef,    "undef",        T_UNDEF },
  {  5, do_error,    "error",        T_ERROR },
  {  7, do_warning,  "warning",      T_WARNING },
  {  6, do_pragma,   "pragma",       T_PRAGMA },
  {  4, do_line,     "line",         T_LINE },
  {  5, do_ident,    "ident",        T_IDENT },
#ifdef SCCS_DIRECTIVE
  {  4, do_sccs,     "sccs",         T_SCCS },
#endif
  {  6, do_assert,   "assert",       T_ASSERT },
  {  8, do_unassert, "unassert",     T_UNASSERT },
  {  -1, 0, "", T_UNUSED }
};

/* Place into PFILE a quoted string representing the string SRC.
   Caller must reserve enough space in pfile->token_buffer.  */

void
quote_string (pfile, src)
     cpp_reader *pfile;
     const char *src;
{
  U_CHAR c;

  CPP_PUTC_Q (pfile, '\"');
  for (;;)
    switch ((c = *src++))
      {
      default:
        if (ISPRINT (c))
	  CPP_PUTC_Q (pfile, c);
	else
	  {
	    sprintf ((char *)CPP_PWRITTEN (pfile), "\\%03o", c);
	    CPP_ADJUST_WRITTEN (pfile, 4);
	  }
	break;

      case '\"':
      case '\\':
	CPP_PUTC_Q (pfile, '\\');
	CPP_PUTC_Q (pfile, c);
	break;
      
      case '\0':
	CPP_PUTC_Q (pfile, '\"');
	CPP_NUL_TERMINATE_Q (pfile);
	return;
      }
}

/* Re-allocates PFILE->token_buffer so it will hold at least N more chars.  */

void
cpp_grow_buffer (pfile, n)
     cpp_reader *pfile;
     long n;
{
  long old_written = CPP_WRITTEN (pfile);
  pfile->token_buffer_size = n + 2 * pfile->token_buffer_size;
  pfile->token_buffer = (U_CHAR *)
    xrealloc(pfile->token_buffer, pfile->token_buffer_size);
  CPP_SET_WRITTEN (pfile, old_written);
}

/* Process the string STR as if it appeared as the body of a #define
   If STR is just an identifier, define it with value 1.
   If STR has anything after the identifier, then it should
   be identifier=definition. */

void
cpp_define (pfile, str)
     cpp_reader *pfile;
     U_CHAR *str;
{
  U_CHAR *buf, *p;
  size_t count;

  /* Copy the entire option so we can modify it.  */
  count = strlen (str) + 3;
  buf = (U_CHAR *) alloca (count);
  memcpy (buf, str, count - 2);
  /* Change the first "=" in the string to a space.  If there is none,
     tack " 1" on the end. */
  p = (U_CHAR *) strchr (buf, '=');
  if (p)
    {
      *p = ' ';
      count -= 2;
    }
  else
      strcpy (&buf[count-3], " 1");
  
  if (cpp_push_buffer (pfile, buf, count - 1) != NULL)
    {
      do_define (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}

/* Process the string STR as if it appeared as the body of a #assert. */
void
cpp_assert (pfile, str)
     cpp_reader *pfile;
     U_CHAR *str;
{
  if (cpp_push_buffer (pfile, str, strlen (str)) != NULL)
    {
      do_assert (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}


static enum cpp_token
null_underflow (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  return CPP_EOF;
}

static int
null_cleanup (pbuf, pfile)
     cpp_buffer *pbuf ATTRIBUTE_UNUSED;
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Skip a comment - C, C++, or Chill style.  M is the first character
   of the comment marker.  If this really is a comment, skip to its
   end and return ' '.  If we hit end-of-file before end-of-comment,
   return EOF.  If this is not a comment, return M (which will be
   '/' or '-').  */

static int
skip_comment (pfile, m)
     cpp_reader *pfile;
     int m;
{
  if (m == '/' && PEEKC() == '*')
    {
      int c, prev_c = -1;
      long line, col;
      
      FORWARD(1);
      cpp_buf_line_and_col (CPP_BUFFER (pfile), &line, &col);
      for (;;)
	{
	  c = GETC ();
	  if (c == EOF)
	    {
	      cpp_error_with_line (pfile, line, col, "unterminated comment");
	      return EOF;
	    }
	  else if (c == '\n' || c == '\r')
	    /* \r cannot be a macro escape marker here. */
	    CPP_BUMP_LINE (pfile);
	  else if (c == '/' && prev_c == '*')
	    return ' ';
	  else if (c == '*' && prev_c == '/'
		   && CPP_OPTIONS (pfile)->warn_comments)
	    cpp_warning (pfile, "`/*' within comment");

	  prev_c = c;
	}
    }
  else if ((m == '/' && PEEKC() == '/'
	    && CPP_OPTIONS (pfile)->cplusplus_comments)
	   || (m == '-' && PEEKC() == '-'
	       && CPP_OPTIONS (pfile)->chill))
    {
      FORWARD(1);
      for (;;)
	{
	  int c = GETC ();
	  if (c == EOF)
	    return ' '; /* Allow // to be terminated by EOF.  */
	      if (c == '\n')
		{
		  /* Don't consider final '\n' to be part of comment.  */
		  FORWARD(-1);
		  return ' ';
		}
	      else if (c == '\r')
		/* \r cannot be a macro escape marker here. */
		CPP_BUMP_LINE (pfile);
	}
    }
  else
    return m;
}

/* Identical to skip_comment except that it copies the comment into the
   token_buffer.  This is used if put_out_comments.  */
static int
copy_comment (pfile, m)
     cpp_reader *pfile;
     int m;
{
  if (m == '/' && PEEKC() == '*')
    {
      int c, prev_c = -1;
      long line, col;

      CPP_PUTC (pfile, '/');
      CPP_PUTC (pfile, '*');
      FORWARD(1);
      cpp_buf_line_and_col (CPP_BUFFER (pfile), &line, &col);
      for (;;)
	{
	  c = GETC ();
	  if (c == EOF)
	    {
	      cpp_error_with_line (pfile, line, col, "unterminated comment");
	      /* We must pretend this was a legitimate comment, so that the
		 output in token_buffer is not passed back tagged CPP_POP. */
	      return ' ';
	    }
	  else if (c == '\r')
	    {
	      /* \r cannot be a macro escape marker here. */
	      CPP_BUMP_LINE (pfile);
	      continue;
	    }

	  CPP_PUTC (pfile, c);
	  if (c == '\n')
	    {
	      pfile->lineno++;
	      CPP_BUMP_LINE (pfile);
	    }
	  else if (c == '/' && prev_c == '*')
	    return ' ';
	  else if (c == '*' && prev_c == '/'
		   && CPP_OPTIONS (pfile)->warn_comments)
	    cpp_warning (pfile, "`/*' within comment");

	  prev_c = c;
	}
    }
  else if ((m == '/' && PEEKC() == '/'
	    && CPP_OPTIONS (pfile)->cplusplus_comments)
	   || (m == '-' && PEEKC() == '-'
	       && CPP_OPTIONS (pfile)->chill))
    {
      CPP_PUTC (pfile, m);
      CPP_PUTC (pfile, m);
      FORWARD(1);
      for (;;)
	{
	  int c = GETC ();
	  if (c == EOF)
	    return ' '; /* Allow line comments to be terminated by EOF. */
	  else if (c == '\n')
	    {
	      /* Don't consider final '\n' to be part of comment.  */
	      FORWARD(-1);
	      return ' ';
	    }
	  else if (c == '\r')
	    /* \r cannot be a macro escape marker here. */
	    CPP_BUMP_LINE (pfile);

	  CPP_PUTC (pfile, c);
	}
    }
  else
    return m;
}


/* Skip whitespace \-newline and comments.  Does not macro-expand.  */

void
cpp_skip_hspace (pfile)
     cpp_reader *pfile;
{
  int c;
  while (1)
    {
      c = GETC();
      if (c == EOF)
	return;
      else if (is_hor_space[c])
	{
	  if ((c == '\f' || c == '\v') && CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "%s in preprocessing directive",
			 c == '\f' ? "formfeed" : "vertical tab");
	}
      else if (c == '\r')
	{
	  /* \r is a backslash-newline marker if !has_escapes, and
	     a deletable-whitespace or no-reexpansion marker otherwise. */
	  if (CPP_BUFFER (pfile)->has_escapes)
	    {
	      if (PEEKC() == ' ')
		FORWARD(1);
	      else
		break;
	    }
	  else
	    CPP_BUFFER (pfile)->lineno++;
	}
      else if (c == '/' || c == '-')
	{
	  c = skip_comment (pfile, c);
	  if (c == EOF)
	    return;
	  else if (c != ' ')
	    break;
	}
      else
	break;
    }
  FORWARD(-1);
}

/* Read the rest of the current line.
   The line is appended to PFILE's output buffer.  */

static void
copy_rest_of_line (pfile)
     cpp_reader *pfile;
{
  for (;;)
    {
      int c = GETC();
      switch (c)
	{
	case '\n':
	  FORWARD(-1);
	case EOF:
	  CPP_NUL_TERMINATE (pfile);
	  return;

	case '\r':
	  if (CPP_BUFFER (pfile)->has_escapes)
	    break;
	  else
	    {
	      CPP_BUFFER (pfile)->lineno++;
	      continue;
	    }
	case '\'':
	case '\"':
	  parse_string (pfile, c);
	  continue;
	case '/':
	  if (PEEKC() == '*' && CPP_TRADITIONAL (pfile))
	    {
	      CPP_PUTS (pfile, "/**/", 4);
	      skip_comment (pfile, c);
	      continue;
	    }
	  /* else fall through */
	case '-':
	  c = skip_comment (pfile, c);
	  break;

	case '\f':
	case '\v':
	  if (CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "%s in preprocessing directive",
			 c == '\f' ? "formfeed" : "vertical tab");
	  break;

	}
      CPP_PUTC (pfile, c);
    }
}

/* FIXME: It is almost definitely a performance win to make this do
   the scan itself.  >75% of calls to copy_r_o_l are from here or
   skip_if_group, which means the common case is to copy stuff into the
   token_buffer only to discard it.  */
void
skip_rest_of_line (pfile)
     cpp_reader *pfile;
{
  long old = CPP_WRITTEN (pfile);
  copy_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old);
}

/* Handle a possible # directive.
   '#' has already been read.  */

static int
handle_directive (pfile)
     cpp_reader *pfile;
{
  int c;
  register struct directive *kt;
  int ident_length;
  U_CHAR *ident;
  long old_written = CPP_WRITTEN (pfile);

  cpp_skip_hspace (pfile);

  c = PEEKC ();
  if (c >= '0' && c <= '9')
    {
      /* Handle # followed by a line number.  */
      if (CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "`#' followed by integer");
      do_line (pfile, NULL);
      return 1;
    }

  /* Now find the directive name.  */
  CPP_PUTC (pfile, '#');
  parse_name (pfile, GETC());
  ident = pfile->token_buffer + old_written + 1;
  ident_length = CPP_PWRITTEN (pfile) - ident;
  if (ident_length == 0)
    {
      /* A line of just `#' becomes blank.  */
      if (PEEKC() == '\n')
	return 1;
      else
	return 0;
    }

  /*
   * Decode the keyword and call the appropriate expansion
   * routine, after moving the input pointer up to the next line.
   */
  for (kt = directive_table; ; kt++)
    {
      if (kt->length <= 0)
	return 0;
      if (kt->length == ident_length
	  && !strncmp (kt->name, ident, ident_length)) 
	break;
    }

  CPP_SET_WRITTEN (pfile, old_written);
  (*kt->func) (pfile, kt);

  return 1;
}

/* Pass a directive through to the output file.
   BUF points to the contents of the directive, as a contiguous string.
   LEN is the length of the string pointed to by BUF.
   KEYWORD is the keyword-table entry for the directive.  */

static void
pass_thru_directive (buf, len, pfile, keyword)
     U_CHAR *buf;
     size_t len;
     cpp_reader *pfile;
     struct directive *keyword;
{
  register unsigned keyword_length = keyword->length;

  CPP_RESERVE (pfile, 1 + keyword_length + len);
  CPP_PUTC_Q (pfile, '#');
  CPP_PUTS_Q (pfile, keyword->name, keyword_length);
  if (len != 0 && buf[0] != ' ')
    CPP_PUTC_Q (pfile, ' ');
  CPP_PUTS_Q (pfile, buf, len);
}

/* Check a purported macro name SYMNAME, and yield its length.
   ASSERTION is nonzero if this is really for an assertion name.  */

int
check_macro_name (pfile, symname, assertion)
     cpp_reader *pfile;
     U_CHAR *symname;
     int assertion;
{
  U_CHAR *p;
  int sym_length;

  for (p = symname; is_idchar[*p]; p++)
    ;
  sym_length = p - symname;
  if (sym_length == 0
      || (sym_length == 1 && *symname == 'L' && (*p == '\'' || *p == '"')))
    cpp_error (pfile,
	       assertion ? "invalid assertion name" : "invalid macro name");
  else if (!is_idstart[*symname]
	   || (! strncmp (symname, "defined", 7) && sym_length == 7)) {
    U_CHAR *msg;			/* what pain...  */
    msg = (U_CHAR *) alloca (sym_length + 1);
    bcopy (symname, msg, sym_length);
    msg[sym_length] = 0;
    cpp_error (pfile,
	       (assertion
		? "invalid assertion name `%s'"
		: "invalid macro name `%s'"),
	       msg);
  }
  return sym_length;
}

/* Process a #define command.
KEYWORD is the keyword-table entry for #define,
or NULL for a "predefined" macro.  */

static int
do_define (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword;
{
  int hashcode;
  MACRODEF mdef;
  HASHNODE *hp;
  long here;
  U_CHAR *macro, *buf, *end;

  here = CPP_WRITTEN (pfile);
  copy_rest_of_line (pfile);

  /* Copy out the line so we can pop the token buffer. */
  buf = pfile->token_buffer + here;
  end = CPP_PWRITTEN (pfile);
  macro = alloca (end - buf + 1);
  bcopy (buf, macro, end - buf + 1);
  end = macro + (end - buf);

  CPP_SET_WRITTEN (pfile, here);

  mdef = create_definition (macro, end, pfile, keyword == NULL);
  if (mdef.defn == 0)
    return 0;

  hashcode = hashf (mdef.symnam, mdef.symlen, HASHSIZE);

  if ((hp = cpp_lookup (pfile, mdef.symnam, mdef.symlen, hashcode)) != NULL)
    {
      int ok = 0;
      /* Redefining a precompiled key is ok.  */
      if (hp->type == T_PCSTRING)
	ok = 1;
      /* Redefining a macro is ok if the definitions are the same.  */
      else if (hp->type == T_MACRO)
	ok = ! compare_defs (pfile, mdef.defn, hp->value.defn);
      /* Redefining a constant is ok with -D.  */
      else if (hp->type == T_CONST || hp->type == T_STDC)
        ok = ! CPP_OPTIONS (pfile)->done_initializing;
      /* Print the warning if it's not ok.  */
      if (!ok)
	{
	  cpp_pedwarn (pfile, "`%.*s' redefined", mdef.symlen, mdef.symnam);
	  if (hp->type == T_MACRO)
	    cpp_pedwarn_with_file_and_line (pfile, hp->value.defn->file,
					    hp->value.defn->line,
			"this is the location of the previous definition");
	}
      /* Replace the old definition.  */
      hp->type = T_MACRO;
      hp->value.defn = mdef.defn;
    }
  else
    cpp_install (pfile, mdef.symnam, mdef.symlen, T_MACRO,
		 (char *) mdef.defn, hashcode);

  if (keyword)
    {
      if (CPP_OPTIONS (pfile)->debug_output
	  || CPP_OPTIONS (pfile)->dump_macros == dump_definitions)
	dump_definition (pfile, mdef);
      else if (CPP_OPTIONS (pfile)->dump_macros == dump_names)
	pass_thru_directive (mdef.symnam, mdef.symlen, pfile, keyword);
    }

  return 0;
}


/* Allocate a new cpp_buffer for PFILE, and push it on the input buffer stack.
   If BUFFER != NULL, then use the LENGTH characters in BUFFER
   as the new input buffer.
   Return the new buffer, or NULL on failure.  */

cpp_buffer *
cpp_push_buffer (pfile, buffer, length)
     cpp_reader *pfile;
     U_CHAR *buffer;
     long length;
{
  cpp_buffer *buf = CPP_BUFFER (pfile);
  cpp_buffer *new;
  if (++pfile->buffer_stack_depth == CPP_STACK_MAX)
    {
      cpp_fatal (pfile, "macro or `#include' recursion too deep");
      return NULL;
    }

  new = (cpp_buffer *) xcalloc (sizeof (cpp_buffer), 1);

  new->if_stack = pfile->if_stack;
  new->cleanup = null_cleanup;
  new->underflow = null_underflow;
  new->buf = new->cur = buffer;
  new->alimit = new->rlimit = buffer + length;
  new->prev = buf;
  new->mark = -1;

  CPP_BUFFER (pfile) = new;
  return new;
}

cpp_buffer *
cpp_pop_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buf = CPP_BUFFER (pfile);
  (*buf->cleanup) (buf, pfile);
  CPP_BUFFER (pfile) = CPP_PREV_BUFFER (buf);
  free (buf);
  pfile->buffer_stack_depth--;
  return CPP_BUFFER (pfile);
}

/* Scan until CPP_BUFFER (PFILE) is exhausted into PFILE->token_buffer.
   Pop the buffer when done.  */

void
cpp_scan_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = CPP_BUFFER (pfile);
  for (;;)
    {
      enum cpp_token token = cpp_get_token (pfile);
      if (token == CPP_EOF) /* Should not happen ...  */
	break;
      if (token == CPP_POP && CPP_BUFFER (pfile) == buffer)
	{
	  cpp_pop_buffer (pfile);
	  break;
	}
    }
}

/*
 * Rescan a string (which may have escape marks) into pfile's buffer.
 * Place the result in pfile->token_buffer.
 *
 * The input is copied before it is scanned, so it is safe to pass
 * it something from the token_buffer that will get overwritten
 * (because it follows CPP_WRITTEN).  This is used by do_include.
 */

void
cpp_expand_to_buffer (pfile, buf, length)
     cpp_reader *pfile;
     U_CHAR *buf;
     int length;
{
  register cpp_buffer *ip;
#if 0
  cpp_buffer obuf;
#endif
  U_CHAR *buf1;
#if 0
  int odepth = indepth;
#endif

  if (length < 0)
    {
      cpp_fatal (pfile, "internal error: length < 0 in cpp_expand_to_buffer");
      return;
    }

  /* Set up the input on the input stack.  */

  buf1 = (U_CHAR *) alloca (length + 1);
  memcpy (buf1, buf, length);
  buf1[length] = 0;

  ip = cpp_push_buffer (pfile, buf1, length);
  if (ip == NULL)
    return;
  ip->has_escapes = 1;
#if 0
  ip->lineno = obuf.lineno = 1;
#endif

  /* Scan the input, create the output.  */
  cpp_scan_buffer (pfile);

  CPP_NUL_TERMINATE (pfile);
}

void
cpp_buf_line_and_col (pbuf, linep, colp)
     register cpp_buffer *pbuf;
     long *linep, *colp;
{
  if (pbuf)
    {
      *linep = pbuf->lineno;
      if (colp)
	*colp = pbuf->cur - pbuf->line_base;
    }
  else
    {
      *linep = 0;
      if (colp)
	*colp = 0;
    }
}

/* Return the cpp_buffer that corresponds to a file (not a macro).  */

cpp_buffer *
cpp_file_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  for ( ; ip != CPP_NULL_BUFFER (pfile); ip = CPP_PREV_BUFFER (ip))
    if (ip->fname != NULL)
      return ip;
  return NULL;
}

/*
 * write out a #line command, for instance, after an #include file.
 * FILE_CHANGE says whether we are entering a file, leaving, or neither.
 */

void
output_line_command (pfile, file_change)
     cpp_reader *pfile;
     enum file_change_code file_change;
{
  long line;
  cpp_buffer *ip = CPP_BUFFER (pfile);

  if (ip->fname == NULL)
    return;

  if (CPP_OPTIONS (pfile)->no_line_commands
      || CPP_OPTIONS (pfile)->no_output)
    return;

  cpp_buf_line_and_col (CPP_BUFFER (pfile), &line, NULL);

  /* If the current file has not changed, we omit the #line if it would
     appear to be a no-op, and we output a few newlines instead
     if we want to increase the line number by a small amount.
     We cannot do this if pfile->lineno is zero, because that means we
     haven't output any line commands yet.  (The very first line command
     output is a `same_file' command.)  */
  if (file_change == same_file && pfile->lineno != 0)
    {
      if (line == pfile->lineno)
	return;

      /* If the inherited line number is a little too small,
	 output some newlines instead of a #line command.  */
      if (line > pfile->lineno && line < pfile->lineno + 8)
	{
	  CPP_RESERVE (pfile, 20);
	  while (line > pfile->lineno)
	    {
	      CPP_PUTC_Q (pfile, '\n');
	      pfile->lineno++;
	    }
	  return;
	}
    }

  CPP_RESERVE (pfile, 4 * strlen (ip->nominal_fname) + 50);
  CPP_PUTS_Q (pfile, "# ", 2);

  sprintf ((char *) CPP_PWRITTEN (pfile), "%ld ", line);
  CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));

  quote_string (pfile, ip->nominal_fname); 
  if (file_change != same_file)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, file_change == enter_file ? '1' : '2');
    }
  /* Tell cc1 if following text comes from a system header file.  */
  if (ip->system_header_p)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, '3');
    }
#ifndef NO_IMPLICIT_EXTERN_C
  /* Tell cc1plus if following text should be treated as C.  */
  if (ip->system_header_p == 2 && CPP_OPTIONS (pfile)->cplusplus)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, '4');
    }
#endif
  CPP_PUTC_Q (pfile, '\n');
  pfile->lineno = line;
}


/* Like cpp_get_token, except that it does not read past end-of-line.
   Also, horizontal space is skipped, and macros are popped.  */

static enum cpp_token
get_directive_token (pfile)
     cpp_reader *pfile;
{
  for (;;)
    {
      long old_written = CPP_WRITTEN (pfile);
      enum cpp_token token;
      cpp_skip_hspace (pfile);
      if (PEEKC () == '\n')
	  return CPP_VSPACE;
      token = cpp_get_token (pfile);
      switch (token)
      {
      case CPP_POP:
	  if (! CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
	      return token;
	  /* ... else fall though ...  */
      case CPP_HSPACE:  case CPP_COMMENT:
	  CPP_SET_WRITTEN (pfile, old_written);
	  break;
      default:
	  return token;
      }
    }
}

/* Handle #include and #import.
   This function expects to see "fname" or <fname> on the input.

   The input is normally in part of the output_buffer following
   CPP_WRITTEN, and will get overwritten by output_line_command.
   I.e. in input file specification has been popped by handle_directive.
   This is safe.  */

static int
do_include (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword;
{
  int importing = (keyword->type == T_IMPORT);
  int skip_dirs = (keyword->type == T_INCLUDE_NEXT);
  int angle_brackets = 0;	/* 0 for "...", 1 for <...> */
  int before;  /* included before? */
  long flen;
  unsigned char *ftok;
  cpp_buffer *fp;

  enum cpp_token token;

  /* Chain of dirs to search */
  struct include_hash *ihash;
  struct file_name_list *search_start;
  
  long old_written = CPP_WRITTEN (pfile);

  int fd;

  if (CPP_PEDANTIC (pfile) && !CPP_BUFFER (pfile)->system_header_p)
    {
      if (importing)
	cpp_pedwarn (pfile, "ANSI C does not allow `#import'");
      if (skip_dirs)
	cpp_pedwarn (pfile, "ANSI C does not allow `#include_next'");
    }

  if (importing && CPP_OPTIONS (pfile)->warn_import
      && !CPP_OPTIONS (pfile)->inhibit_warnings
      && !CPP_BUFFER (pfile)->system_header_p && !pfile->import_warning)
    {
      pfile->import_warning = 1;
      cpp_warning (pfile,
	   "#import is obsolete, use an #ifndef wrapper in the header file");
    }

  pfile->parsing_include_directive++;
  token = get_directive_token (pfile);
  pfile->parsing_include_directive--;

  if (token == CPP_STRING)
    {
      if (pfile->token_buffer[old_written] == '<')
	angle_brackets = 1;
    }
#ifdef VMS
  else if (token == CPP_NAME)
    {
      /* Support '#include xyz' like VAX-C.  It is taken as
         '#include <xyz.h>' and generates a warning.  */
      cpp_warning (pfile,
	       "`#include filename' is obsolete, use `#include <filename.h>'");
      angle_brackets = 1;

      /* Append the missing `.h' to the name. */
      CPP_PUTS (pfile, ".h", 2);
    }
#endif
  else
    {
      cpp_error (pfile,
		 "`#%s' expects \"FILENAME\" or <FILENAME>", keyword->name);
      CPP_SET_WRITTEN (pfile, old_written);
      skip_rest_of_line (pfile);
      return 0;
    }

  flen = CPP_WRITTEN (pfile) - old_written;
  ftok = alloca (flen + 1);
  memcpy (ftok, pfile->token_buffer + old_written, flen);
  ftok[flen] = '\0';

  if (get_directive_token (pfile) != CPP_VSPACE)
    {
      cpp_error (pfile, "junk at end of `#include'");
      skip_rest_of_line (pfile);
    }

  CPP_SET_WRITTEN (pfile, old_written);

  if (flen == 0)
    {
      cpp_error (pfile, "empty file name in `#%s'", keyword->name);
      return 0;
    }

  if (CPP_OPTIONS (pfile)->dump_includes)
    pass_thru_directive (ftok,
			 flen
#ifdef VMS
	  - ((token == CPP_NAME) ? 2 : 0)
#endif
			 , pfile, keyword);

#ifdef VMS
  if (token == CPP_STRING)
#endif
    {
      ftok++;
      flen -= 2;
      ftok[flen] = '\0';
    }

  search_start = 0;

  for (fp = CPP_BUFFER (pfile);
       fp != CPP_NULL_BUFFER (pfile);
       fp = CPP_PREV_BUFFER (fp))
    if (fp->fname != NULL)
      break;

  if (fp == CPP_NULL_BUFFER (pfile))
    {
      cpp_fatal (pfile, "cpp internal error: fp == NULL_BUFFER in do_include");
      return 0;
    }
  
  /* For #include_next, skip in the search path past the dir in which the
     containing file was found.  Treat files specified using an absolute path
     as if there are no more directories to search.  Treat the primary source
     file like any other included source, but generate a warning.  */
  if (skip_dirs && CPP_PREV_BUFFER(fp) != CPP_NULL_BUFFER (pfile))
    {
      if (fp->ihash->foundhere != ABSOLUTE_PATH)
	search_start = fp->ihash->foundhere->next;
    }
  else
    {
      if (skip_dirs)
	cpp_warning (pfile, "#include_next in primary source file");
      
      if (angle_brackets)
	search_start = CPP_OPTIONS (pfile)->bracket_include;
      else
        {
	  if (!CPP_OPTIONS (pfile)->ignore_srcdir)
	    {
	      if (fp)
		search_start = fp->actual_dir;
	    }
	  else
	    search_start = CPP_OPTIONS (pfile)->quote_include;
	}
    }

  if (!search_start)
    {
      cpp_error (pfile, "No include path in which to find %s", ftok);
      return 0;
    }

  fd = find_include_file (pfile, ftok, search_start, &ihash, &before);

  if (fd == -2)
    return 0;
  
  if (fd == -1)
    {
      if (CPP_OPTIONS (pfile)->print_deps_missing_files
	  && CPP_PRINT_DEPS (pfile) > (angle_brackets ||
				       (pfile->system_include_depth > 0)))
        {
	  if (!angle_brackets)
	    deps_output (pfile, ftok, ' ');
	  else
	    {
	      char *p;
	      struct file_name_list *ptr;
	      /* If requested as a system header, assume it belongs in
		 the first system header directory. */
	      if (CPP_OPTIONS (pfile)->bracket_include)
	        ptr = CPP_OPTIONS (pfile)->bracket_include;
	      else
	        ptr = CPP_OPTIONS (pfile)->quote_include;

	      p = (char *) alloca (strlen (ptr->name)
				   + strlen (ftok) + 2);
	      if (*ptr->name != '\0')
	        {
		  strcpy (p, ptr->name);
		  strcat (p, "/");
	        }
	      strcat (p, ftok);
	      deps_output (pfile, p, ' ');
	    }
	}
      /* If -M was specified, and this header file won't be added to
	 the dependency list, then don't count this as an error,
	 because we can still produce correct output.  Otherwise, we
	 can't produce correct output, because there may be
	 dependencies we need inside the missing file, and we don't
	 know what directory this missing file exists in. */
      else if (CPP_PRINT_DEPS (pfile)
	       && (CPP_PRINT_DEPS (pfile)
		   <= (angle_brackets || (pfile->system_include_depth > 0))))
	cpp_warning (pfile, "No include path in which to find %s", ftok);
      else
	cpp_error_from_errno (pfile, ftok);

      return 0;
    }

  /* For -M, add the file to the dependencies on its first inclusion. */
  if (!before && (CPP_PRINT_DEPS (pfile)
		  > (angle_brackets || (pfile->system_include_depth > 0))))
    deps_output (pfile, ihash->name, ' ');

  /* Handle -H option.  */
  if (CPP_OPTIONS(pfile)->print_include_names)
    {
      fp = CPP_BUFFER (pfile);
      while ((fp = CPP_PREV_BUFFER (fp)) != CPP_NULL_BUFFER (pfile))
	putc ('.', stderr);
      fprintf (stderr, " %s\n", ihash->name);
    }

  /* Actually process the file */

  if (importing)
    ihash->control_macro = "";
  
  if (cpp_push_buffer (pfile, NULL, 0) == NULL)
    {
      close (fd);
      return 0;
    }
  
  if (angle_brackets)
    pfile->system_include_depth++;   /* Decremented in file_cleanup. */

  if (finclude (pfile, fd, ihash))
    {
      output_line_command (pfile, enter_file);
      pfile->only_seen_white = 2;
    }

  return 0;
}

/* Interpret #line command.
   Note that the filename string (if any) is treated as if it were an
   include filename.  That means no escape handling.  */

static int
do_line (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  int new_lineno;
  long old_written = CPP_WRITTEN (pfile);
  enum file_change_code file_change = same_file;
  enum cpp_token token;
  char *x;

  token = get_directive_token (pfile);

  if (token != CPP_NUMBER)
    {
      cpp_error (pfile, "token after `#line' is not an integer");
      goto bad_line_directive;
    }

  new_lineno = strtol (pfile->token_buffer + old_written, &x, 10);
  if (x[0] != '\0')
    {
      cpp_error (pfile, "token after `#line' is not an integer");
      goto bad_line_directive;
    }      
  CPP_SET_WRITTEN (pfile, old_written);

  if (CPP_PEDANTIC (pfile) && new_lineno <= 0)
    cpp_pedwarn (pfile, "line number out of range in `#line' command");

  token = get_directive_token (pfile);

  if (token == CPP_STRING)
    {
      U_CHAR *fname = pfile->token_buffer + old_written + 1;
      U_CHAR *end_name = CPP_PWRITTEN (pfile) - 1;
      long num_start = CPP_WRITTEN (pfile);

      token = get_directive_token (pfile);
      if (token != CPP_VSPACE && token != CPP_EOF && token != CPP_POP)
	{
	  U_CHAR *p = pfile->token_buffer + num_start;
	  if (CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "garbage at end of `#line' command");

	  if (token != CPP_NUMBER || *p < '0' || *p > '4' || p[1] != '\0')
	    {
	      cpp_error (pfile, "invalid format `#line' command");
	      goto bad_line_directive;
	    }
	  if (*p == '1')
	    file_change = enter_file;
	  else if (*p == '2')
	    file_change = leave_file;
	  else if (*p == '3')
	    ip->system_header_p = 1;
	  else /* if (*p == '4') */
	    ip->system_header_p = 2;

	  CPP_SET_WRITTEN (pfile, num_start);
	  token = get_directive_token (pfile);
	  p = pfile->token_buffer + num_start;
	  if (token == CPP_NUMBER && p[1] == '\0' && (*p == '3' || *p== '4'))
	    {
	      ip->system_header_p = *p == '3' ? 1 : 2;
	      token = get_directive_token (pfile);
	    }
	  if (token != CPP_VSPACE)
	    {
	      cpp_error (pfile, "invalid format `#line' command");
	      goto bad_line_directive;
	    }
	}
      
      *end_name = '\0';
      
      if (strcmp (fname, ip->nominal_fname))
	{
	  char *newname, *oldname;
	  if (!strcmp (fname, ip->fname))
	    newname = ip->fname;
	  else if (ip->last_nominal_fname
		   && !strcmp (fname, ip->last_nominal_fname))
	    newname = ip->last_nominal_fname;
	  else
	    newname = xstrdup (fname);

	  oldname = ip->nominal_fname;
	  ip->nominal_fname = newname;

	  if (ip->last_nominal_fname
	      && ip->last_nominal_fname != oldname
	      && ip->last_nominal_fname != newname
	      && ip->last_nominal_fname != ip->fname)
	    free (ip->last_nominal_fname);

	  if (newname == ip->fname)
	    ip->last_nominal_fname = NULL;
	  else
	    ip->last_nominal_fname = oldname;
	} 
    }
  else if (token != CPP_VSPACE && token != CPP_EOF)
    {
      cpp_error (pfile, "token after `#line %d' is not a string", new_lineno);
      goto bad_line_directive;
    }

  /* The Newline at the end of this line remains to be processed.
     To put the next line at the specified line number,
     we must store a line number now that is one less.  */
  ip->lineno = new_lineno - 1;
  CPP_SET_WRITTEN (pfile, old_written);
  output_line_command (pfile, file_change);
  return 0;

 bad_line_directive:
  skip_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old_written);
  return 0;
}

/* Remove the definition of a symbol from the symbol table.
   According to the C standard, it is not an error to undef
   something that has no definitions. */
static int
do_undef (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword;
{
  int sym_length;
  HASHNODE *hp;
  U_CHAR *buf, *name, *limit;
  int c;
  long here = CPP_WRITTEN (pfile);
  enum cpp_token token;

  cpp_skip_hspace (pfile);
  c = GETC();
  if (! is_idstart[c])
  {
      cpp_error (pfile, "token after #undef is not an identifier");
      skip_rest_of_line (pfile);
      return 1;
  }

  parse_name (pfile, c);
  buf = pfile->token_buffer + here;
  limit = CPP_PWRITTEN(pfile);

  /* Copy out the token so we can pop the token buffer. */
  name = alloca (limit - buf + 1);
  bcopy(buf, name, limit - buf);
  name[limit - buf] = '\0';

  token = get_directive_token (pfile);
  if (token != CPP_VSPACE && token != CPP_POP)
  {
      cpp_pedwarn (pfile, "junk on line after #undef");
      skip_rest_of_line (pfile);
  }

  CPP_SET_WRITTEN (pfile, here);

  sym_length = check_macro_name (pfile, buf, 0);

  while ((hp = cpp_lookup (pfile, name, sym_length, -1)) != NULL)
    {
      /* If we are generating additional info for debugging (with -g) we
	 need to pass through all effective #undef commands.  */
      if (CPP_OPTIONS (pfile)->debug_output && keyword)
	pass_thru_directive (name, sym_length, pfile, keyword);
      if (hp->type != T_MACRO)
	cpp_warning (pfile, "undefining `%s'", hp->name);
      delete_macro (hp);
    }

  return 0;
}

/* Wrap do_undef for -U processing. */
void
cpp_undef (pfile, macro)
     cpp_reader *pfile;
     U_CHAR *macro;
{
  if (cpp_push_buffer (pfile, macro, strlen (macro)))
    {
      do_undef (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}


/*
 * Report an error detected by the program we are processing.
 * Use the text of the line in the error message.
 * (We use error because it prints the filename & line#.)
 */

static int
do_error (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  long here = CPP_WRITTEN (pfile);
  U_CHAR *text;
  copy_rest_of_line (pfile);
  text = pfile->token_buffer + here;
  SKIP_WHITE_SPACE(text);

  cpp_error (pfile, "#error %s", text);
  CPP_SET_WRITTEN (pfile, here);
  return 0;
}

/*
 * Report a warning detected by the program we are processing.
 * Use the text of the line in the warning message, then continue.
 */

static int
do_warning (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  U_CHAR *text;
  long here = CPP_WRITTEN(pfile);
  copy_rest_of_line (pfile);
  text = pfile->token_buffer + here;
  SKIP_WHITE_SPACE(text);

  if (CPP_PEDANTIC (pfile) && !CPP_BUFFER (pfile)->system_header_p)
    cpp_pedwarn (pfile, "ANSI C does not allow `#warning'");

  /* Use `pedwarn' not `warning', because #warning isn't in the C Standard;
     if -pedantic-errors is given, #warning should cause an error.  */
  cpp_pedwarn (pfile, "#warning %s", text);
  CPP_SET_WRITTEN (pfile, here);
  return 0;
}

/* Report program identification.
   This is not precisely what cccp does with #ident, however I believe
   it matches `closely enough' (behavior is identical as long as there
   are no macros on the #ident line, which is pathological in my opinion).  */

static int
do_ident (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  /* Allow #ident in system headers, since that's not user's fault.  */
  if (CPP_PEDANTIC (pfile) && !CPP_BUFFER (pfile)->system_header_p)
    cpp_pedwarn (pfile, "ANSI C does not allow `#ident'");

  CPP_PUTS (pfile, "#ident ", 7);
  cpp_skip_hspace (pfile);
  copy_rest_of_line (pfile);

  return 0;
}

/* Just check for some recognized pragmas that need validation here,
   and leave the text in the token buffer to be output. */

static int
do_pragma (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  long here;
  U_CHAR *buf;

  CPP_PUTS (pfile, "#pragma ", 8);
  cpp_skip_hspace (pfile);
  
  here = CPP_WRITTEN (pfile);
  copy_rest_of_line (pfile);
  buf = pfile->token_buffer + here;
  
  if (!strncmp (buf, "once", 4))
    {
      cpp_buffer *ip = NULL;

      /* Allow #pragma once in system headers, since that's not the user's
	 fault.  */
      if (!CPP_BUFFER (pfile)->system_header_p)
	cpp_warning (pfile, "`#pragma once' is obsolete");
      
      for (ip = CPP_BUFFER (pfile); ; ip = CPP_PREV_BUFFER (ip))
        {
	  if (ip == CPP_NULL_BUFFER (pfile))
	    return 0;
	  if (ip->fname != NULL)
	    break;
	}

      if (CPP_PREV_BUFFER (ip) == CPP_NULL_BUFFER (pfile))
	cpp_warning (pfile, "`#pragma once' outside include file");
      else
	ip->ihash->control_macro = "";  /* never repeat */
    }
  else if (!strncmp (buf, "implementation", 14))
    {
      /* Be quiet about `#pragma implementation' for a file only if it hasn't
	 been included yet.  */
      struct include_hash *ptr;
      U_CHAR *p = buf + 14, *fname, *fcopy;
      SKIP_WHITE_SPACE (p);
      if (*p == '\n' || *p != '\"')
        return 0;

      fname = p + 1;
      p = (U_CHAR *) index (fname, '\"');

      fcopy = alloca (p - fname + 1);
      bcopy (fname, fcopy, p - fname);
      fcopy[p-fname] = '\0';

      ptr = include_hash (pfile, fcopy, 0);
      if (ptr)
        cpp_warning (pfile,
	  "`#pragma implementation' for `%s' appears after file is included",
		     fcopy);
    }

  return 0;
}

#ifdef SCCS_DIRECTIVE
/* Just ignore #sccs, on systems where we define it at all.  */

static int
do_sccs (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#sccs'");
  skip_rest_of_line (pfile);
  return 0;
}
#endif

/*
 * handle #if command by
 *   1) inserting special `defined' keyword into the hash table
 *	that gets turned into 0 or 1 by special_symbol (thus,
 *	if the luser has a symbol called `defined' already, it won't
 *      work inside the #if command)
 *   2) rescan the input into a temporary output buffer
 *   3) pass the output buffer to the yacc parser and collect a value
 *   4) clean up the mess left from steps 1 and 2.
 *   5) call conditional_skip to skip til the next #endif (etc.),
 *      or not, depending on the value from step 3.
 */

static int
do_if (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  HOST_WIDEST_INT value = eval_if_expression (pfile);
  conditional_skip (pfile, value == 0, T_IF, NULL_PTR);
  return 0;
}

/*
 * handle a #elif directive by not changing  if_stack  either.
 * see the comment above do_else.
 */

static int
do_elif (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack) {
    cpp_error (pfile, "`#elif' not within a conditional");
    return 0;
  } else {
    if (pfile->if_stack->type != T_IF && pfile->if_stack->type != T_ELIF) {
      cpp_error (pfile, "`#elif' after `#else'");
#if 0
      fprintf (stderr, " (matches line %d", pfile->if_stack->lineno);
#endif
      if (pfile->if_stack->fname != NULL && CPP_BUFFER (pfile)->fname != NULL
	  && strcmp (pfile->if_stack->fname,
		     CPP_BUFFER (pfile)->nominal_fname) != 0)
	fprintf (stderr, ", file %s", pfile->if_stack->fname);
      fprintf (stderr, ")\n");
    }
    pfile->if_stack->type = T_ELIF;
  }

  if (pfile->if_stack->if_succeeded)
    skip_if_group (pfile);
  else {
    HOST_WIDEST_INT value = eval_if_expression (pfile);
    if (value == 0)
      skip_if_group (pfile);
    else {
      ++pfile->if_stack->if_succeeded;	/* continue processing input */
      output_line_command (pfile, same_file);
    }
  }
  return 0;
}

/*
 * evaluate a #if expression in BUF, of length LENGTH,
 * then parse the result as a C expression and return the value as an int.
 */

static HOST_WIDEST_INT
eval_if_expression (pfile)
     cpp_reader *pfile;
{
  HOST_WIDEST_INT value;
  long old_written = CPP_WRITTEN (pfile);

  pfile->pcp_inside_if = 1;
  value = cpp_parse_expr (pfile);
  pfile->pcp_inside_if = 0;

  CPP_SET_WRITTEN (pfile, old_written); /* Pop */

  return value;
}

/*
 * routine to handle ifdef/ifndef.  Try to look up the symbol,
 * then do or don't skip to the #endif/#else/#elif depending
 * on what directive is actually being processed.
 */

static int
do_xifdef (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword;
{
  int skip;
  cpp_buffer *ip = CPP_BUFFER (pfile);
  U_CHAR *ident;
  int ident_length;
  enum cpp_token token;
  int start_of_file = 0;
  U_CHAR *control_macro = 0;
  int old_written = CPP_WRITTEN (pfile);

  /* Detect a #ifndef at start of file (not counting comments).  */
  if (ip->fname != 0 && keyword->type == T_IFNDEF)
    start_of_file = pfile->only_seen_white == 2;

  pfile->no_macro_expand++;
  token = get_directive_token (pfile);
  pfile->no_macro_expand--;

  ident = pfile->token_buffer + old_written;
  ident_length = CPP_WRITTEN (pfile) - old_written;
  CPP_SET_WRITTEN (pfile, old_written); /* Pop */

  if (token == CPP_VSPACE || token == CPP_POP || token == CPP_EOF)
    {
      skip = (keyword->type == T_IFDEF);
      if (! CPP_TRADITIONAL (pfile))
	cpp_pedwarn (pfile, "`#%s' with no argument", keyword->name);
    }
  else if (token == CPP_NAME)
    {
      HASHNODE *hp = cpp_lookup (pfile, ident, ident_length, -1);
      skip = (hp == NULL) ^ (keyword->type == T_IFNDEF);
      if (start_of_file && !skip)
	{
	  control_macro = (U_CHAR *) xmalloc (ident_length + 1);
	  bcopy (ident, control_macro, ident_length + 1);
	}
    }
  else
    {
      skip = (keyword->type == T_IFDEF);
      if (! CPP_TRADITIONAL (pfile))
	cpp_error (pfile, "`#%s' with invalid argument", keyword->name);
    }

  if (!CPP_TRADITIONAL (pfile))
    { int c;
      cpp_skip_hspace (pfile);
      c = PEEKC ();
      if (c != EOF && c != '\n')
	cpp_pedwarn (pfile, "garbage at end of `#%s' argument", keyword->name);
    }
  skip_rest_of_line (pfile);

#if 0
    if (pcp_outfile) {
      /* Output a precondition for this macro.  */
      if (hp && hp->value.defn->predefined)
	fprintf (pcp_outfile, "#define %s\n", hp->name);
      else {
	U_CHAR *cp = buf;
	fprintf (pcp_outfile, "#undef ");
	while (is_idchar[*cp]) /* Ick! */
	  fputc (*cp++, pcp_outfile);
	putc ('\n', pcp_outfile);
      }
#endif

  conditional_skip (pfile, skip, T_IF, control_macro);
  return 0;
}

/* Push TYPE on stack; then, if SKIP is nonzero, skip ahead.
   If this is a #ifndef starting at the beginning of a file,
   CONTROL_MACRO is the macro name tested by the #ifndef.
   Otherwise, CONTROL_MACRO is 0.  */

static void
conditional_skip (pfile, skip, type, control_macro)
     cpp_reader *pfile;
     int skip;
     enum node_type type;
     U_CHAR *control_macro;
{
  IF_STACK_FRAME *temp;

  temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
  temp->fname = CPP_BUFFER (pfile)->nominal_fname;
#if 0
  temp->lineno = CPP_BUFFER (pfile)->lineno;
#endif
  temp->next = pfile->if_stack;
  temp->control_macro = control_macro;
  pfile->if_stack = temp;

  pfile->if_stack->type = type;

  if (skip != 0) {
    skip_if_group (pfile);
    return;
  } else {
    ++pfile->if_stack->if_succeeded;
    output_line_command (pfile, same_file);
  }
}

/* Subroutine of skip_if_group.	 Examine one preprocessing directive and
   return 0 if skipping should continue, 1 if it should halt.  Also
   adjusts the if_stack as appropriate.
   The `#' has been read, but not the identifier. */

static int
consider_directive_while_skipping (pfile, stack)
    cpp_reader *pfile;
    IF_STACK_FRAME *stack; 
{
  long ident_len, ident;
  struct directive *kt;
  IF_STACK_FRAME *temp;
    
  cpp_skip_hspace (pfile);

  ident = CPP_WRITTEN (pfile);
  parse_name (pfile, GETC());
  ident_len = CPP_WRITTEN (pfile) - ident;

  CPP_SET_WRITTEN (pfile, ident);

  for (kt = directive_table; kt->length >= 0; kt++)
    if (kt->length == ident_len
	&& strncmp (pfile->token_buffer + ident, kt->name, kt->length) == 0)
      switch (kt->type)
	{
	case T_IF:
	case T_IFDEF:
	case T_IFNDEF:
	    temp = (IF_STACK_FRAME *) xmalloc (sizeof (IF_STACK_FRAME));
	    temp->next = pfile->if_stack;
	    pfile->if_stack = temp;
	    temp->fname = CPP_BUFFER(pfile)->nominal_fname;
	    temp->type = kt->type;
	    return 0;

	case T_ELSE:
	    if (CPP_PEDANTIC (pfile) && pfile->if_stack != stack)
	      validate_else (pfile, "#else");
	    /* fall through */
	case T_ELIF:
	    if (pfile->if_stack->type == T_ELSE)
	      cpp_error (pfile, "`%s' after `#else'", kt->name);
	    
	    if (pfile->if_stack == stack)
	      return 1;
	    else
	      {
		pfile->if_stack->type = kt->type;
		return 0;
	      }

	    case T_ENDIF:
		if (CPP_PEDANTIC (pfile) && pfile->if_stack != stack)
		  validate_else (pfile, "#endif");

		if (pfile->if_stack == stack)
		  return 1;
		    
		temp = pfile->if_stack;
		pfile->if_stack = temp->next;
		free (temp);
		return 0;

	    default:
		return 0;
	    }

    /* Don't let erroneous code go by.	*/
    if (!CPP_OPTIONS (pfile)->lang_asm && CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "invalid preprocessor directive name");
    return 0;
}

/* skip to #endif, #else, or #elif.  adjust line numbers, etc.
 * leaves input ptr at the sharp sign found.
 */
static void
skip_if_group (pfile)
    cpp_reader *pfile;
{
  int c;
  IF_STACK_FRAME *save_if_stack = pfile->if_stack; /* don't pop past here */
  U_CHAR *beg_of_line;
  long old_written;

  if (CPP_OPTIONS (pfile)->output_conditionals)
    {
      CPP_PUTS (pfile, "#failed\n", 8);
      pfile->lineno++;
      output_line_command (pfile, same_file);
    }

  old_written = CPP_WRITTEN (pfile);
  
  for (;;)
    {
      beg_of_line = CPP_BUFFER (pfile)->cur;

      if (! CPP_TRADITIONAL (pfile))
	cpp_skip_hspace (pfile);
      c = GETC();
      if (c == '\n')
	{
	  if (CPP_OPTIONS (pfile)->output_conditionals)
	    CPP_PUTC (pfile, c);
	  CPP_BUMP_LINE (pfile);
	  continue;
	}
      else if (c == '#')
	{
	  if (consider_directive_while_skipping (pfile, save_if_stack))
	    break;
	}
      else if (c == EOF)
	return;	 /* Caller will issue error. */

      FORWARD(-1);
      if (CPP_OPTIONS (pfile)->output_conditionals)
	{
	  CPP_PUTS (pfile, beg_of_line, CPP_BUFFER (pfile)->cur - beg_of_line);
	  copy_rest_of_line (pfile);
	}
      else
	{
	  copy_rest_of_line (pfile);
	  CPP_SET_WRITTEN (pfile, old_written);	 /* discard it */
	}

      c = GETC();
      if (c == EOF)
	return;	 /* Caller will issue error. */
      else
	{
	  /* \n */
	  if (CPP_OPTIONS (pfile)->output_conditionals)
	    {
	      CPP_PUTC (pfile, c);
	      pfile->lineno++;
	    }
	  CPP_BUMP_LINE (pfile);
	}
    }	  

  /* Back up to the beginning of this line.  Caller will process the
     directive. */
  CPP_BUFFER (pfile)->cur = beg_of_line;
  pfile->only_seen_white = 1;
  if (CPP_OPTIONS (pfile)->output_conditionals)
    {
      CPP_PUTS (pfile, "#endfailed\n", 11);
      pfile->lineno++;
    }
}

/*
 * handle a #else directive.  Do this by just continuing processing
 * without changing  if_stack ;  this is so that the error message
 * for missing #endif's etc. will point to the original #if.  It
 * is possible that something different would be better.
 */

static int
do_else (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  if (CPP_PEDANTIC (pfile))
    validate_else (pfile, "#else");
  skip_rest_of_line (pfile);

  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack) {
    cpp_error (pfile, "`#else' not within a conditional");
    return 0;
  } else {
    /* #ifndef can't have its special treatment for containing the whole file
       if it has a #else clause.  */
    pfile->if_stack->control_macro = 0;

    if (pfile->if_stack->type != T_IF && pfile->if_stack->type != T_ELIF) {
      cpp_error (pfile, "`#else' after `#else'");
      fprintf (stderr, " (matches line %d", pfile->if_stack->lineno);
      if (strcmp (pfile->if_stack->fname, ip->nominal_fname) != 0)
	fprintf (stderr, ", file %s", pfile->if_stack->fname);
      fprintf (stderr, ")\n");
    }
    pfile->if_stack->type = T_ELSE;
  }

  if (pfile->if_stack->if_succeeded)
    skip_if_group (pfile);
  else {
    ++pfile->if_stack->if_succeeded;	/* continue processing input */
    output_line_command (pfile, same_file);
  }
  return 0;
}

/*
 * unstack after #endif command
 */

static int
do_endif (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (CPP_PEDANTIC (pfile))
    validate_else (pfile, "#endif");
  skip_rest_of_line (pfile);

  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    cpp_error (pfile, "unbalanced `#endif'");
  else
    {
      IF_STACK_FRAME *temp = pfile->if_stack;
      pfile->if_stack = temp->next;
      if (temp->control_macro != 0)
	{
	  /* This #endif matched a #ifndef at the start of the file.
	     See if it is at the end of the file.  */
	  int c;

	  parse_set_mark (pfile);

	  for (;;)
	    {
	      cpp_skip_hspace (pfile);
	      c = GETC ();
	      if (c != '\n')
		break;
	    }
	  parse_goto_mark (pfile);

	  if (c == EOF)
	    {
	      /* This #endif ends a #ifndef
		 that contains all of the file (aside from whitespace).
		 Arrange not to include the file again
		 if the macro that was tested is defined. */
	      struct cpp_buffer *ip;
	      for (ip = CPP_BUFFER (pfile); ; ip = CPP_PREV_BUFFER (ip))
		if (ip->fname != NULL)
		  break;
	      ip->ihash->control_macro = (char *) temp->control_macro;
	    }
        }
      free (temp);
      output_line_command (pfile, same_file);
    }
  return 0;
}

/* When an #else or #endif is found while skipping failed conditional,
   if -pedantic was specified, this is called to warn about text after
   the command name.  P points to the first char after the command name.  */

static void
validate_else (pfile, directive)
     cpp_reader *pfile;
     char *directive;
{
  int c;
  cpp_skip_hspace (pfile);
  c = PEEKC ();
  if (c != EOF && c != '\n')
    cpp_pedwarn (pfile,
		 "text following `%s' violates ANSI standard", directive);
}

/* Get the next token, and add it to the text in pfile->token_buffer.
   Return the kind of token we got.  */
  
enum cpp_token
cpp_get_token (pfile)
     cpp_reader *pfile;
{
  register int c, c2, c3;
  enum cpp_token token;
  struct cpp_options *opts = CPP_OPTIONS (pfile);

 get_next:
  c = GETC();
  if (c == EOF)
    {
    handle_eof:
      if (CPP_BUFFER (pfile)->seen_eof)
	{
	  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)) == CPP_NULL_BUFFER (pfile))
	    return CPP_EOF;

	  cpp_pop_buffer (pfile);
	  goto get_next;
	}
      else
	{
	  cpp_buffer *next_buf
	    = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
	  CPP_BUFFER (pfile)->seen_eof = 1;
	  if (CPP_BUFFER (pfile)->nominal_fname
	      && next_buf != CPP_NULL_BUFFER (pfile))
	    {
	      /* We're about to return from an #include file.
		 Emit #line information now (as part of the CPP_POP) result.
		 But the #line refers to the file we will pop to.  */
	      cpp_buffer *cur_buffer = CPP_BUFFER (pfile);
	      CPP_BUFFER (pfile) = next_buf;
	      pfile->input_stack_listing_current = 0;
	      output_line_command (pfile, leave_file);
	      CPP_BUFFER (pfile) = cur_buffer;
	    }
	  return CPP_POP;
	}
    }
  else
    {
      switch (c)
	{
	case '/':
	  if (PEEKC () == '=')
	    goto op2;

	comment:
	  if (opts->put_out_comments)
	    c = copy_comment (pfile, c);
	  else
	    c = skip_comment (pfile, c);
	  if (c == EOF)
	    goto handle_eof;
	  else if (c != ' ')
	    goto randomchar;
	  
	  /* Comments are equivalent to spaces.
	     For -traditional, a comment is equivalent to nothing.  */
	  if (opts->traditional || opts->put_out_comments)
	    return CPP_COMMENT;
	  else
	    {
	      CPP_PUTC (pfile, c);
	      return CPP_HSPACE;
	    }
#if 0
	  if (opts->for_lint) {
	    U_CHAR *argbp;
	    int cmdlen, arglen;
	    char *lintcmd = get_lintcmd (ibp, limit, &argbp, &arglen, &cmdlen);
	    
	    if (lintcmd != NULL) {
	      /* I believe it is always safe to emit this newline: */
	      obp[-1] = '\n';
	      bcopy ("#pragma lint ", (char *) obp, 13);
	      obp += 13;
	      bcopy (lintcmd, (char *) obp, cmdlen);
	      obp += cmdlen;

	      if (arglen != 0) {
		*(obp++) = ' ';
		bcopy (argbp, (char *) obp, arglen);
		obp += arglen;
	      }

	      /* OK, now bring us back to the state we were in before we entered
		 this branch.  We need #line because the newline for the pragma
		 could mess things up.  */
	      output_line_command (pfile, same_file);
	      *(obp++) = ' ';	/* just in case, if comments are copied thru */
	      *(obp++) = '/';
	    }
	  }
#endif

	case '#':
#if 0
	  /* If this is expanding a macro definition, don't recognize
	     preprocessor directives.  */
	  if (ip->macro != 0)
	    goto randomchar;
	  /* If this is expand_into_temp_buffer, recognize them
	     only after an actual newline at this level,
	     not at the beginning of the input level.  */
	  if (ip->fname == 0 && beg_of_line == ip->buf)
	    goto randomchar;
	  if (ident_length)
	    goto specialchar;
#endif

	  if (!pfile->only_seen_white)
	    goto randomchar;
	  if (handle_directive (pfile))
	    return CPP_DIRECTIVE;
	  pfile->only_seen_white = 0;
	  return CPP_OTHER;

	case '\"':
	case '\'':
	string:
	  parse_string (pfile, c);
	  pfile->only_seen_white = 0;
	  return c == '\'' ? CPP_CHAR : CPP_STRING;

	case '$':
	  if (!opts->dollars_in_ident)
	    goto randomchar;
	  goto letter;

	case ':':
	  if (opts->cplusplus && PEEKC () == ':')
	    goto op2;
	  goto randomchar;

	case '&':
	case '+':
	case '|':
	  c2 = PEEKC ();
	  if (c2 == c || c2 == '=')
	    goto op2;
	  goto randomchar;

	case '*':
	case '!':
	case '%':
	case '=':
	case '^':
	  if (PEEKC () == '=')
	    goto op2;
	  goto randomchar;

	case '-':
	  c2 = PEEKC ();
	  if (c2 == '-' && opts->chill)
	    goto comment;  /* Chill style comment */
	  if (c2 == '-' || c2 == '=' || c2 == '>')
	    goto op2;
	  goto randomchar;

	case '<':
	  if (pfile->parsing_include_directive)
	    {
	      for (;;)
		{
		  CPP_PUTC (pfile, c);
		  if (c == '>')
		    break;
		  c = GETC ();
		  if (c == '\n' || c == EOF)
		    {
		      cpp_error (pfile,
				 "missing '>' in `#include <FILENAME>'");
		      break;
		    }
		  else if (c == '\r')
		    {
		      if (!CPP_BUFFER (pfile)->has_escapes)
			{
			  /* Backslash newline is replaced by nothing. */
			  CPP_ADJUST_WRITTEN (pfile, -1);
			  CPP_BUMP_LINE (pfile);
			}
		      else
			{
			  /* We might conceivably get \r- or \r<space> in
			     here.  Just delete 'em. */
			  int d = GETC();
			  if (d != '-' && d != ' ')
			    cpp_fatal (pfile,
				  "internal error: unrecognized escape \\r%c",
				       d);
			  CPP_ADJUST_WRITTEN (pfile, -1);
			}			  
		    }
		}
	      return CPP_STRING;
	    }
	  /* else fall through */
	case '>':
	  c2 = PEEKC ();
	  if (c2 == '=')
	    goto op2;
	  if (c2 != c)
	    goto randomchar;
	  FORWARD(1);
	  CPP_RESERVE (pfile, 4);
	  CPP_PUTC (pfile, c);
	  CPP_PUTC (pfile, c2);
	  c3 = PEEKC ();
	  if (c3 == '=')
	    CPP_PUTC_Q (pfile, GETC ());
	  CPP_NUL_TERMINATE_Q (pfile);
	  pfile->only_seen_white = 0;
	  return CPP_OTHER;

	case '.':
	  c2 = PEEKC ();
	  if (ISDIGIT(c2))
	    {
	      CPP_RESERVE(pfile, 2);
	      CPP_PUTC_Q (pfile, '.');
	      c = GETC ();
	      goto number;
	    }
	  if (c2 == '.' && PEEKN(1) == '.')
	    {
	      CPP_RESERVE(pfile, 4);
	      CPP_PUTC_Q (pfile, '.');
	      CPP_PUTC_Q (pfile, '.');
	      CPP_PUTC_Q (pfile, '.');
	      FORWARD (2);
	      CPP_NUL_TERMINATE_Q (pfile);
	      pfile->only_seen_white = 0;
	      return CPP_3DOTS;
	    }
	  goto randomchar;

	op2:
	  token = CPP_OTHER;
	  pfile->only_seen_white = 0;
	  CPP_RESERVE(pfile, 3);
	  CPP_PUTC_Q (pfile, c);
	  CPP_PUTC_Q (pfile, GETC ());
	  CPP_NUL_TERMINATE_Q (pfile);
	  return token;

	case 'L':
	  c2 = PEEKC ();
	  if ((c2 == '\'' || c2 == '\"') && !CPP_TRADITIONAL (pfile))
	    {
	      CPP_PUTC (pfile, c);
	      c = GETC ();
	      goto string;
	    }
	  goto letter;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	number:
	  c2  = '.';
	  for (;;)
	    {
	      CPP_RESERVE (pfile, 2);
	      CPP_PUTC_Q (pfile, c);
	      c = PEEKC ();
	      if (c == EOF)
		break;
	      if (!is_idchar[c] && c != '.'
		  && ((c2 != 'e' && c2 != 'E'
		       && ((c2 != 'p' && c2 != 'P') || CPP_C89 (pfile)))
		      || (c != '+' && c != '-')))
		break;
	      FORWARD(1);
	      c2= c;
	    }
	  CPP_NUL_TERMINATE_Q (pfile);
	  pfile->only_seen_white = 0;
	  return CPP_NUMBER;
	case 'b': case 'c': case 'd': case 'h': case 'o':
	case 'B': case 'C': case 'D': case 'H': case 'O':
	  if (opts->chill && PEEKC () == '\'')
	    {
	      pfile->only_seen_white = 0;
	      CPP_RESERVE (pfile, 2);
	      CPP_PUTC_Q (pfile, c);
	      CPP_PUTC_Q (pfile, '\'');
	      FORWARD(1);
	      for (;;)
		{
		  c = GETC();
		  if (c == EOF)
		    goto chill_number_eof;
		  if (!is_idchar[c])
		    break;
		  CPP_PUTC (pfile, c);
		}
	      if (c == '\'')
		{
		  CPP_RESERVE (pfile, 2);
		  CPP_PUTC_Q (pfile, c);
		  CPP_NUL_TERMINATE_Q (pfile);
		  return CPP_STRING;
		}
	      else
		{
		  FORWARD(-1);
		chill_number_eof:
		  CPP_NUL_TERMINATE (pfile);
		  return CPP_NUMBER;
		}
	    }
	  else
	    goto letter;
	case '_':
	case 'a': case 'e': case 'f': case 'g': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'p': case 'q':
	case 'r': case 's': case 't': case 'u': case 'v': case 'w':
	case 'x': case 'y': case 'z':
	case 'A': case 'E': case 'F': case 'G': case 'I': case 'J':
	case 'K': case 'M': case 'N': case 'P': case 'Q': case 'R':
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	case 'Y': case 'Z':
        letter:
          {
	    HASHNODE *hp;
	    unsigned char *ident;
	    int before_name_written = CPP_WRITTEN (pfile);
	    int ident_len;
	    parse_name (pfile, c);
	    pfile->only_seen_white = 0;
	    if (pfile->no_macro_expand)
	      return CPP_NAME;
	    ident = pfile->token_buffer + before_name_written;
	    ident_len = CPP_PWRITTEN (pfile) - ident;
	    hp = cpp_lookup (pfile, ident, ident_len, -1);
	    if (!hp)
	      return CPP_NAME;
	    if (hp->type == T_DISABLED)
	      {
		if (pfile->output_escapes)
		  { /* Return "\r-IDENT", followed by '\0'.  */
		    int i;
		    CPP_RESERVE (pfile, 3);
		    ident = pfile->token_buffer + before_name_written;
		    CPP_ADJUST_WRITTEN (pfile, 2);
		    for (i = ident_len; i >= 0; i--) ident[i+2] = ident[i];
		    ident[0] = '\r';
		    ident[1] = '-';
		  }
		return CPP_NAME;
	      }

	    /* If macro wants an arglist, verify that a '(' follows.
	       first skip all whitespace, copying it to the output
	       after the macro name.  Then, if there is no '(',
	       decide this is not a macro call and leave things that way.  */
	    if (hp->type == T_MACRO && hp->value.defn->nargs >= 0)
	    {
	      int is_macro_call, macbuf_whitespace = 0;

	      parse_set_mark (pfile);
	      for (;;)
		{
		  cpp_skip_hspace (pfile);
		  c = PEEKC ();
		  is_macro_call = c == '(';
		  if (c != EOF)
		    {
		      if (c != '\n')
		        break;
		      FORWARD (1);
		    }
                  else
                    {
                      if (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
                        {
                          if (CPP_BUFFER (pfile)->mark !=
                              (CPP_BUFFER (pfile)->cur
                               - CPP_BUFFER (pfile)->buf))
                             macbuf_whitespace = 1;

			  /* The mark goes away automatically when
			     the buffer is popped. */
                          cpp_pop_buffer (pfile);
                          parse_set_mark (pfile);
                        }
                      else
                        break;
                    }
		}
	      if (!is_macro_call)
                {
                  parse_goto_mark (pfile);
                  if (macbuf_whitespace)
                    CPP_PUTC (pfile, ' ');
                }
	      else
		parse_clear_mark (pfile);
	      if (!is_macro_call)
		return CPP_NAME;
	    }
	    /* This is now known to be a macro call.
	       Expand the macro, reading arguments as needed,
	       and push the expansion on the input stack.  */
	    macroexpand (pfile, hp);
	    CPP_SET_WRITTEN (pfile, before_name_written);
	  }
	  goto get_next;

	case ' ':  case '\t':  case '\v':
	  for (;;)
	    {
	      CPP_PUTC (pfile, c);
	      c = PEEKC ();
	      if (c == EOF || !is_hor_space[c])
		break;
	      FORWARD(1);
	    }
	  return CPP_HSPACE;

	case '\r':
	  if (CPP_BUFFER (pfile)->has_escapes)
	    {
	      c = GETC ();
	      if (c == '-')
		{
		  if (pfile->output_escapes)
		    CPP_PUTS (pfile, "\r-", 2);
		  parse_name (pfile, GETC ());
		  return CPP_NAME;
		}
	      else if (c == ' ')
		{
		  CPP_RESERVE (pfile, 2);
		  if (pfile->output_escapes)
		    CPP_PUTC_Q (pfile, '\r');
		  CPP_PUTC_Q (pfile, c);
		  return CPP_HSPACE;
		}
	      else
		{
		  cpp_fatal (pfile,
			     "internal error: unrecognized escape \\r%c", c);
		  goto get_next;
		}
	    }
	  else
	    {
	      /* Backslash newline is ignored. */
	      CPP_BUMP_LINE (pfile);
	      goto get_next;
	    }

	case '\n':
	  CPP_PUTC (pfile, c);
	  if (pfile->only_seen_white == 0)
	    pfile->only_seen_white = 1;
	  CPP_BUMP_LINE (pfile);
	  if (! CPP_OPTIONS (pfile)->no_line_commands)
	    {
	      pfile->lineno++;
	      if (CPP_BUFFER (pfile)->lineno != pfile->lineno)
		output_line_command (pfile, same_file);
	    }
	  return CPP_VSPACE;

	case '(': token = CPP_LPAREN;    goto char1;
	case ')': token = CPP_RPAREN;    goto char1;
	case '{': token = CPP_LBRACE;    goto char1;
	case '}': token = CPP_RBRACE;    goto char1;
	case ',': token = CPP_COMMA;     goto char1;
	case ';': token = CPP_SEMICOLON; goto char1;

	randomchar:
	default:
	  token = CPP_OTHER;
	char1:
	  pfile->only_seen_white = 0;
	  CPP_PUTC (pfile, c);
	  return token;
	}
    }
}

/* Like cpp_get_token, but skip spaces and comments.  */

enum cpp_token
cpp_get_non_space_token (pfile)
     cpp_reader *pfile;
{
  int old_written = CPP_WRITTEN (pfile);
  for (;;)
    {
      enum cpp_token token = cpp_get_token (pfile);
      if (token != CPP_COMMENT && token != CPP_POP
	  && token != CPP_HSPACE && token != CPP_VSPACE)
	return token;
      CPP_SET_WRITTEN (pfile, old_written);
    }
}

/* Parse an identifier starting with C.  */

static void
parse_name (pfile, c)
     cpp_reader *pfile;
     int c;
{
  for (;;)
  {
      if (! is_idchar[c])
      {
	  FORWARD (-1);
	  break;
      }

      if (c == '$' && CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "`$' in identifier");

      CPP_RESERVE(pfile, 2); /* One more for final NUL.  */
      CPP_PUTC_Q (pfile, c);
      c = GETC();
      if (c == EOF)
	break;
  }
  CPP_NUL_TERMINATE_Q (pfile);
  return;
}

/* Parse a string starting with C.  A single quoted string is treated
   like a double -- some programs (e.g., troff) are perverse this way.
   (However, a single quoted string is not allowed to extend over
   multiple lines.  */
static void
parse_string (pfile, c)
     cpp_reader *pfile;
     int c;
{
  long start_line, start_column;
  
  cpp_buf_line_and_col (cpp_file_buffer (pfile), &start_line, &start_column);

  CPP_PUTC (pfile, c);
  while (1)
    {
      int cc = GETC();
      if (cc == EOF)
	{
	  if (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
	    {
	      /* try harder: this string crosses a macro expansion
		 boundary.  This can happen naturally if -traditional.
		 Otherwise, only -D can make a macro with an unmatched
		 quote.  */
	      cpp_pop_buffer (pfile);
	      continue;
	    }
	  if (!CPP_TRADITIONAL (pfile))
	    {
	      cpp_error_with_line (pfile, start_line, start_column,
				 "unterminated string or character constant");
	      if (pfile->multiline_string_line != start_line
		  && pfile->multiline_string_line != 0)
		cpp_error_with_line (pfile,
				     pfile->multiline_string_line, -1,
			       "possible real start of unterminated constant");
	      pfile->multiline_string_line = 0;
	    }
	  break;
	}
      CPP_PUTC (pfile, cc);
      switch (cc)
	{
	case '\n':
	  CPP_BUMP_LINE (pfile);
	  pfile->lineno++;
	  /* Traditionally, end of line ends a string constant with
	     no error.  */
	  if (CPP_TRADITIONAL (pfile))
	    return;
	  /* Character constants may not extend over multiple lines.  */
	  if (c == '\'')
	    {
	      cpp_error_with_line (pfile, start_line, start_column,
				   "unterminated character constant");
	      return;
	    }
	  if (CPP_PEDANTIC (pfile) && pfile->multiline_string_line == 0)
	    {
	      cpp_pedwarn_with_line (pfile, start_line, start_column,
				     "string constant runs past end of line");
	    }
	  if (pfile->multiline_string_line == 0)
	    pfile->multiline_string_line = start_line;
	  break;

	case '\r':
	  CPP_ADJUST_WRITTEN (pfile, -1);
	  if (CPP_BUFFER (pfile)->has_escapes)
	    {
	      cpp_fatal (pfile,
			 "internal error: \\r escape inside string constant");
	      FORWARD(1);
	    }
	  else
	    /* Backslash newline is replaced by nothing at all.  */
	    CPP_BUMP_LINE (pfile);
	  break;

	case '\\':
	  cc = GETC();
	  if (cc != EOF)
	    CPP_PUTC (pfile, cc);
	  break;

	case '\"':
	case '\'':
	  if (cc == c)
	    return;
	  break;
	}
    }
}

/* Read an assertion into the token buffer, converting to
   canonical form: `#predicate(a n swe r)'  The next non-whitespace
   character to read should be the first letter of the predicate.
   Returns 0 for syntax error, 1 for bare predicate, 2 for predicate
   with answer (see callers for why). In case of 0, an error has been
   printed. */
static int
parse_assertion (pfile)
     cpp_reader *pfile;
{
  int c, dropwhite;
  cpp_skip_hspace (pfile);
  c = PEEKC();
  if (! is_idstart[c])
    {
      cpp_error (pfile, "assertion predicate is not an identifier");
      return 0;
    }
  CPP_PUTC(pfile, '#');
  FORWARD(1);
  parse_name(pfile, c);

  c = PEEKC();
  if (c != '(')
    {
      if (is_hor_space[c] || c == '\r')
	cpp_skip_hspace (pfile);
      c = PEEKC();
    }
  if (c != '(')
    return 1;

  CPP_PUTC(pfile, '(');
  FORWARD(1);
  dropwhite = 1;
  while ((c = GETC()) != ')')
    {
      if (is_hor_space[c])
	{
	  if (! dropwhite)
	    {
	      CPP_PUTC(pfile, ' ');
	      dropwhite = 1;
	    }
	}
      else if (c == '\n' || c == EOF)
	{
	  if (c == '\n') FORWARD(-1);
	  cpp_error (pfile, "un-terminated assertion answer");
	  return 0;
	}
      else if (c == '\r')
	/* \r cannot be a macro escape here. */
	CPP_BUMP_LINE (pfile);
      else
	{
	  CPP_PUTC (pfile, c);
	  dropwhite = 0;
	}
    }

  if (pfile->limit[-1] == ' ')
    pfile->limit[-1] = ')';
  else if (pfile->limit[-1] == '(')
    {
      cpp_error (pfile, "empty token sequence in assertion");
      return 0;
    }
  else
    CPP_PUTC (pfile, ')');

  CPP_NUL_TERMINATE (pfile);
  return 2;
}

static int
do_assert (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  char *sym;
  int ret, c;
  HASHNODE *base, *this;
  int baselen, thislen;

  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->done_initializing
      && !CPP_BUFFER (pfile)->system_header_p)
    cpp_pedwarn (pfile, "ANSI C does not allow `#assert'");

  cpp_skip_hspace (pfile);
  sym = (char *) CPP_PWRITTEN (pfile);	/* remember where it starts */
  ret = parse_assertion (pfile);
  if (ret == 0)
    goto error;
  else if (ret == 1)
    {
      cpp_error (pfile, "missing token-sequence in `#assert'");
      goto error;
    }

  cpp_skip_hspace (pfile);
  c = PEEKC();
  if (c != EOF && c != '\n')
    {
      cpp_error (pfile, "junk at end of `#assert'");
      goto error;
    }

  thislen = strlen (sym);
  baselen = index (sym, '(') - sym;
  this = cpp_lookup (pfile, sym, thislen, -1);
  if (this)
    {
      cpp_warning (pfile, "`%s' re-asserted", sym);
      goto error;
    }

  base = cpp_lookup (pfile, sym, baselen, -1);
  if (! base)
    base = cpp_install (pfile, sym, baselen, T_ASSERT, 0, -1);
  else if (base->type != T_ASSERT)
  {
    /* Token clash - but with what?! */
    cpp_fatal (pfile,
	       "cpp internal error: base->type != T_ASSERT in do_assert");
    goto error;
  }

  this = cpp_install (pfile, sym, thislen, T_ASSERT,
		      (char *)base->value.aschain, -1);
  base->value.aschain = this;
  
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;

 error:
  skip_rest_of_line (pfile);
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;
}

static int
do_unassert (pfile, keyword)
     cpp_reader *pfile;
     struct directive *keyword ATTRIBUTE_UNUSED;
{
  int c, ret;
  char *sym;
  long baselen, thislen;
  HASHNODE *base, *this, *next;
  
  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->done_initializing
      && !CPP_BUFFER (pfile)->system_header_p)
    cpp_pedwarn (pfile, "ANSI C does not allow `#unassert'");

  cpp_skip_hspace (pfile);

  sym = (char *) CPP_PWRITTEN (pfile);	/* remember where it starts */
  ret = parse_assertion (pfile);
  if (ret == 0)
    goto error;
  
  cpp_skip_hspace (pfile);
  c = PEEKC ();
  if (c != EOF && c != '\n')
      cpp_error (pfile, "junk at end of `#unassert'");

  thislen = strlen (sym);
  if (ret == 1)
    {
      base = cpp_lookup (pfile, sym, thislen, -1);
      if (! base)
	goto error;  /* It isn't an error to #undef what isn't #defined,
			so it isn't an error to #unassert what isn't
			#asserted either. */
      
      for (this = base->value.aschain; this; this = next)
        {
	  next = this->value.aschain;
	  delete_macro (this);
	}
      delete_macro (base);
    }
  else
    {
      baselen = index (sym, '(') - sym;
      base = cpp_lookup (pfile, sym, baselen, -1);
      if (! base) goto error;
      this = cpp_lookup (pfile, sym, thislen, -1);
      if (! this) goto error;

      next = base;
      while (next->value.aschain != this)
	next = next->value.aschain;

      next->value.aschain = this->value.aschain;
      delete_macro (this);

      if (base->value.aschain == NULL)
	delete_macro (base);  /* Last answer for this predicate deleted. */
    }
  
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;
 error:
  skip_rest_of_line (pfile);
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;
}

/* Process STR as if it appeared as the body of an #unassert. */
void
cpp_unassert (pfile, str)
     cpp_reader *pfile;
     unsigned char *str;
{
  if (cpp_push_buffer (pfile, str, strlen (str)) != NULL)
    {
      do_assert (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}  

int
cpp_read_check_assertion (pfile)
     cpp_reader *pfile;
{
  U_CHAR *name = CPP_PWRITTEN (pfile);
  int result;
  HASHNODE *hp;
  
  FORWARD (1);  /* Skip '#' */
  cpp_skip_hspace (pfile);
  if (! parse_assertion (pfile))
    result = 0;
  else
    {
      hp = cpp_lookup (pfile, name, CPP_PWRITTEN (pfile) - name, -1);
      result = (hp != 0);
    }

  pfile->limit = name;
  return result;
}

/* Remember the current position of PFILE.  */

void
parse_set_mark (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  if (ip->mark != -1)
      cpp_fatal (pfile,
		 "cpp internal error: ip->mark != -1 in parse_set_mark");

  ip->mark = ip->cur - ip->buf;
}

/* Clear the current mark - we no longer need it.  */

void
parse_clear_mark (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  if (ip->mark == -1)
      cpp_fatal (pfile,
		 "cpp internal error: ip->mark == -1 in parse_clear_mark");

  ip->mark = -1;
}

/* Backup the current position of PFILE to that saved in its mark,
   and clear the mark.  */

void
parse_goto_mark (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  if (ip->mark == -1)
      cpp_fatal (pfile,
		 "cpp internal error: ip->mark == -1 in parse_goto_mark");

  ip->cur = ip->buf + ip->mark;
  ip->mark = -1;
}

void
cpp_print_file_and_line (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = cpp_file_buffer (pfile);

  if (ip != NULL)
    {
      long line, col;
      cpp_buf_line_and_col (ip, &line, &col);
      cpp_file_line_for_message (pfile, ip->nominal_fname,
				 line, pfile->show_column ? col : -1);
    }
}

static void
v_cpp_error (pfile, msgid, ap)
  cpp_reader *pfile;
  const char *msgid;
  va_list ap;
{
  cpp_print_containing_files (pfile);
  cpp_print_file_and_line (pfile);
  v_cpp_message (pfile, 1, msgid, ap);
}

void
cpp_error VPROTO ((cpp_reader * pfile, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  const char *msgid;
#endif
  va_list ap;

  VA_START(ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  msgid = va_arg (ap, const char *);
#endif

  v_cpp_error (pfile, msgid, ap);
  va_end(ap);
}

/* Print error message but don't count it.  */

static void
v_cpp_warning (pfile, msgid, ap)
  cpp_reader *pfile;
  const char *msgid;
  va_list ap;
{
  if (CPP_OPTIONS (pfile)->inhibit_warnings)
    return;

  if (CPP_OPTIONS (pfile)->warnings_are_errors)
    pfile->errors++;

  cpp_print_containing_files (pfile);
  cpp_print_file_and_line (pfile);
  v_cpp_message (pfile, 0, msgid, ap);
}

void
cpp_warning VPROTO ((cpp_reader * pfile, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  msgid = va_arg (ap, const char *);
#endif

  v_cpp_warning (pfile, msgid, ap);
  va_end(ap);
}

/* Print an error message and maybe count it.  */

void
cpp_pedwarn VPROTO ((cpp_reader * pfile, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTIONS (pfile)->pedantic_errors)
    v_cpp_error (pfile, msgid, ap);
  else
    v_cpp_warning (pfile, msgid, ap);
  va_end(ap);
}

static void
v_cpp_error_with_line (pfile, line, column, msgid, ap)
  cpp_reader * pfile;
  int line;
  int column;
  const char * msgid;
  va_list ap;
{
  cpp_buffer *ip = cpp_file_buffer (pfile);

  cpp_print_containing_files (pfile);

  if (ip != NULL)
    cpp_file_line_for_message (pfile, ip->nominal_fname, line, column);

  v_cpp_message (pfile, 1, msgid, ap);
}

void
cpp_error_with_line VPROTO ((cpp_reader * pfile, int line, int column,
			     const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int line;
  int column;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  v_cpp_error_with_line(pfile, line, column, msgid, ap);
  va_end(ap);
}

static void
v_cpp_warning_with_line (pfile, line, column, msgid, ap)
  cpp_reader * pfile;
  int line;
  int column;
  const char *msgid;
  va_list ap;
{
  cpp_buffer *ip;

  if (CPP_OPTIONS (pfile)->inhibit_warnings)
    return;

  if (CPP_OPTIONS (pfile)->warnings_are_errors)
    pfile->errors++;

  cpp_print_containing_files (pfile);

  ip = cpp_file_buffer (pfile);

  if (ip != NULL)
    cpp_file_line_for_message (pfile, ip->nominal_fname, line, column);

  v_cpp_message (pfile, 0, msgid, ap);
}  

void
cpp_warning_with_line VPROTO ((cpp_reader * pfile, int line, int column,
			       const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int line;
  int column;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  v_cpp_warning_with_line (pfile, line, column, msgid, ap);
  va_end(ap);
}

void
cpp_pedwarn_with_line VPROTO ((cpp_reader * pfile, int line, int column,
			       const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int line;
  int column;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTIONS (pfile)->pedantic_errors)
    v_cpp_error_with_line (pfile, column, line, msgid, ap);
  else
    v_cpp_warning_with_line (pfile, line, column, msgid, ap);
  va_end(ap);
}

/* Report a warning (or an error if pedantic_errors)
   giving specified file name and line number, not current.  */

void
cpp_pedwarn_with_file_and_line VPROTO ((cpp_reader *pfile, char *file, int line,
					const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  char *file;
  int line;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  file = va_arg (ap, char *);
  line = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  if (!CPP_OPTIONS (pfile)->pedantic_errors
      && CPP_OPTIONS (pfile)->inhibit_warnings)
    return;
  if (file != NULL)
    cpp_file_line_for_message (pfile, file, line, -1);
  v_cpp_message (pfile, CPP_OPTIONS (pfile)->pedantic_errors, msgid, ap);
  va_end(ap);
}

/* my_strerror - return the descriptive text associated with an
   `errno' code.  */

static char *
my_strerror (errnum)
     int errnum;
{
  char *result;

#ifndef VMS
#ifndef HAVE_STRERROR
  result = (char *) ((errnum < sys_nerr) ? sys_errlist[errnum] : 0);
#else
  result = strerror (errnum);
#endif
#else	/* VMS */
  /* VAXCRTL's strerror() takes an optional second argument, which only
     matters when the first argument is EVMSERR.  However, it's simplest
     just to pass it unconditionally.  `vaxc$errno' is declared in
     <errno.h>, and maintained by the library in parallel with `errno'.
     We assume that caller's `errnum' either matches the last setting of
     `errno' by the library or else does not have the value `EVMSERR'.  */

  result = strerror (errnum, vaxc$errno);
#endif

  if (!result)
    result = "errno = ?";

  return result;
}

/* Error including a message from `errno'.  */

void
cpp_error_from_errno (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  cpp_message_from_errno (pfile, 1, name);
}

void
cpp_message_from_errno (pfile, is_error, name)
     cpp_reader *pfile;
     int is_error;
     const char *name;
{
  int e = errno;
  cpp_buffer *ip = cpp_file_buffer (pfile);

  cpp_print_containing_files (pfile);

  if (ip != NULL)
    cpp_file_line_for_message (pfile, ip->nominal_fname, ip->lineno, -1);

  cpp_message (pfile, is_error, "%s: %s", name, my_strerror (e));
}

void
cpp_perror_with_name (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  cpp_message (pfile, 1, "%s: %s: %s", progname, name, my_strerror (errno));
}

/* TODO:
 * No pre-compiled header file support.
 *
 * Possibly different enum token codes for each C/C++ token.
 *
 * Find and cleanup remaining uses of static variables,
 *
 * Support -dM flag (dump_all_macros).
 *
 * Support for_lint flag.
 */
