
/*  A Bison parser, made from ../src/parser.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	AND	258
#define	CONTAINS	259
#define	ELSE	260
#define	END	261
#define	FOREACH	262
#define	IF	263
#define	IN	264
#define	IS	265
#define	LOCAL	266
#define	NOT	267
#define	OR	268
#define	PROC	269
#define	READONLY	270
#define	RETURN	271
#define	INTEGER	272
#define	ID	273
#define	STRING	274

#line 1 "../src/parser.y"


/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Fri Jan 27 15:39:31 PST 1995 by kalsow  
 *      modified on Thu Sep 16 12:01:08 PDT 1993 by harrison
 */

#include "quake.h"

String
    Parser_FileName,
    Parser_PathPrefix;

Integer
    Parser_LineNumber;

static void GenerateAssignment();


#line 83 "../src/parser.y"
typedef union {
    Array array;
    Atom atom;
    Integer integer;
    List list;
    Name name;
    String string;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		133
#define	YYFLAG		-32768
#define	YYNTBASE	35

#define YYTRANSLATE(x) ((unsigned)(x) <= 274 ? yytranslate[x] : 67)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,    17,     2,    18,     2,    19,
    20,     2,    21,    22,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    23,     2,    24,
    25,    26,     2,    34,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    27,     2,    28,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    29,     2,    30,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    31,    32,    33
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     1,     4,     6,     7,    10,    11,    14,    15,    22,
    23,    31,    34,    35,    44,    46,    49,    53,    58,    63,
    68,    75,    76,    84,    86,    87,    91,    93,    96,   100,
   104,   105,   107,   109,   112,   114,   116,   118,   119,   123,
   126,   128,   131,   135,   139,   140,   145,   146,   151,   155,
   159,   162,   166,   169,   170,   172,   174,   177,   181,   185,
   186,   188,   192,   193,   195,   199,   201,   205,   209,   210,
   212,   216,   220,   221,   223
};

static const short yyrhs[] = {    -1,
    36,    37,     0,    38,     0,     0,    38,    41,     0,     0,
    40,    37,     0,     0,    26,    53,     9,    42,    38,     6,
     0,     0,    26,    26,    53,     9,    43,    38,     6,     0,
    32,    65,     0,     0,    50,    14,    32,    63,    10,    44,
    37,     6,     0,    16,     0,    16,    53,     0,    46,    25,
    53,     0,    51,    46,    25,    53,     0,    46,    21,    25,
    53,     0,     8,    53,    39,     6,     0,     8,    53,    39,
     5,    39,     6,     0,     0,     7,    32,    45,     9,    53,
    39,     6,     0,    32,     0,     0,    32,    47,    48,     0,
    49,     0,    48,    49,     0,    27,    53,    28,     0,    29,
    53,    30,     0,     0,    51,     0,    52,     0,    51,    52,
     0,    11,     0,    15,     0,    33,     0,     0,    32,    54,
    57,     0,    34,    32,     0,    31,     0,    32,    65,     0,
    19,    53,    20,     0,    53,    18,    53,     0,     0,    27,
    55,    60,    28,     0,     0,    29,    56,    61,    30,     0,
    53,    13,    53,     0,    53,     3,    53,     0,    12,    53,
     0,    53,     4,    53,     0,    17,    32,     0,     0,    58,
     0,    59,     0,    58,    59,     0,    27,    53,    28,     0,
    29,    53,    30,     0,     0,    53,     0,    60,    22,    53,
     0,     0,    62,     0,    61,    22,    62,     0,    53,     0,
    53,    23,    53,     0,    19,    64,    20,     0,     0,    32,
     0,    64,    22,    32,     0,    19,    66,    20,     0,     0,
    53,     0,    66,    22,    53,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   103,   105,   111,   115,   116,   131,   133,   141,   143,   148,
   150,   156,   163,   165,   175,   180,   185,   190,   195,   200,
   205,   210,   212,   220,   223,   226,   229,   230,   234,   237,
   243,   244,   248,   249,   253,   254,   258,   259,   262,   262,
   266,   268,   273,   274,   277,   279,   282,   284,   287,   290,
   293,   296,   299,   306,   307,   311,   312,   316,   319,   325,
   326,   327,   331,   332,   333,   337,   340,   344,   350,   351,
   352,   356,   362,   363,   364
};

static const char * const yytname[] = {   "$","error","$illegal.","AND","CONTAINS",
"ELSE","END","FOREACH","IF","IN","IS","LOCAL","NOT","OR","PROC","READONLY","RETURN",
"'$'","'&'","'('","')'","'+'","','","':'","'<'","'='","'>'","'['","']'","'{'",
"'}'","INTEGER","ID","STRING","'@'","program","@1","unsaved_block","statement_list",
"saved_block","@2","statement","@3","@4","@5","@6","lvalue","@7","lhs_qualifier_list",
"lhs_qualifier","opt_decl_modifier_list","decl_modifier_list","decl_modifier",
"expression","@8","@9","@10","opt_rhs_qualifier_list","rhs_qualifier_list","rhs_qualifier",
"expression_list","table_constructor_list","table_element","formal_arg_spec",
"formal_arg_list","actual_arg_spec","actual_arg_list",""
};
#endif

static const short yyr1[] = {     0,
    36,    35,    37,    38,    38,    40,    39,    42,    41,    43,
    41,    41,    44,    41,    41,    41,    41,    41,    41,    41,
    41,    45,    41,    46,    47,    46,    48,    48,    49,    49,
    50,    50,    51,    51,    52,    52,    53,    54,    53,    53,
    53,    53,    53,    53,    55,    53,    56,    53,    53,    53,
    53,    53,    53,    57,    57,    58,    58,    59,    59,    60,
    60,    60,    61,    61,    61,    62,    62,    63,    64,    64,
    64,    65,    66,    66,    66
};

static const short yyr2[] = {     0,
     0,     2,     1,     0,     2,     0,     2,     0,     6,     0,
     7,     2,     0,     8,     1,     2,     3,     4,     4,     4,
     6,     0,     7,     1,     0,     3,     1,     2,     3,     3,
     0,     1,     1,     2,     1,     1,     1,     0,     3,     2,
     1,     2,     3,     3,     0,     4,     0,     4,     3,     3,
     2,     3,     2,     0,     1,     1,     2,     3,     3,     0,
     1,     3,     0,     1,     3,     1,     3,     3,     0,     1,
     3,     3,     0,     1,     3
};

static const short yydefact[] = {     1,
     4,     2,     3,     0,     0,    35,    36,    15,     0,    24,
     5,     0,     0,    32,    33,    22,     0,     0,     0,    45,
    47,    41,    38,    37,     0,     6,    16,     0,     0,    73,
     0,    12,     0,     0,     0,    25,     0,    34,     0,    51,
    53,     0,    60,    63,    54,    42,    40,     0,     0,     0,
     0,     0,     4,     0,     8,    74,     0,     0,     0,    26,
    27,     0,    17,     0,     0,     0,    43,    61,     0,    66,
     0,    64,     0,     0,    39,    55,    56,    50,    52,    49,
    44,     6,    20,     7,    10,     4,    72,     0,     0,     0,
    28,    19,    69,     0,    18,     6,     0,    46,     0,     0,
    48,     0,     0,    57,     0,     4,    31,    75,    29,    30,
    70,     0,    13,     0,    62,    67,    65,    58,    59,    21,
    31,     9,    68,     0,     4,    23,    11,    71,     0,    14,
     0,     0,     0
};

static const short yydefgoto[] = {   131,
     1,     2,     3,    52,    53,    11,    86,   106,   125,    39,
    12,    31,    60,    61,    13,    14,    15,    70,    45,    43,
    44,    75,    76,    77,    69,    71,    72,    94,   112,    32,
    57
};

static const short yypact[] = {-32768,
-32768,-32768,    82,   -11,   103,-32768,-32768,   103,    92,    20,
-32768,    53,    28,    -2,-32768,-32768,   103,     5,   103,-32768,
-32768,-32768,    31,-32768,    27,    52,    52,   103,   136,   103,
    78,-32768,    41,   103,    67,    66,    75,-32768,    97,     6,
-32768,    15,   103,   103,   104,-32768,-32768,   103,   103,   103,
   103,    81,-32768,   138,-32768,    52,    59,   103,   103,    78,
-32768,   103,    52,    91,   103,   103,-32768,    52,    30,   125,
    18,-32768,   103,   103,-32768,   104,-32768,     6,    95,   149,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   103,    23,     2,
-32768,    52,    84,   140,    52,    52,   103,-32768,   103,   103,
-32768,    99,     4,-32768,   151,-32768,    56,    52,-32768,-32768,
-32768,   124,-32768,   152,    52,    52,-32768,-32768,-32768,-32768,
    69,-32768,-32768,   123,-32768,-32768,-32768,-32768,   153,-32768,
   160,   161,-32768
};

static const short yypgoto[] = {-32768,
-32768,   -52,   -75,   -80,-32768,-32768,-32768,-32768,-32768,-32768,
   148,-32768,-32768,   105,-32768,-32768,   150,    -5,-32768,-32768,
-32768,-32768,-32768,    87,-32768,-32768,    68,-32768,-32768,   143,
-32768
};


#define	YYLAST		168


static const short yytable[] = {    26,
    84,   105,    27,    29,    48,    49,    48,    49,     6,    49,
   107,    40,     7,    42,    50,   114,    50,    48,    49,    51,
    16,    51,    54,    51,    56,    48,    49,    50,    63,    36,
   121,   110,    51,   119,    67,    50,    41,    68,    30,   100,
    51,    35,    78,    79,    80,    81,   -25,   101,   -25,    30,
   109,    97,    89,    90,    48,    49,    92,    98,    47,    95,
    96,   122,     4,     5,    50,    62,     6,   102,   103,    51,
     7,     8,   129,    33,   127,     4,     5,    34,    87,     6,
    88,     9,   108,     7,     8,    82,    83,    10,     4,     5,
   -24,   115,     6,   116,     9,   -31,     7,     8,    64,    65,
    10,    48,    49,    17,    58,    66,    59,     9,    18,    93,
    19,    50,    51,    10,    17,   111,    51,    28,    20,    18,
    21,    19,    22,    23,    24,    25,   118,    48,    49,    20,
    73,    21,    74,    22,    23,    24,    25,    50,    48,    49,
    48,    49,    51,   123,    55,   124,    85,    99,    50,   113,
    50,    48,    49,    51,   128,    51,   120,   126,   130,   132,
   133,    37,   104,    38,    91,    46,    51,   117
};

static const short yycheck[] = {     5,
    53,    82,     8,     9,     3,     4,     3,     4,    11,     4,
    86,    17,    15,    19,    13,    96,    13,     3,     4,    18,
    32,    18,    28,    18,    30,     3,     4,    13,    34,    32,
   106,    30,    18,    30,    20,    13,    32,    43,    19,    22,
    18,    14,    48,    49,    50,    51,    27,    30,    29,    19,
    28,    22,    58,    59,     3,     4,    62,    28,    32,    65,
    66,     6,     7,     8,    13,    25,    11,    73,    74,    18,
    15,    16,   125,    21,     6,     7,     8,    25,    20,    11,
    22,    26,    88,    15,    16,     5,     6,    32,     7,     8,
    25,    97,    11,    99,    26,    14,    15,    16,    32,    25,
    32,     3,     4,    12,    27,     9,    29,    26,    17,    19,
    19,    13,    18,    32,    12,    32,    18,    26,    27,    17,
    29,    19,    31,    32,    33,    34,    28,     3,     4,    27,
    27,    29,    29,    31,    32,    33,    34,    13,     3,     4,
     3,     4,    18,    20,     9,    22,     9,    23,    13,    10,
    13,     3,     4,    18,    32,    18,     6,     6,     6,     0,
     0,    14,    76,    14,    60,    23,    18,   100
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/contrib/share/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/contrib/share/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 103 "../src/parser.y"
{
	    Code_New();
	;
    break;}
case 2:
#line 105 "../src/parser.y"
{
	    (void) Execute_Code(Code_Retrieve());
	;
    break;}
case 3:
#line 111 "../src/parser.y"
{ yyval.integer = yyvsp[0].integer; ;
    break;}
case 4:
#line 115 "../src/parser.y"
{ yyval.integer = RETURN_NOTHING; ;
    break;}
case 5:
#line 116 "../src/parser.y"
{
	    switch (yyvsp[0].integer) {
	    case RETURN_VALUE:
	    case RETURN_SIMPLE:
		/* explicit return overrides default action */
		yyval.integer = (yyvsp[-1].integer | yyvsp[0].integer) & ~RETURN_NOTHING;
		break;
	    default:
		yyval.integer = yyvsp[-1].integer | yyvsp[0].integer;
		break;
	    }
	;
    break;}
case 6:
#line 131 "../src/parser.y"
{
	    Code_New();
	;
    break;}
case 7:
#line 133 "../src/parser.y"
{
	    Code_Compile(Atom_Code(Code_Retrieve()));

	    yyval.integer = yyvsp[0].integer;
	;
    break;}
case 8:
#line 141 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_StartRedirect));
	;
    break;}
case 9:
#line 143 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_EndRedirect));

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 10:
#line 148 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_StartAppendRedirect));
	;
    break;}
case 11:
#line 150 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_EndRedirect));

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 12:
#line 156 "../src/parser.y"
{
	    Code_Compile(Atom_Integer(yyvsp[0].integer));
	    Code_Compile(Atom_Name(yyvsp[-1].name));
	    Code_Compile(Atom_Operator(Op_ProcedureCall));

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 13:
#line 163 "../src/parser.y"
{
	    Code_New();
        ;
    break;}
case 14:
#line 165 "../src/parser.y"
{
	    Code code = Code_Retrieve();

	    Code_Compile(Atom_Designator(Designator_Name(yyvsp[-5].name)));
	    Code_Compile(Atom_Procedure(yyvsp[-5].name, yyvsp[-4].list, Code_IsFunction(yyvsp[-1].integer), code));
	    /* procedures are always local */
	    GenerateAssignment(/*???DICTFLAGS_LOCAL | */yyvsp[-7].integer);

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 15:
#line 175 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_Return));

	    yyval.integer = RETURN_SIMPLE;
	;
    break;}
case 16:
#line 180 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_ReturnValue));

	    yyval.integer = RETURN_VALUE;
	;
    break;}
case 17:
#line 185 "../src/parser.y"
{
	    GenerateAssignment(DICTFLAGS_NONE);

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 18:
#line 190 "../src/parser.y"
{
	    GenerateAssignment(yyvsp[-3].integer);

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 19:
#line 195 "../src/parser.y"
{
    	    Code_Compile(Atom_Operator(Op_Append));

	    yyval.integer = RETURN_NOTHING;
	;
    break;}
case 20:
#line 200 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_If));

	    yyval.integer = yyvsp[-1].integer | RETURN_NOTHING;
	;
    break;}
case 21:
#line 205 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_IfElse));

	    yyval.integer = yyvsp[-3].integer | yyvsp[-1].integer;
	;
    break;}
case 22:
#line 210 "../src/parser.y"
{
	    Code_Compile(Atom_Designator(Designator_Name(yyvsp[0].name)));
        ;
    break;}
case 23:
#line 212 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_Foreach));

	    yyval.integer = yyvsp[-1].integer;
	;
    break;}
case 24:
#line 220 "../src/parser.y"
{
	   Code_Compile(Atom_Designator(Designator_Name(yyvsp[0].name)));
        ;
    break;}
case 25:
#line 223 "../src/parser.y"
{
	   Code_Compile(Atom_Designator(Designator_Name(yyvsp[0].name)));
        ;
    break;}
case 29:
#line 234 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_ArrayDesignator));
	;
    break;}
case 30:
#line 237 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_TableDesignator));
	;
    break;}
case 31:
#line 243 "../src/parser.y"
{ yyval.integer = DICTFLAGS_NONE; ;
    break;}
case 32:
#line 244 "../src/parser.y"
{ yyval.integer = yyvsp[0].integer; ;
    break;}
case 33:
#line 248 "../src/parser.y"
{ yyval.integer = yyvsp[0].integer; ;
    break;}
case 34:
#line 249 "../src/parser.y"
{ yyval.integer = yyvsp[-1].integer | yyvsp[0].integer; ;
    break;}
case 35:
#line 253 "../src/parser.y"
{ yyval.integer = DICTFLAGS_LOCAL; ;
    break;}
case 36:
#line 254 "../src/parser.y"
{ yyval.integer = DICTFLAGS_READONLY; ;
    break;}
case 37:
#line 258 "../src/parser.y"
{ Code_Compile(Atom_String(yyvsp[0].string)); ;
    break;}
case 38:
#line 259 "../src/parser.y"
{
	    Code_Compile(Atom_Name(yyvsp[0].name));
        ;
    break;}
case 40:
#line 262 "../src/parser.y"
{
	    Code_Compile(Atom_Designator(Designator_Name(yyvsp[0].name)));
	    Code_Compile(Atom_Operator(Op_Indirect));
	;
    break;}
case 41:
#line 266 "../src/parser.y"
{ Code_Compile(Atom_Integer(yyvsp[0].integer)); ;
    break;}
case 42:
#line 268 "../src/parser.y"
{
	    Code_Compile(Atom_Integer(yyvsp[0].integer));
	    Code_Compile(Atom_Name(yyvsp[-1].name));
	    Code_Compile(Atom_Operator(Op_FunctionCall));
	;
    break;}
case 44:
#line 274 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_StringCatenate));
	;
    break;}
case 45:
#line 277 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_StartArray));
	;
    break;}
case 46:
#line 279 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_EndArray));
	;
    break;}
case 47:
#line 282 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_StartTable));
	;
    break;}
case 48:
#line 284 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_EndTable));
	;
    break;}
case 49:
#line 287 "../src/parser.y"
{
            Code_Compile(Atom_Operator(Op_Or));
        ;
    break;}
case 50:
#line 290 "../src/parser.y"
{
            Code_Compile(Atom_Operator(Op_And));
        ;
    break;}
case 51:
#line 293 "../src/parser.y"
{
            Code_Compile(Atom_Operator(Op_Not));
        ;
    break;}
case 52:
#line 296 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_TableMembership));
	;
    break;}
case 53:
#line 299 "../src/parser.y"
{
	    Code_Compile(Atom_String(String_New(yyvsp[0].name->text)));
	    Code_Compile(Atom_Operator(Op_Getenv));
	;
    break;}
case 58:
#line 316 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_ArraySelection));
	;
    break;}
case 59:
#line 319 "../src/parser.y"
{
	    Code_Compile(Atom_Operator(Op_TableSelection));
	;
    break;}
case 66:
#line 337 "../src/parser.y"
{
	    Code_Compile(Atom_String(String_FromBoolean(FALSE)));
        ;
    break;}
case 68:
#line 344 "../src/parser.y"
{
	    yyval.list = yyvsp[-1].list;
	;
    break;}
case 69:
#line 350 "../src/parser.y"
{ yyval.list = NULL; ;
    break;}
case 70:
#line 351 "../src/parser.y"
{ yyval.list = List_New(yyvsp[0].name, NULL); ;
    break;}
case 71:
#line 352 "../src/parser.y"
{ yyval.list = List_New(yyvsp[0].name, yyvsp[-2].list); ;
    break;}
case 72:
#line 356 "../src/parser.y"
{
	    yyval.integer = yyvsp[-1].integer;
	;
    break;}
case 73:
#line 362 "../src/parser.y"
{ yyval.integer = 0; ;
    break;}
case 74:
#line 363 "../src/parser.y"
{ yyval.integer = 1; ;
    break;}
case 75:
#line 364 "../src/parser.y"
{ yyval.integer = yyvsp[-2].integer + 1; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/contrib/share/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 367 "../src/parser.y"


static void GenerateAssignment(flags)
DictFlags flags;
{
    if (flags == DICTFLAGS_NONE) {
	Code_Compile(Atom_Operator(Op_Assign));
    } else {
	Code_Compile(Atom_Integer(flags));
	Code_Compile(Atom_Operator(Op_AssignWithOptions));
    }
}

void Parser_ErrorFormat(stream, path_prefix, file_name, line_number)
FILE *stream;
String path_prefix;
String file_name;
int line_number;
{
    if (!String_IsEmpty(path_prefix) &&
	!String_IsEmpty(file_name) &&
	line_number > 0)
	fprintf(stream, "\"%s%c%s\", line %d: ",
		path_prefix->body,
		DIR_SEPARATOR,
		file_name->body,
		line_number);
}

#ifdef __STDC__
void yywarning(char *format, ...)
#else
void yywarning(format, va_alist)
char *format;
va_dcl
#endif
{
    va_list args;

    va_init(args, format);

    Parser_ErrorFormat(stderr,
		       Parser_PathPrefix,
		       Parser_FileName,
		       Parser_LineNumber);
    
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

    va_end(args);
}

#ifdef __STDC__
int yyerror(char *format, ...)
#else
int yyerror(format, va_alist)
char *format;
va_dcl
#endif
{
    va_list args;

    va_init(args, format);
    Parser_ErrorFormat(stderr,
		       Parser_PathPrefix,
		       Parser_FileName,
		       Parser_LineNumber);
    
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

    CallStack_BackTrace(stderr);

    FileIO_Finalize();

    exit(1);
    return 0;
}

void Parser_Initialize()
{
    Parser_PathPrefix = String_New("");
    Parser_FileName = String_New("");
    Parser_LineNumber = -1;
}
