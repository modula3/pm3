/* A Bison parser, made from ../Parse.yacc, by GNU bison 1.75.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON	1

/* Pure parsers.  */
#define YYPURE	0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ENDOFFILE = 0,
     AMPERSAND = 258,
     ASSIGN = 259,
     ASTERISK = 260,
     BAR = 261,
     COLON = 262,
     COMMA = 263,
     DOT = 264,
     DOTDOT = 265,
     EQUAL = 266,
     GREATER = 267,
     GREQUAL = 268,
     LESS = 269,
     LSEQUAL = 270,
     MINUS = 271,
     SHARP = 272,
     PLUS = 273,
     RARROW = 274,
     RPRAGMA = 275,
     RBRACE = 276,
     RBRACKET = 277,
     RPAREN = 278,
     SEMICOLON = 279,
     SLASH = 280,
     SUBTYPE = 281,
     UPARROW = 282,
     LPAREN = 283,
     LBRACKET = 284,
     LBRACE = 285,
     IDENT = 286,
     CARD_CONST = 287,
     REAL_CONST = 288,
     CHAR_CONST = 289,
     STR_CONST = 290,
     PR_EXTERNAL = 291,
     PR_INLINE = 292,
     PR_OBSOLETE = 293,
     PR_UNUSED = 294,
     PR_FATAL = 295,
     PR_NOWARN = 296,
     PR_ASSERT = 297,
     PR_TRACE = 298,
     PR_LINE = 299,
     PR_PRAGMA = 300,
     PR_CALLBACK = 301,
     PR_LL = 302,
     PR_LLsup = 303,
     PR_EXPORTED = 304,
     PR_SPEC = 305,
     PR_LOOPINV = 306,
     IDENTPRIME = 307,
     UPARROWPRIME = 308,
     ALL = 309,
     AXIOM = 310,
     DEPEND = 311,
     ENSURES = 312,
     EXISTS = 313,
     FUNC = 314,
     IFF = 315,
     IMPLIES = 316,
     INVARIANT = 317,
     IS = 318,
     LET = 319,
     MAP = 320,
     MODIFIES = 321,
     ON = 322,
     PRED = 323,
     PROTECT = 324,
     ABSTRACT = 325,
     REQUIRES = 326,
     AND = 327,
     ANY = 328,
     ARRAY = 329,
     AS = 330,
     BGN = 331,
     BITS = 332,
     BRANDED = 333,
     BY = 334,
     CASE = 335,
     CONST = 336,
     DIV = 337,
     DO = 338,
     ELSE = 339,
     ELSIF = 340,
     END = 341,
     EVAL = 342,
     EXCEPT = 343,
     EXCEPTION = 344,
     EXIT = 345,
     EXPORTS = 346,
     FINALLY = 347,
     FOR = 348,
     FROM = 349,
     GENERIC = 350,
     IF = 351,
     IMPORT = 352,
     IN = 353,
     INTERFACE = 354,
     LOCK = 355,
     LOOP = 356,
     METHODS = 357,
     MOD = 358,
     MODULE = 359,
     NOT = 360,
     OBJECT = 361,
     OF = 362,
     OR = 363,
     OVERRIDES = 364,
     PROCEDURE = 365,
     RAISE = 366,
     RAISES = 367,
     READONLY = 368,
     RECORD = 369,
     REF = 370,
     REPEAT = 371,
     RETURN = 372,
     REVEAL = 373,
     ROOT = 374,
     SET = 375,
     THEN = 376,
     TO = 377,
     TRY = 378,
     TYPE = 379,
     TYPECASE = 380,
     UNSAFE = 381,
     UNTIL = 382,
     UNTRACED = 383,
     VALUE = 384,
     VAR = 385,
     WHILE = 386,
     WITH = 387,
     BAD = 388,
     WHITESPACE = 389,
     MODUNIT = 390,
     DEFUNIT = 391
   };
#endif
#define ENDOFFILE 0
#define AMPERSAND 258
#define ASSIGN 259
#define ASTERISK 260
#define BAR 261
#define COLON 262
#define COMMA 263
#define DOT 264
#define DOTDOT 265
#define EQUAL 266
#define GREATER 267
#define GREQUAL 268
#define LESS 269
#define LSEQUAL 270
#define MINUS 271
#define SHARP 272
#define PLUS 273
#define RARROW 274
#define RPRAGMA 275
#define RBRACE 276
#define RBRACKET 277
#define RPAREN 278
#define SEMICOLON 279
#define SLASH 280
#define SUBTYPE 281
#define UPARROW 282
#define LPAREN 283
#define LBRACKET 284
#define LBRACE 285
#define IDENT 286
#define CARD_CONST 287
#define REAL_CONST 288
#define CHAR_CONST 289
#define STR_CONST 290
#define PR_EXTERNAL 291
#define PR_INLINE 292
#define PR_OBSOLETE 293
#define PR_UNUSED 294
#define PR_FATAL 295
#define PR_NOWARN 296
#define PR_ASSERT 297
#define PR_TRACE 298
#define PR_LINE 299
#define PR_PRAGMA 300
#define PR_CALLBACK 301
#define PR_LL 302
#define PR_LLsup 303
#define PR_EXPORTED 304
#define PR_SPEC 305
#define PR_LOOPINV 306
#define IDENTPRIME 307
#define UPARROWPRIME 308
#define ALL 309
#define AXIOM 310
#define DEPEND 311
#define ENSURES 312
#define EXISTS 313
#define FUNC 314
#define IFF 315
#define IMPLIES 316
#define INVARIANT 317
#define IS 318
#define LET 319
#define MAP 320
#define MODIFIES 321
#define ON 322
#define PRED 323
#define PROTECT 324
#define ABSTRACT 325
#define REQUIRES 326
#define AND 327
#define ANY 328
#define ARRAY 329
#define AS 330
#define BGN 331
#define BITS 332
#define BRANDED 333
#define BY 334
#define CASE 335
#define CONST 336
#define DIV 337
#define DO 338
#define ELSE 339
#define ELSIF 340
#define END 341
#define EVAL 342
#define EXCEPT 343
#define EXCEPTION 344
#define EXIT 345
#define EXPORTS 346
#define FINALLY 347
#define FOR 348
#define FROM 349
#define GENERIC 350
#define IF 351
#define IMPORT 352
#define IN 353
#define INTERFACE 354
#define LOCK 355
#define LOOP 356
#define METHODS 357
#define MOD 358
#define MODULE 359
#define NOT 360
#define OBJECT 361
#define OF 362
#define OR 363
#define OVERRIDES 364
#define PROCEDURE 365
#define RAISE 366
#define RAISES 367
#define READONLY 368
#define RECORD 369
#define REF 370
#define REPEAT 371
#define RETURN 372
#define REVEAL 373
#define ROOT 374
#define SET 375
#define THEN 376
#define TO 377
#define TRY 378
#define TYPE 379
#define TYPECASE 380
#define UNSAFE 381
#define UNTIL 382
#define UNTRACED 383
#define VALUE 384
#define VAR 385
#define WHILE 386
#define WITH 387
#define BAD 388
#define WHITESPACE 389
#define MODUNIT 390
#define DEFUNIT 391




/* Copy the first part of user declarations.  */
#line 22 "../Parse.yacc"


#define NULL (0L)

#define lexbufsize 500
char lexbuf[2 * lexbufsize];
int lexptr = 0;
int lexposition = 0;
  /* See BufferLexeme and AddLexLength in Parse.lex */

char *infileName = NULL;
  /* initialized by initParser, needed for error message */

int comdepth = 0;
  /* depth of comments, used only by lexer. */

int pragdepth = 0;
  /* depth of pragmas, used only by lexer. */

int depth = 0;
  /* depth of nesting in blocks, used by NPS for formatting comments */

int blanklinep;  
  /* Set by NPS if the non-program-sequence that it parses ends
     with a blank line. */

int calledFromEmacs;
  /* set to one by main if called from Emacs, to zero otherwise */

int capSwitch;
  /* 1 if -cap switch was set, 0 otherwise. */

int callspace;
  /* 1 if -callspace switch was set, 0 otherwise */

char *formatter;
  /* the opaque Formatter.T object */

double offset = 2.0;		
  /* indentation */

int alignDecls = 1;
  /* True if we should use alignment code for declarations. */

int breakType;
  /* Style of optimal breaks to use. */

double commentCol;
  /* Where comments go. */

int comBreakNLs;
  /* how many NLs before HandleComments bails out the first time */

typedef struct {
  char *body;
  char *keyword;
  char *builtinID;
  char *procName;
  char *comment;
  char *fixedComment;
  char *fixed;
} FontInfo;

FontInfo *fonts; /* various opaque fonts */

double fixedCommentSpaceWidth;
#define MAXWIDTH	(1.0E20)
/* Width of a space in various fonts. */
double bodySpaceWidth;
double commentLeaderWidth;

typedef long STYLE;
#define SRC_STYLE 0
#define EM_STYLE  1
STYLE style = SRC_STYLE;

typedef enum {NonOptimal, OptimalBreak, OptimalNoBreak} Formatter_BreakType;



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#ifndef YYSTYPE
typedef int yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif

#ifndef YYLTYPE
typedef struct yyltype
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} yyltype;
# define YYLTYPE yyltype
# define YYLTYPE_IS_TRIVIAL 1
#endif

/* Copy the second part of user declarations.  */


/* Line 213 of /usr/share/bison/yacc.c.  */
#line 447 "y.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYLTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];	\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  5
#define YYLAST   5747

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  137
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  540
/* YYNRULES -- Number of rules. */
#define YYNRULES  840
/* YYNRULES -- Number of states. */
#define YYNSTATES  2121

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   391

#define YYTRANSLATE(X) \
  ((unsigned)(X) <= YYMAXUTOK ? yytranslate[X] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     4,     9,    10,    15,    16,    21,    22,
      25,    27,    30,    32,    34,    37,    49,    51,    54,    56,
      58,    61,    63,    67,    69,    73,    75,    77,    91,   109,
     122,   141,   156,   169,   170,   180,   181,   188,   189,   192,
     204,   216,   218,   226,   231,   242,   254,   267,   268,   271,
     285,   292,   298,   308,   314,   324,   330,   340,   346,   356,
     362,   370,   371,   375,   379,   383,   387,   391,   395,   399,
     402,   406,   427,   445,   448,   452,   463,   474,   477,   481,
     500,   522,   541,   543,   547,   553,   564,   570,   576,   577,
     581,   582,   590,   594,   595,   601,   607,   614,   617,   621,
     635,   649,   650,   654,   655,   658,   661,   664,   675,   690,
     702,   708,   720,   729,   730,   734,   735,   740,   743,   747,
     748,   754,   758,   760,   762,   766,   768,   770,   774,   776,
     778,   780,   782,   784,   786,   788,   790,   792,   794,   796,
     798,   800,   802,   804,   806,   808,   816,   818,   830,   840,
     841,   847,   854,   856,   862,   866,   874,   876,   882,   903,
     905,   911,   922,   923,   927,   928,   931,   939,   948,   954,
     958,   968,   970,   974,   982,   993,  1002,  1003,  1009,  1024,
    1033,  1047,  1059,  1060,  1066,  1073,  1085,  1095,  1108,  1110,
    1117,  1128,  1129,  1131,  1133,  1138,  1140,  1144,  1146,  1152,
    1156,  1162,  1168,  1172,  1179,  1181,  1183,  1185,  1193,  1197,
    1202,  1206,  1208,  1215,  1220,  1229,  1238,  1240,  1244,  1246,
    1254,  1260,  1265,  1271,  1273,  1277,  1285,  1286,  1290,  1291,
    1295,  1296,  1299,  1304,  1305,  1312,  1319,  1327,  1330,  1334,
    1341,  1348,  1349,  1356,  1363,  1371,  1374,  1378,  1389,  1406,
    1422,  1432,  1448,  1463,  1464,  1471,  1478,  1486,  1489,  1493,
    1506,  1518,  1522,  1528,  1535,  1543,  1549,  1556,  1557,  1560,
    1562,  1566,  1574,  1586,  1587,  1590,  1596,  1602,  1604,  1606,
    1610,  1614,  1618,  1622,  1626,  1629,  1632,  1635,  1638,  1641,
    1647,  1651,  1659,  1667,  1669,  1672,  1676,  1682,  1690,  1698,
    1700,  1702,  1704,  1706,  1708,  1710,  1712,  1714,  1716,  1718,
    1724,  1725,  1728,  1729,  1732,  1733,  1736,  1738,  1743,  1747,
    1754,  1760,  1770,  1778,  1786,  1795,  1796,  1800,  1811,  1819,
    1830,  1834,  1842,  1846,  1854,  1856,  1858,  1862,  1864,  1872,
    1880,  1884,  1886,  1892,  1894,  1896,  1900,  1902,  1908,  1912,
    1914,  1920,  1924,  1926,  1930,  1938,  1942,  1946,  1948,  1954,
    1956,  1958,  1962,  1964,  1970,  1972,  1974,  1976,  1978,  1983,
    1989,  1995,  1999,  2003,  2005,  2010,  2013,  2019,  2021,  2023,
    2026,  2032,  2037,  2039,  2044,  2052,  2054,  2056,  2058,  2060,
    2062,  2064,  2068,  2071,  2077,  2080,  2081,  2086,  2090,  2096,
    2106,  2112,  2121,  2123,  2129,  2131,  2135,  2138,  2144,  2146,
    2151,  2154,  2155,  2159,  2161,  2163,  2167,  2169,  2175,  2179,
    2181,  2187,  2191,  2193,  2197,  2199,  2205,  2207,  2209,  2211,
    2213,  2215,  2217,  2219,  2223,  2225,  2231,  2233,  2235,  2237,
    2241,  2243,  2249,  2251,  2253,  2255,  2257,  2259,  2263,  2267,
    2270,  2272,  2274,  2276,  2278,  2280,  2285,  2287,  2288,  2292,
    2295,  2297,  2306,  2315,  2320,  2330,  2335,  2339,  2341,  2347,
    2351,  2353,  2359,  2363,  2365,  2369,  2371,  2377,  2381,  2383,
    2389,  2393,  2395,  2401,  2403,  2407,  2411,  2413,  2416,  2420,
    2424,  2426,  2428,  2432,  2438,  2440,  2442,  2444,  2446,  2448,
    2450,  2454,  2457,  2459,  2468,  2477,  2482,  2492,  2497,  2507,
    2512,  2516,  2520,  2522,  2528,  2533,  2535,  2537,  2542,  2545,
    2548,  2554,  2557,  2563,  2567,  2585,  2588,  2594,  2596,  2602,
    2608,  2609,  2614,  2615,  2617,  2619,  2624,  2625,  2629,  2630,
    2634,  2635,  2639,  2640,  2644,  2645,  2649,  2650,  2654,  2655,
    2659,  2660,  2664,  2665,  2669,  2670,  2674,  2675,  2679,  2680,
    2684,  2685,  2689,  2690,  2694,  2695,  2699,  2700,  2704,  2705,
    2709,  2710,  2714,  2715,  2719,  2720,  2724,  2725,  2729,  2731,
    2732,  2736,  2738,  2739,  2743,  2744,  2748,  2749,  2753,  2754,
    2758,  2759,  2763,  2764,  2768,  2769,  2773,  2774,  2778,  2779,
    2783,  2784,  2788,  2789,  2793,  2794,  2798,  2799,  2803,  2804,
    2808,  2809,  2813,  2814,  2818,  2819,  2823,  2824,  2828,  2829,
    2833,  2834,  2838,  2839,  2843,  2844,  2848,  2849,  2853,  2854,
    2858,  2859,  2863,  2864,  2868,  2869,  2873,  2874,  2878,  2879,
    2883,  2884,  2888,  2889,  2893,  2894,  2898,  2899,  2903,  2904,
    2908,  2909,  2913,  2914,  2918,  2919,  2923,  2924,  2928,  2929,
    2933,  2934,  2938,  2939,  2943,  2944,  2948,  2949,  2953,  2954,
    2958,  2959,  2963,  2964,  2968,  2969,  2973,  2974,  2978,  2979,
    2983,  2984,  2988,  2989,  2993,  2994,  2998,  2999,  3003,  3004,
    3008,  3009,  3013,  3014,  3018,  3019,  3023,  3024,  3028,  3029,
    3033,  3034,  3038,  3039,  3043,  3044,  3048,  3049,  3053,  3054,
    3058,  3059,  3063,  3064,  3068,  3069,  3073,  3074,  3078,  3079,
    3083,  3084,  3088,  3089,  3093,  3094,  3098,  3099,  3103,  3104,
    3108,  3109,  3113,  3114,  3118,  3119,  3123,  3124,  3128,  3129,
    3133,  3134,  3138,  3139,  3143,  3144,  3148,  3149,  3153,  3154,
    3158,  3159,  3163,  3164,  3168,  3169,  3173,  3174,  3178,  3179,
    3183,  3184,  3188,  3189,  3193,  3194,  3198,  3199,  3203,  3204,
    3208,  3209,  3213,  3214,  3218,  3219,  3223,  3224,  3228,  3229,
    3233,  3234,  3238,  3239,  3243,  3244,  3248,  3249,  3253,  3254,
    3258,  3259,  3263,  3264,  3268,  3269,  3273,  3274,  3278,  3279,
    3283,  3285,  3286,  3290,  3291,  3293,  3295,  3297,  3300,  3302,
    3305,  3307,  3310,  3312,  3314,  3317,  3318,  3319,  3320,  3321,
    3322,  3323,  3324,  3325,  3326,  3327,  3328,  3329,  3330,  3331,
    3332,  3333,  3334,  3335,  3336,  3337,  3338,  3339,  3340,  3341,
    3342
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short yyrhs[] =
{
     138,     0,    -1,    -1,   139,   142,   147,   674,    -1,    -1,
     140,   135,   142,   143,    -1,    -1,   141,   136,   142,   145,
      -1,    -1,   643,   674,    -1,   144,    -1,   143,   144,    -1,
     161,    -1,   155,    -1,   147,   674,    -1,   653,   675,   254,
     183,   661,   676,   515,   664,   473,   655,   674,    -1,   146,
      -1,   145,   146,    -1,   161,    -1,   155,    -1,   147,   674,
      -1,   148,    -1,   595,   664,   148,    -1,   149,    -1,   595,
     664,   149,    -1,   150,    -1,   151,    -1,   541,   664,   653,
     475,   422,   655,   674,   154,   160,   515,   664,   473,   391,
      -1,   541,   664,   653,   475,   664,   395,   658,   653,   473,
     152,   655,   655,   674,   515,   664,   473,   391,    -1,   551,
     664,   653,   475,   153,   422,   655,   674,   154,   159,   663,
     391,    -1,   551,   664,   653,   475,   153,   664,   395,   658,
     653,   473,   152,   655,   655,   674,   515,   664,   473,   391,
      -1,   533,   664,   541,   664,   475,   152,   422,   674,   154,
     160,   515,   664,   473,   391,    -1,   533,   664,   551,   664,
     475,   152,   422,   674,   154,   159,   663,   391,    -1,    -1,
     663,   673,   433,   659,   652,   377,   655,   663,   417,    -1,
      -1,   658,   525,   664,   652,   378,   655,    -1,    -1,   154,
     155,    -1,   531,   664,   473,   664,   537,   664,   652,   378,
     655,   422,   674,    -1,   653,   537,   657,   669,   156,   422,
     655,   655,   670,   655,   674,    -1,   157,    -1,   156,   389,
     665,   655,   655,   671,   157,    -1,   675,   651,   651,   473,
      -1,   675,   651,   651,   473,   656,   651,   664,   493,   664,
     227,    -1,   160,   653,   162,   675,   254,   183,   674,   676,
     515,   663,   655,    -1,   160,   653,   162,   675,   254,   183,
     674,   676,   515,   664,   473,   655,    -1,    -1,   160,   161,
      -1,   653,   162,   171,   664,   395,   666,   652,   159,   663,
     422,   655,   655,   666,    -1,   653,   162,   171,   422,   655,
     666,    -1,   653,   162,   505,   655,   666,    -1,   653,   162,
     505,   660,   668,   163,   670,   655,   666,    -1,   653,   162,
     591,   655,   666,    -1,   653,   162,   591,   660,   667,   165,
     670,   655,   666,    -1,   653,   162,   579,   655,   666,    -1,
     653,   162,   579,   660,   667,   165,   670,   655,   666,    -1,
     653,   162,   603,   655,   666,    -1,   653,   162,   603,   660,
     668,   167,   670,   655,   666,    -1,   653,   162,   521,   655,
     666,    -1,   653,   162,   521,   660,   169,   655,   666,    -1,
      -1,   162,   250,   674,    -1,   162,   260,   674,    -1,   162,
     263,   674,    -1,   162,   262,   674,    -1,   162,   261,   674,
      -1,   162,   258,   674,    -1,   162,   264,   674,    -1,   164,
     671,    -1,   163,   164,   671,    -1,   675,   651,   651,   473,
     656,   651,   387,   664,   227,   664,   656,   651,   395,   664,
     330,   663,   676,   422,   655,   655,    -1,   675,   651,   651,
     473,   656,   651,   664,   656,   651,   395,   664,   330,   663,
     676,   422,   655,   655,    -1,   166,   671,    -1,   165,   166,
     671,    -1,   651,   653,   228,   664,   395,   657,   227,   422,
     655,   655,    -1,   651,   653,   228,   664,   427,   657,   227,
     422,   655,   655,    -1,   168,   671,    -1,   167,   168,   671,
      -1,   675,   651,   651,   378,   656,   651,   387,   664,   227,
     663,   253,   676,   424,   655,   651,   645,   655,   655,    -1,
     675,   651,   651,   378,   656,   651,   387,   664,   227,   664,
     656,   651,   381,   664,   330,   663,   253,   676,   422,   655,
     655,    -1,   675,   651,   651,   378,   656,   651,   664,   656,
     651,   381,   664,   330,   663,   253,   676,   422,   655,   655,
      -1,   170,    -1,   169,   666,   170,    -1,   675,   473,   663,
     676,   422,    -1,   675,   473,   433,   659,   227,   663,   417,
     663,   676,   422,    -1,   563,   664,   475,   664,   172,    -1,
     435,   175,   417,   173,   174,    -1,    -1,   387,   657,   227,
      -1,    -1,   567,   664,   439,   652,   223,   655,   413,    -1,
     567,   664,   489,    -1,    -1,   653,   669,   178,   670,   655,
      -1,   653,   669,   176,   670,   655,    -1,   653,   669,   176,
     178,   670,   655,    -1,   177,   671,    -1,   176,   177,   671,
      -1,   651,   653,   179,   656,   653,   180,   656,   653,   378,
     656,   181,   253,   655,    -1,   651,   653,   179,   656,   653,
     180,   656,   653,   378,   656,   182,   253,   655,    -1,    -1,
     179,   261,   657,    -1,    -1,   601,   664,    -1,   603,   664,
      -1,   569,   664,    -1,   653,   387,   664,   227,   424,   665,
     655,   651,   645,   655,    -1,   653,   387,   664,   227,   656,
     653,   664,   381,   664,   330,   663,   422,   665,   655,    -1,
     651,   655,   653,   664,   381,   664,   330,   663,   422,   665,
     655,    -1,   653,   387,   664,   227,   655,    -1,   653,   387,
     664,   227,   656,   653,   664,   381,   664,   330,   655,    -1,
     651,   655,   653,   664,   381,   664,   330,   655,    -1,    -1,
     660,   186,   655,    -1,    -1,   660,   651,   185,   655,    -1,
     186,   655,    -1,   187,   653,   189,    -1,    -1,   187,   653,
     188,   655,   660,    -1,   190,   663,   422,    -1,   191,    -1,
     190,    -1,   190,   663,   422,    -1,   191,    -1,   192,    -1,
     652,   158,   655,    -1,   193,    -1,   194,    -1,   199,    -1,
     200,    -1,   201,    -1,   203,    -1,   207,    -1,   208,    -1,
     209,    -1,   210,    -1,   211,    -1,   212,    -1,   213,    -1,
     216,    -1,   219,    -1,   220,    -1,   255,    -1,   330,   664,
     381,   658,   653,   330,   655,    -1,   330,    -1,   503,   664,
     330,   664,   557,   662,   196,   195,   204,   672,   515,    -1,
     503,   664,   330,   664,   557,   195,   204,   672,   515,    -1,
      -1,   195,   661,   385,   664,   196,    -1,   653,   197,   664,
     411,   184,   655,    -1,   198,    -1,   197,   663,   389,   657,
     198,    -1,   653,   330,   655,    -1,   653,   330,   664,   393,
     660,   330,   655,    -1,   523,    -1,   517,   658,   653,   330,
     655,    -1,   654,   529,   664,   473,   253,   657,   381,   664,
     330,   657,   587,   664,   330,   202,   509,   655,   256,   183,
     672,   515,    -1,   664,    -1,   657,   501,   664,   330,   664,
      -1,   535,   664,   330,   664,   585,   183,   205,   204,   672,
     515,    -1,    -1,   661,   511,   183,    -1,    -1,   205,   206,
      -1,   661,   513,   664,   330,   664,   585,   183,    -1,   543,
     664,   330,   664,   509,   183,   672,   515,    -1,   545,   256,
     183,   672,   515,    -1,   565,   658,   330,    -1,   575,   256,
     183,   661,   653,   597,   657,   330,   655,    -1,   577,    -1,
     577,   658,   330,    -1,   589,   183,   661,   527,   183,   672,
     515,    -1,   589,   183,   661,   519,   662,   215,   214,   204,
     672,   515,    -1,   589,   183,   661,   519,   214,   204,   672,
     515,    -1,    -1,   214,   661,   385,   664,   215,    -1,   653,
     654,   224,   657,   433,   473,   253,   663,   417,   664,   411,
     655,   184,   655,    -1,   653,   654,   224,   664,   411,   655,
     184,   655,    -1,   654,   593,   657,   330,   657,   557,   655,
     662,   218,   217,   204,   672,   515,    -1,   654,   593,   657,
     330,   657,   557,   655,   217,   204,   672,   515,    -1,    -1,
     217,   661,   385,   664,   218,    -1,   653,   226,   664,   411,
     184,   655,    -1,   653,   226,   664,   433,   473,   253,   417,
     664,   411,   184,   655,    -1,   605,   664,   330,   664,   509,
     256,   183,   672,   515,    -1,   607,   664,   667,   221,   655,
     655,   670,   664,   509,   183,   672,   515,    -1,   222,    -1,
     221,   389,   655,   655,   671,   222,    -1,   651,   651,   473,
     253,   664,   655,   651,   395,   664,   330,    -1,    -1,   224,
      -1,   225,    -1,   224,   389,   657,   225,    -1,   473,    -1,
     473,   391,   473,    -1,   227,    -1,   226,   663,   389,   657,
     227,    -1,   653,   228,   655,    -1,   653,   228,   664,   233,
     655,    -1,   653,   231,   664,   233,   655,    -1,   653,   229,
     655,    -1,   653,   433,   227,   663,   417,   655,    -1,   225,
      -1,   230,    -1,   232,    -1,   497,   657,   330,   657,   529,
     660,   227,    -1,   563,   658,   172,    -1,   263,   563,   658,
     172,    -1,   599,   664,   233,    -1,   233,    -1,   599,   664,
     237,   573,   657,   227,    -1,   237,   573,   657,   227,    -1,
     652,   439,   652,   377,   655,   663,   413,   655,    -1,   437,
     330,   664,   393,   660,   330,   663,   415,    -1,   231,    -1,
     599,   664,   581,    -1,   581,    -1,   491,   657,   226,   664,
     557,   660,   227,    -1,   491,   664,   557,   660,   227,    -1,
     571,   238,   672,   515,    -1,   583,   664,   557,   660,   227,
      -1,   234,    -1,   233,   660,   234,    -1,   237,   555,   238,
     235,   236,   672,   515,    -1,    -1,   661,   547,   242,    -1,
      -1,   661,   561,   246,    -1,    -1,   499,   664,    -1,   499,
     664,   366,   664,    -1,    -1,   660,   651,   668,   241,   670,
     655,    -1,   660,   651,   668,   239,   670,   655,    -1,   660,
     651,   668,   239,   241,   670,   655,    -1,   240,   671,    -1,
     239,   240,   671,    -1,   651,   653,   378,   655,   181,   655,
      -1,   651,   653,   378,   655,   182,   655,    -1,    -1,   660,
     651,   668,   245,   670,   655,    -1,   660,   651,   668,   243,
     670,   655,    -1,   660,   651,   668,   243,   245,   670,   655,
      -1,   244,   671,    -1,   243,   244,   671,    -1,   651,   653,
     473,   656,   653,   664,   172,   422,   655,   655,    -1,   651,
     653,   473,   656,   653,   664,   172,   656,   653,   664,   381,
     664,   225,   422,   655,   655,    -1,   651,   653,   473,   656,
     653,   664,   656,   653,   664,   381,   664,   225,   422,   655,
     655,    -1,   651,   653,   473,   656,   653,   664,   172,   655,
     655,    -1,   651,   653,   473,   656,   653,   664,   172,   656,
     653,   664,   381,   664,   225,   655,   655,    -1,   651,   653,
     473,   656,   653,   664,   656,   653,   664,   381,   664,   225,
     655,   655,    -1,    -1,   660,   651,   667,   249,   670,   655,
      -1,   660,   651,   667,   247,   670,   655,    -1,   660,   651,
     667,   247,   249,   670,   655,    -1,   248,   671,    -1,   247,
     248,   671,    -1,   651,   653,   473,   656,   653,   664,   381,
     664,   225,   422,   655,   655,    -1,   651,   653,   473,   656,
     653,   664,   381,   664,   225,   655,   655,    -1,   441,   664,
     419,    -1,   441,   664,   366,   664,   419,    -1,   441,   664,
     387,   473,   664,   419,    -1,   441,   664,   366,   387,   473,
     664,   419,    -1,   447,   664,   330,   664,   419,    -1,   653,
     447,   183,   655,   664,   419,    -1,    -1,   664,   251,    -1,
     495,    -1,   495,   660,   252,    -1,   445,   664,   653,   330,
     655,   664,   419,    -1,   445,   664,   653,   330,   663,   389,
     657,   366,   655,   664,   419,    -1,    -1,   660,   257,    -1,
     471,   664,   290,   664,   419,    -1,   449,   664,   259,   664,
     419,    -1,   224,    -1,   489,    -1,   443,   664,   419,    -1,
     451,   664,   419,    -1,   453,   664,   419,    -1,   455,   664,
     419,    -1,   457,   664,   419,    -1,   661,   266,    -1,   657,
     267,    -1,   657,   270,    -1,   661,   268,    -1,   661,   271,
      -1,   459,   664,   378,   664,   421,    -1,   461,   664,   421,
      -1,   465,   664,   337,   664,   269,   664,   421,    -1,   467,
     664,   337,   664,   269,   664,   421,    -1,   330,    -1,   439,
     413,    -1,   439,   371,   413,    -1,   463,   664,   479,   664,
     421,    -1,   463,   664,   479,   664,   485,   664,   421,    -1,
     469,   664,   653,   272,   655,   657,   421,    -1,   273,    -1,
     278,    -1,   280,    -1,   279,    -1,   283,    -1,   284,    -1,
     285,    -1,   286,    -1,   287,    -1,   288,    -1,   277,   674,
     274,   275,   276,    -1,    -1,   317,   674,    -1,    -1,   321,
     674,    -1,    -1,   322,   674,    -1,   325,    -1,   325,   433,
     378,   417,    -1,   325,   387,   315,    -1,   325,   433,   378,
     417,   387,   315,    -1,   603,   664,   653,   313,   655,    -1,
     615,   664,   325,   282,   387,   657,   653,   311,   655,    -1,
     609,   664,   281,   657,   623,   664,   290,    -1,   609,   664,
     281,   657,   395,   664,   330,    -1,   325,   282,   387,   657,
     325,   437,   325,   415,    -1,    -1,   437,   314,   415,    -1,
     637,   664,   473,   433,   313,   417,   657,   629,   664,   290,
      -1,   621,   664,   473,   663,   387,   657,   315,    -1,   621,
     664,   473,   433,   313,   417,   663,   387,   657,   315,    -1,
     613,   664,   290,    -1,   639,   664,   326,   657,   501,   664,
     311,    -1,   627,   664,   290,    -1,   631,   664,   473,   657,
     381,   664,   289,    -1,   290,    -1,   291,    -1,   653,   292,
     655,    -1,   293,    -1,   611,   664,   437,   313,   415,   657,
     293,    -1,   619,   664,   437,   313,   415,   657,   293,    -1,
     653,   294,   655,    -1,   296,    -1,   296,   664,   295,   657,
     293,    -1,   625,    -1,   623,    -1,   653,   297,   655,    -1,
     298,    -1,   297,   657,   559,   664,   298,    -1,   653,   299,
     655,    -1,   300,    -1,   299,   657,   487,   664,   300,    -1,
     653,   301,   655,    -1,   302,    -1,   553,   664,   301,    -1,
     653,   303,   657,   316,   664,   303,   655,    -1,   653,   303,
     655,    -1,   653,   304,   655,    -1,   306,    -1,   304,   657,
     305,   664,   306,    -1,   409,    -1,   405,    -1,   653,   307,
     655,    -1,   309,    -1,   307,   657,   308,   664,   309,    -1,
     383,    -1,   507,    -1,   549,    -1,   310,    -1,   325,   663,
     433,   417,    -1,   325,   663,   433,   311,   417,    -1,   309,
     663,   437,   311,   415,    -1,   309,   663,   429,    -1,   309,
     663,   431,    -1,   312,    -1,   433,   289,   663,   417,    -1,
     663,   289,    -1,   311,   663,   389,   657,   289,    -1,   479,
      -1,   327,    -1,   663,   314,    -1,   313,   663,   389,   657,
     314,    -1,   378,   387,   664,   315,    -1,   325,    -1,   325,
     437,   315,   415,    -1,   633,   664,   315,   664,   587,   664,
     315,    -1,   401,    -1,   397,    -1,   403,    -1,   399,    -1,
     395,    -1,   407,    -1,   635,   664,   318,    -1,   663,   319,
      -1,   318,   663,   389,   657,   319,    -1,   325,   320,    -1,
      -1,   320,   437,   289,   415,    -1,   641,   664,   290,    -1,
     617,   664,   653,   290,   655,    -1,   617,   664,   653,   290,
     664,   519,   664,   324,   655,    -1,   325,   664,   411,   664,
     290,    -1,   325,   433,   473,   417,   664,   411,   664,   290,
      -1,   323,    -1,   324,   664,   385,   664,   323,    -1,   473,
      -1,   325,   391,   473,    -1,   663,   325,    -1,   326,   663,
     389,   657,   325,    -1,   325,    -1,   325,   391,   477,   328,
      -1,   477,   328,    -1,    -1,   328,   391,   329,    -1,   473,
      -1,   477,    -1,   653,   331,   655,    -1,   332,    -1,   331,
     657,   559,   664,   332,    -1,   653,   333,   655,    -1,   334,
      -1,   333,   657,   487,   664,   334,    -1,   553,   664,   334,
      -1,   335,    -1,   653,   336,   655,    -1,   338,    -1,   336,
     657,   337,   664,   338,    -1,   395,    -1,   407,    -1,   401,
      -1,   403,    -1,   397,    -1,   399,    -1,   539,    -1,   653,
     339,   655,    -1,   341,    -1,   339,   657,   340,   664,   341,
      -1,   409,    -1,   405,    -1,   379,    -1,   653,   342,   655,
      -1,   344,    -1,   342,   657,   343,   664,   344,    -1,   383,
      -1,   425,    -1,   507,    -1,   549,    -1,   345,    -1,   409,
     663,   344,    -1,   405,   663,   344,    -1,   346,   347,    -1,
     473,    -1,   479,    -1,   481,    -1,   483,    -1,   485,    -1,
     433,   330,   663,   417,    -1,   232,    -1,    -1,   347,   663,
     348,    -1,   391,   473,    -1,   429,    -1,   673,   437,   659,
     652,   371,   655,   663,   415,    -1,   673,   433,   659,   652,
     372,   655,   663,   417,    -1,   673,   433,   663,   417,    -1,
     673,   439,   659,   652,   374,   376,   655,   663,   413,    -1,
     673,   439,   663,   413,    -1,   653,   350,   655,    -1,   351,
      -1,   350,   657,   559,   664,   332,    -1,   653,   352,   655,
      -1,   353,    -1,   352,   657,   487,   664,   334,    -1,   553,
     664,   334,    -1,   354,    -1,   653,   355,   655,    -1,   356,
      -1,   355,   657,   337,   664,   338,    -1,   653,   357,   655,
      -1,   358,    -1,   357,   657,   340,   664,   341,    -1,   653,
     359,   655,    -1,   360,    -1,   359,   657,   343,   664,   344,
      -1,   361,    -1,   409,   663,   344,    -1,   405,   663,   344,
      -1,   228,    -1,   228,   363,    -1,   228,   664,   233,    -1,
     231,   664,   233,    -1,   230,    -1,   232,    -1,   232,   365,
     347,    -1,   433,   349,   663,   417,   347,    -1,   362,    -1,
     479,    -1,   481,    -1,   483,    -1,   485,    -1,   364,    -1,
     363,   663,   348,    -1,   391,   473,    -1,   429,    -1,   673,
     437,   659,   652,   371,   655,   663,   415,    -1,   673,   433,
     659,   652,   372,   655,   663,   417,    -1,   673,   433,   663,
     417,    -1,   673,   439,   659,   652,   374,   376,   655,   663,
     413,    -1,   673,   439,   663,   413,    -1,   673,   439,   659,
     652,   374,   376,   655,   663,   413,    -1,   673,   439,   663,
     413,    -1,   653,   367,   655,    -1,   653,   368,   655,    -1,
     369,    -1,   368,   657,   379,   664,   369,    -1,   653,   370,
     347,   655,    -1,   473,    -1,   485,    -1,   433,   366,   663,
     417,    -1,   232,   365,    -1,   663,   330,    -1,   371,   663,
     389,   657,   330,    -1,   663,   373,    -1,   372,   663,   389,
     657,   373,    -1,   651,   349,   655,    -1,   651,   653,   653,
     653,   653,   653,   473,   664,   381,   657,   330,   655,   655,
     655,   655,   655,   655,    -1,   663,   375,    -1,   374,   663,
     389,   657,   375,    -1,   330,    -1,   330,   663,   393,   657,
     330,    -1,   330,   664,   381,   657,   330,    -1,    -1,   663,
     389,   657,   393,    -1,    -1,   378,    -1,   473,    -1,   378,
     389,   657,   473,    -1,    -1,     3,   380,   645,    -1,    -1,
       4,   382,   645,    -1,    -1,     5,   384,   645,    -1,    -1,
       6,   386,   645,    -1,    -1,     7,   388,   645,    -1,    -1,
       8,   390,   645,    -1,    -1,     9,   392,   645,    -1,    -1,
      10,   394,   645,    -1,    -1,    11,   396,   645,    -1,    -1,
      12,   398,   645,    -1,    -1,    13,   400,   645,    -1,    -1,
      14,   402,   645,    -1,    -1,    15,   404,   645,    -1,    -1,
      16,   406,   645,    -1,    -1,    17,   408,   645,    -1,    -1,
      18,   410,   645,    -1,    -1,    19,   412,   645,    -1,    -1,
      21,   414,   645,    -1,    -1,    22,   416,   645,    -1,    -1,
      23,   418,   645,    -1,    -1,    20,   420,   645,    -1,    20,
      -1,    -1,    24,   423,   645,    -1,    24,    -1,    -1,    25,
     426,   645,    -1,    -1,    26,   428,   645,    -1,    -1,    27,
     430,   645,    -1,    -1,    53,   432,   645,    -1,    -1,    28,
     434,   645,    -1,    -1,    28,   436,   645,    -1,    -1,    29,
     438,   645,    -1,    -1,    30,   440,   645,    -1,    -1,    36,
     442,   645,    -1,    -1,    37,   444,   645,    -1,    -1,    42,
     446,   645,    -1,    -1,    43,   448,   645,    -1,    -1,    40,
     450,   645,    -1,    -1,    39,   452,   645,    -1,    -1,    38,
     454,   645,    -1,    -1,    46,   456,   645,    -1,    -1,    49,
     458,   645,    -1,    -1,    45,   460,   645,    -1,    -1,    41,
     462,   645,    -1,    -1,    44,   464,   645,    -1,    -1,    47,
     466,   645,    -1,    -1,    48,   468,   645,    -1,    -1,    50,
     470,   645,    -1,    -1,    51,   472,   645,    -1,    -1,    31,
     474,   645,    -1,    -1,    31,   476,   645,    -1,    -1,    52,
     478,   645,    -1,    -1,    32,   480,   645,    -1,    -1,    33,
     482,   645,    -1,    -1,    34,   484,   645,    -1,    -1,    35,
     486,   645,    -1,    -1,    72,   488,   645,    -1,    -1,    73,
     490,   645,    -1,    -1,    74,   492,   645,    -1,    -1,    75,
     494,   645,    -1,    -1,    76,   496,   645,    -1,    -1,    77,
     498,   645,    -1,    -1,    78,   500,   645,    -1,    -1,    79,
     502,   645,    -1,    -1,    80,   504,   645,    -1,    -1,    81,
     506,   645,    -1,    -1,    82,   508,   645,    -1,    -1,    83,
     510,   645,    -1,    -1,    84,   512,   645,    -1,    -1,    85,
     514,   645,    -1,    -1,    86,   516,   645,    -1,    -1,    87,
     518,   645,    -1,    -1,    88,   520,   645,    -1,    -1,    89,
     522,   645,    -1,    -1,    90,   524,   645,    -1,    -1,    91,
     526,   645,    -1,    -1,    92,   528,   645,    -1,    -1,    93,
     530,   645,    -1,    -1,    94,   532,   645,    -1,    -1,    95,
     534,   645,    -1,    -1,    96,   536,   645,    -1,    -1,    97,
     538,   645,    -1,    -1,    98,   540,   645,    -1,    -1,    99,
     542,   645,    -1,    -1,   100,   544,   645,    -1,    -1,   101,
     546,   645,    -1,    -1,   102,   548,   645,    -1,    -1,   103,
     550,   645,    -1,    -1,   104,   552,   645,    -1,    -1,   105,
     554,   645,    -1,    -1,   106,   556,   645,    -1,    -1,   107,
     558,   645,    -1,    -1,   108,   560,   645,    -1,    -1,   109,
     562,   645,    -1,    -1,   110,   564,   645,    -1,    -1,   111,
     566,   645,    -1,    -1,   112,   568,   645,    -1,    -1,   113,
     570,   645,    -1,    -1,   114,   572,   645,    -1,    -1,   115,
     574,   645,    -1,    -1,   116,   576,   645,    -1,    -1,   117,
     578,   645,    -1,    -1,   118,   580,   645,    -1,    -1,   119,
     582,   645,    -1,    -1,   120,   584,   645,    -1,    -1,   121,
     586,   645,    -1,    -1,   122,   588,   645,    -1,    -1,   123,
     590,   645,    -1,    -1,   124,   592,   645,    -1,    -1,   125,
     594,   645,    -1,    -1,   126,   596,   645,    -1,    -1,   127,
     598,   645,    -1,    -1,   128,   600,   645,    -1,    -1,   129,
     602,   645,    -1,    -1,   130,   604,   645,    -1,    -1,   131,
     606,   645,    -1,    -1,   132,   608,   645,    -1,    -1,    70,
     610,   645,    -1,    -1,    54,   612,   645,    -1,    -1,    55,
     614,   645,    -1,    -1,    56,   616,   645,    -1,    -1,    57,
     618,   645,    -1,    -1,    58,   620,   645,    -1,    -1,    59,
     622,   645,    -1,    -1,    60,   624,   645,    -1,    -1,    61,
     626,   645,    -1,    -1,    62,   628,   645,    -1,    -1,    63,
     630,   645,    -1,    -1,    64,   632,   645,    -1,    -1,    65,
     634,   645,    -1,    -1,    66,   636,   645,    -1,    -1,    68,
     638,   645,    -1,    -1,    69,   640,   645,    -1,    -1,    71,
     642,   645,    -1,   650,    -1,    -1,   650,   644,   647,    -1,
      -1,   646,    -1,   647,    -1,   649,    -1,   649,   647,    -1,
     648,    -1,   648,   646,    -1,   265,    -1,   648,   265,    -1,
     650,    -1,   134,    -1,   650,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   157,   157,   156,   158,   158,   159,   159,   162,   164,
     167,   169,   172,   174,   175,   176,   179,   181,   184,   186,
     187,   190,   192,   194,   195,   196,   197,   200,   205,   209,
     213,   217,   224,   231,   233,   236,   238,   241,   243,   246,
     248,   251,   253,   256,   258,   261,   265,   269,   271,   275,
     277,   278,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   290,   292,   293,   294,   295,   296,   297,   298,   301,
     303,   306,   309,   312,   314,   317,   319,   322,   324,   327,
     331,   334,   339,   342,   345,   348,   351,   355,   359,   361,
     364,   366,   367,   370,   372,   373,   374,   377,   379,   382,
     386,   390,   392,   395,   397,   398,   399,   402,   404,   405,
     408,   410,   411,   416,   418,   422,   424,   428,   432,   436,
     438,   441,   443,   446,   448,   449,   452,   454,   455,   456,
     457,   458,   459,   460,   461,   462,   463,   464,   465,   466,
     467,   468,   469,   470,   473,   477,   482,   486,   488,   491,
     493,   496,   500,   502,   505,   507,   510,   514,   519,   525,
     527,   530,   534,   536,   539,   541,   544,   548,   552,   556,
     560,   564,   566,   569,   573,   575,   578,   580,   583,   585,
     588,   590,   593,   595,   598,   600,   603,   607,   611,   613,
     616,   620,   622,   625,   627,   630,   632,   637,   639,   642,
     644,   645,   646,   647,   650,   654,   656,   659,   661,   662,
     663,   664,   665,   666,   667,   668,   669,   672,   674,   678,
     680,   681,   682,   685,   687,   690,   694,   696,   699,   701,
     704,   706,   707,   710,   712,   713,   714,   717,   719,   722,
     725,   729,   731,   732,   733,   736,   738,   741,   743,   744,
     747,   749,   750,   753,   755,   756,   757,   760,   762,   765,
     769,   775,   777,   778,   779,   782,   783,   785,   787,   790,
     792,   795,   797,   800,   802,   805,   809,   811,   813,   816,
     817,   818,   819,   820,   825,   827,   828,   829,   830,   834,
     835,   836,   838,   841,   843,   844,   847,   849,   852,   857,
     859,   860,   861,   862,   863,   864,   865,   866,   867,   870,
     877,   879,   882,   884,   887,   889,   892,   894,   895,   896,
     899,   901,   904,   906,   909,   911,   913,   916,   918,   920,
     923,   925,   927,   929,   934,   936,   938,   939,   941,   942,
     945,   946,   948,   951,   951,   953,   954,   956,   959,   960,
     962,   965,   966,   968,   971,   973,   976,   977,   979,   981,
     981,   983,   984,   986,   988,   988,   988,   990,   992,   993,
     994,   995,   996,   999,  1001,  1004,  1006,  1009,  1011,  1015,
    1017,  1020,  1022,  1024,  1025,  1028,  1028,  1028,  1028,  1028,
    1028,  1030,  1032,  1034,  1037,  1039,  1041,  1043,  1045,  1047,
    1050,  1052,  1055,  1057,  1060,  1062,  1065,  1067,  1070,  1072,
    1073,  1076,  1078,  1083,  1083,  1088,  1089,  1089,  1091,  1092,
    1092,  1094,  1094,  1096,  1097,  1097,  1098,  1098,  1098,  1098,
    1098,  1098,  1098,  1100,  1101,  1101,  1102,  1102,  1102,  1104,
    1105,  1105,  1106,  1106,  1106,  1106,  1108,  1108,  1108,  1111,
    1113,  1113,  1113,  1113,  1113,  1114,  1114,  1116,  1118,  1121,
    1123,  1126,  1127,  1128,  1129,  1130,  1136,  1137,  1137,  1139,
    1140,  1140,  1142,  1142,  1144,  1145,  1145,  1147,  1148,  1148,
    1150,  1151,  1151,  1153,  1153,  1153,  1155,  1156,  1157,  1158,
    1159,  1160,  1161,  1162,  1163,  1166,  1166,  1166,  1166,  1168,
    1170,  1173,  1175,  1178,  1179,  1180,  1181,  1182,  1185,  1187,
    1193,  1195,  1196,  1196,  1198,  1200,  1201,  1202,  1203,  1208,
    1210,  1213,  1215,  1218,  1220,  1223,  1225,  1228,  1230,  1231,
    1234,  1236,  1239,  1241,  1244,  1246,  1252,  1252,  1253,  1253,
    1254,  1254,  1255,  1255,  1256,  1256,  1257,  1257,  1258,  1258,
    1259,  1259,  1260,  1260,  1261,  1261,  1262,  1262,  1263,  1263,
    1264,  1264,  1265,  1265,  1266,  1266,  1267,  1267,  1268,  1268,
    1269,  1269,  1270,  1270,  1271,  1271,  1272,  1272,  1273,  1274,
    1274,  1275,  1276,  1276,  1277,  1277,  1278,  1278,  1279,  1279,
    1282,  1282,  1283,  1283,  1284,  1284,  1285,  1285,  1291,  1291,
    1292,  1292,  1293,  1293,  1294,  1294,  1295,  1295,  1296,  1296,
    1297,  1297,  1298,  1298,  1299,  1299,  1301,  1301,  1302,  1302,
    1303,  1303,  1304,  1304,  1305,  1305,  1306,  1306,  1307,  1307,
    1309,  1309,  1310,  1310,  1311,  1311,  1312,  1312,  1313,  1313,
    1314,  1314,  1315,  1315,  1317,  1317,  1318,  1318,  1319,  1319,
    1320,  1320,  1321,  1321,  1322,  1322,  1323,  1323,  1324,  1324,
    1325,  1325,  1326,  1326,  1327,  1327,  1328,  1328,  1329,  1329,
    1330,  1330,  1331,  1331,  1332,  1332,  1333,  1333,  1334,  1334,
    1335,  1335,  1336,  1336,  1337,  1337,  1338,  1338,  1339,  1339,
    1340,  1340,  1341,  1341,  1342,  1342,  1343,  1343,  1344,  1344,
    1345,  1345,  1346,  1346,  1347,  1347,  1348,  1348,  1349,  1349,
    1350,  1350,  1351,  1351,  1352,  1352,  1353,  1353,  1354,  1354,
    1355,  1355,  1356,  1356,  1358,  1358,  1359,  1359,  1360,  1360,
    1361,  1361,  1362,  1362,  1363,  1363,  1364,  1364,  1365,  1365,
    1366,  1366,  1367,  1367,  1368,  1368,  1369,  1369,  1370,  1370,
    1371,  1371,  1372,  1372,  1373,  1373,  1374,  1374,  1375,  1375,
    1376,  1376,  1377,  1377,  1378,  1378,  1381,  1381,  1382,  1382,
    1383,  1383,  1384,  1384,  1385,  1385,  1386,  1386,  1387,  1387,
    1388,  1388,  1389,  1389,  1390,  1390,  1391,  1391,  1392,  1392,
    1393,  1393,  1394,  1394,  1395,  1395,  1396,  1396,  1397,  1397,
    1403,  1405,  1405,  1408,  1410,  1411,  1415,  1417,  1420,  1422,
    1425,  1427,  1430,  1435,  1437,  1442,  1443,  1444,  1445,  1446,
    1447,  1449,  1450,  1451,  1453,  1454,  1455,  1457,  1458,  1459,
    1461,  1463,  1464,  1465,  1466,  1468,  1470,  1471,  1472,  1474,
    1475
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "ENDOFFILE", "error", "$undefined", "AMPERSAND", "ASSIGN", "ASTERISK", 
  "BAR", "COLON", "COMMA", "DOT", "DOTDOT", "EQUAL", "GREATER", "GREQUAL", 
  "LESS", "LSEQUAL", "MINUS", "SHARP", "PLUS", "RARROW", "RPRAGMA", 
  "RBRACE", "RBRACKET", "RPAREN", "SEMICOLON", "SLASH", "SUBTYPE", 
  "UPARROW", "LPAREN", "LBRACKET", "LBRACE", "IDENT", "CARD_CONST", 
  "REAL_CONST", "CHAR_CONST", "STR_CONST", "PR_EXTERNAL", "PR_INLINE", 
  "PR_OBSOLETE", "PR_UNUSED", "PR_FATAL", "PR_NOWARN", "PR_ASSERT", 
  "PR_TRACE", "PR_LINE", "PR_PRAGMA", "PR_CALLBACK", "PR_LL", "PR_LLsup", 
  "PR_EXPORTED", "PR_SPEC", "PR_LOOPINV", "IDENTPRIME", "UPARROWPRIME", 
  "ALL", "AXIOM", "DEPEND", "ENSURES", "EXISTS", "FUNC", "IFF", "IMPLIES", 
  "INVARIANT", "IS", "LET", "MAP", "MODIFIES", "ON", "PRED", "PROTECT", 
  "ABSTRACT", "REQUIRES", "AND", "ANY", "ARRAY", "AS", "BGN", "BITS", 
  "BRANDED", "BY", "CASE", "CONST", "DIV", "DO", "ELSE", "ELSIF", "END", 
  "EVAL", "EXCEPT", "EXCEPTION", "EXIT", "EXPORTS", "FINALLY", "FOR", 
  "FROM", "GENERIC", "IF", "IMPORT", "IN", "INTERFACE", "LOCK", "LOOP", 
  "METHODS", "MOD", "MODULE", "NOT", "OBJECT", "OF", "OR", "OVERRIDES", 
  "PROCEDURE", "RAISE", "RAISES", "READONLY", "RECORD", "REF", "REPEAT", 
  "RETURN", "REVEAL", "ROOT", "SET", "THEN", "TO", "TRY", "TYPE", 
  "TYPECASE", "UNSAFE", "UNTIL", "UNTRACED", "VALUE", "VAR", "WHILE", 
  "WITH", "BAD", "WHITESPACE", "MODUNIT", "DEFUNIT", "$accept", 
  "FormattingUnit", "@1", "@2", "@3", "InitialBlankLines", "ModUnit_list", 
  "ModUnit", "DefUnit_list", "DefUnit", "CompilationUnit", "interface", 
  "module", "generic_interface", "generic_module", "generic_params", 
  "exports", "import_nl_list", "import_nl", "import_module_list", 
  "import_module", "block", "named_block", "declaration_nl_list", 
  "declaration_nl", "decl_pragma", "const_decl_list", "const_decl", 
  "type_decl_list", "type_decl", "var_decl_list", "var_decl", 
  "exception_decl_list", "exception_decl", "procedure_head", "signature", 
  "return_type", "raises", "formals", "formal_semi_list", "formal_semi", 
  "formal", "formal_pragma", "mode", "type_and_or_val_semi", 
  "type_and_or_val", "stmts", "stmts_group", "stmts1", "stmt_list", 
  "stmt_inner_list", "stmt_inner", "stmt_end", "stmt", "stmt_pragma", 
  "assignment_stmt", "call_stmt", "case_stmt", "case_list", "case", 
  "labels_list", "labels", "exit_stmt", "eval_stmt", "for_stmt", "by", 
  "if_stmt", "else", "elsif_list", "elsif", "lock_stmt", "loop_stmt", 
  "raise_stmt", "repeat_stmt", "return_stmt", "try_finally_stmt", 
  "try_stmt", "handler_list", "handler", "typecase_stmt", "tcase_list", 
  "tcase", "while_stmt", "with_stmt", "binding_list", "binding", 
  "opt_qid_list", "qid_list", "qid", "type_list", "type", "type_name", 
  "type_constructor", "type_constructor1", "root_type", 
  "type_constructor2", "simple_object_type_list", "simple_object_type", 
  "methods_part", "overrides_part", "brand", "fields", "field_semi_list", 
  "field_semi", "field", "methods", "method_semi_list", "method_semi", 
  "method", "overrides", "override_semi_list", "override_semi", 
  "override", "external_pragma", "vtrace_pragma", "strace_pragma", 
  "var_trace", "begin_trace", "assert_pragma", "loopinv", 
  "loopinv_pragma", "fatal_pragma", "fatal_exc_list", "inline_pragma", 
  "unused_pragma", "obsolete_pragma", "callback_pragma", 
  "exported_pragma", "anypragma", "pragma_pragma", "nowarn_pragma", 
  "ll_pragma", "ll_set", "line_pragma", "spec_pragma", "esc_spec", 
  "spec_proc", "spec_proc_opt_modifies", "spec_proc_opt_requires", 
  "spec_proc_opt_ensures", "spec_proc_signature", "spec_var", 
  "spec_depend", "spec_abstract", "spec_abstract_lhs", 
  "spec_opt_typed_id", "spec_pred_def", "spec_func_def", "spec_axiom", 
  "spec_protect", "spec_inv", "spec_let", "spec_term", "spec_pred", 
  "spec_quant", "spec_zquant", "spec_concl", "spec_zconcl", 
  "spec_weak_pred_op", "spec_disj", "spec_zdisj", "spec_conj", 
  "spec_zconj", "spec_literal", "spec_zliteral", "spec_atom", 
  "spec_term_sum", "spec_zterm_sum", "spec_addop", "spec_term_prod", 
  "spec_zterm_prod", "spec_mulop", "spec_term_selector", 
  "spec_term_paren", "spec_term_list", "spec_prim_term", 
  "spec_typed_id_list", "spec_typed_id", "spec_type", "spec_bin_rel", 
  "spec_proc_modifies", "spec_sub_id_list", "spec_sub_id", 
  "spec_term_bracket_list", "spec_proc_requires", "spec_proc_ensures", 
  "spec_except_spec", "spec_except_spec_list", "qqid", "qqid_list", 
  "qqidp", "mixed_qqidp", "idp", "expr", "zexpr", "e1", "ze1", "e2", "e3", 
  "ze3", "relop", "e4", "ze4", "addop", "e5", "ze5", "mulop", "e6", "e7", 
  "e8", "selector_list", "selector", "expr_t", "zexpr_t", "e1_t", "ze1_t", 
  "e2_t", "e3_t", "ze3_t", "e4_t", "ze4_t", "e5_t", "ze5_t", "e6_t", 
  "e7_t", "e8_t", "selector_list_t", "selector_t", "cons_value", 
  "Str_expr", "e4_s", "ze4_s", "e7_s", "e8_s", "expr_list", "actual_list", 
  "actual", "elem_list", "elem", "elem_tail", "opt_id_list", "id_list", 
  "Ampersand", "@4", "Assign", "@5", "Asterisk", "@6", "Bar", "@7", 
  "Colon", "@8", "Comma", "@9", "Dot", "@10", "Dotdot", "@11", "Equal", 
  "@12", "Greater", "@13", "Grequal", "@14", "Less", "@15", "Lsequal", 
  "@16", "Minus", "@17", "Notequal", "@18", "Plus", "@19", "Rarrow", 
  "@20", "Rbrace", "@21", "Rbracket", "@22", "Rparen", "@23", "Rpragma", 
  "@24", "Rpragma1", "Semi", "@25", "Semi1", "Slash", "@26", "Subtype", 
  "@27", "Uparrow", "@28", "UparrowPrime", "@29", "Lparen", "@30", 
  "Lparen2", "@31", "Lbracket", "@32", "Lbrace", "@33", "Pr_External", 
  "@34", "Pr_Inline", "@35", "Pr_Assert", "@36", "Pr_Trace", "@37", 
  "Pr_Fatal", "@38", "Pr_Unused", "@39", "Pr_Obsolete", "@40", 
  "Pr_Callback", "@41", "Pr_Exported", "@42", "Pr_Pragma", "@43", 
  "Pr_Nowarn", "@44", "Pr_Line", "@45", "Pr_LL", "@46", "Pr_LLsup", "@47", 
  "Pr_Spec", "@48", "Pr_LoopInv", "@49", "Ident", "@50", "IdentP", "@51", 
  "IdentPrime", "@52", "Card_const", "@53", "Real_const", "@54", 
  "Char_const", "@55", "Str_const", "@56", "And", "@57", "Any", "@58", 
  "Array", "@59", "As", "@60", "Begin", "@61", "Bits", "@62", "Branded", 
  "@63", "By", "@64", "Case", "@65", "Const", "@66", "Div", "@67", "Do", 
  "@68", "Else", "@69", "Elsif", "@70", "End", "@71", "Eval", "@72", 
  "Except", "@73", "Exception", "@74", "Exit", "@75", "Exports", "@76", 
  "Finally", "@77", "For", "@78", "From", "@79", "Generic", "@80", "If", 
  "@81", "Import", "@82", "In", "@83", "Interface", "@84", "Lock", "@85", 
  "Loop", "@86", "Methods", "@87", "Mod", "@88", "Module", "@89", "Not", 
  "@90", "Object", "@91", "Of", "@92", "Or", "@93", "Overrides", "@94", 
  "Procedure", "@95", "Raise", "@96", "Raises", "@97", "Readonly", "@98", 
  "Record", "@99", "Ref", "@100", "Repeat", "@101", "Return", "@102", 
  "Reveal", "@103", "Root", "@104", "Set", "@105", "Then", "@106", "To", 
  "@107", "Try", "@108", "Type", "@109", "Typecase", "@110", "Unsafe", 
  "@111", "Until", "@112", "Untraced", "@113", "Value", "@114", "Var", 
  "@115", "While", "@116", "With", "@117", "Abstract", "@118", "All", 
  "@119", "Axiom", "@120", "Depend", "@121", "Ensures", "@122", "Exists", 
  "@123", "Func", "@124", "Iff", "@125", "Implies", "@126", "Invariant", 
  "@127", "Is", "@128", "Let", "@129", "Map", "@130", "Modifies", "@131", 
  "Pred", "@132", "Protect", "@133", "Requires", "@134", "InitialNPS", 
  "@135", "NPS", "space_anypragma_list", "anypragma_space_list", 
  "anypragma_list", "space_list_emit", "space_list", "G", "B0", "B", "B2", 
  "E", "EF", "A", "AO", "AX", "V", "VZ", "VC", "Z", "SP", "XSP", "BL", 
  "AL2", "AL3", "ALZ5", "EA", "ALNL", "SPNL", "QSP", "NL", "Inc", "Dec", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned short yyr1[] =
{
       0,   137,   139,   138,   140,   138,   141,   138,   142,   142,
     143,   143,   144,   144,   144,   144,   145,   145,   146,   146,
     146,   147,   147,   147,   147,   147,   147,   148,   148,   149,
     149,   150,   151,   152,   152,   153,   153,   154,   154,   155,
     155,   156,   156,   157,   157,   158,   159,   160,   160,   161,
     161,   161,   161,   161,   161,   161,   161,   161,   161,   161,
     161,   162,   162,   162,   162,   162,   162,   162,   162,   163,
     163,   164,   164,   165,   165,   166,   166,   167,   167,   168,
     168,   168,   169,   169,   170,   170,   171,   172,   173,   173,
     174,   174,   174,   175,   175,   175,   175,   176,   176,   177,
     178,   179,   179,   180,   180,   180,   180,   181,   181,   181,
     182,   182,   182,   183,   183,   184,   184,   185,   186,   187,
     187,   188,   188,   189,   189,   189,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   191,   192,   193,   194,   194,   195,
     195,   196,   197,   197,   198,   198,   199,   200,   201,   202,
     202,   203,   204,   204,   205,   205,   206,   207,   208,   209,
     210,   211,   211,   212,   213,   213,   214,   214,   215,   215,
     216,   216,   217,   217,   218,   218,   219,   220,   221,   221,
     222,   223,   223,   224,   224,   225,   225,   226,   226,   227,
     227,   227,   227,   227,   228,   229,   229,   230,   230,   230,
     230,   230,   230,   230,   230,   230,   230,   231,   231,   232,
     232,   232,   232,   233,   233,   234,   235,   235,   236,   236,
     237,   237,   237,   238,   238,   238,   238,   239,   239,   240,
     241,   242,   242,   242,   242,   243,   243,   244,   244,   244,
     245,   245,   245,   246,   246,   246,   246,   247,   247,   248,
     249,   250,   250,   250,   250,   251,   252,   253,   253,   254,
     254,   255,   255,   256,   256,   257,   258,   259,   259,   260,
     261,   262,   263,   264,   265,   265,   265,   265,   265,   266,
     267,   268,   268,   269,   269,   269,   270,   270,   271,   272,
     272,   272,   272,   272,   272,   272,   272,   272,   272,   273,
     274,   274,   275,   275,   276,   276,   277,   277,   277,   277,
     278,   279,   280,   280,   281,   282,   282,   283,   284,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   292,   292,
     293,   294,   294,   295,   295,   296,   297,   297,   298,   299,
     299,   300,   301,   301,   302,   302,   303,   304,   304,   305,
     305,   306,   307,   307,   308,   308,   308,   309,   309,   309,
     309,   309,   309,   310,   310,   311,   311,   312,   312,   313,
     313,   314,   315,   315,   315,   316,   316,   316,   316,   316,
     316,   317,   318,   318,   319,   320,   320,   321,   322,   322,
     323,   323,   324,   324,   325,   325,   326,   326,   327,   327,
     327,   328,   328,   329,   329,   330,   331,   331,   332,   333,
     333,   334,   334,   335,   336,   336,   337,   337,   337,   337,
     337,   337,   337,   338,   339,   339,   340,   340,   340,   341,
     342,   342,   343,   343,   343,   343,   344,   344,   344,   345,
     346,   346,   346,   346,   346,   346,   346,   347,   347,   348,
     348,   348,   348,   348,   348,   348,   349,   350,   350,   351,
     352,   352,   353,   353,   354,   355,   355,   356,   357,   357,
     358,   359,   359,   360,   360,   360,   361,   361,   361,   361,
     361,   361,   361,   361,   361,   362,   362,   362,   362,   363,
     363,   364,   364,   364,   364,   364,   364,   364,   365,   365,
     366,   367,   368,   368,   369,   370,   370,   370,   370,   371,
     371,   372,   372,   373,   373,   374,   374,   375,   375,   375,
     376,   376,   377,   377,   378,   378,   380,   379,   382,   381,
     384,   383,   386,   385,   388,   387,   390,   389,   392,   391,
     394,   393,   396,   395,   398,   397,   400,   399,   402,   401,
     404,   403,   406,   405,   408,   407,   410,   409,   412,   411,
     414,   413,   416,   415,   418,   417,   420,   419,   421,   423,
     422,   424,   426,   425,   428,   427,   430,   429,   432,   431,
     434,   433,   436,   435,   438,   437,   440,   439,   442,   441,
     444,   443,   446,   445,   448,   447,   450,   449,   452,   451,
     454,   453,   456,   455,   458,   457,   460,   459,   462,   461,
     464,   463,   466,   465,   468,   467,   470,   469,   472,   471,
     474,   473,   476,   475,   478,   477,   480,   479,   482,   481,
     484,   483,   486,   485,   488,   487,   490,   489,   492,   491,
     494,   493,   496,   495,   498,   497,   500,   499,   502,   501,
     504,   503,   506,   505,   508,   507,   510,   509,   512,   511,
     514,   513,   516,   515,   518,   517,   520,   519,   522,   521,
     524,   523,   526,   525,   528,   527,   530,   529,   532,   531,
     534,   533,   536,   535,   538,   537,   540,   539,   542,   541,
     544,   543,   546,   545,   548,   547,   550,   549,   552,   551,
     554,   553,   556,   555,   558,   557,   560,   559,   562,   561,
     564,   563,   566,   565,   568,   567,   570,   569,   572,   571,
     574,   573,   576,   575,   578,   577,   580,   579,   582,   581,
     584,   583,   586,   585,   588,   587,   590,   589,   592,   591,
     594,   593,   596,   595,   598,   597,   600,   599,   602,   601,
     604,   603,   606,   605,   608,   607,   610,   609,   612,   611,
     614,   613,   616,   615,   618,   617,   620,   619,   622,   621,
     624,   623,   626,   625,   628,   627,   630,   629,   632,   631,
     634,   633,   636,   635,   638,   637,   640,   639,   642,   641,
     643,   644,   643,   645,   645,   645,   646,   646,   647,   647,
     648,   648,   649,   650,   650,   651,   652,   653,   654,   655,
     656,   657,   658,   659,   660,   661,   662,   663,   664,   665,
     666,   667,   668,   669,   670,   671,   672,   673,   674,   675,
     676
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     4,     0,     4,     0,     4,     0,     2,
       1,     2,     1,     1,     2,    11,     1,     2,     1,     1,
       2,     1,     3,     1,     3,     1,     1,    13,    17,    12,
      18,    14,    12,     0,     9,     0,     6,     0,     2,    11,
      11,     1,     7,     4,    10,    11,    12,     0,     2,    13,
       6,     5,     9,     5,     9,     5,     9,     5,     9,     5,
       7,     0,     3,     3,     3,     3,     3,     3,     3,     2,
       3,    20,    17,     2,     3,    10,    10,     2,     3,    18,
      21,    18,     1,     3,     5,    10,     5,     5,     0,     3,
       0,     7,     3,     0,     5,     5,     6,     2,     3,    13,
      13,     0,     3,     0,     2,     2,     2,    10,    14,    11,
       5,    11,     8,     0,     3,     0,     4,     2,     3,     0,
       5,     3,     1,     1,     3,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     7,     1,    11,     9,     0,
       5,     6,     1,     5,     3,     7,     1,     5,    20,     1,
       5,    10,     0,     3,     0,     2,     7,     8,     5,     3,
       9,     1,     3,     7,    10,     8,     0,     5,    14,     8,
      13,    11,     0,     5,     6,    11,     9,    12,     1,     6,
      10,     0,     1,     1,     4,     1,     3,     1,     5,     3,
       5,     5,     3,     6,     1,     1,     1,     7,     3,     4,
       3,     1,     6,     4,     8,     8,     1,     3,     1,     7,
       5,     4,     5,     1,     3,     7,     0,     3,     0,     3,
       0,     2,     4,     0,     6,     6,     7,     2,     3,     6,
       6,     0,     6,     6,     7,     2,     3,    10,    16,    15,
       9,    15,    14,     0,     6,     6,     7,     2,     3,    12,
      11,     3,     5,     6,     7,     5,     6,     0,     2,     1,
       3,     7,    11,     0,     2,     5,     5,     1,     1,     3,
       3,     3,     3,     3,     2,     2,     2,     2,     2,     5,
       3,     7,     7,     1,     2,     3,     5,     7,     7,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     5,
       0,     2,     0,     2,     0,     2,     1,     4,     3,     6,
       5,     9,     7,     7,     8,     0,     3,    10,     7,    10,
       3,     7,     3,     7,     1,     1,     3,     1,     7,     7,
       3,     1,     5,     1,     1,     3,     1,     5,     3,     1,
       5,     3,     1,     3,     7,     3,     3,     1,     5,     1,
       1,     3,     1,     5,     1,     1,     1,     1,     4,     5,
       5,     3,     3,     1,     4,     2,     5,     1,     1,     2,
       5,     4,     1,     4,     7,     1,     1,     1,     1,     1,
       1,     3,     2,     5,     2,     0,     4,     3,     5,     9,
       5,     8,     1,     5,     1,     3,     2,     5,     1,     4,
       2,     0,     3,     1,     1,     3,     1,     5,     3,     1,
       5,     3,     1,     3,     1,     5,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     5,     1,     1,     1,     3,
       1,     5,     1,     1,     1,     1,     1,     3,     3,     2,
       1,     1,     1,     1,     1,     4,     1,     0,     3,     2,
       1,     8,     8,     4,     9,     4,     3,     1,     5,     3,
       1,     5,     3,     1,     3,     1,     5,     3,     1,     5,
       3,     1,     5,     1,     3,     3,     1,     2,     3,     3,
       1,     1,     3,     5,     1,     1,     1,     1,     1,     1,
       3,     2,     1,     8,     8,     4,     9,     4,     9,     4,
       3,     3,     1,     5,     4,     1,     1,     4,     2,     2,
       5,     2,     5,     3,    17,     2,     5,     1,     5,     5,
       0,     4,     0,     1,     1,     4,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     1,     0,
       3,     1,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     3,
       1,     0,     3,     0,     1,     1,     1,     2,     1,     2,
       1,     2,     1,     1,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short yydefact[] =
{
       2,     0,     8,     0,     0,     1,   813,     0,   838,   800,
       8,     8,   690,   698,   708,   752,   838,    21,    23,    25,
      26,   828,   828,   828,   828,     9,   814,   825,   817,   817,
     825,   825,   825,   825,     3,     0,   817,   817,     0,   810,
     802,   808,     0,     0,   688,   817,    10,   838,    13,    12,
     828,    61,   817,    16,   838,    19,    18,    61,   691,   804,
     805,   806,   812,   699,   709,   753,   828,   828,     0,     0,
      22,    24,   811,   809,   618,   620,   285,   286,   828,   828,
     616,   622,   624,   626,   284,   287,   288,   828,   828,   828,
     828,   825,    11,    14,     0,   694,     0,   821,     0,    17,
      20,   807,     0,     0,   632,   828,    35,   825,   825,     0,
       0,   825,   803,   803,   803,     0,     0,     0,   817,   689,
     630,   828,   825,   598,   600,   610,   608,   606,   612,   614,
     662,   678,   720,   736,   748,   760,   828,   838,   838,   838,
     838,   838,   838,   838,   828,   828,   828,   828,   828,   828,
     828,   819,   819,   828,   819,   819,   819,   833,   652,   824,
     269,    33,    33,   803,   579,   819,     0,   828,     0,   619,
     621,   578,   290,   636,   828,   617,   623,   625,   627,   828,
     534,   552,   554,   556,   558,   560,   564,   696,   828,   426,
     430,   431,   428,   429,   427,   432,   828,     0,   803,     0,
     695,   803,   825,   825,   825,   825,   825,   825,   803,   803,
     825,   803,   803,   803,   819,     0,    62,    67,    63,    66,
      65,    64,    68,   817,     0,     0,     0,     0,     0,     0,
     830,   832,   830,   839,     0,   830,   831,   830,   831,   830,
     832,   839,   803,   825,   119,   817,     0,   837,     0,   633,
     803,   838,   822,   819,     0,   682,   828,   803,     0,   546,
     821,     0,   803,   803,   803,   803,   803,   803,   803,   817,
     817,   770,   772,   778,   784,   788,   794,   796,   766,   819,
     299,   838,   300,   302,   301,   303,   304,   305,   306,   307,
     308,   316,   404,   828,   828,   828,   828,   828,   828,   828,
     828,   828,   631,   828,   599,   601,   611,   609,   607,   613,
     615,   663,   679,   721,   737,   749,   761,   830,   830,   544,
     576,   828,     0,   261,   817,   279,   646,   277,   193,   828,
     195,   278,   280,   281,   282,   283,    51,   839,    59,   819,
      82,     0,   828,    55,   815,    53,   815,    57,   839,     0,
      41,   815,   653,   840,   819,   817,   270,     0,   838,     0,
     838,   580,    37,   817,   838,   822,   825,   816,   637,   642,
     296,   828,   803,     0,   289,   553,   555,   557,   559,   561,
     565,   697,   596,   828,   293,   827,   817,   828,   803,   825,
     825,   803,   825,   825,   825,   825,   821,   310,   548,   590,
       0,     0,     0,   817,     0,   817,     0,     0,   817,     0,
       0,   827,   816,    50,   816,   803,   803,     0,     0,   828,
     819,   817,   803,   821,     0,     0,   834,   835,   815,   830,
     839,   827,     0,   834,   835,   817,   834,   834,   835,   815,
     829,   819,   815,     0,   114,   816,   604,   824,    37,   823,
      37,    47,     0,    37,   817,   683,     0,   803,     0,   547,
     535,   803,     0,   570,   827,   294,   817,   819,   416,   817,
       0,   771,   773,   779,   785,   789,   795,   797,   767,     0,
     792,   312,   838,   828,   803,   803,   790,   318,   382,   828,
     405,     0,   827,   821,   325,   330,   335,   817,   325,   827,
     332,   821,     0,   821,     0,     0,    47,   545,   577,   828,
     262,     0,   510,   819,   512,     0,   647,     0,   276,   196,
     835,   819,    69,   815,    60,    83,   823,   840,   592,    86,
     817,   835,   819,    73,     0,   819,   835,   819,    77,   815,
     819,   819,     0,   672,   828,   602,   660,   674,   680,   692,
     700,   702,   722,   732,   734,   746,   762,   764,   819,   118,
     123,   122,   126,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     146,   828,   828,   822,   156,   828,   828,   273,   822,   273,
     822,   824,   828,   828,    47,     0,   803,   819,    47,   816,
      47,    38,   817,     0,    33,    47,     0,   819,   643,   297,
     597,   291,   803,   295,     0,   519,   415,     0,   710,   819,
     419,   422,   828,   817,   292,   298,   825,   798,   314,   838,
     828,   311,   827,   549,   591,   825,   594,     0,     0,   574,
     317,   819,     0,     0,     0,     0,   768,   776,   819,   337,
     828,   828,   817,     0,   827,     0,     0,   827,     0,     0,
     406,   819,   827,   817,     0,   263,   511,     0,   648,   728,
     740,   837,   457,   817,   515,   516,   821,   233,   828,   194,
      70,   830,     0,   817,     0,   803,     0,   833,    74,   830,
     204,   828,   830,    78,   830,     0,   819,   834,    43,   803,
       0,   803,   803,   803,   803,   803,   803,   803,   803,   803,
     803,   803,   803,   825,   824,     0,     0,   817,   817,   817,
     817,   817,   824,     0,   817,   824,   817,   825,   817,   831,
     819,   817,   686,   750,   828,   821,   605,   828,   817,   532,
     827,    48,   828,    61,   819,   827,    33,    36,   571,   821,
     716,   828,   803,   418,     0,   817,   819,   424,   817,   793,
     803,   774,   309,   838,   828,   313,   817,   391,     0,   791,
     803,     0,   828,   803,     0,   320,     0,   379,     0,   780,
     828,   828,   821,     0,   825,   825,   336,     0,     0,   819,
     341,   817,   821,   827,   821,   538,   828,   827,   658,   828,
     821,     0,     0,    61,   264,   536,   828,   803,   825,   825,
     518,     0,   819,   827,   817,     0,   836,   815,     0,    52,
     820,   827,   230,    84,   593,    88,   815,    56,     0,    54,
      58,   820,   835,   819,   815,   673,   819,   603,   661,   675,
     681,   693,   701,   703,   723,   733,   735,   747,   763,   765,
     120,   121,   822,   817,   828,   817,   828,   828,   836,   628,
     274,   828,   169,   825,   172,     0,   828,   815,   127,    61,
     803,   803,     0,   817,     0,   828,   819,   533,     0,     0,
     819,     0,   819,   817,   803,   817,   711,   644,   828,   421,
     423,     0,   819,   434,     0,   799,   803,   315,   817,   397,
       0,   392,   395,   595,   572,   383,     0,   575,   319,   821,
     828,   803,   817,   817,     0,   326,   769,   777,   827,   827,
     340,     0,   819,   346,   817,   817,   827,     0,   803,   817,
     821,   803,   827,     0,   838,   819,   839,   803,   817,   649,
     729,   741,   823,   514,   837,     0,   827,   197,   714,   824,
       0,   832,   824,   815,     0,   654,   656,   738,   756,   819,
     819,   205,   216,   206,   211,   223,     0,     0,   817,   817,
     821,   828,   822,   218,   828,     0,    90,   821,   815,   835,
     834,   817,   584,   821,   821,   815,   839,   838,   828,   838,
     817,   819,     0,   819,     0,     0,     0,   803,   817,   817,
     676,   684,   176,   824,     0,   819,   188,   815,   839,   687,
     751,   267,   821,   266,     0,   827,    32,     0,   838,    29,
     819,   520,   717,   417,   803,   817,   828,   433,     0,   562,
     566,   638,   640,   456,   819,   440,   446,   457,   827,   827,
     817,   450,   451,   452,   453,   454,   775,   817,   821,   394,
     803,   744,   828,     0,     0,   781,   323,   322,     0,   827,
     827,   782,   821,   344,   343,   345,     0,   819,   349,   817,
     827,     0,   328,   539,   333,   334,     0,   659,   331,   817,
     407,    39,   819,     0,   537,   513,   816,     0,   586,   458,
       0,   460,     0,   517,     0,     0,   803,   817,   221,   815,
     817,   828,   827,   803,   803,   803,   803,   199,   230,   202,
     230,   230,   712,   730,   233,   821,   822,   827,   828,   817,
     817,     0,   230,   816,   724,    87,   828,   817,   835,   834,
     819,    97,   819,   101,   803,   817,   817,   828,    42,    40,
       0,    15,   817,   828,     0,   826,   157,   742,   824,   666,
     824,   168,   629,   828,     0,   803,   803,   825,   817,   836,
     273,   819,   819,     0,     0,   821,     0,     0,     0,     0,
      27,     0,   838,   645,   420,   817,   828,   438,   437,   436,
     803,   803,   803,   803,   439,     0,   449,     0,     0,   827,
     819,     0,   817,   573,   803,     0,   380,   381,     0,   821,
     821,   803,   817,   828,   348,     0,   819,   352,   828,   817,
     819,   821,   786,   828,     0,   375,   830,   824,   827,   509,
     803,   459,   823,   823,   823,   821,   824,   715,   220,   834,
     835,   834,   817,   222,   828,   820,   840,   655,   657,   739,
     757,   819,     0,   819,   224,   803,   803,   226,   817,     0,
       0,     0,   821,   828,   208,   210,     0,   217,   532,   825,
       0,    89,    98,   819,    95,    94,   820,   585,     0,     0,
     828,   820,   650,   828,   819,     0,   821,   825,   817,   803,
     164,   803,   836,     0,   754,   821,   677,   685,   836,     0,
     176,   818,     0,   824,   819,   834,   267,   824,     0,   268,
     828,   819,    31,    34,   828,     0,   425,   817,   563,   567,
     639,   641,   540,   582,   664,   706,   828,   442,   443,   444,
     445,   448,   447,     0,   398,     0,   393,     0,   745,   384,
       0,   817,   817,   783,   342,   817,   828,   351,   817,   819,
     817,   321,     0,   803,   817,   821,    49,   838,   530,   817,
     587,   816,     0,   816,   816,     0,   817,   817,   835,   834,
     819,   237,   819,     0,   817,   815,     0,   200,   201,   713,
     731,   228,     0,   213,   209,   819,   550,   824,     0,   232,
     821,   819,   725,   816,    92,    96,   821,   817,   819,   819,
     817,   815,   803,   817,   145,   271,   817,   836,     0,   149,
     817,   743,   825,   667,     0,   275,   803,   817,     0,   542,
     668,   828,   824,   825,     0,   173,   836,   835,   828,   828,
     838,   828,   817,   826,     0,   828,   435,   803,   803,   803,
     803,     0,   455,   828,   396,   324,   338,   339,   347,   817,
     353,   355,     0,   819,   357,     0,   329,   787,   327,   817,
     840,   819,     0,   527,   525,   827,   463,   827,   827,   465,
     198,   219,   238,   819,   235,   234,   819,   828,     0,    85,
     836,     0,   704,   241,   203,   803,   817,   824,   817,   827,
     191,   102,   103,   819,   819,   827,     0,   651,    44,   819,
       0,   828,   825,   827,   152,   817,   836,   165,     0,   167,
     755,   819,   175,   803,   803,   817,   163,   836,   821,     0,
     815,     0,   819,   840,   817,   828,   825,   817,     0,     0,
     541,   583,   665,   707,   441,     0,   350,   828,   389,   386,
     388,   385,   387,   390,   356,     0,   634,   819,   362,   367,
     373,   408,   378,   817,   411,   377,   376,     0,   827,   821,
       0,     0,   819,   815,   819,   530,   236,   815,   820,   828,
       0,   718,   253,   825,   227,   815,   551,   827,   817,   212,
       0,   819,   192,   726,   758,   820,   828,   828,   828,    75,
      76,   267,   820,   828,   828,   148,   817,   836,     0,     0,
     819,     0,   670,   828,   170,   543,   669,   177,     0,     0,
       0,   186,   189,   824,   815,     0,   821,     0,   836,     0,
     182,   817,    28,     0,   402,   819,   828,   817,   828,   360,
     359,   803,   361,     0,     0,     0,     0,   827,   410,   828,
       0,   817,   821,   821,   827,     0,   521,   817,   827,   819,
     819,   819,   819,     0,   815,   817,   225,   825,   229,   815,
     705,   832,     0,   207,   819,     0,   825,   825,   817,   106,
     104,   105,   840,   815,   817,     0,   150,     0,   821,   568,
     824,   154,     0,   161,   803,   817,   174,     0,   819,   836,
       0,   827,     0,   265,     0,   828,   825,   828,    30,   399,
       0,     0,     0,   819,   817,   635,   828,   364,   365,   366,
     588,   371,   372,   827,   411,   827,     0,     0,     0,   508,
     526,   531,   817,   817,     0,   821,   819,   817,     0,   827,
     239,   240,   817,   828,     0,   827,   719,   831,   815,   215,
     214,    91,   727,   759,     0,     0,     0,   827,   272,   147,
     817,   803,   819,   815,   824,   671,   828,   267,   824,     0,
     828,   819,   828,   181,   817,   836,     0,   828,     0,   828,
     354,   358,     0,   803,   827,   409,   827,   368,   374,   412,
     413,   414,   819,   528,   529,   462,   815,   523,   819,   467,
     817,   461,     0,   828,   817,   828,   840,   815,   834,   835,
     834,   817,   820,   581,   819,   828,   267,   153,   569,   151,
     119,   817,     0,   827,   819,   187,   817,    45,   817,   183,
       0,   824,     0,     0,   828,   817,   363,   589,   370,   369,
      46,   522,   466,     0,   819,   470,   473,   828,   817,   464,
       0,   819,   817,     0,   815,   835,   834,   817,   835,   834,
     819,   245,   819,     0,   815,   815,   817,   840,   819,   819,
     819,   824,     0,   179,   190,   821,   180,   819,   267,   403,
       0,   400,   828,   469,     0,   817,   821,   475,   817,   828,
     829,   110,   817,   827,   819,   835,   834,   819,   257,   819,
       0,   246,   819,   243,   242,   820,   267,   267,   803,   827,
       0,   116,   117,   155,   166,   828,     0,     0,   159,   184,
       0,   828,   817,   828,   472,   474,     0,   819,   478,   230,
     817,   819,   828,   840,   819,   258,   819,   255,   254,   820,
     244,   817,   819,   819,   819,   267,   819,     0,   819,   828,
     828,   817,   468,   817,   828,   477,     0,   486,   490,   216,
     491,   819,   481,   483,   494,   827,   827,   817,   195,   495,
     496,   497,   498,   819,   815,     0,     0,    72,   256,   817,
     828,    99,   100,   819,   840,   819,   819,   273,   817,     0,
     401,   471,   817,   828,   487,   499,     0,   502,   230,     0,
     230,   457,   480,     0,     0,     0,   827,   817,     0,   112,
       0,   803,   828,   819,   828,   820,    79,     0,    81,   824,
     824,   828,   824,   476,   817,   837,   501,   488,   823,   823,
     823,   489,   492,   828,   485,   484,     0,   817,   821,   829,
     819,   817,   819,     0,   819,   817,   819,   819,   836,   160,
     819,   479,   500,   816,     0,   816,   816,     0,     0,   457,
     817,   817,   819,   107,   819,    71,   828,   819,   819,   817,
     828,   819,   178,     0,   185,   827,   505,   827,   827,   507,
     482,   493,   817,   819,   109,   111,     0,     0,   819,   250,
     828,     0,    80,   158,   819,   819,   530,   230,   819,   829,
     819,   247,     0,   828,   827,   827,   819,   819,   819,   819,
     819,   828,     0,     0,     0,   827,   819,   108,   819,   260,
       0,   819,   504,   503,     0,   819,   259,   819,   819,   819,
     506,   819,   819,   819,   819,   252,   524,   819,   251,   249,
     248
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     1,     2,     3,     4,     7,    45,    46,    52,    53,
      47,    17,    18,    19,    20,   246,   167,   451,   601,   349,
     350,   730,   662,   663,   741,    96,   426,   427,   433,   434,
     437,   438,   339,   340,   136,   529,   976,  1125,   686,   978,
     979,   980,  1266,  1575,  1640,  1641,   243,  1742,  1848,   354,
     355,   558,   559,   560,   561,   562,   563,   564,  1277,  1399,
    1493,  1494,   565,   566,   567,  1896,   568,  1288,  1402,  1497,
     569,   570,   571,   572,   573,   574,   575,  1157,  1290,   576,
    1516,  1610,   577,   578,  1005,  1006,  1571,   327,   690,   946,
     947,  1937,   960,  1938,  1939,  1033,   964,   965,  1371,  1470,
    1242,   816,  1229,  1230,  1231,  1564,  1788,  1789,  1790,  1648,
    1834,  1835,  1836,   137,  1299,   356,  1165,   159,   579,   722,
     860,   138,   329,   139,   140,   141,   142,   143,    39,    84,
      76,    85,   383,    77,    86,   279,   280,   481,   628,   762,
     281,   282,   283,   284,   493,   644,   285,   286,   287,   288,
     289,   290,  1074,  1075,   496,   648,   649,   789,  1062,   790,
     922,   923,  1067,  1068,  1206,  1207,  1339,  1443,  1618,  1444,
    1537,  1696,  1538,  1539,  1078,  1540,   641,   777,   487,  1527,
     482,   767,   901,  1049,   629,   763,  1614,  1615,   488,   503,
    1542,  1628,  1769,   384,   467,   468,   619,   620,   621,   756,
     188,   757,   892,  1176,   893,  1034,  1316,  1035,  1036,  1037,
     812,  1089,  1716,  1778,  1779,  1824,  1825,  1826,  1866,  1867,
    1907,  1908,  1941,  1942,  1943,  1944,  1974,  1975,   810,   321,
     420,   513,   514,   672,   464,  1552,  1636,  1348,  1454,  1451,
     876,   778,  1177,   937,   796,   928,  1317,  1427,  1411,  1503,
     322,   415,   260,   372,   401,   484,  1377,  1475,   189,   262,
     190,   263,   191,   264,   192,   265,   193,   266,  1038,  1180,
     194,   267,  1039,  1181,  1670,  1741,   465,   612,   905,  1050,
     640,   773,   323,   416,   172,   165,   250,  1794,  1318,  1428,
     984,  1134,  1091,  1220,  1702,  1763,  1040,   485,   530,   685,
     969,   770,   385,   461,   144,   201,   145,   202,   581,   701,
     447,   596,   146,   205,   147,   204,   148,   203,   149,   206,
     150,   207,    87,   111,    78,   107,    79,   108,    88,   112,
      89,   113,    90,   114,   861,   997,   292,   198,   105,   163,
    1544,  1621,  1042,   257,  1043,  1182,  1044,  1183,  1045,   457,
     888,  1024,   331,   422,   676,   807,  1273,  1392,   160,   242,
     970,  1103,   971,  1104,   799,   931,   582,   702,   151,   208,
    1319,  1429,  1150,  1281,  1412,  1504,  1593,  1674,   544,   699,
     583,   703,  1002,  1155,   152,   209,   584,   704,   256,   366,
    1003,  1156,   734,   870,    50,    91,    21,    30,   585,   705,
      97,   122,   195,   268,    22,    31,   586,   706,   587,   707,
    1473,  1563,  1320,  1430,    23,    32,   622,   752,  1114,  1245,
     949,  1096,   751,   884,  1562,  1647,   153,   210,   588,   708,
    1126,  1259,  1576,  1656,   677,   808,  1115,  1246,   589,   709,
     590,   710,   154,   211,   973,  1105,   678,   809,  1148,  1279,
    1052,  1194,   591,   711,   155,   212,   735,   871,    24,    33,
    1285,  1406,   974,  1106,  1577,  1657,   156,   213,   592,   712,
     593,   713,   294,   395,   650,   784,   295,   388,   296,   389,
     764,   896,   651,   785,   297,   390,   781,   911,  1064,  1201,
     298,   391,  1213,  1343,   299,   392,   489,   635,   483,   626,
     300,   393,   301,   394,   630,   760,     8,    27,    58,    59,
      60,    41,    61,    62,   435,   975,   386,   595,   230,   834,
      42,   168,   599,   244,    43,  1158,   642,  1166,   540,   336,
     344,   337,   241,   521,   522,   950,   811,    25,   341,   443
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -1815
static const short yypact[] =
{
     124,   149,    59,   160,   148, -1815, -1815,   418, -1815,   347,
      59,    59, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815,   370,   309,   309,
     499,   214,   214,   499, -1815,   421, -1815, -1815,   421, -1815,
   -1815,   413,   480,   692, -1815,   242, -1815, -1815, -1815, -1815,
   -1815,   207,   306, -1815, -1815, -1815, -1815,   215, -1815, -1815,
   -1815,   662,   237, -1815, -1815, -1815, -1815, -1815,   271,   271,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   214, -1815, -1815,   350, -1815,  1763, -1815,   377, -1815,
   -1815, -1815,   271,   271, -1815,   431,   387,    89,    76,   485,
     514,   214,   413,   413,   413,   350,   666,   666, -1815, -1815,
   -1815, -1815,   214, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,   431, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   539,   539, -1815,   539,   539,   539, -1815, -1815,   426,
     543,   529,   529,   413, -1815, -1815,   580,   431,   508, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   604,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815,  1303,   413,   215,
   -1815,   413,    89,    89,    89,   184,    89,    89,   413,   413,
     116,   413,   413,   413, -1815,   580, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   409,   629,   119,   629,   629,   629,   629,
   -1815, -1815, -1815, -1815,   271, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,   413, -1815, -1815, -1815,   431, -1815,   431, -1815,
     413, -1815, -1815, -1815,   580, -1815, -1815,   413,   278, -1815,
   -1815,   485,   413,   413,   413,   413,   413,   413,   413,   624,
     624, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   365, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   627,   350, -1815, -1815, -1815, -1815,   604, -1815, -1815,
     659, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   640,
   -1815,   350, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   407,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815,   633, -1815,   654,
   -1815, -1815, -1815, -1815, -1815, -1815,   214, -1815, -1815, -1815,
   -1815, -1815,   413,   350, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   673, -1815, -1815,   413,   214,
     214,   413,   214,   214,   214,   214, -1815,   642, -1815, -1815,
     106,   350,   350, -1815,   350, -1815,   350,   350, -1815,   350,
     350, -1815, -1815, -1815, -1815,   413,   413,   350,   629, -1815,
   -1815, -1815,   413, -1815,   629,   350,   680, -1815, -1815, -1815,
   -1815,   654,   704,   702, -1815, -1815,   702,   680, -1815, -1815,
   -1815, -1815, -1815,   650, -1815,  2588, -1815,   721, -1815, -1815,
   -1815,   461,   350, -1815, -1815, -1815,   350,   413,   485, -1815,
   -1815,   413,   485, -1815,   673, -1815, -1815,   637, -1815,   644,
     485, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   485,
   -1815,   676, -1815, -1815,   413,   413, -1815, -1815,   298, -1815,
   -1815,   344, -1815, -1815,   298, -1815, -1815,   176,   298,   654,
   -1815, -1815,   654,   740,   350,   350, -1815, -1815, -1815, -1815,
   -1815,   629, -1815,   752, -1815,   782, -1815,   350, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     733, -1815, -1815, -1815,   350, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,   350, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     737,   422, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     761, -1815, -1815, -1815, -1815, -1815, -1815,   715, -1815,   715,
     406,   355, -1815, -1815, -1815,   144,   413, -1815,   461, -1815,
     461, -1815,   650,   215,   529,   461,   350,   604, -1815, -1815,
   -1815, -1815,   413, -1815,   604, -1815, -1815,   664, -1815,   695,
   -1815, -1815, -1815, -1815, -1815, -1815,   214, -1815,   716, -1815,
   -1815, -1815, -1815, -1815, -1815,   487, -1815,   106,   106, -1815,
     627,   740,   350,   101,   627,   350, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   627, -1815,   627,   770, -1815,   703,   604,
     659,   604, -1815, -1815,   629, -1815, -1815,   780, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,   683,   539, -1815, -1815,
   -1815, -1815,   350, -1815,   431,   413,   768, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   350, -1815, -1815,   717,   413,
     350,   413,   413,   413,   413,   413,   413,   413,   413,   413,
     413,   413,   413,   214, -1815,   431,   770, -1815, -1815, -1815,
   -1815, -1815,   426,   744, -1815,   669, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   650,   350,
   -1815, -1815, -1815, -1815, -1815, -1815,   529, -1815, -1815, -1815,
   -1815, -1815,   413, -1815,   726,   644,   817, -1815, -1815, -1815,
     413, -1815, -1815, -1815, -1815, -1815, -1815,   740,   350, -1815,
     413,   781, -1815,   413,   106, -1815,   604, -1815,   679, -1815,
   -1815, -1815, -1815,   781,   122,   122, -1815,   775,   775, -1815,
     638, -1815, -1815,   768, -1815, -1815, -1815,   768, -1815, -1815,
   -1815,   431,   431, -1815, -1815, -1815, -1815,   413,   522,   510,
   -1815,   624,   772, -1815, -1815,   700, -1815, -1815,   700, -1815,
   -1815, -1815,  1200, -1815, -1815,   627, -1815, -1815,   391, -1815,
   -1815,   604, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   453, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   384, -1815, -1815, -1815, -1815,
     413,   413,   350, -1815,   629, -1815, -1815,   604,   659,   350,
   -1815,   659, -1815, -1815,   413, -1815, -1815, -1815, -1815, -1815,
   -1815,   666,   579, -1815,   657, -1815,   413, -1815, -1815, -1815,
     604, -1815,   659, -1815, -1815, -1815,   687, -1815, -1815, -1815,
   -1815,   413, -1815, -1815,   350, -1815, -1815, -1815, -1815, -1815,
   -1815,   641,   637, -1815, -1815, -1815, -1815,   106,   413, -1815,
   -1815,   413, -1815,   350, -1815, -1815,  1763,   413, -1815, -1815,
   -1815, -1815,   791, -1815,   155,   768,   683, -1815, -1815, -1815,
     650, -1815, -1815, -1815,   768, -1815, -1815, -1815, -1815,   193,
   -1815, -1815,   193, -1815,   216, -1815,   330,   705, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   624,   706, -1815,   796, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   740,   700, -1815,   693,   739,   650,   413, -1815, -1815,
   -1815, -1815,   789,   426,   739,   604, -1815, -1815,  1763, -1815,
   -1815,   784, -1815, -1815,   350, -1815, -1815,   659, -1815, -1815,
   -1815, -1815, -1815, -1815,   413,   644, -1815, -1815,   584, -1815,
   -1815, -1815, -1815, -1815,   164, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   775,
     413, -1815, -1815,   350,   106, -1815, -1815, -1815,   298,   781,
     781, -1815, -1815, -1815, -1815, -1815,   664,   695, -1815,   644,
   -1815,   627, -1815, -1815, -1815, -1815,   762, -1815,   740, -1815,
     659, -1815, -1815,   377, -1815, -1815, -1815,   673, -1815, -1815,
     350, -1815,   590, -1815,   604,   700,   413, -1815, -1815, -1815,
   -1815,   627, -1815,   413,   413,   413,   413, -1815,   746, -1815,
     746,   746, -1815, -1815,   539, -1815, -1815, -1815, -1815, -1815,
     389,   704,    61, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815,   413, -1815, -1815,   627, -1815, -1815,
     763, -1815, -1815, -1815,   604,   205, -1815, -1815,   643, -1815,
     426, -1815, -1815, -1815,   714,   413,   413,   753, -1815, -1815,
     715, -1815, -1815,   350,   377, -1815,   633,   700,   659,   768,
   -1815,   650, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     413,   413,   413,   413, -1815,   183,   772,   657,   657, -1815,
     754,   350, -1815, -1815,   413,   106, -1815, -1815,   350, -1815,
   -1815,   413, -1815, -1815, -1815,   726, -1815, -1815, -1815, -1815,
     740, -1815, -1815, -1815,   604, -1815, -1815,   426, -1815, -1815,
     413, -1815,   820, -1815,   791, -1815, -1815, -1815, -1815,   702,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   216,   738,   216, -1815,   413,   413,   749, -1815,   704,
     768,   842, -1815, -1815, -1815,   216,   330, -1815,   350,   118,
      83, -1815, -1815, -1815, -1815, -1815,   816, -1815,   431,   431,
   -1815, -1815, -1815, -1815, -1815,   629, -1815,   753, -1815,   413,
   -1815,   413, -1815,   629, -1815, -1815, -1815, -1815, -1815,   136,
   -1815, -1815,   650,   426, -1815, -1815,   784,   426,   770, -1815,
   -1815, -1815, -1815, -1815, -1815,   650, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   768, -1815,   774, -1815,   781, -1815, -1815,
     493, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   644,   846,
   -1815, -1815,   106,   413, -1815, -1815, -1815, -1815,   740, -1815,
   -1815, -1815,   768, -1815, -1815,   673, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   350, -1815, -1815,   431, -1815, -1815, -1815,
   -1815,   755,   767, -1815, -1815, -1815, -1815, -1815,   777, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,   413, -1815, -1815, -1815, -1815, -1815,   136, -1815,
   -1815, -1815,   753, -1815,   650, -1815,   413, -1815,   650, -1815,
   -1815, -1815,   426,   753,   350, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   206,   350, -1815, -1815,   413,   413,   413,
     413,   657, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,   970,   588, -1815,   561, -1815, -1815, -1815, -1815,
   -1815, -1815,   604,   429, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,   604, -1815,   580, -1815,
   -1815,   764, -1815,   539, -1815,   413, -1815, -1815, -1815, -1815,
     350, -1815,   225, -1815, -1815,   761,   770, -1815, -1815, -1815,
     650, -1815,   753,   855, -1815, -1815, -1815, -1815,   634, -1815,
   -1815, -1815, -1815,   413,   413, -1815, -1815, -1815,   168,   650,
   -1815,   739, -1815, -1815, -1815, -1815,   753, -1815,   659,   350,
   -1815, -1815, -1815, -1815, -1815,   350, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   645, -1815,   135,   303, -1815,
   -1815,   220, -1815, -1815, -1815, -1815, -1815,   650, -1815, -1815,
     842,   770,   740, -1815,   740,   740, -1815,   872, -1815, -1815,
     650, -1815,   539,   433, -1815, -1815, -1815, -1815, -1815, -1815,
     673, -1815,   604, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   784, -1815, -1815, -1815, -1815, -1815, -1815,   604,   861,
     871,   650, -1815, -1815, -1815, -1815, -1815, -1815,   650,   654,
     861, -1815, -1815,   426, -1815,   650, -1815,   629, -1815,   136,
   -1815, -1815, -1815,   659, -1815,   877,   311, -1815, -1815, -1815,
   -1815,   413, -1815,   140,   324,   209,   654, -1815,   659, -1815,
     673,   842, -1815, -1815, -1815,   604, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   627, -1815, -1815, -1815,   522, -1815, -1815,
   -1815, -1815,   781, -1815, -1815,   673,   214,   214, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   629, -1815,   650, -1815, -1815,
     279, -1815,   842, -1815,   413, -1815, -1815,   350, -1815, -1815,
     580, -1815,   687, -1815,   650, -1815,   753,   740, -1815, -1815,
     882,   350,   861, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   768,   768,   209,   350, -1815,
   -1815, -1815, -1815, -1815,   768, -1815, -1815, -1815,   781, -1815,
   -1815, -1815, -1815, -1815,   580, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815,   350,   866,   770, -1815, -1815, -1815,
   -1815,   413, -1815, -1815, -1815, -1815, -1815,   784,   279,   650,
   -1815, -1815, -1815, -1815, -1815, -1815,   481, -1815,   768, -1815,
   -1815, -1815,   561,   413,   781,   659,   768, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,   637, -1815,
     644, -1815,   673, -1815, -1815, -1815, -1815, -1815,   702, -1815,
   -1815, -1815,   604, -1815, -1815, -1815,   784, -1815, -1815, -1815,
   -1815, -1815,   693, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     650,   279,   350,   350, -1815, -1815,   303, -1815, -1815, -1815,
   -1815, -1815, -1815,   664,   695, -1815, -1815, -1815, -1815, -1815,
     770,   385, -1815,   431,   805, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   350,   872, -1815, -1815, -1815, -1815, -1815,
   -1815,   643,   768, -1815, -1815,   810, -1815, -1815,   784, -1815,
     861, -1815, -1815, -1815,   726,   644,   113, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     350, -1815, -1815, -1815, -1815, -1815,   784,   784,   413, -1815,
     431, -1815, -1815, -1815, -1815, -1815,   739,   703, -1815, -1815,
     768, -1815, -1815, -1815, -1815, -1815,   666,   579, -1815,  2025,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   784, -1815,   861, -1815, -1815,
   -1815, -1815, -1815,   644, -1815, -1815,   584,   759, -1815,   193,
     864,   164, -1815, -1815, -1815, -1815, -1815, -1815,   526, -1815,
   -1815, -1815, -1815,   737, -1815,   770,   431, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815,   715, -1815,   861,
   -1815, -1815, -1815, -1815,   772, -1815,   350, -1815,   746,   590,
     746, -1815, -1815,   183,   657,   657, -1815, -1815,   770, -1815,
     431,   413, -1815, -1815, -1815,   704, -1815,   431, -1815,   279,
     426, -1815,   279, -1815, -1815,   155, -1815,   216,   820, -1815,
     791,   216,   772, -1815, -1815, -1815,   768,   644, -1815, -1815,
   -1815, -1815, -1815,   770,   403, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815,   768, -1815, -1815,   673,   657, -1815,
   -1815, -1815, -1815, -1815,   737, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815,   650, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815,   772, -1815, -1815, -1815, -1815,   431,   350, -1815, -1815,
   -1815,   770, -1815, -1815,   740,   740,   740,  2025, -1815, -1815,
     431, -1815,   770, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,   350,   768,   781, -1815, -1815, -1815, -1815, -1815,
     350,   431, -1815, -1815,   673, -1815, -1815,   431, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
   -1815, -1815, -1815, -1815, -1815,   712, -1815,   850, -1815,   847,
     318,   869,   870, -1815, -1815,  -103, -1815,   178,   437, -1815,
     -82, -1815,   -22,  -347,   441,  -665, -1815,   486,   565,   141,
   -1815,   476, -1815,   484, -1815, -1072, -1815, -1815, -1815, -1815,
     -62,   -61, -1815, -1815,  -926,  -925,  -396, -1657, -1815,  -880,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,  -478,  -662,
   -1815,  -811, -1815, -1815, -1815, -1815, -1815, -1199, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815,  -360,  -573, -1815,
    -674,  -821, -1815, -1815, -1815,  -571, -1815, -1262,  -221,  -667,
     862,  -435, -1815,   115,   125,  -507, -1035,  -160, -1815, -1815,
    -788,  -159, -1815,  -275,  -273, -1815, -1815,  -830,  -829, -1815,
   -1815,  -873,  -872, -1815, -1815, -1815, -1220,  -986, -1815,  -566,
   -1815, -1815, -1815, -1815,  -301, -1815,  -783, -1815,   930, -1815,
   -1815, -1815,   707, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815,   474, -1815, -1815, -1815, -1815,
   -1815, -1815, -1009,  -387, -1815, -1815, -1100, -1815, -1815, -1815,
   -1815,  -361, -1815,  -463,  -359, -1815,  -639, -1815, -1815,  -708,
   -1815, -1815,  -774, -1815, -1010, -1815,  -543,  -568,  -558, -1815,
   -1815, -1815,  -202, -1815, -1815, -1815,  -820, -1815,  -180, -1815,
   -1815,  -710, -1815,  1316, -1815,  -850, -1815,  -715, -1815, -1815,
    -111, -1123, -1815,  -940, -1254, -1815,  -984, -1098, -1815, -1815,
   -1011, -1008,  -947, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,  -938,  -617,
   -1815, -1815,    65, -1815, -1392, -1051,  -769, -1384,  -623, -1491,
    -246,   -99,   346, -1815,  -448, -1815,  -608, -1815, -1304, -1815,
    -203, -1815,  -130, -1815,  -297, -1815, -1445, -1815,  -144, -1815,
    -421, -1815,  -416, -1815,  -415, -1815,  -413, -1815,  -987, -1815,
    -409, -1815,  -967, -1815, -1502, -1815,  -452, -1815,  -677, -1815,
    -322, -1815,   -28, -1815,  -139,  -105, -1815,  -797, -1815, -1815,
   -1815, -1815, -1552, -1815, -1815, -1815,  -266, -1815, -1815, -1815,
    -444, -1815,  -781, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
    -131, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,   475, -1815,    32, -1815,
   -1494, -1815,  -101, -1815, -1814, -1815, -1813, -1815,  -253, -1815,
   -1150, -1815,  -223, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815,  -858, -1815, -1815, -1815, -1815, -1815,
    -583, -1815,  -961, -1815, -1815, -1815, -1815, -1815,  -499, -1815,
   -1815, -1815,  -283, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,  -333, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
     848, -1815, -1815, -1815,  1011, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815,  -574, -1815,  1018, -1815, -1022, -1815, -1815, -1815,
    -711, -1815, -1018, -1815, -1815, -1815,  -786, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,  -201, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815,   -66, -1815, -1815, -1815,  -745, -1815,
    -622, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,  -170, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815,   142, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,
   -1815, -1815, -1815, -1815, -1815, -1815, -1815, -1815,  3508,  1021,
     145, -1815, -1815,   614,  1038,  -329,  1022,  -222,  1953,  -162,
    3729,  -210,  -498,  -141,  -211, -1064,  3645,   -21, -1778,   -30,
    -218,  -216,   381,  -263,  -352,  -544,  -240,   281,    12,  -490
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, parse error.  */
#define YYTABLE_NINF -840
static const short yytable[] =
{
      35,    36,    37,    38,   328,   371,   196,   359,   671,   174,
     231,   233,   613,   236,   238,   240,   179,   291,   495,   245,
     346,   500,   252,   725,   348,   402,  1186,   293,   683,    94,
     942,   214,   353,   425,   966,  1023,   972,   684,   456,   967,
     889,  1178,   363,  1160,   637,   102,   103,  1208,  1203,  1254,
     645,   597,  1306,  1426,   645,  1336,   813,   109,   110,   248,
    1210,  1179,   253,    98,  1639,  1554,   115,   116,   117,   118,
    1215,   318,  1701,  1241,  1555,  1243,  1419,   783,  1397,   771,
     772,  1278,   533,   505,   166,   506,   538,  1255,   400,  1321,
    1322,  1804,  1911,   449,  1491,  1950,  1951,  1217,  1678,   691,
     199,   106,  1334,   742,   602,  1632,   915,   952,  -803,  -803,
     365,   793,   181,   382,   797,   215,   594,  -821,   417,   370,
    -821,  -819,   374,   223,   224,   225,   226,   227,   228,   229,
    -821,  1704,   234,  -821,   161,   162,  -819,   120,   936,   956,
    -821,   358,  1409,   360,  -803,  1312,   254,  -803,  -803,     5,
     120,  -803,  1508,   258,  1857,   454,   326,  -821,   261,  -821,
    -821,   779,  -821,  -821,   398,   526,  -821,   269,   680,  -821,
     532,   486,    40,   535,   537,   270,   259,  1374,  1297,   688,
     957,  1116,  1088,  1327,   693,  -819,  1711,  -828,  1312,  -821,
    1759,  -803,   326,     6,  1123,   727,   325,   423,   332,   333,
     334,   335,   338,  1496,  1008,   343,   101,   345,  1313,   347,
       6,  -149,  -182,  1771,  1507,  -803,   908,  -821,  1572,   440,
    1410,  -819,  1314,     6,   494,  -821,   498,  1744,  -821,   398,
     646,  1436,  1437,   654,   647,   367,   657,   732,  -821,   875,
     120,  2042,    -5,  1315,   441,  -803,  -821,   731,  -827,   673,
       6,   738,     6,   351,  1811,  -821,     6,  -803,  -821,    -4,
      -6,  1536,   675,  1950,  1951,  1314,   342,  -821,   852,   733,
     739,  -828,   403,   404,   405,   406,   407,   408,   409,   410,
     411,  1145,   412,  -839,    11,  -115,  1315,   413,   414,  -149,
    -182,  -149,  -182,  1587,  -824,    10,   679,    34,   171,  -828,
     418,  2088,   104,   491,    95,  1685,    -7,   398,   424,   430,
    1174,  1224,    95,   369,   996,   963,  1208,  1608,     6,   609,
     398,   432,  -824,   611,   660,    16,   858,   636,    93,   863,
    -827,   624,  -827,  1524,  1256,   100,    44,    12,  1573,   399,
     625,    13,  2027,   918,   919,  2030,    14,    54,     6,   428,
     458,  1088,   259,   636,  1574,   135,  -827,   607,  1901,  1517,
     439,  1662,   462,  -115,   825,  -115,   470,   639,    15,  1072,
      54,    26,   319,   719,   398,  1059,  1060,  1700,   724,   899,
     726,   120,  1199,  1200,  1226,  1977,  1757,   959,  -801,  -820,
     510,  -801,  -801,   399,  -801,  -801,   518,  -801,   511,   524,
      44,    12,   181,    44,    12,    13,   661,  -820,    13,  1793,
      14,  -821,  -171,    14,  -821,   259,   319,   982,   216,   217,
     218,   219,   220,   221,   222,  1966,  -171,   164,  -125,   320,
    -171,   164,    15,  -828,   833,    15,  1112,   774,   428,  -827,
    1546,   782,  -125,  -113,  1086,  1113,   723,  -113,   723,   439,
     792,  1098,   794,   158,  -821,   164,  1301,  -821,  -825,  -124,
    -825,  -825,   632,  -825,  -803,    48,    55,  2002,   638,    49,
      56,   926,  1000,  -124,  -821,   930,  1001,  -821,  -822,  1383,
     986,    26,    48,   665,   749,  1196,    49,  1755,   664,    55,
    -171,  -171,  -171,    56,  -171,  -231,  1197,  1151,  -171,   780,
    1669,   744,   398,  1253,  -231,   171,  -125,  -125,  -125,   399,
    -125,   867,  -113,    12,  -125,   904,   865,    13,  -803,  -803,
      13,    74,    14,   700,    75,    14,  1057,  1803,  -821,   800,
    -828,  -821,   362,  -171,  1627,   398,   817,  -124,  -124,  -124,
    -821,  -124,  -803,  -821,    15,  -124,   173,     6,  1619,  -125,
    1603,  -821,  -803,  -803,  -821,    44,   968,  -827,  -817,   716,
     717,   718,   397,  -821,   720,   721,  -821,     6,  1620,   121,
    -824,   728,   729,   850,   531,   910,  1847,   531,   740,   823,
    -124,  1016,  -821,   745,  1019,  2086,  -824,   805,   902,   399,
     180,   181,   120,   173,  1293,  -821,   831,  -821,  -803,   255,
    1029,   755,  1030,  -803,  -821,  1192,  -821,  1159,  -803,   766,
     851,  1153,   259,  1536,  1198,  1292,     9,  -803,   399,   636,
     382,     6,   977,  1093,     9,     9,   598,  1131,   600,   787,
     788,   605,  1102,     6,   319,  1219,   804,  1329,  1900,   448,
     877,   450,   990,   882,     6,   453,   909,  1090,  1223,   320,
    1434,   819,   999,  1435,   382,   815,     6,   818,   953,   827,
    1190,  1029,   829,  1030,   830,  2075,  1922,  1923,   398,   985,
     828,  -830,  1304,  1029,  2076,  1030,   446,   181,   182,   183,
     184,   185,   399,   186,   983,   399,   319,   259,   120,   173,
    1031,  1032,   369,  1764,   463,  1766,   934,   935,  -828,  -828,
     330,   779,  1061,  -821,  1092,  1964,  -821,  -825,   480,  -825,
    -825,  -839,  -825,   872,  1903,  1130,   874,  1132,  1410,  1592,
    1170,   879,    28,    29,  1351,  1353,  1354,  -113,  -113,  -113,
     885,   668,   528,  -815,  1058,  1099,   543,    80,  1404,    81,
      82,  -113,    83,   898,  1408,  -821,  1366,   627,  -827,   618,
    2031,   906,  1280,  1080,  1282,  -821,   -93,  1218,  1827,   912,
     913,  -827,  1121,   631,   187,  -828,  -824,  -821,   398,   921,
    1048,   669,   750,   761,   795,   929,  1262,   670,   932,  1489,
    1026,  -827,   798,   805,  1446,   938,  1088,  -837,  -837,  -837,
    -828,   639,  -820,  1415,  1258,   859,  -113,   419,   887,  -827,
    -827,  -827,  -827,   904,   636,  1862,  1425,   948,  1097,  1051,
     399,  1100,  -827,   120,  1147,   132,   431,   369,  1124,  -834,
    -826,  1347,  1149,  1111,   956,  1212,  1222,  -828,  -821,  -821,
    -821,  -821,  -821,   992,  -821,   994,   995,  -828,  1272,  -162,
     998,  1284,  -828,  -827,  1112,  1004,  1013,  1303,   460,  2003,
    1421,  -825,  1376,  1490,  1014,   126,   668,  -821,  -821,  -821,
    -821,  -821,  1000,  -821,  -825,  -828,  1263,  1025,  1211,  1472,
     732,  1302,  1509,  1561,  -828,  1161,   490,   180,  1361,  -817,
    1669,  -828,   499,  -828,   501,   502,  2014,  2015,  1409,  1054,
    1793,  -834,   509,  -828,  -837,    92,   669,  1416,  1234,    99,
     519,  1420,   670,  1459,  1138,  1499,  1249,    70,    71,  1502,
     765,   436,   520,   536,   525,  -821,  1128,  1129,  1886,  1887,
    1849,  1492,  1945,  2024,  1666,  1095,  1560,   604,  1375,  1797,
    1413,   180,  1597,  1809,  1270,  1928,  1686,   961,  1108,  1602,
    2060,  1110,  1946,  2007,  1687,  2011,  1289,   962,  1083,  1178,
    1120,  1244,  1591,  1122,  1358,  1247,  1359,  1448,  1838,  1839,
    1547,  1875,  1876,  1598,  1225,  1386,  1360,  1140,  1362,  1179,
    2012,    72,   653,   817,  1438,  1729,  1526,   387,  1693,  1440,
     180,   181,   182,   183,   184,   185,  1761,   186,  1816,  1326,
     674,  1585,   330,  1859,  1765,  1827,  1973,  2032,   351,  2013,
    1986,  1432,  1981,  1085,  2074,  1175,  1462,  1821,  1710,   330,
    1601,   902,  1381,   806,  1276,  1697,  1506,   698,  1330,   723,
    1164,  1529,  1455,  1605,  1457,  1458,  1530,  1531,  2061,  1532,
    1456,  1195,  1418,  1533,  1870,  1300,  1372,  1384,  1583,  1929,
    1698,  1781,  1433,  1667,   897,  1477,    66,   303,  1629,  1699,
      51,    57,  1932,    67,  1480,  1380,  1257,  1851,    68,    69,
    1752,  1646,    73,  1063,  1684,  1510,  1398,    51,   826,  1414,
       0,     0,     0,  1365,    57,     0,     0,     0,     0,     0,
    1235,   746,     0,     0,  1345,  1357,     0,  1818,     0,     0,
    1945,     0,  1673,     0,     0,     0,  1463,  1251,     0,  1676,
    1111,     0,  1111,  1633,  1387,  1260,  1681,     0,     0,  1391,
    1946,     0,     0,     0,  1111,     0,  1271,   180,  1654,     0,
     180,   966,  1275,   972,     0,     0,   967,     0,     0,     0,
       0,     0,  1283,     0,     0,  1749,     0,     0,     0,     0,
     197,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1904,     0,     0,     0,     0,  1307,     0,   820,     0,   877,
    1471,     0,     0,  1388,  1389,     0,     0,     0,  1739,  1325,
     180,     0,  1735,     0,     0,   836,     0,     0,  1709,  1543,
    1703,     0,  1335,     0,     0,  1753,  1346,  1338,     0,     0,
       0,  1498,  1344,   328,     0,     0,     0,     0,  2010,     0,
       0,     0,  1289,  1731,     0,     0,     0,  1679,     0,     0,
       0,  1810,     0,  1364,   180,  1081,     0,     0,  1971,     0,
       0,  1612,     0,     0,     0,     0,     0,     0,   399,   636,
    -816,   120,  1379,     0,     0,     0,  1476,     0,     0,     0,
       0,     0,     0,     0,  1625,   324,   128,  1395,     0,  1390,
    1805,     0,  1393,     0,     0,  1405,     0,     0,     0,   328,
       0,  1469,     0,     0,  1466,  1541,     0,   357,  1139,     0,
    1141,     0,     0,     0,   668,     0,     0,   955,   956,  1422,
       0,  1398,     0,  1424,     0,     0,     0,     0,  1795,   966,
       0,   972,     0,     0,   967,  1431,  1833,   330,  1528,  1171,
       0,     0,     0,     0,     0,  1609,     0,     0,     0,     0,
     132,  1856,  1578,     0,   669,  1439,  1688,     0,     0,   957,
     670,     0,  1549,     0,  1559,     0,     0,     0,   958,     0,
    1829,  1707,  1565,  1677,   120,     0,  1568,     0,     0,     0,
       0,     0,     0,     0,  1545,  1616,   421,  1011,     0,     0,
    1691,     0,     0,     0,  1017,     0,     0,  1890,   271,   272,
    1705,     0,   273,     0,     0,   274,     0,   275,     0,  1041,
       0,   276,   277,   278,     0,     0,     0,   445,   423,     0,
       0,     0,  1869,  1767,  1768,   452,     0,     0,     0,   442,
    1505,     0,  1775,     0,     0,     0,  1644,  1511,  1512,     0,
    1514,  2000,  1940,     0,  1519,     0,     0,     0,   469,     0,
       0,     0,  1525,  1658,     0,     0,     0,  2103,     0,     0,
    1663,  1649,     0,  1956,     0,   492,     0,   497,  1861,     0,
     497,     0,  1551,   135,     0,  1728,  1814,  1841,     0,     0,
    1723,     0,   423,   515,  1819,     0,  1558,     0,     0,     0,
       0,     0,     0,  1305,     0,  1894,     0,   534,  1668,     0,
       0,     0,     0,     0,  1582,     0,   523,     0,  1707,     0,
    1586,     0,  1589,   603,  1997,  1609,   606,   539,     0,     0,
     542,     0,     0,  1878,  2053,     0,  1881,  1600,     0,  1168,
    1812,   623,     0,     0,  1607,     0,  1543,     0,     0,     0,
       0,     0,     0,     0,     0,  1715,  1617,  1992,     0,  1787,
    2033,  2035,  2036,     0,     0,     0,     0,     0,     0,   652,
       0,     0,     0,  1915,     0,  1840,     0,  1842,   180,  1743,
    1895,     0,     0,     0,     0,  2009,  1750,     0,  1645,     0,
    2018,     0,     0,     0,  1970,   821,     0,     0,     0,     0,
       0,     0,   687,     0,  2073,  1659,  1660,  1661,     0,     0,
       0,   682,  1664,  1665,     0,  1221,     0,     0,     0,  1672,
    1940,  1877,  1675,  1879,     0,  2046,  1882,   695,  1930,  1683,
    1785,     0,  1541,     0,     0,  2059,     0,     0,     0,     0,
       0,     0,     0,     0,  1690,  1692,     0,  1694,     0,     0,
       0,     0,     0,  1801,  2028,     0,     0,  1743,  1708,     0,
       0,     0,     0,  1916,     0,     0,     0,     0,     0,     0,
     603,     0,   603,  2083,   743,     0,     0,   603,  1450,     0,
    1844,     0,     0,  1616,  2091,  1792,     0,  1738,  1296,     0,
    1976,     0,     0,  1947,     0,   758,     0,     0,     0,     0,
       0,   425,  2110,     0,     0,     0,  1952,     0,     0,     0,
       0,  1545,  1041,  1041,  1754,     0,  1756,     0,     0,  1872,
    1743,     0,     0,     0,   791,  1762,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   803,     0,     0,     0,     0,
       0,     0,     0,     0,  2039,   324,     0,  1979,     0,     0,
       0,  1513,  1784,     0,  2055,   822,  2057,  2058,  1090,     0,
       0,     0,  2056,  2008,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1921,     0,  1802,     0,     0,  1874,  1806,
       0,  1808,     0,   180,     0,     0,  1813,     0,  1815,   853,
       0,   855,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   869,     0,     0,     0,  1959,     0,     0,
     743,   580,  1830,     0,  1832,  1092,     0,     0,     0,     0,
       0,  2102,     0,     0,  1846,     0,     0,   623,     0,     0,
     894,     0,   615,     0,     0,  1926,     0,     0,   497,     0,
       0,     0,     0,  1860,     0,  1934,     0,     0,     0,   123,
     124,   125,   126,   127,     0,     0,  1865,     0,  1949,   128,
       0,  1947,   129,   924,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1952,     0,   723,     0,     0,     0,
    1117,     0,     0,  2025,  1898,     0,   822,     0,   180,     0,
       0,  1902,     0,     0,   130,     0,  2080,     0,  1910,     0,
       0,  1993,   131,     0,     0,   951,     0,     0,  1743,     0,
       0,  1743,  2049,     0,   981,     0,  1111,     0,     0,     0,
    1111,  2101,   988,   132,  1927,     0,     0,     0,     0,  2107,
    1931,   133,  1933,     0,     0,  2019,     0,   134,     0,   330,
       0,  1955,  2026,   135,     0,     0,     0,     0,     0,  1518,
       0,     0,     0,     0,     0,  1007,  1041,   469,  1968,  1969,
       0,     0,     0,  1972,     0,     0,  1978,     0,  1980,  2047,
    1047,     0,     0,     0,     0,     0,     0,  1988,     0,     0,
       0,     0,     0,     0,     0,   497,     0,     0,     0,  1995,
       0,     0,     0,     0,     0,     0,  1069,  1070,     0,     0,
       0,   497,  2004,     0,     0,   330,     0,     0,     0,  1228,
     515,  2079,  1233,     0,     0,     0,     0,     0,     0,     0,
       0,  2021,     0,  2023,     0,  2089,  1949,     0,     0,     0,
    2029,     0,     0,     0,     0,     0,     0,     0,     0,  1261,
     822,  1101,  2038,     0,  1613,     0,  2108,  1268,  1269,     0,
       0,     0,  2112,  1133,     0,     0,     0,     0,     0,     0,
       0,     0,  1142,     0,     0,     0,   981,     0,     0,     0,
     497,  1154,     0,  1137,     0,  2067,     0,     0,     0,  2071,
       0,     0,     0,     0,   854,     0,   856,   857,     0,     0,
     862,  1029,   864,  1030,   866,  1163,     0,   623,     0,  2082,
       0,     0,     0,   399,   636,  -816,   120,   173,  1031,  1032,
     369,     0,  2092,     0,     0,     0,     0,     0,     0,   497,
    2100,   128,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,     0,     0,     0,     0,     0,     0,     0,   668,
     490,   497,   955,   956,     0,   232,     0,   235,   237,   239,
    1373,     0,     0,     0,     0,     0,     0,     0,   251,   822,
       0,     0,   822,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,  1232,     0,   669,
       0,     0,   324,     0,   957,   670,     0,     0,     0,   822,
       0,     0,  1747,   958,     0,     0,     0,   822,   822,     0,
       0,     0,     0,     0,     0,     0,  1758,   317,     0,   991,
       0,   993,     0,     0,     0,     0,     0,     0,     0,     0,
    1291,     0,  1770,  1772,     0,     0,     0,     0,     0,  1012,
       0,     0,     0,     0,     0,     0,     0,   758,     0,  1021,
       0,     0,     0,     0,     0,     0,   364,     0,     0,   180,
       0,     0,     0,     0,   497,     0,     0,     0,  1460,  1461,
       0,     0,     0,     0,   652,     0,  1467,     0,  1056,     0,
       0,  1340,   396,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1485,     0,  1363,  1488,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1232,     0,     0,
     822,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1118,     0,  1858,     0,     0,
       0,     0,   429,     0,     0,     0,     0,     0,     0,     0,
    1400,     0,     0,     0,     0,     0,     0,   444,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1885,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   894,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1569,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   652,   652,  1919,  1189,   924,     0,     0,
    1209,     0,  1445,     0,     0,     0,   497,     0,     0,     0,
       0,     0,     0,   512,     0,     0,     0,     0,   822,   822,
       0,     0,     0,     0,  1948,     0,   822,     0,     0,     0,
       0,     0,     0,     0,   541,     0,     0,     0,     0,     0,
       0,     0,     0,  1468,     0,     0,     0,     0,     0,  1482,
       0,     0,   822,     0,     0,   822,     0,     0,   324,     0,
     616,     0,  1495,     0,     0,     0,     0,     0,     0,  1486,
    1653,     0,     0,     0,     0,  1252,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2006,     0,     0,     0,     0,     0,     0,  1274,  1041,
    1041,  1069,     0,     0,     0,     0,   666,     0,     0,     0,
       0,   497,     0,     0,   681,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   689,     0,     0,   692,     0,
     694,     0,     0,   696,   697,     0,     0,     0,     0,     0,
     822,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   714,     0,  1041,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1291,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1611,
       0,     0,   330,     0,     0,     0,     0,     0,  1007,     0,
     737,     0,   330,     0,     0,     0,     0,     0,     0,     0,
     747,     0,     0,     0,     0,   497,     0,   330,     0,     0,
       0,     0,   753,     0,     0,   330,     0,     0,     0,  1643,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     822,  1637,     0,     0,   775,  1642,     0,     0,     0,     0,
       0,   786,     0,  1651,  -817,     0,  -817,     0,  1400,     0,
       0,     0,     0,     0,   801,     0,  -817,     0,     0,  -817,
    -817,  -817,  -817,  -817,     0,     0,     0,     0,     0,     0,
     545,     0,     0,   822,     0,     0,     0,     0,     0,  1340,
       0,     0,  1680,     0,     0,     0,  1831,     0,     0,   832,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1717,
       0,     0,  -817,     0,     0,  1453,     0,     0,   546,     0,
       0,     0,     0,     0,     0,   547,     0,     0,   548,     0,
    1734,  -818,  1724,   868,   549,     0,     0,  1727,   550,   551,
       0,     0,     0,  -817,     0,     0,     0,   880,     0,   552,
       0,  1736,  -817,     0,   553,   554,     0,     0,  -817,   890,
       0,   555,     0,  -818,     0,     0,  1445,     0,     0,   556,
     557,     0,     0,  1501,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1515,  1780,
       0,     0,   920,     0,  1783,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1495,     0,     0,   943,  1791,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1611,     0,     0,     0,
       0,  1800,     0,     0,     0,     0,   987,     0,     0,   989,
       0,     0,  1567,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1828,     0,     0,     0,   822,     0,     0,     0,
       0,  1590,     0,  1843,  1637,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1837,  1791,     0,     0,  1015,
    1606,     0,     0,  1018,     0,  1020,     0,   497,     0,     0,
       0,     0,     0,     0,     0,  1027,     0,     0,     0,     0,
    1868,     0,     0,     0,     0,     0,     0,     0,     0,  1880,
       0,     0,     0,     0,     0,     0,  1643,     0,     0,     0,
       0,     0,  1837,     0,     0,  1065,     0,     0,     0,     0,
       0,     0,  1642,  1888,     0,     0,     0,   623,  1082,     0,
    1909,     0,     0,     0,  1912,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1107,  1109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   469,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1960,  1143,     0,  1146,  1453,     0,     0,
       0,     0,     0,   497,     0,   623,     0,     0,  1162,     0,
       0,  1725,     0,     0,     0,     0,     0,     0,     0,  1987,
       0,     0,     0,  1172,     0,     0,     0,     0,     0,     0,
    1737,  1994,     0,     0,     0,     0,     0,  1184,     0,     0,
       0,  1746,  1991,     0,   758,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2017,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1204,     0,     0,     0,     0,     0,   894,     0,  1773,  1774,
       0,     0,     0,     0,     0,  1216,     0,     0,     0,  2040,
       0,     0,     0,     0,     0,     0,     0,  2050,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2062,     0,     0,     0,     0,     0,     0,     0,
       0,  2070,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1264,  2077,  1265,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1294,  1295,     0,  1850,     0,     0,
       0,     0,  1854,     0,  1855,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1324,     0,     0,     0,     0,  1873,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1337,
       0,     0,  1889,  1341,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1367,     0,  1368,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1953,  1394,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1417,     0,     0,
       0,     0,     0,     0,  1423,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2001,     0,     0,     0,     0,     0,
       0,     0,  1441,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1464,     0,  1465,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1474,     0,
       0,     0,     0,     0,  1479,     0,     0,  2044,     0,     0,
       0,  1483,  1484,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2063,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1534,     0,     0,     0,
       0,     0,     0,     0,  1548,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1556,     0,     0,  1557,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1579,  1580,     0,     0,
       0,     0,  1584,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1594,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1604,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1622,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1634,     0,  1638,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1655,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,    65,     0,  1671,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1689,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1719,  1720,  1721,  1722,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,  1730,     0,     0,
       0,     0,     0,     0,     0,   169,   170,     0,     0,   175,
     176,   177,   178,     0,     0,     0,     0,     0,     0,     0,
     200,  1748,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1760,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1777,
       0,   249,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1799,     0,     0,     0,     0,
       0,     0,     0,     0,  1807,     0,   302,     0,     0,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,     0,     0,     0,  1820,     0,     0,     0,     0,
       0,  1822,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1845,     0,     0,
     352,     0,     0,     0,     0,     0,     0,  1853,   361,     0,
       0,     0,     0,     0,     0,   368,     0,     0,     0,     0,
     375,   376,   377,   378,   379,   380,   381,  1863,     0,     0,
       0,     0,     0,     0,  1871,     0,     0,     0,     0,     0,
       0,     0,     0,  1883,     0,  1884,     0,     0,     0,     0,
       0,  1891,  1892,  1893,     0,     0,   247,   247,     0,     0,
    1899,     0,     0,     0,     0,     0,     0,     0,     0,  1905,
       0,     0,     0,     0,     0,     0,   157,  1914,     0,     0,
    1917,     0,  1918,     0,     0,  1920,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1935,     0,     0,     0,  1954,     0,     0,  1957,     0,  1958,
       0,     0,     0,     0,   455,  1961,  1962,  1963,     0,  1965,
     459,  1967,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1982,     0,   471,   472,   473,   474,
     475,   476,   477,   478,     0,     0,  1989,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1996,     0,  1998,  1999,
       0,     0,     0,   507,   508,     0,     0,     0,     0,     0,
     516,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2022,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   608,     0,     0,     0,   610,
       0,     0,     0,  2043,     0,  2045,     0,  2048,     0,  2051,
    2052,     0,     0,  2054,     0,     0,     0,     0,     0,   373,
       0,     0,   633,   634,     0,  2064,     0,  2065,     0,     0,
    2068,  2069,     0,     0,  2072,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2078,     0,     0,     0,
       0,  2081,     0,     0,     0,     0,     0,  2084,  2085,     0,
     466,  2087,     0,  2090,     0,     0,     0,     0,     0,  2095,
    2096,  2097,  2098,  2099,     0,     0,     0,     0,     0,  2105,
       0,  2106,     0,     0,  2109,     0,   504,     0,  2111,     0,
    2113,  2114,  2115,     0,  2116,  2117,  2118,  2119,     0,     0,
    2120,     0,     0,     0,     0,     0,   527,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   736,     0,     0,     0,     0,   614,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     748,     0,     0,     0,     0,   479,     0,     0,     0,     0,
       0,     0,     0,     0,   759,     0,     0,     0,     0,     0,
       0,     0,     0,   769,   655,     0,     0,     0,   659,     0,
       0,     0,   517,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   824,     0,     0,   617,     0,     0,     0,
       0,     0,     0,     0,     0,   715,     0,   835,     0,   837,
     838,   839,   840,   841,   842,   843,   844,   845,   846,   847,
     848,   849,   643,     0,     0,     0,     0,     0,     0,     0,
     656,     0,   658,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   667,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     886,     0,     0,     0,     0,     0,     0,     0,   895,     0,
       0,     0,     0,     0,     0,     0,     0,   768,   903,     0,
       0,   907,     0,     0,     0,     0,   776,     0,     0,     0,
       0,     0,   916,   917,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   802,     0,     0,
       0,     0,     0,     0,     0,   939,   940,   941,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   754,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1009,  1010,
       0,     0,     0,     0,     0,   878,     0,     0,     0,     0,
     881,   247,  1022,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1046,   814,     0,     0,     0,     0,
       0,     0,   900,     0,     0,     0,     0,     0,     0,  1055,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1073,     0,   776,  1077,
       0,     0,   776,     0,     0,  1084,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   944,   945,     0,
       0,     0,     0,     0,   873,     0,   954,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   883,     0,
       0,     0,     0,     0,     0,   891,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1152,     0,     0,     0,     0,
       0,   914,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   925,     0,   927,     0,     0,     0,     0,     0,   933,
       0,     0,  1173,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1193,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1071,     0,     0,     0,     0,     0,  1079,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1087,     0,     0,
       0,  1094,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1227,     0,     0,     0,     0,     0,
       0,  1237,  1238,  1239,  1240,     0,     0,     0,     0,     0,
       0,  1028,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1144,     0,  1053,     0,
       0,     0,  1267,     0,     0,     0,     0,     0,     0,     0,
       0,  1066,     0,     0,     0,     0,     0,     0,     0,  1076,
    1169,     0,     0,  1286,  1287,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1187,  1188,     0,     0,     0,  1308,  1309,
    1310,  1311,     0,     0,     0,     0,     0,     0,     0,  1119,
       0,     0,  1328,     0,   776,   776,  1127,     0,     0,  1333,
       0,     0,  1135,  1136,     0,  1079,     0,     0,     0,     0,
       0,     0,     0,  1214,     0,     0,     0,     0,  1350,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1167,     0,     0,     0,     0,     0,  1236,     0,     0,
       0,     0,     0,  1369,  1370,     0,     0,     0,     0,     0,
       0,     0,  1250,  1185,     0,     0,     0,  1382,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1401,     0,  1403,
       0,  1202,     0,     0,     0,     0,  1205,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   944,     0,     0,  1323,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1248,     0,     0,     0,     0,     0,
       0,  1447,     0,     0,     0,  1214,     0,     0,     0,     0,
       0,     0,     0,  1349,     0,     0,     0,  1352,     0,  1355,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1298,     0,     0,     0,     0,     0,
    1487,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1500,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1331,  1332,
       0,     0,     0,     0,     0,  1520,  1521,  1522,  1523,     0,
    1342,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1356,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1378,     0,  1566,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1396,     0,     0,     0,     0,
       0,  1595,  1596,     0,  1407,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1442,     0,
       0,  1650,     0,     0,  1449,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1550,     0,
    1553,     0,   466,  1349,     0,     0,     0,     0,     0,  1478,
       0,     0,     0,     0,     0,  1481,     0,     0,     0,     0,
       0,     0,     0,     0,  1570,     0,     0,     0,     0,  1695,
    1581,     0,     0,     0,     0,     0,     0,     0,  1588,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1726,     0,     0,     0,     0,
       0,     0,     0,     0,  1732,  1733,     0,     0,     0,     0,
       0,     0,  1535,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1745,  1624,     0,     0,  1626,     0,     0,     0,
       0,     0,     0,  1630,     0,     0,     0,  1635,     0,   614,
    1452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1652,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1599,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1798,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1623,     0,     0,     0,
       0,  1817,  1706,     0,     0,     0,     0,     0,  1631,  1714,
       0,     0,     0,  1718,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1751,     0,     0,     0,
       0,     0,  1094,     0,     0,  1682,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1079,     0,
    1079,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1712,  1713,     0,  1782,     0,     0,     0,     0,     0,
    1786,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1796,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1924,  1740,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1214,
       0,  1214,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1776,     0,     0,     0,  1852,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1624,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2020,
       0,     0,     0,     0,     0,     0,     0,  1823,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1913,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1925,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1864,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1897,     0,     0,     0,     0,     0,
    1984,  1985,     0,     0,     0,  1906,     0,     0,  1990,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2005,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2016,     0,     0,     0,     0,  1936,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2034,     0,  2037,     0,   944,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1983,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2066,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1553,     0,   466,  1349,     0,     0,   944,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1635,
     614,  1452,     0,     0,     0,     0,     0,     0,     0,  2093,
    2094,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2104,     0,     0,     0,     0,     0,     0,  2041
};

static const short yycheck[] =
{
      21,    22,    23,    24,   225,   258,   117,   247,   515,   110,
     151,   152,   464,   154,   155,   156,   115,   197,   405,   160,
     238,   408,   166,   589,   240,   291,  1037,   197,   526,    50,
     811,   136,   243,   330,   822,   885,   822,   527,   367,   822,
     755,  1028,   252,  1004,   488,    66,    67,  1069,  1066,  1121,
     494,   447,  1175,  1307,   498,  1205,   673,    78,    79,   162,
    1070,  1028,   167,    51,  1555,  1457,    87,    88,    89,    90,
    1079,   215,  1624,  1108,  1458,  1110,  1296,   645,  1277,   637,
     638,  1145,   434,   412,   105,   414,   438,  1122,   291,  1187,
    1188,  1748,  1870,   359,  1398,  1909,  1909,  1083,  1600,   534,
     121,    69,  1202,   602,   451,  1550,   783,   818,    32,    20,
     254,   654,    11,    30,   657,   136,   445,    41,   321,   258,
      44,     8,   261,   144,   145,   146,   147,   148,   149,   150,
      41,  1625,   153,    44,   102,   103,    23,    31,   803,    78,
       5,   246,     6,   248,    28,     5,   167,    31,    30,     0,
      31,    29,  1414,   174,  1811,   365,    73,    41,   179,    41,
      44,    60,    44,    41,     9,   431,    44,   188,   520,     5,
     433,    65,    27,   436,   437,   196,     8,  1249,  1164,   531,
     119,   967,    27,  1192,   536,    72,  1631,    19,     5,    25,
    1692,    73,    73,   134,   975,   591,   224,   327,   226,   227,
     228,   229,   232,  1402,   869,   235,    61,   237,    25,   239,
     134,     6,     6,  1707,  1413,    31,   774,    82,  1480,   349,
      84,   108,    82,   134,   404,    41,   406,  1672,    44,     9,
      54,  1331,  1332,   499,    58,   256,   502,    93,   103,   738,
      31,  2019,     0,   103,   349,    31,    82,   594,    28,   515,
     134,   598,   134,   241,  1756,    41,   134,    73,    44,   135,
     136,    52,   515,  2077,  2077,    82,   234,   103,   716,   125,
     599,    78,   293,   294,   295,   296,   297,   298,   299,   300,
     301,   992,   303,    76,   136,     6,   103,   317,   318,    84,
      84,    86,    86,  1492,    78,   135,   517,    16,    20,   106,
     321,  2079,    31,   402,    97,  1609,     0,     9,   329,   339,
    1025,  1092,    97,    35,   858,   822,  1338,  1516,   134,   458,
       9,   342,   106,   462,   504,     7,   722,    29,    47,   725,
      27,   470,    29,  1431,  1122,    54,    94,    95,   113,    28,
     479,    99,  1999,   787,   788,  2002,   104,    29,   134,   337,
     371,    27,     8,    29,   129,   130,    53,   456,  1860,  1423,
     348,  1581,   383,    84,   686,    86,   387,    23,   126,   927,
      52,   134,     7,   583,     9,   918,   919,    53,   588,   766,
     590,    31,  1059,  1060,  1095,  1937,  1690,   822,    41,     4,
     418,    44,    45,    28,    47,    48,   424,    50,   419,   429,
      94,    95,    11,    94,    95,    99,   505,     4,    99,    24,
     104,    41,     6,   104,    44,     8,     7,    26,   137,   138,
     139,   140,   141,   142,   143,  1927,    20,    24,     6,    20,
      24,    24,   126,     4,   697,   126,   106,   640,   426,    10,
    1449,   644,    20,    88,   942,   115,   587,    92,   589,   437,
     653,   950,   655,    76,    41,    24,  1167,    44,    45,     6,
      47,    48,   483,    50,    31,    28,    29,  1969,   489,    28,
      29,   793,    88,    20,    41,   797,    92,    44,    91,  1260,
     832,   134,    45,   511,   614,  1053,    45,  1686,   509,    52,
      84,    85,    86,    52,    88,   106,  1054,   996,    92,   643,
      19,   604,     9,  1120,   115,    20,    84,    85,    86,    28,
      88,   729,    86,    95,    92,    22,   727,    99,    31,    86,
      99,    41,   104,   544,    44,   104,   913,  1747,    41,   659,
       4,    44,   251,   127,  1543,     9,   677,    84,    85,    86,
      41,    88,   109,    44,   126,    92,    32,   134,  1535,   127,
    1511,    41,    65,    31,    44,    94,   822,    28,    97,   580,
     581,   582,   281,    41,   585,   586,    44,   134,  1535,    94,
      31,   592,   593,   714,   433,   778,  1796,   436,   600,   684,
     127,   878,     3,   605,   881,  2076,    43,     3,   768,    28,
     115,    11,    31,    32,  1160,    16,   695,    18,    99,    91,
      16,   622,    18,   104,    16,  1049,    18,  1003,    86,   630,
     715,   998,     8,    52,  1058,  1159,     2,   107,    28,    29,
      30,   134,   825,   945,    10,    11,   448,   979,   450,   650,
     651,   453,   954,   134,     7,  1087,   664,  1195,  1858,   358,
     739,   360,   852,   746,   134,   364,   776,   944,  1092,    20,
    1327,   681,   863,  1330,    30,   676,   134,   678,   820,   689,
    1047,    16,   692,    18,   694,  2057,  1886,  1887,     9,   831,
     691,    31,  1171,    16,  2058,    18,    43,    11,    12,    13,
      14,    15,    28,    17,   828,    28,     7,     8,    31,    32,
      33,    34,    35,  1703,    21,  1705,   801,   802,    60,    61,
     225,    60,    61,    41,   944,  1925,    44,    45,    66,    47,
      48,    31,    50,   734,  1864,   978,   737,   980,    84,    85,
    1017,   742,    10,    11,  1222,  1223,  1224,    84,    85,    86,
     751,    74,    28,    31,   914,   951,    86,    45,  1282,    47,
      48,    20,    50,   764,  1288,   108,  1236,    71,     8,   105,
    2004,   772,  1148,   933,  1150,     3,    23,  1086,  1780,   780,
     781,    24,   972,   482,    98,     4,    51,    72,     9,   790,
     900,   114,   108,    57,     4,   796,  1128,   120,   799,  1396,
     891,     9,    79,     3,  1342,   806,    27,    28,    29,    30,
     107,    23,    75,  1292,  1123,    51,   127,   322,    72,    27,
      28,    29,    30,    22,    29,  1823,  1305,   107,   949,   122,
      28,   952,    21,    31,   121,   110,   341,    35,   112,    23,
      31,  1217,    83,   964,    78,    63,  1092,    43,    11,    12,
      13,    14,    15,   854,    17,   856,   857,    78,    75,    86,
     861,   127,    88,    23,   106,   866,   874,  1169,   373,  1972,
    1298,   102,    10,  1397,   875,    39,    74,    11,    12,    13,
      14,    15,    88,    17,   109,   106,  1129,   888,  1071,   102,
      93,  1168,  1416,   109,    19,  1005,   401,   402,  1230,     7,
      19,    10,   407,     6,   409,   410,  1984,  1985,     6,   910,
      24,    86,   417,    83,    30,    45,   114,  1293,  1101,    52,
     425,  1297,   120,  1355,   986,  1404,  1116,    38,    38,  1408,
     629,   346,   426,   437,   430,    98,   978,   978,  1844,  1844,
    1800,  1399,  1909,  1995,  1586,   946,  1470,   452,  1250,  1740,
    1290,   456,  1505,  1754,  1137,  1896,  1610,   822,   959,  1510,
    2038,   962,  1909,  1978,  1611,  1980,  1157,   822,   936,  1936,
     971,  1111,  1496,   974,  1229,  1114,  1229,  1344,  1788,  1788,
    1450,  1834,  1834,  1507,  1094,  1266,  1229,   988,  1231,  1936,
    1981,    41,   498,  1114,  1335,  1652,  1439,   270,  1617,  1338,
     505,    11,    12,    13,    14,    15,  1694,    17,  1762,  1191,
     515,  1490,   517,  1813,  1704,  2017,  1936,  2005,   986,  1983,
    1947,  1323,  1940,   938,  2055,  1026,  1358,  1776,  1631,   534,
    1509,  1191,  1258,   667,  1144,  1623,  1412,   542,  1198,  1160,
    1008,  1442,  1351,  1513,  1353,  1354,  1442,  1442,  2039,  1442,
    1352,  1052,  1295,  1442,  1831,  1166,  1247,  1260,  1486,  1897,
    1623,  1718,  1325,  1587,   763,  1378,    35,   199,  1547,  1623,
      28,    29,  1902,    35,  1383,  1256,  1122,  1802,    36,    37,
    1682,  1560,    41,   921,  1608,  1417,  1277,    45,   687,  1291,
      -1,    -1,    -1,  1235,    52,    -1,    -1,    -1,    -1,    -1,
    1101,   606,    -1,    -1,  1214,  1226,    -1,  1764,    -1,    -1,
    2077,    -1,  1591,    -1,    -1,    -1,  1359,  1118,    -1,  1598,
    1241,    -1,  1243,  1551,  1266,  1126,  1605,    -1,    -1,  1271,
    2077,    -1,    -1,    -1,  1255,    -1,  1137,   642,  1570,    -1,
     645,  1909,  1143,  1909,    -1,    -1,  1909,    -1,    -1,    -1,
      -1,    -1,  1153,    -1,    -1,  1679,    -1,    -1,    -1,    -1,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1865,    -1,    -1,    -1,    -1,  1176,    -1,   682,    -1,  1258,
    1371,    -1,    -1,  1268,  1269,    -1,    -1,    -1,  1667,  1190,
     695,    -1,  1662,    -1,    -1,   700,    -1,    -1,  1630,  1445,
    1624,    -1,  1203,    -1,    -1,  1684,  1216,  1208,    -1,    -1,
      -1,  1402,  1213,  1414,    -1,    -1,    -1,    -1,  1979,    -1,
      -1,    -1,  1413,  1655,    -1,    -1,    -1,  1603,    -1,    -1,
      -1,  1755,    -1,  1234,   739,   934,    -1,    -1,  1933,    -1,
      -1,  1518,    -1,    -1,    -1,    -1,    -1,    -1,    28,    29,
      30,    31,  1253,    -1,    -1,    -1,  1377,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1541,   223,    46,  1275,    -1,  1270,
    1749,    -1,  1273,    -1,    -1,  1283,    -1,    -1,    -1,  1480,
      -1,  1366,    -1,    -1,  1363,  1445,    -1,   245,   987,    -1,
     989,    -1,    -1,    -1,    74,    -1,    -1,    77,    78,  1300,
      -1,  1492,    -1,  1304,    -1,    -1,    -1,    -1,  1736,  2077,
      -1,  2077,    -1,    -1,  2077,  1316,  1786,   822,  1442,  1018,
      -1,    -1,    -1,    -1,    -1,  1516,    -1,    -1,    -1,    -1,
     110,  1810,  1482,    -1,   114,  1336,  1613,    -1,    -1,   119,
     120,    -1,  1452,    -1,  1468,    -1,    -1,    -1,   128,    -1,
    1782,  1628,  1473,  1599,    31,    -1,  1477,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1445,  1525,   324,   872,    -1,    -1,
    1616,    -1,    -1,    -1,   879,    -1,    -1,  1847,    55,    56,
    1626,    -1,    59,    -1,    -1,    62,    -1,    64,    -1,   894,
      -1,    68,    69,    70,    -1,    -1,    -1,   355,  1508,    -1,
      -1,    -1,  1830,  1705,  1706,   363,    -1,    -1,    -1,   351,
    1411,    -1,  1714,    -1,    -1,    -1,  1558,  1418,  1419,    -1,
    1421,  1967,  1909,    -1,  1425,    -1,    -1,    -1,   386,    -1,
      -1,    -1,  1433,  1575,    -1,    -1,    -1,  2094,    -1,    -1,
    1582,  1562,    -1,  1913,    -1,   403,    -1,   405,  1815,    -1,
     408,    -1,  1453,   130,    -1,  1651,  1758,  1789,    -1,    -1,
    1643,    -1,  1572,   421,  1766,    -1,  1467,    -1,    -1,    -1,
      -1,    -1,    -1,  1172,    -1,  1851,    -1,   435,  1588,    -1,
      -1,    -1,    -1,    -1,  1485,    -1,   428,    -1,  1765,    -1,
    1491,    -1,  1493,   451,  1964,  1686,   454,   439,    -1,    -1,
     442,    -1,    -1,  1835,  2028,    -1,  1838,  1508,    -1,  1014,
    1756,   469,    -1,    -1,  1515,    -1,  1762,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1635,  1527,  1955,    -1,  1727,
    2008,  2009,  2010,    -1,    -1,    -1,    -1,    -1,    -1,   497,
      -1,    -1,    -1,  1875,    -1,  1788,    -1,  1790,  1053,  1670,
    1852,    -1,    -1,    -1,    -1,  1979,  1680,    -1,  1559,    -1,
    1988,    -1,    -1,    -1,  1931,   683,    -1,    -1,    -1,    -1,
      -1,    -1,   530,    -1,  2053,  1576,  1577,  1578,    -1,    -1,
      -1,   523,  1583,  1584,    -1,  1090,    -1,    -1,    -1,  1590,
    2077,  1834,  1593,  1836,    -1,  2023,  1839,   539,  1900,  1607,
    1724,    -1,  1762,    -1,    -1,  2037,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1615,  1616,    -1,  1618,    -1,    -1,
      -1,    -1,    -1,  1744,  2000,    -1,    -1,  1748,  1629,    -1,
      -1,    -1,    -1,  1876,    -1,    -1,    -1,    -1,    -1,    -1,
     598,    -1,   600,  2071,   602,    -1,    -1,   605,  1347,    -1,
    1792,    -1,    -1,  1813,  2082,  1734,    -1,  1665,  1163,    -1,
    1937,    -1,    -1,  1909,    -1,   623,    -1,    -1,    -1,    -1,
      -1,  1948,  2104,    -1,    -1,    -1,  1909,    -1,    -1,    -1,
      -1,  1762,  1187,  1188,  1685,    -1,  1687,    -1,    -1,  1831,
    1811,    -1,    -1,    -1,   652,  1696,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   663,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2016,   673,    -1,  1937,    -1,    -1,
      -1,  1420,  1723,    -1,  2033,   683,  2035,  2036,  2005,    -1,
      -1,    -1,  2034,  1979,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1885,    -1,  1746,    -1,    -1,  1833,  1750,
      -1,  1752,    -1,  1258,    -1,    -1,  1757,    -1,  1759,   717,
      -1,   719,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   731,    -1,    -1,    -1,  1919,    -1,    -1,
     738,   445,  1783,    -1,  1785,  2005,    -1,    -1,    -1,    -1,
      -1,  2093,    -1,    -1,  1795,    -1,    -1,   755,    -1,    -1,
     758,    -1,   466,    -1,    -1,  1890,    -1,    -1,   766,    -1,
      -1,    -1,    -1,  1814,    -1,  1906,    -1,    -1,    -1,    36,
      37,    38,    39,    40,    -1,    -1,  1827,    -1,  1909,    46,
      -1,  2077,    49,   791,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2077,    -1,  1967,    -1,    -1,    -1,
     968,    -1,    -1,  1995,  1855,    -1,   814,    -1,  1363,    -1,
      -1,  1862,    -1,    -1,    81,    -1,  2067,    -1,  1869,    -1,
      -1,  1956,    89,    -1,    -1,   817,    -1,    -1,  1999,    -1,
      -1,  2002,  2024,    -1,   826,    -1,  2007,    -1,    -1,    -1,
    2011,  2092,   834,   110,  1895,    -1,    -1,    -1,    -1,  2100,
    1901,   118,  1903,    -1,    -1,  1990,    -1,   124,    -1,  1414,
      -1,  1912,  1997,   130,    -1,    -1,    -1,    -1,    -1,  1424,
      -1,    -1,    -1,    -1,    -1,   867,  1431,   885,  1929,  1930,
      -1,    -1,    -1,  1934,    -1,    -1,  1937,    -1,  1939,  2024,
     898,    -1,    -1,    -1,    -1,    -1,    -1,  1948,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,  1960,
      -1,    -1,    -1,    -1,    -1,    -1,   924,   925,    -1,    -1,
      -1,   929,  1973,    -1,    -1,  1480,    -1,    -1,    -1,  1097,
     938,  2066,  1100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1992,    -1,  1994,    -1,  2080,  2077,    -1,    -1,    -1,
    2001,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,
     968,   953,  2013,    -1,  1519,    -1,  2101,  1135,  1136,    -1,
      -1,    -1,  2107,   981,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   990,    -1,    -1,    -1,   978,    -1,    -1,    -1,
     998,   999,    -1,   985,    -1,  2046,    -1,    -1,    -1,  2050,
      -1,    -1,    -1,    -1,   718,    -1,   720,   721,    -1,    -1,
     724,    16,   726,    18,   728,  1007,    -1,  1025,    -1,  2070,
      -1,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    -1,  2083,    -1,    -1,    -1,    -1,    -1,    -1,  1047,
    2091,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1069,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
    1625,  1079,    77,    78,    -1,   152,    -1,   154,   155,   156,
    1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,  1097,
      -1,    -1,  1100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,    -1,  1099,    -1,   114,
      -1,    -1,  1120,    -1,   119,   120,    -1,    -1,    -1,  1127,
      -1,    -1,  1677,   128,    -1,    -1,    -1,  1135,  1136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1691,   214,    -1,   853,
      -1,   855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1158,    -1,  1707,  1708,    -1,    -1,    -1,    -1,    -1,   873,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1175,    -1,   883,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,  1734,
      -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,  1356,  1357,
      -1,    -1,    -1,    -1,  1202,    -1,  1364,    -1,   912,    -1,
      -1,  1209,   279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1390,    -1,  1232,  1393,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1229,    -1,    -1,
    1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   969,    -1,  1812,    -1,    -1,
      -1,    -1,   339,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1278,    -1,    -1,    -1,    -1,    -1,    -1,   354,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1843,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1307,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1478,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1331,  1332,  1880,  1040,  1335,    -1,    -1,
    1338,    -1,  1340,    -1,    -1,    -1,  1344,    -1,    -1,    -1,
      -1,    -1,    -1,   420,    -1,    -1,    -1,    -1,  1356,  1357,
      -1,    -1,    -1,    -1,  1909,    -1,  1364,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1365,    -1,    -1,    -1,    -1,    -1,  1387,
      -1,    -1,  1390,    -1,    -1,  1393,    -1,    -1,  1396,    -1,
     467,    -1,  1400,    -1,    -1,    -1,    -1,    -1,    -1,  1391,
    1568,    -1,    -1,    -1,    -1,  1119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1976,    -1,    -1,    -1,    -1,    -1,    -1,  1142,  1984,
    1985,  1439,    -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,
      -1,  1449,    -1,    -1,   521,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   532,    -1,    -1,   535,    -1,
     537,    -1,    -1,   540,   541,    -1,    -1,    -1,    -1,    -1,
    1478,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   558,    -1,  2038,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1505,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1517,
      -1,    -1,  2067,    -1,    -1,    -1,    -1,    -1,  1510,    -1,
     597,    -1,  2077,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     607,    -1,    -1,    -1,    -1,  1543,    -1,  2092,    -1,    -1,
      -1,    -1,   619,    -1,    -1,  2100,    -1,    -1,    -1,  1557,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1568,  1553,    -1,    -1,   641,  1557,    -1,    -1,    -1,    -1,
      -1,   648,    -1,  1565,    16,    -1,    18,    -1,  1586,    -1,
      -1,    -1,    -1,    -1,   661,    -1,    28,    -1,    -1,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,  1611,    -1,    -1,    -1,    -1,    -1,  1617,
      -1,    -1,  1604,    -1,    -1,    -1,  1784,    -1,    -1,   696,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1637,
      -1,    -1,    74,    -1,    -1,  1349,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,    -1,
    1658,    93,  1644,   730,    96,    -1,    -1,  1649,   100,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   744,    -1,   111,
      -1,  1663,   114,    -1,   116,   117,    -1,    -1,   120,   756,
      -1,   123,    -1,   125,    -1,    -1,  1694,    -1,    -1,   131,
     132,    -1,    -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1422,  1717,
      -1,    -1,   789,    -1,  1722,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1740,    -1,    -1,   812,  1728,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1754,    -1,    -1,    -1,
      -1,  1743,    -1,    -1,    -1,    -1,   833,    -1,    -1,   836,
      -1,    -1,  1476,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1780,    -1,    -1,    -1,  1784,    -1,    -1,    -1,
      -1,  1495,    -1,  1791,  1776,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1787,  1788,    -1,    -1,   876,
    1514,    -1,    -1,   880,    -1,   882,    -1,  1815,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   892,    -1,    -1,    -1,    -1,
    1828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,
      -1,    -1,    -1,    -1,    -1,    -1,  1844,    -1,    -1,    -1,
      -1,    -1,  1834,    -1,    -1,   922,    -1,    -1,    -1,    -1,
      -1,    -1,  1844,  1845,    -1,    -1,    -1,  1865,   935,    -1,
    1868,    -1,    -1,    -1,  1872,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   959,   960,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1902,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1921,   991,    -1,   993,  1631,    -1,    -1,
      -1,    -1,    -1,  1931,    -1,  1933,    -1,    -1,  1005,    -1,
      -1,  1645,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1947,
      -1,    -1,    -1,  1020,    -1,    -1,    -1,    -1,    -1,    -1,
    1664,  1959,    -1,    -1,    -1,    -1,    -1,  1034,    -1,    -1,
      -1,  1675,  1954,    -1,  1972,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1987,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1067,    -1,    -1,    -1,    -1,    -1,  2004,    -1,  1712,  1713,
      -1,    -1,    -1,    -1,    -1,  1082,    -1,    -1,    -1,  2017,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2025,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2040,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2049,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1130,  2062,  1132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1161,  1162,    -1,  1801,    -1,    -1,
      -1,    -1,  1806,    -1,  1808,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1190,    -1,    -1,    -1,    -1,  1832,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1206,
      -1,    -1,  1846,  1210,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1241,    -1,  1243,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1263,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1910,  1274,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1294,    -1,    -1,
      -1,    -1,    -1,    -1,  1301,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1968,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1339,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1360,    -1,  1362,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1375,    -1,
      -1,    -1,    -1,    -1,  1381,    -1,    -1,  2021,    -1,    -1,
      -1,  1388,  1389,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2041,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1443,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1451,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1463,    -1,    -1,  1466,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1483,  1484,    -1,    -1,
      -1,    -1,  1489,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1501,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1512,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1537,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1552,    -1,  1554,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    -1,  1590,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1615,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1639,  1640,  1641,  1642,    -1,    -1,    -1,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1654,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,   111,
     112,   113,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,  1678,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1693,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1716,
      -1,   163,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1742,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1751,    -1,   198,    -1,    -1,   201,
     202,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     212,   213,    -1,    -1,    -1,  1772,    -1,    -1,    -1,    -1,
      -1,  1778,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1794,    -1,    -1,
     242,    -1,    -1,    -1,    -1,    -1,    -1,  1804,   250,    -1,
      -1,    -1,    -1,    -1,    -1,   257,    -1,    -1,    -1,    -1,
     262,   263,   264,   265,   266,   267,   268,  1824,    -1,    -1,
      -1,    -1,    -1,    -1,  1831,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1840,    -1,  1842,    -1,    -1,    -1,    -1,
      -1,  1848,  1849,  1850,    -1,    -1,   161,   162,    -1,    -1,
    1857,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1866,
      -1,    -1,    -1,    -1,    -1,    -1,    97,  1874,    -1,    -1,
    1877,    -1,  1879,    -1,    -1,  1882,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1907,    -1,    -1,    -1,  1911,    -1,    -1,  1914,    -1,  1916,
      -1,    -1,    -1,    -1,   366,  1922,  1923,  1924,    -1,  1926,
     372,  1928,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1941,    -1,   388,   389,   390,   391,
     392,   393,   394,   395,    -1,    -1,  1953,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1963,    -1,  1965,  1966,
      -1,    -1,    -1,   415,   416,    -1,    -1,    -1,    -1,    -1,
     422,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1993,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   457,    -1,    -1,    -1,   461,
      -1,    -1,    -1,  2020,    -1,  2022,    -1,  2024,    -1,  2026,
    2027,    -1,    -1,  2030,    -1,    -1,    -1,    -1,    -1,   260,
      -1,    -1,   484,   485,    -1,  2042,    -1,  2044,    -1,    -1,
    2047,  2048,    -1,    -1,  2051,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2063,    -1,    -1,    -1,
      -1,  2068,    -1,    -1,    -1,    -1,    -1,  2074,  2075,    -1,
     385,  2078,    -1,  2080,    -1,    -1,    -1,    -1,    -1,  2086,
    2087,  2088,  2089,  2090,    -1,    -1,    -1,    -1,    -1,  2096,
      -1,  2098,    -1,    -1,  2101,    -1,   411,    -1,  2105,    -1,
    2107,  2108,  2109,    -1,  2111,  2112,  2113,  2114,    -1,    -1,
    2117,    -1,    -1,    -1,    -1,    -1,   431,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   596,    -1,    -1,    -1,    -1,   464,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     612,    -1,    -1,    -1,    -1,   396,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   626,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   635,   499,    -1,    -1,    -1,   503,    -1,
      -1,    -1,   423,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   685,    -1,    -1,   467,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   560,    -1,   699,    -1,   701,
     702,   703,   704,   705,   706,   707,   708,   709,   710,   711,
     712,   713,   493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     501,    -1,   503,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,   604,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   760,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   632,   770,    -1,
      -1,   773,    -1,    -1,    -1,    -1,   641,    -1,    -1,    -1,
      -1,    -1,   784,   785,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   807,   808,   809,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   619,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   870,   871,
      -1,    -1,    -1,    -1,    -1,   740,    -1,    -1,    -1,    -1,
     745,   746,   884,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   896,   676,    -1,    -1,    -1,    -1,
      -1,    -1,   767,    -1,    -1,    -1,    -1,    -1,    -1,   911,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   928,    -1,   793,   931,
      -1,    -1,   797,    -1,    -1,   937,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,   813,    -1,
      -1,    -1,    -1,    -1,   735,    -1,   821,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   749,    -1,
      -1,    -1,    -1,    -1,    -1,   756,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   997,    -1,    -1,    -1,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   792,    -1,   794,    -1,    -1,    -1,    -1,    -1,   800,
      -1,    -1,  1024,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1050,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,    -1,   932,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   942,    -1,    -1,
      -1,   946,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1096,    -1,    -1,    -1,    -1,    -1,
      -1,  1103,  1104,  1105,  1106,    -1,    -1,    -1,    -1,    -1,
      -1,   892,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   991,    -1,   909,    -1,
      -1,    -1,  1134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   922,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   930,
    1015,    -1,    -1,  1155,  1156,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1038,  1039,    -1,    -1,    -1,  1180,  1181,
    1182,  1183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   970,
      -1,    -1,  1194,    -1,  1059,  1060,   977,    -1,    -1,  1201,
      -1,    -1,   983,   984,    -1,  1070,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1078,    -1,    -1,    -1,    -1,  1220,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1012,    -1,    -1,    -1,    -1,    -1,  1102,    -1,    -1,
      -1,    -1,    -1,  1245,  1246,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1117,  1034,    -1,    -1,    -1,  1259,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1048,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1279,    -1,  1281,
      -1,  1062,    -1,    -1,    -1,    -1,  1067,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1186,    -1,    -1,  1189,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1115,    -1,    -1,    -1,    -1,    -1,
      -1,  1343,    -1,    -1,    -1,  1210,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1218,    -1,    -1,    -1,  1222,    -1,  1224,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1165,    -1,    -1,    -1,    -1,    -1,
    1392,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1406,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1199,  1200,
      -1,    -1,    -1,    -1,    -1,  1427,  1428,  1429,  1430,    -1,
    1211,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1225,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1252,    -1,  1475,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1348,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1276,    -1,    -1,    -1,    -1,
      -1,  1503,  1504,    -1,  1285,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1339,    -1,
      -1,  1563,    -1,    -1,  1345,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1453,    -1,
    1455,    -1,  1457,  1458,    -1,    -1,    -1,    -1,    -1,  1380,
      -1,    -1,    -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1479,    -1,    -1,    -1,    -1,  1621,
    1485,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1493,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1647,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1656,  1657,    -1,    -1,    -1,    -1,
      -1,    -1,  1443,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1674,  1538,    -1,    -1,  1541,    -1,    -1,    -1,
      -1,    -1,    -1,  1548,    -1,    -1,    -1,  1552,    -1,  1554,
    1555,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1508,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1741,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1537,    -1,    -1,    -1,
      -1,  1763,  1627,    -1,    -1,    -1,    -1,    -1,  1549,  1634,
      -1,    -1,    -1,  1638,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1681,    -1,    -1,    -1,
      -1,    -1,  1687,    -1,    -1,  1606,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1703,    -1,
    1705,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1632,  1633,    -1,  1719,    -1,    -1,    -1,    -1,    -1,
    1725,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1737,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1888,  1668,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1764,
      -1,  1766,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1715,    -1,    -1,    -1,  1803,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1816,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1991,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1778,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1873,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1889,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1824,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1855,    -1,    -1,    -1,    -1,    -1,
    1945,  1946,    -1,    -1,    -1,  1866,    -1,    -1,  1953,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1974,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1986,    -1,    -1,    -1,    -1,  1907,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2008,    -1,  2010,    -1,  2012,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1941,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2044,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2055,    -1,  2057,  2058,    -1,    -1,  2061,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2074,
    2075,  2076,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2084,
    2085,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2095,    -1,    -1,    -1,    -1,    -1,    -1,  2018
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned short yystos[] =
{
       0,   138,   139,   140,   141,     0,   134,   142,   643,   650,
     135,   136,    95,    99,   104,   126,   147,   148,   149,   150,
     151,   533,   541,   551,   595,   674,   134,   644,   142,   142,
     534,   542,   552,   596,   674,   664,   664,   664,   664,   265,
     647,   648,   657,   661,    94,   143,   144,   147,   155,   161,
     531,   653,   145,   146,   147,   155,   161,   653,   645,   646,
     647,   649,   650,   645,   645,   645,   541,   551,   653,   653,
     148,   149,   265,   646,    41,    44,   267,   270,   461,   463,
      45,    47,    48,    50,   266,   268,   271,   459,   465,   467,
     469,   532,   144,   674,   664,    97,   162,   537,   675,   146,
     674,   647,   664,   664,    31,   475,   475,   462,   464,   664,
     664,   460,   466,   468,   470,   664,   664,   664,   664,   645,
      31,   473,   538,    36,    37,    38,    39,    40,    46,    49,
      81,    89,   110,   118,   124,   130,   171,   250,   258,   260,
     261,   262,   263,   264,   441,   443,   449,   451,   453,   455,
     457,   505,   521,   563,   579,   591,   603,   657,    76,   254,
     495,   475,   475,   476,    24,   422,   664,   153,   658,   645,
     645,    20,   421,    32,   479,   645,   645,   645,   645,   378,
     473,    11,    12,    13,    14,    15,    17,    98,   337,   395,
     397,   399,   401,   403,   407,   539,   337,   653,   474,   664,
     645,   442,   444,   454,   452,   450,   456,   458,   506,   522,
     564,   580,   592,   604,   422,   664,   674,   674,   674,   674,
     674,   674,   674,   664,   664,   664,   664,   664,   664,   664,
     655,   660,   655,   660,   664,   655,   660,   655,   660,   655,
     660,   669,   496,   183,   660,   660,   152,   663,   152,   645,
     423,   655,   395,   422,   664,    91,   525,   480,   664,     8,
     389,   664,   396,   398,   400,   402,   404,   408,   540,   664,
     664,    55,    56,    59,    62,    64,    68,    69,    70,   272,
     273,   277,   278,   279,   280,   283,   284,   285,   286,   287,
     288,   325,   473,   603,   609,   613,   615,   621,   627,   631,
     637,   639,   645,   537,   645,   645,   645,   645,   645,   645,
     645,   645,   645,   645,   645,   645,   645,   655,   395,     7,
      20,   366,   387,   419,   653,   419,    73,   224,   225,   259,
     473,   489,   419,   419,   419,   419,   666,   668,   666,   169,
     170,   675,   475,   666,   667,   666,   667,   666,   668,   156,
     157,   675,   645,   661,   186,   187,   252,   653,   422,   673,
     422,   645,   674,   658,   655,   395,   526,   664,   645,    35,
     421,   485,   390,   657,   421,   645,   645,   645,   645,   645,
     645,   645,    30,   269,   330,   439,   653,   269,   614,   616,
     622,   628,   632,   638,   640,   610,   655,   674,     9,    28,
     387,   391,   433,   664,   664,   664,   664,   664,   664,   664,
     664,   664,   664,   666,   666,   388,   420,   387,   664,   473,
     367,   653,   490,   389,   664,   391,   163,   164,   675,   655,
     666,   473,   664,   165,   166,   651,   165,   167,   168,   675,
     389,   422,   651,   676,   655,   653,    43,   447,   674,   433,
     674,   154,   653,   674,   658,   645,   652,   486,   664,   645,
     473,   440,   664,    21,   371,   413,   663,   331,   332,   653,
     664,   645,   645,   645,   645,   645,   645,   645,   645,   657,
      66,   274,   317,   635,   392,   434,    65,   315,   325,   633,
     473,   378,   653,   281,   325,   290,   291,   653,   325,   473,
     290,   473,   473,   326,   663,   652,   652,   645,   645,   473,
     419,   664,   655,   368,   369,   653,   645,   657,   419,   473,
     164,   670,   671,   651,   666,   170,   433,   663,    28,   172,
     435,   166,   670,   671,   653,   670,   168,   670,   671,   651,
     665,   655,   651,    86,   515,    42,    80,    87,    90,    96,
     100,   101,   111,   116,   117,   123,   131,   132,   188,   189,
     190,   191,   192,   193,   194,   199,   200,   201,   203,   207,
     208,   209,   210,   211,   212,   213,   216,   219,   220,   255,
     330,   445,   503,   517,   523,   535,   543,   545,   565,   575,
     577,   589,   605,   607,   652,   654,   448,   183,   154,   659,
     154,   155,   160,   653,   473,   154,   653,   378,   645,   421,
     645,   421,   414,   413,   663,   330,   655,   657,   105,   333,
     334,   335,   553,   653,   421,   421,   636,    71,   275,   321,
     641,   674,   664,   645,   645,   634,    29,   437,   664,    23,
     417,   313,   663,   657,   282,   437,    54,    58,   292,   293,
     611,   619,   653,   282,   433,   663,   657,   433,   657,   663,
     325,   378,   159,   160,   664,   419,   655,   657,    74,   114,
     120,   232,   370,   433,   473,   485,   491,   571,   583,   225,
     671,   655,   651,   659,   676,   436,   175,   653,   671,   655,
     225,   228,   655,   671,   655,   651,   655,   655,   473,   516,
     664,   446,   504,   518,   524,   536,   544,   546,   566,   576,
     578,   590,   606,   608,   655,   663,   664,   664,   664,   658,
     664,   664,   256,   660,   658,   256,   658,   183,   664,   664,
     158,   160,    93,   125,   529,   593,   645,   655,   160,   652,
     159,   161,   515,   653,   152,   159,   473,   655,   645,   389,
     108,   559,   554,   655,   657,   664,   336,   338,   653,   645,
     642,    57,   276,   322,   617,   674,   664,   318,   663,   645,
     438,   315,   315,   418,   387,   655,   663,   314,   378,    60,
     395,   623,   387,   314,   612,   620,   655,   664,   664,   294,
     296,   653,   387,   313,   387,     4,   381,   313,    79,   501,
     389,   655,   663,   653,   419,     3,   379,   492,   572,   584,
     365,   673,   347,   366,   657,   664,   238,   660,   664,   666,
     473,   227,   653,   422,   645,   417,   669,   666,   664,   666,
     666,   378,   655,   670,   656,   645,   473,   645,   645,   645,
     645,   645,   645,   645,   645,   645,   645,   645,   645,   645,
     660,   422,   381,   653,   330,   653,   330,   330,   183,    51,
     257,   471,   330,   183,   330,   661,   330,   667,   655,   653,
     530,   594,   664,   657,   664,   515,   377,   378,   663,   664,
     655,   663,   152,   657,   560,   664,   645,    72,   487,   334,
     655,   657,   339,   341,   653,   645,   618,   674,   664,   290,
     663,   319,   325,   645,    22,   415,   664,   645,   315,   389,
     387,   624,   664,   664,   657,   415,   645,   645,   437,   437,
     655,   664,   297,   298,   653,   657,   417,   657,   382,   664,
     417,   502,   664,   657,   422,   422,   162,   380,   664,   645,
     645,   645,   439,   655,   663,   663,   226,   227,   107,   557,
     672,   651,   557,   656,   663,    77,    78,   119,   128,   228,
     229,   230,   231,   232,   233,   234,   237,   263,   433,   437,
     497,   499,   563,   581,   599,   652,   173,   387,   176,   177,
     178,   651,    26,   395,   427,   656,   671,   655,   651,   655,
     658,   330,   664,   330,   664,   664,   672,   472,   664,   661,
      88,    92,   519,   527,   664,   221,   222,   651,   162,   645,
     645,   473,   330,   419,   664,   655,   391,   473,   655,   391,
     655,   330,   645,   332,   488,   664,   337,   655,   657,    16,
      18,    33,    34,   232,   342,   344,   345,   346,   405,   409,
     433,   473,   479,   481,   483,   485,   645,   653,   389,   320,
     416,   122,   587,   657,   664,   645,   330,   290,   325,   313,
     313,    61,   295,   623,   625,   655,   657,   299,   300,   653,
     653,   663,   315,   645,   289,   290,   657,   645,   311,   663,
     325,   674,   655,   675,   645,   369,   659,   663,    27,   348,
     391,   429,   673,   417,   663,   664,   558,   660,   515,   668,
     660,   651,   417,   498,   500,   582,   600,   655,   664,   655,
     664,   660,   106,   115,   555,   573,   563,   227,   330,   657,
     664,   658,   664,   439,   112,   174,   567,   657,   177,   178,
     670,   671,   670,   653,   428,   657,   657,   651,   157,   674,
     664,   674,   653,   655,   663,   557,   655,   121,   585,    83,
     509,   515,   645,   290,   653,   520,   528,   214,   662,   183,
     509,   389,   655,   651,   675,   253,   664,   657,   473,   663,
     391,   674,   655,   645,   334,   664,   340,   379,   405,   409,
     406,   410,   482,   484,   655,   657,   347,   663,   663,   330,
     290,   657,   437,   645,   588,   664,   314,   315,   437,   415,
     415,   626,   657,   559,   655,   657,   301,   302,   553,   653,
     311,   387,    63,   629,   663,   289,   655,   254,   652,   413,
     430,   473,   433,   437,   439,   389,   557,   645,   227,   239,
     240,   241,   651,   227,   387,   664,   663,   645,   645,   645,
     645,   233,   237,   233,   234,   556,   574,   238,   657,   658,
     663,   664,   330,   366,   172,   233,   237,   581,   652,   568,
     664,   227,   671,   670,   655,   655,   179,   645,   227,   227,
     387,   664,    75,   493,   330,   664,   389,   195,   662,   586,
     183,   510,   183,   664,   127,   597,   645,   645,   204,   661,
     215,   653,   672,   256,   655,   655,   473,   254,   657,   251,
     447,   557,   391,   417,   515,   674,   338,   664,   645,   645,
     645,   645,     5,    25,    82,   103,   343,   383,   425,   507,
     549,   344,   344,   663,   655,   664,   319,   289,   645,   315,
     325,   657,   657,   645,   293,   664,   487,   655,   664,   303,
     653,   655,   657,   630,   664,   389,   666,   183,   374,   663,
     645,   659,   663,   659,   659,   663,   657,   660,   240,   241,
     670,   671,   670,   653,   664,   656,   676,   655,   655,   645,
     645,   235,   661,   227,   172,   417,    10,   393,   657,   664,
     573,   377,   645,   439,   489,   655,   261,   656,   422,   422,
     664,   656,   494,   664,   655,   419,   657,   204,   661,   196,
     653,   645,   205,   645,   672,   419,   598,   657,   672,     6,
      84,   385,   511,   214,   654,   515,   183,   655,   670,   253,
     183,   381,   664,   655,   664,   515,   341,   384,   426,   508,
     550,   664,   417,   519,   415,   415,   293,   293,   298,   664,
     301,   655,   657,   304,   306,   653,   315,   645,   290,   657,
     674,   376,   663,   330,   375,   652,   417,   652,   652,   413,
     227,   227,   671,   670,   655,   655,   378,   227,   651,   422,
     236,   661,   102,   547,   655,   394,   660,   529,   657,   655,
     652,   657,   653,   655,   655,   227,   651,   645,   227,   366,
     672,   385,   195,   197,   198,   653,   204,   206,   661,   515,
     645,   330,   515,   386,   512,   664,   183,   204,   224,   672,
     671,   664,   664,   674,   664,   330,   217,   662,   473,   664,
     645,   645,   645,   645,   344,   664,   300,   316,   395,   397,
     399,   401,   403,   407,   655,   657,    52,   307,   309,   310,
     312,   325,   327,   433,   477,   479,   289,   676,   655,   389,
     663,   664,   372,   663,   371,   374,   655,   655,   664,   395,
     672,   109,   561,   548,   242,   660,   645,   330,   660,   227,
     663,   223,   224,   113,   129,   180,   569,   601,   603,   655,
     655,   663,   664,   381,   655,   515,   664,   204,   663,   664,
     330,   672,    85,   513,   655,   645,   645,   215,   672,   657,
     664,   515,   222,   509,   655,   676,   330,   664,   204,   661,
     218,   653,   391,   473,   323,   324,   325,   664,   305,   405,
     409,   478,   655,   657,   663,   391,   663,   289,   328,   515,
     663,   657,   393,   381,   655,   663,   373,   651,   655,   376,
     181,   182,   651,   653,   656,   664,   515,   562,   246,   660,
     645,   651,   663,   227,   413,   655,   570,   602,   656,   664,
     664,   664,   253,   656,   664,   664,   196,   672,   389,    19,
     411,   655,   664,   515,   514,   664,   515,   433,   411,   183,
     651,   515,   657,   419,   672,   385,   217,   226,   391,   655,
     664,   433,   664,   303,   664,   645,   308,   383,   507,   549,
      53,   429,   431,   437,   477,   433,   663,   391,   664,   413,
     375,   393,   657,   657,   663,   389,   349,   653,   663,   655,
     655,   655,   655,   387,   651,   330,   645,   651,   668,   415,
     655,   413,   645,   645,   653,   676,   651,   330,   419,   515,
     657,   412,   184,   660,   393,   645,   330,   473,   655,   672,
     395,   663,   587,   515,   664,   204,   664,   385,   473,   411,
     655,   306,   664,   432,   311,   328,   311,   417,   417,   329,
     473,   477,   473,   330,   330,   417,   657,   655,   350,   351,
     653,   415,   663,   653,   664,   395,   663,   667,   243,   244,
     245,   651,   378,    24,   424,   381,   663,   198,   645,   655,
     651,   660,   664,   253,   184,   515,   664,   655,   664,   218,
     672,   411,   433,   664,   417,   664,   309,   645,   415,   417,
     655,   373,   655,   657,   352,   353,   354,   553,   653,   413,
     664,   227,   664,   676,   247,   248,   249,   651,   244,   245,
     670,   671,   670,   653,   656,   655,   664,   253,   185,   186,
     330,   585,   663,   655,   330,   330,   515,   184,   473,   323,
     664,   290,   559,   655,   657,   664,   355,   356,   653,   381,
     424,   655,   656,   330,   422,   248,   249,   670,   671,   670,
     653,   671,   670,   655,   655,   473,   181,   182,   651,   330,
     676,   655,   655,   655,   183,   417,   202,   657,   664,   655,
     253,   411,   664,   487,   334,   655,   657,   357,   358,   653,
     664,   665,   653,   663,   655,   671,   670,   655,   655,   473,
     655,   656,   253,   253,   645,   663,   422,   664,   509,   501,
     417,   664,   332,   664,   337,   655,   657,   228,   230,   231,
     232,   359,   360,   361,   362,   405,   409,   433,   473,   479,
     481,   483,   485,   330,   655,   664,   676,   655,   655,   656,
     653,   655,   655,   655,   253,   655,   411,   655,   664,   664,
     290,   334,   664,   340,   363,   364,   391,   429,   664,   673,
     664,   365,   655,   657,   663,   663,   349,   653,   664,   655,
     663,   651,   381,   422,   653,   664,   655,   676,   655,   655,
     256,   330,   411,   338,   664,   663,   473,   233,   433,   437,
     439,   233,   347,   343,   344,   344,   663,   653,   381,   422,
     645,   664,   655,   664,   172,   656,   422,   184,   183,   664,
     184,   341,   348,   659,   663,   659,   659,   663,   664,   417,
     653,   657,   665,   655,   330,   655,   381,   422,   655,   656,
     653,   655,   655,   672,   655,   652,   417,   652,   652,   413,
     344,   347,   653,   330,   655,   655,   663,   664,   655,   655,
     653,   664,   655,   515,   372,   371,   374,   653,   655,   422,
     225,   655,   664,   381,   655,   655,   376,   655,   665,   422,
     655,   381,   664,   663,   663,   655,   655,   655,   655,   655,
     664,   225,   417,   415,   663,   655,   655,   225,   422,   655,
     413,   655,   422,   655,   655,   655,   655,   655,   655,   655,
     655
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)           \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#define YYLEX	yylex ()

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)
/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*-----------------------------.
| Print this symbol on YYOUT.  |
`-----------------------------*/

static void
#if defined (__STDC__) || defined (__cplusplus)
yysymprint (FILE* yyout, int yytype, YYSTYPE yyvalue)
#else
yysymprint (yyout, yytype, yyvalue)
    FILE* yyout;
    int yytype;
    YYSTYPE yyvalue;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvalue;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyout, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyout, yytoknum[yytype], yyvalue);
# endif
    }
  else
    YYFPRINTF (yyout, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyout, ")");
}
#endif /* YYDEBUG. */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
#if defined (__STDC__) || defined (__cplusplus)
yydestruct (int yytype, YYSTYPE yyvalue)
#else
yydestruct (yytype, yyvalue)
    int yytype;
    YYSTYPE yyvalue;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvalue;

  switch (yytype)
    {
      default:
        break;
    }
}



/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of parse errors so far.  */
int yynerrs;


int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with.  */

  if (yychar <= 0)		/* This means end of input.  */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more.  */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

      /* We have to keep this `#if YYDEBUG', since we use variables
	 which are defined only if `YYDEBUG' is set.  */
      YYDPRINTF ((stderr, "Next token is "));
      YYDSYMPRINT ((stderr, yychar1, yylval));
      YYDPRINTF ((stderr, "\n"));
    }

  /* If the proper action on seeing token YYCHAR1 is to reduce or to
     detect an error, take that action.  */
  yyn += yychar1;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yychar1)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];



#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn - 1, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] >= 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif
  switch (yyn)
    {
        case 2:
#line 157 "../Parse.yacc"
    { depth=0; }
    break;

  case 4:
#line 158 "../Parse.yacc"
    { depth=0; }
    break;

  case 5:
#line 158 "../Parse.yacc"
    { Flush(); }
    break;

  case 6:
#line 159 "../Parse.yacc"
    { depth=0; }
    break;

  case 7:
#line 159 "../Parse.yacc"
    { Flush(); }
    break;

  case 536:
#line 1252 "../Parse.yacc"
    { PR ("&");}
    break;

  case 538:
#line 1253 "../Parse.yacc"
    { PR (":=");}
    break;

  case 540:
#line 1254 "../Parse.yacc"
    { PR ("*");}
    break;

  case 542:
#line 1255 "../Parse.yacc"
    { PR ("|");}
    break;

  case 544:
#line 1256 "../Parse.yacc"
    { PR (":");}
    break;

  case 546:
#line 1257 "../Parse.yacc"
    { PR (",");}
    break;

  case 548:
#line 1258 "../Parse.yacc"
    { PR (".");}
    break;

  case 550:
#line 1259 "../Parse.yacc"
    { PR ("..");}
    break;

  case 552:
#line 1260 "../Parse.yacc"
    { PR ("=");}
    break;

  case 554:
#line 1261 "../Parse.yacc"
    { PR (">");}
    break;

  case 556:
#line 1262 "../Parse.yacc"
    { PR (">=");}
    break;

  case 558:
#line 1263 "../Parse.yacc"
    { PR ("<");}
    break;

  case 560:
#line 1264 "../Parse.yacc"
    { PR ("<=");}
    break;

  case 562:
#line 1265 "../Parse.yacc"
    { PR ("-");}
    break;

  case 564:
#line 1266 "../Parse.yacc"
    { PR ("\043");}
    break;

  case 566:
#line 1267 "../Parse.yacc"
    { PR ("+");}
    break;

  case 568:
#line 1268 "../Parse.yacc"
    { PR ("=>");}
    break;

  case 570:
#line 1269 "../Parse.yacc"
    { PR ("}");}
    break;

  case 572:
#line 1270 "../Parse.yacc"
    { PR ("]");}
    break;

  case 574:
#line 1271 "../Parse.yacc"
    { PR (")");}
    break;

  case 576:
#line 1272 "../Parse.yacc"
    { PR ("*>");}
    break;

  case 578:
#line 1273 "../Parse.yacc"
    { PR ("*>");}
    break;

  case 579:
#line 1274 "../Parse.yacc"
    { PR (";");}
    break;

  case 581:
#line 1275 "../Parse.yacc"
    { PR (";");}
    break;

  case 582:
#line 1276 "../Parse.yacc"
    { PR ("/");}
    break;

  case 584:
#line 1277 "../Parse.yacc"
    { PR ("<:");}
    break;

  case 586:
#line 1278 "../Parse.yacc"
    { PR ("^");}
    break;

  case 588:
#line 1279 "../Parse.yacc"
    { PR ("^'");}
    break;

  case 590:
#line 1282 "../Parse.yacc"
    { PR ("("); }
    break;

  case 592:
#line 1283 "../Parse.yacc"
    { PR ("("); }
    break;

  case 594:
#line 1284 "../Parse.yacc"
    { PR ("["); }
    break;

  case 596:
#line 1285 "../Parse.yacc"
    { PR ("{"); }
    break;

  case 598:
#line 1291 "../Parse.yacc"
    { PF ("<* EXTERNAL", fonts->fixedComment);}
    break;

  case 600:
#line 1292 "../Parse.yacc"
    { PF ("<* INLINE",   fonts->fixedComment);}
    break;

  case 602:
#line 1293 "../Parse.yacc"
    { PF ("<* ASSERT",   fonts->fixedComment);}
    break;

  case 604:
#line 1294 "../Parse.yacc"
    { PF ("<* TRACE",    fonts->fixedComment);}
    break;

  case 606:
#line 1295 "../Parse.yacc"
    { PF ("<* FATAL",    fonts->fixedComment);}
    break;

  case 608:
#line 1296 "../Parse.yacc"
    { PF ("<* UNUSED",   fonts->fixedComment);}
    break;

  case 610:
#line 1297 "../Parse.yacc"
    { PF ("<* OBSOLETE", fonts->fixedComment);}
    break;

  case 612:
#line 1298 "../Parse.yacc"
    { PF ("<* CALLBACK", fonts->fixedComment);}
    break;

  case 614:
#line 1299 "../Parse.yacc"
    { PF ("<* EXPORTED", fonts->fixedComment);}
    break;

  case 616:
#line 1301 "../Parse.yacc"
    { PF ("<* PRAGMA",   fonts->fixedComment);}
    break;

  case 618:
#line 1302 "../Parse.yacc"
    { PF ("<* NOWARN",   fonts->fixedComment);}
    break;

  case 620:
#line 1303 "../Parse.yacc"
    { PF ("<* LINE",     fonts->fixedComment);}
    break;

  case 622:
#line 1304 "../Parse.yacc"
    { PF ("<* LL",       fonts->fixedComment);}
    break;

  case 624:
#line 1305 "../Parse.yacc"
    { PF ("<* LL.sup",   fonts->fixedComment);}
    break;

  case 626:
#line 1306 "../Parse.yacc"
    { PF ("<* SPEC",     fonts->fixedComment);}
    break;

  case 628:
#line 1307 "../Parse.yacc"
    { PF ("<* LOOPINV",  fonts->fixedComment);}
    break;

  case 630:
#line 1309 "../Parse.yacc"
    { PRID (&lexbuf[yyvsp[0]]);}
    break;

  case 632:
#line 1310 "../Parse.yacc"
    { PF (&lexbuf[yyvsp[0]], fonts->procName);}
    break;

  case 634:
#line 1311 "../Parse.yacc"
    { PRID (&lexbuf[yyvsp[0]]);}
    break;

  case 636:
#line 1312 "../Parse.yacc"
    { PR (&lexbuf[yyvsp[0]]);}
    break;

  case 638:
#line 1313 "../Parse.yacc"
    { PR (&lexbuf[yyvsp[0]]);}
    break;

  case 640:
#line 1314 "../Parse.yacc"
    { PF (&lexbuf[yyvsp[0]], fonts->fixed);}
    break;

  case 642:
#line 1315 "../Parse.yacc"
    { PF (&lexbuf[yyvsp[0]], fonts->fixed);}
    break;

  case 644:
#line 1317 "../Parse.yacc"
    { PK ("AND");}
    break;

  case 646:
#line 1318 "../Parse.yacc"
    { PK ("ANY");}
    break;

  case 648:
#line 1319 "../Parse.yacc"
    { PK ("ARRAY");}
    break;

  case 650:
#line 1320 "../Parse.yacc"
    { PK ("AS");}
    break;

  case 652:
#line 1321 "../Parse.yacc"
    { PK ("BEGIN");}
    break;

  case 654:
#line 1322 "../Parse.yacc"
    { PK ("BITS");}
    break;

  case 656:
#line 1323 "../Parse.yacc"
    { PK ("BRANDED");}
    break;

  case 658:
#line 1324 "../Parse.yacc"
    { PK ("BY");}
    break;

  case 660:
#line 1325 "../Parse.yacc"
    { PK ("CASE");}
    break;

  case 662:
#line 1326 "../Parse.yacc"
    { PK ("CONST");}
    break;

  case 664:
#line 1327 "../Parse.yacc"
    { PR ("DIV");}
    break;

  case 666:
#line 1328 "../Parse.yacc"
    { PK ("DO");}
    break;

  case 668:
#line 1329 "../Parse.yacc"
    { PK ("ELSE");}
    break;

  case 670:
#line 1330 "../Parse.yacc"
    { PK ("ELSIF");}
    break;

  case 672:
#line 1331 "../Parse.yacc"
    { PK ("END");}
    break;

  case 674:
#line 1332 "../Parse.yacc"
    { PK ("EVAL");}
    break;

  case 676:
#line 1333 "../Parse.yacc"
    { PK ("EXCEPT");}
    break;

  case 678:
#line 1334 "../Parse.yacc"
    { PK ("EXCEPTION");}
    break;

  case 680:
#line 1335 "../Parse.yacc"
    { PK ("EXIT");}
    break;

  case 682:
#line 1336 "../Parse.yacc"
    { PK ("EXPORTS");}
    break;

  case 684:
#line 1337 "../Parse.yacc"
    { PK ("FINALLY");}
    break;

  case 686:
#line 1338 "../Parse.yacc"
    { PK ("FOR");}
    break;

  case 688:
#line 1339 "../Parse.yacc"
    { PK ("FROM");}
    break;

  case 690:
#line 1340 "../Parse.yacc"
    { PK ("GENERIC");}
    break;

  case 692:
#line 1341 "../Parse.yacc"
    { PK ("IF");}
    break;

  case 694:
#line 1342 "../Parse.yacc"
    { PK ("IMPORT");}
    break;

  case 696:
#line 1343 "../Parse.yacc"
    { PK ("IN");}
    break;

  case 698:
#line 1344 "../Parse.yacc"
    { PK ("INTERFACE");}
    break;

  case 700:
#line 1345 "../Parse.yacc"
    { PK ("LOCK");}
    break;

  case 702:
#line 1346 "../Parse.yacc"
    { PK ("LOOP");}
    break;

  case 704:
#line 1347 "../Parse.yacc"
    { PK ("METHODS");}
    break;

  case 706:
#line 1348 "../Parse.yacc"
    { PK ("MOD");}
    break;

  case 708:
#line 1349 "../Parse.yacc"
    { PK ("MODULE");}
    break;

  case 710:
#line 1350 "../Parse.yacc"
    { PK ("NOT");}
    break;

  case 712:
#line 1351 "../Parse.yacc"
    { PK ("OBJECT");}
    break;

  case 714:
#line 1352 "../Parse.yacc"
    { PK ("OF");}
    break;

  case 716:
#line 1353 "../Parse.yacc"
    { PK ("OR");}
    break;

  case 718:
#line 1354 "../Parse.yacc"
    { PK ("OVERRIDES");}
    break;

  case 720:
#line 1355 "../Parse.yacc"
    { PK ("PROCEDURE");}
    break;

  case 722:
#line 1356 "../Parse.yacc"
    { PK ("RAISE");}
    break;

  case 724:
#line 1358 "../Parse.yacc"
    { DoBreak(1, 2, 0.0); PK ("RAISES");}
    break;

  case 726:
#line 1359 "../Parse.yacc"
    { PK ("READONLY");}
    break;

  case 728:
#line 1360 "../Parse.yacc"
    { PK ("RECORD");}
    break;

  case 730:
#line 1361 "../Parse.yacc"
    { PK ("REF");}
    break;

  case 732:
#line 1362 "../Parse.yacc"
    { PK ("REPEAT");}
    break;

  case 734:
#line 1363 "../Parse.yacc"
    { PK ("RETURN");}
    break;

  case 736:
#line 1364 "../Parse.yacc"
    { PK ("REVEAL");}
    break;

  case 738:
#line 1365 "../Parse.yacc"
    { PK ("ROOT");}
    break;

  case 740:
#line 1366 "../Parse.yacc"
    { PK ("SET");}
    break;

  case 742:
#line 1367 "../Parse.yacc"
    { PK ("THEN");}
    break;

  case 744:
#line 1368 "../Parse.yacc"
    { PK ("TO");}
    break;

  case 746:
#line 1369 "../Parse.yacc"
    { PK ("TRY");}
    break;

  case 748:
#line 1370 "../Parse.yacc"
    { PK ("TYPE");}
    break;

  case 750:
#line 1371 "../Parse.yacc"
    { PK ("TYPECASE");}
    break;

  case 752:
#line 1372 "../Parse.yacc"
    { PK ("UNSAFE");}
    break;

  case 754:
#line 1373 "../Parse.yacc"
    { PK ("UNTIL");}
    break;

  case 756:
#line 1374 "../Parse.yacc"
    { PK ("UNTRACED");}
    break;

  case 758:
#line 1375 "../Parse.yacc"
    { PK ("VALUE");}
    break;

  case 760:
#line 1376 "../Parse.yacc"
    { PK ("VAR");}
    break;

  case 762:
#line 1377 "../Parse.yacc"
    { PK ("WHILE");}
    break;

  case 764:
#line 1378 "../Parse.yacc"
    { PK ("WITH");}
    break;

  case 766:
#line 1381 "../Parse.yacc"
    { PK ("ABSTRACT");}
    break;

  case 768:
#line 1382 "../Parse.yacc"
    { PK ("ALL");}
    break;

  case 770:
#line 1383 "../Parse.yacc"
    { PK ("AXIOM");}
    break;

  case 772:
#line 1384 "../Parse.yacc"
    { PK ("DEPEND");}
    break;

  case 774:
#line 1385 "../Parse.yacc"
    { PK ("ENSURES");}
    break;

  case 776:
#line 1386 "../Parse.yacc"
    { PK ("EXISTS");}
    break;

  case 778:
#line 1387 "../Parse.yacc"
    { PK ("FUNC");}
    break;

  case 780:
#line 1388 "../Parse.yacc"
    { PK ("IFF");}
    break;

  case 782:
#line 1389 "../Parse.yacc"
    { PK ("IMPLIES");}
    break;

  case 784:
#line 1390 "../Parse.yacc"
    { PK ("INVARIANT");}
    break;

  case 786:
#line 1391 "../Parse.yacc"
    { PK ("IS");}
    break;

  case 788:
#line 1392 "../Parse.yacc"
    { PK ("LET");}
    break;

  case 790:
#line 1393 "../Parse.yacc"
    { PK ("MAP");}
    break;

  case 792:
#line 1394 "../Parse.yacc"
    { PK ("MODIFIES");}
    break;

  case 794:
#line 1395 "../Parse.yacc"
    { PK ("PRED");}
    break;

  case 796:
#line 1396 "../Parse.yacc"
    { PK ("PROTECT");}
    break;

  case 798:
#line 1397 "../Parse.yacc"
    { PK ("REQUIRES");}
    break;

  case 800:
#line 1404 "../Parse.yacc"
    { blanklinep = 0; PrintNPS(1); }
    break;

  case 801:
#line 1405 "../Parse.yacc"
    { blanklinep = 0; PrintNPS(1); }
    break;

  case 803:
#line 1409 "../Parse.yacc"
    { blanklinep = 0; }
    break;

  case 812:
#line 1431 "../Parse.yacc"
    { blanklinep = 0; PrintNPS(0); }
    break;

  case 815:
#line 1442 "../Parse.yacc"
    { GR (); }
    break;

  case 816:
#line 1443 "../Parse.yacc"
    { BE (0.0); }
    break;

  case 817:
#line 1444 "../Parse.yacc"
    { BE (offset); }
    break;

  case 818:
#line 1445 "../Parse.yacc"
    { BE (offset*2); }
    break;

  case 819:
#line 1446 "../Parse.yacc"
    { EN (); }
    break;

  case 820:
#line 1447 "../Parse.yacc"
    { ENF (); }
    break;

  case 821:
#line 1449 "../Parse.yacc"
    { DoBreak (1, 2, 0.0); }
    break;

  case 822:
#line 1450 "../Parse.yacc"
    { DoBreak (1, 3, 0.0); }
    break;

  case 823:
#line 1451 "../Parse.yacc"
    { DoBreak (0, 3, 0.0); }
    break;

  case 824:
#line 1453 "../Parse.yacc"
    { DoBreak (1, 1, 0.0); }
    break;

  case 825:
#line 1454 "../Parse.yacc"
    { DoBreak (1, 1, -offset); }
    break;

  case 826:
#line 1455 "../Parse.yacc"
    { DoBreak (1, 1, -offset + 2.0 * bodySpaceWidth); }
    break;

  case 827:
#line 1457 "../Parse.yacc"
    { DoBreak (0, 0, 0.0); }
    break;

  case 828:
#line 1458 "../Parse.yacc"
    { DoBreak (1, 0, 0.0); }
    break;

  case 829:
#line 1459 "../Parse.yacc"
    { P2 (' '); }
    break;

  case 830:
#line 1461 "../Parse.yacc"
    { BL (); }
    break;

  case 831:
#line 1463 "../Parse.yacc"
    { DoAlign (2, 0); }
    break;

  case 832:
#line 1464 "../Parse.yacc"
    { DoAlign (3, 0); }
    break;

  case 833:
#line 1465 "../Parse.yacc"
    { DoAlign (5, 1); }
    break;

  case 834:
#line 1466 "../Parse.yacc"
    { EndAlign (); }
    break;

  case 835:
#line 1468 "../Parse.yacc"
    { ALNL(); }
    break;

  case 836:
#line 1470 "../Parse.yacc"
    { DoSPNL (); }
    break;

  case 837:
#line 1471 "../Parse.yacc"
    { DoQSP (); }
    break;

  case 838:
#line 1472 "../Parse.yacc"
    { NL (); }
    break;

  case 839:
#line 1474 "../Parse.yacc"
    { depth++; }
    break;

  case 840:
#line 1475 "../Parse.yacc"
    { depth--; }
    break;


    }

/* Line 1016 of /usr/share/bison/yacc.c.  */
#line 4781 "y.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyssp > yyss)
	    {
	      YYDPRINTF ((stderr, "Error: popping "));
	      YYDSYMPRINT ((stderr,
			    yystos[*yyssp],
			    *yyvsp));
	      YYDPRINTF ((stderr, "\n"));
	      yydestruct (yystos[*yyssp], *yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yydestruct (yychar1, yylval);
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDPRINTF ((stderr, "Error: popping "));
      YYDSYMPRINT ((stderr,
		    yystos[*yyssp], *yyvsp));
      YYDPRINTF ((stderr, "\n"));

      yydestruct (yystos[yystate], *yyvsp);
      yyvsp--;
      yystate = *--yyssp;


#if YYDEBUG
      if (yydebug)
	{
	  short *yyssp1 = yyss - 1;
	  YYFPRINTF (stderr, "Error: state stack now");
	  while (yyssp1 != yyssp)
	    YYFPRINTF (stderr, " %d", *++yyssp1);
	  YYFPRINTF (stderr, "\n");
	}
#endif
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 1477 "../Parse.yacc"


/*-------- additional C code to implement the semantic routines -----------*/

/* The moreComments variable and CheckComm() macro are a way to move some
   of the comment stuff around in the parse tree.  What we'd like to do is
   move all the comments and newlines starting at the first newline outside
   of any enclosing Formatter.End's.  We do this by having PrintNPS format
   the leading comment in a sequence, set moreComments and return.  All the
   routines that generate output use the CheckComm macro to make sure the
   leftover comments get printed first.  EN() doesn't check so that
   comments move outside of Formatter.End's.  P2() doesn't check, which
   allows comments to move past extra spacing characters.

   To move the comments past sequences like 'G stmts E' where stmts may be
   empty, the grammar has been modified so that the G and E are not
   generated for the empty case.  This is the reason for the stmts_group
   production, and for removing empty productions from type_decl_list, etc.

   Blanklinep is used to coordinate PrintNPS's generation of newlines with
   the grammar's.  If HandleComments is being called from something that
   can emit a newline and the whitespace sequence ends with a newline, then
   HandleComments will not emit that final newline, but will set blanklinep
   instead.  This allows the caller to output the newline, perhaps with
   different parameters than HandleComments would have used.

   The trickiest part is coping with Formatter.Align.  alignDepth is > 0
   whenever we are formatting an align.  Each row consists of a group with
   some nested subgroups for each column.  Comments in all but the last
   column are kept inside the subgroup by using EF instead of E to end
   them.  Comments in the last column are allowed to escape the row and are
   formatted as a special NoAlign row.  The alignRow variable tells
   PrintNPS which case we have. */

static int moreComments = 0;	/* PrintNPS has leftover work to do. */
#define CheckComm(br)	{ if (moreComments) HandleComments(0, 0, br); }
static int alignRow = 0;	/* we are at the end of an Align row. */
static int alignDepth = 0;	/* how many Formatter.Aligns are active? */

/*---- interface to the Modula-3 formatter package ---*/
typedef void (*PROC)();
typedef double (*FPROC)();
static PROC Formatter__Flush;
static PROC Formatter__SetFont;
static PROC Formatter__PutChar;
static PROC Formatter__Break;
static PROC Formatter__NewLine;
static PROC Formatter__UnitedBreak;
static PROC Formatter__Group;
static PROC Formatter__Begin;
static PROC Formatter__Align;
static PROC Formatter__NoAlign;
static PROC Formatter__Col;
static PROC Formatter__End;

PR (s)
char *s;
{
  while (*s != 0) {
    P (*s);
    s++; }
}

/* Print a keyword. */
PK (s)
    char *s;
{
    PF(s, fonts->keyword);
}

/* Print in arbitrary font. */
PF(s, f)
    char *s;
    char *f;
{
    Formatter__SetFont(formatter, f);
    PR(s);
    Formatter__SetFont(formatter, fonts->body);
}

static char *builtins[] = {
    "ABS",
    "ADDRESS",
    "ADR",
    "ADRSIZE",
    "BITSIZE",
    "BOOLEAN",
    "BYTESIZE",
    "CARDINAL",
    "CEILING",
    "CHAR",
    "DEC",
    "DISPOSE",
    "EXTENDED",
    "FALSE",
    "FIRST",
    "FLOAT",
    "FLOOR",
    "INC",
    "INTEGER",
    "ISTYPE",
    "LAST",
    "LONGREAL",
    "LOOPHOLE",
    "MAX",
    "MIN",
    "MUTEX",
    "NARROW",
    "NEW",
    "NIL",
    "NULL",
    "NUMBER",
    "ORD",
    "REAL",
    "REFANY",
    "ROUND",
    "SUBARRAY",
    "TEXT",
    "TRUE",
    "TRUNC",
    "TYPECODE",
    "VAL",
    NULL
};
    

PRID(s)
    register char *s;
{
    register int i;
    register char *b;

    for (i = 0; (b = builtins[i]) != NULL; ++i) {
	if (*b == *s && strcmp(b, s) == 0) {
	    PF(s, fonts->builtinID);
	    return;
	}
    }
    PR(s);
}

PRNONL (s)  /* strip newline */
char *s;
{
  while (*s != 0 && *s != '\n') {
    P (*s);
    s++; }
}

BE (n) double n; { CheckComm(0); Formatter__Begin (formatter, n, MAXWIDTH); }
EN ()          { Formatter__End (formatter); }
ENF ()         { CheckComm(0); Formatter__End (formatter); }
GR ()          { CheckComm(0); Formatter__Group (formatter); }
Flush ()       { CheckComm(0); Formatter__Flush (formatter); }
Reset ()       { }
P(n) int n;    { CheckComm(0); Formatter__PutChar (formatter, n); }
P2(n) int n;   { Formatter__PutChar (formatter, n); }


/* Emit a newline one level out. */
NL ()
{
    CheckComm(1);
    Formatter__NewLine (formatter, -offset, 0);
    blanklinep = 0;
}

/* Emit a newline at current level. */
BL ()
{
    CheckComm(1); 
    Formatter__NewLine (formatter, 0.0, 0);
    blanklinep = 0;
}

DoSPNL ()
{
   if (style == EM_STYLE) {
     DoBreak (1, 0, 0.0);
   } else {
     DoBreak (1, 1, -offset);
   };
}

DoQSP ()
{
    if (callspace) DoBreak (1, 0, 0);
}


DoAlign (cols, oneline)
int cols, oneline;
{
    CheckComm(0);
    ++alignDepth;
    /* Oneline is only true for formals to procedures.  alignDecls does not
       affect them. */
    Formatter__Align(formatter, cols, oneline, (oneline || alignDecls));
}

/* Tell comment code that align is going to insert a newline here. */
ALNL()
{
    /* Only do it if there is comment work left to do. */
    if (moreComments)
	alignRow = 1;
}

EndAlign()
{
    --alignDepth;
    alignRow = 0;
    Formatter__End(formatter);
}

DoBreak (blank, breakpt, offs)
    int blank, breakpt;
    double offs;
{
  CheckComm(1);
  /* Turn breaks into newlines if there is one left to do from comment
     handling. */
  if (blanklinep) {
      Formatter__NewLine(formatter, offs, 0);
      blanklinep = 0;
      return;
  }
  if (blank==1)   Formatter__PutChar (formatter, ' ');
  /* United Break */
  if (breakpt==1) Formatter__UnitedBreak (formatter, offs, 0);
  /* Optimal, OptimalBreak */
  if (breakpt==2) Formatter__Break (formatter, offs, OptimalBreak, 1);
  /* Optimal, OptimalNoBreak */
  if (breakpt==3) Formatter__Break (formatter, offs, breakType, 1);
  /* Not optimal (only used for comments). */
  if (breakpt==4) Formatter__Break (formatter, offs, NonOptimal, 1);
}

#define PRODUCE(x) {fprintf (stderr, "%d ", x); return (x); }

#include <stdio.h>
#include <string.h>
#include "hash.h"
#include "lex.yy.c"
#include "lex_help.h"

initParser (infile, outfile, emacs, caps, fontInfo,
	    offs, ccol, sty, ad, breaktype, follow, callsp,
            charWidth, flush, setFont, putChar, breakF, newLine,
            unitedBreak, group, begin, align, noAlign, col, end)
    char *infile;
    char *outfile;
    long emacs, caps;
    FontInfo *fontInfo;
    double offs, ccol;
    STYLE sty;
    long ad;
    long breaktype, follow, callsp;
    FPROC charWidth;
    PROC flush, setFont, putChar, breakF, newLine;
    PROC unitedBreak, group, begin, align, noAlign, col, end;
{
    yyin = stdin;
    if ((!emacs) && (infile != 0)) {
	yyin = fopen(infile, "r");
	if (yyin == NULL) {
	    fprintf(stderr, "m3pp: unable to open \"%s\".\n", infile);
	    exit(1);
	};
        /* make a copy of the file name for output in case of an error */
        /* Where can I free the allocated memory ? */
        infileName = (char *) malloc (strlen(infile)+1);
        strcpy (infileName, infile);
    };
    Formatter__Flush = flush;
    Formatter__SetFont = setFont;
    Formatter__PutChar = putChar;
    Formatter__Break = breakF;
    Formatter__NewLine = newLine;
    Formatter__UnitedBreak = unitedBreak;
    Formatter__Group = group;
    Formatter__Begin = begin;
    Formatter__Align = align;
    Formatter__NoAlign = noAlign;
    Formatter__Col = col;
    Formatter__End = end;
    formatter = outfile;
    calledFromEmacs = emacs;
    capSwitch = caps;
    fonts = fontInfo;
    bodySpaceWidth = charWidth(formatter, fonts->body, ' ');
    commentLeaderWidth = charWidth(formatter, fonts->comment, '(') +
      charWidth(formatter, fonts->comment, '*') +
      charWidth(formatter, fonts->comment, ' ');
    fixedCommentSpaceWidth = charWidth(formatter, fonts->fixedComment, ' ');
    commentCol = ccol;
    offset = offs;
    style = sty;
    alignDecls = ad;
    breakType = breaktype;
    callspace = callsp;
    comBreakNLs = follow;
    insertKeywords();
}

yyerror(s) char *s; {
  int temp, temp2; /* must be 'int' instead of 'char'
                      otherwise the test (temp>0)
                      will fail for characters above code 127 
                      and we need negative numbers for detecting end of file */
  Reset();
  Flush();
  if (calledFromEmacs == 0) {
        /* XEmacs requires that character counting starts with 1
            - very poor programming */
        fprintf (stderr,
            "%s:%d:%d: (byte %d) %s while pretty-printing\n",
            (infileName != NULL) ? infileName : "",
            currentRow+1, currentCol+1, lexposition, s);
        fprintf(stderr, "Error flagged in output\n");
  }
  PR ("(* SYNTAX ERROR *) ");
  if (!calledFromEmacs && (yychar == 0)) return;  /* end-of-file */
  if ((lexbuf[lexptr] == '\001') && calledFromEmacs) return;
    /* i.e., return if formatting unit from Emacs was incomplete. */
  PR (&lexbuf[lexptr]);
  /* Now print the rest of the input, but if
     the input is terminated by end-of-file (rather than the Emacs
     sentinel '001'), and if the last thing before the end-of-file
     is a newline, then don't print that final newline.  This is
     because Flush() will be called by main when yyerror returns. */
  temp2 = input();   /* input comes from the lex library. */
  if ((calledFromEmacs && (temp2 == '\001')) || (temp2 == 0)) return;
  temp = input();
  while ((temp > 0) && (!calledFromEmacs || (temp != '\001')))
    {P (temp2); temp2 = temp; temp = input();}
  if ((temp2 != '\n') || (temp > 0)) P(temp2);
}

/* Print out first comment.  Hidden down here so it can see the comment
   structure. */
PrintOnePragma()
{
    PR(comments[0].text);
}

PrintNPS(initNPS)
    int initNPS;
{
    HandleComments(1, initNPS, 0);
}

/* Determine if a comment should be refilled or not.  Returns TRUE if it is
   "fixed" (should not be refilled). */
int FixedComment(s)
    char *s;
{
    char c;

    /* True for pragmas, '(**', or '(*|' */
    if (*s == '<' || s[2] == '*' || s[2] == '|')
	return 1;
    /* True for '(*' on a blank line. */
    for (s += 2; (c = *s) != '\n' && c != 0; ++s)
	if (!IsWhite(c))
	    return 0;
    return 1;
}

static int iComment;

/* Comment and newline handling code. */
HandleComments(firstTime, initNPS, doBreak)
    int firstTime;		/* first time on this comment? */
    int initNPS;		/* is this an InitialNPS? */
    int doBreak;		/* is a Break about to happen? */
{
    int i;
    register char *s, c;
    int startCol, ws;
    int needEnd = 0;

    moreComments = 0;		/* avoid recursion in BL, etc. calls. */
    blanklinep = 0;
    if (firstTime) {
	/* Special case: a single newline is discarded.  The pretty-printer
	   will add newlines as necessary. */
	if (nComments == 0 && comments[0].NLs <= 1)
	    return;
	iComment = 0;
    }
    if (!firstTime && alignDepth != 0) {
	/* Put extra Group/End around continuation in case we're in an Align
	   group.  Also, be sure and substract a newline for the one Align
	   will insert automatically.  I hate align. */
	if (alignRow) {
	    /* If we're at the end of a formatter row, then the comment will
	       be a row of its own, so emit a NoAlign op.  NoAlign rows must
	       emit their own leading newline, but not the trailing one. */
	    alignRow = 0;
	    if (iComment < nComments || comments[nComments].NLs > 1) {
		Formatter__NoAlign(formatter);
		GR();
		needEnd = 1;
	    }
	    if (comments[nComments].NLs > 0)
		--comments[nComments].NLs;
	}
    }
    /* Either print a space or goto the comment column for a comment on the
       same line as preceding code. */
    if (firstTime && comments[0].NLs == 0 && !initNPS) {
	if (nComments == 1 && comments[1].NLs == 0)
	    P(' ');
	else
	    Formatter__Col(formatter, commentCol, 0, bodySpaceWidth);
    }
    /* If we're flushing comments in preperation to doing a break, remove one
       last newline, and set blanklinep appropriately.  I believe that this
       code and the align code above should never both execute in the same
       invocation of HandleComments. */
    if (doBreak && comments[nComments].NLs > 0) {
	--comments[nComments].NLs;
	blanklinep = 1;
    }
    for (;; ++iComment) {
	/* The first time, bail out at the first newline. */
	if (firstTime &&
	  (iComment >= nComments || comments[iComment].NLs > comBreakNLs)) {
	    moreComments = 1;
	    break;
	}
	for (i = 0; i < comments[iComment].NLs; ++i) {
	  Formatter__NewLine(formatter, 0.0, 0);
	}
	/* break in middle since last comment struct has no comment text. */
	if (iComment >= nComments)
	    break;
	/* Handle the comment */
	s = comments[iComment].text;
	if (FixedComment(s)) {
	    /* Comment that should not be reformated.  Each line should be
	       output unchanged except for indentation. */
	    startCol = comments[iComment].startCol;
	    BE(0.0);
	    Formatter__SetFont(formatter, fonts->fixedComment);
	    while (*s != 0) {
		/* Emit the text of the line (we're already at the right
		   place for the first line). */
		while (*s != 0 && *s != '\n')
		    P(*s++);
		if (*s == 0)
		    break;
		/* Skip the newline. */
		++s;
		/* Count white space at beginning of the next line. */
		ws = 0;
		while ((c = *s) == ' ' || c == '\t') {
		    if (c == ' ')
			++ws;
		    else
			ws = (ws + 8) & ~7;
		    ++s;
		}
		/* And emit the right amount to indent this line properly. */
		Formatter__NewLine(formatter,
			       fixedCommentSpaceWidth * (ws - startCol), 0);
	    }
	    Formatter__SetFont(formatter, fonts->body);
	    EN();
	}
	else {
	    /* Comment should be reformatted, so parse words and refill.
	       Lines that begin with a vertical bar ('|') are special and
	       should not be filled. */
	    int specialLine = FALSE;	/* this "word" is a special line */
	    int prevSpecial = FALSE;	/* last one was */
	    int nls;
	    int sentenceBreak = FALSE;	/* last word ended sentence. */

	    startCol = comments[iComment].startCol;
	    BE(commentLeaderWidth);
	    Formatter__SetFont(formatter, fonts->comment);
	    P(*s++);		/* '(' */
	    P(*s++);		/* '*' */
	    while (*s != 0) {
		/* Once around per word or special line. */
		nls = 0;
		prevSpecial = specialLine;
		specialLine = FALSE;
		while (IsWhite(*s)) {
		    if (*s == '\n') {
			++nls;
			/* Check for special line. */
			if (s[1] == '|') {
			    ++s;
			    specialLine = TRUE;
			    break;
			}
		    }
		    ++s;
		}
		/* Deal with special lines. */
		if (specialLine) {
		    while (nls-- > 0)
			Formatter__NewLine(formatter, -MAXWIDTH, 0);
		    Formatter__SetFont(formatter, fonts->fixedComment);
		    /* Count white space at beginning of this comment. */
		    ws = 1;	/* count the | for now */
		    P(*s++);	/* and print it */
		    while ((c = *s) == ' ' || c == '\t') {
			if (c == ' ')
			    ++ws;
			else
			    ws = (ws + 8) & ~7;
			++s;
		    }
		    /* Emit the right amount to move indent this line
		       properly. */
		    Formatter__Col(formatter,
			       fixedCommentSpaceWidth * (ws - startCol - 3),
				    1, 0.0);
		    while (*s != '\n' && *s != 0)
			P(*s++);
		    Formatter__SetFont(formatter, fonts->comment);
		}
		else {
		    /* If more than one newline we have a paragraph break.
		       Emit the newlines. */
		    if (nls > 1 || prevSpecial) {
			while (nls-- > 0)
			  Formatter__NewLine(formatter, 0.0, 0);
		    }
		    /* Word break. */
		    else {
			/* Avoid space if there was no leading white space. */
			if (s != comments[iComment].text + 2)
			    Formatter__PutChar(formatter, ' ');
			/* Don't break before the end of the comment.  This
			   should also check for first word, but doesn't yet. */
			if (strcmp(s, "*)") != 0) {
			    if (sentenceBreak)
				Formatter__PutChar(formatter, ' ');
			    Formatter__Break(formatter, 0.0, 0, 1);
			}
		    }
		    /* Emit the word. */
		    while (!IsWhite(*s) && *s != 0)
			P(*s++);
		    sentenceBreak = index(".!?", s[-1]) != 0;
		}
	    }
	    Formatter__SetFont(formatter, fonts->body);
	    EN();
	}
    }
    if (needEnd)
	EN();
}


