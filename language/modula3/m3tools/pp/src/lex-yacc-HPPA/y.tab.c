/* A Bison parser, made by GNU Bison 1.875.  */

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
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

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
#line 92 "../Parse.yacc"


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

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 440 "y.tab.c"

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
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

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
	    (To)[yyi] = (From)[yyi];		\
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
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
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
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   5846

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  137
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  540
/* YYNRULES -- Number of rules. */
#define YYNRULES  840
/* YYNRULES -- Number of states. */
#define YYNSTATES  2122

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   391

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

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
     122,   141,   156,   169,   170,   179,   180,   187,   188,   191,
     203,   215,   217,   225,   230,   241,   253,   266,   267,   270,
     284,   291,   297,   307,   313,   323,   329,   339,   345,   355,
     361,   369,   370,   374,   378,   382,   386,   390,   394,   398,
     401,   405,   426,   444,   447,   451,   462,   473,   476,   480,
     499,   521,   540,   542,   546,   552,   563,   569,   575,   576,
     580,   581,   589,   593,   594,   600,   606,   613,   616,   620,
     634,   648,   649,   653,   654,   657,   660,   663,   674,   689,
     701,   707,   719,   728,   729,   733,   734,   739,   742,   746,
     747,   753,   757,   759,   761,   765,   767,   769,   773,   775,
     777,   779,   781,   783,   785,   787,   789,   791,   793,   795,
     797,   799,   801,   803,   805,   807,   815,   817,   829,   839,
     840,   846,   853,   855,   861,   865,   873,   875,   881,   902,
     904,   910,   921,   922,   926,   927,   930,   938,   947,   953,
     957,   967,   969,   973,   981,   992,  1001,  1002,  1008,  1023,
    1032,  1046,  1058,  1059,  1065,  1072,  1084,  1094,  1107,  1109,
    1116,  1127,  1128,  1130,  1132,  1137,  1139,  1143,  1145,  1151,
    1155,  1161,  1167,  1171,  1178,  1180,  1182,  1184,  1192,  1196,
    1201,  1205,  1207,  1214,  1219,  1228,  1237,  1239,  1243,  1245,
    1253,  1259,  1264,  1270,  1272,  1276,  1284,  1285,  1289,  1290,
    1294,  1295,  1298,  1303,  1304,  1311,  1318,  1326,  1329,  1333,
    1340,  1347,  1348,  1355,  1362,  1370,  1373,  1377,  1388,  1405,
    1421,  1431,  1447,  1462,  1463,  1470,  1477,  1485,  1488,  1492,
    1505,  1517,  1521,  1527,  1534,  1542,  1548,  1555,  1556,  1559,
    1561,  1565,  1573,  1585,  1586,  1589,  1595,  1603,  1605,  1607,
    1611,  1615,  1619,  1623,  1627,  1630,  1633,  1636,  1639,  1642,
    1648,  1652,  1660,  1668,  1670,  1673,  1677,  1683,  1691,  1699,
    1701,  1703,  1705,  1707,  1709,  1711,  1713,  1715,  1717,  1719,
    1725,  1726,  1729,  1730,  1733,  1734,  1737,  1739,  1744,  1748,
    1755,  1761,  1771,  1779,  1787,  1796,  1797,  1801,  1812,  1820,
    1831,  1835,  1843,  1847,  1855,  1857,  1859,  1863,  1865,  1873,
    1881,  1885,  1887,  1893,  1895,  1897,  1901,  1903,  1909,  1913,
    1915,  1921,  1925,  1927,  1931,  1939,  1943,  1947,  1949,  1955,
    1957,  1959,  1963,  1965,  1971,  1973,  1975,  1977,  1979,  1984,
    1990,  1996,  2000,  2004,  2006,  2011,  2014,  2020,  2022,  2024,
    2027,  2033,  2038,  2040,  2045,  2053,  2055,  2057,  2059,  2061,
    2063,  2065,  2069,  2072,  2078,  2081,  2082,  2087,  2091,  2097,
    2107,  2113,  2122,  2124,  2130,  2132,  2136,  2139,  2145,  2147,
    2152,  2155,  2156,  2160,  2162,  2164,  2168,  2170,  2176,  2180,
    2182,  2188,  2192,  2194,  2198,  2200,  2206,  2208,  2210,  2212,
    2214,  2216,  2218,  2220,  2224,  2226,  2232,  2234,  2236,  2238,
    2242,  2244,  2250,  2252,  2254,  2256,  2258,  2260,  2264,  2268,
    2271,  2273,  2275,  2277,  2279,  2281,  2286,  2288,  2289,  2293,
    2296,  2298,  2307,  2316,  2321,  2331,  2336,  2340,  2342,  2348,
    2352,  2354,  2360,  2364,  2366,  2370,  2372,  2378,  2382,  2384,
    2390,  2394,  2396,  2402,  2404,  2408,  2412,  2414,  2417,  2421,
    2425,  2427,  2429,  2433,  2439,  2441,  2443,  2445,  2447,  2449,
    2451,  2455,  2458,  2460,  2469,  2478,  2483,  2493,  2498,  2508,
    2513,  2517,  2521,  2523,  2529,  2534,  2536,  2538,  2543,  2546,
    2549,  2555,  2558,  2564,  2568,  2586,  2589,  2595,  2597,  2603,
    2609,  2610,  2615,  2616,  2618,  2620,  2625,  2626,  2630,  2631,
    2635,  2636,  2640,  2641,  2645,  2646,  2650,  2651,  2655,  2656,
    2660,  2661,  2665,  2666,  2670,  2671,  2675,  2676,  2680,  2681,
    2685,  2686,  2690,  2691,  2695,  2696,  2700,  2701,  2705,  2706,
    2710,  2711,  2715,  2716,  2720,  2721,  2725,  2726,  2730,  2732,
    2733,  2737,  2739,  2740,  2744,  2745,  2749,  2750,  2754,  2755,
    2759,  2760,  2764,  2765,  2769,  2770,  2774,  2775,  2779,  2780,
    2784,  2785,  2789,  2790,  2794,  2795,  2799,  2800,  2804,  2805,
    2809,  2810,  2814,  2815,  2819,  2820,  2824,  2825,  2829,  2830,
    2834,  2835,  2839,  2840,  2844,  2845,  2849,  2850,  2854,  2855,
    2859,  2860,  2864,  2865,  2869,  2870,  2874,  2875,  2879,  2880,
    2884,  2885,  2889,  2890,  2894,  2895,  2899,  2900,  2904,  2905,
    2909,  2910,  2914,  2915,  2919,  2920,  2924,  2925,  2929,  2930,
    2934,  2935,  2939,  2940,  2944,  2945,  2949,  2950,  2954,  2955,
    2959,  2960,  2964,  2965,  2969,  2970,  2974,  2975,  2979,  2980,
    2984,  2985,  2989,  2990,  2994,  2995,  2999,  3000,  3004,  3005,
    3009,  3010,  3014,  3015,  3019,  3020,  3024,  3025,  3029,  3030,
    3034,  3035,  3039,  3040,  3044,  3045,  3049,  3050,  3054,  3055,
    3059,  3060,  3064,  3065,  3069,  3070,  3074,  3075,  3079,  3080,
    3084,  3085,  3089,  3090,  3094,  3095,  3099,  3100,  3104,  3105,
    3109,  3110,  3114,  3115,  3119,  3120,  3124,  3125,  3129,  3130,
    3134,  3135,  3139,  3140,  3144,  3145,  3149,  3150,  3154,  3155,
    3159,  3160,  3164,  3165,  3169,  3170,  3174,  3175,  3179,  3180,
    3184,  3185,  3189,  3190,  3194,  3195,  3199,  3200,  3204,  3205,
    3209,  3210,  3214,  3215,  3219,  3220,  3224,  3225,  3229,  3230,
    3234,  3235,  3239,  3240,  3244,  3245,  3249,  3250,  3254,  3255,
    3259,  3260,  3264,  3265,  3269,  3270,  3274,  3275,  3279,  3280,
    3284,  3286,  3287,  3291,  3292,  3294,  3296,  3298,  3301,  3303,
    3306,  3308,  3311,  3313,  3315,  3318,  3319,  3320,  3321,  3322,
    3323,  3324,  3325,  3326,  3327,  3328,  3329,  3330,  3331,  3332,
    3333,  3334,  3335,  3336,  3337,  3338,  3339,  3340,  3341,  3342,
    3343
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
     663,   673,   433,   652,   377,   655,   663,   417,    -1,    -1,
     658,   525,   664,   652,   378,   655,    -1,    -1,   154,   155,
      -1,   531,   664,   473,   664,   537,   664,   652,   378,   655,
     422,   674,    -1,   653,   537,   657,   669,   156,   422,   655,
     655,   670,   655,   674,    -1,   157,    -1,   156,   389,   665,
     655,   655,   671,   157,    -1,   675,   651,   651,   473,    -1,
     675,   651,   651,   473,   656,   651,   664,   493,   664,   227,
      -1,   160,   653,   162,   675,   254,   183,   674,   676,   515,
     663,   655,    -1,   160,   653,   162,   675,   254,   183,   674,
     676,   515,   664,   473,   655,    -1,    -1,   160,   161,    -1,
     653,   162,   171,   664,   395,   666,   652,   159,   663,   422,
     655,   655,   666,    -1,   653,   162,   171,   422,   655,   666,
      -1,   653,   162,   505,   655,   666,    -1,   653,   162,   505,
     660,   668,   163,   670,   655,   666,    -1,   653,   162,   591,
     655,   666,    -1,   653,   162,   591,   660,   667,   165,   670,
     655,   666,    -1,   653,   162,   579,   655,   666,    -1,   653,
     162,   579,   660,   667,   165,   670,   655,   666,    -1,   653,
     162,   603,   655,   666,    -1,   653,   162,   603,   660,   668,
     167,   670,   655,   666,    -1,   653,   162,   521,   655,   666,
      -1,   653,   162,   521,   660,   169,   655,   666,    -1,    -1,
     162,   250,   674,    -1,   162,   260,   674,    -1,   162,   263,
     674,    -1,   162,   262,   674,    -1,   162,   261,   674,    -1,
     162,   258,   674,    -1,   162,   264,   674,    -1,   164,   671,
      -1,   163,   164,   671,    -1,   675,   651,   651,   473,   656,
     651,   387,   664,   227,   664,   656,   651,   395,   664,   330,
     663,   676,   422,   655,   655,    -1,   675,   651,   651,   473,
     656,   651,   664,   656,   651,   395,   664,   330,   663,   676,
     422,   655,   655,    -1,   166,   671,    -1,   165,   166,   671,
      -1,   651,   653,   228,   664,   395,   657,   227,   422,   655,
     655,    -1,   651,   653,   228,   664,   427,   657,   227,   422,
     655,   655,    -1,   168,   671,    -1,   167,   168,   671,    -1,
     675,   651,   651,   378,   656,   651,   387,   664,   227,   663,
     253,   676,   424,   655,   651,   645,   655,   655,    -1,   675,
     651,   651,   378,   656,   651,   387,   664,   227,   664,   656,
     651,   381,   664,   330,   663,   253,   676,   422,   655,   655,
      -1,   675,   651,   651,   378,   656,   651,   664,   656,   651,
     381,   664,   330,   663,   253,   676,   422,   655,   655,    -1,
     170,    -1,   169,   666,   170,    -1,   675,   473,   663,   676,
     422,    -1,   675,   473,   433,   659,   227,   663,   417,   663,
     676,   422,    -1,   563,   664,   475,   657,   172,    -1,   435,
     175,   417,   173,   174,    -1,    -1,   387,   657,   227,    -1,
      -1,   567,   664,   439,   652,   223,   655,   413,    -1,   567,
     664,   489,    -1,    -1,   653,   669,   178,   670,   655,    -1,
     653,   669,   176,   670,   655,    -1,   653,   669,   176,   178,
     670,   655,    -1,   177,   671,    -1,   176,   177,   671,    -1,
     651,   653,   179,   656,   653,   180,   656,   653,   378,   656,
     181,   253,   655,    -1,   651,   653,   179,   656,   653,   180,
     656,   653,   378,   656,   182,   253,   655,    -1,    -1,   179,
     261,   657,    -1,    -1,   601,   664,    -1,   603,   664,    -1,
     569,   664,    -1,   653,   387,   664,   227,   424,   665,   655,
     651,   645,   655,    -1,   653,   387,   664,   227,   656,   653,
     664,   381,   664,   330,   663,   422,   665,   655,    -1,   651,
     655,   653,   664,   381,   664,   330,   663,   422,   665,   655,
      -1,   653,   387,   664,   227,   655,    -1,   653,   387,   664,
     227,   656,   653,   664,   381,   664,   330,   655,    -1,   651,
     655,   653,   664,   381,   664,   330,   655,    -1,    -1,   660,
     186,   655,    -1,    -1,   660,   651,   185,   655,    -1,   186,
     655,    -1,   187,   653,   189,    -1,    -1,   187,   653,   188,
     655,   660,    -1,   190,   663,   422,    -1,   191,    -1,   190,
      -1,   190,   663,   422,    -1,   191,    -1,   192,    -1,   652,
     158,   655,    -1,   193,    -1,   194,    -1,   199,    -1,   200,
      -1,   201,    -1,   203,    -1,   207,    -1,   208,    -1,   209,
      -1,   210,    -1,   211,    -1,   212,    -1,   213,    -1,   216,
      -1,   219,    -1,   220,    -1,   255,    -1,   330,   664,   381,
     658,   653,   330,   655,    -1,   330,    -1,   503,   664,   330,
     664,   557,   662,   196,   195,   204,   672,   515,    -1,   503,
     664,   330,   664,   557,   195,   204,   672,   515,    -1,    -1,
     195,   661,   385,   664,   196,    -1,   653,   197,   664,   411,
     184,   655,    -1,   198,    -1,   197,   663,   389,   657,   198,
      -1,   653,   330,   655,    -1,   653,   330,   664,   393,   660,
     330,   655,    -1,   523,    -1,   517,   658,   653,   330,   655,
      -1,   654,   529,   664,   473,   253,   657,   381,   664,   330,
     657,   587,   664,   330,   202,   509,   655,   256,   183,   672,
     515,    -1,   664,    -1,   657,   501,   664,   330,   664,    -1,
     535,   664,   330,   664,   585,   183,   205,   204,   672,   515,
      -1,    -1,   661,   511,   183,    -1,    -1,   205,   206,    -1,
     661,   513,   664,   330,   664,   585,   183,    -1,   543,   664,
     330,   664,   509,   183,   672,   515,    -1,   545,   256,   183,
     672,   515,    -1,   565,   658,   330,    -1,   575,   256,   183,
     661,   653,   597,   657,   330,   655,    -1,   577,    -1,   577,
     658,   330,    -1,   589,   183,   661,   527,   183,   672,   515,
      -1,   589,   183,   661,   519,   662,   215,   214,   204,   672,
     515,    -1,   589,   183,   661,   519,   214,   204,   672,   515,
      -1,    -1,   214,   661,   385,   664,   215,    -1,   653,   654,
     224,   657,   433,   473,   253,   663,   417,   664,   411,   655,
     184,   655,    -1,   653,   654,   224,   664,   411,   655,   184,
     655,    -1,   654,   593,   657,   330,   657,   557,   655,   662,
     218,   217,   204,   672,   515,    -1,   654,   593,   657,   330,
     657,   557,   655,   217,   204,   672,   515,    -1,    -1,   217,
     661,   385,   664,   218,    -1,   653,   226,   664,   411,   184,
     655,    -1,   653,   226,   664,   433,   473,   253,   417,   664,
     411,   184,   655,    -1,   605,   664,   330,   664,   509,   256,
     183,   672,   515,    -1,   607,   664,   667,   221,   655,   655,
     670,   664,   509,   183,   672,   515,    -1,   222,    -1,   221,
     389,   655,   655,   671,   222,    -1,   651,   651,   473,   253,
     664,   655,   651,   395,   664,   330,    -1,    -1,   224,    -1,
     225,    -1,   224,   389,   657,   225,    -1,   473,    -1,   473,
     391,   473,    -1,   227,    -1,   226,   663,   389,   657,   227,
      -1,   653,   228,   655,    -1,   653,   228,   664,   233,   655,
      -1,   653,   231,   664,   233,   655,    -1,   653,   229,   655,
      -1,   653,   433,   227,   663,   417,   655,    -1,   225,    -1,
     230,    -1,   232,    -1,   497,   657,   330,   657,   529,   660,
     227,    -1,   563,   658,   172,    -1,   263,   563,   658,   172,
      -1,   599,   664,   233,    -1,   233,    -1,   599,   664,   237,
     573,   657,   227,    -1,   237,   573,   657,   227,    -1,   652,
     439,   652,   377,   655,   663,   413,   655,    -1,   437,   330,
     664,   393,   660,   330,   663,   415,    -1,   231,    -1,   599,
     664,   581,    -1,   581,    -1,   491,   657,   226,   664,   557,
     660,   227,    -1,   491,   664,   557,   660,   227,    -1,   571,
     238,   672,   515,    -1,   583,   664,   557,   660,   227,    -1,
     234,    -1,   233,   660,   234,    -1,   237,   555,   238,   235,
     236,   672,   515,    -1,    -1,   661,   547,   242,    -1,    -1,
     661,   561,   246,    -1,    -1,   499,   664,    -1,   499,   664,
     366,   664,    -1,    -1,   660,   651,   668,   241,   670,   655,
      -1,   660,   651,   668,   239,   670,   655,    -1,   660,   651,
     668,   239,   241,   670,   655,    -1,   240,   671,    -1,   239,
     240,   671,    -1,   651,   653,   378,   655,   181,   655,    -1,
     651,   653,   378,   655,   182,   655,    -1,    -1,   660,   651,
     668,   245,   670,   655,    -1,   660,   651,   668,   243,   670,
     655,    -1,   660,   651,   668,   243,   245,   670,   655,    -1,
     244,   671,    -1,   243,   244,   671,    -1,   651,   653,   473,
     656,   653,   664,   172,   422,   655,   655,    -1,   651,   653,
     473,   656,   653,   664,   172,   656,   653,   664,   381,   664,
     225,   422,   655,   655,    -1,   651,   653,   473,   656,   653,
     664,   656,   653,   664,   381,   664,   225,   422,   655,   655,
      -1,   651,   653,   473,   656,   653,   664,   172,   655,   655,
      -1,   651,   653,   473,   656,   653,   664,   172,   656,   653,
     664,   381,   664,   225,   655,   655,    -1,   651,   653,   473,
     656,   653,   664,   656,   653,   664,   381,   664,   225,   655,
     655,    -1,    -1,   660,   651,   667,   249,   670,   655,    -1,
     660,   651,   667,   247,   670,   655,    -1,   660,   651,   667,
     247,   249,   670,   655,    -1,   248,   671,    -1,   247,   248,
     671,    -1,   651,   653,   473,   656,   653,   664,   381,   664,
     225,   422,   655,   655,    -1,   651,   653,   473,   656,   653,
     664,   381,   664,   225,   655,   655,    -1,   441,   664,   419,
      -1,   441,   664,   366,   664,   419,    -1,   441,   664,   387,
     473,   664,   419,    -1,   441,   664,   366,   387,   473,   664,
     419,    -1,   447,   664,   330,   664,   419,    -1,   653,   447,
     183,   655,   664,   419,    -1,    -1,   664,   251,    -1,   495,
      -1,   495,   660,   252,    -1,   445,   664,   653,   330,   655,
     664,   419,    -1,   445,   664,   653,   330,   663,   389,   657,
     366,   655,   664,   419,    -1,    -1,   660,   257,    -1,   471,
     664,   290,   664,   419,    -1,   449,   664,   652,   259,   664,
     419,   655,    -1,   224,    -1,   489,    -1,   443,   664,   419,
      -1,   451,   664,   419,    -1,   453,   664,   419,    -1,   455,
     664,   419,    -1,   457,   664,   419,    -1,   661,   266,    -1,
     657,   267,    -1,   657,   270,    -1,   661,   268,    -1,   661,
     271,    -1,   459,   664,   378,   664,   421,    -1,   461,   664,
     421,    -1,   465,   664,   337,   664,   269,   664,   421,    -1,
     467,   664,   337,   664,   269,   664,   421,    -1,   330,    -1,
     439,   413,    -1,   439,   371,   413,    -1,   463,   664,   479,
     664,   421,    -1,   463,   664,   479,   664,   485,   664,   421,
      -1,   469,   664,   653,   272,   655,   657,   421,    -1,   273,
      -1,   278,    -1,   280,    -1,   279,    -1,   283,    -1,   284,
      -1,   285,    -1,   286,    -1,   287,    -1,   288,    -1,   277,
     674,   274,   275,   276,    -1,    -1,   317,   674,    -1,    -1,
     321,   674,    -1,    -1,   322,   674,    -1,   325,    -1,   325,
     433,   378,   417,    -1,   325,   387,   315,    -1,   325,   433,
     378,   417,   387,   315,    -1,   603,   664,   653,   313,   655,
      -1,   615,   664,   325,   282,   387,   657,   653,   311,   655,
      -1,   609,   664,   281,   657,   623,   664,   290,    -1,   609,
     664,   281,   657,   395,   664,   330,    -1,   325,   282,   387,
     657,   325,   437,   325,   415,    -1,    -1,   437,   314,   415,
      -1,   637,   664,   473,   433,   313,   417,   657,   629,   664,
     290,    -1,   621,   664,   473,   663,   387,   657,   315,    -1,
     621,   664,   473,   433,   313,   417,   663,   387,   657,   315,
      -1,   613,   664,   290,    -1,   639,   664,   326,   657,   501,
     664,   311,    -1,   627,   664,   290,    -1,   631,   664,   473,
     657,   381,   664,   289,    -1,   290,    -1,   291,    -1,   653,
     292,   655,    -1,   293,    -1,   611,   664,   437,   313,   415,
     657,   293,    -1,   619,   664,   437,   313,   415,   657,   293,
      -1,   653,   294,   655,    -1,   296,    -1,   296,   664,   295,
     657,   293,    -1,   625,    -1,   623,    -1,   653,   297,   655,
      -1,   298,    -1,   297,   657,   559,   664,   298,    -1,   653,
     299,   655,    -1,   300,    -1,   299,   657,   487,   664,   300,
      -1,   653,   301,   655,    -1,   302,    -1,   553,   664,   301,
      -1,   653,   303,   657,   316,   664,   303,   655,    -1,   653,
     303,   655,    -1,   653,   304,   655,    -1,   306,    -1,   304,
     657,   305,   664,   306,    -1,   409,    -1,   405,    -1,   653,
     307,   655,    -1,   309,    -1,   307,   657,   308,   664,   309,
      -1,   383,    -1,   507,    -1,   549,    -1,   310,    -1,   325,
     663,   433,   417,    -1,   325,   663,   433,   311,   417,    -1,
     309,   663,   437,   311,   415,    -1,   309,   663,   429,    -1,
     309,   663,   431,    -1,   312,    -1,   433,   289,   663,   417,
      -1,   663,   289,    -1,   311,   663,   389,   657,   289,    -1,
     479,    -1,   327,    -1,   663,   314,    -1,   313,   663,   389,
     657,   314,    -1,   378,   387,   664,   315,    -1,   325,    -1,
     325,   437,   315,   415,    -1,   633,   664,   315,   664,   587,
     664,   315,    -1,   401,    -1,   397,    -1,   403,    -1,   399,
      -1,   395,    -1,   407,    -1,   635,   664,   318,    -1,   663,
     319,    -1,   318,   663,   389,   657,   319,    -1,   325,   320,
      -1,    -1,   320,   437,   289,   415,    -1,   641,   664,   290,
      -1,   617,   664,   653,   290,   655,    -1,   617,   664,   653,
     290,   664,   519,   664,   324,   655,    -1,   325,   664,   411,
     664,   290,    -1,   325,   433,   473,   417,   664,   411,   664,
     290,    -1,   323,    -1,   324,   664,   385,   664,   323,    -1,
     473,    -1,   325,   391,   473,    -1,   663,   325,    -1,   326,
     663,   389,   657,   325,    -1,   325,    -1,   325,   391,   477,
     328,    -1,   477,   328,    -1,    -1,   328,   391,   329,    -1,
     473,    -1,   477,    -1,   653,   331,   655,    -1,   332,    -1,
     331,   657,   559,   664,   332,    -1,   653,   333,   655,    -1,
     334,    -1,   333,   657,   487,   664,   334,    -1,   553,   664,
     334,    -1,   335,    -1,   653,   336,   655,    -1,   338,    -1,
     336,   657,   337,   664,   338,    -1,   395,    -1,   407,    -1,
     401,    -1,   403,    -1,   397,    -1,   399,    -1,   539,    -1,
     653,   339,   655,    -1,   341,    -1,   339,   657,   340,   664,
     341,    -1,   409,    -1,   405,    -1,   379,    -1,   653,   342,
     655,    -1,   344,    -1,   342,   657,   343,   664,   344,    -1,
     383,    -1,   425,    -1,   507,    -1,   549,    -1,   345,    -1,
     409,   663,   344,    -1,   405,   663,   344,    -1,   346,   347,
      -1,   473,    -1,   479,    -1,   481,    -1,   483,    -1,   485,
      -1,   433,   330,   663,   417,    -1,   232,    -1,    -1,   347,
     663,   348,    -1,   391,   473,    -1,   429,    -1,   673,   437,
     659,   652,   371,   655,   663,   415,    -1,   673,   433,   659,
     652,   372,   655,   663,   417,    -1,   673,   433,   663,   417,
      -1,   673,   439,   659,   652,   374,   376,   655,   663,   413,
      -1,   673,   439,   663,   413,    -1,   653,   350,   655,    -1,
     351,    -1,   350,   657,   559,   664,   332,    -1,   653,   352,
     655,    -1,   353,    -1,   352,   657,   487,   664,   334,    -1,
     553,   664,   334,    -1,   354,    -1,   653,   355,   655,    -1,
     356,    -1,   355,   657,   337,   664,   338,    -1,   653,   357,
     655,    -1,   358,    -1,   357,   657,   340,   664,   341,    -1,
     653,   359,   655,    -1,   360,    -1,   359,   657,   343,   664,
     344,    -1,   361,    -1,   409,   663,   344,    -1,   405,   663,
     344,    -1,   228,    -1,   228,   363,    -1,   228,   664,   233,
      -1,   231,   664,   233,    -1,   230,    -1,   232,    -1,   232,
     365,   347,    -1,   433,   349,   663,   417,   347,    -1,   362,
      -1,   479,    -1,   481,    -1,   483,    -1,   485,    -1,   364,
      -1,   363,   663,   348,    -1,   391,   473,    -1,   429,    -1,
     673,   437,   659,   652,   371,   655,   663,   415,    -1,   673,
     433,   659,   652,   372,   655,   663,   417,    -1,   673,   433,
     663,   417,    -1,   673,   439,   659,   652,   374,   376,   655,
     663,   413,    -1,   673,   439,   663,   413,    -1,   673,   439,
     659,   652,   374,   376,   655,   663,   413,    -1,   673,   439,
     663,   413,    -1,   653,   367,   655,    -1,   653,   368,   655,
      -1,   369,    -1,   368,   657,   379,   664,   369,    -1,   653,
     370,   347,   655,    -1,   473,    -1,   485,    -1,   433,   366,
     663,   417,    -1,   232,   365,    -1,   663,   330,    -1,   371,
     663,   389,   657,   330,    -1,   663,   373,    -1,   372,   663,
     389,   657,   373,    -1,   651,   349,   655,    -1,   651,   653,
     653,   653,   653,   653,   473,   664,   381,   657,   330,   655,
     655,   655,   655,   655,   655,    -1,   663,   375,    -1,   374,
     663,   389,   657,   375,    -1,   330,    -1,   330,   664,   393,
     657,   330,    -1,   330,   664,   381,   657,   330,    -1,    -1,
     663,   389,   664,   393,    -1,    -1,   378,    -1,   473,    -1,
     378,   389,   657,   473,    -1,    -1,     3,   380,   645,    -1,
      -1,     4,   382,   645,    -1,    -1,     5,   384,   645,    -1,
      -1,     6,   386,   645,    -1,    -1,     7,   388,   645,    -1,
      -1,     8,   390,   645,    -1,    -1,     9,   392,   645,    -1,
      -1,    10,   394,   645,    -1,    -1,    11,   396,   645,    -1,
      -1,    12,   398,   645,    -1,    -1,    13,   400,   645,    -1,
      -1,    14,   402,   645,    -1,    -1,    15,   404,   645,    -1,
      -1,    16,   406,   645,    -1,    -1,    17,   408,   645,    -1,
      -1,    18,   410,   645,    -1,    -1,    19,   412,   645,    -1,
      -1,    21,   414,   645,    -1,    -1,    22,   416,   645,    -1,
      -1,    23,   418,   645,    -1,    -1,    20,   420,   645,    -1,
      20,    -1,    -1,    24,   423,   645,    -1,    24,    -1,    -1,
      25,   426,   645,    -1,    -1,    26,   428,   645,    -1,    -1,
      27,   430,   645,    -1,    -1,    53,   432,   645,    -1,    -1,
      28,   434,   645,    -1,    -1,    28,   436,   645,    -1,    -1,
      29,   438,   645,    -1,    -1,    30,   440,   645,    -1,    -1,
      36,   442,   645,    -1,    -1,    37,   444,   645,    -1,    -1,
      42,   446,   645,    -1,    -1,    43,   448,   645,    -1,    -1,
      40,   450,   645,    -1,    -1,    39,   452,   645,    -1,    -1,
      38,   454,   645,    -1,    -1,    46,   456,   645,    -1,    -1,
      49,   458,   645,    -1,    -1,    45,   460,   645,    -1,    -1,
      41,   462,   645,    -1,    -1,    44,   464,   645,    -1,    -1,
      47,   466,   645,    -1,    -1,    48,   468,   645,    -1,    -1,
      50,   470,   645,    -1,    -1,    51,   472,   645,    -1,    -1,
      31,   474,   645,    -1,    -1,    31,   476,   645,    -1,    -1,
      52,   478,   645,    -1,    -1,    32,   480,   645,    -1,    -1,
      33,   482,   645,    -1,    -1,    34,   484,   645,    -1,    -1,
      35,   486,   645,    -1,    -1,    72,   488,   645,    -1,    -1,
      73,   490,   645,    -1,    -1,    74,   492,   645,    -1,    -1,
      75,   494,   645,    -1,    -1,    76,   496,   645,    -1,    -1,
      77,   498,   645,    -1,    -1,    78,   500,   645,    -1,    -1,
      79,   502,   645,    -1,    -1,    80,   504,   645,    -1,    -1,
      81,   506,   645,    -1,    -1,    82,   508,   645,    -1,    -1,
      83,   510,   645,    -1,    -1,    84,   512,   645,    -1,    -1,
      85,   514,   645,    -1,    -1,    86,   516,   645,    -1,    -1,
      87,   518,   645,    -1,    -1,    88,   520,   645,    -1,    -1,
      89,   522,   645,    -1,    -1,    90,   524,   645,    -1,    -1,
      91,   526,   645,    -1,    -1,    92,   528,   645,    -1,    -1,
      93,   530,   645,    -1,    -1,    94,   532,   645,    -1,    -1,
      95,   534,   645,    -1,    -1,    96,   536,   645,    -1,    -1,
      97,   538,   645,    -1,    -1,    98,   540,   645,    -1,    -1,
      99,   542,   645,    -1,    -1,   100,   544,   645,    -1,    -1,
     101,   546,   645,    -1,    -1,   102,   548,   645,    -1,    -1,
     103,   550,   645,    -1,    -1,   104,   552,   645,    -1,    -1,
     105,   554,   645,    -1,    -1,   106,   556,   645,    -1,    -1,
     107,   558,   645,    -1,    -1,   108,   560,   645,    -1,    -1,
     109,   562,   645,    -1,    -1,   110,   564,   645,    -1,    -1,
     111,   566,   645,    -1,    -1,   112,   568,   645,    -1,    -1,
     113,   570,   645,    -1,    -1,   114,   572,   645,    -1,    -1,
     115,   574,   645,    -1,    -1,   116,   576,   645,    -1,    -1,
     117,   578,   645,    -1,    -1,   118,   580,   645,    -1,    -1,
     119,   582,   645,    -1,    -1,   120,   584,   645,    -1,    -1,
     121,   586,   645,    -1,    -1,   122,   588,   645,    -1,    -1,
     123,   590,   645,    -1,    -1,   124,   592,   645,    -1,    -1,
     125,   594,   645,    -1,    -1,   126,   596,   645,    -1,    -1,
     127,   598,   645,    -1,    -1,   128,   600,   645,    -1,    -1,
     129,   602,   645,    -1,    -1,   130,   604,   645,    -1,    -1,
     131,   606,   645,    -1,    -1,   132,   608,   645,    -1,    -1,
      70,   610,   645,    -1,    -1,    54,   612,   645,    -1,    -1,
      55,   614,   645,    -1,    -1,    56,   616,   645,    -1,    -1,
      57,   618,   645,    -1,    -1,    58,   620,   645,    -1,    -1,
      59,   622,   645,    -1,    -1,    60,   624,   645,    -1,    -1,
      61,   626,   645,    -1,    -1,    62,   628,   645,    -1,    -1,
      63,   630,   645,    -1,    -1,    64,   632,   645,    -1,    -1,
      65,   634,   645,    -1,    -1,    66,   636,   645,    -1,    -1,
      68,   638,   645,    -1,    -1,    69,   640,   645,    -1,    -1,
      71,   642,   645,    -1,   650,    -1,    -1,   650,   644,   647,
      -1,    -1,   646,    -1,   647,    -1,   649,    -1,   649,   647,
      -1,   648,    -1,   648,   646,    -1,   265,    -1,   648,   265,
      -1,   650,    -1,   134,    -1,   650,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   230,   230,   230,   231,   231,   232,   232,   235,   237,
     241,   242,   246,   247,   248,   249,   253,   254,   258,   259,
     260,   264,   265,   270,   271,   272,   273,   277,   281,   286,
     289,   294,   301,   307,   309,   312,   314,   317,   319,   323,
     324,   328,   329,   333,   334,   338,   342,   345,   347,   353,
     354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
     364,   367,   369,   370,   371,   372,   373,   374,   375,   379,
     380,   385,   386,   390,   391,   395,   396,   400,   401,   405,
     408,   411,   418,   419,   424,   425,   429,   433,   436,   438,
     441,   443,   444,   447,   449,   450,   451,   455,   456,   460,
     464,   467,   469,   472,   474,   475,   476,   480,   481,   482,
     486,   487,   488,   493,   495,   499,   501,   506,   510,   513,
     515,   519,   520,   524,   525,   526,   530,   531,   532,   533,
     534,   535,   536,   537,   538,   539,   540,   541,   542,   543,
     544,   545,   546,   547,   551,   556,   560,   564,   565,   568,
     570,   574,   578,   579,   583,   584,   588,   593,   599,   604,
     605,   609,   612,   614,   617,   619,   623,   627,   631,   635,
     639,   643,   644,   648,   652,   653,   656,   658,   662,   663,
     667,   668,   671,   673,   677,   678,   682,   686,   690,   691,
     695,   698,   700,   704,   705,   709,   710,   716,   717,   721,
     722,   723,   724,   725,   729,   733,   734,   738,   739,   740,
     741,   742,   743,   744,   745,   746,   747,   751,   752,   757,
     758,   759,   760,   764,   765,   769,   772,   774,   777,   779,
     782,   784,   785,   788,   790,   791,   792,   796,   797,   801,
     804,   807,   809,   810,   811,   815,   816,   820,   821,   822,
     826,   827,   828,   831,   833,   834,   835,   839,   840,   844,
     848,   854,   855,   856,   857,   860,   861,   863,   865,   869,
     870,   874,   875,   878,   880,   883,   887,   890,   891,   894,
     895,   896,   897,   898,   904,   905,   906,   907,   908,   915,
     916,   918,   919,   923,   924,   925,   929,   930,   933,   939,
     940,   941,   942,   943,   944,   945,   946,   947,   948,   952,
     958,   960,   963,   965,   968,   970,   974,   975,   976,   977,
     980,   983,   986,   987,   990,   992,   994,   997,  1000,  1001,
    1004,  1006,  1008,  1010,  1015,  1017,  1019,  1021,  1022,  1023,
    1026,  1028,  1029,  1032,  1032,  1034,  1036,  1037,  1040,  1042,
    1043,  1046,  1048,  1049,  1053,  1054,  1057,  1059,  1060,  1062,
    1062,  1064,  1066,  1067,  1069,  1069,  1069,  1072,  1073,  1074,
    1075,  1076,  1077,  1081,  1082,  1086,  1087,  1091,  1092,  1098,
    1099,  1102,  1105,  1106,  1107,  1110,  1110,  1110,  1110,  1110,
    1110,  1112,  1115,  1116,  1119,  1121,  1123,  1125,  1128,  1129,
    1133,  1134,  1138,  1139,  1143,  1144,  1148,  1149,  1153,  1154,
    1155,  1158,  1160,  1173,  1173,  1178,  1179,  1179,  1181,  1182,
    1182,  1184,  1184,  1186,  1187,  1187,  1188,  1188,  1188,  1188,
    1188,  1188,  1188,  1190,  1191,  1191,  1192,  1192,  1192,  1194,
    1195,  1195,  1196,  1196,  1196,  1196,  1198,  1198,  1198,  1201,
    1203,  1203,  1203,  1203,  1203,  1204,  1204,  1206,  1208,  1212,
    1213,  1216,  1217,  1218,  1219,  1220,  1226,  1227,  1227,  1229,
    1230,  1230,  1232,  1232,  1234,  1235,  1235,  1237,  1238,  1238,
    1240,  1241,  1241,  1243,  1243,  1243,  1245,  1246,  1247,  1248,
    1249,  1250,  1251,  1252,  1253,  1256,  1256,  1256,  1256,  1259,
    1260,  1264,  1265,  1268,  1269,  1270,  1271,  1272,  1276,  1277,
    1284,  1286,  1287,  1287,  1289,  1291,  1292,  1293,  1294,  1300,
    1301,  1305,  1306,  1310,  1311,  1315,  1316,  1320,  1321,  1322,
    1325,  1327,  1330,  1332,  1336,  1337,  1343,  1343,  1344,  1344,
    1345,  1345,  1346,  1346,  1347,  1347,  1348,  1348,  1349,  1349,
    1350,  1350,  1351,  1351,  1352,  1352,  1353,  1353,  1354,  1354,
    1355,  1355,  1356,  1356,  1357,  1357,  1358,  1358,  1359,  1359,
    1360,  1360,  1361,  1361,  1362,  1362,  1363,  1363,  1364,  1365,
    1365,  1366,  1367,  1367,  1368,  1368,  1369,  1369,  1370,  1370,
    1373,  1373,  1374,  1374,  1375,  1375,  1376,  1376,  1382,  1382,
    1383,  1383,  1384,  1384,  1385,  1385,  1386,  1386,  1387,  1387,
    1388,  1388,  1389,  1389,  1390,  1390,  1392,  1392,  1393,  1393,
    1394,  1394,  1395,  1395,  1396,  1396,  1397,  1397,  1398,  1398,
    1400,  1400,  1401,  1401,  1402,  1402,  1403,  1403,  1404,  1404,
    1405,  1405,  1406,  1406,  1408,  1408,  1409,  1409,  1410,  1410,
    1411,  1411,  1412,  1412,  1413,  1413,  1414,  1414,  1415,  1415,
    1416,  1416,  1417,  1417,  1418,  1418,  1419,  1419,  1420,  1420,
    1421,  1421,  1422,  1422,  1423,  1423,  1424,  1424,  1425,  1425,
    1426,  1426,  1427,  1427,  1428,  1428,  1429,  1429,  1430,  1430,
    1431,  1431,  1432,  1432,  1433,  1433,  1434,  1434,  1435,  1435,
    1436,  1436,  1437,  1437,  1438,  1438,  1439,  1439,  1440,  1440,
    1441,  1441,  1442,  1442,  1443,  1443,  1444,  1444,  1445,  1445,
    1446,  1446,  1447,  1447,  1450,  1450,  1451,  1451,  1452,  1452,
    1453,  1453,  1454,  1454,  1455,  1455,  1456,  1456,  1457,  1457,
    1458,  1458,  1459,  1459,  1460,  1460,  1461,  1461,  1462,  1462,
    1463,  1463,  1464,  1464,  1465,  1465,  1466,  1466,  1467,  1467,
    1468,  1468,  1469,  1469,  1470,  1470,  1473,  1473,  1474,  1474,
    1475,  1475,  1476,  1476,  1477,  1477,  1478,  1478,  1479,  1479,
    1480,  1480,  1481,  1481,  1482,  1482,  1483,  1483,  1484,  1484,
    1485,  1485,  1486,  1486,  1487,  1487,  1488,  1488,  1489,  1489,
    1507,  1508,  1508,  1512,  1513,  1514,  1521,  1522,  1526,  1527,
    1531,  1532,  1536,  1541,  1542,  1547,  1548,  1549,  1550,  1551,
    1552,  1554,  1555,  1556,  1558,  1559,  1560,  1562,  1563,  1564,
    1566,  1568,  1569,  1570,  1571,  1573,  1575,  1576,  1577,  1579,
    1580
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
      18,    14,    12,     0,     8,     0,     6,     0,     2,    11,
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
       3,     7,    11,     0,     2,     5,     7,     1,     1,     3,
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
      65,    64,    68,   817,     0,   816,     0,     0,     0,     0,
     830,   832,   830,   839,     0,   830,   831,   830,   831,   830,
     832,   839,   803,   825,   119,   817,     0,   837,     0,   633,
     803,   838,   822,   819,     0,   682,   828,   803,     0,   546,
     821,     0,   803,   803,   803,   803,   803,   803,   803,   817,
     817,   770,   772,   778,   784,   788,   794,   796,   766,   819,
     299,   838,   300,   302,   301,   303,   304,   305,   306,   307,
     308,   316,   404,   828,   828,   828,   828,   828,   828,   828,
     828,   828,   631,   828,   599,   601,   611,   609,   607,   613,
     615,   663,   679,   721,   737,   749,   761,   830,   830,   544,
     576,   828,     0,   261,   817,   279,     0,   280,   281,   282,
     283,    51,   839,    59,   819,    82,     0,   821,    55,   815,
      53,   815,    57,   839,     0,    41,   815,   653,   840,   819,
     817,   270,     0,   838,     0,   838,   580,    37,   817,   838,
     822,   825,   816,   637,   642,   296,   828,   803,     0,   289,
     553,   555,   557,   559,   561,   565,   697,   596,   828,   293,
     827,   817,   828,   803,   825,   825,   803,   825,   825,   825,
     825,   821,   310,   548,   590,     0,     0,     0,   817,     0,
     817,     0,     0,   817,     0,     0,   827,   816,    50,   816,
     803,   803,     0,     0,   828,   819,   817,   646,   277,   193,
     828,   195,   278,   834,   835,   815,   830,   839,   827,     0,
     834,   835,   817,   834,   834,   835,   815,   829,   819,   815,
       0,   114,   816,   604,   824,    37,   816,    37,    47,     0,
      37,   817,   683,     0,   803,     0,   547,   535,   803,     0,
     570,   827,   294,   817,   819,   416,   817,     0,   771,   773,
     779,   785,   789,   795,   797,   767,     0,   792,   312,   838,
     828,   803,   803,   790,   318,   382,   828,   405,     0,   827,
     821,   325,   330,   335,   817,   325,   827,   332,   821,     0,
     821,     0,     0,    47,   545,   577,   828,   262,     0,   510,
     819,   512,     0,   803,   821,     0,     0,   835,   819,    69,
     815,    60,    83,   823,   840,   592,    86,   817,   835,   819,
      73,     0,   819,   835,   819,    77,   815,   819,   819,     0,
     672,   828,   602,   660,   674,   680,   692,   700,   702,   722,
     732,   734,   746,   762,   764,   819,   118,   123,   122,   126,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   146,   828,   828,
     822,   156,   828,   828,   273,   822,   273,   822,   824,   828,
     828,    47,     0,   803,   819,    47,   532,    47,    38,   817,
       0,    33,    47,     0,   819,   643,   297,   597,   291,   803,
     295,     0,   519,   415,     0,   710,   819,   419,   422,   828,
     817,   292,   298,   825,   798,   314,   838,   828,   311,   827,
     549,   591,   825,   594,     0,     0,   574,   317,   819,     0,
       0,     0,     0,   768,   776,   819,   337,   828,   828,   817,
       0,   827,     0,     0,   827,     0,     0,   406,   819,   827,
     817,     0,   263,   511,     0,   648,   728,   740,   837,   457,
     817,   515,   516,   821,   233,   828,   647,     0,   819,   196,
      70,   830,     0,   817,     0,   803,     0,   833,    74,   830,
     204,   828,   830,    78,   830,     0,   819,   834,    43,   803,
       0,   803,   803,   803,   803,   803,   803,   803,   803,   803,
     803,   803,   803,   825,   824,     0,     0,   817,   817,   817,
     817,   817,   824,     0,   817,   824,   817,   825,   817,   831,
     819,   817,   686,   750,   828,   821,   605,   828,   817,   819,
     533,   827,    48,   828,    61,   819,   827,    33,    36,   571,
     821,   716,   828,   803,   418,     0,   817,   819,   424,   817,
     793,   803,   774,   309,   838,   828,   313,   817,   391,     0,
     791,   803,     0,   828,   803,     0,   320,     0,   379,     0,
     780,   828,   828,   821,     0,   825,   825,   336,     0,     0,
     819,   341,   817,   821,   827,   821,   538,   828,   827,   658,
     828,   821,     0,     0,    61,   264,   536,   828,   803,   825,
     825,   518,     0,   819,   827,   817,     0,   836,   815,     0,
     194,   276,    52,   820,   827,   230,    84,   593,    88,   815,
      56,     0,    54,    58,   820,   835,   819,   815,   673,   819,
     603,   661,   675,   681,   693,   701,   703,   723,   733,   735,
     747,   763,   765,   120,   121,   822,   817,   828,   817,   828,
     828,   836,   628,   274,   828,   169,   825,   172,     0,   828,
     815,   127,    61,   803,   803,     0,   817,     0,   828,   827,
       0,     0,   819,     0,   819,   817,   803,   817,   711,   644,
     828,   421,   423,     0,   819,   434,     0,   799,   803,   315,
     817,   397,     0,   392,   395,   595,   572,   383,     0,   575,
     319,   821,   828,   803,   817,   817,     0,   326,   769,   777,
     827,   827,   340,     0,   819,   346,   817,   817,   827,     0,
     803,   817,   821,   803,   827,     0,   838,   819,   839,   803,
     817,   649,   729,   741,   823,   514,   837,     0,   827,   197,
     714,   824,     0,   832,   824,   815,     0,   654,   656,   738,
     756,   819,   819,   205,   216,   206,   211,   223,     0,     0,
     817,   817,   821,   828,   822,   218,   828,     0,    90,   821,
     815,   835,   834,   817,   584,   821,   821,   815,   839,   838,
     828,   838,   817,   819,     0,   819,     0,     0,     0,   803,
     817,   817,   676,   684,   176,   824,     0,   819,   188,   815,
     839,   687,   751,   267,   821,   266,     0,     0,    32,     0,
     838,    29,   819,   520,   717,   417,   803,   817,   828,   433,
       0,   562,   566,   638,   640,   456,   819,   440,   446,   457,
     827,   827,   817,   450,   451,   452,   453,   454,   775,   817,
     821,   394,   803,   744,   828,     0,     0,   781,   323,   322,
       0,   827,   827,   782,   821,   344,   343,   345,     0,   819,
     349,   817,   827,     0,   328,   539,   333,   334,     0,   659,
     331,   817,   407,    39,   819,     0,   537,   513,   816,     0,
     586,   458,     0,   460,     0,   517,     0,     0,   803,   817,
     221,   815,   817,   828,   827,   803,   803,   803,   803,   199,
     230,   202,   230,   230,   712,   730,   233,   821,   822,   827,
     828,   817,   817,     0,   230,   816,   724,    87,   828,   817,
     835,   834,   819,    97,   819,   101,   803,   817,   817,   828,
      42,    40,     0,    15,   817,   828,     0,   826,   157,   742,
     824,   666,   824,   168,   629,   828,     0,   803,   803,   825,
     817,   836,   273,   819,   819,     0,     0,   821,     0,     0,
       0,    34,    27,     0,   838,   645,   420,   817,   828,   438,
     437,   436,   803,   803,   803,   803,   439,     0,   449,     0,
       0,   827,   819,     0,   817,   573,   803,     0,   380,   381,
       0,   821,   821,   803,   817,   828,   348,     0,   819,   352,
     828,   817,   819,   821,   786,   828,     0,   375,   830,   824,
     827,   509,   803,   459,   823,   823,   823,   821,   824,   715,
     220,   834,   835,   834,   817,   222,   828,   820,   840,   655,
     657,   739,   757,   819,     0,   819,   224,   803,   803,   226,
     817,     0,     0,     0,   821,   828,   208,   210,     0,   217,
     532,   825,     0,    89,    98,   819,    95,    94,   820,   585,
       0,     0,   828,   820,   650,   828,   819,     0,   821,   825,
     817,   803,   164,   803,   836,     0,   754,   821,   677,   685,
     836,     0,   176,   818,     0,   824,   819,   834,   267,   824,
       0,   268,   828,   819,    31,   828,     0,   425,   817,   563,
     567,   639,   641,   540,   582,   664,   706,   828,   442,   443,
     444,   445,   448,   447,     0,   398,     0,   393,     0,   745,
     384,     0,   817,   817,   783,   342,   817,   828,   351,   817,
     819,   817,   321,     0,   803,   817,   821,    49,   838,   530,
     817,   587,   816,     0,   816,   816,     0,   817,   817,   835,
     834,   819,   237,   819,     0,   817,   815,     0,   200,   201,
     713,   731,   228,     0,   213,   209,   819,   550,   824,     0,
     232,   821,   819,   725,   816,    92,    96,   821,   817,   819,
     819,   817,   815,   803,   817,   145,   271,   817,   836,     0,
     149,   817,   743,   825,   667,     0,   275,   803,   817,     0,
     542,   668,   828,   824,   825,     0,   173,   836,   835,   828,
     828,   838,   828,   817,   826,     0,   828,   435,   803,   803,
     803,   803,     0,   455,   828,   396,   324,   338,   339,   347,
     817,   353,   355,     0,   819,   357,     0,   329,   787,   327,
     817,   840,   819,     0,   527,   525,   827,   463,   827,   827,
     465,   198,   219,   238,   819,   235,   234,   819,   828,     0,
      85,   836,     0,   704,   241,   203,   803,   817,   824,   817,
     827,   191,   102,   103,   819,   819,   827,     0,   651,    44,
     819,     0,   828,   825,   827,   152,   817,   836,   165,     0,
     167,   755,   819,   175,   803,   803,   817,   163,   836,   821,
       0,   815,     0,   819,   840,   817,   828,   825,   817,     0,
       0,   541,   583,   665,   707,   441,     0,   350,   828,   389,
     386,   388,   385,   387,   390,   356,     0,   634,   819,   362,
     367,   373,   408,   378,   817,   411,   377,   376,     0,   827,
     821,     0,   819,   815,   819,   530,   236,   815,   820,   828,
       0,   718,   253,   825,   227,   815,   551,   827,   817,   212,
       0,   819,   192,   726,   758,   820,   828,   828,   828,    75,
      76,   267,   820,   828,   828,   148,   817,   836,     0,     0,
     819,     0,   670,   828,   170,   543,   669,   177,     0,     0,
       0,   186,   189,   824,   815,     0,   821,     0,   836,     0,
     182,   817,    28,     0,   402,   819,   828,   817,   828,   360,
     359,   803,   361,     0,     0,     0,     0,   827,   410,   828,
       0,   817,     0,   821,   821,   827,     0,   521,   817,   827,
     819,   819,   819,   819,     0,   815,   817,   225,   825,   229,
     815,   705,   832,     0,   207,   819,     0,   825,   825,   817,
     106,   104,   105,   840,   815,   817,     0,   150,     0,   821,
     568,   824,   154,     0,   161,   803,   817,   174,     0,   819,
     836,     0,   827,     0,   265,     0,   828,   825,   828,    30,
     399,     0,     0,     0,   819,   817,   635,   828,   364,   365,
     366,   588,   371,   372,   827,   411,   827,     0,     0,     0,
     508,   526,   531,   817,   817,     0,   821,   819,   817,     0,
     827,   239,   240,   817,   828,     0,   827,   719,   831,   815,
     215,   214,    91,   727,   759,     0,     0,     0,   827,   272,
     147,   817,   803,   819,   815,   824,   671,   828,   267,   824,
       0,   828,   819,   828,   181,   817,   836,     0,   828,     0,
     828,   354,   358,     0,   803,   827,   409,   827,   368,   374,
     412,   413,   414,   819,   529,   528,   462,   815,   523,   819,
     467,   817,   461,     0,   828,   817,   828,   840,   815,   834,
     835,   834,   817,   820,   581,   819,   828,   267,   153,   569,
     151,   119,   817,     0,   827,   819,   187,   817,    45,   817,
     183,     0,   824,     0,     0,   828,   817,   363,   589,   370,
     369,    46,   522,   466,     0,   819,   470,   473,   828,   817,
     464,     0,   819,   817,     0,   815,   835,   834,   817,   835,
     834,   819,   245,   819,     0,   815,   815,   817,   840,   819,
     819,   819,   824,     0,   179,   190,   821,   180,   819,   267,
     403,     0,   400,   828,   469,     0,   817,   821,   475,   817,
     828,   829,   110,   817,   827,   819,   835,   834,   819,   257,
     819,     0,   246,   819,   243,   242,   820,   267,   267,   803,
     827,     0,   116,   117,   155,   166,   828,     0,     0,   159,
     184,     0,   828,   817,   828,   472,   474,     0,   819,   478,
     230,   817,   819,   828,   840,   819,   258,   819,   255,   254,
     820,   244,   817,   819,   819,   819,   267,   819,     0,   819,
     828,   828,   817,   468,   817,   828,   477,     0,   486,   490,
     216,   491,   819,   481,   483,   494,   827,   827,   817,   195,
     495,   496,   497,   498,   819,   815,     0,     0,    72,   256,
     817,   828,    99,   100,   819,   840,   819,   819,   273,   817,
       0,   401,   471,   817,   828,   487,   499,     0,   502,   230,
       0,   230,   457,   480,     0,     0,     0,   827,   817,     0,
     112,     0,   803,   828,   819,   828,   820,    79,     0,    81,
     824,   824,   828,   824,   476,   817,   837,   501,   488,   823,
     823,   823,   489,   492,   828,   485,   484,     0,   817,   821,
     829,   819,   817,   819,     0,   819,   817,   819,   819,   836,
     160,   819,   479,   500,   816,     0,   816,   816,     0,     0,
     457,   817,   817,   819,   107,   819,    71,   828,   819,   819,
     817,   828,   819,   178,     0,   185,   827,   505,   827,   827,
     507,   482,   493,   817,   819,   109,   111,     0,     0,   819,
     250,   828,     0,    80,   158,   819,   819,   530,   230,   819,
     829,   819,   247,     0,   828,   827,   827,   819,   819,   819,
     819,   819,   828,     0,     0,     0,   827,   819,   108,   819,
     260,     0,   819,   504,   503,     0,   819,   259,   819,   819,
     819,   506,   819,   819,   819,   819,   252,   524,   819,   251,
     249,   248
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     1,     2,     3,     4,     7,    45,    46,    52,    53,
      47,    17,    18,    19,    20,   246,   167,   448,   598,   344,
     345,   730,   659,   660,   742,    96,   423,   424,   430,   431,
     434,   435,   334,   335,   136,   526,   978,  1127,   686,   980,
     981,   982,  1268,  1575,  1641,  1642,   243,  1743,  1849,   349,
     350,   555,   556,   557,   558,   559,   560,   561,  1279,  1400,
    1494,  1495,   562,   563,   564,  1897,   565,  1290,  1403,  1498,
     566,   567,   568,   569,   570,   571,   572,  1159,  1292,   573,
    1517,  1610,   574,   575,  1007,  1008,  1571,   418,   690,   948,
     949,  1938,   962,  1939,  1940,  1035,   966,   967,  1372,  1471,
    1244,   817,  1231,  1232,  1233,  1564,  1789,  1790,  1791,  1649,
    1835,  1836,  1837,   137,  1301,   351,  1167,   159,   576,   722,
     863,   138,   420,   139,   140,   141,   142,   143,    39,    84,
      76,    85,   378,    77,    86,   279,   280,   478,   625,   763,
     281,   282,   283,   284,   490,   641,   285,   286,   287,   288,
     289,   290,  1076,  1077,   493,   645,   646,   790,  1064,   791,
     924,   925,  1069,  1070,  1208,  1209,  1340,  1444,  1618,  1445,
    1538,  1697,  1539,  1540,  1080,  1541,   638,   778,   484,  1528,
     479,   768,   903,  1051,   626,   764,  1614,  1615,   485,   500,
    1543,  1628,  1770,   379,   464,   465,   616,   617,   618,   757,
     188,   758,   894,  1178,   895,  1036,  1317,  1037,  1038,  1039,
     813,  1091,  1717,  1779,  1780,  1825,  1826,  1827,  1867,  1868,
    1908,  1909,  1942,  1943,  1944,  1945,  1975,  1976,   811,   321,
     415,   510,   511,   669,   461,  1552,  1637,  1349,  1455,  1452,
     739,   779,  1179,   939,   797,   930,  1318,  1428,  1412,  1504,
     322,   410,   260,   367,   396,   481,  1378,  1476,   189,   262,
     190,   263,   191,   264,   192,   265,   193,   266,  1040,  1182,
     194,   267,  1041,  1183,  1671,  1742,   462,   609,   907,  1052,
     637,   774,   323,   411,   172,   165,   250,  1795,  1319,  1429,
     986,  1136,  1093,  1222,  1703,  1764,  1042,   482,   527,   685,
     971,   771,   380,   458,   144,   201,   145,   202,   578,   701,
     444,   593,   146,   205,   147,   204,   148,   203,   149,   206,
     150,   207,    87,   111,    78,   107,    79,   108,    88,   112,
      89,   113,    90,   114,   864,   999,   292,   198,   105,   163,
    1545,  1621,  1044,   257,  1045,  1184,  1046,  1185,  1047,   454,
     890,  1026,   422,   513,   673,   808,  1275,  1393,   160,   242,
     972,  1105,   973,  1106,   800,   933,   579,   702,   151,   208,
    1320,  1430,  1152,  1283,  1413,  1505,  1593,  1675,   541,   699,
     580,   703,  1004,  1157,   152,   209,   581,   704,   256,   361,
    1005,  1158,   734,   873,    50,    91,    21,    30,   582,   705,
      97,   122,   195,   268,    22,    31,   583,   706,   584,   707,
    1474,  1563,  1321,  1431,    23,    32,   619,   753,  1116,  1247,
     951,  1098,   752,   886,  1562,  1648,   153,   210,   585,   708,
    1128,  1261,  1576,  1657,   674,   809,  1117,  1248,   586,   709,
     587,   710,   154,   211,   975,  1107,   675,   810,  1150,  1281,
    1054,  1196,   588,   711,   155,   212,   735,   874,    24,    33,
    1287,  1407,   976,  1108,  1577,  1658,   156,   213,   589,   712,
     590,   713,   294,   390,   647,   785,   295,   383,   296,   384,
     765,   898,   648,   786,   297,   385,   782,   913,  1066,  1203,
     298,   386,  1215,  1344,   299,   387,   486,   632,   480,   623,
     300,   388,   301,   389,   627,   761,     8,    27,    58,    59,
      60,    41,    61,    62,   432,   977,   381,   592,   230,   837,
      42,   168,   683,   244,    43,  1160,   639,  1168,   537,   331,
     339,   332,   241,   518,   519,   952,   812,    25,   336,   440
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -1812
static const short yypact[] =
{
     205,   118,   123,   149,   250, -1812, -1812,   332, -1812,   621,
     123,   123, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   422,   547,   547,
     432,   119,   119,   432, -1812,   345, -1812, -1812,   345, -1812,
   -1812,   695,   615,   642, -1812,   240, -1812, -1812, -1812, -1812,
   -1812,   218,   358, -1812, -1812, -1812, -1812,   326, -1812, -1812,
   -1812,   836,   390, -1812, -1812, -1812, -1812, -1812,   508,   508,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   119, -1812, -1812,   528, -1812,  1142, -1812,   505, -1812,
   -1812, -1812,   508,   508, -1812,   541,   498,    93,    75,   575,
     579,   119,   695,   695,   695,   528,   623,   623, -1812, -1812,
   -1812, -1812,   119, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   541, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   570,   570, -1812,   570,   570,   570, -1812, -1812,   534,
     612,   594,   594,   695, -1812, -1812,   656,   541,   606, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   692,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   703,   695,   326,
   -1812,   695,    93,    93,    93,   388,    93,    93,   695,   695,
     156,   695,   695,   695, -1812,   656, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   248,   681, -1812,   681,   681,   681,   681,
   -1812, -1812, -1812, -1812,   508, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812,   695, -1812, -1812, -1812,   541, -1812,   541, -1812,
     695, -1812, -1812, -1812,   656, -1812, -1812,   695,   413, -1812,
   -1812,   575,   695,   695,   695,   695,   695,   695,   695,   672,
     672, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   409, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   696,   528, -1812, -1812, -1812,   100, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   679, -1812,   528, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   140, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812,   668, -1812,   686, -1812, -1812, -1812, -1812, -1812,
   -1812,   119, -1812, -1812, -1812, -1812, -1812,   695,   528, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
     702, -1812, -1812,   695,   119,   119,   695,   119,   119,   119,
     119, -1812,   658, -1812, -1812,   154,   528,   528, -1812,   528,
   -1812,   528,   528, -1812,   528,   528, -1812, -1812, -1812, -1812,
     695,   695,   528,   681, -1812, -1812, -1812, -1812,   692, -1812,
   -1812,   716, -1812,   697, -1812, -1812, -1812, -1812,   686,   699,
     704, -1812, -1812,   704,   697, -1812, -1812, -1812, -1812, -1812,
     647, -1812,  2971, -1812,   726, -1812, -1812, -1812,   597,   528,
   -1812, -1812, -1812,   528,   695,   575, -1812, -1812,   695,   575,
   -1812,   702, -1812, -1812,   639, -1812,   645,   575, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   575, -1812,   680, -1812,
   -1812,   695,   695, -1812, -1812,   177, -1812, -1812,   452, -1812,
   -1812,   177, -1812, -1812,   469,   177,   686, -1812, -1812,   686,
     745,   528,   528, -1812, -1812, -1812, -1812, -1812,   681, -1812,
     753, -1812,   412,   695, -1812,   681,   528, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   734, -1812, -1812,
   -1812,   528, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   528,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   739,   201, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   762, -1812, -1812,
   -1812, -1812, -1812, -1812,   717, -1812,   717,   318,   472, -1812,
   -1812, -1812,   163,   695, -1812,   597,   528,   597, -1812,   647,
     326,   594,   597,   528,   692, -1812, -1812, -1812, -1812,   695,
   -1812,   692, -1812, -1812,   661, -1812,   706, -1812, -1812, -1812,
   -1812, -1812, -1812,   119, -1812,   718, -1812, -1812, -1812, -1812,
   -1812, -1812,   357, -1812,   154,   154, -1812,   696,   745,   528,
     110,   696,   528, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
     696, -1812,   696,   776, -1812,   708,   692,   716,   692, -1812,
   -1812,   681, -1812, -1812,   779, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   682,   570, -1812, -1812,   528, -1812, -1812,
   -1812, -1812,   528, -1812,   541,   695,   765, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812,   528, -1812, -1812,   715,   695,
     528,   695,   695,   695,   695,   695,   695,   695,   695,   695,
     695,   695,   695,   119, -1812,   541,   776, -1812, -1812, -1812,
   -1812, -1812,   534,   741, -1812,   670, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   647, -1812,
     692, -1812, -1812, -1812, -1812, -1812, -1812,   594, -1812, -1812,
   -1812, -1812, -1812,   695, -1812,   723,   645,   847, -1812, -1812,
   -1812,   695, -1812, -1812, -1812, -1812, -1812, -1812,   745,   528,
   -1812,   695,   781, -1812,   695,   154, -1812,   692, -1812,   371,
   -1812, -1812, -1812, -1812,   781,   115,   115, -1812,   772,   772,
   -1812,   569, -1812, -1812,   765, -1812, -1812, -1812,   765, -1812,
   -1812, -1812,   541,   541, -1812, -1812, -1812, -1812,   695,   260,
     380, -1812,   672,   545, -1812, -1812,   700, -1812, -1812,   700,
   -1812, -1812, -1812, -1812, -1812,  1682, -1812, -1812,   696, -1812,
   -1812,   489, -1812, -1812,   692, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   449, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   491, -1812,
   -1812, -1812, -1812,   695,   695,   528, -1812,   681, -1812, -1812,
     716,   528, -1812,   716, -1812, -1812,   695, -1812, -1812, -1812,
   -1812, -1812, -1812,   623,   495, -1812,   584, -1812,   695, -1812,
   -1812, -1812,   692, -1812,   716, -1812, -1812, -1812,   688, -1812,
   -1812, -1812, -1812,   695, -1812, -1812,   528, -1812, -1812, -1812,
   -1812, -1812, -1812,   600,   639, -1812, -1812, -1812, -1812,   154,
     695, -1812, -1812,   695, -1812,   528, -1812, -1812,  1142,   695,
   -1812, -1812, -1812, -1812,   783, -1812,   304,   765,   682, -1812,
   -1812, -1812,   647, -1812, -1812, -1812,   765, -1812, -1812, -1812,
   -1812,    63, -1812, -1812,    63, -1812,   139, -1812,    56,   698,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   672,   712, -1812,
     791, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   745,   700, -1812,   694,   733,   647,   695,
   -1812, -1812, -1812, -1812,   789,   534,   733,   692, -1812, -1812,
    1142, -1812, -1812,   782, -1812, -1812,   528,   765, -1812,   716,
   -1812, -1812, -1812, -1812, -1812, -1812,   695,   645, -1812, -1812,
     535, -1812, -1812, -1812, -1812, -1812,   226, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   772,   695, -1812, -1812,   528,   154, -1812, -1812, -1812,
     177,   781,   781, -1812, -1812, -1812, -1812, -1812,   661,   706,
   -1812,   645, -1812,   696, -1812, -1812, -1812, -1812,   763, -1812,
     745, -1812,   716, -1812, -1812,   505, -1812, -1812, -1812,   702,
   -1812, -1812,   528, -1812,   517, -1812,   692,   700,   695, -1812,
   -1812, -1812, -1812,   696, -1812,   695,   695,   695,   695, -1812,
     749, -1812,   749,   749, -1812, -1812,   570, -1812, -1812, -1812,
   -1812, -1812,   374,   699,    64, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   695, -1812, -1812,   696,
   -1812, -1812,   755, -1812, -1812, -1812,   692,   160, -1812, -1812,
     563, -1812,   534, -1812, -1812, -1812,   705,   695,   695,   748,
   -1812, -1812,   717, -1812, -1812,   528,   505, -1812,   668,   700,
     716, -1812, -1812,   647, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812,   695,   695,   695,   695, -1812,   343,   545,   584,
     584, -1812,   743,   528, -1812, -1812,   695,   154, -1812, -1812,
     528, -1812, -1812,   695, -1812, -1812, -1812,   723, -1812, -1812,
   -1812, -1812,   745, -1812, -1812, -1812,   692, -1812, -1812,   534,
   -1812, -1812,   695, -1812,   814, -1812,   783, -1812, -1812, -1812,
   -1812,   704, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   139,   735,   139, -1812,   695,   695,   738,
   -1812,   699,   765,   834, -1812, -1812, -1812,   139,    56, -1812,
     528,    76,    92, -1812, -1812, -1812, -1812, -1812,   806, -1812,
     541,   541, -1812, -1812, -1812, -1812, -1812,   681, -1812,   748,
   -1812,   695, -1812,   695, -1812,   681, -1812, -1812, -1812, -1812,
   -1812,   134, -1812, -1812,   647,   534, -1812, -1812,   782,   534,
     776, -1812, -1812, -1812, -1812, -1812,   647, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   765, -1812,   758, -1812,   781, -1812,
   -1812,   494, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   645,
     886, -1812, -1812,   154,   695, -1812, -1812, -1812, -1812,   745,
   -1812, -1812, -1812,   765, -1812, -1812,   702, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   528, -1812, -1812,   541, -1812, -1812,
   -1812, -1812,   740,   751, -1812, -1812, -1812, -1812, -1812,   754,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   695, -1812, -1812, -1812, -1812, -1812,   134,
   -1812, -1812, -1812,   748, -1812,   647, -1812,   695, -1812,   647,
   -1812, -1812, -1812,   534,   748,   528, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   168,   528, -1812, -1812,   695,   695,
     695,   695,   584, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   936,   291, -1812,   436, -1812, -1812, -1812,
   -1812, -1812, -1812,   692,   220, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,   692, -1812,   656,
   -1812, -1812,   746, -1812,   570, -1812,   695, -1812, -1812, -1812,
   -1812,   528, -1812,    86, -1812, -1812,   762,   776, -1812, -1812,
   -1812,   647, -1812,   748,   831, -1812, -1812, -1812, -1812,   595,
   -1812, -1812, -1812, -1812,   695,   695, -1812, -1812, -1812,   463,
     647, -1812,   733, -1812, -1812, -1812, -1812,   748, -1812,   716,
     528, -1812, -1812, -1812, -1812, -1812,   528, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   591, -1812,   131,   294,
   -1812, -1812,   288, -1812, -1812, -1812, -1812, -1812,   647, -1812,
     844,   540,   745, -1812,   745,   745, -1812,   849, -1812, -1812,
     647, -1812,   570,   286, -1812, -1812, -1812, -1812, -1812, -1812,
     702, -1812,   692, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   782, -1812, -1812, -1812, -1812, -1812, -1812,   692,   846,
     844,   647, -1812, -1812, -1812, -1812, -1812, -1812,   647,   686,
     846, -1812, -1812,   534, -1812,   647, -1812,   681, -1812,   134,
   -1812, -1812, -1812,   716, -1812,   857,   368, -1812, -1812, -1812,
   -1812,   695, -1812,   138,   385,   333,   686, -1812,   716, -1812,
     702, -1812,   834, -1812, -1812, -1812,   692, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   696, -1812, -1812, -1812,   260, -1812,
   -1812, -1812, -1812,   781, -1812, -1812,   702,   119,   119, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   681, -1812,   647, -1812,
   -1812,   197, -1812,   834, -1812,   695, -1812, -1812,   528, -1812,
   -1812,   656, -1812,   688, -1812,   647, -1812,   748,   745, -1812,
   -1812,   860,   528,   846, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   765,   765,   333,   528,
   -1812, -1812, -1812, -1812, -1812,   765, -1812, -1812, -1812,   781,
   -1812, -1812, -1812, -1812, -1812,   656, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812,   528,   843,   776, -1812, -1812,
   -1812, -1812,   695, -1812, -1812, -1812, -1812, -1812,   782,   197,
     647, -1812, -1812, -1812, -1812, -1812, -1812,   482, -1812,   765,
   -1812, -1812, -1812,   436,   695,   781,   716,   765, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   639,
   -1812,   645, -1812,   702, -1812, -1812, -1812, -1812, -1812,   704,
   -1812, -1812, -1812,   692, -1812, -1812, -1812,   782, -1812, -1812,
   -1812, -1812, -1812,   694, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   647,   197,   528,   528, -1812, -1812,   294, -1812, -1812,
   -1812, -1812, -1812, -1812,   661,   706, -1812, -1812, -1812, -1812,
   -1812,   776,   349, -1812,   541,   784, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   528,   849, -1812, -1812, -1812, -1812,
   -1812, -1812,   563,   765, -1812, -1812,   785, -1812, -1812,   782,
   -1812,   846, -1812, -1812, -1812,   723,   645,   384, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812,   528, -1812, -1812, -1812, -1812, -1812,   782,   782,   695,
   -1812,   541, -1812, -1812, -1812, -1812, -1812,   733,   708, -1812,
   -1812,   765, -1812, -1812, -1812, -1812, -1812,   623,   495, -1812,
    2165, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   782, -1812,   846, -1812,
   -1812, -1812, -1812, -1812,   645, -1812, -1812,   535,   648, -1812,
      63,   842,   226, -1812, -1812, -1812, -1812, -1812, -1812,   474,
   -1812, -1812, -1812, -1812,   739, -1812,   776,   541, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,   717, -1812,
     846, -1812, -1812, -1812, -1812,   545, -1812,   528, -1812,   749,
     517,   749, -1812, -1812,   343,   584,   584, -1812, -1812,   776,
   -1812,   541,   695, -1812, -1812, -1812,   699, -1812,   541, -1812,
     197,   534, -1812,   197, -1812, -1812,   304, -1812,   139,   814,
   -1812,   783,   139,   545, -1812, -1812, -1812,   765,   645, -1812,
   -1812, -1812, -1812, -1812,   776,   446, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812,   765, -1812, -1812,   702,   584,
   -1812, -1812, -1812, -1812, -1812,   739, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   647, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812,   545, -1812, -1812, -1812, -1812,   541,   528, -1812,
   -1812, -1812,   776, -1812, -1812,   745,   745,   745,  2165, -1812,
   -1812,   541, -1812,   776, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812,   528,   765,   781, -1812, -1812, -1812, -1812,
   -1812,   528,   541, -1812, -1812,   702, -1812, -1812,   541, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
   -1812, -1812, -1812, -1812, -1812,   674, -1812,   828, -1812,   822,
     150,   850,   851, -1812, -1812,  -108, -1812,   158,   476, -1812,
     -98, -1812,   -90,  -353,   542,  -690, -1812,   464,   551,   266,
   -1812,   459, -1812,   468, -1812, -1075, -1812, -1812, -1812, -1812,
     -74,   -71, -1812, -1812,  -935,  -933,  -420, -1616, -1812,  -886,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,  -484,  -669,
   -1812,  -823, -1812, -1812, -1812, -1812, -1812, -1181, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812,  -373,  -582, -1812,
    -685,  -829, -1812, -1812, -1812,  -580, -1812, -1280,  -315,  -678,
    1026,  -442, -1812,   111,   113,  -493, -1059,  -178, -1812, -1812,
    -791,  -177, -1812,  -290,  -289, -1812, -1812,  -845,  -835, -1812,
   -1812,  -873,  -872, -1812, -1812, -1812, -1206,  -973, -1812,  -566,
   -1812, -1812, -1812, -1812,  -304, -1812,  -788, -1812,   924, -1812,
   -1812, -1812,   701, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812,   471, -1812, -1812, -1812, -1812,
   -1812, -1812,  -996,  -386, -1812, -1812, -1096, -1812, -1812, -1812,
   -1812,  -369, -1812,  -468,  -365, -1812,  -642, -1812, -1812,  -719,
   -1812, -1812,  -786, -1812,  -989, -1812,  -550,  -556,  -547, -1812,
   -1812, -1812,  -213, -1812, -1812, -1812,  -833, -1812,  -187, -1812,
   -1812,  -726, -1812,   583, -1812,  -864, -1812,  -721, -1812, -1812,
    -112, -1138, -1812,  -955, -1256, -1812, -1001, -1134, -1812, -1812,
   -1012, -1022,  -962, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,  -954,  -620,
   -1812, -1812,    48, -1812, -1386, -1067,  -787, -1385,  -640, -1478,
    -268,  -102,   330, -1812,   103, -1812,  -628, -1812, -1317, -1812,
    -261, -1812,  -214, -1812,  -406, -1812, -1441, -1812,  -145, -1812,
    -447, -1812,  -446, -1812,  -444, -1812,  -438, -1812,  -981, -1812,
    -435, -1812,  -952, -1812, -1498, -1812,  -445, -1812,  -751, -1812,
    -439, -1812,  -165, -1812,   -50,   -94, -1812,  -831, -1812, -1812,
   -1812, -1812, -1548, -1812, -1812, -1812,  -273, -1812, -1812, -1812,
    -463, -1812,  -772, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
    -159, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   690, -1812,    36, -1812,
   -1514, -1812,  -101, -1812, -1811, -1812, -1807, -1812,  -254, -1812,
   -1160, -1812,  -252, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,  -883, -1812, -1812, -1812, -1812, -1812,
    -606, -1812,  -963, -1812, -1812, -1812, -1812, -1812,  -587, -1812,
   -1812, -1812,  -307, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812,  -359, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
     823, -1812, -1812, -1812,   986, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812,  -599, -1812,   992, -1812, -1040, -1812, -1812, -1812,
    -728, -1812, -1024, -1812, -1812, -1812,  -789, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,  -229, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812,   -93, -1812, -1812, -1812,  -773, -1812,
    -651, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,  -171, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812,   112, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,
   -1812, -1812, -1812, -1812, -1812, -1812, -1812, -1812,  3751,   995,
     235, -1812, -1812,   622,  1181,  -217,  1012,  -256,  1803,  -645,
    3827,  -172,  -906,   109,  -202, -1057,  3599,   -21, -1777,   117,
    -232,  -215,   351,  -337,  -356,  -680,  -240,  1506,    96,  -453
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -840
static const short yytable[] =
{
      35,    36,    37,    38,   366,   196,   341,   354,   326,   174,
     291,   419,   743,   179,   492,   516,   610,   497,   397,   668,
     725,   252,   634,  1025,   594,   343,   293,  1188,   642,    94,
     395,  1210,   642,   917,   968,   891,   974,   969,  1088,  1307,
     944,   348,   214,  1162,  1205,   102,   103,  1337,  1256,  1180,
     814,  1243,  1427,  1245,   248,  1322,  1323,   109,   110,   325,
     412,   327,   328,   329,   330,  1257,   115,   116,   117,   118,
     318,   684,  1554,   253,  1555,   530,  1702,  1640,  1181,   535,
     358,   446,  1492,  1212,   166,  1217,   784,   772,   773,   691,
    1280,   954,  1420,   529,  1912,   599,   532,   534,  1398,  1951,
     199,   794,  1679,  1952,   798,   106,  -803,  -803,  1335,   360,
    1634,  1705,  1219,  -803,   938,   215,  -821,  -821,     5,  -821,
    -821,   181,   377,   223,   224,   225,   226,   227,   228,   229,
     437,   120,   234,  1805,  -821,  1509,  -821,  -821,   161,   162,
    1410,  -828,   958,  1313,  -803,   453,   254,    98,   259,  -803,
    -803,   878,   353,   258,   355,   523,  -821,    16,   261,  -821,
    -821,   680,  1114,  -821,   164,   417,  -149,   269,   727,  -828,
     780,  1115,   688,   417,  -182,   270,  1375,   693,   955,    54,
    1118,   998,  1010,   959,  -803,   120,   393,  -803,   451,   987,
     502,  1712,   503,  1299,  1772,  1760,  1858,  -821,  1328,  1573,
    -821,  1572,    54,  -115,   514,  1125,   633,  -125,   365,     6,
       6,   369,   491,  -821,   495,  1574,   135,  -824,  1411,   483,
    1315,  -125,  1497,   651,  -828,   591,   654,     6,   910,   596,
    -828,  -821,  1745,  1508,  -821,   362,  1437,  1438,   731,   670,
      -5,  1316,   738,  2043,  -149,  -824,  -149,   828,   507,     6,
     438,  -821,  -182,     6,  -182,   319,   732,     6,   672,  1812,
     231,   233,    40,   236,   238,   240,  1147,  1951,   320,   245,
     337,  1952,   398,   399,   400,   401,   402,   403,   404,   405,
     406,  -115,   407,  -115,    10,  -125,  -125,  -125,   733,  -125,
       6,  -803,  1686,  -125,  -839,   488,   101,   393,  1525,  1210,
     413,  -821,   861,  2089,  -821,   866,  1176,  -821,  -821,  -821,
    1201,  1202,  1587,   393,   657,    95,  -827,  -803,  1352,  1354,
    1355,  -827,  1226,  -827,  -171,   920,   921,  -821,  -125,  -821,
    -821,  1090,   965,  1258,    44,    12,  1608,   346,  -171,    13,
      -4,    -6,  -171,   662,    14,   455,  -803,  -827,  1313,   333,
     678,   604,   338,  -820,   340,   928,   342,   459,    -7,   932,
     836,   467,   820,  1902,   120,  1100,    15,  1518,  1314,  1228,
    1061,  1062,  -803,  1794,  1758,  1663,   775,   393,   319,   259,
     783,   901,  1074,   961,  2028,  1537,    11,  2031,  -803,   793,
    1978,   795,  -819,   508,     6,  -803,   394,   750,  -821,   515,
     658,  -821,  -171,  -171,  -171,   606,  -171,  -819,   719,   608,
    -171,  1153,  1090,   724,   633,   726,   319,   621,   393,  -803,
       6,  -821,  -803,    95,  -821,  1315,   622,    12,   425,  -821,
    1967,    13,  -821,   171,   408,   409,    14,   394,  1701,   436,
     394,  1303,   801,   120,    13,  -171,  1316,   364,   364,    14,
    -820,   427,    44,    12,  1547,  -124,  -819,    13,    15,   629,
     259,  -803,    14,  -821,   394,   635,  -821,   120,   173,  -124,
     164,   259,  2003,  -821,  1018,   636,  -821,  1021,  -828,   988,
    -231,  1294,  -828,   393,    15,   661,   665,  -803,  1537,  -231,
    1384,     6,  -819,   745,   740,   781,   805,   870,  -821,  1198,
     181,  1670,  1255,   393,    48,    55,  1756,   741,  1095,  1199,
     394,  -821,   746,  -821,     6,   984,   906,  1104,   912,   425,
     700,    48,     6,   643,    26,   868,   666,   644,    55,  1059,
     436,  -803,   667,  -124,  -124,  -124,  -803,  -124,   806,   104,
    1092,  -124,  1804,   521,   796,   394,   633,   377,  1627,  1603,
    1377,  1031,   970,  1032,  -827,  1619,   716,   717,   718,   120,
    -113,   720,   721,   911,  -113,   164,     6,   979,   728,   729,
      49,    56,  -827,  -827,  -827,  -827,  -124,  1435,  1171,  1002,
    1436,   158,   904,  1003,  1620,  1161,  1305,    49,  1194,  -822,
     826,  1848,  1366,   834,    56,   171,  1295,  1200,   756,  2087,
    1031,  -824,  1032,   595,  1405,   597,   767,  1031,   602,  1032,
    1409,   173,   394,  1172,  1155,   120,   173,  1033,  1034,   364,
    -113,   854,  -827,  1388,     9,  1133,   788,   789,  1392,  -828,
    -828,  1225,     9,     9,   181,   182,   183,   184,   185,   884,
     186,    44,    12,  1132,  1221,  1134,    13,  -113,  -113,  -113,
    1330,    14,   816,  1901,   819,  -824,    74,   393,   665,    75,
     780,  1063,  -801,  1192,  1001,  -801,  -801,   181,  -801,  -801,
     831,  -801,  2076,    15,  2077,  1090,  -837,  -837,  -837,  1411,
    1592,  1923,  1924,   992,    28,    29,   985,    80,  1050,    81,
      82,    44,    83,   723,  -817,   723,   528,   255,   666,   528,
     259,   320,   377,   319,   667,  1904,  1094,  1416,   936,   937,
    -830,   443,  1015,   875,   394,  1765,   877,  1767,  1491,  1426,
    1965,   187,   881,   460,   477,   393,  -828,   525,  -839,  1060,
    1282,   887,  1284,   540,   120,  -815,  -821,  1510,  1101,  -821,
    -825,  1828,  -825,  -825,   900,  -825,  -113,  -821,  1082,  2032,
     615,   624,   908,  -827,  -828,    26,  -821,   -93,   271,   272,
     914,   915,   273,  -827,  1304,   274,  -828,   275,  -824,   751,
     923,   276,   277,   278,  1264,   762,   931,  1490,  -821,   934,
     796,  1028,   806,   818,   121,  1367,   940,   799,   636,  -828,
    -820,  1560,   862,  1163,  1265,   889,  1447,  -113,   822,  1348,
    1863,   633,  1123,   906,  -827,   180,   830,   950,   132,   832,
    1053,   833,  1213,  1376,  -834,  1149,  1151,  1591,  1500,   855,
    -826,  1224,  1503,   853,  1126,  -828,  1214,   958,  1598,     6,
    1274,  -828,  1286,   135,  -162,  2004,   994,  -827,   996,   997,
    -825,  1114,  1236,  1000,  1377,   126,  1002,   732,  1006,  -825,
    -828,  2015,  2016,  1473,  -828,  1561,  -817,  1016,  -821,  -821,
    -821,  -821,  -821,  -828,  -821,  1670,  1410,  1794,  -828,  1027,
    -834,  1220,  -837,    92,    99,  1417,  1362,  -821,  1272,  1421,
    -821,  -825,  1227,  -825,  -825,  1433,  -825,   517,    70,    71,
    1140,  1056,   433,   533,  1361,   522,  1363,  -821,  -821,  -821,
    -821,  -821,  1730,  -821,  1585,  2061,  1130,  1668,  1260,  1131,
    1887,  1460,  1888,  1645,  1457,  1850,  1493,  1667,  1798,  1414,
    2008,  2025,  2012,  1601,  1597,  1687,  1810,  1097,  1685,  1946,
    1659,  1602,  1278,  1688,  1929,  1246,   963,  1664,   964,  1249,
    1110,  1359,  1360,  1112,  1839,  -821,  1251,   181,   182,   183,
     184,   185,  1122,   186,  1840,  1124,  1180,  1291,  1947,  1449,
    1419,  1629,  1876,  1877,  1387,    72,   650,  1439,  1782,  1142,
    2013,   382,  1527,  1647,  1441,  1694,  1762,  1817,  1828,  1766,
    1327,  1860,  1974,  2014,  2033,  1181,  1987,  1982,  1087,  2075,
    1822,  1711,  1382,  1507,   807,  1698,  1530,  1531,  1548,  1532,
    1750,  1871,  1346,  1463,  1674,  1533,   904,  1177,  1534,  1302,
    1385,  1677,   414,  1331,  1819,  1930,   421,  1699,  1682,  1434,
    1478,    66,   303,  1464,  1700,   577,   428,    67,  2062,  1381,
    1852,  1259,  1753,  1197,  1085,  1065,    73,  1415,   829,  1933,
      51,    57,     0,     0,     0,     0,   612,  1373,    68,    69,
       0,     0,     0,     0,     0,     0,     0,    51,   457,     0,
    1099,  1605,  1511,  1102,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1113,  1811,  1399,     0,     0,
       0,  1740,  1237,     0,   346,     0,   487,   180,     0,     0,
       0,     0,   496,     0,   498,   499,     0,  1946,  1754,  1253,
     419,     0,   506,  2034,  2036,  2037,  1166,  1262,     0,     0,
       0,     0,  1396,  1612,     0,     0,     0,     0,  1273,   968,
    1406,   974,   969,     0,  1277,  1655,  1947,     0,     0,     0,
     197,     0,     0,     0,  1285,  1456,  1625,  1458,  1459,   601,
       0,     0,     0,   180,     0,  1905,     0,     0,  1845,     0,
       0,     0,     0,     0,     0,     0,     0,  1308,   740,     0,
       0,  1704,     0,  1806,     0,     0,   419,  1481,     0,     0,
    1472,  1326,     0,  1544,     0,     0,  1389,  1390,   123,   124,
     125,   126,   127,  1680,  1336,  1710,     0,  1873,   128,  1339,
       0,   129,   180,     0,  1345,     0,     0,     0,     0,     0,
       0,  1499,   671,     0,     0,     0,   679,  1689,  2011,     0,
    1736,  1732,  1291,  1972,     0,  1365,     0,     0,     0,     0,
       0,   421,  1708,   130,  1857,   818,     0,     0,     0,   698,
       0,   131,     0,     0,  1380,   324,     0,     0,     0,  1550,
       0,  1922,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1391,   132,     0,  1394,     0,     0,   352,     0,  1542,
     133,     0,  1467,     0,     0,     0,   134,  1768,  1769,     0,
       0,   723,   135,  1470,     0,  1960,  1776,     0,     0,     0,
       0,  1423,     0,     0,  1425,     0,   180,   968,     0,   974,
     969,  1399,     0,   747,     0,   514,  1432,     0,  1529,     0,
       0,   857,     0,   859,   860,     0,     0,   865,     0,   867,
       0,   869,  1578,     0,     0,  1609,  1440,     0,     0,     0,
    1815,     0,     0,     0,  1559,     0,  1678,     0,  1820,   180,
       0,     0,   180,     0,  1834,  1347,   416,  1358,  1830,  1616,
       0,     0,     0,  1692,  2104,  1546,     0,     0,     0,  2054,
       0,  2026,  1113,  1706,  1113,     0,     0,     0,   514,     0,
    1708,     0,   442,     0,     0,     0,  1113,   421,     0,     0,
     449,     0,   823,     0,  1669,     0,     0,     0,     0,     0,
    2050,     0,     0,  1724,     0,   180,     0,     0,     0,     0,
     839,  1506,     0,   466,     0,  1891,     0,     0,  1512,  1513,
       0,  1515,  2001,  1422,     0,  1520,     0,     0,     0,     0,
     489,     0,   494,  1526,  1896,   494,     0,  1941,     0,     0,
       0,     0,  1716,     0,     0,     0,     0,     0,   512,     0,
    1862,     0,  1895,  1551,  1842,     0,     0,  1729,     0,   993,
       0,   995,  1684,     0,   531,     0,     0,  1558,     0,     0,
       0,     0,  1841,     0,  1843,     0,     0,     0,     0,  1014,
     600,  1957,  1931,   603,     0,  1582,     0,  2074,  1023,     0,
       0,  1586,     0,  1589,     0,     0,     0,     0,   620,     0,
    1879,     0,     0,  1882,  1813,  1609,     0,  1477,  1600,     0,
    1544,     0,     0,     0,     0,  1607,  1788,  1058,  1878,     0,
    1880,  1739,     0,  1883,     0,     0,   649,  1617,     0,     0,
       0,     0,  1998,     0,     0,   421,     0,  2010,     0,     0,
    1916,     0,    34,     0,     0,     0,     0,   439,     0,  1632,
       0,     0,  1977,     0,     0,     0,  1751,     0,  1646,   687,
    1917,     0,     0,   516,     0,     0,  1971,     0,     0,     0,
       0,     0,     0,    93,  1120,  1660,  1661,  1662,     0,     0,
     100,     0,  1665,  1666,     0,  1013,     0,     0,     0,  1673,
       0,  1019,  1676,     0,     0,     0,  1542,     0,  2040,     0,
    1786,  2029,     0,  1565,     0,  1941,  1043,  1568,     0,     0,
    1583,     0,     0,  2060,  1691,  1693,  2057,  1695,     0,     0,
    1092,     0,     0,     0,     0,     0,   520,   600,  1709,   600,
       0,   744,     0,     0,   600,     0,     0,   536,     0,     0,
     539,     0,     0,     0,     0,  1191,     0,  1616,     0,     0,
       0,     0,   759,  1793,     0,     0,     0,  1948,     0,     0,
       0,     0,     0,   216,   217,   218,   219,   220,   221,   222,
       0,     0,     0,     0,  1633,  2103,  1953,     0,     0,     0,
    2111,   792,  1546,     0,     0,  1755,     0,  1757,     0,     0,
       0,  1650,   804,     0,     0,     0,  1763,     0,     0,     0,
       0,     0,   324,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   825,     0,     0,  1980,     0,
       0,   682,     0,  1785,  1254,     0,  1170,  2009,     0,   824,
     394,   633,  -816,   120,     0,     0,     0,   695,     0,     0,
       0,     0,     0,     0,     0,     0,  1803,  1276,   128,   856,
    1807,   858,  1809,     0,     0,     0,     0,  1814,     0,  1816,
    1875,     0,     0,   872,     0,   180,     0,     0,     0,     0,
     744,     0,     0,  2081,     0,     0,   665,   357,     0,   957,
     958,     0,     0,  1831,     0,  1833,  1094,     0,   620,     0,
       0,   896,     0,     0,     0,  1847,     0,     0,  2102,   494,
    1744,     0,  1223,     0,     0,     0,  2108,   392,     0,     0,
       0,     0,   132,     0,  1861,  1935,   666,  1927,     0,     0,
       0,   959,   667,     0,   926,  1948,     0,  1866,     0,  1950,
     960,     0,     0,     0,     0,     0,     0,  2056,     0,  2058,
    2059,     0,     0,     0,  1953,     0,     0,   825,     0,     0,
       0,     0,     0,     0,     0,  1899,     0,     0,     0,     0,
    1796,     0,  1903,     0,     0,     0,     0,     0,     0,  1911,
       0,     0,     0,     0,  1802,  1298,     0,     0,  1744,   445,
       0,   447,     0,  1994,     0,   450,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1928,     0,     0,     0,  1043,
    1043,  1932,     0,  1934,     0,     0,     0,     0,     0,     0,
       0,     0,  1956,     0,     0,     0,     0,  2020,     0,   466,
       0,     0,     0,     0,  2027,     0,     0,     0,     0,  1969,
    1970,     0,  1049,     0,  1973,     0,     0,  1979,     0,  1981,
       0,  1744,     0,     0,     0,     0,     0,   494,  1989,     0,
       0,  2048,     0,  1454,  1870,     0,     0,     0,  1071,  1072,
    1996,     0,     0,   494,     0,     0,     0,     0,     0,     0,
     180,     0,   512,  2005,     0,   232,     0,   235,   237,   239,
       0,     0,     0,     0,     0,     0,     0,     0,   251,     0,
       0,     0,  2022,  2080,  2024,     0,     0,  1950,     0,     0,
       0,  2030,   825,     0,     0,   628,     0,  2090,     0,     0,
       0,  1502,     0,  2039,     0,  1135,  1119,     0,     0,   953,
       0,     0,     0,     0,  1144,     0,  1516,     0,  2109,     0,
     983,     0,   494,  1156,  2113,     0,     0,   317,   990,     0,
       0,     0,     0,     0,     0,     0,  2068,     0,     0,     0,
    2072,     0,     0,     0,     0,     0,     0,     0,     0,   620,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2083,  1009,     0,     0,   180,     0,   359,     0,     0,  1993,
    1567,   494,     0,  2093,     0,     0,     0,     0,     0,     0,
       0,  2101,     0,     0,     0,     0,     0,   723,     0,  1590,
       0,     0,   391,  1211,     0,     0,     0,     0,     0,     0,
       0,     0,  2019,   494,     0,     0,     0,     0,  1606,     0,
       0,     0,     0,     0,     0,   421,     0,     0,     0,  1744,
       0,   825,  1744,     0,   825,  1519,     0,  1113,     0,     0,
       0,  1113,  1043,     0,     0,  1230,     0,  2047,  1235,     0,
       0,     0,   766,     0,   324,     0,  1103,   426,     0,     0,
       0,   825,     0,     0,     0,     0,     0,     0,     0,   825,
     825,     0,   441,     0,     0,  1263,     0,     0,     0,     0,
       0,   983,     0,  1270,  1271,     0,     0,     0,  1139,     0,
       0,   421,  1293,     0,     0,  2084,     0,     0,     0,     0,
       0,  1031,     0,  1032,     0,     0,  2092,     0,     0,   759,
    1165,     0,     0,   394,   633,  -816,   120,   173,  1033,  1034,
     364,     0,     0,     0,     0,     0,   494,     0,     0,     0,
    1613,   128,     0,     0,  1454,     0,   649,     0,   509,     0,
       0,     0,     0,  1341,     0,     0,     0,     0,     0,  1726,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   665,
       0,   538,   957,   958,     0,     0,  1364,     0,  1738,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1747,
       0,     0,   825,     0,     0,     0,     0,   613,     0,     0,
     899,     0,     0,     0,     0,   132,  1374,     0,     0,   666,
       0,     0,  1234,     0,   959,   667,     0,     0,     0,     0,
       0,     0,  1401,   960,     0,     0,  1774,  1775,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   663,     0,   487,     0,     0,     0,     0,
     896,   681,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   689,     0,     0,   692,     0,   694,     0,     0,
     696,   697,     0,     0,   649,   649,     0,     0,   926,     0,
       0,  1211,     0,  1446,     0,     0,     0,   494,   714,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1748,   825,
     825,     0,     0,     0,     0,     0,     0,   825,     0,     0,
       0,     0,  1759,  1461,  1462,  1851,     0,     0,     0,     0,
    1855,  1468,  1856,     0,     0,     0,     0,   737,  1771,  1773,
    1483,     0,     0,   825,     0,     0,   825,   748,     0,   324,
       0,     0,  1234,  1496,     0,     0,  1874,  1486,     0,   754,
    1489,     0,     0,     0,     0,   180,     0,     0,     0,     0,
    1890,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   776,  1083,     0,     0,     0,     0,     0,   787,     0,
       0,     0,  1071,     0,     0,     0,     0,     0,     0,     0,
       0,   802,   494,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   821,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   825,     0,     0,  1954,  1141,     0,  1143,     0,   835,
       0,     0,     0,  1859,     0,  1569,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1293,     0,
       0,     0,     0,     0,     0,     0,  1173,     0,     0,     0,
    1611,     0,     0,   871,  1886,     0,     0,     0,     0,     0,
       0,     0,   879,     0,     0,     0,     0,  1469,   882,     0,
       0,     0,  2002,     0,     0,     0,   494,     0,     0,     0,
     892,     0,     0,     0,     0,     0,     0,     0,     0,  1644,
       0,  1920,     0,  1487,     0,     0,     0,     0,     0,     0,
     825,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   922,  1654,     0,     0,     0,  1401,     0,
    1949,     0,     0,     0,     0,  2045,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   945,     0,     0,     0,
       0,     0,     0,   825,     0,  2064,     0,     0,     0,  1341,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   989,
       0,     0,   991,     0,     0,     0,     0,     0,     0,     0,
    1718,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2007,     0,     0,
       0,  1735,     0,     0,     0,  1043,  1043,     0,     0,     0,
    1306,     0,     0,     0,     0,  1020,     0,  1022,     0,     0,
       0,     0,  1009,     0,     0,     0,     0,  1029,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1446,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1067,     0,  1043,
    1781,     0,     0,     0,  1638,  1784,     0,     0,  1643,     0,
    1084,     0,     0,     0,     0,     0,  1652,     0,     0,     0,
       0,     0,     0,  1496,     0,     0,     0,     0,   421,     0,
       0,     0,     0,     0,  1109,  1111,     0,  1611,   421,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,     0,  1681,     0,     0,     0,     0,
       0,   421,     0,  1829,     0,     0,  1145,   825,  1148,     0,
       0,     0,     0,     0,  1844,     0,     0,     0,     0,     0,
    1164,  1832,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1174,  1725,     0,   494,     0,
       0,  1728,     0,     0,     0,     0,     0,     0,     0,  1186,
       0,  1869,     0,     0,     0,  1737,     0,     0,     0,     0,
    1881,     0,     0,     0,  1451,     0,     0,  1644,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1206,     0,     0,     0,     0,     0,   620,     0,
       0,  1910,     0,     0,     0,  1913,     0,  1218,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1792,     0,     0,     0,     0,   466,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1801,     0,  1514,     0,     0,
       0,     0,     0,     0,  1961,  1266,     0,  1267,     0,     0,
       0,     0,     0,     0,   494,     0,   620,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1638,     0,
    1988,     0,     0,     0,     0,     0,  1296,  1297,     0,  1838,
    1792,     0,  1995,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   759,     0,  -817,     0,  -817,
       0,     0,     0,     0,     0,  1325,     0,     0,     0,  -817,
    2018,     0,  -817,  -817,  -817,  -817,  -817,     0,     0,     0,
       0,  1338,     0,   542,     0,  1342,  1838,   896,     0,     0,
       0,     0,     0,     0,     0,     0,  1643,  1889,     0,     0,
    2041,     0,     0,     0,     0,     0,     0,     0,  2051,     0,
       0,     0,     0,     0,     0,  -817,  1368,     0,  1369,     0,
       0,   543,     0,  2063,     0,     0,     0,     0,   544,     0,
       0,   545,  2071,     0,  -818,     0,     0,   546,  1386,     0,
       0,   547,   548,     0,     0,  2078,  -817,     0,     0,  1395,
       0,     0,   549,     0,     0,  -817,     0,   550,   551,     0,
       0,  -817,     0,     0,   552,     0,  -818,     0,     0,  1418,
       0,     0,   553,   554,     0,     0,  1424,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1992,     0,     0,     0,
       0,     0,     0,  1442,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1465,     0,  1466,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1475,
       0,     0,     0,     0,     0,  1480,     0,     0,     0,     0,
       0,     0,  1484,  1485,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1535,     0,     0,
       0,     0,     0,     0,     0,  1549,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1556,     0,     0,
    1557,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1579,  1580,     0,
       0,     0,     0,  1584,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1594,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1604,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1622,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1635,     0,  1639,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1656,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1672,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1690,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1720,  1721,  1722,  1723,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1731,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1749,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1761,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1778,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1800,     0,     0,     0,
       0,     0,     0,     0,     0,  1808,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1821,     0,     0,     0,
       0,     0,  1823,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1846,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1854,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1864,     0,
       0,     0,     0,     0,     0,  1872,     0,     0,     0,     0,
       0,     0,     0,     0,  1884,     0,  1885,     0,     0,     0,
       0,     0,  1892,  1893,  1894,     0,     0,     0,     0,     0,
       0,  1900,     0,     0,     0,     0,     0,     0,     0,     0,
    1906,     0,     0,     0,     0,     0,     0,     0,  1915,     0,
       0,  1918,     0,  1919,     0,     0,  1921,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1936,     0,     0,     0,  1955,     0,     0,  1958,     0,
    1959,     0,     0,     0,     0,     0,  1962,  1963,  1964,     0,
    1966,     0,  1968,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1983,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1990,     0,     0,
     247,   247,     0,     0,     0,     0,     0,  1997,     0,  1999,
    2000,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2023,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2044,     0,  2046,     0,  2049,     0,
    2052,  2053,     0,     0,  2055,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,  2065,     0,  2066,     0,
       0,  2069,  2070,     0,     0,  2073,     0,     0,   169,   170,
       0,     0,   175,   176,   177,   178,     0,  2079,     0,     0,
       0,     0,  2082,   200,     0,     0,     0,     0,  2085,  2086,
       0,     0,  2088,     0,  2091,     0,     0,     0,     0,     0,
    2096,  2097,  2098,  2099,  2100,     0,     0,     0,     0,     0,
    2106,     0,  2107,     0,     0,  2110,     0,     0,     0,  2112,
       0,  2114,  2115,  2116,   249,  2117,  2118,  2119,  2120,     0,
       0,  2121,     0,     0,   157,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   302,
       0,     0,   304,   305,   306,   307,   308,   309,   310,   311,
     312,   313,   314,   315,   316,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   463,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,     0,     0,     0,     0,     0,     0,
       0,   356,     0,     0,     0,   501,     0,     0,   363,     0,
       0,     0,     0,   370,   371,   372,   373,   374,   375,   376,
       0,     0,     0,     0,     0,     0,     0,   524,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     611,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   368,     0,     0,
       0,     0,     0,     0,     0,   652,     0,     0,     0,   656,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,     0,     0,   456,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   468,   469,   470,   471,   472,   473,
     474,   475,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,     0,
       0,   504,   505,     0,   429,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,   605,     0,     0,     0,   607,
       0,     0,     0,     0,     0,     0,     0,     0,   476,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   769,     0,
       0,     0,   630,   631,     0,     0,     0,   777,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   803,     0,
       0,     0,     0,     0,   676,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   614,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   640,     0,     0,
       0,     0,     0,     0,     0,   653,     0,   655,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   664,     0,     0,
     880,   677,     0,     0,   736,   883,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     749,     0,     0,     0,     0,     0,     0,   902,     0,     0,
       0,     0,     0,     0,   760,     0,     0,     0,     0,     0,
       0,     0,     0,   770,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   777,     0,     0,     0,   777,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   946,   947,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   956,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   827,     0,     0,     0,
       0,     0,     0,   755,     0,     0,     0,     0,     0,     0,
     838,     0,   840,   841,   842,   843,   844,   845,   846,   847,
     848,   849,   850,   851,   852,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1017,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     815,     0,     0,     0,   888,     0,     0,     0,     0,     0,
       0,     0,   897,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   905,     0,     0,   909,     0,  1073,     0,     0,
       0,     0,     0,  1081,     0,     0,   918,   919,     0,     0,
       0,     0,     0,  1089,     0,     0,     0,  1096,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   941,
     942,   943,   876,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   885,     0,     0,
       0,     0,     0,     0,   893,     0,     0,     0,     0,     0,
       0,     0,  1146,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     916,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     927,     0,   929,     0,  1011,  1012,     0,     0,   935,     0,
       0,     0,     0,     0,     0,     0,     0,  1024,     0,  1189,
    1190,     0,     0,     0,     0,     0,     0,     0,     0,  1048,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     777,   777,     0,     0,  1057,     0,     0,     0,     0,     0,
       0,  1081,     0,     0,     0,     0,     0,     0,     0,  1216,
       0,  1075,     0,     0,  1079,     0,     0,     0,     0,     0,
    1086,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1238,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1252,     0,
       0,  1030,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1055,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1154,  1068,     0,     0,     0,     0,     0,     0,     0,  1078,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1175,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   946,     0,     0,
    1324,     0,     0,     0,     0,     0,     0,     0,     0,  1121,
       0,     0,     0,  1195,     0,     0,  1129,     0,     0,     0,
       0,  1216,  1137,  1138,     0,     0,     0,     0,     0,  1350,
       0,     0,     0,  1353,     0,  1356,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1169,     0,     0,     0,     0,     0,     0,     0,  1229,
       0,     0,     0,     0,     0,     0,  1239,  1240,  1241,  1242,
       0,     0,     0,  1187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1269,     0,     0,
       0,  1204,     0,     0,     0,     0,  1207,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1288,  1289,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1309,  1310,  1311,  1312,     0,     0,     0,
       0,     0,     0,     0,  1250,     0,     0,  1329,  1453,     0,
       0,     0,     0,     0,  1334,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1351,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1300,     0,     0,     0,  1370,  1371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1383,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1332,  1333,
       0,     0,  1402,     0,  1404,     0,     0,     0,     0,     0,
    1343,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1357,  1553,     0,   463,  1350,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1570,
       0,  1379,     0,     0,     0,  1581,     0,     0,     0,     0,
       0,     0,     0,  1588,     0,  1448,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1397,     0,     0,     0,     0,
       0,     0,     0,     0,  1408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1624,     0,
       0,  1626,     0,     0,  1488,     0,     0,     0,  1630,     0,
       0,  1636,     0,   611,  1453,     0,     0,     0,  1501,     0,
       0,     0,     0,     0,     0,     0,  1653,  1443,     0,     0,
       0,     0,     0,  1450,     0,     0,     0,     0,     0,  1521,
    1522,  1523,  1524,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1479,     0,
       0,     0,     0,     0,  1482,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1707,  1566,     0,     0,
       0,     0,     0,     0,  1715,     0,     0,     0,  1719,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1595,  1596,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1536,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1752,     0,     0,     0,     0,     0,  1096,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1081,     0,  1081,     0,     0,     0,     0,
       0,     0,     0,     0,  1651,     0,     0,     0,     0,  1783,
       0,     0,     0,     0,     0,  1787,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1599,  1797,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1216,  1623,  1216,     0,     0,     0,
       0,     0,  1696,     0,     0,     0,     0,  1631,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1727,
       0,     0,     0,  1853,     0,     0,     0,     0,  1733,  1734,
       0,     0,     0,     0,     0,     0,  1624,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1746,     0,     0,     0,
       0,     0,     0,  1683,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1713,  1714,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1914,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1926,
       0,     0,     0,  1799,     0,     0,  1741,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1818,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1777,     0,  1985,  1986,     0,     0,     0,
       0,     0,     0,  1991,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2006,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2017,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1824,     0,  2035,     0,
    2038,     0,   946,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1925,     0,     0,     0,  2067,     0,     0,     0,     0,     0,
       0,     0,  1865,     0,     0,  1553,     0,   463,  1350,     0,
       0,   946,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1636,   611,  1453,     0,     0,     0,
       0,     0,     0,  1898,  2094,  2095,     0,     0,     0,     0,
       0,     0,     0,     0,  1907,  2105,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1937,     0,     0,     0,     0,
       0,     0,     0,  2021,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1984,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2042
};

static const short yycheck[] =
{
      21,    22,    23,    24,   258,   117,   238,   247,   225,   110,
     197,   326,   599,   115,   400,   421,   461,   403,   291,   512,
     586,   166,   485,   887,   444,   240,   197,  1039,   491,    50,
     291,  1071,   495,   784,   825,   756,   825,   825,   944,  1177,
     812,   243,   136,  1006,  1068,    66,    67,  1207,  1123,  1030,
     670,  1110,  1308,  1112,   162,  1189,  1190,    78,    79,   224,
     321,   226,   227,   228,   229,  1124,    87,    88,    89,    90,
     215,   524,  1458,   167,  1459,   431,  1624,  1555,  1030,   435,
     252,   354,  1399,  1072,   105,  1081,   642,   634,   635,   531,
    1147,   819,  1298,   430,  1871,   448,   433,   434,  1279,  1910,
     121,   651,  1600,  1910,   654,    69,    30,    32,  1204,   254,
    1551,  1625,  1085,    20,   804,   136,    41,    41,     0,    44,
      44,    11,    30,   144,   145,   146,   147,   148,   149,   150,
     344,    31,   153,  1749,    41,  1415,     5,    44,   102,   103,
       6,    78,    78,     5,    29,   362,   167,    51,     8,    73,
      31,   738,   246,   174,   248,   428,    41,     7,   179,    44,
      41,   517,   106,    44,    24,    73,     6,   188,   588,   106,
      60,   115,   528,    73,     6,   196,  1251,   533,   823,    29,
     969,   861,   872,   119,    28,    31,     9,    31,   360,   834,
     407,  1632,   409,  1166,  1708,  1693,  1812,    41,  1194,   113,
      44,  1481,    52,     6,   418,   977,    29,     6,   258,   134,
     134,   261,   399,    82,   401,   129,   130,    78,    84,    65,
      82,    20,  1403,   496,     4,   442,   499,   134,   775,   446,
      10,     5,  1673,  1414,   103,   256,  1332,  1333,   591,   512,
       0,   103,   595,  2020,    84,   106,    86,   686,   413,   134,
     344,    25,    84,   134,    86,     7,    93,   134,   512,  1757,
     151,   152,    27,   154,   155,   156,   994,  2078,    20,   160,
     234,  2078,   293,   294,   295,   296,   297,   298,   299,   300,
     301,    84,   303,    86,   135,    84,    85,    86,   125,    88,
     134,    31,  1609,    92,    76,   397,    61,     9,  1432,  1339,
     321,    41,   722,  2080,    44,   725,  1027,    16,    82,    18,
    1061,  1062,  1493,     9,   501,    97,    28,    31,  1224,  1225,
    1226,    27,  1094,    29,     6,   788,   789,    41,   127,   103,
      44,    27,   825,  1124,    94,    95,  1517,   241,    20,    99,
     135,   136,    24,   508,   104,   366,    86,    53,     5,   232,
     515,   453,   235,     4,   237,   794,   239,   378,     0,   798,
     697,   382,   677,  1861,    31,   952,   126,  1424,    25,  1097,
     920,   921,    86,    24,  1691,  1581,   637,     9,     7,     8,
     641,   767,   929,   825,  2000,    52,   136,  2003,    31,   650,
    1938,   652,     8,   414,   134,   109,    28,   611,    41,   420,
     502,    44,    84,    85,    86,   455,    88,    23,   580,   459,
      92,   998,    27,   585,    29,   587,     7,   467,     9,    31,
     134,    41,    65,    97,    44,    82,   476,    95,   332,    41,
    1928,    99,    44,    20,   317,   318,   104,    28,    53,   343,
      28,  1169,   656,    31,    99,   127,   103,    35,    35,   104,
       4,   334,    94,    95,  1450,     6,    72,    99,   126,   480,
       8,    73,   104,    41,    28,   486,    44,    31,    32,    20,
      24,     8,  1970,    41,   880,    23,    44,   883,     4,   835,
     106,  1161,    19,     9,   126,   506,    74,   107,    52,   115,
    1262,   134,   108,   601,   596,   640,   661,   729,     3,  1055,
      11,    19,  1122,     9,    28,    29,  1687,   597,   947,  1056,
      28,    16,   602,    18,   134,    26,    22,   956,   779,   423,
     541,    45,   134,    54,   134,   727,   114,    58,    52,   915,
     434,    99,   120,    84,    85,    86,   104,    88,     3,    31,
     946,    92,  1748,   426,     4,    28,    29,    30,  1544,  1512,
      10,    16,   825,    18,     9,  1536,   577,   578,   579,    31,
      88,   582,   583,   777,    92,    24,   134,   828,   589,   590,
      28,    29,    27,    28,    29,    30,   127,  1328,  1017,    88,
    1331,    76,   769,    92,  1536,  1005,  1173,    45,  1051,    91,
     684,  1797,  1237,   695,    52,    20,  1162,  1060,   619,  2077,
      16,    31,    18,   445,  1284,   447,   627,    16,   450,    18,
    1290,    32,    28,  1019,  1000,    31,    32,    33,    34,    35,
      86,   715,    28,  1268,     2,   981,   647,   648,  1273,    60,
      61,  1094,    10,    11,    11,    12,    13,    14,    15,   747,
      17,    94,    95,   980,  1089,   982,    99,    84,    85,    86,
    1197,   104,   673,  1859,   675,    43,    41,     9,    74,    44,
      60,    61,    41,  1049,   866,    44,    45,    11,    47,    48,
     691,    50,  2058,   126,  2059,    27,    28,    29,    30,    84,
      85,  1887,  1888,   855,    10,    11,   831,    45,   902,    47,
      48,    94,    50,   584,    97,   586,   430,    91,   114,   433,
       8,    20,    30,     7,   120,  1865,   946,  1294,   802,   803,
      31,    43,   877,   734,    28,  1704,   737,  1706,  1398,  1306,
    1926,    98,   743,    21,    66,     9,    78,    28,    31,   916,
    1150,   752,  1152,    86,    31,    31,    41,  1417,   953,    44,
      45,  1781,    47,    48,   765,    50,    20,   108,   935,  2005,
     105,    71,   773,     8,   106,   134,     3,    23,    55,    56,
     781,   782,    59,    24,  1170,    62,     4,    64,    51,   108,
     791,    68,    69,    70,  1130,    57,   797,  1397,    72,   800,
       4,   893,     3,   674,    94,  1238,   807,    79,    23,   107,
      75,  1471,    51,  1007,  1131,    72,  1343,   127,   681,  1219,
    1824,    29,   974,    22,    21,   115,   689,   107,   110,   692,
     122,   694,  1073,  1252,    23,   121,    83,  1497,  1405,   716,
      31,  1094,  1409,   714,   112,    43,    63,    78,  1508,   134,
      75,    88,   127,   130,    86,  1973,   857,    23,   859,   860,
     102,   106,  1103,   864,    10,    39,    88,    93,   869,   109,
      19,  1985,  1986,   102,    10,   109,     7,   878,    11,    12,
      13,    14,    15,     6,    17,    19,     6,    24,    83,   890,
      86,  1088,    30,    45,    52,  1295,  1232,    41,  1139,  1299,
      44,    45,  1096,    47,    48,  1324,    50,   423,    38,    38,
     988,   912,   341,   434,  1231,   427,  1233,    11,    12,    13,
      14,    15,  1653,    17,  1491,  2039,   980,  1587,  1125,   980,
    1845,  1356,  1845,  1558,  1353,  1801,  1400,  1586,  1741,  1292,
    1979,  1996,  1981,  1510,  1506,  1610,  1755,   948,  1608,  1910,
    1575,  1511,  1146,  1611,  1897,  1113,   825,  1582,   825,  1116,
     961,  1231,  1231,   964,  1789,    98,  1118,    11,    12,    13,
      14,    15,   973,    17,  1789,   976,  1937,  1159,  1910,  1345,
    1297,  1548,  1835,  1835,  1268,    41,   495,  1336,  1719,   990,
    1982,   270,  1440,  1560,  1339,  1617,  1695,  1763,  2018,  1705,
    1193,  1814,  1937,  1984,  2006,  1937,  1948,  1941,   940,  2056,
    1777,  1631,  1260,  1413,   664,  1623,  1443,  1443,  1451,  1443,
    1680,  1832,  1216,  1359,  1591,  1443,  1193,  1028,  1443,  1168,
    1262,  1598,   322,  1200,  1765,  1898,   326,  1623,  1605,  1326,
    1379,    35,   199,  1360,  1623,   442,   336,    35,  2040,  1258,
    1803,  1124,  1683,  1054,   938,   923,    41,  1293,   687,  1903,
      28,    29,    -1,    -1,    -1,    -1,   463,  1249,    36,    37,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,   368,    -1,
     951,  1514,  1418,   954,    52,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   966,  1756,  1279,    -1,    -1,
      -1,  1668,  1103,    -1,   988,    -1,   396,   397,    -1,    -1,
      -1,    -1,   402,    -1,   404,   405,    -1,  2078,  1685,  1120,
    1415,    -1,   412,  2009,  2010,  2011,  1010,  1128,    -1,    -1,
      -1,    -1,  1277,  1519,    -1,    -1,    -1,    -1,  1139,  1910,
    1285,  1910,  1910,    -1,  1145,  1570,  2078,    -1,    -1,    -1,
     118,    -1,    -1,    -1,  1155,  1352,  1542,  1354,  1355,   449,
      -1,    -1,    -1,   453,    -1,  1866,    -1,    -1,  1793,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1178,  1260,    -1,
      -1,  1624,    -1,  1750,    -1,    -1,  1481,  1384,    -1,    -1,
    1372,  1192,    -1,  1446,    -1,    -1,  1270,  1271,    36,    37,
      38,    39,    40,  1603,  1205,  1630,    -1,  1832,    46,  1210,
      -1,    49,   502,    -1,  1215,    -1,    -1,    -1,    -1,    -1,
      -1,  1403,   512,    -1,    -1,    -1,   516,  1613,  1980,    -1,
    1663,  1656,  1414,  1934,    -1,  1236,    -1,    -1,    -1,    -1,
      -1,   531,  1628,    81,  1811,  1116,    -1,    -1,    -1,   539,
      -1,    89,    -1,    -1,  1255,   223,    -1,    -1,    -1,  1453,
      -1,  1886,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1272,   110,    -1,  1275,    -1,    -1,   245,    -1,  1446,
     118,    -1,  1364,    -1,    -1,    -1,   124,  1706,  1707,    -1,
      -1,  1162,   130,  1367,    -1,  1920,  1715,    -1,    -1,    -1,
      -1,  1302,    -1,    -1,  1305,    -1,   596,  2078,    -1,  2078,
    2078,  1493,    -1,   603,    -1,  1509,  1317,    -1,  1443,    -1,
      -1,   718,    -1,   720,   721,    -1,    -1,   724,    -1,   726,
      -1,   728,  1483,    -1,    -1,  1517,  1337,    -1,    -1,    -1,
    1759,    -1,    -1,    -1,  1469,    -1,  1599,    -1,  1767,   639,
      -1,    -1,   642,    -1,  1787,  1218,   324,  1228,  1783,  1526,
      -1,    -1,    -1,  1616,  2095,  1446,    -1,    -1,    -1,  2029,
      -1,  1996,  1243,  1626,  1245,    -1,    -1,    -1,  1572,    -1,
    1766,    -1,   350,    -1,    -1,    -1,  1257,   677,    -1,    -1,
     358,    -1,   682,    -1,  1588,    -1,    -1,    -1,    -1,    -1,
    2025,    -1,    -1,  1644,    -1,   695,    -1,    -1,    -1,    -1,
     700,  1412,    -1,   381,    -1,  1848,    -1,    -1,  1419,  1420,
      -1,  1422,  1968,  1300,    -1,  1426,    -1,    -1,    -1,    -1,
     398,    -1,   400,  1434,  1853,   403,    -1,  1910,    -1,    -1,
      -1,    -1,  1636,    -1,    -1,    -1,    -1,    -1,   416,    -1,
    1816,    -1,  1852,  1454,  1790,    -1,    -1,  1652,    -1,   856,
      -1,   858,  1607,    -1,   432,    -1,    -1,  1468,    -1,    -1,
      -1,    -1,  1789,    -1,  1791,    -1,    -1,    -1,    -1,   876,
     448,  1914,  1901,   451,    -1,  1486,    -1,  2054,   885,    -1,
      -1,  1492,    -1,  1494,    -1,    -1,    -1,    -1,   466,    -1,
    1836,    -1,    -1,  1839,  1757,  1687,    -1,  1378,  1509,    -1,
    1763,    -1,    -1,    -1,    -1,  1516,  1728,   914,  1835,    -1,
    1837,  1666,    -1,  1840,    -1,    -1,   494,  1528,    -1,    -1,
      -1,    -1,  1965,    -1,    -1,   825,    -1,  1980,    -1,    -1,
    1876,    -1,    16,    -1,    -1,    -1,    -1,   346,    -1,  1550,
      -1,    -1,  1938,    -1,    -1,    -1,  1681,    -1,  1559,   527,
    1877,    -1,    -1,  1949,    -1,    -1,  1932,    -1,    -1,    -1,
      -1,    -1,    -1,    47,   971,  1576,  1577,  1578,    -1,    -1,
      54,    -1,  1583,  1584,    -1,   875,    -1,    -1,    -1,  1590,
      -1,   881,  1593,    -1,    -1,    -1,  1763,    -1,  2017,    -1,
    1725,  2001,    -1,  1474,    -1,  2078,   896,  1478,    -1,    -1,
    1487,    -1,    -1,  2038,  1615,  1616,  2035,  1618,    -1,    -1,
    2006,    -1,    -1,    -1,    -1,    -1,   425,   595,  1629,   597,
      -1,   599,    -1,    -1,   602,    -1,    -1,   436,    -1,    -1,
     439,    -1,    -1,    -1,    -1,  1042,    -1,  1814,    -1,    -1,
      -1,    -1,   620,  1735,    -1,    -1,    -1,  1910,    -1,    -1,
      -1,    -1,    -1,   137,   138,   139,   140,   141,   142,   143,
      -1,    -1,    -1,    -1,  1551,  2094,  1910,    -1,    -1,    -1,
    2105,   649,  1763,    -1,    -1,  1686,    -1,  1688,    -1,    -1,
      -1,  1562,   660,    -1,    -1,    -1,  1697,    -1,    -1,    -1,
      -1,    -1,   670,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   683,    -1,    -1,  1938,    -1,
      -1,   520,    -1,  1724,  1121,    -1,  1016,  1980,    -1,   683,
      28,    29,    30,    31,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1747,  1144,    46,   717,
    1751,   719,  1753,    -1,    -1,    -1,    -1,  1758,    -1,  1760,
    1834,    -1,    -1,   731,    -1,  1055,    -1,    -1,    -1,    -1,
     738,    -1,    -1,  2068,    -1,    -1,    74,   251,    -1,    77,
      78,    -1,    -1,  1784,    -1,  1786,  2006,    -1,   756,    -1,
      -1,   759,    -1,    -1,    -1,  1796,    -1,    -1,  2093,   767,
    1671,    -1,  1092,    -1,    -1,    -1,  2101,   281,    -1,    -1,
      -1,    -1,   110,    -1,  1815,  1907,   114,  1891,    -1,    -1,
      -1,   119,   120,    -1,   792,  2078,    -1,  1828,    -1,  1910,
     128,    -1,    -1,    -1,    -1,    -1,    -1,  2034,    -1,  2036,
    2037,    -1,    -1,    -1,  2078,    -1,    -1,   815,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1856,    -1,    -1,    -1,    -1,
    1737,    -1,  1863,    -1,    -1,    -1,    -1,    -1,    -1,  1870,
      -1,    -1,    -1,    -1,  1745,  1165,    -1,    -1,  1749,   353,
      -1,   355,    -1,  1957,    -1,   359,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1896,    -1,    -1,    -1,  1189,
    1190,  1902,    -1,  1904,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1913,    -1,    -1,    -1,    -1,  1991,    -1,   887,
      -1,    -1,    -1,    -1,  1998,    -1,    -1,    -1,    -1,  1930,
    1931,    -1,   900,    -1,  1935,    -1,    -1,  1938,    -1,  1940,
      -1,  1812,    -1,    -1,    -1,    -1,    -1,   915,  1949,    -1,
      -1,  2025,    -1,  1350,  1831,    -1,    -1,    -1,   926,   927,
    1961,    -1,    -1,   931,    -1,    -1,    -1,    -1,    -1,    -1,
    1260,    -1,   940,  1974,    -1,   152,    -1,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,    -1,
      -1,    -1,  1993,  2067,  1995,    -1,    -1,  2078,    -1,    -1,
      -1,  2002,   970,    -1,    -1,   479,    -1,  2081,    -1,    -1,
      -1,  1408,    -1,  2014,    -1,   983,   970,    -1,    -1,   818,
      -1,    -1,    -1,    -1,   992,    -1,  1423,    -1,  2102,    -1,
     829,    -1,  1000,  1001,  2108,    -1,    -1,   214,   837,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2047,    -1,    -1,    -1,
    2051,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1027,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2071,   870,    -1,    -1,  1364,    -1,   253,    -1,    -1,  1956,
    1477,  1049,    -1,  2084,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2092,    -1,    -1,    -1,    -1,    -1,  1968,    -1,  1496,
      -1,    -1,   279,  1071,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1989,  1081,    -1,    -1,    -1,    -1,  1515,    -1,
      -1,    -1,    -1,    -1,    -1,  1415,    -1,    -1,    -1,  2000,
      -1,  1099,  2003,    -1,  1102,  1425,    -1,  2008,    -1,    -1,
      -1,  2012,  1432,    -1,    -1,  1099,    -1,  2024,  1102,    -1,
      -1,    -1,   626,    -1,  1122,    -1,   955,   334,    -1,    -1,
      -1,  1129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,
    1138,    -1,   349,    -1,    -1,  1129,    -1,    -1,    -1,    -1,
      -1,   980,    -1,  1137,  1138,    -1,    -1,    -1,   987,    -1,
      -1,  1481,  1160,    -1,    -1,  2072,    -1,    -1,    -1,    -1,
      -1,    16,    -1,    18,    -1,    -1,  2083,    -1,    -1,  1177,
    1009,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    -1,    -1,  1194,    -1,    -1,    -1,
    1520,    46,    -1,    -1,  1631,    -1,  1204,    -1,   415,    -1,
      -1,    -1,    -1,  1211,    -1,    -1,    -1,    -1,    -1,  1646,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,   438,    77,    78,    -1,    -1,  1234,    -1,  1665,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1676,
      -1,    -1,  1250,    -1,    -1,    -1,    -1,   464,    -1,    -1,
     764,    -1,    -1,    -1,    -1,   110,  1250,    -1,    -1,   114,
      -1,    -1,  1101,    -1,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,  1280,   128,    -1,    -1,  1713,  1714,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   510,    -1,  1625,    -1,    -1,    -1,    -1,
    1308,   518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   529,    -1,    -1,   532,    -1,   534,    -1,    -1,
     537,   538,    -1,    -1,  1332,  1333,    -1,    -1,  1336,    -1,
      -1,  1339,    -1,  1341,    -1,    -1,    -1,  1345,   555,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1678,  1357,
    1358,    -1,    -1,    -1,    -1,    -1,    -1,  1365,    -1,    -1,
      -1,    -1,  1692,  1357,  1358,  1802,    -1,    -1,    -1,    -1,
    1807,  1365,  1809,    -1,    -1,    -1,    -1,   594,  1708,  1709,
    1388,    -1,    -1,  1391,    -1,    -1,  1394,   604,    -1,  1397,
      -1,    -1,  1231,  1401,    -1,    -1,  1833,  1391,    -1,   616,
    1394,    -1,    -1,    -1,    -1,  1735,    -1,    -1,    -1,    -1,
    1847,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   638,   936,    -1,    -1,    -1,    -1,    -1,   645,    -1,
      -1,    -1,  1440,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   658,  1450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   678,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1479,    -1,    -1,  1911,   989,    -1,   991,    -1,   696,
      -1,    -1,    -1,  1813,    -1,  1479,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1506,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1020,    -1,    -1,    -1,
    1518,    -1,    -1,   730,  1844,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   739,    -1,    -1,    -1,    -1,  1366,   745,    -1,
      -1,    -1,  1969,    -1,    -1,    -1,  1544,    -1,    -1,    -1,
     757,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1557,
      -1,  1881,    -1,  1392,    -1,    -1,    -1,    -1,    -1,    -1,
    1568,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   790,  1568,    -1,    -1,    -1,  1586,    -1,
    1910,    -1,    -1,    -1,    -1,  2022,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,    -1,
      -1,    -1,    -1,  1611,    -1,  2042,    -1,    -1,    -1,  1617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,
      -1,    -1,   839,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1977,    -1,    -1,
      -1,  1659,    -1,    -1,    -1,  1985,  1986,    -1,    -1,    -1,
    1174,    -1,    -1,    -1,    -1,   882,    -1,   884,    -1,    -1,
      -1,    -1,  1511,    -1,    -1,    -1,    -1,   894,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1695,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   924,    -1,  2039,
    1718,    -1,    -1,    -1,  1553,  1723,    -1,    -1,  1557,    -1,
     937,    -1,    -1,    -1,    -1,    -1,  1565,    -1,    -1,    -1,
      -1,    -1,    -1,  1741,    -1,    -1,    -1,    -1,  2068,    -1,
      -1,    -1,    -1,    -1,   961,   962,    -1,  1755,  2078,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2093,    -1,  1604,    -1,    -1,    -1,    -1,
      -1,  2101,    -1,  1781,    -1,    -1,   993,  1785,   995,    -1,
      -1,    -1,    -1,    -1,  1792,    -1,    -1,    -1,    -1,    -1,
    1007,  1785,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1022,  1645,    -1,  1816,    -1,
      -1,  1650,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1036,
      -1,  1829,    -1,    -1,    -1,  1664,    -1,    -1,    -1,    -1,
    1838,    -1,    -1,    -1,  1348,    -1,    -1,  1845,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1069,    -1,    -1,    -1,    -1,    -1,  1866,    -1,
      -1,  1869,    -1,    -1,    -1,  1873,    -1,  1084,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1729,    -1,    -1,    -1,    -1,  1903,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1744,    -1,  1421,    -1,    -1,
      -1,    -1,    -1,    -1,  1922,  1132,    -1,  1134,    -1,    -1,
      -1,    -1,    -1,    -1,  1932,    -1,  1934,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1777,    -1,
    1948,    -1,    -1,    -1,    -1,    -1,  1163,  1164,    -1,  1788,
    1789,    -1,  1960,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1973,    -1,    16,    -1,    18,
      -1,    -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,    28,
    1988,    -1,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      -1,  1208,    -1,    42,    -1,  1212,  1835,  2005,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1845,  1846,    -1,    -1,
    2018,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2026,    -1,
      -1,    -1,    -1,    -1,    -1,    74,  1243,    -1,  1245,    -1,
      -1,    80,    -1,  2041,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    90,  2050,    -1,    93,    -1,    -1,    96,  1265,    -1,
      -1,   100,   101,    -1,    -1,  2063,   105,    -1,    -1,  1276,
      -1,    -1,   111,    -1,    -1,   114,    -1,   116,   117,    -1,
      -1,   120,    -1,    -1,   123,    -1,   125,    -1,    -1,  1296,
      -1,    -1,   131,   132,    -1,    -1,  1303,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1955,    -1,    -1,    -1,
      -1,    -1,    -1,  1340,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1361,    -1,  1363,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1376,
      -1,    -1,    -1,    -1,    -1,  1382,    -1,    -1,    -1,    -1,
      -1,    -1,  1389,  1390,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1444,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1464,    -1,    -1,
    1467,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1484,  1485,    -1,
      -1,    -1,    -1,  1490,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1502,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1513,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1552,    -1,  1554,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1590,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1615,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1640,  1641,  1642,  1643,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1679,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1694,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1743,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,    -1,
      -1,    -1,  1779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1795,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1805,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1825,    -1,
      -1,    -1,    -1,    -1,    -1,  1832,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1841,    -1,  1843,    -1,    -1,    -1,
      -1,    -1,  1849,  1850,  1851,    -1,    -1,    -1,    -1,    -1,
      -1,  1858,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1867,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1875,    -1,
      -1,  1878,    -1,  1880,    -1,    -1,  1883,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1908,    -1,    -1,    -1,  1912,    -1,    -1,  1915,    -1,
    1917,    -1,    -1,    -1,    -1,    -1,  1923,  1924,  1925,    -1,
    1927,    -1,  1929,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1942,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1954,    -1,    -1,
     161,   162,    -1,    -1,    -1,    -1,    -1,  1964,    -1,  1966,
    1967,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    32,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1994,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2021,    -1,  2023,    -1,  2025,    -1,
    2027,  2028,    -1,    -1,  2031,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    -1,  2043,    -1,  2045,    -1,
      -1,  2048,  2049,    -1,    -1,  2052,    -1,    -1,   107,   108,
      -1,    -1,   111,   112,   113,   114,    -1,  2064,    -1,    -1,
      -1,    -1,  2069,   122,    -1,    -1,    -1,    -1,  2075,  2076,
      -1,    -1,  2079,    -1,  2081,    -1,    -1,    -1,    -1,    -1,
    2087,  2088,  2089,  2090,  2091,    -1,    -1,    -1,    -1,    -1,
    2097,    -1,  2099,    -1,    -1,  2102,    -1,    -1,    -1,  2106,
      -1,  2108,  2109,  2110,   163,  2112,  2113,  2114,  2115,    -1,
      -1,  2118,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   198,
      -1,    -1,   201,   202,   203,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   380,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   242,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   250,    -1,    -1,    -1,   406,    -1,    -1,   257,    -1,
      -1,    -1,    -1,   262,   263,   264,   265,   266,   267,   268,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     461,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   260,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   496,    -1,    -1,    -1,   500,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   361,    -1,    -1,    -1,    -1,    -1,   367,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   383,   384,   385,   386,   387,   388,
     389,   390,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,
      -1,   410,   411,    -1,   337,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     601,    -1,    -1,    -1,    -1,   454,    -1,    -1,    -1,   458,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   391,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   629,    -1,
      -1,    -1,   481,   482,    -1,    -1,    -1,   638,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   659,    -1,
      -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   464,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   490,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   498,    -1,   500,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   510,    -1,    -1,
     741,   514,    -1,    -1,   593,   746,   747,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     609,    -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,    -1,
      -1,    -1,    -1,    -1,   623,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   794,    -1,    -1,    -1,   798,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   813,   814,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   824,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   685,    -1,    -1,    -1,
      -1,    -1,    -1,   616,    -1,    -1,    -1,    -1,    -1,    -1,
     699,    -1,   701,   702,   703,   704,   705,   706,   707,   708,
     709,   710,   711,   712,   713,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     673,    -1,    -1,    -1,   753,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   761,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   771,    -1,    -1,   774,    -1,   928,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,   785,   786,    -1,    -1,
      -1,    -1,    -1,   944,    -1,    -1,    -1,   948,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   808,
     809,   810,   735,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   750,    -1,    -1,
      -1,    -1,    -1,    -1,   757,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   993,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     783,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     793,    -1,   795,    -1,   873,   874,    -1,    -1,   801,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   886,    -1,  1040,
    1041,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   898,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1061,  1062,    -1,    -1,   913,    -1,    -1,    -1,    -1,    -1,
      -1,  1072,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1080,
      -1,   930,    -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,
     939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1104,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1119,    -1,
      -1,   894,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   911,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     999,   924,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   932,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1026,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1188,    -1,    -1,
    1191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   972,
      -1,    -1,    -1,  1052,    -1,    -1,   979,    -1,    -1,    -1,
      -1,  1212,   985,   986,    -1,    -1,    -1,    -1,    -1,  1220,
      -1,    -1,    -1,  1224,    -1,  1226,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1014,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1098,
      -1,    -1,    -1,    -1,    -1,    -1,  1105,  1106,  1107,  1108,
      -1,    -1,    -1,  1036,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1050,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1136,    -1,    -1,
      -1,  1064,    -1,    -1,    -1,    -1,  1069,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1157,  1158,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1182,  1183,  1184,  1185,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1117,    -1,    -1,  1196,  1349,    -1,
      -1,    -1,    -1,    -1,  1203,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1222,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1167,    -1,    -1,    -1,  1247,  1248,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1261,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1201,  1202,
      -1,    -1,  1281,    -1,  1283,    -1,    -1,    -1,    -1,    -1,
    1213,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1227,  1456,    -1,  1458,  1459,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1480,
      -1,  1254,    -1,    -1,    -1,  1486,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1494,    -1,  1344,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1278,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1287,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1539,    -1,
      -1,  1542,    -1,    -1,  1393,    -1,    -1,    -1,  1549,    -1,
      -1,  1552,    -1,  1554,  1555,    -1,    -1,    -1,  1407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1567,  1340,    -1,    -1,
      -1,    -1,    -1,  1346,    -1,    -1,    -1,    -1,    -1,  1428,
    1429,  1430,  1431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1381,    -1,
      -1,    -1,    -1,    -1,  1387,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1627,  1476,    -1,    -1,
      -1,    -1,    -1,    -1,  1635,    -1,    -1,    -1,  1639,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1504,  1505,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1682,    -1,    -1,    -1,    -1,    -1,  1688,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1704,    -1,  1706,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1563,    -1,    -1,    -1,    -1,  1720,
      -1,    -1,    -1,    -1,    -1,  1726,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1509,  1738,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1765,  1538,  1767,    -1,    -1,    -1,
      -1,    -1,  1621,    -1,    -1,    -1,    -1,  1550,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1648,
      -1,    -1,    -1,  1804,    -1,    -1,    -1,    -1,  1657,  1658,
      -1,    -1,    -1,    -1,    -1,    -1,  1817,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1675,    -1,    -1,    -1,
      -1,    -1,    -1,  1606,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1633,  1634,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1874,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1890,
      -1,    -1,    -1,  1742,    -1,    -1,  1669,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1764,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1716,    -1,  1946,  1947,    -1,    -1,    -1,
      -1,    -1,    -1,  1954,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1975,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1987,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1779,    -1,  2009,    -1,
    2011,    -1,  2013,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1889,    -1,    -1,    -1,  2045,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1825,    -1,    -1,  2056,    -1,  2058,  2059,    -1,
      -1,  2062,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2075,  2076,  2077,    -1,    -1,    -1,
      -1,    -1,    -1,  1856,  2085,  2086,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1867,  2096,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1908,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1992,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1942,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2019
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
      20,   366,   387,   419,   653,   419,   652,   419,   419,   419,
     419,   666,   668,   666,   169,   170,   675,   475,   666,   667,
     666,   667,   666,   668,   156,   157,   675,   645,   661,   186,
     187,   252,   653,   422,   673,   422,   645,   674,   658,   655,
     395,   526,   664,   645,    35,   421,   485,   390,   657,   421,
     645,   645,   645,   645,   645,   645,   645,    30,   269,   330,
     439,   653,   269,   614,   616,   622,   628,   632,   638,   640,
     610,   655,   674,     9,    28,   387,   391,   433,   664,   664,
     664,   664,   664,   664,   664,   664,   664,   664,   666,   666,
     388,   420,   387,   664,   473,   367,   653,    73,   224,   225,
     259,   473,   489,   163,   164,   675,   655,   666,   473,   657,
     165,   166,   651,   165,   167,   168,   675,   389,   422,   651,
     676,   655,   653,    43,   447,   674,   433,   674,   154,   653,
     674,   658,   645,   652,   486,   664,   645,   473,   440,   664,
      21,   371,   413,   663,   331,   332,   653,   664,   645,   645,
     645,   645,   645,   645,   645,   645,   657,    66,   274,   317,
     635,   392,   434,    65,   315,   325,   633,   473,   378,   653,
     281,   325,   290,   291,   653,   325,   473,   290,   473,   473,
     326,   663,   652,   652,   645,   645,   473,   419,   664,   655,
     368,   369,   653,   490,   389,   664,   391,   164,   670,   671,
     651,   666,   170,   433,   663,    28,   172,   435,   166,   670,
     671,   653,   670,   168,   670,   671,   651,   665,   655,   651,
      86,   515,    42,    80,    87,    90,    96,   100,   101,   111,
     116,   117,   123,   131,   132,   188,   189,   190,   191,   192,
     193,   194,   199,   200,   201,   203,   207,   208,   209,   210,
     211,   212,   213,   216,   219,   220,   255,   330,   445,   503,
     517,   523,   535,   543,   545,   565,   575,   577,   589,   605,
     607,   652,   654,   448,   183,   154,   652,   154,   155,   160,
     653,   473,   154,   653,   378,   645,   421,   645,   421,   414,
     413,   663,   330,   655,   657,   105,   333,   334,   335,   553,
     653,   421,   421,   636,    71,   275,   321,   641,   674,   664,
     645,   645,   634,    29,   437,   664,    23,   417,   313,   663,
     657,   282,   437,    54,    58,   292,   293,   611,   619,   653,
     282,   433,   663,   657,   433,   657,   663,   325,   378,   159,
     160,   664,   419,   655,   657,    74,   114,   120,   232,   370,
     433,   473,   485,   491,   571,   583,   645,   657,   419,   473,
     671,   655,   651,   659,   676,   436,   175,   653,   671,   655,
     225,   228,   655,   671,   655,   651,   655,   655,   473,   516,
     664,   446,   504,   518,   524,   536,   544,   546,   566,   576,
     578,   590,   606,   608,   655,   663,   664,   664,   664,   658,
     664,   664,   256,   660,   658,   256,   658,   183,   664,   664,
     158,   160,    93,   125,   529,   593,   645,   655,   160,   377,
     378,   159,   161,   515,   653,   152,   159,   473,   655,   645,
     389,   108,   559,   554,   655,   657,   664,   336,   338,   653,
     645,   642,    57,   276,   322,   617,   674,   664,   318,   663,
     645,   438,   315,   315,   418,   387,   655,   663,   314,   378,
      60,   395,   623,   387,   314,   612,   620,   655,   664,   664,
     294,   296,   653,   387,   313,   387,     4,   381,   313,    79,
     501,   389,   655,   663,   653,   419,     3,   379,   492,   572,
     584,   365,   673,   347,   366,   657,   664,   238,   660,   664,
     225,   655,   666,   473,   227,   653,   422,   645,   417,   669,
     666,   664,   666,   666,   378,   655,   670,   656,   645,   473,
     645,   645,   645,   645,   645,   645,   645,   645,   645,   645,
     645,   645,   645,   660,   422,   381,   653,   330,   653,   330,
     330,   183,    51,   257,   471,   330,   183,   330,   661,   330,
     667,   655,   653,   530,   594,   664,   657,   664,   515,   655,
     663,   664,   655,   663,   152,   657,   560,   664,   645,    72,
     487,   334,   655,   657,   339,   341,   653,   645,   618,   674,
     664,   290,   663,   319,   325,   645,    22,   415,   664,   645,
     315,   389,   387,   624,   664,   664,   657,   415,   645,   645,
     437,   437,   655,   664,   297,   298,   653,   657,   417,   657,
     382,   664,   417,   502,   664,   657,   422,   422,   162,   380,
     664,   645,   645,   645,   439,   655,   663,   663,   226,   227,
     107,   557,   672,   651,   557,   656,   663,    77,    78,   119,
     128,   228,   229,   230,   231,   232,   233,   234,   237,   263,
     433,   437,   497,   499,   563,   581,   599,   652,   173,   387,
     176,   177,   178,   651,    26,   395,   427,   656,   671,   655,
     651,   655,   658,   330,   664,   330,   664,   664,   672,   472,
     664,   661,    88,    92,   519,   527,   664,   221,   222,   651,
     162,   645,   645,   473,   330,   419,   664,   663,   391,   473,
     655,   391,   655,   330,   645,   332,   488,   664,   337,   655,
     657,    16,    18,    33,    34,   232,   342,   344,   345,   346,
     405,   409,   433,   473,   479,   481,   483,   485,   645,   653,
     389,   320,   416,   122,   587,   657,   664,   645,   330,   290,
     325,   313,   313,    61,   295,   623,   625,   655,   657,   299,
     300,   653,   653,   663,   315,   645,   289,   290,   657,   645,
     311,   663,   325,   674,   655,   675,   645,   369,   659,   663,
      27,   348,   391,   429,   673,   417,   663,   664,   558,   660,
     515,   668,   660,   651,   417,   498,   500,   582,   600,   655,
     664,   655,   664,   660,   106,   115,   555,   573,   563,   227,
     330,   657,   664,   658,   664,   439,   112,   174,   567,   657,
     177,   178,   670,   671,   670,   653,   428,   657,   657,   651,
     157,   674,   664,   674,   653,   655,   663,   557,   655,   121,
     585,    83,   509,   515,   645,   290,   653,   520,   528,   214,
     662,   183,   509,   389,   655,   651,   675,   253,   664,   657,
     473,   417,   391,   674,   655,   645,   334,   664,   340,   379,
     405,   409,   406,   410,   482,   484,   655,   657,   347,   663,
     663,   330,   290,   657,   437,   645,   588,   664,   314,   315,
     437,   415,   415,   626,   657,   559,   655,   657,   301,   302,
     553,   653,   311,   387,    63,   629,   663,   289,   655,   254,
     652,   413,   430,   473,   433,   437,   439,   389,   557,   645,
     227,   239,   240,   241,   651,   227,   387,   664,   663,   645,
     645,   645,   645,   233,   237,   233,   234,   556,   574,   238,
     657,   658,   663,   664,   330,   366,   172,   233,   237,   581,
     652,   568,   664,   227,   671,   670,   655,   655,   179,   645,
     227,   227,   387,   664,    75,   493,   330,   664,   389,   195,
     662,   586,   183,   510,   183,   664,   127,   597,   645,   645,
     204,   661,   215,   653,   672,   256,   655,   655,   473,   254,
     657,   251,   447,   557,   391,   515,   674,   338,   664,   645,
     645,   645,   645,     5,    25,    82,   103,   343,   383,   425,
     507,   549,   344,   344,   663,   655,   664,   319,   289,   645,
     315,   325,   657,   657,   645,   293,   664,   487,   655,   664,
     303,   653,   655,   657,   630,   664,   389,   666,   183,   374,
     663,   645,   659,   663,   659,   659,   663,   657,   660,   240,
     241,   670,   671,   670,   653,   664,   656,   676,   655,   655,
     645,   645,   235,   661,   227,   172,   417,    10,   393,   657,
     664,   573,   377,   645,   439,   489,   655,   261,   656,   422,
     422,   664,   656,   494,   664,   655,   419,   657,   204,   661,
     196,   653,   645,   205,   645,   672,   419,   598,   657,   672,
       6,    84,   385,   511,   214,   654,   515,   183,   655,   670,
     253,   183,   381,   664,   655,   664,   515,   341,   384,   426,
     508,   550,   664,   417,   519,   415,   415,   293,   293,   298,
     664,   301,   655,   657,   304,   306,   653,   315,   645,   290,
     657,   674,   376,   663,   330,   375,   652,   417,   652,   652,
     413,   227,   227,   671,   670,   655,   655,   378,   227,   651,
     422,   236,   661,   102,   547,   655,   394,   660,   529,   657,
     655,   652,   657,   653,   655,   655,   227,   651,   645,   227,
     366,   672,   385,   195,   197,   198,   653,   204,   206,   661,
     515,   645,   330,   515,   386,   512,   664,   183,   204,   224,
     672,   671,   664,   664,   674,   664,   330,   217,   662,   473,
     664,   645,   645,   645,   645,   344,   664,   300,   316,   395,
     397,   399,   401,   403,   407,   655,   657,    52,   307,   309,
     310,   312,   325,   327,   433,   477,   479,   289,   676,   655,
     389,   664,   372,   663,   371,   374,   655,   655,   664,   395,
     672,   109,   561,   548,   242,   660,   645,   330,   660,   227,
     663,   223,   224,   113,   129,   180,   569,   601,   603,   655,
     655,   663,   664,   381,   655,   515,   664,   204,   663,   664,
     330,   672,    85,   513,   655,   645,   645,   215,   672,   657,
     664,   515,   222,   509,   655,   676,   330,   664,   204,   661,
     218,   653,   391,   473,   323,   324,   325,   664,   305,   405,
     409,   478,   655,   657,   663,   391,   663,   289,   328,   515,
     663,   657,   664,   381,   393,   655,   663,   373,   651,   655,
     376,   181,   182,   651,   653,   656,   664,   515,   562,   246,
     660,   645,   651,   663,   227,   413,   655,   570,   602,   656,
     664,   664,   664,   253,   656,   664,   664,   196,   672,   389,
      19,   411,   655,   664,   515,   514,   664,   515,   433,   411,
     183,   651,   515,   657,   419,   672,   385,   217,   226,   391,
     655,   664,   433,   664,   303,   664,   645,   308,   383,   507,
     549,    53,   429,   431,   437,   477,   433,   663,   391,   664,
     413,   375,   393,   657,   657,   663,   389,   349,   653,   663,
     655,   655,   655,   655,   387,   651,   330,   645,   651,   668,
     415,   655,   413,   645,   645,   653,   676,   651,   330,   419,
     515,   657,   412,   184,   660,   393,   645,   330,   473,   655,
     672,   395,   663,   587,   515,   664,   204,   664,   385,   473,
     411,   655,   306,   664,   432,   311,   328,   311,   417,   417,
     329,   473,   477,   473,   330,   330,   417,   657,   655,   350,
     351,   653,   415,   663,   653,   664,   395,   663,   667,   243,
     244,   245,   651,   378,    24,   424,   381,   663,   198,   645,
     655,   651,   660,   664,   253,   184,   515,   664,   655,   664,
     218,   672,   411,   433,   664,   417,   664,   309,   645,   415,
     417,   655,   373,   655,   657,   352,   353,   354,   553,   653,
     413,   664,   227,   664,   676,   247,   248,   249,   651,   244,
     245,   670,   671,   670,   653,   656,   655,   664,   253,   185,
     186,   330,   585,   663,   655,   330,   330,   515,   184,   473,
     323,   664,   290,   559,   655,   657,   664,   355,   356,   653,
     381,   424,   655,   656,   330,   422,   248,   249,   670,   671,
     670,   653,   671,   670,   655,   655,   473,   181,   182,   651,
     330,   676,   655,   655,   655,   183,   417,   202,   657,   664,
     655,   253,   411,   664,   487,   334,   655,   657,   357,   358,
     653,   664,   665,   653,   663,   655,   671,   670,   655,   655,
     473,   655,   656,   253,   253,   645,   663,   422,   664,   509,
     501,   417,   664,   332,   664,   337,   655,   657,   228,   230,
     231,   232,   359,   360,   361,   362,   405,   409,   433,   473,
     479,   481,   483,   485,   330,   655,   664,   676,   655,   655,
     656,   653,   655,   655,   655,   253,   655,   411,   655,   664,
     664,   290,   334,   664,   340,   363,   364,   391,   429,   664,
     673,   664,   365,   655,   657,   663,   663,   349,   653,   664,
     655,   663,   651,   381,   422,   653,   664,   655,   676,   655,
     655,   256,   330,   411,   338,   664,   663,   473,   233,   433,
     437,   439,   233,   347,   343,   344,   344,   663,   653,   381,
     422,   645,   664,   655,   664,   172,   656,   422,   184,   183,
     664,   184,   341,   348,   659,   663,   659,   659,   663,   664,
     417,   653,   657,   665,   655,   330,   655,   381,   422,   655,
     656,   653,   655,   655,   672,   655,   652,   417,   652,   652,
     413,   344,   347,   653,   330,   655,   655,   663,   664,   655,
     655,   653,   664,   655,   515,   372,   371,   374,   653,   655,
     422,   225,   655,   664,   381,   655,   655,   376,   655,   665,
     422,   655,   381,   664,   663,   663,   655,   655,   655,   655,
     655,   664,   225,   417,   415,   663,   655,   655,   225,   422,
     655,   413,   655,   422,   655,   655,   655,   655,   655,   655,
     655,   655
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
#define YYEMPTY		(-2)
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
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

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

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
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
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

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

  if (yyss + yystacksize - 1 <= yyssp)
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
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
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

      if (yyss + yystacksize - 1 <= yyssp)
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

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
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
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

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


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 230 "../Parse.yacc"
    { depth=0; }
    break;

  case 4:
#line 231 "../Parse.yacc"
    { depth=0; }
    break;

  case 5:
#line 231 "../Parse.yacc"
    { Flush(); }
    break;

  case 6:
#line 232 "../Parse.yacc"
    { depth=0; }
    break;

  case 7:
#line 232 "../Parse.yacc"
    { Flush(); }
    break;

  case 536:
#line 1343 "../Parse.yacc"
    { PR ("&");}
    break;

  case 538:
#line 1344 "../Parse.yacc"
    { PR (":=");}
    break;

  case 540:
#line 1345 "../Parse.yacc"
    { PR ("*");}
    break;

  case 542:
#line 1346 "../Parse.yacc"
    { PR ("|");}
    break;

  case 544:
#line 1347 "../Parse.yacc"
    { PR (":");}
    break;

  case 546:
#line 1348 "../Parse.yacc"
    { PR (",");}
    break;

  case 548:
#line 1349 "../Parse.yacc"
    { PR (".");}
    break;

  case 550:
#line 1350 "../Parse.yacc"
    { PR ("..");}
    break;

  case 552:
#line 1351 "../Parse.yacc"
    { PR ("=");}
    break;

  case 554:
#line 1352 "../Parse.yacc"
    { PR (">");}
    break;

  case 556:
#line 1353 "../Parse.yacc"
    { PR (">=");}
    break;

  case 558:
#line 1354 "../Parse.yacc"
    { PR ("<");}
    break;

  case 560:
#line 1355 "../Parse.yacc"
    { PR ("<=");}
    break;

  case 562:
#line 1356 "../Parse.yacc"
    { PR ("-");}
    break;

  case 564:
#line 1357 "../Parse.yacc"
    { PR ("\043");}
    break;

  case 566:
#line 1358 "../Parse.yacc"
    { PR ("+");}
    break;

  case 568:
#line 1359 "../Parse.yacc"
    { PR ("=>");}
    break;

  case 570:
#line 1360 "../Parse.yacc"
    { PR ("}");}
    break;

  case 572:
#line 1361 "../Parse.yacc"
    { PR ("]");}
    break;

  case 574:
#line 1362 "../Parse.yacc"
    { PR (")");}
    break;

  case 576:
#line 1363 "../Parse.yacc"
    { PR ("*>");}
    break;

  case 578:
#line 1364 "../Parse.yacc"
    { PR ("*>");}
    break;

  case 579:
#line 1365 "../Parse.yacc"
    { PR (";");}
    break;

  case 581:
#line 1366 "../Parse.yacc"
    { PR (";");}
    break;

  case 582:
#line 1367 "../Parse.yacc"
    { PR ("/");}
    break;

  case 584:
#line 1368 "../Parse.yacc"
    { PR ("<:");}
    break;

  case 586:
#line 1369 "../Parse.yacc"
    { PR ("^");}
    break;

  case 588:
#line 1370 "../Parse.yacc"
    { PR ("^'");}
    break;

  case 590:
#line 1373 "../Parse.yacc"
    { PR ("("); }
    break;

  case 592:
#line 1374 "../Parse.yacc"
    { PR ("("); }
    break;

  case 594:
#line 1375 "../Parse.yacc"
    { PR ("["); }
    break;

  case 596:
#line 1376 "../Parse.yacc"
    { PR ("{"); }
    break;

  case 598:
#line 1382 "../Parse.yacc"
    { PF ("<* EXTERNAL", fonts->fixedComment);}
    break;

  case 600:
#line 1383 "../Parse.yacc"
    { PF ("<* INLINE",   fonts->fixedComment);}
    break;

  case 602:
#line 1384 "../Parse.yacc"
    { PF ("<* ASSERT",   fonts->fixedComment);}
    break;

  case 604:
#line 1385 "../Parse.yacc"
    { PF ("<* TRACE",    fonts->fixedComment);}
    break;

  case 606:
#line 1386 "../Parse.yacc"
    { PF ("<* FATAL",    fonts->fixedComment);}
    break;

  case 608:
#line 1387 "../Parse.yacc"
    { PF ("<* UNUSED",   fonts->fixedComment);}
    break;

  case 610:
#line 1388 "../Parse.yacc"
    { PF ("<* OBSOLETE", fonts->fixedComment);}
    break;

  case 612:
#line 1389 "../Parse.yacc"
    { PF ("<* CALLBACK", fonts->fixedComment);}
    break;

  case 614:
#line 1390 "../Parse.yacc"
    { PF ("<* EXPORTED", fonts->fixedComment);}
    break;

  case 616:
#line 1392 "../Parse.yacc"
    { PF ("<* PRAGMA",   fonts->fixedComment);}
    break;

  case 618:
#line 1393 "../Parse.yacc"
    { PF ("<* NOWARN",   fonts->fixedComment);}
    break;

  case 620:
#line 1394 "../Parse.yacc"
    { PF ("<* LINE",     fonts->fixedComment);}
    break;

  case 622:
#line 1395 "../Parse.yacc"
    { PF ("<* LL",       fonts->fixedComment);}
    break;

  case 624:
#line 1396 "../Parse.yacc"
    { PF ("<* LL.sup",   fonts->fixedComment);}
    break;

  case 626:
#line 1397 "../Parse.yacc"
    { PF ("<* SPEC",     fonts->fixedComment);}
    break;

  case 628:
#line 1398 "../Parse.yacc"
    { PF ("<* LOOPINV",  fonts->fixedComment);}
    break;

  case 630:
#line 1400 "../Parse.yacc"
    { PRID (&lexbuf[yyvsp[0]]);}
    break;

  case 632:
#line 1401 "../Parse.yacc"
    { PF (&lexbuf[yyvsp[0]], fonts->procName);}
    break;

  case 634:
#line 1402 "../Parse.yacc"
    { PRID (&lexbuf[yyvsp[0]]);}
    break;

  case 636:
#line 1403 "../Parse.yacc"
    { PR (&lexbuf[yyvsp[0]]);}
    break;

  case 638:
#line 1404 "../Parse.yacc"
    { PR (&lexbuf[yyvsp[0]]);}
    break;

  case 640:
#line 1405 "../Parse.yacc"
    { PF (&lexbuf[yyvsp[0]], fonts->fixed);}
    break;

  case 642:
#line 1406 "../Parse.yacc"
    { PF (&lexbuf[yyvsp[0]], fonts->fixed);}
    break;

  case 644:
#line 1408 "../Parse.yacc"
    { PK ("AND");}
    break;

  case 646:
#line 1409 "../Parse.yacc"
    { PK ("ANY");}
    break;

  case 648:
#line 1410 "../Parse.yacc"
    { PK ("ARRAY");}
    break;

  case 650:
#line 1411 "../Parse.yacc"
    { PK ("AS");}
    break;

  case 652:
#line 1412 "../Parse.yacc"
    { PK ("BEGIN");}
    break;

  case 654:
#line 1413 "../Parse.yacc"
    { PK ("BITS");}
    break;

  case 656:
#line 1414 "../Parse.yacc"
    { PK ("BRANDED");}
    break;

  case 658:
#line 1415 "../Parse.yacc"
    { PK ("BY");}
    break;

  case 660:
#line 1416 "../Parse.yacc"
    { PK ("CASE");}
    break;

  case 662:
#line 1417 "../Parse.yacc"
    { PK ("CONST");}
    break;

  case 664:
#line 1418 "../Parse.yacc"
    { PR ("DIV");}
    break;

  case 666:
#line 1419 "../Parse.yacc"
    { PK ("DO");}
    break;

  case 668:
#line 1420 "../Parse.yacc"
    { PK ("ELSE");}
    break;

  case 670:
#line 1421 "../Parse.yacc"
    { PK ("ELSIF");}
    break;

  case 672:
#line 1422 "../Parse.yacc"
    { PK ("END");}
    break;

  case 674:
#line 1423 "../Parse.yacc"
    { PK ("EVAL");}
    break;

  case 676:
#line 1424 "../Parse.yacc"
    { PK ("EXCEPT");}
    break;

  case 678:
#line 1425 "../Parse.yacc"
    { PK ("EXCEPTION");}
    break;

  case 680:
#line 1426 "../Parse.yacc"
    { PK ("EXIT");}
    break;

  case 682:
#line 1427 "../Parse.yacc"
    { PK ("EXPORTS");}
    break;

  case 684:
#line 1428 "../Parse.yacc"
    { PK ("FINALLY");}
    break;

  case 686:
#line 1429 "../Parse.yacc"
    { PK ("FOR");}
    break;

  case 688:
#line 1430 "../Parse.yacc"
    { PK ("FROM");}
    break;

  case 690:
#line 1431 "../Parse.yacc"
    { PK ("GENERIC");}
    break;

  case 692:
#line 1432 "../Parse.yacc"
    { PK ("IF");}
    break;

  case 694:
#line 1433 "../Parse.yacc"
    { PK ("IMPORT");}
    break;

  case 696:
#line 1434 "../Parse.yacc"
    { PK ("IN");}
    break;

  case 698:
#line 1435 "../Parse.yacc"
    { PK ("INTERFACE");}
    break;

  case 700:
#line 1436 "../Parse.yacc"
    { PK ("LOCK");}
    break;

  case 702:
#line 1437 "../Parse.yacc"
    { PK ("LOOP");}
    break;

  case 704:
#line 1438 "../Parse.yacc"
    { PK ("METHODS");}
    break;

  case 706:
#line 1439 "../Parse.yacc"
    { PK ("MOD");}
    break;

  case 708:
#line 1440 "../Parse.yacc"
    { PK ("MODULE");}
    break;

  case 710:
#line 1441 "../Parse.yacc"
    { PK ("NOT");}
    break;

  case 712:
#line 1442 "../Parse.yacc"
    { PK ("OBJECT");}
    break;

  case 714:
#line 1443 "../Parse.yacc"
    { PK ("OF");}
    break;

  case 716:
#line 1444 "../Parse.yacc"
    { PK ("OR");}
    break;

  case 718:
#line 1445 "../Parse.yacc"
    { PK ("OVERRIDES");}
    break;

  case 720:
#line 1446 "../Parse.yacc"
    { PK ("PROCEDURE");}
    break;

  case 722:
#line 1447 "../Parse.yacc"
    { PK ("RAISE");}
    break;

  case 724:
#line 1450 "../Parse.yacc"
    { DoBreak(1, 2, 0.0); PK ("RAISES");}
    break;

  case 726:
#line 1451 "../Parse.yacc"
    { PK ("READONLY");}
    break;

  case 728:
#line 1452 "../Parse.yacc"
    { PK ("RECORD");}
    break;

  case 730:
#line 1453 "../Parse.yacc"
    { PK ("REF");}
    break;

  case 732:
#line 1454 "../Parse.yacc"
    { PK ("REPEAT");}
    break;

  case 734:
#line 1455 "../Parse.yacc"
    { PK ("RETURN");}
    break;

  case 736:
#line 1456 "../Parse.yacc"
    { PK ("REVEAL");}
    break;

  case 738:
#line 1457 "../Parse.yacc"
    { PK ("ROOT");}
    break;

  case 740:
#line 1458 "../Parse.yacc"
    { PK ("SET");}
    break;

  case 742:
#line 1459 "../Parse.yacc"
    { PK ("THEN");}
    break;

  case 744:
#line 1460 "../Parse.yacc"
    { PK ("TO");}
    break;

  case 746:
#line 1461 "../Parse.yacc"
    { PK ("TRY");}
    break;

  case 748:
#line 1462 "../Parse.yacc"
    { PK ("TYPE");}
    break;

  case 750:
#line 1463 "../Parse.yacc"
    { PK ("TYPECASE");}
    break;

  case 752:
#line 1464 "../Parse.yacc"
    { PK ("UNSAFE");}
    break;

  case 754:
#line 1465 "../Parse.yacc"
    { PK ("UNTIL");}
    break;

  case 756:
#line 1466 "../Parse.yacc"
    { PK ("UNTRACED");}
    break;

  case 758:
#line 1467 "../Parse.yacc"
    { PK ("VALUE");}
    break;

  case 760:
#line 1468 "../Parse.yacc"
    { PK ("VAR");}
    break;

  case 762:
#line 1469 "../Parse.yacc"
    { PK ("WHILE");}
    break;

  case 764:
#line 1470 "../Parse.yacc"
    { PK ("WITH");}
    break;

  case 766:
#line 1473 "../Parse.yacc"
    { PK ("ABSTRACT");}
    break;

  case 768:
#line 1474 "../Parse.yacc"
    { PK ("ALL");}
    break;

  case 770:
#line 1475 "../Parse.yacc"
    { PK ("AXIOM");}
    break;

  case 772:
#line 1476 "../Parse.yacc"
    { PK ("DEPEND");}
    break;

  case 774:
#line 1477 "../Parse.yacc"
    { PK ("ENSURES");}
    break;

  case 776:
#line 1478 "../Parse.yacc"
    { PK ("EXISTS");}
    break;

  case 778:
#line 1479 "../Parse.yacc"
    { PK ("FUNC");}
    break;

  case 780:
#line 1480 "../Parse.yacc"
    { PK ("IFF");}
    break;

  case 782:
#line 1481 "../Parse.yacc"
    { PK ("IMPLIES");}
    break;

  case 784:
#line 1482 "../Parse.yacc"
    { PK ("INVARIANT");}
    break;

  case 786:
#line 1483 "../Parse.yacc"
    { PK ("IS");}
    break;

  case 788:
#line 1484 "../Parse.yacc"
    { PK ("LET");}
    break;

  case 790:
#line 1485 "../Parse.yacc"
    { PK ("MAP");}
    break;

  case 792:
#line 1486 "../Parse.yacc"
    { PK ("MODIFIES");}
    break;

  case 794:
#line 1487 "../Parse.yacc"
    { PK ("PRED");}
    break;

  case 796:
#line 1488 "../Parse.yacc"
    { PK ("PROTECT");}
    break;

  case 798:
#line 1489 "../Parse.yacc"
    { PK ("REQUIRES");}
    break;

  case 800:
#line 1507 "../Parse.yacc"
    { blanklinep = 0; PrintNPS(1); }
    break;

  case 801:
#line 1508 "../Parse.yacc"
    { blanklinep = 0; PrintNPS(1); }
    break;

  case 803:
#line 1512 "../Parse.yacc"
    { blanklinep = 0; }
    break;

  case 812:
#line 1536 "../Parse.yacc"
    { blanklinep = 0; PrintNPS(0); }
    break;

  case 815:
#line 1547 "../Parse.yacc"
    { GR (); }
    break;

  case 816:
#line 1548 "../Parse.yacc"
    { BE (0.0); }
    break;

  case 817:
#line 1549 "../Parse.yacc"
    { BE (offset); }
    break;

  case 818:
#line 1550 "../Parse.yacc"
    { BE (offset*2); }
    break;

  case 819:
#line 1551 "../Parse.yacc"
    { EN (); }
    break;

  case 820:
#line 1552 "../Parse.yacc"
    { ENF (); }
    break;

  case 821:
#line 1554 "../Parse.yacc"
    { DoBreak (1, 2, 0.0); }
    break;

  case 822:
#line 1555 "../Parse.yacc"
    { DoBreak (1, 3, 0.0); }
    break;

  case 823:
#line 1556 "../Parse.yacc"
    { DoBreak (0, 3, 0.0); }
    break;

  case 824:
#line 1558 "../Parse.yacc"
    { DoBreak (1, 1, 0.0); }
    break;

  case 825:
#line 1559 "../Parse.yacc"
    { DoBreak (1, 1, -offset); }
    break;

  case 826:
#line 1560 "../Parse.yacc"
    { DoBreak (1, 1, -offset + 2.0 * bodySpaceWidth); }
    break;

  case 827:
#line 1562 "../Parse.yacc"
    { DoBreak (0, 0, 0.0); }
    break;

  case 828:
#line 1563 "../Parse.yacc"
    { DoBreak (1, 0, 0.0); }
    break;

  case 829:
#line 1564 "../Parse.yacc"
    { P2 (' '); }
    break;

  case 830:
#line 1566 "../Parse.yacc"
    { BL (); }
    break;

  case 831:
#line 1568 "../Parse.yacc"
    { DoAlign (2, 0); }
    break;

  case 832:
#line 1569 "../Parse.yacc"
    { DoAlign (3, 0); }
    break;

  case 833:
#line 1570 "../Parse.yacc"
    { DoAlign (5, 1); }
    break;

  case 834:
#line 1571 "../Parse.yacc"
    { EndAlign (); }
    break;

  case 835:
#line 1573 "../Parse.yacc"
    { ALNL(); }
    break;

  case 836:
#line 1575 "../Parse.yacc"
    { DoSPNL (); }
    break;

  case 837:
#line 1576 "../Parse.yacc"
    { DoQSP (); }
    break;

  case 838:
#line 1577 "../Parse.yacc"
    { NL (); }
    break;

  case 839:
#line 1579 "../Parse.yacc"
    { depth++; }
    break;

  case 840:
#line 1580 "../Parse.yacc"
    { depth--; }
    break;


    }

/* Line 991 of yacc.c.  */
#line 4854 "y.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

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
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
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
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



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
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__)
  __attribute__ ((__unused__))
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
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

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
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


#line 1582 "../Parse.yacc"


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


