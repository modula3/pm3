#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# ifndef YYLMAX 
# define YYLMAX BUFSIZ
# endif 
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	int yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng;
#define YYISARRAY
char yytext[YYLMAX];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

# line 3 "../Parse.lex"
/* Copyright (C) 1989, Digital Equipment Corporation               */

# line 4 "../Parse.lex"
/* All rights reserved.                                            */

# line 5 "../Parse.lex"
/* See the file COPYRIGHT for a full description.                  */


# line 7 "../Parse.lex"
/* Last modified on Wed Aug 10 13:21:55 PDT 1994 by kalsow         */

# line 8 "../Parse.lex"
/*      modified on Thu Apr 23 18:08:06 PDT 1992 by muller         */

# line 9 "../Parse.lex"
/*      modified on Mon Apr 20 15:59:06 1992 by nichols@xerox.com  */

# line 10 "../Parse.lex"
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan         */


# line 12 "../Parse.lex"
/*
  We distinguish between known and unknown pragmas.
  Known pragmas can appear at special positions only
  and must fulfill a certain syntax.
  They are part of the grammar and are processed in Parse.yacc.
  Unknown pragmas can appear anywhere and are treated like comments.
  They are processed by the lexer and are identified as WHITESPACE
  for the grammar.
  Some pragmas like NOWARN are known but are handled like unknown
  since they can appear anywhere.
*/
# define Prog 2
# define Com 4
# define Prag 6
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 28 "../Parse.lex"
     {BufferLexeme(1); return(PR_EXTERNAL);}
break;
case 2:

# line 29 "../Parse.lex"
       {BufferLexeme(1); return(PR_INLINE);}
break;
case 3:

# line 30 "../Parse.lex"
       {BufferLexeme(1); return(PR_ASSERT);}
break;
case 4:

# line 31 "../Parse.lex"
        {BufferLexeme(1); return(PR_TRACE);}
break;
case 5:

# line 32 "../Parse.lex"
        {BufferLexeme(1); return(PR_FATAL);}
break;
case 6:

# line 33 "../Parse.lex"
       {BufferLexeme(1); return(PR_UNUSED);}
break;
case 7:

# line 34 "../Parse.lex"
     {BufferLexeme(1); return(PR_OBSOLETE);}
break;
case 8:

# line 35 "../Parse.lex"
     {BufferLexeme(1); return(PR_CALLBACK);}
break;
case 9:

# line 36 "../Parse.lex"
     {BufferLexeme(1); return(PR_EXPORTED);}
break;
case 10:

# line 38 "../Parse.lex"
       {BufferLexeme(1); return(PR_PRAGMA);}
break;
case 11:

# line 39 "../Parse.lex"
       {BufferLexeme(1); return(PR_NOWARN);}
break;
case 12:

# line 40 "../Parse.lex"
         {BufferLexeme(1); return(PR_LINE);}
break;
case 13:

# line 41 "../Parse.lex"
           {BufferLexeme(1); return(PR_LL);}
break;
case 14:

# line 42 "../Parse.lex"
       {BufferLexeme(1); return(PR_LLsup);}
break;
case 15:

# line 43 "../Parse.lex"
         {BufferLexeme(1); return(PR_SPEC);}
break;
case 16:

# line 44 "../Parse.lex"
      {BufferLexeme(1); return(PR_LOOPINV);}
break;
case 17:

# line 46 "../Parse.lex"
	{BufferLexeme(1); return(RPRAGMA);}
break;
case 18:

# line 48 "../Parse.lex"
{ return (HandleSpaces()); }
break;

# line 51 "../Parse.lex"
/*
  We match "<"/"*" (look-ahead for "*") instead of "<*"
  since HandleCommentPragma() contains a loop
  that has to process nested comments
  and since it must process the inner comments char-by-char
  we do so at the top level as well.
*/
case 19:

# line 58 "../Parse.lex"
	{ return (HandleCommentPragma()); }
break;
case 20:

# line 59 "../Parse.lex"
	{ return (HandleCommentPragma()); }
break;
case 21:

# line 62 "../Parse.lex"
	{BufferLexeme(1); return(PLUS);}
break;
case 22:

# line 63 "../Parse.lex"
	{BufferLexeme(1); return(MINUS);}
break;
case 23:

# line 64 "../Parse.lex"
	{BufferLexeme(1); return(ASTERISK);}
break;
case 24:

# line 65 "../Parse.lex"
	{BufferLexeme(1); return(SLASH);}
break;
case 25:

# line 66 "../Parse.lex"
	{BufferLexeme(1); return(ASSIGN);}
break;
case 26:

# line 67 "../Parse.lex"
	{BufferLexeme(1); return(AMPERSAND);}
break;
case 27:

# line 68 "../Parse.lex"
	{BufferLexeme(1); return(DOT);}
break;
case 28:

# line 69 "../Parse.lex"
	{BufferLexeme(1); return(COMMA);}
break;
case 29:

# line 70 "../Parse.lex"
	{BufferLexeme(1); return(SEMICOLON);}
break;
case 30:

# line 71 "../Parse.lex"
	{BufferLexeme(1); return(LPAREN);}
break;
case 31:

# line 72 "../Parse.lex"
	{BufferLexeme(1); return(LBRACKET);}
break;
case 32:

# line 73 "../Parse.lex"
	{BufferLexeme(1); return(LBRACE);}
break;
case 33:

# line 74 "../Parse.lex"
	{BufferLexeme(1); return(UPARROW);}
break;
case 34:

# line 75 "../Parse.lex"
	{BufferLexeme(1); return(UPARROWPRIME);}
break;
case 35:

# line 76 "../Parse.lex"
	{BufferLexeme(1); return(EQUAL);}
break;
case 36:

# line 77 "../Parse.lex"
	{BufferLexeme(1); return(RARROW);}
break;
case 37:

# line 78 "../Parse.lex"
	{BufferLexeme(1); return(SHARP);}
break;
case 38:

# line 79 "../Parse.lex"
	{BufferLexeme(1); return(LESS);}
break;
case 39:

# line 80 "../Parse.lex"
	{BufferLexeme(1); return(GREATER);}
break;
case 40:

# line 81 "../Parse.lex"
	{BufferLexeme(1); return(LSEQUAL);}
break;
case 41:

# line 82 "../Parse.lex"
	{BufferLexeme(1); return(SUBTYPE);}
break;
case 42:

# line 83 "../Parse.lex"
	{BufferLexeme(1); return(GREQUAL);}
break;
case 43:

# line 84 "../Parse.lex"
	{BufferLexeme(1); return(DOTDOT);}
break;
case 44:

# line 85 "../Parse.lex"
	{BufferLexeme(1); return(COLON);}
break;
case 45:

# line 86 "../Parse.lex"
	{BufferLexeme(1); return(RPAREN);}
break;
case 46:

# line 87 "../Parse.lex"
	{BufferLexeme(1); return(RBRACKET);}
break;
case 47:

# line 88 "../Parse.lex"
	{BufferLexeme(1); return(RBRACE);}
break;
case 48:

# line 89 "../Parse.lex"
	{BufferLexeme(1); return(BAR);}
break;
case 49:

# line 91 "../Parse.lex"
       {BufferLexeme(1); return(CARD_CONST);}
break;
case 50:

# line 93 "../Parse.lex"
{BufferLexeme(1); return(REAL_CONST);}
break;
case 51:

# line 95 "../Parse.lex"
{
				 BufferLexeme(1); return(STR_CONST);}
break;
case 52:

# line 98 "../Parse.lex"
{
				 BufferLexeme(1); return(STR_CONST);}
break;
case 53:

# line 101 "../Parse.lex"
{
				 BufferLexeme(1); return(STR_CONST);}
break;

# line 106 "../Parse.lex"
/* Primed identifiers as used in SPEC pragams are difficult to handle
   because primes introduce character literals in Modula 3 program text.
   A primed identifier like W' cannot be handled. */
case 54:

# line 109 "../Parse.lex"
{BufferLexeme(1); return(IDENTPRIME);}
break;
case 55:

# line 111 "../Parse.lex"
{PTRKEYWORDENTRY tempp;
				 if ((tempp=lookup(yytext))!=NULL){
				        CapBufferLexeme(1);
					return(tempp->lexval);}
				 else {BufferLexeme(1); return(IDENT);}}
break;
case 56:

# line 117 "../Parse.lex"
{
	BufferLexeme(0);
	/* Due to bison bug, we return 0 explicitly instead of ENDOFFILE. */
	return(0);
	}
break;
case 57:

# line 122 "../Parse.lex"
{BufferLexeme(0);return(MODUNIT);}
break;
case 58:

# line 123 "../Parse.lex"
{BufferLexeme(0);return(DEFUNIT);}
break;
case 59:

# line 124 "../Parse.lex"
	{BufferLexeme(1); return(BAD);}
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

56,
59,
0, 

57,
59,
0, 

59,
0, 

58,
59,
0, 

18,
59,
0, 

18,
0, 

18,
59,
0, 

59,
0, 

37,
59,
0, 

26,
59,
0, 

59,
0, 

30,
59,
-20,
0, 

45,
59,
0, 

23,
59,
0, 

21,
59,
0, 

28,
59,
0, 

22,
59,
0, 

27,
59,
0, 

24,
59,
0, 

49,
59,
0, 

44,
59,
0, 

29,
59,
0, 

38,
59,
-19,
0, 

35,
59,
0, 

39,
59,
0, 

55,
59,
0, 

55,
59,
0, 

31,
59,
0, 

46,
59,
0, 

33,
59,
0, 

32,
59,
0, 

48,
59,
0, 

47,
59,
0, 

57,
0, 

58,
0, 

51,
0, 

20,
0, 

17,
0, 

43,
0, 

-50,
0, 

49,
0, 

25,
0, 

19,
0, 

41,
0, 

40,
0, 

36,
0, 

42,
0, 

54,
0, 

55,
0, 

54,
0, 

34,
0, 

52,
0, 

50,
0, 

50,
-50,
0, 

50,
0, 

49,
0, 

-50,
0, 

13,
0, 

53,
0, 

50,
-50,
0, 

12,
0, 

15,
0, 

5,
0, 

4,
0, 

3,
0, 

2,
0, 

14,
0, 

11,
0, 

10,
0, 

6,
0, 

16,
0, 

8,
0, 

9,
0, 

1,
0, 

7,
0, 
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,9,	1,10,	
1,11,	45,0,	1,12,	0,0,	
0,0,	0,0,	1,13,	1,14,	
0,0,	0,0,	45,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
14,42,	0,0,	0,0,	14,43,	
0,0,	0,0,	0,0,	0,0,	
14,44,	0,0,	0,0,	0,0,	
0,0,	1,15,	0,0,	1,16,	
1,17,	0,0,	0,0,	1,18,	
1,19,	1,20,	1,21,	1,22,	
1,23,	1,24,	1,25,	1,26,	
1,27,	1,28,	14,44,	20,50,	
26,52,	35,64,	31,57,	38,65,	
48,67,	1,28,	71,0,	1,29,	
1,30,	1,31,	1,32,	1,33,	
22,51,	29,56,	1,34,	32,60,	
33,61,	1,34,	31,58,	76,95,	
1,34,	31,59,	78,97,	2,17,	
82,103,	87,108,	2,18,	79,98,	
2,20,	2,21,	2,22,	81,102,	
2,24,	2,25,	75,94,	2,27,	
1,35,	1,34,	77,96,	83,104,	
1,36,	1,11,	1,37,	1,38,	
1,11,	66,89,	2,29,	2,30,	
2,31,	2,32,	2,33,	28,53,	
68,90,	28,54,	28,54,	28,54,	
28,54,	28,54,	28,54,	28,54,	
28,54,	28,54,	28,54,	80,99,	
84,105,	85,106,	80,100,	86,107,	
91,113,	80,101,	94,115,	95,116,	
1,39,	1,40,	1,41,	2,35,	
97,119,	98,120,	99,121,	2,36,	
3,17,	2,37,	2,38,	3,18,	
96,117,	3,20,	3,21,	3,22,	
96,118,	3,24,	3,25,	100,122,	
3,27,	101,123,	102,124,	103,125,	
104,126,	105,127,	106,128,	107,129,	
28,55,	47,0,	109,130,	3,29,	
3,30,	3,31,	3,32,	3,33,	
111,132,	112,133,	47,45,	2,39,	
2,40,	2,41,	113,134,	115,135,	
116,136,	117,137,	118,138,	4,17,	
119,139,	120,140,	4,18,	121,141,	
4,20,	4,21,	4,22,	122,142,	
4,24,	4,25,	123,143,	4,27,	
3,35,	124,144,	47,45,	125,145,	
3,36,	126,146,	3,37,	3,38,	
127,147,	128,148,	4,29,	4,30,	
4,31,	4,32,	4,33,	129,149,	
47,66,	131,151,	135,152,	136,153,	
137,154,	72,92,	138,155,	72,92,	
47,0,	47,0,	72,93,	72,93,	
72,93,	72,93,	72,93,	72,93,	
72,93,	72,93,	72,93,	72,93,	
3,39,	3,40,	3,41,	4,35,	
139,156,	140,157,	142,158,	4,36,	
5,17,	4,37,	4,38,	5,18,	
143,159,	5,20,	5,21,	5,22,	
144,160,	5,24,	5,25,	145,161,	
5,27,	146,162,	148,163,	149,164,	
47,45,	150,165,	93,0,	114,0,	
93,114,	114,114,	151,166,	5,29,	
5,30,	5,31,	5,32,	5,33,	
93,114,	114,114,	152,167,	4,39,	
4,40,	4,41,	153,168,	154,169,	
155,170,	157,171,	158,172,	6,17,	
93,70,	114,70,	6,18,	159,173,	
6,20,	6,21,	6,22,	160,174,	
6,24,	6,25,	161,175,	6,27,	
5,35,	162,176,	164,177,	168,180,	
5,36,	169,181,	5,37,	5,38,	
93,70,	114,70,	6,29,	6,30,	
6,31,	6,32,	6,33,	89,111,	
89,111,	89,111,	89,111,	89,111,	
89,111,	89,111,	89,111,	90,112,	
90,112,	90,112,	90,112,	90,112,	
90,112,	90,112,	90,112,	170,182,	
173,183,	175,184,	178,185,	179,186,	
5,39,	5,40,	5,41,	6,35,	
180,187,	181,188,	182,189,	6,36,	
7,17,	6,37,	6,38,	7,18,	
184,190,	7,20,	7,21,	7,22,	
186,192,	7,24,	7,25,	191,193,	
7,27,	92,93,	92,93,	92,93,	
92,93,	92,93,	92,93,	92,93,	
92,93,	92,93,	92,93,	7,29,	
7,30,	7,31,	7,32,	7,33,	
194,196,	195,197,	196,198,	6,39,	
6,40,	6,41,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,17,	
0,0,	0,0,	8,18,	0,0,	
8,20,	8,21,	8,22,	0,0,	
8,24,	8,25,	0,0,	8,27,	
7,35,	0,0,	0,0,	0,0,	
7,36,	0,0,	7,37,	7,38,	
0,0,	0,0,	8,29,	8,30,	
8,31,	8,32,	8,33,	0,0,	
16,0,	16,45,	16,45,	0,0,	
16,45,	0,0,	0,0,	0,0,	
16,45,	16,0,	130,150,	130,150,	
130,150,	130,150,	130,150,	130,150,	
130,150,	130,150,	0,0,	0,0,	
7,39,	7,40,	7,41,	8,35,	
0,0,	0,0,	0,0,	8,36,	
0,0,	8,37,	8,38,	16,45,	
0,0,	16,46,	0,0,	19,0,	
19,48,	19,48,	16,45,	19,48,	
0,0,	0,0,	16,45,	19,48,	
19,0,	16,45,	0,0,	16,45,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	49,0,	16,45,	
0,0,	0,0,	0,0,	8,39,	
8,40,	8,41,	0,0,	49,48,	
16,45,	0,0,	19,48,	16,45,	
19,48,	0,0,	16,45,	0,0,	
0,0,	19,0,	0,0,	0,0,	
0,0,	19,48,	0,0,	0,0,	
19,48,	0,0,	19,48,	0,0,	
0,0,	0,0,	0,0,	16,45,	
0,0,	0,0,	19,48,	16,47,	
49,48,	0,0,	16,45,	0,0,	
0,0,	0,0,	0,0,	19,48,	
0,0,	49,68,	19,48,	0,0,	
0,0,	19,48,	0,0,	0,0,	
0,0,	49,0,	49,0,	132,45,	
132,45,	132,45,	132,45,	132,45,	
132,45,	132,45,	132,45,	0,0,	
0,0,	34,62,	19,48,	0,0,	
0,0,	0,0,	19,49,	0,0,	
0,0,	19,48,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
0,0,	0,0,	0,0,	0,0,	
0,0,	49,48,	0,0,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	49,69,	0,0,	0,0,	
0,0,	34,63,	0,0,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	34,63,	34,63,	34,63,	
34,63,	53,70,	53,70,	53,70,	
0,0,	53,70,	0,0,	0,0,	
0,0,	53,70,	53,70,	133,48,	
133,48,	133,48,	133,48,	133,48,	
133,48,	133,48,	133,48,	165,178,	
165,178,	165,178,	165,178,	165,178,	
165,178,	165,178,	165,178,	0,0,	
0,0,	0,0,	0,0,	0,0,	
53,70,	0,0,	53,70,	88,0,	
0,0,	0,0,	0,0,	53,70,	
0,0,	0,0,	0,0,	53,70,	
88,87,	0,0,	53,0,	0,0,	
53,71,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
53,71,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	57,74,	
57,74,	53,70,	57,74,	57,74,	
53,72,	0,0,	0,0,	53,70,	
0,0,	88,87,	185,191,	185,191,	
185,191,	185,191,	185,191,	185,191,	
185,191,	185,191,	88,109,	0,0,	
0,0,	0,0,	57,74,	0,0,	
53,72,	0,0,	88,0,	88,0,	
53,70,	0,0,	0,0,	53,70,	
55,73,	55,73,	55,73,	55,73,	
55,73,	55,73,	55,73,	55,73,	
55,73,	55,73,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	55,73,	55,73,	55,73,	
55,73,	55,73,	55,73,	57,75,	
0,0,	57,76,	0,0,	57,77,	
57,78,	0,0,	88,87,	57,79,	
0,0,	0,0,	57,80,	0,0,	
57,81,	57,82,	57,83,	0,0,	
0,0,	57,84,	57,85,	57,86,	
0,0,	0,0,	0,0,	0,0,	
0,0,	55,73,	55,73,	55,73,	
55,73,	55,73,	55,73,	64,0,	
64,87,	64,87,	88,110,	64,87,	
0,0,	0,0,	0,0,	64,87,	
64,0,	193,195,	193,195,	193,195,	
193,195,	193,195,	193,195,	193,195,	
193,195,	197,87,	197,87,	197,87,	
197,87,	197,87,	197,87,	197,87,	
197,87,	0,0,	0,0,	0,0,	
0,0,	0,0,	64,87,	0,0,	
64,87,	0,0,	0,0,	0,0,	
0,0,	64,0,	0,0,	0,0,	
0,0,	64,87,	0,0,	0,0,	
64,87,	0,0,	64,87,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	64,87,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	64,87,	
0,0,	0,0,	64,87,	0,0,	
0,0,	64,87,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	69,67,	64,87,	0,0,	
0,0,	0,0,	64,88,	0,0,	
0,0,	64,87,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	69,91,	69,91,	69,91,	
69,91,	110,108,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	110,131,	110,131,	110,131,	
110,131,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
134,48,	134,48,	134,48,	134,48,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	166,179,	
166,179,	166,179,	166,179,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	192,194,	192,194,	
192,194,	192,194,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	198,87,	198,87,	198,87,	
198,87,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-40,	yysvec+1,	0,	
yycrank+-97,	yysvec+1,	0,	
yycrank+-136,	yysvec+1,	0,	
yycrank+-193,	yysvec+1,	0,	
yycrank+-232,	yysvec+1,	0,	
yycrank+-289,	yysvec+1,	0,	
yycrank+-328,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+4,
yycrank+0,	0,		yyvstop+7,
yycrank+0,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+12,
yycrank+18,	0,		yyvstop+15,
yycrank+0,	yysvec+14,	yyvstop+17,
yycrank+-391,	0,		yyvstop+20,
yycrank+0,	0,		yyvstop+22,
yycrank+0,	0,		yyvstop+25,
yycrank+-426,	0,		yyvstop+28,
yycrank+9,	0,		yyvstop+30,
yycrank+0,	0,		yyvstop+34,
yycrank+2,	0,		yyvstop+37,
yycrank+0,	0,		yyvstop+40,
yycrank+0,	0,		yyvstop+43,
yycrank+0,	0,		yyvstop+46,
yycrank+6,	0,		yyvstop+49,
yycrank+0,	0,		yyvstop+52,
yycrank+57,	0,		yyvstop+55,
yycrank+4,	0,		yyvstop+58,
yycrank+0,	0,		yyvstop+61,
yycrank+12,	0,		yyvstop+64,
yycrank+5,	0,		yyvstop+68,
yycrank+7,	0,		yyvstop+71,
yycrank+474,	0,		yyvstop+74,
yycrank+14,	yysvec+34,	yyvstop+77,
yycrank+0,	0,		yyvstop+80,
yycrank+0,	0,		yyvstop+83,
yycrank+16,	0,		yyvstop+86,
yycrank+0,	0,		yyvstop+89,
yycrank+0,	0,		yyvstop+92,
yycrank+0,	0,		yyvstop+95,
yycrank+0,	0,		yyvstop+98,
yycrank+0,	0,		yyvstop+100,
yycrank+0,	yysvec+14,	0,	
yycrank+-4,	yysvec+16,	0,	
yycrank+0,	0,		yyvstop+102,
yycrank+-152,	yysvec+16,	0,	
yycrank+17,	0,		0,	
yycrank+-445,	yysvec+19,	0,	
yycrank+0,	0,		yyvstop+104,
yycrank+0,	0,		yyvstop+106,
yycrank+0,	0,		yyvstop+108,
yycrank+-596,	0,		yyvstop+110,
yycrank+0,	yysvec+28,	yyvstop+112,
yycrank+644,	0,		0,	
yycrank+0,	0,		yyvstop+114,
yycrank+650,	0,		yyvstop+116,
yycrank+0,	0,		yyvstop+118,
yycrank+0,	0,		yyvstop+120,
yycrank+0,	0,		yyvstop+122,
yycrank+0,	0,		yyvstop+124,
yycrank+0,	0,		yyvstop+126,
yycrank+0,	yysvec+34,	yyvstop+128,
yycrank+-746,	0,		yyvstop+130,
yycrank+0,	0,		yyvstop+132,
yycrank+5,	0,		0,	
yycrank+0,	0,		yyvstop+134,
yycrank+12,	0,		0,	
yycrank+794,	0,		0,	
yycrank+0,	0,		yyvstop+136,
yycrank+-12,	yysvec+53,	yyvstop+138,
yycrank+162,	0,		yyvstop+141,
yycrank+0,	yysvec+55,	yyvstop+143,
yycrank+0,	yysvec+57,	0,	
yycrank+3,	0,		0,	
yycrank+6,	0,		0,	
yycrank+2,	0,		0,	
yycrank+9,	0,		0,	
yycrank+1,	0,		0,	
yycrank+42,	0,		0,	
yycrank+4,	0,		0,	
yycrank+10,	0,		0,	
yycrank+9,	0,		0,	
yycrank+36,	0,		0,	
yycrank+35,	0,		0,	
yycrank+41,	0,		0,	
yycrank+38,	0,		0,	
yycrank+-630,	yysvec+64,	0,	
yycrank+247,	0,		0,	
yycrank+255,	0,		0,	
yycrank+28,	0,		0,	
yycrank+289,	0,		0,	
yycrank+-200,	yysvec+53,	yyvstop+145,
yycrank+39,	0,		0,	
yycrank+47,	0,		0,	
yycrank+56,	0,		0,	
yycrank+44,	0,		0,	
yycrank+53,	0,		0,	
yycrank+52,	0,		0,	
yycrank+97,	0,		yyvstop+147,
yycrank+66,	0,		0,	
yycrank+59,	0,		0,	
yycrank+64,	0,		0,	
yycrank+83,	0,		0,	
yycrank+80,	0,		0,	
yycrank+85,	0,		0,	
yycrank+66,	0,		0,	
yycrank+0,	0,		yyvstop+149,
yycrank+62,	0,		0,	
yycrank+878,	0,		0,	
yycrank+68,	0,		0,	
yycrank+69,	0,		0,	
yycrank+46,	0,		0,	
yycrank+-201,	yysvec+53,	yyvstop+151,
yycrank+98,	0,		0,	
yycrank+92,	0,		0,	
yycrank+90,	0,		0,	
yycrank+101,	0,		0,	
yycrank+107,	0,		0,	
yycrank+100,	0,		0,	
yycrank+106,	0,		0,	
yycrank+64,	0,		0,	
yycrank+102,	0,		0,	
yycrank+120,	0,		0,	
yycrank+108,	0,		0,	
yycrank+118,	0,		0,	
yycrank+125,	0,		0,	
yycrank+126,	0,		0,	
yycrank+116,	0,		0,	
yycrank+354,	0,		0,	
yycrank+109,	0,		0,	
yycrank+455,	0,		0,	
yycrank+559,	0,		0,	
yycrank+953,	0,		0,	
yycrank+120,	0,		0,	
yycrank+137,	0,		0,	
yycrank+122,	0,		0,	
yycrank+124,	0,		0,	
yycrank+148,	0,		0,	
yycrank+147,	0,		0,	
yycrank+0,	0,		yyvstop+154,
yycrank+109,	0,		0,	
yycrank+159,	0,		0,	
yycrank+154,	0,		0,	
yycrank+163,	0,		0,	
yycrank+164,	0,		0,	
yycrank+0,	0,		yyvstop+156,
yycrank+173,	0,		0,	
yycrank+174,	0,		0,	
yycrank+153,	0,		0,	
yycrank+130,	0,		0,	
yycrank+174,	0,		0,	
yycrank+197,	0,		0,	
yycrank+179,	0,		0,	
yycrank+186,	0,		0,	
yycrank+0,	0,		yyvstop+158,
yycrank+196,	0,		0,	
yycrank+154,	0,		0,	
yycrank+193,	0,		0,	
yycrank+197,	0,		0,	
yycrank+209,	0,		0,	
yycrank+216,	0,		0,	
yycrank+0,	0,		yyvstop+160,
yycrank+214,	0,		0,	
yycrank+567,	0,		0,	
yycrank+1028,	0,		0,	
yycrank+0,	0,		yyvstop+162,
yycrank+216,	0,		0,	
yycrank+216,	0,		0,	
yycrank+246,	0,		0,	
yycrank+0,	0,		yyvstop+164,
yycrank+0,	0,		yyvstop+166,
yycrank+226,	0,		0,	
yycrank+0,	0,		yyvstop+168,
yycrank+229,	0,		0,	
yycrank+0,	0,		yyvstop+170,
yycrank+0,	0,		yyvstop+172,
yycrank+222,	0,		0,	
yycrank+223,	0,		0,	
yycrank+245,	0,		0,	
yycrank+253,	0,		0,	
yycrank+246,	0,		0,	
yycrank+0,	0,		yyvstop+174,
yycrank+259,	0,		0,	
yycrank+622,	0,		0,	
yycrank+212,	0,		0,	
yycrank+0,	0,		yyvstop+176,
yycrank+0,	0,		yyvstop+178,
yycrank+0,	0,		yyvstop+180,
yycrank+0,	0,		yyvstop+182,
yycrank+243,	0,		0,	
yycrank+1103,	0,		0,	
yycrank+709,	0,		0,	
yycrank+260,	0,		0,	
yycrank+261,	0,		0,	
yycrank+234,	0,		0,	
yycrank+717,	0,		0,	
yycrank+1178,	0,		0,	
0,	0,	0};
struct yywork *yytop = yycrank+1300;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   2,   3,   3,   5,   3,   3, 
  3,   9,  10,   3,   9,   9,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
 32,   3,  34,   3,   3,   3,   3,  39, 
  3,   3,   3,  43,   3,  43,  46,   3, 
 48,  48,  48,  48,  48,  48,  48,  48, 
 56,  56,   3,   3,   3,   3,   3,   3, 
  3,  65,  65,  65,  68,  68,  65,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 88,  71,  71,   3,  92,   3,   3,  95, 
  3,  65,  65,  65,  68,  68,  65,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 71,  71,  71,  71,  71,  71,  71,  71, 
 88,  71,  71,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
  3,   3,   3,   3,   3,   3,   3,   3, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,1,1,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,1,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.12	97/12/08 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
#ifdef YYISARRAY
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
#else
			if (yylastch >= &yytext[ yytextsz ]) {
				int	x = yylastch - yytext;

				yytextsz += YYTEXTSZINC;
				if (yytext == yy_tbuf) {
				    yytext = (char *) malloc(yytextsz);
				    memcpy(yytext, yy_tbuf, sizeof (yy_tbuf));
				}
				else
				    yytext = (char *) realloc(yytext, yytextsz);
				if (!yytext) {
				    fprintf(yyout,
					"Cannot realloc yytext\n");
				    exit(1);
				}
				yylastch = yytext + x;
			}
#endif
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (uintptr_t)yyt > (uintptr_t)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((uintptr_t)yyt < (uintptr_t)yycrank) {	/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
