%{
/* Copyright (C) 1989, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */

/* Last modified on Wed Aug 10 13:21:55 PDT 1994 by kalsow         */
/*      modified on Thu Apr 23 18:08:06 PDT 1992 by muller         */
/*      modified on Mon Apr 20 15:59:06 1992 by nichols@xerox.com  */
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan         */
%}

%Start Prog Com Prag

%%

"("/"*"		{ return (HandleNPS()); }
"<"/"*"		{ return (HandleNPS()); }
[ \t\f\n\r]	{ return (HandleNPS()); }
"+"		{BufferLexeme(1); return(PLUS);}
"-"		{BufferLexeme(1); return(MINUS);}
"*"		{BufferLexeme(1); return(ASTERISK);}
"/"		{BufferLexeme(1); return(SLASH);}
":="		{BufferLexeme(1); return(ASSIGN);}
"&"		{BufferLexeme(1); return(AMPERSAND);}
"."		{BufferLexeme(1); return(DOT);}
","		{BufferLexeme(1); return(COMMA);}
";"		{BufferLexeme(1); return(SEMICOLON);}
"("		{BufferLexeme(1); return(LPAREN);}
"["		{BufferLexeme(1); return(LBRACKET);}
"{"		{BufferLexeme(1); return(LBRACE);}
"^"		{BufferLexeme(1); return(UPARROW);}
"="		{BufferLexeme(1); return(EQUAL);}
"=>"		{BufferLexeme(1); return(RARROW);}
"#"		{BufferLexeme(1); return(SHARP);}
"<"		{BufferLexeme(1); return(LESS);}
">"		{BufferLexeme(1); return(GREATER);}
"<="		{BufferLexeme(1); return(LSEQUAL);}
"<:"		{BufferLexeme(1); return(SUBTYPE);}
">="		{BufferLexeme(1); return(GREQUAL);}
".."		{BufferLexeme(1); return(DOTDOT);}
":"		{BufferLexeme(1); return(COLON);}
")"		{BufferLexeme(1); return(RPAREN);}
"]"		{BufferLexeme(1); return(RBRACKET);}
"}"		{BufferLexeme(1); return(RBRACE);}
"|"		{BufferLexeme(1); return(BAR);}

[a-zA-Z][a-zA-Z0-9_]*	{PTRKEYWORDENTRY tempp;
				 if ((tempp=lookup(yytext))!=NULL){
				        CapBufferLexeme(1);
					return(tempp->lexval);}
				 else {BufferLexeme(1); return(IDENT);}}

[0-9]+(_[0-9A-Fa-f]+)?        {BufferLexeme(1); return(CARD_CONST);}

[0-9]+"."[0-9]*([EeDdXx][-+]?[0-9]+)?/[^.] {BufferLexeme(1); return(REAL_CONST);}

["]([^"\\\001\n]|\\[^0-9\001]|\\[0-9]{3,3})*["] {
				 BufferLexeme(1); return(STR_CONST);}

[']([^'\\\001\n]|\\[^0-9\001]|\\[0-9]{3,3})['] {
				 BufferLexeme(1); return(STR_CONST);}

[\001]	{
	BufferLexeme(0);
	/* Due to bison bug, we return 0 explicitly instead of ENDOFFILE. */
	return(0);
	}
[\n ]*[\002]	{BufferLexeme(0);return(MODUNIT);}
[\n ]*[\005]	{BufferLexeme(0);return(DEFUNIT);}
.		{BufferLexeme(1); return(BAD);}
