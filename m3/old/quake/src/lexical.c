/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Wed Mar 30 09:52:49 PST 1994 by harrison
 */

#include "quake.h"
#include "y.tab.h"

#define LOWER_ALPHA_CASES \
	case 'a': \
	case 'b': \
	case 'c': \
	case 'd': \
	case 'e': \
	case 'f': \
	case 'g': \
	case 'h': \
	case 'i': \
	case 'j': \
	case 'k': \
	case 'l': \
	case 'm': \
	case 'n': \
	case 'o': \
	case 'p': \
	case 'q': \
	case 'r': \
	case 's': \
	case 't': \
	case 'u': \
	case 'v': \
	case 'w': \
	case 'x': \
	case 'y': \
	case 'z':

#define UPPER_ALPHA_CASES \
	case 'A': \
	case 'B': \
	case 'C': \
	case 'D': \
	case 'E': \
	case 'F': \
	case 'G': \
	case 'H': \
	case 'I': \
	case 'J': \
	case 'K': \
	case 'L': \
	case 'M': \
	case 'N': \
	case 'O': \
	case 'P': \
	case 'Q': \
	case 'R': \
	case 'S': \
	case 'T': \
	case 'U': \
	case 'V': \
	case 'W': \
	case 'X': \
	case 'Y': \
	case 'Z':

#define ALPHA_CASES \
	LOWER_ALPHA_CASES \
	UPPER_ALPHA_CASES

#define ALPHA_NUMERIC_CASES \
	ALPHA_CASES \
	NUMERIC_CASES

#define NAME_START_CASES \
	ALPHA_CASES \
	case '_':

#define NAME_REMAINER_CASES \
	ALPHA_NUMERIC_CASES \
	case '_': \
	case '-': \
	case '.':

#define PUNCTUATION_CASES \
	case '$': \
	case '&': \
	case '(': \
	case ')': \
	case '+': \
	case ',': \
	case ':': \
	case '<': \
	case '=': \
	case '>': \
	case '[': \
	case ']': \
	case '{': \
	case '}':

#define NUMERIC_CASES \
	case '0': \
	case '1': \
	case '2': \
	case '3': \
	case '4': \
	case '5': \
	case '6': \
	case '7': \
	case '8': \
	case '9':

#define WHITESPACE_CASES \
	case ' ': \
	case '\f': \
	case '\n': \
	case '\r': \
	case '\t':

static int CurrentChar;

#define NextCharacter() \
    if ((CurrentChar = getc(Execute_CurrentStream)) == '\n') { \
        Code_Compile(Atom_LineNumber(++Parser_LineNumber)); \
    }

#define UnGetCharacter() \
{ \
    if (CurrentChar == '\n') \
        Code_Compile(Atom_LineNumber(--Parser_LineNumber)); \
    if (CurrentChar != EOF) \
	ungetc(CurrentChar, Execute_CurrentStream); \
}

static int LookUpKeyword(t)
char *t;
{
    switch (t[0]) {
    case 'a':
	if (t[1] == 'n' &&
	    t[2] == 'd' &&
	    t[3] == '\0') return AND;
	break;
    case 'c':
	if (t[1] == 'o' &&
	    t[2] == 'n' &&
	    t[3] == 't' &&
	    t[4] == 'a' &&
	    t[5] == 'i' &&
	    t[6] == 'n' &&
	    t[7] == 's' && 
	    t[8] == '\0') return CONTAINS;
	break;
    case 'e':
	switch (t[1]) {
	case 'l':
	    if (t[2] == 's' &&
		t[3] == 'e' &&
		t[4] == '\0') return ELSE;
	    break;
	case 'n':
	    if (t[2] == 'd' &&
		t[3] == '\0') return END;
	    break;
	}
	break;
    case 'f':
	if (t[1] == 'o' &&
	    t[2] == 'r' &&
	    t[3] == 'e' &&
	    t[4] == 'a' &&
	    t[5] == 'c' &&
	    t[6] == 'h' && 
	    t[7] == '\0') return FOREACH;
	break;
    case 'i':
	switch (t[1]) {
	case 'f':
	    if (t[2] == '\0') return IF;
	    break;
	case 'n':
	    if (t[2] == '\0') return IN;
	    break;
	case 's':
	    if (t[2] == '\0') return IS;
	    break;
	}
	break;
    case 'l':
	if (t[1] == 'o' &&
	    t[2] == 'c' &&
	    t[3] == 'a' &&
	    t[4] == 'l' &&
	    t[5] == '\0') return LOCAL;
	break;
    case 'n':
	if (t[1] == 'o' &&
	    t[2] == 't' &&
	    t[3] == '\0') return NOT;
	break;
    case 'o':
	if (t[1] == 'r' &&
	    t[2] == '\0') return OR;
	break;
    case 'p':
	if (t[1] == 'r' &&
	    t[2] == 'o' &&
	    t[3] == 'c' &&
	    t[4] == '\0') return PROC;
	break;
    case 'r':
	if (t[1] == 'e') {
	    switch (t[2]) {
	    case 't':
		if (t[3] == 'u' &&
		    t[4] == 'r' &&
		    t[5] == 'n' && 
		    t[6] == '\0') return RETURN;
		break;
	    case 'a':
		if (t[3] == 'd' &&
		    t[4] == 'o' &&
		    t[5] == 'n' &&
		    t[6] == 'l' &&
		    t[7] == 'y' &&
		    t[8] == '\0') return READONLY;
		break;
	    }
	}
	break;
    }

    return ID;
}

static Integer ReadInteger()
{
    Integer result = 0;

    do {
	result = result * 10 + (CurrentChar - '0');
	NextCharacter();
    } while ('0' <= CurrentChar && CurrentChar <= '9');

    UnGetCharacter();
	    
    return result;
}

static int ReadNameOrKeyword(yylval)
YYSTYPE *yylval;
{
    char buffer[MAX_NAMELENGTH + 1];
    char *p = buffer;
    
    *p++ = CurrentChar;
    NextCharacter();

    for (;;) {
	switch (CurrentChar) {
	NAME_REMAINER_CASES
	    if (p < buffer + MAX_NAMELENGTH)
		*p++ = CurrentChar;
	    NextCharacter();
	    break;
        default: {
 	    int token;

	    *p = '\0';
	    
	    UnGetCharacter();
	    
	    if ((token = LookUpKeyword(buffer)) == ID)
		yylval->name = Name_Register(Utils_StringSave(buffer));
	    
	    return token;
	}
        }
    }
}

#define	STRING_INITIAL_SIZE	(10)

static String ReadString()
{
    static char *StringBuffer = NULL;
    static int StringBufferLength;

    String string = String_New(NULL);
    int next = 0;

    if (StringBuffer == NULL) {
	StringBufferLength = STRING_INITIAL_SIZE;
	StringBuffer = NEW_ARRAY(char, StringBufferLength + 1);
    }

    NextCharacter();
    while (CurrentChar != '"' && CurrentChar != EOF) {
	char realChar = CurrentChar;

	if (CurrentChar == '\\') {
	    NextCharacter();
	    switch (CurrentChar) {
	    case '\n':
		continue;	/* ignore quoted new-line */
	    case '\\':
		realChar = '\\';
		break;
	    case 'n':
		realChar = '\n';
		break;
	    case 'r':
		realChar = '\r';
		break;
	    case 't':
		realChar = '\t';
		break;
	    case 'b':
		realChar = '\b';
		break;
	    case 'f':
		realChar = '\f';
		break;
	    case '"':
		realChar = '"';
		break;
	    }
	}
	if (next >= StringBufferLength) {
	    StringBufferLength = 1 + StringBufferLength * 162 / 100;
	    StringBuffer = RENEW_ARRAY(StringBuffer, char, StringBufferLength + 1);
	}
	RANGE_CHECK(next, 0, StringBufferLength);
	StringBuffer[next++] = realChar;

	NextCharacter();
    }
    /* we already made room for the '\0' above */
    StringBuffer[next] = '\0';

    string->body = Utils_StringSave(StringBuffer);

    return string;
}

static char *MakeCharLegible(c)
int c;
{
    static char buf[100];

    if (' ' < c && c <= '~')
	sprintf(buf, "%c", c);
    else
	sprintf(buf, "\\%03o", c);

    return buf;
}

int yylex()
{
    extern YYSTYPE yylval;
    
    for (;;) {
	NextCharacter();
	switch (CurrentChar) {
	case EOF:
	    /* end of file */
	    return 0;
        WHITESPACE_CASES
	    /* it's white space */
	    continue;		/* outer `for' loop */
	case '%':
	case '#':
	    /* it's a single-line comment */
	    do {
		NextCharacter();
	    } while (CurrentChar != EOF && CurrentChar != '\n');
	    continue;		/* outer `for' loop */
	case '/':
	    /* it may be a C-style comment */
	    NextCharacter();
	    if (CurrentChar == '*') {
		Boolean comment_complete = FALSE;

		while (!comment_complete) {
		    do {
			NextCharacter();
		    } while (CurrentChar != EOF && CurrentChar != '*');

		    NextCharacter();
		    if (CurrentChar == '/') {
			comment_complete = TRUE;
		    } else
			UnGetCharacter();
		}

		if (comment_complete)
		    continue;	/* outer `for' loop */
	    }

	    UnGetCharacter();

	    yywarning("unexpected `/'");
	    yywarning("perhaps you're reading an old-style name with a `/' in it");
	    yywarning("this sometimes happens when m3build needs to be rerun in the included directory");
	    yyerror("to update the .M3EXPORTS file to the new format.");
	    break;
	case '"':
	    /* it's a string */
	    yylval.string = ReadString();

	    return STRING;
        NUMERIC_CASES
            /* it's an integer */
	    yylval.integer = ReadInteger();

	    return INTEGER;
	NAME_START_CASES
	    return ReadNameOrKeyword(&yylval);
        PUNCTUATION_CASES
	    /* punctuation */
	    return CurrentChar;
	default:
	    yyerror("unrecognized character '%s' read", MakeCharLegible(CurrentChar));
	    break;
	}
    }
}

