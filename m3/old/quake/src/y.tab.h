typedef union {
    Array array;
    Atom atom;
    Integer integer;
    List list;
    Name name;
    String string;
} YYSTYPE;
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


extern YYSTYPE yylval;
