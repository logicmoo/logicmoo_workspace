/*
	soapcpp2.h

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.cs.fsu.edu/~engelen/gsoapcompilerlicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Original Code is ''gSOAP compiler'' consisting of:
error2.c, error2.h, init2.c, soapcpp2.c, soapcpp2.h, soapcpp2.l, soapcpp2.y, symbol2.c.
The Initial Developer of the Original Code is Robert A. van Engelen.
Copyright (C) 2000-2002 Robert A. van Engelen. All Rights Reserved.

*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "error2.h"

#define VERSION "2.1.6b"	/* Current version */

#if defined(WIN32) || defined(__GLIBC__) || defined(__APPLE__)
#ifndef WITH_BISON
#define WITH_BISON
#endif
#endif

/* #define DEBUG */ /* uncomment to debug */

#ifdef DEBUG
#define	check(expr, msg) ((expr) ? 1 : progerror(msg, __FILE__, __LINE__))
#define DBGLOG(DBGCMD) { DBGCMD; }
#else
#define check(expr, msg) (expr, 1)
#define DBGLOG(DBGCMD)
#endif

#ifdef WITH_BISON
#ifdef WIN32
#define yyparse soapcpp2parse
#define yylex soapcpp2lex
#define yyerror soapcpp2error
#define yylval soapcpp2lval
#define yychar soapcpp2char
#define yydebug soapcpp2debug
#define yynerrs soapcpp2nerrs
#define yylineno soapcpp2lineno
#define yytext soapcpp2text
#define yyin soapcpp2in
#define yywrap soapcpp2wrap
#endif
#endif

#ifdef WIN32
#define LONG64 __int64
#else
#define LONG64 long long
#endif

extern int yylineno;

typedef	enum Bool {False, True} Bool;

typedef	int Token;

typedef	enum Type
{	Tnone,
	Tvoid,
	Tchar,
	Twchar,
	Tshort,
	Tint,
	Tlong,
	Tllong,
	Tfloat,
	Tdouble,
	Tuchar,
	Tushort,
	Tuint,
	Tulong,
	Tullong,
	Tenum,
	Ttime,
	Tclass,
	Tstruct,
	Tunion,
	Tpointer,
	Treference,
	Tarray,
	Tfun
} Type;

#define	TYPES (Tfun+1)	/* number of type (operators) enumerated above */

typedef	enum Storage
{	Snone		= 0,
	Sauto		= 1,
	Sregister	= 2,
	Sstatic		= 4,
	Sextern		= 8,
	Stypedef	= 16,
	Svirtual	= 32,
	Sconst		= 64,
	Sinline		= 128,
	Sconstobj	= 256,
	Sabstract	= 512,
	SmustUnderstand	= 1024
} Storage;

typedef	enum Level { INTERNAL, GLOBAL, PARAM, LOCAL } Level;

#define mknone()	mktype(Tnone,     NULL, 0)
#define mkvoid()	mktype(Tvoid,     NULL, 0)
#define mkbool()	mktype(Tenum,     booltable, 2)
#define mkchar()	mktype(Tchar,     NULL, 1)
#define mkwchart()	mktype(Twchar,    NULL, 4)
#define mkshort()	mktype(Tshort,    NULL, 2)
#define mkint()		mktype(Tint,      NULL, 4)
#define mklong()	mktype(Tlong,     NULL, 8)
#define mkllong()	mktype(Tllong,    NULL, 8)
#define mkfloat()	mktype(Tfloat,    NULL, 4)
#define mkdouble()	mktype(Tdouble,   NULL, 8)
#define mkuchar()	mktype(Tuchar,    NULL, 1)	/* unsigned char */
#define mkushort()	mktype(Tushort,   NULL, 2)	/* unsigned short */
#define mkuint()	mktype(Tuint,     NULL, 4)	/* unsigned int */
#define mkulong()	mktype(Tulong,    NULL, 8)	/* unsigned long */
#define mkullong()	mktype(Tullong,   NULL, 8)	/* unsigned long */
#define mktimet()	mktype(Ttime,     NULL, 4)
#define mkenum(t)	mktype(Tenum,	  t,    4)
#define mkmask(t)	mktype(Tenum,	  t,    8)
#define	mkpointer(t)	mktype(Tpointer,  t,    4)
#define	mkreference(t)	mktype(Treference,t,    4)
#define	mkclass(t, w)	mktype(Tclass,    t,    w)
#define	mkstruct(t, w)	mktype(Tstruct,   t,    w)
#define	mkunion(t, w)	mktype(Tunion,    t,    w)
#define	mkarray(t, w)	mktype(Tarray,    t,    w)
#define	mkfun(t)	mktype(Tfun,      t,    0)
#define mkstring()	mkpointer(mkchar())

typedef struct Symbol
{	char	*name;
	Token	token;
	struct	Symbol *next;
} Symbol;

Symbol	*install(const char*, Token), *lookup(const char*), *gensym(const char*);

typedef	struct Tnode
{	Type	type;
	void	*ref;
	Symbol	*id;	/* struct/class/union/enum name */
	Symbol	*sym;	/* typedef name */
	struct	Entry *response; /* funcs only: points to response struct */
	int	width;
	int	transient;
	struct	Tnode *next;
        Bool generated;
        Bool wsdl;
} Tnode;

typedef	union Value {
	LONG64	i;
	double	r;
	char	*s;
} Value;

typedef	struct IDinfo {
	Tnode	*typ;
	Storage	sto;
	Bool	hasval;		/* if true, identifier is constant */
	Value	val;		/* ... with this value */
	int	offset;
	int	minOccurs;
	int	maxOccurs;
} IDinfo;

typedef	struct Entry {
	Symbol	*sym;
	IDinfo	info;
	Level	level;
	int	lineno;
	struct	Entry *next;
} Entry;

typedef	struct Table {
	Symbol	*sym;
	Level	level;
	Entry	*list;
	struct	Table *prev;
} Table;

typedef struct FNinfo {
	Tnode	*ret;
	Table	*args;
} FNinfo;


typedef	struct Node {
	Tnode	*typ;
	Storage	sto;
	Bool	hasval;		/* if true, this node has a constant value */
	Value	val;		/* ... this is the value */
	int	minOccurs;
	int	maxOccurs;
} Node;

#define HDRIN 1
#define HDROUT 2

typedef struct Method
{	struct Method *next;
	char *name;
	short mess;
	char *part;
} Method;

typedef struct Service
{	struct Service *next;
	char *ns;
	char *name;
	char *URL;
	char *executable;
	char *URI;
	char *WSDL;
	char *encoding;
	struct Method *list;
} Service;

extern Entry *enter(Table*, Symbol*), *entry(Table*, Symbol*), *enumentry(Symbol*);

extern Table *mktable(Table*);

extern Tnode *mkmethod(Tnode*, Table*);

extern char *emalloc(unsigned int);

extern Tnode *mktype(Type, void*, int);
extern Tnode *mksymtype(Tnode*, Symbol*);

extern int is_transient(Tnode*);
extern int is_response(Tnode*);

extern Table *typetable, *uniontable, *enumtable, *classtable, *booltable;

extern void compile(Table*);
extern void freetable(Table*);
extern Entry *unlinklast(Table*); 

extern int cflag;
extern int mflag;
extern char *dirpath;
extern char *prefix;
extern int custom_header;
extern int custom_fault;
extern Service *services;
extern Symbol *namespaceid;
extern int transient;

