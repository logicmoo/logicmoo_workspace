/*
	error2.c
	Error handling routines

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

#include "soapcpp2.h"
#ifdef WITH_BISON
#include "soapcpp2.tab.h"
#else
#include "y.tab.h"
#endif

#define	MAXERR 10

extern char yytext[];	/* lexeme found by the lexical analyzer */

static int lexerrno = 0;
static int synerrno = 0;
static int semerrno = 0;
static int semwarno = 0;

char errbuf[1024];	/* to hold error messages */

/*
yyerror - called by parser from an error production with nonterminal `error'
*/
yyerror(char *s)
{	fprintf(stderr, "%s at line %d\n", s, yylineno);
}

/*
lexerror - called by lexical analyzer upon failure to recognize a token
*/
lexerror(const char *s)
{	fprintf(stderr, "%s: %s at line %d\n", s, yytext, yylineno);
	lexerrno++;
}

/*
synerror - called by a semantic action in the yacc grammar
*/
synerror(const char *s)
{	fprintf(stderr, "Syntax error: %s at line %d\n", s, yylineno-1);
	if (synerrno++ >= MAXERR)
		execerror("too many syntactic errors, bailing out");
}

/*
semerror - report semantic error from static checking
*/
semerror(const char *s)
{	fprintf(stderr, "Error: %s at line %d\n", s, yylineno);
	if (semerrno++ >= MAXERR)
		execerror("too many semantic errors, bailing out");
}

/*
semwarn - report semantic warning from static checking
*/
semwarn(const char *s)
{	fprintf(stderr, "Warning: %s at line %d\n", s, yylineno);
	semwarno++;
}

/*
typerror - report type error (a semantic error)
*/
typerror(const char *s)
{	fprintf(stderr, "Type error: ");
	semerror(s);
}

/*
execerror - print error message and terminate execution
*/
execerror(const char *s)
{	fprintf(stderr, "Critical error: %s\n", s);
	exit(-1);
}

/*
progerror - called when check(expr) failed, i.e. upon programming error
*/
progerror(const char *s, const char *f, int l)
{	fprintf(stderr, "Program failure: %s in file %s line %d\n", s, f, l);
	exit(-1);
}

/*
errstat - show error statistics
*/
errstat()
{	if (!lexerrno && !synerrno && !semerrno)
		fprintf(stderr, "\nCompilation successful\n");
	else
		fprintf(stderr, "\nThere were error(s):\n");
	if (lexerrno)
		fprintf(stderr, "%d lexical error(s)\n", lexerrno);
	if (synerrno)
		fprintf(stderr, "%d syntax error(s)\n", synerrno);
	if (semerrno)
		fprintf(stderr, "%d semantic error(s)\n", semerrno);
	if (semwarno)
		fprintf(stderr, "%d warning(s)\n", semwarno);
	fprintf(stderr, "\n");
}
