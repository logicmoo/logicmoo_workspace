/*
	soapcpp2.l
	Flex/Lex tokenizer

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

%{
#include "soapcpp2.h"
#ifdef WITH_BISON
#include "soapcpp2.tab.h"
YYSTYPE soapcpp2lval;
#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) \
	{ \
	int c = getc(yyin); \
	result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
	}
#else
#include "y.tab.h"
#endif
static Token install_id();
static Token install_int();
static Token install_hex();
static Token install_num();
static Token install_chr();
static Token install_str();
static Token install_pragma();
static void comment(), directive();
static Token error_chr();
static Token error_str();
static int convchar(int*);
static int hexchar(int*);
static int octchar(int*);
%}
digit		[0-9]
alpha		[a-zA-Z_]
scope		::
id		{alpha}({alpha}|{digit}|{scope})*
int		{digit}+
hex		0[xX][0-9a-fA-F]+
num		{int}(\.{int}([Ee][+-]?{int})?|(\.{int})?[Ee][+-]?{int})
chr		'(\\'|[^'\n])*'
str		\"(\\\"|\\\n|.)*\"
%%
[ \t\v\n\f\r\xA0]	{ /* skip white space (\xA0 is HTML non-breakable space) */ }
"/*"		{ comment(); }
"//gsoap".*\n	{ directive(); }
"//".*\n	{ /* skip comment */ }
"+="		{ return PA; }
"-="		{ return NA; }
"*="		{ return TA; }
"/="		{ return DA; }
"%="		{ return MA; }
"&="		{ return AA; }
"^="		{ return XA; }
"|="		{ return OA; }
"<<="		{ return LA; }
">>="		{ return RA; }
"||"		{ return OR; }
"&&"		{ return AN; }
"=="		{ return EQ; }
"!="		{ return NE; }
"<="		{ return LE; }
">="		{ return GE; }
"<<"		{ return LS; }
">>"		{ return RS; }
"++"		{ return PP; }
"--"		{ return NN; }
"->"		{ return AR; }
[;,:=|^&<>+\-*/%!?~(){}\[\].]	{ return yytext[0]; }
{id}		{ return install_id(); }
{int}		{ return install_int(); }
{hex}		{ return install_hex(); }
{num}		{ return install_num(); }
{chr}		{ return install_chr(); }
{str}		{ return install_str(); }
#.*\n		{ return install_pragma(); }
'[^'\n]*/\n	{ return error_chr(); }
\"[^"\n]*/\n	{ return error_str(); }
.		{ lexerror("Ignoring unknown symbol"); }
%%

/*
	install_id - lookup identifier in symbol table. If found, return token
	and symbol table entry. If not found, create entry in symbol table and
	return ID token.
*/ 
static Token
install_id()
{	Symbol *p = lookup(yytext);
	if (!p)
		p = install(yytext, ID);
	yylval.sym = p;
	return p->token;
}

/*
	install_int - convert digits to integer and return LNG token.
*/
static Token
install_int()
{	sscanf(yytext, "%llu", &yylval.i);
	return LNG;
}

/*
	install_hex - convert hexadecimal digits to integer and return LNG
*/
static Token
install_hex()
{	sscanf(yytext, "%llx", &yylval.i);
	return LNG;
}

/*
	install_num - convert digits to floating point number and return DBL
*/
static Token
install_num()
{	sscanf(yytext, "%lf", &yylval.r);
	return DBL;
}

/*
	install_chr - convert character constant and return CHR.
*/
static Token
install_chr()
{	int i = 2;
	if (yytext[1] == '\\')
		yylval.c = convchar(&i);
	else	yylval.c = yytext[i-1];
	if (yytext[i] != '\'')
		lexerror("Illegal character constant");
	return CHR;
}

/*
	install_str - convert and store string in memory. Return STR.
*/
static Token
install_str()
{	int i, j = 0;
	yylval.s = emalloc(yyleng-1);	/* yyleng = length(yytext) */
	for (i = 1; i < yyleng-1; i++)
		if (yytext[i] == '\\')
		{	if (yytext[++i] != '\n')
			{	yylval.s[j++] = convchar(&i);
				i--;
			}
		}
		else
			yylval.s[j++] = yytext[i];
	yylval.s[j] = '\0';
	return STR;
}

/*
	install_pragma - store pragma in string. Return PRAGMA.
*/
static Token
install_pragma()
{	yylval.s = emalloc(yyleng);	/* yyleng = length(yytext) */
	strncpy(yylval.s, yytext, strlen(yytext)-1);
	yylval.s[strlen(yytext)-1] = '\0';
	return PRAGMA;
}

static void comment()
{	char c, c1;
loop:
        while ((c = input()) != '*' && c != 0)
                ;
        if ((c1 = input()) != '/' && c != 0)
                goto loop;
}

static void directive()
{	int i, j, k;
	char *s;
	Service *sp;
	Method *m;
	int service;
	for (i = 7; yytext[i]; i++)
		if (yytext[i] > 32)
			break;
	for (j = i; yytext[j]; j++)
		if (yytext[j] <= 32)
			break;
	if (i == j)
		return;
	s = (char*)emalloc(j-i+1);
	strncpy(s, yytext+i, j-i);
	s[j-i] = '\0';
	for (sp = services; sp; sp = sp->next)
		if (!strcmp(sp->ns, s))
			break;
	if (!sp)
	{	sp = (Service*)emalloc(sizeof(Service));
		sp->next = services;
		sp->ns = s;
		sp->name = NULL;
		sp->URL = NULL;
		sp->URI = NULL;
		sp->encoding = NULL;
		sp->executable = NULL;
		sp->list = NULL;
		services = sp;
	}
	for (i = j; yytext[i]; i++)
		if (yytext[i] > 32)
			break;
	if (!strncmp(yytext+i, "service", 7) || !strncmp(yytext+i, "schema", 6))
	{	service = strncmp(yytext+i, "schema", 6);
		for (i += 7; yytext[i]; i++)
			if (yytext[i] > 32)
				break;
		for (j = i; yytext[j]; j++)
			if (yytext[j] <= 32)
				break;
		for (; yytext[j]; j++)
			if (yytext[j] > 32)
				break;
		for (k = j; yytext[k]; k++)
			if (yytext[k] <= 32)
				break;
		if (j == k)
			return;
		s = (char*)emalloc(k-j+1);
		strncpy(s, yytext+j, k-j);
		s[k-j] = '\0';
		if (!strncmp(yytext+i, "name:", 5))
		{	sp->name = s;
		}
		else if (!strncmp(yytext+i, "location:", 9))
		{	if (s[strlen(s)-1] == '/')
				s[strlen(s)-1] = '\0';
			sp->URL = s;
		}
		else if (!strncmp(yytext+i, "executable:", 11))
		{	sp->executable = s;
		}
		else if (!strncmp(yytext+i, "namespace:", 10))
		{	if (service)
			{	if (!sp->URI)
					sp->URI = s;
				sp->WSDL = s;
			}
			else
				sp->URI = s;
		}
		else if (!strncmp(yytext+i, "encoding:", 9))
		{	sp->encoding = s;
		}
		else if (!strncmp(yytext+i, "method-header-part:", 19))
		{	m = (Method*)emalloc(sizeof(Method));
			m->name = s;
			m->mess = HDRIN | HDROUT;
			m->part = NULL;
			m->next = sp->list;
			sp->list = m;
			for (j = k; yytext[j]; j++)
				if (yytext[j] > 32)
					break;
			for (k = j; yytext[k]; k++)
				if (yytext[k] <= 32)
					break;
			if (j == k)
				return;
			s = (char*)emalloc(k-j+1);
			strncpy(s, yytext+j, k-j);
			s[k-j] = '\0';
			m->part = s;
		}
		else if (!strncmp(yytext+i, "method-input-header-part:", 25))
		{	m = (Method*)emalloc(sizeof(Method));
			m->name = s;
			m->mess = HDRIN;
			m->part = NULL;
			m->next = sp->list;
			sp->list = m;
			for (j = k; yytext[j]; j++)
				if (yytext[j] > 32)
					break;
			for (k = j; yytext[k]; k++)
				if (yytext[k] <= 32)
					break;
			if (j == k)
				return;
			s = (char*)emalloc(k-j+1);
			strncpy(s, yytext+j, k-j);
			s[k-j] = '\0';
			m->part = s;
		}
		else if (!strncmp(yytext+i, "method-output-header-part:", 26))
		{	m = (Method*)emalloc(sizeof(Method));
			m->name = s;
			m->mess = HDROUT;
			m->part = NULL;
			m->next = sp->list;
			sp->list = m;
			for (j = k; yytext[j]; j++)
				if (yytext[j] > 32)
					break;
			for (k = j; yytext[k]; k++)
				if (yytext[k] <= 32)
					break;
			if (j == k)
				return;
			s = (char*)emalloc(k-j+1);
			strncpy(s, yytext+j, k-j);
			s[k-j] = '\0';
			m->part = s;
		}
	}
}

/*
	error_chr - lexical error in character constant. Return character 0 to
	allow parsing to continue
*/
static Token
error_chr()
{	lexerror("Ending-' missing in character constant");
	yylval.c = '\0';
	return CHR;
}

/*
	error_str - lexical error in string. Return empty string to allow
	parsing to continue
*/
static Token
error_str()
{	lexerror("Ending-\" missing in string");
	yylval.s = "";
	return STR;
}

/*
	Character conversion functions
*/
static int
convchar(int *p)
{	switch (yytext[(*p)++])
	{	case 'a':	return '\a';
		case 'b':	return '\b';
		case 'f':	return '\f';
		case 'n':	return '\n';
		case 'r':	return '\r';
		case 't':	return '\t';
		case 'v':	return '\v';
		case 'x':	return hexchar(p);
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':	(*p)--;
				return octchar(p);
		default:	return yytext[*p-1];
	}
}

static int
hexchar(int *p)
{	int i, d, c = 0;
	for (i = 0; isxdigit(d = yytext[*p]) && i < 2; i++)
	{	c = (c << 4) + (d <= '9' ? d - '0' : toupper(d) - '7');
		(*p)++;
	}
	return c;
}

static int
octchar(int *p)
{	int i, d, c = 0;
	for (i = 0; (d = yytext[*p]) >= '0' && d <= '7' && i < 3; i++)
	{	c = (c << 3) + d - '0';
		(*p)++;
	}
	return c;
}
