/*
	init2.c
	Symbol table initialization

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

typedef	struct Keyword
{ char *s;	/* name */
  Token t;	/* token */
} Keyword;

static Keyword keywords[] =
{	"auto",		AUTO,
	"bool",		BOOL,
	"break",	BREAK,
	"case",		CASE,
	"char",		CHAR,
	"class",	CLASS,
	"const",	CONST,
	"continue",	CONTINUE,
	"default",	DEFAULT,
	"do",		DO,
	"double",	DOUBLE,
	"else",		ELSE,
	"enum",		ENUM,
	"extern",	EXTERN,
	"false",	CFALSE,
	"float",	FLOAT,
	"for",		FOR,
	"goto",		GOTO,
	"if",		IF,
	"inline",	INLINE,
	"int",		INT,
	"long",		LONG,
	"LONG64",	LLONG,
	"namespace",	NAMESPACE,
	"operator",	OPERATOR,
	"private",	PRIVATE,
	"protected",	PROTECTED,
	"public",	PUBLIC,
	"register",	REGISTER,
	"return",	RETURN,
	"short",	SHORT,
	"signed",	SIGNED,
	"size_t",	SIZE,
	"sizeof",	SIZEOF,
	"static",	STATIC,
	"struct",	STRUCT,
	"switch",	SWITCH,
	"time_t",	TIME,
	"true",		CTRUE,
	"typedef",	TYPEDEF,
	"ULONG64",	ULLONG,
	"union",	UNION,
	"unsigned",	UNSIGNED,
	"using",	USING,
	"virtual",	VIRTUAL,
	"void",		VOID,
	"volatile",	VOLATILE,
	"wchar_t",	WCHAR,
	"while",	WHILE,

	"operator=",	NONE,
	"operator+=",	NONE,
	"operator-=",	NONE,
	"operator*=",	NONE,
	"operator/=",	NONE,
	"operator%=",	NONE,
	"operator&=",	NONE,
	"operator^=",	NONE,
	"operator|=",	NONE,
	"operator<<=",	NONE,
	"operator>>=",	NONE,
	"operator||",	NONE,
	"operator&&",	NONE,
	"operator|",	NONE,
	"operator^",	NONE,
	"operator&",	NONE,
	"operator=",	NONE,
	"operator!=",	NONE,
	"operator<",	NONE,
	"operator<=",	NONE,
	"operator>",	NONE,
	"operator>=",	NONE,
	"operator<<",	NONE,
	"operator>>",	NONE,
	"operator+",	NONE,
	"operator-",	NONE,
	"operator*",	NONE,
	"operator/",	NONE,
	"operator%",	NONE,
	"operator++",	NONE,
	"operator--",	NONE,
	"operator[]",	NONE,
	"operator()",	NONE,

	"mustUnderstand",	MUSTUNDERSTAND,

	"SOAP_ENV__Header",	ID,
	"dummy",		ID,
	"soap_header",		ID,

	"SOAP_ENV__Fault",	ID,
	"faultcode",		ID,
	"faultstring",		ID,
	"faultactor",		ID,
	"detail",		ID,

	"/**/",		NONE,

	0,		0
};

/*
init - initialize symbol table with predefined keywords
*/
init()
{	struct	Keyword *k;
	for (k = keywords; k->s; k++)
		install(k->s, k->t);
}
