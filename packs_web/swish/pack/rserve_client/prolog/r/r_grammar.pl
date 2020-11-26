/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(r_grammar,
	  [ r_tokens//1,		% -Tokens
	    r_token//1,			% -Token
	    r_identifier/1		% +Name
	  ]).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).


/** <module> R parser primitives

@see https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Parser
*/

%%	r_tokens(-Tokens:list)// is nondet.
%
%	Get R tokens in a non-greedy fashion. Backtracking extracts more
%	tokens from the input. See r_token/1 for getting a single token.

r_tokens([]) --> [].
r_tokens([H|T]) -->
	r_token(H),
	r_tokens(T).

%%	r_identifier(+Atom) is semidet.
%
%	True if Atom is a valid R identifier name.

r_identifier(Atom) :-
	atom(Atom),
	atom_codes(Atom, Codes),
	phrase(r_token(identifier(Atom)), Codes), !.


%%	r_token(-Token)// is semidet.
%
%	Get an R token from the input.  Defined tokens are:
%
%	  - number(FloatOrInt)
%	  Used for numerical constants.
%	  - complex(FloatOrInt)
%	  Used for <number>i.
%	  - string(String)
%	  Used for quoted strings.
%	  - identifier(Atom)
%	  General identifier (includes function names)
%	  - constant(Atom)
%	  Reserved constants (e.g., =NULL=, =NA=, ...)
%	  - logical(Boolean)
%	  Used for =TRUE= and =FALSE=
%	  - keyword(Atom)
%	  Used for =if=, =else=, etc.
%	  - infix(Atom)
%	  Used for =|%...%|= infix operators
%	  - op(Atom)
%	  Used for +, -, *, etc.
%	  - punct(Atom)
%	  Used for the braces =|{[()]}|= and the comma (,)
%	  - comment(String)
%	  Used for # Comment lines.

r_token(Token) -->
	blanks,
	token(Token).

token(Token) --> r_number(N),         !, {number_token(N, Token)}.
token(Token) --> r_string(S),         !, {Token = string(S)}.
token(Token) --> r_identifier(Id),    !, {identifier_token(Id, Token)}.
token(Token) --> r_infix(Id),         !, {Token = infix(Id)}.
token(Token) --> r_operator(Id),      !, {Token = op(Id)}.
token(Token) --> r_punct(Id),         !, {Token = punct(Id)}.
token(Token) --> r_comment(Id),       !, {Token = comment(Id)}.

number_token(complex(I), complex(I)) :- !.
number_token(N, number(N)) :- !.

%%	r_number(-Number)// is semidet.

r_number(Number) -->
	r_basic_number(N),
	(   "L"
	->  { integer(N) -> Number = N; Number is integer(N) /*warning*/ }
	;   "i"
	->  { Number = complex(N) }
	;   { Number = N }
	).

r_basic_number(N) -->
	int_codes(I), !,
	(   dot,
	    digit(DF0),
	    digits(DF)
	->  {F = [0'., DF0|DF]}
	;   dot
	->  {F = `.0`}
	;   {F = []}
	),
	(   exp
	->  int_codes(DI),
	    {E=[0'e|DI]}
	;   {E = []}
	),
	{ append([I, F, E], Codes),
	  number_codes(N, Codes)
	}.
r_basic_number(N) -->
	dot, !,
	digit(DF0),
	digits(DF),
	{F = [0'., DF0|DF]},
	(   exp
	->  int_codes(DI),
	    {E=[0'e|DI]}
	;   {E = []}
	),
	{ append([`0`, F, E], Codes),
	  number_codes(N, Codes)
	}.
r_basic_number(N) --> "0x", !, xinteger(N).
r_basic_number(N) --> "0X", !, xinteger(N).


int_codes([C,D0|D]) -->
	sign(C), !,
	digit(D0),
	digits(D).
int_codes([D0|D]) -->
	digit(D0),
	digits(D).

sign(0'-) --> "-".
sign(0'+) --> "+".

dot --> ".".

exp --> "e".
exp --> "E".

%%	r_string(-String)// is semidet.

r_string(S) -->
	"\"",
	r_string_codes(C),
	"\"", !,
	{ string_codes(S, C) }.
r_string(S) -->
	"'",
	r_string_codes(C),
	"'", !,
	{ string_codes(S, C) }.


r_string_codes([]) --> [].
r_string_codes([H|T]) -->
	r_string_code(H),
	r_string_codes(T).

r_string_code(H) --> "\\", !, r_escape(H).
r_string_code(H) --> [H].

r_escape(H) --> [C], { r_escape(C, H) }, !.
r_escape(H) --> "x", !, xdigit(D1), xdigit(D2), {H is D1<<4 + D2}.
r_escape(H) --> "u", xdigits(4, H), !.
r_escape(H) --> "u{", xdigits(4, H), "}", !.
r_escape(H) --> "U", xdigits(8, H), !.
r_escape(H) --> "U{", xdigits(8, H), "}", !.
r_escape(H) --> digit(D1), {D1 =< 7}, !, odigits(2, D1, H).

xdigits(N, V) --> xdigits(N, 0, V).

xdigits(0, V, V) --> !.
xdigits(N, V0, V) -->
	xdigit(D),
	{ V1 is V0*16 + D,
	  N1 is N - 1
	},
	xdigits(N1, V1, V).

odigits(0, V, V) --> !.
odigits(N, V0, V) -->
	digit(D), {D =< 7}, !,
	{ V1 is V0*8 + D,
	  N1 is N - 1
	},
	odigits(N1, V1, V).
odigits(_, V, V) --> [].

r_escape(0'\', 0'\').
r_escape(0'\", 0'\").
r_escape(0'n, 0'\n).
r_escape(0'r, 0'\r).
r_escape(0't, 0'\t).
r_escape(0'b, 0'\b).
r_escape(0'a, 0'\a).
r_escape(0'f, 0'\f).
r_escape(0'v, 0'\v).
r_escape(0'\\, 0'\\).

%%	r_identifier(-Identifier)
%
%	Recognise an R identifier.  This   includes  reserved  terms and
%	constants. These are filtered later.

r_identifier(Identifier) -->
	r_identifier_code(C0),
	\+ { no_identifier_start(C0) },
	r_identifier_codes(L),
	{ \+ ( C0 == 0'., L = [C1|_], code_type(C1, digit) ),
          atom_codes(Identifier, [C0|L])
	}.

r_identifier_code(C) -->
	[C],
	{   code_type(C, alnum)
	->  true
	;   C == 0'_
	->  true
	;   C == 0'.
	}.

r_identifier_codes([H|T]) -->
	r_identifier_code(H), !,
	r_identifier_codes(T).
r_identifier_codes([]) --> [].

no_identifier_start(C) :- code_type(C, digit).
no_identifier_start(0'_).

%%	identifier_token(+Id, -Token) is det.

identifier_token(Id, Token) :-
	(   reserved_identifier(Id, Token)
	->  true
	;   sub_atom(Id, 0, _, _, '..'),
	    sub_atom(Id, 2, _, 0, After),
	    (	After == '.'
	    ->	true
	    ;	atom_number(After, N),
		integer(N)
	    )
	->  Token = reserved(Id)
	;   Token = identifier(Id)
	).

reserved_identifier('NULL',	     constant('NULL')).
reserved_identifier('NA',	     constant('NA')).
reserved_identifier('NA_integer_',   constant('NA_integer_')).
reserved_identifier('NA_real_',	     constant('NA_real_')).
reserved_identifier('NA_complex_',   constant('NA_complex_')).
reserved_identifier('NA_character_', constant('NA_character_')).
reserved_identifier('TRUE',	     logical(true)).
reserved_identifier('FALSE',	     logical(false)).
reserved_identifier(if,		     keyword(if)).
reserved_identifier(else,	     keyword(else)).
reserved_identifier(repeat,	     keyword(repeat)).
reserved_identifier(while,	     keyword(while)).
reserved_identifier(function,	     keyword(function)).
reserved_identifier(for,	     keyword(for)).
reserved_identifier(in,		     keyword(in)).
reserved_identifier(next,	     keyword(next)).
reserved_identifier(break,	     keyword(break)).
reserved_identifier('Inf',           number(Inf)) :- Inf is inf.
reserved_identifier('NaN',           number(NaN)) :- NaN is nan.

%%	r_infix(-Infix)//
%
%

r_infix(Id) -->
	"%", non_blanks_short(Chars), "%", !,
	{ append([`%`, Chars, `%`], All),
	  atom_codes(Id, All)
	}.

non_blanks_short([]) --> [].
non_blanks_short([H|T]) --> nonblank(H), non_blanks_short(T).


%%	r_operator(-Op)//
%

r_operator(->)	 --> "->".
r_operator(+)	 --> "+".
r_operator(-)	 --> "-".
r_operator(*)	 --> "*".
r_operator(/)	 --> "/".
r_operator('%%') --> "%%".
r_operator(^)	 --> "^".

r_operator(>=)	 --> ">=".
r_operator(>)	 --> ">".
r_operator(<=)	 --> "<=".
r_operator(<-)	 --> "<-".
r_operator(<)	 --> "<".
r_operator(==)	 --> "==".
r_operator('!=') --> "!=".

r_operator(!)	 --> "!".
r_operator(&)	 --> "&".
r_operator('|')	 --> "|".
r_operator(~)	 --> "~".
r_operator($)	 --> "$".
r_operator(:)	 --> ":".
r_operator(=)	 --> "=".

r_punct('(') --> "(".
r_punct(')') --> ")".
r_punct('{') --> "{".
r_punct('}') --> "}".
r_punct('[') --> "[".
r_punct(']') --> "]".
r_punct(',') --> ",".

r_comment(String) -->
	"#", string(Codes), eol, !,
	{ string_codes(String, Codes) }.

eol --> "\r".
eol --> "\n".
eol --> eos.
