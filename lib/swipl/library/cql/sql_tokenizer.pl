/*  Part of SWI-Prolog

    Author:        Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, Mike Elston, Matt Lilley
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

/*  PostgreSQL is a trademark of the PostgreSQL Global Development Group.
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries. SQLite is a registered trademark of Hipp, Wyrick & Company,
    Inc in the United States. All other trademarks or registered trademarks
    are the property of their respective owners.
*/

:-module(sql_tokenizer,
         [sql_tokens//1]).

:-use_module(library(cql/cql), [sql_gripe/3]).

% No codes -> no tokens
sql_tokens([], [], []):- !.
sql_tokens([Token|Tokens])-->
        optional_whitespace,
        sql_token(Token),
        optional_whitespace,
        sql_tokens(Tokens).

optional_whitespace-->
	char_in(_, " \t\n\r"),
        !,
        optional_whitespace.
optional_whitespace--> [].

%:- meta_predicate read_until(//,*,?,?).

:- if(current_prolog_flag(double_quotes,string)).
:- multifile check:valid_string_goal/1.
check:valid_string_goal(sql_tokenizer:read_until(S,_,_,_)) :- string(S).

read_until(Terminator, Codes) -->
	{ string_codes(Terminator, List)
	},
	read_until_(List, Codes).
:- else.
read_until(Terminator, Codes) -->
	read_until_(Terminator, Codes).
:- endif.

read_until_(_, [], [], []).
read_until_(Terminator, [])-->
        Terminator, !.
read_until_(Terminator, [Code|Codes])-->
        [Code],
        read_until_(Terminator, Codes).


numeric_codes([Code|Codes])-->
	char_in(Code, "0123456789."),
        !,
        numeric_codes(Codes).
numeric_codes([])--> [], !.


%%	char_in(+Code, +Set) is semidet.
%
%	True when Code appears in Set.   Set  is a double-quoted string.
%	Calls code_in_set/2 to deal with the SWI-6/7 compatibility.

char_in(Code, Set) -->
	[Code],
	{ code_in_set(Code, Set) }.

:- if(current_prolog_flag(double_quotes,string)).
:- multifile check:valid_string_goal/1.

check:valid_string_goal(sql_tokenizer:char_in(_,S,_,_)) :- string(S).
check:valid_string_goal(sql_tokenizer:code_in_set(_,S)) :- string(S).

code_in_set(Code, Set) :-
	string_code(_, Set, Code), !.

:- else.

code_in_set(Code, Set) :-
	memberchk(Code, Set).

:- endif.

quoted_literal([39|Codes], [39, 39|In], Out):-
        !,
        quoted_literal(Codes, In, Out).
quoted_literal([], [39|In], In):-!.
quoted_literal([Code|Codes])-->
        [Code],
        quoted_literal(Codes).


sql_token(comment(long, Codes))-->
        "/*", !, read_until("*/", Codes).

sql_token(comment(short, Codes))-->
        "--", !, read_until("\n", Codes).

% All of these are a token of their own.
sql_token('=')--> "=", !.
sql_token('<>')--> "!=", !, {sql_gripe(1,'The not-equals operator in SQL is <> and not !=', [])}.
sql_token('<>')--> "! =", !, {sql_gripe(1,'The not-equals operator in SQL is <> and not ! =', [])}.
% There are some WEIRD things that people put in views...
sql_token('>=')--> "! <", !, {sql_gripe(1,'The greater-than-or-equal-to operator in SQL is >= and not ! <', [])}.
sql_token('>=')--> "!<", !, {sql_gripe(1,'The greater-than-or-equal-to operator in SQL is >= and not !<', [])}.
sql_token('<=')--> "!>", !, {sql_gripe(1,'The less-than-or-equal-to operator in SQL is <= and not !>', [])}.
sql_token('<=')--> "! >", !, {sql_gripe(1,'The less-than-or-equal-to operator in SQL is <= and not ! >', [])}.
sql_token('<>')--> "<>", !.
sql_token('>=')--> ">=", !.
sql_token('<=')--> "<=", !.
sql_token('<')--> "<", !.
sql_token('>')--> ">", !.
sql_token('.')--> ".", !.
sql_token(',')--> ",", !.
sql_token('(')--> "(", !.
sql_token(')')--> ")", !.
sql_token('/')--> "/", !.
sql_token('+')--> "+", !.
sql_token('*')--> "*", !.
sql_token('-')--> "-", !.
sql_token('{')--> "{", !.
sql_token('}')--> "}", !.


sql_token(literal(Literal, string))-->
        "'",
        !,
        quoted_literal(Codes),
        {atom_codes(Literal, Codes)}.


sql_token(literal(Literal, identifier))-->
        "\"",
        !,
        read_until("\"", Codes),
        {atom_codes(Literal, Codes)}.

% This should return numeric/2 instead of decimal/2, according to http://msdn.microsoft.com/en-us/library/ms187746
% But it also says they are functionally equivalent
sql_token(literal(Literal, Type))-->
	char_in(Code, "0123456789"),
        !,
        numeric_codes(Codes),
        {number_codes(Literal, [Code|Codes])},
        {(integer(Literal)->
            length(Codes, L),
            LL is L+1,
            ( LL > 10->
                Type = decimal(LL, 0)
            ; otherwise->
                Type = int(LL)
            )
         ; Code == 0'0, Codes = [0'.|_]->
            % 0.00 is numeric(2,2) not (3,2). I suppose the leading 0 is just a placeholder?
            length(Codes, L),
            P is L - 1,
            Type = decimal(P, P)
         ; otherwise->
            nth1(P, Codes, 0'.), %'

            length(Codes, L),
            S is L - P,
            PP is P + S,
            Type = decimal(PP, S)
         )}.

sql_token(literal(Literal, identifier))-->
        "[",
        !,
        read_until("]", Codes),
        {atom_codes(Literal, Codes)}.

sql_token(Token)-->
        !,
        sql_token_1(Codes),
        {atom_codes(Token, Codes)}.

sql_token_1([], [], []):- !.
% Any of these codes should end the current token. This allows us to correctly
% tokens 3+4 as the sum of two values rather than a single token
sql_token_1([], [Terminator|Codes], [Terminator|Codes]):-
        code_in_set(Terminator, ".,()*+-/<=> \t\n\r"), !.

% Everything else goes into the token
sql_token_1([Code|Codes])-->
        [Code],
        sql_token_1(Codes).
