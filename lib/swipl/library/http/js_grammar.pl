/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2016, VU University Amsterdam
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

:- module(javascript_grammar,
          [ js_token//1
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).     % syntax_error//1
:- set_prolog_flag(double_quotes, codes).

/** <module> JavaScript grammar

This file provides a tokenizer for   JavaScript  (EcmaScript). This code
supports  the  quasi  quotation   syntax    =javascript=,   defined   in
library(http/js_write).

@see    http://tomcopeland.blogs.com/EcmaScript.html is used for the
        high-level syntax.
@see    http://www.ecma-international.org/ecma-262/5.1/ is used for
        implementing the tokenization code.
*/

%!  js_token(-TokenType)//
%
%   Matches and classifies the next JavaScript token.

js_token(Type) -->
    token(Type).

%!  token(-Type) is semidet.
%
%   Get the next token from the   input. Fails when encountering the
%   end of the input.
%
%   @error syntax_error(Culprit)

token(comment)        --> comment, !.
token(string)         --> string_literal, !.
token(number)         --> numeric_literal, !.
token(identifier(Id)) --> identifier_name(Id), !.
token(regex)          --> regex_literal, !.
token(ws)             --> blank, !, blanks.
token(punct(Char))    --> [Code], { char_code(Char, Code) }.

%!  comment// is semidet.

comment -->
    "/*",
    !,
    (   string(_), "*/"
    ->  []
    ;   syntax_error(eof_in_comment)
    ).
comment -->
    "//",
    !,
    (   string(_), eol
    ->  []
    ;   string(_), eof
    ->  []
    ).


%!  string_literal// is semidet.
%
%   Matches a string literal

string_literal -->
    "\"",
    !,
    (   q_codes, "\""
    ->  []
    ;   syntax_error(eof_in_string)
    ).
string_literal -->
    "\'",
    !,
    (   q_codes, "\'"
    ->  []
    ;   syntax_error(eof_in_string)
    ).


%!  numeric_literal//
%
%   Matches JavaScript notion of a numeric constant

numeric_literal -->
    (   decimal_literal
    ->  []
    ;   hex_integer
    ),
    (   (   decimal_digit
        ;   js_id_start(_)
        )
    ->  syntax_error(js(illegal_number))
    ;   []
    ).

decimal_literal -->
    decimal_integer, ".", opt_decimal_digits, opt_exponent.
decimal_literal -->
    ".", decimal_digits, opt_exponent.
decimal_literal -->
    decimal_integer,
    opt_exponent.

decimal_integer -->
    "0",
    !.
decimal_integer -->
    non_zero_digit, opt_decimal_digits.

decimal_digits -->
    decimal_digit,
    !,
    opt_decimal_digits.

opt_decimal_digits -->
    decimal_digit,
    !,
    opt_decimal_digits.
opt_decimal_digits -->
    [].

decimal_digit --> [C], { code_type(C, digit) }.
non_zero_digit --> [C], { code_type(C, digit), C \== 0'0 }.

opt_exponent -->
    exponent,
    !.
opt_exponent -->
    [].

exponent -->
    exponent_indictor,
    signed_integer.

exponent_indictor --> "e", !.
exponent_indictor --> "E".

signed_integer --> "+", !, decimal_digits.
signed_integer --> "-", !, decimal_digits.
signed_integer -->         decimal_digits.

hex_integer --> "0", x, hex_digit, hex_digits.

x --> "x".
x --> "X".


%!  regex_literal// is semidet.
%
%   Matches regex expression /.../flags

regex_literal -->
    "/", regex_body, "/", !, regex_flags.

regex_body -->
    regex_first_char,
    regex_chars.

regex_chars --> regex_char, !, regex_chars.
regex_chars --> [].

regex_first_char -->
    regex_non_terminator(C),
    !,
    { \+ memberchk(C, "*\\/[") }.
regex_first_char -->
    regex_backslash_sequence.
regex_first_char -->
    regex_class.

regex_char -->
    regex_non_terminator(C),
    !,
    { \+ memberchk(C, "\\/[") }.
regex_char -->
    regex_backslash_sequence.
regex_char -->
    regex_class.

regex_backslash_sequence -->
    "\\", !, regex_non_terminator(_).

regex_class -->
    "[", regex_class_chars, "]".

regex_class_chars --> regex_class_char, !, regex_class_chars.
regex_class_chars --> "".

regex_class_char -->
    regex_non_terminator(C),
    !,
    { \+ memberchk(C, "]\\") }.

regex_non_terminator(_) -->
    eol, !, {fail}.
regex_non_terminator(C) -->
    source_char(C).

regex_flags -->
    js_id_conts(_).

source_char(C) -->
    [C].


%!  q_codes//
%
%   Shortest list of quoted characters.

q_codes --> [] ; q_code, q_codes.

q_code --> "\\", !, char_esc.
q_code --> eol, !, {fail}.
q_code --> [_].

char_esc --> single_escape_char, !.
char_esc --> "x", !, hex_digit, hex_digit.
char_esc --> "u", !, hex_digit, hex_digit, hex_digit, hex_digit.
char_esc --> eol, !.

hex_digits --> hex_digit, !, hex_digits.
hex_digits --> [].

hex_digit --> [C], {code_type(C, xdigit(_))}.

single_escape_char --> "'".
single_escape_char --> "\"".
single_escape_char --> "\\".
single_escape_char --> "b".
single_escape_char --> "f".
single_escape_char --> "n".
single_escape_char --> "r".
single_escape_char --> "t".
single_escape_char --> "v".

eol --> "\r\n", !.
eol --> "\n", !.
eol --> "\r".

eof -->
    \+ [_].


%       js_identifier classification. Now  based  on   Prolog.  This  is
%       pretty close, but I'm afraid there are corner cases.

identifier_name(Id) -->
    js_id_start(C0),
    !,
    js_id_conts(Rest),
    { atom_codes(Id, [C0|Rest]),
      (   keyword(Id)
      ->  fail, syntax_error(reserved(Id))
      ;   true
      )
    }.


js_id_start(C) --> [C], {js_id_start(C)}.

js_id_start(C) :- code_type(C, prolog_var_start), !.
js_id_start(C) :- code_type(C, prolog_atom_start), !.
js_id_start(0'$).

js_id_conts([H|T]) --> js_id_cont(H), !, js_id_conts(T).
js_id_conts([]) --> [].

js_id_cont(C) --> [C], {js_id_cont(C)}.

js_id_cont(C) :- code_type(C, prolog_identifier_continue), !.
js_id_cont(0'$) :- !.


keyword(break).                         % standard keywords
keyword(do).
keyword(instanceof).
keyword(typeof).
keyword(case).
keyword(else).
keyword(new).
keyword(var).
keyword(catch).
keyword(finally).
keyword(return).
keyword(void).
keyword(continue).
keyword(for).
keyword(switch).
keyword(while).
keyword(debugger).
keyword(function).
keyword(this).
keyword(with).
keyword(default).
keyword(if).
keyword(throw).
keyword(delete).
keyword(in).
keyword(try).

keyword(class).                         % reserved keywords
keyword(enum).
keyword(extends).
keyword(super).
keyword(const).
keyword(export).
keyword(import).

keyword(implements).                    % future reserved keywords
keyword(let).
keyword(private).
keyword(public).
keyword(yield).
keyword(interface).
keyword(package).
keyword(protected).
keyword(static).
