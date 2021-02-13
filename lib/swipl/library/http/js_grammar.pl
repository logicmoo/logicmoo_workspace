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
    `AS IS` AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
library(http/jsg_write).

@see    http://tomcopeland.blogs.com/EcmaScript.html is used for the
        high-level syntax.
@see    http://www.ecma-international.org/ecma-262/5.1/ is used for
        implementing the tokenization code.
*/

%!  jsg_token(-TokenType)//
%
%   Matches and classifies the next JavaScript jsg_token.

js_token(Type) -->
    jsg_token(Type).

%!  jsg_token(-Type) is semidet.
%
%   Get the next jsg_token from the   input. Fails when encountering the
%   end of the input.
%
%   @error syntax_error(Culprit)

jsg_token(comment)        --> jsg_comment, !.
jsg_token(string)         --> jsg_string_literal, !.
jsg_token(number)         --> jsg_numeric_literal, !.
jsg_token(identifier(Id)) --> jsg_identifier_name(Id), !.
jsg_token(regex)          --> jsg_regex_literal, !.
jsg_token(ws)             --> dcg_basics:blank, !, dcg_basics:blanks.
jsg_token(punct(Char))    --> jsg_source_char(Code), { char_code(Char, Code) }.

%!  jsg_comment// is semidet.

jsg_comment -->
    `/*`,
    !,
    (   dcg_basics:string(_), `*/`
    ->  []
    ;   syntax_error(eof_in_comment)
    ).
jsg_comment -->
    `//`,
    !,
    (   dcg_basics:string(_), jsg_eol
    ->  []
    ;   dcg_basics:string(_), jsg_eof
    ->  []
    ).


%!  jsg_string_literal// is semidet.
%
%   Matches a string literal

jsg_string_literal -->
    `"`,
    !,
    (   jsg_q_codes, `"`
    ->  []
    ;   syntax_error(eof_in_string)
    ).
jsg_string_literal -->
    `'`,
    !,
    (   jsg_q_codes, `'`
    ->  []
    ;   syntax_error(eof_in_string)
    ).


%!  jsg_numeric_literal//
%
%   Matches JavaScript notion of a numeric constant

jsg_numeric_literal -->
    (   jsg_decimal_literal
    ->  []
    ;   jsg_hex_integer
    ),
    (   (   jsg_decimal_digit
        ;   jsg_id_start(_)
        )
    ->  syntax_error(js(illegal_number))
    ;   []
    ).

jsg_decimal_literal -->
    jsg_decimal_integer, `.`, jsg_opt_decimal_digits, jsg_opt_jsg_exponent.
jsg_decimal_literal -->
    `.`, jsg_decimal_digits, jsg_opt_jsg_exponent.
jsg_decimal_literal -->
    jsg_decimal_integer,
    jsg_opt_jsg_exponent.

jsg_decimal_integer -->
    `0`,
    !.
jsg_decimal_integer -->
    jsg_non_zero_digit, jsg_opt_decimal_digits.

jsg_decimal_digits -->
    jsg_decimal_digit,
    !,
    jsg_opt_decimal_digits.

jsg_opt_decimal_digits -->
    jsg_decimal_digit,
    !,
    jsg_opt_decimal_digits.
jsg_opt_decimal_digits -->
    [].

jsg_decimal_digit --> jsg_source_char(C), { code_type(C, digit) }.
jsg_non_zero_digit --> jsg_source_char(C), { code_type(C, digit), [C] \== `0` }.

jsg_opt_jsg_exponent -->
    jsg_exponent,
    !.
jsg_opt_jsg_exponent -->
    [].

jsg_exponent -->
    jsg_exponent_indictor,
    jsg_signed_integer.

jsg_exponent_indictor --> `e`, !.
jsg_exponent_indictor --> `E`.

jsg_signed_integer --> `+`, !, jsg_decimal_digits.
jsg_signed_integer --> `-`, !, jsg_decimal_digits.
jsg_signed_integer -->         jsg_decimal_digits.

jsg_hex_integer --> `0`, jsg_x, jsg_hex_digit, jsg_hex_digits.

jsg_x --> `x`.
jsg_x --> `X`.


%!  jsg_regex_literal// is semidet.
%
%   Matches regex expression /.../flags

jsg_regex_literal -->
    `/`, jsg_regex_body, `/`, !, jsg_regex_flags.

jsg_regex_body -->
    jsg_regex_first_char,
    jsg_regex_chars.

jsg_regex_chars --> jsg_regex_char, !, jsg_regex_chars.
jsg_regex_chars --> [].

jsg_regex_first_char -->
    jsg_regex_non_terminator(C),
    !,
    { \+ memberchk(C, `*\\/[`) }.
jsg_regex_first_char -->
    jsg_regex_backslash_sequence.
jsg_regex_first_char -->
    jsg_regex_class.

jsg_regex_char -->
    jsg_regex_non_terminator(C),
    !,
    { \+ memberchk(C, `\\/[`) }.
jsg_regex_char -->
    jsg_regex_backslash_sequence.
jsg_regex_char -->
    jsg_regex_class.

jsg_regex_backslash_sequence -->
    `\\`, !, jsg_regex_non_terminator(_).

jsg_regex_class -->
    `[`, jsg_regex_class_chars, `]`.

jsg_regex_class_chars --> jsg_regex_class_char, !, jsg_regex_class_chars.
jsg_regex_class_chars --> ``.

jsg_regex_class_char -->
    jsg_regex_non_terminator(C),
    !,
    { \+ memberchk(C, `]\\`) }.

jsg_regex_non_terminator(_) -->
    jsg_eol, !, {fail}.
jsg_regex_non_terminator(C) -->
    jsg_source_char(C).

jsg_regex_flags -->
    jsg_id_conts(_).

jsg_source_char(CC) -->
    [C],{C>=0, !, CC=C}.


%!  jsg_q_codes//
%
%   Shortest list of quoted characters.

jsg_q_codes --> [] ; jsg_q_code, jsg_q_codes.

jsg_q_code --> `\\`, !, jsg_char_esc.
jsg_q_code --> jsg_eol, !, {fail}.
jsg_q_code --> jsg_source_char(_).

jsg_char_esc --> jsg_single_escape_char, !.
jsg_char_esc --> `x`, !, jsg_hex_digit, jsg_hex_digit.
jsg_char_esc --> `u`, !, jsg_hex_digit, jsg_hex_digit, jsg_hex_digit, jsg_hex_digit.
jsg_char_esc --> jsg_eol, !.

jsg_hex_digits --> jsg_hex_digit, !, jsg_hex_digits.
jsg_hex_digits --> [].

jsg_hex_digit --> jsg_source_char(C), {code_type(C, xdigit(_))}.

jsg_single_escape_char --> `'`.
jsg_single_escape_char --> `"`.
jsg_single_escape_char --> `\\`.
jsg_single_escape_char --> `b`.
jsg_single_escape_char --> `f`.
jsg_single_escape_char --> `n`.
jsg_single_escape_char --> `r`.
jsg_single_escape_char --> `t`.
jsg_single_escape_char --> `v`.

jsg_eol --> `\r\n`, !.
jsg_eol --> `\n`, !.
jsg_eol --> `\r`.

jsg_eof -->
    \+ [_].


%       jsg_identifier classification. Now  based  on   Prolog.  This  is
%       pretty close, but I'm afraid there are corner cases.

jsg_identifier_name(Id) -->
    jsg_id_start(C0),
    !,
    jsg_id_conts(Rest),
    { atom_codes(Id, [C0|Rest]),
      (   jsg_keyword(Id)
      ->  fail, syntax_error(reserved(Id))
      ;   true
      )
    }.


jsg_id_start(C) --> jsg_source_char(C), {jsg_id_start(C)}.

jsg_id_start(C) :- code_type(C, prolog_var_start), !.
jsg_id_start(C) :- code_type(C, prolog_atom_start), !.
jsg_id_start(C) :- [C] == `$`,!.

jsg_id_conts([H|T]) --> jsg_id_cont(H), !, jsg_id_conts(T).
jsg_id_conts([]) --> [].

jsg_id_cont(C) --> jsg_source_char(C), {jsg_id_cont(C)}.

jsg_id_cont(C) :- code_type(C, prolog_identifier_continue), !.
jsg_id_cont(C) :- [C] == `$`,!.


jsg_keyword(break).                         % standard jsg_keywords
jsg_keyword(do).
jsg_keyword(instanceof).
jsg_keyword(typeof).
jsg_keyword(case).
jsg_keyword(else).
jsg_keyword(new).
jsg_keyword(var).
jsg_keyword(catch).
jsg_keyword(finally).
jsg_keyword(return).
jsg_keyword(void).
jsg_keyword(continue).
jsg_keyword(for).
jsg_keyword(switch).
jsg_keyword(while).
jsg_keyword(debugger).
jsg_keyword(function).
jsg_keyword(this).
jsg_keyword(with).
jsg_keyword(default).
jsg_keyword(if).
jsg_keyword(throw).
jsg_keyword(delete).
jsg_keyword(in).
jsg_keyword(try).

jsg_keyword(class).                         % reserved keywords
jsg_keyword(enum).
jsg_keyword(extends).
jsg_keyword(super).
jsg_keyword(const).
jsg_keyword(export).
jsg_keyword(import).

jsg_keyword(implements).                    % future reserved keywords
jsg_keyword(let).
jsg_keyword(private).
jsg_keyword(public).
jsg_keyword(yield).
jsg_keyword(interface).
jsg_keyword(package).
jsg_keyword(protected).
jsg_keyword(static).
