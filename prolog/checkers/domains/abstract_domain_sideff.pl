/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(abstract_domain_sideff, []).

:- use_module(checkers(abstract_domain)).

% DOMAIN: Side-effects [free, soft, hard, top]
abstract_domain:abstract_domain(sideff).

abstract_domain:top(sideff, top).

abstract_domain:bot(sideff, bot).

abstract_domain:eval(sideff, (A,B), Value)  :- and_sideff(A, B, Value), !.
abstract_domain:eval(sideff, (A->B), Value) :- and_sideff(A, B, Value), !.
abstract_domain:eval(sideff, (A*->B), Value) :- and_sideff(A, B, Value), !.
abstract_domain:eval(sideff, \+ A, A).

% Abstract operations:

and_sideff(top,  R,    R).
and_sideff(free, top,  free).
and_sideff(free, free, free).
and_sideff(free, soft, soft).
and_sideff(free, hard, hard).
and_sideff(free, bot,  bot).
and_sideff(soft, top,  soft).
and_sideff(soft, free, soft).
and_sideff(soft, soft, soft).
and_sideff(soft, hard, hard).
and_sideff(soft, bot,  bot).
and_sideff(hard, _,    hard).
and_sideff(bot,  _,    bot).

% Trusted properties:

abstract_domain:trusted_result(sideff, Goal, Module, Result) :-
        trusted_result_sideff(Goal, Module, Result).

trusted_result_sideff(fail,                        _, free).
trusted_result_sideff(true,                        _, free).
trusted_result_sideff(!,                           _, free).
trusted_result_sideff(atom_concat(_, _, _),        _, free).
trusted_result_sideff(atom_codes(_, _, _),         _, free).
trusted_result_sideff(atomic_list_concat(_, _),    _, free).
trusted_result_sideff(atomic_list_concat(_, _, _), _, free).
trusted_result_sideff(catch(_, _, _),              _, free).
trusted_result_sideff((_>=_),                      _, free).
trusted_result_sideff((_>_),                       _, free).
trusted_result_sideff((_=<_),                      _, free).
trusted_result_sideff((_<_),                       _, free).
trusted_result_sideff((_ =:= _),                   _, free).
trusted_result_sideff((_ == _),                    _, free).
trusted_result_sideff((_ \== _),                   _, free).
trusted_result_sideff((_ =.. _),                   _, free).
trusted_result_sideff((_ is _),                    _, free).
trusted_result_sideff((_ = _),                     _, free).
trusted_result_sideff(nonvar(_),                   _, free).
trusted_result_sideff(var(_),                      _, free).
trusted_result_sideff(atom(_),                     _, free).
trusted_result_sideff(integer(_),                  _, free).
trusted_result_sideff(send(_, _),                  _, hard).
trusted_result_sideff(get(_, _, _),                _, hard).
trusted_result_sideff(nl,                          _, soft).
trusted_result_sideff(nl(_),                       _, soft).
trusted_result_sideff(portray_clause(_, _, _),     _, soft).
trusted_result_sideff(write(_),                    _, soft).
trusted_result_sideff(write_term(_, _),            _, soft).
trusted_result_sideff(format(_, _),                _, soft).
trusted_result_sideff(write(_, _),                 _, hard).
trusted_result_sideff(format(_, _, _),             _, hard).
trusted_result_sideff(retractall(_),               _, hard).
trusted_result_sideff(assertz(_),                  _, hard).
trusted_result_sideff(asserta(_),                  _, hard).
trusted_result_sideff(functor(_, _, _),            _, free).
trusted_result_sideff(sleep(_),                    _, free).
trusted_result_sideff(notrace(_),                  _, free).
trusted_result_sideff(throw(_),                    _, hard).
trusted_result_sideff(current_prolog_flag(_, _),   _, free).
