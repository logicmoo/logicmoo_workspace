/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
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

:- module(abstract_domain_fail, []).

:- use_module(checkers(abstract_domain)).

% DOMAIN: fail [true, fail, top]

% Domain:
% Result = fail -> the program always fails
% Result = true -> the program always succeeds
% Result = canfail -> the program can fail

abstract_domain:abstract_domain(fail).

abstract_domain:top(fail, top).

abstract_domain:bot(fail, bot).

abstract_domain:eval(fail, Expr, Value)  :- eval_fail(Expr, Value), !.

eval_fail((A,B),   Value) :- and_fail(A, B, Value), !.
eval_fail((A->B),  Value) :- and_fail(A, B, Value), !.
eval_fail((A*->B), Value) :- and_fail(A, B, Value), !.
eval_fail(\+ A,    Value) :- not_fail(A, Value), !.

not_fail(top,  top).
not_fail(true, fail).
not_fail(fail, true).
not_fail(bot,  bot).

% Abstract operations:

and_fail(top,  R, R).
and_fail(true, top,  true).
and_fail(true, fail, fail).
and_fail(true, true, true).
and_fail(true, bot,  bot).
and_fail(fail, _, fail).
and_fail(bot,  _, bot).

% Trusted properties:

abstract_domain:trusted_result(fail, Goal, Module, Result) :-
        trusted_result_fail(Goal, Module, Result).

trusted_result_fail(fail,                        _, fail).
trusted_result_fail(true,                        _, true).
trusted_result_fail(retractall(_),               _, true).
trusted_result_fail(!,                           _, true).
trusted_result_fail(assertz(_),                  _, true).
trusted_result_fail(asserta(_),                  _, true).
trusted_result_fail(nl,                          _, true).
trusted_result_fail(nl(_),                       _, true).
trusted_result_fail(write(_),                    _, true).
trusted_result_fail(portray_clause(_, _, _),     _, true).
trusted_result_fail(write_term(_, _),            _, true).
trusted_result_fail(format(_, _),                _, true).
trusted_result_fail(format(_, _, _),             _, true).
trusted_result_fail(retract(_),                  _, top).
trusted_result_fail(send(_, _),                  _, top).
trusted_result_fail(get(_, _, _),                _, top).
trusted_result_fail(atom_concat(_, _, _),        _, top).
trusted_result_fail(atom_codes(_, _, _),         _, top).
trusted_result_fail(atomic_list_concat(_, _),    _, top).
trusted_result_fail(atomic_list_concat(_, _, _), _, top).
trusted_result_fail(catch(_, _, _),              _, top).
trusted_result_fail((_>_),                       _, top).
trusted_result_fail((_=<_),                      _, top).
trusted_result_fail((_<_),                       _, top).
trusted_result_fail((_ =:= _),                   _, top).
trusted_result_fail((_ \== _),                   _, top).
trusted_result_fail((_ == _),                    _, top).
trusted_result_fail((_ =.. _),                   _, top).
trusted_result_fail(sleep(_),                    _, top).
trusted_result_fail(notrace(_),                  _, top).
trusted_result_fail(throw(_),                    _, top).
trusted_result_fail(current_prolog_flag(_, _),   _, top).
trusted_result_fail((_>=_),                      _, top).
trusted_result_fail((A is B),                    _, Result) :-
        ( ground(B) ->
          catch(( \+ (A is B) -> Result = fail
                ; ground(A) -> Result = true
                ; Result = top), _, Result = fail)
        ; Result = top
        ).
trusted_result_fail(functor(T, F, A),            _, Result) :-
        ( atom(F), integer(A), A >= 0 ->
          functor(T0, F, A),
          ( T0 \= T ->
            Result = fail
          ; ground(T) ->
            Result = true
          ; Result = top
          )
        ; nonvar(T) ->
          functor(T, F0, A0),
          ( \+ (F0 = F, A0 = A) ->
            Result = fail
          ; ground(F/A) ->
            Result = true
          ; Result = top
          )
        ; Result = top
        ).
trusted_result_fail(A=B, _, Result) :-
        ( A \= B ->
          Result = fail
        ; A == B ->
          Result = true
        ; Result = top
        ).
trusted_result_fail((A\=B), _, Result) :-
        ( A \= B ->
          Result = true
        ; A == B ->
          Result = fail
        ; Result = top
        ).
trusted_result_fail(nonvar(Var), _, Result) :-
        ( nonvar(Var) ->
          Result = true
        ; Result = top
        ).
trusted_result_fail(var(Var), _, Result) :-
        ( var(Var) ->
            Result = top
        ; Result = fail
        ).
trusted_result_fail(atom(Atom), _, Result) :-
        ( atom(Atom) ->
            Result = true
        ; var(Atom) ->
            Result = top
        ; Result = fail
        ).
trusted_result_fail(integer(Atom), _, Result) :-
        ( integer(Atom) ->
            Result = true
        ; var(Atom) ->
            Result = top
        ; Result = fail
        ).
