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

:- module(abstract_domain_product, []).

:- use_module(checkers(abstract_domain)).

% DOMAIN: Cartesian product of two domains D1xD2
% WARNING: do not use this predicate to list the available domains,
% it will loops infinitely --EMM
abstract_domain:abstract_domain(product(D1, D2)) :-
        abstract_domain:abstract_domain(D1),
        abstract_domain:abstract_domain(D2).

abstract_domain:top(product(D1, D2), Top) :-
        abstract_domain:top(D1, Top1),
        abstract_domain:top(D2, Top2),
        product(Top1, Top2, Top).

abstract_domain:bot(product(D1, D2), Bot) :-
        abstract_domain:bot(D1, Bot1),
        abstract_domain:bot(D2, Bot2),
        product(Bot1, Bot2, Bot).

:- multifile eval_product/4.

abstract_domain:eval(product(D1, D2), Expr, Value) :-
        ( eval_product(D1, D2, Expr, Value) -> true
        ; eval_product_default(D1, D2, Expr, Value)
        ).

eval_product_default(D1, D2, Expr, Value) :-
        split_expr(Expr, Expr1, Expr2),
        abstract_domain:eval(D1, Expr1, Value1),
        abstract_domain:eval(D2, Expr2, Value2),
        product(Value1, Value2, Value).

% TODO: Generalize this to any operator
split_expr((A,B),   (A1, B1),  (A2, B2))  :- product(A1, A2, A), product(B1, B2, B).
split_expr((A->B),  (A1->B1),  (A2->B2))  :- product(A1, A2, A), product(B1, B2, B).
split_expr((A*->B), (A1*->B1), (A2*->B2)) :- product(A1, A2, A), product(B1, B2, B).
split_expr((\+ A),  (\+ A1),   (\+ A2))   :- product(A1, A2, A).

product(A1, A2, product(A1, A2)).

abstract_domain:trusted_result(product(D1, D2), Goal, Module, Result) :-
        abstract_domain:trusted_result(D1, Goal, Module, Result1),
        abstract_domain:trusted_result(D2, Goal, Module, Result2),
        product(Result1, Result2, Result).
