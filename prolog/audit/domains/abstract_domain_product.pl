/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(abstract_domain_product, []).

:- use_module(library(audit/abstract_domain)).

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
