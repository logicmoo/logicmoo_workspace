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
