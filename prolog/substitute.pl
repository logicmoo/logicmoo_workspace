/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

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

:- module(substitute,
	  [substitute/3,
	   substitute_value/4,
	   substitute_values/3,
	   is_subterm/2]).

:- meta_predicate substitute(2, ?, ?).
substitute(Comp, Term0, Term) :-
    ( call(Comp, Term0, Subs)
    ->Term = Subs
    ; compound(Term0)
    ->functor(Term0, F, A),
      functor(Term,  F, A),
      substitute(1, Comp, Term0, Term)
    ; Term = Term0
    ).

substitute(N, Comp, Term0, Term) :-
    arg(N, Term0, Arg0),
    !,
    substitute(Comp, Arg0, Arg),
    arg(N, Term, Arg),
    succ(N, N1),
    substitute(N1, Comp, Term0, Term).
substitute(_, _, _, _).

substitute_one(Value, Var, Term, Var) :-
    Value==Term.

substitute_value(Value, Subs, Term0, Term) :-
    substitute(substitute_one(Value, Subs), Term0, Term).

unpair_eq(V=S, V, S).

substitute_values(Pairs, Term0, Term) :-
    maplist(unpair_eq, Pairs, Values, Subss),
    foldl(substitute_value, Values, Subss, Term0, Term).

is_subterm(SubTerm, Term) :-
    substitute_value(SubTerm, Var, Term, Term1),
    occurrences_of_var(Var, Term1, N),
    !,
    N > 0.
