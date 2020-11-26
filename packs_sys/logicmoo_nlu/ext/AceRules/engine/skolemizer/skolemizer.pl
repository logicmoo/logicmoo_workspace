% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(skolemize, [
			  skolemize/2,         % +Functor, +Term
			  skolemize_fresh/2,   % +Functor, +Term
			  reset_skolemizer/1,  % +Functor
			  reset_skolemizer/0   %
			 ]).

/** <module> Skolemizer

Unifies all variables of a term with skolem-constants. These skolem-constants have the
form f(c) where f is a user-defined functor and c is an integer. Each variable gets its
own integer. There is a counter for each functor.

@author Tobias Kuhn
@version 2007-02-08
*/


%% varnum(-Functor, -VarNum)
%
% Stores the number that will be used for the next variable as the number of the
% skolem-constant.

:- dynamic(varnum/2).


%% skolemize(+Functor, +Term)
%
% Unifies all variables in Term with skolem-constants using Functor. The counter is not
% reset.
%
% Example:
%
% ==
% ?- Term = p(X, a, Y, X), skolemize(foo, Term).
% Term = p(foo(0), a, foo(1), foo(0))
% ==

skolemize(Functor, Term) :-
    atom(Functor),
	varnum(Functor, N1),
	!,
	retractall(varnum(Functor, _)),
	numbervars(Term, N1, N2, [functor_name(Functor)]),
	assert(varnum(Functor, N2)).

skolemize(Functor, Term) :-
    reset_skolemizer(Functor),
    skolemize(Functor, Term).


%% skolemize_fresh(+Functor, +Term)
%
% Unifies all variables in Term with skolem-constants using Functor. The counter is
% reset first.

skolemize_fresh(Functor, Term) :-
    reset_skolemizer(Functor),
    skolemize(Functor, Term).


%% reset_skolemizer(+Functor)
%
% Resets the counter to 0 for upcoming skolemization using Functor.

reset_skolemizer(Functor) :-
    atom(Functor),
    retractall(varnum(Functor, _)),
    assert(varnum(Functor, 0)).


%% reset_skolemizer
%
% Resets the counter to 0 for upcoming skolemization using any functor.

reset_skolemizer :-
    retractall(varnum(_, _)).
