/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Simple Planner Constraints
%%%%
%%%% - A dummy implementation of the constraint handling interface, that
%%%%   just evaluates constraints as soon as they are sufficiently
%%%%   instantiated.
%%%%
%%%%   Support for:
%%%%
%%%%   - Arithmetics: =:=/2, >/2, </2. >=/2, =</2, =\=/2
%%%% 
%%%%     =:=/2 is evaluated using is/2 as soon as one side is ground
%%%%     and the other a variable. If both sides are ground, it is
%%%% 	 evaluated using =:=/2. So arithmetic compound terms should
%%%% 	 only appear within the constraints, i.e. the planning rules
%%%%     should not bind variables to arithmetic compound terms.
%%%% 
%%%%   - Terms: \=/2 (non-unifiablility)
%%%%
%%%%   - Atom number conversion: atom_number
%%%%
%%%%   - Eager evaluation: call/1
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(planner_cs_simple, [reduce_cs/2,
			      compile_merge_cs/4,
			      make_true_cs/1,
			      project_cs/3]).

compile_merge_cs(Cs, Cs1, Cs2, Calls) :-
	reduce_cs(Cs, Cs3),
	append(Cs3, Cs1, Cs4),
	Calls = [planner_cs_simple:reduce_cs(Cs4, Cs2)].

make_true_cs([]).

project_cs(Cs, _, Cs).

reduce_cs(Cs, Cs1) :-
	reduce_cs_1(Cs, Cs2, S),
	( S == true ->
	  reduce_cs(Cs2, Cs1)
	; Cs2 = Cs1
	).

reduce_cs_1([C|Cs], Cs1, S) :-
	solve(C, R, S),
	( R = undefined ->
	  !,
	  Cs1 = [C|Cs2]
	; Cs2 = Cs1
	),
	reduce_cs_1(Cs, Cs2, S).
reduce_cs_1([], [], _).


%%%%
%%%%   - Constraint for eager evaluation: call/1
%%%%
%%%%     Disabled for security reasons. 
%% 
%% solve(call(X), true, Inst) :-
%% 	!,
%% 	( ground(X) -> true ; Inst = true ),
%% 	%% Inst gets also 'true' if the solution does not actually
%% 	%% instantiate X, but this is harmless.
%% 	call(X).

solve(X \= Y, _, _) :- X == Y, !, fail.
solve(X \= Y, true, _) :- \+ unify_with_occurs_check(X, Y), !.
solve(X =\= Y, _, _) :-	X == Y,	!, fail.
solve(X =\= Y, true, _) :- ground(X), ground(Y), !, X =\= Y.
solve(X =:= Y, true, _) :- ground(X), ground(Y), !, X =:= Y.
solve(X =:= Y, true, true) :- var(X), ground(Y), !, X is Y.
solve(X =:= Y, true, true) :- ground(X), var(Y), !, Y is X.
%% unification of X =:= Y, with both X and Y variable is probably
%% wrong, since equal numbers may not unify if they have different
%% types, such as integer and float.
solve(X > Y, _, _) :- X == Y, !, fail.
solve(X < Y, _, _) :- X == Y, !, fail.
solve(X > Y, true, _) :- ground(X), ground(Y), !, X > Y.
solve(X < Y, true, _) :- ground(X), ground(Y), !, X < Y.
solve(X >= Y, true, _) :- ground(X), ground(Y), !, X >= Y.
solve(X =< Y, true, _) :- ground(X), ground(Y), !, X =< Y.
solve(atom_number(X, Y), true, true) :-
	atom(X), var(Y),
	atom_codes(X, Codes),
	catch(number_codes(Y, Codes),
	      error(syntax_error(illegal_number), _),
	      fail),
	!.
solve(atom_number(X, Y), true, true) :-
	var(X), number(Y),
	!,
	term_to_atom(Y, X).
solve(atom_number(X, Y), true, _) :-
	atom(X), number(Y),
	atom_codes(X, Codes),
	catch(number_codes(Number, Codes),
	      error(syntax_error(illegal_number), _),
	      fail),
	!,
	Number = Y.
%% For debugging, to test nondeterministic stuff.
solve(in(X, Y), true, _) :- ground(Y), !, member(X, Y).

solve(_, undefined, _).

