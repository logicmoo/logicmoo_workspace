%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(auxiliary_fol,
	  [append_newq/3,
	   memberq/2,
	   subtractq/3,
	   unionq/3,
	   intersectionq/3,
	   deleteq/4,
	   andseq_to_list/2,
	   orseq_to_list/2,

	   subst_atoms/3,
	   
	   copy_given_vars/4,

	   replace_nonvar_subterm/3]).


append_newq(X, Y, Z) :-
	subtractq(Y, X, Y1),
	append(X, Y1, Z).

memberq(X, [Y|_]) :-
	X == Y,
	!.
memberq(X, [_|Y]) :-
	memberq(X, Y).

subtractq([], _, []) :- !.
subtractq([A|C], B, D) :-
	memberq(A, B),
	!,
	subtractq(C, B, D).
subtractq([A|B], C, [A|D]) :-
	subtractq(B, C, D).

%  unionq(X, Y, Z) :-
%  	append(X, Y, Z).

unionq(X, Y, Z) :-
	append(X, Y, XY),
	sort(XY, Z).

intersectionq([], _, []) :- !.
intersectionq([A|D], B, C) :-
	memberq(A, B),
	!,
	C=[A|E],
	intersectionq(D, B, E).
intersectionq([_|A], B, C) :-
	intersectionq(A, B, C).

deleteq(Xs, Ys, RXs, RYs) :-
	select(X, Xs, Xs1),
	select(Y, Ys, Ys1),
	X == Y,
	!,
	deleteq(Xs1, Ys1, RXs, RYs).
deleteq(Xs, Ys, Xs, Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

andseq_to_list((F, G), FG) :-
	!,
	andseq_to_list(F, F1),
	andseq_to_list(G, G1),
	append(F1, G1, FG).
andseq_to_list(true, []) :-
	!.
andseq_to_list(F, [F]).

orseq_to_list((F; G), FG) :-
	!,
	orseq_to_list(F, F1),
	orseq_to_list(G, G1),
	append(F1, G1, FG).
orseq_to_list(false, []) :-
	!.
orseq_to_list(F, [F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace_nonvar_subterm(V, _, V) :-
	var(V),
	!.
replace_nonvar_subterm(T, R, T1) :-
	member(T1-X, R),
	X == T,
	!.
replace_nonvar_subterm(T, R, T1) :-
	T =.. [F|Args],
	map_replace_nonvar_subterm(Args, R, Args1),
	T1 =.. [F|Args1].

map_replace_nonvar_subterm([X|Xs], Y1, [X1|Xs1]) :-
	replace_nonvar_subterm(X, Y1, X1),
	map_replace_nonvar_subterm(Xs, Y1, Xs1).
map_replace_nonvar_subterm([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Rename the variables in list of variables Xs to fresh variables
%%%% The fresh variables are returned as list Xs1, matching Xs.
%%%% Also accepts a single variable as Xs, treated like [Xs].
%%%%
%%%% +X +F -Xs1 -F1
%%%% 
copy_given_vars(Xs, F, Xs1, F1) :-
	term_variables(F, Vs),
	( var(Xs) -> Xs0 = [Xs]
	; Xs = Xs0
	),
	subtractq(Vs, Xs0, Vs1),
	copy_term(F-Xs-Vs1, F1-Xs1-Vs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subst_atoms(T, Map, V) :-
	atom(T),
	memberchk(T-V, Map),
	!.
subst_atoms(T, Map, T1) :-
	compound(T),
	!,
	functor(T, F, N),
	functor(T1, F, N),
	T =.. [_|Args],
	T1 =.. [_|Args1],
	map_subst_atoms(Args, Map, Args1).
subst_atoms(T, _, T).

map_subst_atoms([X|Xs], Map, [X1|Xs1]) :-
	subst_atoms(X, Map, X1),
	map_subst_atoms(Xs, Map, Xs1).
map_subst_atoms([], _, []).
