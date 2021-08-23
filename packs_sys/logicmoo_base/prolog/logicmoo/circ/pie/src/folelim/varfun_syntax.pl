%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2019 Christoph Wernhard
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

:- module(varfun_syntax,
	  [vfterm_to_term/2,
	   term_to_vfterm/3]).
	   
:- use_module(swilib(err)).
:- use_module(logop_fol, [logform_rename_free_functions/3]).

vfterm_to_term(X, X) :-
	var(X),
	!.
vfterm_to_term('$vf'(F,Xs), X1) :-
	\+ var(F),
	F = lambda(Vars, Body),
	!,
	copy_term(Vars-Body, Vars1-Body1),
	( var(Vars1) -> Vars2 = [Vars1]
	; atom(Vars1) -> Vars2 = [Vars1]
	; Vars2 = Vars1
	),
	length(Xs, L1),
	length(Vars2, L2),
	( L1 = L2 ->
	  true
	; err('Lambda arity mismatch: ~q ~q', [F, Xs])
	),
	mk_lambda_map(Vars2, Map, Vars3),
	logform_rename_free_functions(Body1, Map, Body2),
	( map_vfterm_to_term(Xs, Vars3) ->
	  true
	; err('Lambda replacement failure: ~q ~q', [F, Xs])
	),
	X1 = Body2.
vfterm_to_term('$vf'(F,Xs), X1) :-
	!,
	( atom(F) ->
	  map_vfterm_to_term(Xs, Xs1),
	  X1 =.. [F|Xs1]
	; err('Unbound functor: ~q applied to ~q', [F, Xs])
	).
vfterm_to_term(X, X1) :-
	compound(X),
	!,
	X =.. [F|Xs],
	map_vfterm_to_term(Xs, Xs1),
	X1 =.. [F|Xs1].
vfterm_to_term(X, X).

mk_lambda_map([X|Xs], [X-Y|XYs], [Y|Ys]) :-
	mk_lambda_map(Xs, XYs, Ys).
mk_lambda_map([], [], []).

map_vfterm_to_term([X|Xs], [X1|Xs1]) :-
	vfterm_to_term(X, X1),
	map_vfterm_to_term(Xs, Xs1).
map_vfterm_to_term([], []).

term_to_vfterm(X, Varnames, X1) :-
	term_to_vfterm(X, Varnames, [], _, X1).

term_to_vfterm(X, Varnames, Map, Map1, X1) :-
	compound(X),
	!,
	X =.. [F|Xs],
	( is_varname(F) ->
	  ( memberchk(F=F1, Map) ->
	    Map2 = Map
	  ; memberchk(F=F1, Varnames) ->
	    Map2 = [F=F1|Map]
	  ; Map2 = [F=F1|Map]
	  ),
	  X1 = '$vf'(F1,Xs1),
	  map_term_to_vfterm(Xs, Varnames, Map2, Map1, Xs1)
	; map_term_to_vfterm(Xs, Varnames, Map, Map1, Xs1),
	  X1 =.. [F|Xs1]
	).	
term_to_vfterm(X, _, Map, Map, X).

map_term_to_vfterm([X|Xs], VN, Y1, Y2, [X1|Xs1]) :-
	term_to_vfterm(X, VN, Y1, Y3, X1),
	map_term_to_vfterm(Xs, VN, Y3, Y2, Xs1).
map_term_to_vfterm([], _, M, M, []).

is_varname(F) :-
	sub_atom(F, 0, 1, _, C),
	( is_upper(C) -> true
	; C = '_' -> true
	).
