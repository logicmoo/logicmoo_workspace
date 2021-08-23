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

:- module(view_support, [viewspec_form/2,
			 accessor_predicate/3]).

:- use_module(folelim(support_macrokb), [mac_make_args/2]).
:- use_module(toytools(auxiliary)).
:- use_module(swilib(err)).

%%%% 
%%%% Convert list of "view formula specifiers" to conjunction of formulas.
%%%% View formula specifiers include
%%%% - formulas required for view encoding with accessors
%%%% - common DB intergrity constraints
%%%% 
viewspec_form(Specs, F) :-
	map_viewspec_form_1(Specs, Fs),
	list_to_conjunction(Fs, F).

map_viewspec_form_1([X|Xs], [X1|Xs1]) :-
	viewspec_form_1(X, X1),
	map_viewspec_form_1(Xs, Xs1).
map_viewspec_form_1([], []).

viewspec_form_1(binds(P/N), F) :- !, binds_form(P/N, F).
viewspec_form_1(accessor(P/N, BPV), F) :- !, accessor_form(P/N, BPV, F).
viewspec_form_1(functional_dependency(P/N, FromArgs, ToArgs), F) :-
	!,
	constraint_form(functional_dependency(P/N, FromArgs, ToArgs), F).
viewspec_form_1(inclusion_dependency(P/N, FromArgs, Q/M, ToArgs), F) :-
	!,
	constraint_form(inclusion_dependency(P/N, FromArgs, Q/M, ToArgs), F).	
viewspec_form_1(S, _) :-
	err('Bad view formula specifier: ~q', [S]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Common DB Integrity Constraints
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraint_form(functional_dependency(P/N, FromArgs, ToArgs), F) :-
	( ( member(N0, FromArgs) ; member(N0, ToArgs) ), ( N0 < 1 ; N0 > N ) ->
	  err('Bad functional dependency: argument position ~q for ~q',
	      [N0, P/N])
	; true
	),
	mk_vars(N, u, FromArgs-x, ToArgs-y, Args1),
	mk_vars(N, v, FromArgs-x, ToArgs-z, Args2),
	append(Args1, Args2, Args3),
	sort(Args3, Vars),
	mk_vareqs(ToArgs, y, z, EQs),
	PArgs1 =.. [P|Args1],
	PArgs2 =.. [P|Args2],
	F = all(Vars, (PArgs1, PArgs2 -> EQs)).

constraint_form(inclusion_dependency(P/N, FromArgs, Q/M, ToArgs), F) :-
	length(FromArgs, L1),
	length(ToArgs, L2),
	( L1 \= L2 ->
	  err('Bad inclusion dependency mapping from ~q to ~q',
	      [FromArgs, ToArgs])
	; true
	),
	( member(N0, FromArgs), (N0 < 1 ; N0 > N) ->
	  err('Bad inclusion dependency source specifier: ~q for arity ~q',
	      [FromArgs, N])
	; true
	),
	( member(M0, ToArgs), (M0 < 1 ; M0 > M) ->
	  err('Bad inclusion dependency target specifier: ~q for arity ~q',
	      [ToArgs, M])
	; true
	),
	mk_vars(N, z, FromArgs-x, []-y, Args1),
	mk_vars(M, w, Args2),
	set_vars(ToArgs, FromArgs, x, Args2, Args3),
	subtract(Args3, Args1, ExVars),
	PArgs1 =.. [P|Args1],
	QArgs3 =.. [Q|Args3],
	( ExVars = [] ->
	  B = QArgs3
	; B = ex(ExVars, QArgs3)
	),
	( Args1 = [] ->
	  F = (PArgs1 -> B)
	; F = all(Args1, (PArgs1 -> B))
	).


mk_vars(M, A, B, C, D) :-
	mk_vars(1, M, A, B, C, D).

mk_vars(N, M, _, _, _, []) :-
	N > M,
	!.
mk_vars(N, M, X, ArgsY-Y, ArgsZ-Z, [V|Vs]) :-
	N =< M,
	( memberchk(N, ArgsY) ->
	  concat_atom([Y, N], V)
	; memberchk(N, ArgsZ) ->
	  concat_atom([Z, N], V)
	; concat_atom([X, N], V)
	),
	N1 is N+1,
	mk_vars(N1, M, X, ArgsY-Y, ArgsZ-Z, Vs).

mk_vars(N, X, Vs) :-
	mk_vars_1(1, N, X, Vs).

mk_vars_1(N, M, _, []) :-
	N > M,
	!.
mk_vars_1(N, M, X, [V|Vs]) :-
	N =< M,
	concat_atom([X,N], V),
	N1 is N+1,
	mk_vars_1(N1, M, X, Vs).

mk_vareqs([], _, _, true).
mk_vareqs([N], X, Y, (X1=Y1)) :-
	!,
	concat_atom([X,N], X1),
	concat_atom([Y,N], Y1).
mk_vareqs([N|Ns], X, Y, (X1=Y1, EQs)) :-
	!,
	concat_atom([X,N], X1),
	concat_atom([Y,N], Y1),
	mk_vareqs(Ns, X, Y, EQs).

set_vars([N|Ns], [M|Ms], X, Args, Args1) :-
	concat_atom([X,M], V),
	set_nth_elem(N, Args, V, Args2),
	set_vars(Ns, Ms, X, Args2, Args1).
set_vars([], [], _, Args, Args).

set_nth_elem(1, [_|Xs], X, [X|Xs]) :-
	!.
set_nth_elem(N, [X|Xs], Y, [X|Xs1]) :-
	N > 1,
	N1 is N - 1,
	set_nth_elem(N1, Xs, Y, Xs1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Accessors
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binds_form(_/0, true) :- !.
binds_form(P/N, F) :-
	N > 0,
	mac_make_args(N, Args),
	PArgs =.. [P|Args],
	mapf_wrap(Args, i, Is),
	F = all(Args, PArgs -> Is).

accessor_form(P/N, BPV, F) :-
	( BPV = BP-_ -> true ; BP=BPV ),
	mac_make_args(N, Args),
	mapf_wrap_b(BP, Args, i, Is),
	accessor_predicate(P, BPV, A),
	PArgs =.. [P|Args],
	AArgs =.. [A|Args],
	( Is  = true ->
	  F = all(Args, (PArgs <-> AArgs))
	; F = all(Args, Is -> (PArgs <-> AArgs))
	).

accessor_predicate(P, BP-V, AP) :- !, accessor_predicate_2(P, BP, V, AP).
accessor_predicate(P, BP, AP) :- !, accessor_predicate_1(P, BP, AP).	       

/*
- idea:
- prefer more specialized accessor for same predicate
- prefer predicates with smaller v
  v should indicate roughly the cardinality of solutions
  for given instantiation according to binding
*/

accessor_predicate_1(P, BP, AP) :-
	bp_onum(BP, BP1),
	format(atom(AP), '~w_~|~`0t~d~4+', [P, BP1]).		
accessor_predicate_2(P, BP, V, AP) :-
	bp_onum(BP, BP1),
	V1 is min(truncate(V*10000), 9999),
	format(atom(AP), '~|~`0t~d~4+_~w_~|~`0t~d~4+', [V1, P, BP1]).

%%%% 
%%%% A number, if BP1 is "more instantiated" than BP2 then
%%%%              bp_onum(BP1) < bp_onum(BP2)
%%%%
%%%% It can be used to order predicate symbols such that more instantiated
%%%% patterns (i.e. have a "b" where the other has an "f") are preferred.
%%%%
bp_onum(BP, N) :-
	bp_ostring_1(BP, S1),
	concat_atom(['0b'|S1], S2),
	atom_number(S2, N).

bp_ostring_1([b|BP], [0|BP1]) :- !, bp_ostring_1(BP, BP1).
bp_ostring_1([f|BP], [1|BP1]) :- !, bp_ostring_1(BP, BP1).
bp_ostring_1([], []).

mapf_wrap([X], F, FX) :-
	!,
	FX =.. [F,X].
mapf_wrap([X|Xs], F, (FX,FXs)) :-
	FX =.. [F,X],
	mapf_wrap(Xs, F, FXs).
mapf_wrap([], _, true).


mapf_wrap_b([b|BFs], [X|Xs], F, (FX,FXs)) :-
	memberchk(b, BFs),
	!,
	FX =.. [F,X],
	mapf_wrap_b(BFs, Xs, F, FXs).
mapf_wrap_b([b|_], [X|_], F, FX) :-
	!,
	FX =.. [F,X].
mapf_wrap_b([_|BFs], [_|Xs], F, FXs) :-
	mapf_wrap_b(BFs, Xs, F, FXs).
mapf_wrap_b([], [], _, true).

