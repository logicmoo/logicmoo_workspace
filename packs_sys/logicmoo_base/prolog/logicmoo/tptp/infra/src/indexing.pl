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
%%%% Indexing
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(indexing, [specialized_clauses/3]).

:- use_module('swilib/term_support').
:- use_module('swilib/err').

%%%% 
%%%% specialized_clauses(+Patterns, -PAs, -Clauses)
%%%% 
%%%% Patterns is a list of of pattern specifiers, where a pattern specifier
%%%% is either a Head or a pair Head-Options. PAs is a list of the 
%%%% generated predicates as functor/arity specifiers. Clauses is 
%%%% the properly ordered set of access clauses. These clauses define 
%%%% for each predicate P/N in Patterns three predicates P/N,
%%%% assert_P/N and retractall_P/N.
%%%% 
%%%% - Repeated variables in a pattern are not supported.
%%%% - List of patterns must not include variants.
%%%% - Patterns need not to be standardized apart nor do they have
%%%%   to be supplied in some special ordering.
%%%% - The retractall_P predicates fail after retracting. This makes
%%%%   successively adding specialized clauses easier. 
%%%% - The following Options are recognized:
%%%%         first(N) - The Nth (starting at 0) argument (i.e. variable
%%%%                    in the pattern) is used as first argument of
%%%%                    the generated predicate
%%%%

specialized_clauses(Hs, PAs, Cs) :-
	findall(PA-C, 
	        ( select(H, Hs, Hs1),
	          sp_clause(Hs1, H, PA, C) ),
	        Cs1),
        map_key(Cs1, PAs1),
	map_val(Cs1, Cs2),
	sort(PAs1, PAs), 
	sort(Cs2, Cs).
	
sp_clause(Hs, H, Pred/Arity, (Head :- Body)) :-
	map_remove_options(Hs, Hs1),
	( H = H1-Options -> true ; H = H1, Options=[] ),
	copy_term(H1, H2),
	ordered_term_variables(H2, Params),
	gensym('p', Pred),
	( memberchk(first(N), Options) ->
	  nth_first(N, Params, Params1)
	; Params1 = Params
	),
	Call =.. [Pred|Params1],
	length(Params1, Arity),
	sp_nequations(Hs1, H2, [], Nequations),
	append(Nequations, ECall, BodyList),
	H2 =.. [F|Args],
	atom_concat('assert_', F, FA),
	HA =.. [FA|Args],
	atom_concat('retractall_', F, FR),
	HR =.. [FR|Args],
	( Head = H2, ECall = [ Call ]
	; Head = HA, ECall = [ assert(Call) ]
        ; Head = HR, ECall = [ retractall(Call), fail ]
	),
	list_to_andseq(BodyList, Body).

map_remove_options([X|Xs], [X1|Xs1]) :-
	remove_options(X, X1),
	map_remove_options(Xs, Xs1).
map_remove_options([], []).

remove_options(H-_, H) :-
	!.
remove_options(H, H).

sp_nequations([H1|Hs1], H, Hs2, [N|Ns]) :-
	append(Hs1, Hs2, Hs3),
	sp_nequation(H1, H, Hs3, N),
	!,
	sp_nequations(Hs1, H, [H1|Hs2], Ns).
sp_nequations([H1|Hs1], H, Hs2, Ns) :-
	sp_nequations(Hs1, H, [H1|Hs2], Ns).
sp_nequations([], _, _, []).

sp_nequation(H1, H, Hs, Nequation) :-
	copy_term(H1, H2),
	subsumeschk(H, H2),
	\+ ( member(H3, Hs),
	     subsumeschk(H, H3),
	     subsumeschk(H3, H2)
	   ),
	term_variables(H, Vs),
	copy_term(H-Vs, H2-Vs2),
	sp_args(Vs, Vs2, Args, Args1),
	( Args = [T1] ->
	  Args1 = [T2]
        ; T1 =.. [t|Args],
	  T2 =.. [t|Args1]
	),
	Nequation = (T1 \== T2).

sp_args([X|Xs], [Y|Ys], [X|Xs1], [Y|Ys1]) :-
	nonvar(Y),
	!,
	sp_args(Xs, Ys, Xs1, Ys1).
sp_args([_|Xs], [_|Ys], Xs1, Ys1) :-
	sp_args(Xs, Ys, Xs1, Ys1).
sp_args([], [], [], []).
	
list_to_andseq([X], X) :-
	!.
list_to_andseq([X,Y|Z], (X,YZ1)) :-
        list_to_andseq([Y|Z], YZ1).
list_to_andseq([], true).

map_key([X-_|Xs], [X|Xs1]) :-
	map_key(Xs, Xs1).
map_key([], []).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

nth_first(N, Xs, [X|Xs1]) :-
	sel(N, X, Xs, Xs1),
	!.
nth_first(N, _, _) :-
	err('Invalid indexing first value: ~q.', [N]).

sel(0, X, [X|Xs], Xs) :-
	!.
sel(N, X, [Y|Xs], [Y|Xs1]) :-
	N1 is N - 1,
	sel(N1, X, Xs, Xs1).





