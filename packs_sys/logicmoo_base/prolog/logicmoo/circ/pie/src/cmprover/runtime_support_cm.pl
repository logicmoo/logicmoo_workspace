%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 1992,1993,1996,1997,1998,2016 Christoph Wernhard
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

:- module( runtime_support_cm, 
           [ run_subsumes_chk/2,
	     run_subsumes_chk_noshare/2,
             run_absmember/2,
	     run_oc_member/2,
	     run_std_term_variables/2,

	     run_check_hconstraints/2,
	     run_hb_set_resources/3,
	     run_hw_set_resources/5,

	     run_lrpo_greater/2,
	     run_set_bound_increment/0,
	     run_set_bound_increment/2,
	     run_term_limit_reached/3,

	     run_call_suppressing_duplicates/2

% 	     run_setval/3,
% 	     run_getval/3,
% 	     run_alem_entry/2
	   ]).

:- use_module(swilib(sysdep)).
:- use_module(toytools(lrpo)).

:- module_transparent run_call_suppressing_duplicates/2.

:- flag(run_duration_in_last_depth, _, 0).
:- flag(run_duration_in_this_depth, _, 0).

run_absmember(X, [Y|_]) :- 
	X == Y,
	!.
run_absmember(X, [_|L]) :- 
	run_absmember(X, L).

run_oc_member(X, [Y|_]) :-
	unify_with_occurs_check(X, Y).
run_oc_member(X, [_|Z]) :-
	run_oc_member(X, Z).
	     
run_subsumes_chk(X, Y) :-
  \+ \+ ( copy_term(Y, Y1),
          unify_with_occurs_check(X, Y1),
	  Y =@= Y1 ).

run_subsumes_chk_noshare(X, Y) :-
  \+ \+ ( copy_term(Y, Y1),
          unify_with_occurs_check(X, Y1),
	  Y =@= Y1 ).

run_std_term_variables(Term, Vars) :-
	term_variables(Term, Vars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_check_hconstraints(L, L) :-
	check_hconstraints_1(L).

check_hconstraints_1([L|Ls]) :-
	call(L),
	check_hconstraints_1(Ls).
check_hconstraints_1([]).

run_hb_set_resources(_, 0, 0) :- !.
run_hb_set_resources(DB, Factor, D1) :-
	D1 is (DB / Factor).
%	D1 is DB // Factor.
%	D1 is DB // (Factor + 1).


run_hw_set_resources(NFree, NAdd, InfIn, InfOut, N1) :-
	DeltaI is InfOut - InfIn,
	hw_wdb_w3(NAdd, DeltaI, NAdd1),
	N1 is NFree + NAdd1.

hw_wdb_w1(N, _L, N1) :-
	N1 is N - 1.
hw_wdb_w2(N, L, N1) :-
	N1 is N - L.
hw_wdb_w3(N, DeltaI, N1) :-
	N1 is N / (1 + DeltaI).

run_lrpo_greater(X, Y) :-
 	lrpo_greater(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% increment recording (as suggested in stickel),
%% 
run_set_bound_increment(Available, Required) :-
	Increment is Required - Available,
	flag(bound_increment, Old, Old),
	( Old > 0 -> New is min(Old, Increment)	; New is Increment),
	flag(bound_increment, _, New).

%%
%% run_set_bound_increment/0 - just record whether bound reached
%%
run_set_bound_increment :-
	flag_inc(bound_increment, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% New Stuff 2015
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_term_limit_reached(Term, Factor, Depth) :-
	Limit is (Depth*Factor)+1,
	% Limit is Depth+Factor,
	( term_size_within_limit(Term, Limit, _) ->
	  % term_depth_within_limit(Term, Limit) ->
	  fail
	; !,
	  run_set_bound_increment
	).


term_depth_within_limit(_, L) :-
	L =< 0,
	!,
	fail.
term_depth_within_limit(T, L) :-
	compound(T),
	!,
	L1 is L - 1,
	functor(T, _, N),
	map_term_depth_within_limit(N, T, L1).
term_depth_within_limit(_, _).

map_term_depth_within_limit(0, _, _) :-
	!.
map_term_depth_within_limit(N, T, L) :-
	arg(N, T, T1),
	term_depth_within_limit(T1, L),
	N1 is N - 1,
	map_term_depth_within_limit(N1, T, L).


term_size_within_limit(_, L, _) :-
	L =< 0,
	!,
	fail.
term_size_within_limit(T, L, L1) :-
	compound(T),
	!,
	functor(T, _, N),
	L2 is L - 1,
	map_term_size_within_limit(N, T, L2, L1).
term_size_within_limit(_, L, L1) :-
	L1 is L - 1.

map_term_size_within_limit(0, _, L, L) :-
	!.
map_term_size_within_limit(N, T, L, L1) :-
	arg(N, T, T1),
	term_size_within_limit(T1, L, L2),
	N1 is N-1,
	map_term_size_within_limit(N1, T, L2, L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% run_setval(_Array, Index, Value) :-
% 	% ( Value \= [] -> writeq(a(Index,Value)), nl ; true ),
% 	retractall(array(Index, _)),
% 	assert(array(Index, Value)).
% 
% run_getval(_Array, Index, Value) :-
% 	array(Index, Value),
% 	!.
% 
% run_alem_entry(Call, [Call1|_]) :-
% 	run_subsumes_chk_noshare(Call1, Call),
% 	!,
% 	fail.
% run_alem_entry(Call, [_|Calls]) :-
% 	run_alem_entry(Call, Calls).
% run_alem_entry1(_, []).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_call_suppressing_duplicates(Call, VarTerm) :-
	term_variables(VarTerm, Vs),
	( Vs = [] -> Call
	; Store = store([]),
	  Call,
	  arg(1, Store, Values),
	  ( member(X, Values), subsumes_term(X, VarTerm) ->
	    fail
	  ; true
	  ),
	  nb_setarg(1, Store, [VarTerm|Values])
	).